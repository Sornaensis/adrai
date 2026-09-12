{-# LANGUAGE OverloadedStrings #-}

-- | Explicitly opt-in executable regression gate. Valid ADRAI records are
-- sealed with real parent OIDs and streamed through one @git fast-import@
-- process, including a compact feature branch and two-parent merge.
module Adrai.LargeRepositoryStressTest
  ( tests,
    parsePhaseProfileArguments,
    renderSelectedExecutableDiagnostic,
  )
where

import Adrai.Domain (Domain, DomainError, DomainRefinement, domainText, mkDomain, parseDomainRefinement)
import Adrai.Compiler.CacheSelection (exactCacheArchivePath)
import Adrai.Compiler.CacheSelection.TestSupport
  ( CacheValidationWorkCounters (..),
    MaterializationFingerprintEvidence (..),
    TargetReachabilityPlanDetails (..),
    observeExactCacheValidationWorkForTest,
    observeMaterializationFingerprintForTest,
    observeTargetReachabilityPlansForTest,
  )
import Adrai.Fixture.LargeStress (largeStressV1)
import Adrai.Fixture.ProductionShape (foldRepositoryPlan, repositorySteps)
import qualified Adrai.Fixture.Types as Fixture
import Adrai.Format.Config (defaultConfigText)
import Adrai.Format.Document
import Adrai.Provenance
import Adrai.Git (RevisionSpec (..), discoverRepository, systemGit)
import Adrai.Repository (resolveRepositoryRevision, resolvedCommitOid)
import Adrai.Scope (ScopePattern, mkScopePattern)
import Adrai.Types hiding (ExitSuccess)
import Control.Exception (SomeException, bracket, evaluate, onException, throwIO, try)
import Control.Concurrent (threadDelay)
import Control.Monad (foldM, forM_, join, unless, when)
import Data.Char (isDigit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.ByteString.Lazy as LazyByteString
import Data.IORef
import Data.Int (Int64)
import Data.List (intercalate, isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Scientific (toBoundedInteger, toRealFloat)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Vector as Vector
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Paths_adrai (getBinDir)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), isAbsolute, takeDirectory)
import System.Info (os)
import System.IO (Handle, hClose, hFlush, hGetContents, hGetLine, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process
  ( CreateProcess (std_err, std_in, std_out), StdStream (CreatePipe), createProcess,
    getPid, getProcessExitCode, proc, readProcessWithExitCode, terminateProcess, waitForProcess, ProcessHandle )
import System.Timeout (timeout)
import Text.Read (readMaybe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "P6-06G compact repository stress"
    ( [ testCase "32-commit repository with 16 ADR operations and a two-parent merge" largeRepositoryContract,
        testCase "phase profile option and sidecar parsing are strict" phaseProfileParsingContract,
        testCase "phase profile sidecar is absent by default" phaseProfileAbsentByDefaultContract,
        testCase "pinned GHC RTS pair-list maximum residency parsing is strict" rtsPairListParsingContract,
        testCase "stress executable resolver rejects accidental binary drift" executableResolverContract,
         testCase "process-tree sampler distinguishes failures, zero working sets, and descendants" processTreeSamplerContract,
         testCase "Windows process-tree sampler rejects ambiguous chronology and preserves exact sums" windowsProcessTreeSamplerContract,
         testCase "Windows process-tree sampler observes a live ProcessHandle through PowerShell" windowsProcessTreeIntegrationContract,
        testCase "process-tree complete-sample policy requires cold evidence but permits fast exits" processTreeSamplePolicyContract,
        testCase "taskkill failure accepts only a verified terminated process tree" taskkillFailureContract
      ]
    )

-- | Remove the harness-only option before Tasty receives its arguments.  The
-- option is intentionally exact and single-valued so a typo cannot silently
-- leave a profiling run without its requested artifact.
parsePhaseProfileArguments :: [String] -> Either String (Maybe FilePath, [String])
parsePhaseProfileArguments = go Nothing []
  where
    go output retained [] = Right (output, reverse retained)
    go _ _ ["--phase-profile"] = Left "adrai-stress-test --phase-profile requires a sidecar path"
    go output retained ("--phase-profile" : path : remaining)
      | null path = Left "adrai-stress-test --phase-profile requires a non-empty sidecar path"
      | otherwise =
          case output of
            Nothing -> go (Just path) retained remaining
            Just _ -> Left "adrai-stress-test accepts --phase-profile at most once"
    go output retained (argument : remaining) = go output (argument : retained) remaining

data PhaseProfile = PhaseProfile
  { phaseProfileFixture :: String,
    phaseProfileRts :: [String],
    phaseProfilePhases :: [PhaseSample]
  }
  deriving (Eq, Show)

data PhaseSample = PhaseSample
  { phaseSampleName :: String,
    phaseSampleStartedNs :: Word64,
    phaseSampleFinishedNs :: Word64,
    phaseSampleOutcome :: String,
    phaseSampleElapsedMs :: Integer,
    phaseSampleGitChildCount :: Int,
    phaseSampleGitChildElapsedMs :: Integer,
    phaseSampleGitRequests :: [(String, Int)]
  }
  deriving (Eq, Show)

data PhaseCounters = PhaseCounters
  { phaseCounterGitChildCount :: !Int,
    phaseCounterGitChildElapsedMs :: !Integer,
    phaseCounterGitRequests :: !(Map.Map String Int)
  }

emptyPhaseCounters :: PhaseCounters
emptyPhaseCounters = PhaseCounters 0 0 Map.empty

data PhaseProfileCollector = PhaseProfileCollector
  { collectorCurrentCounters :: IORef (Maybe (IORef PhaseCounters)),
    collectorSamples :: IORef [PhaseSample],
    collectorOutput :: Maybe FilePath,
    collectorFixture :: String,
    collectorRts :: [String]
  }

newPhaseProfileCollector :: Maybe FilePath -> String -> [String] -> IO PhaseProfileCollector
newPhaseProfileCollector output fixture rts = do
  collector <- PhaseProfileCollector <$> newIORef Nothing <*> newIORef [] <*> pure output <*> pure fixture <*> pure rts
  writePhaseProfileCheckpoint collector []
  pure collector

-- | Record a harness phase without altering any application-visible stream.
-- Git child accounting is attached to the currently active phase, so the
-- compiler's own process tree remains opaque and the harness never pretends
-- to report internal CLI Git calls it cannot observe.
withProfilePhase :: PhaseProfileCollector -> String -> IO value -> IO value
withProfilePhase collector name action = do
  started <- getMonotonicTimeNSec
  writePhaseStart collector name started
  counters <- newIORef emptyPhaseCounters
  previous <- atomicModifyIORef' (collectorCurrentCounters collector) (\current -> (Just counters, current))
  let finish outcome = do
        finished <- getMonotonicTimeNSec
        values <- readIORef counters
        atomicModifyIORef' (collectorCurrentCounters collector) (\_ -> (previous, ()))
        modifyIORef'
          (collectorSamples collector)
          ( <> [ PhaseSample
                   { phaseSampleName = name,
                     phaseSampleStartedNs = started,
                     phaseSampleFinishedNs = finished,
                     phaseSampleOutcome = outcome,
                     phaseSampleElapsedMs = elapsedMilliseconds started finished,
                     phaseSampleGitChildCount = phaseCounterGitChildCount values,
                     phaseSampleGitChildElapsedMs = phaseCounterGitChildElapsedMs values,
                     phaseSampleGitRequests = Map.toAscList (phaseCounterGitRequests values)
                   }
               ]
           )
        samples <- readIORef (collectorSamples collector)
        writePhaseProfileCheckpoint collector samples
  result <- action `onException` finish "failed"
  finish "ok"
  pure result

elapsedMilliseconds :: (Integral value) => value -> value -> Integer
elapsedMilliseconds started finished = fromIntegral ((finished - started) `div` 1000000)

{-# NOINLINE activePhaseProfileCollector #-}
activePhaseProfileCollector :: IORef (Maybe PhaseProfileCollector)
activePhaseProfileCollector = unsafePerformIO (newIORef Nothing)

-- | Time an externally spawned Git child only when the opt-in profiler is
-- active.  The action itself, including failure behavior, is otherwise left
-- intact.
profileGitChild :: String -> IO value -> IO value
profileGitChild request action = do
  counters <- currentProfileCounters
  case counters of
    Nothing -> action
    Just activeCounters -> do
      started <- getMonotonicTimeNSec
      let finish = recordGitChild activeCounters request started
      result <- action `onException` finish
      finish
      pure result

currentProfileCounters :: IO (Maybe (IORef PhaseCounters))
currentProfileCounters = do
  active <- readIORef activePhaseProfileCollector
  traverse (readIORef . collectorCurrentCounters) active >>= pure . join

recordGitChild :: IORef PhaseCounters -> String -> Word64 -> IO ()
recordGitChild counters request started = do
  finished <- getMonotonicTimeNSec
  modifyIORef' counters $ \values ->
    values
      { phaseCounterGitChildCount = phaseCounterGitChildCount values + 1,
        phaseCounterGitChildElapsedMs = phaseCounterGitChildElapsedMs values + elapsedMilliseconds started finished,
        phaseCounterGitRequests = Map.insertWith (+) request 1 (phaseCounterGitRequests values)
      }

renderPhaseProfile :: PhaseProfile -> String
renderPhaseProfile profile =
  unlines
    ( [ "adrai-stress-phase-profile-v3",
        "fixture\t" <> phaseProfileFixture profile,
        "rts\t" <> intercalate "," (phaseProfileRts profile)
      ]
        <> map renderPhase (phaseProfilePhases profile)
    )
  where
    renderPhase sample =
      intercalate
        "\t"
         [ "phase-end",
           phaseSampleName sample,
           show (phaseSampleStartedNs sample),
           show (phaseSampleFinishedNs sample),
           phaseSampleOutcome sample,
           show (phaseSampleElapsedMs sample),
          show (phaseSampleGitChildCount sample),
          show (phaseSampleGitChildElapsedMs sample),
          renderRequests (phaseSampleGitRequests sample)
        ]
    renderRequests = intercalate "," . map (\(request, count) -> request <> ":" <> show count)

parsePhaseProfile :: String -> Either String PhaseProfile
parsePhaseProfile raw =
  case lines raw of
    header : fixture : rts : phaseRows
      | header == "adrai-stress-phase-profile-v3" -> do
          parsedFixture <- parseFixture fixture
          parsedRts <- parseRts rts
          parsedPhases <- traverse parsePhase phaseRows
          if null parsedPhases
            then Left "phase profile must contain at least one phase row"
            else
              if uniquePhaseNames parsedPhases
                then Right (PhaseProfile parsedFixture parsedRts parsedPhases)
                else Left "phase profile phase names are duplicated"
      | otherwise -> Left "unsupported phase profile version"
    _ -> Left "phase profile is incomplete"
  where
    parseFixture line =
      case splitTabs line of
        ["fixture", value] | validToken value -> Right value
        _ -> Left "phase profile fixture row is malformed"
    parseRts line =
      case splitTabs line of
        ["rts", value]
          | values <- splitCommas value,
            not (null values),
            all validToken values -> Right values
        _ -> Left "phase profile RTS row is malformed"
    parsePhase line =
      case splitTabs line of
        ["phase-end", name, started, finished, outcome, elapsed, gitCount, gitElapsed, requests]
          | validToken name,
            Just parsedStarted <- decimalWord64 started,
            Just parsedFinished <- decimalWord64 finished,
            parsedFinished >= parsedStarted,
            outcome `elem` ["ok", "failed"],
            Just parsedElapsed <- decimalInteger elapsed,
            parsedElapsed == elapsedMilliseconds parsedStarted parsedFinished,
            Just parsedGitCount <- decimalInt gitCount,
            Just parsedGitElapsed <- decimalInteger gitElapsed -> do
                parsedRequests <- parseRequests requests
                if not (uniqueRequestNames parsedRequests)
                  then Left "phase profile Git request names are duplicated"
                  else
                    if sum (map snd parsedRequests) == parsedGitCount
                      then Right (PhaseSample name parsedStarted parsedFinished outcome parsedElapsed parsedGitCount parsedGitElapsed parsedRequests)
                      else Left "phase profile Git request count does not match Git child count"
        ["phase-start", name, started]
          | validToken name,
            Just _ <- decimalWord64 started -> Left "phase profile contains an unfinished phase"
        _ -> Left "phase profile phase row is malformed"
    parseRequests "" = Right []
    parseRequests value = traverse parseRequest (splitCommas value)
    parseRequest value =
      case break (== ':') value of
        (request, ':' : count)
          | validToken request,
            Just parsedCount <- decimalInt count,
            parsedCount > 0 -> Right (request, parsedCount)
        _ -> Left "phase profile Git request row is malformed"

    uniqueRequestNames requests =
      let names = map fst requests
       in length names == Set.size (Set.fromList names)

    uniquePhaseNames phases =
      let names = map phaseSampleName phases
       in length names == Set.size (Set.fromList names)

splitTabs :: String -> [String]
splitTabs = splitOn '\t'

splitCommas :: String -> [String]
splitCommas = splitOn ','

splitOn :: Char -> String -> [String]
splitOn separator = foldr step [""]
  where
    step character values@(value : remaining)
      | character == separator = "" : values
      | otherwise = (character : value) : remaining
    step _ [] = error "splitOn requires an initial accumulator"

validToken :: String -> Bool
validToken value = not (null value) && all (\character -> character /= '\t' && character /= ',' && character /= ':') value

decimalInteger :: String -> Maybe Integer
decimalInteger value
  | null value || any (not . isDigit) value = Nothing
  | otherwise = readMaybe value

decimalInt :: String -> Maybe Int
decimalInt value = do
  parsed <- decimalInteger value
  if parsed <= fromIntegral (maxBound :: Int)
    then Just (fromIntegral parsed)
    else Nothing

decimalWord64 :: String -> Maybe Word64
decimalWord64 value = do
  parsed <- decimalInteger value
  if parsed <= fromIntegral (maxBound :: Word64)
    then Just (fromIntegral parsed)
    else Nothing

writePhaseProfile :: Maybe FilePath -> PhaseProfile -> IO ()
writePhaseProfile output profile =
  forM_ output $ \path -> writePhaseProfileAtomically path (renderPhaseProfile profile)

writePhaseProfileCheckpoint :: PhaseProfileCollector -> [PhaseSample] -> IO ()
writePhaseProfileCheckpoint collector samples =
  writePhaseProfile
    (collectorOutput collector)
    (PhaseProfile (collectorFixture collector) (collectorRts collector) samples)

-- | A start checkpoint is written before the phase action begins.  If the
-- harness is killed or its timeout interrupts that action, this durable row is
-- retained instead of silently looking like a completed profile.
writePhaseStart :: PhaseProfileCollector -> String -> Word64 -> IO ()
writePhaseStart collector name started = do
  completed <- readIORef (collectorSamples collector)
  forM_ (collectorOutput collector) $ \path ->
    writePhaseProfileAtomically
      path
      ( renderPhaseProfile (PhaseProfile (collectorFixture collector) (collectorRts collector) completed)
          <> "phase-start\t" <> name <> "\t" <> show started <> "\n"
      )

writePhaseProfileAtomically :: FilePath -> String -> IO ()
writePhaseProfileAtomically path content = do
  let staged = path <> ".next"
  BS.writeFile staged (BS8.pack content)
  renameFile staged path

phaseProfileParsingContract :: IO ()
phaseProfileParsingContract = do
  parsePhaseProfileArguments ["--pattern=focused"] @?= Right (Nothing, ["--pattern=focused"])
  parsePhaseProfileArguments ["--phase-profile", "phase.tsv", "--pattern=focused"] @?= Right (Just "phase.tsv", ["--pattern=focused"])
  parsePhaseProfileArguments ["--phase-profile"] @?= Left "adrai-stress-test --phase-profile requires a sidecar path"
  parsePhaseProfileArguments ["--phase-profile", "one.tsv", "--phase-profile", "two.tsv"] @?= Left "adrai-stress-test accepts --phase-profile at most once"
  let expected =
        PhaseProfile
          "production-shape-v1"
          ["-N1", "-M2G"]
           [PhaseSample "materialize" 1000000000 1017000000 "ok" 17 2 4 [("git", 1), ("git-fast-import", 1)], PhaseSample "cold-compile" 2000000000 2009000000 "ok" 9 0 0 []]
  parsePhaseProfile (renderPhaseProfile expected) @?= Right expected
  parsePhaseProfile "adrai-stress-phase-profile-v3\nfixture\tproduction-shape-v1\nrts\t-N1,-M2G\nphase-end\tmaterialize\t1000000000\t1017000000\tok\t17\t2\t4\tgit:1\n" @?= Left "phase profile Git request count does not match Git child count"
  parsePhaseProfile "adrai-stress-phase-profile-v3\nfixture\tproduction-shape-v1\nrts\t-N1,-M2G\nphase-end\tmaterialize\t1000000000\t1017000000\tok\t17\t2\t4\tgit:1,git:1\n" @?= Left "phase profile Git request names are duplicated"
  parsePhaseProfile "adrai-stress-phase-profile-v3\nfixture\tproduction-shape-v1\nrts\t-N1,-M2G\nphase-end\tmaterialize\t1017000000\t1000000000\tok\t17\t2\t4\tgit:1,git-fast-import:1\n" @?= Left "phase profile phase row is malformed"
  parsePhaseProfile "adrai-stress-phase-profile-v3\nfixture\tproduction-shape-v1\nrts\t-N1,-M2G\nphase-start\tmaterialize\t1000000000\n" @?= Left "phase profile contains an unfinished phase"
  trace2ProcessTotals "{\"event\":\"start\",\"t_abs\":1.0}\n{\"event\":\"cmd_name\"}\n{\"event\":\"child_start\"}\n{\"event\":\"child_exit\"}\n{\"event\":\"exit\",\"t_abs\":1.25}\n" @?= Right (1, 1, 250)
  assertTrace2Rejected "duplicate cmd_name" "{\"event\":\"start\",\"t_abs\":1.0}\n{\"event\":\"cmd_name\"}\n{\"event\":\"cmd_name\"}\n"
  assertTrace2Rejected "overlapping top-level starts" "{\"event\":\"start\",\"t_abs\":1.0}\n{\"event\":\"start\",\"t_abs\":1.1}\n"
  assertTrace2Rejected "negative timestamps" "{\"event\":\"start\",\"t_abs\":-1.0}\n"
  assertTrace2Rejected "non-finite timestamps" "{\"event\":\"start\",\"t_abs\":1e999999999}\n"
  withSystemTempDirectory "adrai-phase-profile-lifecycle" $ \root -> do
    let output = root </> "phase.tsv"
    collector <- newPhaseProfileCollector (Just output) "profile-fixture" ["-N1"]
    withProfilePhase collector "successful-phase" $ do
      started <- parsePhaseProfile . BS8.unpack <$> BS.readFile output
      started @?= Left "phase profile contains an unfinished phase"
    successful <- parsePhaseProfile . BS8.unpack <$> BS.readFile output
    case successful of
      Right profile -> map phaseSampleOutcome (phaseProfilePhases profile) @?= ["ok"]
      Left problem -> assertFailure problem
    failed <- try (withProfilePhase collector "failed-phase" (ioError (userError "expected phase failure"))) :: IO (Either SomeException ())
    case failed of
      Left _ -> pure ()
      Right () -> assertFailure "expected phase failure"
    failedProfile <- parsePhaseProfile . BS8.unpack <$> BS.readFile output
    case failedProfile of
      Right profile -> map phaseSampleOutcome (phaseProfilePhases profile) @?= ["ok", "failed"]
      Left problem -> assertFailure problem
    timedOut <- timeout 1000 (withProfilePhase collector "timed-out-phase" (threadDelay 1000000))
    timedOut @?= Nothing
    timeoutProfile <- parsePhaseProfile . BS8.unpack <$> BS.readFile output
    case timeoutProfile of
      Right profile -> map phaseSampleOutcome (phaseProfilePhases profile) @?= ["ok", "failed", "failed"]
      Left problem -> assertFailure problem

phaseProfileAbsentByDefaultContract :: IO ()
phaseProfileAbsentByDefaultContract =
  withSystemTempDirectory "adrai-phase-profile-absence" $ \root -> do
    let output = root </> "not-written.tsv"
    writePhaseProfile Nothing (PhaseProfile "production-shape-v1" ["-N1", "-M2G"] [PhaseSample "materialize" 0 0 "ok" 0 0 0 []])
    exists <- doesFileExist output
    assertBool "profiling sidecar must not be created without --phase-profile" (not exists)

stressProcessTreeTargetBytes :: Integer
-- OS working-set accounting includes executable images and child processes.
-- Both supported samplers capture the root with all observed descendants, so
-- the 2.5 GiB process-tree allowance is an acceptance bound on Windows and
-- POSIX rather than a Windows-only diagnostic.
stressProcessTreeTargetBytes = (5 * 1024 * 1024 * 1024) `div` 2

compactStressImportPlan :: Fixture.RepositoryPlan
compactStressImportPlan =
  Fixture.RepositoryPlan
    { Fixture.repositoryPlanMeta =
        (Fixture.repositoryPlanMeta largeStressV1)
          { Fixture.fixturePurpose =
              "30 imported commits followed by one feature commit and one merge; 32 commits and 16 operations total"
          },
      Fixture.repositoryPlanSpec =
        Fixture.RepositorySpec
          { Fixture.repositorySpecName = "compact-stress-v1",
            Fixture.repositoryCommitCount = 30,
            Fixture.repositoryOperationCounts = compactStressImportOperationCounts,
            Fixture.repositoryNoiseCommitCount = 15,
            Fixture.repositoryBranchEvents =
              [ Fixture.CreateBranchAt (Fixture.CommitOrdinal 31) compactFeatureBranch (Fixture.CommitOrdinal 30),
                Fixture.CheckoutBranchAt (Fixture.CommitOrdinal 31) compactFeatureBranch,
                Fixture.CheckoutBranchAt (Fixture.CommitOrdinal 32) (Fixture.BranchKey "main")
              ],
            Fixture.repositoryInvariants =
              [ Fixture.ExactCommitCount 30,
                Fixture.ExactNoiseCommitCount 15,
                Fixture.ExactOperationCount Fixture.CreateOperation 8,
                Fixture.ExactOperationCount Fixture.AmendOperation 3,
                Fixture.ExactOperationCount Fixture.ScopeOperation 2,
                Fixture.ExactOperationCount Fixture.DomainOperation 1,
                Fixture.ExactOperationCount Fixture.ObsoleteOperation 1,
                Fixture.ParentsPrecedeChildren,
                Fixture.OperationsReferenceCreatedAdrs
              ]
          }
    }

compactStressImportOperationCounts :: Fixture.OperationCounts
compactStressImportOperationCounts = Fixture.OperationCounts 8 3 2 1 1

compactFeatureBranch :: Fixture.BranchKey
compactFeatureBranch = Fixture.BranchKey "stress/feature-adrs"

-- ADR key 7 remains active after the compact plan's amend/scope/domain/status
-- transitions and supplies deterministic data for the compiled repository.
representativeTemplate :: Fixture.AdrTemplate
representativeTemplate =
  case
      [ template
      | planned <- repositorySteps compactStressImportPlan
      , Fixture.PlannedSemantic _ (Fixture.PlannedCreate template) <- [Fixture.plannedCommitKind planned]
      , Fixture.adrTemplateKey template == Fixture.AdrKey 7
      ] of
    [template] -> template
    templates -> error ("compactStressImportPlan must contain exactly one representative template, found " <> show (length templates))

representativeRelevanceText :: Text.Text
representativeRelevanceText =
  "Canonical compact-plan marker: "
    <> Fixture.adrTemplateTitle representativeTemplate
    <> " applies to "
    <> Text.intercalate ", " (Fixture.adrTemplateScopes representativeTemplate)
    <> "."

data AdrState = AdrState
  { stateAdr :: AdrId, stateRecord :: RecordId, stateScope :: ConnectionId
  , stateDomain :: ConnectionId, stateStatus :: ConnectionId, stateDomains :: [Domain]
  , stateScopes :: [ScopePattern], stateTitle :: Text.Text
  }

data Importer = Importer
  { importerInput :: Handle, importerOutput :: Handle, importerMark :: IORef Int
  , importerBasis :: IORef (Maybe GitOid), importerLastCommitMark :: IORef (Maybe Int)
  , importerProcess :: ProcessHandle, importerError :: Handle
  , importerProfileCounters :: Maybe (IORef PhaseCounters), importerStartedAt :: Word64
  }

largeRepositoryContract :: IO ()
largeRepositoryContract = withSystemTempDirectory "adrai-compact-repository" $ \root -> do
  let repository = root </> "repository"
  featureAdr <- timed "materialize" (materializeCompactRepository repository (representativeRelevanceText <> "\n"))
  assertCompactRepositoryTopology repository featureAdr

  cold <- timed "cold-compile" (runAdraiFunctional repository ["compile", "--json"])
  assertContains "cold compile" "\"cache_mode\":\"full\"" cold
  assertContains "cold compile" "\"incremental_kind\":\"full\"" cold
  assertContains "cold compile" "\"errors\":0" cold
  coldHistory <- jsonIntegerAt ["history_commits_scanned"] cold
  assertEqual coldHistory 32 "cold compile must scan the complete compact branch-and-merge history"
  coldDocuments <- jsonIntegerAt ["documents_parsed"] cold
  assertEqual coldDocuments 46 "cold compile must parse every compact semantic document"
  coldAdrs <- jsonIntegerAt ["adrs_rebuilt"] cold
  assertEqual coldAdrs 9 "cold compile must rebuild all eight imported ADRs and the branch ADR"
  compactArchiveValidationContract repository

  -- The only second production-CLI call follows an unmanaged commit and must
  -- prove the bounded tree-identical path without reparsing the corpus.
  addNoiseCommit repository "after-cache-proof"
  treeIdentical <- timed "tree-identical-compile" (runAdraiFunctional repository ["compile", "--json"])
  assertContains "tree-identical compile" "\"incremental_kind\":\"tree-identical\"" treeIdentical
  assertContains "tree-identical compile" "\"documents_parsed\":0" treeIdentical
  treeHistory <- jsonIntegerAt ["history_commits_scanned"] treeIdentical
  assertEqual treeHistory 1 "tree-identical history proof must scan exactly the new noise commit"

compactArchiveValidationContract :: FilePath -> IO ()
compactArchiveValidationContract repositoryPath = do
  discovered <- discoverRepository systemGit repositoryPath
  repository <- either (\problem -> assertFailure (show problem) >> fail "unreachable") pure discovered
  resolved <- resolveRepositoryRevision repository (RevisionSpec "HEAD")
  revision <- either (\problem -> assertFailure (show problem) >> fail "unreachable") pure resolved
  archive <-
    maybe
      (assertFailure "compact cold compile has no exact archive" >> fail "unreachable")
      pure
      (exactCacheArchivePath repository (resolvedCommitOid revision))
  let target = gitOidText (resolvedCommitOid revision)
  (accepted, counters) <- observeExactCacheValidationWorkForTest archive target
  assertBool "compact exact archive validation was not accepted" accepted
  cacheValidationSourceOpens counters @?= 1
  cacheValidationInvocations counters @?= 1
  cacheValidationFullMaterializationLoads counters @?= 1
  cacheValidationFtsPayloadMaterializations counters @?= 0
  cacheValidationFtsParityChecks counters @?= compactExpectedFtsValidationFamilies
  cacheValidationFtsIntegrityChecks counters @?= compactExpectedFtsValidationFamilies
  cacheValidationCanonicalFamilyScans counters @?= compactExpectedFamilyScans
  Map.keysSet (cacheValidationCanonicalFamilyRows counters) @?= Map.keysSet compactExpectedFamilyScans
  let rows = cacheValidationCanonicalFamilyRows counters
  Map.lookup "operation" rows @?= Just 16
  Map.lookup "target_reachable_commit" rows @?= Just 32
  Map.lookup "managed_source" rows @?= Just 46
  Map.lookup "decision_record" rows @?= Just 12
  Map.lookup "reduced_adr" rows @?= Just 9
  Map.lookup "search_document" rows @?= Just 9

  fingerprintEvidence <- observeMaterializationFingerprintForTest archive
  evidence <- maybe (assertFailure "compact archive fingerprint parity evidence was unavailable" >> fail "unreachable") pure fingerprintEvidence
  case
      ( materializationFingerprintPersistedValue evidence,
        materializationFingerprintEstablishedValue evidence,
        materializationFingerprintProductionValue evidence,
        materializationFingerprintObserverValue evidence
      )
    of
      (Just persisted, Just established, Just production, Just observed) -> do
        persisted @?= established
        persisted @?= production
        persisted @?= observed
      _ -> assertFailure "compact archive fingerprint parity did not produce four accepted values"

  plans <- observeTargetReachabilityPlansForTest archive target
  planEvidence <- maybe (assertFailure "compact archive reachability plans were unavailable" >> fail "unreachable") pure plans
  assertReachabilityPlan "bulk" "target_oid=?" (targetReachabilityBulkPlanDetails planEvidence)
  assertReachabilityPlan "lower" "target_oid<?" (targetReachabilityLowerProbePlanDetails planEvidence)
  assertReachabilityPlan "upper" "target_oid>?" (targetReachabilityUpperProbePlanDetails planEvidence)
  where
    assertReachabilityPlan label predicate details = do
      let rendered = Text.intercalate "\n" details
      assertBool (label <> " reachability plan omitted the composite PK index: " <> Text.unpack rendered)
        ("sqlite_autoindex_target_reachable_commit_1" `Text.isInfixOf` rendered)
      assertBool (label <> " reachability plan omitted " <> Text.unpack predicate <> ": " <> Text.unpack rendered)
        (predicate `Text.isInfixOf` rendered)
      assertBool (label <> " reachability plan scanned the table: " <> Text.unpack rendered)
        (not ("SCAN target_reachable_commit" `Text.isInfixOf` rendered))
      assertBool (label <> " reachability plan used a temporary sort: " <> Text.unpack rendered)
        (not ("USE TEMP B-TREE" `Text.isInfixOf` rendered))

compactExpectedFamilyScans :: Map.Map Text.Text Int
compactExpectedFamilyScans = Map.fromList
  [ ("meta", 1), ("sqlite_master", 1), ("repository_config", 1), ("managed_source", 1),
    ("issue", 1), ("adr_conflict", 1), ("operation", 1), ("operation_member", 1),
    ("operation_member_parent", 1), ("decision_record", 1), ("connection_record", 1),
    ("reduced_adr", 1), ("axis_head", 1), ("current_connection", 1),
    ("operation_commit", 1), ("operation_target_coverage", 1), ("line_landing", 1),
    ("target_reachable_commit", 1), ("line_config", 1), ("ref_observation", 1),
    ("search_document", 1), ("search_section", 1), ("local_alias", 1)
  ]

compactExpectedFtsValidationFamilies :: Map.Map Text.Text Int
compactExpectedFtsValidationFamilies = Map.fromList
  [ ("fts_search_exact", 1), ("fts_search_stemmed", 1), ("fts_search_identifier", 1),
    ("fts_passage_exact", 1), ("fts_passage_stemmed", 1), ("fts_passage_identifier", 1)
  ]

-- Trace2 is intentionally parsed independently of the compiler artifact.
-- Fixture construction finishes before this environment is installed, so each
-- matched top-level Git start/exit pair is evidence from the compiler child
-- alone.  Do not count @child_start@/@child_exit@: those describe processes
-- spawned /by/ Git rather than the Git operation requested by the compiler.
trace2ProcessTotals :: String -> Either String (Int, Int, Integer)
trace2ProcessTotals raw = do
  (started, exited, elapsed, open) <- foldM countEvent (0, 0, 0, Nothing) (filter (not . null) (lines raw))
  case open of
    Nothing -> Right (started, exited, elapsed)
    Just _ -> Left "Trace2 ends with an unfinished top-level Git session"
  where
    countEvent (started, exited, elapsed, open) line =
      case Aeson.eitherDecode (LazyByteString.fromStrict (BS8.pack line)) of
        Left problem -> Left problem
        Right (Aeson.Object event) ->
          case AesonKeyMap.lookup "event" event of
            Just (Aeson.String "start") -> do
              startedAt <- absoluteTime event
              case open of
                Nothing -> Right (started + 1, exited, elapsed, Just (startedAt, False))
                Just _ -> Left "Trace2 top-level Git sessions overlap"
            Just (Aeson.String "cmd_name") ->
              case open of
                Just (startedAt, False) -> Right (started, exited, elapsed, Just (startedAt, True))
                _ -> Left "Trace2 cmd_name is outside a top-level Git session"
            Just (Aeson.String "exit") -> do
              finishedAt <- absoluteTime event
              case open of
                Just (startedAt, True)
                  | finishedAt >= startedAt -> Right (started, exited + 1, elapsed + floor ((finishedAt - startedAt) * 1000), Nothing)
                  | otherwise -> Left "Trace2 top-level Git session elapsed time is negative"
                _ -> Left "Trace2 exit does not close a named top-level Git session"
            Just (Aeson.String _) -> Right (started, exited, elapsed, open)
            _ -> Left "Trace2 event row has no string event field"
        Right _ -> Left "Trace2 event row is not an object"
    absoluteTime event =
      case AesonKeyMap.lookup "t_abs" event of
        Just (Aeson.Number value)
          | value >= 0,
            let asDouble = toRealFloat value :: Double,
            not (isInfinite asDouble || isNaN asDouble) -> Right value
        _ -> Left "Trace2 top-level Git event has no non-negative t_abs"

assertTrace2Rejected :: String -> String -> IO ()
assertTrace2Rejected label raw =
  assertBool label (either (const True) (const False) (trace2ProcessTotals raw))

materializeCompactRepository :: FilePath -> Text.Text -> IO String
materializeCompactRepository repository relevanceText = do
  initialise repository
  validatePlan compactStressImportPlan
  featureAdr <- withImporter repository $ \importer -> do
    states <- newIORef []
    forM_ (repositorySteps compactStressImportPlan) (emitPlanned relevanceText importer states)
    mainMark <- requireImporterLastCommitMark importer
    mainBasis <- requireImporterBasis importer
    (adr, featureFiles) <- featureAdrFiles mainBasis
    featureMark <-
      fastCommitAt
        importer
        "refs/heads/stress/feature-adrs"
        [mainMark]
        featureFiles
        "adrai: feature branch only decision"
    _ <-
      fastCommitAt
        importer
        "refs/heads/main"
        [mainMark, featureMark]
        featureFiles
        "merge compact feature ADR"
    pure adr
  synchronizeImportedWorktree repository
  pure featureAdr

assertCompactRepositoryTopology :: FilePath -> String -> IO ()
assertCompactRepositoryTopology repository featureAdr = do
  ancestry <- filter (not . null) . lines <$> gitStdout repository ["rev-list", "--parents", "HEAD"]
  length ancestry @?= 32
  case ancestry of
    merge : _ -> length (words merge) @?= 3
    [] -> assertFailure "compact repository ancestry unexpectedly became empty"
  subjects <- lines <$> gitStdout repository ["log", "--format=%s", "HEAD"]
  length (filter ("adrai:" `isInfixOf`) subjects) @?= 16
  firstParentHasFeature <- gitTreeContains repository "HEAD^1" featureAdr
  secondParentHasFeature <- gitTreeContains repository "HEAD^2" featureAdr
  mergedHeadHasFeature <- gitTreeContains repository "HEAD" featureAdr
  assertBool "the main parent must not contain the feature ADR" (not firstParentHasFeature)
  assertBool "the feature parent must contain the feature ADR" secondParentHasFeature
  assertBool "the merge result must retain the feature ADR" mergedHeadHasFeature

gitTreeContains :: FilePath -> String -> String -> IO Bool
gitTreeContains repository revision marker = do
  (status, output, problem) <-
    readProcessWithExitCode
      "git"
      ["-C", repository, "grep", "-l", marker, revision, "--", "architecture/adrai"]
      ""
  case status of
    ExitSuccess -> do
      assertBool ("git grep returned no paths for " <> revision) (not (null (lines output)))
      pure True
    ExitFailure 1 | null output && null problem -> pure False
    ExitFailure code -> assertFailure ("git tree read failed for " <> revision <> " (" <> show code <> "): " <> problem) >> fail "unreachable"

-- | Fast-import updates refs directly, so reset both ordinary Git views before
-- subsequent fixture commits use the index or worktree.
synchronizeImportedWorktree :: FilePath -> IO ()
synchronizeImportedWorktree repository = git repository ["reset", "--hard", "HEAD"]

initialise :: FilePath -> IO ()
initialise repository = do
  createDirectoryIfMissing True repository
  git repository ["init", "--initial-branch", "main"]
  git repository ["config", "user.name", "ADRAI large stress"]
  git repository ["config", "user.email", "adrai-large-stress@example.invalid"]

emitOperation :: Importer -> IORef [AdrState] -> Int -> Fixture.PlannedOperation -> IO ()
emitOperation importer states operation planned =
  case planned of
    Fixture.PlannedCreate template -> createState importer states operation template
    Fixture.PlannedAmend target amendment -> amendState importer states operation target amendment
    Fixture.PlannedScope target scopes -> scopeState importer states operation target scopes
    Fixture.PlannedDomain target domains -> domainState importer states operation target domains
    Fixture.PlannedObsolete target -> obsoleteState importer states operation target

-- | Interpret the compact import plan directly. The strict fold
-- below guards its cardinalities; this traversal preserves its deterministic
-- due schedule instead of maintaining a second schedule in the stress gate.
emitPlanned :: Text.Text -> Importer -> IORef [AdrState] -> Fixture.PlannedCommit -> IO ()
emitPlanned relevanceText importer states planned =
  case Fixture.plannedCommitKind planned of
    Fixture.PlannedSemantic (Fixture.OperationKey operation) payload -> emitOperation importer states operation payload
    Fixture.PlannedNoise noiseLabel ->
      case Fixture.plannedCommitOrdinal planned of
        Fixture.CommitOrdinal 1 ->
          fastCommit importer
            [ (".adrai.toml", TextEncoding.encodeUtf8 defaultConfigText)
            , ("README.md", "# ADRAI large stress fixture\n")
            , ("analysis/relevance.txt", TextEncoding.encodeUtf8 relevanceText)
            , ("src/noise/module_00001.txt", TextEncoding.encodeUtf8 (noiseLabel <> "\n"))
            ]
            (Text.unpack noiseLabel)
        Fixture.CommitOrdinal ordinal -> emitNoise importer ordinal noiseLabel
    Fixture.PlannedMerge branch -> assertFailure ("compact import plan unexpectedly contains a merge step for " <> show branch)

validatePlan :: Fixture.RepositoryPlan -> IO ()
validatePlan plan = do
  counts <- evaluate (foldRepositoryPlan countStep (0 :: Int, 0 :: Int, 0 :: Int) plan)
  let spec = Fixture.repositoryPlanSpec plan
      expected =
        ( Fixture.repositoryCommitCount spec,
          Fixture.operationCountTotal (Fixture.repositoryOperationCounts spec),
          Fixture.repositoryNoiseCommitCount spec
        )
  assertEqual counts expected "fixture plan must retain its planned commit, operation, and noise counts"
  where
    countStep (commits, operations, noise) planned =
      case Fixture.plannedCommitKind planned of
        Fixture.PlannedNoise _ -> (commits + 1, operations, noise + 1)
        Fixture.PlannedSemantic _ _ -> (commits + 1, operations + 1, noise)
        Fixture.PlannedMerge _ -> (commits + 1, operations, noise)

createState :: Importer -> IORef [AdrState] -> Int -> Fixture.AdrTemplate -> IO ()
createState importer states operation template = do
  let Fixture.AdrKey key = Fixture.adrTemplateKey template
      identity = key + 1
  adr <- checked "ADR" (mkAdrId (identifier 'A' identity))
  record <- checked "record" (mkRecordId (identifier 'R' identity))
  scope <- checked "scope" (mkConnectionId (identifier 'C' (identity * 4)))
  domain <- checked "domain" (mkConnectionId (identifier 'C' (identity * 4 + 1)))
  status <- checked "status" (mkConnectionId (identifier 'C' (identity * 4 + 2)))
  operationId <- checked "operation" (mkOperationId (identifier 'O' operation))
  actor <- checked "actor" (mkActor ServiceActor "large-stress-generator" Nothing)
  domains <- traverse (checked "planned create domain" . mkDomain) (Fixture.adrTemplateDomains template)
  scopes <- traverse (checked "planned create scope" . mkScopePattern) (Fixture.adrTemplateScopes template)
  unless (not (null domains) && not (null scopes)) $ assertFailure "planned create must provide domains and scopes"
  basis <- requireImporterBasis importer
  let decision = ManagedDecision DecisionRecord
        { decisionAdr = adr, decisionRecord = record, decisionTitle = Fixture.adrTemplateTitle template
        , decisionSummary = Fixture.adrTemplateSummary template, decisionDomains = domains
        , decisionBody = renderTemplateBody (Fixture.adrTemplateBodySections template)
        }
      scopeRecord = ManagedConnection (ConnectionRecord scope (AppliesToConnection (AppliesToPayload adr [] "initial" scopes [] scopes)) "Initial stress scope.\n")
      domainRecord = ManagedConnection (ConnectionRecord domain (DomainsConnection (DomainsPayload adr [] "initial" domains [] domains [])) "Initial stress domain.\n")
      statusRecord = ManagedConnection (ConnectionRecord status (StatusConnection (StatusPayload adr [] StatusActive [record] Nothing)) "Initial active status.\n")
  files <- traverse (sealMember actor basis operationId) [(decision, "decision.create", []), (scopeRecord, "scope.initial", [ProvenanceRecord record]), (domainRecord, "domain.initial", [ProvenanceRecord record]), (statusRecord, "status.initial", [ProvenanceRecord record])]
  fastCommit importer files ("adrai: create " <> Text.unpack (Fixture.adrTemplateTitle template))
  modifyIORef' states (<> [AdrState adr record scope domain status domains scopes (Fixture.adrTemplateTitle template)])

featureAdrFiles :: GitOid -> IO (String, [(FilePath, BS.ByteString)])
featureAdrFiles basis = do
  adr <- checked "feature ADR" (mkAdrId (identifier 'A' 99001))
  record <- checked "feature record" (mkRecordId (identifier 'R' 99001))
  scope <- checked "feature scope" (mkConnectionId (identifier 'C' 99001))
  domainEdge <- checked "feature domain" (mkConnectionId (identifier 'C' 99002))
  status <- checked "feature status" (mkConnectionId (identifier 'C' 99003))
  operation <- checked "feature operation" (mkOperationId (identifier 'O' 99001))
  actor <- checked "feature actor" (mkActor ServiceActor "large-stress-feature" Nothing)
  domain <- checked "feature domain" (mkDomain "compiler")
  scopePattern <- checked "feature scope" (mkScopePattern "src/feature/**")
  let decision = ManagedDecision (DecisionRecord adr record "Feature branch only decision" "A branch-only ADR validates revision isolation." [domain] "# Decision\nKeep feature-only state isolated.\n")
      scopeRecord = ManagedConnection (ConnectionRecord scope (AppliesToConnection (AppliesToPayload adr [] "initial" [scopePattern] [] [scopePattern])) "Feature scope.\n")
      domainRecord = ManagedConnection (ConnectionRecord domainEdge (DomainsConnection (DomainsPayload adr [] "initial" [domain] [] [domain] [])) "Feature domain.\n")
      statusRecord = ManagedConnection (ConnectionRecord status (StatusConnection (StatusPayload adr [] StatusActive [record] Nothing)) "Feature status.\n")
  files <- traverse (sealMemberOnBranch "stress/feature-adrs" actor basis operation) [(decision, "decision.create", []), (scopeRecord, "scope.initial", [ProvenanceRecord record]), (domainRecord, "domain.initial", [ProvenanceRecord record]), (statusRecord, "status.initial", [ProvenanceRecord record])]
  pure (Text.unpack (adrIdText adr), files)

amendState :: Importer -> IORef [AdrState] -> Int -> Fixture.AdrKey -> Text.Text -> IO ()
amendState importer states operation target amendment = do
  state <- stateAt states target
  record <- checked "amended record" (mkRecordId (identifier 'R' (100000 + operation)))
  edge <- checked "amend connection" (mkConnectionId (identifier 'C' (100000 + operation)))
  operationId <- checked "amend operation" (mkOperationId (identifier 'O' operation))
  actor <- checked "actor" (mkActor ServiceActor "large-stress-generator" Nothing)
  basis <- requireImporterBasis importer
  let decision = ManagedDecision DecisionRecord
        { decisionAdr = stateAdr state, decisionRecord = record, decisionTitle = stateTitle state
        , decisionSummary = amendment, decisionDomains = stateDomains state
        , decisionBody = "# Amendment\n" <> amendment <> "\n"
        }
      edgeRecord = ManagedConnection (ConnectionRecord edge (AmendsConnection (AmendsPayload (stateAdr state) record [stateRecord state])) "Stress amendment.\n")
  files <- traverse (sealMember actor basis operationId) [(decision, "decision.amend", [ProvenanceRecord (stateRecord state)]), (edgeRecord, "connection.amends", [ProvenanceRecord (stateRecord state)])]
  fastCommit importer files ("adrai: " <> Text.unpack amendment)
  replaceState states target state {stateRecord = record}

scopeState :: Importer -> IORef [AdrState] -> Int -> Fixture.AdrKey -> [Text.Text] -> IO ()
scopeState importer states operation target plannedScopes = do
  state <- stateAt states target
  edge <- checked "scope connection" (mkConnectionId (identifier 'C' (200000 + operation)))
  operationId <- checked "scope operation" (mkOperationId (identifier 'O' operation))
  actor <- checked "actor" (mkActor ServiceActor "large-stress-generator" Nothing)
  additions <- traverse (checked "planned scope" . mkScopePattern) plannedScopes
  unless (not (null additions)) $ assertFailure "planned scope operation must provide additions"
  basis <- requireImporterBasis importer
  let effective = Set.toList (Set.fromList (stateScopes state) `Set.union` Set.fromList additions)
      record = ManagedConnection (ConnectionRecord edge (AppliesToConnection (AppliesToPayload (stateAdr state) [stateScope state] "expand" additions [] effective)) "Stress scope expansion.\n")
  files <- traverse (sealMember actor basis operationId) [(record, "scope.expand", [ProvenanceConnection (stateScope state)])]
  fastCommit importer files ("adrai: apply planned scope to " <> show target)
  replaceState states target state {stateScope = edge, stateScopes = effective}

domainState :: Importer -> IORef [AdrState] -> Int -> Fixture.AdrKey -> [Text.Text] -> IO ()
domainState importer states operation target plannedDomains = do
  state <- stateAt states target
  edge <- checked "domain connection" (mkConnectionId (identifier 'C' (300000 + operation)))
  operationId <- checked "domain operation" (mkOperationId (identifier 'O' operation))
  actor <- checked "actor" (mkActor ServiceActor "large-stress-generator" Nothing)
  domains <- traverse (checked "planned domain" . mkDomain) plannedDomains
  unless (not (null domains)) $ assertFailure "planned domain operation must provide refinements"
  basis <- requireImporterBasis importer
  refinements <- traverse (checked "planned domain refinement" . uncurry domainRefinement) (zip (stateDomains state) domains)
  unless (length refinements == length domains && length domains == length (stateDomains state)) $
    assertFailure "planned domain refinement must replace each current domain"
  let record = ManagedConnection (ConnectionRecord edge (DomainsConnection (DomainsPayload (stateAdr state) [stateDomain state] "refine" domains (stateDomains state) domains refinements)) "Stress domain refinement.\n")
  files <- traverse (sealMember actor basis operationId) [(record, "domain.refine", [ProvenanceConnection (stateDomain state)])]
  fastCommit importer files ("adrai: apply planned domain to " <> show target)
  replaceState states target state {stateDomain = edge, stateDomains = domains}

obsoleteState :: Importer -> IORef [AdrState] -> Int -> Fixture.AdrKey -> IO ()
obsoleteState importer states operation target = do
  state <- stateAt states target
  edge <- checked "status connection" (mkConnectionId (identifier 'C' (400000 + operation)))
  operationId <- checked "obsolete operation" (mkOperationId (identifier 'O' operation))
  actor <- checked "actor" (mkActor ServiceActor "large-stress-generator" Nothing)
  basis <- requireImporterBasis importer
  let record = ManagedConnection (ConnectionRecord edge (StatusConnection (StatusPayload (stateAdr state) [stateStatus state] StatusObsolete [stateRecord state] Nothing)) "Stress retirement.\n")
  files <- traverse (sealMember actor basis operationId) [(record, "decision.obsolete", [ProvenanceConnection (stateStatus state), ProvenanceRecord (stateRecord state)])]
  fastCommit importer files ("adrai: obsolete planned ADR " <> show target)
  replaceState states target state {stateStatus = edge}

sealMember :: Actor -> GitOid -> OperationId -> (ManagedRecord, Text.Text, [ProvenanceObjectId]) -> IO (FilePath, BS.ByteString)
sealMember = sealMemberOnBranch "main"

sealMemberOnBranch :: Text.Text -> Actor -> GitOid -> OperationId -> (ManagedRecord, Text.Text, [ProvenanceObjectId]) -> IO (FilePath, BS.ByteString)
sealMemberOnBranch branch actor basis operation (record, eventText, parents) = do
  semantic <- checked "semantic" (renderManagedSemantic record)
  event <- checked "event" (mkEventKind eventText)
  anchor <- checked "line anchor" (mkLineAnchor "stress@logical\nrecord" basis)
  capsule <- checked "capsule" (mkProvenanceCapsule ProvenanceCapsuleInput
    { capsuleInputOperationId = operation, capsuleInputObjectId = managedObject record, capsuleInputEventKind = event
    , capsuleInputActor = actor, capsuleInputTimestampMs = 1700000000000, capsuleInputBasis = basis, capsuleInputParents = parents
    , capsuleInputBranchHint = Just branch, capsuleInputUpstreamHint = Nothing, capsuleInputLineAnchors = [anchor]
    , capsuleInputSemanticDigest = semanticDigest semantic, capsuleInputToolVersion = "adrai/1.0.0", capsuleInputDigests = ProvenanceInputs Nothing Nothing Nothing })
  bytes <- checked "sealed document" (sealManagedDocument record capsule)
  path <- checked "canonical path" (canonicalManagedPath (configManagedPaths defaultConfig) record)
  pure (Text.unpack (repoPathText path), bytes)

managedObject :: ManagedRecord -> ProvenanceObjectId
managedObject (ManagedDecision decision) = ProvenanceRecord (decisionRecord decision)
managedObject (ManagedConnection connection) = ProvenanceConnection (connectionRecordId connection)

startImporter :: FilePath -> IO Importer
startImporter repository = do
  profileCounters <- currentProfileCounters
  started <- getMonotonicTimeNSec
  (Just input, Just output, Just errors, processHandle) <- createProcess (proc "git" ["-C", repository, "fast-import", "--quiet", "--force"]) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  mark <- newIORef 0
  basisRef <- newIORef Nothing
  lastCommitMark <- newIORef Nothing
  pure (Importer input output mark basisRef lastCommitMark processHandle errors profileCounters started)

withImporter :: FilePath -> (Importer -> IO value) -> IO value
withImporter repository = bracket (startImporter repository) finishImporter

fastCommit :: Importer -> [(FilePath, BS.ByteString)] -> String -> IO ()
fastCommit importer files message = do
  parentMark <- readIORef (importerLastCommitMark importer)
  _ <- fastCommitAt importer "refs/heads/main" (maybe [] pure parentMark) files message
  pure ()

fastCommitAt :: Importer -> String -> [Int] -> [(FilePath, BS.ByteString)] -> String -> IO Int
fastCommitAt importer ref parents files message = do
  blobMarks <- mapM writeBlob files
  commitMark <- freshMark importer
  let input = importerInput importer
  writeProtocol input ("commit " <> ref <> "\nmark :" <> show commitMark <> "\nauthor ADRAI Stress <stress@adrai.invalid> 1700000000 +0000\ncommitter ADRAI Stress <stress@adrai.invalid> 1700000000 +0000\n")
  writeData input (BS8.pack message)
  case parents of
    [] -> pure ()
    parent : merged -> do
      writeProtocol input ("from :" <> show parent <> "\n")
      forM_ merged $ \mergeParent -> writeProtocol input ("merge :" <> show mergeParent <> "\n")
  forM_ blobMarks $ \(path, mark) -> writeProtocol input ("M 100644 :" <> show mark <> " " <> path <> "\n")
  writeProtocol input ("\nget-mark :" <> show commitMark <> "\n")
  lineResult <- try (hGetLine (importerOutput importer)) :: IO (Either SomeException String)
  importedLine <-
    case lineResult of
      Right value -> pure value
      Left problem -> do
        diagnostics <- hGetContents (importerError importer)
        fail ("git fast-import ended before returning a commit mark: " <> show problem <> "\n" <> diagnostics)
  let oidText = Text.strip (Text.pack importedLine)
  oid <- checked "fast-import commit OID" (mkGitOid oidText)
  writeIORef (importerBasis importer) (Just oid)
  writeIORef (importerLastCommitMark importer) (Just commitMark)
  pure commitMark
  where
    writeBlob (path, bytes) = do
      mark <- freshMark importer
      writeProtocol (importerInput importer) ("blob\nmark :" <> show mark <> "\n")
      writeData (importerInput importer) bytes
      pure (path, mark)

writeData :: Handle -> BS.ByteString -> IO ()
writeData handle bytes = do
  writeProtocol handle ("data " <> show (BS.length bytes) <> "\n")
  BS.hPut handle bytes
  BS.hPut handle "\n"
  hFlush handle

writeProtocol :: Handle -> String -> IO ()
writeProtocol handle value = do
  BS.hPut handle (BS8.pack value)
  hFlush handle

freshMark :: Importer -> IO Int
freshMark importer = atomicModifyIORef' (importerMark importer) (\mark -> let next = mark + 1 in (next, next))

requireImporterLastCommitMark :: Importer -> IO Int
requireImporterLastCommitMark importer = do
  mark <- readIORef (importerLastCommitMark importer)
  maybe (assertFailure "stress branch transition has no imported parent" >> fail "unreachable") pure mark

finishImporter :: Importer -> IO ()
finishImporter importer = do
  finished <- getProcessExitCode (importerProcess importer)
  status <-
    case finished of
      Just value -> pure value
      Nothing -> do
        graceful <- try (writeProtocol (importerInput importer) "done\n" >> hClose (importerInput importer)) :: IO (Either SomeException ())
        case graceful of
          Right () -> waitForProcess (importerProcess importer)
          Left _ -> do
            terminateProcess (importerProcess importer)
            waitForProcess (importerProcess importer)
  _ <- try (hClose (importerOutput importer)) :: IO (Either SomeException ())
  _ <- try (hClose (importerError importer)) :: IO (Either SomeException ())
  forM_ (importerProfileCounters importer) $ \counters -> recordGitChild counters "git-fast-import" (importerStartedAt importer)
  case status of
    ExitSuccess -> pure ()
    ExitFailure code -> assertFailure ("git fast-import failed during cleanup (" <> show code <> ")")

emitNoise :: Importer -> Int -> Text.Text -> IO ()
emitNoise importer ordinal plannedLabel =
  fastCommit importer
    [("src/noise/module_" <> Text.unpack (padded 5 ordinal) <> ".txt", TextEncoding.encodeUtf8 (plannedLabel <> "\n"))]
    (Text.unpack plannedLabel)

stateAt :: IORef [AdrState] -> Fixture.AdrKey -> IO AdrState
stateAt states (Fixture.AdrKey index) = do
  values <- readIORef states
  case drop index values of
    value : _ -> pure value
    [] -> assertFailure ("stress state " <> show index <> " is absent") >> fail "unreachable"

replaceState :: IORef [AdrState] -> Fixture.AdrKey -> AdrState -> IO ()
replaceState states (Fixture.AdrKey index) replacement = modifyIORef' states (\values -> take index values <> [replacement] <> drop (index + 1) values)

renderTemplateBody :: [(Text.Text, Text.Text)] -> Text.Text
renderTemplateBody sections = Text.intercalate "\n\n" ["# " <> heading <> "\n" <> body | (heading, body) <- sections] <> "\n"

domainRefinement :: Domain -> Domain -> Either DomainError DomainRefinement
domainRefinement parent child = parseDomainRefinement (domainText parent <> "=" <> domainText child)

identifier :: Char -> Int -> Text.Text
identifier prefix number = Text.cons prefix (Text.replicate (26 - Text.length digits) "0" <> digits)
  where digits = Text.pack (base32 number)

base32 :: Int -> String
base32 0 = "0"
base32 value = reverse (go value)
  where
    alphabet = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"
    go 0 = []
    go current = alphabet !! (current `mod` 32) : go (current `div` 32)

padded :: Int -> Int -> Text.Text
padded width value = Text.replicate (max 0 (width - Text.length text)) "0" <> text where text = Text.pack (show value)

requireImporterBasis :: Importer -> IO GitOid
requireImporterBasis importer = do
  basis <- readIORef (importerBasis importer)
  case basis of
    Just value -> pure value
    Nothing -> assertFailure "stress operation was emitted before the seed commit" >> fail "unreachable"

addNoiseCommit :: FilePath -> String -> IO ()
addNoiseCommit repository label = do
  let path = "src/post/" <> label <> ".txt"
  createDirectoryIfMissing True (repository </> "src" </> "post")
  BS.writeFile (repository </> path) (BS8.pack (label <> "\n"))
  git repository ["add", "--", path]
  git repository ["commit", "-m", label]

data CompleteProcessTreeSamplePolicy
  = RequireCompleteProcessTreeSample
  | PermitNoCompleteProcessTreeSample
  deriving (Eq, Show)

-- | A process-tree sample is deliberately typed rather than a bare total.  On
-- Windows the rows are retained long enough to make a high-water report
-- attributable without changing the value used by the acceptance gate.
data ProcessTreeSample = ProcessTreeSample
  { processTreeSampleBytes :: Integer,
    processTreeSampleWindows :: Maybe WindowsProcessTreeSnapshot
  }
  deriving (Eq, Show)

-- | The selected rows from one Win32_Process snapshot.  Creation ticks are UTC
-- .NET ticks, which are monotonic enough for the parent/child identity checks
-- below and avoid locale-dependent DMTF parsing in the harness.
data WindowsProcessTreeSnapshot = WindowsProcessTreeSnapshot
  { windowsSampledAt :: Text.Text,
    windowsRootPid :: Integer,
    windowsQueryDurationMilliseconds :: Integer,
    windowsRows :: [WindowsProcessRow]
  }
  deriving (Eq, Show)

data WindowsRootIdentity = WindowsRootIdentity
  { windowsRootIdentityPid :: Integer,
    windowsRootIdentityCreationTicks :: Integer
  }
  deriving (Eq, Show)

data ProcessTreeSamplerFailure
  = RootDisappeared
  | RootIdentityMismatch
  | SnapshotTopologyFailure String
  | SnapshotIdentityFailure String
  | SnapshotOverflow
  | SnapshotJsonFailure String
  | SamplerInvocationFailure String
  | DiagnosticPersistenceFailure String
  deriving (Eq, Show)

data WindowsProcessRow = WindowsProcessRow
  { windowsProcessPid :: Integer,
    windowsProcessParentPid :: Integer,
    windowsProcessCreationTicks :: Integer,
    windowsProcessWorkingSetBytes :: Integer,
    windowsProcessName :: Text.Text
  }
  deriving (Eq, Show)

instance Aeson.FromJSON WindowsProcessTreeSnapshot where
  parseJSON = Aeson.withObject "WindowsProcessTreeSnapshot" $ \object ->
    WindowsProcessTreeSnapshot
      <$> object Aeson..: "sampled_at"
      <*> object Aeson..: "root_pid"
      <*> object Aeson..: "query_duration_milliseconds"
      <*> object Aeson..: "rows"

instance Aeson.FromJSON WindowsRootIdentity where
  parseJSON = Aeson.withObject "WindowsRootIdentity" $ \object ->
    WindowsRootIdentity
      <$> object Aeson..: "pid"
      <*> object Aeson..: "creation_ticks"

instance Aeson.FromJSON WindowsProcessRow where
  parseJSON = Aeson.withObject "WindowsProcessRow" $ \object ->
    WindowsProcessRow
      <$> object Aeson..: "pid"
      <*> object Aeson..: "ppid"
      <*> object Aeson..: "creation_ticks"
      <*> object Aeson..: "working_set_bytes"
      <*> object Aeson..: "name"

-- | The complete-run watchdog owns process-tree deadlines and cleanup. This
-- functional smoke therefore avoids the former large-capacity CIM sampler and
-- invokes only the selected production executable.
runAdraiFunctional :: FilePath -> [String] -> IO String
runAdraiFunctional repository arguments = do
  environment <- getEnvironment
  binDirectory <- getBinDir
  let fallback = binDirectory </> ("adrai" <> executableSuffix)
      candidate = maybe fallback id (lookup "ADRAI_EXE" environment)
  candidateExists <- doesFileExist candidate
  executable <-
    case resolveAdraiExecutable environment fallback (\path -> path == candidate && candidateExists) of
      Left problem -> assertFailure problem >> fail "unreachable"
      Right value -> pure value
  when (isJust (lookup "ADRAI_TEST_EXPECTED_EXE" environment)) $
    BS8.hPutStrLn stderr (renderSelectedExecutableDiagnostic executable)
  (status, output, problem) <-
    readProcessWithExitCode executable (["--repo", repository] <> arguments) ""
  case status of
    ExitSuccess -> pure output
    ExitFailure code -> assertFailure ("adrai command failed (" <> show code <> "): " <> problem) >> fail "unreachable"

resolveAdraiExecutable :: [(String, String)] -> FilePath -> (FilePath -> Bool) -> Either String FilePath
resolveAdraiExecutable environment fallback exists = do
  selected <-
    case lookup "ADRAI_EXE" environment of
      Nothing
        | exists fallback -> Right fallback
        | otherwise -> Left ("package-built executable is absent: " <> fallback)
      Just override
        | null override -> Left "ADRAI_EXE is present but empty"
        | not (isAbsolute override) -> Left ("ADRAI_EXE must be absolute: " <> override)
        | not (exists override) -> Left ("ADRAI_EXE must name an existing executable file: " <> override)
        | otherwise -> Right override
  case lookup "ADRAI_TEST_EXPECTED_EXE" environment of
    Nothing -> Right selected
    Just expected
      | expected == selected -> Right selected
      | otherwise ->
          Left
            ( "ADRAI_TEST_EXPECTED_EXE mismatch: selected executable is "
                <> selected
                <> ", expected "
                <> expected
            )

-- | Render a lossless, single-line harness diagnostic. The selected path is a
-- UTF-8 JSON string so Unicode and control characters remain attributable.
renderSelectedExecutableDiagnostic :: FilePath -> BS.ByteString
renderSelectedExecutableDiagnostic executable =
  "P6-08.2R stress executable: " <> LazyByteString.toStrict (Aeson.encode (Text.pack executable))

executableResolverContract :: IO ()
executableResolverContract = do
  let testRoot = if os == "mingw32" then "C:\\adrai-test" else "/adrai-test"
      fallback = testRoot </> "package" </> ("adrai" <> executableSuffix)
      override = testRoot </> "override" </> ("adrai" <> executableSuffix)
      missing = testRoot </> "missing" </> ("adrai" <> executableSuffix)
      known path = path == fallback || path == override
      resolve environment = resolveAdraiExecutable environment fallback known
      assertRejected name environment expected =
        case resolve environment of
          Left problem -> assertBool (name <> ": " <> problem) (expected `isInfixOf` problem)
          Right selected -> assertFailure (name <> ": expected rejection, selected " <> selected)
  resolve [("ADRAI_EXE", override), ("ADRAI_TEST_EXPECTED_EXE", override)] @?= Right override
  resolve [("ADRAI_TEST_EXPECTED_EXE", fallback)] @?= Right fallback
  assertRejected "empty override" [("ADRAI_EXE", "")] "present but empty"
  assertRejected "relative override" [("ADRAI_EXE", "adrai.exe")] "must be absolute"
  assertRejected "missing override" [("ADRAI_EXE", missing)] "existing executable file"
  assertRejected "expected mismatch" [("ADRAI_EXE", override), ("ADRAI_TEST_EXPECTED_EXE", fallback)] "ADRAI_TEST_EXPECTED_EXE mismatch"
  assertRejected "fallback expected mismatch" [("ADRAI_TEST_EXPECTED_EXE", override)] "ADRAI_TEST_EXPECTED_EXE mismatch"
  let diagnosticPath = testRoot </> "unicode-\x03bb-\x00e5\ncontrol-\ESC"
      diagnosticPrefix = "P6-08.2R stress executable: " :: BS.ByteString
      diagnostic = renderSelectedExecutableDiagnostic diagnosticPath
  assertBool "selected executable diagnostic has its stable prefix" (diagnosticPrefix `BS.isPrefixOf` diagnostic)
  assertBool "selected executable diagnostic must stay on one line" (not (BS.elem 10 diagnostic) && not (BS.elem 13 diagnostic))
  (Aeson.eitherDecodeStrict' (BS.drop (BS.length diagnosticPrefix) diagnostic) :: Either String Text.Text) @?= Right (Text.pack diagnosticPath)

parseMaximumResidency :: String -> Maybe Integer
parseMaximumResidency report = do
  pairs <- readMaybe report :: Maybe [(String, String)]
  raw <- uniqueValue "max_bytes_used" pairs
  decimalInteger raw
  where
    uniqueValue key pairs =
      case [value | (actualKey, value) <- pairs, actualKey == key] of
        [value] -> Just value
        _ -> Nothing

rtsPairListParsingContract :: IO ()
rtsPairListParsingContract = do
  parseMaximumResidency "[(\"bytes allocated\",\"456488\"),(\"max_bytes_used\",\"69064\"),(\"max_mem_in_use_bytes\",\"103809024\")]" @?= Just 69064
  parseMaximumResidency "[(\"max_bytes_used\",\"69x64\")]" @?= Nothing
  parseMaximumResidency "[(\"bytes allocated\",\"456488\")]" @?= Nothing
  parseMaximumResidency "[(\"max_bytes_used\",\"1\"),(\"max_bytes_used\",\"2\")]" @?= Nothing

samplerDeadline :: Maybe Int -> IO () -> IO value -> IO (Maybe value)
samplerDeadline Nothing cleanup action = (Just <$> action) `onException` cleanup
samplerDeadline (Just duration) cleanup action = do
  completed <- timeout duration action `onException` cleanup
  case completed of
    Nothing -> cleanup >> pure Nothing
    Just result -> pure (Just result)

terminalSamplerOutcome :: Maybe ExitCode -> ProcessTreeSamplerFailure -> Either ProcessTreeSamplerFailure ExitCode
terminalSamplerOutcome (Just exitCode) RootDisappeared = Right exitCode
terminalSamplerOutcome _ failure = Left failure

renderProcessTreeSamplerFailure :: ProcessTreeSamplerFailure -> String
renderProcessTreeSamplerFailure RootDisappeared = "root process disappeared while sampling"
renderProcessTreeSamplerFailure RootIdentityMismatch = "root process identity does not match the launched process"
renderProcessTreeSamplerFailure (SnapshotTopologyFailure problem) = "snapshot topology failure: " <> problem
renderProcessTreeSamplerFailure (SnapshotIdentityFailure problem) = "snapshot identity failure: " <> problem
renderProcessTreeSamplerFailure SnapshotOverflow = "snapshot working-set sum overflowed"
renderProcessTreeSamplerFailure (SnapshotJsonFailure problem) = "snapshot JSON failure: " <> problem
renderProcessTreeSamplerFailure (SamplerInvocationFailure problem) = "sampler invocation failure: " <> problem
renderProcessTreeSamplerFailure (DiagnosticPersistenceFailure problem) = "diagnostic persistence failure: " <> problem

recordProcessTreeSample :: Maybe FilePath -> IORef Integer -> IORef Int -> Either ProcessTreeSamplerFailure ProcessTreeSample -> IO (Either ProcessTreeSamplerFailure ())
recordProcessTreeSample _ _ _ (Left failure) = pure (Left failure)
recordProcessTreeSample diagnosticPath peakBytesRef successfulSamplesRef (Right sample) = do
  previousPeak <- readIORef peakBytesRef
  let bytes = processTreeSampleBytes sample
      isNewHighWater = bytes > previousPeak
      exceedsTarget = not (withinProcessTreeTarget bytes)
  persisted <-
    case (diagnosticPath, processTreeSampleWindows sample) of
      (Just path, Just snapshot)
        | isNewHighWater || exceedsTarget -> persistWindowsProcessTreeDiagnostic path bytes snapshot
      _ -> pure (Right ())
  case persisted of
    Left problem -> pure (Left (DiagnosticPersistenceFailure problem))
    Right () -> do
      when isNewHighWater (writeIORef peakBytesRef bytes)
      modifyIORef' successfulSamplesRef (+ 1)
      pure (Right ())

completeProcessTreeSamplesAccepted :: CompleteProcessTreeSamplePolicy -> Int -> Bool
completeProcessTreeSamplesAccepted RequireCompleteProcessTreeSample successfulSamples = successfulSamples > 0
completeProcessTreeSamplesAccepted PermitNoCompleteProcessTreeSample _ = True

windowsDiagnosticRequired :: String -> CompleteProcessTreeSamplePolicy -> Maybe FilePath -> Bool
windowsDiagnosticRequired platform policy diagnosticPath =
  platform == "mingw32" && policy == RequireCompleteProcessTreeSample && isNothing diagnosticPath

-- | Only a required Windows cold sampler may consume the diagnostic
-- environment.  Permit-mode commands deliberately ignore inherited values so
-- a previous cold artifact cannot become a false freshness failure or receive
-- extra rows from exact-like work.
resolveProcessTreeDiagnosticPath :: String -> CompleteProcessTreeSamplePolicy -> [(String, String)] -> IO (Maybe FilePath)
resolveProcessTreeDiagnosticPath platform policy environment
  | platform /= "mingw32" || policy /= RequireCompleteProcessTreeSample = pure Nothing
  | otherwise =
      case lookup "ADRAI_TEST_PROCESS_TREE_DIAGNOSTIC" environment of
        Nothing -> assertFailure "Windows RequireCompleteProcessTreeSample requires ADRAI_TEST_PROCESS_TREE_DIAGNOSTIC" >> fail "unreachable"
        Just path
          | null path -> assertFailure "ADRAI_TEST_PROCESS_TREE_DIAGNOSTIC must be a non-empty absolute path" >> fail "unreachable"
          | not (isAbsolute path) -> assertFailure "ADRAI_TEST_PROCESS_TREE_DIAGNOSTIC must be absolute" >> fail "unreachable"
          | otherwise -> do
              exists <- doesFileExist path
              when exists (assertFailure "ADRAI_TEST_PROCESS_TREE_DIAGNOSTIC must name a fresh file" >> fail "unreachable")
              pure (Just path)

withinProcessTreeTarget :: Integer -> Bool
withinProcessTreeTarget peakBytes = peakBytes <= stressProcessTreeTargetBytes

captureInitialWindowsProcessTreeSample :: Maybe FilePath -> ProcessHandle -> IO (Either ProcessTreeSamplerFailure (WindowsRootIdentity, ProcessTreeSample))
captureInitialWindowsProcessTreeSample diagnosticPath processHandle = do
  before <- getProcessExitCode processHandle
  case before of
    Just _ -> pure (Left RootDisappeared)
    Nothing -> do
      processId <- getPid processHandle
      case processId of
        Nothing -> pure (Left (SamplerInvocationFailure "launched root process ID is unavailable"))
        Just value -> do
          observed <- readWindowsProcessTreeSnapshot diagnosticPath (show value)
          after <- getProcessExitCode processHandle
          case after of
            Just _ -> pure (Left RootDisappeared)
            Nothing ->
              case observed of
                Left failure -> pure (Left failure)
                Right snapshot ->
                  case do
                    identity <- initialWindowsRootIdentity (fromIntegral value) snapshot
                    (bytes, admittedSnapshot) <- admitWindowsProcessTreeSnapshot identity (fromIntegral value) snapshot
                    pure (identity, ProcessTreeSample bytes (Just admittedSnapshot)) of
                    Left failure -> persistInvalidWindowsSnapshot diagnosticPath failure "" (Just snapshot)
                    Right initial -> pure (Right initial)

initialWindowsRootIdentity :: Integer -> WindowsProcessTreeSnapshot -> Either ProcessTreeSamplerFailure WindowsRootIdentity
initialWindowsRootIdentity rootPid snapshot =
  case [row | row <- windowsRows snapshot, windowsProcessPid row == rootPid] of
    [] -> Left RootDisappeared
    [root]
      | windowsProcessCreationTicks root <= 0 -> Left (SnapshotIdentityFailure "PowerShell returned a missing or malformed root creation identity")
      | otherwise -> Right (WindowsRootIdentity rootPid (windowsProcessCreationTicks root))
    _ -> Left (SnapshotIdentityFailure "PowerShell returned an ambiguous duplicate root process identity")

readWindowsProcessTreeSnapshot :: Maybe FilePath -> String -> IO (Either ProcessTreeSamplerFailure WindowsProcessTreeSnapshot)
readWindowsProcessTreeSnapshot diagnosticPath rootPid =
  readWindowsProcessTreeSnapshotWithCimRows diagnosticPath rootPid "Get-CimInstance Win32_Process | Select-Object ProcessId,ParentProcessId,CreationDate,WorkingSetSize,Name"

readWindowsProcessTreeSnapshotWithCimRows :: Maybe FilePath -> String -> String -> IO (Either ProcessTreeSamplerFailure WindowsProcessTreeSnapshot)
readWindowsProcessTreeSnapshotWithCimRows diagnosticPath rootPid cimRows = do
  -- PowerShell treats tokens after @-Command <script>@ as part of that
  -- script, not as @\$args@. Embed the validated numeric pid so sampling
  -- does not silently fail with a parser error and report a useless zero.
  -- One CIM snapshot supplies both topology and working-set values. Unlike a
  -- later Get-Process lookup per PID, a short-lived non-root child cannot
  -- invalidate the whole sample after the tree has been discovered.  The
  -- selected rows retain current creation identities so Haskell can reject a
  -- malformed or temporally impossible topology rather than reporting a low
  -- aggregate for an ambiguous one.
  let root = maybe (-1) id (readNonnegativeInteger rootPid)
      script =
        "$root="
          <> show root
          <> ";$ErrorActionPreference='Stop';if($root -lt 1 -or $root -gt [uint32]::MaxValue){exit 43};$rootPid=[uint32]$root;$watch=[Diagnostics.Stopwatch]::StartNew();$all=@("
          <> cimRows
          <> ");$watch.Stop();$byPid=@{};foreach($p in $all){if($null -eq $p.ProcessId){continue};$processPid=[uint32]$p.ProcessId;if($byPid.ContainsKey($processPid)){exit 44};$byPid[$processPid]=$p};if(-not $byPid.ContainsKey($rootPid)){exit 42};function Convert-SelectedRow($p){if($null -eq $p.ProcessId -or $null -eq $p.ParentProcessId -or $null -eq $p.CreationDate -or $null -eq $p.WorkingSetSize -or $null -eq $p.Name){exit 44};$processPid=[int64]$p.ProcessId;$ppid=[int64]$p.ParentProcessId;$created=([datetime]$p.CreationDate).ToUniversalTime().Ticks;$workingSet=[int64]$p.WorkingSetSize;$name=[string]$p.Name;$nameBytes=[Text.Encoding]::UTF8.GetByteCount($name);if($processPid -lt 1 -or $ppid -lt 0 -or $created -le 0 -or $workingSet -lt 0){exit 44};if($nameBytes -lt 1 -or $nameBytes -gt 256){exit 45};[pscustomobject]@{pid=$processPid;ppid=$ppid;creation_ticks=[int64]$created;working_set_bytes=$workingSet;name=$name}};$accepted=@{};$rootRow=Convert-SelectedRow $byPid[$rootPid];$accepted[[uint32]$rootRow.pid]=$rootRow;do{$added=$false;foreach($p in $all){if($null -eq $p.ParentProcessId){continue};$parentPid=[uint32]$p.ParentProcessId;if(-not $accepted.ContainsKey($parentPid)){continue};$candidate=Convert-SelectedRow $p;$candidatePid=[uint32]$candidate.pid;if($accepted.ContainsKey($candidatePid)){continue};$parent=$accepted[$parentPid];if($candidate.creation_ticks -lt $rootRow.creation_ticks -or $candidate.creation_ticks -lt $parent.creation_ticks){continue};$accepted[$candidatePid]=$candidate;$added=$true}}while($added);$rows=@($accepted.Values | Sort-Object pid);if($rows.Count -gt 256){exit 45};$json=([pscustomobject]@{sampled_at=[DateTime]::UtcNow.ToString('o');root_pid=[int64]$rootPid;query_duration_milliseconds=[int64][Math]::Ceiling($watch.Elapsed.TotalMilliseconds);rows=$rows}|ConvertTo-Json -Depth 3 -Compress);$bytes=[Text.Encoding]::UTF8.GetBytes($json);if($bytes.Length -gt 65536){exit 45};$encoded=[Convert]::ToBase64String($bytes).TrimEnd('=').Replace('+','-').Replace('/','_');if($encoded.Length -gt 87382){exit 45};[Console]::Out.Write($encoded)"
  (status, output, _) <- readProcessWithExitCode "powershell" ["-NoProfile", "-NonInteractive", "-Command", script] ""
  case status of
    ExitSuccess -> decodeWindowsProcessTreeSnapshot diagnosticPath output
    ExitFailure 42 -> pure (Left RootDisappeared)
    ExitFailure 44 -> persistInvalidWindowsSnapshot diagnosticPath (SnapshotIdentityFailure "PowerShell returned a row with a missing or malformed process identity") output Nothing
    ExitFailure 45 -> persistInvalidWindowsSnapshot diagnosticPath (SnapshotIdentityFailure "PowerShell sampler exceeded its configured transport or row bounds") output Nothing
    ExitFailure code -> pure (Left (SamplerInvocationFailure ("PowerShell sampler exited " <> show code)))

decodeWindowsProcessTreeSnapshot :: Maybe FilePath -> String -> IO (Either ProcessTreeSamplerFailure WindowsProcessTreeSnapshot)
decodeWindowsProcessTreeSnapshot diagnosticPath output =
  case decodePowerShellJson output of
    Left failure -> persistInvalidWindowsSnapshot diagnosticPath failure output Nothing
    Right snapshot -> pure (Right snapshot)

decodePowerShellJson :: Aeson.FromJSON value => String -> Either ProcessTreeSamplerFailure value
decodePowerShellJson output
  | length output > maxWindowsProcessTreeRawTransportCharacters = Left (SnapshotJsonFailure "PowerShell emitted an overlong UTF-8 transport")
  | otherwise = do
    encoded <-
      case filter (`notElem` [' ', '\r', '\n', '\t']) output of
        [] -> Left (SnapshotJsonFailure "PowerShell emitted an empty UTF-8 transport")
        value
          | length value > maxWindowsProcessTreeTransportCharacters -> Left (SnapshotJsonFailure "PowerShell emitted an overlong base64url transport")
          | otherwise -> Right (Text.pack value)
    bytes <-
      case decodeBase64Url encoded of
        Left problem -> Left (SnapshotJsonFailure ("PowerShell emitted invalid base64url transport: " <> show problem))
        Right value -> Right value
    case Aeson.eitherDecodeStrict' bytes of
      Left problem -> Left (SnapshotJsonFailure problem)
      Right value -> Right value

validateWindowsProcessTreeSnapshot :: WindowsRootIdentity -> Integer -> WindowsProcessTreeSnapshot -> Either ProcessTreeSamplerFailure Integer
validateWindowsProcessTreeSnapshot expectedIdentity expectedRoot =
  fmap fst . admitWindowsProcessTreeSnapshot expectedIdentity expectedRoot

admitWindowsProcessTreeSnapshot :: WindowsRootIdentity -> Integer -> WindowsProcessTreeSnapshot -> Either ProcessTreeSamplerFailure (Integer, WindowsProcessTreeSnapshot)
admitWindowsProcessTreeSnapshot expectedIdentity expectedRoot snapshot
  | windowsRootPid snapshot /= expectedRoot = Left RootIdentityMismatch
  | windowsRootPid snapshot /= windowsRootIdentityPid expectedIdentity = Left RootIdentityMismatch
  | windowsQueryDurationMilliseconds snapshot < 0 = Left (SnapshotIdentityFailure "PowerShell returned a negative query duration")
  | otherwise = do
      rowsByPid <- foldM insertRow Map.empty (windowsRows snapshot)
      root <- maybe (Left RootDisappeared) Right (Map.lookup expectedRoot rowsByPid)
      validateIdentity "root" root
      unless (windowsProcessCreationTicks root == windowsRootIdentityCreationTicks expectedIdentity) (Left RootIdentityMismatch)
      (selected, pruned) <- selectRows rowsByPid root (Set.singleton expectedRoot) Set.empty
      unless (Set.size selected + Set.size pruned == Map.size rowsByPid) (Left (SnapshotTopologyFailure "PowerShell returned an unexplained process row outside the selected root closure"))
      unless (Set.size selected <= maxWindowsProcessTreeRows) (Left (SnapshotIdentityFailure "PowerShell returned too many selected process rows"))
      total <- foldM (sumWorkingSet rowsByPid root) 0 (Set.toAscList selected)
      admittedRows <- traverse (maybe (Left (SnapshotTopologyFailure "PowerShell selected process identity is absent")) Right . (`Map.lookup` rowsByPid)) (Set.toAscList selected)
      pure (total, snapshot {windowsRows = admittedRows})
  where
    insertRow rows row
      | windowsProcessPid row <= 0 = Left (SnapshotIdentityFailure "PowerShell returned a non-positive process ID")
      | windowsProcessParentPid row < 0 = Left (SnapshotIdentityFailure "PowerShell returned a negative parent process ID")
      | Map.member (windowsProcessPid row) rows = Left (SnapshotIdentityFailure "PowerShell returned an ambiguous duplicate process identity")
      | otherwise = Right (Map.insert (windowsProcessPid row) row rows)
    validateIdentity label row
      | windowsProcessCreationTicks row <= 0 = Left (SnapshotIdentityFailure ("PowerShell returned a missing or malformed " <> label <> " creation identity"))
      | windowsProcessWorkingSetBytes row < 0 = Left (SnapshotIdentityFailure ("PowerShell returned a negative " <> label <> " working set"))
      | Text.null (windowsProcessName row) = Left (SnapshotIdentityFailure ("PowerShell returned a missing " <> label <> " name"))
      | BS.length (TextEncoding.encodeUtf8 (windowsProcessName row)) > maxWindowsProcessTreeNameBytes = Left (SnapshotIdentityFailure ("PowerShell returned an overlong " <> label <> " name"))
      | otherwise = Right ()
    selectRows rows root selected pruned = do
      (selected', pruned') <- foldM (selectChild rows root selected pruned) (selected, pruned) (Map.elems rows)
      if selected' == selected && pruned' == pruned
        then Right (selected, pruned)
        else selectRows rows root selected' pruned'
    selectChild rows root selected pruned (selected', pruned') row
      | windowsProcessPid row `Set.member` selected = Right (selected', pruned')
      | windowsProcessPid row `Set.member` pruned = Right (selected', pruned')
      | windowsProcessParentPid row `Set.member` pruned = Right (selected', Set.insert (windowsProcessPid row) pruned')
      | windowsProcessParentPid row `Set.notMember` selected = Right (selected', pruned')
      | otherwise = do
          parent <- maybe (Left (SnapshotTopologyFailure "PowerShell returned a descendant whose selected parent is absent")) Right (Map.lookup (windowsProcessParentPid row) rows)
          validateIdentity "descendant" row
          validateIdentity "parent" parent
          if windowsProcessCreationTicks row < windowsProcessCreationTicks root || windowsProcessCreationTicks row < windowsProcessCreationTicks parent
            then Right (selected', Set.insert (windowsProcessPid row) pruned')
            else Right (Set.insert (windowsProcessPid row) selected', pruned')
    sumWorkingSet rows root total pid = do
      row <- maybe (Left (SnapshotTopologyFailure "PowerShell selected process identity is absent")) Right (Map.lookup pid rows)
      validateIdentity "selected process" row
      unless (windowsProcessCreationTicks row >= windowsProcessCreationTicks root) (Left (SnapshotIdentityFailure "PowerShell returned a selected process older than the root"))
      if total > toInteger (maxBound :: Int64) - windowsProcessWorkingSetBytes row then Left SnapshotOverflow else Right (total + windowsProcessWorkingSetBytes row)

maxWindowsProcessTreeRows :: Int
maxWindowsProcessTreeRows = 256

maxWindowsProcessTreeNameBytes :: Int
maxWindowsProcessTreeNameBytes = 256

maxWindowsProcessTreeDiagnosticRecordBytes :: Int
maxWindowsProcessTreeDiagnosticRecordBytes = 65536

maxWindowsProcessTreeTransportBytes :: Int
maxWindowsProcessTreeTransportBytes = 65536

maxWindowsProcessTreeTransportCharacters :: Int
maxWindowsProcessTreeTransportCharacters =
  let paddedCharacters = ((maxWindowsProcessTreeTransportBytes + 2) `div` 3) * 4
   in case maxWindowsProcessTreeTransportBytes `mod` 3 of
        0 -> paddedCharacters
        1 -> paddedCharacters - 2
        _ -> paddedCharacters - 1

maxWindowsProcessTreeRawTransportCharacters :: Int
maxWindowsProcessTreeRawTransportCharacters = maxWindowsProcessTreeTransportCharacters + 16

persistWindowsProcessTreeDiagnostic :: FilePath -> Integer -> WindowsProcessTreeSnapshot -> IO (Either String ())
persistWindowsProcessTreeDiagnostic path total snapshot = do
  case encodeWindowsProcessTreeDiagnostic total snapshot of
    Left problem -> pure (Left problem)
    Right rendered -> appendWindowsProcessTreeDiagnostic path rendered

persistInvalidWindowsSnapshot :: Maybe FilePath -> ProcessTreeSamplerFailure -> String -> Maybe WindowsProcessTreeSnapshot -> IO (Either ProcessTreeSamplerFailure value)
persistInvalidWindowsSnapshot diagnosticPath failure rawTransport snapshot =
  case diagnosticPath of
    Nothing -> pure (Left failure)
    Just path -> do
      attempted <- case encodeInvalidWindowsProcessTreeDiagnostic failure rawTransport snapshot of
        Left problem -> pure (Left problem)
        Right rendered -> appendWindowsProcessTreeDiagnostic path rendered
      pure $
        case attempted of
          Left problem -> Left (DiagnosticPersistenceFailure problem)
          Right () -> Left failure

appendWindowsProcessTreeDiagnostic :: FilePath -> BS.ByteString -> IO (Either String ())
appendWindowsProcessTreeDiagnostic path rendered = do
  attempted <-
    try $ do
      createDirectoryIfMissing True (takeDirectory path)
      BS8.appendFile path (rendered <> "\n")
  pure $
    case attempted of
      Left exception -> Left ("unable to persist Windows process-tree diagnostic: " <> show (exception :: SomeException))
      Right () -> Right ()

encodeWindowsProcessTreeDiagnostic :: Integer -> WindowsProcessTreeSnapshot -> Either String BS.ByteString
encodeWindowsProcessTreeDiagnostic total snapshot = do
  value <- windowsProcessTreeDiagnostic total snapshot
  boundedDiagnosticEncoding value

encodeInvalidWindowsProcessTreeDiagnostic :: ProcessTreeSamplerFailure -> String -> Maybe WindowsProcessTreeSnapshot -> Either String BS.ByteString
encodeInvalidWindowsProcessTreeDiagnostic failure rawTransport snapshot = do
  value <- invalidWindowsProcessTreeDiagnostic failure rawTransport snapshot
  boundedDiagnosticEncoding value

boundedDiagnosticEncoding :: Aeson.Value -> Either String BS.ByteString
boundedDiagnosticEncoding value =
  let rendered = LazyByteString.toStrict (Aeson.encode value)
   in if BS.length rendered > maxWindowsProcessTreeDiagnosticRecordBytes
        then Left "Windows process-tree diagnostic exceeds its bounded record size"
        else Right rendered

windowsProcessTreeDiagnostic :: Integer -> WindowsProcessTreeSnapshot -> Either String Aeson.Value
windowsProcessTreeDiagnostic total snapshot = do
  unless (length (windowsRows snapshot) <= maxWindowsProcessTreeRows) (Left "Windows process-tree diagnostic has too many process rows")
  rows <- traverse safeDiagnosticRow (windowsRows snapshot)
  let actualTotal = sum (map windowsProcessWorkingSetBytes (windowsRows snapshot))
  unless (actualTotal == total) (Left "Windows process-tree diagnostic total does not match its row sum")
  pure
    ( Aeson.object
        [ "sampled_at" Aeson..= windowsSampledAt snapshot,
          "root_pid" Aeson..= windowsRootPid snapshot,
          "query_duration_milliseconds" Aeson..= windowsQueryDurationMilliseconds snapshot,
          "working_set_bytes" Aeson..= total,
          "rows" Aeson..= rows
        ]
    )

invalidWindowsProcessTreeDiagnostic :: ProcessTreeSamplerFailure -> String -> Maybe WindowsProcessTreeSnapshot -> Either String Aeson.Value
invalidWindowsProcessTreeDiagnostic failure rawTransport snapshot = do
  safeSnapshot <- traverse diagnosticSnapshotWithoutCommands snapshot
  pure
    ( Aeson.object
        [ "failure" Aeson..= renderProcessTreeSamplerFailure failure,
          "raw_transport_utf8_bytes" Aeson..= BS.length rawBytes,
          "raw_transport_sha256" Aeson..= encodeBase64Url (digestBytes (sha256Digest rawBytes)),
          "snapshot" Aeson..= maybe Aeson.Null id safeSnapshot
        ]
    )
  where
    rawBytes = TextEncoding.encodeUtf8 (Text.pack rawTransport)

diagnosticSnapshotWithoutCommands :: WindowsProcessTreeSnapshot -> Either String Aeson.Value
diagnosticSnapshotWithoutCommands value = do
  unless (length (windowsRows value) <= maxWindowsProcessTreeRows) (Left "Windows process-tree diagnostic has too many process rows")
  rows <- traverse safeDiagnosticRow (windowsRows value)
  pure
    ( Aeson.object
        [ "sampled_at" Aeson..= windowsSampledAt value,
          "root_pid" Aeson..= windowsRootPid value,
          "query_duration_milliseconds" Aeson..= windowsQueryDurationMilliseconds value,
          "rows" Aeson..= rows
        ]
    )

safeDiagnosticRow :: WindowsProcessRow -> Either String Aeson.Value
safeDiagnosticRow row = do
  name <- safeDiagnosticProcessName (windowsProcessName row)
  pure $
    Aeson.object
      [ "pid" Aeson..= windowsProcessPid row,
        "ppid" Aeson..= windowsProcessParentPid row,
        "creation_ticks" Aeson..= windowsProcessCreationTicks row,
        "working_set_bytes" Aeson..= windowsProcessWorkingSetBytes row,
        "name" Aeson..= name
      ]

safeDiagnosticProcessName :: Text.Text -> Either String Text.Text
safeDiagnosticProcessName name
  | Text.null name = Left "Windows process-tree diagnostic row has an empty process name"
  | BS.length (TextEncoding.encodeUtf8 name) > maxWindowsProcessTreeNameBytes = Left "Windows process-tree diagnostic row has an overlong process name"
  | Text.any (`elem` ['/', '\\', ':', '\r', '\n', '\0']) name = Left "Windows process-tree diagnostic row has an unsafe process name"
  | otherwise = Right name

windowsProcessTreeTransportPayload :: WindowsProcessTreeSnapshot -> Aeson.Value
windowsProcessTreeTransportPayload snapshot =
  Aeson.object
    [ "sampled_at" Aeson..= windowsSampledAt snapshot,
      "root_pid" Aeson..= windowsRootPid snapshot,
      "query_duration_milliseconds" Aeson..= windowsQueryDurationMilliseconds snapshot,
      "rows" Aeson..= map renderTransportRow (windowsRows snapshot)
    ]
  where
    renderTransportRow row =
      Aeson.object
        [ "pid" Aeson..= windowsProcessPid row,
          "ppid" Aeson..= windowsProcessParentPid row,
          "creation_ticks" Aeson..= windowsProcessCreationTicks row,
          "working_set_bytes" Aeson..= windowsProcessWorkingSetBytes row,
          "name" Aeson..= windowsProcessName row
        ]

processTreeWorkingSetFromPs :: Maybe Integer -> String -> Either ProcessTreeSamplerFailure Integer
processTreeWorkingSetFromPs Nothing _ = Left (SamplerInvocationFailure "process ID is malformed")
processTreeWorkingSetFromPs (Just root) output = do
  processes <- traverse parseProcess (map words (lines output))
  let tree = processTree root processes
  if any (\(pid, _, _) -> pid == root) tree
    then foldM addWorkingSet 0 [rss | (_, _, rss) <- tree]
    else Left RootDisappeared
  where
    addWorkingSet total rss
      | rss > toInteger (maxBound :: Int64) `div` 1024 = Left SnapshotOverflow
      | total > toInteger (maxBound :: Int64) - rss * 1024 = Left SnapshotOverflow
      | otherwise = Right (total + rss * 1024)
    parseProcess [pidText, parentText, rssText] =
      case (readNonnegativeInteger pidText, readNonnegativeInteger parentText, readNonnegativeInteger rssText) of
        (Just pid, Just parent, Just rss) -> Right (pid, parent, rss)
        _ -> Left (SnapshotTopologyFailure "ps returned a malformed process row")
    parseProcess _ = Left (SnapshotTopologyFailure "ps returned an incomplete process row")

processTree :: Integer -> [(Integer, Integer, Integer)] -> [(Integer, Integer, Integer)]
processTree root processes = roots <> descendants [root] processes
  where
    roots = [process | process@(pid, _, _) <- processes, pid == root]
    descendants [] _ = []
    descendants parents remaining =
      let children = [process | process@(_, parent, _) <- remaining, parent `elem` parents]
          childPids = [pid | (pid, _, _) <- children]
          nextRemaining = filter (\(pid, _, _) -> pid `notElem` childPids) remaining
       in children <> descendants childPids nextRemaining

readNonnegativeInteger :: String -> Maybe Integer
readNonnegativeInteger raw =
  case words raw of
    token : _ -> parse token
    [] -> Nothing
  where
    parse token =
      case reads token of
        [(value, "")] | value >= (0 :: Integer) -> Just value
        _ -> Nothing

-- | A failed @taskkill /T@ can be a harmless root-exit race only after both
-- the root and every descendant observed immediately before the command have
-- been verified gone.  A failed topology snapshot is deliberately not
-- treated as an empty tree: accepting it would turn a sampler failure into a
-- possible orphan-process leak.
taskkillFailureResult :: Int -> String -> Maybe ExitCode -> Either String [Integer] -> Either String ()
taskkillFailureResult code problem exited descendantResult =
  case exited of
    Nothing -> Left failurePrefix
    Just _ ->
      case descendantResult of
        Left verificationProblem -> Left (failurePrefix <> "; root exited but descendant verification failed: " <> verificationProblem)
        Right [] -> Right ()
        Right survivors -> Left (failurePrefix <> "; root exited but known descendant processes survived: " <> show survivors)
  where
    failurePrefix = "taskkill failed (" <> show code <> "): " <> problem

processTreeSamplePolicyContract :: IO ()
processTreeSamplePolicyContract = do
  completeProcessTreeSamplesAccepted RequireCompleteProcessTreeSample 0 @?= False
  completeProcessTreeSamplesAccepted PermitNoCompleteProcessTreeSample 0 @?= True
  completeProcessTreeSamplesAccepted RequireCompleteProcessTreeSample 1 @?= True
  assertBool "Windows required sampler rejects an absent diagnostic target" (windowsDiagnosticRequired "mingw32" RequireCompleteProcessTreeSample Nothing)
  assertBool "Windows required sampler accepts a diagnostic target" (not (windowsDiagnosticRequired "mingw32" RequireCompleteProcessTreeSample (Just "C:\\temp\\sampler.jsonl")))
  assertBool "POSIX sampler does not require a Windows diagnostic target" (not (windowsDiagnosticRequired "linux" RequireCompleteProcessTreeSample Nothing))
  withSystemTempDirectory "adrai-process-tree-diagnostic-lifecycle" $ \directory -> do
    let diagnosticPath = directory </> "cold-process-tree.jsonl"
        inherited = [("ADRAI_TEST_PROCESS_TREE_DIAGNOSTIC", diagnosticPath)]
        snapshot = WindowsProcessTreeSnapshot "2026-08-28T10:00:00Z" 1 0 [WindowsProcessRow 1 0 1 1 "adrai.exe"]
        sample = ProcessTreeSample 1 (Just snapshot)
    requiredPath <- resolveProcessTreeDiagnosticPath "mingw32" RequireCompleteProcessTreeSample inherited
    requiredPath @?= Just diagnosticPath
    coldPeak <- newIORef 0
    coldSamples <- newIORef 0
    coldRecorded <- recordProcessTreeSample requiredPath coldPeak coldSamples (Right sample)
    coldRecorded @?= Right ()
    assertBool "required cold sampler must create its fresh diagnostic" =<< doesFileExist diagnosticPath
    coldArtifact <- BS.readFile diagnosticPath
    permitPath <- resolveProcessTreeDiagnosticPath "mingw32" PermitNoCompleteProcessTreeSample inherited
    permitPath @?= Nothing
    exactPeak <- newIORef 0
    exactSamples <- newIORef 0
    exactRecorded <- recordProcessTreeSample permitPath exactPeak exactSamples (Right sample)
    exactRecorded @?= Right ()
    BS.readFile diagnosticPath >>= (@?= coldArtifact)

processTreeSamplerContract :: IO ()
processTreeSamplerContract = do
  processTreeWorkingSetFromPs (Just 10) "10 1 0\n11 10 7\n12 11 3\n" @?= Right (10 * 1024)
  processTreeWorkingSetFromPs (Just 10) "11 10 7\n" @?= Left RootDisappeared
  processTreeWorkingSetFromPs (Just 10) "10 1 invalid\n" @?= Left (SnapshotTopologyFailure "ps returned a malformed process row")
  processTreeWorkingSetFromPs (Just 10) ("10 1 " <> show (toInteger (maxBound :: Int64)) <> "\n") @?= Left SnapshotOverflow
  readNonnegativeInteger "0\n" @?= Just 0
  readNonnegativeInteger "not-a-number\n" @?= Nothing
  assertBool "process-tree threshold accepts its exact boundary" (withinProcessTreeTarget stressProcessTreeTargetBytes)
  assertBool "process-tree threshold rejects excess working set" (not (withinProcessTreeTarget (stressProcessTreeTargetBytes + 1)))
  peak <- newIORef 0
  samples <- newIORef 0
  successfulSample <- recordProcessTreeSample Nothing peak samples (Right (ProcessTreeSample 0 Nothing))
  successfulSample @?= Right ()
  readIORef peak >>= (@?= 0)
  readIORef samples >>= (@?= 1)
  incompleteSample <- recordProcessTreeSample Nothing peak samples (Left (SnapshotTopologyFailure "incomplete tree"))
  incompleteSample @?= Left (SnapshotTopologyFailure "incomplete tree")
  readIORef samples >>= (@?= 1)
  terminalSamplerOutcome (Just ExitSuccess) RootDisappeared @?= Right ExitSuccess
  terminalSamplerOutcome Nothing RootDisappeared @?= Left RootDisappeared
  forM_ [RootIdentityMismatch, SnapshotTopologyFailure "bad topology", SnapshotIdentityFailure "bad identity", SnapshotOverflow, SnapshotJsonFailure "bad JSON", SamplerInvocationFailure "bad sampler", DiagnosticPersistenceFailure "bad diagnostic"] $ \failure ->
    terminalSamplerOutcome (Just ExitSuccess) failure @?= Left failure
  assertBool "PermitNoCompleteProcessTreeSample accepts a quick successful root-disappeared terminal race" (completeProcessTreeSamplesAccepted PermitNoCompleteProcessTreeSample 0)
  timedCleanup <- newIORef (0 :: Int)
  timedCapture <- samplerDeadline (Just 1000) (modifyIORef' timedCleanup (+ 1)) (threadDelay 1000000 >> pure ())
  timedCapture @?= Nothing
  readIORef timedCleanup >>= (@?= 1)
  thrownCleanup <- newIORef (0 :: Int)
  thrown <-
    try
      ( samplerDeadline
          (Just 1000000)
          (modifyIORef' thrownCleanup (+ 1))
          (throwIO (userError "injected initial-snapshot failure") :: IO ())
      ) :: IO (Either SomeException (Maybe ()))
  case thrown of
    Left _ -> pure ()
    Right _ -> assertFailure "an injected root-capture exception must propagate"
  readIORef thrownCleanup >>= (@?= 1)

windowsProcessTreeSamplerContract :: IO ()
windowsProcessTreeSamplerContract = do
  let row pid parent creation workingSet name = WindowsProcessRow pid parent creation workingSet name
      secret = "SUPER_SECRET_never_serialized"
      root = row 10 1 100 5 "adrai.exe"
      child = row 11 10 101 7 "git.exe"
      grandchild = row 12 11 102 3 "conhost.exe"
      sibling = row 13 10 101 2 "conhost.exe"
      finalChild = row 14 12 103 4 "git.exe"
      staleCsrss = row 20 12 50 100 "csrss.exe"
      staleWininit = row 21 12 51 101 "wininit.exe"
      staleGrandchild = row 22 20 52 102 "services.exe"
      staleDescendants = [row (1000 + index) 20 (53 + index) 1000 "stale-service.exe" | index <- [1 .. fromIntegral maxWindowsProcessTreeRows + 1]]
      legitimateHighWater = row 30 14 104 (stressProcessTreeTargetBytes + 1) "legitimate-high-water.exe"
      identity = WindowsRootIdentity 10 100
      snapshot rows = WindowsProcessTreeSnapshot "2026-08-28T10:00:00Z" 10 0 rows
      validate = validateWindowsProcessTreeSnapshot identity 10
      admit = admitWindowsProcessTreeSnapshot identity 10
      reject label expected result =
        case result of
          Left problem -> assertBool (label <> ": " <> renderProcessTreeSamplerFailure problem) (expected `isInfixOf` renderProcessTreeSamplerFailure problem)
          Right bytes -> assertFailure (label <> ": expected rejection, got " <> show bytes)
  validate (snapshot [root, child, grandchild]) @?= Right 15
  validate (snapshot [root, child {windowsProcessCreationTicks = 100}]) @?= Right 12
  validate (snapshot [root, child {windowsProcessCreationTicks = 99}, row 15 11 0 (-1) ""]) @?= Right 5
  let finalThreeShaped = snapshot [root, child, grandchild, sibling, finalChild, staleCsrss, staleWininit, staleGrandchild]
  case admit finalThreeShaped of
    Left problem -> assertFailure ("PID-reuse pruning must admit the current closure: " <> renderProcessTreeSamplerFailure problem)
    Right (bytes, admitted) -> do
      bytes @?= 21
      windowsRows admitted @?= [root, child, grandchild, sibling, finalChild]
      windowsProcessTreeDiagnostic bytes admitted @?= windowsProcessTreeDiagnostic 21 (snapshot [root, child, grandchild, sibling, finalChild])
  case admit (snapshot ([root, child, grandchild, sibling, finalChild, legitimateHighWater, staleCsrss] <> staleDescendants)) of
    Left problem -> assertFailure ("stale descendants must prune before the selected-row bound: " <> renderProcessTreeSamplerFailure problem)
    Right (bytes, admitted) -> do
      bytes @?= 21 + stressProcessTreeTargetBytes + 1
      windowsRows admitted @?= [root, child, grandchild, sibling, finalChild, legitimateHighWater]
      assertBool "the legitimate high-water child remains attributable" (bytes > stressProcessTreeTargetBytes)
  initialPeak <- newIORef 0
  initialSamples <- newIORef 0
  initialRecorded <- recordProcessTreeSample Nothing initialPeak initialSamples (Right (ProcessTreeSample 15 (Just (snapshot [root, child, grandchild]))))
  initialRecorded @?= Right ()
  readIORef initialPeak >>= (@?= 15)
  readIORef initialSamples >>= (@?= 1)
  reject "absent root" "root process disappeared" (validate (snapshot [child]))
  reject "root identity mismatch" "root process identity" (validateWindowsProcessTreeSnapshot (WindowsRootIdentity 10 99) 10 (snapshot [root, child]))
  validate (snapshot [root, child {windowsProcessCreationTicks = 99}]) @?= Right 5
  validate (snapshot [root, child {windowsProcessCreationTicks = 102}, grandchild {windowsProcessCreationTicks = 101}]) @?= Right 12
  reject "missing creation identity" "creation identity" (validate (snapshot [root, child {windowsProcessCreationTicks = 0}]))
  reject "negative direct descendant working set" "negative descendant working set" (validate (snapshot [root, child {windowsProcessWorkingSetBytes = -1}]))
  reject "overlong direct descendant name" "overlong descendant name" (validate (snapshot [root, child {windowsProcessName = Text.replicate (maxWindowsProcessTreeNameBytes + 1) "x"}]))
  reject "duplicate process identity" "ambiguous duplicate" (validate (snapshot [root, child, child]))
  reject "unexplained disconnected row" "unexplained process row" (validate (snapshot [root, child, row 99 98 0 (-1) ""]))
  reject "aggregate overflow" "overflowed" (validate (snapshot [root {windowsProcessWorkingSetBytes = toInteger (maxBound :: Int64)}, child {windowsProcessWorkingSetBytes = 1}]))
  let rendered = windowsProcessTreeDiagnostic 15 (snapshot [root, child, grandchild])
  case rendered >>= (Aeson.eitherDecodeStrict' . LazyByteString.toStrict . Aeson.encode) of
    Left problem -> assertFailure ("Windows process-tree diagnostic must render as JSON: " <> problem)
    Right value -> do
      valueAt ["working_set_bytes"] value @?= Just (Aeson.Number 15)
      valueAt ["rows"] value @?= Just (Aeson.Array (Vector.fromList [renderedRow root, renderedRow child, renderedRow grandchild]))
      assertBool "Windows process-tree diagnostic must not include the secret sentinel" (not (Text.unpack secret `isInfixOf` show value))
  case windowsProcessTreeDiagnostic 14 (snapshot [root, child, grandchild]) of
    Left _ -> pure ()
    Right _ -> assertFailure "diagnostic total must equal its row sum"
  case windowsProcessTreeDiagnostic 15 (snapshot [root {windowsProcessName = "C:\\unsafe.exe"}, child, grandchild]) of
    Left _ -> pure ()
    Right _ -> assertFailure "unsafe diagnostic names must fail closed"
  case windowsProcessTreeDiagnostic 15 (snapshot (replicate (maxWindowsProcessTreeRows + 1) root)) of
    Left _ -> pure ()
    Right _ -> assertFailure "overlarge diagnostics must fail closed"
  let unicodeSnapshot = snapshot [root {windowsProcessName = "\x03bb-\x00e5.exe"}]
      unicodeTransport = Text.unpack (encodeBase64Url (LazyByteString.toStrict (Aeson.encode (windowsProcessTreeTransportPayload unicodeSnapshot))))
  case decodePowerShellJson unicodeTransport :: Either ProcessTreeSamplerFailure WindowsProcessTreeSnapshot of
    Left problem -> assertFailure ("UTF-8 PowerShell transport must decode: " <> renderProcessTreeSamplerFailure problem)
    Right decoded -> windowsRows decoded @?= windowsRows unicodeSnapshot
  case decodePowerShellJson (replicate (maxWindowsProcessTreeRawTransportCharacters + 1) 'A') :: Either ProcessTreeSamplerFailure WindowsProcessTreeSnapshot of
    Left (SnapshotJsonFailure _) -> pure ()
    other -> assertFailure ("raw transport bound must reject before decode: " <> show other)
  case decodePowerShellJson (replicate (maxWindowsProcessTreeTransportCharacters + 1) 'A') :: Either ProcessTreeSamplerFailure WindowsProcessTreeSnapshot of
    Left (SnapshotJsonFailure _) -> pure ()
    other -> assertFailure ("base64 transport bound must reject before decode: " <> show other)
  reject "overlong selected name" "overlong" (validate (snapshot [root {windowsProcessName = Text.replicate (maxWindowsProcessTreeNameBytes + 1) "x"}]))
  reject "overlarge selected row set" "too many" (validate (snapshot (root : [row (100 + index) 10 (101 + index) 1 "git.exe" | index <- [1 .. fromIntegral maxWindowsProcessTreeRows]])))
  withSystemTempDirectory "adrai-windows-process-tree-diagnostic" $ \directory -> do
    let diagnosticPath = directory </> "high-water.jsonl"
        thresholdSnapshot = snapshot [root {windowsProcessWorkingSetBytes = stressProcessTreeTargetBytes + 1}]
        thresholdSample = ProcessTreeSample (stressProcessTreeTargetBytes + 1) (Just thresholdSnapshot)
    peak <- newIORef 0
    samples <- newIORef 0
    firstPersisted <- recordProcessTreeSample (Just diagnosticPath) peak samples (Right thresholdSample)
    firstPersisted @?= Right ()
    repeatedThresholdBreach <- recordProcessTreeSample (Just diagnosticPath) peak samples (Right thresholdSample)
    repeatedThresholdBreach @?= Right ()
    diagnosticRows <- filter (not . null) . lines . BS8.unpack <$> BS8.readFile diagnosticPath
    length diagnosticRows @?= 2
    forM_ diagnosticRows $ \diagnosticRow ->
      case Aeson.eitherDecodeStrict' (BS8.pack diagnosticRow) :: Either String Aeson.Value of
        Left problem -> assertFailure ("persisted Windows process-tree diagnostic must be JSON: " <> problem)
        Right value -> do
          valueAt ["working_set_bytes"] value @?= Just (Aeson.Number (fromIntegral (stressProcessTreeTargetBytes + 1)))
          assertBool "persisted diagnostic must omit the secret sentinel" (not (Text.unpack secret `isInfixOf` diagnosticRow))
    BS8.writeFile (directory </> "not-a-directory") "blocker"
    blockedPeak <- newIORef 0
    blockedSamples <- newIORef 0
    blocked <- recordProcessTreeSample (Just (directory </> "not-a-directory" </> "diagnostic.jsonl")) blockedPeak blockedSamples (Right thresholdSample)
    case blocked of
      Left problem -> assertBool ("diagnostic persistence failure must be attributable: " <> renderProcessTreeSamplerFailure problem) ("diagnostic persistence failure" `isInfixOf` renderProcessTreeSamplerFailure problem)
      Right () -> assertFailure "diagnostic persistence failure must reject the process-tree sample"
    readIORef blockedPeak >>= (@?= 0)
    readIORef blockedSamples >>= (@?= 0)
    let invalidPath = directory </> "invalid-snapshot.jsonl"
    invalid <- persistInvalidWindowsSnapshot (Just invalidPath) RootIdentityMismatch (Text.unpack secret <> unicodeTransport) (Just unicodeSnapshot) :: IO (Either ProcessTreeSamplerFailure ProcessTreeSample)
    invalid @?= Left RootIdentityMismatch
    invalidRows <- filter (not . null) . lines . BS8.unpack <$> BS8.readFile invalidPath
    case invalidRows of
      [invalidRow] ->
        case Aeson.eitherDecodeStrict' (BS8.pack invalidRow) :: Either String Aeson.Value of
          Left problem -> assertFailure ("invalid snapshot diagnostic must be JSON: " <> problem)
          Right value -> do
            valueAt ["failure"] value @?= Just (Aeson.String "root process identity does not match the launched process")
            assertBool "invalid diagnostic must retain a transport digest" (isJust (valueAt ["raw_transport_sha256"] value))
            assertBool "invalid diagnostic must not retain raw transport" (isNothing (valueAt ["raw_transport"] value))
            assertBool "invalid diagnostic must not leak secret sentinel data" (not (Text.unpack secret `isInfixOf` invalidRow))
      _ -> assertFailure "invalid snapshot persistence must append exactly one diagnostic row"
  where
    renderedRow row =
      Aeson.object
        [ "pid" Aeson..= windowsProcessPid row,
          "ppid" Aeson..= windowsProcessParentPid row,
          "creation_ticks" Aeson..= windowsProcessCreationTicks row,
          "working_set_bytes" Aeson..= windowsProcessWorkingSetBytes row,
          "name" Aeson..= windowsProcessName row
        ]

windowsProcessTreeIntegrationContract :: IO ()
windowsProcessTreeIntegrationContract
  | os /= "mingw32" = pure ()
  | otherwise = withSystemTempDirectory "adrai-windows-process-tree-integration" $ \directory -> do
      let fixtureRows =
            "& { $base=[datetime]::UtcNow; $rows=@("
              <> "[pscustomobject]@{ProcessId=[uint32]10;ParentProcessId=[uint32]1;CreationDate=$base;WorkingSetSize=[int64]5;Name='adrai.exe'},"
              <> "[pscustomobject]@{ProcessId=[uint32]11;ParentProcessId=[uint32]10;CreationDate=$base.AddTicks(1);WorkingSetSize=[int64]7;Name='git.exe'},"
              <> "[pscustomobject]@{ProcessId=[uint32]12;ParentProcessId=[uint32]11;CreationDate=$base.AddTicks(2);WorkingSetSize=[int64]3;Name='conhost.exe'},"
              <> "[pscustomobject]@{ProcessId=[uint32]13;ParentProcessId=[uint32]10;CreationDate=$base.AddTicks(1);WorkingSetSize=[int64]2;Name='conhost.exe'},"
              <> "[pscustomobject]@{ProcessId=[uint32]14;ParentProcessId=[uint32]12;CreationDate=$base.AddTicks(3);WorkingSetSize=[int64]4;Name='git.exe'},"
              <> "[pscustomobject]@{ProcessId=[uint32]20;ParentProcessId=[uint32]12;CreationDate=$base.AddTicks(-10);WorkingSetSize=[int64]100;Name='csrss.exe'},"
              <> "[pscustomobject]@{ProcessId=[uint32]21;ParentProcessId=[uint32]12;CreationDate=$base.AddTicks(-9);WorkingSetSize=[int64]101;Name='wininit.exe'});"
              <> "foreach($i in 1..257){$rows += [pscustomobject]@{ProcessId=[uint32](1000+$i);ParentProcessId=[uint32]20;CreationDate=$base.AddTicks(-8+$i);WorkingSetSize=[int64]1000000;Name='stale-service.exe'}};$rows }"
          malformedDirect property =
            "& { $base=[datetime]::UtcNow; @([pscustomobject]@{ProcessId=[uint32]10;ParentProcessId=[uint32]1;CreationDate=$base;WorkingSetSize=[int64]5;Name='adrai.exe'},"
              <> "[pscustomobject]@{ProcessId=[uint32]11;ParentProcessId=[uint32]10;"
              <> property
              <> "}) }"
          duplicateRows =
            "& { $base=[datetime]::UtcNow; @([pscustomobject]@{ProcessId=[uint32]10;ParentProcessId=[uint32]1;CreationDate=$base;WorkingSetSize=[int64]5;Name='adrai.exe'},"
              <> "[pscustomobject]@{ProcessId=[uint32]11;ParentProcessId=[uint32]10;CreationDate=$base.AddTicks(1);WorkingSetSize=[int64]7;Name='git.exe'},"
              <> "[pscustomobject]@{ProcessId=[uint32]11;ParentProcessId=[uint32]10;CreationDate=$base.AddTicks(2);WorkingSetSize=[int64]7;Name='git.exe'}) }"
          expectScriptFailure label rows = do
            result <- readWindowsProcessTreeSnapshotWithCimRows Nothing "10" rows
            case result of
              Left _ -> pure ()
              Right snapshot -> assertFailure (label <> " must fail in the generated PowerShell sampler, got rows " <> show (windowsRows snapshot))
      fixture <- readWindowsProcessTreeSnapshotWithCimRows Nothing "10" fixtureRows
      case fixture of
        Left failure -> assertFailure ("Windows PowerShell temporal-pruning fixture failed: " <> renderProcessTreeSamplerFailure failure)
        Right snapshot -> do
          map windowsProcessPid (windowsRows snapshot) @?= [10, 11, 12, 13, 14]
          sum (map windowsProcessWorkingSetBytes (windowsRows snapshot)) @?= 21
          identity <-
            case initialWindowsRootIdentity 10 snapshot of
              Left failure -> assertFailure ("Windows PowerShell fixture omitted its root identity: " <> renderProcessTreeSamplerFailure failure) >> fail "unreachable"
              Right value -> pure value
          case admitWindowsProcessTreeSnapshot identity 10 snapshot of
            Left failure -> assertFailure ("Windows PowerShell fixture admitted an invalid closure: " <> renderProcessTreeSamplerFailure failure)
            Right (bytes, admitted) -> do
              bytes @?= 21
              map windowsProcessPid (windowsRows admitted) @?= [10, 11, 12, 13, 14]
      expectScriptFailure "duplicate PID" duplicateRows
      expectScriptFailure "missing direct creation identity" (malformedDirect "CreationDate=$null;WorkingSetSize=[int64]7;Name='git.exe'")
      expectScriptFailure "negative direct working set" (malformedDirect "CreationDate=$base.AddTicks(1);WorkingSetSize=[int64]-1;Name='git.exe'")
      expectScriptFailure "empty direct name" (malformedDirect "CreationDate=$base.AddTicks(1);WorkingSetSize=[int64]7;Name=''")
      let cleanup (_, _, _, processHandle) = do
            status <- getProcessExitCode processHandle
            when (isNothing status) (terminateProcess processHandle)
            _ <- waitForProcess processHandle
            pure ()
          sample (maybeInput, _, _, processHandle) = do
            input <- maybe (assertFailure "Windows integration process omitted its requested stdin pipe" >> fail "unreachable") pure maybeInput
            hClose input
            initial <- captureInitialWindowsProcessTreeSample (Just (directory </> "integration.jsonl")) processHandle
            case initial of
              Left failure -> assertFailure ("Windows integration initial snapshot failed: " <> renderProcessTreeSamplerFailure failure) >> fail "unreachable"
              Right (anchored, observed) ->
                case processTreeSampleWindows observed of
                  Nothing -> assertFailure "Windows integration initial sample omitted its typed snapshot"
                  Just snapshot -> do
                    assertBool "Windows integration initial snapshot must include its anchored root" (any (\row -> windowsProcessPid row == windowsRootIdentityPid anchored && windowsProcessCreationTicks row == windowsRootIdentityCreationTicks anchored) (windowsRows snapshot))
                    processTreeSampleBytes observed @?= sum (map windowsProcessWorkingSetBytes (windowsRows snapshot))
            pure ()
      bracket
        (createProcess ((proc "ping.exe" ["-n", "3", "127.0.0.1"]) {std_in = CreatePipe}))
        cleanup
        sample

taskkillFailureContract :: IO ()
taskkillFailureContract = do
  taskkillFailureResult 1 "ERROR: Access denied" (Just ExitSuccess) (Right []) @?= Right ()
  taskkillFailureResult 1 "ERROR: Access denied" (Just ExitSuccess) (Right [101, 102]) @?= Left "taskkill failed (1): ERROR: Access denied; root exited but known descendant processes survived: [101,102]"
  taskkillFailureResult 1 "ERROR: Access denied" (Just ExitSuccess) (Left "timed out while verifying captured descendant exits") @?= Left "taskkill failed (1): ERROR: Access denied; root exited but descendant verification failed: timed out while verifying captured descendant exits"
  taskkillFailureResult 1 "ERROR: Access denied" Nothing (Right []) @?= Left "taskkill failed (1): ERROR: Access denied"

git :: FilePath -> [String] -> IO ()
git repository arguments = profileGitChild "git" $ do
  (status, _, problem) <- readProcessWithExitCode "git" (["-C", repository] <> arguments) ""
  case status of ExitSuccess -> pure (); ExitFailure code -> assertFailure ("git " <> unwords arguments <> " failed (" <> show code <> "): " <> problem) >> fail "unreachable"

gitStdout :: FilePath -> [String] -> IO String
gitStdout repository arguments = profileGitChild "git" $ do
  (status, output, problem) <- readProcessWithExitCode "git" (["-C", repository] <> arguments) ""
  case status of ExitSuccess -> pure output; ExitFailure code -> assertFailure ("git " <> unwords arguments <> " failed (" <> show code <> "): " <> problem) >> fail "unreachable"

assertContains :: String -> String -> String -> IO ()
assertContains label expected actual = assertBool (label <> " omitted " <> expected <> " from " <> actual) (expected `isInfixOf` compact actual)

jsonIntegerAt :: [Text.Text] -> String -> IO Int
jsonIntegerAt path output =
  case Aeson.eitherDecode (LazyByteString.fromStrict (BS8.pack output)) of
    Left problem -> assertFailure ("expected JSON output: " <> problem) >> fail "unreachable"
    Right value ->
      case valueAt path value of
        Just (Aeson.Number number) ->
          case toBoundedInteger number of
            Just integer -> pure integer
            Nothing -> assertFailure ("JSON number is not integral at " <> show path) >> fail "unreachable"
        _ -> assertFailure ("missing integer JSON field at " <> show path) >> fail "unreachable"

valueAt :: [Text.Text] -> Aeson.Value -> Maybe Aeson.Value
valueAt [] value = Just value
valueAt (key : remaining) (Aeson.Object object) = AesonKeyMap.lookup (AesonKey.fromText key) object >>= valueAt remaining
valueAt _ _ = Nothing

compact :: String -> String
compact = filter (`notElem` [' ', '\t', '\r', '\n'])

assertEqual :: (Eq value, Show value) => value -> value -> String -> IO ()
assertEqual actual expected label = unless (actual == expected) (assertFailure (label <> ": expected " <> show expected <> ", got " <> show actual))

timed :: String -> IO value -> IO value
timed label action = do
  started <- getMonotonicTimeNSec
  result <- action
  finished <- getMonotonicTimeNSec
  BS8.hPutStrLn stderr (BS8.pack ("P6-06G stress timing: " <> label <> " elapsed_ms=" <> show ((finished - started) `div` 1000000) <> " (generous regression observation)"))
  pure result

executableSuffix :: String
executableSuffix | os == "mingw32" = ".exe" | otherwise = ""

checked :: Show problem => Text.Text -> Either problem value -> IO value
checked context = either (\problem -> assertFailure (Text.unpack context <> ": " <> show problem) >> fail "unreachable") pure
