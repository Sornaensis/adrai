{-# LANGUAGE OverloadedStrings #-}

-- | Explicitly opt-in, executable-level regression gate for the large Python
-- prototype.  The fixture is generated here, not imported from Python: valid
-- ADRAI records are sealed with real parent OIDs and streamed through one
-- @git fast-import@ process, which makes the 12,000 commit shape practical in
-- CI without weakening the history being compiled.
module Adrai.LargeRepositoryStressTest
  ( tests,
    parsePhaseProfileArguments,
    setPhaseProfileOutput,
  )
where

import Adrai.Domain (Domain, DomainError, DomainRefinement, domainText, mkDomain, parseDomainRefinement)
import Adrai.Compiler.Attribution
  ( AttributionArtifact (..),
    AttributionPhase (..),
    AttributionRow (..),
    appendAttributionEvidence,
    parseAttributionArtifact,
  )
import Adrai.Fixture.LargeStress (largeStressV1)
import Adrai.Fixture.ProductionShape (foldRepositoryPlan, productionShapeV1, repositorySteps)
import qualified Adrai.Fixture.Types as Fixture
import Adrai.Format.Config (defaultConfigText)
import Adrai.Format.Document
import Adrai.Provenance
import Adrai.Scope (ScopePattern, mkScopePattern)
import Adrai.Types hiding (ExitSuccess)
import qualified Control.Concurrent.Async as Async
import Control.Exception (SomeException, bracket, evaluate, onException, try)
import Control.Concurrent (threadDelay)
import Control.Monad (foldM, forM_, join, unless)
import Data.Char (isDigit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.ByteString.Lazy as LazyByteString
import Data.IORef
import Data.List (intercalate, isInfixOf)
import qualified Data.Map.Strict as Map
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
import System.FilePath ((</>), takeDirectory)
import System.Info (os)
import System.IO (Handle, hClose, hFlush, hGetContents, hGetLine, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process
  ( CreateProcess (env, std_err, std_in, std_out), StdStream (CreatePipe), createProcess,
    getPid, getProcessExitCode, proc, readProcessWithExitCode, terminateProcess, waitForProcess, ProcessHandle )
import System.Timeout (timeout)
import Text.Read (readMaybe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "P6-06G large repository stress"
    ( [ testCase "12,000-commit repository with 2,000 ADR operations" largeRepositoryContract,
        testCase "bounded fast-import transition preserves imported managed trees" boundedPostImportTransitionContract,
        testCase "phase profile option and sidecar parsing are strict" phaseProfileParsingContract,
        testCase "phase profile sidecar is absent by default" phaseProfileAbsentByDefaultContract,
        testCase "pinned GHC RTS pair-list maximum residency parsing is strict" rtsPairListParsingContract,
        testCase "process-tree sampler distinguishes failures, zero working sets, and descendants" processTreeSamplerContract,
        testCase "process-tree complete-sample policy requires cold evidence but permits fast exits" processTreeSamplePolicyContract,
        testCase "taskkill failure accepts only a verified terminated process tree" taskkillFailureContract
      ]
        <> optInPhaseProfileTests
    )

-- | Selecting a sidecar must not cause the normal stress suite to grow a
-- second cold compilation.  Main initializes this before Tasty forces the
-- test tree; without the explicit option the bounded profiling test is absent.
{-# NOINLINE optInPhaseProfileTests #-}
optInPhaseProfileTests :: [TestTree]
optInPhaseProfileTests = unsafePerformIO $ do
  output <- getPhaseProfileOutput
  pure (phaseProfileTestsFor output)

phaseProfileTestsFor :: Maybe FilePath -> [TestTree]
phaseProfileTestsFor output =
  [testCase "opt-in 2,000-commit phase profile" productionShapeProfileContract | maybe False (const True) output]

-- | The profiling path is deliberately an opt-in test-runner concern.  The
-- ordinary 12k gate keeps its existing invocation, public streams, limits, and
-- assertions; this path merely writes a caller-selected sidecar after a
-- bounded production-shaped cold compile has completed.
{-# NOINLINE phaseProfileOutputRef #-}
phaseProfileOutputRef :: IORef (Maybe FilePath)
phaseProfileOutputRef = unsafePerformIO (newIORef Nothing)

setPhaseProfileOutput :: Maybe FilePath -> IO ()
setPhaseProfileOutput = writeIORef phaseProfileOutputRef

getPhaseProfileOutput :: IO (Maybe FilePath)
getPhaseProfileOutput = readIORef phaseProfileOutputRef

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
    assertBool "the bounded profiling test must be absent without --phase-profile" (null (phaseProfileTestsFor Nothing))
    writePhaseProfile Nothing (PhaseProfile "production-shape-v1" ["-N1", "-M2G"] [PhaseSample "materialize" 0 0 "ok" 0 0 0 []])
    exists <- doesFileExist output
    assertBool "profiling sidecar must not be created without --phase-profile" (not exists)

-- These are the configured P3-06 retrieval limits.  They deliberately stay
-- absolute even though this fixture has fewer than 2,000 live ADRs, so a
-- regression cannot hide behind a relational bound derived from its input.
stressMaxHybridSectionCandidates, stressMaxFieldRerankCandidates :: Int
stressMaxHybridSectionCandidates = 120
stressMaxFieldRerankCandidates = 80

stressMaxSourceChunks, stressMaxSelectedSourceChunks, stressMaxEligibleSearchItems :: Int
stressMaxSourceChunks = 24
stressMaxSelectedSourceChunks = 24
stressMaxEligibleSearchItems = 2000

stressMaxRelevanceShortlist, stressMaxRelevanceSections, stressMaxExactRerankCandidates :: Int
stressMaxRelevanceShortlist = 160
stressMaxRelevanceSections = 12000
stressMaxExactRerankCandidates = 960

-- Cold compilation must be viable within the production gate's resource
-- envelope. RTS residency is portable and enforced. Windows has a
-- process-tree sampler with a single-snapshot contract, so its OS working-set
-- bound is enforced there as an additional acceptance criterion; other
-- platforms retain the figure as a diagnostic until they provide equivalent
-- process-tree accounting.
coldCompileTimeoutMicros :: Int
coldCompileTimeoutMicros = 5 * 60 * 1000000

processCleanupTimeoutMicros :: Int
processCleanupTimeoutMicros = 10 * 1000000

stressHardHeapBytes, stressTargetResidencyBytes, stressProcessTreeTargetBytes :: Integer
stressHardHeapBytes = 2 * 1024 * 1024 * 1024
stressTargetResidencyBytes = (3 * stressHardHeapBytes) `div` 4
-- OS working-set accounting includes executable images and child processes.
-- Both supported samplers capture the root with all observed descendants, so
-- the 2.5 GiB process-tree allowance is an acceptance bound on Windows and
-- POSIX rather than a Windows-only diagnostic.
stressProcessTreeTargetBytes = (5 * 1024 * 1024 * 1024) `div` 2

-- ADR key 800 is the canonical large-plan compiler template whose rendered
-- identity is ADR 801.  Keep every probe string derived from that owned
-- template, rather than duplicating an obsolete synthetic stress label.
representativeTemplate :: Fixture.AdrTemplate
representativeTemplate =
  case
      [ template
      | planned <- repositorySteps largeStressV1
      , Fixture.PlannedSemantic _ (Fixture.PlannedCreate template) <- [Fixture.plannedCommitKind planned]
      , Fixture.adrTemplateKey template == Fixture.AdrKey 800
      ] of
    [template] -> template
    templates -> error ("largeStressV1 must contain exactly one representative template, found " <> show (length templates))

representativeAdr :: String
representativeAdr =
  case Fixture.adrTemplateKey representativeTemplate of
    Fixture.AdrKey key -> Text.unpack (identifier 'A' (key + 1))

representativeSearchQuery :: String
representativeSearchQuery = Text.unpack (Fixture.adrTemplateTitle representativeTemplate)

representativeRelevanceText :: Text.Text
representativeRelevanceText =
  "Canonical large-plan marker: "
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
largeRepositoryContract = withSystemTempDirectory "adrai-large-repository" $ \root -> do
  let repository = root </> "repository"
  timed "materialize" (materializePlan repository largeStressV1 (representativeRelevanceText <> "\n"))

  coldResult <- timed "cold-compile" (runAdraiWithRtsStats repository ["compile", "--json"])
  let cold = adraiProcessStdout coldResult
      coldRtsStats = adraiProcessStderr coldResult
  maximumResidency <-
    case parseMaximumResidency coldRtsStats of
      Nothing -> assertFailure ("cold compile RTS statistics omitted maximum residency: " <> coldRtsStats) >> fail "unreachable"
      Just value -> pure value
  BS8.hPutStrLn stderr (BS8.pack ("P6-06G stress resource: cold_compile_max_residency_bytes=" <> show maximumResidency <> " process_tree_peak_bytes=" <> show (adraiProcessTreePeakBytes coldResult) <> " process_tree_successful_samples=" <> show (adraiProcessTreeSuccessfulSamples coldResult) <> " process_tree_target_bytes=" <> show stressProcessTreeTargetBytes <> " hard_heap_bytes=" <> show stressHardHeapBytes))
  assertBool
    ("cold compile maximum residency must be <= " <> show stressTargetResidencyBytes <> " bytes, got " <> show maximumResidency)
    (maximumResidency <= stressTargetResidencyBytes)
  assertBool
    ("cold compile process-tree peak must be <= " <> show stressProcessTreeTargetBytes <> " bytes, got " <> show (adraiProcessTreePeakBytes coldResult))
    (withinProcessTreeTarget (adraiProcessTreePeakBytes coldResult))
  assertContains "cold compile" "\"cache_mode\":\"full\"" cold
  assertContains "cold compile" "\"incremental_kind\":\"full\"" cold
  assertContains "cold compile" "\"errors\":0" cold
  coldHistory <- jsonIntegerAt ["history_commits_scanned"] cold
  assertEqual coldHistory 12000 "cold compile must scan exactly the 12,000 reachable commits"
  coldDocuments <- jsonIntegerAt ["documents_parsed"] cold
  assertEqual coldDocuments 5350 "cold compile must parse exactly the 5,350 managed documents"
  coldAdrs <- jsonIntegerAt ["adrs_rebuilt"] cold
  assertEqual coldAdrs 900 "cold compile must rebuild exactly the 900 ADRs"
  -- The exact-cache path is deliberately sampled twice: it must remain exact
  -- after the first immutable archive has been read and after the mutable alias
  -- has been refreshed.
  exactOne <- timed "exact-compile-1" (runAdrai repository ["compile", "--json"])
  exactTwo <- timed "exact-compile-2" (runAdrai repository ["compile", "--json"])
  mapM_ (assertContains "exact compile" "\"cache_mode\":\"exact\"") [exactOne, exactTwo]

  let representative = representativeAdr
  search <- timed "hybrid-search" (runAdrai repository ["search", representativeSearchQuery, "--mode", "hybrid", "--limit", "10", "--json"])
  assertContains "hybrid search" "\"mode\":\"hybrid\"" search
  assertNonEmptyResults "hybrid search" search
  assertContains "hybrid search" representative search
  sectionCandidates <- firstResultIntegerAt ["retrieval", "vector", "section_candidates"] search
  fieldCandidates <- firstResultIntegerAt ["retrieval", "field_rerank_candidates"] search
  assertBounded "hybrid section candidates" stressMaxHybridSectionCandidates sectionCandidates
  assertBounded "hybrid field rerank candidates" stressMaxFieldRerankCandidates fieldCandidates
  relevant <- timed "representative-relevance" (runAdrai repository ["relevant", "analysis/relevance.txt", "--limit", "10", "--json"])
  assertNonEmptyResults "representative relevance" relevant
  assertContains "representative relevance" representative relevant
  relevantBounds <- relevanceBounds relevant
  let (sourceChunks, selectedChunks, eligibleItems, adrShortlist, candidateItems, searchSections, exactRerank) = relevantBounds
  assertBounded "relevance source chunks" stressMaxSourceChunks sourceChunks
  assertBounded "relevance selected source chunks" stressMaxSelectedSourceChunks selectedChunks
  assertBounded "relevance eligible search items" stressMaxEligibleSearchItems eligibleItems
  assertBounded "relevance ADR shortlist" stressMaxRelevanceShortlist adrShortlist
  assertBounded "relevance shortlist candidates" stressMaxRelevanceShortlist candidateItems
  assertBounded "relevance search sections" stressMaxRelevanceSections searchSections
  assertBounded "relevance exact rerank candidates" stressMaxExactRerankCandidates exactRerank
  assertBool "relevance must strictly exclude all-pairs reranking" (exactRerank < selectedChunks * searchSections)

  -- An ordinary final commit has an identical managed tree and must use the
  -- bounded tree-identical proof rather than reparse the corpus.
  addNoiseCommit repository "after-cache-proof"
  treeIdentical <- timed "tree-identical-compile" (runAdrai repository ["compile", "--json"])
  assertContains "tree-identical compile" "\"incremental_kind\":\"tree-identical\"" treeIdentical
  assertContains "tree-identical compile" "\"documents_parsed\":0" treeIdentical
  treeHistory <- jsonIntegerAt ["history_commits_scanned"] treeIdentical
  assertEqual treeHistory 1 "tree-identical history proof must scan exactly the new noise commit"

  -- The feature adds a real managed ADR, so revision switching proves both
  -- branch-only semantic visibility and revision-addressed cache selection.
  git repository ["switch", "-c", "stress/feature-adrs"]
  featureAdr <- createFeatureAdr repository
  featureRevision <- gitStdout repository ["rev-parse", "HEAD"]
  feature <- timed "feature-compile" (runAdrai repository ["compile", "--json"])
  assertContains "feature compile" "\"errors\":0" feature
  assertContains "feature compile revision" (Text.unpack (Text.strip (Text.pack featureRevision))) feature
  featureSearch <- timed "feature-branch-search" (runAdrai repository ["search", "feature branch only decision", "--mode", "hybrid", "--limit", "10", "--json"])
  assertContains "feature branch ADR" featureAdr featureSearch
  git repository ["switch", "main"]
  mainRevision <- gitStdout repository ["rev-parse", "HEAD"]
  mainAgain <- timed "main-branch-compile" (runAdrai repository ["compile", "--json"])
  assertContains "main branch compile" "\"errors\":0" mainAgain
  assertContains "main branch revision" (Text.unpack (Text.strip (Text.pack mainRevision))) mainAgain
  assertContains "main branch exact cache" "\"cache_mode\":\"exact\"" mainAgain
  mainSearch <- timed "main-branch-search" (runAdrai repository ["search", "feature branch only decision", "--mode", "hybrid", "--limit", "10", "--json"])
  assertAbsent "main branch must not expose feature ADR" featureAdr mainSearch
  -- Timings are reporting-only until an isolated completed baseline exists.
  pure ()

-- | This bounded path exists solely to attribute cold-compile work before a
-- full 12k gate is run.  It is a no-op unless Main consumed an explicit
-- @--phase-profile FILE@ option, and it keeps the production executable's
-- normal output in its captured pipes.
productionShapeProfileContract :: IO ()
productionShapeProfileContract = do
  output <- getPhaseProfileOutput
  forM_ output $ \path -> do
    withSystemTempDirectory "adrai-production-shape-profile" $ \root -> do
      let repository = root </> "repository"
          trace2Path = root </> "compiler-git-trace2.json"
      -- Fixture construction happens before the child starts.  Its Git and
      -- fast-import children therefore cannot enter either compiler-owned
      -- attribution artifact or the Trace2 total.
      materializePlan repository productionShapeV1 "Production-shaped phase profile fixture.\n"
      cold <-
        runAdraiWithRuntimeEnvironment
          RequireCompleteProcessTreeSample
          repository
          ["compile", "--json"]
          ["-t", "--machine-readable"]
          (Just coldCompileTimeoutMicros)
          [ ("ADRAI_TEST_COLD_COMPILE_ATTRIBUTION", path),
            ("GIT_TRACE2_EVENT", trace2Path)
          ]
      assertContains "production profile cold compile" "\"cache_mode\":\"full\"" (adraiProcessStdout cold)
      assertContains "production profile cold compile" "\"errors\":0" (adraiProcessStdout cold)
      history <- jsonIntegerAt ["history_commits_scanned"] (adraiProcessStdout cold)
      assertEqual history 2000 "production profile cold compile must scan exactly 2,000 reachable commits"
      maximumResidency <-
        case parseMaximumResidency (adraiProcessStderr cold) of
          Nothing -> assertFailure "production profile RTS output omitted maximum residency" >> fail "unreachable"
          Just value -> pure value
      assertBool "production profile RTS residency must stay within the cold gate" (maximumResidency <= stressTargetResidencyBytes)
      assertBool "production profile process tree needs a sampled peak" (adraiProcessTreePeakBytes cold >= 0)
      artifactText <- BS8.unpack <$> BS.readFile path
      case parseAttributionArtifact artifactText of
        Left problem -> assertFailure ("production profile attribution artifact: " <> problem)
        Right artifact -> assertCompleteAttribution artifact
      traceExists <- doesFileExist trace2Path
      assertBool "compiler Trace2 file is absent" traceExists
      trace2 <- BS8.unpack <$> BS.readFile trace2Path
      case trace2ProcessTotals trace2 of
        Left problem -> assertFailure ("compiler Trace2 file is malformed: " <> problem)
        Right (started, exited, traceElapsedMilliseconds) -> do
          assertBool "compiler Trace2 file contains no Git child operations" (started > 0)
          started @?= exited
          assertBool "compiler Trace2 elapsed total is negative" (traceElapsedMilliseconds >= 0)
          appendAttributionEvidence
            path
            [ ("rts_max_residency_bytes", maximumResidency),
              ("process_tree_peak_bytes", adraiProcessTreePeakBytes cold),
              ("trace2_git_requests", fromIntegral started),
              ("trace2_git_elapsed_ms", traceElapsedMilliseconds)
            ]
          persisted <- BS8.unpack <$> BS.readFile path
          case parseAttributionArtifact persisted of
            Right (AttributionArtifact rows) ->
              assertBool
                "caller-owned profile artifact omits terminal runtime evidence"
                (length [() | AttributionEvidence _ _ _ <- rows] == 4)
            Left problem -> assertFailure ("persisted production profile artifact: " <> problem)

assertCompleteAttribution :: AttributionArtifact -> IO ()
assertCompleteAttribution artifact = do
  let rows = attributionArtifactRows artifact
      completed = [phase | AttributionEnd _ phase _ _ _ True <- rows]
  assertBool "attribution artifact contains no completed phases" (not (null completed))
  assertBool "attribution artifact contains a failed phase" (all isSuccessfulEnd rows)
  assertBool "attribution artifact omits a cold-compile phase" (all (`elem` completed) requiredColdCompilePhases)
  where
    requiredColdCompilePhases =
      [ CliPreflight,
        CliRevisionResolution,
        PreflightCurrentObservation,
        CacheSelection,
        CurrentTreeBlobObservation,
        HistoryGraphEnumeration,
        HistoryPathSelectionDiff,
        HistoryReplayParse,
        BasisChecks,
        AnalysisGate,
        SearchMaterializationPhase,
        Fingerprinting,
        SqliteSchema,
        SqliteMetadataInitial,
        ManagedSourceRestream,
        SemanticInserts,
        SearchInserts,
        FtsInserts,
        SqliteMetadataFinal,
        Verification,
        DatabaseClose,
        ImmutablePublication,
        CurrentAliasCopy
      ]
    isSuccessfulEnd (AttributionEnd _ _ _ _ _ succeeded) = succeeded
    isSuccessfulEnd _ = True

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

materializePlan :: FilePath -> Fixture.RepositoryPlan -> Text.Text -> IO ()
materializePlan repository plan relevanceText = do
  initialise repository
  validatePlan plan
  withImporter repository $ \importer -> do
    states <- newIORef []
    forM_ (repositorySteps plan) (emitPlanned relevanceText importer states)
  synchronizeImportedWorktree repository
  count <- readIntGit repository ["rev-list", "--count", "HEAD"]
  assertEqual count (Fixture.repositoryCommitCount (Fixture.repositoryPlanSpec plan)) "fixture must have its planned commit count"

-- | Fast-import updates refs directly, so reset both ordinary Git views before
-- subsequent fixture commits use the index or worktree.
synchronizeImportedWorktree :: FilePath -> IO ()
synchronizeImportedWorktree repository = git repository ["reset", "--hard", "HEAD"]

-- | Keep the transition regression small while exercising the same fast-import
-- to ordinary-Git handoff used by the large fixture.
boundedPostImportTransitionContract :: IO ()
boundedPostImportTransitionContract = withSystemTempDirectory "adrai-post-import-transition" $ \root -> do
  let repository = root </> "repository"
      managedRoots = ["architecture/adrai/decisions", "architecture/adrai/connections"]
  initialise repository
  withImporter repository $ \importer -> do
    fastCommit importer [(".adrai.toml", TextEncoding.encodeUtf8 defaultConfigText)] "seed configuration"
    states <- newIORef []
    createState importer states 0 representativeTemplate
  synchronizeImportedWorktree repository
  importedRevision <- gitRevision repository "HEAD"
  importedConfig <- gitStdout repository ["show", "HEAD:.adrai.toml"]
  assertEqual importedConfig (Text.unpack defaultConfigText) "synchronized worktree retains imported configuration"
  managedEntries <- traverse (\rootPath -> lines <$> gitStdout repository ["ls-tree", "-r", "--name-only", "HEAD", "--", rootPath]) managedRoots
  assertBool "synchronized worktree retains entries under every configured managed root" (all (not . null) managedEntries)

  cold <- runAdrai repository ["compile", "--json"]
  assertContains "bounded cold compile" "\"errors\":0" cold
  exact <- runAdrai repository ["compile", "--json"]
  assertContains "bounded exact compile" "\"errors\":0" exact
  assertContains "bounded exact compile" "\"cache_mode\":\"exact\"" exact

  addNoiseCommit repository "bounded-transition"
  noiseRevision <- gitRevision repository "HEAD"
  noiseParent <- gitRevision repository "HEAD^"
  assertEqual noiseParent importedRevision "noise commit must parent the imported head"
  git repository (["diff", "--quiet", noiseParent, noiseRevision, "--", ".adrai.toml"] <> managedRoots)
  noiseDiff <- gitStdout repository ["diff", "--name-status", noiseParent, noiseRevision]
  assertEqual (Text.strip (Text.pack noiseDiff)) ("A\tsrc/post/bounded-transition.txt" :: Text.Text) "noise commit must add exactly its unmanaged path"
  treeIdentical <- runAdrai repository ["compile", "--json"]
  assertContains "bounded tree-identical compile" "\"errors\":0" treeIdentical
  assertContains "bounded tree-identical compile" "\"incremental_kind\":\"tree-identical\"" treeIdentical
  assertContains "bounded tree-identical compile" "\"documents_parsed\":0" treeIdentical
  treeHistory <- jsonIntegerAt ["history_commits_scanned"] treeIdentical
  assertEqual treeHistory 1 "bounded tree-identical history proof must scan exactly the noise commit"

  git repository ["switch", "-c", "bounded/post-import-transition"]
  _ <- createFeatureAdr repository
  featureRevision <- gitRevision repository "HEAD"
  forM_ (concat managedEntries) $ \path -> do
    importedEntry <- gitStdout repository ["show", importedRevision <> ":" <> path]
    featureEntry <- gitStdout repository ["show", featureRevision <> ":" <> path]
    assertEqual featureEntry importedEntry ("ordinary feature setup must preserve imported managed entry " <> path)

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

-- | Interpret the architecture-owned large plan directly.  The strict fold
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
    Fixture.PlannedMerge branch -> assertFailure ("largeStressV1 unexpectedly contains a merge step for " <> show branch)

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

createFeatureAdr :: FilePath -> IO String
createFeatureAdr repository = do
  basis <- repositoryHead repository
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
  files <- traverse (sealMember actor basis operation) [(decision, "decision.create", []), (scopeRecord, "scope.initial", [ProvenanceRecord record]), (domainRecord, "domain.initial", [ProvenanceRecord record]), (statusRecord, "status.initial", [ProvenanceRecord record])]
  forM_ files $ \(path, bytes) -> do
    let destination = repository </> path
    createDirectoryIfMissing True (takeDirectory destination)
    BS.writeFile destination bytes
  git repository (["add", "--"] <> map fst files)
  git repository ["commit", "-m", "adrai: feature branch only decision"]
  pure (Text.unpack (adrIdText adr))

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
sealMember actor basis operation (record, eventText, parents) = do
  semantic <- checked "semantic" (renderManagedSemantic record)
  event <- checked "event" (mkEventKind eventText)
  anchor <- checked "line anchor" (mkLineAnchor "stress@logical\nrecord" basis)
  capsule <- checked "capsule" (mkProvenanceCapsule ProvenanceCapsuleInput
    { capsuleInputOperationId = operation, capsuleInputObjectId = managedObject record, capsuleInputEventKind = event
    , capsuleInputActor = actor, capsuleInputTimestampMs = 1700000000000, capsuleInputBasis = basis, capsuleInputParents = parents
    , capsuleInputBranchHint = Just "main", capsuleInputUpstreamHint = Nothing, capsuleInputLineAnchors = [anchor]
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
  blobMarks <- mapM writeBlob files
  commitMark <- freshMark importer
  parentMark <- readIORef (importerLastCommitMark importer)
  let input = importerInput importer
  writeProtocol input ("commit refs/heads/main\nmark :" <> show commitMark <> "\nauthor ADRAI Stress <stress@adrai.invalid> 1700000000 +0000\ncommitter ADRAI Stress <stress@adrai.invalid> 1700000000 +0000\n")
  writeData input (BS8.pack message)
  forM_ parentMark $ \parent -> writeProtocol input ("from :" <> show parent <> "\n")
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

repositoryHead :: FilePath -> IO GitOid
repositoryHead repository = do
  raw <- gitStdout repository ["rev-parse", "HEAD"]
  checked "repository HEAD" (mkGitOid (Text.strip (Text.pack raw)))

gitRevision :: FilePath -> String -> IO String
gitRevision repository revision = Text.unpack . Text.strip . Text.pack <$> gitStdout repository ["rev-parse", revision]

readIntGit :: FilePath -> [String] -> IO Int
readIntGit repository args = do
  value <- gitStdout repository args
  let bad = assertFailure ("expected integer from git: " <> value) >> fail "unreachable"
  case words value of
    [token] ->
      case reads token of
        [(number, "")] -> pure number
        _ -> bad
    _ -> bad

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

runAdrai :: FilePath -> [String] -> IO String
runAdrai repository arguments = adraiProcessStdout <$> runAdraiWithRuntime PermitNoCompleteProcessTreeSample repository arguments [] Nothing

runAdraiWithRtsStats :: FilePath -> [String] -> IO AdraiProcessResult
runAdraiWithRtsStats repository arguments = runAdraiWithRuntime RequireCompleteProcessTreeSample repository arguments ["-t", "--machine-readable"] (Just coldCompileTimeoutMicros)

data AdraiProcessResult = AdraiProcessResult
  { adraiProcessStdout :: String,
    adraiProcessStderr :: String,
    adraiProcessTreePeakBytes :: Integer,
    adraiProcessTreeSuccessfulSamples :: Int
  }

-- | Run the package-built executable while retaining structured RTS output and
-- an OS process-tree working-set high-water mark. Both supported samplers
-- include the root and all observed descendants, and are acceptance evidence.
runAdraiWithRuntime :: CompleteProcessTreeSamplePolicy -> FilePath -> [String] -> [String] -> Maybe Int -> IO AdraiProcessResult
runAdraiWithRuntime samplePolicy repository arguments rtsStatistics timeoutMicros =
  runAdraiWithRuntimeEnvironment samplePolicy repository arguments rtsStatistics timeoutMicros []

runAdraiWithRuntimeEnvironment :: CompleteProcessTreeSamplePolicy -> FilePath -> [String] -> [String] -> Maybe Int -> [(String, String)] -> IO AdraiProcessResult
runAdraiWithRuntimeEnvironment samplePolicy repository arguments rtsStatistics timeoutMicros suppliedEnvironment = do
  binDirectory <- getBinDir
  let executable = binDirectory </> ("adrai" <> executableSuffix)
  exists <- doesFileExist executable
  assertBool ("package-built executable is absent: " <> executable) exists
  inheritedEnvironment <- getEnvironment
  let childEnvironment = suppliedEnvironment <> filter (\(key, _) -> key `notElem` map fst suppliedEnvironment) inheritedEnvironment
  (Just input, Just outputHandle, Just problemHandle, processHandle) <-
    createProcess
      (proc executable (["--repo", repository] <> arguments <> ["+RTS", "-N1", "-M2G"] <> rtsStatistics <> ["-RTS"]))
        { std_in = CreatePipe,
           std_out = CreatePipe,
           std_err = CreatePipe,
           env = Just childEnvironment
        }
  hClose input
  outputWorker <- Async.async (readFully outputHandle)
  problemWorker <- Async.async (readFully problemHandle)
  peakBytesRef <- newIORef 0
  successfulSamplesRef <- newIORef 0
  let waitForExit = pollProcessTreePeak processHandle peakBytesRef successfulSamplesRef
      cleanup = stopAndDrain processHandle [outputWorker, problemWorker]
      awaitWorkers = do
        drained <- timeout processCleanupTimeoutMicros (traverse Async.wait [outputWorker, problemWorker])
        case drained of
          Just [forcedOutput, forcedProblem] -> pure (forcedOutput, forcedProblem)
          Just _ -> assertFailure "unexpected stdout/stderr worker count" >> fail "unreachable"
          Nothing -> do
            mapM_ Async.cancel [outputWorker, problemWorker]
            assertFailure "adrai stdout/stderr did not drain within the bounded cleanup interval" >> fail "unreachable"
  completed <-
    ( case timeoutMicros of
        Nothing -> Just <$> waitForExit
        Just duration -> timeout duration waitForExit
    ) `onException` cleanup
  status <-
    case completed of
      Just (Right exitCode) -> pure exitCode
      Just (Left samplingFailure) -> do
        cleanup
        assertFailure ("adrai process-tree sampler failed: " <> samplingFailure) >> fail "unreachable"
      Nothing -> do
        cleanup
        peakBytes <- readIORef peakBytesRef
        BS8.hPutStrLn stderr (BS8.pack ("P6-06G stress resource: cold_compile_timeout_process_tree_peak_bytes=" <> show peakBytes <> " process_tree_target_bytes=" <> show stressProcessTreeTargetBytes <> " hard_heap_bytes=" <> show stressHardHeapBytes))
        assertFailure ("cold compile exceeded the 5-minute safety timeout under +RTS -N1 -M2G; diagnostic process-tree peak bytes=" <> show peakBytes) >> fail "unreachable"
  (forcedOutput, forcedProblem) <- awaitWorkers `onException` cleanup
  successfulSamples <- readIORef successfulSamplesRef
  assertBool
    "cold compile requires at least one successful complete process-tree sample"
    (completeProcessTreeSamplesAccepted samplePolicy successfulSamples)
  peakBytes <- readIORef peakBytesRef
  case status of
    ExitSuccess -> pure (AdraiProcessResult forcedOutput forcedProblem peakBytes successfulSamples)
    ExitFailure code -> assertFailure ("adrai " <> unwords arguments <> " failed (" <> show code <> "): " <> forcedProblem) >> fail "unreachable"

readFully :: Handle -> IO String
readFully handle = do
  content <- hGetContents handle
  _ <- evaluate (length content)
  pure content

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

pollProcessTreePeak :: ProcessHandle -> IORef Integer -> IORef Int -> IO (Either String ExitCode)
pollProcessTreePeak processHandle peakBytesRef successfulSamplesRef = do
  -- Take the sample before checking exit so a quick successful process cannot
  -- be mistaken for a measured one with a zero-byte peak.
  sample <- processTreeWorkingSet processHandle
  recorded <- recordProcessTreeSample peakBytesRef successfulSamplesRef sample
  case recorded of
    Left problem -> do
      exited <- getProcessExitCode processHandle
      pure $ maybe (Left problem) Right exited
    Right () -> do
      status <- getProcessExitCode processHandle
      case status of
        Just exitCode -> pure (Right exitCode)
        Nothing -> threadDelay 1000000 >> pollProcessTreePeak processHandle peakBytesRef successfulSamplesRef

recordProcessTreeSample :: IORef Integer -> IORef Int -> Either String Integer -> IO (Either String ())
recordProcessTreeSample _ _ (Left problem) = pure (Left problem)
recordProcessTreeSample peakBytesRef successfulSamplesRef (Right bytes) = do
  modifyIORef' peakBytesRef (max bytes)
  modifyIORef' successfulSamplesRef (+ 1)
  pure (Right ())

completeProcessTreeSamplesAccepted :: CompleteProcessTreeSamplePolicy -> Int -> Bool
completeProcessTreeSamplesAccepted RequireCompleteProcessTreeSample successfulSamples = successfulSamples > 0
completeProcessTreeSamplesAccepted PermitNoCompleteProcessTreeSample _ = True

processTreeWorkingSet :: ProcessHandle -> IO (Either String Integer)
processTreeWorkingSet processHandle = do
  processId <- getPid processHandle
  case processId of
    Nothing -> pure (Left "process ID is unavailable")
    Just value
      | os == "mingw32" -> windowsProcessTreeWorkingSet (show value)
       | otherwise -> posixProcessTreeWorkingSet (show value)

withinProcessTreeTarget :: Integer -> Bool
withinProcessTreeTarget peakBytes = peakBytes <= stressProcessTreeTargetBytes

windowsProcessTreeWorkingSet :: String -> IO (Either String Integer)
windowsProcessTreeWorkingSet rootPid = do
  -- PowerShell treats tokens after @-Command <script>@ as part of that
  -- script, not as @\$args@. Embed the validated numeric pid so sampling
  -- does not silently fail with a parser error and report a useless zero.
  -- One CIM snapshot supplies both topology and working-set values. Unlike a
  -- later Get-Process lookup per PID, a short-lived non-root child cannot
  -- invalidate the whole sample after the tree has been discovered. The root
  -- must still be present in the snapshot.
  let root = maybe (-1) id (readNonnegativeInteger rootPid)
      script =
        "$root="
          <> show root
          <> ";$ErrorActionPreference='Stop';if($root -lt 0 -or $root -gt [uint32]::MaxValue){exit 43};$rootPid=[uint32]$root;$all=Get-CimInstance Win32_Process | Select-Object ProcessId,ParentProcessId,WorkingSetSize;if(-not ($all.ProcessId -contains $rootPid)){exit 42};$ids=New-Object 'System.Collections.Generic.HashSet[uint32]';[void]$ids.Add($rootPid);do{$added=$false;foreach($p in $all){if($ids.Contains([uint32]$p.ParentProcessId) -and $ids.Add([uint32]$p.ProcessId)){$added=$true}}}while($added);$total=[int64]0;foreach($p in $all){if($ids.Contains([uint32]$p.ProcessId)){$total += [int64]$p.WorkingSetSize}};[Console]::Out.WriteLine($total)"
  (status, output, _) <- readProcessWithExitCode "powershell" ["-NoProfile", "-NonInteractive", "-Command", script] ""
  case status of
    ExitSuccess -> pure (maybe (Left "PowerShell returned a malformed working-set value") Right (readNonnegativeInteger output))
    ExitFailure 42 -> pure (Left "root process disappeared while sampling")
    ExitFailure code -> pure (Left ("PowerShell sampler exited " <> show code))

posixProcessTreeWorkingSet :: String -> IO (Either String Integer)
posixProcessTreeWorkingSet rootPid = do
  (status, output, _) <- readProcessWithExitCode "ps" ["-eo", "pid=,ppid=,rss="] ""
  case status of
    ExitSuccess -> pure (processTreeWorkingSetFromPs (readNonnegativeInteger rootPid) output)
    ExitFailure code -> pure (Left ("ps sampler exited " <> show code))

processTreeWorkingSetFromPs :: Maybe Integer -> String -> Either String Integer
processTreeWorkingSetFromPs Nothing _ = Left "process ID is malformed"
processTreeWorkingSetFromPs (Just root) output = do
  processes <- traverse parseProcess (map words (lines output))
  let tree = processTree root processes
  if any (\(pid, _, _) -> pid == root) tree
    then Right (1024 * sum [rss | (_, _, rss) <- tree])
    else Left "root process disappeared while sampling"
  where
    parseProcess [pidText, parentText, rssText] =
      case (readNonnegativeInteger pidText, readNonnegativeInteger parentText, readNonnegativeInteger rssText) of
        (Just pid, Just parent, Just rss) -> Right (pid, parent, rss)
        _ -> Left "ps returned a malformed process row"
    parseProcess _ = Left "ps returned an incomplete process row"

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

-- | A timeout must tear down the executable and any helpers it spawned before
-- waiting on inherited stdout/stderr handles.  Otherwise a surviving child
-- can hold either pipe open indefinitely and invalidate the resource gate.
-- Every termination path is checked and the root exit plus pipe drains have a
-- fixed deadline, so cleanup cannot quietly hang the stress worker.
stopAndDrain :: ProcessHandle -> [Async.Async String] -> IO ()
stopAndDrain processHandle workers = do
  stopped <- terminateProcessTree processHandle
  case stopped of
    Left problem -> assertFailure problem >> fail "unreachable"
    Right () -> pure ()
  exited <- timeout processCleanupTimeoutMicros (waitForProcess processHandle)
  case exited of
    Nothing -> assertFailure "adrai process did not terminate within the bounded cleanup interval" >> fail "unreachable"
    Just _ -> pure ()
  drained <- timeout processCleanupTimeoutMicros (traverse Async.waitCatch workers)
  case drained of
    Nothing -> do
      mapM_ Async.cancel workers
      assertFailure "adrai stdout/stderr did not drain during bounded cleanup" >> fail "unreachable"
    Just _ -> pure ()

terminateProcessTree :: ProcessHandle -> IO (Either String ())
terminateProcessTree processHandle = do
  alreadyExited <- getProcessExitCode processHandle
  case alreadyExited of
    Just _ -> pure (Right ())
    Nothing -> terminateLiveProcessTree processHandle

terminateLiveProcessTree :: ProcessHandle -> IO (Either String ())
terminateLiveProcessTree processHandle = do
  processId <- getPid processHandle
  case processId of
    Nothing -> terminateProcess processHandle >> pure (Right ())
    Just spawnedProcessId ->
      case readNonnegativeInteger (show spawnedProcessId) of
        Nothing -> terminateProcess processHandle >> pure (Right ())
        Just root
          | os == "mingw32" -> do
              -- Capture the tree before taskkill opens the root.  If the root
              -- exits in that interval, Windows can report taskkill failure
              -- even though a child it spawned remains alive and is now
              -- reparented.  The captured PIDs are therefore part of the
              -- cleanup obligation on the failed-taskkill race path.
              knownDescendants <- windowsDescendants root
              (status, _, problem) <- readProcessWithExitCode "taskkill" ["/PID", show root, "/T", "/F"] ""
              case status of
                ExitSuccess -> pure (Right ())
                ExitFailure code -> do
                  -- A process can exit after the optimistic poll above but before
                  -- taskkill opens it.  Windows reports that race as, among other
                  -- things, access denied.  Wait for the root's exit before
                  -- accepting the failed taskkill; a still-live root or a
                  -- captured descendant remains a hard cleanup failure.
                  exited <- awaitRootExitAfterTaskkillFailure processHandle
                  descendants <- join <$> traverse awaitWindowsDescendantsExit knownDescendants
                  pure (taskkillFailureResult code problem exited descendants)
          | otherwise -> do
              descendants <- posixDescendants root
              terminateProcess processHandle
              failures <- fmap concat . traverse terminateDescendant $ descendants
              pure $ case failures of
                [] -> Right ()
                values -> Left ("failed to terminate descendant processes: " <> unwords values)
  where
    terminateDescendant pid = do
      (status, _, problem) <- readProcessWithExitCode "kill" ["-TERM", show pid] ""
      pure $ case status of
        ExitSuccess -> []
        ExitFailure code -> [show pid <> " (" <> show code <> "): " <> problem]

awaitRootExitAfterTaskkillFailure :: ProcessHandle -> IO (Maybe ExitCode)
awaitRootExitAfterTaskkillFailure processHandle = do
  exited <- getProcessExitCode processHandle
  case exited of
    Just exitCode -> pure (Just exitCode)
    Nothing -> timeout processCleanupTimeoutMicros (waitForProcess processHandle)

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

-- | Return the complete descendant PID set from one CIM snapshot.  This uses
-- the same single-snapshot topology rule as working-set accounting: a child
-- that exits between topology discovery and a per-PID lookup must not cause a
-- valid snapshot to be rejected, but a missing root means there is no tree we
-- can safely verify after a failed taskkill.
windowsDescendants :: Integer -> IO (Either String [Integer])
windowsDescendants root = do
  let script =
        "$root="
          <> show root
          <> ";$ErrorActionPreference='Stop';if($root -lt 0 -or $root -gt [uint32]::MaxValue){exit 43};$rootPid=[uint32]$root;$all=Get-CimInstance Win32_Process | Select-Object ProcessId,ParentProcessId;if(-not ($all.ProcessId -contains $rootPid)){exit 42};$ids=New-Object 'System.Collections.Generic.HashSet[uint32]';[void]$ids.Add($rootPid);do{$added=$false;foreach($p in $all){if($ids.Contains([uint32]$p.ParentProcessId) -and $ids.Add([uint32]$p.ProcessId)){$added=$true}}}while($added);foreach($id in $ids){if($id -ne $rootPid){[Console]::Out.WriteLine($id)}}"
  (status, output, problem) <- readProcessWithExitCode "powershell" ["-NoProfile", "-NonInteractive", "-Command", script] ""
  case status of
    ExitSuccess -> pure (parseWindowsPidLines output)
    ExitFailure 42 -> pure (Left "root process disappeared before taskkill topology snapshot")
    ExitFailure code -> pure (Left ("PowerShell descendant snapshot exited " <> show code <> ": " <> problem))

-- | Poll the captured descendants for no longer than the cleanup deadline.
-- A root's orderly exit can take its helpers a short time to unwind; after
-- that fixed interval any remaining PID is reported to the caller and causes
-- cleanup to fail closed.
awaitWindowsDescendantsExit :: [Integer] -> IO (Either String [Integer])
awaitWindowsDescendantsExit [] = pure (Right [])
awaitWindowsDescendantsExit descendants = do
  observed <- timeout processCleanupTimeoutMicros waitUntilGone
  pure $
    case observed of
      Nothing -> Left "timed out while verifying captured descendant exits"
      Just result -> result
  where
    waitUntilGone = do
      live <- windowsLiveProcessIds descendants
      case live of
        Left problem -> pure (Left problem)
        Right [] -> pure (Right [])
        Right _survivors -> threadDelay 100000 >> waitUntilGone

windowsLiveProcessIds :: [Integer] -> IO (Either String [Integer])
windowsLiveProcessIds descendants = do
  let targetLiterals = intercalate "," (map show descendants)
      script =
        "$ErrorActionPreference='Stop';$targets=New-Object 'System.Collections.Generic.HashSet[uint32]';"
          <> "foreach($id in @(" <> targetLiterals <> ")){if($id -lt 0 -or $id -gt [uint32]::MaxValue){exit 43};[void]$targets.Add([uint32]$id)};"
          <> "Get-CimInstance Win32_Process | ForEach-Object {if($targets.Contains([uint32]$_.ProcessId)){[Console]::Out.WriteLine($_.ProcessId)}}"
  (status, output, problem) <- readProcessWithExitCode "powershell" ["-NoProfile", "-NonInteractive", "-Command", script] ""
  case status of
    ExitSuccess -> pure (parseWindowsPidLines output)
    ExitFailure code -> pure (Left ("PowerShell descendant verification exited " <> show code <> ": " <> problem))

parseWindowsPidLines :: String -> Either String [Integer]
parseWindowsPidLines output = traverse parseLine (filter (not . null) (lines output))
  where
    parseLine line =
      case words line of
        [pidText] ->
          case readNonnegativeInteger pidText of
            Just pid -> Right pid
            Nothing -> Left "PowerShell returned a malformed descendant PID"
        _ -> Left "PowerShell returned an incomplete descendant PID row"

posixDescendants :: Integer -> IO [Integer]
posixDescendants root = do
  (status, output, _) <- readProcessWithExitCode "ps" ["-eo", "pid=,ppid="] ""
  case status of
    ExitSuccess ->
      case traverse parseRow (map words (lines output)) of
        Left _ -> pure []
        Right rows -> pure (reverse [pid | (pid, _, _) <- processTree root [(pid, parent, 0) | (pid, parent) <- rows], pid /= root])
    ExitFailure _ -> pure []
  where
    parseRow [pidText, parentText] =
      case (readNonnegativeInteger pidText, readNonnegativeInteger parentText) of
        (Just pid, Just parent) -> Right (pid, parent)
        _ -> Left ()
    parseRow _ = Left ()

processTreeSamplePolicyContract :: IO ()
processTreeSamplePolicyContract = do
  completeProcessTreeSamplesAccepted RequireCompleteProcessTreeSample 0 @?= False
  completeProcessTreeSamplesAccepted PermitNoCompleteProcessTreeSample 0 @?= True
  completeProcessTreeSamplesAccepted RequireCompleteProcessTreeSample 1 @?= True

processTreeSamplerContract :: IO ()
processTreeSamplerContract = do
  processTreeWorkingSetFromPs (Just 10) "10 1 0\n11 10 7\n12 11 3\n" @?= Right (10 * 1024)
  processTreeWorkingSetFromPs (Just 10) "11 10 7\n" @?= Left "root process disappeared while sampling"
  processTreeWorkingSetFromPs (Just 10) "10 1 invalid\n" @?= Left "ps returned a malformed process row"
  readNonnegativeInteger "0\n" @?= Just 0
  readNonnegativeInteger "not-a-number\n" @?= Nothing
  assertBool "process-tree threshold accepts its exact boundary" (withinProcessTreeTarget stressProcessTreeTargetBytes)
  assertBool "process-tree threshold rejects excess working set" (not (withinProcessTreeTarget (stressProcessTreeTargetBytes + 1)))
  peak <- newIORef 0
  samples <- newIORef 0
  successfulSample <- recordProcessTreeSample peak samples (Right 0)
  successfulSample @?= Right ()
  readIORef peak >>= (@?= 0)
  readIORef samples >>= (@?= 1)
  incompleteSample <- recordProcessTreeSample peak samples (Left "incomplete tree")
  incompleteSample @?= Left "incomplete tree"
  readIORef samples >>= (@?= 1)

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

assertAbsent :: String -> String -> String -> IO ()
assertAbsent label forbidden actual = assertBool (label <> ": " <> actual) (not (forbidden `isInfixOf` actual))

assertNonEmptyResults :: String -> String -> IO ()
assertNonEmptyResults label output = assertBool (label <> " returned no results: " <> output) (not ("\"results\":[]" `isInfixOf` compact output))

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

firstResultIntegerAt :: [Text.Text] -> String -> IO Int
firstResultIntegerAt path output =
  case Aeson.eitherDecode (LazyByteString.fromStrict (BS8.pack output)) of
    Right (Aeson.Object root) ->
      case AesonKeyMap.lookup "results" root of
        Just (Aeson.Array values)
          | first : _ <- Vector.toList values ->
              case valueAt path first of
                Just (Aeson.Number number)
                  | Just integer <- toBoundedInteger number -> pure integer
                _ -> missing
        _ -> missing
    _ -> missing
  where
    missing = assertFailure ("missing first-result integer JSON field at " <> show path) >> fail "unreachable"

relevanceBounds :: String -> IO (Int, Int, Int, Int, Int, Int, Int)
relevanceBounds output = do
  source <- jsonIntegerAt ["retrieval", "source_chunks"] output
  selected <- jsonIntegerAt ["retrieval", "selected_source_chunks"] output
  eligible <- jsonIntegerAt ["retrieval", "eligible_search_items"] output
  shortlist <- jsonIntegerAt ["retrieval", "adr_shortlist"] output
  candidates <- jsonIntegerAt ["retrieval", "candidate_search_items"] output
  sections <- jsonIntegerAt ["retrieval", "search_sections"] output
  rerank <- jsonIntegerAt ["retrieval", "exact_rerank_candidates"] output
  pure (source, selected, eligible, shortlist, candidates, sections, rerank)

valueAt :: [Text.Text] -> Aeson.Value -> Maybe Aeson.Value
valueAt [] value = Just value
valueAt (key : remaining) (Aeson.Object object) = AesonKeyMap.lookup (AesonKey.fromText key) object >>= valueAt remaining
valueAt _ _ = Nothing

compact :: String -> String
compact = filter (`notElem` [' ', '\t', '\r', '\n'])

assertEqual :: (Eq value, Show value) => value -> value -> String -> IO ()
assertEqual actual expected label = unless (actual == expected) (assertFailure (label <> ": expected " <> show expected <> ", got " <> show actual))

assertBounded :: String -> Int -> Int -> IO ()
assertBounded label upperBound actual =
  assertBool (label <> " must be in 1.." <> show upperBound <> ", got " <> show actual) (actual > 0 && actual <= upperBound)

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
