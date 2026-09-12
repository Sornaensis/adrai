{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Adrai.ColdCompilerTest (tests) where

import Adrai.Compiler
import Adrai.Compiler.Attribution
  ( AttributionPhase (CompileOutcomeEvaluation, PostCloseFingerprintValidation, PostCloseProvenanceRefresh),
    AttributionArtifact (..),
    AttributionRow (..),
    closeColdCompileAttribution,
    newFileColdCompileAttribution,
    parseAttributionArtifact,
  )
import Adrai.Compiler.Snapshot (AnalyzedRepositorySnapshot (..), analyzeRepositorySnapshot, analyzeRepositorySnapshotWithHistoryCounts)
import Adrai.Fixture.CompilerRepository
import Adrai.Git
import Adrai.GitTestSupport
import Adrai.Provenance (mkGitOid)
import Adrai.Repository
import Adrai.RetainedNative.RepositorySeed
  ( RepositorySeed,
    createRepositorySeedWith,
    removeRepositorySeed,
    withRepositorySeedCopy,
  )
import Adrai.Retrieval (SearchMaterialization (..), SearchPassage (..))
import Adrai.Service.PostCommitIndex
  ( PostCommitIndexResult,
    compilePostCommitIndexWithAttributionAndRefresh,
  )
import Adrai.Sqlite
import Control.Exception (AsyncException (ThreadKilled), onException, throwIO, try)
import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Int (Int64)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple (Connection, Only (..), Query, close, execute_, open, query_)
import System.Directory (copyFile, doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getExecutablePath)
import System.FilePath ((</>), normalise)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Control.Monad (forM_)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Lazy as LBS
import System.Process.Typed (proc, readProcess)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  withResource createCompilerRepositorySeeds removeCompilerRepositorySeeds $ \getCompilerRepositorySeeds ->
    let getHealthyRepositorySeed = compilerHealthySeed <$> getCompilerRepositorySeeds
        getBasisRepositorySeed = compilerBasisSeed <$> getCompilerRepositorySeeds
     in testGroup
          "Cold compiler"
          [ testCase "healthy exact-OID revision ignores later HEAD and worktree corruption and persists a reopenable database" $
        withHealthyRepository getHealthyRepositorySeed $ \repository discovered resolved ->
          withSystemTempDirectory "adrai cold sqlite" $ \temporary -> do
            -- Both a later committed config and dirty managed bytes are ambient to
            -- the already bound exact OID and must never enter the cold build.
            _ <- commitFile repository ".adrai.toml" "invalid = ["
            currentHead <- requireResolvedFrom discovered "HEAD"
            assertBool "later committed HEAD differs from the bound healthy OID" (resolvedCommitOid currentHead /= resolvedCommitOid resolved)
            BS.writeFile (repository </> decisionPath) "dirty invalid worktree bytes"
            let database = temporary </> "cold.sqlite"
            connection <- open database
            result <- requireCompiled connection resolved
            coldDatabaseSemanticState (coldCompilerDatabaseStats result) @?= "valid"
            coldDatabaseManagedSourceCount (coldCompilerDatabaseStats result) @?= 4
            coldDatabaseIssueCount (coldCompilerDatabaseStats result) @?= 0
            count connection "decision_record" >>= (@?= 1)
            count connection "connection_record" >>= (@?= 3)
            count connection "operation" >>= (@?= 1)
            count connection "reduced_adr" >>= (@?= 1)
            count connection "search_document" >>= (@?= 1)
            ftsTables <- query_ connection "SELECT count(*) FROM sqlite_master WHERE type='table' AND name LIKE 'fts_%' AND sql LIKE 'CREATE VIRTUAL TABLE%'" :: IO [Only Int64]
            ftsTables @?= [Only 6]
            matches <- query_ connection "SELECT item_id FROM fts_search_stemmed WHERE fts_search_stemmed MATCH 'cold'" :: IO [Only Text]
            assertBool "cold FTS result is present" (not (null matches))
            anchors <- query_ connection "SELECT line_anchors FROM operation" :: IO [Only Text]
            case anchors of
              [Only encoded] -> do
                assertBool "line anchors use a JSON array" (Text.isPrefixOf "[" encoded)
                assertBool "line anchor identifiers preserve @ and escaped newlines" ("logical@anchor\\nsecond-line" `Text.isInfixOf` encoded)
              unexpected -> assertFailure ("unexpected operation anchors: " <> show unexpected)
            scopePayloads <- query_ connection "SELECT payload FROM connection_record WHERE relation_kind='applies_to'" :: IO [Only Text]
            case scopePayloads of
              [Only encoded] -> do
                assertBool "connection payload uses a JSON object" (Text.isPrefixOf "{" encoded)
                assertBool "comma-bearing scope is one quoted JSON value" ("\"src/a,b/**\"" `Text.isInfixOf` encoded)
                assertBool "connection payload has no delimiter NULs" (not (Text.any (== '\NUL') encoded))
              unexpected -> assertFailure ("unexpected scope payload: " <> show unexpected)
            expectedMeta <- meta connection
            expectedLogicalRows <- logicalRows connection
            close connection
            reopened <- open database
            meta reopened >>= (@?= expectedMeta)
            logicalRows reopened >>= (@?= expectedLogicalRows)
            count reopened "decision_record" >>= (@?= 1)
            close reopened
            doesFileExist (database <> "-wal") >>= (@?= False)
            doesFileExist (database <> "-shm") >>= (@?= False)
            doesFileExist (repository </> ".adrai" </> "cold.sqlite") >>= (@?= False),
      testCase "timestamps beyond SQLite Int64 are preserved exactly" $
        withCompilerRepository getBasisRepositorySeed largeTimestampCompilerFiles $ \_ resolved -> do
          connection <- open ":memory:"
          result <- requireCompiled connection resolved
          coldDatabaseSemanticState (coldCompilerDatabaseStats result) @?= "valid"
          timestamps <- query_ connection "SELECT timestamp_ms FROM operation" :: IO [Only Text]
          timestamps @?= [Only "9223372036854775808"]
          storageTypes <- query_ connection "SELECT typeof(timestamp_ms) FROM operation" :: IO [Only Text]
          storageTypes @?= [Only "text"]
          close connection,
      testCase "valid decision multihead stores ADR_CONFLICT and ADR at record search rows" $
        withCompilerRepository getBasisRepositorySeed conflictedCompilerFiles $ \_ resolved -> do
          connection <- open ":memory:"
          result <- requireCompiled connection resolved
          coldDatabaseSemanticState (coldCompilerDatabaseStats result) @?= "conflict"
          coldDatabaseIssueCount (coldCompilerDatabaseStats result) @?= 1
          coldDatabaseConflictCount (coldCompilerDatabaseStats result) @?= 1
          count connection "issue" >>= (@?= 1)
          count connection "adr_conflict" >>= (@?= 1)
          issueRows <- query_ connection "SELECT ordinal,code,severity,origin,adr_id,object_id,operation_id,commit_oid,path,message FROM issue ORDER BY ordinal" :: IO [(Int64, Text, Text, Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Text)]
          conflictRows <- query_ connection "SELECT adr_id,code,candidate_count,state_token,summaries FROM adr_conflict ORDER BY adr_id" :: IO [(Text, Text, Int64, Text, Text)]
          case (issueRows, conflictRows) of
            ([(0, "ADR_CONFLICT", "error", "graph", Just issueAdr, Nothing, Nothing, Nothing, Nothing, issueMessage)], [(conflictAdr, "ADR_CONFLICT", candidateCount, stateToken, summaries)]) -> do
              issueAdr @?= conflictAdr
              issueMessage @?= Text.intercalate "; " (Text.splitOn "\n" summaries)
              assertBool "semantic conflict has at least one candidate" (candidateCount >= 1)
              assertBool "semantic conflict retains its state token" (not (Text.null stateToken))
            other -> assertFailure ("unexpected linked conflict rows: " <> show other)
          publishedMeta <- meta connection
          lookup "issue_count" publishedMeta @?= Just "1"
          lookup "conflict_count" publishedMeta @?= Just "1"
          count connection "search_document" >>= (@?= 2)
          itemIds <- query_ connection "SELECT item_id FROM search_document ORDER BY item_id" :: IO [Only Text]
          assertBool "both search candidates use ADR@record identities" (all (Text.isInfixOf "@" . fromOnly) itemIds)
          close connection,
      testCase "compiler diagnostics precede semantic conflict issues deterministically" $
        withBasisRepository getBasisRepositorySeed "adrai conflict issue ordering" $ \repository discovered _ -> do
          let unavailableBasis = requireOid (Text.replicate 40 "f")
          files <- requireFixture (conflictedCompilerFiles unavailableBasis)
          _ <- commitFiles repository files
          resolved <- requireResolvedFrom discovered "HEAD"
          connection <- open ":memory:"
          result <- requireCompiled connection resolved
          coldDatabaseSemanticState (coldCompilerDatabaseStats result) @?= "conflict"
          coldDatabaseIssueCount (coldCompilerDatabaseStats result) @?= 2
          issueOrder <- query_ connection "SELECT ordinal,code,severity,origin FROM issue ORDER BY ordinal" :: IO [(Int64, Text, Text, Text)]
          issueOrder @?= [(0, "BASIS_COMMIT_UNAVAILABLE", "warning", "basis"), (1, "ADR_CONFLICT", "error", "graph")]
          close connection,
      testCase "invalid committed config creates a diagnostic-only database" $
        withBasisRepository getBasisRepositorySeed "adrai invalid compiler config" $ \repository discovered _ -> do
          _ <- commitFile repository ".adrai.toml" "not valid toml = ["
          resolved <- requireResolvedFrom discovered "HEAD"
          connection <- open ":memory:"
          result <- requireCompiled connection resolved
          coldDatabaseSemanticState (coldCompilerDatabaseStats result) @?= "invalid"
          coldCompilerSearchMaterialization result @?= Nothing
          count connection "issue" >>= (@?= 1)
          count connection "managed_source" >>= (@?= 0)
          count connection "search_document" >>= (@?= 0)
          close connection,
      testCase "late history batch corruption returns the original Git error without a later request" nativeLateHistoryBatchFailure,
      testCase "managed-source restream uses one production session and preserves every SQLite row" $
        withHealthyRepository getHealthyRepositorySeed $ \repository discovered _ ->
          withSystemTempDirectory "adrai managed-source blob-session" $ \temporary -> do
            decisionBytes <- BS.readFile (repository </> decisionPath)
            let newSources =
                  [ ( "architecture/adrai/decisions/Rrestream/source-" <> show index <> ".decision.md",
                      decisionBytes <> BS8.pack ("\n<!-- session-restream-" <> show index <> " -->\n")
                    )
                    | index <- [1 :: Int .. 257]
                  ]
            _ <- commitFiles repository newSources
            normalResolved <- requireResolvedFrom discovered "HEAD"
            normalConnection <- open ":memory:"
            normalResult <- requireCompiled normalConnection normalResolved
            expectedRows <- managedSourceRows normalConnection
            close normalConnection
            let traceFile = temporary </> "restream-git-argv.txt"
                wrapper = temporary </> "restream-tracing-git.cmd"
            writeTracingGitWrapper wrapper traceFile
            wrapped <- requireResolvedWithGitClient repository (GitClient wrapper) "HEAD"
            raw <-
              observeRawRepositorySnapshotAt wrapped
                >>= \case
                  Left problem -> assertFailure (show problem)
                  Right value -> pure value
            analyzed <-
              analyzeRepositorySnapshot raw
                >>= \case
                  Left problem -> assertFailure (show problem)
                  Right value -> pure value
            -- The only Git activity below is Sqlite.insertManagedSources,
            -- reached through the normal writeColdDatabase transaction.
            BS.writeFile traceFile BS.empty
            targetConnection <- open ":memory:"
            writeColdDatabase targetConnection analyzed Nothing (coldCompilerMaterializationFingerprint normalResult)
              >>= \case
                Left problem -> assertFailure (show problem)
                Right _ -> pure ()
            actualRows <- managedSourceRows targetConnection
            close targetConnection
            let selectedRows =
                  Map.fromList
                    [ (path, bytes)
                      | (path, _, _, _, bytes, _) <- actualRows,
                        "architecture/adrai/decisions/Rrestream/" `Text.isPrefixOf` path
                    ]
                expectedSelectedRows = Map.fromList [(Text.pack path, Just bytes) | (path, bytes) <- newSources]
            actualRows @?= expectedRows
            length actualRows @?= 261
            Map.size selectedRows @?= 257
            selectedRows @?= expectedSelectedRows
            assertSingleUnbufferedBlobSession traceFile,
      testCase "nonfresh connection is rejected without modifying existing schema" $
        withHealthyRepository getHealthyRepositorySeed $ \_ _ resolved -> do
          connection <- open ":memory:"
          execute_ connection "CREATE TABLE caller_owned(value TEXT)"
          coldCompileRepository connection resolved >>= \case
            Left (ColdCompilerDatabaseError (ColdDatabaseNotFresh objects)) ->
              assertBool "caller table is reported" (("table", "caller_owned") `elem` objects)
            result -> assertFailure ("expected nonfresh rejection, got " <> show result)
          count connection "caller_owned" >>= (@?= 0)
          close connection,
      testCase "foreign-key failure rolls back schema and all rows" $
        withHealthyRepository getHealthyRepositorySeed $ \_ _ resolved -> do
          sourceConnection <- open ":memory:"
          compiled <- requireCompiled sourceConnection resolved
          close sourceConnection
          analyzed <- requireAnalyzed resolved
          materialization <-
            maybe (assertFailure "healthy compile omitted search materialization") pure (coldCompilerSearchMaterialization compiled)
          broken <- breakFirstPassage materialization
          targetConnection <- open ":memory:"
          writeColdDatabase
            targetConnection
            analyzed
            (Just broken)
            (coldCompilerMaterializationFingerprint compiled)
            >>= \case
              Left (ColdDatabaseStorageFailure _) -> pure ()
              result -> assertFailure ("expected transactional FK failure, got " <> show result)
          schema <- query_ targetConnection "SELECT name FROM sqlite_master WHERE name NOT LIKE 'sqlite_%'" :: IO [Only Text]
          schema @?= []
          close targetConnection,
      testCase "post-close refresh cancellation cleans its private candidate" $
        withHealthyRepository getHealthyRepositorySeed $ \repository _ resolved -> do
          let database = repository </> "post-close.sqlite"
              artifact = repository </> "post-close.tsv"
          observer <- newFileColdCompileAttribution artifact
          cancelled <-
            try
              ( compilePostCommitIndexWithAttributionAndRefresh
                  observer
                  (resolvedRepository resolved)
                  (resolvedCommitOid resolved)
                  database
                  (\_ -> throwIO ThreadKilled)
              )
              :: IO (Either AsyncException PostCommitIndexResult)
          closeColdCompileAttribution observer
          cancelled @?= Left ThreadKilled
          artifactText <- BS8.unpack <$> BS8.readFile artifact
          case parseAttributionArtifact artifactText of
            Left problem -> assertFailure ("post-close cancellation attribution: " <> problem)
            Right (AttributionArtifact rows) ->
              [ (phase, succeeded)
              | AttributionEnd _ phase _ _ _ succeeded <- rows
              , phase `elem` [CompileOutcomeEvaluation, PostCloseProvenanceRefresh, PostCloseFingerprintValidation]
              ]
                @?= [(CompileOutcomeEvaluation, True), (PostCloseProvenanceRefresh, False)]
          doesFileExist database >>= (@?= False)
          siblings <- listDirectory repository
          assertBool
            "post-close cancellation leaves no task-owned candidate"
            (not (any (isPrefixOf "post-close.sqlite.post-commit-") siblings))
      ]

data HealthyRepositorySeed = HealthyRepositorySeed
  { healthyCopySeed :: RepositorySeed,
    healthyRepositoryTemplate :: Repository,
    healthyCommitOid :: GitOid
  }

data BasisRepositorySeed = BasisRepositorySeed
  { basisCopySeed :: RepositorySeed,
    basisRepositoryTemplate :: Repository,
    basisCommitOid :: GitOid
  }

data CompilerRepositorySeeds = CompilerRepositorySeeds
  { compilerHealthySeed :: HealthyRepositorySeed,
    compilerBasisSeed :: BasisRepositorySeed
  }

createCompilerRepositorySeeds :: IO CompilerRepositorySeeds
createCompilerRepositorySeeds = do
  healthy <- createHealthyRepositorySeed
  basis <- createBasisRepositorySeed `onException` removeHealthyRepositorySeed healthy
  pure (CompilerRepositorySeeds healthy basis)

removeCompilerRepositorySeeds :: CompilerRepositorySeeds -> IO ()
removeCompilerRepositorySeeds seeds = do
  removeBasisRepositorySeed (compilerBasisSeed seeds)
  removeHealthyRepositorySeed (compilerHealthySeed seeds)

createHealthyRepositorySeed :: IO HealthyRepositorySeed
createHealthyRepositorySeed =
  do
    (seed, (template, commitOid)) <-
      createRepositorySeedWith "adrai healthy compiler seed" $ \repository -> do
        initTestRepository repository
        basisText <- commitFile repository "seed.txt" "basis"
        basis <- requireGitOid basisText
        files <- requireFixture (healthyCompilerFiles basis)
        commitText <- commitFiles repository files
        discovered <- requireRepository repository
        commitOid <- requireGitOid commitText
        pure (discovered, commitOid)
    auditRepositorySeedCopy seed template commitOid "adrai healthy compiler seed audit"
      `onException` removeRepositorySeed seed
    pure (HealthyRepositorySeed seed template commitOid)

removeHealthyRepositorySeed :: HealthyRepositorySeed -> IO ()
removeHealthyRepositorySeed = removeRepositorySeed . healthyCopySeed

createBasisRepositorySeed :: IO BasisRepositorySeed
createBasisRepositorySeed = do
  (seed, (template, commitOid)) <-
    createRepositorySeedWith "adrai compiler basis seed" $ \repository -> do
      initTestRepository repository
      commitText <- commitFile repository "seed.txt" "basis"
      discovered <- requireRepository repository
      commitOid <- requireGitOid commitText
      pure (discovered, commitOid)
  auditRepositorySeedCopy seed template commitOid "adrai compiler basis seed audit"
    `onException` removeRepositorySeed seed
  pure (BasisRepositorySeed seed template commitOid)

removeBasisRepositorySeed :: BasisRepositorySeed -> IO ()
removeBasisRepositorySeed = removeRepositorySeed . basisCopySeed

withHealthyRepository :: IO HealthyRepositorySeed -> (FilePath -> Repository -> ResolvedRepositoryRevision -> IO value) -> IO value
withHealthyRepository getSeed action = do
  seed <- getSeed
  withRepositorySeedCopy (healthyCopySeed seed) "adrai cold compiler" $ \_ repository -> do
    repairCopiedHooksPath repository
    let copiedRepository = relocateCopiedRepository (healthyRepositoryTemplate seed) repository
        commitOid = healthyCommitOid seed
        resolved =
          ResolvedRepositoryRevision
            { resolvedRepository = copiedRepository,
              resolvedRequestedRevision = requireRevision "HEAD",
              resolvedCommitOid = commitOid,
              resolvedRevisionKey = RepositoryRevisionKey (repositoryCommonDir copiedRepository) commitOid
            }
    action repository copiedRepository resolved

withBasisRepository :: IO BasisRepositorySeed -> String -> (FilePath -> Repository -> GitOid -> IO value) -> IO value
withBasisRepository getSeed label action = do
  seed <- getSeed
  withRepositorySeedCopy (basisCopySeed seed) label $ \_ repository -> do
    repairCopiedHooksPath repository
    let copiedRepository = relocateCopiedRepository (basisRepositoryTemplate seed) repository
    action repository copiedRepository (basisCommitOid seed)

relocateCopiedRepository :: Repository -> FilePath -> Repository
relocateCopiedRepository template repository =
  template
    { repositoryWorktreeRoot = Just repository,
      repositoryGitDir = repository </> ".git",
      repositoryCommonDir = repository </> ".git",
      repositoryCommandDirectory = repository
    }

auditRepositorySeedCopy :: RepositorySeed -> Repository -> GitOid -> String -> IO ()
auditRepositorySeedCopy seed template knownCommit label =
  withRepositorySeedCopy seed label $ \_ repository -> do
    repairCopiedHooksPath repository
    let expected = relocateCopiedRepository template repository
        expectedHooks = repository </> ".git" </> "adrai-no-hooks"
        alternates = repository </> ".git" </> "objects" </> "info" </> "alternates"
    actual <- requireRepository repository
    repositoryWorktreeRoot actual @?= Just repository
    repositoryGitDir actual @?= repository </> ".git"
    repositoryCommonDir actual @?= repository </> ".git"
    repositoryCommandDirectory actual @?= repository
    repositoryClient actual @?= repositoryClient template
    repositoryLayout actual @?= repositoryLayout template
    repositoryCommonIsBare actual @?= repositoryCommonIsBare template
    actual @?= expected
    hooks <- Text.unpack . outputText <$> gitSuccess repository ["config", "--local", "--get", "core.hooksPath"] BS.empty
    normalise hooks @?= normalise expectedHooks
    doesDirectoryExist expectedHooks >>= (@?= True)
    doesFileExist alternates >>= (@?= False)
    resolved <- requireResolvedFrom actual (gitOidText knownCommit)
    resolvedRequestedRevision resolved @?= requireRevision (gitOidText knownCommit)
    resolvedCommitOid resolved @?= knownCommit
    repositoryRevisionNamespace (resolvedRevisionKey resolved) @?= repositoryCommonDir actual

repairCopiedHooksPath :: FilePath -> IO ()
repairCopiedHooksPath repository = do
  _ <- gitSuccess repository ["config", "core.hooksPath", repository </> ".git" </> "adrai-no-hooks"] BS.empty
  pure ()

withCompilerRepository :: IO BasisRepositorySeed -> (GitOid -> Either Text [(FilePath, BS.ByteString)]) -> (FilePath -> ResolvedRepositoryRevision -> IO value) -> IO value
withCompilerRepository getSeed fixture action =
  withBasisRepository getSeed "adrai cold compiler" $ \repository discovered basis -> do
    files <- requireFixture (fixture basis)
    _ <- commitFiles repository files
    resolved <- requireResolvedFrom discovered "HEAD"
    action repository resolved

requireFixture :: Either Text value -> IO value
requireFixture result =
  case result of
    Left problem -> assertFailure (Text.unpack problem)
    Right value -> pure value

requireGitOid :: Text -> IO GitOid
requireGitOid value =
  case mkGitOid value of
    Left problem -> assertFailure (show problem)
    Right oid -> pure oid

requireOid :: Text -> GitOid
requireOid value =
  case mkGitOid value of
    Left problem -> error (show problem)
    Right oid -> oid

-- | Resolve through the supplied client so production snapshot/history callers
-- can be observed without adding a test-only Git path to the compiler.
requireResolvedWithGitClient :: FilePath -> GitClient -> Text -> IO ResolvedRepositoryRevision
requireResolvedWithGitClient repository client revision = do
  discovered <- discoverRepository client repository >>= \case
    Left problem -> assertFailure (show problem)
    Right value -> pure value
  resolveRepositoryRevision discovered (requireRevision revision) >>= \case
    Left problem -> assertFailure (show problem)
    Right value -> pure value

nativeLateHistoryBatchFailure :: IO ()
nativeLateHistoryBatchFailure =
  withSystemTempDirectory "adrai native late history" $ \temporary -> do
      let repository = temporary </> "repository"
          helper = temporary </> "adrai-native-git-fixture-batch-p6133.exe"
      initTestRepository repository
      basisText <- commitFile repository "seed.txt" "basis"
      basis <-
        case mkGitOid basisText of
          Left problem -> assertFailure (show problem) >> fail "unreachable"
          Right value -> pure value
      healthyFiles <- requireFixture (healthyCompilerFiles basis)
      decisionBytes <-
        case lookup decisionPath healthyFiles of
          Nothing -> assertFailure "healthy compiler fixture omitted its decision document" >> fail "unreachable"
          Just bytes -> pure bytes
      _ <- commitFiles repository
        [ ("architecture/adrai/decisions/Rnative/history-" <> show index <> ".decision.md", decisionBytes <> BS8.pack ("\n<!-- native-history-" <> show index <> " -->\n"))
          | index <- [1 :: Int .. 257]
        ]
      resolved <- requireResolved repository "HEAD"
      raw <- observeRawRepositorySnapshotAt resolved >>= either (assertFailure . show) pure
      executable <- getExecutablePath
      copyFile executable helper
      BS.writeFile (temporary </> "fixture-shallow") "false\n"
      graph <- gitSuccess repository ["rev-list", "--topo-order", "--reverse", "--parents", Text.unpack (gitOidText (resolvedCommitOid resolved))] BS.empty
      BS.writeFile (temporary </> "fixture-rev-list") graph
      let graphRows = map BS8.words (BS8.lines graph)
      BS.writeFile (temporary </> "fixture-path-selection") (BS8.unlines [child | child : _ <- graphRows])
      let
          deltaInput = BS8.unlines [case row of [child] -> child; child : parent : _ -> child <> " " <> parent; _ -> BS.empty | row <- graphRows]
          diffArguments = ["--literal-pathspecs", "diff-tree", "--stdin", "--root", "-r", "-t", "--no-renames", "--raw", "--no-abbrev", "--always", "-z", "--pretty=tformat:%x1e%H"]
      diff <- gitSuccess repository diffArguments deltaInput
      BS.writeFile (temporary </> "fixture-diff-tree") diff
      let currentBlobs = [blob | observation <- rawRepositorySnapshotEntries raw, Just blob <- [repositoryTreeBlob observation]]
      forM_ currentBlobs $ \blob -> BS.writeFile (temporary </> "fixture-blob-" <> Text.unpack (gitOidText (gitBlobOid blob))) (gitBlobBytes blob)
      writeFile (temporary </> "fixture-malformed-after") "257"
      let wrappedRepository = (resolvedRepository resolved) {repositoryClient = GitClient helper}
          wrappedRevision = resolved {resolvedRepository = wrappedRepository}
          wrappedRaw = raw {rawRepositorySnapshotRevision = wrappedRevision}
      Async.withAsync (analyzeRepositorySnapshotWithHistoryCounts wrappedRaw) $ \analysis -> do
        waitForNativeHistoryPhase (temporary </> "fixture-phase") analysis
        timeout (10 * 1000000) (Async.wait analysis) >>= \case
          Just (Left (RepositorySnapshotGitError (GitInvalidOutput "cat-file batch" (GitMalformedObjectHeader "malformed"))), parsed, requested) -> do
            parsed @?= 256
            requested @?= 257
          result -> assertFailure ("expected native late history Git error after fixture phase, got " <> show result)
      seen <- lines <$> readFile (temporary </> "fixture-requests")
      length seen @?= 257
      boundaries <- lines <$> readFile (temporary </> "fixture-window-boundaries")
      boundaries @?= ["256:False"]
      helperPid <- read <$> readFile (temporary </> "fixture-helper.pid")
      waitForExactPidAbsence helperPid 50
      analyzeRepositorySnapshotWithHistoryCounts raw >>= \case
        (Right _, parsed, requested) -> do
          parsed @?= 257
          requested @?= 257
        result -> assertFailure ("fresh real-Git history retry failed: " <> show result)

waitForNativeHistoryPhase :: Show value => FilePath -> Async.Async value -> IO ()
waitForNativeHistoryPhase phaseFile analysis = go (300 :: Int)
  where
    go attempts = do
      exists <- doesFileExist phaseFile
      if exists
        then readFile phaseFile >>= (@?= "malformed\n")
        else do
          Async.poll analysis >>= \case
            Just result -> assertFailure ("native late-history analysis completed before fixture phase: " <> show result)
            Nothing
              | attempts <= 0 -> assertFailure "native late-history fixture phase did not arrive within 30 seconds"
              | otherwise -> threadDelay 100000 >> go (attempts - 1)

waitForExactPidAbsence :: Int -> Int -> IO ()
waitForExactPidAbsence pid attempts = do
  (_, output, _) <- readProcess (proc "powershell.exe" ["-NoProfile", "-NonInteractive", "-Command", "$ErrorActionPreference='Stop'; try { [void][Diagnostics.Process]::GetProcessById(" <> show pid <> "); [Console]::Out.Write('present') } catch [ArgumentException] { [Console]::Out.Write('absent') }"])
  if LBS.toStrict output == "absent"
    then pure ()
    else if attempts <= 0
      then assertFailure "exact native history helper PID remained after cleanup"
      else threadDelay 100000 >> waitForExactPidAbsence pid (attempts - 1)

writeTracingGitWrapper :: FilePath -> FilePath -> IO ()
writeTracingGitWrapper wrapper traceFile =
  BS.writeFile
    wrapper
    ( "@echo off\r\n"
        <> "echo %*>> \""
        <> BS8.pack traceFile
        <> "\"\r\ngit %*\r\n"
    )

assertSingleUnbufferedBlobSession :: FilePath -> IO ()
assertSingleUnbufferedBlobSession traceFile = do
  invocations <- BS.readFile traceFile
  let blobSessions =
        filter
          ( \line ->
              "cat-file" `BS.isInfixOf` line
                && "--batch" `BS.isInfixOf` line
                && not ("--batch-check" `BS.isInfixOf` line)
          )
          (BS8.lines invocations)
  length blobSessions @?= 1
  assertBool "production persistent blob session must not request --buffer" (not ("--buffer" `BS.isInfixOf` invocations))

managedSourceRows :: Connection -> IO [(Text, Text, Text, Text, Maybe BS.ByteString, Text)]
managedSourceRows connection =
  query_
    connection
    "SELECT path,oid,object_type,mode,bytes,parse_state FROM managed_source ORDER BY path"

requireRepository :: FilePath -> IO Repository
requireRepository path =
  discoverRepository systemGit path >>= \case
    Left problem -> assertFailure (show problem)
    Right repository -> pure repository

requireResolved :: FilePath -> Text -> IO ResolvedRepositoryRevision
requireResolved repository revision = do
  discovered <- requireRepository repository
  requireResolvedFrom discovered revision

requireResolvedFrom :: Repository -> Text -> IO ResolvedRepositoryRevision
requireResolvedFrom repository revision =
  resolveRepositoryRevision repository (requireRevision revision) >>= \case
    Left problem -> assertFailure (show problem)
    Right value -> pure value

requireCompiled :: Connection -> ResolvedRepositoryRevision -> IO ColdCompilerResult
requireCompiled connection resolved =
  coldCompileRepository connection resolved >>= \case
    Left problem -> assertFailure (show problem)
    Right result -> pure result

requireAnalyzed :: ResolvedRepositoryRevision -> IO AnalyzedRepositorySnapshot
requireAnalyzed resolved = do
  raw <-
    observeRawRepositorySnapshotAt resolved
      >>= \case
        Left problem -> assertFailure (show problem)
        Right value -> pure value
  analyzeRepositorySnapshot raw
    >>= \case
      Left problem -> assertFailure (show problem)
      Right value -> pure value

count :: Connection -> Text -> IO Int64
count connection table = do
  rows <- query_ connection (fromText ("SELECT count(*) FROM " <> table)) :: IO [Only Int64]
  case rows of
    [Only value] -> pure value
    result -> assertFailure ("unexpected count result: " <> show result)

meta :: Connection -> IO [(Text, Text)]
meta connection = query_ connection "SELECT key,value FROM meta ORDER BY key"

logicalRows :: Connection -> IO [(Text, Text)]
logicalRows connection =
  query_ connection "SELECT 'decision:'||record_id,title FROM decision_record UNION ALL SELECT 'connection:'||connection_id,relation_kind FROM connection_record UNION ALL SELECT 'search:'||item_id,title FROM search_document ORDER BY 1"

breakFirstPassage :: SearchMaterialization -> IO SearchMaterialization
breakFirstPassage materialization =
  case searchMaterializationPassages materialization of
    [] -> assertFailure "healthy materialization has no passage"
    passage : remaining ->
      pure
        materialization
          { searchMaterializationPassages =
              passage {searchPassageDocumentItemId = "missing-search-document"} : remaining
          }

fromText :: Text -> Query
fromText = fromString . Text.unpack

decisionPath :: FilePath
decisionPath = "architecture/adrai/decisions/R000/R00000000000000000000000000--use-a-cold-compiler.decision.md"
