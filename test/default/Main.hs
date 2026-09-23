{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Adrai.ConfigFormatTest
import qualified Adrai.CompilerMaterializationGoldenTest
import qualified Adrai.CompilerMaterializationProperties
import qualified Adrai.CompilerMaterializationTest
import qualified Adrai.CompilerSearchSqliteTest
import qualified Adrai.CompilerSnapshotTest
import qualified Adrai.CompilerAttributionTest
import qualified Adrai.ConsistencyTest
import qualified Adrai.ColdCompilerTest
import qualified Adrai.ColdCompilerGoldenTest
import qualified Adrai.Compiler.CacheSelectionTest
import qualified Adrai.Compiler.CacheSyncTest
import qualified Adrai.Compiler.DocumentCacheTest
import qualified Adrai.CurrentSearchTest
import qualified Adrai.CacheIntegrationTest
import qualified Adrai.MutationE2ETest
import qualified Adrai.EnvironmentTest
import qualified Adrai.CliContractTest
import qualified Adrai.CoverageLedgerAudit
import qualified Adrai.CoverageLedgerAuditTest
import qualified Adrai.CoverageLedgerTest
import qualified Adrai.DomainFormatTest
import qualified Adrai.DomainProperties
import qualified Adrai.FixturePrngTest
import qualified Adrai.FixtureQueryMaterializationTest
import qualified Adrai.FixtureRelevanceTest
import qualified Adrai.FingerprintGuardTest
import qualified Adrai.FormatFoundationTest
import qualified Adrai.FormatProperties
import qualified Adrai.GoldenFixturesTest
import qualified Adrai.GraphProperties
import qualified Adrai.GraphTest
import qualified Adrai.GitBatchTest
import qualified Adrai.GitDiscoveryTest
import qualified Adrai.GitTest
import Control.Monad (replicateM, unless, when)
import qualified Adrai.IdentityTest
import qualified Adrai.IntegrityAdversarialTest
import qualified Adrai.IntegrityTest
import qualified Adrai.ManagedDocumentFormatTest
import qualified Adrai.ManagedPathContractTest
import qualified Adrai.MarkdownTest
import qualified Adrai.MutationServiceTest
import qualified Adrai.ProvenanceFormatTest
import qualified Adrai.Provenance.LockTest
import qualified Adrai.P206GoldenTest
import qualified Adrai.P306QualityGoldenTest
import qualified Adrai.PassageFtsTest
import qualified Adrai.ProjectionProperties
import qualified Adrai.QueryHistoryTest
import qualified Adrai.QueryIntegrationTest
import qualified Adrai.ProvenanceOverlayTest
import qualified Adrai.ProvenanceReadTest
import qualified Adrai.ReconciliationProperties
import qualified Adrai.RepositoryIsolationTest
import qualified Adrai.RepositorySnapshotTest
import qualified Adrai.RepositoryTest
import qualified Adrai.RelevanceProperties
import qualified Adrai.RelevanceIntegrationTest
import qualified Adrai.RelevanceGoldenTest
import qualified Adrai.RelevanceQualityTest
import qualified Adrai.RelevanceTest
import qualified Adrai.RetrievalPlanGoldenTest
import qualified Adrai.RetrievalSqliteTest
import qualified Adrai.SearchRetrievalProperties
import qualified Adrai.SearchRetrievalTest
import qualified Adrai.SearchRankingProperties
import qualified Adrai.SearchRankingTest
import qualified Adrai.SearchResultGoldenTest
import qualified Adrai.SearchVectorCorpusTest
import qualified Adrai.SearchVectorReuseTest
import qualified Adrai.SemanticIdentityTest
import qualified Adrai.ScopeFormatTest
import qualified Adrai.ServiceTest
import qualified Adrai.StateTest
import qualified Adrai.TomlCanonicalTest
import qualified Adrai.TransactionTest
import qualified Adrai.TypesProperties
import qualified Adrai.TypesTest
import qualified Adrai.VectorProperties
import qualified Adrai.VectorQualityTest
import qualified Adrai.VectorTest
import qualified Adrai.WebContractTest
import qualified Adrai.WebCompilationTest
import qualified Adrai.WebEventsTest
import qualified Adrai.WebExplorerApiTest
import qualified Adrai.WebServerTest
import qualified Adrai.WebWatchTest
import Control.Concurrent (getNumCapabilities, threadDelay)
import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BS8
import System.Directory (doesFileExist)
import Database.SQLite.Simple (close, execute_, open)
import Hedgehog (property, success)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Environment
  ( getArgs,
    getExecutablePath,
    getProgName,
    lookupEnv,
    setEnv,
  )
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO (hClose, hFlush, hIsEOF, hWaitForInput, stdin, stdout)
import System.Process (StdStream (Inherit), createProcess, proc, std_in)
import System.Win32 (getCurrentProcessId)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit (testCase)
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (catMaybes)

main :: IO ()
main = do
  arguments <- getArgs
  program <- getProgName
  marker <- lookupEnv "ADRAI_TEST_REFERENCE_TRANSACTION_HOOK"
  case takeFileName program of
    finishingHelper | finishingHelper == nativeGitPersistentFinishHelperProgram -> nativeGitPersistentFinishHelper arguments
    persistentHelper | persistentHelper == nativeGitPersistentHelperProgram -> nativeGitPersistentHelper arguments
    fixtureHelper | fixtureHelper == nativeGitFixtureBatchHelperProgram -> nativeGitFixtureBatchHelper arguments
    helperProgram | helperProgram == nativeGitHelperProgram -> nativeGitHelper arguments
    "reference-transaction" ->
      case marker of
        Just "p6-03f-0a-native-hook-v1" -> referenceTransactionHook arguments
        _ -> exitWith (ExitFailure 64)
    _ | "--integration-cli-path-probe" `elem` arguments -> integrationCliPathProbe
    _ -> normalMain arguments

-- | Native test-only Git child.  The exact executable basename is the
-- dispatch sentinel; all remaining arguments are deliberately ignored so it
-- can stand in for Git with any command-line shape.  Its PID file gives the
-- parent test a direct, unambiguous cancellation target without a shell
-- wrapper or process-list search.
nativeGitHelperProgram :: FilePath
nativeGitHelperProgram = "adrai-native-git-helper-p6115.exe"

nativeGitPersistentHelperProgram :: FilePath
nativeGitPersistentHelperProgram = "adrai-native-git-persistent-helper-p6115.exe"

nativeGitFixtureBatchHelperProgram :: FilePath
nativeGitFixtureBatchHelperProgram = "adrai-native-git-fixture-batch-p6133.exe"

-- | A distinct test-only persistent helper which completes one batch-check
-- exchange, closes stdout after the caller closes stdin, then deliberately
-- remains alive.  This positions the parent exactly in the post-protocol
-- process-wait path without changing the silent-pipe fixture's behaviour.
nativeGitPersistentFinishHelperProgram :: FilePath
nativeGitPersistentFinishHelperProgram = "adrai-native-git-persistent-finish-helper-p6115.exe"

nativeGitHelperPidFile :: FilePath
nativeGitHelperPidFile = "adrai-native-git-helper-p6115.pid"

nativeGitHelperTreePhaseFile :: FilePath
nativeGitHelperTreePhaseFile = "adrai-native-git-helper-p6115.tree-phase"

nativeGitHelperDescendantPidFile :: FilePath
nativeGitHelperDescendantPidFile = "adrai-native-git-helper-p6115.descendant.pid"

nativeGitHelperDescendantPhaseFile :: FilePath
nativeGitHelperDescendantPhaseFile = "adrai-native-git-helper-p6115.descendant-phase"

nativeGitHelperDescendantExitFile :: FilePath
nativeGitHelperDescendantExitFile = "adrai-native-git-helper-p6115.descendant-exit"

nativeGitHelper :: [String] -> IO ()
nativeGitHelper arguments
  | "--adrai-native-git-helper-descendant" `elem` arguments = cooperativeDescendant
  | "--buffer" `elem` arguments = do
      requests <- BS8.getContents
      BS8.putStr (BS8.unlines [request <> " commit 1" | request <- BS8.lines requests])
  | "ls-tree" `elem` arguments = blockTreeLookup
  | otherwise = serveBareObjectInfo
  where
    serveBareObjectInfo = do
      atEnd <- hIsEOF stdin
      unless atEnd $ do
        request <- BS8.getLine
        BS8.putStrLn (request <> " commit 1")
        hFlush stdout
        serveBareObjectInfo
    blockTreeLookup = do
      executable <- getExecutablePath
      processId <- getCurrentProcessId
      -- The root deliberately never consumes stdin. Its cooperative descendant
      -- shares that input and exits only when the production cleanup closes the
      -- captured writer; no PID-based descendant termination is involved.
      _ <- createProcess ((proc executable ["--adrai-native-git-helper-descendant", "--stubborn-inherited-pipes"]) {std_in = Inherit})
      writeFile (takeDirectory executable </> nativeGitHelperPidFile) (show processId)
      writeFile (takeDirectory executable </> nativeGitHelperTreePhaseFile) "started"
      let block = threadDelay 1000000 >> block
      block
    cooperativeDescendant = do
      executable <- getExecutablePath
      processId <- getCurrentProcessId
      writeFile (takeDirectory executable </> nativeGitHelperDescendantPidFile) (show processId)
      writeFile (takeDirectory executable </> nativeGitHelperDescendantPhaseFile) "holding-inherited-pipes"
      -- Keep inherited stdout/stderr live beyond cancellation of the root.
      -- This exits deterministically without any PID-directed cleanup.
      threadDelay 3000000
      writeFile (takeDirectory executable </> nativeGitHelperDescendantExitFile) "self-exited"
      pure ()

nativeGitPersistentHelper :: [String] -> IO ()
nativeGitPersistentHelper arguments
  | "--batch" `elem` arguments || "--batch-check" `elem` arguments = do
      processId <- getCurrentProcessId
      executable <- getExecutablePath
      writeFile (takeDirectory executable </> nativeGitHelperPidFile) (show processId)
      writeFile (takeDirectory executable </> nativeGitHelperTreePhaseFile) "silent-batch"
      let block = threadDelay 1000000 >> block
      block
   | otherwise = pure ()

-- | Single-process protocol fixture for production callers.  Fixture files
-- beside the copied executable provide precomputed tree output and blob bytes;
-- the helper itself never launches Git or any other child process.
nativeGitFixtureBatchHelper :: [String] -> IO ()
nativeGitFixtureBatchHelper arguments = do
  executable <- getExecutablePath
  let directory = takeDirectory executable
      fixture name = directory </> name
  if any ("--batch-check" `isPrefixOf`) arguments
    then do
      modeFile <- doesFileExist (fixture "fixture-mode")
      mode <- if modeFile then lines <$> readFile (fixture "fixture-mode") else pure []
      case mode of
        "tree-cross-window-replay" : _ -> serveTreeCrossWindowReplay directory
        _ -> pure ()
    else
      if "--batch" `elem` arguments
        then do
          processId <- getCurrentProcessId
          writeFile (fixture "fixture-helper.pid") (show processId)
          threshold <- (read <$> readFile (fixture "fixture-malformed-after") :: IO Int)
          modeFile <- doesFileExist (fixture "fixture-mode")
          mode <- if modeFile then lines <$> readFile (fixture "fixture-mode") else pure ["malformed"]
          serve directory threshold (case mode of value : _ -> value; [] -> "malformed") 0
      else
        if "ls-tree" `elem` arguments
          then do
            let treeFile = if any (== ".adrai.toml") arguments then fixture "fixture-config-tree" else fixture "fixture-managed-tree"
            exists <- doesFileExist treeFile
            if exists then BS8.readFile treeFile >>= BS8.putStr else pure ()
          else
            if "--is-shallow-repository" `elem` arguments
              then BS8.readFile (fixture "fixture-shallow") >>= BS8.putStr
              else
                if "rev-list" `elem` arguments
                  then BS8.readFile (fixture (if "--parents" `elem` arguments then "fixture-rev-list" else "fixture-path-selection")) >>= BS8.putStr
                  else
                    if "diff-tree" `elem` arguments
                      then BS8.readFile (fixture "fixture-diff-tree") >>= BS8.putStr
                      else pure ()
  where
    serveTreeCrossWindowReplay directory = do
      first <- BS8.getLine
      if not (BS8.elem ':' first)
        then BS8.putStrLn (first <> " commit 1") >> hFlush stdout
        else do
          processId <- getCurrentProcessId
          writeFile (directory </> "fixture-helper.pid") (show processId)
          firstWindow <- (first :) <$> replicateM 255 BS8.getLine
          mapM_ (recordRequest directory) firstWindow
          mapM_ (BS8.putStrLn . (<> " missing") . requestExpression) firstWindow
          hFlush stdout
          next <- BS8.getLine
          recordRequest directory next
          writeFile (directory </> "fixture-phase") "replayed-first-window-expression"
          BS8.putStrLn (requestExpression first <> " missing")
          hFlush stdout
          let block = threadDelay 1000000 >> block
          block
    recordRequest directory request =
      appendFile (directory </> "fixture-requests") (BS8.unpack request <> "\n")
    requestExpression = BS8.takeWhile (/= ' ')
    serve directory threshold mode count = do
      atEnd <- hIsEOF stdin
      if atEnd
        then if mode == "withhold-eof" then failure directory mode BS8.empty else pure ()
        else do
          request <- BS8.getLine
          appendFile (directory </> "fixture-requests") (BS8.unpack request <> "\n")
          if count + 1 == threshold
            then failure directory mode request
            else do
              when ((count + 1) `mod` 256 == 0) $ do
                queued <- hWaitForInput stdin 0
                appendFile (directory </> "fixture-window-boundaries") (show (count + 1) <> ":" <> show queued <> "\n")
              bytes <- BS8.readFile (directory </> "fixture-blob-" <> BS8.unpack request)
              BS8.putStr request
              BS8.putStr " blob "
              BS8.putStr (BS8.pack (show (BS8.length bytes)))
              BS8.putStr "\n"
              BS8.putStr bytes
              BS8.putStr "\n"
              hFlush stdout
              serve directory threshold mode (count + 1)
    failure directory mode request = do
      appendFile (directory </> "fixture-phase") (mode <> "\n")
      case mode of
        "wrong-oid" -> BS8.putStrLn (BS8.replicate 40 '0' <> " blob 1\nx")
        "wrong-type" -> BS8.putStrLn (request <> " tree 1\nx")
        "invalid-size" -> BS8.putStrLn (request <> " blob -1")
        "oversized-size" -> BS8.putStrLn (request <> " blob " <> BS8.pack (show (toInteger (maxBound :: Int) + 1)))
        "eof-response" -> pure ()
        "withhold-response" -> pure ()
        _ -> BS8.putStrLn "malformed"
      hFlush stdout
      when (mode /= "eof-response") $ do
        let loop = threadDelay 1000000 >> loop
        loop

nativeGitPersistentFinishHelper :: [String] -> IO ()
nativeGitPersistentFinishHelper arguments
  | "--batch-check" `elem` arguments = do
      processId <- getCurrentProcessId
      executable <- getExecutablePath
      request <- BS8.getLine
      BS8.putStrLn (request <> " missing")
      hFlush stdout
      drainInput
      hClose stdout
      writeFile (takeDirectory executable </> nativeGitHelperPidFile) (show processId)
      writeFile (takeDirectory executable </> nativeGitHelperTreePhaseFile) "post-trailing-proof-alive"
      let block = threadDelay 1000000 >> block
      block
  | otherwise = pure ()
  where
    drainInput = do
      atEnd <- hIsEOF stdin
      unless atEnd (BS8.getLine >> drainInput)

-- | Test-only child probe used to verify the scrubbed integration environment.
-- It runs before normal test-runner initialization so the observed PATH is the
-- one supplied by 'spawnAdrai'.
integrationCliPathProbe :: IO ()
integrationCliPathProbe = do
  maybePath <- lookupEnv "PATH"
  case maybePath of
    Just path -> putStr path
    Nothing -> exitWith (ExitFailure 64)

normalMain :: [String] -> IO ()
normalMain arguments = do
  setLocaleEncoding utf8
  -- Ensure git and adrai are discoverable on PATH for subprocesses.
  -- Stack test subprocesses may have a minimal PATH.
  let extraPaths =
        [ "C:\\Program Files\\Git\\cmd",
          "D:\\Projects\\adrai\\.stack-work\\install\\0fc81caf\\bin"
        ]
  currentPath <- lookupEnv "PATH"
  let newPath = intercalate ";" (catMaybes [currentPath] ++ extraPaths)
  setEnv "PATH" newPath
  -- Preserve an executable selected by the caller.  Several integration
  -- contracts intentionally exercise the exact binary named by ADRAI_EXE;
  -- replacing it here with this machine's Stack install would silently test
  -- a different build.  ADRAI_EXE is therefore deliberately inherited.
  normalMain' arguments

normalMain' :: [String] -> IO ()
normalMain' arguments =
  case arguments of
    ["--assert-adrai-exe-preserved"] -> assertAdraiExePreserved
    ["--write-p3-01-goldens"] -> Adrai.VectorQualityTest.writeP301Goldens
    ["--write-p3-02-goldens"] -> Adrai.RetrievalPlanGoldenTest.writeP302Goldens
    ["--write-p3-03-goldens"] -> Adrai.CompilerMaterializationGoldenTest.writeP303Goldens
    ["--write-p3-04-goldens"] -> Adrai.SearchResultGoldenTest.writeP304Goldens
    ["--write-p3-05-goldens"] -> Adrai.RelevanceGoldenTest.writeP305Goldens
    ["--write-p3-06-goldens"] -> Adrai.P306QualityGoldenTest.writeP306Goldens
    ["--write-p4-03-goldens"] -> Adrai.ColdCompilerGoldenTest.writeP403Goldens
    ["--coverage-ledger-report"] -> Adrai.CoverageLedgerAudit.writeCurrentLedgerReport
    ["--require-coverage-ledger-closed"] -> Adrai.CoverageLedgerAudit.requireCurrentLedgerClosed
    _ -> do
      configuredThreads <- lookupEnv "TASTY_NUM_THREADS"
      case configuredThreads of
        Nothing -> do
          -- Tasty 1.5.4 defaults to processor count even under +RTS -N1.
          capabilities <- getNumCapabilities
          setEnv "TASTY_NUM_THREADS" (show capabilities)
        Just _ -> pure ()
      defaultMain tests

-- | A focused entry-point regression hook.  It runs after the ordinary test
-- initialization (including PATH setup) without spawning the named program or
-- constructing the suite, so callers can safely prove that an externally
-- selected executable remains visible:
--
-- @ADRAI_EXE=C:\\external\\adrai.exe ADRAI_TEST_EXPECTED_EXE=C:\\external\\adrai.exe stack test adrai:adrai-test --test-arguments=--assert-adrai-exe-preserved@
assertAdraiExePreserved :: IO ()
assertAdraiExePreserved = do
  actual <- lookupEnv "ADRAI_EXE"
  expected <- lookupEnv "ADRAI_TEST_EXPECTED_EXE"
  case (actual, expected) of
    (Just actualPath, Just expectedPath)
      | not (null actualPath), actualPath == expectedPath -> pure ()
    _ -> exitWith (ExitFailure 64)

referenceTransactionHook :: [String] -> IO ()
referenceTransactionHook [phase] = do
  behavior <- lookupEnv "ADRAI_TEST_REFERENCE_TRANSACTION_BEHAVIOR"
  phaseLog <- lookupEnv "ADRAI_TEST_REFERENCE_TRANSACTION_PHASE_LOG"
  headPath <- lookupEnv "ADRAI_TEST_REFERENCE_TRANSACTION_HEAD_PATH"
  case (behavior, phaseLog) of
    (Just configuredBehavior, Just logPath) -> do
      appendFile logPath (phase <> "\n")
      case (phase, configuredBehavior) of
        ("preparing", "reject-prepared") -> exitWith (ExitFailure 42)
        ("preparing", behaviorWithHead) | hookSwitchPrefix `prefixOf` behaviorWithHead -> exitWith (ExitFailure 42)
        ("committed", "reject-prepared") -> exitWith ExitSuccess
        ("aborted", "reject-prepared") -> exitWith ExitSuccess
        ("committed", behaviorWithHead) | hookSwitchPrefix `prefixOf` behaviorWithHead -> exitWith ExitSuccess
        ("aborted", behaviorWithHead) | hookSwitchPrefix `prefixOf` behaviorWithHead -> do
          let target = drop (length hookSwitchPrefix) behaviorWithHead
          case headPath of
            Just configuredHeadPath | validHeadTarget target -> BS8.writeFile configuredHeadPath (BS8.pack ("ref: " <> target <> "\n")) >> exitWith ExitSuccess
            _ -> exitWith (ExitFailure 65)
        _ -> exitWith (ExitFailure 65)
    _ -> exitWith (ExitFailure 65)
referenceTransactionHook _ = exitWith (ExitFailure 65)

prefixOf :: String -> String -> Bool
prefixOf prefix value = take (length prefix) value == prefix

validHeadTarget :: String -> Bool
validHeadTarget target =
  headRefPrefix `prefixOf` target
    && not (null (drop (length headRefPrefix) target))
    && all (`notElem` [' ', '\t', '\r', '\n']) target

hookSwitchPrefix :: String
hookSwitchPrefix = "reject-prepared-switch-head:"

headRefPrefix :: String
headRefPrefix = "refs/heads/"

tests :: TestTree
tests =
  testGroup
    "ADRAI"
    [ testGroup
        "scaffold"
        [ testProperty "Hedgehog is wired into the test suite" $
            property success,
          testCase "SQLite has FTS5 enabled" $
            bracket (open ":memory:") close $ \connection ->
              execute_ connection "CREATE VIRTUAL TABLE scaffold_search USING fts5(content)"
        ],
      Adrai.TypesTest.tests,
      Adrai.FormatFoundationTest.tests,
      Adrai.TypesProperties.tests,
      Adrai.GoldenFixturesTest.tests,
      Adrai.CoverageLedgerTest.tests,
      Adrai.CoverageLedgerAuditTest.tests,
      Adrai.FixturePrngTest.tests,
      Adrai.FixtureRelevanceTest.tests,
      Adrai.DomainFormatTest.tests,
      Adrai.ScopeFormatTest.tests,
      Adrai.ProvenanceFormatTest.tests,
      Adrai.TomlCanonicalTest.tests,
      Adrai.ConfigFormatTest.tests,
      Adrai.ManagedDocumentFormatTest.tests,
      Adrai.GraphTest.tests,
      Adrai.QueryHistoryTest.tests,
      Adrai.ServiceTest.tests,
      Adrai.IdentityTest.tests,
      Adrai.StateTest.tests,
      Adrai.ManagedPathContractTest.tests,
      Adrai.MarkdownTest.tests,
      Adrai.MutationServiceTest.tests,
      Adrai.TransactionTest.tests,
      Adrai.SemanticIdentityTest.tests,
      Adrai.IntegrityTest.tests,
      testGroup
          "P2-06"
          [ Adrai.FormatProperties.tests,
            Adrai.DomainProperties.tests,
            Adrai.GraphProperties.tests,
            Adrai.ReconciliationProperties.tests,
            Adrai.ProjectionProperties.tests,
            Adrai.P206GoldenTest.tests
          ],
      testGroup
          "P3-01"
          [ Adrai.VectorTest.tests,
            Adrai.VectorProperties.tests,
            Adrai.VectorQualityTest.tests
          ],
      testGroup
          "P3-02"
          [ Adrai.SearchRetrievalTest.tests,
            Adrai.SearchRetrievalProperties.tests,
            Adrai.RetrievalSqliteTest.tests,
            Adrai.RetrievalPlanGoldenTest.tests
          ],
      testGroup
          "P3-03"
          [ Adrai.CompilerMaterializationTest.tests,
            Adrai.CompilerMaterializationProperties.tests,
            Adrai.CompilerSearchSqliteTest.tests,
            Adrai.CompilerMaterializationGoldenTest.tests
          ],
      testGroup
          "P3-04"
          [ Adrai.SearchRankingTest.tests,
            Adrai.SearchRankingProperties.tests,
            Adrai.CurrentSearchTest.tests,
            Adrai.SearchResultGoldenTest.tests
          ],
      testGroup
          "P3-05"
          [ Adrai.RelevanceTest.tests,
            Adrai.RelevanceProperties.tests,
            Adrai.PassageFtsTest.tests,
            Adrai.RelevanceIntegrationTest.tests,
            Adrai.RelevanceGoldenTest.tests
          ],
      testGroup
          "P3-06"
          [ Adrai.FixtureQueryMaterializationTest.tests,
            Adrai.P306QualityGoldenTest.tests,
            Adrai.RelevanceQualityTest.tests,
            Adrai.SearchVectorCorpusTest.tests,
            Adrai.SearchVectorReuseTest.tests
          ],
      testGroup
          "P4-01"
          [ Adrai.GitTest.tests,
            Adrai.GitDiscoveryTest.tests,
            Adrai.GitBatchTest.tests
          ],
      testGroup
          "P4-02"
          [ Adrai.RepositoryTest.tests,
            Adrai.RepositorySnapshotTest.tests,
            Adrai.RepositoryIsolationTest.tests
          ],
      testGroup
          "P4-03"
          [ Adrai.ColdCompilerGoldenTest.tests,
            Adrai.CompilerAttributionTest.tests,
            Adrai.CompilerSnapshotTest.tests,
            Adrai.ColdCompilerTest.tests,
            Adrai.IntegrityAdversarialTest.tests,
            Adrai.Compiler.CacheSelectionTest.tests,
            Adrai.FingerprintGuardTest.tests
          ],
      testGroup
          "P4-05"
          [ Adrai.Compiler.CacheSyncTest.tests,
            Adrai.Provenance.LockTest.tests,
            Adrai.Compiler.DocumentCacheTest.tests
          ],
      testGroup
          "P4-06"
          [ Adrai.CliContractTest.tests
          ],
      testGroup
          "P4-07"
          [ Adrai.CacheIntegrationTest.tests,
            Adrai.ConsistencyTest.tests,
            Adrai.QueryIntegrationTest.tests,
            Adrai.EnvironmentTest.tests,
            Adrai.ProvenanceOverlayTest.tests,
            Adrai.ProvenanceReadTest.tests
          ],
      testGroup
          "P5-05"
          [ Adrai.MutationE2ETest.tests ]
      , testGroup
          "P7-01"
          [ Adrai.WebContractTest.tests ]
      , testGroup
          "P7-02"
          [ Adrai.WebServerTest.tests ]
      , testGroup
          "P7-03"
          [ Adrai.WebEventsTest.tests,
            Adrai.WebWatchTest.tests,
            Adrai.WebCompilationTest.exactRevisionCompilationTest,
            Adrai.WebCompilationTest.tests
          ]
      , testGroup
          "P7-04"
          [ Adrai.WebExplorerApiTest.tests,
            Adrai.WebServerTest.generationExhaustionTest
          ]
    ]
