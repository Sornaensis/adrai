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
import qualified Adrai.EvolutionCompareAnnTest
import qualified Adrai.CachePathIntegrationTest
import qualified Adrai.SearchCliTest
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
import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BS8
import Database.SQLite.Simple (close, execute_, open)
import Hedgehog (property, success)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Environment
  ( getArgs,
    getProgName,
    lookupEnv,
    setEnv,
  )
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.FilePath (takeFileName)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit (testCase)
import Data.List (intercalate)
import Data.Maybe (catMaybes)

main :: IO ()
main = do
  arguments <- getArgs
  program <- getProgName
  marker <- lookupEnv "ADRAI_TEST_REFERENCE_TRANSACTION_HOOK"
  case (takeFileName program, marker) of
    ("reference-transaction", Just "p6-03f-0a-native-hook-v1") -> referenceTransactionHook arguments
    ("reference-transaction", _) -> exitWith (ExitFailure 64)
    _ -> normalMain arguments

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
    _ -> defaultMain tests

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
          [ Adrai.CachePathIntegrationTest.tests,
            Adrai.Compiler.CacheSyncTest.tests,
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
            Adrai.EvolutionCompareAnnTest.tests,
            Adrai.ProvenanceOverlayTest.tests,
            Adrai.ProvenanceReadTest.tests,
            Adrai.SearchCliTest.tests
          ],
      testGroup
          "P5-05"
          [ Adrai.MutationE2ETest.tests ]
    ]
