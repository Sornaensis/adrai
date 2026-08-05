{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Adrai.ConfigFormatTest
import qualified Adrai.CompilerMaterializationGoldenTest
import qualified Adrai.CompilerMaterializationProperties
import qualified Adrai.CompilerMaterializationTest
import qualified Adrai.CompilerSearchSqliteTest
import qualified Adrai.CompilerSnapshotTest
import qualified Adrai.ColdCompilerTest
import qualified Adrai.ColdCompilerGoldenTest
import qualified Adrai.Compiler.CacheSelectionTest
import qualified Adrai.Compiler.CacheSyncTest
import qualified Adrai.Compiler.DocumentCacheTest
import qualified Adrai.CurrentSearchTest
import qualified Adrai.CoverageLedgerTest
import qualified Adrai.DomainFormatTest
import qualified Adrai.DomainProperties
import qualified Adrai.FixtureContractTest
import qualified Adrai.FixturePlanTest
import qualified Adrai.FixturePrngTest
import qualified Adrai.FixtureProperties
import qualified Adrai.FixtureQueryMaterializationTest
import qualified Adrai.FixtureRelevanceTest
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
import qualified Adrai.ProvenanceFormatTest
import qualified Adrai.Provenance.LockTest
import qualified Adrai.P206GoldenTest
import qualified Adrai.P306QualityGoldenTest
import qualified Adrai.PassageFtsTest
import qualified Adrai.ProjectionProperties
import qualified Adrai.QueryHistoryTest
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
import qualified Adrai.RetrievalScaleTest
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
import qualified Adrai.TypesProperties
import qualified Adrai.TypesTest
import qualified Adrai.VectorProperties
import qualified Adrai.VectorQualityTest
import qualified Adrai.VectorTest
import Control.Exception (bracket)
import Database.SQLite.Simple (close, execute_, open)
import Hedgehog (property, success)
import System.Environment (getArgs)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit (testCase)

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    ["--write-p3-01-goldens"] -> Adrai.VectorQualityTest.writeP301Goldens
    ["--write-p3-02-goldens"] -> Adrai.RetrievalPlanGoldenTest.writeP302Goldens
    ["--write-p3-03-goldens"] -> Adrai.CompilerMaterializationGoldenTest.writeP303Goldens
    ["--write-p3-04-goldens"] -> Adrai.SearchResultGoldenTest.writeP304Goldens
    ["--write-p3-05-goldens"] -> Adrai.RelevanceGoldenTest.writeP305Goldens
    ["--write-p3-06-goldens"] -> Adrai.P306QualityGoldenTest.writeP306Goldens
    ["--write-p4-03-goldens"] -> Adrai.ColdCompilerGoldenTest.writeP403Goldens
    _ -> defaultMain tests

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
      Adrai.FixturePrngTest.tests,
      Adrai.FixtureRelevanceTest.tests,
      Adrai.FixturePlanTest.tests,
      Adrai.FixtureProperties.tests,
      Adrai.FixtureContractTest.tests,
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
            Adrai.RetrievalScaleTest.tests,
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
            Adrai.CompilerSnapshotTest.tests,
            Adrai.ColdCompilerTest.tests,
            Adrai.IntegrityAdversarialTest.tests,
            Adrai.Compiler.CacheSelectionTest.tests
          ],
      testGroup
          "P4-05"
          [ Adrai.Compiler.CacheSyncTest.tests,
            Adrai.Provenance.LockTest.tests,
            Adrai.Compiler.DocumentCacheTest.tests
          ]
    ]
