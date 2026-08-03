{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Adrai.ConfigFormatTest
import qualified Adrai.CoverageLedgerTest
import qualified Adrai.DomainFormatTest
import qualified Adrai.DomainProperties
import qualified Adrai.FixtureContractTest
import qualified Adrai.FixturePlanTest
import qualified Adrai.FixturePrngTest
import qualified Adrai.FixtureProperties
import qualified Adrai.FixtureRelevanceTest
import qualified Adrai.FormatFoundationTest
import qualified Adrai.FormatProperties
import qualified Adrai.GoldenFixturesTest
import qualified Adrai.GraphProperties
import qualified Adrai.GraphTest
import qualified Adrai.IdentityTest
import qualified Adrai.IntegrityTest
import qualified Adrai.ManagedDocumentFormatTest
import qualified Adrai.ManagedPathContractTest
import qualified Adrai.MarkdownTest
import qualified Adrai.ProvenanceFormatTest
import qualified Adrai.P206GoldenTest
import qualified Adrai.ProjectionProperties
import qualified Adrai.QueryHistoryTest
import qualified Adrai.ReconciliationProperties
import qualified Adrai.RetrievalPlanGoldenTest
import qualified Adrai.RetrievalSqliteTest
import qualified Adrai.SearchRetrievalProperties
import qualified Adrai.SearchRetrievalTest
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
          ]
    ]
