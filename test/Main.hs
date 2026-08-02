{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Adrai.ConfigFormatTest
import qualified Adrai.CoverageLedgerTest
import qualified Adrai.DomainFormatTest
import qualified Adrai.FixtureContractTest
import qualified Adrai.FixturePlanTest
import qualified Adrai.FixturePrngTest
import qualified Adrai.FixtureProperties
import qualified Adrai.FixtureRelevanceTest
import qualified Adrai.FormatFoundationTest
import qualified Adrai.GoldenFixturesTest
import qualified Adrai.GraphTest
import qualified Adrai.IdentityTest
import qualified Adrai.IntegrityTest
import qualified Adrai.ManagedDocumentFormatTest
import qualified Adrai.ManagedPathContractTest
import qualified Adrai.MarkdownTest
import qualified Adrai.ProvenanceFormatTest
import qualified Adrai.SemanticIdentityTest
import qualified Adrai.ScopeFormatTest
import qualified Adrai.StateTest
import qualified Adrai.TomlCanonicalTest
import qualified Adrai.TypesProperties
import qualified Adrai.TypesTest
import Control.Exception (bracket)
import Database.SQLite.Simple (close, execute_, open)
import Hedgehog (property, success)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit (testCase)

main :: IO ()
main = defaultMain tests

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
      Adrai.IdentityTest.tests,
      Adrai.StateTest.tests,
      Adrai.ManagedPathContractTest.tests,
      Adrai.MarkdownTest.tests,
      Adrai.SemanticIdentityTest.tests,
      Adrai.IntegrityTest.tests
    ]
