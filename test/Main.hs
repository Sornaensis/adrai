{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Adrai.FormatFoundationTest
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
      Adrai.TypesProperties.tests
    ]
