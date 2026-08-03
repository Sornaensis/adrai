{-# LANGUAGE OverloadedStrings #-}

module Adrai.RelevanceGoldenTest (tests, writeP305Goldens) where

import Adrai.Format.Json
import Adrai.Query (relevantProjectionJson)
import Adrai.Relevance (relevanceChunkingFingerprint, relevanceScoringContract)
import Adrai.RelevanceIntegrationTest (conflictedRelevantProjection, outsideScopeRelevantProjection, resolvedRelevantProjection)
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "P3-05 relevance goldens"
    [ testCase "raw text relevance public JSON is byte exact" $ do
        expected <- ByteString.readFile goldenPath
        actual <- goldenBytes
        actual @?= expected
    ]

writeP305Goldens :: IO ()
writeP305Goldens = do
  createDirectoryIfMissing True "test/golden/p3-05"
  goldenBytes >>= ByteString.writeFile goldenPath

goldenPath :: FilePath
goldenPath = "test/golden/p3-05/relevance.golden"

goldenBytes :: IO ByteString
goldenBytes = do
  resolved <- resolvedRelevantProjection
  outsideScope <- outsideScopeRelevantProjection
  conflicted <- conflictedRelevantProjection
  pure . renderCanonicalJsonBytes $
    object
      [ ("schema", JsonString "adrai/p3-05-relevance-golden/v1"),
        ("chunking_fingerprint", JsonString relevanceChunkingFingerprint),
        ( "scoring",
          object
            [ (name, fromMaybe JsonNull (jsonNumberRounded6 value))
              | (name, value) <- Map.toAscList relevanceScoringContract
            ]
        ),
        ("resolved", relevantProjectionJson resolved),
        ("outside_scope", relevantProjectionJson outsideScope),
        ("conflicted", relevantProjectionJson conflicted)
      ]
