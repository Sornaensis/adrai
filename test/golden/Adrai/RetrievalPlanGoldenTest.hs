{-# LANGUAGE OverloadedStrings #-}

module Adrai.RetrievalPlanGoldenTest (tests, writeP302Goldens) where

import Adrai.Retrieval
import Adrai.Sqlite
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Numeric (showFFloat)
import System.Directory (createDirectoryIfMissing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "P3-02 Haskell-owned retrieval goldens"
    [ testCase "canonical query plans are frozen" (assertGolden "query-plans.golden" queryPlansBytes),
      testCase "six FTS channels are frozen" (assertGolden "fts-channel-contract.golden" ftsContractBytes)
    ]

assertGolden :: FilePath -> ByteString -> IO ()
assertGolden name actual = do
  expected <- BS.readFile (goldenRoot <> "/" <> name)
  actual @?= expected

queryPlansBytes :: ByteString
queryPlansBytes = TextEncoding.encodeUtf8 (Text.unlines (header <> renderPlan "cache cache identity" <> renderPlan "CacheKeyFactory"))
  where
    header =
      [ "fingerprint=" <> retrievalImplementationFingerprint,
        "payload_bytes=" <> Text.pack (show (BS.length retrievalFingerprintPayload))
      ]

renderPlan :: Text -> [Text]
renderPlan queryText =
  [ "query=" <> queryText,
    "profile=" <> queryProfileName (queryPlanProfile plan),
    "raw_tokens=" <> Text.intercalate "|" (queryPlanRawTokens plan),
    "semantic_terms=" <> Text.intercalate "|" (queryPlanSemanticTerms plan),
    "phrase=" <> queryPlanFtsExactPhrase plan,
    "exact=" <> Text.intercalate " AND " (queryPlanFtsExactTerms plan),
    "near=" <> queryPlanFtsNear plan,
    "prefix=" <> queryPlanFtsPrefix plan,
    "stemmed=" <> queryPlanFtsStemmed plan,
    "identifier=" <> queryPlanFtsIdentifier plan
  ]
  where
    plan = buildQueryPlan queryText []

ftsContractBytes :: ByteString
ftsContractBytes = TextEncoding.encodeUtf8 (Text.unlines (map renderTarget allFtsTargets))

renderTarget :: FtsTarget -> Text
renderTarget target =
  Text.intercalate
    "|"
    [ ftsTargetName target,
      ftsTargetTable target,
      Text.intercalate "," [name <> if isIndexed then ":I" else ":U" | (name, isIndexed) <- ftsTargetColumns target],
      ftsTargetTokenizer target,
      Text.intercalate "," (map (Text.pack . fixed) (ftsTargetBm25Weights target)),
      ftsTargetDdl target
    ]
  where
    fixed value = showFFloat Nothing value ""

-- | Explicit writer. Ordinary tests are strictly read-only.
writeP302Goldens :: IO ()
writeP302Goldens = do
  createDirectoryIfMissing True goldenRoot
  BS.writeFile (goldenRoot <> "/query-plans.golden") queryPlansBytes
  BS.writeFile (goldenRoot <> "/fts-channel-contract.golden") ftsContractBytes

goldenRoot :: FilePath
goldenRoot = "test/golden/p3-02"
