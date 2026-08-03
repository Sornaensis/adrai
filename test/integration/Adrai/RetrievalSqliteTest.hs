{-# LANGUAGE OverloadedStrings #-}

module Adrai.RetrievalSqliteTest (tests) where

import Adrai.Retrieval (QueryPlan (..), buildQueryPlan)
import Adrai.Sqlite
import Control.Exception (bracket)
import Control.Monad (forM_)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple
  ( Connection,
    Only (Only),
    Query,
    SQLData (SQLText),
    close,
    execute,
    execute_,
    open,
    query,
    withTransaction,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "P3-02 SQLite retrieval"
    [ testCase "six target DDLs introspect to exact ordered columns and tokenizers" schemaContract,
      testCase "empty inputs short-circuit while parser and missing-index errors stay distinct and sanitized" errorContract,
      testCase "allowed filtering batching parameterization ranking and stemming are deterministic" rankedContract,
      testCase "prefix trigger excludes phrase and terms merge has no post-merge cap" channelContract
    ]

schemaContract :: IO ()
schemaContract = withMemory $ \connection -> do
  initializeFtsTargets connection >>= (@?= Right ())
  forM_ allFtsTargets $ \target -> do
    rows <- query connection (dynamicQuery ("PRAGMA table_info(" <> ftsTargetTable target <> ")")) () :: IO [(Int, Text, Text, Int, Maybe Text, Int)]
    map (\(_, name, _, _, _, _) -> name) rows @?= map fst (ftsTargetColumns target)
    definitions <- query connection "SELECT sql FROM sqlite_master WHERE type='table' AND name=?" (Only (ftsTargetTable target)) :: IO [Only Text]
    case definitions of
      [Only definition] -> assertBool "DDL records exact tokenizer" (("tokenize='" <> ftsTargetTokenizer target <> "'") `Text.isInfixOf` definition)
      _ -> assertBool "target has exactly one sqlite_master row" False

errorContract :: IO ()
errorContract = withMemory $ \connection -> do
  limit <- pure (mustRight (mkCandidateLimit 3))
  runFtsTarget connection SearchExactTarget "" (Set.singleton "missing") limit >>= (@?= Right [])
  runFtsTarget connection SearchExactTarget "needle" Set.empty limit >>= (@?= Right [])
  runFtsTarget connection SearchExactTarget "needle" (Set.singleton "missing") limit
    >>= (@?= Left (RetrievalIndexError SearchExactTarget))
  initializeFtsTargets connection >>= (@?= Right ())
  malformed <- runFtsTarget connection SearchExactTarget "\"unterminated" (Set.singleton "missing") limit
  malformed @?= Left (MalformedFtsQuery SearchExactTarget)
  qualifiedField <- runFtsTarget connection SearchExactTarget "name:needle" (Set.singleton "missing") limit
  qualifiedField @?= Left (MalformedFtsQuery SearchExactTarget)
  let message = show (retrievalSqlErrorToAdraiError (MalformedFtsQuery SearchExactTarget))
  assertBool "sanitized error excludes raw query and SQL" (not ("unterminated" `Text.isInfixOf` Text.pack message) && not ("SELECT" `Text.isInfixOf` Text.pack message))
  execute_ connection "CREATE TABLE sentinel(value TEXT NOT NULL)"
  execute connection "INSERT INTO sentinel(value) VALUES (?)" (Only ("safe" :: Text))
  let hostile = buildQueryPlan "what\" OR NEAR(cache cache, 99):foo-bar" []
      allowed = Set.singleton "missing"
  result <- runSummaryFtsChannels connection hostile allowed limit
  case result of
    Left failure -> assertBool ("generated hostile plan failed: " <> show failure) False
    Right _ -> pure ()
  sentinelRows <- query connection "SELECT value FROM sentinel" ()
  sentinelRows @?= [Only ("safe" :: Text)]

rankedContract :: IO ()
rankedContract = withMemory $ \connection -> do
  initializeFtsTargets connection >>= (@?= Right ())
  withTransaction connection $ do
    insertExact connection "decision" "" "" "cache identity" "" "" "" "" ""
    insertExact connection "context" "" "" "" "" "" "cache identity" "" ""
    insertExact connection "x') OR 1=1 --" "" "" "cache identity" "" "" "" "" ""
    insertExact connection "duplicate" "" "" "needle" "" "" "" "" ""
    insertExact connection "duplicate" "" "" "" "" "" "needle" "" ""
    insertExact connection "intermediate" "" "" "" "" "needle" "" "" ""
    forM_ [0 .. 700 :: Int] $ \ordinal ->
      insertExact connection (paddedId ordinal) "" "" "needle" "" "" "" "" ""
    insertStemmed connection "porter" "" "" "persist" "" "" ""
  let generous = mustRight (mkCandidateLimit 10)
  weighted <- runFtsTarget connection SearchExactTarget "\"cache identity\"" (Set.fromList ["decision", "context"]) generous
  map ftsHitItemId (mustRight weighted) @?= ["decision", "context"]
  injected <- runFtsTarget connection SearchExactTarget "\"cache\"" (Set.singleton "x') OR 1=1 --") generous
  map ftsHitItemId (mustRight injected) @?= ["x') OR 1=1 --"]
  batched <- runFtsTarget connection SearchExactTarget "\"needle\"" (Set.fromList [paddedId ordinal | ordinal <- [0 .. 700 :: Int]]) (mustRight (mkCandidateLimit 1))
  map ftsHitItemId (mustRight batched) @?= [paddedId 700]
  duplicateMerged <- runFtsTarget connection SearchExactTarget "\"needle\"" (Set.fromList ["duplicate", "intermediate"]) generous
  map ftsHitItemId (mustRight duplicateMerged) @?= ["duplicate", "intermediate"]
  porter <- runFtsTarget connection SearchStemmedTarget "\"persistence\"" (Set.singleton "porter") generous
  map ftsHitItemId (mustRight porter) @?= ["porter"]

channelContract :: IO ()
channelContract = withMemory $ \connection -> do
  initializeFtsTargets connection >>= (@?= Right ())
  let alphaIds = ["alpha-" <> Text.pack (show ordinal) | ordinal <- [0 .. 18 :: Int]]
      betaIds = ["beta-" <> Text.pack (show ordinal) | ordinal <- [0 .. 239 :: Int]]
      phraseOnlyIds = ["phrase-" <> Text.pack (show ordinal) | ordinal <- [0 .. 19 :: Int]]
  withTransaction connection $ do
    forM_ alphaIds $ \itemId -> insertExact connection itemId "" "" "alpha common phrase" "" "" "" "" ""
    forM_ betaIds $ \itemId -> insertExact connection itemId "" "" "beta" "" "" "" "" ""
    forM_ phraseOnlyIds $ \itemId -> insertExact connection itemId "" "" "common phrase" "" "" "" "" ""
  let base = buildQueryPlan "unused" []
      plan =
        base
          { queryPlanFtsExactPhrase = "\"common phrase\"",
            queryPlanFtsExactTerms = ["\"alpha\""],
            queryPlanFtsNear = "",
            queryPlanFtsPrefix = "\"beta\"*",
            queryPlanFtsStemmed = "",
            queryPlanFtsIdentifier = ""
          }
      allowed = Set.fromList (alphaIds <> betaIds <> phraseOnlyIds)
  result <- runSummaryFtsChannels connection plan allowed (mustRight (mkCandidateLimit 1))
  let channels = mustRight result
  summaryFtsPrefixUsed channels @?= True
  length (summaryFtsPhrase channels) @?= 39
  length (summaryFtsTerms channels) @?= 259

insertExact :: Connection -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> IO ()
insertExact connection itemId title summary decision domains rationale context consequences identifiers =
  execute
    connection
    "INSERT INTO fts_search_exact(item_id,adr_id,candidate_record_id,title,summary,decision,domains,rationale,context,consequences,identifiers) VALUES (?,?,?,?,?,?,?,?,?,?,?)"
    (map SQLText [itemId, itemId, itemId, title, summary, decision, domains, rationale, context, consequences, identifiers])

insertStemmed :: Connection -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> IO ()
insertStemmed connection itemId title summary decision rationale context consequences =
  execute
    connection
    "INSERT INTO fts_search_stemmed(item_id,adr_id,candidate_record_id,title,summary,decision,rationale,context,consequences) VALUES (?,?,?,?,?,?,?,?,?)"
    (map SQLText [itemId, itemId, itemId, title, summary, decision, rationale, context, consequences])

paddedId :: Int -> Text
paddedId ordinal = "item-" <> Text.pack (replicate (3 - length digits) '0' <> digits)
  where
    digits = show ordinal

dynamicQuery :: Text -> Query
dynamicQuery = fromString . Text.unpack

withMemory :: (Connection -> IO value) -> IO value
withMemory = bracket (open ":memory:") close

mustRight :: (Show failure) => Either failure value -> value
mustRight (Right value) = value
mustRight (Left failure) = error (show failure)
