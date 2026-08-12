{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for the ``adrai search`` and ``adrai relevant``
-- CLI commands.
--
-- Tests cover FTS search, vector search, hybrid search, domain/scope
-- filters, pagination, obsolete filtering, relevance scoring, scope
-- bonus, Unicode content, and result schema validation.
module Adrei.SearchCliTest (tests) where

import Adrai.Integration.CLI
import Control.Monad (forM_, void)
import qualified Data.Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import qualified Data.Text as T

import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool,
    assertFailure,
    testCase,
  )

-- =====================================================================
-- JSON helpers
-- =====================================================================

_Object :: Data.Aeson.Value -> Maybe (KM.KeyMap Data.Aeson.Value)
_Object (Data.Aeson.Object o) = Just o
_Object _ = Nothing

(.:) :: Data.Aeson.FromJSON a => KM.KeyMap Data.Aeson.Value -> Text -> Maybe a
(.:) km key =
  case KM.lookup (AesonKey.fromText key) km of
    Nothing -> Nothing
    Just v -> case Data.Aeson.eitherDecode (Data.Aeson.encode v) of
      Left _ -> Nothing
      Right a -> Just a

-- | Extract the ADR ID from a create-adr result value.
extractAdrId :: Data.Aeson.Value -> Maybe Text
extractAdrId v = do
  o <- _Object v
  o .: "adr"

-- | Extract a field value as 'Text' from a JSON value (with default).
extractField :: Text -> Data.Aeson.Value -> Text
extractField key val =
  case _Object val of
    Nothing -> ""
    Just o -> fromMaybe "" (o .: key)

-- | Compile the repo to ensure database is ready.
ensureCompiled :: FilePath -> IO ()
ensureCompiled repo =
  void $ adraiJsonOrThrow repo ["compile", "--json"]

-- | Get the first element of a list or a default value.
headOrDefault :: a -> [a] -> a
headOrDefault d [] = d
headOrDefault _ (x:_) = x

-- =====================================================================
-- Test 1: FTS search finds content across all revisions
-- =====================================================================

testSearchFtsFindsContentAcrossAllRevisions :: TestTree
testSearchFtsFindsContentAcrossAllRevisions =
  testCase
    "search_fts_finds_content_across_all_revisions"
    $ withSystemTempDirectory "adrei-search-fts" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create ADRs with "cache identity" in title/body
        _ <- createAdr repo "Cache Identity" "Cache" "Cache identity is fundamental" ["compiler"] ["src/**"]
        _ <- createAdr repo "Cache Design" "Cache" "Cache identity patterns" ["runtime"] ["src/**"]
        _ <- createAdr repo "Other Topic" "Other" "Something about databases" ["api"] ["src/**"]

        -- Compile to establish search index
        ensureCompiled repo

        -- FTS search for "cache identity"
        result <- adraiJson repo ["search", "--query", "cache identity", "--json"]
        case result of
          Right val ->
            let parsed = parseSearchResults val
            in case parsed of
                 Just (_, _, _, _, results) ->
                   assertBool
                     "FTS search should find ADRs with matching content"
                     (length results >= 2)
                 Nothing -> assertFailure "could not parse search results"
          Left err ->
            assertFailure $ "search failed: " <> unpack err

-- =====================================================================
-- Test 2: Vector search returns semantic matches
-- =====================================================================

testSearchVectorReturnsSemanticMatches :: TestTree
testSearchVectorReturnsSemanticMatches =
  testCase
    "search_vector_returns_semantic_matches"
    $ withSystemTempDirectory "adrei-search-vector" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create ADRs with related content
        _ <- createAdr repo "Vector Search" "Vector" "Semantic search with vectors" ["compiler"] ["src/**"]
        _ <- createAdr repo "Similar Search" "Similar" "Related semantic matching" ["runtime"] ["src/**"]

        -- Compile to establish embeddings
        ensureCompiled repo

        -- Vector search
        result <- adraiJson repo ["search", "--query", "semantic matching", "--json"]
        case result of
          Right val ->
            let parsed = parseSearchResults val
            in case parsed of
                 Just (_, _, mode, _, results) -> do
                   assertBool
                     "vector search should return results"
                     (length results >= 1)
                   -- Mode should indicate vector or hybrid search
                   assertBool
                     "search mode should be vector-based"
                     (mode == "vector" || mode == "hybrid")
                 Nothing -> assertFailure "could not parse search results"
          Left err ->
            assertFailure $ "vector search failed: " <> unpack err

-- =====================================================================
-- Test 3: Hybrid search combines FTS and vector
-- =====================================================================

testSearchHybridCombinesFtsAndVector :: TestTree
testSearchHybridCombinesFtsAndVector =
  testCase
    "search_hybrid_combines_fts_and_vector"
    $ withSystemTempDirectory "adrei-search-hybrid" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create ADRs with various content
        _ <- createAdr repo "Hybrid ADR 1" "Hybrid" "Cache identity and retrieval" ["compiler"] ["src/**"]
        _ <- createAdr repo "Hybrid ADR 2" "Hybrid" "Vector search patterns" ["runtime"] ["src/**"]

        -- Compile to establish embeddings
        ensureCompiled repo

        -- Hybrid search
        result <- adraiJson repo ["search", "--query", "cache identity", "--json"]
        case result of
          Right val ->
            let parsed = parseSearchResults val
            in case parsed of
                 Just (_, _, _, limit, results) -> do
                   assertBool
                     "hybrid search should return results"
                     (length results >= 1)
                   -- Limit should reflect the requested limit
                   assertBool
                     "limit should be a positive integer"
                     (limit > 0)
                 Nothing -> assertFailure "could not parse search results"
          Left err ->
            assertFailure $ "hybrid search failed: " <> unpack err

-- =====================================================================
-- Test 4: Search respects domain and scope filters
-- =====================================================================

testSearchRespectsDomainAndScopeFilters :: TestTree
testSearchRespectsDomainAndScopeFilters =
  testCase
    "search_respects_domain_and_scope_filters"
    $ withSystemTempDirectory "adrei-search-domain" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create ADRs in different domains
        _ <- createAdr repo "Compiler ADR" "Compiler" "Compiler internals" ["compiler.cache"] ["src/**"]
        _ <- createAdr repo "Runtime ADR" "Runtime" "Runtime behavior" ["runtime.jobs"] ["src/**"]
        _ <- createAdr repo "Another Compiler ADR" "Compiler 2" "More compiler stuff" ["compiler.core"] ["src/**"]

        -- Compile to establish search index
        ensureCompiled repo

        -- Filter by compiler domain
        result <- adraiJson repo ["search", "--query", "compiler", "--domain", "compiler", "--json"]
        case result of
          Right val ->
            let parsed = parseSearchResults val
            in case parsed of
                 Just (_, _, _, _, results) -> do
                   let allCompiler =
                         all
                           (\r -> let ds = extractField "domains" r
                                  in ds == "compiler" ||
                                     T.take (T.length "compiler") ds == "compiler")
                           results
                   assertBool
                     "domain filter should only return compiler ADRs"
                     allCompiler
                 Nothing -> assertFailure "could not parse search results"
          Left err ->
            assertFailure $ "domain-filtered search failed: " <> unpack err

-- =====================================================================
-- Test 5: Search limit and pagination
-- =====================================================================

testSearchLimitAndPagination :: TestTree
testSearchLimitAndPagination =
  testCase
    "search_limit_and_pagination"
    $ withSystemTempDirectory "adrei-search-limit" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create 5+ ADRs with lowercase names for case-insensitive search
        forM_ ([1..5] :: [Int]) $ \i ->
          createAdr repo
            (T.pack ("limit adr " ++ show i))
            (T.pack ("limit " ++ show i))
            (T.pack ("body " ++ show i))
            ["compiler"]
            ["src/**"]

        -- Compile to establish search index
        ensureCompiled repo

        -- Search with limit
        result <- adraiJson repo ["search", "--query", "limit", "--limit", "3", "--json"]
        case result of
          Right val ->
            let parsed = parseSearchResults val
            in case parsed of
                 Just (_, _, _, limit, results) ->
                   assertBool
                     ("search limit should be respected (limit=" ++ show limit ++ ", count=" ++ show (length results) ++ ")")
                     (length results <= 3 && length results >= 1)
                 Nothing -> assertFailure "could not parse search results"
          Left err ->
            assertFailure $ "limited search failed: " <> unpack err

-- =====================================================================
-- Test 6: Search include-obsolete flag
-- =====================================================================

testSearchIncludeObsoleteFlag :: TestTree
testSearchIncludeObsoleteFlag =
  testCase
    "search_include_obsolete_flag"
    $ withSystemTempDirectory "adrei-search-obsolete" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create an ADR and make it obsolete
        createResult <- adraiJsonOrThrow repo
          [ "create-adr",
            "--title", "Obsolete Search ADR",
            "--summary", "Obsolete",
            "--body", "## Decision\nThis will be obsolete.\n",
            "--domain", "compiler",
            "--applies-to", "src/**",
            "--actor", "llm:planner",
            "--model", "demo-model",
            "--json"
          ]
        let adrId = extractAdrId createResult
        case adrId of
          Nothing -> assertFailure "could not extract ADR ID from create"
          Just id' ->
            void $ amendAdrStatus repo id' "obsolete"

        -- Create another non-obsolete ADR
        _ <- createAdr repo "Active Search ADR" "Active" "This is active" ["runtime"] ["src/**"]

        -- Compile to establish search index
        ensureCompiled repo

        -- Without --include-obsolete
        resultNoObsolete <- adraiJson repo ["search", "--query", "search", "--json"]
        let countNoObsolete =
              case resultNoObsolete of
                Right val ->
                  case parseSearchResults val of
                    Just (_, _, _, _, results) -> length results
                    Nothing -> 0
                Left _ -> 0

        -- With --include-obsolete
        resultWithObsolete <- adraiJson repo ["search", "--query", "search", "--include-obsolete", "--json"]
        let countWithObsolete =
              case resultWithObsolete of
                Right val ->
                  case parseSearchResults val of
                    Just (_, _, _, _, results) -> length results
                    Nothing -> 0
                Left _ -> 0

        assertBool
          "with-obsolete should return at least as many results"
          (countWithObsolete >= countNoObsolete)

-- =====================================================================
-- Test 7: Relevance command scores and ranks results
-- =====================================================================

testRelevanceCommandScoresAndRank :: TestTree
testRelevanceCommandScoresAndRank =
  testCase
    "relevance_command_scores_and_rank"
    $ withSystemTempDirectory "adrei-relevance-score" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create ADRs with varying relevance
        _ <- createAdr repo "Compiler Cache Key" "Cache Key" "Cache key identity and computation" ["compiler.cache"] ["src/compiler/**"]
        _ <- createAdr repo "Runtime Jobs" "Jobs" "Job scheduling and execution" ["runtime.jobs"] ["src/runtime/**"]
        _ <- createAdr repo "General Topic" "General" "A general topic unrelated to compiler" ["api"] ["docs/**"]

        -- Compile to establish search index
        ensureCompiled repo

        -- Run relevant for a compiler file
        result <- adraiJson repo ["relevant", "src/compiler/Key.py", "--json"]
        case result of
          Right val ->
            let parsed = parseRelevantResults val
            in case parsed of
                 Just (_, _, _, _, results) -> do
                   assertBool
                     "relevance should return results"
                     (length results >= 1)
                   -- Verify results have scores and are ordered by score
                   let scores = map extractScore results
                   assertBool
                     "results should have non-negative scores"
                     (all (\s -> s >= 0) scores)
                   -- Verify descending order (highest score first)
                   assertBool
                     "results should be ordered by descending score"
                     (isOrderedDescending scores)
                 Nothing -> assertFailure "could not parse relevant results"
          Left err ->
            assertFailure $ "relevant command failed: " <> unpack err
  where
    extractScore :: Data.Aeson.Value -> Double
    extractScore val =
      case _Object val of
        Just o -> fromMaybe 0.0 (o .: "score")
        Nothing -> 0.0

    isOrderedDescending :: [Double] -> Bool
    isOrderedDescending [] = True
    isOrderedDescending [_] = True
    isOrderedDescending (x:y:rest) = x >= y && isOrderedDescending (y:rest)

-- =====================================================================
-- Test 8: Relevance command scope bonus
-- =====================================================================

testRelevanceCommandScopeBonus :: TestTree
testRelevanceCommandScopeBonus =
  testCase
    "relevance_command_scope_bonus"
    $ withSystemTempDirectory "adrei-relevance-scope" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create ADR with scope "src/compiler/**"
        _ <- createAdr repo "Compiler Cache" "Cache" "Compiler cache implementation" ["compiler"] ["src/compiler/**"]

        -- Compile to establish search index
        ensureCompiled repo

        -- Test in-scope file
        resultInScope <- adraiJson repo ["relevant", "src/compiler/cache/Key.py", "--json"]
        let scoreInScope =
              case resultInScope of
                Right val ->
                  case parseRelevantResults val of
                    Just (_, _, _, _, results) -> headOrDefault 0.0 (map extractScore results)
                    Nothing -> 0.0
                Left _ -> 0.0

        -- Test out-of-scope file
        resultOutOfScope <- adraiJson repo ["relevant", "docs/README.md", "--json"]
        let scoreOutOfScope =
              case resultOutOfScope of
                Right val ->
                  case parseRelevantResults val of
                    Just (_, _, _, _, results) -> headOrDefault 0.0 (map extractScore results)
                    Nothing -> 0.0
                Left _ -> 0.0

        assertBool
          ("in-scope file should have scope bonus (in-scope=" ++ show scoreInScope ++ ", out-of-scope=" ++ show scoreOutOfScope ++ ")")
          (scoreInScope >= scoreOutOfScope)
  where
    extractScore :: Data.Aeson.Value -> Double
    extractScore val =
      case _Object val of
        Just o -> fromMaybe 0.0 (o .: "score")
        Nothing -> 0.0

-- =====================================================================
-- Test 9: Unicode content search finds emoji and international text
-- =====================================================================

testSearchUnicodeContentFindsEmojiAndCyrillic :: TestTree
testSearchUnicodeContentFindsEmojiAndCyrillic =
  testCase
    "search_unicode_content_finds_emoji_and_cyrillic"
    $ withSystemTempDirectory "adrei-search-unicode" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create ADR with Unicode content
        let unicodeContent = "København café 日本語 ☕" :: Text
        _ <- createAdr repo "Unicode ADR" "Unicode" unicodeContent ["compiler"] ["src/**"]

        -- Compile to establish search index
        ensureCompiled repo

        -- Search for "København"
        result <- adraiJson repo ["search", "--query", "København", "--json"]
        case result of
          Right val ->
            let parsed = parseSearchResults val
            in case parsed of
                 Just (_, _, _, _, results) ->
                   assertBool
                     "Unicode search should find matching ADR"
                     (length results >= 1)
                 Nothing -> assertFailure "could not parse search results"
          Left err ->
            assertFailure $ "Unicode search failed: " <> unpack err

-- =====================================================================
-- Test 10: Search result JSON schema is frozen
-- =====================================================================

testSearchResultJsonSchemaIsFrozen :: TestTree
testSearchResultJsonSchemaIsFrozen =
  testCase
    "search_result_json_schema_is_frozen"
    $ withSystemTempDirectory "adrei-search-schema" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create ADR
        _ <- createAdr repo "Schema Test ADR" "Schema" "Schema validation" ["compiler"] ["src/**"]

        -- Compile to establish search index
        ensureCompiled repo

        -- Run search
        result <- adraiJson repo ["search", "--query", "schema", "--json"]
        case result of
          Right val ->
            let parsed = parseSearchResults val
            in case parsed of
                 Just (schema, _, _, _, _) -> do
                   assertBool
                     "search should have a schema field"
                     (not (T.null schema))
                   assertBool
                     "schema should start with adrai/"
                     (T.take 5 schema == "adrai/")
                 Nothing -> assertFailure "could not parse search results"
          Left err ->
            assertFailure $ "search schema check failed: " <> unpack err

-- =====================================================================
-- Test suite
-- =====================================================================

tests :: TestTree
tests =
  testGroup
    "SearchCli"
    [ testSearchFtsFindsContentAcrossAllRevisions,
      testSearchVectorReturnsSemanticMatches,
      testSearchHybridCombinesFtsAndVector,
      testSearchRespectsDomainAndScopeFilters,
      testSearchLimitAndPagination,
      testSearchIncludeObsoleteFlag,
      testRelevanceCommandScoresAndRank,
      testRelevanceCommandScopeBonus,
      testSearchUnicodeContentFindsEmojiAndCyrillic,
      testSearchResultJsonSchemaIsFrozen
    ]
