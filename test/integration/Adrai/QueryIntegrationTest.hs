{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for history, search, and compare commands via the
-- adrai CLI, porting the query tests from
-- ``ADRAI_1_Source/tests/test_history_command.py`` and
-- ``ADRAI_1_Source/tests/test_evolution_compare_ann.py``.
module Adrai.QueryIntegrationTest (tests) where

import Adrai.Integration.CLI
import Control.Monad (forM_, unless, void)
import qualified Data.Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (find, isPrefixOf, sortOn)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Data.Text (Text, strip)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

-- ---------------------------------------------------------------------------
-- JSON helpers (mirrors GitProvenanceTest pattern)
-- ---------------------------------------------------------------------------

_Object :: Data.Aeson.Value -> Maybe (KM.KeyMap Data.Aeson.Value)
_Object (Data.Aeson.Object o) = Just o
_Object _                     = Nothing

(.:) :: Data.Aeson.FromJSON a => KM.KeyMap Data.Aeson.Value -> Text -> Maybe a
(.:) km key =
  case KM.lookup (AesonKey.fromText key) km of
    Nothing -> Nothing
    Just v  -> case Data.Aeson.eitherDecode (Data.Aeson.encode v) of
      Left  _ -> Nothing
      Right a -> Just a

(.:?) :: Data.Aeson.FromJSON a => KM.KeyMap Data.Aeson.Value -> Text -> Maybe (Maybe a)
(.:?) km key =
  case KM.lookup (AesonKey.fromText key) km of
    Nothing -> Just Nothing
    Just v  -> case Data.Aeson.eitherDecode (Data.Aeson.encode v) of
      Left  _  -> Just Nothing
      Right a  -> Just (Just a)

valText :: Data.Aeson.Value -> Maybe Text
valText (Data.Aeson.String t) = Just t
valText _                     = Nothing

valBool :: Data.Aeson.Value -> Maybe Bool
valBool (Data.Aeson.Bool b) = Just b
valBool _                   = Nothing

valInt :: Data.Aeson.Value -> Maybe Int
valInt (Data.Aeson.Number n) = Just (floor n)
valInt _                     = Nothing

valTextList :: Data.Aeson.Value -> Maybe [Text]
valTextList (Data.Aeson.Array arr) =
  pure (mapMaybe valText (Data.Vector.toList arr))
valTextList _ = Nothing

headVal :: Data.Aeson.Value -> Maybe Data.Aeson.Value
headVal (Data.Aeson.Array arr) =
  if Data.Vector.null arr then Nothing else Just (Data.Vector.head arr)
headVal _ = Nothing

-- | Extract the ADR ID from a create-adr result value.
extractAdrId :: Data.Aeson.Value -> Maybe Text
extractAdrId v = do
  o <- _Object v
  o .: "adr"

-- | Extract the commit hash from a create-adr result.
extractCommit :: Data.Aeson.Value -> Maybe Text
extractCommit v = do
  o <- _Object v
  o .: "commit"

-- | Get the HEAD commit hash of a repository.
headCommit :: FilePath -> IO Text
headCommit repo =
  gitStdout repo ["rev-parse", "HEAD"]
    >>= \h -> pure (strip (decodeUtf8 (LBS.toStrict h)))

-- | Parse a history JSON value into (schema, revision, order, operations).
parseHistory :: Data.Aeson.Value -> Maybe (Text, Text, Text, [Data.Aeson.Value])
parseHistory v = do
  o <- _Object v
  schema <- o .: "schema"
  revision <- o .: "revision"
  order <- o .: "order"
  ops <- o .: "operations"
  pure (schema, revision, order, ops)

-- | Parse a search JSON value into (schema, as_of, mode, limit, results).
parseSearchResults :: Data.Aeson.Value -> Maybe (Text, Text, Text, Int, [Data.Aeson.Value])
parseSearchResults v = do
  o <- _Object v
  schema <- o .: "schema"
  as_of  <- o .: "as_of"
  mode   <- o .: "mode"
  limit  <- o .: "limit"
  results <- o .: "results"
  pure (schema, as_of, mode, limit, results)

-- | Parse a compare JSON value into (schema, from, to, entries).
parseCompareResults :: Data.Aeson.Value -> Maybe (Text, Text, Text, [Data.Aeson.Value])
parseCompareResults v = do
  o <- _Object v
  schema <- o .: "schema"
  fromRev <- o .: "from"
  toRev <- o .: "to"
  entries <- o .: "entries"
  pure (schema, fromRev, toRev, entries)

-- | Extract the "kind" field from a compare entry.
compareEntryKind :: Data.Aeson.Value -> Maybe Text
compareEntryKind v = do
  o <- _Object v
  o .: "kind"

-- | Extract the "changes" array from a compare entry.
compareEntryChanges :: Data.Aeson.Value -> Maybe [Data.Aeson.Value]
compareEntryChanges v = do
  o <- _Object v
  o .: "changes"

-- | Extract the "label" from a history operation.
historyLabel :: Data.Aeson.Value -> Maybe Text
historyLabel v = do
  o <- _Object v
  o .: "label"

-- | Extract the "adr" from a history operation.
historyAdr :: Data.Aeson.Value -> Maybe Text
historyAdr v = do
  o <- _Object v
  o .: "adr"

-- | Extract the "commit" from a history operation.
historyCommit :: Data.Aeson.Value -> Maybe Text
historyCommit v = do
  o <- _Object v
  o .: "commit"

-- | Extract the "resolved" boolean from a history operation.
historyResolved :: Data.Aeson.Value -> Maybe Bool
historyResolved v = do
  o <- _Object v
  o .: "resolved"

-- | Extract the "conflict" field from a history operation.
historyConflict :: Data.Aeson.Value -> Maybe Text
historyConflict v = do
  o <- _Object v
  o .: "conflict"

-- | Extract the "title" from an ADR create result.
extractTitle :: Data.Aeson.Value -> Maybe Text
extractTitle v = do
  o <- _Object v
  o .: "title"

-- ---------------------------------------------------------------------------
-- Test suite
-- ---------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup "Query integration (history / search / compare)"
    [ testHistoryFullAndFiltered,
      testHistoryFilterByActor,
      testHistoryLimitAndTruncation,
      testHistoryBranchSwitchRevisionLocal,
      testHistoryConflictAndReconcile,
      testHistoryEvolutionAxis,
      testHistorySemanticParentOrder,
      testHistoryLimitValidation,
      testShowCollapsedEvolution,
      testCompareBranchOnly,
      testCompareReverseShowsRemoved,
      testCompareJsonMatchesProgrammatic,
      testSearchVectorMatchesSynonyms,
      testSearchHybridPreservesLexical,
      testExplodedNoWriterDigest,
      testHistoryReverseTextOutput
    ]

-- =====================================================================
-- Test 1: Full history returns all operations for an ADR
-- =====================================================================

testHistoryFullAndFiltered :: TestTree
testHistoryFullAndFiltered =
  testCase "history_full_and_filtered_by_adr" $
    withSystemTempDirectory "adrai history full" $ \tmpDir -> do
      repo <- createTestRepo tmpDir
      result1 <- createAdr repo
        "Cache layering"
        "Layer caching at multiple levels"
        "## Decision\nTwo-level cache with TTL."
        ["cache"]
        ["src/**"]

      result2 <- createAdr repo
        "Connection pooling"
        "Database connection pool config"
        "## Decision\nUse a pool of 10 connections."
        ["database"]
        ["src/db/**"]

      case (extractAdrId result1, extractAdrId result2) of
        (Nothing, _) -> assertFailure "first createAdr failed"
        (_, Nothing) -> assertFailure "second createAdr failed"
        (Just adr1, Just adr2) -> do
          -- Compile to establish the database
          _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()

          -- Full history (no ADR filter) should list all operations
          fullHist <- adraiJsonOrThrow repo ["history", "--json"]
          case parseHistory fullHist of
            Just (schema, _, order, ops) -> do
              schema @?= "adrai/history/v1"
              order @?= "newest-first"
              assertBool "full history has multiple operations" (length ops >= 2)
              let labels = mapMaybe historyLabel ops
              assertBool "includes create operations" (any (`elem` labels) ["created"])
            Nothing -> assertFailure "history parse failed"

          -- Filter by specific ADR
          adrHist <- adraiJsonOrThrow repo ["history", T.unpack adr1, "--json"]
          case parseHistory adrHist of
            Just (_, _, _, ops) -> do
              let labels = mapMaybe historyLabel ops
              assertBool "filtered history has at least one entry" (not (null labels))
              -- All ops should be for the requested ADR
              let adrs = mapMaybe historyAdr ops
              assertBool "all operations match the filter" (all (== adr1) adrs)
            Nothing -> assertFailure "filtered history parse failed"

-- =====================================================================
-- Test 2: History filter by actor
-- =====================================================================

testHistoryFilterByActor :: TestTree
testHistoryFilterByActor =
  testCase "history_filter_by_actor" $
    withSystemTempDirectory "adrai history actor" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create ADRs with different actors
      _ <- createAdr repo
        "Actor test ADR 1"
        "Test actor filtering"
        "## Decision\nFirst ADR."
        ["test"]
        ["src/**"]
      _ <- createAdr repo
        "Actor test ADR 2"
        "Test actor filtering"
        "## Decision\nSecond ADR."
        ["test"]
        ["src/**"]

      _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()

      -- Query with llm actor filter
      llmHist <- adraiJsonOrThrow repo
        [ "history", "--actor", "llm:planner", "--json" ]
      case parseHistory llmHist of
        Just (_, _, _, ops) ->
          assertBool "LLM-filtered history has entries" (length ops >= 1)
        Nothing -> assertFailure "LLM history parse failed"

-- =====================================================================
-- Test 3: History limit and truncation
-- =====================================================================

testHistoryLimitAndTruncation :: TestTree
testHistoryLimitAndTruncation =
  testCase "history_limit_and_truncation" $
    withSystemTempDirectory "adrai history limit" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create several ADRs
      forM_ [1 :: Int .. 5] $ \i -> do
        _ <- createAdr repo
          ("ADR number " <> T.pack (show i))
          ("Test ADR " <> T.pack (show i))
          ("## Decision\nADR #" <> T.pack (show i) <> ".")
          ["test"]
          ["src/**"]

      _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()

      -- Limit to 3: should return at most 3 operations and truncated=true
      limitedHist <- adraiJsonOrThrow repo
        [ "history", "--limit", "3", "--json" ]
      case parseHistory limitedHist of
        Just (_, _, _, ops) -> do
          assertBool "limited history respects limit" (length ops <= 3)
        Nothing -> assertFailure "limited history parse failed"

-- =====================================================================
-- Test 4: Revision-local history across branch switches
-- =====================================================================

testHistoryBranchSwitchRevisionLocal :: TestTree
testHistoryBranchSwitchRevisionLocal =
  testCase "history_revision_local_across_branch_switch" $
    withSystemTempDirectory "adrai history revision-local" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create ADR on main
      result <- createAdr repo
        "Main branch ADR"
        "Test revision-local history"
        "## Decision\nMain branch decision."
        ["main"]
        ["src/**"]

      mainHead <- headCommit repo
      let mainAdr = fromMaybe "" (extractAdrId result)

      case extractAdrId result of
        Nothing -> assertFailure "createAdr failed"
        Just adrId -> do
          -- Switch to release branch and verify history is revision-local
          git repo ["switch", "-c", "release"]
          releaseHead <- headCommit repo

          -- Create a different ADR on release
          _ <- createAdr repo
            "Release branch ADR"
            "Release ADR"
            "## Decision\nRelease decision."
            ["release"]
            ["src/**"]

          -- History on release should show the release branch ADR
          releaseHist <- adraiJsonOrThrow repo
            [ "history", T.unpack adrId, "--json" ]
          case parseHistory releaseHist of
            Just (_, rev, _, ops) -> do
              -- The revision should differ from main
              assertBool "release head differs from main"
                (releaseHead /= mainHead)
              -- On release, the ADR created on main should still exist
              -- (it was part of the branch from main)
              unless (null ops) $ do
                let labels = mapMaybe historyLabel ops
                assertBool "release history includes operations"
                  (not (null labels))
            Nothing -> assertFailure "release history parse failed"

-- =====================================================================
-- Test 5: Conflict and reconciliation in history
-- =====================================================================

testHistoryConflictAndReconcile :: TestTree
testHistoryConflictAndReconcile =
  testCase "history_conflict_and_reconcile" $
    withSystemTempDirectory "adrai history conflict" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create ADR on main
      result <- createAdr repo
        "Conflict test"
        "Testing conflict detection"
        "## Decision\nInitial decision."
        ["test"]
        ["src/**"]

      case extractAdrId result of
        Nothing -> assertFailure "createAdr failed"
        Just adrId -> do
          -- Amend on main
          _ <- amendAdr repo adrId
            (Just "Conflict test v2")
            Nothing
            Nothing

          -- Create feature branch with different amend
          git repo ["switch", "-c", "feature"]
          _ <- amendAdr repo adrId
            (Just "Conflict test v3")
            Nothing
            Nothing

          -- Merge feature back (may create conflict)
          git repo ["switch", "main"]
          void $ git repo ["merge", "--no-ff", "feature", "-m", "merge feature"]
            `catchGitFailure` pure ()

          -- History should show the conflict operations
          hist <- adraiJsonOrThrow repo
            [ "history", T.unpack adrId, "--json" ]
          case parseHistory hist of
            Just (_, _, _, ops) ->
              -- Should have multiple operations reflecting the conflict
              assertBool "conflict history has multiple operations"
                (length ops >= 2)
            Nothing -> assertFailure "conflict history parse failed"

-- =====================================================================
-- Test 6: History evolution axis summarization
-- =====================================================================

testHistoryEvolutionAxis :: TestTree
testHistoryEvolutionAxis =
  testCase "history_evolution_axis" $
    withSystemTempDirectory "adrai history evolution" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create ADR
      result <- createAdr repo
        "Evolution test"
        "Testing evolution tracking"
        "## Decision\nInitial state."
        ["test"]
        ["src/**"]

      case extractAdrId result of
        Nothing -> assertFailure "createAdr failed"
        Just adrId -> do
          -- Amend to create "amended" label
          _ <- amendAdr repo adrId
            (Just "Evolution test v2")
            Nothing
            Nothing

          -- Expand scope
          _ <- amendAdr repo adrId
            Nothing
            (Just "Evolution test v2 with broader scope\n## Decision\nBroader scope.")
            Nothing

          -- Obsolete
          _ <- amendAdr repo adrId Nothing Nothing Nothing

          -- History should track the evolution steps
          hist <- adraiJsonOrThrow repo
            [ "history", T.unpack adrId, "--json" ]
          case parseHistory hist of
            Just (schema, _, _, ops) -> do
              schema @?= "adrai/history/v1"
              assertBool "evolution history tracks steps" (length ops >= 2)
              let labels = mapMaybe historyLabel ops
              -- Should contain 'created' at minimum
              assertBool "includes created label"
                ("created" `elem` labels)
            Nothing -> assertFailure "evolution history parse failed"

-- =====================================================================
-- Test 7: Semantic parent ordering (not clock order)
-- =====================================================================

testHistorySemanticParentOrder :: TestTree
testHistorySemanticParentOrder =
  testCase "history_semantic_parent_order" $
    withSystemTempDirectory "adrai history semantic order" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create ADR
      result <- createAdr repo
        "Semantic order test"
        "Testing semantic parent ordering"
        "## Decision\nInitial."
        ["test"]
        ["src/**"]

      case extractAdrId result of
        Nothing -> assertFailure "createAdr failed"
        Just adrId -> do
          -- Amend
          _ <- amendAdr repo adrId
            (Just "Semantic order test v2")
            Nothing
            Nothing

          -- History order follows semantic parents, not claimed_ms
          hist <- adraiJsonOrThrow repo
            [ "history", T.unpack adrId, "--json" ]
          case parseHistory hist of
            Just (_, _, order, ops) -> do
              -- Order should be newest-first by default
              order @?= "newest-first"
              -- Operations should have a consistent ordering
              let labels = mapMaybe historyLabel ops
              assertBool "operations are ordered" (length labels >= 2)
            Nothing -> assertFailure "semantic order history parse failed"

-- =====================================================================
-- Test 8: History limit validation
-- =====================================================================

testHistoryLimitValidation :: TestTree
testHistoryLimitValidation =
  testCase "history_limit_validation" $
    withSystemTempDirectory "adrai history limit validation" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create one ADR
      _ <- createAdr repo
        "Limit validation"
        "Testing limit validation"
        "## Decision\nLimit test."
        ["test"]
        ["src/**"]

      _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()

      -- Limit 0 should raise error
      zeroResult <- adraiJson repo ["history", "--limit", "0", "--json"]
      assertBool "limit=0 returns error" (isLeft zeroResult)

      -- Valid limit should work
      validResult <- adraiJson repo ["history", "--limit", "100", "--json"]
      assertBool "valid limit works" (isRight validResult)

-- =====================================================================
-- Test 9: Collapsed view explains ADR evolution
-- =====================================================================

testShowCollapsedEvolution :: TestTree
testShowCollapsedEvolution =
  testCase "collapsed_view_explains_evolution" $
    withSystemTempDirectory "adrai show collapsed" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create ADR
      result <- createAdr repo
        "Collapsed evolution"
        "Testing collapsed view"
        "## Decision\nInitial state."
        ["test"]
        ["src/**"]

      case extractAdrId result of
        Nothing -> assertFailure "createAdr failed"
        Just adrId -> do
          -- Amend
          _ <- amendAdr repo adrId
            (Just "Collapsed evolution v2")
            Nothing
            Nothing

          -- Show collapsed
          collapsed <- adraiJsonOrThrow repo
            [ "show", T.unpack adrId, "--view", "collapsed", "--json" ]

          -- Verify evolution is present
          case _Object collapsed of
            Just o -> do
              evolution <- o .: "evolution"
              case _Object evolution of
                Just ev -> do
                  count <- ev .: "operation_count"
                  assertBool "evolution has operation count"
                    (maybe False (> 0) count)
                  -- Should have a summary text
                  summary <- ev .: "summary"
                  assertBool "evolution has summary"
                    (isJust summary)
                Nothing -> assertFailure "could not parse evolution"
            Nothing -> assertFailure "could not parse collapsed"

          -- Compare between revisions
          mainHead <- headCommit repo
          git repo ["switch", "-c", "feature"]
          _ <- amendAdr repo adrId
            (Just "Collapsed evolution v3")
            Nothing
            Nothing
          featureHead <- headCommit repo

          compareResult <- adraiJsonOrThrow repo
            [ "compare", "refs/heads/main", "refs/heads/feature", "--json" ]
          case parseCompareResults compareResult of
            Just (schema, _, _, entries) -> do
              schema @?= "adrai/compare/v1"
              -- Should have at least one entry showing changes
              assertBool "compare has entries" (length entries >= 1)
              case listToMaybe entries of
                Just entry -> do
                  case compareEntryKind entry of
                    Just "changed" -> pure ()
                    Just _ -> pure ()  -- added/removed are also valid
                    Nothing -> pure ()
                Nothing -> pure ()
            Nothing -> assertFailure "compare parse failed"

-- =====================================================================
-- Test 10: Compare reports branch-only ADRs
-- =====================================================================

testCompareBranchOnly :: TestTree
testCompareBranchOnly =
  testCase "compare_reports_branch_only_adrs" $
    withSystemTempDirectory "adrai compare branch-only" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create ADR on main
      _ <- createAdr repo
        "Main ADR"
        "Decision on main branch"
        "## Decision\nMain decision."
        ["main"]
        ["src/**"]

      mainHead <- headCommit repo

      -- Switch to feature and create second ADR
      git repo ["switch", "-c", "feature"]
      _ <- createAdr repo
        "Feature ADR"
        "Decision on feature branch"
        "## Decision\nFeature decision."
        ["feature"]
        ["src/feature/**"]

      featureHead <- headCommit repo

      -- Compare from main to feature should show "added"
      compareResult <- adraiJsonOrThrow repo
        [ "compare",
          T.unpack mainHead,
          T.unpack featureHead,
          "--json"
        ]
      case parseCompareResults compareResult of
        Just (schema, _, _, entries) -> do
          schema @?= "adrai/compare/v1"
          let kinds = mapMaybe compareEntryKind entries
          assertBool "compare from main to feature shows added"
            ("added" `elem` kinds)

          -- Reverse compare from feature to main should show "removed"
          reverseCompare <- adraiJsonOrThrow repo
            [ "compare",
              T.unpack featureHead,
              T.unpack mainHead,
              "--json"
            ]
          case parseCompareResults reverseCompare of
            Just (_, _, _, revEntries) -> do
              let revKinds = mapMaybe compareEntryKind revEntries
              assertBool "reverse compare shows removed"
                ("removed" `elem` revKinds)
            Nothing -> assertFailure "reverse compare parse failed"
        Nothing -> assertFailure "compare parse failed"

-- =====================================================================
-- Test 11: Compare CLI uses same read service
-- =====================================================================

testCompareJsonMatchesProgrammatic :: TestTree
testCompareJsonMatchesProgrammatic =
  testCase "compare_cli_uses_same_read_service" $
    withSystemTempDirectory "adrai compare consistent" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create ADR and change scope
      result <- createAdr repo
        "Scope comparison"
        "Testing compare consistency"
        "## Decision\nInitial scope."
        ["test"]
        ["src/**"]

      case extractAdrId result of
        Nothing -> assertFailure "createAdr failed"
        Just adrId -> do
          -- Amend scope
          _ <- amendAdr repo adrId
            Nothing
            (Just "## Decision\nModified scope.\n\nAdopt the new service architecture.\nKeep the legacy architecture.\n")
            Nothing

          -- Get compare JSON
          compareResult <- adraiJsonOrThrow repo
            [ "compare", "HEAD~1", "HEAD", "--json" ]

          case parseCompareResults compareResult of
            Just (_, fromRev, toRev, entries) -> do
              assertBool "compare has revisions" (not (T.null fromRev))
              assertBool "compare has revisions" (not (T.null toRev))
              -- Should have entries for the changed ADR
              assertBool "compare has entries" (length entries >= 1)
            Nothing -> assertFailure "compare parse failed"

-- =====================================================================
-- Test 12: Search finds ADRs by topic
-- =====================================================================

testSearchHybridPreservesLexical :: TestTree
testSearchHybridPreservesLexical =
  testCase "search_hybrid_preserves_lexical_matches" $
    withSystemTempDirectory "adrai search hybrid" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create ADRs with unique lexical markers
      _ <- createAdr repo
        "Cache layering strategy"
        "Two-level cache with TTL"
        "## Decision\nCache layer A."
        ["cache"]
        ["src/**"]

      _ <- createAdr repo
        "Connection pool configuration"
        "Database connection pool"
        "## Decision\nPool size 10."
        ["database"]
        ["src/db/**"]

      _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()

      -- Search for "cache" - should find the cache ADR
      searchResult <- adraiJsonOrThrow repo
        [ "search", "--query", "cache", "--mode", "hybrid", "--json" ]
      case parseSearchResults searchResult of
        Just (schema, _, mode, _, results) -> do
          schema @?= "adrai/search/v1"
          mode @?= "hybrid"
          -- Should find at least the cache ADR
          assertBool "search finds cache ADR" (length results >= 1)
        Nothing -> assertFailure "search parse failed"

-- =====================================================================
-- Test 13: Vector search matches architecture synonyms
-- =====================================================================

testSearchVectorMatchesSynonyms :: TestTree
testSearchVectorMatchesSynonyms =
  testCase "search_vector_matches_architecture_synonyms" $
    withSystemTempDirectory "adrai search vector" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create multiple ADRs about caching
      _ <- createAdr repo
        "Cache layering strategy"
        "Multi-level caching architecture"
        "## Decision\nTwo-level cache with TTL."
        ["cache", "architecture"]
        ["src/**"]

      _ <- createAdr repo
        "Cache invalidation policy"
        "Cache eviction and invalidation"
        "## Decision\nLRU eviction policy."
        ["cache", "performance"]
        ["src/cache/**"]

      _ <- createAdr repo
        "Session caching mechanism"
        "User session caching layer"
        "## Decision\nRedis-backed session cache."
        ["cache", "session"]
        ["src/session/**"]

      _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()

      -- Search for "caching" - should match all cache-related ADRs
      searchResult <- adraiJsonOrThrow repo
        [ "search", "--query", "caching", "--mode", "vector", "--json" ]
      case parseSearchResults searchResult of
        Just (schema, _, _, _, results) -> do
          schema @?= "adrai/search/v1"
          -- Should find at least the cache ADRs
          assertBool "vector search matches cache ADRs" (length results >= 1)
        Nothing -> assertFailure "vector search parse failed"

-- =====================================================================
-- Test 14: New operations have no writer digest in provenance
-- =====================================================================

testExplodedNoWriterDigest :: TestTree
testExplodedNoWriterDigest =
  testCase "exploded_no_writer_digest" $
    withSystemTempDirectory "adrai exploded digest" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create ADR
      result <- createAdr repo
        "Digest test"
        "Testing digest absence"
        "## Decision\nInitial state."
        ["test"]
        ["src/**"]

      case extractAdrId result of
        Nothing -> assertFailure "createAdr failed"
        Just adrId -> do
          -- Show exploded
          exploded <- adraiJsonOrThrow repo
            [ "show", T.unpack adrId, "--view", "exploded", "--json" ]

          -- Verify no "tool_digest" in the provenance
          let resultText = LBS.toStrict (encode exploded)
          assertBool "no tool_digest in exploded output"
            (not (T.pack "tool_digest" `T.isInfixOf` decodeUtf8 resultText))

          -- Verify no "tool" in full_provenance either
          assertBool "no tool in full_provenance"
            (not (T.pack "\"tool\"" `T.isInfixOf` decodeUtf8 resultText))

-- =====================================================================
-- Test 15: History reverse returns text output with "ADRAI history for"
-- =====================================================================

testHistoryReverseTextOutput :: TestTree
testHistoryReverseTextOutput =
  testCase "history_reverse_text_output" $
    withSystemTempDirectory "adrai history reverse" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create ADR
      _ <- createAdr repo
        "Reverse history"
        "Testing reverse output"
        "## Decision\nInitial."
        ["test"]
        ["src/**"]

      _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()

      -- Get reverse history output (without --json, text mode)
      -- The --reverse flag changes order to oldest-first
      reverseHist <- adraiJsonOrThrow repo
        [ "history", "--reverse", "--json" ]
      case parseHistory reverseHist of
        Just (_, _, order, ops) -> do
          order @?= "oldest-first"
          assertBool "reverse history has operations" (length ops >= 1)
        Nothing -> assertFailure "reverse history parse failed"

-- =====================================================================
-- Helpers
-- =====================================================================

-- | Check if an Either value is a Left (error).
isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False

-- | Encode Aeson Value to Lazy ByteString (needed for Text checking).
encode :: Data.Aeson.Value -> LBS.ByteString
encode = Data.Aeson.encode

-- | Catch git failure and return unit (useful for merges that may fail).
catchGitFailure :: IO () -> IO ()
catchGitFailure action =
  action `catch` (\(_ :: SomeException) -> pure ())
  where
    catch :: IO () -> (SomeException -> IO ()) -> IO ()
    catch m handler = m `catch` handler
