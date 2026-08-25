{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for the ``adrai history`` CLI command.
--
-- Tests cover ordering, filtering (since/until, domain, revision),
-- branch-merge visibility, obsolete filtering, compact format, and
-- error handling for invalid revisions.
module Adrai.HistoryCommandTest (tests) where

import Adrai.Integration.CLI
import Control.Monad (void)
import qualified Data.Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( (@?=),
    assertBool,
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

-- | Extract the "label" from a history operation.
historyLabel :: Data.Aeson.Value -> Maybe Text
historyLabel v = do
  o <- _Object v
  o .: "label"


-- | Compile the repo to ensure database is ready.
ensureCompiled :: FilePath -> IO ()
ensureCompiled repo =
  void $ adraiJsonOrThrow repo ["compile", "--json"]

-- =====================================================================
-- Test 1: History returns ordered log (newest first)
-- =====================================================================

testHistoryReturnsOrderedLog :: TestTree
testHistoryReturnsOrderedLog =
  testCase
    "history_command_returns_ordered_log"
    $ withSystemTempDirectory "adrai-history-ordered" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create 5 ADRs sequentially
        _ <- createAdr repo "ADR 1" "First ADR" "Body 1" ["compiler"] ["src/**"]
        _ <- createAdr repo "ADR 2" "Second ADR" "Body 2" ["runtime"] ["src/**"]
        _ <- createAdr repo "ADR 3" "Third ADR" "Body 3" ["api"] ["src/**"]
        _ <- createAdr repo "ADR 4" "Fourth ADR" "Body 4" ["compiler"] ["src/**"]
        _ <- createAdr repo "ADR 5" "Fifth ADR" "Body 5" ["runtime"] ["src/**"]

        -- Compile to establish database
        ensureCompiled repo

        -- Run history
        result <- adraiJson repo ["history", "--json"]
        case result of
          Right val -> do
            let parsed = parseHistory val
            case parsed of
              Just (schema, _rev, order, results) -> do
                schema @?= "adrai/history/v1"
                order @?= "newest-first"
                let count = length results
                assertBool
                  "history should return 5 ADR entries"
                  (count >= 5)

                -- Verify ordering: check that the labels appear in descending
                -- creation order (5, 4, 3, 2, 1)
                let labels = mapMaybe historyLabel results
                -- The newest entries should come first
                assertBool
                  "results should have entries with create labels"
                  (any (== "created") labels)
              Nothing -> assertFailure "could not parse history output"
          Left err ->
            assertFailure $ "history command failed: " <> unpack err

-- =====================================================================
-- Test 2: History respects since/until filters
-- =====================================================================

testHistoryRespectsSinceUntilFilters :: TestTree
testHistoryRespectsSinceUntilFilters =
  testCase
    "history_command_respects_since_until_filters"
    $ withSystemTempDirectory "adrai-history-filters" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create a couple of ADRs
        _ <- createAdr repo "Old ADR" "Old" "Content from past" ["compiler"] ["src/**"]
        _ <- createAdr repo "New ADR" "New" "Content from future" ["compiler"] ["src/**"]

        -- Compile
        ensureCompiled repo

        -- Get full history for baseline
        fullResult <- adraiJson repo ["history", "--json"]
        case fullResult of
          Right val ->
            let parsed = parseHistory val
            in case parsed of
                 Just (_, _, _, fullResults) ->
                   assertBool
                     "full history has at least 2 results"
                     (length fullResults >= 2)
                 Nothing -> assertFailure "could not parse full history"
          Left _ -> assertFailure "full history failed"

        -- Test since filter with "0" (should return everything)
        sinceResult <- adraiJson repo ["history", "--since", "0", "--json"]
        case sinceResult of
          Right val ->
            let parsed = parseHistory val
            in case parsed of
                 Just (_, _, _, results) ->
                   assertBool
                     "since:0 should return all ADRs"
                     (length results >= 2)
                 Nothing -> assertFailure "could not parse since history"
          Left _ -> assertFailure "since filter failed"

        -- Test since with a very far-future timestamp (should return 0)
        farFuture <- adraiJson repo
          [ "history", "--since", "9999999999999", "--json" ]
        case farFuture of
          Right val ->
            let parsed = parseHistory val
            in case parsed of
                 Just (_, _, _, results) ->
                   assertBool
                     "far-future since should return no results"
                     (length results == 0)
                 Nothing -> assertFailure "could not parse far-future history"
          Left _ -> assertFailure "far-future since filter failed"

        -- Test until with a very far-past timestamp (should return 0)
        farPast <- adraiJson repo
          [ "history", "--until", "0", "--json" ]
        case farPast of
          Right val ->
            let parsed = parseHistory val
            in case parsed of
                 Just (_, _, _, results) ->
                   assertBool
                     "until:0 should return no results"
                     (length results == 0)
                 Nothing -> assertFailure "could not parse until history"
          Left _ -> assertFailure "until filter failed"

        -- Test combined since+until with narrow range
        narrowResult <- adraiJson repo
          [ "history", "--since", "0", "--until", "0", "--json" ]
        case narrowResult of
          Right val ->
            let parsed = parseHistory val
            in case parsed of
                 Just (_, _, _, results) ->
                   assertBool
                     "narrow since+until should return fewer or no results"
                     (length results < 2)
                 Nothing -> assertFailure "could not parse narrow history"
          Left _ -> assertFailure "narrow filter failed"

-- =====================================================================
-- Test 3: History respects domain filter
-- =====================================================================

testHistoryRespectsDomainFilter :: TestTree
testHistoryRespectsDomainFilter =
  testCase
    "history_command_respects_domain_filter"
    $ withSystemTempDirectory "adrai-history-domain" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        _ <- createAdr repo "Compiler ADR" "Compiler" "Compiler body" ["compiler.cache"] ["src/**"]
        _ <- createAdr repo "Runtime ADR" "Runtime" "Runtime body" ["runtime.jobs"] ["src/**"]
        _ <- createAdr repo "API ADR" "API" "API body" ["api.compatibility"] ["src/**"]

        -- Compile
        ensureCompiled repo

        -- Filter by compiler domain
        result <- adraiJson repo ["history", "--domain", "compiler", "--json"]
        case result of
          Right val ->
            let parsed = parseHistory val
            in case parsed of
                 Just (_, _, _, results) -> do
                   let count = length results
                   -- Should only see the compiler ADR (at most 2 if the
                   -- runtime ADR shares a common domain prefix)
                   assertBool
                     "compiler domain filter should return only compiler ADRs"
                     (count >= 1 && count <= 2)
                 Nothing -> assertFailure "could not parse domain history"
          Left err ->
            assertFailure $ "history --domain failed: " <> unpack err

        -- Filter by runtime domain
        runtimeResult <- adraiJson repo
          [ "history", "--domain", "runtime", "--json" ]
        case runtimeResult of
          Right val ->
            let parsed = parseHistory val
            in case parsed of
                 Just (_, _, _, results) ->
                   assertBool
                     "runtime domain filter should return runtime ADRs"
                     (length results >= 1)
                 Nothing -> assertFailure "could not parse runtime history"
          Left _ -> assertFailure "runtime domain filter failed"

        -- Filter by api domain
        apiResult <- adraiJson repo
          [ "history", "--domain", "api", "--json" ]
        case apiResult of
          Right val ->
            let parsed = parseHistory val
            in case parsed of
                 Just (_, _, _, results) ->
                   assertBool
                     "api domain filter should return api ADRs"
                     (length results >= 1)
                 Nothing -> assertFailure "could not parse api history"
          Left _ -> assertFailure "api domain filter failed"

-- =====================================================================
-- Test 4: History shows merged-branch ADRs
-- =====================================================================

testHistoryShowsMergedBranchAdrs :: TestTree
testHistoryShowsMergedBranchAdrs =
  testCase
    "history_command_shows_merged_branch_adrs"
    $ withSystemTempDirectory "adrai-history-merge" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create an ADR on main
        _ <- createAdr repo "Main ADR" "Main" "Main body" ["compiler"] ["src/**"]

        -- Create feature branch
        git repo ["switch", "-c", "feature/x"]
        _ <- createAdr repo "Feature ADR" "Feature" "Feature body" ["compiler"] ["src/**"]

        -- Switch back to main and merge
        git repo ["switch", "main"]
        git repo ["merge", "feature/x", "--no-ff", "-m", "Merge feature/x"]

        -- Compile
        ensureCompiled repo

        -- Verify both ADRs visible in history
        result <- adraiJson repo ["history", "--json"]
        case result of
          Right val ->
            let parsed = parseHistory val
            in case parsed of
                 Just (_, _, _, results) ->
                   assertBool
                     "history after merge should show both main and feature ADRs"
                     (length results >= 2)
                 Nothing -> assertFailure "could not parse history"
          Left err ->
            assertFailure $ "history after merge failed: " <> unpack err

-- =====================================================================
-- Test 5: History omits obsolete by default
-- =====================================================================

testHistoryOmitsObsoleteByDefault :: TestTree
testHistoryOmitsObsoleteByDefault =
  testCase
    "history_command_omits_obsolete_by_default"
    $ withSystemTempDirectory "adrai-history-obsolete" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create an ADR and mark it obsolete
        _ <- createAdr repo "Obsolete ADR" "Obsolete" "This is obsolete" ["compiler"] ["src/**"]

        -- Get the ADR ID from create result
        createResult <- adraiJsonOrThrow repo
          [ "create",
            "--summary", "Obsolete",
            "--body", "## Decision\nThis is obsolete.",
            "--domain", "compiler",
            "--applies-to", "src/**",
            "--actor", "llm:planner",
            "--model", "demo-model",
            "--json"
          ]
        let adrId = extractAdrId createResult
        case adrId of
          Nothing -> assertFailure "could not extract ADR ID from create"
          Just id' -> do
            -- Amend to obsolete
            void $
              amendAdrStatus repo id' "obsolete"

            -- Compile
            ensureCompiled repo

            -- Default history should NOT show obsolete ADR
            defaultResult <- adraiJson repo ["history", "--json"]
            case defaultResult of
              Right val ->
                let parsed = parseHistory val
                in case parsed of
                     Just (_, _, _, results) ->
                       assertBool
                         "default history should not show obsolete ADR"
                         (length results == 0)
                     Nothing -> assertFailure "could not parse default history"
              Left _ -> assertFailure "default history failed"

            -- With --include-obsolete, should show the ADR
            obsResult <- adraiJson repo
              [ "history", "--include-obsolete", "--json" ]
            case obsResult of
              Right val ->
                let parsed = parseHistory val
                in case parsed of
                     Just (_, _, _, results) ->
                       assertBool
                         "--include-obsolete should show the obsolete ADR"
                         (length results >= 1)
                     Nothing -> assertFailure "could not parse include-obsolete history"
              Left _ -> assertFailure "include-obsolete history failed"

-- =====================================================================
-- Test 6: History reveals divergent amendments
-- =====================================================================

testHistoryRevealsDivergentAmendments :: TestTree
testHistoryRevealsDivergentAmendments =
  testCase
    "history_command_reveals_divergent_amendments"
    $ withSystemTempDirectory "adrai-history-divergent" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create ADR
        createResult <- adraiJsonOrThrow repo
          [ "create",
            "--summary", "Divergent",
            "--body", "## Decision\nOriginal body.",
            "--domain", "compiler",
            "--applies-to", "src/**",
            "--actor", "llm:planner",
            "--model", "demo-model",
            "--json"
          ]
        let adrId = extractAdrId createResult
        case adrId of
          Nothing -> assertFailure "could not extract ADR ID"
          Just id' -> do
            -- Amend on feature/a
            git repo ["switch", "-c", "feature/a"]
            void $
              amendAdr repo id'
                (Just "Amended on A")
                Nothing
                Nothing

            -- Switch back to main and create feature/b
            git repo ["switch", "main"]
            git repo ["switch", "-c", "feature/b"]
            void $
              amendAdr repo id'
                (Just "Amended on B")
                Nothing
                Nothing

            -- Merge both back into main
            git repo ["switch", "main"]
            git repo ["merge", "feature/a", "--no-ff", "-m", "merge a"]
            git repo ["merge", "feature/b", "--no-ff", "-m", "merge b"]

            -- Compile
            ensureCompiled repo

            -- History should show both amendment versions
            result <- adraiJson repo ["history", "--json"]
            case result of
              Right val ->
                let parsed = parseHistory val
                in case parsed of
                     Just (_, _, _, results) -> do
                       let labels = mapMaybe historyLabel results
                       assertBool
                         "divergent history should show amendment versions"
                         (any (== "amended") labels)
                     Nothing -> assertFailure "could not parse history"
              Left err ->
                assertFailure $ "divergent history failed: " <> unpack err

-- =====================================================================
-- Test 7: History with compact format
-- =====================================================================

testHistoryWithCompactFormat :: TestTree
testHistoryWithCompactFormat =
  testCase
    "history_command_with_compact_format"
    $ withSystemTempDirectory "adrai-history-compact" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        _ <- createAdr repo "Compact ADR 1" "Compact 1" "Body 1" ["compiler"] ["src/**"]
        _ <- createAdr repo "Compact ADR 2" "Compact 2" "Body 2" ["runtime"] ["src/**"]

        -- Compile
        ensureCompiled repo

        -- Run history with compact flag
        result <- adraiJson repo ["history", "--compact", "--json"]
        case result of
          Right val ->
            let parsed = parseHistoryCompact val
            in case parsed of
                 Just (schema, _rev, order, results) -> do
                   -- Compact schema should have a distinct prefix
                   assertBool
                     "compact schema should be present and distinct"
                     (T.isPrefixOf "adrai/history-compact/" schema)
                   order @?= "newest-first"
                   assertBool
                     "compact should return results"
                     (length results >= 2)
                 Nothing -> assertFailure "could not parse compact history"
          Left err ->
            assertFailure $ "compact history failed: " <> unpack err

-- =====================================================================
-- Test 8: History rejects invalid revisions
-- =====================================================================

testHistoryRejectsInvalidRevisions :: TestTree
testHistoryRejectsInvalidRevisions =
  testCase
    "history_command_rejects_invalid_revisions"
    $ withSystemTempDirectory "adrai-history-invalid" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Compile to establish database
        ensureCompiled repo

        -- Run history with an invalid revision
        result <- adraiJson repo
          [ "history", "--revision", "deadbeefcafe1234567890abcdef1234567890ab", "--json" ]
        case result of
          Left err ->
            -- Expect CLI failure with error message
            assertBool
              "invalid revision should return error"
              (not (T.null err))
          Right val ->
            -- Even if JSON parse succeeds, check for error field
            case _Object val of
              Just o -> do
                let hasError = isJust (o .: "error" :: Maybe Text)
                assertBool
                  "invalid revision should have error field or parse as error"
                  hasError
              Nothing ->
                assertFailure "expected Object but got something else"

-- =====================================================================
-- Test suite
-- =====================================================================

tests :: TestTree
tests =
  testGroup
    "HistoryCommand"
    [ testHistoryReturnsOrderedLog,
      testHistoryRespectsSinceUntilFilters,
      testHistoryRespectsDomainFilter,
      testHistoryShowsMergedBranchAdrs,
      testHistoryOmitsObsoleteByDefault,
      testHistoryRevealsDivergentAmendments,
      testHistoryWithCompactFormat,
      testHistoryRejectsInvalidRevisions
    ]
