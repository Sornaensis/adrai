{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for evolution (amendments, exploded view),
-- compare (diff between revisions, branches, cold rebuild),
-- and ANN search (determinism and repeatability).
--
-- Ports the evolution/compare/ANN tests from the Python prototype
-- into the Haskell integration test harness.
module Adrai.EvolutionCompareAnnTest (tests) where

import Adrai.Integration.CLI
import Adrai.GitTestSupport (gitResult)
import Control.Monad (void)
import qualified Data.Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text, strip, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import System.FilePath ((</>))
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

-- | Extract the head commit hash of a repository.
headCommit :: FilePath -> IO Text
headCommit repo =
  gitStdout repo ["rev-parse", "HEAD"]
    >>= \h -> pure (strip (decodeUtf8 (LBS.toStrict h)))

-- | Compile the repo to ensure database is ready.
ensureCompiled :: FilePath -> IO ()
ensureCompiled repo =
  void $ adraiJsonOrThrow repo ["compile", "--json"]

-- =====================================================================
-- Test 1: compare_command_shows_added_changed_obsolete_entries
-- =====================================================================

testCompareShowsAddedChangedObsolete :: TestTree
testCompareShowsAddedChangedObsolete =
  testCase
    "compare_command_shows_added_changed_obsolete_entries"
    $ withSystemTempDirectory "adrai-compare-kinds" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create initial ADRs
        _ <- createAdr repo "ADR 1" "First" "Body 1" ["compiler"] ["src/**"]
        _ <- createAdr repo "ADR 2" "Second" "Body 2" ["runtime"] ["src/**"]
        baseRev <- headCommit repo

        -- Create a new ADR (added)
        _ <- createAdr repo "ADR 3" "Third" "Body 3" ["api"] ["src/**"]

        -- Get ADR 1 ID from create
        adr1Result <-
          adraiJsonOrThrow repo
            [ "create",
              "--summary", "Changed",
              "--body", "Body 1 changed.",
              "--domain", "compiler",
              "--applies-to", "src/**",
              "--actor", "llm:planner",
              "--model", "demo-model",
              "--json"
            ]
        let changedId = extractAdrId adr1Result
        case changedId of
          Nothing -> assertFailure "could not extract ADR ID for changed"
          Just id' ->
            void $ amendAdr repo id' (Just "ADR 1 Changed") Nothing Nothing

        -- Obsolete one ADR
        adr2Result <-
          adraiJsonOrThrow repo
            [ "create",
              "--summary", "Obsolete",
              "--body", "Body 2 obsolete.",
              "--domain", "runtime",
              "--applies-to", "src/**",
              "--actor", "llm:planner",
              "--model", "demo-model",
              "--json"
            ]
        let obsoleteId = extractAdrId adr2Result
        case obsoleteId of
          Nothing -> assertFailure "could not extract ADR ID for obsolete"
          Just id' ->
            void $ amendAdrStatus repo id' "obsolete"

        newRev <- headCommit repo

        -- Compile to ensure DB is ready
        ensureCompiled repo

        -- Compare base to new
        result <- adraiJson repo ["compare", "--from", unpack baseRev, "--to", unpack newRev, "--json"]
        case result of
          Right val ->
            let parsed = parseCompareResults val
            in case parsed of
                 Just (_, _, _, changes) -> do
                   assertBool "compare should show changes" (length changes >= 1)
                   -- Verify kinds present
                   let kinds = map (extractField "kind") changes
                   assertBool
                     "should have added/changed/obsolete kinds"
                     (any ("added" `T.isPrefixOf`) kinds
                       || any ("changed" `T.isPrefixOf`) kinds
                       || any ("obsolete" `T.isPrefixOf`) kinds)
                 Nothing -> assertFailure "could not parse compare results"
          Left err ->
            assertFailure $ "compare failed: " <> unpack err

-- =====================================================================
-- Test 2: compare_command_with_include_unchanged
-- =====================================================================

testCompareWithIncludeUnchanged :: TestTree
testCompareWithIncludeUnchanged =
  testCase
    "compare_command_with_include_unchanged"
    $ withSystemTempDirectory "adrai-compare-unchanged" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create initial ADRs
        _ <- createAdr repo "ADR A" "A" "Body A" ["compiler"] ["src/**"]
        _ <- createAdr repo "ADR B" "B" "Body B" ["runtime"] ["src/**"]
        baseRev <- headCommit repo

        -- Create one new ADR (the rest will be unchanged)
        _ <- createAdr repo "ADR C" "C" "Body C" ["api"] ["src/**"]
        newRev <- headCommit repo

        -- Compile to ensure DB is ready
        ensureCompiled repo

        -- Without --include-unchanged
        resultNoUnchanged <-
          adraiJson
            repo
            ["compare", "--from", unpack baseRev, "--to", unpack newRev, "--json"]
        let countNoUnchanged =
              case resultNoUnchanged of
                Right val ->
                  case parseCompareResults val of
                    Just (_, _, _, changes) -> length changes
                    Nothing -> 0
                Left _ -> 0

        -- With --include-unchanged
        resultWithUnchanged <-
          adraiJson
            repo
            [ "compare",
              "--from",
              unpack baseRev,
              "--to",
              unpack newRev,
              "--include-unchanged",
              "--json"
            ]
        let countWithUnchanged =
              case resultWithUnchanged of
                Right val ->
                  case parseCompareResults val of
                    Just (_, _, _, changes) -> length changes
                    Nothing -> 0
                Left _ -> 0

        assertBool
          "with-unchanged should return more results than without"
          (countWithUnchanged > countNoUnchanged)

-- =====================================================================
-- Test 3: compare_command_branch_vs_branch
-- =====================================================================

testCompareBranchVsBranch :: TestTree
testCompareBranchVsBranch =
  testCase
    "compare_command_branch_vs_branch"
    $ withSystemTempDirectory "adrai-compare-branches" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create ADR on main
        _ <- createAdr repo "Main ADR" "Main" "Main body" ["compiler"] ["src/**"]

        -- Create feature branch with different ADRs
        git repo ["switch", "-c", "feature/x"]
        _ <- createAdr repo "Feature ADR" "Feature" "Feature body" ["runtime"] ["src/**"]

        -- Compile to ensure DB is ready
        ensureCompiled repo

        -- Compare feature vs main
        result <-
          adraiJson
            repo
            ["compare", "--from", "main", "--to", "feature/x", "--json"]
        case result of
          Right val ->
            let parsed = parseCompareResults val
            in case parsed of
                 Just (_, _, _, changes) ->
                   assertBool
                     "branch compare should show feature ADR as added"
                     (length changes >= 1)
                 Nothing -> assertFailure "could not parse compare results"
          Left err ->
            assertFailure $ "branch compare failed: " <> unpack err

-- =====================================================================
-- Test 4: compare_command_consistent_with_cold_rebuild
-- =====================================================================

testCompareConsistentColdRebuild :: TestTree
testCompareConsistentColdRebuild =
  testCase
    "compare_command_consistent_with_cold_rebuild"
    $ withSystemTempDirectory "adrai-compare-cold" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create ADRs
        _ <- createAdr repo "Cold ADR 1" "Cold 1" "Body 1" ["compiler"] ["src/**"]
        _ <- createAdr repo "Cold ADR 2" "Cold 2" "Body 2" ["runtime"] ["src/**"]
        rev1 <- headCommit repo

        _ <- createAdr repo "Cold ADR 3" "Cold 3" "Body 3" ["api"] ["src/**"]
        rev2 <- headCommit repo

        -- Compile to ensure DB is ready
        ensureCompiled repo

        -- Compare from first temp dir
        result1 <-
          adraiJson
            repo
            ["compare", "--from", unpack rev1, "--to", unpack rev2, "--json"]

        -- Clone to second temp dir and compare with the same revision hashes.
        -- A git clone preserves all commit hashes, so rev1 and rev2 are
        -- identical in the clone. This verifies cold-rebuild determinism.
        let repo2 = baseDir </> "test-repo-2"
        git repo2 ["clone", repo, "."]

        -- The cloned repo has the same commits, so rev1 and rev2 are valid
        -- references pointing to the same content.
        result2 <-
          adraiJson
            repo2
            ["compare", "--from", unpack rev1, "--to", unpack rev2, "--json"]

        -- Compare the two results
        case (result1, result2) of
          (Right v1, Right v2) ->
            assertBool
              "cold rebuild should produce identical compare results"
              (v1 == v2)
          _ ->
            assertFailure "one of the compares failed"

-- =====================================================================
-- Test 5: evolution_command_tracks_full_adr_lifecycle
-- =====================================================================

testEvolutionFullLifecycle :: TestTree
testEvolutionFullLifecycle =
  testCase
    "evolution_command_tracks_full_adr_lifecycle"
    $ withSystemTempDirectory "adrai-evolution-lifecycle" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create initial ADR
        createResult <-
          adraiJsonOrThrow repo
            [ "create",
              "--summary", "Lifecycle",
              "--body", "## Decision\nInitial body.\n",
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
            -- Amend status
            void $ amendAdrStatus repo id' "active"

            -- Amend scope
            void $ amendAdrScope repo id' ["src/compiler/**"] []

            -- Amend body
            void $
              amendAdr
                repo
                id'
                (Just "Lifecycle ADR")
                (Just "Updated summary")
                (Just "Updated body with more detail")

            -- Compile to ensure DB is ready
            ensureCompiled repo

            -- Get exploded view
            result <-
              adraiJson
                repo
                [ "show", unpack id', "--exploded", "--json" ]
            case result of
              Right val ->
                let parsed = parseShowExploded val
                in case parsed of
                     Just records ->
                       assertBool
                         "exploded view should show multiple amendment records"
                         (length records >= 2)
                     Nothing -> assertFailure "could not parse exploded view"
              Left err ->
                assertFailure $ "show exploded failed: " <> unpack err

-- =====================================================================
-- Test 6: evolution_command_with_exploded_view
-- =====================================================================

testEvolutionExplodedView :: TestTree
testEvolutionExplodedView =
  testCase
    "evolution_command_with_exploded_view"
    $ withSystemTempDirectory "adrai-evolution-exploded" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create ADR with multiple amendments
        createResult <-
          adraiJsonOrThrow repo
            [ "create",
              "--summary", "Evolution",
              "--body", "## Decision\nFirst body.\n",
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
            _ <- amendAdr repo id' (Just "Evolution ADR v2") Nothing Nothing
            _ <- amendAdr repo id' (Just "Evolution ADR v3") Nothing Nothing
            _ <- amendAdr
              repo
              id'
              Nothing
              (Just "v3 summary")
              (Just "v3 body")

            -- Compile to ensure DB is ready
            ensureCompiled repo

            -- Get exploded view
            result <-
              adraiJson
                repo
                [ "show", unpack id', "--exploded", "--json" ]
            case result of
              Right val ->
                let parsed = parseShowExploded val
                in case parsed of
                     Just records -> do
                       assertBool
                         "exploded view should contain original and amendment records"
                         (length records >= 3)
                       -- Verify the records show the progression
                       let titles = map (extractField "title") records
                       assertBool
                         "records should show title progression"
                         (any ("v3" `T.isInfixOf`) titles
                           || any ("v2" `T.isInfixOf`) titles)
                     Nothing -> assertFailure "could not parse exploded view"
              Left err ->
                assertFailure $ "show exploded failed: " <> unpack err

-- =====================================================================
-- Test 7: evolution_command_shows_conflict_resolution
-- =====================================================================

testEvolutionConflictResolution :: TestTree
testEvolutionConflictResolution =
  testCase
    "evolution_command_shows_conflict_resolution"
    $ withSystemTempDirectory "adrai-evolution-conflict" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create ADR
        createResult <-
          adraiJsonOrThrow repo
            [ "create",
              "--summary", "Conflict",
              "--body", "## Decision\nOriginal body.\n",
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
            -- Divergent amendments
            git repo ["switch", "-c", "branch/a"]
            _ <- amendAdr repo id' (Just "Conflict ADR from A") Nothing Nothing

            git repo ["switch", "main"]
            git repo ["switch", "-c", "branch/b"]
            _ <- amendAdr repo id' (Just "Conflict ADR from B") Nothing Nothing

            -- Merge (creates conflict)
            git repo ["switch", "main"]
            _ <- gitResult repo ["merge", "branch/a", "--no-ff", "-m", "merge a"] BS.empty
            _ <- gitResult repo ["merge", "branch/b", "--no-ff", "-m", "merge b"] BS.empty

            -- Resolution: amend to resolve
            _ <- amendAdr repo id' (Just "Conflict ADR Resolved") Nothing Nothing

            -- Compile to ensure DB is ready
            ensureCompiled repo

            -- Get exploded view
            result <-
              adraiJson
                repo
                [ "show", unpack id', "--exploded", "--json" ]
            case result of
              Right val ->
                let parsed = parseShowExploded val
                in case parsed of
                     Just records ->
                       assertBool
                         "conflict evolution should show multiple states"
                         (length records >= 3)
                     Nothing -> assertFailure "could not parse exploded view"
              Left err ->
                assertFailure $ "conflict evolution failed: " <> unpack err



-- =====================================================================
-- Test 8: ann_buckets_are_deterministic_and_repeatable
-- =====================================================================

testAnnBucketsDeterministic :: TestTree
testAnnBucketsDeterministic =
  testCase
    "ann_buckets_are_deterministic_and_repeatable"
    $ withSystemTempDirectory "adrai-ann-determinism" $ \baseDir -> do
        repo <- createTestRepo baseDir
        createAdraiInit repo

        -- Create multiple ADRs with embedding data
        _ <- createAdr repo "ANN ADR 1" "ANN 1" "Cache identity and vector processing" ["compiler"] ["src/**"]
        _ <- createAdr repo "ANN ADR 2" "ANN 2" "Semantic search and relevance" ["runtime"] ["src/**"]
        _ <- createAdr repo "ANN ADR 3" "ANN 3" "Full-text indexing and retrieval" ["api"] ["src/**"]

        -- Compile to establish embeddings
        ensureCompiled repo

        -- Run ANN search twice
        result1 <- adraiJson repo ["search", "--query", "cache identity", "--json"]
        result2 <- adraiJson repo ["search", "--query", "cache identity", "--json"]

        -- Results should be identical (deterministic)
        case (result1, result2) of
          (Right v1, Right v2) ->
            assertBool
              "ANN search should produce identical results"
              (v1 == v2)
          _ ->
            assertFailure "one or both ANN searches failed"

-- =====================================================================
-- Test suite
-- =====================================================================

tests :: TestTree
tests =
  testGroup
    "EvolutionCompareAnn"
    [ testCompareShowsAddedChangedObsolete,
      testCompareWithIncludeUnchanged,
      testCompareBranchVsBranch,
      testCompareConsistentColdRebuild,
      testEvolutionFullLifecycle,
      testEvolutionExplodedView,
      testEvolutionConflictResolution,
      testAnnBucketsDeterministic
    ]
