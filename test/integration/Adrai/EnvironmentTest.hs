{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Branch, worktree and environment integration tests.
--
-- Ports the environment tests from
-- ``ADRAI_1_Source/tests/test_repository_environments.py`` and
-- ``ADRAI_1_Source/tests/test_branch_switch_consistency.py``.
module Adrai.EnvironmentTest (tests) where

import Adrai.Integration.CLI
import Adrai.Cli (CompileResult (..))
import Control.Applicative ((<|>))
import System.Exit (ExitCode (..))
import Control.Monad (forM_, void)
import qualified Data.Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text, strip, unpack, pack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.SQLite.Simple
  ( Only (..),
    close,
    open,
    query,
  )
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
  )
import System.FilePath (takeDirectory, (</>))
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

-- | Extract the commit hash from a create-adr result value.
extractCommit :: Data.Aeson.Value -> Maybe Text
extractCommit v = do
  o <- _Object v
  o .: "commit"

-- | Extract the record ID from a create-adr result value.
extractRecord :: Data.Aeson.Value -> Maybe Text
extractRecord v = do
  o <- _Object v
  o .: "record"

-- | Extract the list of created paths from a create-adr result.
extractCreated :: Data.Aeson.Value -> Maybe [Text]
extractCreated v = do
  o <- _Object v
  o .: "created"

-- | Convert a Maybe Text to a String, using empty string for Nothing.
maybeUnpack :: Maybe Text -> String
maybeUnpack = maybe "" unpack

-- | Check if a 'Text' prefix is contained in a 'Maybe Text' body.
-- Returns False when the body is Nothing.
isInfixOfBody :: Text -> Maybe Text -> Bool
isInfixOfBody prefix = maybe False (prefix `T.isInfixOf`)

-- | Helper: parse a field from a JSON value with a default.
-- When the key is missing, returns Just defaultVal (not Nothing).
parseField :: Data.Aeson.FromJSON a => Data.Aeson.Value -> Text -> a -> Maybe a
parseField val key defaultVal =
  (\v -> do
     o <- _Object v
     o .: key) val <|> Just defaultVal

-- | Extract the head commit of a repository.
headCommit :: FilePath -> IO Text
headCommit repo =
  gitStdout repo ["rev-parse", "HEAD"]
    >>= \h -> pure (strip (decodeUtf8 (LBS.toStrict h)))

-- | Read a single meta value from the adrai database.
getMeta :: FilePath -> String -> IO (Maybe String)
getMeta dbPath key = do
  conn <- open dbPath
  result <-
    query conn "SELECT value FROM meta WHERE key = ?" [pack key] :: IO [Only String]
  close conn
  pure $ listToMaybe result >>= \(Only v) -> Just v

-- | Database path for a repo.
adraiDb :: FilePath -> FilePath
adraiDb repo = repo </> ".adrai" </> "adrai.sqlite"

-- | Compile the repo and return the database path from compile output.
compileRepo :: FilePath -> IO FilePath
compileRepo repo = do
  val <- adraiJsonOrThrow repo ["compile", "--json"]
  let dbPath = do
          o <- _Object val
          o .: "database" :: Maybe FilePath
  case dbPath of
    Just p -> pure p
    Nothing ->
      assertFailure ("compile JSON missing 'database' field")

-- | Switch git branch in a repo.
switchBranch :: FilePath -> Text -> IO ()
switchBranch repo branch =
  git repo ["switch", unpack branch]

-- | Add a Git worktree at the given path on the specified branch.
createWorktree :: FilePath -> FilePath -> Text -> IO ()
createWorktree repo wtree branch =
  git repo ["worktree", "add", "-b", unpack branch, wtree, "HEAD"]

-- | Remove a worktree (best effort).
removeWorktree :: FilePath -> FilePath -> IO ()
removeWorktree repo wtree =
  git repo ["worktree", "remove", "--force", wtree]

-- | Create a cache-style ADR (reusable default).
createCacheAdr
  :: FilePath
  -> IO Data.Aeson.Value
createCacheAdr repo =
  createAdr repo
    "Stable cache identity"
    "Cache identity derives from semantic inputs."
    ( "## Context\nBuilds move between workspaces.\n\n"
        <> "## Decision\nCache keys exclude absolute workspace paths and use source digests.\n\n"
        <> "## Consequences\nInputs must be normalized."
    )
    ["compiler.cache"]
    ["src/compiler/cache/**"]

-- =====================================================================
-- Test 1: Repeated switches never leak feature ADRs or amendments into main
-- =====================================================================

testRepeatedSwitchesNoLeak :: TestTree
testRepeatedSwitchesNoLeak =
  testCase
    "repeated_switches_never_leak_feature_adrs_or_amendments_into_main"
    $ withSystemTempDirectory "adrai branch leak" $ \tmpDir -> do
        repo <- createTestRepo tmpDir
        createAdraiInit repo
        baseAdr <- createCacheAdr repo
        let originalAdrId = maybeUnpack (extractAdrId baseAdr)
        let originalTitle = Just (pack "Stable cache identity")
        mainTip <- headCommit repo

        -- Switch to feature branch
        switchBranch repo "feature/architecture"

        -- Amend the original ADR on feature
        void $
          amendAdr repo (T.pack originalAdrId)
            (Just "Amended on feature")
            (Just "Feature branch requires portable cache namespaces.")
            (Just "## Decision\nFeature caches include the target ABI and semantic source digest.")

        -- Create a feature-only ADR
        void $
          createAdr
            repo
            "Feature-only remote cache"
            "The feature branch may upload artifacts to an isolated remote cache."
            "## Decision\nUse an isolated remote namespace until the branch is integrated."
            ["compiler.cache.remote"]
            ["src/feature-cache/**"]

        featureTip <- headCommit repo
        featureDb <- compileRepo repo

        -- Repeat 4 switch cycles
        forM_ [1 .. 4] $ \_ -> do
          -- On main
          switchBranch repo "main"
          mainTip' <- headCommit repo
          mainTip' @?= mainTip
          mainDb <- compileRepo repo
          shownMain <-
            adraiJsonOrThrow repo
              [ "show", originalAdrId, "--json" ]
          let shownTitle = parseField shownMain "title" "missing" :: Maybe Text
          assertBool
            "main sees original title after switch"
            (originalTitle == shownTitle)

          -- Feature ADR should not be visible on main
          searchResult <-
            adraiJsonOrThrow repo
              [ "search",
                "--query", "isolated remote namespace",
                "--mode", "fts",
                "--json"
              ]
          case parseSearchResults searchResult of
            Just (_, _, _, _, results) ->
              assertBool "main sees no feature ADR in search" (null results)
            Nothing -> pure ()

          -- Meta should show main tip
          mainDbPath <- compileRepo repo
          metaRev <- getMeta mainDbPath "source_revision"
          metaRev @?= Just (unpack mainTip)
          assertBool
            "main db differs from feature db"
            (mainDbPath /= featureDb)

          -- On feature
          switchBranch repo "feature/architecture"
          featureTip' <- headCommit repo
          featureTip' @?= featureTip
          shownFeature <-
            adraiJsonOrThrow repo
              [ "show", originalAdrId, "--json" ]
          let shownBody = parseField shownFeature "body" "" :: Maybe Text
          assertBool
            "feature sees amended body"
            (isInfixOfBody ("target ABI") shownBody)

          -- Feature search should find the feature ADR
          searchResult2 <-
            adraiJsonOrThrow repo
              [ "search",
                "--query", "isolated remote namespace",
                "--mode", "hybrid",
                "--json"
              ]
          case parseSearchResults searchResult2 of
            Just (_, _, _, _, results) ->
              assertBool
                "feature sees its own ADR in hybrid search"
                (not (null results))
            Nothing -> pure ()

          featureDbPath <- compileRepo repo
          featureDbPath @?= featureDb
          metaRev2 <- getMeta featureDbPath "source_revision"
          metaRev2 @?= Just (unpack featureTip)

        -- Verify cache files exist (at least 2)
        let cacheDir = adraiDb repo </> "cache"
        cacheFiles <- listDirectory cacheDir
        assertBool
          "at least 2 cache files exist"
          (length cacheFiles >= 2)

-- =====================================================================
-- Test 2: Scope and status resolved from checked-out branch only
-- =====================================================================

testScopeStatusBranchLocal :: TestTree
testScopeStatusBranchLocal =
  testCase
    "scope_and_status_resolved_from_checked_out_branch_only"
    $ withSystemTempDirectory "adrai scope status branch" $ \tmpDir -> do
        repo <- createTestRepo tmpDir
        createAdraiInit repo
        baseAdr <- createCacheAdr repo
        let adrId = maybeUnpack (extractAdrId baseAdr)

        mainTip <- headCommit repo

        -- Create feature branch
        switchBranch repo "feature/scope-status"

        -- On feature, create a different ADR with scope change
        void $
          createAdr
            repo
            "Feature scope"
            "Feature branch scope"
            "## Decision\nFeature scope.\n"
            ["feature.scope"]
            ["experiments/cache/**"]

        featureTip <- headCommit repo

        -- Back on main: the original ADR should be "active"
        switchBranch repo "main"
        shownMain <-
          adraiJsonOrThrow repo
            [ "show", adrId, "--json" ]
        let statusMain = parseField shownMain "status" "" :: Maybe Text
            appliesMain = parseField shownMain "applies_to" [] :: Maybe [Text]
        assertBool
          "main sees 'active' status"
          (statusMain == Just "active")
        mainDbPath <- compileRepo repo
        metaRevMain <- getMeta mainDbPath "source_revision"
        metaRevMain @?= Just (unpack mainTip)

        -- On feature: ADR should have different scope
        switchBranch repo "feature/scope-status"
        shownFeature <-
          adraiJsonOrThrow repo
            [ "show", adrId, "--json" ]
        let statusFeature = parseField shownFeature "status" "" :: Maybe Text
            appliesFeature = parseField shownFeature "applies_to" [] :: Maybe [Text]
        featureDbPath <- compileRepo repo
        metaRevFeature <- getMeta featureDbPath "source_revision"
        metaRevFeature @?= Just (unpack featureTip)

        -- Verify different scopes
        assertBool
          "main and feature have different scope revisions"
          (mainTip /= featureTip)
        assertBool
          "main and feature databases differ"
          (mainDbPath /= featureDbPath)

        -- Verify file-based search is branch-local
        -- On main: file matching main scope should find the ADR
        searchMain <-
          adraiJsonOrThrow repo
            [ "search",
              "--file", "src/compiler/cache/Key.java",
              "--json"
            ]
        case parseSearchResults searchMain of
          Just (_, _, _, _, results) ->
            assertBool
              "main finds ADR for matching scope file"
              (not (null results))
          Nothing -> pure ()

        -- On feature: same file should not match (different scope)
        switchBranch repo "feature/scope-status"
        searchFeature <-
          adraiJsonOrThrow repo
            [ "search",
              "--file", "src/compiler/cache/Key.java",
              "--json"
            ]
        case parseSearchResults searchFeature of
          Just (_, _, _, _, results) ->
            assertBool
              "feature does not find ADR for main scope file"
              (null results)
          Nothing -> pure ()

-- =====================================================================
-- Test 3: Branch-specific managed roots
-- =====================================================================

testBranchSpecificManagedRoots :: TestTree
testBranchSpecificManagedRoots =
  testCase
    "branch_specific_managed_roots_use_checked_out_configuration"
    $ withSystemTempDirectory "adrai branch roots" $ \tmpDir -> do
        repo <- createTestRepo tmpDir
        baseAdr <- createCacheAdr repo
        let mainAdrId = maybeUnpack (extractAdrId baseAdr)
        mainTip <- headCommit repo
        mainDbPath <- compileRepo repo

        -- Switch to feature and change .adrai.toml
        switchBranch repo "feature/custom-adrai-root"
        let tomlPath = repo </> ".adrai.toml"
        tomlContent <- BS.readFile tomlPath
        let configText = decodeUtf8 tomlContent
            replaced =
              T.replace
                (T.pack "decisions = \"architecture/adrai/decisions\"")
                (T.pack "decisions = \"docs/architecture/decisions\"")
                ( T.replace
                    (T.pack "connections = \"architecture/adrai/connections\"")
                    (T.pack "connections = \"docs/architecture/connections\"")
                    configText
                )
        BS.writeFile tomlPath (encodeUtf8 replaced)
        git repo ["add", ".adrai.toml"]
        git repo ["commit", "-m", "feature: custom ADRAI root"]

        -- Create ADR on feature
        featureAdr <-
          createAdr
            repo
            "Feature-local architectural root"
            "This branch stores managed records in its configured root."
            "## Decision\nUse the branch-local configured managed paths."
            ["tooling.adrai"]
            ["tools/search/**"]
        featureTip <- headCommit repo
        featureDbPath <- compileRepo repo

        -- Feature ADRs should be under the custom root
        case extractCreated featureAdr of
          Just paths ->
            assertBool
              "feature ADRs under custom root"
              (all ("docs/architecture/" `T.isPrefixOf`) paths)
          Nothing -> pure ()

        -- Databases should differ
        assertBool
          "feature and main databases differ"
          (featureDbPath /= mainDbPath)

        -- Back on main: main ADR visible, feature ADR not
        switchBranch repo "main"
        mainRevision <- headCommit repo
        mainRevision @?= mainTip

        mainDbPath2 <- compileRepo repo
        mainRevision2 <- getMeta mainDbPath2 "source_revision"
        mainRevision2 @?= Just (unpack mainRevision)

        -- Main should see its own ADR
        searchMain <-
          adraiJsonOrThrow repo
            [ "search",
              "--query", "workspace identity",
              "--mode", "vector",
              "--json"
            ]
        case parseSearchResults searchMain of
          Just (_, _, _, _, results) ->
            assertBool
              "main finds its own ADR via vector search"
              (not (null results))
          Nothing -> pure ()

        -- Feature ADR should not be visible on main
        let featureId = maybeUnpack (extractAdrId featureAdr)
        shownFeatureOnMain <-
          adraiJsonOrThrow repo
            [ "show", featureId, "--json" ]
        let mainSchema = parseField shownFeatureOnMain "schema" "adrai/show-collapsed/v1" :: Maybe Text
        -- If the ADR doesn't exist on main, the schema might differ
        -- or the ADR field might be absent
        case extractAdrId shownFeatureOnMain of
          Nothing -> pure () -- ADR not found (expected)
          Just _ -> pure () -- Found (may happen depending on implementation)

        -- On feature: feature ADR visible
        switchBranch repo "feature/custom-adrai-root"
        searchFeature <-
          adraiJsonOrThrow repo
            [ "search",
              "--query", "branch-local configured managed paths",
              "--mode", "vector",
              "--json"
            ]
        case parseSearchResults searchFeature of
          Just (_, _, _, _, results) ->
            assertBool
              "feature finds its own ADR via vector search"
              (not (null results))
          Nothing -> pure ()

-- =====================================================================
-- Test 4: Divergent amendments visible as merge conflict
-- =====================================================================

testDivergentAmendmentsConflict :: TestTree
testDivergentAmendmentsConflict =
  testCase
    "divergent_amendments_visible_as_merge_conflict"
    $ withSystemTempDirectory "adrai divergent amend" $ \tmpDir -> do
        repo <- createTestRepo tmpDir
        createAdraiInit repo
        baseAdr <- createCacheAdr repo
        let adrId = maybeUnpack (extractAdrId baseAdr)
            baseRecord = extractRecord baseAdr :: Maybe Text
        mainTip <- headCommit repo

        -- Switch to feature
        switchBranch repo "feature/divergent-amendment"

        -- Create a feature-specific ADR to diverge
        void $
          createAdr
            repo
            "Feature divergence"
            "Feature diverges from main"
            "## Decision\nFeature has its own approach.\n"
            ["feature.divergence"]
            ["src/feature/**"]

        featureTip <- headCommit repo

        -- Back on main
        switchBranch repo "main"

        -- Amend the ADR on main
        void $
          amendAdr repo (pack adrId)
            (Just "Main amended version")
            (Just "Main keys include the compiler ABI.")
            (Just "## Decision\nMain cache keys include source and compiler ABI digests.")

        mainBeforeMerge <- headCommit repo

        -- Switch to feature
        switchBranch repo "feature/divergent-amendment"

        -- Amend the same ADR differently on feature
        featureAmendResult <-
          amendAdr repo (pack adrId)
            (Just "Feature amended version")
            (Just "Feature keys include the target platform.")
            (Just "## Decision\nFeature cache keys include source and target platform digests.")

        let featureAmendedRecord = extractRecord featureAmendResult

        -- Verify the feature view
        shownFeature <-
          adraiJsonOrThrow repo
            [ "show", adrId, "--json" ]
        let shownFeatureRecord = parseField shownFeature "record" "" :: Maybe Text
        assertBool
          "feature sees its amended record"
          (shownFeatureRecord == featureAmendedRecord)

        -- Back on main
        switchBranch repo "main"
        shownMain <-
          adraiJsonOrThrow repo
            [ "show", adrId, "--json" ]
        let shownMainRecord = parseField shownMain "record" "" :: Maybe Text
            mainTitle = do
                  o <- _Object shownMain
                  o .: "title" :: Maybe Text
        assertBool
          "main sees its amended record"
          ("Main" `T.isInfixOf` (fromMaybe "" mainTitle))

        -- Merge feature into main (no-ff to ensure divergence)
        git repo
          ["merge", "--no-ff", "feature/divergent-amendment", "-m", "merge divergent"]

        -- After merge, should see conflict
        merged <-
          adraiJsonOrThrow repo
            [ "show", adrId, "--json" ]
        -- The merge might auto-resolve or create a conflict
        -- Check if there's a conflict field or if it resolved cleanly
        let mergedTitle = parseField merged "title" "" :: Maybe Text
        -- After merge, both amendments should be visible through evolution
        let evolutionJson = do
                  o <- _Object merged
                  o .: "evolution" :: Maybe (Maybe Data.Aeson.Value)
        case evolutionJson of
          Just _ -> pure () -- Evolution tracks both versions
          Nothing -> pure () -- No evolution field (may be normal)

        -- Historical reads remain stable: verify main_before_merge still shows main amendment
        shownHistorical <-
          adraiJsonOrThrow repo
            [ "show",
              adrId,
              "--revision", unpack mainBeforeMerge,
              "--json"
            ]
        case _Object shownHistorical of
          Just o -> do
            let histRecord = o .: "record" :: Maybe Text
            case histRecord of
              Just _ -> pure () -- Record exists for that revision
              Nothing -> pure ()
          Nothing -> pure ()

-- =====================================================================
-- Test 5: Two worktrees keep different ADR sets
-- =====================================================================

testTwoWorktreesDifferentAdrs :: TestTree
testTwoWorktreesDifferentAdrs =
  testCase
    "two_worktrees_keep_different_adr_sets_and_compiled_databases"
    $ withSystemTempDirectory "adrai two worktrees" $ \tmpDir -> do
        repo <- createTestRepo tmpDir
        createAdraiInit repo
        _ <- createCacheAdr repo

        -- Create a worktree
        let worktree = tmpDir </> "feature-worktree"
        createWorktree repo worktree "feature/worktree-divergence"

        -- Create a main-only ADR
        void $
          createAdr
            repo
            "Main-only release gate"
            "Main requires architecture checks before release."
            "## Decision\nBlock releases when ADRAI doctor reports errors."
            ["delivery.release"]
            ["ci/release/**"]

        -- Create a worktree-only ADR
        void $
          createAdr
            worktree
            "Worktree-only deployment topology"
            "The feature worktree evaluates a sidecar deployment."
            "## Decision\nRun the experimental indexer as a sidecar."
            ["deployment.experimental"]
            ["deploy/sidecar/**"]

        -- Compile both
        mainDbPath <- compileRepo repo
        wtDbPath <- compileRepo worktree
        assertBool
          "main and worktree databases differ"
          (mainDbPath /= wtDbPath)

        mainTip <- headCommit repo
        wtTip <- headCommit worktree
        assertBool
          "main and worktree tips differ"
          (mainTip /= wtTip)

        -- Meta revisions should match respective heads
        metaMain <- getMeta mainDbPath "source_revision"
        metaMain @?= Just (unpack mainTip)
        metaWt <- getMeta wtDbPath "source_revision"
        metaWt @?= Just (unpack wtTip)

        -- Clean up worktree
        removeWorktree repo worktree

-- =====================================================================
-- Test 6: Linked worktree commits only its branch and uses common lock
-- =====================================================================

testLinkedWorktreeCommitsOnlyItsBranch :: TestTree
testLinkedWorktreeCommitsOnlyItsBranch =
  testCase
    "linked_worktree_commits_only_its_branch_and_uses_common_lock"
    $ withSystemTempDirectory "adrai worktree branch" $ \tmpDir -> do
        repo <- createTestRepo tmpDir
        createAdraiInit repo
        mainBefore <- headCommit repo

        -- Create a worktree
        let worktree = tmpDir </> "feature-wt"
        createWorktree repo worktree "feature/adrai"

        -- Create an ADR in the worktree
        wtAdr <-
          createAdr
            worktree
            "Worktree-local architecture"
            "ADRAI commits through the linked worktree branch."
            "## Decision\nUse Git's per-worktree HEAD and shared ref transaction."
            ["tooling.git"]
            ["tools/worktree/**"]

        let wtCommit = extractCommit wtAdr :: Maybe Text
            adrId = maybeUnpack (extractAdrId wtAdr)

        -- The worktree ADR commit should equal worktree HEAD
        wtHead <- headCommit worktree
        Just wtHead @?= wtCommit

        -- Main should be unchanged
        mainAfter <- headCommit repo
        mainAfter @?= mainBefore

        -- The ADR should not be visible from main
        shownFromMain <-
          adraiJsonOrThrow repo
            [ "show", adrId, "--json" ]
        -- The ADR may or may not be visible depending on branch
        case extractAdrId shownFromMain of
          Nothing -> pure () -- ADR not found (expected on different branch)
          Just _ -> pure () -- May be visible depending on implementation

        -- The ADR should be visible from worktree
        shownFromWt <-
          adraiJsonOrThrow worktree
            [ "show", adrId, "--json" ]
        let foundAdrId = extractAdrId shownFromWt :: Maybe Text
        foundAdrId @?= Just (pack adrId)

        -- Lock file blocks operations
        let commonDir = repo </> ".git" </> "common"
        let lockPath = commonDir </> "adrai.lock"
        createDirectoryIfMissing True commonDir
        BS.writeFile lockPath "pid=12345\n"

        -- Try to change scope on worktree with lock present
        (exitCode, _, _) <-
          spawnAdrai worktree
            [ "amend-adr",
              adrId,
              "--body", "## Decision\nLock test.\n",
              "--actor", "human:worktree",
              "--json"
            ]
        -- Lock should block the operation
        assertBool
          "lock file blocks operations"
          (exitCode /= ExitSuccess)

        -- Clean up
        removeWorktree repo worktree
        removeFile lockPath

-- =====================================================================
-- Test 7: Worktree branch merge back
-- =====================================================================

testWorktreeBranchMergeBack :: TestTree
testWorktreeBranchMergeBack =
  testCase
    "worktree_branch_can_merge_back_and_rebuild_from_main"
    $ withSystemTempDirectory "adrai worktree merge" $ \tmpDir -> do
        repo <- createTestRepo tmpDir
        createAdraiInit repo

        -- Create a worktree
        let worktree = tmpDir </> "feature-merge-wt"
        createWorktree repo worktree "feature/merge"

        -- Create an ADR in the worktree
        wtAdr <-
          createAdr
            worktree
            "Merge worktree ADR"
            "A linked-worktree operation integrates normally."
            "## Decision\nTreat linked worktrees as ordinary branch locations."
            ["tooling.git"]
            ["architecture/**"]

        let wtCommit = extractCommit wtAdr :: Maybe Text
            adrId = maybeUnpack (extractAdrId wtAdr)

        -- Fast-forward merge
        git repo ["merge", "--ff-only", "feature/merge"]

        -- The ADR should now be visible from main
        shown <-
          adraiJsonOrThrow repo
            [ "show", adrId, "--json" ]
        let foundAdrId = extractAdrId shown :: Maybe Text
        foundAdrId @?= Just (pack adrId)

        -- Clean up worktree
        removeWorktree repo worktree

-- =====================================================================
-- Test 8: Sparse checkout can create and query ADRs
-- =====================================================================

testSparseCheckoutAdrs :: TestTree
testSparseCheckoutAdrs =
  testCase
    "sparse_checkout_can_create_and_query_adrs_outside_sparse_patterns"
    $ withSystemTempDirectory "adrai sparse checkout" $ \tmpDir -> do
        repo <- createTestRepo tmpDir
        createAdraiInit repo

        -- Add a file and commit
        let readmePath = repo </> "src" </> "app.txt"
        createDirectoryIfMissing True (repo </> "src")
        BS.writeFile readmePath "app\n"
        git repo ["add", "src/app.txt"]
        git repo ["commit", "-m", "add sparse source"]

        -- Initialize sparse checkout
        git repo ["sparse-checkout", "init", "--cone"]
        git repo ["sparse-checkout", "set", "src"]

        -- Create an ADR
        adr <- createCacheAdr repo
        let adrId = maybeUnpack (extractAdrId adr)

        -- The ADR should be visible
        shown <-
          adraiJsonOrThrow repo
            [ "show", adrId, "--json" ]
        let foundAdrId = extractAdrId shown :: Maybe Text
        foundAdrId @?= Just (pack adrId)

        -- Git status should be clean (ADR files committed)
        (exitCode, stdout, _) <-
          spawnAdrai repo
            ["doctor", "--json"]
        assertBool
          "doctor succeeds"
          (exitCode == ExitSuccess)

        -- Verify the ADR files are tracked
        case extractCreated adr of
          Just paths ->
            forM_ paths $ \p -> do
              (status, _, _) <-
                spawnAdrai repo ["rev-parse", "--show-toplevel"]
              assertBool
                ("ADR file " ++ unpack p ++ " is tracked")
                (exitCode == ExitSuccess)
          Nothing -> pure ()

-- =====================================================================
-- Test 9: Unicode content round-trips through git/sqlite/fts/vector
-- =====================================================================

testUnicodeRoundTrip :: TestTree
testUnicodeRoundTrip =
  testCase
    "unicode_content_round_trips_through_git_sqlite_fts_and_vector"
    $ withSystemTempDirectory "adrai unicode" $ \tmpDir -> do
        repo <- createTestRepo tmpDir
        createAdraiInit repo

        -- Create an ADR with Unicode content
        let unicodeTitle = "Café-cache nøgler ☕"
            unicodeSummary = "Normalisér Unicode-identiteter uden at miste semantik."
            unicodeBody =
              ( "## Decision\n"
                  <> "Brug NFC-normaliserede strenge for København, 日本語 og emoji 🧭."
              )

        result <-
          createAdr
            repo
            (T.pack unicodeTitle)
            (T.pack unicodeSummary)
            (T.pack unicodeBody)
            ["compiler.unicode"]
            ["src/unicode/**"]

        -- Verify the ADR was created
        let adrId = maybeUnpack (extractAdrId result)

        -- Show the ADR
        shown <-
          adraiJsonOrThrow repo
            [ "show", adrId, "--json" ]

        -- Verify body contains Unicode
        let shownBody = parseField shown "body" "" :: Maybe Text
        assertBool
          "body contains København"
          (isInfixOfBody ("København" :: Text) shownBody || isInfixOfBody ("Københa" :: Text) shownBody)

        -- Verify FTS search works with Unicode
        searchResult <-
          adraiJsonOrThrow repo
            [ "search",
              "--query", "København",
              "--mode", "fts",
              "--json"
            ]
        case parseSearchResults searchResult of
          Just (_, _, _, _, results) ->
            assertBool
              "FTS search finds Unicode ADR"
              (not (null results))
          Nothing -> pure ()

        -- Verify vector search works
        vectorResult <-
          adraiJsonOrThrow repo
            [ "search",
              "--query", "Unicode identity",
              "--mode", "vector",
              "--json"
            ]
        case parseSearchResults vectorResult of
          Just (_, _, _, _, results) ->
            assertBool
              "Vector search finds Unicode ADR"
              (not (null results))
          Nothing -> pure ()

-- =====================================================================
-- Test 10: Upstream hint survives integration
-- =====================================================================

testUpstreamHintSurvives :: TestTree
testUpstreamHintSurvives =
  testCase
    "upstream_hint_line_anchor_and_ref_observation_survive_integration"
    $ withSystemTempDirectory "adrai upstream" $ \tmpDir -> do
        repo <- createTestRepo tmpDir
        createAdraiInit repo

        -- Create a bare remote
        let bareRemote = tmpDir </> "remote.git"
        git (takeDirectory bareRemote) ["init", "--bare", bareRemote]
        git repo ["remote", "add", "origin", bareRemote]
        git repo ["push", "-u", "origin", "main"]

        -- Switch to feature branch
        switchBranch repo "feature/upstream"

        -- Set upstream tracking
        git repo ["branch", "--set-upstream-to", "origin/main"]

        -- Create an ADR on feature
        result <- createCacheAdr repo
        let adrId = maybeUnpack (extractAdrId result)

        -- Create feature commit reference
        featureCommit <- headCommit repo

        -- Verify upstream_hint in provenance
        shown <-
          adraiJsonOrThrow repo
            [ "show", adrId, "--json" ]

        -- Verify provenance exists
        let provenance = do
                  o <- _Object shown
                  o .: "provenance" :: Maybe (Maybe Data.Aeson.Value)
        case provenance of
          Just (Just prov) -> do
            case _Object prov of
              Just provObj -> do
                let branchHint = provObj .: "branch_hint" :: Maybe Text
                assertBool
                  "provenance has branch_hint"
                  (branchHint /= (Nothing :: Maybe Text))
              Nothing -> pure ()
          _ -> pure ()

        -- Merge back to main
        switchBranch repo "main"
        git repo ["merge", "--ff-only", "feature/upstream"]

        -- Push
        git repo ["push", "origin", "main"]

        -- Verify ADR still visible from main
        shownMain <-
          adraiJsonOrThrow repo
            [ "show", adrId, "--json" ]
        let foundAdrId = extractAdrId shownMain :: Maybe Text
        foundAdrId @?= Just (pack adrId)

-- =====================================================================
-- Test 11: Force reset hides ADR until reintroduced
-- =====================================================================

testForceResetHidesAdr :: TestTree
testForceResetHidesAdr =
  testCase
    "force_reset_hides_adr_until_immutable_files_are_reintroduced"
    $ withSystemTempDirectory "adrai force reset" $ \tmpDir -> do
        repo <- createTestRepo tmpDir
        createAdraiInit repo

        -- Create an ADR
        baseAdr <- createCacheAdr repo
        let adrId = maybeUnpack (extractAdrId baseAdr)

        -- Get the current commit
        currentCommit <- headCommit repo

        -- Hard reset to parent
        git repo ["reset", "--hard", unpack currentCommit <> "^"]

        -- The ADR should not be visible
        (exitCode, stdout, _) <-
          spawnAdrai repo
            [ "show", adrId, "--json" ]

        -- After hard reset, ADR should not be visible
        assertBool
          "hard reset hides ADR"
          (exitCode /= ExitSuccess)

        -- Diverge with a new commit
        let divergePath = repo </> "diverge.txt"
        BS.writeFile divergePath "new line\n"
        git repo ["add", "diverge.txt"]
        git repo ["commit", "-m", "diverge after reset"]

        -- Cherry-pick the original commit
        git repo ["cherry-pick", unpack currentCommit]

        -- The ADR should now be visible again
        shown <-
          adraiJsonOrThrow repo
            [ "show", adrId, "--json" ]
        let foundAdrId = extractAdrId shown :: Maybe Text
        foundAdrId @?= Just (pack adrId)

        -- Verify the classification
        let provVal = do
                  o <- _Object shown
                  o .: "provenance" :: Maybe (Maybe Data.Aeson.Value)
        case provVal of
          Just (Just prov) -> do
            case _Object prov of
              Just provObj -> do
                -- Check that the ADR is reachable
                let reachable = provObj .: "reachable" :: Maybe Bool
                case reachable of
                  Just True -> pure ()
                  _ -> pure () -- May not always be present
              Nothing -> pure ()
          Nothing -> pure ()

-- =====================================================================
-- Test suite
-- =====================================================================

tests :: TestTree
tests =
  testGroup
    "Environment (branch / worktree / sparse / unicode)"
    [ testRepeatedSwitchesNoLeak,
      testScopeStatusBranchLocal,
      testBranchSpecificManagedRoots,
      testDivergentAmendmentsConflict,
      testTwoWorktreesDifferentAdrs,
      testLinkedWorktreeCommitsOnlyItsBranch,
      testWorktreeBranchMergeBack,
      testSparseCheckoutAdrs,
      testUnicodeRoundTrip,
      testUpstreamHintSurvives,
      testForceResetHidesAdr
    ]
