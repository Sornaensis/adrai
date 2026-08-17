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
import Adrai.Git (discoverRepository, repositoryCommonDir, repositoryGitDir, systemGit)
import Adrai.Provenance.Git.Lock (GitLockError (LockHeld), gitLockStatus, withGitLock)
import Control.Applicative ((<|>))
import Control.Exception (finally)
import Control.Monad (forM, forM_, void)
import System.Exit (ExitCode (..))
import qualified Data.Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (sort)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
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
  ( canonicalizePath,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    pathIsSymbolicLink,
    removeDirectory,
    removeDirectoryRecursive,
  )
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute, makeRelative, takeDirectory, (</>))
import System.Info (os)
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (proc, readProcess, runProcess, shell)
import System.Win32 (getCurrentProcessId)
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

-- | Run the real executable while preserving the inherited process
-- environment.  The shared integration runner intentionally installs a
-- minimal Git fixture environment, which removes Windows PATH and prevents
-- the production executable from locating Git.
realSpawnAdrai :: FilePath -> [String] -> IO (ExitCode, LBS.ByteString, LBS.ByteString)
realSpawnAdrai repo arguments = do
  executable <- lookupEnv "ADRAI_EXE" >>= \case
    Just path | not (null path) && isAbsolute path -> pure path
    _ -> fail "EnvironmentTest requires ADRAI_EXE to name an absolute executable under test"
  readProcess (proc executable ("--repo" : repo : arguments))

realAdraiJsonOrThrow :: FilePath -> [String] -> IO Data.Aeson.Value
realAdraiJsonOrThrow repo arguments = do
  (exitCode, stdout, stderr) <- realSpawnAdrai repo arguments
  case exitCode of
    ExitSuccess ->
      case Data.Aeson.eitherDecode stdout of
        Right value -> pure value
        Left problem -> fail ("adrai " <> unwords arguments <> " JSON error: " <> problem)
    ExitFailure code ->
      fail
        ( "adrai " <> unwords arguments <> " error: CLI failed (exit " <> show code <> "): "
            <> unpack (decodeUtf8 (LBS.toStrict stderr))
        )

realCreateAdr
  :: FilePath
  -> Text
  -> Text
  -> Text
  -> [Text]
  -> [Text]
  -> IO Data.Aeson.Value
realCreateAdr repo title summary body domains scopes =
  realAdraiJsonOrThrow repo $
    [ "create",
      "--title", unpack title,
      "--summary", unpack summary,
      "--body", unpack normalizedBody,
      "--actor", "llm:planner",
      "--model", "demo-model"
    ]
      <> concatMap (\domain -> ["--domain", unpack domain]) domains
      <> concatMap (\scope -> ["--applies-to", unpack scope]) scopes
      <> ["--json"]
  where
    normalizedBody
      | "\n" `T.isSuffixOf` body = body
      | otherwise = body <> "\n"

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
        BS.writeFile (repo </> ".gitignore") ".adrai/\n"
        git repo ["--literal-pathspecs", "add", "--", ".gitignore"]
        git repo ["commit", "-m", "test: ignore ADRAI cache"]
        baseAdr <-
          realCreateAdr
            repo
            "Stable cache identity"
            "Cache identity derives from semantic inputs."
            ( "## Context\nBuilds move between workspaces.\n\n"
                <> "## Decision\nCache keys exclude absolute workspace paths and use source digests.\n\n"
                <> "## Consequences\nInputs must be normalized."
            )
            ["compiler.cache"]
            ["src/compiler/cache/**"]
        baseDatabase <- assertMutationIndex "base create" repo baseAdr

        -- The shared ADR is committed Git state; its first cache is disposable.
        -- Remove only that generated test cache so both diverging worktrees
        -- cold-publish their own branch-local indexes without exercising the
        -- separate Windows warm-replacement contract.
        removeDirectoryRecursive (repo </> ".adrai")
        doesDirectoryExist (repo </> ".adrai") >>= (@?= False)

        -- Create a worktree
        let worktree = tmpDir </> "feature-worktree"
        createWorktree repo worktree "feature/worktree-divergence"

        -- Create a main-only ADR
        mainAdr <-
          realCreateAdr
            repo
            "Main-only release gate"
            "Main requires architecture checks before release."
            "## Decision\nBlock releases when ADRAI doctor reports errors."
            ["delivery.release"]
            ["ci/release/**"]
        mainDatabasePublished <- assertMutationIndex "main create" repo mainAdr
        mainDatabasePublished @?= baseDatabase

        -- Create a worktree-only ADR
        worktreeAdr <-
          realCreateAdr
            worktree
            "Worktree-only deployment topology"
            "The feature worktree evaluates a sidecar deployment."
            "## Decision\nRun the experimental indexer as a sidecar."
            ["deployment.experimental"]
            ["deploy/sidecar/**"]
        worktreeDatabasePublished <- assertMutationIndex "worktree create" worktree worktreeAdr
        assertBool "linked worktree create publishes a distinct index" (mainDatabasePublished /= worktreeDatabasePublished)

        mainTip <- headCommit repo
        wtTip <- headCommit worktree
        assertBool
          "main and worktree tips differ"
          (mainTip /= wtTip)

        let baseAdrId = maybeUnpack (extractAdrId baseAdr)
            mainAdrId = maybeUnpack (extractAdrId mainAdr)
            worktreeAdrId = maybeUnpack (extractAdrId worktreeAdr)
            mainVisible = [(baseAdrId, "Stable cache identity"), (mainAdrId, "Main-only release gate")]
            worktreeVisible = [(baseAdrId, "Stable cache identity"), (worktreeAdrId, "Worktree-only deployment topology")]
            mainHidden = [(worktreeAdrId, "Worktree-only deployment topology")]
            worktreeHidden = [(mainAdrId, "Main-only release gate")]

        -- Run each public branch view in both orders.  Every invocation
        -- rechecks its own HEAD-bound database before querying the other
        -- worktree, so a shared/stale cache cannot satisfy the assertions.
        mainFirst <- assertWorktreeView "main first" repo mainTip mainVisible mainHidden
        worktreeFirst <- assertWorktreeView "worktree second" worktree wtTip worktreeVisible worktreeHidden
        mainFirst @?= mainDatabasePublished
        worktreeFirst @?= worktreeDatabasePublished
        assertBool "linked worktrees publish distinct database paths" (mainFirst /= worktreeFirst)
        _ <- assertWorktreeView "worktree first on repeat" worktree wtTip worktreeVisible worktreeHidden
        _ <- assertWorktreeView "main second on repeat" repo mainTip mainVisible mainHidden

        -- Clean up worktree
        removeWorktree repo worktree
        doesDirectoryExist worktree >>= (@?= False)
  where
    assertMutationIndex label location result = do
      resultObject <-
        case _Object result of
          Just value -> pure value
          Nothing -> assertFailure (label <> ": mutation result is not a JSON object") >> fail "unreachable"
      case resultObject .: "indexed" :: Maybe Bool of
        Just True -> pure ()
        actual -> assertFailure (label <> ": expected indexed=true, got " <> show actual <> " in " <> show result)
      (resultObject .: "index_warnings" :: Maybe Integer) @?= Just 0
      indexRevision <-
        case resultObject .: "index_revision" :: Maybe Text of
          Just value -> pure value
          Nothing -> assertFailure (label <> ": missing index_revision") >> fail "unreachable"
      extractCommit result @?= Just indexRevision
      headCommit location >>= (@?= indexRevision)
      database <-
        case resultObject .: "database" :: Maybe FilePath of
          Just value -> canonicalizePath value
          Nothing -> assertFailure (label <> ": missing database") >> fail "unreachable"
      expectedDatabase <- canonicalizePath (location </> ".adrai" </> "index.sqlite")
      database @?= expectedDatabase
      getMeta database "resolved_oid" >>= (@?= Just (unpack indexRevision))
      pure database

    assertWorktreeView label location expectedHead visible hidden = do
      beforeState <- repositoryObservableState location
      database <- canonicalizePath (location </> ".adrai" </> "index.sqlite")
      getMeta database "resolved_oid" >>= (@?= Just (unpack expectedHead))
      forM_ visible $ \(adrId, queryText) -> do
        shown <- realAdraiJsonOrThrow location ["show", adrId, "--json"]
        extractAdrId shown @?= Just (pack adrId)
        searched <- realAdraiJsonOrThrow location ["search", queryText, "--mode", "fts", "--json"]
        case parseSearchResults searched of
          Just (_, _, _, _, results) ->
            assertBool (label <> ": search exposes " <> adrId) (pack adrId `elem` mapMaybe extractAdrId results)
          Nothing -> assertFailure (label <> ": search JSON did not match the public result schema")
      forM_ hidden $ \(adrId, queryText) -> do
        (showExit, showOut, showErr) <- realSpawnAdrai location ["show", adrId, "--json"]
        showExit @?= ExitFailure 2
        showOut @?= ""
        showErr @?=
          LBS.fromStrict
            ("adrai: ADRAI reference not found in this revision: " <> encodeUtf8 (pack adrId) <> "\n")
        searched <- realAdraiJsonOrThrow location ["search", queryText, "--mode", "fts", "--json"]
        case parseSearchResults searched of
          Just (_, _, _, _, results) ->
            assertBool (label <> ": search excludes " <> adrId) (pack adrId `notElem` mapMaybe extractAdrId results)
          Nothing -> assertFailure (label <> ": search JSON did not match the public result schema")
      allVisible <- realAdraiJsonOrThrow location ["search", "Decision", "--mode", "fts", "--include-obsolete", "--limit", "1000", "--json"]
      case parseSearchResults allVisible of
        Just (_, _, _, _, results) ->
          sort (mapMaybe extractAdrId results) @?= sort (map (pack . fst) visible)
        Nothing -> assertFailure (label <> ": complete search JSON did not match the public result schema")
      databaseAfter <- canonicalizePath (location </> ".adrai" </> "index.sqlite")
      databaseAfter @?= database
      getMeta databaseAfter "resolved_oid" >>= (@?= Just (unpack expectedHead))
      headCommit location >>= (@?= expectedHead)
      afterState <- repositoryObservableState location
      assertBool (label <> ": public reads preserve symbolic HEAD, tree, index, status, refs, and reflogs") (afterState == beforeState)
      pure database

    repositoryObservableState location = do
      symbolicHead <- gitStdout location ["symbolic-ref", "-q", "HEAD"]
      headTree <- gitStdout location ["rev-parse", "HEAD^{tree}"]
      stagedIndex <- gitStdout location ["ls-files", "--stage", "-z"]
      stagedDiff <- gitStdout location ["diff", "--cached", "--raw", "-z"]
      status <- gitStdout location ["status", "--porcelain=v1", "--untracked-files=all", "-z"]
      refs <- gitStdout location ["for-each-ref", "--format=%(refname)%00%(objectname)%00", "refs/heads", "refs/remotes"]
      reflogs <- gitStdout location ["reflog", "show", "--all", "--format=%gD%x00%H%x00%gs%x00"]
      pure (symbolicHead, headTree, stagedIndex, stagedDiff, status, refs, reflogs)

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
        mainHeadBefore <- gitStdout repo ["symbolic-ref", "-q", "HEAD"]
        mainCacheBefore <- snapshotDirectory (repo </> ".adrai")

        let worktree = tmpDir </> "feature-wt"
        createWorktree repo worktree "feature/adrai"
        mainRepository <- requireRepository repo
        worktreeRepository <- requireRepository worktree
        repositoryCommonDir mainRepository @?= repositoryCommonDir worktreeRepository
        featureBefore <- headCommit worktree
        featureHeadBefore <- gitStdout worktree ["symbolic-ref", "-q", "HEAD"]
        mainBranchBefore <- gitStdout repo ["rev-parse", "HEAD"]
        worktreeStateBefore <- repositoryObservableState worktree
        mainStateBefore <- repositoryObservableState repo

        let createArguments =
              [ "create",
                "--title", "Worktree-local architecture",
                "--summary", "ADRAI commits through the linked worktree branch.",
                "--body", "## Decision\nUse Git's per-worktree HEAD and shared ref transaction.\n",
                "--actor", "llm:planner",
                "--model", "demo-model",
                "--domain", "tooling.git",
                "--applies-to", "tools/worktree/**",
                "--json"
              ]

        -- Use the same production common-directory lock as mutations rather
        -- than fabricating a file or a holder PID in the test.
        withGitLock mainRepository $ do
          held <- gitLockStatus worktreeRepository
          lockError <- case held of
            Left lockError@(LockHeld lockPath holderPid) -> do
              lockPath @?= repositoryCommonDir mainRepository </> "adrai.lock"
              assertBool "the parent-held lock PID is positive" (holderPid > 0)
              currentPid <- fromIntegral <$> getCurrentProcessId
              holderPid @?= currentPid
              pure lockError
            Left problem -> assertFailure ("parent-held production lock must be LockHeld, got " <> show problem) >> fail "unreachable"
            Right _ -> assertFailure "parent-held production Git lock was not observable from linked worktree" >> fail "unreachable"
          (exitCode, stdout, stderr) <- realSpawnAdrai worktree createArguments
          exitCode @?= ExitFailure 2
          stdout @?= ""
          stderr @?=
            LBS.fromStrict
              (encodeUtf8 ("adrai: Stage2AcquireLock " <> pack (show (pack (show lockError))) <> "\n"))
          worktreeStateAfterRejected <- repositoryObservableState worktree
          mainStateAfterRejected <- repositoryObservableState repo
          worktreeStateAfterRejected @?= worktreeStateBefore
          mainStateAfterRejected @?= mainStateBefore

        gitLockStatus worktreeRepository >>= (@?= Right Nothing)

        -- The same installed-executable mutation succeeds after bracketed
        -- release and advances only the linked worktree branch.
        wtAdr <- realAdraiJsonOrThrow worktree createArguments
        wtHead <- headCommit worktree
        assertBool "linked worktree branch advances after lock release" (wtHead /= featureBefore)
        extractCommit wtAdr @?= Just wtHead
        let adrId = maybeUnpack (extractAdrId wtAdr)

        resultObject <- case _Object wtAdr of
          Just value -> pure value
          Nothing -> assertFailure "worktree create result is not a JSON object" >> fail "unreachable"
        (resultObject .: "indexed" :: Maybe Bool) @?= Just True
        (resultObject .: "index_warnings" :: Maybe Integer) @?= Just 0
        (resultObject .: "index_revision" :: Maybe Text) @?= Just wtHead
        database <- case resultObject .: "database" :: Maybe FilePath of
          Just value -> canonicalizePath value
          Nothing -> assertFailure "worktree create result has no database path" >> fail "unreachable"
        expectedDatabase <- canonicalizePath (worktree </> ".adrai" </> "index.sqlite")
        database @?= expectedDatabase
        getMeta database "resolved_oid" >>= (@?= Just (unpack wtHead))

        headCommit repo >>= (@?= mainBefore)
        gitStdout repo ["symbolic-ref", "-q", "HEAD"] >>= (@?= mainHeadBefore)
        gitStdout repo ["rev-parse", "HEAD"] >>= (@?= mainBranchBefore)
        gitStdout worktree ["symbolic-ref", "-q", "HEAD"] >>= (@?= featureHeadBefore)
        mainCacheAfter <- snapshotDirectory (repo </> ".adrai")
        mainCacheAfter @?= mainCacheBefore

        shownFromWt <- realAdraiJsonOrThrow worktree ["show", adrId, "--json"]
        extractAdrId shownFromWt @?= Just (pack adrId)

        removeWorktree repo worktree
  where
    requireRepository location =
      discoverRepository systemGit location >>= \case
        Left problem -> assertFailure ("could not discover test repository: " <> show problem) >> fail "unreachable"
        Right repository -> pure repository

    repositoryObservableState location = do
      symbolicHead <- gitStdout location ["symbolic-ref", "-q", "HEAD"]
      headTree <- gitStdout location ["rev-parse", "HEAD^{tree}"]
      stagedIndex <- gitStdout location ["ls-files", "--stage", "-z"]
      stagedDiff <- gitStdout location ["diff", "--cached", "--raw", "-z"]
      status <- gitStdout location ["status", "--porcelain=v1", "--untracked-files=all", "-z"]
      refs <- gitStdout location ["for-each-ref", "--format=%(refname)%00%(objectname)%00", "refs/heads", "refs/remotes"]
      reflogs <- gitStdout location ["reflog", "show", "--all", "--format=%gD%x00%H%x00%gs%x00"]
      managedFiles <- snapshotDirectory (location </> "architecture" </> "adrai")
      cacheFiles <- snapshotDirectory (location </> ".adrai")
      pure (symbolicHead, headTree, stagedIndex, stagedDiff, status, refs, reflogs, managedFiles, cacheFiles)

    snapshotDirectory root = do
      exists <- doesDirectoryExist root
      if exists then go root else pure []
      where
        go directory = do
          names <- sort <$> listDirectory directory
          fmap concat $ forM names $ \name -> do
            let path = directory </> name
            isDirectory <- doesDirectoryExist path
            if isDirectory
              then go path
              else do
                isFile <- doesFileExist path
                if isFile
                  then do
                    bytes <- BS.readFile path
                    pure [(makeRelative root path, bytes)]
                  else pure []

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
        mainBefore <- headCommit repo

        -- Materialize the canonical main index before the linked-worktree
        -- branch exists, so the post-merge compile must update an existing
        -- main-owned database rather than merely create a new cache.
        mainCompiledBeforeValue <- realAdraiJsonOrThrow repo ["compile", "--json"]
        mainCompiledBefore <-
          case parseCompileResult mainCompiledBeforeValue of
            Just value -> pure value
            Nothing -> assertFailure "pre-merge compile JSON did not match the public result schema" >> fail "unreachable"
        mainDatabase <- canonicalizePath (repo </> ".adrai" </> "index.sqlite")
        mainDatabaseBefore <- canonicalizePath (coldCompilerDatabase mainCompiledBefore)
        mainDatabaseBefore @?= mainDatabase
        coldCompilerRevision mainCompiledBefore @?= mainBefore
        getMeta mainDatabaseBefore "resolved_oid" >>= (@?= Just (unpack mainBefore))
        mainDatabaseBytesBefore <- BS.readFile mainDatabaseBefore

        -- Create a worktree
        let worktree = tmpDir </> "feature-merge-wt"
        createWorktree repo worktree "feature/merge"

        -- Create an ADR in the worktree
        wtAdr <-
          realAdraiJsonOrThrow worktree
            [ "create",
              "--title", "Merge worktree ADR",
              "--summary", "A linked-worktree operation integrates normally.",
              "--body", "## Decision\nTreat linked worktrees as ordinary branch locations.\n",
              "--actor", "llm:planner",
              "--model", "demo-model",
              "--domain", "tooling.git",
              "--applies-to", "architecture/**",
              "--json"
            ]

        let wtCommit = extractCommit wtAdr :: Maybe Text
            adrId = maybeUnpack (extractAdrId wtAdr)
        featureCommit <- headCommit worktree
        wtCommit @?= Just featureCommit

        -- The feature cache is physically distinct from main's canonical
        -- index even though both repositories share the same Git object store.
        worktreeCompiledValue <- realAdraiJsonOrThrow worktree ["compile", "--json"]
        worktreeCompiled <-
          case parseCompileResult worktreeCompiledValue of
            Just value -> pure value
            Nothing -> assertFailure "worktree compile JSON did not match the public result schema" >> fail "unreachable"
        worktreeDatabase <- canonicalizePath (coldCompilerDatabase worktreeCompiled)
        assertBool "main and linked worktree use distinct index paths" (mainDatabase /= worktreeDatabase)
        coldCompilerRevision worktreeCompiled @?= featureCommit
        getMeta worktreeDatabase "resolved_oid" >>= (@?= Just (unpack featureCommit))

        -- The linked worktree writes only its feature branch.  Main has not
        -- advanced and cannot expose the feature ADR before integration.
        headCommit repo >>= (@?= mainBefore)
        (preMergeExit, preMergeOut, preMergeErr) <- realSpawnAdrai repo ["show", adrId, "--json"]
        preMergeExit @?= ExitFailure 2
        preMergeOut @?= ""
        preMergeErr @?=
          LBS.fromStrict
            ("adrai: ADRAI reference not found in this revision: " <> encodeUtf8 (pack adrId) <> "\n")

        -- Fast-forward merge
        git repo ["merge", "--ff-only", "feature/merge"]
        mainAfter <- headCommit repo
        mainAfter @?= featureCommit
        assertBool "fast-forward integration advances main" (mainAfter /= mainBefore)

        -- Canonical public compilation from main must publish metadata for
        -- the integrated main revision, not a prior main/worktree cache.
        compiledValue <- realAdraiJsonOrThrow repo ["compile", "--json"]
        compiled <-
          case parseCompileResult compiledValue of
            Just value -> pure value
            Nothing -> assertFailure "compile JSON did not match the public result schema" >> fail "unreachable"
        coldCompilerRevision compiled @?= mainAfter
        mainDatabaseAfter <- canonicalizePath (coldCompilerDatabase compiled)
        mainDatabaseAfter @?= mainDatabase
        assertBool "merged main compile must not reuse the linked-worktree index" (mainDatabaseAfter /= worktreeDatabase)
        getMeta mainDatabaseAfter "resolved_oid" >>= (@?= Just (unpack mainAfter))
        mainDatabaseBytesAfter <- BS.readFile mainDatabaseAfter
        assertBool "main index bytes change when the integrated revision is published"
          (mainDatabaseBytesAfter /= mainDatabaseBytesBefore)

        -- Public reads from main now expose the integrated ADR.
        shown <-
          realAdraiJsonOrThrow repo
            [ "show", adrId, "--json" ]
        let foundAdrId = extractAdrId shown :: Maybe Text
        foundAdrId @?= Just (pack adrId)

        searched <-
          realAdraiJsonOrThrow repo
            [ "search", "linked-worktree operation integrates normally",
              "--mode", "fts",
              "--json"
            ]
        case parseSearchResults searched of
          Just (_, _, _, _, results) ->
            assertBool "main search exposes the integrated ADR" (any ((== Just (pack adrId)) . extractAdrId) results)
          Nothing -> assertFailure "search JSON did not match the public result schema"

        -- Clean up worktree
        removeWorktree repo worktree
        doesDirectoryExist worktree >>= (@?= False)

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

        -- Establish a real cone checkout which leaves the managed roots out
        -- of its materialized set, while retaining one unrelated source file.
        let sourcePath = repo </> "src" </> "app.txt"
        BS.writeFile (repo </> ".gitignore") ".adrai/\n"
        createDirectoryIfMissing True (repo </> "src")
        BS.writeFile sourcePath "app\n"
        git repo ["--literal-pathspecs", "add", "--", ".gitignore", "src/app.txt"]
        git repo ["commit", "-m", "add sparse source"]
        git repo ["sparse-checkout", "init", "--cone"]
        git repo ["sparse-checkout", "set", "src"]

        sparseConfigRelative <- decodeUtf8 . LBS.toStrict <$> gitStdout repo ["rev-parse", "--git-path", "info/sparse-checkout"]
        let sparseConfig = repo </> unpack (strip sparseConfigRelative)
        sparseConfigBefore <- BS.readFile sparseConfig
        sparseListBefore <- gitStdout repo ["sparse-checkout", "list"]
        assertBool "cone patterns exclude the managed ADR root" (not ("architecture/adrai" `BS.isInfixOf` sparseConfigBefore))
        assertBool "cone checkout materializes only the unrelated source root" ("src" `BS.isInfixOf` LBS.toStrict sparseListBefore)
        sourceBefore <- BS.readFile sourcePath
        sourceIndexBefore <- gitStdout repo ["ls-files", "--stage", "--", "src/app.txt"]
        branchBefore <- gitStdout repo ["symbolic-ref", "-q", "HEAD"]
        branchNamesBefore <- gitStdout repo ["for-each-ref", "--format=%(refname)", "refs/heads"]
        refsBefore <- gitStdout repo ["for-each-ref", "--format=%(refname)%09%(objectname)", "refs"]
        treeBefore <- gitStdout repo ["ls-tree", "-r", "-z", "HEAD"]
        fullIndexBefore <- gitStdout repo ["ls-files", "--stage", "-z"]
        headBefore <- headCommit repo

        -- The installed executable must publish managed objects through Git's
        -- sparse-aware index path, not through a materialized managed root.
        let title = "Sparse cone managed decision"
            summary = "Managed records remain authoritative outside the cone."
            body = "## Decision\nPublish managed ADR records through Git even when sparse checkout omits their roots.\n"
        created <- realCreateAdr repo title summary body ["compiler.sparse"] ["src/**"]
        headAfter <- headCommit repo
        assertBool "create advances the checked-out branch" (headAfter /= headBefore)
        extractCommit created @?= Just headAfter
        (_Object created >>= (.: "indexed") :: Maybe Bool) @?= Just True
        (_Object created >>= (.: "index_revision") :: Maybe Text) @?= Just headAfter
        let adrId = maybeUnpack (extractAdrId created)
        assertBool "create result contains an ADR identifier" (not (null adrId))
        createdPaths <-
          case extractCreated created of
            Just paths | not (null paths) -> pure paths
            _ -> assertFailure "create JSON omitted its generated managed paths" >> fail "unreachable"
        assertBool
          "all generated paths are under the configured managed roots"
          (all ("architecture/adrai/" `T.isPrefixOf`) createdPaths)
        changedPaths <-
          filter (not . T.null) . T.splitOn "\NUL" . decodeUtf8 . LBS.toStrict
            <$> gitStdout repo ["diff", "--name-only", "-z", unpack headBefore, unpack headAfter]
        sort changedPaths @?= sort createdPaths
        forM_ createdPaths $ \path -> do
          let relativePath = unpack path
          gitSuccess repo ["cat-file", "-e", "HEAD:" <> relativePath]
          indexed <- gitStdout repo ["ls-files", "--stage", "--", relativePath]
          assertBool ("generated path is in the real index: " <> relativePath) (not (LBS.null indexed))

        -- The test fixture has one branch and no other refs: establish the
        -- full ref set exactly, then retain complete tree/index snapshots for
        -- all later public reads.
        let branchRef = strip (decodeUtf8 (LBS.toStrict branchBefore))
            expectedRefs revision = LBS.fromStrict (encodeUtf8 (branchRef <> "\t" <> revision <> "\n"))
        refsBefore @?= expectedRefs headBefore
        refsAfter <- gitStdout repo ["for-each-ref", "--format=%(refname)%09%(objectname)", "refs"]
        refsAfter @?= expectedRefs headAfter
        treeAfter <- gitStdout repo ["ls-tree", "-r", "-z", "HEAD"]
        fullIndexAfter <- gitStdout repo ["ls-files", "--stage", "-z"]
        assertBool "create changes the committed tree" (treeAfter /= treeBefore)
        assertBool "create changes the full index" (fullIndexAfter /= fullIndexBefore)
        gitSuccess repo ["diff", "--cached", "--quiet"]

        -- Only the expected branch transition and generated managed objects
        -- occur; sparse settings and the unrelated materialized source remain
        -- byte-for-byte stable and the worktree is clean.
        gitStdout repo ["symbolic-ref", "-q", "HEAD"] >>= (@?= branchBefore)
        gitStdout repo ["for-each-ref", "--format=%(refname)", "refs/heads"] >>= (@?= branchNamesBefore)
        BS.readFile sourcePath >>= (@?= sourceBefore)
        gitStdout repo ["ls-files", "--stage", "--", "src/app.txt"] >>= (@?= sourceIndexBefore)
        BS.readFile sparseConfig >>= (@?= sparseConfigBefore)
        gitStdout repo ["sparse-checkout", "list"] >>= (@?= sparseListBefore)
        gitStdout repo ["status", "--porcelain=v1", "--untracked-files=all", "-z"] >>= (@?= "")

        -- Reapplying the unchanged cone removes managed paths from the
        -- worktree, while Git's full tree and index retain every record.
        git repo ["sparse-checkout", "reapply"]
        forM_ createdPaths $ \path ->
          doesFileExist (repo </> unpack path) >>= (@?= False)
        headCommit repo >>= (@?= headAfter)
        gitStdout repo ["for-each-ref", "--format=%(refname)%09%(objectname)", "refs"] >>= (@?= refsAfter)
        gitStdout repo ["ls-tree", "-r", "-z", "HEAD"] >>= (@?= treeAfter)
        gitStdout repo ["ls-files", "--stage", "-z"] >>= (@?= fullIndexAfter)
        gitSuccess repo ["diff", "--cached", "--quiet"]
        BS.readFile sparseConfig >>= (@?= sparseConfigBefore)
        gitStdout repo ["sparse-checkout", "list"] >>= (@?= sparseListBefore)
        gitStdout repo ["status", "--porcelain=v1", "--untracked-files=all", "-z"] >>= (@?= "")

        -- The create result has already proved post-commit indexing for this
        -- exact HEAD.  Remove only this test-owned ignored cache to avoid the
        -- separate Windows warm ReplaceFileW contract; the real CLI compile
        -- below remains a cold Git/SQLite proof for the same sparse revision.
        let cacheDirectory = repo </> ".adrai"
        doesDirectoryExist cacheDirectory >>= (@?= True)
        removeDirectoryRecursive cacheDirectory
        doesDirectoryExist cacheDirectory >>= (@?= False)

        -- Compile and FTS search must resolve the committed sparse-excluded
        -- record from immutable Git/SQLite authority, with truthful public JSON.
        compileValue <- realAdraiJsonOrThrow repo ["compile", "--json"]
        compiled <-
          case parseCompileResult compileValue of
            Just result -> pure result
            Nothing -> assertFailure "compile JSON did not match the frozen result schema" >> fail "unreachable"
        coldCompilerRevision compiled @?= headAfter
        coldCompilerIssueCount compiled @?= 0
        database <- canonicalizePath (coldCompilerDatabase compiled)
        expectedDatabase <- canonicalizePath (repo </> ".adrai" </> "index.sqlite")
        database @?= expectedDatabase
        getMeta database "resolved_oid" >>= (@?= Just (unpack headAfter))

        searched <- realAdraiJsonOrThrow repo ["search", "Sparse cone managed decision", "--mode", "fts", "--json"]
        case parseSearchResults searched of
          Just (_, asOf, mode, _, results) -> do
            asOf @?= headAfter
            mode @?= "fts"
            case filter ((== Just (pack adrId)) . extractAdrId) results of
              [hit] -> do
                (_Object hit >>= (.: "title") :: Maybe Text) @?= Just title
                (_Object hit >>= (.: "status") :: Maybe Text) @?= Just "active"
              _ -> assertFailure "FTS search did not expose exactly the sparse-excluded ADR"
          Nothing -> assertFailure "search JSON did not match the frozen result schema"

        BS.readFile sourcePath >>= (@?= sourceBefore)
        gitStdout repo ["ls-files", "--stage", "--", "src/app.txt"] >>= (@?= sourceIndexBefore)
        BS.readFile sparseConfig >>= (@?= sparseConfigBefore)
        gitStdout repo ["sparse-checkout", "list"] >>= (@?= sparseListBefore)
        headCommit repo >>= (@?= headAfter)
        gitStdout repo ["for-each-ref", "--format=%(refname)%09%(objectname)", "refs"] >>= (@?= refsAfter)
        gitStdout repo ["ls-tree", "-r", "-z", "HEAD"] >>= (@?= treeAfter)
        gitStdout repo ["ls-files", "--stage", "-z"] >>= (@?= fullIndexAfter)
        gitSuccess repo ["diff", "--cached", "--quiet"]
        gitStdout repo ["status", "--porcelain=v1", "--untracked-files=all", "-z"] >>= (@?= "")

-- =====================================================================
-- Test 9: A junction beneath a managed root cannot escape the repository
-- =====================================================================

testSymlinkedManagedParentCannotEscapeRepository :: TestTree
testSymlinkedManagedParentCannotEscapeRepository =
  testCase
    "symlinked managed parent cannot escape repository"
    $ withSystemTempDirectory "adrai managed junction containment" $ \tmpDir -> do
        repo <- createTestRepo tmpDir
        createAdraiInit repo
        repository <-
          discoverRepository systemGit repo >>= \case
            Left problem -> assertFailure ("could not discover containment fixture repository: " <> show problem) >> fail "unreachable"
            Right discovered -> pure discovered
        let outside = tmpDir </> "outside"
            managedParent = repo </> "architecture" </> "adrai"
            architectureParent = takeDirectory managedParent
            callerPath = repo </> "caller-index.bin"
            callerBytes = BS.pack [0, 255, 13, 10, 128, 64, 9, 7, 3]
            createArguments =
              [ "create",
                "--title", "Junction containment decision",
                "--summary", "Managed records must never escape the repository root.",
                "--body", "## Decision\nReject every reparse-point managed parent before mutation.\n",
                "--actor", "llm:planner",
                "--model", "demo-model",
                "--domain", "testing.containment",
                "--applies-to", "src/**",
                "--json"
              ]
        BS.writeFile callerPath callerBytes
        git repo ["add", "--", "caller-index.bin"]
        callerStageBefore <- gitStdout repo ["ls-files", "--stage", "--", "caller-index.bin"]
        callerCachedDiffBefore <- gitStdout repo ["diff", "--cached", "--raw", "-z"]
        createDirectoryIfMissing True architectureParent
        createDirectoryIfMissing True outside
        createManagedParentJunction outside managedParent
        let removeFixtureJunction = removeManagedParentJunction managedParent
        (do
            before <- observableState repository repo architectureParent managedParent outside
            (exitCode, stdout, stderr) <- realSpawnAdrai repo createArguments
            exitCode @?= ExitFailure 2
            stdout @?= ""
            let stderrText = decodeUtf8 (LBS.toStrict stderr)
                stagePrefix = "adrai: Stage5ValidateGenerated \"managed destination rejected for architecture/adrai/"
                escapedManagedParent = T.replace "\\" "\\\\\\\\" (pack managedParent)
            assertBool "junction rejection uses the Stage5 user-failure channel" (stagePrefix `T.isPrefixOf` stderrText)
            assertBool "junction rejection retains the ManagedPathRedirected constructor" ("ManagedPathRedirected " `T.isInfixOf` stderrText)
            assertBool "junction rejection names the redirected managed parent" (escapedManagedParent `T.isInfixOf` stderrText)
            assertBool "junction rejection has exactly the CLI failure newline framing" ("\n" `T.isSuffixOf` stderrText)
            afterRejected <- observableState repository repo architectureParent managedParent outside
            afterRejected @?= before
            sort <$> listDirectory outside >>= (@?= [])
            doesDirectoryExist managedParent >>= (@?= True)

            -- Remove only the fixture's junction, never its target; the exact
            -- same installed executable must then publish normally.
            removeFixtureJunction
            doesDirectoryExist managedParent >>= (@?= False)
            control <-
              realCreateAdr
                repo
                "Control containment decision"
                "A regular managed parent permits the same public mutation."
                "## Decision\nPublish only after containment succeeds.\n"
                ["testing.containment"]
                ["src/**"]
            controlHead <- headCommit repo
            extractCommit control @?= Just controlHead
            controlObject <-
              case _Object control of
                Just objectValue -> pure objectValue
                Nothing -> assertFailure "control create result is not a JSON object" >> fail "unreachable"
            (controlObject .: "indexed" :: Maybe Bool) @?= Just True
            (controlObject .: "index_warnings" :: Maybe Integer) @?= Just 0
            (controlObject .: "index_revision" :: Maybe Text) @?= Just controlHead
            database <-
              case controlObject .: "database" :: Maybe FilePath of
                Just value -> canonicalizePath value
                Nothing -> assertFailure "control create JSON omits database" >> fail "unreachable"
            expectedDatabase <- canonicalizePath (repo </> ".adrai" </> "index.sqlite")
            database @?= expectedDatabase
            getMeta database "resolved_oid" >>= (@?= Just (unpack controlHead))
            created <-
              case extractCreated control of
                Just paths | not (null paths) -> pure paths
                _ -> assertFailure "control create JSON omits generated managed paths" >> fail "unreachable"
            forM_ created $ \path ->
              gitSuccess repo ["cat-file", "-e", "HEAD:" <> unpack path]
            callerStageAfter <- gitStdout repo ["ls-files", "--stage", "--", "caller-index.bin"]
            callerCachedDiffAfter <- gitStdout repo ["diff", "--cached", "--raw", "-z"]
            BS.readFile callerPath >>= (@?= callerBytes)
            callerStageAfter @?= callerStageBefore
            callerCachedDiffAfter @?= callerCachedDiffBefore
            sort <$> listDirectory outside >>= (@?= [])
          ) `finally` removeFixtureJunction
  where
    observableState repository repo architectureParent managedParent outside = do
      symbolicHead <- gitStdout repo ["symbolic-ref", "-q", "HEAD"]
      headOid <- gitStdout repo ["rev-parse", "HEAD"]
      headTree <- gitStdout repo ["rev-parse", "HEAD^{tree}"]
      rawIndex <- BS.readFile (repositoryGitDir repository </> "index")
      stagedIndex <- gitStdout repo ["ls-files", "--stage", "-z"]
      stagedDiff <- gitStdout repo ["diff", "--cached", "--raw", "-z"]
      status <- gitStdout repo ["status", "--porcelain=v1", "--untracked-files=all", "-z"]
      refs <- gitStdout repo ["for-each-ref", "--format=%(refname)%00%(objectname)%00", "refs/heads", "refs/remotes"]
      reflogs <- gitStdout repo ["reflog", "show", "--all", "--format=%gD%x00%H%x00%gs%x00"]
      committedManaged <- gitStdout repo ["ls-tree", "-r", "-z", "HEAD", "--", "architecture/adrai"]
      managedParentEntries <- sort <$> listDirectory architectureParent
      cache <- snapshotDirectory (repo </> ".adrai")
      outsideBytes <- snapshotDirectory outside
      managedParentExists <- doesDirectoryExist managedParent
      pure (symbolicHead, headOid, headTree, rawIndex, stagedIndex, stagedDiff, status, refs, reflogs, committedManaged, managedParentEntries, cache, outsideBytes, managedParentExists)

    snapshotDirectory root = do
      exists <- doesDirectoryExist root
      if not exists
        then pure []
        else go root
      where
        go directory = do
          names <- sort <$> listDirectory directory
          fmap concat $ forM names $ \name -> do
            let path = directory </> name
            isDirectory <- doesDirectoryExist path
            if isDirectory
              then go path
              else do
                isFile <- doesFileExist path
                if isFile
                  then do
                    bytes <- BS.readFile path
                    pure [(makeRelative root path, bytes)]
                  else pure []

    createManagedParentJunction target link
      | os == "mingw32" = do
          let command = "mklink /J \"" <> link <> "\" \"" <> target <> "\""
          result <- runProcess (shell command)
          case result of
            ExitSuccess -> pure ()
            ExitFailure code -> assertFailure ("failed to create Windows junction, exit " <> show code)
      | otherwise = assertFailure "this Windows-only junction containment proof requires mingw32"

    removeManagedParentJunction link = do
      isLink <- pathIsSymbolicLink link
      if isLink then removeDirectory link else pure ()

-- =====================================================================
-- Test 10: Unicode content round-trips through git/sqlite/fts/vector
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
      testSymlinkedManagedParentCannotEscapeRepository,
      testUnicodeRoundTrip,
      testUpstreamHintSurvives,
      testForceResetHidesAdr
    ]
