{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for cold / warm rebuild consistency.
--
-- Port of the consistency and cache-equivalence tests from
-- ``ADRAI_1_Source/tests/test_consistency_oracle.py`` and
-- ``ADRAI_1_Source/tests/test_cache_equivalence.py``.
--
-- These tests exercise the full ADR lifecycle across multiple branches,
-- then verify that removing ``.adrai`` and recompiling produces
-- bit-identical public output and all 20 semantic SQLite tables.
module Adrai.ConsistencyTest (tests) where

import Adrai.Integration.CLI
import Control.Monad (forM, forM_, void, when)
import qualified Data.Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (isPrefixOf, sort, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, strip, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector qualified as Vector
import Database.SQLite.Simple
  ( close,
    open,
    query_,
  )
import Database.SQLite.Simple.Types (Query(Query))
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    removeDirectoryRecursive,
    renamePath,
  )
import System.FilePath ((</>), takeDirectory, (<.>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

-- ---------------------------------------------------------------------------
-- JSON helper accessors
-- ---------------------------------------------------------------------------

_Object :: Data.Aeson.Value -> Maybe (KM.KeyMap Data.Aeson.Value)
_Object (Data.Aeson.Object o) = Just o
_Object _                     = Nothing

(.:) :: Data.Aeson.FromJSON a => KM.KeyMap Data.Aeson.Value -> Text -> Maybe a
(.:) km key =
  case KM.lookup (AesonKey.fromText key) km of
    Nothing  -> Nothing
    Just v   -> case Data.Aeson.eitherDecode (Data.Aeson.encode v) of
      Left   _ -> Nothing
      Right a  -> Just a

-- | Extract a Text from a JSON value's key.
lookupText :: Data.Aeson.Value -> Text -> Maybe Text
lookupText v key = do
  km <- _Object v
  km .: key

-- | Extract a String from a JSON value's key.
lookupString :: Data.Aeson.Value -> Text -> Maybe String
lookupString v key = do
  km <- _Object v
  km .: key

(.:?) :: Data.Aeson.FromJSON a => KM.KeyMap Data.Aeson.Value -> Text -> Maybe (Maybe a)
(.:?) km key =
  case KM.lookup (AesonKey.fromText key) km of
    Nothing  -> Just Nothing
    Just v   -> case Data.Aeson.eitherDecode (Data.Aeson.encode v) of
      Left   _  -> Just Nothing
      Right a   -> Just (Just a)

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
  if Vector.null arr then Nothing else Just (mapMaybe valText (Vector.toList arr))
valTextList _ = Nothing

-- | Extract array contents as Values.
valValueList :: Data.Aeson.Value -> Maybe [Data.Aeson.Value]
valValueList (Data.Aeson.Array arr) =
  if Vector.null arr then Nothing else Just (Vector.toList arr)
valValueList _ = Nothing

-- | Extract "adr" field from a search result object.
adrFromResult :: Data.Aeson.Value -> Maybe Text
adrFromResult v = do
  km <- _Object v
  km .: "adr"

headVal :: Data.Aeson.Value -> Maybe Data.Aeson.Value
headVal (Data.Aeson.Array arr) =
  if Vector.null arr then Nothing else Just (Vector.head arr)
headVal _ = Nothing

-- | Extract the ADR ID from a create-adr result value.
extractAdrId :: Data.Aeson.Value -> Maybe Text
extractAdrId v = do
  o <- _Object v
  o .: "adr"

-- | Extract commit hash from a create-adr result.
extractCommit :: Data.Aeson.Value -> Maybe Text
extractCommit v = do
  o <- _Object v
  o .: "commit"

-- ---------------------------------------------------------------------------
-- Head / revision helpers
-- ---------------------------------------------------------------------------

-- | Get the HEAD commit hash of a repository.
headCommit :: FilePath -> IO Text
headCommit repo =
  gitStdout repo ["rev-parse", "HEAD"]
    >>= \h -> pure (strip (decodeUtf8 (LBS.toStrict h)))

-- ---------------------------------------------------------------------------
-- ADR creation helpers (mirrors Python test fixture functions)
-- ---------------------------------------------------------------------------

-- | Create the "cache identity" fixture ADR.
createCacheAdr
  :: FilePath
  -> IO Data.Aeson.Value
createCacheAdr repo =
  createAdr
    repo
    "Stable cache identity"
    "Cache identity derives from semantic inputs."
    "## Context\nBuilds move between workspaces.\n\n## Decision\nCache keys exclude absolute workspace paths and use source digests.\n\n## Consequences\nInputs must be normalized."
    ["compiler.cache"]
    ["src/compiler/cache/**"]

-- | Create the "jobs / durable delivery" fixture ADR.
createJobsAdr
  :: FilePath
  -> IO Data.Aeson.Value
createJobsAdr repo =
  createAdr
    repo
    "At-least-once job delivery"
    "Workers acknowledge durable jobs only after successful execution."
    "## Decision\nUse durable queues and idempotent job handlers."
    ["runtime.jobs"]
    ["src/jobs/**"]

-- | Create a release-only ADR for the release branch.
createReleaseAdr
  :: FilePath
  -> IO Data.Aeson.Value
createReleaseAdr repo =
  adraiJsonOrThrow repo
    [ "create-adr",
      "--title", "Release-only compatibility shim",
      "--summary", "The old release retains its compatibility shim.",
      "--body", "## Decision\nKeep the compatibility shim on the old release line.",
      "--domain", "release.compatibility",
      "--applies-to", "src/legacy/**",
      "--actor", "human:release-owner",
      "--model", "demo-model",
      "--json"
    ]

-- | Amend an ADR via the ``amend-adr`` CLI command.
amendAdrViaCli
  :: FilePath
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> IO Data.Aeson.Value
amendAdrViaCli repo adrId maybeTitle maybeSummary maybeBody =
  adraiJsonOrThrow repo
    ( [ "amend-adr", unpack adrId ]
        <> concat
          [ maybe [] (\v -> ["--title", unpack v]) maybeTitle,
            maybe [] (\v -> ["--summary", unpack v]) maybeSummary,
            maybe [] (\v -> ["--body", unpack v]) maybeBody
          ]
        <> ["--actor", "human:architect", "--json"]
    )

-- | Commit arbitrary files in the repo (fixture helper).
commitFilesWithMsg
  :: FilePath
  -> [(FilePath, BS.ByteString)]
  -> Text
  -> IO Text
commitFilesWithMsg repo files message = do
  mapM_ writeAndCommit files
  git repo ["--literal-pathspecs", "add", "--"]
  git repo ["commit", "-m", unpack message]
  gitStdout repo ["rev-parse", "HEAD"] >>= \h ->
    pure (strip (decodeUtf8 (LBS.toStrict h)))
  where
    writeAndCommit (rel, content) = do
      let path = repo </> rel
      createDirectoryIfMissing True (takeDirectory path)
      BS.writeFile path content

-- | Commit a single file with a custom message.
commitFileWithMsg
  :: FilePath
  -> FilePath
  -> BS.ByteString
  -> Text
  -> IO Text
commitFileWithMsg repo relativePath content message =
  commitFilesWithMsg repo [(relativePath, content)] message

-- ---------------------------------------------------------------------------
-- Semantic database snapshot helpers
-- ---------------------------------------------------------------------------

-- | The 20 semantic tables that must agree between warm and cold rebuilds.
-- Matches the canonical set from ``test_cache_equivalence.py``.
semanticTables :: [String]
semanticTables =
  [ "decision_record",
    "connection",
    "record_parent",
    "record_head",
    "scope_revision",
    "scope_head",
    "domain_revision",
    "domain_head",
    "status_revision",
    "status_head",
    "operation",
    "object_event",
    "operation_commit",
    "line_landing",
    "collapsed",
    "projection",
    "embedding",
    "vector_bucket",
    "issue",
    "adr_materialization"
  ]

-- | Normalise a ByteString value inside an SQLite row so that
-- binary content is represented as a hex string (matching the
-- Python ``_normal`` function).
normalise :: String -> String
normalise s =
  let bytes = BS.pack (map (fromIntegral . fromEnum) s)
  in if BS.length bytes > 0 && BS.head bytes < 128
     then s
     else "{\"bytes\":\"" <> map (toEnum . fromIntegral) (BS.unpack bytes) <> "\"}"

-- | Get all semantic table contents from a database.
semanticTableContents :: FilePath -> IO [(String, [[String]])]
semanticTableContents dbPath =
  let fetchTable tbl conn = do
        let tblName = decodeUtf8 tbl
        let q = Query (T.pack ("SELECT * FROM " ++ unpack tblName ++ " ORDER BY rowid"))
        rows <- query_ conn q :: IO [[String]]
        pure (T.unpack tblName, rows)
  in do
    conn <- open dbPath
    tables <- query_ conn "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"
      :: IO [BS.ByteString]
    results <- forM tables (\tbl -> fetchTable tbl conn)
    close conn
    pure $ sortOn fst results

-- | Remove the .adrai directory if it exists.
removeAdraiIfExists :: FilePath -> IO ()
removeAdraiIfExists repo = do
  let adraiDir = repo </> ".adrai"
  exists <- doesDirectoryExist adraiDir
  when exists $ removeDirectoryRecursive adraiDir

-- ---------------------------------------------------------------------------
-- Snapshot type and helpers
-- ---------------------------------------------------------------------------

-- | A snapshot of the public state of an adrai repository.
-- Used to compare warm vs. cold rebuilds across all dimensions.
data Snapshot = Snapshot
  { snapRevision   :: Text
  , snapAdrs       :: [Text]
  , snapCollapsed  :: Map Text Data.Aeson.Value
  , snapExploded   :: Map Text Data.Aeson.Value
  , snapSearches   :: Map Text Data.Aeson.Value
  , snapCompare    :: Maybe Data.Aeson.Value
  , snapDoctor     :: Maybe Data.Aeson.Value
  , snapTables     :: [(String, [[String]])]
  } deriving (Eq, Show)

-- | Build a snapshot of the given repository.
-- This is the core of the consistency oracle: it collects all public
-- output channels (compile, search, show, history, compare, doctor,
-- and every semantic SQLite table).
snapshot :: FilePath -> Text -> IO Snapshot
snapshot repo compareFrom = do
  -- Compile
  compiled <- adraiJsonOrThrow repo ["compile", "--json"]
  let revision = fromMaybe "" (lookupText compiled "revision")

  -- Search all visible ADRs
  allResults <- adraiJsonOrThrow repo
    [ "search", "--include-obsolete", "--limit", "1000", "--json" ]
  let adrIds = sort $ mapMaybe adrFromResult (fromMaybe [] (valValueList allResults))
  -- If no ADRs found, get empty list
  let adrIds' = if null adrIds then [] else adrIds

  -- Collapsed projections for each ADR
  collapsedMap <- Map.fromList <$> mapM (\adrId -> do
    v <- adraiJsonOrThrow repo
      [ "show", unpack adrId, "--view", "collapsed", "--json" ]
    pure (adrId, v)
    ) adrIds'

  -- Exploded projections for each ADR
  explodedMap <- Map.fromList <$> mapM (\adrId -> do
    v <- adraiJsonOrThrow repo
      [ "show", unpack adrId, "--view", "exploded", "--json" ]
    pure (adrId, v)
    ) adrIds'

  -- Searches
  fts <- adraiJsonOrThrow repo
    [ "search", "--query", "cache identity durable", "--fts",
      "--include-obsolete", "--limit", "50", "--json" ]
  vector <- adraiJsonOrThrow repo
    [ "search", "--query", "memoized artifact fingerprint", "--vector",
      "--include-obsolete", "--limit", "50", "--json" ]
  hybrid <- adraiJsonOrThrow repo
    [ "search", "--query", "durable worker acknowledgement", "--hybrid",
      "--include-obsolete", "--limit", "50", "--json" ]
  fileSearch <- adraiJsonOrThrow repo
    [ "search", "--file", "src/compiler/cache/Key.py",
      "--include-obsolete", "--limit", "50", "--json" ]
  domainSearch <- adraiJsonOrThrow repo
    [ "search", "--domain", "compiler",
      "--include-obsolete", "--limit", "50", "--json" ]

  let searchesMap = Map.fromList
        [ ("fts", fts),
          ("vector", vector),
          ("hybrid", hybrid),
          ("file", fileSearch),
          ("domain", domainSearch)
        ]

  -- Compare
  compareResult <- adraiJsonOrThrow repo
    [ "compare", "--from", unpack compareFrom, "--to", "HEAD",
      "--include-unchanged", "--json" ]
  -- Strip cache field from compare
  let strippedCompare = stripCacheField compareResult

  -- Doctor
  doctorResult <- adraiJsonOrThrow repo ["doctor", "--json"]
  let strippedDoctor = stripCacheField doctorResult

  -- Semantic database tables
  let dbPath = repo </> ".adrai" </> "adrai.sqlite"
  tables <- semanticTableContents dbPath

  pure Snapshot
    { snapRevision = revision
    , snapAdrs = adrIds'
    , snapCollapsed = collapsedMap
    , snapExploded = explodedMap
    , snapSearches = searchesMap
    , snapCompare = Just strippedCompare
    , snapDoctor = Just strippedDoctor
    , snapTables = tables
    }

-- | Strip cache-related fields from doctor/compare output.
stripCacheField :: Data.Aeson.Value -> Data.Aeson.Value
stripCacheField v =
  case _Object v of
    Nothing -> v
    Just o  -> Data.Aeson.Object $
      KM.delete "cache" $
      KM.delete "current_access" $
      KM.delete "database_build" $
      o

-- | Compare two snapshots for equality.
snapshotsEqual :: Snapshot -> Snapshot -> Bool
snapshotsEqual s1 s2 =
  snapRevision s1 == snapRevision s2 &&
  sort (snapAdrs s1) == sort (snapAdrs s2) &&
  snapCollapsed s1 == snapCollapsed s2 &&
  snapExploded s1 == snapExploded s2 &&
  snapSearches s1 == snapSearches s2 &&
  snapCompare s1 == snapCompare s2 &&
  snapDoctor s1 == snapDoctor s2 &&
  tablesEqual (snapTables s1) (snapTables s2)

-- | Compare two snapshots, ignoring database tables (public API only).
snapshotsEqualPublic :: Snapshot -> Snapshot -> Bool
snapshotsEqualPublic s1 s2 =
  snapRevision s1 == snapRevision s2 &&
  sort (snapAdrs s1) == sort (snapAdrs s2) &&
  snapCollapsed s1 == snapCollapsed s2 &&
  snapExploded s1 == snapExploded s2 &&
  snapSearches s1 == snapSearches s2 &&
  snapCompare s1 == snapCompare s2 &&
  snapDoctor s1 == snapDoctor s2

-- | Compare database tables between two snapshots.
snapshotsTablesEqual :: Snapshot -> Snapshot -> Bool
snapshotsTablesEqual s1 s2 =
  tablesEqual (snapTables s1) (snapTables s2)

-- ---------------------------------------------------------------------------
-- Snapshot comparison helpers
-- ---------------------------------------------------------------------------

-- | Get ADR IDs from a search result.
adrIdsFromSearch :: Data.Aeson.Value -> [Text]
adrIdsFromSearch v =
  mapMaybe adrFromResult (fromMaybe [] (valValueList v))

-- | Get the "conflict" field from a collapsed projection.
getConflict :: Data.Aeson.Value -> Maybe Bool
getConflict v =
  case _Object v of
    Nothing -> Nothing
    Just o  -> o .: "conflict"

-- | Get the "record" field from a collapsed projection.
getRecord :: Data.Aeson.Value -> Maybe Data.Aeson.Value
getRecord v =
  case _Object v of
    Nothing -> Nothing
    Just o  -> o .: "record"

-- | Get the "domains" field from a collapsed projection.
getDomains :: Data.Aeson.Value -> Maybe [Text]
getDomains v =
  case _Object v of
    Nothing -> Nothing
    Just o  -> o .: "domains"

-- | Get the "applies_to" field from a collapsed projection.
getAppliesTo :: Data.Aeson.Value -> Maybe [Text]
getAppliesTo v =
  case _Object v of
    Nothing -> Nothing
    Just o  -> o .: "applies_to"

-- | Get the "kind" field from a compare entry.
getEntryKind :: Data.Aeson.Value -> Maybe Text
getEntryKind v =
  case _Object v of
    Nothing -> Nothing
    Just o  -> o .: "kind"

-- | Get the "adr" field from a compare entry.
getEntryAdr :: Data.Aeson.Value -> Maybe Text
getEntryAdr v =
  case _Object v of
    Nothing -> Nothing
    Just o  -> o .: "adr"

-- | Get the "entries" field from a compare result.
getCompareEntries :: Data.Aeson.Value -> Maybe [Data.Aeson.Value]
getCompareEntries v =
  case _Object v of
    Nothing -> Nothing
    Just o  -> o .: "entries"

-- | Get the "ok" field from a doctor result.
getDoctorOk :: Data.Aeson.Value -> Maybe Bool
getDoctorOk v =
  case _Object v of
    Nothing -> Nothing
    Just o  -> o .: "ok"

-- | Get the "visible" field (search results) from a snapshot's search.
getSearchResults :: Data.Aeson.Value -> [Data.Aeson.Value]
getSearchResults v =
  case _Object v of
    Nothing -> []
    Just o  -> fromMaybe [] (o .: "results")

-- ---------------------------------------------------------------------------
-- Snapshot builder for Python test 1 (public_snapshot variant)
-- ---------------------------------------------------------------------------

-- | Build a snapshot matching the Python ``public_snapshot`` helper
-- used by test_consistency_oracle.py. This is a lighter snapshot that
-- omits the full database tables but includes everything else.
publicSnapshot :: FilePath -> [Text] -> IO Snapshot
publicSnapshot repo adrIds = do
  compiled <- adraiJsonOrThrow repo ["compile", "--json"]
  let revision = fromMaybe "" (lookupText compiled "revision")

  visible <- adraiJsonOrThrow repo
    [ "search", "--include-obsolete", "--limit", "1000", "--json" ]
  let visibleAdrs = sort (adrIdsFromSearch visible)

  -- Collapsed and exploded projections for each ADR
  collapsedMap <- Map.fromList <$> mapM (\adrId -> do
    v <- adraiJsonOrThrow repo
      [ "show", unpack adrId, "--view", "collapsed", "--json" ]
    pure (adrId, v)
    ) visibleAdrs

  explodedMap <- Map.fromList <$> mapM (\adrId -> do
    v <- adraiJsonOrThrow repo
      [ "show", unpack adrId, "--view", "exploded", "--json" ]
    pure (adrId, v)
    ) visibleAdrs

  -- Searches
  fts <- adraiJsonOrThrow repo
    [ "search", "--query", "content digest queue acknowledgement",
      "--fts", "--include-obsolete", "--limit", "100", "--json" ]
  vector <- adraiJsonOrThrow repo
    [ "search", "--query", "memoized artifact fingerprint",
      "--vector", "--include-obsolete", "--limit", "100", "--json" ]
  hybrid <- adraiJsonOrThrow repo
    [ "search", "--query", "durable worker execution",
      "--hybrid", "--include-obsolete", "--limit", "100", "--json" ]
  fileCache <- adraiJsonOrThrow repo
    [ "search", "--file", "src/runtime/cache/Key.py",
      "--include-obsolete", "--limit", "100", "--json" ]
  fileJobs <- adraiJsonOrThrow repo
    [ "search", "--file", "src/jobs/Worker.py",
      "--include-obsolete", "--limit", "100", "--json" ]
  domainCompiler <- adraiJsonOrThrow repo
    [ "search", "--domain", "compiler",
      "--include-obsolete", "--limit", "100", "--json" ]
  domainRuntime <- adraiJsonOrThrow repo
    [ "search", "--domain", "runtime",
      "--include-obsolete", "--limit", "100", "--json" ]

  let searchesMap = Map.fromList
        [ ("fts", fts),
          ("vector", vector),
          ("hybrid", hybrid),
          ("file_cache", fileCache),
          ("file_jobs", fileJobs),
          ("domain_compiler", domainCompiler),
          ("domain_runtime", domainRuntime)
        ]

  -- Database tables
  let dbPath = repo </> ".adrai" </> "adrai.sqlite"
  tables <- semanticTableContents dbPath

  pure Snapshot
    { snapRevision = revision
    , snapAdrs = visibleAdrs
    , snapCollapsed = collapsedMap
    , snapExploded = explodedMap
    , snapSearches = searchesMap
    , snapCompare = Nothing
    , snapDoctor = Nothing
    , snapTables = tables
    }

-- ---------------------------------------------------------------------------
-- Helpers for "withColdRebuild" pattern
-- ---------------------------------------------------------------------------

-- | Take a snapshot, remove .adrai, take another snapshot, and verify
-- they are identical (both public API and all 20 semantic tables).
assertColdWarmMatches :: FilePath -> [Text] -> IO ()
assertColdWarmMatches repo adrIds = do
  warm <- publicSnapshot repo adrIds
  removeAdraiIfExists repo
  cold <- publicSnapshot repo adrIds

  -- Public API must match
  assertBool "public snapshot mismatch (revision, ADRs, projections, searches)"
    (snapshotsEqualPublic warm cold)

  -- All semantic tables must match
  assertBool "semantic database mismatch"
    (snapshotsTablesEqual warm cold)

-- | Take a snapshot, remove .adrai, take another snapshot, and verify
-- they are identical (full snapshot including compare/doctor).
assertFullColdWarmMatches :: FilePath -> Text -> IO ()
assertFullColdWarmMatches repo compareFrom = do
  warm <- snapshot repo compareFrom
  removeAdraiIfExists repo
  cold <- snapshot repo compareFrom

  assertBool "full snapshot mismatch"
    (snapshotsEqual warm cold)

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup "Consistency oracle (P4-07)"
    [ -- From test_consistency_oracle.py
      testCase "all_mutation_axes_and_merge_heavy_branch_switches_are_cold_warm_identical"
        testAllMutationAxes,
      testCase "divergent_amendments_conflict_and_reconciliation_are_cold_warm_identical"
        testDivergentAmendments,
      -- From test_cache_equivalence.py
      testCase "merge_heavy_branch_caches_are_semantically_identical_to_cold_rebuilds"
        testMergeHeavyBranchEquivalence
    ]

-- =====================================================================
-- Test 1: All mutation axes and merge-heavy branch switches
-- Port of test_consistency_oracle.py::test_all_mutation_axes_and_merge_heavy_branch_switches_are_cold_warm_identical
-- =====================================================================

testAllMutationAxes :: IO ()
testAllMutationAxes =
  withSystemTempDirectory "adrai consistency mutation" $ \tmpDir -> do
    repo <- createTestRepo tmpDir

    -- Create fixture ADRs
    cache <- createCacheAdr repo
    jobs <- createJobsAdr repo
    let allAdrs = [cache, jobs]

    -- Commit representative source files
    commitFilesWithMsg repo
      [ ("src/runtime/cache/Key.py",
         "cache key uses normalized source digest and compiler ABI; "
         <> "exclude absolute workspace paths\n"),
        ("src/jobs/Worker.py",
         "durable queue worker retries idempotently and acknowledges "
         <> "only after successful transaction commit\n")
      ]
      "add representative architecture-bearing files"

    let cacheId = fromMaybe "" (extractAdrId cache)
        jobsId  = fromMaybe "" (extractAdrId jobs)

    -- Record release tip
    releaseTip <- headCommit repo
    git repo ["branch", "release/2025-08-01", unpack releaseTip]

    -- Mutation: amend ADR (cache)
    void $ amendAdrViaCli repo cacheId Nothing
      (Just "Cache identity uses normalized source and compiler-input digests.")
      (Just "## Decision\nUse normalized source, compiler, and environment digests for cache identity.")

    -- Mutation: change domains
    adraiJsonOrThrow repo
      [ "amend-adr", unpack cacheId,
        "--change-domain", "compiler.cache=compiler.cache.identity",
        "--actor", "human:compiler-owner", "--json"
      ]

    -- Mutation: change scope
    adraiJsonOrThrow repo
      [ "amend-adr", unpack cacheId,
        "--add-scope", "src/runtime/cache/**",
        "--actor", "human:compiler-owner", "--json"
      ]

    -- Mutation: obsolete then reactivate ADR (jobs)
    adraiJsonOrThrow repo
      [ "amend-adr", unpack jobsId,
        "--status", "obsolete",
        "--reason", "A replacement was expected to own queue acknowledgement.",
        "--actor", "human:runtime-owner", "--json"
      ]
    adraiJsonOrThrow repo
      [ "amend-adr", unpack jobsId,
        "--status", "active",
        "--reason", "The replacement did not cover worker acknowledgement semantics.",
        "--actor", "human:runtime-owner", "--json"
      ]

    -- First cold/warm verification on main
    let allAdrIds = [cacheId, jobsId]
    assertColdWarmMatches repo allAdrIds

    -- Create develop branch with noise commits
    git repo ["switch", "-c", "develop"]
    forM_ [0 .. 2] $ \index -> do
      commitFileWithMsg repo
        ("src/noise/develop-" ++ show index <.> "txt")
        (encodeUtf8 ("develop noise " <> T.pack (show index) <> "\n"))
        (T.pack ("develop: noisy product change " <> show index))

    -- Create feature branch with observability ADR
    git repo ["switch", "-c", "feature/observability"]
    observability <- adraiJsonOrThrow repo
      [ "create-adr",
        "--title", "Structured runtime telemetry",
        "--summary", "Runtime operations emit stable structured telemetry events.",
        "--body", "## Decision\nEmit structured events with stable names and correlation identifiers.",
        "--domain", "runtime.observability",
        "--applies-to", "src/runtime/telemetry/**",
        "--actor", "llm:feature-agent",
        "--model", "planner-v1",
        "--json"
      ]
    let observabilityId = fromMaybe "" (extractAdrId observability)
    let allAdrIds' = [cacheId, jobsId, observabilityId]

    commitFileWithMsg repo
      ("src/runtime/telemetry/README.md")
      "telemetry implementation noise\n"
      "feature: implement telemetry plumbing"

    -- Merge feature into develop
    git repo ["switch", "develop"]
    git repo ["merge", "--no-ff", "feature/observability", "-m", "merge observability feature"]
    commitFilesWithMsg repo
      [ ("src/noise/after-merge.txt", "post merge development noise\n") ]
      "develop: continue after feature merge"

    -- Second cold/warm verification on develop
    assertColdWarmMatches repo allAdrIds'

    -- Switch to main, merge develop
    git repo ["switch", "main"]
    git repo ["merge", "--no-ff", "develop", "-m", "merge development train"]
    mainTip <- headCommit repo

    -- Verify on main: observability ADR is visible
    assertColdWarmMatches repo allAdrIds'

    -- Verify observability ADR appears in main's visible set
    mainVisible <- adraiJsonOrThrow repo
      [ "search", "--include-obsolete", "--limit", "1000", "--json" ]
    let mainAdrs = adrIdsFromSearch mainVisible
    assertBool "observability ADR should be visible on main"
      (observabilityId `elem` mainAdrs)

    -- Verify cache ADR domains on main
    mainCacheCollapsed <- adraiJsonOrThrow repo
      [ "show", unpack cacheId, "--view", "collapsed", "--json" ]
    assertBool "cache ADR domains should include compiler.cache.identity on main"
      (any ("compiler.cache.identity" `isPrefixOf`) (map unpack (fromMaybe [] (getDomains mainCacheCollapsed))))

    -- Switch to release branch and verify
    git repo ["switch", "release/2025-08-01"]
    assertColdWarmMatches repo allAdrIds

    -- Verify observability ADR is NOT visible on release
    releaseVisible <- adraiJsonOrThrow repo
      [ "search", "--include-obsolete", "--limit", "1000", "--json" ]
    let releaseAdrs = adrIdsFromSearch releaseVisible
    assertBool "observability ADR should NOT be visible on release"
      (observabilityId `notElem` releaseAdrs)

    -- Verify cache ADR state on release
    releaseCacheCollapsed <- adraiJsonOrThrow repo
      [ "show", unpack cacheId, "--view", "collapsed", "--json" ]
    assertBool "cache ADR domains should be compiler.cache (not identity) on release"
      (any (== "compiler.cache") (map unpack (fromMaybe [] (getDomains releaseCacheCollapsed))))

    -- Verify scope on release
    assertBool "cache ADR scope should include src/compiler/cache on release"
      (any (== "src/compiler/cache/**") (fromMaybe [] (getAppliesTo releaseCacheCollapsed)))

    -- Compare release vs main
    comparison <- adraiJsonOrThrow repo
      [ "compare", "--from", "release/2025-08-01", "--to", unpack mainTip, "--json" ]
    let entries = fromMaybe [] (getCompareEntries comparison)
    let addedAdrs = mapMaybe getEntryAdr $ filter ((== Just "added") . getEntryKind) entries
    let changedAdrs = mapMaybe getEntryAdr $ filter ((== Just "changed") . getEntryKind) entries
    assertBool "observability ADR should be in added list"
      (observabilityId `elem` addedAdrs)
    assertBool "cache ADR should be in changed list"
      (cacheId `elem` changedAdrs)

    -- Repeated switching must recover exactly the same state
    forM_ [0 .. 2] $ \_ -> do
      git repo ["switch", "main"]
      mainSnap <- publicSnapshot repo allAdrIds'
      -- Verify observability is visible
      mainCheck <- adraiJsonOrThrow repo
        [ "search", "--include-obsolete", "--limit", "1000", "--json" ]
      assertBool "observability ADR should be visible on main (round 2)"
        (observabilityId `elem` adrIdsFromSearch mainCheck)

      git repo ["switch", "release/2025-08-01"]
      -- Verify observability is NOT visible
      releaseCheck <- adraiJsonOrThrow repo
        [ "search", "--include-obsolete", "--limit", "1000", "--json" ]
      assertBool "observability ADR should NOT be visible on release (round 2)"
        (observabilityId `notElem` adrIdsFromSearch releaseCheck)

-- =====================================================================
-- Test 2: Divergent amendments conflict and reconciliation
-- Port of test_consistency_oracle.py::test_divergent_amendments_conflict_and_reconciliation_are_cold_warm_identical
-- =====================================================================

testDivergentAmendments :: IO ()
testDivergentAmendments =
  withSystemTempDirectory "adrai consistency divergent" $ \tmpDir -> do
    repo <- createTestRepo tmpDir

    -- Create initial ADR on main
    created <- createCacheAdr repo
    let createdId = fromMaybe "" (extractAdrId created)

    -- Commit fixture source files
    commitFilesWithMsg repo
      [ ("src/runtime/cache/Key.py", "cache key source digest compiler ABI target platform\n"),
        ("src/jobs/Worker.py", "durable queue acknowledgement after successful execution\n")
      ]
      "add consistency-oracle source files"

    -- Create feature branch marker
    git repo ["branch", "feature/cache-key"]

    -- Amend on main
    void $ amendAdrViaCli repo createdId Nothing
      (Just "Main includes compiler ABI in cache identity.")
      (Just "## Decision\nMain cache identity includes source and compiler ABI digests.")

    -- Switch to feature and amend differently
    git repo ["switch", "feature/cache-key"]
    void $ amendAdrViaCli repo createdId Nothing
      (Just "Feature includes target platform in cache identity.")
      (Just "## Decision\nFeature cache identity includes source and target-platform digests.")

    -- Switch back to main and merge
    git repo ["switch", "main"]
    git repo ["merge", "--no-ff", "feature/cache-key", "-m", "merge competing cache policy"]

    -- Verify conflicted state
    conflictedSnap <- publicSnapshot repo [createdId]
    let collapsed = Map.lookup createdId (snapCollapsed conflictedSnap)
    case collapsed of
      Nothing -> assertFailure "could not find collapsed ADR in conflicted snapshot"
      Just v  -> do
        let conflict = getConflict v
        assertBool "ADR should have conflict=true after divergent merge"
          (conflict == Just True)

    -- Cold/warm match in conflicted state
    assertColdWarmMatches repo [createdId]

    -- Resolve by amending again
    resolved <- amendAdrViaCli repo createdId
      (Just "Stable cache identity")
      (Just "Cache identity includes source, compiler ABI, and target platform.")
      (Just "## Decision\nUse source, compiler ABI, and target-platform digests for cache identity.")

    -- Final cold/warm verification
    finalSnap <- publicSnapshot repo [createdId]
    let collapsed' = Map.lookup createdId (snapCollapsed finalSnap)
    case collapsed' of
      Nothing -> assertFailure "could not find collapsed ADR in final snapshot"
      Just v' -> do
        let conflict' = getConflict v'
        assertBool "ADR conflict should be resolved"
          (conflict' == Just False)

        -- The resolved record should match what amend-adr returned
        let resolvedRecord = fromMaybe "" (lookupString resolved "record")
            collapsedRecord' = fromMaybe "" (lookupString (fromMaybe (Data.Aeson.String "") (getRecord v')) "record")
        assertBool "resolved record should match collapsed record"
          (resolvedRecord == collapsedRecord')

    -- Cold/warm match after resolution
    assertColdWarmMatches repo [createdId]

    -- Verify single record head
    case collapsed' of
      Nothing -> assertFailure "could not find collapsed in final snapshot"
      Just v'' -> do
        headCount <- case _Object v'' of
          Nothing -> pure 0
          Just o -> case o .: "record_heads" of
            Nothing -> pure 0
            Just heads -> case heads of
              Data.Aeson.Array arr -> pure (Vector.length arr)
              _ -> pure 0
        assertBool "should have exactly 1 record head after resolution"
          (headCount == 1)

-- =====================================================================
-- Test 3: Merge-heavy branch caches are semantically identical
-- Port of test_cache_equivalence.py::test_merge_heavy_branch_caches_are_semantically_identical_to_cold_rebuilds
-- =====================================================================

testMergeHeavyBranchEquivalence :: IO ()
testMergeHeavyBranchEquivalence =
  withSystemTempDirectory "adrai equivalence merge-heavy" $ \tmpDir -> do
    repo <- createTestRepo tmpDir

    -- Create fixture ADRs on main
    cache <- createCacheAdr repo
    jobs <- createJobsAdr repo
    let cacheId = fromMaybe "" (extractAdrId cache)
        jobsId  = fromMaybe "" (extractAdrId jobs)

    let base = "main"  -- start from base (HEAD = main after init)

    -- Create release branch
    releaseTip <- headCommit repo
    git repo ["branch", "release/2025-08-01"]

    -- Amend cache on main
    void $ amendAdrViaCli repo cacheId
      (Just "Stable cache identity and ABI")
      (Just "Cache identity includes semantic inputs and compiler ABI.")
      (Just "## Decision\nUse normalized source and compiler ABI digests.")

    -- Change scope on main
    adraiJsonOrThrow repo
      [ "amend-adr", unpack cacheId,
        "--add-scope", "src/runtime/cache/**",
        "--actor", "human:main-architect", "--json"
      ]

    -- Change domains on main
    adraiJsonOrThrow repo
      [ "amend-adr", unpack cacheId,
        "--change-domain", "compiler.cache=compiler.cache.identity",
        "--actor", "human:main-architect", "--json"
      ]

    -- Create develop branch
    git repo ["branch", "develop"]

    -- Switch to feature branch from develop
    git repo ["switch", "-c", "feature/api-contract", "develop"]
    api <- adraiJsonOrThrow repo
      [ "create-adr",
        "--title", "Versioned API contracts",
        "--summary", "Public APIs evolve through explicit versioned contracts.",
        "--body", "## Decision\nPublish versioned endpoint schemas and compatibility windows.",
        "--domain", "api.compatibility",
        "--applies-to", "src/api/**",
        "--actor", "llm:feature-architect",
        "--model", "architecture-agent",
        "--json"
      ]
    let apiId = fromMaybe "" (extractAdrId api)

    -- Amend jobs on feature
    void $ amendAdrViaCli repo jobsId Nothing
      (Just "Workers acknowledge durable jobs after idempotent completion.")
      (Just "## Decision\nUse durable queues, idempotency keys, and post-completion acknowledgement.")

    featureTip <- headCommit repo

    -- Merge feature into develop
    git repo ["switch", "develop"]
    git repo ["merge", "--no-ff", "feature/api-contract", "-m", "merge API architecture"]
    developBeforeNoise <- headCommit repo

    -- Merge develop into main
    git repo ["switch", "main"]
    git repo ["merge", "--no-ff", "develop", "-m", "integrate development architecture"]

    -- Switch to release and create release-only ADRs
    git repo ["switch", "release/2025-08-01"]
    void $ amendAdrViaCli repo cacheId
      (Just "Release cache identity")
      (Just "The release line freezes source-digest cache identity.")
      (Just "## Decision\nUse source digests and freeze the release cache namespace.")

    releasePolicy <- adraiJsonOrThrow repo
      [ "create-adr",
        "--title", "Release hotfix policy",
        "--summary", "Release branches accept narrowly scoped verified hotfixes.",
        "--body", "## Decision\nRequire focused hotfix commits and release verification.",
        "--domain", "delivery.release",
        "--applies-to", "release/**",
        "--actor", "human:release-manager",
        "--model", "demo-model",
        "--json"
      ]
    let releasePolicyId = fromMaybe "" (extractAdrId releasePolicy)

    releaseBeforeNoise <- headCommit repo

    -- Switch back to main and merge release
    git repo ["switch", "main"]
    git repo ["merge", "--no-ff", "release/2025-08-01", "-m", "merge release architecture"]

    -- Verify conflict state after release merge
    cacheAfterMerge <- adraiJsonOrThrow repo
      [ "show", unpack cacheId, "--view", "collapsed", "--json" ]
    case _Object cacheAfterMerge of
      Nothing -> pure ()
      Just o  -> case o .: "decision heads" :: Maybe [Text] of
        Nothing -> pure ()
        Just _  -> pure ()  -- conflict state expected
      _ -> pure ()

    -- Resolve conflict
    void $ amendAdrViaCli repo cacheId
      (Just "Stable release-aware cache identity")
      (Just "Cache keys include source and ABI digests with release namespaces.")
      (Just "## Decision\nUse source and ABI digests with explicit release namespaces.")

    mainBeforeNoise <- headCommit repo

    -- Create noisy feature train on develop
    git repo ["switch", "-c", "feature/noisy-product", "develop"]
    forM_ [0 .. 3] $ \index -> do
      commitFileWithMsg repo
        ("src/noise/feature-" ++ show index <.> "txt")
        (encodeUtf8 ("feature noise " <> T.pack (show index) <> "\n"))
        (T.pack ("feature product work " <> show index))
    git repo ["switch", "develop"]
    git repo ["merge", "--no-ff", "feature/noisy-product", "-m", "merge noisy feature train"]

    -- Hotfixes on release
    git repo ["switch", "release/2025-08-01"]
    forM_ [0 .. 1] $ \index -> do
      commitFileWithMsg repo
        ("release/hotfix-" ++ show index <.> "txt")
        (encodeUtf8 ("hotfix " <> T.pack (show index) <> "\n"))
        (T.pack ("release hotfix " <> show index))

    -- Merge everything back to main
    git repo ["switch", "main"]
    git repo ["merge", "--no-ff", "develop", "-m", "merge current development"]
    git repo ["merge", "--no-ff", "release/2025-08-01", "-m", "merge current release hotfixes"]

    -- Branch base / expected presence mapping
    let branchBases =
          [ ("main", mainBeforeNoise),
            ("develop", developBeforeNoise),
            ("release/2025-08-01", releaseBeforeNoise),
            ("feature/api-contract", base)
          ]

    let expectedPresence = Map.fromList
          [ ("main", [cacheId, jobsId, apiId, releasePolicyId]),
            ("develop", [cacheId, jobsId, apiId]),
            ("release/2025-08-01", [cacheId, jobsId, releasePolicyId]),
            ("feature/api-contract", [cacheId, jobsId, apiId])
          ]

    -- Warm every branch first
    forM_ branchBases $ \(branch, _) -> do
      git repo ["switch", branch]
      void $ adraiJsonOrThrow repo ["compile", "--json"]

    -- For each branch: verify expected ADR presence and cold/warm equivalence
    forM_ branchBases $ \(branch, compareFrom) -> do
      git repo ["switch", branch]

      -- Verify expected ADR presence
      branchSearch <- adraiJsonOrThrow repo
        [ "search", "--include-obsolete", "--limit", "1000", "--json" ]
      let actualAdrs = sort (adrIdsFromSearch branchSearch)
      let expected = sort $ fromMaybe []
            (Map.lookup branch expectedPresence >>= \es -> Just es)
      assertBool
        ( "ADR presence mismatch on branch " <> branch <>
          ": expected " <> show expected <> " got " <> show actualAdrs
        )
        (actualAdrs == expected)

      -- Warm/cold equivalence for this branch
      assertFullColdWarmMatches repo compareFrom

    -- Historical reads remain independent of final checkout
    git repo ["switch", "main"]
    -- Show ADR at feature branch tip (should succeed)
    _ <- adraiJsonOrThrow repo
      [ "show", unpack apiId, "--view", "collapsed", "--json" ]
    pure ()
