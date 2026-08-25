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

import Adrai.Integration.CLI hiding (adraiJson, adraiJsonOrThrow, tablesEqual)
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
import Database.SQLite.Simple
  ( Only (..),
    SQLData,
    close,
    open,
    query_,
  )
import Database.SQLite.Simple.Types (Query(Query))
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    removeDirectoryRecursive,
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), isAbsolute, takeDirectory, (<.>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (proc, readProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

-- ---------------------------------------------------------------------------
-- JSON helper accessors
-- ---------------------------------------------------------------------------

-- | Run the executable selected for the standard test component without
-- replacing its inherited environment.  The shared integration helper pins a
-- minimal Git environment, which is appropriate for direct Git fixtures but
-- prevents the native executable from resolving @git@ on Windows.
adraiJson :: FilePath -> [String] -> IO (Either Text Data.Aeson.Value)
adraiJson repoPath arguments = do
  (exitCode, stdout, stderr) <- adraiRaw repoPath arguments
  case exitCode of
    ExitSuccess ->
      case Data.Aeson.decode stdout of
        Just value -> pure (Right value)
        Nothing -> pure (Left "JSON parse error: invalid JSON")
    ExitFailure code ->
      pure
        ( Left
            ( "CLI failed (exit " <> T.pack (show code) <> "): "
                <> decodeUtf8 (LBS.toStrict stderr)
            )
        )

adraiRaw :: FilePath -> [String] -> IO (ExitCode, LBS.ByteString, LBS.ByteString)
adraiRaw repoPath arguments = do
  executable <- lookupEnv "ADRAI_EXE" >>= \case
    Just path | not (null path) && isAbsolute path -> pure path
    _ -> fail "ConsistencyTest requires ADRAI_EXE to name an absolute executable under test"
  readProcess (proc executable (adraiTestArgs repoPath arguments))

adraiJsonOrThrow :: FilePath -> [String] -> IO Data.Aeson.Value
adraiJsonOrThrow repoPath arguments = do
  result <- adraiJson repoPath arguments
  case result of
    Left problem -> fail ("adrai " <> unwords arguments <> " error: " <> unpack problem)
    Right value -> pure value

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

valText :: Data.Aeson.Value -> Maybe Text
valText (Data.Aeson.String t) = Just t
valText _                     = Nothing

-- | Extract "adr" field from a search result object.
adrFromResult :: Data.Aeson.Value -> Maybe Text
adrFromResult v = do
  km <- _Object v
  km .: "adr"

-- | Extract the ADR ID from a create result value.
extractAdrId :: Data.Aeson.Value -> Maybe Text
extractAdrId v = do
  o <- _Object v
  o .: "adr"

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
  createConsistencyAdr
    repo
    "Stable cache identity"
    "Cache identity derives from semantic inputs."
    "## Context\nBuilds move between workspaces.\n\n## Decision\nCache keys exclude absolute workspace paths and use source digests.\n\n## Consequences\nInputs must be normalized."
    ["compiler.cache"]
    ["src/compiler/cache/**"]
    "llm:planner"
    (Just "demo-model")

-- | Create the "jobs / durable delivery" fixture ADR.
createJobsAdr
  :: FilePath
  -> IO Data.Aeson.Value
createJobsAdr repo =
  createConsistencyAdr
    repo
    "At-least-once job delivery"
    "Workers acknowledge durable jobs only after successful execution."
    "## Decision\nUse durable queues and idempotent job handlers."
    ["runtime.jobs"]
    ["src/jobs/**"]
    "llm:planner"
    (Just "demo-model")

createConsistencyAdr
  :: FilePath
  -> Text
  -> Text
  -> Text
  -> [Text]
  -> [Text]
  -> Text
  -> Maybe Text
  -> IO Data.Aeson.Value
createConsistencyAdr repo title summary body domains scopes actor model =
  adraiJsonOrThrow repo
    ( [ "create",
        "--title", unpack title,
        "--summary", unpack summary,
        "--body", unpack (canonicalBody body),
        "--actor", unpack actor
      ]
        <> maybe [] (\value -> ["--model", unpack value]) model
        <> concatMap (\domain -> ["--domain", unpack domain]) domains
        <> concatMap (\scope -> ["--applies-to", unpack scope]) scopes
        <> ["--json"]
    )

initializeConsistencyRepo :: FilePath -> IO ()
initializeConsistencyRepo repo = void (adraiJsonOrThrow repo ["init", "--json"])

canonicalBody :: Text -> Text
canonicalBody body
  | "\n" `T.isSuffixOf` body = body
  | otherwise = body <> "\n"

-- | Amend an ADR through the current public CLI.
amendAdrViaCli
  :: FilePath
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> IO Data.Aeson.Value
amendAdrViaCli repo adrId maybeTitle maybeSummary maybeBody =
  adraiJsonOrThrow repo
    ( [ "amend", unpack adrId,
        "--change-summary", "consistency oracle amendment",
        "--actor", "human:architect"
      ]
        <> concat
          [ maybe [] (\v -> ["--title", unpack v]) maybeTitle,
            maybe [] (\v -> ["--summary", unpack v]) maybeSummary,
            maybe [] (\v -> ["--body", unpack (canonicalBody v)]) maybeBody
          ]
        <> ["--json"]
    )

-- | Commit arbitrary files in the repo (fixture helper).
commitFilesWithMsg
  :: FilePath
  -> [(FilePath, BS.ByteString)]
  -> Text
  -> IO Text
commitFilesWithMsg repo files message = do
  mapM_ writeAndCommit files
  git repo (["--literal-pathspecs", "add", "--"] <> map fst files)
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

-- | Get all semantic table contents from a database.
semanticTableContents :: FilePath -> IO [(String, [[SQLData]])]
semanticTableContents dbPath =
  let fetchTable (Only tblName) conn = do
        let q = Query (T.pack ("SELECT * FROM " ++ unpack tblName))
        rows <- query_ conn q :: IO [[SQLData]]
        pure (T.unpack tblName, sortOn show rows)
  in do
    conn <- open dbPath
    tables <- query_ conn "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"
      :: IO [Only Text]
    results <- forM tables (\tbl -> fetchTable tbl conn)
    close conn
    pure $ sortOn fst results

tablesEqual :: Eq value => [(String, [[value]])] -> [(String, [[value]])] -> Bool
tablesEqual = (==)

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
  , snapTables     :: [(String, [[SQLData]])]
  } deriving (Eq, Show)

-- | The semantic state that every public channel in a lightweight snapshot
-- must expose. Healthy snapshots fail fast on any CLI error; conflicted
-- snapshots accept only the one frozen decision-conflict shape.
data PublicSnapshotExpectation
  = ExpectHealthy
  | ExpectDecisionConflict Text
  deriving (Eq, Show)

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
  let adrIds = sort (adrIdsFromSearch allResults)
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
    [ "search", "cache identity durable", "--mode", "fts",
      "--include-obsolete", "--limit", "50", "--json" ]
  vector <- adraiJsonOrThrow repo
    [ "search", "memoized artifact fingerprint", "--mode", "vector",
      "--include-obsolete", "--limit", "50", "--json" ]
  hybrid <- adraiJsonOrThrow repo
    [ "search", "durable worker acknowledgement", "--mode", "hybrid",
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
    [ "compare", unpack compareFrom, "HEAD",
      "--include-unchanged", "--json" ]
  -- Strip cache field from compare
  let strippedCompare = stripCacheField compareResult

  -- Doctor
  doctorResult <- adraiJsonOrThrow repo ["doctor", "--json"]
  let strippedDoctor = stripCacheField doctorResult

  -- Semantic database tables
  let dbPath = repo </> ".adrai" </> "index.sqlite"
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
  mapMaybe adrFromResult (getSearchResults v)

-- | Retain a successful JSON response or one exact, expected semantic
-- conflict. Any other public CLI failure remains a test failure.
publicJsonOrConflict :: FilePath -> [String] -> Text -> IO Data.Aeson.Value
publicJsonOrConflict repo arguments expectedFailure = do
  result <- adraiJson repo arguments
  case result of
    Right value -> pure value
    Left failure
      | failure == expectedFailure -> pure (Data.Aeson.String failure)
      | otherwise -> fail ("adrai " <> unwords arguments <> " error: " <> unpack failure)

showConflictFailure :: Text
showConflictFailure =
  "CLI failed (exit 3): adrai: conflict: ADR requires resolution: 2 decision heads\n"

searchConflictFailure :: Text
searchConflictFailure =
  "CLI failed (exit 3): adrai: conflict: search results require resolution: 2 decision heads\n"

-- | Get the public resolution requirement from a collapsed projection or its
-- frozen conflict failure.
getConflict :: Data.Aeson.Value -> Maybe Bool
getConflict v =
  case _Object v of
    Just o -> o .: "resolution_required"
    Nothing ->
      case v of
        Data.Aeson.String failure
          | "adrai: conflict: ADR requires resolution: 2 decision heads" `T.isInfixOf` failure -> Just True
        _ -> Nothing

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

requireDoctorJson :: FilePath -> ExitCode -> IO Data.Aeson.Value
requireDoctorJson repo expectedExit = do
  (exitCode, stdout, stderr) <- adraiRaw repo ["doctor", "--json"]
  exitCode @?= expectedExit
  stderr @?= ""
  case Data.Aeson.decode stdout of
    Nothing -> assertFailure "doctor did not emit valid JSON"
    Just value -> pure value

assertConflictDoctor :: FilePath -> Text -> IO Data.Aeson.Value
assertConflictDoctor repo expectedAdr = do
  doctor <- requireDoctorJson repo (ExitFailure 4)
  getDoctorOk doctor @?= Just False
  doctorIssues <-
    case _Object doctor >>= (.: "issues") of
      Just values -> pure (values :: [Data.Aeson.Value])
      Nothing -> assertFailure "conflicted doctor output is missing issues"
  connection <- open (repo </> ".adrai" </> "index.sqlite")
  issueRows <- query_ connection
    "SELECT code,adr_id,message FROM issue WHERE code='ADR_CONFLICT' ORDER BY ordinal"
    :: IO [(Text, Maybe Text, Text)]
  conflictRows <- query_ connection
    "SELECT adr_id,state_token,summaries FROM adr_conflict ORDER BY adr_id"
    :: IO [(Text, Text, Text)]
  close connection
  case (doctorIssues, issueRows, conflictRows) of
    ( [Data.Aeson.Object issue]
      , [("ADR_CONFLICT", Just issueAdr, issueMessage)]
      , [(conflictAdr, stateToken, summaries)]
      ) -> do
        issueAdr @?= expectedAdr
        conflictAdr @?= expectedAdr
        issueMessage @?= T.intercalate "; " (T.splitOn "\n" summaries)
        (issue .: "code" :: Maybe Text) @?= Just "ADR_CONFLICT"
        (issue .: "adr_id" :: Maybe Text) @?= Just expectedAdr
        (issue .: "state_token" :: Maybe Text) @?= Just stateToken
        (issue .: "conflicts" :: Maybe [Text]) @?= Just (T.splitOn "\n" summaries)
    other -> assertFailure ("unexpected doctor conflict join: " <> show other)
  pure doctor

assertHealthyDoctor :: FilePath -> IO Data.Aeson.Value
assertHealthyDoctor repo = do
  doctor <- requireDoctorJson repo ExitSuccess
  getDoctorOk doctor @?= Just True
  case _Object doctor >>= (.: "issues") of
    Just values -> (values :: [Data.Aeson.Value]) @?= []
    Nothing -> assertFailure "healthy doctor output is missing issues"
  pure doctor

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
publicSnapshot :: FilePath -> PublicSnapshotExpectation -> [Text] -> IO Snapshot
publicSnapshot repo expectation adrIds = do
  compiled <- adraiJsonOrThrow repo ["compile", "--json"]
  let revision = fromMaybe "" (lookupText compiled "revision")

  visible <- snapshotJson
    [ "search", "--include-obsolete", "--limit", "1000", "--json" ]
    searchConflictFailure
  visibleAdrs <-
    case (expectation, visible) of
      (ExpectDecisionConflict _, Data.Aeson.String failure)
        | failure == searchConflictFailure -> pure (sort adrIds)
      (ExpectDecisionConflict _, value) ->
        assertFailure ("conflicted snapshot must expose the frozen search conflict, got " <> show value)
      (ExpectHealthy, value) -> do
        let discovered = sort (adrIdsFromSearch value)
            expected = sort adrIds
        if discovered == expected
          then pure discovered
          else assertFailure ("visible ADR set mismatch: expected " <> show expected <> ", got " <> show discovered)

  -- Collapsed and exploded projections for each ADR
  collapsedMap <- Map.fromList <$> mapM (\adrId -> do
    v <- snapshotJson
      [ "show", unpack adrId, "--view", "collapsed", "--json" ]
      showConflictFailure
    pure (adrId, v)
    ) visibleAdrs

  explodedMap <- Map.fromList <$> mapM (\adrId -> do
    v <- snapshotJson
      [ "show", unpack adrId, "--view", "exploded", "--json" ]
      showConflictFailure
    pure (adrId, v)
    ) visibleAdrs

  -- Searches
  fts <- snapshotJson
    [ "search", "content digest queue acknowledgement", "--mode", "fts",
      "--include-obsolete", "--limit", "100", "--json" ]
    searchConflictFailure
  vector <- snapshotJson
    [ "search", "memoized artifact fingerprint", "--mode", "vector",
      "--include-obsolete", "--limit", "100", "--json" ]
    searchConflictFailure
  hybrid <- snapshotJson
    [ "search", "durable worker execution", "--mode", "hybrid",
      "--include-obsolete", "--limit", "100", "--json" ]
    searchConflictFailure
  fileCache <- snapshotJson
    [ "search", "--file", "src/runtime/cache/Key.py",
      "--include-obsolete", "--limit", "100", "--json" ]
    searchConflictFailure
  fileJobs <- snapshotJson
    [ "search", "--file", "src/jobs/Worker.py",
      "--include-obsolete", "--limit", "100", "--json" ]
    searchConflictFailure
  domainCompiler <- snapshotJson
    [ "search", "--domain", "compiler",
      "--include-obsolete", "--limit", "100", "--json" ]
    searchConflictFailure
  domainRuntime <- snapshotJson
    [ "search", "--domain", "runtime",
      "--include-obsolete", "--limit", "100", "--json" ]
    searchConflictFailure

  let searchesMap = Map.fromList
        [ ("fts", fts),
          ("vector", vector),
          ("hybrid", hybrid),
          ("file_cache", fileCache),
          ("file_jobs", fileJobs),
          ("domain_compiler", domainCompiler),
          ("domain_runtime", domainRuntime)
        ]

  case expectation of
    ExpectHealthy -> pure ()
    ExpectDecisionConflict _ -> do
      forM_ (Map.elems collapsedMap <> Map.elems explodedMap) $ \projection ->
        projection @?= Data.Aeson.String showConflictFailure
      forM_ ["fts", "vector", "hybrid", "domain_compiler"] $ \key ->
        Map.lookup key searchesMap @?= Just (Data.Aeson.String searchConflictFailure)
      forM_ ["file_cache", "file_jobs", "domain_runtime"] $ \key ->
        case Map.lookup key searchesMap of
          Nothing -> assertFailure ("missing conflicted search projection: " <> unpack key)
          Just outcome -> getSearchResults outcome @?= []

  doctor <-
    case expectation of
      ExpectHealthy -> assertHealthyDoctor repo
      ExpectDecisionConflict expectedAdr -> assertConflictDoctor repo expectedAdr
  let strippedDoctor = stripCacheField doctor

  -- Database tables
  let dbPath = repo </> ".adrai" </> "index.sqlite"
  tables <- semanticTableContents dbPath

  pure Snapshot
    { snapRevision = revision
    , snapAdrs = visibleAdrs
    , snapCollapsed = collapsedMap
    , snapExploded = explodedMap
    , snapSearches = searchesMap
    , snapCompare = Nothing
    , snapDoctor = Just strippedDoctor
    , snapTables = tables
    }
  where
    snapshotJson arguments conflictFailure =
      case expectation of
        ExpectHealthy -> adraiJsonOrThrow repo arguments
        ExpectDecisionConflict _ -> publicJsonOrConflict repo arguments conflictFailure

-- ---------------------------------------------------------------------------
-- Helpers for "withColdRebuild" pattern
-- ---------------------------------------------------------------------------

-- | Take a snapshot, remove .adrai, take another snapshot, and verify
-- they are identical (both public API and all 20 semantic tables).
assertColdWarmMatches :: FilePath -> PublicSnapshotExpectation -> [Text] -> IO ()
assertColdWarmMatches repo expectation adrIds = do
  warm <- publicSnapshot repo expectation adrIds
  removeAdraiIfExists repo
  cold <- publicSnapshot repo expectation adrIds

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
    initializeConsistencyRepo repo

    -- Create fixture ADRs
    cache <- createCacheAdr repo
    jobs <- createJobsAdr repo
    -- Commit representative source files
    _ <- commitFilesWithMsg repo
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
    _ <- adraiJsonOrThrow repo
      [ "domain", unpack cacheId,
        "--refine", "compiler.cache=compiler.cache.identity",
        "--reason", "refine cache domain",
        "--actor", "human:compiler-owner", "--json"
      ]

    -- Mutation: change scope
    _ <- adraiJsonOrThrow repo
      [ "scope", unpack cacheId,
        "--add", "src/runtime/cache/**",
        "--reason", "expand cache scope",
        "--actor", "human:compiler-owner", "--json"
      ]

    -- Mutation: obsolete then reactivate ADR (jobs)
    _ <- adraiJsonOrThrow repo
      [ "obsolete", unpack jobsId,
        "--reason", "A replacement was expected to own queue acknowledgement.",
        "--actor", "human:runtime-owner", "--json"
      ]
    _ <- adraiJsonOrThrow repo
      [ "reactivate", unpack jobsId,
        "--reason", "The replacement did not cover worker acknowledgement semantics.",
        "--actor", "human:runtime-owner", "--json"
      ]

    -- First cold/warm verification on main
    let allAdrIds = [cacheId, jobsId]
    assertColdWarmMatches repo ExpectHealthy allAdrIds

    -- Create develop branch with noise commits
    git repo ["switch", "-c", "develop"]
    forM_ [0 :: Int .. 2] $ \index -> do
      void $ commitFileWithMsg repo
        ("src/noise/develop-" ++ show index <.> "txt")
        (encodeUtf8 ("develop noise " <> T.pack (show index) <> "\n"))
        (T.pack ("develop: noisy product change " <> show index))

    -- Create feature branch with observability ADR
    git repo ["switch", "-c", "feature/observability"]
    observability <-
      createConsistencyAdr
        repo
        "Structured runtime telemetry"
        "Runtime operations emit stable structured telemetry events."
        "## Decision\nEmit structured events with stable names and correlation identifiers."
        ["runtime.observability"]
        ["src/runtime/telemetry/**"]
        "llm:feature-agent"
        (Just "planner-v1")
    let observabilityId = fromMaybe "" (extractAdrId observability)
    let allAdrIds' = [cacheId, jobsId, observabilityId]

    _ <- commitFileWithMsg repo
      ("src/runtime/telemetry/README.md")
      "telemetry implementation noise\n"
      "feature: implement telemetry plumbing"

    -- Merge feature into develop
    git repo ["switch", "develop"]
    git repo ["merge", "--no-ff", "feature/observability", "-m", "merge observability feature"]
    _ <- commitFilesWithMsg repo
      [ ("src/noise/after-merge.txt", "post merge development noise\n") ]
      "develop: continue after feature merge"

    -- Second cold/warm verification on develop
    assertColdWarmMatches repo ExpectHealthy allAdrIds'

    -- Switch to main, merge develop
    git repo ["switch", "main"]
    git repo ["merge", "--no-ff", "develop", "-m", "merge development train"]
    mainTip <- headCommit repo

    -- Verify on main: observability ADR is visible
    assertColdWarmMatches repo ExpectHealthy allAdrIds'

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
    assertColdWarmMatches repo ExpectHealthy allAdrIds

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
      [ "compare", "release/2025-08-01", unpack mainTip, "--json" ]
    let entries = fromMaybe [] (getCompareEntries comparison)
    let addedAdrs = mapMaybe getEntryAdr $ filter ((== Just "added") . getEntryKind) entries
    let changedAdrs = mapMaybe getEntryAdr $ filter ((== Just "changed") . getEntryKind) entries
    assertBool "observability ADR should be in added list"
      (observabilityId `elem` addedAdrs)
    assertBool "cache ADR should be in changed list"
      (cacheId `elem` changedAdrs)

    -- Repeated switching must recover exactly the same state
    forM_ [0 :: Int .. 2] $ \_ -> do
      git repo ["switch", "main"]
      _ <- publicSnapshot repo ExpectHealthy allAdrIds'
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
    initializeConsistencyRepo repo

    -- Create initial ADR on main
    created <- createCacheAdr repo
    let createdId = fromMaybe "" (extractAdrId created)

    -- Commit fixture source files
    _ <- commitFilesWithMsg repo
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
    conflictedSnap <- publicSnapshot repo (ExpectDecisionConflict createdId) [createdId]
    let collapsed = Map.lookup createdId (snapCollapsed conflictedSnap)
    case collapsed of
      Nothing -> assertFailure "could not find collapsed ADR in conflicted snapshot"
      Just v  -> do
        let conflict = getConflict v
        assertBool "ADR should have conflict=true after divergent merge"
          (conflict == Just True)
        v @?= Data.Aeson.String "CLI failed (exit 3): adrai: conflict: ADR requires resolution: 2 decision heads\n"
    let conflictSearchFailure =
          Data.Aeson.String
            "CLI failed (exit 3): adrai: conflict: search results require resolution: 2 decision heads\n"
    forM_ ["domain_compiler", "fts", "hybrid", "vector"] $ \key ->
      Map.lookup key (snapSearches conflictedSnap) @?= Just conflictSearchFailure
    forM_ ["domain_runtime", "file_cache", "file_jobs"] $ \key ->
      case Map.lookup key (snapSearches conflictedSnap) of
        Nothing -> assertFailure ("missing conflicted search projection: " <> unpack key)
        Just outcome -> getSearchResults outcome @?= []
    -- Cold/warm match in conflicted state
    assertColdWarmMatches repo (ExpectDecisionConflict createdId) [createdId]

    -- Resolve by amending again
    resolved <- amendAdrViaCli repo createdId
      (Just "Stable cache identity")
      (Just "Cache identity includes source, compiler ABI, and target platform.")
      (Just "## Decision\nUse source, compiler ABI, and target-platform digests for cache identity.")

    -- Final cold/warm verification
    finalSnap <- publicSnapshot repo ExpectHealthy [createdId]
    let collapsed' = Map.lookup createdId (snapCollapsed finalSnap)
    case collapsed' of
      Nothing -> assertFailure "could not find collapsed ADR in final snapshot"
      Just v' -> do
        let conflict' = getConflict v'
        assertBool "ADR conflict should be resolved"
          (conflict' == Just False)

        -- The resolved record should match the ordinary amend result.
        let resolvedRecord = fromMaybe "" (lookupString resolved "record")
            collapsedRecord' = fromMaybe "" (getRecord v' >>= valText)
        assertBool "resolved record should match collapsed record"
          (T.pack resolvedRecord == collapsedRecord')
    -- Cold/warm match after resolution
    assertColdWarmMatches repo ExpectHealthy [createdId]

    -- The reconciliation operation must retain both divergent parents; the
    -- resolved collapsed output above proves that this is now one current
    -- decision rather than a remaining conflict.
    case _Object resolved >>= (.: "amends") :: Maybe [Text] of
      Just parents -> assertBool "reconciliation must retain both decision heads as parents" (length parents == 2)
      Nothing -> assertFailure "ordinary reconciliation result must expose both amended parents"

-- =====================================================================
-- Test 3: Merge-heavy branch caches are semantically identical
-- Port of test_cache_equivalence.py::test_merge_heavy_branch_caches_are_semantically_identical_to_cold_rebuilds
-- =====================================================================

testMergeHeavyBranchEquivalence :: IO ()
testMergeHeavyBranchEquivalence =
  withSystemTempDirectory "adrai equivalence merge-heavy" $ \tmpDir -> do
    repo <- createTestRepo tmpDir
    initializeConsistencyRepo repo

    -- Create fixture ADRs on main
    cache <- createCacheAdr repo
    jobs <- createJobsAdr repo
    let cacheId = fromMaybe "" (extractAdrId cache)
        jobsId  = fromMaybe "" (extractAdrId jobs)

    let base = "main"  -- start from base (HEAD = main after init)

    -- Create release branch
    git repo ["branch", "release/2025-08-01"]

    -- Amend cache on main
    void $ amendAdrViaCli repo cacheId
      (Just "Stable cache identity and ABI")
      (Just "Cache identity includes semantic inputs and compiler ABI.")
      (Just "## Decision\nUse normalized source and compiler ABI digests.")

    -- Change scope on main
    _ <- adraiJsonOrThrow repo
      [ "scope", unpack cacheId,
        "--add", "src/runtime/cache/**",
        "--reason", "expand cache scope",
        "--actor", "human:main-architect", "--json"
      ]

    -- Change domains on main
    _ <- adraiJsonOrThrow repo
      [ "domain", unpack cacheId,
        "--refine", "compiler.cache=compiler.cache.identity",
        "--reason", "refine cache domain",
        "--actor", "human:main-architect", "--json"
      ]

    -- Create develop branch
    git repo ["branch", "develop"]

    -- Switch to feature branch from develop
    git repo ["switch", "-c", "feature/api-contract", "develop"]
    api <-
      createConsistencyAdr
        repo
        "Versioned API contracts"
        "Public APIs evolve through explicit versioned contracts."
        "## Decision\nPublish versioned endpoint schemas and compatibility windows."
        ["api.compatibility"]
        ["src/api/**"]
        "llm:feature-architect"
        (Just "architecture-agent")
    let apiId = fromMaybe "" (extractAdrId api)

    -- Amend jobs on feature
    void $ amendAdrViaCli repo jobsId Nothing
      (Just "Workers acknowledge durable jobs after idempotent completion.")
      (Just "## Decision\nUse durable queues, idempotency keys, and post-completion acknowledgement.")

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

    releasePolicy <-
      createConsistencyAdr
        repo
        "Release hotfix policy"
        "Release branches accept narrowly scoped verified hotfixes."
        "## Decision\nRequire focused hotfix commits and release verification."
        ["delivery.release"]
        ["release/**"]
        "human:release-manager"
        (Just "demo-model")
    let releasePolicyId = fromMaybe "" (extractAdrId releasePolicy)

    releaseBeforeNoise <- headCommit repo

    -- Switch back to main and merge release
    git repo ["switch", "main"]
    git repo ["merge", "--no-ff", "release/2025-08-01", "-m", "merge release architecture"]

    -- Verify conflict state after release merge
    cacheAfterMerge <- publicJsonOrConflict repo
      [ "show", unpack cacheId, "--view", "collapsed", "--json" ]
      showConflictFailure
    getConflict cacheAfterMerge @?= Just True

    -- Resolve conflict
    void $ amendAdrViaCli repo cacheId
      (Just "Stable release-aware cache identity")
      (Just "Cache keys include source and ABI digests with release namespaces.")
      (Just "## Decision\nUse source and ABI digests with explicit release namespaces.")

    mainBeforeNoise <- headCommit repo

    -- Create noisy feature train on develop
    git repo ["switch", "-c", "feature/noisy-product", "develop"]
    forM_ [0 :: Int .. 3] $ \index -> do
      void $ commitFileWithMsg repo
        ("src/noise/feature-" ++ show index <.> "txt")
        (encodeUtf8 ("feature noise " <> T.pack (show index) <> "\n"))
        (T.pack ("feature product work " <> show index))
    git repo ["switch", "develop"]
    git repo ["merge", "--no-ff", "feature/noisy-product", "-m", "merge noisy feature train"]

    -- Hotfixes on release
    git repo ["switch", "release/2025-08-01"]
    forM_ [0 :: Int .. 1] $ \index -> do
      void $ commitFileWithMsg repo
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
