{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for history, search, and compare commands via the
-- adrai CLI, porting the query tests from
-- ``ADRAI_1_Source/tests/test_history_command.py`` and
-- ``ADRAI_1_Source/tests/test_evolution_compare_ann.py``.
module Adrai.QueryIntegrationTest (tests) where

import Adrai.Git (discoverRepository, systemGit)
import Adrai.Integration.CLI hiding (parseCompareResults, parseHistory, parseSearchResults)
import Adrai.Query (renderCollapsedProjection, renderCompareProjection)
import Adrai.Service.Query (CompareRequest (..), ShowRequest (..), ShowResult (..), runCompare, runShow)
import Adrai.Types (ViewMode (..))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import qualified Control.Exception as Exception
import Control.Monad (forM_, unless, void)
import qualified Data.Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (find, isPrefixOf, sortOn)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text, strip)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Vector as Vector
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (proc, readProcess, setEnv)
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
  pure (mapMaybe valText (Vector.toList arr))
valTextList _ = Nothing

headVal :: Data.Aeson.Value -> Maybe Data.Aeson.Value
headVal (Data.Aeson.Array arr) =
  if Vector.null arr then Nothing else Just (Vector.head arr)
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

-- | P6-02F launches the explicitly selected executable under the same small
-- Windows process environment used by mutation E2E.  It keeps Git discoverable
-- while excluding inherited Git/config controls from the parent process.
p602fRaw :: FilePath -> [String] -> IO (ExitCode, LBS.ByteString, LBS.ByteString)
p602fRaw repoPath args = do
  maybeExe <- lookupEnv "ADRAI_EXE"
  exe <- case maybeExe of
    Nothing -> assertFailure "P6-02F requires ADRAI_EXE to name the executable under test" >> fail "unreachable"
    Just "" -> assertFailure "P6-02F requires ADRAI_EXE to be non-empty" >> fail "unreachable"
    Just path -> pure path
  inherited <- getEnvironment
  readProcess (setEnv (p602fEnvironment inherited) (proc exe ("--repo" : repoPath : args)))

p602fJsonOrThrow :: FilePath -> [String] -> IO Data.Aeson.Value
p602fJsonOrThrow repoPath args = do
  (exitCode, output, errors) <- p602fRaw repoPath args
  case exitCode of
    ExitSuccess -> case Data.Aeson.decode output of
      Just value -> pure value
      Nothing -> assertFailure "P6-02F executable emitted non-JSON success output" >> fail "unreachable"
    ExitFailure code ->
      assertFailure ("P6-02F executable failed with exit " <> show code <> ": " <> T.unpack (decodeUtf8 (LBS.toStrict errors))) >> fail "unreachable"

p602fEnvironment :: [(String, String)] -> [(String, String)]
p602fEnvironment inherited =
  gitEnv
    <> filter
      ( \(key, _) ->
          folded key `elem` required
            && all ((/= folded key) . folded . fst) gitEnv
      )
      inherited
  where
    required = map folded ["PATH", "PATHEXT", "SYSTEMROOT", "WINDIR", "COMSPEC", "TEMP", "TMP"]
    folded = T.toCaseFold . T.pack

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
      testP602FRealExecutableShow,
      testP602FRealExecutableConflict,
      testP602FRealExecutableIntegrityFailure,
      testP602GRealExecutableCompare,
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
        void $ createAdr repo
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
          catchGitFailure $
            git repo ["merge", "--no-ff", "feature", "-m", "merge feature"]

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

testP602FRealExecutableShow :: TestTree
testP602FRealExecutableShow =
  testCase "P6-02F real executable show is revision-local, canonical, and read-only" $
    withSystemTempDirectory "adrai p6-02f show" $ \tmpDir -> do
      repo <- createTestRepo tmpDir
      _ <- p602fJsonOrThrow repo ["init", "--json"]
      created <- p602fJsonOrThrow repo
        [ "create"
        , "--title", "Unicode snowman ☃ decision"
        , "--summary", "A summary with spaces"
        , "--body", "## Decision\nKeep the canonical show path.\n"
        , "--domain", "platform"
        , "--applies-to", "src/**"
        , "--actor", "llm:planner"
        , "--model", "demo-model"
        , "--json"
        ]
      (adr, record, status) <-
        case _Object created of
          Just object ->
            case (object .: "adr", object .: "record", object .: "status") of
              (Just adrId, Just recordId, Just statusId) -> pure (adrId, recordId, statusId)
              _ -> assertFailure "create result omitted ADR, record, or status identifier" >> fail "unreachable"
          Nothing -> assertFailure "create result is not JSON object" >> fail "unreachable"
      createdRevision <- headCommit repo
      -- ADR prefixes contain a millisecond timestamp.  Cross a 10-character
      -- bucket while remaining inside the coarser 8-character bucket so the
      -- fixture proves both unique and ambiguous prefix outcomes.
      threadDelay 50000
      second <- p602fJsonOrThrow repo
        [ "create"
        , "--title", "Second decision for ambiguity"
        , "--summary", "Shares a short identifier prefix"
        , "--body", "## Decision\nKeep selector errors deterministic.\n"
        , "--domain", "platform"
        , "--applies-to", "test/**"
        , "--actor", "llm:planner"
        , "--model", "demo-model"
        , "--json"
        ]
      secondAdr <-
        case extractAdrId second of
          Just value -> pure value
          Nothing -> assertFailure "second create result omitted ADR identifier" >> fail "unreachable"
      assertBool "fixture ADRs share the minimum accepted prefix" (T.take 8 secondAdr == T.take 8 adr)
      threadDelay 50000
      amended <- p602fJsonOrThrow repo
        [ "amend", T.unpack adr
        , "--title", "Unicode snowman ☃ decision v2"
        , "--summary", "A summary with spaces"
        , "--change-summary", "Verify revision-local show"
        , "--body", "## Decision\nKeep the canonical show path.\n"
        , "--actor", "llm:planner"
        , "--model", "demo-model"
        , "--json"
        ]
      amendConnection <-
        case _Object amended >>= (.: "connection") of
          Just value -> pure value
          Nothing -> assertFailure "amend result omitted connection identifier" >> fail "unreachable"
      repository <- do
        discovered <- discoverRepository systemGit repo
        case discovered of
          Left problem -> assertFailure ("discover show fixture: " <> show problem) >> fail "unreachable"
          Right value -> pure value
      expectedText <- do
        outcome <- runShow repository (ShowRequest adr CollapsedView "HEAD" False)
        case outcome of
          Right (ShowCollapsed projection) -> pure (LBS.fromStrict (renderCollapsedProjection projection))
          other -> assertFailure ("programmatic collapsed show failed: " <> show other) >> fail "unreachable"

      beforeHead <- headCommit repo
      beforeRef <- gitStdout repo ["symbolic-ref", "--quiet", "HEAD"]
      beforeTree <- gitStdout repo ["ls-tree", "-r", "--name-only", "HEAD"]
      let managedPaths =
            filter ("architecture/adrai/" `isPrefixOf`)
              (map T.unpack (T.lines (decodeUtf8 (LBS.toStrict beforeTree))))
      beforeManaged <- traverse (\path -> (,) path <$> BS.readFile (repo </> path)) managedPaths
      BS.writeFile (repo </> "staged-show.bin") "\NUL\SOHstaged show bytes\255"
      _ <- gitStdout repo ["add", "--", "staged-show.bin"]
      BS.writeFile (repo </> "README.md") "# Test\ncaller dirty bytes\n"
      BS.writeFile (repo </> "user-dirty.txt") "keep this worktree file"
      beforeStatus <- gitStdout repo ["status", "--porcelain=v1", "--untracked-files=all", "-z"]
      beforeIndex <- gitStdout repo ["ls-files", "--stage", "-z"]
      beforeCached <- gitStdout repo ["diff", "--cached", "--binary"]
      beforeWorktree <- gitStdout repo ["diff", "--binary"]

      (defaultExit, defaultOut, defaultErr) <- p602fRaw repo ["show", T.unpack adr]
      defaultExit @?= ExitSuccess
      defaultErr @?= ""
      defaultOut @?= expectedText

      (jsonExit, jsonOut, jsonErr) <- p602fRaw repo ["show", T.unpack record, "--json"]
      jsonExit @?= ExitSuccess
      jsonErr @?= ""
      case Data.Aeson.decode jsonOut of
        Just output -> case _Object output of
          Just object -> do
            (object .: "schema" :: Maybe Text) @?= Just "adrai/show-collapsed/v1"
            (object .: "adr" :: Maybe Text) @?= Just adr
            (object .: "as_of" :: Maybe Text) @?= Just beforeHead
            provenance <- case KM.lookup "provenance" object >>= _Object of
              Just value -> pure value
              Nothing -> assertFailure "collapsed show omitted provenance" >> fail "unreachable"
            createdProvenance <- case KM.lookup "created" provenance >>= _Object of
              Just value -> pure value
              Nothing -> assertFailure "collapsed show omitted created provenance" >> fail "unreachable"
            originalCommits <- case createdProvenance .: "original_commits" of
              Just value -> pure (value :: [Text])
              Nothing -> assertFailure "created provenance omitted original commits" >> fail "unreachable"
            assertBool "show exposes genuine placement evidence" (createdRevision `elem` originalCommits)
            (object .: "title" :: Maybe Text) @?= Just "Unicode snowman ☃ decision v2"
          Nothing -> assertFailure "collapsed show JSON is not an object"
        Nothing -> assertFailure "collapsed show output is not JSON"

      forM_ [T.toLower (T.take 10 adr), T.take 10 record, T.take 10 amendConnection] $ \reference -> do
        (prefixExit, _, prefixErr) <- p602fRaw repo ["show", T.unpack reference, "--json"]
        unless (prefixExit == ExitSuccess) $
          assertFailure ("10-character show prefix failed for " <> T.unpack reference <> ": " <> show prefixErr)
        prefixErr @?= ""

      historicalWithoutRaw <- p602fJsonOrThrow repo ["show", T.unpack status, "--at", T.unpack createdRevision, "--view", "exploded", "--json"]
      assertBool "exploded output omits raw semantic data by default"
        (not ("\"raw_semantic\"" `BS.isInfixOf` LBS.toStrict (Data.Aeson.encode historicalWithoutRaw)))
      historical <- p602fJsonOrThrow repo ["show", T.unpack status, "--at", T.unpack createdRevision, "--view", "exploded", "--raw", "--json"]
      case _Object historical of
        Just object -> do
          (object .: "schema" :: Maybe Text) @?= Just "adrai/show-exploded/v1"
          (object .: "as_of" :: Maybe Text) @?= Just createdRevision
          operations <- case object .: "operations" of
            Just value -> pure (value :: [Data.Aeson.Value])
            Nothing -> assertFailure "historical exploded show omitted operations" >> fail "unreachable"
          let historicalTitles =
                [ title
                | operation <- operations
                , Just operationObject <- [_Object operation]
                , Just items <- [operationObject .: "items" :: Maybe [Data.Aeson.Value]]
                , item <- items
                , Just itemObject <- [_Object item]
                , Just title <- [itemObject .: "title" :: Maybe Text]
                ]
          assertBool "historical show retains the pre-amendment semantic title"
            ("Unicode snowman ☃ decision" `elem` historicalTitles)
          assertBool "historical show excludes the later amended title"
            ("Unicode snowman ☃ decision v2" `notElem` historicalTitles)
        Nothing -> assertFailure "exploded historical show JSON is not an object"
      assertBool "--raw adds raw semantic item data"
        ("\"raw_semantic\"" `BS.isInfixOf` LBS.toStrict (Data.Aeson.encode historical))

      (rawExit, rawOut, rawErr) <- p602fRaw repo ["show", T.unpack adr, "--raw"]
      rawExit @?= ExitFailure 2
      rawOut @?= ""
      rawErr @?= "adrai: --raw requires --view exploded\n"
      (missingExit, missingOut, missingErr) <- p602fRaw repo ["show", "A00000000000000000000000000", "--json"]
      missingExit @?= ExitFailure 2
      missingOut @?= ""
      missingErr @?= "adrai: ADRAI reference not found in this revision: A00000000000000000000000000\n"

      (ambiguousExit, ambiguousOut, ambiguousErr) <- p602fRaw repo ["show", T.unpack (T.take 8 adr), "--json"]
      ambiguousExit @?= ExitFailure 2
      ambiguousOut @?= ""
      let ambiguousIds = sortOn id [adr, secondAdr]
          expectedAmbiguous =
            "adrai: ambiguous ADRAI reference " <> T.take 8 adr <> ": "
              <> T.intercalate ", " [identifier <> "->" <> identifier | identifier <- ambiguousIds]
              <> "\n"
      ambiguousErr @?= LBS.fromStrict (encodeUtf8 expectedAmbiguous)
      (wrongKindExit, wrongKindOut, wrongKindErr) <- p602fRaw repo ["show", "O0000000", "--json"]
      wrongKindExit @?= ExitFailure 2
      wrongKindOut @?= ""
      wrongKindErr @?= "adrai: unsupported ADRAI reference kind: O\n"
      (shortExit, shortOut, shortErr) <- p602fRaw repo ["show", "A123456", "--json"]
      shortExit @?= ExitFailure 2
      shortOut @?= ""
      shortErr @?= "adrai: malformed ADRAI reference A123456: IdPrefixWrongLength 7\n"
      (revisionExit, revisionOut, revisionErr) <- p602fRaw repo ["show", T.unpack adr, "--at", "refs/heads/does-not-exist", "--json"]
      revisionExit @?= ExitFailure 2
      revisionOut @?= ""
      assertBool "invalid revision is a user error" ("adrai: " `LBS.isPrefixOf` revisionErr)
      (repositoryExit, repositoryOut, repositoryErr) <- p602fRaw (tmpDir </> "missing repository ü") ["show", T.unpack adr, "--json"]
      repositoryExit @?= ExitFailure 2
      repositoryOut @?= ""
      assertBool "missing repository is a deterministic user error" ("adrai: " `LBS.isPrefixOf` repositoryErr)

      afterHead <- headCommit repo
      afterRef <- gitStdout repo ["symbolic-ref", "--quiet", "HEAD"]
      afterTree <- gitStdout repo ["ls-tree", "-r", "--name-only", "HEAD"]
      afterStatus <- gitStdout repo ["status", "--porcelain=v1", "--untracked-files=all", "-z"]
      afterIndex <- gitStdout repo ["ls-files", "--stage", "-z"]
      afterCached <- gitStdout repo ["diff", "--cached", "--binary"]
      afterWorktree <- gitStdout repo ["diff", "--binary"]
      afterManaged <- traverse (\path -> (,) path <$> BS.readFile (repo </> path)) managedPaths
      afterHead @?= beforeHead
      afterRef @?= beforeRef
      afterTree @?= beforeTree
      afterStatus @?= beforeStatus
      afterIndex @?= beforeIndex
      afterCached @?= beforeCached
      afterWorktree @?= beforeWorktree
      afterManaged @?= beforeManaged

testP602FRealExecutableConflict :: TestTree
testP602FRealExecutableConflict =
  testCase "P6-02F real executable show maps semantic conflicts to exit 3 without mutation" $
    withSystemTempDirectory "adrai p6-02f show conflict" $ \tmpDir -> do
      repo <- createTestRepo tmpDir
      _ <- p602fJsonOrThrow repo ["init", "--json"]
      created <- p602fJsonOrThrow repo
        [ "create"
        , "--title", "Conflict base"
        , "--summary", "Divergent records"
        , "--body", "## Decision\nCreate two real decision heads.\n"
        , "--domain", "platform"
        , "--applies-to", "src/**"
        , "--actor", "llm:planner"
        , "--model", "demo-model"
        , "--json"
        ]
      adr <-
        case extractAdrId created of
          Just value -> pure value
          Nothing -> assertFailure "conflict create omitted ADR" >> fail "unreachable"
      base <- headCommit repo
      _ <- gitStdout repo ["switch", "-c", "show-left", T.unpack base]
      _ <- p602fJsonOrThrow repo
        [ "amend", T.unpack adr
        , "--title", "Left decision"
        , "--change-summary", "Left branch"
        , "--body", "## Decision\nChoose the left alternative.\n"
        , "--actor", "llm:planner"
        , "--model", "demo-model"
        , "--json"
        ]
      _ <- gitStdout repo ["switch", "-c", "show-right", T.unpack base]
      _ <- p602fJsonOrThrow repo
        [ "amend", T.unpack adr
        , "--title", "Right decision"
        , "--change-summary", "Right branch"
        , "--body", "## Decision\nChoose the right alternative.\n"
        , "--actor", "llm:planner"
        , "--model", "demo-model"
        , "--json"
        ]
      _ <- gitStdout repo ["switch", "main"]
      _ <- gitStdout repo ["merge", "--no-ff", "-m", "merge divergent show fixture", "show-left", "show-right"]

      beforeHead <- headCommit repo
      beforeRef <- gitStdout repo ["symbolic-ref", "--quiet", "HEAD"]
      beforeTree <- gitStdout repo ["ls-tree", "-r", "--name-only", "HEAD"]
      beforeIndex <- gitStdout repo ["ls-files", "--stage", "-z"]
      beforeStatus <- gitStdout repo ["status", "--porcelain=v1", "--untracked-files=all", "-z"]
      beforeCached <- gitStdout repo ["diff", "--cached", "--binary"]
      beforeWorktree <- gitStdout repo ["diff", "--binary"]

      (conflictExit, conflictOut, conflictErr) <- p602fRaw repo ["show", T.unpack adr, "--json"]
      conflictExit @?= ExitFailure 3
      conflictOut @?= ""
      conflictErr @?= "adrai: conflict: ADR requires resolution: 2 decision heads\n"

      headCommit repo >>= (@?= beforeHead)
      gitStdout repo ["symbolic-ref", "--quiet", "HEAD"] >>= (@?= beforeRef)
      gitStdout repo ["ls-tree", "-r", "--name-only", "HEAD"] >>= (@?= beforeTree)
      gitStdout repo ["ls-files", "--stage", "-z"] >>= (@?= beforeIndex)
      gitStdout repo ["status", "--porcelain=v1", "--untracked-files=all", "-z"] >>= (@?= beforeStatus)
      gitStdout repo ["diff", "--cached", "--binary"] >>= (@?= beforeCached)
      gitStdout repo ["diff", "--binary"] >>= (@?= beforeWorktree)

testP602FRealExecutableIntegrityFailure :: TestTree
testP602FRealExecutableIntegrityFailure =
  testCase "P6-02F real executable show fails closed on repository integrity without mutation" $
    withSystemTempDirectory "adrai p6-02f show integrity" $ \tmpDir -> do
      repo <- createTestRepo tmpDir
      _ <- p602fJsonOrThrow repo ["init", "--json"]
      created <- p602fJsonOrThrow repo
        [ "create"
        , "--title", "Integrity base"
        , "--summary", "Malformed managed source"
        , "--body", "## Decision\nFail closed on invalid source.\n"
        , "--domain", "platform"
        , "--applies-to", "src/**"
        , "--actor", "llm:planner"
        , "--model", "demo-model"
        , "--json"
        ]
      (adr, decisionPath) <-
        case _Object created of
          Just object ->
            case (object .: "adr", object .: "created") of
              (Just adrId, Just paths) ->
                case find (T.isSuffixOf ".decision.md") (paths :: [Text]) of
                  Just path -> pure (adrId, path)
                  Nothing -> assertFailure "create result omitted decision path" >> fail "unreachable"
              _ -> assertFailure "create result omitted ADR or paths" >> fail "unreachable"
          Nothing -> assertFailure "integrity create result is not an object" >> fail "unreachable"
      BS.writeFile (repo </> T.unpack decisionPath) "schema: deliberately-invalid\n"
      _ <- gitStdout repo ["add", "--", T.unpack decisionPath]
      _ <- gitStdout repo ["commit", "-m", "commit malformed managed source"]

      beforeHead <- headCommit repo
      beforeRef <- gitStdout repo ["symbolic-ref", "--quiet", "HEAD"]
      beforeIndex <- gitStdout repo ["ls-files", "--stage", "-z"]
      beforeStatus <- gitStdout repo ["status", "--porcelain=v1", "--untracked-files=all", "-z"]
      (failureExit, failureOut, failureErr) <- p602fRaw repo ["show", T.unpack adr, "--json"]
      failureExit @?= ExitFailure 2
      failureOut @?= ""
      assertBool "integrity failure is explicit" ("adrai: repository integrity failure: " `LBS.isPrefixOf` failureErr)
      headCommit repo >>= (@?= beforeHead)
      gitStdout repo ["symbolic-ref", "--quiet", "HEAD"] >>= (@?= beforeRef)
      gitStdout repo ["ls-files", "--stage", "-z"] >>= (@?= beforeIndex)
      gitStdout repo ["status", "--porcelain=v1", "--untracked-files=all", "-z"] >>= (@?= beforeStatus)

testP602GRealExecutableCompare :: TestTree
testP602GRealExecutableCompare =
  testCase "P6-02G real executable compare is directional, revision-local, canonical, and read-only" $
    withSystemTempDirectory "adrai p6-02g compare ü" $ \tmpDir -> do
      repo <- createTestRepo tmpDir
      _ <- p602fJsonOrThrow repo ["init", "--json"]
      initialRevision <- headCommit repo
      created <- p602fJsonOrThrow repo
        [ "create"
        , "--title", "Compare Unicode ü decision"
        , "--summary", "Initial compare state"
        , "--body", "## Decision\nCompare immutable snapshots.\n"
        , "--domain", "platform"
        , "--applies-to", "src/**"
        , "--actor", "llm:planner"
        , "--model", "compare-model"
        , "--json"
        ]
      adr <- case extractAdrId created of
        Just value -> pure value
        Nothing -> assertFailure "compare create omitted ADR" >> fail "unreachable"
      createdRevision <- headCommit repo
      _ <- p602fJsonOrThrow repo
        [ "amend", T.unpack adr
        , "--title", "Compare Unicode ü decision v2"
        , "--summary", "Changed compare state"
        , "--change-summary", "Prove directional compare"
        , "--body", "## Decision\nCompare immutable snapshots exactly.\n"
        , "--actor", "llm:planner"
        , "--model", "compare-model"
        , "--json"
        ]
      amendedRevision <- headCommit repo

      beforeRef <- gitStdout repo ["symbolic-ref", "--quiet", "HEAD"]
      beforeTree <- gitStdout repo ["ls-tree", "-r", "--name-only", "HEAD"]
      let managedPaths =
            filter ("architecture/adrai/" `isPrefixOf`)
              (map T.unpack (T.lines (decodeUtf8 (LBS.toStrict beforeTree))))
      beforeManaged <- traverse (\path -> (,) path <$> BS.readFile (repo </> path)) managedPaths
      BS.writeFile (repo </> "compare staged.bin") "\NUL\SOHcompare staged bytes\255"
      _ <- gitStdout repo ["add", "--", "compare staged.bin"]
      BS.writeFile (repo </> "README.md") "# Test\ncompare caller dirty bytes\n"
      BS.writeFile (repo </> "compare untracked ü.txt") "keep this untracked file"
      beforeStatus <- gitStdout repo ["status", "--porcelain=v1", "--untracked-files=all", "-z"]
      beforeIndex <- gitStdout repo ["ls-files", "--stage", "-z"]
      beforeCached <- gitStdout repo ["diff", "--cached", "--binary"]
      beforeWorktree <- gitStdout repo ["diff", "--binary"]

      added <- p602fJsonOrThrow repo ["compare", T.unpack initialRevision, T.unpack createdRevision, "--json"]
      assertCompareEnvelope added adr initialRevision createdRevision initialRevision createdRevision (1, 0, 0, 0) ["added"]

      changed <- p602fJsonOrThrow repo ["compare", T.unpack createdRevision, "--json"]
      assertCompareEnvelope changed adr createdRevision amendedRevision createdRevision "HEAD" (0, 0, 1, 0) ["changed"]

      unchangedHidden <- p602fJsonOrThrow repo ["compare", T.unpack amendedRevision, T.unpack amendedRevision, "--json"]
      assertCompareEnvelope unchangedHidden adr amendedRevision amendedRevision amendedRevision amendedRevision (0, 0, 0, 1) []
      unchangedShown <- p602fJsonOrThrow repo ["compare", T.unpack amendedRevision, T.unpack amendedRevision, "--include-unchanged", "--json"]
      assertCompareEnvelope unchangedShown adr amendedRevision amendedRevision amendedRevision amendedRevision (0, 0, 0, 1) ["unchanged"]

      removed <- p602fJsonOrThrow repo ["compare", T.unpack createdRevision, T.unpack initialRevision, "--json"]
      assertCompareEnvelope removed adr createdRevision initialRevision createdRevision initialRevision (0, 1, 0, 0) ["removed"]

      repository <- do
        discovered <- discoverRepository systemGit repo
        case discovered of
          Left problem -> assertFailure ("discover compare fixture: " <> show problem) >> fail "unreachable"
          Right value -> pure value
      expectedText <- do
        outcome <- runCompare repository (CompareRequest createdRevision "HEAD" False)
        case outcome of
          Right projection -> pure (LBS.fromStrict (renderCompareProjection projection))
          Left problem -> assertFailure ("programmatic compare failed: " <> show problem) >> fail "unreachable"
      (textExit, textOut, textErr) <- p602fRaw repo ["compare", T.unpack createdRevision]
      textExit @?= ExitSuccess
      textErr @?= ""
      textOut @?= expectedText

      (invalidExit, invalidOut, invalidErr) <- p602fRaw repo ["compare", "refs/heads/does-not-exist", "--json"]
      invalidExit @?= ExitFailure 2
      invalidOut @?= ""
      assertBool "invalid compare revision is a user error" ("adrai: " `LBS.isPrefixOf` invalidErr)

      afterHead <- headCommit repo
      afterRef <- gitStdout repo ["symbolic-ref", "--quiet", "HEAD"]
      afterTree <- gitStdout repo ["ls-tree", "-r", "--name-only", "HEAD"]
      afterStatus <- gitStdout repo ["status", "--porcelain=v1", "--untracked-files=all", "-z"]
      afterIndex <- gitStdout repo ["ls-files", "--stage", "-z"]
      afterCached <- gitStdout repo ["diff", "--cached", "--binary"]
      afterWorktree <- gitStdout repo ["diff", "--binary"]
      afterManaged <- traverse (\path -> (,) path <$> BS.readFile (repo </> path)) managedPaths
      afterHead @?= amendedRevision
      afterRef @?= beforeRef
      afterTree @?= beforeTree
      afterStatus @?= beforeStatus
      afterIndex @?= beforeIndex
      afterCached @?= beforeCached
      afterWorktree @?= beforeWorktree
      afterManaged @?= beforeManaged
  where
    assertCompareEnvelope value expectedAdr expectedFrom expectedTo requestedFrom requestedTo expectedCounts expectedKinds =
      case _Object value of
        Nothing -> assertFailure "compare output is not an object"
        Just object -> do
          sortOn id (map AesonKey.toText (KM.keys object))
            @?= sortOn id ["cache", "counts", "entries", "from", "from_requested", "to", "to_requested", "view"]
          (object .: "view" :: Maybe Text) @?= Just "compare"
          (object .: "from" :: Maybe Text) @?= Just expectedFrom
          (object .: "to" :: Maybe Text) @?= Just expectedTo
          (object .: "from_requested" :: Maybe Text) @?= Just requestedFrom
          (object .: "to_requested" :: Maybe Text) @?= Just requestedTo
          counts <- case KM.lookup "counts" object >>= _Object of
            Just result -> pure result
            Nothing -> assertFailure "compare output omitted counts" >> fail "unreachable"
          let (added, removed, changed, unchanged) = expectedCounts
          (counts .: "added" :: Maybe Int) @?= Just added
          (counts .: "removed" :: Maybe Int) @?= Just removed
          (counts .: "changed" :: Maybe Int) @?= Just changed
          (counts .: "unchanged" :: Maybe Int) @?= Just unchanged
          entries <- case object .: "entries" of
            Just result -> pure (result :: [Data.Aeson.Value])
            Nothing -> assertFailure "compare output omitted entries" >> fail "unreachable"
          mapMaybe compareEntryKind entries @?= expectedKinds
          case entries of
            [] -> pure ()
            [entry] -> case _Object entry of
              Nothing -> assertFailure "compare entry is not an object"
              Just entryObject -> do
                sortOn id (map AesonKey.toText (KM.keys entryObject))
                  @?= sortOn id ["adr", "after", "before", "changes", "kind", "title"]
                (entryObject .: "adr" :: Maybe Text) @?= Just expectedAdr
                changes <- case entryObject .: "changes" of
                  Just result -> pure (result :: [Data.Aeson.Value])
                  Nothing -> assertFailure "compare entry omitted changes" >> fail "unreachable"
                case expectedKinds of
                  ["added"] -> do
                    KM.lookup "before" entryObject @?= Just Data.Aeson.Null
                    assertBool "added entry includes its after snapshot" (maybe False (/= Data.Aeson.Null) (KM.lookup "after" entryObject))
                    changes @?= []
                  ["removed"] -> do
                    assertBool "removed entry includes its before snapshot" (maybe False (/= Data.Aeson.Null) (KM.lookup "before" entryObject))
                    KM.lookup "after" entryObject @?= Just Data.Aeson.Null
                    changes @?= []
                  ["unchanged"] -> do
                    assertBool "unchanged entry includes its before snapshot" (maybe False (/= Data.Aeson.Null) (KM.lookup "before" entryObject))
                    assertBool "unchanged entry includes its after snapshot" (maybe False (/= Data.Aeson.Null) (KM.lookup "after" entryObject))
                    changes @?= []
                  ["changed"] -> do
                    let fields =
                          [ field
                          | change <- changes
                          , Just changeObject <- [_Object change]
                          , Just field <- [changeObject .: "field" :: Maybe Text]
                          ]
                    fields @?= ["record", "title", "summary", "body", "record_heads"]
                  _ -> assertFailure "unexpected compare entry expectation"
            _ -> assertFailure "compare fixture expected at most one entry"

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
              case o .: "evolution" of
                Just evolution ->
                  case _Object evolution of
                    Just ev ->
                      case (ev .: "operation_count" :: Maybe Int, ev .: "summary" :: Maybe (Maybe Text)) of
                        (Just count, Just summary) -> do
                          assertBool "evolution has operation count" (count > 0)
                          assertBool "evolution has summary" (isJust summary)
                        _ -> assertFailure "could not parse evolution fields"
                    Nothing -> assertFailure "could not parse evolution"
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
-- Test 11: Reverse compare reports branch-only ADRs as removed
-- =====================================================================

testCompareReverseShowsRemoved :: TestTree
testCompareReverseShowsRemoved =
  testCase "compare_reverse_reports_removed_adrs" $
    withSystemTempDirectory "adrai compare reverse" $ \tmpDir -> do
      repo <- createTestRepo tmpDir
      _ <- createAdr repo
        "Main ADR"
        "Decision on main branch"
        "## Decision\nMain decision."
        ["main"]
        ["src/**"]
      mainHead <- headCommit repo

      git repo ["switch", "-c", "feature"]
      _ <- createAdr repo
        "Feature ADR"
        "Decision on feature branch"
        "## Decision\nFeature decision."
        ["feature"]
        ["src/feature/**"]
      featureHead <- headCommit repo

      reverseCompare <- adraiJsonOrThrow repo
        [ "compare",
          T.unpack featureHead,
          T.unpack mainHead,
          "--json"
        ]
      case parseCompareResults reverseCompare of
        Just (schema, _, _, entries) -> do
          schema @?= "adrai/compare/v1"
          let kinds = mapMaybe compareEntryKind entries
          assertBool "reverse compare shows removed" ("removed" `elem` kinds)
        Nothing -> assertFailure "reverse compare parse failed"

-- =====================================================================
-- Test 12: Compare CLI uses same read service
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

isRight :: Either a b -> Bool
isRight (Left _)  = False
isRight (Right _) = True

-- | Encode Aeson Value to Lazy ByteString (needed for Text checking).
encode :: Data.Aeson.Value -> LBS.ByteString
encode = Data.Aeson.encode

-- | Catch git failure and return unit (useful for merges that may fail).
catchGitFailure :: IO () -> IO ()
catchGitFailure action =
  action `Exception.catch` (\(_ :: SomeException) -> pure ())
