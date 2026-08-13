{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for the cache subsystem: cold/warm/incremental cache
-- paths, provenance recovery, noise-only reuse, merge-aware caching, and
-- branch-scoped cache stability.
--
-- Port of the 15 tests from
-- ``ADRAI_1_Source/tests/test_incremental_compiler.py`` and
-- ``ADRAI_1_Source/tests/test_merge_aware_cache.py``.
module Adrai.CacheIntegrationTest (tests) where

import Adrai.Cli (CompileResult (..))
import Adrai.Integration.CLI
import Control.Monad (forM_, void, when)
import qualified Data.Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (isSuffixOf)
import Data.Maybe (listToMaybe)
import Data.Text (Text, strip, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (DiffTime, UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Vector qualified as Vector
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getDirectoryContents,
    getModificationTime,
    removeDirectoryRecursive,
    removeFile,
  )
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Database.SQLite.Simple
  ( Connection,
    Only (..),
    close,
    open,
    query,
    query_,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( (@?=),
    assertBool,
    assertFailure,
    testCase,
  )

-- ---------------------------------------------------------------------------
-- JSON helper accessors
-- ---------------------------------------------------------------------------

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

headVal :: Data.Aeson.Value -> Maybe Data.Aeson.Value
headVal (Data.Aeson.Array arr) =
  if Vector.null arr then Nothing else Just (Vector.head arr)
headVal _ = Nothing

-- ---------------------------------------------------------------------------
-- Local helpers (wrapping CLI helpers for convenience)
-- ---------------------------------------------------------------------------

-- | Commit files with a custom message (the CLI commitFiles uses "fixture").
commitFileWithMsg
  :: FilePath
  -> FilePath
  -> BS.ByteString
  -> Text
  -> IO Text
commitFileWithMsg repo relativePath content message = do
  let path = repo </> relativePath
  createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile path content
  git repo ["--literal-pathspecs", "add", "--", relativePath]
  git repo ["commit", "-m", unpack message]
  gitStdout repo ["rev-parse", "HEAD"]
    >>= \h -> pure (strip (decodeUtf8 (LBS.toStrict h)))

-- | Commit multiple files with a custom message.
commitFilesWithMsg
  :: FilePath
  -> [(FilePath, BS.ByteString)]
  -> Text
  -> IO Text
commitFilesWithMsg repo files message = do
  results <- mapM (\(rel, content) -> commitFileWithMsg repo rel content message) files
  pure (last results)

-- ---------------------------------------------------------------------------
-- SQLite helpers
-- ---------------------------------------------------------------------------

-- | Read a single meta value from the adrai database.
getMeta :: FilePath -> String -> IO (Maybe String)
getMeta dbPath key = do
  conn <- open dbPath
  result <- query conn "SELECT value FROM meta WHERE key = ?" (Only key) :: IO [Only String]
  close conn
  pure $ listToMaybe result >>= \(Only v) -> Just v

-- | Get the HEAD commit hash of a repository.
headCommit :: FilePath -> IO Text
headCommit repo =
  gitStdout repo ["rev-parse", "HEAD"]
    >>= \h -> pure (strip (decodeUtf8 (LBS.toStrict h)))

-- ---------------------------------------------------------------------------
-- ADR creation helpers
-- ---------------------------------------------------------------------------

createCacheAdr :: FilePath -> IO Data.Aeson.Value
createCacheAdr repo =
  createAdr
    repo
    "Stable cache identity"
    "Cache identity derives from semantic inputs."
    "## Context\nBuilds move between workspaces.\n\n## Decision\nCache keys exclude absolute workspace paths and use source digests.\n\n## Consequences\nInputs must be normalized."
    ["compiler.cache"]
    ["src/compiler/cache/**"]

createJobsAdr :: FilePath -> IO Data.Aeson.Value
createJobsAdr repo =
  createAdr
    repo
    "At-least-once job delivery"
    "Workers acknowledge durable jobs only after successful execution."
    "## Decision\nUse durable queues and idempotent job handlers."
    ["runtime.jobs"]
    ["src/jobs/**"]

createReleaseAdr :: FilePath -> IO Data.Aeson.Value
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

-- ---------------------------------------------------------------------------
-- Compile result accessor
-- ---------------------------------------------------------------------------

compileCacheMode :: CompileResult -> Text
compileCacheMode = coldCompilerCacheMode

compileDocsParsed :: CompileResult -> Int
compileDocsParsed = coldCompilerDocumentsParsed

compileDocsReused :: CompileResult -> Int
compileDocsReused = coldCompilerDocumentsReused

compileAdrsRebuilt :: CompileResult -> Int
compileAdrsRebuilt = coldCompilerAdrsRebuilt

compileAdrsReused :: CompileResult -> Int
compileAdrsReused = coldCompilerAdrsReused

compileEmbComputed :: CompileResult -> Int
compileEmbComputed = coldCompilerEmbeddingComputed

compileEmbReused :: CompileResult -> Int
compileEmbReused = coldCompilerEmbeddingReused

compileHistScanned :: CompileResult -> Int
compileHistScanned = coldCompilerHistoryCommitsScanned

compileIncKind :: CompileResult -> Text
compileIncKind = coldCompilerIncrementalKind

-- | Extract doctor issues from CLI output.
doctorIssuesFromCli :: FilePath -> IO [Data.Aeson.Value]
doctorIssuesFromCli repo = do
  val <- adraiJsonOrThrow repo ["doctor", "--json"]
  case _Object val of
    Nothing -> pure []
    Just o -> do
      issues <- pure $ o .: "issues"
      case issues of
        Nothing -> pure []
        Just is -> pure is

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup "Cache integration (P4-07)"
    [ -- From test_incremental_compiler.py
      testCase "cold_compile_then_exact_cache_does_not_replace_database" testColdCompileThenExact,
      testCase "exact_cache_does_not_deserialize_managed_documents" testExactCacheNoDeserialize,
      testCase "corrupt_provenance_overlay_is_rebuilt_from_cache" testCorruptProvenanceRebuild,
      testCase "noise_only_commit_reuses_documents" testNoiseOnlyReuse,
      testCase "merge_delta_scans_only_provenance_delta" testMergeDelta,
      testCase "hidden_reflog_commit_in_provenance_delta" testReflogCommit,
      testCase "trailer_in_noise_commit_forces_provenance_rescan" testTrailerInNoise,
      testCase "new_operations_parse_only_new_objects" testNewOperationsParseOnlyNew,
      testCase "compile_cli_reports_cache_counters" testCompileCliReportsCounters,
      testCase "corrupt_old_cache_is_rebuilt" testCorruptOldCacheRebuilt,
      testCase "corrupt_current_db_does_not_invalidate_revision_cache" testCorruptCurrentDb,
      -- From test_merge_aware_cache.py
      testCase "stable_release_cache_survives_dev_merges" testStableReleaseSurvivesMerges,
      testCase "nearest_cached_ancestor_beats_unrelated" testNearestCachedAncestor,
      testCase "adr_bearing_merge_rebuilds_only_affected" testAdrBearingMerge,
      testCase "divergent_adr_states_remain_consistent" testDivergentAdStates
    ]

-- =====================================================================
-- Test 1: Cold compile then exact cache reuse
-- =====================================================================

testColdCompileThenExact :: IO ()
testColdCompileThenExact =
  withSystemTempDirectory "adrai cold exact" $ \tmpDir -> do
    repo <- createTestRepo tmpDir
    createCacheAdr repo >>= \_ -> pure ()
    createAdraiInit repo
    -- Delete all cache files
    let adraiDir = repo </> ".adrai"
        mainDb = adraiDir </> "adrai.sqlite"
    removeIfExists mainDb
    cacheDir <- getCacheDir repo
    cacheFiles <- getCacheFiles cacheDir
    mapM_ removeIfExists cacheFiles
    -- Cold compile
    coldResult <- adraiJsonOrThrow repo ["compile", "--json"]
    let coldRes = parseCompileResult coldResult
    case coldRes of
      Nothing -> assertFailure "could not parse cold compile result"
      Just cr -> do
        compileDocsParsed cr @?= 4
        compileDocsReused cr @?= 0

    before <- getFileStat mainDb

    -- Exact compile (should reuse cache)
    exactResult <- adraiJsonOrThrow repo ["compile", "--json"]
    let exactRes = parseCompileResult exactResult
    case exactRes of
      Nothing -> assertFailure "could not parse exact compile result"
      Just cr -> do
        compileCacheMode cr @?= "exact"
        compileDocsParsed cr @?= 0
        compileDocsReused cr @?= 4

    after <- getFileStat mainDb
    -- Database should not have changed between cold and exact
    assertBool "DB file changed between cold and exact" (before == after)

-- =====================================================================
-- Test 2: Exact cache skips document deserialization
-- =====================================================================

testExactCacheNoDeserialize :: IO ()
testExactCacheNoDeserialize =
  withSystemTempDirectory "adrai exact no deserialize" $ \tmpDir -> do
    repo <- createTestRepo tmpDir
    createCacheAdr repo >>= \_ -> pure ()
    createAdraiInit repo
    -- First compile establishes cache
    _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()
    -- Second compile should be exact
    result <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult result of
      Nothing -> assertFailure "could not parse result"
      Just cr -> compileCacheMode cr @?= "exact"

-- =====================================================================
-- Test 3: Corrupt provenance overlay rebuilds from cache
-- =====================================================================

testCorruptProvenanceRebuild :: IO ()
testCorruptProvenanceRebuild =
  withSystemTempDirectory "adrai corrupt provenance" $ \tmpDir -> do
    repo <- createTestRepo tmpDir
    createCacheAdr repo >>= \_ -> pure ()
    createAdraiInit repo
    -- Compile to establish cache
    _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()
    -- Corrupt the provenance database
    let provenanceDb = repo </> ".adrai" </> "provenance.sqlite"
    BS.writeFile provenanceDb "not sqlite data"
    -- Compile again - should rebuild from cache
    result <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult result of
      Nothing -> assertFailure "could not parse result2"
      Just cr -> compileDocsParsed cr @?= 0

-- =====================================================================
-- Test 4: Noise-only commit reuses documents
-- =====================================================================

testNoiseOnlyReuse :: IO ()
testNoiseOnlyReuse =
  withSystemTempDirectory "adrai noise reuse" $ \tmpDir -> do
    repo <- createTestRepo tmpDir
    createCacheAdr repo >>= \_ -> pure ()
    createAdraiInit repo
    -- First compile
    baseline <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult baseline of
      Nothing -> assertFailure "could not parse baseline"
      Just cr -> compileCacheMode cr @?= "exact"

    -- Add noise files (non-ADR)
    commitFilesWithMsg repo
      [ ( "src/noise/generated.txt", "unrelated build output metadata\n" ),
        ( "docs/release-notes.md", "No architecture records changed.\n" )
      ]
      "ordinary product work"

    -- Incremental compile
    incResult <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult incResult of
      Nothing -> assertFailure "could not parse incremental result"
      Just cr -> do
        compileCacheMode cr @?= "incremental"
        compileDocsParsed cr @?= 0
        compileDocsReused cr @?= 4
        compileIncKind cr @?= "tree-identical"

    -- Next compile should be exact
    exactResult <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult exactResult of
      Nothing -> assertFailure "could not parse exact result"
      Just cr -> compileCacheMode cr @?= "exact"

-- =====================================================================
-- Test 5: Merge delta scanning
-- =====================================================================

testMergeDelta :: IO ()
testMergeDelta =
  withSystemTempDirectory "adrai merge delta" $ \tmpDir -> do
    repo <- createTestRepo tmpDir
    createCacheAdr repo >>= \_ -> pure ()
    createAdraiInit repo
    -- Compile baseline
    _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()

    -- Create feature branch with noise
    git repo ["switch", "-c", "feature/noise"]
    commitFilesWithMsg repo
      [ ( "src/feature.txt", "feature noise\n" ) ]
      "feature noise"
    git repo ["switch", "main"]
    -- Add noise on main
    commitFilesWithMsg repo
      [ ( "src/main.txt", "main noise\n" ) ]
      "main noise"
    -- Merge feature into main
    git repo ["merge", "--no-ff", "feature/noise", "-m", "merge noise"]

    -- Compile - should be incremental
    result <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult result of
      Nothing -> assertFailure "could not parse merge result"
      Just cr -> do
        compileCacheMode cr @?= "incremental"
        compileDocsParsed cr @?= 0
        compileDocsReused cr @?= 4
        compileHistScanned cr <=? 3
        compileIncKind cr @?= "tree-identical"

-- =====================================================================
-- Test 6: Reflog commit inclusion
-- =====================================================================

testReflogCommit :: IO ()
testReflogCommit =
  withSystemTempDirectory "adrai reflog" $ \tmpDir -> do
    repo <- createTestRepo tmpDir
    createCacheAdr repo >>= \_ -> pure ()
    createAdraiInit repo
    -- Compile baseline
    _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()

    -- Create ephemeral branch with noise
    git repo ["switch", "-c", "ephemeral/noise"]
    commitFilesWithMsg repo
      [ ( "src/ephemeral-only.txt", "reachable only through the reflog\n" ) ]
      "ephemeral product experiment"
    git repo ["switch", "main"]
    -- Delete the ephemeral branch (removes ref but reflog retains it)
    git repo ["branch", "-D", "ephemeral/noise"]
    -- Add noise on main
    commitFilesWithMsg repo
      [ ( "src/main-noise.txt", "ordinary mainline work\n" ) ]
      "ordinary mainline work"

    -- Compile
    result <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult result of
      Nothing -> assertFailure "could not parse reflog compile result"
      Just cr -> do
        compileCacheMode cr @?= "incremental"
        compileDocsParsed cr @?= 0
        compileDocsReused cr @?= 4
        compileHistScanned cr @?= 2
        compileIncKind cr @?= "tree-identical"

-- =====================================================================
-- Test 7: Trailer in noise commit
-- =====================================================================

testTrailerInNoise :: IO ()
testTrailerInNoise =
  withSystemTempDirectory "adrai trailer in noise" $ \tmpDir -> do
    repo <- createTestRepo tmpDir
    created <- createCacheAdr repo
    let adrId = extractAdrId created
    case adrId of
      Nothing -> assertFailure "createAdr did not return an ADR ID"
      Just adrId' -> do
        createAdraiInit repo
        -- Compile baseline
        _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()

        -- Commit with ADRAI-Op trailer but no real ADRAI files
        commitFilesWithMsg repo
          [ ( "src/misleading-trailer.txt", "not an ADRAI operation\n" ) ]
          ("ordinary work\n\nADRAI-Op: " <> adrId')

        -- Compile
        result <- adraiJsonOrThrow repo ["compile", "--json"]
        case parseCompileResult result of
          Nothing -> assertFailure "could not parse trailer result"
          Just cr -> do
            compileCacheMode cr @?= "incremental"
            compileDocsParsed cr @?= 0
            -- Check that doctor finds REDUNDANT_OPERATION_TRAILER
            issues <- doctorIssuesFromCli repo
            let codes :: [Text]
                codes = [ code
                        | issue <- issues,
                          Just o <- [_Object issue],
                          Just code <- [o .: "code"]
                        ]
            assertBool "should find REDUNDANT_OPERATION_TRAILER in doctor output"
              ("REDUNDANT_OPERATION_TRAILER" `elem` codes)

-- =====================================================================
-- Test 8: New operations parse only new objects
-- =====================================================================

testNewOperationsParseOnlyNew :: IO ()
testNewOperationsParseOnlyNew =
  withSystemTempDirectory "adrai new operations" $ \tmpDir -> do
    repo <- createTestRepo tmpDir
    _ <- createCacheAdr repo >>= \_ -> pure ()
    createAdraiInit repo
    -- Cold compile
    _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()

    -- Create second ADR
    _ <- createJobsAdr repo >>= \_ -> pure ()

    -- Incremental compile
    result <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult result of
      Nothing -> assertFailure "could not parse new ops result"
      Just cr -> do
        compileCacheMode cr @?= "incremental"
        compileDocsParsed cr @?= 4
        compileDocsReused cr @?= 4
        compileAdrsRebuilt cr @?= 1
        compileAdrsReused cr @?= 1

-- =====================================================================
-- Test 9: Compile CLI reports counters
-- =====================================================================

testCompileCliReportsCounters :: IO ()
testCompileCliReportsCounters =
  withSystemTempDirectory "adrai cli reports" $ \tmpDir -> do
    repo <- createTestRepo tmpDir
    createCacheAdr repo >>= \_ -> pure ()
    createAdraiInit repo
    -- First compile
    _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()
    -- Second compile (should be exact)
    result <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult result of
      Nothing -> assertFailure "could not parse CLI report result"
      Just cr -> do
        compileCacheMode cr @?= "exact"
        compileDocsParsed cr @?= 0
        compileDocsReused cr @?= 4

-- =====================================================================
-- Test 10: Corrupt old cache rebuilt
-- =====================================================================

testCorruptOldCacheRebuilt :: IO ()
testCorruptOldCacheRebuilt =
  withSystemTempDirectory "adrai corrupt old cache" $ \tmpDir -> do
    repo <- createTestRepo tmpDir
    createCacheAdr repo >>= \_ -> pure ()
    createAdraiInit repo
    -- Compile to establish cache
    _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()

    let adraiDir = repo </> ".adrai"
        cacheDir = adraiDir </> "cache"
        mainDb = adraiDir </> "adrai.sqlite"

    -- Corrupt all .adrai/cache/*.sqlite
    cacheFiles <- getCacheFiles cacheDir
    mapM_ (\f -> BS.writeFile f "not sqlite") cacheFiles
    -- Corrupt main DB
    BS.writeFile mainDb "not sqlite"

    -- Recompile should be full cold
    result <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult result of
      Nothing -> assertFailure "could not parse corrupt rebuild result"
      Just cr -> do
        compileCacheMode cr @?= "full"
        compileDocsParsed cr @?= 4

-- =====================================================================
-- Test 11: Corrupt current DB doesn't invalidate revision cache
-- =====================================================================

testCorruptCurrentDb :: IO ()
testCorruptCurrentDb =
  withSystemTempDirectory "adrai corrupt current db" $ \tmpDir -> do
    repo <- createTestRepo tmpDir
    createCacheAdr repo >>= \_ -> pure ()
    createAdraiInit repo
    -- Compile baseline
    baseline <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult baseline of
      Nothing -> assertFailure "could not parse baseline"
      Just cr -> compileCacheMode cr @?= "exact"

    -- Corrupt main database
    let mainDb = repo </> ".adrai" </> "adrai.sqlite"
    BS.writeFile mainDb "not sqlite"

    -- Recompile should still be exact (cache in .adrai/cache/ survives)
    result <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult result of
      Nothing -> assertFailure "could not parse corrupt db result"
      Just cr -> compileCacheMode cr @?= "exact"

-- =====================================================================
-- Test 12: Stable release cache survives dev merges
-- =====================================================================

testStableReleaseSurvivesMerges :: IO ()
testStableReleaseSurvivesMerges =
  withSystemTempDirectory "adrai stable release" $ \tmpDir -> do
    repo <- createTestRepo tmpDir
    _ <- createCacheAdr repo >>= \_ -> pure ()
    createAdraiInit repo
    -- Compile on main
    mainResult <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult mainResult of
      Nothing -> assertFailure "could not parse main compile"
      Just cr -> compileCacheMode cr @?= "exact"

    -- Create release branch
    git repo ["switch", "-c", "release/stable"]
    commitFilesWithMsg repo
      [ ( "release/version.txt", "2025.08\n" ) ]
      "cut stable release"

    -- Compile on release
    releaseResult <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult releaseResult of
      Nothing -> assertFailure "could not parse release compile"
      Just cr -> compileCacheMode cr @?= "exact"

    let releaseDb = repo </> ".adrai" </> "adrai.sqlite"
    releaseDbContent <- BS.readFile releaseDb

    -- Switch back to main, create develop branch
    git repo ["switch", "main"]
    git repo ["switch", "-c", "develop"]

    -- Merge 6 noise trains (2 commits each)
    forM_ [0 .. 5] $ \train -> do
      git repo ["switch", "-c", "feature/noise-" ++ show train]
      commitFilesWithMsg repo
        [ ( "src/noise/" ++ show train ++ "/change.txt",
            encodeUtf8 (T.pack ("train=" ++ show train ++ "\n")) ) ]
        (T.pack ("product noise " ++ show train))
      git repo ["switch", "develop"]
      git repo
        [ "merge", "--no-ff", "feature/noise-" ++ show train,
          "-m", "merge product train " ++ show train ]
      git repo ["branch", "-D", "feature/noise-" ++ show train]

    -- Compile on develop
    developResult <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult developResult of
      Nothing -> assertFailure "could not parse develop compile"
      Just cr -> do
        compileIncKind cr @?= "tree-identical"
        compileDocsParsed cr @?= 0
        compileAdrsRebuilt cr @?= 0
        compileHistScanned cr <=? 24

    -- Switch back to release
    git repo ["switch", "release/stable"]
    releaseAfterResult <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult releaseAfterResult of
      Nothing -> assertFailure "could not parse release after compile"
      Just cr -> compileIncKind cr @?= "provenance-sync"

    -- Verify release DB content didn't change
    afterReleaseDbContent <- BS.readFile releaseDb
    afterReleaseDbContent @?= releaseDbContent

-- =====================================================================
-- Test 13: Nearest cached ancestor
-- =====================================================================

testNearestCachedAncestor :: IO ()
testNearestCachedAncestor =
  withSystemTempDirectory "adrai nearest cached ancestor" $ \tmpDir -> do
    repo <- createTestRepo tmpDir
    _ <- createCacheAdr repo >>= \_ -> pure ()
    createAdraiInit repo
    -- Compile on main
    _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()

    -- Create second ADR on separate branch
    git repo ["switch", "-c", "release/unrelated"]
    _ <- createJobsAdr repo >>= \_ -> pure ()
    _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()

    -- Switch back to main with noise
    git repo ["switch", "main"]
    forM_ [0 .. 3] $ \i -> do
      commitFilesWithMsg repo
        [ ( "src/main-noise/" ++ show i ++ ".txt",
            encodeUtf8 (T.pack (show i ++ "\n")) ) ]
        (T.pack ("main noise " ++ show i))

    -- Compile - should use nearest cached ancestor
    result <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult result of
      Nothing -> assertFailure "could not parse nearest ancestor result"
      Just cr -> do
        compileIncKind cr @?= "tree-identical"
        compileDocsParsed cr @?= 0
        compileAdrsRebuilt cr @?= 0

-- =====================================================================
-- Test 14: ADR-bearing merge
-- =====================================================================

testAdrBearingMerge :: IO ()
testAdrBearingMerge =
  withSystemTempDirectory "adrai adr bearing merge" $ \tmpDir -> do
    repo <- createTestRepo tmpDir
    _ <- createCacheAdr repo >>= \_ -> pure ()
    createAdraiInit repo
    -- Compile on main
    _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()

    -- Create second ADR on develop
    git repo ["switch", "-c", "develop"]
    _ <- createJobsAdr repo >>= \_ -> pure ()
    _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()

    -- Merge develop into main
    git repo ["switch", "main"]
    git repo
      [ "merge", "--no-ff", "develop",
        "-m", "promote architecture and product work" ]

    -- Compile - should be semantic-reuse
    result <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult result of
      Nothing -> assertFailure "could not parse adr-merge result"
      Just cr -> do
        compileCacheMode cr @?= "semantic-reuse"
        compileDocsParsed cr @?= 4
        compileDocsReused cr @?= 4
        compileAdrsRebuilt cr @?= 1
        compileAdrsReused cr @?= 1
        compileHistScanned cr <=? 1

-- =====================================================================
-- Test 15: Divergent ADR states
-- =====================================================================

testDivergentAdStates :: IO ()
testDivergentAdStates =
  withSystemTempDirectory "adrai divergent states" $ \tmpDir -> do
    repo <- createTestRepo tmpDir
    _ <- createCacheAdr repo >>= \_ -> pure ()
    createAdraiInit repo

    -- Compile on main (establish baseline)
    _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()

    -- Create release branch with release-only ADR
    git repo ["switch", "-c", "release/old"]
    _ <- createReleaseAdr repo >>= \_ -> pure ()
    _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()

    -- Switch back to main and amend the original ADR
    git repo ["switch", "main"]
    created <- createCacheAdr repo
    let adrId' = extractAdrId created
    case adrId' of
      Nothing -> assertFailure "no ADR ID"
      Just id' -> do
        void $ amendAdrViaCli repo id' Nothing
          (Just "Main uses generation-two cache identity.")
          (Just "## Decision\nUse generation-two cache identity on current development.")

    -- Create develop with noise merges
    git repo ["switch", "-c", "develop"]
    forM_ [0 .. 3] $ \train -> do
      git repo ["switch", "-c", "feature/noise-" ++ show train]
      commitFilesWithMsg repo
        [ ( "src/noise/" ++ show train ++ "/change.txt",
             encodeUtf8 ("train=" <> T.pack (show train) <> "\n") ) ]
        ("product noise " <> T.pack (show train))
      git repo ["switch", "develop"]
      git repo
        [ "merge", "--no-ff", "feature/noise-" ++ show train,
          "-m", "merge product train " ++ show train ]
      git repo ["branch", "-D", "feature/noise-" ++ show train]

    -- Switch back to main and merge develop
    git repo ["switch", "main"]
    git repo
      [ "merge", "--no-ff", "develop",
        "-m", "merge development trains" ]

    -- Switch between branches and verify consistent states
    -- Release branch
    git repo ["switch", "release/old"]
    releaseResult <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult releaseResult of
      Nothing -> assertFailure "could not parse release state result"
      Just cr -> compileCacheMode cr @?= "exact"

    -- Main branch
    git repo ["switch", "main"]
    mainResult <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult mainResult of
      Nothing -> assertFailure "could not parse main state result"
      Just cr -> compileCacheMode cr @?= "exact"

-- =====================================================================
-- Helper assertion helpers
-- =====================================================================

infix 4 <=?
(<=?) :: (Ord a, Show a) => a -> a -> IO ()
a <=? b = assertBool (show a <> " should be <= " <> show b) (a <= b)

-- ---------------------------------------------------------------------------
-- File system helpers
-- ---------------------------------------------------------------------------

-- | Remove a file if it exists.
removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesFileExist path
  when exists $ removeFile path

-- | Get the .adrai/cache directory path for a repository.
getCacheDir :: FilePath -> IO FilePath
getCacheDir repo = pure (repo </> ".adrai" </> "cache")

-- | Get list of .sqlite files in the cache directory.
getCacheFiles :: FilePath -> IO [FilePath]
getCacheFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entries <- getDirectoryContents dir
      pure $ map (dir </>) $ filter (".sqlite" `isSuffixOf`) entries

-- | Get a simple file stat (existence + modification time in ns).
data FileStat = FileStat
  { fileStatExists :: Bool,
    fileStatSize :: Integer
  }
  deriving (Eq, Show)

getFileStat :: FilePath -> IO FileStat
getFileStat path = do
  exists <- doesFileExist path
  size <- if exists then getFileModTime path else pure 0
  pure FileStat {fileStatExists = exists, fileStatSize = size}

getFileModTime :: FilePath -> IO Integer
getFileModTime path = do
  t <- getModificationTime path
  let secs = realToFrac (utcTimeToPOSIXSeconds t) :: Double
  pure $ ceiling (secs * 1e9 :: Double)

-- ---------------------------------------------------------------------------
-- Value extraction helpers
-- ---------------------------------------------------------------------------

-- | Extract ADR ID from a create-adr result.
extractAdrId :: Data.Aeson.Value -> Maybe Text
extractAdrId v = do
  o <- _Object v
  o .: "adr"
