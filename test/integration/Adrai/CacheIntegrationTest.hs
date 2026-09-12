{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for the cache subsystem: cold/warm/incremental cache
-- paths, provenance recovery, noise-only reuse, merge-aware caching, and
-- branch-scoped cache stability.
--
-- Retained coverage from
-- ``ADRAI_1_Source/tests/test_incremental_compiler.py`` and
-- ``ADRAI_1_Source/tests/test_merge_aware_cache.py``.
module Adrai.CacheIntegrationTest (tests) where

import Adrai.Cli (CompileResult (..))
import Adrai.Compiler.CacheSelection
  ( validateCacheContract,
    validateExactCacheTarget,
    validateCachePublicationContract,
    refreshCacheMaterializationFingerprint,
  )
import Adrai.Compiler.CacheSelection.TestSupport
  ( PostCommitCloneCancellation (..),
    PostCommitCloneObservation (..),
    PostCommitRefreshPlan (..),
    observeValidatedPostCommitCloneForTest,
  )
import Adrai.Git (GitOid (..))
import Adrai.Integration.CLI
import Adrai.Provenance (mkGitOid)
import Adrai.RetainedCache.CacheFixture
  ( additionalSimpleCompilerFiles,
    healthyCompilerFiles,
    healthySimpleCompilerFiles,
  )
import Adrai.RetainedCache.RepositorySeed
  ( RepositorySeed,
    createRepositorySeed,
    removeRepositorySeed,
    withPrivateRepositorySeed,
  )
import qualified Control.Concurrent.Async as Async
import Control.Monad (forM, forM_, void)
import qualified Data.Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (isSuffixOf)
import Data.Text (Text, strip, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Database.SQLite.Simple (Only (..), Query, close, execute_, open, query_)
import System.Directory
  ( createDirectoryIfMissing,
    copyFile,
    doesDirectoryExist,
    doesFileExist,
    getDirectoryContents,
    removeFile,
  )
import System.FilePath (takeDirectory, (</>))
import Test.Tasty (TestTree, testGroup, withResource)
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
  withResource (createRepositorySeed prepareSimpleIntegrationSeed) removeRepositorySeed $ \getSimpleSeed ->
    testGroup "Cache integration (P4-07)"
      [ -- From test_incremental_compiler.py
        testCase "corrupt_provenance_overlay_is_rebuilt_from_cache" $ getSimpleSeed >>= testCorruptProvenanceRebuild,
        testCase "v3 tree-identical seed reproduces parsed registration signatures from operation_member" $ getSimpleSeed >>= testV3TreeIdenticalSeedReproducesParsedSignatures,
        withResource (createRepositorySeed prepareIntegrationSeed) removeRepositorySeed $ \getRichSeed ->
          testCase "v3 exact archive rejects missing or mismatched target placement coverage" $ getRichSeed >>= testV3ExactArchiveRejectsInvalidTargetCoverage,
        testCase "competing tree-identical targets retain their own provenance projections" $ getSimpleSeed >>= testCompetingTreeIdenticalRefreshes,
        testCase "post-sync candidate without current ref is not published" $ getSimpleSeed >>= testPostSyncMissingCurrentRefIsNotPublished,
        testCase "clone refresh cancellation cleans its private candidate" $ getSimpleSeed >>= testCloneRefreshCancellationCleansCandidate,
        testCase "after-copy source replacement cannot alter private candidate validation" $ getSimpleSeed >>= testAfterCopySourceReplacementCannotAlterPrivateCandidate,
        testCase "tree-identical zero-scan clone publishes a zero history counter" $ getSimpleSeed >>= testTreeIdenticalZeroScanClone,
        testCase "trailer_in_noise_commit_forces_provenance_rescan" $ getSimpleSeed >>= testTrailerInNoise,
        -- From test_merge_aware_cache.py
        testCase "adr_bearing_merge_rebuilds_changed_managed_tree" $ getSimpleSeed >>= testAdrBearingMerge
      ]

data IntegrationSeed = IntegrationSeed
  { integrationSeedRevision :: Text
  }

data SimpleIntegrationSeed = SimpleIntegrationSeed
  { simpleIntegrationSeedRevision :: Text,
    simpleIntegrationSeedOperationId :: Text
  }

prepareIntegrationSeed :: FilePath -> IO (FilePath, IntegrationSeed)
prepareIntegrationSeed root = do
  repository <- createTestRepo root
  basisText <- gitStdout repository ["rev-parse", "HEAD"]
    >>= pure . strip . decodeUtf8 . LBS.toStrict
  basis <- case mkGitOid basisText of
    Left problem -> assertFailure (show problem) >> fail "unreachable"
    Right oid -> pure oid
  files <- case healthyCompilerFiles basis of
    Left problem -> assertFailure (T.unpack problem) >> fail "unreachable"
    Right value -> pure value
  revision <- commitFiles repository files
  compiled <- adraiJsonOrThrow repository ["compile", "--json"]
  case parseCompileResult compiled of
    Nothing -> assertFailure "production seed compile result did not decode" >> fail "unreachable"
    Just result -> do
      coldCompilerRevision result @?= revision
      coldCompilerCacheMode result @?= "full"
  cacheDir <- getCacheDir repository
  archives <- getCacheFiles cacheDir
  archive <- case archives of
    [path] -> pure path
    paths -> assertFailure ("expected one production seed archive, found " <> show paths) >> fail "unreachable"
  validateCachePublicationContract archive >>= assertBool "production integration seed must satisfy the publication contract"
  connection <- open archive
  operationCount <- query_ connection "SELECT count(*) FROM operation" :: IO [Only Int]
  provenanceCounts <- forM
    [ "operation_commit",
      "operation_target_coverage",
      "target_reachable_commit"
    ] $ \table -> query_ connection ("SELECT count(*) FROM " <> table) :: IO [Only Int]
  close connection
  operationCount @?= [Only 2]
  assertBool "production integration seed must retain non-final provenance rows"
    (all (\case [Only count] -> count > 1; _ -> False) provenanceCounts)
  pure (repository, IntegrationSeed revision)

prepareSimpleIntegrationSeed :: FilePath -> IO (FilePath, SimpleIntegrationSeed)
prepareSimpleIntegrationSeed root = do
  repository <- createTestRepo root
  basisText <- gitStdout repository ["rev-parse", "HEAD"]
    >>= pure . strip . decodeUtf8 . LBS.toStrict
  basis <- case mkGitOid basisText of
    Left problem -> assertFailure (show problem) >> fail "unreachable"
    Right oid -> pure oid
  (operationId, files) <- case healthySimpleCompilerFiles basis of
    Left problem -> assertFailure (T.unpack problem) >> fail "unreachable"
    Right value -> pure value
  revision <- commitFiles repository files
  compiled <- adraiJsonOrThrow repository ["compile", "--json"]
  result <- case parseCompileResult compiled of
    Nothing -> assertFailure "production simple seed compile result did not decode" >> fail "unreachable"
    Just value -> pure value
  coldCompilerCacheMode result @?= "full"
  coldCompilerDocumentsParsed result @?= 4
  coldCompilerRevision result @?= revision
  cacheDir <- getCacheDir repository
  archives <- getCacheFiles cacheDir
  archive <- case archives of
    [path] -> pure path
    paths -> assertFailure ("expected one simple production seed archive, found " <> show paths) >> fail "unreachable"
  validateCachePublicationContract archive >>= assertBool "simple production seed must satisfy the publication contract"
  pure (repository, SimpleIntegrationSeed revision operationId)

-- =====================================================================
-- Test 3: Corrupt provenance overlay rebuilds from cache
-- =====================================================================

testCorruptProvenanceRebuild :: RepositorySeed SimpleIntegrationSeed -> IO ()
testCorruptProvenanceRebuild seed =
  withPrivateRepositorySeed seed $ \_ repo -> do
    -- Corrupt the provenance database
    let provenanceDb = repo </> ".adrai" </> "provenance.sqlite"
    BS.writeFile provenanceDb "not sqlite data"
    -- Compile again - should rebuild from cache
    result <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult result of
      Nothing -> assertFailure "could not parse result2"
      Just cr -> compileDocsParsed cr @?= 0

-- | A complete v3 archive seeds an empty overlay from operation_member only.
-- The source signature was originally registered from parsed documents, so an
-- exact equality proves the persisted member tuple is a sufficient authority.
testV3TreeIdenticalSeedReproducesParsedSignatures :: RepositorySeed SimpleIntegrationSeed -> IO ()
testV3TreeIdenticalSeedReproducesParsedSignatures seed =
  withPrivateRepositorySeed seed $ \_ repo -> do
    let overlayPath = repo </> ".adrai" </> "provenance.sqlite"
    parsedOverlay <- open overlayPath
    parsedSignatures <- query_ parsedOverlay "SELECT op_id,signature FROM registered_operation ORDER BY op_id" :: IO [(Text, Text)]
    close parsedOverlay
    assertBool "cold parse must register at least one operation" (not (null parsedSignatures))
    removeFile overlayPath
    void (commitFilesWithMsg repo [("src/v3-tree-identical-noise.txt", "noise only\n")] "seed v3 provenance from archive")
    result <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult result of
      Nothing -> assertFailure "could not parse v3 tree-identical seed result"
      Just compiled -> do
        compileCacheMode compiled @?= "incremental"
        compileIncKind compiled @?= "tree-identical"
        compileDocsParsed compiled @?= 0
        compileDocsReused compiled @?= 4
    seededOverlay <- open overlayPath
    seededSignatures <- query_ seededOverlay "SELECT op_id,signature FROM registered_operation ORDER BY op_id" :: IO [(Text, Text)]
    actualObserved <- query_ seededOverlay "SELECT count(*) FROM observed_commit" :: IO [Only Int]
    close seededOverlay
    seededSignatures @?= parsedSignatures
    target <- strip . decodeUtf8 . LBS.toStrict <$> gitStdout repo ["rev-parse", "HEAD"]
    cacheDir <- getCacheDir repo
    archive <- open (cacheDir </> T.unpack target <> ".sqlite")
    publishedObserved <- query_ archive "SELECT value FROM meta WHERE key='provenance.observed_commit_count'" :: IO [Only Text]
    close archive
    case (publishedObserved, actualObserved) of
      ([Only publishedCount], [Only actualCount]) -> do
        publishedCount @?= T.pack (show actualCount)
        assertBool "post-refresh observed count must include discovered commits" (actualCount > 0)
      _ -> assertFailure "observed commit count queries returned invalid results"

-- | A cache-v3 publication contains one target-bound certificate per
-- registered operation.  These certificates are part of the archive
-- authority, so an exact archive with either a missing or a forged one must
-- fail validation before selection can reuse it.
testV3ExactArchiveRejectsInvalidTargetCoverage :: RepositorySeed IntegrationSeed -> IO ()
testV3ExactArchiveRejectsInvalidTargetCoverage seed =
  withPrivateRepositorySeed seed $ \seedFacts repo -> do
    cacheDir <- getCacheDir repo
    archives <- getCacheFiles cacheDir
    sourceArchive <- case archives of
      [path] -> pure path
      paths -> assertFailure ("expected one v3 archive, found " <> show paths) >> fail "unreachable"
    -- The following checks mutate copies only; the authoritative cold archive
    -- must first satisfy the full publication contract.
    sourceValid <- validateCachePublicationContract sourceArchive
    source <- open sourceArchive
    resolvedRows <- query_ source "SELECT value FROM meta WHERE key='resolved_oid'" :: IO [Only Text]
    coverage <- query_ source "SELECT op_id,target_oid,registration_signature FROM operation_target_coverage ORDER BY op_id" :: IO [(Text, Text, Text)]
    members <- query_ source "SELECT operation_id,object_id,path,blob_oid,semantic_digest FROM operation_member ORDER BY operation_id,path,object_id" :: IO [(Text, Text, Text, Text, Text)]
    operationCommits <- query_ source "SELECT op_id FROM operation_commit" :: IO [Only Text]
    provenanceCounts <- forM
      [ "operation_commit"
      , "operation_target_coverage"
      , "target_reachable_commit"
      , "line_config"
      ]
      $ \table -> do
        rows <- query_ source ("SELECT count(*) FROM " <> table) :: IO [Only Int]
        pure (table, rows)
    close source
    assertBool ("cold archive is publishable before target-coverage corruption; coverage=" <> show coverage <> ", members=" <> show members) sourceValid
    case resolvedRows of
      [Only resolvedOid] ->
        do
          resolvedOid @?= integrationSeedRevision seedFacts
          assertBool "the shared exact-target authority accepts the healthy archive"
            =<< validateExactCacheTarget sourceArchive resolvedOid
          assertBool "the shared exact-target authority rejects a different resolved revision"
            . not =<< validateExactCacheTarget sourceArchive (resolvedOid <> "0")
      _ -> assertFailure "fixture archive did not contain exactly one resolved_oid"
    assertBool "fixture requires at least one exact target certificate" (not (null coverage))
    assertBool "fixture requires an operation_commit payload to tamper" (not (null operationCommits))
    let hasNonFinalProvenanceRow (table, rows)
          | table == "line_config" = True
          | otherwise = case rows of
              [Only count] -> count > 1
              _ -> False
    assertBool "fixture requires non-final target-qualified provenance rows"
      (all hasNonFinalProvenanceRow provenanceCounts)

    let oldSchemaArchive = cacheDir </> "old-schema.sqlite"
        missingCoverageArchive = cacheDir </> "missing-target-coverage.sqlite"
        mismatchedCoverageArchive = cacheDir </> "mismatched-target-coverage.sqlite"
        extraCoverageArchive = cacheDir </> "extra-target-coverage.sqlite"
        tamperedCommitArchive = cacheDir </> "tampered-operation-commit.sqlite"
        missingMemberArchive = cacheDir </> "missing-operation-member.sqlite"
        malformedParentsArchive = cacheDir </> "malformed-operation-parents.sqlite"
        invalidLandingArchive = cacheDir </> "invalid-line-landing.sqlite"
        malformedConfigArchive = cacheDir </> "malformed-line-config.sqlite"
        invalidReachabilityArchive = cacheDir </> "invalid-target-reachability.sqlite"
        deletedCountArchives :: [(Text, Query, FilePath)]
        deletedCountArchives =
          [ ("operation_commit", "operation_commit", cacheDir </> "deleted-operation-commit.sqlite")
          , ("operation_target_coverage", "operation_target_coverage", cacheDir </> "deleted-target-coverage.sqlite")
          , ("target_reachable_commit", "target_reachable_commit", cacheDir </> "deleted-target-reachability.sqlite")
          , ("line_config", "line_config", cacheDir </> "deleted-line-config.sqlite")
          ]
    copyFile sourceArchive oldSchemaArchive
    oldSchema <- open oldSchemaArchive
    execute_ oldSchema "UPDATE meta SET value='adrai-cache/1' WHERE key='schema'"
    close oldSchema
    assertBool "old-schema archive fails the production cache contract"
      . not =<< validateCacheContract oldSchemaArchive
    assertBool "old-schema archive fails exact-target authority for its healthy target"
      . not =<< validateExactCacheTarget oldSchemaArchive (integrationSeedRevision seedFacts)

    copyFile sourceArchive missingCoverageArchive
    missing <- open missingCoverageArchive
    execute_ missing "DELETE FROM operation_target_coverage"
    close missing
    assertBool "v3 exact archive rejects missing target placement coverage"
      . not =<< validateCachePublicationContract missingCoverageArchive

    copyFile sourceArchive mismatchedCoverageArchive
    mismatched <- open mismatchedCoverageArchive
    execute_ mismatched "UPDATE operation_target_coverage SET registration_signature='forged-signature'"
    close mismatched
    assertBool "v3 exact archive rejects mismatched target placement coverage"
      . not =<< validateCachePublicationContract mismatchedCoverageArchive

    copyFile sourceArchive extraCoverageArchive
    extra <- open extraCoverageArchive
    execute_ extra "INSERT INTO operation_target_coverage VALUES('unknown-operation','forged-target','forged-signature')"
    close extra
    assertBool "v3 exact archive rejects coverage for an unknown operation"
      . not =<< validateCachePublicationContract extraCoverageArchive

    copyFile sourceArchive tamperedCommitArchive
    tampered <- open tamperedCommitArchive
    execute_ tampered "UPDATE operation_commit SET classification='forged-classification'"
    close tampered
    -- Recompute the stored materialization digest: validation must still
    -- reject a semantically invalid operation_commit payload rather than
    -- trusting that self-reported fingerprint.
    refreshCacheMaterializationFingerprint tamperedCommitArchive
    assertBool "v3 exact archive rejects a tampered operation_commit after fingerprint refresh"
      . not =<< validateCachePublicationContract tamperedCommitArchive

    copyFile sourceArchive missingMemberArchive
    missingMember <- open missingMemberArchive
    execute_ missingMember "DELETE FROM operation_member WHERE operation_id=(SELECT operation_id FROM operation ORDER BY operation_id LIMIT 1)"
    close missingMember
    refreshCacheMaterializationFingerprint missingMemberArchive
    assertBool "v3 exact archive rejects an operation with no member signature after fingerprint refresh"
      . not =<< validateCachePublicationContract missingMemberArchive

    copyFile sourceArchive malformedParentsArchive
    malformedParents <- open malformedParentsArchive
    execute_ malformedParents "UPDATE operation_commit SET parents_json='[\"not-an-oid\"]'"
    close malformedParents
    refreshCacheMaterializationFingerprint malformedParentsArchive
    assertBool "v3 exact archive rejects noncanonical operation parents after fingerprint refresh"
      . not =<< validateCachePublicationContract malformedParentsArchive

    copyFile sourceArchive invalidLandingArchive
    invalidLanding <- open invalidLandingArchive
    execute_ invalidLanding "INSERT INTO line_landing(config_key,op_id,line_id,ref_name,commit_oid,complete) SELECT (SELECT config_key FROM line_config),op_id,'forged-line','forged-ref',commit_oid,2 FROM operation_commit LIMIT 1"
    close invalidLanding
    refreshCacheMaterializationFingerprint invalidLandingArchive
    assertBool "v3 exact archive rejects non-boolean line landing completion after fingerprint refresh"
      . not =<< validateCachePublicationContract invalidLandingArchive

    copyFile sourceArchive malformedConfigArchive
    malformedConfig <- open malformedConfigArchive
    execute_ malformedConfig "UPDATE line_config SET config_json='{}'"
    close malformedConfig
    refreshCacheMaterializationFingerprint malformedConfigArchive
    assertBool "v3 exact archive rejects a malformed active line config after fingerprint refresh"
      . not =<< validateCachePublicationContract malformedConfigArchive

    copyFile sourceArchive invalidReachabilityArchive
    invalidReachability <- open invalidReachabilityArchive
    execute_ invalidReachability "INSERT INTO target_reachable_commit(target_oid,commit_oid) VALUES((SELECT value FROM meta WHERE key='resolved_oid'),'not-an-oid')"
    close invalidReachability
    refreshCacheMaterializationFingerprint invalidReachabilityArchive
    assertBool "v3 exact archive rejects malformed target reachability membership after fingerprint refresh"
      . not =<< validateCachePublicationContract invalidReachabilityArchive

    -- The materialization fingerprint deliberately excludes the metadata
    -- commitments.  Recomputing it after a private row deletion must not make
    -- a truncated v3 provenance projection publishable.
    forM_ deletedCountArchives $ \(tableName, table, archive) -> do
      copyFile sourceArchive archive
      tamperedCount <- open archive
      execute_ tamperedCount ("DELETE FROM " <> table <> " WHERE rowid=(SELECT rowid FROM " <> table <> " ORDER BY rowid LIMIT 1)")
      close tamperedCount
      refreshCacheMaterializationFingerprint archive
      assertBool ("v3 exact archive rejects a deleted " <> unpack tableName <> " row after fingerprint refresh")
        . not =<< validateCachePublicationContract archive

-- | Refresh callbacks are allowed to mutate only a disposable candidate.  A
-- missing observation for the advertised current ref must reject publication.
testPostSyncMissingCurrentRefIsNotPublished :: RepositorySeed SimpleIntegrationSeed -> IO ()
testPostSyncMissingCurrentRefIsNotPublished seed =
  withPrivateRepositorySeed seed $ \_ repo -> do
    cacheDir <- getCacheDir repo
    sourceArchives <- getCacheFiles cacheDir
    sourceArchive <- case sourceArchives of
      [path] -> pure path
      paths -> assertFailure ("expected one source archive, found " <> show paths) >> fail "unreachable"
    void (commitFilesWithMsg repo [("src/invalid-refresh-noise.txt", "noise only\n")] "invalid refreshed candidate")
    target <- strip . decodeUtf8 . LBS.toStrict <$> gitStdout repo ["rev-parse", "HEAD"]
    observation <-
      observeValidatedPostCommitCloneForTest
        sourceArchive
        (GitOid target)
        (Just 1)
        (PostCommitRefreshDeleteRef "refs/heads/main")
        (pure ())
    assertBool "invalid post-sync candidate must not publish" (not (postCommitCloneIndexed observation))
    postCommitCloneTargetExisted observation @?= False
    postCommitCloneCandidateResidue observation @?= False

testCloneRefreshCancellationCleansCandidate :: RepositorySeed SimpleIntegrationSeed -> IO ()
testCloneRefreshCancellationCleansCandidate seed =
  withPrivateRepositorySeed seed $ \_ repo -> do
    cacheDir <- getCacheDir repo
    sourceArchives <- getCacheFiles cacheDir
    sourceArchive <- case sourceArchives of
      [path] -> pure path
      paths -> assertFailure ("expected one source archive, found " <> show paths) >> fail "unreachable"
    target <- strip . decodeUtf8 . LBS.toStrict <$> gitStdout repo ["rev-parse", "HEAD"]
    observation <-
      observeValidatedPostCommitCloneForTest sourceArchive (GitOid target) (Just 0) PostCommitRefreshCancel (pure ())
    postCommitCloneCancellation observation @?= Just PostCommitCloneThreadKilled
    postCommitCloneTargetExisted observation @?= False
    postCommitCloneCandidateResidue observation @?= False

-- | The validation authority is the owned copy, never a source pathname that
-- can be changed after copy completion.  The test hook receives no candidate
-- capability, only a chance to corrupt the independently owned public source.
testAfterCopySourceReplacementCannotAlterPrivateCandidate :: RepositorySeed SimpleIntegrationSeed -> IO ()
testAfterCopySourceReplacementCannotAlterPrivateCandidate seed =
  withPrivateRepositorySeed seed $ \_ repo -> do
    cacheDir <- getCacheDir repo
    sourceArchives <- getCacheFiles cacheDir
    sourceArchive <- case sourceArchives of
      [path] -> pure path
      paths -> assertFailure ("expected one source archive, found " <> show paths) >> fail "unreachable"
    revision <- strip . decodeUtf8 . LBS.toStrict <$> gitStdout repo ["rev-parse", "HEAD"]
    let corruptSource = do
          connection <- open sourceArchive
          execute_ connection "DELETE FROM meta"
          close connection
    observation <-
      observeValidatedPostCommitCloneForTest
        sourceArchive
        (GitOid revision)
        (Just 0)
        PostCommitRefreshNoop
        corruptSource
    assertBool ("owned copied candidate must remain valid after public source corruption: " <> show observation) (postCommitCloneIndexed observation)
    postCommitCloneTargetExisted observation @?= True
    postCommitCloneCandidateResidue observation @?= False

testTreeIdenticalZeroScanClone :: RepositorySeed SimpleIntegrationSeed -> IO ()
testTreeIdenticalZeroScanClone seed =
  withPrivateRepositorySeed seed $ \_ repo -> do
    cacheDir <- getCacheDir repo
    sourceArchives <- getCacheFiles cacheDir
    sourceArchive <- case sourceArchives of
      [path] -> pure path
      paths -> assertFailure ("expected one source archive, found " <> show paths) >> fail "unreachable"
    void (commitFilesWithMsg repo [("src/zero-scan-noise.txt", "noise only\n")] "zero scan clone")
    target <- strip . decodeUtf8 . LBS.toStrict <$> gitStdout repo ["rev-parse", "HEAD"]
    observation <- observeValidatedPostCommitCloneForTest sourceArchive (GitOid target) (Just 0) PostCommitRefreshNoop (pure ())
    assertBool ("unrefreshed cross-revision v3 clone must fail closed: " <> show observation) (not (postCommitCloneIndexed observation))
    postCommitCloneTargetExisted observation @?= False
    postCommitCloneCandidateResidue observation @?= False

-- | Two target revisions can share one overlay, but their copies must remain
-- serialized through refresh and candidate sync.  Each private archive must
-- receive a complete provenance projection rather than a later target's rows.
type DecisionProjection = ([Only Text], [(Text, Text, Int)])

readDecisionProjection :: FilePath -> IO DecisionProjection
readDecisionProjection archivePath = do
  connection <- open archivePath
  adrs <- query_ connection "SELECT adr_id FROM reduced_adr ORDER BY adr_id" :: IO [Only Text]
  -- search_document is projected from current decision heads. Its conflicted
  -- flag is therefore narrower than reduced_adr.conflicted, which also covers
  -- conflicts on non-decision axes.
  decisions <- query_ connection "SELECT adr_id,candidate_record_id,conflicted FROM search_document ORDER BY adr_id,candidate_record_id,conflicted" :: IO [(Text, Text, Int)]
  close connection
  pure (adrs, decisions)

testCompetingTreeIdenticalRefreshes :: RepositorySeed SimpleIntegrationSeed -> IO ()
testCompetingTreeIdenticalRefreshes seed =
  withPrivateRepositorySeed seed $ \seedFacts repo -> do
    cacheDir <- getCacheDir repo
    sourceArchives <- getCacheFiles cacheDir
    assertBool "baseline archive must exist before competing refreshes" (not (null sourceArchives))
    baselineProjection <- case sourceArchives of
      [sourceArchive] -> readDecisionProjection sourceArchive
      paths -> assertFailure ("expected one immutable baseline archive, found " <> show paths) >> fail "unreachable"
    baselineBytes <- mapM (\sourceArchive -> do
      bytes <- BS.readFile sourceArchive
      pure (sourceArchive, bytes)) sourceArchives
    git repo ["switch", "-c", "refresh-left"]
    leftTarget <- commitFilesWithMsg repo [("src/refresh-left.txt", "left noise\n")] "left target"
    git repo ["switch", "main"]
    git repo ["switch", "-c", "refresh-right"]
    rightTarget <- commitFilesWithMsg repo [("src/refresh-right.txt", "right noise\n")] "right target"
    git repo ["switch", "main"]
    (leftResult, rightResult) <- Async.concurrently
      (adraiJson repo ["compile", "--at", T.unpack leftTarget, "--json"])
      (adraiJson repo ["compile", "--at", T.unpack rightTarget, "--json"])
    forM_ baselineBytes $ \(sourceArchive, expectedBytes) -> do
      assertBool "concurrent refresh must retain the immutable baseline archive" =<< doesFileExist sourceArchive
      assertBool "concurrent refresh must retain a reusable baseline archive" =<< validateCacheContract sourceArchive
      actualBytes <- BS.readFile sourceArchive
      actualBytes @?= expectedBytes
    let assertTargetResult label target result =
          case result >>= maybe (Left "could not parse compile result") Right . parseCompileResult of
            Left problem -> assertFailure (T.unpack label <> ": " <> T.unpack problem)
            Right compiled -> do
              let mode = compileCacheMode compiled
                  kind = compileIncKind compiled
              coldCompilerRevision compiled @?= target
              coldCompilerDatabase compiled @?= cacheDir </> T.unpack target <> ".sqlite"
              case (mode, kind) of
                ("incremental", "tree-identical") -> do
                  compileDocsParsed compiled @?= 0
                  assertBool (T.unpack label <> ": tree-identical reuse must reuse documents") (compileDocsReused compiled > 0)
                  compileAdrsRebuilt compiled @?= 0
                  assertBool (T.unpack label <> ": tree-identical reuse must reuse ADRs") (compileAdrsReused compiled > 0)
                ("full", "full") -> do
                  assertBool (T.unpack label <> ": full fallback must parse documents") (compileDocsParsed compiled > 0)
                  compileDocsReused compiled @?= 0
                  assertBool (T.unpack label <> ": full fallback must rebuild ADRs") (compileAdrsRebuilt compiled > 0)
                  compileAdrsReused compiled @?= 0
                _ -> assertFailure
                  (T.unpack label <> ": expected either tree-identical reuse or a conservative full fallback, got mode="
                    <> T.unpack mode <> ", kind=" <> T.unpack kind)
              pure compiled
    leftCompiled <- assertTargetResult "left target" leftTarget leftResult
    rightCompiled <- assertTargetResult "right target" rightTarget rightResult
    assertBool "competing targets must publish distinct immutable archive paths"
      (coldCompilerDatabase leftCompiled /= coldCompilerDatabase rightCompiled)
    assertBool "competing targets must differ from the immutable seed revision"
      (leftTarget /= simpleIntegrationSeedRevision seedFacts && rightTarget /= simpleIntegrationSeedRevision seedFacts)
    assertProvenanceProjection baselineProjection cacheDir leftTarget rightTarget
    assertProvenanceProjection baselineProjection cacheDir rightTarget leftTarget
  where
    assertProvenanceProjection expectedDecisionProjection cacheDir target otherTarget = do
      let archivePath = cacheDir </> T.unpack target <> ".sqlite"
      assertBool "target archive must satisfy the public publication contract" =<< validateCachePublicationContract archivePath
      assertBool "target archive must remain eligible for future reuse" =<< validateCacheContract archivePath
      archive <- open archivePath
      operationRows <- query_ archive "SELECT count(*) FROM operation_commit" :: IO [Only Int]
      refRows <- query_ archive "SELECT ref_name FROM ref_observation WHERE ref_name IN ('refs/heads/main','refs/heads/refresh-left','refs/heads/refresh-right') ORDER BY ref_name" :: IO [Only Text]
      mainTip <- query_ archive "SELECT tip_oid FROM ref_observation WHERE ref_name='refs/heads/main'" :: IO [Only Text]
      currentHead <- query_ archive "SELECT value FROM meta WHERE key='provenance.current_head'" :: IO [Only Text]
      currentRef <- query_ archive "SELECT value FROM meta WHERE key='provenance.current_ref'" :: IO [Only Text]
      requestedRevision <- query_ archive "SELECT value FROM meta WHERE key='requested_revision'" :: IO [Only Text]
      resolvedOid <- query_ archive "SELECT value FROM meta WHERE key='resolved_oid'" :: IO [Only Text]
      close archive
      readDecisionProjection archivePath >>= (@?= expectedDecisionProjection)
      case operationRows of
        [Only count] -> assertBool "operation provenance was not copied" (count > 0)
        _ -> assertFailure "operation provenance count query returned an invalid result"
      refRows @?= [Only "refs/heads/main", Only "refs/heads/refresh-left", Only "refs/heads/refresh-right"]
      requestedRevision @?= [Only target]
      resolvedOid @?= [Only target]
      assertBool "archive requested revision must not belong to the competing target" (requestedRevision /= [Only otherTarget])
      assertBool "archive resolved OID must not belong to the competing target" (resolvedOid /= [Only otherTarget])
      currentHead @?= mainTip
      assertBool "historical target must not be recorded as the current main HEAD" (currentHead /= [Only target])
      currentRef @?= [Only "refs/heads/main"]

-- =====================================================================
-- Test 7: Trailer in noise commit
-- =====================================================================

testTrailerInNoise :: RepositorySeed SimpleIntegrationSeed -> IO ()
testTrailerInNoise seed =
  withPrivateRepositorySeed seed $ \seedFacts repo -> do
    let operationId' = simpleIntegrationSeedOperationId seedFacts
    -- Commit with ADRAI-Op trailer but no real ADRAI files.
    _ <- commitFilesWithMsg repo
      [ ( "src/misleading-trailer.txt", "not an ADRAI operation\n" ) ]
      ("ordinary work\n\nADRAI-Op: " <> operationId')
    result <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult result of
      Nothing -> assertFailure "could not parse trailer result"
      Just cr -> do
        compileCacheMode cr @?= "incremental"
        compileDocsParsed cr @?= 0
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
-- Test 14: ADR-bearing merge
-- =====================================================================

testAdrBearingMerge :: RepositorySeed SimpleIntegrationSeed -> IO ()
testAdrBearingMerge seed =
  withPrivateRepositorySeed seed $ \_ repo -> do
    -- Commit a second sealed production ADR on develop without invoking the
    -- create command's post-commit compiler or an intermediate warmup.
    git repo ["switch", "-c", "develop"]
    basisText <- gitStdout repo ["rev-parse", "HEAD"]
      >>= pure . strip . decodeUtf8 . LBS.toStrict
    basis <- case mkGitOid basisText of
      Left problem -> assertFailure (show problem) >> fail "unreachable"
      Right oid -> pure oid
    files <- case additionalSimpleCompilerFiles basis of
      Left problem -> assertFailure (T.unpack problem) >> fail "unreachable"
      Right value -> pure value
    _ <- commitFiles repo files

    -- Merge develop into main
    git repo ["switch", "main"]
    git repo
      [ "merge", "--no-ff", "develop",
        "-m", "promote architecture and product work" ]

    -- An ADR-bearing merge changes the managed tree.  The immutable archive
    -- model therefore rebuilds the merged snapshot rather than exposing the
    -- removed semantic-reuse mode.
    result <- adraiJsonOrThrow repo ["compile", "--json"]
    case parseCompileResult result of
      Nothing -> assertFailure "could not parse adr-merge result"
      Just cr -> do
        compileCacheMode cr @?= "full"
        compileIncKind cr @?= "full"
        compileDocsParsed cr @?= 8
        compileDocsReused cr @?= 0
        compileAdrsRebuilt cr @?= 2
        compileAdrsReused cr @?= 0

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
