{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Focused real-executable coverage.  This component deliberately owns only
-- a small Haskell fixture; importing the integration/stress support trees
-- would make the cache gate construct unrelated retrieval fixtures.
module Main (main) where

import qualified AcquisitionRegression
import Adrai.Provenance (GitOid, gitOidText, mkGitOid)
import Adrai.Git (discoverRepository, systemGit)
import Adrai.Compiler.CacheSelection
  ( CacheSelectionCascadeDecision (..),
    CacheSelectionKind (..),
    CacheSelectionMetrics (..),
    ExactArchiveCompileFacts,
    ExactArchiveAliasStatus (..),
    exactArchiveAliasCompileFacts,
    exactArchiveCompileMetadata,
    refreshCacheMaterializationFingerprint,
    validateCachePublicationContract,
    validateExactCacheTarget,
  )
import Adrai.Compiler.CacheSelection.TestSupport
  ( cachePublicationMaterializationFingerprintEvidence,
    CacheValidationWorkCounters (..),
    exactArchiveCleanupPlanForTest,
    loadCacheMetaForTest,
    observeExactCacheValidationWorkForTest,
    observeMaterializationFingerprintForTest,
    observeTargetReachabilityPlansForTest,
    readExactArchiveAliasStatusForTest,
     LeaseProbeResult (..),
     MaterializationFingerprintEvidence (..),
     MaterializationFingerprintIndexWork (..),
     ReuseDiscoveryEvent (..),
     ReuseDiscoveryPlan,
     TargetReachabilityPlanDetails (..),
    CacheSelectionLifecycleEvent (..),
    CacheSelectionLifecyclePlan,
    cacheSelectionLifecyclePlanForTest,
    cacheSelectionLifecyclePlanWithEvents,
    probeCacheSelectionLeaseCopy,
    reuseDiscoveryPlanForTest,
    reuseDiscoveryPlanWithEvents,
    reuseFactCacheKey,
    reuseFactPrivateBytes,
    reuseFactRank,
    reuseFactSourceRevision,
    withCacheSelectionCascadeForTest,
    withCacheSelectionCascadeWithPlansForTest,
    withExactArchiveAliasRepairForTest,
    withExactArchiveAliasRepairWithCleanupForTest,
    PublicationRefreshScenario (..),
    PublicationRefreshOutcome (..),
    PublicationRefreshCleanupArtifact (..),
    PublicationRefreshCleanupRecord (..),
    PublicationRefreshObservation (..),
    observePublicationRefreshForTest,
    observeFtsPayloadMaterializationForTest,
  )
import CacheFixture (healthyCompilerFiles)
import RetainedCacheSeed (CacheSeed, createCacheSeed, removeCacheSeed, withPrivateCacheSeed)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (AsyncException (..), IOException, SomeException, bracket, fromException, throwIO, try)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple (Connection, Only (..), Query, close, execute, execute_, open, query, query_)
import System.Directory (copyFile, createDirectory, createDirectoryIfMissing, doesFileExist, getFileSize, getModificationTime, listDirectory, removeDirectory, removeFile, renameFile, setModificationTime)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), takeDirectory)
import System.Info (os)
import System.IO.Error (ioeGetErrorString, isPermissionError)
import System.Process (readProcessWithExitCode)
import Test.Tasty (defaultMain, testGroup, withResource)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

main :: IO ()
main = defaultMain $
  withResource (createCacheSeed prepareFocusedSeed) removeCacheSeed $ \getSeed ->
    testGroup "production cache selection"
      [ testCase "cold production archive has canonical fingerprints and bounded validation work" $ getSeed >>= coldProductionContract,
        testCase "exact CLI reports immutable archive reuse" $ getSeed >>= exactCliContract,
        testCase "corrupt authoritative archive falls back and republishes" $ getSeed >>= corruptAuthorityFallbackContract,
        testCase "archive, alias, and publication contracts reject forged state" $ getSeed >>= realExecutableCacheContract,
        testCase "candidate selection owns private validation and cleanup" $ getSeed >>= candidateLifecycleContract,
        AcquisitionRegression.tests (withFocusedSeedArchive getSeed)
      ]

withFocusedSeedArchive :: IO (CacheSeed FocusedSeed) -> (FilePath -> IO result) -> IO result
withFocusedSeedArchive getSeed action =
  getSeed >>= \seed ->
    withPrivateCacheSeed seed $ \focused repository ->
      assertSnapshot repository (focusedRevision focused) >>= action

data FocusedSeed = FocusedSeed
  { focusedColdOutput :: String,
    focusedRevision :: GitOid
  }

prepareFocusedSeed :: FilePath -> IO FocusedSeed
prepareFocusedSeed repository = do
  initialiseRepository repository
  basis <- repositoryHead repository
  files <- fixtureOrFail (healthyCompilerFiles basis)
  _ <- writeAndCommit repository "compiler fixture" files
  removeCurrentIndex repository
  cold <- compile "cold compile" repository
  revision <- repositoryHead repository
  pure FocusedSeed
    { focusedColdOutput = cold,
      focusedRevision = revision
    }

coldProductionContract :: CacheSeed FocusedSeed -> IO ()
coldProductionContract seed =
  withPrivateCacheSeed seed $ \focused repository -> do
      let temporary = takeDirectory repository
          cold = focusedColdOutput focused
          coldRevision = focusedRevision focused
      assertFields "cold compile"
        [ "\"cache_mode\":\"full\"",
          "\"incremental_kind\":\"full\"",
          "\"adrs_rebuilt\":1",
          "\"ann_buckets\":0",
          "\"embedding_computed\":0",
          "\"cache_retain_revisions\":0"
        ] cold
      assertAbsent "cold compile must not claim document reuse" "\"documents_parsed\":0" cold
      coldSnapshot <- assertSnapshot repository coldRevision
      let missingSnapshot = coldSnapshot <> ".missing"
      missingExact <- validateExactCacheTarget missingSnapshot (gitOidText coldRevision)
      assertBool "the non-creating exact path validator rejects a missing archive" (not missingExact)
      missingArtifacts <- mapM doesFileExist [missingSnapshot, missingSnapshot <> "-journal", missingSnapshot <> "-wal", missingSnapshot <> "-shm"]
      assertBool "a missing exact archive creates neither database nor SQLite sidecars" (not (or missingArtifacts))
      -- The fixture deliberately assigns current-connection IDs out of graph
      -- axis order.  The persisted table is ordered by connection ID, so this
      -- bounded production shape pins the writer/validator fingerprint order.
      fingerprintEvidence <- cachePublicationMaterializationFingerprintEvidence coldSnapshot
      case fingerprintEvidence of
        Just (writtenFingerprint, recomputedFingerprint, Nothing) -> do
          assertBool
            ( "the persisted materialization fingerprint must equal its canonical reconstruction; written="
                <> Text.unpack writtenFingerprint
                <> ", recomputed="
                <> Text.unpack recomputedFingerprint
            )
            (writtenFingerprint == recomputedFingerprint)
        Just (_, _, Just _) -> assertFailure "cache v3 must not accept a legacy materialization frame"
        Nothing -> assertFailure "the cold archive did not provide materialization fingerprint evidence"
      healthySnapshot <- validateCachePublicationContract coldSnapshot
      assertBool "the real complete compiler fixture validates its canonical cache-v3 fingerprint" healthySnapshot
      assertPublicationRefreshSafety temporary coldSnapshot
      (workAccepted, workCounters) <- observeExactCacheValidationWorkForTest coldSnapshot (gitOidText coldRevision)
      assertBool "the exact work-counter probe accepts the production archive" workAccepted
      cacheValidationInvocations workCounters @?= 1
      cacheValidationSourceOpens workCounters @?= 1
      cacheValidationFullMaterializationLoads workCounters @?= 1
      cacheValidationFtsPayloadMaterializations workCounters @?= 0
      cacheValidationFtsParityChecks workCounters @?= expectedFtsValidationFamilies
      cacheValidationFtsIntegrityChecks workCounters @?= expectedFtsValidationFamilies
      cacheValidationCanonicalFamilyScans workCounters @?= expectedCanonicalFamilyScans
      Map.keysSet (cacheValidationCanonicalFamilyRows workCounters) @?= Map.keysSet expectedCanonicalFamilyScans
      assertBool "actual production family row totals are non-negative" (all (>= 0) (Map.elems (cacheValidationCanonicalFamilyRows workCounters)))
      assertValidationWorkFailureCounters temporary coldSnapshot (gitOidText coldRevision)
      assertTargetReachabilityPlans coldSnapshot (gitOidText coldRevision)
      assertIndexedFingerprintFixture temporary coldSnapshot

exactCliContract :: CacheSeed FocusedSeed -> IO ()
exactCliContract seed =
  withPrivateCacheSeed seed $ \focused repository -> do
    let revision = focusedRevision focused
        currentAlias = repository </> ".adrai" </> "index.sqlite"
    archive <- assertSnapshot repository revision
    archiveBytes <- BS.readFile archive
    archiveMtime <- getModificationTime archive
    aliasBytes <- BS.readFile currentAlias
    aliasMtime <- getModificationTime currentAlias
    exact <- compile "exact immutable archive reuse" repository
    assertFields "exact immutable archive reuse"
      [ "\"cache_mode\":\"exact\"",
        "\"incremental_kind\":\"exact\"",
        "\"documents_parsed\":0",
        "\"adrs_rebuilt\":0",
        "\"documents_reused\":6",
        "\"adrs_reused\":1"
      ] exact
    BS.readFile archive >>= (@?= archiveBytes)
    getModificationTime archive >>= (@?= archiveMtime)
    BS.readFile currentAlias >>= (@?= aliasBytes)
    getModificationTime currentAlias >>= (@?= aliasMtime)

    -- A damaged current alias cannot invalidate or replace the authoritative
    -- immutable archive. Exact acquisition rebuilds only the alias from that
    -- surviving archive.
    BS.writeFile currentAlias "not sqlite"
    validateCachePublicationContract currentAlias >>= assertBool "the corrupt current alias fixture must fail validation" . not
    validateExactCacheTarget archive (gitOidText revision) >>= assertBool "the authoritative exact archive survives alias corruption"
    repairedAlias <- compile "corrupt current alias exact recovery" repository
    assertFields "corrupt current alias exact recovery"
      [ "\"cache_mode\":\"exact\"",
        "\"incremental_kind\":\"exact\"",
        "\"documents_parsed\":0",
        "\"adrs_rebuilt\":0",
        "\"documents_reused\":6",
        "\"adrs_reused\":1"
      ] repairedAlias
    BS.readFile archive >>= (@?= archiveBytes)
    getModificationTime archive >>= (@?= archiveMtime)
    validateExactCacheTarget archive (gitOidText revision) >>= assertBool "exact alias repair leaves the immutable archive valid"
    validateCachePublicationContract currentAlias >>= assertBool "exact alias recovery republishes a canonical current index"

corruptAuthorityFallbackContract :: CacheSeed FocusedSeed -> IO ()
corruptAuthorityFallbackContract seed =
  withPrivateCacheSeed seed $ \focused repository -> do
    let revision = focusedRevision focused
    archive <- assertSnapshot repository revision
    connection <- open archive
    postingRows <- query_ connection "SELECT id FROM fts_search_exact_data WHERE id > 10 ORDER BY id DESC LIMIT 1" :: IO [Only Int]
    postingId <- case postingRows of
      [Only value] -> pure value
      _ -> close connection >> assertFailure "fixture did not expose a removable FTS posting segment" >> fail "unreachable"
    execute connection "DELETE FROM fts_search_exact_data WHERE id=?" (Only postingId)
    readableRows <- query_ connection "SELECT count(*) FROM fts_search_exact" :: IO [Only Int]
    close connection
    readableRows @?= [Only 1]
    validateCachePublicationContract archive >>= assertBool "a readable FTS table with a missing posting segment is rejected" . not
    validateExactCacheTarget archive (gitOidText revision) >>= assertBool "the exact validator rejects missing FTS postings" . not
    rebuilt <- compile "corrupt authoritative archive fallback" repository
    assertFields "corrupt authoritative archive fallback"
      [ "\"cache_mode\":\"full\"",
        "\"incremental_kind\":\"full\"",
        "\"documents_parsed\":6",
        "\"documents_reused\":0",
        "\"adrs_rebuilt\":1",
        "\"adrs_reused\":0"
      ] rebuilt
    repaired <- assertSnapshot repository revision
    validateCachePublicationContract repaired >>= assertBool "cold fallback republishes a canonical archive"

realExecutableCacheContract :: CacheSeed FocusedSeed -> IO ()
realExecutableCacheContract seed =
  withPrivateCacheSeed seed $ \focused repository -> do
      let coldRevision = focusedRevision focused
      coldSnapshot <- assertSnapshot repository coldRevision

      -- Exact selection owns both full validations and returns only closed
      -- facts/status.  A healthy result is exactly archive then alias; neither
      -- a connection nor a callback capable of retaining one escapes.
      let currentAlias = repository </> ".adrai" </> "index.sqlite"
      archiveBytesBeforeProof <- BS.readFile coldSnapshot
      archiveMtimeBeforeProof <- getModificationTime coldSnapshot
      validationEvents <- newIORef ([] :: [Text])
      let observeValidation = modifyIORef' validationEvents (<> ["validation"])
          afterHeldFacts = modifyIORef' validationEvents (<> ["held-facts"])
      healthy <- readExactArchiveAliasStatusForTest observeValidation afterHeldFacts coldSnapshot (gitOidText coldRevision) currentAlias
      case healthy of
        Just (ExactArchiveAliasMatches _) -> pure ()
        _ -> assertFailure "healthy immutable archive and alias did not produce a closed exact decision"
      let healthyFingerprint =
            case healthy of
              Just (ExactArchiveAliasMatches facts) -> lookup "source_fingerprint" (exactArchiveCompileMetadata facts)
              _ -> Nothing
      readIORef validationEvents >>= (@?= ["validation", "held-facts", "validation"])
      BS.readFile coldSnapshot >>= (@?= archiveBytesBeforeProof)
      getModificationTime coldSnapshot >>= (@?= archiveMtimeBeforeProof)

      -- The held-repair variant takes the cheap alias branch only as a route:
      -- a matching alias still receives one full validation and never invokes
      -- the repair callback.  The observer records full validations, not the
      -- metadata-only resolved_oid probe.
      matchingCalls <- newIORef (0 :: Int)
      matchingPublishes <- newIORef (0 :: Int)
      matching <- withExactArchiveAliasRepairForTest
        (modifyIORef' matchingCalls (+ 1))
        (pure ())
        coldSnapshot
        (gitOidText coldRevision)
        currentAlias
        (\_ -> modifyIORef' matchingPublishes (+ 1) >> pure (Right () :: Either () ()))
      case matching of
        Right (Just _) -> pure ()
        _ -> assertFailure "a matching alias did not produce an exact held decision"
      readIORef matchingCalls >>= (@?= 2)
      readIORef matchingPublishes >>= (@?= 0)

      -- A wrong resolved_oid cannot be accepted by the cheap probe.  The
      -- repair callback runs once while the source decision remains held, and
      -- the replacement alias is fully validated exactly once before facts
      -- can escape.
      let heldRepairAlias = repository </> ".adrai" </> "cache" </> "proof-held-repair.sqlite"
      copyFile currentAlias heldRepairAlias
      heldRepairConnection <- open heldRepairAlias
      execute_ heldRepairConnection "UPDATE meta SET value='wrong-alias-target' WHERE key='resolved_oid'"
      close heldRepairConnection
      repairCalls <- newIORef (0 :: Int)
      repairPublishes <- newIORef (0 :: Int)
      repaired <- withExactArchiveAliasRepairForTest
        (modifyIORef' repairCalls (+ 1))
        (pure ())
        coldSnapshot
        (gitOidText coldRevision)
        heldRepairAlias
        (\_ -> do
          modifyIORef' repairPublishes (+ 1)
          copyFile coldSnapshot heldRepairAlias
          pure (Right () :: Either () ()))
      case repaired of
        Right (Just _) -> pure ()
        _ -> assertFailure "a repaired alias was not fully validated against the held archive"
      readIORef repairCalls >>= (@?= 2)
      readIORef repairPublishes >>= (@?= 1)
      let resetWrongRepairAlias = do
            copyFile currentAlias heldRepairAlias
            connection <- open heldRepairAlias
            execute_ connection "UPDATE meta SET value='wrong-alias-target' WHERE key='resolved_oid'"
            close connection

      -- Typed publication failures remain caller errors rather than being
      -- collapsed into a cache miss.
      resetWrongRepairAlias
      typedRepair <- withExactArchiveAliasRepairForTest
        (pure ()) (pure ()) coldSnapshot (gitOidText coldRevision) heldRepairAlias
        (\_ -> pure (Left ("typed alias repair failure" :: Text)))
      typedRepair @?= Left "typed alias repair failure"

      -- Cleanup has r27 precedence: cleanup ThreadKilled dominates a typed
      -- initiating result, while both opaque hooks still run.
      resetWrongRepairAlias
      rollbackHooks <- newIORef (0 :: Int)
      closeHooks <- newIORef (0 :: Int)
      hookedTyped <- try
        (withExactArchiveAliasRepairWithCleanupForTest
          (exactArchiveCleanupPlanForTest
            (modifyIORef' rollbackHooks (+ 1) >> throwIO (userError "injected rollback failure"))
            (modifyIORef' closeHooks (+ 1) >> throwIO ThreadKilled))
          (pure ()) (pure ()) coldSnapshot (gitOidText coldRevision) heldRepairAlias
          (\_ -> pure (Left ("typed cleanup dominance" :: Text))))
      case hookedTyped of
        Left ThreadKilled -> pure ()
        _ -> assertFailure "cleanup ThreadKilled must dominate a typed initiating result"
      readIORef rollbackHooks >>= (@?= 1)
      readIORef closeHooks >>= (@?= 1)
      validateExactCacheTarget coldSnapshot (gitOidText coldRevision) >>= assertBool "real cleanup follows injected hook failures"

      -- A synchronous initiating exception follows the same precedence rule.
      resetWrongRepairAlias
      syncCancelled <- try
        (withExactArchiveAliasRepairWithCleanupForTest
          (exactArchiveCleanupPlanForTest (pure ()) (throwIO ThreadKilled))
          (pure ()) (pure ()) coldSnapshot (gitOidText coldRevision) heldRepairAlias
          (\_ -> throwIO (userError "synchronous initiating failure") :: IO (Either Text ())))
      case syncCancelled of
        Left ThreadKilled -> pure ()
        _ -> assertFailure "cleanup ThreadKilled must dominate a synchronous initiating exception"

      let successfulCleanupFault label hooks = do
            resetWrongRepairAlias
            result <- withExactArchiveAliasRepairWithCleanupForTest hooks (pure ()) (pure ()) coldSnapshot (gitOidText coldRevision) heldRepairAlias $ \_ -> do
              copyFile coldSnapshot heldRepairAlias
              pure (Right () :: Either Text ())
            result @?= Right Nothing
            validateExactCacheTarget coldSnapshot (gitOidText coldRevision) >>= assertBool (label <> " still performs real cleanup")
      successfulCleanupFault "rollback fault"
        (exactArchiveCleanupPlanForTest (throwIO (userError "rollback sync")) (pure ()))
      successfulCleanupFault "close fault"
        (exactArchiveCleanupPlanForTest (pure ()) (throwIO (userError "close sync")))
      resetWrongRepairAlias
      finalizerCancelled <- try
        (withExactArchiveAliasRepairWithCleanupForTest
          (exactArchiveCleanupPlanForTest (pure ()) (throwIO ThreadKilled))
          (pure ()) (pure ()) coldSnapshot (gitOidText coldRevision) heldRepairAlias $ \_ -> do
            copyFile coldSnapshot heldRepairAlias
            pure (Right () :: Either Text ()))
      case finalizerCancelled of
        Left ThreadKilled -> pure ()
        _ -> assertFailure "cleanup ThreadKilled was not rethrown"
      validateExactCacheTarget coldSnapshot (gitOidText coldRevision) >>= assertBool "finalizer cancellation real cleanup reopens archive"

      resetWrongRepairAlias
      actionCancelled <- try
        (withExactArchiveAliasRepairWithCleanupForTest
          (exactArchiveCleanupPlanForTest (throwIO (userError "rollback after action cancellation")) (pure ()))
          (pure ()) (pure ()) coldSnapshot (gitOidText coldRevision) heldRepairAlias
          (\_ -> throwIO ThreadKilled :: IO (Either Text ())))
      case actionCancelled of
        Left ThreadKilled -> pure ()
        _ -> assertFailure "initiating ThreadKilled was not preserved over cleanup fault"
      validateExactCacheTarget coldSnapshot (gitOidText coldRevision) >>= assertBool "action cancellation real cleanup reopens archive"

      -- A repair result is still untrusted.  Both a metadata/fingerprint
      -- corruption and a low-level FTS posting corruption fail cold after the
      -- one required alias validation, while the immutable source reopens.
      resetWrongRepairAlias
      fingerprintCalls <- newIORef (0 :: Int)
      fingerprintRepair <- withExactArchiveAliasRepairForTest
        (modifyIORef' fingerprintCalls (+ 1)) (pure ()) coldSnapshot (gitOidText coldRevision) heldRepairAlias
        (\_ -> do
          copyFile coldSnapshot heldRepairAlias
          connection <- open heldRepairAlias
          execute_ connection "UPDATE meta SET value='forged' WHERE key='source_fingerprint'"
          close connection
          pure (Right () :: Either () ()))
      fingerprintRepair @?= Right Nothing
      readIORef fingerprintCalls >>= (@?= 2)
      validateExactCacheTarget coldSnapshot (gitOidText coldRevision) >>= assertBool "the immutable archive reopens after fingerprint-repair rejection"

      resetWrongRepairAlias
      ftsCalls <- newIORef (0 :: Int)
      ftsRepair <- withExactArchiveAliasRepairForTest
        (modifyIORef' ftsCalls (+ 1)) (pure ()) coldSnapshot (gitOidText coldRevision) heldRepairAlias
        (\_ -> do
          copyFile coldSnapshot heldRepairAlias
          connection <- open heldRepairAlias
          rows <- query_ connection "SELECT id FROM fts_search_exact_data WHERE id > 10 ORDER BY id DESC LIMIT 1" :: IO [Only Int]
          posting <- case rows of
            [Only value] -> pure value
            _ -> assertFailure "fixture did not expose a removable FTS posting segment" >> fail "unreachable"
          execute connection "DELETE FROM fts_search_exact_data WHERE id=?" (Only posting)
          close connection
          pure (Right () :: Either () ()))
      ftsRepair @?= Right Nothing
      readIORef ftsCalls >>= (@?= 2)
      validateExactCacheTarget coldSnapshot (gitOidText coldRevision) >>= assertBool "the immutable archive reopens after FTS-repair rejection"

      resetWrongRepairAlias
      cancelledRepair <- try
        (withExactArchiveAliasRepairForTest
          (pure ()) (pure ()) coldSnapshot (gitOidText coldRevision) heldRepairAlias
          (\_ -> throwIO ThreadKilled :: IO (Either () ())))
      case cancelledRepair of
        Left ThreadKilled -> pure ()
        Left cancellation -> throwIO cancellation
        Right _ -> assertFailure "repair cancellation was not rethrown"
      validateExactCacheTarget coldSnapshot (gitOidText coldRevision) >>= assertBool "the immutable archive reopens after repair cancellation"
      loadCacheMetaForTest heldRepairAlias >>= \metadata -> assertBool "the alias is reopenable after repair cancellation" (maybe False (const True) metadata)

      -- The hook runs after immutable validation while its private transaction
      -- remains held.  POSIX may replace the pathname, but returned facts must
      -- still be from the held original snapshot.  Windows only accepts its
      -- expected sharing/permission failure, leaves the original intact, and
      -- must permit the same replacement after the operation returns.
      let archiveBackup = coldSnapshot <> ".held-proof-backup"
          archiveReplacement = coldSnapshot <> ".held-proof-replacement"
      copyFile coldSnapshot archiveBackup
      copyFile coldSnapshot archiveReplacement
      replacementConnection <- open archiveReplacement
      execute_ replacementConnection "UPDATE meta SET value='forged' WHERE key='source_fingerprint'"
      close replacementConnection
      replacementWhileHeld <- newIORef Nothing
      heldStatus <- readExactArchiveAliasStatusForTest (pure ()) (do
          replacement <- try (removeFile coldSnapshot >> renameFile archiveReplacement coldSnapshot) :: IO (Either IOException ())
          case replacement of
            Right () -> writeIORef replacementWhileHeld (Just True)
            Left exception
              | os == "mingw32" && isPermissionError exception -> writeIORef replacementWhileHeld (Just False)
              | otherwise -> throwIO exception)
          coldSnapshot (gitOidText coldRevision) currentAlias
      let fingerprint facts = lookup "source_fingerprint" (exactArchiveCompileMetadata facts)
      case heldStatus of
        Just (ExactArchiveAliasMatches facts) -> fingerprint facts @?= healthyFingerprint
        _ -> assertFailure "held immutable snapshot did not return a healthy closed decision"
      heldReplacement <- readIORef replacementWhileHeld
      case heldReplacement of
        Just True -> removeFile coldSnapshot >> renameFile archiveBackup coldSnapshot
        Just False -> do
          intact <- validateExactCacheTarget coldSnapshot (gitOidText coldRevision)
          assertBool "a denied Windows replacement leaves the original path intact" intact
          removeFile coldSnapshot
          renameFile archiveReplacement coldSnapshot
          removeFile coldSnapshot
          renameFile archiveBackup coldSnapshot
        Nothing -> assertFailure "post-validation replacement hook did not run"

      -- The repair continuation reads the mutable source pathname only to
      -- publish.  If that pathname is replaced after the held decision, the
      -- published corrupt copy must fail the comparison against held facts.
      let repairSourceBackup = coldSnapshot <> ".repair-source-backup"
          repairSourceReplacement = coldSnapshot <> ".repair-source-replacement"
      copyFile coldSnapshot repairSourceBackup
      copyFile coldSnapshot repairSourceReplacement
      repairSourceReplacementConnection <- open repairSourceReplacement
      execute_ repairSourceReplacementConnection "UPDATE meta SET value='forged' WHERE key='source_fingerprint'"
      close repairSourceReplacementConnection
      resetWrongRepairAlias
      repairReplacementAttempt <- newIORef Nothing
      replacementResult <- withExactArchiveAliasRepairForTest
        (pure ())
        (do
          replacement <- try (removeFile coldSnapshot >> renameFile repairSourceReplacement coldSnapshot) :: IO (Either IOException ())
          case replacement of
            Right () -> writeIORef repairReplacementAttempt (Just True)
            Left exception
              | os == "mingw32" && isPermissionError exception -> writeIORef repairReplacementAttempt (Just False)
              | otherwise -> throwIO exception)
        coldSnapshot
        (gitOidText coldRevision)
        heldRepairAlias
        (\_ -> copyFile coldSnapshot heldRepairAlias >> pure (Right () :: Either () ()))
      attemptedReplacement <- readIORef repairReplacementAttempt
      case attemptedReplacement of
        Just True -> replacementResult @?= Right Nothing
        Just False -> do
          case replacementResult of
            Right (Just _) -> pure ()
            _ -> assertFailure "a denied Windows replacement must retain the held valid source"
          intact <- validateExactCacheTarget coldSnapshot (gitOidText coldRevision)
          assertBool "a denied Windows source replacement leaves the archive intact" intact
          removeFile coldSnapshot
          renameFile repairSourceReplacement coldSnapshot
        Nothing -> assertFailure "held repair replacement hook did not run"
      case attemptedReplacement of
        Just True -> removeFile coldSnapshot >> renameFile repairSourceBackup coldSnapshot
        Just False -> do
          removeFile coldSnapshot
          renameFile repairSourceBackup coldSnapshot
        Nothing -> pure ()

      -- Cancellation inside the one-shot operation must release its transaction
      -- and handle before the caller regains control.
      cancelled <- try (readExactArchiveAliasStatusForTest (pure ()) (throwIO ThreadKilled) coldSnapshot (gitOidText coldRevision) currentAlias) :: IO (Either AsyncException (Maybe ExactArchiveAliasStatus))
      case cancelled of
        Left ThreadKilled -> pure ()
        Left cancellation -> throwIO cancellation
        Right _ -> assertFailure "exact archive cancellation was not rethrown"
      reopenedAfterCancellation <- validateExactCacheTarget coldSnapshot (gitOidText coldRevision)
      assertBool "exact archive is reopenable after async cancellation cleanup" reopenedAfterCancellation

      -- A divergent alias is never saved by the immutable proof: its own full
      -- target validation and complete metadata comparison must reject it.
      let tamperedAlias = repository </> ".adrai" </> "cache" </> "proof-tampered-alias.sqlite"
      copyFile currentAlias tamperedAlias
      tamperedAliasConnection <- open tamperedAlias
      execute_ tamperedAliasConnection "UPDATE meta SET value='63' WHERE key='history_commits_scanned'"
      close tamperedAliasConnection
      aliasCalls <- newIORef (0 :: Int)
      aliasStatus <- readExactArchiveAliasStatusForTest (modifyIORef' aliasCalls (+ 1)) (pure ()) coldSnapshot (gitOidText coldRevision) tamperedAlias
      case aliasStatus of
        Just (ExactArchiveAliasNeedsRepair _) -> pure ()
        _ -> assertFailure "metadata-divergent alias was not returned as a closed repair decision"
      readIORef aliasCalls >>= (@?= 2)

      -- The recovery sequence deliberately uses a new source proof and a new
      -- alias transaction after publication.  The original selection proof is
      -- not accepted as authority for either step.
      let repairedAlias = repository </> ".adrai" </> "cache" </> "proof-repaired-alias.sqlite"
      copyFile coldSnapshot repairedAlias
      recoveryCalls <- newIORef (0 :: Int)
      freshRecovery <- readExactArchiveAliasStatusForTest (modifyIORef' recoveryCalls (+ 1)) (pure ()) coldSnapshot (gitOidText coldRevision) repairedAlias
      case freshRecovery of
        Just (ExactArchiveAliasMatches _) -> pure ()
        _ -> assertFailure "a freshly published alias was not fully revalidated"
      readIORef recoveryCalls >>= (@?= 2)

      -- Alias repair must reacquire source authority rather than reuse a
      -- closed proof.  This source-only mutation models replacement after
      -- selection: the first proof exists, while the required fresh proof
      -- fails after corruption.
      let replacedArchive = repository </> ".adrai" </> "cache" </> "proof-replaced-archive.sqlite"
      copyFile coldSnapshot replacedArchive
      staleSource <- validateExactCacheTarget replacedArchive (gitOidText coldRevision)
      assertBool "healthy replacement fixture validates before its source mutation" staleSource
      replacedArchiveConnection <- open replacedArchive
      execute_ replacedArchiveConnection "UPDATE meta SET value='forged' WHERE key='source_fingerprint'"
      close replacedArchiveConnection
      isNothing <$> readExactArchiveAliasStatusForTest (pure ()) (pure ()) replacedArchive (gitOidText coldRevision) currentAlias
        >>= assertBool "a recovery source mutation invalidates the required fresh authority"

      -- current_connection is a relation-derived projection.  Its axis and
      -- connection-ID ordinal are structural commitments even though the
      -- materialization digest only needs the canonically ordered IDs.
      assertRejectedArchiveMutation repository coldSnapshot "axis-corruption.sqlite"
        "a current-connection axis corruption is not publishable"
        (\connection -> execute_ connection "UPDATE current_connection SET axis='domain' WHERE axis='scope'")
      assertRejectedArchiveMutation repository coldSnapshot "ordinal-corruption.sqlite"
        "a current-connection ordinal corruption is not publishable"
        (\connection -> execute_ connection "UPDATE current_connection SET ordinal=63 WHERE axis='scope'")
      assertRejectedArchiveMutation repository coldSnapshot "requested-revision-corruption.sqlite"
        "a requested-revision-mismatched archive is not publishable"
        (\connection -> execute_ connection "UPDATE meta SET value='not-the-resolved-oid' WHERE key='requested_revision'")
      assertRejectedArchiveMutation repository coldSnapshot "duplicate-fts-row.sqlite"
        "a duplicate FTS row archive is not publishable"
        (\connection -> execute_ connection "INSERT INTO fts_search_exact SELECT * FROM fts_search_exact LIMIT 1")
      let restoredColdSnapshot = coldSnapshot

      -- A readable SQLite file can have a healthy page-level integrity check
      -- while one FTS projection has been logically removed.  The full
      -- canonical reader contract must reject it before candidate discovery.
      let corruptLogicalSnapshot = repository </> ".adrai" </> "cache" </> "readable-logical-corruption.sqlite"
      copyFile restoredColdSnapshot corruptLogicalSnapshot
      corruptConnection <- open corruptLogicalSnapshot
      execute_ corruptConnection "DELETE FROM fts_search_exact"
      close corruptConnection
      corruptedSnapshot <- validateCachePublicationContract corruptLogicalSnapshot
      assertBool "a readable database with an incomplete FTS materialization is not publishable" (not corruptedSnapshot)

      -- Both digest fields are commitments to canonical rows, rather than
      -- syntactically-valid opaque tokens.  These values decode to 32 bytes,
      -- so this proves the validator recomputes them instead of merely checking
      -- the base64url shape.
      let forgedMetadataSnapshot = repository </> ".adrai" </> "cache" </> "forged-cache-metadata.sqlite"
      copyFile restoredColdSnapshot forgedMetadataSnapshot
      forgedMetadataConnection <- open forgedMetadataSnapshot
      execute_ forgedMetadataConnection "UPDATE meta SET value='AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA' WHERE key='source_fingerprint'"
      close forgedMetadataConnection
      forgedMetadata <- validateCachePublicationContract forgedMetadataSnapshot
      assertBool "a syntactically valid but forged source fingerprint is not publishable" (not forgedMetadata)

      let forgedMaterializationSnapshot = repository </> ".adrai" </> "cache" </> "forged-materialization-metadata.sqlite"
      copyFile restoredColdSnapshot forgedMaterializationSnapshot
      forgedMaterializationConnection <- open forgedMaterializationSnapshot
      execute_ forgedMaterializationConnection "UPDATE meta SET value='AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA' WHERE key='materialization_fingerprint'"
      close forgedMaterializationConnection
      forgedMaterialization <- validateCachePublicationContract forgedMaterializationSnapshot
      assertBool "a syntactically valid but forged materialization fingerprint is not publishable" (not forgedMaterialization)

      let forgedIdentifierSourceSnapshot = repository </> ".adrai" </> "cache" </> "forged-identifier-source.sqlite"
      copyFile restoredColdSnapshot forgedIdentifierSourceSnapshot
      forgedIdentifierSourceConnection <- open forgedIdentifierSourceSnapshot
      execute_ forgedIdentifierSourceConnection "UPDATE search_document SET identifier_source='forged raw identifier source'"
      close forgedIdentifierSourceConnection
      forgedIdentifierSource <- validateCachePublicationContract forgedIdentifierSourceSnapshot
      assertBool "a source-only identifier tamper is not publishable" (not forgedIdentifierSource)

      -- A syntactically valid candidate ID is still invalid when it is not a
      -- current decision-axis head for the ADR. Keep every dependent search
      -- projection and the materialization fingerprint internally consistent
      -- so this reaches the candidate/head canonicality check specifically.
      let forgedCandidateSnapshot = repository </> ".adrai" </> "cache" </> "forged-search-candidate.sqlite"
          forgedCandidate = "R00000000000000000000000000" :: Text
      copyFile restoredColdSnapshot forgedCandidateSnapshot
      forgedCandidateConnection <- open forgedCandidateSnapshot
      currentCandidates <- query_ forgedCandidateConnection "SELECT candidate_record_id FROM search_document" :: IO [Only Text]
      assertBool "forged candidate fixture must differ from the current decision head" (Only forgedCandidate `notElem` currentCandidates)
      execute forgedCandidateConnection "UPDATE search_document SET candidate_record_id=?" (Only forgedCandidate)
      execute forgedCandidateConnection "UPDATE search_section SET candidate_record_id=?" (Only forgedCandidate)
      execute forgedCandidateConnection "UPDATE fts_search_exact SET candidate_record_id=?" (Only forgedCandidate)
      execute forgedCandidateConnection "UPDATE fts_search_stemmed SET candidate_record_id=?" (Only forgedCandidate)
      execute forgedCandidateConnection "UPDATE fts_search_identifier SET candidate_record_id=?" (Only forgedCandidate)
      execute forgedCandidateConnection "UPDATE fts_passage_exact SET candidate_record_id=?" (Only forgedCandidate)
      execute forgedCandidateConnection "UPDATE fts_passage_stemmed SET candidate_record_id=?" (Only forgedCandidate)
      execute forgedCandidateConnection "UPDATE fts_passage_identifier SET candidate_record_id=?" (Only forgedCandidate)
      close forgedCandidateConnection
      refreshCacheMaterializationFingerprint forgedCandidateSnapshot
      forgedCandidateEvidence <- cachePublicationMaterializationFingerprintEvidence forgedCandidateSnapshot
      case forgedCandidateEvidence of
        Just (writtenFingerprint, recomputedFingerprint, Nothing) -> writtenFingerprint @?= recomputedFingerprint
        _ -> assertFailure "forged candidate fixture did not retain canonical materialization fingerprint evidence"
      forgedCandidateArchive <- validateCachePublicationContract forgedCandidateSnapshot
      assertBool "a search document for an inactive decision candidate is not publishable" (not forgedCandidateArchive)

      -- The operation wire format is part of the fingerprint proof.  Merely
      -- changing its whitespace does not change the decoded anchor values,
      -- so the validator must also require the exact canonical JSON emitted
      -- by the SQLite writer.
      let nonCanonicalAnchorsSnapshot = repository </> ".adrai" </> "cache" </> "noncanonical-operation-anchors.sqlite"
      copyFile restoredColdSnapshot nonCanonicalAnchorsSnapshot
      nonCanonicalAnchorsConnection <- open nonCanonicalAnchorsSnapshot
      execute_ nonCanonicalAnchorsConnection "UPDATE operation SET line_anchors=line_anchors || ' '"
      close nonCanonicalAnchorsConnection
      nonCanonicalAnchors <- validateCachePublicationContract nonCanonicalAnchorsSnapshot
      assertBool "a non-canonical operation anchor encoding is not publishable" (not nonCanonicalAnchors)

      let forgedFtsTextSnapshot = repository </> ".adrai" </> "cache" </> "forged-fts-text.sqlite"
      copyFile restoredColdSnapshot forgedFtsTextSnapshot
      forgedFtsTextConnection <- open forgedFtsTextSnapshot
      execute_ forgedFtsTextConnection "UPDATE fts_search_exact SET title='forged full-text projection'"
      close forgedFtsTextConnection
      forgedFtsText <- validateCachePublicationContract forgedFtsTextSnapshot
      assertBool "an FTS row with a valid key but forged indexed text is not publishable" (not forgedFtsText)

      -- Passage rowids are the durable bridge from an FTS hit back to its
      -- canonical section.  A content match with a shifted rowid must fail
      -- before query execution can trust the virtual table.
      let forgedPassageRowidSnapshot = repository </> ".adrai" </> "cache" </> "forged-fts-passage-rowid.sqlite"
      copyFile restoredColdSnapshot forgedPassageRowidSnapshot
      forgedPassageRowidConnection <- open forgedPassageRowidSnapshot
      execute_ forgedPassageRowidConnection "UPDATE fts_passage_exact SET rowid=rowid+1000000 WHERE rowid=(SELECT min(rowid) FROM fts_passage_exact)"
      close forgedPassageRowidConnection
      forgedPassageRowid <- validateCachePublicationContract forgedPassageRowidSnapshot
      assertBool "an FTS passage with a noncanonical rowid is not publishable" (not forgedPassageRowid)

      let forgedSemanticSnapshot = repository </> ".adrai" </> "cache" </> "forged-semantic-state.sqlite"
      copyFile restoredColdSnapshot forgedSemanticSnapshot
      forgedSemanticConnection <- open forgedSemanticSnapshot
      execute_ forgedSemanticConnection "UPDATE meta SET value='invalid' WHERE key='semantic_state'"
      execute_ forgedSemanticConnection "UPDATE meta SET value='maybe' WHERE key='history_complete'"
      close forgedSemanticConnection
      forgedSemantic <- validateCachePublicationContract forgedSemanticSnapshot
      assertBool "semantic state and history completeness must be derived canonical booleans" (not forgedSemantic)

candidateLifecycleContract :: CacheSeed FocusedSeed -> IO ()
candidateLifecycleContract seed =
  withPrivateCacheSeed seed $ \focused repository ->
    lazyCandidateValidationContract focused repository

lazyCandidateValidationContract :: FocusedSeed -> FilePath -> IO ()
lazyCandidateValidationContract focused repository = do
    let ancestor = focusedRevision focused
    archive <- assertSnapshot repository ancestor
    git repository ["switch", "-c", "release/unrelated"]
    _ <- writeAndCommit repository "unrelated release candidate" [("release-only.txt", "unrelated\n")]
    unrelated <- repositoryHead repository
    _ <- compile "unrelated release candidate compile" repository
    unrelatedArchive <- assertSnapshot repository unrelated
    git repository ["switch", "main"]
    _ <- writeAndCommit repository "unmanaged candidate target" [("seed.txt", "candidate target\n")]
    target <- repositoryHead repository
    _ <- compile "candidate target compile" repository
    targetArchive <- assertSnapshot repository target
    let candidateDir = repository </> ".adrai" </> "candidate-selection"
        firstCandidate = candidateDir </> "a-first.sqlite"
        secondCandidate = candidateDir </> "b-second.sqlite"
        faultedCandidate = minimum [firstCandidate, secondCandidate]
        fallbackCandidate = maximum [firstCandidate, secondCandidate]
        alias = repository </> ".adrai" </> "index.sqlite"
        schema = "adrai-cache/3"
        targetText = gitOidText target
    createDirectoryIfMissing True candidateDir
    discovered <- discoverRepository systemGit repository
    gitRepository <- either (\problem -> assertFailure (show problem) >> fail "unreachable") pure discovered
    let noExact = pure (Right Nothing)
        exactFacts = do
          status <- readExactArchiveAliasStatusForTest (pure ()) (pure ()) targetArchive targetText alias
          pure (Right (exactArchiveAliasCompileFacts <$> status))
        runCascade
          :: CacheSelectionKind
          -> Integer
          -> ReuseDiscoveryPlan
          -> IO (Either String (Maybe ExactArchiveCompileFacts))
          -> IO Integer
          -> FilePath
          -> (forall scope. CacheSelectionCascadeDecision scope -> IO (Either String value))
          -> IO (Either String value, CacheSelectionMetrics)
        runCascade expectedKind expectedSelected plan exactAction exactBytes directory consume = do
          events <- newIORef ([] :: [Text])
          metricsRef <- newIORef Nothing
          let phase :: forall a. IO a -> IO a
              phase operation = do
                modifyIORef' events (<> ["enter"])
                value <- operation
                modifyIORef' events (<> ["exit"])
                pure value
              observe metrics = modifyIORef' events (<> ["metrics"]) >> writeIORef metricsRef (Just metrics)
              checkedConsume decision = do
                current <- readIORef events
                current @?= ["enter", "metrics", "exit"]
                modifyIORef' events (<> ["consumer"])
                consume decision
          result <- withCacheSelectionCascadeForTest plan phase observe exactAction exactBytes gitRepository directory schema targetText checkedConsume
          readIORef events >>= (@?= ["enter", "metrics", "exit", "consumer"])
          recorded <- readIORef metricsRef
          case recorded of
            Nothing -> assertFailure "cascade did not record metrics"
            Just metrics -> do
              cacheSelectionKind metrics @?= expectedKind
              cacheSelectedCount metrics @?= expectedSelected
          metrics <- case recorded of
            Nothing -> assertFailure "cascade did not record metrics" >> fail "unreachable"
            Just value -> pure value
          pure (result, metrics)
        runLifecycle
          :: ReuseDiscoveryPlan
          -> CacheSelectionLifecyclePlan
          -> (forall scope. CacheSelectionCascadeDecision scope -> IO (Either String value))
          -> IO (Either String value, CacheSelectionMetrics)
        runLifecycle discoveryPlan lifecyclePlan consume = do
          metricsRef <- newIORef Nothing
          result <- withCacheSelectionCascadeWithPlansForTest discoveryPlan lifecyclePlan id (writeIORef metricsRef . Just) noExact (pure 0) gitRepository candidateDir schema targetText consume
          metrics <- readIORef metricsRef >>= \case
            Nothing -> assertFailure "lifecycle cascade did not record metrics" >> fail "unreachable"
            Just value -> pure value
          pure (result, metrics)
        noLifecycle = cacheSelectionLifecyclePlanForTest (pure ())
        noDiscovery = reuseDiscoveryPlanForTest (pure ())
        assertNoPrivateResidue label =
          listDirectory candidateDir >>= \names ->
            assertBool (label <> " leaves no private reuse residue") (not (any (isInfixOf "adrai-reuse-private-") names))
        freshReuse label = do
          (result, _) <- runLifecycle noDiscovery noLifecycle (\case CacheSelectionReuse _ _ _ -> pure (Right ()); _ -> pure (Left (label <> " did not retry as reuse")))
          result @?= Right ()
          assertNoPrivateResidue label
        assertReuseMetrics label considered attempts validationBytes selectedBytes metrics = do
          cacheSelectionKind metrics @?= CacheSelectionReuseKind
          cacheCandidatesConsidered metrics @?= considered
          cacheFullValidationAttempts metrics @?= attempts
          cacheFullValidationBytes metrics @?= validationBytes
          cacheSelectedCount metrics @?= 1
          cacheSelectedBytes metrics @?= selectedBytes
          case cacheSelectedReuse metrics of
            Nothing -> assertFailure (label <> " did not retain selected reuse facts")
            Just facts -> reuseFactPrivateBytes facts @?= selectedBytes
        assertNoSidecars label paths =
          traverse (\path -> traverse (doesFileExist . (path <>)) ["-journal", "-wal", "-shm"]) paths >>= \present ->
            assertBool (label <> " leaves no public SQLite sidecars") (not (or (concat present)))
        laterAcquisitionEvents event =
          drop 1 (dropWhile (/= event)
            [ BeforePrivateSentinelClose,
              BeforePrivateSentinelUnlink,
              BeforePrivateDirectoryCreate,
              BeforePrivatePublicSourceCopy,
              AfterPrivateCopy,
              BeforePrivateValidation
            ])
        assertNoLaterAcquisition label event observed =
          assertBool
            (label <> " must not advance to a later acquisition/copy boundary")
            (all (`notElem` map fst observed) (laterAcquisitionEvents event))
    -- The lexicographically first candidate is unrelated to the target while
    -- the second is its real Git ancestor. Selection must rank topology before
    -- opening a candidate and choose the ancestor despite discovery order.
    copyFile unrelatedArchive firstCandidate
    copyFile archive secondCandidate
    ancestorBytes <- getFileSize secondCandidate
    rankedCandidates <- newIORef (0 :: Int)
    rankingPlan <- reuseDiscoveryPlanWithEvents $ \event _ ->
      when (event == BeforeRank) (modifyIORef' rankedCandidates (+ 1))
    (rankedResult, rankedMetrics) <- runCascade CacheSelectionReuseKind 1 rankingPlan noExact (pure 0) candidateDir
      (\case
        CacheSelectionReuse _ facts _ -> do
          reuseFactSourceRevision facts @?= gitOidText ancestor
          assertBool "the selected ancestor must outrank an unrelated release" (show (reuseFactRank facts) /= "Unrelated")
          pure (Right ())
        _ -> pure (Left "expected ranked ancestor reuse"))
    rankedResult @?= Right ()
    readIORef rankedCandidates >>= (@?= 2)
    cacheCandidatesConsidered rankedMetrics @?= 1
    cacheFullValidationAttempts rankedMetrics @?= 1
    cacheSelectedBytes rankedMetrics @?= ancestorBytes
    removeFile firstCandidate
    removeFile secondCandidate

    -- Exact uses closed facts from the exact archive/status decision and never
    -- discovers a sibling candidate.
    exactDiscoveryEvents <- newIORef (0 :: Int)
    exactPlan <- reuseDiscoveryPlanWithEvents $ \_ _ -> modifyIORef' exactDiscoveryEvents (+ 1)
    targetArchiveBytes <- getFileSize targetArchive
    (exactResult, exactMetrics) <- runCascade CacheSelectionExactKind 1 exactPlan exactFacts (getFileSize targetArchive) candidateDir
      (\case CacheSelectionExact _ -> pure (Right ()); _ -> pure (Left "expected exact"))
    exactResult @?= Right ()
    readIORef exactDiscoveryEvents >>= (@?= 0)
    cacheCandidatesConsidered exactMetrics @?= 0
    cacheFullValidationAttempts exactMetrics @?= 0
    cacheFullValidationBytes exactMetrics @?= 0
    cacheSelectedCount exactMetrics @?= 1
    cacheSelectedBytes exactMetrics @?= targetArchiveBytes
    cacheSelectedReuse exactMetrics @?= Nothing

    -- Empty directory is a single cold decision with no selected reuse.
    emptyDir <- pure (candidateDir </> "empty")
    createDirectory emptyDir
    (coldResult, coldMetrics) <- runCascade CacheSelectionColdKind 0 (reuseDiscoveryPlanForTest (pure ())) noExact (pure 0) emptyDir
      (\case CacheSelectionCold -> pure (Right ()); _ -> pure (Left "expected cold"))
    coldResult @?= Right ()
    cacheCandidatesConsidered coldMetrics @?= 0
    cacheFullValidationAttempts coldMetrics @?= 0
    cacheFullValidationBytes coldMetrics @?= 0
    cacheSelectedCount coldMetrics @?= 0
    cacheSelectedBytes coldMetrics @?= 0

    -- A single valid candidate yields scoped facts only; no raw path escapes.
    copyFile archive firstCandidate
    firstBytes <- getFileSize firstCandidate
    (validResult, validMetrics) <- runCascade CacheSelectionReuseKind 1 (reuseDiscoveryPlanForTest (pure ())) noExact (pure 0) candidateDir
      (\case
        CacheSelectionReuse _ facts _ -> do
          reuseFactSourceRevision facts @?= gitOidText ancestor
          reuseFactPrivateBytes facts @?= firstBytes
          assertBool "accepted key is closed evidence" (not (Text.null (reuseFactCacheKey facts)))
          assertBool "accepted rank is not unrelated" (show (reuseFactRank facts) /= "Unrelated")
          pure (Right ())
        _ -> pure (Left "expected reuse"))
    validResult @?= Right ()
    cacheCandidatesConsidered validMetrics @?= 1
    cacheFullValidationAttempts validMetrics @?= 1
    cacheFullValidationBytes validMetrics @?= firstBytes
    cacheSelectedCount validMetrics @?= 1
    cacheSelectedBytes validMetrics @?= firstBytes
    case cacheSelectedReuse validMetrics of
      Nothing -> assertFailure "single valid candidate did not retain selected reuse facts"
      Just selectedFacts -> reuseFactPrivateBytes selectedFacts @?= firstBytes

    -- Cheap metadata can rank first yet fail full validation; the second
    -- candidate is the only fallback and both attempted byte counts remain in
    -- phase-local metrics.
    copyFile archive firstCandidate
    copyFile archive secondCandidate
    invalid <- open firstCandidate
    execute_ invalid "UPDATE meta SET value='forged-source-fingerprint' WHERE key='source_fingerprint'"
    close invalid
    secondBytes <- getFileSize secondCandidate
    (fallbackResult, fallbackMetrics) <- runCascade CacheSelectionReuseKind 1 (reuseDiscoveryPlanForTest (pure ())) noExact (pure 0) candidateDir
      (\case CacheSelectionReuse _ _ _ -> pure (Right ()); _ -> pure (Left "expected fallback reuse"))
    fallbackResult @?= Right ()
    cacheCandidatesConsidered fallbackMetrics @?= 2
    cacheFullValidationAttempts fallbackMetrics @?= 2
    cacheFullValidationBytes fallbackMetrics @?= firstBytes + secondBytes
    cacheSelectedBytes fallbackMetrics @?= secondBytes

    -- Deep FTS posting corruption remains cheap-metadata eligible, so the
    -- first private copy must fail full validation and the second healthy copy
    -- is the one scoped reuse result.  This cascade has no seed or publication
    -- action: it proves selection alone cannot accept a damaged projection.
    copyFile archive firstCandidate
    copyFile archive secondCandidate
    ftsCorrupt <- open firstCandidate
    postingRows <- query_ ftsCorrupt "SELECT id FROM fts_search_exact_data WHERE id > 10 ORDER BY id DESC LIMIT 1" :: IO [Only Int]
    posting <- case postingRows of
      [Only value] -> pure value
      _ -> assertFailure "fixture did not expose a removable FTS posting segment" >> fail "unreachable"
    execute ftsCorrupt "DELETE FROM fts_search_exact_data WHERE id=?" (Only posting)
    close ftsCorrupt
    ftsFirstBytes <- getFileSize firstCandidate
    ftsSecondBytes <- getFileSize secondCandidate
    ftsCallbacks <- newIORef (0 :: Int)
    (ftsFallbackResult, ftsFallbackMetrics) <- runCascade CacheSelectionReuseKind 1 (reuseDiscoveryPlanForTest (pure ())) noExact (pure 0) candidateDir
      (\case
        CacheSelectionReuse _ _ _ -> modifyIORef' ftsCallbacks (+ 1) >> pure (Right ())
        _ -> pure (Left "expected FTS fallback reuse"))
    ftsFallbackResult @?= Right ()
    readIORef ftsCallbacks >>= (@?= 1)
    cacheCandidatesConsidered ftsFallbackMetrics @?= 2
    cacheFullValidationAttempts ftsFallbackMetrics @?= 2
    cacheFullValidationBytes ftsFallbackMetrics @?= ftsFirstBytes + ftsSecondBytes
    cacheSelectedBytes ftsFallbackMetrics @?= ftsSecondBytes

    -- A malformed OID is discarded before rank: only its valid sibling emits
    -- BeforeRank, so no malformed source can cause Git ranking I/O.
    copyFile archive firstCandidate
    malformed <- open firstCandidate
    execute_ malformed "UPDATE meta SET value='not-a-git-oid' WHERE key='resolved_oid'"
    close malformed
    rankEvents <- newIORef ([] :: [ReuseDiscoveryEvent])
    malformedPlan <- reuseDiscoveryPlanWithEvents $ \event _ ->
      when (event == BeforeRank) (modifyIORef' rankEvents (event :))
    (malformedResult, _) <- runCascade CacheSelectionReuseKind 1 malformedPlan noExact (pure 0) candidateDir
      (\case CacheSelectionReuse _ _ _ -> pure (Right ()); _ -> pure (Left "expected valid sibling reuse"))
    malformedResult @?= Right ()
    reverse <$> readIORef rankEvents >>= (@?= [BeforeRank])

    -- Discovery races are candidate-local.  The first synchronous BeforeOpen
    -- disappearance leaves its valid sibling as the only selected reuse.
    copyFile archive firstCandidate
    copyFile archive secondCandidate
    lifecycleFallbackBytes <- getFileSize fallbackCandidate
    beforeOpenFired <- newIORef (0 :: Int)
    beforeOpenPlan <- reuseDiscoveryPlanWithEvents $ \event ordinal ->
      when (event == BeforeOpen && ordinal == 1) $ do
        removeFile faultedCandidate
        modifyIORef' beforeOpenFired (+ 1)
    (beforeOpenResult, beforeOpenMetrics) <- runLifecycle beforeOpenPlan noLifecycle
      (\case
        CacheSelectionReuse _ facts _ -> do
          reuseFactSourceRevision facts @?= gitOidText ancestor
          pure (Right ())
        _ -> pure (Left "BeforeOpen did not fall through"))
    beforeOpenResult @?= Right ()
    readIORef beforeOpenFired >>= (@?= 1)
    doesFileExist faultedCandidate >>= (@?= False)
    assertReuseMetrics "BeforeOpen disappearance" 1 1 lifecycleFallbackBytes lifecycleFallbackBytes beforeOpenMetrics
    assertNoPrivateResidue "BeforeOpen disappearance"

    -- A synchronous AfterOpen failure is normalized only after the hidden
    -- bracket closes its handle; the first archive remains reopenable and the
    -- second candidate is selected.
    copyFile archive firstCandidate
    copyFile archive secondCandidate
    afterOpenFired <- newIORef (0 :: Int)
    afterOpenFailure <- reuseDiscoveryPlanWithEvents $ \event ordinal ->
      when (event == AfterOpen && ordinal == 1) $ do
        modifyIORef' afterOpenFired (+ 1)
        throwIO (userError "after-open test failure")
    (afterOpenResult, afterOpenMetrics) <- runLifecycle afterOpenFailure noLifecycle
      (\case
        CacheSelectionReuse _ facts _ -> do
          reuseFactSourceRevision facts @?= gitOidText ancestor
          pure (Right ())
        _ -> pure (Left "AfterOpen did not fall through"))
    afterOpenResult @?= Right ()
    readIORef afterOpenFired >>= (@?= 1)
    validateCachePublicationContract faultedCandidate >>= assertBool "AfterOpen failure closes the faulted candidate connection"
    validateCachePublicationContract fallbackCandidate >>= assertBool "AfterOpen failure leaves the fallback candidate reopenable"
    assertNoSidecars "AfterOpen failure" [faultedCandidate, fallbackCandidate]
    assertReuseMetrics "AfterOpen failure" 1 1 lifecycleFallbackBytes lifecycleFallbackBytes afterOpenMetrics
    assertNoPrivateResidue "AfterOpen failure"

    -- Replacing a source after cheap metadata but before private validation
    -- breaks the retained complete map.  The callback observes only the
    -- healthy fallback, with both private attempts accounted in the phase.
    copyFile archive firstCandidate
    copyFile archive secondCandidate
    replacementFired <- newIORef (0 :: Int)
    replacementPlan <- reuseDiscoveryPlanWithEvents $ \event ordinal ->
      when (event == AfterMetadata && ordinal == 1) $ do
        copyFile targetArchive faultedCandidate
        modifyIORef' replacementFired (+ 1)
    callbackSources <- newIORef ([] :: [Text])
    (replacementResult, replacementMetrics) <- runLifecycle replacementPlan noLifecycle
      (\case
        CacheSelectionReuse _ facts _ -> modifyIORef' callbackSources (reuseFactSourceRevision facts :) >> pure (Right ())
        _ -> pure (Left "AfterMetadata replacement did not fall through"))
    replacementResult @?= Right ()
    readIORef replacementFired >>= (@?= 1)
    reverse <$> readIORef callbackSources >>= (@?= [gitOidText ancestor])
    assertReuseMetrics "AfterMetadata replacement" 2 2 (targetArchiveBytes + lifecycleFallbackBytes) lifecycleFallbackBytes replacementMetrics
    assertNoPrivateResidue "AfterMetadata replacement"

    -- A separate AfterMetadata race removes the same lexicographically first
    -- fixture before mtime/rank.  It is never private-copied, so only the
    -- surviving fallback contributes validation bytes.
    copyFile archive firstCandidate
    copyFile archive secondCandidate
    afterMetadataDisappearanceFired <- newIORef (0 :: Int)
    afterMetadataDisappearance <- reuseDiscoveryPlanWithEvents $ \event ordinal ->
      when (event == AfterMetadata && ordinal == 1) $ do
        removeFile faultedCandidate
        modifyIORef' afterMetadataDisappearanceFired (+ 1)
    (afterMetadataDisappearanceResult, afterMetadataDisappearanceMetrics) <- runLifecycle afterMetadataDisappearance noLifecycle
      (\case CacheSelectionReuse _ _ _ -> pure (Right ()); _ -> pure (Left "AfterMetadata disappearance did not fall through"))
    afterMetadataDisappearanceResult @?= Right ()
    readIORef afterMetadataDisappearanceFired >>= (@?= 1)
    doesFileExist faultedCandidate >>= (@?= False)
    assertReuseMetrics "AfterMetadata disappearance" 1 1 lifecycleFallbackBytes lifecycleFallbackBytes afterMetadataDisappearanceMetrics
    assertNoPrivateResidue "AfterMetadata disappearance"

    -- The source can disappear at the private-copy boundary without turning a
    -- synchronous race into a task failure; the next candidate remains valid.
    copyFile archive firstCandidate
    copyFile archive secondCandidate
    faultedMtime <- getModificationTime faultedCandidate
    setModificationTime firstCandidate faultedMtime
    setModificationTime secondCandidate faultedMtime
    beforeCopyFired <- newIORef (0 :: Int)
    beforeCopyPlan <- cacheSelectionLifecyclePlanWithEvents $ \event ordinal ->
      when (event == BeforePrivatePublicSourceCopy && ordinal == 1) $ do
        removeFile faultedCandidate
        modifyIORef' beforeCopyFired (+ 1)
    (beforeCopyResult, beforeCopyMetrics) <- runLifecycle noDiscovery beforeCopyPlan
      (\case
        CacheSelectionReuse _ facts _ -> do
          reuseFactSourceRevision facts @?= gitOidText ancestor
          pure (Right ())
        _ -> pure (Left "BeforePrivatePublicSourceCopy did not fall through"))
    beforeCopyResult @?= Right ()
    readIORef beforeCopyFired >>= (@?= 1)
    doesFileExist faultedCandidate >>= (@?= False)
    assertReuseMetrics "BeforePrivatePublicSourceCopy disappearance" 2 1 lifecycleFallbackBytes lifecycleFallbackBytes beforeCopyMetrics
    assertNoPrivateResidue "BeforePrivatePublicSourceCopy disappearance"

    -- Sentinel reservation is a generative private-resource transition, not a
    -- candidate-local failure.  Each acquisition boundary aborts selection on
    -- both a uniquely-marked synchronous fault and ThreadKilled; no consumer
    -- can run and a fresh no-fault scope remains usable afterwards.
    let acquisitionEvents =
          [ BeforePrivateSentinelClose,
            BeforePrivateSentinelUnlink,
            BeforePrivateDirectoryCreate
          ]
        runSentinelSync event = do
          copyFile archive firstCandidate
          copyFile archive secondCandidate
          fired <- newIORef (0 :: Int)
          seen <- newIORef ([] :: [(CacheSelectionLifecycleEvent, Int)])
          callbacks <- newIORef (0 :: Int)
          let marker = "sentinel-sync-" <> show (fromEnum event)
          plan <- cacheSelectionLifecyclePlanWithEvents $ \actual ordinal -> do
            modifyIORef' seen ((actual, ordinal) :)
            when (actual == event && ordinal == 1) $ do
              modifyIORef' fired (+ 1)
              throwIO (userError marker)
          outcome <- try @SomeException (runLifecycle noDiscovery plan (\_ -> modifyIORef' callbacks (+ 1) >> pure (Right ())))
          case outcome of
            Left exception ->
              case fromException exception :: Maybe IOException of
                Just ioException -> ioeGetErrorString ioException @?= marker
                Nothing -> assertFailure (marker <> " did not preserve an IOException")
            Right _ -> assertFailure (marker <> " was converted into a selection result")
          readIORef fired >>= (@?= 1)
          readIORef callbacks >>= (@?= 0)
          observed <- readIORef seen
          assertNoLaterAcquisition marker event observed
          assertNoPrivateResidue marker
          assertNoSidecars marker [firstCandidate, secondCandidate]
          freshReuse marker
        runSentinelAsync event = do
          copyFile archive firstCandidate
          copyFile archive secondCandidate
          fired <- newIORef (0 :: Int)
          seen <- newIORef ([] :: [(CacheSelectionLifecycleEvent, Int)])
          callbacks <- newIORef (0 :: Int)
          let label = "sentinel-async-" <> show (fromEnum event)
          plan <- cacheSelectionLifecyclePlanWithEvents $ \actual ordinal -> do
            modifyIORef' seen ((actual, ordinal) :)
            when (actual == event && ordinal == 1) $ do
              modifyIORef' fired (+ 1)
              throwIO ThreadKilled
          outcome <- try @AsyncException (runLifecycle noDiscovery plan (\_ -> modifyIORef' callbacks (+ 1) >> pure (Right ())))
          case outcome of
            Left ThreadKilled -> pure ()
            Left exception -> assertFailure (label <> " changed asynchronous identity: " <> show exception)
            Right _ -> assertFailure (label <> " was converted into a selection result")
          readIORef fired >>= (@?= 1)
          readIORef callbacks >>= (@?= 0)
          observed <- readIORef seen
          assertNoLaterAcquisition label event observed
          assertNoPrivateResidue label
          assertNoSidecars label [firstCandidate, secondCandidate]
          freshReuse label
    mapM_ runSentinelSync acquisitionEvents
    mapM_ runSentinelAsync acquisitionEvents

    -- Once a lease is accepted, cleanup owns the private directory.  The
    -- consumer sees only its parent-owned name long enough to add sidecar
    -- fixtures; every lifecycle cleanup action must still be attempted when
    -- any individual deletion boundary faults.
    let cleanupEvents =
          [ BeforePrivateDbUnlink,
            BeforePrivateJournalUnlink,
            BeforePrivateWalUnlink,
            BeforePrivateShmUnlink,
            BeforePrivateDirectoryRemoval
          ]
        materializePrivateSidecars label = do
          names <- filter (isInfixOf "adrai-reuse-private-") <$> listDirectory candidateDir
          privateDirectory <- case names of
            [name] -> pure (candidateDir </> name)
            _ -> assertFailure (label <> " expected one private directory") >> fail "unreachable"
          mapM_ (\suffix -> BS.writeFile (privateDirectory </> ("snapshot.sqlite" <> suffix)) "cleanup-sentinel") ["-journal", "-wal", "-shm"]
        assertAllCleanupAttempts _ observed =
          mapM_
            (\event -> length [() | (actual, ordinal) <- observed, actual == event, ordinal == 1] @?= 1)
            cleanupEvents
        runCleanupSync event = do
          copyFile archive firstCandidate
          copyFile archive secondCandidate
          fired <- newIORef (0 :: Int)
          seen <- newIORef ([] :: [(CacheSelectionLifecycleEvent, Int)])
          consumers <- newIORef (0 :: Int)
          let marker = "cleanup-sync-" <> show (fromEnum event)
          plan <- cacheSelectionLifecyclePlanWithEvents $ \actual ordinal -> do
            modifyIORef' seen ((actual, ordinal) :)
            when (actual == event && ordinal == 1) $ do
              modifyIORef' fired (+ 1)
              throwIO (userError marker)
          outcome <- try @SomeException (runLifecycle noDiscovery plan (\case
            CacheSelectionReuse _ _ _ -> do
              modifyIORef' consumers (+ 1)
              materializePrivateSidecars marker
              pure (Right ())
            _ -> pure (Left "cleanup matrix expected reuse")))
          case outcome of
            Left exception ->
              case fromException exception :: Maybe IOException of
                Just ioException -> ioeGetErrorString ioException @?= marker
                Nothing -> assertFailure (marker <> " did not preserve an IOException")
            Right _ -> assertFailure (marker <> " was converted into a selection result")
          readIORef fired >>= (@?= 1)
          readIORef consumers >>= (@?= 1)
          readIORef seen >>= assertAllCleanupAttempts marker
          assertNoPrivateResidue marker
          assertNoSidecars marker [firstCandidate, secondCandidate]
          freshReuse marker
        runCleanupAsync event = do
          copyFile archive firstCandidate
          copyFile archive secondCandidate
          fired <- newIORef (0 :: Int)
          seen <- newIORef ([] :: [(CacheSelectionLifecycleEvent, Int)])
          consumers <- newIORef (0 :: Int)
          let label = "cleanup-async-" <> show (fromEnum event)
          plan <- cacheSelectionLifecyclePlanWithEvents $ \actual ordinal -> do
            modifyIORef' seen ((actual, ordinal) :)
            when (actual == event && ordinal == 1) $ do
              modifyIORef' fired (+ 1)
              throwIO ThreadKilled
          outcome <- try @AsyncException (runLifecycle noDiscovery plan (\case
            CacheSelectionReuse _ _ _ -> do
              modifyIORef' consumers (+ 1)
              materializePrivateSidecars label
              pure (Right ())
            _ -> pure (Left "cleanup matrix expected reuse")))
          case outcome of
            Left ThreadKilled -> pure ()
            Left _ -> assertFailure (label <> " changed asynchronous identity")
            Right _ -> assertFailure (label <> " was converted into a selection result")
          readIORef fired >>= (@?= 1)
          readIORef consumers >>= (@?= 1)
          readIORef seen >>= assertAllCleanupAttempts label
          assertNoPrivateResidue label
          assertNoSidecars label [firstCandidate, secondCandidate]
          freshReuse label
    mapM_ runCleanupSync cleanupEvents
    mapM_ runCleanupAsync cleanupEvents

    -- Competing failures use the same real private sidecars: initiating async
    -- wins over cleanup async; cleanup async wins over initiating sync; and an
    -- initiating sync failure wins over a cleanup sync failure.
    let runPriority label cleanupFault consumerFault inspect = do
          copyFile archive firstCandidate
          copyFile archive secondCandidate
          seen <- newIORef ([] :: [(CacheSelectionLifecycleEvent, Int)])
          consumers <- newIORef (0 :: Int)
          plan <- cacheSelectionLifecyclePlanWithEvents $ \actual ordinal -> do
            modifyIORef' seen ((actual, ordinal) :)
            when (actual == BeforePrivateDbUnlink && ordinal == 1) cleanupFault
          _ <- inspect (runLifecycle noDiscovery plan (\case
            CacheSelectionReuse _ _ _ -> do
              modifyIORef' consumers (+ 1)
              materializePrivateSidecars label
              consumerFault
            _ -> pure (Left "priority matrix expected reuse")))
          readIORef consumers >>= (@?= 1)
          readIORef seen >>= assertAllCleanupAttempts label
          assertNoPrivateResidue label
          assertNoSidecars label [firstCandidate, secondCandidate]
          freshReuse label

    runPriority "initiating-thread-killed-over-cleanup-user-interrupt" (throwIO UserInterrupt) (throwIO ThreadKilled)
      (\action -> do
        outcome <- try @AsyncException action
        case outcome of
          Left ThreadKilled -> pure ()
          _ -> assertFailure "initiating ThreadKilled did not dominate cleanup UserInterrupt")
    runPriority "cleanup-thread-killed-over-initiating-sync" (throwIO ThreadKilled) (throwIO (userError "priority-initiating-sync"))
      (\action -> do
        outcome <- try @AsyncException action
        case outcome of
          Left ThreadKilled -> pure ()
          _ -> assertFailure "cleanup ThreadKilled did not dominate initiating sync failure")
    runPriority "initiating-sync-over-cleanup-sync" (throwIO (userError "priority-cleanup-sync")) (throwIO (userError "priority-initiating-sync"))
      (\action -> do
        outcome <- try @SomeException action
        case outcome of
          Left exception ->
            case fromException exception :: Maybe IOException of
              Just ioException -> ioeGetErrorString ioException @?= "priority-initiating-sync"
              Nothing -> assertFailure "initiating sync failure was not an IOException"
          Right _ -> assertFailure "initiating sync failure was converted into a selection result")

    -- A typed consumer result is an initiating failure too: a synchronous
    -- cleanup fault must not replace it, while cleanup ThreadKilled does.
    let typedProblem = "typed-lease-retirement-problem" :: String
        runTypedRetirement label cleanupFault inspect = do
          copyFile archive firstCandidate
          copyFile archive secondCandidate
          fired <- newIORef (0 :: Int)
          seen <- newIORef ([] :: [(CacheSelectionLifecycleEvent, Int)])
          consumers <- newIORef (0 :: Int)
          plan <- cacheSelectionLifecyclePlanWithEvents $ \actual ordinal -> do
            modifyIORef' seen ((actual, ordinal) :)
            when (actual == BeforePrivateDbUnlink && ordinal == 1) $ do
              modifyIORef' fired (+ 1)
              cleanupFault
          _ <- inspect $
            runLifecycle noDiscovery plan $ \case
            CacheSelectionReuse _ _ _ -> do
              modifyIORef' consumers (+ 1)
              materializePrivateSidecars label
              pure (Left typedProblem :: Either String ())
            _ -> pure (Left "typed retirement matrix expected reuse" :: Either String ())
          readIORef fired >>= (@?= 1)
          readIORef consumers >>= (@?= 1)
          readIORef seen >>= assertAllCleanupAttempts label
          assertNoPrivateResidue label
          assertNoSidecars label [firstCandidate, secondCandidate]
          freshReuse label

    runTypedRetirement "typed-left-over-cleanup-sync" (throwIO (userError "typed-retirement-cleanup-sync"))
      (\action -> fst <$> action >>= (@?= Left typedProblem))
    runTypedRetirement "cleanup-thread-killed-over-typed-left" (throwIO ThreadKilled)
      (\action -> do
        outcome <- try @AsyncException action
        case outcome of
          Left ThreadKilled -> pure ()
          _ -> assertFailure "cleanup ThreadKilled did not dominate typed Left")

    let assertLifecycleCancellation label lifecyclePlan = do
          copyFile archive firstCandidate
          copyFile archive secondCandidate
          callbacks <- newIORef (0 :: Int)
          cancelled <- try (runLifecycle noDiscovery lifecyclePlan (\_ -> modifyIORef' callbacks (+ 1) >> pure (Right ())))
          case cancelled of
            Left ThreadKilled -> pure ()
            _ -> assertFailure (label <> " did not preserve ThreadKilled")
          readIORef callbacks >>= (@?= 0)
          assertNoPrivateResidue label
          freshReuse label
    afterPrivateCopyPlan <- cacheSelectionLifecyclePlanWithEvents $ \event ordinal ->
      when (event == AfterPrivateCopy && ordinal == 1) (throwIO ThreadKilled)
    assertLifecycleCancellation "AfterPrivateCopy cancellation" afterPrivateCopyPlan
    beforeValidationPlan <- cacheSelectionLifecyclePlanWithEvents $ \event ordinal ->
      when (event == BeforePrivateValidation && ordinal == 1) (throwIO ThreadKilled)
    assertLifecycleCancellation "BeforePrivateValidation cancellation" beforeValidationPlan

    -- Cancellation in the consumer runs after lease acceptance; finalization
    -- still retires the lease, removes the private scope, and permits a fresh
    -- independently-owned retry.
    copyFile archive firstCandidate
    consumerCancelled <- try (runLifecycle noDiscovery noLifecycle (\case CacheSelectionReuse _ _ _ -> throwIO ThreadKilled; _ -> pure (Left "expected consumer reuse")))
    case consumerCancelled of
      Left ThreadKilled -> pure ()
      _ -> assertFailure "consumer cancellation did not preserve ThreadKilled"
    assertNoPrivateResidue "consumer cancellation"
    freshReuse "consumer cancellation"

    -- An interrupt after bracket-owned open preserves identity and leaves the
    -- public candidate reopenable without SQLite sidecars.
    copyFile archive firstCandidate
    copyFile archive secondCandidate
    afterOpenCancellationFired <- newIORef (0 :: Int)
    cancellationPlan <- reuseDiscoveryPlanWithEvents $ \event ordinal ->
      when (event == AfterOpen && ordinal == 1) $ do
        modifyIORef' afterOpenCancellationFired (+ 1)
        throwIO ThreadKilled
    afterOpenCancelled <- try
      (withCacheSelectionCascadeForTest cancellationPlan id (const (pure ())) noExact (pure 0) gitRepository candidateDir schema targetText (\_ -> pure (Right ())))
    case afterOpenCancelled of
      Left ThreadKilled -> pure ()
      _ -> assertFailure "after-open cancellation did not preserve ThreadKilled"
    readIORef afterOpenCancellationFired >>= (@?= 1)
    validateCachePublicationContract faultedCandidate >>= assertBool "after-open cancellation leaves the faulted candidate reopenable"
    validateCachePublicationContract fallbackCandidate >>= assertBool "after-open cancellation leaves the fallback candidate reopenable"
    assertNoSidecars "after-open cancellation" [faultedCandidate, fallbackCandidate]

    -- A finalizer failure after phase work precedes every consumer and leaves
    -- no generated private scope.
    copyFile archive firstCandidate
    callbackRuns <- newIORef (0 :: Int)
    finalizerCancelled <- try
      (withCacheSelectionCascadeForTest (reuseDiscoveryPlanForTest (pure ())) (\operation -> operation >>= \_ -> throwIO ThreadKilled) (const (pure ())) noExact (pure 0) gitRepository candidateDir schema targetText (\_ -> modifyIORef' callbackRuns (+ 1) >> pure (Right ())))
    case finalizerCancelled of
      Left ThreadKilled -> pure ()
      _ -> assertFailure "phase finalizer cancellation did not preserve ThreadKilled"
    readIORef callbackRuns >>= (@?= 0)
    listDirectory candidateDir >>= \names -> assertBool "phase finalizer leaves no private scope" (not (any (isInfixOf "adrai-reuse-private-") names))

    -- Lease probes are checked capabilities: owner access copies, foreign and
    -- escaped/recreated uses are closed without touching their destinations.
    copyFile archive firstCandidate
    ownerDestination <- pure (candidateDir </> "owner-copy.sqlite")
    foreignDestination <- pure (candidateDir </> "foreign-copy.sqlite")
    escapedDestination <- pure (candidateDir </> "escaped-copy.sqlite")
    privateName <- newIORef Nothing
    (leaseResult, _) <- runCascade CacheSelectionReuseKind 1 (reuseDiscoveryPlanForTest (pure ())) noExact (pure 0) candidateDir
      (\decision -> case decision of
        CacheSelectionReuse _ facts _ -> do
          owner <- probeCacheSelectionLeaseCopy decision ownerDestination
          owner @?= LeaseProbeCopied
          names <- filter (isInfixOf "adrai-reuse-private-") <$> listDirectory candidateDir
          case names of
            [name] -> writeIORef privateName (Just name)
            _ -> assertFailure "lease callback did not expose one test-owned private directory"
          completed <- newEmptyMVar
          _ <- forkIO (probeCacheSelectionLeaseCopy decision foreignDestination >>= putMVar completed)
          foreignProbe <- takeMVar completed
          foreignProbe @?= LeaseProbeWrongOwner
          doesFileExist foreignDestination >>= (@?= False)
          pure (Right (facts, probeCacheSelectionLeaseCopy decision))
        _ -> pure (Left "expected lease reuse"))
    case leaseResult of
      Left problem -> assertFailure problem
      Right (_, escapedProbe) -> do
        listDirectory candidateDir >>= \names -> assertBool "lease retirement removes private scope" (not (any (isInfixOf "adrai-reuse-private-") names))
        escapedProbe escapedDestination >>= (@?= LeaseProbeClosed)
        doesFileExist escapedDestination >>= (@?= False)
        readIORef privateName >>= \case
          Nothing -> assertFailure "private directory name was not observed"
          Just name -> do
            let recreated = candidateDir </> name
                sentinel = recreated </> "snapshot.sqlite"
            createDirectory recreated
            BS.writeFile sentinel "sentinel"
            escapedProbe escapedDestination >>= (@?= LeaseProbeClosed)
            BS.readFile sentinel >>= (@?= "sentinel")
            doesFileExist escapedDestination >>= (@?= False)
            removeFile sentinel
            removeDirectory recreated

assertRejectedArchiveMutation
  :: FilePath
  -> FilePath
  -> FilePath
  -> String
  -> (Connection -> IO ())
  -> IO ()
assertRejectedArchiveMutation repository source name message mutate = do
  let candidate = repository </> ".adrai" </> "cache" </> name
  copyFile source candidate
  bracket (open candidate) close mutate
  accepted <- validateCachePublicationContract candidate
  assertBool message (not accepted)

initialiseRepository :: FilePath -> IO ()
initialiseRepository repository = do
  createDirectoryIfMissing True repository
  git repository ["init", "--initial-branch", "main"]
  git repository ["config", "user.name", "ADRAI cache test"]
  git repository ["config", "user.email", "adrai-cache-test@example.invalid"]
  _ <- writeAndCommit repository "initial" [("seed.txt", "basis\n")]
  pure ()

writeAndCommit :: FilePath -> String -> [(FilePath, BS.ByteString)] -> IO Text
writeAndCommit repository message files = do
  mapM_ writeOne files
  git repository (["add", "--"] <> map fst files)
  git repository ["commit", "-m", message]
  repositoryHeadText repository
  where
    writeOne (relative, bytes) = do
      let destination = repository </> relative
      createDirectoryIfMissing True (takeDirectory destination)
      BS.writeFile destination bytes

repositoryHead :: FilePath -> IO GitOid
repositoryHead repository = do
  revision <- repositoryHeadText repository
  case mkGitOid revision of
    Left problem -> assertFailure ("fixture HEAD was not a Git OID: " <> show problem) >> fail "unreachable"
    Right oid -> pure oid

repositoryHeadText :: FilePath -> IO Text
repositoryHeadText repository = Text.strip . Text.pack <$> gitStdout repository ["rev-parse", "HEAD"]

fixtureOrFail :: Either Text value -> IO value
fixtureOrFail = either (\problem -> assertFailure ("invalid compiler fixture: " <> Text.unpack problem) >> fail "unreachable") pure

compile :: String -> FilePath -> IO String
compile label repository = do
  testExecutable <- getExecutablePath
  let executable = takeDirectory (takeDirectory testExecutable) </> "adrai" </> ("adrai" <> executableSuffix)
  exists <- doesFileExist executable
  assertBool ("package-built real-executable test target is absent: " <> executable) exists
  -- The build-tool executable is a sibling of this test component in Cabal's
  -- current build directory.  Resolving it from this running test binary (not
  -- the installed bindir) prevents a freshly changed library from being tested
  -- against an older registered adrai executable.
  (status, stdout, stderr) <- readProcessWithExitCode executable
    ["--repo", repository, "compile", "--json", "+RTS", "-N1", "-RTS"] ""
  case status of
    ExitSuccess -> pure stdout
    ExitFailure code -> assertFailure (label <> " real adrai compile failed (" <> show code <> "): " <> stderr) >> fail "unreachable"

executableSuffix :: String
executableSuffix
  | os == "mingw32" = ".exe"
  | otherwise = ""

git :: FilePath -> [String] -> IO ()
git repository arguments = do
  (status, _, stderr) <- readProcessWithExitCode "git" ("-C" : repository : arguments) ""
  case status of
    ExitSuccess -> pure ()
    ExitFailure code -> assertFailure ("git " <> unwords arguments <> " failed (" <> show code <> "): " <> stderr)

gitStdout :: FilePath -> [String] -> IO String
gitStdout repository arguments = do
  (status, stdout, stderr) <- readProcessWithExitCode "git" ("-C" : repository : arguments) ""
  case status of
    ExitSuccess -> pure stdout
    ExitFailure code -> assertFailure ("git " <> unwords arguments <> " failed (" <> show code <> "): " <> stderr) >> fail "unreachable"

expectedCanonicalFamilyScans :: Map.Map Text Int
expectedCanonicalFamilyScans = Map.fromList
  [ ("meta", 1),
    ("sqlite_master", 1),
    ("repository_config", 1),
    ("managed_source", 1),
    ("issue", 1),
    ("adr_conflict", 1),
    ("operation", 1),
    ("operation_member", 1),
    ("operation_member_parent", 1),
    ("decision_record", 1),
    ("connection_record", 1),
    ("reduced_adr", 1),
    ("axis_head", 1),
    ("current_connection", 1),
    ("operation_commit", 1),
    ("operation_target_coverage", 1),
    ("line_landing", 1),
    ("target_reachable_commit", 1),
    ("line_config", 1),
    ("ref_observation", 1),
    ("search_document", 1),
    ("search_section", 1),
    ("local_alias", 1)
  ]

expectedFtsValidationFamilies :: Map.Map Text Int
expectedFtsValidationFamilies =
  Map.fromList
    [ ("fts_search_exact", 1)
    , ("fts_search_stemmed", 1)
    , ("fts_search_identifier", 1)
    , ("fts_passage_exact", 1)
    , ("fts_passage_stemmed", 1)
    , ("fts_passage_identifier", 1)
    ]

assertValidationWorkFailureCounters :: FilePath -> FilePath -> Text -> IO ()
assertValidationWorkFailureCounters temporary archive target = do
  (_, missing) <- observeExactCacheValidationWorkForTest (temporary </> "missing-counter.sqlite") target
  cacheValidationSourceOpens missing @?= 0
  cacheValidationInvocations missing @?= 0
  cacheValidationCanonicalFamilyScans missing @?= Map.empty
  cacheValidationCanonicalFamilyRows missing @?= Map.empty
  cacheValidationFtsParityChecks missing @?= Map.empty
  cacheValidationFtsIntegrityChecks missing @?= Map.empty
  cacheValidationFtsPayloadMaterializations missing @?= 0
  cacheValidationFullMaterializationLoads missing @?= 0

  let failedQuery = temporary </> "post-open-query-failure.sqlite"
  copyFile archive failedQuery
  failedQueryConnection <- open failedQuery
  execute_ failedQueryConnection "DROP TABLE repository_config"
  close failedQueryConnection
  (failedQueryAccepted, failedQueryWork) <- observeExactCacheValidationWorkForTest failedQuery target
  assertBool "a post-open failed family query is rejected" (not failedQueryAccepted)
  cacheValidationSourceOpens failedQueryWork @?= 1
  cacheValidationInvocations failedQueryWork @?= 1
  cacheValidationCanonicalFamilyScans failedQueryWork @?= Map.fromList [("meta", 1), ("sqlite_master", 1)]
  Map.keysSet (cacheValidationCanonicalFamilyRows failedQueryWork) @?= Map.keysSet (cacheValidationCanonicalFamilyScans failedQueryWork)
  cacheValidationFtsParityChecks failedQueryWork @?= Map.empty
  cacheValidationFtsIntegrityChecks failedQueryWork @?= Map.empty
  cacheValidationFtsPayloadMaterializations failedQueryWork @?= 0
  cacheValidationFullMaterializationLoads failedQueryWork @?= 0

  let failedDecode = temporary </> "search-decode-failure.sqlite"
  copyFile archive failedDecode
  failedDecodeConnection <- open failedDecode
  execute_ failedDecodeConnection "UPDATE search_section SET section_kind='not-a-section' WHERE passage_rowid=(SELECT min(passage_rowid) FROM search_section)"
  close failedDecodeConnection
  (failedDecodeAccepted, failedDecodeWork) <- observeExactCacheValidationWorkForTest failedDecode target
  assertBool "a failed search-materialization decode is rejected" (not failedDecodeAccepted)
  cacheValidationSourceOpens failedDecodeWork @?= 1
  cacheValidationInvocations failedDecodeWork @?= 1
  cacheValidationCanonicalFamilyScans failedDecodeWork @?= Map.fromList
    [ (family, 1)
    | family <-
        [ "meta", "sqlite_master", "repository_config", "managed_source", "issue", "adr_conflict",
          "operation", "operation_member", "operation_member_parent", "decision_record", "connection_record",
          "reduced_adr", "axis_head", "current_connection", "operation_commit", "operation_target_coverage",
          "line_landing", "target_reachable_commit", "line_config", "ref_observation", "search_document",
          "search_section", "local_alias"
        ]
    ]
  Map.keysSet (cacheValidationCanonicalFamilyRows failedDecodeWork) @?= Map.keysSet (cacheValidationCanonicalFamilyScans failedDecodeWork)
  cacheValidationFtsParityChecks failedDecodeWork @?= Map.empty
  cacheValidationFtsIntegrityChecks failedDecodeWork @?= Map.empty
  cacheValidationFtsPayloadMaterializations failedDecodeWork @?= 0
  cacheValidationFullMaterializationLoads failedDecodeWork @?= 0

assertTargetReachabilityPlans :: FilePath -> Text -> IO ()
assertTargetReachabilityPlans archive target = do
  observed <- observeTargetReachabilityPlansForTest archive target
  details <- maybe (assertFailure "the production reachability plans could not be observed" >> fail "unreachable") pure observed
  assertPlan "bulk target reachability" "target_oid=?" (targetReachabilityBulkPlanDetails details)
  assertPlan "lower other-target probe" "target_oid<?" (targetReachabilityLowerProbePlanDetails details)
  assertPlan "upper other-target probe" "target_oid>?" (targetReachabilityUpperProbePlanDetails details)
  where
    assertPlan label predicate plans = do
      let rendered = Text.intercalate "\n" plans
      assertBool (label <> " does not use the canonical composite PK index: " <> Text.unpack rendered)
        ("sqlite_autoindex_target_reachable_commit_1" `Text.isInfixOf` rendered)
      assertBool (label <> " does not show the expected leading-key predicate: " <> Text.unpack rendered)
        (predicate `Text.isInfixOf` rendered)
      assertBool (label <> " performs a full target_reachable_commit scan: " <> Text.unpack rendered)
        (not ("SCAN target_reachable_commit" `Text.isInfixOf` rendered))
      assertBool (label <> " introduces a temporary sort: " <> Text.unpack rendered)
        (not ("USE TEMP B-TREE" `Text.isInfixOf` rendered))

assertIndexedFingerprintFixture :: FilePath -> FilePath -> IO ()
assertIndexedFingerprintFixture temporary archive = do
  let fixture = temporary </> "indexed-fingerprint.sqlite"
      operationA = operationId "a"
      operationB = operationId "aa"
      parentA = objectId "a"
      parentB = objectId "aa"
      childA = objectId "ab"
      emptyA = objectId "ac"
      parentC = objectId "b"
      childB = objectId "ba"
  copyFile archive fixture
  connection <- open fixture
  mapM_ (insertOperation connection) [operationA, operationB]
  mapM_ (uncurry (insertMember connection))
    [ (operationA, parentA), (operationA, parentB), (operationA, childA), (operationA, emptyA),
      (operationB, parentC), (operationB, childB)
    ]
  execute connection "INSERT INTO operation_member_parent(operation_id,object_id,ordinal,parent_object_id) VALUES (?,?,?,?)" (operationA, childA, 0 :: Int, parentA)
  execute connection "INSERT INTO operation_member_parent(operation_id,object_id,ordinal,parent_object_id) VALUES (?,?,?,?)" (operationA, childA, 1 :: Int, parentA)
  execute connection "INSERT INTO operation_member_parent(operation_id,object_id,ordinal,parent_object_id) VALUES (?,?,?,?)" (operationA, childA, 2 :: Int, parentB)
  execute connection "INSERT INTO operation_member_parent(operation_id,object_id,ordinal,parent_object_id) VALUES (?,?,?,?)" (operationB, childB, 0 :: Int, parentC)
  close connection
  refreshCacheMaterializationFingerprint fixture
  evidence <- assertFingerprintWork fixture

  let expected = materializationFingerprintObserverValue evidence
  assertChangedFingerprint (temporary </> "indexed-fingerprint-reordered.sqlite") fixture expected $ \mutated -> do
    execute mutated "UPDATE operation_member_parent SET ordinal=99 WHERE operation_id=? AND object_id=? AND ordinal=0" (operationA, childA)
    execute mutated "UPDATE operation_member_parent SET ordinal=0 WHERE operation_id=? AND object_id=? AND ordinal=2" (operationA, childA)
    execute mutated "UPDATE operation_member_parent SET ordinal=2 WHERE operation_id=? AND object_id=? AND ordinal=99" (operationA, childA)
  assertChangedFingerprint (temporary </> "indexed-fingerprint-multiplicity.sqlite") fixture expected $ \mutated ->
    execute mutated "DELETE FROM operation_member_parent WHERE operation_id=? AND object_id=? AND ordinal=1" (operationA, childA)
  assertChangedFingerprint (temporary </> "indexed-fingerprint-reassignment.sqlite") fixture expected $ \mutated -> do
    execute mutated
      "INSERT INTO operation_member(operation_id,object_id,object_type,event_kind,semantic_digest,path,blob_oid) SELECT ?,object_id,object_type,event_kind,semantic_digest,path,blob_oid FROM operation_member WHERE operation_id=? AND object_id=?"
      (operationB, operationA, emptyA)
    execute mutated "DELETE FROM operation_member WHERE operation_id=? AND object_id=?" (operationA, emptyA)
  assertSkewedFingerprintWork (temporary </> "indexed-fingerprint-skewed.sqlite") archive
  assertManyBucketFingerprintWork (temporary </> "indexed-fingerprint-many-buckets.sqlite") archive
  assertZeroParentFingerprintWork (temporary </> "indexed-fingerprint-zero-parent.sqlite") archive
  assertMalformedFingerprintParity temporary archive
  where
    operationId suffix = "O" <> Text.justifyRight 26 '0' suffix
    objectId suffix = "R" <> Text.justifyRight 26 '0' suffix
    insertOperation connection operation =
      execute connection
        "INSERT INTO operation(operation_id,timestamp_ms,actor_kind,actor_id,actor_model,basis_oid,branch_hint,upstream_hint,line_anchors,tool_version,input_digest,prompt_digest,context_digest) SELECT ?,timestamp_ms,actor_kind,actor_id,actor_model,basis_oid,branch_hint,upstream_hint,line_anchors,tool_version,input_digest,prompt_digest,context_digest FROM operation ORDER BY operation_id LIMIT 1"
        (Only operation)
    insertMember connection operation object =
      execute connection
        "INSERT INTO operation_member(operation_id,object_id,object_type,event_kind,semantic_digest,path,blob_oid) SELECT ?,?,object_type,event_kind,semantic_digest,path,blob_oid FROM operation_member ORDER BY operation_id,object_id LIMIT 1"
        (operation, object)
    singletonCount :: Connection -> Query -> IO Int
    singletonCount connection statement = do
      rows <- query_ connection statement :: IO [Only Int]
      case rows of
        [Only value] -> pure value
        _ -> assertFailure ("count query was not singleton: " <> show statement) >> fail "unreachable"
    assertFingerprintWork path = do
      evidence <- requireFingerprintEvidence path
      let persisted = materializationFingerprintPersistedValue evidence
      persisted @?= materializationFingerprintEstablishedValue evidence
      persisted @?= materializationFingerprintProductionValue evidence
      persisted @?= materializationFingerprintObserverValue evidence
      countConnection <- open path
      operationCount <- singletonCount countConnection "SELECT count(*) FROM operation"
      memberCount <- singletonCount countConnection "SELECT count(*) FROM operation_member"
      parentCount <- singletonCount countConnection "SELECT count(*) FROM operation_member_parent"
      memberBucketCount <- singletonCount countConnection "SELECT count(*) FROM (SELECT operation_id FROM operation_member GROUP BY operation_id)"
      parentBucketCount <- singletonCount countConnection "SELECT count(*) FROM (SELECT operation_id,object_id FROM operation_member_parent GROUP BY operation_id,object_id)"
      close countConnection
      let work = materializationFingerprintIndexWork evidence
      materializationFingerprintOperationRowsVisited work @?= operationCount
      materializationFingerprintMemberRowsConsumed work @?= memberCount
      materializationFingerprintParentRowsConsumed work @?= parentCount
      materializationFingerprintMemberBucketPrepends work @?= memberCount
      materializationFingerprintParentBucketPrepends work @?= parentCount
      materializationFingerprintMemberBucketsFinalized work @?= memberBucketCount
      materializationFingerprintParentBucketsFinalized work @?= parentBucketCount
      materializationFingerprintMemberBucketValuesReversed work @?= memberCount
      materializationFingerprintParentBucketValuesReversed work @?= parentCount
      materializationFingerprintMemberBucketLookups work @?= operationCount
      materializationFingerprintParentBucketLookups work @?= memberCount
      pure evidence
    assertSkewedFingerprintWork path source = do
      copyFile source path
      connection <- open path
      let operation = operationId "skew"
          parent = objectId "skewparent"
          child = objectId "skewchild"
          extraMember ordinal = objectId ("s" <> Text.justifyRight 4 '0' (Text.pack (show ordinal)))
      insertOperation connection operation
      mapM_ (insertMember connection operation) (parent : child : map extraMember [0 :: Int .. 63])
      mapM_
        (\ordinal -> execute connection "INSERT INTO operation_member_parent(operation_id,object_id,ordinal,parent_object_id) VALUES (?,?,?,?)" (operation, child, ordinal, parent))
        [0 :: Int .. 63]
      close connection
      refreshCacheMaterializationFingerprint path
      _ <- assertFingerprintWork path
      pure ()
    assertManyBucketFingerprintWork path source = do
      copyFile source path
      connection <- open path
      mapM_ (insertBucket connection) [0 :: Int .. 31]
      close connection
      refreshCacheMaterializationFingerprint path
      _ <- assertFingerprintWork path
      pure ()
      where
        insertBucket connection ordinal = do
          let suffix = Text.justifyRight 4 '0' (Text.pack (show ordinal))
              operation = operationId ("m" <> suffix)
              parent = objectId ("p" <> suffix)
              child = objectId ("c" <> suffix)
          insertOperation connection operation
          insertMember connection operation parent
          insertMember connection operation child
          execute connection "INSERT INTO operation_member_parent(operation_id,object_id,ordinal,parent_object_id) VALUES (?,?,?,?)" (operation, child, 0 :: Int, parent)
    assertZeroParentFingerprintWork path source = do
      copyFile source path
      connection <- open path
      let operation = operationId "zeroparent"
          member = objectId "zeroparent"
      insertOperation connection operation
      insertMember connection operation member
      matchingParents <- singletonCount connection "SELECT count(*) FROM operation_member_parent WHERE operation_id='O0000000000000000zeroparent' AND object_id='R0000000000000000zeroparent'"
      matchingParents @?= 0
      close connection
      refreshCacheMaterializationFingerprint path
      _ <- assertFingerprintWork path
      pure ()
    assertMalformedFingerprintParity temporaryDirectory source = do
      let invalidEncoding = temporaryDirectory </> "indexed-fingerprint-invalid-source-encoding.sqlite"
          shortDigest = temporaryDirectory </> "indexed-fingerprint-short-source-digest.sqlite"
          malformedRows = temporaryDirectory </> "indexed-fingerprint-malformed-rows.sqlite"
      copyFile source invalidEncoding
      invalidEncodingConnection <- open invalidEncoding
      execute_ invalidEncodingConnection "UPDATE meta SET value='not%%%base64' WHERE key='source_fingerprint'"
      close invalidEncodingConnection
      assertRejectedFingerprint invalidEncoding 0 0 0

      copyFile source shortDigest
      shortDigestConnection <- open shortDigest
      execute_ shortDigestConnection "UPDATE meta SET value='AA' WHERE key='source_fingerprint'"
      close shortDigestConnection
      assertRejectedFingerprint shortDigest 0 0 0

      copyFile source malformedRows
      malformedRowsConnection <- open malformedRows
      execute_ malformedRowsConnection "UPDATE operation SET line_anchors='not-json' WHERE operation_id=(SELECT min(operation_id) FROM operation)"
      close malformedRowsConnection
      assertRejectedFingerprint malformedRows 1 0 0

      let validPrefixThenMalformed = temporaryDirectory </> "indexed-fingerprint-valid-prefix-malformed.sqlite"
          prefixParent = objectId "a8parent"
          prefixChild = objectId "a8child"
      copyFile source validPrefixThenMalformed
      prefixConnection <- open validPrefixThenMalformed
      orderedPrefix <- query_ prefixConnection "SELECT operation_id FROM operation ORDER BY operation_id LIMIT 2" :: IO [Only Text]
      (prefixOperation, malformedOperation) <- case orderedPrefix of
        [Only prefixOperation, Only malformedOperation] -> pure (prefixOperation, malformedOperation)
        _ -> assertFailure "A8 fixture requires two canonically ordered operations" >> fail "unreachable"
      insertMember prefixConnection prefixOperation prefixParent
      insertMember prefixConnection prefixOperation prefixChild
      execute prefixConnection "INSERT INTO operation_member_parent(operation_id,object_id,ordinal,parent_object_id) VALUES (?,?,?,?)" (prefixOperation, prefixChild, 0 :: Int, prefixParent)
      execute prefixConnection "UPDATE operation SET line_anchors='not-json' WHERE operation_id=?" (Only malformedOperation)
      prefixMemberRows <- query prefixConnection "SELECT count(*) FROM operation_member WHERE operation_id=?" (Only prefixOperation) :: IO [Only Int]
      prefixMemberCount <- case prefixMemberRows of
        [Only count] -> pure count
        _ -> assertFailure "A8 prefix member count was not singleton" >> fail "unreachable"
      close prefixConnection
      -- The valid prefix performs one member lookup and one parent lookup for
      -- each of its two members.  The following malformed operation is
      -- consumed but rejects before its member lookup and before digest use.
      assertRejectedFingerprint validPrefixThenMalformed 2 1 prefixMemberCount
    assertRejectedFingerprint path expectedVisited expectedMemberLookups expectedParentLookups = do
      evidence <- requireFingerprintEvidence path
      assertBool "malformed fixture must retain the writer's stale persisted commitment"
        (materializationFingerprintPersistedValue evidence /= Nothing)
      materializationFingerprintEstablishedValue evidence @?= Nothing
      materializationFingerprintProductionValue evidence @?= Nothing
      materializationFingerprintObserverValue evidence @?= Nothing
      validateCachePublicationContract path >>= \accepted -> assertBool "malformed fingerprint evidence was accepted" (not accepted)
      countConnection <- open path
      memberCount <- singletonCount countConnection "SELECT count(*) FROM operation_member"
      parentCount <- singletonCount countConnection "SELECT count(*) FROM operation_member_parent"
      memberBucketCount <- singletonCount countConnection "SELECT count(*) FROM (SELECT operation_id FROM operation_member GROUP BY operation_id)"
      parentBucketCount <- singletonCount countConnection "SELECT count(*) FROM (SELECT operation_id,object_id FROM operation_member_parent GROUP BY operation_id,object_id)"
      close countConnection
      let work = materializationFingerprintIndexWork evidence
      materializationFingerprintMemberRowsConsumed work @?= memberCount
      materializationFingerprintParentRowsConsumed work @?= parentCount
      materializationFingerprintMemberBucketPrepends work @?= memberCount
      materializationFingerprintParentBucketPrepends work @?= parentCount
      materializationFingerprintMemberBucketsFinalized work @?= memberBucketCount
      materializationFingerprintParentBucketsFinalized work @?= parentBucketCount
      materializationFingerprintMemberBucketValuesReversed work @?= memberCount
      materializationFingerprintParentBucketValuesReversed work @?= parentCount
      materializationFingerprintOperationRowsVisited work @?= expectedVisited
      materializationFingerprintMemberBucketLookups work @?= expectedMemberLookups
      materializationFingerprintParentBucketLookups work @?= expectedParentLookups
    requireFingerprintEvidence path = do
      observed <- observeMaterializationFingerprintForTest path
      maybe (assertFailure "fingerprint observation failed" >> fail "unreachable") pure observed
    assertChangedFingerprint destination source expected mutate = do
      copyFile source destination
      connection <- open destination
      _ <- mutate connection
      close connection
      changed <- requireFingerprintEvidence destination
      materializationFingerprintEstablishedValue changed @?= materializationFingerprintProductionValue changed
      materializationFingerprintProductionValue changed @?= materializationFingerprintObserverValue changed
      assertBool "fingerprint mutation did not change the indexed canonical frame"
        (materializationFingerprintObserverValue changed /= expected)

assertFields :: String -> [String] -> String -> IO ()
assertFields label expected output =
  mapM_ (\field -> assertBool (label <> " is missing " <> field <> " in " <> output) (field `isInfixOf` compactJson output)) expected

-- | Focused fail-closed coverage for the fused provenance-refresh write.  Each
-- case uses a disposable candidate; the immutable archive is retained as the
-- publication target whose bytes and metadata must never change on rejection.
assertPublicationRefreshSafety :: FilePath -> FilePath -> IO ()
assertPublicationRefreshSafety _ target = do
  let families = Map.fromList [(name, 1) | name <-
        [ "fts_search_exact", "fts_search_stemmed", "fts_search_identifier"
        , "fts_passage_exact", "fts_passage_stemmed", "fts_passage_identifier"
        ]]
      phaseMap = Map.fromList . map (\phase -> (phase, 1))
      assertPhaseMaps :: String -> [Text] -> [Text] -> [Text] -> PublicationRefreshObservation -> IO ()
      assertPhaseMaps label attempted completed failed observation = do
        let work = publicationRefreshWork observation
        (label, cacheValidationPublicationPhaseAttempts work) @?= (label, phaseMap attempted)
        (label, cacheValidationPublicationPhaseCompletions work) @?= (label, phaseMap completed)
        (label, cacheValidationPublicationPhaseFailures work) @?= (label, phaseMap failed)
      assertCommon label observation = do
        assertBool (label <> " preserves the seed bytes") (publicationRefreshSeedBytesPreserved observation)
        assertBool (label <> " preserves the seed metadata") (publicationRefreshSeedMetadataPreserved observation)
        assertBool (label <> " leaves candidate reopenable") (publicationRefreshCandidateReopenable observation)
        assertBool (label <> " attempts owned cleanup") (publicationRefreshCleanupAttempted observation)
        assertBool (label <> " completes cleanup before constructing its observation") (publicationRefreshCleanupCompletedBeforeObservation observation)
        assertBool (label <> " successfully removes each owned artifact") (and (publicationRefreshCleanupResults observation))
        let cleanupRecords = publicationRefreshCleanupRecords observation
        map publicationRefreshCleanupArtifact cleanupRecords @?= [PublicationRefreshMainFile, PublicationRefreshJournalFile, PublicationRefreshWalFile, PublicationRefreshShmFile]
        assertBool (label <> " attempts every cleanup artifact") (all publicationRefreshCleanupArtifactAttempted cleanupRecords)
        assertBool (label <> " records successful cleanup for every artifact") (all publicationRefreshCleanupArtifactSucceeded cleanupRecords)
        assertBool (label <> " records final absence for every cleanup artifact") (all publicationRefreshCleanupArtifactAbsent cleanupRecords)
        assertBool (label <> " cleans owned candidate") (publicationRefreshOwnedCandidateCleaned observation)
        publicationRefreshPrePayloadSentinel observation @?= publicationRefreshPostPayloadSentinel observation
        publicationRefreshPreTriggerSentinel observation @?= publicationRefreshPostTriggerSentinel observation
        publicationRefreshTriggerBodyExecuted observation @?= False
      assertFailureCleanup :: String -> PublicationRefreshObservation -> IO ()
      assertFailureCleanup _ observation = do
        let work = publicationRefreshWork observation
        cacheValidationPublicationRollbackAttempts work @?= 1
        cacheValidationPublicationRollbackCompletions work @?= 1
        cacheValidationPublicationCloseAttempts work @?= 1
        cacheValidationPublicationCloseCompletions work @?= 1

  healthy <- observePublicationRefreshForTest target PublicationRefreshHealthy
  assertCommon "healthy fused refresh" healthy
  publicationRefreshOutcome healthy @?= PublicationRefreshSucceeded
  let healthyWork = publicationRefreshWork healthy
  assertPhaseMaps "healthy fused refresh"
    ["source-open", "canonical-load", "derivation", "update", "affected-row", "commit", "close"]
    ["source-open", "canonical-load", "derivation", "update", "affected-row", "commit", "close"]
    []
    healthy
  cacheValidationFtsParityAttempts healthyWork @?= families
  cacheValidationFtsParityChecks healthyWork @?= families
  cacheValidationFtsParityFailures healthyWork @?= Map.empty
  cacheValidationFtsIntegrityAttempts healthyWork @?= families
  cacheValidationFtsIntegrityChecks healthyWork @?= families
  cacheValidationFtsIntegrityFailures healthyWork @?= Map.empty
  cacheValidationFtsPayloadMaterializations healthyWork @?= 0
  cacheValidationPublicationRollbackAttempts healthyWork @?= 0
  cacheValidationPublicationCloseAttempts healthyWork @?= 1
  cacheValidationPublicationCloseCompletions healthyWork @?= 1
  publicationRefreshAffectedRows healthy @?= Just 1
  positivePayload <- observeFtsPayloadMaterializationForTest target
  assertBool "FTS payload positive control did not cross its boundary" (positivePayload > 0)

  corrupt <- observePublicationRefreshForTest target PublicationRefreshCorruptLastFtsPosting
  assertCommon "corrupt FTS posting" corrupt
  publicationRefreshOutcome corrupt @?= PublicationRefreshFtsPassageIdentifierRejected
  assertPhaseMaps "corrupt FTS posting"
    ["source-open", "canonical-load", "derivation", "rollback", "close"]
    ["source-open", "canonical-load", "derivation", "rollback", "close"]
    []
    corrupt
  assertBool "corrupt scenario did not remove a real candidate posting segment" (publicationRefreshPostingSegmentRemoved corrupt)
  cacheValidationFtsParityAttempts (publicationRefreshWork corrupt) @?= families
  cacheValidationFtsParityChecks (publicationRefreshWork corrupt) @?= families
  cacheValidationFtsParityFailures (publicationRefreshWork corrupt) @?= Map.empty
  cacheValidationFtsIntegrityAttempts (publicationRefreshWork corrupt) @?= families
  cacheValidationFtsIntegrityChecks (publicationRefreshWork corrupt) @?=
    Map.delete "fts_passage_identifier" families
  cacheValidationFtsIntegrityFailures (publicationRefreshWork corrupt) @?=
    Map.singleton "fts_passage_identifier" 1
  cacheValidationPublicationUpdates (publicationRefreshWork corrupt) @?= 0
  publicationRefreshAffectedRows corrupt @?= Nothing
  cacheValidationPublicationCommits (publicationRefreshWork corrupt) @?= 0
  assertFailureCleanup "corrupt FTS posting" corrupt

  missing <- observePublicationRefreshForTest target PublicationRefreshMissingRawFingerprint
  assertCommon "missing raw row" missing
  publicationRefreshOutcome missing @?= PublicationRefreshMissingRawFingerprintRejected
  assertPhaseMaps "missing raw row"
    ["source-open", "canonical-load", "derivation", "rollback", "close"]
    ["source-open", "canonical-load", "derivation", "rollback", "close"]
    []
    missing
  publicationRefreshPreRawFingerprintRows missing @?= 0
  publicationRefreshPreRawFingerprintValue missing @?= Nothing
  publicationRefreshRawFingerprintRows missing @?= 0
  publicationRefreshRawFingerprintValue missing @?= Nothing
  assertFailureCleanup "missing raw row" missing

  zero <- observePublicationRefreshForTest target PublicationRefreshForceZeroRowUpdate
  assertCommon "zero-row update" zero
  publicationRefreshOutcome zero @?= PublicationRefreshZeroRowUpdateRejected
  assertPhaseMaps "zero-row update"
    ["source-open", "canonical-load", "derivation", "update", "affected-row", "rollback", "close"]
    ["source-open", "canonical-load", "derivation", "update", "rollback", "close"]
    ["affected-row"]
    zero
  cacheValidationPublicationUpdates (publicationRefreshWork zero) @?= 1
  publicationRefreshAffectedRows zero @?= Just 0
  cacheValidationPublicationCommits (publicationRefreshWork zero) @?= 0
  assertFailureCleanup "zero-row update" zero

  forged <- observePublicationRefreshForTest target PublicationRefreshForgedTrigger
  assertCommon "forged SQL object" forged
  publicationRefreshOutcome forged @?= PublicationRefreshUnexpectedSchemaObjectRejected
  assertPhaseMaps "forged SQL object"
    ["source-open", "canonical-load", "derivation", "rollback", "close"]
    ["source-open", "canonical-load", "derivation", "rollback", "close"]
    []
    forged
  cacheValidationPublicationUpdates (publicationRefreshWork forged) @?= 0
  cacheValidationPublicationCommits (publicationRefreshWork forged) @?= 0
  assertFailureCleanup "forged SQL object" forged

  initiatingAsync <- observePublicationRefreshForTest target PublicationRefreshInitiatingThreadKilled
  assertCommon "initiating ThreadKilled" initiatingAsync
  publicationRefreshOutcome initiatingAsync @?= PublicationRefreshInitiatingThreadKilledRejected
  assertPhaseMaps "initiating ThreadKilled"
    ["source-open", "canonical-load", "derivation", "rollback", "close"]
    ["source-open", "canonical-load", "rollback", "close"]
    ["derivation"]
    initiatingAsync
  assertFailureCleanup "initiating ThreadKilled" initiatingAsync

  cleanupAsync <- observePublicationRefreshForTest target PublicationRefreshCleanupThreadKilled
  assertCommon "cleanup ThreadKilled" cleanupAsync
  publicationRefreshOutcome cleanupAsync @?= PublicationRefreshRollbackThreadKilledRejected
  assertPhaseMaps "cleanup ThreadKilled"
    ["source-open", "canonical-load", "derivation", "update", "affected-row", "rollback", "close"]
    ["source-open", "canonical-load", "derivation", "update", "rollback", "close"]
    ["affected-row"]
    cleanupAsync
  assertFailureCleanup "cleanup ThreadKilled" cleanupAsync

  initiatingSync <- observePublicationRefreshForTest target PublicationRefreshInitiatingSyncOverCleanupSync
  assertCommon "initiating sync precedence" initiatingSync
  publicationRefreshOutcome initiatingSync @?= PublicationRefreshInitiatingSync
  assertPhaseMaps "initiating sync precedence"
    ["source-open", "canonical-load", "derivation", "rollback", "close"]
    ["source-open", "canonical-load", "rollback", "close"]
    ["derivation"]
    initiatingSync
  assertFailureCleanup "initiating sync precedence" initiatingSync

  closeFailure <- observePublicationRefreshForTest target PublicationRefreshCloseFailure
  assertCommon "close failure" closeFailure
  publicationRefreshOutcome closeFailure @?= PublicationRefreshCloseSync
  assertPhaseMaps "close failure"
    ["source-open", "canonical-load", "derivation", "update", "affected-row", "commit", "close"]
    ["source-open", "canonical-load", "derivation", "update", "affected-row", "commit", "close"]
    []
    closeFailure
  cacheValidationPublicationRollbackAttempts (publicationRefreshWork closeFailure) @?= 0
  cacheValidationPublicationCloseAttempts (publicationRefreshWork closeFailure) @?= 1

  closeAsync <- observePublicationRefreshForTest target PublicationRefreshCloseThreadKilled
  assertCommon "close ThreadKilled" closeAsync
  publicationRefreshOutcome closeAsync @?= PublicationRefreshCloseThreadKilledRejected
  assertPhaseMaps "close ThreadKilled"
    ["source-open", "canonical-load", "derivation", "update", "affected-row", "commit", "close"]
    ["source-open", "canonical-load", "derivation", "update", "affected-row", "commit", "close"]
    []
    closeAsync

  unexpected <- try @SomeException (observePublicationRefreshForTest target PublicationRefreshUnexpectedInfrastructure)
  case unexpected of
    Left _ -> pure ()
    Right _ -> assertFailure "unexpected observer infrastructure failure was converted into an observation"

{- Historical open callback/path seam retained below only as a review preimage;
   it is deliberately excluded from this test module's live code.
assertPublicationRefreshSafety temporary target = do
  targetBytes <- BS.readFile target
  targetMetadata <- loadCacheMetaForTest target
  let candidate name = temporary </> name
      assertTargetPreserved label = do
        BS.readFile target >>= (@?= targetBytes)
        loadCacheMetaForTest target >>= (@?= targetMetadata)
      assertCandidateClosed label path = do
        validateCachePublicationContract path >>= assertBool (label <> " leaves the candidate reopenable")
        sidecars <- mapM doesFileExist [path <> "-journal", path <> "-wal", path <> "-shm"]
        assertBool (label <> " leaves SQLite sidecar residue") (not (or sidecars))
      mustFail label action = do
        outcome <- try @SomeException action
        case outcome of
          Left _ -> pure ()
          Right () -> assertFailure (label <> " unexpectedly succeeded")

  let healthy = candidate "refresh-single-derivation.sqlite"
  copyFile target healthy
  derivations <- newIORef (0 :: Int)
  refreshCachePublicationMaterializationFingerprintForTest
    (refreshPublicationTestPlanForTest (modifyIORef' derivations (+ 1)) RefreshPublicationNoFault (pure ()) (pure ()))
    healthy
  readIORef derivations >>= (@?= 1)
  assertCandidateClosed "healthy fused refresh" healthy

  let missing = candidate "refresh-missing-materialization-row.sqlite"
  copyFile target missing
  missingConnection <- open missing
  execute_ missingConnection "DELETE FROM meta WHERE key='materialization_fingerprint'"
  close missingConnection
  mustFail "missing raw materialization fingerprint row" $
    refreshCachePublicationMaterializationFingerprintForTest
      (refreshPublicationTestPlanForTest (pure ()) RefreshPublicationNoFault (pure ()) (pure ()))
      missing
  assertTargetPreserved "missing raw materialization fingerprint row"
  sidecarsMissing <- mapM doesFileExist [missing <> "-journal", missing <> "-wal", missing <> "-shm"]
  assertBool "missing raw row leaves SQLite sidecar residue" (not (or sidecarsMissing))

  let zero = candidate "refresh-zero-row-update.sqlite"
  copyFile target zero
  zeroBytes <- BS.readFile zero
  mustFail "zero-row materialization fingerprint update" $
    refreshCachePublicationMaterializationFingerprintForTest
      (refreshPublicationTestPlanForTest (pure ()) RefreshPublicationForceZeroRows (pure ()) (pure ()))
      zero
  BS.readFile zero >>= (@?= zeroBytes)
  assertCandidateClosed "zero-row materialization fingerprint update" zero
  assertTargetPreserved "zero-row materialization fingerprint update"

  let forged = candidate "refresh-forged-trigger.sqlite"
  copyFile target forged
  forgedConnection <- open forged
  beforeTitle <- query_ forgedConnection "SELECT title FROM search_document ORDER BY item_id LIMIT 1" :: IO [Only Text]
  execute_ forgedConnection "CREATE TRIGGER forged_refresh_side_effect AFTER UPDATE ON meta BEGIN UPDATE search_document SET title='forged' WHERE item_id=(SELECT min(item_id) FROM search_document); END"
  close forgedConnection
  mustFail "SQL-bearing trigger before refresh" $
    refreshCachePublicationMaterializationFingerprintForTest
      (refreshPublicationTestPlanForTest (pure ()) RefreshPublicationNoFault (pure ()) (pure ()))
      forged
  forgedCheck <- open forged
  afterTitle <- query_ forgedCheck "SELECT title FROM search_document ORDER BY item_id LIMIT 1" :: IO [Only Text]
  close forgedCheck
  afterTitle @?= beforeTitle
  assertTargetPreserved "SQL-bearing trigger before refresh"
  sidecarsForged <- mapM doesFileExist [forged <> "-journal", forged <> "-wal", forged <> "-shm"]
  assertBool "forged trigger leaves SQLite sidecar residue" (not (or sidecarsForged))

  let cancelled = candidate "refresh-async-arbitration.sqlite"
  copyFile target cancelled
  rollbackAttempts <- newIORef (0 :: Int)
  closeAttempts <- newIORef (0 :: Int)
  cancellation <- try @AsyncException $
    refreshCachePublicationMaterializationFingerprintForTest
      (refreshPublicationTestPlanForTest
        (throwIO ThreadKilled)
        RefreshPublicationNoFault
        (modifyIORef' rollbackAttempts (+ 1) >> throwIO (userError "rollback sync fault"))
        (modifyIORef' closeAttempts (+ 1) >> throwIO (userError "close sync fault")))
      cancelled
  case cancellation of
    Left ThreadKilled -> pure ()
    Left exception -> throwIO exception
    Right () -> assertFailure "initiating ThreadKilled was swallowed by refresh cleanup"
  readIORef rollbackAttempts >>= (@?= 1)
  readIORef closeAttempts >>= (@?= 1)
  assertCandidateClosed "initiating cancellation" cancelled

  let cleanupAsync = candidate "refresh-cleanup-async-precedence.sqlite"
  copyFile target cleanupAsync
  cleanupCancellation <- try @AsyncException $
    refreshCachePublicationMaterializationFingerprintForTest
      (refreshPublicationTestPlanForTest (pure ()) RefreshPublicationForceZeroRows (throwIO ThreadKilled) (pure ()))
      cleanupAsync
  case cleanupCancellation of
    Left ThreadKilled -> pure ()
    Left exception -> throwIO exception
    Right () -> assertFailure "cleanup ThreadKilled did not dominate refresh failure"
  assertCandidateClosed "cleanup cancellation" cleanupAsync

  let cleanupSync = candidate "refresh-cleanup-sync-precedence.sqlite"
  copyFile target cleanupSync
  mustFail "synchronous initiating failure over synchronous cleanup" $
    refreshCachePublicationMaterializationFingerprintForTest
      (refreshPublicationTestPlanForTest (pure ()) RefreshPublicationForceZeroRows (throwIO (userError "rollback cleanup fault")) (pure ()))
      cleanupSync
  assertCandidateClosed "synchronous cleanup precedence" cleanupSync

  let closeFailure = candidate "refresh-close-failure.sqlite"
  copyFile target closeFailure
  mustFail "close failure after committed refresh" $
    refreshCachePublicationMaterializationFingerprintForTest
      (refreshPublicationTestPlanForTest (pure ()) RefreshPublicationNoFault (pure ()) (throwIO (userError "close cleanup fault")))
      closeFailure
  assertCandidateClosed "close failure after committed refresh" closeFailure
-}

assertAbsent :: String -> String -> String -> IO ()
assertAbsent label field output = assertBool (label <> ": " <> output) (not (field `isInfixOf` compactJson output))

compactJson :: String -> String
compactJson = filter (`notElem` [' ', '\t', '\r', '\n'])

assertSnapshot :: FilePath -> GitOid -> IO FilePath
assertSnapshot repository revision = do
  let snapshot = repository </> ".adrai" </> "cache" </> Text.unpack (gitOidText revision) <> ".sqlite"
  exists <- doesFileExist snapshot
  assertBool ("published cache snapshot is missing: " <> snapshot) exists
  pure snapshot

removeCurrentIndex :: FilePath -> IO ()
removeCurrentIndex repository =
  mapM_ removeIfPresent
    [ repository </> ".adrai" </> "index.sqlite"
    , repository </> ".adrai" </> "index.sqlite-journal"
    , repository </> ".adrai" </> "index.sqlite-shm"
    , repository </> ".adrai" </> "index.sqlite-wal"
    ]
  where
    removeIfPresent path = do
      exists <- doesFileExist path
      if exists then removeFile path else pure ()
