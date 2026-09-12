{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test-only, capability-safe seams for cache-selection lifecycle tests.
-- This module intentionally exports plans and closed observations, never a
-- discovered source path, a raw reuse descriptor, a lease constructor, or a
-- runner over an arbitrary lease.
module Adrai.Compiler.CacheSelection.TestSupport
  ( ExactArchiveCleanupPlan,
    exactArchiveCleanupPlanForTest,
    withExactArchiveAliasRepairForTest,
    withExactArchiveAliasRepairWithCleanupForTest,
    readExactArchiveAliasStatusForTest,
    loadCacheMetaForTest,
    cachePublicationMaterializationFingerprintEvidence,
    CacheValidationWorkCounters (..),
    observeExactCacheValidationWorkForTest,
    TargetReachabilityPlanDetails (..),
    observeTargetReachabilityPlansForTest,
    MaterializationFingerprintIndexWork (..),
    MaterializationFingerprintEvidence (..),
    observeMaterializationFingerprintForTest,
    ReuseDiscoveryPlan,
    reuseDiscoveryPlanForTest,
    ReuseDiscoveryEvent (..),
    reuseDiscoveryPlanWithEvents,
    withCacheSelectionCascadeForTest,
    CacheSelectionLifecyclePlan,
    cacheSelectionLifecyclePlanForTest,
    CacheSelectionLifecycleEvent (..),
    cacheSelectionLifecyclePlanWithEvents,
    withCacheSelectionCascadeWithPlansForTest,
    reuseFactSourceRevision,
    reuseFactCacheKey,
    reuseFactPrivateBytes,
    reuseFactRank,
    reuseFactMtime,
    LeaseProbeResult (..),
    probeCacheSelectionLeaseCopy,
    PostCommitRefreshPlan (..),
    PostCommitCloneCancellation (..),
    PostCommitCloneObservation (..),
    observeValidatedPostCommitCloneForTest,
    PublicationRefreshScenario (..),
    PublicationRefreshOutcome (..),
    PublicationRefreshCleanupArtifact (..),
    PublicationRefreshCleanupRecord (..),
    PublicationRefreshObservation (..),
    PublicationRefreshAcquisitionFault (..),
    PublicationRefreshAcquisitionException (..),
    PublicationRefreshAcquisitionObservation (..),
    observePublicationRefreshAcquisitionForTest,
    observePublicationRefreshForTest,
    observeFtsPayloadMaterializationForTest,
  )
where

import Adrai.Compiler.CacheSelection.Internal
  ( ExactArchiveCleanupHooks,
    ExactArchiveCompileFacts,
    ExactArchiveAliasStatus,
    cachePublicationMaterializationFingerprintEvidence,
    exactArchiveCleanupHooksForTest,
    loadCacheMeta,
    readExactArchiveAliasStatusWith,
    withExactArchiveAliasRepairWith,
    withExactArchiveAliasRepairWithCleanupHooks,
    CacheSelectionCascadeDecision (..),
    CacheLeaseReuseFacts (..),
    AncestorRank,
    CacheSelectionMetrics,
    ReuseDiscoveryHooks (..),
    CacheSelectionLifecycleHooks (..),
    CacheSelectionLifecycleEvent (..),
    CacheValidationWorkCounters (..),
    TargetReachabilityPlanDetails (..),
    MaterializationFingerprintIndexWork (..),
    withCacheSelectionCascadeWithHooks,
    loadValidatedCacheRows,
    observeExactCacheValidationWorkForTest,
    observeTargetReachabilityPlansForTest,
    persistedMaterializationFingerprint,
    persistedMaterializationFingerprintFromRows,
    persistedMaterializationFingerprintFromRowsWithWork,
    RefreshPublicationHooks (..),
    RefreshPublicationUpdateMode (..),
    refreshCachePublicationMaterializationFingerprintWithHooks,
    newCacheValidationWorkCountersForTest,
    observeFtsPayloadMaterializationForTest,
    rethrowCacheAsync,
  )
import Adrai.Compiler.CacheLease.Internal (CacheLeaseError (..), LeaseAcceptedFacts (..), copyLeaseSourceTo)
import Adrai.Provenance.Ensure (openReadWriteExisting)
import Adrai.Service.PostCommitIndex.Internal
  ( PostCommitIndexResult (..),
    clonePostCommitIndexValidatedSourceWithAfterCopyHookForTest,
  )
import Control.Applicative ((<|>))
import Control.Exception (AsyncException (ThreadKilled), Exception, IOException, SomeAsyncException, SomeException, bracket, displayException, fromException, mask, throwIO, toException, try)
import Control.Monad (when)
import Adrai.Git (GitOid, Repository)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.IORef (newIORef, atomicModifyIORef', readIORef, writeIORef)
import Database.SQLite.Simple (Only (..), close, execute, execute_, open, query, query_, withTransaction)
import System.Directory (copyFile, doesFileExist, listDirectory, removeFile)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (hClose, openTempFile)
import System.IO.Error (ioeGetErrorString)

data MaterializationFingerprintEvidence = MaterializationFingerprintEvidence
  { materializationFingerprintPersistedValue :: Maybe Text,
    materializationFingerprintEstablishedValue :: Maybe Text,
    materializationFingerprintProductionValue :: Maybe Text,
    materializationFingerprintObserverValue :: Maybe Text,
    materializationFingerprintIndexWork :: MaterializationFingerprintIndexWork
  }
  deriving (Eq, Show)

-- | Fixed, capability-free publication cases.  The test can select only a
-- scenario; this module owns the copied candidate, mutations, hooks, and all
-- cleanup.  No path, connection, SQL, callback, or mutator escapes.
data PublicationRefreshScenario
  = PublicationRefreshHealthy
  | PublicationRefreshMissingRawFingerprint
  | PublicationRefreshForceZeroRowUpdate
  | PublicationRefreshForgedTrigger
  | PublicationRefreshCorruptLastFtsPosting
  | PublicationRefreshInitiatingThreadKilled
  | PublicationRefreshCleanupThreadKilled
  | PublicationRefreshInitiatingSyncOverCleanupSync
  | PublicationRefreshCloseFailure
  | PublicationRefreshCloseThreadKilled
  | PublicationRefreshUnexpectedInfrastructure
  deriving (Eq, Show, Enum, Bounded)

data PublicationRefreshOutcome
  = PublicationRefreshSucceeded
  | PublicationRefreshInitiatingThreadKilledRejected
  | PublicationRefreshRollbackThreadKilledRejected
  | PublicationRefreshCloseThreadKilledRejected
  | PublicationRefreshInitiatingSync
  | PublicationRefreshRollbackSync
  | PublicationRefreshCloseSync
  | PublicationRefreshMissingRawFingerprintRejected
  | PublicationRefreshZeroRowUpdateRejected
  | PublicationRefreshUnexpectedSchemaObjectRejected
  | PublicationRefreshFtsPassageIdentifierRejected
  deriving (Eq, Show)

data PublicationRefreshCleanupArtifact
  = PublicationRefreshMainFile
  | PublicationRefreshJournalFile
  | PublicationRefreshWalFile
  | PublicationRefreshShmFile
  deriving (Eq, Show, Enum, Bounded)

-- | A closed record: the owned pathname is never retained in the result.
data PublicationRefreshCleanupRecord = PublicationRefreshCleanupRecord
  { publicationRefreshCleanupArtifact :: PublicationRefreshCleanupArtifact,
    publicationRefreshCleanupArtifactAttempted :: Bool,
    publicationRefreshCleanupArtifactSucceeded :: Bool,
    publicationRefreshCleanupArtifactAbsent :: Bool
  }
  deriving (Eq, Show)

data PublicationRefreshFaultOrigin
  = PublicationRefreshInitiatingFault
  | PublicationRefreshRollbackFault
  | PublicationRefreshCloseFault
  deriving (Eq, Show)

data PublicationRefreshObservation = PublicationRefreshObservation
  { publicationRefreshOutcome :: PublicationRefreshOutcome,
    publicationRefreshWork :: CacheValidationWorkCounters,
    publicationRefreshPreRawFingerprintRows :: Int,
    publicationRefreshPreRawFingerprintValue :: Maybe Text,
    publicationRefreshRawFingerprintRows :: Int,
    publicationRefreshRawFingerprintValue :: Maybe Text,
    publicationRefreshPostingSegmentRemoved :: Bool,
    publicationRefreshPostingSegmentId :: Maybe Int,
    publicationRefreshAffectedRows :: Maybe Int,
    publicationRefreshPrePayloadSentinel :: Maybe Text,
    publicationRefreshPostPayloadSentinel :: Maybe Text,
    publicationRefreshPreTriggerSentinel :: Maybe Text,
    publicationRefreshPostTriggerSentinel :: Maybe Text,
    publicationRefreshTriggerBodyExecuted :: Bool,
    publicationRefreshCleanupAttempted :: Bool,
    publicationRefreshCleanupResults :: [Bool],
    publicationRefreshCleanupRecords :: [PublicationRefreshCleanupRecord],
    publicationRefreshCleanupCompletedBeforeObservation :: Bool,
    publicationRefreshCandidateReopenable :: Bool,
    publicationRefreshSeedBytesPreserved :: Bool,
    publicationRefreshSeedMetadataPreserved :: Bool,
    publicationRefreshCandidateSidecarsAbsent :: Bool,
    publicationRefreshOwnedCandidateCleaned :: Bool
  }
  deriving (Eq, Show)

data PublicationRefreshFixedException
  = PublicationInitiatingSyncException
  | PublicationRollbackSyncException
  | PublicationCloseSyncException
  deriving (Eq, Show)

instance Exception PublicationRefreshFixedException

-- | Deterministic acquisition faults for the closed publication-candidate
-- observer.  The cancellation cases pause at 'IO ()' supplied to
-- 'observePublicationRefreshAcquisitionForTest'; a test can signal from that
-- action and then use 'throwTo' against the observer thread without a race.
data PublicationRefreshAcquisitionFault
  = PublicationRefreshAcquisitionHealthy
  | PublicationRefreshAcquisitionCloseFailure
  | PublicationRefreshAcquisitionRemoveFailure
  | PublicationRefreshAcquisitionCopyFailure
  | PublicationRefreshAcquisitionPartialCopyFailure
  | PublicationRefreshAcquisitionAwaitCancellation
  | PublicationRefreshAcquisitionAwaitCancellationOverCleanupFailure
  deriving (Eq, Show)

data PublicationRefreshAcquisitionException
  = PublicationRefreshAcquisitionCloseException
  | PublicationRefreshAcquisitionRemoveException
  | PublicationRefreshAcquisitionCopyException
  | PublicationRefreshAcquisitionCleanupException
  deriving (Eq, Show)

instance Exception PublicationRefreshAcquisitionException

-- | Closed acquisition evidence.  The owned pathname and handle never escape;
-- the separately returned exception is the original exception caught from the
-- acquisition action, even when cleanup also fails.
data PublicationRefreshAcquisitionObservation = PublicationRefreshAcquisitionObservation
  { publicationRefreshAcquisitionReserved :: Bool,
    publicationRefreshAcquisitionCloseAttempts :: Int,
    publicationRefreshAcquisitionRemoveAttempts :: Int,
    publicationRefreshAcquisitionCopyAttempts :: Int,
    publicationRefreshAcquisitionPartialCopyCreated :: Bool,
    publicationRefreshAcquisitionCleanupHandleAttempted :: Bool,
    publicationRefreshAcquisitionCleanupHandleSucceeded :: Bool,
    publicationRefreshAcquisitionCleanupRecords :: [PublicationRefreshCleanupRecord],
    publicationRefreshAcquisitionCleanupFailureObserved :: Bool,
    publicationRefreshAcquisitionCandidateAbsent :: Bool,
    publicationRefreshAcquisitionSidecarsAbsent :: Bool,
    publicationRefreshAcquisitionSeedBytesPreserved :: Bool,
    publicationRefreshAcquisitionSeedMetadataPreserved :: Bool,
    publicationRefreshAcquisitionSentinelPreserved :: Bool
  }
  deriving (Eq, Show)

data PublicationRefreshAcquisitionTrace = PublicationRefreshAcquisitionTrace
  { publicationRefreshAcquisitionTraceReserved :: Bool,
    publicationRefreshAcquisitionTraceCloseAttempts :: Int,
    publicationRefreshAcquisitionTraceRemoveAttempts :: Int,
    publicationRefreshAcquisitionTraceCopyAttempts :: Int,
    publicationRefreshAcquisitionTracePartialCopyCreated :: Bool,
    publicationRefreshAcquisitionTraceCleanupHandleAttempted :: Bool,
    publicationRefreshAcquisitionTraceCleanupHandleSucceeded :: Bool,
    publicationRefreshAcquisitionTraceCleanupRecords :: [PublicationRefreshCleanupRecord],
    publicationRefreshAcquisitionTraceCleanupFailure :: Maybe SomeException
  }

-- | Exercise candidate acquisition without exposing the candidate.  The
-- optional exception is the exact initiating exception value; tests can use
-- 'fromException' to verify a real asynchronous 'ThreadKilled'.
observePublicationRefreshAcquisitionForTest
  :: FilePath
  -> FilePath
  -> PublicationRefreshAcquisitionFault
  -> IO ()
  -> IO (Maybe SomeException, PublicationRefreshAcquisitionObservation)
observePublicationRefreshAcquisitionForTest seed sentinel fault atCancellation = mask $ \_ -> do
  seedBytes <- BS.readFile seed
  seedMetadata <- loadCacheMeta seed
  sentinelBytes <- BS.readFile sentinel
  (acquisition, trace) <- acquirePublicationRefreshCandidate fault atCancellation seed
  finalTrace <- case acquisition of
    Left _ -> pure trace
    Right candidate -> do
      (records, cleanupFailure) <- cleanupPublicationRefreshCandidate fault candidate
      pure trace
        { publicationRefreshAcquisitionTraceCleanupRecords = records,
          publicationRefreshAcquisitionTraceCleanupFailure = cleanupFailure
        }
  seedBytesAfter <- BS.readFile seed
  seedMetadataAfter <- loadCacheMeta seed
  sentinelBytesAfter <- BS.readFile sentinel
  let records = publicationRefreshAcquisitionTraceCleanupRecords finalTrace
      mainAbsent = case records of
        record : _ -> publicationRefreshCleanupArtifactAbsent record
        [] -> False
      sidecarsAbsent = length records == 4 && all publicationRefreshCleanupArtifactAbsent (drop 1 records)
      observation = PublicationRefreshAcquisitionObservation
        { publicationRefreshAcquisitionReserved = publicationRefreshAcquisitionTraceReserved finalTrace,
          publicationRefreshAcquisitionCloseAttempts = publicationRefreshAcquisitionTraceCloseAttempts finalTrace,
          publicationRefreshAcquisitionRemoveAttempts = publicationRefreshAcquisitionTraceRemoveAttempts finalTrace,
          publicationRefreshAcquisitionCopyAttempts = publicationRefreshAcquisitionTraceCopyAttempts finalTrace,
          publicationRefreshAcquisitionPartialCopyCreated = publicationRefreshAcquisitionTracePartialCopyCreated finalTrace,
          publicationRefreshAcquisitionCleanupHandleAttempted = publicationRefreshAcquisitionTraceCleanupHandleAttempted finalTrace,
          publicationRefreshAcquisitionCleanupHandleSucceeded = publicationRefreshAcquisitionTraceCleanupHandleSucceeded finalTrace,
          publicationRefreshAcquisitionCleanupRecords = records,
          publicationRefreshAcquisitionCleanupFailureObserved = maybe False (const True) (publicationRefreshAcquisitionTraceCleanupFailure finalTrace),
          publicationRefreshAcquisitionCandidateAbsent = mainAbsent,
          publicationRefreshAcquisitionSidecarsAbsent = sidecarsAbsent,
          publicationRefreshAcquisitionSeedBytesPreserved = seedBytesAfter == seedBytes,
          publicationRefreshAcquisitionSeedMetadataPreserved = seedMetadataAfter == seedMetadata,
          publicationRefreshAcquisitionSentinelPreserved = sentinelBytesAfter == sentinelBytes
        }
  pure (either Just (const Nothing) acquisition, observation)

acquirePublicationRefreshCandidate
  :: PublicationRefreshAcquisitionFault
  -> IO ()
  -> FilePath
  -> IO (Either SomeException FilePath, PublicationRefreshAcquisitionTrace)
acquirePublicationRefreshCandidate fault atCancellation source = mask $ \restore -> do
  closeAttempts <- newIORef 0
  removeAttempts <- newIORef 0
  copyAttempts <- newIORef 0
  partialCopyCreated <- newIORef False
  handleClosed <- newIORef False
  (candidate, handle) <- openTempFile (takeDirectory source) "adrai-publication-refresh.sqlite"
  acquisition <- try @SomeException $ do
    runPhase restore $ do
      atomicModifyIORef' closeAttempts (\n -> (n + 1, ()))
      when (fault == PublicationRefreshAcquisitionCloseFailure) (throwIO PublicationRefreshAcquisitionCloseException)
      hClose handle
    writeIORef handleClosed True
    runPhase restore $ do
      atomicModifyIORef' removeAttempts (\n -> (n + 1, ()))
      when (fault == PublicationRefreshAcquisitionRemoveFailure) (throwIO PublicationRefreshAcquisitionRemoveException)
      removeFile candidate
    runPhase restore $ do
      atomicModifyIORef' copyAttempts (\n -> (n + 1, ()))
      case fault of
        PublicationRefreshAcquisitionCopyFailure -> throwIO PublicationRefreshAcquisitionCopyException
        PublicationRefreshAcquisitionPartialCopyFailure -> do
          bytes <- BS.readFile source
          BS.writeFile candidate (BS.take (max 1 (BS.length bytes `div` 2)) bytes)
          writeIORef partialCopyCreated True
          throwIO PublicationRefreshAcquisitionCopyException
        PublicationRefreshAcquisitionAwaitCancellation -> atCancellation
        PublicationRefreshAcquisitionAwaitCancellationOverCleanupFailure -> atCancellation
        _ -> pure ()
      copyFile source candidate
    pure candidate
  closed <- readIORef handleClosed
  (cleanupHandleAttempted, cleanupHandleSucceeded, handleCleanupFailure) <-
    if closed
      then pure (False, False, Nothing)
      else do
        result <- try @SomeException (hClose handle)
        pure (True, either (const False) (const True) result, either Just (const Nothing) result)
  (cleanupRecords, pathCleanupFailure) <- case acquisition of
    Left _ -> cleanupPublicationRefreshCandidate fault candidate
    Right _ -> pure ([], Nothing)
  observedCloseAttempts <- readIORef closeAttempts
  observedRemoveAttempts <- readIORef removeAttempts
  observedCopyAttempts <- readIORef copyAttempts
  observedPartialCopy <- readIORef partialCopyCreated
  let trace = PublicationRefreshAcquisitionTrace
        { publicationRefreshAcquisitionTraceReserved = True,
          publicationRefreshAcquisitionTraceCloseAttempts = observedCloseAttempts,
          publicationRefreshAcquisitionTraceRemoveAttempts = observedRemoveAttempts,
          publicationRefreshAcquisitionTraceCopyAttempts = observedCopyAttempts,
          publicationRefreshAcquisitionTracePartialCopyCreated = observedPartialCopy,
          publicationRefreshAcquisitionTraceCleanupHandleAttempted = cleanupHandleAttempted,
          publicationRefreshAcquisitionTraceCleanupHandleSucceeded = cleanupHandleSucceeded,
          publicationRefreshAcquisitionTraceCleanupRecords = cleanupRecords,
          publicationRefreshAcquisitionTraceCleanupFailure = handleCleanupFailure <|> pathCleanupFailure
        }
  pure (acquisition, trace)
  where
    -- Each interruptible phase is restored separately.  State is advanced only
    -- after it returns to the masked region, so cancellation cannot create an
    -- unowned handle or pathname gap.
    runPhase restore action = do
      result <- try @SomeException (restore action)
      either throwIO pure result

cleanupPublicationRefreshCandidate
  :: PublicationRefreshAcquisitionFault
  -> FilePath
  -> IO ([PublicationRefreshCleanupRecord], Maybe SomeException)
cleanupPublicationRefreshCandidate fault candidate = do
  results <- traverse removeAndVerify
    [ (PublicationRefreshMainFile, candidate),
      (PublicationRefreshJournalFile, candidate <> "-journal"),
      (PublicationRefreshWalFile, candidate <> "-wal"),
      (PublicationRefreshShmFile, candidate <> "-shm")
    ]
  pure (map fst results, firstFailure (map snd results))
  where
    removeAndVerify (artifact, path) = do
      operation <- try @SomeException $ do
        exists <- doesFileExist path
        when exists (removeFile path)
        when
          ( fault == PublicationRefreshAcquisitionAwaitCancellationOverCleanupFailure
              && artifact == PublicationRefreshMainFile
          )
          (throwIO PublicationRefreshAcquisitionCleanupException)
      finalProbe <- try @SomeException (doesFileExist path)
      let finalAbsence = case finalProbe of
            Right False -> Right ()
            Right True -> Left (toException (userError "owned publication refresh acquisition path remains after cleanup"))
            Left exception -> Left exception
          failure = firstFailure [either Just (const Nothing) operation, either Just (const Nothing) finalAbsence]
      pure
        ( PublicationRefreshCleanupRecord
            artifact
            True
            (either (const False) (const True) operation)
            (either (const False) (const True) finalAbsence),
          failure
        )
    firstFailure = foldr (<|>) Nothing

observePublicationRefreshForTest :: FilePath -> PublicationRefreshScenario -> IO PublicationRefreshObservation
observePublicationRefreshForTest seed scenario = mask $ \restore -> do
  seedBytes <- BS.readFile seed
  seedMetadata <- loadCacheMeta seed
  (acquisition, _) <- acquirePublicationRefreshCandidate PublicationRefreshAcquisitionHealthy (pure ()) seed
  candidate <- either throwIO pure acquisition
  run <- try @SomeException $ restore $ do
    (removedPosting, removedPostingId) <- prepareScenario candidate scenario
    (preRows, preValue) <- readRawFingerprint candidate
    prePayload <- logicalPayloadSentinel candidate
    preTrigger <- triggerSentinel candidate
    (workHooks, readWork) <- newCacheValidationWorkCountersForTest
    affectedRows <- newIORef Nothing
    faults <- newIORef []
    outcome <- try @SomeException (refreshCachePublicationMaterializationFingerprintWithHooks (fixedHooks scenario workHooks affectedRows faults) candidate)
    work <- readWork
    postState <- try @SomeException $ do
      (rawRows, rawValue) <- readRawFingerprint candidate
      postPayload <- logicalPayloadSentinel candidate
      postTrigger <- triggerSentinel candidate
      reopenable <- isReopenable candidate
      seedBytesAfter <- BS.readFile seed
      seedMetadataAfter <- loadCacheMeta seed
      pure (rawRows, rawValue, postPayload, postTrigger, reopenable, seedBytesAfter == seedBytes, seedMetadataAfter == seedMetadata)
    observedAffectedRows <- readIORef affectedRows
    observedFaults <- readIORef faults
    pure (removedPosting, removedPostingId, preRows, preValue, prePayload, preTrigger, outcome, work, observedAffectedRows, observedFaults, postState)
  (cleanupRecords, cleanupFailure) <- cleanupCandidate candidate
  case cleanupFailure of
    Just exception -> throwIO exception
    Nothing -> pure ()
  case run of
    Left exception -> throwIO exception
    Right (removedPosting, removedPostingId, preRows, preValue, prePayload, preTrigger, outcome, work, affectedRows, faults, postState) ->
      case postState of
        Left exception -> throwIO exception
        Right (rawRows, rawValue, postPayload, postTrigger, reopenable, seedPreserved, metadataPreserved) ->
          case classifyOutcome scenario outcome preRows affectedRows faults work of
            Nothing -> either
              (\exception -> throwIO (userError ("publication refresh observer lost its expected outcome; scenario=" <> show scenario <> ", preRows=" <> show preRows <> ", attempts=" <> show (cacheValidationPublicationPhaseAttempts work) <> ", completions=" <> show (cacheValidationPublicationPhaseCompletions work) <> ", failures=" <> show (cacheValidationPublicationPhaseFailures work) <> ", exception=" <> displayException exception)))
              (const (throwIO (userError "publication refresh observer lost its expected outcome")))
              outcome
            Just classified ->
              pure PublicationRefreshObservation
                { publicationRefreshOutcome = classified,
                  publicationRefreshWork = work,
                  publicationRefreshPreRawFingerprintRows = preRows,
                  publicationRefreshPreRawFingerprintValue = preValue,
                  publicationRefreshRawFingerprintRows = rawRows,
                  publicationRefreshRawFingerprintValue = rawValue,
                  publicationRefreshPostingSegmentRemoved = removedPosting,
                  publicationRefreshPostingSegmentId = removedPostingId,
                  publicationRefreshAffectedRows = affectedRows,
                  publicationRefreshPrePayloadSentinel = prePayload,
                  publicationRefreshPostPayloadSentinel = postPayload,
                  publicationRefreshPreTriggerSentinel = preTrigger,
                  publicationRefreshPostTriggerSentinel = postTrigger,
                  publicationRefreshTriggerBodyExecuted = preTrigger /= postTrigger,
                  publicationRefreshCleanupAttempted = True,
                  publicationRefreshCleanupResults = map publicationRefreshCleanupArtifactSucceeded cleanupRecords,
                  publicationRefreshCleanupRecords = cleanupRecords,
                  publicationRefreshCleanupCompletedBeforeObservation = True,
                  publicationRefreshCandidateReopenable = reopenable,
                  publicationRefreshSeedBytesPreserved = seedPreserved,
                  publicationRefreshSeedMetadataPreserved = metadataPreserved,
                  publicationRefreshCandidateSidecarsAbsent = all publicationRefreshCleanupArtifactAbsent (drop 1 cleanupRecords),
                  publicationRefreshOwnedCandidateCleaned = all publicationRefreshCleanupArtifactAbsent cleanupRecords
                }
  where
    cleanupCandidate candidate = do
      results <- traverse removeAndVerify
        [ (PublicationRefreshMainFile, candidate)
        , (PublicationRefreshJournalFile, candidate <> "-journal")
        , (PublicationRefreshWalFile, candidate <> "-wal")
        , (PublicationRefreshShmFile, candidate <> "-shm")
        ]
      pure (map fst results, firstFailure (map snd results))
    removeAndVerify (artifact, path) = do
      operation <- try @SomeException $ do
        exists <- doesFileExist path
        when exists (removeFile path)
      finalProbe <- try @SomeException (doesFileExist path)
      let finalAbsence = case finalProbe of
            Right False -> Right ()
            Right True -> Left (toException (userError "owned publication refresh candidate remains after cleanup"))
            Left exception -> Left exception
          failure = firstFailure [either Just (const Nothing) operation, either Just (const Nothing) finalAbsence]
      pure
        ( PublicationRefreshCleanupRecord
            artifact
            True
            (either (const False) (const True) operation)
            (either (const False) (const True) finalAbsence),
          failure
        )
    firstFailure = foldr (<|>) Nothing
    prepareScenario candidate = \case
      PublicationRefreshMissingRawFingerprint -> withCandidate candidate (\connection -> execute_ connection "DELETE FROM meta WHERE key='materialization_fingerprint'") >> pure (False, Nothing)
      PublicationRefreshCorruptLastFtsPosting -> withCandidate candidate $ \connection -> do
        postingRows <- query_ connection "SELECT id FROM fts_passage_identifier_data WHERE id > 10 ORDER BY id DESC LIMIT 1" :: IO [Only Int]
        case postingRows of
          [Only posting] -> do
             execute connection "DELETE FROM fts_passage_identifier_data WHERE id=?" (Only posting)
             remaining <- query connection "SELECT id FROM fts_passage_identifier_data WHERE id=?" (Only posting) :: IO [Only Int]
             pure (null remaining, Just posting)
          _ -> ioError (userError "fixed FTS corruption requires a removable fts_passage_identifier posting segment")
      PublicationRefreshForgedTrigger -> withCandidate candidate (\connection ->
        execute_ connection "CREATE TRIGGER forged_refresh_side_effect AFTER UPDATE ON meta BEGIN UPDATE search_document SET title='forged' WHERE item_id=(SELECT min(item_id) FROM search_document); END") >> pure (False, Nothing)
      _ -> pure (False, Nothing)
    withCandidate candidate action = bracket (open candidate) close action
    readRawFingerprint candidate = bracket (open candidate) close $ \connection -> do
      rows <- query connection "SELECT value FROM meta WHERE key='materialization_fingerprint'" () :: IO [Only Text]
      pure (length rows, case rows of [Only value] -> Just value; _ -> Nothing)
    logicalPayloadSentinel candidate = bracket (open candidate) close $ \connection -> do
      rows <- query_ connection "SELECT group_concat(frame, '|') FROM (SELECT item_id || ':' || title AS frame FROM search_document ORDER BY item_id)" :: IO [Only (Maybe Text)]
      pure (case rows of [Only value] -> value; _ -> Nothing)
    triggerSentinel candidate = bracket (open candidate) close $ \connection -> do
      rows <- query_ connection "SELECT title FROM search_document ORDER BY item_id LIMIT 1" :: IO [Only Text]
      pure (case rows of [Only value] -> Just value; _ -> Nothing)
    isReopenable candidate = do
      result <- try @SomeException (bracket (open candidate) close (const (pure ())))
      pure (either (const False) (const True) result)
    fixedHooks selected workHooks affectedRows faults =
      RefreshPublicationHooks
        { refreshPublicationObserveDerivation = case selected of
            PublicationRefreshInitiatingThreadKilled -> fault PublicationRefreshInitiatingFault ThreadKilled
            PublicationRefreshInitiatingSyncOverCleanupSync -> fault PublicationRefreshInitiatingFault PublicationInitiatingSyncException
            PublicationRefreshUnexpectedInfrastructure -> ioError (userError "unexpected observer infrastructure failure")
            _ -> pure (),
          refreshPublicationObserveAffectedRows = writeIORef affectedRows . Just,
          refreshPublicationUpdateMode = case selected of
            PublicationRefreshForceZeroRowUpdate -> RefreshPublicationNoMatchUpdate
            PublicationRefreshCleanupThreadKilled -> RefreshPublicationNoMatchUpdate
            _ -> RefreshPublicationExactUpdate,
          refreshPublicationBeforeRollback = case selected of
            PublicationRefreshInitiatingThreadKilled -> fault PublicationRefreshRollbackFault PublicationRollbackSyncException
            PublicationRefreshCleanupThreadKilled -> fault PublicationRefreshRollbackFault ThreadKilled
            PublicationRefreshInitiatingSyncOverCleanupSync -> fault PublicationRefreshRollbackFault PublicationRollbackSyncException
            _ -> pure (),
          refreshPublicationBeforeClose = case selected of
            PublicationRefreshInitiatingThreadKilled -> fault PublicationRefreshCloseFault PublicationCloseSyncException
            PublicationRefreshInitiatingSyncOverCleanupSync -> fault PublicationRefreshCloseFault PublicationCloseSyncException
            PublicationRefreshCloseFailure -> fault PublicationRefreshCloseFault PublicationCloseSyncException
            PublicationRefreshCloseThreadKilled -> fault PublicationRefreshCloseFault ThreadKilled
            _ -> pure (),
          refreshPublicationWorkHooks = Just workHooks
        }
      where
        fault origin exception = atomicModifyIORef' faults (\seen -> (seen <> [origin], ())) >> throwIO exception
    classifyOutcome selected outcome preRows affectedRows faults work = case outcome of
      Right () -> Just PublicationRefreshSucceeded
      Left exception
        | Just ThreadKilled <- fromException exception -> case faults of
            PublicationRefreshInitiatingFault : _ -> Just PublicationRefreshInitiatingThreadKilledRejected
            PublicationRefreshRollbackFault : _ -> Just PublicationRefreshRollbackThreadKilledRejected
            PublicationRefreshCloseFault : _ -> Just PublicationRefreshCloseThreadKilledRejected
            _ -> Nothing
        | Just PublicationInitiatingSyncException <- fromException exception -> Just PublicationRefreshInitiatingSync
        | Just PublicationRollbackSyncException <- fromException exception -> Just PublicationRefreshRollbackSync
        | Just PublicationCloseSyncException <- fromException exception -> Just PublicationRefreshCloseSync
        | Map.member "fts_passage_identifier" (cacheValidationFtsIntegrityFailures work) -> Just PublicationRefreshFtsPassageIdentifierRejected
        | Map.member "affected-row" (cacheValidationPublicationPhaseFailures work) && affectedRows == Just 0 -> Just PublicationRefreshZeroRowUpdateRejected
        | selected == PublicationRefreshMissingRawFingerprint
            && preRows == 0
            && Map.lookup "derivation" (cacheValidationPublicationPhaseCompletions work) == Just 1
            && Map.null (cacheValidationPublicationPhaseFailures work)
            && Map.notMember "update" (cacheValidationPublicationPhaseAttempts work)
            && Map.notMember "commit" (cacheValidationPublicationPhaseAttempts work)
            && hasUserError "cache provenance sync requires exactly one materialization_fingerprint metadata row" exception -> Just PublicationRefreshMissingRawFingerprintRejected
        | selected == PublicationRefreshForgedTrigger
            && preRows == 1
            && Map.lookup "derivation" (cacheValidationPublicationPhaseCompletions work) == Just 1
            && Map.null (cacheValidationPublicationPhaseFailures work)
            && Map.notMember "update" (cacheValidationPublicationPhaseAttempts work)
            && Map.notMember "commit" (cacheValidationPublicationPhaseAttempts work)
            && hasUserError "provenance refresh produced an invalid publication candidate" exception -> Just PublicationRefreshUnexpectedSchemaObjectRejected
        | otherwise -> Nothing
    hasUserError expected exception =
      case fromException exception :: Maybe IOException of
        Just ioException -> ioeGetErrorString ioException == expected
        Nothing -> False

-- | Compare four closed values: writer metadata, the established SQL
-- reconstruction, the stats-free accepted-row production path, and the
-- separately observed accepted-row traversal.  No connection, accepted-row
-- bundle, or authority capability escapes this test-only boundary.
observeMaterializationFingerprintForTest :: FilePath -> IO (Maybe MaterializationFingerprintEvidence)
observeMaterializationFingerprintForTest path = do
  outcome <- try @SomeException $
    bracket (openReadWriteExisting path) close $ \connection -> withTransaction connection $ do
      metadataRows <- query connection "SELECT key,value FROM meta WHERE key IN ('materialization_fingerprint','source_fingerprint')" () :: IO [(Text, Text)]
      let metadata = Map.fromList metadataRows
          persisted = Map.lookup "materialization_fingerprint" metadata
      established <- persistedMaterializationFingerprint connection (Map.lookup "source_fingerprint" metadata)
      rows <- loadValidatedCacheRows connection
      let production = persistedMaterializationFingerprintFromRows rows
          (observed, work) = persistedMaterializationFingerprintFromRowsWithWork rows
      pure MaterializationFingerprintEvidence
        { materializationFingerprintPersistedValue = persisted,
          materializationFingerprintEstablishedValue = established,
          materializationFingerprintProductionValue = production,
          materializationFingerprintObserverValue = observed,
          materializationFingerprintIndexWork = work
        }
  case outcome of
    Left exception -> rethrowCacheAsync exception >> pure Nothing
    Right evidence -> pure (Just evidence)

-- | Opaque pathless discovery fault-plan surface.  Keeping the constructor
-- private prevents tests from gaining an archive capability.
newtype ReuseDiscoveryPlan = ReuseDiscoveryPlan (ReuseDiscoveryEvent -> IO ())

reuseDiscoveryPlanForTest :: IO () -> ReuseDiscoveryPlan
reuseDiscoveryPlanForTest action = ReuseDiscoveryPlan (const action)

data ReuseDiscoveryEvent = BeforeOpen | AfterOpen | AfterMetadata | BeforeRank
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Pathless deterministic plan: ordinal is counted per event after the hidden
-- hook has been reached; the candidate pathname is deliberately discarded.
reuseDiscoveryPlanWithEvents :: (ReuseDiscoveryEvent -> Int -> IO ()) -> IO ReuseDiscoveryPlan
reuseDiscoveryPlanWithEvents fire = do
  counters <- traverse (const (newIORef 0)) ([minBound .. maxBound] :: [ReuseDiscoveryEvent])
  let next event = do
        let counter = counters !! fromEnum event
        ordinal <- atomicModifyIORef' counter (\n -> (n + 1, n + 1))
        fire event ordinal
  pure (ReuseDiscoveryPlan next)

withCacheSelectionCascadeForTest
  :: ReuseDiscoveryPlan
  -> (forall x. IO x -> IO x)
  -> (CacheSelectionMetrics -> IO ())
  -> IO (Either error (Maybe ExactArchiveCompileFacts)) -> IO Integer
  -> Repository -> FilePath -> Text -> Text
  -> (forall scope. CacheSelectionCascadeDecision scope -> IO (Either error a))
  -> IO (Either error a)
withCacheSelectionCascadeForTest (ReuseDiscoveryPlan fire) phase observe exactAction exactBytes repository cacheDirectory schema target consume =
  withCacheSelectionCascadeWithPlansForTest (ReuseDiscoveryPlan fire) (cacheSelectionLifecyclePlanForTest (pure ())) phase observe exactAction exactBytes repository cacheDirectory schema target consume

-- | Opaque pathless private-lifecycle plan.  Its action is all a test can
-- observe or inject: no private temporary name, source, SQLite connection, or
-- lease capability crosses this boundary.
newtype CacheSelectionLifecyclePlan = CacheSelectionLifecyclePlan (CacheSelectionLifecycleEvent -> IO ())

cacheSelectionLifecyclePlanForTest :: IO () -> CacheSelectionLifecyclePlan
cacheSelectionLifecyclePlanForTest action = CacheSelectionLifecyclePlan (const action)

-- | Deterministic per-event ordinals for lifecycle fault plans.  The ordinal
-- is assigned only after the hidden lifecycle boundary is reached.
cacheSelectionLifecyclePlanWithEvents :: (CacheSelectionLifecycleEvent -> Int -> IO ()) -> IO CacheSelectionLifecyclePlan
cacheSelectionLifecyclePlanWithEvents fire = do
  counters <- traverse (const (newIORef 0)) ([minBound .. maxBound] :: [CacheSelectionLifecycleEvent])
  let next event = do
        let counter = counters !! fromEnum event
        ordinal <- atomicModifyIORef' counter (\n -> (n + 1, n + 1))
        fire event ordinal
  pure (CacheSelectionLifecyclePlan next)

-- | Pathless combined test seam.  Production is fixed to no-op lifecycle
-- hooks; this wrapper is the only supported way focused tests can coordinate
-- discovery and private-resource fault schedules.
withCacheSelectionCascadeWithPlansForTest
  :: ReuseDiscoveryPlan
  -> CacheSelectionLifecyclePlan
  -> (forall x. IO x -> IO x)
  -> (CacheSelectionMetrics -> IO ())
  -> IO (Either error (Maybe ExactArchiveCompileFacts)) -> IO Integer
  -> Repository -> FilePath -> Text -> Text
  -> (forall scope. CacheSelectionCascadeDecision scope -> IO (Either error a))
  -> IO (Either error a)
withCacheSelectionCascadeWithPlansForTest (ReuseDiscoveryPlan discoveryFire) (CacheSelectionLifecyclePlan lifecycleFire) phase observe exactAction exactBytes repository cacheDirectory schema target consume =
  withCacheSelectionCascadeWithHooks discoveryHooks (CacheSelectionLifecycleHooks lifecycleFire) phase observe exactAction exactBytes repository cacheDirectory schema target consume
  where
    discoveryHooks = ReuseDiscoveryHooks (const (discoveryFire BeforeOpen)) (const (discoveryFire AfterOpen)) (const (discoveryFire AfterMetadata)) (const (discoveryFire BeforeRank))

reuseFactSourceRevision :: CacheLeaseReuseFacts -> Text
reuseFactSourceRevision = leaseAcceptedSourceRevision . cacheLeaseAcceptedFacts

reuseFactCacheKey :: CacheLeaseReuseFacts -> Text
reuseFactCacheKey = leaseAcceptedCacheKey . cacheLeaseAcceptedFacts

reuseFactPrivateBytes :: CacheLeaseReuseFacts -> Integer
reuseFactPrivateBytes = leaseAcceptedPrivateBytes . cacheLeaseAcceptedFacts

reuseFactRank :: CacheLeaseReuseFacts -> AncestorRank
reuseFactRank = cacheLeaseReuseRank

reuseFactMtime :: CacheLeaseReuseFacts -> Integer
reuseFactMtime = cacheLeaseReuseMtime

data LeaseProbeResult
  = LeaseProbeCopied
  | LeaseProbeWrongOwner
  | LeaseProbeClosed
  | LeaseProbeOtherFailure Text
  | LeaseProbeNoReuse
  deriving (Eq, Show)

probeCacheSelectionLeaseCopy :: CacheSelectionCascadeDecision scope -> FilePath -> IO LeaseProbeResult
probeCacheSelectionLeaseCopy decision destination =
  case decision of
    CacheSelectionReuse lease _ _ -> do
      result <- try @SomeException (copyLeaseSourceTo lease destination)
      pure $ case result of
        Right () -> LeaseProbeCopied
        Left exception ->
          case fromException exception of
            Just CacheLeaseWrongOwner -> LeaseProbeWrongOwner
            Just (CacheLeaseNotOpen _) -> LeaseProbeClosed
            Just (CacheLeaseInvalidTransition _) -> LeaseProbeClosed
            Just CacheLeaseSourceMissing -> LeaseProbeClosed
            Just CacheLeaseSourceAlreadyRegistered -> LeaseProbeOtherFailure (Text.pack (displayException exception))
            Nothing -> LeaseProbeOtherFailure (Text.pack (displayException exception))
    _ -> pure LeaseProbeNoReuse

-- | The only refresh actions available to tests.  The disposable candidate is
-- interpreted inside this module; no test receives its pathname.
data PostCommitRefreshPlan
  = PostCommitRefreshDeleteRef Text
  | PostCommitRefreshCancel
  | PostCommitRefreshNoop
  deriving (Eq, Show)

data PostCommitCloneCancellation
  = PostCommitCloneThreadKilled
  | PostCommitCloneOtherAsync Text
  deriving (Eq, Show)

-- | Closed evidence from the hidden raw-clone harness.  No live target,
-- candidate, or filesystem capability survives this observation.
data PostCommitCloneObservation = PostCommitCloneObservation
  { postCommitCloneIndexed :: Bool,
    postCommitCloneTargetExisted :: Bool,
    postCommitCloneCandidateResidue :: Bool,
    postCommitCloneCancellation :: Maybe PostCommitCloneCancellation,
    postCommitCloneError :: Maybe Text
  }
  deriving (Eq, Show)

-- | Run a raw-fixture clone into a unique internally-owned destination, project
-- only closed facts, and remove every owned output before returning.  The
-- hook occurs after copy but receives no candidate capability.
observeValidatedPostCommitCloneForTest :: FilePath -> GitOid -> Maybe Int -> PostCommitRefreshPlan -> IO () -> IO PostCommitCloneObservation
observeValidatedPostCommitCloneForTest source target history refreshPlan afterCopy = mask $ \restore -> do
  let directory = takeDirectory source
  (destination, reservation) <- openTempFile directory "adrai-test-clone-target-"
  firstClose <- try @SomeException (hClose reservation)
  retryClose <- case firstClose of
    Left _ -> try @SomeException (hClose reservation)
    Right () -> pure (Right ())
  outcome <- case firstClose of
    Left exception -> pure (Left exception)
    Right () -> try @SomeException . restore $ do
      removeFile destination
      clonePostCommitIndexValidatedSourceWithAfterCopyHookForTest source target destination history refresh afterCopy
  observed <- try @SomeException $ do
    targetExisted <- doesFileExist destination
    names <- listDirectory directory
    let candidatePrefix = Text.pack (takeFileName destination <> ".post-commit-")
        candidatePaths = [directory </> name | name <- names, candidatePrefix `Text.isPrefixOf` Text.pack name]
    candidateResidue <- fmap or (mapM doesFileExist candidatePaths)
    pure (targetExisted, candidateResidue)
  enumerated <- try @SomeException (listDirectory directory)
  let candidatePrefix = Text.pack (takeFileName destination <> ".post-commit-")
      candidatePaths = case enumerated of
        Right names -> [directory </> name | name <- names, candidatePrefix `Text.isPrefixOf` Text.pack name]
        Left _ -> []
      ownedPaths = destination : destinationSidecars destination <> candidatePaths
  cleanupOutcomes <- cleanupOwnedFiles ownedPaths
  resolve firstClose retryClose outcome observed enumerated cleanupOutcomes
  where
    refresh candidate =
      case refreshPlan of
        PostCommitRefreshDeleteRef refName ->
          bracket (open candidate) close $ \connection ->
            execute connection "DELETE FROM ref_observation WHERE ref_name=?" (Only refName)
        PostCommitRefreshCancel -> throwIO ThreadKilled
        PostCommitRefreshNoop -> pure ()

    cancellationOf exception =
      case fromException exception :: Maybe AsyncException of
        Just ThreadKilled -> Just PostCommitCloneThreadKilled
        _ ->
          case fromException exception :: Maybe SomeAsyncException of
            Just _ -> Just (PostCommitCloneOtherAsync (Text.pack (displayException exception)))
            Nothing -> Nothing

    synchronousError exception
      | Just _ <- fromException exception :: Maybe SomeAsyncException = Nothing
      | otherwise = Just (Text.pack (displayException exception))

    destinationSidecars destination =
      [ destination <> "-journal",
        destination <> "-shm",
        destination <> "-wal"
      ]


    resolve firstClose retryClose outcome observed enumerated cleanupOutcomes =
      let actionFailures = [voidOutcome firstClose, voidOutcome retryClose, voidOutcome outcome]
          infrastructure = [voidOutcome observed, voidOutcome enumerated]
          actionAsync = firstAsync actionFailures
          infraAsync = firstAsync (infrastructure <> cleanupOutcomes)
          actionSync = firstSync actionFailures
          infraSync = firstSync (infrastructure <> cleanupOutcomes)
       in case (outcome, observed, actionAsync, infraAsync, actionSync, infraSync) of
            (Left exception, Right facts, Just _, Nothing, Nothing, Nothing)
              | Just PostCommitCloneThreadKilled <- cancellationOf exception -> observationFrom exception facts
            _ -> case actionAsync of
              Just exception -> throwIO exception
              Nothing -> case infraAsync of
                Just exception -> throwIO exception
                Nothing -> case actionSync of
                  Just exception -> throwIO exception
                  Nothing -> case infraSync of
                    Just exception -> throwIO exception
                    Nothing -> case (outcome, observed) of
                      (Right result, Right (targetExisted, candidateResidue)) -> pure PostCommitCloneObservation
                        { postCommitCloneIndexed = postCommitIndexed result, postCommitCloneTargetExisted = targetExisted, postCommitCloneCandidateResidue = candidateResidue,
                          postCommitCloneCancellation = Nothing, postCommitCloneError = fmap (Text.pack . show) (postCommitIndexError result) }
                      (Left exception, Right (targetExisted, candidateResidue)) -> pure PostCommitCloneObservation
                        { postCommitCloneIndexed = False, postCommitCloneTargetExisted = targetExisted, postCommitCloneCandidateResidue = candidateResidue,
                          postCommitCloneCancellation = cancellationOf exception, postCommitCloneError = synchronousError exception }
                      _ -> error "unreachable"
      where
        observationFrom exception (targetExisted, candidateResidue) = pure PostCommitCloneObservation
          { postCommitCloneIndexed = False, postCommitCloneTargetExisted = targetExisted, postCommitCloneCandidateResidue = candidateResidue,
            postCommitCloneCancellation = cancellationOf exception, postCommitCloneError = Nothing }

    voidOutcome = either Left (const (Right ()))
    firstAsync = foldr chooseAsync Nothing
      where
        chooseAsync (Left exception) rest | isAsync exception = Just exception | otherwise = rest
        chooseAsync (Right ()) rest = rest
    firstSync = foldr chooseSync Nothing
      where
        chooseSync (Left exception) rest | isAsync exception = rest | otherwise = Just exception
        chooseSync (Right ()) rest = rest

    -- Every owned name is attempted and then proven absent while masked.
    cleanupOwnedFiles paths = do
      removalOutcomes <- mapM removeIfPresent paths
      absenceOutcomes <- mapM verifyAbsent paths
      pure (removalOutcomes <> absenceOutcomes)

    removeIfPresent path = do
      exists <- try @SomeException (doesFileExist path)
      case exists of
        Left exception -> pure (Left exception)
        Right False -> pure (Right ())
        Right True -> try @SomeException (removeFile path)

    verifyAbsent path = do
      exists <- try @SomeException (doesFileExist path)
      pure $ case exists of
        Left exception -> Left exception
        Right True -> Left (toException (userError ("owned post-commit test path remains after cleanup: " <> path)))
        Right False -> Right ()

    isAsync exception =
      case fromException exception :: Maybe SomeAsyncException of
        Just _ -> True
        Nothing -> False

-- | Metadata inspection remains test-only; it returns closed values and never
-- grants an opened connection or a selected candidate capability.
loadCacheMetaForTest :: FilePath -> IO (Maybe (Map Text Text))
loadCacheMetaForTest = loadCacheMeta

-- | Opaque cleanup plan: tests may inject nullary failures but receive no
-- connection, path observer, or internal hook record.
newtype ExactArchiveCleanupPlan = ExactArchiveCleanupPlan ExactArchiveCleanupHooks

exactArchiveCleanupPlanForTest :: IO () -> IO () -> ExactArchiveCleanupPlan
exactArchiveCleanupPlanForTest rollback closeAction =
  ExactArchiveCleanupPlan (exactArchiveCleanupHooksForTest rollback closeAction)

withExactArchiveAliasRepairForTest
  :: IO () -> IO () -> FilePath -> Text -> FilePath
  -> (ExactArchiveCompileFacts -> IO (Either error ()))
  -> IO (Either error (Maybe ExactArchiveCompileFacts))
withExactArchiveAliasRepairForTest observe afterFacts archive target alias repair =
  withExactArchiveAliasRepairWith (const observe) afterFacts archive target alias repair

withExactArchiveAliasRepairWithCleanupForTest
  :: ExactArchiveCleanupPlan -> IO () -> IO () -> FilePath -> Text -> FilePath
  -> (ExactArchiveCompileFacts -> IO (Either error ()))
  -> IO (Either error (Maybe ExactArchiveCompileFacts))
withExactArchiveAliasRepairWithCleanupForTest (ExactArchiveCleanupPlan hooks) observe afterFacts archive target alias repair =
  withExactArchiveAliasRepairWithCleanupHooks hooks (const observe) afterFacts archive target alias repair

readExactArchiveAliasStatusForTest
  :: IO () -> IO () -> FilePath -> Text -> FilePath
  -> IO (Maybe ExactArchiveAliasStatus)
readExactArchiveAliasStatusForTest observe afterFacts archive target alias =
  readExactArchiveAliasStatusWith (const observe) afterFacts archive target alias
