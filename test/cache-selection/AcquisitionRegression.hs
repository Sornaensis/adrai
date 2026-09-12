{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module AcquisitionRegression (tests) where

import Adrai.Compiler.CacheSelection.TestSupport
  ( PublicationRefreshAcquisitionException (..),
    PublicationRefreshAcquisitionFault (..),
    PublicationRefreshAcquisitionObservation (..),
    PublicationRefreshCleanupArtifact (..),
    PublicationRefreshCleanupRecord (..),
    observePublicationRefreshAcquisitionForTest,
  )
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (AsyncException (ThreadKilled), SomeException, fromException, throwTo, try)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import System.FilePath (takeDirectory, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: (forall result. (FilePath -> IO result) -> IO result) -> TestTree
tests withSeedArchive =
  testGroup
    "publication refresh acquisition cleanup"
    [ testCase "successful acquisition preserves inputs and releases its candidate" $
        withSeedArchive assertHealthyAcquisition,
      testCase "synchronous acquisition failures clean every owned artifact" $
        withSeedArchive assertSynchronousFailures,
      testCase "synchronized acquisition cancellation preserves ThreadKilled" $
        withSeedArchive (assertCancellation PublicationRefreshAcquisitionAwaitCancellation False),
      testCase "cleanup failure does not mask cancellation or skip remaining cleanup" $
        withSeedArchive (assertCancellation PublicationRefreshAcquisitionAwaitCancellationOverCleanupFailure True)
    ]

assertHealthyAcquisition :: FilePath -> IO ()
assertHealthyAcquisition seed = withSentinel seed $ \sentinel -> do
  (initiating, observation) <-
    observePublicationRefreshAcquisitionForTest
      seed
      sentinel
      PublicationRefreshAcquisitionHealthy
      (pure ())
  case initiating of
    Nothing -> pure ()
    Just exception -> assertFailure ("healthy acquisition returned an exception: " <> show exception)
  assertPhaseCounts "healthy acquisition" (1, 1, 1) observation
  publicationRefreshAcquisitionPartialCopyCreated observation @?= False
  publicationRefreshAcquisitionCleanupHandleAttempted observation @?= False
  publicationRefreshAcquisitionCleanupHandleSucceeded observation @?= False
  publicationRefreshAcquisitionCleanupFailureObserved observation @?= False
  assertCompleteCleanup "healthy acquisition" observation
  assertInputsPreserved "healthy acquisition" observation

assertSynchronousFailures :: FilePath -> IO ()
assertSynchronousFailures seed =
  forM_ cases $ \(label, fault, expectedException, expectedCounts, expectedPartial, expectedHandleCleanup) ->
    withSentinel seed $ \sentinel -> do
      (initiating, observation) <-
        observePublicationRefreshAcquisitionForTest seed sentinel fault (pure ())
      case initiating >>= fromException of
        Just actualException -> actualException @?= expectedException
        Nothing -> assertFailure (label <> " did not preserve its initiating exception constructor")
      assertPhaseCounts label expectedCounts observation
      publicationRefreshAcquisitionPartialCopyCreated observation @?= expectedPartial
      publicationRefreshAcquisitionCleanupHandleAttempted observation @?= expectedHandleCleanup
      publicationRefreshAcquisitionCleanupHandleSucceeded observation @?= expectedHandleCleanup
      publicationRefreshAcquisitionCleanupFailureObserved observation @?= False
      assertCompleteCleanup label observation
      assertInputsPreserved label observation
  where
    cases =
      [ ( "close failure",
          PublicationRefreshAcquisitionCloseFailure,
          PublicationRefreshAcquisitionCloseException,
          (1, 0, 0),
          False,
          True
        ),
        ( "reservation removal failure",
          PublicationRefreshAcquisitionRemoveFailure,
          PublicationRefreshAcquisitionRemoveException,
          (1, 1, 0),
          False,
          False
        ),
        ( "copy failure",
          PublicationRefreshAcquisitionCopyFailure,
          PublicationRefreshAcquisitionCopyException,
          (1, 1, 1),
          False,
          False
        ),
        ( "partial copy failure",
          PublicationRefreshAcquisitionPartialCopyFailure,
          PublicationRefreshAcquisitionCopyException,
          (1, 1, 1),
          True,
          False
        )
      ]

assertCancellation :: PublicationRefreshAcquisitionFault -> Bool -> FilePath -> IO ()
assertCancellation fault expectCleanupFailure seed = withSentinel seed $ \sentinel -> do
  reached <- newEmptyMVar
  never <- newEmptyMVar
  result <- newEmptyMVar
  worker <- forkIO $ do
    outcome <- try @SomeException $
      observePublicationRefreshAcquisitionForTest
        seed
        sentinel
        fault
        (putMVar reached () >> takeMVar never)
    putMVar result outcome
  takeMVar reached
  throwTo worker ThreadKilled
  outcome <- takeMVar result
  (initiating, observation) <- case outcome of
    Right value -> pure value
    Left exception -> assertFailure ("the acquisition observer leaked its initiating exception: " <> show exception) >> fail "unreachable"
  case initiating >>= fromException of
    Just ThreadKilled -> pure ()
    _ -> assertFailure "acquisition cancellation did not preserve ThreadKilled"
  assertPhaseCounts "cancelled acquisition" (1, 1, 1) observation
  publicationRefreshAcquisitionPartialCopyCreated observation @?= False
  publicationRefreshAcquisitionCleanupHandleAttempted observation @?= False
  publicationRefreshAcquisitionCleanupHandleSucceeded observation @?= False
  publicationRefreshAcquisitionCleanupFailureObserved observation @?= expectCleanupFailure
  assertCompleteCleanup "cancelled acquisition" observation
  let records = publicationRefreshAcquisitionCleanupRecords observation
  if expectCleanupFailure
    then do
      map publicationRefreshCleanupArtifactSucceeded records @?= [False, True, True, True]
      assertBool "cleanup failure must not skip later artifact cleanup" (all publicationRefreshCleanupArtifactAttempted records)
    else assertBool "ordinary cancellation cleanup succeeds for every artifact" (all publicationRefreshCleanupArtifactSucceeded records)
  assertInputsPreserved "cancelled acquisition" observation

assertPhaseCounts :: String -> (Int, Int, Int) -> PublicationRefreshAcquisitionObservation -> IO ()
assertPhaseCounts label expected observation = do
  assertBool (label <> " did not reserve ownership") (publicationRefreshAcquisitionReserved observation)
  ( publicationRefreshAcquisitionCloseAttempts observation,
    publicationRefreshAcquisitionRemoveAttempts observation,
    publicationRefreshAcquisitionCopyAttempts observation
    )
    @?= expected

assertCompleteCleanup :: String -> PublicationRefreshAcquisitionObservation -> IO ()
assertCompleteCleanup label observation = do
  let records = publicationRefreshAcquisitionCleanupRecords observation
  map publicationRefreshCleanupArtifact records
    @?= [ PublicationRefreshMainFile,
          PublicationRefreshJournalFile,
          PublicationRefreshWalFile,
          PublicationRefreshShmFile
        ]
  assertBool (label <> " did not attempt cleanup for every owned artifact") (all publicationRefreshCleanupArtifactAttempted records)
  assertBool (label <> " left an owned artifact behind") (all publicationRefreshCleanupArtifactAbsent records)
  assertBool (label <> " left the candidate behind") (publicationRefreshAcquisitionCandidateAbsent observation)
  assertBool (label <> " left a SQLite sidecar behind") (publicationRefreshAcquisitionSidecarsAbsent observation)

assertInputsPreserved :: String -> PublicationRefreshAcquisitionObservation -> IO ()
assertInputsPreserved label observation = do
  assertBool (label <> " changed seed bytes") (publicationRefreshAcquisitionSeedBytesPreserved observation)
  assertBool (label <> " changed seed metadata") (publicationRefreshAcquisitionSeedMetadataPreserved observation)
  assertBool (label <> " changed the unrelated sentinel") (publicationRefreshAcquisitionSentinelPreserved observation)

withSentinel :: FilePath -> (FilePath -> IO result) -> IO result
withSentinel seed action = do
  let sentinel = takeDirectory seed </> "publication-refresh-unrelated.sentinel"
  BS.writeFile sentinel "unrelated sentinel"
  action sentinel
