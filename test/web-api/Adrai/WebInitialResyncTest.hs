{-# LANGUAGE OverloadedStrings #-}

module Adrai.WebInitialResyncTest (failedSnapshotRecovery, publicationDuringSubscription) where

import qualified Adrai.Web.Api as Api
import qualified Adrai.Web.Application as Application
import qualified Adrai.Web.Events as Events
import qualified Adrai.Web.Security as Security
import qualified Adrai.Web.Watch as Watch
import Adrai.Web.Socket (unavailableEventsTransport)
import qualified Adrai.WebServerTest as Server
import Adrai.Git (Repository, RevisionSpec (..), discoverRepository, resolveRevision, systemGit)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Concurrent.Async (waitCatch, withAsync)
import Control.Exception (bracket, finally)
import Control.Monad (when)
import qualified Data.ByteString as ByteString
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

failedSnapshotRecovery :: TestTree
failedSnapshotRecovery = testCase "initial event reports watcher failure and a later reconnect observes recovery" testFailedSnapshotRecovery

publicationDuringSubscription :: TestTree
publicationDuringSubscription = testCase "initial event retries when publication lands between watcher sample and registration" testPublicationDuringSubscription

testFailedSnapshotRecovery :: IO ()
testFailedSnapshotRecovery = do
  bounded <- timeout 20000000 $ Server.withSeededRepository $ \root ->
    withRuntime root $ \runtime observer repository bound -> do
      let config = root </> ".adrai.toml"
      existed <- doesFileExist config
      original <- if existed then Just <$> ByteString.readFile config else pure Nothing
      let restore = case original of
            Just bytes -> ByteString.writeFile config bytes
            Nothing -> doesFileExist config >>= \present -> when present (removeFile config)
      bracket (pure ()) (const restore) $ \_ -> do
        ByteString.writeFile config "[unterminated"
        failed <- Watch.repositorySnapshot observer bound
        case failed of
          Watch.RepositorySnapshotFailed _ _ -> pure ()
          _ -> assertFailure "invalid configuration did not fail the real watcher snapshot"
        unavailable <- withInitial runtime (Watch.repositorySnapshot observer) $ \initial -> do
          assertBool "the first full-facts event reports watcher observation failure"
            (Events.eventAsOf initial == Events.EventAsOfUnavailable "repository-observation-failed" && fullFacts initial)
          pure (Events.eventGeneration initial)
        restore
        oid <- resolveRevision repository (RevisionSpec "HEAD") >>= either (const (assertFailure "healthy fixture HEAD failed")) pure
        withInitial runtime (Watch.repositorySnapshot observer) $ \initial ->
          assertBool "reconnect after repair has a healthy exact full-facts initial event"
            (Events.eventAsOf initial == Events.EventAt oid && fullFacts initial
              && Events.eventGeneration initial > unavailable)
  case bounded of
    Nothing -> assertFailure "watcher-health initial resync exceeded 20 seconds"
    Just () -> pure ()

testPublicationDuringSubscription :: IO ()
testPublicationDuringSubscription = do
  bounded <- timeout 20000000 $ Server.withSeededRepository $ \root ->
    withRuntime root $ \runtime observer repository _bound -> do
      oid <- resolveRevision repository (RevisionSpec "HEAD") >>= either (const (assertFailure "publication fixture HEAD failed")) pure
      sampled <- newEmptyMVar
      release <- newEmptyMVar
      attempts <- newIORef (0 :: Int)
      let takeSnapshot bound = do
            snapshot <- Watch.repositorySnapshot observer bound
            number <- atomicModifyIORef' attempts (\count -> let next = count + 1 in (next, next))
            when (number == 1) (putMVar sampled () >> takeMVar release)
            pure snapshot
      withAsync (Application.subscribeApplicationEvents runtime takeSnapshot) $ \worker ->
        (do
          reached <- timeout 3000000 (takeMVar sampled)
          assertBool "the first real watcher snapshot completed before registration" (reached == Just ())
          published <- Events.publishInvalidation (Application.applicationEventCoordinator runtime)
            (Events.EventAsOfUnavailable "test-interleaving") [minBound .. maxBound]
          putMVar release ()
          result <- timeout 8000000 (waitCatch worker)
          subscriber <- case result of
            Just (Right (Right active)) -> pure active
            _ -> assertFailure "subscription did not retry the intervening publication"
          bracket (pure subscriber) (Events.unregisterSubscriber (Application.applicationEventCoordinator runtime)) $ \active -> do
            initial <- readInitial active
            assertBool "new initial generation follows the intervening publication"
              (Events.eventGeneration initial > Events.eventGeneration published)
            assertBool "new initial event is healthy and full at the exact OID"
              (Events.eventAsOf initial == Events.EventAt oid && fullFacts initial)
          count <- readIORef attempts
          assertBool "an intervening publication forced a fresh watcher sample" (count == 2)
        ) `finally` do
          _ <- tryPutMVar release ()
          pure ()
  case bounded of
    Nothing -> assertFailure "interleaved publication initial resync exceeded 20 seconds"
    Just () -> pure ()

withRuntime :: FilePath -> (Application.ApplicationRuntime -> Watch.Observer -> Repository -> Api.Repo -> IO a) -> IO a
withRuntime root use = do
  repository <- discoverRepository systemGit root >>= either (const (assertFailure "fixture repository discovery failed")) pure
  bound <- either (const (assertFailure "fixture repository binding failed")) pure (Api.validateRepositoryBinding (Right repository))
  authority <- either (const (assertFailure "fixture authority failed")) pure (Security.mkBoundAuthority 1 "initial-resync")
  secret <- either (const (assertFailure "fixture secret failed")) pure (Security.mkProcessSecret (ByteString.replicate 32 7))
  bracket
    (Application.newApplicationRuntime bound authority secret Api.defaultApiLimits Application.defaultApplicationServices unavailableEventsTransport)
    Application.stopApplicationRuntime $ \runtime -> do
      observer <- Watch.observerForRegistry (Application.applicationActiveFileRegistry runtime) bound
      use runtime observer repository bound

withInitial :: Application.ApplicationRuntime -> (Api.Repo -> IO Watch.RepositorySnapshot) -> (Events.EventEnvelope -> IO a) -> IO a
withInitial runtime takeSnapshot use = do
  result <- Application.subscribeApplicationEvents runtime takeSnapshot
  subscriber <- either (const (assertFailure "health-aware subscription was rejected")) pure result
  bracket (pure subscriber) (Events.unregisterSubscriber (Application.applicationEventCoordinator runtime)) $ \active ->
    readInitial active >>= use

readInitial :: Events.EventSubscriber -> IO Events.EventEnvelope
readInitial subscriber = do
  observed <- timeout 3000000 (Events.readSubscriberEvent subscriber)
  case observed of
    Just (Events.SubscriberEvent envelope) -> pure envelope
    _ -> assertFailure "initial full-facts event was unavailable"

fullFacts :: Events.EventEnvelope -> Bool
fullFacts envelope = case Events.eventPayload envelope of
  Events.RepositoryInvalidated invalidations -> invalidations == [minBound .. maxBound]
  Events.RepositoryObservationFailed _ -> False
