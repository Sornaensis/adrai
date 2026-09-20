{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Process-local single-flight coordination for immutable exact revisions.
-- Producers publish through the existing cache machinery; this coordinator
-- only shares completion and never shares a mutable SQLite connection.
module Adrai.Service.Compilation
  ( CompilationCoordinator,
    CompiledArtifact (..),
    newCompilationCoordinator,
    acquireExactCompilation,
    acquireExactCompilationObserved,
    stopCompilationCoordinator,
  )
where

import Adrai.Git (GitOid, Repository (..))
import Control.Concurrent.Async (Async, asyncWithUnmask, cancel, waitCatch)
import Control.Concurrent.STM
import Control.Exception (SomeAsyncException, SomeException, fromException, mask, throwIO, try)
import Control.Monad (forM_, void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

data CompilationKey = CompilationKey FilePath FilePath GitOid
  deriving (Eq, Ord)

data CompiledArtifact = CompiledArtifact
  { compiledArtifactRevision :: GitOid,
    compiledArtifactDatabase :: FilePath
  }
  deriving (Eq, Show)

data Flight = Flight
  { flightResult :: TMVar (Either SomeException (Either Text CompiledArtifact)),
    flightWorker :: TMVar (Either SomeException (Async ()))
  }

data CoordinatorState = CoordinatorState
  { coordinatorStopped :: Bool,
    coordinatorFlights :: Map CompilationKey Flight
  }

newtype CompilationCoordinator = CompilationCoordinator (TVar CoordinatorState)

newCompilationCoordinator :: IO CompilationCoordinator
newCompilationCoordinator = CompilationCoordinator <$> newTVarIO (CoordinatorState False Map.empty)

-- | Join or elect one producer for a repository binding and exact commit.
-- A cancelled waiter detaches without affecting the owned producer.
acquireExactCompilation
  :: CompilationCoordinator
  -> Repository
  -> GitOid
  -> IO (Either Text CompiledArtifact)
  -> IO (Either Text CompiledArtifact)
acquireExactCompilation coordinator repository revision produce =
  acquireExactCompilationObserved coordinator repository revision (pure ()) produce

acquireExactCompilationObserved
  :: CompilationCoordinator
  -> Repository
  -> GitOid
  -> IO ()
  -> IO (Either Text CompiledArtifact)
  -> IO (Either Text CompiledArtifact)
acquireExactCompilationObserved coordinator@(CompilationCoordinator state) repository revision joined produce = mask $ \restore -> do
  elected <- atomically $ do
    current <- readTVar state
    if coordinatorStopped current
      then pure (Left "compilation coordinator is stopping")
      else case Map.lookup key (coordinatorFlights current) of
        Just flight -> pure (Right (False, flight))
        Nothing
          | Map.size (coordinatorFlights current) >= maximumFlights -> pure (Left "compilation queue is full")
          | otherwise -> do
              result <- newEmptyTMVar
              worker <- newEmptyTMVar
              let flight = Flight result worker
              writeTVar state current {coordinatorFlights = Map.insert key flight (coordinatorFlights current)}
              pure (Right (True, flight))
  case elected of
    Left problem -> pure (Left problem)
    Right (isProducer, flight) -> do
      if isProducer
        then do
          created <- try @SomeException (asyncWithUnmask (\unmask -> runProducer coordinator key flight (unmask produce)))
          atomically $ do
            putTMVar (flightWorker flight) created
            case created of
              Left failure -> do
                void (tryPutTMVar (flightResult flight) (Left failure))
                modifyTVar' state (\current -> current {coordinatorFlights = Map.delete key (coordinatorFlights current)})
              Right _ -> pure ()
        else pure ()
      restore joined
      outcome <- restore (atomically (readTMVar (flightResult flight)))
      case outcome of
        Left exception -> case fromException exception of
          Just cancellation -> throwIO (cancellation :: SomeAsyncException)
          Nothing -> throwIO exception
        Right result -> pure result
  where
    key = CompilationKey (repositoryCommonDir repository) (repositoryCommandDirectory repository) revision
    maximumFlights = 8

runProducer :: CompilationCoordinator -> CompilationKey -> Flight -> IO (Either Text CompiledArtifact) -> IO ()
runProducer (CompilationCoordinator state) key flight produce = mask $ \_ -> do
  outcome <- try @SomeException produce
  atomically $ do
    void (tryPutTMVar (flightResult flight) outcome)
    modifyTVar' state (\current -> current {coordinatorFlights = Map.delete key (coordinatorFlights current)})

stopCompilationCoordinator :: CompilationCoordinator -> IO ()
stopCompilationCoordinator (CompilationCoordinator state) = mask $ \restore -> do
  flights <- atomically $ do
    current <- readTVar state
    writeTVar state current {coordinatorStopped = True}
    pure (Map.elems (coordinatorFlights current))
  workers <- atomically (mapM (readTMVar . flightWorker) flights)
  forM_ workers (either (const (pure ())) cancel)
  forM_ workers (either (const (pure ())) (void . restore . waitCatch))
