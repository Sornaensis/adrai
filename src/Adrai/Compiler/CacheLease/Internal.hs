{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

-- | Internal, scoped ownership for a private validated cache copy.
--
-- This module intentionally exposes no raw source-path accessor.  A caller can
-- register an acquired private resource, accept its facts once validation has
-- completed, and then use only the checked copy/seed operations during the
-- rank-2 scope.  Integration with cache selection is deliberately separate.
module Adrai.Compiler.CacheLease.Internal
  ( CacheLease,
    LeaseAcceptedFacts (..),
    CacheLeaseLifecycle (..),
    CacheLeaseError (..),
    withCacheLeaseScope,
    withCacheLeaseScopeEither,
    beginCacheLeasePreparation,
    discardCacheLeasePreparation,
    registerLeaseResource,
    registerAcceptedLeaseSource,
    readLeaseAcceptedFacts,
    copyLeaseSourceTo,
    seedRegisteredOperationsFromLease,
  )
where

import Adrai.Provenance.Ensure (seedRegisteredOperationsFromSemanticCache)
import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, withMVar)
import Control.Exception (AsyncException, Exception, SomeAsyncException, SomeException, fromException, mask, throwIO, try)
import Data.Either (partitionEithers)
import Data.Maybe (isJust)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import System.Directory (copyFile)

-- | Pathless authority yielded by full validation.  The private source path is
-- retained by the owner and never appears in this value.
data LeaseAcceptedFacts = LeaseAcceptedFacts
  { leaseAcceptedSourceRevision :: !Text,
    leaseAcceptedCacheKey :: !Text,
    leaseAcceptedMetadata :: !(Map Text Text),
    leaseAcceptedPrivateBytes :: !Integer
  }
  deriving (Eq, Show)

data CacheLeaseLifecycle
  = LeaseVacant
  | LeasePreparing
  | LeaseOpen
  | LeaseRetiring
  | LeaseClosed
  deriving (Eq, Show)

data CacheLeaseError
  = CacheLeaseInvalidTransition !CacheLeaseLifecycle
  | CacheLeaseNotOpen !CacheLeaseLifecycle
  | CacheLeaseSourceAlreadyRegistered
  | CacheLeaseSourceMissing
  | CacheLeaseWrongOwner
  deriving (Eq, Show)

instance Exception CacheLeaseError

data LeaseOwner = LeaseOwner
  { leaseLifecycle :: !CacheLeaseLifecycle,
    leaseSourcePath :: !(Maybe FilePath),
    leaseFacts :: !(Maybe LeaseAcceptedFacts),
    leaseCleanup :: ![IO ()]
  }

-- | The phantom scope prevents a lease value from escaping its owner bracket.
-- State and operation gates are separate: retirement irreversibly removes the
-- Open state before it waits for in-flight operations to finish.
data CacheLease scope = CacheLease
  { cacheLeaseThread :: !ThreadId,
    cacheLeaseOwner :: !(MVar LeaseOwner),
    cacheLeaseOperationGate :: !(MVar ())
  }

withCacheLeaseScope :: (forall scope. CacheLease scope -> IO a) -> IO a
withCacheLeaseScope use = mask $ \restore -> do
  ownerThread <- myThreadId
  owner <- newMVar (LeaseOwner LeaseVacant Nothing Nothing [])
  operationGate <- newMVar ()
  let lease = CacheLease ownerThread owner operationGate
  outcome <- try @SomeException (restore (use lease))
  retired <- try @SomeException (retireCacheLease lease)
  resolveLeaseOutcome outcome retired

-- | Typed callback failures are ordinary selection outcomes: they retain their
-- exact 'Left' value over a synchronous retirement fault.  Cancellation during
-- retirement remains higher priority and is rethrown unchanged.
withCacheLeaseScopeEither :: (forall scope. CacheLease scope -> IO (Either error a)) -> IO (Either error a)
withCacheLeaseScopeEither use = mask $ \restore -> do
  ownerThread <- myThreadId
  owner <- newMVar (LeaseOwner LeaseVacant Nothing Nothing [])
  operationGate <- newMVar ()
  let lease = CacheLease ownerThread owner operationGate
  outcome <- try @SomeException (restore (use lease))
  retired <- try @SomeException (retireCacheLease lease)
  case outcome of
    Left exception -> resolveLeaseOutcome (Left exception) retired
    Right typed ->
      case typed of
        Left problem ->
          case retired of
            Left exception | isLeaseAsync exception -> rethrowLeaseAsync exception >> throwIO exception
            _ -> pure (Left problem)
        Right value -> Right <$> resolveLeaseOutcome (Right value) retired

beginCacheLeasePreparation :: CacheLease scope -> IO ()
beginCacheLeasePreparation lease = do
  ensureLeaseOwner lease
  modifyMVar_ (cacheLeaseOwner lease) $ \owner ->
    case leaseLifecycle owner of
      LeaseVacant -> pure owner {leaseLifecycle = LeasePreparing}
      lifecycle -> throwIO (CacheLeaseInvalidTransition lifecycle)

-- | Discard one invalid private candidate while it is still preparing.  The
-- resources are retired under the same masked, all-attempt cleanup policy as
-- final scope retirement, then the owner becomes Vacant for the next ranked
-- candidate.  An accepted Open lease is never reset into a fallback path.
discardCacheLeasePreparation :: CacheLease scope -> IO ()
discardCacheLeasePreparation lease = do
  ensureLeaseOwner lease
  resetLeasePreparation lease

-- | Register cleanup immediately after acquiring any owned resource.  It is
-- called while preparing or open, and retirement attempts every registered
-- cleanup action even after earlier failures.
registerLeaseResource :: CacheLease scope -> IO () -> IO ()
registerLeaseResource lease cleanup = do
  ensureLeaseOwner lease
  modifyMVar_ (cacheLeaseOwner lease) $ \owner ->
    case leaseLifecycle owner of
      LeasePreparing -> pure owner {leaseCleanup = cleanup : leaseCleanup owner}
      LeaseOpen -> pure owner {leaseCleanup = cleanup : leaseCleanup owner}
      lifecycle -> throwIO (CacheLeaseInvalidTransition lifecycle)

-- | Atomically bind the hidden private source to pathless accepted facts.  The
-- source is usable only after this transition reaches Open.
registerAcceptedLeaseSource :: CacheLease scope -> FilePath -> LeaseAcceptedFacts -> IO ()
registerAcceptedLeaseSource lease source facts = do
  ensureLeaseOwner lease
  modifyMVar_ (cacheLeaseOwner lease) $ \owner ->
    case leaseLifecycle owner of
      LeasePreparing
        | isJust (leaseSourcePath owner) -> throwIO CacheLeaseSourceAlreadyRegistered
        | otherwise ->
            pure
              owner
                { leaseLifecycle = LeaseOpen,
                  leaseSourcePath = Just source,
                  leaseFacts = Just facts
                }
      lifecycle -> throwIO (CacheLeaseInvalidTransition lifecycle)

readLeaseAcceptedFacts :: CacheLease scope -> IO LeaseAcceptedFacts
readLeaseAcceptedFacts lease =
  withOpenLease lease $ \owner ->
    case leaseFacts owner of
      Just facts -> pure facts
      Nothing -> throwIO CacheLeaseSourceMissing

-- | Copy only from the registered private source; callers never receive that
-- source path.  The destination remains caller-owned publication state.
copyLeaseSourceTo :: CacheLease scope -> FilePath -> IO ()
copyLeaseSourceTo lease destination =
  withOpenLease lease $ \owner ->
    case leaseSourcePath owner of
      Just source -> copyFile source destination
      Nothing -> throwIO CacheLeaseSourceMissing

-- | Reuse the registered private source for provenance seeding without
-- reopening authority through a public cache pathname.
seedRegisteredOperationsFromLease :: CacheLease scope -> Connection -> IO ([Text], [Text])
seedRegisteredOperationsFromLease lease connection =
  withOpenLease lease $ \owner ->
    case leaseSourcePath owner of
      Just source -> seedRegisteredOperationsFromSemanticCache source connection
      Nothing -> throwIO CacheLeaseSourceMissing

withOpenLease :: CacheLease scope -> (LeaseOwner -> IO a) -> IO a
withOpenLease lease use = do
  ensureLeaseOwner lease
  -- Fast rejection avoids waiting behind a retiring operation.  The second
  -- check closes the race between that read and acquiring the operation gate.
  ensureOpen lease
  withMVar (cacheLeaseOperationGate lease) $ \_ -> do
    owner <- withMVar (cacheLeaseOwner lease) pure
    case leaseLifecycle owner of
      LeaseOpen -> use owner
      lifecycle -> throwIO (CacheLeaseNotOpen lifecycle)

ensureOpen :: CacheLease scope -> IO ()
ensureOpen lease = do
  owner <- withMVar (cacheLeaseOwner lease) pure
  case leaseLifecycle owner of
    LeaseOpen -> pure ()
    lifecycle -> throwIO (CacheLeaseNotOpen lifecycle)

ensureLeaseOwner :: CacheLease scope -> IO ()
ensureLeaseOwner lease = do
  current <- myThreadId
  if current == cacheLeaseThread lease
    then pure ()
    else throwIO CacheLeaseWrongOwner

retireCacheLease :: CacheLease scope -> IO ()
retireCacheLease lease = mask $ \_ -> do
  cleanups <-
    modifyMVar (cacheLeaseOwner lease) $ \owner ->
      case leaseLifecycle owner of
        LeaseClosed -> pure (owner, [])
        LeaseRetiring -> pure (owner, [])
        _ -> pure (owner {leaseLifecycle = LeaseRetiring}, leaseCleanup owner)
  -- State is already non-open before this can wait behind an operation.
  cleanupResults <- withMVar (cacheLeaseOperationGate lease) $ \_ -> traverse (try @SomeException) cleanups
  modifyMVar_ (cacheLeaseOwner lease) $ \owner -> pure owner {leaseLifecycle = LeaseClosed}
  resolveCleanupResults cleanupResults

resetLeasePreparation :: CacheLease scope -> IO ()
resetLeasePreparation lease = mask $ \_ -> do
  cleanups <-
    modifyMVar (cacheLeaseOwner lease) $ \owner ->
      case leaseLifecycle owner of
        LeasePreparing -> pure (owner {leaseLifecycle = LeaseRetiring}, leaseCleanup owner)
        lifecycle -> throwIO (CacheLeaseInvalidTransition lifecycle)
  cleanupResults <- withMVar (cacheLeaseOperationGate lease) $ \_ -> traverse (try @SomeException) cleanups
  modifyMVar_ (cacheLeaseOwner lease) $ \_ ->
    pure (LeaseOwner LeaseVacant Nothing Nothing [])
  resolveCleanupResults cleanupResults

-- | All registered cleanups are attempted.  If cleanup itself is cancelled,
-- retain that async identity; otherwise report the first synchronous cleanup
-- failure after the complete sweep.
resolveCleanupResults :: [Either SomeException ()] -> IO ()
resolveCleanupResults results =
  case firstAsync failures of
    Just exception -> rethrowLeaseAsync exception
    Nothing ->
      case failures of
        exception : _ -> throwIO exception
        [] -> pure ()
  where
    (failures, _) = partitionEithers results

-- | Central precedence rule: the initiating action's async exception wins,
-- then a cleanup async exception, then action failure, then cleanup failure.
resolveLeaseOutcome :: Either SomeException a -> Either SomeException () -> IO a
resolveLeaseOutcome action cleanup =
  case action of
    Left exception
      | isLeaseAsync exception -> rethrowLeaseAsync exception >> throwIO exception
      | otherwise ->
          case cleanup of
            Left cleanupException | isLeaseAsync cleanupException -> rethrowLeaseAsync cleanupException >> throwIO cleanupException
            _ -> throwIO exception
    Right value ->
      case cleanup of
        Left exception -> rethrowLeaseAsync exception >> throwIO exception
        Right () -> pure value

firstAsync :: [SomeException] -> Maybe SomeException
firstAsync = go
  where
    go [] = Nothing
    go (exception : rest)
      | isLeaseAsync exception = Just exception
      | otherwise = go rest

isLeaseAsync :: SomeException -> Bool
isLeaseAsync exception =
  isJust (fromException exception :: Maybe AsyncException)
    || isJust (fromException exception :: Maybe SomeAsyncException)

rethrowLeaseAsync :: SomeException -> IO ()
rethrowLeaseAsync exception =
  case fromException exception :: Maybe AsyncException of
    Just cancellation -> throwIO cancellation
    Nothing ->
      case fromException exception :: Maybe SomeAsyncException of
        Just cancellation -> throwIO cancellation
        Nothing -> pure ()
