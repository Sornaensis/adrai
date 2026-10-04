{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

-- | Cross-process mutation lock at @<git-common-dir>/adrai.lock@.
--
-- The pathname is deliberately persistent.  Ownership is the native open
-- handle, held for the complete
-- protected action.  Consequently neither stale recovery nor release ever
-- renames or deletes the pathname: an old, unheld file is simply claimed and
-- rewritten while the native lock is held.
module Adrai.Provenance.Git.Lock
  ( GitLockError (..),
    GitLockInfo (..),
    GitLock (..),
    GitLockDependencies (GitLockDependencies),
    GitLockCloseOperation (..),
    acquireGitLock,
    acquireGitLockWith,
    releaseGitLock,
    withGitLock,
    withGitLockWith,
    gitLockStatus,
    gitLockStatusWith,
  )
where

import Adrai.Git (Repository (..), repositoryCommonDir)
import Adrai.Provenance.Git.Lock.Native
  ( NativeLock,
    getMyPid,
    nativeCloseCompleted,
    nativeFailureIsContention,
    nativeFailureIsMissing,
    nativeStatusNeedsOpen,
    openExistingNative,
    openOwnedNative,
    releaseNative,
    writeOwnedNative,
  )

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Exception (Exception, IOException, SomeAsyncException, SomeException, fromException, mask, throwIO, try, uninterruptibleMask_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

data GitLockError
  = LockHeld FilePath Int
  | LockFailed FilePath Text
  | LockParseError Text
  deriving (Eq, Show)

instance Exception GitLockError

data GitLockInfo
  = GitLockInfo
      { lockPath :: FilePath,
        lockPid :: Maybe Int,
        lockLive :: Bool,
        lockStale :: Bool
      }
  deriving (Eq, Show)

-- | Typed pre-close hook for narrowly testable cleanup boundaries.  Hooks can
-- fail or cancel before ownership changes, but never receive a native handle
-- or close action: production performs the actual close exactly once.
data GitLockCloseOperation
  = CloseStatusProbe
  | CloseOwnerRelease
  deriving (Eq, Show)

newtype GitLockDependencies = GitLockDependencies
  { beforeNativeClose :: FilePath -> GitLockCloseOperation -> IO ()
  }

defaultGitLockDependencies :: GitLockDependencies
defaultGitLockDependencies = GitLockDependencies (\_ _ -> pure ())

newtype ReservationToken = ReservationToken {unReservationToken :: Int}
  deriving (Eq, Show)

-- | The native owner remains open until 'releaseGitLock'.  The integer field
-- is the opaque local ownership generation.  The constructor and selectors
-- intentionally retain their established three-field public surface.
data GitLock
  = GitLock
      { gitLockPath :: FilePath,
        gitLockFd :: Int,
        gitLockPid :: Int
      }
  deriving (Eq, Show)

lockFileName :: String
lockFileName = "adrai.lock"

lockFilePathFor :: FilePath -> FilePath
lockFilePathFor directory = directory </> lockFileName

-- | A native handle/descriptor is the cross-process authority. This registry
-- serializes same-process acquire and status probes.  Each reservation has an
-- unforgeable-for-this-process generation token, so a late/double release of
-- an old lock cannot clear a newer owner's reservation for the same pathname.
data LocalReservations = LocalReservations
  { nextReservation :: Int,
    reservationsByPath :: Map.Map FilePath ReservationToken,
    ownersByKey :: Map.Map LockKey OwnerState,
    probesByPath :: Map.Map FilePath (ReservationToken, ProbeState)
  }

data LockKey = LockKey FilePath Int Int
  deriving (Eq, Ord)

data OwnerState
  = OwnerAvailable NativeLock GitLockDependencies
  | OwnerClosing

data ProbeState
  = ProbeBlocked NativeLock
  | ProbeClosing

localReservations :: MVar LocalReservations
localReservations = unsafePerformIO (newMVar (LocalReservations 1 Map.empty Map.empty Map.empty))
{-# NOINLINE localReservations #-}

reserveLocal :: FilePath -> IO (Maybe ReservationToken)
reserveLocal path =
  modifyMVar localReservations $ \reservations ->
    case Map.lookup path (reservationsByPath reservations) of
      Just _ -> pure (reservations, Nothing)
      Nothing ->
        let token = ReservationToken (nextReservation reservations)
            advanced = reservations {nextReservation = nextReservation reservations + 1, reservationsByPath = Map.insert path token (reservationsByPath reservations)}
         in pure (advanced, Just token)

releaseLocal :: FilePath -> ReservationToken -> IO ()
releaseLocal path token =
  uninterruptibleMask_ $
    modifyMVar_ localReservations $ \reservations ->
      pure $ case Map.lookup path (reservationsByPath reservations) of
        Just current | current == token -> reservations {reservationsByPath = Map.delete path (reservationsByPath reservations)}
        _ -> reservations

registerOwner :: GitLockDependencies -> FilePath -> Int -> ReservationToken -> NativeLock -> IO GitLock
registerOwner dependencies path pid token native =
  uninterruptibleMask_ $
    modifyMVar localReservations $ \reservations -> do
      let key = LockKey path (unReservationToken token) pid
          registered = reservations {ownersByKey = Map.insert key (OwnerAvailable native dependencies) (ownersByKey reservations)}
      pure (registered, GitLock path (unReservationToken token) pid)

-- | Atomically remove the native handle from the available state before any
-- close.  A duplicate release can therefore observe only 'OwnerClosing',
-- never the handle that is about to be closed (or recycled by the OS).
claimOwner :: GitLock -> IO (Maybe (NativeLock, GitLockDependencies))
claimOwner lock =
  uninterruptibleMask_ $
    modifyMVar localReservations $ \reservations ->
      let key = lockKey lock
       in case Map.lookup key (ownersByKey reservations) of
            Just (OwnerAvailable native dependencies) ->
              pure
                ( reservations {ownersByKey = Map.insert key OwnerClosing (ownersByKey reservations)},
                  Just (native, dependencies)
                )
            _ -> pure (reservations, Nothing)

completeOwnerRelease :: GitLock -> IO ()
completeOwnerRelease lock =
  uninterruptibleMask_ $
    modifyMVar_ localReservations $ \reservations ->
      let key = lockKey lock
          token = ReservationToken (gitLockFd lock)
          withoutOwner = reservations {ownersByKey = Map.delete key (ownersByKey reservations)}
       in pure $ case Map.lookup (gitLockPath lock) (reservationsByPath withoutOwner) of
            Just current | current == token -> withoutOwner {reservationsByPath = Map.delete (gitLockPath lock) (reservationsByPath withoutOwner)}
            _ -> withoutOwner

restoreOwner :: GitLock -> NativeLock -> GitLockDependencies -> IO ()
restoreOwner lock native dependencies =
  uninterruptibleMask_ $
    modifyMVar_ localReservations $ \reservations ->
      let key = lockKey lock
       in pure $ case Map.lookup key (ownersByKey reservations) of
            Just OwnerClosing -> reservations {ownersByKey = Map.insert key (OwnerAvailable native dependencies) (ownersByKey reservations)}
            _ -> reservations

recordBlockedProbe :: FilePath -> ReservationToken -> NativeLock -> IO ()
recordBlockedProbe path token native =
  uninterruptibleMask_ $
    modifyMVar_ localReservations $ \reservations ->
      pure
        reservations
          { probesByPath = Map.insert path (token, ProbeBlocked native) (probesByPath reservations)
          }

claimBlockedProbe :: FilePath -> IO (Maybe (ReservationToken, NativeLock))
claimBlockedProbe path =
  uninterruptibleMask_ $
    modifyMVar localReservations $ \reservations ->
      case Map.lookup path (probesByPath reservations) of
        Just (token, ProbeBlocked native) ->
          pure
            ( reservations {probesByPath = Map.insert path (token, ProbeClosing) (probesByPath reservations)},
              Just (token, native)
            )
        _ -> pure (reservations, Nothing)

restoreBlockedProbe :: FilePath -> ReservationToken -> NativeLock -> IO ()
restoreBlockedProbe path token native =
  uninterruptibleMask_ $
    modifyMVar_ localReservations $ \reservations ->
      pure $ case Map.lookup path (probesByPath reservations) of
        Just (current, ProbeClosing) | current == token ->
          reservations {probesByPath = Map.insert path (token, ProbeBlocked native) (probesByPath reservations)}
        _ -> reservations

completeBlockedProbe :: FilePath -> ReservationToken -> IO ()
completeBlockedProbe path token =
  uninterruptibleMask_ $
    modifyMVar_ localReservations $ \reservations ->
      let withoutProbe = reservations {probesByPath = Map.delete path (probesByPath reservations)}
       in pure $ case Map.lookup path (reservationsByPath withoutProbe) of
            Just current | current == token -> withoutProbe {reservationsByPath = Map.delete path (reservationsByPath withoutProbe)}
            _ -> withoutProbe

releaseProbeReservationUnlessBlocked :: FilePath -> ReservationToken -> IO ()
releaseProbeReservationUnlessBlocked path token =
  uninterruptibleMask_ $
    modifyMVar_ localReservations $ \reservations ->
      case Map.lookup path (probesByPath reservations) of
        Just (current, _) | current == token -> pure reservations
        _ ->
          pure $ case Map.lookup path (reservationsByPath reservations) of
            Just current | current == token -> reservations {reservationsByPath = Map.delete path (reservationsByPath reservations)}
            _ -> reservations

lockKey :: GitLock -> LockKey
lockKey lock = LockKey (gitLockPath lock) (gitLockFd lock) (gitLockPid lock)

parseLockPid :: BS.ByteString -> Maybe Int
parseLockPid bytes =
  case BS8.unpack bytes of
    'p' : 'i' : 'd' : '=' : rest
      | not (null rest), last rest == '\n' ->
          let digits = init rest
           in case reads digits of
                [(pid, "")]
                  | pid > 0, firstDigit : _ <- digits, firstDigit /= '0', all isAsciiDigit digits -> Just pid
                _ -> Nothing
    _ -> Nothing
  where
    isAsciiDigit character = character >= '0' && character <= '9'

-- | A holder may still be truncating and rewriting its canonical bytes.  That
-- initialization window is contention, not malformed ownership: retry a few
-- times and then return a typed held result with an unknown PID.
readHeld :: FilePath -> IO GitLockError
readHeld path = go (0 :: Int)
  where
    maximumReadRetries = 6
    go attempts = do
      bytes <- try @IOException (BS.readFile path)
      case bytes >>= maybe (Left (userError "noncanonical lock contents")) Right . parseLockPid of
        Right pid -> pure (LockHeld path pid)
        Left _
          | attempts < maximumReadRetries -> threadDelay 2000 >> go (attempts + 1)
          | otherwise -> pure (LockHeld path 0)

lockFailureOrHeld :: FilePath -> IOException -> IO a
lockFailureOrHeld path failure = do
  exists <- doesFileExist path
  if exists && nativeFailureIsContention failure
    then readHeld path >>= throwIO
    else throwIO (LockFailed path (Text.pack (show failure)))

-- | Status probes native ownership.  A persistent but unheld file is not a
-- lock; no PID liveness guess is necessary or authoritative.
gitLockStatus :: Repository -> IO (Either GitLockError (Maybe GitLockInfo))
gitLockStatus = gitLockStatusWith defaultGitLockDependencies

gitLockStatusWith :: GitLockDependencies -> Repository -> IO (Either GitLockError (Maybe GitLockInfo))
gitLockStatusWith dependencies repository = mask $ \restore -> do
  let path = lockFilePathFor (repositoryCommonDir repository)
  reserveLocal path >>= \case
    Nothing -> retryBlockedProbe dependencies path >>= \case
      Just result -> pure result
      Nothing -> restore (Left <$> readHeld path)
    Just token -> statusWithReservation dependencies path token restore

statusWithReservation :: GitLockDependencies -> FilePath -> ReservationToken -> (forall a. IO a -> IO a) -> IO (Either GitLockError (Maybe GitLockInfo))
statusWithReservation dependencies path token restoreAction = do
  outcome <- try @SomeException $ do
    exists <- restoreAction (nativeStatusNeedsOpen path)
    if not exists
      then pure (Right Nothing)
      else do
        probe <- try @IOException (openExistingNative path)
        case probe of
          Left failure
            | nativeFailureIsMissing failure -> pure (Right Nothing)
            | nativeFailureIsContention failure -> restoreAction (Left <$> readHeld path)
            | otherwise -> pure (Left (LockFailed path (Text.pack (show failure))))
          Right native -> closeFreshProbe dependencies path token native
  releaseProbeReservationUnlessBlocked path token
  case outcome of
    Right result -> pure result
    Left failure -> rethrowAsync failure (pure (Left (LockFailed path (Text.pack (show failure)))))

closeFreshProbe :: GitLockDependencies -> FilePath -> ReservationToken -> NativeLock -> IO (Either GitLockError (Maybe GitLockInfo))
closeFreshProbe dependencies path token native = do
  closed <- try @SomeException $ do
    beforeNativeClose dependencies path CloseStatusProbe
    releaseNative native
  case closed of
    Right () -> pure (Right Nothing)
    Left failure -> do
      -- A real Linux close has consumed the descriptor, even when reporting
      -- delayed I/O errors. A pre-close hook still retains native ownership.
      if nativeCloseCompleted failure
        then completeBlockedProbe path token
        else recordBlockedProbe path token native
      rethrowAsync failure (pure (Left (LockFailed path (Text.pack (show failure)))))

retryBlockedProbe :: GitLockDependencies -> FilePath -> IO (Maybe (Either GitLockError (Maybe GitLockInfo)))
retryBlockedProbe dependencies path =
  claimBlockedProbe path >>= \case
    Nothing -> pure Nothing
    Just (token, native) -> do
      closed <- try @SomeException $ do
        beforeNativeClose dependencies path CloseStatusProbe
        releaseNative native
      case closed of
        Right () -> completeBlockedProbe path token >> pure (Just (Right Nothing))
        Left failure -> do
          if nativeCloseCompleted failure
            then completeBlockedProbe path token
            else restoreBlockedProbe path token native
          rethrowAsync failure (pure (Just (Left (LockFailed path (Text.pack (show failure))))) )


-- | Acquire native authority first, then truncate and write canonical owner
-- bytes while that authority remains held.  An unheld stale or malformed
-- persistent file is therefore safely recovered without path deletion.
acquireGitLock :: Repository -> IO GitLock
acquireGitLock = acquireGitLockWith defaultGitLockDependencies

acquireGitLockWith :: GitLockDependencies -> Repository -> IO GitLock
acquireGitLockWith dependencies repository = mask $ \restore -> do
  let path = lockFilePathFor (repositoryCommonDir repository)
  pid <- getMyPid
  reserveLocal path >>= \case
    Nothing -> readHeld path >>= throwIO
    Just token -> acquireReserved restore path pid token
  where
    acquireReserved restoreAction path pid token = do
      acquired <- try @SomeException (openOwnedNative path)
      native <- case acquired of
        Right owner -> pure owner
        Left failure -> do
          releaseLocal path token
          rethrowAsync failure $
            case fromException failure of
              Just ioFailure -> lockFailureOrHeld path ioFailure
              Nothing -> throwIO (LockFailed path (Text.pack (show failure)))
      writeResult <- try @SomeException (restoreAction (writeOwnedNative path native pid))
      case writeResult of
        Left failure -> closeAfterFailedWrite dependencies path token native failure
        Right () -> registerOwner dependencies path pid token native

-- | Release native ownership but retain the canonical file.  A close/unlock
-- failure is intentionally surfaced to 'withGitLock' after a successful
-- action; an action exception still takes precedence there.
releaseGitLock :: GitLock -> IO ()
releaseGitLock lock = mask $ \_ -> do
  claimOwner lock >>= \case
    -- A stale public value has no registry ownership and therefore cannot
    -- close or clear a successor that happens to share its pathname/PID.
    Nothing -> pure ()
    Just (native, dependencies) -> do
      released <- try @SomeException $ do
        beforeNativeClose dependencies (gitLockPath lock) CloseOwnerRelease
        releaseNative native
      case released of
        Left failure -> do
          if nativeCloseCompleted failure
            then completeOwnerRelease lock
            else restoreOwner lock native dependencies
          rethrowAsync failure (throwIO (LockFailed (gitLockPath lock) (Text.pack (show failure))))
        Right () -> completeOwnerRelease lock

-- | A cancellation from an interruptible operation must retain its original
-- identity; translating it to 'LockFailed' would make cancellation look like
-- ordinary lock contention.
rethrowAsync :: SomeException -> IO a -> IO a
rethrowAsync failure fallback =
  case fromException failure :: Maybe SomeAsyncException of
    Just asyncFailure -> throwIO asyncFailure
    Nothing -> fallback

-- | Close a partially acquired native owner while masked.  A successful close
-- permits release of this exact reservation token.  If close itself fails the
-- token remains reserved when ownership could not be conclusively released.
-- A consumed Linux descriptor is never restored or closed a second time.
closeAfterFailedWrite :: GitLockDependencies -> FilePath -> ReservationToken -> NativeLock -> SomeException -> IO a
closeAfterFailedWrite dependencies path token native writeFailure = do
  closed <- try @SomeException $ do
    beforeNativeClose dependencies path CloseOwnerRelease
    releaseNative native
  case closed of
    Right () -> do
      releaseLocal path token
      throwIO writeFailure
    Left closeFailure -> do
      if nativeCloseCompleted closeFailure then releaseLocal path token else pure ()
      rethrowAsync writeFailure $
        throwIO
          ( LockFailed
              path
              ( Text.pack
                  ( "lock write failed: " <> show writeFailure
                      <> "; cleanup close failed: " <> show closeFailure
                  )
              )
          )

withGitLock :: Repository -> IO a -> IO a
withGitLock repository action = withGitLockWith defaultGitLockDependencies repository (const action)

-- | Dependency-scoped variant used by lock lifecycle tests.  Existing callers
-- use 'withGitLock'; the acquired public value lets a test retry a deliberately
-- retained pre-close failure without exposing the native handle.
withGitLockWith :: GitLockDependencies -> Repository -> (GitLock -> IO a) -> IO a
withGitLockWith dependencies repository action = mask $ \restore -> do
  lock <- acquireGitLockWith dependencies repository
  actionResult <- try @SomeException (restore (action lock))
  cleanupResult <- try @SomeException (releaseGitLock lock)
  case actionResult of
    Left original -> throwIO original
    Right value -> case cleanupResult of
      Left cleanupFailure -> throwIO cleanupFailure
      Right () -> pure value

