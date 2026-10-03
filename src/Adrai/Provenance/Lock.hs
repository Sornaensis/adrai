{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Process-serialize overlay work with a live SQLite writer transaction.
--
-- Prevents concurrent provenance overlay writes by using a dedicated
-- lock database and an uncommitted singleton claim. The connection holds
-- authority until release; process death rolls it back. A committed row from
-- the former protocol has unknown ownership and is never automatically stolen.
--
-- SQLite's busy timeout is configured on every lock connection.  Two
-- processes can otherwise race while creating or reading the lock table
-- before either has established the logical singleton row; that transient
-- database writer contention is retried as lock contention rather than
-- escaping as 'ErrorBusy'.
--
-- The bounded wrapper retains an attempt limit; Runtime's waiting wrapper
-- waits cancellably for live writer authority. Both use the same transaction.
module Adrai.Provenance.Lock
  ( OverlayLock (..),
    acquireOverlayLock,
    acquireOverlayLockWith,
    acquireOverlayLockWithToken,
    releaseOverlayLock,
    releaseOverlayLockWith,
    withOverlayLock,
    withOverlayLockWithReleaseHook,
    withOverlayLockWaiting,
    withOverlayLockWaitingWithRetryHook,
  )
where

import Adrai.Sqlite (asQuery)
import Control.Concurrent (threadDelay)
import Control.Exception
  ( Exception,
    SomeAsyncException,
    SomeException,
    fromException,
    mask,
    mask_,
    throwIO,
    try,
  )
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Int (Int64)
import Data.Time.Clock (getCurrentTime)
import qualified Data.Text as Text
import Database.SQLite.Simple
  ( Connection,
    Only (..),
    SQLData (SQLText),
    SQLError (..),
    Error (ErrorBusy),
    execute,
    execute_,
    open,
    query_,
    close,
  )
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

-- | Handle for an acquired overlay lock.
--
-- Holds the provenance database path (for locating the lock DB) and
-- the open connection so the lock row can be released.
data OverlayLock
  = OverlayLock
  { lockPath         :: FilePath,
    lockConnection   :: Connection,
    lockHolderPid    :: Text.Text,
    lockReleased     :: IORef Bool
  }

-- | Derive the lock database path from the provenance database path.
--
-- Appends @\".lock.sqlite\"@ to the provenance database path.
lockDatabasePath :: FilePath -> FilePath
lockDatabasePath provenanceDb = provenanceDb </> ".lock.sqlite"

-- | Lock table DDL.
--
-- Single-row table: if the row exists, the lock is held.
lockTableDdl :: String
lockTableDdl =
  "CREATE TABLE IF NOT EXISTS overlay_lock(" <>
  "holder_pid TEXT PRIMARY KEY," <>
  "acquired_at TEXT NOT NULL)"

-- | Attempt to acquire the overlay lock.
--
-- Opens the lock database and claims its writer transaction plus singleton.
-- Returns 'Nothing' for contention or an unknown committed owner.
acquireOverlayLock :: FilePath -> IO (Maybe OverlayLock)
acquireOverlayLock provenanceDb = acquireOverlayLockWith provenanceDb (pure ())

-- | Testable acquisition boundary.  The hook runs after the atomic singleton
-- claim and before ownership verification, allowing cancellation cleanup to
-- be proved without weakening the production wrapper.
acquireOverlayLockWith :: FilePath -> IO () -> IO (Maybe OverlayLock)
acquireOverlayLockWith provenanceDb afterClaim = do
  now <- getCurrentTime
  acquireOverlayLockWithToken provenanceDb (Text.pack (show now)) afterClaim

-- | Deterministic token seam for collision tests.  Ownership never depends on
-- token equality; SQLite's connection-local insertion result is authoritative.
acquireOverlayLockWithToken :: FilePath -> Text.Text -> IO () -> IO (Maybe OverlayLock)
acquireOverlayLockWithToken provenanceDb holderPid afterClaim = mask $ \restore -> do
  attempted <- restore (attemptOverlayLock provenanceDb holderPid afterClaim)
  pure $ case attempted of
    OverlayAcquired lock -> Just lock
    OverlayWriterBusy -> Nothing
    OverlayCommittedOwner -> Nothing

data OverlayAcquisition
  = OverlayAcquired OverlayLock
  | OverlayWriterBusy
  | OverlayCommittedOwner

attemptOverlayLock :: FilePath -> Text.Text -> IO () -> IO OverlayAcquisition
attemptOverlayLock provenanceDb holderPid afterClaim = mask $ \restore -> do
  let lockPath' = lockDatabasePath provenanceDb
      lockDir     = takeDirectory lockPath'
  -- Ensure parent directory exists (SQLite needs the directory).
  createDirectoryIfMissing True lockDir

  let acquiredAt = holderPid
  connectionResult <- try @SomeException (open lockPath')
  case connectionResult of
    Left problem
      | isSqliteBusy problem -> pure OverlayWriterBusy
      | otherwise -> throwIO problem
    Right conn -> do
      acquisition <- try @SomeException $ do
        -- This is connection-local.  A brief wait smooths normal writer
        -- hand-off; a remaining busy result becomes an outer retry. Writer
        -- authority, not a committed marker or its token, proves a live claim.
        restore (execute_ conn "PRAGMA busy_timeout=100")
        restore (execute_ conn (asQuery (Text.pack lockTableDdl)))
        restore $ do
          execute_ conn "BEGIN IMMEDIATE"
          -- Use the fixed SQLite rowid as the singleton constraint.  The
          -- insertion and ownership check share the live transaction, which
          -- remains open for the entire protected action.
          execute conn "INSERT OR IGNORE INTO overlay_lock(rowid, holder_pid, acquired_at) VALUES (1, ?, ?)"
            [ SQLText holderPid, SQLText acquiredAt ]
          afterClaim
          inserted <- query_ conn (asQuery "SELECT changes()") :: IO [Only Int64]
          if inserted == [Only 1]
            then pure ()
            else throwIO LockAlreadyHeld
      case acquisition of
        Right () -> do
          released <- newIORef False
          pure (OverlayAcquired (OverlayLock lockPath' conn holderPid released))
        Left problem -> do
          cleanupProblems <- cleanupConnection conn
          rethrowFirstCancellation (problem : cleanupProblems)
          case fromException problem of
             Just LockAlreadyHeld -> case cleanupProblems of
               cleanupProblem : _ -> throwIO cleanupProblem
               [] -> pure OverlayCommittedOwner
             _
               | isSqliteBusy problem -> case cleanupProblems of
                   cleanupProblem : _ -> throwIO cleanupProblem
                   [] -> pure OverlayWriterBusy
               | otherwise -> throwIO problem

-- | Release the owning transaction by rollback and close. Repeated calls are
-- harmless; a genuine first-release
-- failure is reported after every cleanup step has been attempted.
releaseOverlayLock :: OverlayLock -> IO ()
releaseOverlayLock lock = releaseOverlayLockWith lock (pure ())

-- | Testable release boundary. The hook runs after rollback and before close.
-- Cleanup always completes before any
-- synchronous or asynchronous hook failure is rethrown.
releaseOverlayLockWith :: OverlayLock -> IO () -> IO ()
releaseOverlayLockWith lock afterDelete = mask_ $ do
  alreadyReleased <- atomicModifyIORef' (lockReleased lock) (\released -> (True, released))
  if alreadyReleased
    then pure ()
    else do
      deleteResult <- try @SomeException (execute_ (lockConnection lock) "ROLLBACK")
      hookResult <- try @SomeException afterDelete
      closeResult <- try @SomeException (close (lockConnection lock))
      rethrowFirstProblem [problem | Left problem <- [deleteResult, hookResult, closeResult]]

-- | Run an action with the overlay lock held.
--
-- Makes 50 short retries with 100ms delays, then reports LockTimeout. SQLite's
-- brief per-attempt busy wait is additional. Masked cleanup always runs,
-- and cancellation outranks any simultaneous synchronous cleanup failure.
withOverlayLock :: FilePath -> IO a -> IO a
withOverlayLock provenanceDb action =
  withOverlayLockWithReleaseHook provenanceDb action (pure ())

-- | Testable bracket boundary for proving exception priority when both the
-- protected action and release cleanup fail.  Production callers use
-- 'withOverlayLock'.
withOverlayLockWithReleaseHook :: FilePath -> IO a -> IO () -> IO a
withOverlayLockWithReleaseHook provenanceDb action afterDelete =
  withOverlayAcquisition (\restore -> acquireWithRetry restore (50 :: Int)) action afterDelete
  where
    acquireWithRetry restore remaining = do
      candidate <- acquireOverlayLock provenanceDb
      case candidate of
        Just lock -> pure lock
        Nothing
          | remaining <= 0 -> throwIO LockTimeout
          | otherwise -> restore (delay 100000) >> acquireWithRetry restore (remaining - 1)

-- | Wait for a live writer to finish without assigning it a speed deadline.
-- Cancellation releases every unsuccessful connection. An unknown committed
-- owner fails finitely: this is not evidence that its process is dead, and
-- manual recovery must establish ownership independently before removing it.
withOverlayLockWaiting :: FilePath -> IO a -> IO a
withOverlayLockWaiting provenanceDb = withOverlayLockWaitingWithRetryHook provenanceDb (pure ())

-- | Test seam invoked after a busy attempt has closed its connection. It
-- permits causal handoff/cancellation tests without scheduling thresholds.
withOverlayLockWaitingWithRetryHook :: FilePath -> IO () -> IO a -> IO a
withOverlayLockWaitingWithRetryHook provenanceDb onBusy action =
  withOverlayAcquisition acquireWaiting action (pure ())
  where
    acquireWaiting restore = do
      now <- getCurrentTime
      attempted <- attemptOverlayLock provenanceDb (Text.pack (show now)) (pure ())
      case attempted of
        OverlayAcquired lock -> pure lock
        OverlayCommittedOwner -> throwIO OverlayOwnershipUnknown
        OverlayWriterBusy -> restore (onBusy >> delay 100000) >> acquireWaiting restore

withOverlayAcquisition :: ((IO () -> IO ()) -> IO OverlayLock) -> IO a -> IO () -> IO a
withOverlayAcquisition acquire action afterDelete = mask $ \restore -> do
  lock <- acquire restore
  actionResult <- try @SomeException (restore action)
  releaseResult <- try @SomeException (releaseOverlayLockWith lock afterDelete)
  let problems = [problem | Left problem <- [voidResult actionResult, releaseResult]]
  rethrowFirstCancellation problems
  case (actionResult, releaseResult) of
    (Left problem, _) -> throwIO problem
    (Right _, Left problem) -> throwIO problem
    (Right value, Right ()) -> pure value
  where
    voidResult result = case result of
      Left problem -> Left problem
      Right _ -> Right ()

-- | SQLite writer contention is equivalent to a failed lock acquisition:
-- release any partially opened connection and let the bounded outer retry
-- attempt the singleton claim again.  It is deliberately narrower than a
-- textual exception check so genuine schema and IO failures still surface.
isSqliteBusy :: SomeException -> Bool
isSqliteBusy problem =
  case fromException problem of
    Just SQLError{sqlError = ErrorBusy} -> True
    _ -> False

-- | Close a connection acquired by a failed lock attempt without allowing a
-- synchronous close failure to replace the acquisition failure.  Cancellation
-- is retained and rethrown after cleanup.
cleanupConnection :: Connection -> IO [SomeException]
cleanupConnection connection = mask_ $ do
  closeResult <- try @SomeException (close connection)
  pure [problem | Left problem <- [closeResult]]

rethrowFirstCancellation :: [SomeException] -> IO ()
rethrowFirstCancellation problems =
  case [async | problem <- problems, Just async <- [fromException problem]] of
    async : _ -> throwIO (async :: SomeAsyncException)
    [] -> pure ()

rethrowFirstProblem :: [SomeException] -> IO ()
rethrowFirstProblem problems = do
  rethrowFirstCancellation problems
  case problems of
    problem : _ -> throwIO problem
    [] -> pure ()

-- | Exception thrown when the lock is already held by another process.
data LockException
  = LockAlreadyHeld
  | LockTimeout
  | OverlayOwnershipUnknown
  deriving (Show)

instance Exception LockException

-- | Simple nanosecond sleep (used by withOverlayLock for timeout).
delay :: Int -> IO ()
delay = threadDelay
