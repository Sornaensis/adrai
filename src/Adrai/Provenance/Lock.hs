{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Process-serialize overlay lock using SQLite row locking.
--
-- Prevents concurrent provenance overlay writes by using a dedicated
-- lock database with a single-row approach.  If the lock row already
-- exists (held by another process), acquisition fails.
--
-- Mirrors the Python prototype's ``_overlay_lock()`` context manager
-- in ``ADRAI_1_Source/adrai_core/provenance_cache.py``.
module Adrai.Provenance.Lock
  ( OverlayLock (..),
    acquireOverlayLock,
    acquireOverlayLockWith,
    acquireOverlayLockWithToken,
    releaseOverlayLock,
    releaseOverlayLockWith,
    withOverlayLock,
    withOverlayLockWithReleaseHook,
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
import Data.String (fromString)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Int (Int64)
import Data.Time.Clock (getCurrentTime)
import qualified Data.Text as Text
import Database.SQLite.Simple
  ( Connection,
    Only (..),
    SQLData (SQLText),
    execute,
    execute_,
    open,
    query_,
    close,
    withTransaction,
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
-- Opens the lock database, creates the table if needed, and tries
-- an ``INSERT OR IGNORE`` to claim the lock row.  Returns 'OverlayLock'
-- if acquired, 'Nothing' if the lock is already held.
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
  let lockPath' = lockDatabasePath provenanceDb
      lockDir     = takeDirectory lockPath'
  -- Ensure parent directory exists (SQLite needs the directory).
  createDirectoryIfMissing True lockDir

  let acquiredAt = holderPid
  connectionResult <- try @SomeException (open lockPath')
  case connectionResult of
    Left problem -> throwIO problem
    Right conn -> do
      acquisition <- try @SomeException $ do
        restore (execute_ conn (asQuery (Text.pack lockTableDdl)))
        withTransaction conn $ restore $ do
          -- Use the fixed SQLite rowid as the singleton constraint.  The
          -- insertion and ownership check share one rollback-capable
          -- transaction, so cancellation cannot publish an ownerless row.
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
          pure (Just (OverlayLock lockPath' conn holderPid released))
        Left problem -> do
          cleanupProblems <- cleanupConnection conn
          rethrowFirstCancellation (problem : cleanupProblems)
          case fromException problem of
            Just LockAlreadyHeld -> case cleanupProblems of
              cleanupProblem : _ -> throwIO cleanupProblem
              [] -> pure Nothing
            _ -> throwIO problem

-- | Release the overlay lock by deleting the owned singleton row and closing
-- the connection.  Repeated calls are harmless; a genuine first-release
-- failure is reported after every cleanup step has been attempted.
releaseOverlayLock :: OverlayLock -> IO ()
releaseOverlayLock lock = releaseOverlayLockWith lock (pure ())

-- | Testable release boundary.  The hook runs after the owned row is removed
-- and before the connection closes.  Cleanup always completes before any
-- synchronous or asynchronous hook failure is rethrown.
releaseOverlayLockWith :: OverlayLock -> IO () -> IO ()
releaseOverlayLockWith lock afterDelete = mask_ $ do
  alreadyReleased <- atomicModifyIORef' (lockReleased lock) (\released -> (True, released))
  if alreadyReleased
    then pure ()
    else do
      deleteResult <- try @SomeException
        (execute (lockConnection lock) "DELETE FROM overlay_lock WHERE rowid=1 AND holder_pid=?" [SQLText (lockHolderPid lock)])
      hookResult <- try @SomeException afterDelete
      closeResult <- try @SomeException (close (lockConnection lock))
      rethrowFirstProblem [problem | Left problem <- [deleteResult, hookResult, closeResult]]

-- | Run an action with the overlay lock held.
--
-- Blocks until acquired or times out (5 seconds).  Masked cleanup always runs,
-- and cancellation outranks any simultaneous synchronous cleanup failure.
withOverlayLock :: FilePath -> IO a -> IO a
withOverlayLock provenanceDb action =
  withOverlayLockWithReleaseHook provenanceDb action (pure ())

-- | Testable bracket boundary for proving exception priority when both the
-- protected action and release cleanup fail.  Production callers use
-- 'withOverlayLock'.
withOverlayLockWithReleaseHook :: FilePath -> IO a -> IO () -> IO a
withOverlayLockWithReleaseHook provenanceDb action afterDelete = mask $ \restore -> do
  lock <- acquireWithRetry restore 50
  actionResult <- try @SomeException (restore action)
  releaseResult <- try @SomeException (releaseOverlayLockWith lock afterDelete)
  let problems = [problem | Left problem <- [voidResult actionResult, releaseResult]]
  rethrowFirstCancellation problems
  case (actionResult, releaseResult) of
    (Left problem, _) -> throwIO problem
    (Right _, Left problem) -> throwIO problem
    (Right value, Right ()) -> pure value
  where
    acquireWithRetry restore remaining = do
      candidate <- acquireOverlayLock provenanceDb
      case candidate of
        Just lock -> pure lock
        Nothing
          | remaining <= 0 -> throwIO LockTimeout
          | otherwise -> restore (delay 100000) >> acquireWithRetry restore (remaining - 1)

    voidResult result = case result of
      Left problem -> Left problem
      Right _ -> Right ()

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
  deriving (Show)

instance Exception LockException

-- | Simple nanosecond sleep (used by withOverlayLock for timeout).
delay :: Int -> IO ()
delay = threadDelay
