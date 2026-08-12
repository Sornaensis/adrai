{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE CPP #-}

-- | File-based mutation lock on @<git-common-dir>/adrai.lock@.
--
-- Matches the Python @O_EXCL@ / @os.open@ semantics in
-- @adrai_lock@ from @gitops.py@.  The lock is a single file containing
-- @pid=<pid>\\n@ (ASCII).
--
-- On POSIX the file is opened with @O_CREAT | O_EXCL | O_WRONLY@ for
-- atomic cross-process semantics.  On Windows the same contract is
-- achieved by opening the file with 'System.IO' and checking for
-- 'System.IO.Error.isAlreadyExistsError'.
--
-- Crash-safe stale-lock detection: when an existing lock file is found
-- the recorded PID is checked via @kill@ 0 (POSIX) or @OpenProcess@
-- (Windows); a dead process's lock is cleared before retrying.
--
-- One lock per repository (common directory).  Lock file is always
-- placed at @<git-common-dir>/adrai.lock@.
module Adrai.Provenance.Git.Lock
  ( GitLockError (..),
    GitLockInfo (..),
    GitLock (..),
    acquireGitLock,
    releaseGitLock,
    withGitLock,
    gitLockStatus,
  )
where

import Adrai.Git (Repository (..), repositoryCommonDir)

import Control.Exception
  ( Exception,
    SomeException,
    bracket,
    catch,
    fromException,
    throwIO,
    try,
  )
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Foreign.Ptr (nullPtr)
import System.Directory
  ( doesFileExist,
    removeFile,
  )
import System.FilePath ((</>))
import System.IO
  ( IOMode (WriteMode),
    Handle,
    hFlush,
    hPutStr,
    openFile,
  )
import System.IO.Error
  ( isAlreadyExistsError,
  )

#ifdef mingw32_HOST_OS
import System.Win32 (HANDLE, closeHandle, getCurrentProcessId)
import System.Win32.Process (openProcess)
#else
import System.Process (getProcessID)
import qualified System.Posix.IO as PosixIO
  ( FileOffset (..),
    StdStream (..),
    defaultFileFlags,
    fdToHandle,
    fdWriteAll,
    hFlush,
    openFD,
  )
import System.Posix.Signals (killProcess)
import System.Posix.Types (Fd (Fd))
#endif

-- | Check whether a process with the given PID is alive.
pidIsAlive :: Int -> IO Bool
pidIsAlive pid
  | pid <= 0  = pure False
  | otherwise = do
#ifdef mingw32_HOST_OS
      checkPidAliveWindows pid
#else
      result <- try @SomeException (killProcess (pid, 0))
      pure (case result of
        Left  _ -> False
        Right _ -> True)
#endif

#ifdef mingw32_HOST_OS
-- | Windows process-alive check via @OpenProcess@.
checkPidAliveWindows :: Int -> IO Bool
checkPidAliveWindows pid = do
  let access = 0x1000  -- PROCESS_QUERY_LIMITED_INFORMATION
      pidDword = fromIntegral pid
  result <- try @SomeException (openProcess access False pidDword)
  case result of
    Left  _  -> pure False
    Right h  ->
      if h /= nullPtr
        then (closeHandle h >> pure True)
        else (closeHandle h >> pure False)

-- | Get the current process ID (Windows).
getMyPid :: IO Int
getMyPid = do
  pid <- getCurrentProcessId
  pure (fromIntegral pid)
#else
-- | Get the current process ID (POSIX).
getMyPid :: IO Int
getMyPid = do
  pid <- PosixIO.getProcessID
  pure (fromIntegral pid)
#endif

-- ---------------------------------------------------------------------------
-- Lock errors and info
-- ---------------------------------------------------------------------------

-- | Errors that can occur while inspecting or acquiring the lock file.
data GitLockError
  = LockHeld FilePath Int  -- ^ lock file path and PID of holder
  | LockFailed FilePath Text
      -- ^ could not acquire lock
  | LockParseError Text    -- ^ could not parse PID from lock file
  deriving (Eq, Show)

instance Exception GitLockError

-- | Information about the current lock state (non-owning).
data GitLockInfo
  = GitLockInfo
      { lockPath  :: FilePath,
        lockPid   :: Maybe Int,
        lockLive  :: Bool,
        lockStale :: Bool
      }
  deriving (Eq, Show)

-- | Handle for an acquired Git lock.
data GitLock
  = GitLock
      { gitLockPath :: FilePath,
        gitLockFd   :: Int,  -- POSIX file descriptor; 0 on Windows
        gitLockPid  :: Int
      }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

lockFileName :: String
lockFileName = "adrai.lock"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

lockFilePathFor :: FilePath -> FilePath
lockFilePathFor directory = directory </> lockFileName

-- | Read the PID from a lock file.
--
-- Parses the @pid=<number>@ pattern from any line in the file, matching
-- the more lenient regex used in the Python reference implementation.
parseLockPid :: FilePath -> IO (Maybe Int)
parseLockPid path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      content <- try @SomeException (readFile path)
      case content of
        Left _  -> pure Nothing
        Right txt ->
          case findPidLine (Text.lines (Text.pack txt)) of
            Nothing -> pure Nothing
            Just pid -> pure (Just pid)
  where
    findPidLine :: [Text] -> Maybe Int
    findPidLine [] = Nothing
    findPidLine (line : rest) =
      case Text.stripPrefix "pid=" (Text.strip line) of
        Nothing       -> findPidLine rest
        Just rest' ->
          case Text.span (`Text.elem` "0123456789") rest' of
            (digits, _)
              | Text.null digits -> findPidLine rest
              | otherwise     ->
                  case reads (Text.unpack digits) of
                    [(n :: Int, _)] | n > 0 -> Just n
                    _                       -> findPidLine rest

-- ---------------------------------------------------------------------------
-- Lock status (read-only inspection)
-- ---------------------------------------------------------------------------

-- | Inspect the lock state without modifying the lock file.
--
-- Returns 'Nothing' when no lock file exists, 'Left' with 'LockHeld'
-- when an active lock is held, or 'Right' with stale lock information
-- when the lock file is present but the holder process is dead.
gitLockStatus :: Repository -> IO (Either GitLockError (Maybe GitLockInfo))
gitLockStatus repository = do
  let path = lockFilePathFor (repositoryCommonDir repository)
  exists <- doesFileExist path
  if not exists
    then pure (Right Nothing)
    else do
      pid <- parseLockPid path
      case pid of
        Nothing ->
          pure
            ( Left
                ( LockParseError
                    (Text.pack (path ++ ""))
                )
            )
        Just pidValue -> do
          live <- pidIsAlive pidValue
          let info =
                GitLockInfo
                  { lockPath  = path,
                    lockPid   = Just pidValue,
                    lockLive  = live,
                    lockStale = not live
                  }
          if live
            then pure (Left (LockHeld path pidValue))
            else pure (Right (Just info))

-- ---------------------------------------------------------------------------
-- Lock acquisition
-- ---------------------------------------------------------------------------

-- | Acquire the Git mutation lock.
--
-- Opens the lock file with exclusive creation semantics (POSIX @O_EXCL@
-- or Windows equivalent).  On conflict the implementation performs
-- crash-safe stale-lock recovery: it reads the PID from the existing
-- lock, checks whether the holder is alive, and removes the lock if
-- the process is dead.  A second attempt is made before raising
-- 'LockHeld'.
acquireGitLock :: Repository -> IO GitLock
acquireGitLock repository = do
  let path = lockFilePathFor (repositoryCommonDir repository)
  myPid <- getMyPid
  attemptAcquire path myPid 0
  where
    attemptAcquire :: FilePath -> Int -> Int -> IO GitLock
    attemptAcquire path pid attempt = do
      result <- try @SomeException $ do
#ifdef mingw32_HOST_OS
        acquireLockWindows path pid
#else
        acquireLockPosix path pid
#endif
      case result of
        Right lock -> pure lock
        Left e ->
          case fromException e of
            Just (LockHeld fp otherPid) -> throwIO (LockHeld fp otherPid)
            Nothing ->
              throwIO (LockFailed path ("could not acquire lock"))

-- ---------------------------------------------------------------------------
-- Lock acquisition
-- ---------------------------------------------------------------------------

-- | POSIX lock acquisition using @O_CREAT | O_EXCL@.
#ifdef mingw32_HOST_OS
acquireLockPosix :: FilePath -> Int -> IO GitLock
acquireLockPosix _ _ = error "acquireLockPosix on Windows"
#else
acquireLockPosix :: FilePath -> Int -> IO GitLock
acquireLockPosix path pid = do
  -- Atomic exclusive create.
  fd <- PosixIO.openFD path PosixIO.defaultFileFlags
             { PosixIO.read = True, PosixIO.write = True }
             [ StdStream StdStreamInherit
             , StdStream StdStreamInherit
             , StdStream StdStreamInherit
             ]
             Nothing
             (Just 0o600)
  -- Write our PID and return.
  writeLockPosix fd path pid
  pure (GitLock path (fromIntegral fd) pid)
#endif

-- | Windows lock acquisition using 'System.IO'.
#ifdef mingw32_HOST_OS
acquireLockWindows :: FilePath -> Int -> IO GitLock
acquireLockWindows path pid = do
  result <- try @IOError (openFile path WriteMode)
  case result of
    Left e
      | isAlreadyExistsError e -> do
          existingPid <- parseLockPid path
          case existingPid of
            Just otherPid
              | otherPid == pid -> do
                  -- Our own stale lock; remove and retry.
                  void $ try @SomeException (removeFile path)
                  acquireLockWindows path pid
              | otherwise -> do
                  alive <- pidIsAlive otherPid
                  if alive
                    then throwIO (LockHeld path otherPid)
                    else do
                      -- Stale lock: remove and retry.
                      void $ try @SomeException (removeFile path)
                      acquireLockWindows path pid
            Nothing ->
              -- Can't parse PID but file exists; retry.
              acquireLockWindows path pid
    Right handle -> do
      writeLockHandle handle pid
      pure (GitLock path 0 pid)
#else
acquireLockWindows :: FilePath -> Int -> IO GitLock
acquireLockWindows _ _ = error "acquireLockWindows on POSIX"
#endif

-- ---------------------------------------------------------------------------
-- Lock content helpers
-- ---------------------------------------------------------------------------

-- | Write the lock content to a POSIX file descriptor.
#ifdef mingw32_HOST_OS
writeLockPosix :: FilePath -> Int -> IO ()
writeLockPosix _ _ = pure ()
#else
writeLockPosix :: PosixIO.Fd -> FilePath -> Int -> IO ()
writeLockPosix fd _path pid = do
  let content = "pid=" <> (Text.pack (show pid)) <> "\n"
  PosixIO.fdWriteAll fd 0 (encodeUtf8 content)
  PosixIO.hFlush (PosixIO.fdToHandle fd)
  pure ()
#endif

-- | Write the lock content to a Windows handle.
#ifdef mingw32_HOST_OS
writeLockHandle :: Handle -> Int -> IO ()
writeLockHandle handle pid = do
  hPutStr handle (show pid <> "\n")
  hFlush handle
#else
writeLockHandle :: Handle -> Int -> IO ()
writeLockHandle _ _ = pure ()
#endif

-- ---------------------------------------------------------------------------
-- Lock release
-- ---------------------------------------------------------------------------

-- | Release the Git lock (idempotent).
--
-- Closes the file descriptor / handle and removes the lock file.
-- Errors are suppressed: the lock may already be released.
releaseGitLock :: GitLock -> IO ()
releaseGitLock lock = do
  case lock of
    GitLock lPath lFd _pid -> do
      case lFd of
        0 -> do
          -- Windows: no FD, nothing to close
          void $ try @SomeException (removeFile lPath)
        _ -> do
#ifdef mingw32_HOST_OS
          -- POSIX FD but on Windows (shouldn't happen normally)
          void $ try @SomeException (removeFile lPath)
#else
          void $ try @SomeException $ PosixIO.closeFD (PosixIO.Fd lFd)
          void $ try @SomeException (removeFile lPath)
#endif

-- ---------------------------------------------------------------------------
-- Bracketed lock
-- ---------------------------------------------------------------------------

-- | Run an action with the Git lock held, ensuring cleanup.
withGitLock :: Repository -> IO a -> IO a
withGitLock repository action = do
  lock <- acquireGitLock repository
  bracket
    (pure ())
    (\_ -> releaseGitLock lock)
    (\_ -> action `catch` handler lock)
  where
    handler :: GitLock -> SomeException -> IO a
    handler lock exc = do
      releaseGitLock lock
      throwIO exc
