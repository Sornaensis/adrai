{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Platform authority only. Public lock values never contain these handles.
module Adrai.Provenance.Git.Lock.Native
  ( NativeLock,
    openOwnedNative,
    openExistingNative,
    writeOwnedNative,
    releaseNative,
    getMyPid,
    nativeFailureIsContention,
    nativeFailureIsMissing,
    nativeStatusNeedsOpen,
    nativeCloseCompleted,
  )
where

import Control.Exception (IOException, SomeException, throwIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

#if defined(mingw32_HOST_OS)
import Control.Monad (unless)
import Data.Bits ((.|.))
import Foreign.Ptr (castPtr)
import System.Win32 (getCurrentProcessId)
import qualified System.Win32.File as Win32
import System.Win32.Types (HANDLE)
import System.Directory (doesFileExist)

newtype NativeLock = NativeLock HANDLE

getMyPid :: IO Int
getMyPid = fromIntegral <$> getCurrentProcessId

openOwnedNative :: FilePath -> IO NativeLock
openOwnedNative path =
  NativeLock <$> Win32.createFile path
    (Win32.gENERIC_READ .|. Win32.gENERIC_WRITE) Win32.fILE_SHARE_READ
    Nothing Win32.oPEN_ALWAYS Win32.fILE_ATTRIBUTE_NORMAL Nothing

openExistingNative :: FilePath -> IO NativeLock
openExistingNative path =
  NativeLock <$> Win32.createFile path
    (Win32.gENERIC_READ .|. Win32.gENERIC_WRITE) Win32.fILE_SHARE_READ
    Nothing Win32.oPEN_EXISTING Win32.fILE_ATTRIBUTE_NORMAL Nothing

writeOwnedNative :: FilePath -> NativeLock -> Int -> IO ()
writeOwnedNative _ (NativeLock handle) pid = do
  Win32.setEndOfFile handle
  let content = canonicalLockContent pid
  BS.useAsCStringLen content $ \(pointer, byteCount) -> do
    written <- Win32.win32_WriteFile handle (castPtr pointer) (fromIntegral byteCount) Nothing
    unless (written == fromIntegral byteCount) (throwIO (userError "short Windows Git lock write"))
  Win32.flushFileBuffers handle

releaseNative :: NativeLock -> IO ()
releaseNative (NativeLock handle) = Win32.closeHandle handle

-- Preserve the established Windows sharing-violation/path probe policy.
nativeFailureIsContention :: IOException -> Bool
nativeFailureIsContention _ = True

nativeFailureIsMissing :: IOException -> Bool
nativeFailureIsMissing _ = False

nativeStatusNeedsOpen :: FilePath -> IO Bool
nativeStatusNeedsOpen = doesFileExist

nativeCloseCompleted :: SomeException -> Bool
nativeCloseCompleted _ = False

#elif defined(linux_HOST_OS)
import Control.Exception (Exception, fromException, mask_, onException, try)
import Control.Monad (unless, void)
import Foreign.C.Error (Errno (..), eAGAIN, eWOULDBLOCK, eNOENT, errnoToIOError)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Ptr (Ptr, castPtr)
import GHC.IO.Exception (ioe_errno)
import System.FilePath (isAbsolute, splitDirectories)
import qualified System.Posix.IO as Posix
import System.Posix.Files (getFdStatus, isRegularFile)
import System.Posix.Process (getProcessID)
import System.Posix.Types (Fd (..))

newtype NativeLock = NativeLock Fd

-- Linux close consumes the descriptor even when reporting EINTR or delayed
-- I/O failure. Retrying it could close an unrelated, recycled descriptor.
newtype CloseCompleted = CloseCompleted IOException deriving (Show)
instance Exception CloseCompleted

nativeCloseCompleted :: SomeException -> Bool
nativeCloseCompleted failure = case fromException failure :: Maybe CloseCompleted of
  Just _ -> True
  Nothing -> False

nativeFailureIsContention :: IOException -> Bool
nativeFailureIsContention failure =
  ioe_errno failure `elem` [Just (errnoValue eAGAIN), Just (errnoValue eWOULDBLOCK)]
  where errnoValue (Errno value) = value

nativeFailureIsMissing :: IOException -> Bool
nativeFailureIsMissing failure = ioe_errno failure == Just value
  where Errno value = eNOENT

-- Probe through the secure open itself: a directory or dangling symlink must
-- fail closed rather than being treated as a missing regular file.
nativeStatusNeedsOpen :: FilePath -> IO Bool
nativeStatusNeedsOpen _ = pure True

foreign import ccall unsafe "adrai_git_lock_try"
  nativeTryLock :: CInt -> IO CInt
foreign import ccall safe "adrai_git_lock_write"
  nativeWrite :: CInt -> Ptr () -> CSize -> IO CInt
getMyPid :: IO Int
getMyPid = fromIntegral <$> getProcessID

openOwnedNative :: FilePath -> IO NativeLock
openOwnedNative path = openNative path True

openExistingNative :: FilePath -> IO NativeLock
openExistingNative path = openNative path False

openNative :: FilePath -> Bool -> IO NativeLock
openNative path create = mask_ $ do
  unless (isAbsolute path && '\0' `notElem` path) (throwIO (userError "Git lock requires an absolute path without NUL"))
  let components = filter (`notElem` ["/", ".", ""]) (splitDirectories path)
  unless (not (null components) && ".." `notElem` components) (throwIO (userError "Git lock path contains no regular-file component or contains parent traversal"))
  root <- Posix.openFd "/" Posix.ReadOnly directoryFlags
  parent <- walk root (init components)
  -- openFdAt uses the GHC filesystem encoder, including surrogate escapes,
  -- and passes nofollow/cloexec in the atomic openat call.
  descriptor <- Posix.openFdAt (Just parent) (last components) Posix.ReadWrite
    Posix.defaultFileFlags {Posix.nofollow=True, Posix.cloexec=True, Posix.nonBlock=True, Posix.creat=if create then Just 0o600 else Nothing}
    `onException` closeOnFailure parent
  Posix.closeFd parent `onException` closeOnFailure descriptor
  (do
      status <- getFdStatus descriptor
      unless (isRegularFile status) (throwIO (userError "Git lock descriptor is not a regular file"))
      let Fd value = descriptor
      result <- nativeTryLock value
      if result < 0
        then throwIO (errnoToIOError "flock Git lock" (Errno (negate result)) Nothing (Just path))
        else pure (NativeLock descriptor)
    ) `onException` closeOnFailure descriptor
  where
    directoryFlags = Posix.defaultFileFlags {Posix.nofollow=True, Posix.cloexec=True, Posix.directory=True}
    walk parent [] = pure parent
    walk parent (component:remaining) = do
      next <- Posix.openFdAt (Just parent) component Posix.ReadOnly directoryFlags `onException` closeOnFailure parent
      Posix.closeFd parent `onException` closeOnFailure next
      walk next remaining
    -- The original acquisition/cancellation failure keeps precedence. Linux
    -- close consumes these partially opened descriptors even on I/O error.
    closeOnFailure descriptor = void (try @IOException (Posix.closeFd descriptor))

writeOwnedNative :: FilePath -> NativeLock -> Int -> IO ()
writeOwnedNative path (NativeLock (Fd descriptor)) pid =
  BS.useAsCStringLen (canonicalLockContent pid) $ \(pointer, size) -> do
    result <- nativeWrite descriptor (castPtr pointer) (fromIntegral size)
    if result < 0
      then throwIO (errnoToIOError "write Git lock" (Errno (negate result)) Nothing (Just path))
      else pure ()

releaseNative :: NativeLock -> IO ()
releaseNative (NativeLock descriptor) = do
  result <- try @IOException (Posix.closeFd descriptor)
  either (throwIO . CloseCompleted) pure result

#else
#error Git lock native authority supports Windows and Linux only
#endif

canonicalLockContent :: Int -> BS.ByteString
canonicalLockContent pid = BS8.pack ("pid=" <> show pid <> "\n")
