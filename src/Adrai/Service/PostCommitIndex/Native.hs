{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Install a fully closed sibling database. Linux requests file and directory
-- synchronization; this is not a universal power-loss guarantee. A failure
-- after rename is reported to the publisher's existing observation/reconciliation.
module Adrai.Service.PostCommitIndex.Native (installFile) where

#if defined(mingw32_HOST_OS)
import Data.Bits ((.|.))
import System.Directory (renameFile)
import System.Win32.Types (BOOL, DWORD, LPTSTR, failIfFalse_, withTString)

foreign import ccall unsafe "MoveFileExW" c_MoveFileExW :: LPTSTR -> LPTSTR -> DWORD -> IO BOOL

-- ReplaceFileW opens its replacement without SQLite-compatible sharing.
-- Same-volume MoveFileExW retains the existing REPLACE_EXISTING/WRITE_THROUGH
-- request, without COPY_ALLOWED. First publication retains renameFile.
installFile :: FilePath -> FilePath -> Bool -> IO ()
installFile target candidate replacing
  | not replacing = renameFile candidate target
  | otherwise = withTString candidate $ \source -> withTString target $ \destination ->
      failIfFalse_ "MoveFileExW" (c_MoveFileExW source destination (0x00000001 .|. 0x00000008))
#elif defined(linux_HOST_OS)
import Control.Exception (SomeException, mask, throwIO, try)
import Control.Monad (when)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..))
import qualified GHC.Foreign as Foreign
import GHC.IO.Encoding (getFileSystemEncoding)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>), isAbsolute, normalise, splitDirectories, takeDirectory, takeFileName)
import qualified System.Posix.Files as Files
import qualified System.Posix.IO as Posix
import System.Posix.Types (Fd(..))

foreign import ccall safe "fsync" c_fsync :: CInt -> IO CInt
foreign import ccall safe "renameat" c_renameat :: CInt -> CString -> CInt -> CString -> IO CInt

installFile :: FilePath -> FilePath -> Bool -> IO ()
installFile target candidate _ = do
  currentDirectory <- getCurrentDirectory
  let absolute path = if isAbsolute path then path else currentDirectory </> path
      absoluteTarget = absolute target
      absoluteCandidate = absolute candidate
      parent = takeDirectory absoluteTarget
      sourceName = takeFileName absoluteCandidate
      targetName = takeFileName absoluteTarget
  when (any ('\0' `elem`) [target, candidate]
      || normalise parent /= normalise (takeDirectory absoluteCandidate)
      || sourceName == targetName || any (`elem` ["", ".", ".."]) [sourceName, targetName])
    (throwIO (userError "cache publication requires distinct absolute same-directory paths without NUL"))
  withDirectory parent $ \directory@(Fd directoryFd) -> do
    withOwnedFd (Posix.openFdAt (Just directory) sourceName Posix.ReadOnly
        Posix.defaultFileFlags {Posix.nofollow=True, Posix.cloexec=True, Posix.nonBlock=True}) $ \file@(Fd fileFd) -> do
      status <- Files.getFdStatus file
      when (not (Files.isRegularFile status)) (throwIO (userError "cache publication candidate is not a regular file"))
      throwErrnoIfMinus1_ "fsync cache candidate" (c_fsync fileFd)
    encoding <- getFileSystemEncoding
    Foreign.withCString encoding sourceName $ \source ->
      Foreign.withCString encoding targetName $ \destination ->
        throwErrnoIfMinus1_ "renameat cache candidate" (c_renameat directoryFd source directoryFd destination)
    throwErrnoIfMinus1_ "fsync cache directory" (c_fsync directoryFd)

-- Every ancestor is opened relative to a held directory, without following
-- redirects. All descriptors have atomic CLOEXEC and are closed once, including
-- cancellation. Keeping parent descriptors through traversal avoids handoff gaps.
withDirectory :: FilePath -> (Fd -> IO value) -> IO value
withDirectory path action = do
  let components = filter (`notElem` ["/", ".", ""]) (splitDirectories path)
      flags = Posix.defaultFileFlags {Posix.nofollow=True, Posix.cloexec=True, Posix.directory=True}
      walk parent [] = action parent
      walk parent (component:remaining) =
        withOwnedFd (Posix.openFdAt (Just parent) component Posix.ReadOnly flags) (\next -> walk next remaining)
  when (".." `elem` components) (throwIO (userError "cache publication parent contains traversal"))
  withOwnedFd (Posix.openFd "/" Posix.ReadOnly flags) (\root -> walk root components)

withOwnedFd :: forall value. IO Fd -> (Fd -> IO value) -> IO value
withOwnedFd acquire action = mask $ \restore -> do
  descriptor <- acquire
  outcome <- try (restore (action descriptor)) :: IO (Either SomeException value)
  closed <- try (Posix.closeFd descriptor) :: IO (Either SomeException ())
  -- Linux consumes the descriptor even when close reports EINTR. Never retry it;
  -- an initiating action/cancellation failure retains precedence over close.
  case outcome of
    Left problem -> throwIO problem
    Right value -> either throwIO (const (pure value)) closed
#else
#error ADRAI cache publication supports Windows and Linux only
#endif
