{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Native observation handles. Traversal, enumeration and reads retain the
-- same verified objects; repository policy and observation budgets stay above.
module Adrai.Web.Watch.Native
  ( NativeHandle, FileIdentity, NativeEntryKind(..),
    openAbsoluteDirectory, withNativeHandle, closeHandle, handleIdentity,
    checkedInformation, isDirectoryInfo, isReparseInfo, validateHandleKind,
    foldDirectory, readHandleChunk, nativeNameLengthAcceptedForTest,
  ) where

import Control.Exception (bracket, mask, onException, throwIO)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Word (Word64)

#if defined(mingw32_HOST_OS)
import Data.Bits ((.&.), (.|.))
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word16, Word32, Word8)
import Foreign (Ptr, alloca, allocaBytesAligned, castPtr, fillBytes, nullPtr, peek, peekByteOff, plusPtr, pokeByteOff)
import Foreign.C.String (peekCWStringLen, withCWStringLen)
import Foreign.Ptr (WordPtr)
import Numeric (showHex)
import System.FilePath (normalise)
import qualified System.Win32.File as Win32
import System.Win32.Types (HANDLE)

type NativeHandle = HANDLE
closeHandle :: NativeHandle -> IO ()
closeHandle = Win32.closeHandle
foreign import ccall safe "NtCreateFile"
  c_NtCreateFile :: Ptr NativeHandle -> Word32 -> Ptr () -> Ptr () -> Ptr () -> Word32 -> Word32 -> Word32 -> Word32 -> Ptr () -> Word32 -> IO Int32

foreign import ccall safe "NtQueryDirectoryFile"
  c_NtQueryDirectoryFile :: NativeHandle -> NativeHandle -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Word32 -> Int32 -> Word8 -> Ptr () -> Word8 -> IO Int32

foreign import ccall safe "ReadFile"
  c_ReadFile :: NativeHandle -> Ptr Word8 -> Word32 -> Ptr Word32 -> Ptr () -> IO Int32

handleIdentity :: NativeHandle -> IO FileIdentity
handleIdentity handle = do
  information <- Win32.getFileInformationByHandle handle
  pure (FileIdentity (fromIntegral (Win32.bhfiVolumeSerialNumber information)) (Win32.bhfiFileIndex information))

checkedInformation :: NativeHandle -> IO Win32.BY_HANDLE_FILE_INFORMATION
checkedInformation handle = do
  information <- Win32.getFileInformationByHandle handle
  when (isReparseInfo information) (throwIO (userError "refusing reparse-point observation handle"))
  pure information

isDirectoryInfo :: Win32.BY_HANDLE_FILE_INFORMATION -> Bool
isDirectoryInfo information =
  Win32.bhfiFileAttributes information .&. Win32.fILE_ATTRIBUTE_DIRECTORY /= (0 :: Word32)

isReparseInfo :: Win32.BY_HANDLE_FILE_INFORMATION -> Bool
isReparseInfo information =
  Win32.bhfiFileAttributes information .&. Win32.fILE_ATTRIBUTE_REPARSE_POINT /= (0 :: Word32)

openAbsoluteDirectory :: FilePath -> IO NativeHandle
openAbsoluteDirectory path = do
  opened <- openNative nullPtr ("\\??\\" <> normalise path) DirectoryEntry
  maybe (throwIO (userError ("repository observation root is missing: " <> path))) pure opened

validateHandleKind :: NativeHandle -> NativeEntryKind -> IO ()
validateHandleKind handle kind = do
  information <- checkedInformation handle
  case kind of
    AnyEntry -> pure ()
    FileEntry -> when (isDirectoryInfo information) (throwIO (userError "expected a regular observation file"))
    DirectoryEntry -> when (not (isDirectoryInfo information)) (throwIO (userError "expected an observation directory"))

openNative :: NativeHandle -> FilePath -> NativeEntryKind -> IO (Maybe NativeHandle)
openNative parent name kind = mask $ \_ ->
  openNativeRaw parent name kind

withNativeHandle :: NativeHandle -> FilePath -> NativeEntryKind -> (Maybe NativeHandle -> IO value) -> IO value
withNativeHandle parent name kind action = mask $ \restore -> do
  opened <- openNativeRaw parent name kind
  case opened of
    Nothing -> restore (action Nothing)
    Just handle -> bracket (pure handle) Win32.closeHandle (restore . action . Just)

openNativeRaw :: NativeHandle -> FilePath -> NativeEntryKind -> IO (Maybe NativeHandle)
openNativeRaw parent name kind = do
  nameBytes <- either (throwIO . userError . Text.unpack) pure (nativeNameByteLength name)
  withCWStringLen name $ \(namePointer, characterCount) ->
    allocaBytesAligned unicodeStringBytes 8 $ \unicode ->
      allocaBytesAligned objectAttributesBytes 8 $ \objectAttributes ->
        allocaBytesAligned ioStatusBytes 8 $ \ioStatus ->
          alloca $ \handlePointer -> do
            fillBytes unicode 0 unicodeStringBytes
            fillBytes objectAttributes 0 objectAttributesBytes
            fillBytes ioStatus 0 ioStatusBytes
            when (characterCount * 2 /= fromIntegral nameBytes) (throwIO (userError "native UTF-16 name length mismatch"))
            pokeByteOff unicode 0 nameBytes
            pokeByteOff unicode 2 nameBytes
            pokeByteOff unicode 8 namePointer
            pokeByteOff objectAttributes 0 (fromIntegral objectAttributesBytes :: Word32)
            pokeByteOff objectAttributes 8 parent
            pokeByteOff objectAttributes 16 (castPtr unicode :: Ptr ())
            pokeByteOff objectAttributes 24 (objectCaseInsensitive .|. objectDontReparse :: Word32)
            status <- c_NtCreateFile handlePointer desiredAccess (castPtr objectAttributes) (castPtr ioStatus) nullPtr
              0 shareAll fileOpen (openOptions kind) nullPtr 0
            if status `elem` missingStatuses then pure Nothing
            else if status < 0 then throwNt "NtCreateFile" status
            else do
              handle <- peek handlePointer
              information <- Win32.getFileInformationByHandle handle `onException` Win32.closeHandle handle
              if isReparseInfo information
                then Win32.closeHandle handle >> throwIO (userError "refusing reparse-point observation handle")
                else do
                  validateHandleKind handle kind `onException` Win32.closeHandle handle
                  pure (Just handle)

nativeNameByteLength :: FilePath -> Either Text Word16
nativeNameByteLength name =
  let byteCount = BS.length (TextEncoding.encodeUtf16LE (Text.pack name))
   in if byteCount <= 0 || byteCount > fromIntegral (maxBound :: Word16)
        then Left "native observation name exceeds the UTF-16 length bound"
        else Right (fromIntegral byteCount)

nativeNameLengthAcceptedForTest :: FilePath -> Bool
nativeNameLengthAcceptedForTest name = case nativeNameByteLength name of
  Left _ -> False
  Right _ -> True

openOptions :: NativeEntryKind -> Word32
openOptions kind = fileOpenReparsePoint .|. fileSynchronousIoNonalert .|. case kind of
  AnyEntry -> 0
  FileEntry -> fileNonDirectoryFile
  DirectoryEntry -> fileDirectoryFile

foldDirectory :: NativeHandle -> value -> (value -> FilePath -> IO value) -> IO value
foldDirectory handle initial step =
  allocaBytesAligned ioStatusBytes 8 $ \ioStatus ->
    allocaBytesAligned nativeBufferBytes 8 $ \buffer -> loop ioStatus buffer True initial
  where
    loop ioStatus buffer restart value = do
      fillBytes ioStatus 0 ioStatusBytes
      status <- c_NtQueryDirectoryFile handle nullPtr nullPtr nullPtr (castPtr ioStatus) (castPtr buffer)
        (fromIntegral nativeBufferBytes) fileNamesInformation 1 nullPtr (if restart then 1 else 0)
      if status == statusNoMoreFiles then pure value
      else if status < 0 then throwNt "NtQueryDirectoryFile" status
      else do
        informationBytes <- peekByteOff ioStatus 8 :: IO WordPtr
        when (informationBytes < 12 || informationBytes > fromIntegral nativeBufferBytes)
          (throwIO (userError "invalid native directory information length"))
        nextOffset <- peekByteOff buffer 0 :: IO Word32
        nameBytes <- peekByteOff buffer 8 :: IO Word32
        when (nextOffset /= 0 || odd nameBytes || nameBytes > fromIntegral informationBytes - 12)
          (throwIO (userError "invalid native directory entry"))
        name <- peekCWStringLen (castPtr (buffer `plusPtr` 12), fromIntegral nameBytes `div` 2)
        next <- step value name
        loop ioStatus buffer False next

readHandleChunk :: NativeHandle -> Int -> IO BS.ByteString
readHandleChunk handle requested =
  allocaBytesAligned requested 8 $ \buffer ->
    alloca $ \readPointer -> do
      succeeded <- c_ReadFile handle (castPtr buffer) (fromIntegral requested) readPointer nullPtr
      when (succeeded == 0) (throwIO (userError "native observation read failed"))
      actual <- peek readPointer
      BS.packCStringLen (castPtr buffer, fromIntegral actual)

throwNt :: String -> Int32 -> IO value
throwNt operation status =
  throwIO (userError (operation <> " failed with NTSTATUS 0x" <> showHex (fromIntegral status :: Word32) ""))

desiredAccess, shareAll, fileOpen, fileOpenReparsePoint, fileSynchronousIoNonalert, fileDirectoryFile, fileNonDirectoryFile, objectCaseInsensitive, objectDontReparse :: Word32
desiredAccess = 0x00100081
shareAll = 0x00000007
fileOpen = 1
fileOpenReparsePoint = 0x00200000
fileSynchronousIoNonalert = 0x00000020
fileDirectoryFile = 0x00000001
fileNonDirectoryFile = 0x00000040
objectCaseInsensitive = 0x00000040
objectDontReparse = 0x00001000

fileNamesInformation :: Int32
fileNamesInformation = 12

missingStatuses :: [Int32]
missingStatuses = map fromIntegral ([0xC0000034, 0xC000003A] :: [Word32])

statusNoMoreFiles :: Int32
statusNoMoreFiles = fromIntegral (0x80000006 :: Word32)

unicodeStringBytes, objectAttributesBytes, ioStatusBytes, nativeBufferBytes :: Int
unicodeStringBytes = 16
objectAttributesBytes = 48
ioStatusBytes = 16
nativeBufferBytes = 64 * 1024

#elif defined(linux_HOST_OS)
import Control.Exception (IOException, try)
import Data.Char (ord)
import Foreign (Ptr, alloca, allocaBytes, castPtr, nullPtr, peek)
import Foreign.C.Error (Errno(..), eNOENT, errnoToIOError)
import Foreign.C.Types (CInt(..))
import GHC.IO.Exception (ioe_errno)
import qualified GHC.Foreign as Foreign
import GHC.IO.Encoding (getFileSystemEncoding)
import System.FilePath (isAbsolute, splitDirectories)
import qualified System.Posix.IO as Posix
import qualified System.Posix.Files as Files
import System.Posix.Types (Fd(..))

type NativeHandle = Fd

closeHandle :: NativeHandle -> IO ()
closeHandle = Posix.closeFd

handleIdentity :: NativeHandle -> IO FileIdentity
handleIdentity handle = do
  status <- checkedInformation handle
  pure (FileIdentity (fromIntegral (Files.deviceID status)) (fromIntegral (Files.fileID status)))

checkedInformation :: NativeHandle -> IO Files.FileStatus
checkedInformation handle = do
  status <- Files.getFdStatus handle
  when (not (Files.isDirectory status || Files.isRegularFile status))
    (throwIO (userError "refusing special observation handle"))
  pure status

isDirectoryInfo :: Files.FileStatus -> Bool
isDirectoryInfo = Files.isDirectory

isReparseInfo :: Files.FileStatus -> Bool
isReparseInfo = Files.isSymbolicLink

validateHandleKind :: NativeHandle -> NativeEntryKind -> IO ()
validateHandleKind handle kind = do
  status <- checkedInformation handle
  case kind of
    AnyEntry -> pure ()
    FileEntry -> when (not (Files.isRegularFile status)) (throwIO (userError "expected a regular observation file"))
    DirectoryEntry -> when (not (Files.isDirectory status)) (throwIO (userError "expected an observation directory"))

openAbsoluteDirectory :: FilePath -> IO NativeHandle
openAbsoluteDirectory path = mask $ \_ -> do
  when (not (isAbsolute path) || '\0' `elem` path) (throwIO (userError "observation root must be absolute without NUL"))
  let components = filter (`notElem` ["/", ".", ""]) (splitDirectories path)
  when (".." `elem` components) (throwIO (userError "observation root contains parent traversal"))
  root <- Posix.openFd "/" Posix.ReadOnly directoryFlags
  walk root components
  where
    walk parent [] = pure parent
    walk parent (component:remaining) = do
      next <- Posix.openFdAt (Just parent) component Posix.ReadOnly directoryFlags `onException` closeHandle parent
      closeHandle parent `onException` closeHandle next
      walk next remaining

directoryFlags :: Posix.OpenFileFlags
directoryFlags = Posix.defaultFileFlags {Posix.nofollow=True, Posix.cloexec=True, Posix.directory=True}

withNativeHandle :: NativeHandle -> FilePath -> NativeEntryKind -> (Maybe NativeHandle -> IO value) -> IO value
withNativeHandle parent name kind action = mask $ \restore -> do
  when (not (nativeNameLengthAcceptedForTest name)) (throwIO (userError "unsafe native observation component"))
  opened <- try @IOException (Posix.openFdAt (Just parent) name Posix.ReadOnly
    Posix.defaultFileFlags {Posix.nofollow=True, Posix.cloexec=True, Posix.nonBlock=True, Posix.directory=directoryRequired kind})
  case opened of
    Left problem
      | ioe_errno problem == Just missingErrno -> restore (action Nothing)
      | otherwise -> throwIO problem
    Right handle -> bracket (pure handle) closeHandle $ \owned -> do
      validateHandleKind owned kind
      restore (action (Just owned))
  where
    Errno missingErrno = eNOENT
    directoryRequired DirectoryEntry = True
    directoryRequired _ = False

-- Observation names enter the shared UTF-8 fingerprint contract. Surrogate
-- escapes preserve root/open path bytes, but cannot be framed through Text:
-- reject non-scalar observed components rather than silently replacing them.
-- Actual byte/component limits are checked by openat; no UTF-16 assumption.
nativeNameLengthAcceptedForTest :: FilePath -> Bool
nativeNameLengthAcceptedForTest name =
  not (null name || name `elem` [".",".."] || any (`elem` name) ['\0','/'])
    && all (\character -> ord character < 0xd800 || ord character > 0xdfff) name

foreign import ccall unsafe "adrai_watch_directory_open"
  directoryOpen :: CInt -> Ptr CInt -> IO (Ptr ())
foreign import ccall unsafe "adrai_watch_directory_next"
  directoryNext :: Ptr () -> Ptr (Ptr ()) -> IO CInt
foreign import ccall unsafe "adrai_watch_directory_close"
  directoryClose :: Ptr () -> IO CInt

-- openat(".") gets an independent open description, avoiding a dup's shared
-- directory offset. The stream remains bound to the captured directory.
foldDirectory :: NativeHandle -> value -> (value -> FilePath -> IO value) -> IO value
foldDirectory (Fd descriptor) initial step =
  bracket acquire release (\stream -> loop stream initial)
  where
    acquire = alloca $ \errorPointer -> do
      stream <- directoryOpen descriptor errorPointer
      if stream == nullPtr then peek errorPointer >>= failErrno "open observation directory stream" else pure stream
    release stream = directoryClose stream >>= \result -> when (result < 0) (failErrno "close observation directory stream" (negate result))
    loop stream value = alloca $ \namePointer -> do
      result <- directoryNext stream namePointer
      if result < 0 then failErrno "read observation directory" (negate result)
      else if result == 0 then pure value
      else do
        pointer <- peek namePointer
        encoding <- getFileSystemEncoding
        name <- Foreign.peekCString encoding (castPtr pointer)
        when (any (\character -> ord character >= 0xd800 && ord character <= 0xdfff) name)
          (throwIO (userError "observation entry is not a Unicode scalar name"))
        next <- step value name
        loop stream next

failErrno :: String -> CInt -> IO value
failErrno operation value = throwIO (errnoToIOError operation (Errno value) Nothing Nothing)

readHandleChunk :: NativeHandle -> Int -> IO BS.ByteString
readHandleChunk descriptor requested = allocaBytes requested $ \buffer -> do
  count <- Posix.fdReadBuf descriptor buffer (fromIntegral requested)
  BS.packCStringLen (castPtr buffer, fromIntegral count)

#else
#error Watch observation supports Windows and Linux only
#endif

data FileIdentity = FileIdentity Word64 Word64 deriving (Eq, Show)
data NativeEntryKind = AnyEntry | FileEntry | DirectoryEntry
