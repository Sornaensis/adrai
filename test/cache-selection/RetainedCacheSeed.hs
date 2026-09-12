module RetainedCacheSeed
  ( CacheSeed,
    createCacheSeed,
    removeCacheSeed,
    withPrivateCacheSeed,
  ) where

import Control.Exception (finally, onException)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import System.Directory
  ( copyFile,
    createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
    removePathForcibly,
  )
import System.FilePath (makeRelative, (</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory, withTempDirectory)

data CacheSeed payload = CacheSeed
  { cacheSeedRoot :: FilePath,
    cacheSeedRepository :: FilePath,
    cacheSeedPayload :: payload,
    cacheSeedBytes :: Map.Map FilePath BS.ByteString
  }

createCacheSeed :: (FilePath -> IO payload) -> IO (CacheSeed payload)
createCacheSeed setup = do
  temporary <- getCanonicalTemporaryDirectory
  root <- createTempDirectory temporary "adrai-retained-cache-seed"
  let repository = root </> "repository"
  createDirectory repository
  payload <- setup repository `onException` removePathForcibly root
  originalBytes <- snapshotFiles repository `onException` removePathForcibly root
  pure CacheSeed
    { cacheSeedRoot = root,
      cacheSeedRepository = repository,
      cacheSeedPayload = payload,
      cacheSeedBytes = originalBytes
    }

removeCacheSeed :: CacheSeed payload -> IO ()
removeCacheSeed = removePathForcibly . cacheSeedRoot

withPrivateCacheSeed :: CacheSeed payload -> (payload -> FilePath -> IO result) -> IO result
withPrivateCacheSeed seed action =
  withTempDirectory (cacheSeedRoot seed) "private" $ \privateRoot -> do
    let privateRepository = privateRoot </> "repository"
    copyDirectory (cacheSeedRepository seed) privateRepository
    action (cacheSeedPayload seed) privateRepository
      `finally` assertSeedUnchanged seed

assertSeedUnchanged :: CacheSeed payload -> IO ()
assertSeedUnchanged seed = do
  actual <- snapshotFiles (cacheSeedRepository seed)
  if actual == cacheSeedBytes seed
    then pure ()
    else ioError (userError "immutable cache seed changed while a private copy was in use")

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory source destination = do
  createDirectoryIfMissing True destination
  entries <- listDirectory source
  mapM_ copyEntry entries
  where
    copyEntry entry = do
      let sourcePath = source </> entry
          destinationPath = destination </> entry
      directory <- doesDirectoryExist sourcePath
      if directory
        then copyDirectory sourcePath destinationPath
        else copyFile sourcePath destinationPath

snapshotFiles :: FilePath -> IO (Map.Map FilePath BS.ByteString)
snapshotFiles root = Map.fromList <$> collect root
  where
    collect directory = do
      entries <- listDirectory directory
      fmap concat $ mapM (collectEntry directory) entries
    collectEntry directory entry = do
      let path = directory </> entry
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then collect path
        else do
          bytes <- BS.readFile path
          pure [(makeRelative root path, bytes)]
