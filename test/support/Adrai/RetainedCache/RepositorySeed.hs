module Adrai.RetainedCache.RepositorySeed
  ( RepositorySeed,
    createRepositorySeed,
    removeRepositorySeed,
    withPrivateRepositorySeed,
  ) where

import Control.Exception (finally, onException)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
    removePathForcibly,
  )
import System.FilePath (makeRelative, (</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory, withTempDirectory)

data RepositorySeed payload = RepositorySeed
  { seedRoot :: FilePath,
    seedRepository :: FilePath,
    seedPayload :: payload,
    seedBytes :: Map.Map FilePath BS.ByteString
  }

createRepositorySeed :: (FilePath -> IO (FilePath, payload)) -> IO (RepositorySeed payload)
createRepositorySeed setup = do
  temporary <- getCanonicalTemporaryDirectory
  root <- createTempDirectory temporary "adrai-retained-cache-integration-seed"
  (repository, payload) <- setup root `onException` removePathForcibly root
  originalBytes <- snapshotFiles repository `onException` removePathForcibly root
  pure RepositorySeed
    { seedRoot = root,
      seedRepository = repository,
      seedPayload = payload,
      seedBytes = originalBytes
    }

removeRepositorySeed :: RepositorySeed payload -> IO ()
removeRepositorySeed = removePathForcibly . seedRoot

withPrivateRepositorySeed
  :: RepositorySeed payload
  -> (payload -> FilePath -> IO result)
  -> IO result
withPrivateRepositorySeed seed action =
  withTempDirectory (seedRoot seed) "private" $ \privateRoot -> do
    let privateRepository = privateRoot </> "repository"
    copyDirectory (seedRepository seed) privateRepository
    action (seedPayload seed) privateRepository
      `finally` assertSeedUnchanged seed

assertSeedUnchanged :: RepositorySeed payload -> IO ()
assertSeedUnchanged seed = do
  actual <- snapshotFiles (seedRepository seed)
  if actual == seedBytes seed
    then pure ()
    else ioError (userError "immutable cache integration seed changed while a private copy was in use")

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
