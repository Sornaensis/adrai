module Adrai.RetainedCLI.RepositorySeed
  ( RepositorySeed,
    createRepositorySeed,
    removeRepositorySeed,
    withRepositorySeedCopy,
  )
where

import Control.Exception (onException)
import Control.Monad (forM_)
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
    removePathForcibly,
  )
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory, withSystemTempDirectory)

data RepositorySeed = RepositorySeed
  { repositorySeedRoot :: FilePath,
    repositorySeedRepository :: FilePath
  }

createRepositorySeed :: String -> (FilePath -> IO ()) -> IO RepositorySeed
createRepositorySeed label populate = do
  temporaryRoot <- getCanonicalTemporaryDirectory
  seedRoot <- createTempDirectory temporaryRoot label
  let repository = seedRoot </> "repository"
  onException
    (populate repository >> pure (RepositorySeed seedRoot repository))
    (removePathForcibly seedRoot)

removeRepositorySeed :: RepositorySeed -> IO ()
removeRepositorySeed = removePathForcibly . repositorySeedRoot

withRepositorySeedCopy :: RepositorySeed -> String -> (FilePath -> FilePath -> IO value) -> IO value
withRepositorySeedCopy seed label action =
  withSystemTempDirectory label $ \temporary -> do
    let repository = temporary </> "repository"
    copyDirectory (repositorySeedRepository seed) repository
    action temporary repository

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory source destination = do
  createDirectoryIfMissing True destination
  entries <- listDirectory source
  forM_ entries $ \entry -> do
    let sourcePath = source </> entry
        destinationPath = destination </> entry
    directory <- doesDirectoryExist sourcePath
    if directory
      then copyDirectory sourcePath destinationPath
      else copyFile sourcePath destinationPath
