module Adrai.RetainedNative.ResidualRepositorySeed
  ( RepositorySeed,
    createRepositorySeed,
    createRepositorySeedWith,
    removeRepositorySeed,
    withRepositorySeedCopy,
  )
where

import Control.Exception (onException)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
    removePathForcibly,
  )
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory, withSystemTempDirectory)
import System.Process.Typed (byteStringInput, proc, readProcess, setStdin)

data RepositorySeed = RepositorySeed
  { repositorySeedRoot :: FilePath,
    repositorySeedRepository :: FilePath
  }

createRepositorySeed :: String -> (FilePath -> IO ()) -> IO RepositorySeed
createRepositorySeed label populate = fst <$> createRepositorySeedWith label (\repository -> populate repository >> pure ())

createRepositorySeedWith :: String -> (FilePath -> IO seed) -> IO (RepositorySeed, seed)
createRepositorySeedWith label populate = do
  temporaryRoot <- getCanonicalTemporaryDirectory
  seedRoot <- createTempDirectory temporaryRoot label
  let repository = seedRoot </> "repository"
  onException
    (do
      seed <- populate repository
      pure (RepositorySeed seedRoot repository, seed))
    (removePathForcibly seedRoot)

removeRepositorySeed :: RepositorySeed -> IO ()
removeRepositorySeed = removePathForcibly . repositorySeedRoot

withRepositorySeedCopy :: RepositorySeed -> String -> (FilePath -> IO value) -> IO value
withRepositorySeedCopy seed label action =
  withSystemTempDirectory label $ \temporary -> do
    let repository = temporary </> "repository"
        hooksDirectory = repository </> ".git" </> "adrai-no-hooks"
    copyDirectory (repositorySeedRepository seed) repository
    createDirectoryIfMissing True hooksDirectory
    _ <- gitSuccess repository ["config", "core.hooksPath", hooksDirectory] BS.empty
    action repository

gitSuccess :: FilePath -> [String] -> BS.ByteString -> IO BS.ByteString
gitSuccess directory arguments input = do
  (exitCode, stdoutBytes, stderrBytes) <-
    readProcess
      ( setStdin (byteStringInput (LBS.fromStrict input))
          (proc "git" ("-C" : directory : arguments))
      )
  let stdout = LBS.toStrict stdoutBytes
      stderr = LBS.toStrict stderrBytes
  case exitCode of
    ExitSuccess -> pure stdout
    ExitFailure code ->
      fail
        ( "Git test fixture command failed ("
            <> show code
            <> "): git -C "
            <> show directory
            <> " "
            <> show arguments
            <> "\nstdout: "
            <> Text.unpack (TextEncoding.decodeUtf8Lenient stdout)
            <> "\nstderr: "
            <> Text.unpack (TextEncoding.decodeUtf8Lenient stderr)
        )

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
