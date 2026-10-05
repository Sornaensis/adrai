-- | Copy the current test executable for its existing basename-dispatched
-- native protocols. Permissions and filename suffix follow the actual host.
module Adrai.RetainedNative.NativeFixture
  ( nativeProgramName, copyNativeFixture, configureNativeFixture, constantNativeFixture, tracingNativeFixture ) where

import Adrai.Integration.CLI (requireExecutable)
import qualified Data.ByteString as BS
import System.Directory (copyFile, createDirectoryIfMissing, getPermissions, setPermissions, executable)
import System.Environment (getExecutablePath)
import System.FilePath ((</>))
import System.Info (os)

nativeProgramName :: FilePath -> FilePath
nativeProgramName stem = stem <> if os == "mingw32" then ".exe" else ""

copyNativeFixture :: FilePath -> IO ()
copyNativeFixture destination = do
  current <- getExecutablePath
  copyFile current destination
  permissions <- getPermissions destination
  setPermissions destination permissions {executable=True}

configureNativeFixture :: FilePath -> String -> IO FilePath
configureNativeFixture directory mode = do
  createDirectoryIfMissing True directory
  let destination = directory </> nativeProgramName "adrai-native-git-fixture-batch-p6133"
  copyNativeFixture destination
  writeFile (directory </> "fixture-mode") mode
  pure destination

constantNativeFixture :: FilePath -> BS.ByteString -> BS.ByteString -> Int -> IO FilePath
constantNativeFixture directory output errors code = do
  destination <- configureNativeFixture directory "constant"
  BS.writeFile (directory </> "fixture-stdout") output
  BS.writeFile (directory </> "fixture-stderr") errors
  writeFile (directory </> "fixture-exit") (show code)
  pure destination

tracingNativeFixture :: FilePath -> FilePath -> IO FilePath
tracingNativeFixture directory trace = do
  destination <- configureNativeFixture directory "trace-real-git"
  selectedGit <- requireExecutable "git"
  writeFile (directory </> "fixture-real-git") selectedGit
  writeFile (directory </> "fixture-trace-path") trace
  pure destination
