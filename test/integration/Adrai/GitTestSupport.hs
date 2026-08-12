{-# LANGUAGE OverloadedStrings #-}

module Adrai.GitTestSupport
  ( initTestRepository,
    initBareRepository,
    commitFile,
    commitFiles,
    hashObject,
    gitSuccess,
    gitResult,
    outputText,
    cherryPick,
    createWorktree,
    hardReset,
    rebase,
    removeWorktree,
    squashMerge,
    switchBranch,
    requireRepoPath,
    requireRevision,
  )
where

import Adrai.Git (RevisionSpec, mkRevisionSpec)
import Adrai.Types (RepoPath, mkRepoPath)
import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Process.Typed (byteStringInput, proc, readProcess, setStdin)

gitResult :: FilePath -> [String] -> ByteString -> IO (ExitCode, ByteString, ByteString)
gitResult directory arguments input = do
  (exitCode, stdoutBytes, stderrBytes) <-
    readProcess
      ( setStdin (byteStringInput (LBS.fromStrict input))
          (proc "git" ("-C" : directory : arguments))
      )
  pure (exitCode, LBS.toStrict stdoutBytes, LBS.toStrict stderrBytes)

gitSuccess :: FilePath -> [String] -> ByteString -> IO ByteString
gitSuccess directory arguments input = do
  (exitCode, stdoutBytes, stderrBytes) <- gitResult directory arguments input
  case exitCode of
    ExitSuccess -> pure stdoutBytes
    ExitFailure code ->
      fail
        ( "Git test fixture command failed ("
            <> show code
            <> "): git -C "
            <> show directory
            <> " "
            <> show arguments
            <> "\nstdout: "
            <> Text.unpack (TextEncoding.decodeUtf8Lenient stdoutBytes)
            <> "\nstderr: "
            <> Text.unpack (TextEncoding.decodeUtf8Lenient stderrBytes)
        )

initTestRepository :: FilePath -> IO ()
initTestRepository directory = do
  createDirectoryIfMissing True directory
  _ <- gitSuccess directory ["init", "--initial-branch=main"] BS.empty
  _ <- gitSuccess directory ["config", "user.name", "ADRAI Haskell Test"] BS.empty
  _ <- gitSuccess directory ["config", "user.email", "adrai-haskell@example.invalid"] BS.empty
  createDirectoryIfMissing True (directory </> ".git" </> "adrai-no-hooks")
  _ <- gitSuccess directory ["config", "commit.gpgSign", "false"] BS.empty
  _ <- gitSuccess directory ["config", "tag.gpgSign", "false"] BS.empty
  _ <- gitSuccess directory ["config", "core.autocrlf", "false"] BS.empty
  _ <- gitSuccess directory ["config", "core.safecrlf", "false"] BS.empty
  _ <- gitSuccess directory ["config", "core.hooksPath", ".git/adrai-no-hooks"] BS.empty
  pure ()

initBareRepository :: FilePath -> IO ()
initBareRepository directory = do
  createDirectoryIfMissing True directory
  _ <- gitSuccess directory ["init", "--bare", "--initial-branch=main"] BS.empty
  pure ()

commitFile :: FilePath -> FilePath -> ByteString -> IO Text
commitFile repository relativePath bytes = commitFiles repository [(relativePath, bytes)]

commitFiles :: FilePath -> [(FilePath, ByteString)] -> IO Text
commitFiles repository files = do
  mapM_ writeOne files
  _ <- gitSuccess repository ["--literal-pathspecs", "add", "--all"] BS.empty
  _ <- gitSuccess repository ["commit", "-m", "fixture"] BS.empty
  outputText <$> gitSuccess repository ["rev-parse", "HEAD"] BS.empty
  where
    writeOne (relativePath, bytes) = do
      createDirectoryIfMissing True (takeDirectory (repository </> relativePath))
      BS.writeFile (repository </> relativePath) bytes

hashObject :: FilePath -> ByteString -> IO Text
hashObject repository bytes = outputText <$> gitSuccess repository ["hash-object", "-w", "--stdin"] bytes

outputText :: ByteString -> Text
outputText = Text.strip . TextEncoding.decodeUtf8Lenient

-- | Switch to a branch.
switchBranch :: FilePath -> Text -> IO ()
switchBranch repo branch = void $ gitSuccess repo ["switch", Text.unpack branch] BS.empty

-- | Create a new worktree with its own branch.
createWorktree :: FilePath -> FilePath -> Text -> IO ()
createWorktree repo path branch = void $ gitSuccess repo ["worktree", "add", "-b", Text.unpack branch, path, "HEAD"] BS.empty

-- | Remove a worktree (forcefully).
removeWorktree :: FilePath -> FilePath -> IO ()
removeWorktree repo path = void $ gitSuccess repo ["worktree", "remove", "--force", path] BS.empty

-- | Hard reset the current branch to the given target.
hardReset :: FilePath -> Text -> IO ()
hardReset repo target = void $ gitSuccess repo ["reset", "--hard", Text.unpack target] BS.empty

-- | Cherry-pick a commit onto the current branch.
cherryPick :: FilePath -> Text -> IO ()
cherryPick repo commit = void $ gitSuccess repo ["cherry-pick", Text.unpack commit] BS.empty

-- | Rebase the current branch onto the given target.
rebase :: FilePath -> Text -> IO ()
rebase repo target = void $ gitSuccess repo ["rebase", Text.unpack target] BS.empty

-- | Squash-merge a branch into the current branch.
squashMerge :: FilePath -> Text -> IO ()
squashMerge repo branch = do
  void $ gitSuccess repo ["merge", "--squash", Text.unpack branch] BS.empty
  void $ gitSuccess repo ["commit", "-m", "squash"] BS.empty

requireRepoPath :: Text -> RepoPath
requireRepoPath value =
  case mkRepoPath value of
    Left problem -> error ("invalid test RepoPath: " <> show problem)
    Right path -> path

requireRevision :: Text -> RevisionSpec
requireRevision value =
  case mkRevisionSpec value of
    Left problem -> error ("invalid test RevisionSpec: " <> show problem)
    Right revision -> revision
