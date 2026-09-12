{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Adrai.GitDiscoveryTest (tests) where

import Adrai.Git
import Adrai.GitTestSupport
import Adrai.Provenance (mkGitOid)
import Control.Exception (onException)
import qualified Data.Map.Strict as Map
import System.Directory
  ( createDirectoryIfMissing,
    removePathForcibly,
  )
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory, withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  withResource acquireDiscoverySeed removePathForcibly $ \getSeed ->
    testGroup
      "Git discovery"
      [ testCase "non-repository and unavailable executable are structured errors" $
          withSystemTempDirectory "adrai git nonrepo" $ \directory -> do
            discoverRepository systemGit directory >>= \case
              Left (GitNotRepository _) -> pure ()
              result -> assertFailure ("expected GitNotRepository, got " <> show result)
            let missingPath = directory </> "missing-git-executable"
                missing = either (error . show) id (gitClient missingPath)
            discoverRepository missing directory >>= \case
              Left (GitExecutableUnavailable actualPath) -> actualPath @?= missingPath
              result -> assertFailure ("expected GitExecutableUnavailable, got " <> show result),
        testCase "main worktree discovery is canonical from a Unicode subdirectory" $ do
          temporary <- getSeed
          let repository = temporary </> "repo space København"
              subdirectory = repository </> "nested" </> "start"
          discoverRepository systemGit subdirectory >>= \case
            Left problem -> assertFailure (show problem)
            Right discovered -> do
              repositoryLayout discovered @?= MainWorktree
              repositoryWorktreeRoot discovered @?= Just (repositoryCommandDirectory discovered)
              assertBool "main git/common dirs are equal" (repositoryGitDir discovered == repositoryCommonDir discovered)
              repositoryCommonIsBare discovered @?= False,
        testCase "direct bare discovery supports objects but has no filesystem worktree" $
          withSystemTempDirectory "adrai git bare" $ \temporary -> do
            let repository = temporary </> "bare repo.git"
            initBareRepository repository
            discoverRepository systemGit repository >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> do
                repositoryLayout discovered @?= BareRepository
                repositoryWorktreeRoot discovered @?= Nothing
                repositoryCommonIsBare discovered @?= True
                readWorktreeFileBytes discovered (requireRepoPath "anything.txt") >>= (@?= Left GitWorktreeRequired)
                blobText <- hashObject repository "bare object bytes"
                let blobOid = either (error . show) id (mkGitOid blobText)
                readBlobBatch discovered [blobOid] >>= \case
                  Left problem -> assertFailure (show problem)
                  Right blobs -> gitBlobBytes (blobs Map.! blobOid) @?= "bare object bytes"
      ]

acquireDiscoverySeed :: IO FilePath
acquireDiscoverySeed = do
  temporaryRoot <- getCanonicalTemporaryDirectory
  temporary <- createTempDirectory temporaryRoot "adrai git discovery seed"
  let repository = temporary </> "repo space København"
      subdirectory = repository </> "nested" </> "start"
      setup = do
        initTestRepository repository
        _ <- commitFile repository "seed.txt" "seed"
        createDirectoryIfMissing True subdirectory
  setup `onException` removePathForcibly temporary
  pure temporary
