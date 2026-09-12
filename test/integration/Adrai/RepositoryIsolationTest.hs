{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Adrai.RepositoryIsolationTest (tests) where

import Adrai.Git
import Adrai.GitTestSupport
import Adrai.Repository
import Adrai.RetainedNative.ResidualRepositorySeed
  ( RepositorySeed,
    createRepositorySeed,
    removeRepositorySeed,
    withRepositorySeedCopy,
  )
import Adrai.Types (repoPathText)
import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import System.Directory (Permissions (writable), getPermissions, removeFile, setPermissions)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  withResource createEmptyRepositorySeed removeRepositorySeed $ \getRepositorySeed ->
    testGroup
      "Repository snapshot isolation"
      [ testCase "dirty staged deleted and untracked worktree state never enters committed observations" $
        withRepository getRepositorySeed $ \repository -> do
          _ <- commitFile repository "architecture/adrai/decisions/stable.decision.md" "committed"
          discovered <- requireRepository repository
          resolved <- requireResolved discovered "HEAD"
          BS.writeFile (repository </> "architecture/adrai/decisions/stable.decision.md") "dirty"
          BS.writeFile (repository </> "architecture/adrai/decisions/staged.decision.md") "staged"
          _ <- gitSuccess repository ["add", "architecture/adrai/decisions/staged.decision.md"] BS.empty
          BS.writeFile (repository </> "architecture/adrai/decisions/untracked.decision.md") "untracked"
          snapshot <- requireSnapshotAt resolved
          snapshotBytes snapshot @?= Map.singleton "architecture/adrai/decisions/stable.decision.md" "committed"
          removeFile (repository </> "architecture/adrai/decisions/stable.decision.md")
          afterDelete <- requireSnapshotAt resolved
          snapshotBytes afterDelete @?= snapshotBytes snapshot,
      testCase "a pruned required blob fails the bound observation atomically" $
        withRepository getRepositorySeed $ \repository -> do
          _ <-
            commitFiles
              repository
              [ ("architecture/adrai/decisions/available.decision.md", "available"),
                ("architecture/adrai/decisions/pruned.decision.md", "pruned")
              ]
          discovered <- requireRepository repository
          resolved <- requireResolved discovered "HEAD"
          initial <- requireSnapshotAt resolved
          prunedOid <-
            case
                [ gitTreeOid (repositoryTreeEntry observation)
                  | observation <- repositorySnapshotEntries initial,
                    repoPathText (gitTreePath (repositoryTreeEntry observation)) == "architecture/adrai/decisions/pruned.decision.md"
                ]
              of
                [oid] -> pure oid
                result -> assertFailure ("expected one pruned blob fixture, got " <> show result)
          removeLooseObject (looseObjectPath discovered prunedOid)
          repositorySnapshotAt resolved >>= \case
            Left (RepositorySnapshotGitError (GitObjectMissing missingOid)) -> missingOid @?= prunedOid
            result -> assertFailure ("expected exact missing-object failure, got " <> show result),
      testCase "linked worktrees concurrently retain distinct immutable observations" $
        withRepository getRepositorySeed $ \repository -> do
          _ <- commitFile repository "architecture/adrai/decisions/main.decision.md" "main"
          _ <- gitSuccess repository ["switch", "-c", "feature"] BS.empty
          _ <- commitFile repository "architecture/adrai/decisions/feature.decision.md" "feature"
          _ <- gitSuccess repository ["switch", "main"] BS.empty
          withSystemTempDirectory "adrai linked snapshot" $ \temporary -> do
            let linked = temporary </> "linked"
            _ <- gitSuccess repository ["worktree", "add", linked, "feature"] BS.empty
            mainRepository <- requireRepository repository
            linkedRepository <- requireRepository linked
            mainResolved <- requireResolved mainRepository "HEAD"
            linkedResolved <- requireResolved linkedRepository "HEAD"
            (mainResult, linkedResult) <- Async.concurrently (repositorySnapshotAt mainResolved) (repositorySnapshotAt linkedResolved)
            mainSnapshot <- requireSnapshotResult mainResult
            linkedSnapshot <- requireSnapshotResult linkedResult
            snapshotPaths mainSnapshot @?= ["architecture/adrai/decisions/main.decision.md"]
            snapshotPaths linkedSnapshot @?= ["architecture/adrai/decisions/feature.decision.md", "architecture/adrai/decisions/main.decision.md"]
            assertBool "revision keys share object namespace but not commit" (repositoryRevisionNamespace (resolvedRevisionKey mainResolved) == repositoryRevisionNamespace (resolvedRevisionKey linkedResolved) && repositoryRevisionOid (resolvedRevisionKey mainResolved) /= repositoryRevisionOid (resolvedRevisionKey linkedResolved))
      ]

createEmptyRepositorySeed :: IO RepositorySeed
createEmptyRepositorySeed =
  createRepositorySeed "adrai-repository-isolation-seed" initTestRepository

withRepository :: IO RepositorySeed -> (FilePath -> IO value) -> IO value
withRepository getRepositorySeed action = do
  seed <- getRepositorySeed
  withRepositorySeedCopy seed "adrai repository isolation" action

requireRepository :: FilePath -> IO Repository
requireRepository path =
  discoverRepository systemGit path >>= \case
    Left problem -> assertFailure (show problem)
    Right repository -> pure repository

requireResolved :: Repository -> String -> IO ResolvedRepositoryRevision
requireResolved repository revision =
  resolveRepositoryRevision repository (requireRevision (Text.pack revision)) >>= \case
    Left problem -> assertFailure (show problem)
    Right resolved -> pure resolved

requireSnapshotAt :: ResolvedRepositoryRevision -> IO RepositorySnapshot
requireSnapshotAt resolved = repositorySnapshotAt resolved >>= requireSnapshotResult

requireSnapshotResult :: Either RepositorySnapshotError RepositorySnapshot -> IO RepositorySnapshot
requireSnapshotResult = \case
  Left problem -> assertFailure (show problem)
  Right snapshot -> pure snapshot

snapshotPaths :: RepositorySnapshot -> [Text.Text]
snapshotPaths = map (repoPathText . gitTreePath . repositoryTreeEntry) . repositorySnapshotEntries

snapshotBytes :: RepositorySnapshot -> Map.Map Text.Text BS.ByteString
snapshotBytes snapshot =
  Map.fromList
    [ (repoPathText (gitTreePath (repositoryTreeEntry observation)), gitBlobBytes blob)
      | observation <- repositorySnapshotEntries snapshot,
        Just blob <- [repositoryTreeBlob observation]
    ]

looseObjectPath :: Repository -> GitOid -> FilePath
looseObjectPath repository oid =
  repositoryCommonDir repository </> "objects" </> take 2 encoded </> drop 2 encoded
  where
    encoded = Text.unpack (gitOidText oid)

removeLooseObject :: FilePath -> IO ()
removeLooseObject path = do
  permissions <- getPermissions path
  setPermissions path permissions {writable = True}
  removeFile path
