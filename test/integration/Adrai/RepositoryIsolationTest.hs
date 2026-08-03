{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Adrai.RepositoryIsolationTest (tests) where

import Adrai.Git
import Adrai.GitTestSupport
import Adrai.Repository
import Adrai.Types (gitRefText, repoPathText)
import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Directory (Permissions (writable), getPermissions, removeFile, setPermissions)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Repository snapshot isolation"
    [ testCase "branch switch after resolution cannot change the bound snapshot" $
        withRepository $ \repository -> do
          _ <- commitFiles repository [(".adrai.toml", configBytes "roots/main-decisions" "roots/main-connections"), ("roots/main-decisions/main.decision.md", "main")]
          discovered <- requireRepository repository
          mainResolved <- requireResolved discovered "HEAD"
          _ <- gitSuccess repository ["switch", "-c", "feature"] BS.empty
          _ <- commitFiles repository [(".adrai.toml", configBytes "roots/feature-decisions" "roots/feature-connections"), ("roots/feature-decisions/feature.decision.md", "feature")]
          featureResolved <- requireResolved discovered "HEAD"
          _ <- gitSuccess repository ["switch", "main"] BS.empty
          featureSnapshot <- requireSnapshotAt featureResolved
          snapshotPaths featureSnapshot @?= ["roots/feature-decisions/feature.decision.md"]
          repositoryHeadState discovered >>= \case
            Right (GitHeadAttached reference) -> gitRefText reference @?= "refs/heads/main"
            result -> assertFailure (show result)
          mainSnapshot <- requireSnapshotAt mainResolved
          snapshotPaths mainSnapshot @?= ["roots/main-decisions/main.decision.md"]
          freshMain <- requireSnapshot discovered "HEAD"
          snapshotPaths freshMain @?= snapshotPaths mainSnapshot
          resolvedCommitOid (repositorySnapshotRevision featureSnapshot) @?= resolvedCommitOid featureResolved
          mapM_
            ( \_ -> do
                _ <- gitSuccess repository ["switch", "feature"] BS.empty
                currentFeature <- requireSnapshot discovered "HEAD"
                snapshotPaths currentFeature @?= snapshotPaths featureSnapshot
                _ <- gitSuccess repository ["switch", "main"] BS.empty
                currentMain <- requireSnapshot discovered "HEAD"
                snapshotPaths currentMain @?= snapshotPaths mainSnapshot
            )
            [1 :: Int, 2],
      testCase "dirty staged deleted and untracked worktree state never enters committed observations" $
        withRepository $ \repository -> do
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
      testCase "deleted refs do not invalidate an already resolved immutable commit" $
        withRepository $ \repository -> do
          _ <- commitFile repository "architecture/adrai/decisions/ref.decision.md" "ref"
          _ <- gitSuccess repository ["branch", "snapshot-ref"] BS.empty
          discovered <- requireRepository repository
          resolved <- requireResolved discovered "refs/heads/snapshot-ref"
          _ <- gitSuccess repository ["branch", "-D", "snapshot-ref"] BS.empty
          snapshot <- requireSnapshotAt resolved
          snapshotPaths snapshot @?= ["architecture/adrai/decisions/ref.decision.md"]
          resolveRepositoryRevision discovered (requireRevision "refs/heads/snapshot-ref") >>= \case
            Left (RepositorySnapshotGitError (GitUnknownRevision _)) -> pure ()
            result -> assertFailure ("expected deleted-ref resolution failure, got " <> show result),
      testCase "a pruned required blob fails the bound observation atomically" $
        withRepository $ \repository -> do
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
      testCase "unborn, detached, and direct-bare HEAD states remain advisory" $
        withSystemTempDirectory "adrai head states" $ \temporary -> do
          let unborn = temporary </> "unborn"
              bare = temporary </> "bare.git"
          initTestRepository unborn
          unbornRepository <- requireRepository unborn
          repositoryHeadState unbornRepository >>= \case
            Right (GitHeadAttached reference) -> gitRefText reference @?= "refs/heads/main"
            result -> assertFailure (show result)
          _ <- commitFile unborn "architecture/adrai/decisions/detached.decision.md" "detached"
          headOid <- outputText <$> gitSuccess unborn ["rev-parse", "HEAD"] BS.empty
          _ <- gitSuccess unborn ["switch", "--detach", Text.unpack headOid] BS.empty
          repositoryHeadState unbornRepository >>= (@?= Right GitHeadDetached)
          detached <- requireSnapshot unbornRepository "HEAD"
          snapshotPaths detached @?= ["architecture/adrai/decisions/detached.decision.md"]
          _ <- gitSuccess temporary ["clone", "--bare", unborn, bare] BS.empty
          _ <- gitSuccess bare ["symbolic-ref", "HEAD", "refs/heads/main"] BS.empty
          bareRepository <- requireRepository bare
          repositoryHeadState bareRepository >>= \case
            Right (GitHeadAttached reference) -> gitRefText reference @?= "refs/heads/main"
            result -> assertFailure (show result)
          bareSnapshot <- requireSnapshot bareRepository (Text.unpack headOid)
          snapshotPaths bareSnapshot @?= snapshotPaths detached,
      testCase "linked worktrees concurrently retain distinct immutable observations" $
        withRepository $ \repository -> do
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
            assertBool "revision keys share object namespace but not commit" (repositoryRevisionNamespace (resolvedRevisionKey mainResolved) == repositoryRevisionNamespace (resolvedRevisionKey linkedResolved) && repositoryRevisionOid (resolvedRevisionKey mainResolved) /= repositoryRevisionOid (resolvedRevisionKey linkedResolved)),
      testCase "sparse and shallow repositories observe committed trees by OID" $
        withSystemTempDirectory "adrai sparse shallow" $ \temporary -> do
          let source = temporary </> "source"
              shallow = temporary </> "shallow"
          initTestRepository source
          _ <- commitFiles source [(".adrai.toml", configBytes "hidden/decisions" "hidden/connections"), ("hidden/decisions/first.decision.md", "first")]
          _ <- commitFile source "hidden/decisions/second.decision.md" "second"
          _ <- gitSuccess source ["sparse-checkout", "init", "--cone"] BS.empty
          _ <- gitSuccess source ["sparse-checkout", "set", "visible"] BS.empty
          sourceRepository <- requireRepository source
          sparseSnapshot <- requireSnapshot sourceRepository "HEAD"
          snapshotPaths sparseSnapshot @?= ["hidden/decisions/first.decision.md", "hidden/decisions/second.decision.md"]
          let sourceUri = "file:///" <> map slash source
          _ <- gitSuccess temporary ["clone", "--depth", "1", sourceUri, shallow] BS.empty
          shallowRepository <- requireRepository shallow
          isShallowRepository shallowRepository >>= (@?= Right True)
          shallowSnapshot <- requireSnapshot shallowRepository "HEAD"
          snapshotPaths shallowSnapshot @?= snapshotPaths sparseSnapshot
          resolveRepositoryRevision shallowRepository (requireRevision "HEAD^") >>= \case
            Left (RepositorySnapshotGitError (GitUnknownRevision _)) -> pure ()
            result -> assertFailure ("expected missing shallow parent, got " <> show result),
      testCase "force reset changes only the newly resolved committed path set" $
        withRepository $ \repository -> do
          baseOid <- commitFile repository "seed.txt" "base"
          newerOid <- commitFile repository "architecture/adrai/decisions/reset.decision.md" "newer"
          discovered <- requireRepository repository
          newerResolved <- requireResolved discovered (Text.unpack newerOid)
          _ <- gitSuccess repository ["reset", "--hard", Text.unpack baseOid] BS.empty
          resetSnapshot <- requireSnapshot discovered "HEAD"
          snapshotPaths resetSnapshot @?= []
          boundNewer <- requireSnapshotAt newerResolved
          snapshotBytes boundNewer @?= Map.singleton "architecture/adrai/decisions/reset.decision.md" "newer"
          _ <- commitFile repository "architecture/adrai/decisions/reset.decision.md" "reintroduced"
          restored <- requireSnapshot discovered "HEAD"
          snapshotBytes restored @?= Map.singleton "architecture/adrai/decisions/reset.decision.md" "reintroduced"
    ]

withRepository :: (FilePath -> IO value) -> IO value
withRepository action =
  withSystemTempDirectory "adrai repository isolation" $ \temporary -> do
    let repository = temporary </> "repository"
    initTestRepository repository
    action repository

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

requireSnapshot :: Repository -> String -> IO RepositorySnapshot
requireSnapshot repository revision = requireResolved repository revision >>= requireSnapshotAt

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

configBytes :: Text.Text -> Text.Text -> BS.ByteString
configBytes decisions connections =
  TextEncoding.encodeUtf8
    ( Text.unlines
        [ "schema = 1",
          "",
          "[paths]",
          "decisions = \"" <> decisions <> "\"",
          "connections = \"" <> connections <> "\""
        ]
    )

slash :: Char -> Char
slash '\\' = '/'
slash value = value
