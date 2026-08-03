{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Adrai.RepositorySnapshotTest (tests) where

import Adrai.Git
import Adrai.GitTestSupport
import Adrai.Repository
import Adrai.Types (managedConnectionPath, managedDecisionPath, repoPathText)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Repository snapshots"
    [ testCase "missing config uses defaults and exact suffix selection" $
        withRepository $ \repository -> do
          _ <-
            commitFiles
              repository
              [ ("architecture/adrai/decisions/b.decision.md", "decision-b"),
                ("architecture/adrai/decisions/a.decision.md", "decision-a"),
                ("architecture/adrai/connections/x.connection.md", "connection-x"),
                ("architecture/adrai/connections/wrong.decision.md", "excluded"),
                ("architecture/adrai/decisions/wrong.connection.md", "excluded"),
                ("architecture/adrai/decisions/no.DECISION.md", "excluded"),
                ("architecture/adrai/decisions/a.decision.md.bak", "excluded")
              ]
          discovered <- requireRepository repository
          snapshot <- requireSnapshot discovered "HEAD"
          repositoryConfigOrigin (repositorySnapshotConfig snapshot) @?= DefaultConfigOrigin
          map (repoPathText . gitTreePath . repositoryTreeEntry) (repositorySnapshotEntries snapshot)
            @?= [ "architecture/adrai/connections/x.connection.md",
                  "architecture/adrai/decisions/a.decision.md",
                  "architecture/adrai/decisions/b.decision.md"
                ]
          observationBytes snapshot
            @?= Map.fromList
              [ ("architecture/adrai/connections/x.connection.md", Just "connection-x"),
                ("architecture/adrai/decisions/a.decision.md", Just "decision-a"),
                ("architecture/adrai/decisions/b.decision.md", Just "decision-b")
              ],
      testCase "committed config is strict: empty, invalid UTF-8, and invalid TOML fail" $
        withRepository $ \repository -> do
          _ <- commitFile repository "seed.txt" "seed"
          discovered <- requireRepository repository
          _ <- commitFile repository ".adrai.toml" BS.empty
          repositorySnapshot discovered (requireRevision "HEAD") >>= \case
            Left (RepositorySnapshotConfigParseError _ _) -> pure ()
            result -> assertFailure ("expected empty config parse error, got " <> show result)
          _ <- commitFile repository ".adrai.toml" (BS.pack [0x80])
          repositorySnapshot discovered (requireRevision "HEAD") >>= \case
            Left (RepositorySnapshotConfigInvalidUtf8 _) -> pure ()
            result -> assertFailure ("expected config UTF-8 error, got " <> show result)
          _ <- commitFile repository ".adrai.toml" "schema = ["
          repositorySnapshot discovered (requireRevision "HEAD") >>= \case
            Left (RepositorySnapshotConfigParseError _ _) -> pure ()
            result -> assertFailure ("expected TOML error, got " <> show result),
      testCase "raw observation retains invalid committed config without guessing managed roots" $
        withRepository $ \repository -> do
          let invalidConfig = "schema = ["
          _ <- commitFile repository ".adrai.toml" invalidConfig
          discovered <- requireRepository repository
          resolved <- resolveRepositoryRevision discovered (requireRevision "HEAD") >>= \case
            Left problem -> assertFailure (show problem)
            Right value -> pure value
          raw <- observeRawRepositorySnapshotAt resolved >>= \case
            Left problem -> assertFailure (show problem)
            Right value -> pure value
          let config = rawRepositorySnapshotConfig raw
          rawRepositoryConfigOrigin config @?= CommittedConfigOrigin
          fmap gitBlobBytes (rawRepositoryConfigBlob config) @?= Just invalidConfig
          fmap gitTreeOid (rawRepositoryConfigEntry config) @?= fmap gitBlobOid (rawRepositoryConfigBlob config)
          case rawRepositoryConfigResult config of
            Left (RepositoryConfigFailureParse _ _) -> pure ()
            result -> assertFailure ("expected retained raw config parse failure, got " <> show result)
          rawRepositoryConfigManagedPaths config @?= Nothing
          rawRepositorySnapshotManagedPaths raw @?= Nothing
          rawRepositorySnapshotEntries raw @?= [],
      testCase "Unicode configured roots preserve repeated OIDs and arbitrary managed bytes" $
        withRepository $ \repository -> do
          let config = configBytes "arkitektur/beslutninger" "arkitektur/forbindelser"
              repeated = TextEncoding.encodeUtf8 "samme København"
              invalid = BS.pack [0x66, 0x80]
          _ <-
            commitFiles
              repository
              [ (".adrai.toml", config),
                ("arkitektur/beslutninger/å.decision.md", repeated),
                ("arkitektur/beslutninger/ø.decision.md", repeated),
                ("arkitektur/forbindelser/rå.connection.md", invalid),
                ("architecture/adrai/decisions/default.decision.md", "must not leak")
              ]
          discovered <- requireRepository repository
          snapshot <- requireSnapshot discovered "HEAD"
          repositoryConfigOrigin (repositorySnapshotConfig snapshot) @?= CommittedConfigOrigin
          repoPathText (managedDecisionPath (repositorySnapshotManagedPaths snapshot)) @?= "arkitektur/beslutninger"
          repoPathText (managedConnectionPath (repositorySnapshotManagedPaths snapshot)) @?= "arkitektur/forbindelser"
          let observations = repositorySnapshotEntries snapshot
              duplicateOids = [gitTreeOid (repositoryTreeEntry value) | value <- observations, ".decision.md" `Text.isSuffixOf` repoPathText (gitTreePath (repositoryTreeEntry value))]
          length duplicateOids @?= 2
          assertBool "both paths reuse the same blob OID" (case duplicateOids of [firstOid, secondOid] -> firstOid == secondOid; _ -> False)
          observationBytes snapshot
            @?= Map.fromList
              [ ("arkitektur/beslutninger/å.decision.md", Just repeated),
                ("arkitektur/beslutninger/ø.decision.md", Just repeated),
                ("arkitektur/forbindelser/rå.connection.md", Just invalid)
              ],
      testCase "config blob modes are literal while a nonblob config is rejected" $
        withRepository $ \repository -> do
          _ <- commitFile repository "seed.txt" "seed"
          discovered <- requireRepository repository
          configOid <- hashObject repository (configBytes "custom/decisions" "custom/connections")
          executableTree <- outputText <$> gitSuccess repository ["mktree", "-z"] ("100755 blob " <> TextEncoding.encodeUtf8 configOid <> "\t.adrai.toml\NUL")
          executableCommit <- outputText <$> gitSuccess repository ["commit-tree", Text.unpack executableTree] "executable config\n"
          executableSnapshot <- requireSnapshot discovered executableCommit
          fmap gitTreeMode (repositoryConfigEntry (repositorySnapshotConfig executableSnapshot)) @?= Just GitExecutableFile
          symlinkTree <- outputText <$> gitSuccess repository ["mktree", "-z"] ("120000 blob " <> TextEncoding.encodeUtf8 configOid <> "\t.adrai.toml\NUL")
          symlinkCommit <- outputText <$> gitSuccess repository ["commit-tree", Text.unpack symlinkTree] "symlink config\n"
          symlinkSnapshot <- requireSnapshot discovered symlinkCommit
          fmap gitTreeMode (repositoryConfigEntry (repositorySnapshotConfig symlinkSnapshot)) @?= Just GitSymbolicLink
          basisCommit <- outputText <$> gitSuccess repository ["rev-parse", "HEAD"] BS.empty
          nonblobTree <- outputText <$> gitSuccess repository ["mktree", "-z"] ("160000 commit " <> TextEncoding.encodeUtf8 basisCommit <> "\t.adrai.toml\NUL")
          nonblobCommit <- outputText <$> gitSuccess repository ["commit-tree", Text.unpack nonblobTree] "nonblob config\n"
          repositorySnapshot discovered (requireRevision nonblobCommit) >>= \case
            Left (RepositorySnapshotConfigNotBlob entry) -> gitTreeObjectType entry @?= GitCommitObject
            result -> assertFailure ("expected nonblob config error, got " <> show result),
      testCase "selected symlink blobs and gitlinks preserve raw metadata without interpretation" $
        withRepository $ \repository -> do
          basisText <- commitFile repository "seed.txt" "seed"
          blobText <- hashObject repository "literal-link-target"
          _ <- gitSuccess repository ["update-index", "--add", "--cacheinfo", "120000," <> Text.unpack blobText <> ",architecture/adrai/decisions/link.decision.md"] BS.empty
          _ <- gitSuccess repository ["update-index", "--add", "--cacheinfo", "160000," <> Text.unpack basisText <> ",architecture/adrai/decisions/module.decision.md"] BS.empty
          treeText <- outputText <$> gitSuccess repository ["write-tree"] BS.empty
          commitText <- outputText <$> gitSuccess repository ["commit-tree", Text.unpack treeText] "raw modes\n"
          discovered <- requireRepository repository
          snapshot <- requireSnapshot discovered commitText
          case repositorySnapshotEntries snapshot of
            [linkObservation, moduleObservation] -> do
              gitTreeMode (repositoryTreeEntry linkObservation) @?= GitSymbolicLink
              fmap gitBlobBytes (repositoryTreeBlob linkObservation) @?= Just "literal-link-target"
              gitTreeMode (repositoryTreeEntry moduleObservation) @?= GitSubmodule
              repositoryTreeBlob moduleObservation @?= Nothing
            observations -> assertFailure ("unexpected raw-mode observations: " <> show observations),
      testCase "257 unique selected blobs cross the inherited batch boundary deterministically" $
        withRepository $ \repository -> do
          _ <- commitFiles repository (indexedFiles 257)
          discovered <- requireRepository repository
          firstSnapshot <- requireSnapshot discovered "HEAD"
          secondSnapshot <- requireSnapshot discovered "HEAD"
          length (repositorySnapshotEntries firstSnapshot) @?= 257
          observationBytes firstSnapshot @?= observationBytes secondSnapshot
    ]

withRepository :: (FilePath -> IO value) -> IO value
withRepository action =
  withSystemTempDirectory "adrai repository snapshot" $ \temporary -> do
    let repository = temporary </> "repository"
    initTestRepository repository
    action repository

requireRepository :: FilePath -> IO Repository
requireRepository path =
  discoverRepository systemGit path >>= \case
    Left problem -> assertFailure (show problem)
    Right repository -> pure repository

requireSnapshot :: Repository -> Text.Text -> IO RepositorySnapshot
requireSnapshot repository revision =
  repositorySnapshot repository (requireRevision revision) >>= \case
    Left problem -> assertFailure (show problem)
    Right snapshot -> pure snapshot

observationBytes :: RepositorySnapshot -> Map.Map Text.Text (Maybe ByteString)
observationBytes snapshot =
  Map.fromList
    [ ( repoPathText (gitTreePath (repositoryTreeEntry observation)),
        gitBlobBytes <$> repositoryTreeBlob observation
      )
      | observation <- repositorySnapshotEntries snapshot
    ]

configBytes :: Text.Text -> Text.Text -> ByteString
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

indexedFiles :: Int -> [(FilePath, ByteString)]
indexedFiles count =
  [ ( "architecture/adrai/decisions/item-" <> padded index <> ".decision.md",
      TextEncoding.encodeUtf8 ("blob-" <> Text.pack (show index))
    )
    | index <- [0 .. count - 1]
  ]
  where
    padded index = replicate (4 - length shown) '0' <> shown
      where
        shown = show index
