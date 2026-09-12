{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Adrai.RepositorySnapshotTest (tests) where

import Adrai.Git
import Adrai.GitTestSupport
import Adrai.Repository
import Adrai.RetainedNative.RepositorySeed
  ( RepositorySeed,
    createRepositorySeed,
    removeRepositorySeed,
    withRepositorySeedCopy,
  )
import Adrai.Types (managedConnectionPath, managedDecisionPath, repoPathText)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Control.Monad (forM_)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Lazy as LBS
import System.Directory (copyFile)
import System.Environment (getExecutablePath)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import System.Process.Typed (proc, readProcess)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  withResource createEmptyRepositorySeed removeRepositorySeed $ \getRepositorySeed ->
    testGroup
      "Repository snapshots"
      [ testCase "missing config uses defaults and exact suffix selection" $
        withRepository getRepositorySeed $ \repository -> do
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
      testCase "shared session combines Unicode paths, repeated blobs, arbitrary bytes, and nonblob metadata" (combinedPersistentSessionComposition getRepositorySeed),
      testCase "late managed-tree corruption promotes Git error without a later request" (nativeLateManagedTreeFailure getRepositorySeed),
      testCase "shared session preserves contradictory-path errors as inner repository errors" (contradictoryPathPromotionIdentity getRepositorySeed),
      testCase "raw observation retains invalid committed config without guessing managed roots" $
        withRepository getRepositorySeed $ \repository -> do
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
      testCase "nonblob config is rejected" $
        withRepository getRepositorySeed $ \repository -> do
          _ <- commitFile repository "seed.txt" "seed"
          discovered <- requireRepository repository
          basisCommit <- outputText <$> gitSuccess repository ["rev-parse", "HEAD"] BS.empty
          nonblobTree <- outputText <$> gitSuccess repository ["mktree", "-z"] ("160000 commit " <> TextEncoding.encodeUtf8 basisCommit <> "\t.adrai.toml\NUL")
          nonblobCommit <- outputText <$> gitSuccess repository ["commit-tree", Text.unpack nonblobTree] "nonblob config\n"
          repositorySnapshot discovered (requireRevision nonblobCommit) >>= \case
            Left (RepositorySnapshotConfigNotBlob entry) -> gitTreeObjectType entry @?= GitCommitObject
            result -> assertFailure ("expected nonblob config error, got " <> show result)
      ]

createEmptyRepositorySeed :: IO RepositorySeed
createEmptyRepositorySeed = createRepositorySeed "adrai empty repository seed" initTestRepository

withRepository :: IO RepositorySeed -> (FilePath -> IO value) -> IO value
withRepository getSeed action = do
  seed <- getSeed
  withRepositorySeedCopy seed "adrai repository snapshot" $ \_ repository -> do
    _ <- gitSuccess repository ["config", "core.hooksPath", repository </> ".git" </> "adrai-no-hooks"] BS.empty
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

contradictoryPathPromotionIdentity :: IO RepositorySeed -> IO ()
contradictoryPathPromotionIdentity getSeed =
  withRepository getSeed $ \repository ->
    withSystemTempDirectory "adrai native contradictory path" $ \temporary -> do
    let configured = configBytes "custom/decisions" "custom/connections"
        path = requireRepoPath "custom/decisions/same.decision.md"
        helper = temporary </> "adrai-native-git-fixture-batch-p6133.exe"
    _ <- commitFile repository ".adrai.toml" configured
    discovered <- requireRepository repository
    executable <- getExecutablePath
    copyFile executable helper
    configOid <- GitOid <$> hashObject repository configured
    firstOid <- GitOid <$> hashObject repository "first"
    secondOid <- GitOid <$> hashObject repository "second"
    BS.writeFile (temporary </> "fixture-config-tree") ("100644 blob " <> TextEncoding.encodeUtf8 (gitOidText configOid) <> "\t.adrai.toml\NUL")
    BS.writeFile (temporary </> "fixture-managed-tree") ("100644 blob " <> TextEncoding.encodeUtf8 (gitOidText firstOid) <> "\tcustom/decisions/same.decision.md\NUL100644 blob " <> TextEncoding.encodeUtf8 (gitOidText secondOid) <> "\tcustom/decisions/same.decision.md\NUL")
    BS.writeFile (temporary </> "fixture-blob-" <> Text.unpack (gitOidText configOid)) configured
    writeFile (temporary </> "fixture-shallow") "false\n"
    writeFile (temporary </> "fixture-malformed-after") "99"
    resolved <- resolveRepositoryRevision discovered (requireRevision "HEAD") >>= either (assertFailure . show) pure
    let first = GitTreeEntry path firstOid GitBlobObject GitRegularFile
        second = GitTreeEntry path secondOid GitBlobObject GitRegularFile
        candidates
          | gitOidText firstOid <= gitOidText secondOid = [first, second]
          | otherwise = [second, first]
        expected = RepositorySnapshotContradictoryPath path candidates
        fixtureRepository = discovered {repositoryClient = GitClient helper}
        wrapped = resolved {resolvedRepository = fixtureRepository}
    listTreeEntriesAt fixtureRepository (resolvedCommitOid resolved) [requireRepoPath "custom/decisions", requireRepoPath "custom/connections"] >>= \case
      Left (GitInvalidOutput "list tree" (GitMalformedTreeRecord "contradictory duplicate path")) -> pure ()
      result -> assertFailure ("expected strict list-tree duplicate rejection, got " <> show result)
    observeRawRepositorySnapshotAt wrapped >>= (@?= Left expected)
    lines <$> readFile (temporary </> "fixture-requests") >>= (@?= [Text.unpack (gitOidText configOid)])
    helperPid <- read <$> readFile (temporary </> "fixture-helper.pid")
    waitForExactFixturePidAbsence helperPid 50
    requireSnapshot discovered "HEAD" >>= \snapshot -> repositoryConfigOrigin (repositorySnapshotConfig snapshot) @?= CommittedConfigOrigin

combinedPersistentSessionComposition :: IO RepositorySeed -> IO ()
combinedPersistentSessionComposition getSeed =
  withRepository getSeed $ \repository ->
    withSystemTempDirectory "adrai combined repository blob-session" $ \temporary -> do
      let decisionRoot = "arkitektur/beslutninger med mellemrum"
          connectionRoot = "arkitektur/forbindelser ø"
          configured = configBytes decisionRoot connectionRoot
          repeatedBytes = TextEncoding.encodeUtf8 "samme København med mellemrum"
          arbitraryBytes = BS.pack [0x66, 0x80, 0x00, 0xff]
          uniqueFiles =
            [ ( Text.unpack decisionRoot <> "/item-" <> padded index <> ".decision.md",
                TextEncoding.encodeUtf8 ("decision-" <> Text.pack (show index))
              )
              | index <- [0 :: Int .. 3]
            ]
          repeatedFiles =
            [ (Text.unpack decisionRoot <> "/gentaget å.decision.md", repeatedBytes),
              (Text.unpack decisionRoot <> "/gentaget ø.decision.md", repeatedBytes)
            ]
          arbitraryPath = Text.unpack connectionRoot <> "/rå bytes.connection.md"
          arbitraryFile = (arbitraryPath, arbitraryBytes)
          blobFiles = uniqueFiles <> repeatedFiles <> [arbitraryFile]
          gitlinkPath = Text.unpack decisionRoot <> "/modul med mellemrum.decision.md"
          traceFile = temporary </> "git-argv.txt"
          blobTraceFile = temporary </> "blob-sessions.txt"
          wrapper = temporary </> "tracing-git.cmd"
          padded index = replicate (4 - length shown) '0' <> shown
            where
              shown = show index
      _ <- commitFiles repository ((".adrai.toml", configured) : blobFiles)
      parentCommit <- outputText <$> gitSuccess repository ["rev-parse", "HEAD"] BS.empty
      _ <-
        gitSuccess
          repository
          ["update-index", "--add", "--cacheinfo", "160000," <> Text.unpack parentCommit <> "," <> gitlinkPath]
          BS.empty
      tree <- outputText <$> gitSuccess repository ["write-tree"] BS.empty
      combinedCommit <-
        outputText
          <$> gitSuccess
            repository
            ["commit-tree", Text.unpack tree, "-p", Text.unpack parentCommit]
            "combined persistent session composition\n"
      BS.writeFile
        wrapper
        ( "@echo off\r\necho %*>> \""
            <> BS8.pack traceFile
            <> "\"\r\necho %* | findstr /c:\"cat-file\" >nul\r\nif errorlevel 1 goto run\r\necho blob>> \""
            <> BS8.pack blobTraceFile
            <> "\"\r\n:run\r\ngit %*\r\n"
        )
      discovered <- requireRepository repository
      snapshot <- requireSnapshot (discovered {repositoryClient = GitClient wrapper}) combinedCommit
      let observations = repositorySnapshotEntries snapshot
          observedPaths = map (repoPathText . gitTreePath . repositoryTreeEntry) observations
          byPath = Map.fromList [(repoPathText (gitTreePath (repositoryTreeEntry observation)), observation) | observation <- observations]
          repeatedPathA = decisionRoot <> "/gentaget å.decision.md"
          repeatedPathB = decisionRoot <> "/gentaget ø.decision.md"
          gitlinkRepoPath = Text.pack gitlinkPath
          expectedPaths = sort (map (Text.pack . fst) blobFiles <> [gitlinkRepoPath])
      repositoryConfigOrigin (repositorySnapshotConfig snapshot) @?= CommittedConfigOrigin
      repoPathText (managedDecisionPath (repositorySnapshotManagedPaths snapshot)) @?= decisionRoot
      repoPathText (managedConnectionPath (repositorySnapshotManagedPaths snapshot)) @?= connectionRoot
      length observations @?= 8
      observedPaths @?= expectedPaths
      case (Map.lookup repeatedPathA byPath, Map.lookup repeatedPathB byPath) of
        (Just firstRepeated, Just secondRepeated) -> do
          gitTreeOid (repositoryTreeEntry firstRepeated) @?= gitTreeOid (repositoryTreeEntry secondRepeated)
          fmap gitBlobBytes (repositoryTreeBlob firstRepeated) @?= Just repeatedBytes
          fmap gitBlobBytes (repositoryTreeBlob secondRepeated) @?= Just repeatedBytes
        pair -> assertFailure ("missing repeated-path observations: " <> show pair)
      fmap (fmap gitBlobBytes . repositoryTreeBlob) (Map.lookup (Text.pack arbitraryPath) byPath) @?= Just (Just arbitraryBytes)
      case Map.lookup gitlinkRepoPath byPath of
        Nothing -> assertFailure "combined fixture omitted its managed gitlink"
        Just gitlinkObservation -> do
          gitTreeMode (repositoryTreeEntry gitlinkObservation) @?= GitSubmodule
          gitTreeObjectType (repositoryTreeEntry gitlinkObservation) @?= GitCommitObject
          repositoryTreeBlob gitlinkObservation @?= Nothing
      invocations <- BS.readFile traceFile
      blobSessions <- BS.readFile blobTraceFile
      length (BS8.lines blobSessions) @?= 1
      assertBool "combined snapshot must use one unbuffered blob child" (not ("--buffer" `BS.isInfixOf` invocations))

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

nativeLateManagedTreeFailure :: IO RepositorySeed -> IO ()
nativeLateManagedTreeFailure getSeed =
  withRepository getSeed $ \repository ->
    withSystemTempDirectory "adrai native late managed-tree" $ \temporary -> do
      let helper = temporary </> "adrai-native-git-fixture-batch-p6133.exe"
          configured = configBytes "custom/decisions" "custom/connections"
          managed = [("custom/decisions/item-" <> pad n <> ".decision.md", TextEncoding.encodeUtf8 ("decision-" <> Text.pack (show n))) | n <- [0 :: Int .. 1]]
          pad n = replicate (4 - length shown) '0' <> shown where shown = show n
      _ <- commitFiles repository ((".adrai.toml", configured) : managed)
      discovered <- requireRepository repository
      executable <- getExecutablePath
      copyFile executable helper
      configTree <- gitSuccess repository ["ls-tree", "-z", "HEAD", "--", ".adrai.toml"] BS.empty
      managedTree <- gitSuccess repository ["ls-tree", "-rz", "HEAD", "--", "custom/decisions", "custom/connections"] BS.empty
      BS.writeFile (temporary </> "fixture-config-tree") configTree
      BS.writeFile (temporary </> "fixture-managed-tree") managedTree
      resolved <- resolveRepositoryRevision discovered (requireRevision "HEAD") >>= either (assertFailure . show) pure
      entries <- listTreeEntriesAt discovered (resolvedCommitOid resolved) [requireRepoPath "custom/decisions", requireRepoPath "custom/connections"] >>= either (assertFailure . show) pure
      configEntry <- lookupTreeEntryAt discovered (resolvedCommitOid resolved) (requireRepoPath ".adrai.toml") >>= \case
        Right (Just entry) -> pure entry
        result -> assertFailure (show result) >> fail "unreachable"
      blobs <- readBlobBatch discovered (gitTreeOid configEntry : [gitTreeOid entry | entry <- entries, gitTreeObjectType entry == GitBlobObject]) >>= either (assertFailure . show) pure
      forM_ (Map.elems blobs) $ \blob -> BS.writeFile (temporary </> "fixture-blob-" <> Text.unpack (gitOidText (gitBlobOid blob))) (gitBlobBytes blob)
      writeFile (temporary </> "fixture-malformed-after") "3"
      let wrapped = resolved {resolvedRepository = discovered {repositoryClient = GitClient helper}}
      timeout (5 * 1000000) (observeRawRepositorySnapshotAt wrapped) >>= \case
        Just (Left (RepositorySnapshotGitError (GitInvalidOutput "cat-file batch" _))) -> pure ()
        result -> assertFailure ("expected native late batch failure, got " <> show result)
      linesSeen <- lines <$> readFile (temporary </> "fixture-requests")
      length linesSeen @?= 3
      helperPid <- read <$> readFile (temporary </> "fixture-helper.pid")
      waitForExactFixturePidAbsence helperPid 50
      requireSnapshot discovered "HEAD" >>= \snapshot -> length (repositorySnapshotEntries snapshot) @?= 2

waitForExactFixturePidAbsence :: Int -> Int -> IO ()
waitForExactFixturePidAbsence pid attempts = do
  (_, output, _) <- readProcess (proc "powershell.exe" ["-NoProfile", "-NonInteractive", "-Command", "$ErrorActionPreference='Stop'; try { [void][Diagnostics.Process]::GetProcessById(" <> show pid <> "); [Console]::Out.Write('present') } catch [ArgumentException] { [Console]::Out.Write('absent') }"])
  if LBS.toStrict output == "absent"
    then pure ()
    else if attempts <= 0
      then assertFailure "exact native Repository helper PID remained after cleanup"
      else threadDelay 100000 >> waitForExactFixturePidAbsence pid (attempts - 1)
