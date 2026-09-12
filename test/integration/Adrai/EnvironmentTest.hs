{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Branch, worktree and environment integration tests.
--
-- Ports the environment tests from
-- ``ADRAI_1_Source/tests/test_repository_environments.py`` and
-- ``ADRAI_1_Source/tests/test_branch_switch_consistency.py``.
module Adrai.EnvironmentTest (tests) where

import Adrai.Integration.CLI
import Adrai.Git (Repository, RepositoryLayout (LinkedWorktree), discoverRepository, repositoryCommonDir, repositoryGitDir, repositoryLayout, systemGit)
import Adrai.Provenance.Git.Lock
  ( GitLockError (LockHeld),
    acquireGitLock,
    gitLockPath,
    gitLockPid,
    gitLockStatus,
    releaseGitLock,
    withGitLock,
  )
import Control.Exception (finally, try)
import Control.Monad (forM, forM_)
import System.Exit (ExitCode (..))
import qualified Data.Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.List (sort)
import Data.Maybe (listToMaybe)
import Data.Text (Text, strip, unpack, pack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.SQLite.Simple
  ( Only (..),
    close,
    open,
    query,
  )
import System.Directory
  ( canonicalizePath,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    pathIsSymbolicLink,
    removeDirectory,
  )
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute, makeRelative, takeDirectory, (</>))
import System.Info (os)
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (proc, readProcess, runProcess, shell)
import System.Win32 (getCurrentProcessId)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( (@?=),
    assertBool,
    assertFailure,
    testCase,
  )

-- =====================================================================
-- JSON helpers
-- =====================================================================

_Object :: Data.Aeson.Value -> Maybe (KM.KeyMap Data.Aeson.Value)
_Object (Data.Aeson.Object o) = Just o
_Object _ = Nothing

(.:) :: Data.Aeson.FromJSON a => KM.KeyMap Data.Aeson.Value -> Text -> Maybe a
(.:) km key =
  case KM.lookup (AesonKey.fromText key) km of
    Nothing -> Nothing
    Just v -> case Data.Aeson.eitherDecode (Data.Aeson.encode v) of
      Left _ -> Nothing
      Right a -> Just a

-- | Extract the ADR ID from a create-adr result value.
extractAdrId :: Data.Aeson.Value -> Maybe Text
extractAdrId v = do
  o <- _Object v
  o .: "adr"

extractRecord :: Data.Aeson.Value -> Maybe Text
extractRecord v = do
  o <- _Object v
  o .: "record"

-- | Extract the commit hash from a create-adr result value.
extractCommit :: Data.Aeson.Value -> Maybe Text
extractCommit v = do
  o <- _Object v
  o .: "commit"

-- | Extract the list of created paths from a create-adr result.
extractCreated :: Data.Aeson.Value -> Maybe [Text]
extractCreated v = do
  o <- _Object v
  o .: "created"

-- | Run the real executable while preserving the inherited process
-- environment.  The shared integration runner intentionally installs a
-- minimal Git fixture environment, which removes Windows PATH and prevents
-- the production executable from locating Git.
realSpawnAdrai :: FilePath -> [String] -> IO (ExitCode, LBS.ByteString, LBS.ByteString)
realSpawnAdrai repo arguments = do
  executable <- requireRealAdraiExecutable
  realSpawnAdraiWith executable repo arguments

requireRealAdraiExecutable :: IO FilePath
requireRealAdraiExecutable =
  lookupEnv "ADRAI_EXE" >>= \case
    Just path | not (null path) && isAbsolute path -> pure path
    _ -> fail "EnvironmentTest requires ADRAI_EXE to name an absolute executable under test"

realSpawnAdraiWith :: FilePath -> FilePath -> [String] -> IO (ExitCode, LBS.ByteString, LBS.ByteString)
realSpawnAdraiWith executable repo arguments =
  readProcess (proc executable (adraiTestArgs repo arguments))

realAdraiJsonOrThrow :: FilePath -> [String] -> IO Data.Aeson.Value
realAdraiJsonOrThrow repo arguments = do
  (exitCode, stdout, stderr) <- realSpawnAdrai repo arguments
  decodeRealAdraiJson arguments exitCode stdout stderr

realAdraiJsonOrThrowWith :: FilePath -> FilePath -> [String] -> IO Data.Aeson.Value
realAdraiJsonOrThrowWith executable repo arguments = do
  (exitCode, stdout, stderr) <- realSpawnAdraiWith executable repo arguments
  decodeRealAdraiJson arguments exitCode stdout stderr

decodeRealAdraiJson :: [String] -> ExitCode -> LBS.ByteString -> LBS.ByteString -> IO Data.Aeson.Value
decodeRealAdraiJson arguments exitCode stdout stderr =
  case exitCode of
    ExitSuccess ->
      case Data.Aeson.eitherDecode stdout of
        Right value -> pure value
        Left problem -> fail ("adrai " <> unwords arguments <> " JSON error: " <> problem)
    ExitFailure code ->
      fail
        ( "adrai " <> unwords arguments <> " error: CLI failed (exit " <> show code <> "): "
            <> unpack (decodeUtf8 (LBS.toStrict stderr))
        )

realCreateAdr
  :: FilePath
  -> Text
  -> Text
  -> Text
  -> [Text]
  -> [Text]
  -> IO Data.Aeson.Value
realCreateAdr repo title summary body domains scopes =
  realAdraiJsonOrThrow repo $
    [ "create",
      "--title", unpack title,
      "--summary", unpack summary,
      "--body", unpack normalizedBody,
      "--actor", "llm:planner",
      "--model", "demo-model"
    ]
      <> concatMap (\domain -> ["--domain", unpack domain]) domains
      <> concatMap (\scope -> ["--applies-to", unpack scope]) scopes
      <> ["--json"]
  where
    normalizedBody
      | "\n" `T.isSuffixOf` body = body
      | otherwise = body <> "\n"

-- | Extract the head commit of a repository.
headCommit :: FilePath -> IO Text
headCommit repo =
  gitStdout repo ["rev-parse", "HEAD"]
    >>= \h -> pure (strip (decodeUtf8 (LBS.toStrict h)))

requireRepository :: FilePath -> IO Repository
requireRepository location =
  discoverRepository systemGit location >>= \case
    Left problem -> assertFailure ("could not discover test repository: " <> show problem) >> fail "unreachable"
    Right repository -> pure repository

-- | Read a single meta value from the adrai database.
getMeta :: FilePath -> String -> IO (Maybe String)
getMeta dbPath key = do
  conn <- open dbPath
  result <-
    query conn "SELECT value FROM meta WHERE key = ?" [pack key] :: IO [Only String]
  close conn
  pure $ listToMaybe result >>= \(Only v) -> Just v

-- | Add a Git worktree at the given path on the specified branch.
createWorktree :: FilePath -> FilePath -> Text -> IO ()
createWorktree repo wtree branch =
  git repo ["worktree", "add", "-b", unpack branch, wtree, "HEAD"]

-- | Remove a worktree (best effort).
removeWorktree :: FilePath -> FilePath -> IO ()
removeWorktree repo wtree =
  git repo ["worktree", "remove", "--force", wtree]

snapshotFile :: FilePath -> IO (Maybe BS.ByteString)
snapshotFile path = do
  exists <- doesFileExist path
  if exists then Just <$> BS.readFile path else pure Nothing

snapshotTree :: FilePath -> [FilePath] -> IO [(FilePath, Maybe BS.ByteString)]
snapshotTree root excluded = do
  exists <- doesDirectoryExist root
  if exists then go "" else pure []
  where
    go relative = do
      let directory = if null relative then root else root </> relative
      names <- sort <$> listDirectory directory
      fmap concat $ forM names $ \name -> do
        let childRelative = if null relative then name else relative </> name
            child = root </> childRelative
        if childRelative `elem` excluded
          then pure []
          else do
            isDirectory <- doesDirectoryExist child
            if isDirectory
              then ((childRelative, Nothing) :) <$> go childRelative
              else do
                isFile <- doesFileExist child
                if isFile
                  then do
                    bytes <- BS.readFile child
                    pure [(childRelative, Just bytes)]
                  else pure []

-- =====================================================================
-- Test 4: Divergent amendments visible as merge conflict
-- =====================================================================

testLinkedWorktreeCommitsOnlyItsBranch :: TestTree
testLinkedWorktreeCommitsOnlyItsBranch =
  testCase
    "linked_worktree_commits_only_its_branch_and_uses_common_lock"
    $ withSystemTempDirectory "adrai worktree branch" $ \tmpDir -> do
        repo <- createTestRepo tmpDir
        createAdraiInit repo
        mainBefore <- headCommit repo
        mainHeadBefore <- gitStdout repo ["symbolic-ref", "-q", "HEAD"]

        let worktree = tmpDir </> "feature-wt"
        createWorktree repo worktree "feature/adrai"
        mainRepository <- requireRepository repo
        worktreeRepository <- requireRepository worktree
        LinkedWorktree @?= repositoryLayout worktreeRepository
        repositoryCommonDir mainRepository @?= repositoryCommonDir worktreeRepository
        assertBool
          "linked caller index is distinct from common repository storage"
          (repositoryGitDir worktreeRepository /= repositoryCommonDir worktreeRepository)
        featureRootBase <- headCommit worktree
        let configuredDecisions = worktree </> "docs" </> "architecture" </> "decisions"
            configuredConnections = worktree </> "docs" </> "architecture" </> "connections"
            canonicalConfig =
              T.unlines
                [ "schema = 1",
                  "",
                  "[paths]",
                  "decisions = \"docs/architecture/decisions\"",
                  "connections = \"docs/architecture/connections\""
                ]
        BS.writeFile (worktree </> ".adrai.toml") (encodeUtf8 canonicalConfig)
        git worktree ["add", ".adrai.toml"]
        git worktree ["commit", "-m", "feature: custom ADRAI root"]
        configuredTip <- headCommit worktree
        assertBool "custom-root configuration advances the feature branch" (configuredTip /= featureRootBase)
        assertBool "configured feature branch is distinct from main" (configuredTip /= mainBefore)
        createDirectoryIfMissing True configuredDecisions
        createDirectoryIfMissing True configuredConnections

        executable <- requireRealAdraiExecutable
        (compileExit, _, compileStderr) <- realSpawnAdraiWith executable worktree ["compile", "--json"]
        compileExit @?= ExitSuccess
        compileStderr @?= ""
        doesFileExist (worktree </> ".adrai" </> "index.sqlite") >>= (@?= True)

        let stagedRelative = "linked-staged.bin"
            stagedPath = worktree </> stagedRelative
            stagedBytes = BS.pack [0, 255, 17, 9]
        BS.writeFile stagedPath stagedBytes
        git worktree ["add", "--", stagedRelative]
        stagedOid <- strip . decodeUtf8 . LBS.toStrict <$> gitStdout worktree ["hash-object", "--no-filters", "--", stagedRelative]
        let expectedStagedEntry = LBS.fromStrict (encodeUtf8 ("100644 " <> stagedOid <> " 0\t" <> pack stagedRelative <> "\n"))
        gitStdout worktree ["ls-files", "--stage", "--", stagedRelative] >>= (@?= expectedStagedEntry)
        BS.readFile stagedPath >>= (@?= stagedBytes)

        featureHeadBefore <- gitStdout worktree ["symbolic-ref", "-q", "HEAD"]
        stateBefore@(_, _, mainIndexBefore, _, _, _, _, _, mainWorktreeBefore, _, _) <-
          repositoryObservableState mainRepository worktreeRepository repo worktree

        let createArguments =
              [ "create",
                "--title", "Worktree-local architecture",
                "--summary", "ADRAI commits through the linked worktree branch.",
                "--body", "## Decision\nUse Git's per-worktree HEAD and shared ref transaction.\n",
                "--actor", "llm:planner",
                "--model", "demo-model",
                "--domain", "tooling.git",
                "--applies-to", "tools/worktree/**",
                "--json"
              ]

        -- Use the same production common-directory lock as mutations rather
        -- than fabricating a file or a holder PID in the test.
        (lockPath, lockBytes, holderPid) <- withGitLock mainRepository $ do
          held <- gitLockStatus worktreeRepository
          (lockError, lockPath, holderPid) <- case held of
            Left lockError@(LockHeld lockPath holderPid) -> do
              lockPath @?= repositoryCommonDir mainRepository </> "adrai.lock"
              assertBool "the parent-held lock PID is positive" (holderPid > 0)
              currentPid <- fromIntegral <$> getCurrentProcessId
              holderPid @?= currentPid
              pure (lockError, lockPath, holderPid)
            Left problem -> assertFailure ("parent-held production lock must be LockHeld, got " <> show problem) >> fail "unreachable"
            Right _ -> assertFailure "parent-held production Git lock was not observable from linked worktree" >> fail "unreachable"
          let expectedLockBytes = BS8.pack ("pid=" <> show holderPid <> "\n")
          BS.readFile lockPath >>= (@?= expectedLockBytes)
          secondAcquire <- try (acquireGitLock worktreeRepository)
          case secondAcquire of
            Left actual -> actual @?= lockError
            Right unexpected -> do
              releaseGitLock unexpected
              assertFailure "a second native acquisition unexpectedly replaced the live common lock"
          BS.readFile lockPath >>= (@?= expectedLockBytes)
          (exitCode, stdout, stderr) <- realSpawnAdraiWith executable worktree createArguments
          exitCode @?= ExitFailure 2
          stdout @?= ""
          stderr @?=
            LBS.fromStrict
              (encodeUtf8 ("adrai: Stage2AcquireLock " <> pack (show (pack (show lockError))) <> "\n"))
          stateAfterRejected <- repositoryObservableState mainRepository worktreeRepository repo worktree
          stateAfterRejected @?= stateBefore
          snapshotDirectory configuredDecisions >>= (@?= [])
          snapshotDirectory configuredConnections >>= (@?= [])
          pure (lockPath, expectedLockBytes, holderPid)

        doesFileExist lockPath >>= (@?= True)
        BS.readFile lockPath >>= (@?= lockBytes)
        gitLockStatus worktreeRepository >>= (@?= Right Nothing)
        reacquired <- acquireGitLock worktreeRepository
        gitLockPath reacquired @?= lockPath
        gitLockPid reacquired @?= holderPid
        releaseGitLock reacquired
        gitLockStatus mainRepository >>= (@?= Right Nothing)

        -- The same installed-executable mutation succeeds after bracketed
        -- release and advances only the linked worktree branch.
        wtAdr <- realAdraiJsonOrThrowWith executable worktree createArguments
        wtHead <- headCommit worktree
        assertBool "linked worktree branch advances after lock release" (wtHead /= configuredTip)
        extractCommit wtAdr @?= Just wtHead
        adrId <- maybe (assertFailure "worktree create result omitted adr" >> fail "unreachable") pure (extractAdrId wtAdr)
        recordId <- maybe (assertFailure "worktree create result omitted record" >> fail "unreachable") pure (extractRecord wtAdr)
        createdPaths <- maybe (assertFailure "worktree create result omitted generated paths" >> fail "unreachable") pure (extractCreated wtAdr)
        length createdPaths @?= 4
        length (filter (\path -> "docs/architecture/decisions/" `T.isPrefixOf` path && ".decision.md" `T.isSuffixOf` path) createdPaths) @?= 1
        length (filter (\path -> "docs/architecture/connections/" `T.isPrefixOf` path && ".connection.md" `T.isSuffixOf` path) createdPaths) @?= 3
        forM_ createdPaths $ \path ->
          doesFileExist (worktree </> unpack path)
            >>= assertBool ("configured managed file exists: " <> unpack path)

        resultObject <- case _Object wtAdr of
          Just value -> pure value
          Nothing -> assertFailure "worktree create result is not a JSON object" >> fail "unreachable"
        (resultObject .: "indexed" :: Maybe Bool) @?= Just True
        (resultObject .: "index_warnings" :: Maybe Integer) @?= Just 0
        (resultObject .: "index_revision" :: Maybe Text) @?= Just wtHead
        database <- case resultObject .: "database" :: Maybe FilePath of
          Just value -> canonicalizePath value
          Nothing -> assertFailure "worktree create result has no database path" >> fail "unreachable"
        expectedDatabase <- canonicalizePath (worktree </> ".adrai" </> "index.sqlite")
        database @?= expectedDatabase
        getMeta database "resolved_oid" >>= (@?= Just (unpack wtHead))
        connection <- open database
        decisionRows <- query connection "SELECT record_id,adr_id,title FROM decision_record" () :: IO [(Text, Text, Text)]
        connectionRows <- query connection "SELECT adr_id FROM connection_record ORDER BY connection_id" () :: IO [Only Text]
        close connection
        decisionRows @?= [(recordId, adrId, "Worktree-local architecture")]
        connectionRows @?= replicate 3 (Only adrId)

        headCommit repo >>= (@?= mainBefore)
        gitStdout repo ["symbolic-ref", "-q", "HEAD"] >>= (@?= mainHeadBefore)
        gitStdout worktree ["symbolic-ref", "-q", "HEAD"] >>= (@?= featureHeadBefore)
        gitStdout worktree ["rev-parse", "refs/heads/feature/adrai"] >>= (@?= LBS.fromStrict (encodeUtf8 (wtHead <> "\n")))
        gitStdout worktree ["ls-files", "--stage", "--", stagedRelative] >>= (@?= expectedStagedEntry)
        BS.readFile stagedPath >>= (@?= stagedBytes)
        BS.readFile (repositoryGitDir mainRepository </> "index") >>= (@?= mainIndexBefore)
        snapshotTree repo [".git"] >>= (@?= mainWorktreeBefore)

        removeWorktree repo worktree
  where
    repositoryObservableState mainRepository worktreeRepository mainPath worktreePath = do
      mainHead <- BS.readFile (repositoryGitDir mainRepository </> "HEAD")
      worktreeHead <- BS.readFile (repositoryGitDir worktreeRepository </> "HEAD")
      mainIndex <- BS.readFile (repositoryGitDir mainRepository </> "index")
      worktreeIndex <- BS.readFile (repositoryGitDir worktreeRepository </> "index")
      refs <- snapshotTree (repositoryCommonDir mainRepository </> "refs") []
      packedRefs <- snapshotFile (repositoryCommonDir mainRepository </> "packed-refs")
      commonReflogs <- snapshotTree (repositoryCommonDir mainRepository </> "logs") []
      worktreeReflogs <- snapshotTree (repositoryGitDir worktreeRepository </> "logs") []
      mainWorktree <- snapshotTree mainPath [".git"]
      worktreeGitPointer <- snapshotFile (worktreePath </> ".git")
      linkedWorktree <- snapshotTree worktreePath [".git"]
      pure (mainHead, worktreeHead, mainIndex, worktreeIndex, refs, packedRefs, commonReflogs, worktreeReflogs, mainWorktree, worktreeGitPointer, linkedWorktree)

    snapshotDirectory root = do
      exists <- doesDirectoryExist root
      if exists then go root else pure []
      where
        go directory = do
          names <- sort <$> listDirectory directory
          fmap concat $ forM names $ \name -> do
            let path = directory </> name
            isDirectory <- doesDirectoryExist path
            if isDirectory
              then go path
              else do
                isFile <- doesFileExist path
                if isFile
                  then do
                    bytes <- BS.readFile path
                    pure [(makeRelative root path, bytes)]
                  else pure []

-- =====================================================================
-- Test 7: Worktree branch merge back
-- =====================================================================

testSymlinkedManagedParentCannotEscapeRepository :: TestTree
testSymlinkedManagedParentCannotEscapeRepository =
  testCase
    "symlinked managed parent cannot escape repository"
    $ withSystemTempDirectory "adrai managed junction containment" $ \tmpDir -> do
        repo <- createTestRepo tmpDir
        createAdraiInit repo
        repository <-
          discoverRepository systemGit repo >>= \case
            Left problem -> assertFailure ("could not discover containment fixture repository: " <> show problem) >> fail "unreachable"
            Right discovered -> pure discovered
        let outside = tmpDir </> "outside"
            managedParent = repo </> "architecture" </> "adrai"
            architectureParent = takeDirectory managedParent
            callerPath = repo </> "caller-index.bin"
            callerBytes = BS.pack [0, 255, 13, 10, 128, 64, 9, 7, 3]
            createArguments =
              [ "create",
                "--title", "Junction containment decision",
                "--summary", "Managed records must never escape the repository root.",
                "--body", "## Decision\nReject every reparse-point managed parent before mutation.\n",
                "--actor", "llm:planner",
                "--model", "demo-model",
                "--domain", "testing.containment",
                "--applies-to", "src/**",
                "--json"
              ]
        BS.writeFile callerPath callerBytes
        git repo ["add", "--", "caller-index.bin"]
        callerStageBefore <- gitStdout repo ["ls-files", "--stage", "--", "caller-index.bin"]
        callerCachedDiffBefore <- gitStdout repo ["diff", "--cached", "--raw", "-z"]
        createDirectoryIfMissing True architectureParent
        createDirectoryIfMissing True outside
        createDirectoryIfMissing True (repo </> ".adrai")
        createManagedParentJunction outside managedParent
        let removeFixtureJunction = removeManagedParentJunction managedParent
        (do
            before <- observableState repository repo architectureParent managedParent outside
            (exitCode, stdout, stderr) <- realSpawnAdrai repo createArguments
            exitCode @?= ExitFailure 2
            stdout @?= ""
            let stderrText = decodeUtf8 (LBS.toStrict stderr)
                stagePrefix = "adrai: Stage5ValidateGenerated \"managed destination rejected for architecture/adrai/"
                escapedManagedParent = T.replace "\\" "\\\\\\\\" (pack managedParent)
            assertBool "junction rejection uses the Stage5 user-failure channel" (stagePrefix `T.isPrefixOf` stderrText)
            assertBool "junction rejection retains the ManagedPathRedirected constructor" ("ManagedPathRedirected " `T.isInfixOf` stderrText)
            assertBool "junction rejection names the redirected managed parent" (escapedManagedParent `T.isInfixOf` stderrText)
            assertBool "junction rejection has exactly the CLI failure newline framing" ("\n" `T.isSuffixOf` stderrText)
            afterRejected <- observableState repository repo architectureParent managedParent outside
            afterRejected @?= before
            sort <$> listDirectory outside >>= (@?= [])
            doesDirectoryExist managedParent >>= (@?= True)

            -- Remove only the fixture's junction, never its target; the exact
            -- same installed executable must then publish normally.
            removeFixtureJunction
            doesDirectoryExist managedParent >>= (@?= False)
            control <-
              realCreateAdr
                repo
                "Control containment decision"
                "A regular managed parent permits the same public mutation."
                "## Decision\nPublish only after containment succeeds.\n"
                ["testing.containment"]
                ["src/**"]
            controlHead <- headCommit repo
            extractCommit control @?= Just controlHead
            controlObject <-
              case _Object control of
                Just objectValue -> pure objectValue
                Nothing -> assertFailure "control create result is not a JSON object" >> fail "unreachable"
            (controlObject .: "indexed" :: Maybe Bool) @?= Just True
            (controlObject .: "index_warnings" :: Maybe Integer) @?= Just 0
            (controlObject .: "index_revision" :: Maybe Text) @?= Just controlHead
            database <-
              case controlObject .: "database" :: Maybe FilePath of
                Just value -> canonicalizePath value
                Nothing -> assertFailure "control create JSON omits database" >> fail "unreachable"
            expectedDatabase <- canonicalizePath (repo </> ".adrai" </> "index.sqlite")
            database @?= expectedDatabase
            getMeta database "resolved_oid" >>= (@?= Just (unpack controlHead))
            created <-
              case extractCreated control of
                Just paths | not (null paths) -> pure paths
                _ -> assertFailure "control create JSON omits generated managed paths" >> fail "unreachable"
            forM_ created $ \path ->
              gitSuccess repo ["cat-file", "-e", "HEAD:" <> unpack path]
            callerStageAfter <- gitStdout repo ["ls-files", "--stage", "--", "caller-index.bin"]
            callerCachedDiffAfter <- gitStdout repo ["diff", "--cached", "--raw", "-z"]
            BS.readFile callerPath >>= (@?= callerBytes)
            callerStageAfter @?= callerStageBefore
            callerCachedDiffAfter @?= callerCachedDiffBefore
            sort <$> listDirectory outside >>= (@?= [])
          ) `finally` removeFixtureJunction
  where
    observableState repository repo architectureParent managedParent outside = do
      rawHead <- BS.readFile (repositoryGitDir repository </> "HEAD")
      rawIndex <- BS.readFile (repositoryGitDir repository </> "index")
      refs <- snapshotTree (repositoryCommonDir repository </> "refs") []
      packedRefs <- snapshotFile (repositoryCommonDir repository </> "packed-refs")
      reflogs <- snapshotTree (repositoryCommonDir repository </> "logs") []
      callerWorktree <- snapshotTree repo [".git", "architecture" </> "adrai"]
      managedParentEntries <- sort <$> listDirectory architectureParent
      outsideBytes <- snapshotDirectory outside
      managedParentExists <- doesDirectoryExist managedParent
      managedParentIsRedirect <- pathIsSymbolicLink managedParent
      pure (rawHead, rawIndex, refs, packedRefs, reflogs, callerWorktree, managedParentEntries, outsideBytes, managedParentExists, managedParentIsRedirect)

    snapshotDirectory root = do
      exists <- doesDirectoryExist root
      if not exists
        then pure []
        else go root
      where
        go directory = do
          names <- sort <$> listDirectory directory
          fmap concat $ forM names $ \name -> do
            let path = directory </> name
            isDirectory <- doesDirectoryExist path
            if isDirectory
              then go path
              else do
                isFile <- doesFileExist path
                if isFile
                  then do
                    bytes <- BS.readFile path
                    pure [(makeRelative root path, bytes)]
                  else pure []

    createManagedParentJunction target link
      | os == "mingw32" = do
          let command = "mklink /J \"" <> link <> "\" \"" <> target <> "\""
          result <- runProcess (shell command)
          case result of
            ExitSuccess -> pure ()
            ExitFailure code -> assertFailure ("failed to create Windows junction, exit " <> show code)
      | otherwise = assertFailure "this Windows-only junction containment proof requires mingw32"

    removeManagedParentJunction link = do
      isLink <- pathIsSymbolicLink link
      if isLink then removeDirectory link else pure ()

-- =====================================================================
-- Test 10: Upstream hint survives integration
-- =====================================================================

tests :: TestTree
tests =
  testGroup
    "Environment (branch / worktree / sparse / unicode)"
    [ testLinkedWorktreeCommitsOnlyItsBranch,
      testSymlinkedManagedParentCannotEscapeRepository
    ]
