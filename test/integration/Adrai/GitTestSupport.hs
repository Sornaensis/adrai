{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Adrai.GitTestSupport
  ( initTestRepository,
    initBareRepository,
    cloneIndependentBareRepository,
    cloneIndependentDepthOneRepository,
    commitFile,
    commitFiles,
    hashObject,
    gitSuccess,
    gitResult,
    installFailingCleanFilter,
    withRejectingReferenceTransactionHook,
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
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket, finally)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, normalise, takeDirectory, (</>))
import System.IO (hClose, openTempFile)
import System.Process.Typed (byteStringInput, proc, readProcess, setEnv, setStdin)
import System.Environment (getEnvironment, getExecutablePath)
import qualified System.Environment as Environment
import qualified Data.Map.Strict as Map
import System.IO.Unsafe (unsafePerformIO)

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
  commonDirectory <- gitCommonDirectory directory
  let hooksDirectory = commonDirectory </> "adrai-no-hooks"
  createDirectoryIfMissing True hooksDirectory
  _ <- gitSuccess directory ["config", "core.hooksPath", hooksDirectory] BS.empty
  pure ()

-- | Configure a required clean filter which is proven to reject the exact
-- temporary-index @git add --sparse@ shape used by transaction Stage 6.
-- The probe uses an isolated index and removes every artifact it creates.
installFailingCleanFilter :: FilePath -> IO Text
installFailingCleanFilter repository = do
  parent <- commitFile repository ".gitattributes" "*.md filter=adrai-fail\n"
  _ <- gitSuccess repository ["config", "filter.adrai-fail.required", "true"] BS.empty
  commonDirectory <- gitCommonDirectory repository
  let probePath = "adrai-filter-probe.md"
  (indexPath, indexHandle) <- openTempFile commonDirectory "adrai-filter-index-"
  hClose indexHandle
  BS.writeFile (repository </> probePath) "probe\n"
  let cleanup = do
        probeExists <- doesFileExist (repository </> probePath)
        if probeExists then removeFile (repository </> probePath) else pure ()
        indexExists <- doesFileExist indexPath
        if indexExists then removeFile indexPath else pure ()
  (do
      _ <- gitSuccessWithEnvironment repository (Map.singleton "GIT_INDEX_FILE" indexPath) ["read-tree", "HEAD"] BS.empty
      (exitCode, stdoutBytes, stderrBytes) <- gitResultWithEnvironment repository (Map.singleton "GIT_INDEX_FILE" indexPath) ["add", "--sparse", "--", probePath] BS.empty
      case exitCode of
        ExitFailure _ -> do
          assertRequiredCleanFilterDiagnostic stderrBytes
          (probeExit, probeStdout, probeStderr) <- gitResultWithEnvironment repository (Map.singleton "GIT_INDEX_FILE" indexPath) ["ls-files", "--error-unmatch", "--", probePath] BS.empty
          case probeExit of
            ExitFailure _ -> pure ()
            ExitSuccess ->
              fail
                ( "fixture failing clean filter left the probe in its isolated index"
                    <> "\nstdout: "
                    <> Text.unpack (TextEncoding.decodeUtf8Lenient probeStdout)
                    <> "\nstderr: "
                    <> Text.unpack (TextEncoding.decodeUtf8Lenient probeStderr)
                )
        ExitSuccess ->
          fail
            ( "fixture clean filter did not reject temporary-index git add"
                <> "\nstdout: "
                <> Text.unpack (TextEncoding.decodeUtf8Lenient stdoutBytes)
                <> "\nstderr: "
                <> Text.unpack (TextEncoding.decodeUtf8Lenient stderrBytes)
            )
    ) `finally` cleanup
  pure parent

-- | Install a reference-transaction hook at Git's actual common-directory
-- hooks path and prove that a controlled @update-ref@ is rejected.  When an
-- aborted target is supplied, the hook also proves and then restores that
-- deterministic HEAD redirect so rollback tests can exercise their pinned-ref
-- behavior without leaking fixture state into the transaction itself.
withRejectingReferenceTransactionHook :: FilePath -> Maybe Text -> IO value -> IO value
withRejectingReferenceTransactionHook repository abortedHeadTarget action = withMVar referenceTransactionFixtureLock $ \_ -> do
  commonDirectory <- gitCommonDirectory repository
  let hooksDirectory = commonDirectory </> "adrai-no-hooks"
      hookPath = hooksDirectory </> "reference-transaction"
      phasePath = commonDirectory </> "adrai-reference-transaction-phases"
      behavior = hookBehavior abortedHeadTarget
      hookEnvironment =
        [ ("ADRAI_TEST_REFERENCE_TRANSACTION_HOOK", "p6-03f-0a-native-hook-v1"),
          ("ADRAI_TEST_REFERENCE_TRANSACTION_BEHAVIOR", behavior),
          ("ADRAI_TEST_REFERENCE_TRANSACTION_PHASE_LOG", phasePath),
          ("ADRAI_TEST_REFERENCE_TRANSACTION_HEAD_PATH", commonDirectory </> "HEAD")
        ]
  testExecutable <- getExecutablePath
  createDirectoryIfMissing True hooksDirectory
  _ <- gitSuccess repository ["config", "core.hooksPath", hooksDirectory] BS.empty
  copyFile testExecutable hookPath
  let cleanup = do
        hookExists <- doesFileExist hookPath
        if hookExists then removeFile hookPath else pure ()
        phaseExists <- doesFileExist phasePath
        if phaseExists then removeFile phasePath else pure ()
  withFixtureEnvironment hookEnvironment $ (do
    verifyRejectingReferenceTransactionHook repository commonDirectory phasePath abortedHeadTarget
    action
    ) `finally` cleanup

gitCommonDirectory :: FilePath -> IO FilePath
gitCommonDirectory repository = do
  raw <- Text.unpack <$> (outputText <$> gitSuccess repository ["rev-parse", "--git-common-dir"] BS.empty)
  pure (normalise (if isAbsolute raw then raw else repository </> raw))

gitResultWithEnvironment :: FilePath -> Map.Map String String -> [String] -> ByteString -> IO (ExitCode, ByteString, ByteString)
gitResultWithEnvironment directory overrides arguments input = do
  inherited <- getEnvironment
  let environment = Map.toList (Map.union overrides (Map.fromList inherited))
  (exitCode, stdoutBytes, stderrBytes) <-
    readProcess
      ( setEnv environment
          (setStdin (byteStringInput (LBS.fromStrict input)) (proc "git" ("-C" : directory : arguments)))
      )
  pure (exitCode, LBS.toStrict stdoutBytes, LBS.toStrict stderrBytes)

gitSuccessWithEnvironment :: FilePath -> Map.Map String String -> [String] -> ByteString -> IO ByteString
gitSuccessWithEnvironment directory overrides arguments input = do
  (exitCode, stdoutBytes, stderrBytes) <- gitResultWithEnvironment directory overrides arguments input
  case exitCode of
    ExitSuccess -> pure stdoutBytes
    ExitFailure code ->
      fail
        ( "Git fixture command failed ("
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

hookBehavior :: Maybe Text -> String
hookBehavior = \case
  Nothing -> "reject-prepared"
  Just target -> "reject-prepared-switch-head:" <> Text.unpack target

verifyRejectingReferenceTransactionHook :: FilePath -> FilePath -> FilePath -> Maybe Text -> IO ()
verifyRejectingReferenceTransactionHook repository commonDirectory phasePath abortedHeadTarget = do
  headBefore <- BS.readFile (commonDirectory </> "HEAD")
  currentHead <- outputText <$> gitSuccess repository ["rev-parse", "HEAD"] BS.empty
  let probeRef = "refs/adrai-fixture-reference-transaction-probe"
  (exitCode, stdoutBytes, stderrBytes) <- gitResult repository ["update-ref", probeRef, Text.unpack currentHead, "0000000000000000000000000000000000000000"] BS.empty
  case exitCode of
    ExitFailure _ -> pure ()
    ExitSuccess ->
      fail
        ( "fixture reference-transaction hook did not reject update-ref"
            <> "\nstdout: "
            <> Text.unpack (TextEncoding.decodeUtf8Lenient stdoutBytes)
            <> "\nstderr: "
            <> Text.unpack (TextEncoding.decodeUtf8Lenient stderrBytes)
        )
  probeExists <- gitResult repository ["show-ref", "--verify", "--quiet", probeRef] BS.empty
  case probeExists of
    (ExitFailure _, _, _) -> pure ()
    (ExitSuccess, _, _) -> fail "fixture reference-transaction probe ref was created despite rejection"
  phaseExists <- doesFileExist phasePath
  if not phaseExists
    then
      fail
        ( "fixture reference-transaction hook rejected update-ref without recording any phase"
            <> "\nstdout: "
            <> Text.unpack (TextEncoding.decodeUtf8Lenient stdoutBytes)
            <> "\nstderr: "
            <> Text.unpack (TextEncoding.decodeUtf8Lenient stderrBytes)
        )
    else pure ()
  phases <- BS.readFile phasePath
  if map Text.strip (Text.lines (TextEncoding.decodeUtf8Lenient phases)) == ["preparing", "aborted"]
    then pure ()
    else
      fail
        ( "fixture reference-transaction hook did not receive Git's preparing then aborted phases"
            <> "\nactual phases: "
            <> show (TextEncoding.decodeUtf8Lenient phases)
        )
  case abortedHeadTarget of
    Nothing -> BS.readFile (commonDirectory </> "HEAD") >>= assertFixtureBytes "reference-transaction hook changed HEAD without aborted behavior" headBefore
    Just target -> do
      observedHead <- BS.readFile (commonDirectory </> "HEAD")
      assertFixtureBytes "reference-transaction aborted behavior did not redirect HEAD" (TextEncoding.encodeUtf8 ("ref: " <> target <> "\n")) observedHead
      BS.writeFile (commonDirectory </> "HEAD") headBefore

withFixtureEnvironment :: [(String, String)] -> IO value -> IO value
withFixtureEnvironment bindings action = bracket capture restore apply
  where
    names = map fst bindings
    capture =
      traverse
        (\name -> do
          previous <- lookupEnvironment name
          pure (name, previous)
        )
        names
    apply _ = do
      mapM_ (uncurry Environment.setEnv) bindings
      action
    restore prior =
      mapM_
        (\(name, previous) ->
          case previous of
            Nothing -> Environment.unsetEnv name
            Just value -> Environment.setEnv name value
        )
        prior

lookupEnvironment :: String -> IO (Maybe String)
lookupEnvironment name = do
  inherited <- getEnvironment
  pure (lookup name inherited)

assertRequiredCleanFilterDiagnostic :: ByteString -> IO ()
assertRequiredCleanFilterDiagnostic stderrBytes =
  let diagnostic = Text.toLower (TextEncoding.decodeUtf8Lenient stderrBytes)
   in if "adrai-fail" `Text.isInfixOf` diagnostic && "filter" `Text.isInfixOf` diagnostic && ("required" `Text.isInfixOf` diagnostic || "clean" `Text.isInfixOf` diagnostic)
        then pure ()
        else
          fail
            ( "fixture clean-filter rejection did not identify the required adrai-fail filter"
                <> "\nstderr: "
                <> Text.unpack diagnostic
            )

-- | The hook marker and phase configuration are process-global environment
-- variables inherited by Git.  Serializing the full fixture lifetime prevents
-- parallel Tasty cases from cross-wiring a copied hook to another repository's
-- marker, log, or test-owned HEAD path.
referenceTransactionFixtureLock :: MVar ()
referenceTransactionFixtureLock = unsafePerformIO (newMVar ())
{-# NOINLINE referenceTransactionFixtureLock #-}

assertFixtureBytes :: String -> ByteString -> ByteString -> IO ()
assertFixtureBytes label expected actual =
  if expected == actual then pure () else fail label

initBareRepository :: FilePath -> IO ()
initBareRepository directory = do
  createDirectoryIfMissing True directory
  _ <- gitSuccess directory ["init", "--bare", "--initial-branch=main"] BS.empty
  pure ()

-- | Make a local bare clone without sharing object storage with its source.
cloneIndependentBareRepository :: FilePath -> FilePath -> IO ()
cloneIndependentBareRepository source destination =
  withTemporaryBundle destination $ \bundle -> do
    _ <- gitSuccess source ["bundle", "create", bundle, "--all", "HEAD"] BS.empty
    initBareRepository destination
    _ <- gitSuccess destination ["bundle", "unbundle", bundle] BS.empty
    copySourceRefs source destination
    preserveHead source destination
    assertBareRepository destination
    assertIndependentFixtureRepository destination

-- | Make an independent local clone and mark its checked-out branch boundary
-- as shallow using Git's native common-directory @shallow@ file. Local clones
-- use bundle staging rather than @clone@ transport behavior.
cloneIndependentDepthOneRepository :: FilePath -> Text -> FilePath -> IO ()
cloneIndependentDepthOneRepository source branch destination =
  withTemporaryBundle destination $ \bundle -> do
    let selectedRef = "refs/heads/" <> branch
        selectedCommit = selectedRef <> "^{commit}"
    headOid <- outputText <$> gitSuccess source ["rev-parse", Text.unpack selectedCommit] BS.empty
    sourceTree <- outputText <$> gitSuccess source ["rev-parse", Text.unpack (headOid <> "^{tree}")] BS.empty
    _ <- gitSuccess source ["bundle", "create", bundle, Text.unpack selectedRef] BS.empty
    initTestRepository destination
    _ <- gitSuccess destination ["bundle", "unbundle", bundle] BS.empty
    _ <- gitSuccess destination ["update-ref", Text.unpack selectedRef, Text.unpack headOid] BS.empty
    _ <- gitSuccess destination ["symbolic-ref", "HEAD", Text.unpack selectedRef] BS.empty
    _ <- gitSuccess destination ["reset", "--hard", Text.unpack headOid] BS.empty
    commonDirectory <- gitCommonDirectory destination
    BS.writeFile (commonDirectory </> "shallow") (TextEncoding.encodeUtf8 (headOid <> "\n"))
    assertIndependentFixtureRepository destination
    destinationHead <- outputText <$> gitSuccess destination ["rev-parse", "HEAD"] BS.empty
    if destinationHead /= headOid
      then fail ("fixture depth-one clone resolved the wrong HEAD: " <> destination)
      else pure ()
    destinationTree <- outputText <$> gitSuccess destination ["rev-parse", "HEAD^{tree}"] BS.empty
    if destinationTree /= sourceTree
      then fail ("fixture depth-one clone tree differs from selected branch: " <> destination)
      else pure ()
    refs <- Text.lines . outputText <$> gitSuccess destination ["for-each-ref", "--format=%(refname)", "refs/heads", "refs/tags"] BS.empty
    if refs /= [selectedRef]
      then fail ("fixture depth-one clone retained an unrelated ref: " <> destination)
      else pure ()
    shallow <- outputText <$> gitSuccess destination ["rev-parse", "--is-shallow-repository"] BS.empty
    if shallow /= "true"
      then fail ("fixture depth-one clone was not shallow: " <> destination)
      else pure ()
    reachable <- Text.lines . outputText <$> gitSuccess destination ["rev-list", "--max-count=2", "HEAD"] BS.empty
    if reachable /= [headOid]
      then fail ("fixture shallow history did not stop at HEAD: " <> destination)
      else pure ()
    (parentExit, _, _) <- gitResult destination ["rev-parse", "HEAD^"] BS.empty
    case parentExit of
      ExitFailure _ -> pure ()
      ExitSuccess -> fail ("fixture shallow parent remained available: " <> destination)

withTemporaryBundle :: FilePath -> (FilePath -> IO value) -> IO value
withTemporaryBundle destination action = bracket acquire release action
  where
    parent = takeDirectory destination
    acquire = do
      createDirectoryIfMissing True parent
      (bundle, handle) <- openTempFile parent "adrai-fixture-bundle-"
      hClose handle
      removeFile bundle
      pure bundle
    release bundle = do
      exists <- doesFileExist bundle
      if exists then removeFile bundle else pure ()

copySourceRefs :: FilePath -> FilePath -> IO ()
copySourceRefs source destination = do
  references <- Text.lines . outputText <$> gitSuccess source ["show-ref", "--heads", "--tags"] BS.empty
  mapM_ copyReference references
  where
    copyReference line =
      case Text.words line of
        [oid, reference] -> void $ gitSuccess destination ["update-ref", Text.unpack reference, Text.unpack oid] BS.empty
        _ -> fail ("fixture source emitted malformed show-ref output: " <> Text.unpack line)

preserveHead :: FilePath -> FilePath -> IO ()
preserveHead source destination = do
  (symbolicExit, symbolicOutput, _) <- gitResult source ["symbolic-ref", "-q", "HEAD"] BS.empty
  case symbolicExit of
    ExitSuccess -> void $ gitSuccess destination ["symbolic-ref", "HEAD", Text.unpack (outputText symbolicOutput)] BS.empty
    ExitFailure _ -> do
      headOid <- outputText <$> gitSuccess source ["rev-parse", "HEAD"] BS.empty
      void $ gitSuccess destination ["update-ref", "--no-deref", "HEAD", Text.unpack headOid] BS.empty

assertBareRepository :: FilePath -> IO ()
assertBareRepository destination = do
  bare <- outputText <$> gitSuccess destination ["rev-parse", "--is-bare-repository"] BS.empty
  if bare == "true"
    then pure ()
    else fail ("fixture bare clone was not bare: " <> destination)

assertIndependentFixtureRepository :: FilePath -> IO ()
assertIndependentFixtureRepository destination = do
  commonDirectory <- gitCommonDirectory destination
  alternates <- doesFileExist (commonDirectory </> "objects" </> "info" </> "alternates")
  if alternates
    then fail ("fixture repository retained object alternates: " <> destination)
    else pure ()
  (originExit, _, _) <- gitResult destination ["config", "--get-regexp", "^remote\\.origin\\."] BS.empty
  case originExit of
    ExitFailure _ -> pure ()
    ExitSuccess -> fail ("fixture repository retained an origin configuration: " <> destination)

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
