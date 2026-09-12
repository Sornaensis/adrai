{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | Read-only, byte-exact Git observation.
--
-- Every Git invocation is an argv-based @typed-process@ call.  This module
-- deliberately knows nothing about ADRAI documents, graph reduction, caches,
-- or SQLite: it reports repository facts and object bytes only.
module Adrai.Git
  ( GitClient (..),
    systemGit,
    gitClient,
    TerminationWaitPhase (..),
    TerminationWaitResult (..),
    TerminationWaitDecision (..),
    terminationWaitDecision,
    boundedExitPoll,
    boundedTerminationRequest,
    Repository (..),
    RepositoryLayout (..),
    discoverRepository,
    GitHeadState (..),
    repositoryHeadState,
    decodeGitHeadState,
    RevisionSpec (..),
    mkRevisionSpec,
    revisionSpecText,
    resolveRevision,
    isShallowRepository,
    GitCommitNode,
    gitCommitNodeOid,
     gitCommitNodeParents,
     historyTreeDeltaBatchCount,
    reachableCommitGraphAt,
    decodeGitCommitGraph,
    GitHistoryTreeDelta (..),
    GitTreeChange (..),
    historyTreeDeltasAt,
    decodeGitHistoryTreeDeltas,
    GitObjectType (..),
    GitFileMode (..),
    GitTreeEntry (..),
    listTreeEntriesAt,
    listTreeEntriesForRepositoryValidationAt,
    lookupTreeEntryAt,
    lookupTreeEntriesAt,
    reconcileExactTreeEntries,
    lookupTreeObjectInfoAtRevisions,
    treePathBatchRequestCount,
     treePathRevisionBatchRequestCount,
     treePathBatchSessionCount,
     treePathRevisionLsTreeChildCount,
    treePathRevisionProcessCount,
    treePathArgumentByteLimit,
    treePathArgumentWindows,
    treePathArgumentWindowsForCommand,
    GitBlob (..),
    readRegularBlobAt,
    GitObjectInfo (..),
    GitProcessResult (..),
    batchObjectInfo,
    objectInfoBatchSessionCount,
    foldBlobBatch,
    foldBlobBatchInOrder,
    readBlobBatch,
    readBlobBatchOneSession,
    GitBlobBatchSession,
    withBlobBatchSession,
    withBlobBatchSessionWithWindowObserver,
    withBlobBatchSessionWithFinalizationObserverForTest,
    foldBlobBatchFromSession,
    foldBlobBatchInOrderFromSession,
    readBlobBatchFromSession,
    GitBatchInput (..),
    writePersistentBatchRequests,
    decodeGitBlobUtf8,
    readUtf8BlobBatch,
    readWorktreeFileBytes,
    GitError (..),
    GitProtocolError (..),
    boundedDiagnostic,
    canonicalObjectChunks,
    objectBatchRequestCount,
    decodeGitBoolean,
    decodeGitPathOutput,
    decodeGitTreeOutput,
    decodeGitObjectInfoHeader,
    decodeGitBlobHeader,
    decodeGitBlobPayload,
    validateGitBatchTrailing,
    validateObjectInfoBatchTrailing,
    runRepository,
    runRepositoryWithEnvironment,
    GitOid(..),
    gitOidText,
    OverlayFingerprint,
    mkOverlayFingerprint,
  )
where

import Adrai.ManagedPath (ManagedReadPathError (..), resolveRepositoryReadPath)
import Adrai.Provenance (GitOid(..), gitOidText, mkGitOid, OverlayFingerprint (..), mkOverlayFingerprint)
import Adrai.Types (GitRef, RepoPath, RepoPathViolation, mkGitRef, mkRepoPath, repoPathText)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent (MVar, forkIO, modifyMVar, newMVar, readMVar, threadDelay)
import Control.Concurrent.STM (STM, TMVar, TVar, atomically, check, isEmptyTMVar, newTMVarIO, newTVarIO, orElse, readTVar, registerDelay, retry, tryPutTMVar, tryTakeTMVar, writeTVar)
import Control.Exception (IOException, SomeAsyncException, SomeException, fromException, mask, throwIO, try)
import Control.Monad (foldM, void)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word64)
import System.Directory (canonicalizePath, doesDirectoryExist)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (equalFilePath)
import System.IO (Handle, hClose, hFlush)
import System.IO.Error (ioeGetErrorString)
import qualified System.Process as Process
import System.Process (CreateProcess (env, std_err, std_in, std_out), ProcessHandle, StdStream (CreatePipe))
import Text.Read (readMaybe)

newtype GitClient = GitClient FilePath
  deriving (Eq, Show)

data TerminationWaitPhase = BeforeTermination | AfterTerminationSuccess | AfterTerminationError
  deriving (Eq, Show)

data TerminationWaitResult = TerminationSignaled | TerminationTimedOut | TerminationUnexpected
  deriving (Eq, Show)

data TerminationWaitDecision = TerminationExitProven | TerminationAttemptRequired | TerminationWaitFailed | TerminationErrorRethrow
  deriving (Eq, Show)

terminationWaitDecision :: TerminationWaitPhase -> TerminationWaitResult -> TerminationWaitDecision
terminationWaitDecision _ TerminationSignaled = TerminationExitProven
terminationWaitDecision BeforeTermination TerminationTimedOut = TerminationAttemptRequired
terminationWaitDecision AfterTerminationError TerminationTimedOut = TerminationErrorRethrow
terminationWaitDecision _ TerminationTimedOut = TerminationWaitFailed
terminationWaitDecision _ TerminationUnexpected = TerminationWaitFailed

postTerminationProofMicros :: Int
postTerminationProofMicros = 250000

terminationPollMicros :: Int
terminationPollMicros = 10000

naturalExitGraceMicros :: Int
naturalExitGraceMicros = 100000

readerCleanupMicros :: Int
readerCleanupMicros = 250000

pollAsyncWithin :: Int -> Async.Async value -> IO (Maybe (Either SomeException value))
pollAsyncWithin budgetMicros worker = do
  timer <- registerDelay (max 0 budgetMicros)
  atomically $
    (Just <$> Async.waitCatchSTM worker)
      `orElse` (readTVar timer >>= check >> pure Nothing)

terminationRequestMicros :: Int
terminationRequestMicros = 100000

-- | Start a termination action in a disposable worker and observe it only to a
-- finite deadline. The caller never joins or cancels a slow request.
boundedTerminationRequest :: Int -> IO () -> IO (Either IOException ())
boundedTerminationRequest requestMicros action = do
  worker <- Async.async (try @IOException action)
  observed <- pollAsyncWithin requestMicros worker
  pure $ case observed of
    Nothing -> Left (userError "bounded Git termination request timed out")
    Just (Left problem) -> Left (userError (show problem))
    Just (Right result) -> result

awaitCapturedExitWithin :: Int -> ProcessHandle -> IO (Either IOException (Maybe ExitCode))
awaitCapturedExitWithin deadlineMicros handle = do
  worker <- Async.async (try @IOException (boundedExitPoll deadlineMicros (threadDelay terminationPollMicros) (Process.getProcessExitCode handle)))
  observed <- pollAsyncWithin deadlineMicros worker
  pure $ case observed of
    Nothing -> Right Nothing
    Just (Left problem) -> Left (userError (show problem))
    Just (Right result) -> result

-- | Injectable finite poll used by captured-handle cleanup.  Tests supply a
-- no-op pause and a never-exits probe to prove the timeout path deterministically.
boundedExitPoll :: Int -> IO () -> IO (Maybe ExitCode) -> IO (Maybe ExitCode)
boundedExitPoll deadlineMicros pause probe = go deadlineMicros
  where
    go remaining = do
      observed <- probe
      case observed of
        Just exitCode -> pure (Just exitCode)
        Nothing
          | remaining <= 0 -> pure Nothing
          | otherwise -> pause >> go (remaining - terminationPollMicros)

-- | Bounded cleanup for exactly the handle returned by 'startProcess'.  It
-- never opens or targets a numeric PID.  A timeout is reported to synchronous
-- callers; async cancellation handlers deliberately retain their original
-- exception after invoking this cleanup.
terminateCapturedProcess :: ProcessHandle -> IO (Either IOException ())
terminateCapturedProcess handle = do
  initial <- awaitCapturedExitWithin naturalExitGraceMicros handle
  case initial of
    Left problem -> pure (Left problem)
    Right (Just _) -> pure (Right ())
    Right Nothing -> do
      terminated <- boundedTerminationRequest terminationRequestMicros (Process.terminateProcess handle)
      case terminated of
        Left problem -> pure (Left problem)
        Right () -> do
          waited <- awaitCapturedExitWithin postTerminationProofMicros handle
          pure $ case waited of
            Right (Just _) ->
              case terminationWaitDecision AfterTerminationSuccess TerminationSignaled of
                TerminationExitProven -> Right ()
                _ -> Left (userError "bounded Git child termination wait failed")
            Right Nothing -> Left (userError "bounded Git child termination wait timed out")
            Left problem -> Left problem

systemGit :: GitClient
systemGit = GitClient "git"

gitClient :: FilePath -> Either GitError GitClient
gitClient executable
  | null executable = Left (GitExecutableUnavailable executable)
  | any (`elem` ['\NUL', '\r', '\n']) executable = Left (GitExecutableUnavailable executable)
  | otherwise = Right (GitClient executable)

data RepositoryLayout
  = BareRepository
  | MainWorktree
  | LinkedWorktree
  deriving (Eq, Ord, Show)

data GitHeadState
  = GitHeadAttached GitRef
  | GitHeadDetached
  deriving (Eq, Show)

data Repository = Repository
  { repositoryClient :: GitClient,
    repositoryWorktreeRoot :: Maybe FilePath,
    repositoryGitDir :: FilePath,
    repositoryCommonDir :: FilePath,
    repositoryLayout :: RepositoryLayout,
    repositoryCommonIsBare :: Bool,
    repositoryCommandDirectory :: FilePath
  }
  deriving (Eq, Show)

newtype RevisionSpec = RevisionSpec Text
  deriving (Eq, Ord, Show)

mkRevisionSpec :: Text -> Either GitError RevisionSpec
mkRevisionSpec value
  | Text.null value = Left (GitInvalidRevisionSpec value)
  | Text.head value == '-' = Left (GitInvalidRevisionSpec value)
  | Text.any (`elem` ['\NUL', '\r', '\n']) value = Left (GitInvalidRevisionSpec value)
  | otherwise = Right (RevisionSpec value)

revisionSpecText :: RevisionSpec -> Text
revisionSpecText (RevisionSpec value) = value

data GitObjectType
  = GitBlobObject
  | GitTreeObject
  | GitCommitObject
  | GitTagObject
  deriving (Eq, Ord, Show)

data GitFileMode
  = GitRegularFile
  | GitExecutableFile
  | GitSymbolicLink
  | GitSubmodule
  | GitDirectory
  deriving (Eq, Ord, Show)

data GitTreeEntry = GitTreeEntry
  { gitTreePath :: RepoPath,
    gitTreeOid :: GitOid,
    gitTreeObjectType :: GitObjectType,
    gitTreeMode :: GitFileMode
  }
  deriving (Eq, Show)

data GitBlob = GitBlob
  { gitBlobOid :: GitOid,
    gitBlobBytes :: ByteString
  }
  deriving (Eq, Show)

data GitObjectInfo = GitObjectInfo
  { objectInfoOid :: GitOid,
    objectInfoType :: GitObjectType,
    objectInfoSize :: Word64
  }
  deriving (Eq, Show)

data GitCommitNode = GitCommitNode
  { gitCommitNodeOid :: GitOid,
    gitCommitNodeParents :: [GitOid]
  }
  deriving (Eq, Show)

-- | One parent-to-child tree delta reported by @git diff-tree@.  A root
-- commit has 'Nothing' for its parent.  The types are deliberately raw Git
-- facts: callers decide which paths carry domain meaning.
data GitHistoryTreeDelta = GitHistoryTreeDelta
  { gitHistoryTreeDeltaCommit :: GitOid,
    gitHistoryTreeDeltaParent :: Maybe GitOid,
    gitHistoryTreeDeltaChanges :: [GitTreeChange]
  }
  deriving (Eq, Show)

-- | A single literal path transition.  'Nothing' means that side of the
-- parent/child comparison is absent.  A non-blob entry remains represented so
-- integrity layers can distinguish it from an ordinary deletion.
data GitTreeChange = GitTreeChange
  { gitTreeChangePath :: RepoPath,
    gitTreeChangeOldEntry :: Maybe GitTreeEntry,
    gitTreeChangeNewEntry :: Maybe GitTreeEntry
  }
  deriving (Eq, Show)

data GitProtocolError
  = GitMalformedBoolean ByteString
  | GitMalformedPathOutput ByteString
  | GitMalformedTreeRecord ByteString
  | GitMalformedObjectHeader ByteString
  | GitReturnedObjectMismatch GitOid GitOid
  | GitInvalidObjectSize ByteString
  | GitResponseCountMismatch Int Int
  | GitTruncatedObject GitOid Word64 Int
  | GitMissingObjectFraming GitOid
  | GitUnexpectedTrailingBytes ByteString
  deriving (Eq, Show)

data GitError
  = GitExecutableUnavailable FilePath
  | GitNotRepository FilePath
  | GitInvalidRepositoryLayout Text
  | GitCommandFailed Text Int Text Text
  | GitInvalidRevisionSpec Text
  | GitUnknownRevision RevisionSpec
  | GitRevisionNotCommit RevisionSpec
  | GitInvalidOutput Text GitProtocolError
  | GitInvalidRepositoryPath Text RepoPathViolation
  | GitInvalidUtf8Path ByteString
  | GitPathMissing RepoPath
  | GitPathNotRegular RepoPath GitFileMode GitObjectType
  | GitObjectMissing GitOid
  | GitObjectTypeMismatch GitOid GitObjectType GitObjectType
  | GitObjectTooLargeForPlatform GitOid Word64
  | GitInvalidUtf8Blob GitOid
  | GitBlobBatchSessionClosed
  | GitWorktreeRequired
  | GitWorktreePathError RepoPath ManagedReadPathError
  deriving (Eq, Show)

data GitProcessResult = GitProcessResult
  { processExitCode :: ExitCode,
    processStdout :: ByteString,
    processStderr :: ByteString
  }

diagnosticLimit :: Int
diagnosticLimit = 4096

boundedDiagnostic :: ByteString -> Text
boundedDiagnostic raw =
  normalizeNewlines
    . TextEncoding.decodeUtf8With lenientDecode
    $ if BS.length raw <= diagnosticLimit
      then raw
      else BS.take diagnosticLimit raw <> "...[truncated]"
  where
    normalizeNewlines = Text.replace "\r" "\n" . Text.replace "\r\n" "\n"

-- | The lifecycle paths own these concrete handles directly.  Avoiding
-- typed-process here prevents a hidden waiter from racing our bounded cleanup.
spawnGitProcess :: FilePath -> FilePath -> Maybe [(String, String)] -> [String] -> IO (Handle, Handle, Handle, ProcessHandle)
spawnGitProcess executable commandDirectory environment arguments = do
  (maybeInput, maybeOutput, maybeErrors, handle) <-
    Process.createProcess
      ( (Process.proc executable ("-C" : commandDirectory : arguments))
          { env = environment,
            std_in = CreatePipe,
            std_out = CreatePipe,
            std_err = CreatePipe
          }
      )
  case (maybeInput, maybeOutput, maybeErrors) of
    (Just input, Just output, Just errors) -> pure (input, output, errors, handle)
    _ -> do
      _ <- terminateCapturedProcess handle
      throwIO (userError "Git process did not provide all requested pipes")

runGit :: GitClient -> FilePath -> Map String String -> Text -> [String] -> ByteString -> IO (Either GitError GitProcessResult)
runGit (GitClient executable) commandDirectory environment operation arguments stdinBytes = do
  inheritedEnvironment <- Map.fromList <$> getEnvironment
  let effectiveEnvironment = Map.toList (environment `Map.union` inheritedEnvironment)
  spawned <- try @IOException (spawnGitProcess executable commandDirectory (Just effectiveEnvironment) arguments)
  case spawned of
    Left _ -> pure (Left (GitExecutableUnavailable executable))
    Right (stdinHandle, stdoutHandle, stderrHandle, stableHandle) -> mask $ \restore -> do
      processWorker <- Async.async (Process.waitForProcess stableHandle)
      stdoutWorker <- Async.async (readHandleAll stdoutHandle)
      stderrWorker <- Async.async (drainBounded stderrHandle)
      let cleanup = do
            beginCloseQuietly stdinHandle
            beginCloseQuietly stdoutHandle
            beginCloseQuietly stderrHandle
            stopped <- terminateCapturedProcess stableHandle
            processStopped <- disposeWorkerBounded processWorker
            nudgeReaderWorkers [stdoutWorker, stderrWorker]
            readers <- awaitReaderWorkersBounded [stdoutWorker, stderrWorker]
            pure (stopped >> processStopped >> readers)
          failClosed problem =
            Left (GitCommandFailed operation (-1) "" (boundedDiagnostic (TextEncoding.encodeUtf8 (Text.pack (ioeGetErrorString problem)))))
      attempted <- try @SomeException $ restore $ do
          BS.hPut stdinHandle stdinBytes
          hClose stdinHandle
          -- Normal command completion is deliberately unbounded: a healthy
          -- Git command may legitimately outlive the old five-second reader
          -- budget.  This body runs restored, so async cancellation still
          -- enters the finite captured-handle cleanup path below.
          -- The monitor owns the possibly blocking ProcessHandle wait.  The
          -- caller waits on Async STM instead, so normal completion is
          -- unbounded while async cancellation remains prompt.
          exitCode <- Async.wait processWorker
          stdoutBytes <- Async.wait stdoutWorker
          stderrBytes <- Async.wait stderrWorker
          pure (exitCode, stdoutBytes, stderrBytes)
      case attempted of
        Left original -> do
          cleanupResult <- cleanup
          case fromException original :: Maybe SomeAsyncException of
            Just _ -> throwIO original
            Nothing ->
              pure $
                case cleanupResult of
                  Left problem -> failClosed problem
                  Right () -> failClosed (userError (show original))
        Right (exitCode, stdoutBytes, stderrBytes) -> do
          -- Handles have reached EOF and workers returned; the cleanup still
          -- closes all owned handles and proves the captured process is gone.
          cleanupResult <- cleanup
          pure $
            case cleanupResult of
              Left problem -> failClosed problem
              Right () ->
                Right
                  GitProcessResult
                    { processExitCode = exitCode,
                      processStdout = stdoutBytes,
                      processStderr = stderrBytes
                    }

commandFailure :: Text -> GitProcessResult -> GitError
commandFailure operation result =
  GitCommandFailed
    operation
    (exitCodeNumber (processExitCode result))
    (boundedDiagnostic (processStdout result))
    (boundedDiagnostic (processStderr result))

exitCodeNumber :: ExitCode -> Int
exitCodeNumber ExitSuccess = 0
exitCodeNumber (ExitFailure value) = value

discoverRepository :: GitClient -> FilePath -> IO (Either GitError Repository)
discoverRepository client input = do
  insideResult <- runGit client input Map.empty "discover inside-worktree" ["rev-parse", "--is-inside-work-tree"] BS.empty
  bareResult <- runGit client input Map.empty "discover bare" ["rev-parse", "--is-bare-repository"] BS.empty
  case (insideResult, bareResult) of
    (Left problem, _) -> pure (Left problem)
    (_, Left problem) -> pure (Left problem)
    (Right insideProbe, Right bareProbe)
      | processExitCode insideProbe /= ExitSuccess || processExitCode bareProbe /= ExitSuccess ->
          pure (Left (GitNotRepository input))
      | otherwise ->
          case (decodeGitBoolean (processStdout insideProbe), decodeGitBoolean (processStdout bareProbe)) of
            (Right True, Right False) -> discoverDetails client input True
            (Right False, Right True) -> discoverDetails client input False
            (Right inside, Right bare) ->
              pure (Left (GitInvalidRepositoryLayout ("inside=" <> renderBool inside <> ", bare=" <> renderBool bare)))
            (Left problem, _) -> pure (Left (GitInvalidOutput "discover inside-worktree" problem))
            (_, Left problem) -> pure (Left (GitInvalidOutput "discover bare" problem))
  where
    renderBool True = "true"
    renderBool False = "false"

repositoryHeadState :: Repository -> IO (Either GitError GitHeadState)
repositoryHeadState repository = do
  result <- runRepository repository "symbolic HEAD" ["symbolic-ref", "--quiet", "HEAD"] BS.empty
  pure $ do
    processResult <- result
    decodeGitHeadState (processExitCode processResult) (processStdout processResult) (processStderr processResult)

decodeGitHeadState :: ExitCode -> ByteString -> ByteString -> Either GitError GitHeadState
decodeGitHeadState exitCode stdoutBytes stderrBytes =
  case exitCode of
    ExitSuccess -> do
      refPath <- first (GitInvalidOutput "symbolic HEAD") (decodeGitPathOutput stdoutBytes)
      case mkGitRef (Text.pack refPath) of
        Left _ -> Left (GitInvalidOutput "symbolic HEAD" (GitMalformedPathOutput (protocolSample stdoutBytes)))
        Right reference -> Right (GitHeadAttached reference)
    ExitFailure 1
      | BS.null stdoutBytes -> Right GitHeadDetached
      | otherwise -> Left (GitInvalidOutput "symbolic HEAD" (GitMalformedPathOutput (protocolSample stdoutBytes)))
    _ ->
      Left
        ( GitCommandFailed
            "symbolic HEAD"
            (exitCodeNumber exitCode)
            (boundedDiagnostic stdoutBytes)
            (boundedDiagnostic stderrBytes)
        )

discoverDetails :: GitClient -> FilePath -> Bool -> IO (Either GitError Repository)
discoverDetails client input hasWorktree = do
  gitDirResult <- pathProbe client input "discover git-dir" ["rev-parse", "--path-format=absolute", "--git-dir"]
  commonDirResult <- pathProbe client input "discover common-dir" ["rev-parse", "--path-format=absolute", "--git-common-dir"]
  rootResult <-
    if hasWorktree
      then fmap Just <$> pathProbe client input "discover worktree root" ["rev-parse", "--path-format=absolute", "--show-toplevel"]
      else pure (Right Nothing)
  case (gitDirResult, commonDirResult, rootResult) of
    (Right gitDirectory, Right commonDirectory, Right maybeRoot) -> do
      commonBareResult <-
        runGit
          client
          commonDirectory
          Map.empty
          "discover common storage"
          ["--git-dir", commonDirectory, "rev-parse", "--is-bare-repository"]
          BS.empty
      pure $ do
        commonBareProbe <- commonBareResult
        if processExitCode commonBareProbe /= ExitSuccess
          then Left (commandFailure "discover common storage" commonBareProbe)
          else do
            commonBare <- first (GitInvalidOutput "discover common storage") (decodeGitBoolean (processStdout commonBareProbe))
            let layout =
                  case maybeRoot of
                    Nothing -> BareRepository
                    Just _
                      | equalFilePath gitDirectory commonDirectory -> MainWorktree
                      | otherwise -> LinkedWorktree
                commandDirectory = maybe commonDirectory id maybeRoot
            if layout == BareRepository && not commonBare
              then Left (GitInvalidRepositoryLayout "direct bare repository reported a non-bare common directory")
              else
                Right
                  Repository
                    { repositoryClient = client,
                      repositoryWorktreeRoot = maybeRoot,
                      repositoryGitDir = gitDirectory,
                      repositoryCommonDir = commonDirectory,
                      repositoryLayout = layout,
                      repositoryCommonIsBare = commonBare,
                      repositoryCommandDirectory = commandDirectory
                    }
    (Left problem, _, _) -> pure (Left problem)
    (_, Left problem, _) -> pure (Left problem)
    (_, _, Left problem) -> pure (Left problem)

pathProbe :: GitClient -> FilePath -> Text -> [String] -> IO (Either GitError FilePath)
pathProbe client directory operation arguments = do
  result <- runGit client directory Map.empty operation arguments BS.empty
  case result of
    Left problem -> pure (Left problem)
    Right processResult
      | processExitCode processResult /= ExitSuccess -> pure (Left (commandFailure operation processResult))
      | otherwise ->
          case decodeGitPathOutput (processStdout processResult) of
            Left problem -> pure (Left (GitInvalidOutput operation problem))
            Right path -> do
              exists <- doesDirectoryExist path
              if exists
                then Right <$> canonicalizePath path
                else pure (Left (GitInvalidRepositoryLayout (operation <> " returned a missing directory")))

decodeGitPathOutput :: ByteString -> Either GitProtocolError FilePath
decodeGitPathOutput raw = do
  body <- stripRequiredLineEnding (GitMalformedPathOutput (protocolSample raw)) raw
  if BS.null body || BS.any (`elem` [0, 10, 13]) body
    then Left (GitMalformedPathOutput (protocolSample raw))
    else
      case TextEncoding.decodeUtf8' body of
        Left _ -> Left (GitMalformedPathOutput (protocolSample raw))
        Right value -> Right (Text.unpack value)

decodeGitBoolean :: ByteString -> Either GitProtocolError Bool
decodeGitBoolean raw = do
  body <- stripRequiredLineEnding (GitMalformedBoolean (protocolSample raw)) raw
  case body of
    "true" -> Right True
    "false" -> Right False
    _ -> Left (GitMalformedBoolean (protocolSample raw))

stripRequiredLineEnding :: GitProtocolError -> ByteString -> Either GitProtocolError ByteString
stripRequiredLineEnding problem raw
  | "\r\n" `BS.isSuffixOf` raw = Right (BS.take (BS.length raw - 2) raw)
  | "\n" `BS.isSuffixOf` raw = Right (BS.take (BS.length raw - 1) raw)
  | otherwise = Left problem

resolveRevision :: Repository -> RevisionSpec -> IO (Either GitError GitOid)
resolveRevision repository spec = do
  objectProbe <- runRepository repository "resolve object" ["rev-parse", "--verify", "--end-of-options", Text.unpack (revisionSpecText spec)] BS.empty
  case objectProbe of
    Left problem -> pure (Left problem)
    Right result
      | processExitCode result /= ExitSuccess -> pure (Left (GitUnknownRevision spec))
      | otherwise ->
          case parseSingleOid "resolve object" (processStdout result) of
            Left problem -> pure (Left problem)
            Right objectOid -> do
              commitProbe <-
                runRepository
                  repository
                  "resolve commit"
                  ["rev-parse", "--verify", "--end-of-options", Text.unpack (gitOidText objectOid <> "^{commit}")]
                  BS.empty
              pure $
                case commitProbe of
                  Left problem -> Left problem
                  Right commitResult
                    | processExitCode commitResult /= ExitSuccess -> Left (GitRevisionNotCommit spec)
                    | otherwise -> parseSingleOid "resolve commit" (processStdout commitResult)

isShallowRepository :: Repository -> IO (Either GitError Bool)
isShallowRepository repository = do
  result <- runRepository repository "shallow repository" ["rev-parse", "--is-shallow-repository"] BS.empty
  pure $ do
    processResult <- result
    if processExitCode processResult /= ExitSuccess
      then Left (commandFailure "shallow repository" processResult)
      else first (GitInvalidOutput "shallow repository") (decodeGitBoolean (processStdout processResult))

reachableCommitGraphAt :: Repository -> GitOid -> IO (Either GitError [GitCommitNode])
reachableCommitGraphAt repository target = do
  result <-
    runRepository
      repository
      "reachable commit graph"
      ["rev-list", "--topo-order", "--reverse", "--parents", Text.unpack (gitOidText target)]
      BS.empty
  pure $ do
    processResult <- result
    if processExitCode processResult /= ExitSuccess
      then Left (commandFailure "reachable commit graph" processResult)
      else decodeGitCommitGraph (processStdout processResult)

-- | Bound history-delta requests by merge arity, not graph size.  A commit
-- appears once in parent-ordinal zero (roots included) and once for each
-- additional original parent it has.  Each synthetic node has at most one
-- parent so its returned raw delta has an unambiguous original edge owner.
historyTreeDeltaBatchCount :: [GitCommitNode] -> Int
historyTreeDeltaBatchCount = length . groupHistoryTreeDeltaEdges

groupHistoryTreeDeltaEdges :: [GitCommitNode] -> [[GitCommitNode]]
groupHistoryTreeDeltaEdges selectedNodes =
  filter (not . null) [edgeGroup parentIndex | parentIndex <- [0 .. maximum (0 : map (length . gitCommitNodeParents) selectedNodes)]]
  where
    edgeGroup parentIndex =
      [ case drop parentIndex (gitCommitNodeParents node) of
          [] | parentIndex == 0 && null (gitCommitNodeParents node) -> node
          parent : _ -> node {gitCommitNodeParents = [parent]}
          [] -> node {gitCommitNodeParents = []}
        | node <- selectedNodes,
          parentIndex == 0 || length (gitCommitNodeParents node) > parentIndex
      ]

-- | Batch all reachable parent-edge deltas beneath literal path roots.
--
-- @--always@ is essential: empty edges must still be framed so the decoder
-- can account for every merge parent rather than silently treating a missing
-- record as an absent edge.  @--no-renames@ preserves the historical
-- add/delete semantics used by append-only validation.
historyTreeDeltasAt :: Repository -> Bool -> [GitCommitNode] -> RepoPath -> [RepoPath] -> IO (Either GitError [GitHistoryTreeDelta])
historyTreeDeltasAt repository allowPathSelection nodes configRoot roots =
  if null nodes
    then pure (Right [])
    else do
      selectedResult <- selectHistoryNodes
      case selectedResult of
        Left problem -> pure (Left problem)
        Right semanticNodes -> do
          let pathspecs = map (Text.unpack . repoPathText) (Map.keys (Map.fromList [(root, ()) | root <- roots]))
              arguments =
                [ "--literal-pathspecs",
                  "diff-tree",
                  "--stdin",
                  "--root",
                  "-r",
                  "-t",
                  "--no-renames",
                  "--raw",
                  "--no-abbrev",
                  "--always",
                  "-z",
                  "--pretty=tformat:%x1e%H"
                ]
                  <> if null pathspecs then [] else "--" : pathspecs
          groupResults <- traverse (runHistoryDeltaGroup arguments) (groupHistoryTreeDeltaEdges semanticNodes)
          pure $ do
            selectedDeltas <- concat <$> sequence groupResults
            Right (restoreEmptyEdges nodes selectedDeltas)
  where
    -- Path-limited rev-list with --full-history preserves commits selected by
    -- every original merge side.  It is used only as a set selector: all
    -- parent relationships below come from the original full graph, never the
    -- potentially simplified parent links emitted by a path-limited walk.
    -- A shallow boundary and any post-root config edit are conservative full
    -- traversal cases: a historical custom root then cannot be proved from
    -- the current path configuration alone.
    selectHistoryNodes
      | not allowPathSelection = pure (Right nodes)
      | otherwise = do
          configResult <- historyPathRelevantNodes repository nodes [configRoot]
          semanticResult <- historyPathRelevantNodes repository nodes roots
          pure $ do
            configNodes <- configResult
            semanticNodes <- semanticResult
            let configChangedAfterRoot = any (not . null . gitCommitNodeParents) configNodes
                -- A merge may be tree-identical to one parent while differing
                -- from another at a semantic root.  rev-list's dense path
                -- selection may omit that merge, so preserve every merge for
                -- the original per-parent diff check.
                selectedOids = Set.fromList (map gitCommitNodeOid semanticNodes <> [gitCommitNodeOid node | node <- nodes, length (gitCommitNodeParents node) > 1])
                semanticSelected = if configChangedAfterRoot then nodes else [node | node <- nodes, gitCommitNodeOid node `Set.member` selectedOids]
            Right semanticSelected

    -- A two-object stdin line asks Git to compare exactly that original edge.
    -- Keep each parent ordinal in its own bounded process: Git coalesces
    -- repeated merge commit headers within one --stdin walk, whereas each
    -- group contains a child at most once. This is at most the largest merge
    -- arity in batched invocations, never one process per commit.
    runHistoryDeltaGroup arguments group = do
      let payload = TextEncoding.encodeUtf8 (Text.intercalate "\n" (concatMap historyDeltaRequests group) <> "\n")
      result <- runRepository repository "history tree deltas" arguments payload
      pure $ do
        processResult <- result
        if processExitCode processResult /= ExitSuccess
          then Left (commandFailure "history tree deltas" processResult)
          else decodeGitHistoryTreeDeltas group (processStdout processResult)

    -- Roots retain the one-object form so --root compares them with the null
    -- tree. Every other request names exactly one original parent edge.
    historyDeltaRequests node =
      case gitCommitNodeParents node of
        [] -> [gitOidText (gitCommitNodeOid node)]
        parents -> [gitOidText (gitCommitNodeOid node) <> " " <> gitOidText parent | parent <- parents]

-- | Select graph nodes whose trees are relevant to literal semantic roots.
-- @--full-history@ is deliberately retained even though parent topology is
-- discarded: without it a path-limited merge walk may prune a side whose
-- original parent edge must still be checked by the compiler.
historyPathRelevantNodes :: Repository -> [GitCommitNode] -> [RepoPath] -> IO (Either GitError [GitCommitNode])
historyPathRelevantNodes repository nodes roots = do
  let target = gitCommitNodeOid (last nodes)
      pathspecs = map (Text.unpack . repoPathText) (Map.keys (Map.fromList [(root, ()) | root <- roots]))
      arguments =
        [ "--literal-pathspecs",
          "rev-list",
          "--full-history",
          "--topo-order",
          Text.unpack (gitOidText target)
        ]
          <> if null pathspecs then [] else "--" : pathspecs
  result <- runRepository repository "history path selection" arguments BS.empty
  pure $ do
    processResult <- result
    if processExitCode processResult /= ExitSuccess
      then Left (commandFailure "history path selection" processResult)
      else do
        selectedOids <- decodeHistoryPathSelection nodes (processStdout processResult)
        Right [node | node <- nodes, gitCommitNodeOid node `Set.member` selectedOids]

decodeHistoryPathSelection :: [GitCommitNode] -> ByteString -> Either GitError (Set.Set GitOid)
decodeHistoryPathSelection nodes raw = do
  values <- traverse decodeLine (filter (not . BS.null) (BS.split 10 raw))
  let known = Set.fromList (map gitCommitNodeOid nodes)
  if all (`Set.member` known) values
    then Right (Set.fromList values)
    else Left (GitInvalidOutput "history path selection" (GitMalformedTreeRecord "path selection returned a commit outside the full graph"))
  where
    decodeLine line =
      case TextEncoding.decodeUtf8' line of
        Left _ -> Left (GitInvalidOutput "history path selection" (GitMalformedTreeRecord (protocolSample line)))
        Right text ->
          case mkGitOid text of
            Left _ -> Left (GitInvalidOutput "history path selection" (GitMalformedTreeRecord (protocolSample line)))
            Right oid -> Right oid

restoreEmptyEdges :: [GitCommitNode] -> [GitHistoryTreeDelta] -> [GitHistoryTreeDelta]
restoreEmptyEdges nodes selected =
  [ Map.findWithDefault (GitHistoryTreeDelta (gitCommitNodeOid node) parent []) (gitCommitNodeOid node, parent) byEdge
    | node <- nodes,
      parent <- edgeParents node
  ]
  where
    byEdge = Map.fromList [((gitHistoryTreeDeltaCommit delta, gitHistoryTreeDeltaParent delta), delta) | delta <- selected]
    edgeParents node = case gitCommitNodeParents node of [] -> [Nothing]; parents -> map Just parents

-- | Decode the strict byte protocol emitted by 'historyTreeDeltasAt'.  The
-- repeated commit headers from @-m@ are matched in parent order supplied by
-- the already-validated commit graph; Git's raw records carry no parent oid.
decodeGitHistoryTreeDeltas :: [GitCommitNode] -> ByteString -> Either GitError [GitHistoryTreeDelta]
decodeGitHistoryTreeDeltas nodes raw = do
  validateGraphOidWidths
  go expected raw []
  where
    expected = concatMap edgeHeaders nodes
    -- Git does not annotate raw diff sides with the repository object format.
    -- The already validated commit graph does, so use its OID width as the
    -- protocol width for *every* side, including the all-zero absent sentinel.
    -- This prevents a short or cross-format zero from bypassing 'mkGitOid'.
    oidWidth =
      case nodes of
        node : _ -> Text.length (gitOidText (gitCommitNodeOid node))
        [] -> 0
    validateGraphOidWidths =
      case Set.toList (Set.fromList (map (Text.length . gitOidText) (concatMap nodeOids nodes))) of
        [] -> Right ()
        [_] -> Right ()
        _ -> Left (malformed "mixed OID widths in commit graph" raw)
    nodeOids node = gitCommitNodeOid node : gitCommitNodeParents node
    edgeHeaders node =
      [ (gitCommitNodeOid node, parent)
        | parent <- case gitCommitNodeParents node of [] -> [Nothing]; parents -> map Just parents
      ]
    go [] bytes accumulated
      | BS.null bytes = Right (reverse accumulated)
      | otherwise = Left (malformed "unexpected trailing history-delta bytes" bytes)
    go ((expectedCommit, expectedParent) : remaining) bytes accumulated = do
      (actualCommit, afterHeader) <- decodeHeader bytes
      if actualCommit /= expectedCommit
        then Left (malformed "history-delta commit header does not match graph" (TextEncoding.encodeUtf8 (gitOidText actualCommit)))
        else do
          (changes, afterChanges) <- decodeChanges afterHeader []
          go remaining afterChanges (GitHistoryTreeDelta expectedCommit expectedParent (reverse changes) : accumulated)

    decodeHeader bytes
      | BS.null bytes || BS.head bytes /= recordSeparator = Left (malformed "missing history-delta record separator" bytes)
      | otherwise =
          case BS.break (== 0) (BS.tail bytes) of
            (_, terminator) | BS.null terminator -> Left (malformed "unterminated history-delta header" bytes)
            (oidBytes, terminator) -> do
               oidText <- first (const (malformed "invalid history-delta oid" oidBytes)) (TextEncoding.decodeUtf8' oidBytes)
               oid <- first (const (malformed "invalid history-delta oid" oidBytes)) (mkGitOid oidText)
               case BS.uncons (BS.tail terminator) of
                 Just (10, afterNewline) -> Right (oid, afterNewline)
                 -- With @-z@, Git omits the pretty-format newline for an
                 -- empty edge and emits the following record separator
                 -- immediately.  Preserve that separator for
                 -- 'decodeChanges', which represents the empty edge.
                 Just (separator, _) | separator == recordSeparator -> Right (oid, BS.tail terminator)
                 -- The final reachable commit can likewise have an empty
                 -- selected-path delta, leaving its NUL-terminated header at
                 -- EOF rather than followed by a pretty-format newline.
                 Nothing -> Right (oid, BS.empty)
                 _ -> Left (malformed "history-delta header missing newline" bytes)

    decodeChanges bytes accumulated
      | BS.null bytes = Right (accumulated, bytes)
      | BS.head bytes == recordSeparator = Right (accumulated, bytes)
      | otherwise = do
          (change, remaining) <- decodeChange bytes
          decodeChanges remaining (change : accumulated)

    decodeChange bytes =
      case BS.break (== 0) bytes of
        (_, terminator) | BS.null terminator -> Left (malformed "unterminated history-delta metadata" bytes)
        (metadata, terminator) -> do
          (pathBytes, pathTerminator) <-
            case BS.break (== 0) (BS.tail terminator) of
              (_, finalTerminator) | BS.null finalTerminator -> Left (malformed "unterminated history-delta path" metadata)
              pair -> Right pair
          pathText <- first (const (GitInvalidUtf8Path (protocolSample pathBytes))) (TextEncoding.decodeUtf8' pathBytes)
          path <- first (GitInvalidRepositoryPath pathText) (mkRepoPath pathText)
          (oldEntry, newEntry) <- decodeMetadata path metadata
          Right (GitTreeChange path oldEntry newEntry, BS.tail pathTerminator)

    decodeMetadata path metadata =
      case BS8.split ' ' metadata of
        [oldMode, newMode, oldOid, newOid, status]
          | BS.isPrefixOf ":" oldMode -> do
              oldEntry <- decodeSide path (BS.drop 1 oldMode) oldOid
              newEntry <- decodeSide path newMode newOid
              validateStatus status oldEntry newEntry metadata
              Right (oldEntry, newEntry)
        _ -> Left (malformed "malformed history-delta metadata" metadata)

    validateStatus status oldEntry newEntry metadata
      | status == "A", Nothing <- oldEntry, Just _ <- newEntry = Right ()
      | status == "D", Just _ <- oldEntry, Nothing <- newEntry = Right ()
      | status `elem` ["M", "T"], Just _ <- oldEntry, Just _ <- newEntry = Right ()
      | otherwise = Left (malformed "unsupported history-delta status" metadata)

    decodeSide path modeRaw oidRaw
       | BS.length oidRaw /= oidWidth = Left (malformed "history-delta oid width does not match commit graph" oidRaw)
       | modeRaw == "000000" && zeroOid oidRaw = Right Nothing
       | zeroOid oidRaw = Left (malformed "zero history-delta oid requires zero mode" oidRaw)
      | otherwise = do
          mode <- first (const (malformed "invalid history-delta mode" modeRaw)) (parseFileMode (BS8.unpack modeRaw))
          oidText <- first (const (malformed "invalid history-delta oid" oidRaw)) (TextEncoding.decodeUtf8' oidRaw)
          oid <- first (const (malformed "invalid history-delta oid" oidRaw)) (mkGitOid oidText)
          let objectType = objectTypeForMode mode
          Right (Just (GitTreeEntry path oid objectType mode))

    zeroOid oid = BS.length oid == oidWidth && oidWidth > 0 && BS.all (== 48) oid
    objectTypeForMode mode = case mode of
      GitRegularFile -> GitBlobObject
      GitExecutableFile -> GitBlobObject
      GitSymbolicLink -> GitBlobObject
      GitSubmodule -> GitCommitObject
      GitDirectory -> GitTreeObject
    recordSeparator = 0x1e
    malformed label sample = GitInvalidOutput "history tree deltas" (GitMalformedTreeRecord (TextEncoding.encodeUtf8 label <> ": " <> protocolSample sample))

decodeGitCommitGraph :: ByteString -> Either GitError [GitCommitNode]
decodeGitCommitGraph raw
  | BS.null raw = Left malformedRaw
  | BS.last raw /= 10 = Left malformedRaw
  | BS.elem 13 raw = Left malformedRaw
  | otherwise = do
      let records = BS.split 10 raw
      linesBytes <-
        case reverse records of
          [] -> Left malformedRaw
          finalRecord : reversed
            | not (BS.null finalRecord) || any BS.null reversed -> Left malformedRaw
            | otherwise -> Right (reverse reversed)
      nodes <- traverse decodeLine linesBytes
      validateGraphOids nodes
      let grouped = Map.fromListWith (<>) [(gitCommitNodeOid node, [node]) | node <- nodes]
      traverse_ rejectDuplicate (Map.elems grouped)
      Right nodes
  where
    malformedRaw = GitInvalidOutput "reachable commit graph" (GitMalformedObjectHeader (protocolSample raw))
    decodeLine line =
      case BS.split 32 line of
        [] -> Left malformed
        tokens | any BS.null tokens -> Left malformed
        oidBytes : parentBytes -> do
          oidText <- decodeToken oidBytes
          parentTexts <- traverse decodeToken parentBytes
          oid <- first (const malformed) (mkGitOid oidText)
          parents <- traverse (first (const malformed) . mkGitOid) parentTexts
          Right (GitCommitNode oid parents)
      where
        malformed = GitInvalidOutput "reachable commit graph" (GitMalformedObjectHeader (protocolSample line))
        decodeToken token = first (const malformed) (TextEncoding.decodeUtf8' token)
    rejectDuplicate [] = Right ()
    rejectDuplicate [_] = Right ()
    rejectDuplicate _ = Left (GitInvalidOutput "reachable commit graph" (GitMalformedObjectHeader "duplicate commit node"))
    validateGraphOids nodes =
      case Set.toList (Set.fromList (map (Text.length . gitOidText) (concatMap nodeOids nodes))) of
        [] -> Right ()
        [_] ->
          if any (Text.all (== '0') . gitOidText) (concatMap nodeOids nodes)
            then Left (GitInvalidOutput "reachable commit graph" (GitMalformedObjectHeader "zero commit graph oid"))
            else Right ()
        _ -> Left (GitInvalidOutput "reachable commit graph" (GitMalformedObjectHeader "mixed commit graph oid widths"))
    nodeOids node = gitCommitNodeOid node : gitCommitNodeParents node

runRepository :: Repository -> Text -> [String] -> ByteString -> IO (Either GitError GitProcessResult)
runRepository repository = runRepositoryWithEnvironment repository Map.empty

-- | Run a repository command with explicit environment-variable overrides.
--
-- The override map is layered over the process environment so that callers can
-- direct a single Git invocation (for example, with @GIT_INDEX_FILE@) without
-- losing inherited variables such as @PATH@.
runRepositoryWithEnvironment :: Repository -> Map String String -> Text -> [String] -> ByteString -> IO (Either GitError GitProcessResult)
runRepositoryWithEnvironment repository environment =
  runGit (repositoryClient repository) (repositoryCommandDirectory repository) environment

listTreeEntriesAt :: Repository -> GitOid -> [RepoPath] -> IO (Either GitError [GitTreeEntry])
listTreeEntriesAt repository revision roots = do
  raw <- listTreeEntriesForRepositoryValidationAt repository revision roots
  pure $ do
    entries <- raw
    let grouped = Map.fromListWith (<>) [(gitTreePath entry, [entry]) | entry <- entries]
    traverse_ rejectContradiction (Map.elems grouped)
    Right (mapMaybe firstEntry (Map.elems grouped))
  where
    rejectContradiction [] = Right ()
    rejectContradiction (candidate : remaining)
      | all (== candidate) remaining = Right ()
      | otherwise = Left (GitInvalidOutput "list tree" (GitMalformedTreeRecord "contradictory duplicate path"))
    firstEntry [] = Nothing
    firstEntry (entry : _) = Just entry

-- | Repository validation input: preserves fully decoded duplicate path
-- candidates so Repository can report its domain-specific contradiction.
listTreeEntriesForRepositoryValidationAt :: Repository -> GitOid -> [RepoPath] -> IO (Either GitError [GitTreeEntry])
listTreeEntriesForRepositoryValidationAt repository revision roots = do
  let normalizedRoots = map repoPathText (Map.keys (Map.fromList [(path, ()) | path <- roots]))
      chunks = if null normalizedRoots then [[]] else chunksOf 256 normalizedRoots
  results <- traverse listChunk chunks
  pure $ do
    entries <- concat <$> sequence results
    Right (sortOn (\entry -> (repoPathText (gitTreePath entry), gitOidText (gitTreeOid entry))) entries)
  where
    listChunk rootChunk = do
      let arguments =
            ["--literal-pathspecs", "ls-tree", "-r", "-t", "-z", "--full-tree", Text.unpack (gitOidText revision)]
              <> if null rootChunk then [] else "--" : map Text.unpack rootChunk
      result <- runRepository repository "list tree" arguments BS.empty
      pure $ do
        processResult <- result
        if processExitCode processResult /= ExitSuccess
          then Left (commandFailure "list tree" processResult)
          else decodeGitTreeOutput (processStdout processResult)

lookupTreeEntryAt :: Repository -> GitOid -> RepoPath -> IO (Either GitError (Maybe GitTreeEntry))
lookupTreeEntryAt repository revision path = do
  result <- runRepository repository "lookup tree path" arguments BS.empty
  pure $ do
    processResult <- result
    if processExitCode processResult /= ExitSuccess
      then Left (commandFailure "lookup tree path" processResult)
      else do
        entries <- decodeGitTreeOutput (processStdout processResult)
        case filter ((== path) . gitTreePath) entries of
          [] -> Right Nothing
          [entry] -> Right (Just entry)
          _ -> Left (GitInvalidOutput "lookup tree path" (GitMalformedTreeRecord "duplicate exact path"))
  where
    arguments =
      [ "--literal-pathspecs",
        "ls-tree",
        "-z",
        "--full-tree",
        Text.unpack (gitOidText revision),
        "--",
        Text.unpack (repoPathText path)
      ]

-- | Observe a bounded set of exact paths in one revision.  This is the
-- multi-path counterpart to 'lookupTreeEntryAt': path arguments remain argv
-- values under @--literal-pathspecs@ and the response is decoded from its NUL
-- framing before it is associated with a requested path.  A caller therefore
-- gets a total map (including absent paths) without paying one Git process per
-- path.
--
-- Git's command-line and pipe limits are kept bounded by the same 256-item
-- window used for object batches.  Duplicate requested paths are folded before
-- invocation; contradictory duplicate output is rejected rather than picked
-- arbitrarily.
lookupTreeEntriesAt :: Repository -> GitOid -> [RepoPath] -> IO (Either GitError (Map RepoPath (Maybe GitTreeEntry)))
lookupTreeEntriesAt repository revision paths = do
  let requested = Map.keys (Map.fromList [(path, ()) | path <- paths])
  let GitClient executable = repositoryClient repository
  case treePathArgumentWindowsForCommand executable (repositoryCommandDirectory repository) revision requested of
    Left problem -> pure (Left problem)
    Right chunks -> do
      results <- traverse lookupChunk chunks
      pure $ do
        windowMaps <- sequence results
        foldM mergeWindow Map.empty windowMaps
  where
    lookupChunk pathsChunk = do
      let arguments =
            [ "--literal-pathspecs",
              "ls-tree",
              "-z",
              "--full-tree",
              Text.unpack (gitOidText revision),
              "--"
            ] <> map (Text.unpack . repoPathText) pathsChunk
      result <- runRepository repository "lookup tree paths" arguments BS.empty
      pure $ do
        processResult <- result
        if processExitCode processResult /= ExitSuccess
          then Left (commandFailure "lookup tree paths" processResult)
          else decodeGitTreeOutput (processStdout processResult) >>= reconcileExactTreeEntries pathsChunk
    mergeWindow accumulated window =
      foldM insertOne accumulated (Map.toList window)
    insertOne accumulated (path, entry)
      | Map.member path accumulated = Left (GitInvalidOutput "lookup tree paths" (GitMalformedTreeRecord "duplicate path across exact request windows"))
      | otherwise = Right (Map.insert path entry accumulated)

-- | Reconcile NUL-decoded @ls-tree@ records with the exact literal paths sent
-- to Git. A success therefore remains path-correlated, while a requested path
-- with no returned record is the only accepted absence.
reconcileExactTreeEntries :: [RepoPath] -> [GitTreeEntry] -> Either GitError (Map RepoPath (Maybe GitTreeEntry))
reconcileExactTreeEntries requested entries = do
  traverse_ rejectUnexpected (Map.toList grouped)
  exact <- traverse exactEntry (Map.toList grouped)
  let observed = Map.fromList exact
  Right (Map.fromList [(path, Map.lookup path observed) | path <- canonicalRequested])
  where
    canonicalRequested = Map.keys (Map.fromList [(path, ()) | path <- requested])
    requestedSet = Map.fromList [(path, ()) | path <- canonicalRequested]
    grouped = Map.fromListWith (<>) [(gitTreePath entry, [entry]) | entry <- entries]
    rejectUnexpected (path, _)
      | Map.member path requestedSet = Right ()
      | otherwise = Left (GitInvalidOutput "lookup tree paths" (GitMalformedTreeRecord "non-exact path response"))
    exactEntry (_, []) = Left (GitInvalidOutput "lookup tree paths" (GitMalformedTreeRecord "empty path response group"))
    exactEntry (path, entry : remaining)
      | null remaining = Right (path, entry)
      | otherwise = Left (GitInvalidOutput "lookup tree paths" (GitMalformedTreeRecord "duplicate exact path response"))

-- | Observe exact paths across many commit revisions through finite buffered
-- @cat-file --batch-check@ windows.  Each revision is first proven to be a
-- commit, because @cat-file <oid>:<path>@ reports a missing path, a missing
-- revision, and a non-commit revision with the same ambiguous @missing@
-- response.  The result remains total for every requested revision/path pair.
lookupTreeObjectInfoAtRevisions
  :: Repository
  -> Map GitOid (Set.Set RepoPath)
  -> [GitOid]
  -> IO (Either GitError (Map GitOid (Map RepoPath (Maybe GitObjectInfo))))
lookupTreeObjectInfoAtRevisions repository requestedByRevision expectedBlobOids = do
  let revisions = Map.keys requestedByRevision
  if null revisions && null expectedBlobOids
    then pure (Right Map.empty)
    else do
      objectInfo <- batchObjectInfo repository (revisions <> expectedBlobOids)
      case objectInfo of
        Left problem -> pure (Left problem)
        Right infos ->
          case validateCommitRevisions revisions infos >> validateBlobObjects expectedBlobOids infos of
             Left problem -> pure (Left problem)
             Right () -> do
               let (batchRequests, fallbackRequested) = partitionTreePathRequests requestedByRevision
                   seeded = Map.map (const Map.empty) requestedByRevision
               batched <- observeTreePathBatch repository batchRequests
               case batched of
                 Left problem -> pure (Left problem)
                 Right batchedInfos -> do
                   fallback <- traverse observeFallbackRevision (Map.toAscList fallbackRequested)
                   case sequence fallback of
                     Left problem -> pure (Left problem)
                     Right fallbackEntries -> do
                       metadata <- batchObjectInfo repository [gitTreeOid entry | (_, entries) <- fallbackEntries, Just entry <- Map.elems entries]
                       pure $ do
                         safeValues <- mergeTreePathObservations seeded batchedInfos
                         fallbackInfos <- metadata
                         fallbackValues <- traverse (traverse (traverse (entryInfo fallbackInfos))) (Map.fromList fallbackEntries)
                         mergeTreePathObservations safeValues fallbackValues
  where
    observeFallbackRevision (revision, paths) = do
      entries <- lookupTreeEntriesAt repository revision (Set.toAscList paths)
      pure ((revision,) <$> entries)
    entryInfo infos entry =
      case Map.lookup (gitTreeOid entry) infos of
        Just (Just info)
          | objectInfoOid info == gitTreeOid entry
              && objectInfoType info == gitTreeObjectType entry -> Right info
        _ -> Left (GitInvalidOutput "lookup tree paths" (GitMalformedObjectHeader "tree entry object metadata did not round-trip"))

-- | A line-safe request is served by one persistent custom-format
-- @cat-file --batch-check@ child.  The token in @%(rest)@ correlates every
-- present response; Git's missing form also repeats the exact input expression
-- so a missing path is never inferred positionally.  Paths with framing bytes
-- (or a response too wide for the bounded header reader) continue through the
-- literal-pathspec, NUL-framed @ls-tree@ authority below.
data TreePathBatchRequest = TreePathBatchRequest
  { treePathRequestRevision :: GitOid,
    treePathRequestPath :: RepoPath,
    treePathRequestExpression :: ByteString,
    treePathRequestToken :: ByteString
  }

treePathBatchProtocolLineLimit :: Int
treePathBatchProtocolLineLimit = 400

partitionTreePathRequests :: Map GitOid (Set.Set RepoPath) -> ([TreePathBatchRequest], Map GitOid (Set.Set RepoPath))
partitionTreePathRequests requestedByRevision =
  let (reversedEligible, fallback) = foldl collect ([], Map.empty) (zip [0 :: Int ..] canonical)
   in (reverse reversedEligible, fallback)
  where
    canonical =
      [ (revision, path)
      | (revision, paths) <- Map.toAscList requestedByRevision,
        path <- Set.toAscList paths
      ]
    collect (eligible, fallback) (ordinal, (revision, path)) =
      let expression = treePathExpression revision path
          token = BS8.pack ("adrai-tree-" <> padDecimal 8 ordinal)
          request = TreePathBatchRequest revision path expression token
       in if treePathBatchEligible expression token
            then (request : eligible, fallback)
            else (eligible, Map.insertWith Set.union revision (Set.singleton path) fallback)

padDecimal :: Int -> Int -> String
padDecimal width value = replicate (max 0 (width - length rendered)) '0' <> rendered
  where
    rendered = show value

treePathExpression :: GitOid -> RepoPath -> ByteString
treePathExpression revision path =
  TextEncoding.encodeUtf8 (gitOidText revision <> ":" <> repoPathText path)

treePathBatchEligible :: ByteString -> ByteString -> Bool
treePathBatchEligible expression token =
  not (BS.null expression)
    && BS.length expression + BS.length token + 32 <= treePathBatchProtocolLineLimit
    && BS.all isSafe expression
    && BS.all isSafe token
  where
    isSafe value = value > 32 && value /= 127

mergeTreePathObservations
  :: Map GitOid (Map RepoPath (Maybe GitObjectInfo))
  -> Map GitOid (Map RepoPath (Maybe GitObjectInfo))
  -> Either GitError (Map GitOid (Map RepoPath (Maybe GitObjectInfo)))
mergeTreePathObservations left right = Map.foldlWithKey' mergeRevision (Right left) right
  where
    mergeRevision accumulated revision entries = do
      values <- accumulated
      case Map.lookup revision values of
        Nothing -> Right (Map.insert revision entries values)
        Just existing -> do
          merged <- Map.foldlWithKey' mergePath (Right existing) entries
          Right (Map.insert revision merged values)
    mergePath accumulated path value = do
      values <- accumulated
      if Map.member path values
        then Left (GitInvalidOutput "cat-file tree paths" (GitMalformedObjectHeader "duplicate tree-path observation"))
        else Right (Map.insert path value values)

data GitTreePathBatchSession = GitTreePathBatchSession Handle Handle PipelineCleanupRegistry

withTreePathBatchSession :: Repository -> (GitTreePathBatchSession -> IO (Either GitError value)) -> IO (Either GitError value)
withTreePathBatchSession repository interaction =
  withGitPipes repository "cat-file tree paths" ["cat-file", "--batch-check=%(objectname) %(objecttype) %(objectsize) %(rest)"] $ \stdinHandle stdoutHandle registry -> do
    outcome <- interaction (GitTreePathBatchSession stdinHandle stdoutHandle registry)
    case outcome of
      Left problem -> pure (Left problem)
      Right value -> finishObjectInfoBatchInputFor "cat-file tree paths" stdinHandle stdoutHandle value

observeTreePathBatch :: Repository -> [TreePathBatchRequest] -> IO (Either GitError (Map GitOid (Map RepoPath (Maybe GitObjectInfo))))
observeTreePathBatch _ [] = pure (Right Map.empty)
observeTreePathBatch repository requests =
  withTreePathBatchSession repository $ \session ->
    go session (chunksOf objectWindowLimit requests) Map.empty
  where
    go _ [] accumulated = pure (Right accumulated)
    go session (window : remaining) accumulated = do
      current <- observeTreePathWindow session window
      case current of
        Left problem -> pure (Left problem)
        Right values ->
          case mergeTreePathObservations accumulated values of
            Left problem -> pure (Left problem)
            Right next -> go session remaining next

observeTreePathWindow :: GitTreePathBatchSession -> [TreePathBatchRequest] -> IO (Either GitError (Map GitOid (Map RepoPath (Maybe GitObjectInfo))))
observeTreePathWindow _ [] = pure (Right Map.empty)
observeTreePathWindow _ requested | length requested > objectWindowLimit = pure (Left (objectWindowLimitError "cat-file tree paths"))
observeTreePathWindow (GitTreePathBatchSession stdinHandle stdoutHandle registry) requested =
  pipelineBatchLines registry "cat-file tree paths" (handleBatchInput stdinHandle) (map treePathRequestLine requested) (readTreePathResponses stdoutHandle requested)

treePathRequestLine :: TreePathBatchRequest -> ByteString
treePathRequestLine request = treePathRequestExpression request <> " " <> treePathRequestToken request <> "\n"

readTreePathResponses :: Handle -> [TreePathBatchRequest] -> IO (Either GitError (Map GitOid (Map RepoPath (Maybe GitObjectInfo))))
readTreePathResponses stdoutHandle = go Map.empty
  where
    go accumulated [] = pure (Right accumulated)
    go accumulated (expected : remaining) = do
      response <- readBatchHeader stdoutHandle
      case response of
        Left problem -> pure (Left problem)
        Right header ->
          case decodeTreePathBatchHeader expected header of
            Left problem -> pure (Left problem)
            Right value ->
              case insertTreePathResponse expected value accumulated of
                Left problem -> pure (Left problem)
                Right next -> go next remaining

insertTreePathResponse :: TreePathBatchRequest -> Maybe GitObjectInfo -> Map GitOid (Map RepoPath (Maybe GitObjectInfo)) -> Either GitError (Map GitOid (Map RepoPath (Maybe GitObjectInfo)))
insertTreePathResponse request value accumulated =
  let revision = treePathRequestRevision request
      path = treePathRequestPath request
      paths = Map.findWithDefault Map.empty revision accumulated
   in if Map.member path paths
        then Left (GitInvalidOutput "cat-file tree paths" (GitMalformedObjectHeader "duplicate correlated tree-path response"))
        else Right (Map.insert revision (Map.insert path value paths) accumulated)

decodeTreePathBatchHeader :: TreePathBatchRequest -> ByteString -> Either GitError (Maybe GitObjectInfo)
decodeTreePathBatchHeader expected header =
  case treePathHeaderFields header of
    [returnedRaw, typeRaw, sizeRaw, token]
      | token == treePathRequestToken expected -> do
          returned <- parseHeaderOid returnedRaw header
          objectType <- parseObjectTypeBytes typeRaw
          size <- parseDecimalSize "cat-file tree paths" sizeRaw
          Right (Just (GitObjectInfo returned objectType size))
    [expression, "missing"]
      | expression == treePathRequestExpression expected -> Right Nothing
    _ -> Left (GitInvalidOutput "cat-file tree paths" (GitMalformedObjectHeader (protocolSample header)))

treePathHeaderFields :: ByteString -> [ByteString]
treePathHeaderFields raw
  | BS.null raw || BS.any (\value -> value == 9 || value == 13 || value < 32 || value == 127) raw = []
  | otherwise = BS8.split ' ' raw

-- | A single logical object-info call owns at most one child, irrespective of
-- its bounded request-window count.
objectInfoBatchSessionCount :: [GitOid] -> Int
objectInfoBatchSessionCount = sessionCount . concat . canonicalObjectChunks

sessionCount :: [value] -> Int
sessionCount [] = 0
sessionCount _ = 1

-- | Known pre-observation child count: one initial bare-OID validation session,
-- one persistent custom-format tree-path session when a line-safe request is
-- present, and literal @ls-tree@ children only for the ineligible fallback.
-- The post-observation metadata session is accounted by
-- 'treePathRevisionProcessCount' only when a fallback entry actually exists.
treePathRevisionBatchRequestCount :: FilePath -> FilePath -> Map GitOid (Set.Set RepoPath) -> [GitOid] -> Either GitError Int
treePathRevisionBatchRequestCount executable commandDirectory requestedByRevision expectedBlobOids =
  (objectInfoBatchSessionCount (Map.keys requestedByRevision <> expectedBlobOids) + treePathBatchSessionCount requestedByRevision +)
    <$> treePathRevisionLsTreeChildCount executable commandDirectory requestedByRevision

treePathBatchSessionCount :: Map GitOid (Set.Set RepoPath) -> Int
treePathBatchSessionCount requestedByRevision = sessionCount (fst (partitionTreePathRequests requestedByRevision))

-- | Only the literal @ls-tree@ fallback portion of the plan. Classification
-- uses this distinct metric because line-safe paths now share one persistent
-- correlated session rather than one child per revision.
treePathRevisionLsTreeChildCount :: FilePath -> FilePath -> Map GitOid (Set.Set RepoPath) -> Either GitError Int
treePathRevisionLsTreeChildCount executable commandDirectory requestedByRevision =
  sum <$> traverse countOne (Map.toList fallbackRequested)
  where
    (_, fallbackRequested) = partitionTreePathRequests requestedByRevision
    countOne (revision, paths) =
      length <$> treePathArgumentWindowsForCommand executable commandDirectory revision (Set.toAscList paths)

-- | Actual process count after observation. The final bare-OID metadata
-- session exists only when at least one exact tree entry was returned.
treePathRevisionProcessCount :: FilePath -> FilePath -> Map GitOid (Set.Set RepoPath) -> [GitOid] -> Map GitOid (Map RepoPath (Maybe GitTreeEntry)) -> Either GitError Int
treePathRevisionProcessCount executable commandDirectory requestedByRevision expectedBlobOids observed =
  (+ objectInfoBatchSessionCount fallbackObjectOids)
    <$> treePathRevisionBatchRequestCount executable commandDirectory requestedByRevision expectedBlobOids
  where
    (_, fallbackRequested) = partitionTreePathRequests requestedByRevision
    fallbackObjectOids =
      [ gitTreeOid entry
      | (revision, entries) <- Map.toList observed,
        Just requestedPaths <- [Map.lookup revision fallbackRequested],
        (path, Just entry) <- Map.toList entries,
        Set.member path requestedPaths
      ]

validateCommitRevisions :: [GitOid] -> Map GitOid (Maybe GitObjectInfo) -> Either GitError ()
validateCommitRevisions revisions infos = traverse_ validate revisions
  where
    validate revision =
      case Map.lookup revision infos of
        Nothing -> Left (GitInvalidOutput "cat-file tree paths" (GitResponseCountMismatch (length revisions) (Map.size infos)))
        Just Nothing -> Left (GitObjectMissing revision)
        Just (Just info)
          | objectInfoType info == GitCommitObject -> Right ()
          | otherwise -> Left (GitObjectTypeMismatch revision GitCommitObject (objectInfoType info))

validateBlobObjects :: [GitOid] -> Map GitOid (Maybe GitObjectInfo) -> Either GitError ()
validateBlobObjects objectIds infos = traverse_ validate (Map.keys (Map.fromList [(objectId, ()) | objectId <- objectIds]))
  where
    validate objectId =
      case Map.lookup objectId infos of
        Nothing -> Left (GitInvalidOutput "cat-file tree paths" (GitResponseCountMismatch (length objectIds) (Map.size infos)))
        Just Nothing -> Left (GitObjectMissing objectId)
        Just (Just info)
          | objectInfoType info == GitBlobObject -> Right ()
          | otherwise -> Left (GitObjectTypeMismatch objectId GitBlobObject (objectInfoType info))

-- | Number of bounded @ls-tree@ requests made for a path set.  Keeping this
-- pure makes the no-per-member fan-out contract observable without turning
-- production Git process creation into a test seam.
treePathBatchRequestCount :: FilePath -> FilePath -> GitOid -> [RepoPath] -> Either GitError Int
treePathBatchRequestCount executable commandDirectory revision paths =
  length <$> treePathArgumentWindowsForCommand executable commandDirectory revision paths

-- Windows leaves headroom below CreateProcess's command-line limit.  Exact
-- paths remain argv values; this budget prevents an otherwise valid long path
-- set from turning the 256-item bound into an unbounded command line.
treePathArgumentByteLimit :: Int
treePathArgumentByteLimit = 30000

treePathArgumentWindows :: GitOid -> [RepoPath] -> Either GitError [[RepoPath]]
treePathArgumentWindows = treePathArgumentWindowsForCommand "git" ""

-- | Conservative upper-bound accounting for Windows argv serialization. Every
-- argument is charged as quoted and every UTF-16 code unit is charged twice,
-- covering quote/backslash escaping; fixed executable, @-C@ directory and all
-- literal-pathspec arguments are included before paths are admitted.
treePathArgumentWindowsForCommand :: FilePath -> FilePath -> GitOid -> [RepoPath] -> Either GitError [[RepoPath]]
treePathArgumentWindowsForCommand executable commandDirectory revision = go [] fixedBytes . Map.keys . Map.fromList . map (,())
  where
    fixedArguments =
      [ Text.pack executable,
        "-C",
        Text.pack commandDirectory,
        "--literal-pathspecs",
        "ls-tree",
        "-z",
        "--full-tree",
        gitOidText revision,
        "--"
      ]
    serializedBound arguments = max 0 (length arguments - 1) + sum (map argumentBound arguments)
    argumentBound argument = 2 + 2 * (BS.length (TextEncoding.encodeUtf16LE argument) `div` 2)
    fixedBytes = serializedBound fixedArguments
    go reversed _ [] = Right (reverse (if null reversed then [] else [reverse reversed]))
    go reversed currentBytes (path : remaining) =
      let pathBytes = 1 + argumentBound (repoPathText path)
       in if fixedBytes + pathBytes > treePathArgumentByteLimit
            then Left (GitInvalidOutput "lookup tree paths" (GitMalformedTreeRecord "exact path exceeds command-line byte budget"))
            else
              if length reversed >= objectWindowLimit || currentBytes + pathBytes > treePathArgumentByteLimit
                then do
                  later <- go [] fixedBytes (path : remaining)
                  Right (reverse reversed : later)
                else go (path : reversed) (currentBytes + pathBytes) remaining

decodeGitTreeOutput :: ByteString -> Either GitError [GitTreeEntry]
decodeGitTreeOutput raw
  | BS.null raw = Right []
  | BS.last raw /= 0 = Left (GitInvalidOutput "ls-tree" (GitMalformedTreeRecord (protocolSample raw)))
  | otherwise =
      let records = BS.split 0 raw
       in case reverse records of
            [] -> Right []
            finalRecord : reversedRecords
              | not (BS.null finalRecord) -> Left (GitInvalidOutput "ls-tree" (GitMalformedTreeRecord (protocolSample raw)))
              | any BS.null reversedRecords -> Left (GitInvalidOutput "ls-tree" (GitMalformedTreeRecord (protocolSample raw)))
              | otherwise -> traverse parseTreeRecord (reverse reversedRecords)

parseTreeRecord :: ByteString -> Either GitError GitTreeEntry
parseTreeRecord record =
  case BS.break (== 9) record of
    (metadata, rest)
      | BS.null rest -> Left (GitInvalidOutput "ls-tree" (GitMalformedTreeRecord (protocolSample record)))
      | otherwise -> do
          pathText <- first (const (GitInvalidUtf8Path (protocolSample (BS.drop 1 rest)))) (TextEncoding.decodeUtf8' (BS.drop 1 rest))
          path <- first (GitInvalidRepositoryPath pathText) (mkRepoPath pathText)
          if BS.any (> 127) metadata
            then Left (GitInvalidOutput "ls-tree" (GitMalformedTreeRecord (protocolSample record)))
            else case BS8.split ' ' metadata of
              [modeRaw, typeRaw, oidRaw]
                | all (not . BS.null) [modeRaw, typeRaw, oidRaw] -> do
                    let modeText = BS8.unpack modeRaw
                        typeText = Text.pack (BS8.unpack typeRaw)
                        oidText = Text.pack (BS8.unpack oidRaw)
                    objectType <- parseObjectTypeText typeText
                    mode <- parseFileMode modeText
                    oid <- first (const (GitInvalidOutput "ls-tree" (GitMalformedTreeRecord (protocolSample record)))) (mkGitOid oidText)
                    validateModeType path mode objectType
                    Right (GitTreeEntry path oid objectType mode)
              _ -> Left (GitInvalidOutput "ls-tree" (GitMalformedTreeRecord (protocolSample record)))

parseObjectTypeText :: Text -> Either GitError GitObjectType
parseObjectTypeText value =
  case value of
    "blob" -> Right GitBlobObject
    "tree" -> Right GitTreeObject
    "commit" -> Right GitCommitObject
    "tag" -> Right GitTagObject
    _ -> Left (GitInvalidOutput "object type" (GitMalformedObjectHeader (protocolSample (TextEncoding.encodeUtf8 value))))

parseObjectTypeBytes :: ByteString -> Either GitError GitObjectType
parseObjectTypeBytes raw
  | BS.any (> 127) raw = Left (GitInvalidOutput "object type" (GitMalformedObjectHeader (protocolSample raw)))
  | otherwise = parseObjectTypeText (Text.pack (BS8.unpack raw))

parseFileMode :: String -> Either GitError GitFileMode
parseFileMode value =
  case value of
    "100644" -> Right GitRegularFile
    "100755" -> Right GitExecutableFile
    "120000" -> Right GitSymbolicLink
    "160000" -> Right GitSubmodule
    "040000" -> Right GitDirectory
    _ -> Left (GitInvalidOutput "file mode" (GitMalformedTreeRecord (protocolSample (TextEncoding.encodeUtf8 (Text.pack value)))))

validateModeType :: RepoPath -> GitFileMode -> GitObjectType -> Either GitError ()
validateModeType path mode objectType =
  case (mode, objectType) of
    (GitRegularFile, GitBlobObject) -> Right ()
    (GitExecutableFile, GitBlobObject) -> Right ()
    (GitSymbolicLink, GitBlobObject) -> Right ()
    (GitSubmodule, GitCommitObject) -> Right ()
    (GitDirectory, GitTreeObject) -> Right ()
    _ -> Left (GitPathNotRegular path mode objectType)

readRegularBlobAt :: Repository -> GitOid -> RepoPath -> IO (Either GitError GitBlob)
readRegularBlobAt repository revision path = do
  entryResult <- lookupTreeEntryAt repository revision path
  case entryResult of
    Left problem -> pure (Left problem)
    Right Nothing -> pure (Left (GitPathMissing path))
    Right (Just entry)
      | gitTreeMode entry `notElem` [GitRegularFile, GitExecutableFile] ->
          pure (Left (GitPathNotRegular path (gitTreeMode entry) (gitTreeObjectType entry)))
      | otherwise -> do
          blobs <- readBlobBatch repository [gitTreeOid entry]
          pure $ do
            values <- blobs
            maybe (Left (GitObjectMissing (gitTreeOid entry))) Right (Map.lookup (gitTreeOid entry) values)

canonicalObjectChunks :: [GitOid] -> [[GitOid]]
canonicalObjectChunks = chunksOf objectWindowLimit . Map.keys . Map.fromList . map (,())

-- | Maximum number of object requests issued before a @cat-file@ protocol
-- exchange is drained.  Keeping this bounded prevents a large response from
-- blocking a subsequent request window on a full pipe.
objectWindowLimit :: Int
objectWindowLimit = 256

-- | Number of pipelined @cat-file@ requests needed for a de-duplicated
-- object set. Every request has at most 'objectWindowLimit' OIDs in flight.
objectBatchRequestCount :: [GitOid] -> Int
objectBatchRequestCount = length . canonicalObjectChunks

chunksOf :: Int -> [value] -> [[value]]
chunksOf _ [] = []
chunksOf size values = take size values : chunksOf size (drop size values)

batchObjectInfo :: Repository -> [GitOid] -> IO (Either GitError (Map GitOid (Maybe GitObjectInfo)))
batchObjectInfo _ [] = pure (Right Map.empty)
batchObjectInfo repository objectIds =
  withObjectInfoBatchSession repository $ \session ->
    readObjectInfoFromSession session objectIds

-- | The persistent bare-OID @cat-file --batch-check@ session. Tree-path
-- observations deliberately do not use this positional protocol.
newtype PipelineCleanupRegistry = PipelineCleanupRegistry (MVar (Int, Map Int (IO (Either IOException ()))))

newtype PipelineCleanupToken = PipelineCleanupToken Int

newPipelineCleanupRegistry :: IO PipelineCleanupRegistry
newPipelineCleanupRegistry = PipelineCleanupRegistry <$> newMVar (0, Map.empty)

-- | Register an active pipeline worker before its first unmasked wait.  The
-- enclosing pipe owner invokes this only after it has closed the concrete
-- parent handles and requested child termination, so an abandoned opposite
-- side can never keep error or cancellation cleanup joined indefinitely.
registerPipelineWorker :: PipelineCleanupRegistry -> Async.Async value -> IO PipelineCleanupToken
registerPipelineWorker (PipelineCleanupRegistry registry) worker =
  modifyMVar registry $ \(nextToken, active) -> do
    let token = PipelineCleanupToken nextToken
        dispose = disposeWorkerBounded worker
    pure ((nextToken + 1, Map.insert nextToken dispose active), token)

unregisterPipelineWorker :: PipelineCleanupRegistry -> PipelineCleanupToken -> IO ()
unregisterPipelineWorker (PipelineCleanupRegistry registry) (PipelineCleanupToken token) =
  modifyMVar registry $ \(nextToken, active) -> pure ((nextToken, Map.delete token active), ())

cleanupPipelineWorkers :: PipelineCleanupRegistry -> IO (Either IOException ())
cleanupPipelineWorkers (PipelineCleanupRegistry registry) = do
  (_, active) <- readMVar registry
  foldM
    (\result dispose -> do
      disposed <- dispose
      pure (result >> disposed)
    )
    (Right ())
    (Map.elems active)

disposeWorkerBounded :: Async.Async value -> IO (Either IOException ())
disposeWorkerBounded worker = do
  existing <- Async.poll worker
  case existing of
    Just (Left problem) -> pure (Left (userError (show problem)))
    Just (Right _) -> pure (Right ())
    Nothing -> do
      _ <- forkIO (void (Async.cancel worker))
      observed <- pollAsyncWithin readerCleanupMicros worker
      pure $ case observed of
        Nothing -> Left (userError "bounded Git worker cleanup timed out")
        Just (Left problem)
          | Just Async.AsyncCancelled <- fromException problem -> Right ()
          | otherwise -> Left (userError (show problem))
        Just (Right _) -> Right ()

data GitBatchCheckSession = GitBatchCheckSession Handle Handle PipelineCleanupRegistry

withObjectInfoBatchSession :: Repository -> (GitBatchCheckSession -> IO (Either GitError value)) -> IO (Either GitError value)
withObjectInfoBatchSession repository interaction =
  withGitPipes repository "cat-file batch-check" ["cat-file", "--batch-check"] $ \stdinHandle stdoutHandle registry -> do
    outcome <- interaction (GitBatchCheckSession stdinHandle stdoutHandle registry)
    case outcome of
      Left problem -> pure (Left problem)
      Right value -> finishObjectInfoBatchInput stdinHandle stdoutHandle value

readObjectInfoFromSession :: GitBatchCheckSession -> [GitOid] -> IO (Either GitError (Map GitOid (Maybe GitObjectInfo)))
readObjectInfoFromSession session objectIds = go (canonicalObjectChunks objectIds) Map.empty
  where
    go [] accumulated = pure (Right accumulated)
    go (requested : remaining) accumulated = do
      current <- readObjectInfoWindowFromSession session requested accumulated
      case current of
        Left problem -> pure (Left problem)
        Right next -> go remaining next

readObjectInfoWindowFromSession :: GitBatchCheckSession -> [GitOid] -> Map GitOid (Maybe GitObjectInfo) -> IO (Either GitError (Map GitOid (Maybe GitObjectInfo)))
readObjectInfoWindowFromSession _ requested _ | length requested > objectWindowLimit =
  pure (Left (objectWindowLimitError "cat-file batch-check"))
readObjectInfoWindowFromSession (GitBatchCheckSession stdinHandle stdoutHandle registry) requested accumulated =
  pipelineObjectBatch registry "cat-file batch-check" stdinHandle requested (readInfoResponses stdoutHandle requested accumulated)

readInfoResponses :: Handle -> [GitOid] -> Map GitOid (Maybe GitObjectInfo) -> IO (Either GitError (Map GitOid (Maybe GitObjectInfo)))
readInfoResponses stdoutHandle requested accumulated =
  case requested of
    [] -> pure (Right accumulated)
    expected : remaining -> do
      response <- readBatchHeader stdoutHandle
      case response of
        Left problem -> pure (Left problem)
        Right header ->
          case decodeGitObjectInfoHeader expected header of
            Left problem -> pure (Left problem)
            Right pair -> readInfoResponses stdoutHandle remaining (uncurry Map.insert pair accumulated)

decodeGitObjectInfoHeader :: GitOid -> ByteString -> Either GitError (GitOid, Maybe GitObjectInfo)
decodeGitObjectInfoHeader expected line = do
  fields <- exactAsciiFields "cat-file batch-check" line
  case fields of
    [returnedRaw, "missing"] -> do
      returned <- parseHeaderOid returnedRaw line
      checkReturned expected returned
      Right (expected, Nothing)
    [returnedRaw, typeRaw, sizeRaw] -> do
      returned <- parseHeaderOid returnedRaw line
      checkReturned expected returned
      objectType <- parseObjectTypeBytes typeRaw
      size <- parseDecimalSize "cat-file batch-check" sizeRaw
      Right (expected, Just (GitObjectInfo returned objectType size))
    _ -> Left (GitInvalidOutput "cat-file batch-check" (GitMalformedObjectHeader (protocolSample line)))

foldBlobBatch :: Repository -> [GitOid] -> accumulator -> (accumulator -> GitBlob -> IO accumulator) -> IO (Either GitError accumulator)
foldBlobBatch _ [] initial _ = pure (Right initial)
foldBlobBatch repository objectIds initial step =
  withBlobBatchSession repository (\session -> foldBlobBatchFromSession session objectIds initial step)

readBlobBatch :: Repository -> [GitOid] -> IO (Either GitError (Map GitOid GitBlob))
readBlobBatch repository objectIds =
  foldBlobBatch repository objectIds Map.empty (\values blob -> pure (Map.insert (gitBlobOid blob) blob values))

-- | Read an object set through one buffered @cat-file@ session.  Its request
-- plan is still split into finite windows of at most 'objectWindowLimit'
-- de-duplicated OIDs.
readBlobBatchOneSession :: Repository -> [GitOid] -> IO (Either GitError (Map GitOid GitBlob))
readBlobBatchOneSession _ [] = pure (Right Map.empty)
readBlobBatchOneSession repository objectIds =
  withBlobBatchSession repository (\session -> readBlobBatchFromSession session objectIds)

-- | A caller-scoped @git cat-file --batch@ child.  The
-- constructor remains opaque so requests can only be issued in bounded,
-- drained windows through 'readBlobBatchFromSession'.
data BlobBatchSessionState
  = BlobBatchSessionOpen
  | BlobBatchSessionFailed !GitError
  | BlobBatchSessionClosed !(Maybe GitError)

-- The rank-2 scope is created by 'withBlobBatchSession'.  All folds hold one
-- gate for their complete bounded request plan, so readers and writers never
-- interleave between windows.
data GitBlobBatchSession scope = GitBlobBatchSession Handle Handle PipelineCleanupRegistry (TVar BlobBatchSessionState) (TMVar ()) (IO ())

-- | The native input actions used by one persistent @cat-file --batch@
-- window.  Keeping this boundary explicit makes the one-write/one-flush
-- framing contract independently observable without exposing session handles.
data GitBatchInput = GitBatchInput
  { gitBatchInputWrite :: ByteString -> IO (),
    gitBatchInputFlush :: IO ()
  }

-- | Keep one @cat-file --batch@ child alive for the callback.
-- Each successful callback closes stdin and proves stdout EOF before accepting
-- the child; 'withGitPipes' cancels and reaps it on every error or exception.
withBlobBatchSession :: Repository -> (forall scope. GitBlobBatchSession scope -> IO (Either GitError value)) -> IO (Either GitError value)
withBlobBatchSession repository = withBlobBatchSessionWithWindowObserver repository (pure ())

-- | Production folds invoke this only after a bounded window has fully drained
-- and before a subsequent request is emitted.  The ordinary API uses a no-op;
-- the shared seam makes cross-window ownership directly observable in tests.
withBlobBatchSessionWithWindowObserver :: Repository -> IO () -> (forall scope. GitBlobBatchSession scope -> IO (Either GitError value)) -> IO (Either GitError value)
withBlobBatchSessionWithWindowObserver repository afterWindow = withBlobBatchSessionWithObservers repository afterWindow (pure ())

-- | Test-only finalization seam.  Production uses the no-op hook above; the
-- hook is restored only after the callback has returned, while its exception
-- path commits Closed under the surrounding mask before rethrowing.
withBlobBatchSessionWithFinalizationObserverForTest :: Repository -> IO () -> IO () -> (forall scope. GitBlobBatchSession scope -> IO (Either GitError value)) -> IO (Either GitError value)
withBlobBatchSessionWithFinalizationObserverForTest = withBlobBatchSessionWithObservers

withBlobBatchSessionWithObservers :: Repository -> IO () -> IO () -> (forall scope. GitBlobBatchSession scope -> IO (Either GitError value)) -> IO (Either GitError value)
withBlobBatchSessionWithObservers repository afterWindow afterCallback interaction =
  withGitPipes repository "cat-file batch" ["cat-file", "--batch"] $ \stdinHandle stdoutHandle registry -> do
    -- The callback itself remains interruptible, but every transition from it
    -- into finalization is masked.  An escaped closure can therefore never
    -- observe Open after its owner has returned or raised.
    mask $ \restore -> do
      state <- newTVarIO BlobBatchSessionOpen
      operationGate <- newTMVarIO ()
      let session = GitBlobBatchSession stdinHandle stdoutHandle registry state operationGate afterWindow
      attempted <- try @SomeException (restore (interaction session))
      outcome <- case attempted of
        Left problem -> closeBlobBatchSessionAfterException state >> throwIO problem
        Right value -> pure value
      hook <- try @SomeException (restore afterCallback)
      case hook of
        Left problem -> closeBlobBatchSessionAfterException state >> throwIO problem
        Right () -> case outcome of
          Left problem -> closeBlobBatchSessionAfterFailure state problem
          Right value -> closeBlobBatchSession session value

-- | Callback layering must not replace a prior protocol error with a later
-- outer error.  Closing here also makes a captured closure fail without ever
-- returning to the child after its owner has begun cancellation.
closeBlobBatchSessionAfterFailure :: TVar BlobBatchSessionState -> GitError -> IO (Either GitError value)
closeBlobBatchSessionAfterFailure state problem = do
  closed <- atomically (closeBlobBatchSessionState state (Just problem))
  pure (either Left (const (Left problem)) closed)

-- | An exception must not leave an escaped closure observing an open session
-- while its owning pipe scope is already being torn down.
closeBlobBatchSessionAfterException :: TVar BlobBatchSessionState -> IO ()
closeBlobBatchSessionAfterException state = void (atomically (closeBlobBatchSessionState state Nothing))

closeBlobBatchSession :: GitBlobBatchSession scope -> value -> IO (Either GitError value)
closeBlobBatchSession (GitBlobBatchSession stdinHandle stdoutHandle registry state operationGate _) value = mask $ \restore -> do
  -- Close the lifecycle before the interruptible EOF proof.  In particular,
  -- this must not wait for a captured fold which is blocked in a pipe read:
  -- returning the outer Left lets withGitPipes own cancellation and reaping.
  shouldFinish <- atomically $ do
    current <- readTVar state
    case current of
      BlobBatchSessionOpen -> do
        activeOperation <- isEmptyTMVar operationGate
        writeTVar state (BlobBatchSessionClosed Nothing)
        pure (if activeOperation then Left GitBlobBatchSessionClosed else Right ())
      BlobBatchSessionFailed problem -> do
        writeTVar state (BlobBatchSessionClosed (Just problem))
        pure (Left problem)
      BlobBatchSessionClosed (Just problem) -> pure (Left problem)
      BlobBatchSessionClosed Nothing -> pure (Left GitBlobBatchSessionClosed)
  case shouldFinish of
    Left problem -> pure (Left problem)
    Right () -> do
      -- EOF proof may block forever on a malicious child.  Register it before
      -- the first interruptible wait so owner cancellation transfers cleanup
      -- to withGitPipes, which remains the sole handle/process owner.
      finisher <- Async.async (finishBatchInput stdinHandle stdoutHandle value)
      finisherToken <- registerPipelineWorker registry finisher
      completed <- restore (Async.wait finisher)
      unregisterPipelineWorker registry finisherToken
      case completed of
        Right result -> pure (Right result)
        Left problem -> do
          firstProblem <- atomically (recordBlobBatchCloseFailure state problem)
          pure (Left firstProblem)

-- | Owner-side state change.  It is deliberately independent of the operation
-- gate, which can remain held by a blocked escaped action while the owner must
-- return so 'withGitPipes' can cancel and reap its child.
closeBlobBatchSessionState :: TVar BlobBatchSessionState -> Maybe GitError -> STM (Either GitError ())
closeBlobBatchSessionState state requested = do
  current <- readTVar state
  case current of
    BlobBatchSessionOpen -> do
      writeTVar state (BlobBatchSessionClosed requested)
      pure (maybe (Right ()) Left requested)
    BlobBatchSessionFailed problem -> do
      writeTVar state (BlobBatchSessionClosed (Just problem))
      pure (Left problem)
    BlobBatchSessionClosed (Just problem) -> pure (Left problem)
    BlobBatchSessionClosed Nothing -> pure (Left GitBlobBatchSessionClosed)

recordBlobBatchCloseFailure :: TVar BlobBatchSessionState -> GitError -> STM GitError
recordBlobBatchCloseFailure state problem = do
  current <- readTVar state
  case current of
    BlobBatchSessionClosed Nothing -> writeTVar state (BlobBatchSessionClosed (Just problem)) >> pure problem
    BlobBatchSessionClosed (Just firstProblem) -> pure firstProblem
    BlobBatchSessionFailed firstProblem -> writeTVar state (BlobBatchSessionClosed (Just firstProblem)) >> pure firstProblem
    BlobBatchSessionOpen -> writeTVar state (BlobBatchSessionClosed (Just problem)) >> pure problem

withOpenBlobBatchSession :: GitBlobBatchSession scope -> IO (Either GitError value) -> IO (Either GitError value)
withOpenBlobBatchSession (GitBlobBatchSession _ _ _ state operationGate _) action = mask $ \restore -> do
  acquired <- atomically (acquireBlobBatchOperation state operationGate)
  case acquired of
    Left problem -> pure (Left problem)
    Right () -> do
      attempted <- try @SomeException (restore action)
      case attempted of
        Left problem -> do
          -- This does not wait for any other operation and cannot restore Open.
          void (atomically (closeBlobBatchSessionState state Nothing))
          atomically (void (tryPutTMVar operationGate ()))
          throwIO problem
        Right result ->
          atomically $ do
            completed <- completeBlobBatchOperation state result
            void (tryPutTMVar operationGate ())
            pure completed

acquireBlobBatchOperation :: TVar BlobBatchSessionState -> TMVar () -> STM (Either GitError ())
acquireBlobBatchOperation state operationGate = do
  current <- readTVar state
  case current of
    BlobBatchSessionFailed problem -> pure (Left problem)
    BlobBatchSessionClosed (Just problem) -> pure (Left problem)
    BlobBatchSessionClosed Nothing -> pure (Left GitBlobBatchSessionClosed)
    BlobBatchSessionOpen -> do
      acquired <- tryTakeTMVar operationGate
      case acquired of
        Just () -> pure (Right ())
        Nothing -> retry

completeBlobBatchOperation :: TVar BlobBatchSessionState -> Either GitError value -> STM (Either GitError value)
completeBlobBatchOperation state result = do
  current <- readTVar state
  case current of
    BlobBatchSessionOpen -> case result of
      Left problem -> writeTVar state (BlobBatchSessionFailed problem) >> pure (Left problem)
      Right value -> pure (Right value)
    BlobBatchSessionFailed problem -> pure (Left problem)
    BlobBatchSessionClosed (Just problem) -> pure (Left problem)
    BlobBatchSessionClosed Nothing -> pure (Left GitBlobBatchSessionClosed)

-- | Fold a canonical object set through a caller-owned session.  The object
-- plan remains split into fully-drained windows, while the process remains
-- alive across those windows.  Keeping the session opaque prevents callers
-- from writing an unbounded request plan or retaining its handles.
foldBlobBatchFromSession :: GitBlobBatchSession scope -> [GitOid] -> accumulator -> (accumulator -> GitBlob -> IO accumulator) -> IO (Either GitError accumulator)
foldBlobBatchFromSession session@(GitBlobBatchSession stdinHandle stdoutHandle registry _ _ afterWindow) objectIds initial step =
  withOpenBlobBatchSession session $
    streamBlobResponseChunksOpen afterWindow registry stdinHandle stdoutHandle (canonicalObjectChunks objectIds) initial step

-- | Read one object set through the caller-owned persistent session.  A
-- window is canonicalized exactly as the finite API is, so it contains at
-- most 'objectWindowLimit' OIDs and is drained before the next request/flush
-- is emitted.
readBlobBatchFromSession :: GitBlobBatchSession scope -> [GitOid] -> IO (Either GitError (Map GitOid GitBlob))
readBlobBatchFromSession session objectIds =
  foldBlobBatchFromSession session objectIds Map.empty (\values blob -> pure (Map.insert (gitBlobOid blob) blob values))

-- | Stream blob payloads for the given object ids in request order (no
-- sorting or de-duplication), invoking the step once per blob as it arrives.
-- The step folds each blob incrementally and may release it before the rest
-- arrive, so callers can avoid materializing whole-corpus blob maps.
foldBlobBatchInOrder :: Repository -> [GitOid] -> accumulator -> (accumulator -> GitBlob -> IO accumulator) -> IO (Either GitError accumulator)
foldBlobBatchInOrder _ [] initial _ = pure (Right initial)
foldBlobBatchInOrder repository objectIds initial step =
  withBlobBatchSession repository (\session -> foldBlobBatchInOrderFromSession session objectIds initial step)

-- | Ordered counterpart to 'foldBlobBatchFromSession'.  It deliberately does
-- not sort or de-duplicate requests: each response is consumed in the exact
-- caller sequence, including duplicates on opposite sides of a window
-- boundary.
foldBlobBatchInOrderFromSession :: GitBlobBatchSession scope -> [GitOid] -> accumulator -> (accumulator -> GitBlob -> IO accumulator) -> IO (Either GitError accumulator)
foldBlobBatchInOrderFromSession session@(GitBlobBatchSession stdinHandle stdoutHandle registry _ _ afterWindow) objectIds initial step =
  withOpenBlobBatchSession session $
    streamBlobResponseChunksOpen afterWindow registry stdinHandle stdoutHandle (chunksOf objectWindowLimit objectIds) initial step

-- | Variant for a caller-owned persistent batch session.  An empty request
-- list leaves stdin open so another bounded request can follow on the same
-- process.
streamBlobResponsesOpen :: PipelineCleanupRegistry -> Handle -> Handle -> [GitOid] -> accumulator -> (accumulator -> GitBlob -> IO accumulator) -> IO (Either GitError accumulator)
streamBlobResponsesOpen registry stdinHandle stdoutHandle requested accumulated step
  | length requested > objectWindowLimit = pure (Left (objectWindowLimitError "cat-file batch"))
  | otherwise =
  pipelineObjectBatch registry "cat-file batch" stdinHandle requested (readBlobResponses stdoutHandle requested accumulated step)

-- | Keep one @cat-file --batch@ request window in flight while its responses
-- are read.
-- Writing and reading run together because a large blob can fill a Windows
-- stdout pipe before the child has consumed the entire request batch. The
-- request writer emits one bare-OID sequence, then flushes the handle once.
-- Callers only invoke it with a chunk produced by 'canonicalObjectChunks' (at
-- most 'objectWindowLimit' OIDs).
pipelineObjectBatch :: PipelineCleanupRegistry -> Text -> Handle -> [GitOid] -> IO (Either GitError value) -> IO (Either GitError value)
pipelineObjectBatch _ _ _ [] readResponses = readResponses
pipelineObjectBatch registry operation stdinHandle requested readResponses =
  pipelineBatchLines
    registry
    operation
    (handleBatchInput stdinHandle)
    (map objectRequestLine requested)
    readResponses

-- | Write one finite request window and drain its matching replies before a
-- later window is permitted.  The concrete framing is injected so tree-path
-- requests cannot accidentally use bare object OID framing.  A caller-scoped
-- session always leaves stdin open here; its bracket owns the final EOF/trailing
-- response proof and process reaping.
pipelineBatchLines :: PipelineCleanupRegistry -> Text -> GitBatchInput -> [ByteString] -> IO (Either GitError value) -> IO (Either GitError value)
pipelineBatchLines _ _ _ [] readResponses = readResponses
pipelineBatchLines registry operation input requestLines readResponses = mask $ \restore -> do
  writer <- Async.async (writePersistentBatchLines operation input requestLines)
  writerToken <- registerPipelineWorker registry writer
  reader <- Async.async readResponses
  readerToken <- registerPipelineWorker registry reader
  let unregisterBoth = do
        unregisterPipelineWorker registry writerToken
        unregisterPipelineWorker registry readerToken
      complete value = unregisterBoth >> pure value
  restore (Async.waitEither writer reader) >>= \case
    Left sent ->
      case sent of
        Left problem -> pure (Left problem)
        Right () -> restore (Async.wait reader) >>= complete
    Right received ->
      case received of
        Left problem -> pure (Left problem)
        Right value -> do
          sent <- restore (Async.wait writer)
          complete (value <$ sent)

-- | Emit one bounded plain @--batch@ request plan as one write/flush. Stdin
-- remains open so a later window can use the same child. The 'GitBatchInput'
-- argument is deliberately a narrow native-I/O seam for framing tests.
writePersistentBatchRequests :: GitBatchInput -> [GitOid] -> IO (Either GitError ())
writePersistentBatchRequests input requested =
  writePersistentBatchLines "cat-file batch" input (map objectRequestLine requested)

writePersistentBatchLines :: Text -> GitBatchInput -> [ByteString] -> IO (Either GitError ())
writePersistentBatchLines _ _ [] = pure (Right ())
writePersistentBatchLines operation input requestLines = do
  attempted <- try @IOException $ do
    gitBatchInputWrite input (BS.concat requestLines)
    gitBatchInputFlush input
  pure $
    case attempted of
      Left problem -> Left (gitBatchIoErrorFor operation problem)
      Right () -> Right ()

handleBatchInput :: Handle -> GitBatchInput
handleBatchInput handle = GitBatchInput (BS.hPut handle) (hFlush handle)

objectRequestLine :: GitOid -> ByteString
objectRequestLine = (<> "\n") . TextEncoding.encodeUtf8 . gitOidText

readBatchHeader :: Handle -> IO (Either GitError ByteString)
readBatchHeader stdoutHandle = do
  attempted <- try @IOException (readProtocolLine stdoutHandle)
  pure $
    case attempted of
      Left problem -> Left (gitBatchIoError problem)
      Right header -> Right header

gitBatchIoError :: IOException -> GitError
gitBatchIoError = gitBatchIoErrorFor "cat-file batch"

gitBatchIoErrorFor :: Text -> IOException -> GitError
gitBatchIoErrorFor operation problem =
  GitInvalidOutput
    operation
    (GitMalformedObjectHeader (protocolSample (TextEncoding.encodeUtf8 (Text.pack (ioeGetErrorString problem)))))

readBlobResponses :: Handle -> [GitOid] -> accumulator -> (accumulator -> GitBlob -> IO accumulator) -> IO (Either GitError accumulator)
readBlobResponses stdoutHandle requested accumulated step =
  case requested of
    [] -> pure (Right accumulated)
    expected : remaining -> do
      response <- readBatchHeader stdoutHandle
      case response of
        Left problem -> pure (Left problem)
        Right header ->
          case exactAsciiFields "cat-file batch" header of
            Left problem -> pure (Left problem)
            Right fields ->
              case fields of
                [returnedRaw, "missing"] ->
                  pure $ do
                    returned <- parseHeaderOid returnedRaw header
                    checkReturned expected returned
                    Left (GitObjectMissing expected)
                [returnedRaw, typeRaw, sizeRaw] ->
                  case decodeGitBlobHeader expected header returnedRaw typeRaw sizeRaw of
                    Left problem -> pure (Left problem)
                    Right size -> do
                      payloadRead <- try @IOException $ do
                        payload <- BS.hGet stdoutHandle (fromIntegral size)
                        framing <- BS.hGet stdoutHandle 1
                        pure (payload, framing)
                      case payloadRead of
                        Left problem -> pure (Left (gitBatchIoError problem))
                        Right (payload, framing) ->
                          case decodeGitBlobPayload expected size payload framing of
                            Left problem -> pure (Left problem)
                            Right blob -> do
                              next <- step accumulated blob
                              readBlobResponses stdoutHandle remaining next step
                _ -> pure (Left (GitInvalidOutput "cat-file batch" (GitMalformedObjectHeader (protocolSample header))))

-- | A finite @cat-file --batch@ process closes after each bounded request
-- plan has been fully consumed. A persistent session keeps stdin open while
-- still enforcing the same per-plan in-flight limit.
streamBlobResponseChunksOpen :: IO () -> PipelineCleanupRegistry -> Handle -> Handle -> [[GitOid]] -> accumulator -> (accumulator -> GitBlob -> IO accumulator) -> IO (Either GitError accumulator)
streamBlobResponseChunksOpen afterWindow registry stdinHandle stdoutHandle chunks accumulated step =
  case chunks of
    [] -> pure (Right accumulated)
    chunk : remaining -> do
      chunkResult <- streamBlobResponsesOpen registry stdinHandle stdoutHandle chunk accumulated step
      case chunkResult of
        Left problem -> pure (Left problem)
        Right next -> case remaining of
          [] -> pure (Right next)
          _ -> afterWindow >> streamBlobResponseChunksOpen afterWindow registry stdinHandle stdoutHandle remaining next step

decodeGitBlobHeader :: GitOid -> ByteString -> ByteString -> ByteString -> ByteString -> Either GitError Word64
decodeGitBlobHeader expected header returnedRaw typeRaw sizeRaw = do
  returned <- parseHeaderOid returnedRaw header
  checkReturned expected returned
  objectType <- parseObjectTypeBytes typeRaw
  if objectType /= GitBlobObject
    then Left (GitObjectTypeMismatch expected GitBlobObject objectType)
    else do
      size <- parseDecimalSize "cat-file batch" sizeRaw
      if size > fromIntegral (maxBound :: Int)
        then Left (GitObjectTooLargeForPlatform expected size)
        else Right size

decodeGitBlobPayload :: GitOid -> Word64 -> ByteString -> ByteString -> Either GitError GitBlob
decodeGitBlobPayload expected size payload framing
  | BS.length payload /= fromIntegral size =
      Left (GitInvalidOutput "cat-file batch" (GitTruncatedObject expected size (BS.length payload)))
  | framing /= "\n" = Left (GitInvalidOutput "cat-file batch" (GitMissingObjectFraming expected))
  | otherwise = Right (GitBlob expected payload)

objectWindowLimitError :: Text -> GitError
objectWindowLimitError operation =
  GitInvalidOutput operation (GitMalformedObjectHeader "cat-file window exceeds object-window limit")

parseDecimalSize :: Text -> ByteString -> Either GitError Word64
parseDecimalSize operation raw
  | BS.null raw || BS.any (\value -> value < 48 || value > 57) raw = invalid
  | otherwise =
      case readMaybe (BS8.unpack raw) :: Maybe Integer of
        Just value
          | value <= toInteger (maxBound :: Word64) -> Right (fromInteger value)
        _ -> invalid
  where
    invalid = Left (GitInvalidOutput operation (GitInvalidObjectSize (protocolSample raw)))

exactAsciiFields :: Text -> ByteString -> Either GitError [ByteString]
exactAsciiFields operation raw
  | BS.null raw || BS.any (> 127) raw = malformed
  | otherwise =
      let fields = BS8.split ' ' raw
       in if any BS.null fields || any (BS.any (`elem` [9, 10, 13])) fields
            then malformed
            else Right fields
  where
    malformed = Left (GitInvalidOutput operation (GitMalformedObjectHeader (protocolSample raw)))

readProtocolLine :: Handle -> IO ByteString
readProtocolLine handle = readProtocolLineBounded handle 512

readProtocolLineBounded :: Handle -> Int -> IO ByteString
readProtocolLineBounded handle limit = go BS.empty
  where
    go accumulated
      | BS.length accumulated > limit = ioError (userError "Git protocol header exceeds bounded response limit")
      | otherwise = do
          next <- BS.hGet handle 1
          case BS.uncons next of
            Nothing -> ioError (userError "Git protocol header ended before LF")
            Just (10, _) -> pure accumulated
            Just (value, _) -> go (BS.snoc accumulated value)

protocolSample :: ByteString -> ByteString
protocolSample = BS.take 4096

finishBatchInput :: Handle -> Handle -> value -> IO (Either GitError value)
finishBatchInput stdinHandle stdoutHandle value = do
  attempted <- try @IOException $ do
    hClose stdinHandle
    BS.hGet stdoutHandle 129
  pure $
    case attempted of
      Left problem ->
        Left
          ( GitInvalidOutput
              "cat-file batch"
              (GitMalformedObjectHeader (protocolSample (TextEncoding.encodeUtf8 (Text.pack (ioeGetErrorString problem)))))
          )
      Right trailing -> validateGitBatchTrailing trailing >> Right value

-- | The persistent object-info protocol has the same header-only framing as
-- @--batch@, but keeps its own operation label so malformed EOF/trailing bytes
-- cannot be mistaken for a blob-payload failure.
finishObjectInfoBatchInput :: Handle -> Handle -> value -> IO (Either GitError value)
finishObjectInfoBatchInput = finishObjectInfoBatchInputFor "cat-file batch-check"

finishObjectInfoBatchInputFor :: Text -> Handle -> Handle -> value -> IO (Either GitError value)
finishObjectInfoBatchInputFor operation stdinHandle stdoutHandle value = do
  attempted <- try @IOException $ do
    hClose stdinHandle
    BS.hGet stdoutHandle 129
  pure $
    case attempted of
      Left problem -> Left (gitBatchIoErrorFor operation problem)
      Right trailing ->
        if BS.null trailing
          then Right value
          else Left (GitInvalidOutput operation (GitUnexpectedTrailingBytes (protocolSample trailing)))

validateGitBatchTrailing :: ByteString -> Either GitError ()
validateGitBatchTrailing trailing
  | BS.null trailing = Right ()
  | otherwise = Left (GitInvalidOutput "cat-file batch" (GitUnexpectedTrailingBytes (protocolSample trailing)))

validateObjectInfoBatchTrailing :: ByteString -> Either GitError ()
validateObjectInfoBatchTrailing trailing
  | BS.null trailing = Right ()
  | otherwise = Left (GitInvalidOutput "cat-file batch-check" (GitUnexpectedTrailingBytes (protocolSample trailing)))

withGitPipes :: Repository -> Text -> [String] -> (Handle -> Handle -> PipelineCleanupRegistry -> IO (Either GitError value)) -> IO (Either GitError value)
withGitPipes repository operation arguments interaction = mask $ \restore -> do
  let GitClient executable = repositoryClient repository
  spawned <- try @IOException (spawnGitProcess executable (repositoryCommandDirectory repository) Nothing arguments)
  case spawned of
    Left _ -> pure (Left (GitExecutableUnavailable executable))
    Right (stdinHandle, stdoutHandle, stderrHandle, stableHandle) -> do
      registry <- newPipelineCleanupRegistry
      processWorker <- Async.async (Process.waitForProcess stableHandle)
      processToken <- registerPipelineWorker registry processWorker
      stderrWorker <- Async.async (drainBounded stderrHandle)
      stderrToken <- registerPipelineWorker registry stderrWorker
      let cleanup = do
            -- Closing all parent handles happens before any bounded worker
            -- observation, so inherited child pipes cannot hold a cleanup
            -- join indefinitely.
            beginCloseQuietly stdinHandle
            beginCloseQuietly stdoutHandle
            beginCloseQuietly stderrHandle
            stopped <- terminateCapturedProcess stableHandle
            workers <- cleanupPipelineWorkers registry
            pure (stopped >> workers)
          ioFailure problem =
            GitCommandFailed
              operation
              (-1)
              ""
              (boundedDiagnostic (TextEncoding.encodeUtf8 (Text.pack (ioeGetErrorString problem))) )
          finishSuccess value = do
            closeQuietly stdinHandle
            normal <- try @IOException (Async.wait processWorker)
            case normal of
              Left problem -> do
                cleaned <- cleanup
                pure (Left (either ioFailure (const (ioFailure problem)) cleaned))
              Right exitCode -> do
                stderrResult <- try @IOException (Async.wait stderrWorker)
                unregisterPipelineWorker registry processToken
                unregisterPipelineWorker registry stderrToken
                closeQuietly stdoutHandle
                closeQuietly stderrHandle
                cleaned <- cleanup
                pure $
                  case cleaned of
                    Left problem -> Left (ioFailure problem)
                    Right () ->
                      case stderrResult of
                        Left problem -> Left (ioFailure problem)
                        Right stderrBytes ->
                          if exitCode == ExitSuccess
                            then Right value
                            else Left (GitCommandFailed operation (exitCodeNumber exitCode) "" (boundedDiagnostic stderrBytes))
      attempted <- try @SomeException (restore (interaction stdinHandle stdoutHandle registry))
      case attempted of
        Left original -> do
          cleaned <- cleanup
          case fromException original :: Maybe SomeAsyncException of
            Just _ -> throwIO original
            Nothing ->
              case cleaned of
                Right () -> throwIO original
                Left problem -> pure (Left (ioFailure (userError (show original <> "; cleanup: " <> ioeGetErrorString problem))))
        Right (Left problem) -> do
          cleaned <- cleanup
          pure (either (Left . ioFailure) (const (Left problem)) cleaned)
        Right (Right value) -> do
          -- The post-interaction EOF/process proof can still block when a
          -- child retains inherited pipes.  Keep it restored and caught just
          -- like the callback so async cancellation always enters cleanup.
          completed <- try @SomeException (restore (finishSuccess value))
          case completed of
            Left original -> do
              cleaned <- cleanup
              case fromException original :: Maybe SomeAsyncException of
                Just _ -> throwIO original
                Nothing -> pure (Left (either ioFailure (const (ioFailure (userError (show original)))) cleaned))
            Right result -> pure result

drainBounded :: Handle -> IO ByteString
drainBounded handle = go BS.empty
  where
    go retained = do
      chunk <- BS.hGetSome handle 4096
      if BS.null chunk
        then pure retained
        else go (BS.take diagnosticLimit (retained <> chunk))

readHandleAll :: Handle -> IO ByteString
readHandleAll handle = go []
  where
    go reversedChunks = do
      chunk <- BS.hGetSome handle 32768
      if BS.null chunk
        then pure (BS.concat (reverse reversedChunks))
        else go (chunk : reversedChunks)

closeQuietly :: Handle -> IO ()
closeQuietly handle = do
  _ <- try @IOException (hClose handle)
  pure ()

-- | Request close without joining a reader that may currently own the handle's
-- I/O lock.  Cleanup only observes reader completion through its finite policy.
beginCloseQuietly :: Handle -> IO ()
beginCloseQuietly handle = do
  _ <- Async.async (closeQuietly handle)
  pure ()

-- | A disposable helper delivers cancellation after parent handles are closed.
-- The owner never joins this helper or the reader it nudges; it observes the
-- reader only through 'awaitReaderWorkersBounded'.
nudgeReaderWorkers :: [Async.Async ByteString] -> IO ()
nudgeReaderWorkers = mapM_ nudge
  where
    nudge worker = do
      _ <- forkIO (void (Async.cancel worker))
      pure ()


awaitReaderWorkersBounded :: [Async.Async ByteString] -> IO (Either IOException ())
awaitReaderWorkersBounded = go
  where
    go [] = pure (Right ())
    go (worker : remaining) = do
      observed <- pollAsyncWithin readerCleanupMicros worker
      case observed of
        Nothing -> pure (Left (userError "bounded Git reader cleanup timed out"))
        Just (Left problem) -> pure (Left (userError (show problem)))
        Just (Right _) -> go remaining

parseHeaderOid :: ByteString -> ByteString -> Either GitError GitOid
parseHeaderOid raw context =
  if BS.any (> 127) raw
    then Left malformed
    else first (const malformed) (mkGitOid (Text.pack (BS8.unpack raw)))
  where
    malformed = GitInvalidOutput "cat-file" (GitMalformedObjectHeader (protocolSample context))

checkReturned :: GitOid -> GitOid -> Either GitError ()
checkReturned expected actual
  | expected == actual = Right ()
  | otherwise = Left (GitInvalidOutput "cat-file" (GitReturnedObjectMismatch expected actual))

decodeGitBlobUtf8 :: GitBlob -> Either GitError Text
decodeGitBlobUtf8 blob = first (const (GitInvalidUtf8Blob (gitBlobOid blob))) (TextEncoding.decodeUtf8' (gitBlobBytes blob))

readUtf8BlobBatch :: Repository -> [GitOid] -> IO (Either GitError (Map GitOid Text))
readUtf8BlobBatch repository objectIds = do
  blobs <- readBlobBatch repository objectIds
  pure $ do
    values <- blobs
    traverse decodeGitBlobUtf8 values

readWorktreeFileBytes :: Repository -> RepoPath -> IO (Either GitError (RepoPath, ByteString))
readWorktreeFileBytes repository path =
  case repositoryWorktreeRoot repository of
    Nothing -> pure (Left GitWorktreeRequired)
    Just root -> do
      resolved <- resolveRepositoryReadPath root path
      case resolved of
        Left problem -> pure (Left (GitWorktreePathError path problem))
        Right (_, physicalPath) -> do
          attempted <- try @IOException (BS.readFile physicalPath)
          pure $
            case attempted of
              Left problem ->
                Left
                  ( GitWorktreePathError
                      path
                      (ManagedReadIoError physicalPath (ioeGetErrorString problem))
                  )
              Right bytes -> Right (path, bytes)

parseSingleOid :: Text -> ByteString -> Either GitError GitOid
parseSingleOid operation raw = do
  body <- first (GitInvalidOutput operation) (stripRequiredLineEnding malformed raw)
  if BS.any (> 127) body || BS.any (`elem` [0, 10, 13, 32, 9]) body
    then Left (GitInvalidOutput operation malformed)
    else first (const (GitInvalidOutput operation malformed)) (mkGitOid (Text.pack (BS8.unpack body)))
  where
    malformed = GitMalformedObjectHeader (protocolSample raw)

traverse_ :: (value -> Either error ()) -> [value] -> Either error ()
traverse_ _ [] = Right ()
traverse_ action (value : remaining) = action value >> traverse_ action remaining
