{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}

-- | Read-only, byte-exact Git observation.
--
-- Every Git invocation is an argv-based @typed-process@ call.  This module
-- deliberately knows nothing about ADRAI documents, graph reduction, caches,
-- or SQLite: it reports repository facts and object bytes only.
module Adrai.Git
  ( GitClient (..),
    systemGit,
    gitClient,
    Repository (..),
    RepositoryLayout (..),
    repositoryClient,
    repositoryWorktreeRoot,
    repositoryGitDir,
    repositoryCommonDir,
    repositoryLayout,
    repositoryCommonIsBare,
    repositoryCommandDirectory,
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
    gitHistoryTreeDeltaCommit,
    gitHistoryTreeDeltaParent,
    gitHistoryTreeDeltaChanges,
    GitTreeChange (..),
    gitTreeChangePath,
    gitTreeChangeOldEntry,
    gitTreeChangeNewEntry,
    historyTreeDeltasAt,
    decodeGitHistoryTreeDeltas,
    GitObjectType (..),
    GitFileMode (..),
    GitTreeEntry (..),
    listTreeEntriesAt,
    lookupTreeEntryAt,
    GitBlob (..),
    readRegularBlobAt,
    GitObjectInfo (..),
    GitProcessResult (..),
    batchObjectInfo,
    foldBlobBatch,
    foldBlobBatchInOrder,
    readBlobBatch,
    readBlobBatchOneSession,
    GitBlobBatchSession,
    withBlobBatchSession,
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
import Control.Exception (IOException, finally, try)
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
import System.Process.Typed
  ( createPipe,
    getStderr,
    getStdin,
    getStdout,
    proc,
    setEnv,
    setStderr,
    setStdin,
    setStdout,
    startProcess,
    stopProcess,
    unsafeProcessHandle,
    waitExitCode,
  )
import System.Process (terminateProcess)
import Text.Read (readMaybe)

newtype GitClient = GitClient FilePath
  deriving (Eq, Show)

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

runGit :: GitClient -> FilePath -> Map String String -> Text -> [String] -> ByteString -> IO (Either GitError GitProcessResult)
runGit (GitClient executable) commandDirectory environment operation arguments stdinBytes = do
  inheritedEnvironment <- Map.fromList <$> getEnvironment
  let effectiveEnvironment = Map.toList (environment `Map.union` inheritedEnvironment)
      config =
        setEnv effectiveEnvironment
          . setStderr createPipe
          . setStdout createPipe
          . setStdin createPipe
          $ proc executable ("-C" : commandDirectory : arguments)
  spawned <- try @IOException (startProcess config)
  case spawned of
    Left _ -> pure (Left (GitExecutableUnavailable executable))
    Right process ->
      flip finally (stopProcess process) $ do
        attempted <- try @IOException $
          Async.withAsync (readHandleAll (getStdout process)) $ \stdoutWorker ->
            Async.withAsync (drainBounded (getStderr process)) $ \stderrWorker -> do
              BS.hPut (getStdin process) stdinBytes
              hClose (getStdin process)
              exitCode <- waitExitCode process
              stdoutBytes <- Async.wait stdoutWorker
              stderrBytes <- Async.wait stderrWorker
              pure (exitCode, stdoutBytes, stderrBytes)
        pure $
          case attempted of
            Left problem ->
              Left
                ( GitCommandFailed
                    operation
                    (-1)
                    ""
                    (boundedDiagnostic (TextEncoding.encodeUtf8 (Text.pack (ioeGetErrorString problem))))
                )
            Right (exitCode, stdoutBytes, stderrBytes) ->
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
  let normalizedRoots = map repoPathText (Map.keys (Map.fromList [(path, ()) | path <- roots]))
      chunks = if null normalizedRoots then [[]] else chunksOf 256 normalizedRoots
  results <- traverse listChunk chunks
  pure $ do
    entries <- concat <$> sequence results
    let grouped = Map.fromListWith (<>) [(gitTreePath entry, [entry]) | entry <- entries]
    traverse_ rejectContradiction (Map.elems grouped)
    Right (sortOn (\entry -> (repoPathText (gitTreePath entry), gitOidText (gitTreeOid entry))) (mapMaybe firstEntry (Map.elems grouped)))
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
    rejectContradiction [] = Right ()
    rejectContradiction (candidate : remaining)
      | all (== candidate) remaining = Right ()
      | otherwise = Left (GitInvalidOutput "list tree" (GitMalformedTreeRecord "contradictory duplicate path"))
    firstEntry [] = Nothing
    firstEntry (entry : _) = Just entry

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
batchObjectInfo repository objectIds = do
  chunks <- traverse readChunk (canonicalObjectChunks objectIds)
  pure (Map.unions <$> sequence chunks)
  where
    readChunk chunk = readBufferedInfoWindow repository chunk Map.empty

readBufferedInfoWindow :: Repository -> [GitOid] -> Map GitOid (Maybe GitObjectInfo) -> IO (Either GitError (Map GitOid (Maybe GitObjectInfo)))
readBufferedInfoWindow repository requested accumulated
  | length requested > objectWindowLimit = pure (Left (objectWindowLimitError "cat-file batch-check"))
  | otherwise =
      withGitPipes repository "cat-file batch-check" ["cat-file", "--batch-check", "--buffer"] $ \stdinHandle stdoutHandle ->
        pipelineClosedObjectBatch stdinHandle requested (readFiniteInfoResponses stdoutHandle requested accumulated)

readFiniteInfoResponses :: Handle -> [GitOid] -> Map GitOid (Maybe GitObjectInfo) -> IO (Either GitError (Map GitOid (Maybe GitObjectInfo)))
readFiniteInfoResponses stdoutHandle requested accumulated = do
  responses <- readInfoResponses stdoutHandle requested accumulated
  case responses of
    Left problem -> pure (Left problem)
    Right value -> do
      trailing <- readBatchTrailing stdoutHandle
      pure (trailing >> Right value)

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
foldBlobBatch repository objectIds initial step = go initial (canonicalObjectChunks objectIds)
  where
    go accumulator [] = pure (Right accumulator)
    go accumulator (chunk : remaining) = do
      chunkResult <- readBufferedBlobWindow repository chunk accumulator step
      case chunkResult of
        Left problem -> pure (Left problem)
        Right next -> go next remaining

readBlobBatch :: Repository -> [GitOid] -> IO (Either GitError (Map GitOid GitBlob))
readBlobBatch repository objectIds =
  foldBlobBatch repository objectIds Map.empty (\values blob -> pure (Map.insert (gitBlobOid blob) blob values))

-- | Read an object set through one buffered @cat-file@ session.  Its request
-- plan is still split into finite windows of at most 'objectWindowLimit'
-- de-duplicated OIDs.
readBlobBatchOneSession :: Repository -> [GitOid] -> IO (Either GitError (Map GitOid GitBlob))
readBlobBatchOneSession repository objectIds =
  withBlobBatchSession repository (\session -> readBlobBatchFromSession session objectIds)

-- | A caller-scoped @git cat-file --batch@ child.  The
-- constructor remains opaque so requests can only be issued in bounded,
-- drained windows through 'readBlobBatchFromSession'.
data GitBlobBatchSession = GitBlobBatchSession Handle Handle

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
withBlobBatchSession :: Repository -> (GitBlobBatchSession -> IO (Either GitError value)) -> IO (Either GitError value)
withBlobBatchSession repository interaction =
  withGitPipes repository "cat-file batch" ["cat-file", "--batch"] $ \stdinHandle stdoutHandle -> do
    outcome <- interaction (GitBlobBatchSession stdinHandle stdoutHandle)
    case outcome of
      Left problem -> pure (Left problem)
      Right value -> finishBatchInput stdinHandle stdoutHandle value

-- | Read one object set through the caller-owned persistent session.  A
-- window is canonicalized exactly as the finite API is, so it contains at
-- most 'objectWindowLimit' OIDs and is drained before the next request/flush
-- is emitted.
readBlobBatchFromSession :: GitBlobBatchSession -> [GitOid] -> IO (Either GitError (Map GitOid GitBlob))
readBlobBatchFromSession (GitBlobBatchSession stdinHandle stdoutHandle) objectIds =
  streamBlobResponseChunksOpen stdinHandle stdoutHandle (canonicalObjectChunks objectIds) Map.empty (\values blob -> pure (Map.insert (gitBlobOid blob) blob values))

-- | Stream blob payloads for the given object ids in request order (no
-- sorting or de-duplication), invoking the step once per blob as it arrives.
-- The step folds each blob incrementally and may release it before the rest
-- arrive, so callers can avoid materializing whole-corpus blob maps.
foldBlobBatchInOrder :: Repository -> [GitOid] -> accumulator -> (accumulator -> GitBlob -> IO accumulator) -> IO (Either GitError accumulator)
foldBlobBatchInOrder repository objectIds initial step = go initial (chunksOf objectWindowLimit objectIds)
  where
    go accumulator [] = pure (Right accumulator)
    go accumulator (chunk : remaining) = do
      chunkResult <- readBufferedBlobWindow repository chunk accumulator step
      case chunkResult of
        Left problem -> pure (Left problem)
        Right next -> go next remaining

-- | One finite Git 2.31+ buffered window.  The writer closes stdin once its
-- at-most-'objectWindowLimit' OID plan is emitted; stdout response decoding and stderr
-- draining therefore proceed without either pipe being allowed to deadlock the
-- other.  'withGitPipes' owns child cancellation and waits for stderr on every
-- outcome.
readBufferedBlobWindow :: Repository -> [GitOid] -> accumulator -> (accumulator -> GitBlob -> IO accumulator) -> IO (Either GitError accumulator)
readBufferedBlobWindow repository requested accumulated step
  | length requested > objectWindowLimit = pure (Left (objectWindowLimitError "cat-file batch"))
  | otherwise =
      withGitPipes repository "cat-file batch" ["cat-file", "--batch", "--buffer"] $ \stdinHandle stdoutHandle ->
        pipelineClosedObjectBatch stdinHandle requested (readFiniteBlobResponses stdoutHandle requested accumulated step)

readFiniteBlobResponses :: Handle -> [GitOid] -> accumulator -> (accumulator -> GitBlob -> IO accumulator) -> IO (Either GitError accumulator)
readFiniteBlobResponses stdoutHandle requested accumulated step = do
  responses <- readBlobResponses stdoutHandle requested accumulated step
  case responses of
    Left problem -> pure (Left problem)
    Right value -> do
      trailing <- readBatchTrailing stdoutHandle
      pure (trailing >> Right value)

readBatchTrailing :: Handle -> IO (Either GitError ByteString)
readBatchTrailing stdoutHandle = do
  attempted <- try @IOException (BS.hGet stdoutHandle 129)
  pure $
    case attempted of
      Left problem -> Left (gitBatchIoError problem)
      Right trailing -> validateGitBatchTrailing trailing >> Right trailing

-- | A closed finite plan differs from the persistent protocol helper below:
-- Git's @--buffer@ output is released only after this input closes.  The two
-- workers are still concurrent, so stderr drains while a large first blob is
-- decoded and any asynchronous cancellation tears down both workers.
pipelineClosedObjectBatch :: Handle -> [GitOid] -> IO (Either GitError value) -> IO (Either GitError value)
pipelineClosedObjectBatch _ [] readResponses = readResponses
pipelineClosedObjectBatch stdinHandle requested readResponses =
  Async.withAsync (writeBatchRequestsAndClose stdinHandle requested) $ \writer ->
    Async.withAsync readResponses $ \reader ->
      Async.waitEither writer reader >>= \case
        Left sent ->
          case sent of
            Left problem -> pure (Left problem)
            Right () -> Async.wait reader
        Right received ->
          case received of
            Left problem -> pure (Left problem)
            Right value -> do
              sent <- Async.wait writer
              pure (value <$ sent)

writeBatchRequestsAndClose :: Handle -> [GitOid] -> IO (Either GitError ())
writeBatchRequestsAndClose stdinHandle requested = do
  attempted <- try @IOException $
    (BS.hPut stdinHandle (BS.concat (map objectRequestLine requested)) >> hFlush stdinHandle)
      `finally` closeQuietly stdinHandle
  pure $
    case attempted of
      Left problem -> Left (gitBatchIoError problem)
      Right () -> Right ()

-- | Variant for a caller-owned persistent batch session.  An empty request
-- list leaves stdin open so another bounded request can follow on the same
-- process.
streamBlobResponsesOpen :: Handle -> Handle -> [GitOid] -> accumulator -> (accumulator -> GitBlob -> IO accumulator) -> IO (Either GitError accumulator)
streamBlobResponsesOpen stdinHandle stdoutHandle requested accumulated step
  | length requested > objectWindowLimit = pure (Left (objectWindowLimitError "cat-file batch"))
  | otherwise =
      pipelineObjectBatch stdinHandle requested (readBlobResponses stdoutHandle requested accumulated step)

-- | Keep one @cat-file --batch@ request window in flight while its responses
-- are read.
-- Writing and reading run together because a large blob can fill a Windows
-- stdout pipe before the child has consumed the entire request batch. The
-- request writer emits one bare-OID sequence, then flushes the handle once.
-- Callers only invoke it with a chunk produced by 'canonicalObjectChunks' (at
-- most 'objectWindowLimit' OIDs).
pipelineObjectBatch :: Handle -> [GitOid] -> IO (Either GitError value) -> IO (Either GitError value)
pipelineObjectBatch _ [] readResponses = readResponses
pipelineObjectBatch stdinHandle requested readResponses =
  Async.withAsync (writePersistentBatchRequests (handleBatchInput stdinHandle) requested) $ \writer ->
    Async.withAsync readResponses $ \reader ->
      Async.waitEither writer reader >>= \case
        Left sent ->
          case sent of
            Left problem -> pure (Left problem)
            Right () -> Async.wait reader
        Right received ->
          case received of
            Left problem -> pure (Left problem)
            Right value -> do
              sent <- Async.wait writer
              pure (value <$ sent)

-- | Emit one bounded plain @--batch@ request plan as one write/flush. Stdin
-- remains open so a later window can use the same child. The 'GitBatchInput'
-- argument is deliberately a narrow native-I/O seam for framing tests.
writePersistentBatchRequests :: GitBatchInput -> [GitOid] -> IO (Either GitError ())
writePersistentBatchRequests _ [] = pure (Right ())
writePersistentBatchRequests input requested = do
  attempted <- try @IOException $ do
    gitBatchInputWrite input (BS.concat (map objectRequestLine requested))
    gitBatchInputFlush input
  pure $
    case attempted of
      Left problem -> Left (gitBatchIoError problem)
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
gitBatchIoError problem =
  GitInvalidOutput
    "cat-file batch"
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
streamBlobResponseChunksOpen :: Handle -> Handle -> [[GitOid]] -> accumulator -> (accumulator -> GitBlob -> IO accumulator) -> IO (Either GitError accumulator)
streamBlobResponseChunksOpen stdinHandle stdoutHandle chunks accumulated step =
  case chunks of
    [] -> pure (Right accumulated)
    chunk : remaining -> do
      chunkResult <- streamBlobResponsesOpen stdinHandle stdoutHandle chunk accumulated step
      case chunkResult of
        Left problem -> pure (Left problem)
        Right next -> streamBlobResponseChunksOpen stdinHandle stdoutHandle remaining next step

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
readProtocolLine handle = go BS.empty
  where
    go accumulated
      | BS.length accumulated > 512 = ioError (userError "Git protocol header exceeds 512 bytes")
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

validateGitBatchTrailing :: ByteString -> Either GitError ()
validateGitBatchTrailing trailing
  | BS.null trailing = Right ()
  | otherwise = Left (GitInvalidOutput "cat-file batch" (GitUnexpectedTrailingBytes (protocolSample trailing)))

withGitPipes :: Repository -> Text -> [String] -> (Handle -> Handle -> IO (Either GitError value)) -> IO (Either GitError value)
withGitPipes repository operation arguments interaction = do
  let GitClient executable = repositoryClient repository
      config =
        setStderr createPipe
          . setStdout createPipe
          . setStdin createPipe
          $ proc executable ("-C" : repositoryCommandDirectory repository : arguments)
  spawned <- try @IOException (startProcess config)
  case spawned of
    Left _ -> pure (Left (GitExecutableUnavailable executable))
    Right process ->
      flip finally (stopProcess process) $
        Async.withAsync (drainBounded (getStderr process)) $ \stderrWorker -> do
          -- Exceptions thrown by the caller's fold callback deliberately pass
          -- through this boundary; they are not process-spawn failures.
          outcome <- interaction (getStdin process) (getStdout process)
          completion <- try @IOException $ do
            closeQuietly (getStdin process)
            case outcome of
              -- If forceful termination itself fails, let the enclosing
              -- IOException handler return immediately.  Waiting after a
              -- failed termination could otherwise hang on a live malformed
              -- protocol child before the outer finalizer gets a chance to
              -- stop it.
              Left _ -> terminateProcess (unsafeProcessHandle process)
              Right _ -> pure ()
            exitCode <- waitExitCode process
            stderrBytes <- Async.wait stderrWorker
            pure (exitCode, stderrBytes)
          pure $
            case completion of
              Left problem ->
                Left
                  ( GitCommandFailed
                      operation
                      (-1)
                      ""
                      (boundedDiagnostic (TextEncoding.encodeUtf8 (Text.pack (ioeGetErrorString problem))))
                  )
              Right (exitCode, stderrBytes) ->
                case outcome of
                  Left problem -> Left problem
                  Right value
                    | exitCode == ExitSuccess -> Right value
                    | otherwise ->
                        Left
                          ( GitCommandFailed
                              operation
                              (exitCodeNumber exitCode)
                              ""
                              (boundedDiagnostic stderrBytes)
                          )

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
