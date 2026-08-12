{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

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
    reachableCommitGraphAt,
    decodeGitCommitGraph,
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
    readBlobBatch,
    decodeGitBlobUtf8,
    readUtf8BlobBatch,
    readWorktreeFileBytes,
    GitError (..),
    GitProtocolError (..),
    boundedDiagnostic,
    canonicalObjectChunks,
    decodeGitBoolean,
    decodeGitPathOutput,
    decodeGitTreeOutput,
    decodeGitObjectInfoHeader,
    decodeGitBlobHeader,
    decodeGitBlobPayload,
    validateGitBatchTrailing,
    runRepository,
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
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word64)
import System.Directory (canonicalizePath, doesDirectoryExist)
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
    setStderr,
    setStdin,
    setStdout,
    startProcess,
    stopProcess,
    waitExitCode,
  )
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

runGit :: GitClient -> FilePath -> Text -> [String] -> ByteString -> IO (Either GitError GitProcessResult)
runGit (GitClient executable) commandDirectory operation arguments stdinBytes = do
  let config =
        setStderr createPipe
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
  insideResult <- runGit client input "discover inside-worktree" ["rev-parse", "--is-inside-work-tree"] BS.empty
  bareResult <- runGit client input "discover bare" ["rev-parse", "--is-bare-repository"] BS.empty
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
  result <- runGit client directory operation arguments BS.empty
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

runRepository :: Repository -> Text -> [String] -> ByteString -> IO (Either GitError GitProcessResult)
runRepository repository = runGit (repositoryClient repository) (repositoryCommandDirectory repository)

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
canonicalObjectChunks = chunksOf 256 . Map.keys . Map.fromList . map (,())

chunksOf :: Int -> [value] -> [[value]]
chunksOf _ [] = []
chunksOf size values = take size values : chunksOf size (drop size values)

batchObjectInfo :: Repository -> [GitOid] -> IO (Either GitError (Map GitOid (Maybe GitObjectInfo)))
batchObjectInfo repository objectIds = do
  chunks <- traverse readChunk (canonicalObjectChunks objectIds)
  pure (Map.unions <$> sequence chunks)
  where
    readChunk chunk =
      withGitPipes repository "cat-file batch-check" ["cat-file", "--batch-check"] $ \stdinHandle stdoutHandle ->
        streamInfoResponses stdinHandle stdoutHandle chunk Map.empty

streamInfoResponses :: Handle -> Handle -> [GitOid] -> Map GitOid (Maybe GitObjectInfo) -> IO (Either GitError (Map GitOid (Maybe GitObjectInfo)))
streamInfoResponses stdinHandle stdoutHandle requested accumulated =
  case requested of
    [] -> finishBatchInput stdinHandle stdoutHandle accumulated
    expected : remaining -> do
      response <- requestHeader stdinHandle stdoutHandle expected
      case response of
        Left problem -> pure (Left problem)
        Right header ->
          case decodeGitObjectInfoHeader expected header of
            Left problem -> pure (Left problem)
            Right pair -> streamInfoResponses stdinHandle stdoutHandle remaining (uncurry Map.insert pair accumulated)

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
      chunkResult <-
        withGitPipes repository "cat-file batch" ["cat-file", "--batch"] $ \stdinHandle stdoutHandle ->
          streamBlobResponses stdinHandle stdoutHandle chunk accumulator step
      case chunkResult of
        Left problem -> pure (Left problem)
        Right next -> go next remaining

readBlobBatch :: Repository -> [GitOid] -> IO (Either GitError (Map GitOid GitBlob))
readBlobBatch repository objectIds =
  foldBlobBatch repository objectIds Map.empty (\values blob -> pure (Map.insert (gitBlobOid blob) blob values))

streamBlobResponses :: Handle -> Handle -> [GitOid] -> accumulator -> (accumulator -> GitBlob -> IO accumulator) -> IO (Either GitError accumulator)
streamBlobResponses stdinHandle stdoutHandle requested accumulated step =
  case requested of
    [] -> finishBatchInput stdinHandle stdoutHandle accumulated
    expected : remaining -> do
      response <- requestHeader stdinHandle stdoutHandle expected
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
                        Left problem ->
                          pure
                            ( Left
                                ( GitInvalidOutput
                                    "cat-file batch"
                                    (GitMalformedObjectHeader (protocolSample (TextEncoding.encodeUtf8 (Text.pack (ioeGetErrorString problem)))))
                                )
                            )
                        Right (payload, framing) ->
                          case decodeGitBlobPayload expected size payload framing of
                            Left problem -> pure (Left problem)
                            Right blob -> do
                              next <- step accumulated blob
                              streamBlobResponses stdinHandle stdoutHandle remaining next step
                _ -> pure (Left (GitInvalidOutput "cat-file batch" (GitMalformedObjectHeader (protocolSample header))))

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

requestHeader :: Handle -> Handle -> GitOid -> IO (Either GitError ByteString)
requestHeader stdinHandle stdoutHandle objectId = do
  attempted <- try @IOException $ do
    BS8.hPutStrLn stdinHandle (TextEncoding.encodeUtf8 (gitOidText objectId))
    hFlush stdinHandle
    readProtocolLine stdoutHandle
  pure $
    case attempted of
      Left problem -> Left (GitInvalidOutput "cat-file batch" (GitMalformedObjectHeader (protocolSample (TextEncoding.encodeUtf8 (Text.pack (ioeGetErrorString problem))))))
      Right header -> Right header

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
              Left _ -> stopProcess process
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
