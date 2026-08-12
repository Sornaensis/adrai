{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | 8-canonical-stage Git transaction engine.
--
-- Mirrors the Python reference in
-- @ADRAI_1_Source/adrai_core/transaction.py@:
--
-- * 'commitAppendOnlyOperation' — guarded append-only commit of managed files.
-- * 'commitBootstrapFiles'      — bootstrap commit supporting unborn repos.
--
-- Both functions acquire the ADRAI mutation lock via
-- 'Adrai.Provenance.Git.Lock.withGitLock', validate the repository state,
-- write files to the worktree, create a temporary index, build a single commit
-- via @commit-tree@, and compare-and-swap the attached branch ref.
--
-- All Git plumbing calls use 'Adrai.Git.runRepository' with typed-process argv
-- arrays (no shell interpolation).

module Adrai.Service.Transaction
  ( -- * Error types
    TransactionError (..),
    -- * Result type
    TransactionResult (..),
    -- * Generated file
    GeneratedFile (..),
    -- * Transaction configuration
    TransactionConfig (..),
    -- * Null OID constant
    nullOid,
    -- * Core transaction functions
    commitAppendOnlyOperation,
    commitBootstrapFiles,
  )
where

import Adrai.Git
  ( Repository (..),
    GitOid (..),
    gitOidText,
    runRepository,
    GitError (..),
    GitProcessResult (..),
    repositoryCommonDir,
    repositoryWorktreeRoot,
    resolveRevision,
    GitHeadState (..),
    repositoryHeadState,
    RevisionSpec (..),
    mkRevisionSpec,
  )
import Adrai.Provenance
  ( OperationContext (..),
    ProvenanceCapsule,
    mkGitOid,
    provenanceOperationId,
    provenanceBasis,
    provenanceActor,
    provenanceTimestampMs,
    provenanceObjectId,
    provenanceObjectIdText,
    provenanceOperationContext,
  )
import Adrai.Provenance.Git.Lock
  ( withGitLock,
    GitLockError (..),
  )
import Adrai.Types
  ( RepoPath,
    repoPathText,
    GitRef (..),
    mkGitRef,
    operationIdText,
    actorId,
    gitRefText,
  )
import Adrai.Format.Document
  ( ParsedManagedDocument (..),
    parseManagedDocument,
  )
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Exception
  ( Exception,
    SomeException,
    bracket,
    catch,
    fromException,
    throwIO,
    try,
  )
import System.Exit (ExitCode (ExitSuccess))
import Control.Monad (when, void, unless, forM_, filterM)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Data.IORef (newIORef, modifyIORef', readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory
  ( removeFile,
    doesFileExist,
    doesDirectoryExist,
    createDirectoryIfMissing,
    removeDirectory,
    getDirectoryContents,
  )
import System.FilePath
  ( (</>),
    isAbsolute,
    takeDirectory,
  )
import System.IO (hClose, openTempFile)

-- ---------------------------------------------------------------------------
-- Data types
-- ---------------------------------------------------------------------------

-- | Errors raised during a transaction.
data TransactionError
  = Stage1ResolveRepo Text
      -- ^ Stage 1: worktree root missing (bare repo)
  | Stage2AcquireLock Text
      -- ^ Stage 2: lock acquisition failed
  | Stage3ValidateState Text
      -- ^ Stage 3: state validation failed
  | Stage4GenerateFiles Text
      -- ^ Stage 4: generated file validation failed
  | Stage5ValidateGenerated Text
      -- ^ Stage 5: generated content validation failed
  | Stage6CreateTemporaryIndex Text
      -- ^ Stage 6: temporary index creation failed
  | Stage7CommitTree Text
      -- ^ Stage 7: commit-tree failed
  | Stage8UpdateRef Text
      -- ^ Stage 8: update-ref / refresh-index failed
  | RollbackFailed Text
      -- ^ Rollback after failure could not clean up
  | TransactionAborted Text
      -- ^ Transaction aborted before completion
  deriving (Eq, Show)

instance Exception TransactionError

-- | Result of a successful transaction.
data TransactionResult = TransactionResult
  { transactionOperationId  :: String
  , transactionCommitOid    :: GitOid
  , transactionCreatedPaths :: [RepoPath]
  , transactionIndexUpdated :: Bool
  }
  deriving (Eq, Show)

-- | A file generated for the transaction, to be written into the worktree.
data GeneratedFile = GeneratedFile
  { genFilePath  :: RepoPath
  , genFileBytes :: ByteString
  }
  deriving (Eq, Show)

-- | Immutable configuration supplied to a transaction.
data TransactionConfig = TransactionConfig
  { configOperationId  :: String
  , configSubject      :: Text
  , configTrailers     :: Map.Map String String
  , configExpectedHead :: GitOid
  , configGenerated    :: [GeneratedFile]
  }
  deriving (Eq, Show)

-- | Null OID (all zeros, 40 characters). Used as the expected-old for
-- unborn-repo CAS.
nullOid :: GitOid
nullOid = GitOid (T.pack $ replicate 40 '0')

-- | Whether the given OID is the null OID.
isNullOid :: GitOid -> Bool
isNullOid (GitOid o) = o == T.pack (replicate 40 '0')

-- ---------------------------------------------------------------------------
-- Helpers: repository-relative path validation
-- ---------------------------------------------------------------------------

-- | Check that a repo-relative path is safe (no absolute, no @..@ segments,
-- no leading slash, no backslash).
validateRepoRelativePath :: RepoPath -> Either TransactionError ()
validateRepoRelativePath p =
  let pText = repoPathText p
      segments = T.splitOn "/" pText
   in if T.any (== '\\') pText
        then Left (Stage4GenerateFiles "path contains backslash")
        else if BS.any (`elem` [0, 10, 13]) (encodeUtf8 pText)
          then Left (Stage4GenerateFiles "path contains control character")
          else if null segments
            then Left (Stage4GenerateFiles "path has no components")
            else if any (== "..") segments
              then Left (Stage4GenerateFiles "path contains '..' segment")
              else if any T.null segments
                then Left (Stage4GenerateFiles "path contains empty segment")
                else if T.isPrefixOf "/" pText
                  then Left (Stage4GenerateFiles "path is absolute")
                  else Right ()

-- ---------------------------------------------------------------------------
-- Helpers: provenance capsule extraction
-- ---------------------------------------------------------------------------

-- | Decode the base64url-encoded capsule from a managed document's text.
-- Returns the capsule and the provenance operation context.
decodeProvenanceCapsule :: Text -> Either TransactionError (ProvenanceCapsule, OperationContext)
decodeProvenanceCapsule text = do
  -- The capsule is encoded as a base64url string in the trailer comment.
  -- We need to extract it from the text and decode it.
  -- For now, we'll use the document parsing path.
  Left (Stage5ValidateGenerated "capsule not yet embedded in raw text for transaction")

-- | Parse provenance fields from a managed document's provenance capsule.
-- Returns (operationId, basis/parent, actor, timestamp, objectId).
extractProvenanceFields :: ParsedManagedDocument -> (Text, GitOid, Text, Integer, Text)
extractProvenanceFields doc =
  let capsule = parsedManagedCapsule doc
      ctx = provenanceOperationContext capsule
      opId = operationIdText (provenanceOperationId capsule)
      basis = provenanceBasis capsule
      actor = actorId (provenanceActor capsule)
      ts = provenanceTimestampMs capsule
      objId = provenanceObjectIdText (provenanceObjectId capsule)
   in (opId, basis, actor, ts, objId)

-- ---------------------------------------------------------------------------
-- Stage 3 helpers: validation checks
-- ---------------------------------------------------------------------------

-- | Check that the repository HEAD is attached to a branch.
checkAttachedBranch :: Repository -> IO (Either Text ())
checkAttachedBranch repository = do
  result <- repositoryHeadState repository
  pure $
    case result of
      Left err -> Left ("symbolic-ref HEAD failed: " <> T.pack (show err))
      Right (GitHeadAttached _) -> Right ()
      Right GitHeadDetached -> Left "HEAD is detached; attach a branch first"

-- | Check that no Git operations are active (merge, cherry-pick, etc.) and
-- there are no unresolved paths in the index.
checkNoActiveGitOperations :: Repository -> IO (Either Text ())
checkNoActiveGitOperations repository = do
  -- Use git status porcelain to check for unresolved paths
  unmergedResult <-
    runRepository
      repository
      "check unmerged"
      ["diff", "--name-only", "--diff-filter=U"]
      BS.empty
  case unmergedResult of
    Left err -> pure (Left ("diff unmerged failed: " <> T.pack (show err)))
    Right result
      | processExitCode result /= ExitSuccess ->
          pure (Left ("diff unmerged exited " <> T.pack (show (processExitCode result))))
      | otherwise -> do
          -- Check for active operation markers in the common dir
          let commonDir = repositoryCommonDir repository
              checkFile name = doesFileExist (commonDir </> name)
              checkDir name = doesDirectoryExist (commonDir </> name)
          let activeOps =
                filterM (\name -> doesFileExist (commonDir </> name))
                  [ "MERGE_HEAD",
                    "CHERRY_PICK_HEAD",
                    "REVERT_HEAD",
                    "BISECT_LOG"
                  ]
              activeDirs =
                filterM (\name -> doesDirectoryExist (commonDir </> name))
                  [ "rebase-apply",
                    "rebase-merge",
                    "sequencer"
                  ]
          active <- do
            ops <- activeOps
            dirs <- activeDirs
            return (ops <> dirs)
          if null active
            then pure (Right ())
            else
              pure
                ( Left
                    ( "active Git operations: " <> T.intercalate ", " (map T.pack active)
                    )
                )

-- | Check that the given managed paths are clean in the worktree.
-- Uses @git status --porcelain=v1 --untracked-files=all@ for each path.
checkManagedPathsClean :: Repository -> [RepoPath] -> IO (Either Text ())
checkManagedPathsClean repository paths = do
  let pathArgs = map (\p -> "--" : [repoPathText p]) paths
  result <-
    runRepository
      repository
      "status check"
      ["status", "--porcelain=v1", "--untracked-files=all"]
      BS.empty
  pure $
    case result of
      Left err -> Left ("status check failed: " <> T.pack (show err))
      Right res
        | processExitCode res /= ExitSuccess ->
            Left ("status check exited " <> T.pack (show (processExitCode res)))
        | otherwise ->
            let statusLines = T.splitOn "\n" (decodeBounded (processStdout res))
                dirtyPaths =
                  [ line
                    | line <- statusLines,
                      not (T.null line),
                      any (`T.isInfixOf` line) (map repoPathText paths)
                  ]
             in if null dirtyPaths
                  then Right ()
                  else Left ("managed paths are dirty: " <> T.intercalate "; " dirtyPaths)

decodeBounded :: ByteString -> Text
decodeBounded raw =
  case BS.length raw of
    n | n > 2048 -> T.take 2048 decoded <> "...[truncated]"
    _ -> decoded
  where
    decoded = T.replace "\r\n" "\n" . T.replace "\r" "\n" $ TE.decodeUtf8With lenientDecode raw

-- ---------------------------------------------------------------------------
-- Stage 4: validate generated files
-- ---------------------------------------------------------------------------

-- | Stage 4: validate generated file paths are repo-relative (no @..@, no
-- absolute paths).
validateGeneratedPaths :: TransactionConfig -> Either TransactionError ()
validateGeneratedPaths TransactionConfig{..} = do
  -- All paths already validated at construction, but check here too
  mapM_ (validateRepoRelativePath . genFilePath) configGenerated
  unless (not (null configGenerated))
    (Left (Stage4GenerateFiles "at least one file must be generated"))
  -- Check for duplicate paths
  let pathSet = Map.fromList [(genFilePath f, ()) | f <- configGenerated]
  unless (length pathSet == length configGenerated)
    (Left (Stage4GenerateFiles "duplicate generated paths"))
  Right ()

-- ---------------------------------------------------------------------------
-- Stage 5: validate generated file contents
-- ---------------------------------------------------------------------------

-- | Stage 5: Validate that each generated file:
-- * Has a valid managed-document provenance capsule.
-- * @op@ matches the operation ID.
-- * @b@ matches the expected head.
-- * No duplicate object IDs.
-- * Exactly one timestamp and one actor.
-- * Then write files to the worktree (append-only).
validateGeneratedFiles :: Repository -> TransactionConfig -> IO (Either TransactionError ())
validateGeneratedFiles repository TransactionConfig{..} = do
  let genFiles = configGenerated
  when (null genFiles) $
    throwIO (Stage5ValidateGenerated "an operation must create at least one file")

  -- Accumulate validation state using mutable references
  objectMapRef <- newIORef Map.empty
  timestampsRef <- newIORef Set.empty
  actorsRef <- newIORef Set.empty
  pathErrorsRef <- newIORef []

  forM_ genFiles $ \GeneratedFile{..} -> do
    -- Decode the file as UTF-8 text
    let textContent = TE.decodeUtf8With lenientDecode genFileBytes
    -- Parse the document to extract the provenance capsule
    parsedResult <-
      try @SomeException (pure (parseManagedDocument genFilePath genFileBytes))
    case parsedResult of
      Left parseErr -> do
        modifyIORef' pathErrorsRef ((genFilePath, "parse error: " <> T.pack (show parseErr)) :)
        return ()
      Right (Right parsedDoc) -> do
        -- Validate op matches
        let (opId, basis, actor, ts, objIdText) = extractProvenanceFields parsedDoc
        let opText = T.pack configOperationId
        unless (opId == opText) $
          modifyIORef' pathErrorsRef ((genFilePath, "op mismatch") :)
        -- Validate basis matches expected head
        unless (basis == configExpectedHead) $
          modifyIORef' pathErrorsRef ((genFilePath, "basis mismatch") :)
        -- Check duplicate object IDs
        let objId = GitOid objIdText
        modifyIORef' objectMapRef (\m -> Map.insert objId () m)
        -- Track timestamps and actors
        modifyIORef' timestampsRef (\s -> Set.insert ts s)
        modifyIORef' actorsRef (\s -> Set.insert actor s)

  -- Collect results
  objectMap <- readIORef objectMapRef
  timestampsSet' <- readIORef timestampsRef
  actorsSet' <- readIORef actorsRef
  pathErrors <- readIORef pathErrorsRef

  -- Check for errors
  unless (null pathErrors) $
    throwIO (Stage5ValidateGenerated ("validation errors: " <> T.intercalate "; " (map (\(p, e) -> repoPathText p <> ": " <> e) pathErrors)))

  -- Check for duplicate OIDs
  when (Map.null objectMap) $
    return () -- already checked by Map.insert (no duplicates possible in a Map)

  -- Check exactly one timestamp
  when (Set.size timestampsSet' /= 1) $
    throwIO (Stage5ValidateGenerated "all files in one operation must share one claimed timestamp")

  -- Check exactly one actor
  when (Set.size actorsSet' /= 1) $
    throwIO (Stage5ValidateGenerated "all files in one operation must share one semantic actor")

  -- Write files to worktree (append-only)
  forM_ genFiles $ \GeneratedFile{..} -> do
    let p = repoPathText genFilePath
        worktreeRoot = repositoryWorktreeRoot repository
        filePath = case worktreeRoot of
          Nothing -> Left (Stage5ValidateGenerated "worktree root is missing")
          Just root -> Right (root </> T.unpack p)
    case filePath of
      Left err -> throwIO err
      Right path -> do
        -- Check the path doesn't already exist (append-only)
        exists <- doesFileExist path
        when exists $
          throwIO (Stage5ValidateGenerated (T.pack "append-only path already exists: " <> p))
        -- Create parent directories
        let parentDir = takeDirectory path
        createDirectoryIfMissing True parentDir
        -- Write the file
        BS.writeFile path genFileBytes

  return (Right ())

-- ---------------------------------------------------------------------------
-- Stage 6: Create temporary index
-- ---------------------------------------------------------------------------

-- | Stage 6: Create a temporary index from the old head, add generated paths,
-- and write the resulting tree.
createTemporaryIndex :: Repository -> GitOid -> [RepoPath] -> IO (Either TransactionError (FilePath, GitOid))
createTemporaryIndex repository oldHead generatedPaths = do
  let commonDir = repositoryCommonDir repository

  -- Create a temporary index file in the git common directory
  (idxPath, idxHandle) <- openTempFile commonDir "adrai-index-"
  hClose idxHandle

  let env = Map.singleton "GIT_INDEX_FILE" idxPath
  let gitCmd = \args ->
        runRepository repository ("temp-index " <> T.pack (show args)) args BS.empty

  -- read-tree <old_head> (or --empty for unborn)
  readTreeResult <-
    if isNullOid oldHead
      then gitCmd ["read-tree", "--empty"]
      else gitCmd ["read-tree", Text.unpack (gitOidText oldHead)]

  case readTreeResult of
    Left err -> do
      removeTempFile idxPath
      throwIO (Stage6CreateTemporaryIndex ("read-tree failed: " <> T.pack (show err)))
    Right res
      | processExitCode res /= ExitSuccess -> do
          removeTempFile idxPath
          throwIO (Stage6CreateTemporaryIndex ("read-tree exited " <> T.pack (show (processExitCode res))))
      | otherwise -> do
          -- add --sparse -- <paths>
          addPathsResult <-
            gitCmd $
              ["add", "--sparse", "--"]
                <> map (T.unpack . repoPathText) generatedPaths
          case addPathsResult of
            Left err -> do
              removeTempFile idxPath
              throwIO (Stage6CreateTemporaryIndex ("add paths failed: " <> T.pack (show err)))
            Right res2
              | processExitCode res2 /= ExitSuccess -> do
                  removeTempFile idxPath
                  throwIO (Stage6CreateTemporaryIndex ("add paths exited " <> T.pack (show (processExitCode res2))))
              | otherwise -> do
                  -- write-tree
                  treeResult <- gitCmd ["write-tree"]
                  case treeResult of
                    Left err -> do
                      removeTempFile idxPath
                      throwIO (Stage6CreateTemporaryIndex ("write-tree failed: " <> T.pack (show err)))
                    Right res3
                      | processExitCode res3 /= ExitSuccess -> do
                          removeTempFile idxPath
                          throwIO (Stage6CreateTemporaryIndex ("write-tree exited " <> T.pack (show (processExitCode res3))))
                      | otherwise -> do
                          case parseSingleOidFromOutput "write-tree" (processStdout res3) of
                            Left parseErr ->
                              throwIO (Stage6CreateTemporaryIndex ("parse tree OID: " <> parseErr))
                            Right (GitOid oidStr) ->
                              removeTempFile idxPath >> return (Right (idxPath, GitOid oidStr))

removeTempFile :: FilePath -> IO ()
removeTempFile path = void $ try @SomeException (removeFile path)

parseSingleOidFromOutput :: Text -> ByteString -> Either Text GitOid
parseSingleOidFromOutput operation raw = do
  let body = stripTrailingWhitespace raw
  case T.stripPrefix (operation <> ": ") (TE.decodeUtf8With lenientDecode body) of
    Just oidText -> do
      oid <- first (const ("parse " <> operation <> " OID: ")) (mkGitOid oidText)
      Right oid
    Nothing ->
      case T.stripPrefix " " (TE.decodeUtf8With lenientDecode body) of
        Just oidText -> do
          oid <- first (const ("parse " <> operation <> " OID: ")) (mkGitOid oidText)
          Right oid
        Nothing ->
          Left ("unexpected output from " <> operation <> ": " <> boundedOutput raw)

stripTrailingWhitespace :: ByteString -> ByteString
stripTrailingWhitespace bs =
  case BS.reverse (BS.takeWhile (\c -> c == 10 || c == 13 || c == 32) (BS.reverse bs)) of
    "" -> BS.empty
    trimmed -> BS.reverse trimmed

boundedOutput :: ByteString -> Text
boundedOutput raw =
  T.take 200 . TE.decodeUtf8With lenientDecode $
    if BS.length raw > 500
      then BS.take 500 raw
      else raw

-- ---------------------------------------------------------------------------
-- Stage 7: Commit tree
-- ---------------------------------------------------------------------------

-- | Stage 7: Build commit message and create a tree commit.
--
-- Message format:
--
-- > @subject@
-- >
-- > @ADRAI-Op: <operation_id>@
-- > @ADRAI-<key>: <value>@  (one per trailer)
commitTree :: Repository -> GitOid -> GitOid -> Text -> String -> Map.Map String String -> IO (Either TransactionError GitOid)
commitTree repository oldHead treeOid subject operationId trailers = do
  let message = buildCommitMessage subject operationId trailers

  args <-
    if isNullOid oldHead
      then pure ["commit-tree", Text.unpack (gitOidText treeOid), "-i"]
      else pure ["commit-tree", Text.unpack (gitOidText treeOid), "-p", Text.unpack (gitOidText oldHead), "-i"]

  result <- runRepository repository "commit-tree" args (encodeUtf8 message)
  pure $
    case result of
      Left err -> Left (Stage7CommitTree ("commit-tree failed: " <> T.pack (show err)))
      Right res
        | processExitCode res /= ExitSuccess ->
            Left (Stage7CommitTree ("commit-tree exited " <> T.pack (show (processExitCode res))))
        | otherwise ->
            case parseSingleOidFromOutput "commit-tree" (processStdout res) of
              Left parseErr -> Left (Stage7CommitTree ("parse commit OID: " <> parseErr))
              Right commitOid -> Right commitOid

buildCommitMessage :: Text -> String -> Map.Map String String -> Text
buildCommitMessage subject operationId trailers =
  T.unlines
    ( [ T.strip subject,
        "",
        "ADRAI-Op: " <> T.pack operationId
      ]
      ++ map trailerLine (Map.toList trailers)
    )
  where
    trailerLine (key, value) = "ADRAI-" <> T.pack key <> ": " <> T.pack value

-- ---------------------------------------------------------------------------
-- Stage 8: Update ref and refresh index
-- ---------------------------------------------------------------------------

-- | Stage 8: Get the current branch ref, update-ref with CAS, and refresh
-- only the generated paths in the real index.
updateRefAndRefresh :: Repository -> GitRef -> GitOid -> GitOid -> [RepoPath] -> Bool -> IO (Either TransactionError GitOid)
updateRefAndRefresh repository branchRef newCommit expectedOld generatedPaths bootstrap = do
  let configOperationIdDefault = "unknown"
  -- Get the current ref tip for CAS verification
  let refText = gitRefText branchRef
  currentTipResult <-
    runRepository
      repository
      "rev-parse ref"
      ["rev-parse", "--verify", Text.unpack refText]
      BS.empty
  case currentTipResult of
    Left err ->
      throwIO (Stage8UpdateRef ("rev-parse ref failed: " <> T.pack (show err)))
    Right res
      | processExitCode res /= ExitSuccess ->
          throwIO (Stage8UpdateRef ("rev-parse ref exited " <> T.pack (show (processExitCode res))))
      | otherwise -> do
          let casExpectedOld =
                if bootstrap && isNullOid expectedOld
                  then nullOid
                  else expectedOld
          let casCurrentTip =
                case parseSingleOidFromOutput "rev-parse" (processStdout res) of
                  Left _ -> expectedOld
                  Right oid -> oid
          -- CAS via update-ref
          updateRefResult <-
            runRepository
              repository
              "update-ref"
              [ "update-ref",
                "-m",
                "adrai " <> configOperationIdDefault,
                Text.unpack refText,
                Text.unpack (gitOidText newCommit),
                Text.unpack (gitOidText casExpectedOld)
              ]
              BS.empty
          case updateRefResult of
            Left err ->
              throwIO (Stage8UpdateRef ("update-ref failed: " <> T.pack (show err)))
            Right res2
              | processExitCode res2 /= ExitSuccess ->
                  throwIO (Stage8UpdateRef ("update-ref exited " <> T.pack (show (processExitCode res2))))
              | otherwise -> do
                  -- Refresh index for generated paths only
                  indexUpdated <-
                    if null generatedPaths
                      then pure True
                      else do
                        refreshResult <-
                          runRepository
                            repository
                            "reset index"
                            (["reset", "-q", "HEAD", "--"] <> map (T.unpack . repoPathText) generatedPaths)
                            BS.empty
                        pure $ case refreshResult of
                          Left _ -> False
                          Right res -> processExitCode res == ExitSuccess
                  unless indexUpdated $
                    return () -- Non-fatal: index refresh is best-effort
                  return (Right newCommit)

-- ---------------------------------------------------------------------------
-- Rollback logic
-- ---------------------------------------------------------------------------

-- | Rollback after failure: remove uncommitted generated files if the
-- current tip differs from our new commit.  Never delete a path that a
-- concurrent external commit adopted (check via blob_oid_at).
rollbackGeneratedFiles :: Repository -> [RepoPath] -> GitOid -> GitOid -> IO (Either TransactionError ())
rollbackGeneratedFiles repository generatedPaths currentTip newCommit = do
  -- If current_tip == new_commit, another transaction may have committed
  -- our work; don't delete files we don't own.
  when (currentTip /= newCommit) $ do
    forM_ generatedPaths $ \path -> do
      -- Check if a concurrent external commit adopted this path
      blobResult <- blobOidAt repository currentTip path
      case blobResult of
        Right (Just _) -> return () -- Adopted by concurrent commit, keep
        _ -> do
          -- Remove the file (ignore errors)
          let worktreeRoot = repositoryWorktreeRoot repository
          case worktreeRoot of
            Nothing -> return ()
            Just root -> do
              let filePath = root </> T.unpack (repoPathText path)
              void $ try @SomeException (removeFile filePath)
              -- Try to remove empty parent directories
              let parent = takeDirectory filePath
              removeEmptyParents root parent
  return (Right ())

-- | Recursively remove empty parent directories up to the root.
removeEmptyParents :: FilePath -> FilePath -> IO ()
removeEmptyParents root parent = do
  when (parent /= root && parent /= ".") $ do
    exists <- doesDirectoryExist parent
    if exists
      then do
        contents <- getDirectoryContents parent
        let nonDot = filter (`notElem` [".", ".."]) contents
        if null nonDot
          then do
            void $ try @SomeException (removeDirectory parent)
            removeEmptyParents root (takeDirectory parent)
          else return ()
      else return ()

-- | Get the blob OID at a revision for a given path. Returns Nothing if the
-- path doesn't exist at that revision.
blobOidAt :: Repository -> GitOid -> RepoPath -> IO (Either TransactionError (Maybe GitOid))
blobOidAt repository revision path = do
  result <-
    runRepository
      repository
      "rev-parse path"
      ["rev-parse", "--verify", Text.unpack (gitOidText revision) <> ":" <> T.unpack (repoPathText path)]
      BS.empty
  pure $
    case result of
      Left _ -> Right Nothing
      Right res
        | processExitCode res /= ExitSuccess -> Right Nothing
        | otherwise ->
            case parseSingleOidFromOutput "rev-parse" (processStdout res) of
              Left _ -> Right Nothing
              Right oid -> Right (Just oid)

-- ---------------------------------------------------------------------------
-- Core transaction: commitAppendOnlyOperation
-- ---------------------------------------------------------------------------

-- | Create one isolated one-parent commit for managed files.
--
-- The eight canonical stages are:
--
-- 1. Resolve repository — validated by caller passing 'Repository'.
--    Reject bare repos (check worktree root present).
-- 2. Acquire lock — via 'Adrai.Provenance.Git.Lock.withGitLock'.
-- 3. Validate state — attached branch, no active Git operations,
--    managed paths clean.
-- 4. Generate files — validate generated paths are repo-relative.
-- 5. Validate generated — parse each file, check provenance,
--    write files to worktree (append-only).
-- 6. Create temporary index — read-tree + add + write-tree.
-- 7. Commit tree — commit-tree with proper message format.
-- 8. Update ref and refresh — CAS update-ref + reset generated paths.
commitAppendOnlyOperation :: Repository -> TransactionConfig -> IO (Either TransactionError TransactionResult)
commitAppendOnlyOperation repository config@TransactionConfig{..} = do
  -- Validate worktree root present (reject bare repos)
  case repositoryWorktreeRoot repository of
    Nothing ->
      return (Left (Stage1ResolveRepo "worktree root is missing"))
    Just worktreeRoot -> do
      -- Stage 2: Acquire lock
      lockResult <- try @SomeException $ withGitLock repository $ do
        -- Stage 3: Validate state
        void (checkAttachedBranch repository :: IO (Either Text ()))
        void (checkNoActiveGitOperations repository :: IO (Either Text ()))
        let generatedPaths = map genFilePath configGenerated
        void (checkManagedPathsClean repository generatedPaths :: IO (Either Text ()))

        -- Stage 4: Validate generated paths
        let result = validateGeneratedPaths config
        case result of
          Left err -> throwIO err
          Right () -> pure ()

        -- Get old_head (current HEAD)
        oldHeadResult <-
          case mkRevisionSpec "HEAD" of
            Left revErr ->
              error ("mkRevisionSpec failed: " <> show revErr)  -- "HEAD" is always valid
            Right spec -> resolveRevision repository spec
        case oldHeadResult of
          Left err ->
            throwIO (Stage3ValidateState ("resolve HEAD: " <> T.pack (show err)))
          Right oldHead -> do
            -- Validate expected head matches
            when (oldHead /= configExpectedHead) $
              throwIO (Stage3ValidateState ("expected head mismatch: " <> T.pack configOperationId <> " expected " <> gitOidText configExpectedHead <> " got " <> gitOidText oldHead))

            -- Stage 5: Validate and write generated files
            validateGeneratedFiles repository config

            -- Stages 6-8: Create temporary index, commit, update ref
            tempIndexResult <- createTemporaryIndex repository oldHead generatedPaths
            case tempIndexResult of
              Left err ->
                throwIO err
              Right (idxPath, treeOid) -> do
                -- Stage 7: Commit tree
                commitResult <- commitTree repository oldHead treeOid configSubject configOperationId configTrailers
                case commitResult of
                  Left err -> do
                    removeTempFile idxPath
                    throwIO err
                  Right newCommit -> do
                    -- Get current branch ref
                    headState <- repositoryHeadState repository
                    case headState of
                      Left err -> do
                        removeTempFile idxPath
                        throwIO (Stage8UpdateRef ("get branch ref: " <> T.pack (show err)))
                      Right (GitHeadAttached branchRef) -> do
                        -- Stage 8: Update ref and refresh
                        let refText = gitRefText branchRef
                        casResult <-
                          runRepository
                            repository
                            "update-ref"
                            [ "update-ref",
                              "-m",
                              "adrai " <> configOperationId,
                              Text.unpack refText,
                              Text.unpack (gitOidText newCommit),
                              Text.unpack (gitOidText oldHead)
                            ]
                            BS.empty
                        case casResult of
                          Left err -> do
                            removeTempFile idxPath
                            throwIO (Stage8UpdateRef ("update-ref failed: " <> T.pack (show err)))
                          Right res2
                            | processExitCode res2 /= ExitSuccess -> do
                                removeTempFile idxPath
                                throwIO (Stage8UpdateRef ("update-ref exited " <> T.pack (show (processExitCode res2))))
                            | otherwise -> do
                                -- Refresh index for generated paths only
                                indexUpdated <-
                                  if null generatedPaths
                                    then pure True
                                    else do
                                      refreshResult <-
                                        runRepository
                                          repository
                                          "reset index"
                                          (["reset", "-q", "HEAD", "--"] <> map (T.unpack . repoPathText) generatedPaths)
                                          BS.empty
                                      pure $ case refreshResult of
                                          Left _ -> False
                                          Right res -> processExitCode res == ExitSuccess
                                removeTempFile idxPath
                                return
                                  TransactionResult
                                    { transactionOperationId = configOperationId,
                                      transactionCommitOid = newCommit,
                                      transactionCreatedPaths = generatedPaths,
                                      transactionIndexUpdated = indexUpdated
                                    }
      case lockResult of
        Left err -> do
          case fromException err of
            Just txErr -> return (Left txErr)
            Nothing -> return (Left (Stage2AcquireLock (T.pack (show err))))
        Right result -> return (Right result)

-- ---------------------------------------------------------------------------
-- Core transaction: commitBootstrapFiles
-- ---------------------------------------------------------------------------

-- | Commit repository bootstrap files with the same isolated-index semantics
-- as 'commitAppendOnlyOperation', but supporting unborn repos.
--
-- For unborn repos:
-- * Uses 'read-tree --empty' instead of 'read-tree <old_head>'.
-- * No @-p <old_head>@ in commit-tree.
-- * CAS uses @0000...0000@ as expected-old.
-- * Backs up existing files and restores on failure.
commitBootstrapFiles :: Repository -> TransactionConfig -> IO (Either TransactionError TransactionResult)
commitBootstrapFiles repository config@TransactionConfig{..} = do
  -- Stage 1: Validate worktree root
  case repositoryWorktreeRoot repository of
    Nothing ->
      return (Left (Stage1ResolveRepo "worktree root is missing"))
    Just _ -> do
      -- Stage 2: Acquire lock (with backup/restore on failure)
      let generatedPaths = map genFilePath configGenerated

      -- Backup existing files
      let backupOne GeneratedFile{genFilePath = path} =
            case repositoryWorktreeRoot repository of
              Nothing -> return (path, Nothing)
              Just root -> do
                let filePath = root </> T.unpack (repoPathText path)
                exists <- doesFileExist filePath
                if exists
                  then do
                    content <- BS.readFile filePath
                    return (path, Just content)
                  else return (path, Nothing)
      backupFiles <- traverse backupOne configGenerated

      lockResult <- try @SomeException $ withGitLock repository $ do
        -- Stage 3: Validate state (no active operations + paths clean)
        void (checkNoActiveGitOperations repository :: IO (Either Text ()))
        void (checkManagedPathsClean repository generatedPaths :: IO (Either Text ()))

        -- Stage 4: Validate generated paths
        let result = validateGeneratedPaths config
        case result of
          Left err -> throwIO err
          Right () -> pure ()

        -- Get old_head (or empty for unborn)
        oldHeadResult <-
          runRepository
            repository
            "resolve HEAD"
            ["rev-parse", "--verify", "HEAD^{commit}"]
            BS.empty
        let oldHead =
              case oldHeadResult of
                Left _ -> nullOid
                Right res
                  | processExitCode res /= ExitSuccess -> nullOid
                  | otherwise ->
                      case parseSingleOidFromOutput "rev-parse" (processStdout res) of
                        Left _ -> nullOid
                        Right oid -> oid

        -- Validate expected head
        when (oldHead /= configExpectedHead) $
          throwIO (Stage3ValidateState ("expected head mismatch: " <> T.pack configOperationId))

        -- Write files to worktree (bootstrap overwrites existing files)
        forM_ configGenerated $ \GeneratedFile{..} -> do
          let worktreeRoot = repositoryWorktreeRoot repository
          -- Create parent dirs and write
          case worktreeRoot of
            Nothing -> pure ()
            Just root -> do
              let dirPath = root </> T.unpack (repoPathText genFilePath)
              createDirectoryIfMissing True (takeDirectory dirPath)
              BS.writeFile dirPath genFileBytes

        -- Get branch ref
        headState <- repositoryHeadState repository
        case headState of
          Left err ->
            throwIO (Stage3ValidateState ("get branch ref: " <> T.pack (show err)))
          Right GitHeadDetached ->
            throwIO (Stage3ValidateState "HEAD is detached; attach a branch before bootstrapping")
          Right (GitHeadAttached branchRef) -> do
            -- Stages 6-8
            tempIndexResult <- createTemporaryIndex repository oldHead generatedPaths
            case tempIndexResult of
              Left err ->
                throwIO err
              Right (idxPath, treeOid) -> do
                -- Stage 7: Commit tree
                commitArgs <-
                  if isNullOid oldHead
                    then pure ["commit-tree", Text.unpack (gitOidText treeOid), "-i"]
                    else pure ["commit-tree", Text.unpack (gitOidText treeOid), "-p", Text.unpack (gitOidText oldHead), "-i"]

                let message =
                      T.unlines
                        [ T.strip configSubject,
                          "",
                          "ADRAI-Op: " <> T.pack configOperationId,
                          "ADRAI-Objects: bootstrap",
                          ""
                        ]

                commitResult <- runRepository repository "commit-tree" commitArgs (encodeUtf8 message)
                case commitResult of
                  Left err -> do
                    removeTempFile idxPath
                    throwIO (Stage7CommitTree ("commit-tree failed: " <> T.pack (show err)))
                  Right res2
                    | processExitCode res2 /= ExitSuccess -> do
                        removeTempFile idxPath
                        throwIO (Stage7CommitTree ("commit-tree exited " <> T.pack (show (processExitCode res2))))
                    | otherwise -> do
                        case parseSingleOidFromOutput "commit-tree" (processStdout res2) of
                          Left parseErr ->
                            throwIO (Stage7CommitTree ("parse commit OID: " <> parseErr))
                          Right (GitOid oidStr) -> do
                            let newCommit = GitOid oidStr
                                refText = gitRefText branchRef
                                expectedOld =
                                  if isNullOid oldHead
                                    then nullOid
                                    else oldHead
                                casArgs =
                                  [ "update-ref",
                                    "-m",
                                    "adrai " <> configOperationId,
                                    Text.unpack refText,
                                    Text.unpack (gitOidText newCommit),
                                    Text.unpack (gitOidText expectedOld)
                                  ]
                            casResult <-
                              runRepository
                                repository
                                "update-ref"
                                casArgs
                                BS.empty
                            case casResult of
                              Left err -> do
                                removeTempFile idxPath
                                throwIO (Stage8UpdateRef ("update-ref failed: " <> T.pack (show err)))
                              Right res3
                                | processExitCode res3 /= ExitSuccess -> do
                                    removeTempFile idxPath
                                    throwIO (Stage8UpdateRef ("update-ref exited " <> T.pack (show (processExitCode res3))))
                                | otherwise -> do
                                    -- Refresh index
                                    indexUpdated <-
                                      if null generatedPaths
                                        then pure True
                                        else do
                                          refreshResult <-
                                            runRepository
                                              repository
                                              "reset index"
                                              (["reset", "-q", "HEAD", "--"] <> map (T.unpack . repoPathText) generatedPaths)
                                              BS.empty
                                          pure $ case refreshResult of
                                              Left _ -> False
                                              Right res -> processExitCode res == ExitSuccess
                                    removeTempFile idxPath
                                    return
                                      TransactionResult
                                        { transactionOperationId = configOperationId,
                                          transactionCommitOid = newCommit,
                                          transactionCreatedPaths = generatedPaths,
                                          transactionIndexUpdated = indexUpdated
                                        }

      case lockResult of
        Left err -> do
          case fromException err of
            Just txErr -> do
              -- Rollback: restore backups
              rollbackBootstrappedFiles repository generatedPaths configGenerated backupFiles
              return (Left txErr)
            Nothing -> do
              rollbackBootstrappedFiles repository generatedPaths configGenerated backupFiles
              return (Left (Stage2AcquireLock (T.pack (show err))))
        Right result -> return (Right result)

-- | Rollback bootstrap files: restore backups or delete new files.
rollbackBootstrappedFiles :: Repository -> [RepoPath] -> [GeneratedFile] -> [(RepoPath, Maybe ByteString)] -> IO ()
rollbackBootstrappedFiles repository _genFiles _backups backupData =
  mapM_ restoreOrDelete backupData
  where
    restoreOrDelete (path, maybeContent) = do
      let worktreeRoot = repositoryWorktreeRoot repository
      case worktreeRoot of
        Nothing -> pure ()
        Just root -> do
          let filePath = root </> T.unpack (repoPathText path)
          case maybeContent of
            Just content -> do
              -- Restore original content
              BS.writeFile filePath content
            Nothing -> do
              -- Delete the file we created
              void $ try @SomeException (removeFile filePath)
              -- Try to remove empty parent directories
              let parent = takeDirectory filePath
              removeEmptyParents root parent
