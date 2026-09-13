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
    ExpectedRepositoryBasis (..),
    AppendOnlyDependencies (..),
    defaultAppendOnlyDependencies,
    AppendOnlyTestHooks (..),
    defaultAppendOnlyTestHooks,
    BootstrapDependencies (..),
    defaultBootstrapDependencies,
    -- * Null OID constant
    nullOid,
    -- * Git plumbing output
    parseSingleOidFromOutput,
    commitTree,
    -- * Core transaction functions
    commitAppendOnlyOperation,
    commitAppendOnlyOperationChecked,
    commitAppendOnlyOperationCheckedPublishing,
    commitAppendOnlyOperationCheckedPublishingWithHooks,
    commitAppendOnlyOperationWith,
    commitAppendOnlyOperationCheckedWith,
    commitAppendOnlyOperationWithHooks,
    commitAppendOnlyOperationCheckedWithHooks,
    commitBootstrapFiles,
    commitBootstrapFilesWith,
  )
where

import Adrai.Git
  ( Repository (..),
    GitOid (..),
    gitOidText,
    runRepository,
    runRepositoryWithEnvironment,
    GitProcessResult (..),
     repositoryCommonDir,
     repositoryGitDir,
    repositoryWorktreeRoot,
    resolveRevision,
    GitHeadState (..),
    repositoryHeadState,
    mkRevisionSpec,
  )
import Adrai.Provenance
  ( mkGitOid,
    provenanceOperationId,
    provenanceBasis,
    provenanceActor,
    provenanceTimestampMs,
    provenanceObjectId,
    provenanceObjectIdText,
  )
import Adrai.Provenance.Git.Lock
  ( withGitLock,
  )
import Adrai.ManagedPath
  ( ManagedPathError (..),
    resolveManagedWritePath,
  )
import Adrai.Types
  ( RepoPath,
    repoPathText,
    GitRef (..),
    operationIdText,
    actorId,
    gitRefText,
  )
import Adrai.Format.Document
  ( ParsedManagedDocument (..),
    parseManagedDocument,
  )
import qualified Data.Set as Set

import Control.Exception
  ( Exception,
    SomeException,
    SomeAsyncException,
     fromException,
     toException,
    onException,
    throwIO,
     try,
     mask,
     uninterruptibleMask_,
  )
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import Control.Monad (when, void, unless, forM, forM_, filterM)
import Data.Char (toLower)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.IORef (IORef, newIORef, modifyIORef', readIORef, writeIORef)
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
    pathIsSymbolicLink,
  )
import System.FilePath
  ( (</>),
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
  , transactionPublicationError :: Maybe Text
  }
  deriving (Eq, Show)

data AppendOnlyCompletion
  = AppendOnlyComplete TransactionResult
  | AppendOnlyPrepared GitOid

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

-- | Exact repository state captured by a caller before constructing a
-- mutation.  Checked entry points compare both the attached/detached HEAD
-- identity and the resolved commit while holding the shared Git lock.
data ExpectedRepositoryBasis = ExpectedRepositoryBasis
  { expectedBasisHead :: GitOid,
    expectedBasisHeadState :: GitHeadState
  }
  deriving (Eq, Show)

-- | The caller-visible state which an append-only transaction may touch before
-- its ref CAS becomes authoritative.  Keeping this as bytes (rather than
-- asking Git to reconstruct the index) is deliberate: a caller's index is
-- allowed to contain staged binary content which must survive a failed ADRAI
-- operation byte-for-byte.
data AppendOnlySnapshot = AppendOnlySnapshot
  { snapshotTargetRef :: GitRef,
    snapshotIndexBytes :: Maybe ByteString,
    snapshotGeneratedFiles :: [(GeneratedFile, Maybe ByteString)],
    snapshotOwnedDirectories :: [FilePath]
  }

data AppendOnlyDependencies = AppendOnlyDependencies
  { appendOnlyInspectRef :: Repository -> GitRef -> GitOid -> Maybe GitOid -> IO (Either TransactionError GitOid),
    appendOnlyBeforeSnapshot :: IO (),
    appendOnlyWriteGeneratedFile :: FilePath -> ByteString -> IO (),
    appendOnlyAfterGeneratedFileWrite :: GeneratedFile -> IO (),
    appendOnlyAfterGeneratedWrite :: IO (),
    appendOnlyBeforeRollbackCleanup :: IO (),
    appendOnlyBeforeTempIndexCleanup :: IO (),
     appendOnlyAfterTempIndexCleanup :: IO (),
     appendOnlyAfterSuccessfulCas :: IO ()
  }

defaultAppendOnlyDependencies :: AppendOnlyDependencies
defaultAppendOnlyDependencies =
  AppendOnlyDependencies
    { appendOnlyInspectRef = \repository ref _ _ -> branchTip repository ref,
      appendOnlyBeforeSnapshot = pure (),
      appendOnlyWriteGeneratedFile = BS.writeFile,
      appendOnlyAfterGeneratedFileWrite = \_ -> pure (),
      appendOnlyAfterGeneratedWrite = pure (),
      appendOnlyBeforeRollbackCleanup = pure (),
      appendOnlyBeforeTempIndexCleanup = pure (),
      appendOnlyAfterTempIndexCleanup = pure (),
      appendOnlyAfterSuccessfulCas = pure ()
    }

-- | Additive, timing-specific containment hooks used only by transaction
-- tests.  They intentionally do not alter the long-standing public
-- 'AppendOnlyDependencies' constructor.
data AppendOnlyTestHooks = AppendOnlyTestHooks
  { appendOnlyAfterLockAcquiredHook :: IO (),
    appendOnlyAfterGeneratedParentCreationHook :: GeneratedFile -> IO (),
    appendOnlyBeforeRefUpdateHook :: IO (),
    appendOnlyBeforePostCasIndexRefreshHook :: IO ()
  }

defaultAppendOnlyTestHooks :: AppendOnlyTestHooks
defaultAppendOnlyTestHooks = AppendOnlyTestHooks (pure ()) (\_ -> pure ()) (pure ()) (pure ())

-- | Narrow test seam for the bootstrap write/rollback boundaries. Production
-- uses 'defaultBootstrapDependencies'; it has no observable effect there.
data BootstrapDependencies = BootstrapDependencies
  { bootstrapAfterGeneratedParentCreation :: GeneratedFile -> IO (),
    bootstrapAfterGeneratedFileWrite :: GeneratedFile -> IO (),
    bootstrapBeforeRefUpdate :: IO (),
    bootstrapAfterCasCandidateBeforeUpdateRef :: IO (),
    bootstrapAfterSuccessfulCasBeforeBookkeeping :: IO (),
    bootstrapInspectCasRef :: Repository -> GitRef -> IO (Either TransactionError (Maybe GitOid)),
    bootstrapBeforePostCasIndexRefresh :: IO (),
    bootstrapBeforeRollbackCleanup :: IO (),
    bootstrapDeleteGeneratedFile :: FilePath -> IO ()
  }

defaultBootstrapDependencies :: BootstrapDependencies
defaultBootstrapDependencies =
  BootstrapDependencies
    { bootstrapAfterGeneratedParentCreation = \_ -> pure (),
      bootstrapAfterGeneratedFileWrite = \_ -> pure (),
      bootstrapBeforeRefUpdate = pure (),
      bootstrapAfterCasCandidateBeforeUpdateRef = pure (),
      bootstrapAfterSuccessfulCasBeforeBookkeeping = pure (),
      bootstrapInspectCasRef = inspectBootstrapRef,
      bootstrapBeforePostCasIndexRefresh = pure (),
      bootstrapBeforeRollbackCleanup = pure (),
      bootstrapDeleteGeneratedFile = removeFile
    }

-- | Resolve every generated destination through the single physical
-- containment authority.  Append-only records require a fresh immutable leaf;
-- bootstrap may restore an existing regular leaf, but only after the managed
-- resolver has verified every parent and the leaf has been rejected as neither
-- a redirect nor a directory.
resolveGeneratedDestination :: Repository -> Bool -> GeneratedFile -> IO (Either TransactionError FilePath)
resolveGeneratedDestination repository allowExisting GeneratedFile{..} =
  case repositoryWorktreeRoot repository of
    Nothing -> pure (Left (Stage1ResolveRepo "worktree root is missing"))
    Just root ->
      resolveManagedWritePath root genFilePath >>= \case
        Right path -> pure (Right path)
        Left (ManagedPathLeafExists path)
          | allowExisting -> do
              redirected <- pathIsSymbolicLink path
              directory <- doesDirectoryExist path
              regular <- doesFileExist path
              pure $
                if redirected
                  then Left (managedDestinationFailure genFilePath (ManagedPathRedirected path))
                  else
                    if directory
                      then Left (managedDestinationFailure genFilePath (ManagedPathLeafIsDirectory path))
                      else
                        if regular
                          then Right path
                          else Left (managedDestinationFailure genFilePath (ManagedPathAncestorNotDirectory path))
        Left problem -> pure (Left (managedDestinationFailure genFilePath problem))

resolveGeneratedDestinations :: Repository -> Bool -> [GeneratedFile] -> IO (Either TransactionError [(GeneratedFile, FilePath)])
resolveGeneratedDestinations repository allowExisting generated = do
  resolved <- traverse resolveOne generated
  pure (sequence resolved)
  where
    resolveOne generatedFile = do
      destination <- resolveGeneratedDestination repository allowExisting generatedFile
      pure (fmap (\path -> (generatedFile, path)) destination)

managedDestinationFailure :: RepoPath -> ManagedPathError -> TransactionError
managedDestinationFailure path problem =
  Stage5ValidateGenerated ("managed destination rejected for " <> repoPathText path <> ": " <> T.pack (show problem))

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

-- | Parse provenance fields from a managed document's provenance capsule.
-- Returns (operationId, basis/parent, actor, timestamp, objectId).
extractProvenanceFields :: ParsedManagedDocument -> (Text, GitOid, Text, Integer, Text)
extractProvenanceFields doc =
  let capsule = parsedManagedCapsule doc
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
  -- Porcelain diff independently refreshes stat-only matches by default, so
  -- disable that behavior as well as optional locks before the byte snapshot.
  unmergedResult <-
    runRepository
      repository
      "check unmerged"
      ["--no-optional-locks", "-c", "diff.autoRefreshIndex=false", "diff", "--name-only", "--diff-filter=U"]
      BS.empty
  case unmergedResult of
    Left err -> pure (Left ("diff unmerged failed: " <> T.pack (show err)))
    Right result
      | processExitCode result /= ExitSuccess ->
          pure (Left ("diff unmerged exited " <> T.pack (show (processExitCode result))))
      | otherwise -> do
          -- Check for active operation markers in the common dir
          let commonDir = repositoryCommonDir repository
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
  -- Status may otherwise take an optional index lock solely to refresh stat
  -- metadata, changing caller bytes during an intended read-only preflight.
  result <-
    runRepository
      repository
      "status check"
      ["--no-optional-locks", "status", "--porcelain=v1", "--untracked-files=all"]
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
validateGeneratedFiles :: AppendOnlyDependencies -> AppendOnlyTestHooks -> Repository -> TransactionConfig -> IORef [GeneratedFile] -> IO (Either TransactionError ())
validateGeneratedFiles dependencies hooks repository TransactionConfig{..} writtenFilesRef = do
  let genFiles = configGenerated
  when (null genFiles) $
    throwIO (Stage5ValidateGenerated "an operation must create at least one file")

  -- Accumulate validation state using mutable references
  objectMapRef <- newIORef Map.empty
  timestampsRef <- newIORef Set.empty
  actorsRef <- newIORef Set.empty
  pathErrorsRef <- newIORef []

  forM_ genFiles $ \GeneratedFile{..} -> do
    -- Parse the document to extract the provenance capsule
    parsedResult <-
      try @SomeException (pure (parseManagedDocument genFilePath genFileBytes))
    case parsedResult of
      Left parseErr -> do
        modifyIORef' pathErrorsRef ((genFilePath, "parse error: " <> T.pack (show parseErr)) :)
        return ()
      Right (Left parseErr) ->
        modifyIORef' pathErrorsRef ((genFilePath, "parse error: " <> T.pack (show parseErr)) :)
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
    resolved <- resolveGeneratedDestination repository False (GeneratedFile genFilePath genFileBytes)
    case resolved of
      Left err -> throwIO err
      Right path -> do
        -- Ownership is recorded before the unmasked write.  That lets rollback
        -- remove a prefix left by a failed/truncated write, while the snapshot
        -- still proves that no caller file existed before we touched the path.
        mask $ \restore -> do
           exists <- doesFileExist path
           when exists $
             throwIO (Stage5ValidateGenerated (T.pack "append-only path already exists: " <> p))
           createDirectoryIfMissing True (takeDirectory path)
           appendOnlyAfterGeneratedParentCreationHook hooks (GeneratedFile genFilePath genFileBytes)
           resolvedAfterParentCreation <- resolveGeneratedDestination repository False (GeneratedFile genFilePath genFileBytes)
           writePath <- either throwIO pure resolvedAfterParentCreation
           writeExists <- doesFileExist writePath
           when writeExists $
             throwIO (Stage5ValidateGenerated (T.pack "append-only path already exists after parent creation: " <> p))
           modifyIORef' writtenFilesRef (GeneratedFile genFilePath genFileBytes :)
           restore (appendOnlyWriteGeneratedFile dependencies writePath genFileBytes)
           appendOnlyAfterGeneratedFileWrite dependencies (GeneratedFile genFilePath genFileBytes)

  return (Right ())

-- ---------------------------------------------------------------------------
-- Stage 6: Create temporary index
-- ---------------------------------------------------------------------------

-- | Stage 6: Create a temporary index from the old head, add generated paths,
-- and write the resulting tree.
createTemporaryIndex :: IO () -> IO () -> Repository -> GitOid -> [RepoPath] -> IO (Either TransactionError GitOid)
createTemporaryIndex beforeCleanupHook cleanupHook repository oldHead generatedPaths = do
  contained <- resolveGeneratedDestinations repository True (map (\path -> GeneratedFile path BS.empty) generatedPaths)
  case contained of
    Left err -> pure (Left err)
    Right _ -> createTemporaryIndexContained beforeCleanupHook cleanupHook repository oldHead generatedPaths

createTemporaryIndexContained :: IO () -> IO () -> Repository -> GitOid -> [RepoPath] -> IO (Either TransactionError GitOid)
createTemporaryIndexContained beforeCleanupHook cleanupHook repository oldHead generatedPaths = mask $ \restore -> do
  let commonDir = repositoryCommonDir repository
  (idxPath, idxHandle) <- openTempFile commonDir "adrai-index-"
  hClose idxHandle `onException` removeFile idxPath
  outcome <- try @SomeException (restore (buildTemporaryTree idxPath))
  -- Allow a cancellation exactly at the cleanup boundary, but catch it before
  -- proceeding to the masked deletion.  This prevents a leaked temp index.
  beforeCleanup <- try @SomeException (restore beforeCleanupHook)
  cleanup <- try @SomeException (removeFile idxPath >> cleanupHook)
  case firstFound (asyncFrom outcome) (firstFound (asyncFrom beforeCleanup) (asyncFrom cleanup)) of
    Just asyncFailure -> throwIO asyncFailure
    Nothing ->
      case (outcome, firstSyncFailure beforeCleanup cleanup) of
        (Right result, Nothing) -> pure result
        (Left originalFailure, Nothing) -> throwIO originalFailure
        (Right _, Just cleanupFailure) ->
          throwIO (RollbackFailed ("temporary index cleanup failed: " <> T.pack (show cleanupFailure)))
        (Left originalFailure, Just cleanupFailure) ->
          throwIO
            ( RollbackFailed
                ( "transaction failed " <> T.pack (show originalFailure)
                    <> "; temporary index cleanup also failed: " <> T.pack (show cleanupFailure)
                )
            )
  where
    asyncFrom :: Either SomeException a -> Maybe SomeAsyncException
    asyncFrom = either (fromException :: SomeException -> Maybe SomeAsyncException) (const Nothing)

    firstFound :: Maybe a -> Maybe a -> Maybe a
    firstFound (Just value) _ = Just value
    firstFound Nothing fallback = fallback

    firstSyncFailure :: Either SomeException a -> Either SomeException b -> Maybe SomeException
    firstSyncFailure firstResult secondResult =
      case firstResult of
        Left err -> Just err
        Right _ -> case secondResult of
          Left err -> Just err
          Right _ -> Nothing

    buildTemporaryTree idxPath = do
      let env = Map.singleton "GIT_INDEX_FILE" idxPath
          gitCmd args =
            runRepositoryWithEnvironment repository env ("temp-index " <> T.pack (show args)) args BS.empty
      readTreeResult <-
        if isNullOid oldHead
          then gitCmd ["read-tree", "--empty"]
          else gitCmd ["read-tree", Text.unpack (gitOidText oldHead)]
      case readTreeResult of
        Left err -> throwIO (Stage6CreateTemporaryIndex ("read-tree failed: " <> T.pack (show err)))
        Right res
          | processExitCode res /= ExitSuccess ->
              throwIO (Stage6CreateTemporaryIndex ("read-tree exited " <> T.pack (show (processExitCode res))))
          | otherwise -> do
              addPathsResult <- gitCmd (["add", "--sparse", "--"] <> map (T.unpack . repoPathText) generatedPaths)
              case addPathsResult of
                Left err -> throwIO (Stage6CreateTemporaryIndex ("add paths failed: " <> T.pack (show err)))
                Right res2
                  | processExitCode res2 /= ExitSuccess ->
                      throwIO (Stage6CreateTemporaryIndex ("add paths exited " <> T.pack (show (processExitCode res2))))
                  | otherwise -> do
                      treeResult <- gitCmd ["write-tree"]
                      case treeResult of
                        Left err -> throwIO (Stage6CreateTemporaryIndex ("write-tree failed: " <> T.pack (show err)))
                        Right res3
                          | processExitCode res3 /= ExitSuccess ->
                              throwIO (Stage6CreateTemporaryIndex ("write-tree exited " <> T.pack (show (processExitCode res3))))
                          | otherwise ->
                              case parseSingleOidFromOutput "write-tree" (processStdout res3) of
                                Left parseErr -> throwIO (Stage6CreateTemporaryIndex ("parse tree OID: " <> parseErr))
                                Right treeOid -> pure (Right treeOid)

parseSingleOidFromOutput :: Text -> ByteString -> Either Text GitOid
parseSingleOidFromOutput operation raw = do
  let body = stripTrailingWhitespace raw
  first
    (const ("invalid bare OID output from " <> operation <> ": " <> boundedOutput raw))
    (mkGitOid (normalizeAsciiHex (TE.decodeUtf8With lenientDecode body)))

stripTrailingWhitespace :: ByteString -> ByteString
stripTrailingWhitespace bs =
  case BS.unsnoc (BS.dropWhileEnd (== 32) bs) of
    Just (withoutLf, 10) ->
      case BS.unsnoc withoutLf of
        Just (withoutCr, 13) -> withoutCr
        _ -> withoutLf
    _ -> BS.dropWhileEnd (== 32) bs

normalizeAsciiHex :: Text -> Text
normalizeAsciiHex = T.map normalize
  where
    normalize character
      | character >= 'A' && character <= 'F' = toLower character
      | otherwise = character

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
      then pure ["commit-tree", Text.unpack (gitOidText treeOid)]
      else pure ["commit-tree", Text.unpack (gitOidText treeOid), "-p", Text.unpack (gitOidText oldHead)]

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

-- | Bootstrap cleanup resolves the managed destination immediately before
-- every directory removal.  A late junction swap therefore becomes a typed
-- rollback failure instead of a write outside the worktree.
removeEmptyParentsContained :: Repository -> GeneratedFile -> FilePath -> FilePath -> IO ()
removeEmptyParentsContained repository generatedFile root parent = do
  when (parent /= root && parent /= ".") $ do
    exists <- doesDirectoryExist parent
    if exists
      then do
        contents <- getDirectoryContents parent
        let nonDot = filter (`notElem` [".", ".."]) contents
        when (null nonDot) $ do
          resolved <- resolveGeneratedDestination repository True generatedFile
          void (either (throwIO . rollbackDestinationFailure) pure resolved)
          removeDirectory parent
          removeEmptyParentsContained repository generatedFile root (takeDirectory parent)
      else pure ()

-- | Capture the only caller-visible inputs that append-only work may touch
-- before a successful ref CAS.  The generated paths are normally absent, but
-- retaining the complete preimage makes cleanup fail closed if that invariant
-- ever changes.
captureAppendOnlySnapshot :: AppendOnlyDependencies -> Repository -> GitRef -> [GeneratedFile] -> IO (Either TransactionError AppendOnlySnapshot)
captureAppendOnlySnapshot dependencies repository targetRef generated =
  case repositoryWorktreeRoot repository of
    Nothing -> pure (Left (RollbackFailed "worktree root disappeared before rollback snapshot"))
    Just root -> mask $ \restore -> do
      result <- try @SomeException $ restore $ do
        appendOnlyBeforeSnapshot dependencies
        indexBytes <- readOptionalFile (repositoryGitDir repository </> "index")
        fileBytes <-
          forM generated $ \generatedFile -> do
            resolved <- resolveGeneratedDestination repository False generatedFile
            destination <- either throwIO pure resolved
            before <- readOptionalFile destination
            pure (generatedFile, before)
        ownedDirectories <- concat <$> mapM (missingParentDirectories root . genFilePath) generated
        pure (AppendOnlySnapshot targetRef indexBytes fileBytes ownedDirectories)
      case result of
        Left err ->
          case fromException err :: Maybe SomeAsyncException of
            Just asyncFailure -> throwIO asyncFailure
            Nothing ->
              case fromException err of
                Just transactionError -> pure (Left transactionError)
                Nothing -> pure (Left (RollbackFailed ("capture rollback snapshot failed: " <> T.pack (show err))))
        Right snapshot -> pure (Right snapshot)

readOptionalFile :: FilePath -> IO (Maybe ByteString)
readOptionalFile path = do
  exists <- doesFileExist path
  if exists then Just <$> BS.readFile path else pure Nothing

-- | Directories missing at capture time are the only directories this
-- transaction can own.  The list is deliberately deepest-first for cleanup.
missingParentDirectories :: FilePath -> RepoPath -> IO [FilePath]
missingParentDirectories root repoPath = go (takeDirectory (root </> T.unpack (repoPathText repoPath)))
  where
    go directory
      | directory == root || directory == "." = pure []
      | otherwise = do
          exists <- doesDirectoryExist directory
          if exists
            then pure []
            else (directory :) <$> go (takeDirectory directory)

-- | Reconcile a failed append-only operation.  Once a commit OID exists, a
-- failed update-ref is ambiguous: never touch files or the index until the
-- branch is proven to remain at its original tip.  If the branch names the new
-- commit, it is authoritative and there is deliberately nothing to roll back.
rollbackAppendOnlyFailure :: AppendOnlyDependencies -> Repository -> GitOid -> Maybe GitOid -> AppendOnlySnapshot -> [GeneratedFile] -> IO (Either TransactionError ())
rollbackAppendOnlyFailure dependencies repository expectedOld maybeNew snapshot writtenFiles = do
  currentTip <- appendOnlyInspectRef dependencies repository (snapshotTargetRef snapshot) expectedOld maybeNew
  let safeToRestore = do
        tip <- currentTip
        case maybeNew of
          Just newCommit | tip == newCommit -> Right False
          _ | tip == expectedOld -> Right True
          _ -> Left (RollbackFailed "target ref changed or cannot be confirmed after failure; preserving generated files")
  case safeToRestore of
    Left err -> pure (Left err)
    Right False -> pure (Right ())
    Right True -> restoreAppendOnlySnapshot dependencies repository snapshot writtenFiles

branchTip :: Repository -> GitRef -> IO (Either TransactionError GitOid)
branchTip repository branchRef = do
  result <-
    runRepository repository "rollback rev-parse ref"
      ["rev-parse", "--verify", Text.unpack (gitRefText branchRef)] BS.empty
  pure $ case result of
    Left err -> Left (RollbackFailed ("inspect target ref after failure: " <> T.pack (show err)))
    Right res
      | processExitCode res /= ExitSuccess -> Left (RollbackFailed ("inspect target ref after failure exited " <> T.pack (show (processExitCode res))))
      | otherwise -> first (const (RollbackFailed "inspect target ref after failure returned an invalid OID")) $
           parseSingleOidFromOutput "rollback rev-parse" (processStdout res)

-- | Inspect the ref pinned for bootstrap CAS while rollback is still possible.
-- The candidate is recorded before @update-ref@, so cancellation in the small
-- gap after Git publishes the ref cannot be mistaken for an unpublished CAS.
inspectBootstrapRef :: Repository -> GitRef -> IO (Either TransactionError (Maybe GitOid))
inspectBootstrapRef repository branchRef = do
  presence <- runRepository repository "inspect bootstrap CAS ref" ["show-ref", "--verify", "--quiet", Text.unpack (gitRefText branchRef)] BS.empty
  case presence of
    Left err -> pure (Left (Stage8UpdateRef ("bootstrap CAS ref inspection failed: " <> T.pack (show err))))
    Right res ->
      case processExitCode res of
        ExitSuccess -> fmap Just <$> branchTip repository branchRef
        ExitFailure 1 -> pure (Right Nothing)
        exitCode -> pure (Left (Stage8UpdateRef ("bootstrap CAS ref inspection exited " <> T.pack (show exitCode))))

inspectBootstrapCasCandidate :: BootstrapDependencies -> Repository -> (GitRef, GitOid, GitOid) -> IO (Either TransactionError Bool)
inspectBootstrapCasCandidate dependencies repository (branchRef, oldCommit, newCommit) =
  uninterruptibleMask_ $ do
    observed <- bootstrapInspectCasRef dependencies repository branchRef
    pure $ case observed of
      Right (Just tip)
        | tip == newCommit -> Right True
        | tip == oldCommit -> Right False
        | otherwise ->
            Left
              ( Stage8UpdateRef
                  ( "bootstrap CAS ref changed unexpectedly: expected "
                      <> gitOidText oldCommit
                      <> " or "
                      <> gitOidText newCommit
                      <> ", got "
                      <> gitOidText tip
                  )
              )
      Right Nothing | isNullOid oldCommit -> Right False
      Right Nothing -> Left (Stage8UpdateRef "bootstrap CAS ref is unexpectedly absent")
      Left err -> Left err

restoreAppendOnlySnapshot :: AppendOnlyDependencies -> Repository -> AppendOnlySnapshot -> [GeneratedFile] -> IO (Either TransactionError ())
restoreAppendOnlySnapshot dependencies repository AppendOnlySnapshot{..} writtenFiles = mask $ \restore -> do
  -- Permit a cancellation at this deterministic boundary, then complete all
  -- actual restoration while masked.  An async exception is rethrown only
  -- after the owned bytes and directories have been reconciled.
  cancellation <- try @SomeException (restore (appendOnlyBeforeRollbackCleanup dependencies))
  result <- try @SomeException $ do
    -- Pre-CAS stages must not alter the real index.  Treat any change as
    -- externally owned rather than overwriting it during rollback.
    actualIndex <- readOptionalFile (repositoryGitDir repository </> "index")
    unless (actualIndex == snapshotIndexBytes) $
      throwIO (RollbackFailed "caller index changed during failed transaction; refusing to overwrite it")
    case repositoryWorktreeRoot repository of
      Nothing -> throwIO (RollbackFailed "worktree root disappeared during rollback")
      Just root -> do
        let writtenPaths = Set.fromList (map genFilePath writtenFiles)
            writtenSnapshots = filter (\(file, _) -> genFilePath file `Set.member` writtenPaths) snapshotGeneratedFiles
        forM_ writtenSnapshots (restoreGeneratedFile repository)
        -- Resolve immediately before each directory removal. A changed link
        -- therefore fails closed instead of directing cleanup elsewhere.
        forM_ snapshotOwnedDirectories $ \directory -> do
          contained <- resolveGeneratedDestinations repository False (map fst snapshotGeneratedFiles)
          void (either throwIO pure contained)
          removeOwnedDirectory root directory
  -- Cancellation from either phase outranks every synchronous cleanup result.
  -- Both phases have already run under the surrounding mask, so choosing the
  -- async outcome here cannot strand a partially cleaned transaction.
  case (asyncOutcome result, asyncOutcome cancellation) of
    (Just asyncFailure, _) -> throwIO asyncFailure
    (_, Just asyncFailure) -> throwIO asyncFailure
    (Nothing, Nothing) ->
      case result of
        Left err ->
          case fromException err of
            Just transactionError -> pure (Left transactionError)
            Nothing -> pure (Left (RollbackFailed ("rollback cleanup failed: " <> T.pack (show err))))
        Right () ->
          case cancellation of
            Left err -> pure (Left (RollbackFailed ("rollback cleanup hook failed: " <> T.pack (show err))))
            Right () -> pure (Right ())

restoreGeneratedFile :: Repository -> (GeneratedFile, Maybe ByteString) -> IO ()
restoreGeneratedFile repository (generatedFile@GeneratedFile{..}, before) = do
  resolved <- resolveGeneratedDestination repository True generatedFile
  path <-
    case resolved of
      Left err -> throwIO (RollbackFailed ("managed rollback destination rejected: " <> T.pack (show err)))
      Right destination -> pure destination
  current <- readOptionalFile path
  case (before, current) of
    (Nothing, Nothing) -> pure ()
    (Nothing, Just currentBytes)
      -- A failed write can leave any prefix (including an empty truncation) of
      -- our bytes.  The preimage was absent, so only that recognizable partial
      -- output is owned by this transaction.
      | currentBytes `BS.isPrefixOf` genFileBytes -> do
          resolvedBeforeDelete <- resolveGeneratedDestination repository True generatedFile
          deletePath <- either (throwIO . rollbackDestinationFailure) pure resolvedBeforeDelete
          removeFile deletePath
      | otherwise -> throwIO (RollbackFailed ("generated path changed externally; refusing to delete " <> repoPathText genFilePath))
    (Just originalBytes, Just currentBytes)
      | currentBytes == originalBytes -> pure ()
      | currentBytes == genFileBytes -> do
          resolvedBeforeRestore <- resolveGeneratedDestination repository True generatedFile
          restorePath <- either (throwIO . rollbackDestinationFailure) pure resolvedBeforeRestore
          BS.writeFile restorePath originalBytes
      | otherwise -> throwIO (RollbackFailed ("generated path changed externally; refusing to restore " <> repoPathText genFilePath))
    (Just _, Nothing) -> throwIO (RollbackFailed ("pre-existing generated path disappeared; refusing to recreate " <> repoPathText genFilePath))

rollbackDestinationFailure :: TransactionError -> TransactionError
rollbackDestinationFailure err = RollbackFailed ("managed rollback destination rejected: " <> T.pack (show err))

asyncOutcome :: Either SomeException a -> Maybe SomeAsyncException
asyncOutcome = either fromException (const Nothing)

-- | Complete rollback, but preserve cancellation precedence. A synchronous
-- rollback failure becomes the transaction error because restoration is no
-- longer trustworthy; an original async exception is never masked by cleanup.
rethrowAfterRollback :: SomeException -> IO (Either TransactionError ()) -> IO a
rethrowAfterRollback originalFailure cleanup = do
  cleanupAttempt <- try @SomeException cleanup
  case fromException originalFailure :: Maybe SomeAsyncException of
    Just asyncFailure -> throwIO asyncFailure
    Nothing ->
      case cleanupAttempt of
        Left cleanupException ->
          case fromException cleanupException :: Maybe SomeAsyncException of
            Just asyncFailure -> throwIO asyncFailure
            Nothing ->
              case fromException cleanupException :: Maybe TransactionError of
                Just transactionFailure -> throwIO transactionFailure
                Nothing ->
                  throwIO
                    ( RollbackFailed
                        ( "rollback cleanup failed after "
                            <> T.pack (show originalFailure)
                            <> ": "
                            <> T.pack (show cleanupException)
                        )
                    )
        Right (Left cleanupFailure) -> throwIO cleanupFailure
        Right (Right ()) -> throwIO originalFailure

removeOwnedDirectory :: FilePath -> FilePath -> IO ()
removeOwnedDirectory root directory
  | directory == root || directory == "." = pure ()
  | otherwise = do
      exists <- doesDirectoryExist directory
      when exists $ do
        contents <- getDirectoryContents directory
        let nonDot = filter (`notElem` [".", ".."]) contents
        when (null nonDot) $ removeDirectory directory

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
commitAppendOnlyOperation = commitAppendOnlyOperationWith defaultAppendOnlyDependencies

commitAppendOnlyOperationChecked :: ExpectedRepositoryBasis -> Repository -> TransactionConfig -> IO (Either TransactionError TransactionResult)
commitAppendOnlyOperationChecked basis = commitAppendOnlyOperationCheckedWith defaultAppendOnlyDependencies basis

-- | Checked service variant which publishes the authoritative commit while
-- the shared Git lock remains held.  A synchronous publication failure is
-- recorded on the durable result; asynchronous cancellation is rethrown only
-- after the transaction has completed its finite committed cleanup.
commitAppendOnlyOperationCheckedPublishing :: (GitOid -> IO ()) -> ExpectedRepositoryBasis -> Repository -> TransactionConfig -> IO (Either TransactionError TransactionResult)
commitAppendOnlyOperationCheckedPublishing publish basis repository config =
  commitAppendOnlyOperationMaybeCheckedWithHooks defaultAppendOnlyDependencies defaultAppendOnlyTestHooks (Just basis) (Just publish) repository config

commitAppendOnlyOperationCheckedPublishingWithHooks :: AppendOnlyTestHooks -> (GitOid -> IO ()) -> ExpectedRepositoryBasis -> Repository -> TransactionConfig -> IO (Either TransactionError TransactionResult)
commitAppendOnlyOperationCheckedPublishingWithHooks hooks publish basis repository config =
  commitAppendOnlyOperationMaybeCheckedWithHooks defaultAppendOnlyDependencies hooks (Just basis) (Just publish) repository config

commitAppendOnlyOperationWith :: AppendOnlyDependencies -> Repository -> TransactionConfig -> IO (Either TransactionError TransactionResult)
commitAppendOnlyOperationWith dependencies repository config =
  commitAppendOnlyOperationWithHooks dependencies defaultAppendOnlyTestHooks repository config

commitAppendOnlyOperationCheckedWith :: AppendOnlyDependencies -> ExpectedRepositoryBasis -> Repository -> TransactionConfig -> IO (Either TransactionError TransactionResult)
commitAppendOnlyOperationCheckedWith dependencies basis repository config =
  commitAppendOnlyOperationCheckedWithHooks dependencies defaultAppendOnlyTestHooks basis repository config

commitAppendOnlyOperationWithHooks :: AppendOnlyDependencies -> AppendOnlyTestHooks -> Repository -> TransactionConfig -> IO (Either TransactionError TransactionResult)
commitAppendOnlyOperationWithHooks dependencies hooks repository config =
  commitAppendOnlyOperationMaybeCheckedWithHooks dependencies hooks Nothing Nothing repository config

commitAppendOnlyOperationCheckedWithHooks :: AppendOnlyDependencies -> AppendOnlyTestHooks -> ExpectedRepositoryBasis -> Repository -> TransactionConfig -> IO (Either TransactionError TransactionResult)
commitAppendOnlyOperationCheckedWithHooks dependencies hooks basis repository config =
  commitAppendOnlyOperationMaybeCheckedWithHooks dependencies hooks (Just basis) Nothing repository config

commitAppendOnlyOperationMaybeCheckedWithHooks :: AppendOnlyDependencies -> AppendOnlyTestHooks -> Maybe ExpectedRepositoryBasis -> Maybe (GitOid -> IO ()) -> Repository -> TransactionConfig -> IO (Either TransactionError TransactionResult)
commitAppendOnlyOperationMaybeCheckedWithHooks dependencies hooks expectedBasis publisher repository config@TransactionConfig{..} = do
  -- Validate worktree root present (reject bare repos)
  case repositoryWorktreeRoot repository of
    Nothing ->
      return (Left (Stage1ResolveRepo "worktree root is missing"))
    Just _ -> do
      durableWitness <- newIORef Nothing
      -- Stage 2: Acquire lock
      lockResult <- try @SomeException $ withGitLock repository $ do
        appendOnlyAfterLockAcquiredHook hooks
        -- Establish physical containment before probing, snapshotting, or
        -- writing a generated destination, and repeat it at later boundaries.
        contained <- resolveGeneratedDestinations repository False configGenerated
        void (either throwIO pure contained)
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

        -- Pin the exact attached ref before writing.  Rollback must never use
        -- whichever branch HEAD happens to name later.
        pinnedHead <- repositoryHeadState repository
        case (expectedBasis, pinnedHead) of
          (Just basis, Right actualState)
            | expectedBasisHeadState basis /= actualState ->
                throwIO (Stage3ValidateState "expected repository HEAD/ref basis mismatch")
          _ -> pure ()
        targetRef <- case pinnedHead of
          Left err -> throwIO (Stage3ValidateState ("get branch ref: " <> T.pack (show err)))
          Right GitHeadDetached -> throwIO (Stage3ValidateState "HEAD detached; attach a branch first")
          Right (GitHeadAttached branchRef) -> pure branchRef
        oldHeadResult <-
          case mkRevisionSpec (gitRefText targetRef) of
            Left revErr ->
              error ("mkRevisionSpec failed: " <> show revErr)  -- "HEAD" is always valid
            Right spec -> resolveRevision repository spec
        case oldHeadResult of
          Left err ->
            throwIO (Stage3ValidateState ("resolve HEAD: " <> T.pack (show err)))
          Right oldHead -> do
            case expectedBasis of
              Just basis
                | expectedBasisHead basis /= oldHead ->
                    throwIO (Stage3ValidateState ("expected repository commit mismatch: expected " <> gitOidText (expectedBasisHead basis) <> " got " <> gitOidText oldHead))
              _ -> pure ()
            -- Validate expected head matches
            when (oldHead /= configExpectedHead) $
              throwIO (Stage3ValidateState ("expected head mismatch: " <> T.pack configOperationId <> " expected " <> gitOidText configExpectedHead <> " got " <> gitOidText oldHead))

            mask (\restore -> do
              snapshotResult <- restore (captureAppendOnlySnapshot dependencies repository targetRef configGenerated)
              snapshot <- either throwIO pure snapshotResult
              newCommitRef <- newIORef Nothing
              writtenFilesRef <- newIORef []
              attempt <- try @SomeException $ restore $ do
                 -- Stage 5: Validate and write generated files
                 validatedFiles <- validateGeneratedFiles dependencies hooks repository config writtenFilesRef
                 either throwIO pure validatedFiles
                 appendOnlyAfterGeneratedWrite dependencies

                 -- Stages 6-8: Create temporary index, commit, update ref
                 tempIndexResult <- createTemporaryIndex (appendOnlyBeforeTempIndexCleanup dependencies) (appendOnlyAfterTempIndexCleanup dependencies) repository oldHead generatedPaths
                 case tempIndexResult of
                   Left err -> throwIO err
                   Right treeOid -> do
                     -- Stage 7: Commit tree
                     commitResult <- commitTree repository oldHead treeOid configSubject configOperationId configTrailers
                     case commitResult of
                       Left err -> throwIO err
                       Right newCommit -> do
                          modifyIORef' newCommitRef (const (Just newCommit))
                          -- Stage 8 always updates the ref pinned before Stage 5;
                          -- HEAD is intentionally not consulted again here.
                          appendOnlyBeforeRefUpdateHook hooks
                          preCasContained <- resolveGeneratedDestinations repository True configGenerated
                          void (either throwIO pure preCasContained)
                          case publisher of
                            Just _ -> pure (AppendOnlyPrepared newCommit)
                            Nothing -> do
                              casResult <-
                                runRepository
                                  repository
                                  "update-ref"
                                  [ "update-ref",
                                    "-m",
                                    "adrai " <> configOperationId,
                                    Text.unpack (gitRefText targetRef),
                                    Text.unpack (gitOidText newCommit),
                                    Text.unpack (gitOidText oldHead)
                                  ]
                                  BS.empty
                              case casResult of
                                Left err -> throwIO (Stage8UpdateRef ("update-ref failed: " <> T.pack (show err)))
                                Right casProcess
                                  | processExitCode casProcess /= ExitSuccess -> throwIO (Stage8UpdateRef ("update-ref exited " <> T.pack (show (processExitCode casProcess))))
                                  | otherwise -> do
                                      -- Preserve the established CLI/test behavior when no
                                      -- commit publisher is installed.
                                      appendOnlyAfterSuccessfulCas dependencies
                                      indexUpdated <- refreshAppendOnlyIndex hooks repository newCommit configGenerated generatedPaths
                                      pure (AppendOnlyComplete TransactionResult
                                        { transactionOperationId = configOperationId,
                                          transactionCommitOid = newCommit,
                                          transactionCreatedPaths = generatedPaths,
                                          transactionIndexUpdated = indexUpdated,
                                          transactionPublicationError = Nothing
                                        })
              case attempt of
                 Right (AppendOnlyComplete transactionResult) -> pure transactionResult
                 Right (AppendOnlyPrepared newCommit) -> do
                   -- Restore only the CAS action.  Its continuation is masked, so no
                   -- cancellation gap exists between a successful update and the
                   -- authoritative-ref classification below.
                   casAttempt <- try @SomeException (restore (runAppendOnlyCas repository targetRef oldHead newCommit configOperationId >>= either throwIO pure))
                   committedFailure <- case casAttempt of
                     Right () -> pure Nothing
                     Left originalFailure -> do
                       inspected <- try @SomeException (appendOnlyInspectRef dependencies repository targetRef oldHead (Just newCommit))
                       case inspected of
                         Right (Right actual)
                           | actual == newCommit -> pure (Just originalFailure)
                           | actual == oldHead -> do
                               writtenFiles <- readIORef writtenFilesRef
                               rethrowAfterRollback originalFailure (rollbackAppendOnlyFailure dependencies repository oldHead (Just newCommit) snapshot writtenFiles)
                           | otherwise -> rethrowUnclassifiedCas originalFailure "target ref moved to an unexpected commit after CAS"
                         Right (Left _) -> rethrowUnclassifiedCas originalFailure "target ref could not be classified after CAS"
                         Left inspectionFailure -> case fromException inspectionFailure :: Maybe SomeAsyncException of
                           Just cancellation -> throwIO cancellation
                           Nothing -> rethrowUnclassifiedCas originalFailure "target ref inspection failed after CAS"
                   published <- case publisher of
                     Just publish -> try @SomeException (publish newCommit)
                     Nothing -> pure (Right ())
                   refreshed <- try @SomeException (refreshAppendOnlyIndex hooks repository newCommit configGenerated generatedPaths)
                   let publicationError = case published of
                         Left failure | Nothing <- (fromException failure :: Maybe SomeAsyncException) -> Just (T.pack (show failure))
                         _ -> Nothing
                       indexUpdated = either (const False) id refreshed
                       casCancellation = committedFailure >>= (fromException :: SomeException -> Maybe SomeAsyncException)
                       publicationCancellation = either (fromException :: SomeException -> Maybe SomeAsyncException) (const Nothing) published
                       refreshCancellation = either (fromException :: SomeException -> Maybe SomeAsyncException) (const Nothing) refreshed
                       transactionResult = TransactionResult
                         { transactionOperationId = configOperationId,
                           transactionCommitOid = newCommit,
                           transactionCreatedPaths = generatedPaths,
                           transactionIndexUpdated = indexUpdated,
                           transactionPublicationError = publicationError
                         }
                   writeIORef durableWitness (Just transactionResult)
                   case casCancellation of
                     Just cancellation -> throwIO cancellation
                     Nothing -> case publicationCancellation of
                       Just cancellation -> throwIO cancellation
                       Nothing -> case refreshCancellation of
                         Just cancellation -> throwIO cancellation
                         Nothing -> pure transactionResult
                 Left originalFailure -> do
                  maybeNewCommit <- readIORef newCommitRef
                  writtenFiles <- readIORef writtenFilesRef
                  rethrowAfterRollback originalFailure (rollbackAppendOnlyFailure dependencies repository oldHead maybeNewCommit snapshot writtenFiles))
      case lockResult of
        Left err -> do
          case (fromException err :: Maybe SomeAsyncException) of
            Just asyncFailure -> throwIO asyncFailure
            Nothing -> do
              committed <- readIORef durableWitness
              case committed of
                Just result -> return (Right result)
                Nothing -> case fromException err of
                  Just txErr -> return (Left txErr)
                  Nothing -> return (Left (Stage2AcquireLock (T.pack (show err))))
        Right result -> return (Right result)

runAppendOnlyCas :: Repository -> GitRef -> GitOid -> GitOid -> String -> IO (Either TransactionError ())
runAppendOnlyCas repository targetRef oldHead newCommit operationId = do
  casResult <-
    runRepository
      repository
      "update-ref"
      [ "update-ref",
        "-m",
        "adrai " <> operationId,
        Text.unpack (gitRefText targetRef),
        Text.unpack (gitOidText newCommit),
        Text.unpack (gitOidText oldHead)
      ]
      BS.empty
  pure $ case casResult of
    Left err -> Left (Stage8UpdateRef ("update-ref failed: " <> T.pack (show err)))
    Right result
      | processExitCode result /= ExitSuccess -> Left (Stage8UpdateRef ("update-ref exited " <> T.pack (show (processExitCode result))))
      | otherwise -> Right ()

rethrowUnclassifiedCas :: SomeException -> Text -> IO value
rethrowUnclassifiedCas originalFailure message =
  case fromException originalFailure :: Maybe SomeAsyncException of
    Just cancellation -> throwIO cancellation
    Nothing -> throwIO (Stage8UpdateRef message)

refreshAppendOnlyIndex :: AppendOnlyTestHooks -> Repository -> GitOid -> [GeneratedFile] -> [RepoPath] -> IO Bool
refreshAppendOnlyIndex hooks repository newCommit generated generatedPaths
  | null generatedPaths = pure True
  | otherwise = do
      appendOnlyBeforePostCasIndexRefreshHook hooks
      postCasContained <- resolveGeneratedDestinations repository True generated
      case postCasContained of
        Left _ -> pure False
        Right _ -> do
          refreshResult <-
            runRepository
              repository
              "reset index"
              (["reset", "-q", Text.unpack (gitOidText newCommit), "--"] <> map (T.unpack . repoPathText) generatedPaths)
              BS.empty
          pure $ case refreshResult of
            Left _ -> False
            Right res -> processExitCode res == ExitSuccess

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
commitBootstrapFiles = commitBootstrapFilesWith defaultBootstrapDependencies

-- | Bootstrap variant with the narrow test seams required to exercise a
-- physical path replacement between parent creation and the final write.
commitBootstrapFilesWith :: BootstrapDependencies -> Repository -> TransactionConfig -> IO (Either TransactionError TransactionResult)
commitBootstrapFilesWith dependencies repository config@TransactionConfig{..} = do
  contained <- resolveGeneratedDestinations repository True configGenerated
  case contained of
    Left err -> pure (Left err)
    Right _ -> commitBootstrapFilesContained dependencies repository config

commitBootstrapFilesContained :: BootstrapDependencies -> Repository -> TransactionConfig -> IO (Either TransactionError TransactionResult)
commitBootstrapFilesContained dependencies repository config@TransactionConfig{..} = do
  -- Stage 1: Validate worktree root
  case repositoryWorktreeRoot repository of
    Nothing ->
      return (Left (Stage1ResolveRepo "worktree root is missing"))
    Just _ -> do
      -- Stage 2: Acquire lock (with backup/restore on failure)
      -- Backup existing files.  This happens before the lock only to capture
      -- caller bytes, but remains inside a typed exception boundary so a late
      -- containment rejection never escapes the public Either API.
      let backupOne GeneratedFile{genFilePath = path} =
            do
              resolved <- resolveGeneratedDestination repository True GeneratedFile{genFilePath = path, genFileBytes = BS.empty}
              filePath <- either throwIO pure resolved
              exists <- doesFileExist filePath
              if exists
                then do
                  content <- BS.readFile filePath
                  return (path, Just content)
                else return (path, Nothing)
      backupAttempt <- try @SomeException (traverse backupOne configGenerated)
      backupFiles <-
        case backupAttempt of
          Left failure ->
            case fromException failure :: Maybe SomeAsyncException of
              Just asyncFailure -> throwIO asyncFailure
              Nothing ->
                case fromException failure of
                  Just transactionFailure -> pure (Left transactionFailure)
                  Nothing -> pure (Left (Stage5ValidateGenerated ("bootstrap backup failed: " <> T.pack (show failure))))
          Right backups -> pure (Right backups)
      case backupFiles of
        Left err -> pure (Left err)
        Right backups -> commitBootstrapFilesAfterBackup dependencies repository config backups

commitBootstrapFilesAfterBackup :: BootstrapDependencies -> Repository -> TransactionConfig -> [(RepoPath, Maybe ByteString)] -> IO (Either TransactionError TransactionResult)
commitBootstrapFilesAfterBackup dependencies repository config@TransactionConfig{..} backupFiles = do
  case repositoryWorktreeRoot repository of
    Nothing -> pure (Left (Stage1ResolveRepo "worktree root is missing"))
    Just _ -> do
      let generatedPaths = map genFilePath configGenerated
      casCandidateRef <- newIORef Nothing

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
            Just _ -> do
                resolved <- resolveGeneratedDestination repository True (GeneratedFile genFilePath genFileBytes)
                dirPath <- either throwIO pure resolved
                createDirectoryIfMissing True (takeDirectory dirPath)
                bootstrapAfterGeneratedParentCreation dependencies (GeneratedFile genFilePath genFileBytes)
                resolvedAfterParentCreation <- resolveGeneratedDestination repository True (GeneratedFile genFilePath genFileBytes)
                writePath <- either throwIO pure resolvedAfterParentCreation
                BS.writeFile writePath genFileBytes
                bootstrapAfterGeneratedFileWrite dependencies (GeneratedFile genFilePath genFileBytes)

        -- Get branch ref
        headState <- repositoryHeadState repository
        case headState of
          Left err ->
            throwIO (Stage3ValidateState ("get branch ref: " <> T.pack (show err)))
          Right GitHeadDetached ->
            throwIO (Stage3ValidateState "HEAD is detached; attach a branch before bootstrapping")
          Right (GitHeadAttached branchRef) -> do
            -- Stages 6-8
            tempIndexResult <- createTemporaryIndex (pure ()) (pure ()) repository oldHead generatedPaths
            case tempIndexResult of
              Left err ->
                throwIO err
              Right treeOid -> do
                -- Stage 7: Commit tree
                commitArgs <-
                  if isNullOid oldHead
                    then pure ["commit-tree", Text.unpack (gitOidText treeOid)]
                    else pure ["commit-tree", Text.unpack (gitOidText treeOid), "-p", Text.unpack (gitOidText oldHead)]

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
                  Left err -> throwIO (Stage7CommitTree ("commit-tree failed: " <> T.pack (show err)))
                  Right res2
                    | processExitCode res2 /= ExitSuccess ->
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
                            bootstrapBeforeRefUpdate dependencies
                            preCasContained <- resolveGeneratedDestinations repository True configGenerated
                            void (either throwIO pure preCasContained)
                            modifyIORef' casCandidateRef (const (Just (branchRef, oldHead, newCommit)))
                            bootstrapAfterCasCandidateBeforeUpdateRef dependencies
                            casResult <-
                              runRepository
                                repository
                                "update-ref"
                                casArgs
                                BS.empty
                            case casResult of
                              Left err -> throwIO (Stage8UpdateRef ("update-ref failed: " <> T.pack (show err)))
                              Right res3
                                | processExitCode res3 /= ExitSuccess ->
                                    throwIO (Stage8UpdateRef ("update-ref exited " <> T.pack (show (processExitCode res3))))
                                | otherwise -> do
                                    -- This seam covers the post-update-ref / pre-local-
                                    -- bookkeeping cancellation boundary. Candidate/ref
                                    -- inspection in the outer handler is authoritative.
                                    bootstrapAfterSuccessfulCasBeforeBookkeeping dependencies
                                    -- Refresh index
                                    indexUpdated <-
                                      if null generatedPaths
                                        then pure True
                                        else do
                                          bootstrapBeforePostCasIndexRefresh dependencies
                                          postCasContained <- resolveGeneratedDestinations repository True configGenerated
                                          case postCasContained of
                                            Left _ -> pure False
                                            Right _ -> do
                                              refreshResult <-
                                                runRepository
                                                  repository
                                                  "reset index"
                                                  (["reset", "-q", "HEAD", "--"] <> map (T.unpack . repoPathText) generatedPaths)
                                                  BS.empty
                                              pure $ case refreshResult of
                                                Left _ -> False
                                                Right res -> processExitCode res == ExitSuccess
                                    return
                                      TransactionResult
                                        { transactionOperationId = configOperationId,
                                          transactionCommitOid = newCommit,
                                          transactionCreatedPaths = generatedPaths,
                                          transactionIndexUpdated = indexUpdated,
                                          transactionPublicationError = Nothing
                                        }

      case lockResult of
        Left err -> do
          candidate <- readIORef casCandidateRef
          authority <- traverse (inspectBootstrapCasCandidate dependencies repository) candidate
          case (candidate, authority) of
            (Just (_, _, committed), Just (Right True)) ->
              case fromException err :: Maybe SomeAsyncException of
                Just asyncFailure -> throwIO asyncFailure
                Nothing ->
                  pure
                    ( Right
                        TransactionResult
                          { transactionOperationId = configOperationId,
                              transactionCommitOid = committed,
                              transactionCreatedPaths = generatedPaths,
                              transactionIndexUpdated = False,
                              transactionPublicationError = Nothing
                          }
                    )
            (_, Just (Left conflict)) -> pure (Left conflict)
            _ ->
              case (fromException err :: Maybe SomeAsyncException) of
                Just asyncFailure -> do
                  rethrowAfterRollback (toException asyncFailure) (rollbackBootstrappedFiles dependencies repository generatedPaths configGenerated backupFiles)
                Nothing ->
                  case fromException err of
                    Just txErr -> do
                      recoverBootstrapFailure txErr
                    Nothing -> do
                      recoverBootstrapFailure (Stage2AcquireLock (T.pack (show err)))
        Right result -> return (Right result)
      where
        recoverBootstrapFailure original = do
          rollbackResult <- rollbackBootstrappedFiles dependencies repository (map genFilePath configGenerated) configGenerated backupFiles
          case rollbackResult of
            Left cleanupFailure -> pure (Left (RollbackFailed ("bootstrap rollback failed after " <> T.pack (show original) <> ": " <> T.pack (show cleanupFailure))))
            Right () -> pure (Left original)

-- | Rollback bootstrap files: restore backups or delete new files.
rollbackBootstrappedFiles :: BootstrapDependencies -> Repository -> [RepoPath] -> [GeneratedFile] -> [(RepoPath, Maybe ByteString)] -> IO (Either TransactionError ())
rollbackBootstrappedFiles dependencies repository _genFiles _backups backupData = mask $ \restore -> do
  cleanupHook <- try @SomeException (restore (bootstrapBeforeRollbackCleanup dependencies))
  attempt <- try @SomeException (mapM_ restoreOrDelete backupData)
  -- Consider both phases before selecting a synchronous failure: a
  -- ThreadKilled from the pre-cleanup hook must not be hidden by a later
  -- synchronous restore/delete error.
  case (asyncOutcome attempt, asyncOutcome cleanupHook) of
    (Just asyncFailure, _) -> throwIO asyncFailure
    (_, Just asyncFailure) -> throwIO asyncFailure
    (Nothing, Nothing) ->
      case attempt of
        Left failure ->
          case fromException failure of
            Just transactionFailure -> pure (Left transactionFailure)
            Nothing -> pure (Left (RollbackFailed ("bootstrap rollback cleanup failed: " <> T.pack (show failure))))
        Right () ->
          case cleanupHook of
            Left failure -> pure (Left (RollbackFailed ("bootstrap rollback cleanup hook failed: " <> T.pack (show failure))))
            Right () -> pure (Right ())
  where
    restoreOrDelete (path, maybeContent) = do
      let worktreeRoot = repositoryWorktreeRoot repository
      case worktreeRoot of
        Nothing -> pure ()
        Just root -> do
          resolved <- resolveGeneratedDestination repository True (GeneratedFile path BS.empty)
          void (either (throwIO . RollbackFailed . ("managed bootstrap rollback destination rejected: " <>) . T.pack . show) pure resolved)
          case maybeContent of
            Just content -> do
              -- Restore original content
              resolvedBeforeRestore <- resolveGeneratedDestination repository True (GeneratedFile path content)
              restorePath <- either (throwIO . rollbackDestinationFailure) pure resolvedBeforeRestore
              BS.writeFile restorePath content
            Nothing -> do
              -- Delete the file we created
              resolvedBeforeDelete <- resolveGeneratedDestination repository True (GeneratedFile path BS.empty)
              deletePath <- either (throwIO . rollbackDestinationFailure) pure resolvedBeforeDelete
              deleteAttempt <- try @SomeException (bootstrapDeleteGeneratedFile dependencies deletePath)
              case deleteAttempt of
                Left failure ->
                  case fromException failure :: Maybe SomeAsyncException of
                    Just asyncFailure -> throwIO asyncFailure
                    Nothing ->
                      throwIO
                        ( RollbackFailed
                            ( "bootstrap rollback delete failed for "
                                <> repoPathText path
                                <> ": "
                                <> T.pack (show failure)
                            )
                        )
                Right () -> pure ()
              -- Try to remove empty parent directories
              resolvedBeforeCleanup <- resolveGeneratedDestination repository True (GeneratedFile path BS.empty)
              cleanupPath <- either (throwIO . rollbackDestinationFailure) pure resolvedBeforeCleanup
              let parent = takeDirectory cleanupPath
              removeEmptyParentsContained repository (GeneratedFile path BS.empty) root parent
