{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Disposable SQLite projection of an already committed repository revision.
--
-- A mutation caller supplies the commit that already succeeded and the exact
-- caller-owned database path to materialize.  Each materialization is first
-- compiled into a disposable sibling database and only replaces that path
-- after compilation and close have both succeeded.  Consequently, a failed
-- refresh leaves a prior usable index in place, but never reports it as the
-- index for the new commit.
module Adrai.Service.PostCommitIndex
  ( IndexWarning (..),
    PostCommitIndexError (..),
    PostCommitIndexPublishError (..),
    PostCommitIndexResult (..),
    PostCommitIndexDependencies (..),
    postCommitIndexDependencies,
    compilePostCommitIndex,
    compilePostCommitIndexWithAttribution,
    compilePostCommitIndexWith,
    clonePostCommitIndex,
    clonePostCommitIndexWithHistoryCount,
  )
where

import Adrai.Compiler
  ( ColdCompilerError,
    ColdCompilerResult (..),
    coldCompileRepository,
    coldCompileRepositoryWithAttribution,
  )
import Adrai.Compiler.Attribution
  ( AttributionPhase (DatabaseClose, ImmutablePublication),
    ColdCompileAttribution,
    inertColdCompileAttribution,
    withAttributionPhase,
  )
import Adrai.Compiler.CacheSelection (validateCachePublicationContract)
import Adrai.Compiler.Snapshot
  ( CompilerDiagnostic (..),
    CompilerDiagnosticSeverity (CompilerDiagnosticWarning),
    compilerDiagnosticCodeText,
  )
import Adrai.Git
  ( GitOid,
    Repository,
    RevisionSpec (RevisionSpec),
    gitOidText,
  )
import Adrai.Repository
  ( RepositorySnapshotError,
    ResolvedRepositoryRevision,
    resolveRepositoryRevision,
    resolvedCommitOid,
  )
import Control.Exception (SomeAsyncException, SomeException, displayException, fromException, mask, throwIO, try)
import Data.Bits ((.|.))
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple (Connection, close, execute, open, query_)
import System.Directory (copyFile, doesFileExist, removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (Handle, hClose, openTempFile)
import System.Win32.Types (BOOL, DWORD, LPTSTR, failIfFalse_, withTString)

foreign import ccall unsafe "MoveFileExW" c_MoveFileExW :: LPTSTR -> LPTSTR -> DWORD -> IO BOOL

-- | A stable public projection of a compiler warning.  Compiler diagnostics
-- retain their richer internal provenance; callers of this result only need a
-- deterministic code/message pair.
data IndexWarning = IndexWarning
  { indexWarningCode :: Text,
    indexWarningMessage :: Text
  }
  deriving (Eq, Ord, Show)

-- | Actual failures that can occur after a successful Git mutation.  These
-- failures are data, never transaction errors, because no rollback is valid
-- after the commit is durable.
data PostCommitIndexError
  = PostCommitIndexResolveFailure RepositorySnapshotError
  | PostCommitIndexRevisionMismatch GitOid GitOid
  | PostCommitIndexOpenFailure Text
  | PostCommitIndexCompileFailure ColdCompilerError
  | PostCommitIndexCompileException Text
  | PostCommitIndexCloneFailure Text
  | PostCommitIndexCloseFailure Text
  | PostCommitIndexTemporaryCloseFailure Text
  | PostCommitIndexCleanupFailure FilePath Text
  | PostCommitIndexPublishFailure PostCommitIndexPublishError
  | PostCommitIndexMultipleFailures [PostCommitIndexError]
  deriving (Eq, Show)

-- | Structured publication failures.  The production path uses one native
-- replacement operation, while the restore form remains available to narrow
-- publication seams that implement an explicit install/restore state machine.
data PostCommitIndexPublishError
  = PostCommitIndexInstallFailure Text
  | PostCommitIndexInstallAndRestoreFailure Text Text
  | PostCommitIndexRecoveryInvariantFailure PostCommitIndexPublishError Text
  deriving (Eq, Show)

-- | Truthful result of a disposable cold compilation.  Success-only fields
-- are absent on every failure, including a close failure.
data PostCommitIndexResult = PostCommitIndexResult
  { postCommitIndexed :: Bool,
    postCommitDatabase :: Maybe FilePath,
    postCommitIndexRevision :: Maybe GitOid,
    postCommitIndexWarnings :: [IndexWarning],
    postCommitIndexError :: Maybe PostCommitIndexError
  }
  deriving (Eq, Show)

-- | Narrow IO boundary used by the production entry point and focused tests.
-- The parameterized function cannot affect mutation transaction semantics.
data PostCommitIndexDependencies = PostCommitIndexDependencies
  { postCommitResolveRevision :: Repository -> GitOid -> IO (Either RepositorySnapshotError ResolvedRepositoryRevision),
    postCommitOpenDatabase :: FilePath -> IO Connection,
    postCommitColdCompile :: Connection -> ResolvedRepositoryRevision -> IO (Either ColdCompilerError ColdCompilerResult),
    postCommitCloseDatabase :: Connection -> IO (),
    postCommitOpenTemporary :: FilePath -> String -> IO (FilePath, Handle),
    postCommitCloseTemporary :: Handle -> IO (),
    postCommitDoesOwnedFileExist :: FilePath -> IO Bool,
    postCommitRemoveOwnedFile :: FilePath -> IO (),
    postCommitInstallDatabase :: FilePath -> FilePath -> Maybe FilePath -> IO (Either PostCommitIndexPublishError ()),
    postCommitRestoreDatabase :: FilePath -> FilePath -> IO ()
  }

postCommitIndexDependencies :: PostCommitIndexDependencies
postCommitIndexDependencies =
  PostCommitIndexDependencies
    { postCommitResolveRevision = \repository commitOid -> resolveRepositoryRevision repository (RevisionSpec (gitOidText commitOid)),
      postCommitOpenDatabase = open,
      postCommitColdCompile = coldCompileRepository,
      postCommitCloseDatabase = close,
      postCommitOpenTemporary = openTempFile,
      postCommitCloseTemporary = hClose,
      postCommitDoesOwnedFileExist = doesFileExist,
      postCommitRemoveOwnedFile = removeIfPresent,
      postCommitInstallDatabase = installCandidate,
      postCommitRestoreDatabase = renameFile
    }

-- | Materialize the exact supplied commit into the exact caller-selected
-- SQLite path.  The resolved full commit must agree with the mutation commit
-- before compilation begins.  A successful result is published to that exact
-- path; on failure an earlier target is intentionally retained but is never
-- represented as current in 'PostCommitIndexResult'.
compilePostCommitIndex :: Repository -> GitOid -> FilePath -> IO PostCommitIndexResult
compilePostCommitIndex = compilePostCommitIndexWith postCommitIndexDependencies

-- | Explicit profiling entry point.  No public mutation or ordinary compile
-- path calls this variant; it exists solely for the test-owned cold compiler
-- observer injected by 'Adrai.CliRunner'.
compilePostCommitIndexWithAttribution :: ColdCompileAttribution -> Repository -> GitOid -> FilePath -> IO PostCommitIndexResult
compilePostCommitIndexWithAttribution attribution =
  compilePostCommitIndexWithAttributionDependencies attribution
    postCommitIndexDependencies {postCommitColdCompile = coldCompileRepositoryWithAttribution attribution}

-- | Publish a selected immutable cache under the mutable current-alias path.
-- The caller has already established that @sourcePath@ is the authoritative
-- immutable snapshot for @targetRevision@: either this same process just
-- published it, or cache selection already accepted it as an exact archive.
-- Re-validating that source here is therefore redundant and can spuriously
-- reject a fresh alias repair while the archived snapshot remains the source of
-- truth.  Cross-revision reuse keeps its stricter validation below.
clonePostCommitIndex :: FilePath -> GitOid -> FilePath -> IO PostCommitIndexResult
clonePostCommitIndex sourcePath targetRevision databasePath =
  clonePostCommitIndexInternal False sourcePath targetRevision databasePath Nothing

-- | Tree-identical reuse has already performed a bounded history proof.  Carry
-- its observed commit count into the derived snapshot instead of claiming the
-- reused result scanned no history.  Alias publication passes 'Nothing' and
-- preserves the archive's existing count.
clonePostCommitIndexWithHistoryCount :: FilePath -> GitOid -> FilePath -> Maybe Int -> IO PostCommitIndexResult
clonePostCommitIndexWithHistoryCount sourcePath targetRevision databasePath historyCommitsScanned =
  clonePostCommitIndexInternal True sourcePath targetRevision databasePath historyCommitsScanned

clonePostCommitIndexInternal :: Bool -> FilePath -> GitOid -> FilePath -> Maybe Int -> IO PostCommitIndexResult
clonePostCommitIndexInternal requireValidatedSource sourcePath targetRevision databasePath historyCommitsScanned = do
  sourceValid <-
    if requireValidatedSource
      then validateCachePublicationContract sourcePath
      else pure True
  if not sourceValid
    then pure (failure (PostCommitIndexCloneFailure "cache clone source failed the canonical metadata or integrity contract"))
    else mask $ \restore -> do
      candidate <- createCandidate postCommitIndexDependencies databasePath
      case candidate of
        Left problems -> pure (failure (combineFailures problems))
        Right temporaryPath -> do
          copied <- trySynchronous (restore (copyFile sourcePath temporaryPath))
          case copied of
            Left exception -> failAndCleanup temporaryPath (PostCommitIndexCloneFailure (exceptionText exception))
            Right () -> do
              opened <- trySynchronous (restore (open temporaryPath))
              case opened of
                Left exception -> failAndCleanup temporaryPath (PostCommitIndexOpenFailure (exceptionText exception))
                Right connection -> do
                  retargeted <- trySynchronous (restore (retargetCachedRevision connection targetRevision historyCommitsScanned))
                  closed <- trySynchronous (postCommitCloseDatabase postCommitIndexDependencies connection)
                  case (retargeted, closed) of
                    (Left updateFailure, Left closeFailure) ->
                      failAndCleanup temporaryPath (combineFailures [PostCommitIndexCloneFailure (exceptionText updateFailure), PostCommitIndexCloseFailure (exceptionText closeFailure)])
                    (Left updateFailure, Right ()) ->
                      failAndCleanup temporaryPath (PostCommitIndexCloneFailure (exceptionText updateFailure))
                    (Right (), Left closeFailure) ->
                      failAndCleanup temporaryPath (PostCommitIndexCloseFailure (exceptionText closeFailure))
                    (Right (), Right ()) -> do
                      published <- publishCandidate postCommitIndexDependencies databasePath temporaryPath
                      case published of
                        PublicationSucceeded ->
                          pure (PostCommitIndexResult True (Just databasePath) (Just targetRevision) [] Nothing)
                        PublicationFailed problems -> do
                          cleanupProblems <- cleanupOwnedTemporary postCommitIndexDependencies temporaryPath
                          pure (failure (combineFailures (problems <> cleanupProblems)))
                        PublicationFailStop problems -> pure (failure (combineFailures problems))
  where
    failure problem = PostCommitIndexResult False Nothing Nothing [] (Just problem)
    exceptionText = Text.pack . displayException
    failAndCleanup temporaryPath problem = do
      cleanupProblems <- cleanupOwnedTemporary postCommitIndexDependencies temporaryPath
      pure (failure (combineFailures (problem : cleanupProblems)))

retargetCachedRevision :: Connection -> GitOid -> Maybe Int -> IO ()
retargetCachedRevision connection revision historyCommitsScanned = do
  metadata <- query_ connection "SELECT key,value FROM meta ORDER BY key" :: IO [(Text, Text)]
  case (lookup "schema" metadata, lookup "compiler_abi" metadata, lookup "materializer" metadata, lookup "materialization_fingerprint" metadata, lookup "source_fingerprint" metadata, lookup "resolved_oid" metadata, lookup "requested_revision" metadata) of
    (Just "adrai-cache/1", Just "adrai-cold-compiler/1", Just _, Just _, Just _, Just _, Just _) -> do
      -- The caller may reach this point only after proving a bounded,
      -- compiler-irrelevant delta.  The source was integrity-validated before
      -- copying, and only revision labels change; source/materialization
      -- fingerprints remain intact and are never fabricated or replaced.
      execute connection "UPDATE meta SET value=? WHERE key=?" (gitOidText revision, "resolved_oid" :: Text)
      execute connection "UPDATE meta SET value=? WHERE key=?" (gitOidText revision, "requested_revision" :: Text)
      case historyCommitsScanned of
        Nothing -> pure ()
        Just count | count > 0 -> execute connection "UPDATE meta SET value=? WHERE key=?" (Text.pack (show count), "history_commits_scanned" :: Text)
        _ -> ioError (userError "tree-identical cache clone requires a positive bounded history count")
    _ -> ioError (userError "cache clone source does not contain a complete canonical compiler metadata set")

-- | Parameterized form with deterministic failure composition.  If compilation
-- and close both fail, errors are retained in lifecycle order: compile, close.
compilePostCommitIndexWith :: PostCommitIndexDependencies -> Repository -> GitOid -> FilePath -> IO PostCommitIndexResult
compilePostCommitIndexWith = compilePostCommitIndexWithAttributionDependencies inertColdCompileAttribution

compilePostCommitIndexWithAttributionDependencies :: ColdCompileAttribution -> PostCommitIndexDependencies -> Repository -> GitOid -> FilePath -> IO PostCommitIndexResult
compilePostCommitIndexWithAttributionDependencies attribution dependencies repository commitOid databasePath = do
  resolved <- postCommitResolveRevision dependencies repository commitOid
  case resolved of
    Left problem -> pure (failure (PostCommitIndexResolveFailure problem))
    Right revision
      | resolvedCommitOid revision /= commitOid ->
          pure (failure (PostCommitIndexRevisionMismatch commitOid (resolvedCommitOid revision)))
      | otherwise -> compileAt revision
  where
    compileAt :: ResolvedRepositoryRevision -> IO PostCommitIndexResult
    compileAt revision = mask $ \restore -> do
      candidate <- createCandidate dependencies databasePath
      case candidate of
        Left problems -> pure (failure (combineFailures problems))
        Right temporaryPath -> do
          opened <- tryAny (restore (postCommitOpenDatabase dependencies temporaryPath))
          case opened of
            Left exception
              | isAsyncException exception -> cleanupAfterCancellation Nothing temporaryPath exception
              | otherwise -> do
                  cleanupProblems <- cleanupOwnedTemporary dependencies temporaryPath
                  pure (failure (combineFailures (PostCommitIndexOpenFailure (exceptionText exception) : cleanupProblems)))
            Right connection -> do
              compiled <- tryAny (restore (postCommitColdCompile dependencies connection revision))
              case compiled of
                Left exception
                  | isAsyncException exception -> cleanupAfterCancellation (Just connection) temporaryPath exception
                _ -> do
                  closed <- tryAny (restore (withAttributionPhase attribution DatabaseClose (postCommitCloseDatabase dependencies connection)))
                  case closed of
                    Left exception
                      | isAsyncException exception -> cleanupAfterCancellation (Just connection) temporaryPath exception
                    _ -> complete revision temporaryPath compiled closed

    -- Cancellation retains precedence over finalizer failures: once a database
    -- handle has escaped acquisition, attempt its close and every owned-path
    -- cleanup while masked, then rethrow the original asynchronous exception.
    -- Catching finalizer exceptions here prevents a later cleanup failure from
    -- erasing the cancellation that initiated this unwind.
    cleanupAfterCancellation maybeConnection temporaryPath cancellation = do
      case maybeConnection of
        Nothing -> pure ()
        Just connection -> do
          _ <- tryAny (postCommitCloseDatabase dependencies connection)
          pure ()
      cleanupOwnedTemporaryAfterCancellation dependencies temporaryPath
      throwIO cancellation

    complete revision temporaryPath compiled closed =
      case (compileOutcome compiled, closeOutcome closed) of
        (Left compileError, Left closeError) -> do
          cleanupProblems <- cleanupOwnedTemporary dependencies temporaryPath
          pure (failure (combineFailures ([compileError, closeError] <> cleanupProblems)))
        (Left compileError, Right ()) -> do
          cleanupProblems <- cleanupOwnedTemporary dependencies temporaryPath
          pure (failure (combineFailures (compileError : cleanupProblems)))
        (Right _, Left closeError) -> do
          cleanupProblems <- cleanupOwnedTemporary dependencies temporaryPath
          pure (failure (combineFailures (closeError : cleanupProblems)))
        (Right result, Right ()) -> do
          published <- withAttributionPhase attribution ImmutablePublication (publishCandidate dependencies databasePath temporaryPath)
          case published of
            PublicationFailed publishProblems -> do
              cleanupProblems <- cleanupOwnedTemporary dependencies temporaryPath
              pure (failure (combineFailures (publishProblems <> cleanupProblems)))
            PublicationFailStop publishProblems ->
              -- The retained candidate/backup is deliberate recovery evidence.
              pure (failure (combineFailures publishProblems))
            PublicationSucceeded ->
              pure
                PostCommitIndexResult
                  { postCommitIndexed = True,
                    postCommitDatabase = Just databasePath,
                    postCommitIndexRevision = Just (resolvedCommitOid revision),
                    postCommitIndexWarnings = compilerWarnings result,
                    postCommitIndexError = Nothing
                  }

    compileOutcome = \case
      Left exception -> Left (PostCommitIndexCompileException (Text.pack (displayException exception)))
      Right (Left problem) -> Left (PostCommitIndexCompileFailure problem)
      Right (Right result) -> Right result

    closeOutcome = \case
      Left exception -> Left (PostCommitIndexCloseFailure (Text.pack (displayException exception)))
      Right () -> Right ()

    failure problem =
      PostCommitIndexResult
        { postCommitIndexed = False,
          postCommitDatabase = Nothing,
          postCommitIndexRevision = Nothing,
          postCommitIndexWarnings = [],
          postCommitIndexError = Just problem
        }

    exceptionText = Text.pack . displayException

-- | Reserve a same-directory filename for SQLite, then close its handle before
-- SQLite opens it.  Keeping the candidate alongside the final path makes the
-- final rename a same-filesystem operation.
createCandidate :: PostCommitIndexDependencies -> FilePath -> IO (Either [PostCommitIndexError] FilePath)
createCandidate dependencies databasePath =
  createOwnedTemporary dependencies databasePath ".post-commit-"

-- | Acquire and close a task-owned same-directory temporary.  If the first
-- close reports failure, close is retried before pathname cleanup because the
-- exception does not say whether the handle remains live.
createOwnedTemporary :: PostCommitIndexDependencies -> FilePath -> String -> IO (Either [PostCommitIndexError] FilePath)
createOwnedTemporary dependencies databasePath marker = do
  acquired <- trySynchronous (postCommitOpenTemporary dependencies (takeDirectory databasePath) (takeFileName databasePath <> marker))
  case acquired of
    Left exception -> pure (Left [PostCommitIndexOpenFailure (exceptionText exception)])
    Right (temporaryPath, handle) -> do
      closed <- trySynchronous (postCommitCloseTemporary dependencies handle)
      case closed of
        Right () -> pure (Right temporaryPath)
        Left closeException -> do
          -- A close exception does not specify whether the handle remains live.
          -- Retry once before pathname cleanup so a transient/injected failure
          -- cannot deterministically strand the task-owned file.
          retried <- trySynchronous (postCommitCloseTemporary dependencies handle)
          cleanupProblems <- cleanupOwnedTemporary dependencies temporaryPath
          let closeProblems =
                PostCommitIndexTemporaryCloseFailure (exceptionText closeException)
                  : case retried of
                    Left retryException -> [PostCommitIndexTemporaryCloseFailure (exceptionText retryException)]
                    Right () -> []
          pure (Left (closeProblems <> cleanupProblems))
  where
    exceptionText = Text.pack . displayException

-- | Publish a closed same-directory candidate under an exception mask.  For an
-- existing target, the dependency seam receives an absent task-owned backup
-- pathname so narrow tests can model a replace-style partial failure.  The
-- production primitive is a same-volume atomic rename and does not create that
-- backup, but reconciliation still observes all three owned paths and either
-- retains the prior target or fails stop with the remaining recovery artifacts
-- intact.
publishCandidate :: PostCommitIndexDependencies -> FilePath -> FilePath -> IO PublicationOutcome
publishCandidate dependencies databasePath temporaryPath = mask $ \_ -> do
  observedTarget <- observeFile databasePath
  case observedTarget of
    Left exception ->
      pure
        ( observationFailStop
            (PostCommitIndexInstallFailure "publication not attempted because target state could not be observed")
            "initial target observation failed"
            exception
        )
    Right targetExists ->
      if targetExists
        then publishReplacement
        else do
          installed <- attemptInstall Nothing
          pure $ case installed of
            Left problem -> PublicationFailed [PostCommitIndexPublishFailure problem]
            Right () -> PublicationSucceeded
  where
    publishReplacement = do
      reserved <- reserveBackupPath dependencies databasePath
      case reserved of
        Left problems -> pure (PublicationFailed problems)
        Right backupPath -> do
          installed <- attemptInstall (Just backupPath)
          case installed of
            Right () -> do
              observedInstall <- observeFile databasePath
              case observedInstall of
                Left exception ->
                  pure
                    ( observationFailStop
                        (PostCommitIndexInstallFailure "replacement reported success but target state could not be verified")
                        "post-install target observation failed"
                        exception
                    )
                Right targetInstalled ->
                  if targetInstalled
                    then finishSuccessfulInstall backupPath
                    else reconcileFailure backupPath (PostCommitIndexInstallFailure "replacement reported success but target is absent")
            Left installProblem -> reconcileFailure backupPath installProblem

    attemptInstall backupPath = do
      attempted <- trySynchronous (postCommitInstallDatabase dependencies databasePath temporaryPath backupPath)
      pure $ case attempted of
        Left exception -> Left (PostCommitIndexInstallFailure (exceptionText exception))
        Right result -> result

    finishSuccessfulInstall backupPath = do
      cleanupProblems <- cleanupOwnedPath dependencies backupPath
      pure $ if null cleanupProblems then PublicationSucceeded else PublicationFailed cleanupProblems

    reconcileFailure backupPath installProblem = do
      recovered <- restorePriorTarget dependencies databasePath temporaryPath backupPath
      case recovered of
        PriorTargetRestored restoreProblems -> do
          cleanupProblems <- cleanupOwnedPath dependencies backupPath
          let publishProblem = withRestoreFailures installProblem restoreProblems
          pure (PublicationFailed (PostCommitIndexPublishFailure publishProblem : cleanupProblems))
        PriorTargetUnavailable restoreProblems invariantReason state -> do
          let attemptedProblem = withRestoreFailures installProblem restoreProblems
              invariantProblem =
                PostCommitIndexRecoveryInvariantFailure
                  attemptedProblem
                  (invariantReason <> "; " <> publicationStateText state)
          pure (PublicationFailStop [PostCommitIndexPublishFailure invariantProblem])
        PriorTargetObservationFailed restoreProblems observationProblem -> do
          let attemptedProblem = withRestoreFailures installProblem restoreProblems
              invariantProblem = PostCommitIndexRecoveryInvariantFailure attemptedProblem observationProblem
          pure (PublicationFailStop [PostCommitIndexPublishFailure invariantProblem])

    observeFile path = trySynchronous (postCommitDoesOwnedFileExist dependencies path)

    observationFailStop attemptedProblem phase exception =
      PublicationFailStop
        [ PostCommitIndexPublishFailure
            (PostCommitIndexRecoveryInvariantFailure attemptedProblem (phase <> ": " <> exceptionText exception))
        ]

    exceptionText = Text.pack . displayException

-- | Reserve an absent same-directory backup path for the replacement seam.
-- The reservation handle is closed before the empty placeholder is removed.
reserveBackupPath :: PostCommitIndexDependencies -> FilePath -> IO (Either [PostCommitIndexError] FilePath)
reserveBackupPath dependencies databasePath = do
  reserved <- createOwnedTemporary dependencies databasePath ".post-commit-backup-"
  case reserved of
    Left problems -> pure (Left problems)
    Right backupPath -> do
      removed <- trySynchronous (postCommitRemoveOwnedFile dependencies backupPath)
      case removed of
        Right () -> pure (Right backupPath)
        Left exception -> do
          cleanupProblems <- cleanupOwnedPath dependencies backupPath
          pure (Left (PostCommitIndexCleanupFailure backupPath (Text.pack (displayException exception)) : cleanupProblems))

data PublicationState = PublicationState
  { publicationTargetExists :: Bool,
    publicationBackupExists :: Bool,
    publicationCandidateExists :: Bool
  }

publicationState :: PostCommitIndexDependencies -> FilePath -> FilePath -> FilePath -> IO (Either SomeException PublicationState)
publicationState dependencies databasePath temporaryPath backupPath =
  trySynchronous
    ( PublicationState
        <$> postCommitDoesOwnedFileExist dependencies databasePath
        <*> postCommitDoesOwnedFileExist dependencies backupPath
        <*> postCommitDoesOwnedFileExist dependencies temporaryPath
    )

data PublicationOutcome
  = PublicationSucceeded
  | PublicationFailed [PostCommitIndexError]
  | PublicationFailStop [PostCommitIndexError]

data PriorTargetRecovery
  = PriorTargetRestored [Text]
  | PriorTargetUnavailable [Text] Text PublicationState
  | PriorTargetObservationFailed [Text] Text

-- | Recover the prior target after a failed replacement.  If a backup exists,
-- it is authoritative even when the target also exists: that covers a seam
-- failure reported after the replacement reached the target pathname.
-- Restoration is bounded to two attempts under the publication mask.  The
-- seam contract guarantees that on failure the prior file is at either the
-- target or the supplied backup path; an exhausted or impossible state is
-- returned as fail-stop typed data while its remaining artifacts are retained.
restorePriorTarget :: PostCommitIndexDependencies -> FilePath -> FilePath -> FilePath -> IO PriorTargetRecovery
restorePriorTarget dependencies databasePath temporaryPath backupPath = go 0 []
  where
    maximumRestoreAttempts = 2 :: Int

    go attempts restoreProblems = do
      observed <- publicationState dependencies databasePath temporaryPath backupPath
      case observed of
        Left exception ->
          pure
            ( PriorTargetObservationFailed
                restoreProblems
                ("publication state observation failed during reconciliation: " <> Text.pack (displayException exception))
            )
        Right state ->
          case (publicationTargetExists state, publicationBackupExists state) of
            (_, True)
              | attempts < maximumRestoreAttempts -> do
                  restored <- trySynchronous (postCommitRestoreDatabase dependencies backupPath databasePath)
                  case restored of
                    Left exception -> go (attempts + 1) (restoreProblems <> [statefulMessage state exception])
                    Right () -> go (attempts + 1) restoreProblems
              | otherwise ->
                  pure
                    ( PriorTargetUnavailable
                        restoreProblems
                        "restore attempt limit reached while backup remains"
                        state
                    )
            (True, False) -> pure (PriorTargetRestored restoreProblems)
            (False, False) ->
              pure
                ( PriorTargetUnavailable
                    restoreProblems
                    "replacement invariant violated: target and supplied backup are both absent"
                    state
                )

    statefulMessage state exception =
      Text.pack (displayException exception)
        <> " (target_exists="
        <> Text.pack (show (publicationTargetExists state))
        <> ", backup_exists="
        <> Text.pack (show (publicationBackupExists state))
        <> ", candidate_exists="
        <> Text.pack (show (publicationCandidateExists state))
        <> ")"

publicationStateText :: PublicationState -> Text
publicationStateText state =
  "target_exists="
    <> Text.pack (show (publicationTargetExists state))
    <> ", backup_exists="
    <> Text.pack (show (publicationBackupExists state))
    <> ", candidate_exists="
    <> Text.pack (show (publicationCandidateExists state))

withRestoreFailures :: PostCommitIndexPublishError -> [Text] -> PostCommitIndexPublishError
withRestoreFailures installProblem = \case
  [] -> installProblem
  problems -> PostCommitIndexInstallAndRestoreFailure (publishErrorText installProblem) (Text.intercalate "; " problems)

publishErrorText :: PostCommitIndexPublishError -> Text
publishErrorText = \case
  PostCommitIndexInstallFailure problem -> problem
  PostCommitIndexInstallAndRestoreFailure installProblem restoreProblem -> installProblem <> "; " <> restoreProblem
  PostCommitIndexRecoveryInvariantFailure attemptedProblem invariantProblem -> publishErrorText attemptedProblem <> "; " <> invariantProblem

installCandidate :: FilePath -> FilePath -> Maybe FilePath -> IO (Either PostCommitIndexPublishError ())
installCandidate databasePath temporaryPath backupPath = do
  installed <-
    trySynchronous $ case backupPath of
      Just path -> atomicReplaceFile databasePath temporaryPath path
      Nothing -> renameFile temporaryPath databasePath
  pure $ case installed of
    Left exception -> Left (PostCommitIndexInstallFailure (Text.pack (displayException exception)))
    Right () -> Right ()

-- | Atomically replace a same-directory target on Windows.  'ReplaceFileW'
-- opens its replacement argument without a sharing mode, which conflicts with
-- SQLite's deferred Windows handle release even after a connection has been
-- closed.  'MoveFileExW' with @MOVEFILE_REPLACE_EXISTING@ performs the
-- same-volume namespace replacement without that incompatible open.  The
-- candidate is always a sibling of the target, so omitting COPY_ALLOWED keeps
-- this a rename rather than a copy/delete sequence.  WRITE_THROUGH is retained
-- for the API's strongest completion request, while same-volume publication
-- remains a single namespace operation.
atomicReplaceFile :: FilePath -> FilePath -> FilePath -> IO ()
atomicReplaceFile databasePath temporaryPath _backupPath =
  withTString temporaryPath $ \temporaryPointer ->
    withTString databasePath $ \databasePointer ->
      failIfFalse_
        "MoveFileExW"
        (c_MoveFileExW temporaryPointer databasePointer moveFileReplaceExistingAndWriteThrough)

moveFileReplaceExistingAndWriteThrough :: DWORD
moveFileReplaceExistingAndWriteThrough = 0x00000001 .|. 0x00000008

-- | Cleanup is limited to the candidate and SQLite sidecars derived from its
-- task-owned name.  Every path is attempted in deterministic order and every
-- failure is returned as typed data; later failures never hide earlier ones.
cleanupOwnedTemporary :: PostCommitIndexDependencies -> FilePath -> IO [PostCommitIndexError]
cleanupOwnedTemporary dependencies temporaryPath =
  fmap concat . mapM removeOwned $
    ownedTemporaryPaths temporaryPath
  where
    removeOwned path = do
      removed <- trySynchronous (postCommitRemoveOwnedFile dependencies path)
      pure $ case removed of
        Left exception -> [PostCommitIndexCleanupFailure path (Text.pack (displayException exception))]
        Right () -> []

-- | Cancellation cleanup is best-effort across every owned path.  Each action
-- is caught independently so neither a synchronous cleanup failure nor a new
-- injected asynchronous exception can replace the original cancellation or
-- prevent later siblings from being attempted.
cleanupOwnedTemporaryAfterCancellation :: PostCommitIndexDependencies -> FilePath -> IO ()
cleanupOwnedTemporaryAfterCancellation dependencies temporaryPath =
  mapM_ removeOwned (ownedTemporaryPaths temporaryPath)
  where
    removeOwned path = do
      _ <- tryAny (postCommitRemoveOwnedFile dependencies path)
      pure ()

ownedTemporaryPaths :: FilePath -> [FilePath]
ownedTemporaryPaths temporaryPath =
  [ temporaryPath
  , temporaryPath <> "-journal"
  , temporaryPath <> "-shm"
  , temporaryPath <> "-wal"
  ]

cleanupOwnedPath :: PostCommitIndexDependencies -> FilePath -> IO [PostCommitIndexError]
cleanupOwnedPath dependencies path = do
  removed <- trySynchronous (postCommitRemoveOwnedFile dependencies path)
  pure $ case removed of
    Left exception -> [PostCommitIndexCleanupFailure path (Text.pack (displayException exception))]
    Right () -> []

removeIfPresent :: FilePath -> IO ()
removeIfPresent path = do
  exists <- doesFileExist path
  if exists then removeFile path else pure ()

combineFailures :: [PostCommitIndexError] -> PostCommitIndexError
combineFailures = \case
  [problem] -> problem
  problems -> PostCommitIndexMultipleFailures problems

-- | Preserve cancellation semantics while turning ordinary IO failures into
-- typed lifecycle/publication results.  Every asynchronous exception is
-- rethrown even when injected synchronously by a focused test seam.
trySynchronous :: IO a -> IO (Either SomeException a)
trySynchronous action = do
  attempted <- tryAny action
  case attempted of
    Left exception ->
      if isAsyncException exception
        then throwIO exception
        else pure (Left exception)
    Right result -> pure (Right result)

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try @SomeException

isAsyncException :: SomeException -> Bool
isAsyncException exception =
  case fromException exception :: Maybe SomeAsyncException of
    Just _ -> True
    Nothing -> False

compilerWarnings :: ColdCompilerResult -> [IndexWarning]
compilerWarnings result =
  sortOn (\warning -> (indexWarningCode warning, indexWarningMessage warning))
    [ IndexWarning
        { indexWarningCode = compilerDiagnosticCodeText (compilerDiagnosticCode diagnostic),
          indexWarningMessage = compilerDiagnosticMessage diagnostic
        }
    | diagnostic <- coldCompilerDiagnostics result,
      compilerDiagnosticSeverity diagnostic == CompilerDiagnosticWarning
    ]
