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
module Adrai.Service.PostCommitIndex.Internal
  ( IndexWarning (..),
    PostCommitIndexError (..),
    PostCommitIndexPublishError (..),
    PostCommitIndexResult (..),
    PostCommitIndexDependencies (..),
    postCommitIndexDependencies,
    compilePostCommitIndex,
    compilePostCommitIndexWithAttribution,
    compilePostCommitIndexWithAttributionAndRefresh,
    compilePostCommitIndexWith,
    clonePostCommitIndexWithHistoryCount,
    clonePostCommitIndexWithHistoryCountAndRefresh,
    clonePostCommitIndexValidatedSourceWithAfterCopyHookForTest,
    clonePostCommitIndexFromLeaseWithHistoryCountAndRefresh,
    clonePostCommitIndexTrustedSource,
  )
where

import Adrai.Compiler
  ( ColdCompilerError,
    ColdCompilerResult (..),
    coldCompileRepository,
    coldCompileRepositoryWithAttribution,
  )
import Adrai.Compiler.Attribution
  ( AttributionPhase (CompileOutcomeEvaluation, DatabaseClose, PostCloseProvenanceRefresh, PostCloseFingerprintValidation, ImmutablePublication),
    ColdCompileAttribution,
    inertColdCompileAttribution,
    withAttributionPhase,
  )
import Adrai.Compiler.CacheSelection
  ( refreshCacheMaterializationFingerprint,
    validateCacheContract,
  )
import Adrai.Compiler.CacheSelection.Internal
  ( refreshCachePublicationMaterializationFingerprint,
  )
import Adrai.Compiler.CacheLease.Internal
  ( CacheLease,
    copyLeaseSourceTo,
    readLeaseAcceptedFacts,
  )
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
import Control.Exception (SomeAsyncException, SomeException, displayException, evaluate, fromException, mask, throwIO, try)
import Data.Bits ((.|.))
import Data.IORef (newIORef, readIORef, writeIORef)
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
    postCommitRestoreDatabase :: FilePath -> FilePath -> IO (),
    postCommitAttribution :: ColdCompileAttribution
  }

-- | Local handle ownership is discharged at the instant the close seam is
-- invoked, regardless of how that action reports its outcome.  This prevents
-- an after-effect exception from causing a second close invocation.
data ConnectionOwnership
  = OwnedConnection Connection
  | DischargedConnection

-- | A closed candidate whose main file and SQLite sidecars transfer together
-- into publication.  The constructor stays internal so caller code has one
-- visible handoff point while still under its surrounding mask.
newtype PublicationCandidate = PublicationCandidate FilePath

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
      postCommitRestoreDatabase = renameFile,
      postCommitAttribution = inertColdCompileAttribution
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

-- | Compile into a private candidate and run an additional projection refresh
-- before the candidate is published.  Failures leave the immutable target
-- untouched, just like a compilation failure.
compilePostCommitIndexWithAttributionAndRefresh :: ColdCompileAttribution -> Repository -> GitOid -> FilePath -> (FilePath -> IO ()) -> IO PostCommitIndexResult
compilePostCommitIndexWithAttributionAndRefresh attribution repository commitOid databasePath refreshCandidate =
  compilePostCommitIndexWithAttributionDependenciesAndRefresh
    attribution
    (postCommitIndexDependencies {postCommitColdCompile = coldCompileRepositoryWithAttribution attribution})
    repository commitOid databasePath (Just refreshCandidate)

-- | Publish a selected immutable cache under the mutable current-alias path.
-- The caller has already established that @sourcePath@ is the authoritative
-- immutable snapshot for @targetRevision@: either this same process just
-- published it, or cache selection already accepted it as an exact archive.
-- Re-validating that source here is therefore redundant and can spuriously
-- reject a fresh alias repair while the archived snapshot remains the source of
-- truth.  Cross-revision reuse keeps its stricter validation below.
-- | Tree-identical reuse has already performed a bounded history proof.  Carry
-- its observed commit count into the derived snapshot instead of claiming the
-- reused result scanned no history.  Alias publication passes 'Nothing' and
-- preserves the archive's existing count.
clonePostCommitIndexWithHistoryCount :: FilePath -> GitOid -> FilePath -> Maybe Int -> IO PostCommitIndexResult
clonePostCommitIndexWithHistoryCount _sourcePath _targetRevision _databasePath _historyCommitsScanned =
  pure
    PostCommitIndexResult
      { postCommitIndexed = False,
        postCommitDatabase = Nothing,
        postCommitIndexRevision = Nothing,
        postCommitIndexWarnings = [],
        postCommitIndexError = Just (PostCommitIndexCloneFailure "cross-revision v3 clone requires a target provenance refresh callback")
      }

-- | Like 'clonePostCommitIndexWithHistoryCount', but lets the caller update the
-- private candidate after retargeting and before the single publication step.
-- The callback is deliberately given only the disposable path, making it
-- impossible to expose a partially refreshed archive to readers.
clonePostCommitIndexWithHistoryCountAndRefresh :: FilePath -> GitOid -> FilePath -> Maybe Int -> (FilePath -> IO ()) -> IO PostCommitIndexResult
clonePostCommitIndexWithHistoryCountAndRefresh sourcePath targetRevision databasePath historyCommitsScanned refreshCandidate =
  clonePostCommitIndexValidatedSource sourcePath targetRevision databasePath historyCommitsScanned (Just refreshCandidate)

-- | Exact alias publication is already backed by an authoritative immutable
-- archive decision, so it uses the trusted raw-source route without a second
-- source scan.
clonePostCommitIndexTrustedSource :: FilePath -> GitOid -> FilePath -> Maybe Int -> Maybe (FilePath -> IO ()) -> IO PostCommitIndexResult
clonePostCommitIndexTrustedSource sourcePath targetRevision databasePath historyCommitsScanned maybeRefresh =
  clonePostCommitIndexAfterCopy (copyFile sourcePath) Nothing (pure ()) targetRevision databasePath historyCommitsScanned maybeRefresh

-- | A raw cross-revision source is copied into its owned publication candidate
-- before validation.  Validating the public source and reopening it to copy
-- would otherwise allow a replacement to stale the proof.  This route is
-- structurally distinct from both trusted exact publication and the scoped
-- lease copy route.
clonePostCommitIndexValidatedSource :: FilePath -> GitOid -> FilePath -> Maybe Int -> Maybe (FilePath -> IO ()) -> IO PostCommitIndexResult
clonePostCommitIndexValidatedSource sourcePath targetRevision databasePath historyCommitsScanned maybeRefresh =
  clonePostCommitIndexAfterCopy (copyFile sourcePath) (Just validateCacheContract) (pure ()) targetRevision databasePath historyCommitsScanned maybeRefresh

-- | Hidden test seam for the copy/validation TOCTOU boundary.  Its hook is
-- nullary, so a focused test can mutate only the separately owned source or
-- target fixture; it never receives the private publication candidate.
clonePostCommitIndexValidatedSourceWithAfterCopyHookForTest :: FilePath -> GitOid -> FilePath -> Maybe Int -> (FilePath -> IO ()) -> IO () -> IO PostCommitIndexResult
clonePostCommitIndexValidatedSourceWithAfterCopyHookForTest sourcePath targetRevision databasePath historyCommitsScanned refreshCandidate afterCopy =
  clonePostCommitIndexAfterCopy
    (copyFile sourcePath)
    (Just validateCacheContract)
    afterCopy
    targetRevision
    databasePath
    historyCommitsScanned
    (Just refreshCandidate)

-- | Clone from a scoped validated private lease.  Reading its pathless facts
-- first performs the owner/Open check before any publication temporary exists;
-- the copy itself remains checked and is the only source access.
clonePostCommitIndexFromLeaseWithHistoryCountAndRefresh :: CacheLease scope -> GitOid -> FilePath -> Maybe Int -> (FilePath -> IO ()) -> IO PostCommitIndexResult
clonePostCommitIndexFromLeaseWithHistoryCountAndRefresh lease targetRevision databasePath historyCommitsScanned refreshCandidate = do
  checked <- trySynchronous (readLeaseAcceptedFacts lease)
  case checked of
    Left exception -> pure (cloneFailure (Text.pack (displayException exception)))
    Right _ ->
      clonePostCommitIndexAfterCopy
        (copyLeaseSourceTo lease)
        Nothing
        (pure ())
        targetRevision
        databasePath
        historyCommitsScanned
        (Just refreshCandidate)

-- | Common post-copy pipeline.  Every route must retarget, refresh, validate,
-- and publish the disposable candidate exactly as before; only source authority
-- and the pre-copy validation decision differ.
clonePostCommitIndexAfterCopy :: (FilePath -> IO ()) -> Maybe (FilePath -> IO Bool) -> IO () -> GitOid -> FilePath -> Maybe Int -> Maybe (FilePath -> IO ()) -> IO PostCommitIndexResult
clonePostCommitIndexAfterCopy copySource validateCopiedCandidate afterCopy targetRevision databasePath historyCommitsScanned maybeRefresh = mask $ \restore -> do
  candidate <- createCandidate postCommitIndexDependencies databasePath
  case candidate of
    Left problems -> pure (cloneFailureError (combineFailures problems))
    Right temporaryPath -> do
      -- The candidate is now owned.  Do not let a restored operation throw past
      -- this boundary: doing so would strand the SQLite file or a sidecar.  In
      -- particular, a connection is closed exactly once after every successful
      -- open, before its outcome is interpreted.
      prepared <-
        tryAny . restore $ do
          copySource temporaryPath
          afterCopy
          case validateCopiedCandidate of
            Nothing -> pure (Right ())
            Just validate -> do
              valid <- validate temporaryPath
              pure $
                if valid
                  then Right ()
                  else Left (PostCommitIndexCloneFailure "cache clone source failed the canonical metadata or integrity contract")
      case prepared of
        Left exception -> finishOwnedCandidate temporaryPath (Just (exception, PostCommitIndexCloneFailure . exceptionText)) [] Nothing
        Right (Left problem) -> finishOwnedCandidate temporaryPath Nothing [problem] Nothing
        Right (Right ()) -> do
          opened <- tryAny (restore (open temporaryPath))
          case opened of
            Left exception -> finishOwnedCandidate temporaryPath (Just (exception, PostCommitIndexOpenFailure . exceptionText)) [] Nothing
            Right connection -> do
              retargeted <- tryAny (restore (retargetCachedRevision connection targetRevision historyCommitsScanned))
              -- Still masked.  'tryAny' catches an interruptible close so the
              -- original operation exception remains the outcome authority.
              closed <- tryAny (postCommitCloseDatabase postCommitIndexDependencies connection)
              case (retargeted, closed) of
                (Left exception, closeResult) ->
                  finishOwnedCandidate
                    temporaryPath
                    (Just (exception, PostCommitIndexCloneFailure . exceptionText))
                    []
                    (Just closeResult)
                (Right (), Left closeException) ->
                  finishOwnedCandidate temporaryPath Nothing [] (Just (Left closeException))
                (Right (), Right ()) -> do
                  refreshed <- tryAny (restore (recommitAndMaybeValidateCandidate maybeRefresh temporaryPath))
                  case refreshed of
                    Left exception ->
                      finishOwnedCandidate temporaryPath (Just (exception, PostCommitIndexCloneFailure . exceptionText)) [] Nothing
                    Right () -> do
                      -- Publication takes ownership of the candidate before
                      -- it performs any observation or namespace operation.
                      -- Its masked resolver either sweeps that ownership or
                      -- deliberately retains fail-stop recovery evidence.
                      published <-
                        publishCandidate
                          inertColdCompileAttribution
                          postCommitIndexDependencies
                          databasePath
                          (PublicationCandidate temporaryPath)
                      case published of
                        PublicationSucceeded ->
                          pure (PostCommitIndexResult True (Just databasePath) (Just targetRevision) [] Nothing)
                        PublicationFailed problems ->
                          pure (cloneFailureError (combineFailures problems))
                        PublicationFailStop problems ->
                          -- Publication recovery deliberately retains its
                          -- evidence; this is not a disposable pre-publication
                          -- candidate any more.
                          pure (cloneFailureError (combineFailures problems))
  where
    exceptionText = Text.pack . displayException

    -- Cleanup priority is intentional: an operation's asynchronous exception
    -- keeps its identity, then an asynchronous close/removal dominates any
    -- synchronous failure, and only then do we construct typed failure data.
    finishOwnedCandidate temporaryPath initiating typedProblems maybeClose = do
      cleanup <- cleanupOwnedTemporaryCaptured postCommitIndexDependencies temporaryPath
      case initiating of
        Just (exception, _)
          | isAsyncException exception -> throwIO exception
        _ ->
          case maybeClose of
            Just (Left exception)
              | isAsyncException exception -> throwIO exception
            _ ->
              case cleanupAsyncException cleanup of
                Just exception -> throwIO exception
                Nothing ->
                  let initiatingProblems =
                        case initiating of
                          Just (exception, toProblem) -> [toProblem exception]
                          Nothing -> []
                      closeProblems =
                        case maybeClose of
                          Just (Left exception) -> [PostCommitIndexCloseFailure (exceptionText exception)]
                          _ -> []
                   in pure (cloneFailureError (combineFailures (initiatingProblems <> typedProblems <> closeProblems <> cleanupTypedProblems cleanup)))

cloneFailure :: Text -> PostCommitIndexResult
cloneFailure = cloneFailureError . PostCommitIndexCloneFailure

cloneFailureError :: PostCommitIndexError -> PostCommitIndexResult
cloneFailureError problem = PostCommitIndexResult False Nothing Nothing [] (Just problem)

retargetCachedRevision :: Connection -> GitOid -> Maybe Int -> IO ()
retargetCachedRevision connection revision historyCommitsScanned = do
  metadata <- query_ connection "SELECT key,value FROM meta ORDER BY key" :: IO [(Text, Text)]
  case (lookup "schema" metadata, lookup "compiler_abi" metadata, lookup "materializer" metadata, lookup "materialization_fingerprint" metadata, lookup "source_fingerprint" metadata, lookup "resolved_oid" metadata, lookup "requested_revision" metadata) of
    (Just "adrai-cache/3", Just "adrai-cold-compiler/1", Just _, Just _, Just _, Just _, Just _) -> do
      -- The caller may reach this point only after proving a bounded,
      -- compiler-irrelevant delta.  The source was integrity-validated before
      -- copying, and only revision labels change; source/materialization
      -- fingerprints remain intact and are never fabricated or replaced.
      execute connection "UPDATE meta SET value=? WHERE key=?" (gitOidText revision, "resolved_oid" :: Text)
      execute connection "UPDATE meta SET value=? WHERE key=?" (gitOidText revision, "requested_revision" :: Text)
      case historyCommitsScanned of
        Nothing -> pure ()
        Just count | count >= 0 -> execute connection "UPDATE meta SET value=? WHERE key=?" (Text.pack (show count), "history_commits_scanned" :: Text)
        _ -> ioError (userError "tree-identical cache clone requires a nonnegative bounded history count")
    _ -> ioError (userError "cache clone source does not contain a complete canonical compiler metadata set")

-- | Parameterized form with deterministic failure composition.  If compilation
-- and close both fail, errors are retained in lifecycle order: compile, close.
compilePostCommitIndexWith :: PostCommitIndexDependencies -> Repository -> GitOid -> FilePath -> IO PostCommitIndexResult
compilePostCommitIndexWith dependencies repository commitOid databasePath =
  compilePostCommitIndexWithAttributionDependenciesAndRefresh (postCommitAttribution dependencies) dependencies repository commitOid databasePath Nothing

compilePostCommitIndexWithAttributionDependencies :: ColdCompileAttribution -> PostCommitIndexDependencies -> Repository -> GitOid -> FilePath -> IO PostCommitIndexResult
compilePostCommitIndexWithAttributionDependencies attribution dependencies repository commitOid databasePath =
  compilePostCommitIndexWithAttributionDependenciesAndRefresh attribution dependencies repository commitOid databasePath Nothing

compilePostCommitIndexWithAttributionDependenciesAndRefresh :: ColdCompileAttribution -> PostCommitIndexDependencies -> Repository -> GitOid -> FilePath -> Maybe (FilePath -> IO ()) -> IO PostCommitIndexResult
compilePostCommitIndexWithAttributionDependenciesAndRefresh attribution dependencies repository commitOid databasePath maybeRefresh = do
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
              | isAsyncException exception -> cleanupAfterCancellation DischargedConnection temporaryPath exception
              | otherwise -> do
                  cleanupProblems <- cleanupOwnedTemporary dependencies temporaryPath
                  pure (failure (combineFailures (PostCommitIndexOpenFailure (exceptionText exception) : cleanupProblems)))
            Right connection -> do
              compiled <-
                tryAny
                  ( restore $
                      do
                        outcome <- postCommitColdCompile dependencies connection revision
                        withAttributionPhase attribution CompileOutcomeEvaluation (forceCompilerOutcome outcome)
                  )
              case compiled of
                Left exception
                  | isAsyncException exception -> cleanupAfterCancellation (OwnedConnection connection) temporaryPath exception
                _ -> do
                  ownershipRef <- newIORef (OwnedConnection connection)
                  closeActionRef <- newIORef Nothing
                  wrappedClose <-
                    tryAny
                      ( restore
                          ( withAttributionPhase
                              attribution
                              DatabaseClose
                              (captureCloseAction closeActionRef ownershipRef connection)
                          )
                      )
                  capturedCloseAction <- readIORef closeActionRef
                  let closed = resolveClosePhase wrappedClose capturedCloseAction
                  finalOwnership <- readIORef ownershipRef
                  case closed of
                    Left exception
                      | isAsyncException exception -> cleanupAfterCancellation finalOwnership temporaryPath exception
                      | otherwise -> do
                          cleanupClose <- closeIfOwned ownershipRef
                          case cleanupClose of
                            Left cleanupException
                              | isAsyncException cleanupException -> do
                                  cleanupOwnedTemporaryAfterCancellation dependencies temporaryPath
                                  throwIO cleanupException
                            _ ->
                              complete
                                restore
                                revision
                                temporaryPath
                                compiled
                                closed
                                (closeCleanupProblems cleanupClose)
                    Right () -> complete restore revision temporaryPath compiled closed []

    -- Cancellation retains precedence over finalizer failures: once a database
    -- handle has escaped acquisition, attempt its close and every owned-path
    -- cleanup while masked, then rethrow the original asynchronous exception.
    -- Catching finalizer exceptions here prevents a later cleanup failure from
    -- erasing the cancellation that initiated this unwind.
    cleanupAfterCancellation connectionOwnership temporaryPath cancellation = do
      case connectionOwnership of
        DischargedConnection -> pure ()
        OwnedConnection connection -> do
          _ <- tryAny (postCommitCloseDatabase dependencies connection)
          pure ()
      cleanupOwnedTemporaryAfterCancellation dependencies temporaryPath
      throwIO cancellation

    -- The close action is entered only after ownership is discharged under a
    -- mask.  An attribution prelude failure leaves it Owned, so cleanup invokes
    -- the seam once; any failure after action entry observes Discharged and is
    -- never retried.
    dischargeAndClose ownershipRef connection = mask $ \_ -> do
      writeIORef ownershipRef DischargedConnection
      postCommitCloseDatabase dependencies connection

    captureCloseAction closeActionRef ownershipRef connection = mask $ \_ -> do
      closeAction <- tryAny (dischargeAndClose ownershipRef connection)
      writeIORef closeActionRef (Just closeAction)
      case closeAction of
        Left exception -> throwIO exception
        Right () -> pure ()

    -- The saved action exception is authoritative when it is asynchronous.
    -- Otherwise an asynchronous attribution finalizer outranks a synchronous
    -- action failure.  A completed action plus wrapper failure keeps the
    -- wrapper failure, while ownership remains Discharged in every action row.
    resolveClosePhase wrappedClose capturedCloseAction =
      case wrappedClose of
        Right () -> Right ()
        Left wrapperException ->
          case capturedCloseAction of
            Just (Left actionException)
              | isAsyncException actionException -> Left actionException
              | isAsyncException wrapperException -> Left wrapperException
              | otherwise -> Left actionException
            _ -> Left wrapperException

    closeIfOwned ownershipRef = do
      ownership <- readIORef ownershipRef
      case ownership of
        DischargedConnection -> pure (Right ())
        OwnedConnection connection -> tryAny (dischargeAndClose ownershipRef connection)

    closeCleanupProblems = \case
      Left exception -> [PostCommitIndexCloseFailure (exceptionText exception)]
      Right () -> []

    complete restore revision temporaryPath compiled closed additionalCloseProblems =
      case (compileOutcome compiled, closeOutcome closed) of
        (Left compileError, Left closeError) -> do
          cleanupProblems <- cleanupOwnedTemporary dependencies temporaryPath
          pure (failure (combineFailures ([compileError, closeError] <> additionalCloseProblems <> cleanupProblems)))
        (Left compileError, Right ()) -> do
          cleanupProblems <- cleanupOwnedTemporary dependencies temporaryPath
          pure (failure (combineFailures ([compileError] <> additionalCloseProblems <> cleanupProblems)))
        (Right _, Left closeError) -> do
          cleanupProblems <- cleanupOwnedTemporary dependencies temporaryPath
          pure (failure (combineFailures ([closeError] <> additionalCloseProblems <> cleanupProblems)))
        (Right result, Right ()) -> do
          refreshed <- tryAny (restore $ do
            withAttributionPhase attribution PostCloseProvenanceRefresh (runCandidateRefreshCallback maybeRefresh temporaryPath)
            withAttributionPhase attribution PostCloseFingerprintValidation (runCandidateFingerprintValidation maybeRefresh temporaryPath))
          case refreshed of
            Left refreshFailure
              | isAsyncException refreshFailure ->
                  cleanupAfterCancellation DischargedConnection temporaryPath refreshFailure
            Left refreshFailure -> do
              cleanupProblems <- cleanupOwnedTemporary dependencies temporaryPath
              pure (failure (combineFailures (PostCommitIndexCompileException (exceptionText refreshFailure) : cleanupProblems)))
            Right () -> do
              published <- publishCandidate attribution dependencies databasePath (PublicationCandidate temporaryPath)
              case published of
                PublicationFailed publishProblems ->
                  pure (failure (combineFailures publishProblems))
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

    -- 'tryAny' catches exceptions raised by its action, not thunks returned
    -- from it.  Demand both the typed outcome and successful compiler result
    -- before the database closes, while still inside the restored action.
    forceCompilerOutcome outcome = do
      forcedOutcome <- evaluate outcome
      case forcedOutcome of
        Left problem -> Left <$> evaluate problem
        Right result -> Right <$> evaluate result

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

-- | A provenance refresh mutates the private archive after cold materialization
-- or retargeting.  Recommit its digest and prove the public archive contract
-- before the one-way rename, including current-ref/ref-observation coherence.
runCandidateRefreshCallback :: Maybe (FilePath -> IO ()) -> FilePath -> IO ()
runCandidateRefreshCallback maybeRefresh temporaryPath =
  maybe (pure ()) ($ temporaryPath) maybeRefresh

-- | The fingerprint must be recomputed after every private candidate mutation.
-- A provenance-refresh callback additionally requires the public archive
-- contract before publication; ordinary cold materialization retains its
-- existing fingerprint-only publication rule.
runCandidateFingerprintValidation :: Maybe (FilePath -> IO ()) -> FilePath -> IO ()
runCandidateFingerprintValidation maybeRefresh temporaryPath =
  case maybeRefresh of
    -- Preserve the ordinary cold-materialization helper path exactly.
    Nothing -> refreshCacheMaterializationFingerprint temporaryPath
    -- A provenance refresh owns a private candidate, so fuse its canonical
    -- row load, replacement-fingerprint proof, existing publication contract,
    -- and lone metadata update into one rollback-protected transaction.
    Just _ -> refreshCachePublicationMaterializationFingerprint temporaryPath

-- | Retargeting changes the committed payload metadata even when an ordinary
-- post-commit caller has no provenance overlay to synchronize.  Refreshed
-- candidates are stricter: only a refresh callback represents a request to
-- publish provenance projection data, and that path must prove the complete
-- public contract before rename.
recommitAndMaybeValidateCandidate :: Maybe (FilePath -> IO ()) -> FilePath -> IO ()
recommitAndMaybeValidateCandidate maybeRefresh temporaryPath =
  runCandidateRefreshCallback maybeRefresh temporaryPath
    >> runCandidateFingerprintValidation maybeRefresh temporaryPath

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
createOwnedTemporary dependencies databasePath marker = mask $ \restore -> do
  acquired <- tryAny (restore (postCommitOpenTemporary dependencies (takeDirectory databasePath) (takeFileName databasePath <> marker)))
  case acquired of
    Left exception
      | isAsyncException exception -> throwIO exception
      | otherwise -> pure (Left [PostCommitIndexOpenFailure (exceptionText exception)])
    Right (temporaryPath, handle) -> do
      -- Once a name/handle pair exists it is owned before the close attempt.
      -- Catch interruptible close failure, sweep every derived SQLite path, and
      -- only then select the exception that leaves this boundary.
      closed <- tryAny (postCommitCloseTemporary dependencies handle)
      case closed of
        Right () -> pure (Right temporaryPath)
        Left closeException -> do
          retried <- tryAny (postCommitCloseTemporary dependencies handle)
          cleanup <- cleanupOwnedTemporaryCaptured dependencies temporaryPath
          case () of
            _ | isAsyncException closeException -> throwIO closeException
              | Left retryException <- retried, isAsyncException retryException -> throwIO retryException
              | Just cleanupException <- cleanupAsyncException cleanup -> throwIO cleanupException
              | otherwise ->
                  let closeProblems =
                        PostCommitIndexTemporaryCloseFailure (exceptionText closeException)
                          : case retried of
                            Left retryException -> [PostCommitIndexTemporaryCloseFailure (exceptionText retryException)]
                            Right () -> []
                   in pure (Left (closeProblems <> cleanupTypedProblems cleanup))
  where
    exceptionText = Text.pack . displayException

-- | Transfer a closed candidate into the publication boundary while the caller
-- is still masked.  Attribution is run inside that boundary: if its prelude
-- fails before the action begins, the candidate is still swept here; once the
-- action begins, its own captured state machine has sole cleanup authority.
publishCandidate :: ColdCompileAttribution -> PostCommitIndexDependencies -> FilePath -> PublicationCandidate -> IO PublicationOutcome
publishCandidate attribution dependencies databasePath (PublicationCandidate temporaryPath) = mask $ \restore -> do
  completedRef <- newIORef Nothing
  attributed <-
    tryAny
      ( restore
          ( withAttributionPhase attribution ImmutablePublication $ mask $ \_ -> do
              completed <- tryAny (publishCandidateOwned dependencies databasePath temporaryPath)
              writeIORef completedRef (Just completed)
              case completed of
                Left publicationException -> throwIO publicationException
                Right outcome -> pure outcome
          )
      )
  completed <- readIORef completedRef
  case attributed of
    Right outcome -> pure outcome
    Left attributionException ->
      case completed of
        Just (Left publicationException) ->
          -- Publication already reconciled and swept before surfacing its
          -- initiating async exception; attribution finalization is secondary.
          throwIO publicationException
        Just (Right _) ->
          -- The publication outcome already discharged or deliberately
          -- retained ownership.  A post-action attribution failure must not
          -- trigger a second cleanup pass.
          throwIO attributionException
        Nothing -> do
          cleanup <- cleanupPublicationArtifactsCaptured dependencies temporaryPath Nothing
          finishPublication
            False
            (asyncOnly attributionException)
            [ PostCommitIndexPublishFailure
                ( PostCommitIndexInstallFailure
                    ("publication attribution failed before action entry: " <> exceptionText attributionException)
                )
            ]
            []
            cleanup
  where
    exceptionText = Text.pack . displayException

-- | Publish a closed same-directory candidate under an exception mask.  For an
-- existing target, the dependency seam receives an absent task-owned backup
-- pathname so narrow tests can model a replace-style partial failure.  The
-- production primitive is a same-volume atomic rename and does not create that
-- backup, but reconciliation still observes all three owned paths and either
-- retains the prior target or fails stop with the remaining recovery artifacts
-- intact.
publishCandidateOwned :: PostCommitIndexDependencies -> FilePath -> FilePath -> IO PublicationOutcome
publishCandidateOwned dependencies databasePath temporaryPath = mask $ \_ -> do
  observedTarget <- tryAny (postCommitDoesOwnedFileExist dependencies databasePath)
  case observedTarget of
    Left exception -> do
      cleanup <- cleanupPublicationArtifactsCaptured dependencies temporaryPath Nothing
      finishPublication
        False
        (asyncOnly exception)
        [ PostCommitIndexPublishFailure
            ( PostCommitIndexRecoveryInvariantFailure
                (PostCommitIndexInstallFailure "publication not attempted because target state could not be observed")
                ("initial target observation failed: " <> exceptionText exception)
            )
        ]
        []
        cleanup
    Right targetExists ->
      if targetExists
        then publishReplacement
        else publishWithoutPriorTarget
  where
    publishWithoutPriorTarget = do
      installed <- captureInstall Nothing
      observed <- observePublicationState dependencies databasePath temporaryPath Nothing
      case observed of
        PublicationObserved state
          | installationProven state -> finishInstalled installed Nothing
          | not (publicationTargetExists state) && publicationCandidateExists state ->
              finishUninstalledWithoutPrior installed state
          | not (publicationTargetExists state) && not (publicationCandidateExists state) ->
              finishUninstalledWithoutPrior installed state
          | otherwise ->
              finishObservationFailStop
                installed
                Nothing
                ("publication state is ambiguous without a prior target; " <> publicationStateText state)
                []
        PublicationObservationFailed failures ->
          finishObservationFailStop
            installed
            Nothing
            ("post-install publication state observation failed: " <> observationFailuresText failures)
            (observationAsyncExceptions failures)

    publishReplacement = do
      reserved <- tryAny (createOwnedTemporary dependencies databasePath ".post-commit-backup-")
      case reserved of
        Left exception -> do
          cleanup <- cleanupPublicationArtifactsCaptured dependencies temporaryPath Nothing
          finishPublication
            False
            (asyncOnly exception)
            [PostCommitIndexOpenFailure (exceptionText exception)]
            []
            cleanup
        Right (Left problems) -> do
          cleanup <- cleanupPublicationArtifactsCaptured dependencies temporaryPath Nothing
          finishPublication False Nothing problems [] cleanup
        Right (Right backupPath) -> do
          removedReservation <- tryAny (postCommitRemoveOwnedFile dependencies backupPath)
          case removedReservation of
            Left exception -> do
              cleanup <- cleanupPublicationArtifactsCaptured dependencies temporaryPath (Just backupPath)
              finishPublication
                False
                (asyncOnly exception)
                [PostCommitIndexCleanupFailure backupPath (exceptionText exception)]
                []
                cleanup
            Right () -> do
              installed <- captureInstall (Just backupPath)
              observed <- observePublicationState dependencies databasePath temporaryPath (Just backupPath)
              case observed of
                PublicationObserved state
                  | installationProven state -> finishInstalled installed (Just backupPath)
                  | otherwise -> reconcileReplacement installed backupPath state
                PublicationObservationFailed failures ->
                  finishObservationFailStop
                    installed
                    (Just backupPath)
                    ("post-install publication state observation failed: " <> observationFailuresText failures)
                    (observationAsyncExceptions failures)

    captureInstall backupPath = do
      attempted <- tryAny (postCommitInstallDatabase dependencies databasePath temporaryPath backupPath)
      pure $ case attempted of
        Left exception ->
          CapturedInstall
            { capturedInstallSucceeded = False,
              capturedInstallAsync = asyncOnly exception,
              capturedInstallProblem =
                if isAsyncException exception
                  then Nothing
                  else Just (PostCommitIndexInstallFailure (exceptionText exception))
            }
        Right (Left problem) ->
          CapturedInstall
            { capturedInstallSucceeded = False,
              capturedInstallAsync = Nothing,
              capturedInstallProblem = Just problem
            }
        Right (Right ()) ->
          CapturedInstall
            { capturedInstallSucceeded = True,
              capturedInstallAsync = Nothing,
              capturedInstallProblem = Nothing
            }

    finishInstalled installed backupPath = do
      cleanup <- cleanupPublicationArtifactsCaptured dependencies temporaryPath backupPath
      finishPublication
        False
        (capturedInstallAsync installed)
        (capturedInstallProblems installed)
        []
        cleanup

    finishUninstalledWithoutPrior installed state = do
      cleanup <- cleanupPublicationArtifactsCaptured dependencies temporaryPath Nothing
      finishPublication
        False
        (capturedInstallAsync installed)
        [ PostCommitIndexPublishFailure
            ( fromMaybeInstallProblem
                installed
                ( "publication did not install a target; "
                    <> publicationStateText state
                )
            )
        ]
        []
        cleanup

    reconcileReplacement installed backupPath initialState = do
      recovered <- restorePriorTargetCaptured dependencies databasePath temporaryPath backupPath initialState
      case recovered of
        PriorTargetRestored restoreProblems reconciliationAsync -> do
          cleanup <- cleanupPublicationArtifactsCaptured dependencies temporaryPath (Just backupPath)
          finishPublication
            False
            (capturedInstallAsync installed)
            [ PostCommitIndexPublishFailure
                (withRestoreFailures (fromMaybeInstallProblem installed "replacement was not proven") restoreProblems)
            ]
            reconciliationAsync
            cleanup
        PriorTargetUnavailable restoreProblems reconciliationAsync invariantReason state -> do
          -- Fail stop: candidate main and backup may be the only remaining
          -- copies of the new and prior databases.  Retain both, but sidecars
          -- are independently disposable and are still swept all-attempt.
          cleanup <- cleanupPublicationSidecarsCaptured dependencies temporaryPath
          let attemptedProblem = withRestoreFailures (fromMaybeInstallProblem installed "replacement was not proven") restoreProblems
              invariantProblem =
                PostCommitIndexRecoveryInvariantFailure
                  attemptedProblem
                  (invariantReason <> "; " <> publicationStateText state)
          finishPublication
            True
            (capturedInstallAsync installed)
            [PostCommitIndexPublishFailure invariantProblem]
            reconciliationAsync
            cleanup
        PriorTargetObservationFailed restoreProblems reconciliationAsync observationProblem -> do
          cleanup <- cleanupPublicationSidecarsCaptured dependencies temporaryPath
          let attemptedProblem = withRestoreFailures (fromMaybeInstallProblem installed "replacement was not proven") restoreProblems
              invariantProblem = PostCommitIndexRecoveryInvariantFailure attemptedProblem observationProblem
          finishPublication
            True
            (capturedInstallAsync installed)
            [PostCommitIndexPublishFailure invariantProblem]
            reconciliationAsync
            cleanup

    finishObservationFailStop installed _backupPath detail reconciliationAsync = do
      cleanup <- cleanupPublicationSidecarsCaptured dependencies temporaryPath
      let invariantProblem =
            PostCommitIndexRecoveryInvariantFailure
              (fromMaybeInstallProblem installed "publication installation state could not be proven")
              detail
      -- The supplied backup, if any, and candidate main are retained because
      -- observation did not establish which one carries prior/new authority.
      finishPublication
        True
        (capturedInstallAsync installed)
        [PostCommitIndexPublishFailure invariantProblem]
        reconciliationAsync
        cleanup

    exceptionText = Text.pack . displayException

data CapturedInstall = CapturedInstall
  { capturedInstallSucceeded :: Bool,
    capturedInstallAsync :: Maybe SomeException,
    capturedInstallProblem :: Maybe PostCommitIndexPublishError
  }

capturedInstallProblems :: CapturedInstall -> [PostCommitIndexError]
capturedInstallProblems installed =
  case capturedInstallProblem installed of
    Nothing -> []
    Just problem -> [PostCommitIndexPublishFailure problem]

fromMaybeInstallProblem :: CapturedInstall -> Text -> PostCommitIndexPublishError
fromMaybeInstallProblem installed fallback =
  case capturedInstallProblem installed of
    Just problem -> problem
    Nothing ->
      PostCommitIndexInstallFailure
        ( if capturedInstallSucceeded installed
            then fallback
            else "publication was interrupted before reporting a typed install failure"
        )

asyncOnly :: SomeException -> Maybe SomeException
asyncOnly exception
  | isAsyncException exception = Just exception
  | otherwise = Nothing

data PublicationState = PublicationState
  { publicationTargetExists :: Bool,
    publicationBackupExists :: Bool,
    publicationCandidateExists :: Bool
  }

data PublicationObservation
  = PublicationObserved PublicationState
  | PublicationObservationFailed [(Text, SomeException)]

observePublicationState :: PostCommitIndexDependencies -> FilePath -> FilePath -> Maybe FilePath -> IO PublicationObservation
observePublicationState dependencies databasePath temporaryPath maybeBackupPath = do
  target <- tryAny (postCommitDoesOwnedFileExist dependencies databasePath)
  candidate <- tryAny (postCommitDoesOwnedFileExist dependencies temporaryPath)
  backup <-
    case maybeBackupPath of
      Nothing -> pure (Right False)
      Just backupPath -> tryAny (postCommitDoesOwnedFileExist dependencies backupPath)
  case (target, candidate, backup) of
    (Right targetExists, Right candidateExists, Right backupExists) ->
      pure
        ( PublicationObserved
            PublicationState
              { publicationTargetExists = targetExists,
                publicationBackupExists = backupExists,
                publicationCandidateExists = candidateExists
              }
        )
    _ ->
      pure
        ( PublicationObservationFailed
            ( observationFailure "target" target
                <> observationFailure "candidate" candidate
                <> observationFailure "backup" backup
            )
        )
  where
    observationFailure label = \case
      Left exception -> [(label, exception)]
      Right _ -> []

observationAsyncExceptions :: [(Text, SomeException)] -> [SomeException]
observationAsyncExceptions failures =
  [exception | (_, exception) <- failures, isAsyncException exception]

observationFailuresText :: [(Text, SomeException)] -> Text
observationFailuresText failures =
  Text.intercalate
    "; "
    [label <> "=" <> Text.pack (displayException exception) | (label, exception) <- failures]

installationProven :: PublicationState -> Bool
installationProven state =
  publicationTargetExists state && not (publicationCandidateExists state)

data PublicationOutcome
  = PublicationSucceeded
  | PublicationFailed [PostCommitIndexError]
  | PublicationFailStop [PostCommitIndexError]

data PriorTargetRecovery
  = PriorTargetRestored [Text] [SomeException]
  | PriorTargetUnavailable [Text] [SomeException] Text PublicationState
  | PriorTargetObservationFailed [Text] [SomeException] Text

-- | Recover the prior target only when installation was not proven.  A restore
-- action may perform its namespace effect and then throw, so every outcome is
-- captured and followed by a fresh all-path observation before retry/return.
restorePriorTargetCaptured :: PostCommitIndexDependencies -> FilePath -> FilePath -> FilePath -> PublicationState -> IO PriorTargetRecovery
restorePriorTargetCaptured dependencies databasePath temporaryPath backupPath = go 0 [] [] . Just
  where
    maximumRestoreAttempts = 2 :: Int

    go attempts restoreProblems reconciliationAsync maybeKnownState = do
      observed <-
        case maybeKnownState of
          Just state -> pure (PublicationObserved state)
          Nothing -> observePublicationState dependencies databasePath temporaryPath (Just backupPath)
      case observed of
        PublicationObservationFailed failures ->
          pure
            ( PriorTargetObservationFailed
                restoreProblems
                (reconciliationAsync <> observationAsyncExceptions failures)
                ("publication state observation failed during reconciliation: " <> observationFailuresText failures)
            )
        PublicationObserved state ->
          case (publicationTargetExists state, publicationBackupExists state) of
            (_, True)
              | attempts < maximumRestoreAttempts -> do
                  restored <- tryAny (postCommitRestoreDatabase dependencies backupPath databasePath)
                  case restored of
                    Left exception ->
                      go
                        (attempts + 1)
                        ( if isAsyncException exception
                            then restoreProblems
                            else restoreProblems <> [statefulMessage state exception]
                        )
                        ( if isAsyncException exception
                            then reconciliationAsync <> [exception]
                            else reconciliationAsync
                        )
                        Nothing
                    Right () -> go (attempts + 1) restoreProblems reconciliationAsync Nothing
              | otherwise ->
                  pure
                    ( PriorTargetUnavailable
                        restoreProblems
                        reconciliationAsync
                        "restore attempt limit reached while backup remains"
                        state
                    )
            (True, False) -> pure (PriorTargetRestored restoreProblems reconciliationAsync)
            (False, False) ->
              pure
                ( PriorTargetUnavailable
                    restoreProblems
                    reconciliationAsync
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
cleanupOwnedTemporary dependencies temporaryPath = do
  cleanup <- cleanupOwnedTemporaryCaptured dependencies temporaryPath
  case cleanupAsyncException cleanup of
    Just exception -> throwIO exception
    Nothing -> pure (cleanupTypedProblems cleanup)

-- | All candidate paths are swept while masked, even if a remove is
-- interruptible or a test seam throws.  The caller decides exception priority;
-- this keeps the original operation cancellation distinguishable from a later
-- cleanup cancellation.
data OwnedTemporaryCleanup = OwnedTemporaryCleanup
  { cleanupTypedProblems :: [PostCommitIndexError],
    cleanupAsyncException :: Maybe SomeException
  }

cleanupOwnedTemporaryCaptured :: PostCommitIndexDependencies -> FilePath -> IO OwnedTemporaryCleanup
cleanupOwnedTemporaryCaptured dependencies temporaryPath =
  cleanupOwnedPathsCaptured dependencies (ownedTemporaryPaths temporaryPath)

-- | Publication cleanup owns the candidate and, after reservation, the backup.
-- Every action is attempted in deterministic candidate/journal/shm/wal/backup
-- order.  Capturing rather than throwing lets the publication resolver enforce
-- one exception-priority decision after the complete sweep.
cleanupPublicationArtifactsCaptured :: PostCommitIndexDependencies -> FilePath -> Maybe FilePath -> IO OwnedTemporaryCleanup
cleanupPublicationArtifactsCaptured dependencies temporaryPath maybeBackupPath =
  cleanupOwnedPathsCaptured
    dependencies
    ( ownedTemporaryPaths temporaryPath
        <> case maybeBackupPath of
          Nothing -> []
          Just backupPath -> [backupPath]
    )

cleanupPublicationSidecarsCaptured :: PostCommitIndexDependencies -> FilePath -> IO OwnedTemporaryCleanup
cleanupPublicationSidecarsCaptured dependencies temporaryPath =
  cleanupOwnedPathsCaptured dependencies (drop 1 (ownedTemporaryPaths temporaryPath))

cleanupOwnedPathsCaptured :: PostCommitIndexDependencies -> [FilePath] -> IO OwnedTemporaryCleanup
cleanupOwnedPathsCaptured dependencies paths = do
  outcomes <- mapM removeOwned paths
  pure
    OwnedTemporaryCleanup
      { cleanupTypedProblems = concatMap synchronousProblem outcomes,
        cleanupAsyncException = firstAsync outcomes
      }
  where
    removeOwned path = do
      removed <- tryAny (postCommitRemoveOwnedFile dependencies path)
      pure (path, removed)

    synchronousProblem (path, Left exception)
      | isAsyncException exception = []
      | otherwise = [PostCommitIndexCleanupFailure path (Text.pack (displayException exception))]
    synchronousProblem (_, Right ()) = []

    firstAsync = foldr choose Nothing
    choose (_, Left exception) rest
      | isAsyncException exception = Just exception
      | otherwise = rest
    choose (_, Right ()) rest = rest

-- | Resolve publication only after reconciliation and cleanup have both
-- finished.  Exception priority is fixed: initiating async, then the first
-- reconciliation async, then the first cleanup async, then typed/synchronous
-- lifecycle failures in their recorded order.
finishPublication :: Bool -> Maybe SomeException -> [PostCommitIndexError] -> [SomeException] -> OwnedTemporaryCleanup -> IO PublicationOutcome
finishPublication failStop initiatingAsync initiatingProblems reconciliationAsync cleanup =
  case initiatingAsync of
    Just exception -> throwIO exception
    Nothing ->
      case reconciliationAsync of
        exception : _ -> throwIO exception
        [] ->
          case cleanupAsyncException cleanup of
            Just exception -> throwIO exception
            Nothing ->
              let problems = initiatingProblems <> cleanupTypedProblems cleanup
               in pure $
                    if failStop
                      then PublicationFailStop problems
                      else
                        if null problems
                          then PublicationSucceeded
                          else PublicationFailed problems

-- | Cancellation cleanup is best-effort across every owned path.  Each action
-- is caught independently so neither a synchronous cleanup failure nor a new
-- injected asynchronous exception can replace the original cancellation or
-- prevent later siblings from being attempted.
cleanupOwnedTemporaryAfterCancellation :: PostCommitIndexDependencies -> FilePath -> IO ()
cleanupOwnedTemporaryAfterCancellation dependencies temporaryPath = do
  _ <- cleanupOwnedTemporaryCaptured dependencies temporaryPath
  pure ()

ownedTemporaryPaths :: FilePath -> [FilePath]
ownedTemporaryPaths temporaryPath =
  [ temporaryPath
  , temporaryPath <> "-journal"
  , temporaryPath <> "-shm"
  , temporaryPath <> "-wal"
  ]

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
