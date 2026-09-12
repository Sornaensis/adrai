{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Adrai.MutationServiceTest (tests) where

import Adrai.Compiler.Attribution
  ( AttributionDependencies (..),
    closeColdCompileAttribution,
    defaultAttributionDependencies,
    newFileColdCompileAttributionWith,
  )
import Adrai.Compiler (ColdCompilerResult (..), coldCompileRepository)
import Adrai.Domain (Domain, DomainRefinement, mkDomain, mkDomainRefinement)
import Adrai.Format.Document
  ( AppliesToPayload (..),
    ConnectionPayload (..),
    ConnectionRecord (..),
    DomainsPayload (..),
    ManagedRecord (..),
    ParsedManagedDocument (..),
    StatusPayload (..),
    StatusState (StatusActive, StatusObsolete),
    canonicalManagedPath,
    parseManagedDocument,
    renderConnectionSemantic,
    sealManagedDocument,
  )
import Adrai.Git
  ( GitHeadState (..),
    GitOid (..),
    Repository (..),
    RepositoryLayout (MainWorktree),
    RevisionSpec (..),
    discoverRepository,
    gitOidText,
    repositoryHeadState,
    systemGit,
  )
import Adrai.Repository (resolveRepositoryRevision)
import Adrai.GitTestSupport
  ( commitFile,
    commitFiles,
    gitSuccess,
    initTestRepository,
    outputText,
  )
import Adrai.RetainedNative.RepositorySeed
  ( RepositorySeed,
    createRepositorySeedWith,
    removeRepositorySeed,
    withRepositorySeedCopy,
  )
import Adrai.Fixture.CompilerRepository (healthyCompilerFiles)
import Adrai.Graph
  ( AxisResolution (..),
    ReducedAdr (..),
    lookupReducedAdr,
    reduceManagedGraph,
    reducedStateToken,
  )
import Adrai.Provenance
  ( ProvenanceCapsuleInput (..),
    ProvenanceObjectId (..),
    eventKindText,
    mkEventKind,
    mkProvenanceCapsule,
    provenanceActor,
    provenanceBasis,
    provenanceBranchHint,
    provenanceEventKind,
    provenanceObjectId,
    provenanceOperationId,
    provenanceSemanticDigest,
    provenanceParents,
    provenanceInputs,
    provenanceTimestampMs,
    provenanceToolVersion,
    semanticDigest,
  )
import Adrai.Scope (ScopePattern, mkScopePattern)
import Adrai.Service.Mutation
  ( CreateResult (..),
    AmendResult (..),
    ScopeChangeRequest (..),
    ScopeChangeResult (..),
    DomainChangeRequest (..),
    DomainChangeResult (..),
    ObsoleteRequest (..),
    ObsoleteResult (..),
    InitResult (..),
    amendAdmCommand,
    amendCurrentAdrCommand,
    changeDomainCommand,
    changeScopeCommand,
    createAdrCommand,
    initCommand,
    obsoleteCommand,
  )
import Adrai.Service.PostCommitIndex
  ( IndexWarning (..),
    PostCommitIndexError (..),
    PostCommitIndexDependencies (..),
    PostCommitIndexPublishError (..),
    PostCommitIndexResult (..),
    compilePostCommitIndex,
    compilePostCommitIndexWith,
    postCommitIndexDependencies,
  )
import Adrai.Service.Transaction (TransactionError (..))
import Adrai.Types
  ( ActorKind (HumanActor),
    Actor,
    AdrId,
    ConnectionId,
    RecordId,
    StateToken,
    ProvenanceInputs (..),
    RepoPath,
    configManagedPaths,
    defaultConfig,
    mkActor,
    mkAdrId,
    mkConnectionId,
    mkDigest,
    mkOperationId,
    mkRepoPath,
    mkRecordId,
    adrIdText,
     connectionIdText,
     gitRefText,
     mkStateToken,
    operationIdText,
    repoPathText,
    stateTokenText,
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import Data.Either (isLeft)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, nub, sort)
import Control.Exception (AsyncException (ThreadKilled, UserInterrupt), SomeException, bracket, finally, onException, throw, throwIO, try)
import Database.SQLite.Simple (Only (..), close, execute_, open, query_)
import System.Directory (copyFile, doesDirectoryExist, doesFileExist, listDirectory, removeFile, renameFile)
import System.FilePath
  ( (</>),
    equalFilePath,
    isRelative,
    makeRelative,
    normalise,
    splitDirectories,
    takeDirectory,
    takeFileName,
  )
import System.IO (Handle, hClose)
import System.IO.Temp (withSystemTempDirectory)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  withResource createMutationRepositorySeed removeMutationRepositorySeed $ \getMutationSeed ->
    testGroup "mutation service results"
      [ withResource createPostCommitRepositorySeed removePostCommitRepositorySeed $ \getPostCommitSeed ->
          testGroup
        "P6-02A mutation service results"
        [ testCase "post-commit indexing replaces a same-path projection with the exact latest commit" (postCommitIndexProjectsExactCommit getPostCommitSeed)
    , testCase "post-commit indexing projects actual compiler warnings in stable order" postCommitIndexProjectsCompilerWarnings
    , testCase "post-commit index failure preserves the committed HEAD" (postCommitIndexFailurePreservesCommit getPostCommitSeed)
    , testCase "post-commit index exposes representative open compile and close failures" (postCommitIndexFailureBoundaries getPostCommitSeed)
    , testCase "post-commit index forces deferred compiler outcomes before database close" (postCommitIndexForcesDeferredCompilerOutcomes getPostCommitSeed)
    , testCase "post-commit async compile cancellation finalizes and cleans candidate" (postCommitIndexAsyncCompileCancellationFinalizesCandidate getPostCommitSeed)
    , testCase "post-commit index failure retains prior target and cleans disposable candidate" (postCommitIndexFailureRetainsPriorIndexAndCleansCandidate getPostCommitSeed)
    , testCase "post-commit publication failure retains exact target and cleans candidate" (postCommitIndexPublishFailureIsAtomic getPostCommitSeed)
    , testCase "post-commit partial replacement cancellation restores exact prior target" (postCommitIndexPartialReplacementCancellationRestoresPrior getPostCommitSeed)
    , testCase "post-commit cancellation after backup reservation leaves prior target authoritative" (postCommitIndexPostReservationCancellationCleansAll getPostCommitSeed)
    , testCase "post-commit completed install cancellation retains the proven new target" (postCommitIndexCompletedInstallCancellationRetainsNewTarget getPostCommitSeed)
    , testCase "post-commit close after-effect cancellation discharges the connection once" (postCommitIndexCloseThenCancellationIsExactlyOnce getPostCommitSeed)
    , testCase "post-commit attributed publication prelude cancellation cleans before action" (postCommitIndexAttributedPublicationPreludeCancellation getPostCommitSeed)
    , testCase "post-commit close attribution prelude cancellation closes owned connection" (postCommitIndexCloseAttributionPreludeCancellation getPostCommitSeed)
    , testCase "post-commit close action cancellation outranks attribution finalizer cancellation" (postCommitIndexCloseActionCancellationOutranksAttributionFinalizer getPostCommitSeed)
    , testCase "post-commit publication action cancellation outranks attribution finalizer cancellation" (postCommitIndexPublicationActionCancellationOutranksAttributionFinalizer getPostCommitSeed)
    , testCase "post-commit publication resolver preserves priority and attempts every cleanup" (postCommitIndexPublicationPriorityMatrix getPostCommitSeed)
    , testCase "post-commit reconciliation probe failure fails stop with recovery artifacts" (postCommitIndexPublishProbeFailureFailStop getPostCommitSeed)
    , testCase "post-commit impossible publication state fails stop and retains candidate" (postCommitIndexPublishInvariantFailStop getPostCommitSeed)
    , testCase "post-commit temp close failure retries close before ordered cleanup" (postCommitIndexTemporaryCloseFailureCleansCandidate getPostCommitSeed)
    , testCase "post-commit cleanup failures are typed without skipping later siblings" (postCommitIndexCleanupFailureIsOrdered getPostCommitSeed)
    , testCase "post-commit database close failure keeps its live handle and cleanup failure observable" (postCommitIndexLiveCloseFailureIsObservable getPostCommitSeed)
    , testCase "amend rejection routes preserve complete caller state across attached detached and conflicted histories" (amendRejectionsPreserveRepository getMutationSeed)
        ]
    , testGroup
        "P6-02C scope mutation service"
    [ testCase "scope mixed update reports truthful delta and provenance" (scopeUpdatesAreTruthful getMutationSeed)
        , testCase "scope reviewed merge resolves two heads even when the reviewed set equals their union" (scopeReviewedSetMergesUnchangedUnion getMutationSeed)
        , testCase "representative scope request and stale-token rejections preserve complete caller state" (scopeRejectionsPreserveRepository getMutationSeed)
        , testCase "scope reviewed set rejects a conflict outside the scope axis without state changes" (rejectNonScopeConflict getMutationSeed)
        ]
    , testGroup
        "P6-02D domain mutation service"
        [ testCase "domain mixed update reports truthful state" (domainMixedUpdateIsTruthful getMutationSeed)
        , testCase "representative domain delta set refine and stale-token rejections preserve state" (domainRejectionsPreserveRepository getMutationSeed)
        ]
    , testGroup
        "P6-02E status mutation service"
        [ testCase "representative status request and stale-token rejections preserve complete caller state" (statusRejectionsPreserveRepository getMutationSeed)
        , testCase "status resolve merges every current status head and rejects unapproved conflicts" (statusResolveMergesConflict getMutationSeed)
        ]
    ]

-- | Every observable state that an append-only mutation must leave untouched
-- when it rejects or rolls back.  Keeping the raw index and the complete
-- managed worktree inventory alongside the semantic Git observations catches
-- both accidental caller-index writes and leaked generated paths.
data FailureSnapshot = FailureSnapshot
  { failureHeadRef :: Text.Text,
    failureHeadOid :: Text.Text,
    failureHeadTree :: Text.Text,
    failureTreeEntries :: Text.Text,
    failureRawIndex :: BS.ByteString,
    failureIndexEntries :: BS.ByteString,
    failureStatus :: [Text.Text],
    failureManagedPaths :: [(Text.Text, Maybe BS.ByteString, Maybe BS.ByteString)],
    failureManagedInventory :: [(Text.Text, Maybe BS.ByteString)],
    failureCallerFiles :: [(FilePath, Maybe BS.ByteString)],
    failureDisposableIndexes :: [(FilePath, BS.ByteString)]
  }
  deriving (Eq, Show)

postCommitIndexProjectsExactCommit :: IO PostCommitRepositorySeed -> IO ()
postCommitIndexProjectsExactCommit getSeed =
  withPostCommitRepositoryCopy getSeed "adrai post-commit index" $ \temporary _ repository commitOid _base -> do
    let database = temporary </> "index.sqlite"
    assertIndexed repository commitOid database
    created <- assertRight =<< runCreate repository
    assertIndexed repository (createCommitOid created) database

postCommitIndexFailurePreservesCommit :: IO PostCommitRepositorySeed -> IO ()
postCommitIndexFailurePreservesCommit getSeed =
  withPostCommitRepositoryCopy getSeed "adrai post-commit index failure" $ \temporary directory repository commitOid _base -> do
    let database = temporary </> "failed.sqlite"
    let missing = GitOid "0000000000000000000000000000000000000000"
    result <- compilePostCommitIndex repository missing database
    postCommitIndexed result @?= False
    postCommitDatabase result @?= Nothing
    postCommitIndexRevision result @?= Nothing
    postCommitIndexWarnings result @?= []
    case postCommitIndexError result of
      Just (PostCommitIndexResolveFailure _) -> pure ()
      other -> assertFailure ("expected actual resolve failure, got " <> show other)
    gitText directory ["rev-parse", "HEAD"] >>= (@?= gitOidText commitOid)

postCommitIndexProjectsCompilerWarnings :: IO ()
postCommitIndexProjectsCompilerWarnings =
  withSystemTempDirectory "adrai post-commit warning projection" $ \temporary -> do
    let directory = temporary </> "repository"
        database = temporary </> "warnings.sqlite"
        missingBasis = GitOid "ffffffffffffffffffffffffffffffffffffffff"
    initTestRepository directory
    _ <- commitFile directory "seed.txt" "seed\n"
    files <- assertRight (healthyCompilerFiles missingBasis)
    commitText <- commitFiles directory files
    repository <- assertRight =<< discoverRepository systemGit directory
    let commitOid = GitOid commitText
        expectedWarnings = [IndexWarning "BASIS_COMMIT_UNAVAILABLE" "provenance basis is missing or is not a commit object"]
    result <- compilePostCommitIndex repository commitOid database
    postCommitIndexed result @?= True
    postCommitIndexRevision result @?= Just commitOid
    postCommitIndexWarnings result @?= expectedWarnings
    bracket (open database) close $ \connection -> do
      resolved <- query_ connection "SELECT value FROM meta WHERE key = 'resolved_oid'" :: IO [Only Text.Text]
      resolved @?= [Only (gitOidText commitOid)]

postCommitIndexFailureBoundaries :: IO PostCommitRepositorySeed -> IO ()
postCommitIndexFailureBoundaries getSeed =
  withPostCommitRepositoryCopy getSeed "adrai post-commit injected failures" $ \temporary _ repository commitOid base -> do
    let database = temporary </> "failure.sqlite"
        openFailure = base {postCommitOpenDatabase = \_ -> throwIO (userError "open failure")}
        compileFailure = base {postCommitColdCompile = \_ _ -> throwIO (userError "compile failure")}
        closeFailure = base {postCommitCloseDatabase = \connection -> close connection >> throwIO (userError "close failure")}
        combinedFailure =
          base
            { postCommitColdCompile = \_ _ -> throwIO (userError "compile failure"),
              postCommitCloseDatabase = \connection -> close connection >> throwIO (userError "close failure")
            }
    assertFailureKind (compilePostCommitIndexWith openFailure repository commitOid database) isOpenFailure
    assertFailureKind (compilePostCommitIndexWith compileFailure repository commitOid database) isCompileFailure
    assertFailureKind (compilePostCommitIndexWith closeFailure repository commitOid database) isCloseFailure
    assertFailureKind (compilePostCommitIndexWith combinedFailure repository commitOid database) isOrderedCombinedFailure
  where
    assertFailureKind action predicate = do
      result <- action
      postCommitIndexed result @?= False
      postCommitDatabase result @?= Nothing
      postCommitIndexRevision result @?= Nothing
      postCommitIndexWarnings result @?= []
      assertBool ("unexpected index failure: " <> show (postCommitIndexError result)) (maybe False predicate (postCommitIndexError result))
    isOpenFailure = \case
      PostCommitIndexOpenFailure _ -> True
      _ -> False
    isCompileFailure = \case
      PostCommitIndexCompileException _ -> True
      _ -> False
    isCloseFailure = \case
      PostCommitIndexCloseFailure _ -> True
      _ -> False
    isOrderedCombinedFailure = \case
      PostCommitIndexMultipleFailures [PostCommitIndexCompileException _, PostCommitIndexCloseFailure _] -> True
      _ -> False

postCommitIndexForcesDeferredCompilerOutcomes :: IO PostCommitRepositorySeed -> IO ()
postCommitIndexForcesDeferredCompilerOutcomes getSeed =
  withPostCommitRepositoryCopy getSeed "adrai deferred post-commit compiler outcomes" $ \temporary _ repository commitOid base -> do
    let priorBytes = "prior index survives deferred compiler outcome"
        assertSynchronousFailure name compiler = do
          let database = temporary </> name <> ".sqlite"
          BS.writeFile database priorBytes
          result <- compilePostCommitIndexWith (base {postCommitColdCompile = compiler}) repository commitOid database
          postCommitIndexed result @?= False
          postCommitDatabase result @?= Nothing
          case postCommitIndexError result of
            Just (PostCommitIndexCompileException _) -> pure ()
            other -> assertFailure ("expected deferred compiler exception, got " <> show other)
          BS.readFile database >>= (@?= priorBytes)
          assertNoCandidate database
    assertSynchronousFailure "deferred-either" (\_ _ -> pure (throw (userError "deferred compiler outcome")))
    assertSynchronousFailure "deferred-left" (\_ _ -> pure (Left (throw (userError "deferred compiler failure"))))
    assertSynchronousFailure "deferred-result" (\_ _ -> pure (Right (throw (userError "deferred compiler result"))))
    let assertAsyncCancellation name compiler = do
          let database = temporary </> name <> ".sqlite"
          BS.writeFile database priorBytes
          outcome <- try (compilePostCommitIndexWith (base {postCommitColdCompile = compiler}) repository commitOid database) :: IO (Either AsyncException PostCommitIndexResult)
          outcome @?= Left ThreadKilled
          BS.readFile database >>= (@?= priorBytes)
          assertNoCandidate database
    assertAsyncCancellation "deferred-right-async" (\_ _ -> pure (Right (throw ThreadKilled)))
    assertAsyncCancellation "deferred-left-async" (\_ _ -> pure (Left (throw ThreadKilled)))
  where
    assertNoCandidate database = do
      siblings <- listDirectory (takeDirectory database)
      assertBool
        "deferred compiler outcome leaves no candidate"
        (not (any (isPrefixOf (takeFileName database <> ".post-commit-")) siblings))

postCommitIndexAsyncCompileCancellationFinalizesCandidate :: IO PostCommitRepositorySeed -> IO ()
postCommitIndexAsyncCompileCancellationFinalizesCandidate getSeed =
  withPostCommitRepositoryCopy getSeed "adrai post-commit async compile cancellation" $ \temporary _ repository commitOid base -> do
    let database = temporary </> "index.sqlite"
        priorBytes = "prior index survives cancellation exactly"
    BS.writeFile database priorBytes
    candidateRef <- newIORef Nothing
    connectionRef <- newIORef Nothing
    closeCalls <- newIORef (0 :: Int)
    cleanupCalls <- newIORef []
    let openTracked path = do
          connection <- postCommitOpenDatabase base path
          writeIORef candidateRef (Just path)
          writeIORef connectionRef (Just connection)
          pure connection
        cancelAfterSidecars _ _ = do
          candidate <- readIORef candidateRef >>= maybe (throwIO (userError "candidate path was not captured")) pure
          mapM_ (\suffix -> BS.writeFile (candidate <> suffix) "owned") ["-journal", "-shm", "-wal"]
          throwIO ThreadKilled
        closeTracked connection = do
          atomicModifyIORef' closeCalls $ \count -> (count + 1, ())
          postCommitCloseDatabase base connection
        removeTracked path = do
          atomicModifyIORef' cleanupCalls $ \paths -> (paths <> [path], ())
          postCommitRemoveOwnedFile base path
        asyncCompile =
          base
            { postCommitOpenDatabase = openTracked,
              postCommitColdCompile = cancelAfterSidecars,
              postCommitCloseDatabase = closeTracked,
              postCommitRemoveOwnedFile = removeTracked
            }
    outcome <- try (compilePostCommitIndexWith asyncCompile repository commitOid database) :: IO (Either AsyncException PostCommitIndexResult)
    case outcome of
      Left ThreadKilled -> pure ()
      other -> assertFailure ("expected ThreadKilled cancellation, got " <> show other)
    candidate <- readIORef candidateRef >>= maybe (assertFailure "missing captured candidate") pure
    connection <- readIORef connectionRef >>= maybe (assertFailure "missing captured connection") pure
    readIORef closeCalls >>= (@?= 1)
    readIORef cleanupCalls >>= (@?= [candidate, candidate <> "-journal", candidate <> "-shm", candidate <> "-wal"])
    closedQuery <- try (query_ connection "SELECT 1" :: IO [Only Int]) :: IO (Either SomeException [Only Int])
    assertBool "cancellation propagated before the database connection was finalized" (isLeft closedQuery)
    BS.readFile database >>= (@?= priorBytes)
    assertOwnedPathsExist candidate [False, False, False, False]

postCommitIndexFailureRetainsPriorIndexAndCleansCandidate :: IO PostCommitRepositorySeed -> IO ()
postCommitIndexFailureRetainsPriorIndexAndCleansCandidate getSeed =
  withPostCommitRepositoryCopy getSeed "adrai post-commit index replacement failure" $ \temporary _ repository commitOid base -> do
    let database = temporary </> "index.sqlite"
        candidatePrefix = takeFileName database <> ".post-commit-"
    assertIndexed repository commitOid database
    created <- assertRight =<< runCreate repository
    let compileFailure =
          base
            { postCommitColdCompile = \_ _ -> throwIO (userError "compile failure")
            }
    result <- compilePostCommitIndexWith compileFailure repository (createCommitOid created) database
    postCommitIndexed result @?= False
    postCommitDatabase result @?= Nothing
    postCommitIndexRevision result @?= Nothing
    postCommitIndexWarnings result @?= []
    case postCommitIndexError result of
      Just (PostCommitIndexCompileException _) -> pure ()
      other -> assertFailure ("expected compile failure, got " <> show other)
    bracket (open database) close $ \connection -> do
      resolved <- query_ connection "SELECT value FROM meta WHERE key = 'resolved_oid'" :: IO [Only Text.Text]
      resolved @?= [Only (gitOidText commitOid)]
    siblings <- listDirectory temporary
    assertBool "failed refresh left an owned SQLite candidate behind" (not (any (candidatePrefix `isPrefixOf`) siblings))

postCommitIndexPublishFailureIsAtomic :: IO PostCommitRepositorySeed -> IO ()
postCommitIndexPublishFailureIsAtomic getSeed =
  withPostCommitRepositoryCopy getSeed "adrai post-commit publish failure" $ \temporary _ repository commitOid base -> do
    let database = temporary </> "index.sqlite"
        priorBytes = "prior index bytes\NULremain exact"
    BS.writeFile database priorBytes
    candidateRef <- newIORef Nothing
    backupRef <- newIORef Nothing
    restoreCalls <- newIORef (0 :: Int)
    let openTracked directoryPath template = do
          temporaryPath <- postCommitOpenTemporary base directoryPath template
          if ".post-commit-backup-" `isInfixOf` template
            then writeIORef backupRef (Just (fst temporaryPath))
            else writeIORef candidateRef (Just (fst temporaryPath))
          pure temporaryPath
        partiallyInstall target _ (Just backup) = do
          renameFile target backup
          pure (Left (PostCommitIndexInstallFailure "injected install failure"))
        partiallyInstall _ _ Nothing = pure (Left (PostCommitIndexInstallFailure "missing injected backup"))
        restoreAfterTransientFailure backup target = do
          call <- atomicModifyIORef' restoreCalls $ \count -> let next = count + 1 in (next, next)
          if call == 1
            then throwIO (userError "injected restore failure")
            else renameFile backup target
        publishFailure =
          base
            { postCommitOpenTemporary = openTracked,
              postCommitInstallDatabase = partiallyInstall,
              postCommitRestoreDatabase = restoreAfterTransientFailure
            }
    result <- compilePostCommitIndexWith publishFailure repository commitOid database
    case postCommitIndexError result of
      Just (PostCommitIndexPublishFailure (PostCommitIndexInstallAndRestoreFailure "injected install failure" restoreProblem)) ->
        assertBool "restore failure detail was discarded" ("injected restore failure" `Text.isInfixOf` restoreProblem)
      other -> assertFailure ("expected structured install and restore failure, got " <> show other)
    readIORef restoreCalls >>= (@?= 2)
    BS.readFile database >>= (@?= priorBytes)
    candidate <- readIORef candidateRef >>= maybe (assertFailure "missing captured candidate") pure
    backup <- readIORef backupRef >>= maybe (assertFailure "missing captured backup") pure
    assertOwnedPathsExist candidate [False, False, False, False]
    doesFileExist backup >>= (@?= False)

postCommitIndexPartialReplacementCancellationRestoresPrior :: IO PostCommitRepositorySeed -> IO ()
postCommitIndexPartialReplacementCancellationRestoresPrior getSeed =
  withPostCommitRepositoryCopy getSeed "adrai post-commit partial replacement cancellation" $ \temporary directory repository commitOid base -> do
    let database = temporary </> "index.sqlite"
    priorBytes <- installPreparedPostCommitDatabase directory database
    candidateRef <- newIORef Nothing
    backupRef <- newIORef Nothing
    cleanupCalls <- newIORef []
    let openTracked = capturePublicationTemporary base candidateRef backupRef
        cancelAfterMovingPrior target candidate (Just backup) = do
          writeOwnedSidecars candidate
          renameFile target backup
          throwIO ThreadKilled
        cancelAfterMovingPrior _ _ Nothing = throwIO (userError "missing replacement backup")
        removeTracked path = do
          atomicModifyIORef' cleanupCalls $ \paths -> (paths <> [path], ())
          postCommitRemoveOwnedFile base path
        dependencies =
          base
            { postCommitOpenTemporary = openTracked,
              postCommitInstallDatabase = cancelAfterMovingPrior,
              postCommitRemoveOwnedFile = removeTracked
            }
    outcome <- try (compilePostCommitIndexWith dependencies repository commitOid database) :: IO (Either AsyncException PostCommitIndexResult)
    outcome @?= Left ThreadKilled
    candidate <- requireCaptured "candidate" candidateRef
    backup <- requireCaptured "backup" backupRef
    readIORef cleanupCalls >>= (@?= publicationCleanupOrder candidate backup)
    BS.readFile database >>= (@?= priorBytes)
    assertNoPublicationArtifacts candidate backup
    assertDatabaseReusable database commitOid

postCommitIndexPostReservationCancellationCleansAll :: IO PostCommitRepositorySeed -> IO ()
postCommitIndexPostReservationCancellationCleansAll getSeed =
  withPostCommitRepositoryCopy getSeed "adrai post-commit post-reservation cancellation" $ \temporary directory repository commitOid base -> do
    let database = temporary </> "index.sqlite"
    priorBytes <- installPreparedPostCommitDatabase directory database
    candidateRef <- newIORef Nothing
    backupRef <- newIORef Nothing
    cleanupCalls <- newIORef []
    let openTracked = capturePublicationTemporary base candidateRef backupRef
        cancelBeforeInstall _ candidate (Just _) = writeOwnedSidecars candidate >> throwIO ThreadKilled
        cancelBeforeInstall _ _ Nothing = throwIO (userError "missing replacement backup")
        removeTracked path = do
          atomicModifyIORef' cleanupCalls $ \paths -> (paths <> [path], ())
          postCommitRemoveOwnedFile base path
        dependencies =
          base
            { postCommitOpenTemporary = openTracked,
              postCommitInstallDatabase = cancelBeforeInstall,
              postCommitRemoveOwnedFile = removeTracked
            }
    outcome <- try (compilePostCommitIndexWith dependencies repository commitOid database) :: IO (Either AsyncException PostCommitIndexResult)
    outcome @?= Left ThreadKilled
    candidate <- requireCaptured "candidate" candidateRef
    backup <- requireCaptured "backup" backupRef
    readIORef cleanupCalls >>= (@?= publicationCleanupOrder candidate backup)
    BS.readFile database >>= (@?= priorBytes)
    assertNoPublicationArtifacts candidate backup
    assertDatabaseReusable database commitOid

postCommitIndexCompletedInstallCancellationRetainsNewTarget :: IO PostCommitRepositorySeed -> IO ()
postCommitIndexCompletedInstallCancellationRetainsNewTarget getSeed =
  withPostCommitRepositoryCopy getSeed "adrai post-commit completed install cancellation" $ \temporary _ repository commitOid base -> do
    let database = temporary </> "index.sqlite"
        priorBytes = "stale arbitrary prior bytes must not be restored"
    BS.writeFile database priorBytes
    candidateRef <- newIORef Nothing
    backupRef <- newIORef Nothing
    cleanupCalls <- newIORef []
    let openTracked = capturePublicationTemporary base candidateRef backupRef
        cancelAfterInstall target candidate (Just backup) = do
          writeOwnedSidecars candidate
          renameFile target backup
          renameFile candidate target
          throwIO ThreadKilled
        cancelAfterInstall _ _ Nothing = throwIO (userError "missing replacement backup")
        removeTracked path = do
          atomicModifyIORef' cleanupCalls $ \paths -> (paths <> [path], ())
          postCommitRemoveOwnedFile base path
        dependencies =
          base
            { postCommitOpenTemporary = openTracked,
              postCommitInstallDatabase = cancelAfterInstall,
              postCommitRemoveOwnedFile = removeTracked
            }
    outcome <- try (compilePostCommitIndexWith dependencies repository commitOid database) :: IO (Either AsyncException PostCommitIndexResult)
    outcome @?= Left ThreadKilled
    candidate <- requireCaptured "candidate" candidateRef
    backup <- requireCaptured "backup" backupRef
    readIORef cleanupCalls >>= (@?= publicationCleanupOrder candidate backup)
    assertNoPublicationArtifacts candidate backup
    assertDatabaseRevision database commitOid
    BS.readFile database >>= assertBool "completed candidate was replaced by stale prior bytes" . (/= priorBytes)
    assertDatabaseReusable database commitOid

postCommitIndexCloseThenCancellationIsExactlyOnce :: IO PostCommitRepositorySeed -> IO ()
postCommitIndexCloseThenCancellationIsExactlyOnce getSeed =
  withPostCommitRepositoryCopy getSeed "adrai post-commit close then cancellation" $ \temporary directory repository commitOid base -> do
    let database = temporary </> "index.sqlite"
    priorBytes <- installPreparedPostCommitDatabase directory database
    candidateRef <- newIORef Nothing
    connectionRef <- newIORef Nothing
    closeCalls <- newIORef (0 :: Int)
    cleanupCalls <- newIORef []
    let openTracked path = do
          connection <- postCommitOpenDatabase base path
          writeIORef candidateRef (Just path)
          writeIORef connectionRef (Just connection)
          pure connection
        closeThenCancel connection = do
          atomicModifyIORef' closeCalls $ \count -> (count + 1, ())
          postCommitCloseDatabase base connection
          candidate <- requireCaptured "candidate" candidateRef
          writeOwnedSidecars candidate
          throwIO ThreadKilled
        removeTracked path = do
          atomicModifyIORef' cleanupCalls $ \paths -> (paths <> [path], ())
          postCommitRemoveOwnedFile base path
        dependencies =
          base
            { postCommitOpenDatabase = openTracked,
              postCommitCloseDatabase = closeThenCancel,
              postCommitRemoveOwnedFile = removeTracked
            }
    outcome <- try (compilePostCommitIndexWith dependencies repository commitOid database) :: IO (Either AsyncException PostCommitIndexResult)
    outcome @?= Left ThreadKilled
    candidate <- requireCaptured "candidate" candidateRef
    connection <- requireCaptured "connection" connectionRef
    readIORef closeCalls >>= (@?= 1)
    readIORef cleanupCalls >>= (@?= ownedTemporaryPathsForTest candidate)
    closedQuery <- try (query_ connection "SELECT 1" :: IO [Only Int]) :: IO (Either SomeException [Only Int])
    assertBool "close action completed but captured connection remained usable" (isLeft closedQuery)
    BS.readFile database >>= (@?= priorBytes)
    assertOwnedPathsExist candidate [False, False, False, False]
    assertDatabaseReusable database commitOid

postCommitIndexAttributedPublicationPreludeCancellation :: IO PostCommitRepositorySeed -> IO ()
postCommitIndexAttributedPublicationPreludeCancellation getSeed =
  withPostCommitRepositoryCopy getSeed "adrai post-commit attributed publication prelude" $ \temporary directory repository commitOid base -> do
    let database = temporary </> "index.sqlite"
        attributionPath = temporary </> "attribution.tsv"
        baseAttribution = defaultAttributionDependencies
    priorBytes <- installPreparedPostCommitDatabase directory database
    candidateRef <- newIORef Nothing
    injectionCalls <- newIORef (0 :: Int)
    let writeLine handle line
          | "start\t" `isPrefixOf` line && "\timmutablepublication\t" `isInfixOf` line = do
              atomicModifyIORef' injectionCalls $ \count -> (count + 1, ())
              candidate <- requireSinglePublicationCandidate temporary
              writeIORef candidateRef (Just candidate)
              writeOwnedSidecars candidate
              throwIO ThreadKilled
          | otherwise = attributionWriteLine baseAttribution handle line
        attributionDependencies = baseAttribution {attributionWriteLine = writeLine}
    outcome <-
      bracket
        (newFileColdCompileAttributionWith attributionDependencies attributionPath)
        closeColdCompileAttribution
        (\attribution -> try (compilePostCommitIndexWith (base {postCommitAttribution = attribution}) repository commitOid database))
        :: IO (Either AsyncException PostCommitIndexResult)
    outcome @?= Left ThreadKilled
    readIORef injectionCalls >>= (@?= 1)
    candidate <- requireCaptured "candidate" candidateRef
    BS.readFile database >>= (@?= priorBytes)
    assertOwnedPathsExist candidate [False, False, False, False]
    ownedCandidateSiblings temporary database >>= (@?= [])
    assertDatabaseReusable database commitOid

postCommitIndexCloseAttributionPreludeCancellation :: IO PostCommitRepositorySeed -> IO ()
postCommitIndexCloseAttributionPreludeCancellation getSeed =
  withPostCommitRepositoryCopy getSeed "adrai post-commit close attribution prelude" $ \temporary directory repository commitOid base -> do
    let database = temporary </> "index.sqlite"
        attributionPath = temporary </> "attribution.tsv"
        baseAttribution = defaultAttributionDependencies
    priorBytes <- installPreparedPostCommitDatabase directory database
    candidateRef <- newIORef Nothing
    injectionCalls <- newIORef (0 :: Int)
    let writeLine handle line
          | "start\t" `isPrefixOf` line && "\tdatabaseclose\t" `isInfixOf` line = do
              atomicModifyIORef' injectionCalls $ \count -> (count + 1, ())
              candidate <- requireSinglePublicationCandidate temporary
              writeIORef candidateRef (Just candidate)
              writeOwnedSidecars candidate
              throwIO ThreadKilled
          | otherwise = attributionWriteLine baseAttribution handle line
        attributionDependencies = baseAttribution {attributionWriteLine = writeLine}
    outcome <-
      bracket
        (newFileColdCompileAttributionWith attributionDependencies attributionPath)
        closeColdCompileAttribution
        (\attribution -> try (compilePostCommitIndexWith (base {postCommitAttribution = attribution}) repository commitOid database))
        :: IO (Either AsyncException PostCommitIndexResult)
    outcome @?= Left ThreadKilled
    readIORef injectionCalls >>= (@?= 1)
    candidate <- requireCaptured "candidate" candidateRef
    BS.readFile database >>= (@?= priorBytes)
    -- Successful removal of the SQLite main file and every sidecar is the
    -- production-path evidence that the still-Owned connection was closed by
    -- cancellation cleanup before pathname cleanup ran.
    assertOwnedPathsExist candidate [False, False, False, False]
    ownedCandidateSiblings temporary database >>= (@?= [])
    assertDatabaseReusable database commitOid

postCommitIndexCloseActionCancellationOutranksAttributionFinalizer :: IO PostCommitRepositorySeed -> IO ()
postCommitIndexCloseActionCancellationOutranksAttributionFinalizer getSeed =
  withPostCommitRepositoryCopy getSeed "adrai post-commit close action finalizer collision" $ \temporary directory repository commitOid base -> do
    let database = temporary </> "index.sqlite"
        attributionPath = temporary </> "attribution.tsv"
        baseAttribution = defaultAttributionDependencies
    priorBytes <- installPreparedPostCommitDatabase directory database
    candidateRef <- newIORef Nothing
    connectionRef <- newIORef Nothing
    closeCalls <- newIORef (0 :: Int)
    cleanupCalls <- newIORef []
    finalizerCalls <- newIORef (0 :: Int)
    let writeLine handle line
          | "end\t" `isPrefixOf` line && "\tdatabaseclose\t" `isInfixOf` line = do
              atomicModifyIORef' finalizerCalls $ \count -> (count + 1, ())
              throwIO UserInterrupt
          | otherwise = attributionWriteLine baseAttribution handle line
        attributionDependencies = baseAttribution {attributionWriteLine = writeLine}
        openTracked path = do
          connection <- postCommitOpenDatabase base path
          writeIORef candidateRef (Just path)
          writeIORef connectionRef (Just connection)
          pure connection
        closeThenCancel connection = do
          atomicModifyIORef' closeCalls $ \count -> (count + 1, ())
          postCommitCloseDatabase base connection
          candidate <- requireCaptured "candidate" candidateRef
          writeOwnedSidecars candidate
          throwIO ThreadKilled
        removeTracked path = do
          atomicModifyIORef' cleanupCalls $ \paths -> (paths <> [path], ())
          postCommitRemoveOwnedFile base path
    outcome <-
      bracket
        (newFileColdCompileAttributionWith attributionDependencies attributionPath)
        closeColdCompileAttribution
        ( \attribution ->
            let dependencies =
                  base
                    { postCommitOpenDatabase = openTracked,
                      postCommitCloseDatabase = closeThenCancel,
                      postCommitRemoveOwnedFile = removeTracked,
                      postCommitAttribution = attribution
                    }
             in try (compilePostCommitIndexWith dependencies repository commitOid database)
        )
        :: IO (Either AsyncException PostCommitIndexResult)
    outcome @?= Left ThreadKilled
    readIORef finalizerCalls >>= (@?= 1)
    readIORef closeCalls >>= (@?= 1)
    candidate <- requireCaptured "candidate" candidateRef
    connection <- requireCaptured "connection" connectionRef
    readIORef cleanupCalls >>= (@?= ownedTemporaryPathsForTest candidate)
    closedQuery <- try (query_ connection "SELECT 1" :: IO [Only Int]) :: IO (Either SomeException [Only Int])
    assertBool "close/action collision left the captured connection usable" (isLeft closedQuery)
    BS.readFile database >>= (@?= priorBytes)
    assertOwnedPathsExist candidate [False, False, False, False]
    assertDatabaseReusable database commitOid

postCommitIndexPublicationActionCancellationOutranksAttributionFinalizer :: IO PostCommitRepositorySeed -> IO ()
postCommitIndexPublicationActionCancellationOutranksAttributionFinalizer getSeed =
  withPostCommitRepositoryCopy getSeed "adrai post-commit publication action finalizer collision" $ \temporary directory repository commitOid base -> do
    let database = temporary </> "index.sqlite"
        attributionPath = temporary </> "attribution.tsv"
        baseAttribution = defaultAttributionDependencies
    priorBytes <- installPreparedPostCommitDatabase directory database
    candidateRef <- newIORef Nothing
    backupRef <- newIORef Nothing
    cleanupCalls <- newIORef []
    finalizerCalls <- newIORef (0 :: Int)
    let writeLine handle line
          | "end\t" `isPrefixOf` line && "\timmutablepublication\t" `isInfixOf` line = do
              atomicModifyIORef' finalizerCalls $ \count -> (count + 1, ())
              throwIO UserInterrupt
          | otherwise = attributionWriteLine baseAttribution handle line
        attributionDependencies = baseAttribution {attributionWriteLine = writeLine}
        openTracked = capturePublicationTemporary base candidateRef backupRef
        cancelInstall _ candidate (Just _) = writeOwnedSidecars candidate >> throwIO ThreadKilled
        cancelInstall _ _ Nothing = throwIO (userError "missing replacement backup")
        removeTracked path = do
          atomicModifyIORef' cleanupCalls $ \paths -> (paths <> [path], ())
          postCommitRemoveOwnedFile base path
    outcome <-
      bracket
        (newFileColdCompileAttributionWith attributionDependencies attributionPath)
        closeColdCompileAttribution
        ( \attribution ->
            let dependencies =
                  base
                    { postCommitOpenTemporary = openTracked,
                      postCommitInstallDatabase = cancelInstall,
                      postCommitRemoveOwnedFile = removeTracked,
                      postCommitAttribution = attribution
                    }
             in try (compilePostCommitIndexWith dependencies repository commitOid database)
        )
        :: IO (Either AsyncException PostCommitIndexResult)
    outcome @?= Left ThreadKilled
    readIORef finalizerCalls >>= (@?= 1)
    candidate <- requireCaptured "candidate" candidateRef
    backup <- requireCaptured "backup" backupRef
    readIORef cleanupCalls >>= (@?= publicationCleanupOrder candidate backup)
    BS.readFile database >>= (@?= priorBytes)
    assertNoPublicationArtifacts candidate backup
    assertDatabaseReusable database commitOid

postCommitIndexPublicationPriorityMatrix :: IO PostCommitRepositorySeed -> IO ()
postCommitIndexPublicationPriorityMatrix getSeed =
  withPostCommitRepositoryCopy getSeed "adrai post-commit publication priority" $ \temporary directory repository revision base -> do
    let database = temporary </> "initiating-async.sqlite"
    priorBytes <- installPreparedPostCommitDatabase directory database
    candidateRef <- newIORef Nothing
    backupRef <- newIORef Nothing
    cleanupCalls <- newIORef []
    let openTracked = capturePublicationTemporary base candidateRef backupRef
        cancelPartial target candidate (Just backup) = do
          writeOwnedSidecars candidate
          renameFile target backup
          throwIO ThreadKilled
        cancelPartial _ _ Nothing = throwIO (userError "missing replacement backup")
        removeWithSecondaryFaults path = do
          atomicModifyIORef' cleanupCalls $ \paths -> (paths <> [path], ())
          postCommitRemoveOwnedFile base path
          candidate <- requireCaptured "candidate" candidateRef
          if path == candidate
            then throwIO (userError "candidate cleanup sync failure")
            else
              if path == candidate <> "-journal"
                then throwIO UserInterrupt
                else
                  if path == candidate <> "-shm"
                    then throwIO (userError "shm cleanup sync failure")
                    else pure ()
        dependencies =
          base
            { postCommitOpenTemporary = openTracked,
              postCommitInstallDatabase = cancelPartial,
              postCommitRemoveOwnedFile = removeWithSecondaryFaults
            }
    initiatingAsyncOutcome <- try (compilePostCommitIndexWith dependencies repository revision database) :: IO (Either AsyncException PostCommitIndexResult)
    initiatingAsyncOutcome @?= Left ThreadKilled
    candidate <- requireCaptured "candidate" candidateRef
    backup <- requireCaptured "backup" backupRef
    readIORef cleanupCalls >>= (@?= publicationCleanupOrder candidate backup)
    BS.readFile database >>= (@?= priorBytes)
    assertNoPublicationArtifacts candidate backup
    assertDatabaseReusable database revision

    let cleanupAsyncDatabase = temporary </> "cleanup-async.sqlite"
    cleanupAsyncPrior <- installPreparedPostCommitDatabase directory cleanupAsyncDatabase
    cleanupAsyncCandidateRef <- newIORef Nothing
    cleanupAsyncBackupRef <- newIORef Nothing
    cleanupAsyncCalls <- newIORef []
    let cleanupAsyncOpen = capturePublicationTemporary base cleanupAsyncCandidateRef cleanupAsyncBackupRef
        synchronousInstallFailure _ candidatePath (Just _) = do
          writeOwnedSidecars candidatePath
          pure (Left (PostCommitIndexInstallFailure "synchronous install failure"))
        synchronousInstallFailure _ _ Nothing = pure (Left (PostCommitIndexInstallFailure "missing replacement backup"))
        removeThenCancel path = do
          atomicModifyIORef' cleanupAsyncCalls $ \paths -> (paths <> [path], ())
          postCommitRemoveOwnedFile base path
          candidatePath <- requireCaptured "candidate" cleanupAsyncCandidateRef
          if path == candidatePath <> "-journal" then throwIO UserInterrupt else pure ()
        cleanupAsyncDependencies =
          base
            { postCommitOpenTemporary = cleanupAsyncOpen,
              postCommitInstallDatabase = synchronousInstallFailure,
              postCommitRemoveOwnedFile = removeThenCancel
            }
    cleanupAsyncOutcome <- try (compilePostCommitIndexWith cleanupAsyncDependencies repository revision cleanupAsyncDatabase) :: IO (Either AsyncException PostCommitIndexResult)
    cleanupAsyncOutcome @?= Left UserInterrupt
    cleanupAsyncCandidate <- requireCaptured "candidate" cleanupAsyncCandidateRef
    cleanupAsyncBackup <- requireCaptured "backup" cleanupAsyncBackupRef
    readIORef cleanupAsyncCalls >>= (@?= publicationCleanupOrder cleanupAsyncCandidate cleanupAsyncBackup)
    BS.readFile cleanupAsyncDatabase >>= (@?= cleanupAsyncPrior)
    assertNoPublicationArtifacts cleanupAsyncCandidate cleanupAsyncBackup
    assertDatabaseReusable cleanupAsyncDatabase revision

    let synchronousDatabase = temporary </> "synchronous-order.sqlite"
    synchronousPrior <- installPreparedPostCommitDatabase directory synchronousDatabase
    synchronousCandidateRef <- newIORef Nothing
    synchronousBackupRef <- newIORef Nothing
    synchronousCleanupCalls <- newIORef []
    synchronousInstallStarted <- newIORef False
    let synchronousOpen = capturePublicationTemporary base synchronousCandidateRef synchronousBackupRef
        orderedInstallFailure _ candidatePath (Just _) = do
          writeIORef synchronousInstallStarted True
          writeOwnedSidecars candidatePath
          pure (Left (PostCommitIndexInstallFailure "ordered install failure"))
        orderedInstallFailure _ _ Nothing = pure (Left (PostCommitIndexInstallFailure "missing replacement backup"))
        removeWithOrderedFailures path = do
          atomicModifyIORef' synchronousCleanupCalls $ \paths -> (paths <> [path], ())
          postCommitRemoveOwnedFile base path
          candidatePath <- requireCaptured "candidate" synchronousCandidateRef
          backupPath <- requireCaptured "backup" synchronousBackupRef
          installStarted <- readIORef synchronousInstallStarted
          if installStarted && path `elem` [candidatePath, candidatePath <> "-journal", backupPath]
            then throwIO (userError ("ordered cleanup failure: " <> path))
            else pure ()
        synchronousDependencies =
          base
            { postCommitOpenTemporary = synchronousOpen,
              postCommitInstallDatabase = orderedInstallFailure,
              postCommitRemoveOwnedFile = removeWithOrderedFailures
            }
    synchronousResult <- compilePostCommitIndexWith synchronousDependencies repository revision synchronousDatabase
    synchronousCandidate <- requireCaptured "candidate" synchronousCandidateRef
    synchronousBackup <- requireCaptured "backup" synchronousBackupRef
    readIORef synchronousCleanupCalls >>= (@?= publicationCleanupOrder synchronousCandidate synchronousBackup)
    case postCommitIndexError synchronousResult of
      Just
        ( PostCommitIndexMultipleFailures
            [ PostCommitIndexPublishFailure (PostCommitIndexInstallFailure "ordered install failure"),
              PostCommitIndexCleanupFailure firstPath _,
              PostCommitIndexCleanupFailure secondPath _,
              PostCommitIndexCleanupFailure thirdPath _
              ]
          ) ->
            [firstPath, secondPath, thirdPath]
              @?= [synchronousCandidate, synchronousCandidate <> "-journal", synchronousBackup]
      other -> assertFailure ("expected ordered install and cleanup failures, got " <> show other)
    BS.readFile synchronousDatabase >>= (@?= synchronousPrior)
    assertNoPublicationArtifacts synchronousCandidate synchronousBackup
    assertDatabaseReusable synchronousDatabase revision

    let restoreDatabase = temporary </> "restore-after-effect.sqlite"
    restorePrior <- installPreparedPostCommitDatabase directory restoreDatabase
    restoreCandidateRef <- newIORef Nothing
    restoreBackupRef <- newIORef Nothing
    restoreCleanupCalls <- newIORef []
    let restoreOpen = capturePublicationTemporary base restoreCandidateRef restoreBackupRef
        partialSynchronousFailure target candidatePath (Just backupPath) = do
          writeOwnedSidecars candidatePath
          renameFile target backupPath
          pure (Left (PostCommitIndexInstallFailure "partial synchronous install"))
        partialSynchronousFailure _ _ Nothing = pure (Left (PostCommitIndexInstallFailure "missing replacement backup"))
        restoreThenCancel backupPath target = renameFile backupPath target >> throwIO UserInterrupt
        restoreRemove path = do
          atomicModifyIORef' restoreCleanupCalls $ \paths -> (paths <> [path], ())
          postCommitRemoveOwnedFile base path
        restoreDependencies =
          base
            { postCommitOpenTemporary = restoreOpen,
              postCommitInstallDatabase = partialSynchronousFailure,
              postCommitRestoreDatabase = restoreThenCancel,
              postCommitRemoveOwnedFile = restoreRemove
            }
    restoreOutcome <- try (compilePostCommitIndexWith restoreDependencies repository revision restoreDatabase) :: IO (Either AsyncException PostCommitIndexResult)
    restoreOutcome @?= Left UserInterrupt
    restoreCandidate <- requireCaptured "candidate" restoreCandidateRef
    restoreBackup <- requireCaptured "backup" restoreBackupRef
    readIORef restoreCleanupCalls >>= (@?= publicationCleanupOrder restoreCandidate restoreBackup)
    BS.readFile restoreDatabase >>= (@?= restorePrior)
    assertNoPublicationArtifacts restoreCandidate restoreBackup
    assertDatabaseReusable restoreDatabase revision

postCommitIndexPublishProbeFailureFailStop :: IO PostCommitRepositorySeed -> IO ()
postCommitIndexPublishProbeFailureFailStop getSeed =
  withPostCommitRepositoryCopy getSeed "adrai post-commit reconciliation probe" $ \temporary _ repository commitOid base -> do
    let database = temporary </> "index.sqlite"
        priorBytes = "prior bytes retained at recovery backup"
    BS.writeFile database priorBytes
    candidateRef <- newIORef Nothing
    backupRef <- newIORef Nothing
    probeCalls <- newIORef (0 :: Int)
    let openTracked directoryPath template = do
          temporaryPath <- postCommitOpenTemporary base directoryPath template
          if ".post-commit-backup-" `isInfixOf` template
            then writeIORef backupRef (Just (fst temporaryPath))
            else writeIORef candidateRef (Just (fst temporaryPath))
          pure temporaryPath
        partiallyInstall target _ (Just backup) = do
          renameFile target backup
          pure (Left (PostCommitIndexInstallFailure "injected partial install"))
        partiallyInstall _ _ Nothing = pure (Left (PostCommitIndexInstallFailure "missing injected backup"))
        failReconciliationProbe path = do
          call <- atomicModifyIORef' probeCalls $ \count -> let next = count + 1 in (next, next)
          if call == 2
            then throwIO (userError "injected reconciliation probe failure")
            else postCommitDoesOwnedFileExist base path
        probeFailure =
          base
            { postCommitOpenTemporary = openTracked,
              postCommitDoesOwnedFileExist = failReconciliationProbe,
              postCommitInstallDatabase = partiallyInstall
            }
    result <- compilePostCommitIndexWith probeFailure repository commitOid database
    case postCommitIndexError result of
      Just
        ( PostCommitIndexPublishFailure
            (PostCommitIndexRecoveryInvariantFailure (PostCommitIndexInstallFailure "injected partial install") detail)
          ) ->
            assertBool "probe failure detail was discarded" ("injected reconciliation probe failure" `Text.isInfixOf` detail)
      other -> assertFailure ("expected typed reconciliation probe failure, got " <> show other)
    candidate <- readIORef candidateRef >>= maybe (assertFailure "missing captured candidate") pure
    backup <- readIORef backupRef >>= maybe (assertFailure "missing captured backup") pure
    doesFileExist database >>= (@?= False)
    BS.readFile backup >>= (@?= priorBytes)
    assertOwnedPathsExist candidate [True, False, False, False]
    removeFile candidate
    removeFile backup

postCommitIndexPublishInvariantFailStop :: IO PostCommitRepositorySeed -> IO ()
postCommitIndexPublishInvariantFailStop getSeed =
  withPostCommitRepositoryCopy getSeed "adrai post-commit publish invariant" $ \temporary _ repository commitOid base -> do
    let database = temporary </> "index.sqlite"
    BS.writeFile database "prior target deliberately removed by invalid seam"
    candidateRef <- newIORef Nothing
    backupRef <- newIORef Nothing
    let openTracked directoryPath template = do
          temporaryPath <- postCommitOpenTemporary base directoryPath template
          if ".post-commit-backup-" `isInfixOf` template
            then writeIORef backupRef (Just (fst temporaryPath))
            else writeIORef candidateRef (Just (fst temporaryPath))
          pure temporaryPath
        violateDocumentedState target _ _ = do
          removeFile target
          pure (Left (PostCommitIndexInstallFailure "injected impossible publication state"))
        invariantFailure =
          base
            { postCommitOpenTemporary = openTracked,
              postCommitInstallDatabase = violateDocumentedState
            }
    result <- compilePostCommitIndexWith invariantFailure repository commitOid database
    case postCommitIndexError result of
      Just
        ( PostCommitIndexPublishFailure
            (PostCommitIndexRecoveryInvariantFailure (PostCommitIndexInstallFailure "injected impossible publication state") detail)
          ) -> do
            assertBool "missing invariant state detail" ("target_exists=False" `Text.isInfixOf` detail)
            assertBool "missing retained-candidate state detail" ("candidate_exists=True" `Text.isInfixOf` detail)
      other -> assertFailure ("expected typed recovery invariant failure, got " <> show other)
    candidate <- readIORef candidateRef >>= maybe (assertFailure "missing captured candidate") pure
    backup <- readIORef backupRef >>= maybe (assertFailure "missing captured backup") pure
    doesFileExist database >>= (@?= False)
    assertOwnedPathsExist candidate [True, False, False, False]
    doesFileExist backup >>= (@?= False)
    removeFile candidate

postCommitIndexTemporaryCloseFailureCleansCandidate :: IO PostCommitRepositorySeed -> IO ()
postCommitIndexTemporaryCloseFailureCleansCandidate getSeed =
  withPostCommitRepositoryCopy getSeed "adrai post-commit temp close failure" $ \temporary _ repository commitOid base -> do
    let database = temporary </> "index.sqlite"
        priorBytes = "prior index survives temp close failure"
    BS.writeFile database priorBytes
    closeCalls <- newIORef (0 :: Int)
    candidateRef <- newIORef Nothing
    let openTracked directoryPath template = do
          candidate <- postCommitOpenTemporary base directoryPath template
          writeIORef candidateRef (Just (fst candidate))
          pure candidate
        closeTemporary handle = do
          call <- atomicModifyIORef' closeCalls $ \count -> let next = count + 1 in (next, next)
          if call == 1 then throwIO (userError "injected temp close failure") else hClose handle
        closeFailure =
          base
            { postCommitOpenTemporary = openTracked,
              postCommitCloseTemporary = closeTemporary
            }
    result <- compilePostCommitIndexWith closeFailure repository commitOid database
    case postCommitIndexError result of
      Just (PostCommitIndexTemporaryCloseFailure _) -> pure ()
      other -> assertFailure ("expected temp close failure, got " <> show other)
    readIORef closeCalls >>= (@?= 2)
    BS.readFile database >>= (@?= priorBytes)
    candidate <- readIORef candidateRef >>= maybe (assertFailure "missing captured candidate") pure
    assertOwnedPathsExist candidate [False, False, False, False]

postCommitIndexCleanupFailureIsOrdered :: IO PostCommitRepositorySeed -> IO ()
postCommitIndexCleanupFailureIsOrdered getSeed =
  withPostCommitRepositoryCopy getSeed "adrai post-commit cleanup ordering" $ \temporary _ repository commitOid base -> do
    let database = temporary </> "index.sqlite"
        priorBytes = "prior index survives cleanup failure"
    BS.writeFile database priorBytes
    candidateRef <- newIORef Nothing
    cleanupCalls <- newIORef []
    let openTracked path = writeIORef candidateRef (Just path) >> postCommitOpenDatabase base path
        failAfterSidecars _ _ = do
          readIORef candidateRef >>= \case
            Nothing -> throwIO (userError "candidate path was not captured")
            Just path -> do
              mapM_ (\suffix -> BS.writeFile (path <> suffix) "owned") ["-journal", "-shm", "-wal"]
              throwIO (userError "injected compile failure")
        removeTracked path = do
          atomicModifyIORef' cleanupCalls $ \paths -> (paths <> [path], ())
          if "-shm" `isSuffixOf` path
            then throwIO (userError "injected cleanup failure")
            else postCommitRemoveOwnedFile base path
        cleanupFailure =
          base
            { postCommitOpenDatabase = openTracked,
              postCommitColdCompile = failAfterSidecars,
              postCommitRemoveOwnedFile = removeTracked
            }
    result <- compilePostCommitIndexWith cleanupFailure repository commitOid database
    candidate <- readIORef candidateRef >>= maybe (assertFailure "missing captured candidate") pure
    let cleanupOrder = [candidate, candidate <> "-journal", candidate <> "-shm", candidate <> "-wal"]
    readIORef cleanupCalls >>= (@?= cleanupOrder)
    case postCommitIndexError result of
      Just (PostCommitIndexMultipleFailures [PostCommitIndexCompileException _, PostCommitIndexCleanupFailure path _]) -> path @?= candidate <> "-shm"
      other -> assertFailure ("expected compile then cleanup failures, got " <> show other)
    BS.readFile database >>= (@?= priorBytes)
    assertOwnedPathsExist candidate [False, False, True, False]
    removeFile (candidate <> "-shm")

postCommitIndexLiveCloseFailureIsObservable :: IO PostCommitRepositorySeed -> IO ()
postCommitIndexLiveCloseFailureIsObservable getSeed =
  withPostCommitRepositoryCopy getSeed "adrai post-commit live database close failure" $ \temporary _ repository commitOid base -> do
    let database = temporary </> "index.sqlite"
        priorBytes = "prior index survives live close failure"
    BS.writeFile database priorBytes
    candidateRef <- newIORef Nothing
    connectionRef <- newIORef Nothing
    let openTracked path = do
          connection <- postCommitOpenDatabase base path
          writeIORef candidateRef (Just path)
          writeIORef connectionRef (Just connection)
          pure connection
        leaveLive _ = throwIO (userError "injected close failure before close")
        removeBlocked path = do
          candidate <- readIORef candidateRef
          if candidate == Just path
            then throwIO (userError "injected cleanup failure while database is live")
            else postCommitRemoveOwnedFile base path
        closeFailure =
          base
            { postCommitOpenDatabase = openTracked,
              postCommitCloseDatabase = leaveLive,
              postCommitRemoveOwnedFile = removeBlocked
            }
    result <- compilePostCommitIndexWith closeFailure repository commitOid database
    candidate <- readIORef candidateRef >>= maybe (assertFailure "missing captured candidate") pure
    connection <- readIORef connectionRef >>= maybe (assertFailure "missing captured connection") pure
    case postCommitIndexError result of
      Just (PostCommitIndexMultipleFailures [PostCommitIndexCloseFailure _, PostCommitIndexCleanupFailure path _]) -> path @?= candidate
      other -> assertFailure ("expected close then cleanup failures, got " <> show other)
    -- The injected close failed before closing: a query proves the handle is
    -- still live, while the exact prior target was never selected for publish.
    query_ connection "SELECT value FROM meta WHERE key = 'resolved_oid'" >>= (@?= [Only (gitOidText commitOid)])
    BS.readFile database >>= (@?= priorBytes)
    assertOwnedPathsExist candidate [True, False, False, False]
    close connection
    removeFile candidate

assertOwnedPathsExist :: FilePath -> [Bool] -> IO ()
assertOwnedPathsExist candidate expected =
  mapM doesFileExist [candidate, candidate <> "-journal", candidate <> "-shm", candidate <> "-wal"] >>= (@?= expected)

capturePublicationTemporary :: PostCommitIndexDependencies -> IORef (Maybe FilePath) -> IORef (Maybe FilePath) -> FilePath -> String -> IO (FilePath, Handle)
capturePublicationTemporary base candidateRef backupRef directoryPath template = do
  temporaryPath <- postCommitOpenTemporary base directoryPath template
  if ".post-commit-backup-" `isInfixOf` template
    then writeIORef backupRef (Just (fst temporaryPath))
    else writeIORef candidateRef (Just (fst temporaryPath))
  pure temporaryPath

requireCaptured :: String -> IORef (Maybe value) -> IO value
requireCaptured label ref =
  readIORef ref >>= maybe (assertFailure ("missing captured " <> label)) pure

writeOwnedSidecars :: FilePath -> IO ()
writeOwnedSidecars candidate =
  mapM_ (\suffix -> BS.writeFile (candidate <> suffix) "owned") ["-journal", "-shm", "-wal"]

ownedTemporaryPathsForTest :: FilePath -> [FilePath]
ownedTemporaryPathsForTest candidate =
  [candidate, candidate <> "-journal", candidate <> "-shm", candidate <> "-wal"]

publicationCleanupOrder :: FilePath -> FilePath -> [FilePath]
publicationCleanupOrder candidate backup = backup : ownedTemporaryPathsForTest candidate <> [backup]

assertNoPublicationArtifacts :: FilePath -> FilePath -> IO ()
assertNoPublicationArtifacts candidate backup = do
  assertOwnedPathsExist candidate [False, False, False, False]
  doesFileExist backup >>= (@?= False)

assertDatabaseRevision :: FilePath -> GitOid -> IO ()
assertDatabaseRevision database revision =
  bracket (open database) close $ \connection -> do
    resolved <- query_ connection "SELECT value FROM meta WHERE key = 'resolved_oid'" :: IO [Only Text.Text]
    resolved @?= [Only (gitOidText revision)]

ownedCandidateSiblings :: FilePath -> FilePath -> IO [FilePath]
ownedCandidateSiblings directory _database = do
  siblings <- listDirectory directory
  pure (sort (filter (".post-commit-" `isInfixOf`) siblings))

requireSinglePublicationCandidate :: FilePath -> IO FilePath
requireSinglePublicationCandidate directory = do
  siblings <- listDirectory directory
  let candidates =
        [ directory </> sibling
          | sibling <- siblings,
            ".post-commit-" `isInfixOf` sibling,
            not (".post-commit-backup-" `isInfixOf` sibling),
            not (any (`isSuffixOf` sibling) ["-journal", "-shm", "-wal"])
        ]
  case candidates of
    [candidate] -> pure candidate
    other -> assertFailure ("expected one owned publication candidate, got " <> show other)

assertIndexed :: Repository -> GitOid -> FilePath -> IO ()
assertIndexed repository commitOid database = do
  result <- compilePostCommitIndex repository commitOid database
  assertBool ("expected post-commit indexing success, got " <> show result) (postCommitIndexed result)
  postCommitDatabase result @?= Just database
  postCommitIndexRevision result @?= Just commitOid
  postCommitIndexError result @?= Nothing
  postCommitIndexWarnings result @?= sort (postCommitIndexWarnings result)
  bracket (open database) close $ \connection -> do
    schemas <- query_ connection "SELECT value FROM meta WHERE key = 'schema'" :: IO [Only Text.Text]
    schemas @?= [Only "adrai-cache/3"]
    resolved <- query_ connection "SELECT value FROM meta WHERE key = 'resolved_oid'" :: IO [Only Text.Text]
    resolved @?= [Only (gitOidText commitOid)]

assertDatabaseReusable :: FilePath -> GitOid -> IO ()
assertDatabaseReusable database commitOid = do
  bracket (open database) close $ \connection -> do
    schemas <- query_ connection "SELECT value FROM meta WHERE key = 'schema'" :: IO [Only Text.Text]
    schemas @?= [Only "adrai-cache/3"]
    resolved <- query_ connection "SELECT value FROM meta WHERE key = 'resolved_oid'" :: IO [Only Text.Text]
    resolved @?= [Only (gitOidText commitOid)]
    execute_ connection "BEGIN IMMEDIATE"
    ( do
        execute_ connection "INSERT INTO meta(key, value) VALUES ('retained_reopen_probe', '1')"
        query_ connection "SELECT value FROM meta WHERE key = 'retained_reopen_probe'" >>= (@?= [Only ("1" :: Text.Text)])
      ) `finally` execute_ connection "ROLLBACK"
  bracket (open database) close $ \connection -> do
    probe <- query_ connection "SELECT value FROM meta WHERE key = 'retained_reopen_probe'" :: IO [Only Text.Text]
    probe @?= []
    resolved <- query_ connection "SELECT value FROM meta WHERE key = 'resolved_oid'" :: IO [Only Text.Text]
    resolved @?= [Only (gitOidText commitOid)]

installPreparedPostCommitDatabase :: FilePath -> FilePath -> IO BS.ByteString
installPreparedPostCommitDatabase directory database = do
  copyFile (directory </> postCommitPreparedDatabaseName) database
  BS.readFile database

amendRejectionsPreserveRepository :: IO MutationRepositorySeed -> IO ()
amendRejectionsPreserveRepository getSeed =
  withMutationRepositoryCopy getSeed "adrai amend rejection sequence" $ \directory repository created -> do
    observer <- newAmendFailureObserver

    _ <- assertRejectedWithoutAmendStateChange observer directory
      (Stage3ValidateState "amend would not change the current decision")
      (runAmend repository (createAdrId created) (createRecordId created) "" "" "")

    _ <- gitSuccess directory ["checkout", "--detach"] BS.empty
    invalidateAmendFailureObserver observer
    _ <- assertRejectedWithoutAmendStateChange observer directory
      (Stage3ValidateState "HEAD is detached; attach a branch first")
      (runAmend repository (createAdrId created) (createRecordId created) "Detached failure" "" "body\n")

    _ <- gitSuccess directory ["checkout", "main"] BS.empty
    invalidateAmendFailureObserver observer
    platform <- assertRight (mkDomain "platform")
    first <- commitScopeExpansion directory created "C00000000000000000000000013" "O00000000000000000000000013" "first/**"
    _ <- gitSuccess directory ["reset", "--hard", Text.unpack (gitOidText (createCommitOid created))] BS.empty
    _ <- commitScopeExpansion directory created "C00000000000000000000000014" "O00000000000000000000000014" "second/**"
    _ <- gitSuccess directory ["cherry-pick", Text.unpack first] BS.empty
    invalidateAmendFailureObserver observer
    conflictState <- assertRejectedWithoutAmendStateChange observer directory
      (Stage3ValidateState "amend target ADR is conflicted")
      (runCurrentAmend repository (createAdrId created) Nothing "unsafe scope conflict" "Rejected" "Rejected summary" "rejected body\n")
    _ <- assertRejectedFromAmendState observer directory conflictState
      (Stage3ValidateState "domain target ADR is conflicted")
      (runDomain repository (createAdrId created) Nothing "Unsafe" (DomainReviewedSet [platform]))
    pure ()

data AmendImmutableObservation = AmendImmutableObservation
  { amendImmutableHeadOid :: Text.Text,
    amendImmutableTreeOid :: Text.Text,
    amendImmutableTreeEntries :: BS.ByteString,
    amendImmutableManagedContents :: [(Text.Text, BS.ByteString)]
  }
  deriving (Eq, Show)

newtype AmendFailureObserver = AmendFailureObserver (IORef (Maybe AmendImmutableObservation))

data AmendFailureSnapshot = AmendFailureSnapshot
  { amendRawHead :: BS.ByteString,
    amendLooseRefs :: [(Text.Text, Maybe BS.ByteString)],
    amendPackedRefs :: Maybe BS.ByteString,
    amendRawIndex :: BS.ByteString,
    amendIndexEntries :: BS.ByteString,
    amendCallerStatus :: BS.ByteString,
    amendWorktreeInventory :: [(Text.Text, Maybe BS.ByteString)],
    amendGitControlInventory :: [(Text.Text, Maybe BS.ByteString)],
    amendReflogInventory :: [(Text.Text, Maybe BS.ByteString)],
    amendDisposableIndexes :: [(FilePath, BS.ByteString)],
    amendImmutableObservation :: AmendImmutableObservation
  }
  deriving (Eq, Show)

newAmendFailureObserver :: IO AmendFailureObserver
newAmendFailureObserver = AmendFailureObserver <$> newIORef Nothing

invalidateAmendFailureObserver :: AmendFailureObserver -> IO ()
invalidateAmendFailureObserver (AmendFailureObserver cached) = writeIORef cached Nothing

assertRejectedWithoutAmendStateChange :: (Eq value, Show value) =>
  AmendFailureObserver ->
  FilePath ->
  TransactionError ->
  IO (Either TransactionError value) ->
  IO AmendFailureSnapshot
assertRejectedWithoutAmendStateChange observer directory expected action = do
  before <- observeAmendFailureState observer directory
  assertRejectedFromAmendState observer directory before expected action

assertRejectedFromAmendState :: (Eq value, Show value) =>
  AmendFailureObserver ->
  FilePath ->
  AmendFailureSnapshot ->
  TransactionError ->
  IO (Either TransactionError value) ->
  IO AmendFailureSnapshot
assertRejectedFromAmendState observer directory before expected action = do
  result <- action
  result @?= Left expected
  after <- observeAmendFailureState observer directory
  after @?= before
  pure after

-- This observer is private to the consolidated amend carrier. It reads every
-- mutable repository surface relevant to rejection safety without discovery or
-- index refresh. Committed tree/content reads are cached only for the current
-- exact HEAD and the caller explicitly invalidates that cache at every topology
-- or history transition in the sequence above.
observeAmendFailureState :: AmendFailureObserver -> FilePath -> IO AmendFailureSnapshot
observeAmendFailureState (AmendFailureObserver cached) directory = do
  let gitDirectory = directory </> ".git"
  headOid <- gitText directory ["rev-parse", "HEAD"]
  immutable <- readIORef cached >>= \case
    Just observation | amendImmutableHeadOid observation == headOid -> pure observation
    _ -> do
      observation <-
        AmendImmutableObservation headOid
          <$> gitText directory ["rev-parse", "HEAD^{tree}"]
          <*> gitSuccess directory ["ls-tree", "-r", "-z", "HEAD"] BS.empty
          <*> managedCommittedBytes directory
      writeIORef cached (Just observation)
      pure observation
  rawHead <- BS.readFile (gitDirectory </> "HEAD")
  looseRefs <- amendFilesystemInventory (gitDirectory </> "refs") "refs" []
  packedRefs <- amendOptionalFile (gitDirectory </> "packed-refs")
  rawIndex <- BS.readFile (gitDirectory </> "index")
  indexEntries <- gitSuccess directory ["ls-files", "-s", "-z"] BS.empty
  status <- gitSuccess directory ["--no-optional-locks", "status", "--porcelain=v1", "--untracked-files=all", "-z"] BS.empty
  worktree <- amendFilesystemInventory directory "" [".git"]
  gitControl <- amendFilesystemInventory gitDirectory ".git" []
  reflogs <- amendFilesystemInventory (gitDirectory </> "logs") "logs" []
  disposableIndexes <- disposableIndexInventory directory
  pure
    AmendFailureSnapshot
      { amendRawHead = rawHead,
        amendLooseRefs = looseRefs,
        amendPackedRefs = packedRefs,
        amendRawIndex = rawIndex,
        amendIndexEntries = indexEntries,
        amendCallerStatus = status,
        amendWorktreeInventory = worktree,
        amendGitControlInventory = gitControl,
        amendReflogInventory = reflogs,
        amendDisposableIndexes = disposableIndexes,
        amendImmutableObservation = immutable
      }

amendOptionalFile :: FilePath -> IO (Maybe BS.ByteString)
amendOptionalFile path = doesFileExist path >>= \exists -> if exists then Just <$> BS.readFile path else pure Nothing

-- Directory entries are retained as Nothing so a rejection cannot leak an
-- empty path. Files retain exact bytes, including SQLite caches and Git refs.
amendFilesystemInventory :: FilePath -> FilePath -> [FilePath] -> IO [(Text.Text, Maybe BS.ByteString)]
amendFilesystemInventory root relativeRoot excluded = do
  exists <- doesDirectoryExist root
  if exists then walk root relativeRoot else pure []
  where
    walk native relative = do
      children <- sort <$> listDirectory native
      fmap concat (mapM (visit native relative) [child | child <- children, child `notElem` excluded])
    visit native relative child = do
      let childNative = native </> child
          childRelative = if null relative then child else relative <> "/" <> child
      directoryChild <- doesDirectoryExist childNative
      if directoryChild
        then ((Text.pack (childRelative <> "/"), Nothing) :) <$> walk childNative childRelative
        else do
          fileChild <- doesFileExist childNative
          if fileChild
            then do
              bytes <- BS.readFile childNative
              pure [(Text.pack childRelative, Just bytes)]
            else pure [(Text.pack childRelative, Nothing)]

scopeUpdatesAreTruthful :: IO MutationRepositorySeed -> IO ()
scopeUpdatesAreTruthful getSeed =
  withMutationRepositoryCopy getSeed "adrai scope updates" $ \directory repository created -> do
    assets <- assertRight (mkScopePattern "assets/**")
    docs <- assertRight (mkScopePattern "docs/**")
    source <- assertRight (mkScopePattern "src/**")
    inputs <- scopeInputs
    beforeMs <- posixTimeMs
    mixed <- assertRight =<< runScopeWithInputs repository (createAdrId created) Nothing [docs, assets] [source] inputs
    afterMs <- posixTimeMs
    document <- scopeDocumentAt directory mixed
    gitText directory ["rev-parse", "HEAD"] >>= (@?= gitOidText (scopeChangeCommitOid mixed))
    scopeChangeCreatedPaths mixed @?= [scopeChangeNewPath mixed]
    assertBool "scope transaction reports index refresh" (scopeChangeIndexUpdated mixed)
    case (parsedManagedRecord document, connectionPayloadFrom document) of
        (ManagedConnection connection, Just payload) -> do
          connectionRecordId connection @?= scopeChangeConnectionId mixed
          appliesToSubjectAdr payload @?= createAdrId created
          provenanceObjectId (parsedManagedCapsule document) @?= ProvenanceConnection (scopeChangeConnectionId mixed)
          provenanceBasis (parsedManagedCapsule document) @?= createCommitOid created
          provenanceActor (parsedManagedCapsule document) @?= createActorPure
          provenanceBranchHint (parsedManagedCapsule document) @?= Just "main"
          provenanceToolVersion (parsedManagedCapsule document) @?= "adrai/1.0.0"
          provenanceInputs (parsedManagedCapsule document) @?= inputs
          canonicalManagedPath (configManagedPaths defaultConfig) (parsedManagedRecord document) @?= Right (scopeChangeNewPath mixed)
        other -> assertFailure ("expected one scope connection, got " <> show other)
    assertScopeDocument mixed (createScopeId created) "mixed" [assets, docs] [source] [assets, docs] document
    let timestamp = provenanceTimestampMs (parsedManagedCapsule document)
    assertBool "scope timestamp is a real positive operation timestamp" (timestamp >= beforeMs && timestamp <= afterMs)
    scopeChangeAdrId mixed @?= createAdrId created
    operationIdText (provenanceOperationId (parsedManagedCapsule document)) @?= Text.pack (scopeChangeOperationId mixed)
    finalManagedPaths <- managedCommittedPaths directory
    sort finalManagedPaths @?= sort (map repoPathText (createCreatedPaths created) <> [repoPathText (scopeChangeNewPath mixed)])
  where
    createActorPure = case mkActor HumanActor "mutation-service-test" Nothing of Right actor -> actor; Left err -> error (show err)

scopeReviewedSetMergesUnchangedUnion :: IO MutationRepositorySeed -> IO ()
scopeReviewedSetMergesUnchangedUnion getSeed =
  withMutationRepositoryCopy getSeed "adrai scope unchanged-union merge" $ \directory repository created -> do
    leftPattern <- assertRight (mkScopePattern "left/**")
    rightPattern <- assertRight (mkScopePattern "right/**")
    source <- assertRight (mkScopePattern "src/**")
    left <- assertRight =<< runScope repository (createAdrId created) Nothing [leftPattern] []
    _ <- gitSuccess directory ["reset", "--hard", Text.unpack (gitOidText (createCommitOid created))] BS.empty
    right <- assertRight =<< runScope repository (createAdrId created) Nothing [rightPattern] []
    _ <- gitSuccess directory ["cherry-pick", Text.unpack (gitOidText (scopeChangeCommitOid left))] BS.empty
    conflicted <- currentReducedAdr (createAdrId created) =<< committedManagedDocuments directory
    let parents = sort [scopeChangeConnectionId left, scopeChangeConnectionId right]
        union = sort [source, leftPattern, rightPattern]
    axisResolutionHeads (reducedScopeAxis conflicted) @?= parents
    inputs <- scopeInputs
    basisBefore <- gitText directory ["rev-parse", "HEAD"]
    beforeMs <- posixTimeMs
    merged <- assertRight =<< runScopeSetWithInputs repository (createAdrId created) (Just (reducedStateToken conflicted)) "Reconcile scope heads" union inputs
    afterMs <- posixTimeMs
    document <- scopeDocumentAt directory merged
    assertScopeDocumentWithParents merged parents "merge" [] [] union document
    case parsedManagedRecord document of
      ManagedConnection connection -> connectionRationale connection @?= "Reconcile scope heads\n"
      _ -> assertFailure "expected scope connection"
    let capsule = parsedManagedCapsule document
    gitOidText (provenanceBasis capsule) @?= basisBefore
    provenanceActor capsule @?= createActorPure
    provenanceBranchHint capsule @?= Just "main"
    provenanceInputs capsule @?= inputs
    assertBool "empty-delta merge timestamp is positive and from this operation" (provenanceTimestampMs capsule >= beforeMs && provenanceTimestampMs capsule <= afterMs && provenanceTimestampMs capsule > 0)
    canonicalManagedPath (configManagedPaths defaultConfig) (parsedManagedRecord document) @?= Right (scopeChangeNewPath merged)
    scopeChangeCreatedPaths merged @?= [scopeChangeNewPath merged]
    assertBool "empty-delta merge transaction refreshed the index" (scopeChangeIndexUpdated merged)
    resolved <- currentReducedAdr (createAdrId created) =<< committedManagedDocuments directory
    reducedConflictAxes resolved @?= []
    axisResolutionHeads (reducedScopeAxis resolved) @?= [scopeChangeConnectionId merged]
    axisResolutionEffective (reducedScopeAxis resolved) @?= union
  where
    createActorPure = case mkActor HumanActor "mutation-service-test" Nothing of Right actor -> actor; Left err -> error (show err)

scopeRejectionsPreserveRepository :: IO MutationRepositorySeed -> IO ()
scopeRejectionsPreserveRepository getSeed =
  withMutationRepositoryCopy getSeed "adrai scope rejection matrix" $ \directory repository created -> do
    extra <- assertRight (mkScopePattern "test/**")
    stale <- assertRight (mkStateToken "S0000000000000000000000")
    let stagedPath = directory </> "scope-unrelated-staged.txt"
        dirtyPath = directory </> "scope-unrelated-dirty.txt"
    _ <- commitFile directory "scope-unrelated-dirty.txt" "committed\n"
    BS.writeFile stagedPath "staged\NULbytes"
    _ <- gitSuccess directory ["add", "scope-unrelated-staged.txt"] BS.empty
    BS.writeFile dirtyPath "dirty\NULbytes"
    before <- scopeFailureSnapshot directory [stagedPath, dirtyPath]
    let reject expected action = do
          result <- action
          result @?= Left (Stage3ValidateState expected)
    reject "scope additions and removals overlap" (runScope repository (createAdrId created) Nothing [extra] [extra])
    reject "reviewed scope set contains duplicates" (runScopeSet repository (createAdrId created) Nothing "Reviewed" [extra, extra])
    current <- currentReducedAdr (createAdrId created) =<< committedManagedDocuments directory
    staleResult <- runScope repository (createAdrId created) (Just stale) [extra] []
    staleResult @?= Left (Stage3ValidateState ("stale ADR state: expected " <> stateTokenText stale <> ", current state is " <> stateTokenText (reducedStateToken current)))
    scopeFailureSnapshot directory [stagedPath, dirtyPath] >>= (@?= before)

rejectNonScopeConflict :: IO MutationRepositorySeed -> IO ()
rejectNonScopeConflict getSeed =
  withMutationRepositoryCopy getSeed "adrai scope non-axis conflict" $ \directory repository created -> do
    firstCommit <- commitDomainExpansion directory created "C00000000000000000000000011" "O00000000000000000000000011" "platform"
    _ <- gitSuccess directory ["reset", "--hard", Text.unpack (gitOidText (createCommitOid created))] BS.empty
    _ <- commitDomainExpansion directory created "C00000000000000000000000012" "O00000000000000000000000012" "product"
    _ <- gitSuccess directory ["cherry-pick", Text.unpack firstCommit] BS.empty
    finalPattern <- assertRight (mkScopePattern "final/**")
    prepareScopeObservableFiles directory
    let stagedPath = directory </> "scope-unrelated-staged.txt"
        dirtyPath = directory </> "scope-unrelated-dirty.txt"
    before <- scopeFailureSnapshot directory [stagedPath, dirtyPath]
    result <- runScopeSet repository (createAdrId created) Nothing "Reviewed but unsafe" [finalPattern]
    result @?= Left (Stage3ValidateState "scope target ADR is conflicted")
    scopeFailureSnapshot directory [stagedPath, dirtyPath] >>= (@?= before)

domainMixedUpdateIsTruthful :: IO MutationRepositorySeed -> IO ()
domainMixedUpdateIsTruthful getSeed =
  withMutationRepositoryCopy getSeed "adrai domain updates" $ \directory repository created -> do
    productDomain <- assertRight (mkDomain "product")
    compiler <- assertRight (mkDomain "compiler")
    mixed <- assertRight =<< runDomain repository (createAdrId created) Nothing "Replace compiler" (DomainDelta [productDomain] [compiler])
    assertDomainUpdate directory mixed [createDomainId created] "mixed" [productDomain] [compiler] [productDomain] [] "Replace compiler\n"
    assertDomainCommitMetadata directory mixed
    reduced <- currentReducedAdr (createAdrId created) =<< committedManagedDocuments directory
    axisResolutionHeads (reducedDomainAxis reduced) @?= [domainChangeConnectionId mixed]
    axisResolutionEffective (reducedDomainAxis reduced) @?= [productDomain]

domainRejectionsPreserveRepository :: IO MutationRepositorySeed -> IO ()
domainRejectionsPreserveRepository getSeed =
  withMutationRepositoryCopy getSeed "adrai domain rejection matrix" $ \directory repository created -> do
    compiler <- assertRight (mkDomain "compiler")
    productDomain <- assertRight (mkDomain "product")
    productApi <- assertRight (mkDomain "product.api")
    prepareScopeObservableFiles directory
    let cachePath = directory </> "domain-rejections.sqlite"
    BS.writeFile cachePath "durable rejection cache\NULbytes"
    before <- scopeFailureSnapshot directory [cachePath]
    let reject expected action = do
          result <- action
          result @?= Left (Stage3ValidateState expected)
    reject "domain additions and removals overlap" (runDomain repository (createAdrId created) Nothing "Reason" (DomainDelta [compiler] [compiler]))
    reject "domain set: DomainAntichainViolation (Domain \"product\") (Domain \"product.api\")" (runDomain repository (createAdrId created) Nothing "Reason" (DomainReviewedSet [productDomain, productApi]))
    refinement <- assertRight (mkDomainRefinement productDomain productApi)
    reject "domain refinement source is not active" (runDomain repository (createAdrId created) Nothing "Reason" (DomainRefine [refinement]))
    stale <- assertRight (mkStateToken "S0000000000000000000000")
    current <- currentReducedAdr (createAdrId created) =<< committedManagedDocuments directory
    staleResult <- runDomain repository (createAdrId created) (Just stale) "Reason" (DomainReviewedSet [compiler])
    staleResult @?= Left (Stage3ValidateState ("stale ADR state: expected " <> stateTokenText stale <> ", current state is " <> stateTokenText (reducedStateToken current)))
    scopeFailureSnapshot directory [cachePath] >>= (@?= before)

statusRejectionsPreserveRepository :: IO MutationRepositorySeed -> IO ()
statusRejectionsPreserveRepository getSeed =
  withMutationRepositoryCopy getSeed "adrai status rejections" $ \directory repository created -> do
    let callerFiles = [directory </> "status-unrelated-staged.txt", directory </> "status-unrelated-dirty.txt"]
    _ <- gitSuccess directory ["update-index", "--add", "--cacheinfo", "100644," <> replicate 40 '1' <> ",status-unrelated-staged.txt"] BS.empty
    BS.writeFile (directory </> "status-unrelated-dirty.txt") "dirty\n"
    before <- scopeFailureSnapshot directory callerFiles
    assertStatusError (Stage3ValidateState "obsolete reason must be nonblank") (runObsolete repository (createAdrId created) Nothing " \r\n " Nothing)
    stale <- assertRight (mkStateToken "S0000000000000000000000")
    current <- reducedAdrAt directory (createAdrId created)
    assertStatusError
      (Stage3ValidateState ("stale ADR state: expected " <> stateTokenText stale <> ", current state is " <> stateTokenText (reducedStateToken current)))
      (runObsolete repository (createAdrId created) (Just stale) "Reason" Nothing)
    scopeFailureSnapshot directory callerFiles >>= (@?= before)

statusResolveMergesConflict :: IO MutationRepositorySeed -> IO ()
statusResolveMergesConflict getSeed =
  withMutationRepositoryCopy getSeed "adrai status conflict merge" $ \directory repository created -> do
    left <- commitStatusBranch directory created "C00000000000000000000000009" "O00000000000000000000000009" StatusObsolete
    right <- commitStatusBranch directory created "C00000000000000000000000008" "O00000000000000000000000008" StatusActive
    before <- reducedAdrAt directory (createAdrId created)
    assertStatusFailure directory [] (Stage3ValidateState "status target ADR is conflicted") (runObsolete repository (createAdrId created) (Just (reducedStateToken before)) "No approval" Nothing)
    actor <- createActor
    inputs <- scopeInputs
    merged <- assertRight =<< obsoleteCommand repository (configManagedPaths defaultConfig) actor (createAdrId created) (ObsoleteRequest (Just (reducedStateToken before)) "Reviewed resolution" True Nothing) inputs
    obsoleteResolvedConflict merged @?= True
    obsoleteStatusParents merged @?= sort [left, right]
    document <- statusDocumentAt directory (obsoleteCommitOid merged) (obsoleteNewPath merged)
    provenanceParents (parsedManagedCapsule document) @?= map ProvenanceConnection (sort [left, right]) <> [ProvenanceRecord (createRecordId created)]

runObsolete :: Repository -> AdrId -> Maybe StateToken -> Text.Text -> Maybe AdrId -> IO (Either TransactionError ObsoleteResult)
runObsolete repository adr expected reason replacement = do
  actor <- createActor
  inputs <- scopeInputs
  obsoleteCommand repository (configManagedPaths defaultConfig) actor adr (ObsoleteRequest expected reason False replacement) inputs

reducedAdrAt :: FilePath -> AdrId -> IO ReducedAdr
reducedAdrAt directory adr = do
  committed <- managedCommittedBytes directory
  documents <- mapM parseOne committed
  case lookupReducedAdr adr (reduceManagedGraph (map parsedManagedRecord documents)) of
    Nothing -> assertFailure "expected reduced ADR" >> fail "unreachable"
    Just reduced -> pure reduced
  where
    parseOne (path, bytes) = do
      repoPath <- assertRight (mkRepoPath path)
      assertRight (parseManagedDocument repoPath bytes)

statusDocumentAt :: FilePath -> GitOid -> RepoPath -> IO ParsedManagedDocument
statusDocumentAt directory _commit path = do
  bytes <- BS.readFile (directory </> Text.unpack (repoPathText path))
  assertRight (parseManagedDocument path bytes)

assertStatusFailure :: FilePath -> [FilePath] -> TransactionError -> IO (Either TransactionError value) -> IO ()
assertStatusFailure directory callerFiles expected action = do
  before <- scopeFailureSnapshot directory callerFiles
  result <- action
  case result of
    Left actual -> actual @?= expected
    Right _ -> assertFailure ("expected status failure " <> show expected)
  scopeFailureSnapshot directory callerFiles >>= (@?= before)

assertStatusError :: TransactionError -> IO (Either TransactionError value) -> IO ()
assertStatusError expected action = do
  result <- action
  case result of
    Left actual -> actual @?= expected
    Right _ -> assertFailure ("expected status failure " <> show expected)

runDomain :: Repository -> AdrId -> Maybe StateToken -> Text.Text -> DomainChangeRequest -> IO (Either TransactionError DomainChangeResult)
runDomain repository adr expected reason request = do
  actor <- createActor
  inputs <- scopeInputs
  changeDomainCommand repository (configManagedPaths defaultConfig) actor adr expected reason request inputs

domainDocumentAt :: FilePath -> DomainChangeResult -> IO ParsedManagedDocument
domainDocumentAt directory update = do
  bytes <- BS.readFile (directory </> Text.unpack (repoPathText (domainChangeNewPath update)))
  assertRight (parseManagedDocument (domainChangeNewPath update) bytes)

assertDomainUpdate :: FilePath -> DomainChangeResult -> [ConnectionId] -> Text.Text -> [Domain] -> [Domain] -> [Domain] -> [DomainRefinement] -> Text.Text -> IO ()
assertDomainUpdate directory update parents mode added removed effective refinements rationale = do
  document <- domainDocumentAt directory update
  case parsedManagedRecord document of
    ManagedConnection connection -> case connectionPayload connection of
      DomainsConnection payload -> do
        domainsParentConnections payload @?= parents
        domainsChange payload @?= mode
        domainsAdded payload @?= added
        domainsRemoved payload @?= removed
        domainsEffective payload @?= effective
        domainsRefinements payload @?= refinements
        connectionRationale connection @?= rationale
        domainChangeAdded update @?= domainsAdded payload
        domainChangeRemoved update @?= domainsRemoved payload
        domainChangeRefinements update @?= domainsRefinements payload
      _ -> assertFailure "expected domain payload"
    _ -> assertFailure "expected domain connection"
  domainChangeParents update @?= parents
  domainChangeAdrId update @?= domainsSubjectAdrFrom document
  operationIdText (provenanceOperationId (parsedManagedCapsule document)) @?= Text.pack (domainChangeOperationId update)
  domainChangeMode update @?= mode
  domainChangeAdded update @?= added
  domainChangeRemoved update @?= removed
  domainChangeEffective update @?= effective
  domainChangeRefinements update @?= refinements
  domainChangeCreatedPaths update @?= [domainChangeNewPath update]
  assertBool "domain transaction refreshed index" (domainChangeIndexUpdated update)
  let capsule = parsedManagedCapsule document
  actor <- createActor
  inputs <- scopeInputs
  provenanceObjectId capsule @?= ProvenanceConnection (domainChangeConnectionId update)
  provenanceParents capsule @?= map ProvenanceConnection parents
  eventKindText (provenanceEventKind capsule) @?= "domain." <> mode
  provenanceBranchHint capsule @?= Just "main"
  provenanceActor capsule @?= actor
  provenanceInputs capsule @?= inputs
  provenanceToolVersion capsule @?= "adrai/1.0.0"
  assertBool "domain timestamp is positive" (provenanceTimestampMs capsule > 0)
  canonicalManagedPath (configManagedPaths defaultConfig) (parsedManagedRecord document) @?= Right (domainChangeNewPath update)
  semantic <- case parsedManagedRecord document of
    ManagedConnection connection -> assertRight (renderConnectionSemantic connection)
    _ -> assertFailure "expected domain connection" >> fail "unreachable"
  provenanceSemanticDigest capsule @?= semanticDigest semantic
  where
    domainsSubjectAdrFrom parsed = case parsedManagedRecord parsed of
      ManagedConnection connection -> case connectionPayload connection of
        DomainsConnection payload -> domainsSubjectAdr payload
        _ -> error "expected domains"
      _ -> error "expected connection"

assertDomainCommitMetadata :: FilePath -> DomainChangeResult -> IO ()
assertDomainCommitMetadata directory update = do
  document <- domainDocumentAt directory update
  let capsule = parsedManagedCapsule document
      commit = Text.unpack (gitOidText (domainChangeCommitOid update))
  gitText directory ["rev-parse", "HEAD"] >>= (@?= gitOidText (domainChangeCommitOid update))
  gitText directory ["show", "-s", "--format=%P", commit] >>= (@?= gitOidText (provenanceBasis capsule))
  gitText directory ["show", "-s", "--format=%s", commit] >>= (@?= "adrai: domain " <> adrIdText (domainChangeAdrId update))
  body <- gitText directory ["show", "-s", "--format=%B", commit]
  assertBool "domain commit contains exact ADR trailer" (("ADR: " <> adrIdText (domainChangeAdrId update)) `Text.isInfixOf` body)
  assertBool "domain commit contains exact Objects trailer" (("Objects: " <> connectionIdText (domainChangeConnectionId update)) `Text.isInfixOf` body)

scopeFailureSnapshot :: FilePath -> [FilePath] -> IO FailureSnapshot
scopeFailureSnapshot directory cacheAndCallerPaths = do
  repository <- assertRight =<< discoverRepository systemGit directory
  headState <- assertRight =<< repositoryHeadState repository
  let refNow = case headState of
        GitHeadAttached reference -> "attached:" <> gitRefText reference
        GitHeadDetached -> "detached"
  headNow <- gitText directory ["rev-parse", "HEAD"]
  treeNow <- gitText directory ["rev-parse", "HEAD^{tree}"]
  treeEntries <- gitText directory ["ls-tree", "-r", "HEAD"]
  rawIndex <- BS.readFile (directory </> ".git" </> "index")
  indexEntries <- gitSuccess directory ["ls-files", "-s"] BS.empty
  status <- fmap Text.lines (gitText directory ["--no-optional-locks", "status", "--porcelain=v1", "--untracked-files=all"])
  committed <- managedCommittedBytes directory
  inventory <- managedDirectoryInventory directory
  let worktree = [(path, bytes) | (path, Just bytes) <- inventory]
      allManagedPaths = sort (nub (map fst committed <> map fst worktree))
      managed = [(path, lookup path committed, lookup path worktree) | path <- allManagedPaths]
  rootEntries <- listDirectory directory
  let cachePaths = map (directory </>) (filter (isSuffixOf ".sqlite") rootEntries)
  callerFiles <- mapM snapshotFile (sort (nub (defaultCallerFiles directory <> cacheAndCallerPaths <> cachePaths)))
  disposableIndexes <- disposableIndexInventory directory
  pure FailureSnapshot
    { failureHeadRef = refNow
    , failureHeadOid = headNow
    , failureHeadTree = treeNow
    , failureTreeEntries = treeEntries
    , failureRawIndex = rawIndex
    , failureIndexEntries = indexEntries
    , failureStatus = status
    , failureManagedPaths = managed
    , failureManagedInventory = inventory
    , failureCallerFiles = callerFiles
    , failureDisposableIndexes = disposableIndexes
    }
  where
    snapshotFile path = do
      exists <- doesFileExist path
      bytes <- if exists then Just <$> BS.readFile path else pure Nothing
      pure (path, bytes)

defaultCallerFiles :: FilePath -> [FilePath]
defaultCallerFiles directory =
  [ directory </> "scope-unrelated-staged.txt"
  , directory </> "scope-unrelated-dirty.txt"
  , directory </> "scope-unrelated-untracked.txt"
  ]

-- | Inventory both directories and files so a failed operation cannot hide a
-- new managed path merely by leaving it untracked.  File entries carry their
-- exact worktree bytes; directory entries are suffixed with @/@.
managedDirectoryInventory :: FilePath -> IO [(Text.Text, Maybe BS.ByteString)]
managedDirectoryInventory directory = do
  let root = directory </> "architecture" </> "adrai"
  exists <- doesDirectoryExist root
  if exists then (("architecture/adrai/", Nothing) :) <$> walk root "architecture/adrai" else pure []
  where
    walk native relative = do
      children <- sort <$> listDirectory native
      fmap concat $ mapM (visit native relative) children
    visit native relative child = do
      let childNative = native </> child
          childRelative = relative <> "/" <> child
      directoryChild <- doesDirectoryExist childNative
      if directoryChild
        then ((Text.pack (childRelative <> "/"), Nothing) :) <$> walk childNative childRelative
        else do
          fileChild <- doesFileExist childNative
          if fileChild
            then do
              bytes <- BS.readFile childNative
              pure [(Text.pack childRelative, Just bytes)]
            else pure [(Text.pack childRelative, Nothing)]

-- | A Stage 7 rollback must not leak the transaction engine's disposable
-- temporary index.  Inventorying matching files records both absence and, if
-- present, exact bytes.
disposableIndexInventory :: FilePath -> IO [(FilePath, BS.ByteString)]
disposableIndexInventory directory = do
  let gitDirectory = directory </> ".git"
  names <- sort <$> listDirectory gitDirectory
  let candidates = [gitDirectory </> name | name <- names, "adrai-index-" `isPrefixOf` name]
  fmap concat $ mapM snapshot candidates
  where
    snapshot path = do
      exists <- doesFileExist path
      if exists then (\bytes -> [(path, bytes)]) <$> BS.readFile path else pure []

prepareScopeObservableFiles :: FilePath -> IO ()
prepareScopeObservableFiles directory = do
  _ <- commitFile directory "scope-unrelated-dirty.txt" "committed scope observable bytes\n"
  BS.writeFile (directory </> "scope-unrelated-staged.txt") "staged observable bytes\NUL"
  _ <- gitSuccess directory ["add", "scope-unrelated-staged.txt"] BS.empty
  BS.writeFile (directory </> "scope-unrelated-dirty.txt") "dirty observable bytes\NUL"
  BS.writeFile (directory </> "scope-unrelated-untracked.txt") "untracked observable bytes\NUL"

runScope :: Repository -> AdrId -> Maybe StateToken -> [ScopePattern] -> [ScopePattern] -> IO (Either TransactionError ScopeChangeResult)
runScope repository adr expected added removed = do
  inputs <- scopeInputs
  runScopeWithInputs repository adr expected added removed inputs

runScopeWithInputs :: Repository -> AdrId -> Maybe StateToken -> [ScopePattern] -> [ScopePattern] -> ProvenanceInputs -> IO (Either TransactionError ScopeChangeResult)
runScopeWithInputs repository adr expected added removed inputs = do
  actor <- createActor
  changeScopeCommand repository (configManagedPaths defaultConfig) actor adr expected "Scope change operation" (ScopeDelta added removed) inputs

runScopeSet :: Repository -> AdrId -> Maybe StateToken -> Text.Text -> [ScopePattern] -> IO (Either TransactionError ScopeChangeResult)
runScopeSet repository adr expected reason reviewed = do
  inputs <- scopeInputs
  runScopeSetWithInputs repository adr expected reason reviewed inputs

runScopeSetWithInputs :: Repository -> AdrId -> Maybe StateToken -> Text.Text -> [ScopePattern] -> ProvenanceInputs -> IO (Either TransactionError ScopeChangeResult)
runScopeSetWithInputs repository adr expected reason reviewed inputs =
  runScopeRequest repository adr expected reason (ScopeReviewedSet reviewed) inputs

runScopeRequest :: Repository -> AdrId -> Maybe StateToken -> Text.Text -> ScopeChangeRequest -> ProvenanceInputs -> IO (Either TransactionError ScopeChangeResult)
runScopeRequest repository adr expected reason request inputs = do
  actor <- createActor
  changeScopeCommand repository (configManagedPaths defaultConfig) actor adr expected reason request inputs

scopeInputs :: IO ProvenanceInputs
scopeInputs = do
  input <- assertRight (mkDigest (BS.replicate 32 1))
  prompt <- assertRight (mkDigest (BS.replicate 32 2))
  context <- assertRight (mkDigest (BS.replicate 32 3))
  pure (ProvenanceInputs (Just input) (Just prompt) (Just context))

scopeDocumentAt :: FilePath -> ScopeChangeResult -> IO ParsedManagedDocument
scopeDocumentAt directory update = do
  bytes <- BS.readFile (directory </> Text.unpack (repoPathText (scopeChangeNewPath update)))
  assertRight (parseManagedDocument (scopeChangeNewPath update) bytes)

connectionPayloadFrom :: ParsedManagedDocument -> Maybe AppliesToPayload
connectionPayloadFrom document =
  case parsedManagedRecord document of
    ManagedConnection connection -> case connectionPayload connection of
      AppliesToConnection payload -> Just payload
      _ -> Nothing
    _ -> Nothing

assertScopeDocument :: ScopeChangeResult -> ConnectionId -> Text.Text -> [ScopePattern] -> [ScopePattern] -> [ScopePattern] -> ParsedManagedDocument -> IO ()
assertScopeDocument update parent = assertScopeDocumentWithParents update [parent]

assertScopeDocumentWithParents :: ScopeChangeResult -> [ConnectionId] -> Text.Text -> [ScopePattern] -> [ScopePattern] -> [ScopePattern] -> ParsedManagedDocument -> IO ()
assertScopeDocumentWithParents update parents change added removed effective document = do
  case connectionPayloadFrom document of
    Just payload -> do
      appliesToParentConnections payload @?= parents
      appliesToChange payload @?= change
      appliesToAdded payload @?= added
      appliesToRemoved payload @?= removed
      appliesToEffective payload @?= effective
    Nothing -> assertFailure "expected parsed scope payload"
  let capsule = parsedManagedCapsule document
  provenanceParents capsule @?= map ProvenanceConnection parents
  eventKindText (provenanceEventKind capsule) @?= "scope." <> change
  provenanceObjectId capsule @?= ProvenanceConnection (scopeChangeConnectionId update)

committedManagedDocuments :: FilePath -> IO [ParsedManagedDocument]
committedManagedDocuments directory = do
  managed <- managedCommittedBytes directory
  traverse parse managed
  where
    parse (path, bytes) = do
      repoPath <- assertRight (mkRepoPath path)
      assertRight (parseManagedDocument repoPath bytes)

currentReducedAdr :: AdrId -> [ParsedManagedDocument] -> IO ReducedAdr
currentReducedAdr adr documents =
  case lookupReducedAdr adr (reduceManagedGraph (map parsedManagedRecord documents)) of
    Just reduced -> pure reduced
    Nothing -> assertFailure "expected reduced ADR" >> fail "unreachable"

managedCommittedBytes :: FilePath -> IO [(Text.Text, BS.ByteString)]
managedCommittedBytes directory = do
  paths <- managedCommittedPaths directory
  mapM (\path -> do
    bytes <- gitSuccess directory ["show", Text.unpack ("HEAD:" <> path)] BS.empty
    pure (path, bytes)) paths

managedCommittedPaths :: FilePath -> IO [Text.Text]
managedCommittedPaths directory =
  fmap Text.lines (gitText directory ["ls-tree", "-r", "--name-only", "HEAD", "--", "architecture/adrai"])

commitStatusBranch :: FilePath -> CreateResult -> Text.Text -> Text.Text -> StatusState -> IO ConnectionId
commitStatusBranch directory created connectionText operationText state = do
  actor <- createActor
  connectionId <- assertRight (mkConnectionId connectionText)
  operationId <- assertRight (mkOperationId operationText)
  let statusRecord = ConnectionRecord
        { connectionRecordId = connectionId
        , connectionPayload = StatusConnection StatusPayload
            { statusSubjectAdr = createAdrId created
            , statusParentConnections = [createStatusId created]
            , statusState = state
            , statusRecordHeads = [createRecordId created]
            , statusReplacementAdr = Nothing
            }
        , connectionRationale = "Test status branch.\n"
        }
      managed = ManagedConnection statusRecord
  semantic <- assertRight (renderConnectionSemantic statusRecord)
  eventKind <- assertRight (mkEventKind (if state == StatusObsolete then "decision.obsolete" else "decision.reactivate"))
  basis <- GitOid <$> gitText directory ["rev-parse", "HEAD"]
  capsule <- assertRight (mkProvenanceCapsule ProvenanceCapsuleInput
    { capsuleInputOperationId = operationId
    , capsuleInputObjectId = ProvenanceConnection connectionId
    , capsuleInputEventKind = eventKind
    , capsuleInputActor = actor
    , capsuleInputTimestampMs = 1700000000000
    , capsuleInputBasis = basis
    , capsuleInputParents = [ProvenanceConnection (createStatusId created), ProvenanceRecord (createRecordId created)]
    , capsuleInputBranchHint = Just "main"
    , capsuleInputUpstreamHint = Nothing
    , capsuleInputLineAnchors = []
    , capsuleInputSemanticDigest = semanticDigest semantic
    , capsuleInputToolVersion = "adrai/1.0.0"
    , capsuleInputDigests = ProvenanceInputs Nothing Nothing Nothing
    })
  bytes <- assertRight (sealManagedDocument managed capsule)
  path <- assertRight (canonicalManagedPath (configManagedPaths defaultConfig) managed)
  _ <- commitFile directory (Text.unpack (repoPathText path)) bytes
  pure connectionId

commitScopeExpansion :: FilePath -> CreateResult -> Text.Text -> Text.Text -> Text.Text -> IO Text.Text
commitScopeExpansion directory created connectionText operationText scopeText = do
  actor <- createActor
  connectionId <- assertRight (mkConnectionId connectionText)
  operationId <- assertRight (mkOperationId operationText)
  scope <- assertRight (mkScopePattern scopeText)
  source <- assertRight (mkScopePattern "src/**")
  let scopeRecord =
        ConnectionRecord
          { connectionRecordId = connectionId,
            connectionPayload =
              AppliesToConnection
                AppliesToPayload
                  { appliesToSubjectAdr = createAdrId created,
                    appliesToParentConnections = [createScopeId created],
                    appliesToChange = "expand",
                    appliesToAdded = [scope],
                    appliesToRemoved = [],
                    appliesToEffective = sort [source, scope]
                  },
            connectionRationale = "Scope expansion.\n"
          }
      managed = ManagedConnection scopeRecord
  semantic <- assertRight (renderConnectionSemantic scopeRecord)
  eventKind <- assertRight (mkEventKind "scope.expand")
  basis <- GitOid <$> gitText directory ["rev-parse", "HEAD"]
  capsule <-
    assertRight
      ( mkProvenanceCapsule
          ProvenanceCapsuleInput
            { capsuleInputOperationId = operationId,
              capsuleInputObjectId = ProvenanceConnection connectionId,
              capsuleInputEventKind = eventKind,
              capsuleInputActor = actor,
              capsuleInputTimestampMs = 1700000000000,
              capsuleInputBasis = basis,
              capsuleInputParents = [ProvenanceConnection (createScopeId created)],
              capsuleInputBranchHint = Just "main",
              capsuleInputUpstreamHint = Nothing,
              capsuleInputLineAnchors = [],
              capsuleInputSemanticDigest = semanticDigest semantic,
              capsuleInputToolVersion = "adrai/1.0.0",
              capsuleInputDigests = ProvenanceInputs Nothing Nothing Nothing
            }
      )
  bytes <- assertRight (sealManagedDocument managed capsule)
  path <- assertRight (canonicalManagedPath (configManagedPaths defaultConfig) managed)
  commitFile directory (Text.unpack (repoPathText path)) bytes

commitDomainExpansion :: FilePath -> CreateResult -> Text.Text -> Text.Text -> Text.Text -> IO Text.Text
commitDomainExpansion directory created connectionText operationText domainTextValue = do
  actor <- createActor
  connectionId <- assertRight (mkConnectionId connectionText)
  operationId <- assertRight (mkOperationId operationText)
  domain <- assertRight (mkDomain domainTextValue)
  compiler <- assertRight (mkDomain "compiler")
  let domainRecord =
        ConnectionRecord
          { connectionRecordId = connectionId,
            connectionPayload =
              DomainsConnection
                DomainsPayload
                  { domainsSubjectAdr = createAdrId created,
                    domainsParentConnections = [createDomainId created],
                    domainsChange = "expand",
                    domainsAdded = [domain],
                    domainsRemoved = [],
                    domainsEffective = sort [compiler, domain],
                    domainsRefinements = []
                  },
            connectionRationale = "Domain expansion.\n"
          }
      managed = ManagedConnection domainRecord
  semantic <- assertRight (renderConnectionSemantic domainRecord)
  eventKind <- assertRight (mkEventKind "domain.update")
  basis <- GitOid <$> gitText directory ["rev-parse", "HEAD"]
  capsule <-
    assertRight
      ( mkProvenanceCapsule
          ProvenanceCapsuleInput
            { capsuleInputOperationId = operationId,
              capsuleInputObjectId = ProvenanceConnection connectionId,
              capsuleInputEventKind = eventKind,
              capsuleInputActor = actor,
              capsuleInputTimestampMs = 1700000000000,
              capsuleInputBasis = basis,
              capsuleInputParents = [ProvenanceConnection (createDomainId created)],
              capsuleInputBranchHint = Just "main",
              capsuleInputUpstreamHint = Nothing,
              capsuleInputLineAnchors = [],
              capsuleInputSemanticDigest = semanticDigest semantic,
              capsuleInputToolVersion = "adrai/1.0.0",
              capsuleInputDigests = ProvenanceInputs Nothing Nothing Nothing
            }
      )
  bytes <- assertRight (sealManagedDocument managed capsule)
  path <- assertRight (canonicalManagedPath (configManagedPaths defaultConfig) managed)
  commitFile directory (Text.unpack (repoPathText path)) bytes

runAmend :: Repository -> AdrId -> RecordId -> Text.Text -> Text.Text -> Text.Text -> IO (Either TransactionError AmendResult)
runAmend repository adr sourceRecord title summary body = do
  actor <- createActor
  amendAdmCommand
    repository
    (configManagedPaths defaultConfig)
    actor
    adr
    sourceRecord
    title
    summary
    body
    (ProvenanceInputs Nothing Nothing Nothing)

runCurrentAmend :: Repository -> AdrId -> Maybe StateToken -> Text.Text -> Text.Text -> Text.Text -> Text.Text -> IO (Either TransactionError AmendResult)
runCurrentAmend repository adr expected changeSummary title summary body = do
  actor <- createActor
  amendCurrentAdrCommand
    repository
    actor
    adr
    expected
    changeSummary
    title
    summary
    body
    (ProvenanceInputs Nothing Nothing Nothing)

data PostCommitRepositorySeed = PostCommitRepositorySeed
  { postCommitRepositorySeed :: RepositorySeed,
    postCommitSeedRepository :: Repository,
    postCommitSeedCommitOid :: GitOid,
    postCommitPreparedResult :: ColdCompilerResult
  }

postCommitPreparedDatabaseName :: FilePath
postCommitPreparedDatabaseName = "retained-post-commit.sqlite"

createPostCommitRepositorySeed :: IO PostCommitRepositorySeed
createPostCommitRepositorySeed = do
  (seed, (repository, commitOid, preparedResult)) <-
    createRepositorySeedWith "adrai-post-commit-seed" $ \directory -> do
      initTestRepository directory
      _ <- gitSuccess directory ["config", "core.hooksPath", ".git/adrai-no-hooks"] BS.empty
      repository <- assertRight =<< discoverRepository systemGit directory
      initialized <- assertRight =<< initCommand repository
      let commitOid = initCommitOid initialized
      revision <- assertRight =<< resolveRepositoryRevision repository (RevisionSpec (gitOidText commitOid))
      bracket (open (directory </> postCommitPreparedDatabaseName)) close $ \connection -> do
        preparedResult <- assertRight =<< coldCompileRepository connection revision
        pure (repository, commitOid, preparedResult)
  verifyLocalRepositorySeed seed repository commitOid (Just postCommitPreparedDatabaseName)
    `onException` removeRepositorySeed seed
  pure
    PostCommitRepositorySeed
      { postCommitRepositorySeed = seed,
        postCommitSeedRepository = repository,
        postCommitSeedCommitOid = commitOid,
        postCommitPreparedResult = preparedResult
      }

removePostCommitRepositorySeed :: PostCommitRepositorySeed -> IO ()
removePostCommitRepositorySeed = removeRepositorySeed . postCommitRepositorySeed

data MutationRepositorySeed = MutationRepositorySeed
  { mutationRepositorySeed :: RepositorySeed,
    mutationSeedRepository :: Repository,
    mutationRepositoryCreateResult :: CreateResult
  }

createMutationRepositorySeed :: IO MutationRepositorySeed
createMutationRepositorySeed = do
  (seed, (repository, created)) <-
    createRepositorySeedWith "adrai-mutation-seed" $ \directory -> do
      initTestRepository directory
      _ <- gitSuccess directory ["config", "core.hooksPath", ".git/adrai-no-hooks"] BS.empty
      _ <- commitFile directory "seed.txt" "seed\n"
      repository <- assertRight =<< discoverRepository systemGit directory
      created <- assertRight =<< runCreate repository
      pure (repository, created)
  verifyLocalRepositorySeed seed repository (createCommitOid created) Nothing
    `onException` removeRepositorySeed seed
  pure
    MutationRepositorySeed
      { mutationRepositorySeed = seed,
        mutationSeedRepository = repository,
        mutationRepositoryCreateResult = created
      }

removeMutationRepositorySeed :: MutationRepositorySeed -> IO ()
removeMutationRepositorySeed = removeRepositorySeed . mutationRepositorySeed

withMutationRepositoryCopy ::
  IO MutationRepositorySeed ->
  String ->
  (FilePath -> Repository -> CreateResult -> IO value) ->
  IO value
withMutationRepositoryCopy getSeed label action = do
  seed <- getSeed
  withRepositorySeedCopy (mutationRepositorySeed seed) label $ \_ directory -> do
    repository <- assertRight (relocateLocalMainWorktree directory (mutationSeedRepository seed))
    action directory repository (mutationRepositoryCreateResult seed)

withPostCommitRepositoryCopy ::
  IO PostCommitRepositorySeed ->
  String ->
  (FilePath -> FilePath -> Repository -> GitOid -> PostCommitIndexDependencies -> IO value) ->
  IO value
withPostCommitRepositoryCopy getSeed label action = do
  seed <- getSeed
  withRepositorySeedCopy (postCommitRepositorySeed seed) label $ \temporary directory -> do
    repository <- assertRight (relocateLocalMainWorktree directory (postCommitSeedRepository seed))
    let preparedDatabase = directory </> postCommitPreparedDatabaseName
        base = postCommitIndexDependencies
        preparedDependencies =
          base
            { postCommitOpenDatabase = \candidate -> do
                copyFile preparedDatabase candidate
                postCommitOpenDatabase base candidate,
              postCommitColdCompile = \_ requestedRevision ->
                pure
                  ( Right
                      ( (postCommitPreparedResult seed)
                          { coldCompilerCompiledRevision = requestedRevision
                          }
                      )
                  )
            }
    action temporary directory repository (postCommitSeedCommitOid seed) preparedDependencies

-- The retained seed helper makes byte-for-byte private copies of one ordinary
-- main worktree.  These local checks establish that narrow layout once for
-- every acquired Tasty resource, then every leaf can relocate the immutable
-- discovered identity without another native discovery/HEAD probe.
verifyLocalRepositorySeed :: RepositorySeed -> Repository -> GitOid -> Maybe FilePath -> IO ()
verifyLocalRepositorySeed seed seedRepository requestedOid maybeDatabaseName = do
  seedRoot <- assertSeedMainWorktree seedRepository
  withRepositorySeedCopy seed "adrai repository identity audit" $ \_ firstCopy ->
    withRepositorySeedCopy seed "adrai repository isolation audit" $ \_ secondCopy -> do
      relocated <- assertRight (relocateLocalMainWorktree firstCopy seedRepository)
      discovered <- assertRight =<< discoverRepository systemGit firstCopy
      assertRepositoryEquivalent relocated discovered
      gitText firstCopy ["rev-parse", "HEAD"] >>= (@?= gitOidText requestedOid)
      assertPrivateRepositoryCopy seedRoot firstCopy relocated
      assertCopiedDatabaseIsolation seedRoot firstCopy secondCopy maybeDatabaseName

      let isolationProbe = "adrai-private-copy-isolation.probe"
      BS.writeFile (firstCopy </> isolationProbe) "private copy only\n"
      doesFileExist (firstCopy </> isolationProbe) >>= assertBool "private isolation probe was not written"
      doesFileExist (seedRoot </> isolationProbe) >>= assertBool "private copy changed the immutable seed" . not
      doesFileExist (secondCopy </> isolationProbe) >>= assertBool "private copy changed an independent copy" . not

assertSeedMainWorktree :: Repository -> IO FilePath
assertSeedMainWorktree = assertRight . validateLocalMainWorktree

validateLocalMainWorktree :: Repository -> Either String FilePath
validateLocalMainWorktree repository =
  case repositoryWorktreeRoot repository of
    Nothing -> Left "repository seed has no worktree root"
    Just source
      | isRelative source -> Left "repository seed worktree root is not absolute"
      | repositoryClient repository /= systemGit -> Left "repository seed does not use the real system Git client"
      | repositoryLayout repository /= MainWorktree -> Left "repository seed is not an ordinary main worktree"
      | repositoryCommonIsBare repository -> Left "repository seed unexpectedly has bare common storage"
      | not (equalFilePath (repositoryGitDir repository) (source </> ".git")) -> Left "repository seed Git directory is outside the ordinary main-worktree layout"
      | not (equalFilePath (repositoryCommonDir repository) (source </> ".git")) -> Left "repository seed common directory is outside the ordinary main-worktree layout"
      | not (equalFilePath (repositoryCommandDirectory repository) source) -> Left "repository seed command directory differs from its worktree root"
      | otherwise -> Right source

relocateLocalMainWorktree :: FilePath -> Repository -> Either String Repository
relocateLocalMainWorktree destination repository = do
  source <- validateLocalMainWorktree repository
  if isRelative destination
    then Left "private repository destination is not absolute"
    else
      if equalFilePath destination source
        then Left "private repository destination aliases the immutable seed"
        else
          Right
            repository
              { repositoryWorktreeRoot = Just destination,
                repositoryGitDir = destination </> ".git",
                repositoryCommonDir = destination </> ".git",
                repositoryCommandDirectory = destination
              }

assertRepositoryEquivalent :: Repository -> Repository -> IO ()
assertRepositoryEquivalent relocated discovered = do
  repositoryClient discovered @?= repositoryClient relocated
  repositoryLayout discovered @?= repositoryLayout relocated
  repositoryCommonIsBare discovered @?= repositoryCommonIsBare relocated
  assertMaybePathEqual "worktree root" (repositoryWorktreeRoot relocated) (repositoryWorktreeRoot discovered)
  assertPathEqual "Git directory" (repositoryGitDir relocated) (repositoryGitDir discovered)
  assertPathEqual "common directory" (repositoryCommonDir relocated) (repositoryCommonDir discovered)
  assertPathEqual "command directory" (repositoryCommandDirectory relocated) (repositoryCommandDirectory discovered)

assertMaybePathEqual :: String -> Maybe FilePath -> Maybe FilePath -> IO ()
assertMaybePathEqual label expected actual =
  case (expected, actual) of
    (Just expectedPath, Just actualPath) -> assertPathEqual label expectedPath actualPath
    _ -> assertBool (label <> " presence differs") (expected == actual)

assertPathEqual :: String -> FilePath -> FilePath -> IO ()
assertPathEqual label expected actual =
  assertBool (label <> " differs: expected " <> expected <> ", got " <> actual) (equalFilePath expected actual)

assertPrivateRepositoryCopy :: FilePath -> FilePath -> Repository -> IO ()
assertPrivateRepositoryCopy seedRoot privateRoot repository = do
  let paths =
        maybe [] pure (repositoryWorktreeRoot repository)
          <> [repositoryGitDir repository, repositoryCommonDir repository, repositoryCommandDirectory repository]
  assertBool "relocated Repository escaped its private copy" (all (isPathWithin privateRoot) paths)
  assertBool "relocated Repository retained a seed path" (all (not . isPathWithin seedRoot) paths)

  configBytes <- BS.readFile (repositoryGitDir repository </> "config")
  let normalSeed = normalise seedRoot
      seedForms = nub [BS8.pack normalSeed, BS8.pack (map slash normalSeed)]
  assertBool "private Git config retained the seed's absolute path" (all (not . (`BS.isInfixOf` configBytes)) seedForms)
  assertBool "private Git config omitted its repository-relative hooks path" (BS8.pack ".git/adrai-no-hooks" `BS.isInfixOf` configBytes)
  doesDirectoryExist (repositoryCommonDir repository </> "adrai-no-hooks") >>= assertBool "private Git hooks directory was not copied"
  doesFileExist (repositoryCommonDir repository </> "objects" </> "info" </> "alternates") >>= assertBool "private Git objects unexpectedly share an alternates store" . not
  where
    slash '\\' = '/'
    slash character = character

isPathWithin :: FilePath -> FilePath -> Bool
isPathWithin root path =
  let relative = makeRelative root path
   in isRelative relative
        && case splitDirectories relative of
          ".." : _ -> False
          _ -> True

assertCopiedDatabaseIsolation :: FilePath -> FilePath -> FilePath -> Maybe FilePath -> IO ()
assertCopiedDatabaseIsolation _ _ _ Nothing = pure ()
assertCopiedDatabaseIsolation seedRoot firstCopy secondCopy (Just databaseName) = do
  seedBytes <- BS.readFile (seedRoot </> databaseName)
  firstBytes <- BS.readFile (firstCopy </> databaseName)
  secondBytes <- BS.readFile (secondCopy </> databaseName)
  firstBytes @?= seedBytes
  secondBytes @?= seedBytes

runCreate :: Repository -> IO (Either TransactionError CreateResult)
runCreate repository = do
  actor <- createActor
  adr <- assertRight (mkAdrId "A00000000000000000000000001")
  record <- assertRight (mkRecordId "R00000000000000000000000001")
  domain <- assertRight (mkDomain "compiler")
  scope <- assertRight (mkScopePattern "src/**")
  createAdrCommand
    repository
    (configManagedPaths defaultConfig)
    actor
    adr
    record
    "Create transaction"
    "Create commits the complete canonical document set."
    "## Decision\nUse one append-only transaction.\n"
    [domain]
    [scope]
    Nothing
    Nothing
    Nothing

createActor :: IO Actor
createActor = assertRight (mkActor HumanActor "mutation-service-test" Nothing)

posixTimeMs :: IO Integer
posixTimeMs = floor . (* 1000) <$> getPOSIXTime

gitText :: FilePath -> [String] -> IO Text.Text
gitText directory arguments = outputText <$> gitSuccess directory arguments BS.empty

assertRight :: Show problem => Either problem value -> IO value
assertRight value =
  case value of
    Left problem -> assertFailure (show problem) >> fail "unreachable"
    Right result -> pure result
