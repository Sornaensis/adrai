{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Adrai.MutationServiceTest (tests) where

import Adrai.Domain (Domain, DomainRefinement, mkDomain, mkDomainRefinement)
import Adrai.Format.Document
  ( AppliesToPayload (..),
    AmendsPayload (..),
    ConnectionPayload (..),
    ConnectionRecord (..),
    DecisionRecord (..),
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
    Repository,
    discoverRepository,
    gitOidText,
    repositoryHeadState,
    systemGit,
  )
import Adrai.GitTestSupport
  ( commitFile,
    commitFiles,
    gitSuccess,
    initTestRepository,
    outputText,
  )
import Adrai.Fixture.CompilerRepository (healthyCompilerFiles)
import Adrai.Graph
  ( AxisResolution (..),
    GraphAxis (DomainAxis, ScopeAxis),
    ReducedAdr (..),
    lookupReducedAdr,
    reduceManagedGraph,
    reducedStateToken,
  )
import Adrai.Provenance
  ( ProvenanceCapsule,
    ProvenanceCapsuleInput (..),
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
    ReactivateRequest (..),
    ReactivateResult (..),
    InitResult (..),
    amendAdmCommand,
    changeDomainCommand,
    changeScopeCommand,
    createAdrCommand,
    initCommand,
    obsoleteCommand,
    reactivateCommand,
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
import qualified Data.Text as Text
import Data.Either (isLeft)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, nub, sort, zip5, zip7)
import qualified Data.Set as Set
import Control.Exception (AsyncException (ThreadKilled), SomeException, bracket, throwIO, try)
import Database.SQLite.Simple (Only (..), close, open, query_)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeFile, renameFile)
import System.FilePath ((</>), takeFileName)
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup "mutation service results"
    [ testGroup
        "P6-02A mutation service results"
        [ testCase "init commits bootstrap files and returns transaction result" initCommitsBootstrapFiles
    , testCase "init transaction failure returns no success result" initFailureReturnsTransactionError
    , testCase "post-commit indexing replaces a same-path projection with the exact latest commit" postCommitIndexProjectsExactCommit
    , testCase "post-commit indexing projects actual compiler warnings in stable order" postCommitIndexProjectsCompilerWarnings
    , testCase "post-commit index failure preserves the committed HEAD" postCommitIndexFailurePreservesCommit
    , testCase "post-commit index exposes representative open compile and close failures" postCommitIndexFailureBoundaries
    , testCase "post-commit async compile cancellation finalizes and cleans candidate" postCommitIndexAsyncCompileCancellationFinalizesCandidate
    , testCase "post-commit index failure retains prior target and cleans disposable candidate" postCommitIndexFailureRetainsPriorIndexAndCleansCandidate
    , testCase "post-commit publication failure retains exact target and cleans candidate" postCommitIndexPublishFailureIsAtomic
    , testCase "post-commit reconciliation probe failure fails stop with recovery artifacts" postCommitIndexPublishProbeFailureFailStop
    , testCase "post-commit impossible publication state fails stop and retains candidate" postCommitIndexPublishInvariantFailStop
    , testCase "post-commit temp close failure retries close before ordered cleanup" postCommitIndexTemporaryCloseFailureCleansCandidate
    , testCase "post-commit cleanup failures are typed without skipping later siblings" postCommitIndexCleanupFailureIsOrdered
    , testCase "post-commit database close failure keeps its live handle and cleanup failure observable" postCommitIndexLiveCloseFailureIsObservable
    , testCase "create commits four sealed records and returns transaction result" createCommitsExactlyFourRecords
    , testCase "create failure leaves HEAD and unrelated staged index entry unchanged" createFailurePreservesRepositoryState
    , testCase "amend commits a truthful append-only decision and amendment edge" amendCommitsTruthfulAppendOnlyOperation
    , testCase "amend rejections do not create a commit" amendRejectionsDoNotCommit
    , testCase "successive amendments chain from the prior current head" successiveAmendmentsChainFromPriorHead
    , testCase "amend rejects inactive, conflicted, and misplaced committed sources" amendRejectsInvalidCommittedState
        , testCase "amend transaction failure is retry-safe before generated files exist" amendFailurePropagatesTransactionError
        ]
    , testGroup
        "P6-02C scope mutation service"
    [ testCase "scope updates expand, contract, and mix from the current scope head" scopeUpdatesAreTruthful
        , testCase "scope reviewed set replaces one current scope head truthfully" scopeReviewedSetReplacesOneHead
        , testCase "scope reviewed set merges two current scope heads and clears only scope conflict" scopeReviewedSetMergesScopeConflict
        , testCase "scope reviewed merge resolves two heads even when the reviewed set equals their union" scopeReviewedSetMergesUnchangedUnion
        , testCase "scope rejections preserve the complete caller-visible repository state" scopeRejectionsPreserveRepository
        , testCase "scope rejects inactive ADRs with the exact state error" rejectInactiveScope
        , testCase "scope rejects conflicted heads with the exact state error" rejectConflictedScope
        , testCase "scope reviewed set rejects a conflict outside the scope axis without state changes" rejectNonScopeConflict
        , testCase "scope rejects detached HEAD before generating files" scopeDetachedHeadRejected
        , testCase "scope transaction failure after generation preserves every caller-owned state" scopeTransactionFailurePreservesEverything
        ]
    , testGroup
        "P6-02D domain mutation service"
        [ testCase "domain updates expand contract mixed refine replace and clear truthfully" domainUpdatesAreTruthful
        , testCase "domain rejects invalid requests without changing committed state" domainRejectionsPreserveRepository
        , testCase "domain reviewed set reconciles a domain-only two-head conflict" domainReviewedMergeReconcilesConflict
        , testCase "domain reviewed merge commits an exact two-head union without changing other axes" domainReviewedMergeExactUnionPreservesOtherAxes
        , testCase "domain rejects a real scope-axis conflict without state change" domainRejectsNonDomainConflict
        , testCase "domain rejects a committed ADR with no current domain head" domainMissingCurrentHeadRejected
        , testCase "domain stale unknown detached and inactive authority failures preserve state" domainAuthorityFailuresPreserveRepository
        , testCase "domain induced post-generation transaction failure preserves every observable state" domainTransactionFailurePreservesEverything
        ]
    , testGroup
        "P6-02E status mutation service"
        [ testCase "obsolete and reactivate use current heads, truthful provenance, and state tokens" statusTransitionsAreTruthful
        , testCase "status rejections preserve the complete caller-visible repository state" statusRejectionsPreserveRepository
        , testCase "status resolve merges every current status head and rejects unapproved conflicts" statusResolveMergesConflict
        , testCase "status transaction failure preserves every observable state" statusTransactionFailurePreservesEverything
        , testCase "status authority, replacement, and compatibility rejections preserve state" statusAuthorityMatrixPreservesRepository
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

data StatusUpdate = StatusUpdate
  { statusUpdateOperationId :: String,
    statusUpdateAdrId :: AdrId,
    statusUpdateConnectionId :: ConnectionId,
    statusUpdateParents :: [ConnectionId],
    statusUpdateRecords :: [RecordId],
    statusUpdateReplacement :: Maybe AdrId,
    statusUpdateResolved :: Bool,
    statusUpdateCommit :: GitOid,
    statusUpdatePath :: RepoPath,
    statusUpdatePaths :: [RepoPath],
    statusUpdateIndexUpdated :: Bool
  }
  deriving (Eq, Show)

initCommitsBootstrapFiles :: IO ()
initCommitsBootstrapFiles =
  withSystemTempDirectory "adrai init service" $ \temporary -> do
    let directory = temporary </> "repository"
    initTestRepository directory
    repository <- assertRight =<< discoverRepository systemGit directory
    initialized <- assertRight =<< initCommand repository
    headAfter <- gitText directory ["rev-parse", "HEAD"]
    initInitialized initialized @?= True
    initOperationId initialized @?= "init"
    headAfter @?= gitOidText (initCommitOid initialized)
    sort (map repoPathText (initCreatedPaths initialized)) @?= [".adrai.toml", ".gitattributes", ".gitignore"]
    assertBool "bootstrap transaction reports its own index refresh result" (initIndexUpdated initialized)

initFailureReturnsTransactionError :: IO ()
initFailureReturnsTransactionError =
  withCreateRepository $ \directory repository oldHead -> do
    _ <- gitSuccess directory ["checkout", "--detach"] BS.empty
    result <- initCommand repository
    assertBool "detached bootstrap transaction must not produce InitResult" (isLeft result)
    gitText directory ["rev-parse", "HEAD"] >>= (@?= oldHead)

postCommitIndexProjectsExactCommit :: IO ()
postCommitIndexProjectsExactCommit =
  withSystemTempDirectory "adrai post-commit index" $ \temporary -> do
    let directory = temporary </> "repository"
        database = temporary </> "index.sqlite"
    initTestRepository directory
    repository <- assertRight =<< discoverRepository systemGit directory
    initialized <- assertRight =<< initCommand repository
    assertIndexed repository (initCommitOid initialized) database
    created <- assertRight =<< runCreate repository
    assertIndexed repository (createCommitOid created) database

postCommitIndexFailurePreservesCommit :: IO ()
postCommitIndexFailurePreservesCommit =
  withSystemTempDirectory "adrai post-commit index failure" $ \temporary -> do
    let directory = temporary </> "repository"
        database = temporary </> "failed.sqlite"
    initTestRepository directory
    repository <- assertRight =<< discoverRepository systemGit directory
    initialized <- assertRight =<< initCommand repository
    let missing = GitOid "0000000000000000000000000000000000000000"
    result <- compilePostCommitIndex repository missing database
    postCommitIndexed result @?= False
    postCommitDatabase result @?= Nothing
    postCommitIndexRevision result @?= Nothing
    postCommitIndexWarnings result @?= []
    case postCommitIndexError result of
      Just (PostCommitIndexResolveFailure _) -> pure ()
      other -> assertFailure ("expected actual resolve failure, got " <> show other)
    gitText directory ["rev-parse", "HEAD"] >>= (@?= gitOidText (initCommitOid initialized))

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

postCommitIndexFailureBoundaries :: IO ()
postCommitIndexFailureBoundaries =
  withSystemTempDirectory "adrai post-commit injected failures" $ \temporary -> do
    let directory = temporary </> "repository"
        database = temporary </> "failure.sqlite"
    initTestRepository directory
    repository <- assertRight =<< discoverRepository systemGit directory
    initialized <- assertRight =<< initCommand repository
    let commitOid = initCommitOid initialized
        openFailure = postCommitIndexDependencies {postCommitOpenDatabase = \_ -> throwIO (userError "open failure")}
        compileFailure = postCommitIndexDependencies {postCommitColdCompile = \_ _ -> throwIO (userError "compile failure")}
        closeFailure = postCommitIndexDependencies {postCommitCloseDatabase = \connection -> close connection >> throwIO (userError "close failure")}
        combinedFailure =
          postCommitIndexDependencies
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

postCommitIndexAsyncCompileCancellationFinalizesCandidate :: IO ()
postCommitIndexAsyncCompileCancellationFinalizesCandidate =
  withSystemTempDirectory "adrai post-commit async compile cancellation" $ \temporary -> do
    let directory = temporary </> "repository"
        database = temporary </> "index.sqlite"
        priorBytes = "prior index survives cancellation exactly"
        base = postCommitIndexDependencies
    initTestRepository directory
    repository <- assertRight =<< discoverRepository systemGit directory
    initialized <- assertRight =<< initCommand repository
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
    outcome <- try (compilePostCommitIndexWith asyncCompile repository (initCommitOid initialized) database) :: IO (Either AsyncException PostCommitIndexResult)
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

postCommitIndexFailureRetainsPriorIndexAndCleansCandidate :: IO ()
postCommitIndexFailureRetainsPriorIndexAndCleansCandidate =
  withSystemTempDirectory "adrai post-commit index replacement failure" $ \temporary -> do
    let directory = temporary </> "repository"
        database = temporary </> "index.sqlite"
        candidatePrefix = takeFileName database <> ".post-commit-"
    initTestRepository directory
    repository <- assertRight =<< discoverRepository systemGit directory
    initialized <- assertRight =<< initCommand repository
    assertIndexed repository (initCommitOid initialized) database
    created <- assertRight =<< runCreate repository
    let compileFailure =
          postCommitIndexDependencies
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
      resolved @?= [Only (gitOidText (initCommitOid initialized))]
    siblings <- listDirectory temporary
    assertBool "failed refresh left an owned SQLite candidate behind" (not (any (candidatePrefix `isPrefixOf`) siblings))

postCommitIndexPublishFailureIsAtomic :: IO ()
postCommitIndexPublishFailureIsAtomic =
  withSystemTempDirectory "adrai post-commit publish failure" $ \temporary -> do
    let directory = temporary </> "repository"
        database = temporary </> "index.sqlite"
        priorBytes = "prior index bytes\NULremain exact"
        base = postCommitIndexDependencies
    initTestRepository directory
    repository <- assertRight =<< discoverRepository systemGit directory
    initialized <- assertRight =<< initCommand repository
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
    result <- compilePostCommitIndexWith publishFailure repository (initCommitOid initialized) database
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

postCommitIndexPublishProbeFailureFailStop :: IO ()
postCommitIndexPublishProbeFailureFailStop =
  withSystemTempDirectory "adrai post-commit reconciliation probe" $ \temporary -> do
    let directory = temporary </> "repository"
        database = temporary </> "index.sqlite"
        priorBytes = "prior bytes retained at recovery backup"
        base = postCommitIndexDependencies
    initTestRepository directory
    repository <- assertRight =<< discoverRepository systemGit directory
    initialized <- assertRight =<< initCommand repository
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
    result <- compilePostCommitIndexWith probeFailure repository (initCommitOid initialized) database
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

postCommitIndexPublishInvariantFailStop :: IO ()
postCommitIndexPublishInvariantFailStop =
  withSystemTempDirectory "adrai post-commit publish invariant" $ \temporary -> do
    let directory = temporary </> "repository"
        database = temporary </> "index.sqlite"
        base = postCommitIndexDependencies
    initTestRepository directory
    repository <- assertRight =<< discoverRepository systemGit directory
    initialized <- assertRight =<< initCommand repository
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
    result <- compilePostCommitIndexWith invariantFailure repository (initCommitOid initialized) database
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

postCommitIndexTemporaryCloseFailureCleansCandidate :: IO ()
postCommitIndexTemporaryCloseFailureCleansCandidate =
  withSystemTempDirectory "adrai post-commit temp close failure" $ \temporary -> do
    let directory = temporary </> "repository"
        database = temporary </> "index.sqlite"
        priorBytes = "prior index survives temp close failure"
    initTestRepository directory
    repository <- assertRight =<< discoverRepository systemGit directory
    initialized <- assertRight =<< initCommand repository
    BS.writeFile database priorBytes
    closeCalls <- newIORef (0 :: Int)
    candidateRef <- newIORef Nothing
    let base = postCommitIndexDependencies
        openTracked directoryPath template = do
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
    result <- compilePostCommitIndexWith closeFailure repository (initCommitOid initialized) database
    case postCommitIndexError result of
      Just (PostCommitIndexTemporaryCloseFailure _) -> pure ()
      other -> assertFailure ("expected temp close failure, got " <> show other)
    readIORef closeCalls >>= (@?= 2)
    BS.readFile database >>= (@?= priorBytes)
    candidate <- readIORef candidateRef >>= maybe (assertFailure "missing captured candidate") pure
    assertOwnedPathsExist candidate [False, False, False, False]

postCommitIndexCleanupFailureIsOrdered :: IO ()
postCommitIndexCleanupFailureIsOrdered =
  withSystemTempDirectory "adrai post-commit cleanup ordering" $ \temporary -> do
    let directory = temporary </> "repository"
        database = temporary </> "index.sqlite"
        priorBytes = "prior index survives cleanup failure"
        base = postCommitIndexDependencies
    initTestRepository directory
    repository <- assertRight =<< discoverRepository systemGit directory
    initialized <- assertRight =<< initCommand repository
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
    result <- compilePostCommitIndexWith cleanupFailure repository (initCommitOid initialized) database
    candidate <- readIORef candidateRef >>= maybe (assertFailure "missing captured candidate") pure
    let cleanupOrder = [candidate, candidate <> "-journal", candidate <> "-shm", candidate <> "-wal"]
    readIORef cleanupCalls >>= (@?= cleanupOrder)
    case postCommitIndexError result of
      Just (PostCommitIndexMultipleFailures [PostCommitIndexCompileException _, PostCommitIndexCleanupFailure path _]) -> path @?= candidate <> "-shm"
      other -> assertFailure ("expected compile then cleanup failures, got " <> show other)
    BS.readFile database >>= (@?= priorBytes)
    assertOwnedPathsExist candidate [False, False, True, False]
    removeFile (candidate <> "-shm")

postCommitIndexLiveCloseFailureIsObservable :: IO ()
postCommitIndexLiveCloseFailureIsObservable =
  withSystemTempDirectory "adrai post-commit live database close failure" $ \temporary -> do
    let directory = temporary </> "repository"
        database = temporary </> "index.sqlite"
        priorBytes = "prior index survives live close failure"
        base = postCommitIndexDependencies
    initTestRepository directory
    repository <- assertRight =<< discoverRepository systemGit directory
    initialized <- assertRight =<< initCommand repository
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
    result <- compilePostCommitIndexWith closeFailure repository (initCommitOid initialized) database
    candidate <- readIORef candidateRef >>= maybe (assertFailure "missing captured candidate") pure
    connection <- readIORef connectionRef >>= maybe (assertFailure "missing captured connection") pure
    case postCommitIndexError result of
      Just (PostCommitIndexMultipleFailures [PostCommitIndexCloseFailure _, PostCommitIndexCleanupFailure path _]) -> path @?= candidate
      other -> assertFailure ("expected close then cleanup failures, got " <> show other)
    -- The injected close failed before closing: a query proves the handle is
    -- still live, while the exact prior target was never selected for publish.
    query_ connection "SELECT value FROM meta WHERE key = 'resolved_oid'" >>= (@?= [Only (gitOidText (initCommitOid initialized))])
    BS.readFile database >>= (@?= priorBytes)
    assertOwnedPathsExist candidate [True, False, False, False]
    close connection
    removeFile candidate

assertOwnedPathsExist :: FilePath -> [Bool] -> IO ()
assertOwnedPathsExist candidate expected =
  mapM doesFileExist [candidate, candidate <> "-journal", candidate <> "-shm", candidate <> "-wal"] >>= (@?= expected)

ownedCandidateSiblings :: FilePath -> FilePath -> IO [FilePath]
ownedCandidateSiblings directory _database = do
  siblings <- listDirectory directory
  pure (sort (filter (".post-commit-" `isInfixOf`) siblings))

assertIndexed :: Repository -> GitOid -> FilePath -> IO ()
assertIndexed repository commitOid database = do
  result <- compilePostCommitIndex repository commitOid database
  postCommitIndexed result @?= True
  postCommitDatabase result @?= Just database
  postCommitIndexRevision result @?= Just commitOid
  postCommitIndexError result @?= Nothing
  postCommitIndexWarnings result @?= sort (postCommitIndexWarnings result)
  bracket (open database) close $ \connection -> do
    schemas <- query_ connection "SELECT value FROM meta WHERE key = 'schema'" :: IO [Only Text.Text]
    schemas @?= [Only "adrai-cache/1"]
    resolved <- query_ connection "SELECT value FROM meta WHERE key = 'resolved_oid'" :: IO [Only Text.Text]
    resolved @?= [Only (gitOidText commitOid)]

createCommitsExactlyFourRecords :: IO ()
createCommitsExactlyFourRecords =
  withCreateRepository $ \directory repository oldHead -> do
    let stagedPath = directory </> "unrelated-staged.txt"
        stagedBytes = "preserve these staged bytes\NULexactly"
    BS.writeFile stagedPath stagedBytes
    _ <- gitSuccess directory ["add", "unrelated-staged.txt"] BS.empty
    indexBefore <- gitSuccess directory ["ls-files", "-s", "--", "unrelated-staged.txt"] BS.empty

    beforeMs <- posixTimeMs
    result <- runCreate repository
    afterMs <- posixTimeMs
    created <- assertRight result
    headAfter <- gitText directory ["rev-parse", "HEAD"]
    headAfter @?= gitOidText (createCommitOid created)
    assertBool
      "the returned operation ID has the canonical operation shape"
      (Text.length (Text.pack (createOperationId created)) == 27 && "O" `Text.isPrefixOf` Text.pack (createOperationId created))

    -- The returned commit is the new HEAD, has precisely the previous HEAD as
    -- its only parent, and changes exactly the four canonical managed paths.
    parents <- gitText directory ["show", "-s", "--format=%P", Text.unpack headAfter]
    parents @?= oldHead
    changed <- fmap (sort . Text.lines) (gitText directory ["diff-tree", "--no-commit-id", "--name-only", "-r", Text.unpack headAfter])
    let returnedPaths = sort (map repoPathText (createCreatedPaths created))
    changed @?= returnedPaths
    length returnedPaths @?= 4
    assertBool "the transaction refreshed only generated paths" (createIndexUpdated created)

    -- The unrelated entry stays byte-identical in the caller's real index.
    indexAfter <- gitSuccess directory ["ls-files", "-s", "--", "unrelated-staged.txt"] BS.empty
    indexAfter @?= indexBefore
    BS.readFile stagedPath >>= (@?= stagedBytes)

    documents <- mapM (assertCanonicalCreatedDocument directory created oldHead) (createCreatedPaths created)
    assertCreatedDocuments created beforeMs afterMs documents

createFailurePreservesRepositoryState :: IO ()
createFailurePreservesRepositoryState =
  withCreateRepository $ \directory repository oldHead -> do
    _ <- gitSuccess directory ["checkout", "--detach"] BS.empty
    stateBefore <- repositoryHeadState repository
    case stateBefore of
      Right GitHeadDetached -> pure ()
      other -> assertFailure ("expected detached HEAD fixture, got " <> show other)
    result <- runCreate repository
    assertBool "detached-HEAD transaction must fail" (isLeft result)
    gitText directory ["rev-parse", "HEAD"] >>= (@?= oldHead)

amendCommitsTruthfulAppendOnlyOperation :: IO ()
amendCommitsTruthfulAppendOnlyOperation =
  withCreateRepository $ \directory repository _ -> do
    _ <- commitFile directory "tracked-worktree.txt" "committed tracked bytes\n"
    created <- assertRight =<< runCreate repository
    let stagedPath = directory </> "unrelated-staged.txt"
        stagedBytes = "preserve staged bytes across amend\NUL"
        trackedPath = directory </> "tracked-worktree.txt"
        trackedIndexBytes = "tracked staged bytes across amend\NUL"
        trackedWorktreeBytes = "tracked unstaged bytes across amend\NUL"
    BS.writeFile stagedPath stagedBytes
    _ <- gitSuccess directory ["add", "unrelated-staged.txt"] BS.empty
    BS.writeFile trackedPath trackedIndexBytes
    _ <- gitSuccess directory ["add", "tracked-worktree.txt"] BS.empty
    BS.writeFile trackedPath trackedWorktreeBytes
    indexBefore <- gitSuccess directory ["ls-files", "-s", "--", "unrelated-staged.txt"] BS.empty
    trackedIndexBefore <- gitSuccess directory ["ls-files", "-s", "--", "tracked-worktree.txt"] BS.empty
    beforeMs <- posixTimeMs
    amended <- assertRight =<< runAmend repository (createAdrId created) (createRecordId created) "Amended transaction" ""
      "## Decision\nUse one truthful append-only amendment.\n"
    afterMs <- posixTimeMs
    headAfter <- gitText directory ["rev-parse", "HEAD"]
    headAfter @?= gitOidText (amendCommitOid amended)
    parents <- gitText directory ["show", "-s", "--format=%P", Text.unpack headAfter]
    parents @?= gitOidText (createCommitOid created)
    changed <- fmap (sort . Text.lines) (gitText directory ["diff-tree", "--no-commit-id", "--name-only", "-r", Text.unpack headAfter])
    let returnedPaths = sort (map repoPathText (amendCreatedPaths amended))
    changed @?= returnedPaths
    length returnedPaths @?= 2
    assertBool "the transaction reports its index refresh" (amendIndexUpdated amended)
    assertBool "the compatibility updated path belongs to the exact created-path result" (amendUpdatedPath amended `elem` amendCreatedPaths amended)
    indexAfter <- gitSuccess directory ["ls-files", "-s", "--", "unrelated-staged.txt"] BS.empty
    indexAfter @?= indexBefore
    trackedIndexAfter <- gitSuccess directory ["ls-files", "-s", "--", "tracked-worktree.txt"] BS.empty
    trackedIndexAfter @?= trackedIndexBefore
    BS.readFile stagedPath >>= (@?= stagedBytes)
    BS.readFile trackedPath >>= (@?= trackedWorktreeBytes)
    documents <- mapM (amendDocumentAt directory amended) (amendCreatedPaths amended)
    assertTruthfulAmendDocuments created amended beforeMs afterMs documents

amendRejectionsDoNotCommit :: IO ()
amendRejectionsDoNotCommit =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
    headBefore <- gitText directory ["rev-parse", "HEAD"]
    unknown <- assertRight (mkAdrId "A00000000000000000000000002")
    unknownResult <- runAmend repository unknown (createRecordId created) "Unknown" "" "body\n"
    assertBool "unknown ADR must be rejected" (isLeft unknownResult)
    gitText directory ["rev-parse", "HEAD"] >>= (@?= headBefore)
    unknownRecord <- assertRight (mkRecordId "R00000000000000000000000002")
    staleResult <- runAmend repository (createAdrId created) unknownRecord "Stale" "" "body\n"
    assertBool "a non-current record must be rejected" (isLeft staleResult)
    gitText directory ["rev-parse", "HEAD"] >>= (@?= headBefore)
    noOpResult <- runAmend repository (createAdrId created) (createRecordId created) "" "" ""
    assertBool "a no-op amendment must be rejected" (isLeft noOpResult)
    gitText directory ["rev-parse", "HEAD"] >>= (@?= headBefore)

amendFailurePropagatesTransactionError :: IO ()
amendFailurePropagatesTransactionError =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
    headBefore <- gitText directory ["rev-parse", "HEAD"]
    worktreeBefore <- managedWorktreeState directory
    _ <- gitSuccess directory ["checkout", "--detach"] BS.empty
    result <- runAmend repository (createAdrId created) (createRecordId created) "Detached failure" "" "body\n"
    result @?= Left (Stage3ValidateState "HEAD is detached; attach a branch first")
    gitText directory ["rev-parse", "HEAD"] >>= (@?= headBefore)
    managedWorktreeState directory >>= (@?= worktreeBefore)

successiveAmendmentsChainFromPriorHead :: IO ()
successiveAmendmentsChainFromPriorHead =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
    firstAmendment <- assertRight =<< runAmend repository (createAdrId created) (createRecordId created) "First amendment" "" "first body\n"
    secondAmendment <- assertRight =<< runAmend repository (createAdrId created) (amendRecordId firstAmendment) "Second amendment" "" "second body\n"
    documents <- mapM (amendDocumentAt directory secondAmendment) (amendCreatedPaths secondAmendment)
    let expectedParents = [ProvenanceRecord (amendRecordId firstAmendment)]
    mapM_ (\document -> provenanceParents (parsedManagedCapsule document) @?= expectedParents) documents
    case [payload | document <- documents, ManagedConnection connection <- [parsedManagedRecord document], AmendsConnection payload <- [connectionPayload connection]] of
      [payload] -> do
        amendsFromRecord payload @?= amendRecordId secondAmendment
        amendsToRecords payload @?= [amendRecordId firstAmendment]
      other -> assertFailure ("expected one second amendment edge, got " <> show other)
    stale <- runAmend repository (createAdrId created) (createRecordId created) "Stale root" "" "body\n"
    assertBool "known stale prior head must be rejected" (isLeft stale)

amendRejectsInvalidCommittedState :: IO ()
amendRejectsInvalidCommittedState = do
  rejectInactive
  rejectConflicted
  rejectMisplaced
  where
    rejectInactive = withCreateRepository $ \directory repository _ -> do
      created <- assertRight =<< runCreate repository
      commitInactiveStatus directory created
      headBefore <- gitText directory ["rev-parse", "HEAD"]
      result <- runAmend repository (createAdrId created) (createRecordId created) "Inactive" "" "body\n"
      assertBool "inactive ADR must be rejected" (isLeft result)
      gitText directory ["rev-parse", "HEAD"] >>= (@?= headBefore)
    rejectConflicted = withCreateRepository $ \directory repository _ -> do
      created <- assertRight =<< runCreate repository
      firstAmendment <- assertRight =<< runAmend repository (createAdrId created) (createRecordId created) "First branch" "" "body\n"
      _ <- gitSuccess directory ["reset", "--hard", Text.unpack (gitOidText (createCommitOid created))] BS.empty
      conflicting <- assertRight =<< runAmend repository (createAdrId created) (createRecordId created) "Second branch" "" "body\n"
      _ <- gitSuccess directory ["cherry-pick", Text.unpack (gitOidText (amendCommitOid firstAmendment))] BS.empty
      headBefore <- gitText directory ["rev-parse", "HEAD"]
      result <- runAmend repository (createAdrId created) (amendRecordId conflicting) "Conflict" "" "body\n"
      assertBool "conflicted ADR must be rejected" (isLeft result)
      gitText directory ["rev-parse", "HEAD"] >>= (@?= headBefore)
    rejectMisplaced = withCreateRepository $ \directory repository _ -> do
      created <- assertRight =<< runCreate repository
      path <- case [candidate | candidate <- createCreatedPaths created, ".decision.md" `Text.isSuffixOf` repoPathText candidate] of
        [candidate] -> pure candidate
        other -> assertFailure ("expected one creation decision path, got " <> show other) >> fail "unreachable"
      bytes <- gitSuccess directory ["show", Text.unpack (gitOidText (createCommitOid created) <> ":" <> repoPathText path)] BS.empty
      _ <- gitSuccess directory ["rm", "--", Text.unpack (repoPathText path)] BS.empty
      createDirectoryIfMissing True (directory </> "architecture/adrai/decisions")
      BS.writeFile (directory </> "architecture/adrai/decisions/misplaced.decision.md") bytes
      _ <- gitSuccess directory ["add", "-A"] BS.empty
      _ <- gitSuccess directory ["commit", "-m", "misplace decision"] BS.empty
      headBefore <- gitText directory ["rev-parse", "HEAD"]
      result <- runAmend repository (createAdrId created) (createRecordId created) "Misplaced" "" "body\n"
      assertBool "misplaced committed record must be rejected" (isLeft result)
      gitText directory ["rev-parse", "HEAD"] >>= (@?= headBefore)

scopeUpdatesAreTruthful :: IO ()
scopeUpdatesAreTruthful =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
    library <- assertRight (mkScopePattern "lib/**")
    tests <- assertRight (mkScopePattern "test/**")
    assets <- assertRight (mkScopePattern "assets/**")
    docs <- assertRight (mkScopePattern "docs/**")
    source <- assertRight (mkScopePattern "src/**")
    inputs <- scopeInputs
    beforeMs <- posixTimeMs
    expanded <- assertRight =<< runScopeWithInputs repository (createAdrId created) Nothing [tests, library] [] inputs
    contracted <- assertRight =<< runScopeWithInputs repository (createAdrId created) Nothing [] [tests, library] inputs
    mixed <- assertRight =<< runScopeWithInputs repository (createAdrId created) Nothing [docs, assets] [source] inputs
    afterMs <- posixTimeMs
    let updates = [expanded, contracted, mixed]
        expectedParents = [createScopeId created, scopeChangeConnectionId expanded, scopeChangeConnectionId contracted]
        expectedChanges = ["expand", "contract", "mixed"]
        expectedAdded = [[library, tests], [], [assets, docs]]
        expectedRemoved = [[], [library, tests], [source]]
        expectedEffective = [[library, source, tests], [source], [assets, docs]]
    mapM_ (\update -> do
      gitText directory ["rev-parse", "HEAD"] >>= \headNow ->
        if update == mixed then headNow @?= gitOidText (scopeChangeCommitOid update) else pure ()
      scopeChangeCreatedPaths update @?= [scopeChangeNewPath update]
      assertBool "scope transaction reports index refresh" (scopeChangeIndexUpdated update)
      bytes <- gitSuccess directory ["show", Text.unpack (gitOidText (scopeChangeCommitOid update) <> ":" <> repoPathText (scopeChangeNewPath update))] BS.empty
      parsed <- assertRight (parseManagedDocument (scopeChangeNewPath update) bytes)
      case (parsedManagedRecord parsed, connectionPayloadFrom parsed) of
        (ManagedConnection connection, Just payload) -> do
          connectionRecordId connection @?= scopeChangeConnectionId update
          appliesToSubjectAdr payload @?= createAdrId created
          provenanceObjectId (parsedManagedCapsule parsed) @?= ProvenanceConnection (scopeChangeConnectionId update)
          provenanceBasis (parsedManagedCapsule parsed) @?= previousCommit update updates created
          provenanceActor (parsedManagedCapsule parsed) @?= createActorPure
          provenanceBranchHint (parsedManagedCapsule parsed) @?= Just "main"
          provenanceToolVersion (parsedManagedCapsule parsed) @?= "adrai/1.0.0"
          provenanceInputs (parsedManagedCapsule parsed) @?= inputs
          canonicalManagedPath (configManagedPaths defaultConfig) (parsedManagedRecord parsed) @?= Right (scopeChangeNewPath update)
        other -> assertFailure ("expected one scope connection, got " <> show other)
      ) updates
    documents <- mapM (scopeDocumentAt directory) updates
    sequence_ [assertScopeDocument update parent change added removed effective document | (update, parent, change, added, removed, effective, document) <- zip7 updates expectedParents expectedChanges expectedAdded expectedRemoved expectedEffective documents]
    assertBool "scope timestamps are real positive operation timestamps" (all (\document -> let timestamp = provenanceTimestampMs (parsedManagedCapsule document) in timestamp >= beforeMs && timestamp <= afterMs) documents)
    mapM_ (\(update, document) -> do
      scopeChangeAdrId update @?= createAdrId created
      operationIdText (provenanceOperationId (parsedManagedCapsule document)) @?= Text.pack (scopeChangeOperationId update)
      ) (zip updates documents)
    finalManaged <- managedCommittedBytes directory
    sort (map fst finalManaged) @?= sort (map repoPathText (createCreatedPaths created) <> map (repoPathText . scopeChangeNewPath) updates)
  where
    createActorPure = case mkActor HumanActor "mutation-service-test" Nothing of Right actor -> actor; Left err -> error (show err)
    previousCommit update allUpdates created =
      case lookup update (zip allUpdates (createCommitOid created : map scopeChangeCommitOid allUpdates)) of
        Just basis -> basis
        Nothing -> createCommitOid created

scopeReviewedSetReplacesOneHead :: IO ()
scopeReviewedSetReplacesOneHead =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
    assets <- assertRight (mkScopePattern "assets/**")
    docs <- assertRight (mkScopePattern "docs/**")
    source <- assertRight (mkScopePattern "src/**")
    inputs <- scopeInputs
    beforeDocuments <- committedManagedDocuments directory
    before <- currentReducedAdr (createAdrId created) beforeDocuments
    beforeMs <- posixTimeMs
    update <- assertRight =<< runScopeSetWithInputs repository (createAdrId created) (Just (reducedStateToken before)) "  Reviewed scope replacement\r\nwith evidence  " [docs, assets] inputs
    afterMs <- posixTimeMs
    document <- scopeDocumentAt directory update
    assertScopeDocument update (createScopeId created) "replace" [assets, docs] [source] [assets, docs] document
    case parsedManagedRecord document of
      ManagedConnection connection -> connectionRationale connection @?= "Reviewed scope replacement\nwith evidence\n"
      _ -> assertFailure "expected scope connection"
    let capsule = parsedManagedCapsule document
    provenanceBasis capsule @?= createCommitOid created
    provenanceActor capsule @?= createActorPure
    provenanceBranchHint capsule @?= Just "main"
    provenanceInputs capsule @?= inputs
    assertBool "replace timestamp is positive and from this operation" (provenanceTimestampMs capsule >= beforeMs && provenanceTimestampMs capsule <= afterMs && provenanceTimestampMs capsule > 0)
    canonicalManagedPath (configManagedPaths defaultConfig) (parsedManagedRecord document) @?= Right (scopeChangeNewPath update)
    scopeChangeCreatedPaths update @?= [scopeChangeNewPath update]
    assertBool "replace transaction refreshed the index" (scopeChangeIndexUpdated update)
    after <- currentReducedAdr (createAdrId created) =<< committedManagedDocuments directory
    assertBool "replace changes the checked state token" (reducedStateToken after /= reducedStateToken before)
  where
    createActorPure = case mkActor HumanActor "mutation-service-test" Nothing of Right actor -> actor; Left err -> error (show err)

scopeReviewedSetMergesScopeConflict :: IO ()
scopeReviewedSetMergesScopeConflict =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
    leftPattern <- assertRight (mkScopePattern "left/**")
    rightPattern <- assertRight (mkScopePattern "right/**")
    finalPattern <- assertRight (mkScopePattern "final/**")
    left <- assertRight =<< runScope repository (createAdrId created) Nothing [leftPattern] []
    _ <- gitSuccess directory ["reset", "--hard", Text.unpack (gitOidText (createCommitOid created))] BS.empty
    right <- assertRight =<< runScope repository (createAdrId created) Nothing [rightPattern] []
    _ <- gitSuccess directory ["cherry-pick", Text.unpack (gitOidText (scopeChangeCommitOid left))] BS.empty
    conflictedDocuments <- committedManagedDocuments directory
    conflicted <- currentReducedAdr (createAdrId created) conflictedDocuments
    axisResolutionHeads (reducedScopeAxis conflicted) @?= sort [scopeChangeConnectionId left, scopeChangeConnectionId right]
    basisBefore <- gitText directory ["rev-parse", "HEAD"]
    beforeMs <- posixTimeMs
    inputs <- scopeInputs
    merged <- assertRight =<< runScopeSetWithInputs repository (createAdrId created) (Just (reducedStateToken conflicted)) "Merge reviewed scope" [finalPattern] inputs
    afterMs <- posixTimeMs
    document <- scopeDocumentAt directory merged
    let parents = sort [scopeChangeConnectionId left, scopeChangeConnectionId right]
        oldUnion = sort [leftPattern, rightPattern, assertRightScope "src/**"]
    assertScopeDocumentWithParents merged parents "merge" [finalPattern] oldUnion [finalPattern] document
    case parsedManagedRecord document of
      ManagedConnection connection -> connectionRationale connection @?= "Merge reviewed scope\n"
      _ -> assertFailure "expected scope connection"
    let capsule = parsedManagedCapsule document
    gitOidText (provenanceBasis capsule) @?= basisBefore
    provenanceActor capsule @?= createActorPure
    provenanceBranchHint capsule @?= Just "main"
    provenanceInputs capsule @?= inputs
    assertBool "merge timestamp is positive and from this operation" (provenanceTimestampMs capsule >= beforeMs && provenanceTimestampMs capsule <= afterMs && provenanceTimestampMs capsule > 0)
    canonicalManagedPath (configManagedPaths defaultConfig) (parsedManagedRecord document) @?= Right (scopeChangeNewPath merged)
    scopeChangeCreatedPaths merged @?= [scopeChangeNewPath merged]
    assertBool "merge transaction refreshed the index" (scopeChangeIndexUpdated merged)
    resolvedDocuments <- committedManagedDocuments directory
    resolved <- currentReducedAdr (createAdrId created) resolvedDocuments
    reducedConflictAxes resolved @?= []
    axisResolutionHeads (reducedScopeAxis resolved) @?= [scopeChangeConnectionId merged]
    axisResolutionEffective (reducedScopeAxis resolved) @?= [finalPattern]
    assertBool "merge changes the checked state token" (reducedStateToken resolved /= reducedStateToken conflicted)
  where
    assertRightScope value = case mkScopePattern value of Right scope -> scope; Left err -> error (show err)
    createActorPure = case mkActor HumanActor "mutation-service-test" Nothing of Right actor -> actor; Left err -> error (show err)

scopeReviewedSetMergesUnchangedUnion :: IO ()
scopeReviewedSetMergesUnchangedUnion =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
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

scopeRejectionsPreserveRepository :: IO ()
scopeRejectionsPreserveRepository =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
    extra <- assertRight (mkScopePattern "test/**")
    stale <- assertRight (mkStateToken "S0000000000000000000000")
    let stagedPath = directory </> "scope-unrelated-staged.txt"
        dirtyPath = directory </> "scope-unrelated-dirty.txt"
    _ <- commitFile directory "scope-unrelated-dirty.txt" "committed\n"
    BS.writeFile stagedPath "staged\NULbytes"
    _ <- gitSuccess directory ["add", "scope-unrelated-staged.txt"] BS.empty
    BS.writeFile dirtyPath "dirty\NULbytes"
    before <- scopeFailureSnapshot directory [stagedPath, dirtyPath]
    let reject label expected action = do
          result <- action
          result @?= Left (Stage3ValidateState expected)
          scopeFailureSnapshot directory [stagedPath, dirtyPath] >>= (@?= before)
    reject "empty delta" "scope change would be empty" (runScope repository (createAdrId created) Nothing [] [])
    reject "duplicate additions" "scope additions contain duplicates" (runScope repository (createAdrId created) Nothing [extra, extra] [])
    reject "duplicate removals" "scope removals contain duplicates" (runScope repository (createAdrId created) Nothing [] [assertRightScope "src/**", assertRightScope "src/**"])
    reject "overlapping/canonicalized no-op delta" "scope additions and removals overlap" (runScope repository (createAdrId created) Nothing [extra] [extra])
    reject "already-effective addition" "scope additions already exist in the current scope" (runScope repository (createAdrId created) Nothing [assertRightScope "src/**"] [])
    reject "absent removal" "scope removals are absent from the current scope" (runScope repository (createAdrId created) Nothing [] [extra])
    reject "sole effective scope removal" "scope change would leave no effective scope" (runScope repository (createAdrId created) Nothing [] [assertRightScope "src/**"])
    reject "blank reason" "scope reason must be nonblank" (runScopeRequest repository (createAdrId created) Nothing "  \r\n  " (ScopeDelta [extra] []) =<< scopeInputs)
    reject "empty reviewed set" "reviewed scope set must not be empty" (runScopeSet repository (createAdrId created) Nothing "Reviewed" [])
    reject "duplicate reviewed set" "reviewed scope set contains duplicates" (runScopeSet repository (createAdrId created) Nothing "Reviewed" [extra, extra])
    reject "unchanged reviewed set" "reviewed scope set would not change the current scope" (runScopeSet repository (createAdrId created) Nothing "Reviewed" [assertRightScope "src/**"])
    current <- currentReducedAdr (createAdrId created) =<< committedManagedDocuments directory
    staleResult <- runScope repository (createAdrId created) (Just stale) [extra] []
    staleResult @?= Left (Stage3ValidateState ("stale ADR state: expected " <> stateTokenText stale <> ", current state is " <> stateTokenText (reducedStateToken current)))
    scopeFailureSnapshot directory [stagedPath, dirtyPath] >>= (@?= before)
    unknown <- assertRight (mkAdrId "A00000000000000000000000002")
    let unknownExpected = "scope target ADR is unknown"
    reject "unknown ADR" unknownExpected (runScope repository unknown Nothing [extra] [])
  where
    assertRightScope value = case mkScopePattern value of Right scope -> scope; Left err -> error (show err)

rejectInactiveScope :: IO ()
rejectInactiveScope =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
    extra <- assertRight (mkScopePattern "test/**")
    commitInactiveStatus directory created
    prepareScopeObservableFiles directory
    before <- repositoryObservableState directory
    result <- runScope repository (createAdrId created) Nothing [extra] []
    result @?= Left (Stage3ValidateState "scope target ADR is not active")
    repositoryObservableState directory >>= (@?= before)

rejectConflictedScope :: IO ()
rejectConflictedScope =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
    leftPattern <- assertRight (mkScopePattern "left/**")
    rightPattern <- assertRight (mkScopePattern "right/**")
    followup <- assertRight (mkScopePattern "later/**")
    left <- assertRight =<< runScope repository (createAdrId created) Nothing [leftPattern] []
    _ <- gitSuccess directory ["reset", "--hard", Text.unpack (gitOidText (createCommitOid created))] BS.empty
    _ <- assertRight =<< runScope repository (createAdrId created) Nothing [rightPattern] []
    _ <- gitSuccess directory ["cherry-pick", Text.unpack (gitOidText (scopeChangeCommitOid left))] BS.empty
    prepareScopeObservableFiles directory
    before <- repositoryObservableState directory
    result <- runScope repository (createAdrId created) Nothing [followup] []
    result @?= Left (Stage3ValidateState "scope target ADR is conflicted")
    repositoryObservableState directory >>= (@?= before)

rejectNonScopeConflict :: IO ()
rejectNonScopeConflict =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
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

scopeDetachedHeadRejected :: IO ()
scopeDetachedHeadRejected =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
    extra <- assertRight (mkScopePattern "test/**")
    _ <- gitSuccess directory ["checkout", "--detach"] BS.empty
    result <- runScope repository (createAdrId created) Nothing [extra] []
    result @?= Left (Stage3ValidateState "HEAD is detached; attach a branch first")
    -- Detaching changes only HEAD metadata; the managed tree/index/worktree is still intact.
    gitText directory ["diff", "--cached", "--name-only"] >>= (@?= "")
    managedWorktreeState directory >>= (@?= [])

scopeTransactionFailurePreservesEverything :: IO ()
scopeTransactionFailurePreservesEverything =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
    extra <- assertRight (mkScopePattern "test/**")
    let stagedPath = directory </> "scope-unrelated-staged.txt"
        dirtyPath = directory </> "scope-unrelated-dirty.txt"
        cachePath = directory </> "scope-index.sqlite"
    prepareScopeObservableFiles directory
    BS.writeFile cachePath "SQLite cache bytes must survive exactly\NUL"
    -- The empty local identity makes git commit-tree fail only after the
    -- generated managed file has been validated and written.  Transaction
    -- rollback must therefore restore every observable caller-owned state.
    _ <- gitSuccess directory ["config", "user.name", ""] BS.empty
    _ <- gitSuccess directory ["config", "user.email", ""] BS.empty
    before <- scopeFailureSnapshot directory [stagedPath, dirtyPath, cachePath]
    result <- runScope repository (createAdrId created) Nothing [extra] []
    case result of
      Left (Stage7CommitTree _) -> pure ()
      other -> assertFailure ("expected induced post-generation commit-tree failure, got " <> show other)
    scopeFailureSnapshot directory [stagedPath, dirtyPath, cachePath] >>= (@?= before)

domainUpdatesAreTruthful :: IO ()
domainUpdatesAreTruthful =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
    platform <- assertRight (mkDomain "platform")
    product <- assertRight (mkDomain "product")
    productApi <- assertRight (mkDomain "product.api")
    productWeb <- assertRight (mkDomain "product.web")
    productApi <- assertRight (mkDomain "product.api")
    compiler <- assertRight (mkDomain "compiler")
    expanded <- assertRight =<< runDomain repository (createAdrId created) Nothing "Add platform\r\n" (DomainDelta [platform] [])
    assertDomainUpdate directory expanded [createDomainId created] "expand" [platform] [] [compiler, platform] [] "Add platform\n"
    contracted <- assertRight =<< runDomain repository (createAdrId created) Nothing "Remove platform" (DomainDelta [] [platform])
    assertDomainUpdate directory contracted [domainChangeConnectionId expanded] "contract" [] [platform] [compiler] [] "Remove platform\n"
    mixed <- assertRight =<< runDomain repository (createAdrId created) Nothing "Replace compiler" (DomainDelta [product] [compiler])
    assertDomainUpdate directory mixed [domainChangeConnectionId contracted] "mixed" [product] [compiler] [product] [] "Replace compiler\n"
    refinement <- assertRight (mkDomainRefinement product productApi)
    refined <- assertRight =<< runDomain repository (createAdrId created) Nothing "Refine product" (DomainRefine [refinement])
    assertDomainUpdate directory refined [domainChangeConnectionId mixed] "refine" [productApi] [product] [productApi] [refinement] "Refine product\n"
    replaced <- assertRight =<< runDomain repository (createAdrId created) Nothing "Review" (DomainReviewedSet [compiler])
    assertDomainUpdate directory replaced [domainChangeConnectionId refined] "replace" [compiler] [productApi] [compiler] [] "Review\n"
    cleared <- assertRight =<< runDomain repository (createAdrId created) Nothing "Clear" (DomainReviewedSet [])
    assertDomainUpdate directory cleared [domainChangeConnectionId replaced] "replace" [] [compiler] [] [] "Clear\n"
    readded <- assertRight =<< runDomain repository (createAdrId created) Nothing "Readd" (DomainDelta [product] [])
    assertDomainUpdate directory readded [domainChangeConnectionId cleared] "expand" [product] [] [product] [] "Readd\n"
    reduced <- currentReducedAdr (createAdrId created) =<< committedManagedDocuments directory
    axisResolutionHeads (reducedDomainAxis reduced) @?= [domainChangeConnectionId readded]
    axisResolutionEffective (reducedDomainAxis reduced) @?= [product]

domainRejectionsPreserveRepository :: IO ()
domainRejectionsPreserveRepository =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
    platform <- assertRight (mkDomain "platform")
    compiler <- assertRight (mkDomain "compiler")
    product <- assertRight (mkDomain "product")
    productApi <- assertRight (mkDomain "product.api")
    productWeb <- assertRight (mkDomain "product.web")
    prepareScopeObservableFiles directory
    let cachePath = directory </> "domain-rejections.sqlite"
    BS.writeFile cachePath "durable rejection cache\NULbytes"
    before <- scopeFailureSnapshot directory [cachePath]
    let reject expected action = do
          result <- action
          result @?= Left (Stage3ValidateState expected)
          scopeFailureSnapshot directory [cachePath] >>= (@?= before)
    reject "domain reason must be nonblank" (runDomain repository (createAdrId created) Nothing " \r\n " (DomainDelta [platform] []))
    reject "domain change would be empty" (runDomain repository (createAdrId created) Nothing "Reason" (DomainDelta [] []))
    reject "domain additions contain duplicates" (runDomain repository (createAdrId created) Nothing "Reason" (DomainDelta [platform, platform] []))
    reject "domain additions already exist in the current domain" (runDomain repository (createAdrId created) Nothing "Reason" (DomainDelta [compiler] []))
    reject "domain removals contain duplicates" (runDomain repository (createAdrId created) Nothing "Reason" (DomainDelta [] [compiler, compiler]))
    reject "domain additions and removals overlap" (runDomain repository (createAdrId created) Nothing "Reason" (DomainDelta [compiler] [compiler]))
    reject "domain removals are absent from the current domain" (runDomain repository (createAdrId created) Nothing "Reason" (DomainDelta [] [platform]))
    reject "reviewed domain set contains duplicates" (runDomain repository (createAdrId created) Nothing "Reason" (DomainReviewedSet [compiler, compiler]))
    reject "domain set: DomainAntichainViolation (Domain \"product\") (Domain \"product.api\")" (runDomain repository (createAdrId created) Nothing "Reason" (DomainReviewedSet [product, productApi]))
    reject "reviewed domain set would not change the current domain" (runDomain repository (createAdrId created) Nothing "Reason" (DomainReviewedSet [compiler]))
    refinement <- assertRight (mkDomainRefinement product productApi)
    reject "domain refinement mappings must not be empty" (runDomain repository (createAdrId created) Nothing "Reason" (DomainRefine []))
    reject "domain refinement mappings contain duplicates" (runDomain repository (createAdrId created) Nothing "Reason" (DomainRefine [refinement, refinement]))
    refinementWeb <- assertRight (mkDomainRefinement product productWeb)
    reject "domain refinement sources contain duplicates" (runDomain repository (createAdrId created) Nothing "Reason" (DomainRefine [refinement, refinementWeb]))
    reject "domain refinement source is not active" (runDomain repository (createAdrId created) Nothing "Reason" (DomainRefine [refinement]))

domainReviewedMergeReconcilesConflict :: IO ()
domainReviewedMergeReconcilesConflict =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
    product <- assertRight (mkDomain "product")
    productApi <- assertRight (mkDomain "product.api")
    compiler <- assertRight (mkDomain "compiler")
    left <- assertRight =<< runDomain repository (createAdrId created) Nothing "Left" (DomainDelta [product] [])
    _ <- gitSuccess directory ["reset", "--hard", Text.unpack (gitOidText (createCommitOid created))] BS.empty
    right <- assertRight =<< runDomain repository (createAdrId created) Nothing "Right" (DomainDelta [productApi] [])
    _ <- gitSuccess directory ["cherry-pick", Text.unpack (gitOidText (domainChangeCommitOid left))] BS.empty
    prepareScopeObservableFiles directory
    before <- scopeFailureSnapshot directory []
    rejection <- runDomain repository (createAdrId created) Nothing "Unsafe" (DomainDelta [compiler] [])
    rejection @?= Left (Stage3ValidateState "domain target ADR is conflicted")
    scopeFailureSnapshot directory [] >>= (@?= before)
    -- Resolving two heads is material even when semantic delta is empty.
    merged <- assertRight =<< runDomain repository (createAdrId created) Nothing "Merge" (DomainReviewedSet [compiler, productApi])
    assertDomainUpdate directory merged (sort [domainChangeConnectionId left, domainChangeConnectionId right]) "merge" [] [product] [compiler, productApi] [] "Merge\n"
    reduced <- currentReducedAdr (createAdrId created) =<< committedManagedDocuments directory
    reducedConflictAxes reduced @?= []
    axisResolutionHeads (reducedDomainAxis reduced) @?= [domainChangeConnectionId merged]

domainReviewedMergeExactUnionPreservesOtherAxes :: IO ()
domainReviewedMergeExactUnionPreservesOtherAxes =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
    product <- assertRight (mkDomain "product")
    platform <- assertRight (mkDomain "platform")
    compiler <- assertRight (mkDomain "compiler")
    left <- assertRight =<< runDomain repository (createAdrId created) Nothing "Left" (DomainDelta [product] [])
    _ <- gitSuccess directory ["reset", "--hard", Text.unpack (gitOidText (createCommitOid created))] BS.empty
    right <- assertRight =<< runDomain repository (createAdrId created) Nothing "Right" (DomainDelta [platform] [])
    _ <- gitSuccess directory ["cherry-pick", Text.unpack (gitOidText (domainChangeCommitOid left))] BS.empty
    prepareScopeObservableFiles directory
    beforeSnapshot <- scopeFailureSnapshot directory []
    before <- currentReducedAdr (createAdrId created) =<< committedManagedDocuments directory
    let parents = sort [domainChangeConnectionId left, domainChangeConnectionId right]
        exactUnion = sort [compiler, platform, product]
        nonDomainProjection reduced =
          ( reducedDecisionAxis reduced
          , reducedScopeAxis reduced
          , reducedStatusAxis reduced
          , reducedDecisionHistory reduced
          , reducedAmendmentHistory reduced
          , reducedScopeHistory reduced
          , reducedStatusHistory reduced
          )
    axisResolutionHeads (reducedDomainAxis before) @?= parents
    axisResolutionEffective (reducedDomainAxis before) @?= exactUnion
    reducedConflictAxes before @?= [DomainAxis]
    merged <- assertRight =<< runDomain repository (createAdrId created) Nothing "Exact union merge" (DomainReviewedSet exactUnion)
    assertDomainUpdate directory merged parents "merge" [] [] exactUnion [] "Exact union merge\n"
    afterSnapshot <- scopeFailureSnapshot directory []
    failureCallerFiles afterSnapshot @?= failureCallerFiles beforeSnapshot
    failureDisposableIndexes afterSnapshot @?= failureDisposableIndexes beforeSnapshot
    after <- currentReducedAdr (createAdrId created) =<< committedManagedDocuments directory
    nonDomainProjection after @?= nonDomainProjection before
    reducedConflictAxes after @?= []
    axisResolutionHeads (reducedDomainAxis after) @?= [domainChangeConnectionId merged]
    axisResolutionEffective (reducedDomainAxis after) @?= exactUnion

domainRejectsNonDomainConflict :: IO ()
domainRejectsNonDomainConflict =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
    leftScope <- assertRight (mkScopePattern "left/**")
    rightScope <- assertRight (mkScopePattern "right/**")
    platform <- assertRight (mkDomain "platform")
    left <- assertRight =<< runScope repository (createAdrId created) Nothing [leftScope] []
    _ <- gitSuccess directory ["reset", "--hard", Text.unpack (gitOidText (createCommitOid created))] BS.empty
    _ <- assertRight =<< runScope repository (createAdrId created) Nothing [rightScope] []
    _ <- gitSuccess directory ["cherry-pick", Text.unpack (gitOidText (scopeChangeCommitOid left))] BS.empty
    prepareScopeObservableFiles directory
    before <- scopeFailureSnapshot directory []
    result <- runDomain repository (createAdrId created) Nothing "Unsafe" (DomainReviewedSet [platform])
    result @?= Left (Stage3ValidateState "domain target ADR is conflicted")
    scopeFailureSnapshot directory [] >>= (@?= before)

domainMissingCurrentHeadRejected :: IO ()
domainMissingCurrentHeadRejected =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
    documents <- committedManagedDocuments directory
    path <- case [canonicalManagedPath (configManagedPaths defaultConfig) (parsedManagedRecord document) | document <- documents, ManagedConnection connection <- [parsedManagedRecord document], connectionRecordId connection == createDomainId created] of
      [Right value] -> pure value
      other -> assertFailure ("expected exactly one canonical current domain path, got " <> show other) >> fail "unreachable"
    _ <- gitSuccess directory ["rm", "--", Text.unpack (repoPathText path)] BS.empty
    _ <- gitSuccess directory ["commit", "-m", "remove domain head"] BS.empty
    platform <- assertRight (mkDomain "platform")
    prepareScopeObservableFiles directory
    before <- scopeFailureSnapshot directory []
    result <- runDomain repository (createAdrId created) Nothing "Unsafe" (DomainDelta [platform] [])
    result @?= Left (Stage3ValidateState "domain target ADR has no current domain")
    scopeFailureSnapshot directory [] >>= (@?= before)

domainAuthorityFailuresPreserveRepository :: IO ()
domainAuthorityFailuresPreserveRepository =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
    platform <- assertRight (mkDomain "platform")
    prepareScopeObservableFiles directory
    let cachePath = directory </> "domain-index.sqlite"
    BS.writeFile cachePath "domain cache\NULbytes"
    before <- scopeFailureSnapshot directory [directory </> "scope-unrelated-staged.txt", directory </> "scope-unrelated-dirty.txt", cachePath]
    stale <- assertRight (mkStateToken "S0000000000000000000000")
    staleResult <- runDomain repository (createAdrId created) (Just stale) "Reason" (DomainDelta [platform] [])
    current <- currentReducedAdr (createAdrId created) =<< committedManagedDocuments directory
    staleResult @?= Left (Stage3ValidateState ("stale ADR state: expected " <> stateTokenText stale <> ", current state is " <> stateTokenText (reducedStateToken current)))
    scopeFailureSnapshot directory [directory </> "scope-unrelated-staged.txt", directory </> "scope-unrelated-dirty.txt", cachePath] >>= (@?= before)
    unknown <- assertRight (mkAdrId "A00000000000000000000000002")
    unknownResult <- runDomain repository unknown Nothing "Reason" (DomainDelta [platform] [])
    unknownResult @?= Left (Stage3ValidateState "domain target ADR is unknown")
    scopeFailureSnapshot directory [directory </> "scope-unrelated-staged.txt", directory </> "scope-unrelated-dirty.txt", cachePath] >>= (@?= before)
    _ <- gitSuccess directory ["checkout", "--detach"] BS.empty
    detachedBefore <- scopeFailureSnapshot directory [directory </> "scope-unrelated-staged.txt", directory </> "scope-unrelated-dirty.txt", cachePath]
    detached <- runDomain repository (createAdrId created) Nothing "Reason" (DomainDelta [platform] [])
    detached @?= Left (Stage3ValidateState "HEAD is detached; attach a branch first")
    scopeFailureSnapshot directory [directory </> "scope-unrelated-staged.txt", directory </> "scope-unrelated-dirty.txt", cachePath] >>= (@?= detachedBefore)
    _ <- gitSuccess directory ["checkout", "main"] BS.empty
    commitInactiveStatus directory created
    inactiveBefore <- scopeFailureSnapshot directory [directory </> "scope-unrelated-staged.txt", directory </> "scope-unrelated-dirty.txt", cachePath]
    inactive <- runDomain repository (createAdrId created) Nothing "Reason" (DomainDelta [platform] [])
    inactive @?= Left (Stage3ValidateState "domain target ADR is not active")
    scopeFailureSnapshot directory [directory </> "scope-unrelated-staged.txt", directory </> "scope-unrelated-dirty.txt", cachePath] >>= (@?= inactiveBefore)

domainTransactionFailurePreservesEverything :: IO ()
domainTransactionFailurePreservesEverything =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
    platform <- assertRight (mkDomain "platform")
    prepareScopeObservableFiles directory
    let cachePath = directory </> "domain-transaction.sqlite"
    BS.writeFile cachePath "durable domain cache\NUL"
    _ <- gitSuccess directory ["config", "user.name", ""] BS.empty
    _ <- gitSuccess directory ["config", "user.email", ""] BS.empty
    before <- scopeFailureSnapshot directory [directory </> "scope-unrelated-staged.txt", directory </> "scope-unrelated-dirty.txt", cachePath]
    result <- runDomain repository (createAdrId created) Nothing "Commit must fail" (DomainDelta [platform] [])
    case result of
      Left (Stage7CommitTree _) -> pure ()
      other -> assertFailure ("expected induced Stage7CommitTree failure, got " <> show other)
    scopeFailureSnapshot directory [directory </> "scope-unrelated-staged.txt", directory </> "scope-unrelated-dirty.txt", cachePath] >>= (@?= before)

statusTransitionsAreTruthful :: IO ()
statusTransitionsAreTruthful =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
    before <- reducedAdrAt directory (createAdrId created)
    obsolete <- assertRight =<< runObsolete repository (createAdrId created) (Just (reducedStateToken before)) "  Superseded by a reviewed decision\r\n" Nothing
    assertStatusUpdate directory StatusObsolete "Superseded by a reviewed decision\n" [createStatusId created] [createRecordId created] Nothing False (fromObsolete obsolete)
    afterObsolete <- reducedAdrAt directory (createAdrId created)
    reactivated <- assertRight =<< runReactivate repository (createAdrId created) (Just (reducedStateToken afterObsolete)) "Restore decision" False
    assertStatusUpdate directory StatusActive "Restore decision\n" [obsoleteConnectionId obsolete] [createRecordId created] Nothing False (fromReactivate reactivated)

statusRejectionsPreserveRepository :: IO ()
statusRejectionsPreserveRepository =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
    let callerFiles = [directory </> "status-unrelated-staged.txt", directory </> "status-unrelated-dirty.txt"]
    _ <- gitSuccess directory ["update-index", "--add", "--cacheinfo", "100644," <> replicate 40 '1' <> ",status-unrelated-staged.txt"] BS.empty
    BS.writeFile (directory </> "status-unrelated-dirty.txt") "dirty\n"
    assertStatusFailure directory callerFiles (Stage3ValidateState "obsolete reason must be nonblank") (runObsolete repository (createAdrId created) Nothing " \r\n " Nothing)
    assertStatusFailure directory callerFiles (Stage3ValidateState "obsolete replacement ADR must not name the target ADR") (runObsolete repository (createAdrId created) Nothing "Reason" (Just (createAdrId created)))
    unknown <- assertRight (mkAdrId "A00000000000000000000000002")
    assertStatusFailure directory callerFiles (Stage3ValidateState "obsolete replacement ADR is unknown") (runObsolete repository (createAdrId created) Nothing "Reason" (Just unknown))
    obsolete <- assertRight =<< runObsolete repository (createAdrId created) Nothing "Reason" Nothing
    afterObsolete <- scopeFailureSnapshot directory callerFiles
    assertStatusFailure directory callerFiles (Stage3ValidateState "obsolete target ADR is already obsolete") (runObsolete repository (createAdrId created) Nothing "Again" Nothing)
    reactivated <- assertRight =<< runReactivate repository (createAdrId created) Nothing "Restore" False
    afterReactivate <- scopeFailureSnapshot directory callerFiles
    assertStatusFailure directory callerFiles (Stage3ValidateState "reactivate target ADR is already active") (runReactivate repository (createAdrId created) Nothing "Again" False)
    -- The original rejected attempts made no observable changes before the
    -- one intentional obsolete transition.
    assertBool "intentional transition advanced HEAD" (failureHeadOid afterObsolete /= failureHeadOid afterReactivate)
    obsoleteConnectionId obsolete @?= reactivateStatusParents reactivated !! 0

statusResolveMergesConflict :: IO ()
statusResolveMergesConflict =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
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

statusTransactionFailurePreservesEverything :: IO ()
statusTransactionFailurePreservesEverything =
  withCreateRepository $ \directory repository _ -> do
    created <- assertRight =<< runCreate repository
    prepareScopeObservableFiles directory
    before <- scopeFailureSnapshot directory [directory </> "scope-unrelated-staged.txt", directory </> "scope-unrelated-dirty.txt"]
    _ <- gitSuccess directory ["config", "user.name", ""] BS.empty
    _ <- gitSuccess directory ["config", "user.email", ""] BS.empty
    result <- runObsolete repository (createAdrId created) Nothing "Commit must fail" Nothing
    case result of
      Left (Stage7CommitTree _) -> pure ()
      other -> assertFailure ("expected induced Stage7CommitTree failure, got " <> show other)
    scopeFailureSnapshot directory [directory </> "scope-unrelated-staged.txt", directory </> "scope-unrelated-dirty.txt"] >>= (@?= before)

statusAuthorityMatrixPreservesRepository :: IO ()
statusAuthorityMatrixPreservesRepository = do
  replacementMatrix
  inactiveReplacement
  conflictedReplacement
  authorityMatrix
  missingHeadMatrix
  conflictMatrix
  compatibilityMatrix
  where
    replacementMatrix = withCreateRepository $ \directory repository _ -> do
      created <- assertRight =<< runCreate repository
      replacement <- assertRight =<< runCreateWithIds repository "A00000000000000000000000002" "R00000000000000000000000002"
      positive <- assertRight =<< runObsolete repository (createAdrId created) Nothing "Replace with active ADR" (Just (createAdrId replacement))
      assertStatusUpdate directory StatusObsolete "Replace with active ADR\n" [createStatusId created] [createRecordId created] (Just (createAdrId replacement)) False (fromObsolete positive)
    inactiveReplacement = withCreateRepository $ \directory repository _ -> do
      created <- assertRight =<< runCreate repository
      replacement <- assertRight =<< runCreateWithIds repository "A00000000000000000000000002" "R00000000000000000000000002"
      _ <- assertRight =<< runObsolete repository (createAdrId replacement) Nothing "Retire replacement" Nothing
      assertStatusFailure directory [] (Stage3ValidateState "obsolete replacement ADR is not unambiguously active") (runObsolete repository (createAdrId created) Nothing "Reason" (Just (createAdrId replacement)))
    conflictedReplacement = withCreateRepository $ \directory repository _ -> do
      created <- assertRight =<< runCreate repository
      replacement <- assertRight =<< runCreateWithIds repository "A00000000000000000000000002" "R00000000000000000000000002"
      _ <- commitStatusBranch directory replacement "C00000000000000000000000009" "O00000000000000000000000009" StatusObsolete
      _ <- commitStatusBranch directory replacement "C00000000000000000000000008" "O00000000000000000000000008" StatusActive
      assertStatusFailure directory [] (Stage3ValidateState "obsolete replacement ADR is conflicted") (runObsolete repository (createAdrId created) Nothing "Reason" (Just (createAdrId replacement)))
    authorityMatrix = withCreateRepository $ \directory repository _ -> do
      created <- assertRight =<< runCreate repository
      unknown <- assertRight (mkAdrId "A00000000000000000000000002")
      assertStatusFailure directory [] (Stage3ValidateState "status target ADR is unknown") (runObsolete repository unknown Nothing "Reason" Nothing)
      token <- reducedStateToken <$> reducedAdrAt directory (createAdrId created)
      commitInactiveStatus directory created
      current <- reducedStateToken <$> reducedAdrAt directory (createAdrId created)
      assertStatusFailure directory [] (Stage3ValidateState ("stale ADR state: expected " <> stateTokenText token <> ", current state is " <> stateTokenText current)) (runObsolete repository (createAdrId created) (Just token) "Reason" Nothing)
      _ <- gitSuccess directory ["checkout", "--detach"] BS.empty
      assertStatusFailure directory [] (Stage3ValidateState "HEAD is detached; attach a branch first") (runObsolete repository (createAdrId created) Nothing "Reason" Nothing)
    missingHeadMatrix = do
      withCreateRepository $ \directory repository _ -> do
        created <- assertRight =<< runCreate repository
        path <- managedPathFor directory isStatusDocument
        _ <- gitSuccess directory ["rm", "--", Text.unpack path] BS.empty
        _ <- gitSuccess directory ["commit", "-m", "remove status head"] BS.empty
        assertStatusFailure directory [] (Stage3ValidateState "status target ADR has no current status") (runObsolete repository (createAdrId created) Nothing "Reason" Nothing)
      withCreateRepository $ \directory repository _ -> do
        created <- assertRight =<< runCreate repository
        path <- managedPathFor directory isDecisionDocument
        _ <- gitSuccess directory ["rm", "--", Text.unpack path] BS.empty
        _ <- gitSuccess directory ["commit", "-m", "remove decision head"] BS.empty
        assertStatusFailure directory [] (Stage3ValidateState "status target ADR has no current status") (runObsolete repository (createAdrId created) Nothing "Reason" Nothing)
    conflictMatrix = do
      withCreateRepository $ \directory repository _ -> do
        created <- assertRight =<< runCreate repository
        left <- commitStatusBranch directory created "C00000000000000000000000009" "O00000000000000000000000009" StatusObsolete
        right <- commitStatusBranch directory created "C00000000000000000000000008" "O00000000000000000000000008" StatusActive
        reduced <- reducedAdrAt directory (createAdrId created)
        merged <- assertRight =<< runReactivate repository (createAdrId created) (Just (reducedStateToken reduced)) "Reviewed reactivation" True
        assertStatusUpdate directory StatusActive "Reviewed reactivation\n" (sort [left, right]) [createRecordId created] Nothing True (fromReactivate merged)
      withCreateRepository $ \directory repository _ -> do
        created <- assertRight =<< runCreate repository
        leftPattern <- assertRight (mkScopePattern "conflict/**")
        rightPattern <- assertRight (mkScopePattern "other/**")
        _ <- gitSuccess directory ["checkout", "-b", "status-resolve-scope-left"] BS.empty
        left <- assertRight =<< runScope repository (createAdrId created) Nothing [leftPattern] []
        _ <- gitSuccess directory ["checkout", "main"] BS.empty
        right <- assertRight =<< runScope repository (createAdrId created) Nothing [rightPattern] []
        _ <- gitSuccess directory ["cherry-pick", Text.unpack (gitOidText (scopeChangeCommitOid left))] BS.empty
        conflicted <- reducedAdrAt directory (createAdrId created)
        reducedConflictAxes conflicted @?= [ScopeAxis]
        axisResolutionHeads (reducedScopeAxis conflicted) @?= sort [scopeChangeConnectionId left, scopeChangeConnectionId right]
        assertStatusFailure directory [] (Stage3ValidateState "status resolve requires a conflicted status axis") (runReactivate repository (createAdrId created) Nothing "Unsafe" True)
    compatibilityMatrix = withCreateRepository $ \directory repository _ -> do
      created <- assertRight =<< runCreate repository
      actor <- createActor
      inputs <- scopeInputs
      bogus <- assertRight (mkRecordId "R00000000000000000000000002")
      obsolete <- assertRight =<< obsoleteCommand repository (configManagedPaths defaultConfig) actor (createAdrId created) bogus inputs
      obsoleteRecordHeads obsolete @?= [createRecordId created]
      assertStatusUpdate directory StatusObsolete "Explorer status update\n" [createStatusId created] [createRecordId created] Nothing False (fromObsolete obsolete)

runObsolete :: Repository -> AdrId -> Maybe StateToken -> Text.Text -> Maybe AdrId -> IO (Either TransactionError ObsoleteResult)
runObsolete repository adr expected reason replacement = do
  actor <- createActor
  inputs <- scopeInputs
  obsoleteCommand repository (configManagedPaths defaultConfig) actor adr (ObsoleteRequest expected reason False replacement) inputs

runReactivate :: Repository -> AdrId -> Maybe StateToken -> Text.Text -> Bool -> IO (Either TransactionError ReactivateResult)
runReactivate repository adr expected reason resolve = do
  actor <- createActor
  inputs <- scopeInputs
  reactivateCommand repository (configManagedPaths defaultConfig) actor adr (ReactivateRequest expected reason resolve) inputs

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

managedPathFor :: FilePath -> (ParsedManagedDocument -> Bool) -> IO Text.Text
managedPathFor directory predicate = do
  committed <- managedCommittedBytes directory
  candidates <- fmap concat $ mapM select committed
  case candidates of
    [path] -> pure path
    other -> assertFailure ("expected exactly one managed path, got " <> show other) >> fail "unreachable"
  where
    select (path, bytes) = do
      repoPath <- assertRight (mkRepoPath path)
      document <- assertRight (parseManagedDocument repoPath bytes)
      pure [path | predicate document]

isStatusDocument :: ParsedManagedDocument -> Bool
isStatusDocument document = case parsedManagedRecord document of
  ManagedConnection connection -> case connectionPayload connection of StatusConnection _ -> True; _ -> False
  _ -> False

isDecisionDocument :: ParsedManagedDocument -> Bool
isDecisionDocument document = case parsedManagedRecord document of ManagedDecision _ -> True; _ -> False

statusDocumentAt :: FilePath -> GitOid -> RepoPath -> IO ParsedManagedDocument
statusDocumentAt directory commit path = do
  bytes <- gitSuccess directory ["show", Text.unpack (gitOidText commit <> ":" <> repoPathText path)] BS.empty
  assertRight (parseManagedDocument path bytes)

fromObsolete :: ObsoleteResult -> StatusUpdate
fromObsolete result = StatusUpdate
  { statusUpdateOperationId = obsoleteOperationId result, statusUpdateAdrId = obsoleteAdrId result
  , statusUpdateConnectionId = obsoleteConnectionId result, statusUpdateParents = obsoleteStatusParents result
  , statusUpdateRecords = obsoleteRecordHeads result, statusUpdateReplacement = obsoleteReplacementAdr result
  , statusUpdateResolved = obsoleteResolvedConflict result, statusUpdateCommit = obsoleteCommitOid result
  , statusUpdatePath = obsoleteNewPath result, statusUpdatePaths = obsoleteCreatedPaths result
  , statusUpdateIndexUpdated = obsoleteIndexUpdated result }

fromReactivate :: ReactivateResult -> StatusUpdate
fromReactivate result = StatusUpdate
  { statusUpdateOperationId = reactivateOperationId result, statusUpdateAdrId = reactivateAdrId result
  , statusUpdateConnectionId = reactivateConnectionId result, statusUpdateParents = reactivateStatusParents result
  , statusUpdateRecords = reactivateRecordHeads result, statusUpdateReplacement = Nothing
  , statusUpdateResolved = reactivateResolvedConflict result, statusUpdateCommit = reactivateCommitOid result
  , statusUpdatePath = reactivateNewPath result, statusUpdatePaths = reactivateCreatedPaths result
  , statusUpdateIndexUpdated = reactivateIndexUpdated result }

assertStatusFailure :: FilePath -> [FilePath] -> TransactionError -> IO (Either TransactionError value) -> IO ()
assertStatusFailure directory callerFiles expected action = do
  before <- scopeFailureSnapshot directory callerFiles
  result <- action
  case result of
    Left actual -> actual @?= expected
    Right _ -> assertFailure ("expected status failure " <> show expected)
  scopeFailureSnapshot directory callerFiles >>= (@?= before)

assertStatusUpdate :: FilePath -> StatusState -> Text.Text -> [ConnectionId] -> [RecordId] -> Maybe AdrId -> Bool -> StatusUpdate -> IO ()
assertStatusUpdate directory expectedState rationale parents records replacement resolved update = do
  document <- statusDocumentAt directory (statusUpdateCommit update) (statusUpdatePath update)
  case parsedManagedRecord document of
    ManagedConnection connection -> case connectionPayload connection of
      StatusConnection payload -> do
        statusSubjectAdr payload @?= statusUpdateAdrId update
        statusParentConnections payload @?= parents
        statusState payload @?= expectedState
        statusRecordHeads payload @?= records
        statusReplacementAdr payload @?= replacement
        connectionRationale connection @?= rationale
      _ -> assertFailure "expected status connection payload"
    _ -> assertFailure "expected status connection"
  statusUpdateParents update @?= parents
  statusUpdateRecords update @?= records
  statusUpdateReplacement update @?= replacement
  statusUpdateResolved update @?= resolved
  statusUpdatePaths update @?= [statusUpdatePath update]
  assertBool "status transaction refreshed index" (statusUpdateIndexUpdated update)
  let capsule = parsedManagedCapsule document
      expectedEvent = if expectedState == StatusObsolete then "decision.obsolete" else "decision.reactivate"
  actor <- createActor
  inputs <- scopeInputs
  provenanceObjectId capsule @?= ProvenanceConnection (statusUpdateConnectionId update)
  operationIdText (provenanceOperationId capsule) @?= Text.pack (statusUpdateOperationId update)
  provenanceParents capsule @?= map ProvenanceConnection parents <> map ProvenanceRecord records
  provenanceBranchHint capsule @?= Just "main"
  provenanceActor capsule @?= actor
  provenanceInputs capsule @?= inputs
  eventKindText (provenanceEventKind capsule) @?= expectedEvent
  provenanceToolVersion capsule @?= "adrai/1.0.0"
  assertBool "status timestamp is positive" (provenanceTimestampMs capsule > 0)
  semantic <- case parsedManagedRecord document of
    ManagedConnection connection -> assertRight (renderConnectionSemantic connection)
    _ -> assertFailure "expected status connection" >> fail "unreachable"
  provenanceSemanticDigest capsule @?= semanticDigest semantic
  canonicalManagedPath (configManagedPaths defaultConfig) (parsedManagedRecord document) @?= Right (statusUpdatePath update)
  worktree <- gitSuccess directory ["show", "HEAD:" <> Text.unpack (repoPathText (statusUpdatePath update))] BS.empty
  committed <- gitSuccess directory ["show", Text.unpack (gitOidText (statusUpdateCommit update) <> ":" <> repoPathText (statusUpdatePath update))] BS.empty
  worktree @?= committed
  worktreeStatus <- managedWorktreeState directory
  [] @?= worktreeStatus
  gitText directory ["rev-parse", "HEAD"] >>= (@?= gitOidText (statusUpdateCommit update))
  gitText directory ["show", "-s", "--format=%P", Text.unpack (gitOidText (statusUpdateCommit update))] >>= (@?= gitOidText (provenanceBasis capsule))
  gitText directory ["show", "-s", "--format=%s", Text.unpack (gitOidText (statusUpdateCommit update))] >>= (@?= ("adrai: " <> if expectedState == StatusObsolete then "obsolete " else "reactivate ") <> adrIdText (statusUpdateAdrId update))
  body <- gitText directory ["show", "-s", "--format=%B", Text.unpack (gitOidText (statusUpdateCommit update))]
  assertBool "status commit contains exact ADR trailer" (("ADR: " <> adrIdText (statusUpdateAdrId update)) `Text.isInfixOf` body)
  assertBool "status commit contains exact Objects trailer" (("Objects: " <> connectionIdText (statusUpdateConnectionId update)) `Text.isInfixOf` body)

runDomain :: Repository -> AdrId -> Maybe StateToken -> Text.Text -> DomainChangeRequest -> IO (Either TransactionError DomainChangeResult)
runDomain repository adr expected reason request = do
  actor <- createActor
  inputs <- scopeInputs
  changeDomainCommand repository (configManagedPaths defaultConfig) actor adr expected reason request inputs

domainDocumentAt :: FilePath -> DomainChangeResult -> IO ParsedManagedDocument
domainDocumentAt directory update = do
  bytes <- gitSuccess directory ["show", Text.unpack (gitOidText (domainChangeCommitOid update) <> ":" <> repoPathText (domainChangeNewPath update))] BS.empty
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
  headNow <- gitText directory ["rev-parse", "HEAD"]
  headNow @?= gitOidText (domainChangeCommitOid update)
  parentText <- gitText directory ["show", "-s", "--format=%P", Text.unpack (gitOidText (domainChangeCommitOid update))]
  parentText @?= gitOidText (provenanceBasis capsule)
  subject <- gitText directory ["show", "-s", "--format=%s", Text.unpack (gitOidText (domainChangeCommitOid update))]
  subject @?= "adrai: domain " <> adrIdText (domainChangeAdrId update)
  body <- gitText directory ["show", "-s", "--format=%B", Text.unpack (gitOidText (domainChangeCommitOid update))]
  assertBool "domain commit contains exact ADR trailer" (("ADR: " <> adrIdText (domainChangeAdrId update)) `Text.isInfixOf` body)
  assertBool "domain commit contains exact Objects trailer" (("Objects: " <> connectionIdText (domainChangeConnectionId update)) `Text.isInfixOf` body)
  where
    domainsSubjectAdrFrom parsed = case parsedManagedRecord parsed of
      ManagedConnection connection -> case connectionPayload connection of
        DomainsConnection payload -> domainsSubjectAdr payload
        _ -> error "expected domains"
      _ -> error "expected connection"

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
  status <- fmap Text.lines (gitText directory ["status", "--porcelain=v1", "--untracked-files=all"])
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
  actor <- createActor
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
  bytes <- gitSuccess directory ["show", Text.unpack (gitOidText (scopeChangeCommitOid update) <> ":" <> repoPathText (scopeChangeNewPath update))] BS.empty
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
  eventKindText (provenanceEventKind capsule) @?= "scope.update"
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

repositoryObservableState :: FilePath -> IO FailureSnapshot
repositoryObservableState directory = scopeFailureSnapshot directory []

managedCommittedBytes :: FilePath -> IO [(Text.Text, BS.ByteString)]
managedCommittedBytes directory = do
  paths <- fmap Text.lines (gitText directory ["ls-tree", "-r", "--name-only", "HEAD", "--", "architecture/adrai"])
  mapM (\path -> do
    bytes <- gitSuccess directory ["show", Text.unpack ("HEAD:" <> path)] BS.empty
    pure (path, bytes)) paths

managedWorktreeState :: FilePath -> IO [Text.Text]
managedWorktreeState directory =
  fmap Text.lines (gitText directory ["status", "--porcelain=v1", "--untracked-files=all", "--", "architecture/adrai"])

commitInactiveStatus :: FilePath -> CreateResult -> IO ()
commitInactiveStatus directory created = do
  actor <- createActor
  connectionId <- assertRight (mkConnectionId "C00000000000000000000000009")
  operationId <- assertRight (mkOperationId "O00000000000000000000000009")
  let statusRecord =
        ConnectionRecord
          { connectionRecordId = connectionId,
            connectionPayload =
              StatusConnection
                StatusPayload
                  { statusSubjectAdr = createAdrId created,
                    statusParentConnections = [createStatusId created],
                    statusState = StatusObsolete,
                    statusRecordHeads = [createRecordId created],
                    statusReplacementAdr = Nothing
                  },
            connectionRationale = "Mark decision obsolete.\n"
          }
      managed = ManagedConnection statusRecord
  semantic <- assertRight (renderConnectionSemantic statusRecord)
  eventKind <- assertRight (mkEventKind "decision.obsolete")
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
              capsuleInputParents = [ProvenanceConnection (createStatusId created)],
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
  _ <- commitFile directory (Text.unpack (repoPathText path)) bytes
  pure ()

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

amendDocumentAt :: FilePath -> AmendResult -> RepoPath -> IO ParsedManagedDocument
amendDocumentAt directory amended path = do
  bytes <- gitSuccess directory ["show", Text.unpack (gitOidText (amendCommitOid amended) <> ":" <> repoPathText path)] BS.empty
  assertRight (parseManagedDocument path bytes)

assertTruthfulAmendDocuments :: CreateResult -> AmendResult -> Integer -> Integer -> [ParsedManagedDocument] -> IO ()
assertTruthfulAmendDocuments created amended beforeMs afterMs documents = do
  expectedActor <- createActor
  domain <- assertRight (mkDomain "compiler")
  let capsules = map parsedManagedCapsule documents
      expectedParents = [ProvenanceRecord (createRecordId created)]
      expectedObjects =
        Set.fromList
          [ ProvenanceRecord (amendRecordId amended),
            ProvenanceConnection (amendConnectionId amended)
          ]
  Set.fromList (map provenanceObjectId capsules) @?= expectedObjects
  assertBool "amendment members share one claimed timestamp" (not (null capsules) && all (== provenanceTimestampMs (head capsules)) (map provenanceTimestampMs capsules))
  assertBool "amendment timestamp is a real operation timestamp" (all (\capsule -> provenanceTimestampMs capsule >= beforeMs && provenanceTimestampMs capsule <= afterMs) capsules)
  mapM_ (assertAmendCapsule expectedActor created amended expectedParents) capsules
  case [(decision, capsule) | document <- documents, ManagedDecision decision <- [parsedManagedRecord document], let capsule = parsedManagedCapsule document] of
    [(decision, capsule)] -> do
      decisionAdr decision @?= createAdrId created
      decisionRecord decision @?= amendRecordId amended
      decisionTitle decision @?= "Amended transaction"
      decisionSummary decision @?= "Create commits the complete canonical document set."
      decisionDomains decision @?= [domain]
      decisionBody decision @?= "## Decision\nUse one truthful append-only amendment.\n"
      eventKindText (provenanceEventKind capsule) @?= "decision.amend"
      provenanceParents capsule @?= expectedParents
    other -> assertFailure ("expected one amended decision, got " <> show other)
  case [(connection, payload, capsule) | document <- documents, ManagedConnection connection <- [parsedManagedRecord document], AmendsConnection payload <- [connectionPayload connection], let capsule = parsedManagedCapsule document] of
    [(connection, payload, capsule)] -> do
      connectionRecordId connection @?= amendConnectionId amended
      amendsSubjectAdr payload @?= createAdrId created
      amendsFromRecord payload @?= amendRecordId amended
      amendsToRecords payload @?= [createRecordId created]
      eventKindText (provenanceEventKind capsule) @?= "connection.amends"
      provenanceParents capsule @?= expectedParents
    other -> assertFailure ("expected one amendment edge, got " <> show other)

assertAmendCapsule :: Actor -> CreateResult -> AmendResult -> [ProvenanceObjectId] -> ProvenanceCapsule -> IO ()
assertAmendCapsule expectedActor created amended expectedParents capsule = do
  operationIdText (provenanceOperationId capsule) @?= Text.pack (amendOperationId amended)
  provenanceBasis capsule @?= createCommitOid created
  provenanceActor capsule @?= expectedActor
  provenanceBranchHint capsule @?= Just "main"
  provenanceToolVersion capsule @?= "adrai/1.0.0"
  provenanceParents capsule @?= expectedParents

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

assertCanonicalCreatedDocument :: FilePath -> CreateResult -> Text.Text -> RepoPath -> IO ParsedManagedDocument
assertCanonicalCreatedDocument directory created oldHead path = do
  bytes <- gitSuccess directory ["show", Text.unpack (gitOidText (createCommitOid created) <> ":" <> repoPathText path)] BS.empty
  parsed <- assertRight (parseManagedDocument path bytes)
  let capsule = parsedManagedCapsule parsed
  expectedActor <- createActor
  operationIdText (provenanceOperationId capsule) @?= Text.pack (createOperationId created)
  gitOidText (provenanceBasis capsule) @?= oldHead
  provenanceActor capsule @?= expectedActor
  provenanceBranchHint capsule @?= Just "main"
  provenanceToolVersion capsule @?= "adrai/1.0.0"
  assertBool "create provenance uses a positive millisecond timestamp" (provenanceTimestampMs capsule > 0)
  canonicalManagedPath (configManagedPaths defaultConfig) (parsedManagedRecord parsed) @?= Right path
  pure parsed

assertCreatedDocuments :: CreateResult -> Integer -> Integer -> [ParsedManagedDocument] -> IO ()
assertCreatedDocuments created beforeMs afterMs documents = do
  domain <- assertRight (mkDomain "compiler")
  scope <- assertRight (mkScopePattern "src/**")
  let objects = map (provenanceObjectId . parsedManagedCapsule) documents
      timestamps = map (provenanceTimestampMs . parsedManagedCapsule) documents
      connectionIds = [createScopeId created, createDomainId created, createStatusId created]
      expectedObjects =
        Set.fromList
          [ ProvenanceRecord (createRecordId created),
            ProvenanceConnection (createScopeId created),
            ProvenanceConnection (createDomainId created),
            ProvenanceConnection (createStatusId created)
          ]
  Set.fromList objects @?= expectedObjects
  Set.size (Set.fromList connectionIds) @?= 3
  assertBool "all create members share one millisecond timestamp" (not (null timestamps) && all (== head timestamps) timestamps)
  assertBool "every create timestamp falls within the operation clock bounds" (all (\timestamp -> timestamp >= beforeMs && timestamp <= afterMs) timestamps)
  case [(decision, parsedManagedCapsule document) | document <- documents, ManagedDecision decision <- [parsedManagedRecord document]] of
    [(decision, capsule)] -> do
      decisionAdr decision @?= createAdrId created
      decisionRecord decision @?= createRecordId created
      decisionTitle decision @?= "Create transaction"
      decisionSummary decision @?= "Create commits the complete canonical document set."
      decisionBody decision @?= "## Decision\nUse one append-only transaction.\n"
      decisionDomains decision @?= [domain]
      eventKindText (provenanceEventKind capsule) @?= "decision.create"
      provenanceParents capsule @?= []
    other -> assertFailure ("expected exactly one decision, got " <> show other)
  case [(connection, payload, parsedManagedCapsule document) | document <- documents, ManagedConnection connection <- [parsedManagedRecord document], AppliesToConnection payload <- [connectionPayload connection]] of
    [(connection, payload, capsule)] -> do
      let identifier = connectionRecordId connection
      identifier @?= createScopeId created
      appliesToSubjectAdr payload @?= createAdrId created
      appliesToParentConnections payload @?= []
      appliesToChange payload @?= "initial"
      appliesToAdded payload @?= [scope]
      appliesToRemoved payload @?= []
      appliesToEffective payload @?= [scope]
      connectionRationale connection @?= "Initial scope.\n"
      eventKindText (provenanceEventKind capsule) @?= "scope.initial"
      provenanceParents capsule @?= [ProvenanceRecord (createRecordId created)]
    other -> assertFailure ("expected exactly one scope payload, got " <> show other)
  case [(connection, payload, parsedManagedCapsule document) | document <- documents, ManagedConnection connection <- [parsedManagedRecord document], DomainsConnection payload <- [connectionPayload connection]] of
    [(connection, payload, capsule)] -> do
      let identifier = connectionRecordId connection
      identifier @?= createDomainId created
      domainsSubjectAdr payload @?= createAdrId created
      domainsParentConnections payload @?= []
      domainsChange payload @?= "initial"
      domainsAdded payload @?= [domain]
      domainsRemoved payload @?= []
      domainsEffective payload @?= [domain]
      domainsRefinements payload @?= []
      connectionRationale connection @?= "Initial domain.\n"
      eventKindText (provenanceEventKind capsule) @?= "domain.initial"
      provenanceParents capsule @?= [ProvenanceRecord (createRecordId created)]
    other -> assertFailure ("expected exactly one domain payload, got " <> show other)
  case [(connection, payload, parsedManagedCapsule document) | document <- documents, ManagedConnection connection <- [parsedManagedRecord document], StatusConnection payload <- [connectionPayload connection]] of
    [(connection, payload, capsule)] -> do
      let identifier = connectionRecordId connection
      identifier @?= createStatusId created
      statusSubjectAdr payload @?= createAdrId created
      statusParentConnections payload @?= []
      statusState payload @?= StatusActive
      statusRecordHeads payload @?= [createRecordId created]
      statusReplacementAdr payload @?= Nothing
      connectionRationale connection @?= "Initial active status.\n"
      eventKindText (provenanceEventKind capsule) @?= "status.initial"
      provenanceParents capsule @?= [ProvenanceRecord (createRecordId created)]
    other -> assertFailure ("expected exactly one status payload, got " <> show other)

withCreateRepository :: (FilePath -> Repository -> Text.Text -> IO value) -> IO value
withCreateRepository action =
  withSystemTempDirectory "adrai mutation service" $ \temporary -> do
    let directory = temporary </> "repository"
    initTestRepository directory
    oldHead <- commitFile directory "seed.txt" "seed\n"
    repository <- assertRight =<< discoverRepository systemGit directory
    action directory repository oldHead

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

runCreateWithIds :: Repository -> Text.Text -> Text.Text -> IO (Either TransactionError CreateResult)
runCreateWithIds repository adrText recordText = do
  actor <- createActor
  adr <- assertRight (mkAdrId adrText)
  record <- assertRight (mkRecordId recordText)
  domain <- assertRight (mkDomain "compiler")
  scope <- assertRight (mkScopePattern "src/**")
  createAdrCommand repository (configManagedPaths defaultConfig) actor adr record
    "Create replacement" "Create replacement ADR." "## Decision\nReplacement.\n" [domain] [scope] Nothing Nothing Nothing

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
