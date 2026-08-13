{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Adrai.MutationServiceTest (tests) where

import Adrai.Domain (mkDomain)
import Adrai.Format.Document
  ( AppliesToPayload (..),
    ConnectionPayload (..),
    ConnectionRecord (..),
    DecisionRecord (..),
    DomainsPayload (..),
    ManagedRecord (..),
    ParsedManagedDocument (..),
    StatusPayload (..),
    StatusState (StatusActive),
    canonicalManagedPath,
    parseManagedDocument,
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
import Adrai.Provenance
  ( ProvenanceObjectId (..),
    eventKindText,
    provenanceActor,
    provenanceBasis,
    provenanceBranchHint,
    provenanceEventKind,
    provenanceObjectId,
    provenanceOperationId,
    provenanceParents,
    provenanceTimestampMs,
    provenanceToolVersion,
  )
import Adrai.Scope (mkScopePattern)
import Adrai.Service.Mutation
  ( CreateResult (..),
    InitResult (..),
    createAdrCommand,
    initCommand,
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
import Adrai.Service.Transaction (TransactionError)
import Adrai.Types
  ( ActorKind (HumanActor),
    Actor,
    RepoPath,
    configManagedPaths,
    defaultConfig,
    mkActor,
    mkAdrId,
    mkRecordId,
    operationIdText,
    repoPathText,
  )
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import Data.Either (isLeft)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort)
import qualified Data.Set as Set
import Control.Exception (AsyncException (ThreadKilled), SomeException, bracket, throwIO, try)
import Database.SQLite.Simple (Only (..), close, open, query_)
import System.Directory (doesFileExist, listDirectory, removeFile, renameFile)
import System.FilePath ((</>), takeFileName)
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
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
    ]

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
