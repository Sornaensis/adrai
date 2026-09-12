{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Adrai.TransactionTest (tests) where

import Adrai.Domain (mkDomain)
import Adrai.Format.Document
  ( DecisionRecord (..),
    ManagedRecord (..),
    canonicalManagedPath,
    renderDecisionSemantic,
    sealManagedDocument,
  )
import Adrai.Git
  ( GitOid (..),
    Repository (..),
    RepositoryLayout (MainWorktree),
    discoverRepository,
    gitOidText,
    systemGit,
  )
import Adrai.GitTestSupport
  ( commitFile,
    gitSuccess,
    initTestRepository,
    installFailingCleanFilter,
    outputText,
    withRejectingReferenceTransactionHook,
  )
import Adrai.RetainedNative.RepositorySeed
  ( RepositorySeed,
    createRepositorySeedWith,
    removeRepositorySeed,
    withRepositorySeedCopy,
  )
import Adrai.Provenance
  ( ProvenanceCapsuleInput (..),
    ProvenanceObjectId (..),
    mkEventKind,
    mkProvenanceCapsule,
    semanticDigest,
  )
import Adrai.Provenance.Git.Lock
  ( GitLockError (..),
    GitLock (..),
    GitLockCloseOperation (CloseOwnerRelease, CloseStatusProbe),
    GitLockDependencies (GitLockDependencies),
    acquireGitLock,
    acquireGitLockWith,
    gitLockPath,
    gitLockPid,
    gitLockStatus,
    gitLockStatusWith,
    releaseGitLock,
    withGitLock,
    withGitLockWith,
  )
import Control.Concurrent (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.Async (async, wait)
import Adrai.Service.Transaction
  ( GeneratedFile (..),
    AppendOnlyDependencies (..),
    AppendOnlyTestHooks (..),
    BootstrapDependencies (..),
    TransactionConfig (..),
    TransactionError (..),
    TransactionResult (..),
    commitAppendOnlyOperation,
    commitAppendOnlyOperationWith,
    commitAppendOnlyOperationWithHooks,
    commitBootstrapFiles,
    commitBootstrapFilesWith,
    defaultBootstrapDependencies,
    defaultAppendOnlyDependencies,
    defaultAppendOnlyTestHooks,
    nullOid,
    parseSingleOidFromOutput,
  )
import Adrai.Types
  ( ActorKind (HumanActor),
    ProvenanceInputs (..),
    configManagedPaths,
    defaultConfig,
    mkActor,
    mkAdrId,
     mkOperationId,
     mkRepoPath,
     mkRecordId,
     gitRefText,
     repoPathText,
   )
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Either (isLeft)
import Data.Time.Clock (addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Control.Exception (AsyncException (ThreadKilled), SomeException, throwIO, try)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Directory (createDirectory, createDirectoryLink, doesDirectoryExist, doesFileExist, getModificationTime, listDirectory, removeDirectory, removeDirectoryRecursive, removeFile, setModificationTime)
import Data.List (isPrefixOf, sort)
import qualified System.Exit as Exit
import System.FilePath (isAbsolute, makeRelative, normalise, splitDirectories, (</>), takeDirectory)
import System.Process.Typed (runProcess, shell)
import System.Info (os)
import System.IO.Error (tryIOError)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  withResource createTransactionRepositorySeed removeTransactionRepositorySeed $ \getTransactionSeed ->
    withResource createUnbornTransactionRepositorySeed removeUnbornTransactionRepositorySeed $ \getUnbornTransactionSeed ->
      let withSeed = withTransactionRepositoryCopy getTransactionSeed
       in testGroup
            "Transaction contract"
            [ testGroup "repository-common mutation lock" (gitLockTests getTransactionSeed),
      testCase "Git plumbing accepts one bare 40- or 64-hex OID with trailing stdout framing" $
        mapM_ assertAccepted acceptedOutputs,
      testCase "Git plumbing rejects non-bare or malformed OID output" $
        mapM_ assertRejected rejectedOutputs,
      testCase "Stage7 failure restores caller index and generated worktree state exactly" $
          withSeed "adrai transaction rollback" $ \_ repositoryPath repository parentText -> do
            let stagedPath = repositoryPath </> "staged.bin"
                dirtyPath = repositoryPath </> "dirty.bin"
                untrackedPath = repositoryPath </> "untracked.bin"
                stagedBytes = BS.pack [255, 0, 13, 10, 128, 64, 1, 2, 3]
                dirtyBytes = BS.pack [3, 2, 1, 0, 255]
                untrackedBytes = BS.pack [13, 10, 0, 17, 255]
            BS.writeFile stagedPath stagedBytes
            BS.writeFile dirtyPath dirtyBytes
            BS.writeFile untrackedPath untrackedBytes
            _ <- gitSuccess repositoryPath ["add", "--", "staged.bin"] BS.empty
            (headBefore, treeBefore) <- commitHeadAndTree repositoryPath
            indexEntriesBefore <- gitSuccess repositoryPath ["ls-files", "--stage"] BS.empty
            statusBefore <- gitSuccess repositoryPath ["--no-optional-locks", "status", "--porcelain=v1", "--untracked-files=all"] BS.empty
            parent <- requireGitOid parentText
            (operationText, generated) <- transactionGeneratedFile parent
            let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
                generatedParent = takeDirectory generatedPath
                config =
                  TransactionConfig
                    { configOperationId = operationText,
                      configSubject = "adrai: rollback stage7",
                      configTrailers = Map.fromList [("Objects", "transaction-rollback")],
                      configExpectedHead = parent,
                      configGenerated = [generated]
                    }
            _ <- gitSuccess repositoryPath ["config", "user.name", ""] BS.empty
            _ <- gitSuccess repositoryPath ["config", "user.email", ""] BS.empty
            makeTrackedFileStatStale repositoryPath "seed.txt"
            indexAtSnapshotRef <- newIORef Nothing
            let indexPath = repositoryPath </> ".git" </> "index"
                dependencies =
                  defaultAppendOnlyDependencies
                    { appendOnlyBeforeSnapshot = BS.readFile indexPath >>= writeIORef indexAtSnapshotRef . Just
                    }
            indexAtEntry <- BS.readFile indexPath
            commitAppendOnlyOperationWith dependencies repository config >>= \case
              Left (Stage7CommitTree _) -> pure ()
              Left problem -> assertFailure ("expected original Stage7 failure, got " <> show problem)
              Right result -> assertFailure ("expected Stage7 failure, got " <> show result)
            indexAtSnapshot <- readIORef indexAtSnapshotRef
            (headAfter, treeAfter) <- commitHeadAndTree repositoryPath
            indexAfter <- BS.readFile indexPath
            indexEntriesAfter <- gitSuccess repositoryPath ["ls-files", "--stage"] BS.empty
            statusAfter <- gitSuccess repositoryPath ["--no-optional-locks", "status", "--porcelain=v1", "--untracked-files=all"] BS.empty
            stagedAfter <- BS.readFile stagedPath
            dirtyAfter <- BS.readFile dirtyPath
            untrackedAfter <- BS.readFile untrackedPath
            generatedExists <- doesFileExist generatedPath
            generatedParentExists <- doesDirectoryExist generatedParent
            temporaryIndexes <- adraiTemporaryIndexes repositoryPath
            assertEqual "HEAD is restored exactly" headBefore headAfter
            assertEqual "HEAD tree is restored exactly" treeBefore treeAfter
            assertEqual "Stage7 reaches the snapshot boundary without changing API-entry index bytes" (Just indexAtEntry) indexAtSnapshot
            assertEqual "raw caller index is byte-for-byte unchanged" indexAtEntry indexAfter
            assertEqual "complete caller index entries are unchanged" indexEntriesBefore indexEntriesAfter
            assertEqual "staged, dirty, and untracked path set is unchanged" statusBefore statusAfter
            assertEqual "staged binary bytes are unchanged" stagedBytes stagedAfter
            assertEqual "dirty binary bytes are unchanged" dirtyBytes dirtyAfter
            assertEqual "untracked binary bytes are unchanged" untrackedBytes untrackedAfter
            assertBool "generated managed file is removed" (not generatedExists)
            assertBool "transaction-owned generated directory is removed" (not generatedParentExists)
            assertEqual "Stage7 cleanup leaves no temporary transaction index" [] temporaryIndexes
      , testCase "rollback refuses to overwrite an external caller index change" $
          withSeed "adrai transaction external index" $ \_ repositoryPath repository parentText -> do
            let externalPath = repositoryPath </> "external.bin"
                externalBytes = BS.pack [19, 0, 255, 7]
                indexPath = repositoryPath </> ".git" </> "index"
            BS.writeFile externalPath externalBytes
            parent <- requireGitOid parentText
            (operationText, generated) <- transactionGeneratedFile parent
            indexAtSnapshotRef <- newIORef Nothing
            externalIndexRef <- newIORef Nothing
            let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
                dependencies =
                  defaultAppendOnlyDependencies
                    { appendOnlyBeforeSnapshot = BS.readFile indexPath >>= writeIORef indexAtSnapshotRef . Just,
                      appendOnlyAfterGeneratedWrite = do
                        _ <- gitSuccess repositoryPath ["add", "--", "external.bin"] BS.empty
                        BS.readFile indexPath >>= writeIORef externalIndexRef . Just
                        throwIO (Stage7CommitTree "injected failure after external index write")
                    }
                config = TransactionConfig operationText "adrai: external index" (Map.fromList [("Objects", "external-index")]) parent [generated]
            commitAppendOnlyOperationWith dependencies repository config >>= \case
              Left (RollbackFailed detail) -> assertBool "external index refusal remains typed" ("refusing to overwrite it" `Text.isInfixOf` detail)
              Left problem -> assertFailure ("expected external index refusal, got " <> show problem)
              Right result -> assertFailure ("expected external index refusal, got " <> show result)
            indexAtSnapshot <- readIORef indexAtSnapshotRef
            externalIndex <- readIORef externalIndexRef
            indexAfter <- BS.readFile indexPath
            headAfter <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD"] BS.empty
            externalAfter <- BS.readFile externalPath
            generatedAfter <- BS.readFile generatedPath
            case (indexAtSnapshot, externalIndex) of
              (Just snapshotBytes, Just externalBytesAfter) ->
                assertBool "external Git write changes the post-snapshot caller index" (snapshotBytes /= externalBytesAfter)
              _ -> assertFailure "expected both snapshot and external index callbacks"
            assertEqual "rollback preserves the externally written caller index bytes" (Just indexAfter) externalIndex
            assertEqual "rollback refusal leaves the original ref unchanged" parentText headAfter
            assertEqual "rollback refusal preserves external caller bytes" externalBytes externalAfter
            assertEqual "rollback refusal preserves generated bytes while ownership is uncertain" (genFileBytes generated) generatedAfter
      , testCase "partial Stage5 write removes only the path written by this attempt" $
          withSeed "adrai transaction partial stage5" $ \_ repositoryPath repository _ -> do
            let callerPath = "caller-owned.decision.md"
                callerBytes = "caller-owned bytes\NUL\255"
            parentText <- commitFile repositoryPath callerPath callerBytes
            parent <- requireGitOid parentText
            (operationText, firstGenerated) <- transactionGeneratedFile parent
            callerRepoPath <- requireRight (mkRepoPath (Text.pack callerPath))
            let callerGenerated = GeneratedFile callerRepoPath (genFileBytes firstGenerated)
                firstPath = repositoryPath </> Text.unpack (repoPathText (genFilePath firstGenerated))
                config =
                  TransactionConfig
                    { configOperationId = operationText,
                      configSubject = "adrai: partial stage5 rollback",
                      configTrailers = Map.fromList [("Objects", "partial-stage5")],
                      configExpectedHead = parent,
                      configGenerated = [firstGenerated, callerGenerated]
                    }
            commitAppendOnlyOperation repository config >>= \case
              Left (Stage5ValidateGenerated _) -> pure ()
              Left problem -> assertFailure ("expected Stage5 failure, got " <> show problem)
              Right result -> assertFailure ("expected Stage5 failure, got " <> show result)
            firstExists <- doesFileExist firstPath
            callerAfter <- BS.readFile (repositoryPath </> callerPath)
            assertBool "rollback removes only the successfully written first path" (not firstExists)
            assertEqual "pre-existing caller path is never rewritten or deleted" callerBytes callerAfter
      , testCase "Stage6 failure removes generated files without changing the caller index" $
          withSeed "adrai transaction stage6 rollback" $ \_ repositoryPath repository _ -> do
            let stagedPath = repositoryPath </> "staged.bin"
                stagedBytes = BS.pack [0, 255, 4, 9]
            parentText <- installFailingCleanFilter repositoryPath
            BS.writeFile stagedPath stagedBytes
            _ <- gitSuccess repositoryPath ["add", "--", "staged.bin"] BS.empty
            indexBefore <- BS.readFile (repositoryPath </> ".git" </> "index")
            parent <- requireGitOid parentText
            (operationText, generated) <- transactionGeneratedFile parent
            let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
                config =
                  TransactionConfig
                    { configOperationId = operationText,
                      configSubject = "adrai: rollback stage6",
                      configTrailers = Map.fromList [("Objects", "transaction-stage6")],
                      configExpectedHead = parent,
                      configGenerated = [generated]
                    }
            commitAppendOnlyOperation repository config >>= \case
              Left (Stage6CreateTemporaryIndex _) -> pure ()
              Left problem -> assertFailure ("expected original Stage6 failure, got " <> show problem)
              Right result -> assertFailure ("expected Stage6 failure, got " <> show result)
            indexAfter <- BS.readFile (repositoryPath </> ".git" </> "index")
            stagedAfter <- BS.readFile stagedPath
            generatedExists <- doesFileExist generatedPath
            temporaryIndexes <- adraiTemporaryIndexes repositoryPath
            assertEqual "raw caller index is byte-for-byte unchanged after Stage6" indexBefore indexAfter
            assertEqual "staged binary bytes are unchanged after Stage6" stagedBytes stagedAfter
            assertBool "Stage6 rollback removes the generated managed file" (not generatedExists)
            assertEqual "Stage6 cleanup leaves no temporary transaction index" [] temporaryIndexes
      , testCase "Stage8 rejected CAS restores generated files only after confirming the old ref" $
          withSeed "adrai transaction stage8 rollback" $ \_ repositoryPath repository parentText -> do
            let stagedPath = repositoryPath </> "staged.bin"
                stagedBytes = BS.pack [127, 0, 255, 8]
            BS.writeFile stagedPath stagedBytes
            _ <- gitSuccess repositoryPath ["add", "--", "staged.bin"] BS.empty
            parent <- requireGitOid parentText
            (operationText, generated) <- transactionGeneratedFile parent
            indexAtSnapshotRef <- newIORef Nothing
            let indexPath = repositoryPath </> ".git" </> "index"
                generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
                dependencies =
                  defaultAppendOnlyDependencies
                    { appendOnlyBeforeSnapshot = BS.readFile indexPath >>= writeIORef indexAtSnapshotRef . Just
                    }
                config =
                  TransactionConfig
                    { configOperationId = operationText,
                      configSubject = "adrai: rollback stage8",
                      configTrailers = Map.fromList [("Objects", "transaction-stage8")],
                      configExpectedHead = parent,
                      configGenerated = [generated]
                    }
            (indexAtEntry, indexEntriesAtEntry, transactionResult) <- withRejectingReferenceTransactionHook repositoryPath Nothing $ do
              makeTrackedFileStatStale repositoryPath "seed.txt"
              entryRecords <- gitSuccess repositoryPath ["ls-files", "--stage"] BS.empty
              entryBytes <- BS.readFile indexPath
              result <- commitAppendOnlyOperationWith dependencies repository config
              pure (entryBytes, entryRecords, result)
            case transactionResult of
              Left (Stage8UpdateRef _) -> pure ()
              Left problem -> assertFailure ("expected original Stage8 failure, got " <> show problem)
              Right result -> assertFailure ("expected Stage8 failure, got " <> show result)
            indexAtSnapshot <- readIORef indexAtSnapshotRef
            headAfter <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD"] BS.empty
            indexAfter <- BS.readFile indexPath
            indexEntriesAfter <- gitSuccess repositoryPath ["ls-files", "--stage"] BS.empty
            stagedAfter <- BS.readFile stagedPath
            generatedExists <- doesFileExist generatedPath
            temporaryIndexes <- adraiTemporaryIndexes repositoryPath
            assertEqual "Stage8 rejection leaves the ref at its exact old HEAD" parentText headAfter
            assertEqual "Stage8 reaches the snapshot boundary without changing API-entry index bytes" (Just indexAtEntry) indexAtSnapshot
            assertEqual "Stage8 rejection preserves the raw caller index" indexAtEntry indexAfter
            assertEqual "Stage8 rejection preserves complete caller index entries" indexEntriesAtEntry indexEntriesAfter
            assertEqual "Stage8 rejection preserves staged binary bytes" stagedBytes stagedAfter
            assertBool "Stage8 rollback removes the generated managed file" (not generatedExists)
            assertEqual "Stage8 cleanup leaves no temporary transaction index" [] temporaryIndexes
      , testCase "Stage8 rollback rechecks the pinned main ref after hook rejection" $
          withSeed "adrai transaction pinned ref" $ \_ repositoryPath repository parentText -> do
            _ <- gitSuccess repositoryPath ["branch", "other", "HEAD"] BS.empty
            parent <- requireGitOid parentText
            (operationText, generated) <- transactionGeneratedFile parent
            let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
                config = TransactionConfig operationText "adrai: pinned ref" (Map.fromList [("Objects", "pinned-ref")]) parent [generated]
            withRejectingReferenceTransactionHook repositoryPath (Just "refs/heads/other") (commitAppendOnlyOperation repository config) >>= \case
              Left (Stage8UpdateRef _) -> pure ()
              Left problem -> assertFailure ("expected Stage8 failure, got " <> show problem)
              Right result -> assertFailure ("expected Stage8 failure, got " <> show result)
            mainAfter <- outputText <$> gitSuccess repositoryPath ["rev-parse", "refs/heads/main"] BS.empty
            generatedExists <- doesFileExist generatedPath
            assertEqual "pinned main ref remains at expected old head" parentText mainAfter
            assertBool "rollback still removes only its generated path" (not generatedExists)
      , testCase "authoritative new ref outcome preserves generated paths without destructive rollback" $
          withSeed "adrai transaction authoritative new" $ \_ repositoryPath repository parentText -> do
            parent <- requireGitOid parentText
            (operationText, generated) <- transactionGeneratedFile parent
            let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
                dependencies =
                  defaultAppendOnlyDependencies
                    { appendOnlyInspectRef = \_ _ _ maybeNew ->
                        case maybeNew of
                          Nothing -> pure (Left (RollbackFailed "expected a newly created commit"))
                          Just newCommit -> pure (Right newCommit)
                    }
                config = TransactionConfig operationText "adrai: authoritative new" (Map.fromList [("Objects", "authoritative-new")]) parent [generated]
            withRejectingReferenceTransactionHook repositoryPath Nothing (commitAppendOnlyOperationWith dependencies repository config) >>= \case
              Left (Stage8UpdateRef _) -> pure ()
              Left problem -> assertFailure ("expected original Stage8 failure, got " <> show problem)
              Right result -> assertFailure ("expected Stage8 failure, got " <> show result)
            generatedAfter <- BS.readFile generatedPath
            assertEqual "authoritative new outcome retains the generated bytes" (genFileBytes generated) generatedAfter
      , testCase "ambiguous target ref outcome fails closed and preserves generated paths" $
          withSeed "adrai transaction ambiguous ref" $ \_ repositoryPath repository parentText -> do
            let otherCommit = GitOid "1111111111111111111111111111111111111111"
            parent <- requireGitOid parentText
            (operationText, generated) <- transactionGeneratedFile parent
            let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
                dependencies = defaultAppendOnlyDependencies { appendOnlyInspectRef = \_ _ _ _ -> pure (Right otherCommit) }
                config = TransactionConfig operationText "adrai: ambiguous ref" (Map.fromList [("Objects", "ambiguous-ref")]) parent [generated]
            withRejectingReferenceTransactionHook repositoryPath Nothing (commitAppendOnlyOperationWith dependencies repository config) >>= \case
              Left (RollbackFailed detail) -> assertBool "ambiguous ref is reported as a failed rollback" ("target ref changed" `Text.isInfixOf` detail)
              Left problem -> assertFailure ("expected RollbackFailed, got " <> show problem)
              Right result -> assertFailure ("expected ambiguous ref failure, got " <> show result)
            generatedAfter <- BS.readFile generatedPath
            assertEqual "ambiguous ref outcome preserves generated bytes" (genFileBytes generated) generatedAfter
      , testCase "target ref inspection failure fails closed and preserves generated paths" $
          withSeed "adrai transaction unreadable ref" $ \_ repositoryPath repository parentText -> do
            parent <- requireGitOid parentText
            (operationText, generated) <- transactionGeneratedFile parent
            let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
                dependencies = defaultAppendOnlyDependencies { appendOnlyInspectRef = \_ _ _ _ -> pure (Left (RollbackFailed "injected target-ref read failure")) }
                config = TransactionConfig operationText "adrai: unreadable ref" (Map.fromList [("Objects", "unreadable-ref")]) parent [generated]
            withRejectingReferenceTransactionHook repositoryPath Nothing (commitAppendOnlyOperationWith dependencies repository config) >>= \case
              Left (RollbackFailed detail) -> assertBool "inspection failure is retained" ("injected target-ref read failure" `Text.isInfixOf` detail)
              Left problem -> assertFailure ("expected RollbackFailed, got " <> show problem)
              Right result -> assertFailure ("expected inspection failure, got " <> show result)
            generatedAfter <- BS.readFile generatedPath
            assertEqual "inspection failure preserves generated bytes" (genFileBytes generated) generatedAfter
      , testCase "rollback inspection callback receives the ref pinned before HEAD changes" $
          withSeed "adrai transaction observed pinned ref" $ \_ repositoryPath repository parentText -> do
            _ <- gitSuccess repositoryPath ["branch", "other", "HEAD"] BS.empty
            parent <- requireGitOid parentText
            (operationText, generated) <- transactionGeneratedFile parent
            observedRef <- newIORef Nothing
            let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
                dependencies =
                  defaultAppendOnlyDependencies
                    { appendOnlyInspectRef = \inspectedRepository inspectedRef expectedOld maybeNew -> do
                        writeIORef observedRef (Just (gitRefText inspectedRef))
                        appendOnlyInspectRef defaultAppendOnlyDependencies inspectedRepository inspectedRef expectedOld maybeNew
                    }
                config = TransactionConfig operationText "adrai: observed pinned ref" (Map.fromList [("Objects", "observed-pinned-ref")]) parent [generated]
            withRejectingReferenceTransactionHook repositoryPath (Just "refs/heads/other") (commitAppendOnlyOperationWith dependencies repository config) >>= \case
              Left (Stage8UpdateRef _) -> pure ()
              Left problem -> assertFailure ("expected Stage8 failure, got " <> show problem)
              Right result -> assertFailure ("expected Stage8 failure, got " <> show result)
            observed <- readIORef observedRef
            assertEqual "rollback inspection uses the original main ref, not the later HEAD" (Just "refs/heads/main") observed
            generatedExists <- doesFileExist generatedPath
            assertBool "proven-safe rollback still removes the generated path" (not generatedExists)
      , testCase "temporary index cleanup failure retains the original failure context" $
          withSeed "adrai transaction cleanup failure" $ \_ repositoryPath repository _ -> do
            parentText <- installFailingCleanFilter repositoryPath
            parent <- requireGitOid parentText
            (operationText, generated) <- transactionGeneratedFile parent
            let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
                dependencies = defaultAppendOnlyDependencies { appendOnlyAfterTempIndexCleanup = throwIO (userError "injected temporary-index cleanup failure") }
                config = TransactionConfig operationText "adrai: cleanup failure" (Map.fromList [("Objects", "cleanup-failure")]) parent [generated]
            commitAppendOnlyOperationWith dependencies repository config >>= \case
              Left (RollbackFailed detail) -> do
                assertBool "original Stage6 failure context is retained" ("Stage6CreateTemporaryIndex" `Text.isInfixOf` detail)
                assertBool "cleanup failure context is retained" ("injected temporary-index cleanup failure" `Text.isInfixOf` detail)
              Left problem -> assertFailure ("expected RollbackFailed, got " <> show problem)
              Right result -> assertFailure ("expected cleanup failure, got " <> show result)
            generatedExists <- doesFileExist generatedPath
            assertBool "cleanup failure does not suppress the later safe generated-file rollback" (not generatedExists)
      , testCase "injected cancellation after write cleans then propagates ThreadKilled" $
          withSeed "adrai transaction cancellation" $ \_ repositoryPath repository parentText -> do
            parent <- requireGitOid parentText
            (operationText, generated) <- transactionGeneratedFile parent
            let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
                config = TransactionConfig operationText "adrai: cancellation" (Map.fromList [("Objects", "cancellation")]) parent [generated]
                dependencies = defaultAppendOnlyDependencies { appendOnlyAfterGeneratedWrite = throwIO ThreadKilled }
            result <- try @SomeException (commitAppendOnlyOperationWith dependencies repository config)
            case result of
              Left exception -> assertBool "ThreadKilled propagates" ("thread killed" `Text.isInfixOf` Text.pack (show exception))
              Right _ -> assertFailure "expected cancellation"
            generatedExists <- doesFileExist generatedPath
            assertBool "cancellation rollback removes the exact generated path" (not generatedExists)
      , testCase "partial synchronous generated write removes only its owned bytes and directories" $
          withSeed "adrai transaction partial sync" $ \_ repositoryPath repository parentText -> do
            parent <- requireGitOid parentText
            (operationText, generated) <- transactionGeneratedFile parent
            let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
                generatedDirectory = takeDirectory generatedPath
                dependencies =
                  defaultAppendOnlyDependencies
                    { appendOnlyWriteGeneratedFile = \path bytes -> do
                        BS.writeFile path (BS.take 23 bytes)
                        throwIO (Stage5ValidateGenerated "injected partial write failure")
                    }
                config = TransactionConfig operationText "adrai: partial sync" (Map.fromList [("Objects", "partial-sync")]) parent [generated]
            commitAppendOnlyOperationWith dependencies repository config >>= \case
              Left (Stage5ValidateGenerated detail) -> assertBool "the original write failure is retained" ("partial write" `Text.isInfixOf` detail)
              Left problem -> assertFailure ("expected partial write failure, got " <> show problem)
              Right result -> assertFailure ("expected partial write failure, got " <> show result)
            exists <- doesFileExist generatedPath
            directoryExists <- doesDirectoryExist generatedDirectory
            assertBool "partial bytes are removed" (not exists)
            assertBool "only newly owned parent directories are removed" (not directoryExists)
      , testCase "partial generated write cancellation cleans then rethrows ThreadKilled" $
          withSeed "adrai transaction partial async" $ \_ repositoryPath repository parentText -> do
            parent <- requireGitOid parentText
            (operationText, generated) <- transactionGeneratedFile parent
            let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
                generatedDirectory = takeDirectory generatedPath
                dependencies =
                  defaultAppendOnlyDependencies
                    { appendOnlyWriteGeneratedFile = \path bytes -> BS.writeFile path (BS.take 17 bytes) >> throwIO ThreadKilled }
                config = TransactionConfig operationText "adrai: partial async" (Map.fromList [("Objects", "partial-async")]) parent [generated]
            result <- try @SomeException (commitAppendOnlyOperationWith dependencies repository config)
            case result of
              Left exception -> assertBool "partial-write cancellation propagates" ("thread killed" `Text.isInfixOf` Text.pack (show exception))
              Right _ -> assertFailure "expected cancellation"
            exists <- doesFileExist generatedPath
            directoryExists <- doesDirectoryExist generatedDirectory
            assertBool "partial cancellation removes owned bytes" (not exists)
            assertBool "partial cancellation removes owned directories" (not directoryExists)
      , testCase "snapshot cancellation propagates before generated state exists" $
          withSeed "adrai transaction snapshot async" $ \_ repositoryPath repository parentText -> do
            parent <- requireGitOid parentText
            (operationText, generated) <- transactionGeneratedFile parent
            let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
                generatedDirectory = takeDirectory generatedPath
                dependencies = defaultAppendOnlyDependencies { appendOnlyBeforeSnapshot = throwIO ThreadKilled }
                config = TransactionConfig operationText "adrai: snapshot async" (Map.fromList [("Objects", "snapshot-async")]) parent [generated]
            result <- try @SomeException (commitAppendOnlyOperationWith dependencies repository config)
            case result of
              Left exception -> assertBool "snapshot cancellation propagates" ("thread killed" `Text.isInfixOf` Text.pack (show exception))
              Right _ -> assertFailure "expected cancellation"
            exists <- doesFileExist generatedPath
            directoryExists <- doesDirectoryExist generatedDirectory
            assertBool "snapshot cancellation never writes a file" (not exists)
            assertBool "snapshot cancellation never creates a directory" (not directoryExists)
      , testCase "cleanup cancellation finishes owned cleanup then rethrows ThreadKilled" $
          withSeed "adrai transaction cleanup async" $ \_ repositoryPath repository parentText -> do
            parent <- requireGitOid parentText
            (operationText, generated) <- transactionGeneratedFile parent
            let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
                generatedDirectory = takeDirectory generatedPath
                dependencies =
                  defaultAppendOnlyDependencies
                    { appendOnlyAfterGeneratedWrite = throwIO (Stage7CommitTree "injected pre-cleanup failure"),
                      appendOnlyBeforeRollbackCleanup = throwIO ThreadKilled
                    }
                config = TransactionConfig operationText "adrai: cleanup async" (Map.fromList [("Objects", "cleanup-async")]) parent [generated]
            result <- try @SomeException (commitAppendOnlyOperationWith dependencies repository config)
            case result of
              Left exception -> assertBool "cleanup cancellation propagates instead of RollbackFailed" ("thread killed" `Text.isInfixOf` Text.pack (show exception))
              Right _ -> assertFailure "expected cancellation"
            exists <- doesFileExist generatedPath
            directoryExists <- doesDirectoryExist generatedDirectory
            assertBool "cleanup cancellation removes owned bytes before propagating" (not exists)
            assertBool "cleanup cancellation removes owned directories before propagating" (not directoryExists)
      , testCase "temporary-index cleanup cancellation removes the temp index then rethrows ThreadKilled" $
          withSeed "adrai transaction temp-index cleanup async" $ \_ repositoryPath repository parentText -> do
            parent <- requireGitOid parentText
            (operationText, generated) <- transactionGeneratedFile parent
            let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
                generatedDirectory = takeDirectory generatedPath
                dependencies = defaultAppendOnlyDependencies { appendOnlyBeforeTempIndexCleanup = throwIO ThreadKilled }
                config = TransactionConfig operationText "adrai: temp-index cleanup async" (Map.fromList [("Objects", "temp-index-cleanup-async")]) parent [generated]
            result <- try @SomeException (commitAppendOnlyOperationWith dependencies repository config)
            case result of
              Left exception -> assertBool "temporary-index cleanup cancellation propagates" ("thread killed" `Text.isInfixOf` Text.pack (show exception))
              Right _ -> assertFailure "expected cancellation"
            indexes <- adraiTemporaryIndexes repositoryPath
            generatedExists <- doesFileExist generatedPath
            generatedDirectoryExists <- doesDirectoryExist generatedDirectory
            assertEqual "temporary-index cleanup cancellation leaves no ADRAI temp index" [] indexes
            assertBool "transaction rollback removes generated bytes after cleanup cancellation" (not generatedExists)
            assertBool "transaction rollback removes generated directories after cleanup cancellation" (not generatedDirectoryExists)
      , testCase "successful CAS refreshes from new commit when HEAD switches branches" $
          withSeed "adrai transaction CAS head switch" $ \_ repositoryPath repository parentText -> do
            let headPath = repositoryPath </> ".git" </> "HEAD"
                unrelatedPath = repositoryPath </> "unrelated.bin"
                stagedBytes = BS.pack [255, 0, 13, 10, 128, 64, 1, 2, 3]
            _ <- gitSuccess repositoryPath ["branch", "other", "HEAD"] BS.empty
            BS.writeFile unrelatedPath stagedBytes
            _ <- gitSuccess repositoryPath ["add", "--", "unrelated.bin"] BS.empty
            stagedEntryBefore <- gitSuccess repositoryPath ["ls-files", "--stage", "--", "unrelated.bin"] BS.empty
            worktreeBytesBefore <- BS.readFile unrelatedPath
            parent <- requireGitOid parentText
            (operationText, generated) <- transactionGeneratedFile parent
            let generatedPathText = Text.unpack (repoPathText (genFilePath generated))
                dependencies = defaultAppendOnlyDependencies { appendOnlyAfterSuccessfulCas = BS.writeFile headPath "ref: refs/heads/other\n" }
                config = TransactionConfig operationText "adrai: CAS head switch" (Map.fromList [("Objects", "cas-head-switch")]) parent [generated]
            result <- commitAppendOnlyOperationWith dependencies repository config
            transaction <- case result of
              Left problem -> assertFailure (show problem) >> fail "unreachable"
              Right success -> pure success
            assertBool "the pinned-CAS caller index refresh succeeded" (transactionIndexUpdated transaction)
            assertEqual "the successful result retains its operation ID" operationText (transactionOperationId transaction)
            assertEqual "the successful result reports its created path" [genFilePath generated] (transactionCreatedPaths transaction)
            (observedCommit, actualParents, actualMessage) <-
              commitIdentityParentAndMessage repositoryPath (Text.unpack (gitOidText (transactionCommitOid transaction)))
            headAfter <- outputText <$> gitSuccess repositoryPath ["symbolic-ref", "--short", "HEAD"] BS.empty
            otherAfter <- outputText <$> gitSuccess repositoryPath ["rev-parse", "refs/heads/other"] BS.empty
            stagedEntryAfter <- gitSuccess repositoryPath ["ls-files", "--stage", "--", "unrelated.bin"] BS.empty
            worktreeBytesAfter <- BS.readFile unrelatedPath
            stagedPaths <- outputText <$> gitSuccess repositoryPath ["diff", "--cached", "--name-only", Text.unpack (gitOidText (transactionCommitOid transaction))] BS.empty
            generatedIndexDiff <- gitSuccess repositoryPath ["diff", "--cached", Text.unpack (gitOidText (transactionCommitOid transaction)), "--", generatedPathText] BS.empty
            generatedWorktreeBytes <- gitSuccess repositoryPath ["show", Text.unpack (gitOidText (transactionCommitOid transaction)) <> ":" <> generatedPathText] BS.empty
            assertEqual "the injected HEAD switch took effect" "other" headAfter
            assertEqual "the other branch was not reset or advanced" parentText otherAfter
            assertEqual "the successful result reports the observed commit" (transactionCommitOid transaction) observedCommit
            assertEqual "unrelated index OID, mode, and stage are unchanged" stagedEntryBefore stagedEntryAfter
            assertEqual "unrelated binary worktree bytes are unchanged" worktreeBytesBefore worktreeBytesAfter
            assertEqual "only the unrelated staged path remains relative to the transaction commit" "unrelated.bin" stagedPaths
            assertEqual "the caller index uses the pinned new commit, not mutable HEAD" BS.empty generatedIndexDiff
            assertEqual "generated bytes are committed" (genFileBytes generated) generatedWorktreeBytes
            assertEqual "append commit supplies exactly the previous HEAD as parent" parentText actualParents
            assertEqual
              "append commit reads the complete message and trailer block from stdin"
              "adrai: CAS head switch\n\nADRAI-Op: O00000000000000000000000042\nADRAI-Objects: cas-head-switch"
              actualMessage
      , testCase "append-only transaction rejects a redirected managed parent before touching repository state" $
          assertRedirectedManagedParent getTransactionSeed "append-only" commitAppendOnlyOperation
      , testCase "bootstrap transaction rejects a redirected managed parent before backup or write" $
          assertRedirectedManagedParent getTransactionSeed "bootstrap" commitBootstrapFiles
      , testCase "append-only transaction re-resolves after parent creation before its write" $
          assertLateRedirectAfterParentCreation
            getTransactionSeed
            "append-only"
            (\repository config redirect ->
                commitAppendOnlyOperationWithHooks
                  defaultAppendOnlyDependencies
                  defaultAppendOnlyTestHooks {appendOnlyAfterGeneratedParentCreationHook = redirect}
                  repository config
            )
      , testCase "bootstrap transaction re-resolves after parent creation before its write" $
          assertLateRedirectAfterParentCreation
            getTransactionSeed
            "bootstrap"
            (\repository config redirect ->
                commitBootstrapFilesWith
                  BootstrapDependencies
                    { bootstrapAfterGeneratedParentCreation = redirect,
                      bootstrapAfterGeneratedFileWrite = \_ -> pure (),
                      bootstrapAfterSuccessfulCasBeforeBookkeeping = pure (),
                      bootstrapInspectCasRef = \_ _ -> pure (Right Nothing),
                      bootstrapBeforeRefUpdate = pure (),
                      bootstrapAfterCasCandidateBeforeUpdateRef = pure (),
                      bootstrapBeforePostCasIndexRefresh = pure (),
                      bootstrapBeforeRollbackCleanup = pure (),
                      bootstrapDeleteGeneratedFile = removeFile
                    }
                  repository
                  config
            )
      , testCase "append-only async failure retains ThreadKilled precedence over synchronous rollback failure" $
          assertAppendRollbackPrecedence getTransactionSeed True
      , testCase "append-only synchronous rollback failure is typed and preserves caller state" $
          assertAppendRollbackPrecedence getTransactionSeed False
      , testCase "bootstrap rollback reports synchronous generated-file delete failure and retains bytes for retry" $
          assertBootstrapRollbackDeleteFailure getTransactionSeed False
      , testCase "bootstrap rollback delete cancellation propagates ThreadKilled and retains bytes for retry" $
          assertBootstrapRollbackDeleteFailure getTransactionSeed True
      , testCase "append rollback hook cancellation wins over a synchronous restore failure" $
          assertAppendHookAsyncWinsRestoreFailure getTransactionSeed
      , testCase "bootstrap rollback hook cancellation wins over a synchronous delete failure" $
          assertBootstrapHookAsyncWinsDeleteFailure getTransactionSeed
      , testCase "append-only re-resolves managed destinations after temporary index before CAS" $
          assertPreCasRedirect
            getTransactionSeed
            "append-only"
            (\repository config redirect ->
                commitAppendOnlyOperationWithHooks defaultAppendOnlyDependencies defaultAppendOnlyTestHooks {appendOnlyBeforeRefUpdateHook = redirect} repository config
            )
      , testCase "bootstrap re-resolves managed destinations after temporary index before CAS" $
          assertPreCasRedirect
            getTransactionSeed
            "bootstrap"
            (\repository config redirect ->
                commitBootstrapFilesWith (bootstrapDependencies redirect (pure ())) repository config
            )
      , testCase "append-only preserves authoritative CAS and skips unsafe post-CAS index refresh" $
          assertPostCasRedirect
            getTransactionSeed
            "append-only"
            (\repository config redirect ->
                commitAppendOnlyOperationWithHooks defaultAppendOnlyDependencies defaultAppendOnlyTestHooks {appendOnlyBeforePostCasIndexRefreshHook = redirect} repository config
            )
      , testCase "bootstrap preserves authoritative CAS and skips unsafe post-CAS index refresh" $
          assertPostCasRedirect
            getTransactionSeed
            "bootstrap"
            (\repository config redirect ->
                commitBootstrapFilesWith (bootstrapDependencies (pure ()) redirect) repository config
            )
      , testCase "bootstrap post-CAS ThreadKilled preserves its authoritative commit and releases the lock" $
          assertBootstrapPostCasCancellationAuthority getTransactionSeed
      , testCase "bootstrap post-update-ref ThreadKilled verifies the candidate ref before rollback" $
          assertBootstrapPostUpdateRefCancellationAuthority getTransactionSeed
      , testCase "unborn bootstrap candidate cancellation confirms missing ref then rolls back" $
          assertUnbornCandidateCancellation getUnbornTransactionSeed
      , testCase "unreadable first-commit CAS inspection fails closed without destructive rollback" $
          assertUnreadableFirstCommitInspection getUnbornTransactionSeed
      ]

gitLockTests :: IO TransactionRepositorySeed -> [TestTree]
gitLockTests getSeed =
  [ testCase "persistent stale and noncanonical lock contents are recovered under native ownership" $
      withTransactionRepositoryCopy getSeed "adrai stale Git lock" $ \_ _ repository _ -> do
        let path = repositoryCommonDir repository </> "adrai.lock"
        BS.writeFile path (BS8.pack ("pid=" <> show (maxBound :: Int) <> "\n"))
        recovered <- acquireGitLock repository
        assertEqual "stale recovery retains the canonical path" path (gitLockPath recovered)
        BS.readFile path >>= assertEqual "stale recovery writes the new canonical owner" (BS8.pack ("pid=" <> show (gitLockPid recovered) <> "\n"))
        releaseGitLock recovered
        let malformed = "not a canonical lock\n"
        BS.writeFile path malformed
        malformedRecovered <- acquireGitLock repository
        BS.readFile path >>= assertEqual "unheld malformed bytes are rewritten by their native owner" (BS8.pack ("pid=" <> show (gitLockPid malformedRecovered) <> "\n"))
        releaseGitLock malformedRecovered
        let leadingZero = "pid=0007\n"
        BS.writeFile path leadingZero
        leadingZeroRecovered <- acquireGitLock repository
        BS.readFile path >>= assertEqual "leading-zero PID is rewritten to canonical bytes" (BS8.pack ("pid=" <> show (gitLockPid leadingZeroRecovered) <> "\n"))
        releaseGitLock leadingZeroRecovered
        gitLockStatus repository >>= assertEqual "persistent stale file has no native owner" (Right Nothing),
    testCase "simultaneous persistent-file contenders yield one native owner" $
      withTransactionRepositoryCopy getSeed "adrai stale contender Git lock" $ \_ _ repository _ -> do
        let path = repositoryCommonDir repository </> "adrai.lock"
        BS.writeFile path (BS8.pack ("pid=" <> show (maxBound :: Int) <> "\n"))
        left <- async (try @GitLockError (acquireGitLock repository))
        right <- async (try @GitLockError (acquireGitLock repository))
        outcomes <- sequence [wait left, wait right]
        let acquired = [lock | Right lock <- outcomes]
        assertEqual "only one stale contender becomes owner" 1 (length acquired)
        let contenderFailures = [(heldPath, pid) | Left (LockHeld heldPath pid) <- outcomes]
        assertEqual "exactly one contender is rejected with typed native contention" 1 (length contenderFailures)
        assertEqual "the contended path remains canonical" [path] (map fst contenderFailures)
        winningLock <- case acquired of
          [lock] -> pure lock
          _ -> assertFailure "expected exactly one native lock owner" >> fail "unreachable"
        let winningBytes = BS8.pack ("pid=" <> show (gitLockPid winningLock) <> "\n")
        BS.readFile path >>= assertEqual "the native winner rewrites canonical owner bytes before returning" winningBytes
        gitLockStatus repository >>= \case
          Left (LockHeld observedPath observedPid) -> do
            assertEqual "status observes the native winner's canonical path" path observedPath
            assertEqual "status observes the native winner's canonical PID" (gitLockPid winningLock) observedPid
          other -> assertFailure ("expected held native winner after simultaneous contention, got " <> show other)
        mapM_ releaseGitLock acquired
        gitLockStatus repository >>= assertEqual "released contender leaves no native owner" (Right Nothing),
    testCase "three-field GitLock values cannot release a newer same-process owner" $
      withTransactionRepositoryCopy getSeed "adrai reservation token Git lock" $ \_ _ repository _ -> do
        first <- acquireGitLock repository
        releaseGitLock first
        second <- acquireGitLock repository
        let reconstructed = GitLock (gitLockPath first) (gitLockFd first) (gitLockPid first)
        assertEqual "the exported GitLock constructor remains exactly three fields" first reconstructed
        releaseGitLock reconstructed
        gitLockStatus repository >>= \case
          Left (LockHeld observedPath observedPid) -> do
            assertEqual "the new reservation still owns the canonical path" (gitLockPath second) observedPath
            assertEqual "the new reservation retains its PID" (gitLockPid second) observedPid
          other -> assertFailure ("expected newer reservation to remain held, got " <> show other)
        attempted <- try @GitLockError (acquireGitLock repository)
        assertEqual "the stale three-field value cannot admit another acquirer" (Left (LockHeld (gitLockPath second) (gitLockPid second))) attempted
        releaseGitLock second,
    testCase "concurrent duplicate releases claim one native handle and permit reacquisition" $
      withTransactionRepositoryCopy getSeed "adrai concurrent release Git lock" $ \_ _ repository _ -> do
        lock <- acquireGitLock repository
        ready <- newEmptyMVar
        start <- newEmptyMVar
        workers <-
          mapM
            ( \_ ->
                async $ do
                  putMVar ready ()
                  readMVar start
                  try @SomeException (releaseGitLock lock)
            )
            [1 :: Int .. 32]
        mapM_ (const (takeMVar ready)) [1 :: Int .. 32]
        putMVar start ()
        outcomes <- mapM wait workers
        assertBool "duplicate releasers either claim once or observe an already-closing owner" (all (either (const False) (const True)) outcomes)
        gitLockStatus repository >>= assertEqual "one completed close leaves no held owner" (Right Nothing)
        reacquired <- acquireGitLock repository
        releaseGitLock reacquired,
    testCase "failed status probe close retains its handle until a later status retry" $
      withTransactionRepositoryCopy getSeed "adrai probe cleanup Git lock" $ \_ _ repository _ -> do
        let failingProbeClose =
              GitLockDependencies $ \_ operation ->
                case operation of
                  CloseStatusProbe -> throwIO (userError "injected Git lock probe close failure")
                  _ -> pure ()
        prior <- acquireGitLock repository
        releaseGitLock prior
        failedProbe <- gitLockStatusWith failingProbeClose repository
        case failedProbe of
          Left (LockFailed _ message) ->
            assertBool "the simulated inconclusive CloseHandle is typed" ("injected Git lock probe close failure" `Text.isInfixOf` message)
          other -> assertFailure ("expected typed retained probe failure, got " <> show other)
        blocked <- try @GitLockError (acquireGitLock repository)
        assertEqual "the retained probe still excludes acquisition" (Left (LockHeld (gitLockPath prior) (gitLockPid prior))) blocked
        gitLockStatus repository >>= assertEqual "a later status retry closes the retained probe" (Right Nothing)
        reacquired <- acquireGitLock repository
        releaseGitLock reacquired,
    testCase "async status-probe close retains its reservation and rethrows cancellation" $
      withTransactionRepositoryCopy getSeed "adrai probe cancellation Git lock" $ \_ _ repository _ -> do
        let cancellingProbeClose =
              GitLockDependencies $ \_ operation ->
                case operation of
                  CloseStatusProbe -> throwIO ThreadKilled
                  _ -> pure ()
        prior <- acquireGitLock repository
        releaseGitLock prior
        cancelled <- try @SomeException (gitLockStatusWith cancellingProbeClose repository)
        assertBool "status rethrows asynchronous cancellation rather than LockFailed" ("thread killed" `Text.isInfixOf` Text.pack (show cancelled))
        blocked <- try @GitLockError (acquireGitLock repository)
        assertEqual "the cancelled probe retains exclusion until a retry closes it" (Left (LockHeld (gitLockPath prior) (gitLockPid prior))) blocked
        gitLockStatus repository >>= assertEqual "a normal status retry closes the cancelled probe" (Right Nothing)
        reacquired <- acquireGitLock repository
        releaseGitLock reacquired,
    testCase "owner close failure restores exclusion until a successful retry" $
      withTransactionRepositoryCopy getSeed "adrai owner cleanup Git lock" $ \_ _ repository _ -> do
        firstClose <- newIORef True
        let failOwnerCloseOnce =
              GitLockDependencies $ \_ operation ->
                case operation of
                  CloseOwnerRelease -> do
                    shouldFail <- readIORef firstClose
                    if shouldFail
                      then writeIORef firstClose False >> throwIO (userError "injected Git lock owner close failure")
                      else pure ()
                  _ -> pure ()
        lock <- acquireGitLockWith failOwnerCloseOnce repository
        failedRelease <- try @GitLockError (releaseGitLock lock)
        case failedRelease of
          Left (LockFailed _ message) ->
            assertBool "owner cleanup failure is surfaced as typed failure" ("injected Git lock owner close failure" `Text.isInfixOf` message)
          other -> assertFailure ("expected owner close failure, got " <> show other)
        blocked <- try @GitLockError (acquireGitLock repository)
        assertEqual "failed owner cleanup retains the same-process exclusion" (Left (LockHeld (gitLockPath lock) (gitLockPid lock))) blocked
        releaseGitLock lock
        gitLockStatus repository >>= assertEqual "successful retry removes the restored owner" (Right Nothing)
        reacquired <- acquireGitLock repository
        releaseGitLock reacquired,
    testCase "withGitLock owner pre-close failure preserves action precedence and permits retry" $
      withTransactionRepositoryCopy getSeed "adrai withGitLock cleanup precedence" $ \_ _ repository _ -> do
        let runCase label action expected = do
              failClose <- newIORef True
              capturedLock <- newIORef Nothing
              let dependencies =
                    GitLockDependencies $ \_ operation ->
                      case operation of
                        CloseOwnerRelease -> do
                          shouldFail <- readIORef failClose
                          if shouldFail
                            then throwIO (userError ("injected " <> label <> " cleanup failure"))
                            else pure ()
                        _ -> pure ()
              outcome <-
                try @SomeException $
                  withGitLockWith dependencies repository $ \lock -> do
                    writeIORef capturedLock (Just lock)
                    action
              assertBool (label <> " has the required visible failure precedence") (expected `Text.isInfixOf` Text.pack (show outcome))
              lock <- readIORef capturedLock >>= \case
                Just held -> pure held
                Nothing -> assertFailure (label <> " did not expose the acquired public lock to its test action") >> fail "unreachable"
              blocked <- try @GitLockError (acquireGitLock repository)
              assertEqual (label <> " retains owner exclusion after failed cleanup") (Left (LockHeld (gitLockPath lock) (gitLockPid lock))) blocked
              writeIORef failClose False
              releaseGitLock lock
              gitLockStatus repository >>= assertEqual (label <> " retry closes the retained owner") (Right Nothing)
              reacquired <- acquireGitLock repository
              releaseGitLock reacquired
        runCase "successful action" (pure ()) "injected successful action cleanup failure"
        runCase "synchronous action" (throwIO (userError "synchronous action failure") :: IO ()) "synchronous action failure"
        runCase "asynchronous action" (throwIO ThreadKilled :: IO ()) "thread killed",
    testCase "withGitLock cleans up after synchronous and asynchronous actions" $
      withTransactionRepositoryCopy getSeed "adrai Git lock cleanup" $ \_ _ repository _ -> do
        let path = repositoryCommonDir repository </> "adrai.lock"
        synchronous <- try @SomeException (withGitLock repository (throwIO (userError "synchronous lock action") :: IO ()))
        assertBool "synchronous action exception propagates" ("synchronous lock action" `Text.isInfixOf` Text.pack (show synchronous))
        doesFileExist path >>= assertEqual "synchronous cleanup preserves the canonical lock file" True
        gitLockStatus repository >>= assertEqual "synchronous cleanup releases native ownership" (Right Nothing)
        asynchronous <- try @SomeException (withGitLock repository (throwIO ThreadKilled :: IO ()))
        assertBool "asynchronous cancellation propagates" ("thread killed" `Text.isInfixOf` Text.pack (show asynchronous))
        doesFileExist path >>= assertEqual "asynchronous cleanup preserves the canonical lock file" True
        gitLockStatus repository >>= assertEqual "asynchronous cleanup releases native ownership" (Right Nothing)
  ]

adraiTemporaryIndexes :: FilePath -> IO [FilePath]
adraiTemporaryIndexes repositoryPath =
  filter ("adrai-index-" `isPrefixOf`) <$> listDirectory (repositoryPath </> ".git")

assertRedirectedManagedParent
  :: IO TransactionRepositorySeed
  -> String
  -> (Repository -> TransactionConfig -> IO (Either TransactionError TransactionResult))
  -> IO ()
assertRedirectedManagedParent getSeed label runTransaction =
  withTransactionRepositoryCopy getSeed ("adrai transaction redirected " <> label) $ \temporary repositoryPath repository parentText -> do
    let outsidePath = temporary </> "outside"
        redirectedParent = repositoryPath </> "architecture" </> "adrai"
    createDirectory (repositoryPath </> "architecture")
    createDirectory outsidePath
    createDirectoryRedirect outsidePath redirectedParent
    parent <- requireGitOid parentText
    (operationText, generated) <- transactionGeneratedFile parent
    let config = TransactionConfig operationText "adrai: redirected managed parent" (Map.fromList [("Objects", "redirected-parent")]) parent [generated]
    before <- transactionObservableState repositoryPath
    result <- runTransaction repository config
    case result of
      Left (Stage5ValidateGenerated message) ->
        assertBool (label <> " returns the managed-path typed validation failure") ("ManagedPathRedirected" `Text.isInfixOf` message)
      other -> assertFailure (label <> " expected Stage5ValidateGenerated ManagedPathRedirected, got " <> show other)
    after <- transactionObservableState repositoryPath
    assertEqual (label <> " leaves HEAD, refs, index, status, and reflogs unchanged") before after
    listDirectory outsidePath >>= assertEqual (label <> " creates no outside generated files or directories") []

-- | Force the managed ancestor to become a link exactly after the transaction
-- has created its parents, but before it performs its final write-time path
-- resolution.  The test-owned link is removed before the observable-state
-- comparison: containment must reject it without writing outside the repo or
-- mutating Git state.
assertLateRedirectAfterParentCreation
  :: IO TransactionRepositorySeed
  -> String
  -> (Repository -> TransactionConfig -> (GeneratedFile -> IO ()) -> IO (Either TransactionError TransactionResult))
  -> IO ()
assertLateRedirectAfterParentCreation getSeed label runTransaction =
  withTransactionRepositoryCopy getSeed ("adrai transaction late redirect " <> label) $ \temporary repositoryPath repository parentText -> do
    let outsidePath = temporary </> "outside"
        managedParent = repositoryPath </> "architecture" </> "adrai"
        architectureParent = repositoryPath </> "architecture"
        redirect GeneratedFile{} = do
          removeDirectoryRecursive managedParent
          createDirectoryRedirect outsidePath managedParent
    createDirectory outsidePath
    parent <- requireGitOid parentText
    (operationText, generated) <- transactionGeneratedFile parent
    let config = TransactionConfig operationText "adrai: late managed redirect" (Map.fromList [("Objects", "late-redirect")]) parent [generated]
    before <- transactionObservableState repositoryPath
    result <- runTransaction repository config redirect
    case result of
      Left problem ->
        assertBool (label <> " returns a typed containment-aware failure") ("ManagedPathRedirected" `Text.isInfixOf` Text.pack (show problem))
      Right success -> assertFailure (label <> " unexpectedly committed after a late redirect: " <> show success)
    listDirectory outsidePath >>= assertEqual (label <> " writes no managed bytes through the redirect") []
    _ <- tryIOError (removeDirectory managedParent)
    _ <- tryIOError (removeDirectory architectureParent)
    after <- transactionObservableState repositoryPath
    assertEqual (label <> " leaves HEAD, refs, index, status, and reflogs unchanged after link removal") before after

assertAppendRollbackPrecedence :: IO TransactionRepositorySeed -> Bool -> IO ()
assertAppendRollbackPrecedence getSeed originalIsAsync =
  withTransactionRepositoryCopy getSeed "adrai transaction rollback precedence" $ \_ repositoryPath repository parentText -> do
    parent <- requireGitOid parentText
    (operationText, generated) <- transactionGeneratedFile parent
    let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
        generatedDirectory = takeDirectory generatedPath
        actionFailure
          | originalIsAsync = throwIO ThreadKilled
          | otherwise = throwIO (Stage7CommitTree "injected synchronous action failure")
        dependencies =
          defaultAppendOnlyDependencies
            { appendOnlyAfterGeneratedWrite = actionFailure,
              appendOnlyBeforeRollbackCleanup = throwIO (Stage5ValidateGenerated "injected synchronous cleanup failure")
            }
        config = TransactionConfig operationText "adrai: rollback precedence" (Map.fromList [("Objects", "rollback-precedence")]) parent [generated]
    before <- transactionObservableState repositoryPath
    result <- try @SomeException (commitAppendOnlyOperationWith dependencies repository config)
    if originalIsAsync
      then
        case result of
          Left exception -> assertBool "original ThreadKilled wins over synchronous rollback failure" ("thread killed" `Text.isInfixOf` Text.pack (show exception))
          Right _ -> assertFailure "expected ThreadKilled"
      else
        case result of
          Right (Left (RollbackFailed detail)) -> assertBool "synchronous cleanup failure is reported with deterministic precedence" ("cleanup hook failed" `Text.isInfixOf` detail)
          Right other -> assertFailure ("expected typed rollback failure, got " <> show other)
          Left exception -> assertFailure ("synchronous cleanup failure escaped the typed API: " <> show exception)
    exists <- doesFileExist generatedPath
    directoryExists <- doesDirectoryExist generatedDirectory
    after <- transactionObservableState repositoryPath
    assertBool "rollback removes generated bytes despite its synchronous hook failure" (not exists)
    assertBool "rollback removes generated directories despite its synchronous hook failure" (not directoryExists)
    assertEqual "rollback precedence leaves caller Git state unchanged" before after

assertBootstrapRollbackDeleteFailure :: IO TransactionRepositorySeed -> Bool -> IO ()
assertBootstrapRollbackDeleteFailure getSeed deleteIsAsync =
  withTransactionRepositoryCopy getSeed "adrai bootstrap rollback delete" $ \_ repositoryPath repository parentText -> do
    parent <- requireGitOid parentText
    (operationText, generated) <- transactionGeneratedFile parent
    let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
        deleteGenerated _
          | deleteIsAsync = throwIO ThreadKilled
          | otherwise = throwIO (userError "injected bootstrap delete failure")
        dependencies =
          BootstrapDependencies
            { bootstrapAfterGeneratedParentCreation = \_ -> pure (),
              bootstrapAfterGeneratedFileWrite = \_ -> throwIO (Stage7CommitTree "injected bootstrap action failure"),
              bootstrapAfterSuccessfulCasBeforeBookkeeping = pure (),
              bootstrapInspectCasRef = \_ _ -> pure (Right Nothing),
              bootstrapBeforeRefUpdate = pure (),
              bootstrapAfterCasCandidateBeforeUpdateRef = pure (),
              bootstrapBeforePostCasIndexRefresh = pure (),
              bootstrapBeforeRollbackCleanup = pure (),
              bootstrapDeleteGeneratedFile = deleteGenerated
            }
        config = TransactionConfig operationText "adrai: bootstrap rollback delete" (Map.fromList [("Objects", "bootstrap-rollback-delete")]) parent [generated]
    before <- transactionObservableState repositoryPath
    result <- try @SomeException (commitBootstrapFilesWith dependencies repository config)
    if deleteIsAsync
      then
        case result of
          Left exception -> assertBool "delete cancellation propagates rather than becoming a typed result" ("thread killed" `Text.isInfixOf` Text.pack (show exception))
          Right other -> assertFailure ("expected delete cancellation, got " <> show other)
      else
        case result of
          Right (Left (RollbackFailed detail)) -> do
            assertBool "delete failure is reported by the typed API" ("bootstrap rollback delete failed" `Text.isInfixOf` detail)
            assertBool "original action remains in deterministic rollback context" ("injected bootstrap action failure" `Text.isInfixOf` detail)
          Right other -> assertFailure ("expected typed bootstrap rollback failure, got " <> show other)
          Left exception -> assertFailure ("synchronous delete failure escaped the typed API: " <> show exception)
    retainedBytes <- BS.readFile generatedPath
    afterFailure <- transactionObservableState repositoryPath
    let (refsBefore, indexBefore, _, reflogsBefore) = before
        (refsAfter, indexAfter, _, reflogsAfter) = afterFailure
    assertEqual "failed bootstrap delete retains precisely the transaction-created bytes" (genFileBytes generated) retainedBytes
    assertEqual "failed bootstrap delete leaves HEAD and refs unchanged" refsBefore refsAfter
    assertEqual "failed bootstrap delete leaves index unchanged" indexBefore indexAfter
    assertEqual "failed bootstrap delete leaves reflogs unchanged" reflogsBefore reflogsAfter
    retry <- commitBootstrapFiles repository config
    case retry of
      Left problem -> assertFailure ("default cleanup retry should recover the retained bytes: " <> show problem)
      Right success -> assertEqual "retry creates the requested managed path" [genFilePath generated] (transactionCreatedPaths success)

assertAppendHookAsyncWinsRestoreFailure :: IO TransactionRepositorySeed -> IO ()
assertAppendHookAsyncWinsRestoreFailure getSeed =
  withTransactionRepositoryCopy getSeed "adrai append rollback async ordering" $ \_ repositoryPath repository parentText -> do
    parent <- requireGitOid parentText
    (operationText, generated) <- transactionGeneratedFile parent
    let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
        callerBytes = "caller-owned bytes\n"
        dependencies =
          defaultAppendOnlyDependencies
            { appendOnlyAfterGeneratedWrite = BS.writeFile generatedPath callerBytes >> throwIO (Stage7CommitTree "injected append action failure"),
              appendOnlyBeforeRollbackCleanup = throwIO ThreadKilled
            }
        config = TransactionConfig operationText "adrai: append rollback async ordering" (Map.fromList [("Objects", "append-rollback-async-ordering")]) parent [generated]
    before <- transactionObservableState repositoryPath
    result <- try @SomeException (commitAppendOnlyOperationWith dependencies repository config)
    case result of
      Left exception -> assertBool "hook ThreadKilled wins over synchronous restore failure" ("thread killed" `Text.isInfixOf` Text.pack (show exception))
      Right other -> assertFailure ("expected ThreadKilled, got " <> show other)
    retained <- BS.readFile generatedPath
    afterFailure <- transactionObservableState repositoryPath
    let (refsBefore, indexBefore, _, reflogsBefore) = before
        (refsAfter, indexAfter, _, reflogsAfter) = afterFailure
    assertEqual "rollback does not delete caller-replaced bytes" callerBytes retained
    assertEqual "append rollback leaves HEAD and refs unchanged" refsBefore refsAfter
    assertEqual "append rollback leaves index unchanged" indexBefore indexAfter
    assertEqual "append rollback leaves reflogs unchanged" reflogsBefore reflogsAfter
    removeFile generatedPath
    retry <- commitAppendOnlyOperation repository config
    case retry of
      Left problem -> assertFailure ("retry after removing caller-owned bytes should succeed: " <> show problem)
      Right success -> assertEqual "retry creates the requested managed path" [genFilePath generated] (transactionCreatedPaths success)

assertBootstrapHookAsyncWinsDeleteFailure :: IO TransactionRepositorySeed -> IO ()
assertBootstrapHookAsyncWinsDeleteFailure getSeed =
  withTransactionRepositoryCopy getSeed "adrai bootstrap rollback async ordering" $ \_ repositoryPath repository parentText -> do
    parent <- requireGitOid parentText
    (operationText, generated) <- transactionGeneratedFile parent
    let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
        dependencies =
          BootstrapDependencies
            { bootstrapAfterGeneratedParentCreation = \_ -> pure (),
              bootstrapAfterGeneratedFileWrite = \_ -> throwIO (Stage7CommitTree "injected bootstrap action failure"),
              bootstrapAfterSuccessfulCasBeforeBookkeeping = pure (),
              bootstrapInspectCasRef = \_ _ -> pure (Right Nothing),
              bootstrapBeforeRefUpdate = pure (),
              bootstrapAfterCasCandidateBeforeUpdateRef = pure (),
              bootstrapBeforePostCasIndexRefresh = pure (),
              bootstrapBeforeRollbackCleanup = throwIO ThreadKilled,
              bootstrapDeleteGeneratedFile = \_ -> throwIO (userError "injected synchronous bootstrap delete failure")
            }
        config = TransactionConfig operationText "adrai: bootstrap rollback async ordering" (Map.fromList [("Objects", "bootstrap-rollback-async-ordering")]) parent [generated]
    before <- transactionObservableState repositoryPath
    result <- try @SomeException (commitBootstrapFilesWith dependencies repository config)
    case result of
      Left exception -> assertBool "hook ThreadKilled wins over synchronous delete failure" ("thread killed" `Text.isInfixOf` Text.pack (show exception))
      Right other -> assertFailure ("expected ThreadKilled, got " <> show other)
    retained <- BS.readFile generatedPath
    afterFailure <- transactionObservableState repositoryPath
    let (refsBefore, indexBefore, _, reflogsBefore) = before
        (refsAfter, indexAfter, _, reflogsAfter) = afterFailure
    assertEqual "failed delete retains exactly transaction-created bytes" (genFileBytes generated) retained
    assertEqual "bootstrap rollback leaves HEAD and refs unchanged" refsBefore refsAfter
    assertEqual "bootstrap rollback leaves index unchanged" indexBefore indexAfter
    assertEqual "bootstrap rollback leaves reflogs unchanged" reflogsBefore reflogsAfter
    retry <- commitBootstrapFiles repository config
    case retry of
      Left problem -> assertFailure ("default bootstrap retry should recover retained bytes: " <> show problem)
      Right success -> assertEqual "retry creates the requested managed path" [genFilePath generated] (transactionCreatedPaths success)

bootstrapDependencies :: IO () -> IO () -> BootstrapDependencies
bootstrapDependencies beforeRefUpdate beforeRefresh =
  defaultBootstrapDependencies
    { bootstrapBeforeRefUpdate = beforeRefUpdate,
      bootstrapBeforePostCasIndexRefresh = beforeRefresh
    }

assertPreCasRedirect
  :: IO TransactionRepositorySeed
  -> String
  -> (Repository -> TransactionConfig -> IO () -> IO (Either TransactionError TransactionResult))
  -> IO ()
assertPreCasRedirect getSeed label runTransaction =
  withTransactionRepositoryCopy getSeed ("adrai pre-CAS redirect " <> label) $ \temporary repositoryPath repository parentText -> do
    let outsidePath = temporary </> "outside"
        redirectedParent = repositoryPath </> "architecture" </> "adrai"
        architectureParent = repositoryPath </> "architecture"
        redirect = do
          removeDirectoryRecursive redirectedParent
          createDirectoryRedirect outsidePath redirectedParent
    createDirectory outsidePath
    parent <- requireGitOid parentText
    (operationText, generated) <- transactionGeneratedFile parent
    let config = TransactionConfig operationText "adrai: pre-CAS containment" (Map.fromList [("Objects", "pre-cas-containment")]) parent [generated]
    before <- transactionObservableState repositoryPath
    result <- runTransaction repository config redirect
    case result of
      Left problem -> assertBool (label <> " returns a typed containment/rollback failure") ("ManagedPathRedirected" `Text.isInfixOf` Text.pack (show problem))
      Right success -> assertFailure (label <> " unexpectedly published a ref: " <> show success)
    listDirectory outsidePath >>= assertEqual (label <> " writes no bytes through the pre-CAS redirect") []
    _ <- tryIOError (removeDirectory redirectedParent)
    _ <- tryIOError (removeDirectory architectureParent)
    after <- transactionObservableState repositoryPath
    assertEqual (label <> " publishes no ref or index/worktree/reflog mutation before CAS") before after

assertPostCasRedirect
  :: IO TransactionRepositorySeed
  -> String
  -> (Repository -> TransactionConfig -> IO () -> IO (Either TransactionError TransactionResult))
  -> IO ()
assertPostCasRedirect getSeed label runTransaction =
  withTransactionRepositoryCopy getSeed ("adrai post-CAS redirect " <> label) $ \temporary repositoryPath repository parentText -> do
    let outsidePath = temporary </> "outside"
        redirectedParent = repositoryPath </> "architecture" </> "adrai"
        architectureParent = repositoryPath </> "architecture"
        stagedPath = repositoryPath </> "caller-post-cas.bin"
        stagedBytes = BS.pack [0, 255, 17, 10, 128, 64, 3, 2, 1]
        redirect = do
          removeDirectoryRecursive redirectedParent
          createDirectoryRedirect outsidePath redirectedParent
    BS.writeFile stagedPath stagedBytes
    _ <- gitSuccess repositoryPath ["add", "--", "caller-post-cas.bin"] BS.empty
    createDirectory outsidePath
    indexPath <- pure (repositoryGitDir repository </> "index")
    indexBeforeExists <- doesFileExist indexPath
    indexBefore <- BS.readFile indexPath
    parent <- requireGitOid parentText
    (operationText, generated) <- transactionGeneratedFile parent
    let config = TransactionConfig operationText "adrai: post-CAS containment" (Map.fromList [("Objects", "post-cas-containment")]) parent [generated]
    result <- runTransaction repository config redirect
    success <- case result of
      Left problem -> assertFailure (label <> " must retain its authoritative CAS: " <> show problem) >> fail "unreachable"
      Right committed -> pure committed
    headAfter <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD"] BS.empty
    indexAfterExists <- doesFileExist indexPath
    indexAfter <- BS.readFile indexPath
    stagedBytesAfter <- BS.readFile stagedPath
    assertEqual (label <> " reports skipped unsafe refresh") False (transactionIndexUpdated success)
    assertEqual (label <> " keeps the published commit authoritative") (gitOidText (transactionCommitOid success)) headAfter
    assertBool (label <> " advances the ref despite refresh containment failure") (headAfter /= parentText)
    assertEqual (label <> " retains the caller index file after the post-CAS redirect") indexBeforeExists indexAfterExists
    assertEqual (label <> " preserves the raw staged caller index after the post-CAS redirect") indexBefore indexAfter
    assertEqual (label <> " preserves staged binary caller bytes after the post-CAS redirect") stagedBytes stagedBytesAfter
    listDirectory outsidePath >>= assertEqual (label <> " writes no bytes through the post-CAS redirect") []
    _ <- tryIOError (removeDirectory redirectedParent)
    _ <- tryIOError (removeDirectory architectureParent)
    pure ()

assertBootstrapPostCasCancellationAuthority :: IO TransactionRepositorySeed -> IO ()
assertBootstrapPostCasCancellationAuthority getSeed =
  assertBootstrapCancellationAuthority
    getSeed
    "post-CAS"
    (\dependencies -> dependencies {bootstrapBeforePostCasIndexRefresh = throwIO ThreadKilled})

assertBootstrapPostUpdateRefCancellationAuthority :: IO TransactionRepositorySeed -> IO ()
assertBootstrapPostUpdateRefCancellationAuthority getSeed =
  assertBootstrapCancellationAuthority
    getSeed
    "post-update-ref"
    (\dependencies -> dependencies {bootstrapAfterSuccessfulCasBeforeBookkeeping = throwIO ThreadKilled})

assertBootstrapCancellationAuthority :: IO TransactionRepositorySeed -> String -> (BootstrapDependencies -> BootstrapDependencies) -> IO ()
assertBootstrapCancellationAuthority getSeed label adjustDependencies =
  withTransactionRepositoryCopy getSeed ("adrai bootstrap " <> label <> " cancellation") $ \_ repositoryPath repository parentText -> do
    let stagedPath = repositoryPath </> "caller-bootstrap-cancellation.bin"
        stagedBytes = BS.pack [255, 0, 13, 10, 128, 64, 9, 8, 7]
    BS.writeFile stagedPath stagedBytes
    _ <- gitSuccess repositoryPath ["add", "--", "caller-bootstrap-cancellation.bin"] BS.empty
    indexPath <- pure (repositoryGitDir repository </> "index")
    indexBeforeExists <- doesFileExist indexPath
    indexBefore <- BS.readFile indexPath
    parent <- requireGitOid parentText
    (operationText, generated) <- transactionGeneratedFile parent
    let config = TransactionConfig operationText ("adrai: bootstrap " <> Text.pack label <> " cancellation") (Map.fromList [("Objects", "bootstrap-" <> label <> "-cancellation")]) parent [generated]
        dependencies = adjustDependencies (bootstrapDependencies (pure ()) (pure ()))
    result <- try @SomeException (commitBootstrapFilesWith dependencies repository config)
    case result of
      Left exception -> assertBool (label <> " cancellation propagates after lock release") ("thread killed" `Text.isInfixOf` Text.pack (show exception))
      Right other -> assertFailure ("expected " <> label <> " ThreadKilled, got " <> show other)
    headAfter <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD"] BS.empty
    indexAfterExists <- doesFileExist indexPath
    indexAfter <- BS.readFile indexPath
    stagedBytesAfter <- BS.readFile stagedPath
    committedBytes <- gitSuccess repositoryPath ["show", Text.unpack headAfter <> ":" <> Text.unpack (repoPathText (genFilePath generated))] BS.empty
    worktreeBytes <- BS.readFile (repositoryPath </> Text.unpack (repoPathText (genFilePath generated)))
    assertBool "CAS remains authoritative after cancellation" (headAfter /= parentText)
    assertEqual "committed managed bytes are preserved" (genFileBytes generated) committedBytes
    assertEqual "worktree managed bytes are preserved" (genFileBytes generated) worktreeBytes
    assertEqual "post-CAS cancellation retains the caller index file" indexBeforeExists indexAfterExists
    assertEqual "post-CAS cancellation preserves the raw staged caller index" indexBefore indexAfter
    assertEqual "post-CAS cancellation preserves staged binary caller bytes" stagedBytes stagedBytesAfter
    gitLockStatus repository >>= assertEqual "post-CAS cancellation releases the native lock" (Right Nothing)
    reacquired <- acquireGitLock repository
    releaseGitLock reacquired
    _ <- gitSuccess repositoryPath ["show", "--stat", "HEAD"] BS.empty
    pure ()

assertUnbornCandidateCancellation :: IO UnbornTransactionRepositorySeed -> IO ()
assertUnbornCandidateCancellation getSeed =
  withUnbornTransactionRepositoryCopy getSeed "adrai unborn candidate cancellation" $ \_ repositoryPath repository -> do
    (operationText, generated) <- transactionGeneratedFile nullOid
    let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
        config = TransactionConfig operationText "adrai: unborn candidate cancellation" (Map.fromList [("Objects", "unborn-candidate-cancellation")]) nullOid [generated]
        dependencies = (bootstrapDependencies (pure ()) (pure ())) {bootstrapAfterCasCandidateBeforeUpdateRef = throwIO ThreadKilled}
    result <- try @SomeException (commitBootstrapFilesWith dependencies repository config)
    case result of
      Left exception -> assertBool "candidate cancellation propagates after confirmed unborn absence" ("thread killed" `Text.isInfixOf` Text.pack (show exception))
      Right other -> assertFailure ("expected unborn candidate cancellation, got " <> show other)
    generatedExists <- doesFileExist generatedPath
    mainRefExists <- doesFileExist (repositoryGitDir repository </> "refs" </> "heads" </> "main")
    assertBool "confirmed missing unborn ref publishes no first commit" (not mainRefExists)
    assertBool "confirmed missing unborn ref rolls back generated bytes" (not generatedExists)

assertUnreadableFirstCommitInspection :: IO UnbornTransactionRepositorySeed -> IO ()
assertUnreadableFirstCommitInspection getSeed =
  withUnbornTransactionRepositoryCopy getSeed "adrai unreadable first commit inspection" $ \_ repositoryPath repository -> do
    indexBefore <- doesFileExist (repositoryGitDir repository </> "index")
    (operationText, generated) <- transactionGeneratedFile nullOid
    let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
        config = TransactionConfig operationText "adrai: unreadable first commit inspection" (Map.fromList [("Objects", "unreadable-first-commit-inspection")]) nullOid [generated]
        dependencies =
          (bootstrapDependencies (pure ()) (pure ()))
            { bootstrapAfterSuccessfulCasBeforeBookkeeping = throwIO ThreadKilled,
              bootstrapInspectCasRef = \_ _ -> pure (Left (Stage8UpdateRef "injected unreadable CAS inspection"))
            }
    result <- commitBootstrapFilesWith dependencies repository config
    case result of
      Left (Stage8UpdateRef detail) -> assertBool "unreadable inspection returns typed fail-closed error" ("unreadable CAS inspection" `Text.isInfixOf` detail)
      other -> assertFailure ("expected fail-closed Stage8 error, got " <> show other)
    (headCommit, actualParents, actualMessage) <- commitIdentityParentAndMessage repositoryPath "HEAD"
    let headAfter = gitOidText headCommit
    committedBytes <- gitSuccess repositoryPath ["show", Text.unpack headAfter <> ":" <> Text.unpack (repoPathText (genFilePath generated))] BS.empty
    worktreeBytes <- BS.readFile generatedPath
    indexAfter <- doesFileExist (repositoryGitDir repository </> "index")
    assertEqual "uncertain authority preserves first-commit bytes" (genFileBytes generated) committedBytes
    assertEqual "uncertain authority preserves worktree bytes" (genFileBytes generated) worktreeBytes
    assertEqual "uncertain authority does not materialize or refresh caller index" indexBefore indexAfter
    assertEqual "bootstrap commit supplies no parent for the null OID" "" actualParents
    assertEqual
      "bootstrap commit reads the complete message and trailer block from stdin"
      "adrai: unreadable first commit inspection\n\nADRAI-Op: O00000000000000000000000042\nADRAI-Objects: bootstrap"
      actualMessage

transactionObservableState :: FilePath -> IO (BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString)
transactionObservableState repositoryPath = do
  refs <- gitSuccess repositoryPath ["show-ref", "--head"] BS.empty
  index <- gitSuccess repositoryPath ["ls-files", "--stage", "-z"] BS.empty
  status <- gitSuccess repositoryPath ["--no-optional-locks", "status", "--porcelain=v1", "--untracked-files=all", "-z"] BS.empty
  reflogs <- gitSuccess repositoryPath ["reflog", "show", "--all", "--format=%H%x00%gs"] BS.empty
  pure (refs, index, status, reflogs)

createDirectoryRedirect :: FilePath -> FilePath -> IO ()
createDirectoryRedirect target link
  | os == "mingw32" = do
      result <- runProcess (shell ("mklink /J \"" <> link <> "\" \"" <> target <> "\""))
      case result of
        Exit.ExitSuccess -> pure ()
        Exit.ExitFailure code -> assertFailure ("failed to create Windows junction, exit " <> show code)
  | otherwise = do
      result <- tryIOError (createDirectoryLink target link)
      case result of
        Right () -> pure ()
        Left problem -> assertFailure ("failed to create directory symlink: " <> show problem)

transactionGeneratedFile :: GitOid -> IO (String, GeneratedFile)
transactionGeneratedFile basis = do
  adr <- requireRight (mkAdrId "A00000000000000000000000042")
  record <- requireRight (mkRecordId "R00000000000000000000000042")
  operation <- requireRight (mkOperationId operationText)
  actor <- requireRight (mkActor HumanActor "transaction-test" Nothing)
  event <- requireRight (mkEventKind "decision.create")
  domain <- requireRight (mkDomain "transaction.index")
  let decision =
        DecisionRecord
          { decisionAdr = adr,
            decisionRecord = record,
            decisionTitle = "Transaction index isolation",
            decisionSummary = "Preserve unrelated staged entries.",
            decisionDomains = [domain],
            decisionBody = "## Decision\nUse an isolated temporary Git index.\n"
          }
      managed = ManagedDecision decision
  semantic <- requireRight (renderDecisionSemantic decision)
  capsule <-
    requireRight . mkProvenanceCapsule $
      ProvenanceCapsuleInput
        { capsuleInputOperationId = operation,
          capsuleInputObjectId = ProvenanceRecord record,
          capsuleInputEventKind = event,
          capsuleInputActor = actor,
          capsuleInputTimestampMs = 1700000000000,
          capsuleInputBasis = basis,
          capsuleInputParents = [],
          capsuleInputBranchHint = Nothing,
          capsuleInputUpstreamHint = Nothing,
          capsuleInputLineAnchors = [],
          capsuleInputSemanticDigest = semanticDigest semantic,
          capsuleInputToolVersion = "adrai/1.0.0",
          capsuleInputDigests = ProvenanceInputs Nothing Nothing Nothing
        }
  bytes <- requireRight (sealManagedDocument managed capsule)
  path <- requireRight (canonicalManagedPath (configManagedPaths defaultConfig) managed)
  pure (Text.unpack operationText, GeneratedFile path bytes)
  where
    operationText = "O00000000000000000000000042"

commitIdentityParentAndMessage :: FilePath -> String -> IO (GitOid, Text.Text, Text.Text)
commitIdentityParentAndMessage repositoryPath revision = do
  metadata <-
    outputText
      <$> gitSuccess
        repositoryPath
        ["show", "-s", "--format=%H%x00%P%x00%B", revision]
        BS.empty
  let (commitText, separatorAndRemainder) = Text.breakOn "\NUL" metadata
      (parents, separatorAndMessage) = Text.breakOn "\NUL" (Text.drop 1 separatorAndRemainder)
  assertBool "commit metadata observation contains its identity/parent separator" (not (Text.null separatorAndRemainder))
  assertBool "commit metadata observation contains its parent/message separator" (not (Text.null separatorAndMessage))
  pure (GitOid commitText, parents, Text.drop 1 separatorAndMessage)

commitHeadAndTree :: FilePath -> IO (Text.Text, Text.Text)
commitHeadAndTree repositoryPath = do
  metadata <- outputText <$> gitSuccess repositoryPath ["show", "-s", "--format=%H%x00%T", "HEAD"] BS.empty
  let (headOid, separatorAndTree) = Text.breakOn "\NUL" metadata
  assertBool "commit identity observation contains its HEAD/tree separator" (not (Text.null separatorAndTree))
  pure (headOid, Text.drop 1 separatorAndTree)

data TransactionRepositorySeed = TransactionRepositorySeed
  { transactionCopySeed :: RepositorySeed,
    transactionSeedPath :: FilePath,
    transactionSeedRepository :: Repository,
    transactionSeedParent :: Text.Text
  }

createTransactionRepositorySeed :: IO TransactionRepositorySeed
createTransactionRepositorySeed = do
  (copySeed, (seedPath, seedRepository, seedParent)) <-
    createRepositorySeedWith "adrai-transaction-seed" $ \repositoryPath -> do
      initTestRepository repositoryPath
      _ <- gitSuccess repositoryPath ["config", "core.hooksPath", ".git/adrai-no-hooks"] BS.empty
      parentText <- commitFile repositoryPath "seed.txt" "seed\n"
      repository <-
        discoverRepository systemGit repositoryPath >>= \case
          Left problem -> assertFailure (show problem) >> fail "unreachable"
          Right discovered -> pure discovered
      assertMainWorktreeRepositoryIdentity "committed seed discovery" repositoryPath repository
      pure (repositoryPath, repository, parentText)
  pure
    TransactionRepositorySeed
      { transactionCopySeed = copySeed,
        transactionSeedPath = seedPath,
        transactionSeedRepository = seedRepository,
        transactionSeedParent = seedParent
      }

data UnbornTransactionRepositorySeed = UnbornTransactionRepositorySeed
  { unbornTransactionCopySeed :: RepositorySeed,
    unbornTransactionSeedPath :: FilePath,
    unbornTransactionSeedRepository :: Repository
  }

createUnbornTransactionRepositorySeed :: IO UnbornTransactionRepositorySeed
createUnbornTransactionRepositorySeed = do
  (copySeed, (seedPath, seedRepository)) <-
    createRepositorySeedWith "adrai-unborn-transaction-seed" $ \repositoryPath -> do
      initTestRepository repositoryPath
      _ <- gitSuccess repositoryPath ["config", "core.hooksPath", ".git/adrai-no-hooks"] BS.empty
      repository <-
        discoverRepository systemGit repositoryPath >>= \case
          Left problem -> assertFailure (show problem) >> fail "unreachable"
          Right discovered -> pure discovered
      assertMainWorktreeRepositoryIdentity "unborn seed discovery" repositoryPath repository
      pure (repositoryPath, repository)
  pure
    UnbornTransactionRepositorySeed
      { unbornTransactionCopySeed = copySeed,
        unbornTransactionSeedPath = seedPath,
        unbornTransactionSeedRepository = seedRepository
      }

removeUnbornTransactionRepositorySeed :: UnbornTransactionRepositorySeed -> IO ()
removeUnbornTransactionRepositorySeed = removeRepositorySeed . unbornTransactionCopySeed

removeTransactionRepositorySeed :: TransactionRepositorySeed -> IO ()
removeTransactionRepositorySeed = removeRepositorySeed . transactionCopySeed

withTransactionRepositoryCopy ::
  IO TransactionRepositorySeed ->
  String ->
  (FilePath -> FilePath -> Repository -> Text.Text -> IO value) ->
  IO value
withTransactionRepositoryCopy getSeed label action = do
  seed <- getSeed
  withRepositorySeedCopy (transactionCopySeed seed) label $ \temporary repositoryPath -> do
    repository <- relocateSeedRepository (transactionSeedPath seed) (transactionSeedRepository seed) repositoryPath
    assertTransactionRepositoryCopy seed repositoryPath repository
    action temporary repositoryPath repository (transactionSeedParent seed)

withUnbornTransactionRepositoryCopy ::
  IO UnbornTransactionRepositorySeed ->
  String ->
  (FilePath -> FilePath -> Repository -> IO value) ->
  IO value
withUnbornTransactionRepositoryCopy getSeed label action = do
  seed <- getSeed
  withRepositorySeedCopy (unbornTransactionCopySeed seed) label $ \temporary repositoryPath -> do
    repository <- relocateSeedRepository (unbornTransactionSeedPath seed) (unbornTransactionSeedRepository seed) repositoryPath
    assertUnbornTransactionRepositoryCopy seed repositoryPath repository
    action temporary repositoryPath repository

relocateSeedRepository :: FilePath -> Repository -> FilePath -> IO Repository
relocateSeedRepository sourcePath sourceRepository repositoryPath = do
  let relocate label = relocateSeedPath label sourcePath repositoryPath
  worktreeRoot <- traverse (relocate "worktree root") (repositoryWorktreeRoot sourceRepository)
  gitDirectory <- relocate "Git directory" (repositoryGitDir sourceRepository)
  commonDirectory <- relocate "common Git directory" (repositoryCommonDir sourceRepository)
  commandDirectory <- relocate "command directory" (repositoryCommandDirectory sourceRepository)
  pure
    sourceRepository
      { repositoryWorktreeRoot = worktreeRoot,
        repositoryGitDir = gitDirectory,
        repositoryCommonDir = commonDirectory,
        repositoryCommandDirectory = commandDirectory
      }

relocateSeedPath :: String -> FilePath -> FilePath -> FilePath -> IO FilePath
relocateSeedPath label sourceRoot targetRoot sourcePath = do
  let relative = makeRelative sourceRoot sourcePath
      relocated
        | relative == "." = normalise targetRoot
        | otherwise = normalise (targetRoot </> relative)
  assertBool (label <> " originates inside the immutable seed repository") (isContainedPath sourceRoot sourcePath)
  assertBool (label <> " has no parent traversal after relocation") (".." `notElem` splitDirectories relative)
  assertBool (label <> " is contained inside the private repository copy") (isContainedPath targetRoot relocated)
  pure relocated

isContainedPath :: FilePath -> FilePath -> Bool
isContainedPath root path =
  let relative = makeRelative (normalise root) (normalise path)
   in not (isAbsolute relative) && ".." `notElem` splitDirectories relative

assertTransactionRepositoryCopy :: TransactionRepositorySeed -> FilePath -> Repository -> IO ()
assertTransactionRepositoryCopy seed repositoryPath repository = do
  let seedPath = transactionSeedPath seed
      seedGitDirectory = repositoryGitDir (transactionSeedRepository seed)
      privateGitDirectory = repositoryGitDir repository
      parentText = transactionSeedParent seed
      parentObjectPath = "objects" </> take 2 (Text.unpack parentText) </> drop 2 (Text.unpack parentText)
      assertCopiedFile relative = do
        seedBytes <- BS.readFile (seedGitDirectory </> relative)
        privateBytes <- BS.readFile (privateGitDirectory </> relative)
        assertEqual ("private repository copies exact " <> relative <> " bytes") seedBytes privateBytes
        pure privateBytes
  assertMainWorktreeRepositoryIdentity "private committed repository" repositoryPath repository
  privateHead <- assertCopiedFile "HEAD"
  assertEqual "private committed repository retains the main branch identity" "ref: refs/heads/main\n" privateHead
  mapM_ assertCopiedFile ["index", parentObjectPath]
  privateRef <- assertCopiedFile ("refs" </> "heads" </> "main")
  assertEqual "private repository contains the cached initial commit OID" (TextEncoding.encodeUtf8 (parentText <> "\n")) privateRef
  seedWorktreeBytes <- BS.readFile (seedPath </> "seed.txt")
  privateWorktreeBytes <- BS.readFile (repositoryPath </> "seed.txt")
  assertEqual "private committed repository copies exact tracked worktree bytes" seedWorktreeBytes privateWorktreeBytes
  privateConfig <- assertCopiedFile "config"
  assertPrivateRepositoryIsolation "private committed repository" seedPath privateGitDirectory privateConfig

assertUnbornTransactionRepositoryCopy :: UnbornTransactionRepositorySeed -> FilePath -> Repository -> IO ()
assertUnbornTransactionRepositoryCopy seed repositoryPath repository = do
  let seedPath = unbornTransactionSeedPath seed
      seedGitDirectory = repositoryGitDir (unbornTransactionSeedRepository seed)
      privateGitDirectory = repositoryGitDir repository
      assertCopiedFile relative = do
        seedBytes <- BS.readFile (seedGitDirectory </> relative)
        privateBytes <- BS.readFile (privateGitDirectory </> relative)
        assertEqual ("private unborn repository copies exact " <> relative <> " bytes") seedBytes privateBytes
        pure privateBytes
  assertMainWorktreeRepositoryIdentity "private unborn repository" repositoryPath repository
  privateHead <- assertCopiedFile "HEAD"
  assertEqual "private unborn repository retains the unborn main branch identity" "ref: refs/heads/main\n" privateHead
  privateConfig <- assertCopiedFile "config"
  assertPrivateRepositoryIsolation "private unborn repository" seedPath privateGitDirectory privateConfig
  doesFileExist (privateGitDirectory </> "index")
    >>= assertEqual "private unborn repository has no caller index" False
  doesFileExist (privateGitDirectory </> "refs" </> "heads" </> "main")
    >>= assertEqual "private unborn repository has no main ref" False
  privateObjects <- sort <$> listDirectory (privateGitDirectory </> "objects")
  seedObjects <- sort <$> listDirectory (seedGitDirectory </> "objects")
  assertEqual "private unborn repository copies the empty object-store shape" seedObjects privateObjects
  assertBool "private unborn repository contains no loose commit object" (all (`elem` ["info", "pack"]) privateObjects)
  sort <$> listDirectory repositoryPath
    >>= assertEqual "private unborn repository has no worktree bytes" [".git"]

assertMainWorktreeRepositoryIdentity :: String -> FilePath -> Repository -> IO ()
assertMainWorktreeRepositoryIdentity label repositoryPath repository = do
  let gitDirectory = repositoryPath </> ".git"
  assertEqual (label <> " has its exact worktree root") (Just repositoryPath) (repositoryWorktreeRoot repository)
  assertEqual (label <> " has its exact Git directory") gitDirectory (repositoryGitDir repository)
  assertEqual (label <> " has its exact common Git directory") gitDirectory (repositoryCommonDir repository)
  assertEqual (label <> " has its exact command directory") repositoryPath (repositoryCommandDirectory repository)
  assertEqual (label <> " retains the main-worktree layout") MainWorktree (repositoryLayout repository)
  assertEqual (label <> " retains its non-bare common directory") False (repositoryCommonIsBare repository)

assertPrivateRepositoryIsolation :: String -> FilePath -> FilePath -> BS.ByteString -> IO ()
assertPrivateRepositoryIsolation label seedPath privateGitDirectory privateConfig = do
  let seedPathForward = map (\character -> if character == '\\' then '/' else character) seedPath
      seedPathBackward = map (\character -> if character == '/' then '\\' else character) seedPath
      seedPathSpellings = map BS8.pack [seedPath, seedPathForward, seedPathBackward]
  assertBool (label <> " config contains no immutable seed path") (not (any (`BS8.isInfixOf` privateConfig) seedPathSpellings))
  assertBool (label <> " keeps a repository-relative no-hooks path") (".git/adrai-no-hooks" `BS8.isInfixOf` privateConfig)
  doesDirectoryExist (privateGitDirectory </> "adrai-no-hooks")
    >>= assertEqual (label <> " owns its no-hooks directory") True
  doesFileExist (privateGitDirectory </> "objects" </> "info" </> "alternates")
    >>= assertEqual (label <> " has no shared object alternates") False

makeTrackedFileStatStale :: FilePath -> FilePath -> IO ()
makeTrackedFileStatStale repositoryPath relativePath = do
  let path = repositoryPath </> relativePath
  cachedEntry <- gitSuccess repositoryPath ["--no-optional-locks", "ls-files", "--debug", "--", relativePath] BS.empty
  bytesBefore <- BS.readFile path
  modificationTime <- getModificationTime path
  setModificationTime path (addUTCTime (-3600) modificationTime)
  bytesAfter <- BS.readFile path
  staleModificationTime <- getModificationTime path
  let staleMtime = BS8.pack ("mtime: " <> show (floor (utcTimeToPOSIXSeconds staleModificationTime) :: Integer) <> ":")
  assertEqual "stale-stat fixture preserves tracked file content" bytesBefore bytesAfter
  assertBool "stale-stat fixture changes only tracked file metadata" (staleModificationTime /= modificationTime)
  assertBool "stale-stat fixture differs from the cached index mtime" (not (staleMtime `BS8.isInfixOf` cachedEntry))

requireRight :: (Show problem) => Either problem value -> IO value
requireRight result =
  case result of
    Left problem -> assertFailure (show problem) >> fail "unreachable"
    Right value -> pure value

requireGitOid :: Text.Text -> IO GitOid
requireGitOid = pure . GitOid

assertAccepted :: (String, BS8.ByteString, Text.Text) -> IO ()
assertAccepted (label, raw, expected) =
  assertEqual label (Right expected) (gitOidText <$> parseSingleOidFromOutput "write-tree" raw)

assertRejected :: (String, BS8.ByteString) -> IO ()
assertRejected (label, raw) =
  assertBool label (isLeft (parseSingleOidFromOutput "commit-tree" raw))

acceptedOutputs :: [(String, BS8.ByteString, Text.Text)]
acceptedOutputs =
  [ ("40 hex without terminator", oid40, oid40Text),
    ("40 hex with LF", oid40 <> "\n", oid40Text),
    ("40 hex with CRLF", oid40 <> "\r\n", oid40Text),
    ("40 hex with trailing spaces", oid40 <> "   ", oid40Text),
    ("40 hex with newline and trailing spaces", oid40 <> "\n  ", oid40Text),
    ("40 uppercase hex", BS8.map toUpperAscii oid40 <> "\n", oid40Text),
    ("64 hex without terminator", oid64, oid64Text),
    ("64 hex with LF", oid64 <> "\n", oid64Text),
    ("64 hex with CRLF", oid64 <> "\r\n", oid64Text),
    ("64 hex with trailing spaces", oid64 <> "   ", oid64Text),
    ("64 uppercase hex", BS8.map toUpperAscii oid64 <> "\n", oid64Text)
  ]

rejectedOutputs :: [(String, BS8.ByteString)]
rejectedOutputs =
  [ ("empty", ""),
    ("whitespace only", " \r\n "),
    ("wrong length", BS8.take 39 oid40),
    ("non-hex", BS8.take 39 oid40 <> "g"),
    ("leading whitespace", " " <> oid40),
    ("leading tab", "\t" <> oid40),
    ("labeled output", "write-tree: " <> oid40),
    ("two OIDs separated by space", oid40 <> " " <> oid40),
    ("two OIDs separated by newline", oid40 <> "\n" <> oid40),
    ("bare carriage return terminator", oid40 <> "\r"),
    ("multiple newline terminators", oid40 <> "\n\n")
  ]

oid40 :: BS8.ByteString
oid40 = "0123456789abcdef0123456789abcdef01234567"

oid40Text :: Text.Text
oid40Text = "0123456789abcdef0123456789abcdef01234567"

oid64 :: BS8.ByteString
oid64 = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

oid64Text :: Text.Text
oid64Text = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

toUpperAscii :: Char -> Char
toUpperAscii character
  | character >= 'a' && character <= 'f' = toEnum (fromEnum character - 32)
  | otherwise = character
