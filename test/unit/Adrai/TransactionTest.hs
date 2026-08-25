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
    RepositoryLayout (LinkedWorktree),
    discoverRepository,
    gitOidText,
    systemGit,
  )
import Adrai.GitTestSupport
  ( commitFile,
    createWorktree,
    gitSuccess,
    initTestRepository,
    installFailingCleanFilter,
    outputText,
    withRejectingReferenceTransactionHook,
  )
import Adrai.Integration.CLI (adraiTestArgs, createAdraiInit)
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
    commitTree,
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
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft)
import Control.Exception (AsyncException (ThreadKilled), SomeException, throwIO, try)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Directory (createDirectory, createDirectoryLink, doesDirectoryExist, doesFileExist, listDirectory, removeDirectory, removeDirectoryRecursive, removeFile)
import Data.List (isPrefixOf, sort)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import qualified System.Exit as Exit
import System.FilePath (isAbsolute, (</>), takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import System.Environment (lookupEnv)
import System.Process.Typed (proc, readProcess, runProcess, shell)
import System.Info (os)
import System.IO.Error (tryIOError)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Transaction contract"
    [ testGroup "repository-common mutation lock" gitLockTests,
      testCase "Git plumbing accepts one bare 40- or 64-hex OID with trailing stdout framing" $
        mapM_ assertAccepted acceptedOutputs,
      testCase "Git plumbing rejects non-bare or malformed OID output" $
        mapM_ assertRejected rejectedOutputs,
      testCase "commit-tree creates a one-parent commit from stdin message" $
        withSystemTempDirectory "adrai commit-tree" $ \temporary -> do
          let repositoryPath = temporary </> "repository"
          initTestRepository repositoryPath
          parentText <- commitFile repositoryPath "seed.txt" "seed"
          repository <-
            discoverRepository systemGit repositoryPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
          treeText <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD^{tree}"] ""
          parent <- requireGitOid parentText
          tree <- requireGitOid treeText
          commitTree repository parent tree "Follow up" "op-commit-tree" (Map.fromList [("Reason", "regression")]) >>= \case
            Left problem -> assertFailure (show problem)
            Right commit -> do
              let commitText = Text.unpack (gitOidText commit)
              actualParents <- outputText <$> gitSuccess repositoryPath ["show", "-s", "--format=%P", commitText] ""
              actualMessage <- outputText <$> gitSuccess repositoryPath ["show", "-s", "--format=%B", commitText] ""
              assertEqual "commit-tree supplies exactly the previous HEAD as parent" parentText actualParents
              assertEqual
                "commit-tree reads the complete message from stdin"
                "Follow up\n\nADRAI-Op: op-commit-tree\nADRAI-Reason: regression"
                actualMessage,
      testCase "commit-tree creates a root commit from stdin message" $
        withSystemTempDirectory "adrai commit-tree root" $ \temporary -> do
          let repositoryPath = temporary </> "repository"
          initTestRepository repositoryPath
          repository <-
            discoverRepository systemGit repositoryPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
          treeText <- outputText <$> gitSuccess repositoryPath ["mktree"] ""
          tree <- requireGitOid treeText
          commitTree repository nullOid tree "Bootstrap" "op-bootstrap" (Map.fromList [("Objects", "bootstrap")]) >>= \case
            Left problem -> assertFailure (show problem)
            Right commit -> do
              let commitText = Text.unpack (gitOidText commit)
              actualParents <- outputText <$> gitSuccess repositoryPath ["show", "-s", "--format=%P", commitText] ""
              actualMessage <- outputText <$> gitSuccess repositoryPath ["show", "-s", "--format=%B", commitText] ""
              assertEqual "commit-tree supplies no parent for the null OID" "" actualParents
              assertEqual
                "commit-tree reads the complete root message from stdin"
                "Bootstrap\n\nADRAI-Op: op-bootstrap\nADRAI-Objects: bootstrap"
                actualMessage,
       testCase "append-only transaction preserves unrelated staged entry and binary worktree bytes" $
        withSystemTempDirectory "adrai transaction isolated index" $ \temporary -> do
          let repositoryPath = temporary </> "repository"
              unrelatedPath = repositoryPath </> "unrelated.bin"
              stagedBytes = BS.pack [255, 0, 13, 10, 128, 64, 1, 2, 3]
          initTestRepository repositoryPath
          parentText <- commitFile repositoryPath "seed.txt" "seed\n"
          BS.writeFile unrelatedPath stagedBytes
          _ <- gitSuccess repositoryPath ["add", "--", "unrelated.bin"] BS.empty
          stagedEntryBefore <- gitSuccess repositoryPath ["ls-files", "--stage", "--", "unrelated.bin"] BS.empty
          worktreeBytesBefore <- BS.readFile unrelatedPath
          repository <-
            discoverRepository systemGit repositoryPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
          parent <- requireGitOid parentText
          (operationText, generated) <- transactionGeneratedFile parent
          let generatedPathText = Text.unpack (repoPathText (genFilePath generated))
              config =
                TransactionConfig
                  { configOperationId = operationText,
                    configSubject = "adrai: transaction index isolation",
                    configTrailers = Map.fromList [("Objects", "transaction-isolation")],
                    configExpectedHead = parent,
                    configGenerated = [generated]
                  }
          commitAppendOnlyOperation repository config >>= \case
            Left problem -> assertFailure (show problem)
            Right result -> do
              assertBool "generated paths were refreshed in the real index" (transactionIndexUpdated result)
              stagedEntryAfter <- gitSuccess repositoryPath ["ls-files", "--stage", "--", "unrelated.bin"] BS.empty
              worktreeBytesAfter <- BS.readFile unrelatedPath
              stagedPaths <- outputText <$> gitSuccess repositoryPath ["diff", "--cached", "--name-only"] BS.empty
              generatedIndexDiff <- gitSuccess repositoryPath ["diff", "--cached", "--", generatedPathText] BS.empty
              generatedWorktreeBytes <- gitSuccess repositoryPath ["show", "HEAD:" <> generatedPathText] BS.empty
              assertEqual "unrelated index OID, mode, and stage are unchanged" stagedEntryBefore stagedEntryAfter
              assertEqual "unrelated binary worktree bytes are unchanged" worktreeBytesBefore worktreeBytesAfter
              assertEqual "only the unrelated staged path remains staged" "unrelated.bin" stagedPaths
              assertEqual "generated path is refreshed to the committed HEAD entry" BS.empty generatedIndexDiff
              assertEqual "generated bytes are committed" (genFileBytes generated) generatedWorktreeBytes
      , testCase "Stage7 failure restores caller index and generated worktree state exactly" $
          withSystemTempDirectory "adrai transaction rollback" $ \temporary -> do
            let repositoryPath = temporary </> "repository"
                stagedPath = repositoryPath </> "staged.bin"
                dirtyPath = repositoryPath </> "dirty.bin"
                untrackedPath = repositoryPath </> "untracked.bin"
                stagedBytes = BS.pack [255, 0, 13, 10, 128, 64, 1, 2, 3]
                dirtyBytes = BS.pack [3, 2, 1, 0, 255]
                untrackedBytes = BS.pack [13, 10, 0, 17, 255]
            initTestRepository repositoryPath
            parentText <- commitFile repositoryPath "seed.txt" "seed\n"
            BS.writeFile stagedPath stagedBytes
            BS.writeFile dirtyPath dirtyBytes
            BS.writeFile untrackedPath untrackedBytes
            _ <- gitSuccess repositoryPath ["add", "--", "staged.bin"] BS.empty
            headBefore <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD"] BS.empty
            treeBefore <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD^{tree}"] BS.empty
            indexBefore <- BS.readFile (repositoryPath </> ".git" </> "index")
            indexEntriesBefore <- gitSuccess repositoryPath ["ls-files", "--stage"] BS.empty
            statusBefore <- gitSuccess repositoryPath ["status", "--porcelain=v1", "--untracked-files=all"] BS.empty
            repository <-
              discoverRepository systemGit repositoryPath >>= \case
                Left problem -> assertFailure (show problem)
                Right discovered -> pure discovered
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
            commitAppendOnlyOperation repository config >>= \case
              Left (Stage7CommitTree _) -> pure ()
              Left problem -> assertFailure ("expected original Stage7 failure, got " <> show problem)
              Right result -> assertFailure ("expected Stage7 failure, got " <> show result)
            headAfter <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD"] BS.empty
            treeAfter <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD^{tree}"] BS.empty
            indexAfter <- BS.readFile (repositoryPath </> ".git" </> "index")
            indexEntriesAfter <- gitSuccess repositoryPath ["ls-files", "--stage"] BS.empty
            statusAfter <- gitSuccess repositoryPath ["status", "--porcelain=v1", "--untracked-files=all"] BS.empty
            stagedAfter <- BS.readFile stagedPath
            dirtyAfter <- BS.readFile dirtyPath
            untrackedAfter <- BS.readFile untrackedPath
            generatedExists <- doesFileExist generatedPath
            generatedParentExists <- doesDirectoryExist generatedParent
            temporaryIndexes <- adraiTemporaryIndexes repositoryPath
            assertEqual "HEAD is restored exactly" headBefore headAfter
            assertEqual "HEAD tree is restored exactly" treeBefore treeAfter
            assertEqual "raw caller index is byte-for-byte unchanged" indexBefore indexAfter
            assertEqual "complete caller index entries are unchanged" indexEntriesBefore indexEntriesAfter
            assertEqual "staged, dirty, and untracked path set is unchanged" statusBefore statusAfter
            assertEqual "staged binary bytes are unchanged" stagedBytes stagedAfter
            assertEqual "dirty binary bytes are unchanged" dirtyBytes dirtyAfter
            assertEqual "untracked binary bytes are unchanged" untrackedBytes untrackedAfter
            assertBool "generated managed file is removed" (not generatedExists)
            assertBool "transaction-owned generated directory is removed" (not generatedParentExists)
            assertEqual "Stage7 cleanup leaves no temporary transaction index" [] temporaryIndexes
      , testCase "partial Stage5 write removes only the path written by this attempt" $
          withSystemTempDirectory "adrai transaction partial stage5" $ \temporary -> do
            let repositoryPath = temporary </> "repository"
                callerPath = "caller-owned.decision.md"
                callerBytes = "caller-owned bytes\NUL\255"
            initTestRepository repositoryPath
            parentText <- commitFile repositoryPath callerPath callerBytes
            repository <-
              discoverRepository systemGit repositoryPath >>= \case
                Left problem -> assertFailure (show problem)
                Right discovered -> pure discovered
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
          withSystemTempDirectory "adrai transaction stage6 rollback" $ \temporary -> do
            let repositoryPath = temporary </> "repository"
                stagedPath = repositoryPath </> "staged.bin"
                stagedBytes = BS.pack [0, 255, 4, 9]
            initTestRepository repositoryPath
            parentText <- installFailingCleanFilter repositoryPath
            BS.writeFile stagedPath stagedBytes
            _ <- gitSuccess repositoryPath ["add", "--", "staged.bin"] BS.empty
            indexBefore <- BS.readFile (repositoryPath </> ".git" </> "index")
            repository <-
              discoverRepository systemGit repositoryPath >>= \case
                Left problem -> assertFailure (show problem)
                Right discovered -> pure discovered
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
          withSystemTempDirectory "adrai transaction stage8 rollback" $ \temporary -> do
            let repositoryPath = temporary </> "repository"
                stagedPath = repositoryPath </> "staged.bin"
                stagedBytes = BS.pack [127, 0, 255, 8]
            initTestRepository repositoryPath
            parentText <- commitFile repositoryPath "seed.txt" "seed\n"
            BS.writeFile stagedPath stagedBytes
            _ <- gitSuccess repositoryPath ["add", "--", "staged.bin"] BS.empty
            indexBefore <- BS.readFile (repositoryPath </> ".git" </> "index")
            repository <-
              discoverRepository systemGit repositoryPath >>= \case
                Left problem -> assertFailure (show problem)
                Right discovered -> pure discovered
            parent <- requireGitOid parentText
            (operationText, generated) <- transactionGeneratedFile parent
            let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
                config =
                  TransactionConfig
                    { configOperationId = operationText,
                      configSubject = "adrai: rollback stage8",
                      configTrailers = Map.fromList [("Objects", "transaction-stage8")],
                      configExpectedHead = parent,
                      configGenerated = [generated]
                    }
            withRejectingReferenceTransactionHook repositoryPath Nothing (commitAppendOnlyOperation repository config) >>= \case
              Left (Stage8UpdateRef _) -> pure ()
              Left problem -> assertFailure ("expected original Stage8 failure, got " <> show problem)
              Right result -> assertFailure ("expected Stage8 failure, got " <> show result)
            headAfter <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD"] BS.empty
            indexAfter <- BS.readFile (repositoryPath </> ".git" </> "index")
            stagedAfter <- BS.readFile stagedPath
            generatedExists <- doesFileExist generatedPath
            temporaryIndexes <- adraiTemporaryIndexes repositoryPath
            assertEqual "Stage8 rejection leaves the ref at its exact old HEAD" parentText headAfter
            assertEqual "Stage8 rejection preserves the raw caller index" indexBefore indexAfter
            assertEqual "Stage8 rejection preserves staged binary bytes" stagedBytes stagedAfter
            assertBool "Stage8 rollback removes the generated managed file" (not generatedExists)
            assertEqual "Stage8 cleanup leaves no temporary transaction index" [] temporaryIndexes
      , testCase "Stage8 rollback rechecks the pinned main ref after hook rejection" $
          withSystemTempDirectory "adrai transaction pinned ref" $ \temporary -> do
            let repositoryPath = temporary </> "repository"
            initTestRepository repositoryPath
            parentText <- commitFile repositoryPath "seed.txt" "seed\n"
            _ <- gitSuccess repositoryPath ["branch", "other", "HEAD"] BS.empty
            repository <- discoverRepository systemGit repositoryPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
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
          withSystemTempDirectory "adrai transaction authoritative new" $ \temporary -> do
            let repositoryPath = temporary </> "repository"
            initTestRepository repositoryPath
            parentText <- commitFile repositoryPath "seed.txt" "seed\n"
            repository <- discoverRepository systemGit repositoryPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
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
          withSystemTempDirectory "adrai transaction ambiguous ref" $ \temporary -> do
            let repositoryPath = temporary </> "repository"
                otherCommit = GitOid "1111111111111111111111111111111111111111"
            initTestRepository repositoryPath
            parentText <- commitFile repositoryPath "seed.txt" "seed\n"
            repository <- discoverRepository systemGit repositoryPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
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
          withSystemTempDirectory "adrai transaction unreadable ref" $ \temporary -> do
            let repositoryPath = temporary </> "repository"
            initTestRepository repositoryPath
            parentText <- commitFile repositoryPath "seed.txt" "seed\n"
            repository <- discoverRepository systemGit repositoryPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
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
          withSystemTempDirectory "adrai transaction observed pinned ref" $ \temporary -> do
            let repositoryPath = temporary </> "repository"
            initTestRepository repositoryPath
            parentText <- commitFile repositoryPath "seed.txt" "seed\n"
            _ <- gitSuccess repositoryPath ["branch", "other", "HEAD"] BS.empty
            repository <- discoverRepository systemGit repositoryPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
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
          withSystemTempDirectory "adrai transaction cleanup failure" $ \temporary -> do
            let repositoryPath = temporary </> "repository"
            initTestRepository repositoryPath
            parentText <- installFailingCleanFilter repositoryPath
            repository <- discoverRepository systemGit repositoryPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
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
      , testCase "linked worktree transaction uses its own index and updates only its pinned branch" $
          withSystemTempDirectory "adrai linked transaction" $ \temporary -> do
            let mainPath = temporary </> "main"
                linkedPath = temporary </> "linked"
                stagedPath = linkedPath </> "linked-staged.bin"
                stagedBytes = BS.pack [0, 255, 17, 9]
            initTestRepository mainPath
            parentText <- commitFile mainPath "seed.txt" "seed\n"
            createWorktree mainPath linkedPath "linked"
            BS.writeFile stagedPath stagedBytes
            _ <- gitSuccess linkedPath ["add", "--", "linked-staged.bin"] BS.empty
            mainHeadBefore <- outputText <$> gitSuccess mainPath ["rev-parse", "refs/heads/main"] BS.empty
            linkedIndexEntryBefore <- gitSuccess linkedPath ["ls-files", "--stage", "--", "linked-staged.bin"] BS.empty
            mainIndexBefore <- BS.readFile (mainPath </> ".git" </> "index")
            repository <- discoverRepository systemGit linkedPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
            assertEqual "discovery identifies a real linked worktree" LinkedWorktree (repositoryLayout repository)
            assertBool "linked caller index is distinct from common repository storage" (repositoryGitDir repository /= repositoryCommonDir repository)
            linkedIndexBefore <- BS.readFile (repositoryGitDir repository </> "index")
            parent <- requireGitOid parentText
            (operationText, generated) <- transactionGeneratedFile parent
            let config = TransactionConfig operationText "adrai: linked transaction" (Map.fromList [("Objects", "linked-worktree")]) parent [generated]
                failingDependencies = defaultAppendOnlyDependencies { appendOnlyAfterGeneratedWrite = throwIO (Stage7CommitTree "injected linked-worktree rollback") }
            commitAppendOnlyOperationWith failingDependencies repository config >>= \case
              Left (Stage7CommitTree _) -> pure ()
              Left problem -> assertFailure ("expected injected Stage7 failure, got " <> show problem)
              Right result -> assertFailure ("expected injected failure, got " <> show result)
            linkedIndexAfterFailure <- BS.readFile (repositoryGitDir repository </> "index")
            mainIndexAfterFailure <- BS.readFile (mainPath </> ".git" </> "index")
            assertEqual "failed linked transaction restores the linked caller index byte-for-byte" linkedIndexBefore linkedIndexAfterFailure
            assertEqual "failed linked transaction does not touch the main worktree index" mainIndexBefore mainIndexAfterFailure
            commitAppendOnlyOperation repository config >>= \case
              Left problem -> assertFailure (show problem)
              Right _ -> pure ()
            linkedHeadAfter <- outputText <$> gitSuccess linkedPath ["rev-parse", "refs/heads/linked"] BS.empty
            mainHeadAfter <- outputText <$> gitSuccess mainPath ["rev-parse", "refs/heads/main"] BS.empty
            linkedIndexEntryAfter <- gitSuccess linkedPath ["ls-files", "--stage", "--", "linked-staged.bin"] BS.empty
            assertBool "linked branch advances from its pinned old ref" (linkedHeadAfter /= parentText)
            assertEqual "main branch remains at its original ref" mainHeadBefore mainHeadAfter
            assertEqual "linked caller's unrelated staged entry survives refresh" linkedIndexEntryBefore linkedIndexEntryAfter
      , testCase "injected cancellation after write cleans then propagates ThreadKilled" $
          withSystemTempDirectory "adrai transaction cancellation" $ \temporary -> do
            let repositoryPath = temporary </> "repository"
            initTestRepository repositoryPath
            parentText <- commitFile repositoryPath "seed.txt" "seed\n"
            repository <- discoverRepository systemGit repositoryPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
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
          withSystemTempDirectory "adrai transaction partial sync" $ \temporary -> do
            let repositoryPath = temporary </> "repository"
            initTestRepository repositoryPath
            parentText <- commitFile repositoryPath "seed.txt" "seed\n"
            repository <- discoverRepository systemGit repositoryPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
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
          withSystemTempDirectory "adrai transaction partial async" $ \temporary -> do
            let repositoryPath = temporary </> "repository"
            initTestRepository repositoryPath
            parentText <- commitFile repositoryPath "seed.txt" "seed\n"
            repository <- discoverRepository systemGit repositoryPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
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
          withSystemTempDirectory "adrai transaction snapshot async" $ \temporary -> do
            let repositoryPath = temporary </> "repository"
            initTestRepository repositoryPath
            parentText <- commitFile repositoryPath "seed.txt" "seed\n"
            repository <- discoverRepository systemGit repositoryPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
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
          withSystemTempDirectory "adrai transaction cleanup async" $ \temporary -> do
            let repositoryPath = temporary </> "repository"
            initTestRepository repositoryPath
            parentText <- commitFile repositoryPath "seed.txt" "seed\n"
            repository <- discoverRepository systemGit repositoryPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
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
          withSystemTempDirectory "adrai transaction temp-index cleanup async" $ \temporary -> do
            let repositoryPath = temporary </> "repository"
            initTestRepository repositoryPath
            parentText <- commitFile repositoryPath "seed.txt" "seed\n"
            repository <- discoverRepository systemGit repositoryPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
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
          withSystemTempDirectory "adrai transaction CAS head switch" $ \temporary -> do
            let repositoryPath = temporary </> "repository"
                headPath = repositoryPath </> ".git" </> "HEAD"
            initTestRepository repositoryPath
            parentText <- commitFile repositoryPath "seed.txt" "seed\n"
            _ <- gitSuccess repositoryPath ["branch", "other", "HEAD"] BS.empty
            repository <- discoverRepository systemGit repositoryPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
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
            headAfter <- outputText <$> gitSuccess repositoryPath ["symbolic-ref", "--short", "HEAD"] BS.empty
            otherAfter <- outputText <$> gitSuccess repositoryPath ["rev-parse", "refs/heads/other"] BS.empty
            generatedIndexDiff <- gitSuccess repositoryPath ["diff", "--cached", Text.unpack (gitOidText (transactionCommitOid transaction)), "--", generatedPathText] BS.empty
            assertEqual "the injected HEAD switch took effect" "other" headAfter
            assertEqual "the other branch was not reset or advanced" parentText otherAfter
            assertEqual "the caller index uses the pinned new commit, not mutable HEAD" BS.empty generatedIndexDiff
      , testCase "append-only transaction rejects a redirected managed parent before touching repository state" $
          assertRedirectedManagedParent "append-only" commitAppendOnlyOperation
      , testCase "bootstrap transaction rejects a redirected managed parent before backup or write" $
          assertRedirectedManagedParent "bootstrap" commitBootstrapFiles
      , testCase "append-only transaction re-resolves after parent creation before its write" $
          assertLateRedirectAfterParentCreation
            "append-only"
            (\repository config redirect ->
                commitAppendOnlyOperationWithHooks
                  defaultAppendOnlyDependencies
                  defaultAppendOnlyTestHooks {appendOnlyAfterGeneratedParentCreationHook = redirect}
                  repository config
            )
      , testCase "bootstrap transaction re-resolves after parent creation before its write" $
          assertLateRedirectAfterParentCreation
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
          assertAppendRollbackPrecedence True
      , testCase "append-only synchronous rollback failure is typed and preserves caller state" $
          assertAppendRollbackPrecedence False
      , testCase "bootstrap rollback reports synchronous generated-file delete failure and retains bytes for retry" $
          assertBootstrapRollbackDeleteFailure False
      , testCase "bootstrap rollback delete cancellation propagates ThreadKilled and retains bytes for retry" $
          assertBootstrapRollbackDeleteFailure True
      , testCase "append rollback hook cancellation wins over a synchronous restore failure" $
          assertAppendHookAsyncWinsRestoreFailure
      , testCase "bootstrap rollback hook cancellation wins over a synchronous delete failure" $
          assertBootstrapHookAsyncWinsDeleteFailure
      , testCase "append-only re-resolves managed destinations after temporary index before CAS" $
          assertPreCasRedirect
            "append-only"
            (\repository config redirect ->
                commitAppendOnlyOperationWithHooks defaultAppendOnlyDependencies defaultAppendOnlyTestHooks {appendOnlyBeforeRefUpdateHook = redirect} repository config
            )
      , testCase "bootstrap re-resolves managed destinations after temporary index before CAS" $
          assertPreCasRedirect
            "bootstrap"
            (\repository config redirect ->
                commitBootstrapFilesWith (bootstrapDependencies redirect (pure ())) repository config
            )
      , testCase "append-only preserves authoritative CAS and skips unsafe post-CAS index refresh" $
          assertPostCasRedirect
            "append-only"
            (\repository config redirect ->
                commitAppendOnlyOperationWithHooks defaultAppendOnlyDependencies defaultAppendOnlyTestHooks {appendOnlyBeforePostCasIndexRefreshHook = redirect} repository config
            )
      , testCase "bootstrap preserves authoritative CAS and skips unsafe post-CAS index refresh" $
          assertPostCasRedirect
            "bootstrap"
            (\repository config redirect ->
                commitBootstrapFilesWith (bootstrapDependencies (pure ()) redirect) repository config
            )
      , testCase "bootstrap post-CAS ThreadKilled preserves its authoritative commit and releases the lock" $
          assertBootstrapPostCasCancellationAuthority
      , testCase "bootstrap post-update-ref ThreadKilled verifies the candidate ref before rollback" $
          assertBootstrapPostUpdateRefCancellationAuthority
      , testCase "unborn bootstrap candidate cancellation confirms missing ref then rolls back" $
          assertUnbornCandidateCancellation
      , testCase "unreadable first-commit CAS inspection fails closed without destructive rollback" $
          assertUnreadableFirstCommitInspection
      ]

gitLockTests :: [TestTree]
gitLockTests =
  [ testCase "linked worktrees share one canonical common-directory lock" $
      withSystemTempDirectory "adrai common Git lock" $ \temporary -> do
        let repositoryPath = temporary </> "repository"
            worktreePath = temporary </> "linked-worktree"
        initTestRepository repositoryPath
        _ <- commitFile repositoryPath "seed.txt" "seed\n"
        createWorktree repositoryPath worktreePath "feature/lock-contract"
        repository <- requireRepository repositoryPath
        linkedRepository <- requireRepository worktreePath
        assertEqual "linked worktree uses the repository common directory" (repositoryCommonDir repository) (repositoryCommonDir linkedRepository)
        lock <- acquireGitLock repository
        let path = gitLockPath lock
            expectedBytes = BS8.pack ("pid=" <> show (gitLockPid lock) <> "\n")
        assertEqual "lock lives directly in the canonical common directory" (repositoryCommonDir repository </> "adrai.lock") path
        BS.readFile path >>= assertEqual "lock has canonical ASCII contents" expectedBytes
        gitLockStatus linkedRepository >>= \case
          Left (LockHeld observedPath observedPid) -> do
            assertEqual "status reports the common lock path" path observedPath
            assertEqual "status reports the owning PID" (gitLockPid lock) observedPid
          other -> assertFailure ("expected live common lock status, got " <> show other)
        releaseGitLock lock
        doesFileExist path >>= assertEqual "release retains the persistent canonical lock file" True
        BS.readFile path >>= assertEqual "release leaves the last canonical owner bytes" expectedBytes
        gitLockStatus linkedRepository >>= assertEqual "an unheld persistent file has no owner" (Right Nothing)
        reacquired <- acquireGitLock linkedRepository
        releaseGitLock reacquired,
    testCase "live lock contention is typed and preserves the lock bytes" $
      withSystemTempDirectory "adrai live Git lock" $ \temporary -> do
        let repositoryPath = temporary </> "repository"
            worktreePath = temporary </> "linked-worktree"
        initTestRepository repositoryPath
        _ <- commitFile repositoryPath "seed.txt" "seed\n"
        createWorktree repositoryPath worktreePath "feature/lock-contention"
        repository <- requireRepository repositoryPath
        linkedRepository <- requireRepository worktreePath
        lock <- acquireGitLock repository
        bytesBefore <- BS.readFile (gitLockPath lock)
        attempted <- try @GitLockError (acquireGitLock linkedRepository)
        assertEqual
          "second acquisition reports the existing owner rather than replacing it"
          (Left (LockHeld (gitLockPath lock) (gitLockPid lock)))
          attempted
        BS.readFile (gitLockPath lock) >>= assertEqual "contended acquisition preserves lock bytes" bytesBefore
        releaseGitLock lock
        gitLockStatus linkedRepository >>= assertEqual "status reports no owner after release" (Right Nothing),
    testCase "persistent stale and noncanonical lock contents are recovered under native ownership" $
      withSystemTempDirectory "adrai stale Git lock" $ \temporary -> do
        let repositoryPath = temporary </> "repository"
        initTestRepository repositoryPath
        _ <- commitFile repositoryPath "seed.txt" "seed\n"
        repository <- requireRepository repositoryPath
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
      withSystemTempDirectory "adrai stale contender Git lock" $ \temporary -> do
        let repositoryPath = temporary </> "repository"
        initTestRepository repositoryPath
        _ <- commitFile repositoryPath "seed.txt" "seed\n"
        repository <- requireRepository repositoryPath
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
      withSystemTempDirectory "adrai reservation token Git lock" $ \temporary -> do
        let repositoryPath = temporary </> "repository"
        initTestRepository repositoryPath
        _ <- commitFile repositoryPath "seed.txt" "seed\n"
        repository <- requireRepository repositoryPath
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
      withSystemTempDirectory "adrai concurrent release Git lock" $ \temporary -> do
        let repositoryPath = temporary </> "repository"
        initTestRepository repositoryPath
        _ <- commitFile repositoryPath "seed.txt" "seed\n"
        repository <- requireRepository repositoryPath
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
      withSystemTempDirectory "adrai probe cleanup Git lock" $ \temporary -> do
        let repositoryPath = temporary </> "repository"
            failingProbeClose =
              GitLockDependencies $ \_ operation ->
                case operation of
                  CloseStatusProbe -> throwIO (userError "injected Git lock probe close failure")
                  _ -> pure ()
        initTestRepository repositoryPath
        _ <- commitFile repositoryPath "seed.txt" "seed\n"
        repository <- requireRepository repositoryPath
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
      withSystemTempDirectory "adrai probe cancellation Git lock" $ \temporary -> do
        let repositoryPath = temporary </> "repository"
            cancellingProbeClose =
              GitLockDependencies $ \_ operation ->
                case operation of
                  CloseStatusProbe -> throwIO ThreadKilled
                  _ -> pure ()
        initTestRepository repositoryPath
        _ <- commitFile repositoryPath "seed.txt" "seed\n"
        repository <- requireRepository repositoryPath
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
      withSystemTempDirectory "adrai owner cleanup Git lock" $ \temporary -> do
        let repositoryPath = temporary </> "repository"
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
        initTestRepository repositoryPath
        _ <- commitFile repositoryPath "seed.txt" "seed\n"
        repository <- requireRepository repositoryPath
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
      withSystemTempDirectory "adrai withGitLock cleanup precedence" $ \temporary -> do
        let repositoryPath = temporary </> "repository"
        initTestRepository repositoryPath
        _ <- commitFile repositoryPath "seed.txt" "seed\n"
        repository <- requireRepository repositoryPath
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
    testCase "transaction lock contention preserves refs and generated files" $
      withSystemTempDirectory "adrai transaction Git lock" $ \temporary -> do
        let repositoryPath = temporary </> "repository"
        initTestRepository repositoryPath
        parentText <- commitFile repositoryPath "seed.txt" "seed\n"
        repository <- requireRepository repositoryPath
        parent <- requireGitOid parentText
        (operationText, generated) <- transactionGeneratedFile parent
        headBefore <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD"] BS.empty
        let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
            config = TransactionConfig operationText "adrai: held common lock" (Map.fromList [("Objects", "held-lock")]) parent [generated]
        withGitLock repository $ do
          result <- commitAppendOnlyOperation repository config
          case result of
            Left (Stage2AcquireLock message) ->
              assertBool "transaction returns the typed lock holder failure" ("LockHeld" `Text.isInfixOf` message)
            other -> assertFailure ("expected Stage2AcquireLock, got " <> show other)
          outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD"] BS.empty >>= assertEqual "held lock preserves HEAD" headBefore
          doesFileExist generatedPath >>= assertEqual "held lock creates no managed file" False,
    testCase "parent-held production lock rejects an absolute executable child without repository mutation" $
      withSystemTempDirectory "adrai child process Git lock" $ \temporary -> do
        let repositoryPath = temporary </> "repository"
            createArguments =
              [ "create",
                "--title", "Locked child mutation",
                "--summary", "The child must not mutate while the parent owns the common lock.",
                "--body", "## Decision\nRespect the production Git lock.\n",
                "--actor", "llm:planner",
                "--model", "demo-model",
                "--domain", "tooling.git",
                "--json"
              ]
        initTestRepository repositoryPath
        _ <- commitFile repositoryPath "seed.txt" "seed\n"
        createAdraiInit repositoryPath
        (compileExit, _, compileStderr) <- absoluteAdraiChild repositoryPath ["compile", "--json"]
        assertEqual "fixture cache compilation succeeds before the held-child baseline" ExitSuccess compileExit
        assertEqual "fixture cache compilation emits no stderr" LBS.empty compileStderr
        repository <- requireRepository repositoryPath
        before <- childLockObservableState repositoryPath
        withGitLock repository $ do
          held <- gitLockStatus repository
          lockError <- case held of
            Left problem -> pure problem
            Right _ -> assertFailure "parent-held lock was not observable before child mutation" >> fail "unreachable"
          (exitCode, stdout, stderr) <- absoluteAdraiChild repositoryPath createArguments
          assertEqual "the held child mutation exits with ordinary CLI failure" (ExitFailure 2) exitCode
          assertEqual "the rejected child emits no stdout" LBS.empty stdout
          assertEqual
            "the rejected child renders the typed transaction lock error exactly"
            (LBS.fromStrict (TextEncoding.encodeUtf8 ("adrai: " <> Text.pack (show (Stage2AcquireLock (Text.pack (show lockError)))) <> "\n")))
            stderr
          after <- childLockObservableState repositoryPath
          assertEqual "the rejected cross-process child leaves refs, index, worktree, cache, and reflogs unchanged" before after,
    testCase "withGitLock cleans up after synchronous and asynchronous actions" $
      withSystemTempDirectory "adrai Git lock cleanup" $ \temporary -> do
        let repositoryPath = temporary </> "repository"
        initTestRepository repositoryPath
        _ <- commitFile repositoryPath "seed.txt" "seed\n"
        repository <- requireRepository repositoryPath
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
  where
    requireRepository location =
      discoverRepository systemGit location >>= \case
        Left problem -> assertFailure (show problem) >> fail "unreachable"
        Right repository -> pure repository

absoluteAdraiChild :: FilePath -> [String] -> IO (ExitCode, LBS.ByteString, LBS.ByteString)
absoluteAdraiChild repositoryPath arguments = do
  lookupEnv "ADRAI_EXE" >>= \case
    Just executable | not (null executable) && isAbsolute executable ->
      readProcess (proc executable (adraiTestArgs repositoryPath arguments))
    _ -> fail "TransactionTest requires ADRAI_EXE to name an absolute executable under test"

childLockObservableState :: FilePath -> IO (BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString, [(FilePath, BS.ByteString)])
childLockObservableState repositoryPath = do
  headOid <- gitSuccess repositoryPath ["rev-parse", "HEAD"] BS.empty
  refs <- gitSuccess repositoryPath ["show-ref", "--head"] BS.empty
  index <- gitSuccess repositoryPath ["diff", "--cached", "--binary"] BS.empty
  worktree <- gitSuccess repositoryPath ["status", "--porcelain=v1", "-z", "--untracked-files=all"] BS.empty
  reflogs <- gitSuccess repositoryPath ["reflog", "show", "--all", "--format=%H%x00%gs"] BS.empty
  cache <- snapshotChildCache (repositoryPath </> ".adrai")
  pure (headOid, refs, index, worktree, reflogs, cache)

snapshotChildCache :: FilePath -> IO [(FilePath, BS.ByteString)]
snapshotChildCache cacheRoot = do
  exists <- doesDirectoryExist cacheRoot
  if not exists then pure [] else go ""
  where
    go relative = do
      let directory = cacheRoot </> relative
      entries <- sort <$> listDirectory directory
      fmap concat . mapM (snapshotEntry relative) $ entries

    snapshotEntry relative entry = do
      let childRelative = if null relative then entry else relative </> entry
          child = cacheRoot </> childRelative
      isDirectory <- doesDirectoryExist child
      if isDirectory
        then go childRelative
        else do
          isFile <- doesFileExist child
          if isFile
            then do
              bytes <- BS.readFile child
              pure [(childRelative, bytes)]
            else pure []

adraiTemporaryIndexes :: FilePath -> IO [FilePath]
adraiTemporaryIndexes repositoryPath =
  filter ("adrai-index-" `isPrefixOf`) <$> listDirectory (repositoryPath </> ".git")

assertRedirectedManagedParent
  :: String
  -> (Repository -> TransactionConfig -> IO (Either TransactionError TransactionResult))
  -> IO ()
assertRedirectedManagedParent label runTransaction =
  withSystemTempDirectory ("adrai transaction redirected " <> label) $ \temporary -> do
    let repositoryPath = temporary </> "repository"
        outsidePath = temporary </> "outside"
        redirectedParent = repositoryPath </> "architecture" </> "adrai"
    initTestRepository repositoryPath
    parentText <- commitFile repositoryPath "seed.txt" "seed\n"
    createDirectory (repositoryPath </> "architecture")
    createDirectory outsidePath
    createDirectoryRedirect outsidePath redirectedParent
    repository <-
      discoverRepository systemGit repositoryPath >>= \case
        Left problem -> assertFailure (show problem) >> fail "unreachable"
        Right discovered -> pure discovered
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
  :: String
  -> (Repository -> TransactionConfig -> (GeneratedFile -> IO ()) -> IO (Either TransactionError TransactionResult))
  -> IO ()
assertLateRedirectAfterParentCreation label runTransaction =
  withSystemTempDirectory ("adrai transaction late redirect " <> label) $ \temporary -> do
    let repositoryPath = temporary </> "repository"
        outsidePath = temporary </> "outside"
        managedParent = repositoryPath </> "architecture" </> "adrai"
        architectureParent = repositoryPath </> "architecture"
        redirect GeneratedFile{} = do
          removeDirectoryRecursive managedParent
          createDirectoryRedirect outsidePath managedParent
    initTestRepository repositoryPath
    parentText <- commitFile repositoryPath "seed.txt" "seed\n"
    createDirectory outsidePath
    repository <-
      discoverRepository systemGit repositoryPath >>= \case
        Left problem -> assertFailure (show problem) >> fail "unreachable"
        Right discovered -> pure discovered
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

assertAppendRollbackPrecedence :: Bool -> IO ()
assertAppendRollbackPrecedence originalIsAsync =
  withSystemTempDirectory "adrai transaction rollback precedence" $ \temporary -> do
    let repositoryPath = temporary </> "repository"
    initTestRepository repositoryPath
    parentText <- commitFile repositoryPath "seed.txt" "seed\n"
    repository <-
      discoverRepository systemGit repositoryPath >>= \case
        Left problem -> assertFailure (show problem) >> fail "unreachable"
        Right discovered -> pure discovered
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

assertBootstrapRollbackDeleteFailure :: Bool -> IO ()
assertBootstrapRollbackDeleteFailure deleteIsAsync =
  withSystemTempDirectory "adrai bootstrap rollback delete" $ \temporary -> do
    let repositoryPath = temporary </> "repository"
    initTestRepository repositoryPath
    parentText <- commitFile repositoryPath "seed.txt" "seed\n"
    repository <-
      discoverRepository systemGit repositoryPath >>= \case
        Left problem -> assertFailure (show problem) >> fail "unreachable"
        Right discovered -> pure discovered
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
    let (headBefore, refsBefore, indexBefore, _, reflogsBefore) = before
        (headAfter, refsAfter, indexAfter, _, reflogsAfter) = afterFailure
    assertEqual "failed bootstrap delete retains precisely the transaction-created bytes" (genFileBytes generated) retainedBytes
    assertEqual "failed bootstrap delete leaves HEAD unchanged" headBefore headAfter
    assertEqual "failed bootstrap delete leaves refs unchanged" refsBefore refsAfter
    assertEqual "failed bootstrap delete leaves index unchanged" indexBefore indexAfter
    assertEqual "failed bootstrap delete leaves reflogs unchanged" reflogsBefore reflogsAfter
    retry <- commitBootstrapFiles repository config
    case retry of
      Left problem -> assertFailure ("default cleanup retry should recover the retained bytes: " <> show problem)
      Right success -> assertEqual "retry creates the requested managed path" [genFilePath generated] (transactionCreatedPaths success)

assertAppendHookAsyncWinsRestoreFailure :: IO ()
assertAppendHookAsyncWinsRestoreFailure =
  withSystemTempDirectory "adrai append rollback async ordering" $ \temporary -> do
    let repositoryPath = temporary </> "repository"
    initTestRepository repositoryPath
    parentText <- commitFile repositoryPath "seed.txt" "seed\n"
    repository <-
      discoverRepository systemGit repositoryPath >>= \case
        Left problem -> assertFailure (show problem) >> fail "unreachable"
        Right discovered -> pure discovered
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
    let (headBefore, refsBefore, indexBefore, _, reflogsBefore) = before
        (headAfter, refsAfter, indexAfter, _, reflogsAfter) = afterFailure
    assertEqual "rollback does not delete caller-replaced bytes" callerBytes retained
    assertEqual "append rollback leaves HEAD unchanged" headBefore headAfter
    assertEqual "append rollback leaves refs unchanged" refsBefore refsAfter
    assertEqual "append rollback leaves index unchanged" indexBefore indexAfter
    assertEqual "append rollback leaves reflogs unchanged" reflogsBefore reflogsAfter
    removeFile generatedPath
    retry <- commitAppendOnlyOperation repository config
    case retry of
      Left problem -> assertFailure ("retry after removing caller-owned bytes should succeed: " <> show problem)
      Right success -> assertEqual "retry creates the requested managed path" [genFilePath generated] (transactionCreatedPaths success)

assertBootstrapHookAsyncWinsDeleteFailure :: IO ()
assertBootstrapHookAsyncWinsDeleteFailure =
  withSystemTempDirectory "adrai bootstrap rollback async ordering" $ \temporary -> do
    let repositoryPath = temporary </> "repository"
    initTestRepository repositoryPath
    parentText <- commitFile repositoryPath "seed.txt" "seed\n"
    repository <-
      discoverRepository systemGit repositoryPath >>= \case
        Left problem -> assertFailure (show problem) >> fail "unreachable"
        Right discovered -> pure discovered
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
    let (headBefore, refsBefore, indexBefore, _, reflogsBefore) = before
        (headAfter, refsAfter, indexAfter, _, reflogsAfter) = afterFailure
    assertEqual "failed delete retains exactly transaction-created bytes" (genFileBytes generated) retained
    assertEqual "bootstrap rollback leaves HEAD unchanged" headBefore headAfter
    assertEqual "bootstrap rollback leaves refs unchanged" refsBefore refsAfter
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
  :: String
  -> (Repository -> TransactionConfig -> IO () -> IO (Either TransactionError TransactionResult))
  -> IO ()
assertPreCasRedirect label runTransaction =
  withSystemTempDirectory ("adrai pre-CAS redirect " <> label) $ \temporary -> do
    let repositoryPath = temporary </> "repository"
        outsidePath = temporary </> "outside"
        redirectedParent = repositoryPath </> "architecture" </> "adrai"
        architectureParent = repositoryPath </> "architecture"
        redirect = do
          removeDirectoryRecursive redirectedParent
          createDirectoryRedirect outsidePath redirectedParent
    initTestRepository repositoryPath
    parentText <- commitFile repositoryPath "seed.txt" "seed\n"
    createDirectory outsidePath
    repository <-
      discoverRepository systemGit repositoryPath >>= \case
        Left problem -> assertFailure (show problem) >> fail "unreachable"
        Right discovered -> pure discovered
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
  :: String
  -> (Repository -> TransactionConfig -> IO () -> IO (Either TransactionError TransactionResult))
  -> IO ()
assertPostCasRedirect label runTransaction =
  withSystemTempDirectory ("adrai post-CAS redirect " <> label) $ \temporary -> do
    let repositoryPath = temporary </> "repository"
        outsidePath = temporary </> "outside"
        redirectedParent = repositoryPath </> "architecture" </> "adrai"
        architectureParent = repositoryPath </> "architecture"
        stagedPath = repositoryPath </> "caller-post-cas.bin"
        stagedBytes = BS.pack [0, 255, 17, 10, 128, 64, 3, 2, 1]
        redirect = do
          removeDirectoryRecursive redirectedParent
          createDirectoryRedirect outsidePath redirectedParent
    initTestRepository repositoryPath
    parentText <- commitFile repositoryPath "seed.txt" "seed\n"
    BS.writeFile stagedPath stagedBytes
    _ <- gitSuccess repositoryPath ["add", "--", "caller-post-cas.bin"] BS.empty
    createDirectory outsidePath
    repository <-
      discoverRepository systemGit repositoryPath >>= \case
        Left problem -> assertFailure (show problem) >> fail "unreachable"
        Right discovered -> pure discovered
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

assertBootstrapPostCasCancellationAuthority :: IO ()
assertBootstrapPostCasCancellationAuthority =
  assertBootstrapCancellationAuthority
    "post-CAS"
    (\dependencies -> dependencies {bootstrapBeforePostCasIndexRefresh = throwIO ThreadKilled})

assertBootstrapPostUpdateRefCancellationAuthority :: IO ()
assertBootstrapPostUpdateRefCancellationAuthority =
  assertBootstrapCancellationAuthority
    "post-update-ref"
    (\dependencies -> dependencies {bootstrapAfterSuccessfulCasBeforeBookkeeping = throwIO ThreadKilled})

assertBootstrapCancellationAuthority :: String -> (BootstrapDependencies -> BootstrapDependencies) -> IO ()
assertBootstrapCancellationAuthority label adjustDependencies =
  withSystemTempDirectory ("adrai bootstrap " <> label <> " cancellation") $ \temporary -> do
    let repositoryPath = temporary </> "repository"
        stagedPath = repositoryPath </> "caller-bootstrap-cancellation.bin"
        stagedBytes = BS.pack [255, 0, 13, 10, 128, 64, 9, 8, 7]
    initTestRepository repositoryPath
    parentText <- commitFile repositoryPath "seed.txt" "seed\n"
    BS.writeFile stagedPath stagedBytes
    _ <- gitSuccess repositoryPath ["add", "--", "caller-bootstrap-cancellation.bin"] BS.empty
    repository <-
      discoverRepository systemGit repositoryPath >>= \case
        Left problem -> assertFailure (show problem) >> fail "unreachable"
        Right discovered -> pure discovered
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

assertUnbornCandidateCancellation :: IO ()
assertUnbornCandidateCancellation =
  withSystemTempDirectory "adrai unborn candidate cancellation" $ \temporary -> do
    let repositoryPath = temporary </> "repository"
    initTestRepository repositoryPath
    repository <-
      discoverRepository systemGit repositoryPath >>= \case
        Left problem -> assertFailure (show problem) >> fail "unreachable"
        Right discovered -> pure discovered
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

assertUnreadableFirstCommitInspection :: IO ()
assertUnreadableFirstCommitInspection =
  withSystemTempDirectory "adrai unreadable first commit inspection" $ \temporary -> do
    let repositoryPath = temporary </> "repository"
    initTestRepository repositoryPath
    repository <-
      discoverRepository systemGit repositoryPath >>= \case
        Left problem -> assertFailure (show problem) >> fail "unreachable"
        Right discovered -> pure discovered
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
    headAfter <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD"] BS.empty
    committedBytes <- gitSuccess repositoryPath ["show", Text.unpack headAfter <> ":" <> Text.unpack (repoPathText (genFilePath generated))] BS.empty
    worktreeBytes <- BS.readFile generatedPath
    indexAfter <- doesFileExist (repositoryGitDir repository </> "index")
    assertEqual "uncertain authority preserves first-commit bytes" (genFileBytes generated) committedBytes
    assertEqual "uncertain authority preserves worktree bytes" (genFileBytes generated) worktreeBytes
    assertEqual "uncertain authority does not materialize or refresh caller index" indexBefore indexAfter

transactionObservableState :: FilePath -> IO (BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString)
transactionObservableState repositoryPath = do
  headOid <- gitSuccess repositoryPath ["rev-parse", "HEAD"] BS.empty
  refs <- gitSuccess repositoryPath ["show-ref", "--head"] BS.empty
  index <- gitSuccess repositoryPath ["ls-files", "--stage", "-z"] BS.empty
  status <- gitSuccess repositoryPath ["status", "--porcelain=v1", "--untracked-files=all", "-z"] BS.empty
  reflogs <- gitSuccess repositoryPath ["reflog", "show", "--all", "--format=%H%x00%gs"] BS.empty
  pure (headOid, refs, index, status, reflogs)

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
