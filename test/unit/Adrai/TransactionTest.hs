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
import Adrai.GitTestSupport (commitFile, createWorktree, gitSuccess, initTestRepository, outputText)
import Adrai.Provenance
  ( ProvenanceCapsuleInput (..),
    ProvenanceObjectId (..),
    mkEventKind,
    mkProvenanceCapsule,
    semanticDigest,
  )
import Adrai.Service.Transaction
  ( GeneratedFile (..),
    AppendOnlyDependencies (..),
    TransactionConfig (..),
    TransactionError (..),
    TransactionResult (..),
    commitAppendOnlyOperation,
    commitAppendOnlyOperationWith,
    commitTree,
    defaultAppendOnlyDependencies,
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
import Control.Exception (AsyncException (ThreadKilled), SomeException, throwIO, try)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import Data.List (isPrefixOf)
import System.FilePath ((</>), takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Transaction contract"
    [ testCase "Git plumbing accepts one bare 40- or 64-hex OID with trailing stdout framing" $
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
            _ <- commitFile repositoryPath ".gitattributes" "*.md filter=adrai-fail\n"
            parentText <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD"] BS.empty
            _ <- gitSuccess repositoryPath ["config", "filter.adrai-fail.clean", "false"] BS.empty
            _ <- gitSuccess repositoryPath ["config", "filter.adrai-fail.required", "true"] BS.empty
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
                hookPath = repositoryPath </> ".git" </> "adrai-no-hooks" </> "reference-transaction"
            initTestRepository repositoryPath
            parentText <- commitFile repositoryPath "seed.txt" "seed\n"
            BS.writeFile stagedPath stagedBytes
            _ <- gitSuccess repositoryPath ["add", "--", "staged.bin"] BS.empty
            indexBefore <- BS.readFile (repositoryPath </> ".git" </> "index")
            BS.writeFile hookPath "#!/bin/sh\nif test \"$1\" = prepared; then\n  exit 1\nfi\nexit 0\n"
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
            commitAppendOnlyOperation repository config >>= \case
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
                hookPath = repositoryPath </> ".git" </> "adrai-no-hooks" </> "reference-transaction"
            initTestRepository repositoryPath
            parentText <- commitFile repositoryPath "seed.txt" "seed\n"
            _ <- gitSuccess repositoryPath ["branch", "other", "HEAD"] BS.empty
            BS.writeFile hookPath "#!/bin/sh\nif test \"$1\" = prepared; then\n  exit 1\nfi\nif test \"$1\" = aborted; then\n  printf 'ref: refs/heads/other\\n' > \"$GIT_DIR/HEAD\"\nfi\nexit 0\n"
            repository <- discoverRepository systemGit repositoryPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
            parent <- requireGitOid parentText
            (operationText, generated) <- transactionGeneratedFile parent
            let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
                config = TransactionConfig operationText "adrai: pinned ref" (Map.fromList [("Objects", "pinned-ref")]) parent [generated]
            commitAppendOnlyOperation repository config >>= \case
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
                hookPath = repositoryPath </> ".git" </> "adrai-no-hooks" </> "reference-transaction"
            initTestRepository repositoryPath
            parentText <- commitFile repositoryPath "seed.txt" "seed\n"
            BS.writeFile hookPath "#!/bin/sh\nif test \"$1\" = prepared; then\n  exit 1\nfi\nexit 0\n"
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
            commitAppendOnlyOperationWith dependencies repository config >>= \case
              Left (Stage8UpdateRef _) -> pure ()
              Left problem -> assertFailure ("expected original Stage8 failure, got " <> show problem)
              Right result -> assertFailure ("expected Stage8 failure, got " <> show result)
            generatedAfter <- BS.readFile generatedPath
            assertEqual "authoritative new outcome retains the generated bytes" (genFileBytes generated) generatedAfter
      , testCase "ambiguous target ref outcome fails closed and preserves generated paths" $
          withSystemTempDirectory "adrai transaction ambiguous ref" $ \temporary -> do
            let repositoryPath = temporary </> "repository"
                hookPath = repositoryPath </> ".git" </> "adrai-no-hooks" </> "reference-transaction"
                otherCommit = GitOid "1111111111111111111111111111111111111111"
            initTestRepository repositoryPath
            parentText <- commitFile repositoryPath "seed.txt" "seed\n"
            BS.writeFile hookPath "#!/bin/sh\nif test \"$1\" = prepared; then\n  exit 1\nfi\nexit 0\n"
            repository <- discoverRepository systemGit repositoryPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
            parent <- requireGitOid parentText
            (operationText, generated) <- transactionGeneratedFile parent
            let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
                dependencies = defaultAppendOnlyDependencies { appendOnlyInspectRef = \_ _ _ _ -> pure (Right otherCommit) }
                config = TransactionConfig operationText "adrai: ambiguous ref" (Map.fromList [("Objects", "ambiguous-ref")]) parent [generated]
            commitAppendOnlyOperationWith dependencies repository config >>= \case
              Left (RollbackFailed detail) -> assertBool "ambiguous ref is reported as a failed rollback" ("target ref changed" `Text.isInfixOf` detail)
              Left problem -> assertFailure ("expected RollbackFailed, got " <> show problem)
              Right result -> assertFailure ("expected ambiguous ref failure, got " <> show result)
            generatedAfter <- BS.readFile generatedPath
            assertEqual "ambiguous ref outcome preserves generated bytes" (genFileBytes generated) generatedAfter
      , testCase "target ref inspection failure fails closed and preserves generated paths" $
          withSystemTempDirectory "adrai transaction unreadable ref" $ \temporary -> do
            let repositoryPath = temporary </> "repository"
                hookPath = repositoryPath </> ".git" </> "adrai-no-hooks" </> "reference-transaction"
            initTestRepository repositoryPath
            parentText <- commitFile repositoryPath "seed.txt" "seed\n"
            BS.writeFile hookPath "#!/bin/sh\nif test \"$1\" = prepared; then\n  exit 1\nfi\nexit 0\n"
            repository <- discoverRepository systemGit repositoryPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
            parent <- requireGitOid parentText
            (operationText, generated) <- transactionGeneratedFile parent
            let generatedPath = repositoryPath </> Text.unpack (repoPathText (genFilePath generated))
                dependencies = defaultAppendOnlyDependencies { appendOnlyInspectRef = \_ _ _ _ -> pure (Left (RollbackFailed "injected target-ref read failure")) }
                config = TransactionConfig operationText "adrai: unreadable ref" (Map.fromList [("Objects", "unreadable-ref")]) parent [generated]
            commitAppendOnlyOperationWith dependencies repository config >>= \case
              Left (RollbackFailed detail) -> assertBool "inspection failure is retained" ("injected target-ref read failure" `Text.isInfixOf` detail)
              Left problem -> assertFailure ("expected RollbackFailed, got " <> show problem)
              Right result -> assertFailure ("expected inspection failure, got " <> show result)
            generatedAfter <- BS.readFile generatedPath
            assertEqual "inspection failure preserves generated bytes" (genFileBytes generated) generatedAfter
      , testCase "rollback inspection callback receives the ref pinned before HEAD changes" $
          withSystemTempDirectory "adrai transaction observed pinned ref" $ \temporary -> do
            let repositoryPath = temporary </> "repository"
                hookPath = repositoryPath </> ".git" </> "adrai-no-hooks" </> "reference-transaction"
            initTestRepository repositoryPath
            parentText <- commitFile repositoryPath "seed.txt" "seed\n"
            _ <- gitSuccess repositoryPath ["branch", "other", "HEAD"] BS.empty
            BS.writeFile hookPath "#!/bin/sh\nif test \"$1\" = prepared; then\n  exit 1\nfi\nif test \"$1\" = aborted; then\n  printf 'ref: refs/heads/other\\n' > \"$GIT_DIR/HEAD\"\nfi\nexit 0\n"
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
            commitAppendOnlyOperationWith dependencies repository config >>= \case
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
            _ <- commitFile repositoryPath ".gitattributes" "*.md filter=adrai-fail\n"
            parentText <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD"] BS.empty
            _ <- gitSuccess repositoryPath ["config", "filter.adrai-fail.clean", "false"] BS.empty
            _ <- gitSuccess repositoryPath ["config", "filter.adrai-fail.required", "true"] BS.empty
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
      ]

adraiTemporaryIndexes :: FilePath -> IO [FilePath]
adraiTemporaryIndexes repositoryPath =
  filter ("adrai-index-" `isPrefixOf`) <$> listDirectory (repositoryPath </> ".git")

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
