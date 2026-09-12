{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Adrai.GitBatchTest (tests) where

import Adrai.Git
import Adrai.GitTestSupport
import Adrai.Provenance (mkGitOid)
import Adrai.Types (RepoPath, repoPathText)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent (MVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, threadDelay, tryPutMVar, withMVar)
import Control.Exception (SomeException, finally, fromException, onException, throwIO, throwTo, try)
import Control.Monad (forM_, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft)
import Data.IORef (modifyIORef', newIORef, readIORef)
import GHC.Clock (getMonotonicTimeNSec)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Directory (copyFile, createDirectoryIfMissing, createFileLink, doesDirectoryExist, doesFileExist, listDirectory, removeFile, removePathForcibly)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import System.FilePath (isRelative, makeRelative, normalise, splitDirectories, (</>))
import System.IO.Error (tryIOError)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory, withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process.Typed (proc, readProcess)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase, testCaseSteps)

requireHead :: String -> [a] -> a
requireHead description = \case
  value : _ -> value
  [] -> error (description <> " must be non-empty")

requireLast :: String -> [a] -> a
requireLast description values =
  case reverse values of
    value : _ -> value
    [] -> error (description <> " must be non-empty")

tests :: TestTree
tests =
  withResource acquireRepositorySeed releaseRepositorySeed $ \_ ->
    testGroup
      "Git batch and safe reads"
      [ testCase "tree path groups de-duplicate requests and retain the 256-path bound" $ do
        let revision = requireOid "1111111111111111111111111111111111111111"
            paths = [requireRepoPath ("bounded/" <> Text.pack (show number) <> ".md") | number <- [0 :: Int .. 256]]
        treePathBatchRequestCount "git" "" revision (requireHead "bounded tree-path fixture" paths : paths) @?= Right 2,
      testCase "termination wait policy classifies bounded exit proofs" $
        sequence_ [ terminationWaitDecision BeforeTermination TerminationSignaled @?= TerminationExitProven,
          terminationWaitDecision BeforeTermination TerminationTimedOut @?= TerminationAttemptRequired,
          terminationWaitDecision AfterTerminationSuccess TerminationSignaled @?= TerminationExitProven,
          terminationWaitDecision AfterTerminationSuccess TerminationTimedOut @?= TerminationWaitFailed,
          terminationWaitDecision AfterTerminationSuccess TerminationUnexpected @?= TerminationWaitFailed,
          terminationWaitDecision AfterTerminationError TerminationSignaled @?= TerminationExitProven,
          terminationWaitDecision AfterTerminationError TerminationTimedOut @?= TerminationErrorRethrow,
          terminationWaitDecision AfterTerminationError TerminationUnexpected @?= TerminationWaitFailed
        ],
      testCase "injectable captured-exit poll fails within its finite budget" $ do
        probes <- newIORef (0 :: Int)
        result <- boundedExitPoll 20000 (pure ()) (modifyIORef' probes (+ 1) >> pure Nothing)
        result @?= Nothing
        observedProbes <- readIORef probes
        observedProbes @?= 3,
      testCase "injectable termination request times out without joining its worker" $ do
        result <- boundedTerminationRequest 1000 (threadDelay 200000)
        assertBool "slow termination request must fail closed" (isLeft result),
      testCase "multi-revision tree observations batch exact Unicode paths and preserve absences" $
        withRepository $ \repository discovered -> do
          let present = requireRepoPath "arkitektur/beslutninger/København.md"
              missing = requireRepoPath "arkitektur/beslutninger/missing.md"
          _ <- commitFile repository "arkitektur/beslutninger/København.md" "exact path bytes\n"
          revision <- resolveHead discovered
          expected <- lookupTreeEntriesAt discovered revision [present]
          blobOid <- case expected of
            Right entries -> case Map.lookup present entries of
              Just (Just entry) -> pure (gitTreeOid entry)
              other -> assertFailure ("missing fixture entry: " <> show other) >> fail "unreachable"
            Left problem -> assertFailure (show problem) >> fail "unreachable"
          lookupTreeObjectInfoAtRevisions discovered (Map.singleton revision (Set.fromList [present, missing])) [blobOid] >>= \case
            Left problem -> assertFailure (show problem)
            Right observed -> do
              let entries = Map.findWithDefault Map.empty revision observed
              Map.lookup missing entries @?= Just Nothing
              case Map.lookup present entries of
                Just (Just info) -> do
                  objectInfoOid info @?= blobOid
                  objectInfoType info @?= GitBlobObject
                other -> assertFailure ("missing Unicode path observation: " <> show other),
      testCase "tree-path observations retain requested empty revisions alongside populated revisions" $
        withRepository $ \repository discovered -> do
          emptyRevision <- resolveHead discovered
          let populatedPath = requireRepoPath "populated-safe.md"
          _ <- commitFile repository "populated-safe.md" "populated bytes\n"
          populatedRevision <- resolveHead discovered
          lookupTreeObjectInfoAtRevisions discovered (Map.fromList [(emptyRevision, Set.empty), (populatedRevision, Set.singleton populatedPath)]) [] >>= \case
            Left problem -> assertFailure (show problem)
            Right observed -> do
              Map.lookup emptyRevision observed @?= Just Map.empty
              case Map.lookup populatedRevision observed >>= Map.lookup populatedPath of
                Just (Just info) -> objectInfoType info @?= GitBlobObject
                other -> assertFailure ("populated revision did not retain its exact path: " <> show other),
      testCase "tree-path observations retain an all-empty request map" $
        withSharedRepository $ \_ discovered -> do
          revision <- resolveHead discovered
          observed <- lookupTreeObjectInfoAtRevisions discovered (Map.singleton revision Set.empty) []
          observed @?= Right (Map.singleton revision Map.empty),
      testCase "correlated tree-path observations match ls-tree across history, divergence, merge parents, and shared mode objects" treePathRealGitParity,
      testCase "spaced tree paths retain the literal ls-tree fallback" $
        withRepository $ \repository discovered -> do
          let present = requireRepoPath "architecture/spaced directory/decision file.md"
              missing = requireRepoPath "architecture/spaced directory/missing file.md"
          _ <- commitFile repository "architecture/spaced directory/decision file.md" "fallback bytes\n"
          revision <- resolveHead discovered
          expected <- lookupTreeEntriesAt discovered revision [present, missing]
          lookupTreeObjectInfoAtRevisions discovered (Map.singleton revision (Set.fromList [present, missing])) [] >>= \case
            Left problem -> assertFailure (show problem)
            Right observed ->
              case expected of
                Left problem -> assertFailure (show problem)
                Right oracle -> do
                  let oracleInfo = fmap (fmap (\entry -> GitObjectInfo (gitTreeOid entry) (gitTreeObjectType entry) 0)) oracle
                      actual = Map.findWithDefault Map.empty revision observed
                  Map.lookup missing actual @?= Just Nothing
                  case (Map.lookup present oracleInfo, Map.lookup present actual) of
                    (Just (Just expectedInfo), Just (Just actualInfo)) -> do
                      objectInfoOid actualInfo @?= objectInfoOid expectedInfo
                      objectInfoType actualInfo @?= objectInfoType expectedInfo
                    other -> assertFailure ("literal fallback did not retain the requested path: " <> show other),
      testCase "multi-revision tree observations reject missing and non-commit revisions" $
        withRepository $ \repository discovered -> do
          let path = requireRepoPath "missing.md"
              missingRevision = requireOid (Text.replicate 40 "f")
          blobRevision <- requireOid <$> hashObject repository "not a commit"
          lookupTreeObjectInfoAtRevisions discovered (Map.singleton missingRevision (Set.singleton path)) [] >>= \case
            Left (GitObjectMissing returned) -> returned @?= missingRevision
            result -> assertFailure ("expected missing revision failure, got " <> show result)
          lookupTreeObjectInfoAtRevisions discovered (Map.singleton blobRevision (Set.singleton path)) [] >>= \case
            Left (GitObjectTypeMismatch returned GitCommitObject GitBlobObject) -> returned @?= blobRevision
            result -> assertFailure ("expected non-commit revision failure, got " <> show result)
          validRevision <- resolveHead discovered
          lookupTreeObjectInfoAtRevisions discovered (Map.singleton validRevision (Set.singleton path)) [missingRevision] >>= \case
            Left (GitObjectMissing returned) -> returned @?= missingRevision
            result -> assertFailure ("expected missing sealed blob failure, got " <> show result)
          lookupTreeObjectInfoAtRevisions discovered (Map.singleton validRevision (Set.singleton path)) [validRevision] >>= \case
            Left (GitObjectTypeMismatch returned GitBlobObject GitCommitObject) -> returned @?= validRevision
            result -> assertFailure ("expected non-blob sealed object failure, got " <> show result),
      testCase "multi-revision tree reconciliation retains literal fallback only for ineligible paths" $ do
        let revision = requireOid "1111111111111111111111111111111111111111"
            otherRevision = requireOid "2222222222222222222222222222222222222222"
            path = requireRepoPath ("unicode space/" <> Text.replicate 600 "x" <> ".md")
            requested = [path]
            entry = GitTreeEntry path revision GitBlobObject GitRegularFile
            otherEntry = GitTreeEntry (requireRepoPath "other.md") revision GitBlobObject GitRegularFile
            alteredEntry = GitTreeEntry path otherRevision GitBlobObject GitRegularFile
            plan = Map.fromList
              [ (revision, Set.singleton path)
              , (otherRevision, Set.singleton (requireRepoPath "second.md"))
              ]
            largePlan = Map.singleton revision (Set.fromList [requireRepoPath ("bounded/" <> Text.pack (show number) <> ".md") | number <- [0 :: Int .. 256]])
        reconcileExactTreeEntries requested [entry] @?= Right (Map.singleton path (Just entry))
        reconcileExactTreeEntries requested [] @?= Right (Map.singleton path Nothing)
        assertBool "unexpected success path fails" (isLeft (reconcileExactTreeEntries requested [otherEntry]))
        assertBool "cross-window substituted success fails against this window" (isLeft (reconcileExactTreeEntries [path] [otherEntry]))
        assertBool "identical duplicate success fails" (isLeft (reconcileExactTreeEntries requested [entry, entry]))
        assertBool "duplicate success path fails" (isLeft (reconcileExactTreeEntries requested [entry, alteredEntry]))
        assertBool "withheld path remains an explicit absence" ((Map.lookup path <$> reconcileExactTreeEntries requested []) == Right (Just Nothing))
        objectInfoBatchSessionCount [revision, otherRevision] @?= 1
        treePathRevisionLsTreeChildCount "git" "" plan @?= Right 1
        treePathRevisionLsTreeChildCount "git" "" largePlan @?= Right 0,
      testCase "tree-proof-shaped bare object windows and line-safe paths collapse to persistent sessions" $ do
        let oidText number =
              let digits = Text.pack (show number)
               in Text.replicate (40 - Text.length digits) "0" <> digits
            revisions = map (requireOid . oidText) [1 :: Int .. 54]
            paths = Set.fromList [requireRepoPath ("tree-proof/" <> Text.pack (show number) <> ".md") | number <- [0 :: Int .. 255]]
            expectedBlobOids = map (requireOid . oidText) [1000 :: Int .. 1000 + (42 * 256 - 54) - 1]
            plan = Map.fromList [(revision, paths) | revision <- revisions]
            observed =
              Map.singleton
                (requireHead "tree-proof revision fixture" revisions)
                (Map.singleton (requireHead "tree-proof path fixture" (Set.toAscList paths)) (Just (GitTreeEntry (requireHead "tree-proof path fixture" (Set.toAscList paths)) (requireHead "tree-proof blob fixture" expectedBlobOids) GitBlobObject GitRegularFile)))
        objectBatchRequestCount (revisions <> expectedBlobOids) @?= 42
        objectInfoBatchSessionCount (revisions <> expectedBlobOids) @?= 1
        treePathRevisionLsTreeChildCount "git" "" plan @?= Right 0
        treePathRevisionBatchRequestCount "git" "" plan expectedBlobOids @?= Right 2
        treePathRevisionProcessCount "git" "" plan expectedBlobOids observed @?= Right 2,
      testCase "ls-tree argv windows preserve spaced paths and reject one oversized path" $ do
        let revision = requireOid "1111111111111111111111111111111111111111"
            spaced = requireRepoPath "architecture/space path.md"
            -- Each path is admissible under the conservative serialized argv
            -- bound, but the pair must be split into separate windows.
            nearBudget = requireRepoPath (Text.replicate 8000 "x")
            secondNearBudget = requireRepoPath (Text.replicate 7999 "y" <> "z")
            oversized = requireRepoPath (Text.replicate (treePathArgumentByteLimit + 1) "x")
        case treePathArgumentWindows revision [spaced, nearBudget, secondNearBudget] of
          Left problem -> assertFailure (show problem)
          Right windows -> do
            concat windows @?= [spaced, nearBudget, secondNearBudget]
            assertBool "every argv window stays within item bound" (all ((<= 256) . length) windows)
            length windows @?= 2
            treePathBatchRequestCount "git" "" revision [spaced, nearBudget, secondNearBudget] @?= Right (length windows)
        assertBool "one path over the command-line budget fails closed" (isLeft (treePathArgumentWindows revision [oversized])),
      testCase "ls-tree argv accounting includes executable, repository and 256 quoted paths" $ do
        let revision = requireOid "1111111111111111111111111111111111111111"
            executable = "C:/" <> replicate 400 'x' <> "/git executable with spaces.exe"
            repository = "C:/" <> replicate 400 'r' <> "/repository with spaces"
            paths = [requireRepoPath ("quoted path/" <> Text.pack (show index) <> " file.md") | index <- [1 :: Int .. 256]]
            oversized = requireRepoPath (Text.replicate treePathArgumentByteLimit "x")
        case treePathArgumentWindowsForCommand executable repository revision paths of
          Left problem -> assertFailure (show problem)
          Right windows -> do
            concat windows @?= Map.keys (Map.fromList [(path, ()) | path <- paths])
            assertBool "all windows retain item cap" (all ((<= 256) . length) windows)
        assertBool "single serialized over-limit path fails" (isLeft (treePathArgumentWindowsForCommand executable repository revision [oversized])),
      testCase "multi-revision tree observations reject malformed child output" $
        withSharedRepository $ \_repository discovered ->
          withSystemTempDirectory "adrai malformed tree cat-file" $ \temporary -> do
            let fakeGit = temporary </> "malformed-git.cmd"
                malformedRepository = discovered {repositoryClient = GitClient fakeGit}
                path = requireRepoPath "missing.md"
            BS.writeFile fakeGit "@echo off\r\necho malformed\r\n"
            revision <- resolveHead discovered
            lookupTreeObjectInfoAtRevisions malformedRepository (Map.singleton revision (Set.singleton path)) [] >>= \case
               Left (GitInvalidOutput "cat-file batch-check" _) -> pure ()
               result -> assertFailure ("expected malformed tree protocol failure, got " <> show result),
      testCase "correlated batch failure never launches an ineligible fallback" treePathBatchFailureShortCircuitsFallback,
      testCase "correlated tree-path protocol rejects a present response with the wrong token" $
        treePathProtocolFailure "wrong-token" "1111111111111111111111111111111111111111 blob 1 wrong-token",
      testCase "correlated tree-path protocol rejects a wrong missing expression" $
        treePathProtocolFailure "wrong-missing" "not-the-requested-expression missing",
      testCase "correlated tree-path protocol rejects a withheld response at EOF" $
        treePathProtocolFailure "withheld-response" "",
      testCase "correlated tree-path protocol rejects a malformed object OID" $
        treePathProtocolFailure "malformed-oid" "not-an-oid blob 1 adrai-tree-00000000",
      testCase "correlated tree-path protocol rejects a malformed object type" $
        treePathProtocolFailure "malformed-type" "1111111111111111111111111111111111111111 not-a-type 1 adrai-tree-00000000",
      testCase "correlated tree-path protocol rejects malformed object fields" $
        treePathProtocolFailure "malformed-fields" "1111111111111111111111111111111111111111 blob not-a-size adrai-tree-00000000",
      testCase "correlated tree-path protocol rejects reordered missing responses" treePathReorderedFailure,
      testCase "correlated tree-path protocol rejects a duplicate missing expression" treePathDuplicateExpressionFailure,
      testCase "correlated tree-path protocol rejects a replay across the 256-request window boundary" treePathCrossWindowReplayFailure,
      testCase "correlated tree-path protocol rejects trailing bytes after a valid missing response" treePathTrailingFailure,
      testCase "persistent batch-check nonzero early exit fails closed and reaps" $
        withSharedRepository $ \_repository discovered ->
          withSystemTempDirectory "adrai persistent batch-check failures" $ \temporary -> do
            let nonzeroGit = temporary </> "nonzero-git.cmd"
                path = requireRepoPath "missing.md"
            revision <- resolveHead discovered
            BS.writeFile nonzeroGit "@echo off\r\nexit /b 23\r\n"
            timeout (5 * 1000000) (lookupTreeObjectInfoAtRevisions (discovered {repositoryClient = GitClient nonzeroGit}) (Map.singleton revision (Set.singleton path)) []) >>= \case
              Nothing -> assertFailure "nonzero persistent batch-check child was not reaped promptly"
              Just (Left _) -> pure ()
              Just result -> assertFailure ("expected nonzero child failure, got " <> show result),
      testCase "multi-revision tree lookup cancellation reaps a silent native child and retry succeeds" directTreeCancellationReapsNativeChild,
      testCase "persistent native batch cancellation reaps a silent writer-blocking child and retries" persistentNativeBatchCancellation,
      testCase "persistent native batch-check cancellation reaps a silent writer-blocking child and retries" persistentNativeBatchCheckCancellation,
      testCase "persistent batch-check cancellation after trailing proof reaps a still-alive native child and retries" persistentFinishSuccessCancellation,
      testCase "normal runGit completion has a bounded real success sentinel" $
        withSharedRepository $ \_ discovered ->
          withSystemTempDirectory "adrai bounded git completion" $ \temporary -> do
            let fakeGit = temporary </> "bounded-success.cmd"
                slowRepository = discovered {repositoryClient = GitClient fakeGit}
            BS.writeFile fakeGit "@echo off\r\necho bounded-success\r\n"
            runRepository slowRepository "bounded successful command" [] BS.empty >>= \case
              Right result -> do
                processExitCode result @?= ExitSuccess
                processStdout result @?= "bounded-success\r\n"
              Left problem -> assertFailure ("bounded normal completion failed: " <> show problem),
      testCase "Windows PID-query failures are not classified as an absent child" $
        sequence_
          [ classifyWindowsProcessQuery (ExitFailure 1) "absent" "" @?= Left "PowerShell exit: ExitFailure 1",
            classifyWindowsProcessQuery ExitSuccess "absent" "access denied" @?= Left "PowerShell stderr: access denied",
            classifyWindowsProcessQuery ExitSuccess "" "" @?= Left "PowerShell returned an unknown PID state: ",
            classifyWindowsProcessQuery ExitSuccess "present" "" @?= Right True,
            classifyWindowsProcessQuery ExitSuccess "absent" "" @?= Right False
          ],
      testCase "persistent sessions use ordered bounded exchanges and reap protocol failures" persistentProtocolContract,
      testCase "persistent header EOF latches its first error, reaps, and rejects later IO" persistentSessionFailureLatch,
      testCase "persistent session rejects trailing bytes at close and fresh retry succeeds" persistentSessionTrailingClose,
      testCase "persistent session cancellation during withheld EOF closes captured closures and reaps exact helper" persistentSessionWithheldEofCancellation,
      testCase "post-callback cancellation closes captured session without protocol IO" persistentSessionPostCallbackCancellation,
      testCase "owner close preserves session-closed while canceling a blocked fold callback" persistentCallbackCancellationPreservesSessionClosed,
      testCase "owner close reports blocked fold cleanup failure and reaps exact helper" persistentCallbackCancellationReportsCleanupFailure,
      testCase "persistent owner return closes an active withheld fold and reaps exact helper" (persistentOwnerFinalization False),
      testCase "persistent owner throw closes an active withheld fold and reaps exact helper" (persistentOwnerFinalization True),
      testCase "real 513-object later-window failure latches, reaps, and permits a fresh retry" persistentLaterWindowFailure,
      testCase "persistent sessions serialize complete bounded fold plans" persistentSessionSerializesWholePlan,
      testCase "captured persistent session is closed with no post-close protocol IO" persistentSessionClosedCapture,
      testCase "empty public blob folds launch no protocol child" emptyPublicBlobFoldsLaunchNothing,
      testCase "ordered folds preserve duplicates across the 256-request boundary" $
        withRepository $ \repository discovered -> do
          firstOid <- requireOid <$> hashObject repository "boundary first"
          secondOid <- requireOid <$> hashObject repository "boundary second"
          let requested = replicate 255 firstOid <> [secondOid, firstOid, secondOid]
          foldBlobBatchInOrder discovered requested [] (\seen blob -> pure (seen <> [gitBlobOid blob]))
            >>= (@?= Right requested),
      testCase "public blob folds keep all bounded windows on one non-buffered child" publicBlobFoldsUseOnePersistentChild,
      testCase "missing objects, type mismatch, and invalid UTF-8 are structured" $
        withRepository $ \repository discovered -> do
          let missing = requireOid (Text.replicate 40 "f")
          batchObjectInfo discovered [missing] >>= (@?= Right (Map.singleton missing Nothing))
          readBlobBatch discovered [missing] >>= (@?= Left (GitObjectMissing missing))
          commit <- resolveHead discovered
          readBlobBatch discovered [commit] >>= (@?= Left (GitObjectTypeMismatch commit GitBlobObject GitCommitObject))
          invalidText <- hashObject repository (BS.pack [0x66, 0x80])
          let invalidOid = requireOid invalidText
          readUtf8BlobBatch discovered [invalidOid] >>= (@?= Left (GitInvalidUtf8Blob invalidOid)),
      testCase "persistent-session callback cancellation reaps its child and leaves the next window usable" $
        withRepository $ \repository discovered -> do
          blobText <- hashObject repository "callback"
          let blobOid = requireOid blobText
          attempted <-
            tryIOError
              ( ( withBlobBatchSession discovered $ \session -> do
                    loaded <- readBlobBatchFromSession session [blobOid]
                    case loaded of
                      Left problem -> pure (Left problem)
                      Right _ -> ioError (userError "caller callback failure")
                ) :: IO (Either GitError ())
              )
          case attempted of
            Left problem -> assertBool "original callback exception remains visible" ("caller callback failure" `Text.isInfixOf` Text.pack (show problem))
            Right result -> assertFailure ("expected callback IOException, got " <> show result)
          withBlobBatchSession discovered (\freshSession -> readBlobBatchFromSession freshSession [blobOid])
            >>= (@?= Right (Map.singleton blobOid (GitBlob blobOid "callback"))),
      testCase "persistent-session async cancellation reaps its child and leaves the next window usable" $
        withRepository $ \repository discovered -> do
          blobOid <- requireOid <$> hashObject repository "async callback"
          entered <- newEmptyMVar
          worker <-
            Async.async $
              withBlobBatchSession discovered $ \session -> do
                loaded <- readBlobBatchFromSession session [blobOid]
                case loaded of
                  Left problem -> pure (Left problem)
                  Right _ -> putMVar entered () >> threadDelay (60 * 1000000) >> pure (Right ())
          takeMVar entered
          Async.cancel worker
          cancelled <- Async.waitCatch worker
          assertBool "cancellation propagates" (isLeft cancelled)
          withBlobBatchSession discovered (\freshSession -> readBlobBatchFromSession freshSession [blobOid])
            >>= (@?= Right (Map.singleton blobOid (GitBlob blobOid "async callback"))),
      testCase "persistent malformed output reaps its child and leaves the next window usable" $
        withRepository $ \repository discovered -> do
          blobOid <- requireOid <$> hashObject repository "malformed callback"
          malformedPersistentOutputReapsChild discovered blobOid,
      testCase "tree parser rejects invalid UTF-8 paths and classifies non-regular entries" $
        withRepository $ \repository discovered -> do
          blobText <- hashObject repository "target"
          let blobOid = requireOid blobText
              invalidTreeInput = "100644 blob " <> TextEncoding.encodeUtf8 blobText <> "\tbad-" <> BS.pack [0x80] <> "\NUL"
          invalidTree <- outputText <$> gitSuccess repository ["mktree", "-z"] invalidTreeInput
          invalidCommit <- outputText <$> gitSuccess repository ["commit-tree", Text.unpack invalidTree] "invalid path\n"
          listTreeEntriesAt discovered (requireOid invalidCommit) [] >>= \case
            Left (GitInvalidUtf8Path _) -> pure ()
            result -> assertFailure ("expected invalid UTF-8 path, got " <> show result)
          let symlinkInput = "120000 blob " <> TextEncoding.encodeUtf8 (gitOidText blobOid) <> "\tlink\NUL"
          symlinkTree <- outputText <$> gitSuccess repository ["mktree", "-z"] symlinkInput
          symlinkCommit <- outputText <$> gitSuccess repository ["commit-tree", Text.unpack symlinkTree] "symlink\n"
          readRegularBlobAt discovered (requireOid symlinkCommit) (requireRepoPath "link") >>= \case
            Left (GitPathNotRegular _ GitSymbolicLink GitBlobObject) -> pure ()
            result -> assertFailure ("expected non-regular symlink, got " <> show result)
          currentCommit <- resolveHead discovered
          let classifiedInput =
                BS.concat
                  [ "100755 blob ",
                    TextEncoding.encodeUtf8 (gitOidText blobOid),
                    "\texecutable\NUL",
                    "120000 blob ",
                    TextEncoding.encodeUtf8 (gitOidText blobOid),
                    "\tlink\NUL",
                    "160000 commit ",
                    TextEncoding.encodeUtf8 (gitOidText currentCommit),
                    "\tsubmodule\NUL"
                  ]
          classifiedTree <- outputText <$> gitSuccess repository ["mktree", "-z"] classifiedInput
          classifiedCommit <- outputText <$> gitSuccess repository ["commit-tree", Text.unpack classifiedTree] "classified\n"
          listTreeEntriesAt discovered (requireOid classifiedCommit) [] >>= \case
            Left problem -> assertFailure (show problem)
            Right entries ->
              map (\entry -> (gitTreePath entry, gitTreeMode entry, gitTreeObjectType entry)) entries
                @?= [ (requireRepoPath "executable", GitExecutableFile, GitBlobObject),
                      (requireRepoPath "link", GitSymbolicLink, GitBlobObject),
                      (requireRepoPath "submodule", GitSubmodule, GitCommitObject)
                    ]
          readRegularBlobAt discovered (requireOid classifiedCommit) (requireRepoPath "executable") >>= \case
            Left problem -> assertFailure (show problem)
            Right blob -> gitBlobBytes blob @?= "target"
          readRegularBlobAt discovered (requireOid classifiedCommit) (requireRepoPath "submodule") >>= \case
            Left (GitPathNotRegular _ GitSubmodule GitCommitObject) -> pure ()
            result -> assertFailure ("expected non-regular submodule, got " <> show result),
      testCaseSteps "worktree reads retain logical paths, allow contained links, and reject escapes" $ \step ->
        withRepository $ \repository discovered -> do
          createDirectoryIfMissing True (repository </> "inside")
          BS.writeFile (repository </> "inside" </> "target.txt") "inside"
          readWorktreeFileBytes discovered (requireRepoPath "inside/target.txt") >>= (@?= Right (requireRepoPath "inside/target.txt", "inside"))
          readWorktreeFileBytes discovered (requireRepoPath "inside") >>= \case
            Left (GitWorktreePathError _ _) -> pure ()
            result -> assertFailure ("expected directory read rejection, got " <> show result)
          linkResult <- tryIOError (createFileLink (repository </> "inside" </> "target.txt") (repository </> "inside-link.txt"))
          case linkResult of
            Left _ -> step "file-link creation is unavailable on this platform; regular containment remains asserted"
            Right () ->
              readWorktreeFileBytes discovered (requireRepoPath "inside-link.txt") >>= (@?= Right (requireRepoPath "inside-link.txt", "inside"))
          withSystemTempDirectory "adrai outside" $ \outside -> do
            BS.writeFile (outside </> "outside.txt") "outside"
            outsideLink <- tryIOError (createFileLink (outside </> "outside.txt") (repository </> "outside-link.txt"))
            case outsideLink of
              Left _ -> step "outside-link creation is unavailable on this platform"
              Right () ->
                readWorktreeFileBytes discovered (requireRepoPath "outside-link.txt") >>= \case
                  Left (GitWorktreePathError _ _) -> pure ()
                  result -> assertFailure ("expected outside link rejection, got " <> show result)
          danglingLink <- tryIOError (createFileLink (repository </> "missing.txt") (repository </> "dangling.txt"))
          case danglingLink of
            Left _ -> step "dangling-link creation is unavailable on this platform"
            Right () -> do
              readWorktreeFileBytes discovered (requireRepoPath "dangling.txt") >>= \case
                Left (GitWorktreePathError _ _) -> pure ()
                result -> assertFailure ("expected dangling link rejection, got " <> show result)
              removeFile (repository </> "dangling.txt")
      ]

-- | Windows may transiently deny concurrent copies of the currently running
-- test executable.  Native helper fixtures copy that executable under distinct
-- dispatch basenames, so serialize only the brief copy operation; never hold
-- this lock while a helper is running or cancellation is exercised.
nativeHelperCopyLock :: MVar ()
nativeHelperCopyLock = unsafePerformIO (newMVar ())
{-# NOINLINE nativeHelperCopyLock #-}

-- Native helper fixtures have strict protocol and cancellation deadlines.  Keep
-- their complete lifetimes isolated so concurrent helper process and file I/O
-- cannot consume another fixture's bounded proof interval.
nativeHelperTimingFixtureLock :: MVar ()
{-# NOINLINE nativeHelperTimingFixtureLock #-}
nativeHelperTimingFixtureLock = unsafePerformIO (newMVar ())

copyNativeHelper :: FilePath -> FilePath -> IO ()
copyNativeHelper source destination = withMVar nativeHelperCopyLock $ \_ -> copyFile source destination

data RepositorySeed = RepositorySeed
  { repositorySeedRoot :: FilePath,
    repositorySeedPath :: FilePath,
    repositorySeedDiscovered :: Repository,
    repositorySeedBlobOids :: [GitOid],
    repositorySeedBlobs :: Map.Map GitOid GitBlob,
    repositorySeedPersistentOids :: [GitOid],
    repositorySeedPersistentBlobs :: Map.Map GitOid GitBlob
  }

repositorySeedSlot :: MVar (Maybe RepositorySeed)
repositorySeedSlot = unsafePerformIO (newMVar Nothing)
{-# NOINLINE repositorySeedSlot #-}

acquireRepositorySeed :: IO RepositorySeed
acquireRepositorySeed = do
  temporaryRoot <- getCanonicalTemporaryDirectory >>= (`createTempDirectory` "adrai-git-batch-seed")
  flip onException (removePathForcibly temporaryRoot) $ do
    let repository = temporaryRoot </> "repository"
    initTestRepository repository
    _ <- gitSuccess repository ["config", "core.hooksPath", ".git/adrai-no-hooks"] BS.empty
    _ <- commitFile repository "seed.txt" "seed"
    (blobOids, blobs) <- createBatchedObjectFixture repository "blob-input" "blob-" 257
    (persistentOids, persistentBlobs) <- createBatchedObjectFixture repository "persistent-protocol-input" "persistent-protocol-" 513
    packBatchedObjects repository (blobOids <> persistentOids)
    discovered <-
      discoverRepository systemGit repository >>= \case
        Left problem -> assertFailure (show problem) >> fail "unreachable"
        Right value -> pure value
    let seed =
          RepositorySeed
            { repositorySeedRoot = temporaryRoot,
              repositorySeedPath = repository,
              repositorySeedDiscovered = discovered,
              repositorySeedBlobOids = blobOids,
              repositorySeedBlobs = blobs,
              repositorySeedPersistentOids = persistentOids,
              repositorySeedPersistentBlobs = persistentBlobs
            }
    modifyMVar_ repositorySeedSlot (const (pure (Just seed)))
    pure seed

releaseRepositorySeed :: RepositorySeed -> IO ()
releaseRepositorySeed seed = do
  modifyMVar_ repositorySeedSlot (const (pure Nothing))
  removePathForcibly (repositorySeedRoot seed)

currentRepositorySeed :: IO RepositorySeed
currentRepositorySeed =
  readMVar repositorySeedSlot >>= \case
    Nothing -> assertFailure "Git batch repository seed was not acquired" >> fail "unreachable"
    Just seed -> pure seed

withRepository :: (FilePath -> Repository -> IO value) -> IO value
withRepository action =
  withSystemTempDirectory "adrai git batch" $ \temporary -> do
    seed <- currentRepositorySeed
    let repository = temporary </> "repository"
    copyDirectoryRecursively (repositorySeedPath seed) repository
    let discovered = relocateRepository (repositorySeedPath seed) repository (repositorySeedDiscovered seed)
    assertPrivateRepositoryCopy (repositorySeedPath seed) repository discovered
    action repository discovered

withSharedRepository :: (FilePath -> Repository -> IO value) -> IO value
withSharedRepository action = do
  seed <- currentRepositorySeed
  action (repositorySeedPath seed) (repositorySeedDiscovered seed)

relocateRepository :: FilePath -> FilePath -> Repository -> Repository
relocateRepository source destination repository =
  repository
    { repositoryWorktreeRoot = fmap relocate (repositoryWorktreeRoot repository),
      repositoryGitDir = relocate (repositoryGitDir repository),
      repositoryCommonDir = relocate (repositoryCommonDir repository),
      repositoryCommandDirectory = relocate (repositoryCommandDirectory repository)
    }
  where
    relocate path = normalise (destination </> makeRelative source path)

assertPrivateRepositoryCopy :: FilePath -> FilePath -> Repository -> IO ()
assertPrivateRepositoryCopy seedPath privatePath repository = do
  let pathFields =
        maybe [] pure (repositoryWorktreeRoot repository)
          <> [repositoryGitDir repository, repositoryCommonDir repository, repositoryCommandDirectory repository]
      isPrivate path =
        let relative = makeRelative privatePath path
         in isRelative relative
              && case splitDirectories relative of
                ".." : _ -> False
                _ -> True
  assertBool "relocated Repository retained a seed path" (all (not . containsPath seedPath) pathFields)
  assertBool "relocated Repository escaped its private copy" (all isPrivate pathFields)
  configBytes <- BS.readFile (repositoryGitDir repository </> "config")
  let seedPathBytes =
        [ BS8.pack (normalise seedPath),
          BS8.pack (map (\character -> if character == '\\' then '/' else character) (normalise seedPath))
        ]
      privateHooks = repositoryCommonDir repository </> "adrai-no-hooks"
  assertBool "private Git config retained the seed's absolute path" (all (not . (`BS.isInfixOf` configBytes)) seedPathBytes)
  assertBool "private Git config omitted its repository-relative hooks path" (".git/adrai-no-hooks" `BS.isInfixOf` configBytes)
  doesDirectoryExist privateHooks >>= assertBool "private Git hooks directory was not copied"
  hasAlternates <- doesFileExist (repositoryCommonDir repository </> "objects" </> "info" </> "alternates")
  assertBool "private Git objects unexpectedly share an alternates store" (not hasAlternates)
  where
    containsPath root path = Text.pack (normalise root) `Text.isInfixOf` Text.pack (normalise path)

copyDirectoryRecursively :: FilePath -> FilePath -> IO ()
copyDirectoryRecursively source destination = do
  createDirectoryIfMissing True destination
  entries <- listDirectory source
  forM_ entries $ \entry -> do
    let sourceEntry = source </> entry
        destinationEntry = destination </> entry
    isDirectory <- doesDirectoryExist sourceEntry
    if isDirectory
      then copyDirectoryRecursively sourceEntry destinationEntry
      else copyFile sourceEntry destinationEntry

createBatchedObjectFixture :: FilePath -> FilePath -> String -> Int -> IO ([GitOid], Map.Map GitOid GitBlob)
createBatchedObjectFixture repository directoryName payloadPrefix count = do
  let fixtureDirectory = repository </> directoryName
      paths = [directoryName <> "/" <> show index | index <- [0 :: Int .. count - 1]]
      payload index = TextEncoding.encodeUtf8 (Text.pack (payloadPrefix <> show index))
  createDirectoryIfMissing True fixtureDirectory
  sequence_ [BS.writeFile (repository </> path) (payload index) | (index, path) <- zip [0 :: Int ..] paths]
  oidTexts <- Text.lines . outputText <$> gitSuccess repository ["hash-object", "-w", "--stdin-paths"] (BS8.unlines (map BS8.pack paths))
  let oids = map requireOid oidTexts
      blobs = Map.fromList [(oid, GitBlob oid (payload index)) | (index, oid) <- zip [0 :: Int ..] oids]
  length oids @?= count
  Map.size blobs @?= count
  removePathForcibly fixtureDirectory
  pure (oids, blobs)

packBatchedObjects :: FilePath -> [GitOid] -> IO ()
packBatchedObjects repository oids = do
  let packBase = repository </> ".git" </> "objects" </> "pack" </> "pack"
      input = BS8.unlines (map (TextEncoding.encodeUtf8 . gitOidText) oids)
  _ <- gitSuccess repository ["pack-objects", packBase] input
  _ <- gitSuccess repository ["prune-packed"] BS.empty
  pure ()

-- | A real Git child validates the persistent request/flush lifecycle across
-- bounded windows, including failures that must reap the child.
persistentWindowFixture :: FilePath -> IO ([GitOid], Map.Map GitOid GitBlob)
persistentWindowFixture = persistentWindowFixtureWithCount 257

persistentWindowFixtureWithCount :: Int -> FilePath -> IO ([GitOid], Map.Map GitOid GitBlob)
persistentWindowFixtureWithCount count _repository = do
  seed <- currentRepositorySeed
  let windowOids = take count (repositorySeedPersistentOids seed)
      expectedOids = Set.fromList windowOids
      expectedBlobs = Map.restrictKeys (repositorySeedPersistentBlobs seed) expectedOids
  if count < 0 || length windowOids /= count
    then assertFailure ("persistent fixture requested unsupported object count: " <> show count) >> fail "unreachable"
    else pure (windowOids, expectedBlobs)

persistentOrderedContract :: IO ()
persistentOrderedContract =
  withSharedRepository $ \repository discovered -> do
    (windowOids, expectedBlobs) <- persistentWindowFixture repository
    let permuted = reverse windowOids <> take 4 windowOids
    withBlobBatchSession discovered
      (\session -> do
          empty <- readBlobBatchFromSession session []
          loaded <- readBlobBatchFromSession session permuted
          pure (empty >> loaded)
      )
      >>= (@?= Right expectedBlobs)

persistentLargeFirstContract :: IO ()
persistentLargeFirstContract =
  withRepository $ \repository discovered ->
    persistentLargeFirstBlobContract repository discovered

persistentFailureContract :: IO ()
persistentFailureContract =
  withSharedRepository $ \repository discovered -> do
    (windowOids, _) <- persistentWindowFixture repository
    let missingOid = repeatedOid 'f'
    assertPersistentFailureAndFreshSession discovered (requireHead "persistent-window fixture" windowOids) missingOid (GitObjectMissing missingOid)
    commit <- resolveHead discovered
    assertPersistentFailureAndFreshSession discovered (requireHead "persistent-window fixture" windowOids) commit (GitObjectTypeMismatch commit GitBlobObject GitCommitObject)

-- | Exercise the public fold APIs rather than the pre-existing explicit
-- session seam.  The small binary-safe helper records each launched wrapper
-- and serves arbitrarily many bounded request windows through one stdin/stdout
-- protocol stream, making a regression to one finite --buffer child per
-- window deterministic and visible.
publicBlobFoldsUseOnePersistentChild :: IO ()
publicBlobFoldsUseOnePersistentChild =
  withSharedRepository $ \_ discovered ->
    withSystemTempDirectory "adrai public persistent blob folds" $ \temporary -> do
      let fakeGit = temporary </> "persistent-blob-folds.cmd"
          helper = temporary </> "persistent-blob-folds.ps1"
          launches = temporary </> "launches.txt"
          arguments = temporary </> "arguments.txt"
          fakeRepository = discovered {repositoryClient = GitClient fakeGit}
          oids = [requireOid (Text.justifyRight 40 '0' (Text.pack (show number))) | number <- [1 :: Int .. 257]]
          ordered = take 255 oids <> [requireLast "persistent fold fixture" oids, requireHead "persistent fold fixture" oids, requireLast "persistent fold fixture" oids]
          launcher =
            "@echo off\r\n"
              <> "echo launch>> \"" <> BS8.pack launches <> "\"\r\n"
              <> "echo %*>> \"" <> BS8.pack arguments <> "\"\r\n"
              <> "powershell.exe -NoProfile -NonInteractive -ExecutionPolicy Bypass -File \"" <> BS8.pack helper <> "\"\r\n"
          protocol =
            "$output = [Console]::OpenStandardOutput()\n"
              <> "$ascii = [System.Text.Encoding]::ASCII\n"
              <> "while (($line = [Console]::In.ReadLine()) -ne $null) {\n"
              <> "  $oid = $line.Split(' ')[0]\n"
              <> "  $header = $ascii.GetBytes($oid + ' blob 1' + [char]10)\n"
              <> "  $output.Write($header, 0, $header.Length)\n"
              <> "  $output.WriteByte([byte][char]'x')\n"
              <> "  $output.WriteByte(10)\n"
              <> "  $output.Flush()\n"
              <> "}\n"
      BS.writeFile fakeGit launcher
      BS.writeFile helper protocol
      foldBlobBatch fakeRepository (reverse oids <> take 4 oids) [] (\seen blob -> pure (gitBlobOid blob : seen)) >>= \case
        Left problem -> assertFailure (show problem)
        Right seen -> Map.keys (Map.fromList [(oid, ()) | oid <- seen]) @?= oids
      foldBlobBatchInOrder fakeRepository ordered [] (\seen blob -> pure (seen <> [gitBlobOid blob]))
        >>= (@?= Right ordered)
      observedLaunches <- BS.readFile launches
      length (BS8.lines observedLaunches) @?= 2
      observedArguments <- BS.readFile arguments
      assertBool "production folds must not launch finite --buffer children" (not ("--buffer" `BS.isInfixOf` observedArguments))

persistentProtocolContract :: IO ()
persistentProtocolContract = do
  persistentOrderedContract
  persistentLargeFirstContract
  persistentFailureContract

persistentSessionFailureLatch :: IO ()
persistentSessionFailureLatch =
  withMVar nativeHelperTimingFixtureLock (const persistentSessionFailureLatchIsolated)

persistentSessionFailureLatchIsolated :: IO ()
persistentSessionFailureLatchIsolated =
  withRepository $ \repository discovered ->
    withSystemTempDirectory "adrai latched header eof" $ \temporary -> do
      executable <- getExecutablePath
      blobOid <- requireOid <$> hashObject repository "latched fresh blob"
      let helper = temporary </> "adrai-native-git-fixture-batch-p6133.exe"
          requests = temporary </> "fixture-requests"
          pidFile = temporary </> "fixture-helper.pid"
          fixtureRepository = discovered {repositoryClient = GitClient helper}
          expected = GitInvalidOutput "cat-file batch" (GitMalformedObjectHeader "Git protocol header ended before LF")
      copyNativeHelper executable helper
      writeFile (temporary </> "fixture-malformed-after") "1"
      writeFile (temporary </> "fixture-mode") "eof-response"
      BS.writeFile (temporary </> "fixture-blob-" <> Text.unpack (gitOidText blobOid)) "latched fresh blob"
      outcome <-
        withBlobBatchSession fixtureRepository $ \session -> do
          firstFailure <- readBlobBatchFromSession session [blobOid]
          laterAttempt <- readBlobBatchFromSession session [blobOid]
          pure $ case (firstFailure, laterAttempt) of
            (Left firstProblem, Left laterProblem) | firstProblem == expected && laterProblem == expected -> Left firstProblem
            pair -> Right pair
      outcome @?= Left expected
      BS.readFile requests >>= (\observed -> map (BS8.filter (/= '\r')) (BS8.lines observed) @?= [TextEncoding.encodeUtf8 (gitOidText blobOid)])
      pid <- waitForNativeHelperPid pidFile 50 >>= maybe (assertFailure "header-EOF helper PID was not recorded" >> fail "unreachable") pure
      waitForWindowsProcessAbsence pid 50 >>= either (assertFailure . ("header-EOF helper remained: " <>)) pure
      withBlobBatchSession discovered (\freshSession -> readBlobBatchFromSession freshSession [blobOid])
        >>= (@?= Right (Map.singleton blobOid (GitBlob blobOid "latched fresh blob")))

persistentSessionTrailingClose :: IO ()
persistentSessionTrailingClose =
  withRepository $ \repository discovered ->
    withSystemTempDirectory "adrai persistent trailing bytes" $ \temporary -> do
      let fakeGit = temporary </> "trailing-git.cmd"
          fakeRepository = discovered {repositoryClient = GitClient fakeGit}
      BS.writeFile fakeGit "@echo off\r\nset /p oid=\r\npowershell.exe -NoProfile -NonInteractive -Command \"[Console]::Out.Write($env:oid + ' blob 1' + [char]10 + 'x' + [char]10 + 'trailing'); [Console]::Out.Flush()\"\r\n"
      blobOid <- requireOid <$> hashObject repository "trailing retry"
      captured <- newEmptyMVar :: IO (MVar (IO (Either GitError (Map.Map GitOid GitBlob))))
      timeout (5 * 1000000) (withBlobBatchSession fakeRepository (\session -> do
        putMVar captured (readBlobBatchFromSession session [blobOid])
        readBlobBatchFromSession session [blobOid])) >>= \case
        Nothing -> assertFailure "trailing persistent child did not complete promptly"
        Just (Left (GitInvalidOutput "cat-file batch" (GitUnexpectedTrailingBytes _))) -> pure ()
        Just result -> assertFailure ("expected trailing persistent protocol failure, got " <> show result)
      escaped <- takeMVar captured
      escaped >>= \case
        Left (GitInvalidOutput "cat-file batch" (GitUnexpectedTrailingBytes _)) -> pure ()
        other -> assertFailure ("captured trailing session did not retain its first error: " <> show other)
      withBlobBatchSession discovered (\freshSession -> readBlobBatchFromSession freshSession [blobOid])
        >>= (@?= Right (Map.singleton blobOid (GitBlob blobOid "trailing retry")))

persistentSessionWithheldEofCancellation :: IO ()
persistentSessionPostCallbackCancellation :: IO ()
persistentSessionPostCallbackCancellation =
  withMVar nativeHelperTimingFixtureLock (const persistentSessionPostCallbackCancellationIsolated)

persistentSessionPostCallbackCancellationIsolated :: IO ()
persistentSessionPostCallbackCancellationIsolated =
  withRepository $ \repository discovered ->
    withSystemTempDirectory "adrai fixture post callback cancellation" $ \temporary -> do
      executable <- getExecutablePath
      blobOid <- requireOid <$> hashObject repository "post callback retry"
      captured <- newEmptyMVar :: IO (MVar (IO (Either GitError (Map.Map GitOid GitBlob))))
      entered <- newEmptyMVar
      release <- newEmptyMVar
      let helper = temporary </> "adrai-native-git-fixture-batch-p6133.exe"
          pidFile = temporary </> "fixture-helper.pid"
          requests = temporary </> "fixture-requests"
          fixtureRepository = discovered {repositoryClient = GitClient helper}
          afterCallback = putMVar entered () >> takeMVar release
      copyNativeHelper executable helper
      writeFile (temporary </> "fixture-malformed-after") "1"
      writeFile (temporary </> "fixture-mode") "withhold-eof"
      BS.writeFile (temporary </> "fixture-blob-" <> Text.unpack (gitOidText blobOid)) "post callback retry"
      owner <- Async.async $
        withBlobBatchSessionWithFinalizationObserverForTest fixtureRepository (pure ()) afterCallback $ \session -> do
          putMVar captured (readBlobBatchFromSession session [blobOid])
          pure (Right ())
      timeout (5 * 1000000) (takeMVar entered) >>= (@?= Just ())
      pid <- waitForNativeHelperPid pidFile 50 >>= \case
        Just value -> pure value
        Nothing -> assertFailure "post-callback helper PID was not recorded" >> fail "unreachable"
      timeout (5 * 1000000) (throwTo (Async.asyncThreadId owner) Async.AsyncCancelled >> Async.waitCatch owner) >>= \case
        Just (Left problem) -> fromException problem @?= Just Async.AsyncCancelled
        other -> assertFailure ("post-callback cancellation did not arrive: " <> show other)
      escaped <- takeMVar captured
      escaped >>= (@?= Left GitBlobBatchSessionClosed)
      exists <- doesFileExist requests
      if exists then BS.readFile requests >>= (\contents -> BS8.lines contents @?= []) else pure ()
      waitForWindowsProcessAbsence pid 50 >>= \case
        Right () -> pure ()
        Left problem -> assertFailure ("post-callback helper remained: " <> problem)
      withBlobBatchSession discovered (\fresh -> readBlobBatchFromSession fresh [blobOid])
        >>= (@?= Right (Map.singleton blobOid (GitBlob blobOid "post callback retry")))

persistentSessionWithheldEofCancellation =
  withMVar nativeHelperTimingFixtureLock (const persistentSessionWithheldEofCancellationIsolated)

persistentSessionWithheldEofCancellationIsolated :: IO ()
persistentSessionWithheldEofCancellationIsolated =
  withRepository $ \repository discovered ->
    withSystemTempDirectory "adrai fixture withheld eof" $ \temporary -> do
      executable <- getExecutablePath
      blobOid <- requireOid <$> hashObject repository "fixture withheld eof retry"
      captured <- newEmptyMVar :: IO (MVar (IO (Either GitError (Map.Map GitOid GitBlob))))
      let helper = temporary </> "adrai-native-git-fixture-batch-p6133.exe"
          phase = temporary </> "fixture-phase"
          pidFile = temporary </> "fixture-helper.pid"
          requests = temporary </> "fixture-requests"
          fixtureRepository = discovered {repositoryClient = GitClient helper}
      copyNativeHelper executable helper
      writeFile (temporary </> "fixture-malformed-after") "999"
      writeFile (temporary </> "fixture-mode") "withhold-eof"
      BS.writeFile (temporary </> "fixture-blob-" <> Text.unpack (gitOidText blobOid)) "fixture withheld eof retry"
      owner <- Async.async $ withBlobBatchSession fixtureRepository $ \session -> do
        putMVar captured (readBlobBatchFromSession session [blobOid])
        readBlobBatchFromSession session [blobOid]
      reached <- waitForTreePhase phase 50
      assertBool "fixture helper did not reach withheld-EOF phase" reached
      pid <- waitForNativeHelperPid pidFile 50 >>= \case
        Nothing -> assertFailure "fixture helper PID was not recorded" >> fail "unreachable"
        Just value -> pure value
      timeout (5 * 1000000) (throwTo (Async.asyncThreadId owner) Async.AsyncCancelled >> Async.waitCatch owner) >>= \case
        Nothing -> assertFailure "withheld-EOF owner cancellation exceeded five seconds"
        Just (Left exception) -> fromException exception @?= Just Async.AsyncCancelled
        Just result -> assertFailure ("withheld-EOF owner unexpectedly returned " <> show result)
      waitForWindowsProcessAbsence pid 50 >>= \case
        Left problem -> assertFailure ("withheld-EOF helper remained: " <> problem)
        Right () -> pure ()
      escaped <- takeMVar captured
      escaped >>= (@?= Left GitBlobBatchSessionClosed)
      (map (BS8.filter (/= '\r')) . BS8.lines <$> BS.readFile requests) >>= (@?= [TextEncoding.encodeUtf8 (gitOidText blobOid)])
      withBlobBatchSession discovered (\fresh -> readBlobBatchFromSession fresh [blobOid])
        >>= (@?= Right (Map.singleton blobOid (GitBlob blobOid "fixture withheld eof retry")))

persistentOwnerFinalization :: Bool -> IO ()
persistentOwnerFinalization shouldThrow =
  withMVar nativeHelperTimingFixtureLock (const (persistentOwnerFinalizationIsolated shouldThrow))

persistentCallbackCancellationPreservesSessionClosed :: IO ()
persistentCallbackCancellationPreservesSessionClosed =
  withMVar nativeHelperTimingFixtureLock (const (persistentBlockedCallbackCleanupIsolated Nothing))

persistentCallbackCancellationReportsCleanupFailure :: IO ()
persistentCallbackCancellationReportsCleanupFailure =
  withMVar nativeHelperTimingFixtureLock (const (persistentBlockedCallbackCleanupIsolated (Just "blocked-fold-cleanup-sentinel")))

persistentBlockedCallbackCleanupIsolated :: Maybe String -> IO ()
persistentBlockedCallbackCleanupIsolated cleanupFailure =
  withRepository $ \repository discovered ->
    withSystemTempDirectory "adrai blocked fold callback cancellation" $ \temporary -> do
      executable <- getExecutablePath
      blobOid <- requireOid <$> hashObject repository "blocked fold callback retry"
      entered <- newEmptyMVar
      never <- newEmptyMVar
      workers <- newEmptyMVar
      let helper = temporary </> "adrai-native-git-fixture-batch-p6133.exe"
          pidFile = temporary </> "fixture-helper.pid"
          requests = temporary </> "fixture-requests"
          fixtureRepository = discovered {repositoryClient = GitClient helper}
          blockCallback () _ =
            (putMVar entered () >> takeMVar never)
              `finally` maybe (pure ()) (ioError . userError) cleanupFailure
      copyNativeHelper executable helper
      writeFile (temporary </> "fixture-malformed-after") "999"
      BS.writeFile (temporary </> "fixture-blob-" <> Text.unpack (gitOidText blobOid)) "blocked fold callback retry"
      outcome <- timeout (5 * 1000000) $ withBlobBatchSession fixtureRepository $ \session -> do
        worker <- Async.async (foldBlobBatchFromSession session [blobOid] () blockCallback)
        putMVar workers worker
        timeout (5 * 1000000) (takeMVar entered) >>= (@?= Just ())
        pure (Right ())
      case (cleanupFailure, outcome) of
        (Nothing, Just (Left GitBlobBatchSessionClosed)) -> pure ()
        (Just sentinel, Just (Left (GitCommandFailed operation (-1) stdout diagnostic))) -> do
          operation @?= "cat-file batch"
          stdout @?= ""
          assertBool "registered reader cleanup failure lost its sentinel" (Text.pack sentinel `Text.isInfixOf` diagnostic)
        _ -> assertFailure ("unexpected blocked fold cleanup outcome: " <> show outcome)
      worker <- takeMVar workers
      timeout (5 * 1000000) (Async.waitCatch worker) >>= \case
        Nothing -> assertFailure "blocked fold worker was not reaped"
        Just _ -> pure ()
      pid <- waitForNativeHelperPid pidFile 50 >>= \case
        Nothing -> assertFailure "blocked fold helper PID was not recorded" >> fail "unreachable"
        Just value -> pure value
      waitForWindowsProcessAbsence pid 50 >>= \case
        Left problem -> assertFailure ("blocked fold helper remained: " <> problem)
        Right () -> pure ()
      (map (BS8.filter (/= '\r')) . BS8.lines <$> BS.readFile requests) >>= (@?= [TextEncoding.encodeUtf8 (gitOidText blobOid)])
      withBlobBatchSession discovered (\fresh -> readBlobBatchFromSession fresh [blobOid])
        >>= (@?= Right (Map.singleton blobOid (GitBlob blobOid "blocked fold callback retry")))

persistentOwnerFinalizationIsolated :: Bool -> IO ()
persistentOwnerFinalizationIsolated shouldThrow =
  withRepository $ \repository discovered ->
    withSystemTempDirectory "adrai fixture owner finalization" $ \temporary -> do
      executable <- getExecutablePath
      blobOid <- requireOid <$> hashObject repository "fixture owner finalization retry"
      workers <- newEmptyMVar
      let helper = temporary </> "adrai-native-git-fixture-batch-p6133.exe"
          phase = temporary </> "fixture-phase"
          pidFile = temporary </> "fixture-helper.pid"
          requests = temporary </> "fixture-requests"
          fixtureRepository = discovered {repositoryClient = GitClient helper}
      copyNativeHelper executable helper
      writeFile (temporary </> "fixture-malformed-after") "1"
      writeFile (temporary </> "fixture-mode") "withhold-response"
      BS.writeFile (temporary </> "fixture-blob-" <> Text.unpack (gitOidText blobOid)) "fixture owner finalization retry"
      outcome <- timeout (5 * 1000000) (try (withBlobBatchSession fixtureRepository $ \session -> do
        worker <- Async.async (readBlobBatchFromSession session [blobOid])
        putMVar workers worker
        reached <- waitForTreePhase phase 50
        if not reached then assertFailure "fixture helper did not reach withheld-response phase" else pure ()
        if shouldThrow then throwIO (userError "owner-finalization") else pure (Right ())) :: IO (Either SomeException (Either GitError ())))
      pid <- waitForNativeHelperPid pidFile 50 >>= \case
        Nothing -> assertFailure "fixture helper PID was not recorded" >> fail "unreachable"
        Just value -> pure value
      case outcome of
        Nothing -> assertFailure "owner finalization exceeded five seconds"
        Just (Right (Left GitBlobBatchSessionClosed)) | not shouldThrow -> pure ()
        Just (Left exception) | shouldThrow -> assertBool "owner exception changed" ("owner-finalization" `Text.isInfixOf` Text.pack (show exception))
        Just other -> assertFailure ("unexpected owner finalization outcome: " <> show other)
      worker <- takeMVar workers
      timeout (5 * 1000000) (Async.wait worker) >>= (@?= Just (Left GitBlobBatchSessionClosed))
      waitForWindowsProcessAbsence pid 50 >>= \case
        Left problem -> assertFailure ("owner-finalization helper remained: " <> problem)
        Right () -> pure ()
      (map (BS8.filter (/= '\r')) . BS8.lines <$> BS.readFile requests) >>= (@?= [TextEncoding.encodeUtf8 (gitOidText blobOid)])
      withBlobBatchSession discovered (\fresh -> readBlobBatchFromSession fresh [blobOid])
        >>= (@?= Right (Map.singleton blobOid (GitBlob blobOid "fixture owner finalization retry")))

persistentLaterWindowFailure :: IO ()
persistentLaterWindowFailure =
  withMVar nativeHelperTimingFixtureLock (const persistentLaterWindowFailureIsolated)

persistentLaterWindowFailureIsolated :: IO ()
persistentLaterWindowFailureIsolated =
  withSharedRepository $ \repository discovered ->
    withSystemTempDirectory "adrai fixture late window" $ \temporary -> do
      executable <- getExecutablePath
      (oids, blobs) <- persistentWindowFixtureWithCount 513 repository
      boundary <- newEmptyMVar
      let plan = concat (canonicalObjectChunks oids)
          expectedOid = plan !! 256
          expectedError = GitObjectTypeMismatch expectedOid GitBlobObject GitTreeObject
          helper = temporary </> "adrai-native-git-fixture-batch-p6133.exe"
          phase = temporary </> "fixture-phase"
          pidFile = temporary </> "fixture-helper.pid"
          requests = temporary </> "fixture-requests"
          boundaries = temporary </> "fixture-window-boundaries"
          fixtureRepository = discovered {repositoryClient = GitClient helper}
          observer = void (tryPutMVar boundary ())
      copyNativeHelper executable helper
      writeFile (temporary </> "fixture-malformed-after") "257"
      writeFile (temporary </> "fixture-mode") "wrong-type"
      forM_ (Map.elems blobs) $ \blob -> BS.writeFile (temporary </> "fixture-blob-" <> Text.unpack (gitOidText (gitBlobOid blob))) (gitBlobBytes blob)
      worker <- Async.async (withBlobBatchSessionWithWindowObserver fixtureRepository observer (\session -> do
        first <- readBlobBatchFromSession session oids
        second <- readBlobBatchFromSession session [requireHead "later-window fixture" oids]
        case first of
          Left problem -> second @?= Left problem
          Right _ -> assertFailure "later-window fixture unexpectedly succeeded"
        pure first))
      timeout (5 * 1000000) (takeMVar boundary) >>= (@?= Just ())
      Async.wait worker >>= (@?= Left expectedError)
      lines <$> readFile phase >>= (@?= ["wrong-type"])
      lines <$> readFile boundaries >>= (@?= ["256:False"])
      pid <- waitForNativeHelperPid pidFile 50 >>= \case
        Nothing -> assertFailure "late-window helper PID was not recorded" >> fail "unreachable"
        Just value -> pure value
      waitForWindowsProcessAbsence pid 50 >>= \case
        Left problem -> assertFailure ("late-window helper remained: " <> problem)
        Right () -> pure ()
      (map (BS8.filter (/= '\r')) . BS8.lines <$> BS.readFile requests) >>= (@?= map (TextEncoding.encodeUtf8 . gitOidText) (take 257 plan))
      withBlobBatchSession discovered (\fresh -> readBlobBatchFromSession fresh [requireHead "later-window fixture" oids]) >>= \case
        Left problem -> assertFailure ("fresh late-window retry failed: " <> show problem)
        Right _ -> pure ()

-- | The operation gate is deliberately held for a complete fold, rather than
-- one request window.  This blocks exactly after the first fully drained
-- 256-request window, before the owner can emit its 257th request.
persistentSessionSerializesWholePlan :: IO ()
persistentSessionSerializesWholePlan =
  withSharedRepository $ \repository discovered -> do
      (oids, _) <- persistentWindowFixture repository
      entered <- newEmptyMVar
      release <- newEmptyMVar
      let afterFirstWindow = do
            firstBoundary <- tryPutMVar entered ()
            if firstBoundary then takeMVar release else pure ()
      result <-
        withBlobBatchSessionWithWindowObserver discovered afterFirstWindow $ \session -> do
          first <-
            Async.async $
              foldBlobBatchFromSession session oids ([] :: [GitOid]) (\seen blob -> pure (seen <> [gitBlobOid blob]))
          takeMVar entered
          second <- Async.async (readBlobBatchFromSession session [requireHead "serialized-plan fixture" oids])
          -- A per-window lock would allow the second plan to run between the
          -- fully drained 256th response and the 257th request.  The shared
          -- operation gate keeps it pending until the complete plan releases.
          threadDelay 200000
          Async.poll second >>= \case
            Nothing -> pure ()
            Just _ -> assertFailure "second fold completed before the first full bounded plan released"
          putMVar release ()
          firstResult <- Async.wait first
          secondResult <- Async.wait second
          pure $ do
            firstValues <- firstResult
            secondValues <- secondResult
            Right (firstValues, secondValues)
      case result of
        Left problem -> assertFailure (show problem)
        Right (firstValues, secondValues) -> do
          firstValues @?= concat (canonicalObjectChunks oids)
          Map.keys secondValues @?= [requireHead "serialized-plan fixture" oids]

-- | A captured closure can outlive the lexical callback, but cannot use its
-- already-closed child.  The rank-2 type prevents returning the session itself;
-- this exercises the remaining runtime escape hatch directly.
persistentSessionClosedCapture :: IO ()
persistentSessionClosedCapture =
  withSharedRepository $ \_ discovered -> do
    captured <- newEmptyMVar :: IO (MVar (IO (Either GitError (Map.Map GitOid GitBlob))))
    withBlobBatchSession discovered
      (\session -> do
          putMVar captured (readBlobBatchFromSession session [])
          pure (Right ())
      )
      >>= (@?= Right ())
    capturedUse <- takeMVar captured
    capturedUse >>= (@?= Left GitBlobBatchSessionClosed)

-- | Top-level empty operations must avoid even constructing a scoped session.
emptyPublicBlobFoldsLaunchNothing :: IO ()
emptyPublicBlobFoldsLaunchNothing =
  withSharedRepository $ \_ discovered ->
    withSystemTempDirectory "adrai empty public blobs" $ \temporary -> do
      let fakeGit = temporary </> "should-not-launch.cmd"
          launches = temporary </> "launches.txt"
          fakeRepository = discovered {repositoryClient = GitClient fakeGit}
      BS.writeFile fakeGit ("@echo launched>> \"" <> BS8.pack launches <> "\"\r\nexit /b 99\r\n")
      foldBlobBatch fakeRepository [] ("seed" :: Text) (\seen _ -> pure seen) >>= (@?= Right "seed")
      foldBlobBatchInOrder fakeRepository [] ("seed" :: Text) (\seen _ -> pure seen) >>= (@?= Right "seed")
      readBlobBatch fakeRepository [] >>= (@?= Right Map.empty)
      readBlobBatchOneSession fakeRepository [] >>= (@?= Right Map.empty)
      doesFileExist launches >>= (@?= False)

persistentLargeFirstBlobContract :: FilePath -> Repository -> IO ()
persistentLargeFirstBlobContract repository discovered = do
  let fixtureDirectory = repository </> "persistent-ordering"
      largePaths = ["persistent-ordering/large-" <> show index | index <- [0 :: Int .. 7]]
      smallPaths = ["persistent-ordering/small-" <> show index | index <- [0 :: Int .. 511]]
  createDirectoryIfMissing True fixtureDirectory
  sequence_
    [ BS.writeFile (repository </> path) (BS.cons (fromIntegral index) (BS.replicate (1024 * 1024 - 1) 0))
      | (index, path) <- zip [0 :: Int ..] largePaths
    ]
  sequence_
    [ BS.writeFile (repository </> path) (TextEncoding.encodeUtf8 (Text.pack ("persistent-large-small-" <> show index)))
      | (index, path) <- zip [0 :: Int ..] smallPaths
    ]
  objectLines <- Text.lines . outputText <$> gitSuccess repository ["hash-object", "-w", "--stdin-paths"] (BS8.unlines (map BS8.pack (largePaths <> smallPaths)))
  let objectOids = map requireOid objectLines
      (largeOids, smallOids) = splitAt (length largePaths) objectOids
      eligibleLargeOids = [largeOid | largeOid <- largeOids, length (filter (> largeOid) smallOids) >= 256]
  largeOid <- case eligibleLargeOids of
    candidate : _ -> pure candidate
    [] -> assertFailure "batched ordering fixture did not yield a large blob before 256 small blobs" >> fail "unreachable"
  let requested = largeOid : take 256 (filter (> largeOid) smallOids)
      canonical = concat (canonicalObjectChunks requested)
  case canonical of
    firstOid : _ -> firstOid @?= largeOid
    [] -> assertFailure "large-first persistent request unexpectedly had no OIDs"
  completed <- timeout (30 * 1000000) (withBlobBatchSession discovered (\session -> readBlobBatchFromSession session requested))
  case completed of
    Nothing -> assertFailure "persistent cat-file window timed out while draining the first large blob"
    Just (Left problem) -> assertFailure (show problem)
    Just (Right blobs) -> do
      Map.size blobs @?= 257
      fmap (BS.length . gitBlobBytes) (Map.lookup largeOid blobs) @?= Just (1024 * 1024)

malformedPersistentOutputReapsChild :: Repository -> GitOid -> IO ()
malformedPersistentOutputReapsChild discovered blobOid =
  withSystemTempDirectory "adrai malformed git" $ \temporary -> do
    let fakeGit = temporary </> "malformed-git.cmd"
        malformedRepository = discovered {repositoryClient = GitClient fakeGit}
        -- Deliberately fill one legal persistent request window.  The fake
        -- child emits a malformed header without consuming stdin and loops,
        -- leaving the writer potentially blocked while the reader fails.
        blockedWindow =
          [ requireOid (Text.justifyRight 40 'a' (Text.pack (show index)))
          | index <- [0 :: Int .. 255]
          ]
    -- Keep the batch process itself alive after emitting a malformed frame.
    -- A child process here would inherit stderr and obscure whether
    -- 'withGitPipes' reaped the actual protocol child.
    BS.writeFile fakeGit "@echo off\r\necho malformed\r\n:loop\r\ngoto loop\r\n"
    completed <- timeout (5 * 1000000) (withBlobBatchSession malformedRepository (\session -> readBlobBatchFromSession session blockedWindow))
    case completed of
      Nothing -> assertFailure "malformed persistent child was not reaped promptly"
      Just (Left (GitInvalidOutput "cat-file batch" _)) -> pure ()
      Just result -> assertFailure ("expected malformed persistent protocol failure, got " <> show result)
    withBlobBatchSession discovered (\freshSession -> readBlobBatchFromSession freshSession [blobOid])
      >>= (@?= Right (Map.singleton blobOid (GitBlob blobOid "malformed callback")))

assertPersistentFailureAndFreshSession :: Repository -> GitOid -> GitOid -> GitError -> IO ()
assertPersistentFailureAndFreshSession repository freshOid failedOid expectedFailure = do
  withBlobBatchSession repository (\session -> readBlobBatchFromSession session [failedOid]) >>= (@?= Left expectedFailure)
  withBlobBatchSession repository (\session -> readBlobBatchFromSession session [freshOid])
    >>= (@?= Right (Map.singleton freshOid (GitBlob freshOid (TextEncoding.encodeUtf8 "persistent-protocol-0"))))

directTreeCancellationReapsNativeChild :: IO ()
directTreeCancellationReapsNativeChild =
  withMVar nativeHelperTimingFixtureLock (const directTreeCancellationReapsNativeChildIsolated)

directTreeCancellationReapsNativeChildIsolated :: IO ()
directTreeCancellationReapsNativeChildIsolated =
  withRepository $ \repository discovered ->
    withSystemTempDirectory "adrai-native-tree-cancellation" $ \temporary -> do
      executable <- getExecutablePath
      let helper = temporary </> nativeGitHelperProgram
          pidFile = temporary </> nativeGitHelperPidFile
          descendantPidFile = temporary </> nativeGitHelperDescendantPidFile
          descendantPhaseFile = temporary </> nativeGitHelperDescendantPhaseFile
          descendantExitFile = temporary </> nativeGitHelperDescendantExitFile
          phaseFile = temporary </> nativeGitHelperTreePhaseFile
          loopingRepository = discovered {repositoryClient = GitClient helper}
      assertBool "native helper path must not require cmd quoting" (not (any (`elem` [' ', '\t']) helper))
      copyNativeHelper executable helper
      directTreeCancellationReapsChild repository discovered loopingRepository True (waitForTreePhase phaseFile 50) (waitForNativeHelperPid pidFile 50) (waitForTreePhase descendantPhaseFile 50) descendantExitFile (waitForNativeHelperPid descendantPidFile 50)

persistentNativeBatchCancellation :: IO ()
persistentNativeBatchCancellation =
  withMVar nativeHelperTimingFixtureLock (const persistentNativeBatchCancellationIsolated)

persistentNativeBatchCancellationIsolated :: IO ()
persistentNativeBatchCancellationIsolated =
  withRepository $ \repository discovered -> do
    blobOid <- requireOid <$> hashObject repository "persistent batch cancellation"
    persistentNativeProtocolCancellation repository discovered blobOid
      (\fake -> void (readBlobBatchOneSession fake [blobOid]))
      (void (readBlobBatchOneSession discovered [blobOid]))

persistentNativeBatchCheckCancellation :: IO ()
persistentNativeBatchCheckCancellation =
  withMVar nativeHelperTimingFixtureLock (const persistentNativeBatchCheckCancellationIsolated)

persistentNativeBatchCheckCancellationIsolated :: IO ()
persistentNativeBatchCheckCancellationIsolated =
  withRepository $ \repository discovered -> do
    blobOid <- requireOid <$> hashObject repository "persistent batch-check cancellation"
    persistentNativeProtocolCancellation repository discovered blobOid
      (\fake -> void (batchObjectInfo fake [blobOid]))
      (void (batchObjectInfo discovered [blobOid]))

-- | The child supplies a complete, valid missing-object response and closes
-- stdout only after the session closes stdin.  Its phase marker is written
-- after that close, so cancelling at the marker exercises 'finishSuccess'
-- while it waits for the still-live captured process, rather than either
-- protocol reader/writer path.
persistentFinishSuccessCancellation :: IO ()
persistentFinishSuccessCancellation =
  withMVar nativeHelperTimingFixtureLock (const persistentFinishSuccessCancellationIsolated)

persistentFinishSuccessCancellationIsolated :: IO ()
persistentFinishSuccessCancellationIsolated =
  withRepository $ \repository discovered -> do
    blobOid <- requireOid <$> hashObject repository "persistent finish-success cancellation"
    withSystemTempDirectory "adrai-native-persistent-finish-cancellation" $ \temporary -> do
      executable <- getExecutablePath
      let helper = temporary </> nativeGitPersistentFinishHelperProgram
          pidFile = temporary </> nativeGitHelperPidFile
          phaseFile = temporary </> nativeGitHelperTreePhaseFile
          loopingRepository = discovered {repositoryClient = GitClient helper}
      copyNativeHelper executable helper
      worker <- Async.async (batchObjectInfo loopingRepository [blobOid])
      reached <- waitForTreePhase phaseFile 50
      assertBool "finish-success native child did not close stdout and reach its alive phase" reached
      childPid <- waitForNativeHelperPid pidFile 50
      pid <- maybe (assertFailure "could not observe finish-success native child PID" >> fail "unreachable") pure childPid
      deliveryStarted <- getMonotonicTimeNSec
      cancelled <- timeout (5 * 1000000) $ do
        throwTo (Async.asyncThreadId worker) Async.AsyncCancelled
        deliveryFinished <- getMonotonicTimeNSec
        result <- Async.waitCatch worker
        cleanupFinished <- getMonotonicTimeNSec
        pure (deliveryFinished, cleanupFinished, result)
      case cancelled of
        Nothing -> assertFailure "finish-success cancellation exceeded five seconds"
        Just (_, _, Right result) -> assertFailure ("cancelled finish-success operation unexpectedly returned: " <> show result)
        Just (_, _, Left exception) -> fromException exception @?= Just Async.AsyncCancelled
      case cancelled of
        Just (deliveryFinished, cleanupFinished, _) -> do
          let deliveryElapsed = fromIntegral (deliveryFinished - deliveryStarted) / 1000000000 :: Double
              cleanupElapsed = fromIntegral (cleanupFinished - deliveryFinished) / 1000000000 :: Double
          assertBool ("finish-success throwTo delivery exceeded 1.5s: " <> show deliveryElapsed) (deliveryElapsed < 1.5)
          assertBool ("finish-success cleanup exceeded 1.5s: " <> show cleanupElapsed) (cleanupElapsed < 1.5)
        Nothing -> pure ()
      waitForWindowsProcessAbsence pid 50 >>= \case
        Left problem -> assertFailure ("finish-success child remained after cleanup: " <> problem)
        Right () -> pure ()
      batchObjectInfo discovered [blobOid] >>= \case
        Right infos -> assertBool "fresh batch-check retry omitted its requested object" (Map.member blobOid infos)
        Left problem -> assertFailure ("fresh batch-check retry failed: " <> show problem)

-- | Both persistent protocols issue their bounded write before awaiting a
-- response.  The native child deliberately never consumes that request: this
-- proves cancellation never joins the opposite pipeline side before the pipe
-- owner starts finite cleanup, and that the exact spawned child is reaped.
persistentNativeProtocolCancellation :: FilePath -> Repository -> GitOid -> (Repository -> IO ()) -> IO () -> IO ()
persistentNativeProtocolCancellation _ discovered _ launch retry =
  withSystemTempDirectory "adrai-native-persistent-cancellation" $ \temporary -> do
    executable <- getExecutablePath
    let helper = temporary </> nativeGitPersistentHelperProgram
        pidFile = temporary </> nativeGitHelperPidFile
        phaseFile = temporary </> nativeGitHelperTreePhaseFile
        loopingRepository = discovered {repositoryClient = GitClient helper}
    copyNativeHelper executable helper
    worker <- Async.async (launch loopingRepository)
    reached <- waitForTreePhase phaseFile 50
    assertBool "silent persistent child did not reach the blocking phase" reached
    childPid <- waitForNativeHelperPid pidFile 50
    case childPid of
      Nothing -> assertFailure "could not observe silent persistent child PID" >> fail "unreachable"
      Just _ -> pure ()
    started <- getMonotonicTimeNSec
    cancelled <- timeout (5 * 1000000) $ do
      throwTo (Async.asyncThreadId worker) Async.AsyncCancelled
      Async.waitCatch worker
    finished <- getMonotonicTimeNSec
    case cancelled of
      Nothing -> assertFailure "persistent cancellation exceeded five seconds"
      Just (Right ()) -> assertFailure "cancelled persistent operation unexpectedly succeeded"
      Just (Left exception) -> fromException exception @?= Just Async.AsyncCancelled
    let elapsed = fromIntegral (finished - started) / 1000000000 :: Double
    assertBool ("persistent cancellation cleanup exceeded 1.5s: " <> show elapsed) (elapsed < 1.5)
    case childPid of
      Nothing -> pure ()
      Just pid -> waitForWindowsProcessAbsence pid 50 >>= \case
        Left problem -> assertFailure ("silent persistent child remained after cleanup: " <> problem)
        Right () -> pure ()
    retry

directTreeCancellationReapsChild :: FilePath -> Repository -> Repository -> Bool -> IO Bool -> IO (Maybe Int) -> IO Bool -> FilePath -> IO (Maybe Int) -> IO ()
directTreeCancellationReapsChild repository discovered loopingRepository requirePid awaitPhase awaitPid awaitDescendantPhase descendantExitFile awaitDescendantPid = do
  -- Deliberately retain this as a literal-pathspec fallback exercise.  The
  -- line-safe path protocol has its own persistent-session cancellation tests;
  -- this regression proves the native ls-tree descendant cleanup contract did
  -- not disappear when ordinary paths moved to cat-file.
  let path = requireRepoPath "tree cancellation.md"
  revision <- resolveHead discovered
  worker <- Async.async (lookupTreeObjectInfoAtRevisions loopingRepository (Map.singleton revision (Set.singleton path)) [])
  reachedTreePhase <- awaitPhase
  assertBool "fake Git did not reach the unbuffered tree phase" reachedTreePhase
  maybeChildPid <- awaitPid
  reachedDescendantPhase <- awaitDescendantPhase
  maybeDescendantPid <- awaitDescendantPid
  case (requirePid, maybeChildPid) of
    (True, Nothing) -> assertFailure "could not observe the silent tree lookup child PID" >> fail "unreachable"
    _ -> pure ()
  assertBool "cooperative descendant did not retain inherited pipes" reachedDescendantPhase
  case (requirePid, maybeDescendantPid) of
    (True, Nothing) -> assertFailure "could not observe the cooperative descendant PID" >> fail "unreachable"
    _ -> pure ()
  case (maybeChildPid, maybeDescendantPid) of
    (Just rootPid, Just descendantPid) -> assertBool "root and descendant are distinct processes" (rootPid /= descendantPid)
    _ -> pure ()
  deliveryStarted <- getMonotonicTimeNSec
  cancelled <- timeout (5 * 1000000) $ do
    throwTo (Async.asyncThreadId worker) Async.AsyncCancelled
    deliveryFinished <- getMonotonicTimeNSec
    Async.waitCatch worker
      >>= \result -> do
        cleanupFinished <- getMonotonicTimeNSec
        pure (deliveryFinished, cleanupFinished, result)
  case cancelled of
    Nothing -> assertFailure "Async.cancel did not reap the tree lookup child within five seconds"
    Just (_, _, Right result) -> assertFailure ("cancelled tree lookup unexpectedly returned: " <> show result)
    Just (_, _, Left exception) -> fromException exception @?= Just Async.AsyncCancelled
  case cancelled of
    Just (deliveryFinished, cleanupFinished, _) -> do
      let deliveryElapsedSeconds = fromIntegral (deliveryFinished - deliveryStarted) / 1000000000 :: Double
          cleanupElapsedSeconds = fromIntegral (cleanupFinished - deliveryFinished) / 1000000000 :: Double
      assertBool ("throwTo delivery exceeded 1.5s: " <> show deliveryElapsedSeconds) (deliveryElapsedSeconds < 1.5)
      assertBool ("cleanup exceeded 1.5s: " <> show cleanupElapsedSeconds) (cleanupElapsedSeconds < 1.5)
    Nothing -> pure ()
  didSelfExit <- doesFileExist descendantExitFile
  assertBool "cancellation returned after descendant self-exit marker" (not didSelfExit)
  case maybeDescendantPid of
    Nothing -> pure ()
    Just descendantPid ->
      windowsProcessPresent descendantPid >>= \case
        Right True -> pure ()
        Right False -> pure ()
        Left problem -> assertFailure ("could not query stubborn descendant PID after cancellation: " <> problem)
  forM_ [maybeChildPid, maybeDescendantPid] $ \case
    Nothing -> pure ()
    Just childPid -> do
      waitForWindowsProcessAbsence childPid 50 >>= \case
        Left problem -> assertFailure ("could not query exact cancelled child PID: " <> problem)
        Right () -> pure ()
  _ <- commitFile repository "tree cancellation.md" "retry bytes\n"
  freshRevision <- resolveHead discovered
  expected <- lookupTreeEntriesAt discovered freshRevision [path]
  blobOid <- case expected of
    Right entries -> case Map.lookup path entries of
      Just (Just entry) -> pure (gitTreeOid entry)
      other -> assertFailure ("retry fixture missing exact entry: " <> show other) >> fail "unreachable"
    Left problem -> assertFailure (show problem) >> fail "unreachable"
  lookupTreeObjectInfoAtRevisions discovered (Map.singleton freshRevision (Set.singleton path)) [blobOid]
    >>= (@?= Right (Map.singleton freshRevision (Map.singleton path (Just (GitObjectInfo blobOid GitBlobObject 12)))))

treePathProtocolFailure :: String -> BS.ByteString -> IO ()
treePathProtocolFailure label response =
  withSharedRepository $ \_ discovered ->
    withSystemTempDirectory ("adrai tree-path " <> label) $ \temporary -> do
      let fakeGit = temporary </> "tree-path-protocol-git.cmd"
          fakeRepository = discovered {repositoryClient = GitClient fakeGit}
          path = requireRepoPath "safe-token.md"
      BS.writeFile fakeGit (treePathProtocolScript response)
      revision <- resolveHead discovered
      timeout (5 * 1000000) (lookupTreeObjectInfoAtRevisions fakeRepository (Map.singleton revision (Set.singleton path)) []) >>= \case
        Nothing -> assertFailure ("tree-path " <> label <> " child did not fail promptly")
        Just (Left _) -> pure ()
        Just result -> assertFailure ("expected " <> label <> " tree protocol failure, got " <> show result)

treePathRealGitParity :: IO ()
treePathRealGitParity =
  withRepository $ \repository discovered -> do
    baseRevision <- resolveHead discovered
    mainBranch <- Text.unpack . Text.strip . outputText <$> gitSuccess repository ["branch", "--show-current"] BS.empty
    _ <- gitSuccess repository ["checkout", "-b", "tree-path-feature"] BS.empty
    _ <- commitFile repository "feature-only.md" "shared blob bytes"
    featureRevision <- resolveHead discovered
    _ <- gitSuccess repository ["checkout", mainBranch] BS.empty
    _ <- commitFile repository "main-only.md" "shared blob bytes"
    mainRevision <- resolveHead discovered
    _ <- gitSuccess repository ["merge", "--no-ff", "tree-path-feature", "-m", "tree-path merge"] BS.empty
    mergeRevision <- resolveHead discovered
    sharedBlob <- requireOid <$> hashObject repository "shared blob bytes"
    modeTree <-
      Text.strip . outputText
        <$> gitSuccess
          repository
          ["mktree", "-z"]
          ( BS.concat
              [ "100755 blob ", TextEncoding.encodeUtf8 (gitOidText sharedBlob), "\texecutable\NUL",
                "120000 blob ", TextEncoding.encodeUtf8 (gitOidText sharedBlob), "\tlink\NUL",
                "160000 commit ", TextEncoding.encodeUtf8 (gitOidText mergeRevision), "\tsubmodule\NUL"
              ]
          )
    modeRevision <- requireOid . Text.strip . outputText <$> gitSuccess repository ["commit-tree", Text.unpack modeTree] "mode parity\n"
    let basePaths = Set.fromList [requireRepoPath "feature-only.md", requireRepoPath "main-only.md"]
        featurePaths = Set.fromList [requireRepoPath "feature-only.md", requireRepoPath "main-only.md"]
        mainPaths = Set.fromList [requireRepoPath "feature-only.md", requireRepoPath "main-only.md"]
        mergePaths = Set.fromList [requireRepoPath "feature-only.md", requireRepoPath "main-only.md"]
        modePaths = Set.fromList [requireRepoPath "executable", requireRepoPath "link", requireRepoPath "submodule"]
        plan = Map.fromList [(baseRevision, basePaths), (featureRevision, featurePaths), (mainRevision, mainPaths), (mergeRevision, mergePaths), (modeRevision, modePaths)]
    assertTreePathParity discovered plan

assertTreePathParity :: Repository -> Map.Map GitOid (Set.Set RepoPath) -> IO ()
assertTreePathParity repository plan = do
  observed <- lookupTreeObjectInfoAtRevisions repository plan []
  case observed of
    Left problem -> assertFailure (show problem)
    Right actual ->
      forM_ (Map.toList plan) $ \(revision, paths) -> do
        oracle <- lookupTreeEntriesAt repository revision (Set.toAscList paths)
        case oracle of
          Left problem -> assertFailure (show problem)
          Right expected ->
            forM_ (Set.toAscList paths) $ \path ->
              case (Map.lookup path expected, Map.lookup revision actual >>= Map.lookup path) of
                (Just Nothing, Just Nothing) -> pure ()
                (Just (Just entry), Just (Just info)) -> do
                  objectInfoOid info @?= gitTreeOid entry
                  objectInfoType info @?= gitTreeObjectType entry
                pair -> assertFailure ("tree-path parity mismatch for " <> show (revision, path, pair))

treePathProtocolScript :: BS.ByteString -> BS.ByteString
treePathProtocolScript response =
  "@echo off\r\nset /p first=\r\necho %first% | findstr /c:\":\" >nul\r\nif not errorlevel 1 goto tree\r\necho %first% commit 1\r\nexit /b 0\r\n:tree\r\n"
    <> response
    <> "\r\n"

treePathBatchFailureShortCircuitsFallback :: IO ()
treePathBatchFailureShortCircuitsFallback =
  withSharedRepository $ \_repository discovered ->
    withSystemTempDirectory "adrai tree-path short-circuit" $ \temporary -> do
      let fakeGit = temporary </> "tree-path-short-circuit-git.cmd"
          fallbackMarker = temporary </> "fallback-launched"
          fakeRepository = discovered {repositoryClient = GitClient fakeGit}
          safePath = requireRepoPath "safe.md"
          fallbackPath = requireRepoPath "spaced path.md"
          script =
            "@echo off\r\necho %* | findstr /c:\"ls-tree\" >nul\r\nif not errorlevel 1 goto fallback\r\nset /p first=\r\necho %first% | findstr /c:\":\" >nul\r\nif not errorlevel 1 goto batch\r\necho %first% commit 1\r\nexit /b 0\r\n:batch\r\necho wrong-expression missing\r\nexit /b 0\r\n:fallback\r\necho launched> \"" <> BS8.pack fallbackMarker <> "\"\r\n:loop\r\ngoto loop\r\n"
      BS.writeFile fakeGit script
      revision <- resolveHead discovered
      result <- timeout (5 * 1000000) (lookupTreeObjectInfoAtRevisions fakeRepository (Map.singleton revision (Set.fromList [safePath, fallbackPath])) [])
      case result of
        Nothing -> assertFailure "correlated batch failure did not return within five seconds"
        Just (Left _) -> pure ()
        Just response -> assertFailure ("expected correlated batch failure, got " <> show response)
      launched <- doesFileExist fallbackMarker
      assertBool "ineligible ls-tree fallback launched after a correlated batch failure" (not launched)
      lookupTreeObjectInfoAtRevisions discovered (Map.singleton revision (Set.fromList [safePath, fallbackPath])) [] >>= \case
        Left problem -> assertFailure ("fresh real-Git retry failed: " <> show problem)
        Right _ -> pure ()

treePathReorderedFailure :: IO ()
treePathReorderedFailure =
  withSharedRepository $ \_ discovered ->
    withSystemTempDirectory "adrai tree-path reordered" $ \temporary -> do
      let fakeGit = temporary </> "tree-path-reordered-git.cmd"
          fakeRepository = discovered {repositoryClient = GitClient fakeGit}
          firstPath = requireRepoPath "first-safe.md"
          secondPath = requireRepoPath "second-safe.md"
          script =
            "@echo off\r\nset /p first=\r\necho %first% | findstr /c:\":\" >nul\r\nif not errorlevel 1 goto tree\r\necho %first% commit 1\r\nexit /b 0\r\n:tree\r\nset /p second=\r\nfor /f \"tokens=1\" %%a in (\"%second%\") do echo %%a missing\r\n"
      BS.writeFile fakeGit script
      revision <- resolveHead discovered
      timeout (5 * 1000000) (lookupTreeObjectInfoAtRevisions fakeRepository (Map.singleton revision (Set.fromList [firstPath, secondPath])) []) >>= \case
        Nothing -> assertFailure "reordered tree-path child did not fail promptly"
        Just (Left _) -> pure ()
        Just result -> assertFailure ("expected reordered tree protocol failure, got " <> show result)

treePathDuplicateExpressionFailure :: IO ()
treePathDuplicateExpressionFailure =
  withSharedRepository $ \_ discovered ->
    withSystemTempDirectory "adrai tree-path duplicate expression" $ \temporary -> do
      let fakeGit = temporary </> "tree-path-duplicate-expression-git.cmd"
          fakeRepository = discovered {repositoryClient = GitClient fakeGit}
          firstPath = requireRepoPath "first-duplicate.md"
          secondPath = requireRepoPath "second-duplicate.md"
          script =
            "@echo off\r\nset /p first=\r\necho %first% | findstr /c:\":\" >nul\r\nif not errorlevel 1 goto tree\r\necho %first% commit 1\r\nexit /b 0\r\n:tree\r\nset /p second=\r\nfor /f \"tokens=1\" %%a in (\"%first%\") do echo %%a missing\r\nfor /f \"tokens=1\" %%a in (\"%first%\") do echo %%a missing\r\n"
      BS.writeFile fakeGit script
      revision <- resolveHead discovered
      timeout (5 * 1000000) (lookupTreeObjectInfoAtRevisions fakeRepository (Map.singleton revision (Set.fromList [firstPath, secondPath])) []) >>= \case
        Nothing -> assertFailure "duplicate-expression tree-path child did not fail promptly"
        Just (Left _) -> pure ()
        Just result -> assertFailure ("expected duplicate-expression tree protocol failure, got " <> show result)

treePathCrossWindowReplayFailure :: IO ()
treePathCrossWindowReplayFailure =
  withMVar nativeHelperTimingFixtureLock (const treePathCrossWindowReplayFailureIsolated)

treePathCrossWindowReplayFailureIsolated :: IO ()
treePathCrossWindowReplayFailureIsolated =
  withSharedRepository $ \_ discovered ->
    withSystemTempDirectory "adrai tree-path cross-window replay" $ \temporary -> do
      executable <- getExecutablePath
      let fakeGit = temporary </> nativeGitFixtureBatchHelperProgram
          fakeRepository = discovered {repositoryClient = GitClient fakeGit}
          paths = Set.fromList [requireRepoPath ("cross-window/" <> Text.pack (show number) <> ".md") | number <- [0 :: Int .. 256]]
          pidFile = temporary </> "fixture-helper.pid"
          requestsFile = temporary </> "fixture-requests"
          phaseFile = temporary </> "fixture-phase"
      copyNativeHelper executable fakeGit
      writeFile (temporary </> "fixture-mode") "tree-cross-window-replay"
      revision <- resolveHead discovered
      let expectedRequests =
            [ TextEncoding.encodeUtf8
                ( gitOidText revision
                    <> ":"
                    <> repoPathText path
                    <> " adrai-tree-"
                    <> Text.justifyRight 8 '0' (Text.pack (show ordinal))
                )
            | (ordinal, path) <- zip [0 :: Int ..] (Set.toAscList paths)
            ]
          replayedExpression = BS8.takeWhile (/= ' ') (requireHead "cross-window requests" expectedRequests)
          expectedError = GitInvalidOutput "cat-file tree paths" (GitMalformedObjectHeader (replayedExpression <> " missing"))
      timeout (5 * 1000000) (lookupTreeObjectInfoAtRevisions fakeRepository (Map.singleton revision paths) []) >>= \case
        Nothing -> assertFailure "cross-window replay tree-path child did not fail promptly"
        Just (Left problem) -> problem @?= expectedError
        Just result -> assertFailure ("expected cross-window replay tree protocol failure, got " <> show result)
      observedRequests <- map (BS8.filter (/= '\r')) . BS8.lines <$> BS.readFile requestsFile
      length observedRequests @?= 257
      observedRequests @?= expectedRequests
      assertBool
        "cross-window replay fixture did not distinguish its first and later-window requests"
        (requireHead "observed cross-window requests" observedRequests /= requireLast "observed cross-window requests" observedRequests)
      lines <$> readFile phaseFile >>= (@?= ["replayed-first-window-expression"])
      pid <- waitForNativeHelperPid pidFile 50 >>= maybe (assertFailure "cross-window replay helper PID was not recorded" >> fail "unreachable") pure
      waitForWindowsProcessAbsence pid 50 >>= either (assertFailure . ("cross-window replay helper remained: " <>)) pure
      lookupTreeObjectInfoAtRevisions discovered (Map.singleton revision paths) [] >>= \case
        Left problem -> assertFailure ("fresh real-Git cross-window retry failed: " <> show problem)
        Right _ -> pure ()

treePathTrailingFailure :: IO ()
treePathTrailingFailure =
  withSharedRepository $ \_ discovered ->
    withSystemTempDirectory "adrai tree-path trailing" $ \temporary -> do
      let fakeGit = temporary </> "tree-path-trailing-git.cmd"
          fakeRepository = discovered {repositoryClient = GitClient fakeGit}
          path = requireRepoPath "safe-trailing.md"
          script =
            "@echo off\r\nset /p first=\r\necho %first% | findstr /c:\":\" >nul\r\nif not errorlevel 1 goto tree\r\necho %first% commit 1\r\nexit /b 0\r\n:tree\r\nfor /f \"tokens=1\" %%a in (\"%first%\") do echo %%a missing\r\necho trailing\r\n"
      BS.writeFile fakeGit script
      revision <- resolveHead discovered
      timeout (5 * 1000000) (lookupTreeObjectInfoAtRevisions fakeRepository (Map.singleton revision (Set.singleton path)) []) >>= \case
        Nothing -> assertFailure "trailing tree-path child did not fail promptly"
        Just (Left _) -> pure ()
        Just result -> assertFailure ("expected trailing tree protocol failure, got " <> show result)

nativeGitHelperProgram :: FilePath
nativeGitHelperProgram = "adrai-native-git-helper-p6115.exe"

nativeGitPersistentHelperProgram :: FilePath
nativeGitPersistentHelperProgram = "adrai-native-git-persistent-helper-p6115.exe"

nativeGitFixtureBatchHelperProgram :: FilePath
nativeGitFixtureBatchHelperProgram = "adrai-native-git-fixture-batch-p6133.exe"

nativeGitPersistentFinishHelperProgram :: FilePath
nativeGitPersistentFinishHelperProgram = "adrai-native-git-persistent-finish-helper-p6115.exe"

nativeGitHelperPidFile :: FilePath
nativeGitHelperPidFile = "adrai-native-git-helper-p6115.pid"

nativeGitHelperTreePhaseFile :: FilePath
nativeGitHelperTreePhaseFile = "adrai-native-git-helper-p6115.tree-phase"

nativeGitHelperDescendantPidFile :: FilePath
nativeGitHelperDescendantPidFile = "adrai-native-git-helper-p6115.descendant.pid"

nativeGitHelperDescendantPhaseFile :: FilePath
nativeGitHelperDescendantPhaseFile = "adrai-native-git-helper-p6115.descendant-phase"

nativeGitHelperDescendantExitFile :: FilePath
nativeGitHelperDescendantExitFile = "adrai-native-git-helper-p6115.descendant-exit"

waitForTreePhase :: FilePath -> Int -> IO Bool
waitForTreePhase phaseFile attempts
  | attempts <= 0 = doesFileExist phaseFile
  | otherwise = do
      exists <- doesFileExist phaseFile
      if exists
        then pure True
        else threadDelay 100000 >> waitForTreePhase phaseFile (attempts - 1)

waitForNativeHelperPid :: FilePath -> Int -> IO (Maybe Int)
waitForNativeHelperPid pidFile attempts
  | attempts <= 0 = pure Nothing
  | otherwise = do
      exists <- doesFileExist pidFile
      if not exists
        then threadDelay 100000 >> waitForNativeHelperPid pidFile (attempts - 1)
        else do
          value <- readMaybe <$> readFile pidFile
          case value of
            Just pid -> pure (Just pid)
            Nothing -> threadDelay 100000 >> waitForNativeHelperPid pidFile (attempts - 1)

waitForWindowsProcessAbsence :: Int -> Int -> IO (Either String ())
waitForWindowsProcessAbsence childPid attempts = do
  let boundedAttempts = max 0 attempts
      script =
        "$ErrorActionPreference = 'Stop'; "
          <> "$observedPid = " <> show childPid <> "; "
          <> "$remaining = " <> show boundedAttempts <> "; "
          <> "while ($true) { "
          <> "$present = $true; "
          <> "try { [void][Diagnostics.Process]::GetProcessById($observedPid) } "
          <> "catch [ArgumentException] { $present = $false }; "
          <> "if (-not $present) { [Console]::Out.Write('absent'); exit 0 }; "
          <> "if ($remaining -le 0) { [Console]::Out.Write('present'); exit 0 }; "
          <> "$remaining -= 1; Start-Sleep -Milliseconds 100 "
          <> "}"
  runWindowsProcessQuery script >>= \case
    Left problem -> pure (Left problem)
    Right True -> pure (Left "the exact child PID remained present after bounded retries")
    Right False -> pure (Right ())

windowsProcessPresent :: Int -> IO (Either String Bool)
windowsProcessPresent childPid = do
  let script =
        "$ErrorActionPreference = 'Stop'; try { [void][Diagnostics.Process]::GetProcessById(" <> show childPid <> "); [Console]::Out.Write('present') } "
          <> "catch [ArgumentException] { [Console]::Out.Write('absent') }"
  runWindowsProcessQuery script

runWindowsProcessQuery :: String -> IO (Either String Bool)
runWindowsProcessQuery script = do
  attempted <- try (readProcess (proc "powershell.exe" ["-NoProfile", "-NonInteractive", "-Command", script]))
  pure $
    case attempted of
      Left (problem :: IOError) -> Left ("PowerShell spawn/query failure: " <> show problem)
      Right (exitCode, stdoutBytes, stderrBytes) -> classifyWindowsProcessQuery exitCode (LBS.toStrict stdoutBytes) (LBS.toStrict stderrBytes)

classifyWindowsProcessQuery :: ExitCode -> BS.ByteString -> BS.ByteString -> Either String Bool
classifyWindowsProcessQuery exitCode stdoutBytes stderrBytes
  | exitCode /= ExitSuccess = Left ("PowerShell exit: " <> show exitCode)
  | not (BS.null stderrBytes) = Left ("PowerShell stderr: " <> BS8.unpack stderrBytes)
  | stdoutBytes == "present" = Right True
  | stdoutBytes == "absent" = Right False
  | otherwise = Left ("PowerShell returned an unknown PID state: " <> BS8.unpack stdoutBytes)

repeatedOid :: Char -> GitOid
repeatedOid character = requireOid (Text.replicate 40 (Text.singleton character))

resolveHead :: Repository -> IO GitOid
resolveHead repository =
  resolveRevision repository (requireRevision "HEAD") >>= \case
    Left problem -> assertFailure (show problem)
    Right oid -> pure oid

requireOid :: Text -> GitOid
requireOid value =
  case mkGitOid value of
    Left problem -> error (show problem)
    Right oid -> oid
