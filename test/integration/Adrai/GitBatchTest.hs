{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Adrai.GitBatchTest (tests) where

import Adrai.Git
import Adrai.GitTestSupport
import Adrai.Provenance (mkGitOid)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Directory (createDirectoryIfMissing, createFileLink, removeFile)
import System.FilePath ((</>))
import System.IO.Error (tryIOError)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase, testCaseSteps)

tests :: TestTree
tests =
  testGroup
    "Git batch and safe reads"
    [ testCase "revision tree and blob reads are exact for custom Unicode roots" $
        withRepository $ \repository discovered -> do
          commit <- resolveHead discovered
          let path = requireRepoPath "arkitektur/beslutninger/København.md"
              expected = TextEncoding.encodeUtf8 "øpaque Git bytes\n"
          _ <- commitFile repository "arkitektur/beslutninger/København.md" expected
          current <- resolveHead discovered
          assertBool "commit advanced" (commit /= current)
          listTreeEntriesAt discovered current [requireRepoPath "arkitektur"] >>= \case
            Left problem -> assertFailure (show problem)
            Right entries -> assertBool "custom path listed" (path `elem` map gitTreePath entries)
          readRegularBlobAt discovered current path >>= \case
            Left problem -> assertFailure (show problem)
            Right blob -> do
              gitBlobBytes blob @?= expected
              decodeGitBlobUtf8 blob @?= Right "øpaque Git bytes\n"
          lookupTreeEntryAt discovered current (requireRepoPath "missing.md") >>= (@?= Right Nothing)
          readRegularBlobAt discovered current (requireRepoPath "missing.md") >>= \case
            Left (GitPathMissing _) -> pure ()
            result -> assertFailure ("expected GitPathMissing, got " <> show result)
          readRegularBlobAt discovered current (requireRepoPath "arkitektur") >>= \case
            Left (GitPathNotRegular _ GitDirectory GitTreeObject) -> pure ()
            result -> assertFailure ("expected directory rejection, got " <> show result),
      testCase "unknown and non-commit revisions are distinguished and option-like input is rejected" $
        withRepository $ \repository discovered -> do
          resolveRevision discovered (requireRevision "not-a-revision") >>= \case
            Left (GitUnknownRevision _) -> pure ()
            result -> assertFailure (show result)
          blobText <- hashObject repository "blob"
          resolveRevision discovered (requireRevision blobText) >>= \case
            Left (GitRevisionNotCommit _) -> pure ()
            result -> assertFailure (show result)
          case mkRevisionSpec "--help" of
            Left (GitInvalidRevisionSpec _) -> pure ()
            result -> assertFailure (show result),
      testCase "257 blob OIDs are read through two deterministic bounded windows" $
        withRepository $ \repository discovered -> do
          oidTexts <- mapM (hashObject repository . TextEncoding.encodeUtf8 . Text.pack . ("blob-" <>) . show) [0 :: Int .. 256]
          let oids = map requireOid oidTexts
              permuted = reverse oids <> take 4 oids
          map length (canonicalObjectChunks oids) @?= [256, 1]
          batchObjectInfo discovered permuted >>= \case
            Left problem -> assertFailure (show problem)
            Right infos -> do
              Map.size infos @?= 257
              assertBool "all blobs" (all ((== Just GitBlobObject) . fmap objectInfoType) (Map.elems infos))
          readBlobBatch discovered permuted >>= \case
            Left problem -> assertFailure (show problem)
            Right blobs -> do
              Map.size blobs @?= 257
              Map.keys blobs @?= Map.keys (Map.fromList [(oid, ()) | oid <- oids])
          readBlobBatchOneSession discovered permuted >>= \case
            Left problem -> assertFailure (show problem)
            Right blobs -> Map.keys blobs @?= Map.keys (Map.fromList [(oid, ()) | oid <- oids]),
      testCase "persistent blob sessions keep consecutive windows on one buffered child" $
        withRepository $ \repository discovered -> do
          firstText <- hashObject repository "first persistent batch"
          secondText <- hashObject repository "second persistent batch"
          let firstOid = requireOid firstText
              secondOid = requireOid secondText
          withBlobBatchSession discovered (\session -> do
            firstResult <- readBlobBatchFromSession session [firstOid]
            secondResult <- readBlobBatchFromSession session [secondOid]
            pure $ do
              firstBlobs <- firstResult
              secondBlobs <- secondResult
              Right (firstBlobs, secondBlobs)
            ) >>= \case
              Left problem -> assertFailure (show problem)
              Right (firstBlobs, secondBlobs) -> do
                Map.lookup firstOid firstBlobs @?= Just (GitBlob firstOid "first persistent batch")
                Map.lookup secondOid secondBlobs @?= Just (GitBlob secondOid "second persistent batch"),
      testCase "persistent sessions use ordered bounded exchanges and reap protocol failures" persistentProtocolContract,
      testCase "one buffered exchange preserves in-order duplicate folds" $
        withRepository $ \repository discovered -> do
          firstOid <- requireOid <$> hashObject repository "first ordered blob"
          secondOid <- requireOid <$> hashObject repository "second ordered blob"
          foldBlobBatchInOrder discovered [firstOid, secondOid, firstOid] [] (\seen blob -> pure (seen <> [gitBlobOid blob]))
            >>= (@?= Right [firstOid, secondOid, firstOid]),
      testCase "ordered folds preserve duplicates across the 256-request boundary" $
        withRepository $ \repository discovered -> do
          firstOid <- requireOid <$> hashObject repository "boundary first"
          secondOid <- requireOid <$> hashObject repository "boundary second"
          let requested = replicate 255 firstOid <> [secondOid, firstOid, secondOid]
          foldBlobBatchInOrder discovered requested [] (\seen blob -> pure (seen <> [gitBlobOid blob]))
            >>= (@?= Right requested),
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
              removeFile (repository </> "dangling.txt"),
      testCase "committed tree reads ignore sparse worktree materialization" $
        withRepository $ \repository discovered -> do
          _ <- commitFile repository "visible/keep.txt" "visible"
          _ <- commitFile repository "hidden/custom/adr.md" "committed outside sparse patterns"
          _ <- gitSuccess repository ["sparse-checkout", "init", "--cone"] BS.empty
          _ <- gitSuccess repository ["sparse-checkout", "set", "visible"] BS.empty
          commit <- resolveHead discovered
          readRegularBlobAt discovered commit (requireRepoPath "hidden/custom/adr.md") >>= \case
            Left problem -> assertFailure (show problem)
            Right blob -> gitBlobBytes blob @?= "committed outside sparse patterns",
      testCase "shallow repositories expose incompleteness while retaining tree reads" $
        withSystemTempDirectory "adrai shallow" $ \temporary -> do
          let source = temporary </> "source"
              shallow = temporary </> "shallow"
          initTestRepository source
          _ <- commitFile source "first.txt" "first"
          _ <- commitFile source "second.txt" "second"
          let sourceUri = "file:///" <> map slash source
          _ <- gitSuccess temporary ["clone", "--depth", "1", sourceUri, shallow] BS.empty
          discoverRepository systemGit shallow >>= \case
            Left problem -> assertFailure (show problem)
            Right discovered -> do
              isShallowRepository discovered >>= (@?= Right True)
              commit <- resolveHead discovered
              readRegularBlobAt discovered commit (requireRepoPath "second.txt") >>= \case
                Left problem -> assertFailure (show problem)
                Right blob -> gitBlobBytes blob @?= "second"
    ]

withRepository :: (FilePath -> Repository -> IO value) -> IO value
withRepository action =
  withSystemTempDirectory "adrai git batch" $ \temporary -> do
    let repository = temporary </> "repository"
    initTestRepository repository
    _ <- commitFile repository "seed.txt" "seed"
    discoverRepository systemGit repository >>= \case
      Left problem -> assertFailure (show problem)
      Right discovered -> action repository discovered

-- | A real Git child validates the persistent request/flush lifecycle across
-- bounded windows, including failures that must reap the child.
persistentProtocolContract :: IO ()
persistentProtocolContract =
  withRepository $ \repository discovered -> do
    oidTexts <- mapM (hashObject repository . TextEncoding.encodeUtf8 . Text.pack . ("persistent-protocol-" <>) . show) [0 :: Int .. 256]
    let windowOids = map requireOid oidTexts
        permuted = reverse windowOids <> take 4 windowOids
        expectedBlobs =
          Map.fromList
            [ (oid, GitBlob oid (TextEncoding.encodeUtf8 (Text.pack ("persistent-protocol-" <> show index))))
              | (index, oid) <- zip [0 :: Int ..] windowOids
            ]
    withBlobBatchSession discovered
      (\session -> do
          empty <- readBlobBatchFromSession session []
          loaded <- readBlobBatchFromSession session permuted
          pure (empty >> loaded)
      )
      >>= (@?= Right expectedBlobs)
    persistentLargeFirstBlobContract repository discovered
    let missingOid = repeatedOid 'f'
    assertPersistentFailureAndFreshSession discovered (head windowOids) missingOid (GitObjectMissing missingOid)
    commit <- resolveHead discovered
    assertPersistentFailureAndFreshSession discovered (head windowOids) commit (GitObjectTypeMismatch commit GitBlobObject GitCommitObject)

persistentLargeFirstBlobContract :: FilePath -> Repository -> IO ()
persistentLargeFirstBlobContract repository discovered = do
  largeCandidates <-
    mapM
      (hashObject repository . \index -> BS.cons (fromIntegral index) (BS.replicate (1024 * 1024 - 1) 0))
      [0 :: Int .. 7]
  let largeOid = minimum (map requireOid largeCandidates)
  smallOids <- collectOidsAfter repository largeOid 256 0 []
  let requested = largeOid : smallOids
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

collectOidsAfter :: FilePath -> GitOid -> Int -> Int -> [GitOid] -> IO [GitOid]
collectOidsAfter repository lower remaining next accepted
  | remaining == 0 = pure (reverse accepted)
  | otherwise = do
      candidate <- requireOid <$> hashObject repository (TextEncoding.encodeUtf8 (Text.pack ("persistent-large-small-" <> show next)))
      if candidate > lower
        then collectOidsAfter repository lower (remaining - 1) (next + 1) (candidate : accepted)
        else collectOidsAfter repository lower remaining (next + 1) accepted

malformedPersistentOutputReapsChild :: Repository -> GitOid -> IO ()
malformedPersistentOutputReapsChild discovered blobOid =
  withSystemTempDirectory "adrai malformed git" $ \temporary -> do
    let fakeGit = temporary </> "malformed-git.cmd"
        malformedRepository = discovered {repositoryClient = GitClient fakeGit}
    -- Keep the batch process itself alive after emitting a malformed frame.
    -- A child process here would inherit stderr and obscure whether
    -- 'withGitPipes' reaped the actual protocol child.
    BS.writeFile fakeGit "@echo off\r\necho malformed\r\n:loop\r\ngoto loop\r\n"
    completed <- timeout (5 * 1000000) (withBlobBatchSession malformedRepository (\session -> readBlobBatchFromSession session [blobOid]))
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

slash :: Char -> Char
slash '\\' = '/'
slash value = value
