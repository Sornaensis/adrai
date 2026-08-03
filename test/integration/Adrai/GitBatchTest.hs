{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Adrai.GitBatchTest (tests) where

import Adrai.Git
import Adrai.GitTestSupport
import Adrai.Provenance (mkGitOid)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Directory (createDirectoryIfMissing, createFileLink, removeFile)
import System.FilePath ((</>))
import System.IO.Error (tryIOError)
import System.IO.Temp (withSystemTempDirectory)
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
      testCase "batch info and blobs are deterministic across duplicates and the 256 boundary" $
        withRepository $ \repository discovered -> do
          oidTexts <- mapM (hashObject repository . TextEncoding.encodeUtf8 . Text.pack . ("blob-" <>) . show) [0 :: Int .. 256]
          let oids = map requireOid oidTexts
              permuted = reverse oids <> take 4 oids
          batchObjectInfo discovered permuted >>= \case
            Left problem -> assertFailure (show problem)
            Right infos -> do
              Map.size infos @?= 257
              assertBool "all blobs" (all ((== Just GitBlobObject) . fmap objectInfoType) (Map.elems infos))
          readBlobBatch discovered permuted >>= \case
            Left problem -> assertFailure (show problem)
            Right blobs -> do
              Map.size blobs @?= 257
              Map.keys blobs @?= Map.keys (Map.fromList [(oid, ()) | oid <- oids]),
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
      testCase "fold callback IO exceptions are not misclassified as executable failures" $
        withRepository $ \repository discovered -> do
          blobText <- hashObject repository "callback"
          let blobOid = requireOid blobText
          attempted <-
            tryIOError
              ( foldBlobBatch
                  discovered
                  [blobOid]
                  ()
                  (\() _ -> ioError (userError "caller callback failure"))
              )
          case attempted of
            Left problem -> assertBool "original callback exception remains visible" ("caller callback failure" `Text.isInfixOf` Text.pack (show problem))
            Right result -> assertFailure ("expected callback IOException, got " <> show result),
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
