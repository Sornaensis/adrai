{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Adrai.GitTest (tests) where

import Adrai.Git
import Adrai.GitTestSupport (commitFile, gitSuccess, initTestRepository)
import Adrai.Provenance (mkGitOid)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Either (isLeft)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Numeric (showHex)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Git contract"
    [ testCase "revision specs preserve safe expressions and reject framing or option input" $ do
        revisionSpecText <$> mkRevisionSpec "feature/release~2" @?= Right "feature/release~2"
        assertBool "empty" (isLeft (mkRevisionSpec ""))
        assertBool "leading option" (isLeft (mkRevisionSpec "--help"))
        assertBool "LF" (isLeft (mkRevisionSpec "HEAD\nmain"))
        assertBool "CR" (isLeft (mkRevisionSpec "HEAD\rmain"))
        assertBool "NUL" (isLeft (mkRevisionSpec "HEAD\NULmain")),
      testCase "object batches are sorted, unique, and capped at 256" $ do
        let values = map oid [300, 299 .. 0] <> [oid 42, oid 42]
            chunks = canonicalObjectChunks values
        map length chunks @?= [256, 45]
        concat chunks @?= map oid [0 .. 300]
        objectBatchRequestCount values @?= 2
        objectInfoBatchSessionCount values @?= 1
        objectBatchRequestCount [] @?= 0
        objectInfoBatchSessionCount [] @?= 0
        objectBatchRequestCount [oid 42, oid 42] @?= 1,
      testCase "persistent batch input writes bare OIDs and flushes exactly once per window" $ do
        writes <- newIORef []
        flushes <- newIORef (0 :: Int)
        let input =
              GitBatchInput
                { gitBatchInputWrite = \bytes -> modifyIORef' writes (bytes :),
                  gitBatchInputFlush = modifyIORef' flushes (+ 1)
                }
            expected = BS8.pack (Text.unpack (gitOidText (oid 1)) <> "\n" <> Text.unpack (gitOidText (oid 2)) <> "\n")
        writePersistentBatchRequests input [oid 1, oid 2] >>= (@?= Right ())
        (reverse <$> readIORef writes) >>= (@?= [expected])
        readIORef flushes >>= (@?= 1)
        writePersistentBatchRequests input [] >>= (@?= Right ())
        readIORef flushes >>= (@?= 1),
      testCase "bounded diagnostics normalize newlines and cap untrusted output" $ do
        boundedDiagnostic "first\r\nsecond\rthird" @?= "first\nsecond\nthird"
        assertBool "diagnostic is bounded" (Text.length (boundedDiagnostic (BS.replicate 50000 120)) < 5000),
      testCase "strict UTF-8 convenience never replaces invalid blob bytes" $ do
        decodeGitBlobUtf8 (GitBlob (oid 1) (BS.pack [0x66, 0x80])) @?= Left (GitInvalidUtf8Blob (oid 1)),
      testCase "boolean and discovery-path protocols require exact framing" $ do
        decodeGitBoolean "true\n" @?= Right True
        decodeGitBoolean "false\r\n" @?= Right False
        assertBool "boolean requires LF" (isLeft (decodeGitBoolean "true"))
        assertBool "boolean rejects whitespace" (isLeft (decodeGitBoolean " true\n"))
        decodeGitPathOutput " C:/repo path \n" @?= Right " C:/repo path "
        assertBool "path requires LF" (isLeft (decodeGitPathOutput "C:/repo"))
        assertBool "path rejects multiple records" (isLeft (decodeGitPathOutput "C:/one\nC:/two\n"))
        assertBool "path rejects invalid UTF-8" (isLeft (decodeGitPathOutput (BS.pack [0x80, 0x0a]))),
      testCase "tree protocol requires terminal NUL, strict metadata, and UTF-8 paths" $ do
        let valid = BS8.pack ("100644 blob " <> Text.unpack (gitOidText (oid 1)) <> "\tfile.txt\NUL")
            directory = BS8.pack ("040000 tree " <> Text.unpack (gitOidText (oid 2)) <> "\tmanaged.connection.md\NUL")
        fmap (map gitTreeMode) (decodeGitTreeOutput valid) @?= Right [GitRegularFile]
        fmap (map (\entry -> (gitTreeMode entry, gitTreeObjectType entry))) (decodeGitTreeOutput directory) @?= Right [(GitDirectory, GitTreeObject)]
        assertBool "missing terminal NUL" (isLeft (decodeGitTreeOutput (BS.init valid)))
        assertBool "empty record" (isLeft (decodeGitTreeOutput (valid <> "\NUL")))
        assertBool "noncanonical metadata whitespace" (isLeft (decodeGitTreeOutput (BS8.pack ("100644  blob " <> Text.unpack (gitOidText (oid 1)) <> "\tfile.txt\NUL"))))
        assertBool "invalid UTF-8 path" (isLeft (decodeGitTreeOutput (BS8.pack ("100644 blob " <> Text.unpack (gitOidText (oid 1)) <> "\t") <> BS.pack [0x80, 0x00]))),
      testCase "history-delta protocol preserves root and every merge-parent edge" $ do
        let root = oid 1
            child = oid 2
            merge = oid 3
            blobA = oid 10
            blobB = oid 11
            graph = BS8.pack (Text.unpack (gitOidText root) <> "\n" <> Text.unpack (gitOidText child) <> " " <> Text.unpack (gitOidText root) <> "\n" <> Text.unpack (gitOidText merge) <> " " <> Text.unpack (gitOidText child) <> " " <> Text.unpack (gitOidText root) <> "\n")
            header value = "\x1e" <> BS8.pack (Text.unpack (gitOidText value)) <> "\NUL\n"
            emptyHeader value = "\x1e" <> BS8.pack (Text.unpack (gitOidText value)) <> "\NUL"
            path = "architecture/adrai/decisions/R001/example.decision.md\NUL"
            zero = BS8.replicate 40 '0'
            raw =
              header root
                <> ":000000 100644 " <> zero <> " " <> BS8.pack (Text.unpack (gitOidText blobA)) <> " A\NUL" <> path
                <> header child
                <> ":100644 100644 " <> BS8.pack (Text.unpack (gitOidText blobA)) <> " " <> BS8.pack (Text.unpack (gitOidText blobB)) <> " M\NUL" <> path
                <> emptyHeader merge
                <> header merge
                <> ":000000 100644 " <> zero <> " " <> BS8.pack (Text.unpack (gitOidText blobA)) <> " A\NUL" <> path
        case decodeGitCommitGraph graph >>= (`decodeGitHistoryTreeDeltas` raw) of
          Left problem -> assertFailure (show problem)
          Right deltas -> do
            map gitHistoryTreeDeltaCommit deltas @?= [root, child, merge, merge]
            map gitHistoryTreeDeltaParent deltas @?= [Nothing, Just root, Just child, Just root]
            map (length . gitHistoryTreeDeltaChanges) deltas @?= [1, 1, 0, 1]
        case decodeGitCommitGraph (BS8.pack (Text.unpack (gitOidText root) <> "\n")) >>= (`decodeGitHistoryTreeDeltas` (emptyHeader root)) of
          Right [GitHistoryTreeDelta actualRoot Nothing []] -> actualRoot @?= root
          unexpected -> assertFailure ("expected final empty edge, got " <> show unexpected)
        assertBool "history-delta framing is strict" (isLeft (decodeGitHistoryTreeDeltas [] raw)),
       testCase "history-delta protocol rejects zero OIDs on nonzero modes" $ do
         let root = oid 1
             graph = BS8.pack (Text.unpack (gitOidText root) <> "\n")
             header = "\x1e" <> BS8.pack (Text.unpack (gitOidText root)) <> "\NUL\n"
             path = "architecture/adrai/decisions/R001/example.decision.md\NUL"
             zero = BS8.replicate 40 '0'
             blob = BS8.pack (Text.unpack (gitOidText (oid 2)))
             longZero = BS8.replicate 64 '0'
             longBlob = BS8.replicate 63 '0' <> "1"
             shortZero = "0"
             malformedNew = header <> ":000000 100644 " <> zero <> " " <> zero <> " A\NUL" <> path
             malformedOld = header <> ":100644 100644 " <> zero <> " " <> blob <> " M\NUL" <> path
         assertBool "added side may not use a zero OID" (isLeft (decodeGitCommitGraph graph >>= (`decodeGitHistoryTreeDeltas` malformedNew)))
         assertBool "removed side may not use a zero OID" (isLeft (decodeGitCommitGraph graph >>= (`decodeGitHistoryTreeDeltas` malformedOld)))
         assertBool "short zero absent sentinel is rejected" (isLeft (decodeGitCommitGraph graph >>= (`decodeGitHistoryTreeDeltas` (header <> ":000000 100644 " <> shortZero <> " " <> blob <> " A\NUL" <> path))))
         assertBool "SHA-256 zero sentinel mismatches SHA-1 graph" (isLeft (decodeGitCommitGraph graph >>= (`decodeGitHistoryTreeDeltas` (header <> ":000000 100644 " <> longZero <> " " <> blob <> " A\NUL" <> path))))
         assertBool "SHA-256 nonzero side mismatches SHA-1 graph" (isLeft (decodeGitCommitGraph graph >>= (`decodeGitHistoryTreeDeltas` (header <> ":000000 100644 " <> zero <> " " <> longBlob <> " A\NUL" <> path))))
         assertBool "SHA-256 old nonzero side mismatches SHA-1 graph" (isLeft (decodeGitCommitGraph graph >>= (`decodeGitHistoryTreeDeltas` (header <> ":100644 100644 " <> longBlob <> " " <> blob <> " M\NUL" <> path))))
         let sha256Root = oid64 1
             sha256Graph = BS8.pack (Text.unpack (gitOidText sha256Root) <> "\n")
             sha256Header = "\x1e" <> BS8.pack (Text.unpack (gitOidText sha256Root)) <> "\NUL\n"
         assertBool "SHA-1 zero sentinel mismatches SHA-256 graph" (isLeft (decodeGitCommitGraph sha256Graph >>= (`decodeGitHistoryTreeDeltas` (sha256Header <> ":000000 100644 " <> zero <> " " <> longBlob <> " A\NUL" <> path))))
         assertBool "SHA-1 new nonzero side mismatches SHA-256 graph" (isLeft (decodeGitCommitGraph sha256Graph >>= (`decodeGitHistoryTreeDeltas` (sha256Header <> ":000000 100644 " <> longZero <> " " <> blob <> " A\NUL" <> path))))
         assertBool "SHA-1 old nonzero side mismatches SHA-256 graph" (isLeft (decodeGitCommitGraph sha256Graph >>= (`decodeGitHistoryTreeDeltas` (sha256Header <> ":100644 100644 " <> blob <> " " <> longBlob <> " M\NUL" <> path))))
         case decodeGitCommitGraph sha256Graph >>= (`decodeGitHistoryTreeDeltas` (sha256Header <> ":000000 100644 " <> longZero <> " " <> longBlob <> " A\NUL" <> path)) of
           Right [GitHistoryTreeDelta _ Nothing [GitTreeChange _ Nothing (Just entry)]] -> gitTreeOid entry @?= oid64 1
           unexpected -> assertFailure ("expected exact SHA-256 zero sentinel and object OID to parse, got " <> show unexpected),
        testCase "commit graph requires one nonzero OID width across commits and parents" $ do
          let sha1 = Text.unpack (gitOidText (oid 1))
              sha1Parent = Text.unpack (gitOidText (oid 2))
              sha256 = Text.unpack (gitOidText (oid64 1))
              zero40 = replicate 40 '0'
          assertBool "mixed child widths are rejected" (isLeft (decodeGitCommitGraph (BS8.pack (sha1 <> "\n" <> sha256 <> " " <> sha1 <> "\n"))))
          assertBool "mixed parent width is rejected" (isLeft (decodeGitCommitGraph (BS8.pack (sha1 <> " " <> sha256 <> "\n"))))
          assertBool "zero commit OID is rejected" (isLeft (decodeGitCommitGraph (BS8.pack (zero40 <> "\n"))))
          assertBool "zero parent OID is rejected" (isLeft (decodeGitCommitGraph (BS8.pack (sha1 <> " " <> zero40 <> "\n"))))
          assertBool "uniform nonzero graph is accepted" (not (isLeft (decodeGitCommitGraph (BS8.pack (sha1 <> "\n" <> sha1Parent <> " " <> sha1 <> "\n"))))),
        testCase "history-delta batches are bounded by merge arity, not commit count" $ do
         let root = oid 1
             linear = [oid value | value <- [2 .. 64]]
             merge = oid 65
             line child parents = Text.unpack (gitOidText child) <> concatMap ((" " <>) . Text.unpack . gitOidText) parents <> "\n"
             graph = BS8.pack (line root [] <> concat [line child [parent] | (parent, child) <- zip (root : linear) linear] <> line merge [oid 32, oid 64])
         case decodeGitCommitGraph graph of
           Left problem -> assertFailure (show problem)
           Right nodes -> do
             historyTreeDeltaBatchCount nodes @?= 2
             assertBool "ordinary selected commits do not create batches" (historyTreeDeltaBatchCount nodes < length nodes),
       testCase "batch headers reject mismatches, whitespace, invalid types, and size overflow" $ do
        let expected = oid 1
            returned = BS8.pack (Text.unpack (gitOidText expected))
            other = BS8.pack (Text.unpack (gitOidText (oid 2)))
        fmap (fmap objectInfoType . snd) (decodeGitObjectInfoHeader expected (returned <> " blob 7")) @?= Right (Just GitBlobObject)
        assertBool "returned OID mismatch" (isLeft (decodeGitObjectInfoHeader expected (other <> " blob 7")))
        assertBool "double spaces" (isLeft (decodeGitObjectInfoHeader expected (returned <> "  blob 7")))
        assertBool "unknown type" (isLeft (decodeGitObjectInfoHeader expected (returned <> " mystery 7")))
        assertBool "negative size" (isLeft (decodeGitObjectInfoHeader expected (returned <> " blob -1")))
        assertBool "Word64 overflow" (isLeft (decodeGitObjectInfoHeader expected (returned <> " blob 18446744073709551616")))
        decodeGitBlobHeader expected (returned <> " blob 7") returned "blob" "7" @?= Right 7
        assertBool "blob negative size" (isLeft (decodeGitBlobHeader expected (returned <> " blob -1") returned "blob" "-1"))
        let abovePlatformInt = BS8.pack (show (toInteger (maxBound :: Int) + 1))
        assertBool
          "blob size above the platform Int range"
          (isLeft (decodeGitBlobHeader expected (returned <> " blob " <> abovePlatformInt) returned "blob" abovePlatformInt))
        assertBool "blob mismatch" (isLeft (decodeGitBlobHeader expected (other <> " blob 7") other "blob" "7"))
        assertBool "blob type" (isLeft (decodeGitBlobHeader expected (returned <> " commit 7") returned "commit" "7"))
        decodeGitBlobPayload expected 3 "abc" "\n" @?= Right (GitBlob expected "abc")
        assertBool "truncated payload" (isLeft (decodeGitBlobPayload expected 4 "abc" "\n"))
        assertBool "missing framing LF" (isLeft (decodeGitBlobPayload expected 3 "abc" "x"))
        validateGitBatchTrailing BS.empty @?= Right ()
        assertBool "trailing bytes" (isLeft (validateGitBatchTrailing "x"))
        validateObjectInfoBatchTrailing BS.empty @?= Right ()
        assertBool "object-info trailing bytes" (isLeft (validateObjectInfoBatchTrailing "x"))
    , testCase "environment overrides isolate alternate Git indexes" $
        withSystemTempDirectory "adrai-git-environment" $ \temporary -> do
          let repositoryPath = temporary </> "repository"
              originalPath = "original-staged.txt"
              alternatePath = "alternate-staged.txt"
              alternateIndex = repositoryPath </> ".git" </> "adrai-alternate.index"
          initTestRepository repositoryPath
          _ <- commitFile repositoryPath "seed.txt" "seed"
          BS.writeFile (repositoryPath </> originalPath) "original staged content"
          _ <- gitSuccess repositoryPath ["add", "--", originalPath] BS.empty
          originalIndexBefore <- BS.readFile (repositoryPath </> ".git" </> "index")
          BS.writeFile (repositoryPath </> alternatePath) "alternate staged content"
          repository <- discoverRepository systemGit repositoryPath >>= \case
            Left problem -> assertFailure (show problem)
            Right discovered -> pure discovered
          alternateAdd <-
            runRepositoryWithEnvironment
              repository
              (Map.singleton "GIT_INDEX_FILE" alternateIndex)
              "alternate index add"
              ["add", "--", alternatePath]
              BS.empty
          case alternateAdd of
            Left problem -> assertFailure (show problem)
            Right result -> processExitCode result @?= ExitSuccess
          alternateEntries <-
            runRepositoryWithEnvironment
              repository
              (Map.singleton "GIT_INDEX_FILE" alternateIndex)
              "alternate index entries"
              ["diff", "--cached", "--name-only"]
              BS.empty
          case alternateEntries of
            Left problem -> assertFailure (show problem)
            Right result -> processStdout result @?= BS8.pack (alternatePath <> "\nseed.txt\n")
          originalIndexAfter <- BS.readFile (repositoryPath </> ".git" </> "index")
          originalIndexAfter @?= originalIndexBefore
    ]

oid :: Int -> GitOid
oid value =
  case mkGitOid (Text.pack (pad <> hex)) of
    Left problem -> error (show problem)
    Right result -> result
  where
    hex = showHex value ""
    pad = replicate (40 - length hex) '0'

oid64 :: Int -> GitOid
oid64 value =
  case mkGitOid (Text.pack (pad <> hex)) of
    Left problem -> error (show problem)
    Right result -> result
  where
    hex = showHex value ""
    pad = replicate (64 - length hex) '0'
