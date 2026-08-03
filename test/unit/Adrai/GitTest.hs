{-# LANGUAGE OverloadedStrings #-}

module Adrai.GitTest (tests) where

import Adrai.Git
import Adrai.Provenance (GitOid, gitOidText, mkGitOid)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Either (isLeft)
import qualified Data.Text as Text
import Numeric (showHex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

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
        concat chunks @?= map oid [0 .. 300],
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
        fmap (map gitTreeMode) (decodeGitTreeOutput valid) @?= Right [GitRegularFile]
        assertBool "missing terminal NUL" (isLeft (decodeGitTreeOutput (BS.init valid)))
        assertBool "empty record" (isLeft (decodeGitTreeOutput (valid <> "\NUL")))
        assertBool "noncanonical metadata whitespace" (isLeft (decodeGitTreeOutput (BS8.pack ("100644  blob " <> Text.unpack (gitOidText (oid 1)) <> "\tfile.txt\NUL"))))
        assertBool "invalid UTF-8 path" (isLeft (decodeGitTreeOutput (BS8.pack ("100644 blob " <> Text.unpack (gitOidText (oid 1)) <> "\t") <> BS.pack [0x80, 0x00]))),
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
        assertBool "blob mismatch" (isLeft (decodeGitBlobHeader expected (other <> " blob 7") other "blob" "7"))
        assertBool "blob type" (isLeft (decodeGitBlobHeader expected (returned <> " commit 7") returned "commit" "7"))
        decodeGitBlobPayload expected 3 "abc" "\n" @?= Right (GitBlob expected "abc")
        assertBool "truncated payload" (isLeft (decodeGitBlobPayload expected 4 "abc" "\n"))
        assertBool "missing framing LF" (isLeft (decodeGitBlobPayload expected 3 "abc" "x"))
        validateGitBatchTrailing BS.empty @?= Right ()
        assertBool "trailing bytes" (isLeft (validateGitBatchTrailing "x"))
    ]

oid :: Int -> GitOid
oid value =
  case mkGitOid (Text.pack (pad <> hex)) of
    Left problem -> error (show problem)
    Right result -> result
  where
    hex = showHex value ""
    pad = replicate (40 - length hex) '0'
