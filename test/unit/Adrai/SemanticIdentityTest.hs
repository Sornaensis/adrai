{-# LANGUAGE OverloadedStrings #-}

module Adrai.SemanticIdentityTest (tests) where

import Adrai.Provenance (normalizeSemantic, semanticDigest)
import Adrai.Format.Document (parseManagedDocument, parsedManagedSemantic)
import Adrai.Types (mkRepoPath)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "semantic identity"
    [ testCase "sealed provenance never changes semantic identity" provenanceIndependent,
      testCase "all prototype Unicode line boundaries normalize to LF" unicodeLineBoundaries,
      testCase "managed parsing recognizes Unicode line boundaries" unicodeManagedDocument,
      testCase "semantic edits change identity while trailer edits do not" semanticMeaning
    ]

provenanceIndependent :: IO ()
provenanceIndependent = do
  semantic <- readUtf8 "test/fixtures/contracts/v1/documents/decision-create.semantic.md"
  sealed <- readUtf8 "test/fixtures/contracts/v1/documents/decision-create.sealed.md"
  semanticDigest sealed @?= semanticDigest semantic
  normalizeSemantic sealed @?= normalizeSemantic semantic

unicodeLineBoundaries :: IO ()
unicodeLineBoundaries =
  normalizeSemantic "alpha\vbeta\fgamma\x001c\&delta\x001d\&epsilon\x001e\&zeta\x0085\&eta\x2028\&theta\x2029\&iota\r\nkappa\r"
    @?= "alpha\nbeta\ngamma\ndelta\nepsilon\nzeta\neta\ntheta\niota\nkappa\n"

unicodeManagedDocument :: IO ()
unicodeManagedDocument = do
  sealed <- readUtf8 "test/fixtures/contracts/v1/documents/decision-create.sealed.md"
  semantic <- readUtf8 "test/fixtures/contracts/v1/documents/decision-create.semantic.md"
  path <-
    case mkRepoPath "architecture/adrai/decisions/R000/R00000000000000000000000000--stable-cache-identity.decision.md" of
      Left problem -> assertFailure (show problem) >> fail "unreachable"
      Right value -> pure value
  parsed <-
    case parseManagedDocument path (TextEncoding.encodeUtf8 (Text.replace "\n" "\x2028" sealed)) of
      Left problem -> assertFailure (show problem) >> fail "unreachable"
      Right value -> pure value
  parsedManagedSemantic parsed @?= semantic

semanticMeaning :: IO ()
semanticMeaning = do
  semanticDigest "alpha\n<!-- @adrai:YWJj -->\n" @?= semanticDigest "alpha\n"
  if semanticDigest "alpha\n" == semanticDigest "beta\n"
    then assertFailure "distinct semantic content produced the same test digest"
    else pure ()

readUtf8 :: FilePath -> IO Text
readUtf8 path = do
  bytes <- ByteString.readFile path
  case TextEncoding.decodeUtf8' bytes of
    Left problem -> assertFailure (show problem) >> fail "unreachable"
    Right value -> pure value
