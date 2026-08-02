{-# LANGUAGE OverloadedStrings #-}

module Adrai.FormatFoundationTest (tests) where

import Adrai.Format
import Adrai.Types
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "foundation formats"
    [ identifierFormatTests,
      digestFormatTests,
      tokenFormatTests,
      schemaFormatTests
    ]

identifierFormatTests :: TestTree
identifierFormatTests =
  testGroup
    "identifiers"
    [ testCase "parsers case-normalize valid A/R/C/O identifiers" $ do
        fmap adrIdText (parseAdrId (T.toLower adrText)) @?= Right adrText
        fmap recordIdText (parseRecordId (T.toLower recordText)) @?= Right recordText
        fmap connectionIdText (parseConnectionId (T.toLower connectionText)) @?= Right connectionText
        fmap operationIdText (parseOperationId (T.toLower operationText)) @?= Right operationText,
      testCase "parsers reject malformed and whitespace-padded identifiers" $ do
        assertBool "wrong kind" (isLeft (parseAdrId recordText))
        assertBool "too short" (isLeft (parseRecordId "R123"))
        assertBool "invalid Crockford character" (isLeft (parseConnectionId "C0123456789ABCDEFGHIKMNPQRS"))
        assertBool "surrounding whitespace" (isLeft (parseOperationId (" " <> operationText))),
      testCase "object references render canonically" $
        case parseAdrId (T.toLower adrText) of
          Left problem -> assertFailure (show problem)
          Right identifier -> renderObjectRef (adrObjectRef identifier) @?= adrText
    ]

digestFormatTests :: TestTree
digestFormatTests =
  testGroup
    "digests"
    [ testCase "renders the zero digest using canonical unpadded base64url" $
        case mkDigest (BS.replicate 32 0) of
          Left problem -> assertFailure (show problem)
          Right digest -> renderDigest digest @?= zeroDigestText,
      testCase "parses the zero-digest golden" $
        fmap digestBytes (parseDigest zeroDigestText) @?= Right (BS.replicate 32 0),
      testCase "digest codec round trips binary data" $
        case mkDigest (BS.pack [0 .. 31]) of
          Left problem -> assertFailure (show problem)
          Right digest -> parseDigest (renderDigest digest) @?= Right digest,
      testCase "rejects malformed digest text strictly" $ do
        assertBool "wrong scheme" (isLeft (parseDigest ("SHA256:" <> T.drop 7 zeroDigestText)))
        assertBool "padding is forbidden" (isLeft (parseDigest (zeroDigestText <> "=")))
        assertBool "standard base64 slash is forbidden" (isLeft (parseDigest (T.init zeroDigestText <> "/")))
        assertBool "wrong decoded length" (isLeft (parseDigest "sha256:YWJj"))
        assertBool "noncanonical trailing bits" (isLeft (parseDigest (T.init zeroDigestText <> "B")))
    ]

tokenFormatTests :: TestTree
tokenFormatTests =
  testGroup
    "state tokens"
    [ testCase "state-token codec accepts and preserves canonical text" $
        case parseStateToken canonicalToken of
          Left problem -> assertFailure (show problem)
          Right token -> renderStateToken token @?= canonicalToken,
      testCase "state-token parser rejects wrong prefix, size, padding, and Unicode" $ do
        traverse_
          (\value -> assertBool ("expected rejection: " <> T.unpack value) (isLeft (parseStateToken value)))
          [ "Tabcdefghijklmnopqrstuv",
            "Sshort",
            "Sabcdefghijklmnopqrstu=",
            "Sabcdefghijklmnopqrstuø"
          ]
    ]

schemaFormatTests :: TestTree
schemaFormatTests =
  testGroup
    "schema tags"
    [ testCase "config schema number is exact" $
        configSchemaNumber ConfigSchemaV1 @?= 1,
      testCase "source schema tags are exact" $ do
        sourceSchemaText DecisionSourceV1 @?= "adrai/decision/v1"
        sourceSchemaText ConnectionSourceV1 @?= "adrai/connection/v1",
      testCase "public schema tags are exact" $ do
        publicSchemaText SearchPublicV1 @?= "adrai/search/v1"
        publicSchemaText RelevantPublicV1 @?= "adrai/relevant/v1"
        publicSchemaText HistoryPublicV1 @?= "adrai/history/v1"
        publicSchemaText ShowCollapsedPublicV1 @?= "adrai/show-collapsed/v1"
        publicSchemaText ShowExplodedPublicV1 @?= "adrai/show-exploded/v1"
        publicSchemaText EventsPublicV1 @?= "adrai/events/v1"
    ]

zeroDigestText :: T.Text
zeroDigestText = "sha256:AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"

canonicalToken :: T.Text
canonicalToken = "Sabcdefghijklmnopqrstuv"

adrText :: T.Text
adrText = "A0123456789ABCDEFGHJKMNPQRS"

recordText :: T.Text
recordText = "R0123456789ABCDEFGHJKMNPQRS"

connectionText :: T.Text
connectionText = "C0123456789ABCDEFGHJKMNPQRS"

operationText :: T.Text
operationText = "O0123456789ABCDEFGHJKMNPQRS"
