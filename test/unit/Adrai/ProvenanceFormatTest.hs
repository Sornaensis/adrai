{-# LANGUAGE OverloadedStrings #-}

module Adrai.ProvenanceFormatTest (tests) where

import Adrai.Format (renderDigest)
import Adrai.Provenance
import Adrai.Types
  ( ActorKind (..),
    Digest,
    ProvenanceInputs (..),
    actorId,
    actorKind,
    mkActor,
    mkAdrId,
    mkRecordId,
    operationIdText,
    recordObjectRef,
  )
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TextEncoding
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "provenance format"
    [ base64UrlTests,
      digestTests,
      normalizationTests,
      capsuleGoldenTests,
      capsuleRejectionTests
    ]

base64UrlTests :: TestTree
base64UrlTests =
  testGroup
    "unpadded base64url"
    [ testCase "matches every frozen encoding vector" $ do
        encodeBase64Url BS.empty @?= ""
        encodeBase64Url (BS.pack [0x00]) @?= "AA"
        encodeBase64Url (BS.pack [0x66]) @?= "Zg"
        encodeBase64Url (BS.pack [0x66, 0x6f]) @?= "Zm8"
        encodeBase64Url (BS.pack [0x66, 0x6f, 0x6f]) @?= "Zm9v"
        encodeBase64Url (BS.pack [0xfb, 0xff]) @?= "-_8"
        encodeBase64Url (BS.pack [0 .. 15]) @?= "AAECAwQFBgcICQoLDA0ODw",
      testCase "decodes canonical vectors and rejects every frozen malformed form" $ do
        decodeBase64Url "AAECAwQFBgcICQoLDA0ODw" @?= Right (BS.pack [0 .. 15])
        mapM_
          (\value -> assertBool ("expected rejection: " <> T.unpack value) (isLeft (decodeBase64Url value)))
          ["Zg==", "+/8", "a b", "A", "Zm9v!", "AB"]
    ]

digestTests :: TestTree
digestTests =
  testGroup
    "SHA-256"
    [ testCase "matches frozen UTF-8 digest vectors" $ do
        digestText "" @?= "sha256:47DEQpj8HBSa-_TImW-5JCeuQeRkm5NMpJWZG3hSuFU"
        digestText "abc" @?= "sha256:ungWv48Bz-pBQUDeXa4iI7ADYaOWF3qctBD_YfIAFa0"
        digestText "line one\nline two\n" @?= "sha256:6QJPGgfSnVKtOqXhoY6U2x86n9MrieOdR8RyzZkHHhM"
        digestText "Unicode: café Ω\n" @?= "sha256:4Q988xS8F8PCyxS16iWTuynyHuHSR3K2_eIxHhwONns",
      testCase "matches frozen decision semantic and sealed byte digests" $ do
        renderDigest (sha256Digest (TextEncoding.encodeUtf8 semanticDecision))
          @?= goldenSemanticDigest
        renderDigest (sha256Digest expectedSealedDecision)
          @?= "sha256:SPU81dXKlP3NUGt2a_qjv01vINGcvnssW9937wAnyuw",
      testCase "incremental frame digest equals the concatenated reference digest" $ do
        -- sha256DigestFrames must feed SHA-256 the exact same framed byte
        -- sequence as sha256Digest (BS.concat frames), so persisted
        -- fingerprints stay byte-stable when callers drop BS.concat.
        let frames = [BS.pack [0 .. 255], BS.replicate 300 0x7f, "adrai-frame/1\NUL", BS.empty]
        sha256DigestFrames frames @?= sha256Digest (BS.concat frames)
        renderDigest (sha256DigestFrames [])
          @?= "sha256:47DEQpj8HBSa-_TImW-5JCeuQeRkm5NMpJWZG3hSuFU"
    ]
  where
    digestText = renderDigest . sha256Digest . TextEncoding.encodeUtf8

normalizationTests :: TestTree
normalizationTests =
  testCase "matches every frozen semantic-normalization vector" $ do
    normalizeSemantic "alpha\nbeta\n" @?= "alpha\nbeta\n"
    normalizeSemantic "alpha  \r\nbeta\t\r\n\r\n" @?= "alpha\nbeta\n"
    normalizeSemantic "alpha\n  <!-- @adrai:YWJj -->  \nbeta\n" @?= "alpha\nbeta\n"
    normalizeSemantic "<!-- @adrai:not+url -->\n" @?= "<!-- @adrai:not+url -->\n"
    normalizeSemantic "" @?= "\n"
    renderDigest (semanticDigest "alpha  \r\nbeta\t\r\n\r\n")
      @?= "sha256:5JyB4tL4TiWdQOL7gZLzvNGYs1UYSEXXbY9YgH0NeO4"

capsuleGoldenTests :: TestTree
capsuleGoldenTests =
  testGroup
    "decision capsule golden"
    [ testCase "decodes, validates, and canonically re-encodes the frozen capsule" $ do
        capsule <- assertRight (decodeCapsule goldenEncodedCapsule)
        operationIdText (provenanceOperationId capsule) @?= "O00000000000000000000000000"
        provenanceObjectIdText (provenanceObjectId capsule) @?= "R00000000000000000000000000"
        eventKindText (provenanceEventKind capsule) @?= "decision.create"
        actorKind (provenanceActor capsule) @?= HumanActor
        actorId (provenanceActor capsule) @?= "architect"
        provenanceTimestampMs capsule @?= 1700000000000
        gitOidText (provenanceBasis capsule) @?= T.replicate 40 "0"
        provenanceParents capsule @?= []
        provenanceLineAnchors capsule @?= []
        provenanceToolVersion capsule @?= "adrai/1.0.0"
        renderDigest (provenanceSemanticDigest capsule) @?= goldenSemanticDigest
        record <- assertRight (mkRecordId "R00000000000000000000000000")
        validateCapsule (recordObjectRef record) (semanticDigest semanticDecision) capsule @?= Right ()
        encodeCapsule capsule @?= goldenEncodedCapsule,
      testCase "seals the canonical semantic decision to the exact 649 frozen bytes" $ do
        capsule <- assertRight (decodeCapsule goldenEncodedCapsule)
        let actual = TextEncoding.encodeUtf8 (sealSemantic semanticDecision capsule)
        BS.length actual @?= 649
        actual @?= expectedSealedDecision,
      testCase "canonical JSON recursively sorts keys and ASCII-escapes Unicode" $ do
        golden <- assertRight (decodeCapsule goldenEncodedCapsule)
        adr <- assertRight (mkAdrId "A00000000000000000000000000")
        actor <- assertRight (mkActor HumanActor "architect" (Just "mødel"))
        anchor <- assertRight (mkLineAnchor "trunk" (provenanceBasis golden))
        capsule <-
          assertRight . mkProvenanceCapsule $
            ProvenanceCapsuleInput
              { capsuleInputOperationId = provenanceOperationId golden,
                capsuleInputObjectId = provenanceObjectId golden,
                capsuleInputEventKind = provenanceEventKind golden,
                capsuleInputActor = actor,
                capsuleInputTimestampMs = provenanceTimestampMs golden,
                capsuleInputBasis = provenanceBasis golden,
                capsuleInputParents = [ProvenanceAdr adr],
                capsuleInputBranchHint = Just ("br" <> T.singleton '\DEL' <> "ånch"),
                capsuleInputUpstreamHint = Just "refs/heads/main",
                capsuleInputLineAnchors = [anchor],
                capsuleInputSemanticDigest = provenanceSemanticDigest golden,
                capsuleInputToolVersion = provenanceToolVersion golden,
                capsuleInputDigests = ProvenanceInputs (Just auxiliaryDigest) (Just auxiliaryDigest) (Just auxiliaryDigest)
              }
        jsonBytes <- assertRight (decodeBase64Url (encodeCapsule capsule))
        TextEncoding.decodeUtf8 jsonBytes @?= expectedAllFieldsJson
        decodeCapsule (encodeCapsule capsule) @?= Right capsule
    ]

capsuleRejectionTests :: TestTree
capsuleRejectionTests =
  testGroup
    "strict capsule rejection"
    [ testCase "rejects malformed base64url, JSON, and non-object JSON" $ do
        assertBool "padding" (isLeft (decodeCapsule (goldenEncodedCapsule <> "=")))
        assertBool "invalid length" (isLeft (decodeCapsule "A"))
        assertBool "invalid JSON" (isLeft (decodeCapsule (encodeJson "{")))
        assertBool "non-object JSON" (isLeft (decodeCapsule (encodeJson "[]"))),
      testCase "rejects unknown, missing, wrong-type, version, tool, and actor fields" $ do
        rejectJson (T.dropEnd 1 goldenCapsuleJson <> ",\"z\":true}")
        rejectJson (T.replace ",\"x\":\"adrai/1.0.0\"" "" goldenCapsuleJson)
        rejectJson (T.replace "\"t\":1700000000000" "\"t\":true" goldenCapsuleJson)
        rejectJson (T.replace "\"v\":1" "\"v\":2" goldenCapsuleJson)
        rejectJson (T.replace "\"x\":\"adrai/1.0.0\"" "\"x\":\"adrai/development\"" goldenCapsuleJson)
        rejectJson (T.replace "\"k\":\"human\"}" "\"k\":\"human\",\"role\":\"architect\"}" goldenCapsuleJson)
        rejectJson (T.replace "\"i\":\"architect\"" "\"i\":[\"architect\"]" goldenCapsuleJson),
      testCase "rejects duplicate root and nested actor keys before map decoding" $ do
        decodeCapsule (encodeJson (T.replace "\"v\":1" "\"v\":1,\"v\":1" goldenCapsuleJson))
          @?= Left (DuplicateCapsuleJsonKey "v")
        decodeCapsule (encodeJson (T.replace "\"k\":\"human\"" "\"k\":\"human\",\"k\":\"human\"" goldenCapsuleJson))
          @?= Left (DuplicateCapsuleJsonKey "k"),
      testCase "rejects decimal and exponent notation for required integers" $ do
        decodeCapsule (encodeJson (T.replace "\"t\":1700000000000" "\"t\":1700000000000.0" goldenCapsuleJson))
          @?= Left (InvalidCapsuleField "t" "must use integer JSON notation")
        decodeCapsule (encodeJson (T.replace "\"v\":1" "\"v\":1e0" goldenCapsuleJson))
          @?= Left (InvalidCapsuleField "v" "must use integer JSON notation"),
      testCase "rejects malformed object and digest fields" $ do
        rejectJson (T.replace "R00000000000000000000000000" "Rbad" goldenCapsuleJson)
        rejectJson (T.replace goldenSemanticDigest "sha256:bad" goldenCapsuleJson)
        rejectJson (T.dropEnd 1 goldenCapsuleJson <> ",\"i\":\"sha256:bad\"}"),
      testCase "validation independently rejects object and semantic digest mismatches" $ do
        capsule <- assertRight (decodeCapsule goldenEncodedCapsule)
        otherRecord <- assertRight (mkRecordId "R00000000000000000000000001")
        expectedRecord <- assertRight (mkRecordId "R00000000000000000000000000")
        assertBool
          "object mismatch"
          (isLeft (validateCapsule (recordObjectRef otherRecord) (semanticDigest semanticDecision) capsule))
        assertBool
          "digest mismatch"
          (isLeft (validateCapsule (recordObjectRef expectedRecord) auxiliaryDigest capsule))
    ]

rejectJson :: Text -> IO ()
rejectJson value =
  assertBool ("expected capsule rejection: " <> T.unpack value) (isLeft (decodeCapsule (encodeJson value)))

encodeJson :: Text -> Text
encodeJson = encodeBase64Url . TextEncoding.encodeUtf8

assertRight :: (Show error) => Either error value -> IO value
assertRight result =
  case result of
    Left problem -> assertFailure (show problem) >> fail "unreachable"
    Right value -> pure value

auxiliaryDigest :: Digest
auxiliaryDigest = sha256Digest "auxiliary"

goldenSemanticDigest :: Text
goldenSemanticDigest = "sha256:9qnNalcefUIDnN7WAhrCbt4EmSAH8wpepZOfgwh0uYY"

semanticDecision :: Text
semanticDecision =
  T.unlines
    [ "+++",
      "schema = \"adrai/decision/v1\"",
      "adr = \"A00000000000000000000000000\"",
      "record = \"R00000000000000000000000000\"",
      "title = \"Stable cache identity\"",
      "summary = \"Cache keys derive from semantic inputs.\"",
      "domains = [\"compiler.cache.identity\"]",
      "+++",
      "",
      "## Decision",
      "Use semantic inputs."
    ]

expectedSealedDecision :: BS.ByteString
expectedSealedDecision =
  TextEncoding.encodeUtf8
    ( semanticDecision
        <> "\n<!-- @adrai:"
        <> goldenEncodedCapsule
        <> " -->\n"
    )

goldenCapsuleJson :: Text
goldenCapsuleJson =
  "{\"a\":{\"i\":\"architect\",\"k\":\"human\"},\"b\":\"0000000000000000000000000000000000000000\",\"k\":\"decision.create\",\"o\":\"R00000000000000000000000000\",\"op\":\"O00000000000000000000000000\",\"s\":\"sha256:9qnNalcefUIDnN7WAhrCbt4EmSAH8wpepZOfgwh0uYY\",\"t\":1700000000000,\"v\":1,\"x\":\"adrai/1.0.0\"}"

goldenEncodedCapsule :: Text
goldenEncodedCapsule =
  "eyJhIjp7ImkiOiJhcmNoaXRlY3QiLCJrIjoiaHVtYW4ifSwiYiI6IjAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAiLCJrIjoiZGVjaXNpb24uY3JlYXRlIiwibyI6IlIwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMCIsIm9wIjoiTzAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwIiwicyI6InNoYTI1Njo5cW5OYWxjZWZVSURuTjdXQWhyQ2J0NEVtU0FIOHdwZXBaT2Znd2gwdVlZIiwidCI6MTcwMDAwMDAwMDAwMCwidiI6MSwieCI6ImFkcmFpLzEuMC4wIn0"

expectedAllFieldsJson :: Text
expectedAllFieldsJson =
  "{\"a\":{\"i\":\"architect\",\"k\":\"human\",\"m\":\"m\\u00f8del\"},\"b\":\"0000000000000000000000000000000000000000\",\"c\":\""
    <> auxiliaryDigestText
    <> "\",\"g\":[[\"trunk\",\"0000000000000000000000000000000000000000\"]],\"i\":\""
    <> auxiliaryDigestText
    <> "\",\"k\":\"decision.create\",\"o\":\"R00000000000000000000000000\",\"op\":\"O00000000000000000000000000\",\"p\":[\"A00000000000000000000000000\"],\"q\":\""
    <> auxiliaryDigestText
    <> "\",\"r\":\"br\\u007f\\u00e5nch\",\"s\":\"sha256:9qnNalcefUIDnN7WAhrCbt4EmSAH8wpepZOfgwh0uYY\",\"t\":1700000000000,\"u\":\"refs/heads/main\",\"v\":1,\"x\":\"adrai/1.0.0\"}"

auxiliaryDigestText :: Text
auxiliaryDigestText = renderDigest auxiliaryDigest
