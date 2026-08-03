{-# LANGUAGE OverloadedStrings #-}

module Adrai.FormatProperties (tests) where

import Adrai.Format.Config
import Adrai.Format.Document
import Adrai.Format.Json
import Adrai.Property.Generators
import Adrai.Provenance
import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Hedgehog (Property, assert, footnote, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "P2-06 format properties"
    [ testProperty "configuration render and parse are canonical inverses" prop_configRoundTrip,
      testProperty "capsule encoding is canonical and decodes exactly" prop_capsuleRoundTrip,
      testProperty "managed documents preserve canonical sealed bytes" prop_managedDocumentRoundTrip,
      testProperty "printable Unicode survives managed document round trips" prop_unicodeRoundTrip,
      testProperty "semantic digests ignore line endings and capsule trailers" prop_semanticNormalization,
      testProperty "canonical JSON is key-order independent ASCII" prop_canonicalJson,
      testProperty "single malformed mutations retain structured error classes" prop_malformedClasses
    ]

prop_configRoundTrip :: Property
prop_configRoundTrip = withTests 150 . property $ do
  config <- forAll genConfig
  parseConfigText (renderConfig config) === Right config

prop_capsuleRoundTrip :: Property
prop_capsuleRoundTrip = withTests 150 . property $ do
  spec <- forAll genValidDocumentSpec
  let capsule = sealedDocumentCapsule (sealValidDocument spec)
      encoded = encodeCapsule capsule
  decodeCapsule encoded === Right capsule
  fmap encodeCapsule (decodeCapsule encoded) === Right encoded

prop_managedDocumentRoundTrip :: Property
prop_managedDocumentRoundTrip = withTests 150 . property $ do
  spec <- forAll genValidDocumentSpec
  let sealed = sealValidDocument spec
  parsed <- forAll (pure (parseManagedDocument (sealedDocumentPath sealed) (sealedDocumentBytes sealed)))
  case parsed of
    Left problem -> footnote (show problem) >> assert False
    Right document -> do
      parsedManagedRecord document === sealedDocumentRecord sealed
      parsedManagedCapsule document === sealedDocumentCapsule sealed
      parsedManagedSemantic document === sealedDocumentSemantic sealed
      sealManagedDocument (parsedManagedRecord document) (parsedManagedCapsule document)
        === Right (sealedDocumentBytes sealed)

prop_unicodeRoundTrip :: Property
prop_unicodeRoundTrip = withTests 120 . property $ do
  base <- forAll genValidDocumentSpec
  unicode <- forAll (Gen.element ["caf\233", "M\248del \937", "emoji \128512", "\26085\26412\35486 \35373\35336"])
  let spec = base {validDocumentText = unicode}
      sealed = sealValidDocument spec
      parsed = parseManagedDocument (sealedDocumentPath sealed) (sealedDocumentBytes sealed)
  case parsed of
    Left problem -> footnote (show problem) >> assert False
    Right document -> do
      assert (unicode `Text.isInfixOf` parsedManagedSemantic document)
      parsedManagedRecord document === sealedDocumentRecord sealed

prop_semanticNormalization :: Property
prop_semanticNormalization = withTests 150 . property $ do
  spec <- forAll genValidDocumentSpec
  let sealed = sealValidDocument spec
      semantic = sealedDocumentSemantic sealed
      sealedText = TextEncoding.decodeUtf8 (sealedDocumentBytes sealed)
      crlf = Text.replace "\n" "\r\n" sealedText
  semanticDigest sealedText === semanticDigest semantic
  semanticDigest crlf === semanticDigest semantic

prop_canonicalJson :: Property
prop_canonicalJson = withTests 120 . property $ do
  value <- forAll (Gen.element ["caf\233", "\937", "\128512", "\26085\26412\35486", "line\nvalue"])
  let first = renderCanonicalJsonBytes (object [("z", JsonString value), ("a", JsonString "stable")])
      reversed = renderCanonicalJsonBytes (object [("a", JsonString "stable"), ("z", JsonString value)])
  first === reversed
  assert (BS.all (< 0x80) first)
  assert (BS.isSuffixOf "\n" first)

prop_malformedClasses :: Property
prop_malformedClasses = withTests 100 . property $ do
  firstMutation <- forAll genMalformedMutation
  base <- forAll genValidDocumentSpec
  let decision = sealValidDocument (base {validDocumentKind = DecisionDocument})
      connection = sealValidDocument (base {validDocumentKind = AmendmentDocument})
      mutations = firstMutation : filter (/= firstMutation) [minBound .. maxBound]
  forM_ mutations $ \mutation -> do
    let (matched, detail) = classifyMutation mutation decision connection
    footnote (show mutation <> ": " <> detail)
    assert matched

classifyMutation :: MalformedMutation -> SealedDocument -> SealedDocument -> (Bool, String)
classifyMutation mutation decision connection =
  case mutation of
    MutationUnknownConfigRoot ->
      classify (parseConfigText "schema = 1\n[cache]\nenabled = true\n") isUnknownRoot
    MutationUnknownConfigPath ->
      classify (parseConfigText "schema = 1\n[paths]\ndatabase = \"state\"\n") isUnknownPath
    MutationUnknownConfigLine ->
      classify (parseConfigText "schema = 1\n[[line]]\nid = \"trunk\"\nrefs = [\"refs/heads/main\"]\ndescription = \"x\"\n") isUnknownLine
    MutationUnsupportedManagedSchema ->
      let decisionResult = parseChanged decision (Text.replace "adrai/decision/v1" "adrai/decision/v2")
          connectionResult = parseChanged connection (Text.replace "adrai/connection/v1" "adrai/connection/v2")
       in classifyPair decisionResult connectionResult isUnsupportedSchema
    MutationUnknownDecisionField ->
      classify (parseChanged decision (Text.replace "summary = " "owner = \"x\"\nsummary = ")) isUnknownDecision
    MutationUnknownConnectionField ->
      classify (parseChanged connection (Text.replace "+++\n\n" "priority = \"high\"\n+++\n\n")) isUnknownConnection
    MutationUnknownRelation ->
      classify (parseChanged connection (Text.replace "relation = \"amends\"" "relation = \"retires\"")) isUnknownRelation
    MutationInvalidManagedUtf8 ->
      classify (parseManagedDocument (sealedDocumentPath decision) (BS.pack [0xff])) isInvalidUtf8
    MutationUnknownCapsuleField ->
      classify (decodeMutatedCapsule decision (\json -> Text.dropEnd 1 json <> ",\"z\":true}")) isUnknownCapsule
    MutationUnsupportedCapsuleVersion ->
      classify (decodeMutatedCapsule decision (Text.replace "\"v\":1" "\"v\":2")) isCapsuleVersion
    MutationInvalidCapsuleTool ->
      classify (decodeMutatedCapsule decision (Text.replace "\"x\":\"adrai/1.0.0\"" "\"x\":\"adrai/development\"")) isCapsuleTool
    MutationUnknownCapsuleActorField ->
      classify (decodeMutatedCapsule decision (Text.replace "\"k\":\"human\"}" "\"k\":\"human\",\"role\":\"architect\"}")) isActorKeys
  where
    isUnknownRoot (ConfigUnknownKey "root" "cache") = True
    isUnknownRoot _ = False
    isUnknownPath (ConfigUnknownKey "paths" "database") = True
    isUnknownPath _ = False
    isUnknownLine (ConfigUnknownKey "line[0]" "description") = True
    isUnknownLine _ = False
    isUnsupportedSchema (DocumentUnsupportedSchema schema) = schema `elem` ["adrai/decision/v2", "adrai/connection/v2"]
    isUnsupportedSchema _ = False
    isUnknownDecision (DocumentUnknownKey "owner") = True
    isUnknownDecision _ = False
    isUnknownConnection (DocumentUnknownKey "priority") = True
    isUnknownConnection _ = False
    isUnknownRelation (DocumentUnsupportedRelation "retires") = True
    isUnknownRelation _ = False
    isInvalidUtf8 (DocumentInvalidUtf8 _) = True
    isInvalidUtf8 _ = False
    isUnknownCapsule (UnsupportedCapsuleKeys ["z"]) = True
    isUnknownCapsule _ = False
    isCapsuleVersion (UnsupportedCapsuleVersion 2) = True
    isCapsuleVersion _ = False
    isCapsuleTool (InvalidCapsuleField "x" _) = True
    isCapsuleTool _ = False
    isActorKeys (UnsupportedActorKeys ["role"]) = True
    isActorKeys _ = False

parseChanged :: SealedDocument -> (Text -> Text) -> Either DocumentError ParsedManagedDocument
parseChanged sealed mutate =
  parseManagedDocument
    (sealedDocumentPath sealed)
    (TextEncoding.encodeUtf8 (mutate (TextEncoding.decodeUtf8 (sealedDocumentBytes sealed))))

decodeMutatedCapsule :: SealedDocument -> (Text -> Text) -> Either ProvenanceError ProvenanceCapsule
decodeMutatedCapsule sealed mutate = do
  jsonBytes <- decodeBase64Url (encodeCapsule (sealedDocumentCapsule sealed))
  let json = TextEncoding.decodeUtf8 jsonBytes
  decodeCapsule (encodeBase64Url (TextEncoding.encodeUtf8 (mutate json)))

classify :: (Show error) => Either error value -> (error -> Bool) -> (Bool, String)
classify result predicate =
  case result of
    Left problem -> (predicate problem, show problem)
    Right _ -> (False, "mutation unexpectedly succeeded")

classifyPair :: (Show error) => Either error first -> Either error second -> (error -> Bool) -> (Bool, String)
classifyPair first second predicate =
  case (first, second) of
    (Left firstProblem, Left secondProblem) ->
      ( predicate firstProblem && predicate secondProblem,
        show firstProblem <> " | " <> show secondProblem
      )
    _ -> (False, "one schema mutation unexpectedly succeeded")
