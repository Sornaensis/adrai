{-# LANGUAGE OverloadedStrings #-}

module Adrai.ManagedDocumentFormatTest (tests) where

import Adrai.Domain (mkDomain, parseDomainRefinement)
import Adrai.Format.Document
import Adrai.Provenance
import Adrai.Scope (mkScopePattern)
import Adrai.Types
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "managed document format"
    [ testCase "decision semantic and sealed fixture round trip exactly" decisionRoundTrip,
      testCase "amends renders fixed order, seals, and parses" $
        connectionRoundTrip amendsRecord expectedAmends,
      testCase "applies_to renders fixed order, seals, and parses" $
        connectionRoundTrip appliesToRecord expectedAppliesTo,
      testCase "domains renders fixed order, seals, and parses" $
        connectionRoundTrip domainsRecord expectedDomains,
      testCase "status renders fixed order, seals, and parses" $
        connectionRoundTrip statusRecord expectedStatus,
      testCase "TOML strings preserve Unicode and escape quotes and backslashes" unicodeEscaping,
      testCase "relation is required and unknown relations are rejected" relationFailures,
      testCase "relation arrays reject wrong types and duplicates" arrayFailures,
      testCase "scope arrays must contain canonical spellings" scopeFailure,
      testCase "status state and replacement rules are closed" statusFailures,
      testCase "front matter rejects unknown, missing, duplicate, and wrong-type keys" frontMatterFailures,
      testCase "managed front matter rejects TOML 1.1-only syntax" toml10PreflightFailures,
      testCase "capsule cardinality is exactly one" capsuleCardinality,
      testCase "capsule object and digest must match semantic content" capsuleValidation,
      testCase "capsule schema and keys remain closed" capsuleSchemaFailures,
      testCase "strict UTF-8 is required and CRLF is normalized" encodingAndLf,
      testCase "canonical managed paths are deterministic" canonicalPaths
    ]

decisionRoundTrip :: IO ()
decisionRoundTrip = do
  record <- decisionFixture
  capsule <- assertRight (decodeCapsule goldenEncodedCapsule)
  renderDecisionSemantic record @?= Right expectedDecisionSemantic
  sealManagedDocument (ManagedDecision record) capsule @?= Right expectedSealedDecision
  path <- pathOrFail "architecture/adrai/decisions/R000/R00000000000000000000000000--stable-cache-identity.decision.md"
  parsed <- assertRight (parseManagedDocument path expectedSealedDecision)
  parsedManagedRecord parsed @?= ManagedDecision record
  parsedManagedSemantic parsed @?= expectedDecisionSemantic
  parsedManagedBytes parsed @?= expectedSealedDecision
  case parsedManagedRecord parsed of
    ManagedDecision parsedDecision ->
      decisionBody parsedDecision @?= "## Decision\nUse semantic inputs.\n"
    other -> assertFailure ("expected parsed decision, got: " <> show other)

connectionRoundTrip :: IO ConnectionRecord -> Text -> IO ()
connectionRoundTrip buildRecord expected = do
  record <- buildRecord
  semantic <- assertRight (renderConnectionSemantic record)
  semantic @?= expected
  capsule <- capsuleFor (ManagedConnection record) semantic
  sealed <- assertRight (sealManagedDocument (ManagedConnection record) capsule)
  path <- assertRight (canonicalManagedPath (configManagedPaths defaultConfig) (ManagedConnection record))
  parsed <- assertRight (parseManagedDocument path sealed)
  parsedManagedRecord parsed @?= ManagedConnection record
  parsedManagedSemantic parsed @?= semantic

unicodeEscaping :: IO ()
unicodeEscaping = do
  base <- decisionFixture
  let record =
        base
          { decisionTitle = "Mødel \"cache\"",
            decisionSummary = "Path C:\\cache and café remain stable."
          }
  semantic <- assertRight (renderDecisionSemantic record)
  assertBool "printable Unicode is preserved" ("Mødel" `Text.isInfixOf` semantic)
  assertBool "quotes are escaped" ("\\\"cache\\\"" `Text.isInfixOf` semantic)
  assertBool "backslashes are escaped" ("C:\\\\cache" `Text.isInfixOf` semantic)

relationFailures :: IO ()
relationFailures = do
  sealed <- sealedConnection amendsRecord
  parseChanged sealed (Text.replace "relation = \"amends\"\n" "")
    @?= Left (DocumentMissingKey "relation")
  parseChanged sealed (Text.replace "relation = \"amends\"" "relation = \"links\"")
    @?= Left (DocumentUnsupportedRelation "links")

arrayFailures :: IO ()
arrayFailures = do
  sealed <- sealedConnection amendsRecord
  parseChanged sealed (Text.replace "to_records = [\"R00000000000000000000000000\"]" "to_records = \"R00000000000000000000000000\"")
    @?= Left (DocumentExpectedType "to_records" "array of strings" "string")
  parseChanged sealed (Text.replace "to_records = [\"R00000000000000000000000000\"]" "to_records = [\"R00000000000000000000000000\", \"R00000000000000000000000000\"]")
    @?= Left (DocumentDuplicateList "to_records")

scopeFailure :: IO ()
scopeFailure = do
  sealed <- sealedConnection appliesToRecord
  parseChanged sealed (Text.replace "added = [\"tests/compiler/cache/**\"]" "added = [\"./tests/compiler/cache/**\"]")
    @?= Left (DocumentNonCanonicalList "added")

statusFailures :: IO ()
statusFailures = do
  sealed <- sealedConnection statusRecord
  parseChanged sealed (Text.replace "state = \"obsolete\"" "state = \"retired\"")
    @?= Left (DocumentStatusRule "unsupported status state: retired")
  parseChanged sealed (Text.replace "state = \"obsolete\"" "state = \"active\"")
    @?= Left (DocumentStatusRule "active status may not name a replacement ADR")

frontMatterFailures :: IO ()
frontMatterFailures = do
  record <- decisionFixture
  capsule <- assertRight (decodeCapsule goldenEncodedCapsule)
  sealed <- assertRight (sealManagedDocument (ManagedDecision record) capsule)
  assertLeft (parseChanged sealed (Text.replace "summary = " "unknown = \"x\"\nsummary = ")) isUnknown
  parseChanged sealed (Text.replace "summary = \"Cache keys derive from semantic inputs.\"\n" "")
    @?= Left (DocumentMissingKey "summary")
  assertLeft
    (parseChanged sealed (Text.replace "summary = \"Cache keys derive from semantic inputs.\"" "summary = \"one\"\nsummary = \"two\""))
    isTomlError
  parseChanged sealed (Text.replace "domains = [\"compiler.cache.identity\"]" "domains = \"compiler.cache.identity\"")
    @?= Left (DocumentExpectedType "domains" "array of strings" "string")
  where
    isUnknown (DocumentUnknownKey "unknown") = True
    isUnknown _ = False
    isTomlError (DocumentTomlParseError _) = True
    isTomlError _ = False

toml10PreflightFailures :: IO ()
toml10PreflightFailures = do
  record <- decisionFixture
  capsule <- assertRight (decodeCapsule goldenEncodedCapsule)
  sealed <- assertRight (sealManagedDocument (ManagedDecision record) capsule)
  assertLeft
    (parseChanged sealed (Text.replace "Stable cache identity" "Stable \\e cache identity"))
    isToml10
  assertLeft
    (parseChanged sealed (Text.replace "Stable cache identity" "Stable \\x41 cache identity"))
    isToml10
  assertLeft
    (parseChanged sealed (Text.replace "summary = " "extra = { value = \"x\", }\nsummary = "))
    isToml10
  assertLeft
    (parseChanged sealed (Text.replace "summary = " "extra = {\nvalue = \"x\"\n}\nsummary = "))
    isToml10
  where
    isToml10 (DocumentToml10Error _) = True
    isToml10 _ = False

capsuleCardinality :: IO ()
capsuleCardinality = do
  path <- decisionPath
  parseManagedDocument path (TextEncoding.encodeUtf8 expectedDecisionSemantic)
    @?= Left (DocumentCapsuleCount 0)
  let duplicate = expectedSealedDecision <> TextEncoding.encodeUtf8 ("<!-- @adrai:" <> goldenEncodedCapsule <> " -->\n")
  parseManagedDocument path duplicate @?= Left (DocumentCapsuleCount 2)

capsuleValidation :: IO ()
capsuleValidation = do
  path <- decisionPath
  let wrongObjectSemantic = Text.replace record0 record1 expectedDecisionSemantic
      wrongObjectSealed = TextEncoding.encodeUtf8 (sealSemantic wrongObjectSemantic goldenCapsule)
  assertLeft (parseManagedDocument path wrongObjectSealed) isObjectMismatch
  let wrongDigestSemantic = Text.replace "Stable cache identity" "Stable cache identity changed" expectedDecisionSemantic
      wrongDigestSealed = TextEncoding.encodeUtf8 (sealSemantic wrongDigestSemantic goldenCapsule)
  assertLeft (parseManagedDocument path wrongDigestSealed) isDigestMismatch
  where
    isObjectMismatch (DocumentCapsuleError (CapsuleObjectMismatch _ _)) = True
    isObjectMismatch _ = False
    isDigestMismatch (DocumentCapsuleError (CapsuleDigestMismatch _ _)) = True
    isDigestMismatch _ = False

capsuleSchemaFailures :: IO ()
capsuleSchemaFailures = do
  path <- decisionPath
  let unknown = Text.dropEnd 1 goldenCapsuleJson <> ",\"z\":true}"
      version = Text.replace "\"v\":1" "\"v\":2" goldenCapsuleJson
  assertLeft (parseManagedDocument path (sealWithJson unknown)) isUnknown
  assertLeft (parseManagedDocument path (sealWithJson version)) isVersion
  where
    isUnknown (DocumentCapsuleError (UnsupportedCapsuleKeys ["z"])) = True
    isUnknown _ = False
    isVersion (DocumentCapsuleError (UnsupportedCapsuleVersion 2)) = True
    isVersion _ = False

encodingAndLf :: IO ()
encodingAndLf = do
  path <- decisionPath
  assertLeft (parseManagedDocument path (BS.pack [0xff])) isUtf8
  let crlf = TextEncoding.encodeUtf8 (Text.replace "\n" "\r\n" (TextEncoding.decodeUtf8 expectedSealedDecision))
  parsed <- assertRight (parseManagedDocument path crlf)
  parsedManagedSemantic parsed @?= expectedDecisionSemantic
  parsedManagedBytes parsed @?= expectedSealedDecision
  where
    isUtf8 (DocumentInvalidUtf8 _) = True
    isUtf8 _ = False

canonicalPaths :: IO ()
canonicalPaths = do
  decision <- decisionFixture
  connection <- amendsRecord
  let defaults = configManagedPaths defaultConfig
  fmap repoPathText (canonicalManagedPath defaults (ManagedDecision decision))
    @?= Right "architecture/adrai/decisions/R000/R00000000000000000000000000--stable-cache-identity.decision.md"
  fmap repoPathText (canonicalManagedPath defaults (ManagedConnection connection))
    @?= Right "architecture/adrai/connections/C000/C00000000000000000000000000--amends.connection.md"
  decisionRoot <- pathOrFail "custom/decisions"
  connectionRoot <- pathOrFail "custom/connections"
  custom <- assertRight (mkManagedPaths decisionRoot connectionRoot)
  fmap repoPathText (canonicalManagedPath custom (ManagedDecision decision))
    @?= Right "custom/decisions/R000/R00000000000000000000000000--stable-cache-identity.decision.md"
  fmap repoPathText (canonicalManagedPath custom (ManagedConnection connection))
    @?= Right "custom/connections/C000/C00000000000000000000000000--amends.connection.md"

decisionFixture :: IO DecisionRecord
decisionFixture = do
  adr <- adrOrFail adr0
  record <- recordOrFail record0
  domain <- assertRight (mkDomain "compiler.cache.identity")
  pure
    DecisionRecord
      { decisionAdr = adr,
        decisionRecord = record,
        decisionTitle = "Stable cache identity",
        decisionSummary = "Cache keys derive from semantic inputs.",
        decisionDomains = [domain],
        decisionBody = "## Decision\nUse semantic inputs.\n"
      }

amendsRecord :: IO ConnectionRecord
amendsRecord = do
  identifier <- connectionOrFail connection0
  subject <- adrOrFail adr0
  fromRecord <- recordOrFail record1
  target <- recordOrFail record0
  pure (ConnectionRecord identifier (AmendsConnection (AmendsPayload subject fromRecord [target])) "Replace the previous effective record.\n")

appliesToRecord :: IO ConnectionRecord
appliesToRecord = do
  identifier <- connectionOrFail connection1
  subject <- adrOrFail adr0
  parent <- connectionOrFail connection0
  added <- assertRight (mkScopePattern "tests/compiler/cache/**")
  effective <- assertRight (mkScopePattern "src/compiler/cache/**")
  pure
    ( ConnectionRecord
        identifier
        (AppliesToConnection (AppliesToPayload subject [parent] "expand" [added] [] [effective]))
        "Expand ownership after review.\n"
    )

domainsRecord :: IO ConnectionRecord
domainsRecord = do
  identifier <- connectionOrFail connection2
  subject <- adrOrFail adr0
  parentConnection <- connectionOrFail connection1
  parent <- assertRight (mkDomain "compiler")
  child <- assertRight (mkDomain "compiler.cache")
  refinement <- assertRight (parseDomainRefinement "compiler=compiler.cache")
  pure
    ( ConnectionRecord
        identifier
        (DomainsConnection (DomainsPayload subject [parentConnection] "refine" [child] [parent] [child] [refinement]))
        "Refine the architectural subject.\n"
    )

statusRecord :: IO ConnectionRecord
statusRecord = do
  identifier <- connectionOrFail connection3
  subject <- adrOrFail adr0
  parent <- connectionOrFail connection2
  headRecord <- recordOrFail record1
  replacement <- adrOrFail adr1
  pure
    ( ConnectionRecord
        identifier
        (StatusConnection (StatusPayload subject [parent] StatusObsolete [headRecord] (Just replacement)))
        "Retire the decision after replacement.\n"
    )

sealedConnection :: IO ConnectionRecord -> IO ByteString
sealedConnection buildRecord = do
  record <- buildRecord
  semantic <- assertRight (renderConnectionSemantic record)
  capsule <- capsuleFor (ManagedConnection record) semantic
  assertRight (sealManagedDocument (ManagedConnection record) capsule)

capsuleFor :: ManagedRecord -> Text -> IO ProvenanceCapsule
capsuleFor managed semantic = do
  operation <- assertRight (mkOperationId operation0)
  actor <- assertRight (mkActor HumanActor "architect" Nothing)
  basis <- assertRight (mkGitOid (Text.replicate 40 "0"))
  event <- assertRight (mkEventKind eventText)
  assertRight . mkProvenanceCapsule $
    ProvenanceCapsuleInput
      { capsuleInputOperationId = operation,
        capsuleInputObjectId = objectId,
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
  where
    (objectId, eventText) =
      case managed of
        ManagedDecision record -> (ProvenanceRecord (decisionRecord record), "decision.create")
        ManagedConnection record -> (ProvenanceConnection (connectionRecordId record), "connection.create")

parseChanged :: ByteString -> (Text -> Text) -> Either DocumentError ParsedManagedDocument
parseChanged sealed change =
  parseManagedDocument fixedPath (TextEncoding.encodeUtf8 (change (TextEncoding.decodeUtf8 sealed)))

sealWithJson :: Text -> ByteString
sealWithJson json =
  TextEncoding.encodeUtf8
    ( Text.dropWhileEnd (== '\n') expectedDecisionSemantic
        <> "\n\n<!-- @adrai:"
        <> encodeBase64Url (TextEncoding.encodeUtf8 json)
        <> " -->\n"
    )

fixedPath :: RepoPath
fixedPath = required (mkRepoPath "architecture/adrai/test.md")

decisionPath :: IO RepoPath
decisionPath = pathOrFail "architecture/adrai/decisions/R000/R00000000000000000000000000--stable-cache-identity.decision.md"

pathOrFail :: Text -> IO RepoPath
pathOrFail = assertRight . mkRepoPath

adrOrFail :: Text -> IO AdrId
adrOrFail = assertRight . mkAdrId

recordOrFail :: Text -> IO RecordId
recordOrFail = assertRight . mkRecordId

connectionOrFail :: Text -> IO ConnectionId
connectionOrFail = assertRight . mkConnectionId

assertRight :: (Show error) => Either error value -> IO value
assertRight result =
  case result of
    Left problem -> assertFailure (show problem) >> fail "unreachable"
    Right value -> pure value

assertLeft :: (Show right) => Either DocumentError right -> (DocumentError -> Bool) -> IO ()
assertLeft result predicate =
  case result of
    Left problem -> assertBool ("unexpected error: " <> show problem) (predicate problem)
    Right value -> assertFailure ("expected failure, got: " <> show value)

required :: (Show error) => Either error value -> value
required result =
  case result of
    Left problem -> error ("invalid static fixture: " <> show problem)
    Right value -> value

goldenCapsule :: ProvenanceCapsule
goldenCapsule = required (decodeCapsule goldenEncodedCapsule)

expectedDecisionSemantic :: Text
expectedDecisionSemantic =
  Text.unlines
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

expectedSealedDecision :: ByteString
expectedSealedDecision =
  TextEncoding.encodeUtf8
    ( expectedDecisionSemantic
        <> "\n<!-- @adrai:"
        <> goldenEncodedCapsule
        <> " -->\n"
    )

expectedAmends :: Text
expectedAmends =
  Text.unlines
    [ "+++",
      "schema = \"adrai/connection/v1\"",
      "connection = \"C00000000000000000000000000\"",
      "relation = \"amends\"",
      "from_record = \"R00000000000000000000000001\"",
      "subject_adr = \"A00000000000000000000000000\"",
      "to_records = [\"R00000000000000000000000000\"]",
      "+++",
      "",
      "Replace the previous effective record."
    ]

expectedAppliesTo :: Text
expectedAppliesTo =
  Text.unlines
    [ "+++",
      "schema = \"adrai/connection/v1\"",
      "connection = \"C00000000000000000000000001\"",
      "relation = \"applies_to\"",
      "added = [\"tests/compiler/cache/**\"]",
      "applies_to = [\"src/compiler/cache/**\"]",
      "change = \"expand\"",
      "parent_connections = [\"C00000000000000000000000000\"]",
      "removed = []",
      "subject_adr = \"A00000000000000000000000000\"",
      "+++",
      "",
      "Expand ownership after review."
    ]

expectedDomains :: Text
expectedDomains =
  Text.unlines
    [ "+++",
      "schema = \"adrai/connection/v1\"",
      "connection = \"C00000000000000000000000002\"",
      "relation = \"domains\"",
      "added = [\"compiler.cache\"]",
      "change = \"refine\"",
      "domains = [\"compiler.cache\"]",
      "parent_connections = [\"C00000000000000000000000001\"]",
      "refinements = [\"compiler=compiler.cache\"]",
      "removed = [\"compiler\"]",
      "subject_adr = \"A00000000000000000000000000\"",
      "+++",
      "",
      "Refine the architectural subject."
    ]

expectedStatus :: Text
expectedStatus =
  Text.unlines
    [ "+++",
      "schema = \"adrai/connection/v1\"",
      "connection = \"C00000000000000000000000003\"",
      "relation = \"status\"",
      "parent_connections = [\"C00000000000000000000000002\"]",
      "record_heads = [\"R00000000000000000000000001\"]",
      "replacement_adr = \"A00000000000000000000000001\"",
      "state = \"obsolete\"",
      "subject_adr = \"A00000000000000000000000000\"",
      "+++",
      "",
      "Retire the decision after replacement."
    ]

goldenCapsuleJson :: Text
goldenCapsuleJson =
  "{\"a\":{\"i\":\"architect\",\"k\":\"human\"},\"b\":\"0000000000000000000000000000000000000000\",\"k\":\"decision.create\",\"o\":\"R00000000000000000000000000\",\"op\":\"O00000000000000000000000000\",\"s\":\"sha256:9qnNalcefUIDnN7WAhrCbt4EmSAH8wpepZOfgwh0uYY\",\"t\":1700000000000,\"v\":1,\"x\":\"adrai/1.0.0\"}"

goldenEncodedCapsule :: Text
goldenEncodedCapsule =
  "eyJhIjp7ImkiOiJhcmNoaXRlY3QiLCJrIjoiaHVtYW4ifSwiYiI6IjAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAiLCJrIjoiZGVjaXNpb24uY3JlYXRlIiwibyI6IlIwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMCIsIm9wIjoiTzAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwIiwicyI6InNoYTI1Njo5cW5OYWxjZWZVSURuTjdXQWhyQ2J0NEVtU0FIOHdwZXBaT2Znd2gwdVlZIiwidCI6MTcwMDAwMDAwMDAwMCwidiI6MSwieCI6ImFkcmFpLzEuMC4wIn0"

adr0, adr1, record0, record1, connection0, connection1, connection2, connection3, operation0 :: Text
adr0 = "A00000000000000000000000000"
adr1 = "A00000000000000000000000001"
record0 = "R00000000000000000000000000"
record1 = "R00000000000000000000000001"
connection0 = "C00000000000000000000000000"
connection1 = "C00000000000000000000000001"
connection2 = "C00000000000000000000000002"
connection3 = "C00000000000000000000000003"
operation0 = "O00000000000000000000000000"
