{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Adrai.Property.Generators
  ( IdentifierPool (..),
    genIdentifierPool,
    adrIdAt,
    recordIdAt,
    connectionIdAt,
    operationIdAt,
    DomainSpec (..),
    genDomainSpec,
    domainFromSpec,
    ScopeSpec (..),
    genScopeSpec,
    scopeFromSpec,
    genConfig,
    DocumentKind (..),
    ValidDocumentSpec (..),
    SealedDocument (..),
    genValidDocumentSpec,
    materializeValidDocument,
    sealValidDocument,
    MalformedMutation (..),
    genMalformedMutation,
    AxisConflictSpec (..),
    AxisFixture (..),
    genAxisConflictSpec,
    materializeAxisConflict,
    InvalidGraphFault (..),
    InvalidGraphSpec (..),
    InvalidGraphFixture (..),
    genInvalidGraphSpec,
    materializeInvalidGraph,
    ProjectionDagSpec (..),
    ProjectionFixture (..),
    genProjectionDagSpec,
    materializeProjectionDag,
    parsedDocument,
    managedObjectId,
    sampledPermutation,
  )
where

import Adrai.Domain
  ( Domain,
    mkDomain,
  )
import Adrai.Format.Document
import Adrai.Graph (GraphAxis (..), GraphIssueCode (..), reduceManagedGraph)
import Adrai.History (ReadSnapshot (..), RevisionIdentity (..))
import Adrai.Provenance
import Adrai.Scope (ScopePattern, mkScopePattern, scopePatternText)
import Adrai.Service
import Adrai.Types
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- | A small ordinal namespace. References are always derived from the same
-- shrinking value, so a smaller fixture cannot acquire dangling IDs by accident.
newtype IdentifierPool = IdentifierPool {identifierPoolOffset :: Int}
  deriving (Eq, Show)

genIdentifierPool :: Gen IdentifierPool
genIdentifierPool = IdentifierPool <$> Gen.int (Range.linear 0 1)

adrIdAt :: IdentifierPool -> Int -> AdrId
adrIdAt pool = typedId mkAdrId 'A' pool

recordIdAt :: IdentifierPool -> Int -> RecordId
recordIdAt pool = typedId mkRecordId 'R' pool

connectionIdAt :: IdentifierPool -> Int -> ConnectionId
connectionIdAt pool = typedId mkConnectionId 'C' pool

operationIdAt :: IdentifierPool -> Int -> OperationId
operationIdAt pool = typedId mkOperationId 'O' pool

typedId :: (Text -> Either error value) -> Char -> IdentifierPool -> Int -> value
typedId constructor prefix pool ordinal =
  expect "generated identifier" . constructor $
    Text.singleton prefix <> Text.replicate 25 "0" <> Text.singleton final
  where
    alphabet = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"
    index = identifierPoolOffset pool * 14 + ordinal
    final
      | index >= 0 && index < Text.length alphabet = Text.index alphabet index
      | otherwise = error "P2-06 identifier pool exhausted"

newtype DomainSpec = DomainSpec {domainSpecSegments :: [Text]}
  deriving (Eq, Show)

genDomainSpec :: Gen DomainSpec
genDomainSpec = DomainSpec <$> Gen.list (Range.linear 1 3) genSafeSegment

domainFromSpec :: DomainSpec -> Domain
domainFromSpec = expect "generated domain" . mkDomain . Text.intercalate "." . domainSpecSegments

data ScopeSpec = ScopeSpec
  { scopeSpecSegments :: [Text],
    scopeSpecRecursive :: Bool
  }
  deriving (Eq, Show)

genScopeSpec :: Gen ScopeSpec
genScopeSpec =
  ScopeSpec
    <$> Gen.list (Range.linear 1 3) genSafeSegment
    <*> Gen.bool

scopeFromSpec :: ScopeSpec -> ScopePattern
scopeFromSpec spec =
  expect "generated scope" . mkScopePattern $
    Text.intercalate "/" (scopeSpecSegments spec)
      <> if scopeSpecRecursive spec then "/**" else ""

genConfig :: Gen Config
genConfig = do
  pool <- genIdentifierPool
  stem <- genSafeSegment
  lineSuffix <- genSafeSegment
  let base = "architecture/area-" <> stem
      decisions = expect "decision path" (mkRepoPath (base <> "/decisions"))
      connections = expect "connection path" (mkRepoPath (base <> "/connections"))
      paths = expect "managed paths" (mkManagedPaths decisions connections)
      reference = expect "logical line ref" (mkGitRef ("refs/heads/line-" <> lineSuffix))
      line = expect "logical line" (mkLogicalLine ("line-" <> lineSuffix <> ordinalText pool) [reference])
  pure (expect "config" (mkConfig ConfigSchemaV1 paths [line]))

ordinalText :: IdentifierPool -> Text
ordinalText = Text.pack . show . identifierPoolOffset

genSafeSegment :: Gen Text
genSafeSegment = do
  first <- Gen.element ['a' .. 'z']
  rest <- Gen.list (Range.linear 0 5) (Gen.element (['a' .. 'z'] <> ['0' .. '9']))
  pure (Text.pack (first : rest))

data DocumentKind
  = DecisionDocument
  | AmendmentDocument
  | ScopeDocument
  | DomainDocument
  | StatusDocument
  deriving (Eq, Show, Enum, Bounded)

data ValidDocumentSpec = ValidDocumentSpec
  { validDocumentPool :: IdentifierPool,
    validDocumentKind :: DocumentKind,
    validDocumentDomain :: DomainSpec,
    validDocumentScope :: ScopeSpec,
    validDocumentText :: Text
  }
  deriving (Eq, Show)

data SealedDocument = SealedDocument
  { sealedDocumentRecord :: ManagedRecord,
    sealedDocumentCapsule :: ProvenanceCapsule,
    sealedDocumentPath :: RepoPath,
    sealedDocumentSemantic :: Text,
    sealedDocumentBytes :: ByteString
  }
  deriving (Eq, Show)

genValidDocumentSpec :: Gen ValidDocumentSpec
genValidDocumentSpec =
  ValidDocumentSpec
    <$> genIdentifierPool
    <*> Gen.enumBounded
    <*> genDomainSpec
    <*> genScopeSpec
    <*> Gen.element
      [ "canonical architecture",
        "caf\233 cache",
        "M\248del \937",
        "emoji \128512",
        "\26085\26412\35486 design"
      ]

materializeValidDocument :: ValidDocumentSpec -> ManagedRecord
materializeValidDocument spec =
  case validDocumentKind spec of
    DecisionDocument -> ManagedDecision decision
    AmendmentDocument -> connection 0 (AmendsConnection (AmendsPayload adr (recordIdAt pool 1) [recordIdAt pool 0]))
    ScopeDocument ->
      let value = scopeFromSpec (validDocumentScope spec)
       in connection 1 (AppliesToConnection (AppliesToPayload adr [] "initial" [value] [] [value]))
    DomainDocument ->
      let value = domainFromSpec (validDocumentDomain spec)
       in connection 2 (DomainsConnection (DomainsPayload adr [] "initial" [value] [] [value] []))
    StatusDocument -> connection 3 (StatusConnection (StatusPayload adr [] StatusActive [recordIdAt pool 0] Nothing))
  where
    pool = validDocumentPool spec
    adr = adrIdAt pool 0
    text = validDocumentText spec
    decision = DecisionRecord adr (recordIdAt pool 0) text (text <> " summary") [domainFromSpec (validDocumentDomain spec)] ("# Decision\n" <> text <> "\n")
    connection ordinal payload = ManagedConnection (ConnectionRecord (connectionIdAt pool ordinal) payload (text <> " rationale\n"))

sealValidDocument :: ValidDocumentSpec -> SealedDocument
sealValidDocument spec =
  SealedDocument record capsule path semantic bytes
  where
    record = materializeValidDocument spec
    semantic = expect "managed semantic" (renderManagedSemantic record)
    capsule = capsuleFor (validDocumentPool spec) 0 1700000000000 [] record
    path = expect "canonical managed path" (canonicalManagedPath (configManagedPaths defaultConfig) record)
    bytes = expect "sealed managed document" (sealManagedDocument record capsule)

data MalformedMutation
  = MutationUnknownConfigRoot
  | MutationUnknownConfigPath
  | MutationUnknownConfigLine
  | MutationUnsupportedManagedSchema
  | MutationUnknownDecisionField
  | MutationUnknownConnectionField
  | MutationUnknownRelation
  | MutationInvalidManagedUtf8
  | MutationUnknownCapsuleField
  | MutationUnsupportedCapsuleVersion
  | MutationInvalidCapsuleTool
  | MutationUnknownCapsuleActorField
  deriving (Eq, Show, Enum, Bounded)

genMalformedMutation :: Gen MalformedMutation
genMalformedMutation = Gen.enumBounded

data AxisConflictSpec = AxisConflictSpec
  { axisConflictPool :: IdentifierPool,
    axisConflictAxis :: GraphAxis,
    axisConflictFlavor :: Int
  }
  deriving (Eq, Show)

data AxisFixture = AxisFixture
  { axisFixtureSpec :: AxisConflictSpec,
    axisFixtureSubject :: AdrId,
    axisFixtureBaseline :: [ManagedRecord],
    axisFixtureLeftAppend :: [ManagedRecord],
    axisFixtureRightAppend :: [ManagedRecord],
    axisFixtureRecords :: [ManagedRecord],
    axisFixtureChoice :: ReconciliationChoice
  }
  deriving (Eq, Show)

genAxisConflictSpec :: Maybe GraphAxis -> Gen AxisConflictSpec
genAxisConflictSpec requested =
  AxisConflictSpec
    <$> genIdentifierPool
    <*> maybe Gen.enumBounded pure requested
    <*> Gen.int (Range.linear 0 3)

materializeAxisConflict :: AxisConflictSpec -> AxisFixture
materializeAxisConflict spec =
  AxisFixture spec adr baseline left right (baseline <> left <> right) choice
  where
    pool = axisConflictPool spec
    adr = adrIdAt pool 0
    flavor = axisConflictFlavor spec
    rootName = ["compiler", "runtime", "storage", "search"] !! flavor
    rootDomain = domainTextValue rootName
    leftDomain = domainTextValue (rootName <> ".cache")
    rightDomain = domainTextValue (rootName <> ".search")
    rootScope = scopeTextValue (["src/**", "lib/**", "app/**", "core/**"] !! flavor)
    leftScope = scopeTextValue (["test/**", "spec/**", "checks/**", "verify/**"] !! flavor)
    rightScope = scopeTextValue (["docs/**", "notes/**", "guide/**", "manual/**"] !! flavor)
    d0 = recordIdAt pool 0
    d1 = recordIdAt pool 1
    d2 = recordIdAt pool 2
    d3 = recordIdAt pool 3
    decision identifier label = ManagedDecision (DecisionRecord adr identifier label (label <> " summary") [] ("# Decision\n" <> label <> "\n"))
    connect ordinal payload = ManagedConnection (ConnectionRecord (connectionIdAt pool ordinal) payload "property fixture\n")
    initialDecision = decision d0 "base"
    initialScope ordinal = connect ordinal (AppliesToConnection (AppliesToPayload adr [] "initial" [rootScope] [] [rootScope]))
    initialDomain ordinal = connect ordinal (DomainsConnection (DomainsPayload adr [] "initial" [rootDomain] [] [rootDomain] []))
    initialStatus ordinal = connect ordinal (StatusConnection (StatusPayload adr [] StatusActive [d0] Nothing))
    resolvedDecision = DecisionRecord adr d3 "resolved" "resolved summary" [] "# Decision\nresolved\n"
    (baseline, left, right, choice) =
      case axisConflictAxis spec of
        DecisionAxis ->
          ( [initialDecision, initialScope 2, initialDomain 3, initialStatus 4],
            [decision d1 "left", connect 0 (AmendsConnection (AmendsPayload adr d1 [d0]))],
            [decision d2 "right", connect 1 (AmendsConnection (AmendsPayload adr d2 [d0]))],
            ReconcileDecision (DecisionResolution adr resolvedDecision (connectionIdAt pool 5) "resolve decision\n")
          )
        ScopeAxis ->
          ( [initialDecision, initialScope 0, initialDomain 3, initialStatus 4],
            [connect 1 (AppliesToConnection (AppliesToPayload adr [connectionIdAt pool 0] "expand" [leftScope] [] [rootScope, leftScope]))],
            [connect 2 (AppliesToConnection (AppliesToPayload adr [connectionIdAt pool 0] "expand" [rightScope] [] [rootScope, rightScope]))],
            ReconcileScope (ScopeResolution adr (connectionIdAt pool 5) [rootScope, leftScope, rightScope] "resolve scope\n")
          )
        DomainAxis ->
          ( [initialDecision, initialScope 0, initialDomain 1, initialStatus 4],
            [connect 2 (DomainsConnection (DomainsPayload adr [connectionIdAt pool 1] "replace" [leftDomain] [rootDomain] [leftDomain] []))],
            [connect 3 (DomainsConnection (DomainsPayload adr [connectionIdAt pool 1] "replace" [rightDomain] [rootDomain] [rightDomain] []))],
            ReconcileDomain (DomainResolution adr (connectionIdAt pool 5) [leftDomain, rightDomain] "resolve domains\n")
          )
        StatusAxis ->
          ( [initialDecision, initialScope 0, initialDomain 1, initialStatus 2],
            [connect 3 (StatusConnection (StatusPayload adr [connectionIdAt pool 2] StatusActive [d0] Nothing))],
            [connect 4 (StatusConnection (StatusPayload adr [connectionIdAt pool 2] StatusActive [d0] Nothing))],
            ReconcileStatus (StatusResolution adr (connectionIdAt pool 5) StatusActive Nothing "resolve status\n" True)
          )

data InvalidGraphFault
  = DanglingDecisionParent
  | CyclicScopeParents
  | InvalidDomainDelta
  | CrossAdrStatusParent
  deriving (Eq, Show, Enum, Bounded)

data InvalidGraphSpec = InvalidGraphSpec
  { invalidGraphPool :: IdentifierPool,
    invalidGraphFault :: InvalidGraphFault
  }
  deriving (Eq, Show)

data InvalidGraphFixture = InvalidGraphFixture
  { invalidFixtureSpec :: InvalidGraphSpec,
    invalidFixtureAxis :: GraphAxis,
    invalidFixtureRecords :: [ManagedRecord],
    invalidFixtureExpectedCode :: GraphIssueCode,
    invalidFixturePreservedHead :: Text,
    invalidFixtureFaultLabel :: Text
  }
  deriving (Eq, Show)

genInvalidGraphSpec :: Gen InvalidGraphSpec
genInvalidGraphSpec = InvalidGraphSpec <$> genIdentifierPool <*> Gen.enumBounded

materializeInvalidGraph :: InvalidGraphSpec -> InvalidGraphFixture
materializeInvalidGraph spec =
  case invalidGraphFault spec of
    DanglingDecisionParent ->
      InvalidGraphFixture spec DecisionAxis
        (base <> [decision d1 "orphan", connect 4 (AmendsConnection (AmendsPayload adr d1 [recordIdAt pool 9]))])
        AmendmentMissingParent (recordIdText d0) "dangling-parent"
    CyclicScopeParents ->
      InvalidGraphFixture spec ScopeAxis
        (base <> [scopeRevision 4 [connectionIdAt pool 5] "merge" [] [] [rootScope], scopeRevision 5 [connectionIdAt pool 4] "merge" [] [] [rootScope]])
        ScopeCycle (connectionIdText (connectionIdAt pool 0)) "cycle"
    InvalidDomainDelta ->
      InvalidGraphFixture spec DomainAxis
        (base <> [connect 4 (DomainsConnection (DomainsPayload adr [connectionIdAt pool 1] "expand" [childDomain] [] [rootDomain] []))])
        DomainDeltaMismatch (connectionIdText (connectionIdAt pool 1)) "invalid-delta"
    CrossAdrStatusParent ->
      InvalidGraphFixture spec StatusAxis
        (base <> otherAdr <> [connect 6 (StatusConnection (StatusPayload adr [connectionIdAt pool 9] StatusActive [d0] Nothing))])
        InvalidStatusParents (connectionIdText (connectionIdAt pool 2)) "cross-adr-parent"
  where
    pool = invalidGraphPool spec
    adr = adrIdAt pool 0
    other = adrIdAt pool 1
    d0 = recordIdAt pool 0
    d1 = recordIdAt pool 1
    otherRecord = recordIdAt pool 8
    rootDomain = domainTextValue "compiler"
    childDomain = domainTextValue "compiler.cache"
    rootScope = scopeTextValue "src/**"
    decision identifier label = ManagedDecision (DecisionRecord adr identifier label (label <> " summary") [] (label <> "\n"))
    connect ordinal payload = ManagedConnection (ConnectionRecord (connectionIdAt pool ordinal) payload "invalid fixture\n")
    scopeRevision ordinal parents change added removed effective = connect ordinal (AppliesToConnection (AppliesToPayload adr parents change added removed effective))
    base =
      [ decision d0 "base",
        scopeRevision 0 [] "initial" [rootScope] [] [rootScope],
        connect 1 (DomainsConnection (DomainsPayload adr [] "initial" [rootDomain] [] [rootDomain] [])),
        connect 2 (StatusConnection (StatusPayload adr [] StatusActive [d0] Nothing))
      ]
    otherAdr =
      [ ManagedDecision (DecisionRecord other otherRecord "other" "other summary" [] "other\n"),
        ManagedConnection (ConnectionRecord (connectionIdAt pool 7) (AppliesToConnection (AppliesToPayload other [] "initial" [] [] [])) "other\n"),
        ManagedConnection (ConnectionRecord (connectionIdAt pool 8) (DomainsConnection (DomainsPayload other [] "initial" [] [] [] [])) "other\n"),
        ManagedConnection (ConnectionRecord (connectionIdAt pool 9) (StatusConnection (StatusPayload other [] StatusActive [otherRecord] Nothing)) "other\n")
      ]

data ProjectionDagSpec = ProjectionDagSpec
  { projectionDagPool :: IdentifierPool,
    projectionDagDomain :: DomainSpec,
    projectionDagScope :: ScopeSpec,
    projectionDagText :: Text
  }
  deriving (Eq, Show)

data ProjectionFixture = ProjectionFixture
  { projectionFixturePrimaryAdr :: AdrId,
    projectionFixturePrimaryRecord :: RecordId,
    projectionFixturePrimaryConnection :: ConnectionId,
    projectionFixtureBefore :: ReadSnapshot,
    projectionFixtureAfter :: ReadSnapshot,
    projectionFixtureOperationOrder :: [OperationId]
  }
  deriving (Eq, Show)

genProjectionDagSpec :: Gen ProjectionDagSpec
genProjectionDagSpec =
  ProjectionDagSpec
    <$> genIdentifierPool
    <*> genDomainSpec
    <*> genScopeSpec
    <*> Gen.element ["caf\233 architecture", "M\248del \937", "emoji \128512", "\26085\26412\35486 design"]

materializeProjectionDag :: ProjectionDagSpec -> ProjectionFixture
materializeProjectionDag spec =
  ProjectionFixture primary r0 scope0 beforeSnapshot afterSnapshot [op0, op1, op2, op3]
  where
    pool = projectionDagPool spec
    primary = adrIdAt pool 0
    secondary = adrIdAt pool 1
    r0 = recordIdAt pool 0
    r1 = recordIdAt pool 1
    r8 = recordIdAt pool 8
    scope0 = connectionIdAt pool 0
    domain0 = connectionIdAt pool 1
    status0 = connectionIdAt pool 2
    amend1 = connectionIdAt pool 3
    scope1 = connectionIdAt pool 4
    status1 = connectionIdAt pool 5
    secondaryScope = connectionIdAt pool 7
    secondaryDomain = connectionIdAt pool 8
    secondaryStatus = connectionIdAt pool 9
    op0 = operationIdAt pool 0
    op1 = operationIdAt pool 1
    op2 = operationIdAt pool 2
    op3 = operationIdAt pool 3
    domainValue = domainFromSpec (projectionDagDomain spec)
    scopeValue = scopeFromSpec (projectionDagScope spec)
    extraScope = scopeTextValue (scopePatternText scopeValue <> "/generated")
    text = projectionDagText spec
    createDecision subject record title = ManagedDecision (DecisionRecord subject record title (title <> " summary") [domainValue] ("# Decision\n" <> title <> "\n"))
    connect identifier payload = ManagedConnection (ConnectionRecord identifier payload "projection fixture\n")
    initialRecords =
      [ createDecision primary r0 text,
        connect scope0 (AppliesToConnection (AppliesToPayload primary [] "initial" [scopeValue] [] [scopeValue])),
        connect domain0 (DomainsConnection (DomainsPayload primary [] "initial" [domainValue] [] [domainValue] [])),
        connect status0 (StatusConnection (StatusPayload primary [] StatusActive [r0] Nothing))
      ]
    amendmentRecords =
      [ createDecision primary r1 (text <> " v2"),
        connect amend1 (AmendsConnection (AmendsPayload primary r1 [r0]))
      ]
    scopeRecords =
      [connect scope1 (AppliesToConnection (AppliesToPayload primary [scope0] "expand" [extraScope] [] [scopeValue, extraScope]))]
    statusRecords =
      [connect status1 (StatusConnection (StatusPayload primary [status0] StatusObsolete [r1] Nothing))]
    secondaryRecords =
      [ createDecision secondary r8 "secondary",
        connect secondaryScope (AppliesToConnection (AppliesToPayload secondary [] "initial" [] [] [])),
        connect secondaryDomain (DomainsConnection (DomainsPayload secondary [] "initial" [] [] [] [])),
        connect secondaryStatus (StatusConnection (StatusPayload secondary [] StatusActive [r8] Nothing))
      ]
    beforeDocuments = map (parsedDocument pool 0 300 []) initialRecords
    afterDocuments =
      beforeDocuments
        <> map (parsedDocument pool 1 100 [ProvenanceRecord r0]) amendmentRecords
        <> map (parsedDocument pool 2 100 [ProvenanceConnection scope0]) scopeRecords
        <> map (parsedDocument pool 3 200 [ProvenanceRecord r1, ProvenanceConnection status0]) statusRecords
        <> map (parsedDocument pool 4 50 []) secondaryRecords
    beforeSnapshot = ReadSnapshot (RevisionIdentity "before" "before-resolved") beforeDocuments (reduceManagedGraph initialRecords) Map.empty
    afterRecords = initialRecords <> amendmentRecords <> scopeRecords <> statusRecords <> secondaryRecords
    afterSnapshot = ReadSnapshot (RevisionIdentity "after" "after-resolved") afterDocuments (reduceManagedGraph afterRecords) Map.empty

parsedDocument :: IdentifierPool -> Int -> Integer -> [ProvenanceObjectId] -> ManagedRecord -> ParsedManagedDocument
parsedDocument pool operationOrdinal timestamp parents record =
  ParsedManagedDocument path record capsule semantic bytes
  where
    semantic = expect "projection semantic" (renderManagedSemantic record)
    capsule = capsuleFor pool operationOrdinal timestamp parents record
    path = expect "projection path" (canonicalManagedPath (configManagedPaths defaultConfig) record)
    bytes = expect "projection sealed bytes" (sealManagedDocument record capsule)

capsuleFor :: IdentifierPool -> Int -> Integer -> [ProvenanceObjectId] -> ManagedRecord -> ProvenanceCapsule
capsuleFor pool operationOrdinal timestamp parents record =
  expect "provenance capsule" $
    mkProvenanceCapsule
      ProvenanceCapsuleInput
        { capsuleInputOperationId = operationIdAt pool operationOrdinal,
          capsuleInputObjectId = managedObjectId record,
          capsuleInputEventKind = expect "event kind" (mkEventKind (eventFor parents record)),
          capsuleInputActor = expect "actor" (mkActor HumanActor "p2-06" Nothing),
          capsuleInputTimestampMs = timestamp,
          capsuleInputBasis = expect "basis" (mkGitOid (Text.replicate 40 "a")),
          capsuleInputParents = parents,
          capsuleInputBranchHint = Nothing,
          capsuleInputUpstreamHint = Nothing,
          capsuleInputLineAnchors = [],
          capsuleInputSemanticDigest = semanticDigest (expect "capsule semantic" (renderManagedSemantic record)),
          capsuleInputToolVersion = "adrai/1.0.0",
          capsuleInputDigests = ProvenanceInputs Nothing Nothing Nothing
        }

managedObjectId :: ManagedRecord -> ProvenanceObjectId
managedObjectId managed =
  case managed of
    ManagedDecision record -> ProvenanceRecord (decisionRecord record)
    ManagedConnection record -> ProvenanceConnection (connectionRecordId record)

eventFor :: [ProvenanceObjectId] -> ManagedRecord -> Text
eventFor parents managed =
  case managed of
    ManagedDecision _
      | null parents -> "decision.create"
      | otherwise -> "decision.amend"
    ManagedConnection record ->
      case connectionPayload record of
        AmendsConnection _ -> "decision.amend"
        AppliesToConnection payload -> "scope." <> appliesToChange payload
        DomainsConnection payload -> "domain." <> domainsChange payload
        StatusConnection payload
          | null (statusParentConnections payload) -> "status.initial"
          | statusState payload == StatusObsolete -> "decision.obsolete"
          | otherwise -> "decision.reactivate"

domainTextValue :: Text -> Domain
domainTextValue = expect "fixture domain" . mkDomain

scopeTextValue :: Text -> ScopePattern
scopeTextValue = expect "fixture scope" . mkScopePattern

sampledPermutation :: [value] -> Gen [value]
sampledPermutation = Gen.shuffle

expect :: String -> Either error value -> value
expect label result =
  case result of
    Right value -> value
    Left _ -> error ("invalid P2-06 " <> label)
