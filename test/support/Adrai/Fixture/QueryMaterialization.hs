{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Adrai.Fixture.QueryMaterialization
  ( QueryMaterialization (..),
    QueryMaterializationError (..),
    materializeQueryFixture,
    materializeQueryFixtureAt,
    lookupQueryMaterializationAdr,
    lookupQueryMaterializationSource,
  )
where

import Adrai.Compiler
  ( SearchMaterializationError,
    materializeCurrentSearch,
  )
import Adrai.Domain (Domain, DomainError, canonicalDomains)
import Adrai.Fixture.Prng (seedWord64)
import Adrai.Fixture.Types
  ( AdrKey (..),
    AdrTemplate (..),
    FixtureMeta (..),
    FixtureVersion (..),
    SourceMode (..),
    SourceTemplate (..),
  )
import Adrai.Format (renderDigest)
import Adrai.Format.Document
  ( AppliesToPayload (..),
    ConnectionPayload (..),
    ConnectionRecord (..),
    DecisionRecord (..),
    DocumentError (..),
    DomainsPayload (..),
    ManagedRecord (..),
    ParsedManagedDocument (..),
    StatusPayload (..),
    StatusState (StatusActive),
    canonicalManagedPath,
    parseManagedDocument,
    renderManagedSemantic,
    sealManagedDocument,
    validateManagedLocation,
  )
import Adrai.Graph
  ( GraphIssue,
    GraphReduction (..),
    ReducedAdr (..),
    lookupReducedAdr,
    reduceManagedGraph,
  )
import Adrai.History
  ( ReadSnapshot (..),
    RevisionIdentity (..),
    SnapshotConsistencyError,
    validateReadSnapshot,
  )
import Adrai.Identity
  ( IdentityError,
    adrIdFromBytes,
    connectionIdFromBytes,
    operationIdFromBytes,
    recordIdFromBytes,
  )
import Adrai.Provenance
  ( GitOid,
    ProvenanceCapsuleInput (..),
    ProvenanceError,
    ProvenanceObjectId (..),
    mkEventKind,
    mkGitOid,
    mkProvenanceCapsule,
    semanticDigest,
    sha256Digest,
  )
import Adrai.Query (RelevantSource (..))
import Adrai.Retrieval (SearchMaterialization)
import Adrai.Scope (ScopePattern, ScopePatternError, mkScopePattern)
import Adrai.Types
  ( Actor,
    ActorKind (ServiceActor),
    ActorViolation,
    AdrId,
    ConnectionId,
    OperationId,
    ProvenanceInputs (..),
    RecordId,
    RepoPathViolation,
    configManagedPaths,
    defaultConfig,
    digestBytes,
    mkActor,
    mkRepoPath,
    repoPathText,
  )
import qualified Data.ByteString as ByteString
import Data.Bits (shiftR)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word64)

data QueryMaterialization = QueryMaterialization
  { queryMaterializationSnapshot :: ReadSnapshot,
    queryMaterializationSearch :: SearchMaterialization,
    queryMaterializationAdrIds :: Map AdrKey AdrId,
    queryMaterializationSources :: Map Text RelevantSource
  }
  deriving (Eq, Show)

data QueryMaterializationError
  = QueryMaterializationDuplicateAdrKey AdrKey
  | QueryMaterializationDuplicateSourceKey Text
  | QueryMaterializationInvalidAdrKey AdrKey
  | QueryMaterializationIdentityFailure AdrKey Text IdentityError
  | QueryMaterializationActorFailure ActorViolation
  | QueryMaterializationCapsuleFailure AdrKey Text ProvenanceError
  | QueryMaterializationInvalidRecord AdrKey Text DocumentError
  | QueryMaterializationInvalidDomain AdrKey Text DomainError
  | QueryMaterializationInvalidScope AdrKey Text ScopePatternError
  | QueryMaterializationInvalidSourcePath Text Text RepoPathViolation
  | QueryMaterializationGraphFailure [GraphIssue]
  | QueryMaterializationMissingReducedAdr AdrKey
  | QueryMaterializationConflictedAdr AdrKey
  | QueryMaterializationSnapshotFailure SnapshotConsistencyError
  | QueryMaterializationCompilerFailure SearchMaterializationError
  | QueryMaterializationUnsupportedSourceModeRevision Text SourceMode Text
  | QueryMaterializationMissingAdr AdrKey
  | QueryMaterializationMissingSource Text
  deriving (Eq, Show)

materializeQueryFixture :: FixtureMeta -> NonEmpty AdrTemplate -> [SourceTemplate] -> Either QueryMaterializationError QueryMaterialization
materializeQueryFixture = materializeQueryFixtureAt "HEAD"

materializeQueryFixtureAt :: Text -> FixtureMeta -> NonEmpty AdrTemplate -> [SourceTemplate] -> Either QueryMaterializationError QueryMaterialization
materializeQueryFixtureAt requestedRevision meta templates sourceTemplates = do
  ensureUniqueAdrKeys orderedTemplates
  ensureUniqueSourceKeys orderedSources
  actor <- mapLeft QueryMaterializationActorFailure (mkActor ServiceActor "adrai-query-fixture" Nothing)
  basis <- mapLeft (QueryMaterializationCapsuleFailure (AdrKey 0) "basis") (mkGitOid (Text.replicate 40 "a"))
  materializedAdrs <- traverse (materializeAdr meta actor basis) orderedTemplates
  let documents = sortOn (repoPathText . parsedManagedPath) (concatMap materializedDocuments materializedAdrs)
      records = map parsedManagedRecord documents
      reduction = reduceManagedGraph records
  if null (graphReductionIssues reduction)
    then Right ()
    else Left (QueryMaterializationGraphFailure (graphReductionIssues reduction))
  traverse_ (validateReducedAdr reduction) materializedAdrs
  let revision = RevisionIdentity requestedRevision (snapshotResolvedRevision documents)
      snapshot = ReadSnapshot revision documents reduction Map.empty
      adrIds = Map.fromList [(materializedKey item, materializedAdrId item) | item <- materializedAdrs]
  mapLeft QueryMaterializationSnapshotFailure (validateReadSnapshot snapshot)
  search <- mapLeft QueryMaterializationCompilerFailure (materializeCurrentSearch snapshot)
  sources <- Map.fromList <$> traverse (materializeSource meta revision) orderedSources
  Right
    QueryMaterialization
      { queryMaterializationSnapshot = snapshot,
        queryMaterializationSearch = search,
        queryMaterializationAdrIds = adrIds,
        queryMaterializationSources = sources
      }
  where
    orderedTemplates = sortOn adrTemplateKey (NonEmpty.toList templates)
    orderedSources = sortOn sourceTemplateKey sourceTemplates

lookupQueryMaterializationAdr :: AdrKey -> QueryMaterialization -> Either QueryMaterializationError AdrId
lookupQueryMaterializationAdr key materialization =
  maybe
    (Left (QueryMaterializationMissingAdr key))
    Right
    (Map.lookup key (queryMaterializationAdrIds materialization))

lookupQueryMaterializationSource :: Text -> QueryMaterialization -> Either QueryMaterializationError RelevantSource
lookupQueryMaterializationSource key materialization =
  maybe
    (Left (QueryMaterializationMissingSource key))
    Right
    (Map.lookup key (queryMaterializationSources materialization))

data MaterializedAdr = MaterializedAdr
  { materializedKey :: AdrKey,
    materializedAdrId :: AdrId,
    materializedDocuments :: [ParsedManagedDocument]
  }

materializeAdr :: FixtureMeta -> Actor -> GitOid -> AdrTemplate -> Either QueryMaterializationError MaterializedAdr
materializeAdr meta actor basis template = do
  _ <- ordinalSlot key 1 0
  adr <- generatedAdrId meta key
  record <- generatedRecordId meta key "decision"
  scopeConnection <- generatedConnectionId meta key "scope"
  domainConnection <- generatedConnectionId meta key "domain"
  statusConnection <- generatedConnectionId meta key "status"
  operation <- generatedOperationId meta key
  domains <- canonicalTemplateDomains key (adrTemplateDomains template)
  scopes <- canonicalTemplateScopes key (adrTemplateScopes template)
  body <- templateBody key template
  let managedRecords =
        [ ManagedDecision
            DecisionRecord
              { decisionAdr = adr,
                decisionRecord = record,
                decisionTitle = adrTemplateTitle template,
                decisionSummary = adrTemplateSummary template,
                decisionDomains = domains,
                decisionBody = body
              },
          ManagedConnection
            ConnectionRecord
              { connectionRecordId = scopeConnection,
                connectionPayload = AppliesToConnection (AppliesToPayload adr [] "initial" scopes [] scopes),
                connectionRationale = "Initial fixture scope.\n"
              },
          ManagedConnection
            ConnectionRecord
              { connectionRecordId = domainConnection,
                connectionPayload = DomainsConnection (DomainsPayload adr [] "initial" domains [] domains []),
                connectionRationale = "Initial fixture domains.\n"
              },
          ManagedConnection
            ConnectionRecord
              { connectionRecordId = statusConnection,
                connectionPayload = StatusConnection (StatusPayload adr [] StatusActive [record] Nothing),
                connectionRationale = "Initial fixture status.\n"
              }
        ]
  documents <- traverse (materializeRecord key actor basis operation) managedRecords
  Right (MaterializedAdr key adr documents)
  where
    key = adrTemplateKey template

materializeRecord :: AdrKey -> Actor -> GitOid -> OperationId -> ManagedRecord -> Either QueryMaterializationError ParsedManagedDocument
materializeRecord key actor basis operation managed = do
  semantic <- mapLeft (QueryMaterializationInvalidRecord key "semantic") (renderManagedSemantic managed)
  event <- mapLeft (QueryMaterializationCapsuleFailure key "event") (mkEventKind (recordEvent managed))
  capsule <-
    mapLeft (QueryMaterializationCapsuleFailure key "capsule") . mkProvenanceCapsule $
      ProvenanceCapsuleInput
        { capsuleInputOperationId = operation,
          capsuleInputObjectId = managedObject managed,
          capsuleInputEventKind = event,
          capsuleInputActor = actor,
          capsuleInputTimestampMs = timestampFor key,
          capsuleInputBasis = basis,
          capsuleInputParents = [],
          capsuleInputBranchHint = Nothing,
          capsuleInputUpstreamHint = Nothing,
          capsuleInputLineAnchors = [],
          capsuleInputSemanticDigest = semanticDigest semantic,
          capsuleInputToolVersion = "adrai/1.0.0",
          capsuleInputDigests = ProvenanceInputs Nothing Nothing Nothing
        }
  bytes <- mapLeft (QueryMaterializationInvalidRecord key "seal") (sealManagedDocument managed capsule)
  path <- mapLeft (QueryMaterializationInvalidRecord key "path") (canonicalManagedPath (configManagedPaths defaultConfig) managed)
  document <- mapLeft (QueryMaterializationInvalidRecord key "parse") (parseManagedDocument path bytes)
  mapLeft (QueryMaterializationInvalidRecord key "location") (validateManagedLocation (configManagedPaths defaultConfig) document)
  Right document

materializeSource :: FixtureMeta -> RevisionIdentity -> SourceTemplate -> Either QueryMaterializationError (Text, RelevantSource)
materializeSource meta revision template = do
  path <-
    mapLeft
      (QueryMaterializationInvalidSourcePath (sourceTemplateKey template) (sourceTemplatePath template))
      (mkRepoPath (sourceTemplatePath template))
  let bytes = sourceTemplateBytes template
      source =
        case sourceTemplateMode template of
          CommittedSource ->
            Right
              RevisionRelevantSource
                { relevantSourcePath = path,
                  relevantSourceResolvedRevision = revisionResolved revision,
                  relevantSourceBlob = sourceBlobIdentity meta template,
                  relevantSourceBytes = bytes
                }
          WorktreeSource -> worktreeSource revision path bytes
          DirtyWorktreeSource -> worktreeSource revision path bytes
          UntrackedWorktreeSource -> worktreeSource revision path bytes
  value <- source
  Right (sourceTemplateKey template, value)
  where
    worktreeSource identity path bytes
      | revisionRequested identity == "HEAD" =
          Right
            WorktreeRelevantSource
              { relevantSourcePath = path,
                relevantSourceHeadRevision = revisionResolved identity,
                relevantSourceBytes = bytes
              }
      | otherwise =
          Left
            ( QueryMaterializationUnsupportedSourceModeRevision
                (sourceTemplateKey template)
                (sourceTemplateMode template)
                (revisionRequested identity)
            )

generatedAdrId :: FixtureMeta -> AdrKey -> Either QueryMaterializationError AdrId
generatedAdrId meta key = do
  slot <- ordinalSlot key 1 0
  mapLeft (QueryMaterializationIdentityFailure key "adr") (adrIdFromBytes (identityBytes meta "adr" slot))

generatedRecordId :: FixtureMeta -> AdrKey -> Text -> Either QueryMaterializationError RecordId
generatedRecordId meta key role = do
  slot <- ordinalSlot key 1 0
  mapLeft (QueryMaterializationIdentityFailure key role) (recordIdFromBytes (identityBytes meta "record" slot))

generatedConnectionId :: FixtureMeta -> AdrKey -> Text -> Either QueryMaterializationError ConnectionId
generatedConnectionId meta key role = do
  offset <-
    case role of
      "scope" -> Right 0
      "domain" -> Right 1
      "status" -> Right 2
      _ -> Left (QueryMaterializationInvalidAdrKey key)
  slot <- ordinalSlot key 3 offset
  mapLeft (QueryMaterializationIdentityFailure key role) (connectionIdFromBytes (identityBytes meta "connection" slot))

generatedOperationId :: FixtureMeta -> AdrKey -> Either QueryMaterializationError OperationId
generatedOperationId meta key = do
  slot <- ordinalSlot key 1 0
  mapLeft (QueryMaterializationIdentityFailure key "operation") (operationIdFromBytes (identityBytes meta "operation" slot))

identityBytes :: FixtureMeta -> Text -> Word64 -> ByteString.ByteString
identityBytes meta namespace slot =
  ByteString.take 8 namespaceDigest <> word64BigEndian slot
  where
    namespaceDigest =
      digestBytes . sha256Digest . TextEncoding.encodeUtf8 $
        fixtureIdentity meta <> "\NUL" <> namespace

ordinalSlot :: AdrKey -> Integer -> Integer -> Either QueryMaterializationError Word64
ordinalSlot key@(AdrKey ordinal) multiplier offset
  | ordinal < 0 = Left (QueryMaterializationInvalidAdrKey key)
  | value > toInteger (maxBound :: Word64) = Left (QueryMaterializationInvalidAdrKey key)
  | otherwise = Right (fromInteger value)
  where
    value = toInteger ordinal * multiplier + offset

word64BigEndian :: Word64 -> ByteString.ByteString
word64BigEndian value =
  ByteString.pack [fromIntegral (value `shiftR` offset) | offset <- [56, 48 .. 0]]

snapshotResolvedRevision :: [ParsedManagedDocument] -> Text
snapshotResolvedRevision documents =
  renderDigest . sha256Digest . ByteString.concat $
    [ TextEncoding.encodeUtf8 (repoPathText (parsedManagedPath document))
        <> "\NUL"
        <> parsedManagedBytes document
        <> "\NUL"
      | document <- documents
    ]

sourceBlobIdentity :: FixtureMeta -> SourceTemplate -> Text
sourceBlobIdentity _ = renderDigest . sha256Digest . sourceTemplateBytes

fixtureIdentity :: FixtureMeta -> Text
fixtureIdentity meta =
  Text.intercalate
    "\NUL"
    [ fixtureVersionText (fixtureVersion meta),
      fixtureGenerator meta,
      Text.pack (show (seedWord64 (fixtureSeed meta))),
      fixturePurpose meta
    ]

fixtureVersionText :: FixtureVersion -> Text
fixtureVersionText FixtureV1 = "v1"

canonicalTemplateDomains :: AdrKey -> [Text] -> Either QueryMaterializationError [Domain]
canonicalTemplateDomains key values =
  mapLeft
    (QueryMaterializationInvalidDomain key (Text.intercalate "," values))
    (canonicalDomains values)

canonicalTemplateScopes :: AdrKey -> [Text] -> Either QueryMaterializationError [ScopePattern]
canonicalTemplateScopes key values = do
  parsed <- traverse parseOne (sortOn id values)
  Right (Set.toAscList (Set.fromList parsed))
  where
    parseOne value = mapLeft (QueryMaterializationInvalidScope key value) (mkScopePattern value)

templateBody :: AdrKey -> AdrTemplate -> Either QueryMaterializationError Text
templateBody key template =
  case adrTemplateBodySections template of
    [] -> Right ("# Decision\n" <> Text.strip (adrTemplateSummary template) <> "\n")
    sections -> do
      rendered <- traverse renderSection sections
      Right (Text.intercalate "\n\n" rendered <> "\n")
  where
    renderSection (heading, body)
      | Text.null (Text.strip heading) =
          Left (QueryMaterializationInvalidRecord key "body" (DocumentTextViolation "fixture section heading may not be empty"))
      | Text.null (Text.strip body) =
          Left (QueryMaterializationInvalidRecord key "body" (DocumentTextViolation "fixture section body may not be empty"))
      | otherwise = Right ("# " <> Text.strip heading <> "\n" <> Text.strip body)

recordEvent :: ManagedRecord -> Text
recordEvent managed =
  case managed of
    ManagedDecision _ -> "decision.create"
    ManagedConnection connection ->
      case connectionPayload connection of
        AppliesToConnection _ -> "scope.initial"
        DomainsConnection _ -> "domain.initial"
        StatusConnection _ -> "status.initial"
        _ -> "decision.amend"

managedObject :: ManagedRecord -> ProvenanceObjectId
managedObject managed =
  case managed of
    ManagedDecision decision -> ProvenanceRecord (decisionRecord decision)
    ManagedConnection connection -> ProvenanceConnection (connectionRecordId connection)

timestampFor :: AdrKey -> Integer
timestampFor (AdrKey key) = 1 + toInteger key `mod` 1000000000000

validateReducedAdr :: GraphReduction -> MaterializedAdr -> Either QueryMaterializationError ()
validateReducedAdr reduction item =
  case lookupReducedAdr (materializedAdrId item) reduction of
    Nothing -> Left (QueryMaterializationMissingReducedAdr (materializedKey item))
    Just reduced
      | null (reducedConflictAxes reduced) -> Right ()
      | otherwise -> Left (QueryMaterializationConflictedAdr (materializedKey item))

traverse_ :: (value -> Either error ()) -> [value] -> Either error ()
traverse_ action = foldr (\value rest -> action value >> rest) (Right ())

ensureUniqueAdrKeys :: [AdrTemplate] -> Either QueryMaterializationError ()
ensureUniqueAdrKeys = ensureUnique adrTemplateKey QueryMaterializationDuplicateAdrKey

ensureUniqueSourceKeys :: [SourceTemplate] -> Either QueryMaterializationError ()
ensureUniqueSourceKeys = ensureUnique sourceTemplateKey QueryMaterializationDuplicateSourceKey

ensureUnique :: (Ord key) => (value -> key) -> (key -> QueryMaterializationError) -> [value] -> Either QueryMaterializationError ()
ensureUnique select duplicateError = go Set.empty
  where
    go _ [] = Right ()
    go seen (value : remaining)
      | Set.member selected seen = Left (duplicateError selected)
      | otherwise = go (Set.insert selected seen) remaining
      where
        selected = select value

mapLeft :: (left -> otherLeft) -> Either left right -> Either otherLeft right
mapLeft action value =
  case value of
    Left problem -> Left (action problem)
    Right result -> Right result
