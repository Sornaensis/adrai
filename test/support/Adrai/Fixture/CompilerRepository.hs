{-# LANGUAGE OverloadedStrings #-}

module Adrai.Fixture.CompilerRepository
  ( healthyCompilerFiles,
    largeTimestampCompilerFiles,
    scopeExpandedCompilerFiles,
    conflictedCompilerFiles,
    zeroHeadCompilerFiles,
    compilerAdrId,
    compilerRecordId,
  )
where

import Adrai.Domain (mkDomain)
import Adrai.Format.Document
import Adrai.Provenance
import Adrai.Scope (mkScopePattern)
import Adrai.Types
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.Text as Text

healthyCompilerFiles :: GitOid -> Either Text.Text [(FilePath, ByteString)]
healthyCompilerFiles = healthyCompilerFilesAt defaultTimestamp

largeTimestampCompilerFiles :: GitOid -> Either Text.Text [(FilePath, ByteString)]
largeTimestampCompilerFiles = healthyCompilerFilesAt 9223372036854775808

healthyCompilerFilesAt :: Integer -> GitOid -> Either Text.Text [(FilePath, ByteString)]
healthyCompilerFilesAt timestamp basis = do
  adr <- checked "ADR" (mkAdrId (fixtureId 'A' '0'))
  record <- checked "record" (mkRecordId (fixtureId 'R' '0'))
  scopeConnection <- checked "scope connection" (mkConnectionId (fixtureId 'C' '0'))
  domainConnection <- checked "domain connection" (mkConnectionId (fixtureId 'C' '1'))
  statusConnection <- checked "status connection" (mkConnectionId (fixtureId 'C' '2'))
  operation <- checked "operation" (mkOperationId (fixtureId 'O' '0'))
  actor <- checked "actor" (mkActor ServiceActor "adrai-compiler-fixture" Nothing)
  domain <- checked "domain" (mkDomain "platform")
  scope <- checked "scope" (mkScopePattern "src/a,b/**")
  let decision =
        ManagedDecision
          DecisionRecord
            { decisionAdr = adr,
              decisionRecord = record,
              decisionTitle = "Use a cold compiler",
              decisionSummary = "Compile immutable repository observations into disposable SQLite.",
              decisionDomains = [domain],
              decisionBody = "# Context\nRepository state is immutable.\n\n# Decision\nCompile by exact OID.\n\n# Consequences\nCold rebuilds are deterministic.\n"
            }
      scopeRecord =
        ManagedConnection
          ConnectionRecord
            { connectionRecordId = scopeConnection,
              connectionPayload = AppliesToConnection (AppliesToPayload adr [] "initial" [scope] [] [scope]),
              connectionRationale = "Initial scope.\n"
            }
      domainRecord =
        ManagedConnection
          ConnectionRecord
            { connectionRecordId = domainConnection,
              connectionPayload = DomainsConnection (DomainsPayload adr [] "initial" [domain] [] [domain] []),
              connectionRationale = "Initial domain.\n"
            }
      statusRecord =
        ManagedConnection
          ConnectionRecord
            { connectionRecordId = statusConnection,
              connectionPayload = StatusConnection (StatusPayload adr [] StatusActive [record] Nothing),
              connectionRationale = "Initial active status.\n"
            }
      members =
        [ (decision, "decision.create", []),
          (scopeRecord, "scope.initial", [ProvenanceRecord record]),
          (domainRecord, "domain.initial", [ProvenanceRecord record]),
          (statusRecord, "status.initial", [ProvenanceRecord record])
        ]
  traverse (sealMemberAt timestamp actor basis operation) members

scopeExpandedCompilerFiles :: GitOid -> Either Text.Text [(FilePath, ByteString)]
scopeExpandedCompilerFiles basis = do
  original <- healthyCompilerFiles basis
  adr <- checked "ADR" (mkAdrId (fixtureId 'A' '0'))
  actor <- checked "actor" (mkActor ServiceActor "adrai-compiler-fixture" Nothing)
  parent <- checked "root scope connection" (mkConnectionId (fixtureId 'C' '0'))
  connection <- checked "expanded scope connection" (mkConnectionId (fixtureId 'C' '3'))
  operation <- checked "expanded scope operation" (mkOperationId (fixtureId 'O' '1'))
  initialScope <- checked "initial scope" (mkScopePattern "src/a,b/**")
  addedScope <- checked "added scope" (mkScopePattern "test/**")
  expanded <-
    sealMember
      actor
      basis
      operation
      ( ManagedConnection
          ConnectionRecord
            { connectionRecordId = connection,
              connectionPayload = AppliesToConnection (AppliesToPayload adr [parent] "expand" [addedScope] [] [initialScope, addedScope]),
              connectionRationale = "Expand the managed scope.\n"
            },
        "scope.expand",
        [ProvenanceConnection parent]
      )
  Right (original <> [expanded])

conflictedCompilerFiles :: GitOid -> Either Text.Text [(FilePath, ByteString)]
conflictedCompilerFiles basis = do
  original <- healthyCompilerFiles basis
  adr <- checked "ADR" (mkAdrId (fixtureId 'A' '0'))
  rootRecord <- checked "root record" (mkRecordId (fixtureId 'R' '0'))
  actor <- checked "actor" (mkActor ServiceActor "adrai-compiler-fixture" Nothing)
  firstRecord <- checked "first amendment record" (mkRecordId (fixtureId 'R' '1'))
  secondRecord <- checked "second amendment record" (mkRecordId (fixtureId 'R' '2'))
  firstConnection <- checked "first amendment connection" (mkConnectionId (fixtureId 'C' '3'))
  secondConnection <- checked "second amendment connection" (mkConnectionId (fixtureId 'C' '4'))
  statusConnection <- checked "conflict status connection" (mkConnectionId (fixtureId 'C' '5'))
  rootStatusConnection <- checked "root status connection" (mkConnectionId (fixtureId 'C' '2'))
  firstOperation <- checked "first amendment operation" (mkOperationId (fixtureId 'O' '1'))
  secondOperation <- checked "second amendment operation" (mkOperationId (fixtureId 'O' '2'))
  statusOperation <- checked "conflict status operation" (mkOperationId (fixtureId 'O' '3'))
  let amendment record title =
        ManagedDecision
          DecisionRecord
            { decisionAdr = adr,
              decisionRecord = record,
              decisionTitle = title,
              decisionSummary = "A conflicting amendment candidate.",
              decisionDomains = [],
              decisionBody = "# Context\nTwo branches amended the decision.\n\n# Decision\nRetain this candidate.\n"
            }
      edge connection child =
        ManagedConnection
          ConnectionRecord
            { connectionRecordId = connection,
              connectionPayload = AmendsConnection (AmendsPayload adr child [rootRecord]),
              connectionRationale = "Concurrent amendment.\n"
            }
      parents = [ProvenanceRecord rootRecord]
  firstBranch <-
    traverse
      (sealMember actor basis firstOperation)
      [ (amendment firstRecord "First amendment", "decision.amend", parents),
        (edge firstConnection firstRecord, "connection.amends", parents)
      ]
  second <-
    traverse
      (sealMember actor basis secondOperation)
      [ (amendment secondRecord "Second amendment", "decision.amend", parents),
        (edge secondConnection secondRecord, "connection.amends", parents)
      ]
  status <-
    sealMember
      actor
      basis
      statusOperation
      ( ManagedConnection
          ConnectionRecord
            { connectionRecordId = statusConnection,
              connectionPayload = StatusConnection (StatusPayload adr [rootStatusConnection] StatusActive [firstRecord, secondRecord] Nothing),
              connectionRationale = "Cover both concurrent decision heads.\n"
            },
        "decision.reactivate",
        [ProvenanceConnection rootStatusConnection, ProvenanceRecord firstRecord, ProvenanceRecord secondRecord]
      )
  Right (original <> firstBranch <> second <> [status])

zeroHeadCompilerFiles :: GitOid -> Either Text.Text [(FilePath, ByteString)]
zeroHeadCompilerFiles basis = do
  adr <- checked "ADR" (mkAdrId (fixtureId 'A' '0'))
  missingChild <- checked "missing child" (mkRecordId (fixtureId 'R' '7'))
  missingParent <- checked "missing parent" (mkRecordId (fixtureId 'R' '8'))
  connection <- checked "orphan amendment" (mkConnectionId (fixtureId 'C' '7'))
  operation <- checked "orphan operation" (mkOperationId (fixtureId 'O' '7'))
  actor <- checked "actor" (mkActor ServiceActor "adrai-compiler-fixture" Nothing)
  member <-
    sealMember
      actor
      basis
      operation
      ( ManagedConnection
          ConnectionRecord
            { connectionRecordId = connection,
              connectionPayload = AmendsConnection (AmendsPayload adr missingChild [missingParent]),
              connectionRationale = "Deliberately orphaned amendment.\n"
            },
        "connection.amends",
        [ProvenanceRecord missingParent]
      )
  Right [member]

compilerAdrId :: AdrId
compilerAdrId = expect "ADR" (mkAdrId (fixtureId 'A' '0'))

compilerRecordId :: RecordId
compilerRecordId = expect "record" (mkRecordId (fixtureId 'R' '0'))

sealMember :: Actor -> GitOid -> OperationId -> (ManagedRecord, Text.Text, [ProvenanceObjectId]) -> Either Text.Text (FilePath, ByteString)
sealMember = sealMemberAt defaultTimestamp

sealMemberAt :: Integer -> Actor -> GitOid -> OperationId -> (ManagedRecord, Text.Text, [ProvenanceObjectId]) -> Either Text.Text (FilePath, ByteString)
sealMemberAt timestamp actor basis operation (record, eventText, parents) = do
  semantic <- checked "semantic" (renderManagedSemantic record)
  event <- checked "event" (mkEventKind eventText)
  anchor <- checked "line anchor" (mkLineAnchor "logical@anchor\nsecond-line" basis)
  capsule <-
    checked "capsule" . mkProvenanceCapsule $
      ProvenanceCapsuleInput
        { capsuleInputOperationId = operation,
          capsuleInputObjectId = managedObject record,
          capsuleInputEventKind = event,
          capsuleInputActor = actor,
          capsuleInputTimestampMs = timestamp,
          capsuleInputBasis = basis,
          capsuleInputParents = parents,
          capsuleInputBranchHint = Nothing,
          capsuleInputUpstreamHint = Nothing,
          capsuleInputLineAnchors = [anchor],
          capsuleInputSemanticDigest = semanticDigest semantic,
          capsuleInputToolVersion = "adrai/1.0.0",
          capsuleInputDigests = ProvenanceInputs Nothing Nothing Nothing
        }
  bytes <- checked "sealed document" (sealManagedDocument record capsule)
  path <- checked "canonical path" (canonicalManagedPath (configManagedPaths defaultConfig) record)
  Right (Text.unpack (repoPathText path), bytes)

defaultTimestamp :: Integer
defaultTimestamp = 1700000000000

managedObject :: ManagedRecord -> ProvenanceObjectId
managedObject record =
  case record of
    ManagedDecision decision -> ProvenanceRecord (decisionRecord decision)
    ManagedConnection connection -> ProvenanceConnection (connectionRecordId connection)

fixtureId :: Char -> Char -> Text.Text
fixtureId prefix suffix = Text.singleton prefix <> Text.replicate 25 "0" <> Text.singleton suffix

checked :: (Show problem) => Text.Text -> Either problem value -> Either Text.Text value
checked context = first (\problem -> context <> ": " <> Text.pack (show problem))

expect :: (Show problem) => Text.Text -> Either problem value -> value
expect context result =
  case checked context result of
    Left problem -> error (Text.unpack problem)
    Right value -> value
