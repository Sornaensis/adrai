{-# LANGUAGE OverloadedStrings #-}

-- | Small, Haskell-owned valid repository fixture for the cache executable
-- contract.  Kept beside that contract so its component does not link the
-- large integration/stress support tree.
module CacheFixture (healthyCompilerFiles) where

import Adrai.Domain (mkDomain)
import Adrai.Format.Document
import Adrai.Provenance
import Adrai.Scope (mkScopePattern)
import Adrai.Types
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.Text as Text

healthyCompilerFiles :: GitOid -> Either Text.Text [(FilePath, ByteString)]
healthyCompilerFiles basis = do
  adr <- checked "ADR" (mkAdrId (fixtureId 'A' '0'))
  record <- checked "record" (mkRecordId (fixtureId 'R' '0'))
  amendedRecord <- checked "amended record" (mkRecordId (fixtureId 'R' '1'))
  -- Deliberately invert the graph-axis and canonical storage ID orders.  The
  -- bounded cache-selection executable needs this production row shape to
  -- guard writer/validator fingerprint reconstruction.
  scopeConnection <- checked "scope connection" (mkConnectionId (fixtureId 'C' '2'))
  domainConnection <- checked "domain connection" (mkConnectionId (fixtureId 'C' '0'))
  statusConnection <- checked "status connection" (mkConnectionId (fixtureId 'C' '1'))
  amendmentConnection <- checked "amendment connection" (mkConnectionId (fixtureId 'C' '3'))
  operation <- checked "operation" (mkOperationId (fixtureId 'O' '0'))
  amendmentOperation <- checked "amendment operation" (mkOperationId (fixtureId 'O' '1'))
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
      amendedDecision =
        ManagedDecision
          DecisionRecord
            { decisionAdr = adr,
              decisionRecord = amendedRecord,
              decisionTitle = "Use a canonical cold compiler",
              decisionSummary = "Canonical SQLite rows survive exact archive validation.",
              decisionDomains = [domain],
              decisionBody = "# Context\nRepository state is immutable.\n\n# Decision\nCompile by exact OID using canonical rows.\n\n# Consequences\nCold rebuilds are deterministic.\n"
            }
      amendmentRecord =
        ManagedConnection
          ConnectionRecord
            { connectionRecordId = amendmentConnection,
              connectionPayload = AmendsConnection (AmendsPayload adr amendedRecord [record]),
              connectionRationale = "Canonical amendment.\n"
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
  initial <- traverse (sealMember actor basis operation)
    [ (decision, "decision.create", []),
      (scopeRecord, "scope.initial", [ProvenanceRecord record]),
      (domainRecord, "domain.initial", [ProvenanceRecord record]),
      (statusRecord, "status.initial", [ProvenanceRecord record])
    ]
  amendment <- traverse (sealMember actor basis amendmentOperation)
    [ (amendedDecision, "decision.amend", [ProvenanceRecord record]),
      (amendmentRecord, "connection.amends", [ProvenanceRecord record])
    ]
  pure (initial <> amendment)

sealMember :: Actor -> GitOid -> OperationId -> (ManagedRecord, Text.Text, [ProvenanceObjectId]) -> Either Text.Text (FilePath, ByteString)
sealMember actor basis operation (record, eventText, parents) = do
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
          capsuleInputTimestampMs = 1700000000000,
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

managedObject :: ManagedRecord -> ProvenanceObjectId
managedObject record =
  case record of
    ManagedDecision decision -> ProvenanceRecord (decisionRecord decision)
    ManagedConnection connection -> ProvenanceConnection (connectionRecordId connection)

fixtureId :: Char -> Char -> Text.Text
fixtureId prefix suffix = Text.singleton prefix <> Text.replicate 25 "0" <> Text.singleton suffix

checked :: Show problem => Text.Text -> Either problem value -> Either Text.Text value
checked context = first (\problem -> context <> ": " <> Text.pack (show problem))
