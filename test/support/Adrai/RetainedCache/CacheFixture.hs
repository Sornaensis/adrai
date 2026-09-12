{-# LANGUAGE OverloadedStrings #-}

-- | Small, Haskell-owned valid repository fixture shared by the retained cache
-- integration seed. The isolated cache executable keeps an equivalent local
-- fixture because its component deliberately does not link test/support.
module Adrai.RetainedCache.CacheFixture
  ( additionalSimpleCompilerFiles,
    healthyCompilerFiles,
    healthySimpleCompilerFiles,
  ) where

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

-- | The four-document shape produced by a single ADR creation, without
-- invoking the CLI's post-commit compiler.  The returned operation ID is used
-- by the trailer integration test.
healthySimpleCompilerFiles :: GitOid -> Either Text.Text (Text.Text, [(FilePath, ByteString)])
healthySimpleCompilerFiles basis = simpleCompilerFiles basis InitialSimpleFixture

-- | A distinct four-document ADR for merge-shaped integration setup.
additionalSimpleCompilerFiles :: GitOid -> Either Text.Text [(FilePath, ByteString)]
additionalSimpleCompilerFiles basis = snd <$> simpleCompilerFiles basis AdditionalSimpleFixture

data SimpleFixtureKind = InitialSimpleFixture | AdditionalSimpleFixture

simpleCompilerFiles :: GitOid -> SimpleFixtureKind -> Either Text.Text (Text.Text, [(FilePath, ByteString)])
simpleCompilerFiles basis kind = do
  let (adrSuffix, recordSuffix, scopeSuffix, domainSuffix, statusSuffix, operationSuffix, title, summary, body, domainText, scopeText) =
        case kind of
          InitialSimpleFixture ->
            ( '0', '0', '2', '0', '1', '0',
              "Use a cold compiler",
              "Compile immutable repository observations into disposable SQLite.",
              "# Context\nRepository state is immutable.\n\n# Decision\nCompile by exact OID.\n\n# Consequences\nCold rebuilds are deterministic.\n",
              "platform",
              "src/a,b/**"
            )
          AdditionalSimpleFixture ->
            ( '2', '2', '6', '4', '5', '2',
              "Use durable job delivery",
              "Workers acknowledge durable jobs only after successful execution.",
              "# Context\nJobs must survive worker restarts.\n\n# Decision\nUse durable queues and idempotent handlers.\n\n# Consequences\nRetries are safe.\n",
              "runtime.jobs",
              "src/jobs/**"
            )
      operationText = fixtureId 'O' operationSuffix
  adr <- checked "ADR" (mkAdrId (fixtureId 'A' adrSuffix))
  record <- checked "record" (mkRecordId (fixtureId 'R' recordSuffix))
  scopeConnection <- checked "scope connection" (mkConnectionId (fixtureId 'C' scopeSuffix))
  domainConnection <- checked "domain connection" (mkConnectionId (fixtureId 'C' domainSuffix))
  statusConnection <- checked "status connection" (mkConnectionId (fixtureId 'C' statusSuffix))
  operation <- checked "operation" (mkOperationId operationText)
  actor <- checked "actor" (mkActor ServiceActor "adrai-compiler-fixture" Nothing)
  domain <- checked "domain" (mkDomain domainText)
  scope <- checked "scope" (mkScopePattern scopeText)
  let decision =
        ManagedDecision
          DecisionRecord
            { decisionAdr = adr,
              decisionRecord = record,
              decisionTitle = title,
              decisionSummary = summary,
              decisionDomains = [domain],
              decisionBody = body
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
  files <- traverse (sealMember actor basis operation)
    [ (decision, "decision.create", []),
      (scopeRecord, "scope.initial", [ProvenanceRecord record]),
      (domainRecord, "domain.initial", [ProvenanceRecord record]),
      (statusRecord, "status.initial", [ProvenanceRecord record])
    ]
  pure (operationText, files)

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
