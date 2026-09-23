{-# LANGUAGE OverloadedStrings #-}

module Adrai.WindowDocuments (windowDocuments) where

import Adrai.Domain (mkDomain)
import Adrai.Format.Document (ParsedManagedDocument, ManagedRecord (..), DecisionRecord (..), ConnectionRecord (..), ConnectionPayload (..), AppliesToPayload (..), DomainsPayload (..), StatusPayload (..), StatusState (..), canonicalManagedPath, parseManagedDocument, renderManagedSemantic, sealManagedDocument)
import Adrai.Provenance (GitOid, ProvenanceCapsuleInput (..), ProvenanceObjectId (..), mkEventKind, mkLineAnchor, mkProvenanceCapsule, semanticDigest)
import Adrai.Scope (mkScopePattern)
import Adrai.Types (Actor, ActorKind (..), Config (..), OperationId, ProvenanceInputs (..), defaultConfig, mkActor, mkAdrId, mkConnectionId, mkOperationId, mkRecordId)
import Data.Text (Text)
import qualified Data.Text as Text

windowDocuments :: GitOid -> Int -> [ParsedManagedDocument]
windowDocuments basis number = map (windowDocument basis actor operation) members
  where
    identifier prefix value = Text.cons prefix (Text.justifyRight 26 '0' (Text.pack (show value)))
    adr = must (mkAdrId (identifier 'A' number))
    record = must (mkRecordId (identifier 'R' number))
    scopeConnection = must (mkConnectionId (identifier 'C' (number * 4)))
    domainConnection = must (mkConnectionId (identifier 'C' (number * 4 + 1)))
    statusConnection = must (mkConnectionId (identifier 'C' (number * 4 + 2)))
    operation = must (mkOperationId (identifier 'O' number))
    actor = must (mkActor ServiceActor "window-fixture" Nothing)
    domain = must (mkDomain "platform")
    scope = must (mkScopePattern "src/**")
    decision = ManagedDecision (DecisionRecord adr record ("Window decision " <> Text.pack (show number)) "A sealed matching decision." [domain] "# Decision\nUse an exact bounded window.\n")
    scopeRecord = ManagedConnection (ConnectionRecord scopeConnection (AppliesToConnection (AppliesToPayload adr [] "initial" [scope] [] [scope])) "Initial scope.\n")
    domainRecord = ManagedConnection (ConnectionRecord domainConnection (DomainsConnection (DomainsPayload adr [] "initial" [domain] [] [domain] [])) "Initial domain.\n")
    statusRecord = ManagedConnection (ConnectionRecord statusConnection (StatusConnection (StatusPayload adr [] StatusActive [record] Nothing)) "Initial status.\n")
    members =
      [ (decision, "decision.create", [], ProvenanceRecord record),
        (scopeRecord, "scope.initial", [ProvenanceRecord record], ProvenanceConnection scopeConnection),
        (domainRecord, "domain.initial", [ProvenanceRecord record], ProvenanceConnection domainConnection),
        (statusRecord, "status.initial", [ProvenanceRecord record], ProvenanceConnection statusConnection)
      ]

windowDocument :: GitOid -> Actor -> OperationId -> (ManagedRecord, Text, [ProvenanceObjectId], ProvenanceObjectId) -> ParsedManagedDocument
windowDocument basis actor operation (managed, eventText, parents, objectId) =
  must (parseManagedDocument path bytes)
  where
    semantic = must (renderManagedSemantic managed)
    event = must (mkEventKind eventText)
    anchor = must (mkLineAnchor "window@logical\nline" basis)
    capsule = must (mkProvenanceCapsule (ProvenanceCapsuleInput operation objectId event actor 1700000000000 basis parents Nothing Nothing [anchor] (semanticDigest semantic) "adrai/1.0.0" (ProvenanceInputs Nothing Nothing Nothing)))
    bytes = must (sealManagedDocument managed capsule)
    path = must (canonicalManagedPath (configManagedPaths defaultConfig) managed)

must :: (Show error) => Either error value -> value
must result = case result of Right value -> value; Left problem -> error (show problem)
