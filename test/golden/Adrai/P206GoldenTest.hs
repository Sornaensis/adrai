{-# LANGUAGE OverloadedStrings #-}

module Adrai.P206GoldenTest (tests) where

import Adrai.Format.Document
import Adrai.Format.Json
import Adrai.Graph
import Adrai.Property.Generators
import Adrai.Service
import Adrai.Types
import Data.ByteString qualified as BS
import Data.List (sort)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "P2-06 golden parity"
    [ testCase "four-axis reducer and reconciliation contract is byte exact" $ do
        expected <- BS.readFile "test/golden/p2-06/reducer-reconciliation.golden"
        renderCanonicalJsonBytes goldenValue @?= expected
    ]

goldenValue :: JsonValue
goldenValue =
  object
    [ ("schema", JsonString "adrai/p2-06-reducer-reconciliation/v1"),
      ("invalid_cases", JsonArray (map invalidCase invalidSpecs)),
      ("reconciliation_cases", JsonArray (map reconciliationCase axes))
    ]
  where
    axes = [DecisionAxis, ScopeAxis, DomainAxis, StatusAxis]
    invalidSpecs =
      [ InvalidGraphSpec pool DanglingDecisionParent,
        InvalidGraphSpec pool CyclicScopeParents,
        InvalidGraphSpec pool InvalidDomainDelta,
        InvalidGraphSpec pool CrossAdrStatusParent
      ]
    pool = IdentifierPool 0

invalidCase :: InvalidGraphSpec -> JsonValue
invalidCase spec =
  object
    [ ("axis", JsonString (axisText (invalidFixtureAxis fixture))),
      ("fault", JsonString (invalidFixtureFaultLabel fixture)),
      ("heads", textArray (selectedHeadTexts (invalidFixtureAxis fixture) reduced)),
      ("issue_codes", textArray (sort (map (graphIssueCodeText . graphIssueCode) (graphReductionIssues reduction))))
    ]
  where
    fixture = materializeInvalidGraph spec
    reduction = reduceManagedGraph (invalidFixtureRecords fixture)
    reduced = expectReduced (adrIdAt (invalidGraphPool spec) 0) reduction

reconciliationCase :: GraphAxis -> JsonValue
reconciliationCase axis =
  object
    [ ("axis", JsonString (axisText axis)),
      ("before", stateSummary axis before),
      ("appends", JsonArray (map appendSummary (reconciliationAppends plan))),
      ("after", stateSummary axis after),
      ("untouched_axes", untouchedEvidence axis before after)
    ]
  where
    fixture = materializeAxisConflict (AxisConflictSpec (IdentifierPool 0) axis 0)
    reduction = reduceManagedGraph (axisFixtureRecords fixture)
    before = expectReduced (axisFixtureSubject fixture) reduction
    plan =
      case planReconciliation (axisFixtureRecords fixture) (axisFixtureSubject fixture) (reducedStateToken before) (axisFixtureChoice fixture) of
        Right value -> value
        Left problem -> error (show problem)
    after = reconciliationTargetResult plan

stateSummary :: GraphAxis -> ReducedAdr -> JsonValue
stateSummary axis adr =
  object
    [ ("heads", textArray (selectedHeadTexts axis adr)),
      ("state_token", JsonString (stateTokenText (reducedStateToken adr)))
    ]

appendSummary :: ManagedRecord -> JsonValue
appendSummary managed =
  object
    [ ("kind", JsonString kind),
      ("object", JsonString (objectText managed)),
      ("parents", textArray parents)
    ]
  where
    (kind, parents) =
      case managed of
        ManagedDecision _ -> ("decision", [])
        ManagedConnection connection ->
          case connectionPayload connection of
            AmendsConnection payload -> ("amends", sort (map recordIdText (amendsToRecords payload)))
            AppliesToConnection payload -> ("applies_to", sort (map connectionIdText (appliesToParentConnections payload)))
            DomainsConnection payload -> ("domains", sort (map connectionIdText (domainsParentConnections payload)))
            StatusConnection payload -> ("status", sort (map connectionIdText (statusParentConnections payload)))

untouchedEvidence :: GraphAxis -> ReducedAdr -> ReducedAdr -> JsonValue
untouchedEvidence selected before after =
  object . concat $
    [ [("decision", JsonBool (reducedDecisionAxis before == reducedDecisionAxis after)) | selected /= DecisionAxis],
      [("scope", JsonBool (reducedScopeAxis before == reducedScopeAxis after)) | selected /= ScopeAxis],
      [("domain", JsonBool (reducedDomainAxis before == reducedDomainAxis after)) | selected /= DomainAxis],
      [("status", JsonBool (reducedStatusAxis before == reducedStatusAxis after)) | selected /= StatusAxis]
    ]

selectedHeadTexts :: GraphAxis -> ReducedAdr -> [Text]
selectedHeadTexts axis adr =
  case axis of
    DecisionAxis -> map recordIdText (axisResolutionHeads (reducedDecisionAxis adr))
    ScopeAxis -> map connectionIdText (axisResolutionHeads (reducedScopeAxis adr))
    DomainAxis -> map connectionIdText (axisResolutionHeads (reducedDomainAxis adr))
    StatusAxis -> map connectionIdText (axisResolutionHeads (reducedStatusAxis adr))

objectText :: ManagedRecord -> Text
objectText managed =
  case managed of
    ManagedDecision record -> recordIdText (decisionRecord record)
    ManagedConnection record -> connectionIdText (connectionRecordId record)

axisText :: GraphAxis -> Text
axisText axis =
  case axis of
    DecisionAxis -> "decision"
    ScopeAxis -> "scope"
    DomainAxis -> "domain"
    StatusAxis -> "status"

textArray :: [Text] -> JsonValue
textArray = JsonArray . map JsonString

expectReduced :: AdrId -> GraphReduction -> ReducedAdr
expectReduced adr reduction =
  case lookupReducedAdr adr reduction of
    Just reduced -> reduced
    Nothing -> error "P2-06 golden ADR missing"
