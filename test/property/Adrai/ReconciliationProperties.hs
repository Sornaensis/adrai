{-# LANGUAGE OverloadedStrings #-}

module Adrai.ReconciliationProperties (tests) where

import Adrai.Format.Document
import Adrai.Graph
import Adrai.Property.Generators
import Adrai.Service
import Adrai.Types
import Data.List (nub)
import Hedgehog (Property, PropertyT, assert, footnote, forAll, property, withTests, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "P2-06 reconciliation properties"
    [ testProperty "decision reconciliation is append-only and monotone" (prop_reconcile DecisionAxis),
      testProperty "scope reconciliation is append-only and monotone" (prop_reconcile ScopeAxis),
      testProperty "domain reconciliation is append-only and monotone" (prop_reconcile DomainAxis),
      testProperty "status reconciliation is append-only and monotone" (prop_reconcile StatusAxis)
    ]

prop_reconcile :: GraphAxis -> Property
prop_reconcile axis = withTests 60 . property $ do
  spec <- forAll (genAxisConflictSpec (Just axis))
  let fixture = materializeAxisConflict spec
      records = axisFixtureRecords fixture
      target = axisFixtureSubject fixture
      choice = axisFixtureChoice fixture
      beforeReduction = reduceManagedGraph records
  graphReductionIssues beforeReduction === []
  case lookupReducedAdr target beforeReduction of
    Nothing -> footnote "reconciliation fixture did not materialize" >> assert False
    Just before -> do
      assert (axis `elem` reducedConflictAxes before)
      let oldToken = reducedStateToken before
      case planReconciliation records target oldToken choice of
        Left problem -> footnote (show problem) >> assert False
        Right plan -> verifySuccessfulPlan axis fixture before plan

verifySuccessfulPlan :: GraphAxis -> AxisFixture -> ReducedAdr -> ReconciliationPlan -> PropertyT IO ()
verifySuccessfulPlan axis fixture before plan = do
  let records = axisFixtureRecords fixture
      target = axisFixtureSubject fixture
      choice = axisFixtureChoice fixture
      appends = reconciliationAppends plan
      combined = records <> appends
      after = reconciliationTargetResult plan
      nextToken = reconciliationNextStateToken plan
      expectedAppendCount = if axis == DecisionAxis then 2 else 1
  length appends === expectedAppendCount
  take (length records) combined === records
  drop (length records) combined === appends
  assert (freshObjects records appends)
  assert (consumesSelectedHeads axis before appends)
  reconciliationResult plan === reduceManagedGraph combined
  graphReductionIssues (reconciliationResult plan) === []
  assert (axis `notElem` reducedConflictAxes after)
  assert (unselectedAxesPreserved axis before after)
  reducedStateToken after === nextToken

  shuffled <- forAll (sampledPermutation records)
  planReconciliation shuffled target (reducedStateToken before) choice === Right plan

  planReconciliation combined target (reducedStateToken before) choice
    === Left (ReconciliationStaleState (reducedStateToken before) nextToken)
  planReconciliation combined target nextToken choice
    === Left (ReconciliationNoConflict axis)

  let pool = axisConflictPool (axisFixtureSpec fixture)
      wrongSubject = adrIdAt pool 10
      reused = reusedChoice records choice
  planReconciliation records target (reducedStateToken before) (choiceWithSubject wrongSubject choice)
    === Left (ReconciliationWrongSubject target wrongSubject)
  case reused of
    (reusedChoiceValue, expectedError) ->
      planReconciliation records target (reducedStateToken before) reusedChoiceValue
        === Left expectedError

freshObjects :: [ManagedRecord] -> [ManagedRecord] -> Bool
freshObjects originals appends =
  let originalIds = map managedObjectId originals
      appendedIds = map managedObjectId appends
   in length appendedIds == length (nub appendedIds)
        && all (`notElem` originalIds) appendedIds

consumesSelectedHeads :: GraphAxis -> ReducedAdr -> [ManagedRecord] -> Bool
consumesSelectedHeads axis before appends =
  case axis of
    DecisionAxis ->
      case [amendsToRecords payload | ManagedConnection connection <- appends, AmendsConnection payload <- [connectionPayload connection]] of
        [parents] -> parents == axisResolutionHeads (reducedDecisionAxis before)
        _ -> False
    ScopeAxis ->
      case [appliesToParentConnections payload | ManagedConnection connection <- appends, AppliesToConnection payload <- [connectionPayload connection]] of
        [parents] -> parents == axisResolutionHeads (reducedScopeAxis before)
        _ -> False
    DomainAxis ->
      case [domainsParentConnections payload | ManagedConnection connection <- appends, DomainsConnection payload <- [connectionPayload connection]] of
        [parents] -> parents == axisResolutionHeads (reducedDomainAxis before)
        _ -> False
    StatusAxis ->
      case [statusParentConnections payload | ManagedConnection connection <- appends, StatusConnection payload <- [connectionPayload connection]] of
        [parents] -> parents == axisResolutionHeads (reducedStatusAxis before)
        _ -> False

unselectedAxesPreserved :: GraphAxis -> ReducedAdr -> ReducedAdr -> Bool
unselectedAxesPreserved selected before after =
  and
    [ selected == DecisionAxis || (reducedDecisionAxis before, reducedDecisionHistory before, reducedAmendmentHistory before) == (reducedDecisionAxis after, reducedDecisionHistory after, reducedAmendmentHistory after),
      selected == ScopeAxis || (reducedScopeAxis before, reducedScopeHistory before) == (reducedScopeAxis after, reducedScopeHistory after),
      selected == DomainAxis || (reducedDomainAxis before, reducedDomainHistory before) == (reducedDomainAxis after, reducedDomainHistory after),
      selected == StatusAxis || (reducedStatusAxis before, reducedStatusHistory before) == (reducedStatusAxis after, reducedStatusHistory after)
    ]

choiceWithSubject :: AdrId -> ReconciliationChoice -> ReconciliationChoice
choiceWithSubject subject choice =
  case choice of
    ReconcileDecision resolution -> ReconcileDecision (resolution {decisionResolutionSubject = subject})
    ReconcileScope resolution -> ReconcileScope (resolution {scopeResolutionSubject = subject})
    ReconcileDomain resolution -> ReconcileDomain (resolution {domainResolutionSubject = subject})
    ReconcileStatus resolution -> ReconcileStatus (resolution {statusResolutionSubject = subject})

reusedChoice :: [ManagedRecord] -> ReconciliationChoice -> (ReconciliationChoice, ReconciliationError)
reusedChoice records choice =
  case choice of
    ReconcileDecision resolution ->
      let existing = firstRecordId records
          decision = (decisionResolutionDecision resolution) {decisionRecord = existing}
       in (ReconcileDecision (resolution {decisionResolutionDecision = decision}), ReconciliationReusedRecordId existing)
    ReconcileScope resolution ->
      let existing = firstConnectionId records
       in (ReconcileScope (resolution {scopeResolutionConnectionId = existing}), ReconciliationReusedConnectionId existing)
    ReconcileDomain resolution ->
      let existing = firstConnectionId records
       in (ReconcileDomain (resolution {domainResolutionConnectionId = existing}), ReconciliationReusedConnectionId existing)
    ReconcileStatus resolution ->
      let existing = firstConnectionId records
       in (ReconcileStatus (resolution {statusResolutionConnectionId = existing}), ReconciliationReusedConnectionId existing)

firstRecordId :: [ManagedRecord] -> RecordId
firstRecordId records =
  case [decisionRecord decision | ManagedDecision decision <- records] of
    identifier : _ -> identifier
    [] -> error "P2-06 reconciliation fixture has no decision"

firstConnectionId :: [ManagedRecord] -> ConnectionId
firstConnectionId records =
  case [connectionRecordId connection | ManagedConnection connection <- records] of
    identifier : _ -> identifier
    [] -> error "P2-06 reconciliation fixture has no connection"
