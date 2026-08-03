{-# LANGUAGE OverloadedStrings #-}

module Adrai.GraphProperties (tests) where

import Adrai.Domain (Domain)
import Adrai.Format.Document
import Adrai.Graph
import Adrai.Property.Generators
import Adrai.State (stateTokenForHeads)
import Adrai.Types
import Data.Set qualified as Set
import Data.Text (Text)
import Hedgehog (Property, assert, footnote, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "P2-06 graph properties"
    [ testProperty "whole reductions ignore sampled input permutations" prop_permutationInvariant,
      testProperty "heads tokens conflicts and conservative projections agree" prop_reductionInvariants,
      testProperty "a valid mutation changes only its selected axis" prop_axisIsolation,
      testProperty "dangling decision child is quarantined" (prop_invalidChildQuarantine DanglingDecisionParent),
      testProperty "cyclic scope children are quarantined" (prop_invalidChildQuarantine CyclicScopeParents),
      testProperty "invalid domain delta is quarantined" (prop_invalidChildQuarantine InvalidDomainDelta),
      testProperty "cross-ADR status parent is quarantined" (prop_invalidChildQuarantine CrossAdrStatusParent),
      testProperty "duplicate delivery is quarantined instead of treated as idempotent" prop_duplicateQuarantine
    ]

prop_permutationInvariant :: Property
prop_permutationInvariant = withTests 75 . property $ do
  spec <- forAll (genAxisConflictSpec Nothing)
  let records = axisFixtureRecords (materializeAxisConflict spec)
      expected = reduceManagedGraph records
  first <- forAll (sampledPermutation records)
  second <- forAll (sampledPermutation records)
  reduceManagedGraph first === expected
  reduceManagedGraph second === expected

prop_reductionInvariants :: Property
prop_reductionInvariants = withTests 75 . property $ do
  spec <- forAll (genAxisConflictSpec Nothing)
  let fixture = materializeAxisConflict spec
      axis = axisConflictAxis spec
      reduction = reduceManagedGraph (axisFixtureRecords fixture)
  graphReductionIssues reduction === []
  case lookupReducedAdr (axisFixtureSubject fixture) reduction of
    Nothing -> footnote "generated ADR was not reduced" >> assert False
    Just adr -> do
      assert (allHeadsCanonical adr)
      reducedStateToken adr === stateTokenForHeads (reducedStateHeads adr)
      let expectedMessage = "2 " <> axisLabel axis <> " heads"
          expectedCandidate =
            ConflictCandidate
              { conflictCandidateAxis = axis,
                conflictCandidateHeads = selectedObjectRefs axis adr,
                conflictCandidateHeadCount = 2,
                conflictCandidateSummary = expectedMessage
              }
      reducedConflictAxes adr === [axis]
      reducedConflictMessages adr === [expectedMessage]
      case classifyAdrConflict adr of
        Nothing -> footnote "selected fork was not classified" >> assert False
        Just conflict -> do
          adrConflictCode conflict === adrConflictCodeText
          adrConflictAdr conflict === axisFixtureSubject fixture
          adrConflictCandidates conflict === [expectedCandidate]
          adrConflictCount conflict === 1
          adrConflictSummaries conflict === [expectedMessage]
          adrConflictStateToken conflict === reducedStateToken adr
      case axis of
        DomainAxis ->
          axisResolutionEffective (reducedDomainAxis adr)
            === expectedDomainFallback fixture
        StatusAxis ->
          assert (statusHeadsCoverCurrentDecision adr)
        _ -> pure ()

prop_axisIsolation :: Property
prop_axisIsolation = withTests 75 . property $ do
  spec <- forAll (genAxisConflictSpec Nothing)
  let fixture = materializeAxisConflict spec
      axis = axisConflictAxis spec
      beforeReduction = reduceManagedGraph (axisFixtureBaseline fixture)
      afterReduction = reduceManagedGraph (axisFixtureBaseline fixture <> axisFixtureLeftAppend fixture)
  graphReductionIssues beforeReduction === []
  graphReductionIssues afterReduction === []
  case (lookupReducedAdr (axisFixtureSubject fixture) beforeReduction, lookupReducedAdr (axisFixtureSubject fixture) afterReduction) of
    (Just before, Just after) -> do
      assert (selectedAxisChanged axis before after)
      assert (unselectedAxesPreserved axis before after)
    _ -> footnote "axis isolation fixture did not materialize" >> assert False

prop_invalidChildQuarantine :: InvalidGraphFault -> Property
prop_invalidChildQuarantine fault = withTests 75 . property $ do
  spec <- forAll (InvalidGraphSpec <$> genIdentifierPool <*> pure fault)
  let fixture = materializeInvalidGraph spec
      reduction = reduceManagedGraph (invalidFixtureRecords fixture)
      issueCodes = map graphIssueCode (graphReductionIssues reduction)
  assert (invalidFixtureExpectedCode fixture `elem` issueCodes)
  case lookupReducedAdr (adrIdAt (invalidGraphPool spec) 0) reduction of
    Nothing -> footnote "invalid fixture lost its subject ADR" >> assert False
    Just adr ->
      assert (invalidFixturePreservedHead fixture `elem` headTexts (invalidFixtureAxis fixture) adr)

prop_duplicateQuarantine :: Property
prop_duplicateQuarantine = withTests 75 . property $ do
  spec <- forAll (genAxisConflictSpec (Just ScopeAxis))
  duplicateDecision <- forAll Gen.bool
  let fixture = materializeAxisConflict spec
      baseline = axisFixtureBaseline fixture
      duplicated = if duplicateDecision then firstDecision baseline else firstConnection baseline
      reduction = reduceManagedGraph (baseline <> [duplicated])
      expectedCode = if duplicateDecision then DuplicateRecordId else DuplicateConnectionId
  assert (expectedCode `elem` map graphIssueCode (graphReductionIssues reduction))
  case lookupReducedAdr (axisFixtureSubject fixture) reduction of
    Nothing -> footnote "duplicate fixture lost its subject ADR" >> assert False
    Just adr ->
      case duplicated of
        ManagedDecision decision -> assert (decisionRecord decision `notElem` axisResolutionHeads (reducedDecisionAxis adr))
        ManagedConnection connection -> assert (connectionRecordId connection `notElem` concatConnectionHeads adr)

allHeadsCanonical :: ReducedAdr -> Bool
allHeadsCanonical adr =
  and
    [ canonical (axisResolutionHeads (reducedDecisionAxis adr)),
      canonical (axisResolutionHeads (reducedScopeAxis adr)),
      canonical (axisResolutionHeads (reducedDomainAxis adr)),
      canonical (axisResolutionHeads (reducedStatusAxis adr))
    ]
  where
    canonical values = values == Set.toAscList (Set.fromList values)

expectedDomainFallback :: AxisFixture -> [Domain]
expectedDomainFallback fixture =
  Set.toAscList . Set.unions $
    [ Set.fromList (domainsEffective payload)
      | ManagedConnection connection <- axisFixtureLeftAppend fixture <> axisFixtureRightAppend fixture,
        DomainsConnection payload <- [connectionPayload connection]
    ]

statusHeadsCoverCurrentDecision :: ReducedAdr -> Bool
statusHeadsCoverCurrentDecision adr =
  not (null currentStatuses)
    && all ((== decisionHeads) . statusRecordHeads) currentStatuses
  where
    decisionHeads = axisResolutionHeads (reducedDecisionAxis adr)
    statusHeadIds = Set.fromList (axisResolutionHeads (reducedStatusAxis adr))
    currentStatuses =
      [ payload
        | connection <- reducedStatusHistory adr,
          Set.member (connectionRecordId connection) statusHeadIds,
          StatusConnection payload <- [connectionPayload connection]
      ]

selectedAxisChanged :: GraphAxis -> ReducedAdr -> ReducedAdr -> Bool
selectedAxisChanged axis before after =
  case axis of
    DecisionAxis -> reducedDecisionAxis before /= reducedDecisionAxis after
    ScopeAxis -> reducedScopeAxis before /= reducedScopeAxis after
    DomainAxis -> reducedDomainAxis before /= reducedDomainAxis after
    StatusAxis -> reducedStatusAxis before /= reducedStatusAxis after

unselectedAxesPreserved :: GraphAxis -> ReducedAdr -> ReducedAdr -> Bool
unselectedAxesPreserved selected before after =
  and
    [ selected == DecisionAxis || (reducedDecisionAxis before, reducedDecisionHistory before, reducedAmendmentHistory before) == (reducedDecisionAxis after, reducedDecisionHistory after, reducedAmendmentHistory after),
      selected == ScopeAxis || (reducedScopeAxis before, reducedScopeHistory before) == (reducedScopeAxis after, reducedScopeHistory after),
      selected == DomainAxis || (reducedDomainAxis before, reducedDomainHistory before) == (reducedDomainAxis after, reducedDomainHistory after),
      selected == StatusAxis || (reducedStatusAxis before, reducedStatusHistory before) == (reducedStatusAxis after, reducedStatusHistory after)
    ]

headTexts :: GraphAxis -> ReducedAdr -> [Text]
headTexts axis adr =
  case axis of
    DecisionAxis -> map recordIdText (axisResolutionHeads (reducedDecisionAxis adr))
    ScopeAxis -> map connectionIdText (axisResolutionHeads (reducedScopeAxis adr))
    DomainAxis -> map connectionIdText (axisResolutionHeads (reducedDomainAxis adr))
    StatusAxis -> map connectionIdText (axisResolutionHeads (reducedStatusAxis adr))

concatConnectionHeads :: ReducedAdr -> [ConnectionId]
concatConnectionHeads adr =
  axisResolutionHeads (reducedScopeAxis adr)
    <> axisResolutionHeads (reducedDomainAxis adr)
    <> axisResolutionHeads (reducedStatusAxis adr)

selectedObjectRefs :: GraphAxis -> ReducedAdr -> [ObjectRef]
selectedObjectRefs axis adr =
  case axis of
    DecisionAxis -> map recordObjectRef (axisResolutionHeads (reducedDecisionAxis adr))
    ScopeAxis -> map connectionObjectRef (axisResolutionHeads (reducedScopeAxis adr))
    DomainAxis -> map connectionObjectRef (axisResolutionHeads (reducedDomainAxis adr))
    StatusAxis -> map connectionObjectRef (axisResolutionHeads (reducedStatusAxis adr))

axisLabel :: GraphAxis -> Text
axisLabel axis =
  case axis of
    DecisionAxis -> "decision"
    ScopeAxis -> "scope"
    DomainAxis -> "domain"
    StatusAxis -> "status"

firstDecision :: [ManagedRecord] -> ManagedRecord
firstDecision records =
  case [record | record@(ManagedDecision _) <- records] of
    record : _ -> record
    [] -> error "P2-06 fixture has no decision"

firstConnection :: [ManagedRecord] -> ManagedRecord
firstConnection records =
  case [record | record@(ManagedConnection _) <- records] of
    record : _ -> record
    [] -> error "P2-06 fixture has no connection"
