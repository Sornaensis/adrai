{-# LANGUAGE OverloadedStrings #-}

module Adrai.ServiceTest (tests) where

import Adrai.Domain (Domain, mkDomain)
import Adrai.Format.Document
  ( AmendsPayload (..),
    AppliesToPayload (..),
    ConnectionPayload (..),
    ConnectionRecord (..),
    DecisionRecord (..),
    DomainsPayload (..),
    ManagedRecord (..),
    StatusPayload (..),
    StatusState (..),
  )
import Adrai.Graph
  ( AxisResolution (..),
    GraphAxis (..),
    GraphReduction (..),
    ReducedAdr (..),
    classifyAdrConflict,
    lookupReducedAdr,
    reduceManagedGraph,
  )
import Adrai.Scope (ScopePattern, mkScopePattern)
import Adrai.Service
import Adrai.State (StateHeads (..), stateTokenForHeads)
import Adrai.Types
  ( AdraiError (..),
    AdrId,
    ConnectionId,
    ExitClass (..),
    RecordId,
    StateToken,
    adrIdText,
    exitClassCode,
    mkAdrId,
    mkConnectionId,
    mkRecordId,
    stateTokenText,
  )
import Data.List (permutations)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "pure graph service"
    [ testCase "expected state accepts omitted and equal tokens and rejects stale with exit 3" expectedStateValidation,
      testCase "semantic conflicts map to exit 3 with stable joined summaries" semanticConflictErrorMapping,
      testCase "full-catalog source integrity blocks planning before all other validation" sourceIntegrityPrecedence,
      testCase "planner rejects complete ADRs and stale races before producing appends" conflictAndRaceValidation,
      testCase "lifecycle gates reject inactive targets and ambiguous decision domains" lifecycleGates,
      testCase "decision reconciliation appends a reviewed decision and all-head amendment" decisionPlanShape,
      testCase "scope reconciliation emits an exact union-relative merge" scopePlanShape,
      testCase "domain reconciliation supports a complete empty target set" domainPlanShape,
      testCase "status reconciliation requires explicit resolve and covers current decisions" statusPlanShape,
      testCase "obsolete status reconciliation enforces replacement rules" statusReplacementRules,
      testCase "planner rejects wrong subjects, reused IDs, and invalid decision content" plannerInputValidation,
      testCase "plans are deterministic across retries and input permutations" plannerDeterminism,
      testCase "reconciliation preserves original records and unrelated axes without new issues" appendOnlyAxisIsolation
    ]

expectedStateValidation :: IO ()
expectedStateValidation = do
  let current = tokenFor [rid '1'] [] [] []
      stale = tokenFor [] [] [] []
      expectedMessage =
        "stale ADR state: expected "
          <> stateTokenText stale
          <> ", current state is "
          <> stateTokenText current
  validateExpectedState Nothing current @?= Right ()
  validateExpectedState (Just current) current @?= Right ()
  case validateExpectedState (Just stale) current of
    Left err -> do
      adraiErrorClass err @?= ExitConflict
      exitClassCode (adraiErrorClass err) @?= 3
      adraiErrorMessage err @?= expectedMessage
    Right () -> fail "expected stale-state conflict"
  let mapped = reconciliationErrorToAdraiError (ReconciliationStaleState stale current)
  adraiErrorClass mapped @?= ExitConflict
  exitClassCode (adraiErrorClass mapped) @?= 3
  adraiErrorMessage mapped @?= expectedMessage

semanticConflictErrorMapping :: IO ()
semanticConflictErrorMapping = do
  let a = aid '1'
      adr = reduced a (reduceManagedGraph (simultaneousRecords a))
      conflict = fromJust (classifyAdrConflict adr)
      err = adrConflictToAdraiError conflict
  adraiErrorClass err @?= ExitConflict
  exitClassCode (adraiErrorClass err) @?= 3
  adraiErrorMessage err @?= "2 decision heads; 2 scope heads; 2 domain heads"

sourceIntegrityPrecedence :: IO ()
sourceIntegrityPrecedence = do
  let a = aid '1'
      b = aid '2'
      c = aid '3'
      clean = simultaneousRecords a
      stale = tokenFor [] [] [] []
      cases =
        [ clean <> [scopeRevision (cid 'N') b [cid 'P'] "expand" [scope "other/**"] [] [scope "other/**"]],
          clean <> [scopeRevision (cid 'N') b [cid 'N'] "replace" [] [] []],
          clean <> [decision b (rid '8') "duplicate", decision c (rid '8') "duplicate"]
        ]
  mapM_
    ( \records -> do
        let issues = graphReductionIssues (reduceManagedGraph records)
        assertBool "fixture contains source-integrity issues" (not (null issues))
        planReconciliation records (aid '9') stale (scopeChoice b)
          @?= Left (ReconciliationSourceIntegrity issues)
        let err = reconciliationErrorToAdraiError (ReconciliationSourceIntegrity issues)
        adraiErrorClass err @?= ExitUserError
        adraiErrorMessage err
          @?= ( "ADRAI source has "
                  <> Text.pack (show (length issues))
                  <> " integrity error(s); run 'adrai doctor' before mutating"
              )
    )
    cases

conflictAndRaceValidation :: IO ()
conflictAndRaceValidation = do
  let a = aid '1'
      complete = completeRecords a
      completeAdr = reduced a (reduceManagedGraph complete)
      noConflictChoice =
        ReconcileScope (ScopeResolution a (cid 'H') [scope "src/**"] "resolve\n")
  planReconciliation complete a (reducedStateToken completeAdr) noConflictChoice
    @?= Left (ReconciliationNoConflict ScopeAxis)
  let records = simultaneousRecords a
      current = reduced a (reduceManagedGraph records)
      stale = tokenFor [] [] [] []
      choice = scopeChoice a
  case planReconciliation records a stale choice of
    Left (ReconciliationStaleState expected actual) -> do
      expected @?= stale
      actual @?= reducedStateToken current
    other -> fail ("expected stale planner result, got " <> show other)
  let firstPlan = mustPlan records a (reducedStateToken current) choice
      advanced = records <> reconciliationAppends firstPlan
  planReconciliation advanced a (reducedStateToken current) (domainChoice a)
    @?= Left
      ( ReconciliationStaleState
          (reducedStateToken current)
          (reconciliationNextStateToken firstPlan)
      )

lifecycleGates :: IO ()
lifecycleGates = do
  let a = aid '1'
      activeDecision = decisionConflictRecords a
      noStatus = filter (not . isStatusRecord) activeDecision
      obsoleteRecords =
        noStatus
          <> [ statusRevision (cid '5') a [] StatusActive [rid '1'] Nothing,
               statusRevision (cid '6') a [cid '5'] StatusObsolete [rid '2', rid '3'] Nothing
             ]
      obsoleteAdr = reduced a (reduceManagedGraph obsoleteRecords)
      conflictedRecords =
        noStatus
          <> [ statusRevision (cid '5') a [] StatusActive [rid '1'] Nothing,
               statusRevision (cid '6') a [cid '5'] StatusActive [rid '2', rid '3'] Nothing,
               statusRevision (cid '7') a [cid '5'] StatusActive [rid '2', rid '3'] Nothing
             ]
      conflictedAdr = reduced a (reduceManagedGraph conflictedRecords)
      domainAmbiguous = simultaneousRecords a
      domainAmbiguousAdr = reduced a (reduceManagedGraph domainAmbiguous)
      inactiveMultiRecords =
        filter (not . isStatusRecord) domainAmbiguous
          <> [ statusRevision (cid '9') a [] StatusActive [rid '1'] Nothing,
               statusRevision (cid 'A') a [cid '9'] StatusObsolete [rid '2', rid '3'] Nothing
             ]
      inactiveMultiAdr = reduced a (reduceManagedGraph inactiveMultiRecords)
      decisionChoice = decisionChoiceFor a
  assertPlannerConflict
    (ReconciliationTargetNotActive DecisionAxis a "obsolete")
    (adrText a <> " status is obsolete; only active ADRs can be amended")
    (planReconciliation obsoleteRecords a (reducedStateToken obsoleteAdr) decisionChoice)
  assertPlannerConflict
    (ReconciliationTargetNotActive DecisionAxis a "conflict")
    (adrText a <> " status is conflict; only active ADRs can be amended")
    (planReconciliation conflictedRecords a (reducedStateToken conflictedAdr) decisionChoice)
  assertPlannerConflict
    (ReconciliationDecisionDomainConflict a 2)
    ( adrText a
        <> " has 2 domain heads; reconcile them with 'adrai domain --set ...' before amending decision text"
    )
    (planReconciliation domainAmbiguous a (reducedStateToken domainAmbiguousAdr) decisionChoice)
  assertPlannerConflict
    (ReconciliationTargetNotActive DomainAxis a "obsolete")
    (adrText a <> " status is obsolete; only active ADRs can change domains")
    (planReconciliation inactiveMultiRecords a (reducedStateToken inactiveMultiAdr) (domainChoice a))
  assertPlannerConflict
    (ReconciliationTargetNotActive ScopeAxis a "obsolete")
    (adrText a <> " status is obsolete; only active ADRs can change scope")
    (planReconciliation inactiveMultiRecords a (reducedStateToken inactiveMultiAdr) (scopeChoice a))

decisionPlanShape :: IO ()
decisionPlanShape = do
  let a = aid '1'
      records = decisionConflictRecords a
      current = reduced a (reduceManagedGraph records)
      callerDecision = resolvedDecision a (rid '4')
      newDecision = callerDecision {decisionDomains = [domain "compiler"]}
      choice =
        ReconcileDecision
          DecisionResolution
            { decisionResolutionSubject = a,
              decisionResolutionDecision = callerDecision,
              decisionResolutionAmendmentId = cid '6',
              decisionResolutionRationale = "reconcile decision heads\n"
            }
      plan = mustPlan records a (reducedStateToken current) choice
  reconciliationAppends plan
    @?= [ ManagedDecision newDecision,
          connection
            (cid '6')
            (AmendsConnection (AmendsPayload a (rid '4') [rid '2', rid '3']))
            "reconcile decision heads\n"
        ]
  axisResolutionHeads (reducedDecisionAxis (reconciliationTargetResult plan)) @?= [rid '4']
  axisResolutionHeads (reducedStatusAxis (reconciliationTargetResult plan))
    @?= axisResolutionHeads (reducedStatusAxis current)
  assertBool "state token advances" (reconciliationNextStateToken plan /= reducedStateToken current)

scopePlanShape :: IO ()
scopePlanShape = do
  let a = aid '1'
      records = simultaneousRecords a
      current = reduced a (reduceManagedGraph records)
      plan = mustPlan records a (reducedStateToken current) (scopeChoice a)
      expected =
        AppliesToPayload
          { appliesToSubjectAdr = a,
            appliesToParentConnections = [cid '4', cid '5'],
            appliesToChange = "merge",
            appliesToAdded = [],
            appliesToRemoved = [scope "test/**"],
            appliesToEffective = [scope "docs/**", scope "src/**"]
          }
  reconciliationAppends plan
    @?= [connection (cid 'D') (AppliesToConnection expected) "reconcile scope heads\n"]
  axisResolutionHeads (reducedScopeAxis (reconciliationTargetResult plan)) @?= [cid 'D']
  axisResolutionEffective (reducedScopeAxis (reconciliationTargetResult plan))
    @?= [scope "docs/**", scope "src/**"]

domainPlanShape :: IO ()
domainPlanShape = do
  let a = aid '1'
      records = simultaneousRecords a
      current = reduced a (reduceManagedGraph records)
      plan = mustPlan records a (reducedStateToken current) (domainChoice a)
      expected =
        DomainsPayload
          { domainsSubjectAdr = a,
            domainsParentConnections = [cid '7', cid '8'],
            domainsChange = "merge",
            domainsAdded = [],
            domainsRemoved = [domain "compiler", domain "identity", domain "search"],
            domainsEffective = [],
            domainsRefinements = []
          }
  reconciliationAppends plan
    @?= [connection (cid 'E') (DomainsConnection expected) "reconcile domain heads\n"]
  axisResolutionHeads (reducedDomainAxis (reconciliationTargetResult plan)) @?= [cid 'E']
  axisResolutionEffective (reducedDomainAxis (reconciliationTargetResult plan)) @?= []

statusPlanShape :: IO ()
statusPlanShape = do
  let a = aid '1'
      records = statusConflictRecords a
      current = reduced a (reduceManagedGraph records)
      implicit = statusChoice a False StatusActive Nothing
  planReconciliation records a (reducedStateToken current) implicit
    @?= Left ReconciliationStatusRequiresExplicitResolution
  let explicitErr = reconciliationErrorToAdraiError ReconciliationStatusRequiresExplicitResolution
  adraiErrorClass explicitErr @?= ExitConflict
  exitClassCode (adraiErrorClass explicitErr) @?= 3
  let plan = mustPlan records a (reducedStateToken current) (statusChoice a True StatusActive Nothing)
      expected = StatusPayload a [cid 'A', cid 'B'] StatusActive [rid '1'] Nothing
  reconciliationAppends plan
    @?= [connection (cid 'F') (StatusConnection expected) "reconcile status heads\n"]
  axisResolutionHeads (reducedStatusAxis (reconciliationTargetResult plan)) @?= [cid 'F']
  let staleRecords =
        [ decision a (rid '1') "base",
          decision a (rid '2') "amended",
          amend (cid '1') a (rid '2') [rid '1'],
          statusRevision (cid '2') a [] StatusActive [rid '1'] Nothing,
          statusRevision (cid '3') a [cid '2'] StatusObsolete [rid '1'] Nothing,
          scopeRevision (cid '4') a [] "initial" [] [] [],
          domainRevision (cid '5') a [] "initial" [] [] []
        ]
      staleAdr = reduced a (reduceManagedGraph staleRecords)
      stalePlan =
        mustPlan staleRecords a (reducedStateToken staleAdr) (statusChoice a True StatusActive Nothing)
      staleExpected = StatusPayload a [cid '3'] StatusActive [rid '2'] Nothing
  reconciliationAppends stalePlan
    @?= [connection (cid 'F') (StatusConnection staleExpected) "reconcile status heads\n"]

statusReplacementRules :: IO ()
statusReplacementRules = do
  let a = aid '1'
      replacement = aid '2'
      records = statusConflictRecords a <> replacementRecords replacement
      current = reduced a (reduceManagedGraph records)
      obsolete = statusChoice a True StatusObsolete (Just replacement)
      plan = mustPlan records a (reducedStateToken current) obsolete
      expected = StatusPayload a [cid 'A', cid 'B'] StatusObsolete [rid '1'] (Just replacement)
  reconciliationAppends plan
    @?= [connection (cid 'F') (StatusConnection expected) "reconcile status heads\n"]
  planReconciliation records a (reducedStateToken current) (statusChoice a True StatusActive (Just replacement))
    @?= Left (ReconciliationInvalidStatus "active status may not name a replacement ADR")
  planReconciliation records a (reducedStateToken current) (statusChoice a True StatusObsolete (Just a))
    @?= Left (ReconciliationInvalidStatus "an ADR cannot replace itself")
  planReconciliation records a (reducedStateToken current) (statusChoice a True StatusObsolete (Just (aid '9')))
    @?= Left (ReconciliationInvalidStatus ("replacement ADR does not exist: " <> aidText '9'))
  adraiErrorClass (reconciliationErrorToAdraiError (ReconciliationInvalidStatus "an ADR cannot replace itself"))
    @?= ExitUserError
  let inactiveRecords = statusConflictRecords a <> inactiveReplacementRecords replacement
      inactiveCurrent = reduced a (reduceManagedGraph inactiveRecords)
      inactiveError = ReconciliationReplacementNotActive replacement
  planReconciliation inactiveRecords a (reducedStateToken inactiveCurrent) obsolete
    @?= Left inactiveError
  assertConflictMapping
    inactiveError
    ("replacement ADR " <> adrText replacement <> " is not unambiguously active")
  let conflictedRecords = statusConflictRecords a <> conflictedReplacementRecords replacement
      conflictedCurrent = reduced a (reduceManagedGraph conflictedRecords)
  planReconciliation conflictedRecords a (reducedStateToken conflictedCurrent) obsolete
    @?= Left inactiveError
  assertConflictMapping
    inactiveError
    ("replacement ADR " <> adrText replacement <> " is not unambiguously active")

plannerInputValidation :: IO ()
plannerInputValidation = do
  let a = aid '1'
      b = aid '2'
      records = simultaneousRecords a
      current = reduced a (reduceManagedGraph records)
      token = reducedStateToken current
      decisionRecords = decisionConflictRecords a
      decisionToken = reducedStateToken (reduced a (reduceManagedGraph decisionRecords))
      wrong = ReconcileScope (ScopeResolution b (cid 'D') [] "resolve\n")
      reused = ReconcileScope (ScopeResolution a (cid '4') [] "resolve\n")
      invalidDecision =
        ReconcileDecision
          (DecisionResolution a ((resolvedDecision a (rid '4')) {decisionBody = "not canonical"}) (cid 'C') "resolve\n")
      wrongDecision =
        ReconcileDecision
          (DecisionResolution a (resolvedDecision b (rid '4')) (cid 'C') "resolve\n")
  planReconciliation records a token wrong @?= Left (ReconciliationWrongSubject a b)
  planReconciliation records a token reused @?= Left (ReconciliationReusedConnectionId (cid '4'))
  case planReconciliation decisionRecords a decisionToken invalidDecision of
    Left (ReconciliationInvalidDecision _) -> pure ()
    other -> fail ("expected invalid decision, got " <> show other)
  planReconciliation decisionRecords a decisionToken wrongDecision @?= Left (ReconciliationWrongSubject a b)

plannerDeterminism :: IO ()
plannerDeterminism = do
  let a = aid '1'
      records = simultaneousRecords a
      current = reduced a (reduceManagedGraph records)
      choice = scopeChoice a
      first = planReconciliation records a (reducedStateToken current) choice
  first @?= planReconciliation records a (reducedStateToken current) choice
  let p = scope "src/**"
      q = scope "test/**"
      small =
        [ decision a (rid '1') "base",
          scopeRevision (cid '1') a [] "initial" [p] [] [p],
          scopeRevision (cid '2') a [cid '1'] "expand" [q] [] [p, q],
          scopeRevision (cid '3') a [cid '1'] "replace" [] [] [p],
          domainRevision (cid '5') a [] "initial" [] [] [],
          statusRevision (cid '6') a [] StatusActive [rid '1'] Nothing
        ]
      smallCurrent = reduced a (reduceManagedGraph small)
      smallChoice = ReconcileScope (ScopeResolution a (cid '4') [p] "resolve\n")
      expected = planReconciliation small a (reducedStateToken smallCurrent) smallChoice
  map (\permutation -> planReconciliation permutation a (reducedStateToken smallCurrent) smallChoice) (permutations small)
    @?= replicate 720 expected
  let decisionRecords = decisionConflictRecords a
      decisionToken = reducedStateToken (reduced a (reduceManagedGraph decisionRecords))
      domainRecords = simultaneousRecords a
      domainToken = reducedStateToken (reduced a (reduceManagedGraph domainRecords))
      statusRecords = statusConflictRecords a
      statusToken = reducedStateToken (reduced a (reduceManagedGraph statusRecords))
  planReconciliation (reverse decisionRecords) a decisionToken (decisionChoiceFor a)
    @?= planReconciliation decisionRecords a decisionToken (decisionChoiceFor a)
  planReconciliation (reverse domainRecords) a domainToken (domainChoice a)
    @?= planReconciliation domainRecords a domainToken (domainChoice a)
  planReconciliation (reverse statusRecords) a statusToken (statusChoice a True StatusActive Nothing)
    @?= planReconciliation statusRecords a statusToken (statusChoice a True StatusActive Nothing)

appendOnlyAxisIsolation :: IO ()
appendOnlyAxisIsolation = do
  let a = aid '1'
      records = simultaneousRecords a
      beforeReduction = reduceManagedGraph records
      before = reduced a beforeReduction
      plan = mustPlan records a (reducedStateToken before) (scopeChoice a)
      after = reconciliationTargetResult plan
      combined = records <> reconciliationAppends plan
  take (length records) combined @?= records
  drop (length records) combined @?= reconciliationAppends plan
  graphReductionIssues beforeReduction @?= []
  graphReductionIssues (reconciliationResult plan) @?= []
  reducedDecisionAxis after @?= reducedDecisionAxis before
  reducedDomainAxis after @?= reducedDomainAxis before
  reducedStatusAxis after @?= reducedStatusAxis before
  reducedDecisionHistory after @?= reducedDecisionHistory before
  reducedAmendmentHistory after @?= reducedAmendmentHistory before
  reducedDomainHistory after @?= reducedDomainHistory before
  reducedStatusHistory after @?= reducedStatusHistory before
  length (reducedScopeHistory after) @?= length (reducedScopeHistory before) + 1
  assertBool "new scope ID is absent from originals" (cid 'D' `notElem` originalConnectionIds records)

simultaneousRecords :: AdrId -> [ManagedRecord]
simultaneousRecords a =
  let d0 = rid '1'
      d1 = rid '2'
      d2 = rid '3'
      p = scope "src/**"
      q = scope "test/**"
      r = scope "docs/**"
      compiler = domain "compiler"
      identity = domain "identity"
      search = domain "search"
   in [ decision a d0 "base",
        decision a d1 "left",
        decision a d2 "right",
        amend (cid '1') a d1 [d0],
        amend (cid '2') a d2 [d0],
        scopeRevision (cid '3') a [] "initial" [p] [] [p],
        scopeRevision (cid '4') a [cid '3'] "expand" [q] [] [p, q],
        scopeRevision (cid '5') a [cid '3'] "expand" [r] [] [p, r],
        domainRevision (cid '6') a [] "initial" [compiler] [] [compiler],
        domainRevision (cid '7') a [cid '6'] "expand" [identity] [] [compiler, identity],
        domainRevision (cid '8') a [cid '6'] "expand" [search] [] [compiler, search],
        statusRevision (cid '9') a [] StatusActive [d0] Nothing
      ]

decisionConflictRecords :: AdrId -> [ManagedRecord]
decisionConflictRecords a =
  let d0 = rid '1'
      d1 = rid '2'
      d2 = rid '3'
   in [ decision a d0 "base",
        decision a d1 "left",
        decision a d2 "right",
        amend (cid '1') a d1 [d0],
        amend (cid '2') a d2 [d0],
        scopeRevision (cid '3') a [] "initial" [scope "src/**"] [] [scope "src/**"],
        domainRevision (cid '4') a [] "initial" [domain "compiler"] [] [domain "compiler"],
        statusRevision (cid '5') a [] StatusActive [d0] Nothing
      ]

statusConflictRecords :: AdrId -> [ManagedRecord]
statusConflictRecords a =
  let d = rid '1'
   in [ decision a d "base",
        scopeRevision (cid '3') a [] "initial" [scope "src/**"] [] [scope "src/**"],
        domainRevision (cid '6') a [] "initial" [domain "compiler"] [] [domain "compiler"],
        statusRevision (cid '9') a [] StatusActive [d] Nothing,
        statusRevision (cid 'A') a [cid '9'] StatusActive [d] Nothing,
        statusRevision (cid 'B') a [cid '9'] StatusActive [d] Nothing
      ]

completeRecords :: AdrId -> [ManagedRecord]
completeRecords a =
  let d = rid '1'
   in [ decision a d "complete",
        scopeRevision (cid '1') a [] "initial" [scope "src/**"] [] [scope "src/**"],
        domainRevision (cid '2') a [] "initial" [domain "compiler"] [] [domain "compiler"],
        statusRevision (cid '3') a [] StatusActive [d] Nothing
      ]

replacementRecords :: AdrId -> [ManagedRecord]
replacementRecords a =
  let d = rid '9'
   in [ decision a d "replacement",
        scopeRevision (cid 'K') a [] "initial" [] [] [],
        domainRevision (cid 'M') a [] "initial" [] [] [],
        statusRevision (cid 'G') a [] StatusActive [d] Nothing
      ]

inactiveReplacementRecords :: AdrId -> [ManagedRecord]
inactiveReplacementRecords a =
  let d = rid '9'
   in [ decision a d "replacement",
        scopeRevision (cid 'K') a [] "initial" [] [] [],
        domainRevision (cid 'M') a [] "initial" [] [] [],
        statusRevision (cid 'G') a [] StatusActive [d] Nothing,
        statusRevision (cid 'H') a [cid 'G'] StatusObsolete [d] Nothing
      ]

conflictedReplacementRecords :: AdrId -> [ManagedRecord]
conflictedReplacementRecords a =
  let d = rid '9'
   in [ decision a d "replacement",
        scopeRevision (cid 'K') a [] "initial" [] [] [],
        domainRevision (cid 'M') a [] "initial" [] [] [],
        statusRevision (cid 'G') a [] StatusActive [d] Nothing,
        statusRevision (cid 'H') a [cid 'G'] StatusActive [d] Nothing,
        statusRevision (cid 'J') a [cid 'G'] StatusActive [d] Nothing
      ]

scopeChoice :: AdrId -> ReconciliationChoice
scopeChoice a =
  ReconcileScope
    ScopeResolution
      { scopeResolutionSubject = a,
        scopeResolutionConnectionId = cid 'D',
        scopeResolutionEffective = [scope "src/**", scope "docs/**"],
        scopeResolutionRationale = "reconcile scope heads\n"
      }

domainChoice :: AdrId -> ReconciliationChoice
domainChoice a =
  ReconcileDomain
    DomainResolution
      { domainResolutionSubject = a,
        domainResolutionConnectionId = cid 'E',
        domainResolutionEffective = [],
        domainResolutionRationale = "reconcile domain heads\n"
      }

decisionChoiceFor :: AdrId -> ReconciliationChoice
decisionChoiceFor a =
  ReconcileDecision
    DecisionResolution
      { decisionResolutionSubject = a,
        decisionResolutionDecision = resolvedDecision a (rid '4'),
        decisionResolutionAmendmentId = cid '6',
        decisionResolutionRationale = "reconcile decision heads\n"
      }

statusChoice :: AdrId -> Bool -> StatusState -> Maybe AdrId -> ReconciliationChoice
statusChoice a explicit state replacement =
  ReconcileStatus
    StatusResolution
      { statusResolutionSubject = a,
        statusResolutionConnectionId = cid 'F',
        statusResolutionState = state,
        statusResolutionReplacement = replacement,
        statusResolutionRationale = "reconcile status heads\n",
        statusResolutionExplicit = explicit
      }

resolvedDecision :: AdrId -> RecordId -> DecisionRecord
resolvedDecision a identifier =
  DecisionRecord
    { decisionAdr = a,
      decisionRecord = identifier,
      decisionTitle = "Resolved decision",
      decisionSummary = "Reconciles all current decision heads.",
      decisionDomains = [],
      decisionBody = "Reviewed complete decision.\n"
    }

decision :: AdrId -> RecordId -> Text -> ManagedRecord
decision a identifier label =
  ManagedDecision (DecisionRecord a identifier label label [] label)

amend :: ConnectionId -> AdrId -> RecordId -> [RecordId] -> ManagedRecord
amend identifier a child parents =
  connection identifier (AmendsConnection (AmendsPayload a child parents)) "fixture"

scopeRevision :: ConnectionId -> AdrId -> [ConnectionId] -> Text -> [ScopePattern] -> [ScopePattern] -> [ScopePattern] -> ManagedRecord
scopeRevision identifier a parents change added removed effective =
  connection identifier (AppliesToConnection (AppliesToPayload a parents change added removed effective)) "fixture"

domainRevision :: ConnectionId -> AdrId -> [ConnectionId] -> Text -> [Domain] -> [Domain] -> [Domain] -> ManagedRecord
domainRevision identifier a parents change added removed effective =
  connection identifier (DomainsConnection (DomainsPayload a parents change added removed effective [])) "fixture"

statusRevision :: ConnectionId -> AdrId -> [ConnectionId] -> StatusState -> [RecordId] -> Maybe AdrId -> ManagedRecord
statusRevision identifier a parents state heads replacement =
  connection identifier (StatusConnection (StatusPayload a parents state heads replacement)) "fixture"

connection :: ConnectionId -> ConnectionPayload -> Text -> ManagedRecord
connection identifier payload rationale = ManagedConnection (ConnectionRecord identifier payload rationale)

originalConnectionIds :: [ManagedRecord] -> [ConnectionId]
originalConnectionIds records = [connectionRecordId record | ManagedConnection record <- records]

isStatusRecord :: ManagedRecord -> Bool
isStatusRecord (ManagedConnection ConnectionRecord {connectionPayload = StatusConnection _}) = True
isStatusRecord _ = False

assertPlannerConflict :: ReconciliationError -> Text -> Either ReconciliationError ReconciliationPlan -> IO ()
assertPlannerConflict expected expectedMessage actual = do
  actual @?= Left expected
  assertConflictMapping expected expectedMessage

assertConflictMapping :: ReconciliationError -> Text -> IO ()
assertConflictMapping plannerError expectedMessage = do
  let err = reconciliationErrorToAdraiError plannerError
  adraiErrorClass err @?= ExitConflict
  exitClassCode (adraiErrorClass err) @?= 3
  adraiErrorMessage err @?= expectedMessage

mustPlan :: [ManagedRecord] -> AdrId -> StateToken -> ReconciliationChoice -> ReconciliationPlan
mustPlan records a token choice =
  either (error . show) id (planReconciliation records a token choice)

reduced :: AdrId -> GraphReduction -> ReducedAdr
reduced a = fromJust . lookupReducedAdr a

tokenFor :: [RecordId] -> [ConnectionId] -> [ConnectionId] -> [ConnectionId] -> StateToken
tokenFor records scopes statuses domains = stateTokenForHeads (StateHeads records scopes statuses domains)

aid :: Char -> AdrId
aid = typedId mkAdrId 'A'

rid :: Char -> RecordId
rid = typedId mkRecordId 'R'

cid :: Char -> ConnectionId
cid = typedId mkConnectionId 'C'

typedId :: (Text -> Either error value) -> Char -> Char -> value
typedId constructor prefix final =
  either (const (error "invalid test identifier")) id
    (constructor (Text.singleton prefix <> Text.replicate 25 "0" <> Text.singleton final))

aidText :: Char -> Text
aidText final = "A" <> Text.replicate 25 "0" <> Text.singleton final

adrText :: AdrId -> Text
adrText = adrIdText

domain :: Text -> Domain
domain = either (error . show) id . mkDomain

scope :: Text -> ScopePattern
scope = either (error . show) id . mkScopePattern
