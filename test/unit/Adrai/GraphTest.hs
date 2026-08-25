{-# LANGUAGE OverloadedStrings #-}

module Adrai.GraphTest (tests) where

import Adrai.Domain
  ( Domain,
    DomainRefinement,
    mkDomain,
    mkDomainRefinement,
  )
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
import Adrai.Markdown (MarkdownSections (..))
import Adrai.Scope (ScopePattern, mkScopePattern)
import Adrai.State (StateHeads (..), stateTokenForHeads)
import Adrai.Types
  ( AdrId,
    ConnectionId,
    RecordId,
    connectionIdText,
    connectionObjectRef,
    mkAdrId,
    mkConnectionId,
    mkRecordId,
    recordIdText,
    recordObjectRef,
  )
import Data.List (permutations)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

-- These unit records are deliberately fabricated around the frozen lifecycle
-- semantics; they are not represented as prototype fixture documents.
tests :: TestTree
tests =
  testGroup
    "pure managed graph reducer"
    [ testCase "creation materializes independent current axes and Markdown" creationMaterializes,
      testCase "parallel decisions conflict and an ordinary amend reconciles them" decisionForkAndReconcile,
      testCase "scope supports all transition shapes, forks, and merges" scopeModesForkMerge,
      testCase "domains refine, fork, merge, and ignore embedded historical domains" domainRefineForkMerge,
      testCase "obsolete, replacement, reactivation, and status forks retain history" statusLifecycle,
      testCase "stale singleton obsoletion is a status-only conflict" staleStatusCoverage,
      testCase "dangling and invalid children are quarantined without consuming parents" invalidChildrenAreQuarantined,
      testCase "cross-kind parents and cycles are axis-local structured issues" crossKindAndCycles,
      testCase "invalid domain refinement and replacements are diagnosed" refinementAndReplacementDiagnostics,
      testCase "cycle diagnostics use raw parent snapshots without false delta mismatches" cycleDiagnosticsUseRawSnapshots,
      testCase "compound malformed records retain every independent diagnostic" compoundMalformedDiagnostics,
      testCase "connection-only ADRs are valid replacement targets" connectionOnlyReplacementTargetsExist,
      testCase "scope and domain diagnostics retain the static taxonomy" deltaDiagnosticTaxonomy,
      testCase "semantic conflicts are typed, sorted, and exclude integrity-only zero heads" semanticConflictClassification,
      testCase "head lists and state token are sorted and compatible" sortedHeadsAndToken,
      testCase "connection histories retain sorted duplicates within their ADR and axis" connectionHistoryIndexContract,
      testCase "ADR candidate indexes preserve missing, duplicate, and ordered local records" adrCandidateIndexContract,
      testCase "representative graph reduction is invariant under every permutation" permutationInvariant
    ]

creationMaterializes :: IO ()
creationMaterializes = do
  let a = aid '1'
      d = rid '1'
      s = cid '1'
      n = cid '2'
      t = cid '3'
      embedded = domain "legacy"
      current = domain "compiler"
      records =
        [ decision a d [embedded] "Preamble\n\n# Decision\nUse event sourcing.\n# Consequences\nAuditability.",
          scopeRevision s a [] "initial" [scope "src/**"] [] [scope "src/**"],
          domainRevision n a [] "initial" [current] [] [current] [],
          statusRevision t a [] StatusActive [d] Nothing
        ]
      result = reduceManagedGraph records
      adr = reduced a result
      decisionAxis = reducedDecisionAxis adr
  graphReductionIssues result @?= []
  axisResolutionHeads decisionAxis @?= [d]
  fmap decisionRecord (axisResolutionEffective decisionAxis) @?= Just d
  axisResolutionEffective (reducedScopeAxis adr) @?= [scope "src/**"]
  axisResolutionEffective (reducedDomainAxis adr) @?= [current]
  fmap reducedStatusState (axisResolutionEffective (reducedStatusAxis adr)) @?= Just StatusActive
  reducedConflictAxes adr @?= []
  case reducedCurrentDecisions adr of
    [view] -> do
      markdownContext (currentDecisionSections view) @?= "Preamble"
      markdownDecision (currentDecisionSections view) @?= "Use event sourcing."
      markdownConsequences (currentDecisionSections view) @?= "Auditability."
    views -> fail ("expected one current decision view, got " <> show views)

decisionForkAndReconcile :: IO ()
decisionForkAndReconcile = do
  let a = aid '1'
      d0 = rid '1'
      d1 = rid '2'
      d2 = rid '3'
      d3 = rid '4'
      base = [decision a d0 [] "base", decision a d1 [] "left", decision a d2 [] "right"]
      forks = base <> [amend (cid '1') a d1 [d0], amend (cid '2') a d2 [d0]]
      forked = reduced a (reduceManagedGraph forks)
  axisResolutionHeads (reducedDecisionAxis forked) @?= [d1, d2]
  axisResolutionEffective (reducedDecisionAxis forked) @?= Nothing
  axisResolutionConflict (reducedDecisionAxis forked) @?= Just "2 decision heads"
  reducedConflictAxes forked @?= [DecisionAxis, ScopeAxis, DomainAxis, StatusAxis]
  let reconciledRecords =
        forks <> [decision a d3 [] "merged", amend (cid '3') a d3 [d1, d2]]
      reconciled = reduced a (reduceManagedGraph reconciledRecords)
  axisResolutionHeads (reducedDecisionAxis reconciled) @?= [d3]
  fmap decisionRecord (axisResolutionEffective (reducedDecisionAxis reconciled)) @?= Just d3
  length (reducedAmendmentHistory reconciled) @?= 3

scopeModesForkMerge :: IO ()
scopeModesForkMerge = do
  let a = aid '1'
      d = rid '1'
      p = scope "src/**"
      q = scope "test/**"
      r = scope "docs/**"
      s0 = cid '1'
      s1 = cid '2'
      s2 = cid '3'
      s3 = cid '4'
      s4 = cid '5'
      left = cid '6'
      right = cid '7'
      merged = cid '8'
      domainRoot = cid '9'
      statusRoot = cid 'A'
      records =
        [ decision a d [] "base",
          scopeRevision s0 a [] "initial" [p] [] [p],
          scopeRevision s1 a [s0] "expand" [q] [] [p, q],
          scopeRevision s2 a [s1] "contract" [] [p] [q],
          scopeRevision s3 a [s2] "mixed" [r] [q] [r],
          scopeRevision s4 a [s3] "replace" [p] [r] [p],
          scopeRevision left a [s4] "expand" [q] [] [p, q],
          scopeRevision right a [s4] "expand" [r] [] [p, r],
          scopeRevision merged a [left, right] "merge" [] [] [p, q, r],
          domainRevision domainRoot a [] "initial" [] [] [] [],
          statusRevision statusRoot a [] StatusActive [d] Nothing
        ]
      result = reduceManagedGraph records
      adr = reduced a result
      forked = reduced a (reduceManagedGraph (take 8 records <> drop 9 records))
  codes result @?= []
  axisResolutionHeads (reducedScopeAxis forked) @?= [left, right]
  axisResolutionEffective (reducedScopeAxis forked) @?= []
  axisResolutionConflict (reducedScopeAxis forked) @?= Just "2 scope heads"
  axisResolutionHeads (reducedScopeAxis adr) @?= [merged]
  axisResolutionEffective (reducedScopeAxis adr) @?= [r, p, q]
  length (reducedScopeHistory adr) @?= 8

domainRefineForkMerge :: IO ()
domainRefineForkMerge = do
  let a = aid '1'
      d = rid '1'
      embedded = domain "historical"
      compiler = domain "compiler"
      cache = domain "compiler.cache"
      identity = domain "identity"
      search = domain "search"
      n0 = cid '1'
      n1 = cid '2'
      n2 = cid '3'
      n3 = cid '4'
      n4 = cid '5'
      n5 = cid '6'
      left = cid '7'
      right = cid '8'
      merged = cid '9'
      scopeRoot = cid 'A'
      statusRoot = cid 'B'
      records =
        [ decision a d [embedded] "base",
          domainRevision n0 a [] "initial" [compiler] [] [compiler] [],
          domainRevision n1 a [n0] "refine" [cache] [compiler] [cache] [refinement compiler cache],
          domainRevision n2 a [n1] "expand" [identity] [] [cache, identity] [],
          domainRevision n3 a [n2] "contract" [] [identity] [cache] [],
          domainRevision n4 a [n3] "mixed" [identity] [cache] [identity] [],
          domainRevision n5 a [n4] "replace" [cache] [identity] [cache] [],
          domainRevision left a [n5] "expand" [identity] [] [cache, identity] [],
          domainRevision right a [n5] "expand" [search] [] [cache, search] [],
          domainRevision merged a [left, right] "merge" [] [] [cache, identity, search] [],
          scopeRevision scopeRoot a [] "initial" [] [] [],
          statusRevision statusRoot a [] StatusActive [d] Nothing
        ]
      result = reduceManagedGraph records
      adr = reduced a result
      forked = reduced a (reduceManagedGraph (take 9 records <> drop 10 records))
  codes result @?= []
  axisResolutionHeads (reducedDomainAxis forked) @?= [left, right]
  axisResolutionEffective (reducedDomainAxis forked) @?= [cache, identity, search]
  axisResolutionConflict (reducedDomainAxis forked) @?= Just "2 domain heads"
  axisResolutionHeads (reducedDomainAxis adr) @?= [merged]
  axisResolutionEffective (reducedDomainAxis adr) @?= [cache, identity, search]
  assertBool "embedded decision domains remain historical" (embedded `notElem` axisResolutionEffective (reducedDomainAxis adr))

statusLifecycle :: IO ()
statusLifecycle = do
  let a = aid '1'
      replacementAdr = aid '2'
      d = rid '1'
      replacementRecord = rid '2'
      t0 = cid '1'
      t1 = cid '2'
      t2 = cid '3'
      records =
        [ decision a d [] "base",
          decision replacementAdr replacementRecord [] "replacement",
          statusRevision t0 a [] StatusActive [d] Nothing,
          statusRevision t1 a [t0] StatusObsolete [d] (Just replacementAdr),
          statusRevision t2 a [t1] StatusActive [d] Nothing
        ]
      result = reduceManagedGraph records
      adr = reduced a result
  assertBool "status lifecycle has no status integrity issue" $
    all
      (`notElem` ["INVALID_INITIAL_STATUS", "ACTIVE_STATUS_HAS_REPLACEMENT", "MISSING_REPLACEMENT_ADR", "SELF_REPLACEMENT"])
      (codes result)
  axisResolutionHeads (reducedStatusAxis adr) @?= [t2]
  fmap reducedStatusState (axisResolutionEffective (reducedStatusAxis adr)) @?= Just StatusActive
  length (reducedStatusHistory adr) @?= 3
  -- A fork is a semantic conflict rather than an integrity failure.
  let fork = statusRevision (cid '4') a [t0] StatusActive [d] Nothing
      forked = reduced a (reduceManagedGraph (records <> [fork]))
  axisResolutionHeads (reducedStatusAxis forked) @?= [t2, cid '4']
  axisResolutionConflict (reducedStatusAxis forked) @?= Just "2 status heads"
  assertBool "status axis conflicts" (StatusAxis `elem` reducedConflictAxes forked)

staleStatusCoverage :: IO ()
staleStatusCoverage = do
  let a = aid '1'
      d0 = rid '1'
      d1 = rid '2'
      t0 = cid '1'
      t1 = cid '2'
      records =
        [ decision a d0 [] "base",
          decision a d1 [] "amended",
          amend (cid '3') a d1 [d0],
          statusRevision t0 a [] StatusActive [d0] Nothing,
          statusRevision t1 a [t0] StatusObsolete [d0] Nothing
        ]
      result = reduceManagedGraph records
      adr = reduced a result
  assertBool "stale coverage is a conflict, not an integrity issue" ("INVALID_INITIAL_STATUS" `notElem` codes result)
  axisResolutionHeads (reducedDecisionAxis adr) @?= [d1]
  axisResolutionHeads (reducedStatusAxis adr) @?= [t1]
  axisResolutionEffective (reducedStatusAxis adr) @?= Nothing
  axisResolutionConflict (reducedStatusAxis adr) @?= Just "obsolete status does not cover current decision heads"
  reducedConflictAxes adr @?= [ScopeAxis, DomainAxis, StatusAxis]
  fmap (map conflictCandidateAxis . adrConflictCandidates) (classifyAdrConflict adr) @?= Just [StatusAxis]
  fmap adrConflictSummaries (classifyAdrConflict adr) @?= Just ["obsolete status does not cover current decision heads"]
  let staleAxis = reducedStatusAxis adr
      recordUpdated = adr {reducedStatusAxis = staleAxis {axisResolutionConflict = Nothing}}
      malformedHistory =
        [ ConnectionRecord
            t1
            (AppliesToConnection (AppliesToPayload a [] "initial" [] [] []))
            "typed malformed history"
        ]
      malformedRecordUpdate = recordUpdated {reducedStatusHistory = malformedHistory}
  fmap adrConflictSummaries (classifyAdrConflict recordUpdated)
    @?= Just ["obsolete status does not cover current decision heads"]
  classifyAdrConflict malformedRecordUpdate @?= Nothing

invalidChildrenAreQuarantined :: IO ()
invalidChildrenAreQuarantined = do
  let a = aid '1'
      d0 = rid '1'
      d1 = rid '2'
      d2 = rid '3'
      missingRecord = rid '9'
      s0 = cid '1'
      s1 = cid '2'
      s2 = cid '5'
      missingScope = cid '9'
      p = scope "src/**"
      q = scope "test/**"
      records =
        [ decision a d0 [] "valid parent",
          decision a d1 [] "quarantined child",
          decision a d2 [] "quarantined descendant",
          amend (cid '3') a d1 [missingRecord],
          amend (cid '4') a missingRecord [d0],
          amend (cid '6') a d2 [d1],
          scopeRevision s0 a [] "initial" [p] [] [p],
          scopeRevision s1 a [missingScope] "expand" [q] [] [p, q],
          scopeRevision s2 a [s0] "expand" [] [p] []
        ]
      result = reduceManagedGraph records
      adr = reduced a result
  axisResolutionHeads (reducedDecisionAxis adr) @?= [d0]
  axisResolutionHeads (reducedScopeAxis adr) @?= [s0]
  assertBool "missing amendment parent" ("AMENDMENT_MISSING_PARENT" `elem` codes result)
  assertBool "missing amendment child" ("AMENDMENT_MISSING_CHILD" `elem` codes result)
  assertBool "invalid descendant" ("INVALID_AMENDMENT_PARENTS" `elem` codes result)
  assertBool "missing scope parent" ("SCOPE_MISSING_PARENT" `elem` codes result)
  assertBool "invalid scope delta" ("SCOPE_DELTA_MISMATCH" `elem` codes result)

crossKindAndCycles :: IO ()
crossKindAndCycles = do
  let a = aid '1'
      d0 = rid '1'
      d1 = rid '2'
      d2 = rid '3'
      n0 = cid '1'
      s0 = cid '2'
      sCycle = cid '3'
      p = scope "src/**"
      compiler = domain "compiler"
      records =
        [ decision a d0 [] "zero",
          decision a d1 [] "one",
          decision a d2 [] "two",
          amend (cid '4') a d1 [d2],
          amend (cid '5') a d2 [d1],
          domainRevision n0 a [] "initial" [compiler] [] [compiler] [],
          scopeRevision s0 a [n0] "expand" [p] [] [p],
          scopeRevision sCycle a [sCycle] "replace" [p] [p] []
        ]
      result = reduceManagedGraph records
      adr = reduced a result
  axisResolutionHeads (reducedDecisionAxis adr) @?= [d0]
  axisResolutionHeads (reducedDomainAxis adr) @?= [n0]
  assertBool "decision cycle" ("AMENDMENT_CYCLE" `elem` codes result)
  assertBool "scope cross-kind parent" ("INVALID_SCOPE_PARENTS" `elem` codes result)
  assertBool "scope self-cycle" ("SCOPE_CYCLE" `elem` codes result)
  assertBool "scope self-cycle retains local delta diagnostics" ("SCOPE_DELTA_MISMATCH" `elem` codes result)

refinementAndReplacementDiagnostics :: IO ()
refinementAndReplacementDiagnostics = do
  let a = aid '1'
      d = rid '1'
      compiler = domain "compiler"
      cache = domain "compiler.cache"
      frontend = domain "compiler.frontend"
      n0 = cid '1'
      n1 = cid '2'
      t0 = cid '3'
      t1 = cid '4'
      t2 = cid '5'
      t3 = cid '6'
      t4 = cid '7'
      t5 = cid '8'
      t6 = cid '9'
      records =
        [ decision a d [] "base",
          domainRevision n0 a [] "initial" [compiler] [] [compiler] [],
          domainRevision n1 a [n0] "refine" [cache, frontend] [compiler] [cache, frontend] [refinement compiler cache, refinement compiler frontend],
          statusRevision t0 a [] StatusActive [d] Nothing,
          statusRevision t1 a [t0] StatusObsolete [d] (Just (aid '9')),
          statusRevision t2 a [t0] StatusObsolete [d] (Just a),
          statusRevision t3 a [t0] StatusActive [d] (Just (aid '9')),
          statusRevision t4 a [t0] StatusActive [rid '9'] Nothing,
          statusRevision t5 a [t5] StatusActive [d] Nothing,
          statusRevision t6 a [] StatusActive [] Nothing
        ]
      result = reduceManagedGraph records
  assertBool "refinement mapping" ("DOMAIN_REFINEMENT_MISMATCH" `elem` codes result)
  assertBool "unknown replacement" ("MISSING_REPLACEMENT_ADR" `elem` codes result)
  assertBool "self replacement" ("SELF_REPLACEMENT" `elem` codes result)
  assertBool "active replacement" ("ACTIVE_STATUS_HAS_REPLACEMENT" `elem` codes result)
  assertBool "missing status record" ("STATUS_MISSING_RECORD" `elem` codes result)
  assertBool "status cycle" ("STATUS_CYCLE" `elem` codes result)
  assertBool "initial active coverage" ("INVALID_INITIAL_STATUS" `elem` codes result)

cycleDiagnosticsUseRawSnapshots :: IO ()
cycleDiagnosticsUseRawSnapshots = do
  let a = aid '1'
      d = rid '1'
      p = scope "src/**"
      q = scope "test/**"
      compiler = domain "compiler"
      identity = domain "identity"
      s1 = cid '1'
      s2 = cid '2'
      n1 = cid '3'
      n2 = cid '4'
      records =
        [ decision a d [] "base",
          scopeRevision s1 a [s2] "replace" [p] [q] [p],
          scopeRevision s2 a [s1] "replace" [q] [p] [q],
          domainRevision n1 a [n2] "replace" [compiler] [identity] [compiler] [],
          domainRevision n2 a [n1] "replace" [identity] [compiler] [identity] []
        ]
      issueCodes = codes (reduceManagedGraph records)
  assertBool "scope cycle" ("SCOPE_CYCLE" `elem` issueCodes)
  assertBool "domain cycle" ("DOMAIN_CYCLE" `elem` issueCodes)
  assertBool "consistent cyclic scope deltas are not misdiagnosed" ("SCOPE_DELTA_MISMATCH" `notElem` issueCodes)
  assertBool "consistent cyclic domain deltas are not misdiagnosed" ("DOMAIN_DELTA_MISMATCH" `notElem` issueCodes)

compoundMalformedDiagnostics :: IO ()
compoundMalformedDiagnostics = do
  let a = aid '1'
      replacementOwner = aid '2'
      unknownReplacement = aid '9'
      d0 = rid '1'
      d1 = rid '2'
      replacementDecision = rid '3'
      missingRecord = rid '9'
      amendment = cid '1'
      cyclicStatus = cid '2'
      initialStatus = cid '3'
      records =
        [ decision a d0 [] "zero",
          decision a d1 [] "one",
          amend amendment a d1 [d1],
          statusRevision cyclicStatus a [cyclicStatus] StatusActive [missingRecord] (Just a),
          decision replacementOwner replacementDecision [] "replacement owner",
          statusRevision initialStatus replacementOwner [] StatusActive [replacementDecision] (Just unknownReplacement)
        ]
      expected = reduceManagedGraph records
      issueCodes = codes expected
      adr = reduced a expected
  axisResolutionHeads (reducedDecisionAxis adr) @?= [d0]
  assertBool "invalid child is quarantined without consuming its valid sibling" (d0 `elem` axisResolutionHeads (reducedDecisionAxis adr))
  assertBool "self-amendment is classified as invalid parents" ("INVALID_AMENDMENT_PARENTS" `elem` issueCodes)
  assertBool "ineligible self-amendment is not admitted to cycle detection" ("AMENDMENT_CYCLE" `notElem` issueCodes)
  assertBool "status self-parent is retained" ("INVALID_STATUS_PARENTS" `elem` issueCodes)
  assertBool "status cycle is retained" ("STATUS_CYCLE" `elem` issueCodes)
  assertBool "missing covered record survives quarantine" ("STATUS_MISSING_RECORD" `elem` issueCodes)
  assertBool "active replacement survives quarantine" ("ACTIVE_STATUS_HAS_REPLACEMENT" `elem` issueCodes)
  assertBool "self replacement survives quarantine" ("SELF_REPLACEMENT" `elem` issueCodes)
  assertBool "initial replacement invalidates the initial status" ("INVALID_INITIAL_STATUS" `elem` issueCodes)
  assertBool "unknown replacement is retained" ("MISSING_REPLACEMENT_ADR" `elem` issueCodes)
  map reduceManagedGraph (permutations records) @?= replicate 720 expected

connectionOnlyReplacementTargetsExist :: IO ()
connectionOnlyReplacementTargetsExist = do
  let a = aid '1'
      target = aid '2'
      d = rid '1'
      t0 = cid '1'
      t1 = cid '2'
      targetScope = cid '3'
      p = scope "src/**"
      records =
        [ decision a d [] "base",
          statusRevision t0 a [] StatusActive [d] Nothing,
          statusRevision t1 a [t0] StatusObsolete [d] (Just target),
          scopeRevision targetScope target [] "initial" [p] [] [p]
        ]
      result = reduceManagedGraph records
  assertBool "connection-only replacement target exists" ("MISSING_REPLACEMENT_ADR" `notElem` codes result)
  fmap reducedStatusReplacement (axisResolutionEffective (reducedStatusAxis (reduced a result))) @?= Just (Just target)

deltaDiagnosticTaxonomy :: IO ()
deltaDiagnosticTaxonomy = do
  let a = aid '1'
      d = rid '1'
      p = scope "src/**"
      q = scope "test/**"
      r = scope "docs/**"
      compiler = domain "compiler"
      identity = domain "identity"
      search = domain "search"
      s0 = cid '1'
      sOther = cid '2'
      n0 = cid '3'
      nOther = cid '4'
      records =
        [ decision a d [] "base",
          scopeRevision s0 a [] "initial" [p] [] [p],
          scopeRevision sOther a [] "initial" [q] [] [q],
          scopeRevision (cid '5') a [s0] "unknown" [q] [] [p, q],
          scopeRevision (cid '6') a [s0] "expand" [] [] [p],
          scopeRevision (cid '7') a [s0] "mixed" [q] [q] [p],
          scopeRevision (cid '8') a [s0, sOther] "expand" [r] [] [p, q, r],
          scopeRevision (cid '9') a [] "replace" [r] [] [r],
          domainRevision n0 a [] "initial" [compiler] [] [compiler] [],
          domainRevision nOther a [] "initial" [identity] [] [identity] [],
          domainRevision (cid 'A') a [n0] "unknown" [identity] [] [compiler, identity] [],
          domainRevision (cid 'B') a [n0] "expand" [] [] [compiler] [],
          domainRevision (cid 'C') a [n0] "mixed" [identity] [identity] [compiler] [],
          domainRevision (cid 'D') a [n0, nOther] "expand" [search] [] [compiler, identity, search] [],
          domainRevision (cid 'E') a [] "replace" [search] [] [search] []
        ]
      issueCodes = codes (reduceManagedGraph records)
  mapM_
    (\code -> assertBool ("missing scope diagnostic " <> Text.unpack code) (code `elem` issueCodes))
    [ "INVALID_SCOPE_CHANGE_KIND",
      "SCOPE_CHANGE_SHAPE",
      "SCOPE_DELTA_OVERLAP",
      "SCOPE_MERGE_KIND",
      "SCOPE_ROOT_NOT_INITIAL"
    ]
  mapM_
    (\code -> assertBool ("missing domain diagnostic " <> Text.unpack code) (code `elem` issueCodes))
    [ "INVALID_DOMAIN_CHANGE_KIND",
      "DOMAIN_CHANGE_SHAPE",
      "DOMAIN_DELTA_OVERLAP",
      "DOMAIN_MERGE_KIND",
      "DOMAIN_ROOT_NOT_INITIAL"
    ]

semanticConflictClassification :: IO ()
semanticConflictClassification = do
  let a = aid '1'
      d1 = rid '1'
      d2 = rid '2'
      s1 = cid '1'
      s2 = cid '2'
      n1 = cid '3'
      n2 = cid '4'
      t1 = cid '5'
      t2 = cid '6'
      p = scope "src/**"
      q = scope "test/**"
      compiler = domain "compiler"
      identity = domain "identity"
      records =
        [ decision a d2 [] "two",
          statusRevision t2 a [] StatusActive [d1, d2] Nothing,
          domainRevision n2 a [] "initial" [identity] [] [identity] [],
          scopeRevision s2 a [] "initial" [q] [] [q],
          decision a d1 [] "one",
          statusRevision t1 a [] StatusActive [d1, d2] Nothing,
          domainRevision n1 a [] "initial" [compiler] [] [compiler] [],
          scopeRevision s1 a [] "initial" [p] [] [p]
        ]
      adr = reduced a (reduceManagedGraph records)
      conflict = fromJust (classifyAdrConflict adr)
      emptyAdr = reduced a (reduceManagedGraph [amend (cid '9') a (rid '9') [rid '9']])
      tamperedToken = stateTokenForHeads (StateHeads [] [] [] [])
      tamperedAdr = adr {reducedStateToken = tamperedToken}
  adrConflictCodeText @?= "ADR_CONFLICT"
  adrConflictCode conflict @?= "ADR_CONFLICT"
  adrConflictAdr conflict @?= a
  adrConflictCount conflict @?= 4
  map conflictCandidateAxis (adrConflictCandidates conflict)
    @?= [DecisionAxis, ScopeAxis, DomainAxis, StatusAxis]
  map conflictCandidateHeadCount (adrConflictCandidates conflict) @?= [2, 2, 2, 2]
  map conflictCandidateHeads (adrConflictCandidates conflict)
    @?= [ map recordObjectRef [d1, d2],
          map connectionObjectRef [s1, s2],
          map connectionObjectRef [n1, n2],
          map connectionObjectRef [t1, t2]
        ]
  adrConflictSummaries conflict @?= ["2 decision heads", "2 scope heads", "2 domain heads", "2 status heads"]
  adrConflictStateToken conflict @?= reducedStateToken adr
  classifyAdrConflict emptyAdr @?= Nothing
  adrConflictStateToken (fromJust (classifyAdrConflict tamperedAdr)) @?= stateTokenForHeads (reducedStateHeads adr)
  assertBool "classification ignores a record-updated cached token" (adrConflictStateToken conflict /= tamperedToken)
  assertBool "legacy integrity projection still records zero heads" (not (null (reducedConflictAxes emptyAdr)))

sortedHeadsAndToken :: IO ()
sortedHeadsAndToken = do
  let a = aid '1'
      d1 = rid '1'
      d2 = rid '2'
      s1 = cid '1'
      s2 = cid '2'
      n1 = cid '3'
      n2 = cid '4'
      t1 = cid '5'
      t2 = cid '6'
      p = scope "src/**"
      q = scope "test/**"
      compiler = domain "compiler"
      identity = domain "identity"
      records =
        [ statusRevision t2 a [] StatusActive [d1, d2] Nothing,
          domainRevision n2 a [] "initial" [identity] [] [identity] [],
          scopeRevision s2 a [] "initial" [q] [] [q],
          decision a d2 [] "two",
          statusRevision t1 a [] StatusActive [d1, d2] Nothing,
          domainRevision n1 a [] "initial" [compiler] [] [compiler] [],
          scopeRevision s1 a [] "initial" [p] [] [p],
          decision a d1 [] "one"
        ]
      adr = reduced a (reduceManagedGraph records)
      expected = StateHeads [d1, d2] [s1, s2] [t1, t2] [n1, n2]
  reducedStateHeads adr @?= expected
  reducedStateToken adr @?= stateTokenForHeads expected
  axisResolutionEffective (reducedDomainAxis adr) @?= [compiler, identity]
  let issueCodes = codes (reduceManagedGraph records)
  assertBool "decision multi-root cardinality" ("DECISION_ROOT_CARDINALITY" `elem` issueCodes)
  assertBool "scope multi-root cardinality" ("SCOPE_ROOT_CARDINALITY" `elem` issueCodes)
  assertBool "domain multi-root cardinality" ("DOMAIN_ROOT_CARDINALITY" `elem` issueCodes)
  assertBool "status multi-root cardinality" ("STATUS_ROOT_CARDINALITY" `elem` issueCodes)

connectionHistoryIndexContract :: IO ()
connectionHistoryIndexContract = do
  let a = aid '1'
      otherAdr = aid '2'
      decisionId = rid '1'
      scopeDuplicateZ = ConnectionRecord (cid '1') (AppliesToConnection (AppliesToPayload a [] "initial" [scope "src/**"] [] [scope "src/**"])) "z rationale"
      scopeLater = ConnectionRecord (cid '2') (AppliesToConnection (AppliesToPayload a [] "initial" [scope "test/**"] [] [scope "test/**"])) "later rationale"
      scopeDuplicateA = ConnectionRecord (cid '1') (AppliesToConnection (AppliesToPayload a [] "initial" [scope "docs/**"] [] [scope "docs/**"])) "a rationale"
      domainConnection = ConnectionRecord (cid '3') (DomainsConnection (DomainsPayload a [] "initial" [domain "compiler"] [] [domain "compiler"] [])) "domain rationale"
      statusConnection = ConnectionRecord (cid '4') (StatusConnection (StatusPayload a [] StatusActive [decisionId] Nothing)) "status rationale"
      amendmentConnection = ConnectionRecord (cid '5') (AmendsConnection (AmendsPayload a decisionId [])) "amendment rationale"
      otherScope = ConnectionRecord (cid '6') (AppliesToConnection (AppliesToPayload otherAdr [] "initial" [scope "other/**"] [] [scope "other/**"])) "other ADR rationale"
      records =
        [ ManagedConnection scopeLater,
          ManagedConnection otherScope,
          ManagedConnection scopeDuplicateZ,
          ManagedConnection statusConnection,
          ManagedConnection scopeDuplicateA,
          ManagedConnection amendmentConnection,
          ManagedConnection domainConnection,
          decision a decisionId [] "base"
        ]
      result = reduceManagedGraph records
      adr = reduced a result
  reducedScopeHistory adr @?= [scopeDuplicateA, scopeDuplicateZ, scopeLater]
  reducedAmendmentHistory adr @?= [amendmentConnection]
  reducedDomainHistory adr @?= [domainConnection]
  reducedStatusHistory adr @?= [statusConnection]
  assertBool "duplicate connection remains quarantined from current scope heads" (cid '1' `notElem` axisResolutionHeads (reducedScopeAxis adr))
  assertBool "duplicate diagnostic is retained" ("DUPLICATE_CONNECTION_ID" `elem` codes result)

adrCandidateIndexContract :: IO ()
adrCandidateIndexContract = do
  let a = aid '1'
      b = aid '2'
      aDecision = rid '1'
      bDecision = rid '2'
      missingDecision = rid '3'
      duplicateDecision = rid '9'
      aScope = cid '1'
      bScope = cid '2'
      missingChildAmendment = cid '3'
      duplicateScope = cid '9'
      records =
        [ decision b duplicateDecision [] "duplicate owned by B",
          scopeRevision duplicateScope b [] "initial" [scope "b-duplicate/**"] [] [scope "b-duplicate/**"],
          scopeRevision bScope b [] "initial" [scope "b/**"] [] [scope "b/**"],
          decision b bDecision [] "B",
          decision a duplicateDecision [] "duplicate owned by A",
          scopeRevision duplicateScope a [] "initial" [scope "a-duplicate/**"] [] [scope "a-duplicate/**"],
          amend missingChildAmendment a missingDecision [aDecision],
          scopeRevision aScope a [] "initial" [scope "a/**"] [] [scope "a/**"],
          decision a aDecision [] "A"
        ]
      result = reduceManagedGraph records
      adrA = reduced a result
      adrB = reduced b result
  map reducedAdrId (graphReductionAdrs result) @?= [a, b]
  axisResolutionHeads (reducedDecisionAxis adrA) @?= [aDecision]
  axisResolutionHeads (reducedDecisionAxis adrB) @?= [bDecision]
  axisResolutionHeads (reducedScopeAxis adrA) @?= [aScope]
  axisResolutionHeads (reducedScopeAxis adrB) @?= [bScope]
  map decisionRecord (reducedDecisionHistory adrA) @?= [aDecision, duplicateDecision]
  map decisionRecord (reducedDecisionHistory adrB) @?= [bDecision, duplicateDecision]
  map connectionRecordId (reducedScopeHistory adrA) @?= [aScope, duplicateScope]
  map connectionRecordId (reducedScopeHistory adrB) @?= [bScope, duplicateScope]
  map connectionRecordId (reducedAmendmentHistory adrA) @?= [missingChildAmendment]
  graphReductionIssues result
    @?= [ GraphIssue
            AmendmentMissingChild
            (Just a)
            (Just (connectionObjectRef missingChildAmendment))
            ("amendment child does not exist: " <> recordIdText missingDecision),
          GraphIssue
            DomainRootCardinality
            (Just a)
            Nothing
            "expected exactly 1 valid parentless domain root, found 0",
          GraphIssue
            DomainRootCardinality
            (Just b)
            Nothing
            "expected exactly 1 valid parentless domain root, found 0",
          GraphIssue
            DuplicateConnectionId
            Nothing
            (Just (connectionObjectRef duplicateScope))
            ("connection identifier " <> connectionIdText duplicateScope <> " occurs 2 times; every occurrence is quarantined"),
          GraphIssue
            DuplicateRecordId
            Nothing
            (Just (recordObjectRef duplicateDecision))
            ("record identifier " <> recordIdText duplicateDecision <> " occurs 2 times; every occurrence is quarantined"),
          GraphIssue
            StatusRootCardinality
            (Just a)
            Nothing
            "expected exactly 1 valid parentless status root, found 0",
          GraphIssue
            StatusRootCardinality
            (Just b)
            Nothing
            "expected exactly 1 valid parentless status root, found 0"
        ]
  assertCurrentProjection aDecision aScope adrA
  assertCurrentProjection bDecision bScope adrB

assertCurrentProjection :: RecordId -> ConnectionId -> ReducedAdr -> IO ()
assertCurrentProjection decisionId scopeId adr = do
  axisResolutionHeads (reducedDecisionAxis adr) @?= [decisionId]
  axisResolutionHeads (reducedScopeAxis adr) @?= [scopeId]
  axisResolutionHeads (reducedDomainAxis adr) @?= []
  axisResolutionHeads (reducedStatusAxis adr) @?= []
  map (decisionRecord . currentDecisionRecord) (reducedCurrentDecisions adr) @?= [decisionId]
  reducedCurrentConnections adr @?= [CurrentConnectionRef ScopeAxis scopeId]
  let expectedHeads = StateHeads [decisionId] [scopeId] [] []
  reducedStateHeads adr @?= expectedHeads
  reducedStateToken adr @?= stateTokenForHeads expectedHeads

permutationInvariant :: IO ()
permutationInvariant = do
  let a = aid '1'
      d0 = rid '1'
      d1 = rid '2'
      s = cid '1'
      n = cid '2'
      t = cid '3'
      records =
        [ decision a d0 [] "base",
          decision a d1 [] "amended",
          amend (cid '4') a d1 [d0],
          scopeRevision s a [cid '9'] "expand" [scope "src/**"] [] [scope "src/**"],
          domainRevision n a [] "initial" [domain "compiler"] [] [domain "compiler"] [],
          statusRevision t a [] StatusActive [d0] Nothing
        ]
      expected = reduceManagedGraph records
  assertBool "representative includes stable diagnostics" (not (null (graphReductionIssues expected)))
  map reduceManagedGraph (permutations records) @?= replicate 720 expected

decision :: AdrId -> RecordId -> [Domain] -> Text -> ManagedRecord
decision adr identifier domains body =
  ManagedDecision
    DecisionRecord
      { decisionAdr = adr,
        decisionRecord = identifier,
        decisionTitle = "title",
        decisionSummary = "summary",
        decisionDomains = domains,
        decisionBody = body
      }

amend :: ConnectionId -> AdrId -> RecordId -> [RecordId] -> ManagedRecord
amend identifier adr child parents =
  connection identifier (AmendsConnection (AmendsPayload adr child parents))

scopeRevision :: ConnectionId -> AdrId -> [ConnectionId] -> Text -> [ScopePattern] -> [ScopePattern] -> [ScopePattern] -> ManagedRecord
scopeRevision identifier adr parents change added removed effective =
  connection identifier (AppliesToConnection (AppliesToPayload adr parents change added removed effective))

domainRevision :: ConnectionId -> AdrId -> [ConnectionId] -> Text -> [Domain] -> [Domain] -> [Domain] -> [DomainRefinement] -> ManagedRecord
domainRevision identifier adr parents change added removed effective refinements =
  connection identifier (DomainsConnection (DomainsPayload adr parents change added removed effective refinements))

statusRevision :: ConnectionId -> AdrId -> [ConnectionId] -> StatusState -> [RecordId] -> Maybe AdrId -> ManagedRecord
statusRevision identifier adr parents state recordHeads replacement =
  connection identifier (StatusConnection (StatusPayload adr parents state recordHeads replacement))

connection :: ConnectionId -> ConnectionPayload -> ManagedRecord
connection identifier payload = ManagedConnection (ConnectionRecord identifier payload "reason")

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

domain :: Text -> Domain
domain = either (error . show) id . mkDomain

scope :: Text -> ScopePattern
scope = either (error . show) id . mkScopePattern

refinement :: Domain -> Domain -> DomainRefinement
refinement parent child = either (error . show) id (mkDomainRefinement parent child)

reduced :: AdrId -> GraphReduction -> ReducedAdr
reduced adr = fromJust . lookupReducedAdr adr

codes :: GraphReduction -> [Text]
codes = map (graphIssueCodeText . graphIssueCode) . graphReductionIssues
