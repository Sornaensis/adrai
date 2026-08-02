{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Pure, deterministic reduction of immutable managed records into the four
-- independent semantic axes of an ADR.
module Adrai.Graph
  ( GraphReduction (..),
    ReducedAdr (..),
    GraphAxis (..),
    AxisResolution (..),
    CurrentDecisionView (..),
    ReducedStatus (..),
    GraphIssue (..),
    GraphIssueCode (..),
    graphIssueCodeText,
    reduceManagedGraph,
    lookupReducedAdr,
  )
where

import Adrai.Domain
  ( Domain,
    DomainRefinement,
    domainRefinementChild,
    domainRefinementParent,
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
import Adrai.Markdown (MarkdownSections, extractMarkdownSections)
import Adrai.Scope (ScopePattern)
import Adrai.State (StateHeads (..), stateTokenForHeads)
import Adrai.Types
  ( AdrId,
    ConnectionId,
    ObjectRef,
    RecordId,
    StateToken,
    adrIdText,
    connectionIdText,
    connectionObjectRef,
    objectRefText,
    recordIdText,
    recordObjectRef,
  )
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (sort, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

-- | The axes are deliberately ordered.  This order is used by conflict
-- summaries and is part of the reducer's deterministic output.
data GraphAxis
  = DecisionAxis
  | ScopeAxis
  | DomainAxis
  | StatusAxis
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | One axis projection.  A conflict is semantic state, not an integrity
-- issue.  Some conflicted axes still have a useful conservative projection
-- (notably the union of domain-head values).
data AxisResolution identifier effective = AxisResolution
  { axisResolutionHeads :: [identifier],
    axisResolutionEffective :: effective,
    axisResolutionConflict :: Maybe Text
  }
  deriving (Eq, Show)

data CurrentDecisionView = CurrentDecisionView
  { currentDecisionRecord :: DecisionRecord,
    currentDecisionSections :: MarkdownSections
  }
  deriving (Eq, Show)

data ReducedStatus = ReducedStatus
  { reducedStatusState :: StatusState,
    reducedStatusRecordHeads :: [RecordId],
    reducedStatusReplacement :: Maybe AdrId
  }
  deriving (Eq, Show)

-- | A materialized ADR.  Histories retain every supplied local immutable
-- record, including records which were quarantined from current-head
-- computation.  This keeps obsolete/reactivate and other audit trails visible.
data ReducedAdr = ReducedAdr
  { reducedAdrId :: AdrId,
    reducedDecisionAxis :: AxisResolution RecordId (Maybe DecisionRecord),
    reducedScopeAxis :: AxisResolution ConnectionId [ScopePattern],
    reducedDomainAxis :: AxisResolution ConnectionId [Domain],
    reducedStatusAxis :: AxisResolution ConnectionId (Maybe ReducedStatus),
    reducedCurrentDecisions :: [CurrentDecisionView],
    reducedDecisionHistory :: [DecisionRecord],
    reducedAmendmentHistory :: [ConnectionRecord],
    reducedScopeHistory :: [ConnectionRecord],
    reducedDomainHistory :: [ConnectionRecord],
    reducedStatusHistory :: [ConnectionRecord],
    reducedConflictAxes :: [GraphAxis],
    reducedConflictMessages :: [Text],
    reducedStateHeads :: StateHeads,
    reducedStateToken :: StateToken
  }
  deriving (Eq, Show)

data GraphReduction = GraphReduction
  { graphReductionAdrs :: [ReducedAdr],
    graphReductionIssues :: [GraphIssue]
  }
  deriving (Eq, Show)

data GraphIssueCode
  = DuplicateRecordId
  | DuplicateConnectionId
  | DecisionRootCardinality
  | InvalidAmendmentParents
  | AmendmentMissingChild
  | AmendmentMissingParent
  | AmendmentWithoutParents
  | AmendmentCycle
  | InvalidScopeParents
  | ScopeRootCardinality
  | ScopeMissingParent
  | ScopeCycle
  | InvalidScopeChangeKind
  | ScopeChangeShape
  | ScopeDeltaOverlap
  | ScopeMergeKind
  | ScopeDeltaMismatch
  | ScopeRootNotInitial
  | InvalidDomainParents
  | DomainRootCardinality
  | DomainMissingParent
  | DomainCycle
  | InvalidDomainChangeKind
  | DomainChangeShape
  | DomainDeltaOverlap
  | DomainMergeKind
  | DomainDeltaMismatch
  | DomainRootNotInitial
  | InvalidDomainRefinement
  | InvalidStatusParents
  | StatusRootCardinality
  | StatusMissingParent
  | StatusMissingRecord
  | StatusCycle
  | InvalidStatusInitial
  | ActiveStatusHasReplacement
  | StatusMissingReplacement
  | StatusSelfReplacement
  deriving (Eq, Ord, Show, Enum, Bounded)

graphIssueCodeText :: GraphIssueCode -> Text
graphIssueCodeText code =
  case code of
    DuplicateRecordId -> "DUPLICATE_RECORD_ID"
    DuplicateConnectionId -> "DUPLICATE_CONNECTION_ID"
    DecisionRootCardinality -> "DECISION_ROOT_CARDINALITY"
    InvalidAmendmentParents -> "INVALID_AMENDMENT_PARENTS"
    AmendmentMissingChild -> "AMENDMENT_MISSING_CHILD"
    AmendmentMissingParent -> "AMENDMENT_MISSING_PARENT"
    AmendmentWithoutParents -> "AMENDMENT_WITHOUT_PARENTS"
    AmendmentCycle -> "AMENDMENT_CYCLE"
    InvalidScopeParents -> "INVALID_SCOPE_PARENTS"
    ScopeRootCardinality -> "SCOPE_ROOT_CARDINALITY"
    ScopeMissingParent -> "SCOPE_MISSING_PARENT"
    ScopeCycle -> "SCOPE_CYCLE"
    InvalidScopeChangeKind -> "INVALID_SCOPE_CHANGE_KIND"
    ScopeChangeShape -> "SCOPE_CHANGE_SHAPE"
    ScopeDeltaOverlap -> "SCOPE_DELTA_OVERLAP"
    ScopeMergeKind -> "SCOPE_MERGE_KIND"
    ScopeDeltaMismatch -> "SCOPE_DELTA_MISMATCH"
    ScopeRootNotInitial -> "SCOPE_ROOT_NOT_INITIAL"
    InvalidDomainParents -> "INVALID_DOMAIN_PARENTS"
    DomainRootCardinality -> "DOMAIN_ROOT_CARDINALITY"
    DomainMissingParent -> "DOMAIN_MISSING_PARENT"
    DomainCycle -> "DOMAIN_CYCLE"
    InvalidDomainChangeKind -> "INVALID_DOMAIN_CHANGE_KIND"
    DomainChangeShape -> "DOMAIN_CHANGE_SHAPE"
    DomainDeltaOverlap -> "DOMAIN_DELTA_OVERLAP"
    DomainMergeKind -> "DOMAIN_MERGE_KIND"
    DomainDeltaMismatch -> "DOMAIN_DELTA_MISMATCH"
    DomainRootNotInitial -> "DOMAIN_ROOT_NOT_INITIAL"
    InvalidDomainRefinement -> "DOMAIN_REFINEMENT_MISMATCH"
    InvalidStatusParents -> "INVALID_STATUS_PARENTS"
    StatusRootCardinality -> "STATUS_ROOT_CARDINALITY"
    StatusMissingParent -> "STATUS_MISSING_PARENT"
    StatusMissingRecord -> "STATUS_MISSING_RECORD"
    StatusCycle -> "STATUS_CYCLE"
    InvalidStatusInitial -> "INVALID_INITIAL_STATUS"
    ActiveStatusHasReplacement -> "ACTIVE_STATUS_HAS_REPLACEMENT"
    StatusMissingReplacement -> "MISSING_REPLACEMENT_ADR"
    StatusSelfReplacement -> "SELF_REPLACEMENT"

data GraphIssue = GraphIssue
  { graphIssueCode :: GraphIssueCode,
    graphIssueAdr :: Maybe AdrId,
    graphIssueObject :: Maybe ObjectRef,
    graphIssueMessage :: Text
  }
  deriving (Eq, Show)

lookupReducedAdr :: AdrId -> GraphReduction -> Maybe ReducedAdr
lookupReducedAdr adr = findAdr . graphReductionAdrs
  where
    findAdr [] = Nothing
    findAdr (candidate : remaining)
      | reducedAdrId candidate == adr = Just candidate
      | otherwise = findAdr remaining

-- Internal catalog ------------------------------------------------------------

data ConnectionAxis
  = AmendmentConnectionAxis
  | ScopeConnectionAxis
  | DomainConnectionAxis
  | StatusConnectionAxis
  deriving (Eq, Ord, Show)

data Catalog = Catalog
  { catalogDecisionLists :: Map RecordId [DecisionRecord],
    catalogConnectionLists :: Map ConnectionId [ConnectionRecord],
    catalogUniqueDecisions :: Map RecordId DecisionRecord,
    catalogUniqueConnections :: Map ConnectionId ConnectionRecord,
    catalogKnownAdrs :: Set AdrId
  }

reduceManagedGraph :: [ManagedRecord] -> GraphReduction
reduceManagedGraph managed =
  GraphReduction reducedAdrs (sortGraphIssues (catalogIssues <> concat adrIssues))
  where
    catalog = buildCatalog managed
    catalogIssues = duplicateIssues catalog
    adrs = Set.toAscList (managedAdrs managed)
    reducedPairs = map (reduceAdr catalog) adrs
    reducedAdrs = map fst reducedPairs
    adrIssues = map snd reducedPairs

buildCatalog :: [ManagedRecord] -> Catalog
buildCatalog managed =
  Catalog
    { catalogDecisionLists = decisionLists,
      catalogConnectionLists = connectionLists,
      catalogUniqueDecisions = Map.mapMaybe onlyOne decisionLists,
      catalogUniqueConnections = Map.mapMaybe onlyOne connectionLists,
      catalogKnownAdrs =
        Set.fromList
          ( [ decisionAdr decision
              | decisions <- Map.elems decisionLists,
                decision <- decisions
            ]
              <> [ connectionSubject connection
                   | connections <- Map.elems connectionLists,
                     connection <- connections
                 ]
          )
    }
  where
    decisionLists =
      fmap (sortOn stableDecisionKey)
        . Map.fromListWith (<>)
        $ [ (decisionRecord decision, [decision])
            | ManagedDecision decision <- managed
          ]
    connectionLists =
      fmap (sortOn stableConnectionKey)
        . Map.fromListWith (<>)
        $ [ (connectionRecordId connection, [connection])
            | ManagedConnection connection <- managed
          ]

onlyOne :: [value] -> Maybe value
onlyOne [value] = Just value
onlyOne _ = Nothing

managedAdrs :: [ManagedRecord] -> Set AdrId
managedAdrs = Set.fromList . map managedAdr
  where
    managedAdr (ManagedDecision decision) = decisionAdr decision
    managedAdr (ManagedConnection connection) = connectionSubject connection

duplicateIssues :: Catalog -> [GraphIssue]
duplicateIssues catalog = decisionIssues <> connectionIssues
  where
    decisionIssues =
      [ GraphIssue
          DuplicateRecordId
          (singleAdr (map decisionAdr decisions))
          (Just (recordObjectRef identifier))
          ( "record identifier "
              <> recordIdText identifier
              <> " occurs "
              <> decimal (length decisions)
              <> " times; every occurrence is quarantined"
          )
        | (identifier, decisions) <- Map.toAscList (catalogDecisionLists catalog),
          length decisions > 1
      ]
    connectionIssues =
      [ GraphIssue
          DuplicateConnectionId
          (singleAdr (map connectionSubject connections))
          (Just (connectionObjectRef identifier))
          ( "connection identifier "
              <> connectionIdText identifier
              <> " occurs "
              <> decimal (length connections)
              <> " times; every occurrence is quarantined"
          )
        | (identifier, connections) <- Map.toAscList (catalogConnectionLists catalog),
          length connections > 1
      ]

singleAdr :: [AdrId] -> Maybe AdrId
singleAdr values =
  case Set.toAscList (Set.fromList values) of
    [value] -> Just value
    _ -> Nothing

-- ADR materialization ---------------------------------------------------------

reduceAdr :: Catalog -> AdrId -> (ReducedAdr, [GraphIssue])
reduceAdr catalog adr =
  ( ReducedAdr
      { reducedAdrId = adr,
        reducedDecisionAxis = decisionResolution,
        reducedScopeAxis = scopeResolution,
        reducedDomainAxis = domainResolution,
        reducedStatusAxis = statusResolution,
        reducedCurrentDecisions = currentDecisions,
        reducedDecisionHistory = decisionHistory,
        reducedAmendmentHistory = amendmentHistory,
        reducedScopeHistory = scopeHistory,
        reducedDomainHistory = domainHistory,
        reducedStatusHistory = statusHistory,
        reducedConflictAxes = conflictAxes,
        reducedConflictMessages = conflictMessages,
        reducedStateHeads = stateHeads,
        reducedStateToken = stateTokenForHeads stateHeads
      },
    decisionIssues <> scopeIssues <> domainIssues <> statusIssues
  )
  where
    localDecisions = Map.filter ((== adr) . decisionAdr) (catalogUniqueDecisions catalog)
    localConnections = Map.filter ((== adr) . connectionSubject) (catalogUniqueConnections catalog)

    amendmentConnections = connectionsFor AmendmentConnectionAxis localConnections
    scopeConnections = connectionsFor ScopeConnectionAxis localConnections
    domainConnections = connectionsFor DomainConnectionAxis localConnections
    statusConnections = connectionsFor StatusConnectionAxis localConnections

    (decisionHeads, decisionIssues) = reduceDecisionAxis catalog adr localDecisions amendmentConnections
    (scopeHeads, scopeEffectiveById, scopeIssues) =
      reduceScopeAxis catalog adr scopeConnections
    (domainHeads, domainEffectiveById, domainIssues) =
      reduceDomainAxis catalog adr domainConnections
    decisionRoots =
      [ identifier
        | identifier <- Map.keys localDecisions,
          identifier `Set.notMember` amendmentChildren
      ]
    amendmentChildren =
      Set.fromList
        [ amendsFromRecord payload
          | connection <- Map.elems amendmentConnections,
            AmendsConnection payload <- [connectionPayload connection],
            Map.member (amendsFromRecord payload) localDecisions
        ]
    (statusHeads, statusEffectiveById, statusIssues) =
      reduceStatusAxis catalog adr decisionRoots statusConnections

    decisionEffective =
      case decisionHeads of
        [headId] -> Map.lookup headId localDecisions
        _ -> Nothing
    decisionConflict = headConflict "decision" decisionHeads
    decisionResolution = AxisResolution decisionHeads decisionEffective decisionConflict

    scopeEffective =
      case scopeHeads of
        [headId] -> Map.findWithDefault [] headId scopeEffectiveById
        _ -> []
    scopeConflict = headConflict "scope" scopeHeads
    scopeResolution = AxisResolution scopeHeads scopeEffective scopeConflict

    domainEffective =
      Set.toAscList
        . Set.unions
        $ [ Set.fromList (Map.findWithDefault [] headId domainEffectiveById)
            | headId <- domainHeads
          ]
    domainConflict = headConflict "domain" domainHeads
    domainResolution = AxisResolution domainHeads domainEffective domainConflict

    statusCandidate =
      case statusHeads of
        [headId] -> Map.lookup headId statusEffectiveById
        _ -> Nothing
    statusConflict =
      case statusHeads of
        [] -> Just "0 status heads"
        [_headId]
          | Just status <- statusCandidate,
            reducedStatusState status == StatusObsolete,
            reducedStatusRecordHeads status /= decisionHeads ->
              Just "obsolete status does not cover current decision heads"
        [_] -> Nothing
        heads -> Just (decimal (length heads) <> " status heads")
    statusEffective =
      case statusConflict of
        Nothing -> statusCandidate
        Just _ -> Nothing
    statusResolution = AxisResolution statusHeads statusEffective statusConflict

    currentDecisions =
      [ CurrentDecisionView decision (extractMarkdownSections (decisionBody decision))
        | headId <- decisionHeads,
          Just decision <- [Map.lookup headId localDecisions]
      ]

    decisionHistory =
      sortOn stableDecisionKey
        [ decision
          | decisions <- Map.elems (catalogDecisionLists catalog),
            decision <- decisions,
            decisionAdr decision == adr
        ]
    localConnectionHistory axis =
      sortOn stableConnectionKey
        [ connection
          | connections <- Map.elems (catalogConnectionLists catalog),
            connection <- connections,
            connectionSubject connection == adr,
            connectionAxis connection == axis
        ]
    scopeHistory = localConnectionHistory ScopeConnectionAxis
    amendmentHistory = localConnectionHistory AmendmentConnectionAxis
    domainHistory = localConnectionHistory DomainConnectionAxis
    statusHistory = localConnectionHistory StatusConnectionAxis

    conflicts =
      [ (DecisionAxis, decisionConflict),
        (ScopeAxis, scopeConflict),
        (DomainAxis, domainConflict),
        (StatusAxis, statusConflict)
      ]
    conflictAxes = [axis | (axis, Just _) <- conflicts]
    conflictMessages = [message | (_, Just message) <- conflicts]
    stateHeads =
      StateHeads
        { stateRecordHeads = decisionHeads,
          stateScopeHeads = scopeHeads,
          stateStatusHeads = statusHeads,
          stateDomainHeads = domainHeads
        }

headConflict :: Text -> [identifier] -> Maybe Text
headConflict label heads =
  case heads of
    [_] -> Nothing
    _ -> Just (decimal (length heads) <> " " <> label <> " heads")

-- Decision axis ---------------------------------------------------------------

data AmendmentAnalysis = AmendmentAnalysis
  { amendmentAnalysisConnection :: ConnectionRecord,
    amendmentAnalysisChild :: Maybe RecordId,
    amendmentAnalysisParents :: [RecordId],
    amendmentAnalysisEligible :: Bool,
    amendmentAnalysisIssues :: [GraphIssue]
  }

reduceDecisionAxis :: Catalog -> AdrId -> Map RecordId DecisionRecord -> Map ConnectionId ConnectionRecord -> ([RecordId], [GraphIssue])
reduceDecisionAxis catalog adr localDecisions connections =
  (heads, analysisIssues <> duplicateChildIssues <> cycleIssues <> inheritedIssues <> rootIssues)
  where
    analyses = map analyze (Map.elems connections)
    analyze connection = analyzeAmendment catalog adr localDecisions connection
    analysisIssues = concatMap amendmentAnalysisIssues analyses
    initiallyInvalidChildren =
      Set.fromList
        [ child
          | analysis <- analyses,
            not (amendmentAnalysisEligible analysis),
            Just child <- [amendmentAnalysisChild analysis]
        ]
    eligibleByChild =
      Map.fromListWith (<>)
        [ (child, [analysis])
          | analysis <- analyses,
            amendmentAnalysisEligible analysis,
            Just child <- [amendmentAnalysisChild analysis],
            Set.notMember child initiallyInvalidChildren
        ]
    duplicateChildren =
      Set.fromList
        [ child
          | (child, childAnalyses) <- Map.toAscList eligibleByChild,
            length childAnalyses > 1
        ]
    duplicateChildIssues =
      [ GraphIssue
          InvalidAmendmentParents
          (Just adr)
          (Just (recordObjectRef child))
          ( "record "
              <> recordIdText child
              <> " has more than one amendment relation: "
              <> commaSeparated
                ( map
                    (connectionIdText . connectionRecordId . amendmentAnalysisConnection)
                    childAnalyses
                )
          )
        | (child, childAnalyses) <- Map.toAscList eligibleByChild,
          Set.member child duplicateChildren
      ]
    candidateParents =
      Map.fromList
        [ (child, amendmentAnalysisParents analysis)
          | (child, [analysis]) <- Map.toAscList eligibleByChild,
            Set.notMember child duplicateChildren
        ]
    candidateConnections =
      Map.fromList
        [ (child, connectionRecordId (amendmentAnalysisConnection analysis))
          | (child, [analysis]) <- Map.toAscList eligibleByChild,
            Set.notMember child duplicateChildren
        ]
    cycleComponents = cyclicComponents (Map.keys localDecisions) candidateParents
    cycleNodes = Set.fromList (concat cycleComponents)
    cycleIssues =
      [ GraphIssue
          AmendmentCycle
          (Just adr)
          (Just (recordObjectRef first))
          ("amendment cycle: " <> commaSeparated (map recordIdText (first : remaining)))
        | first : remaining <- cycleComponents
      ]
    initialQuarantine = initiallyInvalidChildren <> duplicateChildren <> cycleNodes
    (quarantined, inheritedIssues) = propagateFailures initialQuarantine []
    propagateFailures failed issues
      | null newlyFailed = (failed, issues)
      | otherwise =
          propagateFailures
            (failed <> Set.fromList newlyFailed)
            ( issues
                <> [ GraphIssue
                       InvalidAmendmentParents
                       (Just adr)
                       (connectionObjectRef <$> Map.lookup child candidateConnections)
                       ( "amendment child "
                           <> recordIdText child
                           <> " depends on a quarantined parent"
                       )
                     | child <- newlyFailed
                   ]
            )
      where
        newlyFailed =
          [ child
            | (child, parents) <- Map.toAscList candidateParents,
              Set.notMember child failed,
              any (`Set.member` failed) parents
          ]
    validParents =
      Map.filterWithKey
        (\child _ -> Set.notMember child quarantined)
        candidateParents
    consumed = Set.fromList (concat (Map.elems validParents))
    rootIds =
      [ identifier
        | identifier <- Map.keys localDecisions,
          Set.notMember identifier quarantined,
          Map.notMember identifier validParents
      ]
    rootIssues =
      [ cardinalityIssue
          DecisionRootCardinality
          adr
          (recordObjectRef <$> firstOf rootIds)
          "decision"
          (length rootIds)
        | length rootIds /= 1
      ]
    heads =
      [ identifier
        | identifier <- Map.keys localDecisions,
          Set.notMember identifier quarantined,
          Set.notMember identifier consumed
      ]

analyzeAmendment :: Catalog -> AdrId -> Map RecordId DecisionRecord -> ConnectionRecord -> AmendmentAnalysis
analyzeAmendment catalog adr localDecisions connection =
  AmendmentAnalysis connection localChild parents eligible issues
  where
    payload =
      case connectionPayload connection of
        AmendsConnection value -> value
        _ -> error "internal error: non-amendment passed to analyzeAmendment"
    child = amendsFromRecord payload
    parents = amendsToRecords payload
    objectId = Just (connectionObjectRef (connectionRecordId connection))
    childEntries = Map.lookup child (catalogDecisionLists catalog)
    localChild =
      case childEntries of
        Just [decision]
          | decisionAdr decision == adr,
            Map.member child localDecisions -> Just child
        _ -> Nothing
    childIssues =
      case childEntries of
        Nothing ->
          [ GraphIssue
              AmendmentMissingChild
              (Just adr)
              objectId
              ("amendment child does not exist: " <> recordIdText child)
          ]
        Just [decision]
          | decisionAdr decision /= adr ->
              [ GraphIssue
                  InvalidAmendmentParents
                  (Just adr)
                  objectId
                  ( "amendment child "
                      <> recordIdText child
                      <> " belongs to ADR "
                      <> adrIdText (decisionAdr decision)
                  )
              ]
        Just (_ : _ : _) ->
          [ GraphIssue
              InvalidAmendmentParents
              (Just adr)
              objectId
              ("amendment child is ambiguous: " <> recordIdText child)
          ]
        Just [_] -> []
        Just [] -> []
    withoutParentsIssues =
      [ GraphIssue
          AmendmentWithoutParents
          (Just adr)
          objectId
          "amendment must name at least one parent record"
        | null parents
      ]
    duplicateParentIssues =
      [ GraphIssue
          InvalidAmendmentParents
          (Just adr)
          objectId
          "amendment parent records must be unique"
        | hasDuplicates parents
      ]
    selfParentIssues =
      [ GraphIssue
          InvalidAmendmentParents
          (Just adr)
          objectId
          "amendment child may not name itself as a parent"
        | child `elem` parents
      ]
    parentIssues = concatMap validateParent parents
    validateParent parent =
      case Map.lookup parent (catalogDecisionLists catalog) of
        Nothing ->
          [ GraphIssue
              AmendmentMissingParent
              (Just adr)
              objectId
              ("amendment parent does not exist: " <> recordIdText parent)
          ]
        Just [decision]
          | decisionAdr decision == adr,
            Map.member parent localDecisions -> []
          | otherwise ->
              [ GraphIssue
                  InvalidAmendmentParents
                  (Just adr)
                  objectId
                  ( "amendment parent "
                      <> recordIdText parent
                      <> " belongs to ADR "
                      <> adrIdText (decisionAdr decision)
                  )
              ]
        Just _ ->
          [ GraphIssue
              InvalidAmendmentParents
              (Just adr)
              objectId
              ("amendment parent is ambiguous: " <> recordIdText parent)
          ]
    issues = childIssues <> withoutParentsIssues <> duplicateParentIssues <> selfParentIssues <> parentIssues
    eligible = null issues

-- Connection axes -------------------------------------------------------------

data StructuralAxis = StructuralAxis
  { structuralParents :: Map ConnectionId [ConnectionId],
    structuralInitiallyFailed :: Set ConnectionId,
    structuralIssues :: [GraphIssue]
  }

prepareConnectionAxis :: Catalog -> AdrId -> ConnectionAxis -> GraphIssueCode -> GraphIssueCode -> Map ConnectionId ConnectionRecord -> StructuralAxis
prepareConnectionAxis catalog adr expectedAxis invalidCode missingCode connections =
  StructuralAxis parentMap initialFailed (concat issuesByNode <> cycleIssues)
  where
    analyzed =
      [ (identifier, parentsOf connection, validateNode connection)
        | (identifier, connection) <- Map.toAscList connections
      ]
    parentMap = Map.fromList [(identifier, parents) | (identifier, parents, _) <- analyzed]
    issuesByNode = [issues | (_, _, issues) <- analyzed]
    structurallyFailed =
      Set.fromList [identifier | (identifier, _, issues) <- analyzed, not (null issues)]
    cycleComponents = cyclicComponents (Map.keys connections) parentMap
    cycleNodes = Set.fromList (concat cycleComponents)
    initialFailed = structurallyFailed <> cycleNodes
    cycleCode =
      case expectedAxis of
        ScopeConnectionAxis -> ScopeCycle
        DomainConnectionAxis -> DomainCycle
        StatusConnectionAxis -> StatusCycle
        AmendmentConnectionAxis -> AmendmentCycle
    cycleIssues =
      [ GraphIssue
          cycleCode
          (Just adr)
          (Just (connectionObjectRef first))
          ( axisLabel expectedAxis
              <> " cycle: "
              <> commaSeparated (map connectionIdText (first : remaining))
          )
        | first : remaining <- cycleComponents
      ]
    validateNode connection =
      duplicateIssuesFor connection <> concatMap (validateParent connection) (parentsOf connection)
    duplicateIssuesFor connection =
      [ GraphIssue
          invalidCode
          (Just adr)
          (Just (connectionObjectRef (connectionRecordId connection)))
          (axisLabel expectedAxis <> " parent connections must be unique")
        | hasDuplicates (parentsOf connection)
      ]
        <> [ GraphIssue
               invalidCode
               (Just adr)
               (Just (connectionObjectRef (connectionRecordId connection)))
               (axisLabel expectedAxis <> " connection may not name itself as a parent")
             | connectionRecordId connection `elem` parentsOf connection
           ]
    validateParent child parent =
      case Map.lookup parent (catalogConnectionLists catalog) of
        Nothing ->
          [ GraphIssue
              missingCode
              (Just adr)
              (Just (connectionObjectRef (connectionRecordId child)))
              (axisLabel expectedAxis <> " parent does not exist: " <> connectionIdText parent)
          ]
        Just [parentConnection]
          | connectionAxis parentConnection == expectedAxis,
            connectionSubject parentConnection == adr,
            Map.member parent connections -> []
          | connectionAxis parentConnection /= expectedAxis ->
              [ GraphIssue
                  invalidCode
                  (Just adr)
                  (Just (connectionObjectRef (connectionRecordId child)))
                  ( axisLabel expectedAxis
                      <> " parent "
                      <> connectionIdText parent
                      <> " is a "
                      <> axisLabel (connectionAxis parentConnection)
                      <> " connection"
                  )
              ]
          | otherwise ->
              [ GraphIssue
                  invalidCode
                  (Just adr)
                  (Just (connectionObjectRef (connectionRecordId child)))
                  ( axisLabel expectedAxis
                      <> " parent "
                      <> connectionIdText parent
                      <> " belongs to ADR "
                      <> adrIdText (connectionSubject parentConnection)
                  )
              ]
        Just _ ->
          [ GraphIssue
              invalidCode
              (Just adr)
              (Just (connectionObjectRef (connectionRecordId child)))
              (axisLabel expectedAxis <> " parent is ambiguous: " <> connectionIdText parent)
          ]

data EvalResult effective = EvalResult
  { evalEffective :: Map ConnectionId effective,
    evalIssues :: [GraphIssue]
  }

evaluateConnectionAxis :: AdrId -> Text -> GraphIssueCode -> Map ConnectionId [ConnectionId] -> Set ConnectionId -> (ConnectionId -> [effective] -> Either [(GraphIssueCode, Text)] effective) -> EvalResult effective
evaluateConnectionAxis adr label inheritedCode parentMap initiallyFailed validate =
  go Map.empty initiallyFailed []
  where
    allNodes = Map.keysSet parentMap
    go effective failed issues
      | Set.null unresolved = EvalResult effective issues
      | null actions =
          let stuck = Set.toAscList unresolved
              stuckIssues =
                [ inheritedIssue identifier "depends on a quarantined or unevaluable parent"
                  | identifier <- stuck
                ]
           in EvalResult effective (issues <> stuckIssues)
      | otherwise =
          let (nextEffective, nextFailed, nextIssues) =
                foldl' applyAction (effective, failed, issues) actions
           in go nextEffective nextFailed nextIssues
      where
        unresolved = allNodes `Set.difference` Map.keysSet effective `Set.difference` failed
        actions = mapMaybe (readyAction effective failed) (Set.toAscList unresolved)

    readyAction effective failed identifier
      | any (`Set.member` failed) parents = Just (identifier, Left Nothing)
      | all (`Map.member` effective) parents =
          Just
            ( identifier,
              case validate identifier (map (effective Map.!) parents) of
                Left problems -> Left (Just problems)
                Right value -> Right value
            )
      | otherwise = Nothing
      where
        parents = Map.findWithDefault [] identifier parentMap

    applyAction (effective, failed, issues) (identifier, result) =
      case result of
        Right value -> (Map.insert identifier value effective, failed, issues)
        Left Nothing ->
          ( effective,
            Set.insert identifier failed,
            issues <> [inheritedIssue identifier "depends on a quarantined parent"]
          )
        Left (Just problems) ->
          ( effective,
            Set.insert identifier failed,
            issues
              <> [ GraphIssue code (Just adr) (Just (connectionObjectRef identifier)) message
                   | (code, message) <- problems
                 ]
          )

    inheritedIssue identifier suffix =
      GraphIssue
        inheritedCode
        (Just adr)
        (Just (connectionObjectRef identifier))
        (label <> " connection " <> connectionIdText identifier <> " " <> suffix)

validConnectionHeads :: Map ConnectionId [ConnectionId] -> Map ConnectionId effective -> [ConnectionId]
validConnectionHeads parentMap effective =
  [ identifier
    | identifier <- Map.keys effective,
      Set.notMember identifier consumed
  ]
  where
    consumed =
      Set.fromList
        [ parent
          | child <- Map.keys effective,
            parent <- Map.findWithDefault [] child parentMap,
            Map.member parent effective
        ]

validConnectionRoots :: Map ConnectionId [ConnectionId] -> Map ConnectionId effective -> [ConnectionId]
validConnectionRoots parentMap effective =
  [ identifier
    | identifier <- Map.keys effective,
      null (Map.findWithDefault [] identifier parentMap)
  ]

axisRootIssues :: GraphIssueCode -> AdrId -> Text -> [ConnectionId] -> [GraphIssue]
axisRootIssues code adr label roots =
  [ cardinalityIssue
      code
      adr
      (connectionObjectRef <$> firstOf roots)
      label
      (length roots)
    | length roots /= 1
  ]

validationIssues :: AdrId -> ConnectionId -> Either [(GraphIssueCode, Text)] effective -> [GraphIssue]
validationIssues adr identifier result =
  case result of
    Right _ -> []
    Left problems ->
      [ GraphIssue code (Just adr) (Just (connectionObjectRef identifier)) message
        | (code, message) <- problems
      ]

reduceScopeAxis :: Catalog -> AdrId -> Map ConnectionId ConnectionRecord -> ([ConnectionId], Map ConnectionId [ScopePattern], [GraphIssue])
reduceScopeAxis catalog adr connections =
  ( validConnectionHeads parents effective,
    effective,
    structuralIssues structural <> evalIssues evaluated <> failedPayloadIssues <> rootIssues
  )
  where
    structural =
      prepareConnectionAxis catalog adr ScopeConnectionAxis InvalidScopeParents ScopeMissingParent connections
    parents = structuralParents structural
    evaluated =
      evaluateConnectionAxis
        adr
        "scope"
        InvalidScopeParents
        parents
        (structuralInitiallyFailed structural)
        validateNode
    effective = evalEffective evaluated
    roots = validConnectionRoots parents effective
    rootIssues = axisRootIssues ScopeRootCardinality adr "scope" roots
    failedPayloadIssues =
      concat
        [ validationIssues adr identifier (validateScopeDelta connection (rawScopeParents connection))
          | identifier <- Set.toAscList (structuralInitiallyFailed structural),
            Just connection <- [Map.lookup identifier connections]
        ]
    rawScopeParents connection =
      [ appliesToEffective payload
        | parent <- appliesToParentConnections (scopePayload connection),
          Just parentConnection <- [Map.lookup parent connections],
          let payload = scopePayload parentConnection
      ]
    validateNode identifier parentValues =
      case Map.lookup identifier connections of
        Just connection -> validateScopeDelta connection parentValues
        Nothing -> Left [(InvalidScopeParents, "scope connection disappeared during reduction")]

validateScopeDelta :: ConnectionRecord -> [[ScopePattern]] -> Either [(GraphIssueCode, Text)] [ScopePattern]
validateScopeDelta connection parentValues =
  if null problems
    then Right (Set.toAscList effective)
    else
      Left
        [ (scopeProblemCode problemClass, issueMessage "scope" connection [message])
          | (problemClass, message) <- problems
        ]
  where
    payload = scopePayload connection
    parent = Set.unions (map Set.fromList parentValues)
    added = Set.fromList (appliesToAdded payload)
    removed = Set.fromList (appliesToRemoved payload)
    effective = Set.fromList (appliesToEffective payload)
    problems =
      validateDelta
        (appliesToChange payload)
        (length (appliesToParentConnections payload))
        False
        True
        parent
        added
        removed
        effective

reduceDomainAxis :: Catalog -> AdrId -> Map ConnectionId ConnectionRecord -> ([ConnectionId], Map ConnectionId [Domain], [GraphIssue])
reduceDomainAxis catalog adr connections =
  ( validConnectionHeads parents effective,
    effective,
    structuralIssues structural <> evalIssues evaluated <> failedPayloadIssues <> rootIssues
  )
  where
    structural =
      prepareConnectionAxis catalog adr DomainConnectionAxis InvalidDomainParents DomainMissingParent connections
    parents = structuralParents structural
    evaluated =
      evaluateConnectionAxis
        adr
        "domain"
        InvalidDomainParents
        parents
        (structuralInitiallyFailed structural)
        validateNode
    effective = evalEffective evaluated
    roots = validConnectionRoots parents effective
    rootIssues = axisRootIssues DomainRootCardinality adr "domain" roots
    failedPayloadIssues =
      concat
        [ validationIssues adr identifier (validateDomainDelta connection (rawDomainParents connection))
          | identifier <- Set.toAscList (structuralInitiallyFailed structural),
            Just connection <- [Map.lookup identifier connections]
        ]
    rawDomainParents connection =
      [ domainsEffective payload
        | parent <- domainsParentConnections (domainPayload connection),
          Just parentConnection <- [Map.lookup parent connections],
          let payload = domainPayload parentConnection
      ]
    validateNode identifier parentValues =
      case Map.lookup identifier connections of
        Just connection -> validateDomainDelta connection parentValues
        Nothing -> Left [(InvalidDomainParents, "domain connection disappeared during reduction")]

validateDomainDelta :: ConnectionRecord -> [[Domain]] -> Either [(GraphIssueCode, Text)] [Domain]
validateDomainDelta connection parentValues =
  case (deltaProblems, refinementProblems) of
    ([], []) -> Right (Set.toAscList effective)
    _ ->
      Left
        ( [ (domainProblemCode problemClass, issueMessage "domain" connection [message])
            | (problemClass, message) <- deltaProblems
          ]
            <> [ (InvalidDomainRefinement, issueMessage "domain refinement" connection refinementProblems)
                 | not (null refinementProblems)
               ]
        )
  where
    payload = domainPayload connection
    parent = Set.unions (map Set.fromList parentValues)
    added = Set.fromList (domainsAdded payload)
    removed = Set.fromList (domainsRemoved payload)
    effective = Set.fromList (domainsEffective payload)
    change = domainsChange payload
    deltaProblems =
      validateDelta
        change
        (length (domainsParentConnections payload))
        True
        False
        parent
        added
        removed
        effective
    refinementProblems = validateRefinements change added removed (domainsRefinements payload)

data DeltaProblemClass
  = DeltaInvalidChangeKind
  | DeltaChangeShape
  | DeltaOverlap
  | DeltaMergeKind
  | DeltaMismatch
  | DeltaRootNotInitial
  deriving (Eq, Ord, Show)

scopeProblemCode :: DeltaProblemClass -> GraphIssueCode
scopeProblemCode problemClass =
  case problemClass of
    DeltaInvalidChangeKind -> InvalidScopeChangeKind
    DeltaChangeShape -> ScopeChangeShape
    DeltaOverlap -> ScopeDeltaOverlap
    DeltaMergeKind -> ScopeMergeKind
    DeltaMismatch -> ScopeDeltaMismatch
    DeltaRootNotInitial -> ScopeRootNotInitial

domainProblemCode :: DeltaProblemClass -> GraphIssueCode
domainProblemCode problemClass =
  case problemClass of
    DeltaInvalidChangeKind -> InvalidDomainChangeKind
    DeltaChangeShape -> DomainChangeShape
    DeltaOverlap -> DomainDeltaOverlap
    DeltaMergeKind -> DomainMergeKind
    DeltaMismatch -> DomainDeltaMismatch
    DeltaRootNotInitial -> DomainRootNotInitial

validateDelta :: (Ord value) => Text -> Int -> Bool -> Bool -> Set value -> Set value -> Set value -> Set value -> [(DeltaProblemClass, Text)]
validateDelta change parentCount allowRefine parentlessNonInitialIsShape parent added removed effective =
  kindProblems <> rootProblems <> mergeProblems <> arityProblems <> shapeProblems <> setProblems
  where
    supportedChanges =
      ["initial", "expand", "contract", "mixed", "replace", "merge"]
        <> ["refine" | allowRefine]
    kindProblems =
      [ (DeltaInvalidChangeKind, "unsupported change mode: " <> change)
        | change `notElem` supportedChanges
      ]
    rootProblems =
      [ (DeltaRootNotInitial, "a parentless root must use initial change mode")
        | parentCount == 0,
          change /= "initial"
      ]
    mergeProblems =
      [ (DeltaMergeKind, "multiple parents require merge change mode")
        | parentCount >= 2,
          change /= "merge"
      ]
    arityProblems =
      [ (DeltaChangeShape, "merge change mode requires at least two parents")
        | change == "merge",
          parentCount < 2
      ]
        <> [ (DeltaChangeShape, "non-initial, non-merge change mode requires exactly one parent")
             | change `notElem` ["initial", "merge"],
               if parentlessNonInitialIsShape
                 then parentCount /= 1
                 else parentCount > 0 && parentCount /= 1
           ]
    shapeProblems =
      case change of
        "initial"
          | parentCount == 0,
            Set.null removed -> []
          | otherwise -> [(DeltaChangeShape, "initial requires no parents and no removals")]
        "expand"
          | not (Set.null added),
            Set.null removed -> []
          | otherwise -> [(DeltaChangeShape, "expand requires additions and no removals")]
        "contract"
          | Set.null added,
            not (Set.null removed) -> []
          | otherwise -> [(DeltaChangeShape, "contract requires removals and no additions")]
        "mixed"
          | not (Set.null added),
            not (Set.null removed) -> []
          | otherwise -> [(DeltaChangeShape, "mixed requires additions and removals")]
        "replace" -> []
        "merge" -> []
        "refine"
          | allowRefine,
            not (Set.null added),
            not (Set.null removed) -> []
          | otherwise -> [(DeltaChangeShape, "refine requires additions and removals")]
        _ -> []
    reconstructed = (parent `Set.difference` removed) <> added
    expectedAdded = effective `Set.difference` parent
    expectedRemoved = parent `Set.difference` effective
    setProblems =
      [ (DeltaOverlap, "added and removed sets overlap")
        | not (Set.disjoint added removed)
      ]
        <> [ (DeltaMismatch, "removed set contains values absent from the parent effective union")
             | not (removed `Set.isSubsetOf` parent)
           ]
        <> [ (DeltaMismatch, "effective set does not reconstruct from the parent union and delta")
             | reconstructed /= effective
           ]
        <> [ (DeltaMismatch, "added and removed sets are not the exact effective-set difference")
             | added /= expectedAdded || removed /= expectedRemoved
           ]

validateRefinements :: Text -> Set Domain -> Set Domain -> [DomainRefinement] -> [Text]
validateRefinements change added removed refinements
  | change /= "refine" =
      ["refinements are only permitted for refine changes" | not (null refinements)]
  | otherwise =
      [ "refinement mappings must exactly cover every removed parent and added child"
        | mappedParents /= removed || mappedChildren /= added
      ]
        <> [ "each removed domain may be refined at most once"
             | length refinements /= Set.size mappedParents
           ]
  where
    mappedParents = Set.fromList (map domainRefinementParent refinements)
    mappedChildren = Set.fromList (map domainRefinementChild refinements)

reduceStatusAxis :: Catalog -> AdrId -> [RecordId] -> Map ConnectionId ConnectionRecord -> ([ConnectionId], Map ConnectionId ReducedStatus, [GraphIssue])
reduceStatusAxis catalog adr decisionRoots connections =
  ( validConnectionHeads parents effective,
    effective,
    structuralIssues structural <> evalIssues evaluated <> failedPayloadIssues <> rootIssues
  )
  where
    structural =
      prepareConnectionAxis catalog adr StatusConnectionAxis InvalidStatusParents StatusMissingParent connections
    parents = structuralParents structural
    evaluated =
      evaluateConnectionAxis
        adr
        "status"
        InvalidStatusParents
        parents
        (structuralInitiallyFailed structural)
        validateNode
    effective = evalEffective evaluated
    roots = validConnectionRoots parents effective
    rootIssues = axisRootIssues StatusRootCardinality adr "status" roots
    failedPayloadIssues =
      concat
        [ validationIssues adr identifier (validateStatusNode catalog adr decisionRoots connection (rawStatusParents connection))
          | identifier <- Set.toAscList (structuralInitiallyFailed structural),
            Just connection <- [Map.lookup identifier connections]
        ]
    rawStatusParents connection =
      [ rawStatus parentConnection
        | parent <- statusParentConnections (statusPayload connection),
          Just parentConnection <- [Map.lookup parent connections]
      ]
    validateNode identifier parentValues =
      case Map.lookup identifier connections of
        Just connection -> validateStatusNode catalog adr decisionRoots connection parentValues
        Nothing -> Left [(InvalidStatusParents, "status connection disappeared during reduction")]

validateStatusNode :: Catalog -> AdrId -> [RecordId] -> ConnectionRecord -> [ReducedStatus] -> Either [(GraphIssueCode, Text)] ReducedStatus
validateStatusNode catalog adr decisionRoots connection _parentValues =
  if null problems
    then
      Right
        ReducedStatus
          { reducedStatusState = statusState payload,
            reducedStatusRecordHeads = sort (statusRecordHeads payload),
            reducedStatusReplacement = statusReplacementAdr payload
          }
    else Left problems
  where
    payload = statusPayload connection
    prefix detail =
      "status connection "
        <> connectionIdText (connectionRecordId connection)
        <> " "
        <> detail
    initialProblems =
      [ (InvalidStatusInitial, prefix "must start active when it has no parent")
        | null (statusParentConnections payload),
          statusState payload /= StatusActive
      ]
        <> [ (InvalidStatusInitial, prefix "does not cover the decision creation root set")
             | null (statusParentConnections payload),
                sort (statusRecordHeads payload) /= sort decisionRoots
           ]
        <> [ (InvalidStatusInitial, prefix "initial active status may not name a replacement ADR")
             | null (statusParentConnections payload),
               statusReplacementAdr payload /= Nothing
           ]
    activeReplacementProblems =
      [ (ActiveStatusHasReplacement, prefix "is active and may not name a replacement ADR")
        | statusState payload == StatusActive,
          statusReplacementAdr payload /= Nothing
      ]
    duplicateCoverageProblems =
      [ (InvalidStatusParents, prefix "contains duplicate record_heads")
        | hasDuplicates (statusRecordHeads payload)
      ]
    coverageProblems = concatMap validateCoveredRecord (statusRecordHeads payload)
    validateCoveredRecord record =
      case Map.lookup record (catalogDecisionLists catalog) of
        Just [decision]
          | decisionAdr decision == adr -> []
        Nothing ->
          [(StatusMissingRecord, prefix ("covers missing record " <> recordIdText record))]
        Just [decision] ->
          [ ( StatusMissingRecord,
              prefix
                ( "covers record "
                    <> recordIdText record
                    <> " from ADR "
                    <> adrIdText (decisionAdr decision)
                )
            )
          ]
        Just _ ->
          [(StatusMissingRecord, prefix ("covers ambiguous record " <> recordIdText record))]
    replacementProblems =
      case statusReplacementAdr payload of
        Nothing -> []
        Just replacement
          | replacement == adr ->
              [(StatusSelfReplacement, prefix "names its own ADR as replacement")]
          | Set.notMember replacement (catalogKnownAdrs catalog) ->
              [ ( StatusMissingReplacement,
                  prefix ("names unknown replacement ADR " <> adrIdText replacement)
                )
              ]
          | otherwise -> []
    problems = initialProblems <> activeReplacementProblems <> duplicateCoverageProblems <> coverageProblems <> replacementProblems

-- Shared helpers --------------------------------------------------------------

scopePayload :: ConnectionRecord -> AppliesToPayload
scopePayload connection =
  case connectionPayload connection of
    AppliesToConnection payload -> payload
    _ -> error "internal error: expected an applies_to connection"

domainPayload :: ConnectionRecord -> DomainsPayload
domainPayload connection =
  case connectionPayload connection of
    DomainsConnection payload -> payload
    _ -> error "internal error: expected a domains connection"

statusPayload :: ConnectionRecord -> StatusPayload
statusPayload connection =
  case connectionPayload connection of
    StatusConnection payload -> payload
    _ -> error "internal error: expected a status connection"

rawStatus :: ConnectionRecord -> ReducedStatus
rawStatus connection =
  ReducedStatus
    { reducedStatusState = statusState payload,
      reducedStatusRecordHeads = sort (statusRecordHeads payload),
      reducedStatusReplacement = statusReplacementAdr payload
    }
  where
    payload = statusPayload connection

connectionsFor :: ConnectionAxis -> Map ConnectionId ConnectionRecord -> Map ConnectionId ConnectionRecord
connectionsFor axis = Map.filter ((== axis) . connectionAxis)

connectionAxis :: ConnectionRecord -> ConnectionAxis
connectionAxis connection =
  case connectionPayload connection of
    AmendsConnection _ -> AmendmentConnectionAxis
    AppliesToConnection _ -> ScopeConnectionAxis
    DomainsConnection _ -> DomainConnectionAxis
    StatusConnection _ -> StatusConnectionAxis

connectionSubject :: ConnectionRecord -> AdrId
connectionSubject connection =
  case connectionPayload connection of
    AmendsConnection payload -> amendsSubjectAdr payload
    AppliesToConnection payload -> appliesToSubjectAdr payload
    DomainsConnection payload -> domainsSubjectAdr payload
    StatusConnection payload -> statusSubjectAdr payload

parentsOf :: ConnectionRecord -> [ConnectionId]
parentsOf connection =
  case connectionPayload connection of
    AmendsConnection _ -> []
    AppliesToConnection payload -> appliesToParentConnections payload
    DomainsConnection payload -> domainsParentConnections payload
    StatusConnection payload -> statusParentConnections payload

axisLabel :: ConnectionAxis -> Text
axisLabel axis =
  case axis of
    AmendmentConnectionAxis -> "amendment"
    ScopeConnectionAxis -> "scope"
    DomainConnectionAxis -> "domain"
    StatusConnectionAxis -> "status"

cyclicComponents :: (Ord identifier) => [identifier] -> Map identifier [identifier] -> [[identifier]]
cyclicComponents nodes adjacency =
  sort
    [ sort members
      | component <- stronglyConnComp graphNodes,
        members <- componentMembers component,
        isCycle component members
    ]
  where
    graphNodes =
      [ (identifier, identifier, Map.findWithDefault [] identifier adjacency)
        | identifier <- sort nodes
      ]
    componentMembers (AcyclicSCC identifier) = [[identifier]]
    componentMembers (CyclicSCC identifiers) = [identifiers]
    isCycle (CyclicSCC _) _ = True
    isCycle (AcyclicSCC identifier) _ = identifier `elem` Map.findWithDefault [] identifier adjacency

issueMessage :: Text -> ConnectionRecord -> [Text] -> Text
issueMessage label connection problems =
  "invalid "
    <> label
    <> " transition at "
    <> connectionIdText (connectionRecordId connection)
    <> ": "
    <> Text.intercalate "; " problems

hasDuplicates :: (Ord value) => [value] -> Bool
hasDuplicates values = Set.size (Set.fromList values) /= length values

firstOf :: [value] -> Maybe value
firstOf [] = Nothing
firstOf (value : _) = Just value

cardinalityIssue :: GraphIssueCode -> AdrId -> Maybe ObjectRef -> Text -> Int -> GraphIssue
cardinalityIssue code adr objectId label actual =
  GraphIssue
    code
    (Just adr)
    objectId
    ( "expected exactly 1 valid parentless "
        <> label
        <> " root, found "
        <> decimal actual
    )

stableDecisionKey :: DecisionRecord -> (RecordId, Text)
stableDecisionKey decision = (decisionRecord decision, Text.pack (show decision))

stableConnectionKey :: ConnectionRecord -> (ConnectionId, Text)
stableConnectionKey connection = (connectionRecordId connection, Text.pack (show connection))

sortGraphIssues :: [GraphIssue] -> [GraphIssue]
sortGraphIssues =
  sortOn
    ( \issue ->
        ( graphIssueCodeText (graphIssueCode issue),
          fmap adrIdText (graphIssueAdr issue),
          fmap objectRefText (graphIssueObject issue),
          graphIssueMessage issue
        )
    )

commaSeparated :: [Text] -> Text
commaSeparated = Text.intercalate ", "

decimal :: Int -> Text
decimal = Text.pack . show
