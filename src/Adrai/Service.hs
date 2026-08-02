{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Pure service-layer validation and append-only conflict reconciliation.
module Adrai.Service
  ( validateExpectedState,
    adrConflictToAdraiError,
    DecisionResolution (..),
    ScopeResolution (..),
    DomainResolution (..),
    StatusResolution (..),
    ReconciliationChoice (..),
    reconciliationChoiceAxis,
    ReconciliationPlan (..),
    ReconciliationError (..),
    reconciliationErrorToAdraiError,
    planReconciliation,
  )
where

import Adrai.Domain (Domain)
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
    renderManagedSemantic,
  )
import Adrai.Graph
  ( AdrConflict (..),
    AxisResolution (..),
    ConflictCandidate (..),
    GraphAxis (..),
    GraphIssue,
    GraphReduction (..),
    ReducedAdr (..),
    ReducedStatus (..),
    classifyAdrConflict,
    lookupReducedAdr,
    reduceManagedGraph,
  )
import Adrai.Scope (ScopePattern)
import Adrai.Types
  ( AdraiError (..),
    AdrId,
    ConnectionId,
    ExitClass (..),
    RecordId,
    StateToken,
    adrIdText,
    connectionIdText,
    recordIdText,
    stateTokenText,
  )
import Data.List (find)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

validateExpectedState :: Maybe StateToken -> StateToken -> Either AdraiError ()
validateExpectedState Nothing _ = Right ()
validateExpectedState (Just expected) current
  | expected == current = Right ()
  | otherwise =
      Left
        AdraiError
          { adraiErrorClass = ExitConflict,
            adraiErrorMessage = staleStateMessage expected current
          }

data DecisionResolution = DecisionResolution
  { decisionResolutionSubject :: AdrId,
    decisionResolutionDecision :: DecisionRecord,
    decisionResolutionAmendmentId :: ConnectionId,
    decisionResolutionRationale :: Text
  }
  deriving (Eq, Show)

data ScopeResolution = ScopeResolution
  { scopeResolutionSubject :: AdrId,
    scopeResolutionConnectionId :: ConnectionId,
    scopeResolutionEffective :: [ScopePattern],
    scopeResolutionRationale :: Text
  }
  deriving (Eq, Show)

data DomainResolution = DomainResolution
  { domainResolutionSubject :: AdrId,
    domainResolutionConnectionId :: ConnectionId,
    domainResolutionEffective :: [Domain],
    domainResolutionRationale :: Text
  }
  deriving (Eq, Show)

data StatusResolution = StatusResolution
  { statusResolutionSubject :: AdrId,
    statusResolutionConnectionId :: ConnectionId,
    statusResolutionState :: StatusState,
    statusResolutionReplacement :: Maybe AdrId,
    statusResolutionRationale :: Text,
    statusResolutionExplicit :: Bool
  }
  deriving (Eq, Show)

data ReconciliationChoice
  = ReconcileDecision DecisionResolution
  | ReconcileScope ScopeResolution
  | ReconcileDomain DomainResolution
  | ReconcileStatus StatusResolution
  deriving (Eq, Show)

reconciliationChoiceAxis :: ReconciliationChoice -> GraphAxis
reconciliationChoiceAxis choice =
  case choice of
    ReconcileDecision _ -> DecisionAxis
    ReconcileScope _ -> ScopeAxis
    ReconcileDomain _ -> DomainAxis
    ReconcileStatus _ -> StatusAxis

data ReconciliationPlan = ReconciliationPlan
  { reconciliationAppends :: [ManagedRecord],
    reconciliationResult :: GraphReduction,
    reconciliationTargetResult :: ReducedAdr,
    reconciliationNextStateToken :: StateToken
  }
  deriving (Eq, Show)

data ReconciliationError
  = ReconciliationSourceIntegrity [GraphIssue]
  | ReconciliationUnknownAdr AdrId
  | ReconciliationStaleState StateToken StateToken
  | ReconciliationNoConflict GraphAxis
  | ReconciliationWrongSubject AdrId AdrId
  | ReconciliationReusedRecordId RecordId
  | ReconciliationReusedConnectionId ConnectionId
  | ReconciliationInvalidDecision Text
  | ReconciliationInvalidRecord Text
  | ReconciliationStatusRequiresExplicitResolution
  | ReconciliationInvalidStatus Text
  | ReconciliationTargetNotActive GraphAxis AdrId Text
  | ReconciliationDecisionDomainConflict AdrId Int
  | ReconciliationReplacementNotActive AdrId
  | ReconciliationIntroducedIssues [GraphIssue]
  | ReconciliationAxisStillConflicted GraphAxis
  | ReconciliationChangedOtherConflict GraphAxis
  deriving (Eq, Show)

adrConflictToAdraiError :: AdrConflict -> AdraiError
adrConflictToAdraiError conflict =
  AdraiError
    { adraiErrorClass = ExitConflict,
      adraiErrorMessage = Text.intercalate "; " (adrConflictSummaries conflict)
    }

reconciliationErrorToAdraiError :: ReconciliationError -> AdraiError
reconciliationErrorToAdraiError plannerError =
  case plannerError of
    ReconciliationSourceIntegrity issues ->
      inputError
        ( "ADRAI source has "
            <> Text.pack (show (length issues))
            <> " integrity error(s); run 'adrai doctor' before mutating"
        )
    ReconciliationStaleState expected current ->
      conflictError (staleStateMessage expected current)
    ReconciliationNoConflict axis ->
      conflictError ("ADR has no current " <> axisText axis <> " conflict to resolve")
    ReconciliationAxisStillConflicted axis ->
      conflictError ("reconciliation did not resolve the " <> axisText axis <> " conflict")
    ReconciliationChangedOtherConflict axis ->
      conflictError ("reconciliation changed the unrelated " <> axisText axis <> " conflict")
    ReconciliationUnknownAdr adr ->
      inputError ("unknown ADR: " <> adrIdText adr)
    ReconciliationWrongSubject expected actual ->
      inputError
        ( "resolution subject "
            <> adrIdText actual
            <> " does not match target "
            <> adrIdText expected
        )
    ReconciliationReusedRecordId identifier ->
      inputError ("resolution record identifier is not fresh: " <> recordIdText identifier)
    ReconciliationReusedConnectionId identifier ->
      inputError ("resolution connection identifier is not fresh: " <> connectionIdText identifier)
    ReconciliationInvalidDecision detail -> inputError ("invalid decision resolution: " <> detail)
    ReconciliationInvalidRecord detail -> inputError ("invalid reconciliation record: " <> detail)
    ReconciliationStatusRequiresExplicitResolution ->
      conflictError "status conflict resolution requires an explicit resolve flag"
    ReconciliationInvalidStatus detail -> inputError ("invalid status resolution: " <> detail)
    ReconciliationTargetNotActive axis adr status ->
      conflictError
        ( adrIdText adr
            <> " status is "
            <> status
            <> "; only active ADRs can "
            <> activeOperation axis
        )
    ReconciliationDecisionDomainConflict adr count ->
      conflictError
        ( adrIdText adr
            <> " has "
            <> Text.pack (show count)
            <> " domain heads; reconcile them with 'adrai domain --set ...' before amending decision text"
        )
    ReconciliationReplacementNotActive replacement ->
      conflictError
        ("replacement ADR " <> adrIdText replacement <> " is not unambiguously active")
    ReconciliationIntroducedIssues issues ->
      inputError
        ( "reconciliation introduced "
            <> Text.pack (show (length issues))
            <> " graph issue(s)"
        )
  where
    conflictError = AdraiError ExitConflict
    inputError = AdraiError ExitUserError
    activeOperation axis =
      case axis of
        DecisionAxis -> "be amended"
        ScopeAxis -> "change scope"
        DomainAxis -> "change domains"
        StatusAxis -> "change status"

-- | Plan one reconciliation without touching the worktree.  The expected token
-- is mandatory here because every resolution consumes the complete current
-- head set.
planReconciliation :: [ManagedRecord] -> AdrId -> StateToken -> ReconciliationChoice -> Either ReconciliationError ReconciliationPlan
planReconciliation original target expected choice = do
  let before = reduceManagedGraph original
  if null (graphReductionIssues before)
    then Right ()
    else Left (ReconciliationSourceIntegrity (graphReductionIssues before))
  current <- maybe (Left (ReconciliationUnknownAdr target)) Right (lookupReducedAdr target before)
  if expected == reducedStateToken current
    then Right ()
    else Left (ReconciliationStaleState expected (reducedStateToken current))
  let axis = reconciliationChoiceAxis choice
  requireConflict axis current
  requireLifecycle axis target current
  appends <- buildAppends before current target choice
  validateFresh original appends
  mapM_ validateGenerated appends
  let after = reduceManagedGraph (original <> appends)
      newIssues = filter (`notElem` graphReductionIssues before) (graphReductionIssues after)
  if null newIssues
    then Right ()
    else Left (ReconciliationIntroducedIssues newIssues)
  resolved <- maybe (Left (ReconciliationUnknownAdr target)) Right (lookupReducedAdr target after)
  if hasConflict axis resolved
    then Left (ReconciliationAxisStillConflicted axis)
    else Right ()
  preserveOtherAxes axis current resolved
  Right
    ReconciliationPlan
      { reconciliationAppends = appends,
        reconciliationResult = after,
        reconciliationTargetResult = resolved,
        reconciliationNextStateToken = reducedStateToken resolved
      }

requireConflict :: GraphAxis -> ReducedAdr -> Either ReconciliationError ()
requireConflict axis adr
  | hasConflict axis adr = Right ()
  | otherwise = Left (ReconciliationNoConflict axis)

hasConflict :: GraphAxis -> ReducedAdr -> Bool
hasConflict axis adr = any ((== axis) . conflictCandidateAxis) (conflictCandidates adr)

conflictCandidates :: ReducedAdr -> [ConflictCandidate]
conflictCandidates adr = maybe [] adrConflictCandidates (classifyAdrConflict adr)

requireLifecycle :: GraphAxis -> AdrId -> ReducedAdr -> Either ReconciliationError ()
requireLifecycle axis target current =
  case axis of
    StatusAxis -> Right ()
    _
      | statusLabel current /= "active" ->
          Left (ReconciliationTargetNotActive axis target (statusLabel current))
      | axis == DecisionAxis,
        domainHeadCount > 1 ->
          Left (ReconciliationDecisionDomainConflict target domainHeadCount)
      | otherwise -> Right ()
  where
    domainHeadCount = length (axisResolutionHeads (reducedDomainAxis current))

statusLabel :: ReducedAdr -> Text
statusLabel adr =
  case axisResolutionEffective (reducedStatusAxis adr) of
    Just status
      | reducedStatusState status == StatusActive -> "active"
      | otherwise -> "obsolete"
    Nothing
      | null (axisResolutionHeads (reducedStatusAxis adr)) -> "missing"
      | otherwise -> "conflict"

buildAppends :: GraphReduction -> ReducedAdr -> AdrId -> ReconciliationChoice -> Either ReconciliationError [ManagedRecord]
buildAppends before current target choice =
  case choice of
    ReconcileDecision resolution -> buildDecision current target resolution
    ReconcileScope resolution -> buildScope current target resolution
    ReconcileDomain resolution -> buildDomain current target resolution
    ReconcileStatus resolution -> buildStatus before current target resolution

buildDecision :: ReducedAdr -> AdrId -> DecisionResolution -> Either ReconciliationError [ManagedRecord]
buildDecision current target resolution = do
  requireSubject target (decisionResolutionSubject resolution)
  let newDecision =
        (decisionResolutionDecision resolution)
          { decisionDomains = axisResolutionEffective (reducedDomainAxis current)
          }
  requireSubject target (decisionAdr newDecision)
  case renderManagedSemantic (ManagedDecision newDecision) of
    Left err -> Left (ReconciliationInvalidDecision (Text.pack (show err)))
    Right _ -> Right ()
  let amendment =
        ConnectionRecord
          { connectionRecordId = decisionResolutionAmendmentId resolution,
            connectionPayload =
              AmendsConnection
                AmendsPayload
                  { amendsSubjectAdr = target,
                    amendsFromRecord = decisionRecord newDecision,
                    amendsToRecords = axisResolutionHeads (reducedDecisionAxis current)
                  },
            connectionRationale = decisionResolutionRationale resolution
          }
  Right [ManagedDecision newDecision, ManagedConnection amendment]

buildScope :: ReducedAdr -> AdrId -> ScopeResolution -> Either ReconciliationError [ManagedRecord]
buildScope current target resolution = do
  requireSubject target (scopeResolutionSubject resolution)
  let parents = axisResolutionHeads (reducedScopeAxis current)
      parentSet = scopeParentUnion parents (reducedScopeHistory current)
      effective = Set.fromList (scopeResolutionEffective resolution)
      payload =
        AppliesToPayload
          { appliesToSubjectAdr = target,
            appliesToParentConnections = parents,
            appliesToChange = "merge",
            appliesToAdded = Set.toAscList (effective `Set.difference` parentSet),
            appliesToRemoved = Set.toAscList (parentSet `Set.difference` effective),
            appliesToEffective = Set.toAscList effective
          }
  Right
    [ ManagedConnection
        ConnectionRecord
          { connectionRecordId = scopeResolutionConnectionId resolution,
            connectionPayload = AppliesToConnection payload,
            connectionRationale = scopeResolutionRationale resolution
          }
    ]

buildDomain :: ReducedAdr -> AdrId -> DomainResolution -> Either ReconciliationError [ManagedRecord]
buildDomain current target resolution = do
  requireSubject target (domainResolutionSubject resolution)
  let parents = axisResolutionHeads (reducedDomainAxis current)
      parentSet = domainParentUnion parents (reducedDomainHistory current)
      effective = Set.fromList (domainResolutionEffective resolution)
      payload =
        DomainsPayload
          { domainsSubjectAdr = target,
            domainsParentConnections = parents,
            domainsChange = "merge",
            domainsAdded = Set.toAscList (effective `Set.difference` parentSet),
            domainsRemoved = Set.toAscList (parentSet `Set.difference` effective),
            domainsEffective = Set.toAscList effective,
            domainsRefinements = []
          }
  Right
    [ ManagedConnection
        ConnectionRecord
          { connectionRecordId = domainResolutionConnectionId resolution,
            connectionPayload = DomainsConnection payload,
            connectionRationale = domainResolutionRationale resolution
          }
    ]

buildStatus :: GraphReduction -> ReducedAdr -> AdrId -> StatusResolution -> Either ReconciliationError [ManagedRecord]
buildStatus before current target resolution = do
  requireSubject target (statusResolutionSubject resolution)
  if statusResolutionExplicit resolution
    then Right ()
    else Left ReconciliationStatusRequiresExplicitResolution
  validateStatusChoice before target resolution
  let payload =
        StatusPayload
          { statusSubjectAdr = target,
            statusParentConnections = axisResolutionHeads (reducedStatusAxis current),
            statusState = statusResolutionState resolution,
            statusRecordHeads = axisResolutionHeads (reducedDecisionAxis current),
            statusReplacementAdr = statusResolutionReplacement resolution
          }
  Right
    [ ManagedConnection
        ConnectionRecord
          { connectionRecordId = statusResolutionConnectionId resolution,
            connectionPayload = StatusConnection payload,
            connectionRationale = statusResolutionRationale resolution
          }
    ]

validateStatusChoice :: GraphReduction -> AdrId -> StatusResolution -> Either ReconciliationError ()
validateStatusChoice reduction target resolution =
  case (statusResolutionState resolution, statusResolutionReplacement resolution) of
    (StatusActive, Just _) ->
      Left (ReconciliationInvalidStatus "active status may not name a replacement ADR")
    (_, Just replacement)
      | replacement == target ->
          Left (ReconciliationInvalidStatus "an ADR cannot replace itself")
      | otherwise ->
          case lookupReducedAdr replacement reduction of
            Just replacementAdr
              | replacementIsActive replacementAdr -> Right ()
              | otherwise -> Left (ReconciliationReplacementNotActive replacement)
            Nothing ->
              Left
                ( ReconciliationInvalidStatus
                    ("replacement ADR does not exist: " <> adrIdText replacement)
                )
    _ -> Right ()

replacementIsActive :: ReducedAdr -> Bool
replacementIsActive adr =
  case axisResolutionEffective (reducedStatusAxis adr) of
    Just status -> reducedStatusState status == StatusActive
    Nothing -> False

requireSubject :: AdrId -> AdrId -> Either ReconciliationError ()
requireSubject expected actual
  | expected == actual = Right ()
  | otherwise = Left (ReconciliationWrongSubject expected actual)

validateFresh :: [ManagedRecord] -> [ManagedRecord] -> Either ReconciliationError ()
validateFresh original = mapM_ fresh
  where
    existingRecords = Set.fromList [decisionRecord record | ManagedDecision record <- original]
    existingConnections = Set.fromList [connectionRecordId record | ManagedConnection record <- original]
    fresh (ManagedDecision record)
      | decisionRecord record `Set.member` existingRecords =
          Left (ReconciliationReusedRecordId (decisionRecord record))
      | otherwise = Right ()
    fresh (ManagedConnection record)
      | connectionRecordId record `Set.member` existingConnections =
          Left (ReconciliationReusedConnectionId (connectionRecordId record))
      | otherwise = Right ()

validateGenerated :: ManagedRecord -> Either ReconciliationError ()
validateGenerated record =
  case renderManagedSemantic record of
    Left err -> Left (ReconciliationInvalidRecord (Text.pack (show err)))
    Right _ -> Right ()

preserveOtherAxes :: GraphAxis -> ReducedAdr -> ReducedAdr -> Either ReconciliationError ()
preserveOtherAxes chosen before after =
  case find changed comparisonAxes of
    Just axis -> Left (ReconciliationChangedOtherConflict axis)
    Nothing -> Right ()
  where
    comparisonAxes =
      [ axis
        | axis <- [DecisionAxis, ScopeAxis, DomainAxis, StatusAxis],
          axis /= chosen
      ]
    changed axis = axisSnapshot axis before /= axisSnapshot axis after

data AxisSnapshot
  = DecisionSnapshot (AxisResolution RecordId (Maybe DecisionRecord)) [DecisionRecord] [ConnectionRecord]
  | ScopeSnapshot (AxisResolution ConnectionId [ScopePattern]) [ConnectionRecord]
  | DomainSnapshot (AxisResolution ConnectionId [Domain]) [ConnectionRecord]
  | StatusSnapshot (AxisResolution ConnectionId (Maybe ReducedStatus)) [ConnectionRecord]
  deriving (Eq, Show)

axisSnapshot :: GraphAxis -> ReducedAdr -> AxisSnapshot
axisSnapshot axis adr =
  case axis of
    DecisionAxis ->
      DecisionSnapshot
        (reducedDecisionAxis adr)
        (reducedDecisionHistory adr)
        (reducedAmendmentHistory adr)
    ScopeAxis -> ScopeSnapshot (reducedScopeAxis adr) (reducedScopeHistory adr)
    DomainAxis -> DomainSnapshot (reducedDomainAxis adr) (reducedDomainHistory adr)
    StatusAxis -> StatusSnapshot (reducedStatusAxis adr) (reducedStatusHistory adr)

scopeParentUnion :: [ConnectionId] -> [ConnectionRecord] -> Set ScopePattern
scopeParentUnion parents history =
  Set.unions
    [ Set.fromList (appliesToEffective payload)
      | parent <- parents,
        Just connection <- [find ((== parent) . connectionRecordId) history],
        AppliesToConnection payload <- [connectionPayload connection]
    ]

domainParentUnion :: [ConnectionId] -> [ConnectionRecord] -> Set Domain
domainParentUnion parents history =
  Set.unions
    [ Set.fromList (domainsEffective payload)
      | parent <- parents,
        Just connection <- [find ((== parent) . connectionRecordId) history],
        DomainsConnection payload <- [connectionPayload connection]
    ]

staleStateMessage :: StateToken -> StateToken -> Text
staleStateMessage expected current =
  "stale ADR state: expected "
    <> stateTokenText expected
    <> ", current state is "
    <> stateTokenText current

axisText :: GraphAxis -> Text
axisText axis =
  case axis of
    DecisionAxis -> "decision"
    ScopeAxis -> "scope"
    DomainAxis -> "domain"
    StatusAxis -> "status"
