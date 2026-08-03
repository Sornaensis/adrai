{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Pure revision-local show, comparison, and logical-reference projections.
module Adrai.Query
  ( ProjectionMode (..),
    ProjectionCounts (..),
    ResolutionConflictKind (..),
    ResolutionCandidate (..),
    ResolutionEntry (..),
    ResolutionState (..),
    PublicIssue (..),
    ProvenanceProjection (..),
    CollapsedProvenance (..),
    CollapsedRichDetail (..),
    CollapsedProjection (..),
    ParentDiff (..),
    ExplodedOptions (..),
    ExplodedItem (..),
    ExplodedOperation (..),
    ExplodedProjection (..),
    QueryError (..),
    projectCollapsed,
    projectExploded,
    collapsedProjectionJson,
    explodedProjectionJson,
    renderCollapsedProjection,
    renderExplodedProjection,
    ReferenceMatch (..),
    ReferenceLookupError (..),
    resolveAdrReference,
    referenceLookupErrorText,
    adrVisible,
    CompareOptions (..),
    CompareCounts (..),
    CompareChange (..),
    CompareSnapshot (..),
    CompareEntry (..),
    CompareProjection (..),
    compareSnapshots,
    compareProjectionJson,
    renderCompareProjection,
    SearchRequest (..),
    defaultSearchRequest,
    SearchError (..),
    ScopeFilterMatch (..),
    SearchMatches (..),
    SearchResult (..),
    SearchProjection (..),
    domainsMatchRequested,
    semanticSummaryText,
    searchDocumentLexicalFields,
    sortCandidateIds,
    chooseBestByAdr,
    sortLogicalAdrs,
    runCurrentSearch,
    searchProjectionJson,
    renderSearchProjection,
  )
where

import Adrai.Domain (Domain, DomainError, canonicalDomains, domainIsWithin, domainRefinementText, domainText)
import Adrai.Format (renderDigest)
import Adrai.Format.Document
import Adrai.Format.Json
import Adrai.Graph
import Adrai.History
import Adrai.Provenance
import Adrai.Retrieval
import Adrai.Scope (ScopePattern, scopeMatches, scopePatternText)
import Adrai.Sqlite
  ( FtsHit (..),
    RetrievalSqlError,
    SummaryFtsCandidates (..),
    mkCandidateLimit,
    runSummaryFtsChannels,
  )
import Adrai.Types
import Adrai.Vector
  ( DenseVector,
    VectorError,
    dot,
    identifierEmbedding,
    semanticEmbedding,
  )
import Data.Array (Array, (!), listArray)
import Data.ByteString (ByteString)
import Data.List (find, sort, sortBy, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Ord (Down (..), comparing)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple (Connection)

data ProjectionMode = CompactProjection | RichProjection
  deriving (Eq, Ord, Show)

data ProjectionCounts = ProjectionCounts
  { projectionEditions :: Int,
    projectionAmendments :: Int,
    projectionScopeRevisions :: Int,
    projectionDomainRevisions :: Int,
    projectionStatusRevisions :: Int
  }
  deriving (Eq, Show)

data ResolutionConflictKind
  = DecisionConflict
  | ScopeConflict
  | DomainConflict
  | StatusConflict
  | IntegrityConflict
  deriving (Eq, Ord, Show)

data ResolutionCandidate = ResolutionCandidate
  { resolutionCandidateId :: Text,
    resolutionCandidateSummary :: Text
  }
  deriving (Eq, Show)

data ResolutionEntry = ResolutionEntry
  { resolutionConflictKind :: ResolutionConflictKind,
    resolutionHeadCount :: Int,
    resolutionHeads :: [Text],
    resolutionSummary :: Text,
    resolutionCandidates :: [ResolutionCandidate]
  }
  deriving (Eq, Show)

data ResolutionState = ResolutionState
  { resolutionStateResolved :: Bool,
    resolutionStateRequired :: Bool,
    resolutionStateConflicts :: [ResolutionEntry]
  }
  deriving (Eq, Show)

data PublicIssue = PublicIssue
  { publicIssueSeverity :: Text,
    publicIssueCode :: Text,
    publicIssueObjectId :: Maybe Text,
    publicIssuePath :: Maybe Text,
    publicIssueMessage :: Text
  }
  deriving (Eq, Show)

data ProvenanceProjection = ProvenanceProjection
  { projectedOperation :: OperationId,
    projectedClaimedAt :: Integer,
    projectedActor :: Actor,
    projectedBasis :: Text,
    projectedCommit :: Maybe Text,
    projectedPlacement :: Maybe Text,
    projectedOriginalCommits :: [Text],
    projectedIntroductions :: [Text],
    projectedLineLandings :: [LineLandingEvidence]
  }
  deriving (Eq, Show)

data CollapsedProvenance = CollapsedProvenance
  { collapsedCreatedProvenance :: Maybe ProvenanceProjection,
    collapsedEffectiveProvenance :: Maybe ProvenanceProjection,
    collapsedDomainsProvenance :: Maybe ProvenanceProjection,
    collapsedStatusProvenance :: Maybe ProvenanceProjection
  }
  deriving (Eq, Show)

data CollapsedRichDetail = CollapsedRichDetail
  { richRawConflicts :: [Text],
    richRecordHeads :: [Text],
    richScopeHeads :: [Text],
    richDomainHeads :: [Text],
    richStatusHeads :: [Text],
    richCandidateRecords :: [JsonValue],
    richCandidateScopes :: [JsonValue],
    richCandidateDomains :: [JsonValue],
    richSourcePaths :: [Text],
    richDecisionHistory :: [Text],
    richConnectionHistory :: [Text]
  }
  deriving (Eq, Show)

data CollapsedProjection = CollapsedProjection
  { collapsedRevision :: RevisionIdentity,
    collapsedMode :: ProjectionMode,
    collapsedAdr :: AdrId,
    collapsedRecord :: Maybe RecordId,
    collapsedTitle :: Text,
    collapsedSummary :: Text,
    collapsedBody :: Text,
    collapsedDomains :: [Text],
    collapsedAppliesTo :: [Text],
    collapsedStatus :: Text,
    collapsedObsolete :: Bool,
    collapsedReplacement :: Maybe AdrId,
    collapsedResolution :: ResolutionState,
    collapsedStateToken :: StateToken,
    collapsedCounts :: ProjectionCounts,
    collapsedEvolution :: EvolutionSummary,
    collapsedProvenance :: CollapsedProvenance,
    collapsedIssues :: [PublicIssue],
    collapsedRichDetail :: Maybe CollapsedRichDetail
  }
  deriving (Eq, Show)

data ParentDiff = ParentDiff
  { parentDiffParent :: RecordId,
    parentDiffText :: Text
  }
  deriving (Eq, Show)

data ExplodedOptions = ExplodedOptions
  { explodedIncludeRawSemantic :: Bool
  }
  deriving (Eq, Show)

data ExplodedItem = ExplodedItem
  { explodedItemId :: Text,
    explodedItemType :: Text,
    explodedItemEvent :: Text,
    explodedItemOperation :: OperationId,
    explodedItemPath :: Text,
    explodedItemTitle :: Maybe Text,
    explodedItemSummary :: Maybe Text,
    explodedItemDomains :: [Text],
    explodedItemBody :: Maybe Text,
    explodedItemParents :: [Text],
    explodedItemDiffs :: [ParentDiff],
    explodedItemRelation :: Maybe Text,
    explodedItemRationale :: Maybe Text,
    explodedItemMetadata :: Maybe JsonValue,
    explodedItemRawSemantic :: Maybe Text
  }
  deriving (Eq, Show)

data ExplodedOperation = ExplodedOperation
  { explodedOperationId :: OperationId,
    explodedOperationProvenance :: ProvenanceProjection,
    explodedOperationFullProvenance :: JsonValue,
    explodedOperationItems :: [ExplodedItem]
  }
  deriving (Eq, Show)

data ExplodedProjection = ExplodedProjection
  { explodedRevision :: RevisionIdentity,
    explodedAdr :: AdrId,
    explodedResolution :: ResolutionState,
    explodedStateToken :: StateToken,
    explodedOperations :: [ExplodedOperation]
  }
  deriving (Eq, Show)

data QueryError
  = QueryAdrNotFound AdrId
  | QuerySnapshotInvalid SnapshotConsistencyError
  deriving (Eq, Show)

-- Search ---------------------------------------------------------------------

data SearchRequest = SearchRequest
  { searchRequestQuery :: Text,
    searchRequestMode :: RetrievalMode,
    searchRequestView :: ViewMode,
    searchRequestIncludeObsolete :: Bool,
    searchRequestDomains :: [Text],
    searchRequestFile :: Maybe RepoPath,
    searchRequestActor :: Maybe ActorSelector,
    searchRequestSince :: Maybe Integer,
    searchRequestUntil :: Maybe Integer,
    searchRequestLimit :: Int,
    searchRequestShallowHistory :: Bool
  }
  deriving (Eq, Show)

defaultSearchRequest :: Text -> SearchRequest
defaultSearchRequest query =
  SearchRequest
    { searchRequestQuery = query,
      searchRequestMode = HybridRetrieval,
      searchRequestView = CollapsedView,
      searchRequestIncludeObsolete = False,
      searchRequestDomains = [],
      searchRequestFile = Nothing,
      searchRequestActor = Nothing,
      searchRequestSince = Nothing,
      searchRequestUntil = Nothing,
      searchRequestLimit = 10,
      searchRequestShallowHistory = False
    }

data SearchError
  = SearchSnapshotInvalid SnapshotConsistencyError
  | SearchInvalidLimit Int
  | SearchUnsupportedView ViewMode
  | SearchInvalidDomains DomainError
  | SearchInvalidTimeRange Integer Integer
  | SearchMaterializationMismatch Text
  | SearchSqlFailure RetrievalSqlError
  | SearchVectorFailure VectorError
  deriving (Eq, Show)

data ScopeFilterMatch = ScopeExact | ScopeAmbiguous | ScopeNone
  deriving (Eq, Ord, Show)

data SearchMatches = SearchMatches
  { searchMatchFts :: Bool,
    searchMatchVector :: Bool,
    searchMatchFileScope :: Maybe ScopeFilterMatch,
    searchMatchDomains :: [Text],
    searchMatchActor :: Maybe Text,
    searchMatchFields :: [Text],
    searchMatchTerms :: [Text],
    searchMatchExactPhraseFields :: [Text],
    searchMatchIdentifierTerms :: [Text],
    searchMatchChannelRanks :: Map RetrievalChannel Int,
    searchMatchBestSection :: Maybe SectionKind,
    searchMatchBestSectionScore :: Maybe Double
  }
  deriving (Eq, Show)

data SearchResult = SearchResult
  { searchResultAdr :: AdrId,
    searchResultRecord :: Maybe RecordId,
    searchResultMatchedCandidate :: Maybe Text,
    searchResultMatchedRecord :: Maybe RecordId,
    searchResultMatchedTitle :: Maybe Text,
    searchResultMatchedSummary :: Maybe Text,
    searchResultTitle :: Text,
    searchResultSummary :: Text,
    searchResultDomains :: [Text],
    searchResultAppliesTo :: [Text],
    searchResultStatus :: Text,
    searchResultObsolete :: Bool,
    searchResultReplacement :: Maybe AdrId,
    searchResultConflict :: Maybe Text,
    searchResultResolution :: ResolutionState,
    searchResultStateToken :: StateToken,
    searchResultScore :: Double,
    searchResultFtsScore :: Double,
    searchResultVectorScore :: Double,
    searchResultIdentifierVectorScore :: Double,
    searchResultSourcePaths :: [Text],
    searchResultMatches :: SearchMatches,
    searchResultRetrieval :: JsonValue,
    searchResultCounts :: ProjectionCounts,
    searchResultScopeAmbiguous :: Bool,
    searchResultDomainAmbiguous :: Bool,
    searchResultConflicted :: Bool,
    searchResultShallowHistory :: Bool
  }
  deriving (Eq, Show)

data SearchProjection = SearchProjection
  { searchProjectionRevision :: RevisionIdentity,
    searchProjectionMode :: RetrievalMode,
    searchProjectionLimit :: Int,
    searchProjectionResults :: [SearchResult]
  }
  deriving (Eq, Show)

projectCollapsed :: ProjectionMode -> ReadSnapshot -> AdrId -> Either QueryError CollapsedProjection
projectCollapsed mode snapshot adr = do
  validateQuerySnapshot snapshot
  case lookupReducedAdr adr (readSnapshotReduction snapshot) of
    Nothing -> Left (QueryAdrNotFound adr)
    Just reduced -> Right (materializeCollapsed mode snapshot reduced)

materializeCollapsed :: ProjectionMode -> ReadSnapshot -> ReducedAdr -> CollapsedProjection
materializeCollapsed mode snapshot reduced =
  CollapsedProjection
    { collapsedRevision = readSnapshotRevision snapshot,
      collapsedMode = mode,
      collapsedAdr = reducedAdrId reduced,
      collapsedRecord = decisionRecord <$> effectiveDecision,
      collapsedTitle = logicalDecisionTitle reduced,
      collapsedSummary = logicalDecisionSummary reduced,
      collapsedBody = maybe conflictBody (Text.strip . decisionBody) effectiveDecision,
      collapsedDomains = map domainText (axisResolutionEffective (reducedDomainAxis reduced)),
      collapsedAppliesTo = map scopePatternText effectiveScope,
      collapsedStatus = statusText effectiveStatus,
      collapsedObsolete = maybe False ((== StatusObsolete) . reducedStatusState) effectiveStatus,
      collapsedReplacement = reducedStatusReplacement =<< effectiveStatus,
      collapsedResolution = resolution,
      collapsedStateToken = reducedStateToken reduced,
      collapsedCounts = countsFor reduced,
      collapsedEvolution = summarizeEvolution (historyOperationsOldestFirst snapshot (Just (reducedAdrId reduced))),
      collapsedProvenance = provenanceFor snapshot reduced,
      collapsedIssues = publicIssuesFor snapshot reduced,
      collapsedRichDetail = if mode == RichProjection then Just (richDetailFor snapshot reduced) else Nothing
    }
  where
    effectiveDecision = axisResolutionEffective (reducedDecisionAxis reduced)
    decisionCandidates =
      [ decision
        | identifier <- axisResolutionHeads (reducedDecisionAxis reduced),
          decision <- reducedDecisionHistory reduced,
          decisionRecord decision == identifier
      ]
    conflictBody = Text.intercalate "\n\n" (map (Text.strip . decisionBody) decisionCandidates)
    scopeAxis = reducedScopeAxis reduced
    effectiveScope = if axisProjectionResolved scopeAxis then axisResolutionEffective scopeAxis else []
    statusAxis = reducedStatusAxis reduced
    effectiveStatus = if axisProjectionResolved statusAxis then axisResolutionEffective statusAxis else Nothing
    resolution = resolutionFor snapshot reduced

logicalDecisionTitle :: ReducedAdr -> Text
logicalDecisionTitle reduced =
  maybe "[conflicted ADR]" decisionTitle (axisResolutionEffective (reducedDecisionAxis reduced))

logicalDecisionSummary :: ReducedAdr -> Text
logicalDecisionSummary reduced =
  maybe conflictSummary decisionSummary (axisResolutionEffective (reducedDecisionAxis reduced))
  where
    conflictSummary = Text.intercalate "; " (map decisionSummary (logicalDecisionCandidates reduced))

logicalDecisionCandidates :: ReducedAdr -> [DecisionRecord]
logicalDecisionCandidates reduced =
  [ decision
    | identifier <- axisResolutionHeads (reducedDecisionAxis reduced),
      decision <- reducedDecisionHistory reduced,
      decisionRecord decision == identifier
  ]

axisProjectionResolved :: AxisResolution identifier effective -> Bool
axisProjectionResolved resolution = length (axisResolutionHeads resolution) == 1 && axisResolutionConflict resolution == Nothing

resolutionFor :: ReadSnapshot -> ReducedAdr -> ResolutionState
resolutionFor snapshot reduced =
  ResolutionState
    { resolutionStateResolved = null conflicts,
      resolutionStateRequired = not (null conflicts),
      resolutionStateConflicts = conflicts
    }
  where
    axisConflicts = catMaybes [decisionEntry, scopeEntry, domainEntry, statusEntry]
    decisionEntry = axisResolutionEntry DecisionConflict recordIdText (reducedDecisionAxis reduced) decisionCandidates
    scopeEntry = axisResolutionEntry ScopeConflict connectionIdText (reducedScopeAxis reduced) scopeCandidates
    domainEntry = axisResolutionEntry DomainConflict connectionIdText (reducedDomainAxis reduced) domainCandidates
    statusEntry = axisResolutionEntry StatusConflict connectionIdText (reducedStatusAxis reduced) statusCandidates
    represented = Set.fromList (map resolutionSummary axisConflicts)
    rawSummaries = rawConflictSummaries snapshot reduced
    unmatched =
      [ ResolutionEntry IntegrityConflict 0 [] summary []
        | summary <- rawSummaries,
          Set.notMember summary represented
      ]
    conflicts = axisConflicts <> unmatched
    decisionCandidates =
      [ ResolutionCandidate (recordIdText identifier) (maybe "missing decision record" decisionSummary (decisionById identifier reduced))
        | identifier <- axisResolutionHeads (reducedDecisionAxis reduced)
      ]
    scopeCandidates =
      [ResolutionCandidate (connectionIdText identifier) (Text.intercalate ", " (map scopePatternText (scopeEffective identifier reduced))) | identifier <- axisResolutionHeads (reducedScopeAxis reduced)]
    domainCandidates =
      [ResolutionCandidate (connectionIdText identifier) (Text.intercalate ", " (map domainText (domainEffective identifier reduced))) | identifier <- axisResolutionHeads (reducedDomainAxis reduced)]
    statusCandidates =
      [ResolutionCandidate (connectionIdText identifier) (maybe "invalid status head" reducedStatusText (statusEffective identifier reduced)) | identifier <- axisResolutionHeads (reducedStatusAxis reduced)]

axisResolutionEntry :: ResolutionConflictKind -> (identifier -> Text) -> AxisResolution identifier effective -> [ResolutionCandidate] -> Maybe ResolutionEntry
axisResolutionEntry kind renderIdentifier resolution candidates
  | length heads == 1 && axisResolutionConflict resolution == Nothing = Nothing
  | otherwise =
      Just
        ResolutionEntry
          { resolutionConflictKind = if null heads then IntegrityConflict else kind,
            resolutionHeadCount = length heads,
            resolutionHeads = map renderIdentifier heads,
            resolutionSummary = fromMaybe (Text.pack (show (length heads)) <> " " <> conflictKindText kind <> " heads") (axisResolutionConflict resolution),
            resolutionCandidates = candidates
          }
  where
    heads = axisResolutionHeads resolution

conflictKindText :: ResolutionConflictKind -> Text
conflictKindText kind =
  case kind of
    DecisionConflict -> "decision"
    ScopeConflict -> "scope"
    DomainConflict -> "domain"
    StatusConflict -> "status"
    IntegrityConflict -> "integrity"

decisionById :: RecordId -> ReducedAdr -> Maybe DecisionRecord
decisionById identifier = find ((== identifier) . decisionRecord) . reducedDecisionHistory

scopeEffective :: ConnectionId -> ReducedAdr -> [ScopePattern]
scopeEffective identifier reduced =
  case find ((== identifier) . connectionRecordId) (reducedScopeHistory reduced) of
    Just connection -> case connectionPayload connection of AppliesToConnection payload -> appliesToEffective payload; _ -> []
    Nothing -> []

domainEffective :: ConnectionId -> ReducedAdr -> [Domain]
domainEffective identifier reduced =
  case find ((== identifier) . connectionRecordId) (reducedDomainHistory reduced) of
    Just connection -> case connectionPayload connection of DomainsConnection payload -> domainsEffective payload; _ -> []
    Nothing -> []

statusEffective :: ConnectionId -> ReducedAdr -> Maybe ReducedStatus
statusEffective identifier reduced =
  case find ((== identifier) . connectionRecordId) (reducedStatusHistory reduced) of
    Just connection ->
      case connectionPayload connection of
        StatusConnection payload -> Just (ReducedStatus (statusState payload) (sort (statusRecordHeads payload)) (statusReplacementAdr payload))
        _ -> Nothing
    Nothing -> Nothing

reducedStatusText :: ReducedStatus -> Text
reducedStatusText = statusText . Just

statusText :: Maybe ReducedStatus -> Text
statusText status =
  case status of
    Nothing -> "conflict"
    Just value -> case reducedStatusState value of StatusActive -> "active"; StatusObsolete -> "obsolete"

countsFor :: ReducedAdr -> ProjectionCounts
countsFor reduced =
  ProjectionCounts
    (length (reducedDecisionHistory reduced))
    (length (reducedAmendmentHistory reduced))
    (length (reducedScopeHistory reduced))
    (length (reducedDomainHistory reduced))
    (length (reducedStatusHistory reduced))

adrGraphIssues :: ReadSnapshot -> AdrId -> [GraphIssue]
adrGraphIssues snapshot adr = [issue | issue <- graphReductionIssues (readSnapshotReduction snapshot), graphIssueAdr issue == Just adr]

rawConflictSummaries :: ReadSnapshot -> ReducedAdr -> [Text]
rawConflictSummaries snapshot reduced =
  stableUnique
    ( reducedConflictMessages reduced
        <> [Text.pack (show (length issues)) <> " integrity errors" | not (null issues)]
    )
  where
    issues = adrGraphIssues snapshot (reducedAdrId reduced)

publicIssuesFor :: ReadSnapshot -> ReducedAdr -> [PublicIssue]
publicIssuesFor snapshot reduced = map convert (adrGraphIssues snapshot (reducedAdrId reduced))
  where
    convert issue =
      PublicIssue
        { publicIssueSeverity = "error",
          publicIssueCode = graphIssueCodeText (graphIssueCode issue),
          publicIssueObjectId = objectRefText <$> graphIssueObject issue,
          publicIssuePath =
            (\objectRef -> nonEmptyText (pathForObject snapshot (provenanceObjectFromRef objectRef)))
              =<< graphIssueObject issue,
          publicIssueMessage = graphIssueMessage issue
        }

richDetailFor :: ReadSnapshot -> ReducedAdr -> CollapsedRichDetail
richDetailFor snapshot reduced =
  CollapsedRichDetail
    { richRawConflicts = rawConflictSummaries snapshot reduced,
      richRecordHeads = map recordIdText (axisResolutionHeads (reducedDecisionAxis reduced)),
      richScopeHeads = map connectionIdText (axisResolutionHeads (reducedScopeAxis reduced)),
      richDomainHeads = map connectionIdText (axisResolutionHeads (reducedDomainAxis reduced)),
      richStatusHeads = map connectionIdText (axisResolutionHeads (reducedStatusAxis reduced)),
      richCandidateRecords =
        [ object
            [ ("path", JsonString (pathForObject snapshot (ProvenanceRecord identifier))),
              ("record", JsonString (recordIdText identifier)),
              ("summary", JsonString (decisionSummary decision)),
              ("title", JsonString (decisionTitle decision))
            ]
          | identifier <- axisResolutionHeads (reducedDecisionAxis reduced),
            Just decision <- [decisionById identifier reduced]
        ],
      richCandidateScopes =
        [ object
            [ ("applies_to", textArray (sortedUnique (map scopePatternText (scopeEffective identifier reduced)))),
              ("connection", JsonString (connectionIdText identifier)),
              ("path", JsonString (pathForObject snapshot (ProvenanceConnection identifier)))
            ]
          | identifier <- axisResolutionHeads (reducedScopeAxis reduced)
            , any ((== identifier) . connectionRecordId) (reducedScopeHistory reduced)
        ],
      richCandidateDomains =
        [ object
            [ ("connection", JsonString (connectionIdText identifier)),
              ("domains", textArray (sortedUnique (map domainText (domainEffective identifier reduced)))),
              ("path", JsonString (pathForObject snapshot (ProvenanceConnection identifier)))
            ]
          | identifier <- axisResolutionHeads (reducedDomainAxis reduced)
            , any ((== identifier) . connectionRecordId) (reducedDomainHistory reduced)
        ],
      richSourcePaths =
        mapMaybe
          (fmap (repoPathText . parsedManagedPath) . (`findDocument` snapshot))
          ( catMaybes
              [ singletonHead ProvenanceRecord (axisResolutionHeads (reducedDecisionAxis reduced)),
                singletonHead ProvenanceConnection (axisResolutionHeads (reducedScopeAxis reduced)),
                singletonHead ProvenanceConnection (axisResolutionHeads (reducedDomainAxis reduced)),
                singletonHead ProvenanceConnection (axisResolutionHeads (reducedStatusAxis reduced))
              ]
          ),
      richDecisionHistory = map (recordIdText . decisionRecord) (reducedDecisionHistory reduced),
      richConnectionHistory =
        map (connectionIdText . connectionRecordId)
          (reducedAmendmentHistory reduced <> reducedScopeHistory reduced <> reducedDomainHistory reduced <> reducedStatusHistory reduced)
    }

provenanceFor :: ReadSnapshot -> ReducedAdr -> CollapsedProvenance
provenanceFor snapshot reduced =
  CollapsedProvenance
    { collapsedCreatedProvenance = projectionForDocument snapshot =<< created,
      collapsedEffectiveProvenance = projectionForDocument snapshot =<< effective,
      collapsedDomainsProvenance = projectionForObject snapshot =<< singletonHead ProvenanceConnection (axisResolutionHeads (reducedDomainAxis reduced)),
      collapsedStatusProvenance = projectionForObject snapshot =<< singletonHead ProvenanceConnection (axisResolutionHeads (reducedStatusAxis reduced))
    }
  where
    decisionDocs = [document | document <- readSnapshotDocuments snapshot, ownerAdr (parsedManagedRecord document) == reducedAdrId reduced, isDecision (parsedManagedRecord document)]
    created = do
      operation <- historyOperationId <$> find ((== "created") . historyOperationLabel) (historyOperationsOldestFirst snapshot (Just (reducedAdrId reduced)))
      find ((== operation) . provenanceOperationId . parsedManagedCapsule) decisionDocs
    effective = case axisResolutionHeads (reducedDecisionAxis reduced) of [identifier] -> findDocument (ProvenanceRecord identifier) snapshot; _ -> Nothing

projectionForObject :: ReadSnapshot -> ProvenanceObjectId -> Maybe ProvenanceProjection
projectionForObject snapshot objectId = projectionForDocument snapshot =<< findDocument objectId snapshot

projectionForDocument :: ReadSnapshot -> ParsedManagedDocument -> Maybe ProvenanceProjection
projectionForDocument snapshot document =
  Just
    ProvenanceProjection
      { projectedOperation = operation,
        projectedClaimedAt = provenanceTimestampMs capsule,
        projectedActor = provenanceActor capsule,
        projectedBasis = gitOidText (provenanceBasis capsule),
        projectedCommit = preferredCommit =<< evidence,
        projectedPlacement = preferredClassification =<< evidence,
        projectedOriginalCommits = maybe [] originalOperationCommits evidence,
        projectedIntroductions = maybe [] introductionCommits evidence,
        projectedLineLandings = maybe [] (sortOn (\landing -> (landingLine landing, landingRef landing)) . placementLineLandings) evidence
      }
  where
    capsule = parsedManagedCapsule document
    operation = provenanceOperationId capsule
    evidence = Map.lookup operation (readSnapshotPlacement snapshot)

preferredCommit :: PlacementEvidence -> Maybe Text
preferredCommit evidence =
  case preferredPlacements evidence of
    placement : _ -> Just (commitPlacementOid placement)
    [] -> placementCommit evidence

preferredClassification :: PlacementEvidence -> Maybe Text
preferredClassification evidence =
  case preferredPlacements evidence of
    placement : _ -> Just (commitPlacementClassification placement)
    [] -> placementLabel evidence

originalOperationCommits :: PlacementEvidence -> [Text]
originalOperationCommits evidence
  | null placements = placementOriginalCommits evidence
  | otherwise = classifiedPlacementOids "original" placements
  where
    placements = publicPlacements evidence

introductionCommits :: PlacementEvidence -> [Text]
introductionCommits evidence
  | null placements = placementIntroductions evidence
  | otherwise = classifiedPlacementOids "introduction" placements
  where
    placements = publicPlacements evidence

preferredPlacements :: PlacementEvidence -> [CommitPlacementEvidence]
preferredPlacements = sortOn placementPreference . placementCommits
  where
    placementPreference placement =
      ( if commitPlacementReachable placement then (0 :: Int) else 1,
        classificationRank (commitPlacementClassification placement),
        commitPlacementCommittedAtMs placement,
        commitPlacementOid placement
      )
    classificationRank classification =
      case classification of
        "original" -> (0 :: Int)
        "introduction" -> 1
        "copy" -> 2
        _ -> 3

publicPlacements :: PlacementEvidence -> [CommitPlacementEvidence]
publicPlacements = sortOn placementOrder . placementCommits
  where
    placementOrder placement =
      ( if commitPlacementReachable placement then (0 :: Int) else 1,
        commitPlacementClassification placement,
        commitPlacementCommittedAtMs placement,
        commitPlacementOid placement
      )

classifiedPlacementOids :: Text -> [CommitPlacementEvidence] -> [Text]
classifiedPlacementOids classification placements =
  [ commitPlacementOid placement
    | placement <- placements,
      commitPlacementClassification placement == classification
  ]

projectExploded :: ExplodedOptions -> ReadSnapshot -> AdrId -> Either QueryError ExplodedProjection
projectExploded options snapshot adr = do
  collapsed <- projectCollapsed RichProjection snapshot adr
  let documents = [document | document <- readSnapshotDocuments snapshot, ownerAdr (parsedManagedRecord document) == adr]
      grouped = Map.fromListWith (<>) [(provenanceOperationId (parsedManagedCapsule document), [document]) | document <- documents]
      operations = mapMaybe (makeOperation options snapshot documents) (Map.toAscList grouped)
  Right
    ExplodedProjection
      { explodedRevision = readSnapshotRevision snapshot,
        explodedAdr = adr,
        explodedResolution = collapsedResolution collapsed,
        explodedStateToken = collapsedStateToken collapsed,
        explodedOperations = sortOn operationSortKey operations
      }

makeOperation :: ExplodedOptions -> ReadSnapshot -> [ParsedManagedDocument] -> (OperationId, [ParsedManagedDocument]) -> Maybe ExplodedOperation
makeOperation options snapshot allDocuments (operation, documents) = do
  firstDocument <- firstOf (sortOn documentObjectText documents)
  provenance <- projectionForDocument snapshot firstDocument
  let items = sortOn explodedItemId (map (documentItem options allDocuments) documents)
      fullProvenance = operationFullProvenanceJson snapshot operation documents
  pure (ExplodedOperation operation provenance fullProvenance items)

operationSortKey :: ExplodedOperation -> (Integer, Text)
operationSortKey operation = (projectedClaimedAt (explodedOperationProvenance operation), operationIdText (explodedOperationId operation))

documentItem :: ExplodedOptions -> [ParsedManagedDocument] -> ParsedManagedDocument -> ExplodedItem
documentItem options allDocuments document =
  case parsedManagedRecord document of
    ManagedDecision decision ->
      ExplodedItem
        { explodedItemId = recordIdText (decisionRecord decision),
          explodedItemType = "decision",
          explodedItemEvent = event,
          explodedItemOperation = operation,
          explodedItemPath = path,
          explodedItemTitle = Just (decisionTitle decision),
          explodedItemSummary = Just (decisionSummary decision),
          explodedItemDomains = map domainText (decisionDomains decision),
          explodedItemBody = Just (Text.strip (decisionBody decision)),
          explodedItemParents = map recordIdText parents,
          explodedItemDiffs = decisionDiffs allDocuments document parents,
          explodedItemRelation = Nothing,
          explodedItemRationale = Nothing,
          explodedItemMetadata = Nothing,
          explodedItemRawSemantic = raw
        }
      where
        parents = sort (decisionParents allDocuments (decisionRecord decision))
    ManagedConnection connection ->
      ExplodedItem
        { explodedItemId = connectionIdText (connectionRecordId connection),
          explodedItemType = "connection",
          explodedItemEvent = event,
          explodedItemOperation = operation,
          explodedItemPath = path,
          explodedItemTitle = Nothing,
          explodedItemSummary = Nothing,
          explodedItemDomains = [],
          explodedItemBody = Nothing,
          explodedItemParents = [],
          explodedItemDiffs = [],
          explodedItemRelation = Just (relationText (connectionPayload connection)),
          explodedItemRationale = nonEmptyText (Text.strip (connectionRationale connection)),
          explodedItemMetadata = Just (connectionMetadata (connectionPayload connection)),
          explodedItemRawSemantic = raw
        }
  where
    capsule = parsedManagedCapsule document
    event = eventKindText (provenanceEventKind capsule)
    operation = provenanceOperationId capsule
    path = repoPathText (parsedManagedPath document)
    raw = if explodedIncludeRawSemantic options then Just (parsedManagedSemantic document) else Nothing

connectionMetadata :: ConnectionPayload -> JsonValue
connectionMetadata payload =
  object
    [ ("added", textArray added),
      ("applies_to", textArray appliesTo),
      ("change", maybeJson JsonString change),
      ("domains", textArray domains),
      ("from_record", maybeJson (JsonString . recordIdText) fromRecord),
      ("parent_connections", textArray (map connectionIdText parents)),
      ("record_heads", textArray (map recordIdText recordHeads)),
      ("refinements", textArray refinements),
      ("removed", textArray removed),
      ("replacement", maybeJson (JsonString . adrIdText) replacement),
      ("state", maybeJson JsonString state),
      ("to_records", textArray (map recordIdText toRecords))
    ]
  where
    (fromRecord, toRecords, parents, state, recordHeads, replacement, appliesTo, domains, added, removed, refinements, change) =
      case payload of
        AmendsConnection value -> (Just (amendsFromRecord value), sort (amendsToRecords value), [], Nothing, [], Nothing, [], [], [], [], [], Nothing)
        AppliesToConnection value -> (Nothing, [], sort (appliesToParentConnections value), Nothing, [], Nothing, map scopePatternText (appliesToEffective value), [], map scopePatternText (appliesToAdded value), map scopePatternText (appliesToRemoved value), [], Just (appliesToChange value))
        DomainsConnection value -> (Nothing, [], sort (domainsParentConnections value), Nothing, [], Nothing, [], map domainText (domainsEffective value), map domainText (domainsAdded value), map domainText (domainsRemoved value), map domainRefinementText (domainsRefinements value), Just (domainsChange value))
        StatusConnection value -> (Nothing, [], sort (statusParentConnections value), Just (case statusState value of StatusActive -> "active"; StatusObsolete -> "obsolete"), sort (statusRecordHeads value), statusReplacementAdr value, [], [], [], [], [], Nothing)

decisionParents :: [ParsedManagedDocument] -> RecordId -> [RecordId]
decisionParents documents child =
  Set.toAscList . Set.fromList $
    [parent | document <- documents, ManagedConnection connection <- [parsedManagedRecord document], AmendsConnection payload <- [connectionPayload connection], amendsFromRecord payload == child, parent <- amendsToRecords payload]

decisionDiffs :: [ParsedManagedDocument] -> ParsedManagedDocument -> [RecordId] -> [ParentDiff]
decisionDiffs documents childDocument parents = mapMaybe diffParent parents
  where
    byRecord =
      Map.fromList
        [ (decisionRecord decision, document)
          | document <- documents,
            ManagedDecision decision <- [parsedManagedRecord document]
        ]
    diffParent parent = do
      parentDocument <- Map.lookup parent byRecord
      pure
        ParentDiff
          { parentDiffParent = parent,
            parentDiffText = semanticUnifiedDiff (parsedManagedSemantic parentDocument) (parsedManagedSemantic childDocument)
          }

data DiffAtom = DiffSame Text | DiffRemoved Text | DiffAdded Text
  deriving (Eq, Show)

data SequenceMatch = SequenceMatch
  { matchOldStart :: Int,
    matchNewStart :: Int,
    matchLength :: Int
  }
  deriving (Eq, Show)

semanticUnifiedDiff :: Text -> Text -> Text
semanticUnifiedDiff = unifiedDiffWith "parent" "child" "\n"

compareUnifiedDiff :: Text -> Text -> Text
compareUnifiedDiff = unifiedDiffWith "before" "after" ""

-- | Equivalent to Python's @difflib.unified_diff(..., n=3)@ for the canonical
-- line sequences used by managed semantic documents and comparison fields.
unifiedDiffWith :: Text -> Text -> Text -> Text -> Text -> Text
unifiedDiffWith beforeName afterName controlTerm before after
  | before == after = ""
  | otherwise =
      "--- " <> beforeName <> controlTerm
        <> "+++ " <> afterName <> controlTerm
        <> Text.concat (map renderGroup groups)
  where
    atoms = diffAtoms (splitLinesKeepEnds before) (splitLinesKeepEnds after)
    groups = diffGroups 3 atoms
    renderGroup (start, stop) =
      let prior = take start atoms
          body = take (stop - start) (drop start atoms)
          oldStart = length (filter consumesOld prior)
          newStart = length (filter consumesNew prior)
          oldStop = oldStart + length (filter consumesOld body)
          newStop = newStart + length (filter consumesNew body)
       in "@@ -"
            <> unifiedRange oldStart oldStop
            <> " +"
            <> unifiedRange newStart newStop
            <> " @@"
            <> controlTerm
            <> Text.concat (map renderAtom body)

diffAtoms :: [Text] -> [Text] -> [DiffAtom]
diffAtoms oldLines newLines = renderMatches 0 0 (sequenceMatchingBlocks oldLines newLines)
  where
    oldCount = length oldLines
    newCount = length newLines
    oldArray = listArray (0, oldCount - 1) oldLines
    newArray = listArray (0, newCount - 1) newLines
    renderMatches oldIndex newIndex matches =
      case matches of
        [] -> []
        SequenceMatch nextOld nextNew size : remaining ->
          prefix oldIndex newIndex nextOld nextNew
            <> [DiffSame (oldArray ! index) | index <- [nextOld .. nextOld + size - 1]]
            <> renderMatches (nextOld + size) (nextNew + size) remaining
    prefix oldIndex newIndex nextOld nextNew =
      [DiffRemoved (oldArray ! index) | index <- [oldIndex .. nextOld - 1]]
        <> [DiffAdded (newArray ! index) | index <- [newIndex .. nextNew - 1]]

-- | Python @difflib.SequenceMatcher(None, old, new)@ matching blocks. There is
-- no junk predicate, but the default popularity filter is retained for inputs
-- of at least 200 lines. The strict @>@ best-match update is important: it
-- chooses the earliest old position and then earliest new position on ties.
sequenceMatchingBlocks :: [Text] -> [Text] -> [SequenceMatch]
sequenceMatchingBlocks oldLines newLines = collapseAdjacent (sortOn matchKey discovered) <> [SequenceMatch oldCount newCount 0]
  where
    oldCount = length oldLines
    newCount = length newLines
    oldArray = listArray (0, oldCount - 1) oldLines
    newArray = listArray (0, newCount - 1) newLines
    newPositions =
      Map.filterWithKey
        (\_ positions -> newCount < 200 || length positions <= newCount `div` 100 + 1)
        (Map.map reverse (Map.fromListWith (<>) [(line, [index]) | (index, line) <- zip [0 ..] newLines]))
    discovered = discover [(0, oldCount, 0, newCount)] []
    discover pending matches =
      case pending of
        [] -> matches
        (oldLow, oldHigh, newLow, newHigh) : remaining ->
          let found@(SequenceMatch oldStart newStart size) = findLongest oldArray newArray newPositions oldLow oldHigh newLow newHigh
              withLeft = if oldLow < oldStart && newLow < newStart then (oldLow, oldStart, newLow, newStart) : remaining else remaining
              withRight = if oldStart + size < oldHigh && newStart + size < newHigh then (oldStart + size, oldHigh, newStart + size, newHigh) : withLeft else withLeft
           in if size == 0 then discover remaining matches else discover withRight (found : matches)
    matchKey match = (matchOldStart match, matchNewStart match)

findLongest :: Array Int Text -> Array Int Text -> Map Text [Int] -> Int -> Int -> Int -> Int -> SequenceMatch
findLongest oldArray newArray newPositions oldLow oldHigh newLow newHigh = extendForward (extendBackward scanned)
  where
    scanned = scanOld oldLow (SequenceMatch oldLow newLow 0) Map.empty
    scanOld oldIndex best previousLengths
      | oldIndex >= oldHigh = best
      | otherwise =
          let candidates = filter (\newIndex -> newLow <= newIndex && newIndex < newHigh) (Map.findWithDefault [] (oldArray ! oldIndex) newPositions)
              (nextBest, nextLengths) = foldl (scanNew oldIndex previousLengths) (best, Map.empty) candidates
           in scanOld (oldIndex + 1) nextBest nextLengths
    scanNew oldIndex previousLengths (best, lengths) newIndex =
      let size = Map.findWithDefault 0 (newIndex - 1) previousLengths + 1
          candidate = SequenceMatch (oldIndex - size + 1) (newIndex - size + 1) size
          nextBest = if size > matchLength best then candidate else best
       in (nextBest, Map.insert newIndex size lengths)
    extendBackward match
      | matchOldStart match > oldLow
          && matchNewStart match > newLow
          && oldArray ! (matchOldStart match - 1) == newArray ! (matchNewStart match - 1) =
          extendBackward (SequenceMatch (matchOldStart match - 1) (matchNewStart match - 1) (matchLength match + 1))
      | otherwise = match
    extendForward match
      | matchOldStart match + matchLength match < oldHigh
          && matchNewStart match + matchLength match < newHigh
          && oldArray ! (matchOldStart match + matchLength match) == newArray ! (matchNewStart match + matchLength match) =
          extendForward (match {matchLength = matchLength match + 1})
      | otherwise = match

collapseAdjacent :: [SequenceMatch] -> [SequenceMatch]
collapseAdjacent matches =
  case matches of
    [] -> []
    firstMatch : remaining -> reverse (foldl combine [firstMatch] remaining)
  where
    combine accumulated next =
      case accumulated of
        previous : rest
          | matchOldStart previous + matchLength previous == matchOldStart next
              && matchNewStart previous + matchLength previous == matchNewStart next ->
              previous {matchLength = matchLength previous + matchLength next} : rest
        _ -> next : accumulated

diffGroups :: Int -> [DiffAtom] -> [(Int, Int)]
diffGroups context atoms =
  case changedIndices of
    [] -> []
    firstChanged : remaining -> finalize (go firstChanged firstChanged [] remaining)
  where
    changedIndices = [index | (index, atom) <- zip [0 ..] atoms, not (isSame atom)]
    go firstChanged previousChanged completed rest =
      case rest of
        [] -> (completed, firstChanged, previousChanged)
        current : others
          | sameCountBetween previousChanged current > context * 2 ->
              go current current (completed <> [(firstChanged, previousChanged)]) others
          | otherwise -> go firstChanged current completed others
    finalize (completed, firstChanged, lastChanged) =
      [ (max 0 (firstIndex - context), min (length atoms) (lastIndex + context + 1))
        | (firstIndex, lastIndex) <- completed <> [(firstChanged, lastChanged)]
      ]
    sameCountBetween firstIndex secondIndex = length [() | atom <- take (secondIndex - firstIndex - 1) (drop (firstIndex + 1) atoms), isSame atom]

splitLinesKeepEnds :: Text -> [Text]
splitLinesKeepEnds input
  | Text.null input = []
  | otherwise =
      let (line, suffix) = Text.breakOn "\n" input
       in if Text.null suffix
            then [line]
            else (line <> "\n") : splitLinesKeepEnds (Text.drop 1 suffix)

unifiedRange :: Int -> Int -> Text
unifiedRange start stop
  | count == 1 = Text.pack (show (start + 1))
  | count == 0 = Text.pack (show start) <> ",0"
  | otherwise = Text.pack (show (start + 1)) <> "," <> Text.pack (show count)
  where
    count = stop - start

consumesOld :: DiffAtom -> Bool
consumesOld atom = case atom of DiffAdded _ -> False; _ -> True

consumesNew :: DiffAtom -> Bool
consumesNew atom = case atom of DiffRemoved _ -> False; _ -> True

isSame :: DiffAtom -> Bool
isSame atom = case atom of DiffSame _ -> True; _ -> False

renderAtom :: DiffAtom -> Text
renderAtom atom = case atom of DiffSame line -> " " <> line; DiffRemoved line -> "-" <> line; DiffAdded line -> "+" <> line

relationText :: ConnectionPayload -> Text
relationText payload = case payload of AmendsConnection _ -> "amends"; AppliesToConnection _ -> "applies_to"; DomainsConnection _ -> "domains"; StatusConnection _ -> "status"

data ReferenceMatch = ReferenceMatch
  { referenceMatchObject :: ObjectRef,
    referenceMatchAdr :: AdrId
  }
  deriving (Eq, Ord, Show)

data ReferenceLookupError
  = LookupMalformed Text IdPrefixViolation
  | LookupWrongKind Char
  | LookupNotFound Text
  | LookupAmbiguous Text [ReferenceMatch]
  deriving (Eq, Show)

resolveAdrReference :: ReadSnapshot -> Text -> Either ReferenceLookupError AdrId
resolveAdrReference snapshot input =
  case mkIdPrefix normalized of
    Left (IdPrefixUnsupportedKind kind) -> Left (LookupWrongKind kind)
    Left violation -> Left (LookupMalformed normalized violation)
    Right prefix ->
      case matches prefix of
        [] -> Left (LookupNotFound normalized)
        found
          | [adr] <- Set.toAscList (Set.fromList (map referenceMatchAdr found)),
            length (Set.fromList (map referenceMatchObject found)) == 1 -> Right adr
          | otherwise -> Left (LookupAmbiguous normalized found)
  where
    normalized = asciiUpper (Text.strip input)
    matches prefix =
      Set.toAscList
        . Set.fromList
        $ [ ReferenceMatch objectRef adr
            | (objectRef, adr) <- referenceOwnership snapshot,
              objectRefKind objectRef == idPrefixKind prefix,
              idPrefixText prefix `Text.isPrefixOf` objectRefText objectRef
          ]

referenceOwnership :: ReadSnapshot -> [(ObjectRef, AdrId)]
referenceOwnership snapshot =
  sort
    ( [(adrObjectRef adr, adr) | adr <- map reducedAdrId (graphReductionAdrs (readSnapshotReduction snapshot))]
        <> concatMap documentReferences (readSnapshotDocuments snapshot)
    )

documentReferences :: ParsedManagedDocument -> [(ObjectRef, AdrId)]
documentReferences document =
  case parsedManagedRecord document of
    ManagedDecision decision -> [(recordObjectRef (decisionRecord decision), decisionAdr decision)]
    ManagedConnection connection -> [(connectionObjectRef (connectionRecordId connection), connectionAdr connection)]

referenceLookupErrorText :: ReferenceLookupError -> Text
referenceLookupErrorText problem =
  case problem of
    LookupMalformed value violation -> "malformed ADRAI reference " <> value <> ": " <> Text.pack (show violation)
    LookupWrongKind kind -> "unsupported ADRAI reference kind: " <> Text.singleton kind
    LookupNotFound value -> "ADRAI reference not found in this revision: " <> value
    LookupAmbiguous value matches ->
      "ambiguous ADRAI reference " <> value <> ": " <> Text.intercalate ", " (map renderMatch preview) <> suffix
      where
        preview = take 5 matches
        renderMatch match = objectRefText (referenceMatchObject match) <> "->" <> adrIdText (referenceMatchAdr match)
        suffix = if length matches <= 5 then "" else " \x2026 (" <> Text.pack (show (length matches)) <> " matches)"

adrVisible :: Bool -> ReducedAdr -> Bool
adrVisible includeObsolete reduced =
  includeObsolete || case axisResolutionEffective (reducedStatusAxis reduced) of Just status -> reducedStatusState status /= StatusObsolete; Nothing -> True

data SearchFilterInfo = SearchFilterInfo
  { filterScopeMatch :: Maybe ScopeFilterMatch,
    filterDomains :: [Text],
    filterActor :: Maybe Text
  }

data SearchVectorDiagnostics = SearchVectorDiagnostics
  { vectorDiagnosticAlgorithm :: Text,
    vectorDiagnosticSummaryCorpus :: Int,
    vectorDiagnosticSectionCandidates :: Int,
    vectorDiagnosticSectionsScanned :: Int,
    vectorDiagnosticIdentifierCorpus :: Maybe Int
  }

runCurrentSearch :: Connection -> ReadSnapshot -> SearchMaterialization -> SearchRequest -> IO (Either SearchError SearchProjection)
runCurrentSearch connection snapshot materialization request =
  case prepareSearch snapshot materialization request of
    Left problem -> pure (Left problem)
    Right (requestedDomains, reducedByAdr, documentsById, filterInfo, allowedItems)
      | Text.null (Text.strip (searchRequestQuery request)) ->
          pure
            ( Right
                SearchProjection
                  { searchProjectionRevision = readSnapshotRevision snapshot,
                    searchProjectionMode = searchRequestMode request,
                    searchProjectionLimit = searchRequestLimit request,
                    searchProjectionResults =
                      take (searchRequestLimit request)
                        [ blankSearchResult snapshot request reduced documentsById info
                          | (adr, info) <- Map.toAscList filterInfo,
                            Just reduced <- [Map.lookup adr reducedByAdr]
                        ]
                  }
            )
      | otherwise -> do
          let plan = buildQueryPlan (searchRequestQuery request) (sortOn fst (searchMaterializationAliases materialization))
          ftsOutcome <- runRequestedFts connection request plan allowedItems
          pure $ do
            (ftsChannels, ftsPrefixUsed) <- ftsOutcome
            let allowedDocuments = Map.restrictKeys documentsById allowedItems
            (semanticScores, identifierScores, sectionDetails, vectorDiagnostics) <-
              buildRequestedVectorScores request plan allowedDocuments (searchMaterializationPassages materialization) ftsChannels
            let channels = finalChannels (searchRequestMode request) ftsChannels semanticScores identifierScores
                fusedBase = weightedReciprocalRankFusion (queryPlanWeights plan) channels
                baseOrder = sortCandidateIds fusedBase semanticScores ftsChannels
                rerankTarget = fieldRerankLimit (length baseOrder) (searchRequestLimit request)
                initiallyReranked = Set.fromList (take rerankTarget baseOrder)
                rerankedAdrs =
                  Set.fromList
                    [ searchDocumentAdrId document
                      | itemId <- Set.toList initiallyReranked,
                        Just document <- [Map.lookup itemId allowedDocuments]
                    ]
                rerankIds =
                  Set.fromList
                    [ itemId
                      | (itemId, document) <- Map.toList allowedDocuments,
                        Set.member itemId (Map.keysSet fusedBase),
                        Set.member (searchDocumentAdrId document) rerankedAdrs
                    ]
                evidenceByItem =
                  Map.fromList
                    [ (itemId, lexicalEvidence plan (searchDocumentLexicalFields document))
                      | itemId <- Set.toAscList rerankIds,
                        Just document <- [Map.lookup itemId allowedDocuments]
                    ]
                fused = Map.mapWithKey (applyRerank evidenceByItem) fusedBase
                rankedItems = sortCandidateIds fused semanticScores ftsChannels
                bestByAdr = chooseBestByAdr rankedItems allowedDocuments
                rankedAdrs =
                  take
                    (searchRequestLimit request)
                    (sortLogicalAdrs bestByAdr fused semanticScores)
                ftsDiagnostics = ftsDiagnosticsJson plan ftsPrefixUsed ftsChannels
                retrieval =
                  retrievalDiagnosticsJson
                    (searchRequestMode request)
                    plan
                    ftsDiagnostics
                    vectorDiagnostics
                    (Set.size rerankIds)
                results =
                  [ buildSearchResult
                      snapshot
                      request
                      requestedDomains
                      reduced
                      info
                      document
                      evidenceByItem
                      fused
                      ftsChannels
                      semanticScores
                      identifierScores
                      sectionDetails
                      retrieval
                    | adr <- rankedAdrs,
                      Just itemId <- [Map.lookup adr bestByAdr],
                      Just document <- [Map.lookup itemId allowedDocuments],
                      Just reduced <- [Map.lookup adr reducedByAdr],
                      Just info <- [Map.lookup adr filterInfo]
                  ]
            Right
              SearchProjection
                { searchProjectionRevision = readSnapshotRevision snapshot,
                  searchProjectionMode = searchRequestMode request,
                  searchProjectionLimit = searchRequestLimit request,
                  searchProjectionResults = results
                }

prepareSearch :: ReadSnapshot -> SearchMaterialization -> SearchRequest -> Either SearchError ([Domain], Map AdrId ReducedAdr, Map Text SearchDocument, Map AdrId SearchFilterInfo, Set Text)
prepareSearch snapshot materialization request = do
  mapLeft SearchSnapshotInvalid (validateReadSnapshot snapshot)
  validateSearchRequest request
  requestedDomains <- mapLeft SearchInvalidDomains (canonicalDomains (searchRequestDomains request))
  mapM_ (validateDocument reducedByAdr) (searchMaterializationDocuments materialization)
  let filterInfo =
        Map.fromList
          [ (reducedAdrId reduced, info)
            | reduced <- Map.elems reducedByAdr,
              Just info <- [searchFilterInfo snapshot request requestedDomains reduced]
          ]
      allowedAdrs = Map.keysSet filterInfo
      allowedItems =
        Set.fromList
          [ itemId
            | (itemId, document) <- Map.toList documentsById,
              Set.member (searchDocumentAdrId document) allowedAdrs
          ]
  Right (requestedDomains, reducedByAdr, documentsById, filterInfo, allowedItems)
  where
    reducedByAdr = Map.fromList [(reducedAdrId reduced, reduced) | reduced <- graphReductionAdrs (readSnapshotReduction snapshot)]
    documentsById = Map.fromList [(searchDocumentItemId document, document) | document <- searchMaterializationDocuments materialization]
    validateDocument reductions document =
      case Map.lookup (searchDocumentAdrId document) reductions of
        Nothing -> Left (SearchMaterializationMismatch (searchDocumentItemId document <> " refers to an ADR outside the snapshot"))
        Just reduced
          | searchDocumentStateToken document /= reducedStateToken reduced ->
              Left (SearchMaterializationMismatch (searchDocumentItemId document <> " has a stale state token"))
          | otherwise -> Right ()

validateSearchRequest :: SearchRequest -> Either SearchError ()
validateSearchRequest request
  | searchRequestView request /= CollapsedView = Left (SearchUnsupportedView (searchRequestView request))
  | searchRequestLimit request < 1 || searchRequestLimit request > 1000 = Left (SearchInvalidLimit (searchRequestLimit request))
  | Just since <- searchRequestSince request,
    Just untilBound <- searchRequestUntil request,
    since > untilBound = Left (SearchInvalidTimeRange since untilBound)
  | otherwise = Right ()

searchFilterInfo :: ReadSnapshot -> SearchRequest -> [Domain] -> ReducedAdr -> Maybe SearchFilterInfo
searchFilterInfo snapshot request requestedDomains reduced
  | not (adrVisible (searchRequestIncludeObsolete request) reduced) = Nothing
  | not domainsMatch = Nothing
  | searchRequestFile request /= Nothing && scopeMatch == Just ScopeNone = Nothing
  | not actorMatches = Nothing
  | not sinceMatches = Nothing
  | not untilMatches = Nothing
  | otherwise =
      Just
        SearchFilterInfo
          { filterScopeMatch = scopeMatch,
            filterDomains = map domainText requestedDomains,
            filterActor = actorSelectorText <$> searchRequestActor request
          }
  where
    logicalDomains =
      Set.toAscList . Set.fromList $
        concatMap (`domainEffective` reduced) (axisResolutionHeads (reducedDomainAxis reduced))
    domainsMatch = domainsMatchRequested requestedDomains logicalDomains
    scopeMatch = logicalScopeMatch (searchRequestFile request) reduced
    operationCapsule = do
      decision <- axisResolutionEffective (reducedDecisionAxis reduced)
      document <- find (matchesDecision (decisionRecord decision)) (readSnapshotDocuments snapshot)
      pure (parsedManagedCapsule document)
    actorMatches = case searchRequestActor request of
      Nothing -> True
      Just selector -> maybe False (actorMatchesSelector selector . provenanceActor) operationCapsule
    claimed = maybe 0 provenanceTimestampMs operationCapsule
    sinceMatches = maybe True (<= claimed) (searchRequestSince request)
    untilMatches = maybe True (>= claimed) (searchRequestUntil request)
    matchesDecision record document = case parsedManagedRecord document of
      ManagedDecision decision -> decisionRecord decision == record
      _ -> False

-- | Domain eligibility is a canonical AND across requested ancestors.  Keeping
-- this as a pure helper makes the filter-before-retrieval contract explicit and
-- independently testable for input-order invariance.
domainsMatchRequested :: [Domain] -> [Domain] -> Bool
domainsMatchRequested requestedDomains logicalDomains =
  all
    (\requested -> any (`domainIsWithin` requested) logicalDomains)
    requestedDomains

logicalScopeMatch :: Maybe RepoPath -> ReducedAdr -> Maybe ScopeFilterMatch
logicalScopeMatch Nothing _ = Nothing
logicalScopeMatch (Just path) reduced
  | length heads == 1 = Just (if matched then ScopeExact else ScopeNone)
  | matched = Just ScopeAmbiguous
  | otherwise = Just ScopeNone
  where
    heads = axisResolutionHeads (reducedScopeAxis reduced)
    matched = any (`scopeMatches` path) (concatMap (`scopeEffective` reduced) heads)

actorMatchesSelector :: ActorSelector -> Actor -> Bool
actorMatchesSelector selector actor = actorKind actor == actorSelectorKind selector && actorId actor == actorSelectorId selector

actorSelectorText :: ActorSelector -> Text
actorSelectorText selector = actorKindProjectionText (actorSelectorKind selector) <> ":" <> actorSelectorId selector

runRequestedFts :: Connection -> SearchRequest -> QueryPlan -> Set Text -> IO (Either SearchError (Map RetrievalChannel (Map Text Double), Bool))
runRequestedFts connection request plan allowed
  | searchRequestMode request == VectorRetrieval = pure (Right (Map.empty, False))
  | otherwise =
      case mkCandidateLimit (toInteger (searchRequestLimit request)) of
        Left problem -> pure (Left (SearchSqlFailure problem))
        Right candidateLimit -> do
          result <- runSummaryFtsChannels connection plan allowed candidateLimit
          pure (mapLeft SearchSqlFailure ((\candidates -> (summaryFtsChannelMaps candidates, summaryFtsPrefixUsed candidates)) <$> result))

summaryFtsChannelMaps :: SummaryFtsCandidates -> Map RetrievalChannel (Map Text Double)
summaryFtsChannelMaps candidates =
  Map.fromList
    [ (FtsPhraseChannel, hitMap (summaryFtsPhrase candidates)),
      (FtsTermsChannel, hitMap (summaryFtsTerms candidates)),
      (FtsStemmedChannel, hitMap (summaryFtsStemmed candidates)),
      (FtsIdentifierChannel, hitMap (summaryFtsIdentifier candidates))
    ]
  where
    hitMap = Map.fromList . map (\hit -> (ftsHitItemId hit, ftsHitScore hit))

buildRequestedVectorScores :: SearchRequest -> QueryPlan -> Map Text SearchDocument -> [SearchPassage] -> Map RetrievalChannel (Map Text Double) -> Either SearchError (Map Text Double, Map Text Double, Map Text SemanticEvidence, SearchVectorDiagnostics)
buildRequestedVectorScores request plan documents passages ftsChannels
  | searchRequestMode request == FtsRetrieval =
      Right
        ( Map.empty,
          Map.empty,
          Map.empty,
          SearchVectorDiagnostics "not-requested" (Map.size documents) 0 0 Nothing
        )
  | otherwise = do
      let semanticQuery = semanticEmbedding (queryPlanSemanticText plan)
          identifierQuery = identifierEmbedding (Text.unwords (searchRequestQuery request : queryPlanAliases plan))
      summaryScores <- exactDocumentScores semanticEmbedding semanticQuery semanticSummaryText documents
      -- P3-03 deliberately persists the normalized identifier stream.  Reuse
      -- that frozen field rather than reconstructing or recursively expanding
      -- the Python prototype's pre-normalized identifier source.
      identifierScores <- exactDocumentScores identifierEmbedding identifierQuery searchDocumentIdentifiers documents
      let preliminaryChannels =
            Map.insert SemanticVectorChannel summaryScores
              (Map.insert IdentifierVectorChannel identifierScores ftsChannels)
          preliminary = weightedReciprocalRankFusion (queryPlanWeights plan) preliminaryChannels
          forced = Set.unions [Set.fromList (take (forcedChannelLimit (searchRequestLimit request)) (rankRawChannel scores)) | scores <- Map.elems preliminaryChannels]
          ordered = sortPreliminaryIds preliminary summaryScores identifierScores
          shortlistTarget = sectionShortlistLimit (Map.size documents) (searchRequestLimit request)
          shortlist = fillCandidateSet shortlistTarget forced ordered
          candidatePassages = [passage | passage <- passages, Set.member (searchPassageDocumentItemId passage) shortlist]
          passagesByItem = Map.fromListWith (<>) [(searchPassageDocumentItemId passage, [passage]) | passage <- candidatePassages]
      details <-
        traverse
          (scoreCandidate semanticQuery summaryScores passagesByItem)
          (Map.fromSet id shortlist)
      let semanticScores = Map.map semanticEvidenceScore details
      Right
        ( semanticScores,
          identifierScores,
          details,
          SearchVectorDiagnostics
            "exact-summary+bounded-exact-sections"
            (Map.size summaryScores)
            (Set.size shortlist)
            (length candidatePassages)
            (Just (Map.size identifierScores))
        )
  where
    scoreCandidate semanticQuery summaryScores passagesByItem itemId = do
      sectionScores <-
        traverse
          (scorePassage semanticQuery)
          (Map.findWithDefault [] itemId passagesByItem)
      let summaryScore = Map.findWithDefault (negate (1 / 0)) itemId summaryScores
      Right (detailedSemanticScore summaryScore sectionScores)

exactDocumentScores :: (Text -> DenseVector) -> DenseVector -> (SearchDocument -> Text) -> Map Text SearchDocument -> Either SearchError (Map Text Double)
exactDocumentScores embedDocument queryVector documentText =
  traverse
    (mapLeft SearchVectorFailure . dot queryVector . embedDocument . documentText)

semanticSummaryText :: SearchDocument -> Text
semanticSummaryText document =
  Text.intercalate
    "\n"
    ( filter
        (not . Text.null . Text.strip)
        [ searchDocumentTitle document,
          searchDocumentSummary document,
          searchDocumentDecision document,
          Text.unwords (searchDocumentDomains document),
          searchDocumentRationale document
        ]
    )

scorePassage :: DenseVector -> SearchPassage -> Either SearchError SemanticSectionScore
scorePassage queryVector passage = do
  raw <- mapLeft SearchVectorFailure (dot queryVector (semanticEmbedding (searchPassageText passage)))
  Right
    SemanticSectionScore
      { semanticSectionKind = searchPassageSectionKind passage,
        semanticSectionId = searchPassageId passage,
        semanticSectionText = searchPassageText passage,
        semanticSectionScore = raw * searchPassageWeight passage
      }

rankRawChannel :: Map Text Double -> [Text]
rankRawChannel = map fst . sortBy (comparing (Down . snd) <> comparing (Down . fst)) . Map.toList

sortPreliminaryIds :: Map Text RrfEvidence -> Map Text Double -> Map Text Double -> [Text]
sortPreliminaryIds fused semanticScores identifierScores =
  sortBy
    ( comparing (Down . score)
        <> comparing (Down . semantic)
        <> comparing (Down . identifier)
        <> comparing Down
    )
    (Map.keys fused)
  where
    score item = maybe (negate (1 / 0)) rrfScore (Map.lookup item fused)
    semantic item = Map.findWithDefault (negate (1 / 0)) item semanticScores
    identifier item = Map.findWithDefault (negate (1 / 0)) item identifierScores

fillCandidateSet :: Int -> Set Text -> [Text] -> Set Text
fillCandidateSet target = foldl add
  where
    add selected item
      | Set.size selected >= target = selected
      | otherwise = Set.insert item selected

finalChannels :: RetrievalMode -> Map RetrievalChannel (Map Text Double) -> Map Text Double -> Map Text Double -> Map RetrievalChannel (Map Text Double)
finalChannels mode fts semanticScores identifierScores =
  Map.fromList
    [ (channel, scores)
      | channel <- retrievalModeChannels mode,
        let scores = case channel of
              SemanticVectorChannel -> semanticScores
              IdentifierVectorChannel -> identifierScores
              _ -> Map.findWithDefault Map.empty channel fts,
        not (Map.null scores)
    ]

applyRerank :: Map Text LexicalEvidence -> Text -> RrfEvidence -> RrfEvidence
applyRerank evidenceByItem itemId evidence =
  case Map.lookup itemId evidenceByItem of
    Nothing -> evidence
    Just lexical ->
      evidence
        { rrfScore =
            rrfScore evidence
              + lexicalBonus lexical
              + agreementBonus (Map.size (rrfChannelRanks evidence))
        }

sortCandidateIds :: Map Text RrfEvidence -> Map Text Double -> Map RetrievalChannel (Map Text Double) -> [Text]
sortCandidateIds fused semanticScores ftsChannels =
  sortBy
    ( comparing (Down . fusedScore)
        <> comparing (Down . semanticScore)
        <> comparing (Down . ftsScore)
        <> comparing Down
    )
    (Map.keys fused)
  where
    fusedScore item = maybe (negate (1 / 0)) rrfScore (Map.lookup item fused)
    semanticScore item = Map.findWithDefault (negate (1 / 0)) item semanticScores
    ftsScore item = maximum ((negate (1 / 0)) : [Map.findWithDefault (negate (1 / 0)) item scores | scores <- Map.elems ftsChannels])

chooseBestByAdr :: [Text] -> Map Text SearchDocument -> Map AdrId Text
chooseBestByAdr ordered documents = foldl add Map.empty ordered
  where
    add selected itemId = case Map.lookup itemId documents of
      Nothing -> selected
      Just document -> Map.insertWith (\_ existing -> existing) (searchDocumentAdrId document) itemId selected

sortLogicalAdrs :: Map AdrId Text -> Map Text RrfEvidence -> Map Text Double -> [AdrId]
sortLogicalAdrs bestByAdr fused semanticScores =
  sortBy
    ( comparing (Down . fusedScore)
        <> comparing (Down . semanticScore)
        <> comparing Down
    )
    (Map.keys bestByAdr)
  where
    item adr = Map.findWithDefault "" adr bestByAdr
    fusedScore adr = maybe (negate (1 / 0)) rrfScore (Map.lookup (item adr) fused)
    semanticScore adr = Map.findWithDefault (negate (1 / 0)) (item adr) semanticScores

searchDocumentLexicalFields :: SearchDocument -> [(Text, Text)]
searchDocumentLexicalFields document =
  [ ("title", searchDocumentTitle document),
    ("summary", searchDocumentSummary document),
    ("decision", searchDocumentDecision document),
    ("domains", Text.unwords (searchDocumentDomains document)),
    ("rationale", searchDocumentRationale document),
    ("context", searchDocumentContext document),
    ("consequences", searchDocumentConsequences document),
    ("identifiers", searchDocumentIdentifiers document)
  ]

blankSearchResult :: ReadSnapshot -> SearchRequest -> ReducedAdr -> Map Text SearchDocument -> SearchFilterInfo -> SearchResult
blankSearchResult snapshot request reduced documents info =
  SearchResult
    { searchResultAdr = adr,
      searchResultRecord = decisionRecord <$> logicalDecision,
      searchResultMatchedCandidate = Nothing,
      searchResultMatchedRecord = Nothing,
      searchResultMatchedTitle = Nothing,
      searchResultMatchedSummary = Nothing,
      searchResultTitle = logicalDecisionTitle reduced,
      searchResultSummary = logicalDecisionSummary reduced,
      searchResultDomains = map domainText (axisResolutionEffective (reducedDomainAxis reduced)),
      searchResultAppliesTo = logicalScope reduced,
      searchResultStatus = logicalStatusText reduced,
      searchResultObsolete = logicalObsolete reduced,
      searchResultReplacement = logicalReplacement reduced,
      searchResultConflict = logicalConflict snapshot reduced,
      searchResultResolution = resolutionFor snapshot reduced,
      searchResultStateToken = reducedStateToken reduced,
      searchResultScore = 1,
      searchResultFtsScore = 0,
      searchResultVectorScore = 0,
      searchResultIdentifierVectorScore = 0,
      searchResultSourcePaths = sourcePaths,
      searchResultMatches = emptySearchMatches info,
      searchResultRetrieval = object [("algorithm", JsonString "filtered-list"), ("profile", JsonNull)],
      searchResultCounts = countsFor reduced,
      searchResultScopeAmbiguous = filterScopeMatch info == Just ScopeAmbiguous,
      searchResultDomainAmbiguous = length (axisResolutionHeads (reducedDomainAxis reduced)) > 1,
      searchResultConflicted = not (resolutionStateResolved (resolutionFor snapshot reduced)),
      searchResultShallowHistory = searchRequestShallowHistory request
    }
  where
    adr = reducedAdrId reduced
    logicalDecision = axisResolutionEffective (reducedDecisionAxis reduced)
    sourcePaths =
      Set.toAscList . Set.fromList $
        concat
          [ searchDocumentSourcePaths document
            | document <- Map.elems documents,
              searchDocumentAdrId document == adr
          ]

buildSearchResult :: ReadSnapshot -> SearchRequest -> [Domain] -> ReducedAdr -> SearchFilterInfo -> SearchDocument -> Map Text LexicalEvidence -> Map Text RrfEvidence -> Map RetrievalChannel (Map Text Double) -> Map Text Double -> Map Text Double -> Map Text SemanticEvidence -> JsonValue -> SearchResult
buildSearchResult snapshot request _requestedDomains reduced info document evidenceByItem fused ftsChannels semanticScores identifierScores sectionDetails retrieval =
  SearchResult
    { searchResultAdr = adr,
      searchResultRecord = decisionRecord <$> logicalDecision,
      searchResultMatchedCandidate = if itemId == adrIdText adr then Nothing else Just itemId,
      searchResultMatchedRecord = Just (searchDocumentCandidateRecordId document),
      searchResultMatchedTitle = if itemId == adrIdText adr then Nothing else Just (searchDocumentTitle document),
      searchResultMatchedSummary = if itemId == adrIdText adr then Nothing else Just (searchDocumentSummary document),
      searchResultTitle = logicalDecisionTitle reduced,
      searchResultSummary = logicalDecisionSummary reduced,
      searchResultDomains = map domainText (axisResolutionEffective (reducedDomainAxis reduced)),
      searchResultAppliesTo = logicalScope reduced,
      searchResultStatus = logicalStatusText reduced,
      searchResultObsolete = logicalObsolete reduced,
      searchResultReplacement = logicalReplacement reduced,
      searchResultConflict = logicalConflict snapshot reduced,
      searchResultResolution = resolution,
      searchResultStateToken = reducedStateToken reduced,
      searchResultScore = maybe 0 rrfScore rrf,
      searchResultFtsScore = maximum (0 : [Map.findWithDefault 0 itemId scores | scores <- Map.elems ftsChannels]),
      searchResultVectorScore = Map.findWithDefault 0 itemId semanticScores,
      searchResultIdentifierVectorScore = Map.findWithDefault 0 itemId identifierScores,
      searchResultSourcePaths = searchDocumentSourcePaths document,
      searchResultMatches =
        SearchMatches
          { searchMatchFts = any (Map.member itemId) (Map.elems ftsChannels),
            searchMatchVector = Map.member itemId semanticScores || Map.member itemId identifierScores,
            searchMatchFileScope = filterScopeMatch info,
            searchMatchDomains = filterDomains info,
            searchMatchActor = filterActor info,
            searchMatchFields = maybe [] lexicalMatchedFields lexical,
            searchMatchTerms = maybe [] lexicalMatchedTerms lexical,
            searchMatchExactPhraseFields = maybe [] lexicalExactPhraseFields lexical,
            searchMatchIdentifierTerms = maybe [] lexicalIdentifierTerms lexical,
            searchMatchChannelRanks = maybe Map.empty rrfChannelRanks rrf,
            searchMatchBestSection = semanticSectionKind <$> (semanticBestSection =<< details),
            searchMatchBestSectionScore = semanticSectionScore <$> (semanticBestSection =<< details)
          },
      searchResultRetrieval = retrieval,
      searchResultCounts = countsFor reduced,
      searchResultScopeAmbiguous = filterScopeMatch info == Just ScopeAmbiguous,
      searchResultDomainAmbiguous = length (axisResolutionHeads (reducedDomainAxis reduced)) > 1,
      searchResultConflicted = not (resolutionStateResolved resolution),
      searchResultShallowHistory = searchRequestShallowHistory request
    }
  where
    adr = reducedAdrId reduced
    itemId = searchDocumentItemId document
    logicalDecision = axisResolutionEffective (reducedDecisionAxis reduced)
    resolution = resolutionFor snapshot reduced
    lexical = Map.lookup itemId evidenceByItem
    rrf = Map.lookup itemId fused
    details = Map.lookup itemId sectionDetails

emptySearchMatches :: SearchFilterInfo -> SearchMatches
emptySearchMatches info =
  SearchMatches False False (filterScopeMatch info) (filterDomains info) (filterActor info) [] [] [] [] Map.empty Nothing Nothing

logicalScope :: ReducedAdr -> [Text]
logicalScope reduced
  | axisProjectionResolved (reducedScopeAxis reduced) = map scopePatternText (axisResolutionEffective (reducedScopeAxis reduced))
  | otherwise = []

logicalStatusText :: ReducedAdr -> Text
logicalStatusText reduced
  | axisProjectionResolved axis = statusText (axisResolutionEffective axis)
  | otherwise = "conflict"
  where
    axis = reducedStatusAxis reduced

logicalObsolete :: ReducedAdr -> Bool
logicalObsolete reduced =
  case axisResolutionEffective (reducedStatusAxis reduced) of
    Just status -> axisProjectionResolved (reducedStatusAxis reduced) && reducedStatusState status == StatusObsolete
    Nothing -> False

logicalReplacement :: ReducedAdr -> Maybe AdrId
logicalReplacement reduced
  | axisProjectionResolved axis = reducedStatusReplacement =<< axisResolutionEffective axis
  | otherwise = Nothing
  where
    axis = reducedStatusAxis reduced

logicalConflict :: ReadSnapshot -> ReducedAdr -> Maybe Text
logicalConflict snapshot reduced =
  case map resolutionSummary (resolutionStateConflicts (resolutionFor snapshot reduced)) of
    [] -> Nothing
    summaries -> Just (Text.intercalate "; " summaries)

ftsDiagnosticsJson :: QueryPlan -> Bool -> Map RetrievalChannel (Map Text Double) -> JsonValue
ftsDiagnosticsJson plan prefixUsed channels =
  object
    ( [ ("profile", JsonString (queryProfileName (queryPlanProfile plan))),
        ( "channel_candidates",
          object
            [ (retrievalChannelName channel, JsonNumber (fromIntegral (Map.size (Map.findWithDefault Map.empty channel channels))))
              | channel <- [FtsPhraseChannel, FtsTermsChannel, FtsStemmedChannel, FtsIdentifierChannel]
            ]
        )
      ]
        <> [ ( "queries",
               object
                 [ ("phrase", JsonString (queryPlanFtsExactPhrase plan)),
                   ("terms", JsonString (Text.intercalate " AND " (queryPlanFtsExactTerms plan))),
                   ("near", JsonString (queryPlanFtsNear plan)),
                   ("prefix", JsonString (if prefixUsed then queryPlanFtsPrefix plan else "")),
                   ("stemmed", JsonString (queryPlanFtsStemmed plan)),
                   ("identifier", JsonString (queryPlanFtsIdentifier plan))
                 ]
             )
             | not (Map.null channels)
           ]
    )

retrievalDiagnosticsJson :: RetrievalMode -> QueryPlan -> JsonValue -> SearchVectorDiagnostics -> Int -> JsonValue
retrievalDiagnosticsJson mode plan ftsDiagnostics vectorDiagnostics rerankCount =
  object
    [ ("algorithm", JsonString (retrievalAlgorithm mode)),
      ("profile", JsonString (queryProfileName (queryPlanProfile plan))),
      ("aliases", textArray (queryPlanAliases plan)),
      ("fts", ftsDiagnostics),
      ("vector", vectorDiagnosticsJson vectorDiagnostics),
      ("field_rerank_candidates", JsonNumber (fromIntegral rerankCount))
    ]

retrievalAlgorithm :: RetrievalMode -> Text
retrievalAlgorithm FtsRetrieval = "three-fts+weighted-rrf"
retrievalAlgorithm VectorRetrieval = "exact-summary+identifier+section-rerank+weighted-rrf"
retrievalAlgorithm HybridRetrieval = "three-fts+exact-summary+section-rerank+weighted-rrf"

vectorDiagnosticsJson :: SearchVectorDiagnostics -> JsonValue
vectorDiagnosticsJson diagnostics =
  objectOmittingNulls
    [ ("algorithm", JsonString (vectorDiagnosticAlgorithm diagnostics)),
      ("summary_corpus", JsonNumber (fromIntegral (vectorDiagnosticSummaryCorpus diagnostics))),
      ("section_candidates", JsonNumber (fromIntegral (vectorDiagnosticSectionCandidates diagnostics))),
      ("sections_scanned", JsonNumber (fromIntegral (vectorDiagnosticSectionsScanned diagnostics))),
      ("identifier_corpus", maybeJson (JsonNumber . fromIntegral) (vectorDiagnosticIdentifierCorpus diagnostics))
    ]

data CompareOptions = CompareOptions
  { compareIncludeUnchanged :: Bool,
    compareCacheMetadata :: [(Text, JsonValue)]
  }
  deriving (Eq, Show)

data CompareCounts = CompareCounts
  { compareAdded :: Int,
    compareRemoved :: Int,
    compareChanged :: Int,
    compareUnchanged :: Int
  }
  deriving (Eq, Show)

data CompareChange = CompareChange
  { compareChangeField :: Text,
    compareChangeBefore :: JsonValue,
    compareChangeAfter :: JsonValue,
    compareChangeDiff :: Maybe Text
  }
  deriving (Eq, Show)

data CompareSnapshot = CompareSnapshot
  { compareSnapshotCollapsed :: CollapsedProjection,
    compareSnapshotRecordHeads :: [Text],
    compareSnapshotScopeHeads :: [Text],
    compareSnapshotDomainHeads :: [Text],
    compareSnapshotStatusHeads :: [Text]
  }
  deriving (Eq, Show)

data CompareEntry = CompareEntry
  { compareEntryAdr :: AdrId,
    compareEntryKind :: Text,
    compareEntryTitle :: Text,
    compareEntryBefore :: Maybe CompareSnapshot,
    compareEntryAfter :: Maybe CompareSnapshot,
    compareEntryChanges :: [CompareChange]
  }
  deriving (Eq, Show)

data CompareProjection = CompareProjection
  { compareFromRevision :: RevisionIdentity,
    compareToRevision :: RevisionIdentity,
    compareCounts :: CompareCounts,
    compareEntries :: [CompareEntry],
    compareCache :: [(Text, JsonValue)]
  }
  deriving (Eq, Show)

compareSnapshots :: CompareOptions -> ReadSnapshot -> ReadSnapshot -> Either QueryError CompareProjection
compareSnapshots options beforeSnapshot afterSnapshot = do
  validateQuerySnapshot beforeSnapshot
  validateQuerySnapshot afterSnapshot
  let beforeMap = snapshotMap beforeSnapshot
      afterMap = snapshotMap afterSnapshot
      allAdrs = Set.toAscList (Map.keysSet beforeMap <> Map.keysSet afterMap)
      entries = map (classify beforeMap afterMap) allAdrs
      includeEntry entry = compareIncludeUnchanged options || compareEntryKind entry /= "unchanged"
      count kind = length (filter ((== kind) . compareEntryKind) entries)
  Right
    CompareProjection
      { compareFromRevision = readSnapshotRevision beforeSnapshot,
        compareToRevision = readSnapshotRevision afterSnapshot,
        compareCounts = CompareCounts (count "added") (count "removed") (count "changed") (count "unchanged"),
        compareEntries = filter includeEntry entries,
        compareCache = compareCacheMetadata options
      }
  where
    classify beforeMap afterMap adr =
      case (Map.lookup adr beforeMap, Map.lookup adr afterMap) of
        (Nothing, Just after) -> CompareEntry adr "added" (snapshotTitle after) Nothing (Just after) []
        (Just before, Nothing) -> CompareEntry adr "removed" (snapshotTitle before) (Just before) Nothing []
        (Just before, Just after)
          | snapshotToken before == snapshotToken after -> CompareEntry adr "unchanged" (snapshotTitle after) (Just before) (Just after) []
          | otherwise -> CompareEntry adr "changed" (snapshotTitle after) (Just before) (Just after) (snapshotChanges before after)
        (Nothing, Nothing) -> error "internal error: compare key vanished"

snapshotMap :: ReadSnapshot -> Map AdrId CompareSnapshot
snapshotMap snapshot =
  Map.fromList
    [ (adr, CompareSnapshot collapsed recordHeads scopeHeads domainHeads statusHeads)
      | reduced <- graphReductionAdrs (readSnapshotReduction snapshot),
        let adr = reducedAdrId reduced,
        Right collapsed <- [projectCollapsed CompactProjection snapshot adr],
        let recordHeads = map recordIdText (axisResolutionHeads (reducedDecisionAxis reduced)),
        let scopeHeads = map connectionIdText (axisResolutionHeads (reducedScopeAxis reduced)),
        let domainHeads = map connectionIdText (axisResolutionHeads (reducedDomainAxis reduced)),
        let statusHeads = map connectionIdText (axisResolutionHeads (reducedStatusAxis reduced))
    ]

snapshotToken :: CompareSnapshot -> StateToken
snapshotToken = collapsedStateToken . compareSnapshotCollapsed

snapshotTitle :: CompareSnapshot -> Text
snapshotTitle = collapsedTitle . compareSnapshotCollapsed

snapshotChanges :: CompareSnapshot -> CompareSnapshot -> [CompareChange]
snapshotChanges before after = mapMaybe changedField fields
  where
    fields = ["record", "title", "summary", "domains", "body", "applies_to", "status", "obsolete", "replacement", "conflict", "resolved", "resolution_required", "record_heads", "scope_heads", "domain_heads", "status_heads"]
    changedField field =
      let old = snapshotField field before
          new = snapshotField field after
       in if old == new then Nothing else Just (CompareChange field old new (snapshotTextDiff field before after))

snapshotField :: Text -> CompareSnapshot -> JsonValue
snapshotField field snapshot =
  case field of
    "record" -> maybeJson (JsonString . recordIdText) (collapsedRecord collapsed)
    "title" -> JsonString (collapsedTitle collapsed)
    "summary" -> JsonString (collapsedSummary collapsed)
    "domains" -> textArray (collapsedDomains collapsed)
    "body" -> JsonString (collapsedBody collapsed)
    "applies_to" -> textArray (collapsedAppliesTo collapsed)
    "status" -> JsonString (collapsedStatus collapsed)
    "obsolete" -> JsonBool (collapsedObsolete collapsed)
    "replacement" -> maybeJson (JsonString . adrIdText) (collapsedReplacement collapsed)
    "conflict" -> maybeJson JsonString (collapsedConflictText collapsed)
    "resolved" -> JsonBool (resolutionStateResolved resolution)
    "resolution_required" -> JsonBool (resolutionStateRequired resolution)
    "record_heads" -> textArray (compareSnapshotRecordHeads snapshot)
    "scope_heads" -> textArray (compareSnapshotScopeHeads snapshot)
    "domain_heads" -> textArray (compareSnapshotDomainHeads snapshot)
    "status_heads" -> textArray (compareSnapshotStatusHeads snapshot)
    _ -> JsonNull
  where
    collapsed = compareSnapshotCollapsed snapshot
    resolution = collapsedResolution collapsed

snapshotTextDiff :: Text -> CompareSnapshot -> CompareSnapshot -> Maybe Text
snapshotTextDiff field before after
  | field == "title" = Just (compareUnifiedDiff (collapsedTitle old) (collapsedTitle new))
  | field == "summary" = Just (compareUnifiedDiff (collapsedSummary old) (collapsedSummary new))
  | field == "body" = Just (compareUnifiedDiff (collapsedBody old) (collapsedBody new))
  | otherwise = Nothing
  where
    old = compareSnapshotCollapsed before
    new = compareSnapshotCollapsed after

searchProjectionJson :: SearchProjection -> JsonValue
searchProjectionJson projection =
  object
    [ ("schema", JsonString "adrai/search/v1"),
      ("as_of", JsonString (revisionResolved (searchProjectionRevision projection))),
      ("view", JsonString "collapsed"),
      ("mode", JsonString (retrievalModeName (searchProjectionMode projection))),
      ("limit", JsonNumber (fromIntegral (searchProjectionLimit projection))),
      ("results", JsonArray (map (searchResultJson (revisionResolved (searchProjectionRevision projection))) (searchProjectionResults projection)))
    ]

renderSearchProjection :: SearchProjection -> ByteString
renderSearchProjection = renderCanonicalJsonBytes . searchProjectionJson

searchResultJson :: Text -> SearchResult -> JsonValue
searchResultJson revision result =
  object
    ( [ ("id", JsonString adr),
        ("adr", JsonString adr),
        ("record", maybeJson (JsonString . recordIdText) (searchResultRecord result)),
        ("title", JsonString (searchResultTitle result)),
        ("summary", JsonString (searchResultSummary result)),
        ("domains", textArray (searchResultDomains result)),
        ("applies_to", textArray (searchResultAppliesTo result)),
        ("status", JsonString (searchResultStatus result)),
        ("obsolete", JsonBool (searchResultObsolete result)),
        ("replacement", maybeJson (JsonString . adrIdText) (searchResultReplacement result)),
        ("conflict", maybeJson JsonString (searchResultConflict result)),
        ("conflicts", JsonArray (map resolutionEntryJson (resolutionStateConflicts resolution))),
        ("resolved", JsonBool (resolutionStateResolved resolution)),
        ("resolution_required", JsonBool (resolutionStateRequired resolution)),
        ("state_token", JsonString (stateTokenText (searchResultStateToken result))),
        ("score", scoreJson (searchResultScore result)),
        ("fts_score", scoreJson (searchResultFtsScore result)),
        ("vector_score", scoreJson (searchResultVectorScore result))
      ]
        <> ( if blank
               then []
               else
                 [ ("identifier_vector_score", scoreJson (searchResultIdentifierVectorScore result)),
                   ("matched_candidate", maybeJson JsonString (searchResultMatchedCandidate result)),
                   ("matched_record", maybeJson (JsonString . recordIdText) (searchResultMatchedRecord result)),
                   ("matched_title", maybeJson JsonString (searchResultMatchedTitle result)),
                   ("matched_summary", maybeJson JsonString (searchResultMatchedSummary result))
                 ]
           )
        <> [ ("view", JsonString "collapsed"),
             ("as_of", JsonString revision),
             ("source_paths", textArray (searchResultSourcePaths result)),
             ("matches", searchMatchesJson blank (searchResultMatches result)),
             ("retrieval", searchResultRetrieval result),
             ("context", searchContextJson result),
             ( "flags",
               object
                 [ ("conflicted", JsonBool (searchResultConflicted result)),
                   ("shallow_history", JsonBool (searchResultShallowHistory result))
                 ]
             )
           ]
    )
  where
    adr = adrIdText (searchResultAdr result)
    resolution = searchResultResolution result
    blank = searchResultMatchedRecord result == Nothing

searchMatchesJson :: Bool -> SearchMatches -> JsonValue
searchMatchesJson blank matches =
  object
    ( [ ("fts", JsonBool (searchMatchFts matches)),
        ("vector", JsonBool (searchMatchVector matches)),
        ("file_scope", maybeJson (JsonString . scopeFilterMatchText) (searchMatchFileScope matches)),
        ("domain_filter", textArray (searchMatchDomains matches)),
        ("actor_filter", maybeJson JsonString (searchMatchActor matches))
      ]
        <> ( if blank
               then []
               else
                 [ ("fields", textArray (searchMatchFields matches)),
                   ("terms", textArray (searchMatchTerms matches)),
                   ("exact_phrase_fields", textArray (searchMatchExactPhraseFields matches)),
                   ("identifier_terms", textArray (searchMatchIdentifierTerms matches)),
                   ( "channel_ranks",
                     object
                       [ (retrievalChannelName channel, JsonNumber (fromIntegral rank))
                         | (channel, rank) <- Map.toAscList (searchMatchChannelRanks matches)
                       ]
                   ),
                   ("best_section", maybeJson (JsonString . sectionKindName) (searchMatchBestSection matches)),
                   ("best_section_score", maybe JsonNull scoreJson (searchMatchBestSectionScore matches))
                 ]
           )
    )

scopeFilterMatchText :: ScopeFilterMatch -> Text
scopeFilterMatchText ScopeExact = "exact"
scopeFilterMatchText ScopeAmbiguous = "ambiguous"
scopeFilterMatchText ScopeNone = "none"

searchContextJson :: SearchResult -> JsonValue
searchContextJson result =
  object
    [ ("editions", JsonNumber (fromIntegral (projectionEditions counts))),
      ("scope_revisions", JsonNumber (fromIntegral (projectionScopeRevisions counts))),
      ("domain_revisions", JsonNumber (fromIntegral (projectionDomainRevisions counts))),
      ("status_revisions", JsonNumber (fromIntegral (projectionStatusRevisions counts))),
      ("scope_ambiguous", JsonBool (searchResultScopeAmbiguous result)),
      ("domain_ambiguous", JsonBool (searchResultDomainAmbiguous result))
    ]
  where
    counts = searchResultCounts result

scoreJson :: Double -> JsonValue
scoreJson = fromMaybe JsonNull . jsonNumberRounded6

collapsedProjectionJson :: CollapsedProjection -> JsonValue
collapsedProjectionJson projection = object (baseMembers <> richMembers)
  where
    resolution = collapsedResolution projection
    baseMembers =
      [ ("adr", JsonString (adrIdText (collapsedAdr projection))),
        ("applies_to", textArray (collapsedAppliesTo projection)),
        ("as_of", JsonString (revisionResolved (collapsedRevision projection))),
        ("body", JsonString (collapsedBody projection)),
        ("counts", countsJson (collapsedCounts projection)),
        ("domains", textArray (collapsedDomains projection)),
        ("evolution", evolutionJson (collapsedEvolution projection)),
        ("issues", JsonArray (map publicIssueJson (collapsedIssues projection))),
        ("obsolete", JsonBool (collapsedObsolete projection)),
        ("provenance", collapsedProvenanceJson (collapsedProvenance projection)),
        ("record", maybeJson (JsonString . recordIdText) (collapsedRecord projection)),
        ("replacement", maybeJson (JsonString . adrIdText) (collapsedReplacement projection)),
        ("resolution", resolutionStateJson resolution),
        ("resolution_required", JsonBool (resolutionStateRequired resolution)),
        ("resolved", JsonBool (resolutionStateResolved resolution)),
        ("schema", JsonString "adrai/show-collapsed/v1"),
        ("state_token", JsonString (stateTokenText (collapsedStateToken projection))),
        ("status", JsonString (collapsedStatus projection)),
        ("summary", JsonString (collapsedSummary projection)),
        ("title", JsonString (collapsedTitle projection)),
        ("view", JsonString "collapsed")
      ]
    richMembers = maybe [] richDetailMembers (collapsedRichDetail projection)

richDetailMembers :: CollapsedRichDetail -> [(Text, JsonValue)]
richDetailMembers detail =
  [ ("candidate_domains", JsonArray (richCandidateDomains detail)),
    ("candidate_records", JsonArray (richCandidateRecords detail)),
    ("candidate_scopes", JsonArray (richCandidateScopes detail)),
    ("conflicts", textArray (richRawConflicts detail)),
    ("connection_history", textArray (richConnectionHistory detail)),
    ("decision_history", textArray (richDecisionHistory detail)),
    ("domain_heads", textArray (richDomainHeads detail)),
    ("record_heads", textArray (richRecordHeads detail)),
    ("scope_heads", textArray (richScopeHeads detail)),
    ("source_paths", textArray (richSourcePaths detail)),
    ("status_heads", textArray (richStatusHeads detail))
  ]

explodedProjectionJson :: ExplodedProjection -> JsonValue
explodedProjectionJson projection =
  object
    [ ("adr", JsonString (adrIdText (explodedAdr projection))),
      ("as_of", JsonString (revisionResolved (explodedRevision projection))),
      ("operations", JsonArray (map explodedOperationJson (explodedOperations projection))),
      ("resolution", resolutionStateJson resolution),
      ("resolution_required", JsonBool (resolutionStateRequired resolution)),
      ("resolved", JsonBool (resolutionStateResolved resolution)),
      ("schema", JsonString "adrai/show-exploded/v1"),
      ("state_token", JsonString (stateTokenText (explodedStateToken projection))),
      ("view", JsonString "exploded")
    ]
  where
    resolution = explodedResolution projection

compareProjectionJson :: CompareProjection -> JsonValue
compareProjectionJson projection =
  object
    [ ("cache", object (compareCache projection)),
      ("counts", compareCountsJson (compareCounts projection)),
      ("entries", JsonArray (map compareEntryJson (compareEntries projection))),
      ("from", JsonString (revisionResolved (compareFromRevision projection))),
      ("from_requested", JsonString (revisionRequested (compareFromRevision projection))),
      ("to", JsonString (revisionResolved (compareToRevision projection))),
      ("to_requested", JsonString (revisionRequested (compareToRevision projection))),
      ("view", JsonString "compare")
    ]

renderCollapsedProjection :: CollapsedProjection -> ByteString
renderCollapsedProjection = renderCanonicalJsonBytes . collapsedProjectionJson

renderExplodedProjection :: ExplodedProjection -> ByteString
renderExplodedProjection = renderCanonicalJsonBytes . explodedProjectionJson

renderCompareProjection :: CompareProjection -> ByteString
renderCompareProjection = renderCanonicalJsonBytes . compareProjectionJson

countsJson :: ProjectionCounts -> JsonValue
countsJson counts =
  object
    [ ("amendments", intJson (projectionAmendments counts)),
      ("domain_revisions", intJson (projectionDomainRevisions counts)),
      ("editions", intJson (projectionEditions counts)),
      ("scope_revisions", intJson (projectionScopeRevisions counts)),
      ("status_revisions", intJson (projectionStatusRevisions counts))
    ]

compareCountsJson :: CompareCounts -> JsonValue
compareCountsJson counts = object [("added", intJson (compareAdded counts)), ("changed", intJson (compareChanged counts)), ("removed", intJson (compareRemoved counts)), ("unchanged", intJson (compareUnchanged counts))]

evolutionJson :: EvolutionSummary -> JsonValue
evolutionJson evolution =
  objectOmittingNulls
    [ ("latest", maybeJson historyLatestJson (evolutionLatest evolution)),
      ("operation_count", intJson (evolutionOperationCount evolution)),
      ("summary", JsonString (evolutionSummaryText evolution))
    ]

historyLatestJson :: HistoryOperation -> JsonValue
historyLatestJson operation =
  objectOmittingNulls
    [ ("actor", JsonString (actorProjectionText (historyOperationActor operation))),
      ("claimed_at", JsonString (isoTimestampFromMs (historyOperationClaimedAt operation))),
      ("details", historyDetailsJson (historyDetailsOrEmpty (historyOperationDetails operation))),
      ("label", JsonString (historyOperationLabel operation)),
      ("model", maybeJson JsonString (actorModel (historyOperationActor operation))),
      ("operation", JsonString (operationIdText (historyOperationId operation))),
      ("reason", maybeJson JsonString (historyOperationReason operation))
    ]

historyDetailsJson :: HistoryDetails -> JsonValue
historyDetailsJson details =
  objectOmittingNulls
    [ optionalTextArray "added_domains" (historyAddedDomains details),
      optionalTextArray "added_scope" (historyAddedScope details),
      optionalTextArray "changed_fields" (historyChangedFields details),
      optionalTextArray "domain_refinements" (historyDomainRefinements details),
      optionalTextArray "removed_domains" (historyRemovedDomains details),
      optionalTextArray "removed_scope" (historyRemovedScope details),
      optionalTextArray "replacement_adrs" (historyReplacementAdrs details)
    ]

collapsedProvenanceJson :: CollapsedProvenance -> JsonValue
collapsedProvenanceJson provenance =
  object
    [ ("created", maybeJson provenanceJson (collapsedCreatedProvenance provenance)),
      ("domains", maybeJson provenanceJson (collapsedDomainsProvenance provenance)),
      ("effective", maybeJson provenanceJson (collapsedEffectiveProvenance provenance)),
      ("status", maybeJson provenanceJson (collapsedStatusProvenance provenance))
    ]

provenanceJson :: ProvenanceProjection -> JsonValue
provenanceJson provenance =
  object
    [ ("actor", JsonString (actorProjectionText (projectedActor provenance))),
      ("basis", JsonString (projectedBasis provenance)),
      ("claimed_at", JsonString (isoTimestampFromMs (projectedClaimedAt provenance))),
      ("introductions", textArray (projectedIntroductions provenance)),
      ("line_landings", JsonArray (map lineLandingJson (projectedLineLandings provenance))),
      ("model", maybeJson JsonString (actorModel (projectedActor provenance))),
      ("operation", JsonString (operationIdText (projectedOperation provenance))),
      ("original_commits", textArray (projectedOriginalCommits provenance))
    ]

operationFullProvenanceJson :: ReadSnapshot -> OperationId -> [ParsedManagedDocument] -> JsonValue
operationFullProvenanceJson snapshot operation documents =
  case firstOf (sortOn documentObjectText documents) of
    Nothing -> JsonNull
    Just document ->
      object
        [ ("actor", JsonString (actorProjectionText actor)),
          ("basis", JsonString (gitOidText (provenanceBasis capsule))),
          ("branch_hint", maybeJson JsonString (provenanceBranchHint capsule)),
          ("claimed_at", JsonString (isoTimestampFromMs claimedMs)),
          ("claimed_ms", JsonNumber claimedMs),
          ("context_digest", maybeJson (JsonString . renderDigest) (provenanceContextDigest inputs)),
          ("events", textArray events),
          ("input_digest", maybeJson (JsonString . renderDigest) (provenanceInputDigest inputs)),
          ("line_anchors", JsonArray (map lineAnchorJson (provenanceLineAnchors capsule))),
          ("line_landings", JsonArray (map lineLandingJson landings)),
          ("model", maybeJson JsonString (actorModel actor)),
          ("objects", textArray objects),
          ("operation", JsonString (operationIdText operation)),
          ("placements", JsonArray (map commitPlacementJson placements)),
          ("prompt_digest", maybeJson (JsonString . renderDigest) (provenancePromptDigest inputs)),
          ("tool", JsonString (provenanceToolVersion capsule)),
          ("upstream_hint", maybeJson JsonString (provenanceUpstreamHint capsule)),
          ("when", whenJson claimedMs placements landings evidence)
        ]
      where
        capsule = parsedManagedCapsule document
        actor = provenanceActor capsule
        claimedMs = provenanceTimestampMs capsule
        inputs = provenanceInputs capsule
        events = sortedUnique [eventKindText (provenanceEventKind (parsedManagedCapsule item)) | item <- documents]
        objects = sortedUnique [provenanceObjectIdText (provenanceObjectId (parsedManagedCapsule item)) | item <- documents]
        evidence = Map.lookup operation (readSnapshotPlacement snapshot)
        placements = maybe [] publicPlacements evidence
        landings = maybe [] (sortOn (\landing -> (landingLine landing, landingRef landing)) . placementLineLandings) evidence

lineAnchorJson :: LineAnchor -> JsonValue
lineAnchorJson anchor =
  object
    [ ("commit", JsonString (gitOidText (lineAnchorCommit anchor))),
      ("line", JsonString (lineAnchorId anchor))
    ]

commitPlacementJson :: CommitPlacementEvidence -> JsonValue
commitPlacementJson placement =
  object
    [ ("authored_at", JsonString (isoTimestampFromMs (commitPlacementAuthoredAtMs placement))),
      ("classification", JsonString (commitPlacementClassification placement)),
      ("commit", JsonString (commitPlacementOid placement)),
      ("committed_at", JsonString (isoTimestampFromMs (commitPlacementCommittedAtMs placement))),
      ("parents", textArray (commitPlacementParents placement)),
      ("reachable", JsonBool (commitPlacementReachable placement)),
      ("subject", JsonString (commitPlacementSubject placement))
    ]

whenJson :: Integer -> [CommitPlacementEvidence] -> [LineLandingEvidence] -> Maybe PlacementEvidence -> JsonValue
whenJson claimedMs placements landings evidence =
  object
    [ ("copies", textArray [commitPlacementOid placement | placement <- placements, commitPlacementClassification placement == "copy"]),
      ("first_present_on_lines", JsonArray (map lineLandingJson landings)),
      ("introductions", textArray (maybe [] introductionCommits evidence)),
      ("original_operation_commits", textArray (maybe [] originalOperationCommits evidence)),
      ("semantic", JsonString (isoTimestampFromMs claimedMs))
    ]

lineLandingJson :: LineLandingEvidence -> JsonValue
lineLandingJson landing =
  object
    [ ("commit", JsonString (landingCommit landing)),
      ("complete", JsonBool (landingComplete landing)),
      ("line", JsonString (landingLine landing)),
      ("ref", JsonString (landingRef landing))
    ]

resolutionStateJson :: ResolutionState -> JsonValue
resolutionStateJson resolution =
  object
    [ ("conflicts", JsonArray (map resolutionEntryJson (resolutionStateConflicts resolution))),
      ("resolution_required", JsonBool (resolutionStateRequired resolution)),
      ("resolved", JsonBool (resolutionStateResolved resolution))
    ]

resolutionEntryJson :: ResolutionEntry -> JsonValue
resolutionEntryJson entry =
  object
    [ ("head_count", intJson (resolutionHeadCount entry)),
      ("heads", textArray (resolutionHeads entry)),
      ("kind", JsonString (conflictKindText (resolutionConflictKind entry))),
      ("summary", JsonString (resolutionSummary entry))
    ]

publicIssueJson :: PublicIssue -> JsonValue
publicIssueJson issue =
  object
    [ ("code", JsonString (publicIssueCode issue)),
      ("message", JsonString (publicIssueMessage issue)),
      ("object_id", maybeJson JsonString (publicIssueObjectId issue)),
      ("path", maybeJson JsonString (publicIssuePath issue)),
      ("severity", JsonString (publicIssueSeverity issue))
    ]

explodedOperationJson :: ExplodedOperation -> JsonValue
explodedOperationJson operation =
  object
    [ ("items", JsonArray (map explodedItemJson (explodedOperationItems operation))),
      ("operation", JsonString (operationIdText (explodedOperationId operation))),
      ("provenance", explodedOperationFullProvenance operation)
    ]

explodedItemJson :: ExplodedItem -> JsonValue
explodedItemJson item =
  objectOmittingNulls
    [ ("body", maybeJson JsonString (explodedItemBody item)),
      ("diffs", if explodedItemType item == "decision" then JsonArray (map parentDiffJson (explodedItemDiffs item)) else JsonNull),
      ("domains", if explodedItemType item == "decision" then textArray (explodedItemDomains item) else JsonNull),
      ("event", JsonString (explodedItemEvent item)),
      ("item", JsonString (explodedItemId item)),
      ("metadata", maybe JsonNull id (explodedItemMetadata item)),
      ("operation", JsonString (operationIdText (explodedItemOperation item))),
      ("parents", if explodedItemType item == "decision" then textArray (explodedItemParents item) else JsonNull),
      ("path", JsonString (explodedItemPath item)),
      ("rationale", maybeJson JsonString (explodedItemRationale item)),
      ("raw_semantic", maybeJson JsonString (explodedItemRawSemantic item)),
      ("relation", maybeJson JsonString (explodedItemRelation item)),
      ("summary", maybeJson JsonString (explodedItemSummary item)),
      ("title", maybeJson JsonString (explodedItemTitle item)),
      ("type", JsonString (explodedItemType item))
    ]

parentDiffJson :: ParentDiff -> JsonValue
parentDiffJson diff = object [("diff", JsonString (parentDiffText diff)), ("parent", JsonString (recordIdText (parentDiffParent diff)))]

compareEntryJson :: CompareEntry -> JsonValue
compareEntryJson entry =
  object
    [ ("adr", JsonString (adrIdText (compareEntryAdr entry))),
      ("after", maybeJson compareSnapshotJson (compareEntryAfter entry)),
      ("before", maybeJson compareSnapshotJson (compareEntryBefore entry)),
      ("changes", JsonArray (map compareChangeJson (compareEntryChanges entry))),
      ("kind", JsonString (compareEntryKind entry)),
      ("title", JsonString (compareEntryTitle entry))
    ]

compareSnapshotJson :: CompareSnapshot -> JsonValue
compareSnapshotJson snapshot =
  object
    [ ("adr", JsonString (adrIdText (collapsedAdr collapsed))),
      ("applies_to", textArray (collapsedAppliesTo collapsed)),
      ("body", JsonString (collapsedBody collapsed)),
      ("conflict", maybeJson JsonString (collapsedConflictText collapsed)),
      ("conflicts", JsonArray (map resolutionEntryJson (resolutionStateConflicts resolution))),
      ("counts", compareSnapshotCountsJson (collapsedCounts collapsed)),
      ("domain_heads", textArray (compareSnapshotDomainHeads snapshot)),
      ("domains", textArray (collapsedDomains collapsed)),
      ("obsolete", JsonBool (collapsedObsolete collapsed)),
      ("record", maybeJson (JsonString . recordIdText) (collapsedRecord collapsed)),
      ("record_heads", textArray (compareSnapshotRecordHeads snapshot)),
      ("replacement", maybeJson (JsonString . adrIdText) (collapsedReplacement collapsed)),
      ("resolution_required", JsonBool (resolutionStateRequired resolution)),
      ("resolved", JsonBool (resolutionStateResolved resolution)),
      ("scope_heads", textArray (compareSnapshotScopeHeads snapshot)),
      ("state_token", JsonString (stateTokenText (collapsedStateToken collapsed))),
      ("status", JsonString (collapsedStatus collapsed)),
      ("status_heads", textArray (compareSnapshotStatusHeads snapshot)),
      ("summary", JsonString (collapsedSummary collapsed)),
      ("title", JsonString (collapsedTitle collapsed))
    ]
  where
    collapsed = compareSnapshotCollapsed snapshot
    resolution = collapsedResolution collapsed

compareSnapshotCountsJson :: ProjectionCounts -> JsonValue
compareSnapshotCountsJson counts =
  object
    [ ("amendments", intJson (projectionAmendments counts)),
      ("domain_revisions", intJson (projectionDomainRevisions counts)),
      ("records", intJson (projectionEditions counts)),
      ("scope_revisions", intJson (projectionScopeRevisions counts)),
      ("status_revisions", intJson (projectionStatusRevisions counts))
    ]

compareChangeJson :: CompareChange -> JsonValue
compareChangeJson change =
  objectOmittingNulls
    [ ("after", compareChangeAfter change),
      ("before", compareChangeBefore change),
      ("diff", maybeJson JsonString (compareChangeDiff change)),
      ("field", JsonString (compareChangeField change))
    ]

collapsedConflictText :: CollapsedProjection -> Maybe Text
collapsedConflictText projection =
  nonEmptyText (Text.intercalate "; " (map resolutionSummary (resolutionStateConflicts (collapsedResolution projection))))

validateQuerySnapshot :: ReadSnapshot -> Either QueryError ()
validateQuerySnapshot snapshot = case validateReadSnapshot snapshot of Left problem -> Left (QuerySnapshotInvalid problem); Right () -> Right ()

ownerAdr :: ManagedRecord -> AdrId
ownerAdr record = case record of ManagedDecision decision -> decisionAdr decision; ManagedConnection connection -> connectionAdr connection

connectionAdr :: ConnectionRecord -> AdrId
connectionAdr connection =
  case connectionPayload connection of
    AmendsConnection payload -> amendsSubjectAdr payload
    AppliesToConnection payload -> appliesToSubjectAdr payload
    DomainsConnection payload -> domainsSubjectAdr payload
    StatusConnection payload -> statusSubjectAdr payload

findDocument :: ProvenanceObjectId -> ReadSnapshot -> Maybe ParsedManagedDocument
findDocument objectId snapshot =
  firstOf
    ( sortOn (repoPathText . parsedManagedPath)
        [ document
          | document <- readSnapshotDocuments snapshot,
            provenanceObjectId (parsedManagedCapsule document) == objectId
        ]
    )

pathForObject :: ReadSnapshot -> ProvenanceObjectId -> Text
pathForObject snapshot objectId = maybe "" (repoPathText . parsedManagedPath) (findDocument objectId snapshot)

singletonHead :: (identifier -> ProvenanceObjectId) -> [identifier] -> Maybe ProvenanceObjectId
singletonHead constructor heads = case heads of [identifier] -> Just (constructor identifier); _ -> Nothing

isDecision :: ManagedRecord -> Bool
isDecision record = case record of ManagedDecision _ -> True; _ -> False

documentObjectText :: ParsedManagedDocument -> Text
documentObjectText = provenanceObjectIdText . provenanceObjectId . parsedManagedCapsule

firstOf :: [value] -> Maybe value
firstOf values = case values of [] -> Nothing; value : _ -> Just value

maybeJson :: (value -> JsonValue) -> Maybe value -> JsonValue
maybeJson renderValue value = case value of Nothing -> JsonNull; Just present -> renderValue present

optionalTextArray :: Text -> [Text] -> (Text, JsonValue)
optionalTextArray key values = (key, if null values then JsonNull else textArray values)

textArray :: [Text] -> JsonValue
textArray = JsonArray . map JsonString

intJson :: Int -> JsonValue
intJson = JsonNumber . fromIntegral

nonEmptyText :: Text -> Maybe Text
nonEmptyText value = if Text.null (Text.strip value) then Nothing else Just (Text.strip value)

sortedUnique :: (Ord value) => [value] -> [value]
sortedUnique = Set.toAscList . Set.fromList

stableUnique :: (Ord value) => [value] -> [value]
stableUnique = go Set.empty
  where
    go _ [] = []
    go seen (value : remaining)
      | Set.member value seen = go seen remaining
      | otherwise = value : go (Set.insert value seen) remaining

mapLeft :: (left -> otherLeft) -> Either left right -> Either otherLeft right
mapLeft action value = case value of
  Left problem -> Left (action problem)
  Right result -> Right result

actorProjectionText :: Actor -> Text
actorProjectionText actor = actorKindProjectionText (actorKind actor) <> ":" <> actorId actor

actorKindProjectionText :: ActorKind -> Text
actorKindProjectionText kind = case kind of HumanActor -> "human"; LlmActor -> "llm"; ServiceActor -> "service"

asciiUpper :: Text -> Text
asciiUpper = Text.map upper
  where
    upper character
      | character >= 'a' && character <= 'z' = toEnum (fromEnum character - 32)
      | otherwise = character

historyDetailsOrEmpty :: Maybe HistoryDetails -> HistoryDetails
historyDetailsOrEmpty value = case value of Just details -> details; Nothing -> HistoryDetails [] [] [] [] [] [] []
