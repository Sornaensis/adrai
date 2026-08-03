{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Revision-local provenance grouping and concise semantic history.
module Adrai.History
  ( RevisionIdentity (..),
    CommitPlacementEvidence (..),
    LineLandingEvidence (..),
    PlacementEvidence (..),
    ReadSnapshot (..),
    SnapshotConsistencyError (..),
    validateReadSnapshot,
    ActorSelector (..),
    HistoryOrder (..),
    HistoryOptions (..),
    defaultHistoryOptions,
    HistoryError (..),
    HistoryDetails (..),
    HistoryOperation (..),
    HistoryProjection (..),
    EvolutionSummary (..),
    projectHistory,
    historyOperationsOldestFirst,
    summarizeEvolution,
    isoTimestampFromMs,
    historyProjectionJson,
    renderHistoryProjection,
  )
where

import Adrai.Domain (domainRefinementText, domainText)
import Adrai.Format.Document
import Adrai.Format.Json
import Adrai.Graph
import Adrai.Provenance
import Adrai.Scope (scopePatternText)
import Adrai.Types
import Data.ByteString (ByteString)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

data RevisionIdentity = RevisionIdentity
  { revisionRequested :: Text,
    revisionResolved :: Text
  }
  deriving (Eq, Show)

data LineLandingEvidence = LineLandingEvidence
  { landingLine :: Text,
    landingRef :: Text,
    landingCommit :: Text,
    landingComplete :: Bool
  }
  deriving (Eq, Ord, Show)

data CommitPlacementEvidence = CommitPlacementEvidence
  { commitPlacementOid :: Text,
    commitPlacementClassification :: Text,
    commitPlacementReachable :: Bool,
    commitPlacementAuthoredAtMs :: Integer,
    commitPlacementCommittedAtMs :: Integer,
    commitPlacementSubject :: Text,
    commitPlacementParents :: [Text]
  }
  deriving (Eq, Ord, Show)

-- | Evidence observed by a repository reader. The capsule basis is not a
-- placement commit and is intentionally represented elsewhere.
data PlacementEvidence = PlacementEvidence
  { placementCommit :: Maybe Text,
    placementLabel :: Maybe Text,
    placementCommits :: [CommitPlacementEvidence],
    placementOriginalCommits :: [Text],
    placementIntroductions :: [Text],
    placementLineLandings :: [LineLandingEvidence]
  }
  deriving (Eq, Show)

data ReadSnapshot = ReadSnapshot
  { readSnapshotRevision :: RevisionIdentity,
    readSnapshotDocuments :: [ParsedManagedDocument],
    readSnapshotReduction :: GraphReduction,
    readSnapshotPlacement :: Map OperationId PlacementEvidence
  }
  deriving (Eq, Show)

data SnapshotConsistencyError = SnapshotConsistencyError [Text]
  deriving (Eq, Show)

-- | Reject a caller-assembled snapshot whose document/provenance set cannot
-- have produced its supplied graph reduction. This makes the pure boundary
-- explicit instead of allowing collapsed and exploded views to disagree.
validateReadSnapshot :: ReadSnapshot -> Either SnapshotConsistencyError ()
validateReadSnapshot snapshot
  | null problems = Right ()
  | otherwise = Left (SnapshotConsistencyError (Set.toAscList (Set.fromList problems)))
  where
    documents = readSnapshotDocuments snapshot
    records = map parsedManagedRecord documents
    reducedFromDocuments = reduceManagedGraph records
    reductionProblems =
      ["supplied graph reduction does not match parsed managed documents" | reducedFromDocuments /= readSnapshotReduction snapshot]
    objectProblems =
      [ "capsule object "
          <> provenanceObjectIdText actual
          <> " does not match document object "
          <> provenanceObjectIdText expected
        | document <- documents,
          let actual = provenanceObjectId (parsedManagedCapsule document),
          let expected = managedProvenanceObject (parsedManagedRecord document),
          actual /= expected
      ]
    duplicatePathProblems =
      [ "object " <> provenanceObjectIdText objectId <> " appears at multiple paths"
        | (objectId, paths) <- Map.toAscList objectPaths,
          Set.size paths > 1
      ]
    objectPaths =
      Map.fromListWith Set.union
        [ (provenanceObjectId (parsedManagedCapsule document), Set.singleton (repoPathText (parsedManagedPath document)))
          | document <- documents
        ]
    operationProblems =
      [ "operation " <> operationIdText operation <> " has inconsistent capsule fields"
        | (operation, capsules) <- Map.toAscList operationCapsules,
          not (allSame (map provenanceOperationContext capsules))
      ]
    operationCapsules =
      Map.fromListWith (<>)
        [ (provenanceOperationId capsule, [capsule])
          | document <- documents,
            let capsule = parsedManagedCapsule document
        ]
    problems = reductionProblems <> objectProblems <> duplicatePathProblems <> operationProblems

data ActorSelector = ActorSelector
  { actorSelectorKind :: ActorKind,
    actorSelectorId :: Text
  }
  deriving (Eq, Ord, Show)

data HistoryOrder = NewestFirst | OldestFirst
  deriving (Eq, Ord, Show)

data HistoryOptions = HistoryOptions
  { historyOptionOrder :: HistoryOrder,
    historyOptionLimit :: Int,
    historyOptionActor :: Maybe ActorSelector,
    historyOptionSince :: Maybe Integer,
    historyOptionUntil :: Maybe Integer
  }
  deriving (Eq, Show)

defaultHistoryOptions :: HistoryOptions
defaultHistoryOptions = HistoryOptions NewestFirst 20 Nothing Nothing Nothing

data HistoryError
  = HistoryAdrNotFound AdrId
  | HistoryInvalidLimit Int
  | HistorySnapshotInvalid SnapshotConsistencyError
  deriving (Eq, Show)

data HistoryDetails = HistoryDetails
  { historyAddedScope :: [Text],
    historyRemovedScope :: [Text],
    historyAddedDomains :: [Text],
    historyRemovedDomains :: [Text],
    historyDomainRefinements :: [Text],
    historyReplacementAdrs :: [Text],
    historyChangedFields :: [Text]
  }
  deriving (Eq, Show)

data HistoryOperation = HistoryOperation
  { historyOperationId :: OperationId,
    historyOperationAdr :: AdrId,
    historyOperationTitle :: Text,
    historyOperationLabel :: Text,
    historyOperationReason :: Maybe Text,
    historyOperationChanges :: [Text],
    historyOperationDetails :: Maybe HistoryDetails,
    historyOperationActor :: Actor,
    historyOperationClaimedAt :: Integer,
    historyOperationBasis :: Text,
    historyOperationCommit :: Maybe Text,
    historyOperationPlacement :: Maybe Text,
    historyOperationResolved :: Bool,
    historyOperationConflict :: Maybe Text,
    historyOperationStatus :: Text
  }
  deriving (Eq, Show)

data HistoryProjection = HistoryProjection
  { historyProjectionRevision :: RevisionIdentity,
    historyProjectionAdr :: Maybe AdrId,
    historyProjectionOrder :: HistoryOrder,
    historyProjectionLimit :: Int,
    historyProjectionTruncated :: Bool,
    historyProjectionOptions :: HistoryOptions,
    historyProjectionOperations :: [HistoryOperation]
  }
  deriving (Eq, Show)

data EvolutionSummary = EvolutionSummary
  { evolutionSummaryText :: Text,
    evolutionOperationCount :: Int,
    evolutionLatest :: Maybe HistoryOperation
  }
  deriving (Eq, Show)

data OperationGroup = OperationGroup
  { groupOperation :: OperationId,
    groupDocuments :: [ParsedManagedDocument],
    groupCapsule :: ProvenanceCapsule,
    groupAdrs :: [AdrId],
    groupParents :: [OperationId]
  }

projectHistory :: ReadSnapshot -> Maybe AdrId -> HistoryOptions -> Either HistoryError HistoryProjection
projectHistory snapshot requestedAdr options = do
  mapSnapshotError (validateReadSnapshot snapshot)
  if historyOptionLimit options < 1 || historyOptionLimit options > 1000
    then Left (HistoryInvalidLimit (historyOptionLimit options))
    else pure ()
  case requestedAdr of
    Just adr
      | lookupReducedAdr adr (readSnapshotReduction snapshot) == Nothing -> Left (HistoryAdrNotFound adr)
    _ -> pure ()
  let oldest = historyOperationsOldestFirst snapshot requestedAdr
      displayed = case historyOptionOrder options of OldestFirst -> oldest; NewestFirst -> reverse oldest
      filtered = filter (matchesOptions options) displayed
  Right
    HistoryProjection
      { historyProjectionRevision = readSnapshotRevision snapshot,
        historyProjectionAdr = requestedAdr,
        historyProjectionOrder = historyOptionOrder options,
        historyProjectionLimit = historyOptionLimit options,
        historyProjectionTruncated = length filtered > historyOptionLimit options,
        historyProjectionOptions = options,
        historyProjectionOperations = take (historyOptionLimit options) filtered
      }

historyOperationsOldestFirst :: ReadSnapshot -> Maybe AdrId -> [HistoryOperation]
historyOperationsOldestFirst snapshot requestedAdr = mapMaybe materialize ordered
  where
    relevant = filter relevantGroup (operationGroups snapshot)
    relevantGroup group = maybe True (`elem` groupAdrs group) requestedAdr
    ordered = semanticTopological relevant
    materialize group = do
      adr <- case requestedAdr of Just selected -> Just selected; Nothing -> firstOf (groupAdrs group)
      reduced <- lookupReducedAdr adr (readSnapshotReduction snapshot)
      pure (operationFromGroup snapshot reduced group)

matchesOptions :: HistoryOptions -> HistoryOperation -> Bool
matchesOptions options operation = actorMatches && sinceMatches && untilMatches
  where
    actorMatches =
      case historyOptionActor options of
        Nothing -> True
        Just selector ->
          actorKind (historyOperationActor operation) == actorSelectorKind selector
            && actorId (historyOperationActor operation) == actorSelectorId selector
    sinceMatches = maybe True (<= historyOperationClaimedAt operation) (historyOptionSince options)
    untilMatches = maybe True (>= historyOperationClaimedAt operation) (historyOptionUntil options)

operationGroups :: ReadSnapshot -> [OperationGroup]
operationGroups snapshot = mapMaybe build (Map.toAscList grouped)
  where
    documents = sortOn documentKey (readSnapshotDocuments snapshot)
    grouped = Map.fromListWith (<>) [(provenanceOperationId (parsedManagedCapsule document), [document]) | document <- documents]
    objectOwners =
      Map.fromListWith Set.union
        [ (provenanceObjectId (parsedManagedCapsule document), Set.singleton (provenanceOperationId (parsedManagedCapsule document)))
          | document <- documents
        ]
    build (operation, docs) =
      case sortOn documentKey docs of
        [] -> Nothing
        sortedDocs@(firstDocument : _) ->
          Just
            OperationGroup
              { groupOperation = operation,
                groupDocuments = sortedDocs,
                groupCapsule = parsedManagedCapsule firstDocument,
                groupAdrs = sortedUnique (map (ownerAdr . parsedManagedRecord) sortedDocs),
                groupParents =
                  Set.toAscList
                    . Set.delete operation
                    . Set.unions
                    $ [Map.findWithDefault Set.empty parent objectOwners | document <- sortedDocs, parent <- documentParentObjects document]
              }

-- | Kahn ordering for the valid prefix. If a cycle or unreachable component
-- remains, append the entire residual set at once in stable display order.
semanticTopological :: [OperationGroup] -> [OperationGroup]
semanticTopological groups = go Set.empty []
  where
    byId = Map.fromList [(groupOperation group, group) | group <- groups]
    ids = Map.keysSet byId
    localParents group = Set.fromList (filter (`Set.member` ids) (groupParents group))
    stableKey group = (provenanceTimestampMs (groupCapsule group), operationIdText (groupOperation group))
    go emitted result =
      let remaining = [group | group <- Map.elems byId, Set.notMember (groupOperation group) emitted]
          ready = sortOn stableKey [group | group <- remaining, localParents group `Set.isSubsetOf` emitted]
       in case ready of
            next : _ -> go (Set.insert (groupOperation next) emitted) (result <> [next])
            [] -> result <> sortOn stableKey remaining

operationFromGroup :: ReadSnapshot -> ReducedAdr -> OperationGroup -> HistoryOperation
operationFromGroup snapshot reduced group =
  HistoryOperation
    { historyOperationId = groupOperation group,
      historyOperationAdr = reducedAdrId reduced,
      historyOperationTitle = operationTitle reduced records,
      historyOperationLabel = label,
      historyOperationReason = reason,
      historyOperationChanges = operationChanges events label,
      historyOperationDetails = details,
      historyOperationActor = provenanceActor capsule,
      historyOperationClaimedAt = provenanceTimestampMs capsule,
      historyOperationBasis = gitOidText (provenanceBasis capsule),
      historyOperationCommit = preferredCommit =<< evidence,
      historyOperationPlacement = preferredClassification =<< evidence,
      historyOperationResolved = null (reducedConflictAxes reduced) && null localIssues,
      historyOperationConflict = currentConflict reduced localIssues,
      historyOperationStatus = currentStatus reduced
    }
  where
    capsule = groupCapsule group
    records = map parsedManagedRecord (groupDocuments group)
    events = sortedUnique (map (eventKindText . provenanceEventKind . parsedManagedCapsule) (groupDocuments group))
    evidence = Map.lookup (groupOperation group) (readSnapshotPlacement snapshot)
    label = operationLabel events records
    reason = operationReason reduced records label
    details = operationDetails snapshot group
    localIssues = [issue | issue <- graphReductionIssues (readSnapshotReduction snapshot), graphIssueAdr issue == Just (reducedAdrId reduced)]

preferredCommit :: PlacementEvidence -> Maybe Text
preferredCommit evidence =
  case preferredPlacement evidence of
    Just placement -> Just (commitPlacementOid placement)
    Nothing -> placementCommit evidence

preferredClassification :: PlacementEvidence -> Maybe Text
preferredClassification evidence =
  case preferredPlacement evidence of
    Just placement -> Just (commitPlacementClassification placement)
    Nothing -> placementLabel evidence

preferredPlacement :: PlacementEvidence -> Maybe CommitPlacementEvidence
preferredPlacement = firstOf . sortOn placementPreference . placementCommits
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

operationLabel :: [Text] -> [ManagedRecord] -> Text
operationLabel events records
  | "decision.create" `elem` events = "created"
  | "decision.amend" `elem` events || not (null amendments) =
      if any ((> 1) . length . amendsToRecords) amendments then "reconciled amendments" else "amended"
  | Just event <- firstOf (filter ("scope." `Text.isPrefixOf`) events) = changeLabel "scope" (eventMode event)
  | Just event <- firstOf (filter ("domain." `Text.isPrefixOf`) events) = changeLabel "domains" (eventMode event)
  | "decision.obsolete" `elem` events = "obsoleted"
  | "decision.reactivate" `elem` events = "reactivated"
  | Just status <- firstStatus records = case statusState status of StatusObsolete -> "obsoleted"; StatusActive -> "reactivated"
  | otherwise = "changed"
  where
    amendments = [payload | ManagedConnection connection <- records, AmendsConnection payload <- [connectionPayload connection]]

eventMode :: Text -> Text
eventMode = Text.drop 1 . Text.dropWhile (/= '.')

changeLabel :: Text -> Text -> Text
changeLabel noun mode =
  case mode of
    "expand" -> "expanded " <> noun
    "contract" -> "contracted " <> noun
    "mixed" -> "changed " <> noun
    "refine" -> "refined " <> noun
    "replace" -> "replaced " <> noun
    "merge" -> "reconciled " <> noun
    "initial" -> "set " <> noun
    _ -> "changed " <> noun

operationChanges :: [Text] -> Text -> [Text]
operationChanges events label
  | label == "created" =
      [axis | (event, axis) <- [("decision.create", "decision"), ("scope.initial", "scope"), ("domain.initial", "domains"), ("status.initial", "status")], event `elem` events]
  | label `elem` ["amended", "reconciled amendments"] = ["decision"]
  | any ("scope." `Text.isPrefixOf`) events = ["scope"]
  | any ("domain." `Text.isPrefixOf`) events = ["domains"]
  | label `elem` ["obsoleted", "reactivated"] = ["status"]
  | otherwise =
      [ axis
        | (predicate, axis) <-
            [ (any ("scope" `Text.isInfixOf`) events, "scope"),
              (any ("domain" `Text.isInfixOf`) events, "domains"),
              (any (\event -> "status" `Text.isInfixOf` event || "obsolete" `Text.isInfixOf` event) events, "status"),
              (any (\event -> "decision" `Text.isInfixOf` event || "amend" `Text.isInfixOf` event) events, "decision")
            ],
          predicate
      ]

operationTitle :: ReducedAdr -> [ManagedRecord] -> Text
operationTitle reduced records =
  case lastMaybe [decisionTitle decision | ManagedDecision decision <- records] of
    Just title -> title
    Nothing -> maybe "[conflicted ADR]" decisionTitle (axisResolutionEffective (reducedDecisionAxis reduced))

operationReason :: ReducedAdr -> [ManagedRecord] -> Text -> Maybe Text
operationReason reduced records label
  | label == "created" = nonEmptyText (maybe (maybe "" decisionSummary effective) id (lastMaybe [decisionSummary decision | ManagedDecision decision <- records]))
  | otherwise = nonEmptyText (Text.intercalate " / " rationales)
  where
    effective = axisResolutionEffective (reducedDecisionAxis reduced)
    rationales = stableUnique (filter (not . Text.null) [Text.strip (connectionRationale connection) | ManagedConnection connection <- records])

operationDetails :: ReadSnapshot -> OperationGroup -> Maybe HistoryDetails
operationDetails snapshot group = if detailsEmpty details then Nothing else Just details
  where
    records = map parsedManagedRecord (groupDocuments group)
    scopes = mapMaybe asScope records
    domains = mapMaybe asDomain records
    statuses = mapMaybe asStatus records
    details =
      HistoryDetails
        { historyAddedScope = sortedUnique [scopePatternText value | payload <- scopes, value <- appliesToAdded payload],
          historyRemovedScope = sortedUnique [scopePatternText value | payload <- scopes, value <- appliesToRemoved payload],
          historyAddedDomains = sortedUnique [domainText value | payload <- domains, value <- domainsAdded payload],
          historyRemovedDomains = sortedUnique [domainText value | payload <- domains, value <- domainsRemoved payload],
          historyDomainRefinements = sortedUnique [domainRefinementText value | payload <- domains, value <- domainsRefinements payload],
          historyReplacementAdrs = sortedUnique [adrIdText value | payload <- statuses, value <- maybeToList (statusReplacementAdr payload)],
          historyChangedFields = changedDecisionFields snapshot group
        }

changedDecisionFields :: ReadSnapshot -> OperationGroup -> [Text]
changedDecisionFields snapshot group =
  [ label
    | (label, differs) <- [("title", titleDiffers), ("summary", summaryDiffers), ("decision text", bodyDiffers)],
      differs
  ]
  where
    allRecords = map parsedManagedRecord (readSnapshotDocuments snapshot)
    decisions = Map.fromList [(decisionRecord decision, decision) | ManagedDecision decision <- allRecords]
    parentMap =
      Map.fromListWith (<>)
        [ (amendsFromRecord payload, amendsToRecords payload)
          | ManagedConnection connection <- allRecords,
            AmendsConnection payload <- [connectionPayload connection]
        ]
    localDecisions = [decision | ManagedDecision decision <- map parsedManagedRecord (groupDocuments group)]
    pairs =
      [ (parent, child)
        | child <- localDecisions,
          parentId <- Map.findWithDefault [] (decisionRecord child) parentMap,
          Just parent <- [Map.lookup parentId decisions]
      ]
    titleDiffers = any (\(parent, child) -> decisionTitle parent /= decisionTitle child) pairs
    summaryDiffers = any (\(parent, child) -> decisionSummary parent /= decisionSummary child) pairs
    bodyDiffers = any (\(parent, child) -> decisionBody parent /= decisionBody child) pairs

detailsEmpty :: HistoryDetails -> Bool
detailsEmpty details =
  all null
    [ historyAddedScope details,
      historyRemovedScope details,
      historyAddedDomains details,
      historyRemovedDomains details,
      historyDomainRefinements details,
      historyReplacementAdrs details,
      historyChangedFields details
    ]

summarizeEvolution :: [HistoryOperation] -> EvolutionSummary
summarizeEvolution operations =
  EvolutionSummary
    { evolutionSummaryText = if null labels then "no operations" else Text.intercalate " \x2192 " (map renderRun (runs labels)),
      evolutionOperationCount = length operations,
      evolutionLatest = lastMaybe operations
    }
  where
    labels = dropLaterCreated (map historyOperationLabel operations)
    renderRun (label, count) = if count <= 1 then label else label <> " \x00d7" <> Text.pack (show count)
    dropLaterCreated [] = []
    dropLaterCreated (first : remaining) = first : filter (/= "created") remaining

historyProjectionJson :: HistoryProjection -> JsonValue
historyProjectionJson projection =
  object
    [ ("adr", maybeJson (JsonString . adrIdText) (historyProjectionAdr projection)),
      ("as_of", JsonString (revisionResolved revision)),
      ("filters", filtersJson options),
      ("limit", JsonNumber (fromIntegral (historyProjectionLimit projection))),
      ("operations", JsonArray (map historyOperationJson (historyProjectionOperations projection))),
      ("order", JsonString (orderText (historyProjectionOrder projection))),
      ("schema", JsonString "adrai/history/v1"),
      ("truncated", JsonBool (historyProjectionTruncated projection)),
      ("view", JsonString "history")
    ]
  where
    revision = historyProjectionRevision projection
    options = historyProjectionOptions projection

renderHistoryProjection :: HistoryProjection -> ByteString
renderHistoryProjection = renderCanonicalJsonBytes . historyProjectionJson

historyOperationJson :: HistoryOperation -> JsonValue
historyOperationJson operation =
  objectOmittingNulls
    [ ("actor", JsonString (actorText (historyOperationActor operation))),
      ("adr", JsonString (adrIdText (historyOperationAdr operation))),
      ("changes", textArray (historyOperationChanges operation)),
      ("claimed_at", JsonString (isoTimestampFromMs (historyOperationClaimedAt operation))),
      ("commit", maybeJson JsonString (historyOperationCommit operation)),
      ("conflict", maybeJson JsonString (historyOperationConflict operation)),
      ("details", detailsJson (fromMaybeDetails (historyOperationDetails operation))),
      ("label", JsonString (historyOperationLabel operation)),
      ("model", maybeJson JsonString (actorModel (historyOperationActor operation))),
      ("operation", JsonString (operationIdText (historyOperationId operation))),
      ("placement", maybeJson JsonString (historyOperationPlacement operation)),
      ("reason", maybeJson JsonString (historyOperationReason operation)),
      ("resolved", JsonBool (historyOperationResolved operation)),
      ("status", JsonString (historyOperationStatus operation)),
      ("title", JsonString (historyOperationTitle operation))
    ]

detailsJson :: HistoryDetails -> JsonValue
detailsJson details =
  objectOmittingNulls
    [ optionalArray "added_domains" (historyAddedDomains details),
      optionalArray "added_scope" (historyAddedScope details),
      optionalArray "changed_fields" (historyChangedFields details),
      optionalArray "domain_refinements" (historyDomainRefinements details),
      optionalArray "removed_domains" (historyRemovedDomains details),
      optionalArray "removed_scope" (historyRemovedScope details),
      optionalArray "replacement_adrs" (historyReplacementAdrs details)
    ]

filtersJson :: HistoryOptions -> JsonValue
filtersJson options =
  object
    [ ("actor", maybeJson (JsonString . selectorText) (historyOptionActor options)),
      ("since", maybeJson (JsonString . isoTimestampFromMs) (historyOptionSince options)),
      ("until", maybeJson (JsonString . isoTimestampFromMs) (historyOptionUntil options))
    ]

isoTimestampFromMs :: Integer -> Text
isoTimestampFromMs milliseconds =
  Text.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" utc)
    <> fractional
    <> "Z"
  where
    (seconds, remainder) = milliseconds `divMod` 1000
    utc = posixSecondsToUTCTime (fromInteger seconds)
    micros = remainder * 1000
    digits = Text.pack (show micros)
    fractional
      | remainder == 0 = ""
      | otherwise = "." <> Text.replicate (6 - Text.length digits) "0" <> digits

selectorText :: ActorSelector -> Text
selectorText selector = actorKindText (actorSelectorKind selector) <> ":" <> actorSelectorId selector

actorText :: Actor -> Text
actorText actor = actorKindText (actorKind actor) <> ":" <> actorId actor

actorKindText :: ActorKind -> Text
actorKindText kind = case kind of HumanActor -> "human"; LlmActor -> "llm"; ServiceActor -> "service"

orderText :: HistoryOrder -> Text
orderText order = case order of NewestFirst -> "newest-first"; OldestFirst -> "oldest-first"

currentStatus :: ReducedAdr -> Text
currentStatus reduced =
  case axisResolutionEffective (reducedStatusAxis reduced) of
    Just status -> case reducedStatusState status of StatusActive -> "active"; StatusObsolete -> "obsolete"
    Nothing -> "conflict"

currentConflict :: ReducedAdr -> [GraphIssue] -> Maybe Text
currentConflict reduced issues =
  nonEmptyText
    . Text.intercalate "; "
    . stableUnique
    $ reducedConflictMessages reduced <> [Text.pack (show (length issues)) <> " integrity errors" | not (null issues)]

ownerAdr :: ManagedRecord -> AdrId
ownerAdr record = case record of ManagedDecision decision -> decisionAdr decision; ManagedConnection connection -> connectionAdr connection

connectionAdr :: ConnectionRecord -> AdrId
connectionAdr connection =
  case connectionPayload connection of
    AmendsConnection payload -> amendsSubjectAdr payload
    AppliesToConnection payload -> appliesToSubjectAdr payload
    DomainsConnection payload -> domainsSubjectAdr payload
    StatusConnection payload -> statusSubjectAdr payload

documentParentObjects :: ParsedManagedDocument -> [ProvenanceObjectId]
documentParentObjects document = provenanceParents capsule <> semanticParents (parsedManagedRecord document)
  where
    capsule = parsedManagedCapsule document

semanticParents :: ManagedRecord -> [ProvenanceObjectId]
semanticParents record =
  case record of
    ManagedDecision _ -> []
    ManagedConnection connection ->
      case connectionPayload connection of
        AmendsConnection payload -> map ProvenanceRecord (amendsToRecords payload)
        AppliesToConnection payload -> map ProvenanceConnection (appliesToParentConnections payload)
        DomainsConnection payload -> map ProvenanceConnection (domainsParentConnections payload)
        StatusConnection payload -> map ProvenanceConnection (statusParentConnections payload)

managedProvenanceObject :: ManagedRecord -> ProvenanceObjectId
managedProvenanceObject record =
  case record of
    ManagedDecision decision -> ProvenanceRecord (decisionRecord decision)
    ManagedConnection connection -> ProvenanceConnection (connectionRecordId connection)

documentKey :: ParsedManagedDocument -> (Text, Text)
documentKey document =
  (operationIdText (provenanceOperationId capsule), provenanceObjectIdText (provenanceObjectId capsule))
  where
    capsule = parsedManagedCapsule document

firstStatus :: [ManagedRecord] -> Maybe StatusPayload
firstStatus = firstOf . mapMaybe asStatus

asScope :: ManagedRecord -> Maybe AppliesToPayload
asScope (ManagedConnection connection) = case connectionPayload connection of AppliesToConnection payload -> Just payload; _ -> Nothing
asScope _ = Nothing

asDomain :: ManagedRecord -> Maybe DomainsPayload
asDomain (ManagedConnection connection) = case connectionPayload connection of DomainsConnection payload -> Just payload; _ -> Nothing
asDomain _ = Nothing

asStatus :: ManagedRecord -> Maybe StatusPayload
asStatus (ManagedConnection connection) = case connectionPayload connection of StatusConnection payload -> Just payload; _ -> Nothing
asStatus _ = Nothing

mapSnapshotError :: Either SnapshotConsistencyError () -> Either HistoryError ()
mapSnapshotError result = case result of Left problem -> Left (HistorySnapshotInvalid problem); Right () -> Right ()

maybeToList :: Maybe value -> [value]
maybeToList value = case value of Nothing -> []; Just present -> [present]

firstOf :: [value] -> Maybe value
firstOf values = case values of [] -> Nothing; value : _ -> Just value

lastMaybe :: [value] -> Maybe value
lastMaybe = foldl (\_ value -> Just value) Nothing

sortedUnique :: (Ord value) => [value] -> [value]
sortedUnique = Set.toAscList . Set.fromList

stableUnique :: (Ord value) => [value] -> [value]
stableUnique = go Set.empty
  where
    go _ [] = []
    go seen (value : remaining)
      | Set.member value seen = go seen remaining
      | otherwise = value : go (Set.insert value seen) remaining

runs :: (Eq value) => [value] -> [(value, Int)]
runs values = case values of [] -> []; value : remaining -> let (same, rest) = span (== value) remaining in (value, 1 + length same) : runs rest

nonEmptyText :: Text -> Maybe Text
nonEmptyText value = if Text.null value then Nothing else Just value

maybeJson :: (value -> JsonValue) -> Maybe value -> JsonValue
maybeJson renderValue value = case value of Nothing -> JsonNull; Just present -> renderValue present

optionalArray :: Text -> [Text] -> (Text, JsonValue)
optionalArray key values = (key, if null values then JsonNull else textArray values)

textArray :: [Text] -> JsonValue
textArray = JsonArray . map JsonString

allSame :: (Eq value) => [value] -> Bool
allSame values = case values of [] -> True; first : remaining -> all (== first) remaining

fromMaybeDetails :: Maybe HistoryDetails -> HistoryDetails
fromMaybeDetails value =
  case value of
    Just details -> details
    Nothing -> HistoryDetails [] [] [] [] [] [] []
