{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Adrai.Compiler
  ( ObsoletePolicy (..),
    SearchMaterializationError (..),
    SearchMaterializationStats (..),
    materializeCurrentSearch,
    visibleSearchItemIds,
    writeCurrentSearch,
  )
where

import Adrai.Domain (domainText)
import Adrai.Format.Document
  ( ConnectionPayload (..),
    ConnectionRecord (..),
    DecisionRecord (..),
    ManagedRecord (..),
    ParsedManagedDocument (..),
    StatusState (StatusObsolete),
  )
import Adrai.Graph
  ( AxisResolution (..),
    CurrentConnectionRef (..),
    CurrentDecisionView (..),
    GraphReduction (..),
    ReducedAdr (..),
    ReducedStatus (..),
  )
import Adrai.History (ReadSnapshot (..), SnapshotConsistencyError, validateReadSnapshot)
import Adrai.Markdown (MarkdownSections (..))
import Adrai.Relevance (ChunkError)
import Adrai.Retrieval
import Adrai.Scope (scopePatternText)
import Adrai.Sqlite (SearchStorageError, replaceSearchMaterialization)
import Adrai.Types
  ( ConnectionId,
    RecordId,
    adrIdText,
    recordIdText,
    repoPathText,
  )
import Adrai.Vector (identifierTerms)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple (Connection)

data ObsoletePolicy = ExcludeObsolete | IncludeObsolete
  deriving (Eq, Ord, Show)

data SearchMaterializationError
  = SearchMaterializationSnapshotError SnapshotConsistencyError
  | SearchMaterializationMissingDecision RecordId
  | SearchMaterializationMissingConnection ConnectionId
  | SearchMaterializationChunkError Text ChunkError
  | SearchMaterializationDuplicateItem Text
  | SearchMaterializationDuplicatePassage Text
  | SearchMaterializationStorageError SearchStorageError
  deriving (Eq, Show)

data SearchMaterializationStats = SearchMaterializationStats
  { materializationDocumentCount :: Int,
    materializationPassageCount :: Int,
    materializationAliasCount :: Int
  }
  deriving (Eq, Show)

materializeCurrentSearch :: ReadSnapshot -> Either SearchMaterializationError SearchMaterialization
materializeCurrentSearch snapshot = do
  mapLeft SearchMaterializationSnapshotError (validateReadSnapshot snapshot)
  documents <- concat <$> traverse (materializeAdr indexes) reducedAdrs
  ensureUnique SearchMaterializationDuplicateItem searchDocumentItemId documents
  passages <- concat <$> traverse materializePassages documents
  ensureUnique SearchMaterializationDuplicatePassage searchPassageId passages
  Right
    SearchMaterialization
      { searchMaterializationDocuments = documents,
        searchMaterializationPassages = passages,
        searchMaterializationAliases = materializationAliases documents
      }
  where
    indexes = buildIndexes (readSnapshotDocuments snapshot)
    reducedAdrs = sortOn reducedAdrId (graphReductionAdrs (readSnapshotReduction snapshot))
    materializePassages document =
      mapLeft (SearchMaterializationChunkError (searchDocumentItemId document)) (chunkSearchDocument document)

visibleSearchItemIds :: ObsoletePolicy -> SearchMaterialization -> Set Text
visibleSearchItemIds policy materialization =
  Set.fromList
    [ searchDocumentItemId document
      | document <- searchMaterializationDocuments materialization,
        policy == IncludeObsolete || not (searchDocumentObsolete document)
    ]

writeCurrentSearch :: Connection -> ReadSnapshot -> IO (Either SearchMaterializationError SearchMaterializationStats)
writeCurrentSearch connection snapshot =
  case materializeCurrentSearch snapshot of
    Left problem -> pure (Left problem)
    Right materialization -> do
      stored <- replaceSearchMaterialization connection materialization
      pure $ case stored of
        Left storageProblem -> Left (SearchMaterializationStorageError storageProblem)
        Right () ->
          Right
            SearchMaterializationStats
              { materializationDocumentCount = length (searchMaterializationDocuments materialization),
                materializationPassageCount = length (searchMaterializationPassages materialization),
                materializationAliasCount = length (searchMaterializationAliases materialization)
              }

data SnapshotIndexes = SnapshotIndexes
  { indexedDecisions :: Map RecordId ParsedManagedDocument,
    indexedConnections :: Map ConnectionId ParsedManagedDocument
  }

buildIndexes :: [ParsedManagedDocument] -> SnapshotIndexes
buildIndexes documents =
  SnapshotIndexes
    { indexedDecisions =
        Map.fromList
          [ (decisionRecord decision, document)
            | document <- documents,
              ManagedDecision decision <- [parsedManagedRecord document]
          ],
      indexedConnections =
        Map.fromList
          [ (connectionRecordId connection, document)
            | document <- documents,
              ManagedConnection connection <- [parsedManagedRecord document]
          ]
    }

materializeAdr :: SnapshotIndexes -> ReducedAdr -> Either SearchMaterializationError [SearchDocument]
materializeAdr indexes reduced = do
  connectionDocuments <- traverse (currentConnectionDocument indexes) (reducedCurrentConnections reduced)
  let sortedConnections = stableUniquePaths (sortOn documentPathText connectionDocuments)
      rationale = Text.intercalate "\n" (rationaleEntries sortedConnections)
      connectionPaths = map documentPathText sortedConnections
      candidates = sortOn (decisionRecord . currentDecisionRecord) (reducedCurrentDecisions reduced)
      conflicted = length candidates > 1
  traverse (materializeCandidate indexes reduced conflicted rationale connectionPaths) candidates

currentConnectionDocument :: SnapshotIndexes -> CurrentConnectionRef -> Either SearchMaterializationError ParsedManagedDocument
currentConnectionDocument indexes reference =
  maybe
    (Left (SearchMaterializationMissingConnection (currentConnectionId reference)))
    Right
    (Map.lookup (currentConnectionId reference) (indexedConnections indexes))

materializeCandidate :: SnapshotIndexes -> ReducedAdr -> Bool -> Text -> [Text] -> CurrentDecisionView -> Either SearchMaterializationError SearchDocument
materializeCandidate indexes reduced conflicted rationale connectionPaths current = do
  decisionDocument <-
    maybe
      (Left (SearchMaterializationMissingDecision recordId))
      Right
      (Map.lookup recordId (indexedDecisions indexes))
  let sections = currentDecisionSections current
      effectiveDomains = map domainText (axisResolutionEffective (reducedDomainAxis reduced))
      effectiveScope =
        if axisResolved (reducedScopeAxis reduced)
          then map scopePatternText (axisResolutionEffective (reducedScopeAxis reduced))
          else []
      obsolete =
        case axisResolutionEffective (reducedStatusAxis reduced) of
          Just status -> reducedStatusState status == StatusObsolete
          Nothing -> False
      itemId = if conflicted then adrIdText adrId <> "@" <> recordIdText recordId else adrIdText adrId
      body = Text.strip (decisionBody decision)
      identifierSource =
        Text.intercalate
          "\n"
          [ decisionTitle decision,
            decisionSummary decision,
            body,
            Text.unwords effectiveDomains,
            rationale
          ]
      identifiers =
        Text.unwords
          ( identifierTerms
              True
              identifierSource
          )
      sourcePaths = stableUniqueTexts (documentPathText decisionDocument : connectionPaths)
  Right
    SearchDocument
      { searchDocumentItemId = itemId,
        searchDocumentAdrId = adrId,
        searchDocumentCandidateRecordId = recordId,
        searchDocumentTitle = decisionTitle decision,
        searchDocumentSummary = decisionSummary decision,
        searchDocumentContext = markdownContext sections,
        searchDocumentDecision = markdownDecision sections,
        searchDocumentConsequences = markdownConsequences sections,
        searchDocumentDomains = effectiveDomains,
        searchDocumentRationale = rationale,
        searchDocumentOther = markdownOther sections,
        searchDocumentScope = effectiveScope,
        searchDocumentObsolete = obsolete,
        searchDocumentConflicted = conflicted,
        searchDocumentStateToken = reducedStateToken reduced,
        searchDocumentSourcePaths = sourcePaths,
        searchDocumentIdentifierSource = identifierSource,
        searchDocumentIdentifiers = identifiers
      }
  where
    decision = currentDecisionRecord current
    adrId = reducedAdrId reduced
    recordId = decisionRecord decision

axisResolved :: AxisResolution identifier effective -> Bool
axisResolved resolution = length (axisResolutionHeads resolution) == 1 && axisResolutionConflict resolution == Nothing

documentPathText :: ParsedManagedDocument -> Text
documentPathText = repoPathText . parsedManagedPath

rationaleEntries :: [ParsedManagedDocument] -> [Text]
rationaleEntries documents =
  [ relationLabel (connectionPayload connection) <> ": " <> rationale
    | document <- documents,
      ManagedConnection connection <- [parsedManagedRecord document],
      let rationale = Text.strip (connectionRationale connection),
      not (Text.null rationale)
  ]

relationLabel :: ConnectionPayload -> Text
relationLabel payload = case payload of
  AmendsConnection _ -> "amends"
  AppliesToConnection _ -> "applies_to"
  DomainsConnection _ -> "domains"
  StatusConnection _ -> "status"

stableUniquePaths :: [ParsedManagedDocument] -> [ParsedManagedDocument]
stableUniquePaths = reverse . snd . foldl' add (Set.empty, [])
  where
    add (seen, result) document
      | Set.member path seen = (seen, result)
      | otherwise = (Set.insert path seen, document : result)
      where
        path = documentPathText document

stableUniqueTexts :: [Text] -> [Text]
stableUniqueTexts = reverse . snd . foldl' add (Set.empty, [])
  where
    add (seen, result) value
      | Set.member value seen = (seen, result)
      | otherwise = (Set.insert value seen, value : result)

ensureUnique :: (Ord key) => (key -> SearchMaterializationError) -> (value -> key) -> [value] -> Either SearchMaterializationError ()
ensureUnique makeError select = go Set.empty
  where
    go _ [] = Right ()
    go seen (value : values)
      | Set.member key seen = Left (makeError key)
      | otherwise = go (Set.insert key seen) values
      where
        key = select value

mapLeft :: (left -> other) -> Either left value -> Either other value
mapLeft convert result = case result of
  Left problem -> Left (convert problem)
  Right value -> Right value
