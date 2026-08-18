{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Adrai.Compiler
  ( ObsoletePolicy (..),
    SearchMaterializationError (..),
    SearchMaterializationStats (..),
    ColdCompilerError (..),
    ColdCompilerResult (..),
    coldCompileRepository,
    materializeReducedSearch,
    materializeParsedReducedSearch,
    materializeCurrentSearch,
    visibleSearchItemIds,
    writeCurrentSearch,
  )
where

import Adrai.Domain (domainText)
import Adrai.Compiler.Snapshot
  ( AnalyzedRepositorySnapshot,
    CompilerDiagnostic (..),
    CompilerDiagnosticOrigin (..),
    CompilerDiagnosticSeverity (..),
    ParsedReducedRepositorySnapshot,
    analyzeRepositorySnapshot,
    analyzedConflicts,
    analyzedDiagnostics,
    analyzedDocuments,
    analyzedReduction,
    analyzedSourceFingerprint,
    compilerDiagnosticCodeText,
    gateAnalyzedRepositorySnapshot,
    parsedReducedDocuments,
    parsedReducedReduction,
  )
import Adrai.Format.Document
  ( ConnectionPayload (..),
    ConnectionRecord (..),
    DecisionRecord (..),
    ManagedRecord (..),
    ParsedManagedDocument (..),
    StatusState (StatusObsolete),
  )
import Adrai.Format (renderDigest)
import Adrai.Graph
  ( AdrConflict (..),
    AxisResolution (..),
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
import Adrai.Provenance
  ( eventKindText,
    gitOidText,
    lineAnchorCommit,
    lineAnchorId,
    provenanceActor,
    provenanceBasis,
    provenanceBranchHint,
    provenanceEventKind,
    provenanceInputs,
    provenanceLineAnchors,
    provenanceObjectId,
    provenanceObjectIdText,
    provenanceOperationId,
    provenanceParents,
    provenanceSemanticDigest,
    provenanceTimestampMs,
    provenanceToolVersion,
    provenanceUpstreamHint,
    sha256DigestFrames,
  )
import Adrai.Repository
  ( RepositorySnapshotError,
    ResolvedRepositoryRevision,
    observeRawRepositorySnapshotAt,
  )
import Adrai.Scope (scopePatternText)
import Adrai.Sqlite
  ( ColdDatabaseError,
    ColdDatabaseStats,
    SearchStorageError,
    replaceSearchMaterialization,
    writeColdDatabase,
  )
import Adrai.Types
  ( ConnectionId,
    Digest,
    RecordId,
    ActorKind (..),
    adrIdText,
    actorId,
    actorKind,
    actorModel,
    connectionIdText,
    digestBytes,
    objectRefText,
    operationIdText,
    provenanceContextDigest,
    provenanceInputDigest,
    provenancePromptDigest,
    recordIdText,
    repoPathText,
    stateTokenText,
  )
import Adrai.Vector (identifierTerms)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Numeric (showFFloat)
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

data ColdCompilerError
  = ColdCompilerRepositoryError RepositorySnapshotError
  | ColdCompilerSearchError SearchMaterializationError
  | ColdCompilerDatabaseError ColdDatabaseError
  deriving (Eq, Show)

data ColdCompilerResult = ColdCompilerResult
  { coldCompilerDiagnostics :: [CompilerDiagnostic],
    coldCompilerCompiledRevision :: ResolvedRepositoryRevision,
    coldCompilerSearchMaterialization :: Maybe SearchMaterialization,
    coldCompilerMaterializationFingerprint :: Digest,
    coldCompilerDatabaseStats :: ColdDatabaseStats
  }
  deriving (Eq, Show)

coldCompileRepository :: Connection -> ResolvedRepositoryRevision -> IO (Either ColdCompilerError ColdCompilerResult)
coldCompileRepository connection revision = do
  rawResult <- observeRawRepositorySnapshotAt revision
  case rawResult of
    Left problem -> pure (Left (ColdCompilerRepositoryError problem))
    Right raw -> do
      analyzedResult <- analyzeRepositorySnapshot raw
      case analyzedResult of
        Left problem -> pure (Left (ColdCompilerRepositoryError problem))
        Right analyzed ->
          case gateAnalyzedRepositorySnapshot analyzed of
            Left _ -> store analyzed Nothing
            Right parsed ->
              case materializeParsedReducedSearch parsed of
                Left problem -> pure (Left (ColdCompilerSearchError problem))
                Right materialization -> store analyzed (Just materialization)
  where
    store analyzed materialization = do
      let fingerprint = coldMaterializationFingerprint analyzed materialization
      stored <- writeColdDatabase connection analyzed materialization fingerprint
      pure $ do
        stats <- mapLeft ColdCompilerDatabaseError stored
        Right
          ColdCompilerResult
            { coldCompilerDiagnostics = analyzedDiagnostics analyzed,
              coldCompilerCompiledRevision = revision,
              coldCompilerSearchMaterialization = materialization,
              coldCompilerMaterializationFingerprint = fingerprint,
              coldCompilerDatabaseStats = stats
            }

coldMaterializationFingerprint :: AnalyzedRepositorySnapshot -> Maybe SearchMaterialization -> Digest
coldMaterializationFingerprint analyzed materialization =
  -- Incremental SHA-256 over the same framed sequence as before (no
  -- corpus-sized BS.concat transient): identical persisted fingerprint bytes.
  sha256DigestFrames
    ( "adrai-cold-materialization/1\NUL"
        : framedText "adrai-cache/1"
        : framedText materializationImplementationFingerprint
        : framedBytes (digestBytes (analyzedSourceFingerprint analyzed))
        : map (framedText . diagnosticFingerprint) (analyzedDiagnostics analyzed)
          <> map (framedText . conflictFingerprint) (analyzedConflicts analyzed)
          <> map (framedText . operationDocumentFingerprint) (sortOn operationDocumentKey (analyzedDocuments analyzed))
          <> map (framedText . reducedFingerprint) (sortOn reducedAdrId (graphReductionAdrs (analyzedReduction analyzed)))
          <> maybe [] searchFingerprint materialization
    )

diagnosticFingerprint :: CompilerDiagnostic -> Text
diagnosticFingerprint problem =
  Text.intercalate
    "\NUL"
    [ compilerDiagnosticCodeText (compilerDiagnosticCode problem),
      diagnosticSeverityFingerprint (compilerDiagnosticSeverity problem),
      diagnosticOriginFingerprint (compilerDiagnosticOrigin problem),
      maybe "" adrIdText (compilerDiagnosticAdr problem),
      maybe "" objectRefText (compilerDiagnosticObject problem),
      maybe "" operationIdText (compilerDiagnosticOperation problem),
      maybe "" gitOidText (compilerDiagnosticCommit problem),
      maybe "" repoPathText (compilerDiagnosticPath problem),
      compilerDiagnosticMessage problem
    ]

operationDocumentKey :: ParsedManagedDocument -> (Text, Text, Text)
operationDocumentKey document =
  ( operationIdText (provenanceOperationId capsule),
    provenanceObjectIdText (provenanceObjectId capsule),
    repoPathText (parsedManagedPath document)
  )
  where
    capsule = parsedManagedCapsule document

operationDocumentFingerprint :: ParsedManagedDocument -> Text
operationDocumentFingerprint document =
  Text.intercalate
    "\NUL"
    [ operationIdText (provenanceOperationId capsule),
      provenanceObjectIdText (provenanceObjectId capsule),
      eventKindText (provenanceEventKind capsule),
      decimal (provenanceTimestampMs capsule),
      actorKindFingerprint (actorKind actor),
      actorId actor,
      maybe "" id (actorModel actor),
      gitOidText (provenanceBasis capsule),
      Text.intercalate "\n" (map provenanceObjectIdText (provenanceParents capsule)),
      maybe "" id (provenanceBranchHint capsule),
      maybe "" id (provenanceUpstreamHint capsule),
      Text.intercalate "\n" [lineAnchorId anchor <> "@" <> gitOidText (lineAnchorCommit anchor) | anchor <- provenanceLineAnchors capsule],
      renderDigest (provenanceSemanticDigest capsule),
      provenanceToolVersion capsule,
      maybe "" renderDigest (provenanceInputDigest inputs),
      maybe "" renderDigest (provenancePromptDigest inputs),
      maybe "" renderDigest (provenanceContextDigest inputs),
      repoPathText (parsedManagedPath document)
    ]
  where
    capsule = parsedManagedCapsule document
    actor = provenanceActor capsule
    inputs = provenanceInputs capsule

actorKindFingerprint :: ActorKind -> Text
actorKindFingerprint kind =
  case kind of
    HumanActor -> "human"
    LlmActor -> "llm"
    ServiceActor -> "service"

conflictFingerprint :: AdrConflict -> Text
conflictFingerprint conflict =
  Text.intercalate
    "\NUL"
    [ adrConflictCode conflict,
      adrIdText (adrConflictAdr conflict),
      decimal (adrConflictCount conflict),
      stateTokenText (adrConflictStateToken conflict),
      Text.intercalate "\n" (adrConflictSummaries conflict)
    ]

reducedFingerprint :: ReducedAdr -> Text
reducedFingerprint reduced =
  Text.intercalate
    "\NUL"
    [ adrIdText (reducedAdrId reduced),
      stateTokenText (reducedStateToken reduced),
      Text.intercalate "," (map recordIdText (axisResolutionHeads (reducedDecisionAxis reduced))),
      Text.intercalate "," (map connectionIdText (axisResolutionHeads (reducedScopeAxis reduced))),
      Text.intercalate "," (map connectionIdText (axisResolutionHeads (reducedDomainAxis reduced))),
      Text.intercalate "," (map connectionIdText (axisResolutionHeads (reducedStatusAxis reduced))),
      Text.intercalate "," (map (connectionIdText . currentConnectionId) (reducedCurrentConnections reduced))
    ]

searchFingerprint :: SearchMaterialization -> [ByteString]
searchFingerprint materialization =
  map (framedText . searchDocumentFingerprint) (sortOn searchDocumentItemId (searchMaterializationDocuments materialization))
    <> map (framedText . searchPassageFingerprint) (sortOn searchPassageId (searchMaterializationPassages materialization))
    <> map (framedText . uncurry (\alias expansion -> alias <> "\NUL" <> expansion)) (sortOn fst (searchMaterializationAliases materialization))

searchDocumentFingerprint :: SearchDocument -> Text
searchDocumentFingerprint document =
  Text.intercalate
    "\NUL"
    [ searchDocumentItemId document,
      adrIdText (searchDocumentAdrId document),
      recordIdText (searchDocumentCandidateRecordId document),
      searchDocumentTitle document,
      searchDocumentSummary document,
      searchDocumentContext document,
      searchDocumentDecision document,
      searchDocumentConsequences document,
      Text.intercalate "\n" (searchDocumentDomains document),
      searchDocumentRationale document,
      searchDocumentIdentifiers document,
      searchDocumentOther document,
      Text.intercalate "\n" (searchDocumentScope document),
      Text.intercalate "\n" (searchDocumentSourcePaths document),
      boolText (searchDocumentObsolete document),
      boolText (searchDocumentConflicted document),
      stateTokenText (searchDocumentStateToken document)
    ]

searchPassageFingerprint :: SearchPassage -> Text
searchPassageFingerprint passage =
  Text.intercalate
    "\NUL"
    [ searchPassageId passage,
      searchPassageDocumentItemId passage,
      adrIdText (searchPassageAdrId passage),
      recordIdText (searchPassageCandidateRecordId passage),
      sectionKindName (searchPassageSectionKind passage),
      decimal (searchPassageOrdinal passage),
      decimal (searchPassageLineStart passage),
      decimal (searchPassageLineEnd passage),
      searchPassageText passage,
      Text.pack (showFFloat (Just 6) (searchPassageWeight passage) ""),
      Text.intercalate "\n" (searchPassageSourcePaths passage),
      searchPassageIdentifiers passage
    ]

framedText :: Text -> ByteString
framedText = framedBytes . TextEncoding.encodeUtf8

framedBytes :: ByteString -> ByteString
framedBytes bytes = TextEncoding.encodeUtf8 (decimal (BS.length bytes) <> ":") <> bytes <> "\NUL"

diagnosticSeverityFingerprint :: CompilerDiagnosticSeverity -> Text
diagnosticSeverityFingerprint CompilerDiagnosticError = "error"
diagnosticSeverityFingerprint CompilerDiagnosticWarning = "warning"

diagnosticOriginFingerprint :: CompilerDiagnosticOrigin -> Text
diagnosticOriginFingerprint origin =
  case origin of
    CompilerConfigOrigin -> "config"
    CompilerPathDocumentOrigin -> "path_document"
    CompilerHistoryOrigin -> "history"
    CompilerOperationOrigin -> "operation"
    CompilerGraphOrigin -> "graph"
    CompilerBasisOrigin -> "basis"

boolText :: Bool -> Text
boolText True = "true"
boolText False = "false"

decimal :: (Show value) => value -> Text
decimal = Text.pack . show

materializeCurrentSearch :: ReadSnapshot -> Either SearchMaterializationError SearchMaterialization
materializeCurrentSearch snapshot = do
  mapLeft SearchMaterializationSnapshotError (validateReadSnapshot snapshot)
  materializeReducedSearch (readSnapshotDocuments snapshot) (readSnapshotReduction snapshot)

materializeParsedReducedSearch :: ParsedReducedRepositorySnapshot -> Either SearchMaterializationError SearchMaterialization
materializeParsedReducedSearch snapshot =
  materializeReducedSearch (parsedReducedDocuments snapshot) (parsedReducedReduction snapshot)

materializeReducedSearch :: [ParsedManagedDocument] -> GraphReduction -> Either SearchMaterializationError SearchMaterialization
materializeReducedSearch sourceDocuments reduction = do
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
    indexes = buildIndexes sourceDocuments
    reducedAdrs = sortOn reducedAdrId (graphReductionAdrs reduction)
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
