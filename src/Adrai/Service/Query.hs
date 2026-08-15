{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Revision-local materialisation for the public @show@ command.
--
-- The service observes one resolved Git commit, gates source-integrity
-- failures, hydrates the established recoverable provenance cache, then hands
-- one coherent snapshot to the pure projections in 'Adrai.Query'.  Cache
-- maintenance never mutates Git, the caller's index, or the worktree.
module Adrai.Service.Query
  ( ShowRequest (..),
    ShowResult (..),
    ShowFailure (..),
    CompareRequest (..),
    CompareFailure (..),
    HistoryRequest (..),
    HistoryFailure (..),
    SearchServiceRequest (..),
    SearchFailure (..),
    RelevantFailure (..),
    runShow,
    runCompare,
    runHistory,
    runSearch,
    runRelevantQuery,
    showFailureText,
    showFailureIsConflict,
    compareFailureText,
    historyFailureText,
    searchFailureText,
    searchFailureIsConflict,
    relevantFailureText,
  )
where

import Adrai.Compiler.Snapshot
  ( analyzeRepositorySnapshot,
    analyzedDocuments,
    analyzedReduction,
    compilerDiagnosticCode,
    compilerDiagnosticCodeText,
    compilerDiagnosticMessage,
    gateAnalyzedRepositorySnapshot,
    parsedReducedAnalyzed,
  )
import Adrai.Compiler
  ( ColdCompilerResult (..),
    coldCompileRepository,
  )
import qualified Adrai.Format.Document as Document
import Adrai.Git (GitBlob (..), Repository, RevisionSpec (..), gitOidText, gitTreeOid, gitTreePath, readRegularBlobAt, readWorktreeFileBytes)
import Adrai.History (ReadSnapshot (..), RevisionIdentity (..))
import Adrai.History
  ( HistoryError (..),
    HistoryOptions,
    HistoryProjection,
    projectHistory,
  )
import Adrai.Provenance
  ( provenanceObjectId,
    provenanceObjectIdText,
    semanticDigest,
  )
import qualified Adrai.Provenance.Classification as Classification
import Adrai.Provenance.Read
  ( PlacementHydrationError,
    hydratePlacementEvidenceAt,
  )
import Adrai.Query
  ( CompareOptions (..),
    CompareProjection,
    ExplodedOptions (..),
    ExplodedProjection (..),
    QueryError,
    ReferenceLookupError (..),
    CollapsedProjection (..),
    ProjectionMode (CompactProjection),
    projectCollapsed,
    compareSnapshots,
    projectExploded,
    resolutionStateConflicts,
    resolutionStateRequired,
    resolutionSummary,
    SearchError,
    SearchProjection (..),
    SearchRequest,
    SearchResult (..),
    RelevantError,
    RelevantProjection,
    RelevantRequest (..),
    RelevantSource (..),
    runCurrentSearch,
    runRelevant,
    referenceLookupErrorText,
    resolveAdrReference,
  )
import Adrai.Repository
  ( observeRawRepositorySnapshotAt,
    rawRepositoryConfigResult,
    rawRepositorySnapshotConfig,
    rawRepositorySnapshotEntries,
    repositoryTreeEntry,
    resolveRepositoryRevision,
    ResolvedRepositoryRevision,
    resolvedCommitOid,
  )
import Adrai.Types (RevisionSelector (..), ViewMode (..))
import Control.Exception (SomeAsyncException, SomeException, bracket, displayException, fromException, throwIO, try)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple (close, open)

data ShowRequest = ShowRequest
  { showRequestReference :: Text,
    showRequestView :: ViewMode,
    showRequestRevision :: Text,
    showRequestRaw :: Bool
  }
  deriving (Eq, Show)

data ShowResult
  = ShowCollapsed CollapsedProjection
  | ShowExploded ExplodedProjection
  deriving (Eq, Show)

data ShowFailure
  = ShowRepositoryFailure Text
  | ShowIntegrityFailure [Text]
  | ShowReferenceFailure ReferenceLookupError
  | ShowProjectionFailure QueryError
  | ShowPlacementFailure PlacementHydrationError
  | ShowSemanticConflict [Text]
  | ShowRawRequiresExploded
  deriving (Eq, Show)

data CompareRequest = CompareRequest
  { compareRequestFrom :: Text,
    compareRequestTo :: Text,
    compareRequestIncludeUnchanged :: Bool
  }
  deriving (Eq, Show)

data CompareFailure
  = CompareRepositoryFailure Text
  | CompareIntegrityFailure [Text]
  | ComparePlacementFailure PlacementHydrationError
  | CompareProjectionFailure QueryError
  deriving (Eq, Show)

data HistoryRequest = HistoryRequest
  { historyRequestReference :: Maybe Text,
    historyRequestRevision :: Text,
    historyRequestOptions :: HistoryOptions
  }
  deriving (Eq, Show)

data HistoryFailure
  = HistoryRepositoryFailure Text
  | HistoryIntegrityFailure [Text]
  | HistoryPlacementFailure PlacementHydrationError
  | HistoryReferenceFailure ReferenceLookupError
  | HistoryProjectionFailure HistoryError
  deriving (Eq, Show)

data SearchServiceRequest = SearchServiceRequest
  { searchServiceRevision :: Text,
    searchServiceQuery :: SearchRequest
  }
  deriving (Eq, Show)

data SearchFailure
  = SearchRepositoryFailure Text
  | SearchIntegrityFailure [Text]
  | SearchPlacementFailure PlacementHydrationError
  | SearchCompilerFailure Text
  | SearchQueryFailure SearchError
  | SearchSemanticConflict [Text]
  deriving (Eq, Show)

data RelevantFailure
  = RelevantRepositoryFailure Text
  | RelevantIntegrityFailure [Text]
  | RelevantPlacementFailure PlacementHydrationError
  | RelevantCompilerFailure Text
  | RelevantSourceFailure Text
  | RelevantQueryFailure RelevantError
  deriving (Eq, Show)

data SnapshotReadFailure
  = SnapshotRepositoryFailure Text
  | SnapshotIntegrityFailure [Text]
  | SnapshotPlacementFailure PlacementHydrationError
  deriving (Eq, Show)

-- | Materialise exactly the requested immutable revision and project one ADR.
runShow :: Repository -> ShowRequest -> IO (Either ShowFailure ShowResult)
runShow repository request
  | showRequestRaw request && showRequestView request /= ExplodedView =
      pure (Left ShowRawRequiresExploded)
  | otherwise = do
      snapshotResult <- readSnapshotAt repository (showRequestRevision request)
      pure $ case snapshotResult of
        Left failure -> Left (showSnapshotFailure failure)
        Right snapshot -> project snapshot
  where
    project snapshot = do
      adr <- either (Left . ShowReferenceFailure) Right (resolveAdrReference snapshot (showRequestReference request))
      case showRequestView request of
        CollapsedView -> ensureResolved . ShowCollapsed =<< either (Left . ShowProjectionFailure) Right (projectCollapsed CompactProjection snapshot adr)
        ExplodedView -> ensureResolved . ShowExploded =<< either (Left . ShowProjectionFailure) Right (projectExploded (ExplodedOptions (showRequestRaw request)) snapshot adr)

    ensureResolved result
      | resolutionStateRequired state = Left (ShowSemanticConflict (map resolutionSummary (resolutionStateConflicts state)))
      | otherwise = Right result
      where
        state = case result of
          ShowCollapsed projection -> collapsedResolution projection
          ShowExploded projection -> explodedResolution projection

runCompare :: Repository -> CompareRequest -> IO (Either CompareFailure CompareProjection)
runCompare repository request = do
  beforeResult <- readSnapshotAt repository (compareRequestFrom request)
  case beforeResult of
    Left failure -> pure (Left (compareSnapshotFailure failure))
    Right beforeSnapshot -> do
      afterResult <- readSnapshotAt repository (compareRequestTo request)
      pure $ case afterResult of
        Left failure -> Left (compareSnapshotFailure failure)
        Right afterSnapshot ->
          either
            (Left . CompareProjectionFailure)
            Right
            ( compareSnapshots
                CompareOptions
                  { compareIncludeUnchanged = compareRequestIncludeUnchanged request,
                    compareCacheMetadata = []
                  }
                beforeSnapshot
                afterSnapshot
            )

runHistory :: Repository -> HistoryRequest -> IO (Either HistoryFailure HistoryProjection)
runHistory repository request = do
  snapshotResult <- readSnapshotAt repository (historyRequestRevision request)
  pure $ case snapshotResult of
    Left failure -> Left (historySnapshotFailure failure)
    Right snapshot -> do
      requestedAdr <-
        case historyRequestReference request of
          Nothing -> Right Nothing
          Just reference -> Just <$> either (Left . HistoryReferenceFailure) Right (resolveAdrReference snapshot reference)
      either (Left . HistoryProjectionFailure) Right (projectHistory snapshot requestedAdr (historyRequestOptions request))

runSearch :: Repository -> SearchServiceRequest -> IO (Either SearchFailure SearchProjection)
runSearch repository request = do
  revisionResult <- resolveRepositoryRevision repository (RevisionSpec (searchServiceRevision request))
  case revisionResult of
    Left problem -> pure (Left (SearchRepositoryFailure (Text.pack (show problem))))
    Right revision -> do
      snapshotResult <- readSnapshotAtResolved repository (searchServiceRevision request) revision
      case snapshotResult of
        Left failure -> pure (Left (searchSnapshotFailure failure))
        Right snapshot -> do
          captured <- try (bracket (open ":memory:") close (compileAndSearch revision snapshot)) :: IO (Either SomeException (Either SearchFailure SearchProjection))
          case captured of
            Left exception ->
              case fromException exception of
                Just cancellation -> throwIO (cancellation :: SomeAsyncException)
                Nothing -> pure (Left (SearchCompilerFailure (Text.pack (displayException exception))))
            Right result -> pure result
  where
    compileAndSearch revision snapshot connection = do
      compiledResult <- coldCompileRepository connection revision
      case compiledResult of
        Left problem -> pure (Left (SearchCompilerFailure (Text.pack (show problem))))
        Right compiled ->
          case coldCompilerSearchMaterialization compiled of
            Nothing -> pure (Left (SearchCompilerFailure "compiler produced no search materialization for an integrity-gated snapshot"))
            Just materialization -> do
              searched <- runCurrentSearch connection snapshot materialization (searchServiceQuery request)
              pure $ case searched of
                Left problem -> Left (SearchQueryFailure problem)
                Right projection
                  | null conflicts -> Right projection
                  | otherwise -> Left (SearchSemanticConflict conflicts)
                  where
                    conflicts =
                      [ resolutionSummary conflict
                        | result <- searchProjectionResults projection,
                          conflict <- resolutionStateConflicts (searchResultResolution result)
                      ]

-- | Rank ADR relevance against one immutable compiled context and exactly one
-- caller-selected source.  Revision sources are read from the resolved tree;
-- worktree sources are explicit and retain the resolved HEAD only as context.
runRelevantQuery :: Repository -> RelevantRequest -> IO (Either RelevantFailure RelevantProjection)
runRelevantQuery repository request = do
  let requestedRevision =
        case relevantRequestRevision request of
          AtRevision revision -> revision
          WorkingRevision -> "HEAD"
  revisionResult <- resolveRepositoryRevision repository (RevisionSpec requestedRevision)
  case revisionResult of
    Left problem -> pure (Left (RelevantRepositoryFailure (Text.pack (show problem))))
    Right revision -> do
      snapshotResult <- readSnapshotAtResolved repository requestedRevision revision
      case snapshotResult of
        Left failure -> pure (Left (relevantSnapshotFailure failure))
        Right snapshot -> do
          sourceResult <- readSource revision
          case sourceResult of
            Left problem -> pure (Left problem)
            Right source -> do
              captured <- try (bracket (open ":memory:") close (compileAndRank revision snapshot source)) :: IO (Either SomeException (Either RelevantFailure RelevantProjection))
              case captured of
                Left exception ->
                  case fromException exception of
                    Just cancellation -> throwIO (cancellation :: SomeAsyncException)
                    Nothing -> pure (Left (RelevantCompilerFailure (Text.pack (displayException exception))))
                Right result -> pure result
  where
    readSource revision =
      case relevantRequestRevision request of
        AtRevision _ -> do
          blobResult <- readRegularBlobAt repository (resolvedCommitOid revision) (relevantRequestFile request)
          pure $ case blobResult of
            Left problem -> Left (RelevantSourceFailure (Text.pack (show problem)))
            Right blob ->
              Right
                RevisionRelevantSource
                  { relevantSourcePath = relevantRequestFile request,
                    relevantSourceResolvedRevision = gitOidText (resolvedCommitOid revision),
                    relevantSourceBlob = gitOidText (gitBlobOid blob),
                    relevantSourceBytes = gitBlobBytes blob
                  }
        WorkingRevision -> do
          worktreeResult <- readWorktreeFileBytes repository (relevantRequestFile request)
          pure $ case worktreeResult of
            Left problem -> Left (RelevantSourceFailure (Text.pack (show problem)))
            Right (_, bytes) ->
              Right
                WorktreeRelevantSource
                  { relevantSourcePath = relevantRequestFile request,
                    relevantSourceHeadRevision = gitOidText (resolvedCommitOid revision),
                    relevantSourceBytes = bytes
                  }

    compileAndRank revision snapshot source connection = do
      compiledResult <- coldCompileRepository connection revision
      case compiledResult of
        Left problem -> pure (Left (RelevantCompilerFailure (Text.pack (show problem))))
        Right compiled ->
          case coldCompilerSearchMaterialization compiled of
            Nothing -> pure (Left (RelevantCompilerFailure "compiler produced no search materialization for an integrity-gated snapshot"))
            Just materialization -> do
              ranked <- runRelevant connection snapshot materialization request source
              pure (either (Left . RelevantQueryFailure) Right ranked)

readSnapshotAt :: Repository -> Text -> IO (Either SnapshotReadFailure ReadSnapshot)
readSnapshotAt repository requestedRevision = do
  revisionResult <- resolveRepositoryRevision repository (RevisionSpec requestedRevision)
  case revisionResult of
    Left problem -> pure (Left (SnapshotRepositoryFailure (Text.pack (show problem))))
    Right revision -> readSnapshotAtResolved repository requestedRevision revision

readSnapshotAtResolved :: Repository -> Text -> ResolvedRepositoryRevision -> IO (Either SnapshotReadFailure ReadSnapshot)
readSnapshotAtResolved repository requestedRevision revision = do
      rawResult <- observeRawRepositorySnapshotAt revision
      case rawResult of
        Left problem -> pure (Left (SnapshotRepositoryFailure (Text.pack (show problem))))
        Right raw -> do
          analysisResult <- analyzeRepositorySnapshot raw
          case analysisResult of
            Left problem -> pure (Left (SnapshotRepositoryFailure (Text.pack (show problem))))
            Right analysis ->
              case gateAnalyzedRepositorySnapshot analysis of
                Left diagnostics ->
                  pure
                    ( Left
                        ( SnapshotIntegrityFailure
                            [ compilerDiagnosticCodeText (compilerDiagnosticCode diagnostic)
                                <> ": " <> compilerDiagnosticMessage diagnostic
                              | diagnostic <- diagnostics
                            ]
                        )
                    )
                Right parsed -> do
                  let analyzed = parsedReducedAnalyzed parsed
                  case rawRepositoryConfigResult (rawRepositorySnapshotConfig raw) of
                    Left problem -> pure (Left (SnapshotRepositoryFailure (Text.pack (show problem))))
                    Right config ->
                      case classificationDocuments raw (analyzedDocuments analyzed) of
                        Left problem -> pure (Left problem)
                        Right provenanceDocuments -> do
                          placementResult <- hydratePlacementEvidenceAt repository revision config provenanceDocuments
                          pure $ case placementResult of
                            Left problem -> Left (SnapshotPlacementFailure problem)
                            Right placements ->
                              Right
                                ( ReadSnapshot
                                    (RevisionIdentity requestedRevision (gitOidText (resolvedCommitOid revision)))
                                    (analyzedDocuments analyzed)
                                    (analyzedReduction analyzed)
                                    placements
                                )

showSnapshotFailure :: SnapshotReadFailure -> ShowFailure
showSnapshotFailure failure =
  case failure of
    SnapshotRepositoryFailure message -> ShowRepositoryFailure message
    SnapshotIntegrityFailure diagnostics -> ShowIntegrityFailure diagnostics
    SnapshotPlacementFailure problem -> ShowPlacementFailure problem

compareSnapshotFailure :: SnapshotReadFailure -> CompareFailure
compareSnapshotFailure failure =
  case failure of
    SnapshotRepositoryFailure message -> CompareRepositoryFailure message
    SnapshotIntegrityFailure diagnostics -> CompareIntegrityFailure diagnostics
    SnapshotPlacementFailure problem -> ComparePlacementFailure problem

historySnapshotFailure :: SnapshotReadFailure -> HistoryFailure
historySnapshotFailure failure =
  case failure of
    SnapshotRepositoryFailure message -> HistoryRepositoryFailure message
    SnapshotIntegrityFailure diagnostics -> HistoryIntegrityFailure diagnostics
    SnapshotPlacementFailure problem -> HistoryPlacementFailure problem

searchSnapshotFailure :: SnapshotReadFailure -> SearchFailure
searchSnapshotFailure failure =
  case failure of
    SnapshotRepositoryFailure message -> SearchRepositoryFailure message
    SnapshotIntegrityFailure diagnostics -> SearchIntegrityFailure diagnostics
    SnapshotPlacementFailure problem -> SearchPlacementFailure problem

relevantSnapshotFailure :: SnapshotReadFailure -> RelevantFailure
relevantSnapshotFailure failure =
  case failure of
    SnapshotRepositoryFailure message -> RelevantRepositoryFailure message
    SnapshotIntegrityFailure diagnostics -> RelevantIntegrityFailure diagnostics
    SnapshotPlacementFailure problem -> RelevantPlacementFailure problem

classificationDocuments raw = traverse classify
  where
    entries = map repositoryTreeEntry (rawRepositorySnapshotEntries raw)
    classify document =
      case filter ((== Document.parsedManagedPath document) . gitTreePath) entries of
        [entry] ->
          Right
            Classification.ParsedManagedDocument
              { Classification.parsedDocumentObjectRef =
                  provenanceObjectIdText (provenanceObjectId (Document.parsedManagedCapsule document)),
                Classification.parsedManagedPath = Document.parsedManagedPath document,
                Classification.parsedManagedCapsule = Document.parsedManagedCapsule document,
                Classification.parsedBlobOid = Just (gitTreeOid entry),
                Classification.parsedSemanticHash = Text.pack (show (semanticDigest (Document.parsedManagedSemantic document)))
              }
        [] -> Left (SnapshotRepositoryFailure ("managed snapshot entry is missing for " <> Text.pack (show (Document.parsedManagedPath document))))
        _ -> Left (SnapshotRepositoryFailure ("managed snapshot entry is duplicated for " <> Text.pack (show (Document.parsedManagedPath document))))

showFailureText :: ShowFailure -> Text
showFailureText failure =
  case failure of
    ShowRepositoryFailure message -> message
    ShowIntegrityFailure diagnostics -> "repository integrity failure: " <> Text.intercalate "; " diagnostics
    ShowReferenceFailure problem -> referenceLookupErrorText problem
    ShowProjectionFailure problem -> Text.pack (show problem)
    ShowPlacementFailure problem -> "provenance hydration failure: " <> Text.pack (show problem)
    ShowSemanticConflict summaries -> "ADR requires resolution: " <> Text.intercalate "; " summaries
    ShowRawRequiresExploded -> "--raw requires --view exploded"

-- | Reference ambiguity is a user selector error (exit 2); unresolved semantic
-- heads are the distinct conflict outcome (exit 3).
showFailureIsConflict :: ShowFailure -> Bool
showFailureIsConflict failure =
  case failure of
    ShowSemanticConflict _ -> True
    _ -> False

compareFailureText :: CompareFailure -> Text
compareFailureText failure =
  case failure of
    CompareRepositoryFailure message -> message
    CompareIntegrityFailure diagnostics -> "repository integrity failure: " <> Text.intercalate "; " diagnostics
    ComparePlacementFailure problem -> "provenance hydration failure: " <> Text.pack (show problem)
    CompareProjectionFailure problem -> Text.pack (show problem)

historyFailureText :: HistoryFailure -> Text
historyFailureText failure =
  case failure of
    HistoryRepositoryFailure message -> message
    HistoryIntegrityFailure diagnostics -> "repository integrity failure: " <> Text.intercalate "; " diagnostics
    HistoryPlacementFailure problem -> "provenance hydration failure: " <> Text.pack (show problem)
    HistoryReferenceFailure problem -> referenceLookupErrorText problem
    HistoryProjectionFailure (HistoryInvalidLimit limit) ->
      "history limit must be between 1 and 1000: " <> Text.pack (show limit)
    HistoryProjectionFailure problem -> Text.pack (show problem)

searchFailureText :: SearchFailure -> Text
searchFailureText failure =
  case failure of
    SearchRepositoryFailure message -> message
    SearchIntegrityFailure diagnostics -> "repository integrity failure: " <> Text.intercalate "; " diagnostics
    SearchPlacementFailure problem -> "provenance hydration failure: " <> Text.pack (show problem)
    SearchCompilerFailure message -> "search materialization failure: " <> message
    SearchQueryFailure problem -> Text.pack (show problem)
    SearchSemanticConflict summaries -> "search results require resolution: " <> Text.intercalate "; " summaries

searchFailureIsConflict :: SearchFailure -> Bool
searchFailureIsConflict (SearchSemanticConflict _) = True
searchFailureIsConflict _ = False

relevantFailureText :: RelevantFailure -> Text
relevantFailureText failure =
  case failure of
    RelevantRepositoryFailure message -> message
    RelevantIntegrityFailure diagnostics -> "repository integrity failure: " <> Text.intercalate "; " diagnostics
    RelevantPlacementFailure problem -> "provenance hydration failure: " <> Text.pack (show problem)
    RelevantCompilerFailure message -> "relevance materialization failure: " <> message
    RelevantSourceFailure message -> "relevance source failure: " <> message
    RelevantQueryFailure problem -> Text.pack (show problem)
