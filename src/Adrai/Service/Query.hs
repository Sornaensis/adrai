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
    runShow,
    runCompare,
    runHistory,
    showFailureText,
    showFailureIsConflict,
    compareFailureText,
    historyFailureText,
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
import qualified Adrai.Format.Document as Document
import Adrai.Git (Repository, RevisionSpec (..), gitOidText, gitTreeOid, gitTreePath)
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
    resolvedCommitOid,
  )
import Adrai.Types (ViewMode (..))
import Data.Text (Text)
import qualified Data.Text as Text

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

readSnapshotAt :: Repository -> Text -> IO (Either SnapshotReadFailure ReadSnapshot)
readSnapshotAt repository requestedRevision = do
  revisionResult <- resolveRepositoryRevision repository (RevisionSpec requestedRevision)
  case revisionResult of
    Left problem -> pure (Left (SnapshotRepositoryFailure (Text.pack (show problem))))
    Right revision -> do
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
