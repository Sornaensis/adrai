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
    runShow,
    showFailureText,
    showFailureIsConflict,
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
  ( ExplodedOptions (..),
    ExplodedProjection (..),
    QueryError,
    ReferenceLookupError (..),
    CollapsedProjection (..),
    ProjectionMode (CompactProjection),
    projectCollapsed,
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

-- | Materialise exactly the requested immutable revision and project one ADR.
runShow :: Repository -> ShowRequest -> IO (Either ShowFailure ShowResult)
runShow repository request
  | showRequestRaw request && showRequestView request /= ExplodedView =
      pure (Left ShowRawRequiresExploded)
  | otherwise = do
      revisionResult <- resolveRepositoryRevision repository (RevisionSpec (showRequestRevision request))
      case revisionResult of
        Left problem -> pure (Left (ShowRepositoryFailure (Text.pack (show problem))))
        Right revision -> do
          rawResult <- observeRawRepositorySnapshotAt revision
          case rawResult of
            Left problem -> pure (Left (ShowRepositoryFailure (Text.pack (show problem))))
            Right raw -> do
              analysisResult <- analyzeRepositorySnapshot raw
              case analysisResult of
                Left problem -> pure (Left (ShowRepositoryFailure (Text.pack (show problem))))
                Right analysis ->
                  case gateAnalyzedRepositorySnapshot analysis of
                    Left diagnostics ->
                      pure
                        ( Left
                            ( ShowIntegrityFailure
                                [ compilerDiagnosticCodeText (compilerDiagnosticCode diagnostic)
                                    <> ": " <> compilerDiagnosticMessage diagnostic
                                  | diagnostic <- diagnostics
                                ]
                            )
                        )
                    Right parsed -> do
                      let analyzed = parsedReducedAnalyzed parsed
                      case rawRepositoryConfigResult (rawRepositorySnapshotConfig raw) of
                        Left problem -> pure (Left (ShowRepositoryFailure (Text.pack (show problem))))
                        Right config -> do
                          case classificationDocuments raw (analyzedDocuments analyzed) of
                            Left problem -> pure (Left problem)
                            Right provenanceDocuments -> do
                              placementResult <- hydratePlacementEvidenceAt repository revision config provenanceDocuments
                              pure $ case placementResult of
                                Left problem -> Left (ShowPlacementFailure problem)
                                Right placements ->
                                  project
                                    ( ReadSnapshot
                                        (RevisionIdentity (showRequestRevision request) (gitOidText (resolvedCommitOid revision)))
                                        (analyzedDocuments analyzed)
                                        (analyzedReduction analyzed)
                                        placements
                                    )
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
        [] -> Left (ShowRepositoryFailure ("managed snapshot entry is missing for " <> Text.pack (show (Document.parsedManagedPath document))))
        _ -> Left (ShowRepositoryFailure ("managed snapshot entry is duplicated for " <> Text.pack (show (Document.parsedManagedPath document))))

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
