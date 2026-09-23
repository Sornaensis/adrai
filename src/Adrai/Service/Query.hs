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
    QueryExecutionHooks (..),
    ExactQueryContext (..),
    runShow,
    runWebShow,
    runCompare,
    runHistory,
    runSearch,
    runSearchExact,
    runWebSearchExact,
    runSearchWithHooks,
    runRelevantQuery,
    runRelevantQueryExact,
    runRelevantQueryAtHead,
    runRelevantQueryWithHooks,
    readSnapshotAt,
    loadExactQueryContextForTest,
    loadExactQueryContextWithAcquisitionHooksForTest,
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
    materializeCurrentSearch,
  )
import Adrai.Compiler.CacheSelection
  ( ValidatedCacheRows,
    ExactArchiveBusy (..),
    exactCacheArchivePath,
    validatedCoverageRows,
    validatedLandingRows,
    validatedLineConfigRows,
    validatedManagedSourceRows,
    validatedMemberRows,
    validatedOperationCommitRows,
    validatedOperationRows,
    validatedRepositoryConfigRows,
    validatedSearchMaterialization,
    validatedTargetRows,
    withExactArchiveTransaction,
    withValidatedExactCacheTargetConnection,
  )
import qualified Adrai.Format.Document as Document
import Adrai.Format.Config (parseConfigText)
import Adrai.Git (GitBlob (..), GitOid, Repository, RevisionSpec (..), gitOidText, gitTreeOid, gitTreePath, readRegularBlobAt, readWorktreeFileBytes)
import Adrai.History (PlacementEvidence, ReadSnapshot (..), RevisionIdentity (..))
import Adrai.History
  ( HistoryError (..),
    HistoryOptions,
    HistoryProjection,
    projectHistory,
  )
import Adrai.Provenance
  ( encodeBase64Url,
    mkGitOid,
    provenanceObjectId,
    provenanceObjectIdText,
    provenanceOperationId,
    semanticDigest,
  )
import qualified Adrai.Provenance.Classification as Classification
import Adrai.Provenance.Read
  ( PlacementHydrationError,
    hydratePlacementEvidenceAt,
    materialize,
  )
import Adrai.Provenance.Overlay
  ( LineConfigRow (..),
    LineLandingRow (..),
    OperationCommitRow (..),
    ProvenanceEvidence (..),
    ProvenanceOperationEvidence (..),
    RegisteredObjectRow (..),
    RegisteredOperationRow (..),
  )
import Adrai.Provenance.Ensure (configKey, openReadWriteExisting)
import Adrai.Query
  ( CompareOptions (..),
    CompareProjection,
    ExplodedOptions (..),
    ExplodedProjection (..),
    QueryError,
    ReferenceLookupError (..),
    CollapsedProjection (..),
    ProjectionMode (CompactProjection, RichProjection),
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
    RawRepositorySnapshotObservation,
    ResolvedRepositoryRevision,
    resolvedCommitOid,
  )
import Adrai.Retrieval (SearchMaterialization (..), SearchPassage (..))
import Adrai.Graph (reduceManagedGraph)
import Adrai.Types
  ( Config (..),
    OperationId,
    RevisionSelector (..),
    ViewMode (..),
    defaultConfig,
    digestBytes,
    logicalLineId,
    managedConnectionPath,
    managedDecisionPath,
    mkRepoPath,
    operationIdText,
    repoPathText,
  )
import Control.Exception (SomeAsyncException, SomeException, bracket, displayException, fromException, throwIO, try)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Database.SQLite.Simple (Connection, close, execute_, open)

-- | Inert observability seam for the query cache boundary.  Production uses
-- 'defaultQueryExecutionHooks'; tests can prove an exact archive never enters
-- the raw/history/compiler path without changing query semantics.
data QueryExecutionHooks = QueryExecutionHooks
  { queryArchiveLoad :: IO (),
    queryArchiveRejected :: Text -> IO (),
    queryColdFallback :: IO (),
    queryRawObservation :: IO (),
    querySnapshotAnalysis :: IO (),
    queryProvenanceHydration :: IO (),
    queryColdCompile :: IO ()
  }

defaultQueryExecutionHooks :: QueryExecutionHooks
defaultQueryExecutionHooks = QueryExecutionHooks (pure ()) (const (pure ())) (pure ()) (pure ()) (pure ()) (pure ()) (pure ())

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

-- | Keep the validated snapshot and existing projection machinery while
-- exposing unresolved candidate detail for the web inspector.  CLI callers
-- continue to receive their semantic-conflict failure from 'runShow'.
runWebShow :: Repository -> ShowRequest -> IO (Either ShowFailure ShowResult)
runWebShow repository request
  | showRequestRaw request && showRequestView request /= ExplodedView = pure (Left ShowRawRequiresExploded)
  | otherwise = do
      snapshotResult <- readSnapshotAt repository (showRequestRevision request)
      pure $ case snapshotResult of
        Left failure -> Left (showSnapshotFailure failure)
        Right snapshot -> do
          adr <- either (Left . ShowReferenceFailure) Right (resolveAdrReference snapshot (showRequestReference request))
          case showRequestView request of
            CollapsedView -> ShowCollapsed <$> either (Left . ShowProjectionFailure) Right (projectCollapsed RichProjection snapshot adr)
            ExplodedView -> ShowExploded <$> either (Left . ShowProjectionFailure) Right (projectExploded (ExplodedOptions (showRequestRaw request)) snapshot adr)

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
runSearch = runSearchWithHooks defaultQueryExecutionHooks

-- | Execute a search only from the validated immutable archive for the
-- caller-captured commit.  Missing or rejected archives are acquisition
-- failures; this entry point never starts the legacy private in-memory cold
-- compiler.
runSearchExact :: Repository -> GitOid -> SearchServiceRequest -> IO (Either SearchFailure SearchProjection)
runSearchExact repository oid request = do
  revisionResult <- resolveRepositoryRevision repository (RevisionSpec (gitOidText oid))
  case revisionResult of
    Left problem -> pure (Left (SearchRepositoryFailure (Text.pack (show problem))))
    Right revision
      | resolvedCommitOid revision /= oid -> pure (Left (SearchCompilerFailure "exact archive resolved to an unexpected revision"))
      | otherwise -> do
          cached <- withExactCacheContext defaultQueryExecutionHooks repository revision (gitOidText oid) $ \connection context ->
            searchProjectionWith request (exactQuerySnapshot context) connection (exactQueryMaterialization context)
          pure (maybe (Left (SearchCompilerFailure "validated exact archive is unavailable")) id cached)

-- | The web explorer renders valid unresolved candidates instead of treating
-- their semantic conflicts as a failed query; archive validation is identical.
runWebSearchExact :: Repository -> GitOid -> SearchServiceRequest -> IO (Either SearchFailure SearchProjection)
runWebSearchExact repository oid request = do
  revisionResult <- resolveRepositoryRevision repository (RevisionSpec (gitOidText oid))
  case revisionResult of
    Left problem -> pure (Left (SearchRepositoryFailure (Text.pack (show problem))))
    Right revision
      | resolvedCommitOid revision /= oid -> pure (Left (SearchCompilerFailure "exact archive resolved to an unexpected revision"))
      | otherwise -> do
          cached <- withExactCacheContext defaultQueryExecutionHooks repository revision (gitOidText oid) $ \connection context ->
            searchProjectionWithPolicy False request (exactQuerySnapshot context) connection (exactQueryMaterialization context)
          pure (maybe (Left (SearchCompilerFailure "validated exact archive is unavailable")) id cached)

runSearchWithHooks :: QueryExecutionHooks -> Repository -> SearchServiceRequest -> IO (Either SearchFailure SearchProjection)
runSearchWithHooks hooks repository request = do
  revisionResult <- resolveRepositoryRevision repository (RevisionSpec (searchServiceRevision request))
  case revisionResult of
    Left problem -> pure (Left (SearchRepositoryFailure (Text.pack (show problem))))
    Right revision -> do
      cached <- withExactCacheContext hooks repository revision (searchServiceRevision request) $ \connection context ->
        searchProjectionWith request (exactQuerySnapshot context) connection (exactQueryMaterialization context)
      case cached of
        Just result -> pure result
        Nothing -> do
          queryColdFallback hooks
          snapshotResult <- readSnapshotAtResolvedWithHooks hooks repository (searchServiceRevision request) revision
          case snapshotResult of
            Left failure -> pure (Left (searchSnapshotFailure failure))
            Right snapshot -> do
              queryColdCompile hooks
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
            Just materialization -> searchProjectionWith request snapshot connection materialization

searchProjectionWith :: SearchServiceRequest -> ReadSnapshot -> Connection -> SearchMaterialization -> IO (Either SearchFailure SearchProjection)
searchProjectionWith = searchProjectionWithPolicy True

searchProjectionWithPolicy :: Bool -> SearchServiceRequest -> ReadSnapshot -> Connection -> SearchMaterialization -> IO (Either SearchFailure SearchProjection)
searchProjectionWithPolicy rejectConflicts request snapshot connection materialization = do
  searched <- runCurrentSearch connection snapshot materialization (searchServiceQuery request)
  pure $ case searched of
    Left problem -> Left (SearchQueryFailure problem)
    Right projection
      | not rejectConflicts || null conflicts -> Right projection
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
runRelevantQuery = runRelevantQueryWithHooks defaultQueryExecutionHooks

runRelevantQueryWithHooks :: QueryExecutionHooks -> Repository -> RelevantRequest -> IO (Either RelevantFailure RelevantProjection)
runRelevantQueryWithHooks hooks repository request =
  runRelevantQueryAtRequested hooks repository request requestedRevision
  where
    requestedRevision = case relevantRequestRevision request of
      AtRevision revision -> revision
      WorkingRevision -> "HEAD"

-- | Preserve worktree-source semantics while pinning repository context to a
-- caller-captured HEAD commit.
runRelevantQueryAtHead :: Repository -> GitOid -> RelevantRequest -> IO (Either RelevantFailure RelevantProjection)
runRelevantQueryAtHead repository oid request =
  runRelevantQueryAtRequested defaultQueryExecutionHooks repository request (gitOidText oid)

-- | Execute relevance only from the validated immutable archive for the
-- caller-captured commit, while retaining the request's explicit revision or
-- worktree source semantics.
runRelevantQueryExact :: Repository -> GitOid -> RelevantRequest -> IO (Either RelevantFailure RelevantProjection)
runRelevantQueryExact repository oid request = do
  revisionResult <- resolveRepositoryRevision repository (RevisionSpec (gitOidText oid))
  case revisionResult of
    Left problem -> pure (Left (RelevantRepositoryFailure (Text.pack (show problem))))
    Right revision
      | resolvedCommitOid revision /= oid -> pure (Left (RelevantCompilerFailure "exact archive resolved to an unexpected revision"))
      | otherwise -> do
          let requestedRevision = case relevantRequestRevision request of
                AtRevision requested -> requested
                WorkingRevision -> "HEAD"
          cached <- withExactCacheContext defaultQueryExecutionHooks repository revision requestedRevision $ \connection context -> do
            sourceResult <- readRelevantSource repository revision request
            case sourceResult of
              Left problem -> pure (Left problem)
              Right source -> rankRelevantWith request (exactQuerySnapshot context) source connection (exactQueryMaterialization context)
          pure (maybe (Left (RelevantCompilerFailure "validated exact archive is unavailable")) id cached)

runRelevantQueryAtRequested :: QueryExecutionHooks -> Repository -> RelevantRequest -> Text -> IO (Either RelevantFailure RelevantProjection)
runRelevantQueryAtRequested hooks repository request requestedRevision = do
  revisionResult <- resolveRepositoryRevision repository (RevisionSpec requestedRevision)
  case revisionResult of
    Left problem -> pure (Left (RelevantRepositoryFailure (Text.pack (show problem))))
    Right revision -> do
      cached <- withExactCacheContext hooks repository revision requestedRevision $ \connection context -> do
        sourceResult <- readRelevantSource repository revision request
        case sourceResult of
          Left problem -> pure (Left problem)
          Right source -> rankRelevantWith request (exactQuerySnapshot context) source connection (exactQueryMaterialization context)
      case cached of
        Just result -> pure result
        Nothing -> do
          queryColdFallback hooks
          snapshotResult <- readSnapshotAtResolvedWithHooks hooks repository requestedRevision revision
          case snapshotResult of
            Left failure -> pure (Left (relevantSnapshotFailure failure))
            Right snapshot -> do
              sourceResult <- readRelevantSource repository revision request
              case sourceResult of
                Left problem -> pure (Left problem)
                Right source -> do
                  queryColdCompile hooks
                  captured <- try (bracket (open ":memory:") close (compileAndRank revision snapshot source)) :: IO (Either SomeException (Either RelevantFailure RelevantProjection))
                  case captured of
                    Left exception ->
                      case fromException exception of
                        Just cancellation -> throwIO (cancellation :: SomeAsyncException)
                        Nothing -> pure (Left (RelevantCompilerFailure (Text.pack (displayException exception))))
                    Right result -> pure result
  where
    compileAndRank revision snapshot source connection = do
      compiledResult <- coldCompileRepository connection revision
      case compiledResult of
        Left problem -> pure (Left (RelevantCompilerFailure (Text.pack (show problem))))
        Right compiled ->
          case coldCompilerSearchMaterialization compiled of
            Nothing -> pure (Left (RelevantCompilerFailure "compiler produced no search materialization for an integrity-gated snapshot"))
            Just materialization -> rankRelevantWith request snapshot source connection materialization

readRelevantSource :: Repository -> ResolvedRepositoryRevision -> RelevantRequest -> IO (Either RelevantFailure RelevantSource)
readRelevantSource repository revision request =
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

rankRelevantWith :: RelevantRequest -> ReadSnapshot -> RelevantSource -> Connection -> SearchMaterialization -> IO (Either RelevantFailure RelevantProjection)
rankRelevantWith request snapshot source connection materialization = do
  ranked <- runRelevant connection snapshot materialization request source
  pure (either (Left . RelevantQueryFailure) Right ranked)

-- | Read a fully validated exact archive context without repository fallback.
-- This narrow inspection seam exists for target-relative parity tests; normal
-- service callers continue through search/relevant entry points.
loadExactQueryContextForTest :: Repository -> Text -> IO (Maybe ExactQueryContext)
loadExactQueryContextForTest repository requestedRevision =
  loadExactQueryContextWithAcquisitionHooksForTest repository requestedRevision (pure ()) (pure ())

-- | Test-only acquisition boundary.  The acquired hook runs only after
-- 'bracket' owns a successfully opened connection; the close hook runs after
-- the one corresponding close completes.  Neither hook receives the
-- connection or archive path.
loadExactQueryContextWithAcquisitionHooksForTest
  :: Repository
  -> Text
  -> IO ()
  -> IO ()
  -> IO (Maybe ExactQueryContext)
loadExactQueryContextWithAcquisitionHooksForTest repository requestedRevision afterAcquired afterClose = do
  revisionResult <- resolveRepositoryRevision repository (RevisionSpec requestedRevision)
  case revisionResult of
    Left _ -> pure Nothing
    Right revision ->
      withExactCacheContextWithAcquisitionHooks defaultQueryExecutionHooks afterAcquired afterClose repository revision requestedRevision
        (\_ context -> pure context)

-- | Run a query only against the one immutable archive named by the resolved
-- revision.  It first opens the existing archive in read/write mode solely for
-- SQLite's full integrity checks, then enables @query_only@ before loading or
-- querying; any failure leaves the candidate untrusted and returns 'Nothing'
-- so the caller executes its existing cold path unchanged.
withExactCacheContext
  :: QueryExecutionHooks
  -> Repository
  -> ResolvedRepositoryRevision
  -> Text
  -> (Connection -> ExactQueryContext -> IO value)
  -> IO (Maybe value)
withExactCacheContext hooks repository revision requestedRevision useContext =
  withExactCacheContextWithAcquisitionHooks hooks (pure ()) (pure ()) repository revision requestedRevision useContext

withExactCacheContextWithAcquisitionHooks
  :: QueryExecutionHooks
  -> IO ()
  -> IO ()
  -> Repository
  -> ResolvedRepositoryRevision
  -> Text
  -> (Connection -> ExactQueryContext -> IO value)
  -> IO (Maybe value)
withExactCacheContextWithAcquisitionHooks hooks afterAcquired afterClose repository revision requestedRevision useContext =
  case exactCacheArchivePath repository (resolvedCommitOid revision) of
    Nothing -> pure Nothing
    Just path ->
      bracket (try @SomeException (openReadWriteExisting path)) release $ \opened ->
        case opened of
          Left exception -> rethrowQueryAsyncOrBusy exception >> pure Nothing
          Right archiveConnection -> do
            afterAcquired
            withExactArchiveTransaction archiveConnection $ do
              checked <- try $
                withValidatedExactCacheTargetConnection oid archiveConnection $ \acceptedRows -> do
                  execute_ archiveConnection "PRAGMA query_only=ON"
                  try @SomeException $ do
                    queryArchiveLoad hooks
                    loaded <- try (loadExactQueryContext acceptedRows requestedRevision oid)
                    case loaded of
                      Left exception -> rethrowQueryAsyncOrBusy exception >> queryArchiveRejected hooks (Text.pack (displayException exception)) >> pure Nothing
                      Right (Left problem) -> queryArchiveRejected hooks problem >> pure Nothing
                      Right (Right context) -> Just <$> useContext archiveConnection context
              case checked of
                Left exception -> rethrowQueryAsyncOrBusy exception >> pure Nothing
                Right Nothing -> do
                  queryArchiveRejected hooks "exact archive publication contract or metadata rejected"
                  pure Nothing
                Right (Just (Left consumerException)) -> throwIO consumerException
                Right (Just (Right result)) -> pure result
  where
    oid = gitOidText (resolvedCommitOid revision)
    release opened =
      case opened of
        Left _ -> pure ()
        Right connection -> close connection >> afterClose

rethrowQueryAsyncOrBusy :: SomeException -> IO ()
rethrowQueryAsyncOrBusy exception =
  case fromException exception of
    Just cancellation -> throwIO (cancellation :: SomeAsyncException)
    Nothing -> case fromException exception of
      Just ExactArchiveBusy -> throwIO ExactArchiveBusy
      Nothing -> pure ()

data ExactQueryContext = ExactQueryContext
  { exactQuerySnapshot :: ReadSnapshot,
    exactQueryMaterialization :: SearchMaterialization
  }

-- | One ordered pass over an archive row family.  SQL already supplies the
-- canonical row order; Map accumulation reverses each bucket, so reverse once
-- after grouping rather than append while ingesting.
indexExactQueryRows :: Ord key => (row -> key) -> [row] -> Map.Map key [row]
indexExactQueryRows key rows = fmap reverse (foldl' insert Map.empty rows)
  where
    insert indexed row = Map.insertWith (<>) (key row) [row] indexed

-- | Rebuild the two pure query inputs from a fully validated immutable archive.
-- All row decoding happens while the same read-only handle is open; no partial
-- value is exposed when any semantic or provenance authority disagrees.
loadExactQueryContext :: ValidatedCacheRows scope -> Text -> Text -> IO (Either Text ExactQueryContext)
loadExactQueryContext acceptedRows requestedRevision resolvedOid = do
  let configRows = validatedRepositoryConfigRows acceptedRows
      sourceRows = validatedManagedSourceRows acceptedRows
      operationRows = validatedOperationRows acceptedRows
      memberRows = validatedMemberRows acceptedRows
      coverageRows = validatedCoverageRows acceptedRows
      commitRows = validatedOperationCommitRows acceptedRows
      landingRows = validatedLandingRows acceptedRows
      targetRows = validatedTargetRows acceptedRows
      lineConfigRows = validatedLineConfigRows acceptedRows
      materialization = validatedSearchMaterialization acceptedRows
  pure $ do
    config <- decodeConfig configRows
    documents <- traverse decodeDocument sourceRows
    classified <- traverse (uncurry classifyDocument) (zip documents sourceRows)
    placements <- decodePlacements config classified resolvedOid operationRows memberRows coverageRows commitRows landingRows targetRows lineConfigRows
    let snapshot = ReadSnapshot (RevisionIdentity requestedRevision resolvedOid) documents (reduceManagedGraph (map Document.parsedManagedRecord documents)) placements
    case materializeCurrentSearch snapshot of
      Left problem -> Left ("cached search materialization does not reconstruct: " <> Text.pack (show problem))
      Right rebuilt -> do
        let persisted = canonicalMaterialization materialization
        if rebuilt /= persisted
          then Left "persisted search materialization differs from reconstructed snapshot"
          else Right (ExactQueryContext snapshot persisted)
  where
    canonicalMaterialization materialization =
      materialization
        { searchMaterializationPassages =
            sortOn passageKey (searchMaterializationPassages materialization)
        }
    passageKey passage =
      ( searchPassageDocumentItemId passage,
        searchPassageSectionKind passage,
        searchPassageOrdinal passage,
        searchPassageLineStart passage,
        searchPassageLineEnd passage
      )
    decodeConfig rows = case rows of
      [("default", Nothing, "valid")] -> Right defaultConfig
      [("committed", Just bytes, "valid")] -> do
        text <- either (const (Left "cached repository config is not UTF-8")) Right (TextEncoding.decodeUtf8' bytes)
        either (Left . ("cached repository config is invalid: " <>) . Text.pack . show) Right (parseConfigText text)
      _ -> Left "cached repository config rows are invalid"
    decodeDocument (pathText, oidText, objectType, mode, maybeBytes, parseState)
      | objectType /= "blob" || mode `notElem` ["100644", "100755"] || parseState /= "valid" = Left "cached managed source metadata is invalid"
      | otherwise = do
          path <- either (Left . ("cached managed path is invalid: " <>) . Text.pack . show) Right (mkRepoPath pathText)
          _ <- either (const (Left "cached managed source oid is invalid")) Right (mkGitOid oidText)
          bytes <- maybe (Left "cached managed source bytes are missing") Right maybeBytes
          either (Left . ("cached managed document is invalid: " <>) . Text.pack . show) Right (Document.parseManagedDocument path bytes)
    classifyDocument document (pathText, oidText, _, _, _, _) = do
      if pathText /= repoPathText (Document.parsedManagedPath document)
        then Left "cached managed documents are not in canonical source order"
        else pure ()
      oid <- either (const (Left "cached classified blob oid is invalid")) Right (mkGitOid oidText)
      Right Classification.ParsedManagedDocument
        { Classification.parsedDocumentObjectRef = provenanceObjectIdText (provenanceObjectId (Document.parsedManagedCapsule document))
        , Classification.parsedManagedPath = Document.parsedManagedPath document
        , Classification.parsedManagedCapsule = Document.parsedManagedCapsule document
        , Classification.parsedBlobOid = Just oid
        , Classification.parsedSemanticHash = encodeBase64Url (digestBytes (semanticDigest (Document.parsedManagedSemantic document)))
        }

decodePlacements
  :: Config
  -> [Classification.ParsedManagedDocument]
  -> Text
  -> [(Text, Text)]
  -> [(Text, Text, Text, Text)]
  -> [(Text, Text, Text)]
  -> [(Text, Text, Text, Int64, Int64, Text, Text)]
  -> [(Text, Text, Text, Text, Text, Int64)]
  -> [(Text, Text)]
  -> [(Text, Text)]
  -> Either Text (Map.Map OperationId PlacementEvidence)
decodePlacements config documents target operationRows memberRows coverageRows commitRows landingRows targetRows configRows = do
  targetOid <- oid "cached target oid is invalid" target
  configRow <- case configRows of
    [(key, json)] -> Right (LineConfigRow key json)
    _ -> Left "cached line config is not singleton"
  let expectedConfig = configKey
        (repoPathText (managedDecisionPath (configManagedPaths config)))
        (repoPathText (managedConnectionPath (configManagedPaths config)))
        (map logicalLineId (configLogicalLines config))
      operationTexts = map fst operationRows
      reachable = [commit | (targetOid', commit) <- targetRows, targetOid' == target]
  if lineConfigRowKey configRow /= expectedConfig
    then Left "cached line config key does not match snapshot configuration"
    else pure ()
  if null targetRows || any (\(targetOid', commit) -> targetOid' /= target || not (validOid targetOid') || not (validOid commit)) targetRows || target `notElem` reachable
    then Left "cached target reachability is invalid"
    else pure ()
  if length operationTexts /= length (unique operationTexts)
    then Left "cached operations are duplicated"
    else pure ()
  reachableOids <- traverse (oid "cached reachable commit oid is invalid") reachable
  let operationSet = Set.fromList operationTexts
      documentIndex = operationDocuments documents
      memberIndex = indexExactQueryRows (\(operationId, _, _, _) -> operationId) memberRows
      coverageIndex = indexExactQueryRows (\(operationId, _, _) -> operationId) coverageRows
      commitIndex = indexExactQueryRows (\(operationId, _, _, _, _, _, _) -> operationId) commitRows
      landingIndex = indexExactQueryRows (\(_, operationId, _, _, _, _) -> operationId) landingRows
      reachableSet = Set.fromList reachableOids
  if any (`Set.notMember` operationSet) (Map.keys memberIndex)
      || any (`Set.notMember` operationSet) (Map.keys coverageIndex)
      || any (`Set.notMember` operationSet) (Map.keys commitIndex)
      || any (`Set.notMember` operationSet) (Map.keys landingIndex)
    then Left "cached placement rows reference an unknown operation"
    else pure ()
  if any (`Set.notMember` operationSet) (Map.keys documentIndex)
    then Left "cached managed documents reference an unknown operation"
    else pure ()
  operations <- traverse (buildOperation expectedConfig documentIndex reachableSet memberIndex coverageIndex commitIndex landingIndex) operationRows
  firstPlacement (materialize config documents expectedConfig
    ProvenanceEvidence
      { provenanceEvidenceTargetOid = targetOid
      , provenanceEvidenceConfig = Just configRow
      , provenanceEvidenceOperations = operations
      , provenanceEvidenceLineRefs = []
      , provenanceEvidenceRefs = []
      , provenanceEvidenceRoots = []
      })
  where
    firstPlacement = either (Left . ("cached placement evidence is invalid: " <>) . Text.pack . show) Right
    validOid value = Text.length value `elem` [40,64] && Text.all (\c -> ('0' <= c && c <= '9') || ('a' <= c && c <= 'f')) value
    oid label value = either (const (Left label)) Right (mkGitOid value)
    unique values = Map.keys (Map.fromList [(value, ()) | value <- values])
    buildOperation expectedConfig documentIndex reachableSet memberIndex coverageIndex commitIndex landingIndex (operation, basisText) = do
      basis <- oid "cached operation basis is invalid" basisText
      let docs = Map.findWithDefault [] operation documentIndex
          objects = Map.findWithDefault [] operation memberIndex
          coverage = Map.findWithDefault [] operation coverageIndex
          certificates = [signature | (_, targetOid', signature) <- coverage, targetOid' == target]
          commits = Map.findWithDefault [] operation commitIndex
          allLandings = Map.findWithDefault [] operation landingIndex
          landings = [row | row@(configKey', _, _, _, _, _) <- allLandings, configKey' == expectedConfig]
      if null docs || any (\(_, targetOid', _) -> targetOid' /= target) coverage
        then Left "cached operation coverage does not match documents"
        else if any (\(configKey', _, _, _, _, _) -> configKey' /= expectedConfig) allLandings
          then Left "cached line landing config does not match snapshot configuration"
        else case certificates of
          [certificate] -> do
            adr <- documentAdr docs
            registeredObjects <- traverse toObject objects
            placements <- traverse toCommit commits
            if null placements || any (\row -> operationCommitRowCommitOid row `Set.notMember` reachableSet) placements
              then Left "cached operation placements are not target reachable"
              else pure ()
            landings' <- traverse toLanding landings
            if any (\row -> lineLandingRowCommitOid row `Set.notMember` reachableSet) landings'
              then Left "cached line landings are not target reachable"
              else pure ()
            Right ProvenanceOperationEvidence
              { provenanceEvidenceRegistration = RegisteredOperationRow operation adr basis certificate
              , provenanceEvidenceObjects = registeredObjects
              , provenanceEvidenceCommits = placements
              , provenanceEvidenceLandings = landings'
              , provenanceEvidenceIssues = []
              }
          _ -> Left "cached operation coverage does not have one certificate"
    operationDocuments parsedDocuments = Map.fromListWith (<>)
       [ (operationIdText (provenanceOperationId (Classification.parsedManagedCapsule document)), [document])
         | document <- parsedDocuments
        ]
    documentAdr [] = Left "cached operation documents are missing"
    documentAdr (document : _) = Right $
       case Classification.parsedDocumentObjectRef document of
         value | Text.isPrefixOf "A" value -> Just value
         value | Text.isPrefixOf "R" value -> Just ("A" <> Text.drop 1 value)
         _ -> Nothing
    toObject (operation, objectId, path, blob) = RegisteredObjectRow operation objectId path <$> oid "cached member blob oid is invalid" blob
    toCommit (operation, commit, classification, authored, committed, subject, parents) =
      OperationCommitRow operation <$> oid "cached placement oid is invalid" commit <*> pure classification <*> pure (fromIntegral authored) <*> pure (fromIntegral committed) <*> pure subject <*> pure parents
    toLanding (configKey', operation, lineId, refName, commit, complete) =
      LineLandingRow configKey' operation lineId refName <$> oid "cached landing oid is invalid" commit <*> pure (fromIntegral complete)

readSnapshotAt :: Repository -> Text -> IO (Either SnapshotReadFailure ReadSnapshot)
readSnapshotAt repository requestedRevision = do
  revisionResult <- resolveRepositoryRevision repository (RevisionSpec requestedRevision)
  case revisionResult of
    Left problem -> pure (Left (SnapshotRepositoryFailure (Text.pack (show problem))))
    Right revision -> readSnapshotAtResolved repository requestedRevision revision

readSnapshotAtResolved :: Repository -> Text -> ResolvedRepositoryRevision -> IO (Either SnapshotReadFailure ReadSnapshot)
readSnapshotAtResolved = readSnapshotAtResolvedWithHooks defaultQueryExecutionHooks

readSnapshotAtResolvedWithHooks :: QueryExecutionHooks -> Repository -> Text -> ResolvedRepositoryRevision -> IO (Either SnapshotReadFailure ReadSnapshot)
readSnapshotAtResolvedWithHooks hooks repository requestedRevision revision = do
      queryRawObservation hooks
      rawResult <- observeRawRepositorySnapshotAt revision
      case rawResult of
        Left problem -> pure (Left (SnapshotRepositoryFailure (Text.pack (show problem))))
        Right raw -> do
          querySnapshotAnalysis hooks
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
                          queryProvenanceHydration hooks
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

classificationDocuments
  :: RawRepositorySnapshotObservation
  -> [Document.ParsedManagedDocument]
  -> Either SnapshotReadFailure [Classification.ParsedManagedDocument]
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
                -- Match the immutable cache's @operation_member.semantic_digest@
                -- representation.  This is the fourth field in the shared
                -- operation-member registration signature.
                Classification.parsedSemanticHash = encodeBase64Url (digestBytes (semanticDigest (Document.parsedManagedSemantic document)))
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
