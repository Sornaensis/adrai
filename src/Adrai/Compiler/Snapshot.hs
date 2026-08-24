{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Placement-free analysis of one immutable repository revision.
--
-- The analyzed form always exists after successful Git observation.  The
-- parsed/reduced wrapper exists only when source-integrity errors are absent;
-- warnings and semantic ADR conflicts remain admissible.
module Adrai.Compiler.Snapshot
  ( CompilerDiagnosticSeverity (..),
    CompilerDiagnosticOrigin (..),
    CompilerDiagnosticCode (..),
    compilerDiagnosticCodeText,
    CompilerDiagnostic (..),
    AnalyzedRepositorySnapshot (..),
    analyzedRawObservation,
    analyzedManagedEntries,
    analyzedNonblobObservations,
    analyzedDiagnostics,
    analyzedDocuments,
    analyzedReduction,
    analyzedConflicts,
    analyzedHistoryComplete,
    analyzedHistoryCommitsScanned,
    analyzedSourceFingerprint,
    ParsedReducedRepositorySnapshot,
    parsedReducedAnalyzed,
    parsedReducedDocuments,
    parsedReducedReduction,
    parsedReducedConflicts,
    analyzeRepositorySnapshot,
    analyzeRepositorySnapshotWithAttribution,
    gateAnalyzedRepositorySnapshot,
    sourceFingerprint,
    validateManagedOperations,
    analyzeRepositorySnapshotWithHistoryParseCount,
    analyzeRepositorySnapshotWithHistoryCounts,
    historyConvergencePairs,
  )
where

import Adrai.Format.Document
import Adrai.Compiler.Attribution
  ( AttributionCounter (CounterBlobRequests, CounterChanges, CounterEdges, CounterNodes, CounterParsedBlobs, CounterSelectedNodes),
    AttributionPhase (BasisChecks, HistoryGraphEnumeration, HistoryPathSelectionDiff, HistoryReplayParse),
    ColdCompileAttribution,
    attributionEnabled,
    inertColdCompileAttribution,
    recordAttributionCounter,
    withAttributionEitherPhase,
  )
import Adrai.Git
import Adrai.Graph
import Adrai.Integrity
import Adrai.Provenance
import Adrai.Repository
import Adrai.Types
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Foldable (foldr')
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

data CompilerDiagnosticSeverity
  = CompilerDiagnosticError
  | CompilerDiagnosticWarning
  deriving (Eq, Ord, Show)

data CompilerDiagnosticOrigin
  = CompilerConfigOrigin
  | CompilerPathDocumentOrigin
  | CompilerHistoryOrigin
  | CompilerOperationOrigin
  | CompilerGraphOrigin
  | CompilerBasisOrigin
  deriving (Eq, Ord, Show)

data CompilerDiagnosticCode
  = CompilerIntegrityCode IntegrityIssueCode
  | CompilerGraphCode GraphIssueCode
  deriving (Eq, Ord, Show)

compilerDiagnosticCodeText :: CompilerDiagnosticCode -> Text
compilerDiagnosticCodeText code =
  case code of
    CompilerIntegrityCode integrityCode -> integrityIssueCodeText integrityCode
    CompilerGraphCode graphCode -> graphIssueCodeText graphCode

data CompilerDiagnostic = CompilerDiagnostic
  { compilerDiagnosticSeverity :: CompilerDiagnosticSeverity,
    compilerDiagnosticOrigin :: CompilerDiagnosticOrigin,
    compilerDiagnosticCode :: CompilerDiagnosticCode,
    compilerDiagnosticAdr :: Maybe AdrId,
    compilerDiagnosticObject :: Maybe ObjectRef,
    compilerDiagnosticOperation :: Maybe OperationId,
    compilerDiagnosticCommit :: Maybe GitOid,
    compilerDiagnosticPath :: Maybe RepoPath,
    compilerDiagnosticMessage :: Text
  }
  deriving (Eq, Ord, Show)

data AnalyzedRepositorySnapshot = AnalyzedRepositorySnapshot
  { analyzedRawObservation :: RawRepositorySnapshotObservation,
    analyzedManagedEntries :: [ManagedSnapshotEntry],
    analyzedNonblobObservations :: [RepositoryTreeObservation],
    analyzedDiagnostics :: [CompilerDiagnostic],
    analyzedDocuments :: [ParsedManagedDocument],
    analyzedReduction :: GraphReduction,
    analyzedConflicts :: [AdrConflict],
    analyzedHistoryComplete :: Bool,
    analyzedHistoryCommitsScanned :: Int,
    analyzedSourceFingerprint :: Digest
  }
  deriving (Eq, Show)

newtype ParsedReducedRepositorySnapshot = ParsedReducedRepositorySnapshot AnalyzedRepositorySnapshot
  deriving (Eq, Show)

parsedReducedAnalyzed :: ParsedReducedRepositorySnapshot -> AnalyzedRepositorySnapshot
parsedReducedAnalyzed (ParsedReducedRepositorySnapshot analyzed) = analyzed

parsedReducedDocuments :: ParsedReducedRepositorySnapshot -> [ParsedManagedDocument]
parsedReducedDocuments = analyzedDocuments . parsedReducedAnalyzed

parsedReducedReduction :: ParsedReducedRepositorySnapshot -> GraphReduction
parsedReducedReduction = analyzedReduction . parsedReducedAnalyzed

parsedReducedConflicts :: ParsedReducedRepositorySnapshot -> [AdrConflict]
parsedReducedConflicts = analyzedConflicts . parsedReducedAnalyzed

gateAnalyzedRepositorySnapshot :: AnalyzedRepositorySnapshot -> Either [CompilerDiagnostic] ParsedReducedRepositorySnapshot
gateAnalyzedRepositorySnapshot analyzed =
  case filter ((== CompilerDiagnosticError) . compilerDiagnosticSeverity) (analyzedDiagnostics analyzed) of
    [] -> Right (ParsedReducedRepositorySnapshot analyzed)
    problems -> Left problems

analyzeRepositorySnapshot :: RawRepositorySnapshotObservation -> IO (Either RepositorySnapshotError AnalyzedRepositorySnapshot)
analyzeRepositorySnapshot = analyzeRepositorySnapshotWithAttribution inertColdCompileAttribution

analyzeRepositorySnapshotWithAttribution :: ColdCompileAttribution -> RawRepositorySnapshotObservation -> IO (Either RepositorySnapshotError AnalyzedRepositorySnapshot)
analyzeRepositorySnapshotWithAttribution attribution raw
  | attributionEnabled attribution = do
      parseCount <- newIORef 0
      requestCount <- newIORef 0
      analyzeRepositorySnapshotWithHistoryCounter attribution (Just parseCount) (Just requestCount) raw
  -- Ordinary compilation must not allocate observer counters.  The explicit
  -- history-count test seam below supplies its own counters when it needs
  -- them, while this inert path has no profiling bookkeeping at all.
  | otherwise = analyzeRepositorySnapshotWithHistoryCounter attribution Nothing Nothing raw

-- | Analyze a snapshot and return the number of managed historical blobs that
-- were parsed while applying reachable-tree deltas.  The production compiler
-- intentionally ignores this observation; it exists so the cold compiler
-- regression suite can prove that a noise-only suffix does not make parsing
-- proportional to the number of commits.
analyzeRepositorySnapshotWithHistoryParseCount :: RawRepositorySnapshotObservation -> IO (Either RepositorySnapshotError AnalyzedRepositorySnapshot, Int)
analyzeRepositorySnapshotWithHistoryParseCount raw = do
  (analyzed, parsed, _requested) <- analyzeRepositorySnapshotWithHistoryCounts raw
  pure (analyzed, parsed)

-- | Test-only observation for the history streaming seam.  The final count is
-- the number of unique blob OIDs sent to @git cat-file@ across bounded reads;
-- it makes accidental re-requesting of a prefetched ordinary batch visible
-- without retaining blob bytes in the production snapshot.
analyzeRepositorySnapshotWithHistoryCounts :: RawRepositorySnapshotObservation -> IO (Either RepositorySnapshotError AnalyzedRepositorySnapshot, Int, Int)
analyzeRepositorySnapshotWithHistoryCounts raw = do
  parseCount <- newIORef 0
  requestCount <- newIORef 0
  analyzed <- analyzeRepositorySnapshotWithHistoryCounter inertColdCompileAttribution (Just parseCount) (Just requestCount) raw
  parsed <- readIORef parseCount
  requested <- readIORef requestCount
  pure (analyzed, parsed, requested)

analyzeRepositorySnapshotWithHistoryCounter :: ColdCompileAttribution -> Maybe (IORef Int) -> Maybe (IORef Int) -> RawRepositorySnapshotObservation -> IO (Either RepositorySnapshotError AnalyzedRepositorySnapshot)
analyzeRepositorySnapshotWithHistoryCounter attribution parseCount requestCount raw =
  case rawRepositorySnapshotManagedPaths raw of
    Nothing ->
      pure
        ( Right
            ( assembleAnalysis
                raw
                []
                []
                (configDiagnostics (rawRepositorySnapshotConfig raw))
                []
                 (GraphReduction [] [])
                 []
                 False
                 0
            )
        )
    Just paths -> do
      let observations = rawRepositorySnapshotEntries raw
          (entries, nonblobs) = partitionObservations observations
          snapshotIssues = validateManagedSnapshot paths entries
          uniqueDocuments = uniqueValidDocuments entries
          reduction = reduceManagedGraph (map parsedManagedRecord uniqueDocuments)
          graphDiagnostics = map graphDiagnostic (graphReductionIssues reduction)
          conflicts = sortOn adrConflictAdr (mapMaybe classifyAdrConflict (graphReductionAdrs reduction))
          currentDiagnostics =
            configDiagnostics (rawRepositorySnapshotConfig raw)
              <> map (integrityDiagnostic CompilerPathDocumentOrigin Nothing) snapshotIssues
              <> map (nonblobDiagnostic CompilerPathDocumentOrigin Nothing) nonblobs
              <> validateManagedOperations uniqueDocuments
              <> graphDiagnostics
      historyResult <- observeHistory attribution parseCount requestCount raw paths entries
      case historyResult of
        Left problem -> pure (Left problem)
        Right (historyComplete, historyCommitsScanned, historyDiagnostics) -> do
          basisResult <- withAttributionEitherPhase attribution BasisChecks (observeBasisDiagnostics raw uniqueDocuments)
          pure $ do
            basisDiagnostics <- basisResult
            Right
              ( assembleAnalysis
                  raw
                  entries
                  nonblobs
                  (currentDiagnostics <> historyDiagnostics <> basisDiagnostics)
                  uniqueDocuments
                  reduction
                   conflicts
                   historyComplete
                   historyCommitsScanned
               )

assembleAnalysis :: RawRepositorySnapshotObservation -> [ManagedSnapshotEntry] -> [RepositoryTreeObservation] -> [CompilerDiagnostic] -> [ParsedManagedDocument] -> GraphReduction -> [AdrConflict] -> Bool -> Int -> AnalyzedRepositorySnapshot
assembleAnalysis raw entries nonblobs diagnostics documents reduction conflicts historyComplete historyCommitsScanned =
  AnalyzedRepositorySnapshot
    { analyzedRawObservation = rawWithoutBlobs,
      analyzedManagedEntries = entries,
      analyzedNonblobObservations = nonblobs,
      analyzedDiagnostics = canonicalDiagnostics diagnostics,
      analyzedDocuments = documents,
      analyzedReduction = reduction,
      analyzedConflicts = conflicts,
      analyzedHistoryComplete = historyComplete,
      analyzedHistoryCommitsScanned = historyCommitsScanned,
      analyzedSourceFingerprint = sourceFingerprint raw
    }
  where
    -- Blob bytes are consumed by the time analysis assembles (document parsing
    -- and the streaming source fingerprint); the managed_source row writer
    -- re-streams them from git, so the analyzed snapshot retains no
    -- whole-corpus blob bytes alongside the search materialization.
    rawWithoutBlobs = raw { rawRepositorySnapshotEntries = map dropBlob (rawRepositorySnapshotEntries raw) }
    dropBlob observation = observation { repositoryTreeBlob = Nothing }

partitionObservations :: [RepositoryTreeObservation] -> ([ManagedSnapshotEntry], [RepositoryTreeObservation])
partitionObservations observations =
  ( [ parseSnapshotEntry (gitTreePath entry) (gitBlobBytes blob)
      | observation <- observations,
        let entry = repositoryTreeEntry observation,
        Just blob <- [repositoryTreeBlob observation]
    ],
    [ observation
      | observation <- observations,
        repositoryTreeBlob observation == Nothing
    ]
  )

uniqueValidDocuments :: [ManagedSnapshotEntry] -> [ParsedManagedDocument]
uniqueValidDocuments entries =
  sortOn (repoPathText . parsedManagedPath) (Map.elems documentsByObject)
  where
    documentsByObject =
      foldl'
        ( \documents entry ->
            case snapshotEntryDocument entry of
              Left _ -> documents
              Right document -> Map.insertWith (\_ existing -> existing) (managedDocumentObject document) document documents
        )
        Map.empty
        (sortOn (repoPathText . snapshotEntryPath) entries)

observeHistory :: ColdCompileAttribution -> Maybe (IORef Int) -> Maybe (IORef Int) -> RawRepositorySnapshotObservation -> ManagedPaths -> [ManagedSnapshotEntry] -> IO (Either RepositorySnapshotError (Bool, Int, [CompilerDiagnostic]))
observeHistory attribution parseCount requestCount raw paths _targetEntries = do
  let revision = rawRepositorySnapshotRevision raw
      repository = resolvedRepository revision
      targetOid = resolvedCommitOid revision
  shallowResult <- isShallowRepository repository
  graphResult <-
    withAttributionEitherPhase attribution HistoryGraphEnumeration $ do
      result <- reachableCommitGraphAt repository targetOid
      case result of
        Right nodes | attributionEnabled attribution -> do
          recordAttributionCounter attribution CounterNodes (fromIntegral (length nodes))
          recordAttributionCounter attribution CounterEdges (fromIntegral (sum (map (length . gitCommitNodeParents) nodes)))
        _ -> pure ()
      pure result
  case (shallowResult, graphResult) of
    (Left problem, _) -> pure (Left (RepositorySnapshotGitError problem))
    (_, Left problem) -> pure (Left (RepositorySnapshotGitError problem))
    (Right shallow, Right nodes) -> do
      let targetPaths =
            Set.fromList
              (map (gitTreePath . repositoryTreeEntry) (rawRepositorySnapshotEntries raw))
      treeResult <- observeCommitTrees attribution parseCount requestCount revision paths targetOid targetPaths shallow nodes
      pure $ do
        historyDiagnostics <- treeResult
        let coverageDiagnostics =
              [ diagnostic
                  CompilerDiagnosticWarning
                  CompilerHistoryOrigin
                  HistoryCoverageIncomplete
                  Nothing
                  Nothing
                  Nothing
                  (Just targetOid)
                  Nothing
                  "reachable history is shallow; append-only coverage is incomplete"
                | shallow
              ]
        Right (not shallow, length nodes, coverageDiagnostics <> historyDiagnostics)

-- | Compact immutable state for one live parent tree.  Blob bytes never enter
-- this state: Git object ids are byte-exact identities, while only the parsed
-- operation/object pair is retained for append-only membership checks.
data HistoryState = HistoryState
  { historyStatePaths :: !(Map RepoPath HistoryPathState),
    historyStateMembers :: !(Map OperationId (Map ObjectRef Int)),
    -- Only nonblobs participate in the per-commit historical diagnostic.  A
    -- separate strict index avoids traversing every live managed blob on each
    -- otherwise empty historical edge.
    historyStateNonblobs :: !(Map RepoPath GitTreeEntry)
  }
  deriving (Eq)

data HistoryPathState
  = HistoryBlob !GitTreeEntry !(Maybe (OperationId, ObjectRef))
  | HistoryNonblob !GitTreeEntry
  deriving (Eq)

emptyHistoryState :: HistoryState
emptyHistoryState = HistoryState Map.empty Map.empty Map.empty

-- | Walk the reachable commit list in rev-list --topo-order --reverse order
-- (parents always precede children).  Git supplies only the changed paths on
-- each parent edge; a persistent compact state is retained only while a
-- remaining child still needs it.  This keeps cold validation proportional to
-- commit edges and managed changes rather than historical whole-tree size.
observeCommitTrees :: ColdCompileAttribution -> Maybe (IORef Int) -> Maybe (IORef Int) -> ResolvedRepositoryRevision -> ManagedPaths -> GitOid -> Set RepoPath -> Bool -> [GitCommitNode] -> IO (Either RepositorySnapshotError [CompilerDiagnostic])
observeCommitTrees attribution parseCount requestCount revision paths targetOid targetPaths shallow nodes = do
  deltasResult <- withAttributionEitherPhase attribution HistoryPathSelectionDiff $ do
    result <- historyTreeDeltasAt repository (not shallow) nodes configRepositoryPath roots
    case result of
      Left _ -> pure ()
      Right deltas | attributionEnabled attribution -> do
        let selected = map selectedChanges deltas
        recordAttributionCounter attribution CounterSelectedNodes (fromIntegral (length (filter (not . null) selected)))
        recordAttributionCounter attribution CounterChanges (fromIntegral (sum (map length selected)))
      _ -> pure ()
    pure result
  case first RepositorySnapshotGitError deltasResult of
    Left problem -> pure (Left problem)
    Right deltas ->
      withAttributionEitherPhase attribution HistoryReplayParse $ do
        result <- go Map.empty remainingChildren [] nodes (groupDeltas deltas)
        if attributionEnabled attribution
          then do
            parsed <- maybe (pure 0) readIORef parseCount
            requested <- maybe (pure 0) readIORef requestCount
            recordAttributionCounter attribution CounterParsedBlobs (fromIntegral parsed)
            recordAttributionCounter attribution CounterBlobRequests (fromIntegral requested)
          else pure ()
        pure result
  where
    repository = resolvedRepository revision
    -- The committed config is a semantic input: selection must observe it as
    -- well as the currently configured managed roots.  Git falls back to the
    -- full graph if it changed after the root boundary.
    roots = [configRepositoryPath, managedDecisionPath paths, managedConnectionPath paths]
    -- Remaining-children refcount per parent oid over the whole node list:
    -- how many reachable commits still reference this parent's compact state.
    remainingChildren :: Map GitOid Int
    remainingChildren =
      Map.fromListWith (+)
        [ (parentOid, 1)
          | node <- nodes,
            parentOid <- gitCommitNodeParents node
        ]
    go :: Map GitOid HistoryState -> Map GitOid Int -> [CompilerDiagnostic] -> [GitCommitNode] -> Map GitOid [GitHistoryTreeDelta] -> IO (Either RepositorySnapshotError [CompilerDiagnostic])
    go _ _ diagnostics [] _ = pure (Right (reverse diagnostics))
    go states refcounts diagnostics remaining deltaGroups = do
      -- A single commit is allowed to touch more than the ordinary batch
      -- budget.  It must not, however, turn that budget into an unbounded
      -- retention exception: process its edges in bounded chunks before
      -- continuing with the normal multi-node batch path.
      case remaining of
        node : laterNodes | nodeBlobCount deltaGroups node > historyBlobBatchLimit ->
          case Map.lookup (gitCommitNodeOid node) deltaGroups of
            Nothing -> pure (Left (missingDelta node))
            Just nodeDeltas -> do
              advanced <- advanceNode states refcounts diagnostics node (observeNodeStreaming parseCount requestCount repository states nodeDeltas)
              case advanced of
                Left problem -> pure (Left problem)
                Right (states', refcounts', diagnostics') ->
                  go states' refcounts' diagnostics' laterNodes deltaGroups
        _ -> do
          let (batchNodes, laterNodes) = takeHistoryBlobBatch deltaGroups remaining
              batchChanges = concatMap (nodeChanges deltaGroups) batchNodes
          blobsResult <- readChangedBlobs requestCount repository batchChanges
          case first RepositorySnapshotGitError blobsResult of
            Left problem -> pure (Left problem)
            Right blobs -> processBatch states refcounts diagnostics batchNodes laterNodes deltaGroups blobs

    -- Every history read is a finite buffered Git window.  The compact parent
    -- state still advances one commit at a time, preserving edge order while
    -- each window retains at most 'historyBlobBatchLimit' blobs.
    processBatch states refcounts diagnostics [] laterNodes deltaGroups _ =
      go states refcounts diagnostics laterNodes deltaGroups
    processBatch states refcounts diagnostics (node : pendingNodes) laterNodes deltaGroups blobs =
      case Map.lookup (gitCommitNodeOid node) deltaGroups of
        Nothing -> pure (Left (missingDelta node))
        Just nodeDeltas -> do
          advanced <- advanceNode states refcounts diagnostics node (observeNode parseCount states nodeDeltas blobs)
          case advanced of
            Left problem -> pure (Left problem)
            Right (states', refcounts', diagnostics') ->
              -- Do not look ahead again until every node whose blobs were
              -- preloaded into this map has consumed them.  Besides avoiding
              -- duplicate cat-file requests, this keeps the retained blob
              -- map bounded to this one ordinary batch.
              processBatch states' refcounts' diagnostics' pendingNodes laterNodes deltaGroups blobs

    advanceNode states refcounts diagnostics node observed = do
      result <- observed
      case result of
        Left problem -> pure (Left problem)
        Right (childState, nodeDiagnostics) -> do
          let nodeOid = gitCommitNodeOid node
              statesWithChild = Map.insert nodeOid childState states
              (refcounts', states') =
                foldr'
                  ( \parentOid (counts, current) ->
                      case Map.lookup parentOid counts of
                        Nothing -> (counts, current)
                        Just 1 -> (Map.delete parentOid counts, Map.delete parentOid current)
                        Just count -> (Map.insert parentOid (count - 1) counts, current)
                  )
                  (refcounts, statesWithChild)
                  (gitCommitNodeParents node)
          pure (Right (states', refcounts', foldl' (flip (:)) diagnostics nodeDiagnostics))

    selectedChanges :: GitHistoryTreeDelta -> [GitTreeChange]
    selectedChanges edge = filter (isSelectedManagedPath paths . gitTreeChangePath) (gitHistoryTreeDeltaChanges edge)

    nodeChanges deltaGroups node =
      maybe [] (concatMap selectedChanges) (Map.lookup (gitCommitNodeOid node) deltaGroups)

    -- A fixed OID budget bounds the blob map and still collapses the 12k/2k
    -- stress history to a small number of cat-file sessions.  An oversized
    -- node is deliberately left for the streaming branch above, rather than
    -- being admitted to an unbounded ordinary batch.
    takeHistoryBlobBatch deltaGroups nodesToBatch =
      case nodesToBatch of
        [] -> ([], [])
        firstNode : rest -> collect (nodeBlobCount deltaGroups firstNode) [firstNode] rest
      where
        collect _count reversedNodes [] = (reverse reversedNodes, [])
        collect count reversedNodes remaining@(node : rest)
          | nodeBlobCount deltaGroups node > historyBlobBatchLimit = (reverse reversedNodes, remaining)
          | count > 0 && count + nodeBlobCount deltaGroups node > historyBlobBatchLimit = (reverse reversedNodes, remaining)
          | otherwise = collect (count + nodeBlobCount deltaGroups node) (node : reversedNodes) rest

    observeNode counter states nodeDeltas blobs = do
      edgeStates <- traverse (observeEdge counter states blobs) nodeDeltas
      pure (sequence edgeStates >>= finishObservedNode nodeDeltas)

    -- Process each edge of an oversized node with a bounded blob map.  The
    -- mutable state retains only parsed identities, never the blob bytes.
    observeNodeStreaming counter blobRequests gitRepository states nodeDeltas = do
      edgeStates <- traverse (observeEdgeStreaming counter blobRequests gitRepository states) nodeDeltas
      pure (finishObservedNode nodeDeltas =<< sequence edgeStates)

    finishObservedNode nodeDeltas edgeStates = do
      (primary, convergencePairs) <-
        maybe
          (Left (missingDeltaForOid (gitHistoryTreeDeltaCommit (head nodeDeltas))))
          Right
          (historyConvergencePairs (map edgeChildState edgeStates))
      -- Ordinary history nodes produce no pairs, avoiding an O(live managed
      -- state) self-comparison.  Genuine merges remain fail-closed by
      -- comparing every independently reconstructed child to the first edge.
      if all (uncurry (==)) convergencePairs
        then Right ()
        else Left (divergentMergeState (gitHistoryTreeDeltaCommit (head nodeDeltas)))
      let nodeOid = gitHistoryTreeDeltaCommit (head nodeDeltas)
          edgeDiagnostics = concatMap (edgeIssues nodeOid) edgeStates
          historicalNonblobs =
            [ nonblobDiagnostic CompilerHistoryOrigin (Just nodeOid) (RepositoryTreeObservation entry Nothing)
              | nodeOid /= targetOid,
                entry <- Map.elems (historyStateNonblobs primary)
            ]
      Right (primary, edgeDiagnostics <> historicalNonblobs)

    edgeChildState (_, _, state) = state

    observeEdge counter states blobs edge =
      case
          case gitHistoryTreeDeltaParent edge of
            Nothing -> Right emptyHistoryState
            Just parentOid -> maybe (Left (missingParent parentOid)) Right (Map.lookup parentOid states)
        of
          Left problem -> pure (Left problem)
          Right parentState -> do
            childState <- applyChangesAtParseSeam counter blobs parentState (selectedChanges edge)
            pure (fmap (\child -> (edge, parentState, child)) childState)

    observeEdgeStreaming counter blobRequests gitRepository states edge = do
      let parentState =
            case gitHistoryTreeDeltaParent edge of
              Nothing -> Right emptyHistoryState
              Just parentOid -> maybe (Left (missingParent parentOid)) Right (Map.lookup parentOid states)
      case parentState of
        Left problem -> pure (Left problem)
        Right state -> do
          childState <- applyChangesStreaming counter blobRequests gitRepository state (selectedChanges edge)
          pure (fmap (\child -> (edge, state, child)) childState)

    edgeIssues nodeOid (edge, parentState, childState) =
      map (integrityDiagnostic CompilerHistoryOrigin (Just nodeOid) . remapDeletion targetPaths)
        (validateHistoryDelta parentState childState (selectedChanges edge))

    nodeBlobCount deltaGroups node =
      length
        [ ()
          | edge <- maybe [] id (Map.lookup (gitCommitNodeOid node) deltaGroups),
            change <- selectedChanges edge,
            Just entry <- [gitTreeChangeNewEntry change],
            gitTreeObjectType entry == GitBlobObject
        ]

    historyBlobBatchLimit = 256

    groupDeltas = foldl' (\groups delta -> Map.insertWith (flip (<>)) (gitHistoryTreeDeltaCommit delta) [delta] groups) Map.empty
    missingDelta node = RepositorySnapshotGitError (GitInvalidOutput "history tree deltas" (GitMalformedTreeRecord (TextEncoding.encodeUtf8 ("missing delta for " <> gitOidText (gitCommitNodeOid node)))))
    missingDeltaForOid oid = RepositorySnapshotGitError (GitInvalidOutput "history tree deltas" (GitMalformedTreeRecord (TextEncoding.encodeUtf8 ("empty delta group for " <> gitOidText oid))))
    missingParent oid = RepositorySnapshotGitError (GitInvalidOutput "history tree deltas" (GitMalformedTreeRecord (TextEncoding.encodeUtf8 ("missing live parent state for " <> gitOidText oid))))
    divergentMergeState oid = RepositorySnapshotGitError (GitInvalidOutput "history tree deltas" (GitMalformedTreeRecord (TextEncoding.encodeUtf8 ("merge parent deltas reconstruct different child trees for " <> gitOidText oid))))

readChangedBlobs :: Maybe (IORef Int) -> Repository -> [GitTreeChange] -> IO (Either GitError (Map GitOid GitBlob))
readChangedBlobs requestCount repository changes = do
  let objectIds =
        [ gitTreeOid entry
          | change <- changes,
            Just entry <- [gitTreeChangeNewEntry change],
            gitTreeObjectType entry == GitBlobObject
        ]
      requested = Map.keys (Map.fromList [(objectId, ()) | objectId <- objectIds])
  incrementHistoryCounter requestCount (length requested)
  readBlobBatch repository requested

-- | Stream one oversized edge in fixed OID batches.  Each map becomes
-- unreachable before the next batch is requested, including when the edge is
-- a single large root commit.
applyChangesStreaming :: Maybe (IORef Int) -> Maybe (IORef Int) -> Repository -> HistoryState -> [GitTreeChange] -> IO (Either RepositorySnapshotError HistoryState)
applyChangesStreaming parseCount requestCount repository initial changes = go initial (chunkChanges historyBlobBatchLimit changes)
  where
    go state [] = pure (Right state)
    go state (chunk : remaining) = do
      blobs <- readChangedBlobs requestCount repository chunk
      case first RepositorySnapshotGitError blobs of
        Left problem -> pure (Left problem)
        Right available -> do
          next <- applyChangesAtParseSeam parseCount available state chunk
          case next of
            Left problem -> pure (Left problem)
            Right updated -> go updated remaining
    historyBlobBatchLimit = 256
    chunkChanges _ [] = []
    chunkChanges limit values = let (next, remaining) = splitAt limit values in next : chunkChanges limit remaining

-- | This is the only historical parse seam.  Count immediately before the
-- parser is invoked so the regression metric represents actual parse attempts
-- (rather than changed paths or requested OIDs).
applyChangesAtParseSeam :: Maybe (IORef Int) -> Map GitOid GitBlob -> HistoryState -> [GitTreeChange] -> IO (Either RepositorySnapshotError HistoryState)
applyChangesAtParseSeam parseCount blobs initial = go initial
  where
    go current [] = pure (Right current)
    go current (change : remaining) = do
      next <- applyOne current change
      case next of
        Left problem -> pure (Left problem)
        Right updated -> go updated remaining
    applyOne current change = do
      let path = gitTreeChangePath change
          previous = Map.lookup path (historyStatePaths current)
      next <- pathStateFromChangeAtParseSeam parseCount blobs previous change
      pure $ do
        replacement <- next
        Right (replaceHistoryPath path replacement current)

pathStateFromChangeAtParseSeam :: Maybe (IORef Int) -> Map GitOid GitBlob -> Maybe HistoryPathState -> GitTreeChange -> IO (Either RepositorySnapshotError (Maybe HistoryPathState))
incrementHistoryCounter :: Maybe (IORef Int) -> Int -> IO ()
incrementHistoryCounter counter amount =
  case counter of
    Nothing -> pure ()
    Just reference -> modifyIORef' reference (+ amount)

pathStateFromChangeAtParseSeam parseCount blobs _previous change =
  case gitTreeChangeNewEntry change of
    Nothing -> pure (Right Nothing)
    Just entry
      | gitTreeObjectType entry /= GitBlobObject -> pure (Right (Just (HistoryNonblob entry)))
      | otherwise -> do
          case Map.lookup (gitTreeOid entry) blobs of
            Nothing -> pure (Left (RepositorySnapshotMissingBatchBlob (gitTreeOid entry)))
            Just blob -> do
              incrementHistoryCounter parseCount 1
              let parsed = parseSnapshotEntry (gitTreePath entry) (gitBlobBytes blob)
                  identity = maybe Nothing documentIdentity (either (const Nothing) Just (snapshotEntryDocument parsed))
              pure (Right (Just (HistoryBlob entry identity)))

replaceHistoryPath :: RepoPath -> Maybe HistoryPathState -> HistoryState -> HistoryState
replaceHistoryPath path replacement state =
  HistoryState nextPaths nextMembers nextNonblobs
  where
    previous = Map.lookup path (historyStatePaths state)
    pathsWithoutPrevious = Map.delete path (historyStatePaths state)
    membersWithoutPrevious = maybe (historyStateMembers state) (\value -> removeIdentity (historyIdentity value) (historyStateMembers state)) previous
    nonblobsWithoutPrevious = Map.delete path (historyStateNonblobs state)
    nextPaths = maybe pathsWithoutPrevious (\value -> Map.insert path value pathsWithoutPrevious) replacement
    nextMembers = maybe membersWithoutPrevious (\value -> addIdentity (historyIdentity value) membersWithoutPrevious) replacement
    nextNonblobs =
      case replacement of
        Just (HistoryNonblob entry) -> Map.insert path entry nonblobsWithoutPrevious
        _ -> nonblobsWithoutPrevious
    removeIdentity :: Maybe (OperationId, ObjectRef) -> Map OperationId (Map ObjectRef Int) -> Map OperationId (Map ObjectRef Int)
    removeIdentity Nothing members = members
    removeIdentity (Just (operation, objectId)) members =
      case Map.lookup operation members of
        Nothing -> members
        Just objects ->
          case Map.lookup objectId objects of
            Nothing -> members
            Just 1 ->
              let remainingObjects = Map.delete objectId objects
               in if Map.null remainingObjects
                    then Map.delete operation members
                    else Map.insert operation remainingObjects members
            Just count -> Map.insert operation (Map.insert objectId (count - 1) objects) members
    addIdentity :: Maybe (OperationId, ObjectRef) -> Map OperationId (Map ObjectRef Int) -> Map OperationId (Map ObjectRef Int)
    addIdentity Nothing members = members
    addIdentity (Just (operation, objectId)) members = Map.insertWith (Map.unionWith (+)) operation (Map.singleton objectId 1) members

historyIdentity :: HistoryPathState -> Maybe (OperationId, ObjectRef)
historyIdentity value = case value of
  HistoryBlob _ identity -> identity
  HistoryNonblob _ -> Nothing

documentIdentity :: ParsedManagedDocument -> Maybe (OperationId, ObjectRef)
documentIdentity document = Just (provenanceOperationId (parsedManagedCapsule document), managedDocumentObject document)

validateHistoryDelta :: HistoryState -> HistoryState -> [GitTreeChange] -> [IntegrityIssue]
validateHistoryDelta previous current changes =
  missingIssues <> rewriteIssues <> incompleteIssues
  where
    changedPaths = Set.fromList (map gitTreeChangePath changes)
    previousBlob path = case Map.lookup path (historyStatePaths previous) of Just value@(HistoryBlob _ _) -> Just value; _ -> Nothing
    currentBlob path = case Map.lookup path (historyStatePaths current) of Just value@(HistoryBlob _ _) -> Just value; _ -> Nothing
    missingIssues =
      [ historyIssue MissingHistoricalObject (historyObject oldEntry) (historyOperation oldEntry) (Just path) "immutable managed object is missing from the current snapshot"
        | path <- Set.toAscList changedPaths,
          Just oldEntry <- [previousBlob path],
          Nothing <- [currentBlob path]
      ]
    rewriteIssues =
      [ historyIssue AppendOnlyRewrite (historyObject oldEntry) (historyOperation oldEntry) (Just path) "immutable managed object bytes changed at an existing path"
        | path <- Set.toAscList changedPaths,
          Just oldEntry@(HistoryBlob oldTreeEntry _) <- [previousBlob path],
          Just (HistoryBlob newTreeEntry _) <- [currentBlob path],
          gitTreeOid oldTreeEntry /= gitTreeOid newTreeEntry
      ]
    affectedOperations =
      Set.toAscList
        ( previousChangedOperations
            `Set.union` currentReusedOperations
        )
    previousChangedOperations =
      Set.fromList
        [ operation
          | path <- Set.toAscList changedPaths,
            Just value <- [Map.lookup path (historyStatePaths previous)],
            Just (operation, _) <- [historyIdentity value]
        ]
    -- A changed path can introduce a member under an operation that was
    -- already live elsewhere.  The old path might be malformed (and therefore
    -- have no identity), so attribution must include this current operation
    -- when it has a previous membership to compare against.
    currentReusedOperations =
      Set.fromList
        [ operation
          | path <- Set.toAscList changedPaths,
            Just value <- [Map.lookup path (historyStatePaths current)],
            Just (operation, _) <- [historyIdentity value],
            Map.member operation (historyStateMembers previous)
        ]
    -- A malformed replacement has no durable semantic identity.  For the
    -- immediately adjacent valid-to-malformed comparison only, retain the old
    -- identity in the comparison member set so that the byte rewrite is not
    -- also reported as an incomplete operation.  Never write that fallback to
    -- the child state: a later delete or rewrite must not be attributed to the
    -- earlier valid document.
    comparisonCurrentMembers =
      foldl'
        addMalformedFallback
        (historyStateMembers current)
        (Set.toAscList changedPaths)
    addMalformedFallback members path =
      case (Map.lookup path (historyStatePaths previous), Map.lookup path (historyStatePaths current)) of
        (Just (HistoryBlob _ (Just identity)), Just (HistoryBlob _ Nothing)) -> addIdentity identity members
        _ -> members
    addIdentity (operation, objectId) members = Map.insertWith (Map.unionWith (+)) operation (Map.singleton objectId 1) members
    incompleteIssues =
      [ historyIssue IncompleteOperation Nothing (Just operation) Nothing ("operation " <> operationIdText operation <> " is missing or has changed immutable members")
        | operation <- affectedOperations,
          Map.lookup operation (historyStateMembers previous) /= Map.lookup operation comparisonCurrentMembers
      ]

historyObject :: HistoryPathState -> Maybe ObjectRef
historyObject value = snd <$> historyIdentity value

historyOperation :: HistoryPathState -> Maybe OperationId
historyOperation value = fst <$> historyIdentity value

historyIssue :: IntegrityIssueCode -> Maybe ObjectRef -> Maybe OperationId -> Maybe RepoPath -> Text -> IntegrityIssue
historyIssue code objectId operation path message =
  IntegrityIssue IntegrityError code objectId operation path message

remapDeletion :: Set RepoPath -> IntegrityIssue -> IntegrityIssue
remapDeletion targetPaths issueValue
  | integrityCode issueValue /= MissingHistoricalObject = issueValue
  | maybe False (`Set.member` targetPaths) (integrityPath issueValue) =
      issueValue
        { integrityCode = AppendOnlyDelete,
          integrityMessage = "immutable managed object was deleted on a reachable history edge"
        }
  | otherwise = issueValue

observeBasisDiagnostics :: RawRepositorySnapshotObservation -> [ParsedManagedDocument] -> IO (Either RepositorySnapshotError [CompilerDiagnostic])
observeBasisDiagnostics raw documents = do
  let revision = rawRepositorySnapshotRevision raw
      repository = resolvedRepository revision
      bases = Set.toAscList (Set.fromList (map (provenanceBasis . parsedManagedCapsule) documents))
  observed <- batchObjectInfo repository bases
  pure $ do
    infos <- first RepositorySnapshotGitError observed
    Right (concatMap (basisDiagnostic infos) bases)

basisDiagnostic :: Map GitOid (Maybe GitObjectInfo) -> GitOid -> [CompilerDiagnostic]
basisDiagnostic infos oid =
  case Map.lookup oid infos of
    Just (Just info) | objectInfoType info == GitCommitObject -> []
    _ ->
      [ diagnostic
          CompilerDiagnosticWarning
          CompilerBasisOrigin
          BasisCommitUnavailable
          Nothing
          Nothing
          Nothing
          (Just oid)
          Nothing
          "provenance basis is missing or is not a commit object"
      ]

configDiagnostics :: RawRepositoryConfigObservation -> [CompilerDiagnostic]
configDiagnostics config =
  case rawRepositoryConfigResult config of
    Right _ -> []
    Left failure ->
      [ diagnostic
          CompilerDiagnosticError
          CompilerConfigOrigin
          InvalidRepositoryConfig
          Nothing
          Nothing
          Nothing
          (configFailureOid failure)
          (Just configRepositoryPath)
          (configFailureMessage failure)
      ]

configFailureOid :: RepositoryConfigFailure -> Maybe GitOid
configFailureOid failure =
  case failure of
    RepositoryConfigFailureInvalidUtf8 oid -> Just oid
    RepositoryConfigFailureParse oid _ -> Just oid
    RepositoryConfigFailureNotBlob entry -> Just (gitTreeOid entry)

configFailureMessage :: RepositoryConfigFailure -> Text
configFailureMessage failure =
  case failure of
    RepositoryConfigFailureInvalidUtf8 _ -> "committed .adrai.toml is not valid UTF-8"
    RepositoryConfigFailureParse _ problem -> "committed .adrai.toml is invalid: " <> Text.pack (show problem)
    RepositoryConfigFailureNotBlob _ -> "committed .adrai.toml is not a blob"

nonblobDiagnostic :: CompilerDiagnosticOrigin -> Maybe GitOid -> RepositoryTreeObservation -> CompilerDiagnostic
nonblobDiagnostic origin commit observation =
  diagnostic
    CompilerDiagnosticError
    origin
    ManagedNonBlob
    Nothing
    Nothing
    Nothing
    commit
    (Just (gitTreePath entry))
    "selected managed source is not a blob"
  where
    entry = repositoryTreeEntry observation

integrityDiagnostic :: CompilerDiagnosticOrigin -> Maybe GitOid -> IntegrityIssue -> CompilerDiagnostic
integrityDiagnostic origin commit issueValue =
  CompilerDiagnostic
    { compilerDiagnosticSeverity =
        case integritySeverity issueValue of
          IntegrityError -> CompilerDiagnosticError
          IntegrityWarning -> CompilerDiagnosticWarning,
      compilerDiagnosticOrigin = origin,
      compilerDiagnosticCode = CompilerIntegrityCode (integrityCode issueValue),
      compilerDiagnosticAdr = Nothing,
      compilerDiagnosticObject = integrityObjectId issueValue,
      compilerDiagnosticOperation = integrityOperationId issueValue,
      compilerDiagnosticCommit = commit,
      compilerDiagnosticPath = integrityPath issueValue,
      compilerDiagnosticMessage = integrityMessage issueValue
    }

graphDiagnostic :: GraphIssue -> CompilerDiagnostic
graphDiagnostic issueValue =
  CompilerDiagnostic
    { compilerDiagnosticSeverity = CompilerDiagnosticError,
      compilerDiagnosticOrigin = CompilerGraphOrigin,
      compilerDiagnosticCode = CompilerGraphCode (graphIssueCode issueValue),
      compilerDiagnosticAdr = graphIssueAdr issueValue,
      compilerDiagnosticObject = graphIssueObject issueValue,
      compilerDiagnosticOperation = Nothing,
      compilerDiagnosticCommit = Nothing,
      compilerDiagnosticPath = Nothing,
      compilerDiagnosticMessage = graphIssueMessage issueValue
    }

diagnostic :: CompilerDiagnosticSeverity -> CompilerDiagnosticOrigin -> IntegrityIssueCode -> Maybe AdrId -> Maybe ObjectRef -> Maybe OperationId -> Maybe GitOid -> Maybe RepoPath -> Text -> CompilerDiagnostic
diagnostic severity origin code adr objectId operation commit path message =
  CompilerDiagnostic severity origin (CompilerIntegrityCode code) adr objectId operation commit path message

canonicalDiagnostics :: [CompilerDiagnostic] -> [CompilerDiagnostic]
canonicalDiagnostics = Map.elems . Map.fromList . map (\value -> (diagnosticKey value, value))

diagnosticKey :: CompilerDiagnostic -> (Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, CompilerDiagnosticSeverity, CompilerDiagnosticOrigin, Text)
diagnosticKey value =
  ( compilerDiagnosticCodeText (compilerDiagnosticCode value),
    adrIdText <$> compilerDiagnosticAdr value,
    objectRefText <$> compilerDiagnosticObject value,
    operationIdText <$> compilerDiagnosticOperation value,
    gitOidText <$> compilerDiagnosticCommit value,
    repoPathText <$> compilerDiagnosticPath value,
    compilerDiagnosticSeverity value,
    compilerDiagnosticOrigin value,
    compilerDiagnosticMessage value
  )

validateManagedOperations :: [ParsedManagedDocument] -> [CompilerDiagnostic]
validateManagedOperations documents =
  canonicalDiagnostics
    ( concatMap (uncurry (validateOperation roots)) (Map.toAscList grouped)
        <> amendmentPairDiagnostics documents
    )
  where
    grouped =
      Map.fromListWith (<>)
        [ (provenanceOperationId (parsedManagedCapsule document), [document])
          | document <- documents
        ]
    roots =
      Map.fromListWith min
        [ (decisionAdr decision, decisionRecord decision)
          | document <- documents,
            ManagedDecision decision <- [parsedManagedRecord document],
            null (provenanceParents (parsedManagedCapsule document))
        ]

validateOperation :: Map AdrId RecordId -> OperationId -> [ParsedManagedDocument] -> [CompilerDiagnostic]
validateOperation roots operation documents =
  contextProblems <> adrProblems <> shapeProblems
  where
    ordered = sortOn (repoPathText . parsedManagedPath) documents
    contexts = map (provenanceOperationContext . parsedManagedCapsule) ordered
    adrs = Set.fromList (map (managedRecordAdr . parsedManagedRecord) ordered)
    contextProblems =
      [ operationDiagnostic InconsistentOperationCapsule operation Nothing Nothing "operation members have inconsistent capsule context"
        | not (allSame contexts)
      ]
    adrProblems =
      [ operationDiagnostic CrossAdrOperation operation Nothing Nothing "operation members span more than one ADR"
        | Set.size adrs > 1
      ]
    shapeProblems = validateOperationShape roots operation ordered

validateOperationShape :: Map AdrId RecordId -> OperationId -> [ParsedManagedDocument] -> [CompilerDiagnostic]
validateOperationShape roots operation documents
  | isCreateCandidate = validateCreate operation decisions connections
  | isAmendmentCandidate = validateAmendment operation decisions connections
  | null decisions,
    [connection] <- connections = validateAxisChange roots operation connection
  | otherwise = [operationDiagnostic UnknownOperationShape operation Nothing Nothing "operation has an unsupported member shape"]
  where
    decisions = [document | document <- documents, ManagedDecision _ <- [parsedManagedRecord document]]
    connections = [document | document <- documents, ManagedConnection _ <- [parsedManagedRecord document]]
    connectionPayloads = [connectionPayload connection | document <- connections, ManagedConnection connection <- [parsedManagedRecord document]]
    isCreateCandidate =
      length decisions == 1
        && length [() | AppliesToConnection _ <- connectionPayloads] == 1
        && length [() | DomainsConnection _ <- connectionPayloads] == 1
        && length [() | StatusConnection _ <- connectionPayloads] == 1
    isAmendmentCandidate =
      not (null [() | AmendsConnection _ <- connectionPayloads])
        || case decisions of
          [document] -> eventKindText (provenanceEventKind (parsedManagedCapsule document)) == "decision.amend"
          _ -> False

validateCreate :: OperationId -> [ParsedManagedDocument] -> [ParsedManagedDocument] -> [CompilerDiagnostic]
validateCreate operation decisions connections =
  cardinality <> eventProblems <> parentProblems <> initialParentProblems <> amendmentProblems
  where
    payloads = [connectionPayload connection | document <- connections, ManagedConnection connection <- [parsedManagedRecord document]]
    scopes = [document | document <- connections, ManagedConnection connection <- [parsedManagedRecord document], AppliesToConnection payload <- [connectionPayload connection], null (appliesToParentConnections payload)]
    domains = [document | document <- connections, ManagedConnection connection <- [parsedManagedRecord document], DomainsConnection payload <- [connectionPayload connection], null (domainsParentConnections payload)]
    statuses = [document | document <- connections, ManagedConnection connection <- [parsedManagedRecord document], StatusConnection payload <- [connectionPayload connection], null (statusParentConnections payload)]
    amendments = [() | AmendsConnection _ <- payloads]
    cardinality =
      [ operationDiagnostic UnknownOperationShape operation Nothing Nothing "create operation must contain one decision and one initial scope, domain, and status member"
        | length decisions /= 1 || length connections /= 3 || length scopes /= 1 || length domains /= 1 || length statuses /= 1
      ]
    eventProblems =
      concat
        [ expectEvent UnknownOperationShape operation "decision.create" document | document <- decisions ]
        <> concat [expectEvent ScopeEventKindMismatch operation "scope.initial" document | document <- scopes]
        <> concat [expectEvent DomainEventKindMismatch operation "domain.initial" document | document <- domains]
        <> concat [expectEvent StatusEventKindMismatch operation "status.initial" document | document <- statuses]
    parentProblems =
      [ operationDiagnostic CreateProvenanceHasParents operation (Just (managedDocumentObject document)) (Just (parsedManagedPath document)) "create decision has provenance parents"
        | document <- decisions,
          not (null (provenanceParents (parsedManagedCapsule document)))
      ]
    initialParentProblems =
      case decisions of
        [decisionDocument] ->
          case parsedManagedRecord decisionDocument of
            ManagedDecision decision ->
              let expected = [ProvenanceRecord (decisionRecord decision)]
               in [ operationDiagnostic ProvenanceParentMismatch operation (Just (managedDocumentObject document)) (Just (parsedManagedPath document)) "initial axis member does not point to the creation record"
                    | document <- scopes <> domains <> statuses,
                      provenanceParents (parsedManagedCapsule document) /= expected
                  ]
            _ -> []
        _ -> []
    amendmentProblems =
      [ operationDiagnostic CreateHasAmendmentEdge operation Nothing Nothing "create operation contains an amendment edge"
        | not (null amendments)
      ]

validateAmendment :: OperationId -> [ParsedManagedDocument] -> [ParsedManagedDocument] -> [CompilerDiagnostic]
validateAmendment operation decisions connections =
  cardinality <> eventProblems <> parentProblems
  where
    amendmentDocuments = [document | document <- connections, ManagedConnection connection <- [parsedManagedRecord document], AmendsConnection _ <- [connectionPayload connection]]
    cardinality =
      [ operationDiagnostic AmendmentEdgeCardinality operation Nothing Nothing "amendment operation must contain one decision and exactly one amendment edge"
        | length decisions /= 1 || length amendmentDocuments /= 1 || length connections /= 1
      ]
    eventProblems =
      concat [expectEvent AmendmentOperationMismatch operation "decision.amend" document | document <- decisions]
        <> concat [expectEvent AmendmentOperationMismatch operation "connection.amends" document | document <- amendmentDocuments]
    parentProblems =
      case (decisions, amendmentDocuments) of
        ([decisionDocument], [edgeDocument]) ->
          case parsedManagedRecord edgeDocument of
            ManagedConnection connection ->
              case connectionPayload connection of
                AmendsConnection payload ->
                  let expected = map ProvenanceRecord (amendsToRecords payload)
                      actualDecision = provenanceParents (parsedManagedCapsule decisionDocument)
                      actualEdge = provenanceParents (parsedManagedCapsule edgeDocument)
                   in [ operationDiagnostic ProvenanceParentMismatch operation Nothing Nothing "amendment member parents do not match to_records"
                        | actualDecision /= expected || actualEdge /= expected
                      ]
                _ -> []
            _ -> []
        _ -> []

validateAxisChange :: Map AdrId RecordId -> OperationId -> ParsedManagedDocument -> [CompilerDiagnostic]
validateAxisChange roots operation document =
  case parsedManagedRecord document of
    ManagedConnection connection ->
      case connectionPayload connection of
        AppliesToConnection payload ->
          expectEvent ScopeEventKindMismatch operation ("scope." <> appliesToChange payload) document
            <> expectParents (scopeParents roots payload) ScopeEventKindMismatch
        DomainsConnection payload ->
          expectEvent DomainEventKindMismatch operation ("domain." <> domainsChange payload) document
            <> expectParents (domainParents roots payload) DomainEventKindMismatch
        StatusConnection payload ->
          expectEvent StatusEventKindMismatch operation (statusEvent payload) document
            <> expectParents (statusParents payload) StatusEventKindMismatch
        AmendsConnection _ -> [operationDiagnostic AmendmentEdgeCardinality operation Nothing (Just (parsedManagedPath document)) "amendment edge has no decision member"]
    _ -> [operationDiagnostic UnknownOperationShape operation Nothing (Just (parsedManagedPath document)) "axis operation contains a decision"]
  where
    expectParents expected code =
      [ operationDiagnostic ProvenanceParentMismatch operation (Just (managedDocumentObject document)) (Just (parsedManagedPath document)) (integrityIssueCodeText code <> " provenance parents do not match payload")
        | provenanceParents (parsedManagedCapsule document) /= expected
      ]

scopeParents :: Map AdrId RecordId -> AppliesToPayload -> [ProvenanceObjectId]
scopeParents roots payload
  | null (appliesToParentConnections payload) = maybe [] (pure . ProvenanceRecord) (Map.lookup (appliesToSubjectAdr payload) roots)
  | otherwise = map ProvenanceConnection (appliesToParentConnections payload)

domainParents :: Map AdrId RecordId -> DomainsPayload -> [ProvenanceObjectId]
domainParents roots payload
  | null (domainsParentConnections payload) = maybe [] (pure . ProvenanceRecord) (Map.lookup (domainsSubjectAdr payload) roots)
  | otherwise = map ProvenanceConnection (domainsParentConnections payload)

statusParents :: StatusPayload -> [ProvenanceObjectId]
statusParents payload =
  map ProvenanceConnection (statusParentConnections payload)
    <> map ProvenanceRecord (statusRecordHeads payload)

statusEvent :: StatusPayload -> Text
statusEvent payload
  | null (statusParentConnections payload) = "status.initial"
  | statusState payload == StatusObsolete = "decision.obsolete"
  | otherwise = "decision.reactivate"

expectEvent :: IntegrityIssueCode -> OperationId -> Text -> ParsedManagedDocument -> [CompilerDiagnostic]
expectEvent code operation expected document =
  [ operationDiagnostic code operation (Just (managedDocumentObject document)) (Just (parsedManagedPath document)) ("expected event " <> expected <> ", got " <> actual)
    | actual /= expected
  ]
  where
    actual = eventKindText (provenanceEventKind (parsedManagedCapsule document))

amendmentPairDiagnostics :: [ParsedManagedDocument] -> [CompilerDiagnostic]
amendmentPairDiagnostics documents =
  concatMap checkEdge amendmentEdges
  where
    decisionsByRecord =
      Map.fromList
        [ (decisionRecord decision, document)
          | document <- documents,
            ManagedDecision decision <- [parsedManagedRecord document]
        ]
    amendmentEdges =
      [ (document, payload)
        | document <- documents,
          ManagedConnection connection <- [parsedManagedRecord document],
          AmendsConnection payload <- [connectionPayload connection]
      ]
    checkEdge (edgeDocument, payload) =
      case Map.lookup (amendsFromRecord payload) decisionsByRecord of
        Nothing -> []
        Just decisionDocument ->
          let edgeOperation = provenanceOperationId (parsedManagedCapsule edgeDocument)
              decisionOperation = provenanceOperationId (parsedManagedCapsule decisionDocument)
           in [ operationDiagnostic AmendmentOperationMismatch edgeOperation (Just (managedDocumentObject edgeDocument)) (Just (parsedManagedPath edgeDocument)) "amendment edge and amended decision use different operations"
                | edgeOperation /= decisionOperation
              ]

operationDiagnostic :: IntegrityIssueCode -> OperationId -> Maybe ObjectRef -> Maybe RepoPath -> Text -> CompilerDiagnostic
operationDiagnostic code operation objectId path message =
  diagnostic CompilerDiagnosticError CompilerOperationOrigin code Nothing objectId (Just operation) Nothing path message

managedDocumentObject :: ParsedManagedDocument -> ObjectRef
managedDocumentObject document =
  case parsedManagedRecord document of
    ManagedDecision decision -> recordObjectRef (decisionRecord decision)
    ManagedConnection connection -> connectionObjectRef (connectionRecordId connection)

managedRecordAdr :: ManagedRecord -> AdrId
managedRecordAdr record =
  case record of
    ManagedDecision decision -> decisionAdr decision
    ManagedConnection connection ->
      case connectionPayload connection of
        AmendsConnection payload -> amendsSubjectAdr payload
        AppliesToConnection payload -> appliesToSubjectAdr payload
        DomainsConnection payload -> domainsSubjectAdr payload
        StatusConnection payload -> statusSubjectAdr payload

allSame :: (Eq value) => [value] -> Bool
allSame [] = True
allSame (firstValue : remaining) = all (== firstValue) remaining

-- | Preserve the first observed edge as the node's published state, while
-- exposing only the comparisons needed to prove that further parent edges
-- reconstruct the same child.  An empty delta group has no primary state.
historyConvergencePairs :: [value] -> Maybe (value, [(value, value)])
historyConvergencePairs [] = Nothing
historyConvergencePairs (primary : additional) = Just (primary, map (\additionalState -> (primary, additionalState)) additional)

sourceFingerprint :: RawRepositorySnapshotObservation -> Digest
-- Streaming SHA-256 over the exact framed sequence the entryFields-based call
-- produced (header frame, config frames, then per-observation entry + blob
-- frames in repoPathText order): identical persisted fingerprint without ever
-- retaining a whole-corpus frame list or concatenating blob bytes.
sourceFingerprint raw = sha256FrameStateFinalize stateAfterEntries
  where
    config = rawRepositorySnapshotConfig raw
    stateAfterHeader = sha256FrameStateFeed sha256FrameStateInit "adrai-source/1\NUL"
    stateAfterConfig = foldl' sha256FrameStateFeed stateAfterHeader configFields
    stateAfterEntries = foldl' feedEntryState stateAfterConfig sortedObservations
    configFields =
      [ framed (TextEncoding.encodeUtf8 (configOriginText (rawRepositoryConfigOrigin config))),
        maybe (framed "default") (framed . treeEntryText) (rawRepositoryConfigEntry config),
        maybe (framed "") (framed . gitBlobBytes) (rawRepositoryConfigBlob config),
        maybe (framed "unavailable") (framed . managedPathsText) (rawRepositoryConfigManagedPaths config)
      ]
    sortedObservations =
      sortOn (repoPathText . gitTreePath . repositoryTreeEntry) (rawRepositorySnapshotEntries raw)
    feedEntryState state observation =
      let stateAfterEntry = sha256FrameStateFeed state (framed (treeEntryText (repositoryTreeEntry observation)))
          stateAfterBlob =
            case repositoryTreeBlob observation of
              Nothing -> sha256FrameStateFeed stateAfterEntry (framed "")
              Just blob -> sha256FrameStateFeed stateAfterEntry (framed (gitBlobBytes blob))
      in stateAfterBlob

framed :: ByteString -> ByteString
framed bytes = TextEncoding.encodeUtf8 (Text.pack (show (BS.length bytes)) <> ":") <> bytes <> "\NUL"

configOriginText :: RepositoryConfigOrigin -> Text
configOriginText DefaultConfigOrigin = "default"
configOriginText CommittedConfigOrigin = "committed"

managedPathsText :: ManagedPaths -> ByteString
managedPathsText paths =
  TextEncoding.encodeUtf8 (repoPathText (managedDecisionPath paths) <> "\NUL" <> repoPathText (managedConnectionPath paths))

treeEntryText :: GitTreeEntry -> ByteString
treeEntryText entry =
  TextEncoding.encodeUtf8
    ( Text.intercalate
        "\NUL"
        [ repoPathText (gitTreePath entry),
          fileModeText (gitTreeMode entry),
          objectTypeText (gitTreeObjectType entry),
          gitOidText (gitTreeOid entry)
        ]
    )

fileModeText :: GitFileMode -> Text
fileModeText mode =
  case mode of
    GitRegularFile -> "100644"
    GitExecutableFile -> "100755"
    GitSymbolicLink -> "120000"
    GitSubmodule -> "160000"
    GitDirectory -> "040000"

objectTypeText :: GitObjectType -> Text
objectTypeText objectType =
  case objectType of
    GitBlobObject -> "blob"
    GitTreeObject -> "tree"
    GitCommitObject -> "commit"
    GitTagObject -> "tag"

configRepositoryPath :: RepoPath
configRepositoryPath =
  case mkRepoPath ".adrai.toml" of
    Right path -> path
    Left problem -> error ("invalid built-in config path: " <> show problem)
