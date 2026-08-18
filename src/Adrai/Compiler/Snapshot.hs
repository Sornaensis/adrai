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
    analyzedSourceFingerprint,
    ParsedReducedRepositorySnapshot,
    parsedReducedAnalyzed,
    parsedReducedDocuments,
    parsedReducedReduction,
    parsedReducedConflicts,
    analyzeRepositorySnapshot,
    gateAnalyzedRepositorySnapshot,
    sourceFingerprint,
    validateManagedOperations,
  )
where

import Adrai.Format.Document
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
analyzeRepositorySnapshot raw =
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
      historyResult <- observeHistory raw paths entries
      case historyResult of
        Left problem -> pure (Left problem)
        Right (historyComplete, historyDiagnostics) -> do
          basisResult <- observeBasisDiagnostics raw uniqueDocuments
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
              )

assembleAnalysis :: RawRepositorySnapshotObservation -> [ManagedSnapshotEntry] -> [RepositoryTreeObservation] -> [CompilerDiagnostic] -> [ParsedManagedDocument] -> GraphReduction -> [AdrConflict] -> Bool -> AnalyzedRepositorySnapshot
assembleAnalysis raw entries nonblobs diagnostics documents reduction conflicts historyComplete =
  AnalyzedRepositorySnapshot
    { analyzedRawObservation = rawWithoutBlobs,
      analyzedManagedEntries = entries,
      analyzedNonblobObservations = nonblobs,
      analyzedDiagnostics = canonicalDiagnostics diagnostics,
      analyzedDocuments = documents,
      analyzedReduction = reduction,
      analyzedConflicts = conflicts,
      analyzedHistoryComplete = historyComplete,
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

observeHistory :: RawRepositorySnapshotObservation -> ManagedPaths -> [ManagedSnapshotEntry] -> IO (Either RepositorySnapshotError (Bool, [CompilerDiagnostic]))
observeHistory raw paths targetEntries = do
  let revision = rawRepositorySnapshotRevision raw
      repository = resolvedRepository revision
      targetOid = resolvedCommitOid revision
  shallowResult <- isShallowRepository repository
  graphResult <- reachableCommitGraphAt repository targetOid
  case (shallowResult, graphResult) of
    (Left problem, _) -> pure (Left (RepositorySnapshotGitError problem))
    (_, Left problem) -> pure (Left (RepositorySnapshotGitError problem))
    (Right shallow, Right nodes) -> do
      let targetPaths =
            Set.fromList
              (map (gitTreePath . repositoryTreeEntry) (rawRepositorySnapshotEntries raw))
      treeResult <- observeCommitTrees revision paths targetOid targetPaths targetEntries nodes
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
        Right (not shallow, coverageDiagnostics <> historyDiagnostics)

-- | Walk the reachable commit list in rev-list --topo-order --reverse order
-- (parents always precede children).  Each commit's tree is inserted just
-- before the commit itself is validated, and it is evicted as soon as no
-- remaining commit references it as a parent, so retained tree data scales
-- with the fan-in window rather than the whole reachable history.  The
-- accumulated diagnostics later pass through canonicalDiagnostics, whose
-- key-ordered output makes the final result independent of this traversal
-- order.
observeCommitTrees :: ResolvedRepositoryRevision -> ManagedPaths -> GitOid -> Set RepoPath -> [ManagedSnapshotEntry] -> [GitCommitNode] -> IO (Either RepositorySnapshotError [CompilerDiagnostic])
observeCommitTrees revision paths targetOid targetPaths targetEntries nodes =
  go Map.empty remainingChildren [] nodes
  where
    -- Remaining-children refcount per parent oid over the whole node list:
    -- how many reachable commits still reference this parent's tree.
    remainingChildren :: Map GitOid Int
    remainingChildren =
      Map.fromListWith (+)
        [ (parentOid, 1)
          | node <- nodes,
            parentOid <- gitCommitNodeParents node
        ]
    go _ _ diagnostics [] = pure (Right diagnostics)
    go trees refcounts diagnostics (node : remaining)
      | gitCommitNodeOid node == targetOid = do
          let trees' = Map.insert targetOid targetEntries trees
          retireNode trees' refcounts diagnostics node [] remaining
      | otherwise = do
          observed <- observeManagedTreeAt revision (gitCommitNodeOid node) paths
          case observed of
            Left problem -> pure (Left problem)
            Right observations -> do
              let (entries, nonblobs) = partitionObservations observations
              let trees' = Map.insert (gitCommitNodeOid node) entries trees
              retireNode trees' refcounts diagnostics node nonblobs remaining
    -- Validate this node against the live tree map (its own tree is already
    -- inserted), accumulate its diagnostics, then decrement the remaining
    -- refcounts of its parents and evict any parent whose count reached zero.
    retireNode trees refcounts diagnostics node nonblobs remaining =
      go trees' refcounts' (diagnostics <> nodeDiagnostics) remaining
      where
        nodeOid = gitCommitNodeOid node
        nodeDiagnostics =
          edgeIssues targetPaths trees node
            <> map (nonblobDiagnostic CompilerHistoryOrigin (Just nodeOid)) nonblobs
        (refcounts', trees') =
          foldr'
            ( \parentOid (counts, current) ->
                case Map.lookup parentOid counts of
                  Nothing -> (counts, current)
                  Just 1 -> (Map.delete parentOid counts, Map.delete parentOid current)
                  Just count -> (Map.insert parentOid (count - 1) counts, current)
            )
            (refcounts, trees)
            (gitCommitNodeParents node)

edgeIssues :: Set RepoPath -> Map GitOid [ManagedSnapshotEntry] -> GitCommitNode -> [CompilerDiagnostic]
edgeIssues targetPaths trees child =
  concatMap validateParent (gitCommitNodeParents child)
  where
    validateParent parentOid =
      case (Map.lookup parentOid trees, Map.lookup (gitCommitNodeOid child) trees) of
        (Just parentEntries, Just childEntries) ->
          map
            (integrityDiagnostic CompilerHistoryOrigin (Just (gitCommitNodeOid child)) . remapDeletion targetPaths)
            (validateAppendOnlyDelta parentEntries childEntries)
        _ -> []

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
