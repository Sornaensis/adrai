{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Adrai.Sqlite
  ( FtsTarget (..),
    ftsTargetName,
    ftsTargetTable,
    ftsTargetColumns,
    ftsTargetTokenizer,
    ftsTargetBm25Weights,
    ftsTargetDdl,
    allFtsTargets,
    asQuery,
    RetrievalSqlError (..),
    retrievalSqlErrorToAdraiError,
    CandidateLimit,
    mkCandidateLimit,
    candidateLimitValue,
    candidateCap,
    FtsHit (..),
    SummaryFtsCandidates (..),
    PassageFtsCandidates (..),
    SearchStorageComponent (..),
    SearchStorageError (..),
    searchStorageErrorToAdraiError,
    searchOrdinarySchemaDdl,
    initializeFtsTargets,
    initializeSearchSchema,
    replaceSearchMaterialization,
    coldSchemaDdl,
    ColdDatabaseError (..),
    ColdDatabaseStats (..),
    writeColdDatabase,
    loadLocalAliases,
    runFtsTarget,
    runSummaryFtsChannels,
    runPassageFtsChannels,
  )
where

import Adrai.Retrieval
  ( LocalAlias,
    QueryPlan (..),
    SearchDocument (..),
    SearchMaterialization (..),
    SearchPassage (..),
    sectionKindName,
    materializationImplementationFingerprint,
  )
import Adrai.Compiler.Snapshot
import Adrai.Domain (domainRefinementText, domainText)
import Adrai.Format.Document
import Adrai.Format.Json (JsonValue (..), object, renderCanonicalJson)
import Adrai.Git
import Adrai.Graph
import Adrai.Integrity (snapshotEntryDocument, snapshotEntryPath)
import Adrai.Provenance
import Adrai.Repository
import Adrai.Scope (scopePatternText)
import Adrai.Types
import Control.Exception (Exception, SomeException, catch, displayException, fromException, throwIO, try)
import Control.Monad (forM_)
import Data.Int (Int64)
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Ord (Down (..), comparing)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple
  ( Connection,
    FromRow (fromRow),
    Query,
    SQLError (..),
    SQLData (SQLBlob, SQLFloat, SQLInteger, SQLNull, SQLText),
    Only (..),
    execute,
    execute_,
    field,
    query,
    query_,
    withTransaction,
  )

data FtsTarget
  = SearchExactTarget
  | SearchStemmedTarget
  | SearchIdentifierTarget
  | PassageExactTarget
  | PassageStemmedTarget
  | PassageIdentifierTarget
  deriving (Eq, Ord, Show, Enum, Bounded)

allFtsTargets :: [FtsTarget]
allFtsTargets = [minBound .. maxBound]

ftsTargetName :: FtsTarget -> Text
ftsTargetName SearchExactTarget = "search-exact"
ftsTargetName SearchStemmedTarget = "search-stemmed"
ftsTargetName SearchIdentifierTarget = "search-identifier"
ftsTargetName PassageExactTarget = "passage-exact"
ftsTargetName PassageStemmedTarget = "passage-stemmed"
ftsTargetName PassageIdentifierTarget = "passage-identifier"

ftsTargetTable :: FtsTarget -> Text
ftsTargetTable SearchExactTarget = "fts_search_exact"
ftsTargetTable SearchStemmedTarget = "fts_search_stemmed"
ftsTargetTable SearchIdentifierTarget = "fts_search_identifier"
ftsTargetTable PassageExactTarget = "fts_passage_exact"
ftsTargetTable PassageStemmedTarget = "fts_passage_stemmed"
ftsTargetTable PassageIdentifierTarget = "fts_passage_identifier"

ftsTargetColumns :: FtsTarget -> [(Text, Bool)]
ftsTargetColumns SearchExactTarget =
  unindexedSearch <> indexed ["title", "summary", "decision", "domains", "rationale", "context", "consequences", "identifiers"]
ftsTargetColumns SearchStemmedTarget =
  unindexedSearch <> indexed ["title", "summary", "decision", "rationale", "context", "consequences"]
ftsTargetColumns SearchIdentifierTarget = unindexedSearch <> indexed ["identifiers"]
ftsTargetColumns PassageExactTarget = unindexedPassage <> indexed ["text", "identifiers"]
ftsTargetColumns PassageStemmedTarget = unindexedPassage <> indexed ["text"]
ftsTargetColumns PassageIdentifierTarget = unindexedPassage <> indexed ["identifiers"]

unindexedSearch :: [(Text, Bool)]
unindexedSearch = [("item_id", False), ("adr_id", False), ("candidate_record_id", False)]

unindexedPassage :: [(Text, Bool)]
unindexedPassage = unindexedSearch <> [("section_kind", False)]

indexed :: [Text] -> [(Text, Bool)]
indexed = map (,True)

ftsTargetTokenizer :: FtsTarget -> Text
ftsTargetTokenizer SearchStemmedTarget = "porter unicode61"
ftsTargetTokenizer PassageStemmedTarget = "porter unicode61"
ftsTargetTokenizer _ = "unicode61"

ftsTargetBm25Weights :: FtsTarget -> [Double]
ftsTargetBm25Weights SearchExactTarget = [0, 0, 0, 9, 7, 5.5, 2, 3, 1.5, 1, 4.5]
ftsTargetBm25Weights SearchStemmedTarget = [0, 0, 0, 8, 6, 5, 3, 1.5, 1]
ftsTargetBm25Weights SearchIdentifierTarget = [0, 0, 0, 8]
ftsTargetBm25Weights PassageExactTarget = [0, 0, 0, 0, 4, 3]
ftsTargetBm25Weights PassageStemmedTarget = [0, 0, 0, 0, 4]
ftsTargetBm25Weights PassageIdentifierTarget = [0, 0, 0, 0, 6]

ftsTargetDdl :: FtsTarget -> Text
ftsTargetDdl target =
  "CREATE VIRTUAL TABLE "
    <> ftsTargetTable target
    <> " USING fts5("
    <> Text.intercalate "," [name <> if isIndexed then "" else " UNINDEXED" | (name, isIndexed) <- ftsTargetColumns target]
    <> ",tokenize='"
    <> ftsTargetTokenizer target
    <> "')"

data RetrievalSqlError
  = InvalidCandidateLimit Integer
  | CandidateLimitOverflow Integer
  | MalformedFtsQuery FtsTarget
  | RetrievalIndexError FtsTarget
  deriving (Eq, Show)

retrievalSqlErrorToAdraiError :: RetrievalSqlError -> AdraiError
retrievalSqlErrorToAdraiError retrievalError =
  AdraiError
    { adraiErrorClass = ExitUserError,
      adraiErrorMessage = case retrievalError of
        InvalidCandidateLimit _ -> "Search candidate limit must be positive."
        CandidateLimitOverflow _ -> "Search candidate limit is too large."
        MalformedFtsQuery target -> "Search query could not be parsed for " <> ftsTargetName target <> "."
        RetrievalIndexError target -> "Search index is unavailable for " <> ftsTargetName target <> "."
    }

newtype CandidateLimit = CandidateLimit Int64
  deriving (Eq, Ord, Show)

mkCandidateLimit :: Integer -> Either RetrievalSqlError CandidateLimit
mkCandidateLimit value
  | value <= 0 = Left (InvalidCandidateLimit value)
  | value > toInteger (maxBound :: Int64) = Left (CandidateLimitOverflow value)
  | otherwise = Right (CandidateLimit (fromInteger value))

candidateLimitValue :: CandidateLimit -> Int64
candidateLimitValue (CandidateLimit value) = value

candidateCap :: CandidateLimit -> Either RetrievalSqlError CandidateLimit
candidateCap limit = mkCandidateLimit (max (toInteger (candidateLimitValue limit) * 30) 240)

data FtsHit = FtsHit
  { ftsHitItemId :: Text,
    ftsHitScore :: Double
  }
  deriving (Eq, Show)

instance FromRow FtsHit where
  fromRow = FtsHit <$> field <*> field

data SummaryFtsCandidates = SummaryFtsCandidates
  { summaryFtsPhrase :: [FtsHit],
    summaryFtsTerms :: [FtsHit],
    summaryFtsStemmed :: [FtsHit],
    summaryFtsIdentifier :: [FtsHit],
    summaryFtsPrefixUsed :: Bool
  }
  deriving (Eq, Show)

data PassageFtsCandidates = PassageFtsCandidates
  { passageFtsExact :: [FtsHit],
    passageFtsStemmed :: [FtsHit],
    passageFtsIdentifier :: [FtsHit]
  }
  deriving (Eq, Show)

data SearchStorageComponent
  = SearchSchemaStorage Text
  | SearchDocumentStorage
  | SearchAliasStorage
  | SearchPassageStorage
  | SearchFtsStorage FtsTarget
  deriving (Eq, Show)

data SearchStorageError = SearchStorageError SearchStorageComponent
  deriving (Eq, Show)

searchStorageErrorToAdraiError :: SearchStorageError -> AdraiError
searchStorageErrorToAdraiError (SearchStorageError component) =
  AdraiError ExitUserError ("Search materialization storage failed for " <> storageComponentName component <> ".")

storageComponentName :: SearchStorageComponent -> Text
storageComponentName component = case component of
  SearchSchemaStorage name -> "schema " <> name
  SearchDocumentStorage -> "search documents"
  SearchAliasStorage -> "local aliases"
  SearchPassageStorage -> "search passages"
  SearchFtsStorage target -> ftsTargetName target

searchOrdinarySchemaDdl :: [(SearchStorageComponent, Text)]
searchOrdinarySchemaDdl =
  [ ( SearchSchemaStorage "search_document",
      "CREATE TABLE search_document("
        <> "item_id TEXT PRIMARY KEY,"
        <> "adr_id TEXT NOT NULL,"
        <> "candidate_record_id TEXT NOT NULL,"
        <> "title TEXT NOT NULL,summary TEXT NOT NULL,context TEXT NOT NULL,decision TEXT NOT NULL,"
        <> "consequences TEXT NOT NULL,domains TEXT NOT NULL,rationale TEXT NOT NULL,identifiers TEXT NOT NULL,"
        <> "other TEXT NOT NULL,scope TEXT NOT NULL,source_paths TEXT NOT NULL,"
        <> "obsolete INTEGER NOT NULL CHECK(obsolete IN (0,1)),"
        <> "conflicted INTEGER NOT NULL CHECK(conflicted IN (0,1)),state_token TEXT NOT NULL)"
    ),
    ( SearchSchemaStorage "local_alias",
      "CREATE TABLE local_alias(alias TEXT PRIMARY KEY,expansion TEXT NOT NULL)"
    ),
    ( SearchSchemaStorage "search_section",
      "CREATE TABLE search_section("
        <> "passage_rowid INTEGER PRIMARY KEY,"
        <> "item_id TEXT NOT NULL UNIQUE,"
        <> "search_item_id TEXT NOT NULL REFERENCES search_document(item_id) ON DELETE CASCADE,"
        <> "adr_id TEXT NOT NULL,candidate_record_id TEXT NOT NULL,section_kind TEXT NOT NULL,"
        <> "ordinal INTEGER NOT NULL CHECK(ordinal>=0),line_start INTEGER NOT NULL CHECK(line_start>=1),"
        <> "line_end INTEGER NOT NULL CHECK(line_end>=line_start),text TEXT NOT NULL,weight REAL NOT NULL,"
        <> "source_paths TEXT NOT NULL,identifiers TEXT NOT NULL,"
        <> "UNIQUE(search_item_id,section_kind,ordinal,line_start,line_end))"
    )
  ]

-- | Canonical declared schema owned by the cold compiler. FTS shadow tables
-- are intentionally excluded; they are SQLite implementation details.
coldSchemaDdl :: [(Text, Text)]
coldSchemaDdl =
  [ ("meta", "CREATE TABLE meta(key TEXT PRIMARY KEY,value TEXT NOT NULL)"),
    ( "repository_config",
      "CREATE TABLE repository_config("
        <> "singleton INTEGER PRIMARY KEY CHECK(singleton=1),origin TEXT NOT NULL,path TEXT NOT NULL,"
        <> "oid TEXT,object_type TEXT,mode TEXT,bytes BLOB,parse_state TEXT NOT NULL,"
        <> "decision_root TEXT,connection_root TEXT)"
    ),
    ( "managed_source",
      "CREATE TABLE managed_source("
        <> "path TEXT PRIMARY KEY,oid TEXT NOT NULL,object_type TEXT NOT NULL,mode TEXT NOT NULL,"
        <> "bytes BLOB,parse_state TEXT NOT NULL)"
    ),
    ( "issue",
      "CREATE TABLE issue("
        <> "ordinal INTEGER PRIMARY KEY CHECK(ordinal>=0),code TEXT NOT NULL,severity TEXT NOT NULL,origin TEXT NOT NULL,"
        <> "adr_id TEXT,object_id TEXT,operation_id TEXT,commit_oid TEXT,path TEXT,message TEXT NOT NULL)"
    ),
    ( "adr_conflict",
      "CREATE TABLE adr_conflict("
        <> "adr_id TEXT PRIMARY KEY,code TEXT NOT NULL,candidate_count INTEGER NOT NULL CHECK(candidate_count>=1),"
        <> "state_token TEXT NOT NULL,summaries TEXT NOT NULL)"
    ),
    ( "operation",
      "CREATE TABLE operation("
        <> "operation_id TEXT PRIMARY KEY,timestamp_ms TEXT NOT NULL CHECK(timestamp_ms GLOB '[1-9]*' AND timestamp_ms NOT GLOB '*[^0-9]*'),actor_kind TEXT NOT NULL,actor_id TEXT NOT NULL,"
        <> "actor_model TEXT,basis_oid TEXT NOT NULL,branch_hint TEXT,upstream_hint TEXT,line_anchors TEXT NOT NULL,"
        <> "tool_version TEXT NOT NULL,input_digest TEXT,prompt_digest TEXT,context_digest TEXT)"
    ),
    ( "operation_member",
      "CREATE TABLE operation_member("
        <> "operation_id TEXT NOT NULL REFERENCES operation(operation_id) ON DELETE CASCADE,"
        <> "object_id TEXT NOT NULL,object_type TEXT NOT NULL,event_kind TEXT NOT NULL,semantic_digest TEXT NOT NULL,path TEXT NOT NULL,"
        <> "PRIMARY KEY(operation_id,object_id))"
    ),
    ( "operation_member_parent",
      "CREATE TABLE operation_member_parent("
        <> "operation_id TEXT NOT NULL,object_id TEXT NOT NULL,ordinal INTEGER NOT NULL CHECK(ordinal>=0),parent_object_id TEXT NOT NULL,"
        <> "PRIMARY KEY(operation_id,object_id,ordinal),"
        <> "FOREIGN KEY(operation_id,object_id) REFERENCES operation_member(operation_id,object_id) ON DELETE CASCADE)"
    ),
    ( "decision_record",
      "CREATE TABLE decision_record("
        <> "record_id TEXT PRIMARY KEY,adr_id TEXT NOT NULL,operation_id TEXT NOT NULL REFERENCES operation(operation_id),"
        <> "title TEXT NOT NULL,summary TEXT NOT NULL,domains TEXT NOT NULL,body TEXT NOT NULL,path TEXT NOT NULL UNIQUE)"
    ),
    ( "connection_record",
      "CREATE TABLE connection_record("
        <> "connection_id TEXT PRIMARY KEY,adr_id TEXT NOT NULL,operation_id TEXT NOT NULL REFERENCES operation(operation_id),"
        <> "relation_kind TEXT NOT NULL,payload TEXT NOT NULL,rationale TEXT NOT NULL,path TEXT NOT NULL UNIQUE)"
    ),
    ( "reduced_adr",
      "CREATE TABLE reduced_adr("
        <> "adr_id TEXT PRIMARY KEY,state_token TEXT NOT NULL,conflicted INTEGER NOT NULL CHECK(conflicted IN (0,1)))"
    ),
    ( "axis_head",
      "CREATE TABLE axis_head("
        <> "adr_id TEXT NOT NULL REFERENCES reduced_adr(adr_id) ON DELETE CASCADE,axis TEXT NOT NULL,ordinal INTEGER NOT NULL CHECK(ordinal>=0),"
        <> "object_id TEXT NOT NULL,PRIMARY KEY(adr_id,axis,ordinal))"
    ),
    ( "current_connection",
      "CREATE TABLE current_connection("
        <> "adr_id TEXT NOT NULL REFERENCES reduced_adr(adr_id) ON DELETE CASCADE,axis TEXT NOT NULL,ordinal INTEGER NOT NULL CHECK(ordinal>=0),"
        <> "connection_id TEXT NOT NULL REFERENCES connection_record(connection_id),PRIMARY KEY(adr_id,axis,ordinal))"
    )
  ]

data ColdDatabaseError
  = ColdDatabaseNotFresh [(Text, Text)]
  | ColdDatabaseForeignKeysUnavailable
  | ColdDatabaseStorageFailure Text
  | ColdDatabaseVerificationFailure Text
  deriving (Eq, Show)

data ColdDatabaseStats = ColdDatabaseStats
  { coldDatabaseSemanticState :: Text,
    coldDatabaseManagedSourceCount :: Int,
    coldDatabaseIssueCount :: Int,
    coldDatabaseConflictCount :: Int,
    coldDatabaseOperationCount :: Int,
    coldDatabaseSearchDocumentCount :: Int
  }
  deriving (Eq, Show)

initializeFtsTargets :: Connection -> IO (Either RetrievalSqlError ())
initializeFtsTargets connection = create allFtsTargets
  where
    create [] = pure (Right ())
    create (target : targets) = do
      outcome <- trySql (execute_ connection (asQuery (ftsTargetDdl target)))
      case outcome of
        Left _ -> pure (Left (RetrievalIndexError target))
        Right () -> create targets

initializeSearchSchema :: Connection -> IO (Either SearchStorageError ())
initializeSearchSchema connection = do
  result <- tryStorage $ withTransaction connection (createSearchSchemaAction connection)
  pure (storageResult result)

replaceSearchMaterialization :: Connection -> SearchMaterialization -> IO (Either SearchStorageError ())
replaceSearchMaterialization connection materialization = do
  result <- tryStorage $ withTransaction connection $ do
    clearSearchMaterializationAction connection
    insertSearchMaterializationAction connection materialization
  pure (storageResult result)

createSearchSchemaAction :: Connection -> IO ()
createSearchSchemaAction connection = do
  forM_ searchOrdinarySchemaDdl $ \(component, ddl) -> runStorage component (execute_ connection (asQuery ddl))
  forM_ allFtsTargets $ \target -> runStorage (SearchFtsStorage target) (execute_ connection (asQuery (ftsTargetDdl target)))

clearSearchMaterializationAction :: Connection -> IO ()
clearSearchMaterializationAction connection = do
  forM_ allFtsTargets $ \target ->
    runStorage (SearchFtsStorage target) (execute_ connection (asQuery ("DELETE FROM " <> ftsTargetTable target)))
  runStorage SearchPassageStorage (execute_ connection "DELETE FROM search_section")
  runStorage SearchAliasStorage (execute_ connection "DELETE FROM local_alias")
  runStorage SearchDocumentStorage (execute_ connection "DELETE FROM search_document")

insertSearchMaterializationAction :: Connection -> SearchMaterialization -> IO ()
insertSearchMaterializationAction connection materialization = do
  forM_ sortedDocuments $ \document -> do
    insertSearchDocument connection document
    forM_ [SearchExactTarget, SearchStemmedTarget, SearchIdentifierTarget] $ \target ->
      insertSummaryFts connection target document
  forM_ sortedAliases (insertLocalAlias connection)
  forM_ numberedPassages $ \(rowId, passage) -> do
    insertSearchPassage connection rowId passage
    forM_ [PassageExactTarget, PassageStemmedTarget, PassageIdentifierTarget] $ \target ->
      insertPassageFts connection target rowId passage
  where
    sortedDocuments = sortBy (comparing searchDocumentItemId) (searchMaterializationDocuments materialization)
    sortedAliases = sortBy (comparing fst) (searchMaterializationAliases materialization)
    sortedPassages = sortBy (comparing searchPassageId) (searchMaterializationPassages materialization)
    numberedPassages = zip [1 :: Int64 ..] sortedPassages

writeColdDatabase :: Connection -> AnalyzedRepositorySnapshot -> Maybe SearchMaterialization -> Digest -> IO (Either ColdDatabaseError ColdDatabaseStats)
writeColdDatabase connection analyzed materialization materializationFingerprint = do
  freshness <- tryColdDatabase (existingSchemaObjects connection)
  case freshness of
    Left problem -> pure (Left problem)
    Right objects
      | not (null objects) -> pure (Left (ColdDatabaseNotFresh objects))
      | otherwise -> do
          foreignKeys <- tryColdDatabase (enableForeignKeys connection)
          case foreignKeys of
            Left problem -> pure (Left problem)
            Right False -> pure (Left ColdDatabaseForeignKeysUnavailable)
            Right True -> do
              let stats = coldStats analyzed materialization
              stored <-
                tryColdDatabase
                  ( withTransaction connection $ do
                      forM_ coldSchemaDdl $ \(_, ddl) -> execute_ connection (asQuery ddl)
                      createSearchSchemaAction connection
                      insertRepositoryConfig connection analyzed
                      insertManagedSources connection analyzed
                      insertCompilerDiagnostics connection (analyzedDiagnostics analyzed)
                      insertAdrConflicts connection (analyzedConflicts analyzed)
                      insertAdrConflictIssues connection (length (analyzedDiagnostics analyzed)) (analyzedConflicts analyzed)
                      case materialization of
                        Nothing -> pure ()
                        Just searchMaterialization -> do
                          insertSemanticRows connection analyzed
                          insertSearchMaterializationAction connection searchMaterialization
                      insertColdMeta connection analyzed materializationFingerprint stats
                      verifyColdDatabase connection analyzed materialization materializationFingerprint stats
                  )
              pure (stats <$ stored)

existingSchemaObjects :: Connection -> IO [(Text, Text)]
existingSchemaObjects connection =
  query_
    connection
    "SELECT type,name FROM sqlite_master WHERE name NOT LIKE 'sqlite_%' UNION ALL SELECT type,name FROM sqlite_temp_master WHERE name NOT LIKE 'sqlite_%' ORDER BY type,name"

enableForeignKeys :: Connection -> IO Bool
enableForeignKeys connection = do
  execute_ connection "PRAGMA foreign_keys=ON"
  values <- query_ connection "PRAGMA foreign_keys"
  pure (values == [Only (1 :: Int64)])

coldStats :: AnalyzedRepositorySnapshot -> Maybe SearchMaterialization -> ColdDatabaseStats
coldStats analyzed materialization =
  ColdDatabaseStats
    { coldDatabaseSemanticState =
        case materialization of
          Nothing -> "invalid"
          Just _ | null (analyzedConflicts analyzed) -> "valid"
          Just _ -> "conflict",
      coldDatabaseManagedSourceCount = length (rawRepositorySnapshotEntries (analyzedRawObservation analyzed)),
      coldDatabaseIssueCount = length (analyzedDiagnostics analyzed) + length (analyzedConflicts analyzed),
      coldDatabaseConflictCount = length (analyzedConflicts analyzed),
      coldDatabaseOperationCount =
        case materialization of
          Nothing -> 0
          Just _ -> Set.size (Set.fromList (map (provenanceOperationId . parsedManagedCapsule) (analyzedDocuments analyzed))),
      coldDatabaseSearchDocumentCount = maybe 0 (length . searchMaterializationDocuments) materialization
    }

insertRepositoryConfig :: Connection -> AnalyzedRepositorySnapshot -> IO ()
insertRepositoryConfig connection analyzed =
  execute
    connection
    "INSERT INTO repository_config(singleton,origin,path,oid,object_type,mode,bytes,parse_state,decision_root,connection_root) VALUES (1,?,?,?,?,?,?,?,?,?)"
    [ SQLText (configOriginValue (rawRepositoryConfigOrigin config)),
      SQLText ".adrai.toml",
      maybe SQLNull (SQLText . gitOidText . gitTreeOid) entry,
      maybe SQLNull (SQLText . gitObjectTypeValue . gitTreeObjectType) entry,
      maybe SQLNull (SQLText . gitFileModeValue . gitTreeMode) entry,
      maybe SQLNull (SQLBlob . gitBlobBytes) (rawRepositoryConfigBlob config),
      SQLText (configParseState config),
      maybe SQLNull (SQLText . repoPathText . managedDecisionPath) managedPaths,
      maybe SQLNull (SQLText . repoPathText . managedConnectionPath) managedPaths
    ]
  where
    config = rawRepositorySnapshotConfig (analyzedRawObservation analyzed)
    entry = rawRepositoryConfigEntry config
    managedPaths = rawRepositoryConfigManagedPaths config

insertManagedSources :: Connection -> AnalyzedRepositorySnapshot -> IO ()
insertManagedSources connection analyzed =
  forM_ observations $ \observation -> do
    let entry = repositoryTreeEntry observation
        path = gitTreePath entry
        bytes = maybe SQLNull (SQLBlob . gitBlobBytes) (repositoryTreeBlob observation)
        parseState =
          case Map.lookup path parsedByPath of
            Nothing -> "nonblob"
            Just (Left _) -> "invalid"
            Just (Right _) -> "valid"
    execute
      connection
      "INSERT INTO managed_source(path,oid,object_type,mode,bytes,parse_state) VALUES (?,?,?,?,?,?)"
      [ SQLText (repoPathText path),
        SQLText (gitOidText (gitTreeOid entry)),
        SQLText (gitObjectTypeValue (gitTreeObjectType entry)),
        SQLText (gitFileModeValue (gitTreeMode entry)),
        bytes,
        SQLText parseState
      ]
  where
    observations =
      sortBy
        (comparing (repoPathText . gitTreePath . repositoryTreeEntry))
        (rawRepositorySnapshotEntries (analyzedRawObservation analyzed))
    parsedByPath = Map.fromList [(snapshotEntryPath entry, snapshotEntryDocument entry) | entry <- analyzedManagedEntries analyzed]

insertCompilerDiagnostics :: Connection -> [CompilerDiagnostic] -> IO ()
insertCompilerDiagnostics connection diagnostics =
  forM_ (zip [0 :: Int64 ..] diagnostics) $ \(ordinal, problem) ->
    execute
      connection
      "INSERT INTO issue(ordinal,code,severity,origin,adr_id,object_id,operation_id,commit_oid,path,message) VALUES (?,?,?,?,?,?,?,?,?,?)"
      [ SQLInteger ordinal,
        SQLText (compilerDiagnosticCodeText (compilerDiagnosticCode problem)),
        SQLText (diagnosticSeverityValue (compilerDiagnosticSeverity problem)),
        SQLText (diagnosticOriginValue (compilerDiagnosticOrigin problem)),
        sqlMaybeText adrIdText (compilerDiagnosticAdr problem),
        sqlMaybeText objectRefText (compilerDiagnosticObject problem),
        sqlMaybeText operationIdText (compilerDiagnosticOperation problem),
        sqlMaybeText gitOidText (compilerDiagnosticCommit problem),
        sqlMaybeText repoPathText (compilerDiagnosticPath problem),
        SQLText (compilerDiagnosticMessage problem)
      ]

insertAdrConflicts :: Connection -> [AdrConflict] -> IO ()
insertAdrConflicts connection conflicts =
  forM_ (sortBy (comparing adrConflictAdr) conflicts) $ \conflict ->
    execute
      connection
      "INSERT INTO adr_conflict(adr_id,code,candidate_count,state_token,summaries) VALUES (?,?,?,?,?)"
      [ SQLText (adrIdText (adrConflictAdr conflict)),
        SQLText (adrConflictCode conflict),
        SQLInteger (fromIntegral (adrConflictCount conflict)),
        SQLText (stateTokenText (adrConflictStateToken conflict)),
        SQLText (Text.intercalate "\n" (adrConflictSummaries conflict))
      ]

insertAdrConflictIssues :: Connection -> Int -> [AdrConflict] -> IO ()
insertAdrConflictIssues connection diagnosticCount conflicts =
  forM_ (zip [fromIntegral diagnosticCount :: Int64 ..] (sortBy (comparing adrConflictAdr) conflicts)) $ \(ordinal, conflict) ->
    execute
      connection
      "INSERT INTO issue(ordinal,code,severity,origin,adr_id,object_id,operation_id,commit_oid,path,message) VALUES (?,?,?,?,?,?,?,?,?,?)"
      [ SQLInteger ordinal,
        SQLText (adrConflictCode conflict),
        SQLText "error",
        SQLText "graph",
        SQLText (adrIdText (adrConflictAdr conflict)),
        SQLNull,
        SQLNull,
        SQLNull,
        SQLNull,
        SQLText (Text.intercalate "; " (adrConflictSummaries conflict))
      ]

insertSemanticRows :: Connection -> AnalyzedRepositorySnapshot -> IO ()
insertSemanticRows connection analyzed = do
  insertOperations connection documents
  forM_ documents (insertParsedDocument connection)
  forM_ (sortBy (comparing reducedAdrId) (graphReductionAdrs reduction)) $ \reduced -> do
    insertReducedAdr connection reduced
    insertAxisHeads connection reduced
    insertCurrentConnections connection reduced
  where
    documents = analyzedDocuments analyzed
    reduction = analyzedReduction analyzed

insertOperations :: Connection -> [ParsedManagedDocument] -> IO ()
insertOperations connection documents = do
  forM_ (Map.toAscList grouped) $ \(operationId, members) -> do
    let context = provenanceOperationContext (parsedManagedCapsule (firstMember members))
        actor = operationContextActor context
        inputs = operationContextInputs context
    execute
      connection
      "INSERT INTO operation(operation_id,timestamp_ms,actor_kind,actor_id,actor_model,basis_oid,branch_hint,upstream_hint,line_anchors,tool_version,input_digest,prompt_digest,context_digest) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)"
      [ SQLText (operationIdText operationId),
        SQLText (Text.pack (show (operationContextTimestampMs context))),
        SQLText (actorKindValue (actorKind actor)),
        SQLText (actorId actor),
        sqlMaybeText id (actorModel actor),
        SQLText (gitOidText (operationContextBasis context)),
        sqlMaybeText id (operationContextBranchHint context),
        sqlMaybeText id (operationContextUpstreamHint context),
        SQLText (renderLineAnchors (operationContextLineAnchors context)),
        SQLText (operationContextToolVersion context),
        sqlMaybeDigest (provenanceInputDigest inputs),
        sqlMaybeDigest (provenancePromptDigest inputs),
        sqlMaybeDigest (provenanceContextDigest inputs)
      ]
    forM_ (sortBy (comparing (objectRefText . parsedDocumentObject)) members) $ \document -> do
      let capsule = parsedManagedCapsule document
          objectId = parsedDocumentObject document
      execute
        connection
        "INSERT INTO operation_member(operation_id,object_id,object_type,event_kind,semantic_digest,path) VALUES (?,?,?,?,?,?)"
        [ SQLText (operationIdText operationId),
          SQLText (objectRefText objectId),
          SQLText (parsedDocumentType document),
          SQLText (eventKindText (provenanceEventKind capsule)),
          SQLText (digestValue (provenanceSemanticDigest capsule)),
          SQLText (repoPathText (parsedManagedPath document))
        ]
      forM_ (zip [0 :: Int64 ..] (provenanceParents capsule)) $ \(ordinal, parent) ->
        execute
          connection
          "INSERT INTO operation_member_parent(operation_id,object_id,ordinal,parent_object_id) VALUES (?,?,?,?)"
          [ SQLText (operationIdText operationId),
            SQLText (objectRefText objectId),
            SQLInteger ordinal,
            SQLText (provenanceObjectIdText parent)
          ]
  where
    grouped =
      Map.fromListWith (<>)
        [ (provenanceOperationId (parsedManagedCapsule document), [document])
          | document <- documents
        ]
    firstMember [] = error "internal error: empty operation group"
    firstMember (member : _) = member

insertParsedDocument :: Connection -> ParsedManagedDocument -> IO ()
insertParsedDocument connection document =
  case parsedManagedRecord document of
    ManagedDecision decision ->
      execute
        connection
        "INSERT INTO decision_record(record_id,adr_id,operation_id,title,summary,domains,body,path) VALUES (?,?,?,?,?,?,?,?)"
        [ SQLText (recordIdText (decisionRecord decision)),
          SQLText (adrIdText (decisionAdr decision)),
          SQLText operation,
          SQLText (decisionTitle decision),
          SQLText (decisionSummary decision),
          SQLText (Text.intercalate "\n" (map domainText (decisionDomains decision))),
          SQLText (decisionBody decision),
          SQLText path
        ]
    ManagedConnection connectionRecord ->
      execute
        connection
        "INSERT INTO connection_record(connection_id,adr_id,operation_id,relation_kind,payload,rationale,path) VALUES (?,?,?,?,?,?,?)"
        [ SQLText (connectionIdText (connectionRecordId connectionRecord)),
          SQLText (adrIdText (connectionAdrValue connectionRecord)),
          SQLText operation,
          SQLText (connectionKindValue (connectionPayload connectionRecord)),
          SQLText (connectionPayloadValue (connectionPayload connectionRecord)),
          SQLText (connectionRationale connectionRecord),
          SQLText path
        ]
  where
    operation = operationIdText (provenanceOperationId (parsedManagedCapsule document))
    path = repoPathText (parsedManagedPath document)

insertReducedAdr :: Connection -> ReducedAdr -> IO ()
insertReducedAdr connection reduced =
  execute
    connection
    "INSERT INTO reduced_adr(adr_id,state_token,conflicted) VALUES (?,?,?)"
    [ SQLText (adrIdText (reducedAdrId reduced)),
      SQLText (stateTokenText (reducedStateToken reduced)),
      SQLInteger (boolInteger (not (null (reducedConflictAxes reduced))))
    ]

insertAxisHeads :: Connection -> ReducedAdr -> IO ()
insertAxisHeads connection reduced =
  forM_ axes $ \(axis, heads) ->
    forM_ (zip [0 :: Int64 ..] heads) $ \(ordinal, objectId) ->
      execute
        connection
        "INSERT INTO axis_head(adr_id,axis,ordinal,object_id) VALUES (?,?,?,?)"
        [SQLText adr, SQLText axis, SQLInteger ordinal, SQLText objectId]
  where
    adr = adrIdText (reducedAdrId reduced)
    axes =
      [ ("decision", map recordIdText (axisResolutionHeads (reducedDecisionAxis reduced))),
        ("scope", map connectionIdText (axisResolutionHeads (reducedScopeAxis reduced))),
        ("domain", map connectionIdText (axisResolutionHeads (reducedDomainAxis reduced))),
        ("status", map connectionIdText (axisResolutionHeads (reducedStatusAxis reduced)))
      ]

insertCurrentConnections :: Connection -> ReducedAdr -> IO ()
insertCurrentConnections connection reduced =
  forM_ (zip [0 :: Int64 ..] (sortBy (comparing currentConnectionId) (reducedCurrentConnections reduced))) $ \(ordinal, reference) ->
    execute
      connection
      "INSERT INTO current_connection(adr_id,axis,ordinal,connection_id) VALUES (?,?,?,?)"
      [ SQLText (adrIdText (reducedAdrId reduced)),
        SQLText (graphAxisValue (currentConnectionAxis reference)),
        SQLInteger ordinal,
        SQLText (connectionIdText (currentConnectionId reference))
      ]

insertColdMeta :: Connection -> AnalyzedRepositorySnapshot -> Digest -> ColdDatabaseStats -> IO ()
insertColdMeta connection analyzed materializationFingerprint stats =
  forM_ (coldMetaValues analyzed materializationFingerprint stats) $ \(key, value) ->
    execute connection "INSERT INTO meta(key,value) VALUES (?,?)" (key, value)

coldMetaValues :: AnalyzedRepositorySnapshot -> Digest -> ColdDatabaseStats -> [(Text, Text)]
coldMetaValues analyzed materializationFingerprint stats =
  [ ("schema", "adrai-cache/1"),
    ("compiler_abi", "adrai-cold-compiler/1"),
    ("materializer", materializationImplementationFingerprint),
    ("requested_revision", revisionSpecText (resolvedRequestedRevision revision)),
    ("resolved_oid", gitOidText (resolvedCommitOid revision)),
    ("source_fingerprint", digestValue (analyzedSourceFingerprint analyzed)),
    ("materialization_fingerprint", digestValue materializationFingerprint),
    ("semantic_state", coldDatabaseSemanticState stats),
    ("history_complete", if analyzedHistoryComplete analyzed then "true" else "false"),
    ("managed_source_count", decimalInt (coldDatabaseManagedSourceCount stats)),
    ("issue_count", decimalInt (coldDatabaseIssueCount stats)),
    ("conflict_count", decimalInt (coldDatabaseConflictCount stats)),
    ("operation_count", decimalInt (coldDatabaseOperationCount stats)),
    ("search_document_count", decimalInt (coldDatabaseSearchDocumentCount stats))
  ]
  where
    raw = analyzedRawObservation analyzed
    revision = rawRepositorySnapshotRevision raw

data ColdExpectedCounts = ColdExpectedCounts
  { expectedOperationMembers :: Int,
    expectedOperationParents :: Int,
    expectedDecisions :: Int,
    expectedConnections :: Int,
    expectedReducedAdrs :: Int,
    expectedAxisHeads :: Int,
    expectedCurrentConnections :: Int,
    expectedSearchDocuments :: Int,
    expectedSearchAliases :: Int,
    expectedSearchSections :: Int
  }

expectedColdCounts :: AnalyzedRepositorySnapshot -> Maybe SearchMaterialization -> ColdExpectedCounts
expectedColdCounts analyzed materialization =
  case materialization of
    Nothing -> emptyCounts
    Just searchMaterialization ->
      ColdExpectedCounts
        { expectedOperationMembers = length documents,
          expectedOperationParents = sum (map (length . provenanceParents . parsedManagedCapsule) documents),
          expectedDecisions = length [() | document <- documents, ManagedDecision _ <- [parsedManagedRecord document]],
          expectedConnections = length [() | document <- documents, ManagedConnection _ <- [parsedManagedRecord document]],
          expectedReducedAdrs = length reducedAdrs,
          expectedAxisHeads = sum (map reducedAxisHeadCount reducedAdrs),
          expectedCurrentConnections = sum (map (length . reducedCurrentConnections) reducedAdrs),
          expectedSearchDocuments = length (searchMaterializationDocuments searchMaterialization),
          expectedSearchAliases = length (searchMaterializationAliases searchMaterialization),
          expectedSearchSections = length (searchMaterializationPassages searchMaterialization)
        }
  where
    documents = analyzedDocuments analyzed
    reducedAdrs = graphReductionAdrs (analyzedReduction analyzed)
    emptyCounts = ColdExpectedCounts 0 0 0 0 0 0 0 0 0 0
    reducedAxisHeadCount reduced =
      length (axisResolutionHeads (reducedDecisionAxis reduced))
        + length (axisResolutionHeads (reducedScopeAxis reduced))
        + length (axisResolutionHeads (reducedDomainAxis reduced))
        + length (axisResolutionHeads (reducedStatusAxis reduced))

verifyColdDatabase :: Connection -> AnalyzedRepositorySnapshot -> Maybe SearchMaterialization -> Digest -> ColdDatabaseStats -> IO ()
verifyColdDatabase connection analyzed materialization materializationFingerprint stats = do
  let hasErrors = any ((== CompilerDiagnosticError) . compilerDiagnosticSeverity) (analyzedDiagnostics analyzed)
      expectedSemanticState =
        case (hasErrors, materialization) of
          (True, Nothing) -> Just "invalid"
          (False, Just _) | null (analyzedConflicts analyzed) -> Just "valid"
          (False, Just _) -> Just "conflict"
          _ -> Nothing
  case expectedSemanticState of
    Just state | state == coldDatabaseSemanticState stats -> pure ()
    _ -> throwIO (ColdDatabaseAbort "semantic state, diagnostics, conflicts, and materialization disagree")
  foreignKeyProblems <- query_ connection "PRAGMA foreign_key_check"
  case (foreignKeyProblems :: [(Text, Int64, Text, Int64)]) of
    [] -> pure ()
    _ -> throwIO (ColdDatabaseAbort "foreign key verification failed")
  actualMeta <- query_ connection "SELECT key,value FROM meta ORDER BY key" :: IO [(Text, Text)]
  let expectedMeta = sortBy (comparing fst) (coldMetaValues analyzed materializationFingerprint stats)
  if actualMeta == expectedMeta
    then pure ()
    else throwIO (ColdDatabaseAbort "canonical meta rows do not match the cold compilation")
  if Set.fromList (map fst countMatrix) == Set.fromList declaredLogicalTables
    then pure ()
    else throwIO (ColdDatabaseAbort "internal verification matrix does not cover every declared logical table")
  forM_ countMatrix (uncurry (verifyCount connection))
  if expectedSearchDocuments expected == coldDatabaseSearchDocumentCount stats
    then pure ()
    else throwIO (ColdDatabaseAbort "search materialization statistics do not match canonical rows")
  verifyBidirectionalProjection
    connection
    "operation member/object rows"
    "SELECT object_id FROM operation_member"
    "SELECT object_id FROM (SELECT record_id AS object_id FROM decision_record UNION ALL SELECT connection_id AS object_id FROM connection_record)"
  forM_ [SearchExactTarget, SearchStemmedTarget, SearchIdentifierTarget] $ \target ->
    verifyBidirectionalProjection
      connection
      ("summary FTS keys for " <> ftsTargetTable target)
      ("SELECT item_id,adr_id,candidate_record_id FROM " <> ftsTargetTable target)
      "SELECT item_id,adr_id,candidate_record_id FROM search_document"
  forM_ [PassageExactTarget, PassageStemmedTarget, PassageIdentifierTarget] $ \target ->
    verifyBidirectionalProjection
      connection
      ("passage FTS keys for " <> ftsTargetTable target)
      ("SELECT rowid,item_id,adr_id,candidate_record_id,section_kind FROM " <> ftsTargetTable target)
      "SELECT passage_rowid,item_id,adr_id,candidate_record_id,section_kind FROM search_section"
  where
    expected = expectedColdCounts analyzed materialization
    countMatrix =
      [ ("meta", length (coldMetaValues analyzed materializationFingerprint stats)),
        ("repository_config", 1),
        ("managed_source", coldDatabaseManagedSourceCount stats),
        ("issue", coldDatabaseIssueCount stats),
        ("adr_conflict", coldDatabaseConflictCount stats),
        ("operation", coldDatabaseOperationCount stats),
        ("operation_member", expectedOperationMembers expected),
        ("operation_member_parent", expectedOperationParents expected),
        ("decision_record", expectedDecisions expected),
        ("connection_record", expectedConnections expected),
        ("reduced_adr", expectedReducedAdrs expected),
        ("axis_head", expectedAxisHeads expected),
        ("current_connection", expectedCurrentConnections expected),
        ("search_document", expectedSearchDocuments expected),
        ("local_alias", expectedSearchAliases expected),
        ("search_section", expectedSearchSections expected)
      ]
        <> [(ftsTargetTable target, expectedSearchDocuments expected) | target <- [SearchExactTarget, SearchStemmedTarget, SearchIdentifierTarget]]
        <> [(ftsTargetTable target, expectedSearchSections expected) | target <- [PassageExactTarget, PassageStemmedTarget, PassageIdentifierTarget]]
    declaredLogicalTables =
      map fst coldSchemaDdl
        <> ["search_document", "local_alias", "search_section"]
        <> map ftsTargetTable allFtsTargets

verifyBidirectionalProjection :: Connection -> Text -> Text -> Text -> IO ()
verifyBidirectionalProjection connection label leftProjection rightProjection = do
  verifyDifference leftProjection rightProjection
  verifyDifference rightProjection leftProjection
  where
    verifyDifference leftSide rightSide = do
      rows <-
        query_
          connection
          (asQuery ("SELECT count(*) FROM (" <> leftSide <> " EXCEPT " <> rightSide <> ")")) :: IO [Only Int64]
      case rows of
        [Only 0] -> pure ()
        _ -> throwIO (ColdDatabaseAbort ("canonical projection mismatch: " <> label))

verifyCount :: Connection -> Text -> Int -> IO ()
verifyCount connection table expected = do
  rows <- query_ connection (asQuery ("SELECT count(*) FROM " <> table)) :: IO [Only Int64]
  case rows of
    [Only actual] | actual == fromIntegral expected -> pure ()
    _ -> throwIO (ColdDatabaseAbort ("canonical count mismatch for " <> table))

data ColdDatabaseAbort = ColdDatabaseAbort Text
  deriving (Show)

instance Exception ColdDatabaseAbort

tryColdDatabase :: IO value -> IO (Either ColdDatabaseError value)
tryColdDatabase action = (Right <$> action) `catch` handle
  where
    handle :: SomeException -> IO (Either ColdDatabaseError value)
    handle exception =
      case fromException exception of
        Just (ColdDatabaseAbort message) -> pure (Left (ColdDatabaseVerificationFailure message))
        Nothing -> pure (Left (ColdDatabaseStorageFailure (Text.pack (displayException exception))))

sqlMaybeText :: (value -> Text) -> Maybe value -> SQLData
sqlMaybeText renderValue value = maybe SQLNull (SQLText . renderValue) value

sqlMaybeDigest :: Maybe Digest -> SQLData
sqlMaybeDigest = sqlMaybeText digestValue

digestValue :: Digest -> Text
digestValue = encodeBase64Url . digestBytes

decimalInt :: Int -> Text
decimalInt = Text.pack . show

configOriginValue :: RepositoryConfigOrigin -> Text
configOriginValue DefaultConfigOrigin = "default"
configOriginValue CommittedConfigOrigin = "committed"

configParseState :: RawRepositoryConfigObservation -> Text
configParseState config =
  case rawRepositoryConfigResult config of
    Right _ -> "valid"
    Left (RepositoryConfigFailureInvalidUtf8 _) -> "invalid_utf8"
    Left (RepositoryConfigFailureParse _ _) -> "invalid_toml"
    Left (RepositoryConfigFailureNotBlob _) -> "nonblob"

diagnosticSeverityValue :: CompilerDiagnosticSeverity -> Text
diagnosticSeverityValue CompilerDiagnosticError = "error"
diagnosticSeverityValue CompilerDiagnosticWarning = "warning"

diagnosticOriginValue :: CompilerDiagnosticOrigin -> Text
diagnosticOriginValue origin =
  case origin of
    CompilerConfigOrigin -> "config"
    CompilerPathDocumentOrigin -> "path_document"
    CompilerHistoryOrigin -> "history"
    CompilerOperationOrigin -> "operation"
    CompilerGraphOrigin -> "graph"
    CompilerBasisOrigin -> "basis"

gitObjectTypeValue :: GitObjectType -> Text
gitObjectTypeValue objectType =
  case objectType of
    GitBlobObject -> "blob"
    GitTreeObject -> "tree"
    GitCommitObject -> "commit"
    GitTagObject -> "tag"

gitFileModeValue :: GitFileMode -> Text
gitFileModeValue mode =
  case mode of
    GitRegularFile -> "100644"
    GitExecutableFile -> "100755"
    GitSymbolicLink -> "120000"
    GitSubmodule -> "160000"
    GitDirectory -> "040000"

actorKindValue :: ActorKind -> Text
actorKindValue HumanActor = "human"
actorKindValue LlmActor = "llm"
actorKindValue ServiceActor = "service"

renderLineAnchors :: [LineAnchor] -> Text
renderLineAnchors anchors =
  canonicalJsonText
    ( JsonArray
        [ object
            [ ("id", JsonString (lineAnchorId anchor)),
              ("commit", JsonString (gitOidText (lineAnchorCommit anchor)))
            ]
          | anchor <- anchors
        ]
    )

parsedDocumentObject :: ParsedManagedDocument -> ObjectRef
parsedDocumentObject document =
  case parsedManagedRecord document of
    ManagedDecision decision -> recordObjectRef (decisionRecord decision)
    ManagedConnection connectionRecord -> connectionObjectRef (connectionRecordId connectionRecord)

parsedDocumentType :: ParsedManagedDocument -> Text
parsedDocumentType document =
  case parsedManagedRecord document of
    ManagedDecision _ -> "decision"
    ManagedConnection _ -> "connection"

connectionAdrValue :: ConnectionRecord -> AdrId
connectionAdrValue connectionRecord =
  case connectionPayload connectionRecord of
    AmendsConnection payload -> amendsSubjectAdr payload
    AppliesToConnection payload -> appliesToSubjectAdr payload
    DomainsConnection payload -> domainsSubjectAdr payload
    StatusConnection payload -> statusSubjectAdr payload

connectionKindValue :: ConnectionPayload -> Text
connectionKindValue payload =
  case payload of
    AmendsConnection _ -> "amends"
    AppliesToConnection _ -> "applies_to"
    DomainsConnection _ -> "domains"
    StatusConnection _ -> "status"

connectionPayloadValue :: ConnectionPayload -> Text
connectionPayloadValue payload =
  canonicalJsonText $
    case payload of
      AmendsConnection value ->
        object
          [ ("subject_adr", JsonString (adrIdText (amendsSubjectAdr value))),
            ("from_record", JsonString (recordIdText (amendsFromRecord value))),
            ("to_records", textArray recordIdText (amendsToRecords value))
          ]
      AppliesToConnection value ->
        object
          [ ("subject_adr", JsonString (adrIdText (appliesToSubjectAdr value))),
            ("parent_connections", textArray connectionIdText (appliesToParentConnections value)),
            ("change", JsonString (appliesToChange value)),
            ("added", textArray scopePatternText (appliesToAdded value)),
            ("removed", textArray scopePatternText (appliesToRemoved value)),
            ("effective", textArray scopePatternText (appliesToEffective value))
          ]
      DomainsConnection value ->
        object
          [ ("subject_adr", JsonString (adrIdText (domainsSubjectAdr value))),
            ("parent_connections", textArray connectionIdText (domainsParentConnections value)),
            ("change", JsonString (domainsChange value)),
            ("added", textArray domainText (domainsAdded value)),
            ("removed", textArray domainText (domainsRemoved value)),
            ("effective", textArray domainText (domainsEffective value)),
            ("refinements", textArray domainRefinementText (domainsRefinements value))
          ]
      StatusConnection value ->
        object
          [ ("subject_adr", JsonString (adrIdText (statusSubjectAdr value))),
            ("parent_connections", textArray connectionIdText (statusParentConnections value)),
            ("state", JsonString (statusStateValue (statusState value))),
            ("record_heads", textArray recordIdText (statusRecordHeads value)),
            ("replacement_adr", maybe JsonNull (JsonString . adrIdText) (statusReplacementAdr value))
          ]

canonicalJsonText :: JsonValue -> Text
canonicalJsonText = Text.dropWhileEnd (== '\n') . renderCanonicalJson

textArray :: (value -> Text) -> [value] -> JsonValue
textArray renderValue = JsonArray . map (JsonString . renderValue)

statusStateValue :: StatusState -> Text
statusStateValue StatusActive = "active"
statusStateValue StatusObsolete = "obsolete"

graphAxisValue :: GraphAxis -> Text
graphAxisValue DecisionAxis = "decision"
graphAxisValue ScopeAxis = "scope"
graphAxisValue DomainAxis = "domain"
graphAxisValue StatusAxis = "status"

loadLocalAliases :: Connection -> IO (Either SearchStorageError [LocalAlias])
loadLocalAliases connection = do
  result <- trySql (query connection "SELECT alias,expansion FROM local_alias ORDER BY alias" ())
  pure $ case result of
    Left _ -> Left (SearchStorageError SearchAliasStorage)
    Right aliases -> Right aliases

insertSearchDocument :: Connection -> SearchDocument -> IO ()
insertSearchDocument connection document =
  runStorage SearchDocumentStorage $
    execute
      connection
      "INSERT INTO search_document(item_id,adr_id,candidate_record_id,title,summary,context,decision,consequences,domains,rationale,identifiers,other,scope,source_paths,obsolete,conflicted,state_token) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
      [ SQLText (searchDocumentItemId document),
        SQLText (adrIdText (searchDocumentAdrId document)),
        SQLText (recordIdText (searchDocumentCandidateRecordId document)),
        SQLText (searchDocumentTitle document),
        SQLText (searchDocumentSummary document),
        SQLText (searchDocumentContext document),
        SQLText (searchDocumentDecision document),
        SQLText (searchDocumentConsequences document),
        SQLText (Text.intercalate "\n" (searchDocumentDomains document)),
        SQLText (searchDocumentRationale document),
        SQLText (searchDocumentIdentifiers document),
        SQLText (searchDocumentOther document),
        SQLText (Text.intercalate "\n" (searchDocumentScope document)),
        SQLText (Text.intercalate "\n" (searchDocumentSourcePaths document)),
        SQLInteger (boolInteger (searchDocumentObsolete document)),
        SQLInteger (boolInteger (searchDocumentConflicted document)),
        SQLText (stateTokenText (searchDocumentStateToken document))
      ]

insertLocalAlias :: Connection -> LocalAlias -> IO ()
insertLocalAlias connection (alias, expansion) =
  runStorage SearchAliasStorage $
    execute connection "INSERT INTO local_alias(alias,expansion) VALUES (?,?)" [SQLText alias, SQLText expansion]

insertSearchPassage :: Connection -> Int64 -> SearchPassage -> IO ()
insertSearchPassage connection rowId passage =
  runStorage SearchPassageStorage $
    execute
      connection
      "INSERT INTO search_section(passage_rowid,item_id,search_item_id,adr_id,candidate_record_id,section_kind,ordinal,line_start,line_end,text,weight,source_paths,identifiers) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)"
      [ SQLInteger rowId,
        SQLText (searchPassageId passage),
        SQLText (searchPassageDocumentItemId passage),
        SQLText (adrIdText (searchPassageAdrId passage)),
        SQLText (recordIdText (searchPassageCandidateRecordId passage)),
        SQLText (sectionKindName (searchPassageSectionKind passage)),
        SQLInteger (fromIntegral (searchPassageOrdinal passage)),
        SQLInteger (fromIntegral (searchPassageLineStart passage)),
        SQLInteger (fromIntegral (searchPassageLineEnd passage)),
        SQLText (searchPassageText passage),
        SQLFloat (searchPassageWeight passage),
        SQLText (Text.intercalate "\n" (searchPassageSourcePaths passage)),
        SQLText (searchPassageIdentifiers passage)
      ]

insertSummaryFts :: Connection -> FtsTarget -> SearchDocument -> IO ()
insertSummaryFts connection target document =
  runStorage (SearchFtsStorage target) $
    execute connection statement parameters
  where
    table = ftsTargetTable target
    base =
      [ SQLText (searchDocumentItemId document),
        SQLText (adrIdText (searchDocumentAdrId document)),
        SQLText (recordIdText (searchDocumentCandidateRecordId document))
      ]
    (columns, values) = case target of
      SearchExactTarget ->
        ( ["title", "summary", "decision", "domains", "rationale", "context", "consequences", "identifiers"],
          [ searchDocumentTitle document,
            searchDocumentSummary document,
            searchDocumentDecision document,
            Text.intercalate "\n" (searchDocumentDomains document),
            searchDocumentRationale document,
            searchDocumentContext document,
            searchDocumentConsequences document,
            searchDocumentIdentifiers document
          ]
        )
      SearchStemmedTarget ->
        ( ["title", "summary", "decision", "rationale", "context", "consequences"],
          [ searchDocumentTitle document,
            searchDocumentSummary document,
            searchDocumentDecision document,
            searchDocumentRationale document,
            searchDocumentContext document,
            searchDocumentConsequences document
          ]
        )
      SearchIdentifierTarget -> (["identifiers"], [searchDocumentIdentifiers document])
      _ -> error "internal error: passage target used for summary insertion"
    allColumns = ["item_id", "adr_id", "candidate_record_id"] <> columns
    statement = insertStatement table allColumns
    parameters = base <> map SQLText values

insertPassageFts :: Connection -> FtsTarget -> Int64 -> SearchPassage -> IO ()
insertPassageFts connection target rowId passage =
  runStorage (SearchFtsStorage target) $
    execute connection statement parameters
  where
    table = ftsTargetTable target
    base =
      [ SQLInteger rowId,
        SQLText (searchPassageId passage),
        SQLText (adrIdText (searchPassageAdrId passage)),
        SQLText (recordIdText (searchPassageCandidateRecordId passage)),
        SQLText (sectionKindName (searchPassageSectionKind passage))
      ]
    (columns, values) = case target of
      PassageExactTarget -> (["text", "identifiers"], [searchPassageText passage, searchPassageIdentifiers passage])
      PassageStemmedTarget -> (["text"], [searchPassageText passage])
      PassageIdentifierTarget -> (["identifiers"], [searchPassageIdentifiers passage])
      _ -> error "internal error: summary target used for passage insertion"
    statement = insertStatementWithRowId table (["item_id", "adr_id", "candidate_record_id", "section_kind"] <> columns)
    parameters = base <> map SQLText values

insertStatement :: Text -> [Text] -> Query
insertStatement table columns =
  asQuery ("INSERT INTO " <> table <> "(" <> Text.intercalate "," columns <> ") VALUES (" <> placeholders (length columns) <> ")")

insertStatementWithRowId :: Text -> [Text] -> Query
insertStatementWithRowId table columns =
  asQuery ("INSERT INTO " <> table <> "(rowid," <> Text.intercalate "," columns <> ") VALUES (" <> placeholders (length columns + 1) <> ")")

placeholders :: Int -> Text
placeholders amount = Text.intercalate "," (replicate amount "?")

boolInteger :: Bool -> Int64
boolInteger value = if value then 1 else 0

data StorageException = StorageException SearchStorageComponent SQLError
  deriving (Show)

instance Exception StorageException

runStorage :: SearchStorageComponent -> IO value -> IO value
runStorage component action = action `catch` (throwIO . StorageException component)

tryStorage :: IO value -> IO (Either StorageException value)
tryStorage = try

storageResult :: Either StorageException value -> Either SearchStorageError value
storageResult result = case result of
  Left (StorageException component _) -> Left (SearchStorageError component)
  Right value -> Right value

runFtsTarget :: Connection -> FtsTarget -> Text -> Set Text -> CandidateLimit -> IO (Either RetrievalSqlError [FtsHit])
runFtsTarget connection target expression allowed limit
  | Text.null expression || Set.null allowed = pure (Right [])
  | otherwise = runBatches Map.empty (batchesOf 700 (Set.toAscList allowed))
  where
    runBatches scores [] = pure (Right (take (fromIntegral (candidateLimitValue limit)) (rankScores scores)))
    runBatches scores (itemIds : remaining) = do
      outcome <- trySql (query connection (rankQuery target (length itemIds)) (rankParameters expression itemIds limit))
      case outcome of
        Left sqlException -> pure (Left (classifySqlError target sqlException))
        Right hits -> runBatches (foldl' insertMaximum scores hits) remaining

runSummaryFtsChannels :: Connection -> QueryPlan -> Set Text -> CandidateLimit -> IO (Either RetrievalSqlError SummaryFtsCandidates)
runSummaryFtsChannels connection plan allowed requested =
  case candidateCap requested of
    Left limitError -> pure (Left limitError)
    Right cap -> do
      phraseResult <- runFtsTarget connection SearchExactTarget (queryPlanFtsExactPhrase plan) allowed cap
      exactAndResult <- runFtsTarget connection SearchExactTarget (Text.intercalate " AND " (queryPlanFtsExactTerms plan)) allowed cap
      nearResult <- runFtsTarget connection SearchExactTarget (queryPlanFtsNear plan) allowed cap
      case (phraseResult, exactAndResult, nearResult) of
        (Right phrase, Right exactAnd, Right near) -> do
          let exactNearIds = Set.fromList (map ftsHitItemId exactAnd <> map ftsHitItemId near)
              threshold = min (max 20 (fromIntegral (candidateLimitValue requested) * 3)) (Set.size allowed)
              usePrefix = Set.size exactNearIds < threshold
          prefixResult <-
            if usePrefix
              then runFtsTarget connection SearchExactTarget (queryPlanFtsPrefix plan) allowed cap
              else pure (Right [])
          stemmedResult <- runFtsTarget connection SearchStemmedTarget (queryPlanFtsStemmed plan) allowed cap
          identifierResult <- runFtsTarget connection SearchIdentifierTarget (queryPlanFtsIdentifier plan) allowed cap
          pure $ do
            prefix <- prefixResult
            stemmed <- stemmedResult
            identifier <- identifierResult
            Right
              SummaryFtsCandidates
                { summaryFtsPhrase = phrase,
                  summaryFtsTerms = mergeHitsNoCap [exactAnd, near, prefix],
                  summaryFtsStemmed = stemmed,
                  summaryFtsIdentifier = identifier,
                  summaryFtsPrefixUsed = usePrefix && not (null prefix)
                }
        (Left retrievalError, _, _) -> pure (Left retrievalError)
        (_, Left retrievalError, _) -> pure (Left retrievalError)
        (_, _, Left retrievalError) -> pure (Left retrievalError)

runPassageFtsChannels :: Connection -> QueryPlan -> Set Text -> CandidateLimit -> IO (Either RetrievalSqlError PassageFtsCandidates)
runPassageFtsChannels connection plan allowed limit = do
  exactResult <- runFtsTarget connection PassageExactTarget exactExpression allowed limit
  stemmedResult <- runFtsTarget connection PassageStemmedTarget (queryPlanFtsStemmed plan) allowed limit
  identifierResult <- runFtsTarget connection PassageIdentifierTarget (queryPlanFtsIdentifier plan) allowed limit
  pure $ do
    exact <- exactResult
    stemmed <- stemmedResult
    identifier <- identifierResult
    Right
      PassageFtsCandidates
        { passageFtsExact = exact,
          passageFtsStemmed = stemmed,
          passageFtsIdentifier = identifier
        }
  where
    exactExpression = Text.intercalate " OR " (queryPlanFtsExactTerms plan)

rankQuery :: FtsTarget -> Int -> Query
rankQuery target allowedCount =
  asQuery $
    "SELECT item_id,-bm25("
      <> table
      <> ","
      <> commaDoubles (ftsTargetBm25Weights target)
      <> ") AS score FROM "
      <> table
      <> " WHERE "
      <> table
      <> " MATCH ? AND item_id IN ("
      <> Text.intercalate "," (replicate allowedCount "?")
      <> ") ORDER BY score DESC,item_id DESC LIMIT ?"
  where
    table = ftsTargetTable target

rankParameters :: Text -> [Text] -> CandidateLimit -> [SQLData]
rankParameters expression itemIds limit =
  SQLText expression : map SQLText itemIds <> [SQLInteger (candidateLimitValue limit)]

commaDoubles :: [Double] -> Text
commaDoubles = Text.intercalate "," . map (Text.pack . show)

asQuery :: Text -> Query
asQuery = fromString . Text.unpack

trySql :: IO value -> IO (Either SQLError value)
trySql = try

classifySqlError :: FtsTarget -> SQLError -> RetrievalSqlError
classifySqlError target sqlException
  | any (`Text.isInfixOf` lowered) parserMarkers || unknownFtsColumn = MalformedFtsQuery target
  | otherwise = RetrievalIndexError target
  where
    lowered = Text.toLower (sqlErrorDetails sqlException)
    unknownFtsColumn = case Text.stripPrefix "no such column:" lowered of
      Nothing -> False
      Just rawColumn ->
        let column = Text.strip rawColumn
            schemaColumns = Set.fromList (ftsTargetTable target : map fst (ftsTargetColumns target))
         in not (Text.null column) && not (Set.member column schemaColumns)
    parserMarkers =
      [ "fts5: syntax error",
        "malformed match expression",
        "unterminated string"
      ]

insertMaximum :: Map Text Double -> FtsHit -> Map Text Double
insertMaximum scores hit = Map.insertWith max (ftsHitItemId hit) (ftsHitScore hit) scores

rankScores :: Map Text Double -> [FtsHit]
rankScores =
  sortBy (comparing (Down . ftsHitScore) <> comparing (Down . ftsHitItemId))
    . map (uncurry FtsHit)
    . Map.toList

mergeHitsNoCap :: [[FtsHit]] -> [FtsHit]
mergeHitsNoCap channels = rankScores (foldl' (foldl' insertMaximum) Map.empty channels)

batchesOf :: Int -> [value] -> [[value]]
batchesOf _ [] = []
batchesOf size values = take size values : batchesOf size (drop size values)
