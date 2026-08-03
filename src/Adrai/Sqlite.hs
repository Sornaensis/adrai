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
  )
import Adrai.Types (AdraiError (..), ExitClass (ExitUserError), adrIdText, recordIdText, stateTokenText)
import Control.Exception (Exception, catch, throwIO, try)
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
    SQLData (SQLFloat, SQLInteger, SQLText),
    execute,
    execute_,
    field,
    query,
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
  result <- tryStorage $ withTransaction connection $ do
    forM_ searchOrdinarySchemaDdl $ \(component, ddl) -> runStorage component (execute_ connection (asQuery ddl))
    forM_ allFtsTargets $ \target -> runStorage (SearchFtsStorage target) (execute_ connection (asQuery (ftsTargetDdl target)))
  pure (storageResult result)

replaceSearchMaterialization :: Connection -> SearchMaterialization -> IO (Either SearchStorageError ())
replaceSearchMaterialization connection materialization = do
  result <- tryStorage $ withTransaction connection $ do
    forM_ allFtsTargets $ \target ->
      runStorage (SearchFtsStorage target) (execute_ connection (asQuery ("DELETE FROM " <> ftsTargetTable target)))
    runStorage SearchPassageStorage (execute_ connection "DELETE FROM search_section")
    runStorage SearchAliasStorage (execute_ connection "DELETE FROM local_alias")
    runStorage SearchDocumentStorage (execute_ connection "DELETE FROM search_document")
    forM_ sortedDocuments (insertSearchDocument connection)
    forM_ sortedAliases (insertLocalAlias connection)
    forM_ numberedPassages (uncurry (insertSearchPassage connection))
    forM_ sortedDocuments $ \document ->
      forM_ [SearchExactTarget, SearchStemmedTarget, SearchIdentifierTarget] $ \target ->
        insertSummaryFts connection target document
    forM_ numberedPassages $ \(rowId, passage) ->
      forM_ [PassageExactTarget, PassageStemmedTarget, PassageIdentifierTarget] $ \target ->
        insertPassageFts connection target rowId passage
  pure (storageResult result)
  where
    sortedDocuments = sortBy (comparing searchDocumentItemId) (searchMaterializationDocuments materialization)
    sortedAliases = sortBy (comparing fst) (searchMaterializationAliases materialization)
    sortedPassages = sortBy (comparing searchPassageId) (searchMaterializationPassages materialization)
    numberedPassages = zip [1 :: Int64 ..] sortedPassages

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
