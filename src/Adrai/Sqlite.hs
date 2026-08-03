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
    initializeFtsTargets,
    runFtsTarget,
    runSummaryFtsChannels,
  )
where

import Adrai.Retrieval (QueryPlan (..))
import Adrai.Types (AdraiError (..), ExitClass (ExitUserError))
import Control.Exception (try)
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
    SQLData (SQLInteger, SQLText),
    execute_,
    field,
    query,
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

initializeFtsTargets :: Connection -> IO (Either RetrievalSqlError ())
initializeFtsTargets connection = create allFtsTargets
  where
    create [] = pure (Right ())
    create (target : targets) = do
      outcome <- trySql (execute_ connection (asQuery (ftsTargetDdl target)))
      case outcome of
        Left _ -> pure (Left (RetrievalIndexError target))
        Right () -> create targets

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
                  summaryFtsPrefixUsed = usePrefix && not (Text.null (queryPlanFtsPrefix plan))
                }
        (Left retrievalError, _, _) -> pure (Left retrievalError)
        (_, Left retrievalError, _) -> pure (Left retrievalError)
        (_, _, Left retrievalError) -> pure (Left retrievalError)

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
