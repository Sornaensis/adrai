{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Shared CLI command types and their JSON projections.
-- This module is imported by both "Adrai.Cli" and "Adrai.CliRunner"
-- to break the module dependency cycle.
module Adrai.CliTypes
  ( CompileResult (..),
    coldCompilerToCompileResult,
    compileResultJson,
    compileResultValue,
    ViewMode (..),
    RetrievalMode (..),
    RelevantSource (..),
    ShowCommand (..),
    showCommandJson,
    HistoryCommand (..),
    historyCommandJson,
    SearchCommand (..),
    searchCommandRequest,
    searchCommandJson,
    RelevantCommand (..),
    relevantCommandJson,
    CompareCommand (..),
    compareCommandJson,
    toAesonValue,
    textToActorKind,
  )
where

import Adrai.Compiler (ColdCompilerResult (..))
import Adrai.History
  ( HistoryOptions (..),
    HistoryOrder (..),
    ActorSelector (..),
    projectHistory,
    historyProjectionJson,
    ReadSnapshot (..),
    revisionRequested,
  )
import Adrai.Query
  ( CompareOptions (..),
    CompareProjection,
    CompareSnapshot,
    compareSnapshots,
    compareProjectionJson,
    ProjectionMode (..),
    SearchRequest (..),
    SearchError (..),
    defaultSearchRequest,
    runCurrentSearch,
    searchProjectionJson,
    searchResultMatches,
    searchResultCounts,
    projectCollapsed,
    projectExploded,
    collapsedProjectionJson,
    explodedProjectionJson,
    ExplodedOptions (..),
    explodedIncludeRawSemantic,
    RelevantRequest (..),
    RelevantSource (..),
    RelevantProjection (..),
    runRelevant,
    relevantProjectionJson,
  )
import Adrai.Retrieval
  ( RetrievalMode (..),
    retrievalModeName,
    SearchMaterialization (..),
  )
import Adrai.Sqlite (ColdDatabaseStats (..))
import Adrai.Types
  ( ViewMode (..),
    RepoPath (..),
    ActorKind (..),
    actorId,
    actorKind,
    RevisionSelector (AtRevision),
    mkActor,
    mkRepoPath,
    RepoPathViolation (..),
  )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Vector as Vector
import Data.Aeson ( (.=) )
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple (Connection, open)
import Adrai.Format.Json (JsonValue (..))
import qualified Adrai.Query as Query

-- | Helper for optional JSON fields: returns 'Aeson.Null' for 'Nothing',
-- otherwise applies the projection function to the wrapped value.
maybeJson :: (a -> Aeson.Value) -> Maybe a -> Aeson.Value
maybeJson _ Nothing  = Aeson.Null
maybeJson f (Just v) = f v

-- | Convert a @JsonValue@ to an Aeson 'Aeson.Value' for JSON serialization.
toAesonValue :: JsonValue -> Aeson.Value
toAesonValue (JsonObject fields) = Aeson.object (map toAesonField fields)
  where
    toAesonField (key, val) = (Aeson.Key.fromText key, toAesonValue val)
toAesonValue (JsonArray items)    = Aeson.Array (Vector.fromList (map toAesonValue items))
toAesonValue (JsonString s)       = Aeson.String s
toAesonValue (JsonNumber n)       = Aeson.Number (fromInteger n)
toAesonValue (JsonDecimal d)      = Aeson.Number (realToFrac d)
toAesonValue (JsonBool b)         = Aeson.Bool b
toAesonValue JsonNull             = Aeson.Null

-- | Result shape for the @compile@ command output.
data CompileResult = CompileResult
  { coldCompilerDatabase :: FilePath
  , coldCompilerRevision :: Text
  , coldCompilerIssueCount :: Int
  , coldCompilerErrorCount :: Int
  , coldCompilerWarningCount :: Int
  , coldCompilerEmbeddingComputed :: Int
  , coldCompilerEmbeddingReused :: Int
  , coldCompilerCacheMode :: Text
  , coldCompilerDocumentsParsed :: Int
  , coldCompilerDocumentsReused :: Int
  , coldCompilerHistoryCommitsScanned :: Int
  , coldCompilerIncrementalKind :: Text
  , coldCompilerAdrsRebuilt :: Int
  , coldCompilerAdrsReused :: Int
  , coldCompilerAnnBuckets :: Int
  , coldCompilerCacheKey :: Text
  , coldCompilerCacheRetainRevisions :: Int
  }
  deriving (Eq, Show)

-- | Preserve the established public compile projection for direct cold builds.
coldCompilerToCompileResult :: ColdCompilerResult -> FilePath -> CompileResult
coldCompilerToCompileResult compiled database =
  let stats = coldCompilerDatabaseStats compiled
      issueCount = coldDatabaseIssueCount stats
      conflictCount = coldDatabaseConflictCount stats
   in CompileResult
        { coldCompilerDatabase = database
        , coldCompilerRevision = coldDatabaseSemanticState stats
        , coldCompilerIssueCount = issueCount
        , coldCompilerErrorCount = conflictCount
        , coldCompilerWarningCount = max 0 (issueCount - conflictCount)
        , coldCompilerEmbeddingComputed = 0
        , coldCompilerEmbeddingReused = 0
        , coldCompilerCacheMode = "full"
        , coldCompilerDocumentsParsed = coldDatabaseManagedSourceCount stats
        , coldCompilerDocumentsReused = 0
        , coldCompilerHistoryCommitsScanned = 0
        , coldCompilerIncrementalKind = "full"
        , coldCompilerAdrsRebuilt = coldDatabaseOperationCount stats
        , coldCompilerAdrsReused = 0
        , coldCompilerAnnBuckets = coldDatabaseSearchDocumentCount stats
        , coldCompilerCacheKey = ""
        , coldCompilerCacheRetainRevisions = 12
        }

compileResultJson :: CompileResult -> Aeson.Value
compileResultJson = toAesonValue . compileResultValue

-- | Canonical renderer input shared by the public executable and Aeson API.
compileResultValue :: CompileResult -> JsonValue
compileResultValue result = JsonObject
  [ ("adrs_rebuilt", JsonNumber (fromIntegral (coldCompilerAdrsRebuilt result)))
  , ("adrs_reused", JsonNumber (fromIntegral (coldCompilerAdrsReused result)))
  , ("ann_buckets", JsonNumber (fromIntegral (coldCompilerAnnBuckets result)))
  , ("cache_key", JsonString (coldCompilerCacheKey result))
  , ("cache_mode", JsonString (coldCompilerCacheMode result))
  , ("cache_retain_revisions", JsonNumber (fromIntegral (coldCompilerCacheRetainRevisions result)))
  , ("database", JsonString (Text.pack (coldCompilerDatabase result)))
  , ("documents_parsed", JsonNumber (fromIntegral (coldCompilerDocumentsParsed result)))
  , ("documents_reused", JsonNumber (fromIntegral (coldCompilerDocumentsReused result)))
  , ("embedding_computed", JsonNumber (fromIntegral (coldCompilerEmbeddingComputed result)))
  , ("embedding_reused", JsonNumber (fromIntegral (coldCompilerEmbeddingReused result)))
  , ("errors", JsonNumber (fromIntegral (coldCompilerErrorCount result)))
  , ("history_commits_scanned", JsonNumber (fromIntegral (coldCompilerHistoryCommitsScanned result)))
  , ("incremental_kind", JsonString (coldCompilerIncrementalKind result))
  , ("issues", JsonNumber (fromIntegral (coldCompilerIssueCount result)))
  , ("revision", JsonString (coldCompilerRevision result))
  , ("warnings", JsonNumber (fromIntegral (coldCompilerWarningCount result)))
  ]

-- | CLI argument representation for the @show@ command.
data ShowCommand = ShowCommand
  { showAdrId       :: Text
  , showView        :: ViewMode         -- CollapsedView | ExplodedView
  , showAt          :: Text             -- immutable revision selector
  , showJson        :: Bool             -- output JSON
  , showRaw         :: Bool             -- include raw_semantic
  }
  deriving (Eq, Show)

-- | Dispatch a show command: either render to bytes or emit JSON.
showCommandJson :: ReadSnapshot -> ShowCommand -> IO Aeson.Value
showCommandJson snapshot cmd =
  case Query.resolveAdrReference snapshot (showAdrId cmd) of
    Left err -> pure $ Aeson.object
      [ "schema" .= Aeson.String schema
      , "error" .= Aeson.String (Query.referenceLookupErrorText err)
      ]
    Right adr -> case showView cmd of
      CollapsedView ->
        case projectCollapsed projMode snapshot adr of
          Left err -> pure $ Aeson.object
            [ "schema" .= Aeson.String schema
            , "error"  .= Aeson.String (Text.pack (show err))
            ]
          Right proj -> pure (toAesonValue (collapsedProjectionJson proj))
      ExplodedView ->
        case projectExploded opts snapshot adr of
          Left err -> pure $ Aeson.object
            [ "schema" .= Aeson.String schema
            , "error"  .= Aeson.String (Text.pack (show err))
            ]
          Right proj -> pure (toAesonValue (explodedProjectionJson proj))
  where
    projMode = CompactProjection
    opts     = ExplodedOptions { explodedIncludeRawSemantic = showRaw cmd }
    schema = case showView cmd of
      CollapsedView -> "adrai/show-collapsed/v1"
      ExplodedView -> "adrai/show-exploded/v1"

-- | CLI argument representation for the @history@ command.
data HistoryCommand = HistoryCommand
  { historyAdrId    :: Maybe Text  -- optional ADR prefix to filter
  , historyAt       :: Text
  , historyLimit    :: Int
  , historyActor    :: Maybe Text
  , historySince    :: Maybe Integer
  , historyUntil    :: Maybe Integer
  , historyReverse  :: Bool
  , historyJson     :: Bool
  }
  deriving (Eq, Show)

-- | Convert CLI Text kind to ActorKind enum.
textToActorKind :: Text -> Maybe ActorKind
textToActorKind "human"  = Just HumanActor
textToActorKind "llm"    = Just LlmActor
textToActorKind "service" = Just ServiceActor
textToActorKind _        = Nothing

-- | Dispatch a history command on a read snapshot.
historyCommandJson :: ReadSnapshot -> HistoryCommand -> IO Aeson.Value
historyCommandJson snapshot cmd =
  case traverse actorSelectorFromText (historyActor cmd) of
    Left problem -> pure (errorJson problem)
    Right selector ->
      let options = HistoryOptions
            { historyOptionOrder = if historyReverse cmd then OldestFirst else NewestFirst
            , historyOptionLimit = historyLimit cmd
            , historyOptionActor = selector
            , historyOptionSince = historySince cmd
            , historyOptionUntil = historyUntil cmd
            }
          adrRef = case historyAdrId cmd of
            Nothing  -> Right Nothing
            Just ref -> Just <$> Query.resolveAdrReference snapshot ref
      in case adrRef of
           Left err -> pure (errorJson (Text.pack (show err)))
           Right adr ->
             case projectHistory snapshot adr options of
               Left err -> pure (errorJson (Text.pack (show err)))
               Right proj -> pure (toAesonValue (historyProjectionJson proj))
  where
    actorSelectorFromText value =
      case Text.splitOn ":" value of
        [kind, identifier]
          | not (Text.null identifier) ->
              case textToActorKind kind of
                Just actorKind -> Right (ActorSelector actorKind identifier)
                Nothing -> Left "actor kind must be human, llm, or service"
        _ -> Left "actor must have the form kind:identifier"
    errorJson problem =
      Aeson.object
        [ "schema" .= Aeson.String "adrai/history/v1"
        , "error" .= Aeson.String problem
        ]

-- | CLI argument representation for the @search@ command.
data SearchCommand = SearchCommand
  { searchQuery          :: Text
  , searchMode           :: RetrievalMode
  , searchView           :: ViewMode
  , searchFile           :: Maybe Text  -- RepoPath as text
  , searchDomains        :: [Text]
  , searchActor          :: Maybe Text
  , searchSince          :: Maybe Integer
  , searchUntil          :: Maybe Integer
  , searchAt             :: Text
  , searchIncludeObsolete :: Bool
  , searchLimit          :: Int
  , searchJson           :: Bool
  } deriving (Eq, Show)

-- | Dispatch a search command on a read snapshot.  Uses an empty
-- 'SearchMaterialization' since the CLI does not materialize
-- documents inline; FTS-only retrieval still works correctly.
searchCommandJson :: ReadSnapshot -> Connection -> SearchCommand -> IO Aeson.Value
searchCommandJson snapshot conn cmd =
  case searchCommandRequest cmd of
    Left problem -> pure (errorJson problem)
    Right request -> do
      let materialization = SearchMaterialization [] [] []
      result <- runCurrentSearch conn snapshot materialization request
      case result of
        Left err -> pure (errorJson (Text.pack (show err)))
        Right proj -> pure (toAesonValue (searchProjectionJson proj))
  where
    errorJson problem =
      Aeson.object
        [ "schema" .= Aeson.String "adrai/search/v1"
        , "error" .= Aeson.String problem
        ]

searchCommandRequest :: SearchCommand -> Either Text SearchRequest
searchCommandRequest cmd = do
  requestedFile <- traverse parseFile (searchFile cmd)
  requestedActor <- traverse parseActorSelector (searchActor cmd)
  Right
    SearchRequest
      { searchRequestQuery = searchQuery cmd
      , searchRequestMode = searchMode cmd
      , searchRequestView = searchView cmd
      , searchRequestIncludeObsolete = searchIncludeObsolete cmd
      , searchRequestDomains = searchDomains cmd
      , searchRequestFile = requestedFile
      , searchRequestActor = requestedActor
      , searchRequestSince = searchSince cmd
      , searchRequestUntil = searchUntil cmd
      , searchRequestLimit = searchLimit cmd
      , searchRequestShallowHistory = False
      }
  where
    parseFile value =
      case mkRepoPath value of
        Left problem -> Left ("invalid search file path: " <> Text.pack (show problem))
        Right path -> Right path
    parseActorSelector value =
      case Text.splitOn ":" value of
        [kind, identifier]
          | not (Text.null identifier) -> do
              requestedKind <- maybe (Left "search actor kind must be human, llm, or service") Right (textToActorKind kind)
              actor <- case mkActor requestedKind identifier Nothing of
                Left problem -> Left ("invalid search actor: " <> Text.pack (show problem))
                Right value -> Right value
              Right (ActorSelector (actorKind actor) (actorId actor))
        _ -> Left "search actor must have the form kind:identifier"

-- | CLI argument representation for the @relevant@ command.
data RelevantCommand = RelevantCommand
  { relevantFile             :: Text  -- RepoPath as text
  , relevantIncludeObsolete :: Bool
  , relevantLimit           :: Int
  , relevantJson            :: Bool
  } deriving (Eq, Show)

-- | Dispatch a relevant command on a read snapshot.
relevantCommandJson
  :: ReadSnapshot
  -> RelevantSource
  -> RelevantCommand
  -> IO Aeson.Value
relevantCommandJson snapshot source cmd =
  case mkRepoPath (relevantFile cmd) of
    Left err ->
      pure $ Aeson.object
        [ "schema" .= Aeson.String "adrai/relevant/v1"
        , "error"  .= Aeson.String (Text.pack (show err))
        ]
    Right repoPath -> do
      let request = RelevantRequest
            { relevantRequestFile = repoPath
            , relevantRequestRevision =
                case readSnapshotRevision snapshot of
                  ri
                    | Text.null (revisionRequested ri) -> AtRevision "HEAD"
                    | otherwise -> AtRevision (revisionRequested ri)
            , relevantRequestIncludeObsolete = relevantIncludeObsolete cmd
            , relevantRequestLimit = min (max (relevantLimit cmd) 1) 1000
            }
          materialization = SearchMaterialization [] [] []
      conn <- open ":memory:"
      result <- runRelevant conn snapshot materialization request source
      case result of
        Left err ->
          pure $ Aeson.object
            [ "schema" .= Aeson.String "adrai/relevant/v1"
            , "error"  .= Aeson.String (Text.pack (show err))
            ]
        Right proj -> pure (toAesonValue (relevantProjectionJson proj))

-- | CLI argument representation for the @compare@ command.
data CompareCommand = CompareCommand
  { compareBefore       :: Text  -- revision or path for "before" snapshot
  , compareAfter        :: Text  -- revision or path for "after" snapshot
  , compareUnchanged    :: Bool
  , compareJson         :: Bool
  } deriving (Eq, Show)

-- | Dispatch a compare command given two read snapshots.
compareCommandJson :: ReadSnapshot -> ReadSnapshot -> CompareCommand -> IO Aeson.Value
compareCommandJson beforeSnap afterSnap cmd =
  let options = CompareOptions
        { compareIncludeUnchanged = compareUnchanged cmd
        , compareCacheMetadata = []
        }
  in case compareSnapshots options beforeSnap afterSnap of
       Left err -> pure $ Aeson.object
         [ "schema" .= Aeson.String "adrai/compare/v1"
         , "error"  .= Aeson.String (Text.pack (show err))
         ]
       Right proj -> pure (toAesonValue (compareProjectionJson proj))
