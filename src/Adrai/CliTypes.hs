{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Shared CLI command types and their JSON projections.
-- This module is imported by both "Adrai.Cli" and "Adrai.CliRunner"
-- to break the module dependency cycle.
module Adrai.CliTypes
  ( ViewMode (..),
    RetrievalMode (..),
    RelevantSource (..),
    ShowCommand (..),
    showCommandJson,
    HistoryCommand (..),
    historyCommandJson,
    SearchCommand (..),
    searchCommandJson,
    RelevantCommand (..),
    relevantCommandJson,
    CompareCommand (..),
    compareCommandJson,
    toAesonValue,
  )
where

import Adrai.Graph (lookupReducedAdr)
import Adrai.History
  ( HistoryError (..),
    HistoryOptions (..),
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
import Adrai.Types
  ( ViewMode (..),
    RepoPath (..),
    ActorKind (..),
    AdrId,
    RevisionSelector (AtRevision),
    mkAdrId,
    mkRepoPath,
    RepoPathViolation (..),
  )
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Vector as Vector
import Data.Aeson ( (.=) )
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple (Connection, open)
import Adrai.Format.Json (JsonValue (..))

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

-- | Parse an 'AdrId' from text, raising a runtime error on failure.
requireAdrId :: Text -> AdrId
requireAdrId value =
  case mkAdrId value of
    Left v  -> error ("invalid AdrId: " <> show v)
    Right a -> a

-- | CLI argument representation for the @show@ command.
data ShowCommand = ShowCommand
  { showAdrId       :: Text
  , showView        :: ViewMode         -- CollapsedView | ExplodedView
  , showJson        :: Bool             -- output JSON
  , showRaw         :: Bool             -- include raw_semantic
  , showRich        :: Bool             -- rich detail
  }
  deriving (Eq, Show)

-- | Dispatch a show command: either render to bytes or emit JSON.
showCommandJson :: ReadSnapshot -> ShowCommand -> IO Aeson.Value
showCommandJson snapshot cmd = case showView cmd of
  CollapsedView ->
    case projectCollapsed projMode snapshot adr of
      Left err -> pure $ Aeson.object
        [ "schema" .= Aeson.String "adrai/show-collapsed/v1"
        , "error"  .= Aeson.String (Text.pack (show err))
        ]
      Right proj -> pure (toAesonValue (collapsedProjectionJson proj))
  ExplodedView ->
    case projectExploded opts snapshot adr of
      Left err -> pure $ Aeson.object
        [ "schema" .= Aeson.String "adrai/show-exploded/v1"
        , "error"  .= Aeson.String (Text.pack (show err))
        ]
      Right proj -> pure (toAesonValue (explodedProjectionJson proj))
  where
    projMode = if showRich cmd then RichProjection else CompactProjection
    opts     = ExplodedOptions { explodedIncludeRawSemantic = showRaw cmd }
    adr      = requireAdrId (showAdrId cmd)

-- | CLI argument representation for the @history@ command.
data HistoryCommand = HistoryCommand
  { historyAdrId    :: Maybe Text  -- optional ADR prefix to filter
  , historyOrder    :: HistoryOrder
  , historyLimit    :: Int
  , historyActor    :: Maybe (Text, Text)  -- (kind, model)
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

-- | Resolve an optional ADR reference text into an AdrId for history queries.
resolveAdrReference :: ReadSnapshot -> Text -> Either HistoryError AdrId
resolveAdrReference snapshot ref
  | Text.null ref  = Left (HistoryAdrNotFound (case mkAdrId (Text.empty :: Text) of Right a -> a; Left _ -> unsafeCoerce (Text.empty :: Text)))
  | otherwise = case mkAdrId ref of
      Left v    -> Left (HistoryAdrNotFound (case mkAdrId (Text.pack (show v)) of Right a -> a; Left _ -> unsafeCoerce (Text.pack (show v))))
      Right adr -> case lookupReducedAdr adr (readSnapshotReduction snapshot) of
        Nothing -> Left (HistoryAdrNotFound adr)
        Just _  -> Right adr

adrFromText :: Text -> AdrId
adrFromText t = case mkAdrId t of
  Right a -> a
  Left _  -> unsafeCoerce t

-- | Dispatch a history command on a read snapshot.
historyCommandJson :: ReadSnapshot -> HistoryCommand -> IO Aeson.Value
historyCommandJson snapshot cmd =
  let options = HistoryOptions
        { historyOptionOrder = if historyReverse cmd then OldestFirst else NewestFirst
        , historyOptionLimit = min (max (historyLimit cmd) 1) 1000
        , historyOptionActor = case historyActor cmd of
            Nothing -> Nothing
            Just (kind, model) -> ActorSelector <$> textToActorKind kind <*> pure model
        , historyOptionSince = historySince cmd
        , historyOptionUntil = historyUntil cmd
        }
      adrRef = case historyAdrId cmd of
        Nothing  -> Left (HistoryAdrNotFound (adrFromText (Text.empty :: Text)))
        Just ref -> resolveAdrReference snapshot ref
  in case adrRef of
       Left err -> pure $ Aeson.object
         [ "schema" .= Aeson.String "adrai/history/v1"
         , "error"  .= Aeson.String (Text.pack (show err))
         ]
       Right adr ->
         case projectHistory snapshot (Just adr) options of
           Left err -> pure $ Aeson.object
             [ "schema" .= Aeson.String "adrai/history/v1"
             , "error"  .= Aeson.String (Text.pack (show err))
             ]
           Right proj -> pure (toAesonValue (historyProjectionJson proj))

-- | CLI argument representation for the @search@ command.
data SearchCommand = SearchCommand
  { searchQuery          :: Text
  , searchMode           :: RetrievalMode
  , searchView           :: ViewMode
  , searchFile           :: Maybe Text  -- RepoPath as text
  , searchDomains        :: [Text]
  , searchActor          :: Maybe (Text, Text)  -- (kind, model)
  , searchSince          :: Maybe Integer
  , searchUntil          :: Maybe Integer
  , searchIncludeObsolete :: Bool
  , searchLimit          :: Int
  , searchJson           :: Bool
  } deriving (Eq, Show)

-- | Dispatch a search command on a read snapshot.  Uses an empty
-- 'SearchMaterialization' since the CLI does not materialize
-- documents inline; FTS-only retrieval still works correctly.
searchCommandJson :: ReadSnapshot -> Connection -> SearchCommand -> IO Aeson.Value
searchCommandJson snapshot conn cmd = do
  let request = SearchRequest
        { searchRequestQuery = searchQuery cmd
        , searchRequestMode = searchMode cmd
        , searchRequestView = searchView cmd
        , searchRequestIncludeObsolete = searchIncludeObsolete cmd
        , searchRequestDomains = searchDomains cmd
        , searchRequestFile = searchFile cmd >>= \p ->
            case mkRepoPath p of
              Left _  -> Nothing
              Right rp -> Just rp
        , searchRequestActor = searchActor cmd >>= \(kind, model) ->
            ActorSelector <$> textToActorKind kind <*> pure model
        , searchRequestSince = searchSince cmd
        , searchRequestUntil = searchUntil cmd
        , searchRequestLimit = min (max (searchLimit cmd) 1) 1000
        , searchRequestShallowHistory = False
        }
      materialization = SearchMaterialization [] [] []
  result <- runCurrentSearch conn snapshot materialization request
  case result of
    Left err -> pure $ Aeson.object
      [ "schema" .= Aeson.String "adrai/search/v1"
      , "error"  .= Aeson.String (Text.pack (show err))
      ]
    Right proj -> pure (toAesonValue (searchProjectionJson proj))

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
