{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | CLI-facing result types and their JSON projections for compile
-- and doctor commands.  Uses Data.Aeson directly (no Adrei.Format.Json
-- dependency) so these pure projections are easy to consume from
-- callers that only want serialised output.
module Adrai.Cli
  ( CompileResult (..),
    coldCompilerToCompileResult,
    compileResultJson,
    DoctorOutput (..),
    DoctorIssue (..),
    DoctorCounts (..),
    DoctorCacheAccess (..),
    DoctorDatabaseBuild (..),
    doctorOutputJson,
    doctorIssueJson,
    doctorCountsJson,
    doctorCacheAccessJson,
    doctorDatabaseBuildJson,
    ShowCommand (..),
    showCommandJson,
    HistoryCommand (..),
    historyCommandJson,
    run,
  )
where

import Adrai.Compiler (ColdCompilerResult (..))
import Adrai.Format.Json (JsonValue (..))
import Adrai.History
  ( HistoryError (..),
    HistoryOptions (..),
    HistoryOrder (..),
    ActorSelector (..),
    projectHistory,
    historyProjectionJson,
    ReadSnapshot (..),
  )
import Adrai.Graph (lookupReducedAdr)
import Adrai.Query
  ( ProjectionMode (..),
    projectCollapsed,
    projectExploded,
    collapsedProjectionJson,
    explodedProjectionJson,
    ExplodedOptions (..),
    explodedIncludeRawSemantic,
  )
import Adrai.Sqlite (ColdDatabaseStats (..))
import Adrai.Types
  ( ViewMode (..),
    ActorKind (..),
    AdrId,
    mkAdrId,
  )
import Unsafe.Coerce (unsafeCoerce)
import Data.Aeson ( (.=) )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Vector as Vector
import Data.List (sortOn)

import Data.Text (Text)
import qualified Data.Text as Text

-- | Helper for optional JSON fields: returns 'Aeson.Null' for 'Nothing',
-- otherwise applies the projection function to the wrapped value.
maybeJson :: (a -> Aeson.Value) -> Maybe a -> Aeson.Value
maybeJson _ Nothing  = Aeson.Null
maybeJson f (Just v) = f v

-- | Result shape for the @compile@ command output.
-- Mirrors the Python @CompileResult@ dataclass so JSON consumers
-- can reason about a single stable schema.
data CompileResult = CompileResult
  { coldCompilerDatabase             :: FilePath
  , coldCompilerRevision             :: Text
  , coldCompilerIssueCount           :: Int
  , coldCompilerErrorCount           :: Int
  , coldCompilerWarningCount         :: Int
  , coldCompilerEmbeddingComputed    :: Int
  , coldCompilerEmbeddingReused      :: Int
  , coldCompilerCacheMode            :: Text
  , coldCompilerDocumentsParsed      :: Int
  , coldCompilerDocumentsReused      :: Int
  , coldCompilerHistoryCommitsScanned :: Int
  , coldCompilerIncrementalKind      :: Text
  , coldCompilerAdrsRebuilt          :: Int
  , coldCompilerAdrsReused           :: Int
  , coldCompilerAnnBuckets           :: Int
  , coldCompilerCacheKey             :: Text
  , coldCompilerCacheRetainRevisions :: Int
  }
  deriving (Eq, Show)

-- | Convert a @ColdCompilerResult@ into the CLI-facing
-- @CompileResult@.  Only the database path and pure stats are
-- required; the rest are filled with safe defaults that will be
-- overridden by subsequent phases (e.g. P4-05.2 for cache mode).
coldCompilerToCompileResult
  :: ColdCompilerResult
  -> FilePath  -- database path
  -> CompileResult
coldCompilerToCompileResult stats dbPath =
  let s = coldCompilerDatabaseStats stats
      issueCount = coldDatabaseIssueCount s
      conflictCount = coldDatabaseConflictCount s
      -- Errors are modelled as conflicts; warnings are derived
      -- as the remainder of issues beyond conflicts.
      errorCount = conflictCount
      warningCount = max 0 (issueCount - conflictCount)
   in CompileResult
        { coldCompilerDatabase = dbPath
        , coldCompilerRevision = coldDatabaseSemanticState s
        , coldCompilerIssueCount = issueCount
        , coldCompilerErrorCount = errorCount
        , coldCompilerWarningCount = warningCount
        , coldCompilerEmbeddingComputed = 0
        , coldCompilerEmbeddingReused = 0
        , coldCompilerCacheMode = "full"
        , coldCompilerDocumentsParsed = coldDatabaseManagedSourceCount s
        , coldCompilerDocumentsReused = 0
        , coldCompilerHistoryCommitsScanned = 0
        , coldCompilerIncrementalKind = "full"
        , coldCompilerAdrsRebuilt = coldDatabaseOperationCount s
        , coldCompilerAdrsReused = 0
        , coldCompilerAnnBuckets = coldDatabaseSearchDocumentCount s
        , coldCompilerCacheKey = ""
        , coldCompilerCacheRetainRevisions = 12
        }

-- | Serialise a @CompileResult@ to an Aeson 'Aeson.Value' object.
-- Keys are sorted at render time for deterministic output.
compileResultJson :: CompileResult -> Aeson.Value
compileResultJson result = Aeson.object $ sortOn fst
  [ "adrs_rebuilt"             .= Aeson.Number (fromIntegral (coldCompilerAdrsRebuilt result))
  , "adrs_reused"              .= Aeson.Number (fromIntegral (coldCompilerAdrsReused result))
  , "ann_buckets"              .= Aeson.Number (fromIntegral (coldCompilerAnnBuckets result))
  , "cache_key"                .= Aeson.String (coldCompilerCacheKey result)
  , "cache_mode"               .= Aeson.String (coldCompilerCacheMode result)
  , "cache_retain_revisions"   .= Aeson.Number (fromIntegral (coldCompilerCacheRetainRevisions result))
  , "database"                 .= Aeson.String (Text.pack (coldCompilerDatabase result))
  , "documents_parsed"         .= Aeson.Number (fromIntegral (coldCompilerDocumentsParsed result))
  , "documents_reused"         .= Aeson.Number (fromIntegral (coldCompilerDocumentsReused result))
  , "embedding_computed"       .= Aeson.Number (fromIntegral (coldCompilerEmbeddingComputed result))
  , "embedding_reused"         .= Aeson.Number (fromIntegral (coldCompilerEmbeddingReused result))
  , "errors"                   .= Aeson.Number (fromIntegral (coldCompilerErrorCount result))
  , "history_commits_scanned"  .= Aeson.Number (fromIntegral (coldCompilerHistoryCommitsScanned result))
  , "incremental_kind"         .= Aeson.String (coldCompilerIncrementalKind result)
  , "issues"                   .= Aeson.Number (fromIntegral (coldCompilerIssueCount result))
  , "revision"                 .= Aeson.String (coldCompilerRevision result)
  , "warnings"                 .= Aeson.Number (fromIntegral (coldCompilerWarningCount result))
  ]

-- | A single diagnostic issue discovered during @doctor@.
data DoctorIssue = DoctorIssue
  { doctorIssueSeverity   :: Text
  , doctorIssueCode       :: Text
  , doctorIssueMessage    :: Text
  , doctorIssueAdrId      :: Maybe Text
  , doctorIssueObjectId   :: Maybe Text
  , doctorIssuePath       :: Maybe Text
  , doctorIssueStateToken :: Maybe Text  -- for ADR_CONFLICT
  , doctorIssueConflicts  :: [Aeson.Value]  -- for ADR_CONFLICT
  }
  deriving (Eq, Show)

-- | Snapshot of cache access metrics during a doctor pass.
data DoctorCacheAccess = DoctorCacheAccess
  { doctorCacheMode             :: Text
  , doctorIncrementalKind       :: Text
  , doctorDocumentsParsed       :: Int
  , doctorDocumentsReused       :: Int
  , doctorAdrsRebuilt           :: Int
  , doctorAdrsReused            :: Int
  , doctorEmbeddingsComputed    :: Int
  , doctorEmbeddingsReused      :: Int
  , doctorHistoryCommitsScanned :: Int
  }
  deriving (Eq, Show)

-- | Summary of document / ADR counts from a cache access snapshot.
data DoctorCounts = DoctorCounts
  { doctorErrorCount   :: Int
  , doctorWarningCount :: Int
  }
  deriving (Eq, Show)

-- | Low-level cache configuration status entries.
data DoctorDatabaseBuild = DoctorDatabaseBuild
  { dbBuildSourceRevision        :: Maybe Text
  , dbBuildDocumentCount         :: Maybe Int
  , dbBuildAdrCount              :: Maybe Int
  , dbBuildProjectionCount       :: Maybe Int
  , dbBuildAnnBucketCount        :: Maybe Int
  , dbBuildDocumentsParsed       :: Maybe Int
  , dbBuildDocumentsReused       :: Maybe Int
  , dbBuildAdrsRebuilt           :: Maybe Int
  , dbBuildAdrsReused            :: Maybe Int
  , dbBuildEmbeddingComputed     :: Maybe Int
  , dbBuildEmbeddingReused       :: Maybe Int
  , dbBuildReuseSourceRevision   :: Maybe Text
  }
  deriving (Eq, Show)

-- | Full output shape for the @doctor@ command.
data DoctorOutput = DoctorOutput
  { doctorOk            :: Bool
  , doctorRevision      :: Text
  , doctorDatabase      :: Maybe FilePath
  , doctorShallow       :: Bool
  , doctorIssues        :: [DoctorIssue]
  , doctorCacheStatus   :: [Aeson.Value]  -- from cache_config_status
  , doctorCounts        :: DoctorCounts
  , doctorCurrentAccess :: Maybe DoctorCacheAccess
  , doctorDatabaseBuild :: Maybe DoctorDatabaseBuild
  }
  deriving (Eq, Show)

-- | Serialise a @DoctorOutput@ to an Aeson 'Aeson.Value' object.
doctorOutputJson :: DoctorOutput -> Aeson.Value
doctorOutputJson output = Aeson.object $ sortOn fst
  [ "cache"            .= Aeson.Array (Vector.fromList (doctorCacheStatus output))
  , "counts"           .= doctorCountsJson (doctorCounts output)
  , "current_access"   .= maybeJson doctorCacheAccessJson (doctorCurrentAccess output)
  , "database"         .= maybeJson (Aeson.String . Text.pack) (doctorDatabase output)
  , "issues"           .= Aeson.Array (Vector.fromList (map doctorIssueJson (doctorIssues output)))
  , "ok"               .= Aeson.Bool (doctorOk output)
  , "revision"         .= Aeson.String (doctorRevision output)
  , "shallow"          .= Aeson.Bool (doctorShallow output)
  , "database_build"   .= maybeJson doctorDatabaseBuildJson (doctorDatabaseBuild output)
  ]
doctorIssueJson :: DoctorIssue -> Aeson.Value
doctorIssueJson issue = Aeson.object $ sortOn fst
  [ "adr_id"      .= maybeJson (Aeson.String) (doctorIssueAdrId issue)
  , "code"        .= Aeson.String (doctorIssueCode issue)
  , "conflicts"   .= Aeson.Array (Vector.fromList (doctorIssueConflicts issue))
  , "message"     .= Aeson.String (doctorIssueMessage issue)
  , "object_id"   .= maybeJson (Aeson.String) (doctorIssueObjectId issue)
  , "path"        .= maybeJson (Aeson.String) (doctorIssuePath issue)
  , "severity"    .= Aeson.String (doctorIssueSeverity issue)
  , "state_token" .= maybeJson (Aeson.String) (doctorIssueStateToken issue)
  ]
doctorCountsJson :: DoctorCounts -> Aeson.Value
doctorCountsJson counts = Aeson.object $ sortOn fst
  [ "errors"   .= Aeson.Number (fromIntegral (doctorErrorCount counts))
  , "warnings" .= Aeson.Number (fromIntegral (doctorWarningCount counts))
  ]

-- | Serialise @DoctorCacheAccess@.
doctorCacheAccessJson :: DoctorCacheAccess -> Aeson.Value
doctorCacheAccessJson access = Aeson.object $ sortOn fst
  [ "adrs_rebuilt"            .= Aeson.Number (fromIntegral (doctorAdrsRebuilt access))
  , "adrs_reused"             .= Aeson.Number (fromIntegral (doctorAdrsReused access))
  , "cache_mode"              .= Aeson.String (doctorCacheMode access)
  , "documents_parsed"        .= Aeson.Number (fromIntegral (doctorDocumentsParsed access))
  , "documents_reused"        .= Aeson.Number (fromIntegral (doctorDocumentsReused access))
  , "embeddings_computed"     .= Aeson.Number (fromIntegral (doctorEmbeddingsComputed access))
  , "embeddings_reused"       .= Aeson.Number (fromIntegral (doctorEmbeddingsReused access))
  , "history_commits_scanned" .= Aeson.Number (fromIntegral (doctorHistoryCommitsScanned access))
  , "incremental_kind"        .= Aeson.String (doctorIncrementalKind access)
  ]

-- | Serialise @DoctorDatabaseBuild@.
doctorDatabaseBuildJson :: DoctorDatabaseBuild -> Aeson.Value
doctorDatabaseBuildJson build = Aeson.object $ sortOn fst
  [ "adrs_rebuilt"            .= maybeJson (Aeson.Number . fromIntegral) (dbBuildAdrsRebuilt build)
  , "adrs_reused"             .= maybeJson (Aeson.Number . fromIntegral) (dbBuildAdrsReused build)
  , "adr_count"               .= maybeJson (Aeson.Number . fromIntegral) (dbBuildAdrCount build)
  , "ann_bucket_count"        .= maybeJson (Aeson.Number . fromIntegral) (dbBuildAnnBucketCount build)
  , "document_count"          .= maybeJson (Aeson.Number . fromIntegral) (dbBuildDocumentCount build)
  , "documents_parsed"        .= maybeJson (Aeson.Number . fromIntegral) (dbBuildDocumentsParsed build)
  , "documents_reused"        .= maybeJson (Aeson.Number . fromIntegral) (dbBuildDocumentsReused build)
  , "embedding_computed"      .= maybeJson (Aeson.Number . fromIntegral) (dbBuildEmbeddingComputed build)
  , "embedding_reused"        .= maybeJson (Aeson.Number . fromIntegral) (dbBuildEmbeddingReused build)
  , "projection_count"        .= maybeJson (Aeson.Number . fromIntegral) (dbBuildProjectionCount build)
  , "reuse_source_revision"   .= maybeJson (Aeson.String) (dbBuildReuseSourceRevision build)
  , "source_revision"         .= maybeJson (Aeson.String) (dbBuildSourceRevision build)
  ]

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

-- | CLI scaffold entry point.  Replaced by the real CLI runner once
-- the @adrai compile@, @adrai doctor@, @adrai show@, and @adrai history@
-- commands are wired up.
run :: IO ()
run = putStrLn "ADRAI scaffold: command-line implementation pending."
