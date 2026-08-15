{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | CLI-facing result types (compile, doctor) and their JSON projections.
-- Command types and their JSON projections are defined in
-- "Adrai.CliTypes" to avoid a dependency cycle with "Adrai.CliRunner".
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
    SearchCommand (..),
    searchCommandJson,
    RelevantCommand (..),
    relevantCommandJson,
    CompareCommand (..),
    compareCommandJson,
    toAesonValue,
    textToActorKind,
    HistoryOrder (..),
    CliCommand (..),
    CliParser,
    run,
  )
where

import qualified Adrai.CliRunner as CliRunner
import Adrai.CliRunner (CliCommand (..))
import Adrai.CliTypes
  ( CompileResult (..),
    coldCompilerToCompileResult,
    compileResultJson,
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
    textToActorKind,
  )
import Adrai.History (HistoryOrder (..))
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import Data.Aeson ( (.=) )
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Type alias re-exported from CliRunner for CLI-facing code.
type CliParser = CliRunner.CliParser

-- | Helper for optional JSON fields: returns 'Aeson.Null' for 'Nothing',
-- otherwise applies the projection function to the wrapped value.
maybeJson :: (a -> Aeson.Value) -> Maybe a -> Aeson.Value
maybeJson _ Nothing  = Aeson.Null
maybeJson f (Just v) = f v

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

-- | CLI runner: delegates to Adrai.CliRunner for parsing and dispatch.
run :: IO ()
run = CliRunner.run
