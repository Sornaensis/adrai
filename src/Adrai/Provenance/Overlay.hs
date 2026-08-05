{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Provenance overlay SQLite schema, data types, and validation.
--
-- This module mirrors the Python prototype at
-- ADRAI_1_Source/adrai_core/provenance_cache.py.  It provides:
--
-- * Frozen record types for overlay tables
-- * The canonical overlay schema DDL (table definitions + indexes)
-- * Schema version validation
-- * Path helpers for locating the provenance overlay database
--
-- Computation / observation logic lives in separate modules.
module Adrai.Provenance.Overlay
  ( -- * Schema version
    overlaySchemaVersion,

    -- * Fingerprint type
    OverlayFingerprint,

    -- * Operation classification
    OperationClassification (..),

    -- * Line landing record
    LineLanding,
    lineLandingConfigKey,
    lineLandingOpId,
    lineLandingLineId,
    lineLandingRefName,
    lineLandingCommitOid,
    lineLandingComplete,

    -- * Provenance issue record
    ProvenanceIssue,
    provenanceIssueSeverity,
    provenanceIssueCode,
    provenanceIssueAdrId,
    provenanceIssueObjectId,
    provenanceIssuePath,
    provenanceIssueMessage,
    provenanceIssueOpId,

    -- * Observation records
    RefObservation (..),
    refObservationRefName,
    refObservationTipOid,
    refObservationObjectType,

    ObservationRoot (..),
    observationRootKind,
    observationRootName,
    observationRootCommitOid,

    CommitObservation (..),
    commitObservationOid,
    commitObservationParents,
    commitObservationAuthored,
    commitObservationCommitted,
    commitObservationSubject,
    commitObservationMessage,

    OperationCommit (..),
    operationCommitOpId,
    operationCommitCommitOid,
    operationCommitClassification,
    operationCommitAuthored,
    operationCommitCommitted,
    operationCommitSubject,
    operationCommitParents,

    ManagedPathAddition (..),
    managedPathAdditionPath,
    managedPathAdditionCommitOid,

    -- * Schema helpers
    overlaySchemaDdl,

    -- * Schema validation and creation
    overlayValid,
    createOverlaySchema,

    -- * Path helpers
    provenanceDatabasePath,
  )
where

import Adrai.Git
import Adrai.Provenance (OverlayFingerprint (..), mkOverlayFingerprint)
import Adrai.Sqlite (asQuery)
import Adrai.Types
  ( AdrId (..),
    Digest,
    OperationId (..),
    RepoPath (..)
  )
import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple
  ( Connection,
    Only (..),
    SQLData (SQLText),
    SQLError (..),
    close,
    execute,
    execute_,
    open,
    query_,
  )
import System.FilePath (takeDirectory, (</>))

-- | Constant schema version string.
overlaySchemaVersion :: Text
overlaySchemaVersion = "adrai-provenance-cache/1"

-- | Newtype for overlay observation fingerprint (SHA-256 hex).
--
-- Re-exported from 'Adrai.Provenance' via 'Adrai.Git'.
-- The newtype itself is defined in 'Adrai.Provenance'.

-- | Classification of how a commit earned its operation membership.
--
-- Mirrors the four provenance classifications in the Python prototype:
--
-- * 'OriginalClassification' — the commit introduced the first parent that
--   is the operation basis.
-- * 'CopyClassification' — the commit carries an ADRAI-Op trailer but the
--   first parent does not contain every sealed operation file.
-- * 'IntroductionClassification' — the commit integrated operation files
--   from a parent other than the first parent.
-- * 'LandingClassification' — the commit represents a logical-line landing.
data OperationClassification
  = OriginalClassification
  | CopyClassification
  | IntroductionClassification
  | LandingClassification
  deriving (Eq, Ord, Show)

operationClassificationValue :: OperationClassification -> Text
operationClassificationValue OriginalClassification = "original"
operationClassificationValue CopyClassification = "copy"
operationClassificationValue IntroductionClassification = "introduction"
operationClassificationValue LandingClassification = "landing"

-- | Record representing a single line landing in the provenance overlay.
--
-- Mirrors the Python dataclass of the same name.
data LineLanding = LineLanding
  { lineLandingConfigKey  :: Text,
    lineLandingOpId       :: Text,
    lineLandingLineId     :: Text,
    lineLandingRefName    :: Text,
    lineLandingCommitOid  :: GitOid,
    lineLandingComplete   :: Bool
  }
  deriving (Eq, Show)

-- | A provenance issue / diagnostic recorded during overlay maintenance.
--
-- Mirrors the Python dataclass of the same name.
data ProvenanceIssue = ProvenanceIssue
  { provenanceIssueSeverity :: Text,
    provenanceIssueCode     :: Text,
    provenanceIssueAdrId    :: Maybe GitOid,
    provenanceIssueObjectId :: Maybe Text,
    provenanceIssuePath     :: Maybe Text,
    provenanceIssueMessage  :: Text,
    provenanceIssueOpId     :: Maybe GitOid
  }
  deriving (Eq, Show)

-- | Observation of a reference name and its current tip.
data RefObservation = RefObservation
  { refObservationRefName   :: Text,
    refObservationTipOid    :: Text,
    refObservationObjectType :: Text
  }
  deriving (Eq, Show)

-- | An observation root — a starting point for commit discovery.
data ObservationRoot = ObservationRoot
  { observationRootKind      :: Text,
    observationRootName      :: Text,
    observationRootCommitOid :: GitOid
  }
  deriving (Eq, Show)

-- | Captured observation of a single commit.
data CommitObservation = CommitObservation
  { commitObservationOid       :: GitOid,
    commitObservationParents   :: Text,
    commitObservationAuthored  :: Integer,
    commitObservationCommitted :: Integer,
    commitObservationSubject   :: Text,
    commitObservationMessage   :: Text
  }
  deriving (Eq, Show)

-- | Operation-to-commit binding with classification metadata.
data OperationCommit = OperationCommit
  { operationCommitOpId          :: Text,
    operationCommitCommitOid     :: GitOid,
    operationCommitClassification :: OperationClassification,
    operationCommitAuthored      :: Integer,
    operationCommitCommitted     :: Integer,
    operationCommitSubject       :: Text,
    operationCommitParents       :: Text
  }
  deriving (Eq, Show)

-- | A managed path that was added in a particular commit.
data ManagedPathAddition = ManagedPathAddition
  { managedPathAdditionPath      :: Text,
    managedPathAdditionCommitOid :: GitOid
  }
  deriving (Eq, Show)

-- | Canonical overlay schema DDL as a list of @(table_name, create_statement)@.
--
-- Mirrors the Python @_create_overlay_schema()@ exactly.  Table definitions
-- are followed by index definitions in 'overlaySchemaIndexes'.
overlaySchemaDdl :: [(Text, Text)]
overlaySchemaDdl =
  [ ("meta", "CREATE TABLE meta(key TEXT PRIMARY KEY,value TEXT NOT NULL)"),
    ( "registered_operation",
      "CREATE TABLE registered_operation("
        <> "op_id TEXT PRIMARY KEY,"
        <> "adr_id TEXT,"
        <> "basis_oid TEXT NOT NULL,"
        <> "signature TEXT NOT NULL)"
    ),
    ( "registered_object",
      "CREATE TABLE registered_object("
        <> "op_id TEXT NOT NULL,"
        <> "object_id TEXT NOT NULL,"
        <> "path TEXT NOT NULL,"
        <> "blob_oid TEXT NOT NULL,"
        <> "PRIMARY KEY(op_id,object_id),"
        <> "UNIQUE(op_id,path))"
    ),
    ( "commit_observation",
      "CREATE TABLE commit_observation("
        <> "commit_oid TEXT PRIMARY KEY,"
        <> "parents_json TEXT NOT NULL,"
        <> "authored_s INTEGER NOT NULL,"
        <> "committed_s INTEGER NOT NULL,"
        <> "subject TEXT NOT NULL,"
        <> "message TEXT NOT NULL)"
    ),
    ( "observed_commit", "CREATE TABLE observed_commit(commit_oid TEXT PRIMARY KEY)"),
    ( "managed_path_addition",
      "CREATE TABLE managed_path_addition("
        <> "path TEXT NOT NULL,"
        <> "commit_oid TEXT NOT NULL,"
        <> "PRIMARY KEY(path,commit_oid))"
    ),
    ( "operation_commit",
      "CREATE TABLE operation_commit("
        <> "op_id TEXT NOT NULL,"
        <> "commit_oid TEXT NOT NULL,"
        <> "classification TEXT NOT NULL,"
        <> "authored_s INTEGER NOT NULL,"
        <> "committed_s INTEGER NOT NULL,"
        <> "subject TEXT NOT NULL,"
        <> "parents_json TEXT NOT NULL,"
        <> "PRIMARY KEY(op_id,commit_oid))"
    ),
    ( "ref_observation",
      "CREATE TABLE ref_observation("
        <> "ref_name TEXT PRIMARY KEY,"
        <> "tip_oid TEXT NOT NULL,"
        <> "object_type TEXT NOT NULL)"
    ),
    ( "observation_root",
      "CREATE TABLE observation_root("
        <> "root_kind TEXT NOT NULL,"
        <> "root_name TEXT NOT NULL,"
        <> "commit_oid TEXT NOT NULL,"
        <> "PRIMARY KEY(root_kind,root_name))"
    ),
    ( "line_config",
      "CREATE TABLE line_config("
        <> "config_key TEXT PRIMARY KEY,"
        <> "config_json TEXT NOT NULL)"
    ),
    ( "line_ref_state",
      "CREATE TABLE line_ref_state("
        <> "config_key TEXT NOT NULL,"
        <> "ref_name TEXT NOT NULL,"
        <> "tip_oid TEXT NOT NULL,"
        <> "PRIMARY KEY(config_key,ref_name))"
    ),
    ( "line_landing",
      "CREATE TABLE line_landing("
        <> "config_key TEXT NOT NULL,"
        <> "op_id TEXT NOT NULL,"
        <> "line_id TEXT NOT NULL,"
        <> "ref_name TEXT NOT NULL,"
        <> "commit_oid TEXT NOT NULL,"
        <> "complete INTEGER NOT NULL,"
        <> "PRIMARY KEY(config_key,op_id,line_id,ref_name))"
    ),
    ( "provenance_issue",
      "CREATE TABLE provenance_issue("
        <> "issue_key TEXT PRIMARY KEY,"
        <> "severity TEXT NOT NULL,"
        <> "code TEXT NOT NULL,"
        <> "adr_id TEXT,"
        <> "object_id TEXT,"
        <> "path TEXT,"
        <> "message TEXT NOT NULL,"
        <> "op_id TEXT)"
    )
  ]

-- | Indexes appended after the table definitions.
overlaySchemaIndexes :: [Text]
overlaySchemaIndexes =
  [ "CREATE INDEX idx_registered_path ON registered_object(path,op_id)",
    "CREATE INDEX idx_addition_path ON managed_path_addition(path,commit_oid)",
    "CREATE INDEX idx_operation_commit ON operation_commit(op_id,commit_oid)",
    "CREATE INDEX idx_line_landing ON line_landing(config_key,op_id)",
    "CREATE INDEX idx_provenance_issue_op ON provenance_issue(op_id,code)"
  ]

-- | Given the current semantic-revision database path, return the parent
-- directory joined with @\"provenance.sqlite\"@.
--
-- Mirrors the Python @provenance_database()@ function.
provenanceDatabasePath :: FilePath -> FilePath
provenanceDatabasePath currentDb = parent </> "provenance.sqlite"
  where
    parent = takeDirectory currentDb

-- | Check whether the overlay database exists and has the correct schema
-- version.  Returns 'False' when the file is missing, corrupt, or has an
-- unexpected schema version.
--
-- Mirrors the Python @_overlay_valid()@ function.
overlayValid :: FilePath -> IO Bool
overlayValid path = do
  result <- try @SomeException (validateSchema path)
  pure (case result of
    Right True  -> True
    _           -> False)
  where
    validateSchema :: FilePath -> IO Bool
    validateSchema dbPath = do
      conn <- open dbPath
      rows <- query_ conn "SELECT value FROM meta WHERE key='schema'" :: IO [Only Text]
      close conn
      pure (rows == [Only overlaySchemaVersion])

-- | Create the overlay schema tables and indexes, then insert the schema
-- version into the meta table.  All operations run within a single
-- transaction.
--
-- Mirrors the Python @_create_overlay_schema()@ function.
createOverlaySchema :: Connection -> IO ()
createOverlaySchema connection = do
  execute_ connection (asQuery "PRAGMA foreign_keys=OFF")
  execute_ connection (asQuery "PRAGMA journal_mode=DELETE")
  execute_ connection (asQuery "PRAGMA synchronous=NORMAL")
  forM_ overlaySchemaDdl $ \(_, ddl) ->
    execute_ connection (asQuery ddl)
  forM_ overlaySchemaIndexes $ \index ->
    execute_ connection (asQuery index)
  execute connection "INSERT INTO meta VALUES('schema',?)" [SQLText overlaySchemaVersion]
