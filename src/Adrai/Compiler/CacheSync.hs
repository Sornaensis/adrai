{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Overlay-to-cache provenance synchronization.
--
-- This module implements the sync path described in the Python prototype
-- @adrai_core/compiler.py@ @_sync_provenance_snapshot()@.  It attaches the
-- provenance overlay SQLite database and copies rows from overlay tables
-- into the corresponding cache tables using @INSERT OR REPLACE@ for
-- idempotency.
--
-- All sync operations are wrapped in a single SQLite transaction so that
-- either every table is updated or none are.
--
-- Mirrors: @ADRAI_1_Source/adrai_core/compiler.py@ ::
--   @_sync_provenance_snapshot()@

module Adrai.Compiler.CacheSync
  ( SyncResult (..),
    syncProvenanceSnapshot,
  )
where

import Adrai.Sqlite (asQuery)
import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Text as Text
import Data.Int (Int64)
import Data.Time.Clock (DiffTime, getCurrentTime, secondsToDiffTime, UTCTime, utctDay, utctDayTime)
import Data.Time.Calendar (fromGregorian, toModifiedJulianDay)
import Database.SQLite.Simple
  ( Connection,
    Only (..),
    SQLData (SQLText, SQLInteger, SQLNull),
    execute,
    execute_,
    query,
    query_,
    withTransaction,
    open,
    close,
  )

-- | Row counts for each synced table plus metadata.
--
-- All fields use @Int@ for easy consumption by callers that need to
-- verify sync progress (e.g. asserting zero provenance errors).
data SyncResult = SyncResult
  { syncOperationCommits :: Int
  , syncLineLandings     :: Int
  , syncRefObservations  :: Int
  , syncProvenanceIssues :: Int
  , syncMetaKeys         :: Int
  }
  deriving (Eq, Show)

-- | Synchronize provenance overlay tables into the semantic cache DB.
--
-- Opens a separate connection to the overlay database and copies rows
-- into the cache using @INSERT OR REPLACE@ for idempotency.  All
-- operations are wrapped in a single transaction on the cache.
--
-- Arguments:
--
--   * /cacheConn/ — connection to the cache (semantic-revision) database
--   * /overlayConn/ — already-open connection to the overlay database
--     (kept for callers that opened it; the function attaches by path
--     rather than reusing this connection)
--   * /overlayPath/ — file system path to the overlay database, used for
--     @ATTACH@ so the cache connection can query overlay tables directly
--   * /operationIds/ — space-separated operation IDs; rows in overlay
--     tables whose @op_id@ is absent from this list are skipped
--   * /lineConfigKey/ — the line-config key used to filter
--     @line_landing@ rows
--   * /refObservations/ — current ref observations as @(ref_name,
--     tip_oid, object_type)@ tuples (not read from the overlay)
--   * /fingerprint/ — provenance fingerprint (SHA-256 hex)
--   * /generation/ — provenance generation counter
--   * /observedCommitCount/ — number of commits observed during the run
--   * /currentHead/ — OID of the current HEAD commit
--   * /currentRef/ — name of the current reference (e.g.
--     @refs/heads/main@)
--   * /currentUpstream/ — upstream reference name
--   * /historyCommitsScanned/ — total commits scanned in history walk
--   * /cacheMode/ — compilation mode string (e.g. @"full"@,
--     @"incremental"@)
--   * /incrementalKind/ — incremental kind (e.g. @"provenance_delta"@)
--
-- Returns a 'SyncResult' with row counts for verification.
syncProvenanceSnapshot
  :: Connection   -- ^ cache DB connection
  -> Connection   -- ^ overlay DB connection (unused after attach; kept for caller)
  -> FilePath     -- ^ path to overlay DB (for ATTACH)
  -> Text         -- ^ operation IDs (space-separated or list)
  -> Text         -- ^ line config key
  -> [(Text, Text, Text)]  -- ^ ref observations (ref_name, tip_oid, object_type)
  -> Text         -- ^ provenance fingerprint
  -> Int          -- ^ provenance generation
  -> Int          -- ^ observed commit count
  -> Text         -- ^ current head OID
  -> Text         -- ^ current ref name
  -> Text         -- ^ current upstream name
  -> Int          -- ^ history commits scanned
  -> Text         -- ^ cache mode
  -> Text         -- ^ incremental kind
  -> IO SyncResult
syncProvenanceSnapshot
  cacheConn
  _overlayConn
  overlayPath
  operationIds
  lineConfigKey
  refObservations
  fingerprint
  generation
  observedCommitCount
  currentHead
  currentRef
  currentUpstream
  historyCommitsScanned
  _cacheMode
  _incrementalKind = do
    -- Open a separate connection to the overlay database; avoids ATTACH
    -- locking issues that occur on Windows when two connections share
    -- the same SQLite file.
    overlayConn <- open overlayPath
    -- DEBUG: write overlay path and table info to file
    overlayTables <- query_ overlayConn "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"
      :: IO [Only Text]
    overlayColumns <- query_ overlayConn "PRAGMA table_info(operation_commit)" :: IO [(Int,Text,Text,Int,Maybe Text,Int)]
    let debugLine = overlayPath <> "|" <> show overlayTables <> "|" <> show overlayColumns
    TIO.appendFile "cachesync_debug.txt" (Text.pack (debugLine <> "\n"))
    -- Also read meta table to verify the connection
    _meta <- query_ overlayConn "SELECT value FROM meta WHERE key='schema'" :: IO [Only Text]
    let metaDebug = "meta=" <> show _meta
    TIO.appendFile "cachesync_debug.txt" (Text.pack ("META:" <> metaDebug <> "\n"))
    -- 2-col test OUTSIDE withTransaction
    _t2 <- query_ overlayConn "SELECT op_id,commit_oid FROM operation_commit LIMIT 1"
      :: IO [(Text, Text)]
    TIO.appendFile "cachesync_debug.txt" (Text.pack ("T2:" <> show _t2 <> "\n"))
    -- 7-col test OUTSIDE withTransaction
    _t7 <- query_ overlayConn "SELECT op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json FROM operation_commit LIMIT 1"
      :: IO [(Text, Text, Text, Int64, Int64, Text, Text)]
    TIO.appendFile "cachesync_debug.txt" (Text.pack ("T7:" <> show _t7 <> "\n"))
    -- -----------------------------------------------------------------------
    -- Read all overlay data BEFORE entering the cache transaction to avoid
    -- SQLite locking interaction between withTransaction (BEGIN on cacheConn)
    -- and overlay queries on overlayConn on Windows.
    -- -----------------------------------------------------------------------
    let opIds = Text.words operationIds

    overlayOpCommits <- query_ overlayConn
      "SELECT op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json FROM operation_commit"
        :: IO [(Text, Text, Text, Int64, Int64, Text, Text)]

    overlayLandings <-
      if null opIds
        then return []
        else query_ overlayConn
          "SELECT config_key,op_id,line_id,ref_name,commit_oid,complete FROM line_landing"
            :: IO [(Text, Text, Text, Text, Text, Int64)]

    overlayIssues <-
      if null opIds
        then return []
        else query_ overlayConn
          "SELECT severity,code,adr_id,object_id,path,message,op_id FROM provenance_issue"
            :: IO [(Text, Text, Maybe Text, Maybe Text, Maybe Text, Text, Maybe Text)]

    result <- withTransaction cacheConn $ do
      -- -----------------------------------------------------------------------
      -- Build a temporary table of the current operation IDs so we can filter
      -- overlay rows by operation membership.
      -- -----------------------------------------------------------------------
      execute_ cacheConn (asQuery "DROP TABLE IF EXISTS current_op")
      execute_ cacheConn (asQuery "CREATE TEMP TABLE current_op(op_id TEXT PRIMARY KEY)")

      when (not (null opIds)) $ do
        forM_ opIds $ \opId ->
          execute cacheConn (asQuery "INSERT INTO current_op VALUES(?)")
            [SQLText opId]

      -- Clear operation_commit before re-inserting filtered overlay rows
      -- so re-syncs with different op_ids are idempotent (no stale rows).
      execute_ cacheConn (asQuery "DELETE FROM operation_commit")

      let filteredOpCommits = filter (\(oid,_,_,_,_,_,_) -> oid `elem` opIds)
                              overlayOpCommits

      forM_ filteredOpCommits $ \(opId, commitOid, classification, authored, committed, subject, parents) ->
        execute cacheConn
          (asQuery "INSERT OR REPLACE INTO operation_commit(op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json) VALUES(?,?,?,?,?,?,?)")
          [ SQLText opId
          , SQLText commitOid
          , SQLText classification
          , SQLInteger (fromIntegral authored)
          , SQLInteger (fromIntegral committed)
          , SQLText subject
          , SQLText parents
          ]

      opCommits <- query_ cacheConn "SELECT count(*) FROM operation_commit" :: IO [Only Int64]
      let syncOpCommits = case opCommits of
            [Only c] -> fromIntegral c
            _        -> 0

      -- -----------------------------------------------------------------------
      -- Sync: line_landing
      -- DELETE existing rows, then re-insert filtered by config_key and
      -- operation membership.
      -- -----------------------------------------------------------------------
      execute_ cacheConn (asQuery "DELETE FROM line_landing")

      when (not (null opIds)) $ do
        let filteredLandings = filter (\(ck, oid, _, _, _, _) ->
                ck == lineConfigKey && oid `elem` opIds)
                               overlayLandings

        forM_ filteredLandings $ \(ck, oid, lid, rn, co, complete) ->
          execute cacheConn
            (asQuery "INSERT INTO line_landing(config_key,op_id,line_id,ref_name,commit_oid,complete) VALUES(?,?,?,?,?,?)")
            [ SQLText ck
            , SQLText oid
            , SQLText lid
            , SQLText rn
            , SQLText co
            , SQLInteger (fromIntegral complete)
            ]

      lineLandings <- query_ cacheConn "SELECT count(*) FROM line_landing" :: IO [Only Int64]
      let syncLineLandings = case lineLandings of
            [Only c] -> fromIntegral c
            _        -> 0

      -- -----------------------------------------------------------------------
      -- Sync: ref_observation (direct from Haskell, not from overlay)
      -- Uses INSERT OR REPLACE keyed on ref_name (primary key).
      -- -----------------------------------------------------------------------
      execute_ cacheConn (asQuery "DELETE FROM ref_observation")

      when (not (null refObservations)) $ do
        forM_ refObservations $ \(refName, tipOid, objectType) ->
          execute cacheConn
            (asQuery "INSERT OR REPLACE INTO ref_observation(ref_name,tip_oid,object_type) VALUES(?,?,?)")
            [SQLText refName, SQLText tipOid, SQLText objectType]

      refObs <- query_ cacheConn "SELECT count(*) FROM ref_observation" :: IO [Only Int64]
      let syncRefObs = case refObs of
            [Only c] -> fromIntegral c
            _        -> 0

      -- -----------------------------------------------------------------------
      -- Sync: provenance_issue → issue (with origin='provenance')
      -- Filters to only rows whose op_id is in current_op.
      -- Deletes existing origin='provenance' issues first.
      -- -----------------------------------------------------------------------
      execute_ cacheConn (asQuery "DELETE FROM issue WHERE origin='provenance'")

      when (not (null opIds)) $ do
        let filteredIssues = filter (\(_,_,_,_,_,_,oid) -> maybe False (\o -> o `elem` opIds) oid || isNothing oid)
                                   overlayIssues

        forM_ filteredIssues $ \(sev, code, adr, obj, path, msg, oid) ->
          execute cacheConn
            (asQuery "INSERT INTO issue(severity,code,adr_id,object_id,path,message,origin,operation_id) VALUES(?,?,?,?,?,?,?,?)")
            [ SQLText sev
            , SQLText code
            , maybe SQLNull (SQLText) adr
            , maybe SQLNull (SQLText) obj
            , maybe SQLNull (SQLText) path
            , SQLText msg
            , SQLText "provenance"
            , maybe SQLNull (SQLText) oid
            ]

      issueCount <- query_ cacheConn "SELECT count(*) FROM issue WHERE origin='provenance'"
        :: IO [Only Int64]
      let syncIssues = case issueCount of
            [Only c] -> fromIntegral c
            _        -> 0

      -- -----------------------------------------------------------------------
      -- Update cache meta table with provenance metadata.
      -- Uses INSERT OR REPLACE (upsert) for each key.
      -- -----------------------------------------------------------------------
      now <- getCurrentTime
      let epochSec = round (timeToEpochSec now)
            where
              timeToEpochSec :: UTCTime -> DiffTime
              timeToEpochSec t =
                let daysSinceEpoch = toModifiedJulianDay (utctDay t) - toModifiedJulianDay (fromGregorian 1970 1 1)
                    timeInDay :: DiffTime
                    timeInDay = utctDayTime t
                in  secondsToDiffTime (daysSinceEpoch * 86400) + timeInDay
          metaValues =
            [ ("provenance.fingerprint", fingerprint),
              ("provenance.generation", Text.pack (show generation)),
              ("provenance.observed_commit_count", Text.pack (show observedCommitCount)),
              ("provenance.current_head", currentHead),
              ("provenance.current_ref", currentRef),
              ("provenance.current_upstream", currentUpstream),
              ("provenance.history_commits_scanned", Text.pack (show historyCommitsScanned)),
              ("provenance.sync_timestamp", Text.pack (show epochSec))
            ]

      forM_ metaValues $ \(key, value) ->
        execute cacheConn
          (asQuery "INSERT OR REPLACE INTO meta(key,value) VALUES(?,?)")
          [SQLText key, SQLText value]

      let syncMetaKeys = length metaValues

      pure SyncResult
        { syncOperationCommits = syncOpCommits
        , syncLineLandings     = syncLineLandings
        , syncRefObservations  = syncRefObs
        , syncProvenanceIssues = syncIssues
        , syncMetaKeys         = syncMetaKeys
        }
    close overlayConn
    pure result

-- | Escape a file path for embedding in a SQL string literal.
--
-- SQLite uses single quotes to delimit string literals.  Any single quote
-- in the path is doubled.  The result is wrapped in single quotes.
escapeSqlPath :: FilePath -> Text
escapeSqlPath path =
  "'" <> Text.pack (escapeChar path) <> "'"
  where
    escapeChar :: FilePath -> String
    escapeChar [] = []
    escapeChar (c : cs)
      | c == '\''  = "''" <> escapeChar cs
      | otherwise  = c : escapeChar cs
