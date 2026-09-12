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
import Control.Monad (when)
import Data.Maybe (isNothing)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Int (Int64)
import Data.Time.Clock (DiffTime, getCurrentTime, secondsToDiffTime, UTCTime, utctDay, utctDayTime)
import Data.Time.Calendar (fromGregorian, toModifiedJulianDay)
import Database.SQLite.Simple
  ( Connection,
    Only (..),
    SQLData (SQLText, SQLInteger, SQLNull),
     executeMany,
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
  -> Text         -- ^ immutable provenance refresh target
  -> [Text]       -- ^ exact reachable commits for that target
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
  provenanceRefreshTarget
  targetReachableCommits
  currentHead
  currentRef
  currentUpstream
  historyCommitsScanned
  _cacheMode
  _incrementalKind = do
    -- Open a separate connection to the overlay database; avoids ATTACH
    -- locking issues that occur on Windows when two connections share
    -- the same SQLite file.
    bracket (open overlayPath) close $ \overlayConn -> do
      -- -----------------------------------------------------------------------
      -- Read all overlay data BEFORE entering the cache transaction to avoid
      -- SQLite locking interaction between withTransaction (BEGIN on cacheConn)
      -- and overlay queries on overlayConn on Windows.
      -- -----------------------------------------------------------------------
      let opIds = Text.words operationIds
          operationIdSet = Set.fromList opIds
          reachable = Set.fromList targetReachableCommits
      when
        ( null targetReachableCommits
            || length targetReachableCommits /= Set.size reachable
            || Set.notMember provenanceRefreshTarget reachable
            || any (not . validOid) targetReachableCommits
        ) $
        ioError (userError "invalid target reachability authority")

      overlayOpCommits <- query_ overlayConn
        "SELECT op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json FROM operation_commit"
          :: IO [(Text, Text, Text, Int64, Int64, Text, Text)]

      coverageTable <- query overlayConn
        "SELECT 1 FROM sqlite_master WHERE type='table' AND name='operation_target_coverage'"
        () :: IO [Only Int]
      when (null coverageTable) $
        ioError (userError "provenance overlay lacks target placement coverage")
      overlayCoverage <- query overlayConn
        "SELECT op_id,target_oid,registration_signature FROM operation_target_coverage WHERE target_oid=?"
        [SQLText provenanceRefreshTarget] :: IO [(Text, Text, Text)]
      registeredSignatures <- query_ overlayConn
        "SELECT op_id,signature FROM registered_operation"
        :: IO [(Text, Text)]
      let registrations = Map.fromList registeredSignatures
          missingRegistrations = filter (`Map.notMember` registrations) opIds
      when (not (null missingRegistrations)) $
        ioError (userError ("provenance overlay is missing requested operation registrations: " <> show missingRegistrations))
      let expectedCoverage = Set.fromList
            [ (operationId, provenanceRefreshTarget, signature)
            | operationId <- opIds
            , Just signature <- [Map.lookup operationId registrations]
            ]
      let selectedCoverage = Set.fromList [row | row@(operationId, _, _) <- overlayCoverage, Set.member operationId operationIdSet]
      when (selectedCoverage /= expectedCoverage) $
        ioError (userError "provenance overlay target placement coverage is incomplete or mismatched")

      overlayLandings <-
        if null opIds
          then return []
          else query_ overlayConn
            "SELECT config_key,op_id,line_id,ref_name,commit_oid,complete FROM line_landing"
              :: IO [(Text, Text, Text, Text, Text, Int64)]
      lineConfigs <- query overlayConn
        "SELECT config_key,config_json FROM line_config WHERE config_key=?"
        [SQLText lineConfigKey] :: IO [(Text, Text)]
      when (case lineConfigs of [(configKey', configJson')] -> configKey' /= lineConfigKey || Text.null configJson'; _ -> True) $
        ioError (userError "provenance overlay line configuration is missing or noncanonical")

      overlayIssues <-
        if null opIds
          then return []
          else query_ overlayConn
            "SELECT severity,code,adr_id,object_id,path,message,op_id FROM provenance_issue"
              :: IO [(Text, Text, Maybe Text, Maybe Text, Maybe Text, Text, Maybe Text)]

      result <- withTransaction cacheConn $ do
        -- These are provenance projections rather than semantic compiler
        -- tables.  Older immutable archives predate them, so establish the
        -- compatible projection schema in the private clone before syncing.
        execute_ cacheConn (asQuery "CREATE TABLE IF NOT EXISTS operation_commit(op_id TEXT NOT NULL,commit_oid TEXT NOT NULL,classification TEXT NOT NULL,authored_s INTEGER NOT NULL,committed_s INTEGER NOT NULL,subject TEXT NOT NULL,parents_json TEXT NOT NULL,PRIMARY KEY(op_id,commit_oid))")
        execute_ cacheConn (asQuery "CREATE TABLE IF NOT EXISTS operation_target_coverage(op_id TEXT NOT NULL,target_oid TEXT NOT NULL,registration_signature TEXT NOT NULL,PRIMARY KEY(op_id,target_oid,registration_signature))")
        execute_ cacheConn (asQuery "CREATE TABLE IF NOT EXISTS line_landing(config_key TEXT NOT NULL,op_id TEXT NOT NULL,line_id TEXT NOT NULL,ref_name TEXT NOT NULL,commit_oid TEXT NOT NULL,complete INTEGER NOT NULL,PRIMARY KEY(config_key,op_id,line_id,ref_name))")
        execute_ cacheConn (asQuery "CREATE TABLE IF NOT EXISTS target_reachable_commit(target_oid TEXT NOT NULL,commit_oid TEXT NOT NULL,PRIMARY KEY(target_oid,commit_oid))")
        execute_ cacheConn (asQuery "CREATE TABLE IF NOT EXISTS line_config(config_key TEXT PRIMARY KEY,config_json TEXT NOT NULL)")
        execute_ cacheConn (asQuery "CREATE TABLE IF NOT EXISTS ref_observation(ref_name TEXT PRIMARY KEY,tip_oid TEXT NOT NULL,object_type TEXT NOT NULL)")
        -- -----------------------------------------------------------------------
        -- Build a temporary table of the current operation IDs so we can filter
        -- overlay rows by operation membership.
        -- -----------------------------------------------------------------------
        execute_ cacheConn (asQuery "DROP TABLE IF EXISTS current_op")
        execute_ cacheConn (asQuery "CREATE TEMP TABLE current_op(op_id TEXT PRIMARY KEY)")

        executeMany cacheConn (asQuery "INSERT INTO current_op VALUES(?)")
          [[SQLText opId] | opId <- opIds]

        -- Clear operation_commit before re-inserting filtered overlay rows
        -- so re-syncs with different op_ids are idempotent (no stale rows).
        execute_ cacheConn (asQuery "DELETE FROM operation_commit")

        let filteredOpCommits = filter (\(oid,commitOid,_,_,_,_,_) -> Set.member oid operationIdSet && Set.member commitOid reachable)
                                overlayOpCommits

        executeMany cacheConn
          (asQuery "INSERT OR REPLACE INTO operation_commit(op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json) VALUES(?,?,?,?,?,?,?)")
          [ [ SQLText opId
            , SQLText commitOid
            , SQLText classification
            , SQLInteger authored
            , SQLInteger committed
            , SQLText subject
            , SQLText parents
            ]
          | (opId, commitOid, classification, authored, committed, subject, parents) <- filteredOpCommits
          ]

        opCommits <- query_ cacheConn "SELECT count(*) FROM operation_commit" :: IO [Only Int64]
        let syncOpCommits = case opCommits of
              [Only c] -> fromIntegral c
              _        -> 0

        execute_ cacheConn (asQuery "DELETE FROM operation_target_coverage")
        executeMany cacheConn
          (asQuery "INSERT INTO operation_target_coverage(op_id,target_oid,registration_signature) VALUES(?,?,?)")
          [ [SQLText opId, SQLText targetOid, SQLText signature]
          | (opId, targetOid, signature) <- overlayCoverage
          , Set.member opId operationIdSet
          ]

        -- -----------------------------------------------------------------------
        -- Sync: line_landing
        -- DELETE existing rows, then re-insert filtered by config_key and
        -- operation membership.
        -- -----------------------------------------------------------------------
        execute_ cacheConn (asQuery "DELETE FROM line_landing")
        execute_ cacheConn (asQuery "DELETE FROM target_reachable_commit")
        executeMany cacheConn
          (asQuery "INSERT INTO target_reachable_commit(target_oid,commit_oid) VALUES(?,?)")
          [[SQLText provenanceRefreshTarget, SQLText commitOid] | commitOid <- targetReachableCommits]
        execute_ cacheConn (asQuery "DELETE FROM line_config")
        executeMany cacheConn
          (asQuery "INSERT INTO line_config(config_key,config_json) VALUES(?,?)")
          [[SQLText configKey', SQLText configJson'] | (configKey', configJson') <- lineConfigs]

        when (not (null opIds)) $ do
          let filteredLandings = filter (\(ck, oid, _, _, co, _) ->
                  ck == lineConfigKey && Set.member oid operationIdSet && Set.member co reachable)
                                 overlayLandings

          executeMany cacheConn
            (asQuery "INSERT INTO line_landing(config_key,op_id,line_id,ref_name,commit_oid,complete) VALUES(?,?,?,?,?,?)")
            [ [ SQLText ck
              , SQLText oid
              , SQLText lid
              , SQLText rn
              , SQLText co
              , SQLInteger complete
              ]
            | (ck, oid, lid, rn, co, complete) <- filteredLandings
            ]

        lineLandings <- query_ cacheConn "SELECT count(*) FROM line_landing" :: IO [Only Int64]
        let syncLineLandings = case lineLandings of
              [Only c] -> fromIntegral c
              _        -> 0
        coverageCount <- query_ cacheConn "SELECT count(*) FROM operation_target_coverage" :: IO [Only Int64]
        reachableCount <- query_ cacheConn "SELECT count(*) FROM target_reachable_commit" :: IO [Only Int64]
        lineConfigCount <- query_ cacheConn "SELECT count(*) FROM line_config" :: IO [Only Int64]

        -- -----------------------------------------------------------------------
        -- Sync: ref_observation (direct from Haskell, not from overlay)
        -- Uses INSERT OR REPLACE keyed on ref_name (primary key).
        -- -----------------------------------------------------------------------
        execute_ cacheConn (asQuery "DELETE FROM ref_observation")

        executeMany cacheConn
          (asQuery "INSERT OR REPLACE INTO ref_observation(ref_name,tip_oid,object_type) VALUES(?,?,?)")
          [[SQLText refName, SQLText tipOid, SQLText objectType] | (refName, tipOid, objectType) <- refObservations]

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
          let filteredIssues = filter (\(_,_,_,_,_,_,oid) -> maybe False (`Set.member` operationIdSet) oid || isNothing oid)
                                     overlayIssues

          executeMany cacheConn
            (asQuery "INSERT INTO issue(severity,code,adr_id,object_id,path,message,origin,operation_id) VALUES(?,?,?,?,?,?,?,?)")
            [ [ SQLText sev
              , SQLText code
              , maybe SQLNull SQLText adr
              , maybe SQLNull SQLText obj
              , maybe SQLNull SQLText path
              , SQLText msg
              , SQLText "provenance"
              , maybe SQLNull SQLText oid
              ]
            | (sev, code, adr, obj, path, msg, oid) <- filteredIssues
            ]

        issueCount <- query_ cacheConn "SELECT count(*) FROM issue WHERE origin='provenance'"
          :: IO [Only Int64]
        let syncIssues = case issueCount of
              [Only c] -> fromIntegral c
              _        -> 0
        totalIssueCount <- query_ cacheConn "SELECT count(*) FROM issue" :: IO [Only Int64]

        -- -----------------------------------------------------------------------
        -- Update cache meta table with provenance metadata.
        -- Uses INSERT OR REPLACE (upsert) for each key.
        -- -----------------------------------------------------------------------
        now <- getCurrentTime
        let epochSec :: Integer
            epochSec = round (timeToEpochSec now)
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
                ("provenance.sync_timestamp", Text.pack (show epochSec)),
                ("provenance.operation_commit_count", Text.pack (show syncOpCommits)),
                ("provenance.operation_target_coverage_count", countText coverageCount),
                ("provenance.line_landing_count", Text.pack (show syncLineLandings)),
                ("provenance.target_reachable_commit_count", countText reachableCount),
                ("provenance.line_config_count", countText lineConfigCount)
              ]

        executeMany cacheConn
          (asQuery "INSERT OR REPLACE INTO meta(key,value) VALUES(?,?)")
          [[SQLText key, SQLText value] | (key, value) <- metaValues]

        -- Provenance diagnostics become ordinary cache issues, so retain the
        -- compiler's count commitment after replacing that projection.
        case totalIssueCount of
          [Only count] ->
            executeMany cacheConn
              (asQuery "INSERT OR REPLACE INTO meta(key,value) VALUES(?,?)")
              [[SQLText "issue_count", SQLText (Text.pack (show count))]]
          _ -> pure ()

        let syncMetaKeys = length metaValues

        pure SyncResult
          { syncOperationCommits = syncOpCommits
          , syncLineLandings     = syncLineLandings
          , syncRefObservations  = syncRefObs
          , syncProvenanceIssues = syncIssues
          , syncMetaKeys         = syncMetaKeys
          }
      pure result

countText :: [Only Int64] -> Text
countText rows = case rows of
  [Only count] -> Text.pack (show count)
  _ -> "invalid"

validOid :: Text -> Bool
validOid value = Text.length value `elem` [40, 64] && Text.all lowerHex value
  where
    lowerHex character = ('0' <= character && character <= '9') || ('a' <= character && character <= 'f')
