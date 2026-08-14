{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for 'Adrai.Compiler.CacheSync.syncProvenanceSnapshot'.
--
-- These tests create a real overlay SQLite database with test data,
-- call the sync function, and verify that rows appear correctly
-- in the cache database.
module Adrai.Compiler.CacheSyncTest (tests) where

import Adrai.Compiler.CacheSync (SyncResult (..), syncProvenanceSnapshot)
import Adrai.Sqlite (asQuery)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Database.SQLite.Simple
  ( Connection,
    Only (..),
    execute_,
    open,
    close,
    query_,
  )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

tests :: TestTree
tests =
  testGroup
    "CacheSync"
      [ testCase "overlay DB can be opened and queried directly" $
          withSystemTempDirectory "adrai_cachesync" $ \tmpDir -> do
            overlayPath <- setupOverlayDB tmpDir
            conn <- open overlayPath
            tables <- query_ conn "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"
              :: IO [Only Text]
            cols <- query_ conn "PRAGMA table_info(operation_commit)" :: IO [(Int,Text,Text,Int,Maybe Text,Int)]
            close conn
            tables @?= [Only "line_landing", Only "meta", Only "operation_commit", Only "provenance_issue"]
            let colNames = [col | (_, col, _, _, _, _) <- cols]
            "op_id" `elem` colNames @?= True
      , testCase "syncs operation_commit rows filtered by operation IDs" $
          withSystemTempDirectory "adrai_cachesync" $ \tmpDir -> do
          overlayPath <- setupOverlayDB tmpDir
          cachePath   <- setupCacheDB tmpDir
          result <- runSync tmpDir overlayPath cachePath
          conn <- open cachePath
          commitRows <- query_ conn "SELECT op_id FROM operation_commit ORDER BY op_id"
            :: IO [Only Text]
          close conn
          commitRows @?= [Only "op1", Only "op2"]
    , testCase "syncs line_landing rows filtered by config_key and op_ids" $
        withSystemTempDirectory "adrai_cachesync" $ \tmpDir -> do
          overlayPath <- setupOverlayDB tmpDir
          cachePath   <- setupCacheDB tmpDir
          _ <- runSync tmpDir overlayPath cachePath
          conn <- open cachePath
          landingRows <- query_ conn "SELECT op_id,config_key FROM line_landing ORDER BY op_id"
            :: IO [(Text, Text)]
          close conn
          landingRows @?= [("op1", "main"), ("op2", "main")]
    , testCase "syncs ref_observations from direct Haskell data" $
        withSystemTempDirectory "adrai_cachesync" $ \tmpDir -> do
          overlayPath <- setupOverlayDB tmpDir
          cachePath   <- setupCacheDB tmpDir
          let refs =
                [ ("refs/heads/main",    "abc123", "commit"),
                  ("refs/heads/develop", "def456", "commit"),
                  ("refs/tags/v1.0",     "ghi789", "commit")
                ]
          cacheConn <- open cachePath
          _ <- syncProvenanceSnapshot
            cacheConn
            cacheConn
            overlayPath
            "op1" "main" refs "fp123" 1 5 "head123" "main" "origin/main" 100 "incremental" "provenance_delta"
          refRows <- query_ cacheConn "SELECT ref_name,tip_oid FROM ref_observation ORDER BY ref_name"
            :: IO [(Text, Text)]
          close cacheConn
          refRows @?=
            [ ("refs/heads/develop", "def456"),
              ("refs/heads/main",    "abc123"),
              ("refs/tags/v1.0",     "ghi789")
            ]
    , testCase "syncs provenance_issue into issue with origin='provenance'" $
        withSystemTempDirectory "adrai_cachesync" $ \tmpDir -> do
          overlayPath <- setupOverlayDB tmpDir
          cachePath   <- setupCacheDB tmpDir
          cacheConn <- open cachePath
          _ <- runSync tmpDir overlayPath cachePath
          issueRows <- query_ cacheConn "SELECT severity,code,origin FROM issue WHERE origin='provenance' ORDER BY code"
            :: IO [(Text, Text, Text)]
          close cacheConn
          issueRows @?=
            [ ("error", "MISSING_OPERATION", "provenance"),
              ("warning", "ORPHANED_COMMIT", "provenance")
            ]
    , testCase "writes provenance metadata to cache meta table" $
        withSystemTempDirectory "adrai_cachesync" $ \tmpDir -> do
          overlayPath <- setupOverlayDB tmpDir
          cachePath   <- setupCacheDB tmpDir
          cacheConn <- open cachePath
          _ <- syncProvenanceSnapshot
            cacheConn
            cacheConn
            overlayPath
            "op1" "main" [] "my-fingerprint" 42 999
            "headabc" "refs/heads/feature" "origin/main" 500 "incremental" "provenance_delta"
          metaRows <- query_ cacheConn "SELECT key,value FROM meta WHERE key LIKE 'provenance.%'"
            :: IO [(Text, Text)]
          close cacheConn
          let metaMap = Map.fromList metaRows
          metaMap Map.!? "provenance.fingerprint"              @?= Just "my-fingerprint"
          metaMap Map.!? "provenance.generation"               @?= Just "42"
          metaMap Map.!? "provenance.observed_commit_count"    @?= Just "999"
          metaMap Map.!? "provenance.current_head"             @?= Just "headabc"
          metaMap Map.!? "provenance.current_ref"              @?= Just "refs/heads/feature"
          metaMap Map.!? "provenance.current_upstream"         @?= Just "origin/main"
          metaMap Map.!? "provenance.history_commits_scanned"  @?= Just "500"
          assertBool "sync_timestamp should be set"
            (isJust (metaMap Map.!? "provenance.sync_timestamp"))
    , testCase "SyncResult has correct counts" $
        withSystemTempDirectory "adrai_cachesync" $ \tmpDir -> do
          overlayPath <- setupOverlayDB tmpDir
          cachePath   <- setupCacheDB tmpDir
          cacheConn <- open cachePath
          result <- syncProvenanceSnapshot
            cacheConn
            cacheConn
            overlayPath
            "op1 op2" "main" [("refs/heads/main", "abc", "commit")]
            "fp1" 1 1 "h1" "main" "origin/main" 10 "full" "full"
          close cacheConn
          result @?= SyncResult
            { syncOperationCommits = 2
            , syncLineLandings     = 2
            , syncRefObservations  = 1
            , syncProvenanceIssues = 2
            , syncMetaKeys         = 8
            }
    , testCase "idempotent re-sync replaces existing rows" $
        withSystemTempDirectory "adrai_cachesync" $ \tmpDir -> do
          overlayPath <- setupOverlayDB tmpDir
          cachePath   <- setupCacheDB tmpDir
          cacheConn <- open cachePath
          _ <- syncProvenanceSnapshot
            cacheConn
            cacheConn
            overlayPath
            "op1" "main" [] "fp1" 1 1 "h1" "main" "origin/main" 10 "full" "full"
          _ <- syncProvenanceSnapshot
            cacheConn
            cacheConn
            overlayPath
            "op2" "main" [("refs/heads/main", "new123", "commit")]
            "fp2" 2 2 "h2" "main" "origin/main" 20 "incremental" "provenance_delta"
          opRows <- query_ cacheConn "SELECT op_id FROM operation_commit ORDER BY op_id"
            :: IO [Only Text]
          refRows <- query_ cacheConn "SELECT tip_oid FROM ref_observation"
            :: IO [Only Text]
          close cacheConn
          opRows @?= [Only "op2"]
          refRows @?= [Only "new123"]
    , testCase "empty operation IDs syncs no overlay rows" $
        withSystemTempDirectory "adrai_cachesync" $ \tmpDir -> do
          overlayPath <- setupOverlayDB tmpDir
          cachePath   <- setupCacheDB tmpDir
          -- DEBUG: verify overlay DB before sync
          overlayBefore <- open overlayPath
          overlayTables <- query_ overlayBefore "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"
            :: IO [Only Text]
          overlayCols <- query_ overlayBefore "PRAGMA table_info(operation_commit)" :: IO [(Int,Text,Text,Int,Maybe Text,Int)]
          close overlayBefore
          assertBool ("overlay tables=" <> show overlayTables <> " cols=" <> show overlayCols)
            ("operation_commit" `elem` [t | Only t <- overlayTables])
          _ <- runSyncEmpty tmpDir overlayPath cachePath
          conn <- open cachePath
          commitCount <- query_ conn "SELECT count(*) FROM operation_commit"
            :: IO [Only Int64]
          landingCount <- query_ conn "SELECT count(*) FROM line_landing"
            :: IO [Only Int64]
          issueCount <- query_ conn "SELECT count(*) FROM issue WHERE origin='provenance'"
            :: IO [Only Int64]
          close conn
          commitCount @?= [Only 0]
          landingCount @?= [Only 0]
          issueCount @?= [Only 0]
    ]

-- | Helper: run a full sync with default op_ids "op1 op2".
runSync :: FilePath -> FilePath -> FilePath -> IO SyncResult
runSync _tmpDir overlayPath cachePath = do
  cacheConn <- open cachePath
  result <- syncProvenanceSnapshot
    cacheConn
    cacheConn
    overlayPath
    "op1 op2" "main" []
    "fp123" 1 5 "head123" "main" "origin/main" 100 "incremental" "provenance_delta"
  close cacheConn
  pure result

-- | Helper: run a sync with empty op_ids.
runSyncEmpty :: FilePath -> FilePath -> FilePath -> IO SyncResult
runSyncEmpty _tmpDir overlayPath cachePath = do
  cacheConn <- open cachePath
  result <- syncProvenanceSnapshot
    cacheConn
    cacheConn
    overlayPath
    "" "main" []
    "fp1" 1 1 "h1" "main" "origin/main" 10 "full" "full"
  close cacheConn
  pure result

-- | Set up a cache database with the required schema tables.
setupCacheDB :: FilePath -> IO FilePath
setupCacheDB tmpDir = do
  let path = tmpDir </> "cache.sqlite"
  conn <- open path
  execute_ conn "CREATE TABLE meta(key TEXT PRIMARY KEY,value TEXT NOT NULL)"
  execute_ conn (asQuery
    "CREATE TABLE operation_commit(op_id TEXT NOT NULL,commit_oid TEXT NOT NULL,"
      <> "classification TEXT NOT NULL,authored_s INTEGER NOT NULL,"
      <> "committed_s INTEGER NOT NULL,subject TEXT NOT NULL,"
      <> "parents_json TEXT NOT NULL,PRIMARY KEY(op_id,commit_oid))")
  execute_ conn (asQuery
    "CREATE TABLE line_landing(config_key TEXT NOT NULL,op_id TEXT NOT NULL,"
      <> "line_id TEXT NOT NULL,ref_name TEXT NOT NULL,commit_oid TEXT NOT NULL,"
      <> "complete INTEGER NOT NULL,PRIMARY KEY(config_key,op_id,line_id,ref_name))")
  execute_ conn (asQuery
    "CREATE TABLE ref_observation(ref_name TEXT PRIMARY KEY,tip_oid TEXT NOT NULL,"
      <> "object_type TEXT NOT NULL)")
  execute_ conn (asQuery
    "CREATE TABLE issue(ordinal INTEGER PRIMARY KEY CHECK(ordinal>=0),code TEXT NOT NULL,"
      <> "severity TEXT NOT NULL,origin TEXT NOT NULL,adr_id TEXT,object_id TEXT,"
      <> "operation_id TEXT,commit_oid TEXT,path TEXT,message TEXT NOT NULL)")
  close conn
  pure path

-- | Set up an overlay database with test data.
setupOverlayDB :: FilePath -> IO FilePath
setupOverlayDB tmpDir = do
  let path = tmpDir </> "overlay.sqlite"
  conn <- open path
  execute_ conn "CREATE TABLE meta(key TEXT PRIMARY KEY,value TEXT NOT NULL)"
  execute_ conn "INSERT INTO meta VALUES('schema','adrai-provenance-cache/1')"
  execute_ conn (asQuery
    "CREATE TABLE operation_commit(op_id TEXT NOT NULL,commit_oid TEXT NOT NULL,"
      <> "classification TEXT NOT NULL,authored_s INTEGER NOT NULL,"
      <> "committed_s INTEGER NOT NULL,subject TEXT NOT NULL,"
      <> "parents_json TEXT NOT NULL,PRIMARY KEY(op_id,commit_oid))")
  execute_ conn (asQuery
    "CREATE TABLE line_landing(config_key TEXT NOT NULL,op_id TEXT NOT NULL,"
      <> "line_id TEXT NOT NULL,ref_name TEXT NOT NULL,commit_oid TEXT NOT NULL,"
      <> "complete INTEGER NOT NULL,PRIMARY KEY(config_key,op_id,line_id,ref_name))")
  execute_ conn (asQuery
    "CREATE TABLE provenance_issue(issue_key TEXT PRIMARY KEY,severity TEXT NOT NULL,"
      <> "code TEXT NOT NULL,adr_id TEXT,object_id TEXT,path TEXT,"
      <> "message TEXT NOT NULL,op_id TEXT)")
  execute_ conn (asQuery
    "INSERT INTO operation_commit VALUES('op1','commit-a1','original',1000,1001,'Initial',''),"
      <> "('op2','commit-b1','copy',2000,2001,'Copy commit','commit-a1')")
  execute_ conn (asQuery
    "INSERT INTO line_landing VALUES('main','op1','L1','main','commit-a1',1),"
      <> "('main','op2','L2','develop','commit-b1',0)")
  execute_ conn (asQuery
    "INSERT INTO provenance_issue VALUES('i1','error','MISSING_OPERATION',NULL,NULL,NULL,"
      <> "'Op3 missing',NULL),"
      <> "('i2','warning','ORPHANED_COMMIT',NULL,NULL,NULL,"
      <> "'Commit not in any op','op1')")
  close conn
  pure path
