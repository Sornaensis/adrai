{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for 'Adrai.Compiler.CacheSync.syncProvenanceSnapshot'.
--
-- These tests create a real overlay SQLite database with test data,
-- call the sync function, and verify that rows appear correctly
-- in the cache database.
module Adrai.Compiler.CacheSyncTest (tests) where

import Adrai.Compiler.CacheSync (SyncResult (..), syncProvenanceSnapshot)
import Adrai.Sqlite (asQuery)
import Control.Exception (SomeException, try)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple
  ( Only (..),
    executeMany,
    execute_,
    execute,
    open,
    close,
    query_,
    withTransaction,
  )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

tests :: TestTree
tests =
  testGroup
    "CacheSync"
      [ testCase "syncs the rich filtered projection, refs, issues, metadata and counts" $
          withSystemTempDirectory "adrai_cachesync" $ \tmpDir -> do
            overlayPath <- setupOverlayDB tmpDir
            cachePath <- setupCacheDB tmpDir
            let refs =
                  [ ("refs/heads/main", "abc123", "commit"),
                    ("refs/heads/develop", "def456", "commit"),
                    ("refs/tags/v1.0", "ghi789", "commit")
                  ]
            cache <- open cachePath
            result <- syncProvenanceSnapshot
              cache
              cache
              overlayPath
              "op1 op2"
              "main"
              refs
              "rich-fingerprint"
              42
              999
              targetOid
              [targetOid, commitAOid, commitBOid]
              "headabc"
              "refs/heads/feature"
              "origin/main"
              500
              "incremental"
              "provenance_delta"
            operationRows <- query_ cache "SELECT op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json FROM operation_commit ORDER BY op_id"
              :: IO [(Text, Text, Text, Int64, Int64, Text, Text)]
            landingRows <- query_ cache "SELECT config_key,op_id,line_id,ref_name,commit_oid,complete FROM line_landing ORDER BY op_id"
              :: IO [(Text, Text, Text, Text, Text, Int64)]
            refRows <- query_ cache "SELECT ref_name,tip_oid,object_type FROM ref_observation ORDER BY ref_name"
              :: IO [(Text, Text, Text)]
            issueRows <- query_ cache "SELECT severity,code,origin FROM issue WHERE origin='provenance' ORDER BY code"
              :: IO [(Text, Text, Text)]
            metaRows <- query_ cache "SELECT key,value FROM meta WHERE key LIKE 'provenance.%'"
              :: IO [(Text, Text)]
            close cache
            operationRows @?=
              [ ("op1", commitAOid, "original", 1000, 1001, "Initial", ""),
                ("op2", commitBOid, "copy", 2000, 2001, "Copy commit", commitAOid)
              ]
            landingRows @?=
              [ ("main", "op1", "L1", "main", commitAOid, 1),
                ("main", "op2", "L2", "develop", commitBOid, 0)
              ]
            refRows @?=
              [ ("refs/heads/develop", "def456", "commit"),
                ("refs/heads/main", "abc123", "commit"),
                ("refs/tags/v1.0", "ghi789", "commit")
              ]
            issueRows @?=
              [ ("error", "MISSING_OPERATION", "provenance"),
                ("warning", "ORPHANED_COMMIT", "provenance")
              ]
            let metaMap = Map.fromList metaRows
            metaMap Map.!? "provenance.fingerprint" @?= Just "rich-fingerprint"
            metaMap Map.!? "provenance.generation" @?= Just "42"
            metaMap Map.!? "provenance.observed_commit_count" @?= Just "999"
            metaMap Map.!? "provenance.current_head" @?= Just "headabc"
            metaMap Map.!? "provenance.current_ref" @?= Just "refs/heads/feature"
            metaMap Map.!? "provenance.current_upstream" @?= Just "origin/main"
            metaMap Map.!? "provenance.history_commits_scanned" @?= Just "500"
            metaMap Map.!? "provenance.operation_commit_count" @?= Just "2"
            metaMap Map.!? "provenance.operation_target_coverage_count" @?= Just "2"
            metaMap Map.!? "provenance.line_landing_count" @?= Just "2"
            metaMap Map.!? "provenance.target_reachable_commit_count" @?= Just "3"
            metaMap Map.!? "provenance.line_config_count" @?= Just "1"
            assertBool "sync_timestamp should be set"
              (isJust (metaMap Map.!? "provenance.sync_timestamp"))
            result @?= SyncResult
              { syncOperationCommits = 2,
                syncLineLandings = 2,
                syncRefObservations = 3,
                syncProvenanceIssues = 2,
                syncMetaKeys = 13
              }
    , testCase "persists only the certified target graph and its active config" $
        withSystemTempDirectory "adrai_cachesync" $ \tmpDir -> do
          overlayPath <- setupOverlayDB tmpDir
          cachePath <- setupCacheDB tmpDir
          overlay <- open overlayPath
          execute overlay
            "INSERT INTO operation_commit VALUES(?,?,?,?,?,?,?)"
            ("op1" :: Text, unreachableOid, "landing" :: Text, 3000 :: Int, 3001 :: Int, "other branch" :: Text, "" :: Text)
          execute overlay
            "INSERT INTO line_landing VALUES(?,?,?,?,?,?)"
            ("main" :: Text, "op1" :: Text, "L-other" :: Text, "other" :: Text, unreachableOid, 1 :: Int)
          close overlay
          _ <- runSync tmpDir overlayPath cachePath
          cache <- open cachePath
          commits <- query_ cache "SELECT commit_oid FROM operation_commit ORDER BY commit_oid" :: IO [Only Text]
          landings <- query_ cache "SELECT commit_oid FROM line_landing ORDER BY commit_oid" :: IO [Only Text]
          reachability <- query_ cache "SELECT target_oid,commit_oid FROM target_reachable_commit ORDER BY commit_oid" :: IO [(Text, Text)]
          configs <- query_ cache "SELECT config_key,config_json FROM line_config" :: IO [(Text, Text)]
          close cache
          commits @?= [Only commitAOid, Only commitBOid]
          landings @?= [Only commitAOid, Only commitBOid]
          reachability @?= [(targetOid, targetOid), (targetOid, commitAOid), (targetOid, commitBOid)]
          configs @?= [("main", "{\"logical_lines\":[]}")]
    , testCase "rejects a missing active line config before publication" $
        withSystemTempDirectory "adrai_cachesync" $ \tmpDir -> do
          overlayPath <- setupOverlayDB tmpDir
          cachePath <- setupCacheDB tmpDir
          overlay <- open overlayPath
          execute_ overlay "DELETE FROM line_config WHERE config_key='main'"
          close overlay
          rejected <- try (runSync tmpDir overlayPath cachePath) :: IO (Either SomeException SyncResult)
          case rejected of
            Left _ -> pure ()
            Right _ -> assertBool "cache publication requires the active line config" False
    , testCase "idempotent re-sync replaces existing rows" $
        withSystemTempDirectory "adrai_cachesync" $ \tmpDir -> do
          overlayPath <- setupOverlayDB tmpDir
          cachePath   <- setupCacheDB tmpDir
          cacheConn <- open cachePath
          _ <- syncProvenanceSnapshot
            cacheConn
            cacheConn
            overlayPath
            "op1" "main" [] "fp1" 1 1 targetOid [targetOid, commitAOid] "h1" "main" "origin/main" 10 "full" "full"
          _ <- syncProvenanceSnapshot
            cacheConn
            cacheConn
            overlayPath
            "op2" "main" [("refs/heads/main", "new123", "commit")]
            "fp2" 2 2 targetOid [targetOid, commitBOid] "h2" "main" "origin/main" 20 "incremental" "provenance_delta"
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
    , testCase "partial v2 overlay rejects empty-operation sync without coverage" $
        withSystemTempDirectory "adrai_cachesync" $ \tmpDir -> do
          overlayPath <- setupOverlayDB tmpDir
          cachePath <- setupCacheDB tmpDir
          overlay <- open overlayPath
          execute_ overlay "DROP TABLE operation_target_coverage"
          close overlay
          rejected <- try (runSyncEmpty tmpDir overlayPath cachePath) :: IO (Either SomeException SyncResult)
          case rejected of
            Left _ -> pure ()
            Right _ -> assertBool "partial v2 overlay must not sync, even with no selected operations" False
     , testCase "requested operation without registration rejects sync" $
         withSystemTempDirectory "adrai_cachesync" $ \tmpDir -> do
          overlayPath <- setupOverlayDB tmpDir
          cachePath <- setupCacheDB tmpDir
          overlay <- open overlayPath
          execute_ overlay "DELETE FROM registered_operation WHERE op_id='op2'"
          close overlay
          rejected <- try (runSync tmpDir overlayPath cachePath) :: IO (Either SomeException SyncResult)
          case rejected of
           Left _ -> pure ()
           Right _ -> assertBool "unregistered requested operation must fail closed" False
     , testCase "repairs a missing final row in the complete 32-reachable-commit 16-operation projection idempotently" $
         withSystemTempDirectory "adrai_cachesync_compact" $ \tmpDir -> do
           overlayPath <- setupOverlayDB tmpDir
           cachePath <- setupCacheDB tmpDir
           populateCompactOverlay overlayPath
           let operationIds = compactOperationIds
               operationText = Text.unwords operationIds
               reachable = compactReachability
               expectedOperationRows =
                 [ ( operationId,
                     compactOid index,
                     "landing",
                     fromIntegral (1000 + index),
                     fromIntegral (2000 + index),
                     "compact subject " <> operationId,
                     ""
                   )
                 | (index, operationId) <- zip [1 .. 16 :: Int] operationIds
                 ]
               expectedCoverageRows =
                 [ (operationId, targetOid, "compact-signature-" <> operationId)
                 | operationId <- operationIds
                 ]
               expectedReachabilityRows =
                 [(targetOid, compactOid index) | index <- [1 .. 31 :: Int]]
                   <> [(targetOid, targetOid)]
               expectedLandingRows =
                 [ ( "main",
                     operationId,
                     "compact-line-" <> Text.justifyRight 2 '0' (Text.pack (show index)),
                     "refs/heads/main",
                     compactOid index,
                     1
                   )
                 | (index, operationId) <- zip [1 .. 16 :: Int] operationIds
                 ]
           cache <- open cachePath
           first <- syncProvenanceSnapshot
             cache cache overlayPath
             operationText "main" [] "compact-fingerprint" 99 32
             targetOid reachable targetOid "main" "origin/main" 32 "full" "full"
           firstOperationRows <- query_ cache "SELECT op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json FROM operation_commit ORDER BY op_id" :: IO [(Text, Text, Text, Int64, Int64, Text, Text)]
           firstCoverageRows <- query_ cache "SELECT op_id,target_oid,registration_signature FROM operation_target_coverage ORDER BY op_id" :: IO [(Text, Text, Text)]
           firstReachabilityRows <- query_ cache "SELECT target_oid,commit_oid FROM target_reachable_commit ORDER BY commit_oid" :: IO [(Text, Text)]
           firstLandingRows <- query_ cache "SELECT config_key,op_id,line_id,ref_name,commit_oid,complete FROM line_landing ORDER BY op_id" :: IO [(Text, Text, Text, Text, Text, Int64)]
           fingerprintBefore <- query_ cache "SELECT value FROM meta WHERE key='provenance.fingerprint'" :: IO [Only Text]
           first @?= SyncResult
             { syncOperationCommits = 16
             , syncLineLandings = 16
             , syncRefObservations = 0
             , syncProvenanceIssues = 0
             , syncMetaKeys = 13
             }
           firstOperationRows @?= expectedOperationRows
           firstCoverageRows @?= expectedCoverageRows
           firstReachabilityRows @?= expectedReachabilityRows
           firstLandingRows @?= expectedLandingRows
           fingerprintBefore @?= [Only "compact-fingerprint"]
           execute cache "DELETE FROM target_reachable_commit WHERE target_oid=? AND commit_oid=?" (targetOid, compactOid 31)
           deletedReachability <- query_ cache "SELECT target_oid,commit_oid FROM target_reachable_commit ORDER BY commit_oid" :: IO [(Text, Text)]
           deletedReachability @?= filter ((/= compactOid 31) . snd) expectedReachabilityRows
           second <- syncProvenanceSnapshot
             cache cache overlayPath
             operationText "main" [] "compact-fingerprint" 99 32
             targetOid reachable targetOid "main" "origin/main" 32 "full" "full"
           secondOperationRows <- query_ cache "SELECT op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json FROM operation_commit ORDER BY op_id" :: IO [(Text, Text, Text, Int64, Int64, Text, Text)]
           secondCoverageRows <- query_ cache "SELECT op_id,target_oid,registration_signature FROM operation_target_coverage ORDER BY op_id" :: IO [(Text, Text, Text)]
           secondReachabilityRows <- query_ cache "SELECT target_oid,commit_oid FROM target_reachable_commit ORDER BY commit_oid" :: IO [(Text, Text)]
           secondLandingRows <- query_ cache "SELECT config_key,op_id,line_id,ref_name,commit_oid,complete FROM line_landing ORDER BY op_id" :: IO [(Text, Text, Text, Text, Text, Int64)]
           fingerprintAfter <- query_ cache "SELECT value FROM meta WHERE key='provenance.fingerprint'" :: IO [Only Text]
           close cache
           second @?= first
           secondOperationRows @?= expectedOperationRows
           secondCoverageRows @?= expectedCoverageRows
           secondReachabilityRows @?= expectedReachabilityRows
           secondLandingRows @?= expectedLandingRows
           fingerprintAfter @?= fingerprintBefore
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
    "fp123" 1 5 targetOid [targetOid, commitAOid, commitBOid] targetOid "main" "origin/main" 100 "incremental" "provenance_delta"
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
    "fp1" 1 1 targetOid [targetOid] "h1" "main" "origin/main" 10 "full" "full"
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
  execute_ conn "CREATE TABLE line_config(config_key TEXT PRIMARY KEY,config_json TEXT NOT NULL)"
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
  execute_ conn "INSERT INTO meta VALUES('schema','adrai-provenance-cache/2')"
  execute_ conn "CREATE TABLE registered_operation(op_id TEXT PRIMARY KEY,adr_id TEXT,basis_oid TEXT NOT NULL,signature TEXT NOT NULL)"
  execute_ conn "CREATE TABLE operation_target_coverage(op_id TEXT NOT NULL,target_oid TEXT NOT NULL,registration_signature TEXT NOT NULL,PRIMARY KEY(op_id,target_oid,registration_signature))"
  execute_ conn (asQuery
    "CREATE TABLE operation_commit(op_id TEXT NOT NULL,commit_oid TEXT NOT NULL,"
      <> "classification TEXT NOT NULL,authored_s INTEGER NOT NULL,"
      <> "committed_s INTEGER NOT NULL,subject TEXT NOT NULL,"
      <> "parents_json TEXT NOT NULL,PRIMARY KEY(op_id,commit_oid))")
  execute_ conn (asQuery
    "CREATE TABLE line_landing(config_key TEXT NOT NULL,op_id TEXT NOT NULL,"
      <> "line_id TEXT NOT NULL,ref_name TEXT NOT NULL,commit_oid TEXT NOT NULL,"
      <> "complete INTEGER NOT NULL,PRIMARY KEY(config_key,op_id,line_id,ref_name))")
  execute_ conn "CREATE TABLE line_config(config_key TEXT PRIMARY KEY,config_json TEXT NOT NULL)"
  execute_ conn (asQuery
    "CREATE TABLE provenance_issue(issue_key TEXT PRIMARY KEY,severity TEXT NOT NULL,"
      <> "code TEXT NOT NULL,adr_id TEXT,object_id TEXT,path TEXT,"
      <> "message TEXT NOT NULL,op_id TEXT)")
  execute conn
    "INSERT INTO operation_commit VALUES(?,?,?,?,?,?,?)"
    ("op1" :: Text, commitAOid, "original" :: Text, 1000 :: Int, 1001 :: Int, "Initial" :: Text, "" :: Text)
  execute conn
    "INSERT INTO operation_commit VALUES(?,?,?,?,?,?,?)"
    ("op2" :: Text, commitBOid, "copy" :: Text, 2000 :: Int, 2001 :: Int, "Copy commit" :: Text, commitAOid)
  execute conn
    "INSERT INTO line_landing VALUES(?,?,?,?,?,?)"
    ("main" :: Text, "op1" :: Text, "L1" :: Text, "main" :: Text, commitAOid, 1 :: Int)
  execute conn
    "INSERT INTO line_landing VALUES(?,?,?,?,?,?)"
    ("main" :: Text, "op2" :: Text, "L2" :: Text, "develop" :: Text, commitBOid, 0 :: Int)
  execute_ conn "INSERT INTO line_config VALUES('main','{\"logical_lines\":[]}')"
  execute_ conn (asQuery
    "INSERT INTO provenance_issue VALUES('i1','error','MISSING_OPERATION',NULL,NULL,NULL,"
      <> "'Op3 missing',NULL),"
      <> "('i2','warning','ORPHANED_COMMIT',NULL,NULL,NULL,"
      <> "'Commit not in any op','op1')")
  execute_ conn "INSERT INTO registered_operation VALUES('op1',NULL,'basis-1','sig-1'),('op2',NULL,'basis-2','sig-2')"
  execute conn
    "INSERT INTO operation_target_coverage VALUES(?,?,?),(?,?,?)"
    ("op1" :: Text, targetOid, "sig-1" :: Text, "op2" :: Text, targetOid, "sig-2" :: Text)
  close conn
  pure path

targetOid :: Text
targetOid = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

commitAOid :: Text
commitAOid = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

commitBOid :: Text
commitBOid = "cccccccccccccccccccccccccccccccccccccccc"

unreachableOid :: Text
unreachableOid = "dddddddddddddddddddddddddddddddddddddddd"

compactOperationIds :: [Text]
compactOperationIds = ["compact-op-" <> Text.justifyRight 2 '0' (Text.pack (show index)) | index <- [1 .. 16 :: Int]]

compactReachability :: [Text]
compactReachability = targetOid : [compactOid index | index <- [1 .. 31 :: Int]]

compactOid :: Int -> Text
compactOid index = Text.justifyRight 40 '0' (Text.pack (show index))

populateCompactOverlay :: FilePath -> IO ()
populateCompactOverlay overlayPath = do
  overlay <- open overlayPath
  withTransaction overlay $ do
    execute_ overlay "DELETE FROM operation_commit"
    execute_ overlay "DELETE FROM operation_target_coverage"
    execute_ overlay "DELETE FROM registered_operation"
    execute_ overlay "DELETE FROM line_landing"
    execute_ overlay "DELETE FROM provenance_issue"
    executeMany overlay "INSERT INTO registered_operation(op_id,adr_id,basis_oid,signature) VALUES(?,?,?,?)"
      [ (operationId, Nothing :: Maybe Text, "compact-basis" :: Text, "compact-signature-" <> operationId)
      | operationId <- compactOperationIds
      ]
    executeMany overlay "INSERT INTO operation_target_coverage(op_id,target_oid,registration_signature) VALUES(?,?,?)"
      [ (operationId, targetOid, "compact-signature-" <> operationId)
      | operationId <- compactOperationIds
      ]
    executeMany overlay "INSERT INTO operation_commit(op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json) VALUES(?,?,?,?,?,?,?)"
      [ ( operationId
        , compactOid index
        , "landing" :: Text
        , 1000 + index
        , 2000 + index
        , "compact subject " <> operationId
        , "" :: Text
        )
      | (index, operationId) <- zip [1 .. 16 :: Int] compactOperationIds
      ]
    executeMany overlay "INSERT INTO line_landing(config_key,op_id,line_id,ref_name,commit_oid,complete) VALUES(?,?,?,?,?,?)"
      [ ( "main" :: Text
        , operationId
        , "compact-line-" <> Text.justifyRight 2 '0' (Text.pack (show index))
        , "refs/heads/main" :: Text
        , compactOid index
        , 1 :: Int
        )
      | (index, operationId) <- zip [1 .. 16 :: Int] compactOperationIds
      ]
  close overlay
