{-# LANGUAGE OverloadedStrings #-}

module Adrai.Compiler.DocumentCacheTest (tests) where

import Adrai.Compiler.DocumentCache
  ( CachedDocument (..),
    isDocumentCached,
    loadCachedCapsule,
    loadDocumentCache,
  )
import qualified Data.Map.Strict as Map
import Database.SQLite.Simple
  ( execute_,
    open,
    close,
  )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- | Create a test document cache database with sample data.
createTestCacheDB :: FilePath -> IO ()
createTestCacheDB dbPath = do
  conn <- open dbPath
  execute_ conn "CREATE TABLE document_cache(repo_path TEXT NOT NULL, blob_oid TEXT NOT NULL, semantic_hash TEXT NOT NULL, capsule TEXT NOT NULL, managed_path TEXT NOT NULL, PRIMARY KEY(repo_path, blob_oid))"
  execute_ conn "INSERT INTO document_cache VALUES ('repo', 'abc123', 'hash1', '{\"key\":\"val\"}', 'managed.md')"
  execute_ conn "INSERT INTO document_cache VALUES ('repo', 'def456', 'hash2', '{\"key\":\"val2\"}', 'other.md')"
  close conn

tests :: TestTree
tests =
  testGroup "DocumentCache"
    [ testGroup
        "loadDocumentCache"
        [ testCase "Returns empty map for non-existent file" $
            withSystemTempDirectory "adrai_doccache_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "nonexistent.db"
              cache <- loadDocumentCache dbPath
              cache @?= Map.empty
        , testCase "Returns cached documents from valid DB" $
            withSystemTempDirectory "adrai_doccache_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "cache.db"
              createTestCacheDB dbPath
              cache <- loadDocumentCache dbPath
              Map.size cache @?= 2
              let key1 = ("repo", "abc123")
              let key2 = ("repo", "def456")
              Map.lookup key1 cache @?=
                Just (CachedDocument
                  { cachedDocPath = "repo"
                  , cachedDocBlobOid = "abc123"
                  , cachedDocSemanticHash = "hash1"
                  , cachedDocCapsule = "{\"key\":\"val\"}"
                  , cachedDocManagedPath = "managed.md"
                  })
              Map.lookup key2 cache @?=
                Just (CachedDocument
                  { cachedDocPath = "repo"
                  , cachedDocBlobOid = "def456"
                  , cachedDocSemanticHash = "hash2"
                  , cachedDocCapsule = "{\"key\":\"val2\"}"
                  , cachedDocManagedPath = "other.md"
                  })
        , testCase "Returns empty map for invalid DB" $
            withSystemTempDirectory "adrai_doccache_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "invalid.db"
              writeFile dbPath "not a database"
              cache <- loadDocumentCache dbPath
              cache @?= Map.empty
    ]
    , testGroup
        "isDocumentCached"
        [ testCase "Returns True for cached document" $
            withSystemTempDirectory "adrai_doccache_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "cache.db"
              createTestCacheDB dbPath
              result <- isDocumentCached dbPath "repo" "abc123"
              result @?= True
        , testCase "Returns False for non-cached document" $
            withSystemTempDirectory "adrai_doccache_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "cache.db"
              createTestCacheDB dbPath
              result <- isDocumentCached dbPath "repo" "nonexistent"
              result @?= False
        , testCase "Returns False for non-existent DB" $
            withSystemTempDirectory "adrai_doccache_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "nonexistent.db"
              result <- isDocumentCached dbPath "repo" "abc123"
              result @?= False
    ]
    , testGroup
        "loadCachedCapsule"
        [ testCase "Returns Just capsule for cached document" $
            withSystemTempDirectory "adrai_doccache_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "cache.db"
              createTestCacheDB dbPath
              result <- loadCachedCapsule dbPath "repo" "abc123"
              result @?= Just "{\"key\":\"val\"}"
        , testCase "Returns Nothing for non-cached document" $
            withSystemTempDirectory "adrai_doccache_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "cache.db"
              createTestCacheDB dbPath
              result <- loadCachedCapsule dbPath "repo" "nonexistent"
              result @?= Nothing
        , testCase "Returns Nothing for non-existent DB" $
            withSystemTempDirectory "adrai_doccache_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "nonexistent.db"
              result <- loadCachedCapsule dbPath "repo" "abc123"
              result @?= Nothing
    ]
    , testGroup
        "CachedDocument"
        [ testCase "CachedDocument shows correctly" $
            let doc = CachedDocument
                  { cachedDocPath = "/test"
                  , cachedDocBlobOid = "oid123"
                  , cachedDocSemanticHash = "hash123"
                  , cachedDocCapsule = "{\"test\":true}"
                  , cachedDocManagedPath = "/test.md"
                  }
             in show doc @?=
                  "CachedDocument {cachedDocPath = \"/test\", cachedDocBlobOid = \"oid123\", cachedDocSemanticHash = \"hash123\", cachedDocCapsule = \"{\\\"test\\\":true}\", cachedDocManagedPath = \"/test.md\"}"
        , testCase "CachedDocument equality works" $ do
              let doc1 = CachedDocument
                    { cachedDocPath = "/test"
                    , cachedDocBlobOid = "oid"
                    , cachedDocSemanticHash = "h"
                    , cachedDocCapsule = "c"
                    , cachedDocManagedPath = "/m"
                    }
              let doc2 = doc1
              let doc3 = doc1{cachedDocPath = "/other"}
              doc1 @?= doc2
              doc1 /= doc3 @?= True
    ]
    ]
