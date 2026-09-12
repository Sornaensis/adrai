{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Load cached parsed documents from a warm cache database.
--
-- Provides access to minimal parsed document information stored in a
-- warm cache SQLite database.  This enables the semantic-reuse cache
-- path to avoid re-parsing unchanged documents by loading their
-- cached capsule data directly.
--
-- Mirrors the Python prototype's ``_load_document_cache()`` function
-- in ``ADRAI_1_Source/adrai_core/compiler.py``.
module Adrai.Compiler.DocumentCache
  ( CachedDocument (..),
    loadDocumentCache,
    isDocumentCached,
    loadCachedCapsule,
  )
where

import Data.String (fromString)
import Control.Exception (SomeException, try)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import Database.SQLite.Simple
  ( FromRow (fromRow),
    Only (..),
    SQLData (SQLText),
    field,
    open,
    query,
    query_,
    close,
  )
import System.Directory (doesFileExist)

-- | Minimal parsed document info loaded from a warm cache database.
--
-- Contains the essential fields needed for semantic-reuse decisions
-- without requiring a full re-parse of the managed document.
data CachedDocument
  = CachedDocument
  { cachedDocPath        :: Text,
    cachedDocBlobOid     :: Text,
    cachedDocSemanticHash :: Text,
    cachedDocCapsule     :: Text,  -- JSON-serialized capsule
    cachedDocManagedPath :: Text
  }
  deriving (Eq, Show)

instance FromRow CachedDocument where
  fromRow = CachedDocument <$> field <*> field <*> field <*> field <*> field

-- | Load cached documents from a warm cache DB.
--
-- Queries the ``document_cache`` table and returns a 'Map' keyed by
-- @(repoPath, blobOid)@.  Returns an empty map if the database does
-- not exist or is empty.
--
-- The ``document_cache`` table is expected to have the following schema:
--
-- > CREATE TABLE document_cache(
-- >   repo_path TEXT NOT NULL,
-- >   blob_oid TEXT NOT NULL,
-- >   semantic_hash TEXT NOT NULL,
-- >   capsule TEXT NOT NULL,
-- >   managed_path TEXT NOT NULL,
-- >   PRIMARY KEY(repo_path, blob_oid))
loadDocumentCache :: FilePath -> IO (Map (Text, Text) CachedDocument)
loadDocumentCache dbPath = do
  exists <- doesFileExist dbPath
  if not exists
    then pure Map.empty
    else do
      result <- try @SomeException $ do
        conn <- open dbPath
        let sqlQuery = fromString "SELECT repo_path, blob_oid, semantic_hash, capsule, managed_path FROM document_cache"
        rows <- query_ conn sqlQuery :: IO [CachedDocument]
        close conn
        pure (Map.fromList [((cachedDocPath doc, cachedDocBlobOid doc), doc) | doc <- rows])
      case result of
        Left _  -> pure Map.empty
        Right m -> pure m

-- | Check if a document is cached (by path and blob_oid).
--
-- Performs a direct lookup in the ``document_cache`` table without
-- loading the full cache.
isDocumentCached :: FilePath -> Text -> Text -> IO Bool
isDocumentCached dbPath repoPath blobOid = do
  exists <- doesFileExist dbPath
  if not exists
    then pure False
    else do
      result <- try @SomeException $ do
        conn <- open dbPath
        let sqlQuery = fromString "SELECT COUNT(*) FROM document_cache WHERE repo_path = ? AND blob_oid = ?"
        rows <- query conn sqlQuery [SQLText repoPath, SQLText blobOid] :: IO [Only Int64]
        close conn
        pure (rows == [Only 1])
      case result of
        Left _  -> pure False
        Right b -> pure b

-- | Load cached capsule for a document (for semantic reuse).
--
-- Returns the JSON-serialized capsule if the document is cached,
-- 'Nothing' otherwise.  This is a lightweight lookup that avoids
-- loading the full cache map.
loadCachedCapsule :: FilePath -> Text -> Text -> IO (Maybe Text)
loadCachedCapsule dbPath repoPath blobOid = do
  exists <- doesFileExist dbPath
  if not exists
    then pure Nothing
    else do
      result <- try @SomeException $ do
        conn <- open dbPath
        let sqlQuery = fromString "SELECT capsule FROM document_cache WHERE repo_path = ? AND blob_oid = ?"
        rows <- query conn sqlQuery [SQLText repoPath, SQLText blobOid] :: IO [Only (Maybe Text)]
        close conn
        pure (findJust rows)
      case result of
        Left _  -> pure Nothing
        Right m -> pure m
  where
    -- Extract the inner value from the first Just in the list.
    findJust :: [Only (Maybe a)] -> Maybe a
    findJust []             = Nothing
    findJust (Only (Just x) : _)   = Just x
    findJust (_ : rest)     = findJust rest
