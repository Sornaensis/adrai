{-# LANGUAGE OverloadedStrings #-}

module Adrai.SearchResultGoldenTest (tests, writeP304Goldens) where

import Adrai.Compiler (materializeCurrentSearch)
import Adrai.CompilerMaterializationTest (p303RationaleSnapshot)
import Adrai.CurrentSearchTest (p304ConflictSnapshot)
import Adrai.Format.Json
import Adrai.History (ReadSnapshot)
import Adrai.Query
import Adrai.Retrieval
import Adrai.Sqlite
import Control.Exception (bracket)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Database.SQLite.Simple (Connection, close, open)
import System.Directory (createDirectoryIfMissing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "P3-04 search result goldens"
    [ testCase "weighted search public JSON and fingerprint are byte exact" $ do
        expected <- BS.readFile goldenPath
        actual <- goldenBytes
        actual @?= expected
    ]

writeP304Goldens :: IO ()
writeP304Goldens = do
  createDirectoryIfMissing True "test/golden/p3-04"
  goldenBytes >>= BS.writeFile goldenPath

goldenPath :: FilePath
goldenPath = "test/golden/p3-04/current-search.golden"

goldenBytes :: IO ByteString
goldenBytes = do
  resolved <- resolvedProjections
  conflict <- conflictProjection
  pure . renderCanonicalJsonBytes $
    object
      [ ("schema", JsonString "adrai/p3-04-search-golden/v1"),
        ("ranking_fingerprint", JsonString rankingImplementationFingerprint),
        ("blank", searchProjectionJson (resolved !! 0)),
        ("fts", searchProjectionJson (resolved !! 1)),
        ("vector", searchProjectionJson (resolved !! 2)),
        ("hybrid", searchProjectionJson (resolved !! 3)),
        ("conflict", searchProjectionJson conflict)
      ]

resolvedProjections :: IO [SearchProjection]
resolvedProjections =
  withSnapshot p303RationaleSnapshot $ \connection materialization ->
    mapM
      (run connection p303RationaleSnapshot materialization)
      [ (defaultSearchRequest "") {searchRequestIncludeObsolete = True},
        (defaultSearchRequest "current decision api") {searchRequestMode = FtsRetrieval, searchRequestIncludeObsolete = True},
        (defaultSearchRequest "current decision api") {searchRequestMode = VectorRetrieval, searchRequestIncludeObsolete = True},
        (defaultSearchRequest "current decision api") {searchRequestMode = HybridRetrieval, searchRequestIncludeObsolete = True}
      ]

conflictProjection :: IO SearchProjection
conflictProjection =
  withSnapshot p304ConflictSnapshot $ \connection materialization ->
    run connection p304ConflictSnapshot materialization (defaultSearchRequest "left")

withSnapshot :: ReadSnapshot -> (Connection -> SearchMaterialization -> IO value) -> IO value
withSnapshot snapshot action = bracket (open ":memory:") close $ \connection -> do
  initializeSearchSchema connection >>= expectRight "schema initialization"
  materialization <- case materializeCurrentSearch snapshot of
    Left problem -> assertFailure ("materialization failed: " <> show problem) >> error "unreachable"
    Right value -> pure value
  replaceSearchMaterialization connection materialization >>= expectRight "materialization persistence"
  action connection materialization

run :: Connection -> ReadSnapshot -> SearchMaterialization -> SearchRequest -> IO SearchProjection
run connection snapshot materialization request = do
  outcome <- runCurrentSearch connection snapshot materialization request
  case outcome of
    Left problem -> assertFailure ("search failed: " <> show problem) >> error "unreachable"
    Right projection -> pure projection

expectRight :: (Show problem) => String -> Either problem () -> IO ()
expectRight _ (Right ()) = pure ()
expectRight label (Left problem) = assertFailure (label <> " failed: " <> show problem)
