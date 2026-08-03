{-# LANGUAGE OverloadedStrings #-}

module Adrai.SearchVectorReuseTest (tests) where

import Adrai.Fixture.QueryMaterialization
  ( QueryMaterialization (..),
    lookupQueryMaterializationSource,
    materializeQueryFixture,
  )
import Adrai.Fixture.Relevance (relevanceCorpusV1)
import Adrai.Fixture.Types (RelevanceCorpus (..))
import Adrai.History (ReadSnapshot (..), RevisionIdentity (..))
import Adrai.Query
import Adrai.Retrieval (SearchDocument (..), SearchMaterialization (..))
import Adrai.SearchVectorCorpus
import Adrai.Sqlite (initializeSearchSchema, replaceSearchMaterialization)
import Adrai.Types (RevisionSelector (AtRevision))
import Control.Exception (bracket)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Database.SQLite.Simple (Connection, close, open)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "P3-06 reusable search vector corpus"
    [ testCase "current search cold-built and repeated reused paths are exactly equal" searchReuseContract,
      testCase "relevance cold-built and repeated reused paths are exactly equal" relevantReuseContract,
      testCase "changed materialization is rejected before retrieval" preRetrievalValidationContract,
      testCase "different relevance sources leave the corpus unchanged" sourceEphemeralityContract
    ]

searchReuseContract :: IO ()
searchReuseContract = withFixture $ \connection fixture -> do
  let snapshot = queryMaterializationSnapshot fixture
      search = queryMaterializationSearch fixture
      corpus = mustRight (buildSearchVectorCorpus search)
      request = (defaultSearchRequest "cache key invalidation compiler abi") {searchRequestLimit = 6}
  cold <- mustSearch =<< runCurrentSearch connection snapshot search request
  reusedFirst <- mustSearch =<< runCurrentSearchWithCorpus connection snapshot search corpus request
  reusedSecond <- mustSearch =<< runCurrentSearchWithCorpus connection snapshot search corpus request
  assertBool "search equality gate must exercise a result" (not (null (searchProjectionResults cold)))
  cold @?= reusedFirst
  reusedFirst @?= reusedSecond
  renderSearchProjection cold @?= renderSearchProjection reusedFirst
  renderSearchProjection reusedFirst @?= renderSearchProjection reusedSecond

relevantReuseContract :: IO ()
relevantReuseContract = withFixture $ \connection fixture -> do
  let snapshot = queryMaterializationSnapshot fixture
      search = queryMaterializationSearch fixture
      corpus = mustRight (buildSearchVectorCorpus search)
      source = fixtureSource "cache-python" fixture
      request = relevantRequest snapshot source
  cold <- mustRelevant =<< runRelevant connection snapshot search request source
  reusedFirst <- mustRelevant =<< runRelevantWithCorpus connection snapshot search corpus request source
  reusedSecond <- mustRelevant =<< runRelevantWithCorpus connection snapshot search corpus request source
  assertBool "relevance equality gate must exercise a result" (not (null (relevantProjectionResults cold)))
  cold @?= reusedFirst
  reusedFirst @?= reusedSecond
  renderRelevantProjection cold @?= renderRelevantProjection reusedFirst
  renderRelevantProjection reusedFirst @?= renderRelevantProjection reusedSecond

preRetrievalValidationContract :: IO ()
preRetrievalValidationContract = withFixture $ \connection fixture -> do
  let snapshot = queryMaterializationSnapshot fixture
      search = queryMaterializationSearch fixture
      corpus = mustRight (buildSearchVectorCorpus search)
      document = firstDocument search
      changed =
        search
          { searchMaterializationDocuments =
              document {searchDocumentSummary = searchDocumentSummary document <> " changed"}
                : drop 1 (searchMaterializationDocuments search)
          }
      request = defaultSearchRequest "cache"
  outcome <- runCurrentSearchWithCorpus connection snapshot changed corpus request
  case outcome of
    Left (SearchVectorCorpusFailure (SearchVectorCorpusFingerprintMismatch _ _)) -> pure ()
    Left problem -> assertFailure ("expected corpus fingerprint rejection, got " <> show problem)
    Right _ -> assertFailure "changed materialization unexpectedly reached retrieval"

sourceEphemeralityContract :: IO ()
sourceEphemeralityContract = withFixture $ \connection fixture -> do
  let snapshot = queryMaterializationSnapshot fixture
      search = queryMaterializationSearch fixture
      corpus = mustRight (buildSearchVectorCorpus search)
      cacheSource = fixtureSource "cache-python" fixture
      queueSource = fixtureSource "queue-compose" fixture
  cacheProjection <- mustRelevant =<< runRelevantWithCorpus connection snapshot search corpus (relevantRequest snapshot cacheSource) cacheSource
  queueProjection <- mustRelevant =<< runRelevantWithCorpus connection snapshot search corpus (relevantRequest snapshot queueSource) queueSource
  assertBool
    "different request sources should retain distinct source digests"
    (relevantFileDigest (relevantProjectionFile cacheProjection) /= relevantFileDigest (relevantProjectionFile queueProjection))
  corpus @?= mustRight (buildSearchVectorCorpus search)
  validateSearchVectorCorpus search corpus @?= Right ()

withFixture :: (Connection -> QueryMaterialization -> IO value) -> IO value
withFixture action = bracket (open ":memory:") close $ \connection -> do
  initializeSearchSchema connection >>= (@?= Right ())
  let fixture =
        mustRight
          ( materializeQueryFixture
              (relevanceCorpusMeta relevanceCorpusV1)
              (relevanceCorpusAdrs relevanceCorpusV1)
              (NonEmpty.toList (relevanceCorpusSources relevanceCorpusV1))
          )
  replaceSearchMaterialization connection (queryMaterializationSearch fixture) >>= (@?= Right ())
  action connection fixture

relevantRequest :: ReadSnapshot -> RelevantSource -> RelevantRequest
relevantRequest snapshot source =
  (defaultRelevantRequest (relevantSourcePath source))
    { relevantRequestRevision = AtRevision (revisionRequested (readSnapshotRevision snapshot)),
      relevantRequestLimit = 6
    }

fixtureSource :: Text -> QueryMaterialization -> RelevantSource
fixtureSource key fixture = mustRight (lookupQueryMaterializationSource key fixture)

firstDocument :: SearchMaterialization -> SearchDocument
firstDocument search =
  case searchMaterializationDocuments search of
    document : _ -> document
    [] -> error "fixture has no search documents"

mustSearch :: Either SearchError SearchProjection -> IO SearchProjection
mustSearch value =
  case value of
    Right projection -> pure projection
    Left problem -> assertFailure ("search failed: " <> show problem) >> error "unreachable"

mustRelevant :: Either RelevantError RelevantProjection -> IO RelevantProjection
mustRelevant value =
  case value of
    Right projection -> pure projection
    Left problem -> assertFailure ("relevance failed: " <> show problem) >> error "unreachable"

mustRight :: (Show problem) => Either problem value -> value
mustRight value =
  case value of
    Right result -> result
    Left problem -> error ("fixture failed: " <> show problem)
