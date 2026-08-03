{-# LANGUAGE OverloadedStrings #-}

module Adrai.SearchVectorCorpusTest (tests) where

import Adrai.Fixture.QueryMaterialization
  ( QueryMaterialization (..),
    materializeQueryFixture,
  )
import Adrai.Fixture.Relevance (relevanceCorpusV1)
import Adrai.Fixture.Types (RelevanceCorpus (..), SourceTemplate)
import Adrai.Query (mergeRelevantSectionCandidates, semanticSummaryText)
import Adrai.Retrieval
  ( SearchDocument (..),
    SearchMaterialization (..),
    SearchPassage (..),
  )
import Adrai.SearchVectorCorpus
import Adrai.Vector
  ( DenseVector,
    identifierEmbedding,
    packVector,
    semanticEmbedding,
    unpackVector,
  )
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "P3-06 search vector corpus"
    [ testCase "construction is deterministic and covers every materialized key" deterministicContract,
      testCase "all stored vectors have canonical float32 origin" float32OriginContract,
      testCase "changed materialization is rejected by compatibility fingerprint" incompatibilityContract,
      testCase "duplicate document and passage keys fail with typed errors" duplicateContract,
      testCase "source fixtures are excluded from the reusable vector corpus" sourceEphemeralityContract,
      testCase "FTS rescue is scored without inflating the public vector candidate count" exactRerankCountContract
    ]

deterministicContract :: IO ()
deterministicContract = do
  let search = fixtureSearch allFixtureSources
      first = mustRight (buildSearchVectorCorpus search)
      second = mustRight (buildSearchVectorCorpus search)
  first @?= second
  assertBool "compatibility fingerprint is nonempty" (not (Text.null (searchVectorCorpusFingerprint first)))
  Map.size (searchVectorCorpusSummaryVectors first) @?= length (searchMaterializationDocuments search)
  Map.size (searchVectorCorpusIdentifierVectors first) @?= length (searchMaterializationDocuments search)
  Map.size (searchVectorCorpusSectionVectors first) @?= length (searchMaterializationPassages search)
  validateSearchVectorCorpus search first @?= Right ()

float32OriginContract :: IO ()
float32OriginContract = do
  let search = fixtureSearch allFixtureSources
      corpus = mustRight (buildSearchVectorCorpus search)
      document = firstDocument search
      passage = firstPassage search
  Map.lookup (searchDocumentItemId document) (searchVectorCorpusSummaryVectors corpus)
    @?= Just (canonicalVector (semanticEmbedding (semanticSummaryText document)))
  Map.lookup (searchDocumentItemId document) (searchVectorCorpusIdentifierVectors corpus)
    @?= Just (canonicalVector (identifierEmbedding (searchVectorIdentifierSourceText document)))
  Map.lookup (searchPassageId passage) (searchVectorCorpusSectionVectors corpus)
    @?= Just (canonicalVector (semanticEmbedding (searchPassageText passage)))

incompatibilityContract :: IO ()
incompatibilityContract = do
  let search = fixtureSearch allFixtureSources
      corpus = mustRight (buildSearchVectorCorpus search)
      document = firstDocument search
      changed =
        search
          { searchMaterializationDocuments =
              document {searchDocumentIdentifierSource = searchDocumentIdentifierSource document <> "\nchanged raw source"}
                : drop 1 (searchMaterializationDocuments search)
          }
  case validateSearchVectorCorpus changed corpus of
    Left (SearchVectorCorpusFingerprintMismatch _ _) -> pure ()
    Left problem -> assertFailure ("expected fingerprint incompatibility, got " <> show problem)
    Right () -> assertFailure "changed materialization unexpectedly accepted the old corpus"

duplicateContract :: IO ()
duplicateContract = do
  let search = fixtureSearch allFixtureSources
      document = firstDocument search
      passage = firstPassage search
      duplicateDocument = search {searchMaterializationDocuments = document : searchMaterializationDocuments search}
      duplicatePassage = search {searchMaterializationPassages = passage : searchMaterializationPassages search}
  buildSearchVectorCorpus duplicateDocument
    @?= Left (SearchVectorCorpusDuplicateDocumentItemId (searchDocumentItemId document))
  buildSearchVectorCorpus duplicatePassage
    @?= Left (SearchVectorCorpusDuplicatePassageId (searchPassageId passage))

sourceEphemeralityContract :: IO ()
sourceEphemeralityContract = do
  let withSources = mustRight (buildSearchVectorCorpus (fixtureSearch allFixtureSources))
      withoutSources = mustRight (buildSearchVectorCorpus (fixtureSearch []))
  withSources @?= withoutSources
  searchVectorCorpusFingerprint withSources @?= searchVectorCorpusFingerprint withoutSources

exactRerankCountContract :: IO ()
exactRerankCountContract = do
  let allowed = Set.fromList ["chunk-0-vector", "chunk-0-fts", "chunk-1-fts"]
      base = [Set.singleton "chunk-0-vector", Set.empty]
      lexical =
        [ Set.fromList ["chunk-0-fts", "not-allowed"],
          Set.singleton "chunk-1-fts"
        ]
      expected =
        [ Set.fromList ["chunk-0-vector", "chunk-0-fts"],
          Set.singleton "chunk-1-fts"
        ]
  mergeRelevantSectionCandidates allowed base lexical @?= (expected, 1)

fixtureSearch :: [SourceTemplate] -> SearchMaterialization
fixtureSearch sources =
  queryMaterializationSearch
    ( mustRight
        ( materializeQueryFixture
            (relevanceCorpusMeta relevanceCorpusV1)
            (relevanceCorpusAdrs relevanceCorpusV1)
            sources
        )
    )

allFixtureSources :: [SourceTemplate]
allFixtureSources = NonEmpty.toList (relevanceCorpusSources relevanceCorpusV1)

canonicalVector :: DenseVector -> DenseVector
canonicalVector = mustRight . unpackVector . packVector

firstDocument :: SearchMaterialization -> SearchDocument
firstDocument search =
  case searchMaterializationDocuments search of
    document : _ -> document
    [] -> error "fixture has no search documents"

firstPassage :: SearchMaterialization -> SearchPassage
firstPassage search =
  case searchMaterializationPassages search of
    passage : _ -> passage
    [] -> error "fixture has no search passages"

mustRight :: (Show problem) => Either problem value -> value
mustRight value =
  case value of
    Right result -> result
    Left problem -> error ("fixture failed: " <> show problem)
