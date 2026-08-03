{-# LANGUAGE OverloadedStrings #-}

module Adrai.P306QualityGoldenTest (tests, writeP306Goldens) where

import Adrai.Fixture.Prng (seedWord64)
import Adrai.Fixture.QueryMaterialization
import Adrai.Fixture.Relevance
  ( authAdrKey,
    cacheAdrKey,
    deploymentAdrKey,
    observabilityAdrKey,
    primarySourceKeys,
    queueAdrKey,
    relevanceCorpusNamespace,
    relevanceCorpusSeed,
    relevanceCorpusV1,
    storageAdrKey,
  )
import Adrai.Fixture.RelevanceQuality
  ( configAdrKey,
    configComposeSourceKey,
    healthAdrKey,
    largeRegionSourceKey,
    leaseTokenSourceKey,
    secretsAdrKey,
  )
import Adrai.Fixture.RetrievalScale
  ( ScaleDiagnosticsContract (..),
    retrievalScaleDiagnosticsV1,
    retrievalScaleV1,
  )
import Adrai.Fixture.Types
  ( AdrKey (..),
    FixtureMeta (..),
    RelevanceCorpus (..),
    RetrievalScaleSpec (..),
  )
import Adrai.Format (renderDigest)
import Adrai.Format.Json
import Adrai.History (ReadSnapshot (..), RevisionIdentity (..))
import Adrai.Provenance (sha256Digest)
import Adrai.Query
import Adrai.Relevance (ConfidenceLabel (..))
import Adrai.Retrieval (SearchMaterialization (..))
import Adrai.SearchVectorCorpus
import Adrai.Sqlite (initializeSearchSchema, replaceSearchMaterialization)
import Adrai.Types (RevisionSelector (AtRevision), adrIdText)
import Adrai.Vector (DenseVector, packVector, unpackVector)
import Control.Exception (bracket)
import qualified Crypto.Hash as Crypto
import Crypto.Hash.Algorithms (SHA256)
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import Data.List (elemIndex, find)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as TextIO
import Database.SQLite.Simple (Connection, close, open)
import System.Directory (createDirectoryIfMissing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "P3-06 quality evidence golden"
    [ testCase "quality, result-only baseline, corpus reuse, and scale evidence are byte exact" $ do
        expected <- ByteString.readFile goldenPath
        actual <- goldenBytes
        actual @?= expected,
      testCase "golden README freezes byte count and SHA-256" $ do
        golden <- ByteString.readFile goldenPath
        expected <- ByteString.readFile readmePath
        readmeBytes golden @?= expected
    ]

writeP306Goldens :: IO ()
writeP306Goldens = do
  createDirectoryIfMissing True goldenRoot
  bytes <- goldenBytes
  ByteString.writeFile goldenPath bytes
  ByteString.writeFile readmePath (readmeBytes bytes)
  TextIO.putStrLn
    ( "quality.golden bytes="
        <> Text.pack (show (ByteString.length bytes))
        <> " sha256="
        <> hexSha256 bytes
    )

goldenRoot, goldenPath, readmePath, dropwirePath :: FilePath
goldenRoot = "test/golden/p3-06"
goldenPath = goldenRoot <> "/quality.golden"
readmePath = goldenRoot <> "/README.md"
dropwirePath = "test/fixtures/contracts/v1/search/dropwire-current.json"

goldenBytes :: IO ByteString
goldenBytes = do
  dropwireBytes <- ByteString.readFile dropwirePath
  withSmallCorpus $ \connection fixture corpus -> do
    qualityCases <- traverse (qualityCase connection fixture corpus) primaryCases
    reuse <- reuseEvidence connection fixture corpus
    let search = queryMaterializationSearch fixture
        storedVectors =
          Map.elems (searchVectorCorpusSummaryVectors corpus)
            <> Map.elems (searchVectorCorpusIdentifierVectors corpus)
            <> Map.elems (searchVectorCorpusSectionVectors corpus)
        roundTripped = traverse (unpackVector . packVector) storedVectors
    assertBool "stored corpus vectors remain canonical after float32 repacking" (roundTripped == Right storedVectors)
    pure . renderCanonicalJsonBytes $
      object
        [ ("schema", JsonString "adrai/p3-06-quality-golden/v1"),
          ("fixture", fixtureJson fixture),
          ("quality", qualityJson qualityCases),
          ("dropwire_result_contract", dropwireJson dropwireBytes),
          ("search_vector_corpus", corpusJson search corpus),
          ("cold_reused_equality", reuse),
          ("scale", scaleJson)
        ]

withSmallCorpus :: (Connection -> QueryMaterialization -> SearchVectorCorpus -> IO value) -> IO value
withSmallCorpus action = bracket (open ":memory:") close $ \connection -> do
  initializeSearchSchema connection >>= expectRight "schema initialization"
  let declarative = relevanceCorpusV1
  fixture <-
    mustEither
      "six-ADR fixture materialization"
      ( materializeQueryFixture
          (relevanceCorpusMeta declarative)
          (relevanceCorpusAdrs declarative)
          (NonEmpty.toList (relevanceCorpusSources declarative))
      )
  replaceSearchMaterialization connection (queryMaterializationSearch fixture) >>= expectRight "search materialization"
  corpus <- mustEither "search vector corpus" (buildSearchVectorCorpus (queryMaterializationSearch fixture))
  action connection fixture corpus

primaryCases :: [(Text, AdrKey)]
primaryCases =
  zip
    primarySourceKeys
    [cacheAdrKey, authAdrKey, queueAdrKey, storageAdrKey, observabilityAdrKey, deploymentAdrKey]

qualityCase :: Connection -> QueryMaterialization -> SearchVectorCorpus -> (Text, AdrKey) -> IO JsonValue
qualityCase connection fixture corpus (sourceKey, expectedKey) = do
  source <- mustEither ("source " <> sourceKey) (lookupQueryMaterializationSource sourceKey fixture)
  expectedAdr <- mustEither ("ADR " <> Text.pack (show expectedKey)) (lookupQueryMaterializationAdr expectedKey fixture)
  let snapshot = queryMaterializationSnapshot fixture
      request =
        (defaultRelevantRequest (relevantSourcePath source))
          { relevantRequestRevision = AtRevision (revisionRequested (readSnapshotRevision snapshot)),
            relevantRequestLimit = 6
          }
  projection <-
    mustEither
      ("quality relevance " <> sourceKey)
      =<< runRelevantWithCorpus connection snapshot (queryMaterializationSearch fixture) corpus request source
  let results = relevantProjectionResults projection
      orderedAdrs = map relevantResultAdr results
  rank <-
    case elemIndex expectedAdr orderedAdrs of
      Just index -> pure (index + 1)
      Nothing -> assertFailure (Text.unpack sourceKey <> " did not retrieve its intended ADR") >> error "unreachable"
  result <-
    case find ((== expectedAdr) . relevantResultAdr) results of
      Just value -> pure value
      Nothing -> assertFailure (Text.unpack sourceKey <> " has no intended result evidence") >> error "unreachable"
  let evidence = relevantResultEvidence result
      lineRangesValid = all (\item -> relevantEvidenceFileLineStart item >= 1 && relevantEvidenceFileLineStart item <= relevantEvidenceFileLineEnd item) evidence
      excerptsNonempty = all (not . Text.null . Text.strip . relevantEvidenceFileExcerpt) evidence && all (not . Text.null . Text.strip . relevantEvidenceAdrExcerpt) evidence
      resultIdsUnique = Set.size (Set.fromList orderedAdrs) == length orderedAdrs
  assertBool (Text.unpack sourceKey <> " expected rank must remain within top 3") (rank <= 3)
  assertBool (Text.unpack sourceKey <> " must retain one to three evidence items") (not (null evidence) && length evidence <= 3)
  assertBool (Text.unpack sourceKey <> " evidence line ranges must remain valid") lineRangesValid
  assertBool (Text.unpack sourceKey <> " evidence excerpts must remain nonempty") excerptsNonempty
  assertBool (Text.unpack sourceKey <> " result ADRs must remain unique") resultIdsUnique
  pure . object $
    [ ("source_key", JsonString sourceKey),
      ("expected_adr_key", JsonNumber (fromIntegral (adrKeyNumber expectedKey))),
      ("expected_adr", JsonString (adrIdText expectedAdr)),
      ("rank", JsonNumber (fromIntegral rank)),
      ("within_top3", JsonBool True),
      ("confidence", JsonString (confidenceText (relevantResultConfidence result))),
      ("evidence_count", JsonNumber (fromIntegral (length evidence))),
      ("evidence_line_ranges_valid", JsonBool lineRangesValid),
      ("evidence_excerpts_nonempty", JsonBool excerptsNonempty),
      ("matched_terms_present", JsonBool (any (not . null . relevantEvidenceMatchedTerms) evidence)),
      ("projection_sha256", JsonString (digest (renderRelevantProjection projection)))
    ]

reuseEvidence :: Connection -> QueryMaterialization -> SearchVectorCorpus -> IO JsonValue
reuseEvidence connection fixture corpus = do
  let snapshot = queryMaterializationSnapshot fixture
      search = queryMaterializationSearch fixture
      searchRequest = (defaultSearchRequest "cache key invalidation compiler abi") {searchRequestLimit = 6}
  searchCold <- mustEither "cold search" =<< runCurrentSearch connection snapshot search searchRequest
  searchReused <- mustEither "reused search" =<< runCurrentSearchWithCorpus connection snapshot search corpus searchRequest
  searchRepeated <- mustEither "repeated search" =<< runCurrentSearchWithCorpus connection snapshot search corpus searchRequest
  assertBool "small search cold/reused/repeated projections must be exact" (searchCold == searchReused && searchReused == searchRepeated)
  let searchColdBytes = renderSearchProjection searchCold
      searchReusedBytes = renderSearchProjection searchReused
      searchRepeatedBytes = renderSearchProjection searchRepeated
  assertBool "small search cold/reused/repeated bytes must be exact" (searchColdBytes == searchReusedBytes && searchReusedBytes == searchRepeatedBytes)

  source <- mustEither "cache-python source" (lookupQueryMaterializationSource "cache-python" fixture)
  let relevantRequest =
        (defaultRelevantRequest (relevantSourcePath source))
          { relevantRequestRevision = AtRevision (revisionRequested (readSnapshotRevision snapshot)),
            relevantRequestLimit = 6
          }
  relevantCold <- mustEither "cold relevance" =<< runRelevant connection snapshot search relevantRequest source
  relevantReused <- mustEither "reused relevance" =<< runRelevantWithCorpus connection snapshot search corpus relevantRequest source
  relevantRepeated <- mustEither "repeated relevance" =<< runRelevantWithCorpus connection snapshot search corpus relevantRequest source
  assertBool "small relevance cold/reused/repeated projections must be exact" (relevantCold == relevantReused && relevantReused == relevantRepeated)
  let relevantColdBytes = renderRelevantProjection relevantCold
      relevantReusedBytes = renderRelevantProjection relevantReused
      relevantRepeatedBytes = renderRelevantProjection relevantRepeated
  assertBool "small relevance cold/reused/repeated bytes must be exact" (relevantColdBytes == relevantReusedBytes && relevantReusedBytes == relevantRepeatedBytes)

  pure . object $
    [ ("search", equalityDigestJson searchColdBytes searchReusedBytes searchRepeatedBytes),
      ("relevance", equalityDigestJson relevantColdBytes relevantReusedBytes relevantRepeatedBytes)
    ]

fixtureJson :: QueryMaterialization -> JsonValue
fixtureJson fixture =
  object
    [ ("namespace", JsonString relevanceCorpusNamespace),
      ("seed", JsonNumber (fromIntegral (seedWord64 relevanceCorpusSeed))),
      ("logical_adrs", JsonNumber (fromIntegral (Map.size (queryMaterializationAdrIds fixture)))),
      ("source_cases", JsonNumber (fromIntegral (Map.size (queryMaterializationSources fixture)))),
      ("search_documents", JsonNumber (fromIntegral (length (searchMaterializationDocuments search)))),
      ("search_passages", JsonNumber (fromIntegral (length (searchMaterializationPassages search))))
    ]
  where
    search = queryMaterializationSearch fixture

qualityJson :: [JsonValue] -> JsonValue
qualityJson cases =
  object
    [ ("execution", JsonString "Haskell runRelevantWithCorpus over materialized six-ADR fixture"),
      ("integration_test", JsonString "test/integration/Adrai/RelevanceQualityTest.hs"),
      ("primary_cases", JsonArray cases),
      ( "large_region_gate",
        object
          [ ("source_key", JsonString largeRegionSourceKey),
            ("expected_adr_keys", JsonArray (map (JsonNumber . fromIntegral . adrKeyNumber) [cacheAdrKey, observabilityAdrKey])),
            ("coverage", JsonString "executable integration; multiple real ADRs and evidence after line 500")
          ]
      ),
      ( "hard_negative_gate",
        object
          [ ("source_keys", JsonArray (map JsonString ["generic-hard-negative", "empty-hard-negative", "tiny-hard-negative"])),
            ("high_confidence_ceiling", JsonNumber 0),
            ("coverage", JsonString "executable integration; boilerplate cannot inflate confidence")
          ]
      ),
      ( "identifier_gate",
        object
          [ ("config_source", JsonString configComposeSourceKey),
            ("config_expected_keys", JsonArray (map (JsonNumber . fromIntegral . adrKeyNumber) [configAdrKey, healthAdrKey, secretsAdrKey])),
            ("lease_source", JsonString leaseTokenSourceKey),
            ("lease_expected_key", JsonNumber (fromIntegral (adrKeyNumber queueAdrKey))),
            ("coverage", JsonString "executable integration; identifier and matched-term evidence required")
          ]
      )
    ]

dropwireJson :: ByteString -> JsonValue
dropwireJson bytes =
  object
    [ ("artifact", JsonString (Text.pack dropwirePath)),
      ("artifact_sha256", JsonString (digest bytes)),
      ("fixture_schema", JsonString "adrai/golden/search-baseline/v1"),
      ("status", JsonString "current"),
      ("qualification", JsonString "result-contract-only; Dropwire source corpus absent; no executable parity claim"),
      ("source_corpus_available", JsonBool False),
      ("source_artifact", JsonString "ADRAI_1_Source/verification/ADRAI_1_Search_Enhanced_Dropwire_Evaluation_24.json"),
      ("source_sha256", JsonString "97B70927B44CC3D4125BC91AC48DCB5096E631E5183261FF59E16960C94E2C73"),
      ("positive_cases", JsonNumber 19),
      ("negative_cases", JsonNumber 3),
      ("expected_associations", JsonNumber 47),
      ("top1_positive_cases", JsonNumber 19),
      ("top1_positive_rate", JsonDecimal 1.0),
      ("any_expected_top3_cases", JsonNumber 19),
      ("any_expected_top3_rate", JsonDecimal 1.0),
      ("association_hits", object [("1", JsonNumber 19), ("3", JsonNumber 38), ("5", JsonNumber 43), ("7", JsonNumber 47), ("10", JsonNumber 47)]),
      ("association_recall", object [("1", JsonDecimal 0.404255), ("3", JsonDecimal 0.808511), ("5", JsonDecimal 0.914894), ("7", JsonDecimal 1.0), ("10", JsonDecimal 1.0)]),
      ("medium_confidence_false_positives", JsonNumber 0),
      ("high_confidence_false_positives", JsonNumber 0),
      ("timing_policy", JsonString "observedSnapshot"),
      ("timings_are_equality_gate", JsonBool False)
    ]

corpusJson :: SearchMaterialization -> SearchVectorCorpus -> JsonValue
corpusJson search corpus =
  object
    [ ("compatibility_fingerprint", JsonString (searchVectorCorpusFingerprint corpus)),
      ("semantic_vector_id", JsonString (searchVectorCorpusSemanticVectorId corpus)),
      ("identifier_vector_id", JsonString (searchVectorCorpusIdentifierVectorId corpus)),
      ("summary_vectors", JsonNumber (fromIntegral (Map.size summaries))),
      ("identifier_vectors", JsonNumber (fromIntegral (Map.size identifiers))),
      ("section_vectors", JsonNumber (fromIntegral (Map.size sections))),
      ("materialization_documents", JsonNumber (fromIntegral (length (searchMaterializationDocuments search)))),
      ("materialization_passages", JsonNumber (fromIntegral (length (searchMaterializationPassages search)))),
      ("storage_contract", JsonString "release embedding -> packVector float32 little-endian -> unpackVector -> stored DenseVector"),
      ("stored_vectors_float32_origin", JsonBool True),
      ("source_and_query_vectors_ephemeral", JsonBool True),
      ("packed_corpus_sha256", JsonString (digest (corpusBytes corpus)))
    ]
  where
    summaries = searchVectorCorpusSummaryVectors corpus
    identifiers = searchVectorCorpusIdentifierVectors corpus
    sections = searchVectorCorpusSectionVectors corpus

scaleJson :: JsonValue
scaleJson =
  object
    [ ("execution", JsonString "validated by test/integration/Adrai/RetrievalScaleTest.hs; not rerun by this golden"),
      ("seed", JsonNumber (fromIntegral (seedWord64 (fixtureSeed (retrievalScaleMeta retrievalScaleV1))))),
      ("logical_adrs", number scaleLogicalAdrs),
      ("search_documents", number scaleSearchDocuments),
      ("search_passages", number scaleSearchPassages),
      ("summary_corpus", number scaleSummaryCorpus),
      ("identifier_corpus", number scaleIdentifierCorpus),
      ("search_section_candidates", number scaleSearchSectionCandidates),
      ("field_rerank_candidates", number scaleFieldRerankCandidates),
      ("relevance_source_chunk_cap", number scaleRelevanceSourceChunkCap),
      ("relevance_adr_shortlist", number scaleRelevanceAdrShortlist),
      ("relevance_candidate_search_items", number scaleRelevanceCandidateSearchItems),
      ("relevance_search_sections", number scaleRelevanceSearchSections),
      ("relevance_exact_rerank_candidates", number scaleRelevanceExactRerankCandidates),
      ("relevance_exact_rerank_bound", JsonString "strictly less than selected_source_chunks * all search_sections"),
      ("unique_probe", JsonString "Architecture rule 1999 -> AdrKey 1999 within top 10"),
      ("timing_policy", JsonString "reporting-only; no wall-clock threshold or golden timing")
    ]
  where
    number field = JsonNumber (fromIntegral (field retrievalScaleDiagnosticsV1))

equalityDigestJson :: ByteString -> ByteString -> ByteString -> JsonValue
equalityDigestJson cold reused repeated =
  object
    [ ("typed_equal", JsonBool True),
      ("rendered_bytes_equal", JsonBool (cold == reused && reused == repeated)),
      ("cold_sha256", JsonString (digest cold)),
      ("reused_sha256", JsonString (digest reused)),
      ("repeated_sha256", JsonString (digest repeated)),
      ("bytes", JsonNumber (fromIntegral (ByteString.length cold)))
    ]

corpusBytes :: SearchVectorCorpus -> ByteString
corpusBytes corpus =
  ByteString.concat
    [ vectorMapBytes "summary" (searchVectorCorpusSummaryVectors corpus),
      vectorMapBytes "identifier" (searchVectorCorpusIdentifierVectors corpus),
      vectorMapBytes "section" (searchVectorCorpusSectionVectors corpus)
    ]

vectorMapBytes :: Text -> Map Text DenseVector -> ByteString
vectorMapBytes label vectors =
  ByteString.concat
    [ TextEncoding.encodeUtf8 label
        <> ByteString.singleton 0
        <> TextEncoding.encodeUtf8 key
        <> ByteString.singleton 0
        <> packVector vector
      | (key, vector) <- Map.toAscList vectors
    ]

readmeBytes :: ByteString -> ByteString
readmeBytes golden =
  TextEncoding.encodeUtf8 . Text.unlines $
    [ "# P3-06 retrieval, relevance, and corpus quality golden",
      "",
      "This Haskell-owned evidence freezes the executable six-ADR quality summary, the qualified result-only Dropwire aggregate, the canonical float32-origin in-memory SearchVectorCorpus, cold/reused exact-byte digests, and deterministic diagnostics from the separately executable 2,000-ADR scale integration test.",
      "",
      "Normal tests are read-only. Regeneration is explicit through `stack test --test-arguments=--write-p3-06-goldens` and never executes or imports the protected Python prototype.",
      "",
      "The Dropwire source corpus is absent, so its section is result-contract validation only and is not executable parity. Scale timings are reporting-only and are neither thresholds nor golden values. Persisted revision caches, incremental invalidation, branch isolation, and corrupt-cache recovery remain deferred.",
      "",
      "- `quality.golden` is " <> Text.pack (show (ByteString.length golden)) <> " bytes; SHA-256: `" <> hexSha256 golden <> "`"
    ]

confidenceText :: ConfidenceLabel -> Text
confidenceText LowConfidence = "low"
confidenceText MediumConfidence = "medium"
confidenceText HighConfidence = "high"

adrKeyNumber :: AdrKey -> Int
adrKeyNumber (AdrKey value) = value

digest :: ByteString -> Text
digest = renderDigest . sha256Digest

hexSha256 :: ByteString -> Text
hexSha256 bytes = Text.toUpper (Text.pack (show (Crypto.hash bytes :: Crypto.Digest SHA256)))

mustEither :: (Show problem) => Text -> Either problem value -> IO value
mustEither _ (Right value) = pure value
mustEither label (Left problem) = assertFailure (Text.unpack label <> " failed: " <> show problem) >> error "unreachable"

expectRight :: (Show problem) => String -> Either problem () -> IO ()
expectRight _ (Right ()) = pure ()
expectRight label (Left problem) = assertFailure (label <> " failed: " <> show problem)
