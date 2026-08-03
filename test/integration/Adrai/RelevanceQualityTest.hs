{-# LANGUAGE OverloadedStrings #-}

module Adrai.RelevanceQualityTest (tests) where

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
    storageAdrKey,
  )
import Adrai.Fixture.RelevanceQuality
import Adrai.Fixture.Types (AdrKey, AdrTemplate)
import Adrai.Format.Json (JsonValue (..))
import Adrai.History (ReadSnapshot (..), RevisionIdentity (..))
import Adrai.Query
import Adrai.Relevance
  ( ConfidenceLabel (..),
    RelevanceScopeMatch (RelevanceScopeNone),
    relevanceMaxSourceQueryChunks,
    repetitiveChunkLowConfidence,
  )
import Adrai.Sqlite (initializeSearchSchema, replaceSearchMaterialization)
import Adrai.Types (AdrId, RevisionSelector (AtRevision), adrIdText, repoPathText)
import Control.Exception (bracket)
import Control.Monad (forM_)
import qualified Data.ByteString as ByteString
import Data.List (elemIndex, find)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple (Connection, close, open)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "P3-06 executable relevance quality"
    [ testCase "row 72 six cross-format positives literal path and renamed extension preserve relevance" crossFormatContract,
      testCase "row 76 large boilerplate source recovers cache and observability evidence after line 500" largeRegionContract,
      testCase "row 77 repeated boilerplate and empty inputs obey hard-negative confidence bounds" hardNegativeContract,
      testCase "row 78 translated config health secrets and lease context retain intended ADRs" configLeaseContract,
      testCase "terse x_request_id input retains identifier-backed lexical evidence" terseIdentifierContract
    ]

crossFormatContract :: IO ()
crossFormatContract = withQuality baseQualityAdrs $ \connection materialization -> do
  let primaryCases =
        zip
          primarySourceKeys
          [ cacheAdrKey,
            authAdrKey,
            queueAdrKey,
            storageAdrKey,
            observabilityAdrKey,
            deploymentAdrKey
          ]
  forM_ primaryCases $ \(sourceKey, expected) -> do
    projection <- runQualitySource connection materialization sourceKey [expected] 6
    assertExpectedWithin materialization sourceKey expected 3 projection
  literal <- runQualitySource connection materialization literalQueueSourceKey [queueAdrKey] 6
  assertExpectedWithin materialization literalQueueSourceKey queueAdrKey 3 literal
  original <- runQualitySource connection materialization "queue-compose" [queueAdrKey] 6
  renamed <- runQualitySource connection materialization renamedQueueSourceKey [queueAdrKey] 6
  let renamedFailure = qualityFailure materialization renamedQueueSourceKey [queueAdrKey] renamed
  assertQualityProjection renamedFailure original
  assertQualityProjection renamedFailure renamed
  case (relevantProjectionResults original, relevantProjectionResults renamed) of
    (originalTop : _, renamedTop : _) -> do
      assertBool renamedFailure (relevantResultAdr renamedTop == relevantResultAdr originalTop)
      assertBool renamedFailure (roundSix (relevantResultSemanticScore renamedTop) == roundSix (relevantResultSemanticScore originalTop))
      assertBool renamedFailure (relevantProjectionResults renamed == relevantProjectionResults original)
      assertBool renamedFailure (relevantProjectionRetrieval renamed == relevantProjectionRetrieval original)
      assertBool renamedFailure (relevantFileDigest (relevantProjectionFile renamed) == relevantFileDigest (relevantProjectionFile original))
      assertBool renamedFailure (relevantFileChunks (relevantProjectionFile renamed) == relevantFileChunks (relevantProjectionFile original))
      assertBool renamedFailure (relevantFileQueryChunks (relevantProjectionFile renamed) == relevantFileQueryChunks (relevantProjectionFile original))
    _ -> assertFailure renamedFailure

largeRegionContract :: IO ()
largeRegionContract = withQuality baseQualityAdrs $ \connection materialization -> do
  projection <- runQualitySource connection materialization largeRegionSourceKey [cacheAdrKey, observabilityAdrKey] 6
  let failure = qualityFailure materialization largeRegionSourceKey [cacheAdrKey, observabilityAdrKey] projection
  assertQualityProjection failure projection
  assertBool failure (relevantFileChunks (relevantProjectionFile projection) > relevanceMaxSourceQueryChunks)
  assertBool failure (relevantFileQueryChunks (relevantProjectionFile projection) == relevanceMaxSourceQueryChunks)
  cache <- expectedResult materialization largeRegionSourceKey cacheAdrKey projection
  telemetry <- expectedResult materialization largeRegionSourceKey observabilityAdrKey projection
  assertBool failure (length (relevantResultEvidence cache) <= 3)
  assertBool failure (length (relevantResultEvidence telemetry) <= 3)
  assertBool failure (any cacheEvidence (relevantResultEvidence cache))
  assertBool failure (any telemetryEvidence (relevantResultEvidence telemetry))
  where
    cacheEvidence evidence =
      relevantEvidenceFileLineStart evidence > 500
        && relevantEvidenceFileLineEnd evidence <= 1802
        && contains "cache identity" (relevantEvidenceFileExcerpt evidence)
        && contains "cache" (relevantEvidenceAdrExcerpt evidence)
    telemetryEvidence evidence =
      relevantEvidenceFileLineStart evidence > 500
        && relevantEvidenceFileLineEnd evidence <= 1802
        && contains "request_id" (relevantEvidenceFileExcerpt evidence)
        && contains "request" (relevantEvidenceAdrExcerpt evidence)

hardNegativeContract :: IO ()
hardNegativeContract = withQuality baseQualityAdrs $ \connection materialization -> do
  negative <- runQualitySource connection materialization "generic-hard-negative" [] 10
  let negativeResults = relevantProjectionResults negative
      failure = qualityFailure materialization "generic-hard-negative" [] negative
  assertQualityProjection failure negative
  assertBool failure (relevantFileChunks (relevantProjectionFile negative) > relevanceMaxSourceQueryChunks)
  assertBool failure (relevantFileQueryChunks (relevantProjectionFile negative) == relevanceMaxSourceQueryChunks)
  assertBool failure (all ((== LowConfidence) . relevantResultConfidence) negativeResults)
  assertBool failure (all ((<= 3) . length . relevantResultEvidence) negativeResults)
  assertBool failure (all ((< repetitiveChunkLowConfidence) . relevantResultSourceInformation) negativeResults)
  case negativeResults of
    top : _ -> do
      assertBool failure (relevantResultSemanticScore top < 0.34)
      assertBool failure (relevantResultSemanticScore top <= relevantResultStrongestPair top + 0.001)
    [] -> pure ()
  whitespace <- runQualitySource connection materialization "empty-hard-negative" [] 10
  tiny <- runQualitySource connection materialization "tiny-hard-negative" [] 10
  assertEmptyInput (qualityFailure materialization "empty-hard-negative" [] whitespace) whitespace
  assertEmptyInput (qualityFailure materialization "tiny-hard-negative" [] tiny) tiny
  scoped <- runQualitySource connection materialization "scope-only-unrelated" [] 10
  let scopedFailure = qualityFailure materialization "scope-only-unrelated" [] scoped
  assertQualityProjection scopedFailure scoped
  assertBool
    scopedFailure
    (all ((/= HighConfidence) . relevantResultConfidence) (relevantProjectionResults scoped))
  assertBool scopedFailure (lookupJsonPath ["retrieval", "scope_used_for_eligibility"] (relevantProjectionJson scoped) == Just (JsonBool False))
  cache <- lookupAdr materialization cacheAdrKey
  case find ((== cache) . relevantResultAdr) (relevantProjectionResults scoped) of
    Nothing -> pure ()
    Just result -> do
      assertBool scopedFailure (relevantResultScopeBonus result == 0.025)
      assertBool scopedFailure (relevantResultConfidence result /= HighConfidence)

configLeaseContract :: IO ()
configLeaseContract = withQuality configQualityAdrs $ \connection materialization -> do
  configProjection <- runQualitySource connection materialization configComposeSourceKey [configAdrKey, healthAdrKey, secretsAdrKey] 10
  let configResults = relevantProjectionResults configProjection
      actual = Set.fromList (map relevantResultAdr configResults)
      configFailure = qualityFailure materialization configComposeSourceKey [configAdrKey, healthAdrKey, secretsAdrKey] configProjection
  assertQualityProjection configFailure configProjection
  expected <- traverse (lookupAdr materialization) [configAdrKey, healthAdrKey, secretsAdrKey]
  assertBool
    configFailure
    (Set.fromList expected `Set.isSubsetOf` actual)
  forM_ [configAdrKey, healthAdrKey, secretsAdrKey] $ \key -> do
    result <- expectedResult materialization configComposeSourceKey key configProjection
    assertBool configFailure (not (null (relevantResultEvidence result)))
  assertExpectedWithin materialization configComposeSourceKey healthAdrKey 3 configProjection
  leaseProjection <- runQualitySource connection materialization leaseTokenSourceKey [queueAdrKey] 10
  assertExpectedWithin materialization leaseTokenSourceKey queueAdrKey 3 leaseProjection
  queue <- lookupAdr materialization queueAdrKey
  auth <- lookupAdr materialization authAdrKey
  let ranked = map relevantResultAdr (relevantProjectionResults leaseProjection)
  case (elemIndex queue ranked, elemIndex auth ranked) of
    (Just queueIndex, Just authIndex) ->
      assertBool
        (qualityFailure materialization leaseTokenSourceKey [queueAdrKey] leaseProjection)
        (queueIndex < authIndex)
    (Just _, Nothing) -> pure ()
    _ -> assertFailure (qualityFailure materialization leaseTokenSourceKey [queueAdrKey] leaseProjection)
  queueResult <- expectedResult materialization leaseTokenSourceKey queueAdrKey leaseProjection
  let leaseFailure = qualityFailure materialization leaseTokenSourceKey [queueAdrKey] leaseProjection
  assertQualityProjection leaseFailure leaseProjection
  assertBool leaseFailure (relevantResultLexicalScore queueResult > 0)
  assertBool leaseFailure (any (not . null . relevantEvidenceMatchedTerms) (relevantResultEvidence queueResult))

terseIdentifierContract :: IO ()
terseIdentifierContract = withQuality correlationQualityAdrs $ \connection materialization -> do
  projection <- runQualitySource connection materialization terseCorrelationSourceKey [correlationAdrKey] 5
  let failure = qualityFailure materialization terseCorrelationSourceKey [correlationAdrKey] projection
  assertQualityProjection failure projection
  target <- expectedResult materialization terseCorrelationSourceKey correlationAdrKey projection
  assertBool failure (relevantResultLexicalScore target > 0)
  assertBool failure (relevantResultScopeMatch target == RelevanceScopeNone)
  assertBool failure (any (not . null . relevantEvidenceMatchedTerms) (relevantResultEvidence target))
  assertBool failure (identifierChannelHits (relevantRetrievalPassageFts (relevantProjectionRetrieval projection)) > 0)
  assertBool failure (lookupJsonPath ["retrieval", "scope_used_for_eligibility"] (relevantProjectionJson projection) == Just (JsonBool False))

withQuality :: NonEmpty AdrTemplate -> (Connection -> QueryMaterialization -> IO value) -> IO value
withQuality templates action = bracket (open ":memory:") close $ \connection -> do
  initializeSearchSchema connection >>= (@?= Right ())
  let materialization = mustMaterialization (materializeQueryFixture relevanceQualityMeta templates relevanceQualitySources)
  replaceSearchMaterialization connection (queryMaterializationSearch materialization) >>= (@?= Right ())
  action connection materialization

runQualitySource :: Connection -> QueryMaterialization -> Text -> [AdrKey] -> Int -> IO RelevantProjection
runQualitySource connection materialization sourceKey expected limit = do
  source <- mustSource materialization sourceKey
  let snapshot = queryMaterializationSnapshot materialization
      request =
        (defaultRelevantRequest (relevantSourcePath source))
          { relevantRequestRevision = AtRevision (revisionRequested (readSnapshotRevision snapshot)),
            relevantRequestLimit = limit
          }
  result <- runRelevant connection snapshot (queryMaterializationSearch materialization) request source
  case result of
    Right projection -> pure projection
    Left problem -> assertFailure (qualityRunFailure materialization sourceKey expected problem) >> error "unreachable"

assertExpectedWithin :: QueryMaterialization -> Text -> AdrKey -> Int -> RelevantProjection -> IO ()
assertExpectedWithin materialization sourceKey expected topK projection = do
  assertQualityProjection (qualityFailure materialization sourceKey [expected] projection) projection
  expectedAdr <- lookupAdr materialization expected
  assertBool
    (qualityFailure materialization sourceKey [expected] projection)
    (expectedAdr `elem` map relevantResultAdr (take topK (relevantProjectionResults projection)))

expectedResult :: QueryMaterialization -> Text -> AdrKey -> RelevantProjection -> IO RelevantResult
expectedResult materialization sourceKey expected projection = do
  expectedAdr <- lookupAdr materialization expected
  case find ((== expectedAdr) . relevantResultAdr) (relevantProjectionResults projection) of
    Just result -> pure result
    Nothing -> assertFailure (qualityFailure materialization sourceKey [expected] projection) >> error "unreachable"

lookupAdr :: QueryMaterialization -> AdrKey -> IO AdrId
lookupAdr materialization key =
  case lookupQueryMaterializationAdr key materialization of
    Right adr -> pure adr
    Left problem -> assertFailure (show problem) >> error "unreachable"

mustSource :: QueryMaterialization -> Text -> IO RelevantSource
mustSource materialization key =
  case lookupQueryMaterializationSource key materialization of
    Right source -> pure source
    Left problem -> assertFailure (show problem) >> error "unreachable"

qualityFailure :: QueryMaterialization -> Text -> [AdrKey] -> RelevantProjection -> String
qualityFailure materialization sourceKey expected projection =
  Text.unpack relevanceCorpusNamespace
    <> "; seed="
    <> show (seedWord64 relevanceCorpusSeed)
    <> "; source="
    <> Text.unpack sourceKey
    <> "; expected_keys="
    <> show expected
    <> "; expected_ids="
    <> show
      [ either (const "<missing>") adrIdText (lookupQueryMaterializationAdr key materialization)
        | key <- expected
      ]
    <> "; actual="
    <> show
      [ (adrIdText (relevantResultAdr result), relevantResultConfidence result)
        | result <- relevantProjectionResults projection
      ]
    <> "; bytes="
    <> sourceBytesDescription materialization sourceKey

qualityRunFailure :: QueryMaterialization -> Text -> [AdrKey] -> RelevantError -> String
qualityRunFailure materialization sourceKey expected problem =
  Text.unpack relevanceCorpusNamespace
    <> "; seed="
    <> show (seedWord64 relevanceCorpusSeed)
    <> "; source="
    <> Text.unpack sourceKey
    <> "; expected_keys="
    <> show expected
    <> "; expected_ids="
    <> show
      [ either (const "<missing>") adrIdText (lookupQueryMaterializationAdr key materialization)
        | key <- expected
      ]
    <> "; actual=run_error("
    <> show problem
    <> "); bytes="
    <> sourceBytesDescription materialization sourceKey

sourceBytesDescription :: QueryMaterialization -> Text -> String
sourceBytesDescription materialization sourceKey =
  case lookupQueryMaterializationSource sourceKey materialization of
    Left problem -> "missing(" <> show problem <> ")"
    Right source ->
      "path="
        <> Text.unpack (repoPathText (relevantSourcePath source))
        <> ",mode="
        <> sourceMode source
        <> ",length="
        <> show (ByteString.length bytes)
        <> ",prefix="
        <> show (ByteString.unpack (ByteString.take 96 bytes))
      where
        bytes = relevantSourceBytes source

sourceMode :: RelevantSource -> String
sourceMode RevisionRelevantSource {} = "revision"
sourceMode WorktreeRelevantSource {} = "worktree"

identifierChannelHits :: Maybe JsonValue -> Integer
identifierChannelHits (Just (JsonObject fields)) =
  case lookup "channel_hits" fields of
    Just (JsonObject channels) ->
      case lookup "identifier" channels of
        Just (JsonNumber value) -> value
        _ -> 0
    _ -> 0
identifierChannelHits _ = 0

lookupJsonPath :: [Text] -> JsonValue -> Maybe JsonValue
lookupJsonPath [] value = Just value
lookupJsonPath (key : remaining) (JsonObject fields) = lookup key fields >>= lookupJsonPath remaining
lookupJsonPath _ _ = Nothing

assertEmptyInput :: String -> RelevantProjection -> IO ()
assertEmptyInput failure projection = do
  assertBool failure (null (relevantProjectionResults projection))
  assertBool failure (relevantFileChunks (relevantProjectionFile projection) == 0)
  assertBool failure (relevantFileQueryChunks (relevantProjectionFile projection) == 0)
  assertBool failure (relevantRetrievalSemanticVectorId (relevantProjectionRetrieval projection) == Nothing)
  assertBool failure (relevantRetrievalIdentifierVectorId (relevantProjectionRetrieval projection) == Nothing)

assertQualityProjection :: String -> RelevantProjection -> IO ()
assertQualityProjection failure projection = do
  let results = relevantProjectionResults projection
      identifiers = map relevantResultAdr results
  assertBool failure (Set.size (Set.fromList identifiers) == length identifiers)
  forM_ results $ \result -> do
    assertBool
      failure
      ( all
          finite
          [ relevantResultScore result,
            relevantResultSemanticScore result,
            relevantResultLexicalScore result,
            relevantResultMargin result,
            relevantResultStrongestPair result,
            relevantResultSourceInformation result
          ]
      )
    assertBool
      failure
      (all (finite . relevantEvidenceScore) (relevantResultEvidence result))
  where
    finite value = not (isNaN value || isInfinite value)

contains :: Text -> Text -> Bool
contains needle = Text.isInfixOf needle . Text.toLower

roundSix :: Double -> Double
roundSix value
  | rounded == 0 = 0
  | otherwise = rounded
  where
    rounded = fromInteger (round (value * 1000000)) / 1000000

mustMaterialization :: Either QueryMaterializationError QueryMaterialization -> QueryMaterialization
mustMaterialization result =
  case result of
    Right materialization -> materialization
    Left problem -> error (show problem)
