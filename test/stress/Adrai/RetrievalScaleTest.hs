{-# LANGUAGE OverloadedStrings #-}

module Adrai.RetrievalScaleTest (tests) where

import Adrai.Fixture.Prng (seedWord64)
import Adrai.Fixture.QueryMaterialization
import Adrai.Fixture.RetrievalScale
import Adrai.Fixture.Types
  ( AdrKey (..),
    AdrTemplate (..),
    FixtureMeta (..),
    RetrievalProbe (..),
    RetrievalScaleSpec (..),
  )
import Adrai.Format.Json (JsonValue (..))
import Adrai.Graph (GraphReduction (..), ReducedAdr (..))
import Adrai.History (ReadSnapshot (..), RevisionIdentity (..), validateReadSnapshot)
import Adrai.Query
import Adrai.Retrieval (SearchDocument (..), SearchMaterialization (..))
import Adrai.SearchVectorCorpus
import Adrai.Sqlite (initializeSearchSchema, replaceSearchMaterialization)
import Adrai.Types
  ( AdrId,
    RevisionSelector (AtRevision),
    adrIdText,
    mkRepoPath,
  )
import Control.Exception (bracket, evaluate)
import Control.Monad (forM_)
import qualified Data.ByteString as ByteString
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as TextIO
import Database.SQLite.Simple (Connection, close, open)
import GHC.Clock (getMonotonicTimeNSec)
import System.IO (stderr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "P3-06 deterministic 2,000 ADR retrieval scale"
    [testCase "unique probes are correct and all query work remains bounded" scaleContract]

scaleContract :: IO ()
scaleContract = bracket (open ":memory:") close $ \connection -> do
  templates <-
    case NonEmpty.nonEmpty adrTemplates of
      Nothing -> assertFailure "seed=0; probe=materialization; 2,000-ADR templates are empty" >> error "unreachable"
      Just values -> pure values
  fixture <-
    timed "materialization" $ do
      value <-
        mustFixture
          "materialization"
          (materializeQueryFixture (retrievalScaleMeta retrievalScaleV1) templates [])
      _ <- evaluate (length (readSnapshotDocuments (queryMaterializationSnapshot value)))
      _ <- evaluate (length (searchMaterializationPassages (queryMaterializationSearch value)))
      pure value
  let snapshot = queryMaterializationSnapshot fixture
      search = queryMaterializationSearch fixture
      diagnosticsContract = retrievalScaleDiagnosticsV1
      logicalAdrs = scaleLogicalAdrs diagnosticsContract
      corpusContext = scaleContext "corpus" [] [] "exact diagnostics contract"
      reduced = graphReductionAdrs (readSnapshotReduction snapshot)
      eligibleAdrs = filter (adrVisible False) reduced
      eligibleAdrIds = Set.fromList (map reducedAdrId eligibleAdrs)
      eligibleItems = filter ((`Set.member` eligibleAdrIds) . searchDocumentAdrId) (searchMaterializationDocuments search)
  assertBool corpusContext (length reduced == logicalAdrs)
  assertBool corpusContext (Map.size (queryMaterializationAdrIds fixture) == logicalAdrs)
  assertBool corpusContext (length eligibleAdrs == logicalAdrs)
  assertBool corpusContext (length eligibleItems == logicalAdrs)
  assertBool corpusContext (length (searchMaterializationDocuments search) == scaleSearchDocuments diagnosticsContract)
  assertBool corpusContext (length (searchMaterializationPassages search) == scaleSearchPassages diagnosticsContract)
  assertBool corpusContext (null (graphReductionIssues (readSnapshotReduction snapshot)))
  assertBool corpusContext (validateReadSnapshot snapshot == Right ())
  initializeSearchSchema connection >>= (@?= Right ())
  _ <-
    timed "sqlite-materialization" $ do
      replaceSearchMaterialization connection search >>= (@?= Right ())
  corpus <-
    timed "vector-corpus-build" $ do
      value <- mustCorpus (buildSearchVectorCorpus search)
      _ <-
        evaluate
          ( Map.size (searchVectorCorpusSummaryVectors value)
              + Map.size (searchVectorCorpusIdentifierVectors value)
              + Map.size (searchVectorCorpusSectionVectors value)
          )
      pure value
  validateSearchVectorCorpus search corpus @?= Right ()
  Map.size (searchVectorCorpusSummaryVectors corpus) @?= scaleSummaryCorpus diagnosticsContract
  Map.size (searchVectorCorpusIdentifierVectors corpus) @?= scaleIdentifierCorpus diagnosticsContract
  Map.size (searchVectorCorpusSectionVectors corpus) @?= scaleRelevanceSearchSections diagnosticsContract

  let genericProbe = NonEmpty.head (retrievalQueryProbes retrievalScaleV1)
  genericProjection <- runScaleSearch "generic-bounds" Nothing connection snapshot search corpus genericProbe
  assertSearchBounds (searchBoundFailure genericProbe genericProjection) genericProjection

  let uniqueProbes = retrievalTailProbeV1 : NonEmpty.toList retrievalUniqueProbesV1
  forM_ (zip [0 :: Int ..] uniqueProbes) $ \(ordinal, probe) -> do
    let expected = expectedAdrIds fixture probe
    reused <- runScaleSearch "unique-reused" (Just expected) connection snapshot search corpus probe
    assertSearchBounds (searchFailure fixture probe reused) reused
    assertExpectedSearch fixture probe reused
    if ordinal == 0
      then do
        repeated <- runScaleSearch "unique-repeated" (Just expected) connection snapshot search corpus probe
        cold <- runColdScaleSearch (Just expected) connection snapshot search probe
        assertBool (searchFailure fixture probe reused) (cold == reused && reused == repeated)
        assertBool (searchFailure fixture probe reused) (renderSearchProjection cold == renderSearchProjection reused)
        assertBool (searchFailure fixture probe reused) (renderSearchProjection reused == renderSearchProjection repeated)
      else pure ()

  forM_ (zip [0 :: Int ..] uniqueProbes) $ \(ordinal, probe) -> do
    let (request, source) = relevanceInput snapshot probe
        expected = expectedAdrIds fixture probe
    reused <- runScaleRelevant "unique-relevant-reused" (Just expected) connection snapshot search corpus request source probe
    assertRelevantBounds fixture search probe reused
    assertExpectedRelevant fixture probe reused
    if ordinal == 0
      then do
        repeated <- runScaleRelevant "unique-relevant-repeated" (Just expected) connection snapshot search corpus request source probe
        cold <- runColdScaleRelevant (Just expected) connection snapshot search request source probe
        assertBool (relevantFailure fixture probe reused) (cold == reused && reused == repeated)
        assertBool (relevantFailure fixture probe reused) (renderRelevantProjection cold == renderRelevantProjection reused)
        assertBool (relevantFailure fixture probe reused) (renderRelevantProjection reused == renderRelevantProjection repeated)
      else pure ()

runScaleSearch :: Text -> Maybe [AdrId] -> Connection -> ReadSnapshot -> SearchMaterialization -> SearchVectorCorpus -> RetrievalProbe -> IO SearchProjection
runScaleSearch observation expected connection snapshot search corpus probe =
  timedProjection observation renderSearchProjection $ do
    outcome <-
      runCurrentSearchWithCorpus
        connection
        snapshot
        search
        corpus
        ((defaultSearchRequest (retrievalProbeQuery probe)) {searchRequestLimit = 10})
    mustSearch expected probe outcome

runColdScaleSearch :: Maybe [AdrId] -> Connection -> ReadSnapshot -> SearchMaterialization -> RetrievalProbe -> IO SearchProjection
runColdScaleSearch expected connection snapshot search probe =
  timedProjection "unique-cold-search" renderSearchProjection $ do
    outcome <- runCurrentSearch connection snapshot search ((defaultSearchRequest (retrievalProbeQuery probe)) {searchRequestLimit = 10})
    mustSearch expected probe outcome

runScaleRelevant :: Text -> Maybe [AdrId] -> Connection -> ReadSnapshot -> SearchMaterialization -> SearchVectorCorpus -> RelevantRequest -> RelevantSource -> RetrievalProbe -> IO RelevantProjection
runScaleRelevant observation expected connection snapshot search corpus request source probe =
  timedProjection observation renderRelevantProjection $ do
    outcome <- runRelevantWithCorpus connection snapshot search corpus request source
    mustRelevant expected probe outcome

runColdScaleRelevant :: Maybe [AdrId] -> Connection -> ReadSnapshot -> SearchMaterialization -> RelevantRequest -> RelevantSource -> RetrievalProbe -> IO RelevantProjection
runColdScaleRelevant expected connection snapshot search request source probe =
  timedProjection "unique-cold-relevant" renderRelevantProjection $ do
    outcome <- runRelevant connection snapshot search request source
    mustRelevant expected probe outcome

assertSearchBounds :: String -> SearchProjection -> IO ()
assertSearchBounds context projection = do
  diagnostics <- searchDiagnostics context projection
  TextIO.hPutStrLn stderr ("P3-06 scale evidence: " <> Text.pack context)
  assertBool context (jsonIntegerAt ["vector", "summary_corpus"] diagnostics == exact scaleSummaryCorpus)
  assertBool context (jsonIntegerAt ["vector", "identifier_corpus"] diagnostics == exact scaleIdentifierCorpus)
  assertBool context (jsonIntegerAt ["vector", "section_candidates"] diagnostics == exact scaleSearchSectionCandidates)
  assertBool context (jsonIntegerAt ["field_rerank_candidates"] diagnostics == exact scaleFieldRerankCandidates)
  assertBool context (length results == 10)
  assertBool context (Set.size (Set.fromList actual) == length actual)
  where
    exact field = Just (fromIntegral (field retrievalScaleDiagnosticsV1))
    results = searchProjectionResults projection
    actual = map searchResultAdr results

assertExpectedSearch :: QueryMaterialization -> RetrievalProbe -> SearchProjection -> IO ()
assertExpectedSearch fixture probe projection =
  assertBool
    (searchFailure fixture probe projection)
    (any (`elem` actual) expected)
  where
    expected = expectedAdrIds fixture probe
    actual = map searchResultAdr (take (retrievalProbeTopK probe) (searchProjectionResults projection))

assertRelevantBounds :: QueryMaterialization -> SearchMaterialization -> RetrievalProbe -> RelevantProjection -> IO ()
assertRelevantBounds fixture search probe projection = do
  let retrieval = relevantProjectionRetrieval projection
      context = relevantFailure fixture probe projection
      selected = relevantRetrievalSelectedSourceChunks retrieval
      sections = relevantRetrievalSearchSections retrieval
      exactRerank = relevantRetrievalExactRerankCandidates retrieval
      results = relevantProjectionResults projection
      actual = map relevantResultAdr results
  TextIO.hPutStrLn stderr ("P3-06 scale evidence: " <> Text.pack context)
  assertBool context (relevantRetrievalSourceChunks retrieval > 0 && relevantRetrievalSourceChunks retrieval <= scaleRelevanceSourceChunkCap contract)
  assertBool context (selected > 0 && selected <= scaleRelevanceSourceChunkCap contract)
  assertBool context (relevantRetrievalEligibleAdrs retrieval == scaleLogicalAdrs contract)
  assertBool context (relevantRetrievalEligibleSearchItems retrieval == scaleSearchDocuments contract)
  assertBool context (sections == scaleRelevanceSearchSections contract)
  assertBool context (sections == length (searchMaterializationPassages search))
  assertBool context (relevantRetrievalAdrShortlist retrieval == scaleRelevanceAdrShortlist contract)
  assertBool context (relevantRetrievalCandidateSearchItems retrieval == scaleRelevanceCandidateSearchItems contract)
  assertBool context (relevantRetrievalAdrShortlist retrieval == relevantRetrievalCandidateSearchItems retrieval)
  assertBool context (exactRerank == scaleRelevanceExactRerankCandidates contract)
  case relevantRetrievalSummary retrieval of
    Just summary -> do
      assertBool context (jsonIntegerAt ["semantic_corpus"] summary == exact scaleSummaryCorpus)
      assertBool context (jsonIntegerAt ["identifier_corpus"] summary == exact scaleIdentifierCorpus)
    Nothing -> assertFailure context
  assertBool context (exactRerank > 0 && exactRerank < selected * sections)
  assertBool context (length results == 10)
  assertBool context (Set.size (Set.fromList actual) == length actual)
  where
    contract = retrievalScaleDiagnosticsV1
    exact field = Just (fromIntegral (field contract))

assertExpectedRelevant :: QueryMaterialization -> RetrievalProbe -> RelevantProjection -> IO ()
assertExpectedRelevant fixture probe projection =
  assertBool
    (relevantFailure fixture probe projection)
    (any (`elem` actual) expected)
  where
    expected = expectedAdrIds fixture probe
    actual = map relevantResultAdr (take (retrievalProbeTopK probe) (relevantProjectionResults projection))

searchDiagnostics :: String -> SearchProjection -> IO JsonValue
searchDiagnostics context projection =
  case searchProjectionResults projection of
    [] -> assertFailure context >> error "unreachable"
    result : remaining -> do
      let diagnostics = searchResultRetrieval result
      assertBool context (all ((== diagnostics) . searchResultRetrieval) remaining)
      pure diagnostics

relevanceInput :: ReadSnapshot -> RetrievalProbe -> (RelevantRequest, RelevantSource)
relevanceInput snapshot probe =
  ( (defaultRelevantRequest path)
      { relevantRequestRevision = AtRevision (revisionRequested identity),
        relevantRequestLimit = 10
      },
    RevisionRelevantSource path (revisionResolved identity) ("scale-blob-" <> marker) bytes
  )
  where
    identity = readSnapshotRevision snapshot
    AdrKey index = onlyExpectedKey probe
    marker = mustJust "unique marker" (retrievalUniqueMarkerAt index)
    path = mustRightValue "probe path" (mkRepoPath ("bench/" <> marker <> ".txt"))
    template = mustJust "unique ADR template" (adrTemplateAt index)
    bytes = TextEncoding.encodeUtf8 (uniqueRelevantSource template)

uniqueRelevantSource :: AdrTemplate -> Text
uniqueRelevantSource template =
  Text.intercalate
    "\n"
    ( [ adrTemplateTitle template,
        adrTemplateSummary template
      ]
        <> [heading <> "\n" <> body | (heading, body) <- adrTemplateBodySections template]
    )

expectedAdrIds :: QueryMaterialization -> RetrievalProbe -> [AdrId]
expectedAdrIds fixture = map (mustRightValue "expected ADR" . (`lookupQueryMaterializationAdr` fixture)) . retrievalProbeExpectedAdrs

onlyExpectedKey :: RetrievalProbe -> AdrKey
onlyExpectedKey probe =
  case retrievalProbeExpectedAdrs probe of
    [key] -> key
    keys -> error ("unique scale probe requires one expected ADR key, got " <> show keys)

searchFailure :: QueryMaterialization -> RetrievalProbe -> SearchProjection -> String
searchFailure fixture probe projection =
  scaleContext
    (retrievalProbeName probe)
    (expectedAdrIds fixture probe)
    (map searchResultAdr (searchProjectionResults projection))
    ("search=" <> show (searchBounds projection))

searchBoundFailure :: RetrievalProbe -> SearchProjection -> String
searchBoundFailure probe projection =
  boundScaleContext
    (retrievalProbeName probe)
    (map searchResultAdr (searchProjectionResults projection))
    ("search=" <> show (searchBounds projection))

relevantFailure :: QueryMaterialization -> RetrievalProbe -> RelevantProjection -> String
relevantFailure fixture probe projection =
  scaleContext
    (retrievalProbeName probe)
    (expectedAdrIds fixture probe)
    (map relevantResultAdr (relevantProjectionResults projection))
    ("relevance=" <> show (relevantBounds (relevantProjectionRetrieval projection)))

scaleContext :: Text -> [AdrId] -> [AdrId] -> String -> String
scaleContext probe expected actual bounds =
  "seed="
    <> show (seedWord64 (fixtureSeed (retrievalScaleMeta retrievalScaleV1)))
    <> "; probe="
    <> Text.unpack probe
    <> "; expected="
    <> show (map adrIdText expected)
    <> "; actual="
    <> show (map adrIdText actual)
    <> "; bounds="
    <> bounds

boundScaleContext :: Text -> [AdrId] -> String -> String
boundScaleContext probe actual bounds =
  "seed="
    <> show (seedWord64 (fixtureSeed (retrievalScaleMeta retrievalScaleV1)))
    <> "; probe="
    <> Text.unpack probe
    <> "; expected=not-gated(repeated-topic)"
    <> "; actual="
    <> show (map adrIdText actual)
    <> "; bounds="
    <> bounds

searchBounds :: SearchProjection -> [(Text, Maybe Integer)]
searchBounds projection =
  case searchProjectionResults projection of
    [] -> []
    result : _ ->
      let diagnostics = searchResultRetrieval result
       in [ ("summary", jsonIntegerAt ["vector", "summary_corpus"] diagnostics),
            ("identifier", jsonIntegerAt ["vector", "identifier_corpus"] diagnostics),
            ("section", jsonIntegerAt ["vector", "section_candidates"] diagnostics),
            ("field", jsonIntegerAt ["field_rerank_candidates"] diagnostics)
          ]

relevantBounds :: RelevantRetrieval -> [(Text, Int)]
relevantBounds retrieval =
  [ ("source_chunks", relevantRetrievalSourceChunks retrieval),
    ("selected_chunks", relevantRetrievalSelectedSourceChunks retrieval),
    ("eligible_adrs", relevantRetrievalEligibleAdrs retrieval),
    ("eligible_items", relevantRetrievalEligibleSearchItems retrieval),
    ("sections", relevantRetrievalSearchSections retrieval),
    ("adr_shortlist", relevantRetrievalAdrShortlist retrieval),
    ("candidate_items", relevantRetrievalCandidateSearchItems retrieval),
    ("exact_rerank", relevantRetrievalExactRerankCandidates retrieval)
  ]

jsonIntegerAt :: [Text] -> JsonValue -> Maybe Integer
jsonIntegerAt [] (JsonNumber value) = Just value
jsonIntegerAt (key : remaining) (JsonObject fields) = lookup key fields >>= jsonIntegerAt remaining
jsonIntegerAt _ _ = Nothing

mustFixture :: Text -> Either QueryMaterializationError value -> IO value
mustFixture probe value =
  case value of
    Right result -> pure result
    Left problem -> assertFailure (scaleContext probe [] [] (show problem)) >> error "unreachable"

mustCorpus :: Either SearchVectorCorpusError value -> IO value
mustCorpus value =
  case value of
    Right result -> pure result
    Left problem -> assertFailure (scaleContext "corpus-build" [] [] (show problem)) >> error "unreachable"

mustSearch :: Maybe [AdrId] -> RetrievalProbe -> Either SearchError SearchProjection -> IO SearchProjection
mustSearch expected probe value =
  case value of
    Right result -> pure result
    Left problem -> assertFailure (probeExecutionFailure expected probe (show problem)) >> error "unreachable"

mustRelevant :: Maybe [AdrId] -> RetrievalProbe -> Either RelevantError RelevantProjection -> IO RelevantProjection
mustRelevant expected probe value =
  case value of
    Right result -> pure result
    Left problem -> assertFailure (probeExecutionFailure expected probe (show problem)) >> error "unreachable"

probeExecutionFailure :: Maybe [AdrId] -> RetrievalProbe -> String -> String
probeExecutionFailure expected probe problem =
  case expected of
    Nothing -> boundScaleContext (retrievalProbeName probe) [] problem
    Just adrIds -> scaleContext (retrievalProbeName probe) adrIds [] problem

mustRightValue :: (Show problem) => String -> Either problem value -> value
mustRightValue label value =
  case value of
    Right result -> result
    Left problem -> error (label <> " failed: " <> show problem)

mustJust :: String -> Maybe value -> value
mustJust label value =
  case value of
    Just result -> result
    Nothing -> error (label <> " unexpectedly missing")

timedProjection :: Text -> (projection -> ByteString.ByteString) -> IO projection -> IO projection
timedProjection label renderProjection action =
  timed label $ do
    projection <- action
    _ <- evaluate (ByteString.length (renderProjection projection))
    pure projection

timed :: Text -> IO value -> IO value
timed label action = do
  started <- getMonotonicTimeNSec
  result <- action
  finished <- getMonotonicTimeNSec
  TextIO.hPutStrLn
    stderr
    ( "P3-06 scale observation: "
        <> label
        <> " elapsed_ms="
        <> Text.pack (show ((finished - started) `div` 1000000))
        <> " (reporting-only)"
    )
  pure result
