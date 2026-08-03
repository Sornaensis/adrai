{-# LANGUAGE OverloadedStrings #-}

module Adrai.SearchRankingProperties (tests) where

import Adrai.Domain (Domain, mkDomain)
import Adrai.Format.Json (jsonNumberRounded6, renderCanonicalJson)
import Adrai.Property.Generators (IdentifierPool (..), adrIdAt, recordIdAt, sampledPermutation)
import Adrai.Query (chooseBestByAdr, domainsMatchRequested, sortCandidateIds, sortLogicalAdrs)
import Adrai.Retrieval
import Adrai.Types (AdrId, RecordId, StateToken, mkStateToken)
import Data.List (sort, sortBy)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Ord (Down (..), comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Hedgehog (Gen, Property, assert, forAll, property, withTests, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "P3-04 ranking properties"
    [ testProperty "weighted RRF agrees with an independent reference" propRrfReference,
      testProperty "RRF is invariant under map insertion permutations" propRrfPermutation,
      testProperty "candidate caps obey their exact bounded formulas" propCandidateBounds,
      testProperty "detailed semantic score agrees with the independent two-section model" propSemanticReference,
      testProperty "domain filtering is invariant under requested and logical permutations" propFilterPermutation,
      testProperty "grouping yields unique ADRs in a stable total order" propGroupingStable,
      testProperty "finite scores always render as stable finite rounded JSON" propFiniteRoundedScores
    ]

propRrfReference :: Property
propRrfReference = withTests 120 . property $ do
  phraseValues <- forAll scoreValues
  semanticValues <- forAll scoreValues
  includePhrase <- forAll Gen.bool
  includeSemantic <- forAll Gen.bool
  let phrase = if includePhrase then scoreMap phraseValues else Map.empty
      semantic = if includeSemantic then scoreMap semanticValues else Map.empty
      channels = Map.fromList [(FtsPhraseChannel, phrase), (SemanticVectorChannel, semantic)]
      weights = profileWeights KeywordProfile
      actual = weightedReciprocalRankFusion weights channels
      expected = referenceRrf weights channels
  Map.keysSet actual === Map.keysSet expected
  mapM_
    (\item -> assert (abs (maybeScore item actual - Map.findWithDefault 0 item expected) < 1.0e-12))
    (Map.keys expected)

propRrfPermutation :: Property
propRrfPermutation = withTests 100 . property $ do
  firstValues <- forAll scoreValues
  secondValues <- forAll scoreValues
  let first = (FtsTermsChannel, scoreMap firstValues)
      second = (IdentifierVectorChannel, scoreMap secondValues)
      weights = profileWeights IdentifierProfile
  weightedReciprocalRankFusion weights (Map.fromList [first, second])
    === weightedReciprocalRankFusion weights (Map.fromList [second, first])

propCandidateBounds :: Property
propCandidateBounds = withTests 100 . property $ do
  allowed <- forAll (Gen.int (Range.linear 0 10000))
  limit <- forAll (Gen.int (Range.linear 1 1000))
  sectionShortlistLimit allowed limit === min allowed (max 120 (limit * 12))
  forcedChannelLimit limit === min 10 (max 3 limit)
  fieldRerankLimit allowed limit === min allowed (max 80 (limit * 8))

propSemanticReference :: Property
propSemanticReference = withTests 100 . property $ do
  summary <- forAll score
  first <- forAll score
  second <- forAll score
  let sections =
        [ SemanticSectionScore DecisionSection "section-a" "a" first,
          SemanticSectionScore ContextSection "section-b" "b" second
        ]
      actual = semanticEvidenceScore (detailedSemanticScore summary sections)
      ordered = reverse (sort [first, second])
      best = case ordered of value : _ -> value; [] -> negate (1 / 0)
      supporting = case ordered of _ : value : _ -> max 0 value * 0.10; _ -> 0
      expected = min 1 (max (summary * 0.92) best + supporting)
  assert (abs (actual - expected) < 1.0e-12)

propFilterPermutation :: Property
propFilterPermutation = withTests 80 . property $ do
  includeSecurity <- forAll Gen.bool
  requested <- forAll (sampledPermutation requestedDomains)
  logical <- forAll (sampledPermutation (compilerDomains <> securityDomains includeSecurity))
  domainsMatchRequested requested logical === includeSecurity
  domainsMatchRequested requested logical
    === domainsMatchRequested requestedDomains (compilerDomains <> securityDomains includeSecurity)
  where
    requestedDomains = map mustDomain ["compiler", "security"]
    compilerDomains = map mustDomain ["compiler.cache", "compiler.search"]
    securityDomains True = [mustDomain "security.auth"]
    securityDomains False = []

propGroupingStable :: Property
propGroupingStable = withTests 100 . property $ do
  scores <- forAll scoreValues
  fusedPairs <- forAll (sampledPermutation (zipWith fusedPair items scores))
  semanticPairs <- forAll (sampledPermutation (zip items (reverse scores)))
  ftsPairs <- forAll (sampledPermutation (zip items scores))
  documentPairs <- forAll (sampledPermutation documents)
  let fused = Map.fromList fusedPairs
      semantic = Map.fromList semanticPairs
      fts = Map.singleton FtsTermsChannel (Map.fromList ftsPairs)
      documentsById = Map.fromList documentPairs
      ordered = sortCandidateIds fused semantic fts
      grouped = chooseBestByAdr ordered documentsById
      logical = sortLogicalAdrs grouped fused semantic
      canonicalFused = Map.fromList (zipWith fusedPair items scores)
      canonicalSemantic = Map.fromList (zip items (reverse scores))
      canonicalFts = Map.singleton FtsTermsChannel (Map.fromList (zip items scores))
      canonicalDocuments = Map.fromList documents
      canonicalOrdered = sortCandidateIds canonicalFused canonicalSemantic canonicalFts
      canonicalGrouped = chooseBestByAdr canonicalOrdered canonicalDocuments
      canonicalLogical = sortLogicalAdrs canonicalGrouped canonicalFused canonicalSemantic
  ordered === canonicalOrdered
  grouped === canonicalGrouped
  logical === canonicalLogical
  Map.keysSet grouped === Set.fromList [firstAdr, secondAdr]
  Set.size (Set.fromList logical) === length logical

propFiniteRoundedScores :: Property
propFiniteRoundedScores = withTests 120 . property $ do
  whole <- forAll (Gen.integral (Range.linear (-1000000) 1000000))
  millionth <- forAll (Gen.int (Range.linear (-999999) 999999))
  let value = fromInteger whole + fromIntegral millionth / 1000000
  case jsonNumberRounded6 value of
    Nothing -> assert False
    Just encoded -> do
      let rendered = renderCanonicalJson encoded
      assert (not ("NaN" `Text.isInfixOf` rendered))
      assert (not ("Infinity" `Text.isInfixOf` rendered))
      renderCanonicalJson encoded === rendered

items :: [Text]
items = ["A00000000000000000000000001", "A00000000000000000000000002", "A00000000000000000000000003"]

scoreValues :: Gen [Double]
scoreValues = traverse (const score) items

score :: Gen Double
score = (/ 100) . fromIntegral <$> Gen.int (Range.linear (-100) 100)

scoreMap :: [Double] -> Map Text Double
scoreMap = Map.fromList . zip items

fusedPair :: Text -> Double -> (Text, RrfEvidence)
fusedPair item value = (item, RrfEvidence value Map.empty Map.empty)

firstAdr :: AdrId
firstAdr = adrIdAt (IdentifierPool 0) 0

secondAdr :: AdrId
secondAdr = adrIdAt (IdentifierPool 0) 1

documents :: [(Text, SearchDocument)]
documents =
  [ (items !! 0, testDocument (items !! 0) firstAdr (recordIdAt (IdentifierPool 0) 0)),
    (items !! 1, testDocument (items !! 1) firstAdr (recordIdAt (IdentifierPool 0) 1)),
    (items !! 2, testDocument (items !! 2) secondAdr (recordIdAt (IdentifierPool 0) 2))
  ]

testDocument :: Text -> AdrId -> RecordId -> SearchDocument
testDocument item adr record =
  SearchDocument
    { searchDocumentItemId = item,
      searchDocumentAdrId = adr,
      searchDocumentCandidateRecordId = record,
      searchDocumentTitle = "title",
      searchDocumentSummary = "summary",
      searchDocumentContext = "context",
      searchDocumentDecision = "decision",
      searchDocumentConsequences = "consequences",
      searchDocumentDomains = ["compiler"],
      searchDocumentRationale = "rationale",
      searchDocumentOther = "other",
      searchDocumentScope = ["src/**"],
      searchDocumentObsolete = False,
      searchDocumentConflicted = False,
      searchDocumentStateToken = mustStateToken,
      searchDocumentSourcePaths = ["docs/adr.md"],
      searchDocumentIdentifierSource = item,
      searchDocumentIdentifiers = item
    }

mustDomain :: Text -> Domain
mustDomain value = either (error . show) id (mkDomain value)

mustStateToken :: StateToken
mustStateToken = either (error . show) id (mkStateToken "S0000000000000000000000")

maybeScore :: Text -> Map Text RrfEvidence -> Double
maybeScore item values = maybe 0 rrfScore (Map.lookup item values)

referenceRrf :: QueryWeights -> Map RetrievalChannel (Map Text Double) -> Map Text Double
referenceRrf weights channels
  | total <= 0 = Map.empty
  | otherwise = Map.fromListWith (+) contributions
  where
    active =
      [ (channel, retrievalChannelWeight weights channel, scores)
        | (channel, scores) <- Map.toAscList channels,
          not (Map.null scores),
          retrievalChannelWeight weights channel > 0
      ]
    total = sum [weight | (_, weight, _) <- active]
    contributions =
      [ (item, (weight / total) / (rrfK + fromIntegral rank))
        | (_channel, weight, scores) <- active,
          (rank, (item, _raw)) <- zip [1 :: Int ..] (sortBy (comparing (Down . snd) <> comparing (Down . fst)) (Map.toList scores))
      ]
