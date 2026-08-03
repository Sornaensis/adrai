{-# LANGUAGE OverloadedStrings #-}

module Adrai.SearchRankingTest (tests) where

import Adrai.Format.Json
import Adrai.Retrieval
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Map.Strict as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), Assertion, assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "P3-04 search ranking contracts"
    [ testCase "retrieval modes freeze exact channel order" modeChannelContract,
      testCase "query profiles freeze every channel weight" profileWeightContract,
      testCase "weighted RRF freezes k active normalization ranks and ties" rrfContract,
      testCase "detailed semantic scoring freezes best support and cap" semanticContract,
      testCase "lexical and agreement bonuses freeze evidence and caps" bonusContract,
      testCase "shortlist forced-channel and field-rerank limits are bounded" limitContract,
      testCase "ranking fingerprint is byte-for-byte frozen" fingerprintContract,
      testCase "canonical score JSON is finite rounded and normalizes negative zero" jsonNumberContract
    ]

modeChannelContract :: Assertion
modeChannelContract = do
  map retrievalModeName [minBound .. maxBound] @?= ["fts", "vector", "hybrid"]
  map retrievalChannelName allChannels
    @?= ["fts_phrase", "fts_terms", "fts_stemmed", "fts_identifier", "semantic_vector", "identifier_vector"]
  retrievalModeChannels FtsRetrieval
    @?= [FtsPhraseChannel, FtsTermsChannel, FtsStemmedChannel, FtsIdentifierChannel]
  retrievalModeChannels VectorRetrieval
    @?= [SemanticVectorChannel, IdentifierVectorChannel]
  retrievalModeChannels HybridRetrieval @?= allChannels
  where
    allChannels = [minBound .. maxBound]

profileWeightContract :: Assertion
profileWeightContract = do
  profileWeights IdentifierProfile @?= QueryWeights 0.21 0.13 0.05 0.24 0.15 0.22
  profileWeights KeywordProfile @?= QueryWeights 0.18 0.18 0.14 0.10 0.28 0.12
  profileWeights ProseProfile @?= QueryWeights 0.09 0.10 0.20 0.05 0.46 0.10
  map (retrievalChannelWeight (profileWeights IdentifierProfile)) [minBound .. maxBound]
    @?= [0.21, 0.13, 0.05, 0.24, 0.15, 0.22]
  mapM_ (assertApprox "profile weights sum to one" 1 . sum . channelWeights . profileWeights) [minBound .. maxBound]
  where
    channelWeights weights = map (retrievalChannelWeight weights) [minBound .. maxBound]

rrfContract :: Assertion
rrfContract = do
  rrfK @?= 20
  rrfChannelRanks itemB @?= Map.singleton FtsPhraseChannel 1
  rrfChannelRanks itemA @?= Map.fromList [(FtsPhraseChannel, 2), (FtsTermsChannel, 1)]
  assertApprox "descending item id wins a raw-score tie" ((2 / 3) / 21) (rrfScore itemB)
  assertApprox "active weights are normalized before rank contributions" (((2 / 3) / 22) + ((1 / 3) / 21)) (rrfScore itemA)
  assertApprox "recorded phrase contribution" ((2 / 3) / 22) (mustLookup FtsPhraseChannel (rrfChannelContributions itemA))
  assertApprox "recorded terms contribution" ((1 / 3) / 21) (mustLookup FtsTermsChannel (rrfChannelContributions itemA))
  Map.keys onlyPhrase @?= ["a", "b"]
  assertApprox "a lone active channel is renormalized to one" (1 / 21) (rrfScore (mustLookup "b" onlyPhrase))
  assertBool "zero-weight channels do not contribute candidates" (Map.notMember "ignored" fused)
  where
    weights = QueryWeights 2 1 0 0 0 0
    phraseScores = Map.fromList [("a", 10), ("b", 10)]
    channels =
      Map.fromList
        [ (FtsPhraseChannel, phraseScores),
          (FtsTermsChannel, Map.singleton "a" 4),
          (SemanticVectorChannel, Map.singleton "ignored" 999)
        ]
    fused = weightedReciprocalRankFusion weights channels
    itemA = mustLookup "a" fused
    itemB = mustLookup "b" fused
    onlyPhrase = weightedReciprocalRankFusion weights (Map.singleton FtsPhraseChannel phraseScores)

semanticContract :: Assertion
semanticContract = do
  assertApprox "best section plus ten-percent support" 0.74 (semanticEvidenceScore evidence)
  fmap semanticSectionId (semanticBestSection evidence) @?= Just "best"
  fmap semanticSectionId (semanticSupportingSection evidence) @?= Just "support"
  fmap semanticSectionId (semanticBestSection tied) @?= Just "z"
  fmap semanticSectionId (semanticSupportingSection tied) @?= Just "a"
  fmap semanticSectionKind (semanticBestSection kindTied) @?= Just DecisionSection
  assertApprox "summary fallback is discounted" 0.46 (semanticEvidenceScore (detailedSemanticScore 0.5 []))
  assertApprox "semantic score is capped" 1 (semanticEvidenceScore (detailedSemanticScore 2 []))
  assertApprox "negative support cannot reduce score" 0.5 (semanticEvidenceScore negativeSupport)
  assertApprox "a distinct passage of the same section kind supports" 0.74 (semanticEvidenceScore sameKindSupport)
  where
    section kind identifier score = SemanticSectionScore kind identifier identifier score
    evidence =
      detailedSemanticScore
        0.5
        [ section DecisionSection "best" 0.7,
          section ContextSection "support" 0.4,
          section ConsequencesSection "negative" (-0.2)
        ]
    tied = detailedSemanticScore 0 [section ContextSection "a" 0.6, section DecisionSection "z" 0.6]
    kindTied = detailedSemanticScore 0 [section ContextSection "z" 0.6, section DecisionSection "a" 0.6]
    negativeSupport = detailedSemanticScore 0 [section DecisionSection "best" 0.5, section ContextSection "negative" (-0.4)]
    sameKindSupport = detailedSemanticScore 0 [section DecisionSection "best" 0.7, section DecisionSection "support" 0.4]

bonusContract :: Assertion
bonusContract = do
  lexicalExactPhraseFields evidence @?= ["summary", "title", "identifiers"]
  lexicalCoverage evidence @?= 1
  assertBool "identifier terms are reported" (not (null (lexicalIdentifierTerms evidence)))
  assertApprox "lexical bonus reaches its exact cap" 0.012 (lexicalBonus evidence)
  mapM_
    (uncurry (assertApprox "agreement bonus point"))
    (zip [0, 0, 0.0015, 0.003, 0.0045, 0.006, 0.006] (map agreementBonus [0, 1, 2, 3, 4, 5, 100]))
  where
    plan = buildQueryPlan "stable cache identity" []
    evidence =
      lexicalEvidence
        plan
        [ ("summary", "stable cache identity"),
          ("title", "Stable cache identity"),
          ("identifiers", queryPlanIdentifierText plan)
        ]

limitContract :: Assertion
limitContract = do
  sectionShortlistLimit 50 1 @?= 50
  sectionShortlistLimit 1000 1 @?= 120
  sectionShortlistLimit 1000 11 @?= 132
  map forcedChannelLimit [0, 1, 5, 10, 100] @?= [3, 3, 5, 10, 10]
  fieldRerankLimit 50 1 @?= 50
  fieldRerankLimit 1000 1 @?= 80
  fieldRerankLimit 1000 11 @?= 88

fingerprintContract :: Assertion
fingerprintContract = do
  rankingFingerprintPayload @?= ByteString.pack expectedPayload
  ByteString.length rankingFingerprintPayload @?= 626
  rankingImplementationFingerprint @?= "sha256:2Xiogjc9P2SaolcX_3ng2BVVp_RXj9TfoiFzJgcQMFk"
  where
    expectedPayload =
      "{\"active_weight_normalization\":\"positive-nonempty\",\"agreement_bonus_cap\":0.006,\"candidate_tie_break\":\"fused,semantic,max-fts,item-id-desc\",\"channel_tie_break\":\"raw-score-desc,item-id-desc\",\"channels\":[\"fts_phrase\",\"fts_terms\",\"fts_stemmed\",\"fts_identifier\",\"semantic_vector\",\"identifier_vector\"],\"contract\":\"adrai-search-ranking/v1\",\"field_rerank\":\"min(candidates,max(80,limit*8))+conflict-heads\",\"forced_per_channel\":\"min(10,max(3,limit))\",\"lexical_bonus_cap\":0.012,\"logical_tie_break\":\"fused,semantic,adr-id-desc\",\"rrf_k\":20.0,\"score_rounding\":\"six-decimal-ties-to-even\",\"section_shortlist\":\"min(allowed,max(120,limit*12))\"}"

jsonNumberContract :: Assertion
jsonNumberContract = do
  jsonNumberRounded6 (0 / 0) @?= Nothing
  jsonNumberRounded6 (1 / 0) @?= Nothing
  jsonNumberRounded6 ((-1) / 0) @?= Nothing
  jsonNumberRounded6 (0.5 / 1000000) @?= Just (JsonDecimal 0)
  jsonNumberRounded6 (1.5 / 1000000) @?= Just (JsonDecimal 0.000002)
  jsonNumberRounded6 (-0.0000004) @?= Just (JsonDecimal 0)
  renderCanonicalJson (mustJust (jsonNumberRounded6 (-0.0000004))) @?= "0.0\n"
  renderCanonicalJson canonical
    @?= "{\n  \"a\": 1,\n  \"score\": 1.234568,\n  \"z\": \"last\"\n}\n"
  where
    canonical =
      object
        [ ("z", JsonString "last"),
          ("score", mustJust (jsonNumberRounded6 1.2345678)),
          ("a", JsonNumber 1)
        ]

assertApprox :: String -> Double -> Double -> Assertion
assertApprox label expected actual =
  assertBool
    (label <> ": expected " <> show expected <> ", got " <> show actual)
    (abs (expected - actual) < 1e-12)

mustLookup :: (Ord key, Show key) => key -> Map.Map key value -> value
mustLookup key values =
  case Map.lookup key values of
    Just value -> value
    Nothing -> error ("missing key: " <> show key)

mustJust :: Maybe value -> value
mustJust value =
  case value of
    Just present -> present
    Nothing -> error "expected Just"
