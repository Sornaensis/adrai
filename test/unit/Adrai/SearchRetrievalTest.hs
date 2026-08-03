{-# LANGUAGE OverloadedStrings #-}

module Adrai.SearchRetrievalTest (tests) where

import Adrai.Retrieval
import Adrai.Sqlite
import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "P3-02 retrieval contracts"
    [ testCase "31 scaffolding terms and profile weights are frozen" constantsContract,
      testCase "profile classification includes Unicode decimal digits" profileContract,
      testCase "query planning preserves phrase duplicates and bounds only fallback channels" planContract,
      testCase "aliases are ordered asymmetric and isolated from exact channels" aliasRoutingContract,
      testCase "local acronym extraction has exact grammar and first-wins ordering" aliasExtractionContract,
      testCase "section sources are ordered weighted and non-empty" sectionContract,
      testCase "six FTS targets close schema tokenizer and BM25 choices" ftsTargetContract,
      testCase "candidate limit validation widens before cap arithmetic" limitContract,
      testCase "retrieval fingerprint is byte-for-byte frozen" fingerprintContract
    ]

constantsContract :: IO ()
constantsContract = do
  Set.size queryScaffolding @?= 31
  Set.toAscList queryScaffolding
    @?= ["a", "about", "an", "and", "are", "can", "could", "did", "do", "does", "for", "from", "how", "in", "is", "it", "of", "on", "our", "should", "the", "this", "to", "we", "what", "when", "where", "which", "why", "with", "would"]
  profileWeights IdentifierProfile @?= QueryWeights 0.21 0.13 0.05 0.24 0.15 0.22
  profileWeights KeywordProfile @?= QueryWeights 0.18 0.18 0.14 0.10 0.28 0.12
  profileWeights ProseProfile @?= QueryWeights 0.09 0.10 0.20 0.05 0.46 0.10

profileContract :: IO ()
profileContract = do
  map classifyQuery ["cache", "How do caches work", "six plain words make this prose"]
    @?= [KeywordProfile, ProseProfile, ProseProfile]
  map classifyQuery ["cache_key", "cacheKey", "HTTP", "cache\x0661"]
    @?= replicate 4 IdentifierProfile
  classifyQuery "ordinary²" @?= KeywordProfile

planContract :: IO ()
planContract = do
  let plan = buildQueryPlan "cache cache identity" []
  queryPlanFtsExactPhrase plan @?= "\"cache cache identity\""
  queryPlanSemanticTerms plan @?= ["cache", "identity"]
  queryPlanFtsExactTerms plan @?= ["\"cache\"", "\"identity\""]
  queryPlanFtsNear plan @?= "NEAR(\"cache\" \"identity\", 6)"
  let allStop = buildQueryPlan "what do we do" []
  queryPlanSemanticTerms allStop @?= ["what", "do", "we"]
  queryPlanFtsExactTerms allStop @?= ["\"what\"", "\"do\"", "\"we\""]
  let tokens = take 49 twoLetterTokens
      boundary = buildQueryPlan (Text.unwords tokens) []
  length (queryPlanFtsExactTerms boundary) @?= 49
  assertBool "unbounded exact phrase retains final token" ("fw\"" `Text.isSuffixOf` queryPlanFtsExactPhrase boundary)
  length (Text.splitOn " OR " (queryPlanFtsPrefix boundary)) @?= 12
  length (Text.splitOn " AND " (queryPlanFtsStemmed boundary)) @?= 12
  length (Text.splitOn " OR " (queryPlanFtsIdentifier boundary)) @?= 48
  queryPlanFtsNear boundary @?= ""
  ftsQuote "a\"b" @?= "\"a\"\"b\""
  let technicalIdentifierTerms = Text.words (queryPlanIdentifierText (buildQueryPlan "CacheKeyFactory" []))
      ordinaryProseTerms = Text.words (queryPlanIdentifierText (buildQueryPlan "cache identity" []))
  assertBool "technical identifier channel includes tri-prefixed terms" (any ("tri" `Text.isPrefixOf`) technicalIdentifierTerms)
  assertBool "ordinary prose identifier channel excludes tri-prefixed terms" (all (not . ("tri" `Text.isPrefixOf`)) ordinaryProseTerms)

aliasRoutingContract :: IO ()
aliasRoutingContract = do
  let aliases = [("http", "hypertext transfer protocol"), ("db", "database"), ("web", "hypertext transfer protocol"), ("empty", "")]
      forward = buildQueryPlan "HTTP" aliases
      reversePlan = buildQueryPlan "hypertext transfer protocol" aliases
      asymmetric = buildQueryPlan "databases" aliases
  queryPlanAliases forward @?= ["hypertext transfer protocol"]
  queryPlanSemanticText forward @?= "HTTP hypertext transfer protocol"
  queryPlanRawTokens forward @?= ["http"]
  queryPlanFtsStemmed forward @?= "\"http\""
  queryPlanAliases reversePlan @?= ["http", "web"]
  queryPlanAliases asymmetric @?= ["db"]
  queryPlanAliases (buildQueryPlan "xhttpx" aliases) @?= []

aliasExtractionContract :: IO ()
aliasExtractionContract = do
  extractLocalAliases ["HyperText Transfer Protocol (HTTP)"] @?= [("http", "hypertext transfer protocol")]
  extractLocalAliases ["HTTP (HyperText Transfer Protocol)"] @?= [("http", "hypertext transfer protocol")]
  extractLocalAliases ["Alpha Beta (AB)", "Another Binding (AB)"] @?= [("ab", "alpha beta")]
  extractLocalAliases ["Bad (A)", "Bad Alias (BAD_ALIAS)", "X.Y (XY)"] @?= []

sectionContract :: IO ()
sectionContract = do
  let actual = sectionSources "Title" "Summary" "# Context\nwhy\n# Decision\nchoose\n# Consequences\ncost" ["cache", "storage"] "because"
  map sectionSourceKind actual
    @?= [TitleSummarySection, DecisionSection, RationaleSection, DomainsSection, ContextSection, ConsequencesSection]
  map sectionSourceWeight actual @?= [1.12, 1, 0.84, 0.62, 0.55, 0.45]

ftsTargetContract :: IO ()
ftsTargetContract = do
  length allFtsTargets @?= 6
  map ftsTargetTokenizer allFtsTargets
    @?= ["unicode61", "porter unicode61", "unicode61", "unicode61", "porter unicode61", "unicode61"]
  map (length . ftsTargetColumns) allFtsTargets @?= [11, 9, 4, 6, 5, 5]
  ftsTargetBm25Weights SearchExactTarget @?= [0, 0, 0, 9, 7, 5.5, 2, 3, 1.5, 1, 4.5]
  ftsTargetBm25Weights PassageIdentifierTarget @?= [0, 0, 0, 0, 6]

limitContract :: IO ()
limitContract = do
  mkCandidateLimit 0 @?= Left (InvalidCandidateLimit 0)
  mkCandidateLimit (-1) @?= Left (InvalidCandidateLimit (-1))
  let beyondInt64 = toInteger (maxBound :: Int64) + 1
  mkCandidateLimit beyondInt64 @?= Left (CandidateLimitOverflow beyondInt64)
  candidateLimitValue (mustRight (mkCandidateLimit 1)) @?= 1
  candidateLimitValue (mustRight (candidateCap (mustRight (mkCandidateLimit 1)))) @?= 240
  candidateLimitValue (mustRight (candidateCap (mustRight (mkCandidateLimit 9)))) @?= 270
  let validMaximum = toInteger (maxBound :: Int64)
      overflowedCap = validMaximum * 30
  candidateCap (mustRight (mkCandidateLimit validMaximum)) @?= Left (CandidateLimitOverflow overflowedCap)

fingerprintContract :: IO ()
fingerprintContract = do
  BS.length retrievalFingerprintPayload @?= 1229
  retrievalImplementationFingerprint @?= "sha256:u87mCRUwjVfElqaHx1QiRSiMUx5V_eDdIJ8XHsm1AAI"

twoLetterTokens :: [Text]
twoLetterTokens = [Text.pack [left, right] | left <- ['e' .. 'f'], right <- ['a' .. 'z']]

mustRight :: (Show failure) => Either failure value -> value
mustRight (Right value) = value
mustRight (Left failure) = error (show failure)
