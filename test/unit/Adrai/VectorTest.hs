{-# LANGUAGE OverloadedStrings #-}

module Adrai.VectorTest (tests) where

import Adrai.Vector
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "P3-01 vector contracts"
    [ testCase "constants, tables, seeds, and index threshold are frozen" constantsContract,
      testCase "semantic tokenization and ordered features match the static prototype" semanticFeatureContract,
      testCase "semantic identifier pieces exclude tri-prefixed values without dropping normal identifiers" semanticTriFilterContract,
      testCase "identifier expansion and ordered features preserve technical shapes" identifierFeatureContract,
      testCase "BLAKE2b-128 is native-width and drives little-endian slots" blakeContract,
      testCase "embedder validation, fingerprints, IDs, and descriptions are exact" embedderContract,
      testCase "float32 little-endian persistence handles empty, malformed, negative zero, and dot errors" persistenceContract,
      testCase "LSH empty and zero semantics, planes, probes, and dimension checks are exact" lshGeometryContract,
      testCase "candidate fallback, filtering, cap ties, overflow, duplicates, and rerank are bounded" candidateContract,
      testCase "50000 is exact and 50001 builds one compact shared-vector LSH index" sectionThresholdContract
    ]

constantsContract :: IO ()
constantsContract = do
  semanticVectorDimensions @?= 1024
  identifierVectorDimensions @?= 768
  vectorDimensions @?= 1024
  vectorIndexAlgorithm @?= "exact-summary+lsh-sections"
  (vectorIndexBands, vectorIndexBits, vectorIndexProbes, vectorIndexMinCandidates, vectorExactSectionThreshold)
    @?= (12, 8, 1, 64, 50000)
  (semanticSeedA, semanticSeedB, identifierSeedA, identifierSeedB)
    @?= ("adrai-semantic-a", "adrai-semantic-b", "adrai-identifier-a", "adrai-identifier-b")
  length aliases @?= 36
  take 3 aliases @?= [("caching", "cache"), ("cached", "cache"), ("caches", "cache")]
  drop 35 aliases @?= [("events", "event")]
  length commonTermWeights @?= 21
  lookup "system" commonTermWeights @?= Just 0.18
  lookup "process" commonTermWeights @?= Just 0.28
  length conceptClusters @?= 22
  lookup "configuration" conceptClusters
    @?= Just ["configuration", "config", "setting", "environment", "env", "variable", "default"]
  length phraseAliases @?= 10
  take 1 phraseAliases @?= [(["busy", "timeout"], ["lock", "waiting"])]
  drop 9 phraseAliases @?= [(["wal"], ["write", "ahead", "log"])]

semanticFeatureContract :: IO ()
semanticFeatureContract = do
  semanticTokens "MÉMOIZED memoized/cache:Hashes!" @?= ["m", "moiz", "memoized/cache:hash"]
  semanticTokens "Memoized cache content hash" @?= ["memoize", "cache", "content", "hash"]
  semanticFeatures "Memoized cache content hash"
    @?= [ WeightedFeature "term:memoize" 1,
          WeightedFeature "concept:cache" 1.45,
          WeightedFeature "term:cache" 1,
          WeightedFeature "concept:cache" 1.45,
          WeightedFeature "term:content" 1,
          WeightedFeature "term:hash" 1,
          WeightedFeature "concept:identity" 1.45,
          WeightedFeature "bigram:memoize:cache" 1.05,
          WeightedFeature "bigram:cache:content" 1.05,
          WeightedFeature "bigram:content:hash" 1.05,
          WeightedFeature "concept-bigram:cache:cache" 1.20,
          WeightedFeature "concept-bigram:cache:identity" 1.20,
          WeightedFeature "alias:fingerprint" 0.48,
          WeightedFeature "identifier-piece:memoize" 0.24,
          WeightedFeature "identifier-piece:cache" 0.24,
          WeightedFeature "identifier-piece:content" 0.24,
          WeightedFeature "identifier-piece:hash" 0.24
        ]

semanticTriFilterContract :: IO ()
semanticTriFilterContract = do
  let triNames = map weightedFeatureName (semanticFeatures "tri")
      mixedNames = map weightedFeatureName (semanticFeatures "tri cache")
      normalNames = map weightedFeatureName (semanticFeatures "DropwireLease")
  triNames @?= ["term:tri"]
  mixedNames
    @?= [ "term:tri",
          "term:cache",
          "concept:cache",
          "bigram:tri:cache",
          "identifier-piece:cache"
        ]
  assertBool "normal identifier whole remains represented" ("identifier-piece:dropwirelease" `elem` normalNames)
  assertBool "normal identifier part remains represented" ("identifier-piece:dropwire" `elem` normalNames)

identifierFeatureContract :: IO ()
identifierFeatureContract = do
  splitIdentifier "HTTPServer2/cache-key" @?= ["http", "server", "cache", "key"]
  identifierTerms False "HTTPServer2/cache-key"
    @?= [ "httpserver2/cache-key",
          "httpserver2cachekey",
          "http",
          "server",
          "cache",
          "key",
          "http_server",
          "server_cache",
          "cache_key"
        ]
  map weightedFeatureName (identifierFeatures "DropwireLease")
    @?= [ "whole:dropwirelease",
          "part:dropwire",
          "part:lease",
          "pair:dropwire:lease"
        ]
      <> ["trigram:" <> Text.take 3 (Text.drop index "dropwirelease") | index <- [0 .. 10]]
  map weightedFeatureWeight (take 4 (identifierFeatures "DropwireLease")) @?= [1.30, 0.95, 0.95, 1.15]

blakeContract :: IO ()
blakeContract = do
  let expected128 = BS.pack [0xca, 0xe6, 0x69, 0x41, 0xd9, 0xef, 0xbd, 0x40, 0x4e, 0x4d, 0x88, 0x75, 0x8e, 0xa6, 0x76, 0x70]
      truncated512 = BS.pack [0x78, 0x6a, 0x02, 0xf7, 0x42, 0x01, 0x59, 0x03, 0xc6, 0xc6, 0xfd, 0x85, 0x25, 0x52, 0xd2, 0x72]
      actual = blake2b128Digest BS.empty
  actual @?= expected128
  assertBool "BLAKE2b-128 must not be the first 16 bytes of BLAKE2b-512" (actual /= truncated512)
  slotFromDigest expected128 512 @?= Right (202, -1)
  slotFromDigest (BS.take 8 expected128) 512 @?= Left (InvalidFeatureDigestLength 8)
  slotFromDigest expected128 0 @?= Left (InvalidHalfDimensions 0)
  featureSlot semanticSeedA "term:memoize" 0 @?= Left (InvalidHalfDimensions 0)

embedderContract :: IO ()
embedderContract = do
  mkEmbedder SemanticSpace 7 @?= Left (InvalidEmbedderDimensions 7)
  mkEmbedder SemanticSpace 9 @?= Left (InvalidEmbedderDimensions 9)
  embedderVectorId semanticEmbedder @?= "adrai-semantic:T_yApQ2sJvyBH21HYYBYuQ1X:1024"
  embedderVectorId identifierEmbedder @?= "adrai-identifier:-JSyvxW6dxoDl9Pzpm6Q_aoG:768"
  embedderDescription semanticEmbedder
    @?= [ ("implementation", "adrai-vector"),
          ("space", "semantic"),
          ("dimensions", "1024"),
          ("fingerprint", embedderFingerprint semanticEmbedder),
          ("vector_id", embedderVectorId semanticEmbedder),
          ("index", vectorIndexAlgorithm)
        ]
  denseDimension (semanticEmbedding "compiler cache") @?= semanticVectorDimensions
  denseDimension (identifierEmbedding "DropwireLease") @?= identifierVectorDimensions

persistenceContract :: IO ()
persistenceContract = do
  let negativeZero = castWord64ToDouble 0x8000000000000000
      negativeZeroBytes = BS.pack [0, 0, 0, 128]
  unpackVector BS.empty @?= Right (denseVector [])
  dot (denseVector []) (denseVector []) @?= Right 0
  dot (denseVector [1]) (denseVector [1, 2]) @?= Left (VectorDimensionMismatch 1 2)
  unpackVector (BS.pack [0, 1, 2]) @?= Left (InvalidVectorBlobLength 3)
  packVector (denseVector [negativeZero]) @?= negativeZeroBytes
  case unpackVector negativeZeroBytes of
    Left failure -> assertBool (show failure) False
    Right vector -> map castDoubleToWord64 (denseValues vector) @?= [0x8000000000000000]
  map castDoubleToWord64 (denseValues (canonicalFloat32Vector (denseVector [negativeZero]))) @?= [0x8000000000000000]
  packVector (denseVector [1.0, -2.5]) @?= BS.pack [0, 0, 128, 63, 0, 0, 32, 192]

lshGeometryContract :: IO ()
lshGeometryContract = do
  buildLshPlan 0 "seed" @?= Left (InvalidLshDimensions 0)
  let plan = mustRight (buildLshPlan 8 "frozen-test-seed")
      planes = lshPlanPlanes plan
  length planes @?= 12
  assertBool "each band has eight planes" (all ((== 8) . length) planes)
  assertBool "dimension-eight planes sample every slot exactly once" (all (all ((== [0 .. 7]) . quickSort . map fst)) planes)
  lshSignatures plan (denseVector []) @?= Right (replicate 12 (LshBucket 0))
  lshSignatures plan (zeroVector 8) @?= Right (replicate 12 (LshBucket 255))
  lshSignatures plan (zeroVector 10) @?= Left (LshPlanDimensionMismatch 8 10)
  lshProbeBuckets (LshBucket 0) @?= map LshBucket [0, 1, 2, 4, 8, 16, 32, 64, 128]

candidateContract :: IO ()
candidateContract = do
  let plan = mustRight (buildLshPlan 8 "candidate-seed")
      one = mustRight (buildLshIndex plan [("a", zeroVector 8)])
      allowedWithUnknown = Set.fromList ["a", "b", "c"]
      (normal, normalDiag) = mustRight (selectCandidates one (zeroVector 8) allowedWithUnknown 1 True)
      (sparse, sparseDiag) = mustRight (selectCandidates one (zeroVector 8) allowedWithUnknown 1 False)
  normal @?= allowedWithUnknown
  candidateFallback normalDiag @?= CandidateExactFallback
  sparse @?= Set.singleton "a"
  candidateFallback sparseDiag @?= CandidateSparseFallback
  buildLshIndex plan [("duplicate", zeroVector 8), ("duplicate", zeroVector 8)]
    @?= Left (DuplicateLshItemId "duplicate")

  let manyEntries = [(itemId index, zeroVector 8) | index <- [0 .. 599]]
      many = mustRight (buildLshIndex plan manyEntries)
      allIds = Set.fromList (map fst manyEntries)
      (capped, cappedDiag) = mustRight (selectCandidates many (zeroVector 8) allIds 1 True)
  Set.size capped @?= 512
  assertBool "descending item-ID tie break keeps the largest ID" (Set.member "item-599" capped)
  assertBool "descending item-ID tie break drops the smallest ID" (not (Set.member "item-000" capped))
  candidateCap cappedDiag @?= Just 512

  let (_, overflowDiag) = mustRight (selectCandidates many (zeroVector 8) allIds maxBound True)
  assertBool "target arithmetic widens before multiplication" (candidateTarget overflowDiag > toInteger (maxBound :: Int))

  rerankByDot
    2
    (denseVector [1, 0])
    (Map.fromList [("a", denseVector [0, 1]), ("b", denseVector [1, 0]), ("c", denseVector [1, 0])])
    (Set.fromList ["a", "b", "c"])
    @?= Right [("c", 1), ("b", 1)]

  let query = denseVector [1, 0, 0, 0, 0, 0, 0, 0]
      designedEntries =
        [ ("nearest", query),
          ("orthogonal", denseVector [0, 1, 0, 0, 0, 0, 0, 0]),
          ("opposite", denseVector [-1, 0, 0, 0, 0, 0, 0, 0])
        ]
      designedCorpus = Map.fromList designedEntries
      designedIndex = mustRight (buildLshIndex plan designedEntries)
      designedAllowed = Set.insert "not-indexed" (Map.keysSet designedCorpus)
      designedSelected = fst (mustRight (selectCandidates designedIndex query designedAllowed 1 False))
      selectedRanking = mustRight (rerankByDot 1 query designedCorpus designedSelected)
      bruteRanking = mustRight (rerankByDot 1 query designedCorpus (Map.keysSet designedCorpus))
  assertBool "the designed global nearest neighbor shares all exact query buckets" (Set.member "nearest" designedSelected)
  selectedRanking @?= [("nearest", 1)]
  selectedRanking @?= bruteRanking

sectionThresholdContract :: IO ()
sectionThresholdContract = do
  sectionIndexMode 50000 @?= ExactSectionScan
  sectionIndexMode 50001 @?= LshSectionScan
  let plan = mustRight (buildLshPlan 8 "boundary-seed")
      sharedSignature = replicate 12 (LshBucket 255)
      entries = [(Text.pack (show itemOrdinal), sharedSignature) | itemOrdinal <- [1 .. 50001 :: Int]]
      index = mustRight (buildLshIndexFromSignatures plan entries)
  lshIndexItemCount index @?= 50001
  lshIndexMembershipCount index @?= 12 * 50001

itemId :: Int -> Text
itemId value = "item-" <> Text.pack (replicate (3 - length digits) '0' <> digits)
  where
    digits = show value

quickSort :: (Ord value) => [value] -> [value]
quickSort [] = []
quickSort (value : values) = quickSort [x | x <- values, x <= value] <> [value] <> quickSort [x | x <- values, x > value]

mustRight :: (Show failure) => Either failure value -> value
mustRight (Right value) = value
mustRight (Left failure) = error (show failure)
