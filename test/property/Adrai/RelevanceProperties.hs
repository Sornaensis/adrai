{-# LANGUAGE OverloadedStrings #-}

module Adrai.RelevanceProperties (tests) where

import Adrai.Property.Generators (sampledPermutation)
import Adrai.Relevance
import qualified Data.ByteString as ByteString
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Hedgehog (Gen, Property, assert, forAll, property, withTests, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "P3-05 relevance properties"
    [ testProperty "relevance chunk normalization ranges and fingerprint are deterministic" propChunkNormalization,
      testProperty "relevance replaces malformed utf8 and chunks long lines" propMalformedAndLongLine,
      testProperty "identical bytes yield identical chunks across filenames" propFilenameIndependent,
      testProperty "embedding whitespace collapses without evidence mutation" propEmbeddingEvidence,
      testProperty "informative selection is permutation invariant above its bound" propInformativePermutation,
      testProperty "evidence aggregation is permutation invariant and unique by source" propAggregatePermutation,
      testProperty "accepted aggregate scores remain finite and bounded" propAggregateFinite
    ]

propChunkNormalization :: Property
propChunkNormalization = withTests 80 . property $ do
  alpha <- forAll tokenText
  beta <- forAll tokenText
  gamma <- forAll tokenText
  let windows = Text.replicate 260 alpha <> "\r\n" <> Text.replicate 260 beta <> "\r" <> Text.replicate 260 gamma
      normalized = normalizeNewlines windows
  chunkText windows === chunkText normalized
  relevanceChunkingFingerprint === "sha256:Nh2G1cvQT0C8UX8nxSoUMS7NI2caJ_rJOHgTwncHEc4"

propMalformedAndLongLine :: Property
propMalformedAndLongLine = withTests 40 . property $ do
  repeats <- forAll (Gen.int (Range.linear 500 750))
  decodeTextBytes "source" (ByteString.pack [97, 255, 98]) === Right ("a" <> Text.singleton '\xfffd' <> "b")
  case chunkText (Text.replicate repeats "semantic-cache-key ") of
    Left _ -> assert False
    Right chunks -> do
      assert (length chunks > 2)
      assert (all ((== 1) . textChunkStartLine) chunks)
      assert (all ((== 1) . textChunkEndLine) chunks)

propFilenameIndependent :: Property
propFilenameIndependent = withTests 80 . property $ do
  linesCount <- forAll (Gen.int (Range.linear 1 120))
  let bytes = TextEncoding.encodeUtf8 (Text.replicate linesCount "durable queue acknowledgement after transaction commit\n")
      chunksFor label =
        case decodeTextBytes label bytes of
          Left _ -> Nothing
          Right decoded -> either (const Nothing) Just (chunkText decoded)
      expected = chunksFor "worker.py"
  assert (maybe False (not . null) expected)
  map chunksFor filenameShapes === replicate (length filenameShapes) expected

propEmbeddingEvidence :: Property
propEmbeddingEvidence = withTests 80 . property $ do
  first <- forAll tokenText
  second <- forAll tokenText
  third <- forAll tokenText
  let evidence = first <> "\n\n " <> second <> "\t " <> third
  embeddingText evidence === Text.unwords [first, second, third]
  case chunkText evidence of
    Right [single] -> textChunkText single === evidence
    _ -> assert False

propInformativePermutation :: Property
propInformativePermutation = withTests 80 . property $ do
  shuffled <- forAll (sampledPermutation informativeChunks)
  selectInformativeChunks shuffled === selectInformativeChunks informativeChunks
  let selected = selectInformativeChunks shuffled
  assert (length selected <= relevanceMaxSourceQueryChunks)
  assert (Set.size (Set.fromList (map textChunkOrdinal selected)) == length selected)

propAggregatePermutation :: Property
propAggregatePermutation = withTests 100 . property $ do
  shuffled <- forAll (sampledPermutation aggregateMatches)
  aggregateRelevance shuffled (Just RelevanceScopeAmbiguous)
    === aggregateRelevance aggregateMatches (Just RelevanceScopeAmbiguous)
  case aggregateRelevance shuffled Nothing of
    Nothing -> assert False
    Just result ->
      Set.size (Set.fromList (map (textChunkOrdinal . pairFileChunk) (aggregateEvidence result)))
        === length (aggregateEvidence result)

propAggregateFinite :: Property
propAggregateFinite = withTests 120 . property $ do
  semantic <- forAll score
  lexical <- forAll score
  let result = aggregateRelevance [PairMatch (TextChunk 0 1 1 "unique source fact") "item" "adr" semantic lexical] (Just RelevanceScopeExact)
  case result of
    Nothing -> assert (semantic < minimumPairScore && lexical < minimumLexicalPairScore)
    Just aggregate -> do
      assert (finite (aggregateSemanticScore aggregate))
      assert (finite (aggregateLexicalScore aggregate))
      assert (finite (aggregateScore aggregate))
      assert (aggregateSemanticScore aggregate >= 0 && aggregateSemanticScore aggregate <= 1)
      assert (aggregateLexicalScore aggregate >= 0 && aggregateLexicalScore aggregate <= 1)
      assert (aggregateScore aggregate >= 0 && aggregateScore aggregate <= 1)

tokenText :: Gen Text.Text
tokenText = Gen.text (Range.linear 1 8) (Gen.element ['a' .. 'z'])

score :: Gen Double
score = (/ 1000) . fromIntegral <$> Gen.int (Range.linear (-1000) 1000)

filenameShapes :: [Text.Text]
filenameShapes = ["worker.py", "worker.txt", "worker.compose", "worker.sql", "worker.unknown", "worker"]

informativeChunks :: [TextChunk]
informativeChunks =
  [ TextChunk ordinal (ordinal + 1) (ordinal + 1) text
    | ordinal <- [0 .. 31],
      let text = if ordinal `elem` [7, 21, 31] then "unique lease fencing epoch token" <> Text.pack (show ordinal) else "generic service module"
  ]

aggregateMatches :: [PairMatch]
aggregateMatches =
  [ PairMatch (TextChunk 0 1 1 "cache lease fencing") "head-a" "adr-a" 0.50 0.20,
    PairMatch (TextChunk 0 1 1 "cache lease fencing") "head-b" "adr-b" 0.45 0.40,
    PairMatch (TextChunk 1 2 2 "adjacent queue") "adjacent" "adr" 0.44 0.10,
    PairMatch (TextChunk 3 4 4 "storage transaction") "third" "adr" 0.40 0.10,
    PairMatch (TextChunk 5 6 6 "observability trace") "fifth" "adr" 0.30 0
  ]

finite :: Double -> Bool
finite value = not (isNaN value || isInfinite value)
