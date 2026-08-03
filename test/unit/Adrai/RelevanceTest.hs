{-# LANGUAGE OverloadedStrings #-}

module Adrai.RelevanceTest (tests) where

import Adrai.Relevance
import Adrai.Vector (SectionIndexMode (..), sectionIndexMode)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar8
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "P3-05 relevance core"
    [ testCase "relevance whitespace binary and size limits are explicit" rawTextBoundaryContract,
      testCase "only CRLF and CR are relevance line boundaries" newlineNormalizationContract,
      testCase "embedding normalization does not mutate evidence text" embeddingContract,
      testCase "informative selection is bounded content-only and ordinal stable" informativeContract,
      testCase "focused excerpts retain exact source line ranges" excerptContract,
      testCase "independent evidence aggregation applies exact weights and scope bonus" aggregationContract,
      testCase "high confidence requires strength and result margin" confidenceContract,
      testCase "relevance section selection changes after exactly 50000 passages" sectionSelectionBoundaryContract,
      testCase "relevance fingerprints freeze compact constants and scoring" fingerprintContract
    ]

rawTextBoundaryContract :: IO ()
rawTextBoundaryContract = do
  decodeTextBytes "file" (ByteString.pack [112, 114, 101, 102, 105, 120, 255, 115, 117, 102, 102, 105, 120])
    @?= Right "prefix\xfffdsuffix"
  decodeTextBytes "file" "text\0binary" @?= Left (TextAppearsBinary "file" BinaryContainsNul)
  decodeTextBytes "file" (ByteString.replicate (maxTextBytes + 1) 97)
    @?= Left (TextBytesTooLarge "file" (maxTextBytes + 1) maxTextBytes)
  assertBool "exact raw byte cap is accepted" (isRight (decodeTextBytes "file" (ByteString.replicate maxTextBytes 97)))
  assertBool "exactly twenty percent controls remains textual" (isRight (decodeTextBytes "sample" (ByteString.replicate 20 1 <> ByteString.replicate 80 97)))
  decodeTextBytes "sample" (ByteString.replicate 21 1 <> ByteString.replicate 79 97)
    @?= Left (TextAppearsBinary "sample" (BinaryTooManyControls 21 100))
  let malformed = ByteString.replicate 32 255
      decoded = expectDecoded (decodeTextBytesWithLimit 64 "malformed" malformed)
  assertBool "lenient replacement expands when re-encoded" (ByteString.length (TextEncoding.encodeUtf8 decoded) > ByteString.length malformed)
  chunkTextWithLimit 64 decoded @?= Left (ChunkTooLarge 96 64)
  map textChunkText (chunkDecodedText decoded) @?= [decoded]
  chunkText " \t\r\n  " @?= Right []

newlineNormalizationContract :: IO ()
newlineNormalizationContract = do
  normalizeNewlines "alpha\r\nbeta\rgamma" @?= "alpha\nbeta\ngamma"
  let source =
        Text.concat
          [ "abcdefghij",
            Text.singleton '\v',
            "klmnopqrst",
            Text.singleton '\f',
            "uvwxyzabcd",
            Text.singleton '\x001c',
            "efghijklmn",
            Text.singleton '\x001d',
            "opqrstuvwx",
            Text.singleton '\x001e',
            "yzabcdefgh",
            Text.singleton '\x0085',
            "ijklmnopqr",
            Text.singleton '\x2028',
            "stuvwxyzab",
            Text.singleton '\x2029',
            "cdefghijkl"
          ]
      bytes = TextEncoding.encodeUtf8 source
  normalizeNewlines source @?= source
  decodeTextBytes "separators" bytes @?= Right source
  chunkText source @?= Right [TextChunk 0 1 1 source]

embeddingContract :: IO ()
embeddingContract = do
  let evidence = "alpha\n\n beta\t gamma"
  chunkText evidence @?= Right [TextChunk 0 1 3 evidence]
  embeddingText evidence @?= "alpha beta gamma"
  sourceEmbeddingText "alpha  beta\nalpha beta\n gamma\n\ngamma" @?= "alpha beta gamma"
  meaningfulAlnumCount "a-b_C 19 \x00e5" @?= 5

informativeContract :: IO ()
informativeContract = do
  let chunks = [chunk ordinal (if ordinal == 29 then "lease token fencing epoch rollover" else "generic service module") | ordinal <- [0 .. 29]]
      selected = selectInformativeChunks chunks
      ordinals = map textChunkOrdinal selected
  length selected @?= relevanceMaxSourceQueryChunks
  ordinals @?= sort ordinals
  assertBool "rare meaningful region survives the max-24 bound" (29 `elem` ordinals)

excerptContract :: IO ()
excerptContract = do
  compactExcerpt (Text.replicate 300 "x") @?= Text.replicate 239 "x" <> "\x2026"
  focusedAdrExcerpt (Text.replicate 260 "x" <> " cache fencing token " <> Text.replicate 260 "y") "cache token"
    @?= "\x2026" <> Text.replicate 65 "x" <> " cache fencing token " <> Text.replicate 154 "y" <> "\x2026"
  let (lineStart, lineEnd, excerpt) = focusedEvidenceExcerpt "noise only\ncache lease token" "cache lease" 10
  (lineStart, lineEnd) @?= (11, 11)
  excerpt @?= "cache lease token"

aggregationContract :: IO ()
aggregationContract = do
  let matches =
        [ pair 0 "a" "cache lease" 0.50 0.20,
          pair 0 "z" "cache lease" 0.45 0.40,
          pair 1 "adjacent" "adjacent queue" 0.45 0.10,
          pair 3 "third" "transaction storage" 0.40 0.10,
          pair 5 "fifth" "observability trace" 0.30 0
        ]
      plain = expectAggregate (aggregateRelevance matches Nothing)
      scoped = expectAggregate (aggregateRelevance matches (Just RelevanceScopeExact))
  map pairAdrItemId (aggregateEvidence plain) @?= ["a", "third", "fifth"]
  assertClose 0.555 (aggregateSemanticScore plain)
  assertClose 0.21 (aggregateLexicalScore plain)
  assertClose 0.021 (aggregateLexicalBonus plain)
  assertClose 0.576 (aggregateScore plain)
  aggregateSemanticScore scoped @?= aggregateSemanticScore plain
  assertClose 0.025 (aggregateScopeBonus scoped)
  assertClose 0.601 (aggregateScore scoped)
  aggregateRelevance [pair 0 "weak" "weak" 0.074 0.179] Nothing @?= Nothing
  assertBool "lexical threshold admits a semantic miss" (isJust (aggregateRelevance [pair 0 "lexical" "identifier" 0 0.18] Nothing))
  let repetitive = Text.intercalate "\n" (replicate 10 "generic module")
      repetitiveAggregate = expectAggregate (aggregateRelevance [pair 0 "repeat" repetitive 0.8 0] Nothing)
  assertClose 0.1 (aggregateSourceInformation repetitiveAggregate)
  confidenceLabel 0.8 0.8 0.8 (aggregateSourceInformation repetitiveAggregate) @?= LowConfidence

confidenceContract :: IO ()
confidenceContract = do
  confidenceLabel 0.60 0.58 0.01 1 @?= MediumConfidence
  confidenceLabel 0.60 0.58 0.08 1 @?= HighConfidence
  confidenceLabel 0.15 0.50 0.50 1 @?= LowConfidence
  confidenceLabel 0.16 0.14 0 1 @?= MediumConfidence

sectionSelectionBoundaryContract :: IO ()
sectionSelectionBoundaryContract = do
  sectionIndexMode 50000 @?= ExactSectionScan
  sectionIndexMode 50001 @?= LshSectionScan

fingerprintContract :: IO ()
fingerprintContract = do
  relevanceChunkingFingerprintPayload
    @?= ByteStringChar8.pack
      "{\"binary_control_ratio\":0.2,\"binary_sample_bytes\":16384,\"implementation\":\"adrai-raw-text-chunks:v1\",\"max_bytes\":4194304,\"newline_radius\":220,\"overlap_chars\":300,\"target_chars\":1500}"
  assertBool "chunking fingerprint is a SHA-256 public identity" ("sha256:" `Text.isPrefixOf` relevanceChunkingFingerprint)
  relevanceChunkingFingerprint @?= "sha256:Nh2G1cvQT0C8UX8nxSoUMS7NI2caJ_rJOHgTwncHEc4"
  relevanceScoringContract
    @?= Map.fromList
      [ ("second_evidence_weight", 0.10),
        ("third_evidence_weight", 0.05),
        ("scope_bonus_exact", 0.025),
        ("scope_bonus_ambiguous", 0.010),
        ("minimum_pair_score", 0.075),
        ("minimum_lexical_pair_score", 0.18),
        ("lexical_rank_weight", 0.10),
        ("lexical_bonus_cap", 0.10),
        ("high_semantic_score", 0.34),
        ("high_pair_score", 0.30),
        ("high_margin", 0.045),
        ("medium_semantic_score", 0.16),
        ("medium_pair_score", 0.14),
        ("repetitive_chunk_low_confidence", 0.15)
      ]

chunk :: Int -> Text.Text -> TextChunk
chunk ordinal text = TextChunk ordinal (ordinal + 1) (ordinal + 1) text

pair :: Int -> Text.Text -> Text.Text -> Double -> Double -> PairMatch
pair ordinal item text semantic lexical = PairMatch (chunk ordinal text) item text semantic lexical

expectAggregate :: Maybe RelevanceAggregate -> RelevanceAggregate
expectAggregate Nothing = error "expected relevance aggregate"
expectAggregate (Just value) = value

expectDecoded :: Either TextDecodeError Text.Text -> Text.Text
expectDecoded (Left problem) = error ("expected decoded text: " <> show problem)
expectDecoded (Right value) = value

assertClose :: Double -> Double -> IO ()
assertClose expected actual =
  if abs (expected - actual) < 1.0e-12
    then pure ()
    else assertFailure ("expected " <> show expected <> ", got " <> show actual)

isRight :: Either left right -> Bool
isRight (Right _) = True
isRight _ = False

isJust :: Maybe value -> Bool
isJust (Just _) = True
isJust Nothing = False
