{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Deterministic, format-independent text chunking for relevance inputs.
module Adrai.Relevance
  ( TextChunk (..),
    ChunkError (..),
    TextDecodeError (..),
    BinaryReason (..),
    PairMatch (..),
    RelevanceScopeMatch (..),
    RelevanceAggregate (..),
    ConfidenceLabel (..),
    maxTextBytes,
    binarySampleBytes,
    binaryControlRatio,
    minChunkAlnum,
    minMeaningfulAlnum,
    targetChunkChars,
    chunkOverlapChars,
    chunkBoundaryRadius,
    evidenceExcerptChars,
    relevanceAdrShortlistMin,
    relevanceAdrShortlistMax,
    relevanceAdrShortlistMultiplier,
    relevanceScopeShortlistCap,
    relevanceMaxSourceQueryChunks,
    secondEvidenceWeight,
    thirdEvidenceWeight,
    scopeBonusExact,
    scopeBonusAmbiguous,
    minimumPairScore,
    minimumLexicalPairScore,
    lexicalRankWeight,
    lexicalBonusCap,
    highSemanticScore,
    highPairScore,
    highMargin,
    mediumSemanticScore,
    mediumPairScore,
    repetitiveChunkLowConfidence,
    normalizeNewlines,
    validateTextBytes,
    decodeTextBytesWithLimit,
    decodeTextBytes,
    embeddingText,
    sourceEmbeddingText,
    meaningfulAlnumCount,
    meaningfulAlnumByteCount,
    chunkTextWithLimit,
    chunkText,
    chunkDecodedText,
    compactExcerpt,
    focusedAdrExcerpt,
    focusedEvidenceExcerpt,
    selectInformativeChunks,
    pairRankingScore,
    scopeBonus,
    sourceInformationScore,
    aggregateRelevance,
    confidenceLabel,
    relevanceChunkingFingerprint,
    relevanceChunkingFingerprintPayload,
    relevanceScoringContract,
  )
where

import Adrai.Format (renderDigest)
import Adrai.Provenance (sha256Digest)
import Adrai.Vector (semanticTokens)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Char (isAscii)
import Data.List (findIndices, sortBy, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word8)

-- | One source-text chunk.  Ordinals are zero-based; line ranges are one-based
-- and inclusive.
data TextChunk = TextChunk
  { textChunkOrdinal :: Int,
    textChunkStartLine :: Int,
    textChunkEndLine :: Int,
    textChunkText :: Text
  }
  deriving (Eq, Show)

-- | Chunking rejects rather than truncates text above the UTF-8 size limit.
-- The constructor carries the actual and maximum byte counts, respectively.
data ChunkError
  = ChunkTooLarge Int Int
  deriving (Eq, Show)

data BinaryReason
  = BinaryContainsNul
  | BinaryTooManyControls Int Int
  deriving (Eq, Show)

data TextDecodeError
  = TextBytesTooLarge Text Int Int
  | TextAppearsBinary Text BinaryReason
  deriving (Eq, Show)

data PairMatch = PairMatch
  { pairFileChunk :: TextChunk,
    pairAdrItemId :: Text,
    pairAdrText :: Text,
    pairScore :: Double,
    pairLexicalScore :: Double
  }
  deriving (Eq, Show)

data RelevanceScopeMatch
  = RelevanceScopeExact
  | RelevanceScopeAmbiguous
  | RelevanceScopeNone
  deriving (Eq, Ord, Show)

data RelevanceAggregate = RelevanceAggregate
  { aggregateSemanticScore :: Double,
    aggregateLexicalScore :: Double,
    aggregateLexicalBonus :: Double,
    aggregateScore :: Double,
    aggregateScopeBonus :: Double,
    aggregateStrongestPair :: Double,
    aggregateStrongestLexicalPair :: Double,
    aggregateSourceInformation :: Double,
    aggregateEvidence :: [PairMatch]
  }
  deriving (Eq, Show)

data ConfidenceLabel
  = LowConfidence
  | MediumConfidence
  | HighConfidence
  deriving (Eq, Ord, Show)

-- | Maximum accepted size after newline normalization, measured as UTF-8.
maxTextBytes :: Int
maxTextBytes = 4 * 1024 * 1024

binarySampleBytes :: Int
binarySampleBytes = 16 * 1024

binaryControlRatio :: Double
binaryControlRatio = 0.20

minChunkAlnum :: Int
minChunkAlnum = 8

minMeaningfulAlnum :: Int
minMeaningfulAlnum = 24

-- | Desired chunk length in Unicode characters.
targetChunkChars :: Int
targetChunkChars = 1500

-- | Character overlap between adjacent chunks.
chunkOverlapChars :: Int
chunkOverlapChars = 300

-- | Radius around a target cut in which a newline is preferred.
chunkBoundaryRadius :: Int
chunkBoundaryRadius = 220

evidenceExcerptChars :: Int
evidenceExcerptChars = 240

relevanceAdrShortlistMin :: Int
relevanceAdrShortlistMin = 48

relevanceAdrShortlistMax :: Int
relevanceAdrShortlistMax = 160

relevanceAdrShortlistMultiplier :: Int
relevanceAdrShortlistMultiplier = 12

relevanceScopeShortlistCap :: Int
relevanceScopeShortlistCap = 24

relevanceMaxSourceQueryChunks :: Int
relevanceMaxSourceQueryChunks = 24

secondEvidenceWeight :: Double
secondEvidenceWeight = 0.10

thirdEvidenceWeight :: Double
thirdEvidenceWeight = 0.05

scopeBonusExact :: Double
scopeBonusExact = 0.025

scopeBonusAmbiguous :: Double
scopeBonusAmbiguous = 0.010

minimumPairScore :: Double
minimumPairScore = 0.075

minimumLexicalPairScore :: Double
minimumLexicalPairScore = 0.18

lexicalRankWeight :: Double
lexicalRankWeight = 0.10

lexicalBonusCap :: Double
lexicalBonusCap = 0.10

highSemanticScore :: Double
highSemanticScore = 0.34

highPairScore :: Double
highPairScore = 0.30

highMargin :: Double
highMargin = 0.045

mediumSemanticScore :: Double
mediumSemanticScore = 0.16

mediumPairScore :: Double
mediumPairScore = 0.14

repetitiveChunkLowConfidence :: Double
repetitiveChunkLowConfidence = 0.15

-- | Normalize only CRLF and bare CR to LF for raw relevance text.  Other
-- Unicode and C0 separator characters remain literal source evidence.
normalizeNewlines :: Text -> Text
normalizeNewlines = Text.replace "\r" "\n" . Text.replace "\r\n" "\n"

decodeTextBytes :: Text -> ByteString -> Either TextDecodeError Text
decodeTextBytes = decodeTextBytesWithLimit maxTextBytes

decodeTextBytesWithLimit :: Int -> Text -> ByteString -> Either TextDecodeError Text
decodeTextBytesWithLimit limit label bytes = do
  validateTextBytesWithLimit limit label bytes
  Right (normalizeNewlines (TextEncoding.decodeUtf8With lenientDecode bytes))

validateTextBytes :: Text -> ByteString -> Either TextDecodeError ()
validateTextBytes = validateTextBytesWithLimit maxTextBytes

validateTextBytesWithLimit :: Int -> Text -> ByteString -> Either TextDecodeError ()
validateTextBytesWithLimit limit label bytes
  | byteCount > limit = Left (TextBytesTooLarge label byteCount limit)
  | ByteString.elem 0 bytes = Left (TextAppearsBinary label BinaryContainsNul)
  | not (ByteString.null sample),
    fromIntegral controlCount / fromIntegral (ByteString.length sample) > binaryControlRatio =
      Left (TextAppearsBinary label (BinaryTooManyControls controlCount (ByteString.length sample)))
  | otherwise = Right ()
  where
    byteCount = ByteString.length bytes
    sample = binarySample bytes
    controlCount = ByteString.foldl' (\count value -> if isBinaryControl value then count + 1 else count) 0 sample

binarySample :: ByteString -> ByteString
binarySample bytes
  | ByteString.length bytes <= binarySampleBytes = bytes
  | otherwise = ByteString.take half bytes <> ByteString.takeEnd half bytes
  where
    half = binarySampleBytes `div` 2

isBinaryControl :: Word8 -> Bool
isBinaryControl value = value < 32 && value `notElem` [9, 10, 12, 13]

embeddingText :: Text -> Text
embeddingText = Text.unwords . Text.words

sourceEmbeddingText :: Text -> Text
sourceEmbeddingText text = Text.unwords (reverse values)
  where
    (_, values) = foldl' addLine (Set.empty, []) (Text.splitOn "\n" (normalizeNewlines text))
    addLine (seen, result) line
      | Text.null normalized || Set.member normalized seen = (seen, result)
      | otherwise = (Set.insert normalized seen, normalized : result)
      where
        normalized = embeddingText line

meaningfulAlnumCount :: Text -> Int
meaningfulAlnumCount = Text.foldl' (\count character -> if isAsciiAlnum character then count + 1 else count) 0

-- | ASCII alphanumeric bytes survive lenient UTF-8 decoding unchanged, so
-- this is the exact meaningful-character count used by the zero-content fast
-- path without constructing an otherwise unobservable decoded value.
meaningfulAlnumByteCount :: ByteString -> Int
meaningfulAlnumByteCount = ByteString.foldl' (\count value -> if isAsciiAlnumByte value then count + 1 else count) 0

isAsciiAlnumByte :: Word8 -> Bool
isAsciiAlnumByte value =
  (value >= 65 && value <= 90)
    || (value >= 97 && value <= 122)
    || (value >= 48 && value <= 57)

isAsciiAlnum :: Char -> Bool
isAsciiAlnum character =
  (character >= 'a' && character <= 'z')
    || (character >= 'A' && character <= 'Z')
    || (character >= '0' && character <= '9')

-- | Split text into deterministic, overlapping, line-aware chunks.
--
-- Whitespace-only chunks are omitted.  Content is otherwise preserved after
-- newline normalization, including leading and trailing whitespace.
chunkText :: Text -> Either ChunkError [TextChunk]
chunkText = chunkTextWithLimit maxTextBytes

chunkTextWithLimit :: Int -> Text -> Either ChunkError [TextChunk]
chunkTextWithLimit limit input
  | utf8Bytes > limit = Left (ChunkTooLarge utf8Bytes limit)
  | otherwise = Right (chunkDecodedText normalized)
  where
    normalized = normalizeNewlines input
    utf8Bytes = ByteString.length (TextEncoding.encodeUtf8 normalized)

-- | Chunk text whose authoritative raw-byte limit has already been checked.
-- Lenient UTF-8 replacement can expand accepted source bytes when re-encoded,
-- so relevance orchestration must not apply the raw limit a second time.
chunkDecodedText :: Text -> [TextChunk]
chunkDecodedText input
  | Text.null normalized || Text.null (Text.strip normalized) = []
  | otherwise = go 0 1 normalized textLength 0 []
  where
    normalized = normalizeNewlines input
    textLength = Text.length normalized

    go start startLine remaining remainingLength ordinal chunks
      | remainingLength <= 0 = reverse chunks
      | otherwise =
          let target = min remainingLength targetChunkChars
              preferred = preferredEnd remaining remainingLength target
              endLocal =
                if preferred <= 0
                  then min remainingLength targetChunkChars
                  else preferred
              end = start + endLocal
              raw = Text.take endLocal remaining
              hasContent = not (Text.null (Text.strip raw))
              trailingNewlines = Text.length (Text.takeWhileEnd (== '\n') raw)
              contentLocal = max 0 (endLocal - trailingNewlines - 1)
              endLine = startLine + Text.count "\n" (Text.take contentLocal raw)
              nextChunks =
                if hasContent
                  then
                    TextChunk
                      { textChunkOrdinal = ordinal,
                        textChunkStartLine = startLine,
                        textChunkEndLine = endLine,
                        textChunkText = raw
                      }
                      : chunks
                  else chunks
              nextOrdinal = if hasContent then ordinal + 1 else ordinal
           in if end >= textLength
                then reverse nextChunks
                else
                  let advance = max 1 (endLocal - chunkOverlapChars)
                      nextStart = start + advance
                      nextStartLine = startLine + Text.count "\n" (Text.take advance raw)
                      nextRemaining = Text.drop advance remaining
                   in go nextStart nextStartLine nextRemaining (remainingLength - advance) nextOrdinal nextChunks

preferredEnd :: Text -> Int -> Int -> Int
preferredEnd text textLength target
  | target >= textLength = textLength
  | otherwise =
      case candidates of
        [] -> target
        first : remaining -> foldl nearer first remaining
  where
    lower = max 1 (target - chunkBoundaryRadius)
    upper = min textLength (target + chunkBoundaryRadius)
    window = Text.take (upper - lower) (Text.drop lower text)
    candidates = [lower + offset + 1 | offset <- findIndices (== '\n') (Text.unpack window)]
    nearer best candidate
      | (abs (candidate - target), candidate) < (abs (best - target), best) = candidate
      | otherwise = best

compactExcerpt :: Text -> Text
compactExcerpt = compactExcerptWithLimit evidenceExcerptChars

compactExcerptWithLimit :: Int -> Text -> Text
compactExcerptWithLimit limit text
  | Text.length value <= limit = value
  | otherwise = Text.stripEnd (Text.take (max 1 (limit - 1)) value) <> "\x2026"
  where
    value = embeddingText text

focusedAdrExcerpt :: Text -> Text -> Text
focusedAdrExcerpt adrText sourceText =
  excerptAroundReference adrText (Set.fromList (semanticTokens sourceText)) evidenceExcerptChars

focusedEvidenceExcerpt :: Text -> Text -> Int -> (Int, Int, Text)
focusedEvidenceExcerpt sourceText referenceText sourceLineStart =
  case bestWindow of
    Nothing -> (sourceLineStart, sourceLineStart, compactExcerpt sourceText)
    Just (_, width, start, spanText) ->
      ( absoluteStart,
        absoluteStart + max 0 (width - 1),
        excerptAroundReference spanText referenceTokens evidenceExcerptChars
      )
      where
        absoluteStart = sourceLineStart + start
  where
    linesOfText = Text.splitOn "\n" (normalizeNewlines sourceText)
    referenceTokens = Set.fromList (semanticTokens referenceText)
    maxWindow = min 3 (length linesOfText)
    bestWindow = foldl' chooseWindow Nothing (candidateWindows linesOfText maxWindow referenceTokens)

candidateWindows :: [Text] -> Int -> Set Text -> [(Double, Int, Int, Text)]
candidateWindows linesOfText maxWindow referenceTokens =
  [ (windowScore referenceTokens spanText, actualWidth, start, spanText)
    | start <- [0 .. length linesOfText - 1],
      requestedWidth <- [1 .. maxWindow],
      let end = min (length linesOfText) (start + requestedWidth),
      let actualWidth = end - start,
      let spanText = Text.strip (Text.intercalate "\n" (take actualWidth (drop start linesOfText))),
      not (Text.null spanText)
  ]

windowScore :: Set Text -> Text -> Double
windowScore referenceTokens spanText
  | Set.null overlap = 0
  | otherwise = weighted + fromIntegral (Set.size overlap) / max 1 (sqrt (fromIntegral (Set.size candidateTokens)))
  where
    candidateTokens = Set.fromList (semanticTokens spanText)
    overlap = Set.intersection referenceTokens candidateTokens
    weighted =
      sum
        [ 1 + fromIntegral (min (Text.length token) 12) / 12
          | token <- Set.toList overlap
        ]

chooseWindow :: Maybe (Double, Int, Int, Text) -> (Double, Int, Int, Text) -> Maybe (Double, Int, Int, Text)
chooseWindow Nothing candidate = Just candidate
chooseWindow (Just previous) candidate
  | windowKey candidate > windowKey previous = Just candidate
  | otherwise = Just previous
  where
    windowKey (score, width, start, _) = (score, Down width, Down start)

excerptAroundReference :: Text -> Set Text -> Int -> Text
excerptAroundReference text referenceTokens limit
  | Text.length value <= limit = value
  | null hits = compactExcerptWithLimit limit value
  | otherwise = prefix <> Text.strip excerpt <> suffix
  where
    value = embeddingText text
    hits =
      [ offset
        | (offset, token) <- referenceRuns value,
          not (Set.disjoint referenceTokens (Set.fromList (semanticTokens token)))
      ]
    center = hits !! (length hits `div` 2)
    initialStart = max 0 (center - limit `div` 3)
    end = min (Text.length value) (initialStart + limit)
    start = max 0 (end - limit)
    excerpt = Text.take (end - start) (Text.drop start value)
    prefix = if start > 0 then "\x2026" else ""
    suffix = if end < Text.length value then "\x2026" else ""

referenceRuns :: Text -> [(Int, Text)]
referenceRuns = go 0 . Text.unpack
  where
    go _ [] = []
    go offset characters =
      case dropRunSeparators offset characters of
        (_, []) -> []
        (start, remaining) ->
          let (token, rest) = span isReferenceCharacter remaining
           in (start, Text.pack token) : go (start + length token) rest

dropRunSeparators :: Int -> String -> (Int, String)
dropRunSeparators offset [] = (offset, [])
dropRunSeparators offset values@(character : remaining)
  | isReferenceCharacter character = (offset, values)
  | otherwise = dropRunSeparators (offset + 1) remaining

isReferenceCharacter :: Char -> Bool
isReferenceCharacter character =
  isAscii character
    && (isAsciiAlnum character || character `elem` ("_./:-" :: String))

selectInformativeChunks :: [TextChunk] -> [TextChunk]
selectInformativeChunks = selectInformativeChunksWithLimit relevanceMaxSourceQueryChunks

selectInformativeChunksWithLimit :: Int -> [TextChunk] -> [TextChunk]
selectInformativeChunksWithLimit limit chunks
  | length chunks <= limit = chunks
  | limit <= 0 = []
  | otherwise = sortOn textChunkOrdinal (map third (take limit (sortBy informativeOrder scored)))
  where
    tokenSets = map (semanticTokenSet . textChunkText) chunks
    frequencies = Map.fromListWith (+) [(token, 1 :: Int) | tokens <- tokenSets, token <- Set.toList tokens]
    total = length chunks
    scored = zipWith (scoreChunk total frequencies) chunks tokenSets
    third (_, _, value) = value
    informativeOrder = comparing (Down . first) <> comparing (Down . second)
    first (value, _, _) = value
    second (_, value, _) = value

scoreChunk :: Int -> Map Text Int -> TextChunk -> Set Text -> (Double, Int, TextChunk)
scoreChunk total frequencies chunk tokens =
  (rarity * max 0.15 information + density, negate (textChunkOrdinal chunk), chunk)
  where
    rarity =
      sum
        [ log ((fromIntegral total + 1) / (fromIntegral (Map.findWithDefault 0 token frequencies) + 1))
          | token <- Set.toList tokens
        ]
    information = sourceInformationScore (textChunkText chunk)
    density = min 4 (sqrt (fromIntegral (Set.size tokens)) / 3)

pairRankingScore :: PairMatch -> Double
pairRankingScore match = max 0 (pairScore match) + max 0 (pairLexicalScore match) * lexicalRankWeight

scopeBonus :: Maybe RelevanceScopeMatch -> Double
scopeBonus (Just RelevanceScopeExact) = scopeBonusExact
scopeBonus (Just RelevanceScopeAmbiguous) = scopeBonusAmbiguous
scopeBonus _ = 0

sourceInformationScore :: Text -> Double
sourceInformationScore text
  | length normalizedLines <= 3 = 1
  | otherwise = min 1 (fromIntegral (Set.size (Set.fromList normalizedLines)) / fromIntegral (length normalizedLines))
  where
    normalizedLines = filter (not . Text.null) (map embeddingText (Text.splitOn "\n" (normalizeNewlines text)))

aggregateRelevance :: [PairMatch] -> Maybe RelevanceScopeMatch -> Maybe RelevanceAggregate
aggregateRelevance matches scopeMatch =
  case ordered of
    [] -> Nothing
    strongest : _
      | pairScore strongest < minimumPairScore && pairLexicalScore strongest < minimumLexicalPairScore -> Nothing
      | otherwise -> buildAggregate (selectIndependent ordered)
  where
    bestBySource = foldl' insertBest Map.empty matches
    ordered = sortBy pairOrder (Map.elems bestBySource)
    buildAggregate [] = Nothing
    buildAggregate selected@(strongestSelected : _) =
      Just
        RelevanceAggregate
          { aggregateSemanticScore = semanticScore,
            aggregateLexicalScore = lexicalScore,
            aggregateLexicalBonus = lexicalBonus,
            aggregateScore = min 1 (semanticScore + lexicalBonus + bonus),
            aggregateScopeBonus = bonus,
            aggregateStrongestPair = pairScore strongestSelected * informationFactor,
            aggregateStrongestLexicalPair = pairLexicalScore strongestSelected * informationFactor,
            aggregateSourceInformation = information,
            aggregateEvidence = selected
          }
      where
        weights = [1, secondEvidenceWeight, thirdEvidenceWeight]
        rawSemantic = min 1 (sum (zipWith (\weight match -> weight * max 0 (pairScore match)) weights selected))
        rawLexical = min 1 (sum (zipWith (\weight match -> weight * max 0 (pairLexicalScore match)) weights selected))
        information = sourceInformationScore (textChunkText (pairFileChunk strongestSelected))
        informationFactor
          | information >= repetitiveChunkLowConfidence = 1
          | otherwise = 0.70 + 0.30 * sqrt (max 0 information)
        semanticScore = rawSemantic * informationFactor
        lexicalScore = rawLexical * informationFactor
        bonus = scopeBonus scopeMatch
        lexicalBonus = min lexicalBonusCap (lexicalScore * lexicalRankWeight)

insertBest :: Map Int PairMatch -> PairMatch -> Map Int PairMatch
insertBest values match = Map.insertWith choose (textChunkOrdinal (pairFileChunk match)) match values
  where
    choose new previous
      | sourceBestKey new > sourceBestKey previous = new
      | otherwise = previous
    sourceBestKey value = (pairRankingScore value, pairScore value, pairAdrItemId value)

pairOrder :: PairMatch -> PairMatch -> Ordering
pairOrder =
  comparing (Down . pairRankingScore)
    <> comparing (Down . pairScore)
    <> comparing (textChunkOrdinal . pairFileChunk)
    <> comparing (Down . pairAdrItemId)

selectIndependent :: [PairMatch] -> [PairMatch]
selectIndependent = go []
  where
    go selected _ | length selected == 3 = selected
    go selected [] = selected
    go selected (candidate : remaining)
      | null selected || independent selected candidate = go (selected <> [candidate]) remaining
      | otherwise = go selected remaining

independent :: [PairMatch] -> PairMatch -> Bool
independent selected candidate = all independentOf selected
  where
    candidateChunk = pairFileChunk candidate
    candidateTokens = semanticTokenSet (textChunkText candidateChunk)
    independentOf previous
      | abs (textChunkOrdinal candidateChunk - textChunkOrdinal previousChunk) <= 1 = False
      | Set.null candidateTokens || Set.null previousTokens = True
      | otherwise = overlapRatio < 0.85
      where
        previousChunk = pairFileChunk previous
        previousTokens = semanticTokenSet (textChunkText previousChunk)
        overlapRatio :: Double
        overlapRatio =
          fromIntegral (Set.size (Set.intersection candidateTokens previousTokens))
            / fromIntegral (min (Set.size candidateTokens) (Set.size previousTokens))

semanticTokenSet :: Text -> Set Text
semanticTokenSet = Set.fromList . asciiAlnumRuns . Text.toLower . sourceEmbeddingText

asciiAlnumRuns :: Text -> [Text]
asciiAlnumRuns input = reverse (finish current reversed)
  where
    (current, reversed) = Text.foldl' step ([], []) input
    step (characters, tokens) character
      | isAsciiAlnum character = (character : characters, tokens)
      | otherwise = ([], finish characters tokens)
    finish [] tokens = tokens
    finish characters tokens = Text.pack (reverse characters) : tokens

confidenceLabel :: Double -> Double -> Double -> Double -> ConfidenceLabel
confidenceLabel semanticScore strongestPair margin sourceInformation
  | sourceInformation < repetitiveChunkLowConfidence = LowConfidence
  | semanticScore >= highSemanticScore,
    strongestPair >= highPairScore,
    margin >= highMargin = HighConfidence
  | semanticScore >= mediumSemanticScore,
    strongestPair >= mediumPairScore = MediumConfidence
  | otherwise = LowConfidence

relevanceChunkingFingerprint :: Text
relevanceChunkingFingerprint = renderDigest (sha256Digest relevanceChunkingFingerprintPayload)

relevanceChunkingFingerprintPayload :: ByteString
relevanceChunkingFingerprintPayload =
  TextEncoding.encodeUtf8
    ( "{\"binary_control_ratio\":"
        <> decimalDouble binaryControlRatio
        <> ",\"binary_sample_bytes\":"
        <> decimal binarySampleBytes
        <> ",\"implementation\":\"adrai-raw-text-chunks:v1\""
        <> ",\"max_bytes\":"
        <> decimal maxTextBytes
        <> ",\"newline_radius\":"
        <> decimal chunkBoundaryRadius
        <> ",\"overlap_chars\":"
        <> decimal chunkOverlapChars
        <> ",\"target_chars\":"
        <> decimal targetChunkChars
        <> "}"
    )
  where
    decimal = Text.pack . show
    decimalDouble = Text.pack . show

relevanceScoringContract :: Map Text Double
relevanceScoringContract =
  Map.fromList
    [ ("second_evidence_weight", secondEvidenceWeight),
      ("third_evidence_weight", thirdEvidenceWeight),
      ("scope_bonus_exact", scopeBonusExact),
      ("scope_bonus_ambiguous", scopeBonusAmbiguous),
      ("minimum_pair_score", minimumPairScore),
      ("minimum_lexical_pair_score", minimumLexicalPairScore),
      ("lexical_rank_weight", lexicalRankWeight),
      ("lexical_bonus_cap", lexicalBonusCap),
      ("high_semantic_score", highSemanticScore),
      ("high_pair_score", highPairScore),
      ("high_margin", highMargin),
      ("medium_semantic_score", mediumSemanticScore),
      ("medium_pair_score", mediumPairScore),
      ("repetitive_chunk_low_confidence", repetitiveChunkLowConfidence)
    ]
