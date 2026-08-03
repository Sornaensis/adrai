{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Adrai.Vector
  ( VectorSpace (..),
    vectorSpaceName,
    semanticVectorDimensions,
    identifierVectorDimensions,
    vectorDimensions,
    vectorIndexAlgorithm,
    vectorIndexBands,
    vectorIndexBits,
    vectorIndexProbes,
    vectorIndexMinCandidates,
    vectorExactSectionThreshold,
    semanticSeedA,
    semanticSeedB,
    identifierSeedA,
    identifierSeedB,
    WeightedFeature (..),
    DenseVector,
    denseVector,
    denseValues,
    denseDimension,
    zeroVector,
    VectorError (..),
    Embedder,
    mkEmbedder,
    embedderSpace,
    embedderDimensions,
    embedderFingerprint,
    embedderVectorId,
    embedderDescription,
    semanticEmbedder,
    identifierEmbedder,
    defaultEmbedder,
    implementationFingerprint,
    aliases,
    commonTermWeights,
    conceptClusters,
    phraseAliases,
    stem,
    semanticTokens,
    splitIdentifier,
    identifierTerms,
    semanticFeatures,
    identifierFeatures,
    embed,
    semanticEmbedding,
    identifierEmbedding,
    architectureEmbedding,
    blake2b128Digest,
    slotFromDigest,
    featureSlot,
    packVector,
    unpackVector,
    dot,
    SectionIndexMode (..),
    sectionIndexMode,
    LshBucket (..),
    LshPlan,
    lshPlanDimensions,
    lshPlanSeed,
    lshPlanPlanes,
    buildLshPlan,
    lshSignatures,
    lshProbeBuckets,
    LshIndex,
    buildLshIndex,
    buildLshIndexFromSignatures,
    lshIndexItemCount,
    lshIndexMembershipCount,
    CandidateFallback (..),
    CandidateDiagnostics (..),
    selectCandidates,
    rerankByDot,
  )
where

import Adrai.Format (renderDigest)
import Adrai.Provenance (sha256Digest)
import Control.Monad (foldM, forM_)
import Control.Monad.ST (runST)
import qualified Crypto.Hash as Crypto
import Crypto.Hash.Algorithms (Blake2b)
import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Char (isAlpha, isAscii, isDigit, isLower, isUpper, ord)
import Data.Foldable (toList, traverse_)
import GHC.Float (castFloatToWord32, castWord32ToFloat)
import Data.List (sortBy, sortOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Sequence as Sequence
import Data.Sequence (Seq, (|>))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word32, Word64, Word8)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Vector.Unboxed.Mutable as Mutable

data VectorSpace
  = SemanticSpace
  | IdentifierSpace
  deriving (Eq, Ord, Show)

vectorSpaceName :: VectorSpace -> Text
vectorSpaceName SemanticSpace = "semantic"
vectorSpaceName IdentifierSpace = "identifier"

semanticVectorDimensions, identifierVectorDimensions, vectorDimensions :: Int
semanticVectorDimensions = 1024
identifierVectorDimensions = 768
vectorDimensions = semanticVectorDimensions

vectorIndexAlgorithm :: Text
vectorIndexAlgorithm = "exact-summary+lsh-sections"

vectorIndexBands, vectorIndexBits, vectorIndexProbes, vectorIndexMinCandidates, vectorExactSectionThreshold :: Int
vectorIndexBands = 12
vectorIndexBits = 8
vectorIndexProbes = 1
vectorIndexMinCandidates = 64
vectorExactSectionThreshold = 50000

semanticSeedA, semanticSeedB, identifierSeedA, identifierSeedB :: Text
semanticSeedA = "adrai-semantic-a"
semanticSeedB = "adrai-semantic-b"
identifierSeedA = "adrai-identifier-a"
identifierSeedB = "adrai-identifier-b"

data WeightedFeature = WeightedFeature
  { weightedFeatureName :: Text,
    weightedFeatureWeight :: Double
  }
  deriving (Eq, Show)

newtype DenseVector = DenseVector (Unboxed.Vector Double)
  deriving (Eq, Show)

denseVector :: [Double] -> DenseVector
denseVector = DenseVector . Unboxed.fromList

denseValues :: DenseVector -> [Double]
denseValues (DenseVector values) = Unboxed.toList values

denseDimension :: DenseVector -> Int
denseDimension (DenseVector values) = Unboxed.length values

zeroVector :: Int -> DenseVector
zeroVector dimensions = DenseVector (Unboxed.replicate (max 0 dimensions) 0)

data VectorError
  = InvalidEmbedderDimensions Int
  | InvalidFeatureDigestLength Int
  | InvalidHalfDimensions Int
  | InvalidVectorBlobLength Int
  | VectorDimensionMismatch Int Int
  | InvalidLshDimensions Int
  | InvalidLshSignatureCount Int
  | LshPlanDimensionMismatch Int Int
  | DuplicateLshItemId Text
  | MissingLshItemId Text
  deriving (Eq, Show)

data Embedder = Embedder
  { embedderSpace :: VectorSpace,
    embedderDimensions :: Int,
    embedderFingerprint :: Text
  }
  deriving (Eq, Show)

mkEmbedder :: VectorSpace -> Int -> Either VectorError Embedder
mkEmbedder space dimensions
  | dimensions < 8 || odd dimensions = Left (InvalidEmbedderDimensions dimensions)
  | otherwise = Right (Embedder space dimensions (spaceFingerprint space dimensions))

semanticEmbedder :: Embedder
semanticEmbedder = checkedEmbedder SemanticSpace semanticVectorDimensions

identifierEmbedder :: Embedder
identifierEmbedder = checkedEmbedder IdentifierSpace identifierVectorDimensions

defaultEmbedder :: Embedder
defaultEmbedder = semanticEmbedder

checkedEmbedder :: VectorSpace -> Int -> Embedder
checkedEmbedder space dimensions =
  case mkEmbedder space dimensions of
    Right value -> value
    Left failure -> error (show failure)

embedderVectorId :: Embedder -> Text
embedderVectorId value =
  "adrai-"
    <> vectorSpaceName (embedderSpace value)
    <> ":"
    <> Text.take 24 (Text.drop 7 (embedderFingerprint value))
    <> ":"
    <> Text.pack (show (embedderDimensions value))

embedderDescription :: Embedder -> [(Text, Text)]
embedderDescription value =
  [ ("implementation", "adrai-vector"),
    ("space", vectorSpaceName (embedderSpace value)),
    ("dimensions", Text.pack (show (embedderDimensions value))),
    ("fingerprint", embedderFingerprint value),
    ("vector_id", embedderVectorId value),
    ("index", vectorIndexAlgorithm)
  ]

aliases :: [(Text, Text)]
aliases =
  [ ("caching", "cache"),
    ("cached", "cache"),
    ("caches", "cache"),
    ("memoized", "memoize"),
    ("memoised", "memoize"),
    ("memoizing", "memoize"),
    ("fingerprints", "fingerprint"),
    ("digests", "digest"),
    ("hashes", "hash"),
    ("identities", "identity"),
    ("identifiers", "identity"),
    ("identifier", "identity"),
    ("keys", "key"),
    ("paths", "path"),
    ("directories", "directory"),
    ("files", "file"),
    ("obsolete", "retire"),
    ("obsoleted", "retire"),
    ("deprecated", "retire"),
    ("amended", "amend"),
    ("amendments", "amend"),
    ("workers", "worker"),
    ("queues", "queue"),
    ("healthcheck", "health"),
    ("healthchecks", "health"),
    ("configs", "config"),
    ("settings", "setting"),
    ("variables", "variable"),
    ("persisted", "persist"),
    ("persistence", "persist"),
    ("retries", "retry"),
    ("retried", "retry"),
    ("timeouts", "timeout"),
    ("failures", "failure"),
    ("messages", "message"),
    ("events", "event")
  ]

commonTermWeights :: [(Text, Double)]
commonTermWeights =
  [ ("system", 0.18),
    ("service", 0.30),
    ("component", 0.30),
    ("decision", 0.12),
    ("architecture", 0.20),
    ("support", 0.25),
    ("data", 0.22),
    ("implementation", 0.18),
    ("application", 0.25),
    ("use", 0.20),
    ("using", 0.20),
    ("provide", 0.25),
    ("ensure", 0.24),
    ("allow", 0.25),
    ("must", 0.35),
    ("should", 0.30),
    ("current", 0.20),
    ("change", 0.30),
    ("record", 0.32),
    ("value", 0.28),
    ("process", 0.28)
  ]

conceptClusters :: [(Text, [Text])]
conceptClusters =
  [ ("cache", ["cache", "caching", "memoization", "memoize", "reuse", "artifact"]),
    ("identity", ["identity", "identifier", "key", "fingerprint", "digest", "hash"]),
    ("storage", ["database", "storage", "persist", "repository", "sqlite"]),
    ("transaction", ["transaction", "atomic", "commit", "rollback", "consistency", "durable"]),
    ("concurrency", ["concurrency", "parallel", "thread", "lock", "race", "synchronization"]),
    ("queue", ["queue", "job", "worker", "task", "delivery", "acknowledge"]),
    ("messaging", ["event", "message", "stream", "pubsub", "publish", "subscribe", "broker"]),
    ("api", ["api", "endpoint", "interface", "contract", "protocol", "schema"]),
    ("network", ["network", "http", "grpc", "socket", "transport", "remote"]),
    ("security", ["security", "authentication", "authorization", "credential", "secret"]),
    ("encryption", ["encryption", "encrypt", "cipher", "tls", "certificate", "keyring"]),
    ("observability", ["observability", "logging", "metrics", "tracing", "telemetry", "monitoring"]),
    ("configuration", ["configuration", "config", "setting", "environment", "env", "variable", "default"]),
    ("health", ["health", "liveness", "readiness", "probe"]),
    ("deployment", ["deployment", "release", "rollout", "container", "kubernetes", "runtime"]),
    ("compatibility", ["compatibility", "migration", "versioning", "backward", "upgrade", "legacy"]),
    ("failure", ["failure", "error", "retry", "timeout", "fallback", "recovery", "resilience"]),
    ("ownership", ["ownership", "authority", "maintainer", "team", "domain", "responsibility"]),
    ("scope", ["scope", "applies", "path", "file", "directory", "module", "component"]),
    ("lifecycle", ["obsolete", "supersede", "replace", "amend", "deprecate", "retire"]),
    ("performance", ["performance", "latency", "throughput", "memory", "cpu", "benchmark"]),
    ("testing", ["test", "testing", "verification", "fixture", "property", "integration"])
  ]

phraseAliases :: [([Text], [Text])]
phraseAliases =
  [ (["busy", "timeout"], ["lock", "waiting"]),
    (["lock", "waiting"], ["busy", "timeout"]),
    (["correlation", "id"], ["request", "id"]),
    (["request", "id"], ["correlation", "id"]),
    (["content", "hash"], ["fingerprint"]),
    (["content", "digest"], ["fingerprint"]),
    (["idempotent"], ["deduplicate"]),
    (["deduplicate"], ["idempotent"]),
    (["write", "ahead", "log"], ["wal"]),
    (["wal"], ["write", "ahead", "log"])
  ]

aliasMap :: Map Text Text
aliasMap = Map.fromList aliases

commonWeightMap :: Map Text Double
commonWeightMap = Map.fromList commonTermWeights

conceptByToken :: Map Text Text
conceptByToken = Map.fromList [(member, concept) | (concept, members) <- conceptClusters, member <- members]

stem :: Text -> Text
stem original =
  case Map.lookup lowered aliasMap of
    Just value -> value
    Nothing -> Map.findWithDefault stripped stripped aliasMap
  where
    lowered = Text.toLower original
    stripped = stripFirstSuffix lowered ["ing", "ed", "es", "s"]

stripFirstSuffix :: Text -> [Text] -> Text
stripFirstSuffix token [] = token
stripFirstSuffix token (suffix : rest)
  | Text.length token > Text.length suffix + 3 && suffix `Text.isSuffixOf` token = Text.dropEnd (Text.length suffix) token
  | otherwise = stripFirstSuffix token rest

semanticTokens :: Text -> [Text]
semanticTokens = map stem . asciiTokenRuns . Text.toLower

asciiTokenRuns :: Text -> [Text]
asciiTokenRuns input =
  reverse (finish current reversed)
  where
    (current, reversed) = Text.foldl' step ([], []) input
    step (chars, tokens) character
      | isSemanticTokenCharacter character = (character : chars, tokens)
      | otherwise = ([], finish chars tokens)
    finish [] tokens = tokens
    finish chars tokens = Text.pack (reverse chars) : tokens

isSemanticTokenCharacter :: Char -> Bool
isSemanticTokenCharacter character =
  isAscii character
    && (isAlpha character || isDigit character || character `elem` ("_./:-" :: String))

splitIdentifier :: Text -> [Text]
splitIdentifier raw = uniqueOrdered (filter ((>= 2) . Text.length) normalized)
  where
    expanded = insertIdentifierBoundaries (Text.unpack raw)
    normalized =
      map (Text.toLower . Text.strip . Text.pack)
        (splitOnIdentifierSeparators expanded)

insertIdentifierBoundaries :: String -> String
insertIdentifierBoundaries = insertLetterDigit . insertCamelOne . insertCamelTwo
  where
    insertCamelTwo = insertBoundary (\left current right -> isUpper left && isUpper current && maybe False isLower right)
    insertCamelOne = insertBoundary (\left current _ -> (isLower left || isDigit left) && isUpper current)
    insertLetterDigit = insertBoundary (\left current _ -> (isAsciiLetter left && isDigit current) || (isDigit left && isAsciiLetter current))
    isAsciiLetter value = isAscii value && isAlpha value

insertBoundary :: (Char -> Char -> Maybe Char -> Bool) -> String -> String
insertBoundary predicate values = go Nothing values
  where
    go _ [] = []
    go previous (current : rest) =
      let separator = case previous of
            Just left | predicate left current (safeHead rest) -> "_"
            _ -> ""
       in separator <> [current] <> go (Just current) rest
    safeHead [] = Nothing
    safeHead (value : _) = Just value

splitOnIdentifierSeparators :: String -> [String]
splitOnIdentifierSeparators = foldr step [[]]
  where
    step character (current : rest)
      | character `elem` ("_./:-" :: String) = [] : current : rest
      | otherwise = (character : current) : rest
    step _ [] = []

identifierTerms :: Bool -> Text -> [Text]
identifierTerms includeTrigrams text = toList orderedValues
  where
    (_, orderedValues) = foldl' addRaw (Set.empty, Sequence.empty) (identifierTokenRuns text)
    addRaw state raw = foldl' add state values
      where
        compact = Text.toLower (Text.filter isAsciiAlphaNumeric raw)
        parts = splitIdentifier raw
        pairs = zipWith (\left right -> left <> "_" <> right) parts (drop 1 parts)
        trigrams
          | includeTrigrams && identifierLike raw && Text.length compact >= 5 && Text.length compact <= 64 =
              ["tri" <> Text.take 3 (Text.drop index compact) | index <- [0 .. Text.length compact - 3]]
          | otherwise = []
        values = Text.toLower raw : compact : parts <> pairs <> trigrams
    add (seen, ordered) value
      | Text.length normalized < 2 || Set.member normalized seen = (seen, ordered)
      | otherwise = (Set.insert normalized seen, ordered |> normalized)
      where
        normalized = Text.toLower (Text.strip value)

identifierTokenRuns :: Text -> [Text]
identifierTokenRuns input = scan (Text.unpack input)
  where
    scan [] = []
    scan (first : rest)
      | isAsciiLetter first =
          let (tailValue, remaining) = spanAtMost 127 isIdentifierTail rest
              token = first : tailValue
           in if length token >= 2 then Text.pack token : scan remaining else scan rest
      | otherwise = scan rest
    isAsciiLetter value = isAscii value && isAlpha value
    isIdentifierTail value = isAscii value && (isAlpha value || isDigit value || value `elem` ("_./:-" :: String))
    spanAtMost :: Int -> (Char -> Bool) -> String -> (String, String)
    spanAtMost 0 _ values = ([], values)
    spanAtMost _ _ [] = ([], [])
    spanAtMost remaining predicate (value : values)
      | predicate value =
          let (matched, rest) = spanAtMost (remaining - 1) predicate values
           in (value : matched, rest)
      | otherwise = ([], value : values)

isAsciiAlphaNumeric :: Char -> Bool
isAsciiAlphaNumeric value = isAscii value && (isAlpha value || isDigit value)

identifierLike :: Text -> Bool
identifierLike raw =
  Text.any (`elem` ("_./:-" :: String)) raw
    || hasCamelOne values
    || hasCamelTwo values
    || Text.any isDigit raw
    || (Text.length raw >= 2 && Text.toUpper raw == raw && Text.any isAlpha raw)
  where
    values = Text.unpack raw
    hasCamelOne (left : right : rest) = ((isLower left || isDigit left) && isUpper right) || hasCamelOne (right : rest)
    hasCamelOne _ = False
    hasCamelTwo (left : middle : right : rest) = (isUpper left && isUpper middle && isLower right) || hasCamelTwo (middle : right : rest)
    hasCamelTwo _ = False

uniqueOrdered :: (Ord value) => [value] -> [value]
uniqueOrdered inputValues = toList orderedValues
  where
    (_, orderedValues) = foldl' step (Set.empty, Sequence.empty) inputValues
    step (seen, ordered) value
      | Set.member value seen = (seen, ordered)
      | otherwise = (Set.insert value seen, ordered |> value)

data OrderedCounter key = OrderedCounter !(Map key Int) !(Seq key)

countOrdered :: (Ord key) => [key] -> [(key, Int)]
countOrdered values =
  let OrderedCounter counts order = foldl' add (OrderedCounter Map.empty Sequence.empty) values
   in [(key, Map.findWithDefault 0 key counts) | key <- toList order]
  where
    add (OrderedCounter counts order) key =
      case Map.lookup key counts of
        Nothing -> OrderedCounter (Map.insert key 1 counts) (order |> key)
        Just count -> OrderedCounter (Map.insert key (count + 1) counts) order

saturated :: Int -> Double
saturated termFrequency = min 2.6 (1.0 + log (fromIntegral (max 1 termFrequency)))

semanticFeatures :: Text -> [WeightedFeature]
semanticFeatures text = termFeatures <> bigramFeatures <> conceptBigramFeatures <> aliasFeatures <> identifierPieceFeatures
  where
    tokens = semanticTokens text
    termCounts = countOrdered tokens
    (termFeatureSequence, conceptSequence) = foldl' addTerm (Sequence.empty, Sequence.empty) termCounts
    termFeatures = toList termFeatureSequence
    concepts = toList conceptSequence
    addTerm (features, conceptValues) (token, count) =
      let common = Map.findWithDefault 1 token commonWeightMap
          termFeature = WeightedFeature ("term:" <> token) (saturated count * common)
       in case Map.lookup token conceptByToken of
            Nothing -> (features |> termFeature, conceptValues)
            Just concept ->
              ( features |> termFeature |> WeightedFeature ("concept:" <> concept) (saturated count * 1.45),
                foldl' (|>) conceptValues (replicate (min count 3) concept)
              )
    bigramFeatures =
      [ WeightedFeature
          ("bigram:" <> left <> ":" <> right)
          ( saturated count
              * 1.05
              * min (Map.findWithDefault 1 left commonWeightMap) (Map.findWithDefault 1 right commonWeightMap)
          )
        | ((left, right), count) <- countOrdered (zip tokens (drop 1 tokens))
      ]
    conceptBigramFeatures =
      [ WeightedFeature ("concept-bigram:" <> left <> ":" <> right) (saturated count * 1.20)
        | ((left, right), count) <- countOrdered (zip concepts (drop 1 concepts))
      ]
    aliasFeatures =
      [ WeightedFeature ("alias:" <> Text.intercalate ":" expansion) (saturated occurrences * 0.48)
        | (phrase, expansion) <- phraseAliases,
          let occurrences = phraseOccurrences phrase tokens,
          occurrences > 0
      ]
    identifierPieceFeatures =
      [ WeightedFeature ("identifier-piece:" <> stem identifier) 0.24
        | identifier <- identifierTerms False text,
          not ("tri" `Text.isPrefixOf` identifier)
      ]

phraseOccurrences :: [Text] -> [Text] -> Int
phraseOccurrences phrase values
  | null phrase = 0
  | otherwise = length [() | suffix <- take possible (tails values), take width suffix == phrase]
  where
    width = length phrase
    possible = max 0 (length values - width + 1)
    tails [] = [[]]
    tails rest@(_ : remaining) = rest : tails remaining

identifierFeatures :: Text -> [WeightedFeature]
identifierFeatures text =
  [WeightedFeature feature (saturated count * baseWeight) | ((feature, baseWeight), count) <- countOrdered occurrences]
  where
    occurrences = concatMap expand (identifierTokenRuns text)
    expand raw
      | Text.null compact = []
      | identifierLike raw =
          [("whole:" <> compact, 1.30)]
            <> [("part:" <> part, 0.95) | part <- parts]
            <> [("pair:" <> left <> ":" <> right, 1.15) | (left, right) <- zip parts (drop 1 parts)]
            <> [ ("trigram:" <> Text.take 3 (Text.drop index compact), 0.16)
                 | Text.length compact >= 5,
                   Text.length compact <= 64,
                   index <- [0 .. Text.length compact - 3]
               ]
      | Text.length compact >= 4 = [("word:" <> compact, 0.12)]
      | otherwise = []
      where
        compact = Text.toLower (Text.filter isAsciiAlphaNumeric raw)
        parts = splitIdentifier raw

embed :: Embedder -> Text -> DenseVector
embed value =
  dualSketch
    (embedderDimensions value)
    seedA
    seedB
    . featureFunction
  where
    (seedA, seedB, featureFunction) = case embedderSpace value of
      SemanticSpace -> (semanticSeedA, semanticSeedB, semanticFeatures)
      IdentifierSpace -> (identifierSeedA, identifierSeedB, identifierFeatures)

semanticEmbedding :: Text -> DenseVector
semanticEmbedding = embed semanticEmbedder

identifierEmbedding :: Text -> DenseVector
identifierEmbedding = embed identifierEmbedder

architectureEmbedding :: Text -> DenseVector
architectureEmbedding = semanticEmbedding

dualSketch :: Int -> Text -> Text -> [WeightedFeature] -> DenseVector
dualSketch dimensions seedA seedB features = DenseVector $ runST $ do
  let half = dimensions `div` 2
  left <- Mutable.replicate half 0
  right <- Mutable.replicate half 0
  forM_ features $ \(WeightedFeature feature weight) -> do
    let (leftIndex, leftSign) = featureSlotForValidHalf seedA feature half
        (rightIndex, rightSign) = featureSlotForValidHalf seedB feature half
    Mutable.modify left (+ (leftSign * weight)) leftIndex
    Mutable.modify right (+ (rightSign * weight)) rightIndex
  frozenLeft <- Unboxed.freeze left
  frozenRight <- Unboxed.freeze right
  pure (normalizeHalf frozenLeft Unboxed.++ normalizeHalf frozenRight)

normalizeHalf :: Unboxed.Vector Double -> Unboxed.Vector Double
normalizeHalf values
  | norm == 0 = values
  | otherwise = Unboxed.map (* (sqrt 0.5 / norm)) values
  where
    norm = sqrt (Unboxed.foldl' (\total value -> total + value * value) 0 values)

blake2b128Digest :: ByteString -> ByteString
blake2b128Digest bytes = convert (Crypto.hash bytes :: Crypto.Digest (Blake2b 128))

featureSlot :: Text -> Text -> Int -> Either VectorError (Int, Double)
featureSlot seed feature halfDimensions = slotFromDigest digest halfDimensions
  where
    digest = blake2b128Digest (TextEncoding.encodeUtf8 (seed <> "\NUL" <> feature))

featureSlotForValidHalf :: Text -> Text -> Int -> (Int, Double)
featureSlotForValidHalf seed feature halfDimensions =
  slotFromValidDigest
    (blake2b128Digest (TextEncoding.encodeUtf8 (seed <> "\NUL" <> feature)))
    halfDimensions

slotFromDigest :: ByteString -> Int -> Either VectorError (Int, Double)
slotFromDigest digest halfDimensions
  | BS.length digest < 9 = Left (InvalidFeatureDigestLength (BS.length digest))
  | halfDimensions <= 0 = Left (InvalidHalfDimensions halfDimensions)
  | otherwise = Right (slotFromValidDigest digest halfDimensions)

slotFromValidDigest :: ByteString -> Int -> (Int, Double)
slotFromValidDigest digest halfDimensions =
  (fromIntegral (littleEndianWord64 digest `mod` fromIntegral halfDimensions), sign)
  where
    sign = if BS.index digest 8 .&. 1 == 1 then 1 else -1

littleEndianWord64 :: ByteString -> Word64
littleEndianWord64 bytes =
  foldl' (\value (index, byte) -> value .|. (fromIntegral byte `shiftL` (index * 8))) 0 (zip [0 .. 7] (BS.unpack (BS.take 8 bytes)))

spaceFingerprint :: VectorSpace -> Int -> Text
spaceFingerprint space dimensions = renderDigest (sha256Digest (renderCompactJson payload))
  where
    payload =
      JObject
        [ ("implementation", JString "adrai-vector:dual-sketch+tf-saturation+separate-spaces"),
          ("space", JString (vectorSpaceName space)),
          ("dimensions", JInteger (toInteger dimensions)),
          ("aliases", JObject [(key, JString value) | (key, value) <- aliases]),
          ("common", JObject [(key, JNumber value) | (key, value) <- commonTermWeights]),
          ("clusters", JObject [(key, JArray (map JString values)) | (key, values) <- conceptClusters]),
          ( "phrase_aliases",
            JObject [(Text.unwords key, JArray (map JString value)) | (key, value) <- phraseAliases]
          )
        ]

implementationFingerprint :: Text
implementationFingerprint = renderDigest (sha256Digest (renderCompactJson payload))
  where
    payload =
      JObject
        [ ("semantic", JString (embedderFingerprint semanticEmbedder)),
          ("identifier", JString (embedderFingerprint identifierEmbedder)),
          ("index", JString vectorIndexAlgorithm)
        ]

data CompactJson
  = JObject [(Text, CompactJson)]
  | JArray [CompactJson]
  | JString Text
  | JInteger Integer
  | JNumber Double

renderCompactJson :: CompactJson -> ByteString
renderCompactJson = LazyByteString.toStrict . Builder.toLazyByteString . render
  where
    render (JObject fields) =
      Builder.char8 '{'
        <> commaSeparated [renderString key <> Builder.char8 ':' <> render value | (key, value) <- sortOn fst fields]
        <> Builder.char8 '}'
    render (JArray values) = Builder.char8 '[' <> commaSeparated (map render values) <> Builder.char8 ']'
    render (JString value) = renderString value
    render (JInteger value) = Builder.integerDec value
    render (JNumber value) = Builder.string8 (show value)
    renderString value = Builder.char8 '"' <> Text.foldl' (\builder character -> builder <> escape character) mempty value <> Builder.char8 '"'
    commaSeparated = mconcat . intersperseBuilder (Builder.char8 ',')
    intersperseBuilder _ [] = []
    intersperseBuilder separator (value : values) = value : concatMap (\next -> [separator, next]) values
    escape '"' = Builder.string8 "\\\""
    escape '\\' = Builder.string8 "\\\\"
    escape '\b' = Builder.string8 "\\b"
    escape '\f' = Builder.string8 "\\f"
    escape '\n' = Builder.string8 "\\n"
    escape '\r' = Builder.string8 "\\r"
    escape '\t' = Builder.string8 "\\t"
    escape character
      | code < 0x20 || code > 0x7f = unicodeEscape code
      | otherwise = Builder.char8 character
      where
        code = ord character
    unicodeEscape code
      | code <= 0xffff = Builder.string8 "\\u" <> hex4 code
      | otherwise =
          let adjusted = code - 0x10000
              high = 0xd800 + (adjusted `shiftR` 10)
              low = 0xdc00 + (adjusted .&. 0x3ff)
           in Builder.string8 "\\u" <> hex4 high <> Builder.string8 "\\u" <> hex4 low
    hex4 code = mconcat [Builder.char8 (hexDigit ((code `shiftR` shift) .&. 0xf)) | shift <- [12, 8, 4, 0]]
    hexDigit value
      | value < 10 = toEnum (fromEnum '0' + value)
      | otherwise = toEnum (fromEnum 'a' + value - 10)

packVector :: DenseVector -> ByteString
packVector (DenseVector values) =
  LazyByteString.toStrict
    ( Builder.toLazyByteString
        (Unboxed.foldl' (\builder value -> builder <> Builder.word32LE (castFloatToWord32 (realToFrac value))) mempty values)
    )

unpackVector :: ByteString -> Either VectorError DenseVector
unpackVector bytes
  | BS.length bytes `mod` 4 /= 0 = Left (InvalidVectorBlobLength (BS.length bytes))
  | otherwise = Right (denseVector (go bytes))
  where
    go remaining
      | BS.null remaining = []
      | otherwise =
          let (chunk, rest) = BS.splitAt 4 remaining
              word = littleEndianWord32 chunk
           in realToFrac (castWord32ToFloat word) : go rest

littleEndianWord32 :: ByteString -> Word32
littleEndianWord32 bytes =
  foldl' (\value (index, byte) -> value .|. (fromIntegral byte `shiftL` (index * 8))) 0 (zip [0 .. 3] (BS.unpack (BS.take 4 bytes)))

dot :: DenseVector -> DenseVector -> Either VectorError Double
dot (DenseVector left) (DenseVector right)
  | leftLength /= rightLength = Left (VectorDimensionMismatch leftLength rightLength)
  | otherwise = Right (go 0 0)
  where
    leftLength = Unboxed.length left
    rightLength = Unboxed.length right
    go index total
      | index >= leftLength = total
      | otherwise = go (index + 1) (total + Unboxed.unsafeIndex left index * Unboxed.unsafeIndex right index)

data SectionIndexMode
  = ExactSectionScan
  | LshSectionScan
  deriving (Eq, Ord, Show)

sectionIndexMode :: Int -> SectionIndexMode
sectionIndexMode currentDetailedSections
  | currentDetailedSections <= vectorExactSectionThreshold = ExactSectionScan
  | otherwise = LshSectionScan

newtype LshBucket = LshBucket Word8
  deriving (Eq, Ord, Show)

type ProjectionPlane = Vector.Vector (Int, Double)

data LshPlan = LshPlan
  { lshPlanDimensions :: Int,
    lshPlanSeed :: Text,
    lshPlanProjectionBands :: Vector.Vector (Vector.Vector ProjectionPlane)
  }
  deriving (Eq, Show)

lshPlanPlanes :: LshPlan -> [[[(Int, Double)]]]
lshPlanPlanes =
  map (map Vector.toList . Vector.toList)
    . Vector.toList
    . lshPlanProjectionBands

buildLshPlan :: Int -> Text -> Either VectorError LshPlan
buildLshPlan dimensions seed
  | dimensions <= 0 = Left (InvalidLshDimensions dimensions)
  | otherwise =
      Right
        LshPlan
          { lshPlanDimensions = dimensions,
            lshPlanSeed = seed,
            lshPlanProjectionBands = Vector.generate vectorIndexBands buildBand
          }
  where
    sampleSize = min 16 dimensions
    buildBand band = Vector.generate vectorIndexBits (buildPlane band)
    buildPlane band bit = Vector.fromList (go Set.empty [] 0)
      where
        go :: Set Int -> [(Int, Double)] -> Int -> [(Int, Double)]
        go used values counter
          | length values >= sampleSize = reverse values
          | Set.member index used = go used values (counter + 1)
          | otherwise = go (Set.insert index used) ((index, sign) : values) (counter + 1)
          where
            digest =
              blake2b128Digest
                ( TextEncoding.encodeUtf8
                    ( seed
                        <> ":"
                        <> Text.pack (show band)
                        <> ":"
                        <> Text.pack (show bit)
                        <> ":"
                        <> Text.pack (show counter)
                    )
                )
            index = fromIntegral (littleEndianWord64 digest `mod` fromIntegral dimensions)
            sign = if BS.index digest 8 .&. 1 == 1 then 1 else -1

lshSignatures :: LshPlan -> DenseVector -> Either VectorError [LshBucket]
lshSignatures plan (DenseVector values)
  | Unboxed.null values = Right (replicate vectorIndexBands (LshBucket 0))
  | actualDimensions /= lshPlanDimensions plan =
      Left (LshPlanDimensionMismatch (lshPlanDimensions plan) actualDimensions)
  | otherwise = Right (map signature (Vector.toList (lshPlanProjectionBands plan)))
  where
    actualDimensions = Unboxed.length values
    signature planes =
      LshBucket
        ( Vector.ifoldl'
            (\bucket bit plane -> if planeScore plane >= 0 then bucket .|. (1 `shiftL` bit) else bucket)
            0
            planes
        )
    planeScore =
      Vector.foldl'
        (\score (index, sign) -> score + Unboxed.unsafeIndex values index * sign)
        0

lshProbeBuckets :: LshBucket -> [LshBucket]
lshProbeBuckets (LshBucket bucket) =
  map LshBucket
    ( Set.toAscList
        (Set.fromList (bucket : [bucket `xor` (1 `shiftL` bit) | bit <- [0 .. vectorIndexBits - 1]]))
    )

data LshIndex = LshIndex
  { lshIndexPlan :: LshPlan,
    lshIndexItemIds :: Vector.Vector Text,
    lshIndexOrdinals :: Map Text Int,
    lshIndexBuckets :: Map (Int, LshBucket) IntSet
  }
  deriving (Eq, Show)

lshIndexItemCount :: LshIndex -> Int
lshIndexItemCount = Vector.length . lshIndexItemIds

lshIndexMembershipCount :: LshIndex -> Int
lshIndexMembershipCount = sum . map IntSet.size . Map.elems . lshIndexBuckets

buildLshIndex :: LshPlan -> [(Text, DenseVector)] -> Either VectorError LshIndex
buildLshIndex plan entries = do
  signatures <- traverse (lshSignatures plan . snd) entries
  buildLshIndexFromSignatures plan (zip (map fst entries) signatures)

buildLshIndexFromSignatures :: LshPlan -> [(Text, [LshBucket])] -> Either VectorError LshIndex
buildLshIndexFromSignatures plan entries = do
  ordinals <- buildOrdinals entries
  traverse_ validateSignature entries
  let ids = Vector.fromList (map fst entries)
      buckets =
        foldl'
          addMemberships
          Map.empty
          (zip [0 ..] (map snd entries))
  pure
    LshIndex
      { lshIndexPlan = plan,
        lshIndexItemIds = ids,
        lshIndexOrdinals = ordinals,
        lshIndexBuckets = buckets
      }
  where
    buildOrdinals = foldM addOrdinal Map.empty . zip [0 ..]
    addOrdinal existing (ordinal, (itemId, _))
      | Map.member itemId existing = Left (DuplicateLshItemId itemId)
      | otherwise = Right (Map.insert itemId ordinal existing)
    validateSignature (_, signature)
      | length signature == vectorIndexBands = Right ()
      | otherwise = Left (InvalidLshSignatureCount (length signature))
    addMemberships buckets (ordinal, signatures) =
      foldl'
        (\current (band, bucket) -> Map.insertWith IntSet.union (band, bucket) (IntSet.singleton ordinal) current)
        buckets
        (zip [0 ..] signatures)

data CandidateFallback
  = CandidateNoFallback
  | CandidateExactFallback
  | CandidateSparseFallback
  deriving (Eq, Ord, Show)

data CandidateDiagnostics = CandidateDiagnostics
  { candidateAlgorithm :: Text,
    candidateCorpusSize :: Int,
    candidateBucketHits :: Int,
    candidateCount :: Int,
    candidateFallback :: CandidateFallback,
    candidateTarget :: Integer,
    candidateCap :: Maybe Integer
  }
  deriving (Eq, Show)

selectCandidates :: LshIndex -> DenseVector -> Set Text -> Int -> Bool -> Either VectorError (Set Text, CandidateDiagnostics)
selectCandidates index query allowed limit fallbackToAll
  | lshIndexItemCount index == 0 =
      Right
        ( allowed,
          CandidateDiagnostics
            { candidateAlgorithm = "exact",
              candidateCorpusSize = 0,
              candidateBucketHits = 0,
              candidateCount = Set.size allowed,
              candidateFallback = CandidateNoFallback,
              candidateTarget = target,
              candidateCap = Nothing
            }
        )
  | otherwise = do
      signatures <- lshSignatures (lshIndexPlan index) query
      let collisions = collisionCounts signatures
          collisionItemCount = Map.size collisions
          fallback = toInteger collisionItemCount < min target (toInteger (Set.size allowed))
          cap = max (target * 8) 512
          candidates
            | fallback && fallbackToAll = allowed
            | fallback = Map.keysSet collisions
            | otherwise =
                Set.fromList
                  ( map fst
                      (takeInteger cap (sortCollisions (Map.toList collisions)))
                  )
          fallbackKind
            | not fallback = CandidateNoFallback
            | fallbackToAll = CandidateExactFallback
            | otherwise = CandidateSparseFallback
          algorithm
            | not fallback = "lsh+exact-rerank"
            | fallbackToAll = "exact-fallback"
            | otherwise = "lsh-sparse"
      pure
        ( candidates,
          CandidateDiagnostics
            { candidateAlgorithm = algorithm,
              candidateCorpusSize = lshIndexItemCount index,
              candidateBucketHits = collisionItemCount,
              candidateCount = Set.size candidates,
              candidateFallback = fallbackKind,
              candidateTarget = target,
              candidateCap = if fallback then Nothing else Just cap
            }
        )
  where
    target = max (toInteger vectorIndexMinCandidates) (toInteger limit * 8)
    collisionCounts signatures =
      foldl' addBand Map.empty (zip [0 ..] signatures)
    addBand counts (band, signature) =
      foldl' addBucket counts (lshProbeBuckets signature)
      where
        addBucket current bucket =
          foldl' addOrdinal current (IntSet.toList (Map.findWithDefault IntSet.empty (band, bucket) (lshIndexBuckets index)))
        addOrdinal current ordinal =
          let itemId = lshIndexItemIds index Vector.! ordinal
           in if Set.member itemId allowed then Map.insertWith (+) itemId 1 current else current

sortCollisions :: [(Text, Int)] -> [(Text, Int)]
sortCollisions =
  sortBy
    ( \(leftId, leftCount) (rightId, rightCount) ->
        compare rightCount leftCount <> compare rightId leftId
    )

takeInteger :: Integer -> [value] -> [value]
takeInteger amount values
  | amount <= 0 = []
  | amount >= toInteger (length values) = values
  | otherwise = take (fromInteger amount) values

rerankByDot :: Int -> DenseVector -> Map Text DenseVector -> Set Text -> Either VectorError [(Text, Double)]
rerankByDot limit query corpus candidates = do
  scored <- traverse score (Set.toList candidates)
  pure (take (max 0 limit) (sortBy compareScore scored))
  where
    score itemId = case Map.lookup itemId corpus of
      Nothing -> Left (MissingLshItemId itemId)
      Just candidate -> do
        similarity <- dot query candidate
        pure (itemId, similarity)
    compareScore (leftId, leftScore) (rightId, rightScore) =
      compare rightScore leftScore <> compare rightId leftId
