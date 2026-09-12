{-# LANGUAGE OverloadedStrings #-}

module Adrai.VectorProperties (tests) where

import Adrai.Vector
import Data.Bits ((.&.), popCount, shiftR, xor)
import qualified Data.ByteString as BS
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import qualified Hedgehog as Hedgehog
import Hedgehog (Gen, Property, assert, forAll, property, withTests, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "P3-01 vector properties"
    [ testProperty "configured embeddings have fixed dimensions and independently normalized halves" propEmbeddingGeometry,
      testProperty "float32-origin blobs pack after unpack byte-for-byte" propFloat32ByteRoundTrip,
      testProperty "LSH planes signatures and probe neighborhoods satisfy frozen geometry" propLshGeometry,
      testProperty "LSH index results ignore input permutation" propIndexPermutation,
      testProperty "candidate selection matches a separate collision reference model" propCandidateReference,
      testProperty "strict-dot rerank equals brute force over the selected subset" propRerankReference
    ]

propEmbeddingGeometry :: Property
propEmbeddingGeometry = withTests 100 . property $ do
  input <- forAll genVectorText
  let semantic = semanticEmbedding input
      identifier = identifierEmbedding input
  denseDimension semantic === semanticVectorDimensions
  denseDimension identifier === identifierVectorDimensions
  assert (validHalfNorms semantic)
  assert (validHalfNorms identifier)

validHalfNorms :: DenseVector -> Bool
validHalfNorms vector = all valid [leftNorm, rightNorm]
  where
    values = denseValues vector
    half = length values `div` 2
    norm part = sqrt (foldl' (\total value -> total + value * value) 0 part)
    leftNorm = norm (take half values)
    rightNorm = norm (drop half values)
    valid value = abs value < 1e-12 || abs (value - sqrt 0.5) < 1e-12

propFloat32ByteRoundTrip :: Property
propFloat32ByteRoundTrip = withTests 100 . property $ do
  randomWords <- forAll (Gen.list (Range.linear 0 80) genNonNanFloatWord)
  let words32 = float32EdgeWords <> randomWords
      bytes = BS.concat (map word32LittleEndian words32)
  case unpackVector bytes of
    Left failure -> Hedgehog.footnote (show failure) >> assert False
    Right vector -> packVector vector === bytes

float32EdgeWords :: [Word32]
float32EdgeWords =
  [ 0x00000000,
    0x80000000,
    0x00000001,
    0x80000001,
    0x7f7fffff,
    0xff7fffff,
    0x7f800000,
    0xff800000
  ]

genNonNanFloatWord :: Gen Word32
genNonNanFloatWord = Gen.filter (\word -> word .&. 0x7f800000 /= 0x7f800000) (Gen.word32 Range.constantBounded)

word32LittleEndian :: Word32 -> BS.ByteString
word32LittleEndian word =
  BS.pack [fromIntegral (word `shiftR` shift) | shift <- [0, 8, 16, 24]]

propLshGeometry :: Property
propLshGeometry = withTests 100 . property $ do
  values <- forAll genFiniteVector
  let plan = mustRight (buildLshPlan 8 "deterministic-seed")
      vector = denseVector values
      planes = lshPlanPlanes plan
      signatures = mustRight (lshSignatures plan vector)
      neighborhoods = map lshProbeBuckets signatures
  length planes === vectorIndexBands
  assert (all ((== vectorIndexBits) . length) planes)
  assert (all (all validPlane) planes)
  length signatures === vectorIndexBands
  assert (and (zipWith validNeighborhood signatures neighborhoods))
  where
    validPlane plane = length plane == 8 && Set.size (Set.fromList (map fst plane)) == 8
    validNeighborhood (LshBucket original) probes =
      length probes == 9
        && probes == Set.toAscList (Set.fromList probes)
        && LshBucket original `elem` probes
        && all (\(LshBucket probe) -> probe == original || popCount (probe `xor` original) == 1) probes

propIndexPermutation :: Property
propIndexPermutation = withTests 60 . property $ do
  shuffled <- forAll (Gen.shuffle fixedEntries)
  limit <- forAll (Gen.int (Range.linear 1 20))
  fallbackToAll <- forAll Gen.bool
  let plan = mustRight (buildLshPlan 8 "permutation-seed")
      expectedIndex = mustRight (buildLshIndex plan fixedEntries)
      actualIndex = mustRight (buildLshIndex plan shuffled)
      allowed = Set.fromList (map fst fixedEntries)
      query = denseVector [1, -2, 3, -4, 5, -6, 7, -8]
      expected = fst (mustRight (selectCandidates expectedIndex query allowed limit fallbackToAll))
      actual = fst (mustRight (selectCandidates actualIndex query allowed limit fallbackToAll))
  actual === expected
  lshIndexMembershipCount actualIndex === lshIndexMembershipCount expectedIndex

propCandidateReference :: Property
propCandidateReference = withTests 60 . property $ do
  limit <- forAll (Gen.int (Range.linear 1 12))
  fallbackToAll <- forAll Gen.bool
  allowedCount <- forAll (Gen.int (Range.linear 55 70))
  let plan = mustRight (buildLshPlan 8 "reference-seed")
      entries = fixedEntries
      allowed = Set.fromList (map fst (take allowedCount entries))
      query = denseVector [1, 2, 1, 2, -1, -2, -1, -2]
      index = mustRight (buildLshIndex plan entries)
      actual = fst (mustRight (selectCandidates index query allowed limit fallbackToAll))
      expected = referenceSelect plan entries query allowed limit fallbackToAll
  actual === expected

referenceSelect :: LshPlan -> [(Text, DenseVector)] -> DenseVector -> Set Text -> Int -> Bool -> Set Text
referenceSelect plan entries query allowed limit fallbackToAll
  | Map.size collisions < fromInteger (min target (toInteger (Set.size allowed))) =
      if fallbackToAll then allowed else Map.keysSet collisions
  | otherwise = Set.fromList (map fst (takeIntegerReference cap (sortReference (Map.toList collisions))))
  where
    querySignatures = mustRight (lshSignatures plan query)
    probeSets = map (Set.fromList . lshProbeBuckets) querySignatures
    collisions = foldl' addItem Map.empty entries
    addItem counts (itemId, vector)
      | not (Set.member itemId allowed) = counts
      | otherwise =
          let signatures = mustRight (lshSignatures plan vector)
              count = length [() | (bucket, probes) <- zip signatures probeSets, Set.member bucket probes]
           in if count == 0 then counts else Map.insert itemId count counts
    target = max 64 (toInteger limit * 8)
    cap = max (target * 8) 512

sortReference :: [(Text, Int)] -> [(Text, Int)]
sortReference = sortBy (\(leftId, leftCount) (rightId, rightCount) -> compare rightCount leftCount <> compare rightId leftId)

takeIntegerReference :: Integer -> [value] -> [value]
takeIntegerReference amount values
  | amount >= toInteger (length values) = values
  | otherwise = take (fromInteger (max 0 amount)) values

propRerankReference :: Property
propRerankReference = withTests 80 . property $ do
  queryValues <- forAll genFiniteVector
  candidateValues <- forAll (Gen.list (Range.linear 1 24) genFiniteVector)
  limit <- forAll (Gen.int (Range.linear 0 24))
  let query = denseVector queryValues
      entries = [(Text.pack (show ordinal), denseVector values) | (ordinal, values) <- zip [0 :: Int ..] candidateValues]
      corpus = Map.fromList entries
      selected = Map.keysSet corpus
      actual = mustRight (rerankByDot limit query corpus selected)
      expected = take limit (sortBy compareScore [(itemId, strictDot queryValues (denseValues vector)) | (itemId, vector) <- entries])
  actual === expected
  where
    compareScore (leftId, leftScore) (rightId, rightScore) = compare rightScore leftScore <> compare rightId leftId

strictDot :: [Double] -> [Double] -> Double
strictDot left right = foldl' (\total (a, b) -> total + a * b) 0 (zip left right)

genVectorText :: Gen Text
genVectorText = Text.pack <$> Gen.list (Range.linear 0 100) (Gen.element vectorCharacters)
  where
    vectorCharacters = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> " _./:-é"

genFiniteVector :: Gen [Double]
genFiniteVector = Gen.list (Range.singleton 8) (fromIntegral <$> Gen.int (Range.linear (-20) 20))

fixedEntries :: [(Text, DenseVector)]
fixedEntries =
  [ (Text.pack ("candidate-" <> show ordinal), denseVector (values ordinal))
    | ordinal <- [0 .. 69 :: Int]
  ]
  where
    values ordinal =
      [ fromIntegral (((ordinal + dimension * 7) `mod` 19) - 9)
        | dimension <- [0 .. 7]
      ]

mustRight :: (Show failure) => Either failure value -> value
mustRight (Right value) = value
mustRight (Left failure) = error (show failure)
