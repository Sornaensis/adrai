{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Adrai.CompilerMaterializationProperties (tests) where

import Adrai.Compiler (materializeCurrentSearch)
import Adrai.Graph (AxisResolution (..), GraphReduction (..), ReducedAdr (..), reduceManagedGraph)
import Adrai.History (ReadSnapshot (..), RevisionIdentity (..))
import Adrai.Property.Generators
import Adrai.Relevance
import Adrai.Retrieval
import Data.List (findIndices, sort, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Hedgehog (Gen, Property, assert, footnote, forAll, property, withTests, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "P3-03 current search materialization properties"
    [ testProperty "document permutation preserves documents passages and aliases" prop_documentPermutation,
      testProperty "item and passage identities are unique and canonically ordered" prop_identityOrdering,
      testProperty "item cardinality equals the reducer's current decision-head count" prop_currentHeadCardinality,
      testProperty "chunking matches an independent target overlap radius and line reference model" prop_chunkReferenceModel
    ]

prop_documentPermutation :: Property
prop_documentPermutation = withTests 100 . property $ do
  spec <- forAll genProjectionDagSpec
  let fixture = materializeProjectionDag spec
      snapshot = projectionFixtureAfter fixture
      expected = materialize snapshot
  firstPermutation <- forAll (sampledPermutation (readSnapshotDocuments snapshot))
  secondPermutation <- forAll (sampledPermutation (readSnapshotDocuments snapshot))
  materialize (snapshot {readSnapshotDocuments = firstPermutation}) === expected
  materialize (snapshot {readSnapshotDocuments = secondPermutation}) === expected

prop_identityOrdering :: Property
prop_identityOrdering = withTests 100 . property $ do
  spec <- forAll (genAxisConflictSpec Nothing)
  let fixture = materializeAxisConflict spec
      materialization = materialize (axisSnapshot fixture)
      documents = searchMaterializationDocuments materialization
      passages = searchMaterializationPassages materialization
      itemIds = map searchDocumentItemId documents
      passageIds = map searchPassageId passages
      passageOrder passage =
        ( searchPassageDocumentItemId passage,
          searchPassageSectionKind passage,
          searchPassageOrdinal passage
        )
  itemIds === sort itemIds
  assert (unique itemIds)
  assert (unique passageIds)
  passages === sortOn passageOrder passages
  searchMaterializationAliases materialization === sort (searchMaterializationAliases materialization)

prop_currentHeadCardinality :: Property
prop_currentHeadCardinality = withTests 100 . property $ do
  spec <- forAll (genAxisConflictSpec Nothing)
  let fixture = materializeAxisConflict spec
      snapshot = axisSnapshot fixture
      materialization = materialize snapshot
      expected =
        sum
          [ length (axisResolutionHeads (reducedDecisionAxis adr))
            | adr <- graphReductionAdrs (readSnapshotReduction snapshot)
          ]
  length (searchMaterializationDocuments materialization) === expected

prop_chunkReferenceModel :: Property
prop_chunkReferenceModel = withTests 100 . property $ do
  input <- forAll genChunkInput
  targetChunkChars === 1500
  chunkOverlapChars === 300
  chunkBoundaryRadius === 220
  chunkText input === Right (referenceChunks input)
  let tieInput = newlineTieInput <> Text.replicate 1700 "z"
      actualTie = expectRight "newline tie chunks" (chunkText tieInput)
      referenceTie = referenceChunks tieInput
  actualTie === referenceTie
  case actualTie of
    first : second : _ -> do
      Text.length (textChunkText first) === targetChunkChars - 99
      Text.take chunkOverlapChars (textChunkText second)
        === Text.takeEnd chunkOverlapChars (textChunkText first)
      textChunkStartLine first === 1
      textChunkEndLine first === 1
      textChunkStartLine second === 1
      assert (textChunkEndLine second >= textChunkStartLine second)
    _ -> footnote "newline tie fixture did not produce overlapping chunks" >> assert False

genChunkInput :: Gen Text
genChunkInput =
  Gen.choice
    [ Text.pack <$> Gen.list (Range.linear 0 5200) (Gen.frequency [(18, Gen.element ['a' .. 'z']), (2, pure ' '), (1, pure '\n')]),
      pure (Text.replicate targetChunkChars "a"),
      pure (Text.replicate (targetChunkChars - chunkBoundaryRadius) "a" <> "\n" <> Text.replicate 1900 "b"),
      pure (newlineTieInput <> Text.replicate 1700 "z"),
      pure "one\ntwo\nthree\n"
    ]

newlineTieInput :: Text
newlineTieInput =
  Text.replicate (targetChunkChars - 100) "a"
    <> "\n"
    <> Text.replicate 197 "b"
    <> "\n"

referenceChunks :: Text -> [TextChunk]
referenceChunks input
  | Text.null input || Text.null (Text.strip input) = []
  | otherwise = go 0 0 []
  where
    textLength = Text.length input
    go start ordinal chunks
      | start >= textLength = reverse chunks
      | otherwise =
          let target = min textLength (start + 1500)
              selectedEnd = referenceEnd input start target
              end = if selectedEnd <= start then min textLength (start + 1500) else selectedEnd
              raw = Text.take (end - start) (Text.drop start input)
              hasContent = not (Text.null (Text.strip raw))
              nextChunks =
                if hasContent
                  then TextChunk ordinal (referenceLine input start) (referenceEndLine input start end raw) raw : chunks
                  else chunks
              nextOrdinal = if hasContent then ordinal + 1 else ordinal
           in if end >= textLength
                then reverse nextChunks
                else go (max (start + 1) (end - 300)) nextOrdinal nextChunks

referenceEnd :: Text -> Int -> Int -> Int
referenceEnd text start target
  | target >= Text.length text = Text.length text
  | otherwise =
      case candidates of
        [] -> target
        first : rest -> foldl' nearer first rest
  where
    lower = max (start + 1) (target - 220)
    upper = min (Text.length text) (target + 220)
    window = Text.take (upper - lower) (Text.drop lower text)
    candidates = [lower + offset + 1 | offset <- findIndices (== '\n') (Text.unpack window)]
    nearer best candidate
      | (abs (candidate - target), candidate) < (abs (best - target), best) = candidate
      | otherwise = best

referenceLine :: Text -> Int -> Int
referenceLine text offset = Text.count "\n" (Text.take (max 0 (min offset (Text.length text))) text) + 1

referenceEndLine :: Text -> Int -> Int -> Text -> Int
referenceEndLine text start end raw =
  referenceLine text (max start (end - Text.length (Text.takeWhileEnd (== '\n') raw) - 1))

axisSnapshot :: AxisFixture -> ReadSnapshot
axisSnapshot fixture =
  ReadSnapshot
    (RevisionIdentity "axis-property" "axis-property-resolved")
    documents
    (reduceManagedGraph records)
    Map.empty
  where
    records = axisFixtureRecords fixture
    pool = axisConflictPool (axisFixtureSpec fixture)
    documents = zipWith (\ordinal -> parsedDocument pool ordinal (fromIntegral ordinal + 100) []) [0 ..] records

materialize :: ReadSnapshot -> SearchMaterialization
materialize snapshot =
  case materializeCurrentSearch snapshot of
    Right result -> result
    Left problem -> error ("P3-03 materialization fixture failed: " <> show problem)

unique :: (Ord value) => [value] -> Bool
unique values = length values == Set.size (Set.fromList values)

expectRight :: String -> Either error value -> value
expectRight label result =
  case result of
    Right value -> value
    Left _ -> error ("P3-03 " <> label <> " failed")
