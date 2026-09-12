module Adrai.FixtureProperties (tests) where

import Adrai.Fixture.LargeStress (largeStressV1)
import Adrai.Fixture.Prng (mkSeed)
import Adrai.Fixture.ProductionShape
import Adrai.Fixture.Types
import qualified Data.Set as Set
import Hedgehog (Property, assert, forAll, property, withTests, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "fixture plan properties"
    [ testProperty "changed seed perturbs noise while preserving counts" prop_changedSeed,
      testProperty "ordinals and operation keys are unique" prop_uniqueKeys,
      testProperty "semantic targets follow their creates" prop_targetsAfterCreate
    ]

prop_changedSeed :: Property
prop_changedSeed = withTests 20 . property $ do
  seed <- forAll (Gen.int (Range.linear 0 1000000))
  let first = withRepositorySeed (mkSeed (fromIntegral seed)) productionShapeV1
      second = withRepositorySeed (mkSeed (fromIntegral seed + 1)) productionShapeV1
  foldCounts first === foldCounts second
  assert (noisePrefix first /= noisePrefix second)

prop_uniqueKeys :: Property
prop_uniqueKeys = withTests 3 . property $ do
  let result = foldRepositoryPlan collectKeys emptyKeys largeStressV1
  keyCount result === 12000
  assert (keysUnique result)

prop_targetsAfterCreate :: Property
prop_targetsAfterCreate = withTests 3 . property $ do
  let (_, valid) = foldRepositoryPlan checkTarget (Set.empty, True) largeStressV1
  assert valid

foldCounts :: RepositoryPlan -> (Int, Int)
foldCounts = foldRepositoryPlan count (0, 0)
  where
    count (semantic, noise) commit =
      case plannedCommitKind commit of
        PlannedSemantic _ _ -> (semantic + 1, noise)
        PlannedNoise _ -> (semantic, noise + 1)
        PlannedMerge _ -> (semantic, noise)

noisePrefix :: RepositoryPlan -> [PlannedCommitKind]
noisePrefix = take 12 . foldRepositoryPlan collect []
  where
    collect found commit
      | length found >= 12 = found
      | otherwise =
          case plannedCommitKind commit of
            noise@(PlannedNoise _) -> found <> [noise]
            _ -> found

data KeyFold = KeyFold
  { keyCount :: Int,
    ordinalKeys :: Set.Set CommitOrdinal,
    operationKeys :: Set.Set OperationKey,
    keysUnique :: Bool
  }

emptyKeys :: KeyFold
emptyKeys = KeyFold 0 Set.empty Set.empty True

collectKeys :: KeyFold -> PlannedCommit -> KeyFold
collectKeys state commit =
  state
    { keyCount = keyCount state + 1,
      ordinalKeys = Set.insert ordinal (ordinalKeys state),
      operationKeys = maybe (operationKeys state) (`Set.insert` operationKeys state) operation,
      keysUnique =
        keysUnique state
          && Set.notMember ordinal (ordinalKeys state)
          && maybe True (`Set.notMember` operationKeys state) operation
    }
  where
    ordinal = plannedCommitOrdinal commit
    operation =
      case plannedCommitKind commit of
        PlannedSemantic key _ -> Just key
        _ -> Nothing

checkTarget :: (Set.Set AdrKey, Bool) -> PlannedCommit -> (Set.Set AdrKey, Bool)
checkTarget state@(_, False) _ = state
checkTarget (created, valid) commit =
  case plannedCommitKind commit of
    PlannedSemantic _ (PlannedCreate template) ->
      (Set.insert (adrTemplateKey template) created, valid)
    PlannedSemantic _ operation ->
      (created, maybe valid (`Set.member` created) (targetOf operation))
    _ -> (created, valid)

targetOf :: PlannedOperation -> Maybe AdrKey
targetOf operation =
  case operation of
    PlannedCreate _ -> Nothing
    PlannedAmend key _ -> Just key
    PlannedScope key _ -> Just key
    PlannedDomain key _ -> Just key
    PlannedObsolete key -> Just key
