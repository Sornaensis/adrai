{-# LANGUAGE OverloadedStrings #-}

module Adrai.TypesProperties (tests) where

import Adrai.Types
import Data.List (permutations)
import qualified Data.Text as T
import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "foundation properties"
    [ testProperty "ID public input normalization is idempotent" prop_prefixNormalization,
      testProperty "unique resolution is invariant under candidate permutation" prop_uniquePermutation,
      testProperty "duplicate candidates do not create ambiguity" prop_duplicateDeterminism,
      testProperty "ambiguity ordering is invariant under candidate permutation" prop_ambiguityPermutation,
      testProperty "safe Unicode path round trips" prop_unicodePathRoundTrip
    ]

prop_prefixNormalization :: Property
prop_prefixNormalization = property $ do
  paddingLeft <- forAll (Gen.int (Range.linear 0 3))
  paddingRight <- forAll (Gen.int (Range.linear 0 3))
  let raw = T.replicate paddingLeft " " <> "a0123456" <> T.replicate paddingRight " "
  case mkIdPrefix raw of
    Left violation -> fail (show violation)
    Right prefix -> do
      idPrefixText prefix === "A0123456"
      mkIdPrefix (idPrefixText prefix) === Right prefix

prop_uniquePermutation :: Property
prop_uniquePermutation = property $ do
  choice <- forAll (Gen.element (permutations candidates))
  resolveObjectRef "a0123456" choice === Right adrRef
  where
    adrRef = required (fmap adrObjectRef (mkAdrId adrText))
    recordRef = required (fmap recordObjectRef (mkRecordId recordText))
    connectionRef = required (fmap connectionObjectRef (mkConnectionId connectionText))
    candidates = [recordRef, adrRef, connectionRef]

prop_duplicateDeterminism :: Property
prop_duplicateDeterminism = property $ do
  copies <- forAll (Gen.int (Range.linear 1 20))
  resolveObjectRef "A0123456" (replicate copies adrRef) === Right adrRef
  where
    adrRef = required (fmap adrObjectRef (mkAdrId adrText))

prop_ambiguityPermutation :: Property
prop_ambiguityPermutation = property $ do
  choice <- forAll (Gen.element (permutations [secondRef, firstRef, secondRef]))
  resolveObjectRef "A0123456" choice === expected
  where
    firstRef = required (fmap adrObjectRef (mkAdrId adrText))
    secondRef = required (fmap adrObjectRef (mkAdrId adrTextB))
    prefix = required (mkIdPrefix "A0123456")
    expected = Left (PrefixAmbiguous prefix [firstRef, secondRef])

prop_unicodePathRoundTrip :: Property
prop_unicodePathRoundTrip = property $ do
  segment <- forAll (Gen.text (Range.linear 1 30) safeUnicode)
  let path = "architecture/" <> segment <> "/record.md"
  fmap repoPathText (mkRepoPath path) === Right path
  where
    safeUnicode = Gen.element (['a' .. 'z'] <> ['0' .. '9'] <> "-øé日本語")

required :: (Show error) => Either error value -> value
required value =
  case value of
    Left problem -> error ("invalid property fixture: " <> show problem)
    Right result -> result

adrText :: T.Text
adrText = "A0123456789ABCDEFGHJKMNPQRS"

adrTextB :: T.Text
adrTextB = "A0123456789ABCDEFGHJKMNPQRT"

recordText :: T.Text
recordText = "R0123456789ABCDEFGHJKMNPQRS"

connectionText :: T.Text
connectionText = "C0123456789ABCDEFGHJKMNPQRS"
