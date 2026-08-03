{-# LANGUAGE OverloadedStrings #-}

module Adrai.DomainProperties (tests) where

import Adrai.Domain
import Adrai.Property.Generators
import Data.List (sort)
import Data.Text qualified as Text
import Hedgehog (Property, assert, forAll, property, withTests, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "P2-06 domain properties"
    [ testProperty "normalization is idempotent" prop_normalizationIdempotent,
      testProperty "canonical sets are sorted unique antichains" prop_canonicalAntichain,
      testProperty "containment is reflexive and transitive" prop_containment,
      testProperty "refinements round trip and remain strict descendants" prop_refinement,
      testProperty "ancestor and descendant sets are rejected" prop_ancestorRejected,
      testProperty "hierarchical filter primitive distinguishes descendants and siblings" prop_filterPrimitive
    ]

prop_normalizationIdempotent :: Property
prop_normalizationIdempotent = withTests 150 . property $ do
  spec <- forAll genDomainSpec
  let canonical = domainFromSpec spec
      raw = "  " <> Text.intercalate " . " (map Text.toUpper (domainSpecSegments spec)) <> "  "
  parsed <- forAll (pure (mkDomain raw))
  parsed === Right canonical
  (parsed >>= mkDomain . domainText) === Right canonical

prop_canonicalAntichain :: Property
prop_canonicalAntichain = withTests 150 . property $ do
  spec <- forAll genDomainSpec
  let left = descendant spec "alpha"
      right = descendant spec "beta"
  canonicalDomains [domainText right, domainText left, domainText right]
    === Right (sort [left, right])

prop_containment :: Property
prop_containment = withTests 150 . property $ do
  spec <- forAll genDomainSpec
  let root = domainFromSpec spec
      child = descendant spec "child"
      grandchild = expectDomain (domainText child <> ".leaf")
  assert (domainIsWithin root root)
  assert (domainIsWithin child root)
  assert (domainIsWithin grandchild child)
  assert (domainIsWithin grandchild root)
  assert (not (domainIsWithin root child))

prop_refinement :: Property
prop_refinement = withTests 150 . property $ do
  spec <- forAll genDomainSpec
  let parent = domainFromSpec spec
      child = descendant spec "refined"
      refinement = expectRefinement parent child
  parseDomainRefinement (domainRefinementText refinement) === Right refinement
  assert (domainIsWithin child parent)
  assert (child /= parent)

prop_ancestorRejected :: Property
prop_ancestorRejected = withTests 150 . property $ do
  spec <- forAll genDomainSpec
  let parent = domainFromSpec spec
      child = descendant spec "child"
  canonicalDomains [domainText child, domainText parent]
    === Left (DomainAntichainViolation parent child)

-- This proves the pure hierarchy primitive used by filtering. The coverage
-- ledger deliberately remains partial until a search consumer applies repeated
-- filters conjunctively.
prop_filterPrimitive :: Property
prop_filterPrimitive = withTests 150 . property $ do
  spec <- forAll genDomainSpec
  let requested = domainFromSpec spec
      descendantValue = descendant spec "selected"
      sibling = expectDomain (Text.intercalate "." (domainSpecSegments spec <> ["sibling"]))
      unrelated = expectDomain "unrelated"
  assert (domainIsWithin requested requested)
  assert (domainIsWithin descendantValue requested)
  assert (not (domainIsWithin sibling descendantValue))
  assert (not (domainIsWithin unrelated requested))

descendant :: DomainSpec -> Text.Text -> Domain
descendant spec suffix =
  expectDomain (Text.intercalate "." (domainSpecSegments spec <> [suffix]))

expectDomain :: Text.Text -> Domain
expectDomain value =
  case mkDomain value of
    Right domain -> domain
    Left problem -> error (show problem)

expectRefinement :: Domain -> Domain -> DomainRefinement
expectRefinement parent child =
  case mkDomainRefinement parent child of
    Right refinement -> refinement
    Left problem -> error (show problem)
