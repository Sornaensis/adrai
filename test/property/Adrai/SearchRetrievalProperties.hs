{-# LANGUAGE OverloadedStrings #-}

module Adrai.SearchRetrievalProperties (tests) where

import Adrai.Retrieval
import Data.Char (isAscii, isAlphaNum)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Hedgehog (Gen, Property, forAll, property, withTests, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "P3-02 retrieval properties"
    [ testProperty "FTS quoting is an independent quote-doubling transform" propFtsQuote,
      testProperty "exact terms use an independent ASCII-run reference" propExactTerms,
      testProperty "deduplication preserves first occurrence while phrase preserves duplicates" propDedupeVsPhrase,
      testProperty "aliases retain caller order independent of query occurrence" propAliasOrder,
      testProperty "section source order and weights are stable under empty optional fields" propSectionOrder
    ]

propFtsQuote :: Property
propFtsQuote = withTests 100 . property $ do
  input <- forAll genText
  ftsQuote input === "\"" <> Text.replace "\"" "\"\"" input <> "\""

propExactTerms :: Property
propExactTerms = withTests 100 . property $ do
  input <- forAll genText
  let actual = map unquote (queryPlanFtsExactTerms (buildQueryPlan input []))
      raw = map Text.toLower (asciiRunsReference input)
      filtered = orderedUnique [token | token <- raw, Text.length token >= 2, not (Set.member token queryScaffolding)]
      expected = if null filtered then orderedUnique raw else filtered
  actual === expected

propDedupeVsPhrase :: Property
propDedupeVsPhrase = withTests 80 . property $ do
  words_ <- forAll (Gen.list (Range.linear 2 30) genPlainWord)
  let input = Text.unwords words_
      plan = buildQueryPlan input []
      raw = map Text.toLower words_
      informative = [word | word <- raw, not (Set.member word queryScaffolding)]
      expected = orderedUnique (if null informative then raw else informative)
  queryPlanFtsExactPhrase plan === ftsQuote (Text.unwords raw)
  map unquote (queryPlanFtsExactTerms plan) === expected

propAliasOrder :: Property
propAliasOrder = withTests 80 . property $ do
  aliases <- forAll (Gen.shuffle [("aa", "first expansion"), ("bb", "second expansion"), ("cc", "third expansion")])
  let expected = map snd aliases
  queryPlanAliases (buildQueryPlan "aa bb cc" aliases) === expected

propSectionOrder :: Property
propSectionOrder = withTests 50 . property $ do
  includeSummary <- forAll Gen.bool
  includeDomains <- forAll Gen.bool
  let summary = if includeSummary then "summary" else ""
      domains = if includeDomains then ["domain"] else []
      sources = sectionSources "title" summary "# Decision\nchoice" domains ""
      expected = [TitleSummarySection, DecisionSection] <> [DomainsSection | includeDomains]
  map sectionSourceKind sources === expected
  map sectionSourceWeight sources === map sectionWeight expected

genText :: Gen Text
genText = Text.pack <$> Gen.list (Range.linear 0 80) (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> " _-/:.\"!?"))

genPlainWord :: Gen Text
genPlainWord = Text.pack <$> Gen.list (Range.linear 2 8) (Gen.element ['g' .. 'z'])

asciiRunsReference :: Text -> [Text]
asciiRunsReference = filter (not . Text.null) . Text.split (not . asciiAlphaNumeric)
  where
    asciiAlphaNumeric character = isAscii character && isAlphaNum character

orderedUnique :: [Text] -> [Text]
orderedUnique = reverse . snd . foldl' add (Set.empty, [])
  where
    add (seen, values) value
      | Text.null value || Set.member value seen = (seen, values)
      | otherwise = (Set.insert value seen, value : values)

unquote :: Text -> Text
unquote value = Text.dropEnd 1 (Text.drop 1 value)
