{-# LANGUAGE OverloadedStrings #-}

module Adrai.DomainFormatTest (tests) where

import Adrai.Domain
import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "domain format"
    [ testCase "canonicalizes frozen root and dotted domain vectors" $ do
        canonical "  Compiler  " @?= Right "compiler"
        canonical "  Compiler . Cache-Key  " @?= Right "compiler.cache-key",
      testCase "canonical domain lists are sorted unique antichains" $
        fmap (fmap domainText)
          ( canonicalDomains
              [ " Architecture ",
                " Build . Reproducibility ",
                "compiler.cache",
                "COMPILER.CACHE"
              ]
          )
          @?= Right ["architecture", "build.reproducibility", "compiler.cache"],
      testCase "rejects all frozen invalid domain spellings" $
        forM_ invalidDomainSpellings $ \value ->
          assertBool ("expected invalid domain: " <> Text.unpack value) (isLeft (mkDomain value)),
      testCase "NFKC folds compatibility domain spellings before validation" $ do
        canonical "Ｃｏｍｐｉｌｅｒ" @?= Right "compiler"
        canonical " Ｃｏｍｐｉｌｅｒ ． Ｃａｃｈｅ－Ｋｅｙ "
          @?= Right "compiler.cache-key"
        canonical "\x00a0\&Compiler\x00a0" @?= Right "compiler",
      testCase "rejects characters that remain outside ASCII after NFKC" $ do
        mkDomain "café.cache" @?= Left (DomainNonAscii "café.cache")
        mkDomain "日本語.cache" @?= Left (DomainNonAscii "日本語.cache"),
      testCase "enforces canonical length and segment bounds" $ do
        let longSegment = Text.replicate 49 "a"
            tooMany = Text.intercalate "." (replicate 9 "a")
            tooLong = Text.replicate 161 "a"
        mkDomain longSegment @?= Left (DomainSegmentTooLong longSegment 49)
        mkDomain tooMany @?= Left (DomainTooManySegments 9)
        mkDomain tooLong @?= Left (DomainTooLong 161),
      testCase "rejects ancestor-descendant domain sets" $
        assertAntichainViolation "compiler" "compiler.cache",
      testCase "domain containment distinguishes siblings and descendants" $ do
        compiler <- domainOrFail "compiler"
        cache <- domainOrFail "compiler.cache"
        identity <- domainOrFail "compiler.cache.identity"
        runtime <- domainOrFail "runtime.cache"
        domainIsWithin compiler compiler @?= True
        domainIsWithin cache compiler @?= True
        domainIsWithin identity compiler @?= True
        domainIsWithin compiler cache @?= False
        domainIsWithin runtime compiler @?= False,
      testCase "parses frozen strict refinement vectors" $ do
        refinement " Compiler = Compiler.Cache "
          @?= Right "compiler=compiler.cache"
        refinement " Compiler . Cache = Compiler.Cache.Identity "
          @?= Right "compiler.cache=compiler.cache.identity",
      testCase "rejects non-strict and malformed refinements" $
        forM_ invalidRefinements $ \value ->
          assertBool
            ("expected invalid refinement: " <> Text.unpack value)
            (isLeft (parseDomainRefinement value)),
      testCase "structured errors have stable explanatory text" $ do
        domainErrorText DomainEmpty @?= "domain may not be empty"
        assertBool
          "non-ASCII error explains its post-normalization boundary"
          ("after NFKC normalization" `Text.isInfixOf` domainErrorText (DomainNonAscii "ø"))
    ]

canonical :: Text -> Either DomainError Text
canonical = fmap domainText . mkDomain

refinement :: Text -> Either DomainError Text
refinement = fmap domainRefinementText . parseDomainRefinement

invalidDomainSpellings :: [Text]
invalidDomainSpellings =
  [ "compiler cache",
    "-compiler",
    "compiler_cache",
    "compiler/cache",
    "compiler..cache",
    ".compiler.cache",
    "compiler.cache.",
    "1compiler.cache",
    "compiler.-cache",
    "compiler.cache-",
    "compiler.cache--key"
  ]

invalidRefinements :: [Text]
invalidRefinements =
  [ "compiler",
    "compiler=compiler",
    "compiler=runtime",
    "compiler.cache",
    "compiler.cache=compiler.cache",
    "compiler.cache=runtime.cache",
    "compiler.cache=compiler",
    "compiler=compiler.cache=identity",
    "compiler=",
    "=compiler.cache"
  ]

assertAntichainViolation :: Text -> Text -> IO ()
assertAntichainViolation parentText childText =
  case canonicalDomains [childText, parentText] of
    Left (DomainAntichainViolation parent child) ->
      (domainText parent, domainText child) @?= (parentText, childText)
    Left other -> assertFailure ("unexpected domain error: " <> show other)
    Right domains -> assertFailure ("unexpected domains: " <> show domains)

domainOrFail :: Text -> IO Domain
domainOrFail value =
  case mkDomain value of
    Left domainError -> assertFailure (Text.unpack (domainErrorText domainError)) >> fail "unreachable"
    Right domainValue -> pure domainValue
