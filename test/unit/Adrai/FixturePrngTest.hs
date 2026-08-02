{-# LANGUAGE OverloadedStrings #-}

module Adrai.FixturePrngTest (tests) where

import Adrai.Fixture.Prng
  ( Gen,
    UniformError (ZeroBound),
    algorithmName,
    chooseIndex,
    mkGen,
    mkSeed,
    nextWord64,
    uniformBelow,
  )
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Word (Word64)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "fixture PRNG"
    [ testCase "algorithm identity is stable" $
        algorithmName @?= ("adrai-fixture-splitmix64/v1" :: Text),
      testCase "seed 260729 has stable first ten outputs" $
        takeWords 10 (mkGen (mkSeed 260729)) @?= expectedSeed260729,
      testCase "the same seed is deterministic" $
        takeWords 32 (mkGen (mkSeed 260729))
          @?= takeWords 32 (mkGen (mkSeed 260729)),
      testCase "zero uniform bound is explicit" $
        uniformBelow 0 (mkGen (mkSeed 260729)) @?= Left ZeroBound,
      testCase "unit uniform bound always selects zero" $
        fmap fst (uniformBelow 1 (mkGen (mkSeed 260729))) @?= Right 0,
      testCase "uniform sampling rejects below-threshold candidates" $
        let (_, startGen) = nextWord64 (mkGen (mkSeed 260729))
            (rejectedCandidate, afterRejectedGen) = nextWord64 startGen
            (acceptedCandidate, expectedNextGen) = nextWord64 afterRejectedGen
            bound = 10000000000000000000
            threshold = 8446744073709551616
         in do
              rejectedCandidate @?= 3512641830275097810
              acceptedCandidate @?= 12305766707619970780
              (rejectedCandidate < threshold) @?= True
              (acceptedCandidate >= threshold) @?= True
              uniformBelow bound startGen
                @?= Right (2305766707619970780, expectedNextGen)
              fst (nextWord64 expectedNextGen) @?= 6377633111760951143,
      testCase "bounded draws remain below the bound" $
        all (< 17) (takeBounded 128 17 (mkGen (mkSeed 260729))) @?= True,
      testCase "chooseIndex selects deterministically from NonEmpty" $
        fmap fst (takeChoices 12 choices (mkGen (mkSeed 260729)))
          @?= fmap fst (takeChoices 12 choices (mkGen (mkSeed 260729)))
    ]
  where
    choices :: NonEmpty Text
    choices = "alpha" :| ["beta", "gamma", "delta"]

expectedSeed260729 :: [Word64]
expectedSeed260729 =
  [ 9250928540916208209,
    3512641830275097810,
    12305766707619970780,
    6377633111760951143,
    3112879710539361114,
    16509334917413603052,
    16853922627576762314,
    14395840773257153925,
    12546768959608094377,
    10026627469454164218
  ]

takeWords :: Int -> Gen -> [Word64]
takeWords count = take count . unfoldWords

unfoldWords :: Gen -> [Word64]
unfoldWords gen =
  let (value, nextGen) = nextWord64 gen
   in value : unfoldWords nextGen

takeBounded :: Int -> Word64 -> Gen -> [Word64]
takeBounded count bound = take count . unfoldBounded
  where
    unfoldBounded gen =
      case uniformBelow bound gen of
        Left ZeroBound -> []
        Right (value, nextGen) -> value : unfoldBounded nextGen

takeChoices :: Int -> NonEmpty a -> Gen -> [(a, Gen)]
takeChoices count values = take count . unfoldChoices
  where
    unfoldChoices gen =
      let choice@(_, nextGen) = chooseIndex values gen
       in choice : unfoldChoices nextGen
