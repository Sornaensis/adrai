{-# LANGUAGE OverloadedStrings #-}

module Adrai.Fixture.Prng
  ( Seed,
    mkSeed,
    seedWord64,
    Gen,
    mkGen,
    algorithmName,
    UniformError (..),
    nextWord64,
    uniformBelow,
    chooseIndex,
  )
where

import Data.Bits (shiftR, xor)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Data.Word (Word64)

-- | Seed for the versioned, test-only fixture generator.
newtype Seed = Seed Word64
  deriving (Eq, Ord, Show)

-- | Opaque state for the fixture generator.
newtype Gen = Gen Word64
  deriving (Eq, Show)

-- | Errors that can arise while selecting a bounded value.
data UniformError
  = ZeroBound
  deriving (Eq, Show)

-- | Stable algorithm identity recorded with generated fixture specifications.
algorithmName :: Text
algorithmName = "adrai-fixture-splitmix64/v1"

mkSeed :: Word64 -> Seed
mkSeed = Seed

seedWord64 :: Seed -> Word64
seedWord64 (Seed value) = value

mkGen :: Seed -> Gen
mkGen (Seed value) = Gen value

-- | Advance SplitMix64 once. 'Word64' arithmetic deliberately wraps modulo 2^64.
nextWord64 :: Gen -> (Word64, Gen)
nextWord64 (Gen state) =
  let nextState = state + 0x9e3779b97f4a7c15
   in (mix64 nextState, Gen nextState)

-- | Draw uniformly from @[0, bound)@ using rejection sampling.
--
-- A zero bound is an explicit caller error. Rejection avoids the modulo bias of
-- applying @mod bound@ to every possible 'Word64'.
uniformBelow :: Word64 -> Gen -> Either UniformError (Word64, Gen)
uniformBelow 0 _ = Left ZeroBound
uniformBelow bound gen = Right (uniformBelowPositive bound gen)

-- | Select an element of a non-empty collection without a partial public API.
chooseIndex :: NonEmpty a -> Gen -> (a, Gen)
chooseIndex values gen =
  let bound = fromIntegral (NonEmpty.length values)
      (selected, nextGen) = uniformBelowPositive bound gen
   in (indexNonEmpty selected values, nextGen)

mix64 :: Word64 -> Word64
mix64 input =
  let mixed1 = (input `xor` (input `shiftR` 30)) * 0xbf58476d1ce4e5b9
      mixed2 = (mixed1 `xor` (mixed1 `shiftR` 27)) * 0x94d049bb133111eb
   in mixed2 `xor` (mixed2 `shiftR` 31)

uniformBelowPositive :: Word64 -> Gen -> (Word64, Gen)
uniformBelowPositive bound = go
  where
    threshold = negate bound `mod` bound

    go gen =
      let (candidate, nextGen) = nextWord64 gen
       in if candidate < threshold
            then go nextGen
            else (candidate `mod` bound, nextGen)

indexNonEmpty :: Word64 -> NonEmpty a -> a
indexNonEmpty target values = go target (NonEmpty.toList values)
  where
    go 0 (value : _) = value
    go remaining (_ : rest) = go (remaining - 1) rest
    go _ [] = NonEmpty.head values
