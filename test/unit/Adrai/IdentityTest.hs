{-# LANGUAGE OverloadedStrings #-}

module Adrai.IdentityTest (tests) where

import Adrai.Identity
import Adrai.Types
  ( adrIdText,
    connectionIdText,
    operationIdText,
    recordIdText,
  )
import qualified Data.ByteString as BS
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "identity"
    [ crockfordVectors,
      typedIdentifierTests,
      validationTests,
      sortableLayoutTests
    ]

-- Exact coverage of every vector in contracts/v1/ids/crockford.json.
crockfordVectors :: TestTree
crockfordVectors =
  testCase "matches all frozen 128-bit Crockford vectors" $
    mapM_
      (\(bytes, expected) -> encodeCrockford128 bytes @?= Right expected)
      [ (BS.replicate 16 0x00, "00000000000000000000000000"),
        (bytesEndingIn 0x01, "00000000000000000000000001"),
        (bytesEndingIn 0x1f, "0000000000000000000000000Z"),
        (bytesEndingIn 0x20, "00000000000000000000000010"),
        (bytesEndingIn 0xff, "0000000000000000000000007Z"),
        (BS.replicate 16 0xff, "7ZZZZZZZZZZZZZZZZZZZZZZZZZ")
      ]

typedIdentifierTests :: TestTree
typedIdentifierTests =
  testCase "constructs strict A/R/C/O identifiers from the same bytes" $ do
    let bytes = BS.pack [0 .. 15]
    payload <- assertRight (encodeCrockford128 bytes)
    adrIdText <$> adrIdFromBytes bytes @?= Right ("A" <> payload)
    recordIdText <$> recordIdFromBytes bytes @?= Right ("R" <> payload)
    connectionIdText <$> connectionIdFromBytes bytes @?= Right ("C" <> payload)
    operationIdText <$> operationIdFromBytes bytes @?= Right ("O" <> payload)

validationTests :: TestTree
validationTests =
  testCase "rejects every invalid byte-component length without truncation" $ do
    encodeCrockford128 (BS.replicate 15 0) @?= Left (IdentityWrongLength 15)
    encodeCrockford128 (BS.replicate 17 0) @?= Left (IdentityWrongLength 17)
    sortableIdentityBytes (BS.replicate 5 0) (BS.replicate 10 0)
      @?= Left (IdentityTimestampWrongLength 5)
    sortableIdentityBytes (BS.replicate 6 0) (BS.replicate 9 0)
      @?= Left (IdentityEntropyWrongLength 9)

sortableLayoutTests :: TestTree
sortableLayoutTests =
  testCase "timestamp(6) precedes entropy(10) and preserves lexical order" $ do
    let earlierTimestamp = BS.pack [0, 0, 0, 0, 0, 1]
        laterTimestamp = BS.pack [0, 0, 0, 0, 0, 2]
        lowEntropy = BS.replicate 10 0x00
        highEntropy = BS.replicate 10 0xff
    sortableIdentityBytes earlierTimestamp highEntropy
      @?= Right (earlierTimestamp <> highEntropy)
    earlier <- assertRight (sortableAdrId earlierTimestamp highEntropy)
    later <- assertRight (sortableAdrId laterTimestamp lowEntropy)
    assertBool
      "big-endian timestamp should dominate entropy in text ordering"
      (adrIdText earlier < adrIdText later)

bytesEndingIn :: Word -> BS.ByteString
bytesEndingIn value = BS.replicate 15 0 <> BS.singleton (fromIntegral value)

assertRight :: (Show problem) => Either problem value -> IO value
assertRight result =
  case result of
    Right value -> pure value
    Left problem -> assertFailure (show problem) >> fail "unreachable"
