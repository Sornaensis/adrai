{-# LANGUAGE OverloadedStrings #-}

module Adrai.TransactionTest (tests) where

import Adrai.Git (gitOidText)
import Adrai.Service.Transaction (parseSingleOidFromOutput)
import qualified Data.ByteString.Char8 as BS8
import Data.Either (isLeft)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "Transaction contract"
    [ testCase "Git plumbing accepts one bare 40- or 64-hex OID with trailing stdout framing" $
        mapM_ assertAccepted acceptedOutputs,
      testCase "Git plumbing rejects non-bare or malformed OID output" $
        mapM_ assertRejected rejectedOutputs
    ]

assertAccepted :: (String, BS8.ByteString, Text.Text) -> IO ()
assertAccepted (label, raw, expected) =
  assertEqual label (Right expected) (gitOidText <$> parseSingleOidFromOutput "write-tree" raw)

assertRejected :: (String, BS8.ByteString) -> IO ()
assertRejected (label, raw) =
  assertBool label (isLeft (parseSingleOidFromOutput "commit-tree" raw))

acceptedOutputs :: [(String, BS8.ByteString, Text.Text)]
acceptedOutputs =
  [ ("40 hex without terminator", oid40, oid40Text),
    ("40 hex with LF", oid40 <> "\n", oid40Text),
    ("40 hex with CRLF", oid40 <> "\r\n", oid40Text),
    ("40 hex with trailing spaces", oid40 <> "   ", oid40Text),
    ("40 hex with newline and trailing spaces", oid40 <> "\n  ", oid40Text),
    ("40 uppercase hex", BS8.map toUpperAscii oid40 <> "\n", oid40Text),
    ("64 hex without terminator", oid64, oid64Text),
    ("64 hex with LF", oid64 <> "\n", oid64Text),
    ("64 hex with CRLF", oid64 <> "\r\n", oid64Text),
    ("64 hex with trailing spaces", oid64 <> "   ", oid64Text),
    ("64 uppercase hex", BS8.map toUpperAscii oid64 <> "\n", oid64Text)
  ]

rejectedOutputs :: [(String, BS8.ByteString)]
rejectedOutputs =
  [ ("empty", ""),
    ("whitespace only", " \r\n "),
    ("wrong length", BS8.take 39 oid40),
    ("non-hex", BS8.take 39 oid40 <> "g"),
    ("leading whitespace", " " <> oid40),
    ("leading tab", "\t" <> oid40),
    ("labeled output", "write-tree: " <> oid40),
    ("two OIDs separated by space", oid40 <> " " <> oid40),
    ("two OIDs separated by newline", oid40 <> "\n" <> oid40),
    ("bare carriage return terminator", oid40 <> "\r"),
    ("multiple newline terminators", oid40 <> "\n\n")
  ]

oid40 :: BS8.ByteString
oid40 = "0123456789abcdef0123456789abcdef01234567"

oid40Text :: Text.Text
oid40Text = "0123456789abcdef0123456789abcdef01234567"

oid64 :: BS8.ByteString
oid64 = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

oid64Text :: Text.Text
oid64Text = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

toUpperAscii :: Char -> Char
toUpperAscii character
  | character >= 'a' && character <= 'f' = toEnum (fromEnum character - 32)
  | otherwise = character
