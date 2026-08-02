{-# LANGUAGE OverloadedStrings #-}

module Adrai.StateTest (tests) where

import Adrai.State
import Adrai.Types
  ( ConnectionId,
    RecordId,
    mkConnectionId,
    mkRecordId,
    stateTokenText,
  )
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "state tokens"
    [ testCase "matches every frozen canonical payload and token" $
        mapM_ assertStateVector stateVectors
    ]

-- Exact coverage of every vector in contracts/v1/encoding/state-tokens.json.
stateVectors :: [StateVector]
stateVectors =
  [ StateVector
      []
      []
      []
      []
      "{\"domains\":[],\"records\":[],\"scopes\":[],\"statuses\":[]}"
      "Sr46U20dkx_Y3MSVvnEWQV6",
    StateVector
      ["R00000000000000000000000000"]
      []
      []
      []
      "{\"domains\":[],\"records\":[\"R00000000000000000000000000\"],\"scopes\":[],\"statuses\":[]}"
      "SlMfyfvA6ZxXyQEiuuxv0uf",
    StateVector
      [ "R00000000000000000000000002",
        "R00000000000000000000000001"
      ]
      ["C00000000000000000000000004"]
      ["C00000000000000000000000005"]
      ["C00000000000000000000000003"]
      "{\"domains\":[\"C00000000000000000000000003\"],\"records\":[\"R00000000000000000000000001\",\"R00000000000000000000000002\"],\"scopes\":[\"C00000000000000000000000004\"],\"statuses\":[\"C00000000000000000000000005\"]}"
      "SFy3F4Vhwpi24WeZt5pR091",
    StateVector
      [ "R00000000000000000000000000",
        "R00000000000000000000000000"
      ]
      []
      []
      []
      "{\"domains\":[],\"records\":[\"R00000000000000000000000000\",\"R00000000000000000000000000\"],\"scopes\":[],\"statuses\":[]}"
      "S6aXx4JmtUGreAQJjTfCM_g"
  ]

data StateVector = StateVector
  { vectorRecords :: [Text],
    vectorScopes :: [Text],
    vectorStatuses :: [Text],
    vectorDomains :: [Text],
    vectorPayload :: Text,
    vectorToken :: Text
  }

assertStateVector :: StateVector -> IO ()
assertStateVector vector = do
  heads <-
    StateHeads
      <$> traverse parseRecord (vectorRecords vector)
      <*> traverse parseConnection (vectorScopes vector)
      <*> traverse parseConnection (vectorStatuses vector)
      <*> traverse parseConnection (vectorDomains vector)
  canonicalStatePayload heads @?= vectorPayload vector
  stateTokenText (stateTokenForHeads heads) @?= vectorToken vector

parseRecord :: Text -> IO RecordId
parseRecord value = assertRight (mkRecordId value)

parseConnection :: Text -> IO ConnectionId
parseConnection value = assertRight (mkConnectionId value)

assertRight :: (Show problem) => Either problem value -> IO value
assertRight result =
  case result of
    Right value -> pure value
    Left problem -> assertFailure (show problem) >> fail "unreachable"
