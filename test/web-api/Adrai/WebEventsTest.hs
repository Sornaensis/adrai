module Adrai.WebEventsTest (tests) where

import qualified Adrai.WebServerTest as Server
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests = testCase "authenticated bounded websocket runtime" Server.testEventRuntime
