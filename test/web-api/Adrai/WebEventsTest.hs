module Adrai.WebEventsTest (tests, livenessTest) where

import qualified Adrai.WebServerTest as Server
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests = testCase "authenticated bounded websocket runtime" Server.testEventRuntime

livenessTest :: TestTree
livenessTest = testCase "idle pong liveness is isolated and silent leases are released" Server.testIdleEventLiveness
