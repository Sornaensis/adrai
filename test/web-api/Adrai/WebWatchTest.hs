module Adrai.WebWatchTest (tests) where

import qualified Adrai.WebServerTest as Server
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests = testCase "fact watcher verifies and retries bounded changes" Server.testWatchRuntime
