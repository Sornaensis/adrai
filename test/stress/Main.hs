{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Adrai.FixtureContractTest
import qualified Adrai.FixturePlanTest
import qualified Adrai.FixtureProperties
import qualified Adrai.LargeRepositoryStressTest
import qualified Adrai.RetrievalScaleTest
import System.Environment (getArgs, withArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = do
  arguments <- getArgs
  case break (== "--run-stress") arguments of
    (_, []) -> do
      hPutStrLn stderr "adrai-stress-test requires the explicit --run-stress opt-in before running stress fixtures"
      exitWith (ExitFailure 64)
    (before, _ : after) -> normalMain (before <> after)

normalMain :: [String] -> IO ()
normalMain arguments =
  case arguments of
    -- `stack test --test-arguments=...` passes a legacy maintenance mode to
    -- every enabled component.  The ordinary suite owns these modes; making
    -- them explicit no-ops here keeps the stress component safe and lets the
    -- bare invocation retain its established single-owner behavior.
    ["--write-p3-01-goldens"] -> pure ()
    ["--write-p3-02-goldens"] -> pure ()
    ["--write-p3-03-goldens"] -> pure ()
    ["--write-p3-04-goldens"] -> pure ()
    ["--write-p3-05-goldens"] -> pure ()
    ["--write-p3-06-goldens"] -> pure ()
    ["--write-p4-03-goldens"] -> pure ()
    ["--coverage-ledger-report"] -> pure ()
    ["--require-coverage-ledger-closed"] -> pure ()
    _ -> withArgs arguments (defaultMain tests)

tests :: TestTree
tests =
  testGroup
    "ADRAI stress"
    [ Adrai.RetrievalScaleTest.tests,
      Adrai.LargeRepositoryStressTest.tests,
      Adrai.FixtureProperties.tests,
      Adrai.FixturePlanTest.tests,
      Adrai.FixtureContractTest.tests
    ]
