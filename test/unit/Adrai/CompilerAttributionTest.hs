{-# LANGUAGE OverloadedStrings #-}

module Adrai.CompilerAttributionTest (tests) where

import Adrai.Compiler.Attribution
import Control.Exception (AsyncException (ThreadKilled), SomeException, throwIO, try)
import qualified Data.ByteString.Char8 as BS8
import Data.IORef (atomicModifyIORef', newIORef)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "P6-06G.5 compiler attribution"
    [ testCase "strict parser rejects order and elapsed violations" parserContract,
      testCase "caller-owned terminal evidence is durable and unique" evidenceContract,
      testCase "file observer checkpoints success and failure" lifecycleContract,
      testCase "enabled phases force values and record typed failures" forcingAndTypedFailureContract,
      testCase "observer write flush close and cancellation failures are fail-closed" observerFailureContract,
      testCase "inert observer preserves the action result" inertContract
    ]

parserContract :: IO ()
parserContract = do
  parseAttributionArtifact validArtifact @?= Right expectedArtifact
  parseAttributionArtifact postCloseRefreshArtifact
    @?= Right
      (AttributionArtifact
        [ AttributionStart 1 PostCloseProvenanceRefresh 20,
          AttributionEnd 2 PostCloseProvenanceRefresh 20 24 4 True
        ])
  parseAttributionArtifact postClosePipelineArtifact
    @?= Right
      (AttributionArtifact
        [ AttributionStart 1 CompileOutcomeEvaluation 30,
          AttributionEnd 2 CompileOutcomeEvaluation 30 31 1 True,
          AttributionStart 3 PostCloseProvenanceRefresh 31,
          AttributionEnd 4 PostCloseProvenanceRefresh 31 34 3 True,
          AttributionStart 5 PostCloseFingerprintValidation 34,
          AttributionEnd 6 PostCloseFingerprintValidation 34 39 5 True
        ])
  assertLeft "out-of-order sequence" (parseAttributionArtifact (replace "end\t3" "end\t4" validArtifact))
  assertLeft "wrong elapsed arithmetic" (parseAttributionArtifact (replace "\t5\tok" "\t6\tok" validArtifact))
  assertLeft "unfinished start" (parseAttributionArtifact "adrai-cold-compile-attribution-v1\nstart\t1\tclipreflight\t10\n")
  assertLeft "duplicate phase" (parseAttributionArtifact (validArtifact <> "start\t4\tclipreflight\t16\nend\t5\tclipreflight\t16\t17\t1\tok\n"))
  assertLeft "bounded rows" (parseAttributionArtifact ("adrai-cold-compile-attribution-v1\n" <> concat (replicate 129 "start\t1\tclipreflight\t1\n")))
  where
    expectedArtifact =
      AttributionArtifact
        [ AttributionStart 1 CliPreflight 10,
          AttributionCounterRow 2 CliPreflight CounterRows 3,
          AttributionEnd 3 CliPreflight 10 15 5 True
        ]
    validArtifact =
      "adrai-cold-compile-attribution-v1\n"
        <> "start\t1\tclipreflight\t10\n"
        <> "counter\t2\tclipreflight\trows\t3\n"
        <> "end\t3\tclipreflight\t10\t15\t5\tok\n"
    postCloseRefreshArtifact =
      "adrai-cold-compile-attribution-v1\n"
        <> "start\t1\tpostcloseprovenancerefresh\t20\n"
        <> "end\t2\tpostcloseprovenancerefresh\t20\t24\t4\tok\n"
    postClosePipelineArtifact =
      "adrai-cold-compile-attribution-v1\n"
        <> "start\t1\tcompileoutcomeevaluation\t30\n"
        <> "end\t2\tcompileoutcomeevaluation\t30\t31\t1\tok\n"
        <> "start\t3\tpostcloseprovenancerefresh\t31\n"
        <> "end\t4\tpostcloseprovenancerefresh\t31\t34\t3\tok\n"
        <> "start\t5\tpostclosefingerprintvalidation\t34\n"
        <> "end\t6\tpostclosefingerprintvalidation\t34\t39\t5\tok\n"

evidenceContract :: IO ()
evidenceContract =
  withSystemTempDirectory "adrai-attribution-evidence" $ \root -> do
    let output = root </> "cold.tsv"
    observer <- newFileColdCompileAttribution output
    withAttributionPhase observer CliPreflight (pure ())
    closeColdCompileAttribution observer
    appendAttributionEvidence output [("trace2_git_requests", 3), ("rts_max_residency_bytes", 42)]
    artifact <- BS8.unpack <$> BS8.readFile output
    case parseAttributionArtifact artifact of
      Right (AttributionArtifact rows) ->
        [name | AttributionEvidence _ name _ <- rows] @?= ["trace2_git_requests", "rts_max_residency_bytes"]
      Left problem -> assertFailure problem
    duplicate <- try (appendAttributionEvidence output [("trace2_git_requests", 4)]) :: IO (Either SomeException ())
    assertBool "terminal evidence cannot be appended twice" (either (const True) (const False) duplicate)

lifecycleContract :: IO ()
lifecycleContract =
  withSystemTempDirectory "adrai-attribution" $ \root -> do
    let output = root </> "cold.tsv"
    observer <- newFileColdCompileAttribution output
    withAttributionPhase observer CliPreflight $ recordAttributionCounter observer CounterRows 2
    failed <- try (withAttributionPhase observer PostCloseProvenanceRefresh (ioError (userError "expected failure"))) :: IO (Either SomeException ())
    case failed of
      Left _ -> pure ()
      Right () -> assertFailure "expected profiled failure"
    closeColdCompileAttribution observer
    artifact <- BS8.unpack <$> BS8.readFile output
    case parseAttributionArtifact artifact of
      Left problem -> assertFailure (problem <> ": " <> artifact)
      Right (AttributionArtifact rows) -> do
        let outcomes = [succeeded | AttributionEnd _ _ _ _ _ succeeded <- rows]
        [phase | AttributionEnd _ phase _ _ _ _ <- rows] @?= [CliPreflight, PostCloseProvenanceRefresh]
        outcomes @?= [True, False]

forcingAndTypedFailureContract :: IO ()
forcingAndTypedFailureContract =
  withSystemTempDirectory "adrai-attribution-forcing" $ \root -> do
    let output = root </> "cold.tsv"
    observer <- newFileColdCompileAttribution output
    forced <- try (withAttributionPhase observer CliPreflight (pure (error "enabled observer must force phase result" :: Int))) :: IO (Either SomeException Int)
    assertBool "enabled observer forces the phase result" (either (const True) (const False) forced)
    typed <- withAttributionEitherPhase observer AnalysisGate (pure (Left () :: Either () ()))
    typed @?= Left ()
    closeColdCompileAttribution observer
    artifact <- BS8.unpack <$> BS8.readFile output
    case parseAttributionArtifact artifact of
      Left problem -> assertFailure (problem <> ": " <> artifact)
      Right (AttributionArtifact rows) -> do
        let outcomes = [succeeded | AttributionEnd _ _ _ _ _ succeeded <- rows]
        outcomes @?= [False, False]

observerFailureContract :: IO ()
observerFailureContract =
  withSystemTempDirectory "adrai-attribution-failures" $ \root -> do
    let output = root </> "cold.tsv"
        failingWrite = defaultAttributionDependencies {attributionWriteLine = \_ _ -> ioError (userError "injected write failure")}
        failingClose = defaultAttributionDependencies {attributionClose = \_ -> ioError (userError "injected close failure")}
    writeFailed <- try (newFileColdCompileAttributionWith failingWrite output) :: IO (Either SomeException ColdCompileAttribution)
    assertBool "header write failure is visible" (either (const True) (const False) writeFailed)
    flushes <- newIORef (0 :: Int)
    let failingSecondFlush =
          defaultAttributionDependencies
            { attributionFlush = \handle -> do
                number <- atomicModifyIORef' flushes (\value -> let next = value + 1 in (next, next))
                if number == 2 then ioError (userError "injected flush failure") else attributionFlush defaultAttributionDependencies handle
            }
    observer <- newFileColdCompileAttributionWith failingSecondFlush output
    flushed <- try (withAttributionPhase observer CliPreflight (pure ())) :: IO (Either SomeException ())
    assertBool "phase flush failure is visible" (either (const True) (const False) flushed)
    closing <- newFileColdCompileAttributionWith failingClose (root </> "close.tsv")
    closeFailed <- try (closeColdCompileAttribution closing) :: IO (Either SomeException ())
    assertBool "close failure is visible" (either (const True) (const False) closeFailed)
    cancelled <- newFileColdCompileAttribution (root </> "cancelled.tsv")
    cancellation <- try (withAttributionPhase cancelled Verification (throwIO ThreadKilled)) :: IO (Either AsyncException ())
    cancellation @?= Left ThreadKilled
    closeColdCompileAttribution cancelled
    cancelledArtifact <- BS8.unpack <$> BS8.readFile (root </> "cancelled.tsv")
    case parseAttributionArtifact cancelledArtifact of
      Right (AttributionArtifact [AttributionStart _ Verification _, AttributionEnd _ Verification _ _ _ False]) -> pure ()
      other -> assertFailure ("cancellation must leave a failed closed phase, got " <> show other)

inertContract :: IO ()
inertContract = do
  value <- withAttributionPhase inertColdCompileAttribution CliPreflight (pure (41 :: Int))
  value @?= 41
  forced <- forceAttributionValue inertColdCompileAttribution (const (error "inert observer forced a value")) value
  forced @?= 41

assertLeft :: String -> Either String value -> IO ()
assertLeft label value =
  assertBool label $ case value of
    Left _ -> True
    Right _ -> False

replace :: String -> String -> String -> String
replace needle replacement source =
  case breakOn needle source of
    Nothing -> source
    Just (before, after) -> before <> replacement <> after

breakOn :: String -> String -> Maybe (String, String)
breakOn needle source
  | null needle = Just ("", source)
  | otherwise = go "" source
  where
    go _ [] = Nothing
    go before remaining
      | needle `prefixOf` remaining = Just (reverse before, drop (length needle) remaining)
      | character : rest <- remaining = go (character : before) rest
    prefixOf prefix value = take (length prefix) value == prefix
