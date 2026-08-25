module Adrai.Benchmark.ProfileDriver (main) where

import Adrai.Benchmark.CurrentSearch (CurrentSearchFixture, currentSearchWarm, withCurrentSearchWarmFixture)
import Control.Exception (evaluate)
import Control.Monad (when)
import Data.ByteString qualified as ByteString
import Data.Word (Word64)
import Debug.Trace (traceMarkerIO)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs)
import System.Exit (die, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Mem (performMajorGC)
import Text.Read (readMaybe)

data Workload = CurrentSearchWarm2000

-- | The default preserves the original one-action attribution contract.  The
-- batch and fixture-only modes are an explicitly paired diagnostic: their
-- setup, GC boundary, marker cadence, and iteration count match, while only
-- the action batch calls the public current-search operation.
data ProfileMode
  = SingleAction
  | ActionBatch Int
  | FixtureOnlyControl Int

data ProfileRequest = ProfileRequest
  { profileWorkload :: Workload,
    profileMode :: ProfileMode
  }

main :: IO ()
main = do
  setLocaleEncoding utf8
  arguments <- getArgs
  case parseRequest arguments of
    Left usage -> die usage
    Right Nothing -> putStrLn usageText >> exitSuccess
    Right (Just request) -> runProfileRequest request

-- | The driver intentionally owns exactly one selected production action.  It
-- has no Criterion import, calibration, or sample loop, so GHC profile output
-- attributes the warm search rather than benchmark-framework work.
runProfileRequest :: ProfileRequest -> IO ()
runProfileRequest request =
  case profileWorkload request of
    CurrentSearchWarm2000 ->
      case profileMode request of
        SingleAction -> runCurrentSearchWarmProfile
        ActionBatch iterations -> runCurrentSearchWarmActionBatch iterations
        FixtureOnlyControl iterations -> runCurrentSearchWarmFixtureControl iterations

-- | Keep this path byte-for-byte equivalent in shape to the original public
-- command: one prepared fixture, one GC, two action markers, and exactly one
-- fully forced warm action.
runCurrentSearchWarmProfile :: IO ()
runCurrentSearchWarmProfile =
  withCurrentSearchWarmFixture $ \fixture -> do
    hPutStrLn stderr "adrai-profile: setup complete; running one current-warm-2000-adr action"
    performMajorGC
    traceMarkerIO "adrai-profile: current-warm-2000-adr action-start"
    forceCurrentSearchWarmAction fixture
    traceMarkerIO "adrai-profile: current-warm-2000-adr action-complete"
    hPutStrLn stderr "adrai-profile: completed one current-warm-2000-adr action"

-- | Attribute a sustained sequence of exactly the existing public warm-search
-- action.  Each timing begins after its start marker and ends before its
-- completion marker, so marker I/O is not counted as action time.
runCurrentSearchWarmActionBatch :: Int -> IO ()
runCurrentSearchWarmActionBatch iterations =
  withCurrentSearchWarmFixture $ \fixture -> do
    logSetup "action batch" iterations
    performMajorGC
    traceMarkerIO "adrai-profile: current-warm-2000-adr action-batch-start"
    runIterations iterations $ \iteration -> do
      traceMarkerIO (iterationMarker "action" iteration "start")
      started <- getMonotonicTimeNSec
      forceCurrentSearchWarmAction fixture
      completed <- getMonotonicTimeNSec
      traceMarkerIO (iterationMarker "action" iteration "complete")
      logIterationDuration "action" iteration started completed
    traceMarkerIO "adrai-profile: current-warm-2000-adr action-batch-complete"
    hPutStrLn stderr "adrai-profile: completed current-warm-2000-adr action batch"

-- | This is the paired control for 'runCurrentSearchWarmActionBatch'.  It
-- retains the identical fixture-ready, major-GC, and marker boundaries, but
-- does not invoke currentSearchWarm.  Its timings quantify driver/marker loop
-- overhead rather than production search work.
runCurrentSearchWarmFixtureControl :: Int -> IO ()
runCurrentSearchWarmFixtureControl iterations =
  withCurrentSearchWarmFixture $ \_fixture -> do
    logSetup "fixture-only control" iterations
    performMajorGC
    traceMarkerIO "adrai-profile: current-warm-2000-adr fixture-control-start"
    runIterations iterations $ \iteration -> do
      traceMarkerIO (iterationMarker "fixture-control" iteration "start")
      started <- getMonotonicTimeNSec
      completed <- getMonotonicTimeNSec
      traceMarkerIO (iterationMarker "fixture-control" iteration "complete")
      logIterationDuration "fixture-control" iteration started completed
    traceMarkerIO "adrai-profile: current-warm-2000-adr fixture-control-complete"
    hPutStrLn stderr "adrai-profile: completed current-warm-2000-adr fixture-only control"

forceCurrentSearchWarmAction :: CurrentSearchFixture -> IO ()
forceCurrentSearchWarmAction fixture = do
  output <- currentSearchWarm fixture
  outputLength <- evaluate (ByteString.length output)
  when (outputLength == 0) (die "adrai-profile: warm search returned an empty public projection")

runIterations :: Int -> (Int -> IO ()) -> IO ()
runIterations iterations runIteration = mapM_ runIteration [1 .. iterations]

logSetup :: String -> Int -> IO ()
logSetup mode iterations =
  hPutStrLn stderr ("adrai-profile: setup complete; running " <> show iterations <> " current-warm-2000-adr " <> mode <> " iterations")

iterationMarker :: String -> Int -> String -> String
iterationMarker mode iteration boundary =
  "adrai-profile: current-warm-2000-adr " <> mode <> "-" <> show iteration <> "-" <> boundary

logIterationDuration :: String -> Int -> Word64 -> Word64 -> IO ()
logIterationDuration mode iteration started completed =
  hPutStrLn stderr
    ( "adrai-profile: current-warm-2000-adr "
        <> mode
        <> "-"
        <> show iteration
        <> " duration-ns="
        <> show (completed - started)
    )

parseRequest :: [String] -> Either String (Maybe ProfileRequest)
parseRequest arguments =
  case arguments of
    ["--help"] -> Right Nothing
    ["--workload", "current-warm-2000-adr"] -> Right (Just (ProfileRequest CurrentSearchWarm2000 SingleAction))
    ["--workload", "current-warm-2000-adr", "--mode", "action-batch", "--iterations", iterations] ->
      Just . ProfileRequest CurrentSearchWarm2000 . ActionBatch <$> parsePositiveIterations iterations
    ["--workload", "current-warm-2000-adr", "--mode", "fixture-control", "--iterations", iterations] ->
      Just . ProfileRequest CurrentSearchWarm2000 . FixtureOnlyControl <$> parsePositiveIterations iterations
    _ -> Left usageText

parsePositiveIterations :: String -> Either String Int
parsePositiveIterations value =
  case readMaybe value of
    Just iterations | iterations > 0 -> Right iterations
    _ -> Left ("--iterations must be a positive decimal integer\n\n" <> usageText)

usageText :: String
usageText =
  unlines
    [ "Usage: adrai-profile --workload current-warm-2000-adr [--mode action-batch|fixture-control --iterations N] [+RTS ... -RTS]",
      "",
      "This criterion-free driver prepares the deterministic warm fixture once,",
      "then runs and fully forces exactly one production current-search action.",
      "The optional action-batch mode times N fully forced public actions;",
      "fixture-control is its matching fixture-only, marker/GC control."
    ]
