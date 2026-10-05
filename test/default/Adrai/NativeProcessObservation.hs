{-# LANGUAGE ForeignFunctionInterface #-}

-- | Test-only observation of a helper pinned while it is held at readiness.
-- A retained handle/pidfd proves that helper's termination, not containment of
-- arbitrary descendants. Missing capability is an error, never absence proof.
module Adrai.NativeProcessObservation (withNativeObservation, withPinnedProcess, awaitNativeReadiness) where

import qualified Control.Concurrent.Async as Async
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, finally)
import Control.Monad (unless)
import Foreign.C.Types (CInt (..), CLLong (..))
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((</>))
import Text.Read (readMaybe)

newtype Observation = Observation CLLong

foreign import ccall unsafe "adrai_test_process_pin" pin :: CInt -> IO CLLong
foreign import ccall unsafe "adrai_test_process_exited" exited :: CLLong -> IO CInt
foreign import ccall unsafe "adrai_test_process_close" close :: CLLong -> IO CInt

acquire :: Int -> IO Observation
acquire pid = do
  handle <- pin (fromIntegral pid)
  if handle == -1 then fail "unable to pin the held native fixture (process observation capability unavailable)"
  else pure (Observation handle)

release :: Observation -> IO ()
release (Observation handle) = do
  result <- close handle
  unless (result == 0) (fail "native fixture observation close failed")

awaitExit :: Observation -> IO ()
awaitExit observation@(Observation handle) = do
  state <- exited handle
  case state of
    1 -> pure ()
    0 -> threadDelay 10000 >> awaitExit observation
    _ -> fail "retained native fixture observation failed"

withPinnedProcess :: Int -> IO value -> IO value
withPinnedProcess pid action = bracket (acquire pid) release $ \observation ->
  action `finally` awaitExit observation

-- Readiness is causal: a terminated operation cannot supply a later signal.
-- The outer retained owner supplies the diagnostic containment boundary.
awaitNativeReadiness :: Async.Async value -> String -> IO (Maybe ready) -> IO ready
awaitNativeReadiness worker label probe = do
  value <- probe
  case value of
    Just ready -> pure ready
    Nothing -> do
      outcome <- Async.poll worker
      case outcome of
        Just _ -> do
          _ <- Async.wait worker
          final <- probe
          maybe (fail (label <> ": operation completed before readiness")) pure final
        Nothing -> threadDelay 10000 >> awaitNativeReadiness worker label probe

-- The copied helper waits before its protocol at this gate. Pinning therefore
-- precedes its response or cancellation, without a post-exit PID lookup.
withNativeObservation :: FilePath -> IO value -> IO value
withNativeObservation directory action = do
  let enabled = directory </> "fixture-observer-enabled"
      ready = directory </> "fixture-observer-ready"
      identity = directory </> "fixture-observer.pid"
      removeIfPresent path = doesFileExist path >>= \exists -> if exists then removeFile path else pure ()
  bracket (writeFile enabled "enabled") (const (mapM_ removeIfPresent [enabled, ready, identity])) $ \() ->
    Async.withAsync action $ \worker -> do
      pid <- awaitIdentity worker identity
      bracket (acquire pid) release $ \observation ->
        (writeFile ready "pinned" >> Async.wait worker)
          `finally` (Async.cancel worker >> awaitExit observation)
  where
    awaitIdentity worker identity = do
      exists <- doesFileExist identity
      value <- if exists then readMaybe <$> readFile identity else pure Nothing
      case value of
        Just pid -> pure pid
        Nothing -> do
          outcome <- Async.poll worker
          case outcome of
            Just _ -> Async.wait worker >> fail "native fixture completed before its observation readiness gate"
            Nothing -> threadDelay 10000 >> awaitIdentity worker identity
