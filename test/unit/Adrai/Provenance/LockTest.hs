{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Adrai.Provenance.LockTest (tests, holdSessionForTest) where

import Adrai.Provenance.Lock
  ( OverlayLock (..),
    acquireOverlayLock,
    acquireOverlayLockWith,
    acquireOverlayLockWithToken,
    releaseOverlayLock,
    releaseOverlayLockWith,
    withOverlayLock,
    withOverlayLockWithReleaseHook,
    withOverlayLockWaiting,
    withOverlayLockWaitingWithRetryHook,
  )
import Adrai.Provenance.Overlay
  ( createOverlaySchema,
    overlayValid,
    overlayValidWith,
    overlayValidWithCleanup,
  )
import Control.Concurrent (forkIO, threadDelay, throwTo)
import Control.Concurrent.Async (cancelWith, race, wait, waitCatch, withAsync)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, takeMVar, tryPutMVar)
import Control.Exception
  ( AsyncException (ThreadKilled),
    SomeException,
    bracket,
    fromException,
    finally,
    throwIO,
    try,
  )
import Control.Monad (forM_, void, when)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Database.SQLite.Simple
  ( Only (..),
    execute_,
    open,
    query_,
    close,
  )
import System.FilePath ((</>))
import System.Environment (getExecutablePath)
import System.IO (hClose, hFlush, hGetLine, stdout)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), StdStream (CreatePipe), createProcess, getProcessExitCode, proc, terminateProcess, waitForProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)

tests :: TestTree
tests =
  testGroup "OverlayLock"
    [ testGroup
        "acquireOverlayLock"
        [ testCase "Returns Lock for new database" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "test_provenance.sqlite"
              result <- acquireOverlayLock dbPath
              assertBool "should acquire lock on new db" (isJust result)
              case result of
                Just lock -> releaseOverlayLock lock
                Nothing -> assertFailure "new database fixture must acquire the lock"
        , testCase "Returns Nothing when already held" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "test_provenance.sqlite"
              bracket (acquireOverlayLock dbPath) (mapM_ releaseOverlayLock) $ \result1 -> do
                assertBool "first acquire should succeed" (isJust result1)
                case result1 of
                  Just lock ->
                    bracket (acquireOverlayLock dbPath) (mapM_ releaseOverlayLock) $ \result2 -> do
                      assertBool "second acquire should return Nothing" (not (isJust result2))
                      rows <- query_ (lockConnection lock) "SELECT holder_pid FROM overlay_lock" :: IO [Only Text]
                      rows @?= [Only (lockHolderPid lock)]
                  Nothing -> assertFailure "first acquisition must succeed"
        , testCase "Lock DB path is correct" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath   = tmpDir </> "subdir" </> "provenance.sqlite"
                  expected = tmpDir </> "subdir" </> "provenance.sqlite" </> ".lock.sqlite"
              result <- acquireOverlayLock dbPath
              case result of
                Just lock@OverlayLock{..} -> do
                  lockPath @?= expected
                  releaseOverlayLock lock
                Nothing -> assertFailure "path fixture must acquire the lock"
        , testCase "Lock table is created" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "provenance.sqlite"
              result <- acquireOverlayLock dbPath
              case result of
                Just lock@OverlayLock{..} -> do
                  conn <- open lockPath
                  tables <- query_ conn "SELECT name FROM sqlite_master WHERE type='table' AND name='overlay_lock'"
                    :: IO [Only Text]
                  close conn
                  assertBool "overlay_lock table should exist" (tables == [Only "overlay_lock"])
                  releaseOverlayLock lock
                Nothing -> assertFailure "table fixture must acquire the lock"
        , testCase "Multiple acquire/release cycles work" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "provenance.sqlite"
              forM_ [1 :: Int .. 5] $ \i -> do
                result <- acquireOverlayLock dbPath
                assertBool ("cycle " <> show i <> " should succeed") (isJust result)
                case result of
                  Just lock -> releaseOverlayLock lock
                  Nothing -> assertFailure "each acquisition cycle must succeed"
        , testCase "Cancellation after singleton claim rolls back and permits retry" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "provenance.sqlite"
              cancelled <- try @SomeException (acquireOverlayLockWith dbPath (throwIO ThreadKilled))
              case cancelled of
                Left problem -> fromException problem @?= Just ThreadKilled
                Right _ -> assertFailure "initial acquisition must rethrow ThreadKilled"
              retried <- acquireOverlayLock dbPath
              assertBool "cancelled acquisition must not publish a row" (isJust retried)
              forM_ retried releaseOverlayLock
        , testCase "Simultaneous acquisitions publish exactly one owner" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "provenance.sqlite"
              start <- newEmptyMVar
              finished <- newEmptyMVar
              forM_ [1 :: Int, 2] $ \_ -> do
                _ <- forkIO $ readMVar start >> acquireOverlayLock dbPath >>= putMVar finished
                pure ()
              putMVar start ()
              first <- takeMVar finished
              second <- takeMVar finished
              length (filter isJust [first, second]) @?= 1
              forM_ first releaseOverlayLock
              forM_ second releaseOverlayLock
        , testCase "Waits for lock-database startup write instead of surfacing SQLite busy" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "provenance.sqlite"
              initialized <- acquireOverlayLock dbPath
              lockDb <- case initialized of
                Nothing -> assertFailure "startup fixture must acquire the lock" >> fail "unreachable"
                Just lock -> do
                  let path = lockPath lock
                  releaseOverlayLock lock
                  pure path
              writer <- open lockDb
              execute_ writer "BEGIN IMMEDIATE"
              finished <- newEmptyMVar
              _ <- forkIO $ do
                outcome <- try @SomeException (withOverlayLock dbPath (pure ()))
                putMVar finished outcome
              -- The contender has opened the same lock DB while this
              -- transaction prevents its schema/claim write.  Releasing the
              -- writer must let the configured SQLite busy wait complete.
              threadDelay 200000
              execute_ writer "COMMIT"
              close writer
              outcome <- takeMVar finished
              case outcome of
                Left problem -> assertFailure ("contended lock acquisition failed: " <> show problem)
                Right () -> pure ()
        , testCase "The same token cannot claim or release another writer owner" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "provenance.sqlite"
                  identicalToken = "identical-attempt-token"
              bracket (acquireOverlayLockWithToken dbPath identicalToken (pure ())) (mapM_ releaseOverlayLock) $ \owner -> do
                assertBool "first identical token must acquire" (isJust owner)
                bracket (acquireOverlayLockWithToken dbPath identicalToken (pure ())) (mapM_ releaseOverlayLock) $ \contender -> do
                  assertBool "same-token contender must not acquire" (not (isJust contender))
                  case owner of
                    Nothing -> assertFailure "same-token fixture lost its owner"
                    Just lock -> do
                      rows <- query_ (lockConnection lock) "SELECT holder_pid FROM overlay_lock" :: IO [Only Text]
                      rows @?= [Only identicalToken]
    ]
    , testGroup "session lifetime"
        [ testCase "mixed bounded and waiting callers hand off beyond the bounded retry count" testMixedHandoff
        , testCase "waiting owner and contender cancellation release only owned authority" testWaitingCancellation
        , testCase "an independently terminated owner releases its writer session" testTerminatedOwner
        , testCase "committed unknown ownership is refused finitely and preserved" testCommittedOwner
        ]
    , testGroup
        "releaseOverlayLock"
        [ testCase "Safe to call multiple times" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "provenance.sqlite"
              result <- acquireOverlayLock dbPath
              case result of
                Just lock -> do
                  releaseOverlayLock lock
                  releaseOverlayLock lock
                  releaseOverlayLock lock
                  pure ()
                Nothing -> assertFailure "should have acquired lock"
        , testCase "Releases lock for re-acquisition" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "provenance.sqlite"
              result1 <- acquireOverlayLock dbPath
              assertBool "first acquire succeeds" (isJust result1)
              case result1 of
                Just lock -> do
                  releaseOverlayLock lock
                  result2 <- acquireOverlayLock dbPath
                  assertBool "re-acquire should succeed" (isJust result2)
                  case result2 of
                    Just lock2 -> releaseOverlayLock lock2
                    Nothing -> assertFailure "re-acquisition must succeed"
                Nothing -> assertFailure "first acquisition must succeed"
        , testCase "Release cancellation closes and removes the owned row before rethrow" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "provenance.sqlite"
              acquired <- acquireOverlayLock dbPath
              case acquired of
                Nothing -> assertFailure "release fixture must acquire the lock"
                Just lock -> do
                  cancelled <- try @SomeException (releaseOverlayLockWith lock (throwIO ThreadKilled))
                  case cancelled of
                    Left problem -> fromException problem @?= Just ThreadKilled
                    Right _ -> assertFailure "release must rethrow ThreadKilled after cleanup"
                  retried <- acquireOverlayLock dbPath
                  assertBool "cancelled release must not strand the row" (isJust retried)
                  forM_ retried releaseOverlayLock
        , testCase "A genuine first-release failure is reported" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "provenance.sqlite"
              acquired <- acquireOverlayLock dbPath
              case acquired of
                Nothing -> assertFailure "release failure fixture must acquire the lock"
                Just lock -> do
                  close (lockConnection lock)
                  outcome <- try @SomeException (releaseOverlayLock lock)
                  assertBool "closed-connection release must not report success" (isLeft outcome)
                  cleanup <- open (lockPath lock)
                  execute_ cleanup "DELETE FROM overlay_lock"
                  close cleanup
    ]

    , testGroup
        "withOverlayLock"
        [ testCase "Action runs with lock held" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "provenance.sqlite"
              ref <- newIORef (0 :: Int)
              _ <- withOverlayLock dbPath (writeIORef ref 99)
              value <- readIORef ref
              value @?= 99
        , testCase "Releases lock on action failure" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "provenance.sqlite"
              ref <- newIORef True
              result <- try @SomeException $
                withOverlayLock dbPath (writeIORef ref False >> throwIO (userError "test error"))
              assertBool "should have thrown" (isLeft result)
              canAcquire <- acquireOverlayLock dbPath
              assertBool "should be able to re-acquire after failure" (isJust canAcquire)
              case canAcquire of
                Just lock -> releaseOverlayLock lock
                Nothing -> assertFailure "failed action must release the lock"
        , testCase "Multiple sequential withOverlayLock calls work" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "provenance.sqlite"
              forM_ [1 :: Int .. 5] $ \_ -> do
                _ <- withOverlayLock dbPath (pure (1 :: Int))
                pure ()
              pure ()
        , testCase "Action cancellation is rethrown after releasing the lock" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "provenance.sqlite"
              result <- try @SomeException (withOverlayLock dbPath (throwIO ThreadKilled))
              case result of
                Left problem -> fromException problem @?= Just ThreadKilled
                Right _ -> assertFailure "ThreadKilled must escape withOverlayLock"
              canAcquire <- acquireOverlayLock dbPath
              assertBool "cancelled action must release the lock" (isJust canAcquire)
              forM_ canAcquire releaseOverlayLock
        , testCase "Action cancellation outranks a synchronous release failure" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "provenance.sqlite"
              outcome <- try @SomeException
                (withOverlayLockWithReleaseHook dbPath (throwIO ThreadKilled) (throwIO (userError "release failure")))
              case outcome of
                Left problem -> fromException problem @?= Just ThreadKilled
                Right _ -> assertFailure "release failure must not replace action cancellation"
              retried <- acquireOverlayLock dbPath
              assertBool "combined failure must still release the lock" (isJust retried)
              forM_ retried releaseOverlayLock
        , testCase "Cancellation while waiting for a held lock is rethrown" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "provenance.sqlite"
              holder <- acquireOverlayLock dbPath
              case holder of
                Nothing -> assertFailure "holder must acquire the lock"
                Just held -> do
                  finished <- newEmptyMVar
                  waiter <- forkIO $ do
                    outcome <- try @SomeException (withOverlayLock dbPath (pure ()))
                    putMVar finished outcome
                  threadDelay 200000
                  throwTo waiter ThreadKilled
                  outcome <- takeMVar finished
                  case outcome of
                    Left problem -> fromException problem @?= Just ThreadKilled
                    Right _ -> assertFailure "contended ThreadKilled must not become LockTimeout"
                  releaseOverlayLock held
                  canAcquire <- acquireOverlayLock dbPath
                  assertBool "contended cancellation must not strand the lock" (isJust canAcquire)
                  forM_ canAcquire releaseOverlayLock
        , testCase "Held lock still produces the ordinary timeout" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "provenance.sqlite"
              holder <- acquireOverlayLock dbPath
              case holder of
                Nothing -> assertFailure "timeout fixture must acquire the lock"
                Just held -> do
                  outcome <- try @SomeException (withOverlayLock dbPath (pure ()))
                  case outcome of
                    Left problem -> show problem @?= "LockTimeout"
                    Right _ -> assertFailure "held lock must time out"
                  releaseOverlayLock held
    ]
    , testGroup
        "overlayValid"
        [ testCase "Validation cancellation is rethrown and the connection closes" $
            withSystemTempDirectory "adrai_overlay_valid_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "provenance.sqlite"
              connection <- open dbPath
              createOverlaySchema connection
              close connection
              cancelled <- try @SomeException (overlayValidWith dbPath (throwIO ThreadKilled))
              case cancelled of
                Left problem -> fromException problem @?= Just ThreadKilled
                Right _ -> assertFailure "overlayValid must not translate ThreadKilled to False"
              valid <- overlayValid dbPath
              valid @?= True
        , testCase "Validation cancellation outranks synchronous cleanup failure" $
            withSystemTempDirectory "adrai_overlay_valid_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "provenance.sqlite"
              connection <- open dbPath
              createOverlaySchema connection
              close connection
              cancelled <- try @SomeException
                (overlayValidWithCleanup dbPath (throwIO ThreadKilled) (throwIO (userError "cleanup failure")))
              case cancelled of
                Left problem -> fromException problem @?= Just ThreadKilled
                Right _ -> assertFailure "cleanup failure must not replace validation cancellation"
              valid <- overlayValid dbPath
              valid @?= True
        ]
    ]

-- | Check if a value is a Just
isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just _) = True

-- | Check if an Either is a Left
isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False

-- | Test-only child entrypoint, never a product command. The ready line proves
-- the writer transaction is held before the parent tests exclusion/termination.
holdSessionForTest :: FilePath -> IO ()
holdSessionForTest path = withOverlayLockWaiting path $ do
  putStrLn "overlay-session-held"
  hFlush stdout
  gate <- newEmptyMVar :: IO (MVar ())
  takeMVar gate

testMixedHandoff :: IO ()
testMixedHandoff = withSystemTempDirectory "overlay-handoff" $ \root -> do
  let path = root </> "overlay"
  entered <- newEmptyMVar
  release <- newEmptyMVar
  withAsync (withOverlayLock path (putMVar entered () >> takeMVar release)) $ \owner ->
    flip finally (void (tryPutMVar release ()) >> void (waitCatch owner)) $ do
      startup <- race (takeMVar entered) (wait owner)
      case startup of
        Left () -> pure ()
        Right () -> assertFailure "owner exited before entering its protected action"
      attempts <- newIORef (0 :: Int)
      let onBusy = do
            count <- atomicModifyIORef' attempts (\value -> (value + 1, value + 1))
            when (count == 51) (void (tryPutMVar release ()))
      outcome <- try @SomeException (withOverlayLockWaitingWithRetryHook path onBusy (pure ()))
      void (tryPutMVar release ())
      ownerResult <- waitCatch owner
      assertBool "bounded owner completed its protected action" (not (isLeft ownerResult))
      assertBool ("waiting handoff succeeds: " <> show outcome) (not (isLeft outcome))
      count <- readIORef attempts
      assertBool "handoff is causal after more than the old retry allowance" (count >= 51)

testWaitingCancellation :: IO ()
testWaitingCancellation = withSystemTempDirectory "overlay-cancellation" $ \root -> do
  let path = root </> "overlay"
  cancelled <- try @SomeException (withOverlayLockWaiting path (throwIO ThreadKilled))
  case cancelled of
    Left problem -> fromException problem @?= Just ThreadKilled
    Right () -> assertFailure "waiting owner cancellation must escape"
  bracket (acquireOverlayLock path) (mapM_ releaseOverlayLock) $ \acquired ->
    case acquired of
      Nothing -> assertFailure "cancelled owner must release its session"
      Just owner -> do
        busy <- newEmptyMVar
        withAsync (withOverlayLockWaitingWithRetryHook path (void (tryPutMVar busy ())) (assertFailure "contender must not enter")) $ \waiter -> do
          startup <- race (takeMVar busy) (wait waiter)
          case startup of
            Left () -> pure ()
            Right () -> assertFailure "contender exited before observing contention"
          cancelWith waiter ThreadKilled
          outcome <- waitCatch waiter
          case outcome of
            Left problem -> fromException problem @?= Just ThreadKilled
            Right () -> assertFailure "contender cancellation must escape"
          rows <- query_ (lockConnection owner) "SELECT holder_pid FROM overlay_lock" :: IO [Only Text]
          rows @?= [Only (lockHolderPid owner)]
  withOverlayLockWaiting path (pure ())

testTerminatedOwner :: IO ()
testTerminatedOwner = withSystemTempDirectory "overlay-process" $ \root -> do
  executable <- getExecutablePath
  let path = root </> "overlay"
  bracket (createProcess (proc executable ["--hold-overlay-session", path]) {std_out = CreatePipe})
    (\(_, output, _, process) -> flip finally (forM_ output hClose) $ do
      status <- getProcessExitCode process
      when (status == Nothing) (terminateProcess process)
      void (waitForProcess process)) $ \(_, output, _, process) -> do
        case output of
          Nothing -> assertFailure "lock child requires its ready pipe"
          Just pipe -> hGetLine pipe >>= (@?= "overlay-session-held")
        bracket (acquireOverlayLock path) (mapM_ releaseOverlayLock) $ \contender ->
          assertBool "independent owner excludes another connection" (not (isJust contender))
        terminateProcess process
        void (waitForProcess process)
        withOverlayLockWaiting path (pure ())

testCommittedOwner :: IO ()
testCommittedOwner = withSystemTempDirectory "overlay-unknown" $ \root -> do
  let path = root </> "overlay"
  database <- bracket (acquireOverlayLock path) (mapM_ releaseOverlayLock) $ \initialized ->
    case initialized of
      Nothing -> assertFailure "unknown-owner fixture must initialize" >> fail "unreachable"
      Just owner -> pure (lockPath owner)
  bracket (open database) close $ \connection ->
    execute_ connection "INSERT INTO overlay_lock(rowid,holder_pid,acquired_at) VALUES(1,'unknown-owner','unknown-time')"
  result <- try @SomeException (withOverlayLockWaiting path (assertFailure "unknown ownership cannot be acquired"))
  case result of
    Left problem -> show problem @?= "OverlayOwnershipUnknown"
    Right () -> assertFailure "unknown committed owner must be refused"
  bracket (open database) close $ \connection -> do
    rows <- query_ connection "SELECT holder_pid FROM overlay_lock" :: IO [Only Text]
    rows @?= [Only "unknown-owner"]
