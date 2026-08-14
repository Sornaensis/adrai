{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Adrai.Provenance.LockTest (tests) where

import Adrai.Provenance.Lock
  ( OverlayLock (..),
    acquireOverlayLock,
    acquireOverlayLockWith,
    acquireOverlayLockWithToken,
    releaseOverlayLock,
    releaseOverlayLockWith,
    withOverlayLock,
    withOverlayLockWithReleaseHook,
  )
import Adrai.Provenance.Overlay
  ( createOverlaySchema,
    overlayValid,
    overlayValidWith,
    overlayValidWithCleanup,
  )
import Control.Concurrent (forkIO, threadDelay, throwTo)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Exception
  ( AsyncException (ThreadKilled),
    SomeException,
    fromException,
    throwIO,
    try,
  )
import Control.Monad (forM_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple
  ( Connection,
    Only (..),
    execute_,
    open,
    query_,
    close,
  )
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
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
        , testCase "Returns Nothing when already held" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "test_provenance.sqlite"
              result1 <- acquireOverlayLock dbPath
              assertBool "first acquire should succeed" (isJust result1)
              case result1 of
                Just lock -> do
                  result2 <- acquireOverlayLock dbPath
                  assertBool "second acquire should return Nothing" (not (isJust result2))
                  inspection <- open (lockPath lock)
                  rows <- query_ inspection "SELECT holder_pid FROM overlay_lock" :: IO [Only Text]
                  close inspection
                  rows @?= [Only (lockHolderPid lock)]
                  releaseOverlayLock lock
        , testCase "Lock DB path is correct" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath   = tmpDir </> "subdir" </> "provenance.sqlite"
                  expected = tmpDir </> "subdir" </> "provenance.sqlite" </> ".lock.sqlite"
              result <- acquireOverlayLock dbPath
              case result of
                Just lock@OverlayLock{..} -> do
                  lockPath @?= expected
                  releaseOverlayLock lock
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
        , testCase "Multiple acquire/release cycles work" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "provenance.sqlite"
              forM_ [1 :: Int .. 5] $ \i -> do
                result <- acquireOverlayLock dbPath
                assertBool ("cycle " <> show i <> " should succeed") (isJust result)
                case result of
                  Just lock -> releaseOverlayLock lock
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
        , testCase "An ignored insert with the same token cannot claim or delete the owner" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "provenance.sqlite"
                  identicalToken = "identical-attempt-token"
              owner <- acquireOverlayLockWithToken dbPath identicalToken (pure ())
              assertBool "first identical token must acquire" (isJust owner)
              contender <- acquireOverlayLockWithToken dbPath identicalToken (pure ())
              assertBool "ignored same-token insert must not acquire" (not (isJust contender))
              case owner of
                Nothing -> assertFailure "same-token fixture lost its owner"
                Just lock -> do
                  inspection <- open (lockPath lock)
                  rows <- query_ inspection "SELECT holder_pid FROM overlay_lock" :: IO [Only Text]
                  close inspection
                  rows @?= [Only identicalToken]
                  releaseOverlayLock lock
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
        , testCase "Multiple sequential withOverlayLock calls work" $
            withSystemTempDirectory "adrai_lock_test" $ \tmpDir -> do
              let dbPath = tmpDir </> "provenance.sqlite"
              forM_ [1 :: Int .. 5] $ \_ -> do
                _ <- withOverlayLock dbPath (pure 1)
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
