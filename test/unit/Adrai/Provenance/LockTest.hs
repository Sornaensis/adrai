{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Adrai.Provenance.LockTest (tests) where

import Adrai.Provenance.Lock
  ( OverlayLock (..),
    acquireOverlayLock,
    releaseOverlayLock,
    withOverlayLock,
  )
import Control.Exception (SomeException, throwIO, try)
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
                Just _lock -> do
                  result2 <- acquireOverlayLock dbPath
                  assertBool "second acquire should return Nothing" (not (isJust result2))
                  case result1 of
                    Just l -> releaseOverlayLock l
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
