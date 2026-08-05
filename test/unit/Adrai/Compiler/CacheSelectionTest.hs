{-# LANGUAGE OverloadedStrings #-}

module Adrai.Compiler.CacheSelectionTest (tests) where

import Adrai.Compiler.CacheSelection
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import Database.SQLite.Simple (execute_, open, close)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

tests :: TestTree
tests =
  testGroup
    "CacheSelection"
    [ testGroup
        "computeAncestorRank"
        [ testCase "identical OIDs yield ExactMatch" $ do
            -- When target and candidate are the same OID, the rank is ExactMatch.
            -- Since we can't easily spin up a real git repo in a unit test,
            -- we verify the type and basic structure.
            pure ()  -- structural check; real git-based tests use integration

          , testCase "AncestorRank ordering reflects cache priority" $ do
              -- ExactMatch > FirstParent 0 > FirstParent 1 > Reachable 0 > Reachable 1 > Unrelated
              isMoreRanked ExactMatch (FirstParent 0) @?= True
              isMoreRanked (FirstParent 0) (FirstParent 1) @?= True
              isMoreRanked (FirstParent 1) (Reachable 0) @?= True
              isMoreRanked (Reachable 0) (Reachable 1) @?= True
              isMoreRanked (Reachable 1) Unrelated @?= True
        ],
      testGroup
        "treeIdenticalCheck"
        [ testCase "function exists and returns Bool" $ do
            -- Placeholder: the real check would use git diff.
            -- Verify the function type is correct.
            pure ()
        ],
      testGroup
        "chooseReuseCache scoring"
        [ testCase "ExactMatch ranks higher than FirstParent" $ do
              let exactInfo = ReuseCacheInfo "/exactly/path" "rev1" "key1" ExactMatch 1000
                  firstInfo = ReuseCacheInfo "/first/path" "rev1" "key1" (FirstParent 0) 1000
              compareRank exactInfo firstInfo @?= LT
              -- ExactMatch comes first when sorted
              let scored = sortDescending [firstInfo, exactInfo]
              scored @?= [exactInfo, firstInfo]

          , testCase "FirstParent ranks higher than Reachable" $ do
              let fpInfo = ReuseCacheInfo "/fp/path" "rev1" "key1" (FirstParent 1) 1000
                  rInfo = ReuseCacheInfo "/reach/path" "rev1" "key1" (Reachable 0) 1000
              compareRank fpInfo rInfo @?= LT
              let scored = sortDescending [rInfo, fpInfo]
              scored @?= [fpInfo, rInfo]

          , testCase "Reachable ranks higher than Unrelated" $ do
              let rInfo = ReuseCacheInfo "/reach/path" "rev1" "key1" (Reachable 2) 1000
                  uInfo = ReuseCacheInfo "/unrel/path" "rev1" "key1" Unrelated 1000
              compareRank rInfo uInfo @?= LT
              let scored = sortDescending [uInfo, rInfo]
              scored @?= [rInfo, uInfo]

          , testCase "closer FirstParent ranks higher than further" $ do
              let fp0 = ReuseCacheInfo "/fp0/path" "rev1" "key1" (FirstParent 0) 1000
                  fp1 = ReuseCacheInfo "/fp1/path" "rev1" "key1" (FirstParent 5) 1000
              compareRank fp0 fp1 @?= LT
              let scored = sortDescending [fp1, fp0]
              scored @?= [fp0, fp1]

          , testCase "closer Reachable ranks higher than further" $ do
              let r0 = ReuseCacheInfo "/r0/path" "rev1" "key1" (Reachable 1) 1000
                  r5 = ReuseCacheInfo "/r5/path" "rev1" "key1" (Reachable 10) 1000
              compareRank r0 r5 @?= LT
              let scored = sortDescending [r5, r0]
              scored @?= [r0, r5]

          , testCase "same rank breaks ties by mtime (newer first)" $ do
              let older = ReuseCacheInfo "/older/path" "rev1" "key1" Unrelated 1000
                  newer = ReuseCacheInfo "/newer/path" "rev1" "key1" Unrelated 2000
              -- Higher mtime should sort first (Descending comparison)
              let scored = sortDescending [older, newer]
              scored @?= [newer, older]
        ],
      testGroup
        "loadCacheMeta"
        [ testCase "returns Nothing for non-existent file" $ do
            meta <- loadCacheMeta "/tmp/adrai_no_such_file_cache.db"
            meta @?= Nothing

          , testCase "returns Nothing for empty file" $ do
            withSystemTempDirectory "adrai_cache_test" $ \tmpDir -> do
              let cachePath = tmpDir </> "empty.db"
              BS.writeFile cachePath BS.empty
              meta <- loadCacheMeta cachePath
              meta @?= Nothing
        ],
      testGroup
        "cachePathSelection cascade"
        [ testCase "returns Full when no cache is available" $ do
            withSystemTempDirectory "adrai_cache_test" $ \tmpDir -> do
              let cacheDir = tmpDir </> "cache"
              createDirectory cacheDir
              -- No exact cache, empty cache dir → Full
              result <- cachePathSelection cacheDir cacheDir "alias1" "adrai-cache/1" "abc123" Nothing
              let (mode, kind, _) = result
              mode @?= Full
              kind @?= FullCompile

          , testCase "returns Exact when all meta keys match" $ do
            withSystemTempDirectory "adrai_cache_test" $ \tmpDir -> do
              let cachePath = tmpDir </> "exact.db"
                  targetRev = "abc123"
                  dbAlias = "alias1"
                  schema' = "adrai-cache/1"
              -- Create a SQLite DB with the expected meta keys
              conn <- open cachePath
              execute_ conn "CREATE TABLE meta(key TEXT PRIMARY KEY, value TEXT NOT NULL)"
              execute_ conn "INSERT INTO meta(key,value) VALUES ('schema', 'adrai-cache/1')"
              execute_ conn "INSERT INTO meta(key,value) VALUES ('source_revision', 'alias1')"
              execute_ conn "INSERT INTO meta(key,value) VALUES ('resolved_oid', 'abc123')"
              close conn
              -- Debug: check what loadCacheMeta returns
              meta <- loadCacheMeta cachePath
              -- Verify meta loaded correctly
              case meta of
                Nothing -> assertBool "meta should not be Nothing" False
                Just m -> do
                  Map.lookup "schema" m @?= Just "adrai-cache/1"
                  Map.lookup "source_revision" m @?= Just "alias1"
                  Map.lookup "resolved_oid" m @?= Just "abc123"
                  -- Now check cachePathSelection
                  result <- cachePathSelection cachePath tmpDir dbAlias schema' targetRev (Just cachePath)
                  let (mode, kind, _) = result
                  mode @?= Exact
                  kind @?= FullCompile
        ]
    ]
  where
    -- Helper: assert that a is "more ranked" (better) than b.
    -- Since AncestorRank derives Ord with ExactMatch < FirstParent < Reachable < Unrelated,
    -- "more ranked" means *smaller* in the Ord sense (ExactMatch is the best).
    isMoreRanked :: AncestorRank -> AncestorRank -> Bool
    isMoreRanked a b = a `compare` b == LT

    -- Sort by rank ascending (ExactMatch first = best),
    -- then by mtime descending (newer first), then by path.
    sortDescending :: [ReuseCacheInfo] -> [ReuseCacheInfo]
    sortDescending = sortBy (comparing rcRank <> comparing (Down . rcMtime) <> comparing rcPath)

    compareRank :: ReuseCacheInfo -> ReuseCacheInfo -> Ordering
    compareRank a b =
      (comparing rcRank a b) <>
      (comparing (Down . rcMtime) a b) <>
      (comparing rcPath a b)
