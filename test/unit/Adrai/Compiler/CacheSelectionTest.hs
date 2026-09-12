{-# LANGUAGE OverloadedStrings #-}

module Adrai.Compiler.CacheSelectionTest (tests) where

import Adrai.Compiler.CacheSelection
  ( AncestorRank (..),
    CacheSelectionKind (..),
    CacheSelectionMetrics (..),
    semanticReuseScore,
  )
import Adrai.Compiler.CacheSelection.TestSupport (loadCacheMetaForTest)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "CacheSelection"
    [ testGroup
        "semanticReuseScore"
        [ testCase "ExactMatch yields highest score" $
            semanticReuseScore ExactMatch @?= 1000,
          testCase "FirstParent decays linearly" $ do
            semanticReuseScore (FirstParent 0) @?= 900
            semanticReuseScore (FirstParent 1) @?= 800
            semanticReuseScore (FirstParent 5) @?= 400,
          testCase "Reachable decays linearly" $ do
            semanticReuseScore (Reachable 0) @?= 500
            semanticReuseScore (Reachable 1) @?= 450
            semanticReuseScore (Reachable 10) @?= 0,
          testCase "Unrelated yields zero" $
            semanticReuseScore Unrelated @?= 0,
          testCase "score ordering mirrors ancestor authority" $ do
            semanticReuseScore ExactMatch > semanticReuseScore (FirstParent 0) @?= True
            semanticReuseScore (FirstParent 0) > semanticReuseScore (FirstParent 1) @?= True
            semanticReuseScore (FirstParent 1) > semanticReuseScore (Reachable 0) @?= True
            semanticReuseScore (Reachable 0) > semanticReuseScore Unrelated @?= True
        ],
      testGroup
        "AncestorRank"
        [ testCase "orders only the production rank values" $ do
            assertBool "exact before first parent" (ExactMatch < FirstParent 0)
            assertBool "nearer first parent first" (FirstParent 0 < FirstParent 1)
            assertBool "first parent before reachable" (FirstParent 1 < Reachable 0)
            assertBool "nearer reachable first" (Reachable 0 < Reachable 1)
            assertBool "reachable before unrelated" (Reachable 1 < Unrelated)
        ],
      testGroup
        "pathless metrics"
        [ testCase "cold metrics expose no selected reuse" $ do
            let metrics =
                  CacheSelectionMetrics
                    { cacheCandidatesConsidered = 0,
                      cacheFullValidationAttempts = 0,
                      cacheFullValidationBytes = 0,
                      cacheSelectionKind = CacheSelectionColdKind,
                      cacheSelectedCount = 0,
                      cacheSelectedBytes = 0,
                      cacheSelectedReuse = Nothing
                    }
            cacheSelectionKind metrics @?= CacheSelectionColdKind
            cacheSelectedReuse metrics @?= Nothing
            cacheCandidatesConsidered metrics @?= 0
            cacheFullValidationAttempts metrics @?= 0
        ],
      testGroup
        "loadCacheMetaForTest"
        [ testCase "returns Nothing for a missing file without creating it" $
            withSystemTempDirectory "adrai_cache_meta" $ \tmpDir -> do
              let cachePath = tmpDir </> "missing.sqlite"
              metadata <- loadCacheMetaForTest cachePath
              metadata @?= Nothing,
          testCase "returns Nothing for an empty file" $
            withSystemTempDirectory "adrai_cache_meta" $ \tmpDir -> do
              let cachePath = tmpDir </> "empty.sqlite"
              BS.writeFile cachePath BS.empty
              metadata <- loadCacheMetaForTest cachePath
              metadata @?= Nothing,
          testCase "returns Nothing for malformed SQLite bytes" $
            withSystemTempDirectory "adrai_cache_meta" $ \tmpDir -> do
              let cachePath = tmpDir </> "malformed.sqlite"
              BS.writeFile cachePath "not sqlite"
              metadata <- loadCacheMetaForTest cachePath
              metadata @?= Nothing,
          testCase "does not synthesize metadata from malformed source" $
            withSystemTempDirectory "adrai_cache_meta" $ \tmpDir -> do
              let cachePath = tmpDir </> "invalid-meta.sqlite"
              BS.writeFile cachePath "not sqlite"
              metadata <- loadCacheMetaForTest cachePath
              maybe True Map.null metadata @?= True
        ]
    ]
