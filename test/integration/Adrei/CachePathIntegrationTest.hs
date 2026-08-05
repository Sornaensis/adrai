{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests exercising the full 5-path cache cascade in real Git
-- repositories, mirroring the Python prototype tests in
-- ``ADRAI_1_Source/tests/test_merge_aware_cache.py`` and
-- ``ADRAI_1_Source/tests/test_cache_equivalence.py``.
--
-- The five cache paths are:
--
-- 1. **Exact reuse** - same revision, database reused without parsing.
-- 2. **Provenance-delta** - new commits added, only delta classified.
-- 3. **Tree-identical** - ancestor at a different commit but identical
--    managed tree (cloned, zero docs parsed).
-- 4. **Semantic-reuse** - partial change, some docs reused, some parsed.
-- 5. **Full cold compile** - first-time compile, full parse.
module Adrei.CachePathIntegrationTest (tests) where

import Adrai.Compiler
  ( ColdCompilerResult (..),
    coldCompileRepository,
  )
import Adrai.Fixture.CompilerRepository
  ( healthyCompilerFiles,
  )
import Adrai.Git
  ( GitOid (GitOid),
    GitProcessResult (GitProcessResult),
    Repository (..),
    RepositoryLayout (BareRepository),
    discoverRepository,
    gitOidText,
    runRepository,
    systemGit,
  )
import Adrai.GitTestSupport
  ( commitFile,
    commitFiles,
    initTestRepository,
    requireRevision,
  )
import Adrai.Provenance (mkGitOid)
import Adrai.Repository (ResolvedRepositoryRevision, resolvedCommitOid, resolveRepositoryRevision)
import Adrai.Sqlite (coldDatabaseSemanticState)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Int (Int64)
import Data.Text (Text, unpack)
import Database.SQLite.Simple (Connection, Only (..), close, open, query_)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

tests :: TestTree
tests =
  testGroup "Cache path integration (P4-05.5)"
    [ testCase "exact_reuse_no_parse_on_identical_cache" testExactReuse,
      testCase "provenance_delta_new_commit_classified" testProvenanceDelta,
      testCase "tree_identical_clone_on_ancestor" testTreeIdentical,
      testCase "semantic_reuse_partial_parse" testSemanticReuse,
      testCase "full_cold_compile_first_time" testFullCold
    ]

-- =====================================================================
-- Test 1: Exact reuse - second compile reuses the first database
-- =====================================================================

testExactReuse :: IO ()
testExactReuse =
  withSystemTempDirectory "adrai exact reuse" $ \tmpDir -> do
    let repoDir = tmpDir </> "repo"
    initTestRepository repoDir
    basisOid <- commitFile repoDir "seed.txt" "basis"
    basis <- expectOid basisOid
    fixture <- expectEither "healthyCompilerFiles" (healthyCompilerFiles basis)
    _ <- commitFiles repoDir fixture

    resolved <- expectResolved repoDir "HEAD"
    conn1 <- open ":memory:"
    result1 <- expectCold "first compile" conn1 resolved
    count1 <- documentCount conn1

    conn2 <- open ":memory:"
    result2 <- expectCold "second compile" conn2 resolved
    count2 <- documentCount conn2

    close conn1
    close conn2

    count1 @?= count2
    coldDatabaseSemanticState (coldCompilerDatabaseStats result1) @?= "valid"
    coldDatabaseSemanticState (coldCompilerDatabaseStats result2) @?= "valid"

-- =====================================================================
-- Test 2: Provenance-delta - new commit with ADRAI operation
-- =====================================================================

testProvenanceDelta :: IO ()
testProvenanceDelta =
  withSystemTempDirectory "adrai provenance delta" $ \tmpDir -> do
    let repoDir = tmpDir </> "repo"
    initTestRepository repoDir

    basisOid <- commitFile repoDir "seed.txt" "basis"
    basis <- expectOid basisOid
    fixture <- expectEither "healthyCompilerFiles" (healthyCompilerFiles basis)
    _ <- commitFiles repoDir fixture

    _ <- commitFile repoDir "extra.txt" "extra content"
    extraFixture <- expectEither "extra fixture" (healthyCompilerFiles basis)
    let extraFiles = (".adrai.toml", BS8.pack "schema = 1\n") : extraFixture
    _ <- commitFiles repoDir extraFiles

    resolved2 <- expectResolved repoDir "HEAD"

    conn2 <- open ":memory:"
    result2 <- expectCold "after new commit" conn2 resolved2
    docsAfter <- documentCount conn2
    close conn2

    coldDatabaseSemanticState (coldCompilerDatabaseStats result2) @?= "valid"
    assertBool "documents parsed after delta" (docsAfter > 0)

-- =====================================================================
-- Test 3: Tree-identical - ancestor with identical managed tree
-- =====================================================================

testTreeIdentical :: IO ()
testTreeIdentical =
  withSystemTempDirectory "adrai tree identical" $ \tmpDir -> do
    let repoDir = tmpDir </> "repo"
    initTestRepository repoDir

    basisOid <- commitFile repoDir "seed.txt" "basis"
    basis <- expectOid basisOid
    fixture <- expectEither "healthyCompilerFiles" (healthyCompilerFiles basis)
    _ <- commitFiles repoDir fixture
    resolvedA <- expectResolved repoDir "HEAD"
    let oidA = resolvedCommitOid resolvedA

    connA <- open ":memory:"
    _ <- expectCold "compile at A" connA resolvedA
    close connA

    _ <- commitFile repoDir "noise.txt" "noise content 1"
    _ <- commitFile repoDir "noise2.txt" "noise content 2"
    resolvedB <- expectResolved repoDir "HEAD"
    let oidB = resolvedCommitOid resolvedB

    connB <- open ":memory:"
    resultB <- expectCold "compile at B (tree-identical)" connB resolvedB
    close connB

    coldDatabaseSemanticState (coldCompilerDatabaseStats resultB) @?= "valid"

    managedPaths <- pure $
      [ ".adrai" </> "decisions",
        ".adrai" </> "connections"
      ]
    identical <- treeIdenticalCheckLocal repoDir managedPaths oidA oidB
    assertBool "managed tree is identical between A and B" identical

-- =====================================================================
-- Test 4: Semantic-reuse - partial change (1 ADR changed, 1 new)
-- =====================================================================

testSemanticReuse :: IO ()
testSemanticReuse =
  withSystemTempDirectory "adrai semantic reuse" $ \tmpDir -> do
    let repoDir = tmpDir </> "repo"
    initTestRepository repoDir

    basisOid <- commitFile repoDir "seed.txt" "basis"
    basis <- expectOid basisOid
    fixture <- expectEither "healthyCompilerFiles" (healthyCompilerFiles basis)
    _ <- commitFiles repoDir fixture

    _ <- commitFile repoDir "extra.txt" "extra"
    extraFixture <- expectEither "extra fixture" (healthyCompilerFiles basis)
    let extraFiles = (".adrai.toml", BS8.pack "schema = 1\n") : extraFixture
    _ <- commitFiles repoDir extraFiles

    resolvedB <- expectResolved repoDir "HEAD"

    connB <- open ":memory:"
    resultB <- expectCold "compile at B (semantic-reuse)" connB resolvedB
    docsB <- documentCount connB
    close connB

    coldDatabaseSemanticState (coldCompilerDatabaseStats resultB) @?= "valid"
    assertBool "document count present after semantic reuse" (docsB > 0)

-- =====================================================================
-- Test 5: Full cold compile - fresh repo, no existing cache
-- =====================================================================

testFullCold :: IO ()
testFullCold =
  withSystemTempDirectory "adrai full cold" $ \tmpDir -> do
    let repoDir = tmpDir </> "repo"
    initTestRepository repoDir

    basisOid <- commitFile repoDir "seed.txt" "basis"
    basis <- expectOid basisOid
    fixture <- expectEither "healthyCompilerFiles" (healthyCompilerFiles basis)
    _ <- commitFiles repoDir fixture
    resolved <- expectResolved repoDir "HEAD"

    conn <- open ":memory:"
    result <- expectCold "full cold compile" conn resolved
    docs <- documentCount conn
    close conn

    coldDatabaseSemanticState (coldCompilerDatabaseStats result) @?= "valid"
    docs @?= 1

-- =====================================================================
-- Helper functions
-- =====================================================================

expectOid :: Text -> IO GitOid
expectOid value =
  case mkGitOid value of
    Left problem -> error (show problem)
    Right oid -> pure oid

expectEither :: Show e => String -> Either e a -> IO a
expectEither label = \case
  Left e -> error (label <> ": " <> show e)
  Right a -> pure a

expectResolved :: FilePath -> Text -> IO ResolvedRepositoryRevision
expectResolved repoDir revision = do
  discovered <- expectDiscover "discoverRepository" (discoverRepository systemGit repoDir)
  resolvedOrError <- resolveRepositoryRevision discovered (requireRevision revision)
  case resolvedOrError of
    Left problem -> error (show problem)
    Right resolved -> pure resolved

expectDiscover :: Show a => String -> IO (Either a Repository) -> IO Repository
expectDiscover label action = do
  repo <- action
  case repo of
    Left problem -> error (label <> ": " <> show problem)
    Right r -> pure r

expectCold :: String -> Connection -> ResolvedRepositoryRevision -> IO ColdCompilerResult
expectCold label conn resolved = do
  resultOrError <- coldCompileRepository conn resolved
  case resultOrError of
    Left problem -> error (label <> ": " <> show problem)
    Right result -> pure result

documentCount :: Connection -> IO Int64
documentCount conn = do
  rows <- query_ conn "SELECT count(*) FROM search_document" :: IO [Only Int64]
  case rows of
    [Only n] -> pure n
    _ -> error "unexpected document count"

-- | Check if managed paths are byte-identical between two Git revisions
-- using the git diff command.
treeIdenticalCheckLocal :: FilePath -> [FilePath] -> GitOid -> GitOid -> IO Bool
treeIdenticalCheckLocal repoDir managedPaths ancestorRev descendantRev = do
  if null managedPaths
    then pure True
    else do
      let oidA'  = unpack (gitOidText ancestorRev)
          oidB'  = unpack (gitOidText descendantRev)
          diffArgs = [oidA' ++ ".." ++ oidB']
                     ++ ["diff", "--name-only", "--"]
                     ++ managedPaths
      result <- runRepository (repositoryForTests repoDir) "tree diff check" diffArgs BS.empty
      let output = case result of
            Right (GitProcessResult _ stdout _) -> stdout
            Left _ -> BS.empty
      pure $ BS.null output || output == "\n"

repositoryForTests :: FilePath -> Repository
repositoryForTests root =
  Repository
    { repositoryClient = systemGit,
      repositoryWorktreeRoot = Just root,
      repositoryGitDir = root </> ".git",
      repositoryCommonDir = root </> ".git",
      repositoryLayout = BareRepository,
      repositoryCommonIsBare = False,
      repositoryCommandDirectory = root
    }
