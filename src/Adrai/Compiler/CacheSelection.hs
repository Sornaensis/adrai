{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

-- | Cache path selection logic for the ADRAI cold compiler.
--
-- Implements the cache cascade described in the Python prototype
-- (``ADRAI_1_Source/adrai_core/compiler.py`` ``compile_repo()`` cache path
-- selection, ~lines 2800-3350).  The cascade orders cache reuse from
-- most aggressive (exact hit) to least (cold compile):
--
-- 1. **Exact** — current database exists with matching schema, source
--    revision, and resolved OID.
-- 2. **Provenance-delta** — schema matches, source revision matches,
--    but resolved OID differs (new commits since last compile).
-- 3. **Tree-identical** — an ancestor database's managed tree is
--    byte-identical at the target revision.
-- 4. **Semantic-reuse** — an ancestor database is available for the
--    same semantic revision, requiring only incremental provenance refresh.
-- 5. **Cold compile** — no usable ancestor; rebuild from scratch.
--
-- "Provenance-sync" (same source revision, ref heads moved, no new
-- commits) is handled in P4-05.3 (overlay-to-cache provenance sync).
--
-- Ancestor distance is measured by Git first-parent BFS (immediate parents
-- are closer than siblings) and full reachable BFS as a tiebreaker.
module Adrai.Compiler.CacheSelection
  ( CacheMode (..),
    IncrementalKind (..),
    AncestorRank (..),
    ReuseCacheInfo (..),
    CompileCachePath (..),
    semanticReuseScore,
    loadCacheMeta,
    computeAncestorRank,
    treeIdenticalCheck,
    chooseReuseCache,
    cachePathSelection,
  )
where

import Adrai.Git (GitOid (..), GitProcessResult (..), Repository (..), gitOidText, runRepository)
import Adrai.Provenance.Ensure (ProvenanceUpdate (..))
import Control.Exception (SomeException, try)
import Control.Monad (guard, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toLower)
import Data.List (elemIndex, find, isSuffixOf, sortBy)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple (Connection, Only (..), SQLData (SQLText), execute_, open, query_, close)
import System.Directory (doesFileExist, getModificationTime, listDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))

-- | How the compiler should treat the target revision's cache.
data CacheMode
  = Exact
  | Incremental { incrementalKind :: IncrementalKind }
  | Full
  deriving (Eq, Ord, Show)

-- | What kind of incremental work is needed.
data IncrementalKind
  = ProvenanceDelta
  | ProvenanceSync
  | TreeIdentical
  | SemanticReuse
  | FullCompile
  deriving (Eq, Ord, Show)

-- | Position of a candidate cache's source revision relative to the
-- target revision, measured in Git commit ancestry.
data AncestorRank
  = ExactMatch
  | FirstParent Int  -- ^ Distance in first-parent BFS (0 = immediate parent).
  | Reachable Int    -- ^ Distance in full reachable BFS.
  | Unrelated
  deriving (Eq, Ord, Show)

-- | Metadata about a candidate cache file that survived selection.
data ReuseCacheInfo
  = ReuseCacheInfo
  { rcPath        :: FilePath,
    rcSourceRev   :: Text,
    rcCacheKey    :: Text,
    rcRank        :: AncestorRank,
    rcMtime       :: Integer
  }
  deriving (Eq, Show)

-- | The final cache-path selection result.
data CompileCachePath
  = ExactHit
  | TreeIdenticalClone ReuseCacheInfo
  | IncrementalCache AncestorRank
  | FullColdCompile
  deriving (Eq, Show)

-- ============================================================
-- loadCacheMeta
-- ============================================================

-- | Attempt to read the @meta@ table from a SQLite cache database.
--
-- Returns @Nothing@ if the file does not exist, is not a valid SQLite
-- database, or the @meta@ table is absent.
loadCacheMeta :: FilePath -> IO (Maybe (Map Text Text))
loadCacheMeta path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      result <- try @SomeException $ do
        conn <- open path
        rows <- query_ conn "SELECT key, value FROM meta ORDER BY key" :: IO [(Text, Text)]
        close conn
        pure (Map.fromList rows)
      case result of
        Left _  -> pure Nothing
        Right m -> pure (Just m)

-- ============================================================
-- computeAncestorRank
-- ============================================================

-- | Compute the ancestor rank of @candidateOid@ relative to
-- @targetOid@ within a Git repository.
--
-- Uses Git rev-list to measure BFS distance:
--
-- 1. If the OIDs are identical → 'ExactMatch'.
-- 2. If @candidate@ is on the first-parent path to @target@ → 'FirstParent' N.
-- 3. If @candidate@ is reachable but not first-parent → 'Reachable' N.
-- 4. Otherwise → 'Unrelated'.
computeAncestorRank
  :: Repository   -- ^ Git repository handle.
  -> FilePath     -- ^ Repository root (for running git commands).
  -> Text         -- ^ Target revision OID (descendant).
  -> Text         -- ^ Candidate revision OID (ancestor).
  -> IO AncestorRank
computeAncestorRank repository repoRoot targetOid candidateOid
  | targetOid == candidateOid = pure ExactMatch
  | otherwise = do
      -- First-parent BFS: walk first-parent chain from target backwards.
      firstParentResult <- runRepository repository "first-parent rev-list"
        ["rev-list", "--first-parent", "--topo-order", Text.unpack targetOid]
        BS.empty
      let firstParentOids = case firstParentResult of
            Right pr -> parseRevList $ processStdout pr
            Left _   -> []
      case elemIndex candidateOid firstParentOids of
        Just dist -> pure (FirstParent dist)
        Nothing -> do
          -- Full reachable BFS.
          fullResult <- runRepository repository "full rev-list"
            ["rev-list", "--topo-order", Text.unpack targetOid]
            BS.empty
          let fullOids = case fullResult of
                Right pr -> parseRevList $ processStdout pr
                Left _   -> []
          case elemIndex candidateOid fullOids of
            Just dist -> pure (Reachable dist)
            Nothing -> pure Unrelated

-- | Parse a newline-separated rev-list output into a list of OIDs.
-- Handles both Unix (LF) and Windows (CRLF) line endings.
parseRevList :: BS.ByteString -> [Text]
parseRevList raw
  | BS.null raw = []
  | otherwise =
      let cleaned = BS.concat $ BS.split 13 raw
          parts = BS.split 10 cleaned
       in map (Text.pack . BS8.unpack) $
          filter (not . BS.null) parts

-- ============================================================
-- semanticReuseScore
-- ============================================================

-- | Compute a numeric reuse score for a candidate cache.
--
-- Higher scores indicate better reuse potential.  The scoring model
-- rewards closer ancestry and tree identity:
--
-- * @ExactMatch@ → 1000
-- * @FirstParent N@ → 900 - N * 100 (clamped to ≥ 0)
-- * @Reachable N@ → 500 - N * 50 (clamped to ≥ 0)
-- * @Unrelated@ → 0
--
-- This is a simple linear decay model; the Python prototype uses
-- a similar scoring scheme in the cache-selection cascade.
semanticReuseScore :: AncestorRank -> Int
semanticReuseScore = \case
  ExactMatch      -> 1000
  FirstParent n   -> max 0 (900 - n * 100)
  Reachable n     -> max 0 (500 - n * 50)
  Unrelated       -> 0

-- ============================================================
-- treeIdenticalCheck
-- ============================================================

-- | Check whether the managed paths are byte-identical between two
-- revisions.
--
-- Runs @git diff --name-only <ancestor>..<descendant> -- <paths>@ and
-- returns 'True' if no files changed.
treeIdenticalCheck
  :: Repository   -- ^ Git repository handle.
  -> [FilePath]   -- ^ Managed paths to compare.
  -> Text         -- ^ Ancestor revision OID.
  -> Text         -- ^ Descendant revision OID.
  -> IO Bool
treeIdenticalCheck repository managedPaths ancestorRev descendantRev = do
  if null managedPaths
    then pure True
    else do
      let diffArgs =
            [ "diff", "--name-only", Text.unpack ancestorRev <> ".." <> Text.unpack descendantRev, "--" ]
              ++ managedPaths
      result <- runRepository repository "tree diff check" diffArgs BS.empty
      let output = case result of
            Right pr -> processStdout pr
            Left _   -> BS.empty
      pure $ BS.null output || output == "\n"

-- ============================================================
-- chooseReuseCache
-- ============================================================

-- | Choose the best reuse candidate from the cache directory.
--
-- Lists candidate SQLite files in @cacheDirectory@, filters by schema
-- match and non-empty source_revision, computes ancestor rank for each,
-- and returns the highest-scoring candidate.
--
-- Scoring order: ExactMatch > FirstParent > Reachable > Unrelated.
-- Ties are broken by closer position (list order), then more recent mtime.
chooseReuseCache
  :: Repository   -- ^ Git repository handle.
  -> FilePath     -- ^ Cache directory.
  -> Text         -- ^ Required schema (e.g. @"adrai-cache/1"@).
  -> Text         -- ^ Target revision OID.
  -> IO (Maybe ReuseCacheInfo)
chooseReuseCache repo cacheDir requiredSchema targetRev = do
  entries <- listDirectory cacheDir
  let candidateFiles = filter isCacheFile entries
  if null candidateFiles
    then pure Nothing
    else do
      infos <- mapMaybeM (toCandidate cacheDir requiredSchema) candidateFiles
      let scored = sortBy candidateScore infos
      pure (listToMaybe scored)
  where
    isCacheFile name =
      let lower = map toLower name
       in ".db" `isSuffixOf` lower || ".sqlite" `isSuffixOf` lower

    mapMaybeM f xs = do
      results <- traverse f xs
      pure (mapMaybe id results)

    toCandidate
      :: FilePath        -- ^ Cache directory.
      -> Text            -- ^ Required schema.
      -> FilePath        -- ^ Candidate file name.
      -> IO (Maybe ReuseCacheInfo)
    toCandidate cacheDir requiredSchema name = do
      let fullPath = cacheDir </> name
      meta <- loadCacheMeta fullPath
      case meta of
        Nothing -> pure Nothing
        Just m -> case (Map.lookup "schema" m, Map.lookup "source_revision" m, Map.lookup "cache_key" m) of
          (Just schema, Just srcRev, Just cacheKey')
            | schema == requiredSchema && not (Text.null srcRev) -> do
                mtime <- tryGetMtime fullPath
                rank <- computeAncestorRank repo (repositoryCommandDirectory repo) targetRev srcRev
                pure (Just ReuseCacheInfo
                  { rcPath = fullPath,
                    rcSourceRev = srcRev,
                    rcCacheKey = cacheKey',
                    rcRank = rank,
                    rcMtime = mtime
                  })
            | otherwise -> pure Nothing
          _ -> pure Nothing

    tryGetMtime :: FilePath -> IO Integer
    tryGetMtime p = do
      mt <- getModificationTime p
      pure (floor (utcTimeToPOSIXSeconds mt))

    candidateScore
      :: ReuseCacheInfo -> ReuseCacheInfo -> Ordering
    candidateScore a b =
      (comparing rcRank a b) <>
      (comparing (Down . rcMtime) a b) <>
      (comparing rcPath a b)

-- ============================================================
-- cachePathSelection
-- ============================================================

-- | Implement the cache path selection cascade.
--
-- The cascade evaluates cache reuse from most aggressive (exact hit) to
-- least (cold compile):
--
-- 1. **Exact** — the provided cache path has matching schema, source
--    revision, and resolved OID.
-- 2. **Provenance-delta** — schema matches, source revision matches,
--    but resolved OID differs (new commits since last compile).
-- 3. **Tree-identical** — an ancestor database's managed tree is
--    byte-identical at the target revision (checked via
--    'treeIdenticalCheck').
-- 4. **Semantic-reuse** — an ancestor database is available for the
--    same semantic revision, requiring only incremental provenance
--    refresh.
-- 5. **Cold compile** — no usable ancestor; rebuild from scratch.
--
-- Returns @(cacheMode, incrementalKind, bestReuseInfo)@.
cachePathSelection
    :: Repository      -- ^ Git repository handle (needed for ancestor ranking).
    -> FilePath        -- ^ Cache directory.
    -> Text            -- ^ Current database alias.
    -> Text            -- ^ Required schema (e.g. "adrai-cache/1").
    -> Text            -- ^ Target revision OID.
    -> Maybe FilePath  -- ^ Optional exact-match cache path.
    -> [FilePath]      -- ^ Managed paths for tree-identical comparison.
    -> IO (CacheMode, IncrementalKind, Maybe ReuseCacheInfo)
cachePathSelection repo cacheDir dbAlias requiredSchema targetRev exactCache managedPaths = do
  -- Path 1: Check for exact match on the provided cache path.
  let tryExact = case exactCache of
        Just cachePath -> do
          meta <- loadCacheMeta cachePath
          case meta of
            Just m -> case (Map.lookup "schema" m, Map.lookup "source_revision" m, Map.lookup "resolved_oid" m) of
              (Just s, Just srcRev, Just resolvedOid)
                | s == requiredSchema && srcRev == dbAlias && resolvedOid == targetRev ->
                    return (Just (Exact, FullCompile, Nothing))
                | s == requiredSchema && srcRev == dbAlias ->
                    return (Just (Incremental ProvenanceDelta, ProvenanceDelta,
                      Just (ReuseCacheInfo cachePath srcRev s Unrelated 0)))
                | s == requiredSchema ->
                    return (Just (Incremental SemanticReuse, SemanticReuse,
                      Just (ReuseCacheInfo cachePath srcRev s Unrelated 0)))
                | otherwise ->
                    return Nothing
              _ -> return Nothing
            Nothing -> return Nothing
        Nothing -> return Nothing
  exactResult <- tryExact
  case exactResult of
    Just result -> return result
    Nothing -> do
      -- Paths 2-5: Search the cache directory for reusable ancestors.
      bestCandidate <- chooseReuseCache repo cacheDir requiredSchema targetRev
      case bestCandidate of
        Nothing ->
          -- Path 5 (fall-through): No reusable ancestor found → cold compile.
          pure (Full, FullCompile, Nothing)
        Just candidate -> do
          let srcRev = rcSourceRev candidate
          isIdentical <- treeIdenticalCheck repo managedPaths srcRev targetRev
          pure $ if isIdentical
            then (Incremental TreeIdentical, TreeIdentical, Just candidate)
            else (Incremental SemanticReuse, SemanticReuse, Just candidate)
