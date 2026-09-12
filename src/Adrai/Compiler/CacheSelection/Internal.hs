{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
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
module Adrai.Compiler.CacheSelection.Internal where

import Adrai.Git (GitOid, Repository (..), gitOidText, processExitCode, processStdout, runRepository)
import Adrai.Compiler.CacheLease.Internal
  ( CacheLease,
    LeaseAcceptedFacts (..),
    beginCacheLeasePreparation,
    discardCacheLeasePreparation,
    registerAcceptedLeaseSource,
    registerLeaseResource,
    withCacheLeaseScopeEither,
  )
import Adrai.Format.Json (JsonValue (..), object, renderCanonicalJson)
import Adrai.Provenance (decodeBase64Url, encodeBase64Url, mkGitOid, sha256FrameStateFeed, sha256FrameStateFinalize, sha256FrameStateInit)
import Adrai.Provenance.Classification (operationMemberSignature)
import Adrai.Provenance.Ensure (configKey, openReadWriteExisting)
import Adrai.Provenance.RecoveryWitness (ProvenanceRecoveryWitness (..))
import Adrai.Retrieval
  ( SectionKind,
    SearchDocument (..),
    SearchMaterialization (..),
    SearchPassage (..),
    chunkSearchDocument,
    materializationAliases,
    materializationImplementationFingerprint,
    sectionKindName,
  )
import Adrai.Sqlite (SearchStorageComponent (..), allFtsTargets, coldSchemaDdl, ftsTargetDdl, ftsTargetTable, searchOrdinarySchemaDdl)
import Adrai.Types (Digest (..), adrIdText, digestBytes, mkAdrId, mkRecordId, mkStateToken, recordIdText)
import Control.Applicative ((<|>))
import Control.Exception (AsyncException, SomeAsyncException, SomeException, bracket, evaluate, fromException, mask, mask_, throwIO, toException, try)
import Control.Monad (forM_, unless)
import Data.Aeson (Value, eitherDecodeStrict', encode)
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import Data.Aeson.Types (Parser, parseMaybe, withArray, withObject, (.:))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as Lazy
import Data.Char (isDigit, toLower)
import Data.List (elemIndex, isSuffixOf, sort, sortBy)
import Data.Int (Int64)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.Vector as Vector
import Database.SQLite.Simple (Connection, FromRow (..), Only (..), Query (..), SQLData (..), close, execute, execute_, field, open, query, query_, withTransaction)
import System.Directory (copyFile, createDirectory, doesFileExist, getFileSize, getModificationTime, listDirectory, removeDirectory, removeFile)
import System.IO (hClose, openTempFile)
import System.IO.Error (isDoesNotExistError)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))

-- | How the compiler should treat the target revision's cache.
data CacheMode
  = Exact
  | Incremental { incrementalKind :: IncrementalKind }
  | Full
  deriving (Eq, Ord, Show)

-- | The exact archive's closed compile projection.  It contains no SQLite
-- capability, so publication/recovery may only run after validation has
-- rolled back and closed its snapshot.
data ExactArchiveCompileFacts = ExactArchiveCompileFacts
  { exactArchiveCompileMetadata :: [(Text, Text)],
    exactArchiveCompileManagedSources :: Int,
    exactArchiveCompileIssues :: Int,
    exactArchiveCompileErrors :: Int,
    exactArchiveCompileWarnings :: Int,
    exactArchiveCompileOperations :: Int,
    exactArchiveCompileReducedAdrs :: Int,
    exactArchiveCompileSearchDocuments :: Int,
    exactArchiveCompileSearchSections :: Int
  }
  deriving (Eq, Show)

-- | A fully closed exact archive decision.  An alias mismatch is a repair
-- decision, not archive authority; both branches retain only copied facts.
data ExactArchiveAliasStatus
  = ExactArchiveAliasMatches ExactArchiveCompileFacts
  | ExactArchiveAliasNeedsRepair ExactArchiveCompileFacts
  deriving (Eq, Show)

-- | The immutable revision archive is the only cache authority available to a
-- query.  This helper is intentionally non-creating: search/relevant must not
-- prepare cache directories, aliases, or overlays merely to look for a hit.
exactCacheArchivePath :: Repository -> GitOid -> Maybe FilePath
exactCacheArchivePath repository revision =
  (\root -> root </> ".adrai" </> "cache" </> Text.unpack (gitOidText revision) <> ".sqlite")
    <$> repositoryWorktreeRoot repository

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

-- | Discovery-only data retained until the archive has been copied and fully
-- validated.  The complete map, rather than only rank-driving fields, binds a
-- private accepted copy to the cheap observation that selected it.
data ReuseCacheDescriptor = ReuseCacheDescriptor
  { reuseDescriptorInfo :: ReuseCacheInfo,
    reuseDescriptorMetadata :: Map Text Text
  }

data ReuseDiscoveryHooks = ReuseDiscoveryHooks (FilePath -> IO ()) (FilePath -> IO ()) (FilePath -> IO ()) (FilePath -> IO ())

reuseDiscoveryHooksForTest :: (FilePath -> IO ()) -> (FilePath -> IO ()) -> (FilePath -> IO ()) -> ReuseDiscoveryHooks
reuseDiscoveryHooksForTest beforeOpen afterMetadata beforeRank =
  ReuseDiscoveryHooks beforeOpen (const (pure ())) afterMetadata beforeRank

-- | The additional hook is deliberately path-only.  It runs after bracket
-- ownership of an existing-file connection is installed, but before its first
-- query; it cannot acquire a SQLite capability.
reuseDiscoveryHooksForTestWithAfterOpen :: (FilePath -> IO ()) -> (FilePath -> IO ()) -> (FilePath -> IO ()) -> (FilePath -> IO ()) -> ReuseDiscoveryHooks
reuseDiscoveryHooksForTestWithAfterOpen = ReuseDiscoveryHooks

noReuseDiscoveryHooks :: ReuseDiscoveryHooks
noReuseDiscoveryHooks = ReuseDiscoveryHooks (const (pure ())) (const (pure ())) (const (pure ())) (const (pure ()))

-- | Hidden, pathless fault boundaries for the private-copy lease lifecycle.
-- Tests may arrange an action at one of these points through TestSupport, but
-- never receive the private name, a descriptor, or a live resource.
data CacheSelectionLifecycleEvent
  = BeforePrivatePublicSourceCopy
  | AfterPrivateCopy
  | BeforePrivateValidation
  | BeforePrivateSentinelClose
  | BeforePrivateSentinelUnlink
  | BeforePrivateDirectoryCreate
  | BeforePrivateDbUnlink
  | BeforePrivateJournalUnlink
  | BeforePrivateWalUnlink
  | BeforePrivateShmUnlink
  | BeforePrivateDirectoryRemoval
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype CacheSelectionLifecycleHooks = CacheSelectionLifecycleHooks
  { runCacheSelectionLifecycleHook :: CacheSelectionLifecycleEvent -> IO ()
  }

noCacheSelectionLifecycleHooks :: CacheSelectionLifecycleHooks
noCacheSelectionLifecycleHooks = CacheSelectionLifecycleHooks (const (pure ()))

-- Private lease authority is imported only from the hidden lifecycle module;
-- this public selection surface deliberately contains facts, never a source
-- pathname.
data CacheLeaseReuseFacts = CacheLeaseReuseFacts
  { cacheLeaseAcceptedFacts :: !LeaseAcceptedFacts,
    cacheLeaseReuseRank :: !AncestorRank,
    cacheLeaseReuseMtime :: !Integer
  }
  deriving (Eq, Show)

data CacheSelectionKind = CacheSelectionExactKind | CacheSelectionReuseKind | CacheSelectionColdKind
  deriving (Eq, Show)

-- | Selection facts deliberately separate cheap discovery and full-validation
-- work from stable externally attributed selection counters.  The former are
-- observer/test evidence; only selected count/bytes describe a compile phase.
data CacheSelectionMetrics = CacheSelectionMetrics
  { cacheCandidatesConsidered :: !Integer,
    cacheFullValidationAttempts :: !Integer,
    cacheFullValidationBytes :: !Integer,
    cacheSelectionKind :: !CacheSelectionKind,
    cacheSelectedCount :: !Integer,
    cacheSelectedBytes :: !Integer,
    cacheSelectedReuse :: !(Maybe CacheLeaseReuseFacts)
  }
  deriving (Eq, Show)

data CacheSelectionCascadeDecision scope
  = CacheSelectionExact !ExactArchiveCompileFacts
  | CacheSelectionReuse !(CacheLease scope) !CacheLeaseReuseFacts !(Maybe ProvenanceRecoveryWitness)
  | CacheSelectionCold

-- | Keep selection independent for each cache entry: an exact, unrelated, or
-- invalid sibling is discarded without affecting a usable baseline archive.
selectBestReuseCandidate :: [Maybe ReuseCacheInfo] -> Maybe ReuseCacheInfo
selectBestReuseCandidate =
  listToMaybe
    . sortBy candidateScore
    . filter (\info -> rcRank info /= ExactMatch && rcRank info /= Unrelated)
    . mapMaybe id

candidateScore :: ReuseCacheInfo -> ReuseCacheInfo -> Ordering
candidateScore a b =
  (comparing rcRank a b) <>
  (comparing (Down . rcMtime) a b) <>
  (comparing rcPath a b)

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
loadCacheMeta = loadCacheMetaWithHooks (const (pure ())) (const (pure ()))

-- | Discovery-only TOCTOU hook.  It runs after existence succeeds and just
-- before the existing-file open; it receives no SQLite capability.
loadCacheMetaWithBeforeOpen :: (FilePath -> IO ()) -> FilePath -> IO (Maybe (Map Text Text))
loadCacheMetaWithBeforeOpen beforeOpen = loadCacheMetaWithHooks beforeOpen (const (pure ()))

-- | Discover metadata without ever owning an opened connection outside its
-- bracket.  The pre-open hook runs after a positive existence check; the
-- acquire itself remains masked inside 'bracket', and only the hook/query body
-- is restored.  Thus a synchronous race is local ineligibility while an async
-- cancellation retains its identity after the owned close completes.
loadCacheMetaWithHooks :: (FilePath -> IO ()) -> (FilePath -> IO ()) -> FilePath -> IO (Maybe (Map Text Text))
loadCacheMetaWithHooks beforeOpen afterOpen path = mask $ \restore -> do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      -- Discovery is non-creating even when a candidate disappears between
      -- the existence check and open.  Such synchronous races are ineligible;
      -- cancellation is still released with its original identity.
      preOpen <- try @SomeException (restore (beforeOpen path))
      case preOpen of
        Left exception -> rethrowCacheAsync exception >> pure Nothing
        Right () -> do
          outcome <- runDiscoveryConnection (openReadWriteExisting path) $ \connection -> do
            afterOpen path
            rows <- query_ connection "SELECT key, value FROM meta ORDER BY key" :: IO [(Text, Text)]
            pure (Map.fromList rows)
          case outcome of
            Left exception -> rethrowCacheAsync exception >> pure Nothing
            Right (actionResult, closeResult) ->
              case actionResult of
                Left actionException
                  | isCacheAsync actionException -> throwIO actionException
                  | otherwise ->
                      case closeResult of
                        Left closeException | isCacheAsync closeException -> throwIO closeException
                        Left closeException -> throwIO closeException
                        Right () -> pure Nothing
                Right metadata ->
                  case closeResult of
                    Left closeException -> rethrowCacheAsync closeException >> pure Nothing
                    Right () -> pure (Just metadata)

-- | Bracketed existing-file ownership with separately retained action and
-- close outcomes.  The bracket releases its connection before callers apply
-- cache-local eligibility policy, so no path can strand a discovered handle.
runDiscoveryConnection :: IO Connection -> (Connection -> IO a) -> IO (Either SomeException (Either SomeException a, Either SomeException ()))
runDiscoveryConnection acquire action = mask $ \restore -> do
  actionResult <- newIORef Nothing
  closeResult <- newIORef Nothing
  outer <- try @SomeException $
    bracket
      acquire
      (\connection -> do
        result <- try @SomeException (close connection)
        writeIORef closeResult (Just result))
      (\connection -> do
        result <- try @SomeException (restore (action connection))
        writeIORef actionResult (Just result))
  case outer of
    Left exception -> pure (Left exception)
    Right () -> do
      actionOutcome <- readIORef actionResult
      closeOutcome <- readIORef closeResult
      case (actionOutcome, closeOutcome) of
        (Just action', Just close') -> pure (Right (action', close'))
        _ -> pure (Left (toException (userError "discovery connection ended without an owned outcome")))

-- | Decode cache metadata from an already-open read transaction.  Callers that
-- need a coherent archive view pair this with the connection-level validator.
loadCacheMetaConnection :: Connection -> IO (Map Text Text)
loadCacheMetaConnection connection =
  Map.fromList <$> (query_ connection "SELECT key, value FROM meta ORDER BY key" :: IO [(Text, Text)])

rethrowCacheAsync :: SomeException -> IO ()
rethrowCacheAsync exception =
  case fromException exception :: Maybe AsyncException of
    Just cancellation -> throwIO cancellation
    Nothing ->
      case fromException exception :: Maybe SomeAsyncException of
        Just cancellation -> throwIO cancellation
        Nothing -> pure ()

-- | Reject anything other than a complete, healthy canonical cold-compiler
-- database that is eligible for cross-revision reuse.  Cache discovery must
-- fail closed: metadata aliases, incomplete history, conflicts, and a merely
-- readable SQLite file are insufficient evidence that it is safe to reuse.
validateCacheContract :: FilePath -> IO Bool
validateCacheContract = validateCacheWith True

-- | Validate a compiler-produced snapshot before publishing it under the
-- mutable current alias.  A completed cold compilation can faithfully contain
-- diagnostics, conflicts, or incomplete history.  Such a snapshot is never
-- eligible for cross-revision reuse, but it is still a complete,
-- integrity-checked result that the CLI must be able to expose and recover as
-- an exact revision snapshot.
validateCachePublicationContract :: FilePath -> IO Bool
validateCachePublicationContract = validateCacheWith False

-- | Validate an archive through an already-open connection.  This has no
-- exception capture so an enclosing exact-query read transaction can preserve
-- both a coherent SQLite snapshot and asynchronous cancellation semantics.
validateCachePublicationConnection :: Connection -> IO Bool
validateCachePublicationConnection = validateCacheConnection False

-- | Validate one already-published archive as the immutable authority for a
-- particular resolved revision.  This is deliberately the path-level entry
-- point shared by recovery/alias selection: it does not treat a merely
-- publication-valid sibling as an exact result.  The connection-level form is
-- used by query execution so validation, loading, and FTS use share a single
-- SQLite snapshot.
validateExactCacheTarget :: FilePath -> Text -> IO Bool
validateExactCacheTarget path target =
  maybe False (const True) <$> readExactArchiveFactsWith (const (pure ())) (pure ()) path target

-- | Read, validate, and materialize an immutable exact archive in one masked
-- transaction.  The connection never escapes this module: by the time a
-- caller receives a result, rollback and close have completed exactly once.
-- Synchronous faults reject the candidate; cancellation is cleaned up then
-- rethrown with its original exception identity.
readExactArchiveFactsWith :: (FilePath -> IO ()) -> IO () -> FilePath -> Text -> IO (Maybe (Map Text Text, ExactArchiveCompileFacts))
readExactArchiveFactsWith observe afterFacts path target = mask $ \_ -> do
  opened <- try @SomeException (openReadWriteExisting path)
  case opened of
    Left exception -> rethrowCacheAsync exception >> pure Nothing
    Right connection -> do
      outcome <- runExactArchiveTransaction connection $ \held -> do
          -- @mode=rw@ proves non-creation and this one transaction keeps all
          -- publication, schema, integrity, FK, count, fingerprint, current
          -- ref, provenance, and six-FTS checks in the same snapshot.
          execute_ held "BEGIN"
          observe path
          withValidatedExactCacheTargetConnection target held $ \accepted -> do
              execute_ held "PRAGMA query_only=ON"
              let metadata = validatedMetadata accepted
                  facts = exactArchiveCompileFactsFromRows accepted
              -- Test-only hooks may observe the post-validation interval, but
              -- receive no connection or proof.  This makes replacement races
              -- reproducible without extending archive authority beyond this
              -- bracket.
              afterFacts
              pure (metadata, facts)
      checked <- try @SomeException (finishExactArchiveTransaction outcome)
      case checked of
        Left exception -> rethrowCacheAsync exception >> pure Nothing
        Right result -> pure result

releaseExactArchiveConnection :: Connection -> IO ()
releaseExactArchiveConnection connection =
  cleanupExactArchiveConnection [execute_ connection "ROLLBACK", close connection]

-- | Opaque test-only cleanup hooks.  They are deliberately nullary: tests can
-- inject cleanup faults without gaining a database capability or a path.
data ExactArchiveCleanupHooks = ExactArchiveCleanupHooks (IO ()) (IO ())

exactArchiveCleanupHooksForTest :: IO () -> IO () -> ExactArchiveCleanupHooks
exactArchiveCleanupHooksForTest = ExactArchiveCleanupHooks

noExactArchiveCleanupHooks :: ExactArchiveCleanupHooks
noExactArchiveCleanupHooks = ExactArchiveCleanupHooks (pure ()) (pure ())

releaseExactArchiveConnectionWithHooks :: ExactArchiveCleanupHooks -> Connection -> IO ()
releaseExactArchiveConnectionWithHooks (ExactArchiveCleanupHooks beforeRollback beforeClose) connection =
  cleanupExactArchiveConnection [beforeRollback, execute_ connection "ROLLBACK", beforeClose, close connection]

-- | A metadata-routing connection never starts a transaction, so closing it
-- must not hide a failed rollback that did not belong to this scope.
closeExactArchiveConnection :: Connection -> IO ()
closeExactArchiveConnection connection = cleanupExactArchiveConnection [close connection]

-- | Cleanup is masked, attempts every required action, and reports the first
-- failure only after later actions have run.  A clean successful decision can
-- therefore never escape an unproven rollback/close; callers with an
-- initiating typed result keep that result outside this cleanup boundary.
cleanupExactArchiveConnection :: [IO ()] -> IO ()
cleanupExactArchiveConnection actions = mask_ $ do
  outcomes <- traverse (try @SomeException) actions
  let failures = [exception | Left exception <- outcomes]
  case filter isCacheAsync failures of
    exception : _ -> throwIO exception
    [] -> case failures of
      exception : _ -> throwIO exception
      [] -> pure ()

-- | Run an exact-archive action and its masked cleanup without allowing an
-- interruptible rollback/close finalizer to erase the action outcome.  Callers
-- choose their typed-result policy from these two outcomes; ordinary archive
-- readers use 'finishExactArchiveTransaction' below, while alias repair keeps
-- a completed typed repair error dominant.
runExactArchiveTransaction :: Connection -> (Connection -> IO a) -> IO (Either SomeException a, Either SomeException ())
runExactArchiveTransaction = runExactConnectionWith releaseExactArchiveConnection

runExactArchiveTransactionWithHooks :: ExactArchiveCleanupHooks -> Connection -> (Connection -> IO a) -> IO (Either SomeException a, Either SomeException ())
runExactArchiveTransactionWithHooks hooks = runExactConnectionWith (releaseExactArchiveConnectionWithHooks hooks)

runExactAliasProbe :: Connection -> (Connection -> IO a) -> IO (Either SomeException a, Either SomeException ())
runExactAliasProbe = runExactConnectionWith closeExactArchiveConnection

runExactConnectionWith :: (Connection -> IO ()) -> Connection -> (Connection -> IO a) -> IO (Either SomeException a, Either SomeException ())
runExactConnectionWith release connection action = mask $ \restore -> do
  actionResult <- try @SomeException (restore (action connection))
  cleanupResult <- try @SomeException (release connection)
  pure (actionResult, cleanupResult)

finishExactArchiveTransaction :: (Either SomeException a, Either SomeException ()) -> IO a
finishExactArchiveTransaction (actionResult, cleanupResult) =
  case actionResult of
    Left actionException
      | isCacheAsync actionException -> throwIO actionException
      | otherwise ->
          case cleanupResult of
            Left cleanupException | isCacheAsync cleanupException -> throwIO cleanupException
            _ -> throwIO actionException
    Right value ->
      case cleanupResult of
        Left cleanupException -> throwIO cleanupException
        Right () -> pure value

isCacheAsync :: SomeException -> Bool
isCacheAsync exception =
  isJust (fromException exception :: Maybe AsyncException)
    || isJust (fromException exception :: Maybe SomeAsyncException)

-- | The only exact-query archive authority.  It combines the complete
-- publication contract, immutable target metadata, and FTS5's strongest
-- content-versus-index proof in the caller's single existing-file transaction.
-- The FTS commands may throw on a malformed posting index; callers retain
-- their established synchronous/async exception boundary.
validateExactCacheTargetConnection :: Text -> Connection -> IO Bool
validateExactCacheTargetConnection target connection =
  maybe False (const True) <$> withValidatedExactCacheTargetConnection target connection (const (pure ()))

-- | Read the exact compile projection from a closed archive decision.
exactArchiveAliasCompileFacts :: ExactArchiveAliasStatus -> ExactArchiveCompileFacts
exactArchiveAliasCompileFacts = \case
  ExactArchiveAliasMatches facts -> facts
  ExactArchiveAliasNeedsRepair facts -> facts

-- | Validate an immutable archive once and keep that transaction continuously
-- live while the mutable alias is independently opened, fully validated, and
-- closed.  Only then is the archive rolled back and closed before the closed
-- result is returned.  A healthy result therefore observes exactly
-- @[archive, alias]@ without a validation/use TOCTOU gap.
readExactArchiveAliasStatus :: FilePath -> Text -> FilePath -> IO (Maybe ExactArchiveAliasStatus)
readExactArchiveAliasStatus = readExactArchiveAliasStatusWith (const (pure ())) (pure ())

readExactArchiveAliasStatusWith :: (FilePath -> IO ()) -> IO () -> FilePath -> Text -> FilePath -> IO (Maybe ExactArchiveAliasStatus)
readExactArchiveAliasStatusWith observe afterFacts archive target alias = mask $ \_ -> do
  opened <- try @SomeException (openReadWriteExisting archive)
  case opened of
    Left exception -> rethrowCacheAsync exception >> pure Nothing
    Right connection -> do
      outcome <- runExactArchiveTransaction connection $ \held -> do
          execute_ held "BEGIN"
          observe archive
          archiveDecision <- withValidatedExactCacheTargetConnection target held $ \acceptedRows ->
            pure (validatedMetadata acceptedRows, exactArchiveCompileFactsFromRows acceptedRows)
          case archiveDecision of
            Nothing -> pure Nothing
            Just (accepted, facts) -> do
              execute_ held "PRAGMA query_only=ON"
              -- The hook has no archive capability.  It can expose a narrow
              -- post-validation replacement race, while this held snapshot
              -- remains the sole source of returned facts.
              afterFacts
              aliasMetadata <- readExactArchiveMetadataWith observe alias target
              pure $
                Just $
                  if aliasMetadata == Just accepted
                    then ExactArchiveAliasMatches facts
                    else ExactArchiveAliasNeedsRepair facts
      checked <- try @SomeException (finishExactArchiveTransaction outcome)
      case checked of
        Left exception -> rethrowCacheAsync exception >> pure Nothing
        Right result -> pure result

-- | Hold one fully validated immutable archive decision while deciding whether
-- the mutable alias can be used or must be repaired.  The cheap alias probe is
-- only a routing hint: it can send a missing or wrong alias to @repair@, but it
-- can never establish an exact hit.  Both the matching and repaired paths
-- fully validate the alias once and compare its complete accepted metadata and
-- materialization facts with the held archive before returning facts.
--
-- The polymorphic repair result deliberately lets callers preserve their own
-- typed publication error.  Synchronous archive/alias faults fall cold;
-- asynchronous exceptions are released and rethrown unchanged.
withExactArchiveAliasRepair
  :: FilePath
  -> Text
  -> FilePath
  -> (ExactArchiveCompileFacts -> IO (Either error ()))
  -> IO (Either error (Maybe ExactArchiveCompileFacts))
withExactArchiveAliasRepair = withExactArchiveAliasRepairWith (const (pure ())) (pure ())

-- | Test-observable form of 'withExactArchiveAliasRepair'.  The observer runs
-- for the held archive and once for the fully validated alias; neither hook
-- receives a connection.
withExactArchiveAliasRepairWith
  :: (FilePath -> IO ())
  -> IO ()
  -> FilePath
  -> Text
  -> FilePath
  -> (ExactArchiveCompileFacts -> IO (Either error ()))
  -> IO (Either error (Maybe ExactArchiveCompileFacts))
withExactArchiveAliasRepairWith = withExactArchiveAliasRepairWithCleanupHooks noExactArchiveCleanupHooks

-- | Test-only variant with opaque cleanup hooks.  The real rollback and close
-- always run after the injected actions, so fault injection cannot mask a
-- leaked SQLite handle.
withExactArchiveAliasRepairWithCleanupHooks
  :: ExactArchiveCleanupHooks
  -> (FilePath -> IO ())
  -> IO ()
  -> FilePath
  -> Text
  -> FilePath
  -> (ExactArchiveCompileFacts -> IO (Either error ()))
  -> IO (Either error (Maybe ExactArchiveCompileFacts))
withExactArchiveAliasRepairWithCleanupHooks hooks observe afterFacts archive target alias repair = mask $ \restore -> do
  opened <- try @SomeException (openReadWriteExisting archive)
  case opened of
    Left exception -> rethrowCacheAsync exception >> pure (Right Nothing)
    Right held -> do
      outcome <- runExactArchiveTransactionWithHooks hooks held $ \connection -> do
          execute_ connection "BEGIN"
          observe archive
          archiveDecision <- withValidatedExactCacheTargetConnection target connection $ \acceptedRows ->
            pure (validatedMetadata acceptedRows, exactArchiveCompileFactsFromRows acceptedRows)
          case archiveDecision of
            Nothing -> pure (Right Nothing)
            Just (accepted, facts) -> do
              execute_ connection "PRAGMA query_only=ON"
              afterFacts
              resolvedAlias <- readExactAliasResolvedOid alias
              let validateAlias = do
                    aliasFacts <- readExactArchiveFactsWith observe (pure ()) alias target
                    pure $ case aliasFacts of
                      Just (aliasMetadata, candidateFacts)
                        | aliasMetadata == accepted && candidateFacts == facts -> Just facts
                      _ -> Nothing
                  repairAndValidate = do
                    repaired <- restore (repair facts)
                    case repaired of
                      Left problem -> pure (Left problem)
                      Right () -> Right <$> validateAlias
              if resolvedAlias == Just target
                then do
                  initiallyValidated <- validateAlias
                  -- A target-looking alias remains only a routing hint.  A
                  -- full metadata/materialization mismatch is repaired from
                  -- the held authority, then validated once as published.
                  maybe repairAndValidate (pure . Right . Just) initiallyValidated
                else repairAndValidate
      case fst outcome of
        Right (Left problem) ->
          case snd outcome of
            Left cleanupException | isCacheAsync cleanupException -> throwIO cleanupException
            _ -> pure (Left problem)
        _ -> do
          checked <- try @SomeException (finishExactArchiveTransaction outcome)
          case checked of
            Left exception -> rethrowCacheAsync exception >> pure (Right Nothing)
            Right result -> pure result

-- | This inexpensive metadata read only chooses the repair branch.  It is not
-- a validator and no caller may use a matching value as exact authority.
readExactAliasResolvedOid :: FilePath -> IO (Maybe Text)
readExactAliasResolvedOid alias = mask $ \_ -> do
  opened <- try @SomeException (openReadWriteExisting alias)
  case opened of
    Left exception -> rethrowCacheAsync exception >> pure Nothing
    Right connection -> do
      outcome <- runExactAliasProbe connection $ \held -> do
        rows <- query held "SELECT value FROM meta WHERE key='resolved_oid'" () :: IO [Only Text]
        pure $ case rows of
          [Only oid] -> Just oid
          _ -> Nothing
      checked <- try @SomeException (finishExactArchiveTransaction outcome)
      case checked of
        Left exception -> rethrowCacheAsync exception >> pure Nothing
        Right resolved -> pure resolved

-- | The alias is a standalone fully validated transaction.  It never borrows
-- an immutable archive connection or allows one to survive publication.
readExactArchiveMetadataWith :: (FilePath -> IO ()) -> FilePath -> Text -> IO (Maybe (Map Text Text))
readExactArchiveMetadataWith observe alias target = mask $ \_ -> do
  opened <- try @SomeException (openReadWriteExisting alias)
  case opened of
    Left exception -> rethrowCacheAsync exception >> pure Nothing
    Right connection -> do
      outcome <- runExactArchiveTransaction connection $ \held -> do
        execute_ held "BEGIN"
        observe alias
        metadata <- exactArchiveMetadataFromConnection target held
        case metadata of
          Nothing -> pure Nothing
          Just accepted -> execute_ held "PRAGMA query_only=ON" >> pure (Just accepted)
      checked <- try @SomeException (finishExactArchiveTransaction outcome)
      case checked of
        Left exception -> rethrowCacheAsync exception >> pure Nothing
        Right metadata -> pure metadata

-- | Materialize facts while the one-shot archive transaction is still held.
exactArchiveCompileFactsFromConnection :: Connection -> IO ExactArchiveCompileFacts
exactArchiveCompileFactsFromConnection connection = do
  exactArchiveCompileMetadata <- query_ connection "SELECT key,value FROM meta ORDER BY key"
  exactArchiveCompileManagedSources <- scalarCount "SELECT count(*) FROM managed_source"
  exactArchiveCompileIssues <- scalarCount "SELECT count(*) FROM issue"
  exactArchiveCompileErrors <- scalarCount "SELECT count(*) FROM issue WHERE severity='error'"
  exactArchiveCompileWarnings <- scalarCount "SELECT count(*) FROM issue WHERE severity='warning'"
  exactArchiveCompileOperations <- scalarCount "SELECT count(*) FROM operation"
  exactArchiveCompileReducedAdrs <- scalarCount "SELECT count(*) FROM reduced_adr"
  exactArchiveCompileSearchDocuments <- scalarCount "SELECT count(*) FROM search_document"
  exactArchiveCompileSearchSections <- scalarCount "SELECT count(*) FROM search_section"
  pure ExactArchiveCompileFacts {..}
  where
    scalarCount sql = do
      rows <- query_ connection sql :: IO [Only Int]
      case rows of
        [Only count] -> pure count
        _ -> fail "exact archive count query returned an invalid result"

exactArchiveCompileFactsFromRows :: ValidatedCacheRows scope -> ExactArchiveCompileFacts
exactArchiveCompileFactsFromRows rows =
  ExactArchiveCompileFacts
    { exactArchiveCompileMetadata = validatedMetadataRows rows,
      exactArchiveCompileManagedSources = length (validatedManagedSourceRowsInternal rows),
      exactArchiveCompileIssues = length issues,
      exactArchiveCompileErrors = length [() | (_, _, severity, _, _, _, _, _, _, _) <- issues, severity == "error"],
      exactArchiveCompileWarnings = length [() | (_, _, severity, _, _, _, _, _, _, _) <- issues, severity == "warning"],
      exactArchiveCompileOperations = length (validatedOperationRowsInternal rows),
      exactArchiveCompileReducedAdrs = length (validatedReducedRows rows),
      exactArchiveCompileSearchDocuments = length (searchMaterializationDocuments (validatedSearchMaterializationInternal rows)),
      exactArchiveCompileSearchSections = length (searchMaterializationPassages (validatedSearchMaterializationInternal rows))
    }
  where
    issues = validatedIssueRows rows

exactArchiveMetadataFromConnection :: Text -> Connection -> IO (Maybe (Map Text Text))
exactArchiveMetadataFromConnection target connection =
  withValidatedExactCacheTargetConnection target connection (pure . validatedMetadata)

-- | Load only the target-qualified provenance facts that can suppress a
-- redundant recovery replay.  Validation and reads share one existing-file
-- SQLite transaction, so a path replacement or concurrent archive mutation
-- cannot mix a validated contract with unrelated rows.  This intentionally
-- does not load semantic or search payloads.
loadProvenanceRecoveryWitness :: FilePath -> Text -> IO (Maybe ProvenanceRecoveryWitness)
loadProvenanceRecoveryWitness path sourceTarget = do
  checked <- try @SomeException $
    bracket (openReadWriteExisting path) close $ \connection -> do
      execute_ connection "PRAGMA query_only=ON"
      withTransaction connection $ do
        accepted <- loadValidatedCacheRows connection
        let metadata = validatedMetadata accepted
        if cacheRowsAreCanonical False accepted
             && Map.lookup "schema" metadata == Just "adrai-cache/3"
             && Map.lookup "requested_revision" metadata == Just sourceTarget
             && Map.lookup "resolved_oid" metadata == Just sourceTarget
             && Map.lookup "semantic_state" metadata == Just "valid"
             && Map.lookup "history_complete" metadata == Just "true"
          then pure (provenanceRecoveryWitnessFromRows accepted sourceTarget)
          else pure Nothing
  case checked of
    Left exception -> rethrowCacheAsync exception >> pure Nothing
    Right witness -> pure witness

-- | Extract a witness from the same transaction that has already established
-- the complete canonical cache contract.  It deliberately performs no path
-- open, metadata read, or second validation, preventing a private-copy
-- validation/use race.
loadProvenanceRecoveryWitnessFromValidatedConnection :: Connection -> Text -> IO (Maybe ProvenanceRecoveryWitness)
loadProvenanceRecoveryWitnessFromValidatedConnection connection sourceTarget = do
  members <- query_ connection
    "SELECT operation_id,object_id,path,blob_oid,semantic_digest FROM operation_member ORDER BY operation_id,path,object_id"
    :: IO [(Text, Text, Text, Text, Text)]
  coverage <- query connection
    "SELECT op_id,registration_signature FROM operation_target_coverage WHERE target_oid=? ORDER BY op_id,registration_signature"
    (Only sourceTarget) :: IO [(Text, Text)]
  placements <- query_ connection "SELECT op_id,commit_oid FROM operation_commit ORDER BY op_id,commit_oid" :: IO [(Text, Text)]
  placementProjection <- query_ connection "SELECT op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json FROM operation_commit ORDER BY op_id,commit_oid" :: IO [(Text, Text, Text, Integer, Integer, Text, Text)]
  issueProjection <- query_ connection "SELECT operation_id,severity,code,adr_id,object_id,path,message FROM issue WHERE origin='provenance' AND operation_id IS NOT NULL ORDER BY operation_id,severity,code,adr_id,object_id,path,message" :: IO [(Text, Text, Text, Maybe Text, Maybe Text, Maybe Text, Text)]
  landingProjection <- query_ connection "SELECT config_key,op_id,line_id,ref_name,commit_oid,complete FROM line_landing ORDER BY config_key,op_id,line_id,ref_name" :: IO [(Text, Text, Text, Text, Text, Integer)]
  lineConfigs <- query_ connection "SELECT config_key,config_json FROM line_config ORDER BY config_key" :: IO [(Text, Text)]
  reachable <- query connection
    "SELECT commit_oid FROM target_reachable_commit WHERE target_oid=? ORDER BY commit_oid"
    (Only sourceTarget) :: IO [Only Text]
  case ( traverse (mkGitOid . fromOnly) reachable
       , traverse (mkGitOid . snd) placements
       ) of
    (Right reachableOids, Right _) -> do
      let signatureMap = Map.map (operationSignatureText . operationMemberSignature) $
            Map.fromListWith (<>)
              [ (operationId, [(objectId, path', blobOid, semanticDigest)])
              | (operationId, objectId, path', blobOid, semanticDigest) <- members
              ]
          reachableSet = Set.fromList reachableOids
          covered = Set.fromList
            [ operationId
            | (operationId, signature) <- coverage
            , Map.lookup operationId signatureMap == Just signature
            ]
          placed = Set.fromList
            [ operationId
            | (operationId, rawOid) <- placements
            , Right placementOid <- [mkGitOid rawOid]
            , Set.member placementOid reachableSet
            ]
      case mkGitOid sourceTarget of
        Right sourceOid
          | Set.member sourceOid reachableSet ->
              pure (Just (ProvenanceRecoveryWitness sourceOid signatureMap covered placed reachableSet (Set.fromList placementProjection) (Set.fromList issueProjection) (Set.fromList landingProjection) (Set.fromList lineConfigs)))
        _ -> pure Nothing
    _ -> pure Nothing

provenanceRecoveryWitnessFromRows :: ValidatedCacheRows scope -> Text -> Maybe ProvenanceRecoveryWitness
provenanceRecoveryWitnessFromRows rows sourceTarget = do
  reachableOids <- traverse (maybeOid . snd) (validatedTargetRowsInternal rows)
  _ <- traverse (maybeOid . commitOid) placements
  sourceOid <- either (const Nothing) Just (mkGitOid sourceTarget)
  let signatureMap = Map.map (operationSignatureText . operationMemberSignature) $
        Map.fromListWith (<>)
          [ (operationId, [(objectId, path, blobOid, semanticDigest)])
          | (operationId, objectId, _, _, semanticDigest, path, blobOid) <- members
          ]
      reachableSet = Set.fromList reachableOids
      covered = Set.fromList
        [ operationId
        | (operationId, target, signature) <- validatedCoverageRowsInternal rows
        , target == sourceTarget
        , Map.lookup operationId signatureMap == Just signature
        ]
      placed = Set.fromList
        [ operationId
        | (operationId, rawOid, _, _, _, _, _) <- placements
        , Right placementOid <- [mkGitOid rawOid]
        , Set.member placementOid reachableSet
        ]
      placementProjection =
        Set.fromList
          [ (operationId, oid, classification, fromIntegral authored, fromIntegral committed, subject, parents)
          | (operationId, oid, classification, authored, committed, subject, parents) <- placements
          ]
      issueProjection =
        Set.fromList
          [ (operationId, severity, code, adr, objectId, path, message)
          | (_, code, severity, origin, adr, objectId, Just operationId, _, path, message) <- validatedIssueRows rows
          , origin == "provenance"
          ]
      landingProjection =
        Set.fromList
          [ (configKey', operationId, lineId, refName, oid, fromIntegral complete)
          | (configKey', operationId, lineId, refName, oid, complete) <- validatedLandingRowsInternal rows
          ]
  if Set.member sourceOid reachableSet
    then Just (ProvenanceRecoveryWitness sourceOid signatureMap covered placed reachableSet placementProjection issueProjection landingProjection (Set.fromList (validatedLineConfigRowsInternal rows)))
    else Nothing
  where
    members = validatedMemberRowsInternal rows
    placements = validatedOperationCommitRowsInternal rows
    commitOid (_, oid, _, _, _, _, _) = oid
    maybeOid = either (const Nothing) Just . mkGitOid

validateFtsContentIndexes :: Maybe CacheValidationWorkHooks -> Connection -> IO ()
validateFtsContentIndexes maybeWorkHooks connection =
  forM_ ftsIntegrityChecks $ \(name, command) -> do
    maybe (pure ()) (\workHooks -> recordCacheValidationWork workHooks (CacheValidationFtsIntegrityAttempt name)) maybeWorkHooks
    result <- try @SomeException (execute_ connection command)
    case result of
      Left exception -> do
        maybe (pure ()) (\workHooks -> recordCacheValidationWork workHooks (CacheValidationFtsIntegrityFailure name)) maybeWorkHooks
        throwIO exception
      Right () ->
        maybe (pure ()) (\workHooks -> recordCacheValidationWork workHooks (CacheValidationFtsIntegrityCheck name)) maybeWorkHooks
  where
    ftsIntegrityChecks =
      [ ("fts_search_exact", "INSERT INTO fts_search_exact(fts_search_exact,rank) VALUES('integrity-check',1)")
      , ("fts_search_stemmed", "INSERT INTO fts_search_stemmed(fts_search_stemmed,rank) VALUES('integrity-check',1)")
      , ("fts_search_identifier", "INSERT INTO fts_search_identifier(fts_search_identifier,rank) VALUES('integrity-check',1)")
      , ("fts_passage_exact", "INSERT INTO fts_passage_exact(fts_passage_exact,rank) VALUES('integrity-check',1)")
      , ("fts_passage_stemmed", "INSERT INTO fts_passage_stemmed(fts_passage_stemmed,rank) VALUES('integrity-check',1)")
      , ("fts_passage_identifier", "INSERT INTO fts_passage_identifier(fts_passage_identifier,rank) VALUES('integrity-check',1)")
      ]

-- | Test-visible decomposition of the materialization commitment checked by
-- 'validateCachePublicationContract'.  The public compiler path consumes only
-- the boolean validator; this seam lets bounded regressions distinguish a
-- metadata/fingerprint disagreement from the other fail-closed checks.
-- The triple is @(written, current, legacy)@. Cache v3 has no compatible
-- historical materialization frame, so the final field is always 'Nothing'.
cachePublicationMaterializationFingerprintEvidence :: FilePath -> IO (Maybe (Text, Text, Maybe Text))
cachePublicationMaterializationFingerprintEvidence path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      result <- try @SomeException $
         bracket (open path) close $ \conn -> do
          metadata <- Map.fromList <$> (query_ conn "SELECT key, value FROM meta ORDER BY key" :: IO [(Text, Text)])
          sourceFingerprint <- persistedSourceFingerprint conn
          current <- persistedMaterializationFingerprint conn sourceFingerprint
          pure $ do
            written <- Map.lookup "materialization_fingerprint" metadata
            actual <- current
            pure (written, actual, Nothing)
      pure (either (const Nothing) id result)

-- | Recommit the materialization digest after a private provenance sync.
-- Provenance diagnostics are ordinary issue rows and therefore participate in
-- the immutable payload commitment.  A candidate is never published until
-- this refresh succeeds, so the archive remains self-validating to later
-- cache selection.
refreshCacheMaterializationFingerprint :: FilePath -> IO ()
refreshCacheMaterializationFingerprint path =
  bracket (open path) close $ \connection -> do
    metadata <- Map.fromList <$> (query_ connection "SELECT key,value FROM meta ORDER BY key" :: IO [(Text, Text)])
    fingerprint <- persistedMaterializationFingerprint connection (Map.lookup "source_fingerprint" metadata)
    case fingerprint of
      Nothing -> ioError (userError "cache provenance sync could not recompute materialization fingerprint")
      Just value ->
        execute connection "UPDATE meta SET value=? WHERE key='materialization_fingerprint'" (Only value)

-- | Recommit a private provenance-refresh candidate only when the exact
-- replacement fingerprint makes its already-loaded canonical rows satisfy the
-- full publication contract.  The proof and one metadata update share one
-- transaction: malformed rows and SQLite faults roll the candidate back
-- without writing a self-consistent-looking fingerprint for an invalid
-- archive.  Keep 'refreshCacheMaterializationFingerprint' above as the
-- deliberately narrower helper used by its existing callers.
refreshCachePublicationMaterializationFingerprint :: FilePath -> IO ()
refreshCachePublicationMaterializationFingerprint =
  refreshCachePublicationMaterializationFingerprintWithHooks noRefreshPublicationHooks

-- | The refresh transaction has a deliberately narrow test seam.  Hooks can
-- observe the sole derivation or add a fixed fault before real cleanup, but
-- cannot receive a connection or replace the real SQL operation.
data RefreshPublicationUpdateMode
  = RefreshPublicationExactUpdate
  | RefreshPublicationNoMatchUpdate

data RefreshPublicationHooks = RefreshPublicationHooks
  { refreshPublicationObserveDerivation :: IO (),
    -- | This observer receives the value returned by the real
    -- @SELECT changes()@ boundary.  It is deliberately observation-only: the
    -- transaction still makes the acceptance decision below.
    refreshPublicationObserveAffectedRows :: Int -> IO (),
    refreshPublicationUpdateMode :: RefreshPublicationUpdateMode,
    refreshPublicationBeforeRollback :: IO (),
    refreshPublicationBeforeClose :: IO (),
    refreshPublicationWorkHooks :: Maybe CacheValidationWorkHooks
  }

noRefreshPublicationHooks :: RefreshPublicationHooks
noRefreshPublicationHooks =
  RefreshPublicationHooks
      { refreshPublicationObserveDerivation = pure (),
        refreshPublicationObserveAffectedRows = const (pure ()),
        refreshPublicationUpdateMode = RefreshPublicationExactUpdate,
        refreshPublicationBeforeRollback = pure (),
        refreshPublicationBeforeClose = pure (),
        refreshPublicationWorkHooks = Nothing
    }

-- | Refresh a private publication candidate with explicit masked ownership.
-- A failed action (including COMMIT) always receives a real rollback attempt;
-- close is always attempted exactly once.  Failure resolution occurs only
-- after both required cleanup steps have been attempted.
refreshCachePublicationMaterializationFingerprintWithHooks :: RefreshPublicationHooks -> FilePath -> IO ()
refreshCachePublicationMaterializationFingerprintWithHooks hooks path = mask $ \restore -> do
  opened <- try @SomeException (refreshPublicationPhase hooks "source-open" (openReadWriteExisting path))
  case opened of
    Left exception -> throwIO exception
    Right connection -> do
      initiating <- try @SomeException $ restore $ do
        execute_ connection "BEGIN"
        rows <- refreshPublicationPhase hooks "canonical-load" (loadValidatedCacheRowsWithOptionalWork (refreshPublicationWorkHooks hooks) connection)
        fingerprint <- refreshPublicationPhase hooks "derivation" $ do
          refreshPublicationObserveDerivation hooks
          case persistedMaterializationFingerprintFromRows rows of
            Nothing -> ioError (userError "cache provenance sync could not recompute materialization fingerprint")
            Just value -> evaluate value
        refreshedRows <- case replaceMaterializationFingerprint fingerprint rows of
          Nothing -> ioError (userError "cache provenance sync requires exactly one materialization_fingerprint metadata row")
          Just value -> pure value
        -- FTS5 posting integrity is a separate authority from the relational
        -- projection.  Run it while this exact loaded snapshot is still held;
        -- a damaged shadow segment must reach the real failing family rather
        -- than be collapsed into a later boolean rejection.
        validateFtsContentIndexes (refreshPublicationWorkHooks hooks) connection
        unless (cacheRowsAreCanonicalWithMaterialization False (Just fingerprint) refreshedRows) $
          ioError (userError "provenance refresh produced an invalid publication candidate")
        refreshPublicationPhase hooks "update" $ case refreshPublicationUpdateMode hooks of
          RefreshPublicationExactUpdate ->
            execute connection "UPDATE meta SET value=? WHERE key='materialization_fingerprint'" (Only fingerprint)
          RefreshPublicationNoMatchUpdate ->
              execute connection "UPDATE meta SET value=? WHERE key='__adrai_refresh_no_match__'" (Only fingerprint)
        refreshPublicationPhase hooks "affected-row" $ do
          changedRows <- query_ connection "SELECT changes()" :: IO [Only Int]
          case changedRows of
            [Only value] -> do
              refreshPublicationObserveAffectedRows hooks value
              unless (value == 1) $
                ioError (userError "cache provenance sync metadata refresh did not affect exactly one row")
            _ -> ioError (userError "cache provenance sync metadata refresh did not affect exactly one row")
        refreshPublicationPhase hooks "commit" (execute_ connection "COMMIT")
      cleanup <- refreshPublicationCleanup hooks connection (either (const True) (const False) initiating)
      finishRefreshPublicationTransaction initiating cleanup

refreshPublicationCleanup :: RefreshPublicationHooks -> Connection -> Bool -> IO [Either SomeException ()]
refreshPublicationCleanup hooks connection requiresRollback = mask_ $
  traverse (try @SomeException) $
    (if requiresRollback then [rollback] else []) <> [closeConnection]
  where
    rollback = do
      refreshPublicationPhase hooks "rollback" (execute_ connection "ROLLBACK")
      -- Fixed test faults happen only after the real cleanup call completed.
      refreshPublicationBeforeRollback hooks
    closeConnection = do
      refreshPublicationPhase hooks "close" (close connection)
      refreshPublicationBeforeClose hooks

recordRefreshWork :: RefreshPublicationHooks -> CacheValidationWorkEvent -> IO ()
recordRefreshWork hooks event = maybe (pure ()) (`recordCacheValidationWork` event) (refreshPublicationWorkHooks hooks)

-- | Telemetry surrounds the existing operation and never controls it.  The
-- failure event is recorded before the identical exception is rethrown.
refreshPublicationPhase :: RefreshPublicationHooks -> Text -> IO a -> IO a
refreshPublicationPhase hooks phase action = do
  recordRefreshWork hooks (CacheValidationPublicationPhaseAttempt phase)
  maybe (pure ()) (recordRefreshWork hooks) (legacyAttemptEvent phase)
  outcome <- try @SomeException action
  case outcome of
    Right value -> do
      recordRefreshWork hooks (CacheValidationPublicationPhaseComplete phase)
      maybe (pure ()) (recordRefreshWork hooks) (legacyCompletionEvent phase)
      pure value
    Left exception -> do
      recordRefreshWork hooks (CacheValidationPublicationPhaseFailure phase)
      throwIO exception

legacyCompletionEvent :: Text -> Maybe CacheValidationWorkEvent
legacyCompletionEvent = \case
  "derivation" -> Just CacheValidationPublicationDerivation
  "update" -> Just CacheValidationPublicationUpdate
  "commit" -> Just CacheValidationPublicationCommit
  "rollback" -> Just CacheValidationPublicationRollbackComplete
  "close" -> Just CacheValidationPublicationCloseComplete
  _ -> Nothing

legacyAttemptEvent :: Text -> Maybe CacheValidationWorkEvent
legacyAttemptEvent = \case
  "rollback" -> Just CacheValidationPublicationRollbackAttempt
  "close" -> Just CacheValidationPublicationCloseAttempt
  _ -> Nothing

finishRefreshPublicationTransaction :: Either SomeException () -> [Either SomeException ()] -> IO ()
finishRefreshPublicationTransaction initiating cleanup =
  case firstAsyncFailure [initiating] <|> firstAsyncFailure cleanup <|> firstSyncFailure [initiating] <|> firstSyncFailure cleanup of
    Just exception -> throwIO exception
    Nothing -> pure ()
  where
    firstSyncFailure outcomes = listToMaybe [exception | Left exception <- outcomes]

replaceMaterializationFingerprint :: Text -> ValidatedCacheRows scope -> Maybe (ValidatedCacheRows scope)
replaceMaterializationFingerprint fingerprint rows =
  case replace (validatedMetadataRows rows) of
    Just metadataRows ->
      Just rows
        { validatedMetadataRows = metadataRows,
          validatedMetadata = Map.insert "materialization_fingerprint" fingerprint (validatedMetadata rows)
        }
    Nothing -> Nothing
  where
    replace metadataRows =
      case [() | (key, _) <- metadataRows, key == "materialization_fingerprint"] of
        [_] -> Just [(key, if key == "materialization_fingerprint" then fingerprint else value) | (key, value) <- metadataRows]
        _ -> Nothing

validateCacheWith :: Bool -> FilePath -> IO Bool
validateCacheWith requireValidSemantics path =
  maybe False (const True) <$> validatedCacheMetadataWith requireValidSemantics path

-- | Validate and read cache metadata through the same connection and
-- transaction.  Reuse selection compares this accepted metadata with its
-- cheap rank descriptor, so a path replacement between discovery and full
-- validation cannot inherit the old revision/rank facts.
validatedCacheMetadataWith :: Bool -> FilePath -> IO (Maybe (Map Text Text))
validatedCacheMetadataWith requireValidSemantics path = mask $ \restore -> do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      opened <- try @SomeException (openReadWriteExisting path)
      case opened of
        Left exception -> rethrowCacheAsync exception >> pure Nothing
        Right connection -> do
          checked <- try @SomeException $
            bracket (pure connection) close $ \held -> restore $ withTransaction held $ do
              accepted <- loadValidatedCacheRows held
              if cacheRowsAreCanonical requireValidSemantics accepted
                then pure (Just (validatedMetadata accepted))
                else pure Nothing
          case checked of
            Left exception -> rethrowCacheAsync exception >> pure Nothing
            Right metadata -> pure metadata

data CacheValidationWorkCounters = CacheValidationWorkCounters
  { cacheValidationCanonicalFamilyScans :: Map Text Int,
    cacheValidationCanonicalFamilyRows :: Map Text Int,
    cacheValidationFtsIntegrityChecks :: Map Text Int,
    cacheValidationFtsIntegrityAttempts :: Map Text Int,
    cacheValidationFtsIntegrityFailures :: Map Text Int,
    cacheValidationFtsParityChecks :: Map Text Int,
    cacheValidationFtsParityAttempts :: Map Text Int,
    cacheValidationFtsParityFailures :: Map Text Int,
    cacheValidationFtsPayloadMaterializations :: Int,
    cacheValidationFullMaterializationLoads :: Int,
    cacheValidationInvocations :: Int,
    cacheValidationSourceOpens :: Int,
    cacheValidationPublicationDerivations :: Int,
    cacheValidationPublicationUpdates :: Int,
    cacheValidationPublicationCommits :: Int,
    cacheValidationPublicationRollbackAttempts :: Int,
    cacheValidationPublicationRollbackCompletions :: Int,
    cacheValidationPublicationCloseAttempts :: Int,
    cacheValidationPublicationCloseCompletions :: Int,
    cacheValidationPublicationPhaseAttempts :: Map Text Int,
    cacheValidationPublicationPhaseCompletions :: Map Text Int,
    cacheValidationPublicationPhaseFailures :: Map Text Int
  }
  deriving (Eq, Show)

data CacheValidationWorkEvent
  = CacheValidationCanonicalFamilyScan Text Int
  | CacheValidationFtsIntegrityAttempt Text
  | CacheValidationFtsIntegrityCheck Text
  | CacheValidationFtsIntegrityFailure Text
  | CacheValidationFtsParityAttempt Text
  | CacheValidationFtsParityCheck Text
  | CacheValidationFtsParityFailure Text
  | CacheValidationFtsPayloadMaterialization
  | CacheValidationFullMaterializationLoad
  | CacheValidationInvocation
  | CacheValidationSourceOpen
  | CacheValidationPublicationDerivation
  | CacheValidationPublicationUpdate
  | CacheValidationPublicationCommit
  | CacheValidationPublicationRollbackAttempt
  | CacheValidationPublicationRollbackComplete
  | CacheValidationPublicationCloseAttempt
  | CacheValidationPublicationCloseComplete
  | CacheValidationPublicationPhaseAttempt Text
  | CacheValidationPublicationPhaseComplete Text
  | CacheValidationPublicationPhaseFailure Text

newtype CacheValidationWorkHooks = CacheValidationWorkHooks (CacheValidationWorkEvent -> IO ())

recordCacheValidationWork :: CacheValidationWorkHooks -> CacheValidationWorkEvent -> IO ()
recordCacheValidationWork (CacheValidationWorkHooks record) = record

scanCanonicalFamily :: Maybe CacheValidationWorkHooks -> Text -> IO [row] -> IO [row]
scanCanonicalFamily Nothing _ action = action
scanCanonicalFamily (Just workHooks) family action = do
  rows <- action
  recordCacheValidationWork workHooks (CacheValidationCanonicalFamilyScan family (length rows))
  pure rows

recordSuccessfulMaterializationLoad :: Maybe CacheValidationWorkHooks -> IO value -> IO value
recordSuccessfulMaterializationLoad Nothing action = action
recordSuccessfulMaterializationLoad (Just workHooks) action = do
  value <- action
  recordCacheValidationWork workHooks CacheValidationFullMaterializationLoad
  pure value

-- | One collision-free, transaction-scoped read of every canonical logical
-- family needed by validation and exact-query reconstruction.  The constructor
-- stays hidden; public callers can only consume a value inside the continuation
-- in 'withValidatedExactCacheTargetConnection'.
data ValidatedCacheRows scope = ValidatedCacheRows
  { validatedMetadataRows :: [(Text, Text)],
    validatedMetadata :: Map Text Text,
    validatedSchemaRows :: [(Text, Text)],
    validatedSchemaObjects :: [(Text, Text, Text)],
    validatedRepositoryConfigRowsInternal :: [RepositoryConfigRow],
    validatedManagedSourceRowsInternal :: [ManagedSourceRow],
    validatedIssueRows :: [IssueRow],
    validatedConflictRows :: [ConflictRow],
    validatedOperationRowsInternal :: [OperationRow],
    validatedMemberRowsInternal :: [OperationMemberRow],
    validatedParentRows :: [OperationParentRow],
    validatedDecisionRows :: [DecisionRow],
    validatedConnectionRows :: [ConnectionRow],
    validatedReducedRows :: [ReducedRow],
    validatedAxisRows :: [AxisRow],
    validatedCurrentRows :: [CurrentConnectionRow],
    validatedOperationCommitRowsInternal :: [OperationCommitRow],
    validatedCoverageRowsInternal :: [CoverageRow],
    validatedLandingRowsInternal :: [LandingRow],
    validatedTargetRowsInternal :: [(Text, Text)],
    validatedHasOtherTargets :: Bool,
    validatedTargetCommitSet :: Set.Set Text,
    validatedLineConfigRowsInternal :: [(Text, Text)],
    validatedRefObservationRows :: [(Text, Text, Text)],
    validatedSearchMaterializationInternal :: SearchMaterialization,
    validatedSearchDocumentFrames :: [Text],
    validatedSearchPassageFrames :: [Text],
    validatedSearchAliasFrames :: [Text],
    validatedFtsProjectionParity :: Bool,
    validatedIntegrityRows :: [Only Text],
    validatedForeignKeyRows :: [(Text, Int64, Text, Int64)]
  }

type RepositoryConfigRow = (Text, Text, Maybe Text, Maybe Text, Maybe Text, Maybe BS.ByteString, Text, Maybe Text, Maybe Text)
type ManagedSourceRow = (Text, Text, Text, Text, Maybe BS.ByteString, Text)
type IssueRow = (Int64, Text, Text, Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Text)
type ConflictRow = (Text, Text, Int64, Text, Text)
type OperationMemberRow = (Text, Text, Text, Text, Text, Text, Text)
type OperationParentRow = (Text, Text, Int64, Text)
type DecisionRow = (Text, Text, Text, Text, Text, Text, Text, Text)
type ConnectionRow = (Text, Text, Text, Text, Text, Text, Text)
type ReducedRow = (Text, Text, Int64)
type AxisRow = (Text, Text, Int64, Text)
type CurrentConnectionRow = (Text, Text, Int64, Text)
type OperationCommitRow = (Text, Text, Text, Int64, Int64, Text, Text)
type CoverageRow = (Text, Text, Text)
type LandingRow = (Text, Text, Text, Text, Text, Int64)

data OperationRow = OperationRow
  Text Text Text Text (Maybe Text) Text (Maybe Text) (Maybe Text) Text Text
  (Maybe Text) (Maybe Text) (Maybe Text)
  deriving (Eq, Ord, Show)

instance FromRow OperationRow where
  fromRow =
    OperationRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field <*> field

data ValidatedRawDocumentRow = ValidatedRawDocumentRow
  SQLData SQLData SQLData SQLData SQLData SQLData SQLData SQLData SQLData SQLData
  SQLData SQLData SQLData SQLData SQLData SQLData SQLData SQLData SQLData

instance FromRow ValidatedRawDocumentRow where
  fromRow =
    ValidatedRawDocumentRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data ValidatedRawPassageRow = ValidatedRawPassageRow
  SQLData SQLData SQLData SQLData SQLData SQLData SQLData SQLData SQLData SQLData
  SQLData SQLData SQLData SQLData

instance FromRow ValidatedRawPassageRow where
  fromRow =
    ValidatedRawPassageRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data ValidatedRawAliasRow = ValidatedRawAliasRow SQLData SQLData SQLData

instance FromRow ValidatedRawAliasRow where
  fromRow = ValidatedRawAliasRow <$> field <*> field <*> field

-- These exact production statements are also explained through the closed
-- TestSupport observer below.  The composite primary key is
-- (target_oid,commit_oid), so the bulk load and both rejection probes all use
-- its leading key without a whole-table scan or a temporary sort.
targetReachabilityBulkQuery :: Query
targetReachabilityBulkQuery =
  "SELECT target_oid,commit_oid FROM target_reachable_commit WHERE target_oid=? ORDER BY commit_oid"

targetReachabilityLowerProbeQuery :: Query
targetReachabilityLowerProbeQuery =
  "SELECT 1 FROM target_reachable_commit WHERE target_oid<? LIMIT 1"

targetReachabilityUpperProbeQuery :: Query
targetReachabilityUpperProbeQuery =
  "SELECT 1 FROM target_reachable_commit WHERE target_oid>? LIMIT 1"

loadValidatedCacheRows :: Connection -> IO (ValidatedCacheRows scope)
loadValidatedCacheRows = loadValidatedCacheRowsWithOptionalWork Nothing

loadValidatedCacheRowsWithWork :: CacheValidationWorkHooks -> Connection -> IO (ValidatedCacheRows scope)
loadValidatedCacheRowsWithWork workHooks = loadValidatedCacheRowsWithOptionalWork (Just workHooks)

loadValidatedCacheRowsWithOptionalWork :: Maybe CacheValidationWorkHooks -> Connection -> IO (ValidatedCacheRows scope)
loadValidatedCacheRowsWithOptionalWork maybeWorkHooks connection = do
  validatedMetadataRows <- scanCanonicalFamily maybeWorkHooks "meta" $ query_ connection "SELECT key,value FROM meta ORDER BY key"
  let validatedMetadata = Map.fromList validatedMetadataRows
      target = Map.findWithDefault "" "resolved_oid" validatedMetadata
  validatedIntegrityRows <- query_ connection "PRAGMA integrity_check"
  validatedForeignKeyRows <- query_ connection "PRAGMA foreign_key_check"
  validatedSchemaObjects <- scanCanonicalFamily maybeWorkHooks "sqlite_master" $ query_ connection "SELECT type,name,sql FROM sqlite_master WHERE sql IS NOT NULL"
  let validatedSchemaRows = [(name, ddl) | ("table", name, ddl) <- validatedSchemaObjects]
  validatedRepositoryConfigRowsInternal <- scanCanonicalFamily maybeWorkHooks "repository_config" $ query_ connection "SELECT origin,path,oid,object_type,mode,bytes,parse_state,decision_root,connection_root FROM repository_config WHERE singleton=1"
  validatedManagedSourceRowsInternal <- scanCanonicalFamily maybeWorkHooks "managed_source" $ query_ connection "SELECT path,oid,object_type,mode,bytes,parse_state FROM managed_source ORDER BY path"
  validatedIssueRows <- scanCanonicalFamily maybeWorkHooks "issue" $ query_ connection "SELECT ordinal,code,severity,origin,adr_id,object_id,operation_id,commit_oid,path,message FROM issue ORDER BY code,adr_id,object_id,operation_id,commit_oid,path,severity,CASE origin WHEN 'config' THEN 0 WHEN 'path_document' THEN 1 WHEN 'history' THEN 2 WHEN 'operation' THEN 3 WHEN 'graph' THEN 4 WHEN 'basis' THEN 5 ELSE 6 END,origin,message"
  validatedConflictRows <- scanCanonicalFamily maybeWorkHooks "adr_conflict" $ query_ connection "SELECT adr_id,code,candidate_count,state_token,summaries FROM adr_conflict ORDER BY adr_id"
  validatedOperationRowsInternal <- scanCanonicalFamily maybeWorkHooks "operation" $ query_ connection "SELECT operation_id,timestamp_ms,actor_kind,actor_id,actor_model,basis_oid,branch_hint,upstream_hint,line_anchors,tool_version,input_digest,prompt_digest,context_digest FROM operation ORDER BY operation_id"
  validatedMemberRowsInternal <- scanCanonicalFamily maybeWorkHooks "operation_member" $ query_ connection "SELECT operation_id,object_id,object_type,event_kind,semantic_digest,path,blob_oid FROM operation_member ORDER BY operation_id,object_id"
  validatedParentRows <- scanCanonicalFamily maybeWorkHooks "operation_member_parent" $ query_ connection "SELECT operation_id,object_id,ordinal,parent_object_id FROM operation_member_parent ORDER BY operation_id,object_id,ordinal"
  validatedDecisionRows <- scanCanonicalFamily maybeWorkHooks "decision_record" $ query_ connection "SELECT record_id,adr_id,operation_id,title,summary,domains,body,path FROM decision_record ORDER BY record_id"
  validatedConnectionRows <- scanCanonicalFamily maybeWorkHooks "connection_record" $ query_ connection "SELECT connection_id,adr_id,operation_id,relation_kind,payload,rationale,path FROM connection_record ORDER BY connection_id"
  validatedReducedRows <- scanCanonicalFamily maybeWorkHooks "reduced_adr" $ query_ connection "SELECT adr_id,state_token,conflicted FROM reduced_adr ORDER BY adr_id"
  validatedAxisRows <- scanCanonicalFamily maybeWorkHooks "axis_head" $ query_ connection "SELECT adr_id,axis,ordinal,object_id FROM axis_head ORDER BY adr_id,axis,ordinal"
  validatedCurrentRows <- scanCanonicalFamily maybeWorkHooks "current_connection" $ query_ connection "SELECT adr_id,axis,ordinal,connection_id FROM current_connection ORDER BY adr_id,ordinal"
  validatedOperationCommitRowsInternal <- scanCanonicalFamily maybeWorkHooks "operation_commit" $ query_ connection "SELECT op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json FROM operation_commit ORDER BY op_id,commit_oid"
  validatedCoverageRowsInternal <- scanCanonicalFamily maybeWorkHooks "operation_target_coverage" $ query_ connection "SELECT op_id,target_oid,registration_signature FROM operation_target_coverage ORDER BY op_id,target_oid,registration_signature"
  validatedLandingRowsInternal <- scanCanonicalFamily maybeWorkHooks "line_landing" $ query_ connection "SELECT config_key,op_id,line_id,ref_name,commit_oid,complete FROM line_landing ORDER BY config_key,op_id,line_id,ref_name"
  validatedTargetRowsInternal <- scanCanonicalFamily maybeWorkHooks "target_reachable_commit" $
    query connection targetReachabilityBulkQuery (Only target)
  lowerTargets <- query connection targetReachabilityLowerProbeQuery (Only target) :: IO [Only Int]
  upperTargets <- query connection targetReachabilityUpperProbeQuery (Only target) :: IO [Only Int]
  let validatedHasOtherTargets = not (null lowerTargets && null upperTargets)
  let validatedTargetCommitSet = Set.fromList (map snd validatedTargetRowsInternal)
  validatedLineConfigRowsInternal <- scanCanonicalFamily maybeWorkHooks "line_config" $ query_ connection "SELECT config_key,config_json FROM line_config ORDER BY config_key"
  validatedRefObservationRows <- scanCanonicalFamily maybeWorkHooks "ref_observation" $ query_ connection "SELECT ref_name,tip_oid,object_type FROM ref_observation ORDER BY ref_name"
  (validatedSearchMaterializationInternal, validatedSearchDocumentFrames, validatedSearchPassageFrames, validatedSearchAliasFrames) <-
    recordSuccessfulMaterializationLoad maybeWorkHooks $
      loadValidatedSearchRows maybeWorkHooks connection
  validatedFtsProjectionParity <- validateFtsProjectionParity maybeWorkHooks connection
  pure ValidatedCacheRows {..}

-- | Establish exact authority and consume the accepted row bundle before the
-- caller's surrounding transaction can end.  The rank-2 scope prevents the
-- bundle itself from escaping; consumers may only derive closed values.
withValidatedExactCacheTargetConnection
  :: Text
  -> Connection
  -> (forall scope. ValidatedCacheRows scope -> IO value)
  -> IO (Maybe value)
withValidatedExactCacheTargetConnection target connection consume = do
  withValidatedExactCacheTargetConnectionWithOptionalWork Nothing target connection consume

withValidatedExactCacheTargetConnectionWithWork
  :: CacheValidationWorkHooks
  -> Text
  -> Connection
  -> (forall scope. ValidatedCacheRows scope -> IO value)
  -> IO (Maybe value)
withValidatedExactCacheTargetConnectionWithWork workHooks target connection consume = do
  withValidatedExactCacheTargetConnectionWithOptionalWork (Just workHooks) target connection consume

withValidatedExactCacheTargetConnectionWithOptionalWork
  :: Maybe CacheValidationWorkHooks
  -> Text
  -> Connection
  -> (forall scope. ValidatedCacheRows scope -> IO value)
  -> IO (Maybe value)
withValidatedExactCacheTargetConnectionWithOptionalWork maybeWorkHooks target connection consume = do
  maybe (pure ()) (`recordCacheValidationWork` CacheValidationInvocation) maybeWorkHooks
  rows <- loadValidatedCacheRowsWithOptionalWork maybeWorkHooks connection
  let metadata = validatedMetadata rows
      exact =
        cacheRowsAreCanonical False rows
          && Map.lookup "schema" metadata == Just "adrai-cache/3"
          && Map.lookup "requested_revision" metadata == Just target
          && Map.lookup "resolved_oid" metadata == Just target
  if exact
    then validateFtsContentIndexes maybeWorkHooks connection >> Just <$> consume rows
    else pure Nothing

observeExactCacheValidationWorkForTest :: FilePath -> Text -> IO (Bool, CacheValidationWorkCounters)
observeExactCacheValidationWorkForTest path target = do
  counters <- newIORef emptyCounters
  let hooks = CacheValidationWorkHooks (record counters)
  outcome <- try @SomeException $
    bracket (openReadWriteExisting path) close $ \connection -> do
      record counters CacheValidationSourceOpen
      withTransaction connection $
        maybe False (const True) <$> withValidatedExactCacheTargetConnectionWithWork hooks target connection (\accepted -> pure (validatedSearchMaterialization accepted))
  case outcome of
    Left exception -> do
      rethrowCacheAsync exception
      observed <- readIORef counters
      pure (False, observed)
    Right accepted -> do
      observed <- readIORef counters
      pure (accepted, observed)
  where
    emptyCounters = CacheValidationWorkCounters Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty 0 0 0 0 0 0 0 0 0 0 0 Map.empty Map.empty Map.empty
    record counter event = atomicModifyIORef' counter $ \current ->
      let updated = case event of
            CacheValidationCanonicalFamilyScan family rowCount ->
              current
                { cacheValidationCanonicalFamilyScans = Map.insertWith (+) family 1 (cacheValidationCanonicalFamilyScans current),
                  cacheValidationCanonicalFamilyRows = Map.insertWith (+) family rowCount (cacheValidationCanonicalFamilyRows current)
                }
            CacheValidationFtsIntegrityAttempt family -> current {cacheValidationFtsIntegrityAttempts = Map.insertWith (+) family 1 (cacheValidationFtsIntegrityAttempts current)}
            CacheValidationFtsIntegrityCheck family -> current {cacheValidationFtsIntegrityChecks = Map.insertWith (+) family 1 (cacheValidationFtsIntegrityChecks current)}
            CacheValidationFtsIntegrityFailure family -> current {cacheValidationFtsIntegrityFailures = Map.insertWith (+) family 1 (cacheValidationFtsIntegrityFailures current)}
            CacheValidationFtsParityAttempt family -> current {cacheValidationFtsParityAttempts = Map.insertWith (+) family 1 (cacheValidationFtsParityAttempts current)}
            CacheValidationFtsParityCheck family -> current {cacheValidationFtsParityChecks = Map.insertWith (+) family 1 (cacheValidationFtsParityChecks current)}
            CacheValidationFtsParityFailure family -> current {cacheValidationFtsParityFailures = Map.insertWith (+) family 1 (cacheValidationFtsParityFailures current)}
            CacheValidationFtsPayloadMaterialization -> current {cacheValidationFtsPayloadMaterializations = cacheValidationFtsPayloadMaterializations current + 1}
            CacheValidationFullMaterializationLoad -> current {cacheValidationFullMaterializationLoads = cacheValidationFullMaterializationLoads current + 1}
            CacheValidationInvocation -> current {cacheValidationInvocations = cacheValidationInvocations current + 1}
            CacheValidationSourceOpen -> current {cacheValidationSourceOpens = cacheValidationSourceOpens current + 1}
            CacheValidationPublicationDerivation -> current {cacheValidationPublicationDerivations = cacheValidationPublicationDerivations current + 1}
            CacheValidationPublicationUpdate -> current {cacheValidationPublicationUpdates = cacheValidationPublicationUpdates current + 1}
            CacheValidationPublicationCommit -> current {cacheValidationPublicationCommits = cacheValidationPublicationCommits current + 1}
            CacheValidationPublicationRollbackAttempt -> current {cacheValidationPublicationRollbackAttempts = cacheValidationPublicationRollbackAttempts current + 1}
            CacheValidationPublicationRollbackComplete -> current {cacheValidationPublicationRollbackCompletions = cacheValidationPublicationRollbackCompletions current + 1}
            CacheValidationPublicationCloseAttempt -> current {cacheValidationPublicationCloseAttempts = cacheValidationPublicationCloseAttempts current + 1}
            CacheValidationPublicationCloseComplete -> current {cacheValidationPublicationCloseCompletions = cacheValidationPublicationCloseCompletions current + 1}
            CacheValidationPublicationPhaseAttempt phase -> current {cacheValidationPublicationPhaseAttempts = Map.insertWith (+) phase 1 (cacheValidationPublicationPhaseAttempts current)}
            CacheValidationPublicationPhaseComplete phase -> current {cacheValidationPublicationPhaseCompletions = Map.insertWith (+) phase 1 (cacheValidationPublicationPhaseCompletions current)}
            CacheValidationPublicationPhaseFailure phase -> current {cacheValidationPublicationPhaseFailures = Map.insertWith (+) phase 1 (cacheValidationPublicationPhaseFailures current)}
       in (updated, ())

-- | Test-only positive control for the one boundary which would materialize an
-- FTS payload in Haskell.  Production validation deliberately never calls it;
-- keeping the event here makes its reported zero count falsifiable.
observeFtsPayloadMaterializationForTest :: FilePath -> IO Int
observeFtsPayloadMaterializationForTest path = do
  (hooks, readWork) <- newCacheValidationWorkCountersForTest
  bracket (openReadWriteExisting path) close $ \connection -> do
    materializeOneFtsPayloadForTest hooks connection
  cacheValidationFtsPayloadMaterializations <$> readWork

-- | The sole bounded test-only boundary for FTS payload materialization.
-- Production validation deliberately never calls it.
materializeOneFtsPayloadForTest :: CacheValidationWorkHooks -> Connection -> IO ()
materializeOneFtsPayloadForTest hooks connection = do
  (_ :: [(Text, Text)]) <- query_ connection "SELECT item_id,title FROM fts_search_exact LIMIT 1"
  recordCacheValidationWork hooks CacheValidationFtsPayloadMaterialization

newCacheValidationWorkCountersForTest :: IO (CacheValidationWorkHooks, IO CacheValidationWorkCounters)
newCacheValidationWorkCountersForTest = do
  counter <- newIORef emptyCounters
  pure (CacheValidationWorkHooks (record counter), readIORef counter)
  where
    emptyCounters = CacheValidationWorkCounters Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty 0 0 0 0 0 0 0 0 0 0 0 Map.empty Map.empty Map.empty
    record ref event = atomicModifyIORef' ref $ \current ->
      let updated = case event of
            CacheValidationCanonicalFamilyScan family rowCount -> current
              { cacheValidationCanonicalFamilyScans = Map.insertWith (+) family 1 (cacheValidationCanonicalFamilyScans current)
              , cacheValidationCanonicalFamilyRows = Map.insertWith (+) family rowCount (cacheValidationCanonicalFamilyRows current)
              }
            CacheValidationFtsIntegrityAttempt family -> current {cacheValidationFtsIntegrityAttempts = Map.insertWith (+) family 1 (cacheValidationFtsIntegrityAttempts current)}
            CacheValidationFtsIntegrityCheck family -> current {cacheValidationFtsIntegrityChecks = Map.insertWith (+) family 1 (cacheValidationFtsIntegrityChecks current)}
            CacheValidationFtsIntegrityFailure family -> current {cacheValidationFtsIntegrityFailures = Map.insertWith (+) family 1 (cacheValidationFtsIntegrityFailures current)}
            CacheValidationFtsParityAttempt family -> current {cacheValidationFtsParityAttempts = Map.insertWith (+) family 1 (cacheValidationFtsParityAttempts current)}
            CacheValidationFtsParityCheck family -> current {cacheValidationFtsParityChecks = Map.insertWith (+) family 1 (cacheValidationFtsParityChecks current)}
            CacheValidationFtsParityFailure family -> current {cacheValidationFtsParityFailures = Map.insertWith (+) family 1 (cacheValidationFtsParityFailures current)}
            CacheValidationFtsPayloadMaterialization -> current {cacheValidationFtsPayloadMaterializations = cacheValidationFtsPayloadMaterializations current + 1}
            CacheValidationFullMaterializationLoad -> current {cacheValidationFullMaterializationLoads = cacheValidationFullMaterializationLoads current + 1}
            CacheValidationInvocation -> current {cacheValidationInvocations = cacheValidationInvocations current + 1}
            CacheValidationSourceOpen -> current {cacheValidationSourceOpens = cacheValidationSourceOpens current + 1}
            CacheValidationPublicationDerivation -> current {cacheValidationPublicationDerivations = cacheValidationPublicationDerivations current + 1}
            CacheValidationPublicationUpdate -> current {cacheValidationPublicationUpdates = cacheValidationPublicationUpdates current + 1}
            CacheValidationPublicationCommit -> current {cacheValidationPublicationCommits = cacheValidationPublicationCommits current + 1}
            CacheValidationPublicationRollbackAttempt -> current {cacheValidationPublicationRollbackAttempts = cacheValidationPublicationRollbackAttempts current + 1}
            CacheValidationPublicationRollbackComplete -> current {cacheValidationPublicationRollbackCompletions = cacheValidationPublicationRollbackCompletions current + 1}
            CacheValidationPublicationCloseAttempt -> current {cacheValidationPublicationCloseAttempts = cacheValidationPublicationCloseAttempts current + 1}
            CacheValidationPublicationCloseComplete -> current {cacheValidationPublicationCloseCompletions = cacheValidationPublicationCloseCompletions current + 1}
            CacheValidationPublicationPhaseAttempt phase -> current {cacheValidationPublicationPhaseAttempts = Map.insertWith (+) phase 1 (cacheValidationPublicationPhaseAttempts current)}
            CacheValidationPublicationPhaseComplete phase -> current {cacheValidationPublicationPhaseCompletions = Map.insertWith (+) phase 1 (cacheValidationPublicationPhaseCompletions current)}
            CacheValidationPublicationPhaseFailure phase -> current {cacheValidationPublicationPhaseFailures = Map.insertWith (+) phase 1 (cacheValidationPublicationPhaseFailures current)}
       in (updated, ())

data TargetReachabilityPlanDetails = TargetReachabilityPlanDetails
  { targetReachabilityBulkPlanDetails :: [Text],
    targetReachabilityLowerProbePlanDetails :: [Text],
    targetReachabilityUpperProbePlanDetails :: [Text]
  }
  deriving (Eq, Show)

-- | Run EXPLAIN against the exact production statements without returning a
-- connection, a validated bundle, or any authority-bearing value.
observeTargetReachabilityPlansForTest :: FilePath -> Text -> IO (Maybe TargetReachabilityPlanDetails)
observeTargetReachabilityPlansForTest path target = do
  outcome <- try @SomeException $
    bracket (openReadWriteExisting path) close $ \connection -> withTransaction connection $ do
      bulk <- planDetails connection targetReachabilityBulkQuery target
      lower <- planDetails connection targetReachabilityLowerProbeQuery target
      upper <- planDetails connection targetReachabilityUpperProbeQuery target
      pure TargetReachabilityPlanDetails
        { targetReachabilityBulkPlanDetails = bulk,
          targetReachabilityLowerProbePlanDetails = lower,
          targetReachabilityUpperProbePlanDetails = upper
        }
  case outcome of
    Left exception -> rethrowCacheAsync exception >> pure Nothing
    Right details -> pure (Just details)
  where
    planDetails connection statement parameter = do
      rows <- query connection (explainQuery statement) (Only parameter) :: IO [(Int, Int, Int, Text)]
      pure [detail | (_, _, _, detail) <- rows]
    explainQuery (Query statement) = Query ("EXPLAIN QUERY PLAN " <> statement)

validatedRepositoryConfigRows :: ValidatedCacheRows scope -> [(Text, Maybe BS.ByteString, Text)]
validatedRepositoryConfigRows rows =
  [(origin, bytes, parseState) | (origin, _, _, _, _, bytes, parseState, _, _) <- validatedRepositoryConfigRowsInternal rows]

validatedManagedSourceRows :: ValidatedCacheRows scope -> [(Text, Text, Text, Text, Maybe BS.ByteString, Text)]
validatedManagedSourceRows = validatedManagedSourceRowsInternal

validatedOperationRows :: ValidatedCacheRows scope -> [(Text, Text)]
validatedOperationRows rows = [(operationId, basisOid) | OperationRow operationId _ _ _ _ basisOid _ _ _ _ _ _ _ <- validatedOperationRowsInternal rows]

validatedMemberRows :: ValidatedCacheRows scope -> [(Text, Text, Text, Text)]
validatedMemberRows rows = [(operationId, objectId, path, blobOid) | (operationId, objectId, _, _, _, path, blobOid) <- validatedMemberRowsInternal rows]

validatedCoverageRows :: ValidatedCacheRows scope -> [(Text, Text, Text)]
validatedCoverageRows = validatedCoverageRowsInternal

validatedOperationCommitRows :: ValidatedCacheRows scope -> [(Text, Text, Text, Int64, Int64, Text, Text)]
validatedOperationCommitRows = validatedOperationCommitRowsInternal

validatedLandingRows :: ValidatedCacheRows scope -> [(Text, Text, Text, Text, Text, Int64)]
validatedLandingRows = validatedLandingRowsInternal

validatedTargetRows :: ValidatedCacheRows scope -> [(Text, Text)]
validatedTargetRows = validatedTargetRowsInternal

validatedLineConfigRows :: ValidatedCacheRows scope -> [(Text, Text)]
validatedLineConfigRows = validatedLineConfigRowsInternal

validatedSearchMaterialization :: ValidatedCacheRows scope -> SearchMaterialization
validatedSearchMaterialization = validatedSearchMaterializationInternal

-- | Decode search rows and capture their established fingerprint frames in the
-- same physical table scan.  This is the strict loader contract from
-- 'Adrai.Sqlite', kept internal here so validation can retain collision-free
-- typed values without rereading the three full materialization families.
loadValidatedSearchRows :: Maybe CacheValidationWorkHooks -> Connection -> IO (SearchMaterialization, [Text], [Text], [Text])
loadValidatedSearchRows maybeWorkHooks connection = do
  rawDocuments <- scanCanonicalFamily maybeWorkHooks "search_document" $ query_ connection validatedDocumentLoaderQuery
  rawPassages <- scanCanonicalFamily maybeWorkHooks "search_section" $ query_ connection validatedPassageLoaderQuery
  rawAliases <- scanCanonicalFamily maybeWorkHooks "local_alias" $ query_ connection validatedAliasLoaderQuery
  case decodeAll rawDocuments rawPassages rawAliases of
    Left problem -> ioError (userError (Text.unpack problem))
    Right accepted -> pure accepted
  where
    decodeAll documentsRaw passagesRaw aliasesRaw = do
      documentPairs <- traverse decodeValidatedDocument documentsRaw
      ensureUniqueValues "search document item_id" (map (searchDocumentItemId . fst) documentPairs)
      aliasPairs <- traverse decodeValidatedAlias aliasesRaw
      ensureUniqueValues "local alias" (map (fst . fst) aliasPairs)
      passageTriples <- traverse decodeValidatedPassage passagesRaw
      ensureSequentialPassages passageTriples
      let documents = map fst documentPairs
          passages = map (\(_, passage, _) -> passage) passageTriples
          aliases = map fst aliasPairs
          materialization = SearchMaterialization documents passages aliases
      if aliases /= materializationAliases documents
        then Left "local_alias rows do not exactly match the materialized document corpus"
        else do
          expectedPassages <- either (const (Left "search document cannot be deterministically chunked")) Right (concat <$> traverse chunkSearchDocument documents)
          let expected = sortBy (comparing searchPassageId) expectedPassages
          if passages /= expected
            then Left "search_section rows do not exactly match the materialized document corpus"
            else Right (materialization, map snd documentPairs, map (\(_, _, frame) -> frame) passageTriples, map snd aliasPairs)

validatedDocumentLoaderQuery :: Query
validatedDocumentLoaderQuery =
  "SELECT item_id,adr_id,candidate_record_id,title,summary,context,decision,consequences,domains,rationale,identifier_source,identifiers,other,scope,source_paths,obsolete,conflicted,state_token,"
    <> "item_id || char(0) || adr_id || char(0) || candidate_record_id || char(0) || title || char(0) || summary || char(0) || context || char(0) || decision || char(0) || consequences || char(0) || domains || char(0) || rationale || char(0) || identifier_source || char(0) || identifiers || char(0) || other || char(0) || scope || char(0) || source_paths || char(0) || CASE obsolete WHEN 1 THEN 'true' ELSE 'false' END || char(0) || CASE conflicted WHEN 1 THEN 'true' ELSE 'false' END || char(0) || state_token "
    <> "FROM search_document ORDER BY item_id"

validatedPassageLoaderQuery :: Query
validatedPassageLoaderQuery =
  "SELECT passage_rowid,item_id,search_item_id,adr_id,candidate_record_id,section_kind,ordinal,line_start,line_end,text,weight,source_paths,identifiers,"
    <> "item_id || char(0) || search_item_id || char(0) || adr_id || char(0) || candidate_record_id || char(0) || section_kind || char(0) || ordinal || char(0) || line_start || char(0) || line_end || char(0) || text || char(0) || printf('%.6f',weight) || char(0) || source_paths || char(0) || identifiers "
    <> "FROM search_section ORDER BY passage_rowid"

validatedAliasLoaderQuery :: Query
validatedAliasLoaderQuery =
  "SELECT alias,expansion,alias || char(0) || expansion FROM local_alias ORDER BY alias"

decodeValidatedDocument :: ValidatedRawDocumentRow -> Either Text (SearchDocument, Text)
decodeValidatedDocument (ValidatedRawDocumentRow itemRaw adrRaw recordRaw titleRaw summaryRaw contextRaw decisionRaw consequencesRaw domainsRaw rationaleRaw identifierSourceRaw identifiersRaw otherRaw scopeRaw sourcePathsRaw obsoleteRaw conflictedRaw stateRaw frameRaw) = do
  itemId <- validatedRequiredText "search_document.item_id" itemRaw
  adr <- validatedTypedId "search_document.adr_id" mkAdrId adrRaw
  record <- validatedTypedId "search_document.candidate_record_id" mkRecordId recordRaw
  title <- validatedRequiredText "search_document.title" titleRaw
  summary <- validatedRequiredText "search_document.summary" summaryRaw
  context <- validatedRequiredText "search_document.context" contextRaw
  decision <- validatedRequiredText "search_document.decision" decisionRaw
  consequences <- validatedRequiredText "search_document.consequences" consequencesRaw
  domains <- validatedNewlineList "search_document.domains" domainsRaw
  rationale <- validatedRequiredText "search_document.rationale" rationaleRaw
  identifierSource <- validatedRequiredText "search_document.identifier_source" identifierSourceRaw
  identifiers <- validatedRequiredText "search_document.identifiers" identifiersRaw
  other <- validatedRequiredText "search_document.other" otherRaw
  scope <- validatedNewlineList "search_document.scope" scopeRaw
  sourcePaths <- validatedNewlineList "search_document.source_paths" sourcePathsRaw
  obsolete <- validatedStrictBool "search_document.obsolete" obsoleteRaw
  conflicted <- validatedStrictBool "search_document.conflicted" conflictedRaw
  state <- validatedTypedId "search_document.state_token" mkStateToken stateRaw
  frame <- validatedFingerprintText "search_document fingerprint frame" frameRaw
  let expectedItem = if conflicted then adrIdText adr <> "@" <> recordIdText record else adrIdText adr
  if itemId /= expectedItem
    then Left "search_document.item_id is inconsistent with its ADR, candidate, or conflict state"
    else Right
      ( SearchDocument
          { searchDocumentItemId = itemId,
            searchDocumentAdrId = adr,
            searchDocumentCandidateRecordId = record,
            searchDocumentTitle = title,
            searchDocumentSummary = summary,
            searchDocumentContext = context,
            searchDocumentDecision = decision,
            searchDocumentConsequences = consequences,
            searchDocumentDomains = domains,
            searchDocumentRationale = rationale,
            searchDocumentOther = other,
            searchDocumentScope = scope,
            searchDocumentObsolete = obsolete,
            searchDocumentConflicted = conflicted,
            searchDocumentStateToken = state,
            searchDocumentSourcePaths = sourcePaths,
            searchDocumentIdentifierSource = identifierSource,
            searchDocumentIdentifiers = identifiers
          },
        frame
      )

decodeValidatedPassage :: ValidatedRawPassageRow -> Either Text (Int64, SearchPassage, Text)
decodeValidatedPassage (ValidatedRawPassageRow rowIdRaw itemRaw documentItemRaw adrRaw recordRaw kindRaw ordinalRaw lineStartRaw lineEndRaw textRaw weightRaw sourcePathsRaw identifiersRaw frameRaw) = do
  rowId <- validatedPositiveInteger "search_section.passage_rowid" rowIdRaw
  itemId <- validatedRequiredText "search_section.item_id" itemRaw
  documentItemId <- validatedRequiredText "search_section.search_item_id" documentItemRaw
  adr <- validatedTypedId "search_section.adr_id" mkAdrId adrRaw
  record <- validatedTypedId "search_section.candidate_record_id" mkRecordId recordRaw
  kindText <- validatedRequiredText "search_section.section_kind" kindRaw
  kind <- case filter ((== kindText) . sectionKindName) ([minBound .. maxBound] :: [SectionKind]) of
    [value] -> Right value
    _ -> Left "search_section.section_kind is invalid"
  ordinal <- validatedBoundedInt "search_section.ordinal" 0 ordinalRaw
  lineStart <- validatedBoundedInt "search_section.line_start" 1 lineStartRaw
  lineEnd <- validatedBoundedInt "search_section.line_end" (fromIntegral lineStart) lineEndRaw
  passageText <- validatedRequiredText "search_section.text" textRaw
  weight <- validatedFiniteWeight weightRaw
  sourcePaths <- validatedNewlineList "search_section.source_paths" sourcePathsRaw
  identifiers <- validatedRequiredText "search_section.identifiers" identifiersRaw
  frame <- validatedFingerprintText "search_section fingerprint frame" frameRaw
  Right
    ( rowId,
      SearchPassage
        { searchPassageId = itemId,
          searchPassageDocumentItemId = documentItemId,
          searchPassageAdrId = adr,
          searchPassageCandidateRecordId = record,
          searchPassageSectionKind = kind,
          searchPassageOrdinal = ordinal,
          searchPassageLineStart = lineStart,
          searchPassageLineEnd = lineEnd,
          searchPassageText = passageText,
          searchPassageWeight = weight,
          searchPassageSourcePaths = sourcePaths,
          searchPassageIdentifiers = identifiers
        },
      frame
    )

decodeValidatedAlias :: ValidatedRawAliasRow -> Either Text ((Text, Text), Text)
decodeValidatedAlias (ValidatedRawAliasRow aliasRaw expansionRaw frameRaw) = do
  alias <- validatedRequiredText "local_alias.alias" aliasRaw
  expansion <- validatedRequiredText "local_alias.expansion" expansionRaw
  frame <- validatedFingerprintText "local_alias fingerprint frame" frameRaw
  pure ((alias, expansion), frame)

validatedRequiredText :: Text -> SQLData -> Either Text Text
validatedRequiredText label value = case value of
  SQLText text | Text.any (== '\NUL') text -> Left (label <> " contains NUL")
               | otherwise -> Right text
  _ -> Left (label <> " is not TEXT")

validatedFingerprintText :: Text -> SQLData -> Either Text Text
validatedFingerprintText _ (SQLText text) = Right text
validatedFingerprintText label _ = Left (label <> " is not TEXT")

validatedTypedId :: Text -> (Text -> Either violation value) -> SQLData -> Either Text value
validatedTypedId label constructor value = do
  text <- validatedRequiredText label value
  either (const (Left (label <> " is invalid"))) Right (constructor text)

validatedNewlineList :: Text -> SQLData -> Either Text [Text]
validatedNewlineList label value = do
  text <- validatedRequiredText label value
  if Text.null text
    then Right []
    else
      let entries = Text.splitOn "\n" text
       in if any (Text.null . Text.strip) entries || any (Text.any (== '\r')) entries || Text.intercalate "\n" entries /= text
            then Left (label <> " is not a canonical newline-delimited list")
            else Right entries

validatedStrictBool :: Text -> SQLData -> Either Text Bool
validatedStrictBool _ (SQLInteger 0) = Right False
validatedStrictBool _ (SQLInteger 1) = Right True
validatedStrictBool label _ = Left (label <> " is not INTEGER 0 or 1")

validatedPositiveInteger :: Text -> SQLData -> Either Text Int64
validatedPositiveInteger _ (SQLInteger integer) | integer > 0 = Right integer
validatedPositiveInteger label _ = Left (label <> " is not a positive INTEGER")

validatedBoundedInt :: Text -> Int64 -> SQLData -> Either Text Int
validatedBoundedInt label lower value = do
  integer <- case value of
    SQLInteger candidate | candidate >= 0 -> Right candidate
    _ -> Left (label <> " is not a non-negative INTEGER")
  if integer < lower || integer > fromIntegral (maxBound :: Int)
    then Left (label <> " is outside the supported bounds")
    else Right (fromIntegral integer)

validatedFiniteWeight :: SQLData -> Either Text Double
validatedFiniteWeight value = case value of
  SQLFloat weight | finite weight && weight > 0 -> Right weight
  SQLInteger weight | weight > 0 -> Right (fromIntegral weight)
  _ -> Left "search_section.weight is not a positive finite REAL"
  where
    finite weight = not (isNaN weight || isInfinite weight)

ensureUniqueValues :: Ord value => Text -> [value] -> Either Text ()
ensureUniqueValues label = go Set.empty
  where
    go _ [] = Right ()
    go seen (value : rest)
      | Set.member value seen = Left (label <> " is duplicated")
      | otherwise = go (Set.insert value seen) rest

ensureSequentialPassages :: [(Int64, SearchPassage, Text)] -> Either Text ()
ensureSequentialPassages passages =
  if map (\(rowId, _, _) -> rowId) passages == [1 .. fromIntegral (length passages)]
    then ensureUniqueValues "search section item_id" (map (searchPassageId . (\(_, passage, _) -> passage)) passages)
    else Left "search_section.passage_rowid values are not canonical"

validateCacheConnection :: Bool -> Connection -> IO Bool
validateCacheConnection requireValidSemantics connection =
  cacheRowsAreCanonical requireValidSemantics <$> loadValidatedCacheRows connection

cacheRowsAreCanonical :: Bool -> ValidatedCacheRows scope -> Bool
cacheRowsAreCanonical requireValidSemantics rows =
  cacheRowsAreCanonicalWithMaterialization requireValidSemantics (persistedMaterializationFingerprintFromRows rows) rows

-- | The fused publication refresh derives its replacement fingerprint exactly
-- once, then supplies that closed value here.  Ordinary validators retain the
-- self-deriving entry point above.
cacheRowsAreCanonicalWithMaterialization :: Bool -> Maybe Text -> ValidatedCacheRows scope -> Bool
cacheRowsAreCanonicalWithMaterialization requireValidSemantics materializationFingerprint =
  all snd . cacheRowsCanonicalChecksWithMaterialization requireValidSemantics materializationFingerprint

cacheRowsCanonicalChecks :: Bool -> ValidatedCacheRows scope -> [(Text, Bool)]
cacheRowsCanonicalChecks requireValidSemantics rows =
  cacheRowsCanonicalChecksWithMaterialization requireValidSemantics (persistedMaterializationFingerprintFromRows rows) rows

cacheRowsCanonicalChecksWithMaterialization :: Bool -> Maybe Text -> ValidatedCacheRows scope -> [(Text, Bool)]
cacheRowsCanonicalChecksWithMaterialization requireValidSemantics materializationFingerprintValue rows =
  [ ("integrity", validatedIntegrityRows rows == [Only "ok"])
  , ("foreign-keys", null (validatedForeignKeyRows rows))
  , ("metadata-unique", uniqueMetadata (validatedMetadataRows rows))
  , ("metadata-canonical", maybe False (\semanticState -> canonicalMetadata metadata semanticState hasProvenance) semanticStateValue)
  , ("schema", canonicalSchema (validatedSchemaRows rows) (validatedSchemaObjects rows))
  , ("counts", declaredCountsMatch metadata (actualCounts rows))
  , ("projections", canonicalProjectionRows rows)
  , ("provenance", canonicalProvenanceRows rows)
  , ("current-ref", canonicalCurrentRefRows rows)
  , ("source-fingerprint", maybe False (\sourceFingerprint -> Map.lookup "source_fingerprint" metadata == Just sourceFingerprint) sourceFingerprintValue)
  , ("materialization-fingerprint", maybe False (\materializationFingerprint -> Map.lookup "materialization_fingerprint" metadata == Just materializationFingerprint) materializationFingerprintValue)
  ]
  where
    metadata = validatedMetadata rows
    hasProvenance = hasProvenanceProjection (validatedSchemaRows rows)
    semanticStateValue = persistedSemanticStateFromRows rows
    sourceFingerprintValue = persistedSourceFingerprintFromRows rows
    provenanceProjectionSchema =
      [ ("operation_commit", "CREATE TABLE operation_commit(op_id TEXT NOT NULL,commit_oid TEXT NOT NULL,classification TEXT NOT NULL,authored_s INTEGER NOT NULL,committed_s INTEGER NOT NULL,subject TEXT NOT NULL,parents_json TEXT NOT NULL,PRIMARY KEY(op_id,commit_oid))")
      , ("operation_target_coverage", "CREATE TABLE operation_target_coverage(op_id TEXT NOT NULL,target_oid TEXT NOT NULL,registration_signature TEXT NOT NULL,PRIMARY KEY(op_id,target_oid,registration_signature))")
      , ("line_landing", "CREATE TABLE line_landing(config_key TEXT NOT NULL,op_id TEXT NOT NULL,line_id TEXT NOT NULL,ref_name TEXT NOT NULL,commit_oid TEXT NOT NULL,complete INTEGER NOT NULL,PRIMARY KEY(config_key,op_id,line_id,ref_name))")
      , ("target_reachable_commit", "CREATE TABLE target_reachable_commit(target_oid TEXT NOT NULL,commit_oid TEXT NOT NULL,PRIMARY KEY(target_oid,commit_oid))")
      , ("line_config", "CREATE TABLE line_config(config_key TEXT PRIMARY KEY,config_json TEXT NOT NULL)")
      , ("ref_observation", "CREATE TABLE ref_observation(ref_name TEXT PRIMARY KEY,tip_oid TEXT NOT NULL,object_type TEXT NOT NULL)")
      ]
    canonicalSchema schemaRows schemaObjects =
      let actual = Map.fromList schemaRows
          semanticSchema =
            coldSchemaDdl
              <> [(name, ddl) | (component, ddl) <- searchOrdinarySchemaDdl, let name = case component of { SearchSchemaStorage value -> value; _ -> error "unexpected ordinary search storage component" }]
              <> [(ftsTargetTable target, ftsTargetDdl target) | target <- allFtsTargets]
          provenanceNames = Set.fromList (map fst provenanceProjectionSchema)
          expectedNames = Set.fromList (map fst semanticSchema) <> provenanceNames
          ftsShadowNames =
            Set.fromList
              [ ftsTargetTable target <> suffix
              | target <- allFtsTargets,
                suffix <- ["_data", "_idx", "_content", "_docsize", "_config"]
              ]
          permitted name = Set.member name expectedNames || Set.member name ftsShadowNames
          schemaHasProvenance = all (`Map.member` actual) provenanceNames
          semanticSchemaMatches = all (\(name, ddl) -> Map.lookup name actual == Just ddl) semanticSchema
        in schemaHasProvenance && semanticSchemaMatches && all (\(name, ddl) -> Map.lookup name actual == Just ddl) provenanceProjectionSchema
              && all permitted (Map.keys actual)
              && all (\(objectType, name, _) -> objectType == "table" && permitted name) schemaObjects
    uniqueMetadata metadataRows = Map.size (Map.fromList metadataRows) == length metadataRows
    canonicalMetadata metadataMap semanticState schemaHasProvenance =
      and
        [ Map.lookup "schema" metadataMap == Just "adrai-cache/3",
          Map.lookup "compiler_abi" metadataMap == Just "adrai-cold-compiler/1",
          Map.lookup "materializer" metadataMap == Just materializationImplementationFingerprint,
          semanticStateIsAcceptable metadataMap semanticState,
          Map.lookup "requested_revision" metadataMap == Map.lookup "resolved_oid" metadataMap,
           all (nonEmpty metadataMap) ["requested_revision", "resolved_oid", "source_fingerprint", "materialization_fingerprint"],
           all (nonNegative metadataMap) (["history_commits_scanned", "managed_source_count", "issue_count", "conflict_count", "operation_count", "search_document_count"] <> provenanceCountKeys schemaHasProvenance),
           validFingerprint metadataMap "source_fingerprint",
           validFingerprint metadataMap "materialization_fingerprint",
           provenanceMetadataIsComplete metadataMap schemaHasProvenance
         ]
    semanticStateIsAcceptable metadataMap actualState =
      let declaredState = Map.lookup "semantic_state" metadataMap
          historyComplete = Map.lookup "history_complete" metadataMap
          hasCanonicalHistoryFlag = historyComplete `elem` [Just "true", Just "false"]
       in declaredState == Just actualState
            && hasCanonicalHistoryFlag
            && if requireValidSemantics
              then declaredState == Just "valid" && historyComplete == Just "true"
              else True
    nonEmpty metadataMap key = maybe False (not . Text.null) (Map.lookup key metadataMap)
    validFingerprint metadataMap key =
      case Map.lookup key metadataMap of
        -- The SQLite writer stores the raw canonical 32-byte SHA-256 payload
        -- as unpadded base64url, rather than the human-facing @sha256:@ form
        -- used in managed documents.  Decode it instead of merely checking an
        -- arbitrary prefix, so corrupt or non-canonical metadata still fails
        -- closed while fresh compiler snapshots remain publishable.
        Just value ->
          case decodeBase64Url value of
            Right bytes -> BS.length bytes == 32
            Left _ -> False
        Nothing -> False
    validHexFingerprint metadataMap key =
      case Map.lookup key metadataMap of
        Just value -> Text.length value == 64 && Text.all isLowerHex value
        Nothing -> False
    isLowerHex character =
      (character >= '0' && character <= '9') || (character >= 'a' && character <= 'f')
    nonNegative metadataMap key =
      case Map.lookup key metadataMap >>= readNonNegative of
        Just _ -> True
        Nothing -> False
    readNonNegative raw =
      case reads (Text.unpack raw) of
        [(value, "")] | value >= (0 :: Integer) -> Just value
        _ -> Nothing
    declaredCountsMatch metadataMap = all $ \(key, actual) ->
      case Map.lookup key metadataMap >>= readNonNegative of
        Just declared -> declared == actual
        Nothing -> False
    hasProvenanceProjection schemaRows =
      let names = Set.fromList (map fst schemaRows)
       in all (`Set.member` names) (map fst provenanceProjectionSchema)
    provenanceMetadataIsComplete metadataMap schemaHasProvenance
      | schemaHasProvenance =
          validHexFingerprint metadataMap "provenance.fingerprint"
            && nonEmpty metadataMap "provenance.current_head"
            && nonEmpty metadataMap "provenance.current_ref"
            && nonEmpty metadataMap "provenance.sync_timestamp"
      | otherwise = all (`Map.notMember` metadataMap) provenanceMetadataKeys
    provenanceMetadataKeys =
      [ "provenance.fingerprint"
      , "provenance.generation"
      , "provenance.observed_commit_count"
      , "provenance.current_head"
      , "provenance.current_ref"
      , "provenance.current_upstream"
      , "provenance.history_commits_scanned"
      , "provenance.sync_timestamp"
      ]
    provenanceCountKeys schemaHasProvenance
      | schemaHasProvenance =
          [ "provenance.generation"
          , "provenance.observed_commit_count"
          , "provenance.history_commits_scanned"
          , "provenance.operation_commit_count"
          , "provenance.operation_target_coverage_count"
          , "provenance.line_landing_count"
          , "provenance.target_reachable_commit_count"
          , "provenance.line_config_count"
          ]
      | otherwise = []
actualCounts :: ValidatedCacheRows scope -> [(Text, Integer)]
actualCounts rows =
  [ ("managed_source_count", count (validatedManagedSourceRowsInternal rows))
  , ("issue_count", count (validatedIssueRows rows))
  , ("conflict_count", count (validatedConflictRows rows))
  , ("operation_count", count (validatedOperationRowsInternal rows))
  , ("search_document_count", count (searchMaterializationDocuments (validatedSearchMaterializationInternal rows)))
  , ("provenance.operation_commit_count", count (validatedOperationCommitRowsInternal rows))
  , ("provenance.operation_target_coverage_count", count (validatedCoverageRowsInternal rows))
  , ("provenance.line_landing_count", count (validatedLandingRowsInternal rows))
  , ("provenance.target_reachable_commit_count", count (validatedTargetRowsInternal rows))
  , ("provenance.line_config_count", count (validatedLineConfigRowsInternal rows))
  ]
  where
    count = fromIntegral . length

canonicalProjectionRows :: ValidatedCacheRows scope -> Bool
canonicalProjectionRows rows =
      sort memberObjectIds == sort recordObjectIds
        && sort memberPathBlobs == sort sourcePathBlobsForMembers
        && validatedFtsProjectionParity rows
        && documentAdrIds == Set.fromList (map reducedAdr (validatedReducedRows rows))
        && documentDecisionCandidates == expectedDecisionCandidates
        && canonicalCurrentConnectionProjection rows
  where
    documents = searchMaterializationDocuments (validatedSearchMaterializationInternal rows)
    documentAdrIds = Set.fromList (map (adrIdText . searchDocumentAdrId) documents)
    documentDecisionCandidates =
      sort
        [ (adrIdText (searchDocumentAdrId document), recordIdText (searchDocumentCandidateRecordId document), searchDocumentConflicted document)
        | document <- documents
        ]
    decisionHeads = [(adr, objectId) | (adr, axis, _, objectId) <- validatedAxisRows rows, axis == "decision"]
    decisionHeadCounts = Map.fromListWith (+) [(adr, 1 :: Int) | (adr, _) <- decisionHeads]
    expectedDecisionCandidates =
      sort
        [ (adr, objectId, Map.findWithDefault 0 adr decisionHeadCounts > 1)
        | (adr, objectId) <- decisionHeads
        ]
    memberObjectIds = [objectId | (_, objectId, _, _, _, _, _) <- validatedMemberRowsInternal rows]
    recordObjectIds = [recordId | (recordId, _, _, _, _, _, _, _) <- validatedDecisionRows rows] <> [connectionId | (connectionId, _, _, _, _, _, _) <- validatedConnectionRows rows]
    memberPathBlobs = [(path, blobOid) | (_, _, _, _, _, path, blobOid) <- validatedMemberRowsInternal rows]
    sourceByPath = Map.fromList [(path, oid) | (path, oid, _, _, _, _) <- validatedManagedSourceRowsInternal rows]
    sourcePathBlobsForMembers = [(path, oid) | (_, _, _, _, _, path, _) <- validatedMemberRowsInternal rows, Just oid <- [Map.lookup path sourceByPath]]
    reducedAdr (adr, _, _) = adr

canonicalCurrentConnectionProjection :: ValidatedCacheRows scope -> Bool
canonicalCurrentConnectionProjection rows =
  all (uncurry canonicalGroup) (Map.toList grouped)
    && all (`Map.member` connectionIndex) [connectionId | (_, _, _, connectionId) <- validatedCurrentRows rows]
  where
    connectionIndex = Map.fromList [(connectionId, (recordAdr, relationKind)) | (connectionId, recordAdr, _, relationKind, _, _, _) <- validatedConnectionRows rows]
    grouped = Map.fromListWith (flip (<>)) [(adr, [(axis, ordinal, connectionId)]) | (adr, axis, ordinal, connectionId) <- validatedCurrentRows rows]
    canonicalGroup adr entries =
      and
        [ Map.lookup connectionId connectionIndex == Just (adr, relationKind)
            && expectedAxis relationKind == Just axis
            && ordinal == fromIntegral position
        | (position, (axis, ordinal, connectionId)) <- zip [0 :: Int ..] (sortBy (comparing (\(_, _, connectionId) -> connectionId)) entries)
        , let relationKind = maybe "" snd (Map.lookup connectionId connectionIndex)
        ]
    expectedAxis relationKind = case relationKind of
      "amends" -> Just "decision"
      "applies_to" -> Just "scope"
      "domains" -> Just "domain"
      "status" -> Just "status"
      _ -> Nothing

canonicalProvenanceRows :: ValidatedCacheRows scope -> Bool
canonicalProvenanceRows rows =
  case Map.lookup "resolved_oid" metadata of
    Nothing -> False
    Just target ->
      all ((`Set.member` operationIds) . commitOperation) commits
        && all ((`Set.member` operationIds) . memberOperation) members
        && all (\(_, _, classification, _, _, _, _) -> classification `elem` ["original", "copy", "introduction", "landing"]) commits
        && all (\(_, commitOid, _, _, _, _, parents) -> validOidTextCanonical commitOid && canonicalParentOidsValue parents) commits
        && all ((`Set.member` operationIds) . landingOperation) landings
        && all (\(config, _, _, _, _, complete) -> Set.member config configKeys && complete `elem` [0, 1]) landings
        && all (\(_, commitOid, _, _, _, _, _) -> Set.member commitOid reachable) commits
        && all (\(_, _, _, _, commitOid, _) -> Set.member commitOid reachable) landings
        && all coverageHasReachableCommit coverage
        && all provenanceIssueHasOperation (validatedIssueRows rows)
        && all (`Map.member` expectedSignatures) (Set.toList operationIds)
        && sort coverage == sort expectedCoverage
        && not (null targetRows)
        && fromIntegral (length targetRows) == declaredTargetCount
        && not (validatedHasOtherTargets rows)
        && all (\(targetOid, commitOid) -> targetOid == target && validOidTextCanonical targetOid && validOidTextCanonical commitOid) targetRows
        && Set.member target reachable
        && case validatedLineConfigRowsInternal rows of
             [configRow] -> canonicalLineConfigValue configRow
             _ -> False
  where
    metadata = validatedMetadata rows
    operations = validatedOperationRowsInternal rows
    members = validatedMemberRowsInternal rows
    commits = validatedOperationCommitRowsInternal rows
    coverage = validatedCoverageRowsInternal rows
    landings = validatedLandingRowsInternal rows
    targetRows = validatedTargetRowsInternal rows
    reachable = validatedTargetCommitSet rows
    operationIds = Set.fromList [operationId | OperationRow operationId _ _ _ _ _ _ _ _ _ _ _ _ <- operations]
    configKeys = Set.fromList (map fst (validatedLineConfigRowsInternal rows))
    commitOperation (operationId, _, _, _, _, _, _) = operationId
    memberOperation (operationId, _, _, _, _, _, _) = operationId
    landingOperation (_, operationId, _, _, _, _) = operationId
    commitsByOperation = Map.fromListWith (<>) [(operationId, [commitOid]) | (operationId, commitOid, _, _, _, _, _) <- commits]
    coverageHasReachableCommit (operationId, targetOid, _) =
      Map.lookup "resolved_oid" metadata == Just targetOid
        && maybe False (any (`Set.member` reachable)) (Map.lookup operationId commitsByOperation)
    provenanceIssueHasOperation (_, _, _, origin, _, _, maybeOperation, _, _, _) =
      origin /= "provenance" || maybe True (`Set.member` operationIds) maybeOperation
    expectedSignatures =
      Map.map (operationSignatureText . operationMemberSignature)
        (Map.fromListWith (<>) [(operationId, [(objectId, path, blobOid, semanticDigest)]) | (operationId, objectId, _, _, semanticDigest, path, blobOid) <- members])
    expectedCoverage =
      case Map.lookup "resolved_oid" metadata of
        Nothing -> []
        Just target -> [(operationId, target, signature) | operationId <- sort (Set.toList operationIds), Just signature <- [Map.lookup operationId expectedSignatures]]
    declaredTargetCount = maybe (-1) id (Map.lookup "provenance.target_reachable_commit_count" metadata >>= readNonNegativeText)

canonicalCurrentRefRows :: ValidatedCacheRows scope -> Bool
canonicalCurrentRefRows rows =
  case (Map.lookup "provenance.current_ref" metadata, Map.lookup "provenance.current_head" metadata) of
    (Just "HEAD", Just headOid) -> not (Text.null headOid)
    (Just refName, Just headOid) -> Map.lookup refName observations == Just headOid
    _ -> False
  where
    metadata = validatedMetadata rows
    observations = Map.fromList [(refName, tipOid) | (refName, tipOid, _) <- validatedRefObservationRows rows]

validOidTextCanonical :: Text -> Bool
validOidTextCanonical oid =
  (Text.length oid == 40 || Text.length oid == 64)
    && Text.all (\character -> isDigit character || (character >= 'a' && character <= 'f')) oid

canonicalParentOidsValue :: Text -> Bool
canonicalParentOidsValue storedJson =
  case eitherDecodeStrict' (TextEncoding.encodeUtf8 storedJson) :: Either String [Text] of
    Left _ -> False
    Right parents ->
      storedJson == TextEncoding.decodeUtf8 (Lazy.toStrict (encode parents))
        && all validOidTextCanonical parents
        && length parents == Set.size (Set.fromList parents)

canonicalLineConfigValue :: (Text, Text) -> Bool
canonicalLineConfigValue (storedKey, storedJson) =
  case eitherDecodeStrict' (TextEncoding.encodeUtf8 storedJson) of
    Left (_ :: String) -> False
    Right value ->
      case parseMaybe parseLineConfig value of
        Nothing -> False
        Just (decisions, connections, logicalLines) ->
          storedJson == TextEncoding.decodeUtf8 (Lazy.toStrict (encode value))
            && storedKey == configKey decisions connections logicalLines
  where
    parseLineConfig :: Value -> Parser (Text, Text, [Text])
    parseLineConfig = withObject "line config" $ \config ->
      if Set.fromList (map AesonKey.toText (AesonKeyMap.keys config)) == Set.fromList ["decisions", "connections", "logical_lines"]
        then (,,) <$> config .: "decisions" <*> config .: "connections" <*> config .: "logical_lines"
        else fail "line config has noncanonical keys"

readNonNegativeText :: Text -> Maybe Integer
readNonNegativeText raw = case reads (Text.unpack raw) of
  [(value, "")] | value >= 0 -> Just value
  _ -> Nothing

-- | The cold writer verifies these bidirectional keys before publication.  A
-- cache reader repeats the proof so a readable database with deleted or
-- spliced logical rows cannot become reuse evidence.
-- | Render the shared operation-member digest exactly as provenance
-- registration does.  Cache validation recomputes this from persisted v2
-- members, so a copied certificate cannot be retargeted or spliced.
operationSignatureText :: Digest -> Text
operationSignatureText (Digest bytes) = Text.concat (map byteHex (BS.unpack bytes))
  where
    byteHex byte = Text.pack [hexDigit (byte `div` 16), hexDigit (byte `mod` 16)]
    hexDigit nibble
      | nibble < 10 = toEnum (fromEnum '0' + fromIntegral nibble)
      | otherwise = toEnum (fromEnum 'a' + fromIntegral nibble - 10)

-- | Check each external-content FTS projection without materializing its
-- payload in Haskell.  Every proof uses BINARY text comparison, rejects NULL
-- and non-TEXT virtual-table payloads, proves both directions and cardinality,
-- and independently proves the logical item key is unique.  Including rowid
-- in the passage projections binds every FTS row to its canonical passage.
validateFtsProjectionParity :: Maybe CacheValidationWorkHooks -> Connection -> IO Bool
validateFtsProjectionParity maybeWorkHooks connection = and <$> traverse validate ftsProjectionSpecs
  where
    validate (name, actualProjection, expectedProjection, actualKeyProjection, expectedKeyProjection, malformedPayloadPredicate) = do
      maybe (pure ()) (\workHooks -> recordCacheValidationWork workHooks (CacheValidationFtsParityAttempt name)) maybeWorkHooks
      result <- try @SomeException $ do
        actualOnly <- projectionDifferenceIsEmpty actualProjection expectedProjection
        expectedOnly <- projectionDifferenceIsEmpty expectedProjection actualProjection
        sameCardinality <- projectionCardinalitiesMatch actualProjection expectedProjection
        actualKeysUnique <- projectionIsUnique actualKeyProjection
        expectedKeysUnique <- projectionIsUnique expectedKeyProjection
        wellTypedPayload <- projectionHasNoMalformedPayload name malformedPayloadPredicate
        pure (actualOnly && expectedOnly && sameCardinality && actualKeysUnique && expectedKeysUnique && wellTypedPayload)
      case result of
        Left exception -> do
          maybe (pure ()) (\workHooks -> recordCacheValidationWork workHooks (CacheValidationFtsParityFailure name)) maybeWorkHooks
          throwIO exception
        Right valid -> do
          maybe (pure ()) (\workHooks -> recordCacheValidationWork workHooks (CacheValidationFtsParityCheck name)) maybeWorkHooks
          pure valid

    projectionDifferenceIsEmpty leftProjection rightProjection = do
      rows <- query_ connection (asQuery ("SELECT 1 FROM (" <> leftProjection <> " EXCEPT " <> rightProjection <> ") LIMIT 1")) :: IO [Only Int]
      pure (null rows)

    projectionCardinalitiesMatch leftProjection rightProjection = do
      leftRows <- query_ connection (asQuery ("SELECT count(*) FROM (" <> leftProjection <> ")")) :: IO [Only Int64]
      rightRows <- query_ connection (asQuery ("SELECT count(*) FROM (" <> rightProjection <> ")")) :: IO [Only Int64]
      pure (leftRows == rightRows)

    projectionIsUnique keyProjection = do
      duplicate <- query_ connection (asQuery ("SELECT 1 FROM (" <> keyProjection <> ") GROUP BY item_id HAVING count(*) <> 1 LIMIT 1")) :: IO [Only Int]
      pure (null duplicate)

    projectionHasNoMalformedPayload table predicate = do
      malformed <- query_ connection (asQuery ("SELECT 1 FROM " <> table <> " WHERE " <> predicate <> " LIMIT 1")) :: IO [Only Int]
      pure (null malformed)

    asQuery = fromString . Text.unpack

ftsProjectionSpecs :: [(Text, Text, Text, Text, Text, Text)]
ftsProjectionSpecs =
  [ ( "fts_search_exact"
    , "SELECT item_id COLLATE BINARY,adr_id COLLATE BINARY,candidate_record_id COLLATE BINARY,title COLLATE BINARY,summary COLLATE BINARY,decision COLLATE BINARY,domains COLLATE BINARY,rationale COLLATE BINARY,context COLLATE BINARY,consequences COLLATE BINARY,identifiers COLLATE BINARY FROM fts_search_exact"
    , "SELECT item_id COLLATE BINARY,adr_id COLLATE BINARY,candidate_record_id COLLATE BINARY,title COLLATE BINARY,summary COLLATE BINARY,decision COLLATE BINARY,domains COLLATE BINARY,rationale COLLATE BINARY,context COLLATE BINARY,consequences COLLATE BINARY,identifiers COLLATE BINARY FROM search_document"
    , "SELECT item_id COLLATE BINARY FROM fts_search_exact"
    , "SELECT item_id COLLATE BINARY FROM search_document"
    , "item_id IS NULL OR typeof(item_id) <> 'text' OR adr_id IS NULL OR typeof(adr_id) <> 'text' OR candidate_record_id IS NULL OR typeof(candidate_record_id) <> 'text' OR title IS NULL OR typeof(title) <> 'text' OR summary IS NULL OR typeof(summary) <> 'text' OR decision IS NULL OR typeof(decision) <> 'text' OR domains IS NULL OR typeof(domains) <> 'text' OR rationale IS NULL OR typeof(rationale) <> 'text' OR context IS NULL OR typeof(context) <> 'text' OR consequences IS NULL OR typeof(consequences) <> 'text' OR identifiers IS NULL OR typeof(identifiers) <> 'text'"
    )
  , ( "fts_search_stemmed"
    , "SELECT item_id COLLATE BINARY,adr_id COLLATE BINARY,candidate_record_id COLLATE BINARY,title COLLATE BINARY,summary COLLATE BINARY,decision COLLATE BINARY,rationale COLLATE BINARY,context COLLATE BINARY,consequences COLLATE BINARY FROM fts_search_stemmed"
    , "SELECT item_id COLLATE BINARY,adr_id COLLATE BINARY,candidate_record_id COLLATE BINARY,title COLLATE BINARY,summary COLLATE BINARY,decision COLLATE BINARY,rationale COLLATE BINARY,context COLLATE BINARY,consequences COLLATE BINARY FROM search_document"
    , "SELECT item_id COLLATE BINARY FROM fts_search_stemmed"
    , "SELECT item_id COLLATE BINARY FROM search_document"
    , "item_id IS NULL OR typeof(item_id) <> 'text' OR adr_id IS NULL OR typeof(adr_id) <> 'text' OR candidate_record_id IS NULL OR typeof(candidate_record_id) <> 'text' OR title IS NULL OR typeof(title) <> 'text' OR summary IS NULL OR typeof(summary) <> 'text' OR decision IS NULL OR typeof(decision) <> 'text' OR rationale IS NULL OR typeof(rationale) <> 'text' OR context IS NULL OR typeof(context) <> 'text' OR consequences IS NULL OR typeof(consequences) <> 'text'"
    )
  , ( "fts_search_identifier"
    , "SELECT item_id COLLATE BINARY,adr_id COLLATE BINARY,candidate_record_id COLLATE BINARY,identifiers COLLATE BINARY FROM fts_search_identifier"
    , "SELECT item_id COLLATE BINARY,adr_id COLLATE BINARY,candidate_record_id COLLATE BINARY,identifiers COLLATE BINARY FROM search_document"
    , "SELECT item_id COLLATE BINARY FROM fts_search_identifier"
    , "SELECT item_id COLLATE BINARY FROM search_document"
    , "item_id IS NULL OR typeof(item_id) <> 'text' OR adr_id IS NULL OR typeof(adr_id) <> 'text' OR candidate_record_id IS NULL OR typeof(candidate_record_id) <> 'text' OR identifiers IS NULL OR typeof(identifiers) <> 'text'"
    )
  , ( "fts_passage_exact"
    , "SELECT rowid,item_id COLLATE BINARY,adr_id COLLATE BINARY,candidate_record_id COLLATE BINARY,section_kind COLLATE BINARY,text COLLATE BINARY,identifiers COLLATE BINARY FROM fts_passage_exact"
    , "SELECT passage_rowid,item_id COLLATE BINARY,adr_id COLLATE BINARY,candidate_record_id COLLATE BINARY,section_kind COLLATE BINARY,text COLLATE BINARY,identifiers COLLATE BINARY FROM search_section"
    , "SELECT item_id COLLATE BINARY FROM fts_passage_exact"
    , "SELECT item_id COLLATE BINARY FROM search_section"
    , "typeof(rowid) <> 'integer' OR item_id IS NULL OR typeof(item_id) <> 'text' OR adr_id IS NULL OR typeof(adr_id) <> 'text' OR candidate_record_id IS NULL OR typeof(candidate_record_id) <> 'text' OR section_kind IS NULL OR typeof(section_kind) <> 'text' OR text IS NULL OR typeof(text) <> 'text' OR identifiers IS NULL OR typeof(identifiers) <> 'text'"
    )
  , ( "fts_passage_stemmed"
    , "SELECT rowid,item_id COLLATE BINARY,adr_id COLLATE BINARY,candidate_record_id COLLATE BINARY,section_kind COLLATE BINARY,text COLLATE BINARY FROM fts_passage_stemmed"
    , "SELECT passage_rowid,item_id COLLATE BINARY,adr_id COLLATE BINARY,candidate_record_id COLLATE BINARY,section_kind COLLATE BINARY,text COLLATE BINARY FROM search_section"
    , "SELECT item_id COLLATE BINARY FROM fts_passage_stemmed"
    , "SELECT item_id COLLATE BINARY FROM search_section"
    , "typeof(rowid) <> 'integer' OR item_id IS NULL OR typeof(item_id) <> 'text' OR adr_id IS NULL OR typeof(adr_id) <> 'text' OR candidate_record_id IS NULL OR typeof(candidate_record_id) <> 'text' OR section_kind IS NULL OR typeof(section_kind) <> 'text' OR text IS NULL OR typeof(text) <> 'text'"
    )
  , ( "fts_passage_identifier"
    , "SELECT rowid,item_id COLLATE BINARY,adr_id COLLATE BINARY,candidate_record_id COLLATE BINARY,section_kind COLLATE BINARY,identifiers COLLATE BINARY FROM fts_passage_identifier"
    , "SELECT passage_rowid,item_id COLLATE BINARY,adr_id COLLATE BINARY,candidate_record_id COLLATE BINARY,section_kind COLLATE BINARY,identifiers COLLATE BINARY FROM search_section"
    , "SELECT item_id COLLATE BINARY FROM fts_passage_identifier"
    , "SELECT item_id COLLATE BINARY FROM search_section"
    , "typeof(rowid) <> 'integer' OR item_id IS NULL OR typeof(item_id) <> 'text' OR adr_id IS NULL OR typeof(adr_id) <> 'text' OR candidate_record_id IS NULL OR typeof(candidate_record_id) <> 'text' OR section_kind IS NULL OR typeof(section_kind) <> 'text' OR identifiers IS NULL OR typeof(identifiers) <> 'text'"
    )
  ]

-- | The current-connection table is a canonical graph projection rather than
-- merely an ID list.  Its writer emits connection-ID order and records the
-- relation-derived axis and the source ADR on every row; verify all three so
-- an edit to fields outside the materialization fingerprint still fails closed.
canonicalCurrentConnectionRows :: Connection -> IO Bool
canonicalCurrentConnectionRows connection = do
  rows <- query_ connection
    "SELECT current.adr_id,current.axis,current.ordinal,current.connection_id,record.adr_id,record.relation_kind FROM current_connection AS current JOIN connection_record AS record ON record.connection_id=current.connection_id ORDER BY current.adr_id,current.ordinal" :: IO [(Text, Text, Int64, Text, Text, Text)]
  let grouped = Map.fromListWith (flip (<>)) [(adr, [(axis, ordinal, connectionId, recordAdr, relationKind)]) | (adr, axis, ordinal, connectionId, recordAdr, relationKind) <- rows]
  pure $ all (uncurry canonicalGroup) (Map.toList grouped)
  where
    canonicalGroup adr entries =
      and
        [ recordAdr == adr
            && expectedAxis relationKind == Just axis
            && ordinal == fromIntegral position
        | (position, (axis, ordinal, _connectionId, recordAdr, relationKind)) <- zip [0 :: Int ..] (sortBy (comparing (\(_, _, connectionId, _, _) -> connectionId)) entries)
        ]
    expectedAxis relationKind =
      case relationKind of
        "amends" -> Just "decision"
        "applies_to" -> Just "scope"
        "domains" -> Just "domain"
        "status" -> Just "status"
        _ -> Nothing

-- | Reconstruct the writer's source digest directly from immutable persisted
-- rows.  Metadata alone is not evidence: every framing byte is recovered from
-- @repository_config@ and @managed_source@ in the same canonical path order
-- used by the cold compiler.
persistedSourceFingerprint :: Connection -> IO (Maybe Text)
persistedSourceFingerprint connection = do
  configs <- query_ connection "SELECT origin,path,oid,object_type,mode,bytes,decision_root,connection_root FROM repository_config WHERE singleton=1" :: IO [(Text, Text, Maybe Text, Maybe Text, Maybe Text, Maybe BS.ByteString, Maybe Text, Maybe Text)]
  sources <- query_ connection "SELECT path,oid,object_type,mode,bytes FROM managed_source ORDER BY path" :: IO [(Text, Text, Text, Text, Maybe BS.ByteString)]
  pure $ case configs of
    [(origin, path, maybeOid, maybeObjectType, maybeMode, maybeBytes, decisionRoot, connectionRoot)]
      | origin `elem` ["default", "committed"]
          && path == ".adrai.toml"
          && configEntryIsCoherent maybeOid maybeObjectType maybeMode
          && configOriginIsCoherent origin maybeOid maybeObjectType maybeMode maybeBytes
          && managedRootsAreCoherent decisionRoot connectionRoot
          && all sourceIsCoherent sources ->
          let configEntry = case (maybeOid, maybeObjectType, maybeMode) of
                (Just oid, Just objectType, Just mode) -> treeEntryFrame path mode objectType oid
                _ -> "default"
              configPaths = case (decisionRoot, connectionRoot) of
                (Just decisions, Just connections) -> TextEncoding.encodeUtf8 (decisions <> "\NUL" <> connections)
                _ -> "unavailable"
              state0 = sha256FrameStateFeed sha256FrameStateInit "adrai-source/1\NUL"
              state1 = foldl' sha256FrameStateFeed state0
                [ framedBytes (TextEncoding.encodeUtf8 origin)
                , framedBytes configEntry
                , framedBytes (maybe "" id maybeBytes)
                , framedBytes configPaths
                ]
              state2 = foldl' feedSource state1 sources
           in Just (encodeBase64Url (digestBytes (sha256FrameStateFinalize state2)))
    _ -> Nothing
  where
    configEntryIsCoherent Nothing Nothing Nothing = True
    configEntryIsCoherent (Just _) (Just objectType) (Just mode) = objectType `elem` objectTypes && mode `elem` fileModes
    configEntryIsCoherent _ _ _ = False
    configOriginIsCoherent "default" Nothing Nothing Nothing Nothing = True
    configOriginIsCoherent "committed" (Just _) (Just _) (Just _) _ = True
    configOriginIsCoherent _ _ _ _ _ = False
    managedRootsAreCoherent Nothing Nothing = True
    managedRootsAreCoherent (Just _) (Just _) = True
    managedRootsAreCoherent _ _ = False
    sourceIsCoherent (_, _, objectType, mode, maybeBytes) =
      objectType `elem` objectTypes
        && mode `elem` fileModes
        && (objectType == "blob") == maybe False (const True) maybeBytes
    feedSource state (path, oid, objectType, mode, maybeBytes) =
      sha256FrameStateFeed
        (sha256FrameStateFeed state (framedBytes (treeEntryFrame path mode objectType oid)))
        (framedBytes (maybe "" id maybeBytes))
    objectTypes = ["blob", "tree", "commit", "tag"]
    fileModes = ["100644", "100755", "120000", "160000", "040000"]

persistedSourceFingerprintFromRows :: ValidatedCacheRows scope -> Maybe Text
persistedSourceFingerprintFromRows rows =
  case validatedRepositoryConfigRowsInternal rows of
    [(origin, path, maybeOid, maybeObjectType, maybeMode, maybeBytes, _parseState, decisionRoot, connectionRoot)]
      | origin `elem` ["default", "committed"]
          && path == ".adrai.toml"
          && configEntryIsCoherent maybeOid maybeObjectType maybeMode
          && configOriginIsCoherent origin maybeOid maybeObjectType maybeMode maybeBytes
          && managedRootsAreCoherent decisionRoot connectionRoot
          && all sourceIsCoherent sources ->
          let configEntry = case (maybeOid, maybeObjectType, maybeMode) of
                (Just oid, Just objectType, Just mode) -> treeEntryFrame path mode objectType oid
                _ -> "default"
              configPaths = case (decisionRoot, connectionRoot) of
                (Just decisions, Just connections) -> TextEncoding.encodeUtf8 (decisions <> "\NUL" <> connections)
                _ -> "unavailable"
              state0 = sha256FrameStateFeed sha256FrameStateInit "adrai-source/1\NUL"
              state1 = foldl' sha256FrameStateFeed state0
                [ framedBytes (TextEncoding.encodeUtf8 origin)
                , framedBytes configEntry
                , framedBytes (maybe "" id maybeBytes)
                , framedBytes configPaths
                ]
              state2 = foldl' feedSource state1 sources
           in Just (encodeBase64Url (digestBytes (sha256FrameStateFinalize state2)))
    _ -> Nothing
  where
    sources = validatedManagedSourceRowsInternal rows
    configEntryIsCoherent Nothing Nothing Nothing = True
    configEntryIsCoherent (Just _) (Just objectType) (Just mode) = objectType `elem` objectTypes && mode `elem` fileModes
    configEntryIsCoherent _ _ _ = False
    configOriginIsCoherent "default" Nothing Nothing Nothing Nothing = True
    configOriginIsCoherent "committed" (Just _) (Just _) (Just _) _ = True
    configOriginIsCoherent _ _ _ _ _ = False
    managedRootsAreCoherent Nothing Nothing = True
    managedRootsAreCoherent (Just _) (Just _) = True
    managedRootsAreCoherent _ _ = False
    sourceIsCoherent (_, _, objectType, mode, maybeBytes, _) =
      objectType `elem` objectTypes
        && mode `elem` fileModes
        && (objectType == "blob") == maybe False (const True) maybeBytes
    feedSource state (path, oid, objectType, mode, maybeBytes, _) =
      sha256FrameStateFeed
        (sha256FrameStateFeed state (framedBytes (treeEntryFrame path mode objectType oid)))
        (framedBytes (maybe "" id maybeBytes))
    objectTypes = ["blob", "tree", "commit", "tag"]
    fileModes = ["100644", "100755", "120000", "160000", "040000"]

treeEntryFrame :: Text -> Text -> Text -> Text -> BS.ByteString
treeEntryFrame path mode objectType oid = TextEncoding.encodeUtf8 (Text.intercalate "\NUL" [path, mode, objectType, oid])

framedBytes :: BS.ByteString -> BS.ByteString
framedBytes bytes = TextEncoding.encodeUtf8 (Text.pack (show (BS.length bytes)) <> ":") <> bytes <> "\NUL"

-- | The semantic-state label is a derived claim, not caller-controlled
-- metadata.  Conflict rows synthesize ADR_CONFLICT issues, so errors are
-- counted separately from those rows just as the cold compiler does.
persistedSemanticState :: Connection -> IO (Maybe Text)
persistedSemanticState connection = do
  -- Provenance diagnostics describe the repository history proof rather than
  -- the compiler's semantic projection.  They are synchronized after the
  -- cold compiler writes its semantic-state commitment, so including them
  -- here would retroactively make a semantically valid archive ineligible for
  -- reuse.
  compilerErrors <- scalar "SELECT count(*) FROM issue WHERE severity='error' AND code <> 'ADR_CONFLICT' AND origin <> 'provenance'"
  conflicts <- scalar "SELECT count(*) FROM adr_conflict"
  semanticRows <- scalar "SELECT count(*) FROM operation_member"
  searchRows <- scalar "SELECT count(*) FROM search_document"
  pure $ case (compilerErrors, conflicts, semanticRows, searchRows) of
    (Just errors, _, Just 0, Just 0) | errors > 0 -> Just "invalid"
    (Just 0, Just 0, _, _) -> Just "valid"
    (Just 0, Just count, _, _) | count > 0 -> Just "conflict"
    _ -> Nothing
  where
    scalar sql = do
      rows <- query_ connection (fromString sql) :: IO [Only Int64]
      pure $ case rows of
        [Only value] -> Just value
        _ -> Nothing

persistedSemanticStateFromRows :: ValidatedCacheRows scope -> Maybe Text
persistedSemanticStateFromRows rows =
  case (compilerErrors, length (validatedConflictRows rows), length (validatedMemberRowsInternal rows), searchCount) of
    (errors, _, 0, 0) | errors > 0 -> Just "invalid"
    (0, 0, _, _) -> Just "valid"
    (0, conflicts, _, _) | conflicts > 0 -> Just "conflict"
    _ -> Nothing
  where
    compilerErrors = length [() | (_, code, severity, origin, _, _, _, _, _, _) <- validatedIssueRows rows, severity == "error", code /= "ADR_CONFLICT", origin /= "provenance"]
    searchCount = length (searchMaterializationDocuments (validatedSearchMaterializationInternal rows))

-- | Recompute the established materialization fingerprint from normalized
-- persisted rows.  The SQL projections deliberately contain every logical
-- value (including FTS source text), so forged metadata or a payload splice
-- cannot retain a cache's reuse eligibility.
persistedMaterializationFingerprint :: Connection -> Maybe Text -> IO (Maybe Text)
persistedMaterializationFingerprint = persistedMaterializationFingerprintWith "SELECT adr_id,connection_id FROM current_connection ORDER BY adr_id,ordinal"

persistedMaterializationFingerprintWith :: String -> Connection -> Maybe Text -> IO (Maybe Text)
persistedMaterializationFingerprintWith _ _ Nothing = pure Nothing
persistedMaterializationFingerprintWith currentRowsSql connection (Just sourceFingerprint) = do
  sourceBytes <- pure (decodeBase64Url sourceFingerprint)
  diagnosticRows <- query_ connection "SELECT code,severity,origin,COALESCE(adr_id,''),COALESCE(object_id,''),COALESCE(operation_id,''),COALESCE(commit_oid,''),COALESCE(path,''),message FROM issue WHERE code <> 'ADR_CONFLICT' ORDER BY code,adr_id,object_id,operation_id,commit_oid,path,severity,CASE origin WHEN 'config' THEN 0 WHEN 'path_document' THEN 1 WHEN 'history' THEN 2 WHEN 'operation' THEN 3 WHEN 'graph' THEN 4 WHEN 'basis' THEN 5 ELSE 6 END,origin,message" :: IO [(Text, Text, Text, Text, Text, Text, Text, Text, Text)]
  conflictRows <- query_ connection "SELECT code,adr_id,candidate_count,state_token,summaries FROM adr_conflict ORDER BY adr_id" :: IO [(Text, Text, Int64, Text, Text)]
  operationRows <- query_ connection "SELECT operation_id,timestamp_ms,actor_kind,actor_id,actor_model,basis_oid,branch_hint,upstream_hint,line_anchors,tool_version FROM operation ORDER BY operation_id" :: IO [(Text, Text, Text, Text, Maybe Text, Text, Maybe Text, Maybe Text, Text, Text)]
  inputRows <- query_ connection "SELECT operation_id,input_digest,prompt_digest,context_digest FROM operation" :: IO [(Text, Maybe Text, Maybe Text, Maybe Text)]
  memberRows <- query_ connection "SELECT operation_id,object_id,event_kind,semantic_digest,path,blob_oid FROM operation_member ORDER BY operation_id,object_id,path" :: IO [(Text, Text, Text, Text, Text, Text)]
  provenanceTables <- query connection
    "SELECT name FROM sqlite_master WHERE type='table' AND name IN ('line_config','line_landing','operation_commit','operation_target_coverage','target_reachable_commit') ORDER BY name"
    () :: IO [Only Text]
  (operationCommitRows, coverageRows, landingRows, targetReachabilityRows, lineConfigRows) <- case map fromOnly provenanceTables of
    [] -> pure ([], [], [], [], [])
    ["line_config", "line_landing", "operation_commit", "operation_target_coverage", "target_reachable_commit"] -> do
      commits <- query_ connection "SELECT op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json FROM operation_commit ORDER BY op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json" :: IO [(Text, Text, Text, Int64, Int64, Text, Text)]
      coverage <- query_ connection "SELECT op_id,target_oid,registration_signature FROM operation_target_coverage ORDER BY op_id,target_oid,registration_signature" :: IO [(Text, Text, Text)]
      landings <- query_ connection "SELECT config_key,op_id,line_id,ref_name,commit_oid,complete FROM line_landing ORDER BY config_key,op_id,line_id,ref_name,commit_oid,complete" :: IO [(Text, Text, Text, Text, Text, Int64)]
      targetReachability <- query_ connection "SELECT target_oid,commit_oid FROM target_reachable_commit ORDER BY target_oid,commit_oid" :: IO [(Text, Text)]
      lineConfigs <- query_ connection "SELECT config_key,config_json FROM line_config ORDER BY config_key,config_json" :: IO [(Text, Text)]
      pure (commits, coverage, landings, targetReachability, lineConfigs)
    _ -> ioError (userError "partial provenance projection schema cannot be fingerprinted")
  parentRows <- query_ connection "SELECT operation_id,object_id,parent_object_id FROM operation_member_parent ORDER BY operation_id,object_id,ordinal" :: IO [(Text, Text, Text)]
  reducedRows <- query_ connection "SELECT adr_id,state_token FROM reduced_adr ORDER BY adr_id" :: IO [(Text, Text)]
  axisRows <- query_ connection "SELECT adr_id,axis,object_id FROM axis_head ORDER BY adr_id,axis,ordinal" :: IO [(Text, Text, Text)]
  currentRows <- query_ connection (fromString currentRowsSql) :: IO [(Text, Text)]
  documentRows <- query_ connection "SELECT item_id || char(0) || adr_id || char(0) || candidate_record_id || char(0) || title || char(0) || summary || char(0) || context || char(0) || decision || char(0) || consequences || char(0) || domains || char(0) || rationale || char(0) || identifier_source || char(0) || identifiers || char(0) || other || char(0) || scope || char(0) || source_paths || char(0) || CASE obsolete WHEN 1 THEN 'true' ELSE 'false' END || char(0) || CASE conflicted WHEN 1 THEN 'true' ELSE 'false' END || char(0) || state_token FROM search_document ORDER BY item_id" :: IO [Only Text]
  passageRows <- query_ connection "SELECT item_id || char(0) || search_item_id || char(0) || adr_id || char(0) || candidate_record_id || char(0) || section_kind || char(0) || ordinal || char(0) || line_start || char(0) || line_end || char(0) || text || char(0) || printf('%.6f',weight) || char(0) || source_paths || char(0) || identifiers FROM search_section ORDER BY item_id" :: IO [Only Text]
  aliases <- query_ connection "SELECT alias || char(0) || expansion FROM local_alias ORDER BY alias" :: IO [Only Text]
  pure $ do
    rawSource <- either (const Nothing) Just sourceBytes
    if BS.length rawSource /= 32
      then Nothing
      else do
        let memberIndex = stableListIndex (\(operation, _, _, _, _, _) -> operation) memberRows
            parentIndex = stableListIndex (\(operation, objectId, _) -> (operation, objectId)) parentRows
        operationFrames <- traverse (operationFrame (Map.fromList [(operation, (inputDigest, promptDigest, contextDigest)) | (operation, inputDigest, promptDigest, contextDigest) <- inputRows]) memberIndex parentIndex) operationRows
        let axisMap = Map.fromListWith (flip (<>)) [((adr, axis), [objectId]) | (adr, axis, objectId) <- axisRows]
            currentMap = Map.fromListWith (flip (<>)) [(adr, [connectionId]) | (adr, connectionId) <- currentRows]
            reducedFrames =
              [ framedText (Text.intercalate "\NUL" [adr, stateToken, heads "decision", heads "scope", heads "domain", heads "status", Text.intercalate "," (Map.findWithDefault [] adr currentMap)])
              | (adr, stateToken) <- reducedRows
              , let heads axis = Text.intercalate "," (Map.findWithDefault [] (adr, axis) axisMap)
              ]
            frames =
              "adrai-cold-materialization/1\NUL"
                : framedText "adrai-cache/3"
                : framedText materializationImplementationFingerprint
                : framedBytes rawSource
                : map (framedText . diagnosticRowFingerprint) diagnosticRows
                  <> map (framedText . conflictRowFingerprint) conflictRows
                  <> concat operationFrames
                   <> map (framedText . operationCommitFrame) operationCommitRows
                  <> map (framedText . coverageFrame) coverageRows
                  <> map (framedText . landingFrame) landingRows
                  <> map (framedText . targetReachabilityFrame) targetReachabilityRows
                  <> map (framedText . lineConfigFrame) lineConfigRows
                  <> reducedFrames
                  <> map (framedText . unOnly) documentRows
                  <> map (framedText . unOnly) passageRows
                  <> map (framedText . unOnly) aliases
        Just (encodeBase64Url (digestBytes (sha256FrameStateFinalize (foldl' sha256FrameStateFeed sha256FrameStateInit frames))))
  where
    unOnly (Only value) = value
    diagnosticRowFingerprint (code, severity, origin, adr, objectId, operation, commit, path, message) = Text.intercalate "\NUL" [code, severity, origin, adr, objectId, operation, commit, path, message]
    conflictRowFingerprint (code, adr, count, stateToken, summaries) = Text.intercalate "\NUL" [code, adr, Text.pack (show count), stateToken, summaries]
    coverageFrame (operation, target, signature) = Text.intercalate "\NUL" [operation, target, signature]
    landingFrame (landingConfigKey, operation, lineId, refName, commit, complete) =
      Text.intercalate "\NUL" [landingConfigKey, operation, lineId, refName, commit, Text.pack (show complete)]
    targetReachabilityFrame (target, commit) = Text.intercalate "\NUL" [target, commit]
    lineConfigFrame (lineConfigKey, configJson) = Text.intercalate "\NUL" [lineConfigKey, configJson]
    operationCommitFrame (operation, commit, classification, authored, committed, subject, parents) =
      Text.intercalate "\NUL" [operation, commit, classification, Text.pack (show authored), Text.pack (show committed), subject, parents]
    operationFrame inputs memberIndex parentIndex (operation, timestamp, actorKind, actorId, actorModel, basis, branch, upstream, anchors, toolVersion) = do
      anchorValues <- parseAnchorValues anchors
      let matchingMembers = Map.findWithDefault [] operation memberIndex
          parentsFor objectId = Text.intercalate "\n" [parent | (_, _, parent) <- Map.findWithDefault [] (operation, objectId) parentIndex]
          (inputDigest, promptDigest, contextDigest) = Map.findWithDefault (Nothing, Nothing, Nothing) operation inputs
      pure $
        [ framedText (Text.intercalate "\NUL"
            [ operation, objectId, eventKind, timestamp, actorKind, actorId, maybe "" id actorModel, basis, parentsFor objectId, maybe "" id branch, maybe "" id upstream, Text.intercalate "\n" anchorValues, "sha256:" <> semanticDigest, blobOid, toolVersion, digestText inputDigest, digestText promptDigest, digestText contextDigest, path
            ])
         | (_, objectId, eventKind, semanticDigest, path, blobOid) <- matchingMembers
        ]

-- | Accumulate duplicate-preserving buckets in the exact input order.  Each
-- input is prepended in O(1), then every completed bucket is reversed once.
-- SQL has already provided the canonical ordering, and no weaker re-sort is
-- safe for fingerprinting.
stableListIndex :: Ord key => (row -> key) -> [row] -> Map key [row]
stableListIndex key = finalizeStableBuckets . foldl' insert Map.empty
  where
    insert indexed row = prependStableBucketValue (key row) row indexed

prependStableBucketValue :: Ord key => key -> value -> Map key [value] -> Map key [value]
prependStableBucketValue key value = Map.alter (Just . maybe [value] (value :)) key

finalizeStableBucketStep :: [value] -> value -> [value]
finalizeStableBucketStep reversed value = value : reversed

finalizeStableBucket :: [value] -> [value]
finalizeStableBucket = foldl' finalizeStableBucketStep []

finalizeStableBuckets :: Map key [value] -> Map key [value]
finalizeStableBuckets = Map.map finalizeStableBucket

data MaterializationFingerprintIndexWork = MaterializationFingerprintIndexWork
  { materializationFingerprintOperationRowsVisited :: Int,
    materializationFingerprintMemberRowsConsumed :: Int,
    materializationFingerprintParentRowsConsumed :: Int,
    materializationFingerprintMemberBucketPrepends :: Int,
    materializationFingerprintParentBucketPrepends :: Int,
    materializationFingerprintMemberBucketsFinalized :: Int,
    materializationFingerprintParentBucketsFinalized :: Int,
    materializationFingerprintMemberBucketValuesReversed :: Int,
    materializationFingerprintParentBucketValuesReversed :: Int,
    materializationFingerprintMemberBucketLookups :: Int,
    materializationFingerprintParentBucketLookups :: Int
  }
  deriving (Eq, Show)

emptyMaterializationFingerprintIndexWork :: MaterializationFingerprintIndexWork
emptyMaterializationFingerprintIndexWork = MaterializationFingerprintIndexWork 0 0 0 0 0 0 0 0 0 0 0

appendMaterializationFingerprintIndexWork :: MaterializationFingerprintIndexWork -> MaterializationFingerprintIndexWork -> MaterializationFingerprintIndexWork
appendMaterializationFingerprintIndexWork left right =
  MaterializationFingerprintIndexWork
    { materializationFingerprintOperationRowsVisited = materializationFingerprintOperationRowsVisited left + materializationFingerprintOperationRowsVisited right,
      materializationFingerprintMemberRowsConsumed = materializationFingerprintMemberRowsConsumed left + materializationFingerprintMemberRowsConsumed right,
      materializationFingerprintParentRowsConsumed = materializationFingerprintParentRowsConsumed left + materializationFingerprintParentRowsConsumed right,
      materializationFingerprintMemberBucketPrepends = materializationFingerprintMemberBucketPrepends left + materializationFingerprintMemberBucketPrepends right,
      materializationFingerprintParentBucketPrepends = materializationFingerprintParentBucketPrepends left + materializationFingerprintParentBucketPrepends right,
      materializationFingerprintMemberBucketsFinalized = materializationFingerprintMemberBucketsFinalized left + materializationFingerprintMemberBucketsFinalized right,
      materializationFingerprintParentBucketsFinalized = materializationFingerprintParentBucketsFinalized left + materializationFingerprintParentBucketsFinalized right,
      materializationFingerprintMemberBucketValuesReversed = materializationFingerprintMemberBucketValuesReversed left + materializationFingerprintMemberBucketValuesReversed right,
      materializationFingerprintParentBucketValuesReversed = materializationFingerprintParentBucketValuesReversed left + materializationFingerprintParentBucketValuesReversed right,
      materializationFingerprintMemberBucketLookups = materializationFingerprintMemberBucketLookups left + materializationFingerprintMemberBucketLookups right,
      materializationFingerprintParentBucketLookups = materializationFingerprintParentBucketLookups left + materializationFingerprintParentBucketLookups right
    }

persistedMaterializationFingerprintFromRows :: ValidatedCacheRows scope -> Maybe Text
persistedMaterializationFingerprintFromRows rows =
  persistedMaterializationFingerprintFromStableIndexes
    rows
    (stableListIndex validatedMemberBucketKey (validatedMemberRowsInternal rows))
    (stableListIndex validatedParentBucketKey (validatedParentRows rows))

persistedMaterializationFingerprintFromRowsWithWork :: ValidatedCacheRows scope -> (Maybe Text, MaterializationFingerprintIndexWork)
persistedMaterializationFingerprintFromRowsWithWork rows =
  let (memberIndex, memberIndexWork) = indexMemberRowsWithWork (validatedMemberRowsInternal rows)
      (parentIndex, parentIndexWork) = indexParentRowsWithWork (validatedParentRows rows)
      indexWork = appendMaterializationFingerprintIndexWork memberIndexWork parentIndexWork
   in case materializationFingerprintRawSource rows of
        Nothing -> (Nothing, indexWork)
        Just rawSource ->
          let (maybeOperationFrames, traversalWork) =
                materializationFingerprintOperationFramesWithWork rows memberIndex parentIndex
              totalWork = appendMaterializationFingerprintIndexWork indexWork traversalWork
           in case maybeOperationFrames of
            Nothing -> (Nothing, totalWork)
            Just operationFrames ->
              ( Just (materializationFingerprintDigest rows rawSource operationFrames),
                totalWork
              )

validatedMemberBucketKey :: OperationMemberRow -> Text
validatedMemberBucketKey (operation, _, _, _, _, _, _) = operation

validatedParentBucketKey :: OperationParentRow -> (Text, Text)
validatedParentBucketKey (operation, objectId, _, _) = (operation, objectId)

data StableListIndexWork = StableListIndexWork
  { stableListRowsConsumed :: Int,
    stableListBucketPrepends :: Int,
    stableListBucketsFinalized :: Int,
    stableListBucketValuesReversed :: Int
  }

stableListIndexWithWork :: Ord key => (row -> key) -> [row] -> (Map key [row], StableListIndexWork)
stableListIndexWithWork key rows =
  let (reversedBuckets, consumed, prepends) = foldl' consume (Map.empty, 0, 0) rows
      (stableBuckets, finalized, reversedValues) = finalizeStableBucketsWithWork reversedBuckets
   in (stableBuckets, StableListIndexWork consumed prepends finalized reversedValues)
  where
    consume (indexed, consumed, prepends) row =
      (prependStableBucketValue (key row) row indexed, consumed + 1, prepends + 1)

finalizeStableBucketsWithWork :: Ord key => Map key [value] -> (Map key [value], Int, Int)
finalizeStableBucketsWithWork = Map.foldlWithKey' finalize (Map.empty, 0, 0)
  where
    finalize (stable, finalized, reversedValues) key reversedBucket =
      let (stableBucket, bucketValuesReversed) =
            foldl'
              (\(values, count) value -> (finalizeStableBucketStep values value, count + 1))
              ([], 0)
              reversedBucket
       in (Map.insert key stableBucket stable, finalized + 1, reversedValues + bucketValuesReversed)

indexMemberRowsWithWork :: [OperationMemberRow] -> (Map Text [OperationMemberRow], MaterializationFingerprintIndexWork)
indexMemberRowsWithWork rows =
  let (indexed, work) = stableListIndexWithWork validatedMemberBucketKey rows
   in ( indexed,
        emptyMaterializationFingerprintIndexWork
          { materializationFingerprintMemberRowsConsumed = stableListRowsConsumed work,
            materializationFingerprintMemberBucketPrepends = stableListBucketPrepends work,
            materializationFingerprintMemberBucketsFinalized = stableListBucketsFinalized work,
            materializationFingerprintMemberBucketValuesReversed = stableListBucketValuesReversed work
          }
      )

indexParentRowsWithWork :: [OperationParentRow] -> (Map (Text, Text) [OperationParentRow], MaterializationFingerprintIndexWork)
indexParentRowsWithWork rows =
  let (indexed, work) = stableListIndexWithWork validatedParentBucketKey rows
   in ( indexed,
        emptyMaterializationFingerprintIndexWork
          { materializationFingerprintParentRowsConsumed = stableListRowsConsumed work,
            materializationFingerprintParentBucketPrepends = stableListBucketPrepends work,
            materializationFingerprintParentBucketsFinalized = stableListBucketsFinalized work,
            materializationFingerprintParentBucketValuesReversed = stableListBucketValuesReversed work
          }
      )

persistedMaterializationFingerprintFromStableIndexes
  :: ValidatedCacheRows scope
  -> Map Text [OperationMemberRow]
  -> Map (Text, Text) [OperationParentRow]
  -> Maybe Text
persistedMaterializationFingerprintFromStableIndexes rows memberIndex parentIndex = do
  rawSource <- materializationFingerprintRawSource rows
  operationFrames <- materializationFingerprintOperationFrames rows memberIndex parentIndex
  pure (materializationFingerprintDigest rows rawSource operationFrames)

materializationFingerprintRawSource :: ValidatedCacheRows scope -> Maybe BS.ByteString
materializationFingerprintRawSource rows = do
  persistedSource <- Map.lookup "source_fingerprint" (validatedMetadata rows)
  reconstructedSource <- persistedSourceFingerprintFromRows rows
  if persistedSource == reconstructedSource then Just () else Nothing
  rawSource <- either (const Nothing) Just (decodeBase64Url persistedSource)
  if BS.length rawSource == 32 then Just rawSource else Nothing

materializationFingerprintOperationFrames
  :: ValidatedCacheRows scope
  -> Map Text [OperationMemberRow]
  -> Map (Text, Text) [OperationParentRow]
  -> Maybe [[BS.ByteString]]
materializationFingerprintOperationFrames rows memberIndex parentIndex =
  traverse (operationFrameFromRows memberIndex parentIndex) (validatedOperationRowsInternal rows)

materializationFingerprintOperationFramesWithWork
  :: ValidatedCacheRows scope
  -> Map Text [OperationMemberRow]
  -> Map (Text, Text) [OperationParentRow]
  -> (Maybe [[BS.ByteString]], MaterializationFingerprintIndexWork)
materializationFingerprintOperationFramesWithWork rows memberIndex parentIndex =
  go emptyMaterializationFingerprintIndexWork [] (validatedOperationRowsInternal rows)
  where
    go work reversedFrames [] = (Just (reverse reversedFrames), work)
    go work reversedFrames (operationRow : remainingRows) =
      let visitedWork =
            work
              { materializationFingerprintOperationRowsVisited =
                  materializationFingerprintOperationRowsVisited work + 1
              }
          (maybeFrames, nextWork) = operationFrameFromRowsWithWork memberIndex parentIndex visitedWork operationRow
       in case maybeFrames of
            Nothing -> (Nothing, nextWork)
            Just frames -> go nextWork (frames : reversedFrames) remainingRows

operationFrameFromRows
  :: Map Text [OperationMemberRow]
  -> Map (Text, Text) [OperationParentRow]
  -> OperationRow
  -> Maybe [BS.ByteString]
operationFrameFromRows memberIndex parentIndex (OperationRow operation timestamp actorKind actorId actorModel basis branch upstream anchors toolVersion inputDigest promptDigest contextDigest) = do
  anchorValues <- parseAnchorValues anchors
  let matchingMembers = lookupMemberBucket operation memberIndex
  traverse (memberFrameFromRows operation timestamp actorKind actorId actorModel basis branch upstream anchorValues toolVersion inputDigest promptDigest contextDigest parentIndex) matchingMembers

operationFrameFromRowsWithWork
  :: Map Text [OperationMemberRow]
  -> Map (Text, Text) [OperationParentRow]
  -> MaterializationFingerprintIndexWork
  -> OperationRow
  -> (Maybe [BS.ByteString], MaterializationFingerprintIndexWork)
operationFrameFromRowsWithWork memberIndex parentIndex work (OperationRow operation timestamp actorKind actorId actorModel basis branch upstream anchors toolVersion inputDigest promptDigest contextDigest) =
  case parseAnchorValues anchors of
    Nothing -> (Nothing, work)
    Just anchorValues ->
      let (matchingMembers, memberLookupWork) = observeMemberBucketLookup operation memberIndex work
          (reversedFrames, finalWork) =
            foldl'
              (observeMemberFrame operation timestamp actorKind actorId actorModel basis branch upstream anchorValues toolVersion inputDigest promptDigest contextDigest parentIndex)
              ([], memberLookupWork)
              matchingMembers
       in (Just (reverse reversedFrames), finalWork)

observeMemberFrame
  :: Text -> Text -> Text -> Text -> Maybe Text -> Text -> Maybe Text -> Maybe Text -> [Text] -> Text
  -> Maybe Text -> Maybe Text -> Maybe Text -> Map (Text, Text) [OperationParentRow]
  -> ([BS.ByteString], MaterializationFingerprintIndexWork)
  -> OperationMemberRow
  -> ([BS.ByteString], MaterializationFingerprintIndexWork)
observeMemberFrame operation timestamp actorKind actorId actorModel basis branch upstream anchorValues toolVersion inputDigest promptDigest contextDigest parentIndex (reversedFrames, work) member@(_, objectId, _, _, _, _, _) =
  let (parentsForObject, nextWork) = observeParentBucketLookup operation objectId parentIndex work
      frame = memberFrameFromResolvedParents operation timestamp actorKind actorId actorModel basis branch upstream anchorValues toolVersion inputDigest promptDigest contextDigest parentsForObject member
   in (frame : reversedFrames, nextWork)

memberFrameFromRows
  :: Text -> Text -> Text -> Text -> Maybe Text -> Text -> Maybe Text -> Maybe Text -> [Text] -> Text
  -> Maybe Text -> Maybe Text -> Maybe Text -> Map (Text, Text) [OperationParentRow] -> OperationMemberRow
  -> Maybe BS.ByteString
memberFrameFromRows operation timestamp actorKind actorId actorModel basis branch upstream anchorValues toolVersion inputDigest promptDigest contextDigest parentIndex member@(_, objectId, _, _, _, _, _) =
  let parentsForObject = lookupParentBucket operation objectId parentIndex
   in Just (memberFrameFromResolvedParents operation timestamp actorKind actorId actorModel basis branch upstream anchorValues toolVersion inputDigest promptDigest contextDigest parentsForObject member)

memberFrameFromResolvedParents
  :: Text -> Text -> Text -> Text -> Maybe Text -> Text -> Maybe Text -> Maybe Text -> [Text] -> Text
  -> Maybe Text -> Maybe Text -> Maybe Text -> [OperationParentRow] -> OperationMemberRow
  -> BS.ByteString
memberFrameFromResolvedParents operation timestamp actorKind actorId actorModel basis branch upstream anchorValues toolVersion inputDigest promptDigest contextDigest parentsForObject (_, objectId, _, eventKind, semanticDigest, path, blobOid) =
  let parents = Text.intercalate "\n" [parent | (_, _, _, parent) <- parentsForObject]
   in framedText . Text.intercalate "\NUL" $
        [ operation
        , objectId
        , eventKind
        , timestamp
        , actorKind
        , actorId
        , maybe "" id actorModel
        , basis
        , parents
        , maybe "" id branch
        , maybe "" id upstream
        , Text.intercalate "\n" anchorValues
        , "sha256:" <> semanticDigest
        , blobOid
        , toolVersion
        , digestText inputDigest
        , digestText promptDigest
        , digestText contextDigest
        , path
        ]

lookupMemberBucket :: Text -> Map Text [OperationMemberRow] -> [OperationMemberRow]
lookupMemberBucket operation = Map.findWithDefault [] operation

lookupParentBucket :: Text -> Text -> Map (Text, Text) [OperationParentRow] -> [OperationParentRow]
lookupParentBucket operation objectId = Map.findWithDefault [] (operation, objectId)

observeMemberBucketLookup
  :: Text
  -> Map Text [OperationMemberRow]
  -> MaterializationFingerprintIndexWork
  -> ([OperationMemberRow], MaterializationFingerprintIndexWork)
observeMemberBucketLookup operation memberIndex work =
  let bucket = lookupMemberBucket operation memberIndex
   in bucket `seq`
        let nextWork =
              work
                { materializationFingerprintMemberBucketLookups =
                    materializationFingerprintMemberBucketLookups work + 1
                }
         in nextWork `seq` (bucket, nextWork)

observeParentBucketLookup
  :: Text
  -> Text
  -> Map (Text, Text) [OperationParentRow]
  -> MaterializationFingerprintIndexWork
  -> ([OperationParentRow], MaterializationFingerprintIndexWork)
observeParentBucketLookup operation objectId parentIndex work =
  let bucket = lookupParentBucket operation objectId parentIndex
   in bucket `seq`
        let nextWork =
              work
                { materializationFingerprintParentBucketLookups =
                    materializationFingerprintParentBucketLookups work + 1
                }
         in nextWork `seq` (bucket, nextWork)

materializationFingerprintDigest :: ValidatedCacheRows scope -> BS.ByteString -> [[BS.ByteString]] -> Text
materializationFingerprintDigest rows rawSource operationFrames =
  encodeBase64Url . digestBytes . sha256FrameStateFinalize $
    foldl' sha256FrameStateFeed sha256FrameStateInit (fingerprintFrames rawSource)
  where
    axisMap = Map.fromListWith (flip (<>)) [((adr, axis), [objectId]) | (adr, axis, _, objectId) <- validatedAxisRows rows]
    currentMap = Map.fromListWith (flip (<>)) [(adr, [connectionId]) | (adr, _, _, connectionId) <- validatedCurrentRows rows]
    reducedFrames =
      [ framedText (Text.intercalate "\NUL" [adr, stateToken, heads "decision", heads "scope", heads "domain", heads "status", Text.intercalate "," (Map.findWithDefault [] adr currentMap)])
      | (adr, stateToken, _) <- validatedReducedRows rows
      , let heads axis = Text.intercalate "," (Map.findWithDefault [] (adr, axis) axisMap)
      ]
    diagnosticRows =
      [ (code, severity, origin, maybe "" id adr, maybe "" id objectId, maybe "" id operation, maybe "" id commit, maybe "" id path, message)
      | (_, code, severity, origin, adr, objectId, operation, commit, path, message) <- validatedIssueRows rows
      , code /= "ADR_CONFLICT"
      ]
    fingerprintFrames sourceBytes =
      "adrai-cold-materialization/1\NUL"
        : framedText "adrai-cache/3"
        : framedText materializationImplementationFingerprint
        : framedBytes sourceBytes
        : map (framedText . diagnosticFrame) diagnosticRows
          <> map (framedText . conflictFrame) (validatedConflictRows rows)
          <> concat operationFrames
          <> map (framedText . operationCommitFrame) (validatedOperationCommitRowsInternal rows)
          <> map (framedText . coverageFrame) (validatedCoverageRowsInternal rows)
          <> map (framedText . landingFrame) (validatedLandingRowsInternal rows)
          <> map (framedText . targetReachabilityFrame) (validatedTargetRowsInternal rows)
          <> map (framedText . lineConfigFrame) (validatedLineConfigRowsInternal rows)
          <> reducedFrames
          <> map framedText (validatedSearchDocumentFrames rows)
          <> map framedText (sort (validatedSearchPassageFrames rows))
          <> map framedText (validatedSearchAliasFrames rows)
    diagnosticFrame (code, severity, origin, adr, objectId, operation, commit, path, message) = Text.intercalate "\NUL" [code, severity, origin, adr, objectId, operation, commit, path, message]
    conflictFrame (adr, code, count, stateToken, summaries) = Text.intercalate "\NUL" [code, adr, Text.pack (show count), stateToken, summaries]
    coverageFrame (operation, target, signature) = Text.intercalate "\NUL" [operation, target, signature]
    landingFrame (configKey', operation, lineId, refName, commit, complete) = Text.intercalate "\NUL" [configKey', operation, lineId, refName, commit, Text.pack (show complete)]
    targetReachabilityFrame (target, commit) = Text.intercalate "\NUL" [target, commit]
    lineConfigFrame (configKey', configJson) = Text.intercalate "\NUL" [configKey', configJson]
    operationCommitFrame (operation, commit, classification, authored, committed, subject, parents) = Text.intercalate "\NUL" [operation, commit, classification, Text.pack (show authored), Text.pack (show committed), subject, parents]

framedText :: Text -> BS.ByteString
framedText = framedBytes . TextEncoding.encodeUtf8

digestText :: Maybe Text -> Text
digestText = maybe "" ("sha256:" <>)

parseAnchorValues :: Text -> Maybe [Text]
parseAnchorValues raw = do
  decoded <- either (const Nothing) Just (eitherDecodeStrict' (TextEncoding.encodeUtf8 raw) :: Either String Value)
  anchors <- parseMaybe parseAnchors decoded
  -- The writer persists the renderer's exact canonical JSON (without its
  -- trailing LF).  Parsing alone would accept a whitespace-only edit that
  -- leaves the materialization digest unchanged, so require the stored form
  -- itself to be canonical before treating it as fingerprint evidence.
  if raw == renderCanonicalAnchors anchors
    then Just [anchorId <> "@" <> commit | (anchorId, commit) <- anchors]
    else Nothing
  where
    parseAnchors :: Value -> Parser [(Text, Text)]
    parseAnchors = withArray "line anchors" (traverse parseAnchor . Vector.toList)
    parseAnchor :: Value -> Parser (Text, Text)
    parseAnchor = withObject "line anchor" $ \anchor -> do
      anchorId <- anchor .: "id"
      commit <- anchor .: "commit"
      pure (anchorId, commit)
    renderCanonicalAnchors anchors =
      Text.dropWhileEnd (== '\n')
        . renderCanonicalJson
        . JsonArray
        $ [object [("id", JsonString anchorId), ("commit", JsonString commit)] | (anchorId, commit) <- anchors]

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
computeAncestorRank repository _repoRoot targetOid candidateOid
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
      -- A failed comparison is not evidence of tree identity.  Reusing a
      -- cached projection is safe only when Git successfully proves that
      -- every relevant path is unchanged, so command failures fail closed.
      pure $ case result of
        Right processResult -> BS.null (processStdout processResult)
        Left _ -> False

-- | Prove the delta is small and compiler-irrelevant.  Endpoint tree equality
-- alone admits change/revert histories and merge-side provenance changes; the
-- per-commit name walk closes that hole.  We deliberately require a nonempty
-- managed-path set and cap the proof to a modest history window, failing cold
-- whenever Git cannot establish either fact.
boundedHistoryIrrelevantCheck
  :: Repository
  -> [FilePath]
  -> Text
  -> Text
  -> IO Bool
boundedHistoryIrrelevantCheck repository managedPaths ancestorRev descendantRev
  = maybe False (const True) <$> boundedHistoryIrrelevantCount repository managedPaths ancestorRev descendantRev

-- | Return the exact bounded delta size when the per-commit path proof is
-- successful.  Reuse reports this real work rather than a made-up zero.
boundedHistoryIrrelevantCount
  :: Repository
  -> [FilePath]
  -> Text
  -> Text
  -> IO (Maybe Int)
boundedHistoryIrrelevantCount repository managedPaths ancestorRev descendantRev
  | null managedPaths = pure Nothing
  | otherwise = do
      countResult <- runRepository repository "bounded cache delta count"
        ["rev-list", "--count", Text.unpack ancestorRev <> ".." <> Text.unpack descendantRev] BS.empty
      pathsResult <- runRepository repository "bounded cache delta paths"
        (["log", "--format=", "--name-only", Text.unpack ancestorRev <> ".." <> Text.unpack descendantRev, "--"] <> managedPaths) BS.empty
      pure $ case (countResult, pathsResult) of
        (Right countOutput, Right changedPaths)
          | processExitCode countOutput == ExitSuccess
              && processExitCode changedPaths == ExitSuccess ->
          case reads (BS8.unpack (BS8.filter (/= '\n') (processStdout countOutput))) of
            [(commitCount, "")] | commitCount > (0 :: Int) && commitCount <= 64 && BS.null (processStdout changedPaths) -> Just commitCount
            _ -> Nothing
        _ -> Nothing

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
  -> Text         -- ^ Required schema (e.g. @"adrai-cache/3"@).
  -> Text         -- ^ Target revision OID.
  -> IO (Maybe ReuseCacheInfo)
chooseReuseCache = chooseReuseCacheWith (const (pure ()))

-- | Test-observable reuse selection.  The observer runs immediately before a
-- full candidate validation, after cheap metadata/rank discovery and in the
-- exact rank order used by production.
chooseReuseCacheWith
  :: (FilePath -> IO ())
  -> Repository
  -> FilePath
  -> Text
  -> Text
  -> IO (Maybe ReuseCacheInfo)
chooseReuseCacheWith observeFullValidation repo cacheDir requiredSchema targetRev = do
  ranked <- rankedReuseCacheDescriptors noReuseDiscoveryHooks repo cacheDir requiredSchema targetRev
  firstValid ranked
  where
    firstValid [] = pure Nothing
    firstValid (candidate : rest) = do
      let info = reuseDescriptorInfo candidate
      observeFullValidation (rcPath info)
      accepted <- validatedCacheMetadataWith True (rcPath info)
      if maybe False (reuseCandidateMatches candidate) accepted
        then pure (Just info)
        else firstValid rest

    reuseCandidateMatches candidate metadata =
      metadata == reuseDescriptorMetadata candidate
        && Map.lookup "schema" metadata == Just requiredSchema

chooseReuseCacheWithDiscoveryHooks
  :: ReuseDiscoveryHooks -> (FilePath -> IO ()) -> Repository -> FilePath -> Text -> Text -> IO (Maybe ReuseCacheInfo)
chooseReuseCacheWithDiscoveryHooks hooks observeFullValidation repo cacheDir requiredSchema targetRev = do
  ranked <- rankedReuseCacheDescriptors hooks repo cacheDir requiredSchema targetRev
  firstValid ranked
  where
    firstValid [] = pure Nothing
    firstValid (candidate : rest) = do
      let info = reuseDescriptorInfo candidate
      observeFullValidation (rcPath info)
      accepted <- validatedCacheMetadataWith True (rcPath info)
      if maybe False (reuseCandidateMatches candidate) accepted
        then pure (Just info)
        else firstValid rest

    reuseCandidateMatches candidate metadata =
      metadata == reuseDescriptorMetadata candidate
        && Map.lookup "schema" metadata == Just requiredSchema


-- | One cache-selection phase encompasses exact repair/miss, cheap ranking,
-- every private copy/full validation attempt, and its pathless metrics.  The
-- owner lease deliberately outlives that phase so exact/reuse/cold consumers
-- run after its end while cleanup still brackets the callback.
withCacheSelectionCascade
  :: (forall x. IO x -> IO x)
  -> (CacheSelectionMetrics -> IO ())
  -> IO (Either error (Maybe ExactArchiveCompileFacts))
  -> IO Integer
  -> Repository
  -> FilePath
  -> Text
  -> Text
  -> (forall scope. CacheSelectionCascadeDecision scope -> IO (Either error a))
  -> IO (Either error a)
withCacheSelectionCascade phase observe exactAction exactBytes repo cacheDir requiredSchema targetRev consume =
  withCacheSelectionCascadeWithDiscoveryHooks noReuseDiscoveryHooks phase observe exactAction exactBytes repo cacheDir requiredSchema targetRev consume

-- | Hidden hook-bearing form used only by the pathless test façade.  Production
-- callers are permanently bound to the no-hook wrapper above.
withCacheSelectionCascadeWithDiscoveryHooks
  :: ReuseDiscoveryHooks
  -> (forall x. IO x -> IO x)
  -> (CacheSelectionMetrics -> IO ())
  -> IO (Either error (Maybe ExactArchiveCompileFacts))
  -> IO Integer
  -> Repository
  -> FilePath
  -> Text
  -> Text
  -> (forall scope. CacheSelectionCascadeDecision scope -> IO (Either error a))
  -> IO (Either error a)
withCacheSelectionCascadeWithDiscoveryHooks discoveryHooks phase observe exactAction exactBytes repo cacheDir requiredSchema targetRev consume =
  withCacheSelectionCascadeWithHooks discoveryHooks noCacheSelectionLifecycleHooks phase observe exactAction exactBytes repo cacheDir requiredSchema targetRev consume

-- | Hidden combined hook form.  The public production cascade always selects
-- both no-op hook records; only the pathless test façade can install these
-- action-only fault boundaries.
withCacheSelectionCascadeWithHooks
  :: ReuseDiscoveryHooks
  -> CacheSelectionLifecycleHooks
  -> (forall x. IO x -> IO x)
  -> (CacheSelectionMetrics -> IO ())
  -> IO (Either error (Maybe ExactArchiveCompileFacts))
  -> IO Integer
  -> Repository
  -> FilePath
  -> Text
  -> Text
  -> (forall scope. CacheSelectionCascadeDecision scope -> IO (Either error a))
  -> IO (Either error a)
withCacheSelectionCascadeWithHooks discoveryHooks lifecycleHooks phase observe exactAction exactBytes repo cacheDir requiredSchema targetRev consume =
  withCacheLeaseScopeEither $ \lease -> do
    decision <- phase $ do
      exact <- exactAction
      case exact of
        Left problem -> pure (Left problem)
        Right (Just facts) -> do
          bytes <- exactBytes
          observe (CacheSelectionMetrics 0 0 0 CacheSelectionExactKind 1 bytes Nothing)
          pure (Right (CacheSelectionExact facts))
        Right Nothing -> do
          ranked <- rankedReuseCacheDescriptors discoveryHooks repo cacheDir requiredSchema targetRev
          prepareRankedLease lease ranked 0 0 0
    case decision of
      Left problem -> pure (Left problem)
      Right selected -> consume selected
  where
    prepareRankedLease _ [] considered validated bytes = do
      observe (CacheSelectionMetrics considered validated bytes CacheSelectionColdKind 0 0 Nothing)
      pure (Right CacheSelectionCold)
    prepareRankedLease lease (descriptor : rest) considered validated bytes = do
      (attempted, attemptedBytes) <- preparePrivateLease lifecycleHooks lease cacheDir descriptor
      let considered' = considered + 1
          validated' = validated + maybe 0 (const 1) attemptedBytes
          bytes' = bytes + maybe 0 id attemptedBytes
      case attempted of
        Nothing -> prepareRankedLease lease rest considered' validated' bytes'
        Just (reuseFacts, witness, copiedBytes) -> do
          let metrics = CacheSelectionMetrics considered' validated' bytes' CacheSelectionReuseKind 1 copiedBytes (Just reuseFacts)
          observe metrics
          pure (Right (CacheSelectionReuse lease reuseFacts witness))

rankedReuseCacheDescriptors
  :: ReuseDiscoveryHooks
  -> Repository
  -> FilePath
  -> Text
  -> Text
  -> IO [ReuseCacheDescriptor]
rankedReuseCacheDescriptors (ReuseDiscoveryHooks beforeOpen afterOpen afterMetadata beforeRank) repo cacheDir requiredSchema targetRev = do
  listed <- try @SomeException (listDirectory cacheDir)
  entries <- case listed of
    Left exception -> rethrowCacheAsync exception >> pure []
    Right names -> pure names
  -- Directory enumeration is not ordered.  Discovery hooks therefore run over
  -- lexicographically ordered names; the separate semantic rank sort below is
  -- unchanged and still decides reuse precedence.
  let candidateFiles = sortBy compare (filter isCacheFile entries)
  fmap (sortBy (\a b -> candidateScore (reuseDescriptorInfo a) (reuseDescriptorInfo b)) . mapMaybe id) $
    traverse (toDescriptor cacheDir) candidateFiles
  where
    isCacheFile name =
      let lower = map toLower name
       in ".db" `isSuffixOf` lower || ".sqlite" `isSuffixOf` lower

    toDescriptor directory name = do
      let fullPath = directory </> name
      meta <- loadCacheMetaWithHooks beforeOpen afterOpen fullPath
      case meta of
        Nothing -> pure Nothing
        Just m ->
          case (Map.lookup "schema" m, Map.lookup "resolved_oid" m, Map.lookup "materialization_fingerprint" m) of
            (Just schema, Just srcRev, Just cacheKey')
              | schema == requiredSchema && not (Text.null srcRev) ->
                  case mkGitOid srcRev of
                    Left _ -> pure Nothing
                    Right _ -> do
                      afterMetadata fullPath
                      mtime <- tryGetMtime fullPath
                      case mtime of
                        Nothing -> pure Nothing
                        Just observedMtime -> do
                          beforeRank fullPath
                          rank <- computeAncestorRank repo (repositoryCommandDirectory repo) targetRev srcRev
                          pure $ case rank of
                            Unrelated -> Nothing
                            ExactMatch -> Nothing
                            _ -> Just ReuseCacheDescriptor
                              { reuseDescriptorInfo = ReuseCacheInfo fullPath srcRev cacheKey' rank observedMtime,
                                reuseDescriptorMetadata = m
                              }
              | otherwise -> pure Nothing
            _ -> pure Nothing

    tryGetMtime p = do
      observed <- try @SomeException (getModificationTime p)
      case observed of
        Left exception -> rethrowCacheAsync exception >> pure Nothing
        Right mt -> pure (Just (floor (utcTimeToPOSIXSeconds mt)))

-- | Prepare exactly one ranked candidate inside a Vacant lease.  Every owned
-- private directory is registered immediately; synchronous faults discard and
-- reset this preparation so the caller can try the next descriptor, while an
-- asynchronous exception escapes after the same cleanup sweep.
preparePrivateLease
  :: CacheSelectionLifecycleHooks
  -> CacheLease scope
  -> FilePath
  -> ReuseCacheDescriptor
  -> IO (Maybe (CacheLeaseReuseFacts, Maybe ProvenanceRecoveryWitness, Integer), Maybe Integer)
preparePrivateLease lifecycleHooks lease cacheDirectory descriptor = mask $ \restore -> do
  beginCacheLeasePreparation lease
  -- Acquiring the private scope is not a candidate-local miss.  A failed
  -- sentinel close/remove/create transition means cleanup is unproven, so do
  -- not continue to a later candidate that could incorrectly succeed.
  privateDirectory <- createPrivateReuseDirectory lifecycleHooks cacheDirectory
  registered <- try @SomeException (registerLeaseResource lease (cleanupPrivateReuseDirectory lifecycleHooks privateDirectory))
  case registered of
    Left registrationException -> do
      cleanupResult <- try @SomeException (cleanupPrivateReuseDirectory lifecycleHooks privateDirectory)
      if isCacheAsync registrationException
        then throwIO registrationException
        else case cleanupResult of
          Left cleanupException | isCacheAsync cleanupException -> throwIO cleanupException
          _ -> throwIO registrationException
    Right () -> pure ()
  let privatePath = privateDirectory </> "snapshot.sqlite"
      candidate = reuseDescriptorInfo descriptor
  -- Only copy/size faults are candidate-local.  The directory itself has
  -- already been registered under the masked lease before this restored work.
  prepared <- try @SomeException $ do
    restore (runCacheSelectionLifecycleHook lifecycleHooks BeforePrivatePublicSourceCopy)
    restore (copyFile (rcPath candidate) privatePath)
    copiedBytes <- getFileSize privatePath
    restore (runCacheSelectionLifecycleHook lifecycleHooks AfterPrivateCopy)
    pure (privatePath, copiedBytes)
  case prepared of
    Left exception -> do
      discarded <- try @SomeException (discardCacheLeasePreparation lease)
      rethrowCacheAsync exception
      case discarded of
        Left cleanupException -> rethrowCacheAsync cleanupException >> throwIO exception
        Right () -> pure (Nothing, Nothing)
    Right (preparedPath, copiedBytes) -> do
      -- Full-validation faults are normalized by the validator itself only
      -- after its rollback/close have been proven.  Cleanup failures escape
      -- this scope rather than being mistaken for another ranked candidate.
      witness <- validatedPrivateCopyWitness lifecycleHooks (reuseDescriptorMetadata descriptor) preparedPath (rcSourceRev (reuseDescriptorInfo descriptor))
      case witness of
        Nothing -> do
          discarded <- try @SomeException (discardCacheLeasePreparation lease)
          case discarded of
            Left exception -> rethrowCacheAsync exception >> throwIO exception
            Right () -> pure (Nothing, Just copiedBytes)
        Just acceptedWitness -> do
          let acceptedCandidate = reuseDescriptorInfo descriptor
              acceptedFacts =
                LeaseAcceptedFacts
                  { leaseAcceptedSourceRevision = rcSourceRev acceptedCandidate,
                    leaseAcceptedCacheKey = rcCacheKey acceptedCandidate,
                    leaseAcceptedMetadata = reuseDescriptorMetadata descriptor,
                    leaseAcceptedPrivateBytes = copiedBytes
                  }
              reuseFacts = CacheLeaseReuseFacts acceptedFacts (rcRank acceptedCandidate) (rcMtime acceptedCandidate)
          registerAcceptedLeaseSource lease preparedPath acceptedFacts
          pure (Just (reuseFacts, acceptedWitness, copiedBytes), Just copiedBytes)

-- | Full private-copy validation and provenance witness extraction share one
-- existing-file transaction.  The witness is therefore bound to precisely the
-- canonical metadata map that accepted the private candidate.
validatedPrivateCopyWitness :: CacheSelectionLifecycleHooks -> Map Text Text -> FilePath -> Text -> IO (Maybe (Maybe ProvenanceRecoveryWitness))
validatedPrivateCopyWitness lifecycleHooks expectedMetadata privatePath sourceTarget = mask $ \restore -> do
  opened <- try @SomeException (openReadWriteExisting privatePath)
  case opened of
    Left exception -> rethrowCacheAsync exception >> pure Nothing
    Right connection -> do
      actionResult <- try @SomeException $ restore $ do
        execute_ connection "BEGIN"
        runCacheSelectionLifecycleHook lifecycleHooks BeforePrivateValidation
        accepted <- loadValidatedCacheRows connection
        let metadata = validatedMetadata accepted
        if cacheRowsAreCanonical True accepted && metadata == expectedMetadata
          then do
            -- A private reuse candidate receives the same six FTS5 posting
            -- index checks as an exact archive before it can become a lease.
            validateFtsContentIndexes Nothing connection
            pure (Just (provenanceRecoveryWitnessFromRows accepted sourceTarget))
          else pure Nothing
      cleanupResult <- try @SomeException (cleanupExactArchiveConnection [execute_ connection "ROLLBACK", close connection])
      case actionResult of
        Left initiating
          | isCacheAsync initiating -> throwIO initiating
          | otherwise ->
              case cleanupResult of
                Left cleanupException
                  | isCacheAsync cleanupException -> throwIO cleanupException
                  | otherwise -> throwIO initiating
                Right () -> pure Nothing
        Right result ->
          case cleanupResult of
            Left cleanupException -> throwIO cleanupException
            Right () -> pure result

-- | Reserve a directory name without a cache-looking suffix.  'openTempFile'
-- comes from @base@, and the sentinel is removed before creating the directory
-- at that exact name; discovery only enumerates SQLite files, so this private
-- scope can never become a reuse candidate itself.
createPrivateReuseDirectory :: CacheSelectionLifecycleHooks -> FilePath -> IO FilePath
createPrivateReuseDirectory lifecycleHooks parent = mask_ $ do
  (sentinel, handle) <- openTempFile parent ".adrai-reuse-private-"
  closed <- attemptAll
    [ runCacheSelectionLifecycleHook lifecycleHooks BeforePrivateSentinelClose,
      hClose handle
    ]
  case firstFailure closed of
    Just initiating -> failTransition initiating [hClose handle, removeFileIfPresent sentinel]
    Nothing -> do
      unlinked <- attemptAll
        [ runCacheSelectionLifecycleHook lifecycleHooks BeforePrivateSentinelUnlink,
          removeFile sentinel
        ]
      case firstFailure unlinked of
        Just initiating -> failTransition initiating [removeFileIfPresent sentinel]
        Nothing -> do
          created <- attemptAll
            [ runCacheSelectionLifecycleHook lifecycleHooks BeforePrivateDirectoryCreate,
              createDirectory sentinel
            ]
          case firstFailure created of
            Nothing -> pure sentinel
            Just initiating -> failTransition initiating [removeDirectoryIfPresent sentinel, removeFileIfPresent sentinel]
  where
    removeFileIfPresent path = do
      result <- try @SomeException (removeFile path)
      case result of
        Left exception
          | Just ioException <- fromException exception,
            isDoesNotExistError ioException -> pure ()
          | otherwise -> throwIO exception
        Right () -> pure ()
    removeDirectoryIfPresent path = do
      result <- try @SomeException (removeDirectory path)
      case result of
        Left exception
          | Just ioException <- fromException exception,
            isDoesNotExistError ioException -> pure ()
          | otherwise -> throwIO exception
        Right () -> pure ()
    failTransition initiating cleanupActions = do
      cleanupResults <- traverse (try @SomeException) cleanupActions
      -- The transition exception remains authoritative unless cleanup itself
      -- was asynchronously cancelled; nevertheless every owned resource was
      -- attempted before returning either identity.
      throwCleanupOutcome [Left initiating] cleanupResults

cleanupPrivateReuseDirectory :: CacheSelectionLifecycleHooks -> FilePath -> IO ()
cleanupPrivateReuseDirectory lifecycleHooks privateDirectory = mask_ $ do
  let removeFileIfPresent path = do
        removed <- try @SomeException (removeFile path)
        case removed of
          Right () -> pure ()
          Left exception
            | Just ioException <- fromException exception,
              isDoesNotExistError ioException -> pure ()
            | otherwise -> throwIO exception
      removeDirectoryIfPresent path = do
        removed <- try @SomeException (removeDirectory path)
        case removed of
          Right () -> pure ()
          Left exception
            | Just ioException <- fromException exception,
              isDoesNotExistError ioException -> pure ()
            | otherwise -> throwIO exception
      cleanupFile (event, path) = attemptAll
        [ runCacheSelectionLifecycleHook lifecycleHooks event,
          removeFileIfPresent path
        ]
      snapshot = privateDirectory </> "snapshot.sqlite"
  -- Do not short-circuit: journal/WAL/SHM and the directory must each receive
  -- a cleanup attempt even when an earlier unlink fails.
  results <- concat <$> traverse cleanupFile
    [ (BeforePrivateDbUnlink, snapshot),
      (BeforePrivateJournalUnlink, snapshot <> "-journal"),
      (BeforePrivateWalUnlink, snapshot <> "-wal"),
      (BeforePrivateShmUnlink, snapshot <> "-shm")
    ]
  directoryResults <- attemptAll
    [ runCacheSelectionLifecycleHook lifecycleHooks BeforePrivateDirectoryRemoval,
      removeDirectoryIfPresent privateDirectory
    ]
  throwFirstCleanupFailure (results <> directoryResults)

firstAsyncFailure :: [Either SomeException ()] -> Maybe SomeException
firstAsyncFailure outcomes =
  listToMaybe [exception | Left exception <- outcomes, isCacheAsync exception]

throwFirstCleanupFailure :: [Either SomeException ()] -> IO ()
throwFirstCleanupFailure outcomes =
  case firstAsyncFailure outcomes of
    Just exception -> throwIO exception
    Nothing ->
      case [exception | Left exception <- outcomes] of
        exception : _ -> throwIO exception
        [] -> pure ()

attemptAll :: [IO ()] -> IO [Either SomeException ()]
attemptAll = traverse (try @SomeException)

firstFailure :: [Either SomeException ()] -> Maybe SomeException
firstFailure outcomes =
  firstAsyncFailure outcomes <|> listToMaybe [exception | Left exception <- outcomes]

-- | Preserve the r27 lifecycle priority: an initiating asynchronous exception
-- wins, then cleanup asynchronous, then the initiating synchronous failure,
-- then cleanup synchronous failure.  All cleanup actions have already run.
throwCleanupOutcome :: [Either SomeException ()] -> [Either SomeException ()] -> IO a
throwCleanupOutcome initiating cleanup =
  case firstAsyncFailure initiating <|> firstAsyncFailure cleanup <|> firstSyncFailure initiating <|> firstSyncFailure cleanup of
    Just exception -> throwIO exception
    Nothing -> pure (error "throwCleanupOutcome: no failure")
  where
    firstSyncFailure outcomes = listToMaybe [exception | Left exception <- outcomes]

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
    -> Text            -- ^ Required schema (e.g. "adrai-cache/3").
    -> Text            -- ^ Target revision OID.
    -> Maybe FilePath  -- ^ Optional exact-match cache path.
    -> [FilePath]      -- ^ Managed paths for tree-identical comparison.
    -> IO (CacheMode, IncrementalKind, Maybe ReuseCacheInfo)
cachePathSelection repo cacheDir dbAlias requiredSchema targetRev exactCache managedPaths =
  fst <$> cachePathSelectionWithHistoryCount repo cacheDir dbAlias requiredSchema targetRev exactCache managedPaths

-- | Select a cache path and retain the exact bounded-history proof that made a
-- tree-identical candidate eligible.  The caller must consume this count rather
-- than run a second Git proof: target and source OIDs are immutable, so a
-- second probe only introduces a race-to-cold fallback without adding safety.
cachePathSelectionWithHistoryCount
    :: Repository
    -> FilePath
    -> Text
    -> Text
    -> Text
    -> Maybe FilePath
    -> [FilePath]
    -> IO ((CacheMode, IncrementalKind, Maybe ReuseCacheInfo), Maybe Int)
cachePathSelectionWithHistoryCount repo cacheDir dbAlias requiredSchema targetRev exactCache managedPaths = do
  (selection, historyProof, _) <- cachePathSelectionWithHistoryCountAndProvenanceCandidate repo cacheDir dbAlias requiredSchema targetRev exactCache managedPaths
  pure (selection, historyProof)

-- | The selected ancestor remains useful as a provenance-only authority even
-- when a managed-tree difference correctly forces a full semantic compile.
-- The caller must still independently validate and prove the witness before
-- consuming it.
cachePathSelectionWithHistoryCountAndProvenanceCandidate
  :: Repository
  -> FilePath
  -> Text
  -> Text
  -> Text
  -> Maybe FilePath
  -> [FilePath]
    -> IO ((CacheMode, IncrementalKind, Maybe ReuseCacheInfo), Maybe Int, Maybe ReuseCacheInfo)
cachePathSelectionWithHistoryCountAndProvenanceCandidate repo cacheDir _dbAlias requiredSchema targetRev exactCache managedPaths = do
  -- Path 1: Check for exact match on the provided cache path.
  let tryExact = case exactCache of
        Just cachePath -> do
          -- Exact recovery only reads the immutable snapshot for the requested
          -- revision.  A conflict or shallow result is still an exact result
          -- and may be exposed, although it is deliberately excluded from
          -- cross-revision candidate discovery.  The shared exact-target
          -- authority includes FTS5 posting integrity; do not bypass it with
          -- a metadata-only copy of the contract here.
          -- Public selector semantics remain fail-closed: a partial or invalid
          -- exact file cannot suppress a valid historical reuse candidate.
          exact <-
            if requiredSchema == "adrai-cache/3"
              then validateExactCacheTarget cachePath targetRev
              else pure False
          pure $
            if exact
              then Just (Exact, FullCompile, Nothing)
              else Nothing
        Nothing -> return Nothing
  exactResult <- tryExact
  case exactResult of
    Just result -> pure (result, Nothing, Nothing)
    Nothing -> do
      bestCandidate <- chooseReuseCache repo cacheDir requiredSchema targetRev
      case bestCandidate of
        Nothing -> pure ((Full, FullCompile, Nothing), Nothing, Nothing)
        Just candidate -> do
          sameTree <- treeIdenticalCheck repo managedPaths (rcSourceRev candidate) targetRev
          historyCount <-
            if sameTree
              then boundedHistoryIrrelevantCount repo managedPaths (rcSourceRev candidate) targetRev
              else pure Nothing
          case historyCount of
            Just count | sameTree -> pure ((Incremental TreeIdentical, TreeIdentical, Just candidate), Just count, Just candidate)
            _ -> pure ((Full, FullCompile, Nothing), Nothing, Just candidate)

-- | Production exact selection combines validation, closed fact loading, and
-- alias comparison before deciding the cascade.  The immutable archive is
-- validated exactly once on a healthy exact hit; invalid exact candidates use
-- the ordinary historical cascade unchanged.
cachePathSelectionWithHistoryCountAndProvenanceCandidateAndExactAlias
  :: Repository
  -> FilePath
  -> Text
  -> Text
  -> Text
  -> Maybe FilePath
  -> FilePath
  -> [FilePath]
  -> IO ((CacheMode, IncrementalKind, Maybe ReuseCacheInfo), Maybe Int, Maybe ReuseCacheInfo, Maybe ExactArchiveAliasStatus)
cachePathSelectionWithHistoryCountAndProvenanceCandidateAndExactAlias repo cacheDir dbAlias requiredSchema targetRev exactCache alias managedPaths = do
  exact <-
    case exactCache of
      Just archive | requiredSchema == "adrai-cache/3" -> readExactArchiveAliasStatus archive targetRev alias
      _ -> pure Nothing
  case exact of
    Just status -> pure ((Exact, FullCompile, Nothing), Nothing, Nothing, Just status)
    Nothing -> do
      (selection, historyProof, provenanceCandidate) <-
        cachePathSelectionWithHistoryCountAndProvenanceCandidate repo cacheDir dbAlias requiredSchema targetRev Nothing managedPaths
      pure (selection, historyProof, provenanceCandidate, Nothing)
