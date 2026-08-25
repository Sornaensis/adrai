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
    validateCacheContract,
    validateCachePublicationContract,
    cachePublicationMaterializationFingerprintEvidence,
    computeAncestorRank,
    treeIdenticalCheck,
    boundedHistoryIrrelevantCheck,
    boundedHistoryIrrelevantCount,
    chooseReuseCache,
    cachePathSelection,
  )
where

import Adrai.Git (Repository (..), processExitCode, processStdout, runRepository)
import Adrai.Format.Json (JsonValue (..), object, renderCanonicalJson)
import Adrai.Provenance (decodeBase64Url, encodeBase64Url, sha256FrameStateFeed, sha256FrameStateFinalize, sha256FrameStateInit)
import Adrai.Retrieval (materializationImplementationFingerprint)
import Adrai.Sqlite (SearchStorageComponent (..), allFtsTargets, coldSchemaDdl, ftsTargetDdl, ftsTargetTable, searchOrdinarySchemaDdl)
import Adrai.Types (digestBytes)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM)
import Data.Aeson (Value, eitherDecodeStrict')
import Data.Aeson.Types (Parser, parseMaybe, withArray, withObject, (.:))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toLower)
import Data.List (elemIndex, isSuffixOf, sortBy)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.Vector as Vector
import Database.SQLite.Simple (Connection, Only (..), close, open, query_)
import System.Directory (doesFileExist, getModificationTime, listDirectory)
import System.Exit (ExitCode (ExitSuccess))
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
      -- SQLite's default open mode can create a database.  The existence
      -- check above therefore guards the open, while 'bracket' guarantees a
      -- read-side probe never leaves a Windows handle live when a query (or
      -- metadata decoding) fails before the explicit close.
      result <- try @SomeException $
        bracket (open path) close $ \conn -> do
          rows <- query_ conn "SELECT key, value FROM meta ORDER BY key" :: IO [(Text, Text)]
          pure (Map.fromList rows)
      case result of
        Left _  -> pure Nothing
        Right m -> pure (Just m)

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

-- | Test-visible decomposition of the materialization commitment checked by
-- 'validateCachePublicationContract'.  The public compiler path consumes only
-- the boolean validator; this seam lets bounded regressions distinguish a
-- metadata/fingerprint disagreement from the other fail-closed checks.
-- The triple is @(written, current, legacy)@; legacy is present only when its
-- graph-axis reconstruction is well-formed.
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
          legacy <- persistedLegacyMaterializationFingerprint conn sourceFingerprint
          pure $ do
            written <- Map.lookup "materialization_fingerprint" metadata
            actual <- current
            pure (written, actual, legacy)
      pure (either (const Nothing) id result)

validateCacheWith :: Bool -> FilePath -> IO Bool
validateCacheWith requireValidSemantics path = do
  exists <- doesFileExist path
  if not exists
    then pure False
    else do
      checked <- try @SomeException $
        bracket (open path) close $ \conn -> do
          metadataRows <- query_ conn "SELECT key, value FROM meta ORDER BY key" :: IO [(Text, Text)]
          integrity <- query_ conn "PRAGMA integrity_check" :: IO [Only Text]
          foreignKeys <- query_ conn "PRAGMA foreign_key_check" :: IO [(Text, Int64, Text, Int64)]
          schemaRows <- query_ conn "SELECT name, sql FROM sqlite_master WHERE type='table' AND sql IS NOT NULL" :: IO [(Text, Text)]
          actualCounts <- forM declaredCountTables $ \(key, table) -> do
            value <- scalarCount conn ("SELECT count(*) FROM " <> table)
            pure (key, value)
          projections <- canonicalProjectionChecks conn
          sourceFingerprint <- persistedSourceFingerprint conn
          materializationFingerprint <- persistedMaterializationFingerprint conn sourceFingerprint
          legacyMaterializationFingerprint <- persistedLegacyMaterializationFingerprint conn sourceFingerprint
          semanticState <- persistedSemanticState conn
          pure (metadataRows, integrity, foreignKeys, schemaRows, actualCounts, projections, sourceFingerprint, materializationFingerprint, legacyMaterializationFingerprint, semanticState)
      pure $ case checked of
        Right (metadataRows, [Only "ok"], [], schemaRows, actualCounts, True, Just sourceFingerprint, Just materializationFingerprint, legacyMaterializationFingerprint, Just semanticState)
          | uniqueMetadata metadataRows ->
              let metadata = Map.fromList metadataRows
                  writtenMaterializationFingerprint = Map.lookup "materialization_fingerprint" metadata
               in canonicalMetadata metadata semanticState
                    && canonicalSchema schemaRows
                    && declaredCountsMatch metadata actualCounts
                    && Map.lookup "source_fingerprint" metadata == Just sourceFingerprint
                    -- P6-06G.7 changed the current-connection commitment from
                    -- graph-axis order to SQLite's connection-ID order.  The
                    -- ABI intentionally stayed stable, so immutable archives
                    -- published by the prior writer remain exact authorities.
                    -- Both reconstructions commit to every row and the
                    -- projection proof above rejects axis/ordinal corruption.
                    && writtenMaterializationFingerprint `elem` [Just materializationFingerprint, legacyMaterializationFingerprint]
        _ -> False
  where
    declaredCountTables =
      [ ("managed_source_count", "managed_source")
      , ("issue_count", "issue")
      , ("conflict_count", "adr_conflict")
      , ("operation_count", "operation")
      , ("search_document_count", "search_document")
      ]
    expectedSchema =
      coldSchemaDdl
        <> [(name, ddl) | (component, ddl) <- searchOrdinarySchemaDdl, let name = case component of { SearchSchemaStorage value -> value; _ -> error "unexpected ordinary search storage component" }]
        <> [(ftsTargetTable target, ftsTargetDdl target) | target <- allFtsTargets]
    canonicalSchema rows =
      let actual = Map.fromList rows
          expectedNames = Set.fromList (map fst expectedSchema)
          ftsShadowNames =
            Set.fromList
              [ ftsTargetTable target <> suffix
              | target <- allFtsTargets,
                suffix <- ["_data", "_idx", "_content", "_docsize", "_config"]
              ]
          permitted name = Set.member name expectedNames || Set.member name ftsShadowNames
       in all (\(name, ddl) -> Map.lookup name actual == Just ddl) expectedSchema
            && all permitted (Map.keys actual)
    uniqueMetadata rows = Map.size (Map.fromList rows) == length rows
    canonicalMetadata metadata semanticState =
      and
        [ Map.lookup "schema" metadata == Just "adrai-cache/1",
          Map.lookup "compiler_abi" metadata == Just "adrai-cold-compiler/1",
          Map.lookup "materializer" metadata == Just materializationImplementationFingerprint,
          semanticStateIsAcceptable metadata semanticState,
          Map.lookup "requested_revision" metadata == Map.lookup "resolved_oid" metadata,
          all (nonEmpty metadata) ["requested_revision", "resolved_oid", "source_fingerprint", "materialization_fingerprint"],
          all (nonNegative metadata) ["history_commits_scanned", "managed_source_count", "issue_count", "conflict_count", "operation_count", "search_document_count"],
          validFingerprint metadata "source_fingerprint",
          validFingerprint metadata "materialization_fingerprint"
        ]
    semanticStateIsAcceptable metadata actualState =
      let declaredState = Map.lookup "semantic_state" metadata
          historyComplete = Map.lookup "history_complete" metadata
          hasCanonicalHistoryFlag = historyComplete `elem` [Just "true", Just "false"]
       in declaredState == Just actualState
            && hasCanonicalHistoryFlag
            && if requireValidSemantics
              then declaredState == Just "valid" && historyComplete == Just "true"
              else True
    nonEmpty metadata key = maybe False (not . Text.null) (Map.lookup key metadata)
    validFingerprint metadata key =
      case Map.lookup key metadata of
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
    nonNegative metadata key =
      case Map.lookup key metadata >>= readNonNegative of
        Just _ -> True
        Nothing -> False
    readNonNegative raw =
      case reads (Text.unpack raw) of
        [(value, "")] | value >= (0 :: Integer) -> Just value
        _ -> Nothing
    scalarCount connection sql = do
      rows <- query_ connection sql :: IO [Only Integer]
      case rows of
        [Only count] -> pure count
        _ -> fail "cache count query returned an invalid result"
    declaredCountsMatch metadata = all $ \(key, actual) ->
      case Map.lookup key metadata >>= readNonNegative of
        Just declared -> declared == actual
        Nothing -> False

-- | The cold writer verifies these bidirectional keys before publication.  A
-- cache reader repeats the proof so a readable database with deleted or
-- spliced logical rows cannot become reuse evidence.
canonicalProjectionChecks :: Connection -> IO Bool
canonicalProjectionChecks connection = do
  membershipResults <- traverse checkMembership
    [ ("SELECT object_id FROM operation_member", "SELECT object_id FROM (SELECT record_id AS object_id FROM decision_record UNION ALL SELECT connection_id AS object_id FROM connection_record)")
    , ("SELECT item_id,adr_id,candidate_record_id,title,summary,decision,domains,rationale,context,consequences,identifiers FROM fts_search_exact", "SELECT item_id,adr_id,candidate_record_id,title,summary,decision,domains,rationale,context,consequences,identifiers FROM search_document")
    , ("SELECT item_id,adr_id,candidate_record_id,title,summary,decision,rationale,context,consequences FROM fts_search_stemmed", "SELECT item_id,adr_id,candidate_record_id,title,summary,decision,rationale,context,consequences FROM search_document")
    , ("SELECT item_id,adr_id,candidate_record_id,identifiers FROM fts_search_identifier", "SELECT item_id,adr_id,candidate_record_id,identifiers FROM search_document")
    , ("SELECT rowid,item_id,adr_id,candidate_record_id,section_kind,text,identifiers FROM fts_passage_exact", "SELECT passage_rowid,item_id,adr_id,candidate_record_id,section_kind,text,identifiers FROM search_section")
    , ("SELECT rowid,item_id,adr_id,candidate_record_id,section_kind,text FROM fts_passage_stemmed", "SELECT passage_rowid,item_id,adr_id,candidate_record_id,section_kind,text FROM search_section")
    , ("SELECT rowid,item_id,adr_id,candidate_record_id,section_kind,identifiers FROM fts_passage_identifier", "SELECT passage_rowid,item_id,adr_id,candidate_record_id,section_kind,identifiers FROM search_section")
    , ("SELECT adr_id FROM search_document", "SELECT adr_id FROM reduced_adr")
    ]
  cardinalityResults <- traverse countsMatch
    [ ("fts_search_exact", "search_document")
    , ("fts_search_stemmed", "search_document")
    , ("fts_search_identifier", "search_document")
    , ("fts_passage_exact", "search_section")
    , ("fts_passage_stemmed", "search_section")
    , ("fts_passage_identifier", "search_section")
    ]
  currentConnections <- canonicalCurrentConnectionRows connection
  pure (and membershipResults && and cardinalityResults && currentConnections)
  where
    checkMembership (leftSide, rightSide) = do
      leftOnly <- differenceIsEmpty leftSide rightSide
      rightOnly <- differenceIsEmpty rightSide leftSide
      pure (leftOnly && rightOnly)
    differenceIsEmpty leftSide rightSide = do
      rows <- query_ connection (fromString ("SELECT count(*) FROM (" <> leftSide <> " EXCEPT " <> rightSide <> ")")) :: IO [Only Int64]
      pure (rows == [Only 0])
    countsMatch (leftTable, rightTable) = do
      leftRows <- query_ connection (fromString ("SELECT count(*) FROM " <> leftTable)) :: IO [Only Int64]
      rightRows <- query_ connection (fromString ("SELECT count(*) FROM " <> rightTable)) :: IO [Only Int64]
      pure (leftRows == rightRows)

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

treeEntryFrame :: Text -> Text -> Text -> Text -> BS.ByteString
treeEntryFrame path mode objectType oid = TextEncoding.encodeUtf8 (Text.intercalate "\NUL" [path, mode, objectType, oid])

framedBytes :: BS.ByteString -> BS.ByteString
framedBytes bytes = TextEncoding.encodeUtf8 (Text.pack (show (BS.length bytes)) <> ":") <> bytes <> "\NUL"

-- | The semantic-state label is a derived claim, not caller-controlled
-- metadata.  Conflict rows synthesize ADR_CONFLICT issues, so errors are
-- counted separately from those rows just as the cold compiler does.
persistedSemanticState :: Connection -> IO (Maybe Text)
persistedSemanticState connection = do
  compilerErrors <- scalar "SELECT count(*) FROM issue WHERE severity='error' AND code <> 'ADR_CONFLICT'"
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

-- | Recompute the established materialization fingerprint from normalized
-- persisted rows.  The SQL projections deliberately contain every logical
-- value (including FTS source text), so forged metadata or a payload splice
-- cannot retain a cache's reuse eligibility.
persistedMaterializationFingerprint :: Connection -> Maybe Text -> IO (Maybe Text)
persistedMaterializationFingerprint = persistedMaterializationFingerprintWith "SELECT adr_id,connection_id FROM current_connection ORDER BY adr_id,ordinal"

-- | Archives emitted before P6-06G.7 retain their graph-axis frame order.
-- This compatibility reconstruction does not relax row validation; it merely
-- verifies the immutable digest using the ordering used by that writer.
persistedLegacyMaterializationFingerprint :: Connection -> Maybe Text -> IO (Maybe Text)
persistedLegacyMaterializationFingerprint = persistedMaterializationFingerprintWith "SELECT adr_id,connection_id FROM current_connection ORDER BY adr_id,CASE axis WHEN 'decision' THEN 0 WHEN 'scope' THEN 1 WHEN 'domain' THEN 2 WHEN 'status' THEN 3 ELSE 4 END,ordinal"

persistedMaterializationFingerprintWith :: String -> Connection -> Maybe Text -> IO (Maybe Text)
persistedMaterializationFingerprintWith _ _ Nothing = pure Nothing
persistedMaterializationFingerprintWith currentRowsSql connection (Just sourceFingerprint) = do
  sourceBytes <- pure (decodeBase64Url sourceFingerprint)
  diagnosticRows <- query_ connection "SELECT code,severity,origin,COALESCE(adr_id,''),COALESCE(object_id,''),COALESCE(operation_id,''),COALESCE(commit_oid,''),COALESCE(path,''),message FROM issue WHERE code <> 'ADR_CONFLICT' ORDER BY code,adr_id,object_id,operation_id,commit_oid,path,severity,CASE origin WHEN 'config' THEN 0 WHEN 'path_document' THEN 1 WHEN 'history' THEN 2 WHEN 'operation' THEN 3 WHEN 'graph' THEN 4 WHEN 'basis' THEN 5 ELSE 6 END,origin,message" :: IO [(Text, Text, Text, Text, Text, Text, Text, Text, Text)]
  conflictRows <- query_ connection "SELECT code,adr_id,candidate_count,state_token,summaries FROM adr_conflict ORDER BY adr_id" :: IO [(Text, Text, Int64, Text, Text)]
  operationRows <- query_ connection "SELECT operation_id,timestamp_ms,actor_kind,actor_id,actor_model,basis_oid,branch_hint,upstream_hint,line_anchors,tool_version FROM operation ORDER BY operation_id" :: IO [(Text, Text, Text, Text, Maybe Text, Text, Maybe Text, Maybe Text, Text, Text)]
  inputRows <- query_ connection "SELECT operation_id,input_digest,prompt_digest,context_digest FROM operation" :: IO [(Text, Maybe Text, Maybe Text, Maybe Text)]
  memberRows <- query_ connection "SELECT operation_id,object_id,event_kind,semantic_digest,path FROM operation_member ORDER BY operation_id,object_id,path" :: IO [(Text, Text, Text, Text, Text)]
  parentRows <- query_ connection "SELECT operation_id,object_id,parent_object_id FROM operation_member_parent ORDER BY operation_id,object_id,ordinal" :: IO [(Text, Text, Text)]
  reducedRows <- query_ connection "SELECT adr_id,state_token FROM reduced_adr ORDER BY adr_id" :: IO [(Text, Text)]
  axisRows <- query_ connection "SELECT adr_id,axis,object_id FROM axis_head ORDER BY adr_id,axis,ordinal" :: IO [(Text, Text, Text)]
  currentRows <- query_ connection (fromString currentRowsSql) :: IO [(Text, Text)]
  documentRows <- query_ connection "SELECT item_id || char(0) || adr_id || char(0) || candidate_record_id || char(0) || title || char(0) || summary || char(0) || context || char(0) || decision || char(0) || consequences || char(0) || domains || char(0) || rationale || char(0) || identifiers || char(0) || other || char(0) || scope || char(0) || source_paths || char(0) || CASE obsolete WHEN 1 THEN 'true' ELSE 'false' END || char(0) || CASE conflicted WHEN 1 THEN 'true' ELSE 'false' END || char(0) || state_token FROM search_document ORDER BY item_id" :: IO [Only Text]
  passageRows <- query_ connection "SELECT item_id || char(0) || search_item_id || char(0) || adr_id || char(0) || candidate_record_id || char(0) || section_kind || char(0) || ordinal || char(0) || line_start || char(0) || line_end || char(0) || text || char(0) || printf('%.6f',weight) || char(0) || source_paths || char(0) || identifiers FROM search_section ORDER BY item_id" :: IO [Only Text]
  aliases <- query_ connection "SELECT alias || char(0) || expansion FROM local_alias ORDER BY alias" :: IO [Only Text]
  pure $ do
    rawSource <- either (const Nothing) Just sourceBytes
    if BS.length rawSource /= 32
      then Nothing
      else do
        operationFrames <- traverse (operationFrame (Map.fromList [(operation, (inputDigest, promptDigest, contextDigest)) | (operation, inputDigest, promptDigest, contextDigest) <- inputRows]) memberRows parentRows) operationRows
        let axisMap = Map.fromListWith (flip (<>)) [((adr, axis), [objectId]) | (adr, axis, objectId) <- axisRows]
            currentMap = Map.fromListWith (flip (<>)) [(adr, [connectionId]) | (adr, connectionId) <- currentRows]
            reducedFrames =
              [ framedText (Text.intercalate "\NUL" [adr, stateToken, heads "decision", heads "scope", heads "domain", heads "status", Text.intercalate "," (Map.findWithDefault [] adr currentMap)])
              | (adr, stateToken) <- reducedRows
              , let heads axis = Text.intercalate "," (Map.findWithDefault [] (adr, axis) axisMap)
              ]
            frames =
              "adrai-cold-materialization/1\NUL"
                : framedText "adrai-cache/1"
                : framedText materializationImplementationFingerprint
                : framedBytes rawSource
                : map (framedText . diagnosticRowFingerprint) diagnosticRows
                  <> map (framedText . conflictRowFingerprint) conflictRows
                  <> concat operationFrames
                  <> reducedFrames
                  <> map (framedText . unOnly) documentRows
                  <> map (framedText . unOnly) passageRows
                  <> map (framedText . unOnly) aliases
        Just (encodeBase64Url (digestBytes (sha256FrameStateFinalize (foldl' sha256FrameStateFeed sha256FrameStateInit frames))))
  where
    framedText = framedBytes . TextEncoding.encodeUtf8
    unOnly (Only value) = value
    diagnosticRowFingerprint (code, severity, origin, adr, objectId, operation, commit, path, message) = Text.intercalate "\NUL" [code, severity, origin, adr, objectId, operation, commit, path, message]
    conflictRowFingerprint (code, adr, count, stateToken, summaries) = Text.intercalate "\NUL" [code, adr, Text.pack (show count), stateToken, summaries]
    operationFrame inputs members parents (operation, timestamp, actorKind, actorId, actorModel, basis, branch, upstream, anchors, toolVersion) = do
      anchorValues <- parseAnchorValues anchors
      let matchingMembers = [member | member@(memberOperation, _, _, _, _) <- members, memberOperation == operation]
          parentsFor objectId = Text.intercalate "\n" [parent | (parentOperation, parentObject, parent) <- parents, parentOperation == operation, parentObject == objectId]
          (inputDigest, promptDigest, contextDigest) = Map.findWithDefault (Nothing, Nothing, Nothing) operation inputs
      pure $
        [ framedText (Text.intercalate "\NUL"
            [ operation, objectId, eventKind, timestamp, actorKind, actorId, maybe "" id actorModel, basis, parentsFor objectId, maybe "" id branch, maybe "" id upstream, Text.intercalate "\n" anchorValues, "sha256:" <> semanticDigest, toolVersion, digestText inputDigest, digestText promptDigest, digestText contextDigest, path
            ])
        | (_, objectId, eventKind, semanticDigest, path) <- matchingMembers
        ]
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
    toCandidate directory _ name = do
      let fullPath = directory </> name
      meta <- loadCacheMeta fullPath
      valid <- validateCacheContract fullPath
      case (valid, meta) of
        (_, Nothing) -> pure Nothing
        (False, _) -> pure Nothing
        (True, Just m) ->
          case (Map.lookup "schema" m, Map.lookup "resolved_oid" m, Map.lookup "materialization_fingerprint" m) of
            (Just schema, Just srcRev, Just cacheKey')
              | schema == requiredSchema && not (Text.null srcRev) -> do
                  mtime <- tryGetMtime fullPath
                  rank <- computeAncestorRank repo (repositoryCommandDirectory repo) targetRev srcRev
                  pure $ case rank of
                    Unrelated -> Nothing
                    ExactMatch -> Nothing
                    _ -> Just ReuseCacheInfo
                      { rcPath = fullPath,
                        rcSourceRev = srcRev,
                        rcCacheKey = cacheKey',
                        rcRank = rank,
                        rcMtime = mtime
                      }
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
cachePathSelection repo cacheDir _dbAlias requiredSchema targetRev exactCache _managedPaths = do
  -- Path 1: Check for exact match on the provided cache path.
  let tryExact = case exactCache of
        Just cachePath -> do
          meta <- loadCacheMeta cachePath
          -- Exact recovery only reads the immutable snapshot for the requested
          -- revision.  A conflict or shallow result is still an exact result
          -- and may be exposed, although it is deliberately excluded from
          -- cross-revision candidate discovery.
          valid <- validateCachePublicationContract cachePath
          case (valid, meta) of
            (True, Just m)
              | Map.lookup "schema" m == Just requiredSchema
                  && Map.lookup "requested_revision" m == Just targetRev
                  && Map.lookup "resolved_oid" m == Just targetRev ->
                      return (Just (Exact, FullCompile, Nothing))
            _ -> return Nothing
        Nothing -> return Nothing
  exactResult <- tryExact
  case exactResult of
    Just result -> return result
    Nothing -> do
      bestCandidate <- chooseReuseCache repo cacheDir requiredSchema targetRev
      case bestCandidate of
        Nothing -> pure (Full, FullCompile, Nothing)
        Just candidate -> do
          sameTree <- treeIdenticalCheck repo _managedPaths (rcSourceRev candidate) targetRev
          harmlessHistory <- boundedHistoryIrrelevantCheck repo _managedPaths (rcSourceRev candidate) targetRev
          pure $
            if sameTree && harmlessHistory
              then (Incremental TreeIdentical, TreeIdentical, Just candidate)
              else (Full, FullCompile, Nothing)
