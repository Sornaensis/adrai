{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Provenance overlay orchestration module.
--
-- Mirrors the Python prototype at
-- ADRAI_1_Source/adrai_core/provenance_cache.py functions:
--
-- * @ensure_provenance()@
-- * @line_config_key()@ (exported as 'configKey')
-- * @_refresh_line_landings()@ (exported as 'refreshLineLandings')
-- * @_observation_fingerprint()@ (exported via 'observationFingerprint')
-- * @overlay_rows_for_operations()@ (exported as 'overlayRowsForOperations')
-- * @is_shallow()@ (exported as 'isShallow')
--
-- This module provides the main orchestration entry point for provenance
-- overlay maintenance: configuration key computation, line landing refresh,
-- shallow history detection, and the full ensure_provenance pipeline.
module Adrai.Provenance.Ensure
  ( -- | Configuration key computation
    configKey,

    -- | Line landing refresh
    refreshLineLandings,

    -- | Shallow history detection
    isShallow,

    -- | Main orchestration entry point
    ensureProvenance,

    -- | Update record
    ProvenanceUpdate (..),

    -- | Overlay rows query
    overlayRowsForOperations,
  )
where

import Adrai.Git
  ( GitError (..),
    GitOid (..),
    GitObjectType (GitCommitObject),
    GitObjectInfo (..),
    GitProcessResult (..),
    Repository (..),
    batchObjectInfo,
    gitOidText,
    isShallowRepository,
    runRepository,
  )
import Adrai.Provenance
  ( LineAnchor (..),
    OverlayFingerprint (..),
    mkGitOid,
    mkOverlayFingerprint,
    sha256Digest,
  )
import Adrai.Provenance.Classification
  ( ParsedManagedDocument (..),
    RegisteredObjectData (..),
    RegisteredOperationData (..),
    candidateCommits,
    processCandidates,
    pruneUnavailablePlacements,
    registerOperationGroups,
    recordIssue,
    registeredOperations,
    storeNewCommits,
  )
import Adrai.Provenance.Overlay
  ( CommitObservation (..),
    ManagedPathAddition (..),
    ObservationRoot (..),
    RefObservation (..),
    LineLanding (..),
    OperationClassification (..),
    OperationCommit (..),
    ProvenanceIssue (..),
    createOverlaySchema,
    overlayValid,
    provenanceDatabasePath,
  )
import Adrai.Provenance.Discovery
  ( addedPathsForCommits,
    commitLogSnapshotForOids,
    firstParentPathLandings,
    listRefs,
    managedPathAdditions,
    managedSuffixes,
    observationFingerprint,
    observationRoots,
    reflogCommitRoots,
    revListDelta,
  )

import Adrai.Sqlite (asQuery)
import Adrai.Types
  ( Config (..),
    LogicalLine (..),
    RepoPath (..),
    GitRef (..),
    Digest (..),
    digestBytes,
    mkGitRef,
    repoPathText,
    gitRefText,
  )
import Control.Exception (Exception, SomeException (SomeException), toException, try)
import Control.Monad (forM_, when, void)
import Data.Maybe (isJust)
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as Lazy (toStrict)
import Data.List (sort, sortBy)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import qualified Data.Vector as Vector
import Database.SQLite.Simple
  ( Connection,
    Only (..),
    SQLData (SQLNull, SQLText, SQLInteger),
    execute,
    execute_,
    open,
    query,
    query_,
    close,
  )
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeDirectory, (</>))
import Data.Word (Word8)
import Data.Int (Int64)

-- ============================================================
-- Configuration key computation
-- ============================================================

-- | Compute the SHA-256 config key for a logical-line configuration.
--
-- Mirrors the Python @line_config_key()@ function.  The key is a SHA-256
-- hash of the canonical JSON representation of the decisions path,
-- connections path, and logical lines.
configKey :: Text -> Text -> [Text] -> Text
configKey decisionsPath connectionsPath logicalLines =
  let payload = Aeson.Object (KeyMap.fromList
        [ (Key.fromText "connections", Aeson.String connectionsPath),
          (Key.fromText "decisions", Aeson.String decisionsPath),
          (Key.fromText "logical_lines", Aeson.Array (Vector.fromList (map Aeson.String logicalLines)))
        ])
      json = Lazy.toStrict (Aeson.encode payload)
      digest = sha256Digest json
  in digestToHex digest

-- ============================================================
-- Shallow history detection
-- ============================================================

-- | Check if the repository is shallow (truncated clone).
--
-- Mirrors the Python @is_shallow()@ function from @adrai_core.gitops@.
isShallow :: Repository -> IO (Either SomeException Bool)
isShallow repository = do
  shallowResult <- try @SomeException $ isShallowRepository repository
  pure $ case shallowResult of
    Left e         -> Left e
    Right (Left e) -> Left (toException (GitErrorSome e))
    Right (Right b) -> Right b

-- ============================================================
-- Line landing refresh
-- ============================================================

-- | Refresh line landing records for a configuration key.
--
-- Mirrors the Python @_refresh_line_landings()@ function.
refreshLineLandings
  :: Repository
  -> Connection
  -> Text               -- ^ Decisions path
  -> Text               -- ^ Connections path
  -> [LogicalLine]      -- ^ Logical lines
  -> [Text]             -- ^ New operation IDs
  -> Maybe [ParsedManagedDocument]  -- ^ Operation members (for future use)
  -> Maybe GroupsLoader   -- ^ Lazy groups loader (for future use)
  -> IO (Either SomeException Bool)
refreshLineLandings repo conn decisionsPath connectionsPath logicalLines newOps _maybeParsedDocs _groupsLoader = do
  result <- try @SomeException $ do
    -- Compute config key and JSON
    let lineIds = [llId | LogicalLine llId _ <- logicalLines]
        configKey' = configKey decisionsPath connectionsPath lineIds
        configJson = Lazy.toStrict (Aeson.encode
          (Aeson.Object (KeyMap.fromList
            [ (Key.fromText "connections", Aeson.String connectionsPath),
              (Key.fromText "decisions", Aeson.String decisionsPath),
              (Key.fromText "logical_lines", Aeson.Array (Vector.fromList (map Aeson.String lineIds)))
            ]))) :: ByteString
        configText = TextEncoding.decodeUtf8 configJson

    -- Load previous config JSON for this key
    storedRows <- query conn "SELECT config_json FROM line_config WHERE config_key=?" [SQLText configKey'] :: IO [Only Text]
    let storedConfig = case storedRows of
          [Only json] -> Just json
          _ -> Nothing

    -- Get current ref tips
    refsResult <- listRefs repo
    let currentTips = case refsResult of
          Right refs -> Map.fromList
            [ (refObservationRefName r, refObservationTipOid r)
            | r <- refs,
              refObservationObjectType r == "commit"
            ]
          Left _ -> Map.empty

    -- Get previous ref state from DB
    prevRefState <- query conn
      "SELECT ref_name, tip_oid FROM line_ref_state WHERE config_key=?"
      [SQLText configKey'] :: IO [(Text, Text)]
    let previousTips = Map.fromList prevRefState

    -- Determine relevant refs from logical lines
    let relevantRefs = computeRelevantRefs logicalLines currentTips
    let nowTips = Map.restrictKeys currentTips (Set.fromList relevantRefs)

    -- Check if config changed
    let configChanged = case storedConfig of
          Nothing -> True
          Just stored -> stored /= configText

    -- Check if refs changed
    let refsChanged = previousTips /= nowTips

    -- Decide which operations to target
    opsResult <- registeredOperations conn
    let targetOps = case opsResult of
          Right ops
            | configChanged || refsChanged || not (null newOps) -> ops
            | otherwise -> Map.filterWithKey (\opId _ -> opId `elem` newOps) ops
          Left _ -> Map.empty

    -- If nothing changed and no new ops and no config/refs change, return False
    if null targetOps && not configChanged && not refsChanged && null newOps
      then pure False
      else do
        -- Delete old line ref state
        execute conn "DELETE FROM line_ref_state WHERE config_key=?" [SQLText configKey']

        -- Store new config
        execute conn "INSERT OR REPLACE INTO line_config VALUES(?,?)"
          [ SQLText configKey', SQLText configText ]

        -- Get shallow status
        shallowResult <- isShallowRepository repo
        let isShallowFlag = case shallowResult of
              Right s -> s
              Left _  -> False

        -- Get root directories
        let rootDirs = [decisionsPath, connectionsPath]

        -- Process each logical line
        forM_ logicalLines $ \ll -> do
          let llId = logicalLineId ll
          forM_ (logicalLineRefs ll) $ \gitRef ->
            case gitRef of
              GitRef refName -> do
                -- Check if ref exists
                refExists <- refExistsGit repo refName
                when refExists $ do
                  -- Get first-parent path landings for this ref
                  landingsResult <- firstParentPathLandings repo refName rootDirs
                  case landingsResult of
                    Left _ -> pure ()
                    Right landings -> do
                      -- For each target operation, find the landing commit
                      forM_ (Map.toList targetOps) $ \(opId, opData) -> do
                        let opPaths = Set.fromList [regObjectPath obj | obj <- regOpObjects opData]
                            landingCommits = [Map.lookup p landings | p <- Set.toList opPaths]
                            filteredLandings = filter isJust landingCommits
                            uniqueLanding =
                              if length filteredLandings == 1
                              then head filteredLandings
                              else Nothing
                        case uniqueLanding of
                          Nothing -> pure ()
                          Just landingOid -> do
                            let completeVal = if isShallowFlag then 0 else 1
                                landing = LineLanding
                                  { lineLandingConfigKey = configKey'
                                  , lineLandingOpId = opId
                                  , lineLandingLineId = llId
                                  , lineLandingRefName = refName
                                  , lineLandingCommitOid = landingOid
                                  , lineLandingComplete = completeVal /= 0
                                  }
                            execute conn "INSERT OR REPLACE INTO line_landing VALUES(?,?,?,?,?,?)"
                              [ SQLText (lineLandingConfigKey landing)
                              , SQLText (lineLandingOpId landing)
                              , SQLText (lineLandingLineId landing)
                              , SQLText (lineLandingRefName landing)
                              , SQLText (gitOidText (lineLandingCommitOid landing))
                              , SQLInteger (if lineLandingComplete landing then 1 else 0)
                              ]

          -- Store ref state for this logical line's refs
          forM_ (logicalLineRefs ll) $ \gitRef ->
            case gitRef of
              GitRef refName ->
                case Map.lookup refName currentTips of
                  Just tipOid ->
                    execute conn "INSERT OR REPLACE INTO line_ref_state VALUES(?,?,?)"
                      [ SQLText configKey', SQLText refName, SQLText tipOid ]
                  Nothing -> pure ()

        pure True

  pure result

-- | Helper: check if a git ref exists.
refExistsGit :: Repository -> Text -> IO Bool
refExistsGit repo ref = do
  result <- runRepository repo "ref verify" ["rev-parse", "--verify", Text.unpack ref] BS.empty
  pure (case result of
    Left _ -> False
    Right proc -> processExitCode proc == ExitSuccess)

-- ============================================================
-- Provenance update record
-- ============================================================

-- | Record of what changed during a provenance overlay update.
--
-- Mirrors the Python @ProvenanceUpdate@ dataclass.
data ProvenanceUpdate = ProvenanceUpdate
  { databasePath         :: FilePath,
    fingerprint          :: OverlayFingerprint,
    generation           :: Int,
    commitsScanned       :: Int,
    observedCommitCount  :: Int,
    changed              :: Bool,
    newOperations        :: Int
  }
  deriving (Eq, Show)

-- ============================================================
-- Main orchestration entry point
-- ============================================================

-- | Lazy operation group loader: loads operation groups on demand.
-- Used for the exact-reuse fast path where we avoid loading all documents
-- if every required operation is already registered in the overlay.
type GroupsLoader = IO (Map Text [ParsedManagedDocument])

-- | The main orchestration entry point mirroring Python's
-- @ensure_provenance()@.
--
-- Steps:
--
-- 1. Compute observation fingerprint from refs + roots.
-- 2. If overlay exists and fingerprint matches and all required ops registered
--    → return cached result (no-op).
-- 3. Otherwise:
--    a. Register new operations
--    b. Discover new commits via rev-list delta
--    c. Store commit observations + managed path additions
--    d. Find candidate commits per operation
--    e. Classify candidates (original/copy/introduction)
--    f. Prune unavailable placements (if roots removed)
--    g. Refresh line landings
--    h. Update ref observation + observation root tables
--    i. Update meta table with fingerprint/generation/counters
-- 4. Return 'ProvenanceUpdate' with stats.
ensureProvenance
  :: Repository
  -> Connection
  -> FilePath             -- ^ Current semantic-revision database path
  -> [Text]               -- ^ Logical line IDs (from config)
  -> Text                 -- ^ Decisions path
  -> Text                 -- ^ Connections path
  -> [LogicalLine]        -- ^ Logical lines
  -> Maybe [ParsedManagedDocument]  -- ^ Operation members grouped by op ID (lazy)
  -> [Text]               -- ^ Operation IDs (required_ops)
  -> Maybe GroupsLoader   -- ^ Lazy groups loader
  -> GitOid               -- ^ Target revision
  -> IO (Either SomeException ProvenanceUpdate)
ensureProvenance repo conn currentDbPath _logicalLineIds
  decisionsPath connectionsPath logicalLines maybeParsedDocs operationIds groupsLoader targetRevision = do
  result <- try @SomeException $ do
    let dbPath = provenanceDatabasePath currentDbPath

    -- Get current refs and reflog roots
    refsResult <- listRefs repo
    reflogResult <- reflogCommitRoots repo

    let refs' :: [RefObservation]
        refs' = case refsResult of
          Right r -> r
          Left _  -> []

        reflogOids' :: [GitOid]
        reflogOids' = case reflogResult of
          Right r -> r
          Left _  -> []

    -- Read prior query roots from existing observation_root table
    priorRootOids' <- do
      rootRows <- query_ conn
        "SELECT commit_oid FROM observation_root WHERE kind='query'"
        :: IO [Only Text]
      pure [case mkGitOid oid of Left _ -> error "invalid GitOid in observation_root"; Right oid -> oid
           | Only oid <- rootRows]

    -- Build observation roots using prior roots for delta computation
    observationRoots' <- observationRoots repo targetRevision priorRootOids'
    let refsTuple = case observationRoots' of
          Right t -> t
          Left _  -> (refs', [])

        newRootOids :: [GitOid]
        newRootOids = [observationRootCommitOid r | r <- snd refsTuple]

        oldRootOids :: [GitOid]
        oldRootOids = priorRootOids'

    -- Compute fingerprint
    fp <- case refs' of
          [] -> pure (OverlayFingerprint "0000000000000000000000000000000000000000000000000000000000000000")
          _  -> observationFingerprint refs' (snd refsTuple)

    -- Discover new commits via rev-list delta
    commitsResult <- revListDelta repo newRootOids oldRootOids
    let newCommits :: [GitOid]
        newCommits = case commitsResult of
          Right c -> c
          Left _  -> []

    -- Check if all required operations are already registered
    registeredOps <- query_ conn "SELECT op_id FROM registered_operation"
      :: IO [(Only Text)]
    let registeredSet = Set.fromList (map fromOnly registeredOps)

    -- Compute fingerprint value for cached result
    let fingerprintVal = gitOidText (case fp of OverlayFingerprint t -> GitOid t)

    -- Compute generation from DB
    generationRows <- query_ conn "SELECT value FROM meta WHERE key='generation'" :: IO [Only Text]
    let currentGeneration = case generationRows of
          [Only g] -> case reads (Text.unpack g) of
            [(n, _)] -> n
            _ -> 0
          _ -> 0
        generation = currentGeneration + 1
        generationSql :: Int64
        generationSql = fromIntegral generation

    -- Read observed commit count for cached result
    observedCountRows <- query_ conn "SELECT count(*) FROM observed_commit"
      :: IO [Only Integer]
    let observedCount :: Int
        observedCount = case observedCountRows of
          [Only c] -> fromIntegral c
          _ -> 0
        observedCountSql :: Int64
        observedCountSql = fromIntegral observedCount

    -- Check if overlay exists and fingerprint matches
    metaFingerprintRows <- query_ conn "SELECT value FROM meta WHERE key='observation_fingerprint'" :: IO [Only Text]
    let fingerprintMatches = case metaFingerprintRows of
          [Only f] -> f == fingerprintVal
          _ -> False

    configExistsRows <- query_ conn "SELECT count(*) FROM line_config" :: IO [Only Integer]
    let configExists = case configExistsRows of
          [Only c] -> c > 0
          _ -> False

    -- Fast path check: fingerprint matches, all ops registered, config exists.
    -- When all conditions hold, the overlay is already fully up to date.
    let fastPath = fingerprintMatches
                    && Set.fromList operationIds `Set.isSubsetOf` registeredSet
                    && configExists

    if fastPath
      then do
        -- Update meta table and return cached result.
        -- changed=False and newOperations=0 because the overlay was already
        -- consistent; we only advance the generation counter as a "refresh tick".
        execute conn
          "INSERT INTO meta(key,value) VALUES(?,?) ON CONFLICT(key) DO UPDATE SET value=excluded.value"
          [SQLText "schema", SQLText "adrai-provenance-cache/1"]
        execute conn
          "INSERT INTO meta(key,value) VALUES(?,?) ON CONFLICT(key) DO UPDATE SET value=excluded.value"
          [SQLText "observation_fingerprint", SQLText fingerprintVal]
        execute conn
          "INSERT INTO meta(key,value) VALUES(?,?) ON CONFLICT(key) DO UPDATE SET value=excluded.value"
          [SQLText "generation", SQLInteger generationSql]
        execute conn
          "INSERT INTO meta(key,value) VALUES(?,?) ON CONFLICT(key) DO UPDATE SET value=excluded.value"
          [SQLText "observed_commit_count", SQLInteger observedCountSql]
        pure (ProvenanceUpdate
          { databasePath = dbPath
          , fingerprint = fp
          , generation = generation
          , commitsScanned = length newCommits
          , observedCommitCount = observedCount
          , changed = False
          , newOperations = 0
          })
      else do
        -- LoadMissing helper: load only operations not yet in the overlay
        let loadMissing :: Set.Set Text -> IO (Map Text [ParsedManagedDocument])
            loadMissing missing
              | Set.null missing = pure Map.empty
              | otherwise = case (maybeParsedDocs, groupsLoader) of
                  (Just docs, _) -> pure (Map.filterWithKey (\k _ -> k `Set.member` missing) (groupParsedDocs docs))
                  (Nothing, Just gl) -> do
                    allGroups <- gl
                    let absent = Set.toList (missing `Set.difference` Map.keysSet allGroups)
                    when (not (null absent)) $
                      recordIssue conn "warning" "MISSING_OPERATIONS"
                        ("Operations not available for loading: " <> Text.intercalate "," (sort absent))
                        Nothing Nothing Nothing Nothing
                    pure (Map.filterWithKey (\k _ -> k `Set.member` missing) allGroups)
                  (Nothing, Nothing) -> pure Map.empty

        -- Register new operations using optional parsed docs or groupsLoader
        let groups = maybe Map.empty groupParsedDocs maybeParsedDocs
        newOpsResult <- registerOperationGroups conn groups

        -- Load missing operations for classification.
        -- loadMissing is called primarily for its side effect (MISSING_OPERATIONS
        -- warning) when groupsLoader is used but some operations are absent.
        -- The result is filtered and discarded because the classification
        -- pipeline uses registeredOperation data, not pre-loaded docs.
        let requiredOps = Set.fromList operationIds
        _ <- loadMissing requiredOps

        -- Build the new op list from registration result
        let newOpList :: [Text]
            newOpList = case newOpsResult of
              Right ops -> ops
              Left _  -> []

        -- Store commit observations + managed path additions
        when (not (null newCommits)) $ do
          storeResult <- storeNewCommits repo conn newCommits
          case storeResult of
            Right _ -> pure ()
            Left e  -> recordIssue conn "error" "STORE_NEW_COMMITS_FAILED" (Text.pack (show e)) Nothing Nothing Nothing Nothing

        -- Find candidate commits per operation
        candidatesResult <- candidateCommits conn newCommits newOpList

        -- Classify candidates
        case (candidatesResult, newOpsResult) of
          (Right candidates, Right newOps) ->
            void (processCandidates repo conn candidates newOps)
          _ -> pure ()

        -- Prune unavailable placements
        prunedResult <- pruneUnavailablePlacements repo conn

        -- Refresh line landings (only if new ops or pruned)
        let opsForRefresh = case newOpsResult of
              Right ops ->
                case prunedResult of
                  Right n | n > 0 -> []
                  _ -> ops
              Left _ -> []

        lineChangedResult <- refreshLineLandings repo conn decisionsPath connectionsPath
          logicalLines opsForRefresh maybeParsedDocs groupsLoader
        let lineChanged = case lineChangedResult of
              Right b -> b
              Left _  -> False

        -- Update ref observation table
        execute_ conn "DELETE FROM ref_observation"
        case refsResult of
          Right refs -> do
            forM_ refs $ \refObs ->
              execute conn "INSERT INTO ref_observation VALUES(?,?,?)"
                [ SQLText (refObservationRefName refObs)
                , SQLText (refObservationTipOid refObs)
                , SQLText (refObservationObjectType refObs)
                ]
          Left _ -> pure ()

        -- Update observation root table
        execute_ conn "DELETE FROM observation_root"
        let roots' = case observationRoots' of
              Right (_, roots) -> roots
              Left _  -> []
        forM_ roots' $ \root ->
          execute conn "INSERT INTO observation_root VALUES(?,?,?)"
            [ SQLText (observationRootKind root)
            , SQLText (observationRootName root)
            , SQLText (gitOidText (observationRootCommitOid root))
            ]

        -- Update meta table
        execute conn
          "INSERT INTO meta(key,value) VALUES(?,?) ON CONFLICT(key) DO UPDATE SET value=excluded.value"
          [SQLText "schema", SQLText "adrai-provenance-cache/1"]
        execute conn
          "INSERT INTO meta(key,value) VALUES(?,?) ON CONFLICT(key) DO UPDATE SET value=excluded.value"
          [SQLText "observation_fingerprint", SQLText fingerprintVal]
        execute conn
          "INSERT INTO meta(key,value) VALUES(?,?) ON CONFLICT(key) DO UPDATE SET value=excluded.value"
          [SQLText "generation", SQLInteger generationSql]
        execute conn
          "INSERT INTO meta(key,value) VALUES(?,?) ON CONFLICT(key) DO UPDATE SET value=excluded.value"
          [SQLText "observed_commit_count", SQLInteger observedCountSql]

        let newOpsCount = case newOpsResult of
              Right ops -> length ops
              Left _ -> 0

        pure (ProvenanceUpdate
          { databasePath = dbPath
          , fingerprint = fp
          , generation = generation
          , commitsScanned = length newCommits
          , observedCommitCount = observedCount
          , changed = lineChanged || not (null newCommits) || newOpsCount > 0
          , newOperations = newOpsCount
          })

  pure result

-- | Group parsed managed documents by operation ID.
groupParsedDocs :: [ParsedManagedDocument] -> Map Text [ParsedManagedDocument]
groupParsedDocs docs = Map.fromListWith (++)
  [ (parsedDocumentObjectRef doc, [doc]) | doc <- docs ]

-- ============================================================
-- Overlay rows query
-- ============================================================

-- | Query the overlay for operation placements, line landings, and issues.
--
-- Returns a 3-tuple of lists suitable for consumption by the show/doctor
-- commands:
--
-- 1. Operation placements: @(op_id, commit_oid, classification, authored_s,
--    committed_s, subject, parents_json)@
-- 2. Line landings: @(op_id, line_id, ref_name, commit_oid, complete)@
-- 3. Issues: @(severity, code, adr_id, object_id, path, message)@
--
-- Mirrors the Python @overlay_rows_for_operations()@ function.
overlayRowsForOperations
  :: FilePath
  -> [Text]    -- ^ Operation IDs
  -> Text      -- ^ Config key
  -> IO (Either SomeException ([(Text,Text,Text,Int,Int,Text,Text)],
                           [(Text,Text,Text,Text,Int)],
                           [(Text,Text,Maybe Text,Maybe Text,Maybe Text,Text)]))
overlayRowsForOperations overlayPath opIds configKey' = do
  result <- try @SomeException $ do
    if null opIds
      then pure ([], [], [])
      else do
        conn <- open overlayPath
        let placeholders = Text.intercalate "," (replicate (length opIds) "?")
            params = map SQLText opIds

        -- Operation placements
        placements <- query conn
          (asQuery ("SELECT op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json "
           <> "FROM operation_commit WHERE op_id IN (" <> placeholders <> ")"))
          params :: IO [(Text, Text, Text, Integer, Integer, Text, Text)]

        -- Line landings
        landings <- query conn
          (asQuery ("SELECT op_id,line_id,ref_name,commit_oid,complete "
           <> "FROM line_landing WHERE config_key=? AND op_id IN (" <> placeholders <> ")"))
          (SQLText configKey' : params) :: IO [(Text, Text, Text, Text, Integer)]

        -- Issues
        issues <- query conn
          (asQuery ("SELECT severity,code,adr_id,object_id,path,message "
           <> "FROM provenance_issue WHERE op_id IN (" <> placeholders <> ")"))
          params :: IO [(Text, Text, Maybe Text, Maybe Text, Maybe Text, Text)]

        close conn

        let placements' =
              [ (opId, commitOid, classification, fromInteger authored, fromInteger committed, subject, parents)
              | (opId, commitOid, classification, authored, committed, subject, parents) <- placements
              ]
            landings' =
              [ (opId, lineId, refName, commitOid, fromInteger complete)
              | (opId, lineId, refName, commitOid, complete) <- landings
              ]
            issues' =
              [ (severity, code, adrId, objectId, path, message)
              | (severity, code, adrId, objectId, path, message) <- issues
              ]

        pure (placements', landings', issues')

  pure result

-- ============================================================
-- Utility helpers
-- ============================================================

-- | Compute the set of relevant refs from logical lines that exist in current tips.
computeRelevantRefs :: [LogicalLine] -> Map Text Text -> [Text]
computeRelevantRefs logicalLines currentTips =
  [ refName
    | LogicalLine _ refs <- logicalLines,
      refName <- map gitRefText refs,
      Map.member refName currentTips
  ]

-- | Convert a 'Digest' to a hex-encoded 'Text' (40-char SHA-256 hex string).
digestToHex :: Digest -> Text
digestToHex (Digest bytes) =
  Text.pack (concatMap byteToHex (BS.unpack bytes))
  where
    byteToHex :: Word8 -> String
    byteToHex w =
      [ hexDigit ((w `shiftR` 4) .&. 0xF),
        hexDigit (w .&. 0xF)
      ]
    hexDigit n
      | n < 10    = chr (ord '0' + fromIntegral n)
      | otherwise = chr (ord 'a' + fromIntegral n - 10)
    chr = toEnum
    ord = fromEnum

-- | GitError -> SomeException wrapper
data GitErrorSome = GitErrorSome GitError
  deriving (Eq, Show)

instance Exception GitErrorSome
