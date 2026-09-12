{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
    ensureProvenanceWithRecoveryWitness,
    ensureProvenanceWithRecoveryWitnessAndWorklistObserver,

    -- | Narrow recovery-authority seam for real Git regression tests
    usableWitnessRange,
    recoveryWitnessSuppresses,
    recoveryProjectionValidity,

    -- | Bounded ancestry cache seam used by provenance maintenance tests
    extendTargetAncestryCache,

    -- | Seed overlay operation registrations from an immutable semantic cache
    seedRegisteredOperationsFromSemanticCache,
    seedRegisteredOperationsFromSemanticCacheWithQueryObserver,

    -- | Update record
    ProvenanceUpdate (..),

    -- | Overlay rows query
    overlayRowsForOperations,

    -- | Read immutable target-relative provenance evidence
    readProvenanceEvidenceAt,
    readProvenanceEvidenceAtWith,
    openReadOnly,
    openReadWriteExisting,
  )
where

import Adrai.Git
  ( GitError (..),
    GitCommitNode,
    GitOid (..),
    GitObjectType (GitCommitObject),
    GitObjectInfo (..),
    GitProcessResult (..),
    Repository (..),
    batchObjectInfo,
    gitCommitNodeOid,
    gitCommitNodeParents,
    gitOidText,
    isShallowRepository,
    reachableCommitGraphAt,
    runRepository,
  )
import Adrai.Provenance
  ( OverlayFingerprint (..),
    mkGitOid,
    provenanceOperationId,
    sha256Digest,
  )
import Adrai.Provenance.Classification
  ( ParsedManagedDocument (..),
    RegisteredObjectData (..),
    RegisteredOperationData (..),
    candidateCommits,
    findAllOpTrailers,
    issueKey,
    operationMemberSignature,
    processCandidates,
    pruneUnavailablePlacements,
    registerOperationGroups,
    recordIssue,
    registeredOperations,
    storeNewCommits,
  )
import Adrai.Provenance.Overlay
  ( ObservationRoot (..),
    RefObservation (..),
    LineLanding (..),
    RegisteredOperationRow (..),
    RegisteredObjectRow (..),
    OperationCommitRow (..),
    LineConfigRow (..),
    LineRefStateRow (..),
    LineLandingRow (..),
    RefObservationRow (..),
    ObservationRootRow (..),
    ProvenanceIssueRow (..),
    ProvenanceOperationEvidence (..),
    ProvenanceEvidence (..),
    ProvenanceEvidenceError (..),
    overlaySchemaVersion,
    provenanceDatabasePath,
   )
import Adrai.Provenance.RecoveryWitness (ProvenanceRecoveryWitness (..))
import Adrai.Provenance.Discovery
  ( firstParentPathLandings,
    listRefs,
    observationFingerprint,
    observationRoots,
    revListDelta,
  )

import Adrai.Sqlite (asQuery)
import Adrai.Types
  ( LogicalLine (..),
    GitRef (..),
    Digest (..),
    gitRefText,
    operationIdText,
  )
import Control.Exception
  ( SomeAsyncException,
    Exception,
    SomeException,
    bracket,
    fromException,
    throwIO,
    toException,
    try,
  )
import Control.Monad (forM_, unless, when, void)
import Data.Maybe (isJust)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bits ((.&.), shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lazy (toStrict)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Vector as Vector
import Database.SQLite.Simple
  ( Connection,
    Only (..),
    SQLData (SQLNull, SQLText, SQLInteger),
     execute,
     executeMany,
     execute_,
    open,
    query,
    query_,
    close,
    withTransaction,
  )
import System.Exit (ExitCode (ExitSuccess))
import Data.Word (Word8)
import Data.Int (Int64)
import System.Directory (makeAbsolute)

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
  let json = TextEncoding.encodeUtf8 (configJsonText decisionsPath connectionsPath logicalLines)
      digest = sha256Digest json
  in digestToHex digest

configJsonText :: Text -> Text -> [Text] -> Text
configJsonText decisionsPath connectionsPath logicalLines =
  TextEncoding.decodeUtf8 (Lazy.toStrict (Aeson.encode
    (Aeson.Object (KeyMap.fromList
      [ (Key.fromText "connections", Aeson.String connectionsPath),
        (Key.fromText "decisions", Aeson.String decisionsPath),
        (Key.fromText "logical_lines", Aeson.Array (Vector.fromList (map Aeson.String logicalLines)))
       ]))))

-- | Preserve the first occurrence so Git observation and candidate processing
-- see one stable, shared worklist even when a target is also a delta or a
-- historical recovery candidate.
deduplicateOids :: [GitOid] -> [GitOid]
deduplicateOids = go Set.empty
  where
    go _ [] = []
    go seen (oid:rest)
      | oid `Set.member` seen = go seen rest
      | otherwise = oid : go (Set.insert oid seen) rest

-- | A persisted archive may reduce recovery only after Git independently
-- proves that it is a complete immutable ancestor authority.  The target graph
-- establishes both the source's complete all-parent closure and the exact
-- target-relative range without additional Git history processes.
usableWitnessRange
  :: Repository
  -> GitOid
  -> Maybe ProvenanceRecoveryWitness
  -> IO (Maybe [GitOid])
usableWitnessRange repository target maybeWitness = do
  (_, _, witnessedRange) <- newTargetReachabilityChecker repository target
  witnessedRange maybeWitness

usableWitnessRangeFromTarget
  :: Repository
  -> GitOid
  -> IO (Either GitError TargetReachability)
  -> Maybe ProvenanceRecoveryWitness
  -> IO (Maybe [GitOid])
usableWitnessRangeFromTarget _ _ _ Nothing = pure Nothing
usableWitnessRangeFromTarget repository target readTarget (Just witness) = do
  shallow <- isShallowRepository repository
  case shallow of
    Right False -> do
      targetResult <- readTarget
      pure $ case targetResult of
        Left _ -> Nothing
        Right cachedTarget -> do
          targetReachability <- either (const Nothing) Just
            (validateTargetReachability target (targetReachabilityGraph cachedTarget))
          let nodeMap = Map.fromList
                [ (gitCommitNodeOid node, gitCommitNodeParents node)
                | node <- targetReachabilityGraph targetReachability
                ]
          sourceReachable <- commitClosure
            nodeMap
            (recoveryWitnessSourceTarget witness)
          if sourceReachable == recoveryWitnessSourceReachability witness
            then Just
              ( Set.toAscList
                  (targetReachabilityOids targetReachability `Set.difference` sourceReachable)
              )
            else Nothing
    _ -> pure Nothing

-- | Decide whether a source-authorized operation needs no historical replay.
-- The caller supplies only facts established in the current transaction; this
-- is kept pure so regressions cannot accidentally make a test-only decision
-- diverge from production recovery planning.
recoveryWitnessSuppresses
  :: Maybe ProvenanceRecoveryWitness
  -> Maybe [GitOid]
  -> Map Text Text
  -> Map Text Bool
  -> Map Text Bool
  -> Text
  -> Bool
recoveryWitnessSuppresses maybeWitness witnessedRange registeredSignatures projectionValidity targetPlacementChecks operationId =
  case maybeWitness of
    Just witness
      | Just witnessedRange' <- witnessedRange
      , Map.lookup operationId registeredSignatures == Map.lookup operationId (recoveryWitnessSignatures witness)
      , Set.member operationId (recoveryWitnessCoveredOperations witness)
      , Set.member operationId (recoveryWitnessPlacedOperations witness)
      , Map.lookup operationId projectionValidity == Just True
      , Map.lookup operationId targetPlacementChecks == Just True
      , not (null witnessedRange') -> True
    _ -> False

-- | One batched, transaction-local comparison of the source-authorized
-- provenance projection against the mutable overlay.  It deliberately makes
-- a fixed number of reads regardless of operation count.
recoveryProjectionValidity
  :: Connection -> [Text] -> Text -> Text -> ProvenanceRecoveryWitness -> IO (Map Text Bool)
recoveryProjectionValidity conn operationIds requestedConfigKey _requestedConfigText witness
  | null operationIds = pure Map.empty
  | otherwise = do
      let placeholders = Text.intercalate "," (replicate (length operationIds) "?")
          parameters = map SQLText operationIds
      placementRows <- query conn
        (asQuery ("SELECT op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json FROM operation_commit WHERE op_id IN (" <> placeholders <> ")"))
        parameters :: IO [(Text, Text, Text, Integer, Integer, Text, Text)]
      issueRows <- query conn
        (asQuery ("SELECT op_id,severity,code,adr_id,object_id,path,message FROM provenance_issue WHERE op_id IN (" <> placeholders <> ")"))
        parameters :: IO [(Text, Text, Text, Maybe Text, Maybe Text, Maybe Text, Text)]
      landingRows <- query conn
        (asQuery ("SELECT config_key,op_id,line_id,ref_name,commit_oid,complete FROM line_landing WHERE op_id IN (" <> placeholders <> ")"))
        parameters :: IO [(Text, Text, Text, Text, Text, Integer)]
      configRows <- query conn
        "SELECT config_key,config_json FROM line_config WHERE config_key=?"
        (Only requestedConfigKey) :: IO [(Text, Text)]
      let sourceReachable = recoveryWitnessSourceReachability witness
          relevantPlacement (_, rawOid, _, _, _, _, _) = either (const False) (`Set.member` sourceReachable) (mkGitOid rawOid)
          relevantLanding (_, _, _, _, rawOid, _) = either (const False) (`Set.member` sourceReachable) (mkGitOid rawOid)
          groupedBy first rows = Map.fromListWith (<>) [(first row, [row]) | row <- rows]
          placementsByOperation = groupedBy (\(op, _, _, _, _, _, _) -> op) (filter relevantPlacement placementRows)
          issuesByOperation = groupedBy (\(op, _, _, _, _, _, _) -> op) issueRows
          landingsByOperation = groupedBy (\(_, op, _, _, _, _) -> op) (filter relevantLanding landingRows)
          expectedPlacements = groupedBy (\(op, _, _, _, _, _, _) -> op) (Set.toList (recoveryWitnessPlacements witness))
          expectedIssues = groupedBy (\(op, _, _, _, _, _, _) -> op) (Set.toList (recoveryWitnessIssues witness))
          expectedLandings = groupedBy (\(_, op, _, _, _, _) -> op) (Set.toList (recoveryWitnessLandings witness))
          currentConfigs = Set.fromList configRows
      pure (Map.fromList
        [ (operationId, currentConfigs == recoveryWitnessLineConfigs witness
            && Set.fromList (Map.findWithDefault [] operationId placementsByOperation) == Set.fromList (Map.findWithDefault [] operationId expectedPlacements)
            && Set.fromList (Map.findWithDefault [] operationId issuesByOperation) == Set.fromList (Map.findWithDefault [] operationId expectedIssues)
            && Set.fromList (Map.findWithDefault [] operationId landingsByOperation) == Set.fromList (Map.findWithDefault [] operationId expectedLandings))
        | operationId <- operationIds ])

importWitnessBaseline :: Connection -> [Text] -> Map Text Text -> Text -> Text -> ProvenanceRecoveryWitness -> IO ()
importWitnessBaseline conn operationIds registeredSignatures requestedConfigKey requestedConfigText witness
  | recoveryWitnessLineConfigs witness /= Set.singleton (requestedConfigKey, requestedConfigText) = pure ()
  | otherwise = do
      forM_ (Set.toList (recoveryWitnessLineConfigs witness)) $ \(configKey', configText') ->
        execute conn "INSERT OR REPLACE INTO line_config(config_key,config_json) VALUES(?,?)"
          [SQLText configKey', SQLText configText']
      let eligibleOperations = Set.fromList
            [ operationId
            | operationId <- operationIds
            , Map.lookup operationId registeredSignatures == Map.lookup operationId (recoveryWitnessSignatures witness)
            , Set.member operationId (recoveryWitnessCoveredOperations witness)
            , Set.member operationId (recoveryWitnessPlacedOperations witness)
            ]
          eligibleRows predicate rows = Set.toList (Set.filter (\row -> Set.member (predicate row) eligibleOperations) rows)
          placements = eligibleRows (\(op, _, _, _, _, _, _) -> op) (recoveryWitnessPlacements witness)
          issues = eligibleRows (\(op, _, _, _, _, _, _) -> op) (recoveryWitnessIssues witness)
          landings = eligibleRows (\(_, op, _, _, _, _) -> op) (recoveryWitnessLandings witness)
      execute_ conn "CREATE TEMP TABLE IF NOT EXISTS recovery_witness_operation(op_id TEXT PRIMARY KEY)"
      execute_ conn "CREATE TEMP TABLE IF NOT EXISTS recovery_witness_reachable(commit_oid TEXT PRIMARY KEY)"
      execute_ conn "DELETE FROM recovery_witness_operation"
      execute_ conn "DELETE FROM recovery_witness_reachable"
      executeMany conn "INSERT INTO recovery_witness_operation(op_id) VALUES(?)"
        [[SQLText operationId] | operationId <- Set.toList eligibleOperations]
      executeMany conn "INSERT INTO recovery_witness_reachable(commit_oid) VALUES(?)"
        [[SQLText (gitOidText oid)] | oid <- Set.toList (recoveryWitnessSourceReachability witness)]
      execute_ conn
        "DELETE FROM operation_commit WHERE op_id IN (SELECT op_id FROM recovery_witness_operation) AND commit_oid IN (SELECT commit_oid FROM recovery_witness_reachable)"
      executeMany conn "INSERT OR REPLACE INTO operation_commit(op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json) VALUES(?,?,?,?,?,?,?)"
        [[SQLText op, SQLText oid, SQLText classification, SQLInteger (fromInteger authored), SQLInteger (fromInteger committed), SQLText subject, SQLText parents] | (op, oid, classification, authored, committed, subject, parents) <- placements]
      executeMany conn "INSERT OR REPLACE INTO provenance_issue(issue_key,severity,code,adr_id,object_id,path,message,op_id) VALUES(?,?,?,?,?,?,?,?)"
        [[ SQLText (issueKey code message (Just op) objectId path), SQLText severity, SQLText code
         , maybe SQLNull SQLText adr, maybe SQLNull SQLText objectId, maybe SQLNull SQLText path
         , SQLText message, SQLText op
         ] | (op, severity, code, adr, objectId, path, message) <- issues]
      execute conn
        "DELETE FROM line_landing WHERE config_key=? AND op_id IN (SELECT op_id FROM recovery_witness_operation) AND commit_oid IN (SELECT commit_oid FROM recovery_witness_reachable)"
        (Only requestedConfigKey)
      executeMany conn "INSERT OR REPLACE INTO line_landing(config_key,op_id,line_id,ref_name,commit_oid,complete) VALUES(?,?,?,?,?,?)"
        [[SQLText configKey', SQLText op, SQLText lineId, SQLText refName, SQLText oid, SQLInteger (fromInteger complete)] | (configKey', op, lineId, refName, oid, complete) <- landings]

-- | Extend an ancestry-result cache with one shared batch for all unseen
-- placement candidates.  Keeping this seam independent of repository access
-- makes the bounded-request contract directly testable.
extendTargetAncestryCache
  :: ([GitOid] -> IO (Either GitError (Map GitOid Bool)))
  -> Map GitOid Bool
  -> [GitOid]
  -> IO (Either GitError (Map GitOid Bool))
extendTargetAncestryCache checkCandidates cache candidates = do
  let missing = deduplicateOids [candidate | candidate <- candidates, Map.notMember candidate cache]
  if null missing
    then pure (Right cache)
    else do
      checked <- checkCandidates missing
      pure $ do
        answers <- checked
        let requested = Set.fromList missing
            answerKeys = Map.keysSet answers
        if answerKeys == requested
          then Right (Map.union cache answers)
          else Left (GitCommandFailed "target ancestry" 128 "" "batched target ancestry returned an incomplete or unexpected candidate set")

-- | Construct one target-relative reachability proof that can be reused by
-- every certification pass in an overlay refresh.  Candidates already in the
-- decoded graph are proven commits; only outsiders require bounded object-info
-- checks, preserving the old fail-closed behavior for missing or non-commit
-- placement rows without probing every reachable placement.
newTargetReachabilityChecker
  :: Repository
  -> GitOid
  -> IO
       ( [GitOid] -> IO (Either GitError (Map GitOid Bool)),
         IO (Either GitError (Set.Set GitOid)),
         Maybe ProvenanceRecoveryWitness -> IO (Maybe [GitOid])
       )
newTargetReachabilityChecker repository target = do
  reachableRef <- newIORef Nothing
  let checkCandidates candidates = do
        let uniqueCandidates = deduplicateOids candidates
        reachableResult <- cachedTarget reachableRef
        case reachableResult of
          Left problem -> pure (Left problem)
          Right targetReachability -> do
            let reachable = targetReachabilityOids targetReachability
            let outsiders = filter (`Set.notMember` reachable) uniqueCandidates
            infosResult <- batchObjectInfo repository outsiders
            pure $ do
              infos <- infosResult
              validateCommitCandidates infos outsiders
              Right (Map.fromList [(candidate, candidate `Set.member` reachable) | candidate <- uniqueCandidates])
      readTarget = cachedTarget reachableRef
  pure
    ( checkCandidates,
      fmap (fmap targetReachabilityOids) readTarget,
      usableWitnessRangeFromTarget repository target readTarget
    )
  where
    cachedTarget reachableRef = do
      cached <- readIORef reachableRef
      case cached of
        Just targetReachability -> pure (Right targetReachability)
        Nothing -> do
          graphResult <- reachableCommitGraphAt repository target
          case graphResult of
            Left problem -> pure (Left problem)
            Right graph -> do
              let targetReachability = TargetReachability
                    { targetReachabilityGraph = graph,
                      targetReachabilityOids = Set.insert target (Set.fromList (map gitCommitNodeOid graph))
                    }
              writeIORef reachableRef (Just targetReachability)
              pure (Right targetReachability)

    validateCommitCandidates infos candidates = do
      _ <- traverse validate candidates
      pure ()
      where
        validate candidate =
          case Map.lookup candidate infos of
            Just (Just info) | objectInfoType info == GitCommitObject -> Right ()
            _ ->
              Left
                ( GitCommandFailed
                    "target ancestry"
                    128
                    ""
                    ("candidate is unavailable or is not a commit: " <> gitOidText candidate)
                )

data TargetReachability = TargetReachability
  { targetReachabilityGraph :: [GitCommitNode],
    targetReachabilityOids :: Set.Set GitOid
  }

data GraphTraversalStep
  = EnterCommit GitOid
  | LeaveCommit GitOid

validateTargetReachability
  :: GitOid
  -> [GitCommitNode]
  -> Either GitError TargetReachability
validateTargetReachability target graph = do
  let nodeRows =
        [ (gitCommitNodeOid node, gitCommitNodeParents node)
        | node <- graph
        ]
      nodeMap = Map.fromList nodeRows
      supplied = Map.keysSet nodeMap
  if Map.size nodeMap /= length graph
    then Left (invalidTargetGraph "reachable graph contains duplicate commit nodes")
    else pure ()
  traversed <- traverseTargetGraph nodeMap target
  if traversed /= supplied
    then Left (invalidTargetGraph "reachable graph contains nodes outside the target traversal")
    else Right
      TargetReachability
        { targetReachabilityGraph = graph,
          targetReachabilityOids = traversed
        }

traverseTargetGraph
  :: Map GitOid [GitOid]
  -> GitOid
  -> Either GitError (Set.Set GitOid)
traverseTargetGraph nodeMap target = go Set.empty Set.empty [EnterCommit target]
  where
    go _ completed [] = Right completed
    go active completed (EnterCommit oid : rest)
      | oid `Set.member` completed = go active completed rest
      | oid `Set.member` active =
          Left (invalidTargetGraph "reachable graph contains a parent cycle")
      | otherwise =
          case Map.lookup oid nodeMap of
            Nothing -> Left (invalidTargetGraph "reachable graph references a missing commit node")
            Just parents ->
              go
                (Set.insert oid active)
                completed
                (map EnterCommit parents <> (LeaveCommit oid : rest))
    go active completed (LeaveCommit oid : rest)
      | oid `Set.member` active =
          go (Set.delete oid active) (Set.insert oid completed) rest
      | otherwise =
          Left (invalidTargetGraph "reachable graph traversal closed an inactive commit")

commitClosure :: Map GitOid [GitOid] -> GitOid -> Maybe (Set.Set GitOid)
commitClosure nodeMap source
  | Map.notMember source nodeMap = Nothing
  | otherwise = Just (go Set.empty [source])
  where
    go completed [] = completed
    go completed (oid : rest)
      | oid `Set.member` completed = go completed rest
      | otherwise =
          case Map.lookup oid nodeMap of
            Nothing -> completed
            Just parents -> go (Set.insert oid completed) (parents <> rest)

invalidTargetGraph :: Text -> GitError
invalidTargetGraph message =
  GitCommandFailed "target ancestry" 128 "" message

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
        configText = configJsonText decisionsPath connectionsPath lineIds

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
                            uniqueLanding = case filteredLandings of
                              [landing] -> landing
                              _ -> Nothing
                        case uniqueLanding of
                          Nothing -> pure ()
                          Just landingOid -> do
                            let landing = LineLanding
                                  { lineLandingConfigKey = configKey'
                                  , lineLandingOpId = opId
                                  , lineLandingLineId = llId
                                  , lineLandingRefName = refName
                                  , lineLandingCommitOid = landingOid
                                  , lineLandingComplete = not isShallowFlag
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
    newOperations        :: Int,
    -- | The immutable target graph used for this refresh.  Cache publication
    -- consumes it directly rather than observing history a second time.
    targetReachableCommits :: Set.Set GitOid
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
  ensureProvenanceWithRecoveryWitness repo conn currentDbPath _logicalLineIds
    decisionsPath connectionsPath logicalLines maybeParsedDocs operationIds groupsLoader targetRevision Nothing []

-- | Variant used by post-commit compilation.  The optional witness is an
-- independently validated immutable source archive; it can only suppress a
-- replay of source-complete operations and never authorizes an exact target
-- result or semantic payload reuse.
ensureProvenanceWithRecoveryWitness
  :: Repository
  -> Connection
  -> FilePath
  -> [Text]
  -> Text
  -> Text
  -> [LogicalLine]
  -> Maybe [ParsedManagedDocument]
  -> [Text]
  -> Maybe GroupsLoader
  -> GitOid
  -> Maybe ProvenanceRecoveryWitness
  -> [Text]
  -> IO (Either SomeException ProvenanceUpdate)
ensureProvenanceWithRecoveryWitness repo conn currentDbPath _logicalLineIds
  decisionsPath connectionsPath logicalLines maybeParsedDocs operationIds groupsLoader targetRevision maybeWitness reseededChangedOperations =
  ensureProvenanceWithRecoveryWitnessAndWorklistObserver (const (pure ()))
    repo conn currentDbPath _logicalLineIds decisionsPath connectionsPath logicalLines
    maybeParsedDocs operationIds groupsLoader targetRevision maybeWitness reseededChangedOperations

-- | Testable recovery form.  The observer runs once, after the complete
-- target-relative worklist is fixed and before any commit observation or
-- classification.  Production delegates with a no-op observer.
ensureProvenanceWithRecoveryWitnessAndWorklistObserver
  :: ([GitOid] -> IO ())
  -> Repository
  -> Connection
  -> FilePath
  -> [Text]
  -> Text
  -> Text
  -> [LogicalLine]
  -> Maybe [ParsedManagedDocument]
  -> [Text]
  -> Maybe GroupsLoader
  -> GitOid
  -> Maybe ProvenanceRecoveryWitness
  -> [Text]
  -> IO (Either SomeException ProvenanceUpdate)
ensureProvenanceWithRecoveryWitnessAndWorklistObserver observeWorklist repo conn currentDbPath _logicalLineIds
  decisionsPath connectionsPath logicalLines maybeParsedDocs operationIds groupsLoader targetRevision maybeWitness reseededChangedOperations = do
  result <- try @SomeException $ do
    let dbPath = provenanceDatabasePath currentDbPath

    -- Every previously observed root participates in the next delta boundary.
    -- Restricting this to query roots re-walked refs/reflogs on every refresh
    -- and made a hidden reflog commit indistinguishable from a cold scan.
    priorRootOids' <- do
      rootRows <- query_ conn
        "SELECT commit_oid FROM observation_root"
        :: IO [Only Text]
      pure [case mkGitOid oid of Left _ -> error "invalid GitOid in observation_root"; Right parsedOid -> parsedOid
           | Only oid <- rootRows]

    -- Build observation roots using prior roots for delta computation
    refsTuple <- observationRoots repo targetRevision priorRootOids' >>= either (throwIO . GitErrorSome) pure
    let refs' = fst refsTuple

        newRootOids :: [GitOid]
        newRootOids = [observationRootCommitOid r | r <- snd refsTuple]

        oldRootOids :: [GitOid]
        oldRootOids = priorRootOids'

    -- Compute fingerprint
    fp <- case refs' of
          [] -> pure (OverlayFingerprint "0000000000000000000000000000000000000000000000000000000000000000")
          _  -> observationFingerprint refs' (snd refsTuple)

    -- Discover new commits via rev-list delta
    newCommits <- revListDelta repo newRootOids oldRootOids >>= either (throwIO . GitErrorSome) pure

    -- Check if all required operations are already registered
    registeredOps <- query_ conn "SELECT op_id,signature FROM registered_operation"
      :: IO [(Text, Text)]
    let registeredSet = Set.fromList (map fst registeredOps)
        registeredSignatures = Map.fromList registeredOps

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

    -- Read observed commit count from the authority table.  The slow path
    -- re-reads this after storing both delta and target commits so published
    -- metadata describes the committed overlay, not the pre-refresh state.
    let readObservedCount = do
          observedCountRows <- query_ conn "SELECT count(*) FROM observed_commit"
            :: IO [Only Integer]
          pure $ case observedCountRows of
            [Only c] -> fromIntegral c
            _ -> 0
    observedCount <- readObservedCount
    let
        observedCountSql :: Int64
        observedCountSql = fromIntegral observedCount

    -- Check if overlay exists and fingerprint matches
    metaFingerprintRows <- query_ conn "SELECT value FROM meta WHERE key='observation_fingerprint'" :: IO [Only Text]
    let fingerprintMatches = case metaFingerprintRows of
          [Only f] -> f == fingerprintVal
          _ -> False

    let requestedLineIds = map logicalLineId logicalLines
        requestedConfigKey = configKey decisionsPath connectionsPath requestedLineIds
        requestedConfigText = configJsonText decisionsPath connectionsPath requestedLineIds
    configRows <- query conn
      "SELECT config_json FROM line_config WHERE config_key=?"
      [SQLText requestedConfigKey]
      :: IO [Only Text]
    let configReady = configRows == [Only requestedConfigText]

    (targetReachability, readTargetReachability, deriveWitnessRange) <-
      newTargetReachabilityChecker repo targetRevision
    witnessedRange <- deriveWitnessRange maybeWitness

    -- A placement is target-ready only when its individual commit is an
    -- ancestor of the immutable caller target.  One shared target graph and
    -- bounded candidate batches replace one merge-base process per placement.
    placementRows <-
      if null operationIds
        then pure []
        else do
          let placeholders = Text.intercalate "," (replicate (length operationIds) "?")
          query conn
            (asQuery
              ( "SELECT op_id, commit_oid FROM operation_commit WHERE op_id IN ("
                  <> placeholders <> ")"
              )
            )
            (map SQLText operationIds)
            :: IO [(Text, Text)]
    let placementsByOperation = Map.fromListWith (<>)
          [ (operationId, [placementOid])
          | (operationId, rawOid) <- placementRows
          , Right placementOid <- [mkGitOid rawOid]
          ]
        placementCandidates = deduplicateOids (concat (Map.elems placementsByOperation))
    placementAncestryResult <- extendTargetAncestryCache
      targetReachability
      Map.empty
      placementCandidates
    placementAncestry <- case placementAncestryResult of
      Left problem -> throwIO (GitErrorSome problem)
      Right cache -> pure cache
    coverageRows <-
      if null operationIds
        then pure []
        else do
          let placeholders = Text.intercalate "," (replicate (length operationIds) "?")
          query conn
            (asQuery ("SELECT op_id,registration_signature FROM operation_target_coverage WHERE target_oid=? AND op_id IN (" <> placeholders <> ")"))
            (SQLText (gitOidText targetRevision) : map SQLText operationIds)
            :: IO [(Text, Text)]
    case (maybeWitness, witnessedRange) of
      (Just witness, Just range) | not (null range) ->
        importWitnessBaseline conn operationIds registeredSignatures requestedConfigKey requestedConfigText witness
      _ -> pure ()
    placementRowsAfterImport <-
      if null operationIds
        then pure []
        else do
          let placeholders = Text.intercalate "," (replicate (length operationIds) "?")
          query conn
            (asQuery ("SELECT op_id,commit_oid FROM operation_commit WHERE op_id IN (" <> placeholders <> ")"))
            (map SQLText operationIds)
            :: IO [(Text, Text)]
    let placementsAfterImport = Map.fromListWith (<>)
          [ (operationId, [placementOid])
          | (operationId, rawOid) <- placementRowsAfterImport
          , Right placementOid <- [mkGitOid rawOid]
          ]
    placementAncestryAfterImportResult <- extendTargetAncestryCache
      targetReachability placementAncestry (deduplicateOids (concat (Map.elems placementsAfterImport)))
    placementAncestryAfterImport <- case placementAncestryAfterImportResult of
      Left problem -> throwIO (GitErrorSome problem)
      Right cache -> pure cache
    projectionValidity <- case maybeWitness of
      Nothing -> pure Map.empty
      Just witness -> recoveryProjectionValidity conn operationIds requestedConfigKey requestedConfigText witness
    let targetPlacementChecks =
          [ any (\placementOid -> Map.lookup placementOid placementAncestryAfterImport == Just True)
              (Map.findWithDefault [] operationId placementsAfterImport)
          | operationId <- operationIds
          ]
        coverageByOperation = Map.fromListWith (<>)
          [ (operationId, [signature]) | (operationId, signature) <- coverageRows ]
        coverageChecks =
          [ Map.lookup operationId registeredSignatures == Just signature
              && Map.lookup operationId coverageByOperation == Just [signature]
          | operationId <- operationIds
          , let signature = Map.findWithDefault "" operationId registeredSignatures
          ]
        targetReady = and (zipWith (&&) targetPlacementChecks coverageChecks)
        -- A missing or mismatched target certificate must replay every
        -- already-indexed candidate for that operation.  Placement rows are
        -- intentionally repository-wide; target ancestry is consulted only
        -- later when issuing this target's certificate.
        recoveryUncoveredOperations =
          [ operationId
          | (operationId, covered) <- zip operationIds coverageChecks
          , not covered
          && not
            (recoveryWitnessSuppresses maybeWitness witnessedRange registeredSignatures projectionValidity
              (Map.fromList (zip operationIds targetPlacementChecks)) operationId)
          ]

    -- Fast path check: fingerprint matches, all ops are registered, this
    -- exact requested configuration exists, and every requested operation has
    -- reachable placement evidence in the caller-resolved immutable target.
    -- The reachability condition prevents a globally observed commit on an
    -- unrelated branch from hiding incomplete operation-specific evidence.
    let fastPath = fingerprintMatches
                    && Set.fromList operationIds `Set.isSubsetOf` registeredSet
                    && configReady
                    && targetReady

    if fastPath
      then do
        -- Update meta table and return cached result.
        -- changed=False and newOperations=0 because the overlay was already
        -- consistent; we only advance the generation counter as a "refresh tick".
        execute conn
          "INSERT INTO meta(key,value) VALUES(?,?) ON CONFLICT(key) DO UPDATE SET value=excluded.value"
          [SQLText "schema", SQLText overlaySchemaVersion]
        execute conn
          "INSERT INTO meta(key,value) VALUES(?,?) ON CONFLICT(key) DO UPDATE SET value=excluded.value"
          [SQLText "observation_fingerprint", SQLText fingerprintVal]
        execute conn
          "INSERT INTO meta(key,value) VALUES(?,?) ON CONFLICT(key) DO UPDATE SET value=excluded.value"
          [SQLText "generation", SQLInteger generationSql]
        execute conn
          "INSERT INTO meta(key,value) VALUES(?,?) ON CONFLICT(key) DO UPDATE SET value=excluded.value"
          [SQLText "observed_commit_count", SQLInteger observedCountSql]
        targetReachable <-
          if null operationIds
            then pure (Set.singleton targetRevision)
            else readTargetReachability >>= either (throwIO . GitErrorSome) pure
        pure (ProvenanceUpdate
          { databasePath = dbPath
          , fingerprint = fp
          , generation = generation
          , commitsScanned = 0
          , observedCommitCount = observedCount
           , changed = False
           , newOperations = 0
           , targetReachableCommits = targetReachable
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

        -- Classification completeness is operation-specific.  Every observed
        -- commit must be considered against every registered operation, not
        -- only operations first registered during this maintenance pass.
        registeredNow <- query_ conn "SELECT op_id FROM registered_operation ORDER BY op_id"
          :: IO [Only Text]
        let classificationOps = map fromOnly registeredNow

        -- A requested operation can be registered after all refs have already
        -- been observed.  When its exact target coverage is absent, replay the
        -- complete indexed candidate union for that operation.  The overlay is
        -- repository-wide, so this deliberately includes placements on other
        -- refs; ancestry is reserved for target certification below.
        recoveryRowsResult <-
          if null recoveryUncoveredOperations
            then pure (Right [])
            else do
              let placeholders = Text.intercalate "," (replicate (length recoveryUncoveredOperations) "?")
              try @SomeException
                ( do
                    additions <- query conn
                      (asQuery
                        ( "SELECT DISTINCT addition.commit_oid "
                            <> "FROM managed_path_addition AS addition "
                            <> "JOIN registered_object AS object "
                            <> "ON object.path=addition.path "
                            <> "WHERE object.op_id IN (" <> placeholders <> ")"
                        )
                      )
                      (map SQLText recoveryUncoveredOperations)
                      :: IO [Only Text]
                    existing <- query conn
                      (asQuery
                        ( "SELECT DISTINCT commit_oid FROM operation_commit "
                            <> "WHERE op_id IN (" <> placeholders <> ")"
                        )
                      )
                      (map SQLText recoveryUncoveredOperations)
                      :: IO [Only Text]
                    pure (additions <> existing)
                )
        recoveryRows <- case recoveryRowsResult of
          Left problem -> throwIO problem
          Right rows -> pure rows
        -- Trailer-only placements are indexed observations too.  Replaying
        -- them avoids a history walk while covering a merge whose sealed files
        -- live solely on a non-first parent and whose membership is declared
        -- by an exact ADRAI-Op trailer.
        trailerRows <-
          if null recoveryUncoveredOperations
            then pure []
            else query_ conn "SELECT commit_oid,message FROM commit_observation"
              :: IO [(Text, Text)]
        let recoveryCandidates = deduplicateOids
              ( [ oid
                | Only rawOid <- recoveryRows
                , Right oid <- [mkGitOid rawOid]
                ]
                  <> [ oid
                     | (rawOid, message) <- trailerRows
                     , not (null (findAllOpTrailers message recoveryUncoveredOperations))
                     , Right oid <- [mkGitOid rawOid]
                     ]
              )
        let classificationCommits = deduplicateOids
              (newCommits <> [targetRevision] <> maybe [] id witnessedRange <> recoveryCandidates)
        observeWorklist classificationCommits

        -- Store commit observations + managed path additions
        when (not (null classificationCommits)) $ do
          storeResult <- storeNewCommits repo conn classificationCommits
          case storeResult of
            Right _ -> pure ()
            Left e  -> throwIO e

        -- Find candidate commits per operation
        candidatesResult <- candidateCommits conn classificationCommits classificationOps
        -- Classify candidates
        case (candidatesResult, newOpsResult) of
          (Right candidates, Right _) -> do
            processed <- processCandidates repo conn candidates classificationOps
            case processed of
              Left e -> throwIO e
              Right () -> pure ()
          (Left e, _) -> throwIO e
          (_, Left e) -> throwIO e

        -- Prune before certification: a target proof is published only for
        -- canonical placements that still exist in the object database.
        prunedResult <- pruneUnavailablePlacements repo conn
        prunedCount <- case prunedResult of
          Left e -> throwIO (GitErrorSome e)
          Right n -> pure n

        -- Certify each requested registration only after classification has
        -- produced reachable target evidence.  The certificate binds the
        -- immutable target and registration signature, so a later
        -- registration change cannot reuse an earlier coverage claim.
        certifiedRows <-
          if null operationIds then pure [] else do
            let placeholders = Text.intercalate "," (replicate (length operationIds) "?")
            query conn
              (asQuery ("SELECT op_id,commit_oid FROM operation_commit WHERE op_id IN (" <> placeholders <> ")"))
              (map SQLText operationIds) :: IO [(Text, Text)]
        let certificationCandidates = deduplicateOids
              [ oid | (_, rawOid) <- certifiedRows, Right oid <- [mkGitOid rawOid] ]
        certifiedAncestryResult <- extendTargetAncestryCache
          targetReachability placementAncestry certificationCandidates
        certifiedAncestry <- case certifiedAncestryResult of
          Left problem -> throwIO (GitErrorSome problem)
          Right cache -> pure cache
        registeredAfter <- query_ conn "SELECT op_id,signature FROM registered_operation"
          :: IO [(Text, Text)]
        let registeredAfterSignatures = Map.fromList registeredAfter
        let reachableByOperation = Map.fromListWith (||)
              [ (operationId, Map.lookup oid certifiedAncestry == Just True)
              | (operationId, rawOid) <- certifiedRows
              , Right oid <- [mkGitOid rawOid]
              ]
        -- A target certificate is a proof for this exact registration.  Drop
        -- any stale or duplicate proof before publishing the freshly checked
        -- one; an operation without reachable canonical evidence deliberately
        -- remains uncertified.
        forM_ operationIds $ \operationId ->
          execute conn "DELETE FROM operation_target_coverage WHERE op_id=? AND target_oid=?"
            [SQLText operationId, SQLText (gitOidText targetRevision)]
        forM_ operationIds $ \operationId ->
          when (Map.findWithDefault False operationId reachableByOperation) $
            case Map.lookup operationId registeredAfterSignatures of
              Nothing -> pure ()
              Just signature -> execute conn
                "INSERT OR REPLACE INTO operation_target_coverage(op_id,target_oid,registration_signature) VALUES(?,?,?)"
                [SQLText operationId, SQLText (gitOidText targetRevision), SQLText signature]

        -- Refresh line landings (only if new ops or pruned)
        let opsForRefresh = case newOpsResult of
              Right ops ->
                if prunedCount > 0
                  then []
                  else deduplicateTexts (ops <> reseededChangedOperations <> recoveryUncoveredOperations)
              Left _ -> []

        lineChangedResult <- refreshLineLandings repo conn decisionsPath connectionsPath
          logicalLines opsForRefresh maybeParsedDocs groupsLoader
        let lineChanged = case lineChangedResult of
              Right b -> b
              Left _  -> False

        -- Update ref observation table
        execute_ conn "DELETE FROM ref_observation"
        forM_ refs' $ \refObs ->
          execute conn "INSERT INTO ref_observation VALUES(?,?,?)"
            [ SQLText (refObservationRefName refObs)
            , SQLText (refObservationTipOid refObs)
            , SQLText (refObservationObjectType refObs)
            ]

        -- Update observation root table
        execute_ conn "DELETE FROM observation_root"
        let roots' = snd refsTuple
        forM_ roots' $ \root ->
          execute conn "INSERT INTO observation_root VALUES(?,?,?)"
            [ SQLText (observationRootKind root)
            , SQLText (observationRootName root)
            , SQLText (gitOidText (observationRootCommitOid root))
            ]

        -- Update meta table
        observedCountAfterRefresh <- readObservedCount
        let observedCountAfterRefreshSql :: Int64
            observedCountAfterRefreshSql = fromIntegral observedCountAfterRefresh
        execute conn
          "INSERT INTO meta(key,value) VALUES(?,?) ON CONFLICT(key) DO UPDATE SET value=excluded.value"
          [SQLText "schema", SQLText overlaySchemaVersion]
        execute conn
          "INSERT INTO meta(key,value) VALUES(?,?) ON CONFLICT(key) DO UPDATE SET value=excluded.value"
          [SQLText "observation_fingerprint", SQLText fingerprintVal]
        execute conn
          "INSERT INTO meta(key,value) VALUES(?,?) ON CONFLICT(key) DO UPDATE SET value=excluded.value"
          [SQLText "generation", SQLInteger generationSql]
        execute conn
          "INSERT INTO meta(key,value) VALUES(?,?) ON CONFLICT(key) DO UPDATE SET value=excluded.value"
          [SQLText "observed_commit_count", SQLInteger observedCountAfterRefreshSql]

        let newOpsCount = case newOpsResult of
              Right ops -> length ops
              Left _ -> 0

        targetReachable <-
          if null operationIds
            then pure (Set.singleton targetRevision)
            else readTargetReachability >>= either (throwIO . GitErrorSome) pure
        pure (ProvenanceUpdate
          { databasePath = dbPath
          , fingerprint = fp
          , generation = generation
          -- Replaying already-observed indexed candidates repairs placement
          -- evidence but is not a history scan.
          , commitsScanned = length (deduplicateOids newCommits)
          , observedCommitCount = observedCountAfterRefresh
           , changed = lineChanged || not (null classificationCommits) || newOpsCount > 0 || not targetReady
           , newOperations = newOpsCount
           , targetReachableCommits = targetReachable
          })

  pure result

-- | Populate an empty provenance overlay from the immutable semantic cache
-- without reading or parsing managed source files.  The cold database already
-- stores the operation member identity, semantic digest, and source blob OID
-- needed for the same registration signature used by normal parsing.
-- A same-ID semantic change replaces stale overlay registration evidence before
-- recovery.  Retaining an old signature here would let an ancestor witness
-- suppress the replay required by the new member set.
seedRegisteredOperationsFromSemanticCache :: FilePath -> Connection -> IO ([Text], [Text])
seedRegisteredOperationsFromSemanticCache = seedRegisteredOperationsFromSemanticCacheWithQueryObserver (pure ())

-- | The observer counts cache/overlay reads only.  Per-row registration DML
-- deliberately remains prepared execution work; this seam protects against a
-- regression to an operation-by-operation SELECT/prepare fan-out.
seedRegisteredOperationsFromSemanticCacheWithQueryObserver :: IO () -> FilePath -> Connection -> IO ([Text], [Text])
seedRegisteredOperationsFromSemanticCacheWithQueryObserver beforeQuery semanticDatabase overlay =
  bracket (open semanticDatabase) close $ \cache -> do
    beforeQuery
    schemas <- query_ cache "SELECT value FROM meta WHERE key='schema'" :: IO [Only Text]
    unless (schemas == [Only "adrai-cache/3"])
      (ioError (userError "cache provenance seeding requires adrai-cache/3"))
    beforeQuery
    operationRows <- query_ cache
      "SELECT operation_id,basis_oid FROM operation ORDER BY operation_id"
      :: IO [(Text, Text)]
    beforeQuery
    memberRows <- query_ cache
      "SELECT operation_id,object_id,path,blob_oid,semantic_digest FROM operation_member ORDER BY operation_id,path,object_id"
      :: IO [(Text, Text, Text, Text, Text)]
    beforeQuery
    existingRows <- query_ overlay
      "SELECT op_id,signature FROM registered_operation ORDER BY op_id"
      :: IO [(Text, Text)]
    let membersByOperation = Map.fromListWith (<>)
          [ (operationId, [(objectId, path, blobOid, digest)])
          | (operationId, objectId, path, blobOid, digest) <- memberRows
          ]
        existingByOperation = Map.fromListWith (<>)
          [ (operationId, [signature]) | (operationId, signature) <- existingRows ]
    changedRef <- newIORef []
    forM_ operationRows $ \(operationId, basisOid) -> do
      let existing = Map.findWithDefault [] operationId existingByOperation
          members = Map.findWithDefault [] operationId membersByOperation
      let signature = semanticCacheOperationSignature members
          adrId = firstAdrId [objectId | (objectId, _, _, _) <- members]
      case existing of
        [] -> do
          execute overlay
            "INSERT INTO registered_operation(op_id,adr_id,basis_oid,signature) VALUES(?,?,?,?)"
            [ SQLText operationId
            , maybe SQLNull SQLText adrId
            , SQLText basisOid
            , SQLText signature
            ]
          forM_ members $ \(objectId, path, blobOid, _) ->
            execute overlay
              "INSERT INTO registered_object(op_id,object_id,path,blob_oid) VALUES(?,?,?,?)"
              [SQLText operationId, SQLText objectId, SQLText path, SQLText blobOid]
        [oldSignature] | oldSignature /= signature -> do
          modifyIORef' changedRef (operationId :)
          execute overlay "DELETE FROM operation_target_coverage WHERE op_id=?" (Only operationId)
          execute overlay "DELETE FROM operation_commit WHERE op_id=?" (Only operationId)
          execute overlay "DELETE FROM registered_object WHERE op_id=?" (Only operationId)
          execute overlay "DELETE FROM line_landing WHERE op_id=?" (Only operationId)
          execute overlay "DELETE FROM provenance_issue WHERE op_id=?" (Only operationId)
          execute overlay "UPDATE registered_operation SET adr_id=?,basis_oid=?,signature=? WHERE op_id=?"
            [ maybe SQLNull SQLText adrId, SQLText basisOid, SQLText signature, SQLText operationId ]
          forM_ members $ \(objectId, path, blobOid, _) ->
            execute overlay
              "INSERT INTO registered_object(op_id,object_id,path,blob_oid) VALUES(?,?,?,?)"
              [SQLText operationId, SQLText objectId, SQLText path, SQLText blobOid]
        [_] -> pure ()
        _ -> ioError (userError "duplicate registered operation rows")
    changed <- readIORef changedRef
    pure (map fst operationRows, deduplicateTexts changed)
  where
    firstAdrId = foldr choose Nothing
    choose objectId rest
      | Text.isPrefixOf "A" objectId = Just objectId
      | Text.isPrefixOf "R" objectId = Just ("A" <> Text.drop 1 objectId)
      | otherwise = rest
    semanticCacheOperationSignature = digestToHex . operationMemberSignature

deduplicateTexts :: [Text] -> [Text]
deduplicateTexts = go Set.empty
  where
    go _ [] = []
    go seen (value : rest)
      | Set.member value seen = go seen rest
      | otherwise = value : go (Set.insert value seen) rest

-- | Group parsed managed documents by operation ID.
groupParsedDocs :: [ParsedManagedDocument] -> Map Text [ParsedManagedDocument]
groupParsedDocs docs = Map.fromListWith (++)
  [ (operationIdText (provenanceOperationId (parsedManagedCapsule doc)), [doc])
    | doc <- docs
  ]

-- ============================================================
-- Overlay rows query
-- ============================================================

-- | Read the cache as evidence for an immutable, caller-resolved commit.
--
-- Cache maintenance deliberately observes current refs and reflogs.  This
-- reader does neither: the only graph authority is @target@, and all commit,
-- landing, ref, and root rows are selected only when their OID belongs to
-- that graph.  In particular, it is safe to keep a shared cache while callers
-- render an older revision.
readProvenanceEvidenceAt
  :: Repository
  -> FilePath
  -> GitOid
  -> [Text]
  -> Text
  -> IO (Either ProvenanceEvidenceError ProvenanceEvidence)
readProvenanceEvidenceAt repository overlayPath target requestedOperations requestedConfig
  = readProvenanceEvidenceAtWith repository overlayPath target requestedOperations requestedConfig (pure ())

-- | Testable form of 'readProvenanceEvidenceAt'.  The hook runs after the
-- read transaction has established its snapshot with a schema query, but
-- before any evidence-table query.  Production callers use
-- 'readProvenanceEvidenceAt'; the hook makes cancellation and release
-- behavior observable without changing the production authority.
readProvenanceEvidenceAtWith
  :: Repository
  -> FilePath
  -> GitOid
  -> [Text]
  -> Text
  -> IO ()
  -> IO (Either ProvenanceEvidenceError ProvenanceEvidence)
readProvenanceEvidenceAtWith repository overlayPath target requestedOperations requestedConfig afterOpen
  | null requestedOperations =
      pure (Right (ProvenanceEvidence target Nothing [] [] [] []))
  | otherwise = do
      graphResult <- reachableCommitGraphAt repository target
      case graphResult of
        Left gitError -> pure (Left (ProvenanceEvidenceGitError gitError))
        Right graph -> do
          let reachable = Set.fromList
                (gitOidText target : [gitOidText (gitCommitNodeOid node) | node <- graph])
          databaseResult <- captureSynchronous $
            bracket (openReadOnly overlayPath) close $ \connection ->
              withTransaction connection $ do
                -- Establish the read snapshot before invoking the narrow
                -- test hook.  The transaction then keeps every evidence
                -- table coherent even when overlay maintenance commits
                -- concurrently.
                void (query_ connection "SELECT 1 FROM sqlite_schema LIMIT 1" :: IO [Only Int])
                afterOpen
                readEvidenceRows connection reachable
          pure $ case databaseResult of
            Left databaseError -> Left databaseError
            Right evidence -> evidence
  where
    readEvidenceRows connection reachable = do
      let placeholders = Text.intercalate "," (replicate (length requestedOperations) "?")
          operationParams = map SQLText requestedOperations
          requested = Set.fromList requestedOperations
          operationQuery selectColumns orderBy = asQuery (selectColumns <> " WHERE op_id IN (" <> placeholders <> ")" <> orderBy)

      registrationRaw <- query connection
        (operationQuery "SELECT op_id,adr_id,basis_oid,signature FROM registered_operation" " ORDER BY op_id,adr_id,basis_oid,signature")
        operationParams :: IO [(Text, Maybe Text, Text, Text)]
      objectRaw <- query connection
        (operationQuery "SELECT op_id,object_id,path,blob_oid FROM registered_object" " ORDER BY op_id,object_id,path,blob_oid")
        operationParams :: IO [(Text, Text, Text, Text)]
      placementRaw <- query connection
        (operationQuery "SELECT op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json FROM operation_commit" " ORDER BY op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json")
        operationParams :: IO [(Text, Text, Text, Integer, Integer, Text, Text)]
      coverageRaw <- query connection
        (asQuery ("SELECT op_id,registration_signature FROM operation_target_coverage WHERE target_oid=? AND op_id IN (" <> placeholders <> ") ORDER BY op_id,registration_signature"))
        (SQLText (gitOidText target) : operationParams) :: IO [(Text, Text)]
      landingRaw <- query connection
        (asQuery ("SELECT op_id,line_id,ref_name,commit_oid,complete FROM line_landing WHERE config_key=? AND op_id IN (" <> placeholders <> ") ORDER BY op_id,line_id,ref_name,commit_oid,complete"))
        (SQLText requestedConfig : operationParams) :: IO [(Text, Text, Text, Text, Integer)]
      issueRaw <- query connection
        (operationQuery "SELECT op_id,severity,code,adr_id,object_id,path,message FROM provenance_issue" " ORDER BY op_id,severity,code,adr_id,object_id,path,message")
        operationParams :: IO [(Maybe Text, Text, Text, Maybe Text, Maybe Text, Maybe Text, Text)]
      configRaw <- query connection
        "SELECT config_key,config_json FROM line_config WHERE config_key=? ORDER BY config_key,config_json"
        [SQLText requestedConfig] :: IO [(Text, Text)]
      lineRefRaw <- query connection
        "SELECT config_key,ref_name,tip_oid FROM line_ref_state WHERE config_key=? ORDER BY config_key,ref_name,tip_oid"
        [SQLText requestedConfig] :: IO [(Text, Text, Text)]
      refRaw <- query_ connection
        "SELECT ref_name,tip_oid,object_type FROM ref_observation ORDER BY ref_name,tip_oid,object_type"
        :: IO [(Text, Text, Text)]
      rootRaw <- query_ connection
        "SELECT root_kind,root_name,commit_oid FROM observation_root ORDER BY root_kind,root_name,commit_oid"
        :: IO [(Text, Text, Text)]

      pure $ do
        config <- case configRaw of
          [(configKey', configJson)] -> Right (LineConfigRow configKey' configJson)
          [] -> Left (ProvenanceEvidenceMissingConfig requestedConfig)
          _ -> Left (ProvenanceEvidenceDatabaseError "duplicate line_config rows")
        registrations <- traverse registrationRow registrationRaw
        objects <- traverse objectRow objectRaw
        allPlacements <- traverse placementRow placementRaw
        allLandings <- traverse landingRow landingRaw
        allLineRefs <- traverse lineRefRow lineRefRaw
        allRefs <- traverse refRow refRaw
        allRoots <- traverse rootRow rootRaw
        let placements = filter ((`Set.member` reachable) . gitOidText . operationCommitRowCommitOid) allPlacements
            landings = filter ((`Set.member` reachable) . gitOidText . lineLandingRowCommitOid) allLandings
            lineRefs = filter ((`Set.member` reachable) . gitOidText . lineRefStateRowTipOid) allLineRefs
            refs = filter ((`Set.member` reachable) . gitOidText . refObservationRowTipOid) allRefs
            roots = filter ((`Set.member` reachable) . gitOidText . observationRootRowCommitOid) allRoots
        let issues =
              [ ProvenanceIssueRow severity code adrId objectId path message opId
                | (opId, severity, code, adrId, objectId, path, message) <- issueRaw
              ]
        operations <- traverse
          (operationEvidence registrations objects placements landings issues coverageRaw)
          requestedOperations
        -- This guard documents that caller ownership is exact even if a
        -- malformed database managed to manufacture an extra row.
        if all ((`Set.member` requested) . registeredOperationRowOpId . provenanceEvidenceRegistration) operations
          then Right (ProvenanceEvidence target (Just config) operations lineRefs refs roots)
          else Left (ProvenanceEvidenceDatabaseError "unrequested operation evidence")

    operationEvidence registrations objects placements landings issues coverage opId = do
      registration <- case filter ((== opId) . registeredOperationRowOpId) registrations of
        [] -> Left (ProvenanceEvidenceMissingRegistration opId)
        [row] -> Right row
        _ -> Left (ProvenanceEvidenceDuplicateRegistration opId)
      let members = filter ((== opId) . registeredObjectRowOpId) objects
          certificates = [signature | (certificateOpId, signature) <- coverage, certificateOpId == opId]
      if null members
        then Left (ProvenanceEvidenceMissingObjects opId)
        else if certificates /= [registeredOperationRowSignature registration]
          then Left (ProvenanceEvidenceMissingTargetPlacement opId)
        else if null (filter ((== opId) . operationCommitRowOpId) placements)
          then Left (ProvenanceEvidenceMissingTargetPlacement opId)
          else Right
          (ProvenanceOperationEvidence
            registration
            members
            (filter ((== opId) . operationCommitRowOpId) placements)
            (filter ((== opId) . lineLandingRowOpId) landings)
            (filter ((== Just opId) . provenanceIssueRowOpId) issues))

    registrationRow (opId, adrId, basisOid, signature) =
      RegisteredOperationRow opId adrId <$> rowOid "registered_operation.basis_oid" basisOid <*> pure signature
    objectRow (opId, objectId, path, blobOid) =
      RegisteredObjectRow opId objectId path <$> rowOid "registered_object.blob_oid" blobOid
    placementRow (opId, commitOid, classification, authored, committed, subject, parents) =
      OperationCommitRow opId <$> rowOid "operation_commit.commit_oid" commitOid <*> pure classification <*> pure authored <*> pure committed <*> pure subject <*> pure parents
    landingRow (opId, lineId, refName, commitOid, complete) =
      LineLandingRow requestedConfig opId lineId refName <$> rowOid "line_landing.commit_oid" commitOid <*> pure complete
    lineRefRow (configKey', refName, tipOid) =
      LineRefStateRow configKey' refName <$> rowOid "line_ref_state.tip_oid" tipOid
    refRow (refName, tipOid, objectType) =
      RefObservationRow refName <$> rowOid "ref_observation.tip_oid" tipOid <*> pure objectType
    rootRow (rootKind, rootName, commitOid) =
      ObservationRootRow rootKind rootName <$> rowOid "observation_root.commit_oid" commitOid
    rowOid field raw = case mkGitOid raw of
      Left _ -> Left (ProvenanceEvidenceInvalidOid field raw)
      Right oid -> Right oid

-- | Open SQLite through its URI read-only mode.  Unlike 'open' on a plain
-- filename this cannot create a missing database or journal sidecars.  The
-- The read-only URI is deliberately /not/ immutable: an immutable handle can
-- ignore a concurrent WAL and therefore observe a physically inconsistent
-- cache.  A normal read-only transaction gives a coherent snapshot without
-- creating the database or any journal sidecar.
openReadOnly :: FilePath -> IO Connection
openReadOnly path = makeAbsolute path >>= open . sqliteUri "ro"

-- | Open an existing SQLite database through URI @mode=rw@.  This never
-- creates a database; callers use it only for SQLite checks that FTS5 cannot
-- perform on a read-only handle, and must enable connection-local query-only
-- mode before reading application rows.
openReadWriteExisting :: FilePath -> IO Connection
openReadWriteExisting path = makeAbsolute path >>= open . sqliteUri "rw"

sqliteUri :: String -> FilePath -> String
sqliteUri mode path = prefix <> concatMap escapeByte (BS.unpack utf8Path) <> "?mode=" <> mode
  where
    normalized = map replaceBackslash path
    utf8Path = TextEncoding.encodeUtf8 (Text.pack normalized)
    prefix = case normalized of
      '/' : _ -> "file://"
      _ -> "file:///"
    replaceBackslash '\\' = '/'
    replaceBackslash character = character
    escapeByte byte
      | asciiSafe byte = [toEnum (fromIntegral byte)]
      | otherwise = ['%', hexDigit (byte `shiftR` 4), hexDigit (byte .&. 0x0f)]
    asciiSafe byte =
      (byte >= 0x41 && byte <= 0x5a)
        || (byte >= 0x61 && byte <= 0x7a)
        || (byte >= 0x30 && byte <= 0x39)
        || byte `elem` [0x2d, 0x2e, 0x2f, 0x3a, 0x5f, 0x7e]
    hexDigit nibble
      | nibble < 10 = toEnum (fromEnum '0' + fromIntegral nibble)
      | otherwise = toEnum (fromEnum 'A' + fromIntegral nibble - 10)

-- | Catch only synchronous failures.  'bracket' masks acquisition/release, and
-- asynchronous cancellation is rethrown rather than made observable as a
-- normal cache failure.
captureSynchronous :: IO a -> IO (Either ProvenanceEvidenceError a)
captureSynchronous action = do
  result <- try @SomeException action
  case result of
    Right value -> pure (Right value)
    Left exception -> case fromException exception of
      Just async -> throwIO (async :: SomeAsyncException)
      Nothing -> pure (Left (ProvenanceEvidenceDatabaseError (Text.pack (show exception))))

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
  -> IO (Either SomeException ([(Text,Text,Text,Integer,Integer,Text,Text)],
                           [(Text,Text,Text,Text,Integer)],
                           [(Text,Text,Maybe Text,Maybe Text,Maybe Text,Text)]))
overlayRowsForOperations overlayPath opIds configKey' = do
  result <- try @SomeException $ do
    if null opIds
      then pure ([], [], [])
      else bracket (openReadOnly overlayPath) close $ \conn -> do
        let placeholders = Text.intercalate "," (replicate (length opIds) "?")
            params = map SQLText opIds

        -- Operation placements
        placements <- query conn
          (asQuery ("SELECT op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json "
           <> "FROM operation_commit WHERE op_id IN (" <> placeholders <> ") ORDER BY op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json"))
          params :: IO [(Text, Text, Text, Integer, Integer, Text, Text)]

        -- Line landings
        landings <- query conn
          (asQuery ("SELECT op_id,line_id,ref_name,commit_oid,complete "
           <> "FROM line_landing WHERE config_key=? AND op_id IN (" <> placeholders <> ") ORDER BY op_id,line_id,ref_name,commit_oid,complete"))
          (SQLText configKey' : params) :: IO [(Text, Text, Text, Text, Integer)]

        -- Issues
        issues <- query conn
          (asQuery ("SELECT severity,code,adr_id,object_id,path,message "
           <> "FROM provenance_issue WHERE op_id IN (" <> placeholders <> ") ORDER BY severity,code,adr_id,object_id,path,message"))
          params :: IO [(Text, Text, Maybe Text, Maybe Text, Maybe Text, Text)]

        pure (placements, landings, issues)

  case result of
    Left exception -> case fromException exception of
      Just async -> throwIO (async :: SomeAsyncException)
      Nothing -> pure (Left exception)
    Right rows -> pure (Right rows)

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

-- | Convert a 'Digest' to a zero-padded 64-character SHA-256 hex string.
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
