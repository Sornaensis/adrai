{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

-- | Provenance overlay core classification module.
--
-- Mirrors the Python prototype at
-- ADRAI_1_Source/adrai_core/provenance_cache.py functions:
--
-- * @_operation_signature@
-- * @_registered_operations@
-- * @_register_groups@
-- * @_store_new_commits@
-- * @_load_commit_rows@
-- * @_candidate_commits@
-- * @_process_candidates@
-- * @_prune_unavailable_placements@
-- * @_record_issue@
-- * @_issue_key@
module Adrai.Provenance.Classification
  ( operationSignature
  , operationMemberSignature
  , ParsedManagedDocument (..)
  , RegisteredOperationData (..)
  , RegisteredObjectData (..)
  , registeredOperations
  , registerOperationGroups
  , StoredCommitData (..)
  , storeNewCommits
  , loadCommitRows
  , loadCommitRowsWithQueryObserver
  , candidateCommits
  , candidateCommitsWithQueryObserver
  , findAllOpTrailers
  , processCandidates
  , processCandidatesWithQueryObserver
  , basisObjectRequestCount
  , groupTreeObservationRequests
  , treeObservationRequestCount
  , decodeExactBlobTreeEntry
  , canonicalParentsJson
  , pruneUnavailablePlacements
  , recordIssue
  , issueKey
  )
where

import Adrai.Git
  ( GitError,
    GitOid(..),
    GitObjectType (GitBlobObject, GitCommitObject),
    GitObjectInfo (..),
    Repository (..),
    batchObjectInfo,
    decodeGitTreeOutput,
    gitTreeObjectType,
    gitTreeOid,
    gitTreePath,
    gitOidText,
    lookupTreeObjectInfoAtRevisions,
    objectBatchRequestCount,
    treePathBatchSessionCount,
    treePathRevisionLsTreeChildCount,
  )
import Adrai.Provenance
  ( ProvenanceCapsule,
    mkGitOid,
    provenanceBasis,
    sha256Digest,
  )
import Adrai.Provenance.Discovery (addedPathsForCommits, commitLogSnapshotForOids)
import Adrai.Sqlite (asQuery)
import Adrai.Provenance.Overlay
  ( CommitObservation (..),
    OperationClassification (..),
    commitObservationMessage,
    commitObservationOid,
    commitObservationParents,
    commitObservationSubject,
    operationClassificationValue,
  )
import Adrai.Types
  ( Digest(..),
    RepoPath (..),
    mkRepoPath,
    repoPathText,
  )
import Control.Exception (SomeException, throwIO, toException, try)
import Control.Monad (forM_, when)
import Data.Bits ((.&.), shiftR)
import Data.List (sort, sortBy)
import qualified Data.Aeson as Aeson
import Data.Ord (comparing)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text (Text)
import qualified Data.Vector as Vector
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Database.SQLite.Simple
  ( Connection,
    Only (..),
    SQLData (SQLNull, SQLText, SQLInteger),
    execute,
    query,
    query_,
  )
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL

-- | Parsed managed document, minimal view for provenance classification.
data ParsedManagedDocument = ParsedManagedDocument
  { parsedDocumentObjectRef  :: !Text
  , parsedManagedPath        :: !RepoPath
  , parsedManagedCapsule     :: !ProvenanceCapsule
  , parsedBlobOid            :: !(Maybe GitOid)
  , parsedSemanticHash       :: !Text
  } deriving (Eq, Show)

_parsedObjectId :: ParsedManagedDocument -> Text
_parsedObjectId = parsedDocumentObjectRef

_parsedSubjectAdrText :: ParsedManagedDocument -> Maybe Text
_parsedSubjectAdrText doc =
  case parsedDocumentObjectRef doc of
    ref | Text.isPrefixOf "A" ref -> Just ref
    ref | Text.isPrefixOf "R" ref -> Just ("A" <> Text.drop 1 ref)
    _                             -> Nothing

-- | SHA-256 digest of a sorted operation member list. Mirrors Python @_operation_signature().
operationSignature :: [ParsedManagedDocument] -> Digest
operationSignature members =
  operationMemberSignature
    [ ( parsedDocumentObjectRef doc,
        repoPathText (parsedManagedPath doc),
        maybe "" gitOidText (parsedBlobOid doc),
        parsedSemanticHash doc
      )
      | doc <- members
    ]

-- | SHA-256 digest of the canonical operation-member registration payload.
-- Both parsed source and immutable cache rows use this one representation so
-- a no-parse seed cannot silently drift from normal registration.
operationMemberSignature :: [(Text, Text, Text, Text)] -> Digest
operationMemberSignature members =
  let payload =
        [ Aeson.Array
            (Vector.fromList
              [ Aeson.String objectId,
                Aeson.String path,
                Aeson.String blobOid,
                Aeson.String semanticDigest
              ])
          | (objectId, path, blobOid, semanticDigest) <- sortBy (comparing (\(objectId, path, _, _) -> (path, objectId))) members
        ]
      json = Aeson.Array (Vector.fromList payload)
      encoded = BSL.toStrict (Aeson.encode json)
  in sha256Digest encoded

-- | Registered operation data.
data RegisteredOperationData = RegisteredOperationData
  { regOpAdrId     :: Maybe Text
  , regOpBasis     :: GitOid
  , regOpSignature :: Text
  , regOpObjects   :: [RegisteredObjectData]
  } deriving (Eq, Show)

data RegisteredObjectData = RegisteredObjectData
  { regObjectObjectId :: Text
  , regObjectPath     :: Text
  , regObjectBlobOid  :: Text
  } deriving (Eq, Show)

-- | Query all registered operations and their objects.
registeredOperations
  :: Connection
  -> IO (Either SomeException (Map Text RegisteredOperationData))
registeredOperations = registeredOperationsWithQueryObserver (pure ())

-- | Internal variant used by the bounded candidate-query seam.  The observer
-- runs immediately before each SQLite read, so callers can assert statement
-- fan-out without treating expected row writes as query work.
registeredOperationsWithQueryObserver
  :: IO ()
  -> Connection
  -> IO (Either SomeException (Map Text RegisteredOperationData))
registeredOperationsWithQueryObserver beforeQuery conn = do
  result <- try @SomeException $ do
    beforeQuery
    ops <- query_ conn "SELECT op_id,adr_id,basis_oid,signature FROM registered_operation"
      :: IO [(Text, Maybe Text, Text, Text)]
    let baseMap = Map.fromList
          [ (oid, RegisteredOperationData
                { regOpAdrId = adrId
                , regOpBasis = GitOid basis
                , regOpSignature = signature
                , regOpObjects = []
                })
          | (oid, adrId, basis, signature) <- ops
          ]
    beforeQuery
    objs <- query_ conn "SELECT op_id,object_id,path,blob_oid FROM registered_object ORDER BY op_id,path"
      :: IO [(Text, Text, Text, Text)]
    let updated = foldl (\m (oid, objId, path, blobOid) ->
          Map.update (\op -> pure op { regOpObjects =
            (regOpObjects op) ++ [RegisteredObjectData objId path blobOid]
          }) oid m) baseMap objs
    pure (Right updated)
  case result of
    Right val -> pure val
    Left e  -> pure (Left e)

-- | Register new operation groups. First registration wins.
registerOperationGroups
  :: Connection
  -> Map Text [ParsedManagedDocument]
  -> IO (Either SomeException [Text])
registerOperationGroups conn groups = do
  result <- try @SomeException $ do
    existing <- query_ conn "SELECT op_id FROM registered_operation"
    let existingOps = Set.fromList [oid | Only oid <- existing]

    let sortedGroups = sortBy (comparing fst) (Map.toList groups)

    let go [] acc = pure (Right (reverse acc))
        go ((opId, members):rest) acc
          | opId `Set.member` existingOps = go rest acc
          | otherwise = case members of
              [] -> go rest acc
              firstDoc:_ -> do
                let sig = operationSignature members
                    basis = provenanceBasis (parsedManagedCapsule firstDoc)
                    adrId = _parsedSubjectAdrText firstDoc
                execute conn
                  "INSERT INTO registered_operation VALUES(?,?,?,?)"
                  [ SQLText opId
                  , maybe SQLNull SQLText adrId
                  , SQLText (gitOidText basis)
                  , SQLText (digestToHex sig)
                  ]
                forM_ members $ \doc ->
                  execute conn
                    "INSERT INTO registered_object VALUES(?,?,?,?)"
                    [ SQLText opId
                    , SQLText (parsedDocumentObjectRef doc)
                    , SQLText (repoPathText (parsedManagedPath doc))
                    , SQLText (maybe "" gitOidText (parsedBlobOid doc))
                    ]
                go rest (opId : acc)
    go sortedGroups []
  case result of
    Right val -> pure val
    Left e  -> pure (Left e)

-- | Stored commit data from commit observation.
data StoredCommitData = StoredCommitData
  { storedCommitParents   :: [Text]
  , storedCommitAuthored  :: Integer
  , storedCommitCommitted :: Integer
  , storedCommitSubject   :: Text
  , storedCommitMessage   :: Text
  } deriving (Eq, Show)

-- | Store new commit observations and managed path additions.
storeNewCommits
  :: Repository
  -> Connection
  -> [GitOid]
  -> IO (Either SomeException (Map GitOid StoredCommitData, Map GitOid (Set Text)))
storeNewCommits repo conn commits = do
  result <- try @SomeException $ do
    if null commits
      then pure (Right (Map.empty, Map.empty))
      else do
        snapshots <- addedPathsForCommits repo commits
        case snapshots of
          Left e -> pure (Left (toException (userError ("managed-path discovery failed: " <> show e))) )
          Right allPaths -> do
            -- We need commit log snapshots too. Use the Discovery module function.
            -- But addedPathsForCommits already returns both. Actually, looking at
            -- the Discovery module, addedPathsForCommits returns Map GitOid (Set Text)
            -- and we also need commit observations. Let me use commitLogSnapshotForOids.
            obs <- commitLogSnapshotForOids repo commits
            case obs of
              Left e -> pure (Left (toException (userError ("commit discovery failed: " <> show e))) )
              Right observations -> do
                let commitMap = Map.fromList
                      [ (commitObservationOid o,
                          StoredCommitData
                            { storedCommitParents = Text.words (commitObservationParents o)
                            , storedCommitAuthored = commitObservationAuthored o
                            , storedCommitCommitted = commitObservationCommitted o
                            , storedCommitSubject = commitObservationSubject o
                            , storedCommitMessage = commitObservationMessage o
                            })
                      | o <- observations
                      ]

                forM_ observations $ \o ->
                  execute conn "INSERT OR IGNORE INTO observed_commit VALUES(?)"
                    [ SQLText (gitOidText (commitObservationOid o)) ]

                forM_ observations $ \o ->
                  execute conn
                    "INSERT OR REPLACE INTO commit_observation VALUES(?,?,?,?,?,?)"
                    [ SQLText (gitOidText (commitObservationOid o))
                    , SQLText (commitObservationParents o)
                    , SQLInteger (fromIntegral (commitObservationAuthored o))
                    , SQLInteger (fromIntegral (commitObservationCommitted o))
                    , SQLText (commitObservationSubject o)
                    , SQLText (commitObservationMessage o)
                    ]

                let managedAdditions =
                      [ (oid, path)
                        | (oid, paths) <- Map.toList allPaths
                        , path <- Set.toList paths
                        , any (`Text.isSuffixOf` (basename path)) managedSuffixes
                      ]
                forM_ managedAdditions $ \(oid, path) ->
                  execute conn
                    "INSERT OR IGNORE INTO managed_path_addition VALUES(?,?)"
                    [ SQLText path, SQLText (gitOidText oid) ]

                pure (Right (commitMap, Map.fromListWith (<>)
                  [ (oid, Set.singleton path)
                    | (oid, path) <- managedAdditions
                  ]))
  case result of
    Right val -> pure val
    Left e  -> pure (Left e)

basename :: Text -> Text
basename = Text.reverse . fst . Text.break (== '/') . Text.reverse

managedSuffixes :: [Text]
managedSuffixes = [".decision.md", ".connection.md"]

-- | Load existing commit observations, filling missing from the Git store.
loadCommitRows
  :: Repository
  -> Connection
  -> [GitOid]
  -> IO (Either SomeException (Map GitOid StoredCommitData))
loadCommitRows = loadCommitRowsWithQueryObserver (pure ())

loadCommitRowsWithQueryObserver
  :: IO ()
  -> Repository
  -> Connection
  -> [GitOid]
  -> IO (Either SomeException (Map GitOid StoredCommitData))
loadCommitRowsWithQueryObserver beforeQuery repo conn oids = do
  result <- try @SomeException $ do
    let unique = dedupList oids
    if null unique
      then pure (Right Map.empty)
      else do
        let placeholders = Text.intercalate "," (replicate (length unique) "?")
            params = map (\oid -> SQLText (gitOidText oid)) unique
        beforeQuery
        rows <- query conn
          (asQuery ("SELECT commit_oid,parents_json,authored_s,committed_s,subject,message "
           <> "FROM commit_observation WHERE commit_oid IN (" <> placeholders <> ")"))
          params
          :: IO [(Text, Text, Integer, Integer, Text, Text)]

        let existing = Map.fromList
              [ (oid,
                  StoredCommitData
                    { storedCommitParents = Text.words parents
                    , storedCommitAuthored = authored
                    , storedCommitCommitted = committed
                    , storedCommitSubject = subject
                    , storedCommitMessage = message
                    })
              | (oidText, parents, authored, committed, subject, message) <- rows
              , let oid = GitOid oidText
              ]

        let missingOids = filter (`notElem` Map.keys existing) unique
        if null missingOids
          then pure (Right existing)
          else do
            -- A missing observation is repaired by one bounded Git discovery
            -- pass.  Recursing here previously retried the identical database
            -- miss forever and could turn a missing object into an apparent
            -- empty successful classification.
            stored <- storeNewCommits repo conn missingOids
            case stored of
              Left e -> pure (Left e)
              Right _ -> do
                -- Observe immediately before the repair-path read as well as
                -- before the initial lookup: callers use this hook to count
                -- every SQLite authority boundary.
                beforeQuery
                repaired <- query conn
                  (asQuery ("SELECT commit_oid,parents_json,authored_s,committed_s,subject,message "
                    <> "FROM commit_observation WHERE commit_oid IN (" <> Text.intercalate "," (replicate (length missingOids) "?") <> ")"))
                  (map (SQLText . gitOidText) missingOids)
                  :: IO [(Text, Text, Integer, Integer, Text, Text)]
                let repairedMap = Map.fromList
                      [ (GitOid oid,
                          StoredCommitData (Text.words parents) authored committed subject message)
                      | (oid, parents, authored, committed, subject, message) <- repaired
                      ]
                    stillMissing = filter (`Map.notMember` repairedMap) missingOids
                if null stillMissing
                  then pure (Right (Map.union existing repairedMap))
                  else throwIO (userError ("missing commit observation after Git discovery: " <> show stillMissing))
  case result of
    Right val -> pure val
    Left e  -> pure (Left e)

dedupList :: (Ord a) => [a] -> [a]
dedupList = go Set.empty
  where
    go _ [] = []
    go seen (x:xs)
      | x `Set.member` seen = go seen xs
      | otherwise = x : go (Set.insert x seen) xs

-- | Find candidate commits for each operation.
candidateCommits
  :: Connection
  -> [GitOid]
  -> [Text]
  -> IO (Either SomeException (Map Text (Set GitOid)))
candidateCommits = candidateCommitsWithQueryObserver (pure ())

-- | Candidate construction performs one grouped read for registrations and
-- two grouped reads for the supplied commit set.  The observer is an inert
-- test seam that counts those reads; production uses 'candidateCommits'.
candidateCommitsWithQueryObserver
  :: IO ()
  -> Connection
  -> [GitOid]
  -> [Text]
  -> IO (Either SomeException (Map Text (Set GitOid)))
candidateCommitsWithQueryObserver beforeQuery conn newCommits newOpIds = do
  result <- try @SomeException $ do
    operations <- registeredOperationsWithQueryObserver beforeQuery conn
    case operations of
      Left e -> pure (Left e)
      Right ops -> do
        let pathToOps :: Map Text (Set Text)
            pathToOps = Map.fromListWith Set.union
              [ (objPath, Set.singleton opId)
              | (opId, opData) <- Map.toList ops
              , obj <- regOpObjects opData
              , let objPath = regObjectPath obj
              ]

        let candidates :: Map Text (Set GitOid)
            candidates = Map.fromList [(opId, Set.empty) | opId <- Map.keys ops]

        if null newCommits
          then pure (Right candidates)
          else do
            let placeholders = Text.intercalate "," (replicate (length newCommits) "?")
                candidateParams = map (SQLText . gitOidText) newCommits
            beforeQuery
            pathCommits <- query conn
              (asQuery ("SELECT path,commit_oid FROM managed_path_addition "
               <> "WHERE commit_oid IN (" <> placeholders <> ")"))
              candidateParams
              :: IO [(Text, Text)]

            beforeQuery
            msgCommits <- query conn
              (asQuery ("SELECT commit_oid,message FROM commit_observation "
               <> "WHERE commit_oid IN (" <> placeholders <> ")"))
              candidateParams
              :: IO [(Text, Text)]

            let pathCandidates = foldl (\m (path, commitOid) ->
                  foldl (\m' opId ->
                    Map.alter (Just . maybe (Set.singleton (GitOid commitOid)) (Set.insert (GitOid commitOid)))
                      opId m')
                    m
                    (Set.toList (Map.findWithDefault Set.empty path pathToOps)))
                  candidates
                  pathCommits

                trailerCandidates = foldl (\m (commitOid, msg) ->
                  foldl (\m' opId ->
                    Map.alter (Just . maybe (Set.singleton (GitOid commitOid)) (Set.insert (GitOid commitOid)))
                      opId m')
                    m
                     (findAllOpTrailers msg (Map.keys ops)))
                  pathCandidates
                  msgCommits
            let pathsByCommit = Map.fromListWith Set.union
                  [ (GitOid commitOid, Set.singleton path) | (path, commitOid) <- pathCommits ]
                finalCandidates = foldl (\m opId ->
                  case Map.lookup opId ops of
                    Nothing -> m
                    Just opData ->
                      let opPaths = Set.fromList [regObjectPath obj | obj <- regOpObjects opData]
                          qualifying = Set.fromList
                            [ commitOid
                            | (commitOid, paths) <- Map.toList pathsByCommit
                            , opPaths `Set.isSubsetOf` paths
                            ]
                      in Map.insertWith Set.union opId qualifying m)
                  trailerCandidates newOpIds

            pure (Right finalCandidates)
  case result of
    Right val -> pure val
    Left e  -> pure (Left e)

findAllOpTrailers :: Text -> [Text] -> [Text]
findAllOpTrailers message existingOps =
  let matches =
        [ stripped
          | line <- Text.lines message
          , let trimmed = Text.strip line
          , Just opId <- [Text.stripPrefix "ADRAI-Op:" trimmed]
          , let stripped = Text.strip opId
          , not (Text.null stripped)
          , stripped `elem` existingOps
        ]
  in nubList matches
  where
    nubList :: (Ord a) => [a] -> [a]
    nubList = go Set.empty
      where
        go _ [] = []
        go seen (x:xs)
          | x `Set.member` seen = go seen xs
          | otherwise = x : go (Set.insert x seen) xs

-- | Core classification logic.
processCandidates
  :: Repository
  -> Connection
  -> Map Text (Set GitOid)
  -> [Text]
  -> IO (Either SomeException ())
processCandidates = processCandidatesWithQueryObserver (pure ())

-- | Testable form of 'processCandidates'.  The observer runs before each
-- SQLite read; production delegates with a no-op observer.  This keeps the
-- grouped-plan contract measurable without changing reconciliation semantics.
processCandidatesWithQueryObserver
  :: IO ()
  -> Repository
  -> Connection
  -> Map Text (Set GitOid)
  -> [Text]
  -> IO (Either SomeException ())
processCandidatesWithQueryObserver beforeQuery repo conn candidates newOpIds = do
  result <- try @SomeException $ do
    ops <- registeredOperationsWithQueryObserver beforeQuery conn
    case ops of
      Left e -> pure (Left e)
      Right operations -> do
        let allCommits = dedupList
              [ oid
              | commits <- Map.elems candidates
              , oid <- Set.toList commits
              ]
        let allNewOps = dedupList newOpIds

        commitRows <- loadCommitRowsWithQueryObserver beforeQuery repo conn allCommits
        case commitRows of
          Left e -> pure (Left e)
          Right rows -> do
            -- All newly registered operation bases are one finite object
            -- observation.  A fresh overlay formerly opened one cat-file
            -- process per operation here, which made a cold compile grow with
            -- the number of managed documents before any tree classification
            -- even started.
            let bases =
                  [ regOpBasis opData
                  | opId <- allNewOps
                  , Just opData <- [Map.lookup opId operations]
                  ]
            basisInfo <- batchObjectInfo repo bases
            observedTrees <- observeCandidateTrees repo operations candidates rows

            -- Check basis commits for new operations from that shared batch.
            forM_ (zip allNewOps [Map.lookup op operations | op <- allNewOps]) $ \(opId, maybeOpData) ->
              case maybeOpData of
                Nothing -> pure ()
                Just opData -> do
                  let basis = regOpBasis opData
                      basisText = gitOidText basis
                      unavailableBasis = recordIssue conn "warning" "BASIS_COMMIT_UNAVAILABLE"
                        ("operation " <> opId <> " records basis " <> basisText <>
                         ", but that commit is not available in the local object database")
                        Nothing (Just opId) Nothing (Just opId)
                  case basisInfo of
                    Right info -> case Map.lookup basis info of
                      Just (Just oi)
                        | objectInfoType oi == GitCommitObject -> pure ()
                      _ -> unavailableBasis
                    Left _ -> unavailableBasis

            -- Classify each operation + candidate commit
            forM_ (Map.toList candidates) $ \(opId, commitSet) -> do
              case Map.lookup opId operations of
                Nothing -> throwIO (userError ("candidate references missing registered operation: " <> Text.unpack opId))
                Just opData -> do
                  let objectIds = Set.fromList [regObjectObjectId obj | obj <- regOpObjects opData]
                      opObjects = regOpObjects opData

                  forM_ (Set.toList commitSet) $ \commitOid -> do
                    let commitText = gitOidText commitOid
                        row = Map.lookup commitOid rows
                    case row of
                      Nothing -> throwIO (userError ("candidate references missing commit observation: " <> Text.unpack commitText))
                      Just commitData -> do
                        let hasTrailer = opId `elem` findAllOpTrailers
                              (storedCommitMessage commitData)
                              (Map.keys operations)

                        let contains = containsRegisteredObjects observedTrees opObjects commitOid

                        when contains $ do
                          let firstParentTexts = case storedCommitParents commitData of
                                [] -> []
                                (p:_) -> [p]
                          let firstParentOids = catMaybesList (map textToGitOid firstParentTexts)
                              firstParentContains = not (null firstParentTexts)
                                && all (containsRegisteredObjects observedTrees opObjects) firstParentOids

                          if firstParentContains
                            then do
                              -- Reconciliation is canonical: a commit whose
                              -- first parent already has every sealed object
                              -- is not a placement, even if an earlier warm
                              -- pass had inserted it.
                              execute conn "DELETE FROM operation_commit WHERE op_id=? AND commit_oid=?"
                                [SQLText opId, SQLText commitText]
                              when hasTrailer $
                                recordIssue conn "warning" "REDUNDANT_OPERATION_TRAILER"
                                  ("commit " <> commitText <> " repeats ADRAI-Op " <> opId <>
                                   ", but its first parent already contains every sealed operation file")
                                  Nothing (Just opId) Nothing (Just opId)
                            else do
                              let nonFirstParentTexts = drop 1 (storedCommitParents commitData)
                              let nonFirstParentOids = catMaybesList (map textToGitOid nonFirstParentTexts)
                                  intro = not (null nonFirstParentTexts)
                                    && all (containsRegisteredObjects observedTrees opObjects) nonFirstParentOids

                              let basis = regOpBasis opData
                                  firstParentIsBasis = case storedCommitParents commitData of
                                    [] -> False
                                    (p:_) -> textToGitOid p == Just basis
                                  classification = determineClassification
                                    hasTrailer firstParentIsBasis intro

                              case canonicalParentsJson (storedCommitParents commitData) of
                                Nothing -> recordIssue conn "warning" "INVALID_COMMIT_PARENTS"
                                  ("commit " <> commitText <> " has malformed or duplicate parent OIDs")
                                  Nothing (Just opId) Nothing (Just opId)
                                Just parentsJson -> do
                                  execute conn
                                    "INSERT OR REPLACE INTO operation_commit VALUES(?,?,?,?,?,?,?)"
                                    [ SQLText opId
                                    , SQLText commitText
                                    , SQLText (operationClassificationValue classification)
                                    , SQLInteger (fromIntegral (storedCommitAuthored commitData))
                                    , SQLInteger (fromIntegral (storedCommitCommitted commitData))
                                    , SQLText (storedCommitSubject commitData)
                                    , SQLText parentsJson
                                    ]

                                  let expectedObjects = parseObjectsTrailer (storedCommitMessage commitData)
                                  when (isJust expectedObjects && expectedObjects /= Just objectIds) $ do
                                    let expectedText = Text.pack (show (sort (Set.toList (fromMaybe Set.empty expectedObjects))))
                                        actualText = Text.pack (show (sort (Set.toList objectIds)))
                                    recordIssue conn "warning" "OPERATION_OBJECT_SET_MISMATCH"
                                      ("commit " <> commitText <> " declares objects " <> expectedText <>
                                       " but current operation contains " <> actualText)
                                      Nothing (Just opId) Nothing (Just opId)

                        when (not contains) $ do
                          execute conn "DELETE FROM operation_commit WHERE op_id=? AND commit_oid=?"
                            [SQLText opId, SQLText commitText]
                          when hasTrailer $
                            recordIssue conn "warning" "TRAILER_WITHOUT_SEALED_OBJECTS"
                              ("commit " <> commitText <> " carries ADRAI-Op " <> opId <>
                               " but not the sealed operation files")
                              Nothing (Just opId) Nothing (Just opId)

            -- New operations with no placement.  One projection avoids a
            -- SELECT per registered operation and deliberately does not use
            -- a giant IN list, so SQLite variable limits cannot change the
            -- maintenance result.  We retain allNewOps order for diagnostics.
            beforeQuery
            placedRows <- query_ conn "SELECT DISTINCT op_id FROM operation_commit" :: IO [Only Text]
            let placedOperations = Set.fromList (map fromOnly placedRows)
            forM_ [opId | opId <- allNewOps, opId `Set.notMember` placedOperations] $ \opId ->
              case Map.lookup opId operations of
                Nothing -> pure ()
                Just _ -> recordIssue conn "warning" "NO_OPERATION_COMMIT"
                  ("no available commit could be bound to operation " <> opId)
                  Nothing (Just opId) Nothing (Just opId)
            pure (Right ())
  case result of
    Right val -> pure val
    Left e  -> pure (Left e)

-- | Gather all exact tree facts needed by this reconciliation in bounded
-- groups.  One bounded multi-revision cat-file plan observes every candidate,
-- first-parent, and merge-parent exact path; all classification checks then
-- consult that same immutable observation.  Git errors and malformed protocol
-- output remain fatal to the enclosing reconciliation, preserving fail-closed
-- behaviour.
observeCandidateTrees
  :: Repository
  -> Map Text RegisteredOperationData
  -> Map Text (Set GitOid)
  -> Map GitOid StoredCommitData
  -> IO (Map GitOid (Map RepoPath (Maybe GitObjectInfo)))
observeCandidateTrees repo operations candidates rows = do
  let requests = groupTreeObservationRequests
        [ (observedCommit, mapMaybeRegisteredPath opObjects)
        | (opId, candidateSet) <- Map.toList candidates
        , Just opData <- [Map.lookup opId operations]
        , let opObjects = regOpObjects opData
        , candidate <- Set.toList candidateSet
        , observedCommit <- candidate : catMaybesList (map textToGitOid (maybe [] storedCommitParents (Map.lookup candidate rows)))
        ]
      expectedBlobOids =
        [ blobOid
        | (opId, candidateSet) <- Map.toList candidates
        , not (Set.null candidateSet)
        , Just operation <- [Map.lookup opId operations]
        , object <- regOpObjects operation
        , Just blobOid <- [textToGitOid (regObjectBlobOid object)]
        ]
  result <- lookupTreeObjectInfoAtRevisions repo requests expectedBlobOids
  case result of
    Left problem -> throwIO (userError ("grouped cat-file tree observation failed: " <> show problem))
    Right entries -> pure entries
  where
    mapMaybeRegisteredPath = foldr addPath []
    addPath object paths =
      case mkRepoPath (regObjectPath object) of
        Left _ -> paths
        Right path -> path : paths

-- | The number of bounded bare-object request windows required for a basis
-- set.  A caller-scoped persistent session may serve all nonempty windows in
-- one process; this value intentionally does not count processes.
-- Keeping this visible makes the fresh-overlay invariant testable: duplicate
-- operation bases are coalesced before the single 'batchObjectInfo' call.
basisObjectRequestCount :: [GitOid] -> Int
basisObjectRequestCount = objectBatchRequestCount

-- | Coalesce all requested managed paths for each observed commit.  Candidate
-- and parent checks share this plan, so adding operation members never creates
-- a process per member and duplicate commits remain one observation group.
groupTreeObservationRequests :: [(GitOid, [RepoPath])] -> Map GitOid (Set RepoPath)
groupTreeObservationRequests =
  Map.fromListWith Set.union . map (\(commitOid, paths) -> (commitOid, Set.fromList paths))

-- | Context-aware exact tree-path child count for a grouped observation plan.
-- Line-safe requests share one persistent correlated @cat-file@ child, while
-- paths with line-protocol framing bytes retain the literal @ls-tree -z@
-- fallback.  This uses the same eligibility and argv splitter as execution,
-- so the result is a truthful total process fan-out rather than a former
-- 256-item-only estimate.
treeObservationRequestCount :: FilePath -> FilePath -> [(GitOid, [RepoPath])] -> Either GitError Int
treeObservationRequestCount executable commandDirectory requests =
  let grouped = groupTreeObservationRequests requests
   in (treePathBatchSessionCount grouped +) <$> treePathRevisionLsTreeChildCount executable commandDirectory grouped

containsRegisteredObjects
  :: Map GitOid (Map RepoPath (Maybe GitObjectInfo))
  -> [RegisteredObjectData]
  -> GitOid
  -> Bool
containsRegisteredObjects observedTrees objects commitOid =
  case Map.lookup commitOid observedTrees of
    Nothing -> False
    Just entries -> all (matches entries) objects
  where
    matches entries object =
      case (mkRepoPath (regObjectPath object), mkGitOid (regObjectBlobOid object)) of
        (Right path, Right expectedOid) ->
          case Map.lookup path entries of
            Just (Just info) ->
              objectInfoOid info == expectedOid
                && objectInfoType info == GitBlobObject
            _ -> False
        _ -> False

-- | Validate the exact, single NUL-framed response expected from an argv-based
-- @git ls-tree -z -- <path>@ request.  The Git decoder operates on raw bytes,
-- splitting only Git's structural NUL/TAB delimiters; no path bytes are ever
-- whitespace-tokenized or leniently decoded here.
decodeExactBlobTreeEntry :: Text -> Text -> ByteString -> Bool
decodeExactBlobTreeEntry expectedPath expectedBlobOid raw =
  case (mkRepoPath expectedPath, mkGitOid expectedBlobOid, decodeGitTreeOutput raw) of
    (Right path, Right blobOid, Right [entry]) ->
      gitTreePath entry == path
        && gitTreeOid entry == blobOid
        && gitTreeObjectType entry == GitBlobObject
    _ -> False

canonicalParentsJson :: [Text] -> Maybe Text
canonicalParentsJson parents = do
  validated <- traverse (fmap gitOidText . textToGitOid) parents
  if length validated == Set.size (Set.fromList validated)
    then Just (TextEncoding.decodeUtf8 (BSL.toStrict (Aeson.encode validated)))
    else Nothing

determineClassification
  :: Bool  -- hasTrailer
  -> Bool  -- firstParentIsBasis
  -> Bool  -- introFromNonFirst
  -> OperationClassification
determineClassification hasTrailer firstParentIsBasis introFromNonFirst =
  if introFromNonFirst
    then IntroductionClassification
    else if firstParentIsBasis
      then OriginalClassification
      else if hasTrailer
        then CopyClassification
        else IntroductionClassification

parseObjectsTrailer :: Text -> Maybe (Set Text)
parseObjectsTrailer message =
  let match = find (\line ->
        let trimmed = Text.stripStart line
        in Text.isPrefixOf "ADRAI-Objects:" trimmed) (Text.lines message)
  in case match of
       Nothing -> Nothing
       Just line ->
         let raw = Text.strip (Text.drop 14 (Text.stripStart line))
             stripped = Text.strip raw
         in if stripped == "bootstrap"
            then Just (Set.fromList ["bootstrap"])
             else
               Just
                 ( Set.fromList
                     [ objectId
                     | field <- Text.splitOn "," stripped
                     , objectId <- Text.words (Text.strip field)
                     ]
                 )

-- | Prune unavailable placements.
pruneUnavailablePlacements
  :: Repository
  -> Connection
  -> IO (Either GitError Int)
pruneUnavailablePlacements repo conn = do
  allCommitOids <- query_ conn
    "SELECT DISTINCT commit_oid FROM operation_commit UNION SELECT DISTINCT commit_oid FROM line_landing"
    :: IO [Only Text]
  let commitOids = map (\(Only oid) -> GitOid oid) allCommitOids
  if null commitOids
    then pure (Right 0)
    else do
      infoResult <- batchObjectInfo repo commitOids
      case infoResult of
        Left e -> pure (Left e)
        Right info ->
          let missing =
                [ oid
                | oid <- commitOids
                , case Map.lookup oid info of
                    Just (Just oi) -> objectInfoType oi /= GitCommitObject
                    _ -> True
                ]
          in if null missing
             then pure (Right 0)
             else do
               forM_ (map gitOidText missing) $ \oidText -> do
                 execute conn "DELETE FROM operation_commit WHERE commit_oid=?" [SQLText oidText]
                 execute conn "DELETE FROM line_landing WHERE commit_oid=?" [SQLText oidText]
               pure (Right (length missing))

-- | Record a provenance issue.
recordIssue
  :: Connection
  -> Text  -- severity
  -> Text  -- code
  -> Text  -- message
  -> Maybe Text  -- adrId
  -> Maybe Text  -- objectId
  -> Maybe Text  -- path
  -> Maybe Text  -- opId
  -> IO ()
recordIssue conn severity code message adrId objectId path opId =
  let key = issueKey code message opId objectId path
  in execute conn
      "INSERT OR IGNORE INTO provenance_issue VALUES(?,?,?,?,?,?,?,?)"
      [ SQLText key
      , SQLText severity
      , SQLText code
      , maybe SQLNull SQLText adrId
      , maybe SQLNull SQLText objectId
      , maybe SQLNull SQLText path
      , SQLText message
      , maybe SQLNull SQLText opId
      ]

-- | Compute a SHA-256 issue key.
issueKey :: Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text
issueKey code message opId objectId path =
  let payload = Aeson.Array (Vector.fromList
        [ Aeson.String code
        , Aeson.String message
        , maybe Aeson.Null Aeson.String opId
        , maybe Aeson.Null Aeson.String objectId
        , maybe Aeson.Null Aeson.String path
        ])
      encoded = BSL.toStrict (Aeson.encode payload)
      digest = sha256Digest encoded
  in digestToHex digest

-- ============================================================
-- Utility helpers
-- ============================================================

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

textToGitOid :: Text -> Maybe GitOid
textToGitOid t = case mkGitOid t of
  Right oid -> Just oid
  Left _    -> Nothing

catMaybesList :: [Maybe a] -> [a]
catMaybesList = foldr (\x acc -> case x of Just v -> v : acc; Nothing -> acc) []

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs)
  | p x       = Just x
  | otherwise = find p xs

fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing  = d
fromMaybe _ (Just v) = v

isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just _) = True
