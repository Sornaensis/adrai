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
  , RegisteredOperationData (..)
  , RegisteredObjectData (..)
  , registeredOperations
  , registerOperationGroups
  , StoredCommitData (..)
  , storeNewCommits
  , loadCommitRows
  , candidateCommits
  , processCandidates
  , pruneUnavailablePlacements
  , recordIssue
  , issueKey
  )
where

import Adrai.Git
  ( GitError,
    GitOid(..),
    GitObjectType (GitCommitObject),
    GitObjectInfo (..),
    GitProcessResult (..),
    Repository (..),
    batchObjectInfo,
    gitOidText,
    runRepository,
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
    OperationId (..),
    RepoPath (..),
    digestBytes,
    operationIdText,
    repoPathText,
  )
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, foldM, when)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.List (sort, sortBy)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.Ord (comparing)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
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
import System.Exit (ExitCode (ExitSuccess))

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
  let payload =
        [ Aeson.Array
            (Vector.fromList
              [ Aeson.String (_parsedObjectId doc),
                Aeson.String (repoPathText (parsedManagedPath doc)),
                Aeson.String (maybe "" gitOidText (parsedBlobOid doc)),
                Aeson.String (parsedSemanticHash doc)
              ])
          | doc <- sortBy (comparing (repoPathText . parsedManagedPath)) members
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
registeredOperations conn = do
  result <- try @SomeException $ do
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
          | otherwise = do
              let sig = operationSignature members
                  firstDoc = head members
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
          Left _ -> pure (Right (Map.empty, Map.empty))
          Right allPaths -> do
            -- We need commit log snapshots too. Use the Discovery module function.
            -- But addedPathsForCommits already returns both. Actually, looking at
            -- the Discovery module, addedPathsForCommits returns Map GitOid (Set Text)
            -- and we also need commit observations. Let me use commitLogSnapshotForOids.
            obs <- commitLogSnapshotForOids repo commits
            case obs of
              Left _ -> pure (Right (Map.empty, Map.empty))
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
basename p =
  case Text.reverse p of
    "" -> ""
    r -> case Text.break (== '/') r of
           (_, "") -> r
           (_, rs) -> Text.drop 1 rs

managedSuffixes :: [Text]
managedSuffixes = [".decision.md", ".connection.md"]

-- | Load existing commit observations, filling missing from the Git store.
loadCommitRows
  :: Repository
  -> Connection
  -> [GitOid]
  -> IO (Either SomeException (Map GitOid StoredCommitData))
loadCommitRows repo conn oids = do
  result <- try @SomeException $ do
    let unique = dedupList oids
    if null unique
      then pure (Right Map.empty)
      else do
        let placeholders = Text.intercalate "," (replicate (length unique) "?")
            params = map (\oid -> SQLText (gitOidText oid)) unique
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
            stored <- loadCommitRows repo conn missingOids
            case stored of
              Left e -> pure (Left e)
              Right missingData -> pure (Right (Map.union existing missingData))
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
candidateCommits conn newCommits newOpIds = do
  result <- try @SomeException $ do
    operations <- registeredOperations conn
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
            pathCommits <- query_ conn
              (asQuery ("SELECT path,commit_oid FROM managed_path_addition "
               <> "WHERE commit_oid IN (" <> placeholders <> ")"))
              :: IO [(Text, Text)]

            msgCommits <- query_ conn
              (asQuery ("SELECT commit_oid,message FROM commit_observation "
               <> "WHERE commit_oid IN (" <> placeholders <> ")"))
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
            finalCandidates <- foldM (\m opId ->
                  case Map.lookup opId ops of
                    Nothing -> pure m
                    Just opData ->
                      let opPaths = Set.fromList [regObjectPath obj | obj <- regOpObjects opData]
                      in if Set.null opPaths
                         then pure m
                         else do
                           let pathPh = Text.intercalate "," (replicate (Set.size opPaths) "?")
                               pathParams = map (\p -> SQLText p) (Set.toList opPaths)
                           qualifyingCommits <- query conn
                             (asQuery ("SELECT DISTINCT commit_oid FROM managed_path_addition "
                              <> "WHERE path IN (" <> pathPh <> ")"))
                             pathParams
                             :: IO [Only Text]
                           let qualifying = [ GitOid co
                                             | Only co <- qualifyingCommits
                                             , let allPathsInCommit = all (\p -> any (\(p2, c2) -> p2 == p && c2 == co) pathCommits) (Set.toList opPaths)
                                             , allPathsInCommit
                                             ]
                           let updated = Map.alter (\_ -> Just (Set.union (Map.findWithDefault Set.empty opId m) (Set.fromList qualifying)))
                                   opId m
                           pure updated)
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
processCandidates repo conn candidates newOpIds = do
  result <- try @SomeException $ do
    ops <- registeredOperations conn
    case ops of
      Left e -> pure (Left e)
      Right operations -> do
        let allCommits = dedupList
              [ oid
              | commits <- Map.elems candidates
              , oid <- Set.toList commits
              ]
        let allNewOps = dedupList newOpIds

        commitRows <- loadCommitRows repo conn allCommits
        case commitRows of
          Left e -> pure (Left e)
          Right rows -> do
            -- Check basis commits for new operations
            forM_ (zip allNewOps [Map.lookup op operations | op <- allNewOps]) $ \(opId, maybeOpData) ->
              case maybeOpData of
                Nothing -> pure ()
                Just opData -> do
                  let basis = regOpBasis opData
                      basisText = gitOidText basis
                  basisInfo <- batchObjectInfo repo [basis]
                  case basisInfo of
                    Right info -> case Map.lookup basis info of
                      Just (Just oi)
                        | objectInfoType oi == GitCommitObject -> pure ()
                        | otherwise -> recordIssue conn "warning" "BASIS_COMMIT_UNAVAILABLE"
                          ("operation " <> opId <> " records basis " <> basisText <>
                           ", but that commit is not available in the local object database")
                          Nothing (Just opId) Nothing (Just opId)
                    Left _ -> recordIssue conn "warning" "BASIS_COMMIT_UNAVAILABLE"
                      ("operation " <> opId <> " records basis " <> basisText <>
                       ", but that commit is not available in the local object database")
                      Nothing (Just opId) Nothing (Just opId)

            -- Classify each operation + candidate commit
            forM_ (Map.toList candidates) $ \(opId, commitSet) -> do
              case Map.lookup opId operations of
                Nothing -> pure ()
                Just opData -> do
                  let objectIds = Set.fromList [regObjectObjectId obj | obj <- regOpObjects opData]
                      opObjects = regOpObjects opData

                  forM_ (Set.toList commitSet) $ \commitOid -> do
                    let commitText = gitOidText commitOid
                        row = Map.lookup commitOid rows
                    case row of
                      Nothing -> pure ()
                      Just commitData -> do
                        let hasTrailer = opId `elem` findAllOpTrailers
                              (storedCommitMessage commitData)
                              (Map.keys operations)

                        contains <- checkContainsAllObjects repo opObjects [commitOid]

                        when contains $ do
                          let firstParentTexts = case storedCommitParents commitData of
                                [] -> []
                                (p:_) -> [p]
                          firstParentContains <- if null firstParentTexts
                            then pure False
                            else do
                              let firstParentOids = catMaybesList (map textToGitOid firstParentTexts)
                              checkContainsAllObjects repo opObjects firstParentOids

                          if firstParentContains
                            then recordIssue conn "warning" "REDUNDANT_OPERATION_TRAILER"
                              ("commit " <> commitText <> " repeats ADRAI-Op " <> opId <>
                               ", but its first parent already contains every sealed operation file")
                              Nothing (Just opId) Nothing (Just opId)
                            else do
                              let nonFirstParentTexts = drop 1 (storedCommitParents commitData)
                              intro <- if null nonFirstParentTexts
                                then pure False
                                else do
                                  let nonFirstParentOids = catMaybesList (map textToGitOid nonFirstParentTexts)
                                      introCheck = checkContainsAllObjects repo opObjects nonFirstParentOids
                                  introCheck

                              let basis = regOpBasis opData
                                  firstParentIsBasis = case storedCommitParents commitData of
                                    [] -> False
                                    (p:_) -> textToGitOid p == Just basis
                                  classification = determineClassification
                                    hasTrailer firstParentIsBasis intro

                              execute conn
                                "INSERT OR IGNORE INTO operation_commit VALUES(?,?,?,?,?,?,?)"
                                [ SQLText opId
                                , SQLText commitText
                                , SQLText (operationClassificationValue classification)
                                , SQLInteger (fromIntegral (storedCommitAuthored commitData))
                                , SQLInteger (fromIntegral (storedCommitCommitted commitData))
                                , SQLText (storedCommitSubject commitData)
                                , SQLText (Text.unwords (storedCommitParents commitData))
                                ]

                              let expectedObjects = parseObjectsTrailer (storedCommitMessage commitData)
                              when (isJust expectedObjects && expectedObjects /= Just objectIds) $ do
                                let expectedText = Text.pack (show (sort (Set.toList (fromMaybe Set.empty expectedObjects))))
                                    actualText = Text.pack (show (sort (Set.toList objectIds)))
                                recordIssue conn "warning" "OPERATION_OBJECT_SET_MISMATCH"
                                  ("commit " <> commitText <> " declares objects " <> expectedText <>
                                   " but current operation contains " <> actualText)
                                  Nothing (Just opId) Nothing (Just opId)

                        when (not contains && hasTrailer) $ do
                          recordIssue conn "warning" "TRAILER_WITHOUT_SEALED_OBJECTS"
                            ("commit " <> commitText <> " carries ADRAI-Op " <> opId <>
                             " but not the sealed operation files")
                            Nothing (Just opId) Nothing (Just opId)

            -- New operations with no placement
            forM_ allNewOps $ \opId -> do
              placed <- query conn (asQuery "SELECT 1 FROM operation_commit WHERE op_id=?") [SQLText opId]
                :: IO [(Only Integer)]
              when (null placed) $ do
                case Map.lookup opId operations of
                  Nothing -> pure ()
                  Just opData -> recordIssue conn "warning" "NO_OPERATION_COMMIT"
                    ("no available commit could be bound to operation " <> opId)
                    Nothing (Just opId) Nothing (Just opId)
            pure (Right ())
  case result of
    Right val -> pure val
    Left e  -> pure (Left e)

checkContainsAllObjects
  :: Repository
  -> [RegisteredObjectData]
  -> [GitOid]
  -> IO Bool
checkContainsAllObjects repo objects commits = do
  if null objects
    then pure True
    else do
      let specs =
            [ (oid, regObjectPath obj, regObjectBlobOid obj)
            | oid <- commits
            , obj <- objects
            ]
      results <- forM specs $ \(commitOid, path, expectedBlobOid) ->
        checkCommitPath repo commitOid path expectedBlobOid
      pure (and results)

checkCommitPath
  :: Repository
  -> GitOid
  -> Text
  -> Text
  -> IO Bool
checkCommitPath repo commitOid path expectedBlobOid =
  let args = ["ls-tree", "-z", "--full-tree", Text.unpack (gitOidText commitOid),
              "--", Text.unpack path]
  in do
    result <- runRepository repo "ls-tree path" args mempty
    case result of
      Left _ -> pure False
      Right proc ->
        if processExitCode proc /= ExitSuccess
          then pure False
          else pure $ case decodeTreeEntry (processStdout proc) of
            Just (entryOid, _) -> entryOid == expectedBlobOid
            Nothing -> False

decodeTreeEntry :: ByteString -> Maybe (Text, Text)
decodeTreeEntry raw =
  case Text.strip (TextEncoding.decodeUtf8With TextEncodingError.lenientDecode raw) of
    "" -> Nothing
    line ->
      case Text.words line of
        [mode, typeStr, oid, _path] -> Just (oid, typeStr)
        _ -> Nothing

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
            else Just (Set.fromList (filter (not . Text.null) (Text.words stripped)))

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
