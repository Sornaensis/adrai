{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Adrai.Integrity
  ( IntegritySeverity (..),
    IntegrityIssueCode (..),
    integrityIssueCodeText,
    IntegrityIssue (..),
    ManagedSnapshotEntry,
    snapshotEntryPath,
    snapshotEntryBytes,
    snapshotEntryDocument,
    parseSnapshotEntry,
    snapshotEntryFromParsed,
    relocateSnapshotEntry,
    validateManagedSnapshot,
    validateAppendOnlyDelta,
    validateAppendOnlyHistory,
    validateManagedHistory,
  )
where

import Adrai.Format.Document
  ( ConnectionRecord (..),
    DecisionRecord (..),
    DocumentError,
    ManagedRecord (..),
    ParsedManagedDocument (..),
    parseManagedDocument,
    validateManagedLocation,
  )
import Adrai.Provenance (provenanceOperationId)
import Adrai.Types
  ( ManagedPaths,
    ObjectRef,
    OperationId,
    RepoPath,
    connectionObjectRef,
    objectRefText,
    operationIdText,
    recordObjectRef,
    repoPathText,
  )
import Data.ByteString (ByteString)
import Data.List (groupBy, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

data IntegritySeverity
  = IntegrityWarning
  | IntegrityError
  deriving (Eq, Ord, Show)

data IntegrityIssueCode
  = NonCanonicalPath
  | DuplicateObjectId
  | AppendOnlyRewrite
  | AppendOnlyDelete
  | MissingHistoricalObject
  | IncompleteOperation
  | InvalidManagedDocument
  | InconsistentOperationCapsule
  | BasisCommitUnavailable
  | UnknownOperationShape
  | CrossAdrOperation
  | ProvenanceParentMismatch
  | AmendmentOperationMismatch
  | AmendmentEdgeCardinality
  | CreateHasAmendmentEdge
  | CreateProvenanceHasParents
  | ScopeEventKindMismatch
  | DomainEventKindMismatch
  | StatusEventKindMismatch
  | ManagedNonBlob
  | InvalidRepositoryConfig
  | HistoryCoverageIncomplete
  deriving (Eq, Ord, Show, Enum, Bounded)

integrityIssueCodeText :: IntegrityIssueCode -> Text
integrityIssueCodeText code =
  case code of
    NonCanonicalPath -> "NON_CANONICAL_PATH"
    DuplicateObjectId -> "DUPLICATE_OBJECT_ID"
    AppendOnlyRewrite -> "APPEND_ONLY_REWRITE"
    AppendOnlyDelete -> "APPEND_ONLY_DELETE"
    MissingHistoricalObject -> "MISSING_HISTORICAL_OBJECT"
    IncompleteOperation -> "INCOMPLETE_OPERATION"
    InvalidManagedDocument -> "INVALID_MANAGED_DOCUMENT"
    InconsistentOperationCapsule -> "INCONSISTENT_OPERATION_CAPSULE"
    BasisCommitUnavailable -> "BASIS_COMMIT_UNAVAILABLE"
    UnknownOperationShape -> "UNKNOWN_OPERATION_SHAPE"
    CrossAdrOperation -> "CROSS_ADR_OPERATION"
    ProvenanceParentMismatch -> "PROVENANCE_PARENT_MISMATCH"
    AmendmentOperationMismatch -> "AMENDMENT_OPERATION_MISMATCH"
    AmendmentEdgeCardinality -> "AMENDMENT_EDGE_CARDINALITY"
    CreateHasAmendmentEdge -> "CREATE_HAS_AMENDMENT_EDGE"
    CreateProvenanceHasParents -> "CREATE_PROVENANCE_HAS_PARENTS"
    ScopeEventKindMismatch -> "SCOPE_EVENT_KIND_MISMATCH"
    DomainEventKindMismatch -> "DOMAIN_EVENT_KIND_MISMATCH"
    StatusEventKindMismatch -> "STATUS_EVENT_KIND_MISMATCH"
    ManagedNonBlob -> "MANAGED_NONBLOB"
    InvalidRepositoryConfig -> "INVALID_REPOSITORY_CONFIG"
    HistoryCoverageIncomplete -> "HISTORY_COVERAGE_INCOMPLETE"

data IntegrityIssue = IntegrityIssue
  { integritySeverity :: IntegritySeverity,
    integrityCode :: IntegrityIssueCode,
    integrityObjectId :: Maybe ObjectRef,
    integrityOperationId :: Maybe OperationId,
    integrityPath :: Maybe RepoPath,
    integrityMessage :: Text
  }
  deriving (Eq, Show)

-- | One authoritative tree entry. Presence and exact bytes are independent of
-- parsing so malformed rewrites cannot be mistaken for deletions.
data ManagedSnapshotEntry = ManagedSnapshotEntry
  { snapshotEntryPath :: RepoPath,
    snapshotEntryBytes :: ByteString,
    snapshotEntryDocument :: Either DocumentError ParsedManagedDocument
  }
  deriving (Eq, Show)

snapshotEntryFromParsed :: ParsedManagedDocument -> ManagedSnapshotEntry
snapshotEntryFromParsed document =
  parseSnapshotEntry (parsedManagedPath document) (parsedManagedBytes document)

-- | Bind an authoritative path and byte string to the parse result produced
-- from those exact inputs.
parseSnapshotEntry :: RepoPath -> ByteString -> ManagedSnapshotEntry
parseSnapshotEntry path bytes =
  ManagedSnapshotEntry path bytes (parseManagedDocument path bytes)

-- | Model a tree rename without altering the immutable bytes. Successful
-- parsed content is rebound to the authoritative destination path.
relocateSnapshotEntry :: RepoPath -> ManagedSnapshotEntry -> ManagedSnapshotEntry
relocateSnapshotEntry path entry =
  entry
    { snapshotEntryPath = path,
      snapshotEntryDocument =
        fmap (\document -> document {parsedManagedPath = path}) (snapshotEntryDocument entry)
    }

-- | Validate one complete managed tree. The entry path and bytes are
-- authoritative; redundant values retained by a successful parse are replaced
-- before location and duplicate checks.
validateManagedSnapshot :: ManagedPaths -> [ManagedSnapshotEntry] -> [IntegrityIssue]
validateManagedSnapshot paths entries =
  sortIssues (parseIssues <> locationIssues <> duplicateIssues)
  where
    parseIssues =
      [ issue
          InvalidManagedDocument
          Nothing
          Nothing
          (Just (snapshotEntryPath entry))
          ("invalid managed document: " <> Text.pack (show problem))
        | entry <- entries,
          Left problem <- [snapshotEntryDocument entry]
      ]
    documents = mapMaybe authoritativeDocument entries
    locationIssues = mapMaybe locationIssue documents
    locationIssue document =
      case validateManagedLocation paths document of
        Right () -> Nothing
        Left problem ->
          Just
            ( issue
                NonCanonicalPath
                (Just (managedObject document))
                (Just (managedOperation document))
                (Just (parsedManagedPath document))
                ("managed object is not at its canonical path: " <> Text.pack (show problem))
            )
    duplicateIssues = concatMap duplicateGroupIssues duplicateGroups
    duplicateGroups =
      filter ((> 1) . length)
        . groupBy (\left right -> fst left == fst right)
        . sortOn (\(objectId, document) -> (objectId, repoPathText (parsedManagedPath document)))
        $ [(managedObject document, document) | document <- documents]
    -- Discovery is path-sorted. Preserve the first occurrence as the origin
    -- and report only later copies, matching the compiler contract.
    duplicateGroupIssues group =
      case group of
        [] -> []
        _origin : duplicates ->
          [ issue
              DuplicateObjectId
              (Just objectId)
              (Just (managedOperation document))
              (Just (parsedManagedPath document))
              ("object " <> objectRefText objectId <> " occurs at more than one managed path")
            | (objectId, document) <- duplicates
          ]

-- | Validate one adjacent repository transition. Callers proving whole-history
-- append-only behavior must apply this to every consecutive revision (or use
-- 'validateAppendOnlyHistory'); endpoint comparison alone is insufficient.
validateAppendOnlyDelta :: [ManagedSnapshotEntry] -> [ManagedSnapshotEntry] -> [IntegrityIssue]
validateAppendOnlyDelta previous current =
  sortIssues (missingIssues <> rewriteIssues <> incompleteIssues)
  where
    previousByPath = entryMap previous
    currentByPath = entryMap current
    missing =
      [ entry
        | (path, entry) <- Map.toAscList previousByPath,
          Map.notMember path currentByPath
      ]
    missingIssues =
      [ issue
          MissingHistoricalObject
          (managedEntryObject entry)
          (managedEntryOperation entry)
          (Just (snapshotEntryPath entry))
          "immutable managed object is missing from the current snapshot"
        | entry <- missing
      ]
    rewriteIssues =
      [ issue
          AppendOnlyRewrite
          (managedEntryObject oldEntry)
          (managedEntryOperation oldEntry)
          (Just path)
          "immutable managed object bytes changed at an existing path"
        | (path, oldEntry) <- Map.toAscList previousByPath,
          Just newEntry <- [Map.lookup path currentByPath],
          snapshotEntryBytes oldEntry /= snapshotEntryBytes newEntry
      ]
    previousMembers = operationMembers previousByPath previous
    currentMembers = operationMembers previousByPath current
    incompleteOperations =
      [ operation
        | (operation, expectedObjects) <- Map.toAscList previousMembers,
          sort expectedObjects /= sort (Map.findWithDefault [] operation currentMembers)
      ]
    incompleteIssues =
      [ issue
          IncompleteOperation
          Nothing
          (Just operation)
          Nothing
          ("operation " <> operationIdText operation <> " is missing or has changed immutable members")
        | operation <- incompleteOperations
      ]

-- | Preserve violations across a complete ordered revision sequence, including
-- rewrite/restore and delete/recreate histories whose endpoints are identical.
validateAppendOnlyHistory :: [[ManagedSnapshotEntry]] -> [IntegrityIssue]
validateAppendOnlyHistory snapshots =
  sortIssues (concat (zipWith validateAppendOnlyDelta snapshots (drop 1 snapshots)))

-- | Combined current-tree and adjacent-history validation.
validateManagedHistory :: ManagedPaths -> [ManagedSnapshotEntry] -> [ManagedSnapshotEntry] -> [IntegrityIssue]
validateManagedHistory paths previous current =
  sortIssues (validateManagedSnapshot paths current <> validateAppendOnlyDelta previous current)

authoritativeDocument :: ManagedSnapshotEntry -> Maybe ParsedManagedDocument
authoritativeDocument entry =
  case snapshotEntryDocument entry of
    Left _ -> Nothing
    Right document ->
      Just
        document
          { parsedManagedPath = snapshotEntryPath entry,
            parsedManagedBytes = snapshotEntryBytes entry
          }

entryMap :: [ManagedSnapshotEntry] -> Map.Map RepoPath ManagedSnapshotEntry
entryMap = Map.fromList . map (\entry -> (snapshotEntryPath entry, entry))

operationMembers :: Map.Map RepoPath ManagedSnapshotEntry -> [ManagedSnapshotEntry] -> Map.Map OperationId [ObjectRef]
operationMembers previousByPath = foldr addMember Map.empty
  where
    addMember entry members =
      case entryIdentity previousByPath entry of
        Nothing -> members
        Just (operation, objectId) -> Map.insertWith (<>) operation [objectId] members

-- Invalid current bytes at a historical path inherit the previous identity for
-- membership only. This preserves presence/completeness while still reporting
-- the parse failure and byte rewrite separately.
entryIdentity :: Map.Map RepoPath ManagedSnapshotEntry -> ManagedSnapshotEntry -> Maybe (OperationId, ObjectRef)
entryIdentity previousByPath entry =
  case authoritativeDocument entry of
    Just document -> Just (managedOperation document, managedObject document)
    Nothing -> do
      previousEntry <- Map.lookup (snapshotEntryPath entry) previousByPath
      previousDocument <- authoritativeDocument previousEntry
      Just (managedOperation previousDocument, managedObject previousDocument)

managedEntryObject :: ManagedSnapshotEntry -> Maybe ObjectRef
managedEntryObject = fmap managedObject . authoritativeDocument

managedEntryOperation :: ManagedSnapshotEntry -> Maybe OperationId
managedEntryOperation = fmap managedOperation . authoritativeDocument

managedObject :: ParsedManagedDocument -> ObjectRef
managedObject document =
  case parsedManagedRecord document of
    ManagedDecision decision -> recordObjectRef (decisionRecord decision)
    ManagedConnection connection -> connectionObjectRef (connectionRecordId connection)

managedOperation :: ParsedManagedDocument -> OperationId
managedOperation = provenanceOperationId . parsedManagedCapsule

issue :: IntegrityIssueCode -> Maybe ObjectRef -> Maybe OperationId -> Maybe RepoPath -> Text -> IntegrityIssue
issue code objectId operationId path message =
  IntegrityIssue IntegrityError code objectId operationId path message

sortIssues :: [IntegrityIssue] -> [IntegrityIssue]
sortIssues =
  sortOn
    ( \value ->
        ( integrityCode value,
          fmap repoPathText (integrityPath value),
          fmap objectRefText (integrityObjectId value),
          fmap operationIdText (integrityOperationId value),
          integrityMessage value
        )
    )
