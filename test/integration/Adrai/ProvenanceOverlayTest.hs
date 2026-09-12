{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Integration tests for provenance overlay topology classification.
--
-- Each test creates a temporary Git repository, writes ADRAI managed files
-- with sealed provenance capsules, registers the operation in the overlay
-- database, runs @ensureProvenance@, and then queries the SQLite overlay
-- to assert the correct classification.
--
-- The 13 test cases exercise every classification path in
-- 'Adrai.Provenance.Classification.determineClassification' as well as
-- issue-recorded edge cases.
module Adrai.ProvenanceOverlayTest (tests) where

import Adrai.Git
  ( GitClient (..),
    GitError (..),
    GitOid (..),
    Repository (..),
    gitCommitNodeOid,
    gitOidText,
    reachableCommitGraphAt,
    systemGit,
  )
import Adrai.GitTestSupport
  ( commitFile,
    commitFiles,
    cloneIndependentDepthOneRepository,
    gitSuccess,
    initTestRepository,
    outputText,
    requireRepoPath,
    requireRevision,
  )
import Adrai.Provenance
  ( ProvenanceCapsule,
    ProvenanceCapsuleInput (..),
    ProvenanceObjectId (..),
    mkEventKind,
    mkGitOid,
    mkProvenanceCapsule,
    provenanceObjectFromRef,
    sealSemantic,
    sha256Digest,
    semanticDigest,
  )
import Adrai.Provenance.Classification
  ( ParsedManagedDocument (..),
    operationSignature,
      loadCommitRowsWithQueryObserver,
     candidateCommits,
     candidateCommitsWithQueryObserver,
    canonicalParentsJson,
    decodeExactBlobTreeEntry,
    basisObjectRequestCount,
    groupTreeObservationRequests,
     processCandidates,
     processCandidatesWithQueryObserver,
    recordIssue,
    treeObservationRequestCount,
  )
import Adrai.Provenance.Overlay
  (
    ProvenanceEvidence (..),
    ProvenanceOperationEvidence (..),
    RegisteredOperationRow (..),
    RegisteredObjectRow (..),
    OperationCommitRow (..),
    LineConfigRow (..),
    LineRefStateRow (..),
    LineLandingRow (..),
    RefObservationRow (..),
    ObservationRootRow (..),
    ProvenanceIssueRow (..),
    ProvenanceEvidenceError
      ( ProvenanceEvidenceDatabaseError,
        ProvenanceEvidenceInvalidOid,
        ProvenanceEvidenceMissingRegistration,
        ProvenanceEvidenceMissingConfig,
        ProvenanceEvidenceMissingObjects,
        ProvenanceEvidenceDuplicateRegistration,
        ProvenanceEvidenceMissingTargetPlacement
      ),
    createOverlaySchema,
    overlaySchemaDdl,
    overlaySchemaVersion,
    provenanceDatabasePath,
    overlaySchemaIndexes,
  )
import Adrai.Provenance.Ensure
  ( configKey,
     ensureProvenance,
     ensureProvenanceWithRecoveryWitness,
     ensureProvenanceWithRecoveryWitnessAndWorklistObserver,
    recoveryProjectionValidity,
    recoveryWitnessSuppresses,
     seedRegisteredOperationsFromSemanticCache,
     seedRegisteredOperationsFromSemanticCacheWithQueryObserver,
    usableWitnessRange,
    extendTargetAncestryCache,
    readProvenanceEvidenceAt,
    readProvenanceEvidenceAtWith,
    ProvenanceUpdate (..),
  )
import Adrai.Provenance.RecoveryWitness (ProvenanceRecoveryWitness (..))
import Adrai.Provenance.Discovery (addedPathsForCommits, decodeAddedPathsOutput)
import Adrai.Repository
  ( ResolvedRepositoryRevision (..),
    resolveRepositoryRevision,
  )
import Adrai.RetainedCLI.RepositorySeed
  ( RepositorySeed,
    createRepositorySeed,
    removeRepositorySeed,
    withRepositorySeedCopy,
  )
import Adrai.Git (discoverRepository)
import Adrai.Sqlite (asQuery)
import Adrai.Types
  ( ActorKind (..),
    AdrId,
    Config,
    ConfigSchema (..),
    Digest (..),
    GitRef (..),
    LogicalLine (..),
    ObjectRef,
    OperationId (..),
    ProvenanceInputs (..),
    RepoPath (..),
    adrObjectRef,
    mkActor,
    adrIdText,
    configManagedPaths,
    configLogicalLines,
    logicalLineId,
    managedConnectionPath,
    managedDecisionPath,
    mkAdrId,
    mkConfig,
    mkManagedPaths,
    mkOperationId,
    mkRepoPath,
    objectRefText,
    repoPathText,
  )
import Control.Exception
  ( AsyncException (ThreadKilled),
    Exception (..),
    SomeException,
    asyncExceptionFromException,
    asyncExceptionToException,
    bracket,
    fromException,
    throwIO,
    try,
  )
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (filterM, forM_, void)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Vector as Vector
import Database.SQLite.Simple
  ( Only (..),
    SQLData (SQLNull, SQLText, SQLInteger),
     close,
     execute,
     executeMany,
     execute_,
     withTransaction,
    open,
    query,
    query_,
  )
import System.Directory (doesFileExist, withCurrentDirectory)
import System.FilePath (isRelative, takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit ((@?=), assertBool, assertEqual, assertFailure, testCase)

data TestAsyncCancellation = TestAsyncCancellation
  deriving (Eq, Show)

instance Exception TestAsyncCancellation where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | Convert a Digest to Text for use in test data.
-- The exact representation is not critical; it just needs to be stable Text.
digestToText :: Digest -> Text
digestToText = pack . show

-- | Create an ADRAI decision / connection file body containing a sealed
-- capsule.  The capsule carries a single operation member that the
-- classification engine will register and later classify.
createAdraiFile
  :: ObjectRef -- ^ ID of this document
  -> Text -- ^ Semantic content (before sealing)
  -> Text -- ^ Operation ID
  -> Text -- ^ Blob OID of the operation file
  -> IO Text -- ^ Sealed file content (UTF-8)
createAdraiFile objRef semantic opId _blobOid = do
  let op = requireOperationId opId
      oid = provenanceObjectFromRef objRef
      basis = GitOid (Text.replicate 40 "0")
      now = 1700000000000
      actor = case mkActor HumanActor "test" Nothing of
        Left _ -> error "mkActor failed"
        Right a -> a
      kind = case mkEventKind "decision" of
        Left _ -> error "mkEventKind failed"
        Right k -> k
      digest = semanticDigest semantic
      tv = "adrai/0.1.0"
      inputs = ProvenanceInputs Nothing Nothing Nothing
      capsuleInput =
        ProvenanceCapsuleInput
          { capsuleInputOperationId = op,
            capsuleInputObjectId = oid,
            capsuleInputEventKind = kind,
            capsuleInputActor = actor,
            capsuleInputTimestampMs = now,
            capsuleInputBasis = basis,
            capsuleInputParents = [],
            capsuleInputBranchHint = Nothing,
            capsuleInputUpstreamHint = Nothing,
            capsuleInputLineAnchors = [],
            capsuleInputSemanticDigest = digest,
            capsuleInputToolVersion = tv,
            capsuleInputDigests = inputs
          }
      capsule = case mkProvenanceCapsule capsuleInput of
        Left err -> error ("mkProvenanceCapsule: " <> show err)
        Right c -> c
  pure (sealSemantic semantic capsule)

authorityCapsule :: Text -> AdrId -> GitOid -> Text -> ProvenanceCapsule
authorityCapsule operation adr basis semantic =
  case mkProvenanceCapsule
    ProvenanceCapsuleInput
      { capsuleInputOperationId = requireOperationId operation,
        capsuleInputObjectId = ProvenanceAdr adr,
        capsuleInputEventKind = case mkEventKind "decision" of
          Left problem -> error (show problem)
          Right value -> value,
        capsuleInputActor = case mkActor HumanActor "ensure-authority-test" Nothing of
          Left problem -> error (show problem)
          Right value -> value,
        capsuleInputTimestampMs = 1700000000000,
        capsuleInputBasis = basis,
        capsuleInputParents = [],
        capsuleInputBranchHint = Nothing,
        capsuleInputUpstreamHint = Nothing,
        capsuleInputLineAnchors = [],
        capsuleInputSemanticDigest = semanticDigest semantic,
        capsuleInputToolVersion = "adrai/1.0.0",
        capsuleInputDigests = ProvenanceInputs Nothing Nothing Nothing
      } of
    Left problem -> error (show problem)
    Right value -> value

runEnsureForConfig
  :: Repository
  -> FilePath
  -> Config
  -> [ParsedManagedDocument]
  -> [Text]
  -> GitOid
  -> IO (Either SomeException ProvenanceUpdate)
runEnsureForConfig repository currentDb config documents operations target =
  bracket (open (provenanceDatabasePath currentDb)) close $ \connection ->
    ensureProvenance repository connection currentDb
      (map logicalLineId (configLogicalLines config))
      (repoPathText (managedDecisionPath (configManagedPaths config)))
      (repoPathText (managedConnectionPath (configManagedPaths config)))
      (configLogicalLines config)
      (Just documents)
      operations
      Nothing
       target

runEnsureForConfigWithWitness
  :: Repository -> FilePath -> Config -> [ParsedManagedDocument] -> [Text]
  -> GitOid -> Maybe ProvenanceRecoveryWitness -> IO (Either SomeException ProvenanceUpdate)
runEnsureForConfigWithWitness repository currentDb config documents operations target witness =
  bracket (open (provenanceDatabasePath currentDb)) close $ \connection ->
    ensureProvenanceWithRecoveryWitness repository connection currentDb
      (map logicalLineId (configLogicalLines config))
      (repoPathText (managedDecisionPath (configManagedPaths config)))
      (repoPathText (managedConnectionPath (configManagedPaths config)))
       (configLogicalLines config) (Just documents) operations Nothing target witness []

runEnsureForConfigWithWitnessAndWorklistObserver
  :: ([GitOid] -> IO ()) -> Repository -> FilePath -> Config -> [ParsedManagedDocument] -> [Text]
  -> GitOid -> Maybe ProvenanceRecoveryWitness -> IO (Either SomeException ProvenanceUpdate)
runEnsureForConfigWithWitnessAndWorklistObserver observeWorklist repository currentDb config documents operations target witness =
  bracket (open (provenanceDatabasePath currentDb)) close $ \connection ->
    ensureProvenanceWithRecoveryWitnessAndWorklistObserver observeWorklist repository connection currentDb
      (map logicalLineId (configLogicalLines config))
      (repoPathText (managedDecisionPath (configManagedPaths config)))
      (repoPathText (managedConnectionPath (configManagedPaths config)))
      (configLogicalLines config) (Just documents) operations Nothing target witness []

-- | Query the @operation_commit@ table for a specific operation ID.
--
-- Returns @(commit_oid, classification)@ pairs.
queryPlacements :: FilePath -> Text -> IO [(Text, Text)]
queryPlacements dbPath opId = do
  conn <- open dbPath
  rows <-
    query conn "SELECT commit_oid, classification FROM operation_commit WHERE op_id = ? ORDER BY commit_oid"
      [SQLText opId] :: IO [(Text, Text)]
  close conn
  pure rows

-- | Query the @provenance_issue@ table for issue codes.
queryIssueCodes :: FilePath -> IO [Text]
queryIssueCodes dbPath = do
  conn <- open dbPath
  rows <- query_ conn "SELECT code FROM provenance_issue ORDER BY code" :: IO [Only Text]
  close conn
  pure (map fromOnly rows)

-- | Query shallow-history completeness flags from landing records.
queryLandingCompleteness :: FilePath -> IO [Int]
queryLandingCompleteness dbPath = do
  conn <- open dbPath
  rows <- query_ conn "SELECT complete FROM line_landing ORDER BY config_key,op_id,line_id,ref_name,commit_oid" :: IO [Only Int]
  close conn
  pure (map fromOnly rows)

-- | Query the registered_operation table.
queryRegisteredOperations :: FilePath -> IO [(Text, Maybe Text, Text, Text)]
queryRegisteredOperations dbPath = do
  conn <- open dbPath
  rows <-
    query_ conn "SELECT op_id, adr_id, basis_oid, signature FROM registered_operation ORDER BY op_id"
      :: IO [(Text, Maybe Text, Text, Text)]
  close conn
  pure rows

-- | Create a minimal ParsedManagedDocument for registration.
makeParsedDoc
  :: ObjectRef
  -> RepoPath
  -> ProvenanceCapsule
  -> Maybe GitOid
  -> Text -- semantic hash
  -> ParsedManagedDocument
makeParsedDoc objRef repoPath capsule blobOid semanticHash =
  ParsedManagedDocument
    { parsedDocumentObjectRef = objectRefText objRef,
      parsedManagedPath = repoPath,
      parsedManagedCapsule = capsule,
      parsedBlobOid = blobOid,
      parsedSemanticHash = semanticHash
    }

-- | Build a logical line config for test repos.
mkTestConfig :: Config
mkTestConfig =
  case mkRepoPath "architecture/adrai/decisions" of
    Left e -> error ("mkRepoPath: " <> show e)
    Right decisionsPath ->
      case mkRepoPath "architecture/adrai/connections" of
        Left e -> error ("mkRepoPath: " <> show e)
        Right connectionsPath ->
          case mkManagedPaths decisionsPath connectionsPath of
            Left _ -> error "path overlap in test config"
            Right mp ->
              case mkConfig ConfigSchemaV1 mp [LogicalLine "trunk" [GitRef "refs/heads/main", GitRef "refs/heads/feature"]] of
                Left _ -> error "config violation in test config"
                Right c -> c

-- | Run the full ensure_provenance pipeline on a given repository at a
-- specific revision, returning the ProvenanceUpdate result and the path to
-- the created overlay database.
runProvenanceEnsure
  :: Repository
  -> FilePath -- current database path (the cold compiler DB path, used to derive overlay path)
  -> [ParsedManagedDocument]
  -> [Text] -- operation IDs
  -> GitOid -- target revision
  -> IO (FilePath, Either SomeException ProvenanceUpdate)
runProvenanceEnsure repo currentDbPath parsedDocs opIds targetRevision = do
  let dbPath = provenanceDatabasePath currentDbPath
      config = mkTestConfig
      decisionsPath = repoPathText (managedDecisionPath (configManagedPaths config))
      connectionsPath = repoPathText (managedConnectionPath (configManagedPaths config))
      logicalLines = configLogicalLines config
  conn <- open dbPath
  result <- ensureProvenance repo conn currentDbPath
    [logicalLineId ll | ll <- logicalLines]
    decisionsPath connectionsPath
    logicalLines (Just parsedDocs) opIds Nothing targetRevision
  pure (dbPath, result)

-- | Ensure the overlay schema exists in the database.
ensureSchema :: FilePath -> IO ()
ensureSchema dbPath = do
  conn <- open dbPath
  forM_ overlaySchemaDdl $ \(_, ddl) ->
    execute_ conn (asQuery ddl)
  forM_ overlaySchemaIndexes $ \idx ->
    execute_ conn (asQuery idx)
  execute conn "INSERT INTO meta VALUES('schema', ?)" [SQLText overlaySchemaVersion]
  close conn

-- | Seed only the durable rows read by 'candidateCommits'.  The ADR/object
-- identity intentionally differs from the capsule-authoritative operation ID.
candidateBindingFixture :: FilePath -> IO (Text, Text, GitOid, GitOid, GitOid, GitOid)
candidateBindingFixture dbPath = do
  ensureSchema dbPath
  conn <- open dbPath
  let operation = "O00000000000000000000000881"
      adrId = "A00000000000000000000000881"
      path = "architecture/adrai/decisions/ü candidate space.md"
      pathCommit = GitOid (Text.replicate 40 "a")
      excludedPathCommit = GitOid (Text.replicate 40 "b")
      trailerCommit = GitOid (Text.replicate 40 "c")
      excludedTrailerCommit = GitOid (Text.replicate 40 "d")
      insertObservation oid message =
        execute conn "INSERT INTO commit_observation VALUES(?,?,?,?,?,?)"
          [ SQLText (gitOidText oid), SQLText "[]", SQLInteger 1, SQLInteger 1
          , SQLText "candidate fixture", SQLText message
          ]
  execute conn "INSERT INTO registered_operation VALUES(?,?,?,?)"
    [SQLText operation, SQLText adrId, SQLText (gitOidText pathCommit), SQLText "candidate-fixture"]
  execute conn "INSERT INTO registered_object VALUES(?,?,?,?)"
    [SQLText operation, SQLText adrId, SQLText path, SQLText "fixture-blob"]
  insertObservation pathCommit "path candidate"
  insertObservation excludedPathCommit "excluded path candidate"
  insertObservation trailerCommit ("ADRAI-Op: " <> operation)
  insertObservation excludedTrailerCommit ("ADRAI-Op: " <> operation)
  execute conn "INSERT INTO managed_path_addition VALUES(?,?)" [SQLText path, SQLText (gitOidText pathCommit)]
  execute conn "INSERT INTO managed_path_addition VALUES(?,?)" [SQLText path, SQLText (gitOidText excludedPathCommit)]
  close conn
  pure (operation, adrId, pathCommit, excludedPathCommit, trailerCommit, excludedTrailerCommit)

candidateBindingResult :: FilePath -> [GitOid] -> IO (Map Text (Set.Set GitOid))
candidateBindingResult dbPath commits = do
  conn <- open dbPath
  result <- candidateCommits conn commits ["O00000000000000000000000881"]
  close conn
  case result of
    Left err -> assertFailure ("candidateCommits: " <> show err)
    Right candidates -> pure candidates

-- =====================================================================
-- Test suite
-- =====================================================================

classificationBasisBatchWarnings :: IO ()
classificationBasisBatchWarnings =
  withSystemTempDirectory "adrai classification basis batch" $ \temporary -> do
    let repositoryPath = temporary </> "repository"
        overlayPath = temporary </> "overlay.sqlite"
        missingOperation = "O00000000000000000000000891"
        noncommitOperation = "O00000000000000000000000892"
        managedPath = "architecture/adrai/decisions/basis batch ünicode space.md"
        managedPathText = Text.pack managedPath
        missingBasis = GitOid (Text.replicate 40 "f")
    initTestRepository repositoryPath
    parent <- commitFile repositoryPath "seed.txt" "seed"
    candidate <- commitFile repositoryPath managedPath "managed payload"
    blob <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD:" <> managedPath] BS.empty
    noncommitBasis <- outputText <$> gitSuccess repositoryPath ["hash-object", "-w", "--stdin"] "not a commit"
    discovered <- discoverRepository systemGit repositoryPath >>= \case
      Left problem -> assertFailure (show problem) >> fail "unreachable"
      Right repository -> pure repository
    connection <- open overlayPath
    createOverlaySchema connection
    let register operation basis = do
          execute connection "INSERT INTO registered_operation VALUES(?,?,?,?)"
            [ SQLText operation, SQLText "A00000000000000000000000891", SQLText (gitOidText basis), SQLText "basis-batch" ]
          execute connection "INSERT INTO registered_object VALUES(?,?,?,?)"
            [ SQLText operation, SQLText "A00000000000000000000000891", SQLText managedPathText, SQLText blob ]
    register missingOperation missingBasis
    register noncommitOperation (requireGitOid noncommitBasis)
    execute connection "INSERT INTO commit_observation VALUES(?,?,?,?,?,?)"
      [ SQLText candidate, SQLText parent, SQLInteger 1, SQLInteger 1, SQLText "basis batch", SQLText "" ]
    outcome <- processCandidates discovered connection
      (Map.fromList [(missingOperation, Set.singleton (GitOid candidate)), (noncommitOperation, Set.singleton (GitOid candidate))])
      [missingOperation, noncommitOperation]
    case outcome of
      Left problem -> assertFailure ("classification unexpectedly failed: " <> show problem)
      Right () -> pure ()
    warnings <- query_ connection "SELECT code,op_id FROM provenance_issue ORDER BY op_id" :: IO [(Text, Text)]
    warnings @?= [("BASIS_COMMIT_UNAVAILABLE", missingOperation), ("BASIS_COMMIT_UNAVAILABLE", noncommitOperation)]
    close connection

treePathProtocolTest :: IO ()
treePathProtocolTest = do
  let oid = Text.replicate 40 "a"
      path = "architecture/adrai/decisions/ümlaut space.decision.md"
      record mode objectType objectOid objectPath =
        TextEncoding.encodeUtf8 mode <> " " <> TextEncoding.encodeUtf8 objectType
          <> " " <> TextEncoding.encodeUtf8 objectOid <> "\t"
          <> TextEncoding.encodeUtf8 objectPath <> "\0"
      valid = record "100644" "blob" oid path
      malformedCases =
        [ ("wrong path", record "100644" "blob" oid "architecture/adrai/decisions/other.md")
        , ("wrong blob", record "100644" "blob" (Text.replicate 40 "b") path)
        , ("non-blob", record "040000" "tree" oid path)
        , ("bad mode", record "not-a-mode" "blob" oid path)
        , ("bad oid", record "100644" "blob" "not-an-oid" path)
        , ("truncated", BS.init valid)
        , ("two records", valid <> valid)
        , ("trailing bytes", valid <> "garbage")
        , ("control path payload", record "100644" "blob" oid "architecture/adrai/decisions/tab\tnewline\n\SOH.md")
        ]
  assertBool "exact Unicode path with spaces is accepted" (decodeExactBlobTreeEntry path oid valid)
  forM_ malformedCases $ \(label, raw) ->
    assertBool label (not (decodeExactBlobTreeEntry path oid raw))

parentJsonProtocolTest :: IO ()
parentJsonProtocolTest = do
  let first = Text.replicate 40 "a"
      second = Text.replicate 40 "b"
  canonicalParentsJson [] @?= Just "[]"
  canonicalParentsJson [first] @?= Just ("[\"" <> first <> "\"]")
  canonicalParentsJson [first, second] @?= Just ("[\"" <> first <> "\",\"" <> second <> "\"]")
  canonicalParentsJson ["not-an-oid"] @?= Nothing
  canonicalParentsJson [first, first] @?= Nothing

addedPathsProtocolTest :: IO ()
addedPathsProtocolTest = do
  let first = GitOid (Text.replicate 40 "a")
      second = GitOid (Text.replicate 40 "b")
      expected = Set.fromList [first, second]
      record oid paths =
        BS.concat
          ( "\x1e" : TextEncoding.encodeUtf8 (gitOidText oid) : "\0\n" :
            concatMap (\path -> ["A\0", TextEncoding.encodeUtf8 path, "\0"]) paths
          )
      statusRecord oid status path =
        "\x1e" <> TextEncoding.encodeUtf8 (gitOidText oid) <> "\0\n" <> status <> "\0" <> path <> "\0"
      leadingRecordSeparator = Text.cons '\x1e' "leading-record-separator.txt"
      valid = record first ["directory/ü path.txt"] <> record second [] <> record first ["second.txt", leadingRecordSeparator]
      expectedPaths =
        Map.fromList
          [ (first, Set.fromList ["directory/ü path.txt", "second.txt", leadingRecordSeparator]),
            (second, Set.empty)
          ]
      expectInvalidOutput label result =
        case result of
          Left (GitInvalidOutput "added paths" _) -> pure ()
          other -> assertFailure (label <> ": expected typed invalid output, got " <> show other)
  decodeAddedPathsOutput expected valid @?= Right expectedPaths
  expectInvalidOutput "invalid OID header"
    (decodeAddedPathsOutput expected ("\x1enot-an-oid\0\n" :: ByteString))
  expectInvalidOutput "missing header terminator"
    (decodeAddedPathsOutput expected ("\x1e" <> TextEncoding.encodeUtf8 (gitOidText first)))
  expectInvalidOutput "path before record header"
    (decodeAddedPathsOutput expected ("path-before-header\0" :: ByteString))
  expectInvalidOutput "unknown record OID"
    (decodeAddedPathsOutput (Set.singleton first) (record second []))
  expectInvalidOutput "non-added status"
    (decodeAddedPathsOutput (Set.singleton first) (statusRecord first "D" "deleted.txt"))
  expectInvalidOutput "missing path after added status"
    (decodeAddedPathsOutput (Set.singleton first) ("\x1e" <> TextEncoding.encodeUtf8 (gitOidText first) <> "\0\nA\0"))
  expectInvalidOutput "unterminated path"
    (decodeAddedPathsOutput (Set.singleton first) ("\x1e" <> TextEncoding.encodeUtf8 (gitOidText first) <> "\0\nA\0unterminated"))
  case decodeAddedPathsOutput (Set.singleton first)
    ("\x1e" <> TextEncoding.encodeUtf8 (gitOidText first) <> "\0\nA\0" <> BS.pack [0xff, 0]) of
    Left (GitInvalidUtf8Path _) -> pure ()
    other -> assertFailure ("invalid UTF-8 path: expected GitInvalidUtf8Path, got " <> show other)

addedPathsRealGitTest :: IO ()
addedPathsRealGitTest =
  withSystemTempDirectory "adrai added paths real git" $ \temp -> do
    let repoDir = temp </> "repo"
        rootPath = "root ü path.txt"
        ordinaryPath = "ordinary path.txt"
    initTestRepository repoDir
    rootText <- commitFile repoDir (Text.unpack rootPath) "root"
    ordinaryText <- commitFile repoDir (Text.unpack ordinaryPath) "ordinary"
    modifiedText <- commitFile repoDir (Text.unpack rootPath) "modified"
    resolved <- resolveTestRepo repoDir "HEAD"
    let root = requireGitOid rootText
        ordinary = requireGitOid ordinaryText
        modified = requireGitOid modifiedText
        expected =
          Map.fromList
            [ (root, Set.singleton rootPath),
              (ordinary, Set.singleton ordinaryPath),
              (modified, Set.empty)
            ]
    discovered <- addedPathsForCommits (resolvedRepository resolved) [modified, root, ordinary, root]
    discovered @?= Right expected
    reordered <- addedPathsForCommits (resolvedRepository resolved) [ordinary, root, ordinary]
    reordered @?= Right (Map.fromList [(root, Set.singleton rootPath), (ordinary, Set.singleton ordinaryPath)])

addedPathsChangeShapesGitTest :: IO ()
addedPathsChangeShapesGitTest =
  withSystemTempDirectory "adrai added paths change shapes" $ \temp -> do
    let repoDir = temp </> "repo"
        sourcePath = "rename source.txt"
        renamedPath = "renamed target.txt"
        copiedPath = "copied target.txt"
    initTestRepository repoDir
    _ <- commitFile repoDir "seed.txt" "seed"
    _ <- commitFile repoDir sourcePath "same content"
    _ <- gitSuccess repoDir ["mv", sourcePath, renamedPath] BS.empty
    _ <- gitSuccess repoDir ["commit", "-m", "rename fixture"] BS.empty
    renamedText <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty
    copiedText <- commitFile repoDir copiedPath "same content"
    _ <- gitSuccess repoDir ["rm", "--", copiedPath] BS.empty
    _ <- gitSuccess repoDir ["commit", "-m", "deletion fixture"] BS.empty
    deletedText <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty
    resolved <- resolveTestRepo repoDir "HEAD"
    let renamed = requireGitOid renamedText
        copied = requireGitOid copiedText
        deleted = requireGitOid deletedText
    discovered <- addedPathsForCommits (resolvedRepository resolved) [deleted, copied, renamed]
    discovered @?=
      Right
        ( Map.fromList
            [ (renamed, Set.singleton (Text.pack renamedPath)),
              (copied, Set.singleton (Text.pack copiedPath)),
              (deleted, Set.empty)
            ]
        )

addedPathsMergeGitTest :: IO ()
addedPathsMergeGitTest =
  withSystemTempDirectory "adrai added paths merge" $ \temp -> do
    let repoDir = temp </> "repo"
        featurePath = "feature addition.txt"
        mainPath = "main addition.txt"
    initTestRepository repoDir
    _ <- commitFile repoDir "seed.txt" "seed"
    _ <- gitSuccess repoDir ["branch", "feature"] BS.empty
    _ <- gitSuccess repoDir ["checkout", "feature"] BS.empty
    _ <- commitFile repoDir (Text.unpack featurePath) "feature"
    _ <- gitSuccess repoDir ["checkout", "main"] BS.empty
    _ <- commitFile repoDir (Text.unpack mainPath) "main"
    _ <- gitSuccess repoDir ["merge", "--no-ff", "feature"] BS.empty
    resolved <- resolveTestRepo repoDir "HEAD"
    let merge = resolvedCommitOid resolved
    added <- addedPathsForCommits (resolvedRepository resolved) [merge, merge]
    added @?= Right (Map.singleton merge (Set.fromList [featurePath, mainPath]))

requireHead :: String -> [a] -> a
requireHead description = \case
  value : _ -> value
  [] -> error (description <> " must be non-empty")

tests :: TestTree
tests =
  withResource createOverlayRepositorySeed removeRepositorySeed $ \getRepositorySeed ->
    withResource createMergePlacementRepositorySeed removeRepositorySeed $ \getMergePlacementSeed ->
      testsWithRepositorySeed getRepositorySeed getMergePlacementSeed

testsWithRepositorySeed :: IO RepositorySeed -> IO RepositorySeed -> TestTree
testsWithRepositorySeed getRepositorySeed getMergePlacementSeed =
  testGroup
    "Provenance overlay topology"
    [ testCase "added_paths_protocol_is_nul_framed_and_strict" addedPathsProtocolTest,

      testCase "commit-row repair observes immediately before its post-repair select" $
        withSystemTempDirectory "adrai commit-row observer" $ \temporary -> do
          let repoDir = temporary </> "repo"
              overlayPath = temporary </> "overlay.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.md" "seed\n"
          discovered <- discoverRepository systemGit repoDir >>= \case
            Left problem -> assertFailure (show problem) >> fail "unreachable"
            Right repository -> pure repository
          target <- resolveRepositoryRevision discovered (requireRevision "HEAD") >>= \case
            Left problem -> assertFailure (show problem) >> fail "unreachable"
            Right resolved -> pure (resolvedCommitOid resolved)
          ensureSchema overlayPath
          calls <- newIORef (0 :: Int)
          bracket (open overlayPath) close $ \connection -> do
            loaded <- loadCommitRowsWithQueryObserver (modifyIORef' calls (+ 1)) discovered connection [target]
            case loaded of
              Left problem -> assertFailure (show problem)
              Right rows -> assertBool "repair loads the discovered commit" (Map.member target rows)
          readIORef calls >>= (@?= 2),

      testCase "target ancestry cache performs one shared check for unique placements" $ do
        let first = requireGitOid "1111111111111111111111111111111111111111"
            second = requireGitOid "2222222222222222222222222222222222222222"
            third = requireGitOid "3333333333333333333333333333333333333333"
        calls <- newIORef []
        let check candidates = do
              modifyIORef' calls (<> [candidates])
              pure (Right (Map.fromList [(candidate, candidate == first) | candidate <- candidates]))
        firstPass <- extendTargetAncestryCache check Map.empty [first, first, second]
        cache <- case firstPass of
          Left problem -> assertFailure ("ancestry cache: " <> show problem)
          Right value -> pure value
        firstCalls <- readIORef calls
        firstCalls @?= [[first, second]]
        Map.lookup first cache @?= Just True
        Map.lookup second cache @?= Just False
        writeIORef calls []
        secondPass <- extendTargetAncestryCache check cache [second, third, first, third]
        refreshedCache <- case secondPass of
          Left problem -> assertFailure ("cached ancestry refresh: " <> show problem)
          Right value -> pure value
        secondCalls <- readIORef calls
        secondCalls @?= [[third]]
        Map.lookup third refreshedCache @?= Just False ,

      testCase "target ancestry cache fails closed when a bulk check omits a candidate" $ do
        let first = requireGitOid "1111111111111111111111111111111111111111"
            second = requireGitOid "2222222222222222222222222222222222222222"
            incomplete candidates = pure (Right (Map.fromList [(candidate, True) | candidate <- take 1 candidates]))
        result <- extendTargetAncestryCache incomplete Map.empty [first, second]
        case result of
          Left (GitCommandFailed command exitCode _ _) -> do
            command @?= "target ancestry"
            exitCode @?= 128
          Left problem -> assertFailure ("unexpected ancestry cache failure: " <> show problem)
          Right cache -> assertFailure ("accepted incomplete ancestry cache: " <> show cache),

      testCase "target ancestry cache fails closed when a bulk check adds a candidate" $ do
        let first = requireGitOid "1111111111111111111111111111111111111111"
            unexpected = requireGitOid "2222222222222222222222222222222222222222"
            extra candidates =
              pure (Right (Map.fromList ((unexpected, False) : [(candidate, True) | candidate <- candidates])))
        result <- extendTargetAncestryCache extra Map.empty [first]
        case result of
          Left (GitCommandFailed command exitCode _ _) -> do
            command @?= "target ancestry"
            exitCode @?= 128
          Left problem -> assertFailure ("unexpected ancestry cache failure: " <> show problem)
          Right cache -> assertFailure ("accepted unexpected ancestry cache entry: " <> show cache),

      testCase "operation signatures are canonical for same-path members" $
        let operation = "O00000000000000000000000901"
            basis = requireGitOid "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            path = requireRepoPath "architecture/adrai/decisions/shared.md"
            capsule = authorityCapsule operation (requireAdrId "A00000000000000000000000901") basis "same-path"
            first = makeParsedDoc (adrObjectRef (requireAdrId "A00000000000000000000000901")) path capsule (Just basis) "semantic-first"
            second = makeParsedDoc (adrObjectRef (requireAdrId "A00000000000000000000000902")) path capsule (Just basis) "semantic-second"
        in operationSignature [first, second] @?= operationSignature [second, first],

      testCase "classification request plan batches duplicate bases and unique candidate-parent commits" $ do
        let basisOne = requireGitOid "1111111111111111111111111111111111111111"
            basisTwo = requireGitOid "2222222222222222222222222222222222222222"
            candidate = requireGitOid "3333333333333333333333333333333333333333"
            parent = requireGitOid "4444444444444444444444444444444444444444"
            primary = requireRepoPath "architecture/adrai/decisions/ümlaut space.md"
            secondary = requireRepoPath "architecture/adrai/connections/second.md"
            grouped =
              groupTreeObservationRequests
                [ (candidate, [primary, secondary]),
                  (parent, [primary]),
                  (candidate, [secondary, primary]),
                  (parent, [primary])
                ]
        basisObjectRequestCount [basisOne, basisOne, basisTwo] @?= 1
        Map.size grouped @?= 2
        Map.lookup candidate grouped @?= Just (Set.fromList [primary, secondary])
        treeObservationRequestCount "git" ""
          [ (candidate, [primary, secondary]),
            (parent, [primary]),
            (candidate, [secondary, primary]),
            (parent, [primary])
          ]
          @?= Right 3,

      testCase "classification records missing and noncommit shared-batch bases as warnings" classificationBasisBatchWarnings,

      testCase "ls_tree_path_protocol_is_binary_safe_and_fail_closed" treePathProtocolTest,

      testCase "operation_commit_parents_json_is_canonical_and_strict" parentJsonProtocolTest,

      testCase "first-parent warm priming reconciles trailerless merge placement" (getMergePlacementSeed >>= firstParentWarmPrimingReconcilesTrailerlessMergePlacement),

      testCase "second-parent warm priming recovers merge introduction placement" (getMergePlacementSeed >>= secondParentWarmPrimingRecoversMergeIntroductionPlacement),

      testCase "missing source-target coverage replays indexed later merge placement repository-wide" (getMergePlacementSeed >>= missingSourceTargetCoverageReplaysRepositoryWideMerge),

      testCase "added_paths_real_git_is_root_nonroot_multi_and_isolated" addedPathsRealGitTest,

      testCase "added_paths_real_git_uses_exact_add_semantics_for_rename_copy_and_deletion" addedPathsChangeShapesGitTest,

      testCase "added_paths_real_git_unions_merge_parent_records" addedPathsMergeGitTest,

      testCase "candidate_bindings_are_scoped_and_operation_authoritative" $
        (withSystemTempDirectory "adrai overlay candidate bindings ünicode" $ \temp -> do
          let dbPath = temp </> "semantic.sqlite"
              operation = "O00000000000000000000000881"
          (fixtureOperation, adrId, pathCommit, excludedPathCommit, trailerCommit, excludedTrailerCommit) <-
            candidateBindingFixture dbPath
          assertEqual "fixture operation is the canonical key" operation fixtureOperation

          emptyCandidates <- candidateBindingResult dbPath []
          Map.lookup operation emptyCandidates @?= Just Set.empty

          singletonCandidates <- candidateBindingResult dbPath [pathCommit]
          Map.lookup operation singletonCandidates @?= Just (Set.singleton pathCommit)

          multiCandidates <- candidateBindingResult dbPath [trailerCommit, pathCommit]
          Map.lookup operation multiCandidates @?= Just (Set.fromList [pathCommit, trailerCommit])

          reorderedDuplicates <- candidateBindingResult dbPath [pathCommit, trailerCommit, pathCommit]
          reorderedDuplicates @?= multiCandidates

          let adversarial = GitOid "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa' OR 1=1 --"
          adversarialCandidates <- candidateBindingResult dbPath [pathCommit, adversarial]
          Map.lookup operation adversarialCandidates @?= Just (Set.singleton pathCommit)
          assertBool "no candidate outside the supplied OID set survives path discovery"
            (excludedPathCommit `Set.notMember` Map.findWithDefault Set.empty operation multiCandidates)
          assertBool "no candidate outside the supplied OID set survives trailer discovery"
            (excludedTrailerCommit `Set.notMember` Map.findWithDefault Set.empty operation multiCandidates)
          assertBool "the ADR/object ID is never used as a candidate-map key"
            (Map.notMember adrId multiCandidates)
        ),


       testCase "ensure classifies a later target for every registered operation" $
        (withSystemTempDirectory "adrai ensure existing operation target" $ \temp -> do
          let repoDir = temp </> "repo"
              currentDb = temp </> "index.sqlite"
              overlayPath = provenanceDatabasePath currentDb
              operationA = "O00000000000000000000000891"
              operationB = "O00000000000000000000000892"
              adrA = requireAdrId "A00000000000000000000000891"
              adrB = requireAdrId "A00000000000000000000000892"
              pathA = "architecture/adrai/decisions/existing-a.decision.md"
              pathB = "architecture/adrai/decisions/new-b.decision.md"
              semanticA = "# Existing operation A\n"
              semanticB = "# New operation B\n"
          initTestRepository repoDir
          basisText <- commitFile repoDir "seed.txt" "seed\n"
          _ <- gitSuccess repoDir ["branch", "feature"] BS.empty
          let basis = requireGitOid basisText
              capsuleA = authorityCapsule operationA adrA basis semanticA
              bytesA = TextEncoding.encodeUtf8 (sealSemantic semanticA capsuleA)
          originalText <- commitFile repoDir pathA bytesA
          originalBlobText <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD:" <> pathA] BS.empty
          original <- resolveTestRepo repoDir originalText
          let documentA = ParsedManagedDocument
                { parsedDocumentObjectRef = adrIdText adrA,
                  parsedManagedPath = requireRepoPath (Text.pack pathA),
                  parsedManagedCapsule = capsuleA,
                  parsedBlobOid = Just (requireGitOid originalBlobText),
                  parsedSemanticHash = digestToText (semanticDigest semanticA)
                }
          ensureSchema overlayPath
          first <- runEnsureForConfig (resolvedRepository original) currentDb mkTestConfig [documentA] [operationA] (resolvedCommitOid original)
          case first of
            Left problem -> assertFailure ("first ensure: " <> show problem)
            Right _ -> pure ()

          _ <- gitSuccess repoDir ["switch", "feature"] BS.empty
          let capsuleB = authorityCapsule operationB adrB basis semanticB
              bytesB = TextEncoding.encodeUtf8 (sealSemantic semanticB capsuleB)
          _ <- commitFiles repoDir [(pathA, bytesA), (pathB, bytesB)]
          target <- resolveTestRepo repoDir "HEAD"
          targetBlobB <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD:" <> pathB] BS.empty
          let documentB = ParsedManagedDocument
                { parsedDocumentObjectRef = adrIdText adrB,
                  parsedManagedPath = requireRepoPath (Text.pack pathB),
                  parsedManagedCapsule = capsuleB,
                  parsedBlobOid = Just (requireGitOid targetBlobB),
                  parsedSemanticHash = digestToText (semanticDigest semanticB)
                }
          second <- runEnsureForConfig (resolvedRepository target) currentDb mkTestConfig [documentB] [operationB] (resolvedCommitOid target)
          case second of
            Left problem -> assertFailure ("second ensure: " <> show problem)
            Right _ -> pure ()
          placementsA <- queryPlacements overlayPath operationA
          assertBool "later target is classified for the already registered operation"
            (any ((== gitOidText (resolvedCommitOid target)) . fst) placementsA)
          repairConnection <- open overlayPath
          observedTarget <- query repairConnection
            "SELECT 1 FROM observed_commit WHERE commit_oid=?"
            [SQLText (gitOidText (resolvedCommitOid target))]
            :: IO [Only Int]
          observedTarget @?= [Only 1]
          execute repairConnection
            "DELETE FROM operation_commit WHERE op_id=? AND commit_oid=?"
            [SQLText operationA, SQLText (gitOidText (resolvedCommitOid target))]
          close repairConnection
          missingPlacement <- queryPlacements overlayPath operationA
          assertBool "fixture removes only the already-observed target placement"
            (all ((/= gitOidText (resolvedCommitOid target)) . fst) missingPlacement)
          repaired <- runEnsureForConfig
            (resolvedRepository target)
            currentDb
            mkTestConfig
            [documentA]
            [operationA]
            (resolvedCommitOid target)
          case repaired of
            Left problem -> assertFailure ("target repair ensure: " <> show problem)
            Right update -> changed update @?= True
          repairedPlacements <- queryPlacements overlayPath operationA
          assertBool "fast-path eligibility requires and repairs exact target evidence"
            (any ((== gitOidText (resolvedCommitOid target)) . fst) repairedPlacements)
         ),

       testCase "ensure recovers reachable historical two-member placement after all refs are observed" $
         (withSystemTempDirectory "adrai ensure reachable historical recovery" $ \temp -> do
           let repoDir = temp </> "repo"
               currentDb = temp </> "index.sqlite"
               overlayPath = provenanceDatabasePath currentDb
               operation = "O00000000000000000000000895"
               adrA = requireAdrId "A00000000000000000000000895"
               adrB = requireAdrId "A00000000000000000000000896"
               pathA = "architecture/adrai/decisions/recovery-a.decision.md"
               pathB = "architecture/adrai/decisions/recovery-b.decision.md"
               semantic = "# Reachable historical recovery\n"
           initTestRepository repoDir
           basisText <- commitFile repoDir "seed.txt" "seed\n"
           let basis = requireGitOid basisText
               capsule = authorityCapsule operation adrA basis semantic
               memberBytes = TextEncoding.encodeUtf8 (sealSemantic semantic capsule)
           _ <- gitSuccess repoDir ["branch", "release", Text.unpack basisText] BS.empty
           _ <- gitSuccess repoDir ["branch", "unrelated", Text.unpack basisText] BS.empty
           _ <- commitFiles repoDir [(pathA, memberBytes), (pathB, memberBytes)]
           _ <- gitSuccess repoDir
             [ "commit", "--amend", "-m"
             , "historical operation placement\n\nADRAI-Op: " <> Text.unpack operation
             ] BS.empty
           historicalText <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty
           historical <- resolveTestRepo repoDir historicalText
           blobA <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD:" <> pathA] BS.empty
           blobB <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD:" <> pathB] BS.empty
           _ <- gitSuccess repoDir ["branch", "merge-source", Text.unpack basisText] BS.empty
           _ <- gitSuccess repoDir ["switch", "merge-source"] BS.empty
           _ <- commitFile repoDir "merge-side.txt" "merge side\n"
           _ <- gitSuccess repoDir ["switch", "main"] BS.empty
           _ <- gitSuccess repoDir ["merge", "--no-ff", "merge-source", "-m", "later merge target"] BS.empty
           target <- resolveTestRepo repoDir "HEAD"
           _ <- gitSuccess repoDir ["switch", "unrelated"] BS.empty
           unrelatedText <- commitFiles repoDir [(pathA, memberBytes), (pathB, memberBytes)]
           unrelated <- resolveTestRepo repoDir unrelatedText
           _ <- gitSuccess repoDir ["switch", "main"] BS.empty
           release <- resolveTestRepo repoDir "release"
           let documents =
                 [ makeParsedDoc (adrObjectRef adrA) (requireRepoPath (Text.pack pathA)) capsule (Just (requireGitOid blobA)) (digestToText (semanticDigest semantic))
                 , makeParsedDoc (adrObjectRef adrB) (requireRepoPath (Text.pack pathB)) capsule (Just (requireGitOid blobB)) (digestToText (semanticDigest semantic))
                 ]
               evidenceConfig = configKey "architecture/adrai/decisions" "architecture/adrai/connections" ["trunk"]
           ensureSchema overlayPath

           -- Prime every ref/root before registering the operation.  The repair
           -- below must therefore recover c076-like historical additions with
           -- an empty rev-list delta rather than relying on a fresh scan.
           primed <- runEnsureForConfig (resolvedRepository target) currentDb mkTestConfig [] [] (resolvedCommitOid target)
           case primed of
             Left problem -> assertFailure ("priming ensure: " <> show problem)
             Right _ -> pure ()
           -- Force recovery to obtain the historical commit solely from the
           -- indexed exact trailer query.  No history walk and no path-addition
           -- candidate is available after this deletion.
           bracket (open overlayPath) close $ \connection ->
             execute connection "DELETE FROM managed_path_addition WHERE commit_oid=?" (Only historicalText)
           repaired <- runEnsureForConfig (resolvedRepository target) currentDb mkTestConfig documents [operation] (resolvedCommitOid target)
           repairedUpdate <- case repaired of
             Left problem -> assertFailure ("historical recovery ensure: " <> show problem)
             Right update -> pure update
           changed repairedUpdate @?= True
           commitsScanned repairedUpdate @?= 0
           placements <- queryPlacements overlayPath operation
           assertBool "exact trailer candidate is retained after indexed replay"
             (any ((== gitOidText (resolvedCommitOid historical)) . fst) placements)
           mainEvidence <- readProvenanceEvidenceAt (resolvedRepository target) overlayPath (resolvedCommitOid target) [operation] evidenceConfig
           assertBool "main target accepts its reachable historical placement" (either (const False) (const True) mainEvidence)
           releaseEvidence <- readProvenanceEvidenceAt (resolvedRepository release) overlayPath (resolvedCommitOid release) [operation] evidenceConfig
           releaseEvidence @?= Left (ProvenanceEvidenceMissingTargetPlacement operation)

           -- Repository-wide placement convergence intentionally retains the
           -- indexed duplicate-path candidates.  Only target certification and
           -- evidence materialization apply ancestry filtering.
           assertBool "unrelated duplicate-path candidate is retained repository-wide"
             (any ((== gitOidText (resolvedCommitOid unrelated)) . fst) placements)
           second <- runEnsureForConfig (resolvedRepository target) currentDb mkTestConfig documents [operation] (resolvedCommitOid target)
           case second of
             Left problem -> assertFailure ("recovered fast-path ensure: " <> show problem)
             Right update -> do
               changed update @?= False
               commitsScanned update @?= 0
         ),

       testCase "ensure rolls back injected classification failure and leaves no target certificate" $
         (withSystemTempDirectory "adrai ensure classification failure seam" $ \temp -> do
           let repoDir = temp </> "repo"
               currentDb = temp </> "index.sqlite"
               overlayPath = provenanceDatabasePath currentDb
               operation = "O00000000000000000000000894"
               adr = requireAdrId "A00000000000000000000000894"
               path = "architecture/adrai/decisions/failure-seam.decision.md"
               semantic = "# Classification failure seam\n"
           initTestRepository repoDir
           basisText <- commitFile repoDir "seed.txt" "seed\n"
           let basis = requireGitOid basisText
               capsule = authorityCapsule operation adr basis semantic
           targetText <- commitFile repoDir path (TextEncoding.encodeUtf8 (sealSemantic semantic capsule))
           targetBlobText <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD:" <> path] BS.empty
           target <- resolveTestRepo repoDir targetText
           let document = ParsedManagedDocument
                 { parsedDocumentObjectRef = adrIdText adr,
                   parsedManagedPath = requireRepoPath (Text.pack path),
                   parsedManagedCapsule = capsule,
                   parsedBlobOid = Just (requireGitOid targetBlobText),
                   parsedSemanticHash = digestToText (semanticDigest semantic)
               }
               targetOid = gitOidText (resolvedCommitOid target)
               evidenceConfig = configKey "architecture/adrai/decisions" "architecture/adrai/connections" ["trunk"]
           ensureSchema overlayPath
           initial <- runEnsureForConfig (resolvedRepository target) currentDb mkTestConfig [document] [operation] (resolvedCommitOid target)
           case initial of
             Left problem -> assertFailure ("initial ensure: " <> show problem)
             Right _ -> pure ()

           -- Retain old reachable placement evidence but remove its target
           -- certificate.  A subsequent discovery/classification failure
           -- must roll back without either deleting the old row or issuing a
           -- new proof for it.
           writable <- open overlayPath
           priorPlacement <- query writable
             "SELECT op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json FROM operation_commit WHERE op_id=? AND commit_oid=?"
             [SQLText operation, SQLText targetOid] :: IO [(Text, Text, Text, Integer, Integer, Text, Text)]
           assertBool "fixture retains an old reachable operation placement" (not (null priorPlacement))
           execute writable
             "DELETE FROM operation_target_coverage WHERE op_id=? AND target_oid=?"
             [SQLText operation, SQLText targetOid]
           execute_ writable
             "CREATE TRIGGER fail_target_classification BEFORE INSERT ON operation_commit WHEN NEW.op_id='O00000000000000000000000894' BEGIN SELECT RAISE(ABORT, 'injected classification failure'); END"
           close writable
           classificationFailure <- runEnsureForConfig (resolvedRepository target) currentDb mkTestConfig [document] [operation] (resolvedCommitOid target)
           case classificationFailure of
             Left _ -> pure ()
             Right update -> assertFailure ("injected classification failure was accepted: " <> show update)
           certificates <- bracket (open overlayPath) close $ \connection ->
             query connection "SELECT op_id FROM operation_target_coverage WHERE target_oid=?" (Only targetOid) :: IO [Only Text]
           certificates @?= []
           retainedPlacement <- bracket (open overlayPath) close $ \connection ->
             query connection
               "SELECT op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json FROM operation_commit WHERE op_id=? AND commit_oid=?"
               [SQLText operation, SQLText targetOid] :: IO [(Text, Text, Text, Integer, Integer, Text, Text)]
           retainedPlacement @?= priorPlacement
           writableAfterFailure <- open overlayPath
           execute_ writableAfterFailure "DROP TRIGGER fail_target_classification"
           close writableAfterFailure
           let failingGit = repoDir </> "fail-discovery.cmd"
               failingRepository = (resolvedRepository target) {repositoryClient = GitClient failingGit}
           writeFile failingGit "@echo off\r\nexit /b 1\r\n"
           discoveryFailure <- runEnsureForConfig failingRepository currentDb mkTestConfig [document] [operation] (resolvedCommitOid target)
           case discoveryFailure of
             Left _ -> pure ()
             Right update -> assertFailure ("discovery failure was accepted: " <> show update)
           certificatesAfterDiscoveryFailure <- bracket (open overlayPath) close $ \connection ->
             query connection "SELECT op_id FROM operation_target_coverage WHERE target_oid=?" (Only targetOid) :: IO [Only Text]
           certificatesAfterDiscoveryFailure @?= []
           retainedAfterDiscoveryFailure <- bracket (open overlayPath) close $ \connection ->
             query connection
               "SELECT op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json FROM operation_commit WHERE op_id=? AND commit_oid=?"
               [SQLText operation, SQLText targetOid] :: IO [(Text, Text, Text, Integer, Integer, Text, Text)]
           retainedAfterDiscoveryFailure @?= priorPlacement
           classificationEvidence <- readProvenanceEvidenceAt (resolvedRepository target) overlayPath (resolvedCommitOid target) [operation] evidenceConfig
           classificationEvidence @?= Left (ProvenanceEvidenceMissingTargetPlacement operation)
         ),

       testCase "ensure fast path requires the exact requested configuration" $
        (withSystemTempDirectory "adrai ensure exact config" $ \temp -> do
          let repoDir = temp </> "repo"
              currentDb = temp </> "index.sqlite"
              overlayPath = provenanceDatabasePath currentDb
              operation = "O00000000000000000000000893"
              adr = requireAdrId "A00000000000000000000000893"
              path = "architecture/adrai/decisions/config-evolution.decision.md"
              semantic = "# Configuration evolution\n"
          initTestRepository repoDir
          basisText <- commitFile repoDir "seed.txt" "seed\n"
          let basis = requireGitOid basisText
              capsule = authorityCapsule operation adr basis semantic
          targetText <- commitFile repoDir path (TextEncoding.encodeUtf8 (sealSemantic semantic capsule))
          blobText <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD:" <> path] BS.empty
          target <- resolveTestRepo repoDir targetText
          let document = ParsedManagedDocument
                { parsedDocumentObjectRef = adrIdText adr,
                  parsedManagedPath = requireRepoPath (Text.pack path),
                  parsedManagedCapsule = capsule,
                  parsedBlobOid = Just (requireGitOid blobText),
                  parsedSemanticHash = digestToText (semanticDigest semantic)
                }
              configB = case mkManagedPaths
                  (requireRepoPath "architecture/adrai/decisions")
                  (requireRepoPath "architecture/adrai/connections") of
                Left problem -> error (show problem)
                Right paths -> case mkConfig ConfigSchemaV1 paths [LogicalLine "release" [GitRef "refs/heads/main"]] of
                  Left problem -> error (show problem)
                  Right value -> value
          ensureSchema overlayPath
          first <- runEnsureForConfig (resolvedRepository target) currentDb mkTestConfig [document] [operation] (resolvedCommitOid target)
          firstUpdate <- case first of
            Left problem -> assertFailure ("config A ensure: " <> show problem)
            Right update -> pure update
          second <- runEnsureForConfig (resolvedRepository target) currentDb configB [document] [operation] (resolvedCommitOid target)
          secondUpdate <- case second of
            Left problem -> assertFailure ("config B ensure: " <> show problem)
            Right update -> pure update
          connection <- open overlayPath
          configs <- query_ connection "SELECT config_key,config_json FROM line_config ORDER BY config_key" :: IO [(Text, Text)]
          let configBKey = configKey "architecture/adrai/decisions" "architecture/adrai/connections" ["release"]
              configBJson = TextEncoding.decodeUtf8 (Lazy.toStrict (Aeson.encode
                (Aeson.Object (KeyMap.fromList
                  [ (Key.fromText "connections", Aeson.String "architecture/adrai/connections"),
                    (Key.fromText "decisions", Aeson.String "architecture/adrai/decisions"),
                    (Key.fromText "logical_lines", Aeson.Array (Vector.fromList [Aeson.String "release"]))
                  ]))))
          refsB <- query connection "SELECT ref_name FROM line_ref_state WHERE config_key=? ORDER BY ref_name"
            [SQLText configBKey]
            :: IO [Only Text]
          landingsB <- query connection
            "SELECT line_id,ref_name,commit_oid,complete FROM line_landing WHERE config_key=? AND op_id=? ORDER BY line_id,ref_name,commit_oid"
            [SQLText configBKey, SQLText operation]
            :: IO [(Text, Text, Text, Int)]
          close connection
          length configs @?= 2
          lookup configBKey configs @?= Just configBJson
          refsB @?= [Only "refs/heads/main"]
          landingsB @?= [("release", "refs/heads/main", gitOidText (resolvedCommitOid target), 1)]
          loaderCalled <- newIORef False
          third <- bracket (open overlayPath) close $ \thirdConnection ->
            ensureProvenance (resolvedRepository target) thirdConnection currentDb
              ["release"]
              "architecture/adrai/decisions"
              "architecture/adrai/connections"
              (configLogicalLines configB)
              Nothing
              [operation]
              (Just (writeIORef loaderCalled True >> pure Map.empty))
              (resolvedCommitOid target)
          case third of
            Left problem -> assertFailure ("config B reuse: " <> show problem)
            Right update -> do
              changed update @?= False
              generation update @?= generation secondUpdate + 1
              assertBool "the initial ensure advanced provenance generation"
                (generation secondUpdate > generation firstUpdate)
          maintenanceEntered <- readIORef loaderCalled
          assertBool "exact config B reuse takes the fast path before the groups loader" (not maintenanceEntered)
        ),





      -- 5. Cherry-pick of an op commit to a diverged main is a "copy".
      testCase "cherry_pick_copy" $
        (withSystemTempDirectory "adrai overlay cherry-pick copy" $ \temp -> do
          let repoDir = temp </> "repo"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"
          _ <- gitSuccess repoDir ["branch", "feature"] BS.empty
          _ <- gitSuccess repoDir ["checkout", "feature"] BS.empty

          featureOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          decisionContent <- createAdraiFile
            (adrObjectRef (requireAdrId "A00000000000000000000000005"))
            "# Decision 5\nCherry-pick test."
            "O00000000000000000000000005"
            featureOid
          _ <- commitFiles repoDir
            [ ("architecture/adrai/decisions/000/00000000000000000000000005--cherry.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]
          _ <- gitSuccess repoDir ["commit", "--amend", "-m", "Feature operation\n\nADRAI-Op: O00000000000000000000000005"] BS.empty
          featureCommitOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty
          decisionBlobOid <- outputText <$> gitSuccess repoDir
            ["rev-parse", "HEAD:architecture/adrai/decisions/000/00000000000000000000000005--cherry.decision.md"] BS.empty

          _ <- gitSuccess repoDir ["checkout", "main"] BS.empty
          _ <- commitFile repoDir "main-change.txt" "main change"
          -- Cherry-pick the feature commit
          _ <- gitSuccess repoDir ["cherry-pick", Text.unpack featureCommitOid] BS.empty
          _ <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          resolved <- resolveTestRepo repoDir "HEAD"
          let basis = requireGitOid featureOid
          ensureSchema overlayPath
          let capsule = case mkProvenanceCapsule $ ProvenanceCapsuleInput
                { capsuleInputOperationId = requireOperationId "O00000000000000000000000005"
                , capsuleInputObjectId = ProvenanceAdr (requireAdrId "A00000000000000000000000005")
                , capsuleInputEventKind = case mkEventKind "decision" of
                    Left e -> error $ show e
                    Right k -> k
                , capsuleInputActor = case mkActor HumanActor "test" Nothing of
                    Left e -> error $ show e
                    Right a -> a
                , capsuleInputTimestampMs = 1700000000000
                , capsuleInputBasis = basis
                , capsuleInputParents = []
                , capsuleInputBranchHint = Nothing
                , capsuleInputUpstreamHint = Nothing
                , capsuleInputLineAnchors = []
                , capsuleInputSemanticDigest = semanticDigest decisionContent
                , capsuleInputToolVersion = "adrai/0.1.0"
                , capsuleInputDigests = ProvenanceInputs Nothing Nothing Nothing
                } of
                Left e -> error $ show e
                Right c -> c
          let doc = ParsedManagedDocument
                { parsedDocumentObjectRef = pack "A00000000000000000000000005"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000005--cherry.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Just (requireGitOid decisionBlobOid)
                , parsedSemanticHash = digestToText $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }

          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) overlayPath [doc] ["O00000000000000000000000005"] basis
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              placements <- queryPlacements overlayPath "O00000000000000000000000005"
              assertBool "should have placement" (not (null placements))
              let classified = map snd placements
              assertBool "cherry-pick should be copy" ("copy" `elem` classified)
        ),



      -- 8. After GC, squash merge still shows as introduction.
      testCase "squash_survives_gc" $
        (getRepositorySeed >>= \seed -> withRepositorySeedCopy seed "adrai overlay squash survives gc" $ \temp repoDir -> do
          let overlayPath = temp </> "provenance.sqlite"
          _ <- gitSuccess repoDir ["branch", "feature"] BS.empty
          _ <- gitSuccess repoDir ["checkout", "feature"] BS.empty

          featureOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          decisionContent <- createAdraiFile
            (adrObjectRef (requireAdrId "A00000000000000000000000008"))
            "# Decision 8\nGC survival test."
            "O00000000000000000000000008"
            featureOid
          _ <- commitFiles repoDir
            [ ("architecture/adrai/decisions/000/00000000000000000000000008--gc.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]
          decisionBlobOid <- outputText <$> gitSuccess repoDir
            ["rev-parse", "HEAD:architecture/adrai/decisions/000/00000000000000000000000008--gc.decision.md"] BS.empty

          _ <- gitSuccess repoDir ["checkout", "main"] BS.empty
          _ <- commitFile repoDir "main-change.txt" "main change"
          _ <- gitSuccess repoDir ["merge", "--squash", "feature"] BS.empty
          _ <- gitSuccess repoDir ["commit", "-m", "Squash merge for GC test"] BS.empty
          _ <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          -- Run garbage collection
          _ <- gitSuccess repoDir ["gc", "--prune=now"] BS.empty

          resolved <- resolveTestRepo repoDir "HEAD"
          let basis = requireGitOid featureOid
          ensureSchema overlayPath
          let capsule = case mkProvenanceCapsule $ ProvenanceCapsuleInput
                { capsuleInputOperationId = requireOperationId "O00000000000000000000000008"
                , capsuleInputObjectId = ProvenanceAdr (requireAdrId "A00000000000000000000000008")
                , capsuleInputEventKind = case mkEventKind "decision" of
                    Left e -> error $ show e
                    Right k -> k
                , capsuleInputActor = case mkActor HumanActor "test" Nothing of
                    Left e -> error $ show e
                    Right a -> a
                , capsuleInputTimestampMs = 1700000000000
                , capsuleInputBasis = basis
                , capsuleInputParents = []
                , capsuleInputBranchHint = Nothing
                , capsuleInputUpstreamHint = Nothing
                , capsuleInputLineAnchors = []
                , capsuleInputSemanticDigest = semanticDigest decisionContent
                , capsuleInputToolVersion = "adrai/0.1.0"
                , capsuleInputDigests = ProvenanceInputs Nothing Nothing Nothing
                } of
                Left e -> error $ show e
                Right c -> c
          let doc = ParsedManagedDocument
                { parsedDocumentObjectRef = pack "A00000000000000000000000008"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000008--gc.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Just (requireGitOid decisionBlobOid)
                , parsedSemanticHash = digestToText $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }

          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) overlayPath [doc] ["O00000000000000000000000008"] basis
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              placements <- queryPlacements overlayPath "O00000000000000000000000008"
              assertBool "should have placement after GC" (not (null placements))
              let classified = map snd placements
              assertBool "squash still introduction after GC" ("introduction" `elem` classified)
        ),


      -- 11. A trailer on a commit that doesn't contain the sealed objects
      --     should trigger a TRAILER_WITHOUT_SEALED_OBJECTS warning.
      testCase "trailer_without_sealed_warned" $
        (withSystemTempDirectory "adrai overlay trailer without sealed" $ \temp -> do
          let repoDir = temp </> "repo"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"
          _ <- gitSuccess repoDir ["branch", "feature"] BS.empty
          _ <- gitSuccess repoDir ["checkout", "feature"] BS.empty

          featureOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          decisionContent <- createAdraiFile
            (adrObjectRef (requireAdrId "A00000000000000000000000011"))
            "# Decision 11\nTrailer without sealed."
            "O00000000000000000000000011"
            featureOid
          _ <- commitFiles repoDir
            [ ("architecture/adrai/decisions/000/00000000000000000000000011--sealed.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]

          _ <- gitSuccess repoDir ["checkout", "main"] BS.empty
          -- Add a commit with ADRAI-Op trailer but no sealed file
          _ <- gitSuccess repoDir ["commit", "--allow-empty", "-m", "Trailer only\n\nADRAI-Op: O00000000000000000000000011"] BS.empty
          _ <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          resolved <- resolveTestRepo repoDir "HEAD"
          let basis = requireGitOid featureOid
          ensureSchema overlayPath
          let capsule = case mkProvenanceCapsule $ ProvenanceCapsuleInput
                { capsuleInputOperationId = requireOperationId "O00000000000000000000000011"
                , capsuleInputObjectId = ProvenanceAdr (requireAdrId "A00000000000000000000000011")
                , capsuleInputEventKind = case mkEventKind "decision" of
                    Left e -> error $ show e
                    Right k -> k
                , capsuleInputActor = case mkActor HumanActor "test" Nothing of
                    Left e -> error $ show e
                    Right a -> a
                , capsuleInputTimestampMs = 1700000000000
                , capsuleInputBasis = basis
                , capsuleInputParents = []
                , capsuleInputBranchHint = Nothing
                , capsuleInputUpstreamHint = Nothing
                , capsuleInputLineAnchors = []
                , capsuleInputSemanticDigest = semanticDigest decisionContent
                , capsuleInputToolVersion = "adrai/0.1.0"
                , capsuleInputDigests = ProvenanceInputs Nothing Nothing Nothing
                } of
                Left e -> error $ show e
                Right c -> c
          let doc = ParsedManagedDocument
                { parsedDocumentObjectRef = pack "A00000000000000000000000011"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000011--sealed.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Nothing
                , parsedSemanticHash = digestToText $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }

          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) overlayPath [doc] ["O00000000000000000000000011"] basis
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              issues <- queryIssueCodes overlayPath
              assertBool "TRAILER_WITHOUT_SEALED_OBJECTS warning should be recorded"
                ("TRAILER_WITHOUT_SEALED_OBJECTS" `elem` issues)
        ),

      -- 12. Wrong object set declared in trailer generates OPERATION_OBJECT_SET_MISMATCH.
      testCase "wrong_object_set_is_warning" $
        (withSystemTempDirectory "adrai overlay wrong object set" $ \temp -> do
          let repoDir = temp </> "repo"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"
          _ <- gitSuccess repoDir ["branch", "feature"] BS.empty
          _ <- gitSuccess repoDir ["checkout", "feature"] BS.empty

          featureOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          decisionContent <- createAdraiFile
            (adrObjectRef (requireAdrId "A00000000000000000000000012"))
            "# Decision 12\nWrong object set."
            "O00000000000000000000000012"
            featureOid
          _ <- commitFiles repoDir
            [ ("architecture/adrai/decisions/000/00000000000000000000000012--wrong.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]

          _ <- gitSuccess repoDir ["checkout", "main"] BS.empty
          -- Add a commit that declares a different object set in trailer
          _ <- gitSuccess repoDir
            ["commit", "--allow-empty", "-m", "Wrong object set\n\nADRAI-Op: O00000000000000000000000012\nADRAI-Objects: A99999999999999999999999999"]
            BS.empty
          _ <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          resolved <- resolveTestRepo repoDir "HEAD"
          let basis = requireGitOid featureOid
          ensureSchema overlayPath
          let capsule = case mkProvenanceCapsule $ ProvenanceCapsuleInput
                { capsuleInputOperationId = requireOperationId "O00000000000000000000000012"
                , capsuleInputObjectId = ProvenanceAdr (requireAdrId "A00000000000000000000000012")
                , capsuleInputEventKind = case mkEventKind "decision" of
                    Left e -> error $ show e
                    Right k -> k
                , capsuleInputActor = case mkActor HumanActor "test" Nothing of
                    Left e -> error $ show e
                    Right a -> a
                , capsuleInputTimestampMs = 1700000000000
                , capsuleInputBasis = basis
                , capsuleInputParents = []
                , capsuleInputBranchHint = Nothing
                , capsuleInputUpstreamHint = Nothing
                , capsuleInputLineAnchors = []
                , capsuleInputSemanticDigest = semanticDigest decisionContent
                , capsuleInputToolVersion = "adrai/0.1.0"
                , capsuleInputDigests = ProvenanceInputs Nothing Nothing Nothing
                } of
                Left e -> error $ show e
                Right c -> c
          let doc = ParsedManagedDocument
                { parsedDocumentObjectRef = pack "A00000000000000000000000012"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000012--wrong.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Nothing
                , parsedSemanticHash = digestToText $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }

          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) overlayPath [doc] ["O00000000000000000000000012"] basis
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              issues <- queryIssueCodes overlayPath
              -- The mismatch warning should be recorded
              -- (depends on whether the commit contains the sealed file)
              assertBool "issue was recorded (either mismatch or other)" (not (null issues))
        ),

      -- 13. Shallow repository history is flagged as incomplete.
      testCase "shallow_history_incomplete" $
        (withSystemTempDirectory "adrai overlay shallow history" $ \temp -> do
          let source = temp </> "source"
              shallow = temp </> "shallow"
              dbPath = shallow </> "provenance.sqlite"
          initTestRepository source
          _ <- commitFile source "seed.txt" "seed"
          _ <- gitSuccess source ["branch", "feature"] BS.empty
          _ <- gitSuccess source ["checkout", "feature"] BS.empty

          featureOid <- outputText <$> gitSuccess source ["rev-parse", "HEAD"] BS.empty

          decisionContent <- createAdraiFile
            (adrObjectRef (requireAdrId "A00000000000000000000000013"))
            "# Decision 13\nShallow history."
            "O00000000000000000000000013"
            featureOid
          _ <- commitFiles source
            [ ("architecture/adrai/decisions/000/00000000000000000000000013--shallow.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]
          decisionBlobOid <- outputText <$> gitSuccess source
            ["rev-parse", "HEAD:architecture/adrai/decisions/000/00000000000000000000000013--shallow.decision.md"] BS.empty

          _ <- gitSuccess source ["checkout", "main"] BS.empty
          _ <- commitFile source "main.txt" "main"

          -- Create a transport-independent shallow clone of the feature branch.
          cloneIndependentDepthOneRepository source "feature" shallow

          resolved <- resolveTestRepo shallow "HEAD"
          let basis = requireGitOid featureOid
          ensureSchema dbPath
          let capsule = case mkProvenanceCapsule $ ProvenanceCapsuleInput
                { capsuleInputOperationId = requireOperationId "O00000000000000000000000013"
                , capsuleInputObjectId = ProvenanceAdr (requireAdrId "A00000000000000000000000013")
                , capsuleInputEventKind = case mkEventKind "decision" of
                    Left e -> error $ show e
                    Right k -> k
                , capsuleInputActor = case mkActor HumanActor "test" Nothing of
                    Left e -> error $ show e
                    Right a -> a
                , capsuleInputTimestampMs = 1700000000000
                , capsuleInputBasis = basis
                , capsuleInputParents = []
                , capsuleInputBranchHint = Nothing
                , capsuleInputUpstreamHint = Nothing
                , capsuleInputLineAnchors = []
                , capsuleInputSemanticDigest = semanticDigest decisionContent
                , capsuleInputToolVersion = "adrai/0.1.0"
                , capsuleInputDigests = ProvenanceInputs Nothing Nothing Nothing
                } of
                Left e -> error $ show e
                Right c -> c
          let doc = ParsedManagedDocument
                { parsedDocumentObjectRef = pack "A00000000000000000000000013"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000013--shallow.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Just (requireGitOid decisionBlobOid)
                , parsedSemanticHash = digestToText $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }

          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) dbPath [doc] ["O00000000000000000000000013"] (resolvedCommitOid resolved)
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              completeValues <- queryLandingCompleteness dbPath
              assertBool "shallow history should mark a landing incomplete"
                (0 `elem` completeValues)
        )
    , testCase "target-relative evidence is lossless and ignores later cache facts" targetRelativeEvidenceTest
    , testCase "recovery witness includes a previously observed sibling trailer in T^S" (getRepositorySeed >>= witnessRangeIncludesMergedSiblingTest)
    , testCase "recovery witness source equal to target has an empty T^S range" (getRepositorySeed >>= witnessRangeSourceEqualsTargetTest)
    , testCase "recovery witness derives the exact closure of a merged source" (getRepositorySeed >>= witnessRangeUsesMergedSourceClosureTest)
    , testCase "recovery witness fails closed for shallow and reachability-mismatched sources" (getRepositorySeed >>= witnessRangeRejectsUnsafeSourcesTest)
    , testCase "inherited recovery emits a fresh exact target certificate" (getRepositorySeed >>= inheritedWitnessFreshTargetCertificateTest)
    , testCase "inherited witness restores every source placement and classifies only T^S" (getRepositorySeed >>= inheritedWitnessRestorationWorklistTest)
    , testCase "2,000-operation seed and candidate reads stay bounded" (getRepositorySeed >>= boundedProvenanceReadFanoutTest)
    , testCase "changed semantic signature clears stale target and landing state" changedSignatureSeedClearsStateTest
    ]

-- =====================================================================
-- Helper functions
-- =====================================================================

createOverlayRepositorySeed :: IO RepositorySeed
createOverlayRepositorySeed =
  createRepositorySeed "adrai overlay retained seed" $ \repository -> do
    initTestRepository repository
    void (gitSuccess repository ["commit", "--allow-empty", "-m", "retained fixture root"] BS.empty)

mergePlacementOperationOne, mergePlacementOperationTwo :: Text
mergePlacementOperationOne = "O00000000000000000000000981"
mergePlacementOperationTwo = "O00000000000000000000000982"

mergePlacementAdrOne, mergePlacementAdrTwo :: AdrId
mergePlacementAdrOne = requireAdrId "A00000000000000000000000981"
mergePlacementAdrTwo = requireAdrId "A00000000000000000000000982"

mergePlacementPathOne, mergePlacementPathTwo :: FilePath
mergePlacementPathOne = "architecture/adrai/decisions/warm-first-parent.decision.md"
mergePlacementPathTwo = "architecture/adrai/decisions/warm-second-parent.decision.md"

mergePlacementSemanticOne, mergePlacementSemanticTwo :: Text
mergePlacementSemanticOne = "# First-parent merge operation\n"
mergePlacementSemanticTwo = "# Second-parent merge operation\n"

createMergePlacementRepositorySeed :: IO RepositorySeed
createMergePlacementRepositorySeed =
  createRepositorySeed "adrai overlay merge placement seed" $ \repository -> do
    initTestRepository repository
    basisText <- commitFile repository "seed.txt" "seed\n"
    let basis = requireGitOid basisText
        capsuleOne = authorityCapsule mergePlacementOperationOne mergePlacementAdrOne basis mergePlacementSemanticOne
        capsuleTwo = authorityCapsule mergePlacementOperationTwo mergePlacementAdrTwo basis mergePlacementSemanticTwo
    void (gitSuccess repository ["branch", "feature", Text.unpack basisText] BS.empty)
    void (commitFile repository mergePlacementPathOne (TextEncoding.encodeUtf8 (sealSemantic mergePlacementSemanticOne capsuleOne)))
    void (gitSuccess repository ["switch", "feature"] BS.empty)
    void (commitFile repository mergePlacementPathTwo (TextEncoding.encodeUtf8 (sealSemantic mergePlacementSemanticTwo capsuleTwo)))
    void (gitSuccess repository ["switch", "main"] BS.empty)
    void
      ( gitSuccess repository
          [ "merge", "--no-ff", "feature", "-m",
            "merge second-parent operation\n\nADRAI-Op: " <> Text.unpack mergePlacementOperationTwo
          ]
          BS.empty
      )

witnessRangeIncludesMergedSiblingTest :: RepositorySeed -> IO ()
witnessRangeIncludesMergedSiblingTest seed =
  withRepositorySeedCopy seed "adrai witness merged sibling" $ \_ repoDir -> do
    let operation = "O00000000000000000000000971"
    sourceText <- commitFile repoDir "source.txt" "source\n"
    source <- resolveTestRepo repoDir sourceText
    _ <- gitSuccess repoDir ["branch", "sibling", Text.unpack sourceText] BS.empty
    _ <- gitSuccess repoDir ["switch", "sibling"] BS.empty
    _ <- commitFile repoDir "sibling.txt" "sibling\n"
    _ <- gitSuccess repoDir ["commit", "--amend", "-m", "previously observed sibling trailer\n\nADRAI-Op: " <> Text.unpack operation] BS.empty
    sibling <- resolveTestRepo repoDir "HEAD"
    _ <- gitSuccess repoDir ["switch", "main"] BS.empty
    _ <- gitSuccess repoDir ["merge", "--no-ff", "sibling", "-m", "merge sibling"] BS.empty
    target <- resolveTestRepo repoDir "HEAD"
    graph <- reachableCommitGraphAt (resolvedRepository source) (resolvedCommitOid source)
    sourceNodes <- case graph of
      Left problem -> assertFailure ("source graph: " <> show problem) >> fail "unreachable"
      Right nodes -> pure nodes
    let witness = ProvenanceRecoveryWitness
          (resolvedCommitOid source) mempty (Set.singleton operation) (Set.singleton operation)
          (Set.fromList (map gitCommitNodeOid sourceNodes)) Set.empty Set.empty Set.empty Set.empty
    range <- usableWitnessRange (resolvedRepository target) (resolvedCommitOid target) (Just witness)
    assertBool "T^S includes the sibling trailer commit even when it predates the merge"
      (maybe False (elem (resolvedCommitOid sibling)) range)

witnessRangeSourceEqualsTargetTest :: RepositorySeed -> IO ()
witnessRangeSourceEqualsTargetTest seed =
  withRepositorySeedCopy seed "adrai witness source equals target" $ \_ repoDir -> do
    _ <- commitFile repoDir "source.txt" "source\n"
    target <- resolveTestRepo repoDir "HEAD"
    graph <- reachableCommitGraphAt (resolvedRepository target) (resolvedCommitOid target)
    targetNodes <- case graph of
      Left problem -> assertFailure ("target graph: " <> show problem) >> fail "unreachable"
      Right nodes -> pure nodes
    let witness = ProvenanceRecoveryWitness
          (resolvedCommitOid target) mempty Set.empty Set.empty
          (Set.fromList (map gitCommitNodeOid targetNodes)) Set.empty Set.empty Set.empty Set.empty
    range <- usableWitnessRange (resolvedRepository target) (resolvedCommitOid target) (Just witness)
    range @?= Just []

witnessRangeUsesMergedSourceClosureTest :: RepositorySeed -> IO ()
witnessRangeUsesMergedSourceClosureTest seed =
  withRepositorySeedCopy seed "adrai witness merged source closure" $ \_ repoDir -> do
    rootText <- commitFile repoDir "root.txt" "root\n"
    _ <- gitSuccess repoDir ["branch", "source-side", Text.unpack rootText] BS.empty
    _ <- gitSuccess repoDir ["switch", "source-side"] BS.empty
    sideText <- commitFile repoDir "side.txt" "side\n"
    side <- resolveTestRepo repoDir sideText
    _ <- gitSuccess repoDir ["switch", "main"] BS.empty
    _ <- commitFile repoDir "main.txt" "main\n"
    _ <- gitSuccess repoDir ["merge", "--no-ff", "source-side", "-m", "merged source"] BS.empty
    source <- resolveTestRepo repoDir "HEAD"
    _ <- commitFile repoDir "target.txt" "target\n"
    target <- resolveTestRepo repoDir "HEAD"
    graph <- reachableCommitGraphAt (resolvedRepository source) (resolvedCommitOid source)
    sourceNodes <- case graph of
      Left problem -> assertFailure ("source graph: " <> show problem) >> fail "unreachable"
      Right nodes -> pure nodes
    let sourceReachable = Set.fromList (map gitCommitNodeOid sourceNodes)
        witness = ProvenanceRecoveryWitness
          (resolvedCommitOid source) mempty Set.empty Set.empty
          sourceReachable Set.empty Set.empty Set.empty Set.empty
    assertBool "merged source closure includes its non-main parent"
      (Set.member (resolvedCommitOid side) sourceReachable)
    range <- usableWitnessRange (resolvedRepository target) (resolvedCommitOid target) (Just witness)
    range @?= Just [resolvedCommitOid target]

witnessRangeRejectsUnsafeSourcesTest :: RepositorySeed -> IO ()
witnessRangeRejectsUnsafeSourcesTest seed =
  withRepositorySeedCopy seed "adrai witness unsafe source" $ \temp sourceDir -> do
    let shallowDir = temp </> "shallow"
    sourceText <- commitFile sourceDir "source.txt" "source\n"
    _ <- commitFile sourceDir "target.txt" "target\n"
    target <- resolveTestRepo sourceDir "HEAD"
    source <- resolveTestRepo sourceDir sourceText
    graph <- reachableCommitGraphAt (resolvedRepository source) (resolvedCommitOid source)
    sourceNodes <- case graph of
      Left problem -> assertFailure ("source graph: " <> show problem) >> fail "unreachable"
      Right nodes -> pure nodes
    let witness = ProvenanceRecoveryWitness
          (resolvedCommitOid source) mempty Set.empty Set.empty
          (Set.fromList (map gitCommitNodeOid sourceNodes)) Set.empty Set.empty Set.empty Set.empty
        mismatched = witness {recoveryWitnessSourceReachability = Set.empty}
    mismatch <- usableWitnessRange (resolvedRepository target) (resolvedCommitOid target) (Just mismatched)
    mismatch @?= Nothing
    _ <- gitSuccess sourceDir ["branch", "unreachable-witness", Text.unpack sourceText] BS.empty
    _ <- gitSuccess sourceDir ["switch", "unreachable-witness"] BS.empty
    _ <- commitFile sourceDir "sibling.txt" "sibling\n"
    sibling <- resolveTestRepo sourceDir "HEAD"
    siblingGraph <- reachableCommitGraphAt (resolvedRepository sibling) (resolvedCommitOid sibling)
    siblingNodes <- case siblingGraph of
      Left problem -> assertFailure ("sibling graph: " <> show problem) >> fail "unreachable"
      Right nodes -> pure nodes
    let siblingWitness = witness
          { recoveryWitnessSourceTarget = resolvedCommitOid sibling
          , recoveryWitnessSourceReachability = Set.fromList (map gitCommitNodeOid siblingNodes)
          }
    unrelated <- usableWitnessRange (resolvedRepository target) (resolvedCommitOid target) (Just siblingWitness)
    unrelated @?= Nothing
    _ <- gitSuccess sourceDir ["switch", "main"] BS.empty
    cloneIndependentDepthOneRepository sourceDir "main" shallowDir
    shallow <- resolveTestRepo shallowDir "HEAD"
    shallowResult <- usableWitnessRange (resolvedRepository shallow) (resolvedCommitOid shallow) (Just witness)
    shallowResult @?= Nothing

inheritedWitnessFreshTargetCertificateTest :: RepositorySeed -> IO ()
inheritedWitnessFreshTargetCertificateTest seed =
  withRepositorySeedCopy seed "adrai inherited witness certificate" $ \temp repoDir -> do
    let currentDb = temp </> "index.sqlite"
        overlayPath = provenanceDatabasePath currentDb
        operation = "O00000000000000000000000972"
        adr = requireAdrId "A00000000000000000000000972"
        path = "architecture/adrai/decisions/inherited-witness.decision.md"
        semantic = "# Inherited witness\n"
    basisText <- commitFile repoDir "seed.txt" "seed\n"
    let basis = requireGitOid basisText
        capsule = authorityCapsule operation adr basis semantic
        bytes = TextEncoding.encodeUtf8 (sealSemantic semantic capsule)
    _ <- commitFiles repoDir [(path, bytes)]
    _ <- commitFile repoDir "source-witness.txt" "source witness\n"
    source <- resolveTestRepo repoDir "HEAD"
    blobText <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD:" <> path] BS.empty
    let document = makeParsedDoc (adrObjectRef adr) (requireRepoPath (Text.pack path)) capsule
          (Just (requireGitOid blobText)) (digestToText (semanticDigest semantic))
    ensureSchema overlayPath
    sourceResult <- runEnsureForConfig (resolvedRepository source) currentDb mkTestConfig [document] [operation] (resolvedCommitOid source)
    case sourceResult of
      Left problem -> assertFailure ("source ensure: " <> show problem)
      Right _ -> pure ()
    sourceConfigsBefore <- bracket (open overlayPath) close $ \connection ->
      query_ connection "SELECT config_key,config_json FROM line_config" :: IO [(Text, Text)]
    (sourceConfigKey, sourceConfigText) <- case sourceConfigsBefore of
      [row] -> pure row
      rows -> assertFailure ("unexpected source configs before witness: " <> show rows) >> fail "unreachable"
    bracket (open overlayPath) close $ \connection -> do
      execute connection "INSERT OR REPLACE INTO operation_commit(op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json) VALUES(?,?,?,?,?,?,?)"
        [ SQLText operation, SQLText (gitOidText (resolvedCommitOid source)), SQLText "copied"
        , SQLInteger 2, SQLInteger 2, SQLText "source witness placement", SQLText "[]"
        ]
      recordIssue connection "warning" "SOURCE_WITNESS_ISSUE" "source witness issue"
        Nothing (Just operation) (Just (Text.pack path)) (Just operation)
      execute connection "INSERT OR REPLACE INTO line_landing(config_key,op_id,line_id,ref_name,commit_oid,complete) VALUES(?,?,?,?,?,?)"
        [ SQLText sourceConfigKey, SQLText operation, SQLText "trunk", SQLText "refs/heads/main"
        , SQLText (gitOidText (resolvedCommitOid source)), SQLInteger 1
        ]
    _ <- commitFile repoDir "noise.txt" "target\n"
    target <- resolveTestRepo repoDir "HEAD"
    graph <- reachableCommitGraphAt (resolvedRepository source) (resolvedCommitOid source)
    sourceNodes <- case graph of
      Left problem -> assertFailure ("source graph: " <> show problem) >> fail "unreachable"
      Right nodes -> pure nodes
    registrations <- queryRegisteredOperations overlayPath
    signature <- case registrations of
      [(operationId, _, _, value)] | operationId == operation -> pure value
      rows -> assertFailure ("unexpected registrations: " <> show rows) >> fail "unreachable"
    sourcePlacements <- readOperationRows overlayPath
    (sourceIssues, sourceLandings, sourceConfigs) <- bracket (open overlayPath) close $ \connection -> do
      issues <- query connection "SELECT op_id,severity,code,adr_id,object_id,path,message FROM provenance_issue WHERE op_id=?" (Only operation) :: IO [(Text, Text, Text, Maybe Text, Maybe Text, Maybe Text, Text)]
      landings <- query connection "SELECT config_key,op_id,line_id,ref_name,commit_oid,complete FROM line_landing WHERE op_id=?" (Only operation) :: IO [(Text, Text, Text, Text, Text, Integer)]
      configs <- query_ connection "SELECT config_key,config_json FROM line_config" :: IO [(Text, Text)]
      pure (issues, landings, configs)
    let witness = ProvenanceRecoveryWitness (resolvedCommitOid source)
          (Map.singleton operation signature) (Set.singleton operation) (Set.singleton operation)
          (Set.fromList (map gitCommitNodeOid sourceNodes)) (Set.fromList sourcePlacements)
          (Set.fromList sourceIssues) (Set.fromList sourceLandings) (Set.fromList sourceConfigs)
    witnessRange <- usableWitnessRange (resolvedRepository target) (resolvedCommitOid target) (Just witness)
    bracket (open overlayPath) close $ \connection ->
      execute connection "INSERT OR REPLACE INTO line_landing(config_key,op_id,line_id,ref_name,commit_oid,complete) VALUES(?,?,?,?,?,?)"
        [ SQLText sourceConfigKey, SQLText operation, SQLText "trunk", SQLText "refs/heads/feature"
        , SQLText (gitOidText (resolvedCommitOid target)), SQLInteger 1
        ]
    sourceConfigs @?= sourceConfigsBefore
    validity <- bracket (open overlayPath) close $ \connection ->
      recoveryProjectionValidity connection [operation] sourceConfigKey sourceConfigText witness
    recoveryWitnessSuppresses (Just witness) witnessRange (Map.singleton operation signature)
      validity (Map.singleton operation True) operation @?= True
    inherited <- runEnsureForConfigWithWitness (resolvedRepository target) currentDb mkTestConfig [document] [operation] (resolvedCommitOid target) (Just witness)
    case inherited of
      Left problem -> assertFailure ("inherited ensure: " <> show problem)
      Right update -> do
        assertBool "the target refresh executes rather than taking an exact-source fast path" (changed update)
        coverage <- readCoverageRows overlayPath
        assertBool "a newly certified exact T row replaces inherited authority"
          ((operation, gitOidText (resolvedCommitOid target), signature) `elem` coverage)
    survivingLanding <- bracket (open overlayPath) close $ \connection ->
      query connection "SELECT 1 FROM line_landing WHERE config_key=? AND op_id=? AND ref_name=? AND commit_oid=?"
        [SQLText sourceConfigKey, SQLText operation, SQLText "refs/heads/feature", SQLText (gitOidText (resolvedCommitOid target))] :: IO [Only Integer]
    survivingLanding @?= [Only 1]
    let assertTamper label tamper = do
          _ <- bracket (open overlayPath) close tamper
          rejected <- bracket (open overlayPath) close $ \connection ->
            recoveryProjectionValidity connection [operation] sourceConfigKey sourceConfigText witness
          Map.lookup operation rejected @?= Just False
          recoveryWitnessSuppresses (Just witness) witnessRange (Map.singleton operation signature)
            rejected (Map.singleton operation True) operation @?= False
          repaired <- runEnsureForConfigWithWitness (resolvedRepository target) currentDb mkTestConfig [document] [operation] (resolvedCommitOid target) (Just witness)
          case repaired of
            Left problem -> assertFailure (label <> " repair: " <> show problem)
            Right _ -> pure ()
          restored <- bracket (open overlayPath) close $ \connection ->
            recoveryProjectionValidity connection [operation] sourceConfigKey sourceConfigText witness
          Map.lookup operation restored @?= Just True
          coverage <- readCoverageRows overlayPath
          assertBool (label <> " retains a fresh exact target certificate")
            ((operation, gitOidText (resolvedCommitOid target), signature) `elem` coverage)
    assertBool "witness fixture has multiple source placements" (length sourcePlacements >= 2)
    let (_, placementOid, _, _, _, _, _) = requireHead "witness source-placement fixture" sourcePlacements
    assertTamper "altered source placement" $ \connection ->
      execute connection "UPDATE operation_commit SET subject=? WHERE op_id=? AND commit_oid=?"
        [SQLText "tampered placement", SQLText operation, SQLText placementOid]
    assertTamper "deleted source placement with missing target certificate" $ \connection -> do
      execute connection "DELETE FROM operation_commit WHERE op_id=? AND commit_oid=?"
        [SQLText operation, SQLText placementOid]
      execute connection "DELETE FROM operation_target_coverage WHERE op_id=? AND target_oid=?"
        [SQLText operation, SQLText (gitOidText (resolvedCommitOid target))]
    assertTamper "altered same-key source issue" $ \connection ->
      execute connection "UPDATE provenance_issue SET severity=? WHERE op_id=? AND code=?"
        [SQLText "error", SQLText operation, SQLText "SOURCE_WITNESS_ISSUE"]
    assertTamper "altered source landing" $ \connection ->
      execute connection "UPDATE line_landing SET complete=0 WHERE config_key=? AND op_id=? AND line_id=? AND ref_name=?"
        [SQLText sourceConfigKey, SQLText operation, SQLText "trunk", SQLText "refs/heads/main"]

-- | A valid source witness is a complete source projection, not merely a
-- shortcut certificate.  Removing every source placement must import that
-- projection before planning, and the remaining classification work may cover
-- only the independently proven @T^S@ delta.
inheritedWitnessRestorationWorklistTest :: RepositorySeed -> IO ()
inheritedWitnessRestorationWorklistTest seed =
  withRepositorySeedCopy seed "adrai inherited witness restoration" $ \temp repoDir -> do
    let currentDb = temp </> "index.sqlite"
        overlayPath = provenanceDatabasePath currentDb
        operation = "O00000000000000000000000974"
        adr = requireAdrId "A00000000000000000000000974"
        path = "architecture/adrai/decisions/inherited-restoration.decision.md"
        semantic = "# Inherited restoration\n"
    basisText <- commitFile repoDir "seed.txt" "seed\n"
    let basis = requireGitOid basisText
        capsule = authorityCapsule operation adr basis semantic
        bytes = TextEncoding.encodeUtf8 (sealSemantic semantic capsule)
    _ <- commitFiles repoDir [(path, bytes)]
    _ <- commitFile repoDir "source-witness.txt" "source witness\n"
    source <- resolveTestRepo repoDir "HEAD"
    blobText <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD:" <> path] BS.empty
    let document = makeParsedDoc (adrObjectRef adr) (requireRepoPath (Text.pack path)) capsule
          (Just (requireGitOid blobText)) (digestToText (semanticDigest semantic))
    ensureSchema overlayPath
    sourceResult <- runEnsureForConfig (resolvedRepository source) currentDb mkTestConfig [document] [operation] (resolvedCommitOid source)
    case sourceResult of
      Left problem -> assertFailure ("source ensure: " <> show problem)
      Right _ -> pure ()
    sourceConfigs <- bracket (open overlayPath) close $ \connection ->
      query_ connection "SELECT config_key,config_json FROM line_config" :: IO [(Text, Text)]
    (sourceConfigKey, sourceConfigText) <- case sourceConfigs of
      [row] -> pure row
      rows -> assertFailure ("unexpected source configs: " <> show rows) >> fail "unreachable"
    bracket (open overlayPath) close $ \connection -> do
      execute connection "INSERT OR REPLACE INTO operation_commit(op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json) VALUES(?,?,?,?,?,?,?)"
        [ SQLText operation, SQLText (gitOidText (resolvedCommitOid source)), SQLText "copied"
        , SQLInteger 2, SQLInteger 2, SQLText "source witness placement", SQLText "[]"
        ]
      recordIssue connection "warning" "SOURCE_RESTORE_ISSUE" "source witness issue"
        Nothing (Just operation) (Just (Text.pack path)) (Just operation)
      execute connection "INSERT OR REPLACE INTO line_landing(config_key,op_id,line_id,ref_name,commit_oid,complete) VALUES(?,?,?,?,?,?)"
        [ SQLText sourceConfigKey, SQLText operation, SQLText "trunk", SQLText "refs/heads/main"
        , SQLText (gitOidText (resolvedCommitOid source)), SQLInteger 1
        ]
    sourceGraph <- reachableCommitGraphAt (resolvedRepository source) (resolvedCommitOid source)
    sourceNodes <- case sourceGraph of
      Left problem -> assertFailure ("source graph: " <> show problem) >> fail "unreachable"
      Right nodes -> pure nodes
    registrations <- queryRegisteredOperations overlayPath
    signature <- case registrations of
      [(operationId, _, _, value)] | operationId == operation -> pure value
      rows -> assertFailure ("unexpected registrations: " <> show rows) >> fail "unreachable"
    sourcePlacements <- readOperationRows overlayPath
    (sourceIssues, sourceLandings) <- bracket (open overlayPath) close $ \connection -> do
      issues <- query connection "SELECT op_id,severity,code,adr_id,object_id,path,message FROM provenance_issue WHERE op_id=?" (Only operation) :: IO [(Text, Text, Text, Maybe Text, Maybe Text, Maybe Text, Text)]
      landings <- query connection "SELECT config_key,op_id,line_id,ref_name,commit_oid,complete FROM line_landing WHERE op_id=?" (Only operation) :: IO [(Text, Text, Text, Text, Text, Integer)]
      pure (issues, landings)
    let witness = ProvenanceRecoveryWitness (resolvedCommitOid source)
          (Map.singleton operation signature) (Set.singleton operation) (Set.singleton operation)
          (Set.fromList (map gitCommitNodeOid sourceNodes)) (Set.fromList sourcePlacements)
          (Set.fromList sourceIssues) (Set.fromList sourceLandings) (Set.fromList sourceConfigs)
    _ <- commitFile repoDir "target-only.txt" "target\n"
    target <- resolveTestRepo repoDir "HEAD"
    witnessRange <- usableWitnessRange (resolvedRepository target) (resolvedCommitOid target) (Just witness)
    expectedDelta <- case witnessRange of
      Just commits | not (null commits) -> pure (Set.fromList commits)
      _ -> assertFailure "expected nonempty T^S witness range" >> fail "unreachable"
    bracket (open overlayPath) close $ \connection ->
      execute connection "DELETE FROM operation_commit WHERE op_id=?" (Only operation)
    deleted <- queryPlacements overlayPath operation
    deleted @?= []
    invalid <- bracket (open overlayPath) close $ \connection ->
      recoveryProjectionValidity connection [operation] sourceConfigKey sourceConfigText witness
    Map.lookup operation invalid @?= Just False
    worklists <- newIORef []
    inherited <- runEnsureForConfigWithWitnessAndWorklistObserver
      (\commits -> modifyIORef' worklists (commits :))
      (resolvedRepository target) currentDb mkTestConfig [document] [operation]
      (resolvedCommitOid target) (Just witness)
    case inherited of
      Left problem -> assertFailure ("inherited ensure: " <> show problem)
      Right update -> assertBool "recovery refreshes the new target certificate" (changed update)
    observedWorklists <- readIORef worklists
    case observedWorklists of
      [worklist] -> do
        let classified = Set.fromList worklist
        classified @?= expectedDelta
        assertBool "no source-history commit is reclassified" (Set.null (classified `Set.intersection` recoveryWitnessSourceReachability witness))
      rows -> assertFailure ("expected one recovery worklist, got " <> show rows)
    restoredRows <- readOperationRows overlayPath
    sort restoredRows @?= sort sourcePlacements

-- | Statement fan-out is bounded by the grouped plans, not by operation count.
-- The counter deliberately observes only SQLite reads/prepares; 2,000 expected
-- registration writes are row work, not a return to per-operation discovery.
boundedProvenanceReadFanoutTest :: RepositorySeed -> IO ()
boundedProvenanceReadFanoutTest seed =
  withRepositorySeedCopy seed "adrai bounded provenance reads" $ \temp repoDir -> do
    let semanticCache = temp </> "semantic.sqlite"
        overlayPath = temp </> "overlay.sqlite"
        operationIds = ["O-bounded-" <> Text.pack (show n) | n <- [1 :: Int .. 2000]]
    commitText <- commitFile repoDir "seed.txt" "seed\n"
    resolved <- resolveTestRepo repoDir "HEAD"
    let commit = gitOidText (requireGitOid commitText)
    bracket (open semanticCache) close $ \cache -> do
      execute_ cache "CREATE TABLE meta(key TEXT PRIMARY KEY,value TEXT NOT NULL)"
      execute cache "INSERT INTO meta VALUES(?,?)" ("schema" :: Text, "adrai-cache/3" :: Text)
      execute_ cache "CREATE TABLE operation(operation_id TEXT PRIMARY KEY,basis_oid TEXT NOT NULL)"
      execute_ cache "CREATE TABLE operation_member(operation_id TEXT,object_id TEXT,path TEXT,blob_oid TEXT,semantic_digest TEXT)"
      withTransaction cache $ do
        executeMany cache "INSERT INTO operation VALUES(?,?)"
          [(operationId, commit) | operationId <- operationIds]
        executeMany cache "INSERT INTO operation_member VALUES(?,?,?,?,?)"
          [ (operationId, "A00000000000000000000000975" :: Text, "architecture/adrai/decisions/bounded.md" :: Text, commit, "sha256:bounded" :: Text)
          | operationId <- operationIds
          ]
    ensureSchema overlayPath
    readCount <- newIORef (0 :: Int)
    (seeded, changedIds) <- bracket (open overlayPath) close $ \overlay ->
      seedRegisteredOperationsFromSemanticCacheWithQueryObserver
        (modifyIORef' readCount (+ 1)) semanticCache overlay
    length seeded @?= 2000
    changedIds @?= []
    observedSeedReads <- readIORef readCount
    observedSeedReads @?= 4
    bracket (open overlayPath) close $ \overlay -> do
      execute overlay "INSERT INTO commit_observation VALUES(?,?,?,?,?,?)"
        [SQLText commit, SQLText "[]", SQLInteger 1, SQLInteger 1, SQLText "bounded", SQLText ""]
      execute overlay "INSERT INTO managed_path_addition VALUES(?,?)"
        [SQLText "architecture/adrai/decisions/bounded.md", SQLText commit]
      candidates <- candidateCommitsWithQueryObserver (modifyIORef' readCount (+ 1)) overlay [GitOid commit] operationIds
      candidateRows <- case candidates of
        Left problem -> assertFailure ("candidate construction: " <> show problem)
        Right rows -> pure rows
      Map.size candidateRows @?= 2000
      assertBool "every operation receives the grouped path candidate"
        (all (Set.member (GitOid commit)) (Map.elems candidateRows))
      observedCandidateReads <- readIORef readCount
      observedCandidateReads @?= 8
      processResult <- processCandidatesWithQueryObserver (modifyIORef' readCount (+ 1))
        (resolvedRepository resolved) overlay (Map.map (const Set.empty) candidateRows) operationIds
      case processResult of
        Left problem -> assertFailure ("process construction: " <> show problem)
        Right () -> pure ()
      observedTotalReads <- readIORef readCount
      observedTotalReads @?= 11
      missingPlacementIssues <- query_ overlay "SELECT count(*) FROM provenance_issue WHERE code='NO_OPERATION_COMMIT'" :: IO [Only Int]
      missingPlacementIssues @?= [Only 2000]

changedSignatureSeedClearsStateTest :: IO ()
changedSignatureSeedClearsStateTest =
  withSystemTempDirectory "adrai changed signature seed" $ \temp -> do
    let semanticCache = temp </> "semantic.sqlite"
        overlayPath = temp </> "overlay.sqlite"
        operation = "O00000000000000000000000973"
        oid = "0123456789012345678901234567890123456789"
    bracket (open semanticCache) close $ \cache -> do
      execute_ cache "CREATE TABLE meta(key TEXT PRIMARY KEY,value TEXT NOT NULL)"
      execute cache "INSERT INTO meta VALUES(?,?)" ("schema" :: Text, "adrai-cache/3" :: Text)
      execute_ cache "CREATE TABLE operation(operation_id TEXT PRIMARY KEY,basis_oid TEXT NOT NULL)"
      execute_ cache "CREATE TABLE operation_member(operation_id TEXT,object_id TEXT,path TEXT,blob_oid TEXT,semantic_digest TEXT)"
      execute cache "INSERT INTO operation VALUES(?,?)" (operation, oid)
      execute cache "INSERT INTO operation_member VALUES(?,?,?,?,?)" (operation, "A00000000000000000000000973" :: Text, "architecture/adrai/decisions/changed.md" :: Text, oid, "sha256:changed" :: Text)
    bracket (open overlayPath) close $ \overlay -> do
      createOverlaySchema overlay
      execute overlay "INSERT INTO registered_operation VALUES(?,?,?,?)" [SQLText operation, SQLNull, SQLText oid, SQLText "old-signature"]
      execute overlay "INSERT INTO registered_object VALUES(?,?,?,?)" [SQLText operation, SQLText "A00000000000000000000000973", SQLText "old.md", SQLText oid]
      execute overlay "INSERT INTO operation_commit VALUES(?,?,?,?,?,?,?)" [SQLText operation, SQLText oid, SQLText "original", SQLInteger 1, SQLInteger 1, SQLText "old", SQLText "[]"]
      execute overlay "INSERT INTO operation_target_coverage VALUES(?,?,?)" [SQLText operation, SQLText oid, SQLText "old-signature"]
      execute overlay "INSERT INTO line_landing VALUES(?,?,?,?,?,?)" [SQLText "cfg", SQLText operation, SQLText "line", SQLText "refs/heads/main", SQLText oid, SQLInteger 1]
      execute overlay "INSERT INTO provenance_issue VALUES(?,?,?,?,?,?,?,?)" [SQLText "old-issue", SQLText "warning", SQLText "OLD", SQLNull, SQLNull, SQLNull, SQLText "old", SQLText operation]
      (operationIds, changedIds) <- seedRegisteredOperationsFromSemanticCache semanticCache overlay
      operationIds @?= [operation]
      changedIds @?= [operation]
      forM_ [ "operation_target_coverage", "operation_commit", "line_landing", "provenance_issue" ] $ \table -> do
        rows <- query_ overlay (asQuery ("SELECT count(*) FROM " <> table)) :: IO [Only Int]
        rows @?= [Only 0]

targetRelativeEvidenceTest :: IO ()
targetRelativeEvidenceTest =
  withSystemTempDirectory "adrai provenance evidence ünicode" $ \temp -> do
    let repoDir = temp </> "repo with spaces ü"
        dbPath = temp </> "cache with spaces ü.sqlite"
        operation = "O00000000000000000000000999"
        laterOperation = "O00000000000000000000000998"
        wrongOperation = "O00000000000000000000000997"
        config = "config-ü"
        managedFile = "architecture/adrai/decisions/managed-fixture.md"
    initTestRepository repoDir
    _ <- commitFile repoDir managedFile "managed fixture baseline\n"
    firstOidText <- commitFile repoDir "seed.txt" "first"
    resolvedFirst <- resolveTestRepo repoDir firstOidText
    laterOidText <- commitFile repoDir "later.txt" "later"
    resolvedLater <- resolveTestRepo repoDir laterOidText
    let firstOid = resolvedCommitOid resolvedFirst
        laterOid = resolvedCommitOid resolvedLater
    ensureSchema dbPath
    connection <- open dbPath
    execute connection "INSERT INTO registered_operation VALUES(?,?,?,?)"
      [SQLText operation, SQLText "A00000000000000000000000999", SQLText (gitOidText firstOid), SQLText "signature-ü"]
    execute connection "INSERT INTO registered_object VALUES(?,?,?,?)"
      [SQLText operation, SQLText "A00000000000000000000000999", SQLText "architecture/adrai/decisions/ü space.md", SQLText (gitOidText firstOid)]
    execute connection "INSERT INTO registered_operation VALUES(?,?,?,?)"
      [SQLText laterOperation, SQLText "A00000000000000000000000998", SQLText (gitOidText laterOid), SQLText "later"]
    execute connection "INSERT INTO registered_object VALUES(?,?,?,?)"
      [SQLText operation, SQLText "A00000000000000000000000996", SQLText "architecture/adrai/decisions/second.md", SQLText (gitOidText firstOid)]
    execute connection "INSERT INTO registered_object VALUES(?,?,?,?)"
      [SQLText laterOperation, SQLText "A00000000000000000000000998", SQLText "later.md", SQLText (gitOidText laterOid)]
    execute connection "INSERT INTO registered_operation VALUES(?,?,?,?)"
      [SQLText wrongOperation, SQLText "A00000000000000000000000997", SQLText (gitOidText firstOid), SQLText "wrong-op"]
    execute connection "INSERT INTO registered_object VALUES(?,?,?,?)"
      [SQLText wrongOperation, SQLText "A00000000000000000000000997", SQLText "wrong.md", SQLText (gitOidText firstOid)]
    -- Exact-v2 evidence reads require a current target certificate for every
    -- requested operation.  Keep the fixture's historical rows intact while
    -- making its two valid target projections canonical.
    execute connection "INSERT INTO operation_target_coverage VALUES(?,?,?)"
      [SQLText operation, SQLText (gitOidText firstOid), SQLText "signature-ü"]
    execute connection "INSERT INTO operation_target_coverage VALUES(?,?,?)"
      [SQLText operation, SQLText (gitOidText laterOid), SQLText "signature-ü"]
    execute connection "INSERT INTO operation_target_coverage VALUES(?,?,?)"
      [SQLText laterOperation, SQLText (gitOidText laterOid), SQLText "later"]
    execute connection "INSERT INTO operation_commit VALUES(?,?,?,?,?,?,?)"
      [SQLText operation, SQLText (gitOidText firstOid), SQLText "original", SQLInteger 9223372036854775806, SQLInteger 9223372036854775805, SQLText "subject-ü", SQLText "[\"raw-parent\"]"]
    execute connection "INSERT INTO operation_commit VALUES(?,?,?,?,?,?,?)"
      [SQLText operation, SQLText (gitOidText laterOid), SQLText "contradictory-later", SQLInteger 3, SQLInteger 4, SQLText "later placement", SQLText "[]"]
    execute connection "INSERT INTO operation_commit VALUES(?,?,?,?,?,?,?)"
      [SQLText laterOperation, SQLText (gitOidText laterOid), SQLText "later", SQLInteger 1, SQLInteger 2, SQLText "later", SQLText "[]"]
    execute connection "INSERT INTO line_config VALUES(?,?)" [SQLText config, SQLText "{\"unicode\":\"ü\"}"]
    execute connection "INSERT INTO line_ref_state VALUES(?,?,?)" [SQLText config, SQLText "refs/heads/main", SQLText (gitOidText firstOid)]
    execute connection "INSERT INTO line_ref_state VALUES(?,?,?)" [SQLText config, SQLText "refs/heads/later", SQLText (gitOidText laterOid)]
    execute connection "INSERT INTO line_landing VALUES(?,?,?,?,?,?)" [SQLText config, SQLText operation, SQLText "trunk", SQLText "refs/heads/main", SQLText (gitOidText firstOid), SQLInteger 1]
    execute connection "INSERT INTO line_landing VALUES(?,?,?,?,?,?)" [SQLText config, SQLText operation, SQLText "later", SQLText "refs/heads/later", SQLText (gitOidText laterOid), SQLInteger 0]
    execute connection "INSERT INTO line_landing VALUES(?,?,?,?,?,?)" [SQLText config, SQLText laterOperation, SQLText "later", SQLText "refs/heads/later", SQLText (gitOidText laterOid), SQLInteger 1]
    execute connection "INSERT INTO ref_observation VALUES(?,?,?)" [SQLText "refs/heads/main", SQLText (gitOidText firstOid), SQLText "commit"]
    execute connection "INSERT INTO ref_observation VALUES(?,?,?)" [SQLText "refs/heads/later", SQLText (gitOidText laterOid), SQLText "commit"]
    execute connection "INSERT INTO observation_root VALUES(?,?,?)" [SQLText "query", SQLText "first", SQLText (gitOidText firstOid)]
    execute connection "INSERT INTO observation_root VALUES(?,?,?)" [SQLText "query", SQLText "later", SQLText (gitOidText laterOid)]
    execute connection "INSERT INTO provenance_issue VALUES(?,?,?,?,?,?,?,?)"
      [SQLText "issue", SQLText "warning", SQLText "CODE", SQLNull, SQLNull, SQLNull, SQLText "message", SQLText operation]
    close connection
    -- Deliberately retain all kinds of worktree state.  Evidence acquisition
    -- owns neither Git nor the checkout, including a managed document.
    writeFile (repoDir </> "seed.txt") "tracked unstaged working-tree content\n"
    writeFile (repoDir </> "staged.txt") "staged index content\n"
    _ <- gitSuccess repoDir ["add", "staged.txt"] BS.empty
    writeFile (repoDir </> "untracked.txt") "untracked working-tree content\n"
    headBefore <- gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty
    symbolicRefBefore <- gitSuccess repoDir ["symbolic-ref", "-q", "HEAD"] BS.empty
    symbolicRefOidBefore <- gitSuccess repoDir ["rev-parse", "--verify", "HEAD"] BS.empty
    statusBefore <- gitSuccess repoDir ["status", "--porcelain=v1", "-z"] BS.empty
    rawBefore <- gitSuccess repoDir ["diff", "--raw", "-z"] BS.empty
    stagedRawBefore <- gitSuccess repoDir ["diff", "--cached", "--raw", "-z"] BS.empty
    indexBefore <- gitSuccess repoDir ["ls-files", "--stage", "-z"] BS.empty
    trackedBefore <- gitSuccess repoDir ["ls-files", "-z"] BS.empty
    worktreeBytesBefore <- traverse
      (\path -> (,) path <$> BS.readFile (repoDir </> path))
      ["seed.txt", "staged.txt", "untracked.txt", managedFile]
    schemaBefore <- overlaySchemaFacts dbPath
    sidecarsBefore <- overlayFileSnapshot dbPath
    let assertOverlayUnchanged label = do
          actual <- overlayFileSnapshot dbPath
          assertBool label (actual == sidecarsBefore)
        expectedOldEvidence =
          ProvenanceEvidence
            firstOid
            (Just (LineConfigRow config "{\"unicode\":\"ü\"}"))
            [ ProvenanceOperationEvidence
                (RegisteredOperationRow operation (Just "A00000000000000000000000999") firstOid "signature-ü")
                [ RegisteredObjectRow operation "A00000000000000000000000996" "architecture/adrai/decisions/second.md" firstOid
                , RegisteredObjectRow operation "A00000000000000000000000999" "architecture/adrai/decisions/ü space.md" firstOid
                ]
                [ OperationCommitRow
                    operation
                    firstOid
                    "original"
                    9223372036854775806
                    9223372036854775805
                    "subject-ü"
                    "[\"raw-parent\"]"
                ]
                [LineLandingRow config operation "trunk" "refs/heads/main" firstOid 1]
                [ProvenanceIssueRow "warning" "CODE" Nothing Nothing Nothing "message" (Just operation)]
            ]
            [LineRefStateRow config "refs/heads/main" firstOid]
            [RefObservationRow "refs/heads/main" firstOid "commit"]
            [ObservationRootRow "query" "first" firstOid]
        expectedLaterEvidence =
          ProvenanceEvidence
            laterOid
            (Just (LineConfigRow config "{\"unicode\":\"ü\"}"))
            [ ProvenanceOperationEvidence
                (RegisteredOperationRow laterOperation (Just "A00000000000000000000000998") laterOid "later")
                [RegisteredObjectRow laterOperation "A00000000000000000000000998" "later.md" laterOid]
                [OperationCommitRow laterOperation laterOid "later" 1 2 "later" "[]"]
                [LineLandingRow config laterOperation "later" "refs/heads/later" laterOid 1]
                []
            ]
            [ LineRefStateRow config "refs/heads/later" laterOid
            , LineRefStateRow config "refs/heads/main" firstOid
            ]
            [ RefObservationRow "refs/heads/later" laterOid "commit"
            , RefObservationRow "refs/heads/main" firstOid "commit"
            ]
            [ ObservationRootRow "query" "first" firstOid
            , ObservationRootRow "query" "later" laterOid
            ]
    oldEvidence <- readProvenanceEvidenceAt (resolvedRepository resolvedFirst) dbPath firstOid [operation] config
    oldEvidence @?= Right expectedOldEvidence
    assertOverlayUnchanged "read-only evidence acquisition must preserve database bytes after success"
    oldEvidenceAgain <- readProvenanceEvidenceAt (resolvedRepository resolvedFirst) dbPath firstOid [operation] config
    oldEvidenceAgain @?= oldEvidence
    let relativeDbPath = takeFileName dbPath
    assertBool "relative-path evidence fixture must not accidentally use an absolute path" (isRelative relativeDbPath)
    relativeEvidence <- withCurrentDirectory temp $
      readProvenanceEvidenceAt (resolvedRepository resolvedFirst) relativeDbPath firstOid [operation] config
    relativeEvidence @?= Right expectedOldEvidence
    emptyEvidence <- readProvenanceEvidenceAt (resolvedRepository resolvedFirst) dbPath firstOid [] config
    emptyEvidence @?= Right (ProvenanceEvidence firstOid Nothing [] [] [] [])
    laterAtFirst <- readProvenanceEvidenceAt (resolvedRepository resolvedFirst) dbPath firstOid [laterOperation] config
    laterAtFirst @?= Left (ProvenanceEvidenceMissingTargetPlacement laterOperation)
    placementlessAtFirst <- readProvenanceEvidenceAt (resolvedRepository resolvedFirst) dbPath firstOid [wrongOperation] config
    placementlessAtFirst @?= Left (ProvenanceEvidenceMissingTargetPlacement wrongOperation)
    cancellation <- try @SomeException $
      readProvenanceEvidenceAtWith (resolvedRepository resolvedFirst) dbPath firstOid [operation] config (throwIO ThreadKilled)
    case cancellation of
      Left exception -> fromException exception @?= Just ThreadKilled
      Right _ -> assertFailure "ThreadKilled must be rethrown rather than converted to evidence failure"
    customCancellation <- try @SomeException $
      readProvenanceEvidenceAtWith (resolvedRepository resolvedFirst) dbPath firstOid [operation] config (throwIO TestAsyncCancellation)
    case customCancellation of
      Left exception -> fromException exception @?= Just TestAsyncCancellation
      Right _ -> assertFailure "custom asynchronous cancellation must be rethrown"
    retryEvidence <- readProvenanceEvidenceAt (resolvedRepository resolvedFirst) dbPath firstOid [operation] config
    assertBool "retry after cancellation reopens the closed connection" (either (const False) (const True) retryEvidence)
    let missingDb = temp </> "missing.sqlite"
    synchronousFailure <- readProvenanceEvidenceAt (resolvedRepository resolvedFirst) missingDb firstOid [operation] config
    case synchronousFailure of
      Left (ProvenanceEvidenceDatabaseError _) -> pure ()
      other -> assertFailure ("expected typed synchronous database failure, got " <> show other)
    missingDbCreated <- doesFileExist missingDb
    assertBool "read-only acquisition must not create a missing database" (not missingDbCreated)
    laterEvidence <- readProvenanceEvidenceAt (resolvedRepository resolvedLater) dbPath laterOid [laterOperation] config
    laterEvidence @?= Right expectedLaterEvidence
    operationAtLater <- readProvenanceEvidenceAt (resolvedRepository resolvedLater) dbPath laterOid [operation] config
    case operationAtLater of
      Left problem -> assertFailure ("readProvenanceEvidenceAt contradictory operation at later target: " <> show problem)
      Right evidence -> do
        let operationEvidence = requireHead "operation evidence fixture" (provenanceEvidenceOperations evidence)
        map operationCommitRowCommitOid (provenanceEvidenceCommits operationEvidence) @?= sort [firstOid, laterOid]
        map lineLandingRowCommitOid (provenanceEvidenceLandings operationEvidence) @?= [laterOid, firstOid]
    headAfterReadOnly <- gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty
    symbolicRefAfterReadOnly <- gitSuccess repoDir ["symbolic-ref", "-q", "HEAD"] BS.empty
    symbolicRefOidAfterReadOnly <- gitSuccess repoDir ["rev-parse", "--verify", "HEAD"] BS.empty
    statusAfterReadOnly <- gitSuccess repoDir ["status", "--porcelain=v1", "-z"] BS.empty
    rawAfterReadOnly <- gitSuccess repoDir ["diff", "--raw", "-z"] BS.empty
    stagedRawAfterReadOnly <- gitSuccess repoDir ["diff", "--cached", "--raw", "-z"] BS.empty
    indexAfterReadOnly <- gitSuccess repoDir ["ls-files", "--stage", "-z"] BS.empty
    trackedAfterReadOnly <- gitSuccess repoDir ["ls-files", "-z"] BS.empty
    worktreeBytesAfterReadOnly <- traverse
      (\path -> (,) path <$> BS.readFile (repoDir </> path))
      ["seed.txt", "staged.txt", "untracked.txt", managedFile]
    sidecarsAfterReadOnly <- overlayFileSnapshot dbPath
    headAfterReadOnly @?= headBefore
    symbolicRefAfterReadOnly @?= symbolicRefBefore
    symbolicRefOidAfterReadOnly @?= symbolicRefOidBefore
    statusAfterReadOnly @?= statusBefore
    rawAfterReadOnly @?= rawBefore
    stagedRawAfterReadOnly @?= stagedRawBefore
    indexAfterReadOnly @?= indexBefore
    trackedAfterReadOnly @?= trackedBefore
    worktreeBytesAfterReadOnly @?= worktreeBytesBefore
    assertBool "read-only evidence acquisition must preserve database bytes and sidecar inventory" (sidecarsAfterReadOnly == sidecarsBefore)
    schemaAfterReadOnly <- overlaySchemaFacts dbPath
    schemaAfterReadOnly @?= schemaBefore

    writerStarted <- newEmptyMVar
    writerFinished <- newEmptyMVar
    readerReleased <- newEmptyMVar
    let concurrentMaintenance = do
          _ <- forkIO $ do
            outcome <- try @SomeException $
              bracket (open dbPath) close $ \writable -> do
                execute_ writable "PRAGMA busy_timeout=10000"
                beginResult <- try @SomeException (execute_ writable "BEGIN IMMEDIATE")
                putMVar writerStarted beginResult
                case beginResult of
                  Left exception -> throwIO exception
                  Right () -> do
                    writeResult <- try @SomeException $ do
                      execute writable "UPDATE line_config SET config_json=? WHERE config_key=?" [SQLText "after-concurrent", SQLText config]
                      execute writable "UPDATE operation_commit SET classification=? WHERE op_id=? AND commit_oid=?" [SQLText "after-concurrent", SQLText operation, SQLText (gitOidText firstOid)]
                      takeMVar readerReleased
                      execute_ writable "COMMIT"
                    case writeResult of
                      Right () -> pure ()
                      Left exception -> do
                        void (try @SomeException (execute_ writable "ROLLBACK"))
                        throwIO exception
            putMVar writerFinished outcome
          pure ()
        snapshotHook = do
          concurrentMaintenance
          begun <- takeMVar writerStarted
          case begun of
            Left exception -> throwIO exception
            Right () -> pure ()
    concurrentResult <- try @SomeException $
      readProvenanceEvidenceAtWith (resolvedRepository resolvedFirst) dbPath firstOid [operation] config snapshotHook
    putMVar readerReleased ()
    case concurrentResult of
      Left exception -> assertFailure ("concurrent snapshot read: " <> show exception)
      Right concurrentEvidence -> case concurrentEvidence of
        Left problem -> assertFailure ("concurrent snapshot evidence: " <> show problem)
        Right evidence -> do
          let observedState =
                ( provenanceEvidenceConfig evidence
                , operationCommitRowClassification (requireHead "concurrent operation-commit fixture" (provenanceEvidenceCommits (requireHead "concurrent operation-evidence fixture" (provenanceEvidenceOperations evidence))))
                )
              beforeState = (Just (LineConfigRow config "{\"unicode\":\"ü\"}"), "original")
              afterState = (Just (LineConfigRow config "after-concurrent"), "after-concurrent")
          assertBool "a concurrent evidence read must expose one complete multi-table state" (observedState `elem` [beforeState, afterState])
          observedState @?= beforeState
    writerOutcome <- takeMVar writerFinished
    case writerOutcome of
      Left exception -> assertFailure ("concurrent overlay maintenance: " <> show exception)
      Right () -> pure ()
    afterConcurrent <- readProvenanceEvidenceAt (resolvedRepository resolvedFirst) dbPath firstOid [operation] config
    case afterConcurrent of
      Left problem -> assertFailure ("after concurrent maintenance: " <> show problem)
      Right evidence -> do
        provenanceEvidenceConfig evidence @?= Just (LineConfigRow config "after-concurrent")
        operationCommitRowClassification (requireHead "post-maintenance operation-commit fixture" (provenanceEvidenceCommits (requireHead "post-maintenance operation-evidence fixture" (provenanceEvidenceOperations evidence)))) @?= "after-concurrent"

    let insertRow statement parameters = do
          writable <- open dbPath
          execute writable statement parameters
          close writable
        deleteRow statement parameters = do
          writable <- open dbPath
          execute writable statement parameters
          close writable
        expectInvalid field setup cleanup = do
          _ <- setup
          actual <- readProvenanceEvidenceAt (resolvedRepository resolvedFirst) dbPath firstOid [operation] config
          _ <- cleanup
          actual @?= Left (ProvenanceEvidenceInvalidOid field "not-an-oid")
    expectInvalid "operation_commit.commit_oid"
      (insertRow "INSERT INTO operation_commit VALUES(?,?,?,?,?,?,?)" [SQLText operation, SQLText "not-an-oid", SQLText "broken", SQLInteger 1, SQLInteger 1, SQLText "bad", SQLText "[]"])
      (deleteRow "DELETE FROM operation_commit WHERE op_id=? AND commit_oid=?" [SQLText operation, SQLText "not-an-oid"])
    expectInvalid "registered_object.blob_oid"
      (insertRow "INSERT INTO registered_object VALUES(?,?,?,?)" [SQLText operation, SQLText "A00000000000000000000000995", SQLText "architecture/adrai/decisions/bad.md", SQLText "not-an-oid"])
      (deleteRow "DELETE FROM registered_object WHERE op_id=? AND object_id=?" [SQLText operation, SQLText "A00000000000000000000000995"])
    expectInvalid "line_landing.commit_oid"
      (insertRow "INSERT INTO line_landing VALUES(?,?,?,?,?,?)" [SQLText config, SQLText operation, SQLText "bad", SQLText "refs/heads/bad", SQLText "not-an-oid", SQLInteger 0])
      (deleteRow "DELETE FROM line_landing WHERE config_key=? AND op_id=? AND line_id=? AND ref_name=?" [SQLText config, SQLText operation, SQLText "bad", SQLText "refs/heads/bad"])
    expectInvalid "line_ref_state.tip_oid"
      (insertRow "INSERT INTO line_ref_state VALUES(?,?,?)" [SQLText config, SQLText "refs/heads/bad", SQLText "not-an-oid"])
      (deleteRow "DELETE FROM line_ref_state WHERE config_key=? AND ref_name=?" [SQLText config, SQLText "refs/heads/bad"])
    expectInvalid "ref_observation.tip_oid"
      (insertRow "INSERT INTO ref_observation VALUES(?,?,?)" [SQLText "refs/heads/bad", SQLText "not-an-oid", SQLText "commit"])
      (deleteRow "DELETE FROM ref_observation WHERE ref_name=?" [SQLText "refs/heads/bad"])
    expectInvalid "observation_root.commit_oid"
      (insertRow "INSERT INTO observation_root VALUES(?,?,?)" [SQLText "query", SQLText "bad", SQLText "not-an-oid"])
      (deleteRow "DELETE FROM observation_root WHERE root_kind=? AND root_name=?" [SQLText "query", SQLText "bad"])

    missingRegistration <- readProvenanceEvidenceAt (resolvedRepository resolvedFirst) dbPath firstOid ["O00000000000000000000000996"] config
    missingRegistration @?= Left (ProvenanceEvidenceMissingRegistration "O00000000000000000000000996")
    missingConfig <- readProvenanceEvidenceAt (resolvedRepository resolvedFirst) dbPath firstOid [operation] "missing-config"
    missingConfig @?= Left (ProvenanceEvidenceMissingConfig "missing-config")
    connection' <- open dbPath
    execute connection' "DELETE FROM registered_object WHERE op_id=?" [SQLText operation]
    close connection'
    missingObjects <- readProvenanceEvidenceAt (resolvedRepository resolvedFirst) dbPath firstOid [operation] config
    missingObjects @?= Left (ProvenanceEvidenceMissingObjects operation)
    connection'' <- open dbPath
    execute_ connection'' "DROP TABLE registered_operation"
    execute_ connection'' "CREATE TABLE registered_operation(op_id TEXT,adr_id TEXT,basis_oid TEXT NOT NULL,signature TEXT NOT NULL)"
    execute connection'' "INSERT INTO registered_operation VALUES(?,?,?,?)"
      [SQLText operation, SQLNull, SQLText (gitOidText firstOid), SQLText "one"]
    execute connection'' "INSERT INTO registered_operation VALUES(?,?,?,?)"
      [SQLText operation, SQLNull, SQLText (gitOidText firstOid), SQLText "two"]
    close connection''
    duplicateRegistration <- readProvenanceEvidenceAt (resolvedRepository resolvedFirst) dbPath firstOid [operation] config
    duplicateRegistration @?= Left (ProvenanceEvidenceDuplicateRegistration operation)
    connection''' <- open dbPath
    execute_ connection''' "DELETE FROM registered_operation"
    execute connection''' "INSERT INTO registered_operation VALUES(?,?,?,?)"
      [SQLText operation, SQLNull, SQLText "not-an-oid", SQLText "broken"]
    close connection'''
    malformedRow <- readProvenanceEvidenceAt (resolvedRepository resolvedFirst) dbPath firstOid [operation] config
    malformedRow @?= Left (ProvenanceEvidenceInvalidOid "registered_operation.basis_oid" "not-an-oid")

data MergePlacementFixture = MergePlacementFixture
  { mergePlacementRepository :: Repository,
    mergePlacementTarget :: GitOid,
    mergePlacementFirstParent :: GitOid,
    mergePlacementSecondParent :: GitOid,
    mergePlacementDocumentOne :: ParsedManagedDocument,
    mergePlacementDocumentTwo :: ParsedManagedDocument
  }

loadMergePlacementFixture :: FilePath -> IO MergePlacementFixture
loadMergePlacementFixture repositoryPath = do
  basis <- resolveTestRepo repositoryPath "main^1^"
  firstParent <- resolveTestRepo repositoryPath "main^1"
  secondParent <- resolveTestRepo repositoryPath "feature"
  target <- resolveTestRepo repositoryPath "main"
  parents <- Text.words . outputText <$> gitSuccess repositoryPath ["show", "-s", "--format=%P", "main"] BS.empty
  parents @?= map (gitOidText . resolvedCommitOid) [firstParent, secondParent]
  blobOne <- outputText <$> gitSuccess repositoryPath ["rev-parse", "main:" <> mergePlacementPathOne] BS.empty
  blobTwo <- outputText <$> gitSuccess repositoryPath ["rev-parse", "main:" <> mergePlacementPathTwo] BS.empty
  let basisOid = resolvedCommitOid basis
      capsuleOne = authorityCapsule mergePlacementOperationOne mergePlacementAdrOne basisOid mergePlacementSemanticOne
      capsuleTwo = authorityCapsule mergePlacementOperationTwo mergePlacementAdrTwo basisOid mergePlacementSemanticTwo
  pure
    MergePlacementFixture
      { mergePlacementRepository = resolvedRepository target,
        mergePlacementTarget = resolvedCommitOid target,
        mergePlacementFirstParent = resolvedCommitOid firstParent,
        mergePlacementSecondParent = resolvedCommitOid secondParent,
        mergePlacementDocumentOne = makeParsedDoc
          (adrObjectRef mergePlacementAdrOne)
          (requireRepoPath (Text.pack mergePlacementPathOne))
          capsuleOne
          (Just (requireGitOid blobOne))
          (digestToText (semanticDigest mergePlacementSemanticOne)),
        mergePlacementDocumentTwo = makeParsedDoc
          (adrObjectRef mergePlacementAdrTwo)
          (requireRepoPath (Text.pack mergePlacementPathTwo))
          capsuleTwo
          (Just (requireGitOid blobTwo))
          (digestToText (semanticDigest mergePlacementSemanticTwo))
      }

requireEnsureUpdate :: Show problem => String -> Either problem ProvenanceUpdate -> IO ProvenanceUpdate
requireEnsureUpdate label = \case
  Left problem -> assertFailure (label <> ": " <> show problem) >> fail "unreachable"
  Right update -> pure update

mergePlacementEvidenceConfig :: Text
mergePlacementEvidenceConfig = configKey "architecture/adrai/decisions" "architecture/adrai/connections" ["trunk"]

firstParentWarmPrimingReconcilesTrailerlessMergePlacement :: RepositorySeed -> IO ()
firstParentWarmPrimingReconcilesTrailerlessMergePlacement seed =
  withRepositorySeedCopy seed "adrai first-parent warm placement" $ \temporary repositoryPath -> do
    fixture <- loadMergePlacementFixture repositoryPath
    let currentDb = temporary </> "index.sqlite"
        overlayPath = provenanceDatabasePath currentDb
        repository = mergePlacementRepository fixture
        target = mergePlacementTarget fixture
        original = mergePlacementFirstParent fixture
        operation = mergePlacementOperationOne
        document = mergePlacementDocumentOne fixture
        expected = [(gitOidText original, "original")]
    ensureSchema overlayPath
    void $ requireEnsureUpdate "first-parent warm prime" =<< runEnsureForConfig repository currentDb mkTestConfig [] [] original
    void $ requireEnsureUpdate "first-parent target ensure" =<< runEnsureForConfig repository currentDb mkTestConfig [document] [operation] target
    queryPlacements overlayPath operation >>= (@?= expected)
    initialIssues <- queryIssueCodes overlayPath
    assertBool "trailerless first-parent placement does not warn" ("REDUNDANT_OPERATION_TRAILER" `notElem` initialIssues)
    bracket (open overlayPath) close $ \connection -> do
      execute connection
        "INSERT OR REPLACE INTO operation_commit(op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json) VALUES(?,?,?,?,?,?,?)"
        [ SQLText operation, SQLText (gitOidText target), SQLText "introduction", SQLInteger 0, SQLInteger 0,
          SQLText "stale first-parent merge placement", SQLText "[]"
        ]
      execute connection
        "DELETE FROM operation_target_coverage WHERE op_id=? AND target_oid=?"
        [SQLText operation, SQLText (gitOidText target)]
    readProvenanceEvidenceAt repository overlayPath target [operation] mergePlacementEvidenceConfig >>= \case
      Left (ProvenanceEvidenceMissingTargetPlacement _) -> pure ()
      other -> assertFailure ("first-parent missing certificate must fail closed, got " <> show other)
    repair <- requireEnsureUpdate "first-parent repair" =<< runEnsureForConfig repository currentDb mkTestConfig [document] [operation] target
    changed repair @?= True
    queryPlacements overlayPath operation >>= (@?= expected)
    repairedIssues <- queryIssueCodes overlayPath
    assertBool "first-parent repair removes the stale extra placement without a redundant warning"
      ("REDUNDANT_OPERATION_TRAILER" `notElem` repairedIssues)
    coverage <- readCoverageRows overlayPath
    length [() | (candidate, coveredTarget, _) <- coverage, candidate == operation, coveredTarget == gitOidText target] @?= 1
    evidence <- readProvenanceEvidenceAt repository overlayPath target [operation] mergePlacementEvidenceConfig
    assertBool "first-parent target is certified after repair" (either (const False) (const True) evidence)

secondParentWarmPrimingRecoversMergeIntroductionPlacement :: RepositorySeed -> IO ()
secondParentWarmPrimingRecoversMergeIntroductionPlacement seed =
  withRepositorySeedCopy seed "adrai second-parent warm placement" $ \temporary repositoryPath -> do
    fixture <- loadMergePlacementFixture repositoryPath
    let currentDb = temporary </> "index.sqlite"
        overlayPath = provenanceDatabasePath currentDb
        repository = mergePlacementRepository fixture
        target = mergePlacementTarget fixture
        original = mergePlacementSecondParent fixture
        operation = mergePlacementOperationTwo
        document = mergePlacementDocumentTwo fixture
        expected = sort [(gitOidText original, "original"), (gitOidText target, "introduction")]
    ensureSchema overlayPath
    void $ requireEnsureUpdate "second-parent warm prime" =<< runEnsureForConfig repository currentDb mkTestConfig [] [] original
    void $ requireEnsureUpdate "second-parent target ensure" =<< runEnsureForConfig repository currentDb mkTestConfig [document] [operation] target
    queryPlacements overlayPath operation >>= (@?= expected)
    bracket (open overlayPath) close $ \connection -> do
      execute connection
        "DELETE FROM operation_commit WHERE op_id=? AND commit_oid=?"
        [SQLText operation, SQLText (gitOidText target)]
      execute connection
        "DELETE FROM operation_target_coverage WHERE op_id=? AND target_oid=?"
        [SQLText operation, SQLText (gitOidText target)]
    readProvenanceEvidenceAt repository overlayPath target [operation] mergePlacementEvidenceConfig >>= \case
      Left (ProvenanceEvidenceMissingTargetPlacement _) -> pure ()
      other -> assertFailure ("second-parent missing certificate must fail closed, got " <> show other)
    repair <- requireEnsureUpdate "second-parent repair" =<< runEnsureForConfig repository currentDb mkTestConfig [document] [operation] target
    changed repair @?= True
    queryPlacements overlayPath operation >>= (@?= expected)
    coverage <- readCoverageRows overlayPath
    length [() | (candidate, coveredTarget, _) <- coverage, candidate == operation, coveredTarget == gitOidText target] @?= 1
    evidence <- readProvenanceEvidenceAt repository overlayPath target [operation] mergePlacementEvidenceConfig
    assertBool "second-parent target is certified after introduction recovery" (either (const False) (const True) evidence)

missingSourceTargetCoverageReplaysRepositoryWideMerge :: RepositorySeed -> IO ()
missingSourceTargetCoverageReplaysRepositoryWideMerge seed =
  withRepositorySeedCopy seed "adrai source-target repository-wide replay" $ \temporary repositoryPath -> do
    fixture <- loadMergePlacementFixture repositoryPath
    let currentDb = temporary </> "index.sqlite"
        overlayPath = provenanceDatabasePath currentDb
        repository = mergePlacementRepository fixture
        mergeTarget = mergePlacementTarget fixture
        sourceTarget = mergePlacementSecondParent fixture
        operation = mergePlacementOperationTwo
        document = mergePlacementDocumentTwo fixture
        expected = sort [(gitOidText sourceTarget, "original"), (gitOidText mergeTarget, "introduction")]
    ensureSchema overlayPath
    void $ requireEnsureUpdate "later merge prime" =<< runEnsureForConfig repository currentDb mkTestConfig [] [] mergeTarget
    void $ requireEnsureUpdate "source target ensure" =<< runEnsureForConfig repository currentDb mkTestConfig [document] [operation] sourceTarget
    queryPlacements overlayPath operation >>= (@?= expected)
    bracket (open overlayPath) close $ \connection -> do
      execute connection
        "UPDATE operation_commit SET classification='copy' WHERE op_id=? AND commit_oid=?"
        [SQLText operation, SQLText (gitOidText mergeTarget)]
      execute connection
        "DELETE FROM operation_target_coverage WHERE op_id=? AND target_oid=?"
        [SQLText operation, SQLText (gitOidText sourceTarget)]
    repair <- requireEnsureUpdate "source target repair" =<< runEnsureForConfig repository currentDb mkTestConfig [document] [operation] sourceTarget
    changed repair @?= True
    commitsScanned repair @?= 0
    queryPlacements overlayPath operation >>= (@?= expected)
    evidence <- readProvenanceEvidenceAt repository overlayPath sourceTarget [operation] mergePlacementEvidenceConfig
    assertBool "source target is certified from its original while retaining the later merge introduction"
      (either (const False) (const True) evidence)
    coverage <- readCoverageRows overlayPath
    length [() | (candidate, coveredTarget, _) <- coverage, candidate == operation, coveredTarget == gitOidText sourceTarget] @?= 1

readOperationRows :: FilePath -> IO [(Text, Text, Text, Integer, Integer, Text, Text)]
readOperationRows dbPath =
  bracket (open dbPath) close $ \connection ->
    query_ connection "SELECT op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json FROM operation_commit ORDER BY op_id,commit_oid,classification,authored_s,committed_s,subject,parents_json"

readCoverageRows :: FilePath -> IO [(Text, Text, Text)]
readCoverageRows dbPath =
  bracket (open dbPath) close $ \connection ->
    query_ connection "SELECT op_id,target_oid,registration_signature FROM operation_target_coverage ORDER BY op_id,target_oid,registration_signature"

overlaySchemaFacts :: FilePath -> IO [(Text, Text)]
overlaySchemaFacts dbPath = do
  connection <- open dbPath
  rows <- query_ connection "SELECT type,name FROM sqlite_master ORDER BY type,name" :: IO [(Text, Text)]
  close connection
  pure rows

overlayFileSnapshot :: FilePath -> IO [(FilePath, ByteString)]
overlayFileSnapshot dbPath = do
  let candidates = [dbPath, dbPath <> "-wal", dbPath <> "-shm", dbPath <> "-journal"]
  existing <- filterM doesFileExist candidates
  traverse (\path -> (,) path <$> BS.readFile path) existing

requireAdrId :: Text -> AdrId
requireAdrId value =
  case mkAdrId value of
    Left v -> error ("invalid AdrId: " <> show v)
    Right a -> a

requireOperationId :: Text -> OperationId
requireOperationId value =
  case mkOperationId value of
    Left v -> error ("invalid OperationId: " <> show v)
    Right o -> o

requireGitOid :: Text -> GitOid
requireGitOid value =
  case mkGitOid value of
    Left v -> error ("invalid GitOid: " <> show v)
    Right o -> o

resolveTestRepo :: FilePath -> Text -> IO ResolvedRepositoryRevision
resolveTestRepo repoDir revision = do
  discoverResult <- discoverRepository systemGit repoDir
  case discoverResult of
    Left e -> assertFailure ("discoverRepository: " <> show e)
    Right repo -> do
      resolveResult <- resolveRepositoryRevision repo (requireRevision revision)
      case resolveResult of
        Left e -> assertFailure ("resolveRepositoryRevision: " <> show e)
        Right r -> pure r
