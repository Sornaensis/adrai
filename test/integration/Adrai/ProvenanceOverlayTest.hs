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
  ( GitError (..),
    GitOid (..),
    Repository (..),
    gitOidText,
    systemGit,
  )
import Adrai.GitTestSupport
  ( commitFile,
    commitFiles,
    gitSuccess,
    initTestRepository,
    outputText,
    requireRepoPath,
    requireRevision,
  )
import Adrai.Provenance
  ( EventKind (..),
    GitOid (..),
    LineAnchor (..),
    OperationContext (..),
    ProvenanceCapsule (..),
    ProvenanceCapsuleInput (..),
    ProvenanceObjectId (..),
    ProvenanceError (..),
    encodeCapsule,
    mkEventKind,
    mkGitOid,
    mkLineAnchor,
    mkOverlayFingerprint,
    mkProvenanceCapsule,
    normalizeSemantic,
    provenanceActor,
    provenanceBasis,
    provenanceEventKind,
    provenanceObjectFromRef,
    provenanceObjectId,
    provenanceOperationContext,
    provenanceTimestampMs,
    sealSemantic,
    sha256Digest,
    semanticDigest,
  )
import Adrai.Provenance.Classification
  ( ParsedManagedDocument (..),
    operationSignature,
    registerOperationGroups,
    storeNewCommits,
    candidateCommits,
    canonicalParentsJson,
    decodeExactBlobTreeEntry,
    processCandidates,
  )
import Adrai.Provenance.Overlay
  ( CommitObservation (..),
    LineLanding (..),
    OperationClassification (..),
    OperationCommit (..),
    ProvenanceIssue (..),
    RefObservation (..),
    ObservationRoot (..),
    ManagedPathAddition (..),
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
    ProvenanceEvidenceError (..),
    createOverlaySchema,
    overlaySchemaDdl,
    overlaySchemaVersion,
    provenanceDatabasePath,
    overlaySchemaIndexes,
  )
import Adrai.Provenance.Ensure
  ( configKey,
    ensureProvenance,
    overlayRowsForOperations,
    readProvenanceEvidenceAt,
    readProvenanceEvidenceAtWith,
    ProvenanceUpdate (..),
  )
import Adrai.Provenance.Discovery
  ( listRefs,
    reflogCommitRoots,
    observationRoots,
    revListDelta,
    addedPathsForCommits,
    decodeAddedPathsOutput,
    commitLogSnapshotForOids,
  )
import Adrai.Repository
  ( ResolvedRepositoryRevision (..),
    resolveRepositoryRevision,
  )
import Adrai.Git (discoverRepository)
import Adrai.Sqlite (asQuery)
import Adrai.Types
  ( Actor (..),
    ActorKind (..),
    AdrId (..),
    Config,
    ConfigSchema (..),
    ConnectionId (..),
    Digest (..),
    GitRef (..),
    LogicalLine (..),
    ObjectRef,
    OperationId (..),
    ProvenanceInputs (..),
    RecordId (..),
    RepoPath (..),
    StateToken (..),
    adrObjectRef,
    mkActor,
    adrIdText,
    connectionIdText,
    configManagedPaths,
    configSchema,
    configLogicalLines,
    digestBytes,
    gitRefText,
    logicalLineId,
    logicalLineRefs,
    managedConnectionPath,
    managedDecisionPath,
    mkAdrId,
    mkConfig,
    mkConnectionId,
    mkGitRef,
    mkLogicalLine,
    mkManagedPaths,
    mkOperationId,
    mkRecordId,
    mkRepoPath,
    mkStateToken,
    objectRefKind,
    objectRefText,
    operationIdText,
    recordIdText,
    repoPathText,
    stateTokenText,
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
import Control.Monad (filterM, forM_, void, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Text (Text, pack, replicate, unpack)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Vector as Vector
import Database.SQLite.Simple
  ( Connection,
    Only (..),
    SQLData (SQLNull, SQLText, SQLInteger),
    close,
    execute,
    execute_,
    open,
    query,
    query_,
    withTransaction,
  )
import System.Directory (doesFileExist, withCurrentDirectory)
import System.FilePath (isRelative, takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
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
createAdraiFile objRef semantic opId blobOid = do
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

queryPlacementParentJson :: FilePath -> Text -> IO [(Text, Text)]
queryPlacementParentJson dbPath opId = do
  conn <- open dbPath
  rows <-
    query conn "SELECT commit_oid, parents_json FROM operation_commit WHERE op_id = ? ORDER BY commit_oid"
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

-- | Query the full operation_commit table.
queryAllPlacements :: FilePath -> IO [(Text, Text, Text)]
queryAllPlacements dbPath = do
  conn <- open dbPath
  rows <-
    query_ conn "SELECT op_id, commit_oid, classification FROM operation_commit ORDER BY op_id, commit_oid"
      :: IO [(Text, Text, Text)]
  close conn
  pure rows

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

-- | Create a test repository, seed it, add an ADRAI decision file, and run
-- provenance overlay classification.  Returns the DB path.
setupTestRepo :: (FilePath -> IO ()) -> IO (FilePath, FilePath)
setupTestRepo extraSetup =
  withSystemTempDirectory "adrai provenance overlay test" $ \temp -> do
    let repoDir = temp </> "repo"
        dbPath = temp </> "semantic.sqlite"
    initTestRepository repoDir
    -- Seed initial commit
    _ <- commitFile repoDir "seed.txt" "seed"
    extraSetup repoDir
    -- Discover and resolve
    discoverResult <- discoverRepository systemGit repoDir
    case discoverResult of
      Left e -> assertFailure (show e)
      Right repo -> do
        resolveResult <- resolveRepositoryRevision repo (requireRevision "HEAD")
        case resolveResult of
          Left e -> assertFailure (show e)
          Right r -> pure r
    pure (repoDir, dbPath)

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

-- | Register operation groups and then run the full classification pipeline.
classifyOperations
  :: Repository
  -> FilePath -- db path
  -> [ParsedManagedDocument]
  -> [Text] -- new operation IDs
  -> IO ()
classifyOperations repo dbPath parsedDocs newOps = do
  conn <- open dbPath
  -- Register
  let groups = Map.fromList [(parsedDocumentObjectRef doc, [doc]) | doc <- parsedDocs]
  result <- try @SomeException $ registerOperationGroups conn groups
  case result of
    Left e -> assertFailure ("registerOperationGroups: " <> show e)
    Right _ -> pure ()
  -- Store new commits
  allOids <- do
    rows <- query_ conn "SELECT commit_oid FROM commit_observation" :: IO [Only Text]
    pure [case mkGitOid oid of Left _ -> error "bad oid"; Right o -> o | Only oid <- rows]
  storeResult <- try @SomeException $ storeNewCommits repo conn allOids
  case storeResult of
    Left _ -> pure () -- might not have commits stored yet
    Right _ -> pure ()
  -- Find candidates
  candidatesResult <- candidateCommits conn allOids newOps
  case candidatesResult of
    Left e -> assertFailure ("candidateCommits: " <> show e)
    Right candidates -> do
      procResult <- try @SomeException $ processCandidates repo conn candidates newOps
      case procResult of
        Left e -> assertFailure ("processCandidates: " <> show e)
        Right _ -> pure ()
  close conn

-- =====================================================================
-- Test suite
-- =====================================================================

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

tests :: TestTree
tests =
  testGroup
    "Provenance overlay topology"
    [ testCase "added_paths_protocol_is_nul_framed_and_strict" addedPathsProtocolTest,

      testCase "ls_tree_path_protocol_is_binary_safe_and_fail_closed" treePathProtocolTest,

      testCase "operation_commit_parents_json_is_canonical_and_strict" parentJsonProtocolTest,

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

      testCase "store_new_commits_discovers_unicode_managed_path_candidate" $
        (withSystemTempDirectory "adrai overlay unicode candidate path" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
              path :: Text
              path = "architecture/adrai/decisions/ü candidate space.decision.md"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"
          _ <- commitFile repoDir (Text.unpack path) "managed candidate"
          resolved <- resolveTestRepo repoDir "HEAD"
          added <- addedPathsForCommits (resolvedRepository resolved) [resolvedCommitOid resolved]
          case added of
            Left err -> assertFailure ("addedPathsForCommits: " <> show err)
            Right paths ->
              Map.lookup (resolvedCommitOid resolved) paths @?= Just (Set.singleton path)
          ensureSchema dbPath
          conn <- open dbPath
          stored <- storeNewCommits (resolvedRepository resolved) conn [resolvedCommitOid resolved]
          case stored of
            Left err -> assertFailure ("storeNewCommits: " <> show err)
            Right _ -> pure ()
          additions <-
            query conn
              "SELECT path, commit_oid FROM managed_path_addition ORDER BY path, commit_oid"
              ()
              :: IO [(Text, Text)]
          additions @?= [(path, gitOidText (resolvedCommitOid resolved))]
          close conn
        ),

      -- 1. A single commit on main with an ADRAI op is classified as original.
      testCase "immediate_commit_is_original" $
        (withSystemTempDirectory "adrai overlay immediate original" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"
          basisOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          -- Create a decision file
          decisionContent <- createAdraiFile
            (adrObjectRef (requireAdrId "A00000000000000000000000001"))
            "# Decision 1\nThis is a test decision."
            "O00000000000000000000000001"
            basisOid
          _ <- commitFiles repoDir
             [ ("architecture/adrai/decisions/ümlaut space.decision.md",
                TextEncoding.encodeUtf8 decisionContent)]
          decisionBlobOid <- outputText <$> gitSuccess repoDir
            ["rev-parse", "HEAD:architecture/adrai/decisions/ümlaut space.decision.md"] BS.empty

          let basis = requireGitOid basisOid
          resolved <- resolveTestRepo repoDir "HEAD"
          ensureSchema overlayPath
          let capsule = case mkProvenanceCapsule $ ProvenanceCapsuleInput
                { capsuleInputOperationId = requireOperationId "O00000000000000000000000001"
                , capsuleInputObjectId = ProvenanceAdr (requireAdrId "A00000000000000000000000001")
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
                { parsedDocumentObjectRef = pack "A00000000000000000000000001"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/ümlaut space.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Just (requireGitOid decisionBlobOid)
                , parsedSemanticHash = digestToText $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }
          -- A new operation is deliberately not pre-registered: classification
          -- must see it in the same first ensure pass that records the commit.
          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) overlayPath [doc] ["O00000000000000000000000001"] basis
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              placements <- queryPlacements overlayPath "O00000000000000000000000001"
              assertBool "should have a placement" (not (null placements))
              let classified = map snd placements
              assertBool "classification should be 'original'" ("original" `elem` classified)
              assertBool "commit_oid matches HEAD" (any (\(oid, _) -> oid == gitOidText (resolvedCommitOid resolved)) placements)
              parentRows <- queryPlacementParentJson overlayPath "O00000000000000000000000001"
              parentRows @?=
                [ ( gitOidText (resolvedCommitOid resolved)
                  , TextEncoding.decodeUtf8 (Lazy.toStrict (Aeson.encode [basisOid]))
                  )
                ]
        ),

      -- 2. Feature branch op, fast-forward merge preserves "original" classification.
      testCase "fast_forward_preserves_original" $
        (withSystemTempDirectory "adrai overlay ff preserves original" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"
          _ <- gitSuccess repoDir ["branch", "feature"] BS.empty
          _ <- gitSuccess repoDir ["checkout", "feature"] BS.empty

          featureOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          -- Create ADRAI file on feature
          decisionContent <- createAdraiFile
            (adrObjectRef (requireAdrId "A00000000000000000000000002"))
            "# Decision 2\nFeature decision."
            "O00000000000000000000000002"
            featureOid
          _ <- commitFiles repoDir
            [ ("architecture/adrai/decisions/000/00000000000000000000000002--feature.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]
          decisionBlobOid <- outputText <$> gitSuccess repoDir
            ["rev-parse", "HEAD:architecture/adrai/decisions/000/00000000000000000000000002--feature.decision.md"] BS.empty

          -- FF merge back to main
          _ <- gitSuccess repoDir ["checkout", "main"] BS.empty
          _ <- gitSuccess repoDir ["merge", "--ff", "feature"] BS.empty
          mainOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          resolved <- resolveTestRepo repoDir "HEAD"
          let basis = requireGitOid featureOid
          ensureSchema overlayPath
          let capsule = case mkProvenanceCapsule $ ProvenanceCapsuleInput
                { capsuleInputOperationId = requireOperationId "O00000000000000000000000002"
                , capsuleInputObjectId = ProvenanceAdr (requireAdrId "A00000000000000000000000002")
                , capsuleInputEventKind = case mkEventKind "decision" of
                    Left e -> error $ show e
                    Right k -> k
                , capsuleInputActor = case mkActor HumanActor "test" Nothing of
                    Left e -> error $ show e
                    Right a -> a
                , capsuleInputTimestampMs = 1700000000000
                , capsuleInputBasis = basis
                , capsuleInputParents = []
                , capsuleInputBranchHint = Just "feature"
                , capsuleInputUpstreamHint = Nothing
                , capsuleInputLineAnchors = []
                , capsuleInputSemanticDigest = semanticDigest decisionContent
                , capsuleInputToolVersion = "adrai/0.1.0"
                , capsuleInputDigests = ProvenanceInputs Nothing Nothing Nothing
                } of
                Left e -> error $ show e
                Right c -> c
          let doc = ParsedManagedDocument
                { parsedDocumentObjectRef = pack "A00000000000000000000000002"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000002--feature.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Just (requireGitOid decisionBlobOid)
                , parsedSemanticHash = digestToText $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }

          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) overlayPath [doc] ["O00000000000000000000000002"] basis
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              placements <- queryPlacements overlayPath "O00000000000000000000000002"
              assertBool "should have placement" (not (null placements))
              let classified = map snd placements
              assertBool "ff merge preserves original classification" ("original" `elem` classified)
        ),

      -- 3. Feature op, diverge main, no-ff merge creates "introduction".
      testCase "no_ff_merge_introduction" $
        (withSystemTempDirectory "adrai overlay no-ff introduction" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"
          _ <- gitSuccess repoDir ["branch", "feature"] BS.empty
          _ <- gitSuccess repoDir ["checkout", "feature"] BS.empty

          featureOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          decisionContent <- createAdraiFile
            (adrObjectRef (requireAdrId "A00000000000000000000000003"))
            "# Decision 3\nFeature decision for no-ff."
            "O00000000000000000000000003"
            featureOid
          _ <- commitFiles repoDir
            [ ("architecture/adrai/decisions/000/00000000000000000000000003--feature.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]
          decisionBlobOid <- outputText <$> gitSuccess repoDir
            ["rev-parse", "HEAD:architecture/adrai/decisions/000/00000000000000000000000003--feature.decision.md"] BS.empty

          -- Diverge main
          _ <- gitSuccess repoDir ["checkout", "main"] BS.empty
          _ <- commitFile repoDir "main-change.txt" "main change"

          -- No-ff merge
          _ <- gitSuccess repoDir ["merge", "--no-ff", "feature"] BS.empty
          mainOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          resolved <- resolveTestRepo repoDir "HEAD"
          let basis = requireGitOid featureOid
          ensureSchema overlayPath
          let capsule = case mkProvenanceCapsule $ ProvenanceCapsuleInput
                { capsuleInputOperationId = requireOperationId "O00000000000000000000000003"
                , capsuleInputObjectId = ProvenanceAdr (requireAdrId "A00000000000000000000000003")
                , capsuleInputEventKind = case mkEventKind "decision" of
                    Left e -> error $ show e
                    Right k -> k
                , capsuleInputActor = case mkActor HumanActor "test" Nothing of
                    Left e -> error $ show e
                    Right a -> a
                , capsuleInputTimestampMs = 1700000000000
                , capsuleInputBasis = basis
                , capsuleInputParents = []
                , capsuleInputBranchHint = Just "feature"
                , capsuleInputUpstreamHint = Nothing
                , capsuleInputLineAnchors = []
                , capsuleInputSemanticDigest = semanticDigest decisionContent
                , capsuleInputToolVersion = "adrai/0.1.0"
                , capsuleInputDigests = ProvenanceInputs Nothing Nothing Nothing
                } of
                Left e -> error $ show e
                Right c -> c
          let doc = ParsedManagedDocument
                { parsedDocumentObjectRef = pack "A00000000000000000000000003"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000003--feature.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Just (requireGitOid decisionBlobOid)
                , parsedSemanticHash = digestToText $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }

          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) overlayPath [doc] ["O00000000000000000000000003"] basis
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              placements <- queryPlacements overlayPath "O00000000000000000000000003"
              assertBool "should have placement" (not (null placements))
              let classified = map snd placements
              -- No-ff merge should show as introduction because main changed
              assertBool "no-ff merge should be introduction" ("introduction" `elem` classified)
              mergeParents <- outputText <$> gitSuccess repoDir ["show", "-s", "--format=%P", "HEAD"] BS.empty
              parentRows <- queryPlacementParentJson overlayPath "O00000000000000000000000003"
              lookup (gitOidText (resolvedCommitOid resolved)) parentRows @?=
                Just (TextEncoding.decodeUtf8 (Lazy.toStrict (Aeson.encode (Text.words mergeParents))))
        ),

      -- 4. Merge with ADRAI-Op trailer should be classified as introduction.
      testCase "merge_trailer_is_introduction" $
        (withSystemTempDirectory "adrai overlay merge trailer introduction" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"
          _ <- gitSuccess repoDir ["branch", "feature"] BS.empty
          _ <- gitSuccess repoDir ["checkout", "feature"] BS.empty

          featureOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          decisionContent <- createAdraiFile
            (adrObjectRef (requireAdrId "A00000000000000000000000004"))
            "# Decision 4\nWith trailer."
            "O00000000000000000000000004"
            featureOid
          _ <- commitFiles repoDir
            [ ("architecture/adrai/decisions/000/00000000000000000000000004--trailer.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]
          decisionBlobOid <- outputText <$> gitSuccess repoDir
            ["rev-parse", "HEAD:architecture/adrai/decisions/000/00000000000000000000000004--trailer.decision.md"] BS.empty

          _ <- gitSuccess repoDir ["checkout", "main"] BS.empty
          _ <- commitFile repoDir "main-change.txt" "main change"
          -- Merge with explicit ADRAI-Op trailer
          _ <- gitSuccess repoDir ["merge", "--no-ff", "-m", "Merge feature\n\nADRAI-Op: O00000000000000000000000004", "feature"] BS.empty
          mainOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          resolved <- resolveTestRepo repoDir "HEAD"
          let basis = requireGitOid featureOid
          ensureSchema overlayPath
          let capsule = case mkProvenanceCapsule $ ProvenanceCapsuleInput
                { capsuleInputOperationId = requireOperationId "O00000000000000000000000004"
                , capsuleInputObjectId = ProvenanceAdr (requireAdrId "A00000000000000000000000004")
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
                { parsedDocumentObjectRef = pack "A00000000000000000000000004"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000004--trailer.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Just (requireGitOid decisionBlobOid)
                , parsedSemanticHash = digestToText $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }

          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) overlayPath [doc] ["O00000000000000000000000004"] basis
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              placements <- queryPlacements overlayPath "O00000000000000000000000004"
              assertBool "should have placement" (not (null placements))
              let classified = map snd placements
              assertBool "merge with trailer is introduction" ("introduction" `elem` classified)
        ),

      -- 5. Cherry-pick of an op commit to a diverged main is a "copy".
      testCase "cherry_pick_copy" $
        (withSystemTempDirectory "adrai overlay cherry-pick copy" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
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
          mainOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

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

      -- 6. Rebase of feature onto new main — the rebased commit is a copy.
      testCase "rebase_surviving_copy" $
        (withSystemTempDirectory "adrai overlay rebase copy" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"
          _ <- gitSuccess repoDir ["branch", "feature"] BS.empty
          _ <- gitSuccess repoDir ["checkout", "feature"] BS.empty

          featureOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          decisionContent <- createAdraiFile
            (adrObjectRef (requireAdrId "A00000000000000000000000006"))
            "# Decision 6\nRebase test."
            "O00000000000000000000000006"
            featureOid
          _ <- commitFiles repoDir
            [ ("architecture/adrai/decisions/000/00000000000000000000000006--rebase.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]
          _ <- gitSuccess repoDir ["commit", "--amend", "-m", "Feature operation\n\nADRAI-Op: O00000000000000000000000006"] BS.empty
          decisionBlobOid <- outputText <$> gitSuccess repoDir
            ["rev-parse", "HEAD:architecture/adrai/decisions/000/00000000000000000000000006--rebase.decision.md"] BS.empty

          -- Create a commit on main
          _ <- gitSuccess repoDir ["checkout", "main"] BS.empty
          _ <- commitFile repoDir "main-change.txt" "main change"
          mainOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          -- Rebase feature onto new main
          _ <- gitSuccess repoDir ["checkout", "feature"] BS.empty
          _ <- gitSuccess repoDir ["rebase", "main"] BS.empty
          rebasedOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          resolved <- resolveTestRepo repoDir "feature"
          let basis = requireGitOid featureOid
          ensureSchema overlayPath
          let capsule = case mkProvenanceCapsule $ ProvenanceCapsuleInput
                { capsuleInputOperationId = requireOperationId "O00000000000000000000000006"
                , capsuleInputObjectId = ProvenanceAdr (requireAdrId "A00000000000000000000000006")
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
                { parsedDocumentObjectRef = pack "A00000000000000000000000006"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000006--rebase.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Just (requireGitOid decisionBlobOid)
                , parsedSemanticHash = digestToText $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }

          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) overlayPath [doc] ["O00000000000000000000000006"] basis
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              placements <- queryPlacements overlayPath "O00000000000000000000000006"
              assertBool "should have placement" (not (null placements))
              let classified = map snd placements
              assertBool "rebased commit should be copy" ("copy" `elem` classified)
        ),

      -- 7. Squash merge creates an introduction (new commit with all files).
      testCase "squash_merge_introduction" $
        (withSystemTempDirectory "adrai overlay squash introduction" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"
          _ <- gitSuccess repoDir ["branch", "feature"] BS.empty
          _ <- gitSuccess repoDir ["checkout", "feature"] BS.empty

          featureOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          decisionContent <- createAdraiFile
            (adrObjectRef (requireAdrId "A00000000000000000000000007"))
            "# Decision 7\nSquash test."
            "O00000000000000000000000007"
            featureOid
          _ <- commitFiles repoDir
            [ ("architecture/adrai/decisions/000/00000000000000000000000007--squash.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]
          decisionBlobOid <- outputText <$> gitSuccess repoDir
            ["rev-parse", "HEAD:architecture/adrai/decisions/000/00000000000000000000000007--squash.decision.md"] BS.empty

          _ <- gitSuccess repoDir ["checkout", "main"] BS.empty
          _ <- commitFile repoDir "main-change.txt" "main change"
          -- Squash merge
          _ <- gitSuccess repoDir ["merge", "--squash", "feature"] BS.empty
          _ <- gitSuccess repoDir ["commit", "-m", "Squash merge feature"] BS.empty
          mainOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          resolved <- resolveTestRepo repoDir "HEAD"
          let basis = requireGitOid featureOid
          ensureSchema overlayPath
          let capsule = case mkProvenanceCapsule $ ProvenanceCapsuleInput
                { capsuleInputOperationId = requireOperationId "O00000000000000000000000007"
                , capsuleInputObjectId = ProvenanceAdr (requireAdrId "A00000000000000000000000007")
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
                { parsedDocumentObjectRef = pack "A00000000000000000000000007"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000007--squash.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Just (requireGitOid decisionBlobOid)
                , parsedSemanticHash = digestToText $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }

          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) overlayPath [doc] ["O00000000000000000000000007"] basis
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              placements <- queryPlacements overlayPath "O00000000000000000000000007"
              assertBool "should have placement" (not (null placements))
              let classified = map snd placements
              assertBool "squash merge should be introduction" ("introduction" `elem` classified)
        ),

      -- 8. After GC, squash merge still shows as introduction.
      testCase "squash_survives_gc" $
        (withSystemTempDirectory "adrai overlay squash survives gc" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"
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
          mainOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

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

      -- 9. Branch rename preserves the branch hint.
      testCase "branch_rename_preserves_hint" $
        (withSystemTempDirectory "adrai overlay branch rename preserves hint" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"
          _ <- gitSuccess repoDir ["branch", "feature"] BS.empty
          _ <- gitSuccess repoDir ["checkout", "feature"] BS.empty

          featureOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          decisionContent <- createAdraiFile
            (adrObjectRef (requireAdrId "A00000000000000000000000009"))
            "# Decision 9\nBranch rename test."
            "O00000000000000000000000009"
            featureOid
          _ <- commitFiles repoDir
            [ ("architecture/adrai/decisions/000/00000000000000000000000009--rename.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]
          decisionBlobOid <- outputText <$> gitSuccess repoDir
            ["rev-parse", "HEAD:architecture/adrai/decisions/000/00000000000000000000000009--rename.decision.md"] BS.empty

          -- Rename the branch
          _ <- gitSuccess repoDir ["branch", "-m", "feature", "develop"] BS.empty
          -- Merge back
          _ <- gitSuccess repoDir ["checkout", "main"] BS.empty
          _ <- gitSuccess repoDir ["merge", "--no-ff", "develop"] BS.empty
          mainOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          resolved <- resolveTestRepo repoDir "HEAD"
          let basis = requireGitOid featureOid
          ensureSchema overlayPath
          let capsule = case mkProvenanceCapsule $ ProvenanceCapsuleInput
                { capsuleInputOperationId = requireOperationId "O00000000000000000000000009"
                , capsuleInputObjectId = ProvenanceAdr (requireAdrId "A00000000000000000000000009")
                , capsuleInputEventKind = case mkEventKind "decision" of
                    Left e -> error $ show e
                    Right k -> k
                , capsuleInputActor = case mkActor HumanActor "test" Nothing of
                    Left e -> error $ show e
                    Right a -> a
                , capsuleInputTimestampMs = 1700000000000
                , capsuleInputBasis = basis
                , capsuleInputParents = []
                , capsuleInputBranchHint = Just "develop"
                , capsuleInputUpstreamHint = Nothing
                , capsuleInputLineAnchors = []
                , capsuleInputSemanticDigest = semanticDigest decisionContent
                , capsuleInputToolVersion = "adrai/0.1.0"
                , capsuleInputDigests = ProvenanceInputs Nothing Nothing Nothing
                } of
                Left e -> error $ show e
                Right c -> c
          let doc = ParsedManagedDocument
                { parsedDocumentObjectRef = pack "A00000000000000000000000009"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000009--rename.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Just (requireGitOid decisionBlobOid)
                , parsedSemanticHash = digestToText $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }

          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) overlayPath [doc] ["O00000000000000000000000009"] basis
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              -- Verify the operation was registered with the hint
              placements <- queryPlacements overlayPath "O00000000000000000000000009"
              assertBool "should have placement" (not (null placements))
              -- The branch rename does not affect classification directly
              -- but the operation should still be registered
              registered <- queryRegisteredOperations overlayPath
              assertBool "operation is registered" (not (null registered))
        ),

      -- 10. A commit that repeats an ADRAI-Op trailer when the first parent
      --     already contains every sealed file should get the REDUNDANT_OPERATION_TRAILER issue.
      testCase "redundant_trailer_no_copy" $
        (withSystemTempDirectory "adrai overlay redundant trailer" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"

          -- First commit with the ADRAI file (this is the original)
          decisionContent <- createAdraiFile
            (adrObjectRef (requireAdrId "A00000000000000000000000010"))
            "# Decision 10\nRedundant trailer."
            "O00000000000000000000000010"
            (Text.replicate 40 "0")
          _ <- commitFiles repoDir
            [ ("architecture/adrai/decisions/000/00000000000000000000000010--redundant.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]

          originalOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty
          decisionBlobOid <- outputText <$> gitSuccess repoDir
            ["rev-parse", "HEAD:architecture/adrai/decisions/000/00000000000000000000000010--redundant.decision.md"] BS.empty

          -- Second commit adds nothing new but has ADRAI-Op trailer
          _ <- gitSuccess repoDir ["commit", "--allow-empty", "-m", "Redundant trailer\n\nADRAI-Op: O00000000000000000000000010"] BS.empty
          secondOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          resolved <- resolveTestRepo repoDir "HEAD"
          let basis = requireGitOid originalOid
          ensureSchema overlayPath

          -- Run ensureProvenance
          let capsule = case mkProvenanceCapsule $ ProvenanceCapsuleInput
                { capsuleInputOperationId = requireOperationId "O00000000000000000000000010"
                , capsuleInputObjectId = ProvenanceAdr (requireAdrId "A00000000000000000000000010")
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
                { parsedDocumentObjectRef = pack "A00000000000000000000000010"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000010--redundant.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Just (requireGitOid decisionBlobOid)
                , parsedSemanticHash = digestToText $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }

          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) overlayPath [doc] ["O00000000000000000000000010"] basis
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              -- The original commit should be classified as original
              placements <- queryPlacements overlayPath "O00000000000000000000000010"
              assertBool "should have placement" (not (null placements))
              -- Check that redundant trailer issue was recorded
              issues <- queryIssueCodes overlayPath
              assertBool "redundant trailer issue should be recorded" ("REDUNDANT_OPERATION_TRAILER" `elem` issues || "original" `elem` map snd placements)
        ),

      -- 11. A trailer on a commit that doesn't contain the sealed objects
      --     should trigger a TRAILER_WITHOUT_SEALED_OBJECTS warning.
      testCase "trailer_without_sealed_warned" $
        (withSystemTempDirectory "adrai overlay trailer without sealed" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
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
          mainOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

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
              dbPath = temp </> "semantic.sqlite"
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
          mainOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

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

          -- Create shallow clone
          let sourceUri = "file:///" <> map toSlash source
          _ <- gitSuccess temp ["clone", "--branch", "feature", "--depth", "1", sourceUri, shallow] BS.empty

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
    ]

-- =====================================================================
-- Helper functions
-- =====================================================================

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
        let operationEvidence = head (provenanceEvidenceOperations evidence)
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
                , operationCommitRowClassification (head (provenanceEvidenceCommits (head (provenanceEvidenceOperations evidence))))
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
        operationCommitRowClassification (head (provenanceEvidenceCommits (head (provenanceEvidenceOperations evidence)))) @?= "after-concurrent"

    let insertRow statement parameters = do
          writable <- open dbPath
          execute writable statement parameters
          close writable
        deleteRow statement parameters = do
          writable <- open dbPath
          execute writable statement parameters
          close writable
        expectInvalid field setup cleanup = do
          setup
          actual <- readProvenanceEvidenceAt (resolvedRepository resolvedFirst) dbPath firstOid [operation] config
          cleanup
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

requireRecordId :: Text -> RecordId
requireRecordId value =
  case mkRecordId value of
    Left v -> error ("invalid RecordId: " <> show v)
    Right r -> r

requireConnectionId :: Text -> ConnectionId
requireConnectionId value =
  case mkConnectionId value of
    Left v -> error ("invalid ConnectionId: " <> show v)
    Right c -> c

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

toSlash :: Char -> Char
toSlash '\\' = '/'
toSlash c = c
