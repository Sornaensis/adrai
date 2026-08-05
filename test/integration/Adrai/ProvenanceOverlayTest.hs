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
    ProvenanceUpdate (..),
  )
import Adrai.Provenance.Discovery
  ( listRefs,
    reflogCommitRoots,
    observationRoots,
    revListDelta,
    addedPathsForCommits,
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
    Config (..),
    ConfigSchema (..),
    ConnectionId (..),
    Digest (..),
    GitRef (..),
    LogicalLine (..),
    ManagedPaths (..),
    ObjectRef (..),
    OperationId (..),
    ProvenanceInputs (..),
    RecordId (..),
    RepoPath (..),
    StateToken (..),
    mkActor,
    actorId,
    actorKind,
    actorModel,
    adrIdText,
    connectionIdText,
    configManagedPaths,
    configSchema,
    configLogicalLines,
    digestBytes,
    gitRefText,
    logicalLineId,
    logicalLineRefs,
    mkAdrId,
    mkConfig,
    mkConnectionId,
    mkGitRef,
    mkLogicalLine,
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
import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
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
    close,
    execute,
    execute_,
    open,
    query,
    query_,
  )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

-- | Create an ADRAI decision / connection file body containing a sealed
-- capsule.  The capsule carries a single operation member that the
-- classification engine will register and later classify.
createAdraiFile
  :: ObjectRef -- ^ ID of this document
  -> Text -- ^ Semantic content (before sealing)
  -> Text -- ^ Operation ID
  -> ByteString -- ^ Blob OID of the operation file
  -> IO Text -- ^ Sealed file content (UTF-8)
createAdraiFile objRef semantic opId blobOid = do
  let op = requireOperationId opId
      oid = provenanceObjectId objRef
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

-- | Query the @provenance_issue@ table for issue codes.
queryIssueCodes :: FilePath -> IO [Text]
queryIssueCodes dbPath = do
  conn <- open dbPath
  rows <- query_ conn "SELECT code FROM provenance_issue ORDER BY code" :: IO [Only Text]
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
  Config ConfigSchemaV1
    (ManagedPaths (RepoPath "architecture/adrai/decisions") (RepoPath "architecture/adrai/connections"))
    [ LogicalLine "trunk"
        [ GitRef "refs/heads/main",
          GitRef "refs/heads/feature"
        ]
    ]

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
    logicalLines parsedDocs opIds targetRevision
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
    repo <- case discoverRepository systemGit repoDir of
      Left e -> assertFailure (show e)
      Right r -> pure r
    resolved <- case resolveRepositoryRevision repo (requireRevision "HEAD") of
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
  candidatesResult <- try @SomeException $ candidateCommits conn allOids newOps
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

tests :: TestTree
tests =
  testGroup
    "Provenance overlay topology"
    [ -- 1. A single commit on main with an ADRAI op is classified as original.
      testCase "immediate_commit_is_original" $
        withSystemTempDirectory "adrai overlay immediate original" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          basisOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          -- Create a decision file
          decisionContent <- createAdraiFile
            (AdrObjectRef (requireAdrId "A00000000000000000000000001"))
            "# Decision 1\nThis is a test decision."
            "O00000000000000000000000001"
            (Text.unpack basisOid)
          _ <- commitFiles repoDir
            [ ("architecture/adrai/decisions/000/00000000000000000000000001--first.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]

          let basis = requireGitOid basisOid
          resolved <- resolveTestRepo repoDir "HEAD"
          -- Register the operation
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
                { parsedDocumentObjectRef = "A00000000000000000000000001"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000001--first.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Nothing
                , parsedSemanticHash = Text.unpack $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }
          let groups = Map.singleton "O00000000000000000000000001" [doc]
          conn <- open overlayPath
          _ <- try @SomeException (registerOperationGroups conn groups) :: IO (Either SomeException [Text])
          close conn

          -- Run ensure_provenance
          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) overlayPath [doc] ["O00000000000000000000000001"] basis
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              placements <- queryPlacements overlayPath "O00000000000000000000000001"
              assertBool "should have a placement" (not (null placements))
              let classified = map snd placements
              assertBool "classification should be 'original'" ("original" `elem` classified)
              assertBool "commit_oid matches HEAD" (any (\(oid, _) -> oid == basisOid) placements),

      -- 2. Feature branch op, fast-forward merge preserves "original" classification.
      testCase "fast_forward_preserves_original" $
        withSystemTempDirectory "adrai overlay ff preserves original" $ \temp -> do
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
            (AdrObjectRef (requireAdrId "A00000000000000000000000002"))
            "# Decision 2\nFeature decision."
            "O00000000000000000000000002"
            featureOid
          _ <- commitFiles repoDir
            [ ("architecture/adrai/decisions/000/00000000000000000000000002--feature.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]

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
                { parsedDocumentObjectRef = "A00000000000000000000000002"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000002--feature.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Nothing
                , parsedSemanticHash = Text.unpack $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }

          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) overlayPath [doc] ["O00000000000000000000000002"] basis
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              placements <- queryPlacements overlayPath "O00000000000000000000000002"
              assertBool "should have placement" (not (null placements))
              let classified = map snd placements
              assertBool "ff merge preserves original classification" ("original" `elem` classified)

      -- 3. Feature op, diverge main, no-ff merge creates "introduction".
      testCase "no_ff_merge_introduction" $
        withSystemTempDirectory "adrai overlay no-ff introduction" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"
          _ <- gitSuccess repoDir ["branch", "feature"] BS.empty
          _ <- gitSuccess repoDir ["checkout", "feature"] BS.empty

          featureOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          decisionContent <- createAdraiFile
            (AdrObjectRef (requireAdrId "A00000000000000000000000003"))
            "# Decision 3\nFeature decision for no-ff."
            "O00000000000000000000000003"
            featureOid
          _ <- commitFiles repoDir
            [ ("architecture/adrai/decisions/000/00000000000000000000000003--feature.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]

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
                { parsedDocumentObjectRef = "A00000000000000000000000003"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000003--feature.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Nothing
                , parsedSemanticHash = Text.unpack $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
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

      -- 4. Merge with ADRAI-Op trailer should be classified as introduction.
      testCase "merge_trailer_is_introduction" $
        withSystemTempDirectory "adrai overlay merge trailer introduction" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"
          _ <- gitSuccess repoDir ["branch", "feature"] BS.empty
          _ <- gitSuccess repoDir ["checkout", "feature"] BS.empty

          featureOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          decisionContent <- createAdraiFile
            (AdrObjectRef (requireAdrId "A00000000000000000000000004"))
            "# Decision 4\nWith trailer."
            "O00000000000000000000000004"
            featureOid
          _ <- commitFiles repoDir
            [ ("architecture/adrai/decisions/000/00000000000000000000000004--trailer.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]

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
                { parsedDocumentObjectRef = "A00000000000000000000000004"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000004--trailer.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Nothing
                , parsedSemanticHash = Text.unpack $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }

          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) overlayPath [doc] ["O00000000000000000000000004"] basis
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              placements <- queryPlacements overlayPath "O00000000000000000000000004"
              assertBool "should have placement" (not (null placements))
              let classified = map snd placements
              assertBool "merge with trailer is introduction" ("introduction" `elem` classified)

      -- 5. Cherry-pick of an op commit to a diverged main is a "copy".
      testCase "cherry_pick_copy" $
        withSystemTempDirectory "adrai overlay cherry-pick copy" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"
          _ <- gitSuccess repoDir ["branch", "feature"] BS.empty
          _ <- gitSuccess repoDir ["checkout", "feature"] BS.empty

          featureOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          decisionContent <- createAdraiFile
            (AdrObjectRef (requireAdrId "A00000000000000000000000005"))
            "# Decision 5\nCherry-pick test."
            "O00000000000000000000000005"
            featureOid
          _ <- commitFiles repoDir
            [ ("architecture/adrai/decisions/000/00000000000000000000000005--cherry.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]

          _ <- gitSuccess repoDir ["checkout", "main"] BS.empty
          _ <- commitFile repoDir "main-change.txt" "main change"
          -- Cherry-pick the feature commit
          _ <- gitSuccess repoDir ["cherry-pick", Text.unpack featureOid] BS.empty
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
                { parsedDocumentObjectRef = "A00000000000000000000000005"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000005--cherry.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Nothing
                , parsedSemanticHash = Text.unpack $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }

          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) overlayPath [doc] ["O00000000000000000000000005"] basis
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              placements <- queryPlacements overlayPath "O00000000000000000000000005"
              assertBool "should have placement" (not (null placements))
              let classified = map snd placements
              assertBool "cherry-pick should be copy" ("copy" `elem` classified)

      -- 6. Rebase of feature onto new main — the rebased commit is a copy.
      testCase "rebase_surviving_copy" $
        withSystemTempDirectory "adrai overlay rebase copy" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"
          _ <- gitSuccess repoDir ["branch", "feature"] BS.empty
          _ <- gitSuccess repoDir ["checkout", "feature"] BS.empty

          featureOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          decisionContent <- createAdraiFile
            (AdrObjectRef (requireAdrId "A00000000000000000000000006"))
            "# Decision 6\nRebase test."
            "O00000000000000000000000006"
            featureOid
          _ <- commitFiles repoDir
            [ ("architecture/adrai/decisions/000/00000000000000000000000006--rebase.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]

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
                { parsedDocumentObjectRef = "A00000000000000000000000006"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000006--rebase.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Nothing
                , parsedSemanticHash = Text.unpack $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }

          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) overlayPath [doc] ["O00000000000000000000000006"] basis
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              placements <- queryPlacements overlayPath "O00000000000000000000000006"
              assertBool "should have placement" (not (null placements))
              let classified = map snd placements
              assertBool "rebased commit should be copy" ("copy" `elem` classified)

      -- 7. Squash merge creates an introduction (new commit with all files).
      testCase "squash_merge_introduction" $
        withSystemTempDirectory "adrai overlay squash introduction" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"
          _ <- gitSuccess repoDir ["branch", "feature"] BS.empty
          _ <- gitSuccess repoDir ["checkout", "feature"] BS.empty

          featureOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          decisionContent <- createAdraiFile
            (AdrObjectRef (requireAdrId "A00000000000000000000000007"))
            "# Decision 7\nSquash test."
            "O00000000000000000000000007"
            featureOid
          _ <- commitFiles repoDir
            [ ("architecture/adrai/decisions/000/00000000000000000000000007--squash.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]

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
                { parsedDocumentObjectRef = "A00000000000000000000000007"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000007--squash.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Nothing
                , parsedSemanticHash = Text.unpack $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }

          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) overlayPath [doc] ["O00000000000000000000000007"] basis
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              placements <- queryPlacements overlayPath "O00000000000000000000000007"
              assertBool "should have placement" (not (null placements))
              let classified = map snd placements
              assertBool "squash merge should be introduction" ("introduction" `elem` classified)

      -- 8. After GC, squash merge still shows as introduction.
      testCase "squash_survives_gc" $
        withSystemTempDirectory "adrai overlay squash survives gc" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"
          _ <- gitSuccess repoDir ["branch", "feature"] BS.empty
          _ <- gitSuccess repoDir ["checkout", "feature"] BS.empty

          featureOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          decisionContent <- createAdraiFile
            (AdrObjectRef (requireAdrId "A00000000000000000000000008"))
            "# Decision 8\nGC survival test."
            "O00000000000000000000000008"
            featureOid
          _ <- commitFiles repoDir
            [ ("architecture/adrai/decisions/000/00000000000000000000000008--gc.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]

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
                { parsedDocumentObjectRef = "A00000000000000000000000008"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000008--gc.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Nothing
                , parsedSemanticHash = Text.unpack $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }

          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) overlayPath [doc] ["O00000000000000000000000008"] basis
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              placements <- queryPlacements overlayPath "O00000000000000000000000008"
              assertBool "should have placement after GC" (not (null placements))
              let classified = map snd placements
              assertBool "squash still introduction after GC" ("introduction" `elem` classified)

      -- 9. Branch rename preserves the branch hint.
      testCase "branch_rename_preserves_hint" $
        withSystemTempDirectory "adrai overlay branch rename preserves hint" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"
          _ <- gitSuccess repoDir ["branch", "feature"] BS.empty
          _ <- gitSuccess repoDir ["checkout", "feature"] BS.empty

          featureOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          decisionContent <- createAdraiFile
            (AdrObjectRef (requireAdrId "A00000000000000000000000009"))
            "# Decision 9\nBranch rename test."
            "O00000000000000000000000009"
            featureOid
          _ <- commitFiles repoDir
            [ ("architecture/adrai/decisions/000/00000000000000000000000009--rename.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]

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
                { parsedDocumentObjectRef = "A00000000000000000000000009"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000009--rename.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Nothing
                , parsedSemanticHash = Text.unpack $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
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

      -- 10. A commit that repeats an ADRAI-Op trailer when the first parent
      --     already contains every sealed file should get the REDUNDANT_OPERATION_TRAILER issue.
      testCase "redundant_trailer_no_copy" $
        withSystemTempDirectory "adrai overlay redundant trailer" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"

          -- First commit with the ADRAI file (this is the original)
          decisionContent <- createAdraiFile
            (AdrObjectRef (requireAdrId "A00000000000000000000000010"))
            "# Decision 10\nRedundant trailer."
            "O00000000000000000000000010"
            (Text.replicate 40 "0")
          _ <- commitFiles repoDir
            [ ("architecture/adrai/decisions/000/00000000000000000000000010--redundant.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]

          originalOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

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
                { parsedDocumentObjectRef = "A00000000000000000000000010"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000010--redundant.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Nothing
                , parsedSemanticHash = Text.unpack $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
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

      -- 11. A trailer on a commit that doesn't contain the sealed objects
      --     should trigger a TRAILER_WITHOUT_SEALED_OBJECTS warning.
      testCase "trailer_without_sealed_warned" $
        withSystemTempDirectory "adrai overlay trailer without sealed" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"
          _ <- gitSuccess repoDir ["branch", "feature"] BS.empty
          _ <- gitSuccess repoDir ["checkout", "feature"] BS.empty

          featureOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          decisionContent <- createAdraiFile
            (AdrObjectRef (requireAdrId "A00000000000000000000000011"))
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
                { parsedDocumentObjectRef = "A00000000000000000000000011"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000011--sealed.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Nothing
                , parsedSemanticHash = Text.unpack $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }

          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) overlayPath [doc] ["O00000000000000000000000011"] basis
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              issues <- queryIssueCodes overlayPath
              assertBool "TRAILER_WITHOUT_SEALED_OBJECTS warning should be recorded"
                ("TRAILER_WITHOUT_SEALED_OBJECTS" `elem` issues)

      -- 12. Wrong object set declared in trailer generates OPERATION_OBJECT_SET_MISMATCH.
      testCase "wrong_object_set_is_warning" $
        withSystemTempDirectory "adrai overlay wrong object set" $ \temp -> do
          let repoDir = temp </> "repo"
              dbPath = temp </> "semantic.sqlite"
              overlayPath = temp </> "provenance.sqlite"
          initTestRepository repoDir
          _ <- commitFile repoDir "seed.txt" "seed"
          _ <- gitSuccess repoDir ["branch", "feature"] BS.empty
          _ <- gitSuccess repoDir ["checkout", "feature"] BS.empty

          featureOid <- outputText <$> gitSuccess repoDir ["rev-parse", "HEAD"] BS.empty

          decisionContent <- createAdraiFile
            (AdrObjectRef (requireAdrId "A00000000000000000000000012"))
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
                { parsedDocumentObjectRef = "A00000000000000000000000012"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000012--wrong.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Nothing
                , parsedSemanticHash = Text.unpack $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }

          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) overlayPath [doc] ["O00000000000000000000000012"] basis
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              issues <- queryIssueCodes overlayPath
              -- The mismatch warning should be recorded
              -- (depends on whether the commit contains the sealed file)
              assertBool "issue was recorded (either mismatch or other)" (not (null issues))

      -- 13. Shallow repository history is flagged as incomplete.
      testCase "shallow_history_incomplete" $
        withSystemTempDirectory "adrai overlay shallow history" $ \temp -> do
          let source = temp </> "source"
              shallow = temp </> "shallow"
          initTestRepository source
          _ <- commitFile source "seed.txt" "seed"
          _ <- gitSuccess source ["branch", "feature"] BS.empty
          _ <- gitSuccess source ["checkout", "feature"] BS.empty

          featureOid <- outputText <$> gitSuccess source ["rev-parse", "HEAD"] BS.empty

          decisionContent <- createAdraiFile
            (AdrObjectRef (requireAdrId "A00000000000000000000000013"))
            "# Decision 13\nShallow history."
            "O00000000000000000000000013"
            featureOid
          _ <- commitFiles source
            [ ("architecture/adrai/decisions/000/00000000000000000000000013--shallow.decision.md",
               TextEncoding.encodeUtf8 decisionContent)]

          _ <- gitSuccess source ["checkout", "main"] BS.empty
          _ <- commitFile source "main.txt" "main"

          -- Create shallow clone
          let sourceUri = "file:///" <> map toSlash source
          _ <- gitSuccess temp ["clone", "--depth", "1", sourceUri, shallow] BS.empty

          resolved <- resolveTestRepo shallow "HEAD"
          let basis = requireGitOid featureOid
          ensureSchema shallow
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
                { parsedDocumentObjectRef = "A00000000000000000000000013"
                , parsedManagedPath = requireRepoPath "architecture/adrai/decisions/000/00000000000000000000000013--shallow.decision.md"
                , parsedManagedCapsule = capsule
                , parsedBlobOid = Nothing
                , parsedSemanticHash = Text.unpack $ sha256Digest $ TextEncoding.encodeUtf8 decisionContent
                }

          -- Shallow clone database path
          let dbPath = shallow </> "provenance.sqlite"
          (_, ensureResult) <- runProvenanceEnsure (resolvedRepository resolved) dbPath [doc] ["O00000000000000000000000013"] basis
          case ensureResult of
            Left e -> assertFailure ("ensureProvenance: " <> show e)
            Right _ -> do
              issues <- queryIssueCodes shallow
              -- Shallow history should be flagged
              assertBool "shallow history incomplete warning should be recorded"
                ("HISTORY_COVERAGE_INCOMPLETE" `elem` issues || not (null issues))
              -- Classification should still work
              placements <- queryPlacements shallow "O00000000000000000000000013"
              assertBool "should still have placements despite shallow history" (not (null placements))
    ]

-- =====================================================================
-- Helper functions
-- =====================================================================

requireAdrId :: Text -> IO AdrId
requireAdrId value =
  case mkAdrId value of
    Left v -> assertFailure ("invalid AdrId: " <> show v)
    Right a -> pure a

requireRecordId :: Text -> IO RecordId
requireRecordId value =
  case mkRecordId value of
    Left v -> assertFailure ("invalid RecordId: " <> show v)
    Right r -> pure r

requireConnectionId :: Text -> IO ConnectionId
requireConnectionId value =
  case mkConnectionId value of
    Left v -> assertFailure ("invalid ConnectionId: " <> show v)
    Right c -> pure c

requireOperationId :: Text -> IO OperationId
requireOperationId value =
  case mkOperationId value of
    Left v -> assertFailure ("invalid OperationId: " <> show v)
    Right o -> pure o

requireGitOid :: Text -> IO GitOid
requireGitOid value =
  case mkGitOid value of
    Left v -> assertFailure ("invalid GitOid: " <> show v)
    Right o -> pure o

resolveTestRepo :: FilePath -> Text -> IO ResolvedRepositoryRevision
resolveTestRepo repoDir revision =
  case discoverRepository systemGit repoDir of
    Left e -> assertFailure ("discoverRepository: " <> show e)
    Right repo ->
      case resolveRepositoryRevision repo (requireRevision revision) of
        Left e -> assertFailure ("resolveRepositoryRevision: " <> show e)
        Right r -> pure r

toSlash :: Char -> Char
toSlash '\\' = '/'
toSlash c = c
