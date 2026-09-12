{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Adrai.IntegrityAdversarialTest (tests) where

import Adrai.Compiler
import Adrai.Fixture.CompilerRepository
import Adrai.Format.Document
import Adrai.Git
import Adrai.GitTestSupport
import Adrai.Integrity
  ( IntegrityIssue (integrityCode),
    IntegrityIssueCode (AppendOnlyRewrite),
    parseSnapshotEntry,
    snapshotEntryDocument,
    validateAppendOnlyDelta,
  )
import Adrai.Provenance
import Adrai.Repository
import Adrai.RetainedNative.ResidualRepositorySeed
  ( RepositorySeed,
    createRepositorySeed,
    createRepositorySeedWith,
    removeRepositorySeed,
    withRepositorySeedCopy,
  )
import Adrai.Types (mkOperationId, mkRepoPath, operationIdText)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple (Connection, Only (..), close, open, query_)
import System.Directory (removeFile)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  withResource createEmptyRepositorySeed removeRepositorySeed $ \getEmptyRepositorySeed ->
    withResource createExpandedRepositorySeed (removeRepositorySeed . fst) $ \getExpandedRepositorySeed ->
      testGroup
        "Compiler integrity adversarial"
        [ testCase "malformed managed bytes persist INVALID_MANAGED_DOCUMENT and gate semantics" $
        withCompilerFiles getEmptyRepositorySeed malformedFiles $ \_ resolved ->
          assertInvalidWith resolved "INVALID_MANAGED_DOCUMENT",
      testCase "selected Git nonblob persists MANAGED_NONBLOB" $
        withEmptyRepository getEmptyRepositorySeed "adrai compiler nonblob" $ \repository -> do
          let path = "architecture/adrai/connections/C000/orphan.connection.md"
          commitOid <- commitFile repository "seed.txt" "seed"
          _ <- gitSuccess repository ["update-index", "--add", "--cacheinfo", "160000," <> Text.unpack commitOid <> "," <> path] BS.empty
          _ <- gitSuccess repository ["commit", "-m", "managed nonblob"] BS.empty
          resolved <- resolveHead repository
          assertInvalidWith resolved "MANAGED_NONBLOB",
      testCase "valid then malformed then delete does not retain historical identity" $
        withExpandedRepository getExpandedRepositorySeed $ \repository _files (path, bytes) -> do
          identity <- requireFixture (documentIdentityText path bytes)
          rewritten <- requireFixture (resealChangedRationale path bytes)
          assertBool "freshly resealed semantic rewrite changes committed bytes" (rewritten /= bytes)
          rewrittenIdentity <- requireFixture (documentIdentityText path rewritten)
          rewrittenIdentity @?= identity
          repoPath <- requireFixture (firstShow "path" (mkRepoPath (Text.pack path)))
          let originalEntry = parseSnapshotEntry repoPath bytes
              rewrittenEntry = parseSnapshotEntry repoPath rewritten
          case snapshotEntryDocument rewrittenEntry of
            Left problem -> assertFailure ("freshly resealed semantic rewrite did not parse: " <> show problem)
            Right _ -> pure ()
          map integrityCode (validateAppendOnlyDelta [originalEntry] [rewrittenEntry]) @?= [AppendOnlyRewrite]
          BS.writeFile (repository </> path) "malformed managed document"
          _ <- commitFiles repository []
          removeFile (repository </> path)
          _ <- commitFiles repository []
          resolved <- resolveHead repository
          connection <- compileIntoMemory resolved
          assertIssue connection "APPEND_ONLY_REWRITE"
          assertAttributedIssue connection "APPEND_ONLY_REWRITE" identity
          assertUnattributedIssue connection "MISSING_HISTORICAL_OBJECT"
          assertNoIssue connection "INCOMPLETE_OPERATION"
          assertNoSemantics connection
          close connection,
      testCase "valid then malformed then different valid does not retain historical identity" $
         withExpandedRepository getExpandedRepositorySeed $ \repository _files (path, bytes) -> do
          identity <- requireFixture (documentIdentityText path bytes)
          -- Do not copy a member from an operation that remains live: that is
          -- covered below and must fail closed as INCOMPLETE_OPERATION.  This
          -- case isolates the historical-identity rule by resealing the same
          -- valid object under a new, otherwise non-live operation.
          differentBytes <- requireFixture (resealWithDistinctOperation path bytes)
          differentIdentity <- requireFixture (documentIdentityText path differentBytes)
          assertBool "replacement must not retain the historical identity" (differentIdentity /= identity)
          BS.writeFile (repository </> path) "malformed managed document"
          _ <- commitFiles repository []
          BS.writeFile (repository </> path) differentBytes
          _ <- commitFiles repository []
          resolved <- resolveHead repository
          connection <- compileIntoMemory resolved
          assertIssue connection "APPEND_ONLY_REWRITE"
          assertAttributedIssue connection "APPEND_ONLY_REWRITE" identity
          assertUnattributedIssue connection "APPEND_ONLY_REWRITE"
          assertNoIssue connection "INCOMPLETE_OPERATION"
          assertNoSemantics connection
          close connection,
       testCase "malformed then valid member reused from a live operation persists INCOMPLETE_OPERATION" $
         withExpandedRepository getExpandedRepositorySeed $ \repository files (path, _bytes) -> do
           (reusedPath, reusedBytes) <-
             case [(candidatePath, candidate) | (candidatePath, candidate) <- files, candidatePath /= path] of
               candidate : _ -> pure candidate
               [] -> assertFailure "expanded fixture has no live member to reuse" >> fail "unreachable"
           (_, reusedOperation) <- requireFixture (documentIdentityText reusedPath reusedBytes)
           BS.writeFile (repository </> path) "malformed managed document"
           _ <- commitFiles repository []
           BS.writeFile (repository </> path) reusedBytes
           _ <- commitFiles repository []
           resolved <- resolveHead repository
           connection <- compileIntoMemory resolved
           assertIssue connection "INCOMPLETE_OPERATION"
           assertIssueOperation connection "INCOMPLETE_OPERATION" reusedOperation
           assertNoSemantics connection
           close connection,
       testCase "delete and exact restore persists APPEND_ONLY_DELETE" $
        withExpandedRepository getExpandedRepositorySeed $ \repository _files (path, bytes) -> do
          removeFile (repository </> path)
          _ <- commitFiles repository []
          _ <- commitFile repository path bytes
          resolveHead repository >>= (\resolved -> assertInvalidWith resolved "APPEND_ONLY_DELETE"),
      testCase "selected merge deltas exactly match full traversal when both parents change managed paths" $
        withEmptyRepository getEmptyRepositorySeed "adrai compiler selected merge parity" $ \repository -> do
          let repoPath value = case mkRepoPath value of
                Right path -> path
                Left problem -> error (show problem)
              roots = [repoPath ".adrai.toml", repoPath "architecture/adrai/decisions", repoPath "architecture/adrai/connections"]
          basisText <- commitFile repository "seed.txt" "basis"
          basis <- requireOid basisText
          healthy <- requireFixture (healthyCompilerFiles basis)
          _ <- commitFiles repository healthy
          _ <- gitSuccess repository ["checkout", "-b", "feature"] BS.empty
          (featurePath, _) <-
            case healthy of
              entry : _ -> pure entry
              [] -> assertFailure "healthy fixture must contain a removable file" >> fail "unreachable"
          removeFile (repository </> featurePath)
          _ <- commitFiles repository []
          _ <- gitSuccess repository ["checkout", "main"] BS.empty
          expanded <- requireFixture (scopeExpandedCompilerFiles basis)
          let member@(_memberPath, _) = requireExpandedMember healthy expanded
          _ <- commitFiles repository [member]
          _ <- gitSuccess repository ["merge", "--no-commit", "--no-ff", "feature"] BS.empty
          _ <- commitFiles repository []
          resolved <- resolveHead repository
          graphResult <- reachableCommitGraphAt (resolvedRepository resolved) (resolvedCommitOid resolved)
          graph <- case graphResult of
            Left problem -> assertFailure (show problem) >> fail "unreachable"
            Right value -> pure value
          selectedRoot <-
            case roots of
              root : _ -> pure root
              [] -> assertFailure "history roots fixture must be non-empty" >> fail "unreachable"
          selectedResult <- historyTreeDeltasAt (resolvedRepository resolved) True graph selectedRoot roots
          fullResult <- historyTreeDeltasAt (resolvedRepository resolved) False graph selectedRoot roots
          selected <- case selectedResult of
            Left problem -> assertFailure (show problem) >> fail "unreachable"
            Right value -> pure value
          full <- case fullResult of
            Left problem -> assertFailure (show problem) >> fail "unreachable"
            Right value -> pure value
          selected @?= full
          mergeNode <-
            case reverse graph of
              node : _ -> pure node
              [] -> assertFailure "merge history graph must be non-empty" >> fail "unreachable"
          let
              mergeEdges = [delta | delta <- selected, gitHistoryTreeDeltaCommit delta == gitCommitNodeOid mergeNode]
          map gitHistoryTreeDeltaParent mergeEdges @?= map Just (gitCommitNodeParents mergeNode)
          assertBool "both original merge edges carry managed deltas" (length mergeEdges == 2 && all (not . null . gitHistoryTreeDeltaChanges) mergeEdges)
          assertInvalidWith resolved "INCOMPLETE_OPERATION"
    ]

malformedFiles :: GitOid -> Either Text [(FilePath, ByteString)]
malformedFiles _ = Right [("architecture/adrai/decisions/broken.decision.md", "broken")]

resealChangedRationale :: FilePath -> ByteString -> Either Text ByteString
resealChangedRationale path bytes = do
  repoPath <- firstShow "path" (mkRepoPath (Text.pack path))
  document <- firstShow "parse" (parseManagedDocument repoPath bytes)
  changedRecord <-
    case parsedManagedRecord document of
      ManagedDecision _ -> Left "expanded fixture member was not a connection"
      ManagedConnection connection ->
        Right (ManagedConnection connection {connectionRationale = "Internally resealed semantic rewrite.\n"})
  semantic <- firstShow "semantic" (renderManagedSemantic changedRecord)
  let capsule = parsedManagedCapsule document
  changedCapsule <-
    firstShow "capsule" . mkProvenanceCapsule $
      ProvenanceCapsuleInput
        { capsuleInputOperationId = provenanceOperationId capsule,
          capsuleInputObjectId = provenanceObjectId capsule,
          capsuleInputEventKind = provenanceEventKind capsule,
          capsuleInputActor = provenanceActor capsule,
          capsuleInputTimestampMs = provenanceTimestampMs capsule,
          capsuleInputBasis = provenanceBasis capsule,
          capsuleInputParents = provenanceParents capsule,
          capsuleInputBranchHint = provenanceBranchHint capsule,
          capsuleInputUpstreamHint = provenanceUpstreamHint capsule,
          capsuleInputLineAnchors = provenanceLineAnchors capsule,
          capsuleInputSemanticDigest = semanticDigest semantic,
          capsuleInputToolVersion = provenanceToolVersion capsule,
          capsuleInputDigests = provenanceInputs capsule
        }
  firstShow "seal" (sealManagedDocument changedRecord changedCapsule)

-- | Build a parse-valid, standalone axis operation.  The object remains the
-- same so the test isolates provenance-operation identity rather than a
-- semantic-record replacement.
resealWithDistinctOperation :: FilePath -> ByteString -> Either Text ByteString
resealWithDistinctOperation path bytes = do
  repoPath <- firstShow "path" (mkRepoPath (Text.pack path))
  document <- firstShow "parse" (parseManagedDocument repoPath bytes)
  operation <- firstShow "operation" (mkOperationId ("O" <> Text.replicate 25 "0" <> "2"))
  let capsule = parsedManagedCapsule document
  changedCapsule <-
    firstShow "capsule" . mkProvenanceCapsule $
      ProvenanceCapsuleInput
        { capsuleInputOperationId = operation,
          capsuleInputObjectId = provenanceObjectId capsule,
          capsuleInputEventKind = provenanceEventKind capsule,
          capsuleInputActor = provenanceActor capsule,
          capsuleInputTimestampMs = provenanceTimestampMs capsule,
          capsuleInputBasis = provenanceBasis capsule,
          capsuleInputParents = provenanceParents capsule,
          capsuleInputBranchHint = provenanceBranchHint capsule,
          capsuleInputUpstreamHint = provenanceUpstreamHint capsule,
          capsuleInputLineAnchors = provenanceLineAnchors capsule,
          capsuleInputSemanticDigest = provenanceSemanticDigest capsule,
          capsuleInputToolVersion = provenanceToolVersion capsule,
          capsuleInputDigests = provenanceInputs capsule
        }
  firstShow "seal" (sealManagedDocument (parsedManagedRecord document) changedCapsule)

firstShow :: (Show problem) => Text -> Either problem value -> Either Text value
firstShow label = either (Left . ((label <> ": ") <>) . Text.pack . show) Right

data ExpandedRepositoryFacts = ExpandedRepositoryFacts
  { expandedRepositoryFiles :: [(FilePath, ByteString)],
    expandedRepositoryMember :: (FilePath, ByteString)
  }

type ExpandedRepositorySeed = (RepositorySeed, ExpandedRepositoryFacts)

createEmptyRepositorySeed :: IO RepositorySeed
createEmptyRepositorySeed =
  createRepositorySeed "adrai-integrity-empty-seed" initTestRepository

createExpandedRepositorySeed :: IO ExpandedRepositorySeed
createExpandedRepositorySeed =
  createRepositorySeedWith "adrai-integrity-expanded-seed" $ \repository -> do
    initTestRepository repository
    basisText <- commitFile repository "seed.txt" "basis"
    basis <- requireOid basisText
    healthy <- requireFixture (healthyCompilerFiles basis)
    expanded <- requireFixture (scopeExpandedCompilerFiles basis)
    _ <- commitFiles repository expanded
    pure
      ExpandedRepositoryFacts
        { expandedRepositoryFiles = expanded,
          expandedRepositoryMember = requireExpandedMember healthy expanded
        }

withEmptyRepository :: IO RepositorySeed -> String -> (FilePath -> IO value) -> IO value
withEmptyRepository getRepositorySeed label action = do
  seed <- getRepositorySeed
  withRepositorySeedCopy seed label action

withCompilerFiles :: IO RepositorySeed -> (GitOid -> Either Text [(FilePath, ByteString)]) -> (FilePath -> ResolvedRepositoryRevision -> IO value) -> IO value
withCompilerFiles getRepositorySeed fixture action =
  withEmptyRepository getRepositorySeed "adrai compiler adversarial" $ \repository -> do
    basisText <- commitFile repository "seed.txt" "basis"
    basis <- requireOid basisText
    files <- requireFixture (fixture basis)
    _ <- commitFiles repository files
    resolved <- resolveHead repository
    action repository resolved

withExpandedRepository :: IO ExpandedRepositorySeed -> (FilePath -> [(FilePath, ByteString)] -> (FilePath, ByteString) -> IO value) -> IO value
withExpandedRepository getExpandedRepositorySeed action = do
  (seed, facts) <- getExpandedRepositorySeed
  withRepositorySeedCopy seed "adrai compiler history" $ \repository ->
    action repository (expandedRepositoryFiles facts) (expandedRepositoryMember facts)

requireExpandedMember :: [(FilePath, ByteString)] -> [(FilePath, ByteString)] -> (FilePath, ByteString)
requireExpandedMember healthy expanded =
  case drop (length healthy) expanded of
    [member] -> member
    members -> error ("expected one expanded member, got " <> show (length members))

resolveHead :: FilePath -> IO ResolvedRepositoryRevision
resolveHead repository = do
  discovered <- discoverRepository systemGit repository >>= \case
    Left problem -> assertFailure (show problem)
    Right value -> pure value
  resolveRepositoryRevision discovered (requireRevision "HEAD") >>= \case
    Left problem -> assertFailure (show problem)
    Right value -> pure value

compileIntoMemory :: ResolvedRepositoryRevision -> IO Connection
compileIntoMemory resolved = do
  connection <- open ":memory:"
  coldCompileRepository connection resolved >>= \case
    Left problem -> close connection >> assertFailure (show problem)
    Right _ -> pure connection

assertInvalidWith :: ResolvedRepositoryRevision -> Text -> IO ()
assertInvalidWith resolved code = do
  connection <- compileIntoMemory resolved
  assertIssue connection code
  assertNoSemantics connection
  close connection

assertIssue :: Connection -> Text -> IO ()
assertIssue connection expected = do
  codes <- query_ connection "SELECT code FROM issue ORDER BY ordinal" :: IO [Only Text]
  assertBool ("missing diagnostic " <> Text.unpack expected <> " in " <> show codes) (Only expected `elem` codes)

assertNoIssue :: Connection -> Text -> IO ()
assertNoIssue connection forbidden = do
  codes <- query_ connection "SELECT code FROM issue ORDER BY ordinal" :: IO [Only Text]
  assertBool ("unexpected diagnostic " <> Text.unpack forbidden <> " in " <> show codes) (Only forbidden `notElem` codes)

assertUnattributedIssue :: Connection -> Text -> IO ()
assertUnattributedIssue connection expected = do
  rows <- query_ connection "SELECT code,object_id,operation_id FROM issue ORDER BY ordinal" :: IO [(Text, Maybe Text, Maybe Text)]
  assertBool
    ("missing unattributed " <> Text.unpack expected <> " in " <> show rows)
    (any (\(code, objectId, operationId) -> code == expected && objectId == Nothing && operationId == Nothing) rows)

assertIssueOperation :: Connection -> Text -> Text -> IO ()
assertIssueOperation connection expected expectedOperation = do
  rows <- query_ connection "SELECT code,operation_id FROM issue ORDER BY ordinal" :: IO [(Text, Maybe Text)]
  assertBool
    ("missing " <> Text.unpack expected <> " attributed to " <> Text.unpack expectedOperation <> " in " <> show rows)
    (any (\(code, operationId) -> code == expected && operationId == Just expectedOperation) rows)

assertAttributedIssue :: Connection -> Text -> (Text, Text) -> IO ()
assertAttributedIssue connection expected (objectId, operationId) = do
  rows <- query_ connection "SELECT code,object_id,operation_id FROM issue ORDER BY ordinal" :: IO [(Text, Maybe Text, Maybe Text)]
  assertBool
    ("missing attributed " <> Text.unpack expected <> " in " <> show rows)
    (any (\(code, actualObject, actualOperation) -> code == expected && actualObject == Just objectId && actualOperation == Just operationId) rows)

documentIdentityText :: FilePath -> ByteString -> Either Text (Text, Text)
documentIdentityText path bytes = do
  repoPath <- firstShow "path" (mkRepoPath (Text.pack path))
  document <- firstShow "parse" (parseManagedDocument repoPath bytes)
  pure
    ( provenanceObjectIdText (provenanceObjectId (parsedManagedCapsule document)),
      operationIdText (provenanceOperationId (parsedManagedCapsule document))
    )

assertNoSemantics :: Connection -> IO ()
assertNoSemantics connection = do
  query_ connection "SELECT value FROM meta WHERE key='semantic_state'" >>= (@?= [Only ("invalid" :: Text)])
  query_ connection "SELECT count(*) FROM decision_record" >>= (@?= [Only (0 :: Int64)])
  query_ connection "SELECT count(*) FROM search_document" >>= (@?= [Only (0 :: Int64)])

requireFixture :: Either Text value -> IO value
requireFixture result =
  case result of
    Left problem -> assertFailure (Text.unpack problem)
    Right value -> pure value

requireOid :: Text -> IO GitOid
requireOid value =
  case mkGitOid value of
    Left problem -> assertFailure (show problem)
    Right oid -> pure oid
