{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Adrai.IntegrityAdversarialTest (tests) where

import Adrai.Compiler
import Adrai.Fixture.CompilerRepository
import Adrai.Format.Document
import Adrai.Git
import Adrai.GitTestSupport
import Adrai.Provenance
import Adrai.Repository
import Adrai.Types (mkOperationId, mkRepoPath, operationIdText)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple (Connection, Only (..), close, open, query_)
import System.Directory (removeFile, renameFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Compiler integrity adversarial"
    [ testCase "malformed managed bytes persist INVALID_MANAGED_DOCUMENT and gate semantics" $
        withCompilerFiles malformedFiles $ \_ resolved ->
          assertInvalidWith resolved "INVALID_MANAGED_DOCUMENT",
      testCase "wrong path and duplicate identity persist canonical diagnostics" $
        withCompilerFiles wrongAndDuplicateFiles $ \_ resolved -> do
          connection <- compileIntoMemory resolved
          assertIssue connection "NON_CANONICAL_PATH"
          assertIssue connection "DUPLICATE_OBJECT_ID"
          assertNoSemantics connection
          close connection,
      testCase "selected Git nonblob persists MANAGED_NONBLOB" $
        withSystemTempDirectory "adrai compiler nonblob" $ \temporary -> do
          let repository = temporary </> "repository"
              path = "architecture/adrai/connections/C000/orphan.connection.md"
          initTestRepository repository
          commitOid <- commitFile repository "seed.txt" "seed"
          _ <- gitSuccess repository ["update-index", "--add", "--cacheinfo", "160000," <> Text.unpack commitOid <> "," <> path] BS.empty
          _ <- gitSuccess repository ["commit", "-m", "managed nonblob"] BS.empty
          resolved <- resolveHead repository
          assertInvalidWith resolved "MANAGED_NONBLOB",
      testCase "managed-suffix directories are observed as nonblobs" $
        withSystemTempDirectory "adrai compiler managed directories" $ \temporary -> do
          let repository = temporary </> "repository"
          initTestRepository repository
          _ <-
            commitFiles
              repository
              [ ("architecture/adrai/decisions/tree.decision.md/child.txt", "decision directory child"),
                ("architecture/adrai/connections/tree.connection.md/child.txt", "connection directory child")
              ]
          resolved <- resolveHead repository
          connection <- compileIntoMemory resolved
          issues <- query_ connection "SELECT code FROM issue WHERE code='MANAGED_NONBLOB' ORDER BY path" :: IO [Only Text]
          issues @?= [Only "MANAGED_NONBLOB", Only "MANAGED_NONBLOB"]
          assertNoSemantics connection
          close connection,
      testCase "graph zero-head input persists graph diagnostics and no semantics" $
        withCompilerFiles zeroHeadCompilerFiles $ \_ resolved -> do
          connection <- compileIntoMemory resolved
          graphCount <- query_ connection "SELECT count(*) FROM issue WHERE origin='graph'" :: IO [Only Int64]
          assertBool "zero-head graph diagnostic is persisted" (graphCount /= [Only 0])
          assertNoSemantics connection
          close connection,
      testCase "internally resealed same-path rewrite persists APPEND_ONLY_REWRITE" $
        withExpandedRepository $ \repository _files (path, bytes) -> do
          rewritten <- requireFixture (resealChangedRationale path bytes)
          BS.writeFile (repository </> path) rewritten
          _ <- commitFiles repository []
          resolveHead repository >>= (\resolved -> assertInvalidWith resolved "APPEND_ONLY_REWRITE"),
      testCase "valid then malformed then delete does not retain historical identity" $
        withExpandedRepository $ \repository _files (path, bytes) -> do
          identity <- requireFixture (documentIdentityText path bytes)
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
         withExpandedRepository $ \repository _files (path, bytes) -> do
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
         withExpandedRepository $ \repository files (path, _bytes) -> do
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
       testCase "permanent deletion persists MISSING_HISTORICAL_OBJECT" $
        withExpandedRepository $ \repository _files (path, _bytes) -> do
          removeFile (repository </> path)
          _ <- commitFiles repository []
          resolved <- resolveHead repository
          connection <- compileIntoMemory resolved
          assertIssue connection "MISSING_HISTORICAL_OBJECT"
          assertIssue connection "INCOMPLETE_OPERATION"
          assertNoSemantics connection
          close connection,
      testCase "delete and exact restore persists APPEND_ONLY_DELETE" $
        withExpandedRepository $ \repository _files (path, bytes) -> do
          removeFile (repository </> path)
          _ <- commitFiles repository []
          _ <- commitFile repository path bytes
          resolveHead repository >>= (\resolved -> assertInvalidWith resolved "APPEND_ONLY_DELETE"),
      testCase "rename persists noncanonical destination and historical disappearance" $
        withExpandedRepository $ \repository _files (path, _bytes) -> do
          let renamed = "architecture/adrai/connections/renamed.connection.md"
          renameFile (repository </> path) (repository </> renamed)
          _ <- commitFiles repository []
          resolved <- resolveHead repository
          connection <- compileIntoMemory resolved
          assertIssue connection "NON_CANONICAL_PATH"
          assertIssue connection "MISSING_HISTORICAL_OBJECT"
          assertNoSemantics connection
          close connection,
      testCase "each merge parent edge is checked independently" $
        withSystemTempDirectory "adrai compiler merge edge" $ \temporary -> do
          let repository = temporary </> "repository"
          initTestRepository repository
          basisText <- commitFile repository "seed.txt" "basis"
          basis <- requireOid basisText
          healthy <- requireFixture (healthyCompilerFiles basis)
          _ <- commitFiles repository healthy
          _ <- gitSuccess repository ["checkout", "-b", "feature"] BS.empty
          _ <- commitFile repository "feature.txt" "independent branch"
          _ <- gitSuccess repository ["checkout", "main"] BS.empty
          expanded <- requireFixture (scopeExpandedCompilerFiles basis)
          let member@(memberPath, _memberBytes) = requireExpandedMember healthy expanded
          _ <- commitFiles repository [member]
          _ <- gitSuccess repository ["merge", "--no-commit", "--no-ff", "feature"] BS.empty
          removeFile (repository </> memberPath)
          _ <- commitFiles repository []
          resolveHead repository >>= (\resolved -> assertInvalidWith resolved "MISSING_HISTORICAL_OBJECT"),
      testCase "selected merge deltas exactly match full traversal when both parents change managed paths" $
        withSystemTempDirectory "adrai compiler selected merge parity" $ \temporary -> do
          let repository = temporary </> "repository"
              repoPath value = case mkRepoPath value of
                Right path -> path
                Left problem -> error (show problem)
              roots = [repoPath ".adrai.toml", repoPath "architecture/adrai/decisions", repoPath "architecture/adrai/connections"]
          initTestRepository repository
          basisText <- commitFile repository "seed.txt" "basis"
          basis <- requireOid basisText
          healthy <- requireFixture (healthyCompilerFiles basis)
          _ <- commitFiles repository healthy
          _ <- gitSuccess repository ["checkout", "-b", "feature"] BS.empty
          let (featurePath, _) = head healthy
          removeFile (repository </> featurePath)
          _ <- commitFiles repository []
          _ <- gitSuccess repository ["checkout", "main"] BS.empty
          expanded <- requireFixture (scopeExpandedCompilerFiles basis)
          let member@(memberPath, _) = requireExpandedMember healthy expanded
          _ <- commitFiles repository [member]
          _ <- gitSuccess repository ["merge", "--no-commit", "--no-ff", "feature"] BS.empty
          _ <- commitFiles repository []
          resolved <- resolveHead repository
          graphResult <- reachableCommitGraphAt (resolvedRepository resolved) (resolvedCommitOid resolved)
          graph <- case graphResult of
            Left problem -> assertFailure (show problem) >> fail "unreachable"
            Right value -> pure value
          selectedResult <- historyTreeDeltasAt (resolvedRepository resolved) True graph (head roots) roots
          fullResult <- historyTreeDeltasAt (resolvedRepository resolved) False graph (head roots) roots
          selected <- case selectedResult of
            Left problem -> assertFailure (show problem) >> fail "unreachable"
            Right value -> pure value
          full <- case fullResult of
            Left problem -> assertFailure (show problem) >> fail "unreachable"
            Right value -> pure value
          selected @?= full
          let mergeNode = last graph
              mergeEdges = [delta | delta <- selected, gitHistoryTreeDeltaCommit delta == gitCommitNodeOid mergeNode]
          map gitHistoryTreeDeltaParent mergeEdges @?= map Just (gitCommitNodeParents mergeNode)
          assertBool "both original merge edges carry managed deltas" (length mergeEdges == 2 && all (not . null . gitHistoryTreeDeltaChanges) mergeEdges)
          assertInvalidWith resolved "INCOMPLETE_OPERATION"
    ]

malformedFiles :: GitOid -> Either Text [(FilePath, ByteString)]
malformedFiles _ = Right [("architecture/adrai/decisions/broken.decision.md", "broken")]

wrongAndDuplicateFiles :: GitOid -> Either Text [(FilePath, ByteString)]
wrongAndDuplicateFiles basis = do
  files <- healthyCompilerFiles basis
  case break (Text.isSuffixOf ".decision.md" . Text.pack . fst) files of
    (_, []) -> Left "healthy fixture has no decision"
    (before, decision@(_path, bytes) : after) ->
      Right
        ( before
            <> [decision]
            <> after
            <> [("architecture/adrai/decisions/zz-copy.decision.md", bytes)]
        )

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

withCompilerFiles :: (GitOid -> Either Text [(FilePath, ByteString)]) -> (FilePath -> ResolvedRepositoryRevision -> IO value) -> IO value
withCompilerFiles fixture action =
  withSystemTempDirectory "adrai compiler adversarial" $ \temporary -> do
    let repository = temporary </> "repository"
    initTestRepository repository
    basisText <- commitFile repository "seed.txt" "basis"
    basis <- requireOid basisText
    files <- requireFixture (fixture basis)
    _ <- commitFiles repository files
    resolved <- resolveHead repository
    action repository resolved

withExpandedRepository :: (FilePath -> [(FilePath, ByteString)] -> (FilePath, ByteString) -> IO value) -> IO value
withExpandedRepository action =
  withSystemTempDirectory "adrai compiler history" $ \temporary -> do
    let repository = temporary </> "repository"
    initTestRepository repository
    basisText <- commitFile repository "seed.txt" "basis"
    basis <- requireOid basisText
    healthy <- requireFixture (healthyCompilerFiles basis)
    expanded <- requireFixture (scopeExpandedCompilerFiles basis)
    _ <- commitFiles repository expanded
    action repository expanded (requireExpandedMember healthy expanded)

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
