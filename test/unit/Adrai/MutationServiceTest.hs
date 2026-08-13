{-# LANGUAGE OverloadedStrings #-}

module Adrai.MutationServiceTest (tests) where

import Adrai.Domain (mkDomain)
import Adrai.Format.Document
  ( AppliesToPayload (..),
    ConnectionPayload (..),
    ConnectionRecord (..),
    DecisionRecord (..),
    DomainsPayload (..),
    ManagedRecord (..),
    ParsedManagedDocument (..),
    StatusPayload (..),
    StatusState (StatusActive),
    canonicalManagedPath,
    parseManagedDocument,
  )
import Adrai.Git
  ( GitHeadState (..),
    Repository,
    discoverRepository,
    gitOidText,
    repositoryHeadState,
    systemGit,
  )
import Adrai.GitTestSupport
  ( commitFile,
    gitSuccess,
    initTestRepository,
    outputText,
  )
import Adrai.Provenance
  ( ProvenanceObjectId (..),
    eventKindText,
    provenanceActor,
    provenanceBasis,
    provenanceBranchHint,
    provenanceEventKind,
    provenanceObjectId,
    provenanceOperationId,
    provenanceParents,
    provenanceTimestampMs,
    provenanceToolVersion,
  )
import Adrai.Scope (mkScopePattern)
import Adrai.Service.Mutation
  ( CreateResult (..),
    createAdrCommand,
  )
import Adrai.Service.Transaction (TransactionError)
import Adrai.Types
  ( ActorKind (HumanActor),
    Actor,
    RepoPath,
    configManagedPaths,
    defaultConfig,
    mkActor,
    mkAdrId,
    mkRecordId,
    operationIdText,
    repoPathText,
  )
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import Data.Either (isLeft)
import Data.List (sort)
import qualified Data.Set as Set
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "P6-02A.0a create service transaction"
    [ testCase "create commits four sealed records and returns transaction result" createCommitsExactlyFourRecords
    , testCase "create failure leaves HEAD and unrelated staged index entry unchanged" createFailurePreservesRepositoryState
    ]

createCommitsExactlyFourRecords :: IO ()
createCommitsExactlyFourRecords =
  withCreateRepository $ \directory repository oldHead -> do
    let stagedPath = directory </> "unrelated-staged.txt"
        stagedBytes = "preserve these staged bytes\NULexactly"
    BS.writeFile stagedPath stagedBytes
    _ <- gitSuccess directory ["add", "unrelated-staged.txt"] BS.empty
    indexBefore <- gitSuccess directory ["ls-files", "-s", "--", "unrelated-staged.txt"] BS.empty

    beforeMs <- posixTimeMs
    result <- runCreate repository
    afterMs <- posixTimeMs
    created <- assertRight result
    headAfter <- gitText directory ["rev-parse", "HEAD"]
    headAfter @?= gitOidText (createCommitOid created)
    assertBool
      "the returned operation ID has the canonical operation shape"
      (Text.length (Text.pack (createOperationId created)) == 27 && "O" `Text.isPrefixOf` Text.pack (createOperationId created))

    -- The returned commit is the new HEAD, has precisely the previous HEAD as
    -- its only parent, and changes exactly the four canonical managed paths.
    parents <- gitText directory ["show", "-s", "--format=%P", Text.unpack headAfter]
    parents @?= oldHead
    changed <- fmap (sort . Text.lines) (gitText directory ["diff-tree", "--no-commit-id", "--name-only", "-r", Text.unpack headAfter])
    let returnedPaths = sort (map repoPathText (createCreatedPaths created))
    changed @?= returnedPaths
    length returnedPaths @?= 4
    assertBool "the transaction refreshed only generated paths" (createIndexUpdated created)

    -- The unrelated entry stays byte-identical in the caller's real index.
    indexAfter <- gitSuccess directory ["ls-files", "-s", "--", "unrelated-staged.txt"] BS.empty
    indexAfter @?= indexBefore
    BS.readFile stagedPath >>= (@?= stagedBytes)

    documents <- mapM (assertCanonicalCreatedDocument directory created oldHead) (createCreatedPaths created)
    assertCreatedDocuments created beforeMs afterMs documents

createFailurePreservesRepositoryState :: IO ()
createFailurePreservesRepositoryState =
  withCreateRepository $ \directory repository oldHead -> do
    _ <- gitSuccess directory ["checkout", "--detach"] BS.empty
    stateBefore <- repositoryHeadState repository
    case stateBefore of
      Right GitHeadDetached -> pure ()
      other -> assertFailure ("expected detached HEAD fixture, got " <> show other)
    result <- runCreate repository
    assertBool "detached-HEAD transaction must fail" (isLeft result)
    gitText directory ["rev-parse", "HEAD"] >>= (@?= oldHead)

assertCanonicalCreatedDocument :: FilePath -> CreateResult -> Text.Text -> RepoPath -> IO ParsedManagedDocument
assertCanonicalCreatedDocument directory created oldHead path = do
  bytes <- gitSuccess directory ["show", Text.unpack (gitOidText (createCommitOid created) <> ":" <> repoPathText path)] BS.empty
  parsed <- assertRight (parseManagedDocument path bytes)
  let capsule = parsedManagedCapsule parsed
  expectedActor <- createActor
  operationIdText (provenanceOperationId capsule) @?= Text.pack (createOperationId created)
  gitOidText (provenanceBasis capsule) @?= oldHead
  provenanceActor capsule @?= expectedActor
  provenanceBranchHint capsule @?= Just "main"
  provenanceToolVersion capsule @?= "adrai/1.0.0"
  assertBool "create provenance uses a positive millisecond timestamp" (provenanceTimestampMs capsule > 0)
  canonicalManagedPath (configManagedPaths defaultConfig) (parsedManagedRecord parsed) @?= Right path
  pure parsed

assertCreatedDocuments :: CreateResult -> Integer -> Integer -> [ParsedManagedDocument] -> IO ()
assertCreatedDocuments created beforeMs afterMs documents = do
  domain <- assertRight (mkDomain "compiler")
  scope <- assertRight (mkScopePattern "src/**")
  let objects = map (provenanceObjectId . parsedManagedCapsule) documents
      timestamps = map (provenanceTimestampMs . parsedManagedCapsule) documents
      connectionIds = [createScopeId created, createDomainId created, createStatusId created]
      expectedObjects =
        Set.fromList
          [ ProvenanceRecord (createRecordId created),
            ProvenanceConnection (createScopeId created),
            ProvenanceConnection (createDomainId created),
            ProvenanceConnection (createStatusId created)
          ]
  Set.fromList objects @?= expectedObjects
  Set.size (Set.fromList connectionIds) @?= 3
  assertBool "all create members share one millisecond timestamp" (not (null timestamps) && all (== head timestamps) timestamps)
  assertBool "every create timestamp falls within the operation clock bounds" (all (\timestamp -> timestamp >= beforeMs && timestamp <= afterMs) timestamps)
  case [(decision, parsedManagedCapsule document) | document <- documents, ManagedDecision decision <- [parsedManagedRecord document]] of
    [(decision, capsule)] -> do
      decisionAdr decision @?= createAdrId created
      decisionRecord decision @?= createRecordId created
      decisionTitle decision @?= "Create transaction"
      decisionSummary decision @?= "Create commits the complete canonical document set."
      decisionBody decision @?= "## Decision\nUse one append-only transaction.\n"
      decisionDomains decision @?= [domain]
      eventKindText (provenanceEventKind capsule) @?= "decision.create"
      provenanceParents capsule @?= []
    other -> assertFailure ("expected exactly one decision, got " <> show other)
  case [(connection, payload, parsedManagedCapsule document) | document <- documents, ManagedConnection connection <- [parsedManagedRecord document], AppliesToConnection payload <- [connectionPayload connection]] of
    [(connection, payload, capsule)] -> do
      let identifier = connectionRecordId connection
      identifier @?= createScopeId created
      appliesToSubjectAdr payload @?= createAdrId created
      appliesToParentConnections payload @?= []
      appliesToChange payload @?= "initial"
      appliesToAdded payload @?= [scope]
      appliesToRemoved payload @?= []
      appliesToEffective payload @?= [scope]
      connectionRationale connection @?= "Initial scope.\n"
      eventKindText (provenanceEventKind capsule) @?= "scope.initial"
      provenanceParents capsule @?= [ProvenanceRecord (createRecordId created)]
    other -> assertFailure ("expected exactly one scope payload, got " <> show other)
  case [(connection, payload, parsedManagedCapsule document) | document <- documents, ManagedConnection connection <- [parsedManagedRecord document], DomainsConnection payload <- [connectionPayload connection]] of
    [(connection, payload, capsule)] -> do
      let identifier = connectionRecordId connection
      identifier @?= createDomainId created
      domainsSubjectAdr payload @?= createAdrId created
      domainsParentConnections payload @?= []
      domainsChange payload @?= "initial"
      domainsAdded payload @?= [domain]
      domainsRemoved payload @?= []
      domainsEffective payload @?= [domain]
      domainsRefinements payload @?= []
      connectionRationale connection @?= "Initial domain.\n"
      eventKindText (provenanceEventKind capsule) @?= "domain.initial"
      provenanceParents capsule @?= [ProvenanceRecord (createRecordId created)]
    other -> assertFailure ("expected exactly one domain payload, got " <> show other)
  case [(connection, payload, parsedManagedCapsule document) | document <- documents, ManagedConnection connection <- [parsedManagedRecord document], StatusConnection payload <- [connectionPayload connection]] of
    [(connection, payload, capsule)] -> do
      let identifier = connectionRecordId connection
      identifier @?= createStatusId created
      statusSubjectAdr payload @?= createAdrId created
      statusParentConnections payload @?= []
      statusState payload @?= StatusActive
      statusRecordHeads payload @?= [createRecordId created]
      statusReplacementAdr payload @?= Nothing
      connectionRationale connection @?= "Initial active status.\n"
      eventKindText (provenanceEventKind capsule) @?= "status.initial"
      provenanceParents capsule @?= [ProvenanceRecord (createRecordId created)]
    other -> assertFailure ("expected exactly one status payload, got " <> show other)

withCreateRepository :: (FilePath -> Repository -> Text.Text -> IO value) -> IO value
withCreateRepository action =
  withSystemTempDirectory "adrai mutation service" $ \temporary -> do
    let directory = temporary </> "repository"
    initTestRepository directory
    oldHead <- commitFile directory "seed.txt" "seed\n"
    repository <- assertRight =<< discoverRepository systemGit directory
    action directory repository oldHead

runCreate :: Repository -> IO (Either TransactionError CreateResult)
runCreate repository = do
  actor <- createActor
  adr <- assertRight (mkAdrId "A00000000000000000000000001")
  record <- assertRight (mkRecordId "R00000000000000000000000001")
  domain <- assertRight (mkDomain "compiler")
  scope <- assertRight (mkScopePattern "src/**")
  createAdrCommand
    repository
    (configManagedPaths defaultConfig)
    actor
    adr
    record
    "Create transaction"
    "Create commits the complete canonical document set."
    "## Decision\nUse one append-only transaction.\n"
    [domain]
    [scope]
    Nothing
    Nothing
    Nothing

createActor :: IO Actor
createActor = assertRight (mkActor HumanActor "mutation-service-test" Nothing)

posixTimeMs :: IO Integer
posixTimeMs = floor . (* 1000) <$> getPOSIXTime

gitText :: FilePath -> [String] -> IO Text.Text
gitText directory arguments = outputText <$> gitSuccess directory arguments BS.empty

assertRight :: Show problem => Either problem value -> IO value
assertRight value =
  case value of
    Left problem -> assertFailure (show problem) >> fail "unreachable"
    Right result -> pure result
