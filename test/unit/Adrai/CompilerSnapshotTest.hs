{-# LANGUAGE OverloadedStrings #-}

module Adrai.CompilerSnapshotTest (tests) where

import Adrai.Compiler.Snapshot
import Adrai.Fixture.CompilerRepository
import Adrai.Format.Document
import Adrai.Git
import Adrai.Integrity
import Adrai.Provenance
import Adrai.Types
import Data.ByteString (ByteString)
import Data.List (find)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Compiler snapshot"
    [ testCase "healthy four-member create satisfies canonical operation invariants" $ do
        basis <- requireRight (mkGitOid (Text.replicate 40 "a"))
        files <- requireRight (healthyCompilerFiles basis)
        documents <-
          traverse
            ( \(pathText, bytes) -> do
                path <- requireRight (mkRepoPath (Text.pack pathText))
                requireRight (parseManagedDocument path bytes)
            )
            files
        validateManagedOperations documents @?= [],
      testCase "operation grouping diagnoses capsule context cross-ADR and unknown shapes" $ do
        documents <- healthyDocuments
        scope <- requireDocument isScope documents
        changedContext <- mutateCapsule (\input -> input {capsuleInputTimestampMs = capsuleInputTimestampMs input + 1}) scope
        assertCode InconsistentOperationCapsule (replaceDocument scope changedContext documents)
        changedDigests <-
          mutateCapsule
            (\input -> input {capsuleInputDigests = ProvenanceInputs (Just (semanticDigest "different input")) Nothing Nothing})
            scope
        assertCode InconsistentOperationCapsule (replaceDocument scope changedDigests documents)
        foreignAdr <- requireRight (mkAdrId (fixtureId 'A' '1'))
        assertCode CrossAdrOperation (replaceDocument scope (setDocumentAdr foreignAdr scope) documents)
        decision <- requireDocument isDecision documents
        assertCode UnknownOperationShape [decision],
      testCase "create operation diagnoses parents amendment edges and initial-axis parent mismatch" $ do
        documents <- healthyDocuments
        decision <- requireDocument isDecision documents
        scope <- requireDocument isScope documents
        parentedDecision <- mutateCapsule (\input -> input {capsuleInputParents = [ProvenanceRecord compilerRecordId]}) decision
        assertCode CreateProvenanceHasParents (replaceDocument decision parentedDecision documents)
        assertCode CreateHasAmendmentEdge (documents <> [asAmendmentEdge scope])
        unparentedScope <- mutateCapsule (\input -> input {capsuleInputParents = []}) scope
        assertCode ProvenanceParentMismatch (replaceDocument scope unparentedScope documents),
      testCase "amendment operation diagnoses operation edge-cardinality and parent mismatches" $ do
        documents <- conflictedDocuments
        validateManagedOperations documents @?= []
        operation <- requireRight (mkOperationId (fixtureId 'O' '1'))
        otherOperation <- requireRight (mkOperationId (fixtureId 'O' '9'))
        let members = filter ((== operation) . provenanceOperationId . parsedManagedCapsule) documents
        decision <- requireDocument isDecision members
        edge <- requireDocument isAmendment members
        changedOperation <- mutateCapsule (\input -> input {capsuleInputOperationId = otherOperation}) edge
        assertCode AmendmentOperationMismatch [decision, changedOperation]
        assertCode AmendmentEdgeCardinality [decision]
        changedParents <- mutateCapsule (\input -> input {capsuleInputParents = []}) edge
        assertCode ProvenanceParentMismatch [decision, changedParents],
      testCase "axis operations diagnose exact scope domain and status events" $ do
        documents <- healthyDocuments
        scope <- requireDocument isScope documents >>= withEvent "scope.wrong"
        domain <- requireDocument isDomain documents >>= withEvent "domain.wrong"
        status <- requireDocument isStatus documents >>= withEvent "status.wrong"
        assertCode ScopeEventKindMismatch [scope]
        assertCode DomainEventKindMismatch [domain]
        assertCode StatusEventKindMismatch [status],
      testCase "reachable commit DAG decoder requires canonical strict framing" $ do
        let child = Text.replicate 40 "a"
            parent = Text.replicate 40 "b"
            valid = TextEncoding.encodeUtf8 (parent <> "\n" <> child <> " " <> parent <> "\n")
        nodes <- requireRight (decodeGitCommitGraph valid)
        map gitCommitNodeOid nodes @?= [requireOid parent, requireOid child]
        map gitCommitNodeParents nodes @?= [[], [requireOid parent]]
        assertLeft (decodeGitCommitGraph "")
        assertLeft (decodeGitCommitGraph (TextEncoding.encodeUtf8 child))
        assertLeft (decodeGitCommitGraph (TextEncoding.encodeUtf8 (child <> "  " <> parent <> "\n")))
        assertLeft (decodeGitCommitGraph (TextEncoding.encodeUtf8 (child <> "\n\n")))
    ]

healthyDocuments :: IO [ParsedManagedDocument]
healthyDocuments = loadDocuments healthyCompilerFiles

conflictedDocuments :: IO [ParsedManagedDocument]
conflictedDocuments = loadDocuments conflictedCompilerFiles

loadDocuments :: (GitOid -> Either Text.Text [(FilePath, ByteString)]) -> IO [ParsedManagedDocument]
loadDocuments fixture = do
  basis <- requireRight (mkGitOid (Text.replicate 40 "a"))
  files <- requireRight (fixture basis)
  traverse
    ( \(pathText, bytes) -> do
        path <- requireRight (mkRepoPath (Text.pack pathText))
        requireRight (parseManagedDocument path bytes)
    )
    files

requireDocument :: (ParsedManagedDocument -> Bool) -> [ParsedManagedDocument] -> IO ParsedManagedDocument
requireDocument predicate documents =
  case find predicate documents of
    Nothing -> assertFailure "required managed document is absent"
    Just document -> pure document

replaceDocument :: ParsedManagedDocument -> ParsedManagedDocument -> [ParsedManagedDocument] -> [ParsedManagedDocument]
replaceDocument original replacement = map (\document -> if parsedManagedPath document == parsedManagedPath original then replacement else document)

mutateCapsule :: (ProvenanceCapsuleInput -> ProvenanceCapsuleInput) -> ParsedManagedDocument -> IO ParsedManagedDocument
mutateCapsule change document = do
  capsule <- requireRight (mkProvenanceCapsule (change (capsuleInput document)))
  pure document {parsedManagedCapsule = capsule}

capsuleInput :: ParsedManagedDocument -> ProvenanceCapsuleInput
capsuleInput document =
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
      capsuleInputSemanticDigest = provenanceSemanticDigest capsule,
      capsuleInputToolVersion = provenanceToolVersion capsule,
      capsuleInputDigests = provenanceInputs capsule
    }
  where
    capsule = parsedManagedCapsule document

withEvent :: Text.Text -> ParsedManagedDocument -> IO ParsedManagedDocument
withEvent value document = do
  event <- requireRight (mkEventKind value)
  mutateCapsule (\input -> input {capsuleInputEventKind = event}) document

setDocumentAdr :: AdrId -> ParsedManagedDocument -> ParsedManagedDocument
setDocumentAdr adr document =
  document
    { parsedManagedRecord =
        case parsedManagedRecord document of
          ManagedDecision decision -> ManagedDecision decision {decisionAdr = adr}
          ManagedConnection connection ->
            ManagedConnection connection {connectionPayload = setPayloadAdr adr (connectionPayload connection)}
    }

setPayloadAdr :: AdrId -> ConnectionPayload -> ConnectionPayload
setPayloadAdr adr payload =
  case payload of
    AmendsConnection value -> AmendsConnection value {amendsSubjectAdr = adr}
    AppliesToConnection value -> AppliesToConnection value {appliesToSubjectAdr = adr}
    DomainsConnection value -> DomainsConnection value {domainsSubjectAdr = adr}
    StatusConnection value -> StatusConnection value {statusSubjectAdr = adr}

asAmendmentEdge :: ParsedManagedDocument -> ParsedManagedDocument
asAmendmentEdge document =
  document
    { parsedManagedRecord =
        case parsedManagedRecord document of
          ManagedConnection connection ->
            ManagedConnection
              connection
                { connectionPayload = AmendsConnection (AmendsPayload compilerAdrId compilerRecordId [compilerRecordId])
                }
          ManagedDecision decision -> ManagedDecision decision
    }

assertCode :: IntegrityIssueCode -> [ParsedManagedDocument] -> IO ()
assertCode code documents =
  assertBool
    ("missing operation diagnostic " <> Text.unpack (integrityIssueCodeText code))
    (CompilerIntegrityCode code `elem` map compilerDiagnosticCode (validateManagedOperations documents))

isDecision :: ParsedManagedDocument -> Bool
isDecision document = case parsedManagedRecord document of ManagedDecision _ -> True; _ -> False

isScope :: ParsedManagedDocument -> Bool
isScope document =
  case parsedManagedRecord document of
    ManagedDecision _ -> False
    ManagedConnection connection ->
      case connectionPayload connection of
        AppliesToConnection _ -> True
        _ -> False

isDomain :: ParsedManagedDocument -> Bool
isDomain document =
  case parsedManagedRecord document of
    ManagedDecision _ -> False
    ManagedConnection connection ->
      case connectionPayload connection of
        DomainsConnection _ -> True
        _ -> False

isStatus :: ParsedManagedDocument -> Bool
isStatus document =
  case parsedManagedRecord document of
    ManagedDecision _ -> False
    ManagedConnection connection ->
      case connectionPayload connection of
        StatusConnection _ -> True
        _ -> False

isAmendment :: ParsedManagedDocument -> Bool
isAmendment document =
  case parsedManagedRecord document of
    ManagedDecision _ -> False
    ManagedConnection connection ->
      case connectionPayload connection of
        AmendsConnection _ -> True
        _ -> False

fixtureId :: Char -> Char -> Text.Text
fixtureId prefix suffix = Text.singleton prefix <> Text.replicate 25 "0" <> Text.singleton suffix

requireOid :: Text.Text -> GitOid
requireOid value =
  case mkGitOid value of
    Left problem -> error (show problem)
    Right oid -> oid

requireRight :: (Show problem) => Either problem value -> IO value
requireRight result =
  case result of
    Left problem -> assertFailure (show problem)
    Right value -> pure value

assertLeft :: (Show value) => Either problem value -> IO ()
assertLeft result =
  case result of
    Left _ -> pure ()
    Right value -> assertFailure ("expected failure, got " <> show value)
