{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Adrai.TransactionTest (tests) where

import Adrai.Domain (mkDomain)
import Adrai.Format.Document
  ( DecisionRecord (..),
    ManagedRecord (..),
    canonicalManagedPath,
    renderDecisionSemantic,
    sealManagedDocument,
  )
import Adrai.Git (GitOid (..), discoverRepository, gitOidText, systemGit)
import Adrai.GitTestSupport (commitFile, gitSuccess, initTestRepository, outputText)
import Adrai.Provenance
  ( ProvenanceCapsuleInput (..),
    ProvenanceObjectId (..),
    mkEventKind,
    mkProvenanceCapsule,
    semanticDigest,
  )
import Adrai.Service.Transaction
  ( GeneratedFile (..),
    TransactionConfig (..),
    TransactionResult (..),
    commitAppendOnlyOperation,
    commitTree,
    nullOid,
    parseSingleOidFromOutput,
  )
import Adrai.Types
  ( ActorKind (HumanActor),
    ProvenanceInputs (..),
    configManagedPaths,
    defaultConfig,
    mkActor,
    mkAdrId,
    mkOperationId,
    mkRecordId,
    repoPathText,
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Transaction contract"
    [ testCase "Git plumbing accepts one bare 40- or 64-hex OID with trailing stdout framing" $
        mapM_ assertAccepted acceptedOutputs,
      testCase "Git plumbing rejects non-bare or malformed OID output" $
        mapM_ assertRejected rejectedOutputs,
      testCase "commit-tree creates a one-parent commit from stdin message" $
        withSystemTempDirectory "adrai commit-tree" $ \temporary -> do
          let repositoryPath = temporary </> "repository"
          initTestRepository repositoryPath
          parentText <- commitFile repositoryPath "seed.txt" "seed"
          repository <-
            discoverRepository systemGit repositoryPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
          treeText <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD^{tree}"] ""
          parent <- requireGitOid parentText
          tree <- requireGitOid treeText
          commitTree repository parent tree "Follow up" "op-commit-tree" (Map.fromList [("Reason", "regression")]) >>= \case
            Left problem -> assertFailure (show problem)
            Right commit -> do
              let commitText = Text.unpack (gitOidText commit)
              actualParents <- outputText <$> gitSuccess repositoryPath ["show", "-s", "--format=%P", commitText] ""
              actualMessage <- outputText <$> gitSuccess repositoryPath ["show", "-s", "--format=%B", commitText] ""
              assertEqual "commit-tree supplies exactly the previous HEAD as parent" parentText actualParents
              assertEqual
                "commit-tree reads the complete message from stdin"
                "Follow up\n\nADRAI-Op: op-commit-tree\nADRAI-Reason: regression"
                actualMessage,
      testCase "commit-tree creates a root commit from stdin message" $
        withSystemTempDirectory "adrai commit-tree root" $ \temporary -> do
          let repositoryPath = temporary </> "repository"
          initTestRepository repositoryPath
          repository <-
            discoverRepository systemGit repositoryPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
          treeText <- outputText <$> gitSuccess repositoryPath ["mktree"] ""
          tree <- requireGitOid treeText
          commitTree repository nullOid tree "Bootstrap" "op-bootstrap" (Map.fromList [("Objects", "bootstrap")]) >>= \case
            Left problem -> assertFailure (show problem)
            Right commit -> do
              let commitText = Text.unpack (gitOidText commit)
              actualParents <- outputText <$> gitSuccess repositoryPath ["show", "-s", "--format=%P", commitText] ""
              actualMessage <- outputText <$> gitSuccess repositoryPath ["show", "-s", "--format=%B", commitText] ""
              assertEqual "commit-tree supplies no parent for the null OID" "" actualParents
              assertEqual
                "commit-tree reads the complete root message from stdin"
                "Bootstrap\n\nADRAI-Op: op-bootstrap\nADRAI-Objects: bootstrap"
                actualMessage,
      testCase "append-only transaction preserves unrelated staged entry and binary worktree bytes" $
        withSystemTempDirectory "adrai transaction isolated index" $ \temporary -> do
          let repositoryPath = temporary </> "repository"
              unrelatedPath = repositoryPath </> "unrelated.bin"
              stagedBytes = BS.pack [255, 0, 13, 10, 128, 64, 1, 2, 3]
          initTestRepository repositoryPath
          parentText <- commitFile repositoryPath "seed.txt" "seed\n"
          BS.writeFile unrelatedPath stagedBytes
          _ <- gitSuccess repositoryPath ["add", "--", "unrelated.bin"] BS.empty
          stagedEntryBefore <- gitSuccess repositoryPath ["ls-files", "--stage", "--", "unrelated.bin"] BS.empty
          worktreeBytesBefore <- BS.readFile unrelatedPath
          repository <-
            discoverRepository systemGit repositoryPath >>= \case
              Left problem -> assertFailure (show problem)
              Right discovered -> pure discovered
          parent <- requireGitOid parentText
          (operationText, generated) <- transactionGeneratedFile parent
          let generatedPathText = Text.unpack (repoPathText (genFilePath generated))
              config =
                TransactionConfig
                  { configOperationId = operationText,
                    configSubject = "adrai: transaction index isolation",
                    configTrailers = Map.fromList [("Objects", "transaction-isolation")],
                    configExpectedHead = parent,
                    configGenerated = [generated]
                  }
          commitAppendOnlyOperation repository config >>= \case
            Left problem -> assertFailure (show problem)
            Right result -> do
              assertBool "generated paths were refreshed in the real index" (transactionIndexUpdated result)
              stagedEntryAfter <- gitSuccess repositoryPath ["ls-files", "--stage", "--", "unrelated.bin"] BS.empty
              worktreeBytesAfter <- BS.readFile unrelatedPath
              stagedPaths <- outputText <$> gitSuccess repositoryPath ["diff", "--cached", "--name-only"] BS.empty
              generatedIndexDiff <- gitSuccess repositoryPath ["diff", "--cached", "--", generatedPathText] BS.empty
              generatedWorktreeBytes <- gitSuccess repositoryPath ["show", "HEAD:" <> generatedPathText] BS.empty
              assertEqual "unrelated index OID, mode, and stage are unchanged" stagedEntryBefore stagedEntryAfter
              assertEqual "unrelated binary worktree bytes are unchanged" worktreeBytesBefore worktreeBytesAfter
              assertEqual "only the unrelated staged path remains staged" "unrelated.bin" stagedPaths
              assertEqual "generated path is refreshed to the committed HEAD entry" BS.empty generatedIndexDiff
              assertEqual "generated bytes are committed" (genFileBytes generated) generatedWorktreeBytes
    ]

transactionGeneratedFile :: GitOid -> IO (String, GeneratedFile)
transactionGeneratedFile basis = do
  adr <- requireRight (mkAdrId "A00000000000000000000000042")
  record <- requireRight (mkRecordId "R00000000000000000000000042")
  operation <- requireRight (mkOperationId operationText)
  actor <- requireRight (mkActor HumanActor "transaction-test" Nothing)
  event <- requireRight (mkEventKind "decision.create")
  domain <- requireRight (mkDomain "transaction.index")
  let decision =
        DecisionRecord
          { decisionAdr = adr,
            decisionRecord = record,
            decisionTitle = "Transaction index isolation",
            decisionSummary = "Preserve unrelated staged entries.",
            decisionDomains = [domain],
            decisionBody = "## Decision\nUse an isolated temporary Git index.\n"
          }
      managed = ManagedDecision decision
  semantic <- requireRight (renderDecisionSemantic decision)
  capsule <-
    requireRight . mkProvenanceCapsule $
      ProvenanceCapsuleInput
        { capsuleInputOperationId = operation,
          capsuleInputObjectId = ProvenanceRecord record,
          capsuleInputEventKind = event,
          capsuleInputActor = actor,
          capsuleInputTimestampMs = 1700000000000,
          capsuleInputBasis = basis,
          capsuleInputParents = [],
          capsuleInputBranchHint = Nothing,
          capsuleInputUpstreamHint = Nothing,
          capsuleInputLineAnchors = [],
          capsuleInputSemanticDigest = semanticDigest semantic,
          capsuleInputToolVersion = "adrai/1.0.0",
          capsuleInputDigests = ProvenanceInputs Nothing Nothing Nothing
        }
  bytes <- requireRight (sealManagedDocument managed capsule)
  path <- requireRight (canonicalManagedPath (configManagedPaths defaultConfig) managed)
  pure (Text.unpack operationText, GeneratedFile path bytes)
  where
    operationText = "O00000000000000000000000042"

requireRight :: (Show problem) => Either problem value -> IO value
requireRight result =
  case result of
    Left problem -> assertFailure (show problem) >> fail "unreachable"
    Right value -> pure value

requireGitOid :: Text.Text -> IO GitOid
requireGitOid = pure . GitOid

assertAccepted :: (String, BS8.ByteString, Text.Text) -> IO ()
assertAccepted (label, raw, expected) =
  assertEqual label (Right expected) (gitOidText <$> parseSingleOidFromOutput "write-tree" raw)

assertRejected :: (String, BS8.ByteString) -> IO ()
assertRejected (label, raw) =
  assertBool label (isLeft (parseSingleOidFromOutput "commit-tree" raw))

acceptedOutputs :: [(String, BS8.ByteString, Text.Text)]
acceptedOutputs =
  [ ("40 hex without terminator", oid40, oid40Text),
    ("40 hex with LF", oid40 <> "\n", oid40Text),
    ("40 hex with CRLF", oid40 <> "\r\n", oid40Text),
    ("40 hex with trailing spaces", oid40 <> "   ", oid40Text),
    ("40 hex with newline and trailing spaces", oid40 <> "\n  ", oid40Text),
    ("40 uppercase hex", BS8.map toUpperAscii oid40 <> "\n", oid40Text),
    ("64 hex without terminator", oid64, oid64Text),
    ("64 hex with LF", oid64 <> "\n", oid64Text),
    ("64 hex with CRLF", oid64 <> "\r\n", oid64Text),
    ("64 hex with trailing spaces", oid64 <> "   ", oid64Text),
    ("64 uppercase hex", BS8.map toUpperAscii oid64 <> "\n", oid64Text)
  ]

rejectedOutputs :: [(String, BS8.ByteString)]
rejectedOutputs =
  [ ("empty", ""),
    ("whitespace only", " \r\n "),
    ("wrong length", BS8.take 39 oid40),
    ("non-hex", BS8.take 39 oid40 <> "g"),
    ("leading whitespace", " " <> oid40),
    ("leading tab", "\t" <> oid40),
    ("labeled output", "write-tree: " <> oid40),
    ("two OIDs separated by space", oid40 <> " " <> oid40),
    ("two OIDs separated by newline", oid40 <> "\n" <> oid40),
    ("bare carriage return terminator", oid40 <> "\r"),
    ("multiple newline terminators", oid40 <> "\n\n")
  ]

oid40 :: BS8.ByteString
oid40 = "0123456789abcdef0123456789abcdef01234567"

oid40Text :: Text.Text
oid40Text = "0123456789abcdef0123456789abcdef01234567"

oid64 :: BS8.ByteString
oid64 = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

oid64Text :: Text.Text
oid64Text = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

toUpperAscii :: Char -> Char
toUpperAscii character
  | character >= 'a' && character <= 'f' = toEnum (fromEnum character - 32)
  | otherwise = character
