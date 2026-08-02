{-# LANGUAGE OverloadedStrings #-}

module Adrai.IntegrityTest (tests) where

import Adrai.Format.Document
import Adrai.Integrity
import Adrai.Types
import qualified Data.ByteString as ByteString
import Data.List (sort)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "managed integrity contracts"
    [ testCase "healthy canonical snapshot has no issues" healthySnapshot,
      testCase "renames and duplicate IDs have stable diagnostics" renameAndDuplicate,
      testCase "parse failures become invalid-document diagnostics" invalidDocument,
      testCase "malformed rewrites are present, invalid, and rewritten" malformedRewrite,
      testCase "resealed rewrites remain append-only violations" rewrittenObject,
      testCase "deletes make the historical object and operation incomplete" deletedObject,
      testCase "renames combine noncanonical destination and historical deletion" renamedHistory,
      testCase "whole-history validation catches rewrite then restoration" restoredRewrite,
      testCase "the snapshot entry path is authoritative" authoritativeOuterPath,
      testCase "snapshotEntryFromParsed revalidates retained bytes" revalidatedParsedEntry
    ]

healthySnapshot :: IO ()
healthySnapshot = do
  entry <- frozenEntry
  validateManagedSnapshot managedPaths [entry] @?= []

renameAndDuplicate :: IO ()
renameAndDuplicate = do
  entry <- frozenEntry
  renamedPath <- pathOrFail "architecture/adrai/decisions/R999/copied.decision.md"
  let renamed = relocateSnapshotEntry renamedPath entry
      codes = map integrityCode (validateManagedSnapshot managedPaths [entry, renamed])
  sort codes @?= sort [NonCanonicalPath, DuplicateObjectId]

invalidDocument :: IO ()
invalidDocument = do
  path <- frozenPath
  let issues = validateManagedSnapshot managedPaths [parseSnapshotEntry path "broken"]
  map integrityCode issues @?= [InvalidManagedDocument]
  map integrityPath issues @?= [Just path]

malformedRewrite :: IO ()
malformedRewrite = do
  oldEntry <- frozenEntry
  let current = parseSnapshotEntry (snapshotEntryPath oldEntry) "broken"
      codes = map integrityCode (validateManagedHistory managedPaths [oldEntry] [current])
  sort codes @?= sort [AppendOnlyRewrite, InvalidManagedDocument]

rewrittenObject :: IO ()
rewrittenObject = do
  entry <- frozenEntry
  let rewritten = parseSnapshotEntry (snapshotEntryPath entry) (snapshotEntryBytes entry <> "\n")
  map integrityCode (validateAppendOnlyDelta [entry] [rewritten]) @?= [AppendOnlyRewrite]

deletedObject :: IO ()
deletedObject = do
  entry <- frozenEntry
  map integrityCode (validateAppendOnlyDelta [entry] [])
    @?= [MissingHistoricalObject, IncompleteOperation]

renamedHistory :: IO ()
renamedHistory = do
  entry <- frozenEntry
  renamedPath <- pathOrFail "architecture/adrai/decisions/R000/renamed.decision.md"
  let renamed = relocateSnapshotEntry renamedPath entry
      codes = map integrityCode (validateManagedHistory managedPaths [entry] [renamed])
  sort codes @?= sort [NonCanonicalPath, MissingHistoricalObject]

restoredRewrite :: IO ()
restoredRewrite = do
  entry <- frozenEntry
  let changed = parseSnapshotEntry (snapshotEntryPath entry) (snapshotEntryBytes entry <> "changed")
      codes = map integrityCode (validateAppendOnlyHistory [[entry], [changed], [entry]])
  codes @?= [AppendOnlyRewrite, AppendOnlyRewrite]

authoritativeOuterPath :: IO ()
authoritativeOuterPath = do
  entry <- frozenEntry
  wrong <- pathOrFail "architecture/adrai/decisions/R000/outer-name.decision.md"
  map integrityCode (validateManagedSnapshot managedPaths [relocateSnapshotEntry wrong entry])
    @?= [NonCanonicalPath]

revalidatedParsedEntry :: IO ()
revalidatedParsedEntry = do
  document <- frozenDocument
  let forged = document {parsedManagedBytes = "broken"}
  map integrityCode (validateManagedSnapshot managedPaths [snapshotEntryFromParsed forged])
    @?= [InvalidManagedDocument]

frozenEntry :: IO ManagedSnapshotEntry
frozenEntry = snapshotEntryFromParsed <$> frozenDocument

frozenDocument :: IO ParsedManagedDocument
frozenDocument = do
  bytes <- ByteString.readFile "test/fixtures/contracts/v1/documents/decision-create.sealed.md"
  path <- frozenPath
  case parseManagedDocument path bytes of
    Left problem -> assertFailure (show problem) >> fail "unreachable"
    Right document -> pure document

frozenPath :: IO RepoPath
frozenPath =
  pathOrFail "architecture/adrai/decisions/R000/R00000000000000000000000000--stable-cache-identity.decision.md"

managedPaths :: ManagedPaths
managedPaths = configManagedPaths defaultConfig

pathOrFail :: Text -> IO RepoPath
pathOrFail value =
  case mkRepoPath value of
    Left problem -> assertFailure (show problem) >> fail "unreachable"
    Right path -> pure path
