{-# LANGUAGE OverloadedStrings #-}

-- | Guard tests that pin the exact frame sequence each cold-compile
-- fingerprint function feeds into SHA-256 against a known golden digest.
--
-- The persisted fingerprints stored in meta derive from those frame
-- sequences, so any edit to frame construction, framing, list order, or
-- sort key changes the digests below and fails these tests instead of
-- silently mutating persisted fingerprints.  The golden values are derived
-- from the current implementation's output; keep them byte-identical.

module Adrai.FingerprintGuardTest (tests) where

import Adrai.Compiler
  ( coldMaterializationFingerprint,
    coldMaterializationFingerprintFrames
  )
import Adrai.Compiler.Snapshot
  ( AnalyzedRepositorySnapshot (..),
    CompilerDiagnostic (..),
    CompilerDiagnosticCode (..),
    CompilerDiagnosticOrigin (..),
    CompilerDiagnosticSeverity (..),
    sourceFingerprint
  )
import Adrai.Fixture.CompilerRepository
  ( compilerAdrId,
    compilerRecordId,
    healthyCompilerFiles
  )
import Adrai.Format (renderDigest)
import Adrai.Format.Document
  ( ManagedRecord (..),
    ParsedManagedDocument (..),
    parseManagedDocument,
    parsedManagedRecord
  )
import Adrai.Git
  ( GitBlob (..),
    GitFileMode (GitRegularFile),
    GitObjectType (GitBlobObject),
    GitTreeEntry (..),
    Repository (..),
    RepositoryLayout (BareRepository),
    RevisionSpec (..),
    systemGit
  )
import Adrai.Graph
  ( AdrConflict (..),
    ConflictCandidate (..),
    GraphAxis (DecisionAxis),
    reduceManagedGraph
  )
import Adrai.Integrity (IntegrityIssueCode (InconsistentOperationCapsule))
import Adrai.Provenance
  ( GitOid,
    mkGitOid,
    sha256Digest,
    sha256DigestFrames
  )
import Adrai.Repository
  ( RawRepositoryConfigObservation (..),
    RawRepositorySnapshotObservation (..),
    RepositoryConfigOrigin (CommittedConfigOrigin),
    RepositoryRevisionKey (..),
    RepositoryTreeObservation (..),
    ResolvedRepositoryRevision (..)
  )
import Adrai.Types
  ( ManagedPaths,
    OperationId,
    RepoPath (..),
    StateToken (..),
    defaultConfig,
    mkManagedPaths,
    mkOperationId,
    mkRepoPath,
    recordObjectRef
  )
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as TextEncoding
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "fingerprint-frame-guard"
    [ testCase "cold materialization fingerprint pins the exact frame sequence" $ do
        let analyzed = guardAnalyzed guardDocuments
        renderDigest (coldMaterializationFingerprint analyzed Nothing) @?= goldenColdMaterializationFingerprint
        -- The public function must digest exactly the frames the helper
        -- exposes; any divergence between the two would silently change the
        -- persisted materialization fingerprint.
        renderDigest (sha256DigestFrames (coldMaterializationFingerprintFrames analyzed Nothing)) @?= goldenColdMaterializationFingerprint,
      testCase "source fingerprint pins the exact frame sequence" $ do
        -- sourceFingerprint streams the identical framed byte sequence
        -- (header frame, config frames, then per-observation entry + blob
        -- frames in repoPathText order) into SHA-256, so the rendered digest
        -- pins the whole sequence byte-for-byte.
        renderDigest (sourceFingerprint guardRawObservation) @?= goldenSourceFingerprint
    ]

-- | Golden digests derived from the current frame sequences.  A failing
-- assertion reports the actual digest: update both sides of the equation
-- together only when the frame format change is intentional.
goldenColdMaterializationFingerprint :: Text
goldenColdMaterializationFingerprint = "sha256:xSl_EAlUQySjtO3M_8iLk8sVx2NQxILlfGODTO4GD7I"

goldenSourceFingerprint :: Text
goldenSourceFingerprint = "sha256:LwtXmmMXX-IYLVlEQHKtNsl4s07MskGuF9jctUN7al8"

-- Fixture -------------------------------------------------------------

-- | The analyzed snapshot the cold-materialization guard pins.  With
-- @Nothing@, its fingerprint includes header, schema, materializer, source,
-- diagnostic, and conflict frames, and excludes semantic and search frames.
guardAnalyzed :: [ParsedManagedDocument] -> AnalyzedRepositorySnapshot
guardAnalyzed documents =
  AnalyzedRepositorySnapshot
    { analyzedRawObservation = guardRawObservation,
      analyzedManagedEntries = [],
      analyzedNonblobObservations = [],
      analyzedDiagnostics = [guardDiagnostic],
      analyzedDocuments = documents,
       analyzedReduction = reduceManagedGraph (map parsedManagedRecord documents),
       analyzedConflicts = [guardConflict],
       analyzedHistoryComplete = True,
       analyzedHistoryCommitsScanned = 0,
       analyzedSourceFingerprint = sha256Digest "guard-source"
    }

-- | Documents parsed from the healthy compiler fixture corpus.
guardDocuments :: [ParsedManagedDocument]
guardDocuments =
  case healthyCompilerFiles guardBasisOid of
    Left problem -> error ("fingerprint guard fixture: " <> Text.unpack problem)
    Right files -> map parseDocument files
  where
    parseDocument (pathText, bytes) =
      case mkRepoPath (Text.pack pathText) of
        Left problem -> error ("fingerprint guard fixture: path: " <> show problem)
        Right path ->
          case parseManagedDocument path bytes of
            Left problem -> error ("fingerprint guard fixture: document: " <> show problem)
            Right document -> document

-- | The raw observation the source-fingerprint guard pins.  It carries a
-- committed config observation (origin, entry, blob, and managed-paths
-- frames) plus two tree observations with distinct paths: one with a blob
-- and one without, so both branches of the per-observation entry + blob
-- frame sequence contribute frames.
guardRawObservation :: RawRepositorySnapshotObservation
guardRawObservation =
  RawRepositorySnapshotObservation
    { rawRepositorySnapshotRevision = guardRevision,
      rawRepositorySnapshotConfig =
          RawRepositoryConfigObservation
            { rawRepositoryConfigOrigin = CommittedConfigOrigin,
              rawRepositoryConfigResult = Right defaultConfig,
              rawRepositoryConfigManagedPaths = Just guardManagedPaths,
              rawRepositoryConfigEntry = Just guardConfigEntry,
              rawRepositoryConfigBlob = Just guardConfigBlob
            },
      rawRepositorySnapshotManagedPaths = Just guardManagedPaths,
      rawRepositorySnapshotEntries =
        [ guardEntryObservation "a" (Just "guard blob one\n"),
          guardEntryObservation "b" Nothing
        ]
    }

guardEntryObservation :: String -> Maybe ByteString -> RepositoryTreeObservation
guardEntryObservation suffix blobBytes =
  RepositoryTreeObservation
    { repositoryTreeEntry =
          GitTreeEntry
            { gitTreePath = RepoPath ("docs/guard-" <> Text.pack suffix),
              gitTreeOid = guardEntryOid suffix,
              gitTreeObjectType = GitBlobObject,
              gitTreeMode = GitRegularFile
            },
      repositoryTreeBlob = fmap (GitBlob (guardEntryOid suffix)) blobBytes
    }

guardRevision :: ResolvedRepositoryRevision
guardRevision =
  ResolvedRepositoryRevision
    { resolvedRepository =
          Repository
            { repositoryClient = systemGit,
              repositoryWorktreeRoot = Nothing,
              repositoryGitDir = "guard",
              repositoryCommonDir = "guard",
              repositoryLayout = BareRepository,
              repositoryCommonIsBare = True,
              repositoryCommandDirectory = "guard"
            },
      resolvedRequestedRevision = RevisionSpec "guard-revision",
      resolvedCommitOid = guardEntryOid "a",
      resolvedRevisionKey = RepositoryRevisionKey "guard" (guardEntryOid "a")
    }

guardConfigEntry :: GitTreeEntry
guardConfigEntry =
  GitTreeEntry
    { gitTreePath = RepoPath "architecture/adrai/adrai.toml",
      gitTreeOid = guardEntryOid "c",
      gitTreeObjectType = GitBlobObject,
      gitTreeMode = GitRegularFile
    }

guardConfigBlob :: GitBlob
guardConfigBlob = GitBlob (guardEntryOid "c") (TextEncoding.encodeUtf8 "guard config blob\n")

guardManagedPaths :: ManagedPaths
guardManagedPaths =
  expect
    "managed paths"
    ( mkManagedPaths
        (RepoPath "architecture/adrai/decisions")
        (RepoPath "architecture/adrai/connections")
    )

guardDiagnostic :: CompilerDiagnostic
guardDiagnostic =
  CompilerDiagnostic
    { compilerDiagnosticSeverity = CompilerDiagnosticWarning,
      compilerDiagnosticOrigin = CompilerOperationOrigin,
      compilerDiagnosticCode = CompilerIntegrityCode InconsistentOperationCapsule,
      compilerDiagnosticAdr = Just compilerAdrId,
      compilerDiagnosticObject = Just (recordObjectRef compilerRecordId),
      compilerDiagnosticOperation = Just guardOperationId,
      compilerDiagnosticCommit = Nothing,
      compilerDiagnosticPath = Just guardDiagnosticPath,
      compilerDiagnosticMessage = "guard diagnostic"
    }

guardOperationId :: OperationId
guardOperationId = expect "operation" (mkOperationId (fixtureId 'O' '0'))

guardDiagnosticPath :: RepoPath
guardDiagnosticPath = expect "diagnostic path" (mkRepoPath "docs/guard-a")

guardConflict :: AdrConflict
guardConflict =
  AdrConflict
    { adrConflictCode = "ADR_CONFLICT",
      adrConflictAdr = compilerAdrId,
      adrConflictCandidates =
          [ ConflictCandidate
              { conflictCandidateAxis = DecisionAxis,
                conflictCandidateHeads = [recordObjectRef compilerRecordId],
                conflictCandidateHeadCount = 1,
                conflictCandidateSummary = "guard conflict"
              }
          ],
      adrConflictCount = 1,
      adrConflictSummaries = ["guard conflict summary"],
      adrConflictStateToken = StateToken ("S" <> Text.replicate 22 "0")
    }

guardBasisOid :: GitOid
guardBasisOid = expect "basis" (mkGitOid (Text.replicate 40 "a"))

guardEntryOid :: String -> GitOid
guardEntryOid suffix =
  expect "entry oid" (mkGitOid (Text.replicate 39 "0" <> Text.pack suffix))

fixtureId :: Char -> Char -> Text
fixtureId prefix suffix = Text.singleton prefix <> Text.replicate 25 "0" <> Text.singleton suffix

expect :: (Show problem) => Text -> Either problem value -> value
expect context result =
  case result of
    Left problem -> error ("fingerprint guard fixture: " <> Text.unpack context <> ": " <> show problem)
    Right value -> value
