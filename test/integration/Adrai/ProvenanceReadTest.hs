{-# LANGUAGE OverloadedStrings #-}

module Adrai.ProvenanceReadTest (tests) where

import Adrai.Git (GitOid, Repository, discoverRepository, gitOidText, systemGit)
import Adrai.GitTestSupport
  ( commitFile,
    gitSuccess,
    initTestRepository,
    outputText,
    requireRepoPath,
    requireRevision,
  )
import Adrai.Provenance
  ( EventKind,
    ProvenanceCapsule,
    ProvenanceCapsuleInput (..),
    ProvenanceObjectId (..),
    mkEventKind,
     mkGitOid,
     mkProvenanceCapsule,
     sealSemantic,
     semanticDigest,
   )
import Adrai.Provenance.Classification (ParsedManagedDocument (..), operationSignature)
import Adrai.Provenance.Read
  ( PlacementHydrationError (..),
    hydratePlacementEvidenceAt,
    hydratePlacementEvidenceAtWith,
    hydratePlacementEvidenceAtWithHooks,
    materialize,
  )
import Adrai.Provenance.Overlay
  ( LineConfigRow (..),
    LineLandingRow (..),
    OperationCommitRow (..),
    ProvenanceEvidence (..),
    ProvenanceOperationEvidence (..),
    RegisteredObjectRow (..),
    RegisteredOperationRow (..),
  )
import Adrai.Provenance.Lock
  ( acquireOverlayLock,
    releaseOverlayLock,
  )
import Adrai.Sqlite (asQuery)
import Adrai.Repository
  ( ResolvedRepositoryRevision,
    resolveRepositoryRevision,
    resolvedCommitOid,
    resolvedRepository,
  )
import Adrai.Provenance.Ensure (configKey)
import Adrai.History
  ( CommitPlacementEvidence (..),
    LineLandingEvidence (..),
    PlacementEvidence (..),
  )
import Adrai.Types
  ( Actor,
    ActorKind (HumanActor),
    AdrId,
    Config,
     ConfigSchema (ConfigSchemaV1),
     digestBytes,
    GitRef (GitRef),
    LogicalLine (LogicalLine),
    OperationId,
    ProvenanceInputs (ProvenanceInputs),
    mkActor,
    mkAdrId,
    mkConfig,
    mkManagedPaths,
    mkOperationId,
    operationIdText,
    repoPathText,
  )
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Control.Concurrent (forkIO, threadDelay, throwTo)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (AsyncException (ThreadKilled), SomeException, bracket, fromException, throwIO, try)
import Database.SQLite.Simple (Only (..), SQLData (SQLInteger, SQLText), close, execute, open, query_)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup "ProvenanceRead"
    [ testCase "empty snapshot is deterministic and does not create a cache" emptySnapshotTest,
      testCase "cold and warm hydration preserve genuine original placement for Unicode paths" coldWarmTest,
      testCase "configuration evolution is exact and reusable at the public boundary" configEvolutionTest,
      testCase "a real cherry-pick retains copy placement metadata" copyPlacementTest,
      testCase "a real merge preserves first-parent order" mergeParentOrderTest,
      testCase "historical revision stays isolated after HEAD advances" historicalIsolationTest,
      testCase "tampered cache mismatches fail closed while missing placement repairs" tamperedCacheTest,
      testCase "invalid existing schema is typed and byte-preserved" invalidSchemaPreservationTest,
      testCase "v1 overlay is rebuilt as v2 before placement hydration" v1OverlayRebuiltBeforeHydrationTest,
      testCase "extra overlay trigger and view are invalid and byte-preserved" extraExecutableObjectsPreservationTest,
       testCase "materialize indexes canonical identities, groups documents, and rejects registration drift" materializeCheckedIdentityIndexTest,
       testCase "an unopenable cache location is a synchronous typed failure" synchronousFailureTest,
      testCase "writer cancellation removes a fresh overlay and releases the lock" freshCancellationCleanupTest,
      testCase "warm validation cancellation propagates without mutation" warmValidationCancellationTest,
      testCase "contended lock cancellation propagates without mutation" contendedLockCancellationTest
    ]

data ObservableRepositoryState = ObservableRepositoryState
  { observedHead :: Text.Text,
    observedRef :: Text.Text,
    observedTree :: Text.Text,
    observedRaw :: BS.ByteString,
    observedStaged :: BS.ByteString,
    observedIndex :: BS.ByteString,
    observedStatus :: BS.ByteString,
    observedManagedBytes :: BS.ByteString,
    observedCacheBytes :: Maybe BS.ByteString
  }
  deriving (Eq, Show)

snapshotRepositoryState :: FilePath -> FilePath -> IO ObservableRepositoryState
snapshotRepositoryState repositoryPath managedPath = do
  observedHead <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD"] ""
  observedRef <- outputText <$> gitSuccess repositoryPath ["symbolic-ref", "-q", "HEAD"] ""
  observedTree <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD^{tree}"] ""
  observedRaw <- gitSuccess repositoryPath ["diff", "--raw", "-z"] ""
  observedStaged <- gitSuccess repositoryPath ["diff", "--cached", "--raw", "-z"] ""
  observedIndex <- gitSuccess repositoryPath ["ls-files", "--stage", "-z"] ""
  observedStatus <- gitSuccess repositoryPath ["status", "--porcelain=v1", "-z"] ""
  observedManagedBytes <- BS.readFile (repositoryPath </> managedPath)
  observedCacheBytes <- readIfExists (repositoryPath </> ".adrai" </> "provenance.sqlite")
  pure
    (ObservableRepositoryState
      observedHead observedRef observedTree observedRaw observedStaged
      observedIndex observedStatus observedManagedBytes observedCacheBytes)

readIfExists :: FilePath -> IO (Maybe BS.ByteString)
readIfExists path = do
  exists <- doesFileExist path
  if exists then Just <$> BS.readFile path else pure Nothing

documentManagedPath :: ParsedManagedDocument -> FilePath
documentManagedPath = Text.unpack . repoPathText . parsedManagedPath

assertRepositoryStatePreserved :: ObservableRepositoryState -> ObservableRepositoryState -> IO ()
assertRepositoryStatePreserved before after = do
  ( observedHead after,
    observedRef after,
    observedTree after,
    observedRaw after,
    observedStaged after,
    observedIndex after,
    observedStatus after,
    observedManagedBytes after
    )
    @?=
    ( observedHead before,
      observedRef before,
      observedTree before,
      observedRaw before,
      observedStaged before,
      observedIndex before,
      observedStatus before,
      observedManagedBytes before
      )
  case observedCacheBytes before of
    Nothing -> pure ()
    Just cache -> observedCacheBytes after @?= Just cache

-- Cache maintenance may intentionally observe a newly advanced ref while an
-- old resolved revision is read.  The caller-visible repository state must
-- still remain byte-for-byte stable.
assertRepositoryWorktreePreserved :: ObservableRepositoryState -> ObservableRepositoryState -> IO ()
assertRepositoryWorktreePreserved before after =
  assertRepositoryStatePreserved (before {observedCacheBytes = Nothing}) after

emptySnapshotTest :: IO ()
emptySnapshotTest =
  withSystemTempDirectory "adrai provenance read empty" $ \temporary -> do
    let repositoryPath = temporary </> "repository"
    initTestRepository repositoryPath
    seed <- commitFile repositoryPath "seed.txt" "seed\n"
    resolved <- requireResolved repositoryPath seed
    result <- hydratePlacementEvidenceAt (repositoryOf resolved) resolved testConfig []
    result @?= Right Map.empty
    cacheDirectoryExists <- doesDirectoryExist (repositoryPath </> ".adrai")
    assertBool "empty hydration must not create the cache directory" (not cacheDirectoryExists)

coldWarmTest :: IO ()
coldWarmTest =
  withSystemTempDirectory "adrai provenance read Unicode" $ \temporary -> do
    let repositoryPath = temporary </> "repo with spaces ünicode"
        managedPath = "architecture/adrai/decisions/000/ü spaced.decision.md"
    initTestRepository repositoryPath
    _ <- commitFile repositoryPath ".gitignore" ".adrai/\n"
    basisText <- commitFile repositoryPath "seed.txt" "seed\n"
    basis <- requireOid basisText
    let operation = requireOperation "O00000000000000000000000091"
        adr = requireAdr "A00000000000000000000000091"
        semantic = "# Unicode provenance\n"
        capsule = makeCapsule operation adr basis semantic
        documentBytes = TextEncoding.encodeUtf8 (sealSemantic semantic capsule)
    targetText <- commitFile repositoryPath managedPath documentBytes
    blobText <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD:" <> managedPath] ""
    blob <- requireOid blobText
    resolved <- requireResolved repositoryPath targetText
    let document =
          ParsedManagedDocument
            { parsedDocumentObjectRef = "A00000000000000000000000091",
              parsedManagedPath = requireRepoPath (Text.pack managedPath),
              parsedManagedCapsule = capsule,
              parsedBlobOid = Just blob,
              parsedSemanticHash = Text.pack (show (semanticDigest semantic))
            }
        repository = repositoryOf resolved
    BS.writeFile (repositoryPath </> "seed.txt") "dirty worktree state\n"
    BS.writeFile (repositoryPath </> "untracked evidence.txt") "untracked worktree state\n"
    BS.writeFile (repositoryPath </> ".gitignore") ".adrai/\n# staged caller state\n"
    _ <- gitSuccess repositoryPath ["add", ".gitignore"] ""
    beforeState <- snapshotRepositoryState repositoryPath (documentManagedPath document)
    cold <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    warm <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    afterState <- snapshotRepositoryState repositoryPath (documentManagedPath document)
    assertRepositoryStatePreserved beforeState afterState
    cold @?= warm
    case cold of
      Left problem -> assertFailure ("hydratePlacementEvidenceAt: " <> show problem)
      Right placements -> do
        assertBool "map keys are exactly snapshot operation ids" (Map.keys placements == [operation])
        case Map.lookup operation placements of
          Nothing -> assertFailure "operation evidence missing"
          Just evidence -> do
            assertBool "genuine original placement is present" (not (null (placementOriginalCommits evidence)))
            placementOriginalCommits evidence @?= [targetText]
            assertBool "placement parents were populated" (all (not . null . commitPlacementParents) (placementCommits evidence))
            assertBool "placement timestamps are exact milliseconds" (all (\placement -> commitPlacementAuthoredAtMs placement `mod` 1000 == 0 && commitPlacementCommittedAtMs placement `mod` 1000 == 0) (placementCommits evidence))
            assertBool "target-relative evidence is reachable" (all commitPlacementReachable (placementCommits evidence))

configEvolutionTest :: IO ()
configEvolutionTest =
  withSystemTempDirectory "adrai provenance read config evolution" $ \temporary -> do
    let repositoryPath = temporary </> "repo with spaces config"
        managedPath = "architecture/adrai/decisions/000/config spaced.decision.md"
    initTestRepository repositoryPath
    _ <- commitFile repositoryPath ".gitignore" ".adrai/\n"
    basisText <- commitFile repositoryPath "seed.txt" "seed\n"
    basis <- requireOid basisText
    let operation = requireOperation "O00000000000000000000000096"
        adr = requireAdr "A00000000000000000000000096"
        semantic = "# Configuration evolution provenance\n"
        capsule = makeCapsule operation adr basis semantic
    targetText <- commitFile repositoryPath managedPath (TextEncoding.encodeUtf8 (sealSemantic semantic capsule))
    blobText <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD:" <> managedPath] ""
    blob <- requireOid blobText
    resolved <- requireResolved repositoryPath targetText
    let document =
          ParsedManagedDocument
            { parsedDocumentObjectRef = "A00000000000000000000000096",
              parsedManagedPath = requireRepoPath (Text.pack managedPath),
              parsedManagedCapsule = capsule,
              parsedBlobOid = Just blob,
              parsedSemanticHash = Text.pack (show (semanticDigest semantic))
            }
        repository = repositoryOf resolved
    trunk <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    case trunk >>= maybe (Left (PlacementHydrationRegistrationMismatch "missing trunk config evidence")) Right . Map.lookup operation of
      Left problem -> assertFailure (show problem)
      Right evidence ->
        assertBool "first configuration returns only its complete trunk landing" $
          any (\landing -> landingLine landing == "trunk" && landingRef landing == "refs/heads/main" && landingCommit landing == targetText && landingComplete landing)
            (placementLineLandings evidence)
    release <- hydratePlacementEvidenceAt repository resolved releaseConfig [document]
    releaseAgain <- hydratePlacementEvidenceAt repository resolved releaseConfig [document]
    releaseAgain @?= release
    case release >>= maybe (Left (PlacementHydrationRegistrationMismatch "missing release config evidence")) Right . Map.lookup operation of
      Left problem -> assertFailure (show problem)
      Right evidence -> do
        assertBool "evolved configuration returns its exact complete release landing" $
          any (\landing -> landingLine landing == "release" && landingRef landing == "refs/heads/main" && landingCommit landing == targetText && landingComplete landing)
            (placementLineLandings evidence)
        assertBool "evolved result excludes the prior configuration landing" $
          all ((/= "trunk") . landingLine) (placementLineLandings evidence)

copyPlacementTest :: IO ()
copyPlacementTest =
  withSystemTempDirectory "adrai provenance read copy" $ \temporary -> do
    let repositoryPath = temporary </> "repo with spaces ünicode"
        managedPath = "architecture/adrai/decisions/000/ü spaced.decision.md"
    initTestRepository repositoryPath
    _ <- commitFile repositoryPath ".gitignore" ".adrai/\n"
    basisText <- commitFile repositoryPath "seed.txt" "seed\n"
    _ <- gitSuccess repositoryPath ["branch", "feature"] ""
    basis <- requireOid basisText
    let operation = requireOperation "O00000000000000000000000092"
        adr = requireAdr "A00000000000000000000000092"
        semantic = "# Copy provenance\n"
        capsule = makeCapsule operation adr basis semantic
        documentBytes = TextEncoding.encodeUtf8 (sealSemantic semantic capsule)
    _ <- commitFile repositoryPath managedPath documentBytes
    _ <- gitSuccess repositoryPath
      [ "commit", "--amend", "--no-edit", "--trailer"
      , "ADRAI-Op: " <> Text.unpack (operationIdText operation)
      ] ""
    originalCommit <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD"] ""
    blobText <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD:" <> managedPath] ""
    blob <- requireOid blobText
    originalResolved <- requireResolved repositoryPath originalCommit
    let document = ParsedManagedDocument
          { parsedDocumentObjectRef = "A00000000000000000000000092",
            parsedManagedPath = requireRepoPath (Text.pack managedPath),
            parsedManagedCapsule = capsule,
            parsedBlobOid = Just blob,
            parsedSemanticHash = Text.pack (show (semanticDigest semantic))
          }
    originalEvidence <- hydratePlacementEvidenceAt (repositoryOf originalResolved) originalResolved testConfig [document]
    case originalEvidence >>= maybe (Left (PlacementHydrationRegistrationMismatch "missing original evidence")) Right . Map.lookup operation of
      Left problem -> assertFailure (show problem)
      Right evidence -> do
        placementOriginalCommits evidence @?= [originalCommit]
        assertBool "copy is absent before the later topology exists" (all ((/= "copy") . commitPlacementClassification) (placementCommits evidence))
    _ <- gitSuccess repositoryPath ["switch", "feature"] ""
    copyParent <- commitFile repositoryPath "feature context.txt" "feature\n"
    _ <- gitSuccess repositoryPath ["cherry-pick", Text.unpack originalCommit] ""
    copiedCommit <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD"] ""
    resolved <- requireResolved repositoryPath copiedCommit
    result <- hydratePlacementEvidenceAt (repositoryOf resolved) resolved testConfig [document]
    case result >>= maybe (Left (PlacementHydrationRegistrationMismatch "missing copy evidence")) Right . Map.lookup operation of
      Left problem -> assertFailure (show problem)
      Right evidence -> do
        assertBool "copy classification is real evidence" ("copy" `elem` map commitPlacementClassification (placementCommits evidence))
        assertBool "copy placement retains its Git parent topology" (all (not . null . commitPlacementParents) (placementCommits evidence))
        [copyPlacement] <- pure [placement | placement <- placementCommits evidence, commitPlacementClassification placement == "copy"]
        commitPlacementParents copyPlacement @?= [copyParent]
    historicalEvidence <- hydratePlacementEvidenceAt (repositoryOf originalResolved) originalResolved testConfig [document]
    historicalEvidence @?= originalEvidence

mergeParentOrderTest :: IO ()
mergeParentOrderTest =
  withSystemTempDirectory "adrai provenance read merge parents" $ \temporary -> do
    let repositoryPath = temporary </> "repo with spaces Ã¼nicode"
        managedPath = "architecture/adrai/decisions/000/Ã¼ merge.decision.md"
    initTestRepository repositoryPath
    basisText <- commitFile repositoryPath "seed.txt" "seed\n"
    basis <- requireOid basisText
    _ <- gitSuccess repositoryPath ["branch", "feature"] ""
    mainParent <- commitFile repositoryPath "main context.txt" "main\n"
    _ <- gitSuccess repositoryPath ["switch", "feature"] ""
    let operation = requireOperation "O00000000000000000000000094"
        adr = requireAdr "A00000000000000000000000094"
        semantic = "# Merge provenance\n"
        capsule = makeCapsule operation adr basis semantic
    featureCommit <- commitFile repositoryPath managedPath (TextEncoding.encodeUtf8 (sealSemantic semantic capsule))
    _ <- gitSuccess repositoryPath ["switch", "main"] ""
    _ <- gitSuccess repositoryPath ["merge", "--no-ff", "feature", "-m", "merge fixture"] ""
    mergeCommit <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD"] ""
    blobText <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD:" <> managedPath] ""
    blob <- requireOid blobText
    resolved <- requireResolved repositoryPath mergeCommit
    let document =
          ParsedManagedDocument
            { parsedDocumentObjectRef = "A00000000000000000000000094",
              parsedManagedPath = requireRepoPath (Text.pack managedPath),
              parsedManagedCapsule = capsule,
              parsedBlobOid = Just blob,
              parsedSemanticHash = Text.pack (show (semanticDigest semantic))
            }
    result <- hydratePlacementEvidenceAt (repositoryOf resolved) resolved testConfig [document]
    case result >>= maybe (Left (PlacementHydrationRegistrationMismatch "missing merge evidence")) Right . Map.lookup operation of
      Left problem -> assertFailure (show problem)
      Right evidence ->
        case [placement | placement <- placementCommits evidence, commitPlacementOid placement == mergeCommit] of
          [mergePlacement] -> commitPlacementParents mergePlacement @?= [mainParent, featureCommit]
          other -> assertFailure ("expected one merge placement, got " <> show other)

historicalIsolationTest :: IO ()
historicalIsolationTest =
  withFixture "historical" $ \repositoryPath repository resolved document operation -> do
    before <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    let database = repositoryPath </> ".adrai" </> "provenance.sqlite"
    generationBefore <- bracket (open database) close $ \connection ->
      query_ connection "SELECT value FROM meta WHERE key='generation'" :: IO [Only Text.Text]
    observedBefore <- bracket (open database) close $ \connection ->
      query_ connection "SELECT value FROM meta WHERE key='observed_commit_count'" :: IO [Only Text.Text]
    let laterOperation = requireOperation "O00000000000000000000000093"
        laterAdr = requireAdr "A00000000000000000000000093"
        laterSemantic = "# Later managed provenance\n"
        laterCapsule = makeCapsule laterOperation laterAdr (resolvedCommitOid resolved) laterSemantic
    _ <- commitFile repositoryPath (documentManagedPath document) (TextEncoding.encodeUtf8 (sealSemantic laterSemantic laterCapsule))
    advancedRef <- outputText <$> gitSuccess repositoryPath ["rev-parse", "refs/heads/main"] ""
    assertBool "fixture advanced the configured ref" (advancedRef /= gitOidText (resolvedCommitOid resolved))
    beforeHistorical <- snapshotRepositoryState repositoryPath (documentManagedPath document)
    after <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    afterHistorical <- snapshotRepositoryState repositoryPath (documentManagedPath document)
    assertRepositoryWorktreePreserved beforeHistorical afterHistorical
    before @?= after
    generationAfter <- bracket (open database) close $ \connection ->
      query_ connection "SELECT value FROM meta WHERE key='generation'" :: IO [Only Text.Text]
    observedAfter <- bracket (open database) close $ \connection ->
      query_ connection "SELECT value FROM meta WHERE key='observed_commit_count'" :: IO [Only Text.Text]
    assertBool "direct hydration refreshes stale ref/reflog observation evidence" (generationAfter > generationBefore)
    assertBool "direct hydration discovers the newly reachable ref evidence" (observedAfter > observedBefore)
    repeated <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    repeated @?= after
    observedRepeated <- bracket (open database) close $ \connection ->
      query_ connection "SELECT value FROM meta WHERE key='observed_commit_count'" :: IO [Only Text.Text]
    observedRepeated @?= observedAfter
    case after of
      Right placements -> Map.keys placements @?= [operation]
      Left problem -> assertFailure (show problem)

tamperedCacheTest :: IO ()
tamperedCacheTest =
  withFixture "tampered" $ \repositoryPath repository resolved document operation -> do
    initial <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    case initial of
      Left problem -> assertFailure (show problem)
      Right _ -> pure ()
    let database = repositoryPath </> ".adrai" </> "provenance.sqlite"
        operationText = operationIdText operation
    -- Make observation roots stale as well as the evidence row.  The direct
    -- hydration gate must reject the contradictory row before maintenance and
    -- preserve the caller-owned database byte-for-byte.
    _ <- commitFile repositoryPath "freshness-noise.txt" "ref/reflog freshness noise\n"
    tamper database "UPDATE registered_operation SET adr_id='A00000000000000000000000999' WHERE op_id=?" [SQLText operationText]
    staleTamperBytes <- BS.readFile database
    beforeRejection <- snapshotRepositoryState repositoryPath (documentManagedPath document)
    wrongAdr <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    case wrongAdr of
      Left (PlacementHydrationRegistrationMismatch _) -> pure ()
      other -> assertFailure ("expected typed ADR failure, got " <> show other)
    afterRejection <- snapshotRepositoryState repositoryPath (documentManagedPath document)
    assertRepositoryStatePreserved beforeRejection afterRejection
    staleTamperBytesAfter <- BS.readFile database
    staleTamperBytesAfter @?= staleTamperBytes
    tamper database "UPDATE registered_operation SET adr_id='A00000000000000000000000091' WHERE op_id=?" [SQLText operationText]
    tamper database "UPDATE operation_commit SET classification='invalid' WHERE op_id=?" [SQLText operationText]
    invalidClassification <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    case invalidClassification of
      Left (PlacementHydrationInvalidClassification "invalid") -> pure ()
      other -> assertFailure ("expected typed classification failure, got " <> show other)
    tamper database "UPDATE operation_commit SET classification='original', commit_oid='not-an-oid' WHERE op_id=?" [SQLText operationText]
    malformedPlacement <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    case malformedPlacement of
      Left (PlacementHydrationEvidenceFailure _) -> pure ()
      other -> assertFailure ("expected typed malformed-OID failure, got " <> show other)
    tamper database "UPDATE operation_commit SET commit_oid=? WHERE op_id=?" [SQLText (gitOidText (resolvedCommitOid resolved)), SQLText operationText]
    tamper database "UPDATE operation_commit SET classification='original', parents_json='not-json' WHERE op_id=?" [SQLText operationText]
    invalidParents <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    case invalidParents of
      Left (PlacementHydrationInvalidParents _) -> pure ()
      other -> assertFailure ("expected typed parent failure, got " <> show other)
    tamper database "UPDATE operation_commit SET parents_json='[]' WHERE op_id=?" [SQLText operationText]
    tamper database "UPDATE registered_object SET path='wrong-owner.md' WHERE op_id=?" [SQLText operationText]
    wrongOwner <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    case wrongOwner of
      Left (PlacementHydrationObjectMismatch _) -> pure ()
      other -> assertFailure ("expected typed ownership failure, got " <> show other)
    tamper database "UPDATE registered_object SET path=? WHERE op_id=?" [SQLText "architecture/adrai/decisions/000/ü spaced.decision.md", SQLText operationText]
    let requestedConfig = configKey "architecture/adrai/decisions" "architecture/adrai/connections" ["trunk"]
    tamper database "INSERT OR REPLACE INTO line_landing VALUES(?,?,?,?,?,?)"
      [ SQLText requestedConfig, SQLText operationText, SQLText "trunk", SQLText "refs/heads/main"
      , SQLText (gitOidText (resolvedCommitOid resolved)), SQLInteger 3
      ]
    invalidLanding <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    case invalidLanding of
      Left (PlacementHydrationInvalidLanding _) -> pure ()
      other -> assertFailure ("expected typed landing failure, got " <> show other)
    tamper database "UPDATE line_landing SET complete=1 WHERE op_id=?" [SQLText operationText]
    tamper database "UPDATE line_landing SET line_id='stale-line' WHERE op_id=?" [SQLText operationText]
    beforeStaleLine <- snapshotRepositoryState repositoryPath (documentManagedPath document)
    staleLine <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    case staleLine of
      Left (PlacementHydrationInvalidLanding _) -> pure ()
      other -> assertFailure ("expected typed stale-line failure, got " <> show other)
    afterStaleLine <- snapshotRepositoryState repositoryPath (documentManagedPath document)
    assertRepositoryStatePreserved beforeStaleLine afterStaleLine
    tamper database "UPDATE line_landing SET line_id='trunk' WHERE op_id=?" [SQLText operationText]
    tamper database "UPDATE line_landing SET ref_name='refs/heads/stale' WHERE op_id=?" [SQLText operationText]
    beforeStaleRef <- snapshotRepositoryState repositoryPath (documentManagedPath document)
    staleRef <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    case staleRef of
      Left (PlacementHydrationInvalidLanding _) -> pure ()
      other -> assertFailure ("expected typed stale-ref failure, got " <> show other)
    afterStaleRef <- snapshotRepositoryState repositoryPath (documentManagedPath document)
    assertRepositoryStatePreserved beforeStaleRef afterStaleRef
    tamper database "UPDATE line_landing SET ref_name='refs/heads/main' WHERE op_id=?" [SQLText operationText]
    tamper database "UPDATE line_config SET config_json='{}'" []
    staleConfig <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    case staleConfig of
      Left (PlacementHydrationInvalidConfig _) -> pure ()
      other -> assertFailure ("expected typed stale-config failure, got " <> show other)
    tamper database "UPDATE line_config SET config_json=?" [SQLText expectedConfigJson]
    tamper database "DELETE FROM operation_commit WHERE op_id=?" [SQLText operationText]
    missingPlacement <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    case missingPlacement of
      Right placements -> assertBool "missing target placement is repaired from authoritative documents" (Map.member operation placements)
      other -> assertFailure ("expected missing-placement repair, got " <> show other)
  where
    expectedConfigJson = "{\"connections\":\"architecture/adrai/connections\",\"decisions\":\"architecture/adrai/decisions\",\"logical_lines\":[\"trunk\"]}"

invalidSchemaPreservationTest :: IO ()
invalidSchemaPreservationTest =
  withFixture "invalid schema" $ \repositoryPath repository resolved document _ -> do
    initial <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    case initial of
      Left problem -> assertFailure (show problem)
      Right _ -> pure ()
    let database = repositoryPath </> ".adrai" </> "provenance.sqlite"
    tamper database "UPDATE meta SET value='adrai-provenance-cache/wrong' WHERE key='schema'" []
    beforeRejection <- snapshotRepositoryState repositoryPath (documentManagedPath document)
    result <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    result @?= Left (PlacementHydrationInvalidOverlay "existing overlay schema is invalid")
    afterRejection <- snapshotRepositoryState repositoryPath (documentManagedPath document)
    assertRepositoryStatePreserved beforeRejection afterRejection

-- | Overlay v1 predates target-bound placement certificates.  It must be
-- discarded and rebuilt from the immutable requested documents, never merely
-- retagged as v2 or trusted for a warm read.
v1OverlayRebuiltBeforeHydrationTest :: IO ()
v1OverlayRebuiltBeforeHydrationTest =
  withFixture "v1 overlay rebuild" $ \repositoryPath repository resolved document operation -> do
    initial <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    case initial of
      Left problem -> assertFailure ("initial v2 hydration: " <> show problem)
      Right _ -> pure ()
    let database = repositoryPath </> ".adrai" </> "provenance.sqlite"
    tamper database "UPDATE meta SET value='adrai-provenance-cache/1' WHERE key='schema'" []
    -- A genuine v1 database has no coverage table.  Removing it also proves
    -- the writer rebuilds schema instead of relying on a partial upgrade.
    tamper database "DROP TABLE operation_target_coverage" []
    schemaFacts <- bracket (open database) close $ \connection ->
      query_ connection "SELECT type,name,sql FROM sqlite_master WHERE sql IS NOT NULL AND type IN ('table','index') ORDER BY type,name" :: IO [(Text.Text, Text.Text, Text.Text)]
    schemaTag <- bracket (open database) close $ \connection ->
      query_ connection "SELECT value FROM meta WHERE key='schema'" :: IO [Only Text.Text]
    assertBool ("legacy v1 schema facts are present: " <> show schemaFacts) (not (null schemaFacts))
    rebuilt <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    case rebuilt >>= maybe (Left (PlacementHydrationRegistrationMismatch "missing rebuilt operation evidence")) Right . Map.lookup operation of
      Left problem -> assertFailure ("v1 rebuild hydration: " <> show problem <> "; tag=" <> show schemaTag <> "; facts=" <> show schemaFacts)
      Right evidence -> assertBool "rebuilt overlay contains reachable placement" (not (null (placementCommits evidence)))
    connection <- open database
    schema <- query_ connection "SELECT value FROM meta WHERE key='schema'" :: IO [Only Text.Text]
    coverage <- query_ connection "SELECT registration_signature FROM operation_target_coverage" :: IO [Only Text.Text]
    close connection
    schema @?= [Only "adrai-provenance-cache/2"]
    assertBool "v2 rebuild issues a target-bound placement certificate" (not (null coverage))

extraExecutableObjectsPreservationTest :: IO ()
extraExecutableObjectsPreservationTest =
  withFixture "extra overlay executable objects" $ \repositoryPath repository resolved document _ -> do
    initial <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    case initial of
      Left problem -> assertFailure (show problem)
      Right _ -> pure ()
    let database = repositoryPath </> ".adrai" </> "provenance.sqlite"
    tamper database "CREATE VIEW overlay_extra_view AS SELECT key FROM meta" []
    tamper database "CREATE TRIGGER overlay_extra_trigger AFTER INSERT ON meta BEGIN SELECT 1; END" []
    beforeRejection <- BS.readFile database
    result <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    result @?= Left (PlacementHydrationInvalidOverlay "existing overlay schema is invalid")
    afterRejection <- BS.readFile database
    afterRejection @?= beforeRejection

-- | Exercise the production materializer without an overlay round trip,
-- retaining its canonical identity and registration-drift checks.
materializeCheckedIdentityIndexTest :: IO ()
materializeCheckedIdentityIndexTest = do
  let target = pureOid "0000000000000000000000000000000000000011"
      operationA = requireOperation "O00000000000000000000000011"
      operationB = requireOperation "O00000000000000000000000012"
      documents =
        [ syntheticDocument target operationA "A00000000000000000000000011" "architecture/adrai/decisions/000/a.decision.md",
          syntheticDocument target operationA "A00000000000000000000000012" "architecture/adrai/decisions/000/b.decision.md",
          syntheticDocument target operationB "A00000000000000000000000013" "architecture/adrai/decisions/000/c.decision.md"
        ]
      -- Preserve the established Map.fromListWith (<>) grouping order: repeated
      -- operations are accumulated newest-first, and registration validation
      -- deliberately reads that bucket's first document.
      evidenceA = syntheticEvidence target (reverse (take 2 documents)) operationA
      evidenceB = syntheticEvidence target (drop 2 documents) operationB
      valid = materialize testConfig documents fixtureConfigKey (syntheticProvenance target [evidenceA, evidenceB])
      assertRegistrationDrift label value =
        case value of
          Left (PlacementHydrationRegistrationMismatch _) -> pure ()
          other -> assertFailure (label <> ": expected registration mismatch, got " <> show other)
  case valid of
    Left problem -> assertFailure ("valid checked identity index failed: " <> show problem)
    Right placements -> do
      Map.keys placements @?= [operationA, operationB]
  -- Canonical typed identities render distinctly.  The implementation retains
  -- a collision guard even though the validated constructor currently makes a
  -- distinct-value/same-text collision unconstructible.
  assertBool "constructible canonical operation identities have distinct exact text" (operationIdText operationA /= operationIdText operationB)
  let malformed = evidenceA {provenanceEvidenceRegistration = (provenanceEvidenceRegistration evidenceA) {registeredOperationRowOpId = "not-an-operation"}}
      caseVariant = evidenceA {provenanceEvidenceRegistration = (provenanceEvidenceRegistration evidenceA) {registeredOperationRowOpId = Text.toLower (operationIdText operationA)}}
      duplicate = syntheticProvenance target [evidenceA, evidenceA, evidenceB]
      missing = syntheticProvenance target [evidenceA]
      extra = syntheticProvenance target [evidenceA, evidenceB, malformed]
      reordered = syntheticProvenance target [evidenceB, evidenceA]
  assertRegistrationDrift "malformed exact registration" (materialize testConfig documents fixtureConfigKey (syntheticProvenance target [malformed, evidenceB]))
  assertRegistrationDrift "case-variant registration" (materialize testConfig documents fixtureConfigKey (syntheticProvenance target [caseVariant, evidenceB]))
  assertRegistrationDrift "duplicate registration" (materialize testConfig documents fixtureConfigKey duplicate)
  assertRegistrationDrift "missing registration" (materialize testConfig documents fixtureConfigKey missing)
  assertRegistrationDrift "extra registration" (materialize testConfig documents fixtureConfigKey extra)
  assertRegistrationDrift "reordered registration" (materialize testConfig documents fixtureConfigKey reordered)
  where
    fixtureConfigKey = configKey "architecture/adrai/decisions" "architecture/adrai/connections" ["trunk"]
    pureOid value = case mkGitOid value of
      Left problem -> error (show problem)
      Right result -> result
    syntheticDocument target operation adr path =
      ParsedManagedDocument
        { parsedDocumentObjectRef = adr,
          parsedManagedPath = requireRepoPath path,
          parsedManagedCapsule = makeCapsule operation (requireAdr adr) target ("synthetic " <> adr),
          parsedBlobOid = Just target,
          parsedSemanticHash = "synthetic-" <> adr
        }
    syntheticEvidence target documents operation =
      ProvenanceOperationEvidence
        { provenanceEvidenceRegistration =
            RegisteredOperationRow
              { registeredOperationRowOpId = operationIdText operation,
                registeredOperationRowAdrId = Just (parsedDocumentObjectRef (firstDocument documents)),
                registeredOperationRowBasisOid = target,
                registeredOperationRowSignature = signature documents
              },
          provenanceEvidenceObjects =
            [ RegisteredObjectRow (operationIdText operation) (parsedDocumentObjectRef document) (repoPathText (parsedManagedPath document)) target
              | document <- documents
            ],
          provenanceEvidenceCommits = [OperationCommitRow (operationIdText operation) target "original" 0 0 "synthetic" "[]"],
          provenanceEvidenceLandings = [LineLandingRow fixtureConfigKey (operationIdText operation) "trunk" "refs/heads/main" target 1],
          provenanceEvidenceIssues = []
        }
    firstDocument documents = case documents of
      document : _ -> document
      [] -> error "synthetic provenance evidence requires a document"
    syntheticProvenance target operations =
      ProvenanceEvidence
        { provenanceEvidenceTargetOid = target,
           provenanceEvidenceConfig = Just (LineConfigRow fixtureConfigKey "{\"connections\":\"architecture/adrai/connections\",\"decisions\":\"architecture/adrai/decisions\",\"logical_lines\":[\"trunk\"]}"),
          provenanceEvidenceOperations = operations,
          provenanceEvidenceLineRefs = [],
          provenanceEvidenceRefs = [],
          provenanceEvidenceRoots = []
        }
    signature = Text.concat . map byteHex . BS.unpack . digestBytes . operationSignature
    byteHex byte = Text.pack [hex (byte `div` 16), hex (byte `mod` 16)]
    hex nibble
      | nibble < 10 = toEnum (fromEnum '0' + fromIntegral nibble)
      | otherwise = toEnum (fromEnum 'a' + fromIntegral nibble - 10)

synchronousFailureTest :: IO ()
synchronousFailureTest =
  withFixture "unopenable" $ \repositoryPath repository resolved document _ -> do
    writeFile (repositoryPath </> ".adrai") "not a cache directory"
    result <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    case result of
      Left (PlacementHydrationSynchronousFailure _) -> pure ()
      other -> assertFailure ("expected typed synchronous failure, got " <> show other)

freshCancellationCleanupTest :: IO ()
freshCancellationCleanupTest =
  withFixture "fresh cancellation" $ \repositoryPath repository resolved document _ -> do
    let database = repositoryPath </> ".adrai" </> "provenance.sqlite"
    before <- doesFileExist database
    assertBool "fixture begins without an overlay" (not before)
    beforeCancellation <- snapshotRepositoryState repositoryPath (documentManagedPath document)
    cancelled <- try @SomeException $
      hydratePlacementEvidenceAtWith repository resolved testConfig [document] (throwIO ThreadKilled)
    case cancelled of
      Left exception -> fromException exception @?= Just ThreadKilled
      Right result -> assertFailure ("writer cancellation was converted to " <> show result)
    after <- doesFileExist database
    assertBool "writer cancellation removes only the fresh overlay" (not after)
    afterCancellation <- snapshotRepositoryState repositoryPath (documentManagedPath document)
    assertRepositoryStatePreserved beforeCancellation afterCancellation
    retry <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    case retry of
      Left problem -> assertFailure ("fresh-overlay retry failed: " <> show problem)
      Right _ -> pure ()

warmValidationCancellationTest :: IO ()
warmValidationCancellationTest =
  withFixture "warm validation cancellation" $ \repositoryPath repository resolved document _ -> do
    initial <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    case initial of
      Left problem -> assertFailure (show problem)
      Right _ -> pure ()
    before <- snapshotRepositoryState repositoryPath (documentManagedPath document)
    cancelled <- try @SomeException $
      hydratePlacementEvidenceAtWithHooks repository resolved testConfig [document]
        (throwIO ThreadKilled)
        (pure ())
    case cancelled of
      Left exception -> fromException exception @?= Just ThreadKilled
      Right result -> assertFailure ("validation cancellation was converted to " <> show result)
    after <- snapshotRepositoryState repositoryPath (documentManagedPath document)
    assertRepositoryStatePreserved before after
    retry <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    retry @?= initial

contendedLockCancellationTest :: IO ()
contendedLockCancellationTest =
  withFixture "contended lock cancellation" $ \repositoryPath repository resolved document _ -> do
    initial <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    case initial of
      Left problem -> assertFailure (show problem)
      Right _ -> pure ()
    before <- snapshotRepositoryState repositoryPath (documentManagedPath document)
    let cacheDirectory = repositoryPath </> ".adrai"
        acquireRequired = do
          acquired <- acquireOverlayLock cacheDirectory
          case acquired of
            Nothing -> assertFailure "failed to acquire fixture overlay lock" >> fail "unreachable"
            Just lock -> pure lock
    bracket acquireRequired releaseOverlayLock $ \_ -> do
      completed <- newEmptyMVar
      waiter <- forkIO $ do
        outcome <- try @SomeException (hydratePlacementEvidenceAt repository resolved testConfig [document])
        putMVar completed outcome
      threadDelay 250000
      throwTo waiter ThreadKilled
      outcome <- takeMVar completed
      case outcome of
        Left exception -> fromException exception @?= Just ThreadKilled
        Right result -> assertFailure ("contended cancellation was converted to " <> show result)
    after <- snapshotRepositoryState repositoryPath (documentManagedPath document)
    assertRepositoryStatePreserved before after
    retry <- hydratePlacementEvidenceAt repository resolved testConfig [document]
    retry @?= initial

withFixture
  :: String
  -> (FilePath -> Repository -> ResolvedRepositoryRevision -> ParsedManagedDocument -> OperationId -> IO ())
  -> IO ()
withFixture label action =
  withSystemTempDirectory ("adrai provenance read " <> label) $ \temporary -> do
    let repositoryPath = temporary </> "repo with spaces ünicode"
        managedPath = "architecture/adrai/decisions/000/ü spaced.decision.md"
    initTestRepository repositoryPath
    _ <- commitFile repositoryPath ".gitignore" ".adrai/\n"
    basisText <- commitFile repositoryPath "seed.txt" "seed\n"
    basis <- requireOid basisText
    let operation = requireOperation "O00000000000000000000000091"
        adr = requireAdr "A00000000000000000000000091"
        semantic = "# Unicode provenance\n"
        capsule = makeCapsule operation adr basis semantic
        documentBytes = TextEncoding.encodeUtf8 (sealSemantic semantic capsule)
    targetText <- commitFile repositoryPath managedPath documentBytes
    blobText <- outputText <$> gitSuccess repositoryPath ["rev-parse", "HEAD:" <> managedPath] ""
    blob <- requireOid blobText
    resolved <- requireResolved repositoryPath targetText
    let document = ParsedManagedDocument
          { parsedDocumentObjectRef = "A00000000000000000000000091",
            parsedManagedPath = requireRepoPath (Text.pack managedPath),
            parsedManagedCapsule = capsule,
            parsedBlobOid = Just blob,
            parsedSemanticHash = Text.pack (show (semanticDigest semantic))
          }
    action repositoryPath (repositoryOf resolved) resolved document operation

tamper :: FilePath -> Text.Text -> [SQLData] -> IO ()
tamper database statement parameters = do
  connection <- open database
  execute connection (asQuery statement) parameters
  close connection

repositoryOf :: ResolvedRepositoryRevision -> Repository
repositoryOf = resolvedRepository

requireResolved :: FilePath -> Text.Text -> IO ResolvedRepositoryRevision
requireResolved path revision = do
  discovered <- discoverRepository systemGit path
  repository <- case discovered of
    Left problem -> assertFailure (show problem) >> fail "unreachable"
    Right value -> pure value
  resolved <- resolveRepositoryRevision repository (requireRevision revision)
  case resolved of
    Left problem -> assertFailure (show problem) >> fail "unreachable"
    Right value -> pure value

makeCapsule :: OperationId -> AdrId -> GitOid -> Text.Text -> ProvenanceCapsule
makeCapsule operation adr basis semantic =
  case mkProvenanceCapsule
    ProvenanceCapsuleInput
      { capsuleInputOperationId = operation,
        capsuleInputObjectId = ProvenanceAdr adr,
        capsuleInputEventKind = requireEventKind,
        capsuleInputActor = requireActor,
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

requireActor :: Actor
requireActor = case mkActor HumanActor "tester" Nothing of
  Left problem -> error (show problem)
  Right value -> value

requireEventKind :: EventKind
requireEventKind = case mkEventKind "decision" of
  Left problem -> error (show problem)
  Right value -> value

requireOperation :: Text.Text -> OperationId
requireOperation value = case mkOperationId value of
  Left problem -> error (show problem)
  Right operation -> operation

requireAdr :: Text.Text -> AdrId
requireAdr value = case mkAdrId value of
  Left problem -> error (show problem)
  Right adr -> adr

requireOid :: Text.Text -> IO GitOid
requireOid value = case mkGitOid value of
  Left problem -> assertFailure (show problem) >> fail "unreachable"
  Right oid -> pure oid

testConfig :: Config
testConfig = case mkManagedPaths (requireRepoPath "architecture/adrai/decisions") (requireRepoPath "architecture/adrai/connections") of
  Left problem -> error (show problem)
  Right paths -> case mkConfig ConfigSchemaV1 paths [LogicalLine "trunk" [GitRef "refs/heads/main"]] of
    Left problem -> error (show problem)
    Right config -> config

releaseConfig :: Config
releaseConfig = case mkManagedPaths (requireRepoPath "architecture/adrai/decisions") (requireRepoPath "architecture/adrai/connections") of
  Left problem -> error (show problem)
  Right paths -> case mkConfig ConfigSchemaV1 paths [LogicalLine "release" [GitRef "refs/heads/main"]] of
    Left problem -> error (show problem)
    Right config -> config
