{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for exact-cache query hooks and archive integrity.
module Adrai.QueryIntegrationTest (tests) where

import Adrai.Compiler.CacheSelection (exactCacheArchivePath)
import Adrai.Domain (mkDomain)
import Adrai.Format.Document
  ( AppliesToPayload (..),
    ConnectionPayload (..),
    ConnectionRecord (..),
    DecisionRecord (..),
    DomainsPayload (..),
    ManagedRecord (..),
    StatusPayload (..),
    StatusState (..),
    canonicalManagedPath,
    renderManagedSemantic,
    sealManagedDocument,
  )
import Adrai.Git (Repository, RevisionSpec (..), discoverRepository, gitOidText, systemGit)
import Adrai.Integration.CLI hiding (parseCompareResults, parseHistory, parseSearchResults)
import Adrai.Compiler (materializeCurrentSearch)
import Adrai.History (CommitPlacementEvidence (..), LineLandingEvidence (..), PlacementEvidence (..), ReadSnapshot (..), RevisionIdentity (..))
import Adrai.Query (RelevantProjection (..), RelevantRequest (..), SearchProjection (..), defaultRelevantRequest, defaultSearchRequest)
import Adrai.Provenance
  ( GitOid,
    ProvenanceCapsuleInput (..),
    ProvenanceObjectId (..),
    mkEventKind,
    mkGitOid,
    mkProvenanceCapsule,
    semanticDigest,
  )
import Adrai.Provenance.Ensure (openReadWriteExisting)
import Adrai.Provenance.Overlay (provenanceDatabasePath)
import Adrai.RetainedCache.RepositorySeed
  ( RepositorySeed,
    createRepositorySeed,
    removeRepositorySeed,
    withPrivateRepositorySeed,
  )
import Adrai.Repository (resolveRepositoryRevision, resolvedCommitOid)
import Adrai.Retrieval (SearchDocument (..), SearchMaterialization (..))
import Adrai.SearchVectorCorpus (buildSearchVectorCorpus)
import Adrai.Scope (mkScopePattern)
import Adrai.Service.Query
  ( ExactQueryContext (..),
    QueryExecutionHooks (..),
    RelevantFailure (..),
    SearchFailure,
    SearchServiceRequest (..),
    loadExactQueryContextForTest,
    loadExactQueryContextWithAcquisitionHooksForTest,
    readSnapshotAt,
    runRelevantQueryWithHooks,
    runSearchWithHooks,
  )
import Adrai.Types
  ( ActorKind (HumanActor),
    Actor,
    AdrId,
    ConnectionId,
    OperationId,
    ProvenanceInputs (..),
    RevisionSelector (..),
    adrIdText,
    connectionIdText,
    configManagedPaths,
    defaultConfig,
    mkActor,
    mkAdrId,
    mkConnectionId,
    mkOperationId,
    mkRecordId,
    mkRepoPath,
    operationIdText,
    recordIdText,
    repoPathText,
  )
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, throwTo)
import qualified Control.Concurrent.Async as Async
import Control.Exception (AsyncException (ThreadKilled), SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (find, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text, strip)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (UTCTime)
import System.Directory (createDirectoryIfMissing, doesFileExist, getFileSize, getModificationTime, removeFile, renameFile)
import Database.SQLite.Simple (Connection, Only (..), close, execute_, open, query, query_)
import System.FilePath (takeDirectory, (</>))
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

cacheSidecarMetadata :: FilePath -> IO [(FilePath, Maybe (Integer, UTCTime))]
cacheSidecarMetadata archive =
  traverse snapshot [archive <> "-journal", archive <> "-wal", archive <> "-shm"]
  where
    snapshot path = do
      exists <- doesFileExist path
      if exists
        then do
          size <- getFileSize path
          modified <- getModificationTime path
          pure (path, Just (size, modified))
        else pure (path, Nothing)

-- | Get the HEAD commit hash of a repository.
headCommit :: FilePath -> IO Text
headCommit repo =
  gitStdout repo ["rev-parse", "HEAD"]
    >>= \h -> pure (strip (decodeUtf8 (LBS.toStrict h)))

-- ---------------------------------------------------------------------------
-- Test suite
-- ---------------------------------------------------------------------------

tests :: TestTree
tests =
  withResource createExactQuerySeed removeRepositorySeed $ \getSeed ->
    testGroup "Query integration (history / search / compare)"
      [ testExactCacheQueryHooks getSeed,
        testExactCacheAcquisitionCancellation getSeed
      ]

data ExactQuerySeed = ExactQuerySeed
  { exactQuerySeedDecisionPath :: Text,
    exactQuerySeedScopeOperation :: Text,
    exactQuerySeedSourceRelativePath :: FilePath,
    exactQuerySeedMainCommit :: Text,
    exactQuerySeedFeatureCommit :: Text
  }

data ExactQueryCreateFixture = ExactQueryCreateFixture
  { exactQueryFixtureAdr :: AdrId,
    exactQueryFixtureInitialScope :: ConnectionId,
    exactQueryFixtureExpandedScope :: ConnectionId,
    exactQueryFixtureScopeOperation :: OperationId,
    exactQueryFixtureDecisionPath :: FilePath,
    exactQueryFixtureCreateObjects :: [Text],
    exactQueryFixtureFiles :: [(FilePath, BS.ByteString)]
  }

requireRight :: Show problem => String -> Either problem value -> IO value
requireRight label = either (\problem -> assertFailure (label <> ": " <> show problem) >> fail "unreachable") pure

fixtureRight :: Show problem => Text -> Either problem value -> Either Text value
fixtureRight label = first (\problem -> label <> ": " <> T.pack (show problem))

fixtureId :: Char -> Char -> Text
fixtureId prefix suffix = T.singleton prefix <> T.replicate 25 "0" <> T.singleton suffix

fixtureObject :: ManagedRecord -> ProvenanceObjectId
fixtureObject managed =
  case managed of
    ManagedDecision decision -> ProvenanceRecord (decisionRecord decision)
    ManagedConnection connection -> ProvenanceConnection (connectionRecordId connection)

sealFixtureMember :: Actor -> GitOid -> OperationId -> ManagedRecord -> Text -> [ProvenanceObjectId] -> Either Text (FilePath, BS.ByteString)
sealFixtureMember actor basis operation managed eventText parents = do
  semantic <- fixtureRight "render exact-query fixture member" (renderManagedSemantic managed)
  event <- fixtureRight "create exact-query fixture event" (mkEventKind eventText)
  capsule <-
    fixtureRight "create exact-query fixture capsule" . mkProvenanceCapsule $
      ProvenanceCapsuleInput
        { capsuleInputOperationId = operation,
          capsuleInputObjectId = fixtureObject managed,
          capsuleInputEventKind = event,
          capsuleInputActor = actor,
          capsuleInputTimestampMs = 1700000000000,
          capsuleInputBasis = basis,
          capsuleInputParents = parents,
          capsuleInputBranchHint = Just "main",
          capsuleInputUpstreamHint = Nothing,
          capsuleInputLineAnchors = [],
          capsuleInputSemanticDigest = semanticDigest semantic,
          capsuleInputToolVersion = "adrai/1.0.0",
          capsuleInputDigests = ProvenanceInputs Nothing Nothing Nothing
        }
  bytes <- fixtureRight "seal exact-query fixture member" (sealManagedDocument managed capsule)
  path <- fixtureRight "resolve exact-query fixture path" (canonicalManagedPath (configManagedPaths defaultConfig) managed)
  pure (T.unpack (repoPathText path), bytes)

buildExactQueryCreateFixture :: GitOid -> Either Text ExactQueryCreateFixture
buildExactQueryCreateFixture basis = do
  adr <- fixtureRight "create exact-query ADR" (mkAdrId (fixtureId 'A' '1'))
  record <- fixtureRight "create exact-query record" (mkRecordId (fixtureId 'R' '1'))
  initialScope <- fixtureRight "create exact-query initial scope connection" (mkConnectionId (fixtureId 'C' '1'))
  domainConnection <- fixtureRight "create exact-query domain connection" (mkConnectionId (fixtureId 'C' '2'))
  statusConnection <- fixtureRight "create exact-query status connection" (mkConnectionId (fixtureId 'C' '3'))
  expandedScope <- fixtureRight "create exact-query expanded scope connection" (mkConnectionId (fixtureId 'C' '4'))
  createOperation <- fixtureRight "create exact-query operation" (mkOperationId (fixtureId 'O' '1'))
  scopeOperation <- fixtureRight "create exact-query scope operation" (mkOperationId (fixtureId 'O' '2'))
  actor <- fixtureRight "create exact-query actor" (mkActor HumanActor "query-fixture" Nothing)
  domains <- traverse (fixtureRight "create exact-query domain" . mkDomain) ["cache", "platform.api", "unicode"]
  scopes <- traverse (fixtureRight "create exact-query scope" . mkScopePattern) ["src/cache/**", "src/api/**"]
  let decision =
        ManagedDecision
          DecisionRecord
            { decisionAdr = adr,
              decisionRecord = record,
              decisionTitle = "RFC-HTTP/2 OAuth2 København cache",
              decisionSummary = "Unicode punctuation and API_ACRONYM exact archive query context",
              decisionDomains = domains,
              decisionBody = "## Decision\nUse RFC-HTTP/2 OAuth2 tokens for København café clients. 日本語 🧭\n"
            }
      scope =
        ManagedConnection
          ConnectionRecord
            { connectionRecordId = initialScope,
              connectionPayload = AppliesToConnection (AppliesToPayload adr [] "initial" (sort scopes) [] (sort scopes)),
              connectionRationale = "Initial exact-query scope.\n"
            }
      domain =
        ManagedConnection
          ConnectionRecord
            { connectionRecordId = domainConnection,
              connectionPayload = DomainsConnection (DomainsPayload adr [] "initial" domains [] domains []),
              connectionRationale = "Initial exact-query domains.\n"
            }
      status =
        ManagedConnection
          ConnectionRecord
            { connectionRecordId = statusConnection,
              connectionPayload = StatusConnection (StatusPayload adr [] StatusActive [record] Nothing),
              connectionRationale = "Initial active status.\n"
            }
      members =
        [ (decision, "decision.create", []),
          (scope, "scope.initial", [ProvenanceRecord record]),
          (domain, "domain.initial", [ProvenanceRecord record]),
          (status, "status.initial", [ProvenanceRecord record])
        ]
  files <- traverse (\(managed, event, parents) -> sealFixtureMember actor basis createOperation managed event parents) members
  decisionPath <-
    case find (T.isSuffixOf ".decision.md" . T.pack . fst) files of
      Nothing -> Left "exact-query fixture has no managed decision path"
      Just (path, _) -> Right path
  pure
    ExactQueryCreateFixture
      { exactQueryFixtureAdr = adr,
        exactQueryFixtureInitialScope = initialScope,
        exactQueryFixtureExpandedScope = expandedScope,
        exactQueryFixtureScopeOperation = scopeOperation,
        exactQueryFixtureDecisionPath = decisionPath,
        exactQueryFixtureCreateObjects = recordIdText record : map connectionIdText [initialScope, domainConnection, statusConnection],
        exactQueryFixtureFiles = files
      }

buildExactQueryScopeFixture :: GitOid -> ExactQueryCreateFixture -> Either Text (FilePath, BS.ByteString)
buildExactQueryScopeFixture basis fixture = do
  actor <- fixtureRight "create exact-query scope actor" (mkActor HumanActor "query-fixture" Nothing)
  initialScopes <- traverse (fixtureRight "create exact-query initial scope" . mkScopePattern) ["src/cache/**", "src/api/**"]
  addedScopes <- traverse (fixtureRight "create exact-query expanded scope" . mkScopePattern) ["src/query-context/**", "src/feature-parity/**"]
  let managed =
        ManagedConnection
          ConnectionRecord
            { connectionRecordId = exactQueryFixtureExpandedScope fixture,
              connectionPayload =
                AppliesToConnection
                  (AppliesToPayload (exactQueryFixtureAdr fixture) [exactQueryFixtureInitialScope fixture] "expand" (sort addedScopes) [] (sort (initialScopes <> addedScopes))),
              connectionRationale = "Cover query context and branch placement sources.\n"
            }
  sealFixtureMember actor basis (exactQueryFixtureScopeOperation fixture) managed "scope.expand" [ProvenanceConnection (exactQueryFixtureInitialScope fixture)]

commitFixtureFiles :: FilePath -> [(FilePath, BS.ByteString)] -> String -> IO ()
commitFixtureFiles repository files message = do
  mapM_ writeFixtureFile files
  _ <- gitStdout repository ["add", "--all"]
  _ <- gitStdout repository ["commit", "-m", message]
  pure ()
  where
    writeFixtureFile (relativePath, bytes) = do
      createDirectoryIfMissing True (takeDirectory (repository </> relativePath))
      BS.writeFile (repository </> relativePath) bytes

createExactQuerySeed :: IO (RepositorySeed ExactQuerySeed)
createExactQuerySeed =
  createRepositorySeed $ \tmpDir -> do
    repoPath <- createTestRepo tmpDir
    commitFixtureFiles repoPath
      [ ( ".adrai.toml",
          "schema = 1\n\n[[line]]\nid = \"main\"\nrefs = [\"refs/heads/main\"]\n\n[[line]]\nid = \"feature\"\nrefs = [\"refs/heads/query-context-feature\"]\n"
        )
      ]
      "configure main and feature logical lines"
    createBasis <- requireRight "parse exact-query create basis" . mkGitOid =<< headCommit repoPath
    createFixture <- requireRight "build sealed exact-query create fixture" (buildExactQueryCreateFixture createBasis)
    let sourceRelativePath = "src/cache/query-context.txt"
        createMessage =
          T.unpack . T.unlines $
            [ "seed sealed exact-query create operation",
              "",
              "ADRAI-Op: " <> fixtureId 'O' '1',
              "ADRAI-ADR: " <> adrIdText (exactQueryFixtureAdr createFixture),
              "ADRAI-Objects: " <> T.intercalate "," (exactQueryFixtureCreateObjects createFixture)
            ]
    commitFixtureFiles
      repoPath
      (exactQueryFixtureFiles createFixture <> [(sourceRelativePath, encodeUtf8 "RFC-HTTP/2 OAuth2 København 日本語 🧭 API_ACRONYM exact cache relevance source\n")])
      createMessage
    branchPoint <- headCommit repoPath
    scopeBasis <- requireRight "parse exact-query scope basis" (mkGitOid branchPoint)
    scopeFile <- requireRight "build sealed exact-query scope fixture" (buildExactQueryScopeFixture scopeBasis createFixture)
    let scopeOperation = operationIdText (exactQueryFixtureScopeOperation createFixture)
        scopeMessage =
          T.unpack . T.unlines $
            [ "seed sealed exact-query scope operation",
              "",
              "ADRAI-Op: " <> scopeOperation,
              "ADRAI-ADR: " <> adrIdText (exactQueryFixtureAdr createFixture),
              "ADRAI-Objects: " <> connectionIdText (exactQueryFixtureExpandedScope createFixture)
            ]
    commitFixtureFiles repoPath [scopeFile] scopeMessage
    mainCommit <- headCommit repoPath
    _ <- gitStdout repoPath ["switch", "-c", "query-context-feature", T.unpack branchPoint]
    _ <- gitStdout repoPath ["commit", "--allow-empty", "-m", "feature-only parent for shared operation copy"]
    _ <- gitStdout repoPath ["cherry-pick", T.unpack mainCommit]
    featureCommit <- headCommit repoPath
    assertBool "cherry-picked feature placement has a distinct commit OID" (featureCommit /= mainCommit)
    _ <- adraiJsonOrThrow repoPath ["compile", "--json"]
    _ <- gitStdout repoPath ["switch", "main"]
    _ <- adraiJsonOrThrow repoPath ["compile", "--json"]
    pure
      ( repoPath,
        ExactQuerySeed
          { exactQuerySeedDecisionPath = T.pack (exactQueryFixtureDecisionPath createFixture),
            exactQuerySeedScopeOperation = scopeOperation,
            exactQuerySeedSourceRelativePath = sourceRelativePath,
            exactQuerySeedMainCommit = mainCommit,
            exactQuerySeedFeatureCommit = featureCommit
          }
      )

placementCommitOids :: ReadSnapshot -> [Text]
placementCommitOids snapshot =
  sortOn id
    [ commitPlacementOid placement
      | evidence <- Map.elems (readSnapshotPlacement snapshot),
        placement <- placementCommits evidence
    ]

landingCommitOids :: ReadSnapshot -> [Text]
landingCommitOids snapshot =
  sortOn id
    [ landingCommit landing
      | evidence <- Map.elems (readSnapshotPlacement snapshot),
        landing <- placementLineLandings evidence
    ]

evidenceForOperation :: Text -> ReadSnapshot -> [PlacementEvidence]
evidenceForOperation operation snapshot =
  [ evidence
    | (operationId, evidence) <- Map.toList (readSnapshotPlacement snapshot),
      operationIdText operationId == operation
  ]

assertExactBranchPlacementParity :: ExactQuerySeed -> FilePath -> Repository -> ExactQueryContext -> IO ()
assertExactBranchPlacementParity payload repoPath repository mainContext = do
  let sharedOperation = exactQuerySeedScopeOperation payload
      mainCommit = exactQuerySeedMainCommit payload
      featureCommit = exactQuerySeedFeatureCommit payload
      mainSnapshot = exactQuerySnapshot mainContext
  overlayConnection <- open (provenanceDatabasePath (repoPath </> ".adrai" </> "index.sqlite"))
  overlayPlacementOids <- query overlayConnection
    "SELECT commit_oid FROM operation_commit WHERE op_id=? ORDER BY commit_oid"
    (Only sharedOperation) :: IO [Only Text]
  overlayLandingOids <- query overlayConnection
    "SELECT commit_oid FROM line_landing WHERE op_id=? ORDER BY commit_oid"
    (Only sharedOperation) :: IO [Only Text]
  close overlayConnection
  assertBool "shared overlay retains main placement" (Only mainCommit `elem` overlayPlacementOids)
  assertBool "shared overlay retains feature placement" (Only featureCommit `elem` overlayPlacementOids)
  assertBool "shared overlay retains main landing" (Only mainCommit `elem` overlayLandingOids)
  assertBool "shared overlay retains feature landing" (Only featureCommit `elem` overlayLandingOids)
  assertBool "main exact snapshot retains placement rows" (not (null (placementCommitOids mainSnapshot)))
  assertBool "main exact snapshot retains landing rows" (not (null (landingCommitOids mainSnapshot)))
  case evidenceForOperation sharedOperation mainSnapshot of
    [mainEvidence] -> do
      sortOn id (map commitPlacementOid (placementCommits mainEvidence)) @?= [mainCommit]
      sortOn id (map landingCommit (placementLineLandings mainEvidence)) @?= [mainCommit]
      assertBool "main target retains the shared operation placement" (mainCommit `elem` map commitPlacementOid (placementCommits mainEvidence))
      assertBool "feature copy cannot win the preferred placement" (placementCommit mainEvidence /= Just featureCommit)
      assertBool "feature copy is absent from main placement ordering" (featureCommit `notElem` map commitPlacementOid (placementCommits mainEvidence))
      assertBool "feature copy is absent from main original ordering" (featureCommit `notElem` placementOriginalCommits mainEvidence)
      assertBool "feature copy is absent from main introduction ordering" (featureCommit `notElem` placementIntroductions mainEvidence)
      assertBool "feature copy is absent from main landing ordering" (featureCommit `notElem` map landingCommit (placementLineLandings mainEvidence))
    other -> assertFailure ("expected exactly one main shared-operation evidence row, got " <> show other)
  featureExact <- loadExactQueryContextForTest repository featureCommit
  case featureExact of
    Just context ->
      case evidenceForOperation sharedOperation (exactQuerySnapshot context) of
        [featureEvidence] -> do
          sortOn id (map commitPlacementOid (placementCommits featureEvidence)) @?= [featureCommit]
          sortOn id (map landingCommit (placementLineLandings featureEvidence)) @?= [featureCommit]
          assertBool "feature target contains its copy placement" (featureCommit `elem` map commitPlacementOid (placementCommits featureEvidence))
          assertBool "feature target contains its copy landing" (featureCommit `elem` map landingCommit (placementLineLandings featureEvidence))
          assertBool "main copy is absent from feature placement ordering" (mainCommit `notElem` map commitPlacementOid (placementCommits featureEvidence))
        other -> assertFailure ("expected exactly one feature shared-operation evidence row, got " <> show other)
    Nothing -> assertFailure "valid feature exact archive was not accepted for target-qualified context"

-- =====================================================================
-- Test 1: Full history returns all operations for an ADR
-- =====================================================================

testExactCacheQueryHooks :: IO (RepositorySeed ExactQuerySeed) -> TestTree
testExactCacheQueryHooks getSeed =
  testCase "P6-08Q exact-v3 archive hooks and tamper fallback" $ do
    seed <- getSeed
    withPrivateRepositorySeed seed $ \payload repoPath -> do
      let decisionPath = exactQuerySeedDecisionPath payload
          scopeOperation = exactQuerySeedScopeOperation payload
          sourceRelativePath = exactQuerySeedSourceRelativePath payload
          mainCommit = exactQuerySeedMainCommit payload
      discovered <- discoverRepository systemGit repoPath
      repository <- case discovered of
        Left problem -> assertFailure (show problem) >> fail "unreachable"
        Right value -> pure value
      resolved <- resolveRepositoryRevision repository (RevisionSpec "HEAD")
      revision <- case resolved of
        Left problem -> assertFailure (show problem) >> fail "unreachable"
        Right value -> pure value
      let resolvedOid = gitOidText (resolvedCommitOid revision)
      resolvedOid @?= mainCommit
      archive <- case exactCacheArchivePath repository (resolvedCommitOid revision) of
        Nothing -> assertFailure "compiled revision has no exact archive path" >> fail "unreachable"
        Just value -> pure value
      coldSnapshotResult <- readSnapshotAt repository "HEAD"
      exactContext <- loadExactQueryContextForTest repository "HEAD"
      case (coldSnapshotResult, exactContext) of
        (Right coldSnapshot, Just context) -> do
          coldMaterialization <- case materializeCurrentSearch coldSnapshot of
            Left problem -> assertFailure ("cold identifier materialization failed: " <> show problem) >> fail "unreachable"
            Right value -> pure value
          let exactMaterialization = exactQueryMaterialization context
              documents = searchMaterializationDocuments exactMaterialization
              identifierSources = map searchDocumentIdentifierSource documents
              identifiers = map searchDocumentIdentifiers documents
          exactQuerySnapshot context @?= coldSnapshot
          exactMaterialization @?= coldMaterialization
          case (buildSearchVectorCorpus exactMaterialization, buildSearchVectorCorpus coldMaterialization) of
            (Right exactCorpus, Right coldCorpus) -> exactCorpus @?= coldCorpus
            (Left problem, _) -> assertFailure ("exact identifier corpus failed: " <> show problem)
            (_, Left problem) -> assertFailure ("cold identifier corpus failed: " <> show problem)
          assertBool "raw identifier source retains Unicode" (any (T.isInfixOf "København") identifierSources)
          assertBool "raw identifier source retains Japanese text" (any (T.isInfixOf "日本語") identifierSources)
          assertBool "raw identifier source retains emoji" (any (T.isInfixOf "🧭") identifierSources)
          assertBool "raw identifier source retains punctuation" (any (T.isInfixOf "RFC-HTTP/2") identifierSources)
          assertBool "raw identifier source is newline-delimited" (any (T.isInfixOf "\n") identifierSources)
          assertBool "raw identifier source differs from normalized lexical identifiers" (identifierSources /= identifiers)
          assertExactBranchPlacementParity payload repoPath repository context
        (Left problem, _) -> assertFailure ("cold identifier snapshot failed: " <> show problem)
        (_, Nothing) -> assertFailure "valid identifier archive was not accepted"
      overlayConnection <- open (provenanceDatabasePath (repoPath </> ".adrai" </> "index.sqlite"))
      registeredAdrRows <- query overlayConnection
        "SELECT adr_id FROM registered_operation WHERE op_id=?"
        (Only scopeOperation) :: IO [Only (Maybe Text)]
      close overlayConnection
      registeredAdrRows @?= [Only Nothing]
      archiveConnection <- open archive
      scopeMemberKinds <- query archiveConnection
        "SELECT object_type FROM operation_member WHERE operation_id=? ORDER BY object_id"
        (Only scopeOperation) :: IO [Only Text]
      close archiveConnection
      assertBool "scope operation must persist a connection member" (Only "connection" `elem` scopeMemberKinds)
      assertBool "scope operation must not persist a decision member" (Only "decision" `notElem` scopeMemberKinds)
      beforeBytes <- BS.readFile archive
      beforeMtime <- getModificationTime archive
      beforeSidecars <- cacheSidecarMetadata archive
      let missingArchive = archive <> ".missing"
      missingOpen <- try (openReadWriteExisting missingArchive) :: IO (Either SomeException Connection)
      case missingOpen of
        Left _ -> pure ()
        Right connection -> close connection >> assertFailure "mode=rw opener unexpectedly created a missing archive"
      assertBool "mode=rw opener must not create a missing archive" . not =<< doesFileExist missingArchive
      counters <- newIORef ([] :: [Text])
      let count label = modifyIORef' counters (label :)
          hooks = QueryExecutionHooks
            { queryArchiveLoad = count "archive",
              queryArchiveRejected = \problem -> count ("rejected:" <> problem),
              queryColdFallback = count "fallback",
              queryRawObservation = count "raw",
              querySnapshotAnalysis = count "analyze",
              queryProvenanceHydration = count "hydrate",
              queryColdCompile = count "cold"
            }
          request = SearchServiceRequest "HEAD" (defaultSearchRequest "RFC-HTTP/2")
      hit <- runSearchWithHooks hooks repository request
      case hit of
        Left problem -> assertFailure (show problem)
        Right projection -> assertBool "exact identifier search returns a result" (not (null (searchProjectionResults projection)))
      readIORef counters >>= (@?= ["archive"])
      BS.readFile archive >>= (@?= beforeBytes)
      getModificationTime archive >>= (@?= beforeMtime)
      cacheSidecarMetadata archive >>= (@?= beforeSidecars)

      oidCounters <- newIORef ([] :: [Text])
      let oidHooks = QueryExecutionHooks
            { queryArchiveLoad = modifyIORef' oidCounters ("archive" :),
              queryArchiveRejected = \problem -> modifyIORef' oidCounters (("rejected:" <> problem) :),
              queryColdFallback = modifyIORef' oidCounters ("fallback" :),
              queryRawObservation = modifyIORef' oidCounters ("raw" :),
              querySnapshotAnalysis = modifyIORef' oidCounters ("analyze" :),
              queryProvenanceHydration = modifyIORef' oidCounters ("hydrate" :),
              queryColdCompile = modifyIORef' oidCounters ("cold" :)
            }
      oidHit <- runSearchWithHooks oidHooks repository (SearchServiceRequest resolvedOid (defaultSearchRequest "RFC-HTTP/2"))
      case (hit, oidHit) of
        (Right headProjection, Right oidProjection) -> do
          searchProjectionResults oidProjection @?= searchProjectionResults headProjection
          searchProjectionRevision headProjection @?= RevisionIdentity "HEAD" resolvedOid
          searchProjectionRevision oidProjection @?= RevisionIdentity resolvedOid resolvedOid
        _ -> assertFailure "HEAD or explicit-OID exact search did not produce a projection"
      readIORef oidCounters >>= (@?= ["archive"])

      relevantPath <- case mkRepoPath (T.pack sourceRelativePath) of
        Left problem -> assertFailure (show problem) >> fail "unreachable"
        Right value -> pure value
      relevantCounters <- newIORef ([] :: [Text])
      let relevantHooks = QueryExecutionHooks
            { queryArchiveLoad = modifyIORef' relevantCounters ("archive" :),
              queryArchiveRejected = \problem -> modifyIORef' relevantCounters (("rejected:" <> problem) :),
              queryColdFallback = modifyIORef' relevantCounters ("fallback" :),
              queryRawObservation = modifyIORef' relevantCounters ("raw" :),
              querySnapshotAnalysis = modifyIORef' relevantCounters ("analyze" :),
              queryProvenanceHydration = modifyIORef' relevantCounters ("hydrate" :),
              queryColdCompile = modifyIORef' relevantCounters ("cold" :)
            }
      relevantHit <- runRelevantQueryWithHooks relevantHooks repository (defaultRelevantRequest relevantPath)
      case relevantHit of
        Left problem -> assertFailure (show problem)
        Right projection -> assertBool "exact identifier relevant returns a result" (not (null (relevantProjectionResults projection)))
      readIORef relevantCounters >>= (@?= ["archive"])
      BS.readFile archive >>= (@?= beforeBytes)
      getModificationTime archive >>= (@?= beforeMtime)
      cacheSidecarMetadata archive >>= (@?= beforeSidecars)

      relevantOidCounters <- newIORef ([] :: [Text])
      let relevantOidHooks = QueryExecutionHooks
            { queryArchiveLoad = modifyIORef' relevantOidCounters ("archive" :),
              queryArchiveRejected = \problem -> modifyIORef' relevantOidCounters (("rejected:" <> problem) :),
              queryColdFallback = modifyIORef' relevantOidCounters ("fallback" :),
              queryRawObservation = modifyIORef' relevantOidCounters ("raw" :),
              querySnapshotAnalysis = modifyIORef' relevantOidCounters ("analyze" :),
              queryProvenanceHydration = modifyIORef' relevantOidCounters ("hydrate" :),
              queryColdCompile = modifyIORef' relevantOidCounters ("cold" :)
            }
          relevantOidRequest = (defaultRelevantRequest relevantPath) {relevantRequestRevision = AtRevision resolvedOid}
      relevantOidHit <- runRelevantQueryWithHooks relevantOidHooks repository relevantOidRequest
      case (relevantHit, relevantOidHit) of
        (Right headProjection, Right oidProjection) -> do
          relevantProjectionResults oidProjection @?= relevantProjectionResults headProjection
          relevantProjectionRevision headProjection @?= RevisionIdentity "HEAD" resolvedOid
          relevantProjectionRevision oidProjection @?= RevisionIdentity resolvedOid resolvedOid
        _ -> assertFailure "HEAD or explicit-OID exact relevant query did not produce a projection"
      readIORef relevantOidCounters >>= (@?= ["archive"])

      missingRelevantPath <- case mkRepoPath "src/cache/missing-query-context.txt" of
        Left problem -> assertFailure (show problem) >> fail "unreachable"
        Right value -> pure value
      missingSourceCounters <- newIORef ([] :: [Text])
      let missingSourceHooks = QueryExecutionHooks
            { queryArchiveLoad = modifyIORef' missingSourceCounters ("archive" :),
              queryArchiveRejected = \problem -> modifyIORef' missingSourceCounters (("rejected:" <> problem) :),
              queryColdFallback = modifyIORef' missingSourceCounters ("fallback" :),
              queryRawObservation = modifyIORef' missingSourceCounters ("raw" :),
              querySnapshotAnalysis = modifyIORef' missingSourceCounters ("analyze" :),
              queryProvenanceHydration = modifyIORef' missingSourceCounters ("hydrate" :),
              queryColdCompile = modifyIORef' missingSourceCounters ("cold" :)
            }
      missingSource <- runRelevantQueryWithHooks missingSourceHooks repository (defaultRelevantRequest missingRelevantPath)
      case missingSource of
        Left (RelevantSourceFailure _) -> pure ()
        other -> assertFailure ("valid exact archive did not preserve requested-source failure: " <> show other)
      readIORef missingSourceCounters >>= (@?= ["archive"])

      let throwingExactHooks exception = QueryExecutionHooks
            { queryArchiveLoad = throwIO exception,
              queryArchiveRejected = \_ -> pure (),
              queryColdFallback = pure (),
              queryRawObservation = pure (),
              querySnapshotAnalysis = pure (),
              queryProvenanceHydration = pure (),
              queryColdCompile = pure ()
            }
      synchronousExact <- try (runSearchWithHooks (throwingExactHooks (userError "query archive hook")) repository request) :: IO (Either SomeException (Either SearchFailure SearchProjection))
      case synchronousExact of
        Left _ -> pure ()
        Right _ -> assertFailure "synchronous archive hook exception was converted into a query result or fallback"
      asynchronousExact <- try (runSearchWithHooks (throwingExactHooks ThreadKilled) repository request) :: IO (Either SomeAsyncException (Either SearchFailure SearchProjection))
      case asynchronousExact of
        Left _ -> pure ()
        Right _ -> assertFailure "asynchronous archive hook exception was swallowed or converted into a fallback"

      let assertArchiveFallback label mutate = do
            BS.writeFile archive beforeBytes
            _ <- mutate archive
            fallbackCounters <- newIORef ([] :: [Text])
            let fallbackHooks = QueryExecutionHooks
                  { queryArchiveLoad = modifyIORef' fallbackCounters ("archive" :),
                    queryArchiveRejected = \problem -> modifyIORef' fallbackCounters (("rejected:" <> problem) :),
                    queryColdFallback = modifyIORef' fallbackCounters ("fallback" :),
                    queryRawObservation = modifyIORef' fallbackCounters ("raw" :),
                    querySnapshotAnalysis = modifyIORef' fallbackCounters ("analyze" :),
                    queryProvenanceHydration = modifyIORef' fallbackCounters ("hydrate" :),
                    queryColdCompile = modifyIORef' fallbackCounters ("cold" :)
                  }
            fallback <- runSearchWithHooks fallbackHooks repository request
            fallback @?= hit
            events <- readIORef fallbackCounters
            assertBool (T.unpack label <> " is rejected before the archive-load callback") ("archive" `notElem` events)
            events @?=
              [ "cold",
                "hydrate",
                "analyze",
                "raw",
                "fallback",
                "rejected:exact archive publication contract or metadata rejected"
              ]
            BS.writeFile archive beforeBytes
            pure fallbackHooks
          assertRelevantArchiveFallback label mutate = do
            BS.writeFile archive beforeBytes
            _ <- mutate archive
            relevantFallbackCounters <- newIORef ([] :: [Text])
            let relevantFallbackHooks = QueryExecutionHooks
                  { queryArchiveLoad = modifyIORef' relevantFallbackCounters ("archive" :),
                    queryArchiveRejected = \problem -> modifyIORef' relevantFallbackCounters (("rejected:" <> problem) :),
                    queryColdFallback = modifyIORef' relevantFallbackCounters ("fallback" :),
                    queryRawObservation = modifyIORef' relevantFallbackCounters ("raw" :),
                    querySnapshotAnalysis = modifyIORef' relevantFallbackCounters ("analyze" :),
                    queryProvenanceHydration = modifyIORef' relevantFallbackCounters ("hydrate" :),
                    queryColdCompile = modifyIORef' relevantFallbackCounters ("cold" :)
                  }
            relevantFallback <- runRelevantQueryWithHooks relevantFallbackHooks repository (defaultRelevantRequest relevantPath)
            relevantFallback @?= relevantHit
            events <- readIORef relevantFallbackCounters
            case events of
              ["cold", "hydrate", "analyze", "raw", "fallback"] -> pure ()
              other -> assertFailure (T.unpack label <> " relevant fallback hook order/count mismatch: " <> show other)
            BS.writeFile archive beforeBytes
          assertArchiveRejected label mutate = do
            BS.writeFile archive beforeBytes
            _ <- mutate archive
            rejected <- loadExactQueryContextForTest repository "HEAD"
            case rejected of
              Nothing -> pure ()
              Just _ -> assertFailure (T.unpack label <> " archive was accepted")
            BS.writeFile archive beforeBytes
          mutateArchive action path = do
            connection <- open path
            _ <- action connection
            close connection
      _ <- assertRelevantArchiveFallback "missing exact archive" $ \path ->
        renameFile path missingArchive
      removeFile missingArchive
      _ <- assertArchiveRejected "wrong revision metadata" $ mutateArchive $ \connection ->
        execute_ connection "UPDATE meta SET value='0000000000000000000000000000000000000000' WHERE key='requested_revision'"
      _ <- assertArchiveRejected "v1 archive label" $ mutateArchive $ \connection ->
        execute_ connection "UPDATE meta SET value='adrai-cache/1' WHERE key='schema'"
      _ <- assertArchiveRejected "v2 archive label" $ mutateArchive $ \connection ->
        execute_ connection "UPDATE meta SET value='adrai-cache/2' WHERE key='schema'"
      _ <- assertArchiveRejected "missing required metadata" $ mutateArchive $ \connection ->
        execute_ connection "DELETE FROM meta WHERE key='requested_revision'"
      _ <- assertArchiveRejected "missing required materialization row" $ mutateArchive $ \connection ->
        execute_ connection "DELETE FROM search_document"
      _ <- assertArchiveRejected "invalid materialization fingerprint" $ mutateArchive $ \connection ->
        execute_ connection "UPDATE meta SET value='0000000000000000000000000000000000000000000000000000000000000000' WHERE key='materialization_fingerprint'"
      _ <- assertArchiveRejected "forged v3 old DDL" $ mutateArchive $ \connection ->
        execute_ connection "ALTER TABLE search_document RENAME COLUMN identifier_source TO identifier_source_v2"
      _ <- assertArchiveRejected "missing provenance line configuration" $ mutateArchive $ \connection ->
        execute_ connection "DELETE FROM line_config"
      _ <- assertArchiveRejected "foreign-key-invalid operation member" $ mutateArchive $ \connection -> do
        execute_ connection "PRAGMA foreign_keys=OFF"
        execute_ connection "UPDATE operation_member SET operation_id='O00000000000000000000000000' WHERE rowid=(SELECT min(rowid) FROM operation_member)"
        foreignKeyRows <- query_ connection "PRAGMA foreign_key_check" :: IO [(Text, Int, Text, Int)]
        assertBool "operation_member orphan produces a SQLite foreign-key violation" (not (null foreignKeyRows))
      _ <- assertArchiveRejected "unknown operation placement row" $ mutateArchive $ \connection -> do
        placementRows <- query_ connection "SELECT count(*) FROM operation_commit" :: IO [Only Int]
        assertBool "fixture has an operation placement to corrupt" (placementRows /= [Only 0])
        execute_ connection "PRAGMA foreign_keys=OFF"
        execute_ connection "UPDATE operation_commit SET op_id='O00000000000000000000000000' WHERE rowid=(SELECT min(rowid) FROM operation_commit)"
      _ <- assertArchiveRejected "invalid state-token archive" $ mutateArchive $ \connection ->
        execute_ connection "UPDATE reduced_adr SET state_token='not-a-valid-state-token'"
      let corruptFtsShadow connection = do
            readableFtsRows <- query_ connection "SELECT count(*) FROM fts_search_exact" :: IO [Only Int]
            readableFtsRows @?= [Only 1]
            postingSegments <- query_ connection "SELECT count(*) FROM fts_search_exact_data WHERE id > 10" :: IO [Only Int]
            assertBool "FTS fixture has a non-reserved posting segment" (postingSegments /= [Only 0])
            execute_ connection "DELETE FROM fts_search_exact_data WHERE id=(SELECT max(id) FROM fts_search_exact_data WHERE id > 10)"
            stillReadableFtsRows <- query_ connection "SELECT count(*) FROM fts_search_exact" :: IO [Only Int]
            stillReadableFtsRows @?= [Only 1]
      fallbackHooks <- assertArchiveFallback "FTS shadow corruption" $ mutateArchive corruptFtsShadow

      connection <- open archive
      execute_ connection "DELETE FROM line_config"
      close connection
      let throwingHooks exception = fallbackHooks { queryColdCompile = throwIO exception }
      synchronous <- try (runSearchWithHooks (throwingHooks (userError "query cold hook")) repository request) :: IO (Either SomeException (Either SearchFailure SearchProjection))
      case synchronous of
        Left _ -> pure ()
        Right _ -> assertFailure "synchronous cold hook exception was converted into a query result"
      asynchronous <- try (runSearchWithHooks (throwingHooks ThreadKilled) repository request) :: IO (Either SomeAsyncException (Either SearchFailure SearchProjection))
      case asynchronous of
        Left _ -> pure ()
        Right _ -> assertFailure "asynchronous cold hook exception was swallowed or converted into a query result"
      BS.writeFile archive beforeBytes

      -- On a cache miss, snapshot integrity remains the first relevant-query
      -- failure even when the requested source is also absent.
      BS.writeFile (repoPath </> T.unpack decisionPath) "schema: deliberately-invalid\n"
      _ <- gitStdout repoPath ["add", "--", T.unpack decisionPath]
      _ <- gitStdout repoPath ["commit", "-m", "add invalid query context"]
      precedenceCounters <- newIORef ([] :: [Text])
      let precedenceHooks = QueryExecutionHooks
            { queryArchiveLoad = modifyIORef' precedenceCounters ("archive" :),
              queryArchiveRejected = \problem -> modifyIORef' precedenceCounters (("rejected:" <> problem) :),
              queryColdFallback = modifyIORef' precedenceCounters ("fallback" :),
              queryRawObservation = modifyIORef' precedenceCounters ("raw" :),
              querySnapshotAnalysis = modifyIORef' precedenceCounters ("analyze" :),
              queryProvenanceHydration = modifyIORef' precedenceCounters ("hydrate" :),
              queryColdCompile = modifyIORef' precedenceCounters ("cold" :)
            }
      precedence <- runRelevantQueryWithHooks precedenceHooks repository (defaultRelevantRequest missingRelevantPath)
      case precedence of
        Left (RelevantIntegrityFailure _) -> pure ()
        other -> assertFailure ("cache-miss relevant error precedence did not select snapshot failure: " <> show other)
      precedenceEvents <- readIORef precedenceCounters
      mapM_ (\event -> assertBool ("snapshot precedence runs " <> T.unpack event <> " once") (length (filter (== event) precedenceEvents) == 1)) ["fallback", "raw", "analyze"]
      assertBool "snapshot integrity gating prevents provenance hydration" (not ("hydrate" `elem` precedenceEvents))
      assertBool "snapshot failure prevents cold compilation" (not ("cold" `elem` precedenceEvents))

testExactCacheAcquisitionCancellation :: IO (RepositorySeed ExactQuerySeed) -> TestTree
testExactCacheAcquisitionCancellation getSeed =
  testCase "P6-08Q exact archive acquisition owns cancellation cleanup" $ do
    seed <- getSeed
    withPrivateRepositorySeed seed $ \_ repoPath -> do
      discovered <- discoverRepository systemGit repoPath
      repository <- case discovered of
        Left problem -> assertFailure (show problem) >> fail "unreachable"
        Right value -> pure value
      resolved <- resolveRepositoryRevision repository (RevisionSpec "HEAD")
      revision <- case resolved of
        Left problem -> assertFailure (show problem) >> fail "unreachable"
        Right value -> pure value
      archive <- case exactCacheArchivePath repository (resolvedCommitOid revision) of
        Nothing -> assertFailure "compiled revision has no exact archive path" >> fail "unreachable"
        Just value -> pure value

      successfulAcquisitions <- newIORef (0 :: Int)
      successfulCloses <- newIORef (0 :: Int)
      successfulContext <-
        loadExactQueryContextWithAcquisitionHooksForTest
          repository
          "HEAD"
          (modifyIORef' successfulAcquisitions (+ 1))
          (modifyIORef' successfulCloses (+ 1))
      case successfulContext of
        Nothing -> assertFailure "valid exact archive did not load through the acquisition seam"
        Just _ -> pure ()
      readIORef successfulAcquisitions >>= (@?= 1)
      readIORef successfulCloses >>= (@?= 1)

      acquired <- newEmptyMVar
      neverRelease <- newEmptyMVar
      cancellationCloses <- newIORef (0 :: Int)
      acquisitionWorker <- Async.async $
        loadExactQueryContextWithAcquisitionHooksForTest
          repository
          "HEAD"
          (putMVar acquired () >> takeMVar neverRelease)
          (modifyIORef' cancellationCloses (+ 1))
      takeMVar acquired
      throwTo (Async.asyncThreadId acquisitionWorker) ThreadKilled
      cancelled <- Async.waitCatch acquisitionWorker
      case cancelled of
        Left exception ->
          (fromException exception :: Maybe AsyncException) @?= Just ThreadKilled
        Right _ -> assertFailure "acquisition handoff cancellation was swallowed"
      readIORef cancellationCloses >>= (@?= 1)

      let releasedArchive = archive <> ".released"
      renameFile archive releasedArchive
      reopened <- openReadWriteExisting releasedArchive
      close reopened
      failedAcquisitions <- newIORef (0 :: Int)
      failedCloses <- newIORef (0 :: Int)
      missingContext <-
        loadExactQueryContextWithAcquisitionHooksForTest
          repository
          "HEAD"
          (modifyIORef' failedAcquisitions (+ 1))
          (modifyIORef' failedCloses (+ 1))
      case missingContext of
        Nothing -> pure ()
        Just _ -> assertFailure "missing exact archive unexpectedly acquired a query context"
      readIORef failedAcquisitions >>= (@?= 0)
      readIORef failedCloses >>= (@?= 0)
      removeFile releasedArchive
      assertBool "released exact archive remains removable after cancellation" . not =<< doesFileExist releasedArchive
