{-# LANGUAGE OverloadedStrings #-}

module Adrai.RelevanceIntegrationTest (tests, resolvedRelevantProjection, outsideScopeRelevantProjection, conflictedRelevantProjection) where

import Adrai.Compiler (materializeCurrentSearch)
import Adrai.CompilerMaterializationTest (p303RationaleSnapshot)
import Adrai.CurrentSearchTest (p304ConflictSnapshot)
import Adrai.Format.Json (JsonValue (..))
import Adrai.History (ReadSnapshot (..), RevisionIdentity (..))
import Adrai.Property.Generators
  ( DomainSpec (..),
    IdentifierPool (..),
    ProjectionDagSpec (..),
    ProjectionFixture (..),
    ScopeSpec (..),
    materializeProjectionDag,
  )
import Adrai.Query
import Adrai.Relevance
import Adrai.Retrieval (SearchMaterialization)
import Adrai.Sqlite (initializeSearchSchema, replaceSearchMaterialization)
import Adrai.Types (RepoPath, RevisionSelector (..), mkRepoPath)
import Control.Exception (bracket)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.Text as Text
import Database.SQLite.Simple (Connection, close, open)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "P3-05 relevant projection"
    [ testCase "raw source retrieves obsolete ADR only under explicit visibility" obsoleteVisibilityContract,
      testCase "scope is bonus not filter or confidence source" scopeBonusContract,
      testCase "decision conflicts group once while retaining matched candidate evidence" conflictGroupingContract,
      testCase "source revision mode path binary and limit failures are typed" sourceValidationContract,
      testCase "lenient malformed source uses authoritative raw-byte validation" lenientBoundaryContract,
      testCase "stopword-only source skips passage FTS channel diagnostics" emptyTermsPassageContract,
      testCase "confidence is computed against all aggregates before the public limit" globalConfidenceContract,
      testCase "injected worktree source is HEAD-bound ephemeral and byte stable" worktreeContract,
      testCase "relevant projection is canonical finite and repeatable" repeatabilityContract
    ]

obsoleteVisibilityContract :: IO ()
obsoleteVisibilityContract = withRelevant p303RationaleSnapshot $ \connection materialization -> do
  hidden <- mustRelevant =<< runCurrent connection materialization (baseRequest {relevantRequestIncludeObsolete = False}) baseSource
  visible <- mustRelevant =<< runCurrent connection materialization baseRequest baseSource
  relevantProjectionResults hidden @?= []
  let result = onlyResult visible
  relevantResultTitle result @?= "Current Decision API (CDAPI)"
  relevantResultObsolete result @?= True
  assertBool "selected evidence retains line ranges and ADR chunks" (not (null (relevantResultEvidence result)))
  relevantRetrievalSearchSections (relevantProjectionRetrieval visible) @?= 7
  lookupNested "algorithm" (relevantRetrievalSections (relevantProjectionRetrieval visible)) @?= Just (JsonString "exact-bounded-section-scan")
  lookupNested "candidates" (relevantRetrievalSections (relevantProjectionRetrieval visible))
    @?= Just (JsonNumber (fromIntegral (relevantRetrievalExactRerankCandidates (relevantProjectionRetrieval visible))))

scopeBonusContract :: IO ()
scopeBonusContract = withRelevant p303RationaleSnapshot $ \connection materialization -> do
  exact <- mustRelevant =<< runCurrent connection materialization baseRequest baseSource
  let outsidePath = mustRepoPath "unrelated/file.txt"
      outsideRequest = baseRequest {relevantRequestFile = outsidePath}
      outsideSource = baseSource {relevantSourcePath = outsidePath}
  outside <- mustRelevant =<< runCurrent connection materialization outsideRequest outsideSource
  let exactResult = onlyResult exact
      outsideResult = onlyResult outside
  relevantResultScopeMatch exactResult @?= RelevanceScopeExact
  relevantResultScopeMatch outsideResult @?= RelevanceScopeNone
  relevantResultSemanticScore exactResult @?= relevantResultSemanticScore outsideResult
  relevantResultScopeBonus exactResult @?= 0.025
  relevantResultScopeBonus outsideResult @?= 0
  assertClose 0.025 (relevantResultScore exactResult - relevantResultScore outsideResult)
  let weakBytes = "unrelated graphics rendering shader texture widget interface animation layout"
      weakSource = RevisionRelevantSource basePath "rationale-resolved" "blob-weak" weakBytes
  weak <- mustRelevant =<< runCurrent connection materialization baseRequest weakSource
  relevantRetrievalEligibleAdrs (relevantProjectionRetrieval weak) @?= 1
  assertBool "scope alone never creates high confidence" (all ((/= HighConfidence) . relevantResultConfidence) (relevantProjectionResults weak))

conflictGroupingContract :: IO ()
conflictGroupingContract = withRelevant p304ConflictSnapshot $ \connection materialization -> do
  projection <- mustRelevant =<< runRelevant connection p304ConflictSnapshot materialization conflictRequest conflictSource
  length (relevantProjectionResults projection) @?= 1
  let result = onlyResult projection
  relevantResultTitle result @?= "[conflicted ADR]"
  relevantResultSummary result @?= "left summary; right summary"
  assertBool "matched head stays separate from the logical conflict row" (maybe False (Text.isInfixOf "@") (relevantResultMatchedCandidate result))
  assertBool "conflict requires resolution" (resolutionStateRequired (relevantResultResolution result))

sourceValidationContract :: IO ()
sourceValidationContract = withRelevant p303RationaleSnapshot $ \connection materialization -> do
  runCurrent connection materialization (baseRequest {relevantRequestLimit = 0}) baseSource
    >>= (@?= Left (RelevantInvalidLimit 0))
  runCurrent connection materialization (baseRequest {relevantRequestLimit = 101}) baseSource
    >>= (@?= Left (RelevantInvalidLimit 101))
  let wrongPath = mustRepoPath "wrong/file.txt"
  runCurrent connection materialization baseRequest (baseSource {relevantSourcePath = wrongPath})
    >>= (@?= Left (RelevantSourceMismatch "source path does not match the relevance request"))
  runCurrent connection materialization baseRequest (RevisionRelevantSource basePath "wrong-revision" "blob-rationale" sourceBytes)
    >>= (@?= Left (RelevantSourceMismatch "revision source does not match the compiled snapshot revision"))
  runCurrent connection materialization baseRequest (RevisionRelevantSource basePath "rationale-resolved" "" sourceBytes)
    >>= (@?= Left (RelevantSourceMismatch "revision source requires a blob identity"))
  runCurrent connection materialization baseRequest (baseSource {relevantSourceBytes = "text\0binary"})
    >>= (@?= Left (RelevantDecodeFailure (TextAppearsBinary "file src/current/cache.py" BinaryContainsNul)))
  runCurrent connection materialization baseRequest (WorktreeRelevantSource basePath "rationale-resolved" sourceBytes)
    >>= (@?= Left (RelevantSourceMismatch "historical revision requires a revision source"))
  short <- mustRelevant =<< runCurrent connection materialization baseRequest (baseSource {relevantSourceBytes = "tiny"})
  relevantProjectionResults short @?= []
  relevantFileQueryChunks (relevantProjectionFile short) @?= 0

lenientBoundaryContract :: IO ()
lenientBoundaryContract = withRelevant p303RationaleSnapshot $ \connection materialization -> do
  let bytes = ByteString.replicate 32 255
      source = RevisionRelevantSource basePath "rationale-resolved" "blob-malformed-boundary" bytes
  projection <- mustRelevant =<< runCurrent connection materialization baseRequest source
  relevantFileBytes (relevantProjectionFile projection) @?= ByteString.length bytes
  relevantFileChunks (relevantProjectionFile projection) @?= 0
  relevantProjectionResults projection @?= []

emptyTermsPassageContract :: IO ()
emptyTermsPassageContract = withRelevant p303RationaleSnapshot $ \connection materialization -> do
  let bytes = "the and this about our system architecture component implementation decision service value"
      source = RevisionRelevantSource basePath "rationale-resolved" "blob-stopwords" bytes
  projection <- mustRelevant =<< runCurrent connection materialization baseRequest source
  let diagnostics = relevantRetrievalPassageFts (relevantProjectionRetrieval projection)
  lookupNested "queries" diagnostics @?= Just (JsonNumber 1)
  lookupNested "channel_hits" diagnostics @?= Just (JsonObject [])

globalConfidenceContract :: IO ()
globalConfidenceContract = withRelevant multiAdrSnapshot $ \connection materialization -> do
  let path = mustRepoPath "notes/multi.txt"
      request =
        (defaultRelevantRequest path)
          { relevantRequestRevision = AtRevision "after",
            relevantRequestIncludeObsolete = True,
            relevantRequestLimit = 100
          }
      bytes = "cache lease token secondary summary decision architecture context consequences"
      source = RevisionRelevantSource path "after-resolved" "blob-multi" bytes
  complete <- mustRelevant =<< runRelevant connection multiAdrSnapshot materialization request source
  limited <- mustRelevant =<< runRelevant connection multiAdrSnapshot materialization (request {relevantRequestLimit = 1}) source
  assertBool "fixture yields a real competitor outside the public limit" (length (relevantProjectionResults complete) >= 2)
  case (relevantProjectionResults complete, relevantProjectionResults limited) of
    (completeTop : _, [limitedTop]) -> do
      relevantResultAdr limitedTop @?= relevantResultAdr completeTop
      relevantResultConfidence limitedTop @?= relevantResultConfidence completeTop
      relevantResultMargin limitedTop @?= relevantResultMargin completeTop
    _ -> assertFailure "expected at least two complete results and one limited result"

worktreeContract :: IO ()
worktreeContract = withRelevant headSnapshot $ \connection materialization -> do
  let request = baseRequest {relevantRequestRevision = WorkingRevision}
      source = WorktreeRelevantSource basePath "rationale-resolved" sourceBytes
  first <- mustRelevant =<< runRelevant connection headSnapshot materialization request source
  second <- mustRelevant =<< runRelevant connection headSnapshot materialization request source
  relevantFileSource (relevantProjectionFile first) @?= "worktree"
  relevantFileBlob (relevantProjectionFile first) @?= Nothing
  renderRelevantProjection first @?= renderRelevantProjection second
  assertBool "worktree JSON preserves the nullable blob member" ("\"blob\": null" `ByteStringChar8.isInfixOf` renderRelevantProjection first)

repeatabilityContract :: IO ()
repeatabilityContract = withRelevant p303RationaleSnapshot $ \connection materialization -> do
  first <- mustRelevant =<< runCurrent connection materialization baseRequest baseSource
  second <- mustRelevant =<< runCurrent connection materialization baseRequest baseSource
  renderRelevantProjection first @?= renderRelevantProjection second
  assertBool "public schema is frozen" ("\"schema\": \"adrai/relevant/v1\"" `ByteStringChar8.isInfixOf` renderRelevantProjection first)
  mapM_ assertFiniteResult (relevantProjectionResults first)

resolvedRelevantProjection :: IO RelevantProjection
resolvedRelevantProjection = withRelevant p303RationaleSnapshot $ \connection materialization -> mustRelevant =<< runCurrent connection materialization baseRequest baseSource

outsideScopeRelevantProjection :: IO RelevantProjection
outsideScopeRelevantProjection = withRelevant p303RationaleSnapshot $ \connection materialization -> do
  let path = mustRepoPath "unrelated/file.txt"
  mustRelevant =<< runCurrent connection materialization (baseRequest {relevantRequestFile = path}) (baseSource {relevantSourcePath = path})

conflictedRelevantProjection :: IO RelevantProjection
conflictedRelevantProjection = withRelevant p304ConflictSnapshot $ \connection materialization -> mustRelevant =<< runRelevant connection p304ConflictSnapshot materialization conflictRequest conflictSource

basePath :: RepoPath
basePath = mustRepoPath "src/current/cache.py"

baseRequest :: RelevantRequest
baseRequest =
  (defaultRelevantRequest basePath)
    { relevantRequestRevision = AtRevision "rationale",
      relevantRequestIncludeObsolete = True
    }

sourceBytes :: ByteStringChar8.ByteString
sourceBytes = "current decision api context first decision second consequence current domain cache lease token"

baseSource :: RelevantSource
baseSource = RevisionRelevantSource basePath "rationale-resolved" "blob-rationale" sourceBytes

conflictPath :: RepoPath
conflictPath = mustRepoPath "src/conflict.txt"

conflictRequest :: RelevantRequest
conflictRequest =
  (defaultRelevantRequest conflictPath)
    { relevantRequestRevision = AtRevision "p3-04-conflict"
    }

conflictSource :: RelevantSource
conflictSource = RevisionRelevantSource conflictPath "p3-04-conflict-resolved" "blob-conflict" "left decision summary right decision summary architectural choice"

headSnapshot :: ReadSnapshot
headSnapshot = p303RationaleSnapshot {readSnapshotRevision = RevisionIdentity "HEAD" "rationale-resolved"}

multiAdrSnapshot :: ReadSnapshot
multiAdrSnapshot =
  projectionFixtureAfter
    ( materializeProjectionDag
        (ProjectionDagSpec (IdentifierPool 0) (DomainSpec ["cache"]) (ScopeSpec ["src"] True) "cache lease token")
    )

runCurrent :: Connection -> SearchMaterialization -> RelevantRequest -> RelevantSource -> IO (Either RelevantError RelevantProjection)
runCurrent connection materialization = runRelevant connection p303RationaleSnapshot materialization

withRelevant :: ReadSnapshot -> (Connection -> SearchMaterialization -> IO value) -> IO value
withRelevant snapshot action = bracket (open ":memory:") close $ \connection -> do
  initializeSearchSchema connection >>= (@?= Right ())
  materialization <- case materializeCurrentSearch snapshot of
    Left problem -> assertFailure ("materialization failed: " <> show problem) >> error "unreachable"
    Right value -> pure value
  replaceSearchMaterialization connection materialization >>= (@?= Right ())
  action connection materialization

mustRelevant :: Either RelevantError RelevantProjection -> IO RelevantProjection
mustRelevant (Left problem) = assertFailure ("relevance failed: " <> show problem) >> error "unreachable"
mustRelevant (Right value) = pure value

onlyResult :: RelevantProjection -> RelevantResult
onlyResult projection = case relevantProjectionResults projection of
  [result] -> result
  results -> error ("expected one relevance result, got " <> show (length results))

lookupNested :: Text.Text -> Maybe JsonValue -> Maybe JsonValue
lookupNested key (Just (JsonObject members)) = lookup key members
lookupNested _ _ = Nothing

assertClose :: Double -> Double -> IO ()
assertClose expected actual =
  assertBool ("expected " <> show expected <> ", got " <> show actual) (abs (expected - actual) < 1.0e-12)

assertFiniteResult :: RelevantResult -> IO ()
assertFiniteResult result =
  assertBool
    "relevance scores are finite"
    (all finite [relevantResultScore result, relevantResultSemanticScore result, relevantResultLexicalScore result, relevantResultMargin result])
  where
    finite value = not (isNaN value || isInfinite value)

mustRepoPath :: Text.Text -> RepoPath
mustRepoPath value = case mkRepoPath value of
  Left problem -> error (show problem)
  Right path -> path
