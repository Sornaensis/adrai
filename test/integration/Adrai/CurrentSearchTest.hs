{-# LANGUAGE OverloadedStrings #-}

module Adrai.CurrentSearchTest (tests, p304ConflictSnapshot) where

import Adrai.Compiler (materializeCurrentSearch)
import Adrai.CompilerMaterializationTest (p303RationaleSnapshot)
import Adrai.Format.Json (JsonValue (..))
import Adrai.Graph (GraphAxis (..), reduceManagedGraph)
import Adrai.History (ActorSelector (..), ReadSnapshot (..), RevisionIdentity (..))
import Adrai.Property.Generators
import Adrai.Query
import Adrai.Retrieval
import Adrai.Sqlite
import Adrai.Types
import Control.Exception (bracket)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Database.SQLite.Simple (Connection, close, open)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "P3-04 current weighted search"
    [ testCase "blank query is a filtered deterministic ADR listing" blankSearchContract,
      testCase "fts, vector, and hybrid run without optional dependencies" threeModeContract,
      testCase "eligibility filters run before retrieval" filterBeforeRetrievalContract,
      testCase "obsolete actor time and validation filters are exact" ordinaryFilterValidationContract,
      testCase "scope forks retain matching ambiguity and exclude no-match files" scopeConflictFilterContract,
      testCase "decision heads are retrieved independently then grouped once" conflictGroupingContract,
      testCase "repeated hybrid runs are byte-stable projections" repeatStabilityContract
    ]

blankSearchContract :: IO ()
blankSearchContract = withSearch p303RationaleSnapshot $ \connection materialization -> do
  let request = (defaultSearchRequest "") {searchRequestIncludeObsolete = True}
  projection <- mustSearch =<< runCurrentSearch connection p303RationaleSnapshot materialization request
  map searchResultAdr (searchProjectionResults projection) @?= map searchDocumentAdrId (searchMaterializationDocuments materialization)
  map searchResultScore (searchProjectionResults projection) @?= [1]
  assertBool "blank retrieval is explicitly labelled" (all (hasAlgorithm "filtered-list" . searchResultRetrieval) (searchProjectionResults projection))

threeModeContract :: IO ()
threeModeContract = withSearch p303RationaleSnapshot $ \connection materialization -> do
  let run mode =
        runCurrentSearch
          connection
          p303RationaleSnapshot
          materialization
          ((defaultSearchRequest "current decision api") {searchRequestMode = mode, searchRequestIncludeObsolete = True})
  fts <- mustSearch =<< run FtsRetrieval
  vector <- mustSearch =<< run VectorRetrieval
  hybrid <- mustSearch =<< run HybridRetrieval
  map (map searchResultAdr . searchProjectionResults) [fts, vector, hybrid]
    @?= replicate 3 [searchDocumentAdrId (onlyDocument materialization)]
  algorithms (fts, vector, hybrid)
    @?= [ "three-fts+weighted-rrf",
          "exact-summary+identifier+section-rerank+weighted-rrf",
          "three-fts+exact-summary+section-rerank+weighted-rrf"
        ]
  assertBool "vector evidence is finite and non-zero" (all ((> 0) . searchResultVectorScore . onlyResult) [vector, hybrid])
  semanticSummaryText (onlyDocument materialization)
    @?= Text.intercalate
      "\n"
      [ "Current Decision API (CDAPI)",
        "Current summary",
        "decision first\ndecision second",
        "current.domain",
        "status: current status\namends: current amendment\ndomains: current domain\napplies_to: current scope"
      ]
  let multiDomainDocument = (onlyDocument materialization) {searchDocumentDomains = ["compiler", "cache"]}
      multiDomainFields = searchDocumentLexicalFields multiDomainDocument
      multiDomainEvidence = lexicalEvidence (buildQueryPlan "compiler cache" []) multiDomainFields
  lookup "domains" multiDomainFields @?= Just "compiler cache"
  assertBool "semantic summary uses the prototype's space-joined domains" ("\ncompiler cache\n" `Text.isInfixOf` semanticSummaryText multiDomainDocument)
  assertBool "a phrase spanning canonical domains is exact lexical evidence" ("domains" `elem` lexicalExactPhraseFields multiDomainEvidence)
  where
    algorithms (fts, vector, hybrid) = map (algorithm . onlyResult) [fts, vector, hybrid]
    algorithm = retrievalAlgorithmText . searchResultRetrieval

filterBeforeRetrievalContract :: IO ()
filterBeforeRetrievalContract = withSearch p303RationaleSnapshot $ \connection materialization -> do
  let base = (defaultSearchRequest "current") {searchRequestIncludeObsolete = True}
      matching = base {searchRequestDomains = ["current"], searchRequestFile = Just (mustRepoPath "src/current/module.hs")}
      wrongDomain = base {searchRequestDomains = ["unrelated"]}
      wrongFile = base {searchRequestFile = Just (mustRepoPath "docs/readme.md")}
  matched <- mustSearch =<< runCurrentSearch connection p303RationaleSnapshot materialization matching
  excludedDomain <- mustSearch =<< runCurrentSearch connection p303RationaleSnapshot materialization wrongDomain
  excludedFile <- mustSearch =<< runCurrentSearch connection p303RationaleSnapshot materialization wrongFile
  length (searchProjectionResults matched) @?= 1
  searchProjectionResults excludedDomain @?= []
  searchProjectionResults excludedFile @?= []
  searchMatchFileScope (searchResultMatches (onlyResult matched)) @?= Just ScopeExact

ordinaryFilterValidationContract :: IO ()
ordinaryFilterValidationContract = withSearch p303RationaleSnapshot $ \connection materialization -> do
  let run request = runCurrentSearch connection p303RationaleSnapshot materialization request
      base = defaultSearchRequest "current"
      visible = base {searchRequestIncludeObsolete = True}
      exactOperation =
        visible
          { searchRequestActor = Just (ActorSelector HumanActor "p2-06"),
            searchRequestSince = Just 105,
            searchRequestUntil = Just 105
          }
  defaultExcluded <- mustSearch =<< run base
  exactIncluded <- mustSearch =<< run exactOperation
  wrongActor <- mustSearch =<< run (visible {searchRequestActor = Just (ActorSelector HumanActor "other")})
  afterDecision <- mustSearch =<< run (visible {searchRequestSince = Just 106})
  beforeDecision <- mustSearch =<< run (visible {searchRequestUntil = Just 104})
  searchProjectionResults defaultExcluded @?= []
  length (searchProjectionResults exactIncluded) @?= 1
  searchProjectionResults wrongActor @?= []
  searchProjectionResults afterDecision @?= []
  searchProjectionResults beforeDecision @?= []
  run (visible {searchRequestLimit = 0}) >>= (@?= Left (SearchInvalidLimit 0))
  run (visible {searchRequestLimit = 1001}) >>= (@?= Left (SearchInvalidLimit 1001))
  run (visible {searchRequestSince = Just 106, searchRequestUntil = Just 105})
    >>= (@?= Left (SearchInvalidTimeRange 106 105))

scopeConflictFilterContract :: IO ()
scopeConflictFilterContract = withSearch p304ScopeConflictSnapshot $ \connection materialization -> do
  let run path =
        runCurrentSearch
          connection
          p304ScopeConflictSnapshot
          materialization
          ((defaultSearchRequest "base") {searchRequestFile = Just (mustRepoPath path)})
  matching <- mustSearch =<< run "test/module.hs"
  excluded <- mustSearch =<< run "unmatched/module.hs"
  let result = onlyResult matching
  searchMatchFileScope (searchResultMatches result) @?= Just ScopeAmbiguous
  searchResultScopeAmbiguous result @?= True
  searchProjectionResults excluded @?= []

conflictGroupingContract :: IO ()
conflictGroupingContract = withSearch p304ConflictSnapshot $ \connection materialization -> do
  let search term = runCurrentSearch connection p304ConflictSnapshot materialization ((defaultSearchRequest term) {searchRequestMode = HybridRetrieval})
  left <- mustSearch =<< search "left"
  right <- mustSearch =<< search "right"
  length (searchProjectionResults left) @?= 1
  length (searchProjectionResults right) @?= 1
  let leftResult = onlyResult left
      rightResult = onlyResult right
  searchResultAdr leftResult @?= searchResultAdr rightResult
  searchResultTitle leftResult @?= "[conflicted ADR]"
  searchResultSummary leftResult @?= "left summary; right summary"
  searchResultTitle rightResult @?= "[conflicted ADR]"
  searchResultSummary rightResult @?= "left summary; right summary"
  searchResultMatchedTitle leftResult @?= Just "left"
  searchResultMatchedTitle rightResult @?= Just "right"
  assertBool "selected heads retain ADR@RID identity" (maybe False (Text.isInfixOf "@") (searchResultMatchedCandidate leftResult))
  map resolutionConflictKind (resolutionStateConflicts (searchResultResolution leftResult)) @?= [DecisionConflict]

repeatStabilityContract :: IO ()
repeatStabilityContract = withSearch p304ConflictSnapshot $ \connection materialization -> do
  let request = defaultSearchRequest "left decision"
  first <- mustSearch =<< runCurrentSearch connection p304ConflictSnapshot materialization request
  second <- mustSearch =<< runCurrentSearch connection p304ConflictSnapshot materialization request
  renderSearchProjection first @?= renderSearchProjection second

p304ConflictSnapshot :: ReadSnapshot
p304ConflictSnapshot = axisConflictSnapshot DecisionAxis

p304ScopeConflictSnapshot :: ReadSnapshot
p304ScopeConflictSnapshot = axisConflictSnapshot ScopeAxis

axisConflictSnapshot :: GraphAxis -> ReadSnapshot
axisConflictSnapshot axis =
  ReadSnapshot
    (RevisionIdentity requested resolved)
    documents
    (reduceManagedGraph records)
    Map.empty
  where
    pool = IdentifierPool 0
    fixture = materializeAxisConflict (AxisConflictSpec pool axis 0)
    records = axisFixtureRecords fixture
    documents = zipWith (\ordinal record -> parsedDocument pool (10 + ordinal) (1000 + fromIntegral ordinal) [] record) [0 ..] records
    requested = "p3-04-" <> axisLabel
    resolved = requested <> "-resolved"
    axisLabel = case axis of DecisionAxis -> "conflict"; ScopeAxis -> "scope-conflict"; DomainAxis -> "domain-conflict"; StatusAxis -> "status-conflict"

withSearch :: ReadSnapshot -> (Connection -> SearchMaterialization -> IO value) -> IO value
withSearch snapshot action = bracket (open ":memory:") close $ \connection -> do
  initializeSearchSchema connection >>= (@?= Right ())
  materialization <- case materializeCurrentSearch snapshot of
    Left problem -> assertFailure ("materialization failed: " <> show problem) >> error "unreachable"
    Right value -> pure value
  replaceSearchMaterialization connection materialization >>= (@?= Right ())
  action connection materialization

mustSearch :: Either SearchError SearchProjection -> IO SearchProjection
mustSearch value = case value of
  Left problem -> assertFailure ("search failed: " <> show problem) >> error "unreachable"
  Right result -> pure result

onlyDocument :: SearchMaterialization -> SearchDocument
onlyDocument materialization = case searchMaterializationDocuments materialization of
  [document] -> document
  documents -> error ("expected one search document, got " <> show (length documents))

onlyResult :: SearchProjection -> SearchResult
onlyResult projection = case searchProjectionResults projection of
  [result] -> result
  results -> error ("expected one search result, got " <> show (length results))

mustRepoPath :: Text.Text -> RepoPath
mustRepoPath value = case mkRepoPath value of
  Right path -> path
  Left problem -> error ("invalid repository path fixture: " <> show problem)

hasAlgorithm :: Text.Text -> JsonValue -> Bool
hasAlgorithm expected value = retrievalAlgorithmText value == expected

retrievalAlgorithmText :: JsonValue -> Text.Text
retrievalAlgorithmText value = case value of
  JsonObject members -> case lookup "algorithm" members of Just (JsonString algorithm) -> algorithm; _ -> ""
  _ -> ""
