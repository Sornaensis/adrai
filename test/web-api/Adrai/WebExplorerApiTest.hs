{-# LANGUAGE OverloadedStrings #-}

module Adrai.WebExplorerApiTest (tests) where

import Adrai.CliTypes (toAesonValue)
import Adrai.Fixture.CompilerRepository (compilerAdrId, conflictedCompilerFiles)
import Adrai.Domain (mkDomain)
import Adrai.Format.Document (ParsedManagedDocument (..), ManagedRecord (..), DecisionRecord (..), ConnectionRecord (..), ConnectionPayload (..), AppliesToPayload (..), DomainsPayload (..), StatusPayload (..), StatusState (..), canonicalManagedPath, parseManagedDocument, renderManagedSemantic, sealManagedDocument)
import qualified Adrai.Graph as Graph
import Adrai.History (ReadSnapshot (..), RevisionIdentity (..))
import Adrai.Provenance (GitOid, ProvenanceCapsuleInput (..), ProvenanceObjectId (..), mkEventKind, mkGitOid, mkLineAnchor, mkProvenanceCapsule, semanticDigest)
import Adrai.Query (CollapsedProjection (..), CollapsedRichDetail (..), ExplodedProjection (..), ExplodedOptions (..), ProjectionMode (..), ResolutionState (..), SearchProjection (..), SearchRequest (..), SearchResult (..), defaultSearchRequest, collapsedProjectionJson, explodedProjectionJson, projectCollapsed, projectExploded, runCurrentSearch, searchProjectionJson)
import Adrai.Retrieval (SearchMaterialization (..))
import Adrai.Scope (mkScopePattern)
import Adrai.Types (Actor, ActorKind (..), Config (..), OperationId, ProvenanceInputs (..), defaultConfig, mkActor, mkAdrId, mkConnectionId, mkOperationId, mkRecordId, mkRepoPath)
import Control.Exception (bracket)
import qualified Adrai.Web.Api as Api
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple (close, open)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests = testGroup "web explorer API"
  [ testCase "real conflicted rich and exploded projections retain the same candidate bodies" conflictedProjectionFixture,
    testCase "1001 valid matching decisions yield one exact 1000-result blank window" boundedWindowFixture
  ]

conflictedProjectionFixture :: IO ()
conflictedProjectionFixture = do
  let basisText = Text.replicate 40 "1"
      basis = must (mkGitOid basisText)
      files = must (conflictedCompilerFiles basis)
      documents = must (traverse parseFixtureFile files)
      records = map parsedManagedRecord documents
      reduction = Graph.reduceManagedGraph records
      snapshot = ReadSnapshot (RevisionIdentity basisText basisText) documents reduction Map.empty
      collapsed = must (projectCollapsed RichProjection snapshot compilerAdrId)
      exploded = must (projectExploded (ExplodedOptions False) snapshot compilerAdrId)
      collapsedJson = toAesonValue (collapsedProjectionJson collapsed)
      explodedJson = toAesonValue (explodedProjectionJson exploded)
      conflictsJson = Api.apiResultPayload (Api.ApiConflictsResult (Api.ConflictsResult (mapMaybe Graph.classifyAdrConflict (Graph.graphReductionAdrs reduction))))
  assertBool "fixture has no integrity errors" (null (Graph.graphReductionIssues reduction))
  assertBool "shared rich projection is actually conflicted" (not (resolutionStateResolved (collapsedResolution collapsed)))
  assertBool "shared exploded projection is actually conflicted" (not (resolutionStateResolved (explodedResolution exploded)))
  assertBool "candidate decision heads are retained" (maybe False ((== 2) . length . richRecordHeads) (collapsedRichDetail collapsed))
  searched <- bracket (open ":memory:") close $ \connection ->
    runCurrentSearch connection snapshot (SearchMaterialization [] [] []) (defaultSearchRequest "")
  let searchJson = toAesonValue (searchProjectionJson (must searched))
  fixture <- must . Aeson.eitherDecodeStrict' <$> ByteString.readFile "web/fixtures/api-v1.json"
  fixtureData fixture "rich_conflicted" @?= Just collapsedJson
  fixtureData fixture "exploded_conflicted" @?= Just explodedJson
  fixtureData fixture "search_conflicted" @?= Just searchJson
  fixtureData fixture "conflicts" @?= Just conflictsJson

fixtureData :: Aeson.Value -> Text -> Maybe Aeson.Value
fixtureData (Aeson.Object root) name = do
  Aeson.Object cases <- KeyMap.lookup "cases" root
  Aeson.Object envelope <- KeyMap.lookup (Key.fromText name) cases
  KeyMap.lookup "data" envelope
fixtureData _ _ = Nothing

boundedWindowFixture :: IO ()
boundedWindowFixture = do
  let basisText = Text.replicate 40 "2"
      basis = must (mkGitOid basisText)
      documents = concatMap (windowDocuments basis) [1 .. 1001]
      reduction = Graph.reduceManagedGraph (map parsedManagedRecord documents)
      snapshot = ReadSnapshot (RevisionIdentity basisText basisText) documents reduction Map.empty
      request = (defaultSearchRequest "") {searchRequestLimit = 1000}
  assertBool "every synthetic source is a validated sealed document" (null (Graph.graphReductionIssues reduction))
  length (Graph.graphReductionAdrs reduction) @?= 1001
  projected <- bracket (open ":memory:") close $ \connection ->
    runCurrentSearch connection snapshot (SearchMaterialization [] [] []) request
  let result = must projected
      ids = map searchResultAdr (searchProjectionResults result)
  searchProjectionLimit result @?= 1000
  length ids @?= 1000
  Set.size (Set.fromList ids) @?= 1000

windowDocuments :: GitOid -> Int -> [ParsedManagedDocument]
windowDocuments basis number = map (windowDocument basis actor operation) members
  where
    identifier prefix value = Text.cons prefix (Text.justifyRight 26 '0' (Text.pack (show value)))
    adr = must (mkAdrId (identifier 'A' number))
    record = must (mkRecordId (identifier 'R' number))
    scopeConnection = must (mkConnectionId (identifier 'C' (number * 4)))
    domainConnection = must (mkConnectionId (identifier 'C' (number * 4 + 1)))
    statusConnection = must (mkConnectionId (identifier 'C' (number * 4 + 2)))
    operation = must (mkOperationId (identifier 'O' number))
    actor = must (mkActor ServiceActor "window-fixture" Nothing)
    domain = must (mkDomain "platform")
    scope = must (mkScopePattern "src/**")
    decision = ManagedDecision (DecisionRecord adr record ("Window decision " <> Text.pack (show number)) "A sealed matching decision." [domain] "# Decision\nUse an exact bounded window.\n")
    scopeRecord = ManagedConnection (ConnectionRecord scopeConnection (AppliesToConnection (AppliesToPayload adr [] "initial" [scope] [] [scope])) "Initial scope.\n")
    domainRecord = ManagedConnection (ConnectionRecord domainConnection (DomainsConnection (DomainsPayload adr [] "initial" [domain] [] [domain] [])) "Initial domain.\n")
    statusRecord = ManagedConnection (ConnectionRecord statusConnection (StatusConnection (StatusPayload adr [] StatusActive [record] Nothing)) "Initial status.\n")
    members =
      [ (decision, "decision.create", [], ProvenanceRecord record),
        (scopeRecord, "scope.initial", [ProvenanceRecord record], ProvenanceConnection scopeConnection),
        (domainRecord, "domain.initial", [ProvenanceRecord record], ProvenanceConnection domainConnection),
        (statusRecord, "status.initial", [ProvenanceRecord record], ProvenanceConnection statusConnection)
      ]

windowDocument :: GitOid -> Actor -> OperationId -> (ManagedRecord, Text, [ProvenanceObjectId], ProvenanceObjectId) -> ParsedManagedDocument
windowDocument basis actor operation (managed, eventText, parents, objectId) =
  must (parseManagedDocument path bytes)
  where
    semantic = must (renderManagedSemantic managed)
    event = must (mkEventKind eventText)
    anchor = must (mkLineAnchor "window@logical\nline" basis)
    capsule = must (mkProvenanceCapsule (ProvenanceCapsuleInput operation objectId event actor 1700000000000 basis parents Nothing Nothing [anchor] (semanticDigest semantic) "adrai/1.0.0" (ProvenanceInputs Nothing Nothing Nothing)))
    bytes = must (sealManagedDocument managed capsule)
    path = must (canonicalManagedPath (configManagedPaths defaultConfig) managed)

parseFixtureFile :: (FilePath, ByteString.ByteString) -> Either Text ParsedManagedDocument
parseFixtureFile (path, bytes) = do
  repoPath <- either (Left . Text.pack . show) Right (mkRepoPath (Text.pack path))
  either (Left . Text.pack . show) Right (parseManagedDocument repoPath bytes)

must :: (Show error) => Either error value -> value
must result = case result of Right value -> value; Left problem -> error (show problem)
