{-# LANGUAGE OverloadedStrings #-}

module Adrai.WebExplorerApiTest (tests) where

import Adrai.CliTypes (toAesonValue)
import Adrai.Fixture.CompilerRepository (compilerAdrId, conflictedCompilerFiles)
import Adrai.Format.Document (ParsedManagedDocument (..), parseManagedDocument)
import qualified Adrai.Graph as Graph
import Adrai.History (ReadSnapshot (..), RevisionIdentity (..))
import Adrai.Provenance (mkGitOid)
import Adrai.Query (CollapsedProjection (..), CollapsedRichDetail (..), ExplodedProjection (..), ExplodedOptions (..), ProjectionMode (..), ResolutionState (..), SearchProjection (..), SearchRequest (..), SearchResult (..), defaultSearchRequest, collapsedProjectionJson, explodedProjectionJson, projectCollapsed, projectExploded, runCurrentSearch, searchProjectionJson)
import Adrai.Retrieval (SearchMaterialization (..))
import Adrai.Types (mkRepoPath)
import Adrai.WindowDocuments (windowDocuments)
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

parseFixtureFile :: (FilePath, ByteString.ByteString) -> Either Text ParsedManagedDocument
parseFixtureFile (path, bytes) = do
  repoPath <- either (Left . Text.pack . show) Right (mkRepoPath (Text.pack path))
  either (Left . Text.pack . show) Right (parseManagedDocument repoPath bytes)

must :: (Show error) => Either error value -> value
must result = case result of Right value -> value; Left problem -> error (show problem)
