module Adrai.FixtureQueryMaterializationScaleTest (tests) where

import Adrai.Fixture.QueryMaterialization
import Adrai.Fixture.RetrievalScale
  ( adrTemplates,
    retrievalScaleV1,
  )
import Adrai.Fixture.Types (AdrKey (..), RetrievalScaleSpec (..))
import Adrai.Graph (GraphReduction (graphReductionIssues))
import Adrai.History
  ( ReadSnapshot (readSnapshotDocuments, readSnapshotReduction),
    validateReadSnapshot,
  )
import Adrai.Retrieval (SearchMaterialization (searchMaterializationDocuments))
import Adrai.Types (adrIdText)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "P3-06 scale identity materialization"
    [ testCase "byte-derived identities and production materialization support 2,000 ADR ordinals" scaleIdentityContract
    ]

scaleIdentityContract :: IO ()
scaleIdentityContract =
  case NonEmpty.nonEmpty adrTemplates of
    Nothing -> assertFailure "the 2,000 ADR fixture unexpectedly became empty"
    Just templates -> do
      let materialization = mustRight (materializeQueryFixture (retrievalScaleMeta retrievalScaleV1) templates [])
          adrIds = queryMaterializationAdrIds materialization
          documents = searchMaterializationDocuments (queryMaterializationSearch materialization)
          snapshot = queryMaterializationSnapshot materialization
      Map.size adrIds @?= retrievalLogicalAdrCount retrievalScaleV1
      length documents @?= retrievalLogicalAdrCount retrievalScaleV1
      length (readSnapshotDocuments snapshot) @?= retrievalLogicalAdrCount retrievalScaleV1 * 4
      Set.size (Set.fromList (map adrIdText (Map.elems adrIds))) @?= retrievalLogicalAdrCount retrievalScaleV1
      mapM_ (\key -> assertRight (lookupQueryMaterializationAdr (AdrKey key) materialization)) [0, 999, 1999]
      graphReductionIssues (readSnapshotReduction snapshot) @?= []
      validateReadSnapshot snapshot @?= Right ()

assertRight :: (Show problem) => Either problem value -> IO ()
assertRight result =
  case result of
    Right _ -> pure ()
    Left problem -> assertFailure (show problem)

mustRight :: (Show problem) => Either problem value -> value
mustRight result =
  case result of
    Right value -> value
    Left problem -> error (show problem)
