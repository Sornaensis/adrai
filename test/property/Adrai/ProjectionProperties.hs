{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Adrai.ProjectionProperties (tests) where

import Adrai.Format (renderDigest)
import Adrai.Format.Json (JsonValue)
import Adrai.History
import Adrai.Property.Generators
import Adrai.Provenance (sha256Digest)
import Adrai.Query
import Adrai.State
import Adrai.Types
import Data.ByteString (ByteString)
import Data.List (find, sortOn)
import Data.Text qualified as Text
import Hedgehog (Property, assert, footnote, forAll, property, withTests, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "P2-06 projection properties"
    [ testProperty "all public projection bytes ignore document permutations" prop_projectionPermutation,
      testProperty "history topology overrides clocks and orders siblings" prop_historyTopology,
      testProperty "reverse comparisons swap direction without requiring symmetric diff text" prop_compareDirection,
      testProperty "state tokens canonicalize order and preserve multiplicity" prop_stateTokenMultiplicity,
      testProperty "A R and C lookup ignores snapshot document order" prop_referencePermutation
    ]

data ProjectionBytes = ProjectionBytes
  { bytesCollapsed :: ByteString,
    bytesExploded :: ByteString,
    bytesHistory :: ByteString,
    bytesCompare :: ByteString
  }
  deriving (Eq, Show)

prop_projectionPermutation :: Property
prop_projectionPermutation = withTests 60 . property $ do
  spec <- forAll genProjectionDagSpec
  let fixture = materializeProjectionDag spec
      before = projectionFixtureBefore fixture
      after = projectionFixtureAfter fixture
      expected = renderAll fixture before after
  beforeDocuments <- forAll (sampledPermutation (readSnapshotDocuments before))
  afterDocuments <- forAll (sampledPermutation (readSnapshotDocuments after))
  secondAfterDocuments <- forAll (sampledPermutation (readSnapshotDocuments after))
  let shuffledBefore = before {readSnapshotDocuments = beforeDocuments}
      shuffledAfter = after {readSnapshotDocuments = afterDocuments}
      secondAfter = after {readSnapshotDocuments = secondAfterDocuments}
  renderAll fixture shuffledBefore shuffledAfter === expected
  renderAll fixture shuffledBefore secondAfter === expected

prop_historyTopology :: Property
prop_historyTopology = withTests 60 . property $ do
  spec <- forAll genProjectionDagSpec
  let fixture = materializeProjectionDag spec
      options = defaultHistoryOptions {historyOptionOrder = OldestFirst}
  case projectHistory (projectionFixtureAfter fixture) (Just (projectionFixturePrimaryAdr fixture)) options of
    Left problem -> footnote (show problem) >> assert False
    Right projection -> do
      let operations = historyProjectionOperations projection
      map historyOperationId operations === projectionFixtureOperationOrder fixture
      map historyOperationClaimedAt operations === [300, 100, 100, 200]
      case operations of
        _created : amended : _ -> do
          historyOperationLabel amended === "amended"
          historyOperationChanges amended === ["decision"]
        _ -> footnote "projection DAG did not retain its amendment operation" >> assert False

prop_compareDirection :: Property
prop_compareDirection = withTests 60 . property $ do
  spec <- forAll genProjectionDagSpec
  let fixture = materializeProjectionDag spec
      options = CompareOptions True []
      forward = expect "forward comparison" (compareSnapshots options (projectionFixtureBefore fixture) (projectionFixtureAfter fixture))
      reverseProjection = expect "reverse comparison" (compareSnapshots options (projectionFixtureAfter fixture) (projectionFixtureBefore fixture))
      forwardCounts = compareCounts forward
      reverseCounts = compareCounts reverseProjection
  compareAdded forwardCounts === compareRemoved reverseCounts
  compareRemoved forwardCounts === compareAdded reverseCounts
  compareChanged forwardCounts === compareChanged reverseCounts
  compareUnchanged forwardCounts === compareUnchanged reverseCounts
  assert (all (entryReverses (compareEntries reverseProjection)) (compareEntries forward))

prop_stateTokenMultiplicity :: Property
prop_stateTokenMultiplicity = withTests 75 . property $ do
  pool <- forAll genIdentifierPool
  let r0 = recordIdAt pool 0
      r1 = recordIdAt pool 1
      c0 = connectionIdAt pool 0
      c1 = connectionIdAt pool 1
      heads = StateHeads [r1, r0] [c1, c0] [c0] [c1]
  records <- forAll (sampledPermutation (stateRecordHeads heads))
  scopes <- forAll (sampledPermutation (stateScopeHeads heads))
  statuses <- forAll (sampledPermutation (stateStatusHeads heads))
  domains <- forAll (sampledPermutation (stateDomainHeads heads))
  let permuted = StateHeads records scopes statuses domains
      duplicateHeads = heads {stateRecordHeads = [r1, r0, r0]}
      duplicatePayload = canonicalStatePayload duplicateHeads
  canonicalStatePayload permuted === canonicalStatePayload heads
  stateTokenForHeads permuted === independentlyComputedToken heads
  Text.count (recordIdText r0) duplicatePayload === 2
  stateTokenForHeads duplicateHeads === independentlyComputedToken duplicateHeads

prop_referencePermutation :: Property
prop_referencePermutation = withTests 60 . property $ do
  spec <- forAll genProjectionDagSpec
  let fixture = materializeProjectionDag spec
      snapshot = projectionFixtureAfter fixture
      expected = Right (projectionFixturePrimaryAdr fixture)
      references =
        [ adrIdText (projectionFixturePrimaryAdr fixture),
          recordIdText (projectionFixturePrimaryRecord fixture),
          connectionIdText (projectionFixturePrimaryConnection fixture)
        ]
  documents <- forAll (sampledPermutation (readSnapshotDocuments snapshot))
  let shuffled = snapshot {readSnapshotDocuments = documents}
  map (resolveAdrReference snapshot) references === replicate 3 expected
  map (resolveAdrReference shuffled) references === replicate 3 expected

renderAll :: ProjectionFixture -> ReadSnapshot -> ReadSnapshot -> ProjectionBytes
renderAll fixture before after =
  ProjectionBytes
    { bytesCollapsed = renderCollapsedProjection (expect "collapsed projection" (projectCollapsed RichProjection after adr)),
      bytesExploded = renderExplodedProjection (expect "exploded projection" (projectExploded (ExplodedOptions True) after adr)),
      bytesHistory = renderHistoryProjection (expect "history projection" (projectHistory after (Just adr) defaultHistoryOptions)),
      bytesCompare = renderCompareProjection (expect "compare projection" (compareSnapshots (CompareOptions True []) before after))
    }
  where
    adr = projectionFixturePrimaryAdr fixture

entryReverses :: [CompareEntry] -> CompareEntry -> Bool
entryReverses reverseEntries forward =
  case find ((== compareEntryAdr forward) . compareEntryAdr) reverseEntries of
    Nothing -> False
    Just reversed ->
      compareEntryKind reversed == reversedKind (compareEntryKind forward)
        && compareEntryBefore forward == compareEntryAfter reversed
        && compareEntryAfter forward == compareEntryBefore reversed
        && directionalChanges (compareEntryChanges forward) == reverseDirectionalChanges (compareEntryChanges reversed)

reversedKind :: Text.Text -> Text.Text
reversedKind kind =
  case kind of
    "added" -> "removed"
    "removed" -> "added"
    other -> other

directionalChanges :: [CompareChange] -> [(Text.Text, JsonValue, JsonValue)]
directionalChanges =
  sortOn first . map (\change -> (compareChangeField change, compareChangeBefore change, compareChangeAfter change))
  where
    first (field, _, _) = field

reverseDirectionalChanges :: [CompareChange] -> [(Text.Text, JsonValue, JsonValue)]
reverseDirectionalChanges =
  sortOn first . map (\change -> (compareChangeField change, compareChangeAfter change, compareChangeBefore change))
  where
    first (field, _, _) = field

independentlyComputedToken :: StateHeads -> StateToken
independentlyComputedToken heads =
  expect "independent state token" . mkStateToken $
    "S" <> Text.take 22 (Text.drop 7 digest)
  where
    digest = renderDigest (sha256Digest (canonicalStatePayloadBytes heads))

expect :: String -> Either error value -> value
expect label result =
  case result of
    Right value -> value
    Left _ -> error ("invalid P2-06 " <> label)
