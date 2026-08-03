{-# LANGUAGE OverloadedStrings #-}

module Adrai.QueryHistoryTest (tests, writeProjectionGoldens) where

import Adrai.Domain (Domain, mkDomain)
import Adrai.Format.Document
import Adrai.Format.Json
import Adrai.Graph
import Adrai.History
import Adrai.Provenance
import Adrai.Query
import Adrai.Scope (ScopePattern, mkScopePattern)
import Adrai.Types
import qualified Data.ByteString as BS
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "query, history, comparison, and public JSON"
    [ testCase "canonical JSON byte golden recursively sorts, indents, ASCII escapes, and ends LF" canonicalJsonContract,
      testCase "public fixture key inventories are exact byte goldens" keyInventoryContract,
      testCase "read snapshot keeps placement evidence separate from capsule basis" snapshotEvidenceContract,
      testCase "collapsed compact and rich projections preserve exact resolved semantics" collapsedResolvedContract,
      testCase "resolution envelope orders axes then unmatched integrity and handles stale obsolete" collapsedConflictContracts,
      testCase "snapshot inconsistency is rejected by collapsed, exploded, history, and compare" snapshotConsistencyContract,
      testCase "ADR and repository histories use topology, exact labels, filters, and optional omission" historyContract,
      testCase "cyclic history appends the whole residual set in stable claimed-time order" historyCycleContract,
      testCase "exploded operations have exact item fields, grouped provenance, raw opt-in, and real hunks" explodedContract,
      testCase "revision-local A/R/C lookup is deterministic under duplicate ownership and permutations" prefixContract,
      testCase "compare shape, classifications, hidden unchanged counts, order, and hunks are stable" compareContract,
      testCase "full public projections match byte-for-byte goldens" projectionByteGoldenContract,
      testCase "obsolete visibility is explicit while show/history retain obsolete ADRs" obsoleteVisibilityContract,
      testCase "all projection bytes are invariant under input permutation" permutationContract
    ]

canonicalJsonContract :: IO ()
canonicalJsonContract = do
  expected <- BS.readFile "test/golden/p2-05/canonical-json.golden"
  let value =
        objectOmittingNulls
          [ ("z", JsonString "caf\233 \128512 \DEL"),
            ("drop", JsonNull),
            ("a", JsonArray [JsonNumber 1, JsonBool True])
          ]
      actual = renderCanonicalJsonBytes value
  actual @?= expected
  snd (mustJust (BS.unsnoc actual)) @?= 10

keyInventoryContract :: IO ()
keyInventoryContract = do
  expected <- BS.readFile "test/golden/p2-05/public-key-inventories.golden"
  renderCanonicalJsonBytes publicKeyInventories @?= expected

projectionByteGoldenContract :: IO ()
projectionByteGoldenContract = mapM_ assertGolden projectionGoldenArtifacts
  where
    assertGolden (path, actual) = do
      expected <- BS.readFile path
      actual @?= expected

-- | Explicit acceptance hook for regenerating P2-05 projection goldens after a
-- deliberate public-contract change. It is never invoked by the test suite.
writeProjectionGoldens :: IO ()
writeProjectionGoldens = mapM_ (uncurry BS.writeFile) projectionGoldenArtifacts

projectionGoldenArtifacts :: [(FilePath, BS.ByteString)]
projectionGoldenArtifacts =
  [ ("test/golden/p2-05/collapsed-compact.golden", renderCollapsedProjection (must (projectCollapsed CompactProjection fullSnapshot primaryAdr))),
    ("test/golden/p2-05/collapsed-rich.golden", renderCollapsedProjection (must (projectCollapsed RichProjection fullSnapshot primaryAdr))),
    ("test/golden/p2-05/exploded-compact.golden", renderExplodedProjection (must (projectExploded (ExplodedOptions False) fullSnapshot primaryAdr))),
    ("test/golden/p2-05/exploded-raw.golden", renderExplodedProjection (must (projectExploded (ExplodedOptions True) fullSnapshot primaryAdr))),
    ("test/golden/p2-05/history-adr.golden", renderHistoryProjection (must (projectHistory fullSnapshot (Just primaryAdr) defaultHistoryOptions))),
    ("test/golden/p2-05/history-repository.golden", renderHistoryProjection (must (projectHistory repositoryGoldenSnapshot Nothing defaultHistoryOptions))),
    ("test/golden/p2-05/compare.golden", renderCompareProjection compareGoldenProjection),
    ("test/golden/p2-05/ambiguity-unicode.golden", ambiguityUnicodeGolden)
  ]

repositoryGoldenSnapshot :: ReadSnapshot
repositoryGoldenSnapshot =
  snapshotFor
    "repository"
    records
    (readSnapshotDocuments fullSnapshot <> [document (operationFromChar '8') 400 HumanActor "other" [] other])
  where
    otherAdr = exactAdr "A22222222222222222222222222"
    otherRecord = exactRecord "R88888888888888888888888888"
    other = decisionRecordFor otherAdr otherRecord "other"
    records = fullRecords <> [other]

compareGoldenProjection :: CompareProjection
compareGoldenProjection =
  must
    ( compareSnapshots
        (CompareOptions True [("mode", JsonString "pure")])
        (snapshotFor "before" beforeRecords (documentsSimple beforeRecords))
        (snapshotFor "after" afterRecords (documentsSimple afterRecords))
    )
  where
    unchangedAdr = exactAdr "A22222222222222222222222222"
    unchangedRecord = exactRecord "R88888888888888888888888888"
    unchanged = decisionRecordFor unchangedAdr unchangedRecord "unchanged"
    beforeRecords = take 4 fullRecords <> [unchanged]
    afterRecords = fullRecords <> [unchanged]

ambiguityUnicodeGolden :: BS.ByteString
ambiguityUnicodeGolden =
  renderCanonicalJsonBytes
    ( object
        [ ("error", JsonString (referenceLookupErrorText problem)),
          ("matches", JsonArray (map matchJson matches)),
          ("unicode", JsonString "Caf\233 architecture \937 \128512")
        ]
    )
  where
    firstAdr = exactAdr "A22222222222222222222222222"
    secondAdr = exactAdr "A33333333333333333333333333"
    shared = exactRecord "R12345671111111111111111111"
    records = [decisionRecordFor firstAdr shared "first", decisionRecordFor secondAdr shared "second"]
    snapshot = snapshotFor "duplicates" records (documentsSimple records)
    problem =
      case resolveAdrReference snapshot (recordIdText shared) of
        Left lookupProblem@(LookupAmbiguous _ _) -> lookupProblem
        other -> error ("expected ambiguity golden, got " <> show other)
    matches = case problem of LookupAmbiguous _ values -> values; _ -> []
    matchJson match =
      object
        [ ("adr", JsonString (adrIdText (referenceMatchAdr match))),
          ("object", JsonString (objectRefText (referenceMatchObject match)))
        ]

snapshotEvidenceContract :: IO ()
snapshotEvidenceContract = do
  let collapsed = must (projectCollapsed CompactProjection fullSnapshot primaryAdr)
      created = mustJust (collapsedCreatedProvenance (collapsedProvenance collapsed))
  revisionRequested (readSnapshotRevision fullSnapshot) @?= "refs/heads/main"
  revisionResolved (readSnapshotRevision fullSnapshot) @?= "deadbeef"
  projectedBasis created @?= Text.replicate 40 "a"
  projectedCommit created @?= Just "commit-placement-1"
  projectedOriginalCommits created @?= ["commit-placement-1"]
  projectedIntroductions created @?= []
  projectedLineLandings created @?= [LineLandingEvidence "trunk" "refs/heads/main" "landing-1" True]
  let rendered = TextEncoding.decodeUtf8 (renderCollapsedProjection collapsed)
      createdKeys = nestedObjectKeys ["provenance", "created"] (collapsedProjectionJson collapsed)
  assertBool "public provenance omits internal commit/placement fields" (all (`notElem` createdKeys) ["commit", "placement"])
  assertBool "public provenance retains classified original commits" ("commit-placement-1" `Text.isInfixOf` rendered)
  assertBool "public provenance names the basis" (Text.replicate 40 "a" `Text.isInfixOf` rendered)

collapsedResolvedContract :: IO ()
collapsedResolvedContract = do
  let compact = must (projectCollapsed CompactProjection fullSnapshot primaryAdr)
      rich = must (projectCollapsed RichProjection fullSnapshot primaryAdr)
      resolution = collapsedResolution compact
  collapsedRecord compact @?= Just r2
  collapsedTitle compact @?= "Caf\233 architecture v2"
  collapsedBody compact @?= Text.strip (versionedDecisionBody afterDecisionTail)
  collapsedDomains compact @?= ["compiler"]
  collapsedAppliesTo compact @?= ["src/**"]
  collapsedStatus compact @?= "active"
  resolutionStateResolved resolution @?= True
  resolutionStateRequired resolution @?= False
  resolutionStateConflicts resolution @?= []
  collapsedCounts compact @?= ProjectionCounts 2 1 1 1 2
  evolutionSummaryText (collapsedEvolution compact) @?= "created \8594 amended \8594 reactivated"
  historyOperationReason (mustJust (evolutionLatest (collapsedEvolution compact))) @?= Just "status update"
  collapsedRichDetail compact @?= Nothing
  assertBool "rich materially retains histories and sources" (maybe False (not . null . richSourcePaths) (collapsedRichDetail rich))
  assertBool "compact and rich bytes differ" (renderCollapsedProjection compact /= renderCollapsedProjection rich)
  let compactKeys = objectKeys (collapsedProjectionJson compact)
  compactKeys @?= expectedCollapsedKeys

collapsedConflictContracts :: IO ()
collapsedConflictContracts = do
  let conflictedRecords = conflictRecords primaryAdr
      conflicted = snapshotFor "conflict" conflictedRecords (documentsSimple conflictedRecords)
      projection = must (projectCollapsed RichProjection conflicted primaryAdr)
      conflicts = resolutionStateConflicts (collapsedResolution projection)
  collapsedRecord projection @?= Nothing
  collapsedTitle projection @?= "[conflicted ADR]"
  collapsedSummary projection @?= "first summary; second summary"
  collapsedBody projection @?= "# Decision\nfirst\n\n# Decision\nsecond"
  map resolutionConflictKind conflicts @?= [DecisionConflict, IntegrityConflict]
  resolutionHeadCount (firstUnsafe conflicts) @?= 2
  let invalidRecords = fullRecords <> [ManagedConnection (ConnectionRecord c6 (AmendsConnection (AmendsPayload primaryAdr r2 [exactRecord "R99999999999999999999999999"])) "dangling\n")]
      invalid = snapshotFor "invalid" invalidRecords (documentsSimple invalidRecords)
      invalidProjection = must (projectCollapsed RichProjection invalid primaryAdr)
      invalidConflicts = resolutionStateConflicts (collapsedResolution invalidProjection)
  assertBool "integrity survives singleton axes" (IntegrityConflict `elem` map resolutionConflictKind invalidConflicts)
  richRawConflicts (mustJust (collapsedRichDetail invalidProjection)) @?= ["1 integrity errors"]
  length (collapsedIssues invalidProjection) @?= 1
  resolutionStateResolved (collapsedResolution invalidProjection) @?= False
  let onlyScope = [scopeInitial primaryAdr c1]
      zeroProjection = must (projectCollapsed CompactProjection (snapshotFor "zero" onlyScope (documentsSimple onlyScope)) primaryAdr)
  map resolutionConflictKind (take 4 (resolutionStateConflicts (collapsedResolution zeroProjection)))
    @?= [IntegrityConflict, IntegrityConflict, IntegrityConflict, IntegrityConflict]
  let staleRecords = take 6 fullRecords <> [statusRevision c6 [c3] StatusObsolete [r1] Nothing]
      staleProjection = must (projectCollapsed CompactProjection (snapshotFor "stale" staleRecords (documentsSimple staleRecords)) primaryAdr)
      stale = filter ((== StatusConflict) . resolutionConflictKind) (resolutionStateConflicts (collapsedResolution staleProjection))
  map resolutionSummary stale @?= ["obsolete status does not cover current decision heads"]
  collapsedStatus staleProjection @?= "conflict"
  collapsedObsolete staleProjection @?= False

snapshotConsistencyContract :: IO ()
snapshotConsistencyContract = do
  let mismatched = fullSnapshot {readSnapshotDocuments = drop 1 (readSnapshotDocuments fullSnapshot)}
      expected = QuerySnapshotInvalid (SnapshotConsistencyError ["supplied graph reduction does not match parsed managed documents"])
  projectCollapsed CompactProjection mismatched primaryAdr @?= Left expected
  projectExploded (ExplodedOptions False) mismatched primaryAdr @?= Left expected
  projectHistory mismatched (Just primaryAdr) defaultHistoryOptions
    @?= Left (HistorySnapshotInvalid (SnapshotConsistencyError ["supplied graph reduction does not match parsed managed documents"]))
  compareSnapshots (CompareOptions False []) fullSnapshot mismatched @?= Left expected
  let firstDocument = firstUnsafe (readSnapshotDocuments fullSnapshot)
      inconsistent =
        fullSnapshot
          { readSnapshotDocuments = retimeDocument 301 firstDocument : drop 1 (readSnapshotDocuments fullSnapshot)
          }
      operationProblem = QuerySnapshotInvalid (SnapshotConsistencyError ["operation " <> operationIdText o1 <> " has inconsistent capsule fields"])
  projectCollapsed CompactProjection inconsistent primaryAdr @?= Left operationProblem

historyContract :: IO ()
historyContract = do
  let oldest = historyOperationsOldestFirst fullSnapshot (Just primaryAdr)
  map historyOperationLabel oldest @?= ["created", "amended", "reactivated"]
  map historyOperationClaimedAt oldest @?= [300, 100, 200]
  historyOperationChanges (firstUnsafe oldest) @?= ["decision", "scope", "domains", "status"]
  historyChangedFields (mustJust (historyOperationDetails (firstUnsafe (drop 1 oldest)))) @?= ["title", "summary", "decision text"]
  assertBool "current resolution stamped everywhere" (all historyOperationResolved oldest)
  let newest = must (projectHistory fullSnapshot (Just primaryAdr) defaultHistoryOptions)
  map historyOperationLabel (historyProjectionOperations newest) @?= ["reactivated", "amended", "created"]
  let filteredOptions = defaultHistoryOptions {historyOptionOrder = OldestFirst, historyOptionUntil = Just 100, historyOptionLimit = 1}
      filtered = must (projectHistory fullSnapshot (Just primaryAdr) filteredOptions)
  map historyOperationLabel (historyProjectionOperations filtered) @?= ["amended"]
  projectHistory fullSnapshot (Just primaryAdr) (defaultHistoryOptions {historyOptionLimit = 0}) @?= Left (HistoryInvalidLimit 0)
  let otherAdr = exactAdr "A22222222222222222222222222"
      otherRecord = exactRecord "R88888888888888888888888888"
      repositoryRecords = fullRecords <> [decisionRecordFor otherAdr otherRecord "other"]
      repository = snapshotFor "repository" repositoryRecords (readSnapshotDocuments fullSnapshot <> [document (operationFromChar '8') 400 HumanActor "other" [] (lastUnsafe repositoryRecords)])
      repoHistory = must (projectHistory repository Nothing defaultHistoryOptions)
  historyProjectionAdr repoHistory @?= Nothing
  assertBool "repository includes both ADRs" (all (`elem` map historyOperationAdr (historyProjectionOperations repoHistory)) [primaryAdr, otherAdr])
  let json = TextEncoding.decodeUtf8 (renderHistoryProjection newest)
  assertBool "claimed timestamp ISO" ("1970-01-01T00:00:00.200000Z" `Text.isInfixOf` json)
  assertBool "empty details retained" ("\"details\": {}" `Text.isInfixOf` json)

historyCycleContract :: IO ()
historyCycleContract = do
  let a = exactAdr "A22222222222222222222222222"
      b = exactAdr "A33333333333333333333333333"
      ra = exactRecord "R77777777777777777777777777"
      rb = exactRecord "R88888888888888888888888888"
      oa = operationFromChar '7'
      ob = operationFromChar '8'
      records = [decisionRecordFor a ra "a", decisionRecordFor b rb "b"]
      docs =
        [ document oa 20 HumanActor "a" [ProvenanceRecord rb] (headUnsafe records),
          document ob 10 HumanActor "b" [ProvenanceRecord ra] (lastUnsafe records)
        ]
      snapshot = snapshotFor "cycle" records docs
      operations = historyOperationsOldestFirst snapshot Nothing
  map historyOperationId operations @?= [ob, oa]

explodedContract :: IO ()
explodedContract = do
  let compact = must (projectExploded (ExplodedOptions False) fullSnapshot primaryAdr)
      raw = must (projectExploded (ExplodedOptions True) fullSnapshot primaryAdr)
  objectKeys (explodedProjectionJson compact) @?= expectedExplodedKeys
  length (explodedOperations compact) @?= 3
  map (projectedClaimedAt . explodedOperationProvenance) (explodedOperations compact) @?= [100, 200, 300]
  assertBool "compact raw omitted" (all (all ((== Nothing) . explodedItemRawSemantic) . explodedOperationItems) (explodedOperations compact))
  assertBool "raw semantic retained" (all (all ((/= Nothing) . explodedItemRawSemantic) . explodedOperationItems) (explodedOperations raw))
  let amendmentGroup = firstUnsafe (explodedOperations compact)
      decisionItems = filter ((== "decision") . explodedItemType) (explodedOperationItems amendmentGroup)
      diff = firstUnsafe (explodedItemDiffs (firstUnsafe decisionItems))
  parentDiffParent diff @?= r1
  assertBool "diff has true unified hunk" ("@@ -1" `Text.isInfixOf` parentDiffText diff)
  countText "@@ -" (parentDiffText diff) @?= 2
  assertBool "diff covers full semantic domains" ("domains =" `Text.isInfixOf` parentDiffText diff)
  assertBool
    "reordered lines use SequenceMatcher's earliest-old anchor"
    ("+Adopt the service architecture.\n Keep the legacy architecture.\n-Adopt the service architecture.\n" `Text.isInfixOf` parentDiffText diff)
  let rendered = TextEncoding.decodeUtf8 (renderExplodedProjection compact)
  assertBool "group provenance once" (countText "\"provenance\"" rendered == 3)
  assertBool "compact item provenance absent" (countText "\"provenance\"" rendered == length (explodedOperations compact))
  assertBool "full operation provenance retains events" ("\"claimed_ms\"" `Text.isInfixOf` rendered && "\"events\"" `Text.isInfixOf` rendered && "\"placements\"" `Text.isInfixOf` rendered)

prefixContract :: IO ()
prefixContract = do
  resolveAdrReference fullSnapshot (Text.toLower (adrIdText primaryAdr)) @?= Right primaryAdr
  resolveAdrReference fullSnapshot (Text.take 12 (recordIdText r2)) @?= Right primaryAdr
  resolveAdrReference fullSnapshot (Text.take 12 (connectionIdText c5)) @?= Right primaryAdr
  case resolveAdrReference fullSnapshot "O1234567" of Left (LookupWrongKind 'O') -> pure (); other -> fail (show other)
  let a = exactAdr "A22222222222222222222222222"
      b = exactAdr "A33333333333333333333333333"
      shared = exactRecord "R12345671111111111111111111"
      records = [decisionRecordFor a shared "first", decisionRecordFor b shared "second"]
      forward = snapshotFor "duplicates" records (documentsSimple records)
      reversed = forward {readSnapshotDocuments = reverse (readSnapshotDocuments forward), readSnapshotReduction = reduceManagedGraph (reverse records)}
  resolveAdrReference forward (recordIdText shared) @?= resolveAdrReference reversed (recordIdText shared)
  case resolveAdrReference forward (recordIdText shared) of
    Left (LookupAmbiguous _ matches) -> map referenceMatchAdr matches @?= [a, b]
    other -> fail ("expected duplicate-owner ambiguity, got " <> show other)

compareContract :: IO ()
compareContract = do
  let unchangedAdr = exactAdr "A22222222222222222222222222"
      unchangedRecord = exactRecord "R88888888888888888888888888"
      unchanged = decisionRecordFor unchangedAdr unchangedRecord "unchanged"
      beforeRecords = take 4 fullRecords <> [unchanged]
      afterRecords = fullRecords <> [unchanged]
      before = snapshotFor "before" beforeRecords (documentsSimple beforeRecords)
      after = snapshotFor "after" afterRecords (documentsSimple afterRecords)
      comparison = must (compareSnapshots (CompareOptions False [("mode", JsonString "pure")]) before after)
  objectKeys (compareProjectionJson comparison) @?= expectedCompareKeys
  compareChanged (compareCounts comparison) @?= 1
  compareUnchanged (compareCounts comparison) @?= 1
  length (compareEntries comparison) @?= 1
  let entry = firstUnsafe (compareEntries comparison)
  compareEntryTitle entry @?= "Caf\233 architecture v2"
  map compareChangeField (compareEntryChanges entry) @?= ["record", "title", "summary", "body", "record_heads", "status_heads"]
  let diffFor field = compareChangeDiff =<< findChange field (compareEntryChanges entry)
  assertBool "title diff has exact single-line hunk" (maybe False ("@@ -1 +1 @@" `Text.isInfixOf`) (diffFor "title"))
  assertBool "summary diff has exact single-line hunk" (maybe False ("@@ -1 +1 @@" `Text.isInfixOf`) (diffFor "summary"))
  assertBool "body diff has exact n=3 context range" (maybe False ("@@ -7,5 +7,5 @@" `Text.isInfixOf`) (diffFor "body"))
  let withUnchanged = must (compareSnapshots (CompareOptions True []) before after)
  map compareEntryAdr (compareEntries withUnchanged) @?= sort [primaryAdr, unchangedAdr]
  let snapshotKeys = firstCompareAfterKeys (compareProjectionJson withUnchanged)
  assertBool "heads are flat" (all (`elem` snapshotKeys) ["record_heads", "scope_heads", "domain_heads", "status_heads"])
  assertBool "no nested heads" ("heads" `notElem` snapshotKeys)

obsoleteVisibilityContract :: IO ()
obsoleteVisibilityContract = do
  let records = take 4 fullRecords <> [statusRevision c6 [c3] StatusObsolete [r1] Nothing]
      snapshot = snapshotFor "obsolete" records (documentsSimple records)
      reduced = mustJust (lookupReducedAdr primaryAdr (readSnapshotReduction snapshot))
      projection = must (projectCollapsed CompactProjection snapshot primaryAdr)
      history = must (projectHistory snapshot (Just primaryAdr) defaultHistoryOptions)
  adrVisible False reduced @?= False
  adrVisible True reduced @?= True
  collapsedObsolete projection @?= True
  assertBool "history retains obsoletion" ("obsoleted" `elem` map historyOperationLabel (historyProjectionOperations history))

permutationContract :: IO ()
permutationContract = do
  let reversed = fullSnapshot {readSnapshotDocuments = reverse (readSnapshotDocuments fullSnapshot), readSnapshotReduction = reduceManagedGraph (reverse fullRecords)}
      firstCollapsed = must (projectCollapsed RichProjection fullSnapshot primaryAdr)
      secondCollapsed = must (projectCollapsed RichProjection reversed primaryAdr)
      firstExploded = must (projectExploded (ExplodedOptions True) fullSnapshot primaryAdr)
      secondExploded = must (projectExploded (ExplodedOptions True) reversed primaryAdr)
      firstHistory = must (projectHistory fullSnapshot (Just primaryAdr) defaultHistoryOptions)
      secondHistory = must (projectHistory reversed (Just primaryAdr) defaultHistoryOptions)
      firstCompare = must (compareSnapshots (CompareOptions True []) fullSnapshot fullSnapshot)
      secondCompare = must (compareSnapshots (CompareOptions True []) reversed reversed)
  renderCollapsedProjection firstCollapsed @?= renderCollapsedProjection secondCollapsed
  renderExplodedProjection firstExploded @?= renderExplodedProjection secondExploded
  renderHistoryProjection firstHistory @?= renderHistoryProjection secondHistory
  renderCompareProjection firstCompare @?= renderCompareProjection secondCompare

publicKeyInventories :: JsonValue
publicKeyInventories =
  object
    [ ("collapsed", textArrayValue expectedCollapsedKeys),
      ("compare", textArrayValue expectedCompareKeys),
      ("exploded", textArrayValue expectedExplodedKeys),
      ("history", textArrayValue expectedHistoryKeys),
      ("resolution", textArrayValue ["conflicts", "resolution_required", "resolved"])
    ]

expectedCollapsedKeys, expectedExplodedKeys, expectedHistoryKeys, expectedCompareKeys :: [Text]
expectedCollapsedKeys = sort ["schema", "view", "as_of", "adr", "record", "title", "summary", "body", "domains", "applies_to", "status", "obsolete", "replacement", "resolved", "resolution_required", "resolution", "state_token", "counts", "evolution", "provenance", "issues"]
expectedExplodedKeys = sort ["schema", "view", "as_of", "adr", "resolved", "resolution_required", "resolution", "state_token", "operations"]
expectedHistoryKeys = sort ["schema", "view", "as_of", "adr", "order", "limit", "truncated", "filters", "operations"]
expectedCompareKeys = sort ["view", "from", "to", "from_requested", "to_requested", "counts", "entries", "cache"]

primaryAdr :: AdrId
primaryAdr = exactAdr "A11111111111111111111111111"

r1, r2 :: RecordId
r1 = exactRecord "R11111111111111111111111111"
r2 = exactRecord "R22222222222222222222222222"

c1, c2, c3, c4, c5, c6 :: ConnectionId
c1 = exactConnection "C11111111111111111111111111"
c2 = exactConnection "C22222222222222222222222222"
c3 = exactConnection "C33333333333333333333333333"
c4 = exactConnection "C44444444444444444444444444"
c5 = exactConnection "C55555555555555555555555555"
c6 = exactConnection "C66666666666666666666666666"

o1, o2, o3 :: OperationId
o1 = exactOperation "O11111111111111111111111111"
o2 = exactOperation "O22222222222222222222222222"
o3 = exactOperation "O33333333333333333333333333"

fullRecords :: [ManagedRecord]
fullRecords =
  [ ManagedDecision (DecisionRecord primaryAdr r1 "Caf\233 architecture v1" "Caf\233 architecture v1 summary" [domain "compiler"] (versionedDecisionBody beforeDecisionTail)),
    scopeInitial primaryAdr c1,
    domainInitial primaryAdr c2,
    statusRevision c3 [] StatusActive [r1] Nothing,
    ManagedDecision (DecisionRecord primaryAdr r2 "Caf\233 architecture v2" "new summary" [domain "compiler"] (versionedDecisionBody afterDecisionTail)),
    ManagedConnection (ConnectionRecord c4 (AmendsConnection (AmendsPayload primaryAdr r2 [r1])) "reviewed amendment\n"),
    statusRevision c5 [c3] StatusActive [r2] Nothing
  ]

fullSnapshot :: ReadSnapshot
fullSnapshot =
  ReadSnapshot
    { readSnapshotRevision = RevisionIdentity "refs/heads/main" "deadbeef",
      readSnapshotDocuments =
        [document o1 300 HumanActor "architect" [] record | record <- take 4 fullRecords]
          <> [document o2 100 LlmActor "reviewer" [ProvenanceRecord r1] record | record <- take 2 (drop 4 fullRecords)]
          <> [document o3 200 ServiceActor "service" [ProvenanceRecord r2, ProvenanceConnection c3] (lastUnsafe fullRecords)],
      readSnapshotReduction = reduceManagedGraph fullRecords,
      readSnapshotPlacement =
        Map.fromList
          [ (o1, PlacementEvidence (Just "commit-placement-1") (Just "original") [CommitPlacementEvidence "commit-placement-1" "original" True 250 300 "create ADR" []] ["commit-placement-1"] [] [LineLandingEvidence "trunk" "refs/heads/main" "landing-1" True]),
            (o2, PlacementEvidence (Just "commit-placement-2") (Just "introduction") [CommitPlacementEvidence "commit-placement-2" "introduction" True 90 100 "amend ADR" ["commit-placement-1"]] [] ["commit-placement-2"] []),
            (o3, PlacementEvidence Nothing Nothing [] [] [] [])
          ]
    }

conflictRecords :: AdrId -> [ManagedRecord]
conflictRecords adr =
  [ decisionRecordFor adr r1 "first",
    decisionRecordFor adr r2 "second",
    scopeInitial adr c1,
    domainInitial adr c2,
    statusRevision c3 [] StatusActive [r1, r2] Nothing
  ]

decisionRecordFor :: AdrId -> RecordId -> Text -> ManagedRecord
decisionRecordFor adr record title = ManagedDecision (DecisionRecord adr record title (title <> " summary") [domain "compiler"] ("# Decision\n" <> title <> "\n"))

versionedDecisionBody :: [Text] -> Text
versionedDecisionBody finalLines =
  Text.unlines
    ( ["# Decision"]
        <> ["Shared context " <> Text.pack (show index) | index <- [1 :: Int .. 8]]
        <> finalLines
    )

beforeDecisionTail, afterDecisionTail :: [Text]
beforeDecisionTail = ["Keep the legacy architecture.", "Adopt the service architecture."]
afterDecisionTail = reverse beforeDecisionTail

scopeInitial :: AdrId -> ConnectionId -> ManagedRecord
scopeInitial adr identifier = ManagedConnection (ConnectionRecord identifier (AppliesToConnection (AppliesToPayload adr [] "initial" [scope "src/**"] [] [scope "src/**"])) "initial scope\n")

domainInitial :: AdrId -> ConnectionId -> ManagedRecord
domainInitial adr identifier = ManagedConnection (ConnectionRecord identifier (DomainsConnection (DomainsPayload adr [] "initial" [domain "compiler"] [] [domain "compiler"] [])) "initial domain\n")

statusRevision :: ConnectionId -> [ConnectionId] -> StatusState -> [RecordId] -> Maybe AdrId -> ManagedRecord
statusRevision identifier parents state heads replacement = ManagedConnection (ConnectionRecord identifier (StatusConnection (StatusPayload primaryAdr parents state heads replacement)) "status update\n")

snapshotFor :: Text -> [ManagedRecord] -> [ParsedManagedDocument] -> ReadSnapshot
snapshotFor label records documents = ReadSnapshot (RevisionIdentity label (label <> "-resolved")) documents (reduceManagedGraph records) Map.empty

documentsSimple :: [ManagedRecord] -> [ParsedManagedDocument]
documentsSimple records =
  [document operation (fromIntegral index) HumanActor "fixture" [] record | (index, operation, record) <- zip3 [1 :: Int ..] operationIds records]
  where
    operationIds = map operationFromChar (take (length records) "123456789ABCDEFGHJKMNPQRSTVWXYZ")

operationFromChar :: Char -> OperationId
operationFromChar character = exactOperation ("O" <> Text.replicate 26 (Text.singleton character))

document :: OperationId -> Integer -> ActorKind -> Text -> [ProvenanceObjectId] -> ManagedRecord -> ParsedManagedDocument
document operation timestamp kind actorIdentifier parents record =
  ParsedManagedDocument
    { parsedManagedPath = exactPath ("fixtures/" <> provenanceObjectIdText objectId <> ".md"),
      parsedManagedRecord = record,
      parsedManagedCapsule = capsule,
      parsedManagedSemantic = semantic,
      parsedManagedBytes = TextEncoding.encodeUtf8 semantic
    }
  where
    objectId = managedObject record
    semantic = must (renderManagedSemantic record)
    capsule =
      must
        ( mkProvenanceCapsule
            ProvenanceCapsuleInput
              { capsuleInputOperationId = operation,
                capsuleInputObjectId = objectId,
                capsuleInputEventKind = must (mkEventKind (eventFor parents record)),
                capsuleInputActor = must (mkActor kind actorIdentifier Nothing),
                capsuleInputTimestampMs = timestamp,
                capsuleInputBasis = must (mkGitOid (Text.replicate 40 "a")),
                capsuleInputParents = parents,
                capsuleInputBranchHint = Nothing,
                capsuleInputUpstreamHint = Nothing,
                capsuleInputLineAnchors = [],
                capsuleInputSemanticDigest = semanticDigest semantic,
                capsuleInputToolVersion = "adrai/1.0.0",
                capsuleInputDigests = ProvenanceInputs Nothing Nothing Nothing
              }
        )

retimeDocument :: Integer -> ParsedManagedDocument -> ParsedManagedDocument
retimeDocument timestamp documentValue =
  documentValue {parsedManagedCapsule = retimed}
  where
    original = parsedManagedCapsule documentValue
    retimed =
      must
        ( mkProvenanceCapsule
            ProvenanceCapsuleInput
              { capsuleInputOperationId = provenanceOperationId original,
                capsuleInputObjectId = provenanceObjectId original,
                capsuleInputEventKind = provenanceEventKind original,
                capsuleInputActor = provenanceActor original,
                capsuleInputTimestampMs = timestamp,
                capsuleInputBasis = provenanceBasis original,
                capsuleInputParents = provenanceParents original,
                capsuleInputBranchHint = provenanceBranchHint original,
                capsuleInputUpstreamHint = provenanceUpstreamHint original,
                capsuleInputLineAnchors = provenanceLineAnchors original,
                capsuleInputSemanticDigest = provenanceSemanticDigest original,
                capsuleInputToolVersion = provenanceToolVersion original,
                capsuleInputDigests = provenanceInputs original
              }
        )

managedObject :: ManagedRecord -> ProvenanceObjectId
managedObject record = case record of ManagedDecision decision -> ProvenanceRecord (decisionRecord decision); ManagedConnection connection -> ProvenanceConnection (connectionRecordId connection)

eventFor :: [ProvenanceObjectId] -> ManagedRecord -> Text
eventFor parents record =
  case record of
    ManagedDecision _ -> if null parents then "decision.create" else "decision.amend"
    ManagedConnection connection ->
      case connectionPayload connection of
        AmendsConnection _ -> "connection.amends"
        AppliesToConnection payload -> "scope." <> appliesToChange payload
        DomainsConnection payload -> "domain." <> domainsChange payload
        StatusConnection payload
          | null (statusParentConnections payload) -> "status.initial"
          | statusState payload == StatusObsolete -> "decision.obsolete"
          | otherwise -> "decision.reactivate"

domain :: Text -> Domain
domain = must . mkDomain

scope :: Text -> ScopePattern
scope = must . mkScopePattern

exactAdr :: Text -> AdrId
exactAdr = must . mkAdrId

exactRecord :: Text -> RecordId
exactRecord = must . mkRecordId

exactConnection :: Text -> ConnectionId
exactConnection = must . mkConnectionId

exactOperation :: Text -> OperationId
exactOperation = must . mkOperationId

exactPath :: Text -> RepoPath
exactPath = must . mkRepoPath

must :: (Show error) => Either error value -> value
must result = case result of Right value -> value; Left problem -> error (show problem)

mustJust :: Maybe value -> value
mustJust value = case value of Just present -> present; Nothing -> error "expected Just"

firstUnsafe :: [value] -> value
firstUnsafe values = case values of value : _ -> value; [] -> error "expected non-empty list"

headUnsafe :: [value] -> value
headUnsafe = firstUnsafe

lastUnsafe :: [value] -> value
lastUnsafe values = case reverse values of value : _ -> value; [] -> error "expected non-empty list"

objectKeys :: JsonValue -> [Text]
objectKeys value = case value of JsonObject members -> sort (map fst members); _ -> []

nestedObjectKeys :: [Text] -> JsonValue -> [Text]
nestedObjectKeys keys value = objectKeys (foldl descend value keys)
  where
    descend current key =
      case current of
        JsonObject members -> maybe JsonNull id (lookup key members)
        _ -> JsonNull

firstCompareAfterKeys :: JsonValue -> [Text]
firstCompareAfterKeys value =
  case value of
    JsonObject root ->
      case lookup "entries" root of
        Just (JsonArray (JsonObject entry : _)) -> maybe [] objectKeys (lookup "after" entry)
        _ -> []
    _ -> []

findChange :: Text -> [CompareChange] -> Maybe CompareChange
findChange field changes =
  case filter ((== field) . compareChangeField) changes of
    change : _ -> Just change
    [] -> Nothing

textArrayValue :: [Text] -> JsonValue
textArrayValue = JsonArray . map JsonString

countText :: Text -> Text -> Int
countText needle = length . Text.breakOnAll needle
