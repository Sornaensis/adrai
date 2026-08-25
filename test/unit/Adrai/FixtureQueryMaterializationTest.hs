{-# LANGUAGE OverloadedStrings #-}

module Adrai.FixtureQueryMaterializationTest (tests) where

import Adrai.Fixture.QueryMaterialization
import Adrai.Fixture.Relevance (relevanceCorpusV1)
import Adrai.Fixture.Types
  ( AdrKey (..),
    AdrTemplate (..),
    RelevanceCorpus (..),
    SourceMode (..),
    SourceTemplate (..),
  )
import Adrai.Format.Document
  ( ParsedManagedDocument (..),
    validateManagedLocation,
  )
import Adrai.Graph (GraphReduction (..), lookupReducedAdr)
import Adrai.History
  ( ReadSnapshot (..),
    RevisionIdentity (..),
    validateReadSnapshot,
  )
import Adrai.Query (RelevantSource (..))
import Adrai.Retrieval
  ( SearchDocument (..),
    SearchMaterialization (..),
    SearchPassage (..),
  )
import Adrai.Types
  ( configManagedPaths,
    defaultConfig,
    mkRepoPath,
    repoPathText,
  )
import qualified Data.ByteString as ByteString
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( (@?=),
    assertBool,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "P3-06 query fixture materialization"
    [ testCase "six ADR templates materialize canonically and independently of input order" sixAdrContract,
      testCase "all relevance source templates preserve deterministic bytes and source semantics" sourceContract,
      testCase "historical fixtures accept committed sources and reject worktree sources" historicalSourceContract,
      testCase "duplicate and invalid fixture inputs fail with typed errors" typedFailureContract
    ]

sixAdrContract :: IO ()
sixAdrContract = do
  let corpus = relevanceCorpusV1
      forward = mustRight (materializeQueryFixture (relevanceCorpusMeta corpus) (relevanceCorpusAdrs corpus) (NonEmpty.toList (relevanceCorpusSources corpus)))
      reversedTemplates = NonEmpty.fromList (reverse (NonEmpty.toList (relevanceCorpusAdrs corpus)))
      reversedSources = reverse (NonEmpty.toList (relevanceCorpusSources corpus))
      reversed = mustRight (materializeQueryFixture (relevanceCorpusMeta corpus) reversedTemplates reversedSources)
      snapshot = queryMaterializationSnapshot forward
      search = queryMaterializationSearch forward
      documents = searchMaterializationDocuments search
      passages = searchMaterializationPassages search
  forward @?= reversed
  renderedFixtureBytes forward @?= renderedFixtureBytes reversed
  assertBool "the rendered fixture is nonempty" (not (ByteString.null (renderedFixtureBytes forward)))
  Map.size (queryMaterializationAdrIds forward) @?= 6
  length (readSnapshotDocuments snapshot) @?= 24
  graphReductionIssues (readSnapshotReduction snapshot) @?= []
  validateReadSnapshot snapshot @?= Right ()
  mapM_ (\document -> validateManagedLocation (configManagedPaths defaultConfig) document @?= Right ()) (readSnapshotDocuments snapshot)
  length documents @?= 6
  assertUnique "search item IDs" (map searchDocumentItemId documents)
  assertUnique "passage IDs" (map searchPassageId passages)
  assertBool
    "every passage refers to a materialized search item"
    ( all
        (\passage -> searchPassageDocumentItemId passage `Set.member` Set.fromList (map searchDocumentItemId documents))
        passages
    )
  mapM_
    (\key -> do
       adr <- mustRightIo (lookupQueryMaterializationAdr key forward)
       assertBool "mapped ADR is present in the graph reduction" (maybe False (const True) (lookupReducedAdr adr (readSnapshotReduction snapshot)))
    )
    (map adrTemplateKey (NonEmpty.toList (relevanceCorpusAdrs corpus)))

sourceContract :: IO ()
sourceContract = do
  let corpus = relevanceCorpusV1
      materialization = mustRight (materializeQueryFixture (relevanceCorpusMeta corpus) (relevanceCorpusAdrs corpus) allSources)
      revision = readSnapshotRevision (queryMaterializationSnapshot materialization)
  length sources @?= 13
  Map.size (queryMaterializationSources materialization) @?= length allSources
  mapM_ (assertSource revision materialization) allSources
  where
    sources = NonEmpty.toList (relevanceCorpusSources relevanceCorpusV1)
    allSources =
      sources
        <> [ syntheticSource "synthetic-worktree" WorktreeSource,
             syntheticSource "synthetic-dirty" DirtyWorktreeSource,
             syntheticSource "synthetic-untracked" UntrackedWorktreeSource
           ]

historicalSourceContract :: IO ()
historicalSourceContract = do
  let corpus = relevanceCorpusV1
      committedSource = NonEmpty.head (relevanceCorpusSources corpus)
      historical =
        mustRight
          (materializeQueryFixtureAt "fixture-history" (relevanceCorpusMeta corpus) (relevanceCorpusAdrs corpus) [committedSource])
      revision = readSnapshotRevision (queryMaterializationSnapshot historical)
  revisionRequested revision @?= "fixture-history"
  case lookupQueryMaterializationSource (sourceTemplateKey committedSource) historical of
    Right (RevisionRelevantSource _ resolved blob bytes) -> do
      resolved @?= revisionResolved revision
      assertBool "historical committed source has a blob identity" (not (Text.null blob))
      bytes @?= sourceTemplateBytes committedSource
    Right _ -> assertFailure "historical committed source became a worktree source"
    Left problem -> assertFailure (show problem)
  assertLeft
    "historical worktree source"
    isUnsupportedSourceMode
    ( materializeQueryFixtureAt
        "fixture-history"
        (relevanceCorpusMeta corpus)
        (relevanceCorpusAdrs corpus)
        [syntheticSource "historical-worktree" WorktreeSource]
    )

typedFailureContract :: IO ()
typedFailureContract = do
  let corpus = relevanceCorpusV1
      templates = relevanceCorpusAdrs corpus
      firstTemplate = NonEmpty.head templates
      firstSource = NonEmpty.head (relevanceCorpusSources corpus)
      noSources = []
  assertLeft
    "duplicate AdrKey"
    isDuplicateAdr
    (materializeQueryFixture (relevanceCorpusMeta corpus) (firstTemplate NonEmpty.:| [firstTemplate]) noSources)
  assertLeft
    "duplicate source key"
    isDuplicateSource
    (materializeQueryFixture (relevanceCorpusMeta corpus) templates [firstSource, firstSource])
  assertLeft
    "invalid generated decision"
    isInvalidRecord
    (materializeQueryFixture (relevanceCorpusMeta corpus) (firstTemplate {adrTemplateTitle = ""} NonEmpty.:| []) noSources)
  assertLeft
    "negative AdrKey"
    isInvalidAdrKey
    (materializeQueryFixture (relevanceCorpusMeta corpus) (firstTemplate {adrTemplateKey = AdrKey (-1)} NonEmpty.:| []) noSources)
  assertLeft
    "invalid domain"
    isInvalidDomain
    (materializeQueryFixture (relevanceCorpusMeta corpus) (firstTemplate {adrTemplateDomains = ["not a domain"]} NonEmpty.:| []) noSources)
  assertLeft
    "invalid scope"
    isInvalidScope
    (materializeQueryFixture (relevanceCorpusMeta corpus) (firstTemplate {adrTemplateScopes = ["../escape"]} NonEmpty.:| []) noSources)
  assertLeft
    "invalid source path"
    isInvalidSourcePath
    (materializeQueryFixture (relevanceCorpusMeta corpus) templates [firstSource {sourceTemplatePath = "../escape"}])
  let valid = mustRight (materializeQueryFixture (relevanceCorpusMeta corpus) templates noSources)
  lookupQueryMaterializationAdr (AdrKey 999999) valid @?= Left (QueryMaterializationMissingAdr (AdrKey 999999))
  lookupQueryMaterializationSource "missing" valid @?= Left (QueryMaterializationMissingSource "missing")

assertSource :: RevisionIdentity -> QueryMaterialization -> SourceTemplate -> IO ()
assertSource revision materialization template =
  case lookupQueryMaterializationSource (sourceTemplateKey template) materialization of
    Left problem -> assertFailure ("source lookup failed: " <> show problem)
    Right source -> do
      relevantSourcePath source @?= mustRight (mkRepoPath (sourceTemplatePath template))
      relevantSourceBytes source @?= sourceTemplateBytes template
      case (sourceTemplateMode template, source) of
        (CommittedSource, RevisionRelevantSource _ resolved blob _) -> do
          resolved @?= revisionResolved revision
          assertBool "committed source has a deterministic nonempty blob identity" (not (Text.null blob))
        (CommittedSource, _) -> assertFailure "committed fixture source became a worktree source"
        (_, WorktreeRelevantSource _ headRevision _) -> headRevision @?= revisionResolved revision
        (_, _) -> assertFailure "worktree fixture source became a revision source"

renderedFixtureBytes :: QueryMaterialization -> ByteString.ByteString
renderedFixtureBytes materialization =
  ByteString.concat
    [ TextEncoding.encodeUtf8 (repoPathText (parsedManagedPath document))
        <> "\NUL"
        <> parsedManagedBytes document
        <> "\NUL"
      | document <- readSnapshotDocuments (queryMaterializationSnapshot materialization)
    ]

assertUnique :: (Ord value, Show value) => String -> [value] -> IO ()
assertUnique label values =
  assertBool (label <> " were not unique: " <> show values) (Set.size (Set.fromList values) == length values)

assertLeft :: String -> (problem -> Bool) -> Either problem value -> IO ()
assertLeft label predicate result =
  case result of
    Left problem -> assertBool (label <> " returned the wrong typed error") (predicate problem)
    Right _ -> assertFailure (label <> " unexpectedly succeeded")

isDuplicateAdr :: QueryMaterializationError -> Bool
isDuplicateAdr problem =
  case problem of
    QueryMaterializationDuplicateAdrKey _ -> True
    _ -> False

isDuplicateSource :: QueryMaterializationError -> Bool
isDuplicateSource problem =
  case problem of
    QueryMaterializationDuplicateSourceKey _ -> True
    _ -> False

isInvalidAdrKey :: QueryMaterializationError -> Bool
isInvalidAdrKey problem =
  case problem of
    QueryMaterializationInvalidAdrKey _ -> True
    _ -> False

isInvalidRecord :: QueryMaterializationError -> Bool
isInvalidRecord problem =
  case problem of
    QueryMaterializationInvalidRecord _ _ _ -> True
    _ -> False

isInvalidDomain :: QueryMaterializationError -> Bool
isInvalidDomain problem =
  case problem of
    QueryMaterializationInvalidDomain _ _ _ -> True
    _ -> False

isInvalidScope :: QueryMaterializationError -> Bool
isInvalidScope problem =
  case problem of
    QueryMaterializationInvalidScope _ _ _ -> True
    _ -> False

isInvalidSourcePath :: QueryMaterializationError -> Bool
isInvalidSourcePath problem =
  case problem of
    QueryMaterializationInvalidSourcePath _ _ _ -> True
    _ -> False

isUnsupportedSourceMode :: QueryMaterializationError -> Bool
isUnsupportedSourceMode problem =
  case problem of
    QueryMaterializationUnsupportedSourceModeRevision _ _ _ -> True
    _ -> False

mustRight :: (Show problem) => Either problem value -> value
mustRight result =
  case result of
    Right value -> value
    Left problem -> error (show problem)

mustRightIo :: (Show problem) => Either problem value -> IO value
mustRightIo result =
  case result of
    Right value -> pure value
    Left problem -> assertFailure (show problem) >> error "unreachable"

syntheticSource :: Text.Text -> SourceMode -> SourceTemplate
syntheticSource key mode =
  SourceTemplate
    { sourceTemplateKey = key,
      sourceTemplatePath = "synthetic/" <> key <> ".txt",
      sourceTemplateBytes = TextEncoding.encodeUtf8 (key <> "\n"),
      sourceTemplateMode = mode
    }
