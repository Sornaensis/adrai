{-# LANGUAGE OverloadedStrings #-}

module Adrai.FixtureContractTest (tests) where

import Adrai.Fixture.LargeStress
  ( largeStressDocumentCountsV1,
    largeStressV1,
  )
import Adrai.Fixture.Prng (algorithmName, seedWord64)
import Adrai.Fixture.ProductionShape
  ( DocumentCounts (..),
    foldRepositoryPlan,
    productionDocumentCountsV1,
    productionShapeV1,
  )
import Adrai.Fixture.Relevance
  ( relevanceCorpusNamespace,
    relevanceCorpusV1,
    relevanceTopK,
  )
import Adrai.Fixture.RetrievalScale
  ( adrTemplateAt,
    retrievalOperationCountsV1,
    retrievalScaleV1,
    retrievalTopicCountsV1,
  )
import Adrai.Fixture.Types
  ( AdrKey (..),
    AdrTemplate (adrTemplateKey, adrTemplateTitle),
    BranchKey (..),
    CommitOrdinal (..),
    FixtureMeta (..),
    FixtureVersion (..),
    OperationCounts (..),
    OperationKey (..),
    PlannedCommit (..),
    PlannedCommitKind (..),
    PlannedOperation (..),
    RelevanceCorpus (..),
    RepositoryPlan (..),
    RepositorySpec (..),
    RetrievalScaleSpec (..),
  )
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TextEncoding
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "fixture contract"
    [ testCase "canonical compact summary is exact" $
        canonicalFixtureSummary @?= expectedFixtureSummary,
      testCase "canonical compact summary SHA-256 is stable" $
        sha256Text canonicalFixtureSummary @?= expectedFixtureSummarySha256
    ]

canonicalFixtureSummary :: BS.ByteString
canonicalFixtureSummary =
  TextEncoding.encodeUtf8 . T.unlines $
    [ "fixture-contract=" <> renderVersion (fixtureVersion productionMeta) <> "|generator=" <> algorithmName <> "|seed=" <> natural (seedWord64 (fixtureSeed productionMeta)),
      planSummary "production" productionShapeV1 productionDocumentCountsV1,
      planSummary "large" largeStressV1 largeStressDocumentCountsV1,
      retrievalSummary,
      relevanceSummary
    ]
  where
    productionMeta = repositoryPlanMeta productionShapeV1

planSummary :: Text -> RepositoryPlan -> DocumentCounts -> Text
planSummary name plan documents =
  T.intercalate
    "|"
    [ name,
      "commits=" <> decimal (repositoryCommitCount specification),
      "operations=" <> operationCountsSummary (repositoryOperationCounts specification),
      "noise=" <> decimal (repositoryNoiseCommitCount specification),
      "documents=" <> documentCountsSummary documents,
      "first=" <> maybe "none" plannedCommitSummary firstCommit,
      "last=" <> maybe "none" plannedCommitSummary lastCommit
    ]
  where
    specification = repositoryPlanSpec plan
    (firstCommit, lastCommit) = planEndpoints plan

retrievalSummary :: Text
retrievalSummary =
  T.intercalate
    "|"
    [ "retrieval",
      "generator=" <> fixtureGenerator metadata,
      "seed=" <> natural (seedWord64 (fixtureSeed metadata)),
      "logical-adrs=" <> decimal (retrievalLogicalAdrCount retrievalScaleV1),
      "operations=" <> operationCountsSummary retrievalOperationCountsV1,
      "shortlist=" <> decimal (retrievalShortlistBound retrievalScaleV1),
      "probes=" <> decimal (NonEmpty.length (retrievalQueryProbes retrievalScaleV1)),
      "topics=" <> T.intercalate "," (map renderTopic retrievalTopicCountsV1),
      "first=" <> maybe "missing" adrTemplateTitle (adrTemplateAt 0),
      "last=" <> maybe "missing" adrTemplateTitle (adrTemplateAt 1999)
    ]
  where
    metadata = retrievalScaleMeta retrievalScaleV1
    renderTopic (topic, count) = topic <> ":" <> decimal count

relevanceSummary :: Text
relevanceSummary =
  T.intercalate
    "|"
    [ "relevance",
      "namespace=" <> relevanceCorpusNamespace,
      "generator=" <> fixtureGenerator metadata,
      "seed=" <> natural (seedWord64 (fixtureSeed metadata)),
      "adrs=" <> decimal (NonEmpty.length (relevanceCorpusAdrs relevanceCorpusV1)),
      "sources=" <> decimal (NonEmpty.length (relevanceCorpusSources relevanceCorpusV1)),
      "expectations=" <> decimal (NonEmpty.length (relevanceCorpusExpectations relevanceCorpusV1)),
      "top-k=" <> decimal relevanceTopK
    ]
  where
    metadata = relevanceCorpusMeta relevanceCorpusV1

planEndpoints :: RepositoryPlan -> (Maybe PlannedCommit, Maybe PlannedCommit)
planEndpoints = foldRepositoryPlan collect (Nothing, Nothing)
  where
    collect (Nothing, _) commit = (Just commit, Just commit)
    collect result@((Just _), _) commit = (fst result, Just commit)

plannedCommitSummary :: PlannedCommit -> Text
plannedCommitSummary commit =
  ordinal
    <> "@"
    <> branch
    <> "<-"
    <> parents
    <> ":"
    <> plannedKindSummary (plannedCommitKind commit)
  where
    CommitOrdinal ordinalValue = plannedCommitOrdinal commit
    BranchKey branch = plannedCommitBranch commit
    ordinal = decimal ordinalValue
    parents =
      case plannedCommitParents commit of
        [] -> "none"
        values -> T.intercalate "," (map renderCommitOrdinal values)

plannedKindSummary :: PlannedCommitKind -> Text
plannedKindSummary kind =
  case kind of
    PlannedNoise label -> "noise:" <> label
    PlannedSemantic (OperationKey key) operation ->
      "semantic:" <> decimal key <> ":" <> plannedOperationSummary operation
    PlannedMerge (BranchKey branch) -> "merge:" <> branch

plannedOperationSummary :: PlannedOperation -> Text
plannedOperationSummary operation =
  case operation of
    PlannedCreate template -> "create:" <> renderAdrKey (adrTemplateKey template)
    PlannedAmend key _ -> "amend:" <> renderAdrKey key
    PlannedScope key _ -> "scope:" <> renderAdrKey key
    PlannedDomain key _ -> "domain:" <> renderAdrKey key
    PlannedObsolete key -> "obsolete:" <> renderAdrKey key

operationCountsSummary :: OperationCounts -> Text
operationCountsSummary counts =
  T.intercalate
    ","
    ( map
        decimal
        [ createOperationCount counts,
          amendOperationCount counts,
          scopeOperationCount counts,
          domainOperationCount counts,
          obsoleteOperationCount counts
        ]
    )

documentCountsSummary :: DocumentCounts -> Text
documentCountsSummary counts =
  T.intercalate
    ","
    ( map
        decimal
        [ decisionDocumentCount counts,
          connectionDocumentCount counts,
          semanticDocumentCount counts
        ]
    )

renderVersion :: FixtureVersion -> Text
renderVersion FixtureV1 = "v1"

renderAdrKey :: AdrKey -> Text
renderAdrKey (AdrKey key) = decimal key

renderCommitOrdinal :: CommitOrdinal -> Text
renderCommitOrdinal (CommitOrdinal ordinal) = decimal ordinal

decimal :: Int -> Text
decimal = T.pack . show

natural :: (Show value) => value -> Text
natural = T.pack . show

sha256Text :: BS.ByteString -> Text
sha256Text bytes = T.pack (show (hash bytes :: Digest SHA256))

expectedFixtureSummary :: BS.ByteString
expectedFixtureSummary =
  BC.pack . unlines $
    [ "fixture-contract=v1|generator=adrai-fixture-splitmix64/v1|seed=260729",
      "production|commits=2000|operations=200,100,50,25,25|noise=1600|documents=300,800,1100|first=1@main<-none:noise:adrai-fixture-splitmix64/v1/noise-1-9250928540916208209|last=2000@main<-1999:semantic:400:obsolete:88",
      "large|commits=12000|operations=900,650,250,100,100|noise=10000|documents=1550,3800,5350|first=1@main<-none:noise:adrai-fixture-splitmix64/v1/noise-1-9250928540916208209|last=12000@main<-11999:semantic:2000:obsolete:63",
      "retrieval|generator=adrai-indexed-retrieval/v1|seed=0|logical-adrs=2000|operations=2000,4,4,0,2|shortlist=120|probes=4|topics=cache identity:667,trace propagation:667,job delivery:666|first=Decision 00: cache identity|last=Decision 1999: trace propagation",
      "relevance|namespace=adrai-relevance-six:v1|generator=adrai-fixture-splitmix64/v1|seed=260729|adrs=6|sources=13|expectations=13|top-k=3"
    ]

expectedFixtureSummarySha256 :: Text
expectedFixtureSummarySha256 = "42b86f764b1d3d171a2c7877a71207af41745c5e293609b42f23540d21d068f2"
