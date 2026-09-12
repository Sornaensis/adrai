{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Adrai.Benchmark.CurrentSearch
  ( CurrentSearchFixture,
    closeCurrentSearchFixture,
    currentSearchCold,
    currentSearchWarm,
    forceCurrentSearchFixture,
    prepareCurrentSearchFixture,
    scaleSearchInputs,
  )
import Adrai.Fixture.QueryMaterialization
  ( QueryMaterialization (..),
    lookupQueryMaterializationSource,
    materializeQueryFixture,
  )
import Adrai.Fixture.Relevance (relevanceCorpusV1)
import Adrai.Fixture.Types
  ( RelevanceCorpus (..),
  )
import Adrai.History (ReadSnapshot)
import Adrai.Query
  ( RelevantRequest,
    RelevantSource (..),
    defaultRelevantRequest,
    relevantRequestLimit,
    renderRelevantProjection,
    runRelevant,
    runRelevantWithCorpus,
  )
import Adrai.Retrieval
  ( SearchDocument (..),
    SearchMaterialization (..),
    SearchPassage (..),
  )
import Adrai.SearchVectorCorpus
  ( SearchVectorCorpus,
    buildSearchVectorCorpus,
    searchVectorCorpusFingerprint,
    searchVectorCorpusIdentifierVectors,
    searchVectorCorpusSectionVectors,
    searchVectorCorpusSummaryVectors,
  )
import Adrai.Sqlite (initializeSearchSchema, replaceSearchMaterialization)
import Adrai.Vector (DenseVector, dot)
import Control.DeepSeq (NFData (rnf))
import Control.Exception (onException)
import Criterion.Main (bench, bgroup, defaultMain, envWithCleanup, nf, nfIO, whnfIO)
import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Database.SQLite.Simple (Connection, close, open)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

data CorpusFixture = CorpusFixture
  { corpusFixtureMaterialization :: SearchMaterialization
  }

data SqliteFixture = SqliteFixture
  { sqliteFixtureConnection :: Connection,
    sqliteFixtureMaterialization :: SearchMaterialization
  }

newtype CurrentSearchBenchmarkFixture = CurrentSearchBenchmarkFixture
  { unCurrentSearchBenchmarkFixture :: CurrentSearchFixture
  }

data RelevanceFixture = RelevanceFixture
  { relevanceConnection :: Connection,
    relevanceSnapshot :: ReadSnapshot,
    relevanceMaterialization :: SearchMaterialization,
    relevanceRequest :: RelevantRequest,
    relevanceSource :: RelevantSource,
    relevanceCorpus :: Maybe SearchVectorCorpus
  }

-- Criterion forces environments before timing. These instances deliberately
-- force deterministic fixture inputs and unboxed corpus values without
-- materializing dense vectors as boxed lists.
instance NFData CorpusFixture where
  rnf fixture = materializationChecksum (corpusFixtureMaterialization fixture) `seq` ()

instance NFData SqliteFixture where
  rnf fixture = materializationChecksum (sqliteFixtureMaterialization fixture) `seq` ()

instance NFData CurrentSearchBenchmarkFixture where
  rnf fixture = forceCurrentSearchFixture (unCurrentSearchBenchmarkFixture fixture) `seq` ()

instance NFData RelevanceFixture where
  rnf fixture =
    materializationChecksum (relevanceMaterialization fixture)
      + maybe 0 corpusChecksum (relevanceCorpus fixture)
      `seq` ()

-- Criterion renders its human-readable report with the Unicode micro sign.
-- Use UTF-8 before Criterion touches stdout or stderr so Windows console code
-- pages cannot turn an otherwise completed profiling run into failure.
main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain
    [ bgroup
        "adrai"
        [ bgroup
            "corpus"
            [ envWithCleanup
                prepareCorpusFixture
                discardFixture
                (\fixture -> bench "construction-2000-adr" (nf (forceCorpus . buildSearchVectorCorpus) (corpusFixtureMaterialization fixture)))
            ],
          bgroup
            "sqlite"
            [ envWithCleanup
                prepareSqliteFixture
                closeSqliteFixture
                (\fixture -> bench "replacement-warm-2000-adr" (whnfIO (sqliteReplacement fixture)))
            ],
          bgroup
            "search"
            [ envWithCleanup
                (CurrentSearchBenchmarkFixture <$> prepareCurrentSearchFixture False)
                (closeCurrentSearchFixture . unCurrentSearchBenchmarkFixture)
                (\fixture -> bench "current-cold-2000-adr" (nfIO (currentSearchCold (unCurrentSearchBenchmarkFixture fixture)))),
              envWithCleanup
                (CurrentSearchBenchmarkFixture <$> prepareCurrentSearchFixture True)
                (closeCurrentSearchFixture . unCurrentSearchBenchmarkFixture)
                (\fixture -> bench "current-warm-2000-adr" (nfIO (currentSearchWarm (unCurrentSearchBenchmarkFixture fixture))))
            ],
          bgroup
            "relevance"
            [ envWithCleanup
                (prepareRelevanceFixture False)
                closeRelevanceFixture
                (\fixture -> bench "cold-six-adr" (nfIO (relevanceSearchCold fixture))),
              envWithCleanup
                (prepareRelevanceFixture True)
                closeRelevanceFixture
                (\fixture -> bench "warm-six-adr" (nfIO (relevanceSearchWarm fixture)))
            ]
        ]
    ]

prepareCorpusFixture :: IO CorpusFixture
prepareCorpusFixture = CorpusFixture <$> scaleMaterialization

prepareSqliteFixture :: IO SqliteFixture
prepareSqliteFixture = do
  materialization <- scaleMaterialization
  connection <- open ":memory:"
  ( do
      requireRight "2,000-ADR SQLite schema" =<< initializeSearchSchema connection
      requireRight "2,000-ADR SQLite materialization" =<< replaceSearchMaterialization connection materialization
      pure (SqliteFixture connection materialization)
    ) `onException` close connection

prepareRelevanceFixture :: Bool -> IO RelevanceFixture
prepareRelevanceFixture reuseCorpus = do
  (snapshot, materialization, request, source) <- relevanceSearchInputs
  connection <- open ":memory:"
  ( do
      requireRight "six-ADR SQLite schema" =<< initializeSearchSchema connection
      requireRight "six-ADR SQLite materialization" =<< replaceSearchMaterialization connection materialization
      corpus <-
        if reuseCorpus
          then Just <$> requireRight "six-ADR vector corpus" (buildSearchVectorCorpus materialization)
          else pure Nothing
      pure
        RelevanceFixture
          { relevanceConnection = connection,
            relevanceSnapshot = snapshot,
            relevanceMaterialization = materialization,
            relevanceRequest = request,
            relevanceSource = source,
            relevanceCorpus = corpus
          }
    ) `onException` close connection

scaleMaterialization :: IO SearchMaterialization
scaleMaterialization = do
  (_, materialization, _) <- scaleSearchInputs
  pure materialization

relevanceSearchInputs :: IO (ReadSnapshot, SearchMaterialization, RelevantRequest, RelevantSource)
relevanceSearchInputs = do
  fixture <-
    requireRight
      "six-ADR fixture materialization"
      ( materializeQueryFixture
          (relevanceCorpusMeta relevanceCorpusV1)
          (relevanceCorpusAdrs relevanceCorpusV1)
          (NonEmpty.toList (relevanceCorpusSources relevanceCorpusV1))
      )
  source <- requireRight "cache relevance source" (lookupQueryMaterializationSource "cache-python" fixture)
  pure
    ( queryMaterializationSnapshot fixture,
      queryMaterializationSearch fixture,
      (defaultRelevantRequest (relevantSourcePath source)) {relevantRequestLimit = 3},
      source
    )

sqliteReplacement :: SqliteFixture -> IO ()
sqliteReplacement fixture =
  requireRight "2,000-ADR SQLite replacement"
    =<< replaceSearchMaterialization
      (sqliteFixtureConnection fixture)
      (sqliteFixtureMaterialization fixture)

relevanceSearchCold :: RelevanceFixture -> IO ByteString
relevanceSearchCold fixture = do
  result <-
    runRelevant
      (relevanceConnection fixture)
      (relevanceSnapshot fixture)
      (relevanceMaterialization fixture)
      (relevanceRequest fixture)
      (relevanceSource fixture)
  renderRelevantProjection <$> requireRight "six-ADR cold relevance search" result

relevanceSearchWarm :: RelevanceFixture -> IO ByteString
relevanceSearchWarm fixture = do
  corpus <- requireJust "six-ADR reusable vector corpus" (relevanceCorpus fixture)
  result <-
    runRelevantWithCorpus
      (relevanceConnection fixture)
      (relevanceSnapshot fixture)
      (relevanceMaterialization fixture)
      corpus
      (relevanceRequest fixture)
      (relevanceSource fixture)
  renderRelevantProjection <$> requireRight "six-ADR warm relevance search" result

-- | Force every dense vector through the unboxed implementation used by the
-- production scorer. 'dot vector vector' avoids a benchmark-only conversion
-- to boxed lists while still demanding every coordinate.
forceCorpus :: Either problem SearchVectorCorpus -> Int
forceCorpus result =
  case result of
    Left _ -> error "benchmark corpus construction failed"
    Right corpus -> corpusChecksum corpus

corpusChecksum :: SearchVectorCorpus -> Int
corpusChecksum corpus =
  Text.length (searchVectorCorpusFingerprint corpus)
    + vectorMapChecksum (searchVectorCorpusSummaryVectors corpus)
    + vectorMapChecksum (searchVectorCorpusIdentifierVectors corpus)
    + vectorMapChecksum (searchVectorCorpusSectionVectors corpus)

vectorMapChecksum :: Map.Map key DenseVector -> Int
vectorMapChecksum = Map.foldl' addVector 0
  where
    addVector total vector =
      case dot vector vector of
        Left _ -> error "benchmark vector checksum failed"
        Right squaredLength -> total + round squaredLength

materializationChecksum :: SearchMaterialization -> Int
materializationChecksum materialization =
  foldl' (\total document -> total + documentChecksum document) 0 (searchMaterializationDocuments materialization)
    + foldl' (\total passage -> total + passageChecksum passage) 0 (searchMaterializationPassages materialization)

documentChecksum :: SearchDocument -> Int
documentChecksum document =
  sum
    [ textLength (searchDocumentItemId document),
      textLength (searchDocumentTitle document),
      textLength (searchDocumentSummary document),
      textLength (searchDocumentDecision document),
      textLength (searchDocumentRationale document),
      textLength (searchDocumentIdentifierSource document)
    ]
    + foldl' (\total domain -> total + textLength domain) 0 (searchDocumentDomains document)

passageChecksum :: SearchPassage -> Int
passageChecksum passage = textLength (searchPassageId passage) + textLength (searchPassageText passage)

textLength :: Text.Text -> Int
textLength = Text.length

discardFixture :: value -> IO ()
discardFixture _ = pure ()

closeSqliteFixture :: SqliteFixture -> IO ()
closeSqliteFixture = close . sqliteFixtureConnection

closeRelevanceFixture :: RelevanceFixture -> IO ()
closeRelevanceFixture = close . relevanceConnection

requireJust :: String -> Maybe value -> IO value
requireJust label value =
  case value of
    Nothing -> ioError (userError (label <> " was not prepared"))
    Just result -> pure result

requireRight :: Show problem => String -> Either problem value -> IO value
requireRight label result =
  case result of
    Left problem -> ioError (userError (label <> ": " <> show problem))
    Right value -> pure value
