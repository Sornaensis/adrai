module Adrai.Benchmark.CurrentSearch
  ( CurrentSearchFixture,
    closeCurrentSearchFixture,
    currentSearchCold,
    currentSearchWarm,
    forceCurrentSearchFixture,
    prepareCurrentSearchFixture,
    scaleSearchInputs,
    withCurrentSearchWarmFixture,
  )
where

import Adrai.Fixture.QueryMaterialization
  ( QueryMaterialization (..),
    materializeQueryFixture,
  )
import Adrai.Fixture.RetrievalScale
  ( adrTemplates,
    retrievalScaleV1,
    retrievalTailProbeV1,
  )
import Adrai.Fixture.Types (RetrievalProbe (..), RetrievalScaleSpec (..))
import Adrai.History (ReadSnapshot)
import Adrai.Query
  ( SearchRequest,
    defaultSearchRequest,
    renderSearchProjection,
    runCurrentSearch,
    runCurrentSearchWithCorpus,
    searchRequestLimit,
  )
import Adrai.Retrieval (SearchDocument (..), SearchMaterialization (..), SearchPassage (..))
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
import Control.Exception (bracket, evaluate, onException)
import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Database.SQLite.Simple (Connection, close, open)

-- | The fixed 2,000-ADR current-search state shared by Criterion and the
-- profiling driver.  Preparing this fixture creates the SQLite indexes and,
-- for a warm run, the reusable vector corpus before the measured action.
data CurrentSearchFixture = CurrentSearchFixture
  { currentSearchConnection :: Connection,
    currentSearchSnapshot :: ReadSnapshot,
    currentSearchMaterialization :: SearchMaterialization,
    currentSearchRequest :: SearchRequest,
    currentSearchCorpus :: Maybe SearchVectorCorpus
  }

prepareCurrentSearchFixture :: Bool -> IO CurrentSearchFixture
prepareCurrentSearchFixture reuseCorpus = do
  (snapshot, materialization, request) <- scaleSearchInputs
  connection <- open ":memory:"
  ( do
      requireRight "2,000-ADR SQLite schema" =<< initializeSearchSchema connection
      requireRight "2,000-ADR SQLite materialization" =<< replaceSearchMaterialization connection materialization
      corpus <-
        if reuseCorpus
          then Just <$> requireRight "2,000-ADR vector corpus" (buildSearchVectorCorpus materialization)
          else pure Nothing
      let preparedFixture =
            CurrentSearchFixture
              { currentSearchConnection = connection,
                currentSearchSnapshot = snapshot,
                currentSearchMaterialization = materialization,
                currentSearchRequest = request,
                currentSearchCorpus = corpus
              }
      -- This is the single fixture-ready boundary shared by Criterion's
      -- environment forcing and the profile driver.  In particular, a warm
      -- corpus has no remaining lazy vector construction before either path
      -- begins its action.
      _ <- evaluate (forceCurrentSearchFixture preparedFixture)
      pure preparedFixture
    ) `onException` close connection

-- | Run one warm production action after the caller-owned setup phase.  The
-- bracket makes the setup/action boundary explicit and closes the in-memory
-- database even if profiling or rendering raises an exception.
withCurrentSearchWarmFixture :: (CurrentSearchFixture -> IO result) -> IO result
withCurrentSearchWarmFixture = bracket (prepareCurrentSearchFixture True) closeCurrentSearchFixture

closeCurrentSearchFixture :: CurrentSearchFixture -> IO ()
closeCurrentSearchFixture = close . currentSearchConnection

-- | Fully demand the deterministic state needed by a current-search action.
-- Criterion uses this as its fixture NFData boundary, while the profile driver
-- receives an already forced fixture from 'prepareCurrentSearchFixture'.
forceCurrentSearchFixture :: CurrentSearchFixture -> Int
forceCurrentSearchFixture fixture =
  materializationChecksum (currentSearchMaterialization fixture)
    + maybe 0 corpusChecksum (currentSearchCorpus fixture)

scaleSearchInputs :: IO (ReadSnapshot, SearchMaterialization, SearchRequest)
scaleSearchInputs = do
  fixture <-
    requireRight
      "2,000-ADR fixture materialization"
      ( materializeQueryFixture
          (retrievalScaleMeta retrievalScaleV1)
          (case NonEmpty.nonEmpty adrTemplates of
             Nothing -> error "2,000-ADR fixture had no templates"
             Just templates -> templates)
          []
      )
  let probe = retrievalTailProbeV1
  pure
    ( queryMaterializationSnapshot fixture,
      queryMaterializationSearch fixture,
      (defaultSearchRequest (retrievalProbeQuery probe)) {searchRequestLimit = retrievalProbeTopK probe}
    )

currentSearchCold :: CurrentSearchFixture -> IO ByteString
currentSearchCold fixture = do
  result <-
    runCurrentSearch
      (currentSearchConnection fixture)
      (currentSearchSnapshot fixture)
      (currentSearchMaterialization fixture)
      (currentSearchRequest fixture)
  renderSearchProjection <$> requireRight "2,000-ADR cold current search" result

currentSearchWarm :: CurrentSearchFixture -> IO ByteString
currentSearchWarm fixture = do
  corpus <- requireJust "2,000-ADR reusable vector corpus" (currentSearchCorpus fixture)
  result <-
    runCurrentSearchWithCorpus
      (currentSearchConnection fixture)
      (currentSearchSnapshot fixture)
      (currentSearchMaterialization fixture)
      corpus
      (currentSearchRequest fixture)
  renderSearchProjection <$> requireRight "2,000-ADR warm current search" result

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
