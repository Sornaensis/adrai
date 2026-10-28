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
    searchRequestLimit,
  )
import Adrai.Retrieval (SearchDocument (..), SearchMaterialization (..), SearchPassage (..))
import Adrai.Sqlite (initializeSearchSchema, replaceSearchMaterialization)
import Control.Exception (bracket, evaluate, onException)
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Database.SQLite.Simple (Connection, close, open)

-- | The fixed 2,000-ADR current-search state shared by Criterion and the
-- profiling driver. Preparing this fixture creates the SQLite indexes. A warm
-- fixture runs one production query before timing. A large optional TEI corpus
-- requires explicit preparation outside this fixture; setup alone does not
-- establish vector readiness under the interactive repair limit.
data CurrentSearchFixture = CurrentSearchFixture
  { currentSearchConnection :: Connection,
    currentSearchSnapshot :: ReadSnapshot,
    currentSearchMaterialization :: SearchMaterialization,
    currentSearchRequest :: SearchRequest
  }

prepareCurrentSearchFixture :: Bool -> IO CurrentSearchFixture
prepareCurrentSearchFixture reuseCorpus = do
  (snapshot, materialization, request) <- scaleSearchInputs
  connection <- open ":memory:"
  ( do
      requireRight "2,000-ADR SQLite schema" =<< initializeSearchSchema connection
      requireRight "2,000-ADR SQLite materialization" =<< replaceSearchMaterialization connection materialization
      let preparedFixture =
            CurrentSearchFixture
              { currentSearchConnection = connection,
                currentSearchSnapshot = snapshot,
                currentSearchMaterialization = materialization,
                currentSearchRequest = request
              }
      -- This is the single fixture-ready boundary shared by Criterion's
      -- environment forcing and the profile driver.  In particular, a warm
      -- materialization has no remaining lazy construction before either path
      -- begins its action.
      _ <- evaluate (forceCurrentSearchFixture preparedFixture)
      when reuseCorpus (void (currentSearchCold preparedFixture))
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
currentSearchWarm = currentSearchCold

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

requireRight :: Show problem => String -> Either problem value -> IO value
requireRight label result =
  case result of
    Left problem -> ioError (userError (label <> ": " <> show problem))
    Right value -> pure value
