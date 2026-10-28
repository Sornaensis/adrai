{-# LANGUAGE OverloadedStrings #-}

module Adrai.TextEmbeddingTest (tests, configEntryProbe) where

import Adrai.Compiler (materializeCurrentSearch)
import qualified Adrai.CliTypes as Cli
import qualified Adrai.Web.Api as Api
import Adrai.CompilerMaterializationTest (p303RationaleSnapshot)
import Adrai.CurrentSearchTest (p304ConflictSnapshot)
import Adrai.History (ReadSnapshot (..), RevisionIdentity (..))
import Adrai.Query
import Adrai.Retrieval
import Adrai.Sqlite (initializeSearchSchema, replaceSearchMaterialization)
import Adrai.TextEmbedding
import Adrai.Types (RevisionSelector (AtRevision), mkRepoPath)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, waitCatch)
import Control.Exception (bracket)
import Control.Monad (forM_, unless, void)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:), (.=))
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Database.SQLite.Simple (Only (..), close, execute, execute_, open, query_)
import Network.HTTP.Types (status200, status503)
import Network.Wai (Application, pathInfo, responseLBS, responseRaw, responseStream, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.Environment (getEnvironment, getExecutablePath)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (proc, readProcess, setEnv)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

data Behavior = Healthy | ThreeDimensions | Offline | Malformed | WrongCount | MixedDimensions | Zero | Tiny | Overflow | Disconnected | Slow | SlowBody | AfterBatches Int Behavior deriving (Eq, Show)

tests :: TestTree
tests = testGroup "optional TEI and sqlite-vec"
  [ testCase "user config is optional and validates bounded service settings" configContract,
    testCase "native cosine retrieval, pre-K eligibility, warm reuse and input/model invalidation" cacheContract,
    testCase "outage and malformed responses fall back even after warming" unavailableContract,
    testCase "request timeout is bounded and async cancellation propagates" cancellationContract,
    testCase "search, explicit FTS, blank, vector fallback and relevance share optional provider" queryContract,
    testCase "production config loading reaches CLI and web projections and relevant" configEntryContract
  ]

configContract :: IO ()
configContract = do
  parseTextEmbeddingConfig "" @?= Right Nothing
  parseTextEmbeddingConfig "[text_embedding]\nendpoint = 'http://localhost:8080/'\nmodel = 'local-model'\n"
    @?= Right (Just (TextEmbeddingConfig "http://localhost:8080" (Just "local-model") 2000 16))
  parseTextEmbeddingConfig "[text_embedding]\nendpoint='http://[::1]:8080'"
    @?= Right (Just (TextEmbeddingConfig "http://[::1]:8080" Nothing 2000 16))
  forM_ ["endpoint='http://tei.internal:8080'", "endpoint='http://999.1.1.1'", "endpoint='https://localhost'", "endpoint='ftp://localhost'", "endpoint='http://user:secret@localhost'", "endpoint='http://localhost'\nbatch_size=0", "endpoint='http://localhost'\ntimeout_ms=60000", "endpoint='http://localhost'\nunknown=true"] $ \body ->
    assertBool "invalid optional config is rejected" (case parseTextEmbeddingConfig ("[text_embedding]\n" <> body) of Left _ -> True; _ -> False)

withTei :: (TextEmbeddingConfig -> IORef Behavior -> IORef Text -> IORef [[Text]] -> FilePath -> IO a) -> IO a
withTei action = withSystemTempDirectory "adrai-tei" $ \cache -> do
  behavior <- newIORef Healthy
  model <- newIORef "fixture-model"
  inputs <- newIORef []
  testWithApplication (pure (fixture behavior model inputs)) $ \port ->
    action (TextEmbeddingConfig ("http://127.0.0.1:" <> Text.pack (show port)) Nothing 500 2) behavior model inputs cache

fixture :: IORef Behavior -> IORef Text -> IORef [[Text]] -> Application
fixture behavior model inputs request respond = do
  state <- readIORef behavior
  if state == Slow then threadDelay 2000000 else pure ()
  if state == SlowBody
    then respond (responseStream status200 [("Content-Type", "application/json")] $ \send flush -> do
      send "{\"model_id\":\"fixture\""
      flush
      threadDelay 2000000
      send "}")
    else if state == Offline
    then respond (responseLBS status503 [] "unavailable")
    else if state == Malformed
      then respond (responseLBS status200 [] "invalid JSON")
      else case dropWhile (== "alternate") (pathInfo request) of
        ["info"] -> do
          name <- readIORef model
          respond (json (Aeson.object ["model_id" .= name, "model_sha" .= ("fixture-revision" :: Text)]))
        ["embed"] -> do
          body <- strictRequestBody request
          let parseBatch = Aeson.withObject "inputs" $ \object -> do
                normalized <- object .: "normalize"
                truncated <- object .: "truncate"
                unless (normalized && truncated) (fail "expected normalized model-truncated embeddings")
                object .: "inputs"
              batch = case Aeson.eitherDecode body >>= Aeson.parseEither parseBatch of
                Right values -> values
                Left _ -> []
          prior <- readIORef inputs
          modifyIORef' inputs (<> [batch])
          let current = case state of AfterBatches count next | length prior >= count -> next; _ -> state
          if current == Slow then threadDelay 2000000 else pure ()
          let vectors = case current of
                WrongCount -> []
                MixedDimensions -> zipWith (\n _ -> replicate n (1 :: Double)) [2..] batch
                Zero -> replicate (length batch) [0,0]
                Tiny -> replicate (length batch) [1e-300,0]
                Overflow -> replicate (length batch) [1e100,0]
                ThreeDimensions -> map ((<> [0]) . vector) batch
                _ -> map vector batch
          case current of
            Offline -> respond (responseLBS status503 [] "unavailable")
            Disconnected -> respond (responseRaw (\_ _ -> pure ()) (responseLBS status503 [] "unavailable"))
            Malformed -> respond (responseLBS status200 [] "invalid JSON")
            _ -> respond (json (Aeson.toJSON vectors))
        _ -> respond (responseLBS status503 [] "unexpected endpoint")
  where
    json value = responseLBS status200 [("Content-Type", "application/json")] (Aeson.encode value)
    vector text | "opposite" `Text.isInfixOf` text = [0,1 :: Double]
                | otherwise = [1,0]

cacheContract :: IO ()
cacheContract = withTei $ \config behavior model inputs cache -> do
  invalid <- timeout 1000000 (retrieveTextEmbeddingsWith (config {textEmbeddingEndpoint = "http://tei.invalid:8080"}) cache [("doc", "same")] ["query"] 1)
  invalid @?= Just Nothing
  readIORef inputs >>= (@?= [])
  let retrieve eligibleCorpus = retrieveTextEmbeddingsWith config cache eligibleCorpus ["query"] 1
      corpus = [("allowed", "same"), ("opposite", "opposite")]
  first <- retrieve corpus
  assertBool "native vec0 succeeds" (isJust first)
  fmap embeddingRetrievalScores first @?= Just [Map.singleton "allowed" 1]
  initialFiles <- listDirectory cache
  case initialFiles of
    [file] -> bracket (open (cache </> file)) close $ \stored -> do
      rows <- query_ stored "SELECT count(*) FROM embedding" :: IO [Only Int]
      rows @?= [Only 2] -- corpus only; the query text is never persisted
    _ -> fail "expected one portable corpus cache file"
  count1 <- length <$> readIORef inputs
  second <- retrieve corpus
  second @?= first
  count2 <- length <$> readIORef inputs
  count2 @?= count1 + 1 -- query only; corpus is cached
  eligible <- retrieve [("opposite", "opposite")]
  fmap (map Map.keys . embeddingRetrievalScores) eligible @?= Just [["opposite"]]
  void (retrieve [("allowed", "changed same")])
  count3 <- length <$> readIORef inputs
  count3 @?= count2 + 3 -- eligible query plus changed query and one corpus miss
  writeIORef model "new-model"
  changed <- retrieve corpus
  assertBool "changed live model remains available" (isJust changed)
  assertBool "model change invalidates identity" (fmap embeddingRetrievalIdentity changed /= fmap embeddingRetrievalIdentity first)
  files <- listDirectory cache
  length files @?= 2
  writeIORef behavior ThreeDimensions
  dimensional <- retrieve corpus
  fmap embeddingRetrievalDimensions dimensional @?= Just 3
  assertBool "dimension change invalidates identity" (fmap embeddingRetrievalIdentity dimensional /= fmap embeddingRetrievalIdentity changed)
  alternate <- retrieveTextEmbeddingsWith (config {textEmbeddingEndpoint = textEmbeddingEndpoint config <> "/alternate"}) cache corpus ["query"] 1
  assertBool "endpoint change invalidates identity" (isJust alternate && fmap embeddingRetrievalIdentity alternate /= fmap embeddingRetrievalIdentity dimensional)
  batches <- readIORef inputs
  assertBool "batch limit applies" (all ((<=2) . length) batches)
  cacheReadBoundaryContract
  preparationContract

-- More than two cache-read and vector-insert chunks, with a duplicate text and
-- a non-full final chunk.
-- The result oracle is independent of cache row order and request item order.
cacheReadBoundaryContract :: IO ()
cacheReadBoundaryContract = withTei $ \config _ _ inputs cache -> do
  let unique = [("item-" <> Text.pack (show index), (if even index then "same-" else "opposite-") <> Text.pack (show index)) | index <- [1 :: Int .. 513]]
      corpus = unique <> [("duplicate-text", "same-2")]
      retrieve values = retrieveTextEmbeddingsWith config cache values ["query"] (length corpus)
      expected = Just [Map.fromList [(key, if "opposite" `Text.isInfixOf` text then 0 else 1) | (key, text) <- corpus]]
  prepared <- prepareTextEmbeddingsWith config cache 120000 corpus
  embeddingPreparationStatus prepared @?= PreparationComplete
  embeddingPreparationPreparedTexts prepared @?= 513
  first <- retrieve corpus
  fmap embeddingRetrievalScores first @?= expected
  writeIORef inputs []
  warm <- retrieve (reverse corpus)
  fmap embeddingRetrievalScores warm @?= expected
  readIORef inputs >>= (@?= [["query"]])
  -- More than one insert chunk of only less-similar eligible items must still
  -- fill K. The many cached, closer items must never enter this request's KNN.
  let eligible = filter (("opposite" `Text.isInfixOf`) . snd) corpus
      restricted values limit = retrieveTextEmbeddingsWith config cache values ["query"] limit
  length eligible @?= 257
  subset <- restricted eligible (length corpus)
  fmap embeddingRetrievalScores subset @?= Just [Map.fromList [(key, 0) | (key, _) <- eligible]]
  tied <- restricted eligible 17
  reordered <- restricted (reverse eligible) 17
  reordered @?= tied
  fmap (map Map.size . embeddingRetrievalScores) tied @?= Just [17]
  assertBool "tied neighbours are all eligible" (maybe False (all (all (`elem` map fst eligible) . Map.keys) . embeddingRetrievalScores) tied)
  files <- listDirectory cache
  case files of
    [file] -> bracket (open (cache </> file)) close $ \stored -> do
      rows <- query_ stored "SELECT input_hash,vector FROM embedding ORDER BY input_hash" :: IO [(Text, BS.ByteString)]
      length rows @?= length unique
      (missingHash, corruptHash, bytes) <- case (rows, drop 256 rows) of
        ((firstHash, _) : _, (laterHash, laterBytes) : _) -> pure (firstHash, laterHash, laterBytes)
        _ -> fail "expected cache rows across the SQL chunk boundary"
      execute stored "DELETE FROM embedding WHERE input_hash=?" (Only missingHash)
      execute stored "UPDATE embedding SET vector=? WHERE input_hash=?" (BS.take 1 bytes, corruptHash)
      execute_ stored "INSERT INTO embedding(input_hash,vector) VALUES ('unrelated','not a requested vector')"
      writeIORef inputs []
      repaired <- retrieve corpus
      fmap embeddingRetrievalScores repaired @?= expected
      requested <- readIORef inputs
      length requested @?= 2 -- the query plus one batch for two distinct misses
      length (concat requested) @?= 3
      assertBool "only the query and missing corpus texts are embedded" (all (`elem` ("query" : map snd unique)) (concat requested))
      unrelated <- query_ stored "SELECT vector FROM embedding WHERE input_hash='unrelated'" :: IO [Only Text]
      unrelated @?= [Only "not a requested vector"]
      writeIORef inputs []
      rejected <- retrieve [("duplicate-item", "same-1"), ("duplicate-item", "opposite")]
      assertBool "duplicate item IDs remain rejected" (isNothing rejected)
      readIORef inputs >>= (@?= [["query"]])
    _ -> fail "expected one cache file for bounded-read contract"

preparationContract :: IO ()
preparationContract = do
  -- A bad later batch must not undo earlier complete validated transactions.
  forM_ [WrongCount, MixedDimensions, ThreeDimensions, Zero, Tiny, Overflow, Malformed, Offline, Disconnected] $ \bad ->
    withTei $ \config behavior _ inputs cache -> do
      let corpus = [(Text.pack (show n), "same-" <> Text.pack (show n)) | n <- [1 :: Int .. 6]]
      writeIORef behavior (AfterBatches 2 bad) -- ephemeral probe, valid batch, bad batch
      partial <- prepareTextEmbeddingsWith config cache 120000 corpus
      embeddingPreparationStatus partial @?= PreparationUnavailable
      embeddingPreparationReason partial @?= Just (if bad `elem` [Offline, Disconnected] then ServiceUnavailable else InvalidResponse)
      embeddingPreparationPreparedTexts partial @?= 2
      embeddingPreparationRemainingTexts partial @?= Just 4
      cacheRowCount cache >>= (@?= 2)
      calls <- readIORef inputs
      committedTexts <- case calls of _ : firstBatch : _ -> pure firstBatch; _ -> fail "expected committed first batch"
      writeIORef behavior Healthy
      writeIORef inputs []
      resumed <- prepareTextEmbeddingsWith config cache 120000 corpus
      embeddingPreparationStatus resumed @?= PreparationComplete
      embeddingPreparationCacheHits resumed @?= Just 2
      embeddingPreparationPreparedTexts resumed @?= 4
      embeddingPreparationRemainingTexts resumed @?= Just 0
      resumedCalls <- readIORef inputs
      assertBool "resume does not repeat committed corpus texts" (all (`notElem` concat resumedCalls) committedTexts)
      cacheRowCount cache >>= (@?= 6) -- no dimension probe/query/source persisted
      -- Same-length invalid float32 cache bytes are misses, too.
      files <- listDirectory cache
      case files of
        [file] -> bracket (open (cache </> file)) close $ \stored -> do
          execute_ stored "UPDATE embedding SET vector=zeroblob(8) WHERE input_hash=(SELECT min(input_hash) FROM embedding)"
          repaired <- prepareTextEmbeddingsWith config cache 120000 corpus
          embeddingPreparationPreparedTexts repaired @?= 1
          execute stored "UPDATE embedding SET vector=? WHERE input_hash=(SELECT min(input_hash) FROM embedding)" (Only (BS.pack [0,0,128,127,0,0,0,0]))
          repairedFinite <- prepareTextEmbeddingsWith config cache 120000 corpus
          embeddingPreparationPreparedTexts repairedFinite @?= 1
        _ -> fail "expected one preparation cache"
  forM_ [False, True] $ \full -> withTei $ \config behavior _ inputs cache -> do
    let corpus = [(Text.pack (show n), "same-" <> Text.pack (show n)) | n <- [1 :: Int .. 6]]
        bounded = config {textEmbeddingTimeoutMs = 10000}
    writeIORef behavior (AfterBatches 2 Slow)
    progress <- if full then prepareTextEmbeddingsWith bounded cache 1000 corpus
      else do
        outcome <- retrieveTextEmbeddingsOutcomeWith bounded cache corpus ["query"] 6
        case outcome of EmbeddingNotReady value -> pure value; _ -> fail "partial corpus must not run KNN"
    assertBool ("deadline progress " <> show (full, progress)) (embeddingPreparationStatus progress == PreparationPending)
    embeddingPreparationReason progress @?= Just BudgetExhausted
    embeddingPreparationPreparedTexts progress @?= 2
    embeddingPreparationRemainingTexts progress @?= Just 4
    length <$> readIORef inputs >>= (@?= 3) -- no next repair request after budget
    cacheRowCount cache >>= (@?= 2)
    writeIORef behavior Healthy
    ready <- retrieveTextEmbeddingsWith config cache corpus ["query"] 6
    assertBool "small remaining delta heals automatically" (isJust ready)
  forM_ [False, True] $ \full -> withTei $ \config behavior _ inputs cache -> do
    let corpus = [(Text.pack (show n), "same-" <> Text.pack (show n)) | n <- [1 :: Int .. 6]]
    writeIORef behavior (AfterBatches 2 Slow)
    worker <- async (if full
      then void (prepareTextEmbeddingsWith (config {textEmbeddingTimeoutMs = 10000}) cache 120000 corpus)
      else void (retrieveTextEmbeddingsOutcomeWith (config {textEmbeddingTimeoutMs = 10000}) cache corpus ["query"] 6))
    let awaitThird = do calls <- readIORef inputs; if length calls >= 3 then pure () else threadDelay 1000 >> awaitThird
    observed <- timeout 2000000 awaitThird
    observed @?= Just ()
    cancel worker
    cancelled <- waitCatch worker
    assertBool "preparation cancellation remains exceptional" (case cancelled of Left _ -> True; _ -> False)
    cacheRowCount cache >>= (@?= 2)
    writeIORef behavior Healthy
    resumed <- prepareTextEmbeddingsWith config cache 120000 corpus
    embeddingPreparationCacheHits resumed @?= Just 2
    embeddingPreparationPreparedTexts resumed @?= 4
  withTei $ \config _ _ inputs cache -> do
    let corpus n = [(Text.pack (show i), "same-" <> Text.pack (show i)) | i <- [1 :: Int .. n]]
    empty <- prepareTextEmbeddingsWith config cache 120000 []
    embeddingPreparationStatus empty @?= PreparationComplete
    readIORef inputs >>= (@?= [])
    invalid <- prepareTextEmbeddingsWith config cache 99 (corpus 1)
    embeddingPreparationReason invalid @?= Just InvalidConfiguration
    readIORef inputs >>= (@?= [])
    pending <- retrieveTextEmbeddingsOutcomeWith config cache (corpus 33) ["query"] 33
    case pending of
      EmbeddingNotReady progress -> do
        embeddingPreparationStatus progress @?= PreparationPending
        embeddingPreparationRemainingTexts progress @?= Just 33
        embeddingPreparationPreparedTexts progress @?= 0
      _ -> fail "bulk corpus must use explicit preparation"
    readIORef inputs >>= (@?= [["query"]])
    ready <- retrieveTextEmbeddingsWith config cache (corpus 32) ["query"] 32
    assertBool "32 missing texts recover automatically" (isJust ready)
    edited <- retrieveTextEmbeddingsWith config cache (corpus 32 <> [("edit", "same-edit")]) ["query"] 33
    assertBool "small post-edit delta recovers automatically" (isJust edited)
    cacheRowCount cache >>= (@?= 33)

cacheRowCount :: FilePath -> IO Int
cacheRowCount cache = do
  files <- listDirectory cache
  counts <- mapM (\file -> bracket (open (cache </> file)) close $ \stored -> do
    rows <- query_ stored "SELECT count(*) FROM embedding" :: IO [Only Int]
    case rows of [Only count] -> pure count; _ -> fail "invalid cache count") files
  pure (sum counts)

unavailableContract :: IO ()
unavailableContract = withTei $ \config behavior _ _ cache -> do
  let retrieve = retrieveTextEmbeddingsWith config cache [("doc", "same")] ["query", "query two"] 1
  assertBool "warm fixture" . isJust =<< retrieve
  forM_ [Offline, Malformed, WrongCount, MixedDimensions, Zero, Tiny] $ \state -> do
    writeIORef behavior state
    assertBool ("optional failure: " <> show state) . isNothing =<< retrieve
  writeIORef behavior Healthy
  assertBool "configured model mismatch disables vectors" . isNothing =<< retrieveTextEmbeddingsWith (config {textEmbeddingModel = Just "different"}) cache [("doc", "same")] ["query"] 1

cancellationContract :: IO ()
cancellationContract = withTei $ \config behavior _ _ cache -> do
  writeIORef behavior Slow
  let retrieve = retrieveTextEmbeddingsWith (config {textEmbeddingTimeoutMs = 100}) cache [("doc", "same")] ["query"] 1
  timeout 1000000 retrieve >>= (@?= Just Nothing)
  writeIORef behavior SlowBody
  timeout 1000000 retrieve >>= (@?= Just Nothing)
  -- RFC 5737 documentation address: no service or public DNS is required.
  -- On Windows this previously waited 21 seconds in native connect.
  let unreachable = config {textEmbeddingEndpoint = "http://192.0.2.1:9", textEmbeddingTimeoutMs = 100}
  timeout 1000000 (retrieveTextEmbeddingsWith unreachable cache [("doc", "same")] ["query"] 1) >>= (@?= Just Nothing)
  writeIORef behavior Slow
  running <- async (retrieveTextEmbeddingsWith config cache [("doc", "same")] ["query"] 1)
  threadDelay 20000
  cancel running
  outcome <- waitCatch running
  assertBool "cancellation stays exceptional" (case outcome of Left _ -> True; _ -> False)

queryContract :: IO ()
queryContract = withTei $ \config behavior _ inputs cache -> bracket (open ":memory:") close $ \connection -> do
  let snapshot = p303RationaleSnapshot
      materialization = must (materializeCurrentSearch snapshot)
      provider = retrieveTextEmbeddingsWith config cache
      request mode text = (defaultSearchRequest text) {searchRequestMode = mode, searchRequestIncludeObsolete = True}
      run mode text = fmap must (runCurrentSearchWithEmbedding provider connection snapshot materialization (request mode text))
  initializeSearchSchema connection >>= (@?= Right ())
  replaceSearchMaterialization connection materialization >>= (@?= Right ())
  void (run FtsRetrieval "current")
  void (run HybridRetrieval "")
  readIORef inputs >>= (@?= [])
  vector <- run VectorRetrieval "current"
  searchProjectionMode vector @?= VectorRetrieval
  assertBool "service projection includes vector evidence" (any ((>0) . searchResultVectorScore) (searchProjectionResults vector))
  callsBefore <- readIORef inputs
  forM_ [ (request VectorRetrieval "current") {searchRequestDomains = ["absent"]},
          (request VectorRetrieval "current") {searchRequestFile = Just (must (mkRepoPath "unmatched/file.hs"))},
          (request VectorRetrieval "current") {searchRequestIncludeObsolete = False}
        ] $ \filteredRequest -> do
    filtered <- fmap must (runCurrentSearchWithEmbedding provider connection snapshot materialization filteredRequest)
    searchProjectionResults filtered @?= []
  readIORef inputs >>= (@?= callsBefore)
  paged <- fmap must (runCurrentSearchWithEmbedding provider connection snapshot materialization ((request VectorRetrieval "current") {searchRequestPage = 2, searchRequestLimit = 1}))
  searchProjectionResults paged @?= []
  searchProjectionMode paged @?= VectorRetrieval
  writeIORef behavior Offline
  fallback <- run VectorRetrieval "current"
  lexical <- run FtsRetrieval "current"
  fallback @?= lexical
  searchProjectionMode fallback @?= FtsRetrieval
  let path = must (mkRepoPath "src/current.hs")
      revision = revisionRequested (readSnapshotRevision snapshot)
      source = RevisionRelevantSource path (revisionResolved (readSnapshotRevision snapshot)) "source-blob" "current decision api current summary decision first decision second"
      relevant = (defaultRelevantRequest path) {relevantRequestRevision = AtRevision revision, relevantRequestIncludeObsolete = True}
      hooks = RelevantTimingHooks (pure ()) (pure ())
  result <- fmap must (runRelevantWithEmbedding provider hooks connection snapshot materialization relevant source)
  relevantRetrievalStrategy (relevantProjectionRetrieval result) @?= "passage-fts"
  relevantRetrievalSemanticVectorId (relevantProjectionRetrieval result) @?= Nothing
  writeIORef behavior Healthy
  healthy <- fmap must (runRelevantWithEmbedding provider hooks connection snapshot materialization relevant source)
  relevantRetrievalStrategy (relevantProjectionRetrieval healthy) @?= "tei+sqlite-vec+passage-fts"
  assertBool "healthy relevance uses external vectors" (isJust (relevantRetrievalSemanticVectorId (relevantProjectionRetrieval healthy)))
  richWarm <- fmap must (runCurrentSearchWithEmbeddingOutcome (retrieveTextEmbeddingsOutcomeWith config cache) connection snapshot materialization (request VectorRetrieval "current"))
  renderSearchProjection richWarm @?= renderSearchProjection vector
  richRelevant <- fmap must (runRelevantWithEmbeddingOutcome (retrieveTextEmbeddingsOutcomeWith config cache) hooks connection snapshot materialization relevant source)
  renderRelevantProjection richRelevant @?= renderRelevantProjection healthy
  let progress = EmbeddingPreparation PreparationPending (Just "known-model") (Just 2) 33 (Just 0) 0 (Just 33) (Just CorpusMissing)
      pending _ _ _ = pure (EmbeddingNotReady progress)
      unavailable _ _ _ = pure (EmbeddingNotReady (progress {embeddingPreparationStatus = PreparationUnavailable, embeddingPreparationReason = Just ServiceUnavailable,
        embeddingPreparationIdentity = Nothing, embeddingPreparationDimensions = Nothing, embeddingPreparationCacheHits = Nothing, embeddingPreparationRemainingTexts = Nothing}))
      field name value = case value of Aeson.Object object -> KeyMap.lookup name object; _ -> Nothing
      decodeProjection = must . Aeson.eitherDecodeStrict'
  pendingSearch <- fmap must (runCurrentSearchWithEmbeddingOutcome pending connection snapshot materialization (request VectorRetrieval "current"))
  searchProjectionMode pendingSearch @?= FtsRetrieval
  renderSearchProjection (pendingSearch {searchProjectionEmbeddingPreparation = Nothing}) @?= renderSearchProjection lexical
  let metadata = field "embedding_preparation" (decodeProjection (renderSearchProjection pendingSearch))
  (metadata >>= field "status") @?= Just (Aeson.String "pending")
  (metadata >>= field "reason") @?= Just (Aeson.String "corpus_missing")
  (metadata >>= field "action" >>= field "command") @?= Just (Aeson.String "prepare-embeddings")
  (metadata >>= field "action" >>= field "at") @?= Just (Aeson.String (revisionResolved (readSnapshotRevision snapshot)))
  unavailableSearch <- fmap must (runCurrentSearchWithEmbeddingOutcome unavailable connection snapshot materialization (request VectorRetrieval "current"))
  let unavailableMetadata = field "embedding_preparation" (decodeProjection (renderSearchProjection unavailableSearch))
  (unavailableMetadata >>= field "status") @?= Just (Aeson.String "unavailable")
  forM_ ["model_identity", "dimensions", "cache_hits", "remaining_texts"] $ \key ->
    (unavailableMetadata >>= field key) @?= Just Aeson.Null
  pendingRelevant <- fmap must (runRelevantWithEmbeddingOutcome pending hooks connection snapshot materialization relevant source)
  relevantRetrievalStrategy (relevantProjectionRetrieval pendingRelevant) @?= "passage-fts"
  assertBool "relevance carries request-local pending progress" (isJust (relevantProjectionEmbeddingPreparation pendingRelevant))
  forM_ [request FtsRetrieval "current", request HybridRetrieval ""] $ \skipped -> do
    noCalls <- fmap must (runCurrentSearchWithEmbeddingOutcome (\_ _ _ -> fail "FTS/blank must not call embeddings") connection snapshot materialization skipped)
    searchProjectionEmbeddingPreparation noCalls @?= Nothing
  let conflicted = must (materializeCurrentSearch p304ConflictSnapshot)
  replaceSearchMaterialization connection conflicted >>= (@?= Right ())
  conflict <- fmap must (runCurrentSearchWithEmbedding provider connection p304ConflictSnapshot conflicted (defaultSearchRequest "left"))
  length (searchProjectionResults conflict) @?= 1
  map searchResultTitle (searchProjectionResults conflict) @?= ["[conflicted ADR]"]

-- Run the production config loader in a child so no other test can observe a
-- temporary config/cache environment or contact this fixture accidentally.
configEntryContract :: IO ()
configEntryContract = withTei $ \config behavior _ _ directory -> do
  executable <- getExecutablePath
  inherited <- getEnvironment
  let configRoot = directory </> "config"
      cacheRoot = directory </> "cache"
      environment = [("XDG_CONFIG_HOME", configRoot), ("XDG_CACHE_HOME", cacheRoot), ("http_proxy", "http://invalid.proxy:9"), ("HTTP_PROXY", "http://invalid.proxy:9"), ("NO_PROXY", ""), ("no_proxy", "")]
        <> filter (\(key,_) -> key `notElem` ["XDG_CONFIG_HOME", "XDG_CACHE_HOME", "http_proxy", "HTTP_PROXY", "NO_PROXY", "no_proxy"]) inherited
      run = do
        (code, output, errors) <- readProcess (setEnv environment (proc executable ["--tei-config-entry-probe", "+RTS", "-N1", "-RTS"]))
        code @?= ExitSuccess
        errors @?= ""
        pure (must (Aeson.eitherDecode output) :: Aeson.Value)
      field name value = case value of Aeson.Object object -> KeyMap.lookup name object; _ -> Nothing
  absent <- run
  field "mode" absent @?= Just (Aeson.String "fts")
  createDirectoryIfMissing True (configRoot </> "adrai")
  TextIO.writeFile (configRoot </> "adrai" </> "config.toml")
    ("[text_embedding]\nendpoint='" <> textEmbeddingEndpoint config <> "'\n")
  healthy <- run
  field "mode" healthy @?= Just (Aeson.String "hybrid")
  (field "cli" healthy >>= field "algorithm") @?= Just (Aeson.String "three-fts+tei+sqlite-vec+weighted-rrf")
  (field "web" healthy >>= field "mode") @?= Just (Aeson.String "hybrid")
  field "relevant_strategy" healthy @?= Just (Aeson.String "tei+sqlite-vec+passage-fts")
  writeIORef behavior Offline
  offline <- run
  field "mode" offline @?= Just (Aeson.String "fts")
  (field "cli" offline >>= field "algorithm") @?= Just (Aeson.String "three-fts+weighted-rrf")
  field "relevant_strategy" offline @?= Just (Aeson.String "passage-fts")
  TextIO.writeFile (configRoot </> "adrai" </> "config.toml") "[text_embedding]\nendpoint='invalid'\n"
  malformed <- run
  (field "web" malformed >>= field "embedding_preparation" >>= field "reason") @?= Just (Aeson.String "invalid_configuration")
  (field "web" malformed >>= field "embedding_preparation" >>= field "cache_hits") @?= Just Aeson.Null

configEntryProbe :: IO ()
configEntryProbe = bracket (open ":memory:") close $ \connection -> do
  let snapshot = p303RationaleSnapshot
      materialization = must (materializeCurrentSearch snapshot)
      request = (defaultSearchRequest "current") {searchRequestIncludeObsolete = True}
      command = Cli.SearchCommand "current" HybridRetrieval Cli.CollapsedView Nothing [] Nothing Nothing Nothing "HEAD" True 10 True False 1
      path = must (mkRepoPath "src/current.hs")
      source = RevisionRelevantSource path (revisionResolved (readSnapshotRevision snapshot)) "source-blob" "current decision api current summary decision first decision second"
      relevant = (defaultRelevantRequest path) {relevantRequestRevision = AtRevision (revisionRequested (readSnapshotRevision snapshot)), relevantRequestIncludeObsolete = True}
  initializeSearchSchema connection >>= (@?= Right ())
  replaceSearchMaterialization connection materialization >>= (@?= Right ())
  projection <- fmap must (runCurrentSearch connection snapshot materialization request)
  relevance <- fmap must (runRelevant connection snapshot materialization relevant source)
  LBS8.putStrLn (Aeson.encode (Aeson.object
    [ "mode" .= retrievalModeName (searchProjectionMode projection),
      "cli" .= Cli.toAesonValue (Cli.searchCliJson command projection),
      "web" .= Api.apiResultPayload (Api.ApiSearchResult projection),
      "relevant_strategy" .= relevantRetrievalStrategy (relevantProjectionRetrieval relevance)
    ]))

must :: Show e => Either e a -> a
must = either (error . show) id
