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
import Network.Wai (Application, pathInfo, responseLBS, responseStream, strictRequestBody)
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

data Behavior = Healthy | ThreeDimensions | Offline | Malformed | WrongCount | MixedDimensions | Zero | Tiny | Slow | SlowBody deriving (Eq, Show)

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
          modifyIORef' inputs (<> [batch])
          let vectors = case state of
                WrongCount -> []
                MixedDimensions -> zipWith (\n _ -> replicate n (1 :: Double)) [2..] batch
                Zero -> replicate (length batch) [0,0]
                Tiny -> replicate (length batch) [1e-300,0]
                ThreeDimensions -> map ((<> [0]) . vector) batch
                _ -> map vector batch
          respond (json (Aeson.toJSON vectors))
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

-- More than two SQL chunks, with a duplicate text and a non-full final chunk.
-- The result oracle is independent of cache row order and request item order.
cacheReadBoundaryContract :: IO ()
cacheReadBoundaryContract = withTei $ \config _ _ inputs cache -> do
  let unique = [("item-" <> Text.pack (show index), (if even index then "same-" else "opposite-") <> Text.pack (show index)) | index <- [1 :: Int .. 513]]
      corpus = unique <> [("duplicate-text", "same-2")]
      retrieve values = retrieveTextEmbeddingsWith config cache values ["query"] (length corpus)
      expected = Just [Map.fromList [(key, if "opposite" `Text.isInfixOf` text then 0 else 1) | (key, text) <- corpus]]
  first <- retrieve corpus
  fmap embeddingRetrievalScores first @?= expected
  writeIORef inputs []
  warm <- retrieve (reverse corpus)
  fmap embeddingRetrievalScores warm @?= expected
  readIORef inputs >>= (@?= [["query"]])
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
