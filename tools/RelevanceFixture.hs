{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

-- Run through the project environment; generated repositories are data artifacts.
-- No Git repository is initialized by this tool.
module Main where

import Adrai.Git (Repository, discoverRepository, systemGit)
import Adrai.Format (renderDigest)
import Adrai.Provenance (sha256Digest)
import Adrai.Query
import Adrai.Relevance (ConfidenceLabel (..))
import Adrai.Retrieval (RetrievalMode (..))
import Adrai.Scope (scopeEvidencePattern)
import qualified Adrai.Service.Query as Service
import Adrai.TextEmbedding (TextEmbeddingConfig (..), loadTextEmbeddingConfig, textEmbeddingConfigPath)
import Adrai.Types (RevisionSelector (AtRevision), adrIdText, mkRepoPath)
import Control.Exception (SomeException, bracket, catch, finally)
import Control.Monad (forM, forM_, unless, when)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?), (.!=), (.=))
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.List (findIndex)
import qualified Data.List as List
import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.IO as TextIO
import Database.SQLite.Simple (Only (..), close, open, query_)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Status (statusCode)
import System.Directory
import System.Environment (getArgs, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), normalise, splitDirectories, takeDirectory, takeExtension)
import System.Info (os)
import System.FilePath (takeFileName)
import System.Process (proc, readCreateProcessWithExitCode)

data Case = Case
  { caseName :: Text, caseFile :: Maybe FilePath, caseQuery :: Maybe Text,
    caseExpected :: [Text], caseExcluded :: [Text]
  }

instance Aeson.FromJSON Case where
  parseJSON = Aeson.withObject "evaluation case" $ \o -> Case
    <$> o .: "name" <*> o .:? "file" <*> o .:? "query"
    <*> o .:? "expected" .!= [] <*> o .:? "excluded" .!= []

instance Aeson.ToJSON Case where
  toJSON c = Aeson.object
    [ "name" .= caseName c, "file" .= caseFile c, "query" .= caseQuery c,
      "expected" .= caseExpected c, "excluded" .= caseExcluded c ]

data External = External FilePath Text [Case]
instance Aeson.FromJSON External where
  parseJSON = Aeson.withObject "external evaluation manifest" $ \o -> External
    <$> o .: "source_repository" <*> o .: "revision" <*> o .: "cases"

data Document = Document Text Text Text Text [Text]

main :: IO ()
main = do
  setLocaleEncoding utf8
  args <- getArgs
  case args of
    ["guard", path] -> guardLocalPath path >>= putStrLn
    ["create", seed, artifact, executable] -> createSynthetic seed artifact executable
    ["evaluate", artifactInput, executable] -> do
      artifact <- requireArtifact artifactInput
      cases <- readJson (artifact </> "synthetic" </> "fixture" </> "labels.json")
      evaluateCorpus artifact executable "synthetic" (artifact </> "synthetic") cases
    ["external", artifactInput, executable, manifest] -> do
      artifact <- requireArtifact artifactInput
      External sourceInput revision cases <- readJson manifest
      source <- guardLocalPath sourceInput
      destination <- guardLocalPath (artifact </> "transient" </> "external")
      exists <- doesDirectoryExist destination
      if exists
        then do
          provenance <- readJson (artifact </> "external-provenance.json")
          prior <- parseValue (Aeson.withObject "external provenance" $ \o -> (,) <$> o .: "source_repository" <*> o .: "source_revision") provenance
          unless (pathsEqual (fst prior) source && snd prior == revision) (fail "existing external snapshot is owned by different provenance")
        else do
          writeJson (artifact </> "external-provenance.json") (Aeson.object
            ["source_revision" .= revision, "source_repository" .= source, "cases" .= cases,
             "disposition" .= ("temporary clone through review; retain only bounded sanitized results" :: Text)])
          createDirectoryIfMissing True (takeDirectory destination)
          voidRun "git" ["clone", "--no-local", "--no-checkout", source, destination]
          disablePush destination
          voidRun "git" ["-C", destination, "update-ref", "refs/heads/evaluation", Text.unpack revision]
          voidRun "git" ["-C", destination, "symbolic-ref", "HEAD", "refs/heads/evaluation"]
      actual <- Text.strip . Text.pack <$> run "git" ["-C", destination, "rev-parse", "HEAD"]
      unless (actual == revision) (fail "external snapshot revision differs from its manifest")
      evaluateCorpus artifact executable "external" destination cases
    _ -> fail "usage: RelevanceFixture.hs create SEED_REPOSITORY ARTIFACT ADRAI_EXE | evaluate ARTIFACT ADRAI_EXE | external ARTIFACT ADRAI_EXE PRIVATE_MANIFEST"

createSynthetic :: FilePath -> FilePath -> FilePath -> IO ()
createSynthetic seedInput artifactInput executable = do
  seed <- guardLocalPath seedInput
  artifact <- guardLocalPath artifactInput
  seedRoot <- Text.unpack . Text.strip . Text.pack <$> run "git" ["-C", seed, "rev-parse", "--show-toplevel"]
  canonicalSeed <- canonicalizePath seed
  unless (pathsEqual seedRoot canonicalSeed)
    (fail "seed must be an existing exact repository root outside Codex configuration")
  occupied <- doesPathExist artifact
  when occupied (fail "artifact path already exists; use evaluate for an existing owned artifact")
  config <- loadTextEmbeddingConfig
  createDirectoryIfMissing True artifact
  seedSha <- Text.strip . Text.pack <$> run "git" ["-C", seed, "rev-parse", "HEAD"]
  writeJson (artifact </> "artifact.json") (Aeson.object
    ["schema" .= ("adrai/relevance-fixture/v1" :: Text), "seed_revision" .= seedSha,
     "retention" .= ("synthetic fixture and sanitized receipts are requested deliverables" :: Text)])
  writeConfig (artifact </> "config-default") config
  createDirectoryIfMissing True (artifact </> "config-fts")
  let repository = artifact </> "synthetic"
  voidRun "git" ["clone", "--no-local", "--depth", "1", seed, repository]
  disablePush repository
  forM_ [("user.name", "ADRAI Retrieval Fixture"), ("user.email", "fixture@example.invalid"),
         ("commit.gpgsign", "false"), ("core.autocrlf", "false"), ("gc.auto", "0")] $ \(key, value) ->
    voidRun "git" ["-C", repository, "config", key, value]
  let hooks = artifact </> "empty-hooks"
  createDirectoryIfMissing True hooks
  voidRun "git" ["-C", repository, "config", "core.hooksPath", hooks]
  forM_ sources $ \(path, body) -> do
    createDirectoryIfMissing True (takeDirectory (repository </> path))
    TextIO.writeFile (repository </> path) body
  voidRun "git" ["-C", repository, "add", "--", "fixture"]
  voidRun "git" ["-C", repository, "commit", "-m", "Add controlled retrieval source cases"]
  ids <- withRoots (artifact </> "config-fts") (artifact </> "cache-setup") $ do
    _ <- cli executable repository ["init", "--json"]
    fmap Map.fromList $ forM documents $ \(Document key title summary body scopes) -> do
      response <- cli executable repository
        (["create", "--title", Text.unpack title, "--summary", Text.unpack summary,
          "--body", Text.unpack body, "--domain", "fixture.retrieval", "--actor", "service:retrieval-fixture", "--json"]
          <> concatMap (\scope -> ["--applies-to", Text.unpack scope]) scopes)
      identifier <- parseValue (Aeson.withObject "create result" (.: "adr")) response
      pure (key, identifier)
  let resolve key = Map.findWithDefault (error "missing synthetic ADR") key ids
      labels = [c {caseExpected = map resolve (caseExpected c), caseExcluded = map resolve (caseExcluded c)} | c <- syntheticCases]
  writeJson (repository </> "fixture" </> "labels.json") labels
  voidRun "git" ["-C", repository, "add", "--", "fixture/labels.json"]
  voidRun "git" ["-C", repository, "commit", "-m", "Record generated ADR labels"]
  finalSha <- Text.strip . Text.pack <$> run "git" ["-C", repository, "rev-parse", "HEAD"]
  writeJson (artifact </> "synthetic-provenance.json") (Aeson.object
    ["seed_revision" .= seedSha, "fixture_revision" .= finalSha, "adrs" .= ids,
     "git_creation" .= ("clone of existing repository; no Git initialization" :: Text)])
  putStrLn ("Created persistent synthetic fixture: " <> repository)

evaluateCorpus :: FilePath -> FilePath -> String -> FilePath -> [Case] -> IO ()
evaluateCorpus artifactInput executable label repositoryInput cases = do
  artifact <- guardLocalPath artifactInput
  repositoryPath <- guardLocalPath repositoryInput
  executableHash <- renderDigest . sha256Digest <$> BS.readFile executable
  harnessHash <- renderDigest . sha256Digest <$> BS.readFile __FILE__
  repository <- discoverRepository systemGit repositoryPath >>= must
  withRoots (artifact </> "config-fts") (artifact </> "cache-setup") $ do
    _ <- cli executable repositoryPath ["compile", "--json"]
    pure ()
  let cache = artifact </> "transient" </> (label <> "-cache")
  fallback <- withRoots (artifact </> "config-fts") cache (evaluatePhase repository cache "fts" cases)
  createDirectoryIfMissing True (artifact </> "transient")
  writeJson (artifact </> "transient" </> (label <> "-fts.json")) fallback
  (info, actual) <- withRoots (artifact </> "config-default") cache $ do
    config <- loadTextEmbeddingConfig
    info <- maybe (pure Aeson.Null) modelInfo config
    cold <- evaluatePhase repository cache "default-first" cases
    writeJson (artifact </> "transient" </> (label <> "-first.json")) cold
    repeated <- evaluatePhase repository cache "default-repeat" cases
    writeJson (artifact </> "transient" </> (label <> "-repeat.json")) repeated
    pairs <- forM [c | c <- cases, caseQuery c /= Nothing] $ \c -> do
      let query = maybe "" id (caseQuery c)
      _ <- search repository query
      baseline <- timedSearch repository c query "plain-warm"
      instructed <- timedSearch repository c (instruction <> query) "instructed-warm"
      pure (Aeson.object ["case" .= caseName c, "plain" .= baseline, "instructed" .= instructed])
    smoke <- case [c | c <- cases, caseFile c /= Nothing, caseQuery c /= Nothing] <> [c | c <- cases, caseFile c /= Nothing] of
      c : _ -> do
        response <- cli executable repositoryPath ["relevant", maybe "" id (caseFile c), "--limit", "100", "--json"]
        pure (sanitizeCli response)
      [] -> pure Aeson.Null
    pure (Aeson.object ["info" .= info, "settings" .= fmap configJson config],
      Aeson.object ["first" .= cold, "repeat" .= repeated, "prompt_pairs" .= pairs, "cli_smoke" .= smoke])
  revision <- Text.strip . Text.pack <$> run "git" ["-C", repositoryPath, "rev-parse", "HEAD"]
  writeJson (artifact </> (label <> "-results.json")) (Aeson.object
    ["schema" .= ("adrai/retrieval-evaluation/v1" :: Text), "revision" .= revision,
     "adrai_executable_sha256" .= executableHash, "harness_sha256" .= harnessHash,
     "pipeline" .= ("exported production query service; lexical archive prepared before embedding-cache measurements" :: Text),
     "model_info" .= info, "fts" .= fallback, "actual_default" .= actual,
     "instruction" .= instruction,
     "limitations" .= (["Labels identify selected positives and hard negatives, not exhaustive precision.",
       "A repeat is warm only if the actual backend succeeds and corpus vectors were published.",
       "Instruction comparisons change explicit query text only; production prompting and thresholds are unchanged."] :: [Text])])
  putStrLn ("Wrote sanitized receipts: " <> (artifact </> (label <> "-results.json")))

evaluatePhase :: Repository -> FilePath -> Text -> [Case] -> IO [Aeson.Value]
evaluatePhase repository cache phase cases = forM cases $ \c -> do
  putStrLn (Text.unpack phase <> ": " <> Text.unpack (caseName c))
  before <- cacheRows cache
  value <- case caseFile c of
    Nothing -> pure Aeson.Null
    Just filename -> do
      path <- either (fail . show) pure (mkRepoPath (Text.pack filename))
      (elapsed, projection) <- timed (Service.runRelevantQuery repository ((defaultRelevantRequest path)
        {relevantRequestRevision = AtRevision "HEAD", relevantRequestLimit = 100}) >>= must)
      let results = relevantProjectionResults projection
          rows = [(adrIdText (relevantResultAdr row), relevantResultScore row) | row <- results]
      pure (Aeson.object ["elapsed_ms" .= elapsed,
        "backend" .= relevantRetrievalStrategy (relevantProjectionRetrieval projection),
        "vector_identity" .= relevantRetrievalSemanticVectorId (relevantProjectionRetrieval projection),
        "source_digest" .= relevantFileDigest (relevantProjectionFile projection),
        "diagnostics" .= fmap diagnosticsJson (relevantProjectionDiagnostics projection),
        "counts" .= Aeson.object ["eligible_adrs" .= relevantRetrievalEligibleAdrs (relevantProjectionRetrieval projection),
          "passages" .= relevantRetrievalSearchSections (relevantProjectionRetrieval projection),
          "scope_candidates" .= relevantRetrievalScopeCandidates (relevantProjectionRetrieval projection),
          "content_pairs" .= relevantRetrievalExactRerankCandidates (relevantProjectionRetrieval projection)],
        "total" .= length results, "labels" .= labelRanks c rows,
        "top" .= map relevantRow (take 10 results),
        "selected" .= map relevantRow [row | row <- results, adrIdText (relevantResultAdr row) `elem` (caseExpected c <> caseExcluded c)]])
  afterRelevant <- cacheRows cache
  query <- case caseQuery c of
    Nothing -> pure Aeson.Null
    Just text -> timedSearch repository c text phase
  after <- cacheRows cache
  pure (Aeson.object ["case" .= caseName c, "phase" .= phase, "embedding_rows_before" .= before,
    "embedding_rows_after_relevant" .= afterRelevant, "embedding_rows_after" .= after,
    "relevant" .= value, "search" .= query])

search :: Repository -> Text -> IO SearchProjection
search repository query = Service.runSearch repository
  (Service.SearchServiceRequest "HEAD" ((defaultSearchRequest query) {searchRequestMode = VectorRetrieval, searchRequestLimit = 1000})) >>= must

timedSearch :: Repository -> Case -> Text -> Text -> IO Aeson.Value
timedSearch repository c query variant = do
  (elapsed, projection) <- timed (search repository query)
  let results = searchProjectionResults projection
      rows = [(adrIdText (searchResultAdr row), searchResultScore row) | row <- results]
  pure (Aeson.object ["variant" .= variant, "elapsed_ms" .= elapsed,
    "effective_mode" .= show (searchProjectionMode projection), "labels" .= labelRanks c rows,
    "top" .= [Aeson.object ["adr" .= identifier, "score" .= score] | (identifier, score) <- take 10 rows]])

relevantRow :: RelevantResult -> Aeson.Value
relevantRow row = Aeson.object
  ["adr" .= adrIdText (relevantResultAdr row), "score" .= relevantResultScore row,
   "semantic" .= relevantResultSemanticScore row, "lexical" .= relevantResultLexicalScore row,
   "scope" .= relevantResultScopeBonus row, "pattern" .= fmap scopeEvidencePattern (relevantResultScopeEvidence row),
   "confidence" .= (case relevantResultConfidence row of LowConfidence -> "low"; MediumConfidence -> "medium"; HighConfidence -> "high" :: Text),
   "evidence_count" .= length (relevantResultEvidence row)]

diagnosticsJson :: RelevantDiagnostics -> Aeson.Value
diagnosticsJson diagnostics = Aeson.object ["index_revision" .= relevantIndexRevision diagnostics,
  "index_preparation" .= relevantIndexPreparation diagnostics,
  "prepare_index_ms" .= relevantPrepareIndexMs diagnostics,
  "retrieve_candidates_ms" .= relevantRetrieveCandidatesMs diagnostics,
  "rank_results_ms" .= relevantRankResultsMs diagnostics]

labelRanks :: Case -> [(Text, Double)] -> Aeson.Value
labelRanks c rows = Aeson.object
  ["expected" .= map rank (caseExpected c), "excluded" .= map rank (caseExcluded c)]
  where
    rank identifier = Aeson.object ["adr" .= identifier,
      "rank" .= fmap (+ 1) (findIndex ((== identifier) . fst) rows),
      "score" .= lookup identifier rows]

sanitizeCli :: Aeson.Value -> Aeson.Value
sanitizeCli value = either (const Aeson.Null) id $ Aeson.parseEither
  (Aeson.withObject "CLI result" $ \o -> do
    algorithm <- o .: "algorithm" :: Aeson.Parser Text
    results <- o .: "results" :: Aeson.Parser [Aeson.Value]
    rows <- mapM (Aeson.withObject "CLI row" $ \r -> do
      identifier <- r .: "adr" :: Aeson.Parser Text
      score <- r .: "score" :: Aeson.Parser Double
      pure (Aeson.object ["adr" .= identifier, "score" .= score])) results
    pure (Aeson.object ["algorithm" .= algorithm, "total_returned" .= length rows, "top" .= take 10 rows])) value

cacheRows :: FilePath -> IO Int
cacheRows root = do
  let directory = root </> "adrai" </> "text-embeddings-v1"
  exists <- doesDirectoryExist directory
  if not exists then pure 0 else do
    files <- listDirectory directory
    fmap sum $ forM [directory </> name | name <- files, takeExtension name == ".sqlite"] $ \path ->
      (bracket (open path) close $ \connection -> do
        rows <- query_ connection "SELECT count(*) FROM embedding" :: IO [Only Int]
        pure (sum [count | Only count <- rows])) `catch` (\(_ :: SomeException) -> pure 0)

modelInfo :: TextEmbeddingConfig -> IO Aeson.Value
modelInfo config = (do
  manager <- HTTP.newManager (HTTP.managerSetProxy HTTP.noProxy HTTP.defaultManagerSettings)
  request <- HTTP.parseRequest (Text.unpack (textEmbeddingEndpoint config <> "/info"))
  response <- HTTP.httpLbs (request {HTTP.responseTimeout = HTTP.responseTimeoutMicro (textEmbeddingTimeoutMs config * 1000)}) manager
  unless (statusCode (HTTP.responseStatus response) == 200) (fail "model info unavailable")
  either fail pure (Aeson.eitherDecode (HTTP.responseBody response)))
  `catch` (\(_ :: SomeException) -> pure Aeson.Null)

configJson :: TextEmbeddingConfig -> Aeson.Value
configJson config = Aeson.object ["endpoint" .= textEmbeddingEndpoint config,
  "model" .= textEmbeddingModel config, "timeout_ms" .= textEmbeddingTimeoutMs config,
  "batch_size" .= textEmbeddingBatchSize config, "optional_phase_limit_ms" .= (30000 :: Int)]

writeConfig :: FilePath -> Maybe TextEmbeddingConfig -> IO ()
writeConfig rootInput configured = do
  root <- guardLocalPath rootInput
  createDirectoryIfMissing True (root </> "adrai")
  forM_ configured $ \config -> TextIO.writeFile (root </> "adrai" </> "config.toml") (Text.unlines
    (["[text_embedding]", "endpoint = " <> quoted (textEmbeddingEndpoint config),
      "timeout_ms = " <> Text.pack (show (textEmbeddingTimeoutMs config)),
      "batch_size = " <> Text.pack (show (textEmbeddingBatchSize config))]
      <> maybe [] (\model -> ["model = " <> quoted model]) (textEmbeddingModel config)))
  where quoted = Encoding.decodeUtf8 . LBS.toStrict . Aeson.encode

withRoots :: FilePath -> FilePath -> IO a -> IO a
withRoots configInput cacheInput action = do
  config <- guardLocalPath configInput
  cache <- guardLocalPath cacheInput
  oldConfig <- lookupEnv "XDG_CONFIG_HOME"
  oldCache <- lookupEnv "XDG_CACHE_HOME"
  createDirectoryIfMissing True config
  createDirectoryIfMissing True cache
  setEnv "XDG_CONFIG_HOME" config
  setEnv "XDG_CACHE_HOME" cache
  let guarded = do
        observedConfig <- textEmbeddingConfigPath
        observedCache <- getXdgDirectory XdgCache ("adrai" </> "text-embeddings-v1")
        unless (normalise observedConfig == normalise (config </> "adrai" </> "config.toml")
          && normalise observedCache == normalise (cache </> "adrai" </> "text-embeddings-v1"))
          (fail "process-local XDG roots did not isolate configuration and cache")
        action
  guarded `finally` (restore "XDG_CONFIG_HOME" oldConfig >> restore "XDG_CACHE_HOME" oldCache)
  where restore name = maybe (unsetEnv name) (setEnv name)

requireArtifact :: FilePath -> IO FilePath
requireArtifact artifactInput = do
  artifact <- guardLocalPath artifactInput
  value <- readJson (artifact </> "artifact.json") :: IO Aeson.Value
  schema <- parseValue (Aeson.withObject "artifact" (.: "schema")) value
  unless (schema == ("adrai/relevance-fixture/v1" :: Text)) (fail "unrecognized artifact ownership receipt")
  pure artifact

disablePush :: FilePath -> IO ()
disablePush repository = voidRun "git" ["-C", repository, "remote", "set-url", "--push", "origin", "disabled://retrieval-fixture"]

cli :: FilePath -> FilePath -> [String] -> IO Aeson.Value
cli executable repository args = run executable (["--repo", repository] <> args) >>= either fail pure . Aeson.eitherDecodeStrict' . Encoding.encodeUtf8 . Text.pack

run :: FilePath -> [String] -> IO String
run executable args = do
  (code, output, problem) <- readCreateProcessWithExitCode (proc executable args) ""
  case code of
    ExitSuccess -> pure output
    _ -> fail (takeFileName executable <> " failed: " <> take 1200 problem)

voidRun :: FilePath -> [String] -> IO ()
voidRun executable args = run executable args >> pure ()

must :: Show e => Either e a -> IO a
must = either (fail . show) pure

parseValue :: (Aeson.Value -> Aeson.Parser a) -> Aeson.Value -> IO a
parseValue parser = either fail pure . Aeson.parseEither parser

readJson :: Aeson.FromJSON a => FilePath -> IO a
readJson path = LBS.readFile path >>= either fail pure . Aeson.eitherDecode

writeJson :: Aeson.ToJSON a => FilePath -> a -> IO ()
writeJson path value = do
  safePath <- guardLocalPath path
  LBS.writeFile safePath (Aeson.encode value <> "\n")

timed :: IO a -> IO (Double, a)
timed action = do
  start <- getMonotonicTimeNSec
  value <- action
  end <- getMonotonicTimeNSec
  pure (fromIntegral (end - start) / 1000000, value)

-- Reject configuration roots and aliases before any Git operation or artifact
-- write. canonicalizePath resolves existing symlinks/junctions and prefixes of
-- not-yet-created destinations; Windows comparisons ignore path case.
guardLocalPath :: FilePath -> IO FilePath
guardLocalPath input = do
  resolved <- canonicalizePath =<< makeAbsolute input
  userHome <- getHomeDirectory
  configured <- lookupEnv "CODEX_HOME"
  roots <- mapM canonicalizePath ((userHome </> ".codex") : [root | Just root <- [configured], not (null root)])
  when (any (\root -> pathParts root `List.isPrefixOf` pathParts resolved) roots)
    (fail "Codex configuration directories are not repository or artifact targets")
  pure resolved

pathsEqual :: FilePath -> FilePath -> Bool
pathsEqual left right = pathParts left == pathParts right

pathParts :: FilePath -> [String]
pathParts = map normalizeCase . splitDirectories . normalise
  where normalizeCase = if os == "mingw32" then map toLower else id

instruction :: Text
instruction = "Instruct: Given a web search query, retrieve relevant passages that answer the query\nQuery: "

documents :: [Document]
documents =
  [ Document "literal" "Record chromatic spectra" "Keep stellar observations reproducible." "Measure absorption bands before classifying a star." ["fixture/scoped/opaque.rs"],
    Document "subtree" "Keep provenance for collected samples" "Every collected sample retains its origin." "Record collection provenance when handling samples." ["fixture/scoped/**"],
    Document "global" "Keep operation names descriptive" "Repository operations use clear names." "Prefer descriptive public operation names." ["**"],
    Document "durability" "Synchronize the write-ahead journal before acknowledgement" "Committed commands survive process termination." "Acknowledge mutations only after fsync completes. Replay durable log entries to reconstruct state during recovery." [],
    Document "authorization" "Validate bearer credentials at protected endpoints" "Reject expired tokens and check caller permissions." "Verify cryptographic signatures and expiration before allowing access to restricted HTTP handlers." [],
    Document "telemetry" "Carry correlation identifiers through structured telemetry" "Link activity across component boundaries." "Propagate a request identifier into JSON logs and spans so operators can follow one operation across services." [],
    Document "queue" "Acknowledge durable queue work after commit" "Handlers retry delivery without applying a mutation twice." "Acknowledge durable queue messages only after transactional processing commits. Deduplicate redeliveries with stable message keys." [],
    Document "navigation" "Choose land routes by traversal cost" "Travel follows connected neighboring settlements." "Use a weighted graph to plan movement through passable terrain. Route selection accounts for distance and road quality." []
  ]

sources :: [(FilePath, Text)]
sources =
  [ ("fixture/scoped/opaque.rs", ""), ("fixture/scoped/short.toml", "x=1\n"),
    ("fixture/semantic/recovery.rs", "// A crash must not make a successful request disappear.\n// After a relaunch, previously accepted updates must still be there.\nfn reconcile(xs: &[u8]) -> usize { xs.len() }\n"),
    ("fixture/semantic/access.py", "# A caller may enter a protected route only while its signed credential remains valid.\ndef allow(subject, permission):\n    return subject.is_current() and subject.can(permission)\n"),
    ("fixture/semantic/telemetry.yaml", "observability:\n  purpose: follow one action while it crosses several services\n  context: put the same correlation value into each diagnostic event\n"),
    ("fixture/lexical/worker.hs", "acknowledgeDurableQueue :: Job -> IO ()\nacknowledgeDurableQueue job = commitOnce job >>= confirmDelivery\n"),
    ("fixture/negative/graphics.ts", "// Blend neighboring pixels into a smooth color gradient on the canvas.\nfunction interpolate(left, right, amount) { return left + (right - left) * amount; }\n")
  ]

syntheticCases :: [Case]
syntheticCases =
  [ Case "literal-scope-only" (Just "fixture/scoped/opaque.rs") Nothing ["literal", "subtree"] ["global"],
    Case "subtree-short-source" (Just "fixture/scoped/short.toml") Nothing ["subtree"] ["global"],
    Case "durability-paraphrase" (Just "fixture/semantic/recovery.rs") (Just "A crash must not make a successful request disappear") ["durability"] ["navigation", "authorization"],
    Case "authorization-paraphrase" (Just "fixture/semantic/access.py") (Just "A caller may enter a protected route only while its signed credential remains valid") ["authorization"] ["navigation", "durability"],
    Case "telemetry-yaml" (Just "fixture/semantic/telemetry.yaml") (Just "Follow one action while it crosses several services") ["telemetry"] ["navigation", "authorization"],
    Case "lexical-worker" (Just "fixture/lexical/worker.hs") Nothing ["queue"] ["navigation", "authorization"],
    Case "graphics-hard-negative" (Just "fixture/negative/graphics.ts") (Just "Blend pixels into a smooth color gradient on a canvas") [] ["durability", "authorization", "telemetry", "queue", "navigation"]
  ]
