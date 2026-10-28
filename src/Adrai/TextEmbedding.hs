{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Optional user-owned TEI configuration and a disposable embedding cache.
-- Nothing in this module participates in immutable compiler cache validity.
module Adrai.TextEmbedding
  ( TextEmbeddingConfig (..),
    EmbeddingProvider,
    EmbeddingRetrieval (..),
    EmbeddingOutcome (..),
    EmbeddingOutcomeProvider,
    EmbeddingPreparation (..),
    EmbeddingPreparationStatus (..),
    EmbeddingPreparationReason (..),
    embeddingOutcomeRetrieval,
    retrieveTextEmbeddingsOutcome,
    retrieveTextEmbeddingsOutcomeWith,
    prepareTextEmbeddings,
    prepareTextEmbeddingsWith,
    parseTextEmbeddingConfig,
    textEmbeddingConfigPath,
    loadTextEmbeddingConfig,
    retrieveTextEmbeddings,
    retrieveTextEmbeddingsWith,
  ) where

import Adrai.Format (renderDigest)
import Adrai.Provenance (sha256Digest)
import Control.Exception (Exception, SomeException, SomeAsyncException, bracket, bracketOnError, catch, fromException, mask, throwIO)
import Control.Monad (forM, forM_, unless, when)
import Data.Char (isDigit, isHexDigit)
import Data.Bits ((.|.), shiftL)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word32)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Float (castWord32ToFloat)
import Text.Read (readMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Database.SQLite.Simple
import Foreign.C.Types (CInt (..))
#ifdef mingw32_HOST_OS
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr)
import qualified Network.Socket.Address as Address
#endif
import qualified Network.Socket.ByteString as SocketBS
import qualified Network.HTTP.Client as HTTP
import qualified Network.Socket as Socket
import Network.HTTP.Types.Status (statusCode)
import System.Directory (XdgDirectory (XdgCache, XdgConfig), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.FilePath ((</>))
import System.IO (IOMode (ReadMode), withBinaryFile)
import System.Timeout (timeout)
import qualified Toml

data TextEmbeddingConfig = TextEmbeddingConfig
  { textEmbeddingEndpoint :: Text,
    textEmbeddingModel :: Maybe Text,
    textEmbeddingTimeoutMs :: Int,
    textEmbeddingBatchSize :: Int
  } deriving (Eq, Show)

type EmbeddingProvider = [(Text, Text)] -> [Text] -> Int -> IO (Maybe EmbeddingRetrieval)

type EmbeddingOutcomeProvider = [(Text, Text)] -> [Text] -> Int -> IO EmbeddingOutcome

data EmbeddingOutcome = EmbeddingReady EmbeddingRetrieval | EmbeddingNotReady EmbeddingPreparation
  deriving (Eq, Show)

data EmbeddingPreparationStatus = PreparationComplete | PreparationPending | PreparationUnavailable
  deriving (Eq, Show)

data EmbeddingPreparationReason
  = CorpusMissing | BudgetExhausted | NotConfigured | InvalidConfiguration
  | ServiceUnavailable | InvalidResponse | CacheUnavailable | NativeUnavailable
  deriving (Eq, Show)

-- Counts describe unique corpus texts, never request-local query/source texts.
-- Nothing means that the live model/cache could not yet be inspected.
data EmbeddingPreparation = EmbeddingPreparation
  { embeddingPreparationStatus :: EmbeddingPreparationStatus,
    embeddingPreparationIdentity :: Maybe Text,
    embeddingPreparationDimensions :: Maybe Int,
    embeddingPreparationTotalTexts :: Int,
    embeddingPreparationCacheHits :: Maybe Int,
    embeddingPreparationPreparedTexts :: Int,
    embeddingPreparationRemainingTexts :: Maybe Int,
    embeddingPreparationReason :: Maybe EmbeddingPreparationReason
  } deriving (Eq, Show)

data EmbeddingFailure = EmbeddingFailure EmbeddingPreparationReason deriving (Show)
instance Exception EmbeddingFailure

embeddingOutcomeRetrieval :: EmbeddingOutcome -> Maybe EmbeddingRetrieval
embeddingOutcomeRetrieval (EmbeddingReady retrieval) = Just retrieval
embeddingOutcomeRetrieval (EmbeddingNotReady _) = Nothing

data EmbeddingRetrieval = EmbeddingRetrieval
  { embeddingRetrievalIdentity :: Text,
    embeddingRetrievalDimensions :: Int,
    embeddingRetrievalScores :: [Map Text Double]
  } deriving (Eq, Show)

textEmbeddingConfigPath :: IO FilePath
textEmbeddingConfigPath = getXdgDirectory XdgConfig ("adrai" </> "config.toml")

-- | This is intentionally independent of the committed repository TOML.
parseTextEmbeddingConfig :: Text -> Either Text (Maybe TextEmbeddingConfig)
parseTextEmbeddingConfig input = do
  parsed <- either (Left . Text.pack) Right (Toml.parse input)
  let Toml.MkTable entries = Toml.forgetTableAnns parsed
      table = Map.map snd entries
  case Map.lookup "text_embedding" table of
    Nothing -> Right Nothing
    Just (Toml.Table' _ (Toml.MkTable nested)) -> do
      let values = Map.map snd nested
      endpoint <- case Map.lookup "endpoint" values of
        Just (Toml.Text' _ value) | validEndpoint value -> Right (Text.dropWhileEnd (== '/') value)
        _ -> Left "text_embedding.endpoint must use http:// with localhost or a numeric IP and no credentials, query, or fragment"
      model <- case Map.lookup "model" values of
        Nothing -> Right Nothing
        Just (Toml.Text' _ value) | not (Text.null (Text.strip value)) -> Right (Just value)
        _ -> Left "text_embedding.model must be a nonempty string"
      timeoutMs <- intSetting values "timeout_ms" 2000 100 10000
      batchSize <- intSetting values "batch_size" 16 1 64
      unless (all (`elem` ["endpoint", "model", "timeout_ms", "batch_size"]) (Map.keys values)) (Left "unknown text_embedding setting")
      Right (Just (TextEmbeddingConfig endpoint model timeoutMs batchSize))
    Just _ -> Left "text_embedding must be a TOML table"
  where
    intSetting :: Map Text Toml.Value -> Text -> Int -> Int -> Int -> Either Text Int
    intSetting values key fallback low high = case Map.lookup key values of
      Nothing -> Right fallback
      Just (Toml.Integer' _ value) | value >= toInteger low && value <= toInteger high -> Right (fromInteger value)
      _ -> Left ("invalid text_embedding." <> key)

-- Restrict optional local transport to addresses that never invoke DNS.
validEndpoint :: Text -> Bool
validEndpoint value =
  "http://" `Text.isPrefixOf` value
    && not (Text.any (`elem` ("@?#\\\r\n\t " :: String)) value)
    && case HTTP.parseRequest (Text.unpack value) :: Either SomeException HTTP.Request of
      Left _ -> False
      Right request -> localHost (Encoding.decodeUtf8 (HTTP.host request))
  where
    localHost host = host == "localhost" || ipv4 host || ipv6 host
    ipv4 host = let parts = Text.splitOn "." host in length parts == 4 && all octet parts
    octet part = not (Text.null part) && Text.all isDigit part
      && maybe False (\n -> n >= 0 && n <= 255) (readMaybe (Text.unpack part) :: Maybe Int)
    ipv6 host = let address = Text.dropAround (`elem` ("[]" :: String)) host
      in Text.count ":" address >= 2 && Text.all (\c -> isHexDigit c || c == ':' || c == '.') address

#ifdef mingw32_HOST_OS
foreign import ccall safe "adrai_connect_bounded" boundedConnect :: CInt -> Ptr a -> CInt -> CInt -> IO CInt
#endif

loadTextEmbeddingConfig :: IO (Maybe TextEmbeddingConfig)
loadTextEmbeddingConfig = either (const Nothing) id <$> loadTextEmbeddingConfigResult

loadTextEmbeddingConfigResult :: IO (Either EmbeddingPreparationReason (Maybe TextEmbeddingConfig))
loadTextEmbeddingConfigResult = (do
    path <- textEmbeddingConfigPath
    exists <- doesFileExist path
    Right <$> if not exists then pure Nothing else withBinaryFile path ReadMode (\handle -> do
      bytes <- BS.hGet handle 65537
      when (BS.length bytes > 65536) (throwIO (EmbeddingFailure InvalidConfiguration))
      text <- either (const (throwIO (EmbeddingFailure InvalidConfiguration))) pure (Encoding.decodeUtf8' bytes)
      either (const (throwIO (EmbeddingFailure InvalidConfiguration))) pure (parseTextEmbeddingConfig text)))
  `catch` \(problem :: SomeException) -> case fromException problem :: Maybe SomeAsyncException of
    Just _ -> throwIO problem
    Nothing -> pure (Left InvalidConfiguration)

foreign import ccall unsafe "adrai_register_sqlite_vec" registerSqliteVec :: IO CInt

-- | Each query checks the live model and embeds its request-local inputs before
-- reading cached corpus vectors. A warm cache can never activate an offline model.
retrieveTextEmbeddings :: [(Text, Text)] -> [Text] -> Int -> IO (Maybe EmbeddingRetrieval)
retrieveTextEmbeddings corpus queries limit = embeddingOutcomeRetrieval <$> retrieveTextEmbeddingsOutcome corpus queries limit

retrieveTextEmbeddingsOutcome :: EmbeddingOutcomeProvider
retrieveTextEmbeddingsOutcome corpus queries limit
  | null corpus || null queries || limit <= 0 = pure (EmbeddingNotReady (emptyPreparation corpus))
  | otherwise = configuredOperation corpus $ \config cache -> retrieveTextEmbeddingsOutcomeWith config cache corpus queries limit

prepareTextEmbeddings :: Int -> [(Text, Text)] -> IO EmbeddingPreparation
prepareTextEmbeddings budget corpus
  | null corpus = pure (emptyPreparation corpus)
  | otherwise = outcomePreparation <$> configuredOperation corpus (\config cache -> runEmbeddingOperation True budget config cache corpus [] 0)

prepareTextEmbeddingsWith :: TextEmbeddingConfig -> FilePath -> Int -> [(Text, Text)] -> IO EmbeddingPreparation
prepareTextEmbeddingsWith config cache budget corpus = outcomePreparation <$> runEmbeddingOperation True budget config cache corpus [] 0

configuredOperation :: [(Text, Text)] -> (TextEmbeddingConfig -> FilePath -> IO EmbeddingOutcome) -> IO EmbeddingOutcome
configuredOperation corpus action = do
  config <- loadTextEmbeddingConfigResult
  case config of
    Left reason -> unavailable reason
    Right Nothing -> unavailable NotConfigured
    Right (Just configured) -> (do
        cache <- getXdgDirectory XdgCache ("adrai" </> "text-embeddings-v1")
        action configured cache)
      `catch` \(problem :: SomeException) -> case fromException problem :: Maybe SomeAsyncException of
        Just _ -> throwIO problem
        Nothing -> unavailable CacheUnavailable
  where
    unavailable reason = pure (EmbeddingNotReady ((initialPreparation corpus) {embeddingPreparationReason = Just reason}))

initialPreparation :: [(Text, Text)] -> EmbeddingPreparation
initialPreparation corpus = EmbeddingPreparation PreparationUnavailable Nothing Nothing
  (Map.size (Map.fromList [(text, ()) | (_, text) <- corpus])) Nothing 0 Nothing Nothing

emptyPreparation :: [(Text, Text)] -> EmbeddingPreparation
emptyPreparation corpus = (initialPreparation corpus)
  {embeddingPreparationStatus = PreparationComplete, embeddingPreparationCacheHits = Just 0, embeddingPreparationRemainingTexts = Just 0}

outcomePreparation :: EmbeddingOutcome -> EmbeddingPreparation
outcomePreparation (EmbeddingNotReady progress) = progress
outcomePreparation (EmbeddingReady _) = error "preparation unexpectedly ran KNN"

retrieveTextEmbeddingsWith :: TextEmbeddingConfig -> FilePath -> [(Text, Text)] -> [Text] -> Int -> IO (Maybe EmbeddingRetrieval)
retrieveTextEmbeddingsWith config cacheDirectory corpus queries limit =
  embeddingOutcomeRetrieval <$> retrieveTextEmbeddingsOutcomeWith config cacheDirectory corpus queries limit

retrieveTextEmbeddingsOutcomeWith :: TextEmbeddingConfig -> FilePath -> EmbeddingOutcomeProvider
retrieveTextEmbeddingsOutcomeWith = runEmbeddingOperation False 30000

runEmbeddingOperation :: Bool -> Int -> TextEmbeddingConfig -> FilePath -> [(Text, Text)] -> [Text] -> Int -> IO EmbeddingOutcome
runEmbeddingOperation full budget config cacheDirectory corpus queries limit
  | null corpus || not full && (null queries || limit <= 0) = pure (EmbeddingNotReady (emptyPreparation corpus))
  | textEmbeddingBatchSize config < 1 || textEmbeddingBatchSize config > 64
      || textEmbeddingTimeoutMs config < 100 || textEmbeddingTimeoutMs config > 10000
      || not (validEndpoint (textEmbeddingEndpoint config)) || budget < 100 || budget > 600000 =
      pure (EmbeddingNotReady ((initialPreparation corpus) {embeddingPreparationReason = Just InvalidConfiguration}))
  | otherwise = do
      progressRef <- newIORef (initialPreparation corpus)
      failureRef <- newIORef ServiceUnavailable
      started <- getMonotonicTimeNSec
      let operationDeadline = started + fromIntegral budget * 1000000
      deadlineRef <- newIORef operationDeadline
      let failed reason = do
            progress <- readIORef progressRef
            pure (EmbeddingNotReady (progress {embeddingPreparationStatus = if reason == BudgetExhausted then PreparationPending else PreparationUnavailable, embeddingPreparationReason = Just reason}))
          handleFailure (problem :: SomeException) = do
            rethrowCancellation problem
            deadline <- readIORef deadlineRef
            now <- getMonotonicTimeNSec
            reason <- case embeddingFailureReason problem of
              Just category -> pure category
              Nothing | now >= deadline -> pure BudgetExhausted
              Nothing -> readIORef failureRef
            Just <$> failed reason
      result <- (timeout (budget * 1000) $ do
        manager <- HTTP.newManager (HTTP.managerSetProxy HTTP.noProxy HTTP.defaultManagerSettings
          { HTTP.managerIdleConnectionCount = 0,
            HTTP.managerRawConnection = pure (\_ host port -> do
              remaining <- remainingTimeout deadlineRef
              localConnection remaining host port)
          })
        native <- registerSqliteVec
        unless (native == 0) (throwIO (EmbeddingFailure NativeUnavailable))
        info <- requestJson deadlineRef manager "/info" (Nothing :: Maybe Aeson.Value) >>= decodeInfo
        let (model, revision) = info
        unless (maybe True (== model) (textEmbeddingModel config)) (throwIO (EmbeddingFailure InvalidResponse))
        let ephemeral = if full then ["adrai embedding dimension probe"] else queries
        queryVectors <- embedTexts deadlineRef manager ephemeral
        dimensions <- validateVectors Nothing (length ephemeral) queryVectors
        let identity = digest (LBS.toStrict (Aeson.encode (textEmbeddingEndpoint config, model, revision, dimensions, "tei-normalized-f32-cosine-truncate-v1" :: Text)))
            inputs = Map.fromList [(digest (Encoding.encodeUtf8 text), text) | (_, text) <- corpus]
        unless (Map.size (Map.fromList corpus) == length corpus) (throwIO (EmbeddingFailure InvalidResponse))
        writeIORef progressRef ((initialPreparation corpus) {embeddingPreparationIdentity = Just identity, embeddingPreparationDimensions = Just dimensions})
        writeIORef failureRef CacheUnavailable
        createDirectoryIfMissing True cacheDirectory
        -- ':' is an NTFS stream separator, not a portable filename character.
        let cacheName = Text.unpack (Text.replace ":" "-" identity) <> ".sqlite"
        bracket (open (cacheDirectory </> cacheName)) close $ \cache -> do
          execute_ cache "PRAGMA busy_timeout=250"
          execute_ cache "CREATE TABLE IF NOT EXISTS embedding(input_hash TEXT PRIMARY KEY,vector BLOB NOT NULL)"
          cached <- readCachedVectors cache dimensions (Map.keys inputs)
          let missing = [(key, text) | (key, text) <- Map.toAscList inputs, Map.findWithDefault Nothing key cached == Nothing]
              remaining = length missing
              progress = EmbeddingPreparation PreparationPending (Just identity) (Just dimensions) (Map.size inputs) (Just (Map.size inputs - remaining)) 0 (Just remaining) (Just CorpusMissing)
          writeIORef progressRef progress
          if not full && remaining > 32 then pure (EmbeddingNotReady progress) else do
            now <- getMonotonicTimeNSec
            unless full (writeIORef deadlineRef (min operationDeadline (now + 1000000000)))
            let publish chunk = do
                  writeIORef failureRef ServiceUnavailable
                  newVectors <- embedTexts deadlineRef manager (map snd chunk)
                  _ <- validateVectors (Just dimensions) (length chunk) newVectors
                  let rows = zip (map fst chunk) (map packFloat32 newVectors)
                  writeIORef failureRef CacheUnavailable
                  -- Insertion remains cancellable/rollback-safe. Once COMMIT
                  -- succeeds, publish matching counts before delivering a cancel.
                  mask $ \restore -> do
                    withTransaction cache $ restore (executeMany cache "INSERT OR REPLACE INTO embedding(input_hash,vector) VALUES (?,?)" rows)
                    previous <- readIORef progressRef
                    writeIORef progressRef (previous {embeddingPreparationPreparedTexts = embeddingPreparationPreparedTexts previous + length chunk,
                      embeddingPreparationRemainingTexts = subtract (length chunk) <$> embeddingPreparationRemainingTexts previous})
                  pure (Map.fromList rows)
            repaired <- if full
              then forM_ (batches (textEmbeddingBatchSize config) missing) (\chunk -> publish chunk >> pure ()) >> pure (Just [])
              else timeout 1000000 (mapM publish (batches (textEmbeddingBatchSize config) missing))
            writeIORef deadlineRef operationDeadline
            case repaired of
              Nothing -> failed BudgetExhausted
              Just chunks -> do
                complete <- readIORef progressRef
                let ready = complete {embeddingPreparationStatus = PreparationComplete, embeddingPreparationReason = Nothing}
                    stored = Map.union (Map.unions chunks) (Map.mapMaybe id cached)
                writeIORef progressRef ready
                if full then pure (EmbeddingNotReady ready) else do
                  writeIORef failureRef NativeUnavailable
                  retrieveVectors dimensions identity stored queryVectors) `catch` handleFailure
      maybe (failed BudgetExhausted) pure result
  where
    retrieveVectors dimensions identity stored queryVectors =
          bracket (open ":memory:") close $ \vectors -> do
            -- Eligibility precedes KNN: only the caller's eligible corpus is
            -- inserted. No post-K filter can hide eligible neighbours.
            execute_ vectors (Query ("CREATE VIRTUAL TABLE candidates USING vec0(item_id TEXT PRIMARY KEY,embedding float[" <> Text.pack (show dimensions) <> "] distance_metric=cosine)"))
            withTransaction vectors $ forM_ (batches 256 (Map.toAscList (Map.fromList corpus))) $ \chunk -> do
              rows <- forM chunk $ \(key, text) ->
                case Map.lookup (digest (Encoding.encodeUtf8 text)) stored of
                  Nothing -> fail "incomplete embedding cache"
                  Just bytes -> pure (key, bytes)
              executeMany vectors "INSERT INTO candidates(item_id,embedding) VALUES (?,?)" rows
            scores <- forM queryVectors $ \vector -> do
              rows <- query vectors "SELECT item_id,distance FROM candidates WHERE embedding MATCH ? AND k=? ORDER BY distance" (packFloat32 vector, min limit (length corpus)) :: IO [(Text, Double)]
              unless (all (finite . snd) rows) (fail "nonfinite sqlite-vec distance")
              pure (Map.fromList [(key, max (-1) (min 1 (1 - distance))) | (key, distance) <- rows])
            pure (EmbeddingReady (EmbeddingRetrieval identity dimensions scores))
    remainingTimeout deadlineRef = do
      deadline <- readIORef deadlineRef
      now <- getMonotonicTimeNSec
      when (now >= deadline) (throwIO (EmbeddingFailure BudgetExhausted))
      -- Ceil to the transport's millisecond granularity: flooring would expire
      -- a request before the monotonic operation/repair deadline was reached.
      pure (min (textEmbeddingTimeoutMs config) (fromIntegral ((deadline - now + 999999) `div` 1000000)))
    -- HTTP's public connection hook keeps all sockets under bracketed ownership.
    -- Numeric getAddrInfo cannot wait on a resolver; localhost has one IPv4 loopback
    -- address. Windows connect uses nonblocking select, never concurrent closesocket.
    localConnection deadlineMs host port = do
      let numericHost = if host == "localhost" then "127.0.0.1" else dropBrackets host
          hints = Socket.defaultHints {Socket.addrFlags = [Socket.AI_NUMERICHOST, Socket.AI_NUMERICSERV], Socket.addrSocketType = Socket.Stream}
      addresses <- Socket.getAddrInfo (Just hints) (Just numericHost) (Just (show port))
      case addresses of
        [] -> fail "invalid local embedding address"
        address : _ -> bracketOnError
          (Socket.socket (Socket.addrFamily address) Socket.Stream (Socket.addrProtocol address)) Socket.close $ \socket -> do
            Socket.setSocketOption socket Socket.NoDelay 1
            Socket.setSocketOption socket Socket.RecvTimeOut (deadlineMs * 1000)
            Socket.setSocketOption socket Socket.SendTimeOut (deadlineMs * 1000)
#ifdef mingw32_HOST_OS
            let target = Socket.addrAddress address
                size = Address.sizeOfSocketAddress target
            code <- allocaBytes size $ \pointer -> do
              Address.pokeSocketAddress pointer target
              Socket.withFdSocket socket $ \fd -> boundedConnect fd pointer (fromIntegral size) (fromIntegral deadlineMs)
            unless (code == 0) (fail "embedding connection unavailable")
#else
            connected <- timeout (deadlineMs * 1000) (Socket.connect socket (Socket.addrAddress address))
            unless (connected == Just ()) (fail "embedding connection timeout")
#endif
            HTTP.makeConnection (SocketBS.recv socket 8192) (SocketBS.sendAll socket) (Socket.close socket)
      where
        dropBrackets ('[' : rest) = reverse (dropWhile (== ']') (reverse rest))
        dropBrackets value = value
    
    requestJson deadlineRef manager suffix body = do
      remaining <- remainingTimeout deadlineRef
      base <- HTTP.parseRequest (Text.unpack (textEmbeddingEndpoint config <> suffix))
      let request = base
            { HTTP.method = maybe "GET" (const "POST") body,
              HTTP.requestHeaders = [("Content-Type", "application/json")],
              HTTP.requestBody = maybe (HTTP.RequestBodyBS "") (HTTP.RequestBodyLBS . Aeson.encode) body,
              HTTP.responseTimeout = HTTP.responseTimeoutMicro (remaining * 1000),
              HTTP.redirectCount = 0
            }
      outcome <- timeout (remaining * 1000) $
        HTTP.withResponse request manager $ \response -> do
          unless (statusCode (HTTP.responseStatus response) == 200) (throwIO (EmbeddingFailure ServiceUnavailable))
          bytes <- readBounded (HTTP.responseBody response) (8 * 1024 * 1024)
          either (const (throwIO (EmbeddingFailure InvalidResponse))) pure (Aeson.eitherDecodeStrict' bytes)
      maybe (throwIO (EmbeddingFailure (if remaining < textEmbeddingTimeoutMs config then BudgetExhausted else ServiceUnavailable))) pure outcome
    decodeInfo value = either (const (throwIO (EmbeddingFailure InvalidResponse))) pure (Aeson.parseEither parser value)
      where
        parser = Aeson.withObject "TEI info" $ \o -> do
          model <- o .: "model_id"
          revision <- o .:? "model_sha"
          when (Text.null model) (fail "empty model")
          pure (model, revision :: Maybe Text)
    embedTexts _ _ [] = pure []
    embedTexts deadlineRef manager texts = do
      unless (all ((<= 32768) . BS.length . Encoding.encodeUtf8) texts) (throwIO (EmbeddingFailure InvalidResponse))
      fmap concat $ forM (batches (textEmbeddingBatchSize config) texts) $ \batch -> do
        value <- requestJson deadlineRef manager "/embed" (Just (Aeson.object ["inputs" .= batch, "normalize" .= True, "truncate" .= True]))
        vectors <- either (const (throwIO (EmbeddingFailure InvalidResponse))) pure (Aeson.parseEither Aeson.parseJSON value)
        _ <- validateVectors Nothing (length batch) vectors
        pure vectors

-- http-client can wrap connection-hook exceptions. Cancellation must remain
-- exceptional through those wrappers; typed validation failures keep their cause.
rethrowCancellation :: SomeException -> IO ()
rethrowCancellation problem = case fromException problem :: Maybe SomeAsyncException of
  Just _ -> throwIO problem
  Nothing -> maybe (pure ()) rethrowCancellation (transportCause problem)

embeddingFailureReason :: SomeException -> Maybe EmbeddingPreparationReason
embeddingFailureReason problem = case fromException problem of
  Just (EmbeddingFailure category) -> Just category
  Nothing -> transportCause problem >>= embeddingFailureReason

transportCause :: SomeException -> Maybe SomeException
transportCause problem = case fromException problem of
  Just (HTTP.HttpExceptionRequest _ (HTTP.InternalException cause)) -> Just cause
  Just (HTTP.HttpExceptionRequest _ (HTTP.ConnectionFailure cause)) -> Just cause
  _ -> Nothing

-- Read only the requested text hashes, with a bounded number of SQL parameters.
-- Reconstruct by hash because SQLite does not promise the order of IN results.
-- A missing, wrong-length or duplicated row remains an unusable cache entry.
readCachedVectors :: Connection -> Int -> [Text] -> IO (Map Text (Maybe BS.ByteString))
readCachedVectors cache dimensions hashes =
  fmap Map.unions $ forM (batches 256 hashes) $ \chunk -> do
    let statement = Query ("SELECT input_hash,vector FROM embedding WHERE input_hash IN (" <> Text.intercalate "," (replicate (length chunk) "?") <> ")")
    rows <- query cache statement chunk :: IO [(Text, BS.ByteString)]
    pure (Map.fromListWith (\_ _ -> Nothing)
      [(key, if validCachedVector dimensions bytes then Just bytes else Nothing) | (key, bytes) <- rows])

validCachedVector :: Int -> BS.ByteString -> Bool
validCachedVector dimensions bytes = BS.length bytes == dimensions * 4 && go 0 False
  where
    go offset nonzero
      | offset == BS.length bytes = nonzero
      | otherwise = let value = castWord32ToFloat (word offset)
         in not (isNaN value || isInfinite value)
              && if value /= 0 then go (offset + 4) True else go (offset + 4) nonzero
    word offset = fromIntegral (BS.index bytes offset)
      .|. (fromIntegral (BS.index bytes (offset + 1)) `shiftL` 8)
      .|. (fromIntegral (BS.index bytes (offset + 2)) `shiftL` 16)
      .|. (fromIntegral (BS.index bytes (offset + 3)) `shiftL` 24) :: Word32

validateVectors :: Maybe Int -> Int -> [[Double]] -> IO Int
validateVectors expected count vectors = do
  let dimensions = case vectors of [] -> maybe 0 id expected; first : _ -> length first
  unless (length vectors == count && (count == 0 || dimensions > 0 && dimensions <= 8192)
    && maybe True (== dimensions) expected
    && all (\v -> length v == dimensions && all finite v
      && any (\x -> (realToFrac x :: Float) /= 0) v
      && all (\x -> finite (realToFrac (realToFrac x :: Float))) v) vectors) (throwIO (EmbeddingFailure InvalidResponse))
  pure dimensions

packFloat32 :: [Double] -> BS.ByteString
packFloat32 = LBS.toStrict . Builder.toLazyByteString . foldMap (Builder.floatLE . realToFrac)

finite :: Double -> Bool
finite x = not (isNaN x || isInfinite x)

digest :: BS.ByteString -> Text
digest = renderDigest . sha256Digest

batches :: Int -> [a] -> [[a]]
batches _ [] = []
batches n values = let (first, rest) = splitAt n values in first : batches n rest

readBounded :: HTTP.BodyReader -> Int -> IO BS.ByteString
readBounded reader limit = go [] 0
  where
    go chunks size = do
      chunk <- HTTP.brRead reader
      let next = size + BS.length chunk
      when (next > limit) (throwIO (EmbeddingFailure InvalidResponse))
      if BS.null chunk then pure (BS.concat (reverse chunks)) else go (chunk : chunks) next

