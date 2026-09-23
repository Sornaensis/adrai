{-# LANGUAGE OverloadedStrings #-}

module Adrai.WebArchiveDiagnosticTest (tests, ftsLockRegression, ftsBusyRecovery, committedBusyIndexWarning, mixedExactArchiveConsumers) where

import qualified Adrai.Compiler.CacheSelection as Cache
import Adrai.Git (RevisionSpec (..), discoverRepository, resolveRevision, systemGit)
import Adrai.Provenance (gitOidText)
import Adrai.Provenance.Ensure (openReadWriteExisting)
import qualified Adrai.Service.Runtime as Runtime
import Adrai.Sqlite (allFtsTargets, ftsTargetTable)
import qualified Adrai.Web.Api as Api
import Adrai.Web.Application (ApplicationServices (..), defaultApplicationServices)
import Adrai.Web.Server (RunningServer (..), ServerDependencies (..), withWebServer)
import qualified Adrai.Web.Security as Security
import qualified Adrai.WebServerTest as Server
import Control.Concurrent (MVar, newEmptyMVar, putMVar, readMVar, takeMVar, threadDelay, tryPutMVar)
import Control.Concurrent.Async (Async, async, mapConcurrently, poll, race, waitCatch, withAsync)
import Control.Exception (SomeAsyncException, SomeException, bracket, finally, fromException, throwIO, try)
import Control.Monad (forM_, unless)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString8
import qualified Data.ByteString.Lazy as LazyByteString
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word64)
import Database.SQLite.Simple (Only (..), execute_, query_, withTransaction)
import qualified Database.SQLite.Simple as SQLite (close)
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket (Family (AF_INET), ShutdownCmd (ShutdownBoth), SockAddr (SockAddrInet), Socket, SocketType (Stream), close, connect, defaultProtocol, shutdown, socket, tupleToHostAddress)
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.Process (CreateProcess (..), StdStream (NoStream), ProcessHandle, createProcess, getProcessExitCode, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)
import Text.Read (readMaybe)
import Numeric (readHex)

tests :: TestTree
tests = testCase "concurrent first exact HTTP searches avoid service failure" testColdExactArchive

ftsLockRegression :: TestTree
ftsLockRegression = testCase "concurrent exact FTS validators preserve full integrity results" testConcurrentExactFtsValidation

ftsBusyRecovery :: TestTree
ftsBusyRecovery = testCase "exact archive lock exhaustion is typed busy and recovers at the same OID" testExactArchiveBusyRecovery

committedBusyIndexWarning :: TestTree
committedBusyIndexWarning = testCase "injected archive busy preserves durable commit and types read 503" testCommittedBusyIndexWarning

mixedExactArchiveConsumers :: TestTree
mixedExactArchiveConsumers = testCase "physical exact archive contention types mixed HTTP consumers and recovers without cold compile" testMixedExactArchiveConsumers

testMixedExactArchiveConsumers :: IO ()
testMixedExactArchiveConsumers = do
  bounded <- timeout 40000000 $ Server.withSeededRepository $ \root -> do
    mixedPhase "seeded"
    repository <- discoverRepository systemGit root >>= either (const (assertFailure "mixed fixture discovery failed")) pure
    oid <- resolveRevision repository (RevisionSpec "HEAD") >>= either (const (assertFailure "mixed fixture HEAD failed")) pure
    archive <- Runtime.ensureExactArchive repository oid >>= either (const (assertFailure "mixed fixture archive failed")) pure
    mixedPhase "archive-valid"
    let target = gitOidText oid
        doctorPath = "/api/v1/doctor?at=" <> target
        searchPath = "/api/v1/search?q=Seeded&mode=fts&view=collapsed&limit=100&include_obsolete=false&shallow=false&at=" <> target
        relevantPath = "/api/v1/relevant?file=seed.txt&at=" <> target
    coldBranches <- newIORef (0 :: Int)
    producerEntries <- newIORef (0 :: Int)
    joinEntries <- newIORef (0 :: Int)
    firstJoined <- newEmptyMVar
    producerEntered <- newEmptyMVar
    releaseProducer <- newEmptyMVar
    peerJoined <- newEmptyMVar
    releaseJoiners <- newEmptyMVar
    holderEntered <- newEmptyMVar
    releaseHolder <- newEmptyMVar
    let recordColdBranch = atomicModifyIORef' coldBranches (\count -> (count + 1, ()))
        compileExact compileRepository compileOid = do
          entry <- atomicModifyIORef' producerEntries (\count -> let next = count + 1 in (next, next))
          if entry == 1 then putMVar producerEntered () >> takeMVar releaseProducer else pure ()
          Runtime.ensureExactArchiveWithColdPathObserver recordColdBranch compileRepository compileOid
        afterJoin = do
          entry <- atomicModifyIORef' joinEntries (\count -> let next = count + 1 in (next, next))
          if entry == 1 then putMVar firstJoined () else pure ()
          if entry >= 2 && entry <= 4 then do
            putMVar peerJoined ()
            _ <- readMVar releaseJoiners
            pure ()
          else pure ()
        services = defaultApplicationServices
          { applicationCompileExact = compileExact,
            applicationAfterCompilationJoin = afterJoin
          }
        dependencies = Server.dependencies {serverApplicationServices = services}
        holdValidated = bracket (openReadWriteExisting archive) SQLite.close $ \connection ->
          withTransaction connection $
            Cache.withValidatedExactCacheTargetConnection target connection $ \_ -> do
              execute_ connection "PRAGMA query_only=ON"
              putMVar holderEntered ()
              takeMVar releaseHolder
        request running path = rawJsonRequest running "GET" path Nothing ByteString.empty
    started <- withWebServer dependencies root (Api.WebOptions Nothing False) $ \running _ ->
      withAsync (request running doctorPath) $ \first ->
        (do
          joined <- timeout 5000000 (takeMVar firstJoined)
          assertBool "first HTTP doctor joined the real exact compilation flight" (joined == Just ())
          entered <- timeout 5000000 (takeMVar producerEntered)
          assertBool "the real exact producer reached its pause" (entered == Just ())
          mixedPhase "first-joined"
          admissionDeadline <- (+ 9000000000) <$> getMonotonicTimeNSec
          withAdmittedPeer (request running searchPath) peerJoined admissionDeadline "search" $ \search ->
            withAdmittedPeer (request running relevantPath) peerJoined admissionDeadline "relevance" $ \relevant ->
              withAdmittedPeer (request running doctorPath) peerJoined admissionDeadline "doctor" $ \peerDoctor -> do
                joinedPeers <- readIORef joinEntries
                assertBool "search, relevance, and doctor joined the first exact flight" (joinedPeers == 4)
                beforeProducerRelease <- mapM poll [search, relevant, peerDoctor]
                assertBool "all three admitted consumers remain in the first flight before producer release"
                  (all isPending beforeProducerRelease)
                mixedPhase "three-joined"
                _ <- tryPutMVar releaseProducer ()
                firstResult <- awaitHttp first "first real doctor"
                assertExactSuccess target firstResult
                mixedPhase "first-doctor-200"
                searchPending <- poll search
                relevantPending <- poll relevant
                doctorPending <- poll peerDoctor
                assertBool "all three consumers remain held after the producer completed"
                  (all isPending [searchPending, relevantPending, doctorPending])
                beforeBytes <- ByteString.readFile archive
                withAsync holdValidated $ \holder ->
                  (do
                    held <- timeout 5000000 (takeMVar holderEntered)
                    assertBool "the peer holds a fully validated exact archive" (held == Just ())
                    mixedPhase "holder-entered"
                    _ <- tryPutMVar releaseJoiners ()
                    busyPeers <- timeout 8000000 $ mapConcurrently
                      (\(worker, label) -> awaitHttp worker label)
                      [(search, "real search consumer"), (relevant, "real relevance consumer"), (peerDoctor, "real doctor consumer")]
                    mapM_ assertTypedBusy =<< maybe (assertFailure "mixed busy consumers exceeded the shared phase bound") pure busyPeers
                    mixedPhase "three-consumers-503"
                    producerBusy <- request running doctorPath
                    assertTypedBusy producerBusy
                    mixedPhase "new-producer-503"
                    entries <- readIORef producerEntries
                    assertBool "a separate real producer attempted the held archive" (entries == 2)
                    coldWhileHeld <- readIORef coldBranches
                    assertBool "busy never entered the physical cold compile branch" (coldWhileHeld == 0)
                    _ <- tryPutMVar releaseHolder ()
                    heldResult <- waitCatch holder
                    case heldResult of
                      Left exception -> rethrowAsync exception >> assertFailure "held exact consumer failed"
                      Right accepted -> assertBool "held exact consumer completed" (accepted == Just ())
                    recoveryDeadline <- (+ 9000000000) <$> getMonotonicTimeNSec
                    recoverExact running target recoveryDeadline "search" searchPath
                    recoverExact running target recoveryDeadline "relevance" relevantPath
                    recoverExact running target recoveryDeadline "doctor" doctorPath
                    mixedPhase "three-consumers-recovered"
                    afterBytes <- ByteString.readFile archive
                    assertBool "the same exact archive bytes survive busy and recovery" (beforeBytes == afterBytes)
                    coldAfterRecovery <- readIORef coldBranches
                    assertBool "recovery did not enter the cold compile branch" (coldAfterRecovery == 0)
                  ) `finally` do
                    _ <- tryPutMVar releaseHolder ()
                    pure ()
        ) `finally` do
          _ <- tryPutMVar releaseProducer ()
          _ <- tryPutMVar releaseJoiners ()
          _ <- tryPutMVar releaseHolder ()
          pure ()
    either (assertFailure . Text.unpack) pure started
  case bounded of
    Nothing -> assertFailure "mixed exact HTTP consumers exceeded 40 seconds"
    Just () -> pure ()
  where
    isPending Nothing = True
    isPending _ = False

mixedPhase :: String -> IO ()
mixedPhase label = putStrLn ("mixed-exact-phase: " <> label) >> hFlush stdout

withAdmittedPeer :: IO (Int, Aeson.Value) -> MVar () -> Word64 -> String -> (Async (Int, Aeson.Value) -> IO a) -> IO a
withAdmittedPeer request joined deadline label useWorker = go (0 :: Int)
  where
    go retries
      | retries >= 4 = assertFailure (label <> " did not join the first exact flight after typed busy admission")
      | otherwise = withAsync request $ \worker -> do
          now <- getMonotonicTimeNSec
          let remaining = if now >= deadline then 0 else fromIntegral ((deadline - now) `div` 1000)
          if remaining <= 0
            then assertFailure (label <> " admission exceeded the shared nine-second bound")
            else do
              outcome <- timeout (min 3000000 remaining) (race (takeMVar joined) (waitCatch worker))
              case outcome of
                Nothing -> assertFailure (label <> " admission did not reach a join or typed busy response")
                Just (Left ()) -> useWorker worker
                Just (Right (Left exception)) -> rethrowAsync exception >> assertFailure (label <> " admission socket failed")
                Just (Right (Right response)) -> assertTypedBusy response >> go (retries + 1)

recoverExact :: RunningServer -> Text -> Word64 -> String -> Text -> IO ()
recoverExact running target deadline label path = go (0 :: Int)
  where
    go attempts
      | attempts >= 6 = assertFailure (label <> " recovery exhausted typed busy attempts")
      | otherwise = do
          now <- getMonotonicTimeNSec
          let remaining = if now >= deadline then 0 else fromIntegral ((deadline - now) `div` 1000)
          if remaining <= 0
            then assertFailure (label <> " recovery exceeded the shared nine-second bound")
            else do
              response <- timeout remaining (rawJsonRequest running "GET" path Nothing ByteString.empty)
              case response of
                Nothing -> assertFailure (label <> " recovery HTTP request exceeded the shared bound")
                Just result@(status, value)
                  | status == 200 -> do
                      assertExactSuccess target result
                      putStrLn ("mixed-exact-recovery: " <> label <> " status=200 as_of=exact") >> hFlush stdout
                  | status == 503 && safeErrorCode value == "repository-busy" -> do
                      putStrLn ("mixed-exact-recovery: " <> label <> " status=503 code=repository-busy as_of=" <> safeAsOfKind value) >> hFlush stdout
                      threadDelay 50000
                      go (attempts + 1)
                  | otherwise -> assertFailure
                      (label <> " recovery returned status=" <> show status <> " code=" <> Text.unpack (safeErrorCode value)
                        <> " as_of=" <> safeAsOfKind value)

safeErrorCode :: Aeson.Value -> Text
safeErrorCode (Aeson.Object outer) = case KeyMap.lookup "error" outer of
  Just (Aeson.Object problem) -> case KeyMap.lookup "code" problem of
    Just (Aeson.String "repository-busy") -> "repository-busy"
    Just (Aeson.String "repository-lock-unavailable") -> "repository-lock-unavailable"
    Just (Aeson.String "service-failure") -> "service-failure"
    _ -> "other"
  _ -> "other"
safeErrorCode _ = "other"

safeAsOfKind :: Aeson.Value -> String
safeAsOfKind (Aeson.Object outer) = case KeyMap.lookup "metadata" outer of
  Just (Aeson.Object metadata) -> case KeyMap.lookup "as_of" metadata of
    Just (Aeson.Object asOf) -> case KeyMap.lookup "kind" asOf of
      Just (Aeson.String "commit") -> "commit"
      Just (Aeson.String "unavailable") -> "unavailable"
      Just (Aeson.String "comparison") -> "comparison"
      _ -> "other"
    _ -> "other"
  _ -> "other"
safeAsOfKind _ = "other"

awaitHttp :: Async (Int, Aeson.Value) -> String -> IO (Int, Aeson.Value)
awaitHttp worker label = do
  result <- timeout 8000000 (waitCatch worker)
  case result of
    Nothing -> assertFailure (label <> " exceeded its bounded HTTP wait")
    Just (Left exception) -> rethrowAsync exception >> assertFailure (label <> " failed")
    Just (Right response) -> pure response

assertExactSuccess :: Text -> (Int, Aeson.Value) -> IO ()
assertExactSuccess target (status, response) = do
  assertBool "a real exact HTTP consumer returned 200" (status == 200)
  asOf <- Server.textAt ["metadata", "as_of", "oid"] response
  assertBool "the real HTTP response retained the exact requested OID" (asOf == target)

assertTypedBusy :: (Int, Aeson.Value) -> IO ()
assertTypedBusy (status, response) = do
  assertBool "physical archive contention maps to HTTP 503" (status == 503)
  code <- Server.textAt ["error", "code"] response
  assertBool "physical archive contention retains the repository-busy code" (code == "repository-busy")

testCommittedBusyIndexWarning :: IO ()
testCommittedBusyIndexWarning = do
  bounded <- timeout 45000000 $ Server.withSeededRepository $ \root -> do
    let services = defaultApplicationServices
          { applicationCompileExact = \_ _ -> throwIO Cache.ExactArchiveBusy }
        dependencies = Server.dependencies {serverApplicationServices = services}
    started <- withWebServer dependencies root (Api.WebOptions Nothing False) $ \running _ -> do
      repositoryResult <- Server.getJson running "/api/v1/repository"
      basis <- Server.valueAt ["data", "repository_state"] repositoryResult
      before <- Server.gitHead root
      let body = Aeson.object
            [ "repository_state" Aeson..= basis,
              "title" Aeson..= ("Busy index receipt" :: Text),
              "summary" Aeson..= ("Committed before index acquisition" :: Text),
              "body" Aeson..= ("The decision remains durable.\n" :: Text),
              "domains" Aeson..= (["core"] :: [Text]),
              "scopes" Aeson..= (["src/**"] :: [Text]),
              "actor" Aeson..= Aeson.object
                ["kind" Aeson..= ("human" :: Text), "id" Aeson..= ("archive-busy-test" :: Text)]
            ]
      cookie <- bootstrapCookie running
      (mutationStatus, mutation) <- rawJsonRequest running "POST" "/api/v1/adrs" (Just cookie) (LazyByteString.toStrict (Aeson.encode body))
      assertBool "injected postcommit archive busy retains HTTP 200" (mutationStatus == 200)
      committed <- Server.valueAt ["data", "committed"] mutation
      assertBool "the response reports a durable committed operation" (committed == Aeson.Bool True)
      after <- Server.gitHead root
      assertBool "the real mutation advanced HEAD" (after /= before)
      commit <- Server.textAt ["data", "commit"] mutation
      assertBool "the response carries the actual committed OID" (commit == after)
      indexed <- Server.valueAt ["data", "indexed"] mutation
      assertBool "busy indexing remains explicitly incomplete" (indexed == Aeson.Bool False)
      indexError <- Server.textAt ["data", "index_error"] mutation
      assertBool "index warning identifies temporary archive busy" ("temporarily busy" `Text.isInfixOf` indexError)
      dataValue <- Server.valueAt ["data"] mutation
      case dataValue of
        Aeson.Object fields -> assertBool "successful publication has no publication warning" (not (KeyMap.member "publication_warning" fields))
        _ -> assertFailure "mutation data is not an object"
      (readStatus, busy) <- rawJsonRequest running "GET"
        ("/api/v1/search?q=&mode=hybrid&view=collapsed&limit=100&include_obsolete=false&shallow=false&at=" <> after)
        Nothing ByteString.empty
      assertBool "injected pre-read archive busy is HTTP 503" (readStatus == 503)
      code <- Server.textAt ["error", "code"] busy
      assertBool "the read error has the typed repository-busy code" (code == "repository-busy")
    either (assertFailure . Text.unpack) pure started
  case bounded of
    Nothing -> assertFailure "durable busy response test exceeded 45 seconds"
    Just () -> pure ()

testExactArchiveBusyRecovery :: IO ()
testExactArchiveBusyRecovery = do
  bounded <- timeout 55000000 $ Server.withSeededRepository $ \root -> do
    seedDecisions root
    repository <- discoverRepository systemGit root >>= either (const (assertFailure "busy fixture discovery failed")) pure
    oid <- resolveRevision repository (RevisionSpec "HEAD") >>= either (const (assertFailure "busy fixture HEAD failed")) pure
    archive <- Runtime.ensureExactArchive repository oid >>= either (const (assertFailure "busy fixture archive failed")) pure
    entered <- newEmptyMVar
    release <- newEmptyMVar
    secondStarted <- newEmptyMVar
    let target = gitOidText oid
        holdValidated = bracket (openReadWriteExisting archive) SQLite.close $ \connection ->
          withTransaction connection $
            Cache.withValidatedExactCacheTargetConnection target connection $ \_ -> do
              execute_ connection "PRAGMA query_only=ON"
              putMVar entered ()
              takeMVar release
    withAsync holdValidated $ \first ->
      (do
        reached <- timeout 8000000 (takeMVar entered)
        assertBool "held archive passed full validation" (reached == Just ())
        withAsync (putMVar secondStarted () >> Cache.validateExactCacheTarget archive target) $ \second -> do
          started <- timeout 2000000 (takeMVar secondStarted)
          assertBool "contending production validator started" (started == Just ())
          threadDelay 1500000
          pending <- poll second
          assertBool "bounded busy acquisition waits for the held validator" (case pending of Nothing -> True; _ -> False)
          finished <- timeout 2500000 (waitCatch second)
          case finished of
            Nothing -> assertFailure "busy acquisition exceeded its 2-second bound"
            Just (Right _) -> assertFailure "busy acquisition was mistaken for an archive decision"
            Just (Left exception) -> do
              rethrowAsync exception
              case fromException exception of
                Just Cache.ExactArchiveBusy -> pure ()
                Nothing -> assertFailure "busy acquisition did not retain its typed result"
        _ <- tryPutMVar release ()
        firstResult <- waitCatch first
        case firstResult of
          Left exception -> rethrowAsync exception >> assertFailure "held validator failed"
          Right accepted -> assertBool "held validator succeeded" (accepted == Just ())
        recovered <- Cache.validateExactCacheTarget archive target
        assertBool "the same exact OID validates after the peer releases" recovered
      ) `finally` do
        _ <- tryPutMVar release ()
        pure ()
  case bounded of
    Nothing -> assertFailure "exact archive busy/recovery exceeded 55 seconds"
    Just () -> pure ()

testConcurrentExactFtsValidation :: IO ()
testConcurrentExactFtsValidation = do
  bounded <- timeout 55000000 $ Server.withSeededRepository $ \root -> do
    seedDecisions root
    repository <- discoverRepository systemGit root >>= either (assertFailure . show) pure
    oid <- resolveRevision repository (RevisionSpec "HEAD") >>= either (assertFailure . show) pure
    archive <- Runtime.ensureExactArchive repository oid >>= either (const (assertFailure "exact archive seed failed")) pure
    entered <- newEmptyMVar
    release <- newEmptyMVar
    secondStarted <- newEmptyMVar
    let target = gitOidText oid
        firstValidation = bracket (openReadWriteExisting archive) SQLite.close $ \connection ->
          withTransaction connection $
            Cache.withValidatedExactCacheTargetConnection target connection $ \_ -> do
              execute_ connection "PRAGMA query_only=ON"
              putMVar entered ()
              takeMVar release
    withAsync firstValidation $ \first ->
      (do
        reached <- timeout 8000000 (takeMVar entered)
        assertBool "first exact validator reached its accepted consumer after all FTS checks" (reached == Just ())
        integrity <- bracket (openReadWriteExisting archive) SQLite.close $ \connection ->
          withTransaction connection (query_ connection "PRAGMA integrity_check" :: IO [Only Text])
        let observed = Set.fromList [row | Only row <- integrity]
            expected = Set.fromList
              [ "unable to validate the inverted index for FTS5 table main."
                  <> ftsTargetTable targetTable <> ": database is locked"
              | targetTable <- allFtsTargets
              ]
        assertBool "a peer FTS validator causes the six exact locked integrity rows"
          (length integrity == 6 && observed == expected)
        putStrLn "exact-archive-regression: peer validator produced six FTS locked integrity rows"
        withAsync (putMVar secondStarted () >> Cache.validateExactCacheTarget archive target) $ \second -> do
          started <- timeout 2000000 (takeMVar secondStarted)
          assertBool "second production validator started while first is held" (started == Just ())
          threadDelay 100000
          beforeRelease <- poll second
          case beforeRelease of
            Just (Right False) -> putStrLn "exact-archive-regression: production validator rejected held archive"
            _ -> pure ()
          threadDelay 400000
          _ <- tryPutMVar release ()
          result <- timeout 8000000 (waitCatch second)
          case result of
            Nothing -> assertFailure "second production validation exceeded the bound"
            Just (Left exception) -> do
              rethrowAsync exception
              assertFailure "second production validation raised a synchronous exception"
            Just (Right accepted) -> do
              let earlySuccess = case beforeRelease of
                    Just (Right True) -> True
                    _ -> False
              assertBool "second production validator cannot pass before the held peer releases"
                (not earlySuccess)
              assertBool "second production validator succeeds after peer releases" accepted
        _ <- tryPutMVar release ()
        firstResult <- waitCatch first
        case firstResult of
          Left exception -> do
            rethrowAsync exception
            assertFailure "first exact validation raised a synchronous exception"
          Right accepted -> assertBool "first exact validation succeeds" (accepted == Just ())
      ) `finally` do
        _ <- tryPutMVar release ()
        pure ()
  case bounded of
    Nothing -> assertFailure "concurrent exact FTS validation exceeded 55 seconds"
    Just () -> pure ()

rethrowAsync :: SomeException -> IO ()
rethrowAsync exception =
  case fromException exception of
    Just cancellation -> throwIO (cancellation :: SomeAsyncException)
    Nothing -> pure ()

bootstrapCookie :: RunningServer -> IO Text
bootstrapCookie running = do
  let host = Security.authorityHost (runningAuthority running)
      tokenQuery = Text.drop 1 (snd (Text.breakOn "?" (runningBootstrapUrl running)))
      requestBytes = TextEncoding.encodeUtf8
        ("GET /?" <> tokenQuery <> " HTTP/1.1\r\nHost: " <> host <> "\r\nConnection: close\r\n\r\n")
  response <- rawExchange running requestBytes
  let (headers, _) = ByteString.breakSubstring "\r\n\r\n" response
      cookies =
        [ ByteString8.takeWhile (/= ';') (ByteString8.drop (ByteString8.length "Set-Cookie: ") line)
        | line <- ByteString8.lines headers,
          "Set-Cookie: " `ByteString8.isPrefixOf` line
        ]
  case cookies of
    [cookie] -> pure (Text.pack (ByteString8.unpack cookie))
    _ -> assertFailure "bootstrap did not return one session cookie"

rawJsonRequest :: RunningServer -> Text -> Text -> Maybe Text -> ByteString.ByteString -> IO (Int, Aeson.Value)
rawJsonRequest running method path maybeCookie body = do
  let host = Security.authorityHost (runningAuthority running)
      token = Text.drop (Text.length "token=") (snd (Text.breakOn "token=" (runningBootstrapUrl running)))
      optionalCookie = maybe "" ("\r\nCookie: " <>) maybeCookie
      requestHead = TextEncoding.encodeUtf8
        (method <> " " <> path <> " HTTP/1.1\r\nHost: " <> host
          <> "\r\nOrigin: " <> Security.authorityOrigin (runningAuthority running)
          <> "\r\nAuthorization: Bearer " <> token <> optionalCookie
          <> "\r\nContent-Type: application/json\r\nContent-Length: " <> Text.pack (show (ByteString.length body))
          <> "\r\nConnection: close\r\n\r\n")
  response <- rawExchange running (requestHead <> body)
  assertBool "HTTP response does not disclose the process credential"
    (not (TextEncoding.encodeUtf8 token `ByteString.isInfixOf` response))
  status <- case ByteString8.words (ByteString8.takeWhile (/= '\r') response) of
    _ : rawStatus : _ -> maybe (assertFailure "invalid raw HTTP status") pure (readMaybe (ByteString8.unpack rawStatus))
    _ -> assertFailure "missing raw HTTP status"
  let (headers, suffix) = ByteString.breakSubstring "\r\n\r\n" response
  assertBool "HTTP JSON response has a body" (not (ByteString.null suffix))
  let encoded = ByteString.drop 4 suffix
      decoded = if "Transfer-Encoding: chunked" `ByteString.isInfixOf` headers
        then decodeChunks encoded else Just encoded
  bytes <- maybe (assertFailure "malformed bounded HTTP body") pure decoded
  value <- maybe (assertFailure "invalid bounded HTTP JSON") pure (Aeson.decodeStrict' bytes)
  pure (status, value)

rawExchange :: RunningServer -> ByteString.ByteString -> IO ByteString.ByteString
rawExchange running requestBytes = do
  let host = Security.authorityHost (runningAuthority running)
      rawPort = snd (Text.breakOnEnd ":" host)
  port <- maybe (assertFailure "invalid reported loopback port") pure (readMaybe (Text.unpack rawPort) :: Maybe Int)
  bracket (socket AF_INET Stream defaultProtocol) closeOwnedSocket $ \client -> do
    let cleanup worker = do
          closeOwnedSocket client
          joined <- timeout 2000000 (waitCatch worker)
          assertBool "owned raw HTTP socket worker joined after close" (maybe False (const True) joined)
    bracket (async $ do
      connect client (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))
      sendAll client requestBytes
      receiveAll client [] 0) cleanup $ \worker -> do
        bounded <- timeout 8000000 (waitCatch worker)
        case bounded of
          Nothing -> assertFailure "bounded raw HTTP request timed out"
          Just (Left exception) -> rethrowAsync exception >> assertFailure "bounded raw HTTP socket failed"
          Just (Right response) -> pure response
  where
    receiveAll client chunks size = do
      bytes <- recv client 4096
      let total = size + ByteString.length bytes
      if total > 2 * 1024 * 1024
        then assertFailure "raw HTTP response exceeded its bound"
        else if ByteString.null bytes
          then pure (ByteString.concat (reverse chunks))
          else receiveAll client (bytes : chunks) total

closeOwnedSocket :: Socket -> IO ()
closeOwnedSocket client = do
  shut <- try (shutdown client ShutdownBoth) :: IO (Either SomeException ())
  closed <- try (close client) :: IO (Either SomeException ())
  either rethrowAsync pure shut
  either rethrowAsync pure closed

decodeChunks :: ByteString.ByteString -> Maybe ByteString.ByteString
decodeChunks input =
  let (rawSize, suffix) = ByteString.breakSubstring "\r\n" input
      rest = ByteString.drop 2 suffix
   in case readHex (ByteString8.unpack rawSize) of
        [(0, "")] -> Just ByteString.empty
        [(size, "")]
          | ByteString.length rest >= size + 2
              && ByteString.take 2 (ByteString.drop size rest) == "\r\n" ->
              (ByteString.take size rest <>) <$> decodeChunks (ByteString.drop (size + 2) rest)
        _ -> Nothing

testColdExactArchive :: IO ()
testColdExactArchive = do
  bounded <- timeout 55000000 $ Server.withSeededRepository $ \root -> do
    seedDecisions root
    repository <- discoverRepository systemGit root >>= either (assertFailure . show) pure
    seedOid <- resolveRevision repository (RevisionSpec "HEAD") >>= either (assertFailure . show) pure
    let exactArchive = root </> ".adrai" </> "cache" </> Text.unpack (gitOidText seedOid) <> ".sqlite"
    absent <- not <$> doesFileExist exactArchive
    assertBool "exact archive is absent before the first HTTP search" absent
    started <- withWebServer Server.dependencies root (Api.WebOptions Nothing False) $ \running _ -> do
      absentAtFirstGet <- not <$> doesFileExist exactArchive
      assertBool "exact archive is absent immediately before the first HTTP search" absentAtFirstGet
      let exactSearch = "/api/v1/search?q=Seeded&mode=fts&view=collapsed&limit=100&include_obsolete=false&shallow=false&at=" <> gitOidText seedOid
      outcomes <- timeout 38000000 (mapConcurrently (const (rawSearchStatus running exactSearch)) [1 :: Int .. 12])
      statuses <- maybe (assertFailure "cold exact archive requests exceeded the bound") pure outcomes
      assertBool "bounded cold searches return only success or typed busy" (all (`elem` [200, 503]) statuses)
      assertBool "at least one real exact search succeeds" (200 `elem` statuses)
      created <- doesFileExist exactArchive
      assertBool "the first real HTTP search created the exact archive" created
      (settledStatus, settled) <- rawJsonRequest running "GET" exactSearch Nothing ByteString.empty
      assertExactSuccess (gitOidText seedOid) (settledStatus, settled)
    either (assertFailure . Text.unpack) pure started
  case bounded of
    Nothing -> assertFailure "entire cold exact archive diagnostic exceeded 55 seconds"
    Just () -> pure ()

seedDecisions :: FilePath -> IO ()
seedDecisions root = do
  executable <- lookupEnv "ADRAI_EXE" >>= maybe (assertFailure "ADRAI_EXE is required") pure
  forM_ [1 :: Int, 2] $ \number -> do
    let arguments =
          [ "create", "--title", "Seeded archive decision " <> show number,
            "--summary", "Exact archive diagnostic seed",
            "--body", "Read this sealed decision.\n",
            "--actor", "human:archive-test",
            "--domain", "runtime.web",
            "--applies-to", "seed.txt",
            "--json"
          ]
        config = (proc executable arguments) {cwd = Just root, std_out = NoStream, std_err = NoStream}
    bracket (createProcess config) (stopChild . fourth) $ \(_, _, _, handle) -> do
      outcome <- timeout 4000000 (waitForProcess handle)
      unless (outcome == Just ExitSuccess) (assertFailure "bounded CLI seed decision failed")

stopChild :: ProcessHandle -> IO ()
stopChild handle = do
  status <- getProcessExitCode handle
  case status of
    Just _ -> pure ()
    Nothing -> do
      terminateProcess handle
      joined <- timeout 2000000 (waitForProcess handle)
      assertBool "CLI seed child joined after termination" (maybe False (const True) joined)

fourth :: (a, b, c, d) -> d
fourth (_, _, _, value) = value

rawSearchStatus :: RunningServer -> Text -> IO Int
rawSearchStatus running path = do
  let host = Security.authorityHost (runningAuthority running)
      token = Text.drop (Text.length "token=") (snd (Text.breakOn "token=" (runningBootstrapUrl running)))
      rawPort = snd (Text.breakOnEnd ":" host)
      requestBytes = TextEncoding.encodeUtf8
        ("GET " <> path <> " HTTP/1.1\r\nHost: " <> host
          <> "\r\nAuthorization: Bearer " <> token <> "\r\nConnection: close\r\n\r\n")
  port <- maybe (assertFailure "invalid reported loopback port") pure (readMaybe (Text.unpack rawPort) :: Maybe Int)
  bounded <- timeout 8000000 $ bracket (socket AF_INET Stream defaultProtocol) close $ \client -> do
    connect client (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))
    sendAll client requestBytes
    receiveAll client []
  response <- maybe (assertFailure "bounded raw HTTP search timed out") pure bounded
  assertBool "raw HTTP response does not disclose the process credential"
    (not (TextEncoding.encodeUtf8 token `ByteString.isInfixOf` response))
  case ByteString8.words (ByteString8.takeWhile (/= '\r') response) of
    _ : rawStatus : _ -> do
      status <- maybe (assertFailure "invalid raw HTTP status") pure (readMaybe (ByteString8.unpack rawStatus))
      if status == 503 then checkBusyCode response else pure ()
      pure status
    _ -> assertFailure "missing raw HTTP status"
  where
    receiveAll client chunks = do
      bytes <- recv client 4096
      let size = sum (map ByteString.length chunks) + ByteString.length bytes
      if size > 2 * 1024 * 1024
        then assertFailure "raw HTTP diagnostic response exceeded its bound"
        else if ByteString.null bytes then pure (ByteString.concat (reverse chunks)) else receiveAll client (bytes : chunks)

    checkBusyCode response = do
      let (headers, suffix) = ByteString.breakSubstring "\r\n\r\n" response
      assertBool "busy response has an HTTP body" (not (ByteString.null suffix))
      let encoded = ByteString.drop 4 suffix
          decoded = if "Transfer-Encoding: chunked" `ByteString.isInfixOf` headers
            then unchunk encoded else Just encoded
      body <- maybe (assertFailure "malformed bounded busy response") pure decoded
      value <- maybe (assertFailure "invalid bounded busy JSON") pure (Aeson.decodeStrict' body)
      case value of
        Aeson.Object outer -> case KeyMap.lookup "error" outer of
          Just (Aeson.Object problem) -> case KeyMap.lookup "code" problem of
            Just (Aeson.String code) -> assertBool "503 is a typed repository busy response"
              (code `elem` ["repository-busy", "repository-lock-unavailable"])
            _ -> assertFailure "503 is missing a typed error code"
          _ -> assertFailure "503 is missing a typed error"
        _ -> assertFailure "503 is not a typed JSON response"

    unchunk input =
      let (rawSize, suffix) = ByteString.breakSubstring "\r\n" input
          rest = ByteString.drop 2 suffix
       in case readHex (ByteString8.unpack rawSize) of
            [(0, "")] -> Just ByteString.empty
            [(size, "")]
              | ByteString.length rest >= size + 2
                  && ByteString.take 2 (ByteString.drop size rest) == "\r\n" ->
                  (ByteString.take size rest <>) <$> unchunk (ByteString.drop (size + 2) rest)
            _ -> Nothing
