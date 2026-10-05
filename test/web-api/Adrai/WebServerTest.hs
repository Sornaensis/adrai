{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Adrai.WebServerTest
  ( tests,
    generationExhaustionTest,
    testEventRuntime,
    testWatchRuntime,
    testCompilationRuntime,
    dependencies,
    withSeededRepository,
    getJson,
    getJsonLabeled,
    getJsonLabeledObserved,
    getJsonStatus,
    valueAt,
    textAt,
    gitHead,
  ) where

import qualified Adrai.Web.Api as Api
import Adrai.Web.Application (ApplicationServices (..), applicationActiveFileRegistry, applicationEventCoordinator, defaultApplicationServices, newApplicationRuntime, publishWatcherEvent, stopApplicationRuntime)
import Adrai.CliRunner (parseArguments)
import Adrai.Compiler.CacheSelection (validateExactCacheTarget)
import Adrai.Format.Config (defaultConfigText)
import Adrai.Git (Repository (..), GitOid, RevisionSpec (RevisionSpec), discoverRepository, repositoryHeadState, resolveRevision, systemGit)
import Adrai.Domain (mkDomain)
import Adrai.Repository (repositorySnapshot, repositorySnapshotManagedPaths)
import Adrai.Scope (mkScopePattern)
import Adrai.Service.Transaction (ExpectedRepositoryBasis (..))
import Adrai.History (revisionRequested, revisionResolved)
import Adrai.Provenance (gitOidText)
import Adrai.Provenance.Git.Lock (GitLockError (LockHeld), gitLockStatus, withGitLock)
import qualified Adrai.Service.Compilation as Compilation
import qualified Adrai.Service.Mutation as Mutation
import qualified Adrai.Service.Query as Query
import qualified Adrai.Query as DomainQuery
import qualified Adrai.Service.Runtime as Runtime
import Adrai.Types (ViewMode (CollapsedView))
import qualified Adrai.Types as Types
import qualified Adrai.Web.Events as Events
import Adrai.Web.Server (RunningServer (..), ServerDependencies (..), defaultServerDependencies, withWebServer)
import Adrai.RetainedNative.NativeFixture (copyNativeFixture)
import qualified Adrai.Web.Security as Security
import Adrai.Web.Socket (unavailableEventsTransport)
import qualified Adrai.Web.Watch as Watch
import Control.Concurrent (MVar, newEmptyMVar, putMVar, readMVar, takeMVar, threadDelay, tryPutMVar, tryTakeMVar)
import Control.Concurrent.Async (Async, async, cancel, poll, race, wait, waitCatch, withAsync)
import Control.Exception (SomeAsyncException, SomeException, bracket, bracketOnError, finally, fromException, onException, throwIO, try)
import Control.Monad (forM, forM_, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Pair)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bits ((.&.), xor)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Text.Read (readMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket
  ( Family (AF_INET), PortNumber, ShutdownCmd (ShutdownBoth), SockAddr (SockAddrInet), Socket, SocketOption (RecvBuffer, ReuseAddr), SocketType (Stream),
    bind, close, connect, defaultProtocol, getSocketName, setSocketOption, shutdown, socket, tupleToHostAddress )
import Network.Socket.ByteString (recv, sendAll)
import qualified Network.WebSockets as WS
import System.Directory (Permissions (writable), copyFile, createDirectory, createDirectoryIfMissing, createDirectoryLink, doesDirectoryExist, doesFileExist, getCurrentDirectory, getPermissions, listDirectory, removeDirectoryLink, removeDirectoryRecursive, removeFile, renameDirectory, setPermissions)
import System.Info (os)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>), searchPathSeparator)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), StdStream (CreatePipe, Inherit), callProcess, createProcess, getProcessExitCode, proc, readCreateProcessWithExitCode, readProcess, shell, terminateProcess, waitForProcess)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hClose, hGetLine, hFlush, stdout)
import Numeric (readHex)
import Data.Word (Word8, Word64)
import Database.SQLite.Simple (Only (..))
import qualified Database.SQLite.Simple as SQLite
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests = testGroup "web server runtime"
  [ testCase "real loopback bootstrap exchanges the query credential for a secure cookie" testBootstrap,
    testCase "real loopback admission protects unknown routes" testUnknownAdmission,
    testCase "authenticated events report unavailable snapshot metadata" testEventsUnavailable,
    testCase "relevant v2 HTTP route serves the current projection" testRelevantV2Route,
    testCase "shared query routes retain exact response snapshots" testQueryRoutes,
    testCase "all six HTTP mutations commit through checked shared services" testMutationRoutes,
    testCase "oversized inputs recover and stale repository bases preserve HEAD" testBoundsAndStale,
    testCase "built web command and repository bindings start and stop cleanly" testExecutableAndBindings,
    testCase "an occupied explicit port fails without stealing the listener" testOccupiedPort,
    testCase "server shutdown releases the acquired port" testShutdownRelease
  ]

generationExhaustionTest :: TestTree
generationExhaustionTest = testCase "maximum generation refuses new reads but retains a durable committed mutation" testGenerationExhaustion

testGenerationExhaustion :: IO ()
testGenerationExhaustion = withSeededRepository $ \root -> do
  ready <- newEmptyMVar
  let services = defaultApplicationServices
        { applicationBeforeCommitPublication = \_ -> takeMVar ready >>= \coordinator -> Events.setEventGenerationForTest coordinator maxBound }
      injected = dependencies
        { serverEventCoordinatorReady = putMVar ready,
          serverApplicationServices = services
        }
  started <- withWebServer injected root (Api.WebOptions Nothing False) $ \running _ -> do
    basis <- repositoryBasis running
    before <- gitHead root
    let authority = runningAuthority running
        token = bootstrapToken running
        headers =
          [ ("Origin", TextEncoding.encodeUtf8 (Security.authorityOrigin authority)),
            ("Authorization", TextEncoding.encodeUtf8 ("Bearer " <> token))
          ]
        authenticate = Aeson.encode (Aeson.object ["type" Aeson..= ("authenticate" :: Text), "credential" Aeson..= token])
    port <- either assertFailure pure (authorityPort (Security.authorityHost authority))
    socketReady <- newEmptyMVar
    socketPhase <- newIORef ("connecting" :: String)
    let connected = runOwnedWebSocketClient 60000000 port headers $ \connection -> do
          writeIORef socketPhase "authenticating"
          WS.sendTextData connection authenticate
          initial <- timeout 15000000 (WS.receiveData connection :: IO LBS.ByteString)
          assertBool "existing authenticated subscriber received initial resync" (maybe False (BS.isInfixOf "repository-invalidated" . LBS.toStrict) initial)
          writeIORef socketPhase "awaiting terminal close"
          putMVar socketReady ()
          closed <- timeout 10000000 (trySynchronous (WS.receiveDataMessage connection))
          writeIORef socketPhase ("terminal close observed: " <> show (fmap (either show (const "data message")) closed))
          assertBool "terminal exhaustion closes existing subscriber with restart reason" (expectedClose "generation-exhausted; restart the web server" closed)
          writeIORef socketPhase "terminal close validated; cleaning up client"
    bracket (async connected) (\worker -> cancel worker >> void (waitCatch worker)) $ \socketWorker -> do
      readySocket <- timeout 20000000 (takeMVar socketReady)
      readyPhase <- readIORef socketPhase
      readyWorker <- poll socketWorker
      assertBool ("existing socket authenticated before mutation publication; phase=" <> readyPhase <> "; worker=" <> show readyWorker)
        (maybe False (const True) readySocket)
      committed <- postJson running "/api/v1/adrs" (createBody basis)
      assertCommitted committed
      after <- gitHead root
      assertBool "the actual commit survived exhausted publication" (after /= before)
      textAt ["data", "commit"] committed >>= (@?= after)
      textAt ["data", "publication_warning"] committed >>= (@?= "commit generation publication failed after the durable commit")
      textAt ["metadata", "as_of", "kind"] committed >>= (@?= "unavailable")
      socketEnded <- timeout 15000000 (waitCatch socketWorker)
      phase <- readIORef socketPhase
      assertBool ("existing socket and owned worker finish after terminal close; phase=" <> phase <> "; result=" <> show socketEnded)
        (maybe False (either (const False) (maybe False (either (const False) (const True)))) socketEnded)
      (status, exhausted) <- getJsonStatus running "/api/v1/repository"
      status @?= 503
      textAt ["metadata", "generation"] exhausted >>= (@?= "18446744073709551615")
      textAt ["metadata", "as_of", "reason"] exhausted >>= (@?= "generation-exhausted")
      textAt ["error", "code"] exhausted >>= (@?= "generation-exhausted")
      repository <- discoverRepository systemGit root >>= either (assertFailure . show) pure
      lockAcquired <- newEmptyMVar
      releaseLock <- newEmptyMVar
      let holdUntilAcquired remaining = do
            if remaining <= (0 :: Int) then ioError (userError "terminal WebSocket test could not acquire Git lock") else pure ()
            attempt <- try @SomeException (withGitLock repository (putMVar lockAcquired () >> takeMVar releaseLock))
            case attempt of
              Left failure -> case fromException failure of
                Just cancellation -> throwIO (cancellation :: SomeAsyncException)
                Nothing -> threadDelay 20000 >> holdUntilAcquired (remaining - 1)
              Right () -> pure ()
      lockOwner <- async (holdUntilAcquired 100)
      (`finally` do _ <- tryPutMVar releaseLock (); cancel lockOwner; void (waitCatch lockOwner)) $ do
        held <- timeout 3000000 (race (takeMVar lockAcquired) (waitCatch lockOwner))
        case held of
          Just (Left ()) -> pure ()
          other -> assertFailure ("terminal WebSocket test never held the Git lock: " <> show other)
        late <- runOwnedWebSocketClient 5000000 port headers $ \connection -> do
          WS.sendTextData connection authenticate
          closed <- timeout 2000000 (trySynchronous (WS.receiveDataMessage connection))
          assertBool "new authenticated socket closes with terminal restart reason while Git lock remains held" (expectedClose "generation-exhausted; restart the web server" closed)
        assertBool "post-terminal socket owns a bounded close and cleanup before Git unlock" (maybe False (either (const False) (const True)) late)
  either (assertFailure . Text.unpack) pure started

dependencies :: ServerDependencies
dependencies = ServerDependencies
  { serverEntropy = pure (BS.pack [0 .. 31]),
    serverOpenBrowser = const (pure (Right ())),
    serverReady = const (pure ()),
    serverStopping = pure (),
    serverEventCoordinatorReady = const (pure ()),
    serverEventSendDeadline = pure (),
    serverApplicationServices = defaultApplicationServices,
    serverEventsTransport = unavailableEventsTransport
  }

withServer :: (RunningServer -> Async () -> IO value) -> IO value
withServer action = do
  root <- getCurrentDirectory
  started <- withWebServer dependencies root (Api.WebOptions Nothing False) action
  either (assertFailure . Text.unpack) pure started

testBootstrap :: IO ()
testBootstrap = withServer $ \running _ -> do
  let token = Text.drop 1 (snd (Text.breakOn "?" (runningBootstrapUrl running)))
      host = Security.authorityHost (runningAuthority running)
  response <- request host ("GET /?" <> token <> " HTTP/1.1\r\nHost: " <> host <> "\r\nConnection: close\r\n\r\n")
  assertBool "bootstrap succeeds" ("HTTP/1.1 200" `BS.isPrefixOf` response)
  assertBool "cookie is HttpOnly and Strict" ("HttpOnly; SameSite=Strict" `BS.isInfixOf` response)
  assertBool "bootstrap carries snapshot metadata" ("X-Adrai-Generation:" `BS.isInfixOf` response)
  assertBool "bootstrap does not declare resources before token removal" (not ("<script src=" `BS.isInfixOf` response) && not ("<link rel=" `BS.isInfixOf` response))
  cookie <- sessionCookiePair running
  reloaded <- request host ("GET / HTTP/1.1\r\nHost: " <> host <> "\r\nCookie: " <> cookie <> "\r\nConnection: close\r\n\r\n")
  assertBool "cleaned root reload accepts the valid session cookie and serves the explorer" ("HTTP/1.1 200" `BS.isPrefixOf` reloaded && "<title>ADRAI repository explorer</title>" `BS.isInfixOf` reloaded)
  assertBool "cookie reload does not set another session cookie" (not ("Set-Cookie:" `BS.isInfixOf` reloaded))
  deniedReload <- request host ("GET / HTTP/1.1\r\nHost: " <> host <> "\r\nConnection: close\r\n\r\n")
  assertBool "cleaned root without a session remains denied" ("HTTP/1.1 401" `BS.isPrefixOf` deniedReload)
  deniedQuery <- request host ("GET /?unexpected=1 HTTP/1.1\r\nHost: " <> host <> "\r\nCookie: " <> cookie <> "\r\nConnection: close\r\n\r\n")
  assertBool "an unexpected root query cannot bypass bootstrap admission" ("HTTP/1.1 401" `BS.isPrefixOf` deniedQuery)
  css <- bearerRequest running "GET /app.css" []
  js <- bearerRequest running "GET /app.js" []
  assertBool "embedded assets require and accept authentication" ("HTTP/1.1 200" `BS.isPrefixOf` css && "HTTP/1.1 200" `BS.isPrefixOf` js)
  deniedAsset <- request host ("GET /app.css HTTP/1.1\r\nHost: " <> host <> "\r\nConnection: close\r\n\r\n")
  assertBool "asset without process credential is denied" ("HTTP/1.1 401" `BS.isPrefixOf` deniedAsset)

testUnknownAdmission :: IO ()
testUnknownAdmission = withServer $ \running _ -> do
  let host = Security.authorityHost (runningAuthority running)
  denied <- request host ("GET /missing HTTP/1.1\r\nHost: " <> host <> "\r\nConnection: close\r\n\r\n")
  assertBool "missing credential wins" ("HTTP/1.1 401" `BS.isPrefixOf` denied)
  allowed <- bearerRequest running "GET /missing" []
  assertBool "authenticated route miss is visible" ("HTTP/1.1 404" `BS.isPrefixOf` allowed)
  let token = bootstrapToken running
  invalidHost <- request host ("GET /missing HTTP/1.1\r\nHost: 127.0.0.1:1\r\nAuthorization: Bearer " <> token <> "\r\nConnection: close\r\n\r\n")
  assertBool "invalid Host maps to the documented origin-policy status" ("HTTP/1.1 403" `BS.isPrefixOf` invalidHost)
  duplicateHost <- request host ("GET /missing HTTP/1.1\r\nHost: " <> host <> "\r\nHost: " <> host <> "\r\nAuthorization: Bearer " <> token <> "\r\nConnection: close\r\n\r\n")
  assertBool "duplicate Host is rejected by transport or admission" (not ("HTTP/1.1 2" `BS.isPrefixOf` duplicateHost))
  duplicateAuthorization <- request host ("GET /missing HTTP/1.1\r\nHost: " <> host <> "\r\nAuthorization: Bearer " <> token <> "\r\nAuthorization: Bearer " <> token <> "\r\nConnection: close\r\n\r\n")
  assertBool "duplicate Authorization is rejected" ("HTTP/1.1 401" `BS.isPrefixOf` duplicateAuthorization)
  foreignOrigin <- request host ("POST /api/v1/adrs HTTP/1.1\r\nHost: " <> host <> "\r\nOrigin: http://example.invalid\r\nAuthorization: Bearer " <> token <> "\r\nContent-Type: application/json\r\nContent-Length: 2\r\nConnection: close\r\n\r\n{}")
  nullOrigin <- request host ("POST /api/v1/adrs HTTP/1.1\r\nHost: " <> host <> "\r\nOrigin: null\r\nAuthorization: Bearer " <> token <> "\r\nContent-Type: application/json\r\nContent-Length: 2\r\nConnection: close\r\n\r\n{}")
  missingOrigin <- request host ("POST /api/v1/adrs HTTP/1.1\r\nHost: " <> host <> "\r\nAuthorization: Bearer " <> token <> "\r\nContent-Type: application/json\r\nContent-Length: 2\r\nConnection: close\r\n\r\n{}")
  assertBool "POST origin policy rejects foreign, null, and missing origins" (all ("HTTP/1.1 403" `BS.isPrefixOf`) [foreignOrigin, nullOrigin, missingOrigin])
  let checkError label expectedStatus expectedCategory expectedCode response = do
        responseStatus response >>= (@?= expectedStatus)
        assertBool (label <> " does not disclose the process credential") (not (TextEncoding.encodeUtf8 token `BS.isInfixOf` response))
        value <- decodeBody response
        textAt ["schema"] value >>= (@?= "adrai/api/v1")
        _ <- integerAt ["metadata", "generation"] value
        textAt ["metadata", "as_of", "kind"] value >>= (@?= "unavailable")
        valueAt ["error", "status"] value >>= (@?= Aeson.Number (fromIntegral expectedStatus))
        textAt ["error", "category"] value >>= (@?= expectedCategory)
        textAt ["error", "code"] value >>= (@?= expectedCode)
      sendJson path body = do
        let bytes = LBS.toStrict (Aeson.encode body)
            headBytes = TextEncoding.encodeUtf8
              ("POST " <> path <> " HTTP/1.1\r\nHost: " <> host <> "\r\nOrigin: " <> Security.authorityOrigin (runningAuthority running)
                <> "\r\nAuthorization: Bearer " <> token <> "\r\nContent-Type: application/json\r\nContent-Length: "
                <> Text.pack (show (BS.length bytes)) <> "\r\nConnection: close\r\n\r\n")
        requestRaw host (headBytes <> bytes)
  wrongMethod <- bearerRequest running "GET /api/v1/adrs" []
  checkError "wrong method" 405 "malformed-input" "wrong-method" wrongMethod
  invalidAdr <- bearerRequest running "GET /api/v1/adrs/bad" []
  checkError "invalid ADR path" 400 "malformed-input" "invalid-field" invalidAdr
  duplicateQuery <- bearerRequest running "GET /api/v1/search?q=a&q=b" []
  checkError "duplicate query" 400 "malformed-input" "duplicate-query" duplicateQuery
  unknownJson <- sendJson "/api/v1/adrs" (Aeson.object
    [ "repository_state" Aeson..= Aeson.Null,
      "title" Aeson..= ("rejected" :: Text),
      "summary" Aeson..= ("rejected" :: Text),
      "body" Aeson..= ("rejected" :: Text),
      "actor" Aeson..= Aeson.Null,
      "unexpected" Aeson..= True
    ])
  checkError "unknown JSON field" 400 "malformed-input" "unknown-json-field" unknownJson
  cookie <- sessionCookiePair running
  cookieOnly <- request host
    ("POST /api/v1/adrs HTTP/1.1\r\nHost: " <> host <> "\r\nOrigin: " <> Security.authorityOrigin (runningAuthority running)
      <> "\r\nCookie: " <> cookie <> "\r\nContent-Type: application/json\r\nContent-Length: 2\r\nConnection: close\r\n\r\n{}")
  checkError "cookie-only mutation" 401 "authentication" "authentication-failed" cookieOnly
  queryCredential <- bearerRequest running ("GET /api/v1/repository?token=" <> token) []
  checkError "API query credential" 401 "authentication" "authentication-failed" queryCredential
  invalidSocketHost <- request host
    ("GET /api/v1/events HTTP/1.1\r\nHost: 127.0.0.1:1\r\nOrigin: " <> Security.authorityOrigin (runningAuthority running)
      <> "\r\nAuthorization: Bearer " <> token
      <> "\r\nConnection: Upgrade, close\r\nUpgrade: websocket\r\nSec-WebSocket-Version: 13\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\r\n")
  checkError "WebSocket invalid Host" 403 "origin" "origin-rejected" invalidSocketHost

testEventsUnavailable :: IO ()
testEventsUnavailable = withServer $ \running _ -> do
  response <- bearerRequest running "GET /api/v1/events" [("Origin", Security.authorityOrigin (runningAuthority running))]
  assertBool "events are explicitly unavailable" ("HTTP/1.1 503" `BS.isPrefixOf` response)
  assertBool "metadata is unavailable, not an ambient commit" ("X-Adrai-As-Of: unavailable:events-transport-unavailable" `BS.isInfixOf` response)

testRelevantV2Route :: IO ()
testRelevantV2Route = withSeededRepository $ \root -> do
  repository <- discoverRepository systemGit root >>= either (assertFailure . show) pure
  (_, committed) <- seedRelevantDecision repository
  -- This leaf owns the read projection. HTTP mutation behavior is covered by
  -- testMutationRoutes; prepare its real sealed state without a timed POST.
  _ <- Runtime.ensureExactArchive repository committed >>= either (assertFailure . Text.unpack) pure
  started <- withWebServer dependencies root (Api.WebOptions Nothing False) $ \running _ ->
    assertRelevantProjection running (gitOidText committed)
  either (assertFailure . Text.unpack) pure started

seedRelevantDecision :: Repository -> IO (Text, GitOid)
seedRelevantDecision repository = do
  headOid <- resolveRevision repository (RevisionSpec "HEAD") >>= either (assertFailure . show) pure
  headState <- repositoryHeadState repository >>= either (assertFailure . show) pure
  snapshot <- repositorySnapshot repository (RevisionSpec (gitOidText headOid)) >>= either (assertFailure . show) pure
  actor <- either (assertFailure . show) pure (Types.mkActor Types.HumanActor "web-test" Nothing)
  domain <- either (assertFailure . show) pure (mkDomain "core")
  scope <- either (assertFailure . show) pure (mkScopePattern "src/**")
  created <- Mutation.createAdrCommandAutoChecked
    (ExpectedRepositoryBasis headOid headState) repository (repositorySnapshotManagedPaths snapshot) actor
    "Runtime ADR" "HTTP shared mutation" "runtime body\n" [domain] [scope] Nothing Nothing Nothing
    >>= either (assertFailure . ("relevant fixture checked create: " <>) . show) pure
  assertBool "relevant fixture updates the committed Git index" (Mutation.createIndexUpdated created)
  Mutation.createPublicationError created @?= Nothing
  pure (Types.adrIdText (Mutation.createAdrId created), Mutation.createCommitOid created)

assertRelevantProjection :: RunningServer -> Text -> IO ()
assertRelevantProjection running current = do
  response <- getJson running ("/api/v1/relevant?file=seed.txt&at=" <> current)
  textAt ["data", "schema"] response >>= (@?= "adrai/relevant/v2")
  textAt ["metadata", "as_of", "oid"] response >>= (@?= current)
  textAt ["data", "diagnostics", "index_revision"] response >>= (@?= current)
  textAt ["data", "diagnostics", "checkout_head"] response >>= (@?= current)
  valueAt ["data", "diagnostics", "stale"] response >>= (@?= Aeson.Bool False)
  textAt ["data", "diagnostics", "index_preparation"] response >>= (@?= "cache-hit")
  valueAt ["data", "diagnostics", "timing_ms", "prepare_index"] response >>= \case
    Aeson.Number elapsed -> assertBool "HTTP index preparation elapsed is nonnegative" (elapsed >= 0)
    _ -> assertFailure "HTTP index preparation timing is missing"
  valueAt ["data", "results"] response >>= \case
    Aeson.Array _ -> pure ()
    _ -> assertFailure "relevant route did not return a result array"

testQueryRoutes :: IO ()
testQueryRoutes = withSeededRepository $ \root -> do
  repository <- discoverRepository systemGit root >>= either (assertFailure . show) pure
  (adr, committed) <- seedRelevantDecision repository
  _ <- Runtime.ensureExactArchive repository committed >>= either (assertFailure . Text.unpack) pure
  started <- withWebServer dependencies root (Api.WebOptions Nothing False) $ \running _ ->
    assertQueryRouteSnapshots root repository adr (gitOidText committed) running
  either (assertFailure . Text.unpack) pure started

assertQueryRouteSnapshots :: FilePath -> Repository -> Text -> Text -> RunningServer -> IO ()
assertQueryRouteSnapshots root repository adr current running = do
  sharedShown <- Query.runWebShow repository (Query.ShowRequest adr CollapsedView current False) >>= either (assertFailure . Text.unpack . Query.showFailureText) pure
  httpShown <- getJson running ("/api/v1/adrs/" <> adr <> "?at=" <> current)
  valueAt ["data"] httpShown >>= (@?= Api.apiResultPayload (Api.ApiShowResult sharedShown))
  cliShown <- Query.runShow repository (Query.ShowRequest adr CollapsedView current False) >>= either (assertFailure . Text.unpack . Query.showFailureText) pure
  assertBool "web inspection retains rich candidate detail without changing CLI compact show" (Api.apiResultPayload (Api.ApiShowResult sharedShown) /= Api.apiResultPayload (Api.ApiShowResult cliShown))
  defaultRelevant <- getJson running "/api/v1/relevant?file=seed.txt"
  namedRelevant <- getJson running "/api/v1/relevant?file=seed.txt&at=main"
  explicitRelevant <- getJson running ("/api/v1/relevant?file=seed.txt&at=" <> current)
  mapM_ (\response -> textAt ["data", "schema"] response >>= (@?= "adrai/relevant/v2"))
    [defaultRelevant, namedRelevant, explicitRelevant]
  mapM_ (\response -> valueAt ["data"] response >>= \value -> assertBool "relevant query returned a projection" (value /= Aeson.Null))
    [defaultRelevant, namedRelevant, explicitRelevant]
  currentOid <- resolveRevision repository (RevisionSpec current) >>= either (assertFailure . show) pure
  relevantPath <- either (assertFailure . show) pure (Types.mkRepoPath "seed.txt")
  BS.writeFile (root </> "seed.txt") "modified worktree relevance bytes"
  worktreeRelevant <- getJson running "/api/v1/relevant?file=seed.txt&worktree=true"
  textAt ["data", "file", "source"] worktreeRelevant >>= (@?= "worktree")
  textAt ["metadata", "as_of", "oid"] worktreeRelevant >>= (@?= current)
  committedRelevant <- getJson running ("/api/v1/relevant?file=seed.txt&at=" <> current)
  textAt ["data", "file", "source"] committedRelevant >>= (@?= "revision")
  worktreeDigest <- textAt ["data", "file", "digest"] worktreeRelevant
  committedDigest <- textAt ["data", "file", "digest"] committedRelevant
  assertBool "worktree and committed relevance use distinct file bytes" (worktreeDigest /= committedDigest)
  Query.runRelevantQueryExact repository currentOid (DomainQuery.RelevantRequest relevantPath Types.WorkingRevision False 10)
    >>= either (assertFailure . Text.unpack . Query.relevantFailureText) (const (pure ()))
  blankSearch <- getJson running ("/api/v1/search?q=&at=" <> current <> "&limit=1000")
  valueAt ["data", "limit"] blankSearch >>= (@?= Aeson.Number 1000)
  _ <- getJson running ("/api/v1/history?adr=" <> adr <> "&at=" <> current <> "&limit=1000&actor=service:service&since=-1&until=1000")
  _ <- getJson running ("/api/v1/doctor?at=" <> current)
  let archive = root </> ".adrai" </> "cache" </> Text.unpack current <> ".sqlite"
  bracket (SQLite.open archive) SQLite.close $ \connection ->
    SQLite.execute connection "DELETE FROM meta WHERE key=?" (Only ("managed_source_count" :: Text))
  executable <- lookupEnv "ADRAI_EXE" >>= maybe (assertFailure "ADRAI_EXE is required") pure
  (doctorExit, doctorStdout, _) <- readCreateProcessWithExitCode ((proc executable ["doctor", "--at", Text.unpack current, "--json"]) {cwd = Just root}) ""
  doctorExit @?= ExitSuccess
  cliDoctor <- maybe (assertFailure "CLI doctor did not return JSON") pure (Aeson.decodeStrict' (BS8.pack doctorStdout))
  httpDoctor <- getJson running ("/api/v1/doctor?at=" <> current)
  let cliDatabase = root </> ".adrai" </> "index.sqlite"
  valueAt ["database"] cliDoctor >>= (@?= Aeson.String (Text.pack cliDatabase))
  validateExactCacheTarget archive current >>= assertBool "HTTP doctor reads a validated exact archive"
  valueAt ["data", "database"] httpDoctor >>= (@?= Aeson.String (Text.pack archive))
  let expectedHttpDoctor = case cliDoctor of
        Aeson.Object fields -> Aeson.Object (KeyMap.insert "database" (Aeson.String (Text.pack archive)) fields)
        other -> other
  valueAt ["data"] httpDoctor >>= (@?= expectedHttpDoctor)
  bracket (SQLite.open archive) SQLite.close $ \connection -> do
    [Only operationCommits] <- SQLite.query_ connection "SELECT COUNT(*) FROM operation_commit" :: IO [Only Int]
    [Only coverageRows] <- SQLite.query_ connection "SELECT COUNT(*) FROM operation_target_coverage" :: IO [Only Int]
    assertBool "committed overlay rows are visible to the published exact doctor cache" (operationCommits > 0 && coverageRows > 0)
  callProcess "git" ["-C", root, "commit", "--allow-empty", "-m", "move after exact request basis"]
  ambient <- gitHead root
  assertBool "ambient HEAD moved after the exact response basis" (ambient /= current)
  pinnedNamed <- Query.runRelevantQueryExact repository currentOid (DomainQuery.RelevantRequest relevantPath (Types.AtRevision "main") False 10)
    >>= either (assertFailure . Text.unpack . Query.relevantFailureText) pure
  revisionRequested (DomainQuery.relevantProjectionRevision pinnedNamed) @?= "main"
  revisionResolved (DomainQuery.relevantProjectionRevision pinnedNamed) @?= current
  DomainQuery.relevantFileRevision (DomainQuery.relevantProjectionFile pinnedNamed) @?= current
  DomainQuery.relevantFileSource (DomainQuery.relevantProjectionFile pinnedNamed) @?= "revision"
  let routes =
        [ "/api/v1/adrs/" <> adr <> "?at=" <> current,
          "/api/v1/history?adr=" <> adr <> "&at=" <> current,
          "/api/v1/compare?from=" <> current <> "&to=" <> current,
          "/api/v1/conflicts?at=" <> current,
          "/api/v1/doctor?at=" <> current,
          "/api/v1/search?q=runtime&at=" <> current,
          "/api/v1/relevant?file=seed.txt&at=" <> current
        ]
      readAfterMove route = getMonotonicTimeNSec >>= \started -> retryBusyRead route started (1 :: Int) []
      responseField [] value = Just value
      responseField (key : rest) (Aeson.Object fields) = KeyMap.lookup (Key.fromText key) fields >>= responseField rest
      responseField _ _ = Nothing
      checkPinned route attempt response = do
        let asOf = responseField ["metadata", "as_of"] response
            matches = case asOf of
              Just (Aeson.Object fields) -> case KeyMap.lookup "kind" fields of
                Just (Aeson.String "comparison") ->
                  KeyMap.lookup "from" fields == Just (Aeson.String current)
                    && KeyMap.lookup "to" fields == Just (Aeson.String current)
                Just (Aeson.String "commit") -> KeyMap.lookup "oid" fields == Just (Aeson.String current)
                _ -> False
              _ -> False
        assertBool ("exact read returned wrong revision: " <> Text.unpack route <> ", attempt " <> show attempt <> ", HTTP 200, expected " <> Text.unpack current <> ", as_of " <> show asOf) matches
      retryBusyRead route started attempt statuses = do
        -- Guard an in-flight HTTP request separately from bounded busy retries.
        -- The old three-second guard canceled successful exact reads mid-request.
        result <- timeout 30000000 (getJsonStatus running route)
        case result of
          Nothing -> do
            elapsed <- getMonotonicTimeNSec
            assertFailure ("exact read timed out waiting for HTTP response: " <> Text.unpack route <> ", attempt " <> show attempt <> ", elapsed " <> show ((elapsed - started) `div` 1000000) <> " ms, prior statuses " <> show (reverse statuses))
          Just (status, response) -> case status of
            200 -> checkPinned route attempt response
            503 -> do
              let typedBusy =
                    responseField ["error", "category"] response == Just (Aeson.String "service-failure")
                      && responseField ["error", "status"] response == Just (Aeson.Number 503)
                      && responseField ["error", "code"] response == Just (Aeson.String "repository-busy")
                      && responseField ["metadata", "as_of", "kind"] response == Just (Aeson.String "unavailable")
              assertBool ("exact read returned malformed busy response: " <> Text.unpack route <> ", attempt " <> show attempt <> ", HTTP 503, response " <> show response) typedBusy
              elapsed <- getMonotonicTimeNSec
              if elapsed - started >= 15000000000
                then assertFailure ("exact read stayed repository-busy for fifteen seconds: " <> Text.unpack route <> ", attempts " <> show attempt <> ", statuses " <> show (reverse (status : statuses)))
                else threadDelay 50000 >> retryBusyRead route started (attempt + 1) (status : statuses)
            _ -> assertFailure ("exact read returned unexpected HTTP status for " <> Text.unpack route <> ", attempt " <> show attempt <> ", HTTP " <> show status <> ", response " <> show response)
  mapM_ readAfterMove routes
  withSeededRepository $ \movingRoot -> do
    moved <- newIORef False
    let moveAfterResolution = do
          shouldMove <- atomicModifyIORef' moved (\seen -> (True, not seen))
          if shouldMove then callProcess "git" ["-C", movingRoot, "commit", "--allow-empty", "-m", "move between resolution and stamp"] else pure ()
        services = defaultApplicationServices {applicationAfterQueryResolution = moveAfterResolution}
        injected = dependencies {serverApplicationServices = services}
    started <- withWebServer injected movingRoot (Api.WebOptions Nothing False) $ \moving _ -> do
      response <- getJson moving "/api/v1/repository"
      payloadHead <- textAt ["data", "head"] response
      metadataHead <- textAt ["metadata", "as_of", "oid"] response
      ambientHead <- gitHead movingRoot
      payloadHead @?= metadataHead
      assertBool "HEAD moved after resolution but before the response adapter returned" (ambientHead /= payloadHead)
    either (assertFailure . Text.unpack) pure started

testMutationRoutes :: IO ()
testMutationRoutes = withSeededRepository $ \root -> do
  blockNextArchive <- newIORef False
  blockedArchive <- newIORef Nothing
  lastPhase <- newIORef ("server not started" :: String)
  fixtureRepository <- discoverRepository systemGit root >>= either (assertFailure . show) pure
  phaseOrigin <- getMonotonicTimeNSec
  let recordPhase label = do
        now <- getMonotonicTimeNSec
        writeIORef lastPhase label
        putStrLn ("all-six server " <> show ((now - phaseOrigin) `div` 1000000) <> " ms: " <> label)
        hFlush stdout
      onFailure = do
        phase <- readIORef lastPhase
        lock <- gitLockStatus fixtureRepository
        putStrLn ("all-six failure; last server phase " <> phase <> ", Git lock " <> show lock)
  let compileExact repository oid = do
        recordPhase "exact archive compilation entered"
        shouldBlock <- atomicModifyIORef' blockNextArchive (\armed -> (False, armed))
        if shouldBlock then do
          let archive = root </> ".adrai" </> "cache" </> Text.unpack (gitOidText oid) <> ".sqlite"
          createDirectoryIfMissing True (root </> ".adrai" </> "cache")
          createDirectory archive
          writeIORef blockedArchive (Just archive)
        else pure ()
        result <- Runtime.ensureExactArchive repository oid
        recordPhase "exact archive compilation returned"
        pure result
      dispatch compilation compileExactService afterJoin fallback allocate afterResolve publisher repo requestValue = do
        let operation = case requestValue of
              Api.ApiCreateRequest _ _ -> "create"
              Api.ApiAmendRequest _ -> "amend"
              Api.ApiScopeRequest _ -> "scope"
              Api.ApiDomainRequest _ -> "domain"
              Api.ApiObsoleteRequest _ -> "obsolete"
              Api.ApiReactivateRequest _ -> "reactivate"
              _ -> "read"
        recordPhase ("dispatch entered " <> operation)
        result <- dispatchApplicationRequest defaultApplicationServices compilation compileExactService afterJoin fallback allocate afterResolve publisher repo requestValue
        recordPhase ("dispatch returned " <> operation)
        pure result
      services = defaultApplicationServices {applicationCompileExact = compileExact, dispatchApplicationRequest = dispatch}
      injected = dependencies {serverApplicationServices = services}
  started <- withWebServer injected root (Api.WebOptions Nothing False) $ \running _ ->
    testMutationRoutesOnServer root running blockNextArchive blockedArchive `onException` onFailure
  either (assertFailure . Text.unpack) pure started

testMutationRoutesOnServer :: FilePath -> RunningServer -> IORef Bool -> IORef (Maybe FilePath) -> IO ()
testMutationRoutesOnServer root running blockNextArchive blockedArchive = do
  basis0 <- repositoryBasis running
  created <- postJsonLabeled 60000000 "all-six create" running "/api/v1/adrs" (createBody basis0)
  adr <- textAt ["data", "adr"] created
  assertCommitted created
  initiallyShown <- getJson running ("/api/v1/adrs/" <> adr)
  staleState <- valueAt ["data", "state_token"] initiallyShown
  mutateExisting running adr "amend" ["change_summary" Aeson..= ("revise" :: Text), "title" Aeson..= ("Revised" :: Text), "summary" Aeson..= ("revised" :: Text), "body" Aeson..= ("body two\n" :: Text)] >>= assertCommitted
  mutateExisting running adr "scope" ["reason" Aeson..= ("broaden" :: Text), "mode" Aeson..= ("delta" :: Text), "add" Aeson..= (["docs/**"] :: [Text]), "remove" Aeson..= ([] :: [Text])] >>= assertCommitted
  mutateExisting running adr "domain" ["reason" Aeson..= ("broaden" :: Text), "mode" Aeson..= ("delta" :: Text), "add" Aeson..= (["ui"] :: [Text]), "remove" Aeson..= ([] :: [Text])] >>= assertCommitted
  mutateExisting running adr "obsolete" ["reason" Aeson..= ("retired" :: Text)] >>= assertCommitted
  writeIORef blockNextArchive True
  reactivated <- mutateExisting running adr "reactivate" ["reason" Aeson..= ("needed again" :: Text)]
  assertCommitted reactivated
  reactivatedOid <- textAt ["data", "commit"] reactivated
  durableHead <- gitHead root
  reactivatedOid @?= durableHead
  physicallyBlocked <- readIORef blockedArchive >>= maybe (assertFailure "the exact archive obstruction was not reached") pure
  physicallyBlocked @?= root </> ".adrai" </> "cache" </> Text.unpack reactivatedOid <> ".sqlite"
  doesDirectoryExist physicallyBlocked >>= (@?= True)
  valueAt ["data", "indexed"] reactivated >>= (@?= Aeson.Bool False)
  indexError <- valueAt ["data", "index_error"] reactivated
  assertBool "post-commit index failure is reported as a warning payload" (indexError /= Aeson.Null)
  currentBasis <- repositoryBasis running
  let stale operation fields = postJsonStatus running ("/api/v1/adrs/" <> adr <> "/" <> operation) $ Aeson.object
        ([ "repository_state" Aeson..= currentBasis,
           "state_token" Aeson..= staleState,
           "actor" Aeson..= actorValue
         ] <> fields)
  rejected <- sequence
    [ stale "amend" ["change_summary" Aeson..= ("stale" :: Text), "title" Aeson..= ("No" :: Text), "summary" Aeson..= ("No" :: Text), "body" Aeson..= ("No\n" :: Text)],
      stale "scope" ["reason" Aeson..= ("stale" :: Text), "mode" Aeson..= ("delta" :: Text), "add" Aeson..= (["stale/**"] :: [Text]), "remove" Aeson..= ([] :: [Text])],
      stale "domain" ["reason" Aeson..= ("stale" :: Text), "mode" Aeson..= ("delta" :: Text), "add" Aeson..= (["stale"] :: [Text]), "remove" Aeson..= ([] :: [Text])],
      stale "obsolete" ["reason" Aeson..= ("stale" :: Text)],
      stale "reactivate" ["reason" Aeson..= ("stale" :: Text)]
    ]
  mapM_ (\(status, _) -> status @?= 409) rejected
  withSeededRepository $ \orderedRoot -> do
    committed <- newEmptyMVar
    release <- newEmptyMVar
    responseGate <- newEmptyMVar
    orderedRepository <- discoverRepository systemGit orderedRoot >>= either (assertFailure . show) pure
    dispatchPhase <- newIORef ("not entered" :: String)
    compilePhase <- newIORef ("not entered" :: String)
    clientPhase <- newIORef ("not started" :: String)
    phaseOrigin <- getMonotonicTimeNSec
    let recordPhase phaseRef label = do
          now <- getMonotonicTimeNSec
          writeIORef phaseRef label
          putStrLn ("controlled dispatch " <> show ((now - phaseOrigin) `div` 1000000) <> " ms: " <> label)
          hFlush stdout
        readinessMicros = 130000000
        observationMicros = 10000000
        postReleaseMicros = 5000000
        socketMicros = 150000000
        compileExact repository oid = do
          recordPhase compilePhase "exact archive compilation entered"
          result <- Runtime.ensureExactArchive repository oid
          recordPhase compilePhase "exact archive compilation returned"
          pure result
        delayedDispatch compilation compileExactService afterJoin fallback allocate afterResolve publisher repo requestValue = do
          recordPhase dispatchPhase "dispatch entered"
          outcome <- dispatchApplicationRequest defaultApplicationServices compilation compileExactService afterJoin fallback allocate afterResolve publisher repo requestValue
          recordPhase dispatchPhase "dispatch returned"
          case (requestValue, outcome) of
            (Api.ApiCreateRequest _ _, Right _) -> do
              recordPhase dispatchPhase "commit signaled; awaiting release"
              putMVar committed ()
              takeMVar release
              recordPhase dispatchPhase "release observed"
            _ -> pure ()
          pure outcome
        services = defaultApplicationServices {applicationCompileExact = compileExact, dispatchApplicationRequest = delayedDispatch}
        injected = dependencies {serverApplicationServices = services}
        releaseHeld = do
          void (tryPutMVar release ())
          void (tryPutMVar responseGate ())
        reportFailure delayed = do
          dispatch <- readIORef dispatchPhase
          compile <- readIORef compilePhase
          client <- readIORef clientPhase
          lock <- gitLockStatus orderedRepository
          clientResult <- poll delayed
          let clientStatus = case clientResult of
                Nothing -> "running"
                Just (Left failure) -> "failed: " <> show failure
                Just (Right _) -> "returned"
          putStrLn ("controlled dispatch failure: dispatch " <> dispatch <> ", compile " <> compile <> ", client " <> client <> " (" <> clientStatus <> "), Git lock " <> show lock)
          hFlush stdout
    started <- withWebServer injected orderedRoot (Api.WebOptions Nothing False) $ \ordered _ -> do
      basis <- repositoryBasis ordered
      cookie <- sessionCookiePair ordered
      recordPhase clientPhase "session cookie ready; held HTTP request not started"
      let heldClient = do
            recordPhase clientPhase "held HTTP request started"
            (status, value) <- postJsonStatusWithCookie (requestRawHeld (recordPhase clientPhase) socketMicros responseGate) ordered cookie "/api/v1/adrs" (createBody basis)
            if status == 200
              then recordPhase clientPhase "held HTTP client returned" >> pure value
              else assertFailure ("POST returned HTTP " <> show status <> ": " <> show value)
      withAsync heldClient $ \delayed ->
        (do
           ready <- timeout readinessMicros (race (takeMVar committed) (waitCatch delayed))
           case ready of
             Nothing -> assertFailure "controlled HTTP dispatch did not finish within its 130-second readiness guard"
             Just (Right (Left failure)) -> assertFailure ("controlled HTTP client failed before dispatch was ready: " <> show failure)
             Just (Right (Right _)) -> assertFailure "controlled HTTP client returned before its response gate was released"
             Just (Left ()) -> pure ()
           observed <- timeout observationMicros (getJson ordered "/api/v1/repository")
             >>= maybe (assertFailure "controlled repository observation exceeded its ten-second guard") pure
           recordPhase clientPhase "repository observed; releasing held response"
           releaseHeld
           mutation <- timeout postReleaseMicros (wait delayed) >>= maybe (assertFailure "controlled HTTP response exceeded its five-second post-release budget") pure
           mutationGeneration <- integerAt ["metadata", "generation"] mutation
           observedGeneration <- integerAt ["metadata", "generation"] observed
           assertBool "a delayed mutation response retains its commit-time generation" (mutationGeneration < observedGeneration)
           mutationOid <- textAt ["metadata", "as_of", "oid"] mutation
           observedOid <- textAt ["data", "head"] observed
           mutationOid @?= observedOid
         ) `onException` reportFailure delayed `finally` releaseHeld
    either (assertFailure . Text.unpack) pure started
  withSeededRepository $ \lockedRoot -> do
    reached <- newEmptyMVar
    release <- newEmptyMVar
    lockedRepository <- discoverRepository systemGit lockedRoot >>= either (assertFailure . show) pure
    phaseOrigin <- getMonotonicTimeNSec
    phases <- newIORef ([] :: [(String, Word64)])
    let recordPhase label = do
          now <- getMonotonicTimeNSec
          atomicModifyIORef' phases (\events -> ((label, now) : events, ()))
        reportPhases = do
          events <- reverse <$> readIORef phases
          putStrLn ("external-advance publication phases (ms): " <> show [(label, (at - phaseOrigin) `div` 1000000) | (label, at) <- events])
          hFlush stdout
        reportFailure delayed = do
          reportPhases
          lock <- gitLockStatus lockedRepository
          client <- poll delayed
          let clientStatus = case client of
                Nothing -> "running"
                Just (Left failure) -> "failed: " <> show failure
                Just (Right _) -> "returned"
          putStrLn ("external-advance failure: client " <> clientStatus <> "; Git lock " <> show lock)
          hFlush stdout
        releaseHeld = void (tryPutMVar release ())
        externalCommit = bracket
          (createProcess (proc "git" ["-C", lockedRoot, "commit", "--allow-empty", "-m", "external move during commit publication"]))
          (\(_, _, _, processHandle) -> do
            processState <- getProcessExitCode processHandle
            case processState of
              Just _ -> pure ()
              Nothing -> do
                terminateProcess processHandle
                reaped <- timeout 5000000 (waitForProcess processHandle)
                assertBool "external-advance Git child survived bounded termination" (maybe False (const True) reaped))
          (\(_, _, _, processHandle) -> do
            completed <- timeout 20000000 (waitForProcess processHandle)
            case completed of
              Nothing -> assertFailure "external-advance Git commit exceeded twenty seconds"
              Just ExitSuccess -> pure ()
              Just code -> assertFailure ("external-advance Git commit failed: " <> show code))
        compileExact repository oid = do
          recordPhase "post-commit exact archive compilation entered"
          result <- Runtime.ensureExactArchive repository oid
          recordPhase "post-commit exact archive compilation returned"
          pure result
        dispatch compilation compileExactService afterJoin fallback allocate afterResolve publisher repo requestValue = do
          recordPhase "create dispatch entered"
          result <- dispatchApplicationRequest defaultApplicationServices compilation compileExactService afterJoin fallback allocate afterResolve publisher repo requestValue
          recordPhase "create dispatch returned"
          pure result
        beforePublication _ = do
          recordPhase "commit publication callback entered"
          putMVar reached ()
          takeMVar release
          recordPhase "commit publication callback released"
        services = defaultApplicationServices
          { applicationBeforeCommitPublication = beforePublication,
            applicationAfterCompilationJoin = recordPhase "post-commit compilation joined",
            applicationCompileExact = compileExact,
            dispatchApplicationRequest = dispatch
          }
        injected = dependencies {serverApplicationServices = services}
    started <- withWebServer injected lockedRoot (Api.WebOptions Nothing False) $ \locked _ -> do
      basis <- repositoryBasis locked
      cookie <- sessionCookiePair locked
      let delayedClient = do
            recordPhase "create POST client started"
            (status, value) <- postJsonStatusWithCookie
              (requestRawWithStepAndDeadlineObserved (recordPhase . ("client " <>)) 135000000 "external-advance create")
              locked cookie "/api/v1/adrs" (createBody basis)
            recordPhase "create POST response received"
            if status == 200
              then pure value
              else assertFailure ("POST returned HTTP " <> show status <> ": " <> show value)
      withAsync delayedClient $ \delayed -> do
        completed <- timeout 150000000 $ (do
          ready <- timeout 30000000 (race (takeMVar reached) (waitCatch delayed))
          case ready of
            Nothing -> assertFailure "external-advance commit publication callback did not enter within thirty seconds"
            Just (Right (Left failure)) -> assertFailure ("external-advance POST failed before callback entry: " <> show failure)
            Just (Right (Right _)) -> assertFailure "external-advance POST returned before callback entry"
            Just (Left ()) -> pure ()
          recordPhase "repository busy observation started"
          (busyStatus, busy) <- timeout 10000000 (getJsonStatus locked "/api/v1/repository")
            >>= maybe (assertFailure "external-advance repository busy observation exceeded ten seconds") pure
          recordPhase "repository busy observation returned"
          busyStatus @?= 503
          textAt ["metadata", "as_of", "kind"] busy >>= (@?= "unavailable")
          recordPhase "external Git commit started"
          externalCommit
          recordPhase "external Git commit returned"
          externalHead <- timeout 10000000 (gitHead lockedRoot)
            >>= maybe (assertFailure "external-advance HEAD lookup exceeded ten seconds") pure
          recordPhase "external HEAD resolved"
          releaseHeld
          recordPhase "callback release signaled"
          result <- timeout 60000000 (waitCatch delayed)
          mutation <- case result of
            Nothing -> assertFailure "external-advance POST did not finish within its sixty-second post-release guard"
            Just (Left failure) -> assertFailure ("external-advance POST failed after callback release: " <> show failure)
            Just (Right response) -> pure response
          assertCommitted mutation
          mutationOid <- textAt ["metadata", "as_of", "oid"] mutation
          assertBool "the mutation retains its own committed OID after an external advance" (mutationOid /= externalHead)
          retry <- timeout 10000000 (getJson locked "/api/v1/repository")
            >>= maybe (assertFailure "external-advance later repository observation exceeded ten seconds") pure
          mutationGeneration <- integerAt ["metadata", "generation"] mutation
          retryGeneration <- integerAt ["metadata", "generation"] retry
          assertBool "the observation after lock release has a later generation" (retryGeneration > mutationGeneration)
          reportPhases
          ) `onException` reportFailure delayed `finally` releaseHeld
        case completed of
          Nothing -> assertFailure "external-advance fixture exceeded its 150-second whole-operation guard"
          Just () -> pure ()
    either (assertFailure . Text.unpack) pure started
  withSeededRepository $ \queryRoot -> do
    armed <- newIORef False
    reached <- newEmptyMVar
    release <- newEmptyMVar
    let afterResolution = do
          shouldPause <- atomicModifyIORef' armed (\value -> (False, value))
          if shouldPause then putMVar reached () >> takeMVar release else pure ()
        services = defaultApplicationServices {applicationAfterQueryResolution = afterResolution}
        injected = dependencies {serverApplicationServices = services}
    started <- withWebServer injected queryRoot (Api.WebOptions Nothing False) $ \queryServer _ -> do
      writeIORef armed True
      delayed <- async (getJson queryServer "/api/v1/repository")
      takeMVar reached
      (busyStatus, _) <- getJsonStatus queryServer "/api/v1/repository"
      busyStatus @?= 503
      callProcess "git" ["-C", queryRoot, "commit", "--allow-empty", "-m", "external move after query resolution"]
      movedHead <- gitHead queryRoot
      putMVar release ()
      retained <- wait delayed
      retainedHead <- textAt ["data", "head"] retained
      textAt ["metadata", "as_of", "oid"] retained >>= (@?= retainedHead)
      assertBool "the fenced query retains the OID resolved before the external move" (retainedHead /= movedHead)
    either (assertFailure . Text.unpack) pure started
  withSeededRepository $ \failureRoot -> do
    phaseOrigin <- getMonotonicTimeNSec
    phases <- newIORef ([] :: [(String, Word64)])
    let recordPhase label = do
          now <- getMonotonicTimeNSec
          atomicModifyIORef' phases (\events -> ((label, now) : events, ()))
        reportPhases = do
          events <- reverse <$> readIORef phases
          putStrLn ("all-six publication failure phases (ms): " <> show [(label, (at - phaseOrigin) `div` 1000000) | (label, at) <- events])
          hFlush stdout
        compileExact repository oid = do
          recordPhase "post-commit exact archive compilation entered"
          result <- Runtime.ensureExactArchive repository oid
          recordPhase "post-commit exact archive compilation returned"
          pure result
        dispatch compilation compileExactService afterJoin fallback allocate afterResolve publisher repo requestValue = do
          recordPhase "server dispatch entered"
          result <- dispatchApplicationRequest defaultApplicationServices compilation compileExactService afterJoin fallback allocate afterResolve publisher repo requestValue
          recordPhase "server dispatch returned"
          pure result
        services = defaultApplicationServices
          { applicationBeforeCommitPublication = \_ -> recordPhase "commit publication callback entered" >> ioError (userError "injected publication failure"),
            applicationAfterCompilationJoin = recordPhase "post-commit compilation joined",
            applicationCompileExact = compileExact,
            dispatchApplicationRequest = dispatch
          }
        injected = dependencies {serverApplicationServices = services}
    started <- (withWebServer injected failureRoot (Api.WebOptions Nothing False) $ \failureServer _ -> do
      basis <- repositoryBasis failureServer
      before <- gitHead failureRoot
      recordPhase "final POST starting"
      (status, committed) <- postJsonStatusWith
        (requestRawWithStepAndDeadlineObserved (recordPhase . ("client " <>)) 60000000 "all-six publication-failure create")
        failureServer "/api/v1/adrs" (createBody basis)
      recordPhase "final POST response received"
      status @?= 200
      assertCommitted committed
      after <- gitHead failureRoot
      assertBool "synchronous publication failure cannot roll back the durable commit" (after /= before)
      textAt ["data", "commit"] committed >>= (@?= after)
      textAt ["data", "publication_warning"] committed >>= (@?= "commit generation publication failed after the durable commit")
      textAt ["metadata", "as_of", "kind"] committed >>= (@?= "unavailable")
      port <- either assertFailure pure (authorityPort (Security.authorityHost (runningAuthority failureServer)))
      receiveStarted <- newEmptyMVar
      timedOut <- runOwnedSocket 1000000 port $ \client -> do
        putMVar receiveStarted ()
        recv client 4096
      tryTakeMVar receiveStarted >>= (@?= Just ())
      case timedOut of
        Nothing -> recordPhase "blocked receive timed out; owned socket worker joined"
        Just result -> assertFailure ("idle socket receive unexpectedly completed before its deadline: " <> show result)) `finally` reportPhases
    either (assertFailure . Text.unpack) pure started

testBoundsAndStale :: IO ()
testBoundsAndStale = withSeededServer $ \root running -> do
  huge <- bearerRequest running ("GET /app.css?" <> Text.replicate 5000 "x") []
  assertBool "encoded query bound applies to assets" ("HTTP/1.1 413" `BS.isPrefixOf` huge)
  basis0 <- repositoryBasis running
  created <- postJsonLabeled 60000000 "bounds create" running "/api/v1/adrs" (createBody basis0)
  adr <- textAt ["data", "adr"] created
  shown <- getJson running ("/api/v1/adrs/" <> adr)
  state <- valueAt ["data", "state_token"] shown
  basis <- repositoryBasis running
  let host = Security.authorityHost (runningAuthority running)
      token = bootstrapToken running
      oversized = "POST /api/v1/adrs HTTP/1.1\r\nHost: " <> host <> "\r\nOrigin: " <> Security.authorityOrigin (runningAuthority running) <> "\r\nAuthorization: Bearer " <> token <> "\r\nContent-Type: application/json\r\nContent-Length: 1048577\r\nConnection: close\r\n\r\n{}"
  rejected <- request host oversized
  assertBool "oversized body is rejected before buffering" ("HTTP/1.1 413" `BS.isPrefixOf` rejected)
  let chunk = BS.replicate 1048577 120
      chunkedHead = TextEncoding.encodeUtf8
        ("POST /api/v1/adrs HTTP/1.1\r\nHost: " <> host <> "\r\nOrigin: " <> Security.authorityOrigin (runningAuthority running)
          <> "\r\nAuthorization: Bearer " <> token <> "\r\nContent-Type: application/json\r\nTransfer-Encoding: chunked\r\nConnection: close\r\n\r\n100001\r\n")
  chunkedRejected <- requestRaw host (chunkedHead <> chunk <> "\r\n0\r\n\r\n")
  assertBool "unknown-length chunked body is bounded while reading" ("HTTP/1.1 413" `BS.isPrefixOf` chunkedRejected)
  repositoryBasis running >>= \_ -> pure ()
  missing <- postJsonStatus running "/api/v1/adrs" (withoutField "repository_state" (createBody basis))
  fst missing @?= 400
  invalid <- postJsonStatus running "/api/v1/adrs" (createBody (corruptRepositoryToken basis))
  fst invalid @?= 409
  let routeBodies =
        [ ("amend", ["change_summary" Aeson..= ("invalid" :: Text), "title" Aeson..= ("No" :: Text), "summary" Aeson..= ("No" :: Text), "body" Aeson..= ("No\n" :: Text)]),
          ("scope", ["reason" Aeson..= ("invalid" :: Text), "mode" Aeson..= ("delta" :: Text), "add" Aeson..= (["invalid/**"] :: [Text]), "remove" Aeson..= ([] :: [Text])]),
          ("domain", ["reason" Aeson..= ("invalid" :: Text), "mode" Aeson..= ("delta" :: Text), "add" Aeson..= (["invalid"] :: [Text]), "remove" Aeson..= ([] :: [Text])]),
          ("obsolete", ["reason" Aeson..= ("invalid" :: Text)]),
          ("reactivate", ["reason" Aeson..= ("invalid" :: Text)])
        ]
  BS.writeFile (root </> "seed.txt") (BS.pack [0, 255, 10, 13, 1])
  BS.writeFile (root </> "caller-untracked.bin") (BS.pack [9, 0, 8, 255, 7])
  invalidBefore <- callerState root
  forM_ routeBodies $ \(operation, fields) -> do
    let path = "/api/v1/adrs/" <> adr <> "/" <> operation
        validBody = existingBody basis state fields
    missingRepository <- postJsonStatus running path (withoutField "repository_state" validBody)
    invalidRepository <- postJsonStatus running path (replaceField "repository_state" (corruptRepositoryToken basis) validBody)
    missingAdr <- postJsonStatus running path (withoutField "state_token" validBody)
    invalidAdr <- postJsonStatus running path (replaceField "state_token" (corruptTokenValue state) validBody)
    fst missingRepository @?= 400
    fst invalidRepository @?= 409
    fst missingAdr @?= 400
    fst invalidAdr @?= 409
  callerState root >>= (@?= invalidBefore)
  callProcess "git" ["-C", root, "commit", "--allow-empty", "-m", "external move"]
  afterMove <- gitHead root
  before <- callerState root
  repository <- discoverRepository systemGit root >>= either (assertFailure . show) pure
  cookie <- sessionCookiePair running
  busyCount <- newIORef (0 :: Int)
  let heldStalePost remaining = do
        attempt <- try @GitLockError $ withGitLock repository $ do
          held <- gitLockStatus repository
          case held of
            Left (LockHeld _ _) -> pure ()
            other -> assertFailure ("stale POST fixture did not hold the Git lock: " <> show other)
          (status, response) <- postJsonStatusWithCookie requestRaw running cookie "/api/v1/adrs" (createBody basis)
          status @?= 503
          valueAt ["error", "category"] response >>= (@?= Aeson.String "service-failure")
          valueAt ["error", "status"] response >>= (@?= Aeson.Number 503)
          valueAt ["error", "code"] response >>= (@?= Aeson.String "repository-busy")
          valueAt ["error", "message"] response >>= (@?= Aeson.String "repository is temporarily busy")
          valueAt ["metadata", "as_of", "kind"] response >>= (@?= Aeson.String "unavailable")
          putStrLn ("held Git lock produced typed stale POST repository-busy: " <> show held)
        case attempt of
          Left (LockHeld _ _) | remaining > (0 :: Int) -> threadDelay 20000 >> heldStalePost (remaining - 1)
          Left failure -> throwIO failure
          Right () -> pure ()
      stalePost path body = getMonotonicTimeNSec >>= \started -> retryStalePost started (1 :: Int) path body
      retryStalePost started attempt path body = do
        result@(status, response) <- postJsonStatusWithCookie requestRaw running cookie path body
        case status of
          409 -> pure result
          503 -> do
            category <- valueAt ["error", "category"] response
            errorStatus <- valueAt ["error", "status"] response
            code <- valueAt ["error", "code"] response
            message <- valueAt ["error", "message"] response
            asOf <- valueAt ["metadata", "as_of", "kind"] response
            assertBool ("stale POST returned an untyped 503: " <> Text.unpack path <> ", response " <> show response)
              ( category == Aeson.String "service-failure"
                  && errorStatus == Aeson.Number 503
                  && code == Aeson.String "repository-busy"
                  && message == Aeson.String "repository is temporarily busy"
                  && asOf == Aeson.String "unavailable"
              )
            count <- atomicModifyIORef' busyCount (\seen -> let next = seen + 1 in (next, next))
            if count == 1
              then gitLockStatus repository >>= \lock -> putStrLn ("first stale POST typed repository-busy response: " <> Text.unpack path <> ", lock " <> show lock)
              else pure ()
            elapsed <- getMonotonicTimeNSec
            if elapsed - started >= 15000000000
              then assertFailure ("stale POST stayed repository-busy for fifteen seconds: " <> Text.unpack path <> ", attempts " <> show attempt <> ", response " <> show response)
              else threadDelay 250000 >> retryStalePost started (attempt + 1) path body
          _ -> assertFailure ("stale POST returned HTTP " <> show status <> " instead of 409: " <> Text.unpack path <> ", response " <> show response)
  heldStalePost 250
  (staleCreate, staleExisting) <-
    (do
       create <- stalePost "/api/v1/adrs" (createBody basis)
       existing <- sequence
         [ stalePost ("/api/v1/adrs/" <> adr <> "/amend") (existingBody basis state ["change_summary" Aeson..= ("stale" :: Text), "title" Aeson..= ("No" :: Text), "summary" Aeson..= ("No" :: Text), "body" Aeson..= ("No\n" :: Text)]),
           stalePost ("/api/v1/adrs/" <> adr <> "/scope") (existingBody basis state ["reason" Aeson..= ("stale" :: Text), "mode" Aeson..= ("delta" :: Text), "add" Aeson..= (["stale/**"] :: [Text]), "remove" Aeson..= ([] :: [Text])]),
           stalePost ("/api/v1/adrs/" <> adr <> "/domain") (existingBody basis state ["reason" Aeson..= ("stale" :: Text), "mode" Aeson..= ("delta" :: Text), "add" Aeson..= (["stale"] :: [Text]), "remove" Aeson..= ([] :: [Text])]),
           stalePost ("/api/v1/adrs/" <> adr <> "/obsolete") (existingBody basis state ["reason" Aeson..= ("stale" :: Text)]),
           stalePost ("/api/v1/adrs/" <> adr <> "/reactivate") (existingBody basis state ["reason" Aeson..= ("stale" :: Text)])
         ]
       pure (create, existing))
      `finally` (readIORef busyCount >>= \count -> putStrLn ("stale POST typed repository-busy retries: " <> show count))
  mapM_ (\(status, _) -> status @?= 409) (staleCreate : staleExisting)
  callerState root >>= (@?= before)
  gitHead root >>= (@?= afterMove)

testExecutableAndBindings :: IO ()
testExecutableAndBindings = withSeededRepository $ \root -> do
  executable <- lookupEnv "ADRAI_EXE" >>= maybe (assertFailure "ADRAI_EXE is required") pure
  assertBool "explicit --repo is rejected for web" (either (const True) (const False) (parseArguments ["--repo=.", "web", "--no-open"]))
  assertBool "a repository path named web is not mistaken for the command" (either (const False) (const True) (parseArguments ["--repo", "web", "show", "A0000000000000000000000000"]))
  mapM_ (assertBuiltRejected executable root)
    [ ["--repo=.", "web", "--no-open"],
      ["--repo", ".", "web", "--no-open"],
      ["web", "--repo", ".", "--no-open"]
    ]
  withSystemTempDirectory "adrai-p7-02-non-git" $ \nonGit -> do
    refused <- withWebServer dependencies nonGit (Api.WebOptions Nothing False) (\_ _ -> pure ())
    assertBool "non-Git root is rejected" (either (const True) (const False) refused)
    let bare = nonGit </> "bare.git"
    callProcess "git" ["init", "--bare", bare]
    bareRefused <- withWebServer dependencies bare (Api.WebOptions Nothing False) (\_ _ -> pure ())
    assertBool "bare repository is rejected" (either (const True) (const False) bareRefused)
    assertBuiltRejected executable nonGit ["web", "--no-open"]
    assertBuiltRejected executable bare ["web", "--no-open"]
  let linked = root <> "-linked"
  callProcess "git" ["-C", root, "worktree", "add", "-b", "linked", linked]
  linkedResult <- withWebServer dependencies linked (Api.WebOptions Nothing False) $ \running _ -> do
    response <- bearerRequest running "GET /api/v1/repository" []
    assertBool "linked worktree serves repository" ("HTTP/1.1 200" `BS.isPrefixOf` response)
  either (assertFailure . Text.unpack) pure linkedResult
  exerciseBuiltWeb executable linked
  withSystemTempDirectory "adrai native opener fixture" $ \temporary -> do
    let opener = if os == "mingw32" then "rundll32.exe" else "xdg-open"
        url = "http://127.0.0.1:32123/?fixture=two spaces Ω"
        expected = if os == "mingw32" then ["url.dll,FileProtocolHandler", Text.unpack url] else [Text.unpack url]
        proof = temporary </> "opener-validated"
    copyNativeFixture (temporary </> opener)
    LBS.writeFile (temporary </> "opener-expected.json") (Aeson.encode expected)
    writeFile (temporary </> "opener-exit") "0"
    inherited <- lookupEnv "PATH"
    let selectedPath = temporary <> [searchPathSeparator] <> maybe "" id inherited
        restorePath () = maybe (unsetEnv "PATH") (setEnv "PATH") inherited
    bracket (setEnv "PATH" selectedPath) restorePath $ \() -> do
      exerciseBuiltWeb executable root
      doesFileExist proof >>= (@?= False)
      serverOpenBrowser defaultServerDependencies url >>= (@?= Right ())
      readFile proof >>= (@?= "single argv validated")
      writeFile (temporary </> "opener-exit") "7"
      serverOpenBrowser defaultServerDependencies url >>= (@?= Left "browser opener exited ExitFailure 7")

assertBuiltRejected :: FilePath -> FilePath -> [String] -> IO ()
assertBuiltRejected executable cwdPath arguments = do
  (exitCode, _, _) <- readCreateProcessWithExitCode ((proc executable arguments) {cwd = Just cwdPath}) ""
  assertBool ("built web command unexpectedly accepted " <> show arguments <> " in " <> cwdPath) (exitCode /= ExitSuccess)

exerciseBuiltWeb :: FilePath -> FilePath -> IO ()
exerciseBuiltWeb executable root = do
  startedAt <- getMonotonicTimeNSec
  phaseRef <- newIORef ("launching child" :: String)
  let config = (proc executable ["web", "--no-open"]) {cwd = Just root, std_out = CreatePipe, std_err = Inherit}
      cleanup (_, output, _, processHandle) =
        (do state <- getProcessExitCode processHandle
            case state of
              Nothing -> do
                terminated <- trySynchronous (terminateProcess processHandle)
                case terminated of
                  Left failure -> putStrLn ("built web termination reported " <> show failure)
                  Right () -> pure ()
              Just _ -> pure ()
            stopped <- timeout 10000000 (waitForProcess processHandle)
            case stopped of
              Just _ -> pure ()
              Nothing -> do
                current <- readIORef phaseRef
                now <- getMonotonicTimeNSec
                exit <- getProcessExitCode processHandle
                assertFailure ("built web cleanup wait timed out in " <> root <> " after " <> show (elapsedMs now)
                  <> " ms; phase " <> current <> "; child " <> maybe "running" show exit))
          `finally` maybe (pure ()) hClose output
      elapsedMs now = (now - startedAt) `div` 1000000
  bracket (createProcess config) cleanup $ \(_, output, _, processHandle) -> do
    let diagnosis phase = do
          now <- getMonotonicTimeNSec
          state <- getProcessExitCode processHandle
          pure ("built web " <> phase <> " in " <> root <> " after " <> show (elapsedMs now)
            <> " ms; child " <> maybe "running" show state)
        failWith context = do
          message <- diagnosis context
          writeIORef phaseRef message
          assertFailure message
    handle <- maybe (failWith "stdout pipe missing") pure output
    putStrLn ("built web launched in " <> root)
    hFlush stdout
    writeIORef phaseRef "awaiting readiness line"
    observed <- trySynchronous (timeout 30000000 (hGetLine handle))
    line <- case observed of
      Left failure -> failWith ("readiness read failed: " <> show failure)
      Right Nothing -> failWith "readiness timed out"
      Right (Just readyLine) -> pure readyLine
    url <- maybe (failWith "returned an invalid readiness line") pure
      (Text.stripPrefix "ADRAI web ready at " (Text.pack line))
    let host = Text.takeWhile (/= '/') (Text.drop (Text.length "http://") url)
        token = Text.drop (Text.length "token=") (snd (Text.breakOn "token=" url))
    if not ("http://" `Text.isPrefixOf` url && not (Text.null host) && not (Text.null token))
      then failWith "returned an invalid bootstrap URL"
      else pure ()
    readyAt <- getMonotonicTimeNSec
    putStrLn ("built web ready in " <> root <> " after " <> show (elapsedMs readyAt) <> " ms")
    hFlush stdout
    writeIORef phaseRef "requesting authenticated repository"
    attempted <- trySynchronous $ requestRawWithStep "built web authenticated repository" host
      (TextEncoding.encodeUtf8 ("GET /api/v1/repository HTTP/1.1\r\nHost: " <> host <> "\r\nAuthorization: Bearer " <> token <> "\r\nConnection: close\r\n\r\n"))
    response <- either (\failure -> failWith ("authenticated request failed: " <> show failure)) pure attempted
    if "HTTP/1.1 200" `BS.isPrefixOf` response
      then writeIORef phaseRef "authenticated repository returned HTTP 200"
      else failWith "authenticated request returned non-200"

testEventRuntime :: IO ()
testEventRuntime = withSeededServer $ \root running -> do
  let authority = runningAuthority running
      token = bootstrapToken running
      headers =
        [ ("Origin", TextEncoding.encodeUtf8 (Security.authorityOrigin authority)),
          ("Authorization", TextEncoding.encodeUtf8 ("Bearer " <> token))
        ]
      authenticate = Aeson.encode (Aeson.object ["type" Aeson..= ("authenticate" :: Text), "credential" Aeson..= token])
      -- Subscription may use two bounded snapshots before the five-second initial send.
      initialResyncGuardMicros = 10000000
      awaitConfigWithoutRelevant label connection = loop (5 :: Int)
        where
          loop remaining
            | remaining <= 0 = assertFailure (label <> ": no configuration-only event arrived within five bounded attempts")
            | otherwise = do
                BS.writeFile (root </> "seed.txt") (BS8.pack (label <> show remaining))
                BS.appendFile (root </> ".adrai.toml") "\n"
                received <- timeout 3000000 (WS.receiveData connection :: IO LBS.ByteString)
                frame <- maybe (assertFailure (label <> ": event receive timed out")) pure received
                let bytes = LBS.toStrict frame
                if "configuration" `BS.isInfixOf` bytes && not ("relevant-worktree-file" `BS.isInfixOf` bytes)
                  then pure ()
                  else loop (remaining - 1)
  port <- either assertFailure pure (authorityPort (Security.authorityHost authority))
  sessionStarted <- getMonotonicTimeNSec
  sessionPhase <- newIORef ("connecting or awaiting upgrade" :: String)
  sessionMilestones <- newIORef [("connecting or awaiting upgrade", 0 :: Word64)]
  let markSession label = do
        now <- getMonotonicTimeNSec
        writeIORef sessionPhase label
        atomicModifyIORef' sessionMilestones (\events -> ((label, (now - sessionStarted) `div` 1000000) : events, ()))
  session <- runOwnedWebSocketClient 20000000 port headers $ \connection -> do
      markSession "upgrade admitted; sending authentication"
      WS.sendTextData connection authenticate
      markSession "authentication sent; awaiting initial resync"
      initial <- timeout initialResyncGuardMicros (WS.receiveData connection :: IO LBS.ByteString)
      frame <- maybe (assertFailure "authenticated socket did not receive its initial resync") pure initial
      markSession "initial resync received"
      assertInitialFullResync "initial authenticated socket" frame
      WS.sendTextData connection (Aeson.encode (Aeson.object ["type" Aeson..= ("active-files" :: Text), "paths" Aeson..= (["seed.txt"] :: [Text])]))
      leaseAdded <- timeout 3000000 (WS.receiveData connection :: IO LBS.ByteString)
      leaseAddedFrame <- maybe (assertFailure "active-files addition did not publish its interest transition") pure leaseAdded
      assertBool "active-files addition publishes a relevant-interest transition" ("relevant-worktree-file" `BS.isInfixOf` LBS.toStrict leaseAddedFrame)
      BS.writeFile (root </> "seed.txt") "leased relevant change"
      leased <- timeout 3000000 (WS.receiveData connection :: IO LBS.ByteString)
      leasedFrame <- maybe (assertFailure "leased relevant-file change was not delivered") pure leased
      assertBool "active-files lease adds relevant-file invalidation" ("relevant-worktree-file" `BS.isInfixOf` LBS.toStrict leasedFrame)
      WS.sendTextData connection (Aeson.encode (Aeson.object ["type" Aeson..= ("active-files" :: Text), "paths" Aeson..= ([] :: [Text])]))
      leaseRemoved <- timeout 3000000 (WS.receiveData connection :: IO LBS.ByteString)
      leaseRemovedFrame <- maybe (assertFailure "active-files removal did not publish its interest transition") pure leaseRemoved
      assertBool "active-files removal publishes a relevant-interest transition" ("relevant-worktree-file" `BS.isInfixOf` LBS.toStrict leaseRemovedFrame)
      awaitConfigWithoutRelevant "replace-set removes the old relevant lease" connection
      WS.sendTextData connection authenticate
      markSession "repeated authentication sent; awaiting invalid-control close"
      closed <- timeout 2000000 (trySynchronous (WS.receiveDataMessage connection))
      assertBool "repeated authentication closes with the invalid-control rejection" (expectedClose "invalid or idle control stream" closed)
  sessionEnded <- getMonotonicTimeNSec
  initialSessionPhase <- readIORef sessionPhase
  milestones <- reverse <$> readIORef sessionMilestones
  let sessionEvidence = "phase=" <> initialSessionPhase
        <> "; elapsed-ms=" <> show ((sessionEnded - sessionStarted) `div` 1000000)
        <> "; milestones(ms)=" <> show milestones
        <> "; outcome=" <> describeWebSocketClientOutcome session
        <> "; owned-client cleanup completed"
  putStrLn ("p7-03-events: initial session " <> sessionEvidence)
  assertBool ("authenticated websocket session terminates within its owner bound: " <> sessionEvidence) (maybe False (either (const False) (const True)) session)
  rejected <- runOwnedWebSocketClient 5000000 port
    [ ("Origin", "http://example.invalid"),
      ("Authorization", TextEncoding.encodeUtf8 ("Bearer " <> token))
    ]
    (const (pure ()))
  assertBool ("foreign-origin websocket handshake did not report HTTP 403: " <> show rejected) (expectedHandshakeStatus 403 rejected)
  cookie <- sessionCookiePair running
  let (cookieName, _) = Text.breakOn "=" cookie
  ambiguous <- runOwnedWebSocketClient 5000000 port
    (headers <> [("Cookie", TextEncoding.encodeUtf8 (cookieName <> "=wrong"))])
    (const (pure ()))
  assertBool ("credential-conflict websocket handshake did not report HTTP 401: " <> show ambiguous) (expectedHandshakeStatus 401 ambiguous)
  wrongFrame <- runOwnedWebSocketClient 5000000 port headers $ \connection -> do
      WS.sendTextData connection (Aeson.encode (Aeson.object ["type" Aeson..= ("authenticate" :: Text), "credential" Aeson..= ("wrong" :: Text)]))
      result <- timeout 2000000 (trySynchronous (WS.receiveDataMessage connection))
      assertBool "wrong websocket frame credential receives the authentication-rejected close" (expectedClose "authentication rejected" result)
  assertBool "wrong websocket credential session remains bounded" (maybe False (either (const False) (const True)) wrongFrame)
  binaryPhase <- newIORef ("connecting or awaiting upgrade" :: String)
  binaryFrame <- runOwnedWebSocketClient 15000000 port headers $ \connection -> do
      writeIORef binaryPhase "sending authentication"
      WS.sendTextData connection authenticate
      writeIORef binaryPhase "awaiting initial resync"
      initial <- timeout initialResyncGuardMicros (trySynchronous (WS.receiveData connection :: IO LBS.ByteString))
      case initial of
        Nothing -> do
          writeIORef binaryPhase "initial resync receive timed out after ten seconds"
          assertFailure "binary-control session did not receive its initial resync within the bounded subscription and send window"
        Just (Left failure) -> do
          let detail = "initial resync receive failed: " <> describeWebSocketClientFailure failure
          writeIORef binaryPhase detail
          assertFailure detail
        Just (Right frame) -> do
          writeIORef binaryPhase "checking full initial resync"
          assertInitialFullResync "binary-control session" frame
      writeIORef binaryPhase "sending binary control"
      WS.sendBinaryData connection ("binary-control" :: BS.ByteString)
      writeIORef binaryPhase "awaiting invalid-control close"
      result <- timeout 2000000 (trySynchronous (WS.receiveDataMessage connection))
      assertBool "binary post-auth control receives the invalid-control close" (expectedClose "invalid or idle control stream" result)
  binaryStage <- readIORef binaryPhase
  assertBool ("binary websocket control session remains bounded while " <> binaryStage <> ": " <> describeWebSocketClientOutcome binaryFrame)
    (maybe False (either (const False) (const True)) binaryFrame)
  putStrLn "p7-03-events: negative sessions complete"
  reconnectPhase <- newIORef ("connecting or awaiting upgrade" :: String)
  reconnect <- runOwnedWebSocketClient 20000000 port headers $ \connection -> do
      putStrLn "p7-03-events: reconnect admitted"
      writeIORef reconnectPhase "sending authentication"
      WS.sendTextData connection authenticate
      writeIORef reconnectPhase "awaiting initial resync"
      received <- timeout initialResyncGuardMicros (trySynchronous (WS.receiveData connection :: IO LBS.ByteString))
      case received of
        Nothing -> do
          writeIORef reconnectPhase "initial resync receive timed out after ten seconds"
          assertFailure "a reconnect did not receive its fresh initial resync within the bounded subscription and send window"
        Just (Left failure) -> do
          let detail = "initial resync receive failed: " <> describeWebSocketClientFailure failure
          writeIORef reconnectPhase detail
          assertFailure detail
        Just (Right frame) -> do
          writeIORef reconnectPhase "checking full initial resync"
          assertInitialFullResync "reconnect after invalid-session cleanup" frame
      putStrLn "p7-03-events: reconnect initial received"
      writeIORef reconnectPhase "sending close"
      WS.sendClose connection ("test complete" :: Text)
      putStrLn "p7-03-events: reconnect close sent"
  reconnectStage <- readIORef reconnectPhase
  assertBool ("reconnect completed after invalid-session cleanup while " <> reconnectStage <> ": " <> describeWebSocketClientOutcome reconnect)
    (maybe False (either (const False) (const True)) reconnect)
  putStrLn "p7-03-events: reconnect cleanup complete"
  nonGet <- requestPrefix (Security.authorityHost authority)
    ("POST /api/v1/events HTTP/1.1\r\nHost: " <> Security.authorityHost authority
      <> "\r\nOrigin: " <> Security.authorityOrigin authority
      <> "\r\nAuthorization: Bearer " <> token
      <> "\r\nConnection: Upgrade, close\r\nUpgrade: websocket\r\nSec-WebSocket-Version: 13\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nContent-Length: 0\r\n\r\n")
  assertBool "a non-GET upgrade stays on the admitted HTTP error path" (not ("HTTP/1.1 101" `BS.isPrefixOf` nonGet) && "X-Adrai-Generation:" `BS.isInfixOf` nonGet)
  fragmentedOversizeRejected authority token
  putStrLn "p7-03-events: fragmented bound complete"
  withRawPendingPeers 16 authority token $ \peers -> do
    pendingStarted <- getMonotonicTimeNSec
    readers <- mapM (async . receiveUntilEof) peers
    (`finally` do
        mapM_ closeOwnedSocket peers
        mapM_ waitCatch readers) $ do
      mapM poll readers >>= assertBool "sixteen silent upgraded peers remain connected before the auth deadline" . all (\case Nothing -> True; Just _ -> False)
      seventeenth <- runOwnedWebSocketClient 1500000 port headers (const (pure ()))
      assertBool "the seventeenth pre-auth client is rejected with HTTP 400" (expectedHandshakeStatus 400 seventeenth)
      ended <- timeout 7000000 (mapM waitCatch readers)
      pendingEnded <- getMonotonicTimeNSec
      putStrLn ("p7-03-events: silent peer EOF results=" <> show ended <> "; elapsed-ms=" <> show ((pendingEnded - pendingStarted) `div` 1000000))
      assertBool ("all sixteen silent raw peers receive server EOF before the client guard closes them: " <> show ended)
        (maybe False (all (either (const False) id)) ended)
      capacityReleaseStarted <- getMonotonicTimeNSec
      let describeFreshOutcome = \case
            Nothing -> "owned client timed out"
            Just (Right ()) -> "authenticated resync and close completed"
            Just (Left failure) -> case fromException failure of
              Just (WS.RequestRejected _ response) -> "HTTP " <> show (WS.responseCode response) <> " rejected upgrade"
              Just (WS.MalformedResponse response _) -> "HTTP " <> show (WS.responseCode response) <> " malformed upgrade response"
              _ -> "non-handshake client exception"
          awaitReleasedCapacity attempt = do
            phase <- newIORef ("connecting or awaiting upgrade" :: String)
            attemptStarted <- getMonotonicTimeNSec
            recovered <- runOwnedWebSocketClient 12000000 port headers $ \connection -> do
              writeIORef phase "sending authentication"
              WS.sendTextData connection authenticate
              writeIORef phase "awaiting initial resync"
              received <- timeout 10000000 (WS.receiveData connection :: IO LBS.ByteString)
              initialFrame <- maybe (assertFailure "new authenticated client did not receive its initial resync within its bounded server subscription and send window") pure received
              assertInitialFullResync "authenticated client after pending-capacity release" initialFrame
              writeIORef phase "sending close"
              WS.sendClose connection ("cap recovery complete" :: Text)
              writeIORef phase "closed"
            attemptEnded <- getMonotonicTimeNSec
            observedPhase <- readIORef phase
            putStrLn ("p7-03-events: fresh attempt=" <> show attempt <> "; elapsed-ms=" <> show ((attemptEnded - attemptStarted) `div` 1000000) <> "; phase=" <> observedPhase <> "; outcome=" <> describeFreshOutcome recovered)
            case recovered of
              Just (Right ()) -> pure ()
              failure | expectedHandshakeStatus 400 failure -> do
                now <- getMonotonicTimeNSec
                assertBool ("pending capacity remained occupied more than five seconds after all silent peers received EOF; attempt=" <> show attempt)
                  (now - capacityReleaseStarted < 5000000000)
                threadDelay 20000
                awaitReleasedCapacity (attempt + 1)
              _ -> assertFailure ("fresh WebSocket failed for a reason other than bounded HTTP 400 admission; attempt=" <> show attempt <> "; phase=" <> observedPhase <> "; outcome=" <> describeFreshOutcome recovered)
      awaitReleasedCapacity (1 :: Int)
  putStrLn "p7-03-events: pending cap complete"
  Events.decodeClientFrame 4096 "{\"type\":\"active-files\",\"type\":\"active-files\",\"paths\":[]}" @?= Left Events.MalformedFrame
  coordinator <- Events.newEventCoordinator
  subscriber <- Events.registerSubscriberWithInitial coordinator (Events.EventAsOfUnavailable "test") >>= either (assertFailure . Text.unpack) pure
  forM_ [1 :: Int .. 64] $ \_ -> do
    _ <- Events.publishInvalidation coordinator (Events.EventAsOfUnavailable "test") [Events.HeadChanged]
    pure ()
  _ <- Events.publishInvalidation coordinator (Events.EventAsOfUnavailable "test") [Events.IndexChanged]
  Events.readSubscriberEvent subscriber >>= \case
    Events.SubscriberOverflow -> pure ()
    Events.SubscriberEvent _ -> assertFailure "overflowed subscriber delivered a narrower event instead of closing"
    Events.SubscriberGenerationExhausted -> assertFailure "overflowed subscriber unexpectedly exhausted its generation"
  Events.unregisterSubscriber coordinator subscriber
  bracket (socket AF_INET Stream defaultProtocol) closeOwnedSocket $ \blockedPeer -> do
    stopped <- withWebServer dependencies root (Api.WebOptions Nothing False) $ \second _ ->
      admitRawPendingPeer blockedPeer (runningAuthority second) (bootstrapToken second)
    either (assertFailure . Text.unpack) pure stopped
    reader <- async (receiveUntilEof blockedPeer)
    observed <- timeout 3000000 (waitCatch reader)
    case observed of
      Just (Right True) -> pure ()
      other -> do
        closeOwnedSocket blockedPeer
        _ <- waitCatch reader
        assertFailure ("server shutdown did not physically end its blocked unauthenticated peer: " <> show other)
  verifySlowNetworkSubscriber root

verifySlowNetworkSubscriber :: FilePath -> IO ()
verifySlowNetworkSubscriber root = do
  coordinatorReady <- newEmptyMVar
  sendDeadline <- newEmptyMVar
  let liveDependencies =
        dependencies
          { serverEventCoordinatorReady = putMVar coordinatorReady,
            serverEventSendDeadline = void (tryPutMVar sendDeadline ())
          }
  started <- withWebServer liveDependencies root (Api.WebOptions Nothing False) $ \running _ -> do
    coordinator <- takeMVar coordinatorReady
    let authority = runningAuthority running
        token = bootstrapToken running
    bracket (openRawSlowSubscriber authority token) closeOwnedSocket $ \slowPeer -> do
      let largeButBoundedAsOf = Events.EventAsOfUnavailable (Text.replicate (64 * 1024) "s")
      publisher <- async $ forM_ [1 :: Int .. 48] $ \_ ->
        void (Events.publishInvalidation coordinator largeButBoundedAsOf [Events.HeadChanged])
      published <- timeout 3000000 (waitCatch publisher)
      case published of
        Just (Right ()) -> pure ()
        other -> do
          closeOwnedSocket slowPeer
          _ <- waitCatch publisher
          assertFailure ("a physically nonreading subscriber blocked live event publication: " <> show other)
      -- Fewer than the 64 queue slots are published. Keep the tiny-window peer
      -- unread until the live sender's deadline abort has actually completed.
      observedDeadline <- timeout 7000000 (takeMVar sendDeadline)
      assertBool "the unread live sender reached its physical send deadline" (maybe False (const True) observedDeadline)
      putStrLn "p7-03-slow-peer: physical send deadline observed"
      port <- either assertFailure pure (authorityPort (Security.authorityHost authority))
      let headers =
            [ ("Origin", TextEncoding.encodeUtf8 (Security.authorityOrigin authority)),
              ("Authorization", TextEncoding.encodeUtf8 ("Bearer " <> token))
            ]
      freshPhase <- newIORef ("connecting or awaiting upgrade" :: String)
      freshStarted <- getMonotonicTimeNSec
      let markFresh phase = do
            writeIORef freshPhase phase
            now <- getMonotonicTimeNSec
            putStrLn ("p7-03-slow-peer: fresh phase=" <> phase <> "; elapsed-ms=" <> show ((now - freshStarted) `div` 1000000))
      recovered <- runOwnedWebSocketClient 12000000 port headers $ \connection -> do
        markFresh "sending authentication"
        WS.sendTextData connection (Aeson.encode (Aeson.object ["type" Aeson..= ("authenticate" :: Text), "credential" Aeson..= token]))
        markFresh "awaiting initial resync"
        received <- timeout 10000000 (WS.receiveData connection :: IO LBS.ByteString)
        initial <- maybe (assertFailure "the fresh authenticated subscriber did not receive its initial resync within the bounded snapshot and send window") pure received
        markFresh "checking initial resync"
        assertInitialFullResync "fresh subscriber after slow peer closes" initial
        markFresh "sending close"
        WS.sendClose connection ("slow-peer recovery complete" :: Text)
        markFresh "close sent"
      freshEnded <- getMonotonicTimeNSec
      observedFreshPhase <- readIORef freshPhase
      let redactedOutcome = Text.unpack (Text.replace token "<redacted>" (Text.pack (show recovered)))
      putStrLn ("p7-03-slow-peer: fresh worker joined; elapsed-ms=" <> show ((freshEnded - freshStarted) `div` 1000000) <> "; phase=" <> observedFreshPhase <> "; outcome=" <> redactedOutcome)
      assertBool ("a new authenticated subscriber remains serviceable after the slow peer closes; phase=" <> observedFreshPhase <> "; outcome=" <> redactedOutcome) (maybe False (either (const False) (const True)) recovered)
      reader <- async (receiveUntilEof slowPeer)
      ended <- timeout 2000000 (waitCatch reader)
      putStrLn ("p7-03-slow-peer: physical EOF outcome=" <> show ended)
      case ended of
        Just (Right True) -> pure ()
        other -> do
          closeOwnedSocket slowPeer
          _ <- waitCatch reader
          assertFailure ("the unread live subscriber was not physically closed after its send deadline: " <> show other)
  either (assertFailure . Text.unpack) pure started

-- Windows junctions and Linux symlinks exercise the same redirected-component
-- rejection without changing the production observer's read policy.
createObservationRedirect :: FilePath -> FilePath -> IO ()
createObservationRedirect target link
  | os == "mingw32" = do
      (code, _, diagnostic) <- readCreateProcessWithExitCode (shell ("mklink /J \"" <> link <> "\" \"" <> target <> "\"")) ""
      assertBool ("observation junction creation failed: " <> diagnostic) (code == ExitSuccess)
  | otherwise = createDirectoryLink target link

testWatchRuntime :: IO ()
testWatchRuntime = withSeededRepository $ \root -> do
  repository <- discoverRepository systemGit root >>= either (assertFailure . show) pure
  bound <- either (assertFailure . show) pure (Api.validateRepositoryBinding (Right repository))
  registry <- Watch.newActiveFileRegistry
  client <- Watch.registerActiveClient registry >>= either (assertFailure . Text.unpack) pure
  path <- either (assertFailure . show) pure (Types.mkRepoPath "seed.txt")
  Watch.replaceActiveFiles registry client [path] >>= either (assertFailure . Text.unpack) pure
  unionClient <- Watch.registerActiveClient registry >>= either (assertFailure . Text.unpack) pure
  unionPath <- either (assertFailure . show) pure (Types.mkRepoPath "union.txt")
  Watch.replaceActiveFiles registry unionClient [unionPath] >>= either (assertFailure . Text.unpack) pure
  Watch.activeFileUnion registry >>= (@?= [path, unionPath])
  Watch.unregisterActiveClient registry unionClient
  Watch.activeFileUnion registry >>= (@?= [path])
  createDirectoryIfMissing True (root </> "architecture" </> "adrai")
  BS.writeFile (root </> "architecture" </> "adrai" </> "native-owned.txt") "owned managed bytes"
  pauseAncestor <- newIORef False
  lastHandleComponent <- newIORef (Nothing :: Maybe String)
  ancestorOpened <- newEmptyMVar
  resumeAncestor <- newEmptyMVar
  nativeSwapTracing <- newIORef False
  nativeSwapPhases <- newIORef ([] :: [(Word64, String)])
  let recordNativeSwapPhase phase = do
        tracing <- readIORef nativeSwapTracing
        if tracing then do
          at <- getMonotonicTimeNSec
          atomicModifyIORef' nativeSwapPhases (\phases -> ((at, phase) : phases, ()))
        else pure ()
  let afterHandleOpen component = do
        writeIORef lastHandleComponent (Just (show component))
        recordNativeSwapPhase ("opened component " <> show component)
        pause <- atomicModifyIORef' pauseAncestor $ \enabled ->
          let fire = enabled && component == "architecture"
           in (enabled && not fire, fire)
        if pause then do
          recordNativeSwapPhase "verified ancestor handle opened"
          putMVar ancestorOpened ()
          recordNativeSwapPhase "readiness signalled"
          takeMVar resumeAncestor
          recordNativeSwapPhase "ancestor handle released"
        else pure ()
  observer <- Watch.observerForRegistryWithHandleHook registry bound afterHandleOpen
  let expectFactChange label invalidation action = do
        left <- Watch.repositorySnapshot observer bound
        _ <- action
        right <- Watch.repositorySnapshot observer bound
        case (left, right) of
          (Watch.RepositorySnapshot _ leftFacts, Watch.RepositorySnapshot _ rightFacts) ->
            assertBool label (invalidation `elem` Watch.diffRepositoryFacts leftFacts rightFacts)
          _ -> assertFailure (label <> ": observation failed")
      gitDirectory = Api.repoGitDirectory bound
      commonDirectory = Api.repoCommonDirectory bound
  callProcess "git" ["-C", root, "branch", "watch-same-oid"]
  expectFactChange "same-OID attached branch switches change repository identity" Events.RepositoryIdentityChanged
    (callProcess "git" ["-C", root, "checkout", "watch-same-oid"])
  expectFactChange "attached-to-detached transition changes repository identity" Events.RepositoryIdentityChanged
    (callProcess "git" ["-C", root, "checkout", "--detach", "HEAD"])
  callProcess "git" ["-C", root, "checkout", "main"]
  expectFactChange "index content changes are fingerprinted" Events.IndexChanged $ do
    BS.writeFile (root </> "seed.txt") "staged watcher bytes"
    callProcess "git" ["-C", root, "add", "--", "seed.txt"]
  callProcess "git" ["-C", root, "reset", "--mixed", "HEAD"]
  expectFactChange "sequencer markers are observed" Events.SequencerChanged
    (BS.writeFile (gitDirectory </> "MERGE_HEAD") "0000000000000000000000000000000000000000\n")
  removeFile (gitDirectory </> "MERGE_HEAD")
  let configPath = root </> ".adrai.toml"
  configPresent <- doesFileExist configPath
  originalConfig <- if configPresent then BS.readFile configPath else pure (TextEncoding.encodeUtf8 defaultConfigText)
  expectFactChange "configuration bytes are fingerprinted" Events.ConfigurationChanged
    (BS.writeFile configPath (originalConfig <> "\n"))
  if configPresent then BS.writeFile configPath originalConfig else removeFile configPath
  let alternateConfig =
        Text.replace "architecture/adrai/connections" "watch-alternate/connections"
          (Text.replace "architecture/adrai/decisions" "watch-alternate/decisions" (TextEncoding.decodeUtf8 originalConfig))
      alternateDecision = root </> "watch-alternate" </> "decisions" </> "reroot.md"
  createDirectoryIfMissing True (root </> "watch-alternate" </> "decisions")
  createDirectoryIfMissing True (root </> "watch-alternate" </> "connections")
  BS.writeFile alternateDecision "alternate managed fact"
  BS.writeFile configPath (TextEncoding.encodeUtf8 alternateConfig)
  rerooted <- Watch.repositorySnapshot observer bound
  BS.writeFile alternateDecision "alternate managed fact changed"
  rerootedChanged <- Watch.repositorySnapshot observer bound
  case (rerooted, rerootedChanged) of
    (Watch.RepositorySnapshot _ leftFacts, Watch.RepositorySnapshot _ rightFacts) ->
      assertBool "configuration changes refresh the managed observation roots" (Events.ManagedSourceChanged `elem` Watch.diffRepositoryFacts leftFacts rightFacts)
    _ -> assertFailure "rerooted managed observation failed"
  if configPresent then BS.writeFile configPath originalConfig else removeFile configPath
  let managedDecision = root </> "architecture" </> "adrai" </> "decisions" </> "watch.md"
  createDirectoryIfMissing True (root </> "architecture" </> "adrai" </> "decisions")
  expectFactChange "managed source bytes are fingerprinted" Events.ManagedSourceChanged
    (BS.writeFile managedDecision "managed fact")
  expectFactChange "missing managed paths remain observable" Events.ManagedSourceChanged (removeFile managedDecision)
  expectFactChange "recreated managed paths recover without restart" Events.ManagedSourceChanged
    (BS.writeFile managedDecision "managed fact restored")
  headForFacts <- gitHead root
  let looseFactRef = commonDirectory </> "refs" </> "heads" </> "watch-raw-fact"
  expectFactChange "common loose references are fingerprinted" Events.CommonReferencesChanged
    (BS.writeFile looseFactRef (TextEncoding.encodeUtf8 (headForFacts <> "\n")))
  removeFile looseFactRef
  let packedRefsPath = commonDirectory </> "packed-refs"
  packedPresent <- doesFileExist packedRefsPath
  packedBefore <- if packedPresent then BS.readFile packedRefsPath else pure BS.empty
  expectFactChange "packed references are fingerprinted" Events.PackedReferencesChanged
    (BS.writeFile packedRefsPath ("# pack-refs with: peeled fully-peeled sorted \n" <> TextEncoding.encodeUtf8 headForFacts <> " refs/heads/watch-packed-fact\n"))
  if packedPresent then BS.writeFile packedRefsPath packedBefore else removeFile packedRefsPath
  let headLog = gitDirectory </> "logs" </> "HEAD"
  reflogBefore <- BS.readFile headLog
  expectFactChange "reflog bytes are fingerprinted" Events.ReflogsChanged (BS.writeFile headLog (reflogBefore <> "\n"))
  BS.writeFile headLog reflogBefore
  let worktreeMetadata = gitDirectory </> "watch-metadata"
  expectFactChange "worktree Git metadata is fingerprinted" Events.WorktreeMetadataChanged
    (BS.writeFile worktreeMetadata "worktree metadata")
  removeFile worktreeMetadata
  before <- Watch.repositorySnapshot observer bound
  if os == "mingw32" then do
    assertBool "the native scanner accepts the largest even UTF-16 byte length" (Watch.nativeNameLengthAcceptedForTest (replicate 32767 'a'))
    assertBool "the native scanner rejects a wrapping UTF-16 byte length" (not (Watch.nativeNameLengthAcceptedForTest (replicate 32768 'a')))
    assertBool "the native scanner counts surrogate pairs as two UTF-16 code units" (not (Watch.nativeNameLengthAcceptedForTest (replicate 16384 '\x1f600')))
  else do
    assertBool "the Linux scanner preserves non-ASCII component spelling" (Watch.nativeNameLengthAcceptedForTest "répertoire-Δ")
    assertBool "the Linux scanner refuses NUL in components" (not (Watch.nativeNameLengthAcceptedForTest "bad\0name"))
  BS.writeFile (root </> "seed.txt") "changed relevant bytes"
  after <- Watch.repositorySnapshot observer bound
  case (before, after) of
    (Watch.RepositorySnapshot _ leftFacts, Watch.RepositorySnapshot _ rightFacts) ->
      assertBool "active relevant-file content is fingerprinted" (Events.RelevantWorktreeFileChanged `elem` Watch.diffRepositoryFacts leftFacts rightFacts)
    _ -> assertFailure "watch snapshots unexpectedly failed"
  createDirectoryIfMissing True (root </> ".adrai")
  cacheBefore <- Watch.repositorySnapshot observer bound
  BS.writeFile (root </> ".adrai" </> "watch-noise.sqlite-wal") "cache noise"
  cacheAfter <- Watch.repositorySnapshot observer bound
  cacheAfter @?= cacheBefore
  epoch <- case cacheAfter of
    Watch.RepositorySnapshot observedEpoch _ -> pure observedEpoch
    Watch.RepositorySnapshotFailed _ failure -> assertFailure (show failure)
  secondPath <- either (assertFailure . show) pure (Types.mkRepoPath "other.txt")
  Watch.replaceActiveFiles registry client [secondPath] >>= either (assertFailure . Text.unpack) pure
  coordinator <- Events.newEventCoordinator
  stale <- Events.publishInvalidationWhen (Watch.observationEpochMatches registry epoch) coordinator (Events.EventAsOfUnavailable "test") [Events.RelevantWorktreeFileChanged]
  stale @?= Nothing
  current <- Watch.repositorySnapshot observer bound
  currentEpoch <- case current of
    Watch.RepositorySnapshot observedEpoch _ -> pure observedEpoch
    Watch.RepositorySnapshotFailed _ failure -> assertFailure (show failure)
  fresh <- Events.publishInvalidationWhen (Watch.observationEpochMatches registry currentEpoch) coordinator (Events.EventAsOfUnavailable "test") [Events.RelevantWorktreeFileChanged]
  assertBool "current scan epoch and generation enqueue commit atomically" (maybe False (const True) fresh)
  BS.writeFile (root </> ".adrai.toml") (BS.replicate (16 * 1024 * 1024 + 1) 120)
  bounded <- Watch.repositorySnapshot observer bound
  case bounded of
    Watch.RepositorySnapshotFailed _ _ -> pure ()
    Watch.RepositorySnapshot _ _ -> assertFailure "oversized config escaped the global observation byte budget"
  removeFile (root </> ".adrai.toml")
  let entryBudgetDirectory = root </> "architecture" </> "adrai" </> "decisions" </> "entry-budget"
  createDirectoryIfMissing True entryBudgetDirectory
  forM_ [1 :: Int .. 4100] $ \index -> BS.writeFile (entryBudgetDirectory </> ("entry-" <> show index)) "x"
  entryBounded <- Watch.repositorySnapshot observer bound
  case entryBounded of
    Watch.RepositorySnapshotFailed _ _ -> pure ()
    Watch.RepositorySnapshot _ _ -> assertFailure "aggregate directory entries escaped the global 4096-entry observation budget"
  removeDirectoryRecursive entryBudgetDirectory
  Watch.replaceActiveFiles registry client [path] >>= either (assertFailure . Text.unpack) pure
  retryStarted <- getMonotonicTimeNSec
  retryPhases <- newIORef []
  let markRetryPhase phase = do
        now <- getMonotonicTimeNSec
        atomicModifyIORef' retryPhases (\entries -> ((now - retryStarted, phase) : entries, ()))
  attempts <- newIORef (0 :: Int)
  scanArmed <- newIORef False
  scanOpenings <- newIORef (0 :: Int)
  firstScanOpened <- newEmptyMVar
  releaseFirstScan <- newEmptyMVar
  retryScanOpened <- newEmptyMVar
  retryObserver <- Watch.observerForRegistryWithHandleHook registry bound $ \component ->
    when (component == "seed.txt") $ do
      armed <- readIORef scanArmed
      when armed $ do
        opening <- atomicModifyIORef' scanOpenings (\count -> let next = count + 1 in (next, next))
        markRetryPhase "scan opened relevant file"
        if opening == 1
          then do
            _ <- tryPutMVar firstScanOpened ()
            -- The native handle shares reads/writes; hold it before reading
            -- while the parent writes the final coalesced bytes.
            _ <- readMVar releaseFirstScan
            pure ()
          else do
            count <- readIORef attempts
            when (count >= 1) (void (tryPutMVar retryScanOpened ()))
  firstFailure <- newEmptyMVar
  delivered <- newEmptyMVar
  let publishRetry event = do
        attempt <- atomicModifyIORef' attempts (\value -> let next = value + 1 in (next, next))
        markRetryPhase ("publication attempt " <> show attempt <> " entered")
        if attempt == 1
          then (ioError (userError "simulated busy publication") `onException` do
                  markRetryPhase "first publication failed synchronously"
                  void (tryPutMVar firstFailure ()))
          else do
            _ <- tryPutMVar delivered event
            markRetryPhase "later publication delivered"
      startRetryWatcher = do
        markRetryPhase "starting watcher initial snapshot"
        watcher <- Watch.watchRepository retryObserver bound publishRetry
        markRetryPhase "watcher initial snapshot completed"
        writeIORef scanArmed True
        pure watcher
      stopRetryWatcher watcher = do
        _ <- tryPutMVar releaseFirstScan ()
        Watch.stopWatching watcher `finally` Watch.awaitWatcher watcher
        markRetryPhase "watcher stopped"
        retryPhaseLog <- reverse <$> readIORef retryPhases
        putStrLn ("p7-03-watch: unacknowledged retry phase trace (ms) " <>
          show [((at `div` 1000000), phase) | (at, phase) <- retryPhaseLog])
  bracket startRetryWatcher stopRetryWatcher $ \watcher ->
    withAsync (Watch.awaitWatcher watcher) $ \finished -> do
      let awaitRetrySignal label signal = do
            observed <- race (takeMVar signal) (wait finished)
            case observed of
              Left value -> pure value
              Right () -> assertFailure ("watcher terminated before " <> label)
      _ <- awaitRetrySignal "post-baseline relevant-file scan" firstScanOpened
      BS.writeFile (root </> "seed.txt") "coalesced change one"
      markRetryPhase "first file write completed"
      BS.writeFile (root </> "seed.txt") "coalesced change two"
      markRetryPhase "second file write completed"
      BS.writeFile (root </> "seed.txt") "coalesced final bytes"
      markRetryPhase "final file write completed"
      _ <- tryPutMVar releaseFirstScan ()
      _ <- awaitRetrySignal "first synchronous publication failure" firstFailure
      markRetryPhase "first failure observed"
      _ <- awaitRetrySignal "retry scan opening the relevant file" retryScanOpened
      markRetryPhase "retry scan opened relevant file"
      retried <- awaitRetrySignal "retried publication delivery" delivered
      markRetryPhase "delivery received"
      count <- readIORef attempts
      assertBool "publication was attempted again without another filesystem change" (count >= 2)
      case retried of
        Watch.RepositoryFactsChanged _ eventSnapshot _ -> do
          finalSnapshot <- Watch.repositorySnapshot observer bound
          case (eventSnapshot, finalSnapshot) of
            (Watch.RepositorySnapshot _ eventFacts, Watch.RepositorySnapshot _ finalFacts) ->
              Watch.factsRelevantWorktreeIdentity eventFacts @?= Watch.factsRelevantWorktreeIdentity finalFacts
            _ -> assertFailure "coalesced-hint snapshots failed"
        Watch.RepositoryObservationFailure _ failure -> assertFailure ("coalesced hints ended in observation failure: " <> show failure)
  terminalStarted <- getMonotonicTimeNSec
  terminalPhases <- newIORef []
  let markTerminalPhase phase = do
        now <- getMonotonicTimeNSec
        atomicModifyIORef' terminalPhases (\entries -> ((now - terminalStarted, phase) : entries, ()))
  terminalScanArmed <- newIORef False
  terminalScanOpened <- newEmptyMVar
  terminalObserver <- Watch.observerForRegistryWithHandleHook registry bound $ \component ->
    when (component == "seed.txt") $ do
      armed <- readIORef terminalScanArmed
      when armed $ do
        markTerminalPhase "scan opened relevant file"
        void (tryPutMVar terminalScanOpened ())
  terminalAttempts <- newIORef (0 :: Int)
  terminalPublished <- newEmptyMVar
  markTerminalPhase "starting watcher initial snapshot"
  terminalWatcher <- Watch.watchRepository terminalObserver bound $ \event -> do
    attempt <- atomicModifyIORef' terminalAttempts (\value -> let next = value + 1 in (next, next))
    markTerminalPhase ("publication attempt " <> show attempt <> " entered")
    throwIO Events.GenerationExhausted `onException` do
      markTerminalPhase "typed terminal publication failure thrown"
      void (tryPutMVar terminalPublished event)
  markTerminalPhase "watcher initial snapshot completed"
  (`finally` do
      Watch.stopWatching terminalWatcher
      Watch.awaitWatcher terminalWatcher) $ do
    writeIORef terminalScanArmed True
    BS.writeFile (root </> "seed.txt") "terminal generation changes the active relevant fact"
    markTerminalPhase "terminal file write completed"
    terminalScan <- timeout 6000000 (takeMVar terminalScanOpened)
    markTerminalPhase (if maybe False (const True) terminalScan then "terminal scan opened relevant file" else "terminal scan deadline elapsed")
    terminalEvent <- case terminalScan of
      Just () -> timeout 3000000 (takeMVar terminalPublished)
      Nothing -> pure Nothing
    markTerminalPhase (if maybe False (const True) terminalEvent then "typed terminal callback observed" else "terminal callback deadline elapsed")
    terminalStop <- case terminalEvent of
      Just _ -> timeout 3000000 (Watch.awaitWatcher terminalWatcher)
      Nothing -> pure Nothing
    markTerminalPhase (if maybe False (const True) terminalStop then "verifier and native backend joined" else "watcher join deadline elapsed")
    terminalPhaseLog <- reverse <$> readIORef terminalPhases
    terminalAttemptCount <- readIORef terminalAttempts
    putStrLn ("p7-03-watch: terminal phase trace (ms) " <>
      show [((at `div` 1000000), phase) | (at, phase) <- terminalPhaseLog] <>
      "; attempts=" <> show terminalAttemptCount)
    assertBool ("terminal verification scans the changed relevant file; phases=" <> show terminalPhaseLog) (maybe False (const True) terminalScan)
    case terminalEvent of
      Just (Watch.RepositoryFactsChanged _ _ invalidations) ->
        assertBool ("terminal publication carries the relevant fact change; phases=" <> show terminalPhaseLog) (Events.RelevantWorktreeFileChanged `elem` invalidations)
      Just event -> assertFailure ("terminal publication did not carry changed facts: " <> show event <> "; phases=" <> show terminalPhaseLog)
      Nothing -> assertFailure ("terminal publication callback did not enter; phases=" <> show terminalPhaseLog)
    assertBool ("terminal publication ends verifier and native backend without external shutdown; phases=" <> show terminalPhaseLog) (maybe False (const True) terminalStop)
    threadDelay 350000
    readIORef terminalAttempts >>= (@?= 1)
  Watch.unregisterActiveClient registry client
  Watch.activeFileUnion registry >>= (@?= [])
  nativeBaseline <- Watch.repositorySnapshot observer bound
  let ownedArchitecture = root </> "architecture-owned"
      external = root </> "external-managed"
      architecture = root </> "architecture"
  createDirectoryIfMissing True (external </> "adrai")
  BS.writeFile (external </> "adrai" </> "external-sentinel.txt") "must never be observed"
  let controlLink = root </> "junction-control"
  createObservationRedirect external controlLink
  doesFileExist (controlLink </> "adrai" </> "external-sentinel.txt") >>= assertBool "junction control exposes the external sentinel"
  removeDirectoryLink controlLink
  putStrLn "p7-03-watch: junction control complete"
  nativeSwapOrigin <- getMonotonicTimeNSec
  writeIORef nativeSwapTracing True
  writeIORef pauseAncestor True
  recordNativeSwapPhase "pause enabled"
  nativeAfterSwap <- bracket
    (do
        recordNativeSwapPhase "worker launching"
        async $ do
          recordNativeSwapPhase "worker entered snapshot"
          snapshot <- Watch.repositorySnapshot observer bound
          recordNativeSwapPhase "worker returned snapshot"
          pure snapshot)
    (\worker -> do
        recordNativeSwapPhase "cleanup releasing ancestor"
        _ <- tryPutMVar resumeAncestor ()
        stopped <- timeout 2000000 (cancel worker)
        joined <- timeout 2000000 (waitCatch worker)
        assertBool "held native scan cancels within the cleanup bound" (maybe False (const True) stopped)
        assertBool "held native scan joins within the cleanup bound" (maybe False (const True) joined)
        recordNativeSwapPhase "worker joined"
        -- The release is only reusable after the first worker has joined.
        -- Remove an unconsumed token before the cancellation scan uses this latch.
        void (tryTakeMVar resumeAncestor))
    (\worker -> do
      -- Root identity, HEAD, configuration, and index are observed before the
      -- native ancestor opens; bound readiness separately from the held scan.
      opened <- timeout 15000000 (race (takeMVar ancestorOpened) (waitCatch worker))
      case opened of
        Just (Left ()) -> recordNativeSwapPhase "readiness observed"
        Just (Right (Left exception)) ->
          assertFailure ("native scan raised before opening the managed ancestor: " <> show exception)
        Just (Right (Right (Watch.RepositorySnapshot _ _))) ->
          assertFailure "native scan returned before opening the managed ancestor"
        Just (Right (Right (Watch.RepositorySnapshotFailed _ failure))) ->
          assertFailure ("native scan failed before opening the managed ancestor: " <> show failure)
        Nothing -> do
          outcome <- poll worker
          lastComponent <- readIORef lastHandleComponent
          stillPaused <- readIORef pauseAncestor
          phases <- reverse <$> readIORef nativeSwapPhases
          elapsed <- getMonotonicTimeNSec
          let workerState = case outcome of
                Nothing -> "running"
                Just (Left exception) -> "raised " <> show exception
                Just (Right (Watch.RepositorySnapshot _ _)) -> "returned a snapshot"
                Just (Right (Watch.RepositorySnapshotFailed _ failure)) -> "returned an observation failure: " <> show failure
          assertFailure
            ("native scan did not open the managed ancestor within the readiness bound"
              <> "; elapsed_ms=" <> show ((elapsed - nativeSwapOrigin) `div` 1000000)
              <> "; worker=" <> workerState
              <> "; last_opened_component=" <> show lastComponent
              <> "; pause_enabled=" <> show stillPaused
              <> "; phases_ms=" <> show [((at - nativeSwapOrigin) `div` 1000000, phase) | (at, phase) <- phases])
      renameDirectory architecture ownedArchitecture
      let restoreArchitecture = do
            replacement <- doesDirectoryExist architecture
            original <- doesDirectoryExist ownedArchitecture
            if original then do
              if replacement then removeDirectoryLink architecture else pure ()
              renameDirectory ownedArchitecture architecture
            else pure ()
      (`finally` restoreArchitecture) $ do
        createObservationRedirect external architecture
        doesFileExist (architecture </> "adrai" </> "external-sentinel.txt") >>= assertBool "the pathname was replaced by the external junction while the ancestor handle stayed open"
        recordNativeSwapPhase "junction replacement verified"
        putMVar resumeAncestor ()
        recordNativeSwapPhase "ancestor released"
        nativeOutcome <- timeout 3000000 (waitCatch worker)
        case nativeOutcome of
          Nothing -> assertFailure "handle-relative scan did not finish after the swap latch was released"
          Just (Left exception) -> assertFailure ("handle-relative scan raised: " <> show exception)
          Just (Right snapshot) -> pure snapshot)
  writeIORef nativeSwapTracing False
  nativeSwapTrace <- reverse <$> readIORef nativeSwapPhases
  putStrLn ("p7-03-watch: native swap phase trace (ms) "
    <> show [((at - nativeSwapOrigin) `div` 1000000, phase) | (at, phase) <- nativeSwapTrace])
  putStrLn "p7-03-watch: native swap complete"
  baselineIdentity <- managedIdentityOf nativeBaseline
  case nativeAfterSwap of
    Watch.RepositorySnapshotFailed _ _ -> pure ()
    Watch.RepositorySnapshot _ facts -> Watch.factsManagedSourceIdentity facts @?= baselineIdentity
  writeIORef pauseAncestor True
  writeIORef lastHandleComponent Nothing
  bracket
    (async (Watch.repositorySnapshot observer bound))
    (\worker -> do
        _ <- tryPutMVar resumeAncestor ()
        _ <- timeout 2000000 (cancel worker)
        _ <- timeout 2000000 (waitCatch worker)
        pure ()) $ \cancellationWorker -> do
      cancellationStarted <- getMonotonicTimeNSec
      -- Root identity, Git HEAD, and configuration are observed before the
      -- managed ancestor opens; keep the readiness guard independent of scan cost.
      cancellationOpened <- timeout 15000000 (race (takeMVar ancestorOpened) (waitCatch cancellationWorker))
      case cancellationOpened of
        Just (Left ()) -> do
          elapsed <- getMonotonicTimeNSec
          lastComponent <- readIORef lastHandleComponent
          putStrLn ("p7-03-watch: cancellation ancestor opened after "
            <> show ((elapsed - cancellationStarted) `div` 1000000) <> "ms; last component=" <> show lastComponent)
        Just (Right (Left exception)) ->
          assertFailure ("cancellation scan raised before opening the verified ancestor: " <> show exception)
        Just (Right (Right (Watch.RepositorySnapshot _ _))) ->
          assertFailure "cancellation scan returned before opening the verified ancestor"
        Just (Right (Right (Watch.RepositorySnapshotFailed _ failure))) ->
          assertFailure ("cancellation scan failed before opening the verified ancestor: " <> show failure)
        Nothing -> do
          outcome <- poll cancellationWorker
          lastComponent <- readIORef lastHandleComponent
          stillPaused <- readIORef pauseAncestor
          elapsed <- getMonotonicTimeNSec
          let workerState = case outcome of
                Nothing -> "running"
                Just (Left exception) -> "raised " <> show exception
                Just (Right (Watch.RepositorySnapshot _ _)) -> "returned a snapshot"
                Just (Right (Watch.RepositorySnapshotFailed _ _)) -> "returned an observation failure"
          assertFailure
            ("cancellation scan did not open a verified ancestor handle within the readiness bound"
              <> "; elapsed_ms=" <> show ((elapsed - cancellationStarted) `div` 1000000)
              <> "; worker=" <> workerState
              <> "; last_opened_component=" <> show lastComponent
              <> "; pause_enabled=" <> show stillPaused)
      heldWorker <- poll cancellationWorker
      assertBool "cancellation scan still holds the verified ancestor before cancellation" (maybe True (const False) heldWorker)
      cancellationStopped <- timeout 2000000 (cancel cancellationWorker)
      assertBool "cancelling a native scan releases its owned handles within the bound" (maybe False (const True) cancellationStopped)
      timeout 2000000 (waitCatch cancellationWorker) >>= assertBool "cancelled native scan joins within the cleanup bound" . maybe False (const True)
  Watch.repositorySnapshot observer bound >>= \case
    Watch.RepositorySnapshot _ _ -> pure ()
    Watch.RepositorySnapshotFailed _ failure -> assertFailure ("native scanner did not recover after cancellation cleanup: " <> show failure)
  authority <- either (assertFailure . show) pure (Security.mkBoundAuthority 1 "watch-runtime")
  secret <- either (assertFailure . show) pure (Security.mkProcessSecret (BS.replicate 32 7))
  runtime <- newApplicationRuntime bound authority secret Api.defaultApiLimits defaultApplicationServices unavailableEventsTransport
  runtimeClient <- Watch.registerActiveClient (applicationActiveFileRegistry runtime) >>= either (assertFailure . Text.unpack) pure
  Watch.replaceActiveFiles (applicationActiveFileRegistry runtime) runtimeClient [path] >>= either (assertFailure . Text.unpack) pure
  supersedeScanEnabled <- newIORef False
  supersedeScanStarted <- newEmptyMVar
  supersedeScanContinue <- newEmptyMVar
  supersedeEnabled <- newIORef False
  supersedeOpened <- newEmptyMVar
  supersedeRelease <- newEmptyMVar
  supersedeTraceRetries <- newIORef False
  supersedeOrigin <- getMonotonicTimeNSec
  supersedePhases <- newIORef ([] :: [(Word64, String)])
  let recordSupersedePhase phase = do
        at <- getMonotonicTimeNSec
        atomicModifyIORef' supersedePhases (\phases -> ((at, phase) : phases, ()))
  lockPhase <- newIORef False
  lockPhases <- newIORef ([] :: [(Word64, String)])
  lockRetryScanArmed <- newIORef False
  lockRetryScanOpened <- newEmptyMVar
  terminalOrigin <- getMonotonicTimeNSec
  terminalPhase <- newIORef False
  runtimeTerminalPhases <- newIORef ([] :: [(Word64, String)])
  runtimeTerminalScanOpened <- newEmptyMVar
  terminalCallback <- newEmptyMVar
  let recordTerminalPhase phase = do
        active <- readIORef terminalPhase
        when active $ do
          at <- getMonotonicTimeNSec
          atomicModifyIORef' runtimeTerminalPhases (\phases -> (take 32 ((at, phase) : phases), ()))
  let recordLockPhase phase = do
        active <- readIORef lockPhase
        if active then do
          at <- getMonotonicTimeNSec
          atomicModifyIORef' lockPhases (\phases -> ((at, phase) : phases, ()))
        else pure ()
  let afterRuntimeHandle component = do
        when (component == "watch.md") $ do
          recordTerminalPhase "verified managed decision handle opened"
          active <- readIORef terminalPhase
          when active (void (tryPutMVar runtimeTerminalScanOpened ()))
        tracingRetries <- readIORef supersedeTraceRetries
        if tracingRetries && (component == "index" || component == "architecture")
          then recordSupersedePhase ("post-epoch scan opened " <> component)
          else pure ()
        if component == "architecture" then do
          recordLockPhase "verified architecture handle opened"
          retryScan <- atomicModifyIORef' lockRetryScanArmed (\armed -> (False, armed))
          when retryScan (void (tryPutMVar lockRetryScanOpened ()))
        else pure ()
        start <- atomicModifyIORef' supersedeScanEnabled $ \enabled ->
          let fire = enabled && component == "index"
           in (enabled && not fire, fire)
        if start then do
          recordSupersedePhase "scan captured epoch and opened Git index"
          putMVar supersedeScanStarted ()
          takeMVar supersedeScanContinue
        else pure ()
        pause <- atomicModifyIORef' supersedeEnabled $ \enabled ->
          let fire = enabled && component == "architecture"
           in (enabled && not fire, fire)
        if pause then do
          recordSupersedePhase "changed scan opened architecture handle"
          putMVar supersedeOpened ()
          takeMVar supersedeRelease
        else pure ()
  runtimeObserver <- Watch.observerForRegistryWithHandleHook (applicationActiveFileRegistry runtime) bound afterRuntimeHandle
  lockDiagnosticObserver <- Watch.observerForRegistry (applicationActiveFileRegistry runtime) bound
  subscriber <- Events.registerSubscriberWithInitial (applicationEventCoordinator runtime) (Events.EventAsOfUnavailable "test") >>= either (assertFailure . Text.unpack) pure
  Events.readSubscriberEvent subscriber >>= \case
    Events.SubscriberEvent _ -> pure ()
    Events.SubscriberOverflow -> assertFailure "initial runtime watcher subscriber overflowed"
    Events.SubscriberGenerationExhausted -> assertFailure "initial runtime watcher subscriber exhausted its generation"
  publishAttempts <- newIORef ([] :: [String])
  lockRejections <- newEmptyMVar
  runtimeWatcher <- Watch.watchRepository runtimeObserver bound $ \event -> do
    tracingRetries <- readIORef supersedeTraceRetries
    if tracingRetries then recordSupersedePhase "publication callback entered" else pure ()
    recordLockPhase "publication callback entered"
    recordTerminalPhase "publication callback entered"
    outcome <- try @SomeException (publishWatcherEvent runtime event)
    if tracingRetries then recordSupersedePhase ("publication callback returned " <> either show (const "published") outcome) else pure ()
    recordLockPhase ("publication callback returned " <> either show (const "published") outcome)
    recordTerminalPhase ("publication callback returned " <> either show (const "published") outcome)
    activeTerminal <- readIORef terminalPhase
    when activeTerminal (void (tryPutMVar terminalCallback (event, outcome)))
    atomicModifyIORef' publishAttempts (\observed -> ((either show (const "published") outcome : observed), ()))
    case outcome of
      Left failure -> case fromException failure of
        Just held@(LockHeld _ _) -> do
          active <- readIORef lockPhase
          if active then void (tryPutMVar lockRejections held) else pure ()
        _ -> pure ()
      Right () -> pure ()
    either throwIO pure outcome
  let reportSupersedeFailure = do
        phases <- reverse <$> readIORef supersedePhases
        callbackAttempts <- reverse <$> readIORef publishAttempts
        watcherAwait <- timeout 100000 (Watch.awaitWatcher runtimeWatcher)
        activeFiles <- Watch.activeFileUnion (applicationActiveFileRegistry runtime)
        currentSnapshot <- timeout 3000000 $ do
          diagnosticObserver <- Watch.observerForRegistry (applicationActiveFileRegistry runtime) bound
          Watch.repositorySnapshot diagnosticObserver bound
        let snapshotStatus = case currentSnapshot of
              Nothing -> "timed out"
              Just (Watch.RepositorySnapshot diagnosticEpoch facts) ->
                "epoch=" <> show diagnosticEpoch <> ", managed_identity=" <> show (Watch.factsManagedSourceIdentity facts)
              Just (Watch.RepositorySnapshotFailed diagnosticEpoch failure) ->
                "epoch=" <> show diagnosticEpoch <> ", failure=" <> show failure
        putStrLn ("p7-03-watch: epoch failure evidence; phases_ms="
          <> show [((at - supersedeOrigin) `div` 1000000, phase) | (at, phase) <- phases]
          <> "; callback_attempts=" <> show callbackAttempts
          <> "; watcher_await=" <> maybe "pending" (const "returned") watcherAwait
          <> "; active_files=" <> show activeFiles
          <> "; independent_snapshot=" <> snapshotStatus)
  (`finally` do
      stoppedRuntimeWatcher <- timeout 3000000 (Watch.stopWatching runtimeWatcher >> Watch.awaitWatcher runtimeWatcher)
      Events.unregisterSubscriber (applicationEventCoordinator runtime) subscriber
      Watch.unregisterActiveClient (applicationActiveFileRegistry runtime) runtimeClient
      stopApplicationRuntime runtime
      case stoppedRuntimeWatcher of Nothing -> ioError (userError "runtime watcher cleanup timed out"); Just () -> pure ()) $ do
    supersedeGeneration <- (do
        supersedeBaseline <- Watch.repositorySnapshot observer bound >>= managedIdentityOf
        recordSupersedePhase "baseline managed identity captured"
        writeIORef supersedeScanEnabled True
        scanStarted <- timeout 15000000 (takeMVar supersedeScanStarted)
        phasesAtStart <- reverse <$> readIORef supersedePhases
        assertBool ("runtime scan captured the pre-write epoch; phases=" <> show [((at - supersedeOrigin) `div` 1000000, phase) | (at, phase) <- phasesAtStart]) (maybe False (const True) scanStarted)
        BS.writeFile managedDecision "managed scan captured before interest epoch advance"
        recordSupersedePhase "managed file write completed"
        supersedeChanged <- Watch.repositorySnapshot observer bound >>= managedIdentityOf
        assertBool "the single managed write changes the observed source identity" (supersedeChanged /= supersedeBaseline)
        recordSupersedePhase "changed managed identity verified"
        writeIORef supersedeEnabled True
        putMVar supersedeScanContinue ()
        supersedeReached <- timeout 15000000 (takeMVar supersedeOpened)
        phasesAtBarrier <- reverse <$> readIORef supersedePhases
        assertBool ("runtime scan reached the deterministic pre-publication epoch barrier; phases=" <> show [((at - supersedeOrigin) `div` 1000000, phase) | (at, phase) <- phasesAtBarrier]) (maybe False (const True) supersedeReached)
        Watch.replaceActiveFiles (applicationActiveFileRegistry runtime) runtimeClient [secondPath] >>= either (assertFailure . Text.unpack) pure
        recordSupersedePhase "active-file epoch advanced"
        writeIORef supersedeTraceRetries True
        putMVar supersedeRelease ()
        supersedeRecovered <- timeout 15000000 (Events.readSubscriberEvent subscriber)
        generation <- case supersedeRecovered of
          Just (Events.SubscriberEvent envelope) -> do
            expectedOid <- resolveRevision repository (RevisionSpec "HEAD") >>= either (assertFailure . show) pure
            Events.eventAsOf envelope @?= Events.EventAt expectedOid
            case Events.eventPayload envelope of
              Events.RepositoryInvalidated invalidations -> assertBool "superseded scan is retried to the persistent managed fact" (Events.ManagedSourceChanged `elem` invalidations)
              _ -> assertFailure "superseded scan retry emitted an observation failure"
            pure (Events.eventGeneration envelope)
          Just Events.SubscriberOverflow -> assertFailure "superseded scan retry overflowed its subscriber"
          Just Events.SubscriberGenerationExhausted -> assertFailure "superseded scan retry exhausted its generation"
          Nothing -> assertFailure "superseded scan was not retried after the active-file epoch advanced"
        recordSupersedePhase "subscriber observed coherent retry"
        readIORef publishAttempts >>= assertBool "the stale scan was explicitly rejected before a later retry published" . any (Text.isInfixOf "superseded" . Text.toLower . Text.pack)
        phasesAfterRetry <- reverse <$> readIORef supersedePhases
        putStrLn ("p7-03-watch: epoch phase trace (ms) " <> show [((at - supersedeOrigin) `div` 1000000, phase) | (at, phase) <- phasesAfterRetry])
        writeIORef supersedeTraceRetries False
        pure generation) `onException` reportSupersedeFailure
    writeIORef publishAttempts []
    lockAcquired <- newEmptyMVar
    releaseLock <- newEmptyMVar
    let acquireUntilHeld remaining = do
          if remaining <= (0 :: Int) then ioError (userError "unable to acquire the test Git lock within 100 attempts") else pure ()
          attempt <- try @SomeException (withGitLock repository (putMVar lockAcquired () >> takeMVar releaseLock))
          case attempt of
            Left failure -> case fromException failure of
              Just cancellation -> throwIO (cancellation :: SomeAsyncException)
              Nothing -> threadDelay 20000 >> acquireUntilHeld (remaining - 1)
            Right () -> pure ()
    lockOwner <- async (acquireUntilHeld 100)
    let stopLockOwner = do
          void (tryPutMVar releaseLock ())
          cancel lockOwner
          void (waitCatch lockOwner)
        acquisition = do
          acquired <- timeout 3000000 (race (takeMVar lockAcquired) (waitCatch lockOwner))
          case acquired of
            Just (Left ()) -> pure ()
            Just (Right (Left exception)) -> assertFailure ("Git-lock owner failed before acquisition: " <> show exception)
            Just (Right (Right ())) -> assertFailure "Git-lock owner ended before acquisition"
            Nothing -> assertFailure "Git-lock acquisition timed out"
    acquisition `onException` stopLockOwner
    (`finally` stopLockOwner) $ do
      putStrLn "p7-03-watch: git lock acquired"
      ownerStatus <- gitLockStatus repository
      (ownerPath, ownerPid) <- case ownerStatus of
        Left (LockHeld lockPath lockPid) -> pure (lockPath, lockPid)
        other -> assertFailure ("Git-lock owner was not live after acquisition: " <> show other)
      phaseOrigin <- getMonotonicTimeNSec
      writeIORef lockPhase True
      lockBaseline <- Watch.repositorySnapshot lockDiagnosticObserver bound >>= managedIdentityOf
      let reportLockFailure reason = do
            phases <- reverse <$> readIORef lockPhases
            recordedAttempts <- reverse <$> readIORef publishAttempts
            activeFiles <- Watch.activeFileUnion (applicationActiveFileRegistry runtime)
            watcherAwait <- timeout 100000 (Watch.awaitWatcher runtimeWatcher)
            independent <- timeout 3000000 (Watch.repositorySnapshot lockDiagnosticObserver bound)
            let snapshotStatus = case independent of
                  Nothing -> "timed out"
                  Just (Watch.RepositorySnapshot snapshotEpoch facts) ->
                    "epoch=" <> show snapshotEpoch <> ", managed_identity=" <> show (Watch.factsManagedSourceIdentity facts)
                  Just (Watch.RepositorySnapshotFailed snapshotEpoch failure) ->
                    "epoch=" <> show snapshotEpoch <> ", failure=" <> show failure
            assertFailure (reason <> "; phases_ms="
              <> show [((at - phaseOrigin) `div` 1000000, phase) | (at, phase) <- phases]
              <> "; callback_attempts=" <> show recordedAttempts
              <> "; baseline_managed_identity=" <> show lockBaseline
              <> "; active_files=" <> show activeFiles
              <> "; watcher_await=" <> maybe "pending" (const "returned") watcherAwait
              <> "; independent_snapshot=" <> snapshotStatus)
      recordLockPhase "managed file write started under Git lock"
      BS.writeFile managedDecision "contention change without a second filesystem hint"
      recordLockPhase "managed file write completed under Git lock"
      rejected <- timeout 8000000 (takeMVar lockRejections)
      case rejected of
        Just (LockHeld rejectedPath rejectedPid) -> do
          rejectedPath @?= ownerPath
          rejectedPid @?= ownerPid
          ownerStillRunning <- poll lockOwner
          assertBool "Git-lock owner remains live through typed watcher rejection" (maybe True (const False) ownerStillRunning)
          putStrLn "p7-03-watch: typed Git-lock rejection observed under held owner"
        Just other -> assertFailure ("watcher reported a different Git-lock error: " <> show other)
        Nothing -> do
          ownerOutcome <- poll lockOwner
          reportLockFailure ("watcher did not report typed LockHeld within the bounded verification interval; owner=" <> show ownerOutcome)
      writeIORef lockRetryScanArmed True
      recordLockPhase "Git-lock owner release requested"
      putMVar releaseLock ()
      lockFinished <- timeout 2000000 (waitCatch lockOwner)
      assertBool "Git-lock owner released within the bound" (maybe False (either (const False) (const True)) lockFinished)
      recordLockPhase "Git-lock owner released"
      retryScan <- timeout 10000000 (takeMVar lockRetryScanOpened)
      case retryScan of
        Just () -> recordLockPhase "retry scan opened verified architecture handle"
        Nothing -> reportLockFailure "watcher did not begin a retry scan after typed Git-lock rejection"
      observed <- timeout 5000000 (Events.readSubscriberEvent subscriber)
      expectedOid <- resolveRevision repository (RevisionSpec "HEAD") >>= either (assertFailure . show) pure
      case observed of
        Just (Events.SubscriberEvent envelope) -> do
          Events.eventAsOf envelope @?= Events.EventAt expectedOid
          assertBool "retried watcher publication receives a later coherent generation" (Events.eventGeneration envelope > supersedeGeneration)
          case Events.eventPayload envelope of
            Events.RepositoryInvalidated invalidations -> assertBool "retried watcher publication includes the managed file change" (Events.ManagedSourceChanged `elem` invalidations)
            _ -> assertFailure "Git-lock retry emitted an observation failure"
          recordLockPhase "subscriber observed the retried managed change"
        Just Events.SubscriberOverflow -> assertFailure "watcher retry subscriber overflowed"
        Just Events.SubscriberGenerationExhausted -> assertFailure "watcher retry subscriber exhausted its generation"
        Nothing -> reportLockFailure "watcher did not deliver the retried event after the retry scan opened"
      writeIORef lockPhase False
      phases <- reverse <$> readIORef lockPhases
      putStrLn ("p7-03-watch: lock phase trace (ms) " <> show [((at - phaseOrigin) `div` 1000000, phase) | (at, phase) <- phases])
    writeIORef publishAttempts []
    withGitLock repository (pure ())
    threadDelay 750000
    readIORef publishAttempts >>= (@?= [])
    putStrLn "p7-03-watch: lock retry complete"
    terminalLockAcquired <- newEmptyMVar
    releaseTerminalLock <- newEmptyMVar
    let acquireTerminalLock remaining = do
          if remaining <= (0 :: Int) then ioError (userError "terminal watcher test could not acquire Git lock") else pure ()
          attempt <- try @SomeException (withGitLock repository (putMVar terminalLockAcquired () >> takeMVar releaseTerminalLock))
          case attempt of
            Left failure -> case fromException failure of
              Just cancellation -> throwIO (cancellation :: SomeAsyncException)
              Nothing -> threadDelay 20000 >> acquireTerminalLock (remaining - 1)
            Right () -> pure ()
    terminalLockOwner <- async (acquireTerminalLock 100)
    (`finally` do _ <- tryPutMVar releaseTerminalLock (); cancel terminalLockOwner; void (waitCatch terminalLockOwner)) $ do
      held <- timeout 3000000 (race (takeMVar terminalLockAcquired) (waitCatch terminalLockOwner))
      case held of
        Just (Left ()) -> pure ()
        other -> assertFailure ("terminal watcher test never held the Git lock: " <> show other)
      ownerStatus <- gitLockStatus repository
      case ownerStatus of
        Left (LockHeld _ _) -> pure ()
        other -> assertFailure ("terminal Git lock was not held by its live owner: " <> show other)
      writeIORef terminalPhase True
      recordTerminalPhase "Git lock owner confirmed"
      Events.setEventGenerationForTest (applicationEventCoordinator runtime) maxBound
      recordTerminalPhase "generation set to maxBound under Git lock"
      BS.writeFile managedDecision "terminal generation while Git lock remains held"
      recordTerminalPhase "managed decision write completed under Git lock"
      scanned <- timeout 6000000 (takeMVar runtimeTerminalScanOpened)
      recordTerminalPhase (if maybe False (const True) scanned then "managed decision scan opened" else "managed decision scan deadline elapsed")
      callback <- case scanned of
        Just () -> timeout 3000000 (takeMVar terminalCallback)
        Nothing -> pure Nothing
      recordTerminalPhase (if maybe False (const True) callback then "publication callback observed" else "publication callback deadline elapsed")
      terminalStop <- case callback of
        Just _ -> timeout 3000000 (Watch.awaitWatcher runtimeWatcher)
        Nothing -> pure Nothing
      recordTerminalPhase (if maybe False (const True) terminalStop then "verifier and native backend joined" else "watcher join deadline elapsed")
      phases <- reverse <$> readIORef runtimeTerminalPhases
      let terminalTrace = show [((at - terminalOrigin) `div` 1000000, phase) | (at, phase) <- phases]
      putStrLn ("p7-03-watch: terminal phase trace (ms) " <> terminalTrace)
      assertBool ("terminal watcher scanned the changed managed file before Git unlock; phases=" <> terminalTrace) (maybe False (const True) scanned)
      case callback of
        Just (Watch.RepositoryFactsChanged _ _ invalidations, Left failure) -> do
          assertBool ("terminal callback carries the managed change; phases=" <> terminalTrace)
            (Events.ManagedSourceChanged `elem` invalidations)
          assertBool ("terminal callback threw GenerationExhausted; failure=" <> show failure <> "; phases=" <> terminalTrace)
            (maybe False (const True) (fromException failure :: Maybe Events.GenerationExhausted))
        Just (_, Right ()) -> assertFailure ("terminal callback unexpectedly published; phases=" <> terminalTrace)
        Just (event, Left failure) -> assertFailure ("terminal callback observed another event or failure; event=" <> show event <> "; failure=" <> show failure <> "; phases=" <> terminalTrace)
        Nothing -> assertFailure ("terminal publication callback did not finish; phases=" <> terminalTrace)
      assertBool ("terminal watcher finishes and joins its native backend before Git unlock; phases=" <> terminalTrace) (maybe False (const True) terminalStop)
      terminalOwner <- poll terminalLockOwner
      assertBool ("terminal Git lock owner remains live through watcher join; phases=" <> terminalTrace) (maybe True (const False) terminalOwner)
      readIORef publishAttempts >>= assertBool "typed terminal failure replaces Git-lock retry" . any (Text.isInfixOf "GenerationExhausted" . Text.pack)
      Events.readSubscriberEvent subscriber >>= \case
        Events.SubscriberGenerationExhausted -> pure ()
        _ -> assertFailure "terminal watcher must wake its existing subscriber before Git unlock"
  let linkedRoot = root <> "-watch-linked"
  callProcess "git" ["-C", root, "worktree", "add", "--detach", linkedRoot, "HEAD"]
  (`finally` callProcess "git" ["-C", root, "worktree", "remove", "--force", linkedRoot]) $ do
    linkedRepository <- discoverRepository systemGit linkedRoot >>= either (assertFailure . show) pure
    linkedBound <- either (assertFailure . show) pure (Api.validateRepositoryBinding (Right linkedRepository))
    linkedRegistry <- Watch.newActiveFileRegistry
    linkedOrigin <- getMonotonicTimeNSec
    linkedPhases <- newIORef ([] :: [(Word64, String)])
    let markLinked phase = do
          at <- getMonotonicTimeNSec
          atomicModifyIORef' linkedPhases (\phases -> (take 64 ((at, phase) : phases), ()))
    linkedObserver <- Watch.observerForRegistryWithHandleHook linkedRegistry linkedBound $ \component ->
      if component == "refs" then markLinked "common refs opened" else pure ()
    linkedBefore <- Watch.repositorySnapshot linkedObserver linkedBound
    case linkedBefore of
      Watch.RepositorySnapshot _ facts -> assertBool "linked-worktree snapshot retains detached HEAD and shared common metadata" (Watch.factsHead facts /= Nothing && Watch.factsHeadState facts /= Nothing)
      Watch.RepositorySnapshotFailed _ failure -> assertFailure (show failure)
    let linkedCommonRef = Api.repoCommonDirectory linkedBound </> "refs" </> "heads" </> "watch-linked-common"
        linkedMetadata = Api.repoGitDirectory linkedBound </> "watch-linked-private"
    BS.writeFile linkedCommonRef (TextEncoding.encodeUtf8 (headForFacts <> "\n"))
    linkedCommonChanged <- Watch.repositorySnapshot linkedObserver linkedBound
    case (linkedBefore, linkedCommonChanged) of
      (Watch.RepositorySnapshot _ leftFacts, Watch.RepositorySnapshot _ rightFacts) ->
        assertBool "linked worktrees observe shared common-reference changes" (Events.CommonReferencesChanged `elem` Watch.diffRepositoryFacts leftFacts rightFacts)
      _ -> assertFailure "linked common-reference observation failed"
    removeFile linkedCommonRef
    linkedRestored <- Watch.repositorySnapshot linkedObserver linkedBound
    BS.writeFile linkedMetadata "linked-private-metadata"
    linkedPrivateChanged <- Watch.repositorySnapshot linkedObserver linkedBound
    case (linkedRestored, linkedPrivateChanged) of
      (Watch.RepositorySnapshot _ leftFacts, Watch.RepositorySnapshot _ rightFacts) ->
        assertBool "linked worktrees observe their own private Git metadata" (Events.WorktreeMetadataChanged `elem` Watch.diffRepositoryFacts leftFacts rightFacts)
      _ -> assertFailure "linked private-metadata observation failed"
    removeFile linkedMetadata
    sharedPeriodic <- newEmptyMVar
    markLinked "watcher start began"
    linkedWatcher <- Watch.watchRepository linkedObserver linkedBound $ \event ->
      case event of
        Watch.RepositoryFactsChanged _ _ invalidations -> do
          markLinked ("callback " <> show invalidations)
          if Events.CommonReferencesChanged `elem` invalidations
            then do
              selected <- tryPutMVar sharedPeriodic event
              markLinked (if selected then "exact callback selected" else "duplicate exact callback ignored")
            else markLinked "incidental callback ignored"
        other -> do
          markLinked ("callback " <> show other)
          markLinked "incidental callback ignored"
    markLinked "watcher start returned"
    (`finally` do
        Watch.stopWatching linkedWatcher
        stopped <- timeout 3000000 (Watch.awaitWatcher linkedWatcher)
        sharedExists <- doesFileExist linkedCommonRef
        if sharedExists then removeFile linkedCommonRef else pure ()
        assertBool "linked watcher workers stop within the owner bound" (maybe False (const True) stopped)) $ do
      linkedBaseline <- Watch.repositorySnapshot linkedObserver linkedBound
      case linkedBaseline of
        Watch.RepositorySnapshot _ _ -> markLinked "pre-write baseline captured"
        Watch.RepositorySnapshotFailed _ failure -> assertFailure ("linked pre-write baseline failed: " <> show failure)
      markLinked "common ref write began"
      BS.writeFile linkedCommonRef (TextEncoding.encodeUtf8 (headForFacts <> "\n"))
      markLinked "common ref write returned"
      periodic <- timeout 3000000 (race (takeMVar sharedPeriodic) (Watch.awaitWatcher linkedWatcher))
      case periodic of
        Just (Left (Watch.RepositoryFactsChanged _ _ invalidations)) -> do
          markLinked ("selected callback consumed " <> show invalidations)
          assertBool "periodic verification detects shared metadata outside the linked worktree watch root" (Events.CommonReferencesChanged `elem` invalidations)
        Just (Left other) -> assertFailure ("linked periodic observation returned " <> show other)
        Just (Right ()) -> do
          phases <- reverse <$> readIORef linkedPhases
          assertFailure ("linked watcher ended before periodic common-reference delivery; phases(ms)="
            <> show [((at - linkedOrigin) `div` 1000000, phase) | (at, phase) <- phases])
        Nothing -> do
          markLinked "exact callback wait timed out while watcher remained live"
          observed <- timeout 3000000 (Watch.repositorySnapshot linkedObserver linkedBound)
          lateExact <- tryTakeMVar sharedPeriodic
          markLinked (if maybe False (const True) lateExact then "exact callback available after deadline" else "no exact callback available after deadline")
          phases <- reverse <$> readIORef linkedPhases
          let change = case (linkedBaseline, observed) of
                (Watch.RepositorySnapshot _ baselineFacts, Just (Watch.RepositorySnapshot _ observedFacts)) -> show (Watch.diffRepositoryFacts baselineFacts observedFacts)
                (_, Just other) -> show other
                (_, Nothing) -> "direct snapshot timed out"
          assertFailure ("linked periodic common-reference change was not delivered; phases(ms)="
            <> show [((at - linkedOrigin) `div` 1000000, phase) | (at, phase) <- phases]
            <> "; direct change=" <> change)
      phases <- reverse <$> readIORef linkedPhases
      putStrLn ("p7-03-watch: linked periodic phase trace (ms) "
        <> show [((at - linkedOrigin) `div` 1000000, phase) | (at, phase) <- phases])
  let backendOfflineRoot = root <> "-backend-offline"
  backendRecovered <- newEmptyMVar
  renameDirectory root backendOfflineRoot
  fallbackWatcher <- Watch.watchRepository observer bound $ \event -> do
    _ <- tryPutMVar backendRecovered event
    pure ()
  (`finally` do
      Watch.stopWatching fallbackWatcher
      _ <- timeout 3000000 (Watch.awaitWatcher fallbackWatcher)
      rootPresent <- doesDirectoryExist root
      if rootPresent then pure () else renameDirectory backendOfflineRoot root) $ do
    threadDelay 350000
    renameDirectory backendOfflineRoot root
    recovered <- timeout 3000000 (takeMVar backendRecovered)
    assertBool "periodic verification recovers after the fsnotify backend starts while the bound root is absent" (maybe False (const True) recovered)
  let originalRoot = root <> "-original"
  originalHead <- gitHead root
  renameDirectory root originalRoot
  (`finally` do
      replacement <- doesDirectoryExist root
      if replacement then removeDirectoryRecursive root else pure ()
      renameDirectory originalRoot root) $ do
    copyFixtureTree originalRoot root
    gitHead root >>= (@?= originalHead)
    replaced <- Watch.repositorySnapshot observer bound
    case replaced of
      Watch.RepositorySnapshotFailed _ _ -> pure ()
      Watch.RepositorySnapshot _ _ -> assertFailure "replacement repository root with the same spelling was adopted"
  putStrLn "p7-03-watch: root replacement complete"

copyFixtureTree :: FilePath -> FilePath -> IO ()
copyFixtureTree source destination = do
  createDirectory destination
  entries <- listDirectory source
  forM_ entries $ \entry -> do
    let sourceEntry = source </> entry
        destinationEntry = destination </> entry
    directory <- doesDirectoryExist sourceEntry
    if directory
      then copyFixtureTree sourceEntry destinationEntry
      else do
        copyFile sourceEntry destinationEntry
        permissions <- getPermissions destinationEntry
        setPermissions destinationEntry permissions {writable = True}

managedIdentityOf :: Watch.RepositorySnapshot -> IO (Maybe Text)
managedIdentityOf snapshot = case snapshot of
  Watch.RepositorySnapshot _ facts -> pure (Watch.factsManagedSourceIdentity facts)
  Watch.RepositorySnapshotFailed _ failure -> assertFailure (show failure)

testCompilationRuntime :: IO ()
testCompilationRuntime = withSeededRepository $ \root -> do
  starts <- newIORef (0 :: Int)
  joined <- newEmptyMVar
  producerEntered <- newEmptyMVar
  releaseProducer <- newEmptyMVar
  let compileExact repository oid = do
        count <- atomicModifyIORef' starts (\value -> let next = value + 1 in (next, next))
        if count == 1 then putMVar producerEntered () >> takeMVar releaseProducer else pure ()
        Runtime.ensureExactArchive repository oid
      services = defaultApplicationServices
        { applicationCompileExact = compileExact,
          applicationAfterCompilationJoin = putMVar joined ()
        }
      injected = dependencies {serverApplicationServices = services}
      awaitSignal label signal worker = do
        outcome <- timeout 5000000 (race (takeMVar signal) (waitCatch worker))
        case outcome of
          Just (Left ()) -> pure ()
          Just (Right (Left exception)) -> assertFailure (label <> " request failed before its signal: " <> show exception)
          Just (Right (Right _)) -> assertFailure (label <> " request completed before its signal")
          Nothing -> assertFailure (label <> " signal timed out")
  repository <- discoverRepository systemGit root >>= either (assertFailure . show) pure
  started <- withWebServer injected root (Api.WebOptions Nothing False) $ \running _ -> do
    headBefore <- gitHead root
    let coldArchive = root </> ".adrai" </> "cache" </> Text.unpack headBefore <> ".sqlite"
    coldPresent <- doesFileExist coldArchive
    assertBool "single-flight exercise begins without an exact archive" (not coldPresent)
    first <- async (getJson running "/api/v1/doctor")
    awaitSignal "first compilation join" joined first
    awaitSignal "producer entry" producerEntered first
    second <- async (getJson running "/api/v1/search?q=seed")
    awaitSignal "second compilation join" joined second
    putMVar releaseProducer ()
    doctor <- wait first
    search <- wait second
    valueAt ["data"] doctor >>= \value -> assertBool "doctor returned a typed projection" (value /= Aeson.Null)
    valueAt ["data"] search >>= \value -> assertBool "search returned its distinct projection" (value /= Aeson.Null)
    readIORef starts >>= (@?= 1)
    doesFileExist coldArchive >>= assertBool "the elected producer created the exact archive consumed by both routes"
    phaseOrigin <- getMonotonicTimeNSec
    callProcess "git" ["-C", root, "commit", "--allow-empty", "-m", "second exact revision"]
    secondHead <- gitHead root
    commitFinished <- getMonotonicTimeNSec
    lastBusy <- newIORef "no busy response"
    let route = "/api/v1/doctor"
        retryDoctor attempt = do
          (status, response) <- getJsonStatus running route
          case status of
            200 -> do
              if attempt > 1 then do
                now <- getMonotonicTimeNSec
                lock <- gitLockStatus repository
                previous <- readIORef lastBusy
                putStrLn ("second-revision doctor recovered after " <> show attempt <> " attempts, "
                  <> show ((now - phaseOrigin) `div` 1000000) <> " ms after commit start, lock " <> show lock
                  <> "; prior busy " <> previous)
              else pure ()
              pure response
            503 -> do
              let field keys value = case (keys, value) of
                    ([], _) -> Just value
                    (key : rest, Aeson.Object fields) -> KeyMap.lookup key fields >>= field rest
                    _ -> Nothing
                  typedBusy =
                    field ["error", "category"] response == Just (Aeson.String "service-failure")
                      && field ["error", "status"] response == Just (Aeson.Number 503)
                      && field ["error", "code"] response == Just (Aeson.String "repository-busy")
                      && field ["metadata", "as_of", "kind"] response == Just (Aeson.String "unavailable")
                      && field ["metadata", "as_of", "reason"] response == Just (Aeson.String "request-failed")
              assertBool ("second-revision doctor returned malformed HTTP 503: " <> show response) typedBusy
              now <- getMonotonicTimeNSec
              lock <- gitLockStatus repository
              let evidence = "attempt " <> show attempt <> ", " <> show ((now - phaseOrigin) `div` 1000000)
                    <> " ms after commit start, " <> show ((now - commitFinished) `div` 1000000)
                    <> " ms after commit completed, response " <> show response <> ", lock " <> show lock
              writeIORef lastBusy evidence
              if attempt == 1 then putStrLn ("second-revision doctor transient busy: " <> evidence) else pure ()
              threadDelay 50000
              retryDoctor (attempt + 1)
            _ -> assertFailure ("second-revision doctor returned HTTP " <> show status <> ": " <> show response)
    secondDoctor <- timeout 30000000 (retryDoctor (1 :: Int))
    secondDoctorResponse <- case secondDoctor of
      Just response -> pure response
      Nothing -> do
        evidence <- readIORef lastBusy
        lock <- gitLockStatus repository
        assertFailure ("second-revision doctor did not succeed within thirty seconds at " <> Text.unpack secondHead <> ": " <> evidence <> ", final lock " <> show lock)
    textAt ["metadata", "as_of", "oid"] secondDoctorResponse >>= (@?= secondHead)
    readIORef starts >>= (@?= 2)
  either (assertFailure . Text.unpack) pure started
  revision <- resolveRevision repository (RevisionSpec "HEAD") >>= either (assertFailure . show) pure
  detachedCoordinator <- Compilation.newCompilationCoordinator
  detachedEntered <- newEmptyMVar
  detachedRelease <- newEmptyMVar
  let detachedProducer = putMVar detachedEntered () >> takeMVar detachedRelease >> pure (Right (Compilation.CompiledArtifact revision "detached.sqlite"))
  survivor <- async (Compilation.acquireExactCompilation detachedCoordinator repository revision detachedProducer)
  awaitSignal "detached producer entry" detachedEntered survivor
  detachedJoined <- newEmptyMVar
  abandoned <- async (Compilation.acquireExactCompilationObserved detachedCoordinator repository revision (putMVar detachedJoined ()) detachedProducer)
  awaitSignal "canceled waiter join" detachedJoined abandoned
  abandonedStop <- timeout 2000000 (cancel abandoned)
  assertBool "a canceled waiter detaches within the bound" (maybe False (const True) abandonedStop)
  putMVar detachedRelease ()
  wait survivor >>= (@?= Right (Compilation.CompiledArtifact revision "detached.sqlite"))
  Compilation.stopCompilationCoordinator detachedCoordinator
  stoppingCoordinator <- Compilation.newCompilationCoordinator
  stoppingEntered <- newEmptyMVar
  neverFinish <- newEmptyMVar
  let stoppingProducer = putMVar stoppingEntered () >> takeMVar neverFinish >> pure (Left "unreachable")
  stoppingWaiter <- async (Compilation.acquireExactCompilation stoppingCoordinator repository revision stoppingProducer)
  awaitSignal "stopping producer entry" stoppingEntered stoppingWaiter
  stopped <- timeout 2000000 (Compilation.stopCompilationCoordinator stoppingCoordinator)
  assertBool "stopping the coordinator cancels its active producer" (maybe False (const True) stopped)
  waiterStopped <- timeout 2000000 (waitCatch stoppingWaiter)
  assertBool "the canceled producer releases its waiter" (maybe False (const True) waiterStopped)
  namespaceCoordinator <- Compilation.newCompilationCoordinator
  namespaceStarted <- newEmptyMVar
  namespaceRelease <- newEmptyMVar
  namespaceCount <- newIORef (0 :: Int)
  let otherBinding = repository {repositoryCommandDirectory = repositoryCommandDirectory repository <> "-other-binding"}
      namespaceProducer database = do
        atomicModifyIORef' namespaceCount (\value -> (value + 1, ()))
        putMVar namespaceStarted ()
        takeMVar namespaceRelease
        pure (Right (Compilation.CompiledArtifact revision database))
  namespaceFirst <- async (Compilation.acquireExactCompilation namespaceCoordinator repository revision (namespaceProducer "first.sqlite"))
  namespaceSecond <- async (Compilation.acquireExactCompilation namespaceCoordinator otherBinding revision (namespaceProducer "second.sqlite"))
  awaitSignal "first repository namespace producer" namespaceStarted namespaceFirst
  awaitSignal "second repository namespace producer" namespaceStarted namespaceSecond
  readIORef namespaceCount >>= (@?= 2)
  putMVar namespaceRelease ()
  putMVar namespaceRelease ()
  wait namespaceFirst >>= (@?= Right (Compilation.CompiledArtifact revision "first.sqlite"))
  wait namespaceSecond >>= (@?= Right (Compilation.CompiledArtifact revision "second.sqlite"))
  Compilation.stopCompilationCoordinator namespaceCoordinator
  withSeededRepository $ \overlapRoot -> do
    overlapRepository <- discoverRepository systemGit overlapRoot >>= either (assertFailure . show) pure
    overlapStarts <- newIORef (0 :: Int)
    overlapJoined <- newEmptyMVar
    overlapEntered <- newEmptyMVar
    overlapRelease <- newEmptyMVar
    publicationEntered <- newEmptyMVar
    overlapOrigin <- getMonotonicTimeNSec
    overlapPhases <- newIORef ([] :: [String])
    let recordOverlap phase = do
          now <- getMonotonicTimeNSec
          atomicModifyIORef' overlapPhases (\phases -> ((show ((now - overlapOrigin) `div` 1000000) <> "ms " <> phase) : phases, ()))
        overlapEvidence = do
          phases <- reverse <$> readIORef overlapPhases
          headNow <- gitHead overlapRoot
          lock <- gitLockStatus overlapRepository
          pure ("phases=" <> show phases <> ", HEAD=" <> Text.unpack headNow <> ", Git lock " <> show lock)
        awaitOverlap label micros signal worker = do
          outcome <- timeout micros (race (takeMVar signal) (waitCatch worker))
          case outcome of
            Just (Left ()) -> recordOverlap (label <> " observed")
            Just (Right (Left exception)) -> do
              evidence <- overlapEvidence
              assertFailure (label <> " request failed before its signal: " <> show exception <> "; " <> evidence)
            Just (Right (Right _)) -> do
              evidence <- overlapEvidence
              assertFailure (label <> " request completed before its signal; " <> evidence)
            Nothing -> do
              evidence <- overlapEvidence
              assertFailure (label <> " signal timed out; " <> evidence)
    let originalDispatch = dispatchApplicationRequest defaultApplicationServices
        tracedDispatch coordinator compileForDispatch afterJoin fallback allocate afterResolution publish bound apiRequest = do
          let route = case apiRequest of
                Api.ApiCreateRequest _ _ -> "mutation"
                Api.ApiDoctorRequest _ -> "doctor"
                _ -> "other"
          recordOverlap (route <> " server dispatch entered")
          result <- originalDispatch coordinator compileForDispatch afterJoin fallback allocate afterResolution publish bound apiRequest
            `onException` recordOverlap (route <> " server dispatch failed or cancelled")
          recordOverlap (route <> " server dispatch completed")
          pure result
        overlapServices = defaultApplicationServices
          { applicationCompileExact = \repositoryToCompile oid -> do
              count <- atomicModifyIORef' overlapStarts (\value -> let next = value + 1 in (next, next))
              recordOverlap ("physical producer " <> show count <> " entered")
              if count == 1 then putMVar overlapEntered () >> takeMVar overlapRelease else pure ()
              recordOverlap ("physical producer " <> show count <> " release observed; exact archive entered")
              archive <- Runtime.ensureExactArchiveWithColdPathObserver
                (recordOverlap ("physical producer " <> show count <> " cold archive compilation entered")) repositoryToCompile oid
                `onException` recordOverlap ("physical producer " <> show count <> " exact archive failed or cancelled")
              recordOverlap ("physical producer " <> show count <> " exact archive completed " <> either (const "with error") (const "successfully") archive)
              pure archive,
            dispatchApplicationRequest = tracedDispatch,
            applicationAfterCompilationJoin = recordOverlap "compilation joined" >> putMVar overlapJoined (),
            applicationBeforeCommitPublication = \_ -> recordOverlap "commit publication entered" >> putMVar publicationEntered ()
          }
        overlapDependencies = dependencies {serverApplicationServices = overlapServices}
    overlapStarted <- withWebServer overlapDependencies overlapRoot (Api.WebOptions Nothing False) $ \running serverWorker -> do
      let observeWorker label worker = poll worker >>= \case
            Nothing -> recordOverlap (label <> " running")
            Just (Left _) -> recordOverlap (label <> " failed or cancelled")
            Just (Right _) -> recordOverlap (label <> " completed")
      basis <- repositoryBasis running
      let createOverlap = postJsonStatusWith
            (requestRawWithStepAndDeadlineObserved (recordOverlap . ("mutation HTTP " <>)) 145000000 "one-flight overlap create")
            running "/api/v1/adrs" (createBody basis) >>= \(status, value) ->
              if status == 200 then pure value else assertFailure ("POST returned HTTP " <> show status <> ": " <> show value)
      bounded <- timeout 150000000 $ withAsync createOverlap $ \mutation ->
        ((do
          recordOverlap "mutation request launched"
          awaitOverlap "commit publication entry" 30000000 publicationEntered mutation
          committedHead <- timeout 10000000 (gitHead overlapRoot) >>= \case
            Just value -> pure value
            Nothing -> assertFailure "committed HEAD lookup timed out after publication"
          recordOverlap ("committed HEAD " <> Text.unpack committedHead)
          awaitOverlap "post-mutation compilation join" 15000000 overlapJoined mutation
          awaitOverlap "post-mutation producer entry" 15000000 overlapEntered mutation
          withAsync (getJsonLabeledObserved 70000000 (recordOverlap . ("doctor HTTP " <>)) "one-flight overlap doctor" running "/api/v1/doctor") $ \query ->
            (do
               recordOverlap "concurrent doctor request launched"
               awaitOverlap "concurrent query compilation join" 15000000 overlapJoined query
               putMVar overlapRelease ()
               recordOverlap "physical producer released by test"
               committedOutcome <- timeout 45000000 (wait mutation)
               committed <- case committedOutcome of
                 Just value -> pure value
                 Nothing -> overlapEvidence >>= assertFailure . ("mutation response did not complete within forty-five seconds after producer release; " <>)
               recordOverlap "mutation response received"
               assertCommitted committed
               textAt ["data", "commit"] committed >>= (@?= committedHead)
               valueAt ["data", "indexed"] committed >>= (@?= Aeson.Bool True)
               textAt ["metadata", "as_of", "oid"] committed >>= (@?= committedHead)
               doctor <- wait query
               recordOverlap "concurrent doctor response received"
               valueAt ["data"] doctor >>= \value -> assertBool "query joining post-mutation compilation receives its own projection" (value /= Aeson.Null)
               textAt ["metadata", "as_of", "oid"] doctor >>= (@?= committedHead)
               readIORef overlapStarts >>= (@?= 1)
            ) `onException` do
              observeWorker "doctor client" query
        ) `onException` do
          observeWorker "mutation client" mutation
          observeWorker "server" serverWorker
          overlapEvidence >>= putStrLn . ("post-mutation compilation overlap failed: " <>)
        ) `finally` void (tryPutMVar overlapRelease ())
      case bounded of
        Just () -> overlapEvidence >>= putStrLn . ("post-mutation compilation overlap: " <>)
        Nothing -> overlapEvidence >>= assertFailure . ("post-mutation compilation overlap timed out; " <>)
    either (assertFailure . Text.unpack) pure overlapStarted
  withSeededRepository $ \stoppingRoot -> do
    stoppingCompileEntered <- newEmptyMVar
    stoppingCompileCleaned <- newEmptyMVar
    stoppingCompileNever <- newEmptyMVar
    stoppingClient <- newIORef Nothing
    let partialCandidate = stoppingRoot </> ".adrai" </> "cache" </> "p7-03-cancelled-candidate.sqlite"
        stoppingServices = defaultApplicationServices
          { applicationCompileExact = \_ _ ->
              (`finally` do
                  exists <- doesFileExist partialCandidate
                  if exists then removeFile partialCandidate else pure ()
                  putMVar stoppingCompileCleaned ()) $ do
                createDirectoryIfMissing True (stoppingRoot </> ".adrai" </> "cache")
                bracket (SQLite.open partialCandidate) SQLite.close $ \connection -> do
                  SQLite.execute_ connection "CREATE TABLE owned_resource(value INTEGER)"
                  putMVar stoppingCompileEntered ()
                  _ <- takeMVar stoppingCompileNever
                  pure (Left "unreachable")
          }
        stoppingDependencies = dependencies {serverApplicationServices = stoppingServices}
    stoppedServer <- timeout 8000000 $ withWebServer stoppingDependencies stoppingRoot (Api.WebOptions Nothing False) $ \running _ -> do
      clientRequest <- async (getJson running "/api/v1/doctor")
      writeIORef stoppingClient (Just clientRequest)
      awaitSignal "server-owned compilation producer" stoppingCompileEntered clientRequest
    case stoppedServer of
      Nothing -> assertFailure "server stop timed out while an owned compiler was active"
      Just (Left problem) -> assertFailure ("server stop failed: " <> Text.unpack problem)
      Just (Right ()) -> pure ()
    timeout 2000000 (takeMVar stoppingCompileCleaned) >>= assertBool "server stop waits for SQLite and candidate cleanup" . maybe False (const True)
    doesFileExist partialCandidate >>= assertBool "server stop removes the partial compilation candidate" . not
    readIORef stoppingClient >>= \case
      Nothing -> assertFailure "server-stop client was not recorded"
      Just clientRequest -> timeout 2000000 (waitCatch clientRequest) >>= assertBool "server stop releases the active compilation request" . maybe False (const True)
  withSeededRepository $ \failureRoot -> do
    failures <- newIORef (0 :: Int)
    let failureServices = defaultApplicationServices
          { applicationCompileExact = \repositoryToCompile oid -> do
              attempt <- atomicModifyIORef' failures (\value -> let next = value + 1 in (next, next))
              if attempt == 1 then ioError (userError "injected exact producer failure") else Runtime.ensureExactArchive repositoryToCompile oid
          }
        failureDependencies = dependencies {serverApplicationServices = failureServices}
    failedStarted <- withWebServer failureDependencies failureRoot (Api.WebOptions Nothing False) $ \running _ -> do
      basis <- repositoryBasis running
      committed <- postJson running "/api/v1/adrs" (createBody basis)
      assertCommitted committed
      valueAt ["data", "indexed"] committed >>= (@?= Aeson.Bool False)
      warning <- valueAt ["data", "index_error"] committed
      assertBool "post-commit producer failure is reported without losing the commit" (warning /= Aeson.Null)
      _ <- getJson running "/api/v1/doctor"
      readIORef failures >>= (@?= 2)
    either (assertFailure . Text.unpack) pure failedStarted

withSeededServer :: (FilePath -> RunningServer -> IO value) -> IO value
withSeededServer action = withSeededRepository $ \root -> do
  started <- withWebServer dependencies root (Api.WebOptions Nothing False) (\running _ -> action root running)
  either (assertFailure . Text.unpack) pure started

withSeededRepository :: (FilePath -> IO value) -> IO value
withSeededRepository action = withSystemTempDirectory "adrai-p7-02-web" $ \temporary -> do
  let root = temporary </> "repo"
  createDirectoryIfMissing True root
  callProcess "git" ["-C", root, "init", "--initial-branch=main"]
  callProcess "git" ["-C", root, "config", "user.name", "ADRAI web test"]
  callProcess "git" ["-C", root, "config", "user.email", "web-test@example.invalid"]
  writeFile (root </> "seed.txt") "runtime search seed\n"
  callProcess "git" ["-C", root, "add", "--", "seed.txt"]
  callProcess "git" ["-C", root, "commit", "-m", "seed"]
  repository <- discoverRepository systemGit root >>= either (assertFailure . show) pure
  Mutation.initCommand repository >>= either (assertFailure . show) (const (pure ()))
  action root

repositoryBasis :: RunningServer -> IO Aeson.Value
repositoryBasis running = getJson running "/api/v1/repository" >>= valueAt ["data", "repository_state"]

createBody :: Aeson.Value -> Aeson.Value
createBody basis = Aeson.object
  [ "repository_state" Aeson..= basis,
    "title" Aeson..= ("Runtime ADR" :: Text),
    "summary" Aeson..= ("HTTP shared mutation" :: Text),
    "body" Aeson..= ("runtime body\n" :: Text),
    "domains" Aeson..= (["core"] :: [Text]),
    "scopes" Aeson..= (["src/**"] :: [Text]),
    "actor" Aeson..= actorValue
  ]

actorValue :: Aeson.Value
actorValue = Aeson.object ["kind" Aeson..= ("human" :: Text), "id" Aeson..= ("web-test" :: Text)]

existingBody :: Aeson.Value -> Aeson.Value -> [Pair] -> Aeson.Value
existingBody basis state fields = Aeson.object
  ([ "repository_state" Aeson..= basis,
     "state_token" Aeson..= state,
     "actor" Aeson..= actorValue
   ] <> fields)

withoutField :: Text -> Aeson.Value -> Aeson.Value
withoutField name (Aeson.Object object) = Aeson.Object (KeyMap.delete (Key.fromText name) object)
withoutField _ value = value

replaceField :: Text -> Aeson.Value -> Aeson.Value -> Aeson.Value
replaceField name replacement (Aeson.Object object) = Aeson.Object (KeyMap.insert (Key.fromText name) replacement object)
replaceField _ _ value = value

corruptTokenValue :: Aeson.Value -> Aeson.Value
corruptTokenValue (Aeson.String token) = Aeson.String (Text.dropEnd 1 token <> if Text.isSuffixOf "0" token then "1" else "0")
corruptTokenValue value = value

corruptRepositoryToken :: Aeson.Value -> Aeson.Value
corruptRepositoryToken (Aeson.Object object) =
  let key = Key.fromText "token"
   in Aeson.Object (maybe object (\value -> KeyMap.insert key (corrupt value) object) (KeyMap.lookup key object))
  where
    corrupt (Aeson.String token) = Aeson.String (Text.dropEnd 1 token <> if Text.isSuffixOf "0" token then "1" else "0")
    corrupt value = value
corruptRepositoryToken value = value

callerState :: FilePath -> IO (Text, Text, BS.ByteString, [(FilePath, BS.ByteString)])
callerState root = do
  headOid <- gitHead root
  refs <- Text.pack <$> readProcess "git" ["-C", root, "show-ref"] ""
  indexPath <- Text.strip . Text.pack <$> readProcess "git" ["-C", root, "rev-parse", "--path-format=absolute", "--git-path", "index"] ""
  index <- BS.readFile (Text.unpack indexPath)
  files <- concat <$> mapM listFiles [root </> "architecture" </> "adrai", root </> ".adrai.toml", root </> "seed.txt", root </> "caller-untracked.bin"]
  bytes <- forM files $ \path -> do
    content <- BS.readFile path
    pure (path, content)
  pure (headOid, refs, index, bytes)
  where
    listFiles path = do
      directory <- doesDirectoryExist path
      file <- doesFileExist path
      if directory
        then concat <$> (listDirectory path >>= mapM (listFiles . (path </>)))
        else pure [path | file]

mutateExisting :: RunningServer -> Text -> Text -> [Pair] -> IO Aeson.Value
mutateExisting running adr operation fields = do
  basis <- repositoryBasis running
  shown <- getJson running ("/api/v1/adrs/" <> adr)
  state <- valueAt ["data", "state_token"] shown
  postJsonLabeled 60000000 ("all-six " <> Text.unpack operation) running ("/api/v1/adrs/" <> adr <> "/" <> operation) $ Aeson.object
    ([ "repository_state" Aeson..= basis,
       "state_token" Aeson..= state,
       "actor" Aeson..= actorValue
     ] <> fields)

getJson :: RunningServer -> Text -> IO Aeson.Value
getJson = getJsonLabeled "GET"

getJsonLabeled :: String -> RunningServer -> Text -> IO Aeson.Value
getJsonLabeled = getJsonLabeledObserved 30000000 (const (pure ()))

getJsonLabeledObserved :: Int -> (String -> IO ()) -> String -> RunningServer -> Text -> IO Aeson.Value
getJsonLabeledObserved deadlineMicros observe label running path = do
  (status, value) <- getJsonStatusWith (requestRawWithStepAndDeadlineObserved observe deadlineMicros label) running path
  if status == 200 then pure value else assertFailure ("GET " <> Text.unpack path <> " returned " <> show status <> ": " <> show value)

getJsonStatus :: RunningServer -> Text -> IO (Int, Aeson.Value)
getJsonStatus = getJsonStatusWith requestRaw

getJsonStatusWith :: (Text -> BS.ByteString -> IO BS.ByteString) -> RunningServer -> Text -> IO (Int, Aeson.Value)
getJsonStatusWith rawRequest running path = do
  response <- bearerRequestWith rawRequest running ("GET " <> path) []
  status <- responseStatus response
  value <- decodeBody response
  pure (status, value)

postJson :: RunningServer -> Text -> Aeson.Value -> IO Aeson.Value
postJson running path body = postJsonStatus running path body >>= \(status, value) ->
  if status == 200 then pure value else assertFailure ("POST returned HTTP " <> show status <> ": " <> show value)

postJsonLabeled :: Int -> String -> RunningServer -> Text -> Aeson.Value -> IO Aeson.Value
postJsonLabeled deadlineMicros label running path body = postJsonStatusWith (requestRawWithStepAndDeadline deadlineMicros label) running path body >>= \(status, value) ->
  if status == 200 then pure value else assertFailure ("POST returned HTTP " <> show status <> ": " <> show value)

postJsonStatus :: RunningServer -> Text -> Aeson.Value -> IO (Int, Aeson.Value)
postJsonStatus = postJsonStatusWith requestRaw

postJsonStatusWith :: (Text -> BS.ByteString -> IO BS.ByteString) -> RunningServer -> Text -> Aeson.Value -> IO (Int, Aeson.Value)
postJsonStatusWith rawRequest running path body = do
  cookie <- sessionCookiePair running
  postJsonStatusWithCookie rawRequest running cookie path body

postJsonStatusWithCookie :: (Text -> BS.ByteString -> IO BS.ByteString) -> RunningServer -> Text -> Text -> Aeson.Value -> IO (Int, Aeson.Value)
postJsonStatusWithCookie rawRequest running cookie path body = do
  let host = Security.authorityHost (runningAuthority running)
      bytes = LBS.toStrict (Aeson.encode body)
  let
      requestHead = TextEncoding.encodeUtf8
        ("POST " <> path <> " HTTP/1.1\r\nHost: " <> host <> "\r\nOrigin: " <> Security.authorityOrigin (runningAuthority running)
          <> "\r\nAuthorization: Bearer " <> bootstrapToken running <> "\r\nCookie: " <> cookie <> "\r\nContent-Type: application/json\r\nContent-Length: "
          <> Text.pack (show (BS.length bytes)) <> "\r\nConnection: close\r\n\r\n")
  response <- rawRequest host (requestHead <> bytes)
  status <- responseStatus response
  value <- decodeBody response
  pure (status, value)

sessionCookiePair :: RunningServer -> IO Text
sessionCookiePair running = do
  let host = Security.authorityHost (runningAuthority running)
      token = Text.drop 1 (snd (Text.breakOn "?" (runningBootstrapUrl running)))
  response <- request host ("GET /?" <> token <> " HTTP/1.1\r\nHost: " <> host <> "\r\nConnection: close\r\n\r\n")
  case [Text.takeWhile (/= ';') (Text.drop (Text.length "Set-Cookie: ") line) | line <- Text.lines (TextEncoding.decodeUtf8 response), "Set-Cookie: " `Text.isPrefixOf` line] of
    [cookie] -> pure cookie
    other -> assertFailure ("expected one bootstrap cookie, got " <> show other)

assertCommitted :: Aeson.Value -> IO ()
assertCommitted value = valueAt ["data", "committed"] value >>= \case
  Aeson.Bool True -> pure ()
  other -> assertFailure ("mutation was not committed: " <> show other)

textAt :: [Text] -> Aeson.Value -> IO Text
textAt path value = valueAt path value >>= \case
  Aeson.String text -> pure text
  other -> assertFailure ("expected text at " <> show path <> ", got " <> show other)

integerAt :: [Text] -> Aeson.Value -> IO Integer
integerAt path value = valueAt path value >>= \case
  Aeson.String raw
    | not (Text.null raw)
        && (raw == "0" || Text.head raw /= '0')
        && Text.all (\character -> character >= '0' && character <= '9') raw ->
        case readMaybe (Text.unpack raw) of
          Just integer | integer <= toInteger (maxBound :: Word64) -> pure integer
          _ -> assertFailure ("expected Word64 decimal generation at " <> show path)
  other -> assertFailure ("expected integer at " <> show path <> ", got " <> show other)

valueAt :: [Text] -> Aeson.Value -> IO Aeson.Value
valueAt [] value = pure value
valueAt (key : rest) (Aeson.Object object) =
  maybe (assertFailure ("missing JSON field " <> Text.unpack key)) (valueAt rest) (KeyMap.lookup (Key.fromText key) object)
valueAt path value = assertFailure ("expected JSON object before " <> show path <> ", got " <> show value)

gitHead :: FilePath -> IO Text
gitHead root = Text.strip . Text.pack <$> readProcess "git" ["-C", root, "rev-parse", "HEAD"] ""

testOccupiedPort :: IO ()
testOccupiedPort = bracketOnError (socket AF_INET Stream defaultProtocol) close $ \held -> do
  setSocketOption held ReuseAddr 0
  bind held (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  SockAddrInet port _ <- getSocketName held
  root <- getCurrentDirectory
  bounded <- either (assertFailure . show) pure (Api.mkBoundPort (fromIntegral port))
  started <- withWebServer dependencies root (Api.WebOptions (Just bounded) False) (\_ _ -> pure ())
  assertBool "occupied bind is rejected" (either (const True) (const False) started)
  close held

testShutdownRelease :: IO ()
testShutdownRelease = do
  observed <- withServer $ \running _ -> pure (authorityPort (Security.authorityHost (runningAuthority running)))
  port <- either assertFailure pure observed
  probe <- socket AF_INET Stream defaultProtocol
  setSocketOption probe ReuseAddr 0
  outcome <- timeout 1000000 (bind probe (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1))))
  close probe
  assertBool "released port can be rebound" (maybe False (const True) outcome)

fragmentedOversizeRejected :: Security.BoundAuthority -> Text -> IO ()
fragmentedOversizeRejected authority token = do
  port <- either assertFailure pure (authorityPort (Security.authorityHost authority))
  outcome <- runOwnedSocket 5000000 port $ \client -> do
    let host = Security.authorityHost authority
        handshake =
          "GET /api/v1/events HTTP/1.1\r\nHost: " <> host
            <> "\r\nOrigin: " <> Security.authorityOrigin authority
            <> "\r\nAuthorization: Bearer " <> token
            <> "\r\nConnection: Upgrade\r\nUpgrade: websocket\r\nSec-WebSocket-Version: 13\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\r\n"
    sendAll client (TextEncoding.encodeUtf8 handshake)
    response <- recv client 4096
    assertBool "fragmented websocket test established a real upgrade" ("HTTP/1.1 101" `BS.isPrefixOf` response)
    sendAll client (maskedFrame False 1 (BS.replicate 3000 97) <> maskedFrame True 0 (BS.replicate 2000 98))
    terminated <- trySynchronous (recv client 4096)
    let closed = case terminated of
          Left _ -> True
          Right bytes -> BS.null bytes || (BS.head bytes .&. 0x0f) == 8
    assertBool "fragmented message exceeding 4096 bytes receives EOF, reset, or a close frame" closed
  _ <- ownedResult "fragmented websocket exchange" outcome
  pure ()

withRawPendingPeers :: Int -> Security.BoundAuthority -> Text -> ([Socket] -> IO value) -> IO value
withRawPendingPeers count authority token action = go count []
  where
    go remaining opened
      | remaining <= 0 = action (reverse opened)
      | otherwise = bracket (openRawPendingPeer authority token) closeOwnedSocket $ \client ->
          go (remaining - 1) (client : opened)

openRawPendingPeer :: Security.BoundAuthority -> Text -> IO Socket
openRawPendingPeer authority token = bracketOnError (socket AF_INET Stream defaultProtocol) closeOwnedSocket $ \client -> do
  admitRawPendingPeer client authority token
  pure client

admitRawPendingPeer :: Socket -> Security.BoundAuthority -> Text -> IO ()
admitRawPendingPeer client authority token = do
  port <- either assertFailure pure (authorityPort (Security.authorityHost authority))
  connect client (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))
  let handshake =
        "GET /api/v1/events HTTP/1.1\r\nHost: " <> Security.authorityHost authority
          <> "\r\nOrigin: " <> Security.authorityOrigin authority
          <> "\r\nAuthorization: Bearer " <> token
          <> "\r\nConnection: Upgrade\r\nUpgrade: websocket\r\nSec-WebSocket-Version: 13\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\r\n"
  sendAll client (TextEncoding.encodeUtf8 handshake)
  response <- recv client 4096
  assertBool "raw pending peer receives a real WebSocket upgrade" ("HTTP/1.1 101" `BS.isPrefixOf` response)

receiveUntilEof :: Socket -> IO Bool
receiveUntilEof client = do
  bytes <- recv client 4096
  if BS.null bytes then pure True else receiveUntilEof client

openRawSlowSubscriber :: Security.BoundAuthority -> Text -> IO Socket
openRawSlowSubscriber authority token = bracketOnError (socket AF_INET Stream defaultProtocol) closeOwnedSocket $ \client -> do
  setSocketOption client RecvBuffer 512
  admitRawPendingPeer client authority token
  let authenticate = LBS.toStrict (Aeson.encode (Aeson.object ["type" Aeson..= ("authenticate" :: Text), "credential" Aeson..= token]))
  sendAll client (maskedFrame True 1 authenticate)
  initial <- timeout 10000000 (readRawInitialText client)
  payload <- maybe (assertFailure "slow raw subscriber timed out reading its first complete resync message") pure initial
  assertInitialFullResync "slow raw subscriber" (LBS.fromStrict payload)
  pure client

readRawInitialText :: Socket -> IO BS.ByteString
readRawInitialText client = readFrame True 0 []
  where
    maxPayload = 256 * 1024 :: Int
    maxFrames = 64 :: Int

    readExact count = go count []
      where
        go remaining chunks
          | remaining == 0 = pure (BS.concat (reverse chunks))
          | otherwise = do
              chunk <- recv client (min remaining 4096)
              if BS.null chunk
                then assertFailure "slow raw subscriber reached EOF before its first complete resync message"
                else go (remaining - BS.length chunk) (chunk : chunks)

    readFrame first total chunks = do
      when (length chunks >= maxFrames) (assertFailure "slow raw subscriber's first message exceeded the frame bound")
      header <- readExact 2
      let flags = BS.index header 0
          lengthByte = BS.index header 1
          finished = flags .&. 0x80 /= 0
          opcode = flags .&. 0x0f
          shortLength = lengthByte .&. 0x7f
      when (flags .&. 0x70 /= 0) (assertFailure "slow raw subscriber received a frame with reserved bits")
      when (lengthByte .&. 0x80 /= 0) (assertFailure "slow raw subscriber received a masked server frame")
      when (opcode /= if first then 1 else 0) (assertFailure "slow raw subscriber's first message had an unexpected frame type")
      extraLength <- case shortLength of
        126 -> readExact 2
        127 -> readExact 8
        _ -> pure BS.empty
      let payloadLength = if BS.null extraLength
            then fromIntegral shortLength
            else BS.foldl' (\size byte -> size * 256 + fromIntegral byte) (0 :: Integer) extraLength
      when (shortLength == 126 && payloadLength < 126) (assertFailure "slow raw subscriber received a nonminimal extended length")
      when (shortLength == 127 && payloadLength < 65536) (assertFailure "slow raw subscriber received a nonminimal extended length")
      when (payloadLength > fromIntegral (maxPayload - total)) (assertFailure "slow raw subscriber's first message exceeded the payload bound")
      payload <- readExact (fromIntegral payloadLength)
      let next = payload : chunks
      if finished
        then pure (BS.concat (reverse next))
        else readFrame False (total + fromIntegral payloadLength) next

maskedFrame :: Bool -> Word8 -> BS.ByteString -> BS.ByteString
maskedFrame finished opcode payload =
  let first = (if finished then 0x80 else 0) + opcode
      lengthValue = BS.length payload
      mask = BS.pack [1, 2, 3, 4]
      header
        | lengthValue < 126 = BS.pack [first, 0x80 + fromIntegral lengthValue]
        | otherwise = BS.pack [first, 0x80 + 126, fromIntegral (lengthValue `div` 256), fromIntegral (lengthValue `mod` 256)]
      masked = BS.pack (zipWith xor (BS.unpack payload) (cycle (BS.unpack mask)))
   in header <> mask <> masked

bearerRequest :: RunningServer -> Text -> [(Text, Text)] -> IO BS.ByteString
bearerRequest = bearerRequestWith requestRaw

bearerRequestWith :: (Text -> BS.ByteString -> IO BS.ByteString) -> RunningServer -> Text -> [(Text, Text)] -> IO BS.ByteString
bearerRequestWith rawRequest running requestLine extra =
  let host = Security.authorityHost (runningAuthority running)
      token = bootstrapToken running
      headers = Text.concat [name <> ": " <> value <> "\r\n" | (name, value) <- extra]
   in rawRequest host (TextEncoding.encodeUtf8 (requestLine <> " HTTP/1.1\r\nHost: " <> host <> "\r\nAuthorization: Bearer " <> token <> "\r\n" <> headers <> "Connection: close\r\n\r\n"))

bootstrapToken :: RunningServer -> Text
bootstrapToken = Text.drop (Text.length "token=") . snd . Text.breakOn "token=" . runningBootstrapUrl

request :: Text -> Text -> IO BS.ByteString
request host bytes = requestRaw host (TextEncoding.encodeUtf8 bytes)

closeOwnedSocket :: Socket -> IO ()
closeOwnedSocket client = do
  _ <- trySynchronous (shutdown client ShutdownBoth)
  _ <- trySynchronous (close client)
  pure ()

runOwnedSocket :: Int -> Int -> (Socket -> IO value) -> IO (Maybe (Either SomeException value))
runOwnedSocket deadlineMicros port action = bracket (socket AF_INET Stream defaultProtocol) closeOwnedSocket $ \client -> do
  let address = SockAddrInet (fromIntegral port :: PortNumber) (tupleToHostAddress (127, 0, 0, 1))
      cleanup worker = do
        startedAt <- getMonotonicTimeNSec
        beforeClose <- poll worker
        closeOwnedSocket client
        closedAt <- getMonotonicTimeNSec
        stopped <- timeout 5000000 (cancel worker)
        cancelledAt <- getMonotonicTimeNSec
        let cleanupEvidence = "; worker " <> (if maybe True (const False) beforeClose then "running" else "finished")
              <> " before close, close " <> show ((closedAt - startedAt) `div` 1000000)
              <> " ms, cancellation " <> show ((cancelledAt - closedAt) `div` 1000000) <> " ms"
        case stopped of
          Nothing -> assertFailure ("owned socket worker survived shutdown and bounded cancellation" <> cleanupEvidence)
          Just () -> poll worker >>= \case
            Nothing -> assertFailure ("owned socket worker survived shutdown and bounded cancellation" <> cleanupEvidence)
            Just _ -> pure ()
  bracket (async (trySynchronous (connect client address >> action client))) cleanup $ \worker -> do
    observed <- timeout deadlineMicros (waitCatch worker)
    case observed of
      Just (Right result) -> pure (Just result)
      Just (Left failure) -> throwIO failure
      Nothing -> pure Nothing

runOwnedWebSocketClient :: Int -> Int -> WS.Headers -> WS.ClientApp value -> IO (Maybe (Either SomeException value))
runOwnedWebSocketClient deadlineMicros port headers clientApp =
  runOwnedSocket deadlineMicros port $ \client ->
    WS.runClientWithSocket client ("127.0.0.1:" <> show port) "/api/v1/events" WS.defaultConnectionOptions headers clientApp

requestRaw :: Text -> BS.ByteString -> IO BS.ByteString
requestRaw = requestRawWithStep "unlabeled"

requestRawWithStep :: String -> Text -> BS.ByteString -> IO BS.ByteString
requestRawWithStep = requestRawWithStepAndDeadline 30000000

requestRawWithStepAndDeadline :: Int -> String -> Text -> BS.ByteString -> IO BS.ByteString
requestRawWithStepAndDeadline = requestRawWithStepAndDeadlineObserved (const (pure ()))

requestRawWithStepAndDeadlineObserved :: (String -> IO ()) -> Int -> String -> Text -> BS.ByteString -> IO BS.ByteString
requestRawWithStepAndDeadlineObserved observe deadlineMicros step host bytes = do
  port <- either assertFailure pure (authorityPort host)
  let requestLine = BS8.takeWhile (/= '\r') bytes
      route = case BS8.words requestLine of
        method : target : _ -> BS8.unpack method <> " " <> BS8.unpack (BS8.takeWhile (/= '?') target)
        _ -> "unrecognized request"
  startedAt <- getMonotonicTimeNSec
  phase <- newIORef ("connecting" :: String)
  milestones <- newIORef [("connecting", startedAt)]
  let mark label = do
        now <- getMonotonicTimeNSec
        writeIORef phase label
        atomicModifyIORef' milestones (\events -> ((label, now) : events, ()))
        observe label
      progress label = writeIORef phase label >> observe label
      evidence = do
        current <- readIORef phase
        events <- reverse <$> readIORef milestones
        now <- getMonotonicTimeNSec
        let elapsed = (now - startedAt) `div` 1000000
            trace = [(label, (at - startedAt) `div` 1000000) | (label, at) <- events]
        pure (" for " <> route <> " [" <> step <> "] after " <> show elapsed <> " ms; phase " <> current <> "; milestones(ms) " <> show trace)
  observe "connecting"
  attempted <- trySynchronous $ runOwnedSocket deadlineMicros port $ \client -> do
    mark "connected; sending request"
    sendAll client bytes
    mark "request sent; awaiting first response byte"
    receiveAllWithProgress mark progress client []
  outcome <- case attempted of
    Left failure -> do
      observed <- evidence
      assertFailure ("HTTP owned socket failed" <> observed <> ": " <> show failure)
    Right value -> pure value
  case outcome of
    Nothing -> do
      observed <- evidence
      assertFailure ("HTTP response timed out" <> observed)
    Just (Left failure) -> do
      observed <- evidence
      assertFailure ("HTTP response failed" <> observed <> ": " <> show failure)
    Just (Right response) -> pure response

requestRawHeld :: (String -> IO ()) -> Int -> MVar () -> Text -> BS.ByteString -> IO BS.ByteString
requestRawHeld recordPhase deadlineMicros responseGate host bytes = do
  port <- either assertFailure pure (authorityPort host)
  phase <- newIORef ("connecting" :: String)
  let mark label = writeIORef phase label >> recordPhase label
  outcome <- runOwnedSocket deadlineMicros port $ \client -> do
    mark "connected; sending held request"
    sendAll client bytes
    mark "held request sent; waiting for response gate"
    takeMVar responseGate
    mark "response gate opened; receiving held response"
    receiveAllWithProgress (mark . ("held response " <>)) (const (pure ())) client []
  case outcome of
    Nothing -> do
      observed <- readIORef phase
      assertFailure ("controlled HTTP response timed out while " <> observed)
    Just _ -> ownedResult "controlled HTTP response" outcome

receiveAllWithProgress :: (String -> IO ()) -> (String -> IO ()) -> Socket -> [BS.ByteString] -> IO BS.ByteString
receiveAllWithProgress mark progress client chunks = do
  let accumulated = BS.concat (reverse chunks)
  if httpResponseComplete accumulated then mark "declared response complete" >> pure accumulated else do
    received <- trySynchronous (recv client 4096)
    case received of
      Left exception -> if BS.null accumulated then throwIO exception else assertFailure ("HTTP response ended before its declared body: " <> show exception)
      Right chunk -> if BS.null chunk
        then mark "response EOF" >> pure accumulated
        else do
          let next = accumulated <> chunk
              (_, oldSuffix) = BS.breakSubstring "\r\n\r\n" accumulated
              (headers, suffix) = BS.breakSubstring "\r\n\r\n" next
          when (BS.null accumulated) (mark "first response byte received")
          if BS.null oldSuffix && not (BS.null suffix)
            then mark "response headers complete"
            else pure ()
          if BS.null suffix
            then progress ("receiving headers: " <> show (BS.length next) <> " bytes")
            else do
              let bodyBytes = BS.length (BS.drop 4 suffix)
                  lengths = [count | line <- BS8.lines headers,
                    "Content-Length:" `BS.isPrefixOf` line,
                    Just (count, _) <- [BS8.readInt (BS8.dropWhile (== ' ') (BS.drop (BS.length "Content-Length:") line))]]
              progress ("receiving body: " <> show bodyBytes <> " bytes" <>
                case lengths of [count] -> " of " <> show count; _ -> " (chunked or EOF framed)")
          receiveAllWithProgress mark progress client (chunk : chunks)

httpResponseComplete :: BS.ByteString -> Bool
httpResponseComplete response =
  let (headers, suffix) = BS.breakSubstring "\r\n\r\n" response
      body = BS.drop 4 suffix
      lengths =
        [ count
        | line <- BS8.lines headers,
          "Content-Length:" `BS.isPrefixOf` line,
          Just (count, _) <- [BS8.readInt (BS8.dropWhile (== ' ') (BS.drop (BS.length "Content-Length:") line))]
        ]
   in not (BS.null suffix) && case lengths of
        [count] -> BS.length body >= count
        _ | "Transfer-Encoding: chunked" `BS.isInfixOf` headers -> "\r\n0\r\n\r\n" `BS.isSuffixOf` body || body == "0\r\n\r\n"
        _ -> False

trySynchronous :: IO value -> IO (Either SomeException value)
trySynchronous action = try action >>= \case
  Left failure -> case fromException failure of
    Just cancellation -> throwIO (cancellation :: SomeAsyncException)
    Nothing -> pure (Left failure)
  Right value -> pure (Right value)

expectedClose :: BS.ByteString -> Maybe (Either SomeException value) -> Bool
expectedClose expected = \case
  Just (Left failure) -> case fromException failure of
    Just (WS.CloseRequest _ reason) -> expected `BS.isInfixOf` LBS.toStrict reason
    _ -> False
  _ -> False

expectedHandshakeStatus :: Int -> Maybe (Either SomeException value) -> Bool
expectedHandshakeStatus expected = \case
  Just (Left failure) -> case fromException failure of
    Just (WS.RequestRejected _ response) -> WS.responseCode response == expected
    Just (WS.MalformedResponse response _) -> WS.responseCode response == expected
    _ -> False
  _ -> False

assertInitialFullResync :: String -> LBS.ByteString -> IO ()
assertInitialFullResync label bytes = do
  envelope <- either (const (assertFailure (label <> ": invalid event JSON"))) pure
    (Aeson.eitherDecode bytes :: Either String Aeson.Value)
  schema <- textAt ["schema"] envelope
  schema @?= Events.eventsSchema
  generation <- integerAt ["generation"] envelope
  assertBool (label <> ": initial generation must be positive") (generation > 0)
  asOfKind <- textAt ["as_of", "kind"] envelope
  case asOfKind of
    "commit" -> do
      oid <- textAt ["as_of", "oid"] envelope
      assertBool (label <> ": commit as_of needs an OID") (not (Text.null oid))
    "unavailable" -> do
      reason <- textAt ["as_of", "reason"] envelope
      assertBool (label <> ": unavailable as_of needs a reason") (not (Text.null reason))
    _ -> assertFailure (label <> ": invalid as_of kind")
  let full = Events.eventEnvelopeJson
        (Events.EventEnvelope 1 (Events.EventAsOfUnavailable "expected")
          (Events.RepositoryInvalidated [minBound .. maxBound]))
  expectedEvent <- valueAt ["event"] full
  actualEvent <- valueAt ["event"] envelope
  assertBool (label <> ": initial event must invalidate every fact") (actualEvent == expectedEvent)

describeWebSocketClientOutcome :: Maybe (Either SomeException value) -> String
describeWebSocketClientOutcome = \case
  Nothing -> "owned client deadline expired"
  Just (Left failure) -> describeWebSocketClientFailure failure
  Just (Right _) -> "completed"

describeWebSocketClientFailure :: SomeException -> String
describeWebSocketClientFailure failure = case fromException failure :: Maybe WS.HandshakeException of
  Just (WS.RequestRejected _ response) -> "upgrade rejected with HTTP " <> show (WS.responseCode response)
  Just (WS.MalformedResponse response _) -> "malformed upgrade response with HTTP " <> show (WS.responseCode response)
  _ -> case fromException failure :: Maybe WS.ConnectionException of
    Just (WS.CloseRequest code _) -> "server close frame with code " <> show code
    Just WS.ConnectionClosed -> "connection closed without a close frame"
    _ -> case fromException failure :: Maybe IOError of
      Just networkFailure -> "socket I/O failure: " <> show networkFailure
      Nothing -> "client callback exception"

requestPrefix :: Text -> Text -> IO BS.ByteString
requestPrefix host bytes = do
  port <- either assertFailure pure (authorityPort host)
  outcome <- runOwnedSocket 3000000 port $ \client -> do
    sendAll client (TextEncoding.encodeUtf8 bytes)
    recv client 8192
  ownedResult "HTTP response prefix" outcome

ownedResult :: String -> Maybe (Either SomeException value) -> IO value
ownedResult label = \case
  Nothing -> assertFailure (label <> " timed out")
  Just (Left failure) -> assertFailure (label <> " failed: " <> show failure)
  Just (Right value) -> pure value

responseStatus :: BS.ByteString -> IO Int
responseStatus response = case words (takeWhile (/= '\r') (BS8.unpack response)) of
  _ : raw : _ -> case reads raw of [(value, "")] -> pure value; _ -> assertFailure "invalid HTTP status"
  _ -> assertFailure "missing HTTP status"

decodeBody :: BS.ByteString -> IO Aeson.Value
decodeBody response =
  let (headers, suffix) = BS.breakSubstring "\r\n\r\n" response
      encoded = BS.drop 4 suffix
      body = if "Transfer-Encoding: chunked" `BS.isInfixOf` headers then unchunk encoded else encoded
   in maybe (assertFailure ("invalid JSON response: " <> show body)) pure (Aeson.decodeStrict' body)

unchunk :: BS.ByteString -> BS.ByteString
unchunk bytes = BS.concat (go bytes)
  where
    go input =
      let (rawSize, rest0) = BS.breakSubstring "\r\n" input
          rest = BS.drop 2 rest0
       in case readHex (BS8.unpack rawSize) of
            [(0, "")] -> []
            [(size, "")] -> BS.take size rest : go (BS.drop (size + 2) rest)
            _ -> []

authorityPort :: Text -> Either String Int
authorityPort host = case reads (Text.unpack (snd (Text.breakOnEnd ":" host))) of
  [(value, "")] -> Right value
  _ -> Left "bound authority did not contain a decimal port"
