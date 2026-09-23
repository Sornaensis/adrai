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
import Adrai.Git (Repository (..), RevisionSpec (RevisionSpec), discoverRepository, resolveRevision, systemGit)
import Adrai.History (revisionRequested, revisionResolved)
import Adrai.Provenance (gitOidText)
import Adrai.Provenance.Git.Lock (withGitLock)
import qualified Adrai.Service.Compilation as Compilation
import qualified Adrai.Service.Mutation as Mutation
import qualified Adrai.Service.Query as Query
import qualified Adrai.Query as DomainQuery
import qualified Adrai.Service.Runtime as Runtime
import Adrai.Types (ViewMode (CollapsedView))
import qualified Adrai.Types as Types
import qualified Adrai.Web.Events as Events
import Adrai.Web.Server (RunningServer (..), ServerDependencies (..), withWebServer)
import qualified Adrai.Web.Security as Security
import Adrai.Web.Socket (unavailableEventsTransport)
import qualified Adrai.Web.Watch as Watch
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay, tryPutMVar)
import Control.Concurrent.Async (Async, async, cancel, poll, race, wait, waitCatch)
import Control.Exception (SomeAsyncException, SomeException, bracket, bracketOnError, finally, fromException, throwIO, try)
import Control.Monad (forM, forM_, void)
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
import Network.Socket
  ( Family (AF_INET), PortNumber, ShutdownCmd (ShutdownBoth), SockAddr (SockAddrInet), Socket, SocketOption (RecvBuffer, ReuseAddr), SocketType (Stream),
    bind, close, connect, defaultProtocol, getSocketName, setSocketOption, shutdown, socket, tupleToHostAddress )
import Network.Socket.ByteString (recv, sendAll)
import qualified Network.WebSockets as WS
import System.Directory (Permissions (writable), copyFile, createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, getPermissions, listDirectory, removeDirectory, removeDirectoryRecursive, removeFile, renameDirectory, setPermissions)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), StdStream (CreatePipe), callProcess, createProcess, proc, readCreateProcessWithExitCode, readProcess, shell, terminateProcess, waitForProcess)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hGetLine)
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
    let connected = runOwnedWebSocketClient 8000000 port headers $ \connection -> do
          WS.sendTextData connection authenticate
          initial <- timeout 2000000 (WS.receiveData connection :: IO LBS.ByteString)
          assertBool "existing authenticated subscriber received initial resync" (maybe False (BS.isInfixOf "repository-invalidated" . LBS.toStrict) initial)
          putMVar socketReady ()
          closed <- timeout 3000000 (trySynchronous (WS.receiveDataMessage connection))
          assertBool "terminal exhaustion closes existing subscriber with restart reason" (expectedClose "generation-exhausted; restart the web server" closed)
    bracket (async connected) (\worker -> cancel worker >> void (waitCatch worker)) $ \socketWorker -> do
      readySocket <- timeout 3000000 (takeMVar socketReady)
      assertBool "existing socket authenticated before mutation publication" (maybe False (const True) readySocket)
      committed <- postJson running "/api/v1/adrs" (createBody basis)
      assertCommitted committed
      after <- gitHead root
      assertBool "the actual commit survived exhausted publication" (after /= before)
      textAt ["data", "commit"] committed >>= (@?= after)
      textAt ["data", "publication_warning"] committed >>= (@?= "commit generation publication failed after the durable commit")
      textAt ["metadata", "as_of", "kind"] committed >>= (@?= "unavailable")
      socketEnded <- timeout 5000000 (waitCatch socketWorker)
      assertBool "existing socket and owned worker finish after terminal close" (maybe False (either (const False) (maybe False (either (const False) (const True)))) socketEnded)
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

testEventsUnavailable :: IO ()
testEventsUnavailable = withServer $ \running _ -> do
  response <- bearerRequest running "GET /api/v1/events" [("Origin", Security.authorityOrigin (runningAuthority running))]
  assertBool "events are explicitly unavailable" ("HTTP/1.1 503" `BS.isPrefixOf` response)
  assertBool "metadata is unavailable, not an ambient commit" ("X-Adrai-As-Of: unavailable:events-transport-unavailable" `BS.isInfixOf` response)

testQueryRoutes :: IO ()
testQueryRoutes = withSeededServer $ \root running -> do
  basis <- repositoryBasis running
  created <- postJson running "/api/v1/adrs" (createBody basis)
  adr <- textAt ["data", "adr"] created
  current <- textAt ["metadata", "as_of", "oid"] created
  repository <- discoverRepository systemGit root >>= either (assertFailure . show) pure
  sharedShown <- Query.runWebShow repository (Query.ShowRequest adr CollapsedView current False) >>= either (assertFailure . Text.unpack . Query.showFailureText) pure
  httpShown <- getJson running ("/api/v1/adrs/" <> adr <> "?at=" <> current)
  valueAt ["data"] httpShown >>= (@?= Api.apiResultPayload (Api.ApiShowResult sharedShown))
  cliShown <- Query.runShow repository (Query.ShowRequest adr CollapsedView current False) >>= either (assertFailure . Text.unpack . Query.showFailureText) pure
  assertBool "web inspection retains rich candidate detail without changing CLI compact show" (Api.apiResultPayload (Api.ApiShowResult sharedShown) /= Api.apiResultPayload (Api.ApiShowResult cliShown))
  defaultRelevant <- getJson running "/api/v1/relevant?file=seed.txt"
  namedRelevant <- getJson running "/api/v1/relevant?file=seed.txt&at=main"
  explicitRelevant <- getJson running ("/api/v1/relevant?file=seed.txt&at=" <> current)
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
      readAfterMove route = do
        settled <- timeout 3000000 (retryBusyRead route)
        assertBool "exact read stayed busy after the external HEAD move" (settled == Just ())
      retryBusyRead route = do
        (status, response) <- getJsonStatus running route
        case status of
          200 -> assertCommitMetadata current response
          503 -> do
            textAt ["error", "category"] response >>= (@?= "service-failure")
            valueAt ["error", "status"] response >>= (@?= Aeson.Number 503)
            textAt ["error", "code"] response >>= (@?= "repository-busy")
            textAt ["metadata", "as_of", "kind"] response >>= (@?= "unavailable")
            threadDelay 50000
            retryBusyRead route
          _ -> assertFailure ("exact read returned unexpected HTTP status " <> show status)
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
  let compileExact repository oid = do
        shouldBlock <- atomicModifyIORef' blockNextArchive (\armed -> (False, armed))
        if shouldBlock then do
          let archive = root </> ".adrai" </> "cache" </> Text.unpack (gitOidText oid) <> ".sqlite"
          createDirectoryIfMissing True (root </> ".adrai" </> "cache")
          createDirectory archive
          writeIORef blockedArchive (Just archive)
        else pure ()
        Runtime.ensureExactArchive repository oid
      services = defaultApplicationServices {applicationCompileExact = compileExact}
      injected = dependencies {serverApplicationServices = services}
  started <- withWebServer injected root (Api.WebOptions Nothing False) $ \running _ ->
    testMutationRoutesOnServer root running blockNextArchive blockedArchive
  either (assertFailure . Text.unpack) pure started

testMutationRoutesOnServer :: FilePath -> RunningServer -> IORef Bool -> IORef (Maybe FilePath) -> IO ()
testMutationRoutesOnServer root running blockNextArchive blockedArchive = do
  basis0 <- repositoryBasis running
  created <- postJson running "/api/v1/adrs" (createBody basis0)
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
    let delayedDispatch compilation compileExact afterJoin fallback allocate afterResolve publisher repo requestValue = do
          outcome <- dispatchApplicationRequest defaultApplicationServices compilation compileExact afterJoin fallback allocate afterResolve publisher repo requestValue
          case (requestValue, outcome) of
            (Api.ApiCreateRequest _ _, Right _) -> putMVar committed () >> takeMVar release
            _ -> pure ()
          pure outcome
        services = defaultApplicationServices {dispatchApplicationRequest = delayedDispatch}
        injected = dependencies {serverApplicationServices = services}
    started <- withWebServer injected orderedRoot (Api.WebOptions Nothing False) $ \ordered _ -> do
      basis <- repositoryBasis ordered
      delayed <- async (postJson ordered "/api/v1/adrs" (createBody basis))
      takeMVar committed
      observed <- getJson ordered "/api/v1/repository"
      putMVar release ()
      mutation <- wait delayed
      mutationGeneration <- integerAt ["metadata", "generation"] mutation
      observedGeneration <- integerAt ["metadata", "generation"] observed
      assertBool "a delayed mutation response retains its commit-time generation" (mutationGeneration < observedGeneration)
      mutationOid <- textAt ["metadata", "as_of", "oid"] mutation
      observedOid <- textAt ["data", "head"] observed
      mutationOid @?= observedOid
    either (assertFailure . Text.unpack) pure started
  withSeededRepository $ \lockedRoot -> do
    reached <- newEmptyMVar
    release <- newEmptyMVar
    let services = defaultApplicationServices
          { applicationBeforeCommitPublication = \_ -> putMVar reached () >> takeMVar release }
        injected = dependencies {serverApplicationServices = services}
    started <- withWebServer injected lockedRoot (Api.WebOptions Nothing False) $ \locked _ -> do
      basis <- repositoryBasis locked
      delayed <- async (postJson locked "/api/v1/adrs" (createBody basis))
      takeMVar reached
      (busyStatus, busy) <- getJsonStatus locked "/api/v1/repository"
      busyStatus @?= 503
      textAt ["metadata", "as_of", "kind"] busy >>= (@?= "unavailable")
      callProcess "git" ["-C", lockedRoot, "commit", "--allow-empty", "-m", "external move during commit publication"]
      externalHead <- gitHead lockedRoot
      putMVar release ()
      mutation <- wait delayed
      assertCommitted mutation
      mutationOid <- textAt ["metadata", "as_of", "oid"] mutation
      assertBool "the mutation retains its own committed OID after an external advance" (mutationOid /= externalHead)
      retry <- getJson locked "/api/v1/repository"
      mutationGeneration <- integerAt ["metadata", "generation"] mutation
      retryGeneration <- integerAt ["metadata", "generation"] retry
      assertBool "the observation after lock release has a later generation" (retryGeneration > mutationGeneration)
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
    let services = defaultApplicationServices
          { applicationBeforeCommitPublication = \_ -> ioError (userError "injected publication failure") }
        injected = dependencies {serverApplicationServices = services}
    started <- withWebServer injected failureRoot (Api.WebOptions Nothing False) $ \failureServer _ -> do
      basis <- repositoryBasis failureServer
      before <- gitHead failureRoot
      committed <- postJson failureServer "/api/v1/adrs" (createBody basis)
      assertCommitted committed
      after <- gitHead failureRoot
      assertBool "synchronous publication failure cannot roll back the durable commit" (after /= before)
      textAt ["data", "commit"] committed >>= (@?= after)
      textAt ["data", "publication_warning"] committed >>= (@?= "commit generation publication failed after the durable commit")
      textAt ["metadata", "as_of", "kind"] committed >>= (@?= "unavailable")
    either (assertFailure . Text.unpack) pure started

testBoundsAndStale :: IO ()
testBoundsAndStale = withSeededServer $ \root running -> do
  huge <- bearerRequest running ("GET /app.css?" <> Text.replicate 5000 "x") []
  assertBool "encoded query bound applies to assets" ("HTTP/1.1 413" `BS.isPrefixOf` huge)
  basis0 <- repositoryBasis running
  created <- postJson running "/api/v1/adrs" (createBody basis0)
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
  staleCreate <- postJsonStatus running "/api/v1/adrs" (createBody basis)
  staleExisting <- sequence
    [ postJsonStatus running ("/api/v1/adrs/" <> adr <> "/amend") (existingBody basis state ["change_summary" Aeson..= ("stale" :: Text), "title" Aeson..= ("No" :: Text), "summary" Aeson..= ("No" :: Text), "body" Aeson..= ("No\n" :: Text)]),
      postJsonStatus running ("/api/v1/adrs/" <> adr <> "/scope") (existingBody basis state ["reason" Aeson..= ("stale" :: Text), "mode" Aeson..= ("delta" :: Text), "add" Aeson..= (["stale/**"] :: [Text]), "remove" Aeson..= ([] :: [Text])]),
      postJsonStatus running ("/api/v1/adrs/" <> adr <> "/domain") (existingBody basis state ["reason" Aeson..= ("stale" :: Text), "mode" Aeson..= ("delta" :: Text), "add" Aeson..= (["stale"] :: [Text]), "remove" Aeson..= ([] :: [Text])]),
      postJsonStatus running ("/api/v1/adrs/" <> adr <> "/obsolete") (existingBody basis state ["reason" Aeson..= ("stale" :: Text)]),
      postJsonStatus running ("/api/v1/adrs/" <> adr <> "/reactivate") (existingBody basis state ["reason" Aeson..= ("stale" :: Text)])
    ]
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
  exerciseBuiltWeb executable root

assertBuiltRejected :: FilePath -> FilePath -> [String] -> IO ()
assertBuiltRejected executable cwdPath arguments = do
  (exitCode, _, _) <- readCreateProcessWithExitCode ((proc executable arguments) {cwd = Just cwdPath}) ""
  assertBool ("built web command unexpectedly accepted " <> show arguments <> " in " <> cwdPath) (exitCode /= ExitSuccess)

exerciseBuiltWeb :: FilePath -> FilePath -> IO ()
exerciseBuiltWeb executable root = do
  let config = (proc executable ["web", "--no-open"]) {cwd = Just root, std_out = CreatePipe, std_err = CreatePipe}
  (_, output, _, processHandle) <- createProcess config
  handle <- maybe (assertFailure "web stdout pipe missing") pure output
  ready <- timeout 5000000 (hGetLine handle)
  line <- maybe (terminateProcess processHandle >> waitForProcess processHandle >> assertFailure "built web command readiness timed out") pure ready
  let url = Text.pack (drop 19 line)
      host = Text.takeWhile (/= '/') (Text.drop (Text.length "http://") url)
      token = Text.drop (Text.length "token=") (snd (Text.breakOn "token=" url))
  response <- request host ("GET /api/v1/repository HTTP/1.1\r\nHost: " <> host <> "\r\nAuthorization: Bearer " <> token <> "\r\nConnection: close\r\n\r\n")
  assertBool "built executable serves a request" ("HTTP/1.1 200" `BS.isPrefixOf` response)
  terminateProcess processHandle
  _ <- waitForProcess processHandle
  pure ()

testEventRuntime :: IO ()
testEventRuntime = withSeededServer $ \root running -> do
  let authority = runningAuthority running
      token = bootstrapToken running
      headers =
        [ ("Origin", TextEncoding.encodeUtf8 (Security.authorityOrigin authority)),
          ("Authorization", TextEncoding.encodeUtf8 ("Bearer " <> token))
        ]
      authenticate = Aeson.encode (Aeson.object ["type" Aeson..= ("authenticate" :: Text), "credential" Aeson..= token])
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
  session <- runOwnedWebSocketClient 20000000 port headers $ \connection -> do
      WS.sendTextData connection authenticate
      initial <- timeout 2000000 (WS.receiveData connection :: IO LBS.ByteString)
      frame <- maybe (assertFailure "authenticated socket did not receive its initial resync") pure initial
      assertBool "initial socket frame is a versioned invalidation" ("\"type\":\"repository-invalidated\"" `BS.isInfixOf` LBS.toStrict frame)
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
      closed <- timeout 2000000 (trySynchronous (WS.receiveDataMessage connection))
      assertBool "repeated authentication closes with the invalid-control rejection" (expectedClose "invalid or idle control stream" closed)
  assertBool ("authenticated websocket session terminates within its owner bound: " <> show session) (maybe False (either (const False) (const True)) session)
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
  binaryFrame <- runOwnedWebSocketClient 5000000 port headers $ \connection -> do
      WS.sendTextData connection authenticate
      _ <- WS.receiveDataMessage connection
      WS.sendBinaryData connection ("binary-control" :: BS.ByteString)
      result <- timeout 2000000 (trySynchronous (WS.receiveDataMessage connection))
      assertBool "binary post-auth control receives the invalid-control close" (expectedClose "invalid or idle control stream" result)
  assertBool "binary websocket control session remains bounded" (maybe False (either (const False) (const True)) binaryFrame)
  putStrLn "p7-03-events: negative sessions complete"
  reconnect <- runOwnedWebSocketClient 20000000 port headers $ \connection -> do
      putStrLn "p7-03-events: reconnect admitted"
      WS.sendTextData connection authenticate
      frame <- timeout 2000000 (WS.receiveDataMessage connection)
      assertBool "a reconnect receives a fresh initial resync after prior lease cleanup" (maybe False (const True) frame)
      putStrLn "p7-03-events: reconnect initial received"
      WS.sendClose connection ("test complete" :: Text)
      putStrLn "p7-03-events: reconnect close sent"
  assertBool "reconnect completed after invalid-session cleanup" (maybe False (either (const False) (const True)) reconnect)
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
    readers <- mapM (async . receiveUntilEof) peers
    (`finally` do
        mapM_ closeOwnedSocket peers
        mapM_ waitCatch readers) $ do
      mapM poll readers >>= assertBool "sixteen silent upgraded peers remain connected before the auth deadline" . all (\case Nothing -> True; Just _ -> False)
      seventeenth <- runOwnedWebSocketClient 1500000 port headers (const (pure ()))
      assertBool "the seventeenth pre-auth client is rejected with HTTP 400" (expectedHandshakeStatus 400 seventeenth)
      ended <- timeout 7000000 (mapM waitCatch readers)
      assertBool ("all sixteen silent raw peers receive server EOF before the client guard closes them: " <> show ended)
        (maybe False (all (either (const False) id)) ended)
      let awaitReleasedCapacity = do
            recovered <- runOwnedWebSocketClient 3000000 port headers $ \connection -> do
              WS.sendTextData connection authenticate
              initialFrame <- WS.receiveData connection :: IO LBS.ByteString
              assertBool "a new client authenticates after all timed-out slots are released" ("repository-invalidated" `BS.isInfixOf` LBS.toStrict initialFrame)
              WS.sendClose connection ("cap recovery complete" :: Text)
            case recovered of
              Just (Right ()) -> pure True
              failure | expectedHandshakeStatus 400 failure -> threadDelay 20000 >> awaitReleasedCapacity
              _ -> assertFailure "fresh WebSocket failed for a reason other than bounded HTTP 400 admission"
      released <- timeout 5000000 awaitReleasedCapacity
      assertBool "pending capacity becomes usable after all timed-out peers close" (released == Just True)
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
      port <- either assertFailure pure (authorityPort (Security.authorityHost authority))
      let headers =
            [ ("Origin", TextEncoding.encodeUtf8 (Security.authorityOrigin authority)),
              ("Authorization", TextEncoding.encodeUtf8 ("Bearer " <> token))
            ]
      recovered <- runOwnedWebSocketClient 3000000 port headers $ \connection -> do
        WS.sendTextData connection (Aeson.encode (Aeson.object ["type" Aeson..= ("authenticate" :: Text), "credential" Aeson..= token]))
        initial <- WS.receiveData connection :: IO LBS.ByteString
        assertBool "another live subscriber receives its own resync after the slow peer closes" ("repository-invalidated" `BS.isInfixOf` LBS.toStrict initial)
        WS.sendClose connection ("slow-peer recovery complete" :: Text)
      assertBool "a new authenticated subscriber remains serviceable after the slow peer closes" (maybe False (either (const False) (const True)) recovered)
      reader <- async (receiveUntilEof slowPeer)
      ended <- timeout 2000000 (waitCatch reader)
      case ended of
        Just (Right True) -> pure ()
        other -> do
          closeOwnedSocket slowPeer
          _ <- waitCatch reader
          assertFailure ("the unread live subscriber was not physically closed after its send deadline: " <> show other)
  either (assertFailure . Text.unpack) pure started

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
  ancestorOpened <- newEmptyMVar
  resumeAncestor <- newEmptyMVar
  let afterHandleOpen component = do
        pause <- atomicModifyIORef' pauseAncestor $ \enabled ->
          let fire = enabled && component == "architecture"
           in (enabled && not fire, fire)
        if pause then putMVar ancestorOpened () >> takeMVar resumeAncestor else pure ()
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
  assertBool "the native scanner accepts the largest even UTF-16 byte length" (Watch.nativeNameLengthAcceptedForTest (replicate 32767 'a'))
  assertBool "the native scanner rejects a wrapping UTF-16 byte length" (not (Watch.nativeNameLengthAcceptedForTest (replicate 32768 'a')))
  assertBool "the native scanner counts surrogate pairs as two UTF-16 code units" (not (Watch.nativeNameLengthAcceptedForTest (replicate 16384 '\x1f600')))
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
  attempts <- newIORef (0 :: Int)
  delivered <- newEmptyMVar
  watcher <- Watch.watchRepository observer bound $ \event -> do
    attempt <- atomicModifyIORef' attempts (\value -> let next = value + 1 in (next, next))
    if attempt == 1 then ioError (userError "simulated busy publication") else putMVar delivered event
  BS.writeFile (root </> "seed.txt") "coalesced change one"
  BS.writeFile (root </> "seed.txt") "coalesced change two"
  BS.writeFile (root </> "seed.txt") "coalesced final bytes"
  retried <- timeout 3000000 (takeMVar delivered)
  watcherStopped <- timeout 3000000 (Watch.stopWatching watcher >> Watch.awaitWatcher watcher)
  assertBool "fact watcher workers stop within the owner bound" (maybe False (const True) watcherStopped)
  assertBool "periodic verification retries an unacknowledged publication" (maybe False (const True) retried)
  count <- readIORef attempts
  assertBool "publication was attempted again without another filesystem change" (count >= 2)
  case retried of
    Just (Watch.RepositoryFactsChanged _ eventSnapshot _) -> do
      finalSnapshot <- Watch.repositorySnapshot observer bound
      case (eventSnapshot, finalSnapshot) of
        (Watch.RepositorySnapshot _ eventFacts, Watch.RepositorySnapshot _ finalFacts) ->
          Watch.factsRelevantWorktreeIdentity eventFacts @?= Watch.factsRelevantWorktreeIdentity finalFacts
        _ -> assertFailure "coalesced-hint snapshots failed"
    Just (Watch.RepositoryObservationFailure _ failure) -> assertFailure ("coalesced hints ended in observation failure: " <> show failure)
    Nothing -> pure ()
  terminalAttempts <- newIORef (0 :: Int)
  terminalWatcher <- Watch.watchRepository observer bound $ \_ -> do
    atomicModifyIORef' terminalAttempts (\value -> (value + 1, ()))
    throwIO Events.GenerationExhausted
  (`finally` do
      Watch.stopWatching terminalWatcher
      Watch.awaitWatcher terminalWatcher) $ do
    BS.writeFile (root </> "seed.txt") "terminal generation changes the active relevant fact"
    terminalStop <- timeout 3000000 (Watch.awaitWatcher terminalWatcher)
    assertBool "terminal publication ends verifier and native backend without external shutdown" (maybe False (const True) terminalStop)
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
  (controlExit, _, controlError) <- readCreateProcessWithExitCode (shell ("mklink /J \"" <> controlLink <> "\" \"" <> external <> "\"")) ""
  assertBool ("junction control failed before the held-handle test: " <> controlError) (controlExit == ExitSuccess)
  doesFileExist (controlLink </> "adrai" </> "external-sentinel.txt") >>= assertBool "junction control exposes the external sentinel"
  removeDirectory controlLink
  putStrLn "p7-03-watch: junction control complete"
  writeIORef pauseAncestor True
  nativeAfterSwap <- bracket
    (async (Watch.repositorySnapshot observer bound))
    (\worker -> do _ <- tryPutMVar resumeAncestor (); cancel worker; _ <- waitCatch worker; pure ())
    (\worker -> do
      opened <- timeout 2000000 (takeMVar ancestorOpened)
      assertBool "native scan opened the managed ancestor before the swap" (maybe False (const True) opened)
      renameDirectory architecture ownedArchitecture
      let restoreArchitecture = do
            replacement <- doesDirectoryExist architecture
            original <- doesDirectoryExist ownedArchitecture
            if original then do
              if replacement then removeDirectory architecture else pure ()
              renameDirectory ownedArchitecture architecture
            else pure ()
      (`finally` restoreArchitecture) $ do
        (swapExit, _, swapError) <- readCreateProcessWithExitCode (shell ("mklink /J \"" <> architecture <> "\" \"" <> external <> "\"")) ""
        assertBool ("held-ancestor junction replacement failed: " <> swapError) (swapExit == ExitSuccess)
        doesFileExist (architecture </> "adrai" </> "external-sentinel.txt") >>= assertBool "the pathname was replaced by the external junction while the ancestor handle stayed open"
        putMVar resumeAncestor ()
        nativeOutcome <- timeout 3000000 (waitCatch worker)
        case nativeOutcome of
          Nothing -> assertFailure "handle-relative scan did not finish after the swap latch was released"
          Just (Left exception) -> assertFailure ("handle-relative scan raised: " <> show exception)
          Just (Right snapshot) -> pure snapshot)
  putStrLn "p7-03-watch: native swap complete"
  baselineIdentity <- managedIdentityOf nativeBaseline
  case nativeAfterSwap of
    Watch.RepositorySnapshotFailed _ _ -> pure ()
    Watch.RepositorySnapshot _ facts -> Watch.factsManagedSourceIdentity facts @?= baselineIdentity
  writeIORef pauseAncestor True
  bracket
    (async (Watch.repositorySnapshot observer bound))
    (\worker -> do
        _ <- tryPutMVar resumeAncestor ()
        _ <- timeout 2000000 (cancel worker)
        _ <- timeout 2000000 (waitCatch worker)
        pure ()) $ \cancellationWorker -> do
      cancellationOpened <- timeout 2000000 (takeMVar ancestorOpened)
      assertBool "cancellation scan owns a verified ancestor handle before cancellation" (maybe False (const True) cancellationOpened)
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
  supersedeEnabled <- newIORef False
  supersedeOpened <- newEmptyMVar
  supersedeRelease <- newEmptyMVar
  let afterRuntimeHandle component = do
        pause <- atomicModifyIORef' supersedeEnabled $ \enabled ->
          let fire = enabled && component == "architecture"
           in (enabled && not fire, fire)
        if pause then putMVar supersedeOpened () >> takeMVar supersedeRelease else pure ()
  runtimeObserver <- Watch.observerForRegistryWithHandleHook (applicationActiveFileRegistry runtime) bound afterRuntimeHandle
  subscriber <- Events.registerSubscriberWithInitial (applicationEventCoordinator runtime) (Events.EventAsOfUnavailable "test") >>= either (assertFailure . Text.unpack) pure
  initialEnvelope <- Events.readSubscriberEvent subscriber >>= \case
    Events.SubscriberEvent envelope -> pure envelope
    Events.SubscriberOverflow -> assertFailure "initial runtime watcher subscriber overflowed"
    Events.SubscriberGenerationExhausted -> assertFailure "initial runtime watcher subscriber exhausted its generation"
  publishAttempts <- newIORef ([] :: [String])
  runtimeWatcher <- Watch.watchRepository runtimeObserver bound $ \event -> do
    outcome <- try @SomeException (publishWatcherEvent runtime event)
    atomicModifyIORef' publishAttempts (\observed -> ((either show (const "published") outcome : observed), ()))
    either throwIO pure outcome
  (`finally` do
      stoppedRuntimeWatcher <- timeout 3000000 (Watch.stopWatching runtimeWatcher >> Watch.awaitWatcher runtimeWatcher)
      Events.unregisterSubscriber (applicationEventCoordinator runtime) subscriber
      Watch.unregisterActiveClient (applicationActiveFileRegistry runtime) runtimeClient
      stopApplicationRuntime runtime
      case stoppedRuntimeWatcher of Nothing -> ioError (userError "runtime watcher cleanup timed out"); Just () -> pure ()) $ do
    writeIORef supersedeEnabled True
    BS.writeFile managedDecision "managed scan captured before interest epoch advance"
    supersedeReached <- timeout 2000000 (takeMVar supersedeOpened)
    assertBool "runtime scan reached the deterministic pre-publication epoch barrier" (maybe False (const True) supersedeReached)
    Watch.replaceActiveFiles (applicationActiveFileRegistry runtime) runtimeClient [secondPath] >>= either (assertFailure . Text.unpack) pure
    putMVar supersedeRelease ()
    supersedeRecovered <- timeout 4000000 (Events.readSubscriberEvent subscriber)
    case supersedeRecovered of
      Just (Events.SubscriberEvent envelope) -> do
        expectedOid <- resolveRevision repository (RevisionSpec "HEAD") >>= either (assertFailure . show) pure
        Events.eventAsOf envelope @?= Events.EventAt expectedOid
        case Events.eventPayload envelope of
          Events.RepositoryInvalidated invalidations -> assertBool "superseded scan is retried to the persistent managed fact" (Events.ManagedSourceChanged `elem` invalidations)
          _ -> assertFailure "superseded scan retry emitted an observation failure"
      Just Events.SubscriberOverflow -> assertFailure "superseded scan retry overflowed its subscriber"
      Just Events.SubscriberGenerationExhausted -> assertFailure "superseded scan retry exhausted its generation"
      Nothing -> assertFailure "superseded scan was not retried after the active-file epoch advanced"
    readIORef publishAttempts >>= assertBool "the stale scan was explicitly rejected before a later retry published" . any (Text.isInfixOf "superseded" . Text.toLower . Text.pack)
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
    acquired <- timeout 3000000 (race (takeMVar lockAcquired) (waitCatch lockOwner))
    case acquired of
      Just (Left ()) -> pure ()
      Just (Right (Left exception)) -> assertFailure ("Git-lock owner failed before acquisition: " <> show exception)
      Just (Right (Right ())) -> assertFailure "Git-lock owner ended before acquisition"
      Nothing -> assertFailure "Git-lock acquisition timed out"
    putStrLn "p7-03-watch: git lock acquired"
    BS.writeFile managedDecision "contention change without a second filesystem hint"
    threadDelay 700000
    rejectedAttempts <- readIORef publishAttempts
    assertBool
      ("watcher did not record a real Git-lock rejection while the lock was held: " <> show (reverse rejectedAttempts))
      (any (Text.isInfixOf "lock" . Text.toLower . Text.pack) rejectedAttempts)
    putMVar releaseLock ()
    lockFinished <- timeout 2000000 (waitCatch lockOwner)
    assertBool "Git-lock owner released within the bound" (maybe False (either (const False) (const True)) lockFinished)
    observed <- timeout 4000000 (Events.readSubscriberEvent subscriber)
    expectedOid <- resolveRevision repository (RevisionSpec "HEAD") >>= either (assertFailure . show) pure
    case observed of
      Just (Events.SubscriberEvent envelope) -> do
        Events.eventAsOf envelope @?= Events.EventAt expectedOid
        assertBool "retried watcher publication receives a later coherent generation" (Events.eventGeneration envelope > Events.eventGeneration initialEnvelope)
      Just Events.SubscriberOverflow -> assertFailure "watcher retry subscriber overflowed"
      Just Events.SubscriberGenerationExhausted -> assertFailure "watcher retry subscriber exhausted its generation"
      Nothing -> do
        attemptsObserved <- readIORef publishAttempts
        assertFailure ("watcher event was not retried after Git-lock release; callback attempts=" <> show (reverse attemptsObserved))
    writeIORef publishAttempts []
    withGitLock repository (pure ())
    threadDelay 750000
    readIORef publishAttempts >>= (@?= [])
    putStrLn "p7-03-watch: lock retry complete"
    Events.setEventGenerationForTest (applicationEventCoordinator runtime) maxBound
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
      BS.writeFile managedDecision "terminal generation while Git lock remains held"
      terminalStop <- timeout 3000000 (Watch.awaitWatcher runtimeWatcher)
      assertBool "terminal watcher finishes and joins its native backend before Git unlock" (maybe False (const True) terminalStop)
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
    linkedObserver <- Watch.observerForRegistry linkedRegistry linkedBound
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
    linkedWatcher <- Watch.watchRepository linkedObserver linkedBound $ \event -> do
      _ <- tryPutMVar sharedPeriodic event
      pure ()
    (`finally` do
        Watch.stopWatching linkedWatcher
        _ <- timeout 3000000 (Watch.awaitWatcher linkedWatcher)
        sharedExists <- doesFileExist linkedCommonRef
        if sharedExists then removeFile linkedCommonRef else pure ()) $ do
      BS.writeFile linkedCommonRef (TextEncoding.encodeUtf8 (headForFacts <> "\n"))
      periodic <- timeout 3000000 (takeMVar sharedPeriodic)
      case periodic of
        Just (Watch.RepositoryFactsChanged _ _ invalidations) ->
          assertBool "periodic verification detects shared metadata outside the linked worktree watch root" (Events.CommonReferencesChanged `elem` invalidations)
        Just other -> assertFailure ("linked periodic observation returned " <> show other)
        Nothing -> assertFailure "linked periodic common-reference change was not delivered"
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
    callProcess "git" ["-C", root, "commit", "--allow-empty", "-m", "second exact revision"]
    _ <- getJson running "/api/v1/doctor"
    readIORef starts >>= (@?= 2)
  either (assertFailure . Text.unpack) pure started
  repository <- discoverRepository systemGit root >>= either (assertFailure . show) pure
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
    overlapStarts <- newIORef (0 :: Int)
    overlapJoined <- newEmptyMVar
    overlapEntered <- newEmptyMVar
    overlapRelease <- newEmptyMVar
    let overlapServices = defaultApplicationServices
          { applicationCompileExact = \repositoryToCompile oid -> do
              count <- atomicModifyIORef' overlapStarts (\value -> let next = value + 1 in (next, next))
              if count == 1 then putMVar overlapEntered () >> takeMVar overlapRelease else pure ()
              Runtime.ensureExactArchive repositoryToCompile oid,
            applicationAfterCompilationJoin = putMVar overlapJoined ()
          }
        overlapDependencies = dependencies {serverApplicationServices = overlapServices}
    overlapStarted <- withWebServer overlapDependencies overlapRoot (Api.WebOptions Nothing False) $ \running _ -> do
      basis <- repositoryBasis running
      mutation <- async (postJson running "/api/v1/adrs" (createBody basis))
      awaitSignal "post-mutation compilation join" overlapJoined mutation
      awaitSignal "post-mutation producer entry" overlapEntered mutation
      query <- async (getJson running "/api/v1/doctor")
      awaitSignal "concurrent query compilation join" overlapJoined query
      putMVar overlapRelease ()
      committed <- wait mutation
      assertCommitted committed
      doctor <- wait query
      valueAt ["data"] doctor >>= \value -> assertBool "query joining post-mutation compilation receives its own projection" (value /= Aeson.Null)
      readIORef overlapStarts >>= (@?= 1)
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
  postJson running ("/api/v1/adrs/" <> adr <> "/" <> operation) $ Aeson.object
    ([ "repository_state" Aeson..= basis,
       "state_token" Aeson..= state,
       "actor" Aeson..= actorValue
     ] <> fields)

getJson :: RunningServer -> Text -> IO Aeson.Value
getJson running path = do
  (status, value) <- getJsonStatus running path
  if status == 200 then pure value else assertFailure ("GET " <> Text.unpack path <> " returned " <> show status <> ": " <> show value)

getJsonStatus :: RunningServer -> Text -> IO (Int, Aeson.Value)
getJsonStatus running path = do
  response <- bearerRequest running ("GET " <> path) []
  status <- responseStatus response
  value <- decodeBody response
  pure (status, value)

postJson :: RunningServer -> Text -> Aeson.Value -> IO Aeson.Value
postJson running path body = postJsonStatus running path body >>= \(status, value) ->
  if status == 200 then pure value else assertFailure ("POST returned HTTP " <> show status <> ": " <> show value)

postJsonStatus :: RunningServer -> Text -> Aeson.Value -> IO (Int, Aeson.Value)
postJsonStatus running path body = do
  let host = Security.authorityHost (runningAuthority running)
      bytes = LBS.toStrict (Aeson.encode body)
  cookie <- sessionCookiePair running
  let
      requestHead = TextEncoding.encodeUtf8
        ("POST " <> path <> " HTTP/1.1\r\nHost: " <> host <> "\r\nOrigin: " <> Security.authorityOrigin (runningAuthority running)
          <> "\r\nAuthorization: Bearer " <> bootstrapToken running <> "\r\nCookie: " <> cookie <> "\r\nContent-Type: application/json\r\nContent-Length: "
          <> Text.pack (show (BS.length bytes)) <> "\r\nConnection: close\r\n\r\n")
  response <- requestRaw host (requestHead <> bytes)
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

assertCommitMetadata :: Text -> Aeson.Value -> IO ()
assertCommitMetadata expected value = do
  kind <- textAt ["metadata", "as_of", "kind"] value
  if kind == "comparison"
    then do
      textAt ["metadata", "as_of", "from"] value >>= (@?= expected)
      textAt ["metadata", "as_of", "to"] value >>= (@?= expected)
    else do
      kind @?= "commit"
      textAt ["metadata", "as_of", "oid"] value >>= (@?= expected)

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
  initial <- recv client 4096
  assertBool "slow raw subscriber receives its initial versioned resync before it stops reading" ("repository-invalidated" `BS.isInfixOf` initial)
  pure client

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
bearerRequest running requestLine extra =
  let host = Security.authorityHost (runningAuthority running)
      token = bootstrapToken running
      headers = Text.concat [name <> ": " <> value <> "\r\n" | (name, value) <- extra]
   in request host (requestLine <> " HTTP/1.1\r\nHost: " <> host <> "\r\nAuthorization: Bearer " <> token <> "\r\n" <> headers <> "Connection: close\r\n\r\n")

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
        closeOwnedSocket client
        joined <- timeout 2000000 (waitCatch worker)
        case joined of
          Nothing -> assertFailure "owned socket worker survived shutdown and the bounded join"
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
requestRaw host bytes = do
  port <- either assertFailure pure (authorityPort host)
  outcome <- runOwnedSocket 3000000 port $ \client -> do
    sendAll client bytes
    receiveAll client []
  ownedResult "HTTP response" outcome
  where
    receiveAll client chunks = do
      let accumulated = BS.concat (reverse chunks)
      if httpResponseComplete accumulated then pure accumulated else do
        received <- try @SomeException (recv client 4096)
        case received of
          Left exception -> if BS.null accumulated then throwIO exception else assertFailure ("HTTP response ended before its declared body: " <> show exception)
          Right chunk -> if BS.null chunk then pure accumulated else receiveAll client (chunk : chunks)

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
