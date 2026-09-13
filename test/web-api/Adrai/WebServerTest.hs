{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Adrai.WebServerTest (tests) where

import qualified Adrai.Web.Api as Api
import Adrai.Web.Application (ApplicationServices (..), defaultApplicationServices)
import Adrai.CliRunner (parseArguments)
import Adrai.Git (discoverRepository, systemGit)
import qualified Adrai.Service.Mutation as Mutation
import qualified Adrai.Service.Query as Query
import Adrai.Types (ViewMode (CollapsedView))
import Adrai.Web.Server (RunningServer (..), ServerDependencies (..), withWebServer)
import qualified Adrai.Web.Security as Security
import Adrai.Web.Socket (unavailableEventsTransport)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (Async, async, wait)
import Control.Exception (bracket, bracketOnError)
import Control.Monad (forM, forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Pair)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef (atomicModifyIORef', newIORef, writeIORef)
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Network.Socket
  ( Family (AF_INET), PortNumber, SockAddr (SockAddrInet), SocketOption (ReuseAddr), SocketType (Stream),
    bind, close, connect, defaultProtocol, getSocketName, setSocketOption, socket, tupleToHostAddress )
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory, removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), StdStream (CreatePipe), callProcess, createProcess, proc, readCreateProcessWithExitCode, readProcess, terminateProcess, waitForProcess)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hGetLine)
import Numeric (readHex)
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

dependencies :: ServerDependencies
dependencies = ServerDependencies
  { serverEntropy = pure (BS.pack [0 .. 31]),
    serverOpenBrowser = const (pure (Right ())),
    serverReady = const (pure ()),
    serverStopping = pure (),
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
  sharedShown <- Query.runShow repository (Query.ShowRequest adr CollapsedView current False) >>= either (assertFailure . Text.unpack . Query.showFailureText) pure
  httpShown <- getJson running ("/api/v1/adrs/" <> adr <> "?at=" <> current)
  valueAt ["data"] httpShown >>= (@?= Api.apiResultPayload (Api.ApiShowResult sharedShown))
  _ <- getJson running ("/api/v1/doctor?at=" <> current)
  let archive = root </> ".adrai" </> "cache" </> Text.unpack current <> ".sqlite"
  bracket (SQLite.open archive) SQLite.close $ \connection ->
    SQLite.execute connection "DELETE FROM meta WHERE key=?" (Only ("managed_source_count" :: Text))
  executable <- lookupEnv "ADRAI_EXE" >>= maybe (assertFailure "ADRAI_EXE is required") pure
  (doctorExit, doctorStdout, _) <- readCreateProcessWithExitCode ((proc executable ["doctor", "--at", Text.unpack current, "--json"]) {cwd = Just root}) ""
  doctorExit @?= ExitSuccess
  cliDoctor <- maybe (assertFailure "CLI doctor did not return JSON") pure (Aeson.decodeStrict' (BS8.pack doctorStdout))
  httpDoctor <- getJson running ("/api/v1/doctor?at=" <> current)
  valueAt ["data"] httpDoctor >>= (@?= cliDoctor)
  bracket (SQLite.open (root </> ".adrai" </> "index.sqlite")) SQLite.close $ \connection -> do
    [Only operationCommits] <- SQLite.query_ connection "SELECT COUNT(*) FROM operation_commit" :: IO [Only Int]
    [Only coverageRows] <- SQLite.query_ connection "SELECT COUNT(*) FROM operation_target_coverage" :: IO [Only Int]
    assertBool "committed overlay rows are visible to the published exact doctor cache" (operationCommits > 0 && coverageRows > 0)
  callProcess "git" ["-C", root, "commit", "--allow-empty", "-m", "move after exact request basis"]
  ambient <- gitHead root
  assertBool "ambient HEAD moved after the exact response basis" (ambient /= current)
  let routes =
        [ "/api/v1/adrs/" <> adr <> "?at=" <> current,
          "/api/v1/history?adr=" <> adr <> "&at=" <> current,
          "/api/v1/compare?from=" <> current <> "&to=" <> current,
          "/api/v1/conflicts?at=" <> current,
          "/api/v1/doctor?at=" <> current,
          "/api/v1/search?q=runtime&at=" <> current,
          "/api/v1/relevant?file=seed.txt&at=" <> current
        ]
  mapM_ (\route -> getJson running route >>= assertCommitMetadata current) routes
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
testMutationRoutes = withSeededServer $ \root running -> do
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
  let currentIndex = root </> ".adrai" </> "index.sqlite"
  exists <- doesFileExist currentIndex
  if exists then removeFile currentIndex else pure ()
  createDirectory currentIndex
  reactivated <- mutateExisting running adr "reactivate" ["reason" Aeson..= ("needed again" :: Text)]
  assertCommitted reactivated
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
    let delayedDispatch fallback allocate afterResolve publisher repo requestValue = do
          outcome <- dispatchApplicationRequest defaultApplicationServices fallback allocate afterResolve publisher repo requestValue
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
  Aeson.Number number -> case (Scientific.floatingOrInteger number :: Either Double Integer) of
    Right integer -> pure integer
    Left _ -> assertFailure ("expected integer at " <> show path)
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

requestRaw :: Text -> BS.ByteString -> IO BS.ByteString
requestRaw host bytes = do
  port <- either assertFailure pure (authorityPort host)
  bracketOnError (socket AF_INET Stream defaultProtocol) close $ \client -> do
    connect client (SockAddrInet (fromIntegral port :: PortNumber) (tupleToHostAddress (127, 0, 0, 1)))
    sendAll client bytes
    response <- timeout 3000000 (receiveAll client [])
    close client
    maybe (assertFailure "HTTP response timed out") pure response
  where
    receiveAll client chunks = recv client 4096 >>= \chunk ->
      if BS.null chunk then pure (BS.concat (reverse chunks)) else receiveAll client (chunk : chunks)

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
