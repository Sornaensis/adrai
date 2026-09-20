{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Loopback listener ownership and executable lifecycle.
module Adrai.Web.Server
  ( ServerDependencies (..),
    RunningServer (..),
    defaultServerDependencies,
    withWebServer,
    runWebServer,
    runWebServerAt,
  )
where

import Adrai.Git (discoverRepository, systemGit)
import qualified Adrai.Web.Api as Api
import Adrai.Web.Application
  ( ApplicationServices,
    defaultApplicationServices,
    applicationActiveFileRegistry,
    applicationEventCoordinator,
    newApplicationRuntime,
    publishWatcherEvent,
    stopApplicationRuntime,
    subscribeApplicationEvents,
    webSocketUpgradeAdmitted,
    webApplication,
  )
import qualified Adrai.Web.Security as Security
import qualified Adrai.Web.Events as Events
import Adrai.Web.Socket (EventsTransport, eventsServerApplication, newSocketRuntime, unavailableEventsTransport)
import Adrai.Web.Watch (awaitWatcher, observerForRegistry, stopWatching, watchRepository)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, race, waitCatch)
import Control.Concurrent.MVar (MVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, tryPutMVar)
import Control.Concurrent.STM
import Control.Exception
  ( SomeAsyncException,
    SomeException,
    bracket,
    bracketOnError,
    displayException,
    finally,
    fromException,
    mask,
    onException,
    throwIO,
    try,
  )
import Control.Monad (unless, void)
import Crypto.Random (getRandomBytes)
import Adrai.Provenance (encodeBase64Url, sha256Digest)
import Adrai.Types (digestBytes)
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.IORef (readIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.Socket
  ( Family (AF_INET),
    PortNumber,
    SockAddr (SockAddrInet),
    Socket,
    SocketOption (NoDelay, ReuseAddr),
    SocketType (Stream),
    ShutdownCmd (ShutdownBoth),
    bind,
    close,
    defaultProtocol,
    getSocketName,
    listen,
    maxListenQueue,
    setSocketOption,
    shutdown,
    socket,
    tupleToHostAddress,
  )
import Network.Wai (Application, remoteHost)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    setBeforeMainLoop,
    setGracefulCloseTimeout1,
    setGracefulCloseTimeout2,
    setGracefulShutdownTimeout,
    setHost,
    setOnException,
    setTimeout,
  )
import qualified Network.Wai.Handler.Warp.Internal as WarpInternal
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WebSockets
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Process (proc, terminateProcess, waitForProcess, withCreateProcess)
import System.Timeout (timeout)

data ServerDependencies = ServerDependencies
  { serverEntropy :: IO ByteString,
    serverOpenBrowser :: Text -> IO (Either Text ()),
    serverReady :: RunningServer -> IO (),
    serverStopping :: IO (),
    serverEventCoordinatorReady :: Events.EventCoordinator -> IO (),
    serverEventSendDeadline :: IO (),
    serverApplicationServices :: ApplicationServices,
    serverEventsTransport :: EventsTransport
  }

data RunningServer = RunningServer
  { runningAuthority :: Security.BoundAuthority,
    runningBootstrapUrl :: Text
  }
  deriving (Eq)

instance Show RunningServer where
  show running = "RunningServer {authority=" <> show (runningAuthority running) <> ", bootstrap=<redacted>}"

data SocketOwner = SocketOwner
  { ownerToken :: Int,
    ownerPeer :: SockAddr,
    ownerSocket :: Socket,
    ownerAborting :: TVar Bool,
    ownerCloseGate :: MVar Bool,
    ownerDisposeBeforeFork :: IO (),
    ownerComplete :: MVar ()
  }

data OwnerState = OwnerState Int Bool (Map.Map Int SocketOwner)
data SocketOwners = SocketOwners (TVar OwnerState) (TQueue SocketOwner)

newSocketOwners :: IO SocketOwners
newSocketOwners = SocketOwners <$> newTVarIO (OwnerState 0 False Map.empty) <*> newTQueueIO

registerSocketOwner :: SocketOwners -> Socket -> SockAddr -> IO () -> IO (Maybe SocketOwner)
registerSocketOwner (SocketOwners state pending) accepted peer freeBuffer = do
  aborting <- newTVarIO False
  closeGate <- newMVar False
  complete <- newEmptyMVar
  atomically $ do
    OwnerState next stopping owners <- readTVar state
    if stopping then pure Nothing else do
      let owner = SocketOwner next peer accepted aborting closeGate (abortSocketOwner owner `finally` freeBuffer) complete
      writeTVar state (OwnerState (next + 1) False (Map.insert next owner owners))
      writeTQueue pending owner
      pure (Just owner)

lookupSocketAbort :: SocketOwners -> SockAddr -> IO (Maybe (IO ()))
lookupSocketAbort (SocketOwners state _) peer = atomically $ do
  OwnerState _ stopping owners <- readTVar state
  let matches = filter ((== peer) . ownerPeer) (Map.elems owners)
  case matches of
    [owner] | not stopping -> do
      aborting <- readTVar (ownerAborting owner)
      pure (if aborting then Nothing else Just (abortSocketOwner owner))
    _ -> pure Nothing

abortSocketOwner :: SocketOwner -> IO ()
abortSocketOwner owner = do
  atomically (writeTVar (ownerAborting owner) True)
  modifyMVar_ (ownerCloseGate owner) $ \closed ->
    if closed then pure True else do
      void (trySynchronous (shutdown (ownerSocket owner) ShutdownBoth))
      void (trySynchronous (close (ownerSocket owner)))
      pure True

finishSocketOwner :: SocketOwners -> SocketOwner -> IO ()
finishSocketOwner (SocketOwners state _) owner = do
  abortSocketOwner owner
  atomically $ modifyTVar' state $ \(OwnerState next stopping owners) ->
    OwnerState next stopping (Map.delete (ownerToken owner) owners)
  void (tryPutMVar (ownerComplete owner) ())

acceptOwnedConnection :: WarpInternal.Settings -> Socket -> SocketOwners -> IO (WarpInternal.Connection, SockAddr)
acceptOwnedConnection settings listener owners = mask $ \restore -> do
  (accepted, peer) <- restore (WarpInternal.settingsAccept settings listener)
  let closeAccepted = do
        void (trySynchronous (shutdown accepted ShutdownBoth))
        void (trySynchronous (close accepted))
  (do
      WarpInternal.setSocketCloseOnExec accepted
      void (trySynchronous (setSocketOption accepted NoDelay 1))
      connection <- WarpInternal.socketConnection settings accepted
      let freeBuffer = readIORef (WarpInternal.connWriteBuffer connection) >>= WarpInternal.bufFree
          disposeBeforeHandoff = WarpInternal.connClose connection `finally` freeBuffer
      bracketOnError (pure connection) (const disposeBeforeHandoff) $ \_ -> do
        registered <- registerSocketOwner owners accepted peer freeBuffer
        case registered of
          Nothing -> ioError (userError "web server is stopping")
          Just owner -> pure (connection {WarpInternal.connClose = abortSocketOwner owner}, peer))
    `onException` closeAccepted

forkOwnedWorker :: SocketOwners -> (((forall a. IO a -> IO a) -> IO ()) -> IO ()) -> ((forall a. IO a -> IO a) -> IO ()) -> IO ()
forkOwnedWorker owners@(SocketOwners _ pending) originalFork worker = mask $ \restore -> do
  owner <- atomically (readTQueue pending)
  let ownedWorker :: (forall a. IO a -> IO a) -> IO ()
      ownedWorker unmask = worker unmask `finally` finishSocketOwner owners owner
  restore (originalFork ownedWorker) `onException`
    (ownerDisposeBeforeFork owner `finally` finishSocketOwner owners owner)

runOwnedWarp :: WarpInternal.Settings -> Socket -> SocketOwners -> Application -> IO ()
runOwnedWarp settings listener owners application = do
  let ownedSettings = settings {WarpInternal.settingsFork = forkOwnedWorker owners (WarpInternal.settingsFork settings)}
  WarpInternal.settingsInstallShutdownHandler ownedSettings (close listener)
  WarpInternal.runSettingsConnection ownedSettings (acceptOwnedConnection ownedSettings listener owners) application

defaultServerDependencies :: ServerDependencies
defaultServerDependencies =
  ServerDependencies
    { serverEntropy = getRandomBytes 32,
      serverOpenBrowser = openBrowser,
      serverReady = \running -> putStrLn (Text.unpack ("ADRAI web ready at " <> runningBootstrapUrl running)) >> hFlush stdout,
      serverStopping = pure (),
      serverEventCoordinatorReady = const (pure ()),
      serverEventSendDeadline = pure (),
      serverApplicationServices = defaultApplicationServices,
      serverEventsTransport = unavailableEventsTransport
    }

runWebServer :: Api.WebOptions -> IO (Either Text ())
runWebServer options = do
  root <- getCurrentDirectory
  runWebServerAt root options

runWebServerAt :: FilePath -> Api.WebOptions -> IO (Either Text ())
runWebServerAt root options = do
  started <- withWebServer defaultServerDependencies root options $ \running worker -> do
    if Api.webOpenBrowser options
      then serverOpenBrowser defaultServerDependencies (runningBootstrapUrl running) >>= either (hPutStrLn stderr . Text.unpack . ("adrai: browser warning: " <>)) pure
      else pure ()
    outcome <- waitCatch worker
    case outcome of
      Left exception -> throwIO exception
      Right () -> pure ()
  pure started

withWebServer :: ServerDependencies -> FilePath -> Api.WebOptions -> (RunningServer -> Async () -> IO value) -> IO (Either Text value)
withWebServer dependencies startDirectory options consume = do
  discovered <- discoverRepository systemGit startDirectory
  case Api.validateRepositoryBinding discovered of
    Left problem -> pure (Left (Text.pack (show problem)))
    Right bound -> do
      entropy <- serverEntropy dependencies
      case Security.mkProcessSecret entropy of
        Left problem -> pure (Left (Text.pack (show problem)))
        Right secret -> withListener (Api.webRequestedPort options) $ \listener actualPort -> do
          let processId = Text.take 16 (encodeBase64Url (digestBytes (sha256Digest entropy)))
          case Security.mkBoundAuthority actualPort processId of
            Left problem -> ioError (userError (show problem))
            Right authority -> do
              runtime <- newApplicationRuntime bound authority secret Api.defaultApiLimits (serverApplicationServices dependencies) (serverEventsTransport dependencies)
              let registry = applicationActiveFileRegistry runtime
                  socketOptions =
                    WebSockets.defaultConnectionOptions
                      { WebSockets.connectionStrictUnicode = True,
                        WebSockets.connectionFramePayloadSizeLimit = WebSockets.SizeLimit (fromIntegral Security.websocketAuthFrameBytes),
                        WebSockets.connectionMessageDataSizeLimit = WebSockets.SizeLimit (fromIntegral Security.websocketAuthFrameBytes)
                      }
              observer <- observerForRegistry registry bound
              socketRuntime <- newSocketRuntime authority secret (applicationEventCoordinator runtime) registry (subscribeApplicationEvents runtime) (serverEventSendDeadline dependencies)
              owners <- newSocketOwners
              let application request respond =
                    if webSocketUpgradeAdmitted runtime request
                      then do
                        matched <- lookupSocketAbort owners (remoteHost request)
                        case matched of
                          Nothing -> webApplication runtime request respond
                          Just abort -> websocketsOr socketOptions (eventsServerApplication socketRuntime abort) (webApplication runtime) request respond
                      else webApplication runtime request respond
              bracket
                (watchRepository observer bound (publishWatcherEvent runtime))
                (\watcher -> stopWatching watcher >> awaitWatcher watcher >> stopApplicationRuntime runtime)
                (\_watcher -> do
                  serverEventCoordinatorReady dependencies (applicationEventCoordinator runtime)
                  ready <- newEmptyMVar
                  let settings =
                        setHost "127.0.0.1"
                          . setTimeout 15
                          . setGracefulCloseTimeout1 0
                          . setGracefulCloseTimeout2 0
                          . setGracefulShutdownTimeout (Just 0)
                          . setOnException (\_ _ -> pure ())
                          . setBeforeMainLoop (putMVar ready ())
                          $ defaultSettings
                  bracket
                    (async (runOwnedWarp settings listener owners application))
                    (stopWorker dependencies listener owners)
                    (\worker -> do
                      readyResult <- race (waitCatch worker) (race (threadDelay 5000000) (takeMVar ready))
                      case readyResult of
                        Left (Left exception) -> throwIO exception
                        Left (Right ()) -> ioError (userError "web server stopped before becoming ready")
                        Right (Left ()) -> ioError (userError "web server readiness timed out")
                        Right (Right ()) -> do
                          let running = RunningServer authority (Security.authorityOrigin authority <> "/?token=" <> Security.processSecretText secret)
                          serverReady dependencies running
                          consume running worker))

stopWorker :: ServerDependencies -> Socket -> SocketOwners -> Async () -> IO ()
stopWorker dependencies listener (SocketOwners state _) worker = do
  owners <- atomically $ do
    OwnerState next _ current <- readTVar state
    writeTVar state (OwnerState next True current)
    pure (Map.elems current)
  void (trySynchronous (close listener))
  mapM_ abortSocketOwner owners
  serverStopping dependencies
  stopped <- timeout 5000000 $ do
    void (waitCatch worker)
    mapM_ (readMVar . ownerComplete) owners
  unless (maybe False (const True) stopped) (ioError (userError "web server workers did not close within the shutdown bound"))

withListener :: Maybe Api.BoundPort -> (Socket -> Int -> IO value) -> IO (Either Text value)
withListener requested use = do
  outcome <- trySynchronous $ bracket acquire close $ \listener -> do
    let port = maybe 0 (fromIntegral . Api.boundPortValue) requested :: PortNumber
    bind listener (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1)))
    listen listener maxListenQueue
    address <- getSocketName listener
    case address of
      SockAddrInet actual _ -> use listener (fromIntegral actual)
      _ -> ioError (userError "loopback listener returned a non-IPv4 address")
  pure (either (Left . Text.pack . displayException) Right outcome)
  where
    acquire = bracketOnError (socket AF_INET Stream defaultProtocol) close $ \listener -> do
      -- Winsock's default exclusive bind semantics are retained explicitly by
      -- disabling address reuse.  Enabling SO_REUSEADDR on Windows permits a
      -- second process to force-bind the same port and makes routing indeterminate.
      setSocketOption listener ReuseAddr 0
      pure listener

openBrowser :: Text -> IO (Either Text ())
openBrowser url = do
  outcome <- trySynchronous $
    withCreateProcess (proc "rundll32.exe" ["url.dll,FileProtocolHandler", Text.unpack url]) $ \_ _ _ handle -> do
      completed <- race (threadDelay 5000000) (waitForProcess handle)
      case completed of
        Left () -> terminateProcess handle >> pure (Left "browser opener timed out")
        Right code -> pure (Right code)
  pure $ case outcome of
    Left _ -> Left "unable to start browser opener"
    Right (Left problem) -> Left problem
    Right (Right ExitSuccess) -> Right ()
    Right (Right code) -> Left ("browser opener exited " <> Text.pack (show code))

trySynchronous :: IO value -> IO (Either SomeException value)
trySynchronous action = do
  result <- try action
  case result of
    Left exception -> case fromException exception of
      Just cancellation -> throwIO (cancellation :: SomeAsyncException)
      Nothing -> pure (Left exception)
    Right value -> pure (Right value)
