{-# LANGUAGE OverloadedStrings #-}
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
    newApplicationRuntime,
    webApplication,
  )
import qualified Adrai.Web.Security as Security
import Adrai.Web.Socket (EventsTransport, unavailableEventsTransport)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, race, waitCatch)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception
  ( SomeAsyncException,
    SomeException,
    bracket,
    bracketOnError,
    displayException,
    fromException,
    throwIO,
    try,
  )
import Crypto.Random (getRandomBytes)
import Adrai.Provenance (encodeBase64Url, sha256Digest)
import Adrai.Types (digestBytes)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.Socket
  ( Family (AF_INET),
    PortNumber,
    SockAddr (SockAddrInet),
    Socket,
    SocketOption (ReuseAddr),
    SocketType (Stream),
    bind,
    close,
    defaultProtocol,
    getSocketName,
    listen,
    maxListenQueue,
    setSocketOption,
    socket,
    tupleToHostAddress,
  )
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettingsSocket,
    setBeforeMainLoop,
    setGracefulShutdownTimeout,
    setHost,
    setOnException,
    setTimeout,
  )
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Process (proc, terminateProcess, waitForProcess, withCreateProcess)

data ServerDependencies = ServerDependencies
  { serverEntropy :: IO ByteString,
    serverOpenBrowser :: Text -> IO (Either Text ()),
    serverReady :: RunningServer -> IO (),
    serverStopping :: IO (),
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

defaultServerDependencies :: ServerDependencies
defaultServerDependencies =
  ServerDependencies
    { serverEntropy = getRandomBytes 32,
      serverOpenBrowser = openBrowser,
      serverReady = \running -> putStrLn (Text.unpack ("ADRAI web ready at " <> runningBootstrapUrl running)) >> hFlush stdout,
      serverStopping = pure (),
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
              ready <- newEmptyMVar
              let settings =
                    setHost "127.0.0.1"
                      . setTimeout 15
                      . setGracefulShutdownTimeout (Just 5)
                      . setOnException (\_ _ -> pure ())
                      . setBeforeMainLoop (putMVar ready ())
                      $ defaultSettings
              bracket
                (async (runSettingsSocket settings listener (webApplication runtime)))
                (stopWorker dependencies)
                (\worker -> do
                    readyResult <- race (waitCatch worker) (race (threadDelay 5000000) (takeMVar ready))
                    case readyResult of
                      Left (Left exception) -> throwIO exception
                      Left (Right ()) -> ioError (userError "web server stopped before becoming ready")
                      Right (Left ()) -> ioError (userError "web server readiness timed out")
                      Right (Right ()) -> do
                        let running = RunningServer authority (Security.authorityOrigin authority <> "/?token=" <> Security.processSecretText secret)
                        serverReady dependencies running
                        consume running worker)

stopWorker :: ServerDependencies -> Async () -> IO ()
stopWorker dependencies worker = do
  serverStopping dependencies
  cancel worker
  _ <- waitCatch worker
  pure ()

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
