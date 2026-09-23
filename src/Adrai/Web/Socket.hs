{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Authenticated bounded WebSocket transport for @adrai/events/v1@.
module Adrai.Web.Socket
  ( EventsTransport (..), unavailableEventsTransport,
    SocketRuntime, newSocketRuntime, eventsServerApplication,
  ) where

import qualified Adrai.Web.Api as Api
import qualified Adrai.Web.Events as Events
import qualified Adrai.Web.Security as Security
import qualified Adrai.Web.Watch as Watch
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (asyncWithUnmask, cancel, waitCatch, waitEitherCatch)
import Control.Concurrent.STM
import Control.Exception (bracket, finally, mask, onException, throwIO)
import Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextError
import qualified Network.HTTP.Types.Header as Header
import Network.Wai (Request, Response)
import qualified Network.WebSockets as WS

newtype EventsTransport = EventsTransport { runEventsTransport :: Request -> Api.ResponseMetadata -> IO (Maybe Response) }
unavailableEventsTransport :: EventsTransport
unavailableEventsTransport = EventsTransport (\_ _ -> pure Nothing)

data SocketRuntime = SocketRuntime
  { socketAuthority :: Security.BoundAuthority,
    socketSecret :: Security.ProcessSecret,
    socketCoordinator :: Events.EventCoordinator,
    socketActiveFiles :: Watch.ActiveFileRegistry,
    socketSubscribe :: IO (Either Text Events.EventSubscriber),
    socketOnSendDeadline :: IO (),
    socketPending :: TVar Int
  }

newSocketRuntime :: Security.BoundAuthority -> Security.ProcessSecret -> Events.EventCoordinator -> Watch.ActiveFileRegistry -> IO (Either Text Events.EventSubscriber) -> IO () -> IO SocketRuntime
newSocketRuntime authority secret coordinator active subscribe onSendDeadline =
  SocketRuntime authority secret coordinator active subscribe onSendDeadline <$> newTVarIO 0

eventsServerApplication :: SocketRuntime -> IO () -> WS.ServerApp
eventsServerApplication runtime abort pending = do
  let request = WS.pendingRequest pending
  case admitHandshake runtime request of
    Left _ -> WS.rejectRequest pending "WebSocket admission rejected"
    Right () -> bracket (acquirePending runtime) (releasePending runtime) $ \admitted ->
      if not admitted then WS.rejectRequest pending "WebSocket client limit reached" else do
        connection <- WS.acceptRequest pending
        first <- receiveBeforeDeadline abort connection Security.websocketAuthTimeoutMicros
        case first >>= textMessage of
          Nothing -> pure ()
          Just bytes ->
            case Events.decodeClientFrame Security.websocketAuthFrameBytes bytes of
              Right (Events.AuthenticateFrame credential)
                | Security.credentialMatches (socketSecret runtime) (Security.mkWebSocketCredential credential) -> authenticated runtime abort connection
              _ -> boundedClose abort connection "authentication rejected"

authenticated :: SocketRuntime -> IO () -> WS.Connection -> IO ()
authenticated runtime abort connection =
  bracket (Watch.registerActiveClient registry) releaseActive $ \case
    Left _ -> boundedClose abort connection "client limit reached"
    Right activeClient ->
      bracket (socketSubscribe runtime) releaseSubscriber $ \case
        Left "generation-exhausted" -> boundedClose abort connection generationExhaustedCloseReason
        Left _ -> boundedClose abort connection "snapshot unavailable"
        Right subscriber -> runOwnedSession abort (senderLoop subscriber) (WS.withPingThread connection 30 (pure ()) (receiveLoop activeClient))
  where
    registry = socketActiveFiles runtime
    releaseActive (Left _) = pure ()
    releaseActive (Right client) = Watch.unregisterActiveClient registry client
    releaseSubscriber (Left _) = pure ()
    releaseSubscriber (Right subscriber) = Events.unregisterSubscriber (socketCoordinator runtime) subscriber
    senderLoop subscriber = Events.readSubscriberEvent subscriber >>= \case
      Events.SubscriberGenerationExhausted -> boundedClose abort connection generationExhaustedCloseReason
      Events.SubscriberOverflow -> boundedClose abort connection "event queue overflow"
      Events.SubscriberEvent envelope -> do
        sent <- runBeforeDeadlineWithTimeout abort (socketOnSendDeadline runtime) socketSendTimeoutMicros (WS.sendTextData connection (Aeson.encode (Events.eventEnvelopeJson envelope)))
        case sent of
          Nothing -> pure ()
          Just () -> senderLoop subscriber
    receiveLoop activeClient = do
      received <- receiveBeforeDeadline abort connection socketIdleTimeoutMicros
      case received of
        Nothing -> pure ()
        Just message -> case textMessage message >>= either (const Nothing) Just . Events.decodeClientFrame Security.websocketAuthFrameBytes of
          Just (Events.ActiveFilesFrame paths) -> do
            replaced <- Watch.replaceActiveFiles registry activeClient paths
            case replaced of
              Left _ -> boundedClose abort connection "invalid active files"
              Right () -> receiveLoop activeClient
          _ -> boundedClose abort connection "invalid or idle control stream"

receiveBeforeDeadline :: IO () -> WS.Connection -> Int -> IO (Maybe WS.DataMessage)
receiveBeforeDeadline abort connection deadlineMicros =
  runBeforeDeadline abort deadlineMicros (WS.receiveDataMessage connection)

runBeforeDeadline :: IO () -> Int -> IO value -> IO (Maybe value)
runBeforeDeadline abort = runBeforeDeadlineWithTimeout abort (pure ())

runBeforeDeadlineWithTimeout :: IO () -> IO () -> Int -> IO value -> IO (Maybe value)
runBeforeDeadlineWithTimeout abort onTimeout deadlineMicros action = mask $ \restore -> do
  expired <- newTVarIO False
  watchdog <- asyncWithUnmask $ \unmask ->
    unmask (threadDelay deadlineMicros >> atomically (writeTVar expired True) >> abort >> onTimeout)
  message <- (restore action `onException` abort) `finally` do
    didExpire <- readTVarIO expired
    if didExpire then pure () else cancel watchdog
    void (waitCatch watchdog)
  timedOut <- readTVarIO expired
  pure (if timedOut then Nothing else Just message)

runOwnedSession :: IO () -> IO () -> IO () -> IO ()
runOwnedSession abort senderAction receiverAction = mask $ \restore -> do
  sender <- asyncWithUnmask (\unmask -> unmask senderAction)
  receiver <- asyncWithUnmask (\unmask -> unmask receiverAction)
    `onException` (abort >> cancel sender)
  let cleanup = do
        abort
        cancel sender
        cancel receiver
        void (waitCatch sender)
        void (waitCatch receiver)
  outcome <- restore (waitEitherCatch sender receiver) `finally` cleanup
  case outcome of
    Left (Left failure) -> throwIO failure
    Right (Left failure) -> throwIO failure
    _ -> pure ()

acquirePending :: SocketRuntime -> IO Bool
acquirePending runtime = atomically $ do
  current <- readTVar (socketPending runtime)
  if current >= maximumPendingClients then pure False else writeTVar (socketPending runtime) (current + 1) >> pure True

releasePending :: SocketRuntime -> Bool -> IO ()
releasePending _ False = pure ()
releasePending runtime True = atomically (modifyTVar' (socketPending runtime) (subtract 1))

boundedClose :: IO () -> WS.Connection -> Text -> IO ()
boundedClose abort connection reason = void (runBeforeDeadline abort socketCloseTimeoutMicros (WS.sendClose connection reason))

generationExhaustedCloseReason :: Text
generationExhaustedCloseReason = "generation-exhausted; restart the web server"

maximumPendingClients, socketSendTimeoutMicros, socketCloseTimeoutMicros, socketIdleTimeoutMicros :: Int
maximumPendingClients = 16
socketSendTimeoutMicros = 5000000
socketCloseTimeoutMicros = 1000000
socketIdleTimeoutMicros = 60000000

textMessage :: WS.DataMessage -> Maybe BS.ByteString
textMessage = \case
  WS.Text bytes _ | LBS.length bytes <= fromIntegral Security.websocketAuthFrameBytes -> Just (LBS.toStrict bytes)
  _ -> Nothing

admitHandshake :: SocketRuntime -> WS.RequestHead -> Either Security.SecurityError ()
admitHandshake runtime request = do
  if WS.requestPath request == "/api/v1/events" then Right () else Left Security.InvalidMethodForTarget
  let headers = WS.requestHeaders request
      values name = [decode value | (actual, value) <- headers, actual == name]
      cookies = concatMap parseCookies (values Header.hCookie)
      securityRequest = Security.SecurityRequest
        { Security.securityMethod = Security.SecurityGet,
          Security.securityTarget = Security.WebSocketUpgradeTarget,
          Security.securityHosts = values Header.hHost,
          Security.securityOrigins = values Header.hOrigin,
          Security.securityAuthorization = values Header.hAuthorization,
          Security.securityCookies = cookies,
          Security.securityQuery = [],
          Security.securityContentTypes = [] }
  void (Security.admitRequest (socketAuthority runtime) (socketSecret runtime) securityRequest)
  where
    decode = TextEncoding.decodeUtf8With TextError.lenientDecode
    parseCookies value =
      [ (Text.strip name, Text.drop 1 remainder)
      | part <- Text.splitOn ";" value,
        let (name, remainder) = Text.breakOn "=" part,
        not (Text.null name), not (Text.null remainder) ]
