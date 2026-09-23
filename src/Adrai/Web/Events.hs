{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Versioned event wire contract and authentication-first socket state.
module Adrai.Web.Events
  ( eventsSchema,
    Invalidation (..),
    RepositoryEvent (..),
    EventAsOf (..),
    EventEnvelope (..),
    eventEnvelopeJson,
    invalidationText,
    ClientFrame (..),
    decodeClientFrame,
    SocketState (..),
    SocketCloseReason (..),
    acceptClientFrame,
    EventProtocolError (..),
    EventCoordinator,
    EventSubscriber,
    newEventCoordinator,
    setEventGenerationForTest,
    GenerationExhausted (..),
    isGenerationExhausted,
    nextEventGeneration,
    publishInvalidation,
    publishInvalidationWhen,
    registerSubscriberWithInitial,
    SubscriberRead (..),
    readSubscriberEvent,
    unregisterSubscriber,
  )
where

import Adrai.Provenance (GitOid, gitOidText)
import Adrai.Types (RepoPath, mkRepoPath)
import Control.Concurrent.STM
import Control.Exception (Exception, throwIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import Data.List (sort)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)

eventsSchema :: Text
eventsSchema = "adrai/events/v1"

data Invalidation
  = RepositoryIdentityChanged
  | HeadChanged
  | IndexChanged
  | SequencerChanged
  | ConfigurationChanged
  | ManagedSourceChanged
  | CommonReferencesChanged
  | PackedReferencesChanged
  | ReflogsChanged
  | WorktreeMetadataChanged
  | RelevantWorktreeFileChanged
  deriving (Eq, Ord, Show, Enum, Bounded)

data RepositoryEvent
  = RepositoryInvalidated [Invalidation]
  | RepositoryObservationFailed Text
  deriving (Eq, Show)

data EventAsOf
  = EventAt GitOid
  | EventAsOfUnavailable Text
  deriving (Eq, Show)

data EventEnvelope = EventEnvelope
  { eventGeneration :: Word64,
    eventAsOf :: EventAsOf,
    eventPayload :: RepositoryEvent
  }
  deriving (Eq, Show)

eventEnvelopeJson :: EventEnvelope -> Aeson.Value
eventEnvelopeJson envelope =
  Aeson.object
    [ "schema" Aeson..= eventsSchema,
      "generation" Aeson..= Text.pack (show (eventGeneration envelope)),
      "as_of" Aeson..= asOfJson (eventAsOf envelope),
      "event" Aeson..= eventJson (eventPayload envelope)
    ]
  where
    asOfJson (EventAt oid) = Aeson.object ["kind" Aeson..= ("commit" :: Text), "oid" Aeson..= gitOidText oid]
    asOfJson (EventAsOfUnavailable reason) = Aeson.object ["kind" Aeson..= ("unavailable" :: Text), "reason" Aeson..= reason]
    eventJson (RepositoryInvalidated facts) =
      Aeson.object ["type" Aeson..= ("repository-invalidated" :: Text), "facts" Aeson..= map invalidationText facts]
    eventJson (RepositoryObservationFailed reason) =
      Aeson.object ["type" Aeson..= ("observation-failed" :: Text), "reason" Aeson..= reason]

invalidationText :: Invalidation -> Text
invalidationText value = case value of
  RepositoryIdentityChanged -> "repository-identity"
  HeadChanged -> "head"
  IndexChanged -> "index"
  SequencerChanged -> "sequencer"
  ConfigurationChanged -> "configuration"
  ManagedSourceChanged -> "managed-source"
  CommonReferencesChanged -> "common-refs"
  PackedReferencesChanged -> "packed-refs"
  ReflogsChanged -> "reflogs"
  WorktreeMetadataChanged -> "worktree-metadata"
  RelevantWorktreeFileChanged -> "relevant-worktree-file"

data ClientFrame = AuthenticateFrame Text | ActiveFilesFrame [RepoPath]
  deriving (Eq)

instance Show ClientFrame where
  show (AuthenticateFrame _) = "AuthenticateFrame <redacted>"
  show (ActiveFilesFrame paths) = "ActiveFilesFrame " <> show (length paths) <> " paths"

data EventProtocolError
  = FrameTooLarge
  | MalformedFrame
  | UnknownFrameFields [Text]
  | InvalidAuthenticationFrame
  | InvalidActiveFilesFrame
  deriving (Eq, Show)

decodeClientFrame :: Int -> ByteString -> Either EventProtocolError ClientFrame
decodeClientFrame maximumBytes bytes
  | ByteString.length bytes > maximumBytes = Left FrameTooLarge
  | otherwise = do
      duplicate <- either (const (Left MalformedFrame)) Right (hasDuplicateTopLevelKeys bytes)
      if duplicate then Left MalformedFrame else Right ()
      value <- either (const (Left MalformedFrame)) Right (Aeson.eitherDecodeStrict' bytes)
      object <- case value of
        Aeson.Object fields -> Right fields
        _ -> Left InvalidAuthenticationFrame
      frameType <- textField "type" object
      case frameType of
        "authenticate" -> do
          exactFields ["credential", "type"] object
          credential <- textField "credential" object
          if Text.null credential then Left InvalidAuthenticationFrame else Right (AuthenticateFrame credential)
        "active-files" -> do
          exactFields ["paths", "type"] object
          values <- case KeyMap.lookup "paths" object of
            Just (Aeson.Array items) | length items <= 32 -> traverse pathValue (toList items)
            _ -> Left InvalidActiveFilesFrame
          if length values == length (unique values) then Right (ActiveFilesFrame values) else Left InvalidActiveFilesFrame
        _ -> Left InvalidAuthenticationFrame
  where
    textField name object = case KeyMap.lookup (Key.fromText name) object of
      Just (Aeson.String value) -> Right value
      _ -> Left InvalidAuthenticationFrame
    exactFields allowed object =
      let unknown = filter (`notElem` allowed) (sort (map Key.toText (KeyMap.keys object)))
       in if null unknown then Right () else Left (UnknownFrameFields unknown)
    pathValue (Aeson.String value) = either (const (Left InvalidActiveFilesFrame)) Right (mkRepoPath value)
    pathValue _ = Left InvalidActiveFilesFrame
    unique = foldr (\value values -> if value `elem` values then values else value : values) []

-- Aeson objects are maps and therefore cannot report duplicate input keys.
-- Frames are bounded to 4096 bytes, so a small lexical pass preserves strict
-- duplicate rejection before the ordinary typed decoder runs.
hasDuplicateTopLevelKeys :: ByteString -> Either () Bool
hasDuplicateTopLevelKeys input = do
  rest <- consume 123 (dropSpace input)
  loop Set.empty (dropSpace rest)
  where
    loop seen bytes = case ByteString.uncons bytes of
      Just (125, trailing) | ByteString.null (dropSpace trailing) -> Right False
      Just (34, _) -> do
        (quoted, afterKey) <- takeString bytes
        key <- either (const (Left ())) Right (Aeson.eitherDecodeStrict' quoted :: Either String Text)
        afterColon <- consume 58 (dropSpace afterKey)
        afterValue <- skipValue (dropSpace afterColon)
        if Set.member key seen then Right True else
          case ByteString.uncons (dropSpace afterValue) of
            Just (44, more) -> loop (Set.insert key seen) (dropSpace more)
            Just (125, trailing) | ByteString.null (dropSpace trailing) -> Right False
            _ -> Left ()
      _ -> Left ()

    consume expected bytes = case ByteString.uncons bytes of
      Just (actual, rest) | actual == expected -> Right rest
      _ -> Left ()

    takeString bytes = case ByteString.uncons bytes of
      Just (34, rest) -> go False 1 rest
      _ -> Left ()
      where
        go escaped lengthSoFar remaining = case ByteString.uncons remaining of
          Nothing -> Left ()
          Just (byte, more)
            | escaped -> go False (lengthSoFar + 1) more
            | byte == 92 -> go True (lengthSoFar + 1) more
            | byte == 34 -> Right (ByteString.take (lengthSoFar + 1) bytes, more)
            | byte < 32 -> Left ()
            | otherwise -> go False (lengthSoFar + 1) more

    skipValue bytes
      | ByteString.null bytes = Left ()
      | otherwise = go False False 0 0 bytes
      where
        go :: Bool -> Bool -> Int -> Int -> ByteString -> Either () ByteString
        go inString escaped objects arrays remaining = case ByteString.uncons remaining of
          Nothing -> if inString || objects /= 0 || arrays /= 0 then Left () else Right remaining
          Just (byte, more)
            | inString && escaped -> go True False objects arrays more
            | inString && byte == 92 -> go True True objects arrays more
            | inString && byte == 34 -> go False False objects arrays more
            | inString -> go True False objects arrays more
            | byte == 34 -> go True False objects arrays more
            | byte == 123 -> go False False (objects + 1) arrays more
            | byte == 91 -> go False False objects (arrays + 1) more
            | byte == 125 && objects > 0 -> go False False (objects - 1) arrays more
            | byte == 93 && arrays > 0 -> go False False objects (arrays - 1) more
            | (byte == 44 || byte == 125) && objects == 0 && arrays == 0 -> Right remaining
            | otherwise -> go False False objects arrays more

    dropSpace = ByteString.dropWhile (`elem` [9, 10, 13, 32])

data SocketState = AwaitingAuthentication | SocketAuthenticated | SocketClosed SocketCloseReason
  deriving (Eq, Show)

data SocketCloseReason
  = MalformedAuthentication
  | AuthenticationRejected
  | AuthenticationRepeated
  | DataBeforeAuthentication
  deriving (Eq, Show)

-- | Apply one decoded client frame.  Events may be emitted only while the
-- returned state is 'SocketAuthenticated'.
acceptClientFrame :: (Text -> Bool) -> SocketState -> Either EventProtocolError ClientFrame -> SocketState
acceptClientFrame verify state frame = case state of
  SocketClosed reason -> SocketClosed reason
  SocketAuthenticated -> SocketClosed AuthenticationRepeated
  AwaitingAuthentication -> case frame of
    Left _ -> SocketClosed MalformedAuthentication
    Right (AuthenticateFrame supplied)
      | verify supplied -> SocketAuthenticated
      | otherwise -> SocketClosed AuthenticationRejected
    Right (ActiveFilesFrame _) -> SocketClosed DataBeforeAuthentication

data CoordinatorState = CoordinatorState Word64 Int (Map.Map Int EventSubscriber)
data EventCoordinator = EventCoordinator (TVar CoordinatorState) (TVar Bool)
data EventSubscriber = EventSubscriber Int (TBQueue EventEnvelope) (TVar Bool) (TVar Bool)

data GenerationExhausted = GenerationExhausted deriving (Eq, Show)
instance Exception GenerationExhausted

data SubscriberRead = SubscriberEvent EventEnvelope | SubscriberOverflow | SubscriberGenerationExhausted

newEventCoordinator :: IO EventCoordinator
newEventCoordinator = EventCoordinator <$> newTVarIO (CoordinatorState 0 0 Map.empty) <*> newTVarIO False

-- | Observe terminal state without allocating a generation. A clock already at
-- its maximum becomes terminal here as well, waking existing subscribers
-- before callers attempt repository work that may be temporarily locked.
isGenerationExhausted :: EventCoordinator -> IO Bool
isGenerationExhausted (EventCoordinator state terminal) = atomically $ do
  exhausted <- readTVar terminal
  CoordinatorState generation _ _ <- readTVar state
  if exhausted || generation == maxBound
    then writeTVar terminal True >> pure True
    else pure False

-- | A narrow boundary seam for tests; it can only move a live clock forward.
setEventGenerationForTest :: EventCoordinator -> Word64 -> IO ()
setEventGenerationForTest (EventCoordinator state terminal) target = atomically $ do
  CoordinatorState current next subscribers <- readTVar state
  exhausted <- readTVar terminal
  if target >= current && not exhausted
    then writeTVar state (CoordinatorState target next subscribers)
    else throwSTM (userError "cannot move generation backward or reset an exhausted coordinator")

nextEventGeneration :: EventCoordinator -> IO Word64
nextEventGeneration (EventCoordinator state terminal) = do
  result <- atomically $ do
    exhausted <- readTVar terminal
    CoordinatorState generation next subscribers <- readTVar state
    if exhausted || generation == maxBound
      then writeTVar terminal True >> pure (Left GenerationExhausted)
      else do
        let advanced = generation + 1
        writeTVar state (CoordinatorState advanced next subscribers)
        pure (Right advanced)
  either throwIO pure result

publishInvalidation :: EventCoordinator -> EventAsOf -> [Invalidation] -> IO EventEnvelope
publishInvalidation coordinator asOf invalidations = do
  published <- publishInvalidationWhen (pure True) coordinator asOf invalidations
  maybe (error "unconditional event publication was rejected") pure published

-- | Validate a captured observation and allocate its generation/enqueue it in
-- the same STM transaction.  The caller keeps the repository Git lock around
-- this bounded in-memory operation.
publishInvalidationWhen :: STM Bool -> EventCoordinator -> EventAsOf -> [Invalidation] -> IO (Maybe EventEnvelope)
publishInvalidationWhen admissible (EventCoordinator state terminal) asOf invalidations = do
  result <- atomically $ do
    exhausted <- readTVar terminal
    CoordinatorState generation next subscribers <- readTVar state
    if exhausted || generation == maxBound
      then writeTVar terminal True >> pure (Left GenerationExhausted)
      else do
        accepted <- admissible
        if not accepted then pure (Right Nothing) else do
          let advanced = generation + 1
              envelope = EventEnvelope advanced asOf (RepositoryInvalidated (sort invalidations))
          mapM_ (enqueue envelope) (Map.elems subscribers)
          writeTVar state (CoordinatorState advanced next subscribers)
          pure (Right (Just envelope))
  either throwIO pure result

registerSubscriberWithInitial :: EventCoordinator -> EventAsOf -> IO (Either Text EventSubscriber)
registerSubscriberWithInitial (EventCoordinator state terminal) asOf = atomically $ do
  exhausted <- readTVar terminal
  CoordinatorState generation next subscribers <- readTVar state
  if exhausted || generation == maxBound then writeTVar terminal True >> pure (Left "generation-exhausted")
  else if Map.size subscribers >= 16 then pure (Left "event subscriber limit reached")
  else do
    queue <- newTBQueue 64
    overflow <- newTVar False
    let subscriber = EventSubscriber next queue overflow terminal
        advanced = generation + 1
        initial = EventEnvelope advanced asOf (RepositoryInvalidated [minBound .. maxBound])
    writeTBQueue queue initial
    writeTVar state (CoordinatorState advanced (next + 1) (Map.insert next subscriber subscribers))
    pure (Right subscriber)

readSubscriberEvent :: EventSubscriber -> IO SubscriberRead
readSubscriberEvent (EventSubscriber _ queue overflow terminal) = atomically $ do
  exhausted <- readTVar terminal
  if exhausted then pure SubscriberGenerationExhausted else do
    full <- readTVar overflow
    if full then pure SubscriberOverflow else SubscriberEvent <$> readTBQueue queue

unregisterSubscriber :: EventCoordinator -> EventSubscriber -> IO ()
unregisterSubscriber (EventCoordinator state _) (EventSubscriber identifier _ _ _) = atomically $
  modifyTVar' state (\(CoordinatorState generation next subscribers) -> CoordinatorState generation next (Map.delete identifier subscribers))

enqueue :: EventEnvelope -> EventSubscriber -> STM ()
enqueue envelope (EventSubscriber _ queue overflow _) = do
  terminal <- readTVar overflow
  if terminal then pure () else do
    full <- isFullTBQueue queue
    if full then writeTVar overflow True else writeTBQueue queue envelope
