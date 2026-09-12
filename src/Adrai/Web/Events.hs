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
    ClientFrame (..),
    decodeClientFrame,
    SocketState (..),
    SocketCloseReason (..),
    acceptClientFrame,
    EventProtocolError (..),
  )
where

import Adrai.Provenance (GitOid, gitOidText)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import Data.List (sort)
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
      "generation" Aeson..= eventGeneration envelope,
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

data ClientFrame = AuthenticateFrame Text
  deriving (Eq)

instance Show ClientFrame where
  show (AuthenticateFrame _) = "AuthenticateFrame <redacted>"

data EventProtocolError
  = FrameTooLarge
  | MalformedFrame
  | UnknownFrameFields [Text]
  | InvalidAuthenticationFrame
  deriving (Eq, Show)

decodeClientFrame :: Int -> ByteString -> Either EventProtocolError ClientFrame
decodeClientFrame maximumBytes bytes
  | ByteString.length bytes > maximumBytes = Left FrameTooLarge
  | otherwise = do
      value <- either (const (Left MalformedFrame)) Right (Aeson.eitherDecodeStrict' bytes)
      object <- case value of
        Aeson.Object fields -> Right fields
        _ -> Left InvalidAuthenticationFrame
      let keys = sort (map Key.toText (KeyMap.keys object))
          unknown = filter (`notElem` ["credential", "type"]) keys
      if null unknown then Right () else Left (UnknownFrameFields unknown)
      frameType <- textField "type" object
      credential <- textField "credential" object
      if frameType == "authenticate" && not (Text.null credential)
        then Right (AuthenticateFrame credential)
        else Left InvalidAuthenticationFrame
  where
    textField name object = case KeyMap.lookup (Key.fromText name) object of
      Just (Aeson.String value) -> Right value
      _ -> Left InvalidAuthenticationFrame

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
