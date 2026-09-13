-- | Transport injection point for @/api/v1/events@.  P7-02 deliberately ships
-- no WebSocket runtime; P7-03 can install an admitted transport here without
-- changing HTTP routing or authentication.
module Adrai.Web.Socket
  ( EventsTransport (..),
    unavailableEventsTransport,
  )
where

import Adrai.Web.Api (ResponseMetadata)
import Network.Wai (Request, Response)

newtype EventsTransport = EventsTransport
  { runEventsTransport :: Request -> ResponseMetadata -> IO (Maybe Response)
  }

unavailableEventsTransport :: EventsTransport
unavailableEventsTransport = EventsTransport (\_ _ -> pure Nothing)
