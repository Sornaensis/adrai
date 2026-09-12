{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Pure admission policy for the repository-bound loopback web process.
-- Runtime entropy, HTTP parsing, and socket enforcement are deliberately
-- owned by the server increment; this module makes their security decisions
-- explicit and independently testable.
module Adrai.Web.Security
  ( ProcessSecret,
    SessionCredential,
    BearerCredential,
    WebSocketCredential,
    mkProcessSecret,
    processSecretText,
    mkSessionCredential,
    mkBearerCredential,
    mkWebSocketCredential,
    credentialMatches,
    BoundAuthority,
    mkBoundAuthority,
    authorityHost,
    authorityOrigin,
    processCookieName,
    sessionCookie,
    SecurityMethod (..),
    AdmissionTarget (..),
    SecurityRequest (..),
    CredentialSource (..),
    SecurityError (..),
    admitRequest,
    bootstrapHeaders,
    websocketAuthTimeoutMicros,
    websocketAuthFrameBytes,
  )
where

import Adrai.Provenance (encodeBase64Url, sha256Digest)
import Adrai.Types (digestBytes)
import Data.ByteArray (constEq)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Char (isAscii, isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

newtype ProcessSecret = ProcessSecret ByteString
  deriving (Eq)

newtype SessionCredential = SessionCredential Text
  deriving (Eq)

newtype BearerCredential = BearerCredential Text
  deriving (Eq)

newtype WebSocketCredential = WebSocketCredential Text
  deriving (Eq)

instance Show ProcessSecret where show _ = "<redacted process secret>"
instance Show SessionCredential where show _ = "<redacted session credential>"
instance Show BearerCredential where show _ = "<redacted bearer credential>"
instance Show WebSocketCredential where show _ = "<redacted websocket credential>"

-- | The runtime must supply at least 256 bits from a cryptographically secure
-- source.  Generation is intentionally absent from this contract increment.
mkProcessSecret :: ByteString -> Either SecurityError ProcessSecret
mkProcessSecret value
  | ByteString.length value < 32 = Left SecretTooShort
  | otherwise = Right (ProcessSecret value)

processSecretText :: ProcessSecret -> Text
processSecretText (ProcessSecret value) = encodeBase64Url value

mkSessionCredential :: Text -> SessionCredential
mkSessionCredential = SessionCredential

mkBearerCredential :: Text -> BearerCredential
mkBearerCredential = BearerCredential

mkWebSocketCredential :: Text -> WebSocketCredential
mkWebSocketCredential = WebSocketCredential

class Credential candidate where
  credentialBytes :: candidate -> ByteString

instance Credential SessionCredential where credentialBytes (SessionCredential value) = TextEncoding.encodeUtf8 value
instance Credential BearerCredential where credentialBytes (BearerCredential value) = TextEncoding.encodeUtf8 value
instance Credential WebSocketCredential where credentialBytes (WebSocketCredential value) = TextEncoding.encodeUtf8 value

credentialMatches :: Credential candidate => ProcessSecret -> candidate -> Bool
credentialMatches (ProcessSecret expected) candidate =
  let encoded = TextEncoding.encodeUtf8 (encodeBase64Url expected)
      actual = credentialBytes candidate
   in ByteString.length encoded == ByteString.length actual && constEq encoded actual

data BoundAuthority = BoundAuthority
  { boundPort :: Int,
    boundProcessId :: Text
  }
  deriving (Eq, Show)

mkBoundAuthority :: Int -> Text -> Either SecurityError BoundAuthority
mkBoundAuthority port processId
  | port < 1 || port > 65535 = Left InvalidBoundPort
  | Text.null processId || Text.any (not . validIdCharacter) processId = Left InvalidProcessIdentifier
  | otherwise = Right (BoundAuthority port processId)
  where
    validIdCharacter character = isAscii character && (isAlphaNum character || character == '-' || character == '_')

authorityHost :: BoundAuthority -> Text
authorityHost authority = "127.0.0.1:" <> Text.pack (show (boundPort authority))

authorityOrigin :: BoundAuthority -> Text
authorityOrigin authority = "http://" <> authorityHost authority

processCookieName :: BoundAuthority -> ProcessSecret -> Text
processCookieName authority (ProcessSecret secret) =
  "adrai_session_"
    <> Text.pack (show (boundPort authority))
    <> "_"
    <> Text.take 12 (encodeBase64Url (digestBytes (sha256Digest secret)))

sessionCookie :: BoundAuthority -> ProcessSecret -> Text
sessionCookie authority secret =
  processCookieName authority secret
    <> "=" <> processSecretText secret
    <> "; Path=/; HttpOnly; SameSite=Strict"

data SecurityMethod = SecurityGet | SecurityPost
  deriving (Eq, Show)

data AdmissionTarget
  = BootstrapTarget
  | StaticTarget
  | ApiReadTarget
  | MutationTarget
  | WebSocketUpgradeTarget
  deriving (Eq, Show)

-- | Header-valued inputs remain lists so duplicate and ambiguous values can be
-- rejected before any framework normalisation discards evidence.
data SecurityRequest = SecurityRequest
  { securityMethod :: SecurityMethod,
    securityTarget :: AdmissionTarget,
    securityHosts :: [Text],
    securityOrigins :: [Text],
    securityAuthorization :: [Text],
    securityCookies :: [(Text, Text)],
    securityQuery :: [(Text, Maybe Text)],
    securityContentTypes :: [Text]
  }
  deriving (Eq)

instance Show SecurityRequest where
  show request =
    "SecurityRequest {method=" <> show (securityMethod request)
      <> ", target=" <> show (securityTarget request)
      <> ", hosts=" <> show (length (securityHosts request))
      <> ", origins=" <> show (length (securityOrigins request))
      <> ", authorization=<redacted>, cookies=<redacted>, query=<redacted>, contentTypes="
      <> show (securityContentTypes request) <> "}"

data CredentialSource
  = BootstrapQueryCredential Text
  | SessionCookieCredential Text
  | AuthorizationBearerCredential
  deriving (Eq)

instance Show CredentialSource where
  show (BootstrapQueryCredential _) = "BootstrapQueryCredential <redacted>"
  show (SessionCookieCredential name) = "SessionCookieCredential " <> show name
  show AuthorizationBearerCredential = "AuthorizationBearerCredential <redacted>"

data SecurityError
  = SecretTooShort
  | InvalidBoundPort
  | InvalidProcessIdentifier
  | InvalidHost
  | InvalidOrigin
  | MissingOrigin
  | QueryCredentialForbidden
  | MissingCredential
  | InvalidCredential
  | AmbiguousCredential
  | BearerRequired
  | JsonContentTypeRequired
  | InvalidMethodForTarget
  deriving (Eq, Show)

admitRequest :: BoundAuthority -> ProcessSecret -> SecurityRequest -> Either SecurityError CredentialSource
admitRequest authority secret request = do
  requireHost
  requireMethod
  requireOrigin
  case securityTarget request of
    BootstrapTarget -> admitBootstrap
    MutationTarget -> admitMutation
    _ -> admitOrdinary
  where
    requireHost =
      case securityHosts request of
        [value] | value == authorityHost authority -> Right ()
        _ -> Left InvalidHost

    requireMethod = case (securityMethod request, securityTarget request) of
      (SecurityGet, BootstrapTarget) -> Right ()
      (SecurityGet, StaticTarget) -> Right ()
      (SecurityGet, ApiReadTarget) -> Right ()
      (SecurityGet, WebSocketUpgradeTarget) -> Right ()
      (SecurityPost, MutationTarget) -> Right ()
      _ -> Left InvalidMethodForTarget

    requireOrigin =
      case (securityMethod request, securityTarget request) of
        (SecurityPost, _) -> exactOrigin
        (_, WebSocketUpgradeTarget) -> exactOrigin
        _ -> case securityOrigins request of
          [] -> Right ()
          [value] | value == authorityOrigin authority -> Right ()
          _ -> Left InvalidOrigin

    exactOrigin = case securityOrigins request of
      [] -> Left MissingOrigin
      [value] | value == authorityOrigin authority -> Right ()
      _ -> Left InvalidOrigin

    admitBootstrap = do
      if null (securityAuthorization request) && null (matchingCookies request)
        then Right ()
        else Left AmbiguousCredential
      case securityQuery request of
        [("token", Just supplied)]
          | credentialMatches secret (mkBearerCredential supplied) ->
              Right (BootstrapQueryCredential (sessionCookie authority secret))
          | otherwise -> Left InvalidCredential
        [] -> Left MissingCredential
        _ -> Left AmbiguousCredential

    admitMutation = do
      if hasTokenQuery request then Left QueryCredentialForbidden else Right ()
      case securityContentTypes request of
        ["application/json"] -> Right ()
        _ -> Left JsonContentTypeRequired
      candidates <- bearerCredentials request
      case candidates of
        [candidate]
          | credentialMatches secret candidate -> do
              case matchingCookies request of
                [] -> Right ()
                [cookie]
                  | credentialMatches secret cookie -> Right ()
                  | otherwise -> Left InvalidCredential
                _ -> Left AmbiguousCredential
              Right AuthorizationBearerCredential
          | otherwise -> Left InvalidCredential
        [] -> Left BearerRequired
        _ -> Left AmbiguousCredential

    admitOrdinary = do
      if hasTokenQuery request then Left QueryCredentialForbidden else Right ()
      candidates <- bearerCredentials request
      case (matchingCookies request, candidates) of
        ([candidate], [])
          | credentialMatches secret candidate -> Right (SessionCookieCredential (processCookieName authority secret))
          | otherwise -> Left InvalidCredential
        ([], [candidate])
          | credentialMatches secret candidate -> Right AuthorizationBearerCredential
          | otherwise -> Left InvalidCredential
        ([], []) -> Left MissingCredential
        _ -> Left AmbiguousCredential

    matchingCookies req =
      [mkSessionCredential value | (name, value) <- securityCookies req, name == processCookieName authority secret]

hasTokenQuery :: SecurityRequest -> Bool
hasTokenQuery = any ((== "token") . fst) . securityQuery

bearerCredentials :: SecurityRequest -> Either SecurityError [BearerCredential]
bearerCredentials request = case securityAuthorization request of
  [] -> Right []
  [value]
    | "Bearer " `Text.isPrefixOf` value && Text.length value > 7 -> Right [mkBearerCredential (Text.drop 7 value)]
    | otherwise -> Left InvalidCredential
  _ -> Left AmbiguousCredential

bootstrapHeaders :: [(Text, Text)]
bootstrapHeaders =
  [ ("Cache-Control", "no-store"),
    ("Referrer-Policy", "no-referrer")
  ]

websocketAuthTimeoutMicros :: Int
websocketAuthTimeoutMicros = 5 * 1000 * 1000

websocketAuthFrameBytes :: Int
websocketAuthFrameBytes = 4096
