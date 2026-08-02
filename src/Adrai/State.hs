{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Canonical semantic-state payloads and their compact state tokens.
module Adrai.State
  ( StateHeads (..),
    canonicalStatePayload,
    canonicalStatePayloadBytes,
    stateTokenForHeads,
  )
where

import Adrai.Types
  ( ConnectionId,
    RecordId,
    StateToken,
    connectionIdText,
    mkStateToken,
    recordIdText,
  )
import qualified Crypto.Hash as Crypto
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (digitToInt, isDigit)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8)

-- | The semantic heads whose canonical representation identifies repository
-- state.  Lists intentionally preserve multiplicity; canonicalization only
-- imposes ordering.
data StateHeads = StateHeads
  { stateRecordHeads :: [RecordId],
    stateScopeHeads :: [ConnectionId],
    stateStatusHeads :: [ConnectionId],
    stateDomainHeads :: [ConnectionId]
  }
  deriving (Eq, Show)

-- | Render the exact compact JSON payload used by the v1 state-token contract.
--
-- The four keys have a fixed order.  Each array is sorted lexicographically by
-- canonical identifier text, with duplicates preserved.
canonicalStatePayload :: StateHeads -> Text
canonicalStatePayload heads =
  "{\"domains\":"
    <> renderArray (map connectionIdText (stateDomainHeads heads))
    <> ",\"records\":"
    <> renderArray (map recordIdText (stateRecordHeads heads))
    <> ",\"scopes\":"
    <> renderArray (map connectionIdText (stateScopeHeads heads))
    <> ",\"statuses\":"
    <> renderArray (map connectionIdText (stateStatusHeads heads))
    <> "}"

canonicalStatePayloadBytes :: StateHeads -> ByteString
canonicalStatePayloadBytes = TextEncoding.encodeUtf8 . canonicalStatePayload

-- | Compute @S@ followed by the first 22 unpadded base64url characters of the
-- SHA-256 digest of 'canonicalStatePayloadBytes'.
stateTokenForHeads :: StateHeads -> StateToken
stateTokenForHeads heads =
  case mkStateToken tokenText of
    Right token -> token
    Left _ -> error "internal error: generated an invalid state token"
  where
    digest = sha256Bytes (canonicalStatePayloadBytes heads)
    tokenText = "S" <> T.take 22 (encodeBase64Url digest)

renderArray :: [Text] -> Text
renderArray values =
  "[" <> T.intercalate "," (map quote (sort values)) <> "]"
  where
    -- Typed identifiers contain only the strict Crockford alphabet and their
    -- one-character type prefix, so JSON escaping cannot alter these values.
    quote value = "\"" <> value <> "\""

sha256Bytes :: ByteString -> ByteString
sha256Bytes bytes =
  case hexToBytes (T.pack (show (Crypto.hash bytes :: Crypto.Digest Crypto.SHA256))) of
    Right digest -> digest
    Left _ -> error "crypton returned a non-SHA-256 digest"

hexToBytes :: Text -> Either String ByteString
hexToBytes value
  | T.length value /= 64 = Left "wrong SHA-256 hex length"
  | otherwise = BS.pack <$> pairs (T.unpack value)
  where
    pairs [] = Right []
    pairs (high : low : remaining)
      | isHex high && isHex low =
          (fromIntegral (digitToInt high * 16 + digitToInt low) :) <$> pairs remaining
      | otherwise = Left "invalid SHA-256 hex"
    pairs _ = Left "odd SHA-256 hex length"
    isHex character = isDigit character || character >= 'a' && character <= 'f'

base64UrlAlphabet :: Text
base64UrlAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

encodeBase64Url :: ByteString -> Text
encodeBase64Url = T.pack . encodeWords . BS.unpack
  where
    encodeWords :: [Word8] -> String
    encodeWords [] = []
    encodeWords [first] =
      [ alphabetAt (fromIntegral first `shiftR` 2),
        alphabetAt ((fromIntegral first .&. 3) `shiftL` 4)
      ]
    encodeWords [first, second] =
      [ alphabetAt (fromIntegral first `shiftR` 2),
        alphabetAt (((fromIntegral first .&. 3) `shiftL` 4) .|. (fromIntegral second `shiftR` 4)),
        alphabetAt ((fromIntegral second .&. 15) `shiftL` 2)
      ]
    encodeWords (first : second : third : remaining) =
      alphabetAt (fromIntegral first `shiftR` 2)
        : alphabetAt (((fromIntegral first .&. 3) `shiftL` 4) .|. (fromIntegral second `shiftR` 4))
        : alphabetAt (((fromIntegral second .&. 15) `shiftL` 2) .|. (fromIntegral third `shiftR` 6))
        : alphabetAt (fromIntegral third .&. 63)
        : encodeWords remaining
    alphabetAt = T.index base64UrlAlphabet
