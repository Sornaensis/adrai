{-# LANGUAGE OverloadedStrings #-}

-- | Construction of the canonical, sortable ADRAI identifiers.
--
-- The payload is the Crockford Base32 representation of exactly 128 bits.  It
-- is deliberately encoded directly from the big-endian integer value: the
-- first of the 26 characters therefore contains only three significant bits,
-- and textual ordering agrees with byte ordering.
module Adrai.Identity
  ( IdentityError (..),
    encodeCrockford128,
    adrIdFromBytes,
    recordIdFromBytes,
    connectionIdFromBytes,
    operationIdFromBytes,
    sortableIdentityBytes,
    sortableAdrId,
    sortableRecordId,
    sortableConnectionId,
    sortableOperationId,
  )
where

import Adrai.Types
  ( AdrId,
    ConnectionId,
    IdViolation,
    OperationId,
    RecordId,
    mkAdrId,
    mkConnectionId,
    mkOperationId,
    mkRecordId,
  )
import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T

-- | Failures that can occur before a typed identifier is constructed.
data IdentityError
  = IdentityWrongLength Int
  | IdentityTimestampWrongLength Int
  | IdentityEntropyWrongLength Int
  | IdentityRejected IdViolation
  deriving (Eq, Show)

-- | Encode exactly 16 bytes as 26 Crockford Base32 characters.
--
-- No normalization or ambiguous-character aliases are involved.  This is an
-- encoder for the committed wire representation, not a forgiving input
-- parser.
encodeCrockford128 :: ByteString -> Either IdentityError Text
encodeCrockford128 bytes
  | BS.length bytes /= identityByteLength =
      Left (IdentityWrongLength (BS.length bytes))
  | otherwise = Right (T.pack (map encodeDigit digitOffsets))
  where
    value = BS.foldl' (\result byte -> result * 256 + fromIntegral byte) (0 :: Integer) bytes
    digitOffsets = [encodedCharacterCount - 1, encodedCharacterCount - 2 .. 0]
    encodeDigit offset =
      T.index crockfordAlphabet
        (fromIntegral ((value `shiftR` (offset * 5)) .&. 31))

-- | Construct an ADR identifier from its exact 16-byte representation.
adrIdFromBytes :: ByteString -> Either IdentityError AdrId
adrIdFromBytes = identifierFromBytes 'A' mkAdrId

-- | Construct a record identifier from its exact 16-byte representation.
recordIdFromBytes :: ByteString -> Either IdentityError RecordId
recordIdFromBytes = identifierFromBytes 'R' mkRecordId

-- | Construct a connection identifier from its exact 16-byte representation.
connectionIdFromBytes :: ByteString -> Either IdentityError ConnectionId
connectionIdFromBytes = identifierFromBytes 'C' mkConnectionId

-- | Construct an operation identifier from its exact 16-byte representation.
operationIdFromBytes :: ByteString -> Either IdentityError OperationId
operationIdFromBytes = identifierFromBytes 'O' mkOperationId

-- | Join the sortable 6-byte big-endian timestamp and 10-byte entropy fields.
--
-- Keeping this operation byte-oriented makes the byte order explicit and does
-- not silently truncate timestamps outside the committed 48-bit layout.
sortableIdentityBytes :: ByteString -> ByteString -> Either IdentityError ByteString
sortableIdentityBytes timestamp entropy
  | BS.length timestamp /= timestampByteLength =
      Left (IdentityTimestampWrongLength (BS.length timestamp))
  | BS.length entropy /= entropyByteLength =
      Left (IdentityEntropyWrongLength (BS.length entropy))
  | otherwise = Right (timestamp <> entropy)

sortableAdrId :: ByteString -> ByteString -> Either IdentityError AdrId
sortableAdrId timestamp entropy =
  sortableIdentityBytes timestamp entropy >>= adrIdFromBytes

sortableRecordId :: ByteString -> ByteString -> Either IdentityError RecordId
sortableRecordId timestamp entropy =
  sortableIdentityBytes timestamp entropy >>= recordIdFromBytes

sortableConnectionId :: ByteString -> ByteString -> Either IdentityError ConnectionId
sortableConnectionId timestamp entropy =
  sortableIdentityBytes timestamp entropy >>= connectionIdFromBytes

sortableOperationId :: ByteString -> ByteString -> Either IdentityError OperationId
sortableOperationId timestamp entropy =
  sortableIdentityBytes timestamp entropy >>= operationIdFromBytes

identifierFromBytes :: Char -> (Text -> Either IdViolation identifier) -> ByteString -> Either IdentityError identifier
identifierFromBytes prefix constructor bytes = do
  payload <- encodeCrockford128 bytes
  mapLeft IdentityRejected (constructor (T.cons prefix payload))

mapLeft :: (left -> otherLeft) -> Either left right -> Either otherLeft right
mapLeft action value =
  case value of
    Left problem -> Left (action problem)
    Right result -> Right result

crockfordAlphabet :: Text
crockfordAlphabet = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

identityByteLength :: Int
identityByteLength = 16

encodedCharacterCount :: Int
encodedCharacterCount = 26

timestampByteLength :: Int
timestampByteLength = 6

entropyByteLength :: Int
entropyByteLength = identityByteLength - timestampByteLength
