{-# LANGUAGE OverloadedStrings #-}

module Adrai.Format
  ( parseAdrId,
    parseRecordId,
    parseConnectionId,
    parseOperationId,
    renderObjectRef,
    parseDigest,
    renderDigest,
    parseStateToken,
    renderStateToken,
    sourceSchemaText,
    publicSchemaText,
    configSchemaNumber,
  )
where

import Adrai.Types
  ( AdraiError (..),
    AdrId,
    ConfigSchema (..),
    ConnectionId,
    Digest,
    ExitClass (ExitUserError),
    ObjectRef,
    OperationId,
    PublicSchema (..),
    RecordId,
    SourceSchema (..),
    StateToken,
    digestBytes,
    mkAdrId,
    mkConnectionId,
    mkDigest,
    mkOperationId,
    mkRecordId,
    mkStateToken,
    objectRefText,
    stateTokenText,
  )
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)

-- Identifier text is case-insensitive at this input boundary. Whitespace is
-- deliberately not stripped: committed identifiers have one canonical shape.
parseAdrId :: Text -> Either AdraiError AdrId
parseAdrId = parseIdentifier "invalid-adr-id" mkAdrId

parseRecordId :: Text -> Either AdraiError RecordId
parseRecordId = parseIdentifier "invalid-record-id" mkRecordId

parseConnectionId :: Text -> Either AdraiError ConnectionId
parseConnectionId = parseIdentifier "invalid-connection-id" mkConnectionId

parseOperationId :: Text -> Either AdraiError OperationId
parseOperationId = parseIdentifier "invalid-operation-id" mkOperationId

parseIdentifier :: (Show violation) => Text -> (Text -> Either violation identifier) -> Text -> Either AdraiError identifier
parseIdentifier errorCode constructor input =
  mapLeft (formatError errorCode . T.pack . show) (constructor (asciiUpper input))

renderObjectRef :: ObjectRef -> Text
renderObjectRef = objectRefText

parseDigest :: Text -> Either AdraiError Digest
parseDigest input = do
  payload <-
    maybe
      (Left (formatError "invalid-digest" "expected the sha256: prefix"))
      Right
      (T.stripPrefix "sha256:" input)
  if T.length payload /= 43
    then Left (formatError "invalid-digest" "expected 43 unpadded base64url characters")
    else do
      bytes <- mapLeft (formatError "invalid-digest") (decodeBase64Url payload)
      mapLeft (formatError "invalid-digest" . T.pack . show) (mkDigest bytes)

renderDigest :: Digest -> Text
renderDigest digest = "sha256:" <> encodeBase64Url (digestBytes digest)

parseStateToken :: Text -> Either AdraiError StateToken
parseStateToken = mapLeft (formatError "invalid-state-token" . T.pack . show) . mkStateToken

renderStateToken :: StateToken -> Text
renderStateToken = stateTokenText

sourceSchemaText :: SourceSchema -> Text
sourceSchemaText sourceSchema =
  case sourceSchema of
    DecisionSourceV1 -> "adrai/decision/v1"
    ConnectionSourceV1 -> "adrai/connection/v1"

publicSchemaText :: PublicSchema -> Text
publicSchemaText publicSchema =
  case publicSchema of
    SearchPublicV1 -> "adrai/search/v1"
    RelevantPublicV1 -> "adrai/relevant/v1"
    HistoryPublicV1 -> "adrai/history/v1"
    ShowCollapsedPublicV1 -> "adrai/show-collapsed/v1"
    ShowExplodedPublicV1 -> "adrai/show-exploded/v1"
    EventsPublicV1 -> "adrai/events/v1"

configSchemaNumber :: ConfigSchema -> Int
configSchemaNumber ConfigSchemaV1 = 1

formatError :: Text -> Text -> AdraiError
formatError errorCode detail =
  AdraiError
    { adraiErrorClass = ExitUserError,
      adraiErrorMessage = errorCode <> ": " <> detail
    }

mapLeft :: (left -> otherLeft) -> Either left right -> Either otherLeft right
mapLeft action value =
  case value of
    Left problem -> Left (action problem)
    Right result -> Right result

asciiUpper :: Text -> Text
asciiUpper = T.map upper
  where
    upper character
      | character >= 'a' && character <= 'z' = toEnum (fromEnum character - 32)
      | otherwise = character

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

alphabetAt :: Int -> Char
alphabetAt = T.index base64UrlAlphabet

decodeBase64Url :: Text -> Either Text ByteString
decodeBase64Url input = BS.pack <$> decodeWords (T.unpack input)
  where
    decodeWords :: String -> Either Text [Word8]
    decodeWords [] = Right []
    decodeWords [first, second] = do
      firstValue <- decodeCharacter first
      secondValue <- decodeCharacter second
      if secondValue .&. 15 /= 0
        then Left "non-canonical trailing base64url bits"
        else Right [fromIntegral ((firstValue `shiftL` 2) .|. (secondValue `shiftR` 4))]
    decodeWords [first, second, third] = do
      firstValue <- decodeCharacter first
      secondValue <- decodeCharacter second
      thirdValue <- decodeCharacter third
      if thirdValue .&. 3 /= 0
        then Left "non-canonical trailing base64url bits"
        else
          Right
            [ fromIntegral ((firstValue `shiftL` 2) .|. (secondValue `shiftR` 4)),
              fromIntegral (((secondValue .&. 15) `shiftL` 4) .|. (thirdValue `shiftR` 2))
            ]
    decodeWords (first : second : third : fourth : remaining) = do
      firstValue <- decodeCharacter first
      secondValue <- decodeCharacter second
      thirdValue <- decodeCharacter third
      fourthValue <- decodeCharacter fourth
      rest <- decodeWords remaining
      Right
        ( fromIntegral ((firstValue `shiftL` 2) .|. (secondValue `shiftR` 4))
            : fromIntegral (((secondValue .&. 15) `shiftL` 4) .|. (thirdValue `shiftR` 2))
            : fromIntegral (((thirdValue .&. 3) `shiftL` 6) .|. fourthValue)
            : rest
        )
    decodeWords _ = Left "invalid unpadded base64url length"

decodeCharacter :: Char -> Either Text Int
decodeCharacter character =
  maybe
    (Left ("invalid base64url character: " <> T.singleton character))
    Right
    (T.findIndex (== character) base64UrlAlphabet)
