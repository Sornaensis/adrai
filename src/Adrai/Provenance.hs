{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Adrai.Provenance
  ( GitOid(..),
    mkGitOid,
    gitOidText,
    OverlayFingerprint (..),
    mkOverlayFingerprint,
    EventKind,
    mkEventKind,
    eventKindText,
    LineAnchor,
    mkLineAnchor,
    lineAnchorId,
    lineAnchorCommit,
    ProvenanceObjectId (..),
    provenanceObjectIdText,
    provenanceObjectFromRef,
    ProvenanceCapsuleInput (..),
    ProvenanceCapsule,
    mkProvenanceCapsule,
    provenanceOperationId,
    provenanceObjectId,
    provenanceEventKind,
    provenanceActor,
    provenanceTimestampMs,
    provenanceBasis,
    provenanceParents,
    provenanceBranchHint,
    provenanceUpstreamHint,
    provenanceLineAnchors,
    provenanceSemanticDigest,
    provenanceToolVersion,
    provenanceInputs,
    OperationContext (..),
    provenanceOperationContext,
    ProvenanceError (..),
    encodeBase64Url,
    decodeBase64Url,
    sha256Digest,
    sha256DigestFrames,
    normalizeLineEndings,
    normalizeSemantic,
    semanticDigest,
    encodeCapsule,
    decodeCapsule,
    validateCapsule,
    sealSemantic,
  )
where

import Adrai.Types
  ( Actor,
    ActorKind (..),
    AdrId,
    ConnectionId,
    Digest,
    ObjectRef,
    OperationId,
    ProvenanceInputs (..),
    RecordId,
    actorId,
    actorKind,
    actorModel,
    adrIdText,
    connectionIdText,
    digestBytes,
    mkActor,
    mkAdrId,
    mkConnectionId,
    mkDigest,
    mkOperationId,
    mkRecordId,
    objectRefText,
    operationIdText,
    recordIdText,
  )
import qualified Crypto.Hash as Crypto
import Data.ByteArray (convert)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (digitToInt, isAsciiLower, isDigit, isSpace, ord)
import Data.List (sort, sortOn)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Vector as Vector
import Numeric (showHex)

-- Validated capsule primitives -------------------------------------------------

newtype GitOid = GitOid Text
  deriving (Eq, Ord, Show)

newtype OverlayFingerprint = OverlayFingerprint Text
  deriving (Eq, Ord, Show)

mkOverlayFingerprint :: Text -> Maybe OverlayFingerprint
mkOverlayFingerprint value
  | T.length value == 64 = Just (OverlayFingerprint value)
  | otherwise = Nothing

mkGitOid :: Text -> Either ProvenanceError GitOid
mkGitOid value
  | T.length value `notElem` [40, 64] = Left (InvalidGitOid value)
  | T.all isLowerHex value = Right (GitOid value)
  | otherwise = Left (InvalidGitOid value)
  where
    isLowerHex character = isDigit character || character >= 'a' && character <= 'f'

gitOidText :: GitOid -> Text
gitOidText (GitOid value) = value

newtype EventKind = EventKind Text
  deriving (Eq, Ord, Show)

mkEventKind :: Text -> Either ProvenanceError EventKind
mkEventKind value =
  case T.uncons value of
    Just (first, remaining)
      | isAsciiLower first && T.all validRemaining remaining -> Right (EventKind value)
    _ -> Left (InvalidEventKind value)
  where
    validRemaining character =
      isAsciiLower character || isDigit character || character == '_' || character == '.'

eventKindText :: EventKind -> Text
eventKindText (EventKind value) = value

data LineAnchor = LineAnchor
  { lineAnchorId :: Text,
    lineAnchorCommit :: GitOid
  }
  deriving (Eq, Ord, Show)

mkLineAnchor :: Text -> GitOid -> Either ProvenanceError LineAnchor
mkLineAnchor identifier commit
  | T.null (T.strip identifier) = Left (InvalidLineAnchor identifier)
  | otherwise = Right (LineAnchor identifier commit)

data ProvenanceObjectId
  = ProvenanceAdr AdrId
  | ProvenanceRecord RecordId
  | ProvenanceConnection ConnectionId
  | ProvenanceOperation OperationId
  deriving (Eq, Ord, Show)

provenanceObjectIdText :: ProvenanceObjectId -> Text
provenanceObjectIdText objectId =
  case objectId of
    ProvenanceAdr identifier -> adrIdText identifier
    ProvenanceRecord identifier -> recordIdText identifier
    ProvenanceConnection identifier -> connectionIdText identifier
    ProvenanceOperation identifier -> operationIdText identifier

provenanceObjectFromRef :: ObjectRef -> ProvenanceObjectId
provenanceObjectFromRef reference =
  case T.uncons (objectRefText reference) of
    Just ('A', _) -> parseKnown (ProvenanceAdr <$> mkAdrId (objectRefText reference))
    Just ('R', _) -> parseKnown (ProvenanceRecord <$> mkRecordId (objectRefText reference))
    Just ('C', _) -> parseKnown (ProvenanceConnection <$> mkConnectionId (objectRefText reference))
    _ -> error "Adrai.Types.ObjectRef violated its constructor invariant"
  where
    parseKnown result =
      case result of
        Right objectId -> objectId
        Left _ -> error "Adrai.Types.ObjectRef violated its constructor invariant"

data ProvenanceCapsuleInput = ProvenanceCapsuleInput
  { capsuleInputOperationId :: OperationId,
    capsuleInputObjectId :: ProvenanceObjectId,
    capsuleInputEventKind :: EventKind,
    capsuleInputActor :: Actor,
    capsuleInputTimestampMs :: Integer,
    capsuleInputBasis :: GitOid,
    capsuleInputParents :: [ProvenanceObjectId],
    capsuleInputBranchHint :: Maybe Text,
    capsuleInputUpstreamHint :: Maybe Text,
    capsuleInputLineAnchors :: [LineAnchor],
    capsuleInputSemanticDigest :: Digest,
    capsuleInputToolVersion :: Text,
    capsuleInputDigests :: ProvenanceInputs
  }
  deriving (Eq, Show)

-- | Capsule fields shared by every member of one logical operation. Member
-- identity, event kind, semantic digest, and ordered parents are intentionally
-- excluded because they are object-specific.
data OperationContext = OperationContext
  { operationContextTimestampMs :: Integer,
    operationContextActor :: Actor,
    operationContextBasis :: GitOid,
    operationContextBranchHint :: Maybe Text,
    operationContextUpstreamHint :: Maybe Text,
    operationContextLineAnchors :: [LineAnchor],
    operationContextToolVersion :: Text,
    operationContextInputs :: ProvenanceInputs
  }
  deriving (Eq, Show)

newtype ProvenanceCapsule = ProvenanceCapsule ProvenanceCapsuleInput
  deriving (Eq, Show)

mkProvenanceCapsule :: ProvenanceCapsuleInput -> Either ProvenanceError ProvenanceCapsule
mkProvenanceCapsule input
  | capsuleInputTimestampMs input <= 0 =
      Left (InvalidCapsuleField "t" "provenance timestamp must be a positive integer")
  | Just duplicate <- firstDuplicate (capsuleInputParents input) =
      Left (DuplicateCapsuleParent duplicate)
  | not (validToolVersion (capsuleInputToolVersion input)) =
      Left (InvalidCapsuleField "x" "tool version must have the form adrai/N.N.N")
  | Just invalid <- invalidHint (capsuleInputBranchHint input) =
      Left (InvalidCapsuleField "r" invalid)
  | Just invalid <- invalidHint (capsuleInputUpstreamHint input) =
      Left (InvalidCapsuleField "u" invalid)
  | otherwise = Right (ProvenanceCapsule input)
  where
    invalidHint Nothing = Nothing
    invalidHint (Just value)
      | T.any (`elem` ['\n', '\r', '\NUL']) value = Just "hint contains an invalid control character"
      | otherwise = Nothing

provenanceOperationId :: ProvenanceCapsule -> OperationId
provenanceOperationId (ProvenanceCapsule input) = capsuleInputOperationId input

provenanceObjectId :: ProvenanceCapsule -> ProvenanceObjectId
provenanceObjectId (ProvenanceCapsule input) = capsuleInputObjectId input

provenanceEventKind :: ProvenanceCapsule -> EventKind
provenanceEventKind (ProvenanceCapsule input) = capsuleInputEventKind input

provenanceActor :: ProvenanceCapsule -> Actor
provenanceActor (ProvenanceCapsule input) = capsuleInputActor input

provenanceTimestampMs :: ProvenanceCapsule -> Integer
provenanceTimestampMs (ProvenanceCapsule input) = capsuleInputTimestampMs input

provenanceBasis :: ProvenanceCapsule -> GitOid
provenanceBasis (ProvenanceCapsule input) = capsuleInputBasis input

provenanceParents :: ProvenanceCapsule -> [ProvenanceObjectId]
provenanceParents (ProvenanceCapsule input) = capsuleInputParents input

provenanceBranchHint :: ProvenanceCapsule -> Maybe Text
provenanceBranchHint (ProvenanceCapsule input) = capsuleInputBranchHint input

provenanceUpstreamHint :: ProvenanceCapsule -> Maybe Text
provenanceUpstreamHint (ProvenanceCapsule input) = capsuleInputUpstreamHint input

provenanceLineAnchors :: ProvenanceCapsule -> [LineAnchor]
provenanceLineAnchors (ProvenanceCapsule input) = capsuleInputLineAnchors input

provenanceSemanticDigest :: ProvenanceCapsule -> Digest
provenanceSemanticDigest (ProvenanceCapsule input) = capsuleInputSemanticDigest input

provenanceToolVersion :: ProvenanceCapsule -> Text
provenanceToolVersion (ProvenanceCapsule input) = capsuleInputToolVersion input

provenanceInputs :: ProvenanceCapsule -> ProvenanceInputs
provenanceInputs (ProvenanceCapsule input) = capsuleInputDigests input

provenanceOperationContext :: ProvenanceCapsule -> OperationContext
provenanceOperationContext capsule =
  OperationContext
    { operationContextTimestampMs = provenanceTimestampMs capsule,
      operationContextActor = provenanceActor capsule,
      operationContextBasis = provenanceBasis capsule,
      operationContextBranchHint = provenanceBranchHint capsule,
      operationContextUpstreamHint = provenanceUpstreamHint capsule,
      operationContextLineAnchors = provenanceLineAnchors capsule,
      operationContextToolVersion = provenanceToolVersion capsule,
      operationContextInputs = provenanceInputs capsule
    }

data ProvenanceError
  = InvalidGitOid Text
  | InvalidEventKind Text
  | InvalidLineAnchor Text
  | InvalidBase64Url Text
  | InvalidCapsuleJson Text
  | CapsuleJsonMustBeObject
  | DuplicateCapsuleJsonKey Text
  | UnsupportedCapsuleKeys [Text]
  | MissingCapsuleKeys [Text]
  | UnsupportedActorKeys [Text]
  | InvalidCapsuleField Text Text
  | UnsupportedCapsuleVersion Integer
  | DuplicateCapsuleParent ProvenanceObjectId
  | CapsuleObjectMismatch Text Text
  | CapsuleDigestMismatch Digest Digest
  deriving (Eq, Show)

-- Canonical base64url and digests ---------------------------------------------

base64UrlAlphabet :: Text
base64UrlAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

encodeBase64Url :: ByteString -> Text
encodeBase64Url = T.pack . encodeWords . BS.unpack
  where
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

decodeBase64Url :: Text -> Either ProvenanceError ByteString
decodeBase64Url input = BS.pack <$> decodeWords (T.unpack input)
  where
    decodeWords [] = Right []
    decodeWords [first, second] = do
      firstValue <- decodeCharacter first
      secondValue <- decodeCharacter second
      if secondValue .&. 15 /= 0
        then Left (InvalidBase64Url "non-canonical trailing base64url bits")
        else Right [fromIntegral ((firstValue `shiftL` 2) .|. (secondValue `shiftR` 4))]
    decodeWords [first, second, third] = do
      firstValue <- decodeCharacter first
      secondValue <- decodeCharacter second
      thirdValue <- decodeCharacter third
      if thirdValue .&. 3 /= 0
        then Left (InvalidBase64Url "non-canonical trailing base64url bits")
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
    decodeWords _ = Left (InvalidBase64Url "invalid unpadded base64url length")

    decodeCharacter character =
      maybe
        (Left (InvalidBase64Url ("invalid base64url character: " <> T.singleton character)))
        Right
        (T.findIndex (== character) base64UrlAlphabet)

sha256Digest :: ByteString -> Digest
sha256Digest bytes = digestFromHash (Crypto.hash bytes :: Crypto.Digest Crypto.SHA256)

-- | SHA-256 of a sequence of frames without building one concatenated
-- ByteString.
--
-- Produces exactly the same digest as @sha256Digest (BS.concat frames)@ while
-- keeping transient memory to a single frame (a lazy chain of chunks rather
-- than a corpus-sized strict ByteString), so persisted fingerprints fed from
-- an unchanged frame sequence stay byte-stable.
sha256DigestFrames :: [ByteString] -> Digest
sha256DigestFrames frames =
  digestFromHash
    ( Crypto.hashlazy (BL.concat (map BL.fromStrict frames))
        :: Crypto.Digest Crypto.SHA256
    )

digestFromHash :: Crypto.Digest Crypto.SHA256 -> Digest
digestFromHash digest =
  case mkDigest (convert digest) of
    Right value -> value
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

normalizeSemantic :: Text -> Text
normalizeSemantic input = T.intercalate "\n" retained <> "\n"
  where
    -- Python's frozen prototype uses @str.splitlines()@ after normalizing CRLF.
    -- Match its complete Unicode line-boundary vocabulary, not only CR/LF, so
    -- semantic identity is stable across both implementations.
    normalizedEndings = normalizeLineEndings input
    retained = dropTrailingEmpty (map (T.dropWhileEnd isSpace) semanticLines)
    semanticLines = filter (not . isCapsuleTrailer . T.strip) (T.splitOn "\n" normalizedEndings)

    dropTrailingEmpty = reverse . dropWhile T.null . reverse


-- | Match the complete line-boundary vocabulary recognized by the frozen
-- prototype's @splitlines()@ behavior.
normalizeLineEndings :: Text -> Text
normalizeLineEndings = T.map normalizeLineBoundary . T.replace "\r\n" "\n"
  where
    normalizeLineBoundary character
      | character `elem` ['\r', '\v', '\f', '\x001c', '\x001d', '\x001e', '\x0085', '\x2028', '\x2029'] = '\n'
      | otherwise = character

semanticDigest :: Text -> Digest
semanticDigest = sha256Digest . TextEncoding.encodeUtf8 . normalizeSemantic

-- Capsule JSON ----------------------------------------------------------------

encodeCapsule :: ProvenanceCapsule -> Text
encodeCapsule = encodeBase64Url . TextEncoding.encodeUtf8 . renderCapsuleJson

decodeCapsule :: Text -> Either ProvenanceError ProvenanceCapsule
decodeCapsule encoded = do
  bytes <- decodeBase64Url encoded
  rawText <-
    case TextEncoding.decodeUtf8' bytes of
      Left problem -> Left (InvalidCapsuleJson (T.pack (show problem)))
      Right decoded -> Right decoded
  rawValue <- parseRawJson rawText
  validateRawIntegerFields rawValue
  value <-
    case Aeson.eitherDecodeStrict' bytes of
      Left problem -> Left (InvalidCapsuleJson (T.pack problem))
      Right decoded -> Right decoded
  object <-
    case value of
      Aeson.Object mapping -> Right mapping
      _ -> Left CapsuleJsonMustBeObject
  rejectUnknown capsuleKeys object
  requireKeys requiredCapsuleKeys object

  version <- requireInteger "v" object
  if version /= 1
    then Left (UnsupportedCapsuleVersion version)
    else Right ()
  operation <- requireText "op" object >>= parseOperation
  objectId <- requireText "o" object >>= parseObjectId
  event <- requireText "k" object >>= mkEventKind
  actor <- requireValue "a" object >>= decodeActor
  timestamp <- requireInteger "t" object
  basis <- requireText "b" object >>= mkGitOid
  parents <- optionalArray "p" object >>= mapM decodeObjectValue
  branch <- optionalNullableText "r" object
  upstream <- optionalNullableText "u" object
  anchors <- optionalArray "g" object >>= mapM decodeAnchor
  digest <- requireText "s" object >>= parseDigestText
  tool <- requireText "x" object
  inputDigest <- optionalNullableDigest "i" object
  promptDigest <- optionalNullableDigest "q" object
  contextDigest <- optionalNullableDigest "c" object
  mkProvenanceCapsule
    ProvenanceCapsuleInput
      { capsuleInputOperationId = operation,
        capsuleInputObjectId = objectId,
        capsuleInputEventKind = event,
        capsuleInputActor = actor,
        capsuleInputTimestampMs = timestamp,
        capsuleInputBasis = basis,
        capsuleInputParents = parents,
        capsuleInputBranchHint = branch,
        capsuleInputUpstreamHint = upstream,
        capsuleInputLineAnchors = anchors,
        capsuleInputSemanticDigest = digest,
        capsuleInputToolVersion = tool,
        capsuleInputDigests = ProvenanceInputs inputDigest promptDigest contextDigest
      }
  where
    capsuleKeys =
      Set.fromList ["v", "op", "o", "k", "a", "t", "b", "p", "r", "u", "g", "s", "x", "i", "q", "c"]

    requiredCapsuleKeys = Set.fromList ["v", "op", "o", "k", "a", "t", "b", "s", "x"]

validateCapsule :: ObjectRef -> Digest -> ProvenanceCapsule -> Either ProvenanceError ()
validateCapsule expectedObject expectedDigest capsule
  | actualObject /= expectedObjectText =
      Left (CapsuleObjectMismatch expectedObjectText actualObject)
  | actualDigest /= expectedDigest =
      Left (CapsuleDigestMismatch expectedDigest actualDigest)
  | otherwise = Right ()
  where
    expectedObjectText = objectRefText expectedObject
    actualObject = provenanceObjectIdText (provenanceObjectId capsule)
    actualDigest = provenanceSemanticDigest capsule

sealSemantic :: Text -> ProvenanceCapsule -> Text
sealSemantic semantic capsule =
  T.dropWhileEnd isSpace (normalizeSemantic semantic)
    <> "\n\n<!-- @adrai:"
    <> encodeCapsule capsule
    <> " -->\n"

data CanonicalJson
  = JsonString Text
  | JsonInteger Integer
  | JsonArray [CanonicalJson]
  | JsonObject [(Text, CanonicalJson)]

renderCapsuleJson :: ProvenanceCapsule -> Text
renderCapsuleJson capsule = renderCanonicalJson (JsonObject fields)
  where
    actor = provenanceActor capsule
    actorFields =
      [ ("k", JsonString (renderActorKind (actorKind actor))),
        ("i", JsonString (actorId actor))
      ]
        <> maybe [] (\model -> [("m", JsonString model)]) (actorModel actor)
    ProvenanceInputs inputDigest promptDigest contextDigest = provenanceInputs capsule
    fields =
      [ ("a", JsonObject actorFields),
        ("b", JsonString (gitOidText (provenanceBasis capsule)))
      ]
        <> optionalDigest "c" contextDigest
        <> optionalArrayField
          "g"
          [ JsonArray [JsonString (lineAnchorId anchor), JsonString (gitOidText (lineAnchorCommit anchor))]
            | anchor <- provenanceLineAnchors capsule
          ]
        <> optionalDigest "i" inputDigest
        <> [ ("k", JsonString (eventKindText (provenanceEventKind capsule))),
             ("o", JsonString (provenanceObjectIdText (provenanceObjectId capsule))),
             ("op", JsonString (operationIdText (provenanceOperationId capsule)))
           ]
        <> optionalArrayField
          "p"
          [JsonString (provenanceObjectIdText parent) | parent <- provenanceParents capsule]
        <> optionalDigest "q" promptDigest
        <> optionalText "r" (provenanceBranchHint capsule)
        <> [ ("s", JsonString (renderDigestText (provenanceSemanticDigest capsule))),
             ("t", JsonInteger (provenanceTimestampMs capsule))
           ]
        <> optionalText "u" (provenanceUpstreamHint capsule)
        <> [ ("v", JsonInteger 1),
             ("x", JsonString (provenanceToolVersion capsule))
           ]

    optionalArrayField _ [] = []
    optionalArrayField key values = [(key, JsonArray values)]
    optionalText key = maybe [] (\value -> [(key, JsonString value)])
    optionalDigest key = maybe [] (\value -> [(key, JsonString (renderDigestText value))])

renderActorKind :: ActorKind -> Text
renderActorKind kind =
  case kind of
    HumanActor -> "human"
    LlmActor -> "llm"
    ServiceActor -> "service"

renderCanonicalJson :: CanonicalJson -> Text
renderCanonicalJson value =
  case value of
    JsonString string -> renderJsonString string
    JsonInteger integer -> T.pack (show integer)
    JsonArray values -> "[" <> T.intercalate "," (map renderCanonicalJson values) <> "]"
    JsonObject fields ->
      "{"
        <> T.intercalate
          ","
          [renderJsonString key <> ":" <> renderCanonicalJson fieldValue | (key, fieldValue) <- sortOn fst fields]
        <> "}"

renderJsonString :: Text -> Text
renderJsonString value = "\"" <> T.concatMap escape value <> "\""
  where
    escape character =
      case character of
        '"' -> "\\\""
        '\\' -> "\\\\"
        '\b' -> "\\b"
        '\f' -> "\\f"
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        _
          | code < 0x20 -> unicodeEscape code
          | code <= 0x7e -> T.singleton character
          | code <= 0xffff -> unicodeEscape code
          | otherwise ->
              let scalar = code - 0x10000
                  high = 0xd800 + scalar `shiftR` 10
                  low = 0xdc00 + scalar .&. 0x3ff
               in unicodeEscape high <> unicodeEscape low
      where
        code = ord character

    unicodeEscape code = "\\u" <> T.pack (replicate (4 - length digits) '0' <> digits)
      where
        digits = showHex code ""

-- Raw JSON validation ---------------------------------------------------------

-- Aeson intentionally represents objects as maps. This small structural pass
-- runs first so duplicate decoded keys cannot disappear during that conversion,
-- and so the original number token for required integer fields remains visible.
data RawJson
  = RawObject [(Text, RawJson)]
  | RawNumber Text
  | RawOther

parseRawJson :: Text -> Either ProvenanceError RawJson
parseRawJson input = do
  (value, remaining) <- parseRawValue (T.unpack input)
  case skipJsonWhitespace remaining of
    [] -> Right value
    _ -> rawJsonFailure "unexpected content after the JSON value"

parseRawValue :: String -> Either ProvenanceError (RawJson, String)
parseRawValue input =
  case skipJsonWhitespace input of
    ('{' : remaining) -> parseRawObject remaining
    ('[' : remaining) -> parseRawArray remaining
    value@('"' : _) -> do
      (_, rest) <- parseJsonString value
      Right (RawOther, rest)
    value@('-' : _) -> parseRawNumber value
    value@(first : _)
      | isDigit first -> parseRawNumber value
    value
      | Just remaining <- stripJsonLiteral "true" value -> Right (RawOther, remaining)
      | Just remaining <- stripJsonLiteral "false" value -> Right (RawOther, remaining)
      | Just remaining <- stripJsonLiteral "null" value -> Right (RawOther, remaining)
    [] -> rawJsonFailure "unexpected end of JSON input"
    _ -> rawJsonFailure "invalid JSON value"

parseRawObject :: String -> Either ProvenanceError (RawJson, String)
parseRawObject input = members Set.empty [] (skipJsonWhitespace input)
  where
    members _ fields ('}' : remaining) = Right (RawObject (reverse fields), remaining)
    members seen fields value@('"' : _) = do
      (key, afterKey) <- parseJsonString value
      if key `Set.member` seen
        then Left (DuplicateCapsuleJsonKey key)
        else Right ()
      afterColon <-
        case skipJsonWhitespace afterKey of
          ':' : remaining -> Right remaining
          _ -> rawJsonFailure "JSON object key must be followed by ':'"
      (fieldValue, afterValue) <- parseRawValue afterColon
      case skipJsonWhitespace afterValue of
        ',' : remaining ->
          members (Set.insert key seen) ((key, fieldValue) : fields) (skipJsonWhitespace remaining)
        '}' : remaining ->
          Right (RawObject (reverse ((key, fieldValue) : fields)), remaining)
        _ -> rawJsonFailure "JSON object members must be separated by ','"
    members _ _ [] = rawJsonFailure "unterminated JSON object"
    members _ _ _ = rawJsonFailure "JSON object keys must be strings"

parseRawArray :: String -> Either ProvenanceError (RawJson, String)
parseRawArray input = elements (skipJsonWhitespace input)
  where
    elements (']' : remaining) = Right (RawOther, remaining)
    elements [] = rawJsonFailure "unterminated JSON array"
    elements value = do
      (_, afterValue) <- parseRawValue value
      case skipJsonWhitespace afterValue of
        ',' : remaining -> elements (skipJsonWhitespace remaining)
        ']' : remaining -> Right (RawOther, remaining)
        _ -> rawJsonFailure "JSON array values must be separated by ','"

parseJsonString :: String -> Either ProvenanceError (Text, String)
parseJsonString ('"' : remaining) = do
  (token, rest) <- collect ['"'] remaining
  case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 (T.pack token)) :: Either String Text of
    Left problem -> Left (InvalidCapsuleJson (T.pack problem))
    Right decoded -> Right (decoded, rest)
  where
    collect _ [] = rawJsonFailure "unterminated JSON string"
    collect accumulated ('"' : rest) = Right (reverse ('"' : accumulated), rest)
    collect _ ['\\'] = rawJsonFailure "unterminated JSON string escape"
    collect accumulated ('\\' : escaped : rest) = collect (escaped : '\\' : accumulated) rest
    collect accumulated (character : rest) = collect (character : accumulated) rest
parseJsonString _ = rawJsonFailure "expected a JSON string"

parseRawNumber :: String -> Either ProvenanceError (RawJson, String)
parseRawNumber input =
  let (token, remaining) = span isJsonNumberCharacter input
   in if validJsonNumber token
        then Right (RawNumber (T.pack token), remaining)
        else rawJsonFailure "invalid JSON number"
  where
    isJsonNumberCharacter character = isDigit character || character `elem` ("-+.eE" :: String)

validJsonNumber :: String -> Bool
validJsonNumber token =
  case integerPart (stripMinus token) of
    Nothing -> False
    Just afterInteger ->
      case fractionPart afterInteger of
        Nothing -> False
        Just afterFraction ->
          case exponentPart afterFraction of
            Just [] -> True
            _ -> False
  where
    stripMinus ('-' : remaining) = remaining
    stripMinus value = value

    integerPart ('0' : remaining)
      | firstIsDigit remaining = Nothing
      | otherwise = Just remaining
    integerPart (first : remaining)
      | first >= '1' && first <= '9' = Just (dropWhile isDigit remaining)
    integerPart _ = Nothing

    fractionPart ('.' : first : remaining)
      | isDigit first = Just (dropWhile isDigit remaining)
    fractionPart ('.' : _) = Nothing
    fractionPart remaining = Just remaining

    exponentPart (marker : remaining)
      | marker == 'e' || marker == 'E' = exponentDigits (stripExponentSign remaining)
    exponentPart remaining = Just remaining

    stripExponentSign (sign : remaining)
      | sign == '+' || sign == '-' = remaining
    stripExponentSign remaining = remaining

    exponentDigits (first : remaining)
      | isDigit first = Just (dropWhile isDigit remaining)
    exponentDigits _ = Nothing

    firstIsDigit (first : _) = isDigit first
    firstIsDigit [] = False

validateRawIntegerFields :: RawJson -> Either ProvenanceError ()
validateRawIntegerFields (RawObject fields) = do
  validate "v"
  validate "t"
  where
    validate key =
      case lookup key fields of
        Just (RawNumber token)
          | T.all (\character -> isDigit character || character == '-') token -> Right ()
          | otherwise -> Left (InvalidCapsuleField key "must use integer JSON notation")
        _ -> Right ()
validateRawIntegerFields _ = Right ()

skipJsonWhitespace :: String -> String
skipJsonWhitespace = dropWhile (`elem` (" \t\r\n" :: String))

stripJsonLiteral :: String -> String -> Maybe String
stripJsonLiteral [] remaining = Just remaining
stripJsonLiteral (expected : expectedRest) (actual : actualRest)
  | expected == actual = stripJsonLiteral expectedRest actualRest
stripJsonLiteral _ _ = Nothing

rawJsonFailure :: Text -> Either ProvenanceError value
rawJsonFailure = Left . InvalidCapsuleJson

-- JSON decoding helpers -------------------------------------------------------

rejectUnknown :: Set.Set Text -> Aeson.Object -> Either ProvenanceError ()
rejectUnknown allowed object =
  case sort (Set.toList (actual `Set.difference` allowed)) of
    [] -> Right ()
    unknown -> Left (UnsupportedCapsuleKeys unknown)
  where
    actual = Set.fromList (map Key.toText (KeyMap.keys object))

requireKeys :: Set.Set Text -> Aeson.Object -> Either ProvenanceError ()
requireKeys required object =
  case sort (Set.toList (required `Set.difference` actual)) of
    [] -> Right ()
    missing -> Left (MissingCapsuleKeys missing)
  where
    actual = Set.fromList (map Key.toText (KeyMap.keys object))

requireValue :: Text -> Aeson.Object -> Either ProvenanceError Aeson.Value
requireValue key object =
  maybe
    (Left (MissingCapsuleKeys [key]))
    Right
    (KeyMap.lookup (Key.fromText key) object)

requireText :: Text -> Aeson.Object -> Either ProvenanceError Text
requireText key object = requireValue key object >>= valueAsText key

valueAsText :: Text -> Aeson.Value -> Either ProvenanceError Text
valueAsText _ (Aeson.String value) = Right value
valueAsText key _ = Left (InvalidCapsuleField key "must be a string")

requireInteger :: Text -> Aeson.Object -> Either ProvenanceError Integer
requireInteger key object = do
  value <- requireValue key object
  case Aeson.fromJSON value of
    Aeson.Success integer -> Right integer
    Aeson.Error _ -> Left (InvalidCapsuleField key "must be an integer")

optionalArray :: Text -> Aeson.Object -> Either ProvenanceError [Aeson.Value]
optionalArray key object =
  case KeyMap.lookup (Key.fromText key) object of
    Nothing -> Right []
    Just (Aeson.Array values) -> Right (Vector.toList values)
    Just _ -> Left (InvalidCapsuleField key "must be an array")

optionalNullableText :: Text -> Aeson.Object -> Either ProvenanceError (Maybe Text)
optionalNullableText key object =
  case KeyMap.lookup (Key.fromText key) object of
    Nothing -> Right Nothing
    Just Aeson.Null -> Right Nothing
    Just value -> Just <$> valueAsText key value

optionalNullableDigest :: Text -> Aeson.Object -> Either ProvenanceError (Maybe Digest)
optionalNullableDigest key object = do
  value <- optionalNullableText key object
  mapM parseDigestText value

decodeActor :: Aeson.Value -> Either ProvenanceError Actor
decodeActor (Aeson.Object object) = do
  let actual = Set.fromList (map Key.toText (KeyMap.keys object))
      allowed = Set.fromList ["k", "i", "m"]
      unknown = sort (Set.toList (actual `Set.difference` allowed))
  if null unknown then Right () else Left (UnsupportedActorKeys unknown)
  kindText <- requireText "k" object
  identifier <- requireText "i" object
  model <- optionalActorModel object
  kind <-
    case kindText of
      "human" -> Right HumanActor
      "llm" -> Right LlmActor
      "service" -> Right ServiceActor
      _ -> Left (InvalidCapsuleField "a.k" "must be human, llm, or service")
  case mkActor kind identifier model of
    Left problem -> Left (InvalidCapsuleField "a" (T.pack (show problem)))
    Right actor -> Right actor
decodeActor _ = Left (InvalidCapsuleField "a" "must be an object")

optionalActorModel :: Aeson.Object -> Either ProvenanceError (Maybe Text)
optionalActorModel object =
  case KeyMap.lookup (Key.fromText "m") object of
    Nothing -> Right Nothing
    Just value -> Just <$> valueAsText "a.m" value

decodeObjectValue :: Aeson.Value -> Either ProvenanceError ProvenanceObjectId
decodeObjectValue value = valueAsText "p" value >>= parseObjectId

decodeAnchor :: Aeson.Value -> Either ProvenanceError LineAnchor
decodeAnchor (Aeson.Array values)
  | [lineValue, commitValue] <- Vector.toList values = do
      line <- valueAsText "g" lineValue
      commitText <- valueAsText "g" commitValue
      commit <- mkGitOid commitText
      mkLineAnchor line commit
decodeAnchor _ = Left (InvalidCapsuleField "g" "each line anchor must be a two-string array")

parseOperation :: Text -> Either ProvenanceError OperationId
parseOperation value =
  mapIdError "op" (mkOperationId value)

parseObjectId :: Text -> Either ProvenanceError ProvenanceObjectId
parseObjectId value =
  case T.uncons value of
    Just ('A', _) -> ProvenanceAdr <$> mapIdError "o" (mkAdrId value)
    Just ('R', _) -> ProvenanceRecord <$> mapIdError "o" (mkRecordId value)
    Just ('C', _) -> ProvenanceConnection <$> mapIdError "o" (mkConnectionId value)
    Just ('O', _) -> ProvenanceOperation <$> mapIdError "o" (mkOperationId value)
    _ -> Left (InvalidCapsuleField "o" "must be a canonical ADRAI object ID")

mapIdError :: (Show problem) => Text -> Either problem value -> Either ProvenanceError value
mapIdError key result =
  case result of
    Left problem -> Left (InvalidCapsuleField key (T.pack (show problem)))
    Right value -> Right value

parseDigestText :: Text -> Either ProvenanceError Digest
parseDigestText value = do
  payload <-
    maybe
      (Left (InvalidCapsuleField "digest" "must begin with sha256:"))
      Right
      (T.stripPrefix "sha256:" value)
  if T.length payload /= 43
    then Left (InvalidCapsuleField "digest" "must contain 43 unpadded base64url characters")
    else do
      bytes <- decodeBase64Url payload
      case mkDigest bytes of
        Left problem -> Left (InvalidCapsuleField "digest" (T.pack (show problem)))
        Right digest -> Right digest

renderDigestText :: Digest -> Text
renderDigestText digest = "sha256:" <> encodeBase64Url (digestBytes digest)

validToolVersion :: Text -> Bool
validToolVersion value =
  case T.stripPrefix "adrai/" value of
    Nothing -> False
    Just version ->
      case T.splitOn "." version of
        [major, minor, patch] -> all decimalComponent [major, minor, patch]
        _ -> False
  where
    decimalComponent component = not (T.null component) && T.all isDigit component

firstDuplicate :: (Ord value) => [value] -> Maybe value
firstDuplicate = go Set.empty
  where
    go _ [] = Nothing
    go seen (value : remaining)
      | value `Set.member` seen = Just value
      | otherwise = go (Set.insert value seen) remaining

isCapsuleTrailer :: Text -> Bool
isCapsuleTrailer line =
  case T.stripPrefix "<!-- @adrai:" line >>= T.stripSuffix " -->" of
    Just payload -> not (T.null payload) && T.all (`T.elem` base64UrlAlphabet) payload
    Nothing -> False
