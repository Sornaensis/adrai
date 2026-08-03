{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | The deliberately small JSON surface used by public, pure projections.
-- Object members are sorted by key at rendering time; callers retain control
-- over array ordering and optional-member omission.
module Adrai.Format.Json
  ( JsonValue (..),
    jsonNumberRounded6,
    object,
    objectOmittingNulls,
    renderCanonicalJson,
    renderCanonicalJsonBytes,
  )
where

import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Numeric (showEFloat, showFFloat, showHex)

data JsonValue
  = JsonObject [(Text, JsonValue)]
  | JsonArray [JsonValue]
  | JsonString Text
  | JsonNumber Integer
  | JsonDecimal Double
  | JsonBool Bool
  | JsonNull
  deriving (Eq, Show)

-- | Construct a finite JSON number rounded with Haskell's ties-to-even
-- semantics to six fractional places.  The smart constructor keeps NaN and
-- infinities out of public projections; negative zero is normalized so the
-- canonical renderer never emits @-0.0@.
jsonNumberRounded6 :: Double -> Maybe JsonValue
jsonNumberRounded6 value
  | isNaN value || isInfinite value = Nothing
  | otherwise = Just (JsonDecimal normalized)
  where
    rounded = fromInteger (round (value * 1000000)) / 1000000
    normalized
      | rounded == 0 = 0
      | otherwise = rounded

object :: [(Text, JsonValue)] -> JsonValue
object = JsonObject

objectOmittingNulls :: [(Text, JsonValue)] -> JsonValue
objectOmittingNulls = JsonObject . filter ((/= JsonNull) . snd)

-- | Python-compatible @indent=2, sort_keys=True, ensure_ascii=True@ output,
-- including exactly one terminal LF.
renderCanonicalJson :: JsonValue -> Text
renderCanonicalJson value = render 0 value <> "\n"

renderCanonicalJsonBytes :: JsonValue -> ByteString
renderCanonicalJsonBytes = TextEncoding.encodeUtf8 . renderCanonicalJson

render :: Int -> JsonValue -> Text
render depth value =
  case value of
    JsonObject members -> renderObject depth (sortOn fst members)
    JsonArray values -> renderArray depth values
    JsonString text -> quote text
    JsonNumber number -> Text.pack (show number)
    JsonDecimal number -> renderDecimal number
    JsonBool boolean -> if boolean then "true" else "false"
    JsonNull -> "null"

renderObject :: Int -> [(Text, JsonValue)] -> Text
renderObject _ [] = "{}"
renderObject depth members =
  "{\n"
    <> Text.intercalate
      ",\n"
      [ indent (depth + 1) <> quote key <> ": " <> render (depth + 1) value
        | (key, value) <- members
      ]
    <> "\n"
    <> indent depth
    <> "}"

renderArray :: Int -> [JsonValue] -> Text
renderArray _ [] = "[]"
renderArray depth values =
  "[\n"
    <> Text.intercalate
      ",\n"
      [indent (depth + 1) <> render (depth + 1) value | value <- values]
    <> "\n"
    <> indent depth
    <> "]"

indent :: Int -> Text
indent depth = Text.replicate (depth * 2) " "

quote :: Text -> Text
quote value = "\"" <> Text.concatMap escape value <> "\""

escape :: Char -> Text
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
      | code < 0x20 || code > 0x7e -> unicodeEscape code
      | otherwise -> Text.singleton character
  where
    code = ord character

unicodeEscape :: Int -> Text
unicodeEscape code
  | code <= 0xffff = "\\u" <> hex4 code
  | otherwise =
      let adjusted = code - 0x10000
          high = 0xd800 + adjusted `div` 0x400
          low = 0xdc00 + adjusted `mod` 0x400
       in "\\u" <> hex4 high <> "\\u" <> hex4 low

hex4 :: Int -> Text
hex4 value =
  let rendered = Text.pack (showHex value "")
   in Text.replicate (4 - Text.length rendered) "0" <> rendered

-- Python's canonical public fixtures use the float representation produced by
-- json.dumps after round(value, 6): fixed notation at 1e-4 and above,
-- scientific notation below it, a two-digit exponent, and an explicit .0 for
-- integral floats.
renderDecimal :: Double -> Text
renderDecimal value
  | value == 0 = "0.0"
  | absolute < 0.0001 = renderScientific value
  | otherwise = ensureFraction (trimFraction (Text.pack (showFFloat (Just 6) value "")))
  where
    absolute = abs value

renderScientific :: Double -> Text
renderScientific value = trimScientificMantissa mantissa <> "e" <> renderExponent exponentText
  where
    rendered = Text.pack (showEFloat (Just 6) value "")
    (mantissa, exponentWithMarker) = Text.breakOn "e" rendered
    exponentText = Text.drop 1 exponentWithMarker

trimScientificMantissa :: Text -> Text
trimScientificMantissa = Text.dropWhileEnd (== '.') . Text.dropWhileEnd (== '0')

trimFraction :: Text -> Text
trimFraction value
  | "." `Text.isInfixOf` value = Text.dropWhileEnd (== '.') (Text.dropWhileEnd (== '0') value)
  | otherwise = value

ensureFraction :: Text -> Text
ensureFraction value
  | "." `Text.isInfixOf` value = value
  | otherwise = value <> ".0"

renderExponent :: Text -> Text
renderExponent exponentText = sign <> Text.replicate (max 0 (2 - Text.length digits)) "0" <> digits
  where
    (sign, digits) = case Text.uncons exponentText of
      Just ('-', rest) -> ("-", rest)
      Just ('+', rest) -> ("+", rest)
      _ -> ("+", exponentText)
