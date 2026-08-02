{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | The deliberately small JSON surface used by public, pure projections.
-- Object members are sorted by key at rendering time; callers retain control
-- over array ordering and optional-member omission.
module Adrai.Format.Json
  ( JsonValue (..),
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
import Numeric (showHex)

data JsonValue
  = JsonObject [(Text, JsonValue)]
  | JsonArray [JsonValue]
  | JsonString Text
  | JsonNumber Integer
  | JsonBool Bool
  | JsonNull
  deriving (Eq, Show)

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
