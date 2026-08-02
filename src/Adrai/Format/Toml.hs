{-# LANGUAGE OverloadedStrings #-}

module Adrai.Format.Toml
  ( Toml10Violation (..),
    validateToml10,
    renderTomlString,
    renderTomlStringArray,
  )
where

import Data.Char (isControl, isSpace, ord)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex)

-- | TOML 1.1 syntax accepted by the installed parser but excluded from the
-- ADRAI TOML 1.0 contract.
data Toml10Violation
  = Toml10UnsupportedEscape Char
  | Toml10MultilineInlineTable
  | Toml10TrailingInlineTableComma
  deriving (Eq, Show)

-- | Reject the TOML 1.1 additions that can otherwise pass ADRAI's strict
-- table schemas. This is a narrow lexical preflight; the raw TOML parser
-- remains responsible for general syntax and semantic validation.
validateToml10 :: Text -> Either Toml10Violation ()
validateToml10 = scan ScanNormal 0 [] . T.unpack

data ScanState
  = ScanNormal
  | ScanComment
  | ScanBasic
  | ScanBasicMultiline
  | ScanLiteral
  | ScanLiteralMultiline

data InlineFrame = InlineFrame
  { inlineArrayDepth :: Int,
    inlineLastToken :: Maybe Char
  }

scan :: ScanState -> Int -> [InlineFrame] -> String -> Either Toml10Violation ()
scan _ _ _ [] = Right ()
scan ScanNormal arrayDepth frames ('#' : remaining) =
  scan ScanComment arrayDepth frames remaining
scan ScanNormal arrayDepth frames ('"' : '"' : '"' : remaining) =
  scan ScanBasicMultiline arrayDepth (markToken '"' frames) remaining
scan ScanNormal arrayDepth frames ('"' : remaining) =
  scan ScanBasic arrayDepth (markToken '"' frames) remaining
scan ScanNormal arrayDepth frames ('\'' : '\'' : '\'' : remaining) =
  scan ScanLiteralMultiline arrayDepth (markToken '\'' frames) remaining
scan ScanNormal arrayDepth frames ('\'' : remaining) =
  scan ScanLiteral arrayDepth (markToken '\'' frames) remaining
scan ScanNormal arrayDepth frames ('{' : remaining) =
  scan
    ScanNormal
    arrayDepth
    (InlineFrame arrayDepth Nothing : markToken '{' frames)
    remaining
scan ScanNormal arrayDepth frames ('}' : remaining) = do
  parentFrames <- closeInlineFrame frames
  scan ScanNormal arrayDepth parentFrames remaining
scan ScanNormal arrayDepth frames ('[' : remaining) =
  scan ScanNormal (arrayDepth + 1) (markToken '[' frames) remaining
scan ScanNormal arrayDepth frames (']' : remaining) =
  scan ScanNormal (max 0 (arrayDepth - 1)) (markToken ']' frames) remaining
scan ScanNormal arrayDepth frames ('\n' : remaining)
  | newlineAtInlineTableLevel arrayDepth frames = Left Toml10MultilineInlineTable
  | otherwise = scan ScanNormal arrayDepth frames remaining
scan ScanNormal arrayDepth frames (character : remaining)
  | isSpace character = scan ScanNormal arrayDepth frames remaining
  | otherwise = scan ScanNormal arrayDepth (markToken character frames) remaining
scan ScanComment arrayDepth frames ('\n' : remaining)
  | newlineAtInlineTableLevel arrayDepth frames = Left Toml10MultilineInlineTable
  | otherwise = scan ScanNormal arrayDepth frames remaining
scan ScanComment arrayDepth frames (_ : remaining) =
  scan ScanComment arrayDepth frames remaining
scan ScanBasic arrayDepth frames ('\\' : escape : remaining)
  | escape == 'e' || escape == 'x' = Left (Toml10UnsupportedEscape escape)
  | otherwise = scan ScanBasic arrayDepth frames remaining
scan ScanBasic arrayDepth frames ('"' : remaining) =
  scan ScanNormal arrayDepth frames remaining
scan ScanBasic arrayDepth frames (_ : remaining) =
  scan ScanBasic arrayDepth frames remaining
scan ScanBasicMultiline arrayDepth frames ('"' : '"' : '"' : remaining) =
  scan ScanNormal arrayDepth frames remaining
scan ScanBasicMultiline arrayDepth frames ('\\' : escape : remaining)
  | escape == 'e' || escape == 'x' = Left (Toml10UnsupportedEscape escape)
  | otherwise = scan ScanBasicMultiline arrayDepth frames remaining
scan ScanBasicMultiline arrayDepth frames (_ : remaining) =
  scan ScanBasicMultiline arrayDepth frames remaining
scan ScanLiteral arrayDepth frames ('\'' : remaining) =
  scan ScanNormal arrayDepth frames remaining
scan ScanLiteral arrayDepth frames (_ : remaining) =
  scan ScanLiteral arrayDepth frames remaining
scan ScanLiteralMultiline arrayDepth frames ('\'' : '\'' : '\'' : remaining) =
  scan ScanNormal arrayDepth frames remaining
scan ScanLiteralMultiline arrayDepth frames (_ : remaining) =
  scan ScanLiteralMultiline arrayDepth frames remaining

-- Closing an inline table always discards its frame. Only the parent, when
-- present, is marked with the completed table value.
closeInlineFrame :: [InlineFrame] -> Either Toml10Violation [InlineFrame]
closeInlineFrame [] = Right []
closeInlineFrame (closedFrame : parentFrames)
  | inlineLastToken closedFrame == Just ',' = Left Toml10TrailingInlineTableComma
  | otherwise = Right (markToken '}' parentFrames)

markToken :: Char -> [InlineFrame] -> [InlineFrame]
markToken _ [] = []
markToken token (frame : frames) = frame {inlineLastToken = Just token} : frames

newlineAtInlineTableLevel :: Int -> [InlineFrame] -> Bool
newlineAtInlineTableLevel _ [] = False
newlineAtInlineTableLevel arrayDepth (frame : _) = inlineArrayDepth frame == arrayDepth

-- | Render a TOML basic string using only TOML 1.0 escape forms.
-- Printable Unicode is preserved verbatim; control characters use the
-- shortest named escape where one exists and an uppercase Unicode escape
-- otherwise.
renderTomlString :: Text -> Text
renderTomlString value = "\"" <> T.concatMap renderCharacter value <> "\""

-- | Render a compact, deterministic TOML array of basic strings.
renderTomlStringArray :: [Text] -> Text
renderTomlStringArray values =
  "[" <> T.intercalate ", " (map renderTomlString values) <> "]"

renderCharacter :: Char -> Text
renderCharacter character =
  case character of
    '"' -> "\\\""
    '\\' -> "\\\\"
    '\b' -> "\\b"
    '\t' -> "\\t"
    '\n' -> "\\n"
    '\f' -> "\\f"
    '\r' -> "\\r"
    _
      | isControl character -> unicodeEscape character
      | otherwise -> T.singleton character

unicodeEscape :: Char -> Text
unicodeEscape character
  | codePoint <= 0xffff = "\\u" <> fixedHex 4 codePoint
  | otherwise = "\\U" <> fixedHex 8 codePoint
  where
    codePoint = ord character

fixedHex :: Int -> Int -> Text
fixedHex width value =
  let digits = T.toUpper (T.pack (showHex value ""))
   in T.replicate (width - T.length digits) "0" <> digits
