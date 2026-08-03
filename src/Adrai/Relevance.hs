{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Deterministic, format-independent text chunking for relevance inputs.
module Adrai.Relevance
  ( TextChunk (..),
    ChunkError (..),
    maxTextBytes,
    targetChunkChars,
    chunkOverlapChars,
    chunkBoundaryRadius,
    normalizeNewlines,
    chunkText,
  )
where

import Adrai.Provenance (normalizeLineEndings)
import qualified Data.ByteString as ByteString
import Data.List (findIndices)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

-- | One source-text chunk.  Ordinals are zero-based; line ranges are one-based
-- and inclusive.
data TextChunk = TextChunk
  { textChunkOrdinal :: Int,
    textChunkStartLine :: Int,
    textChunkEndLine :: Int,
    textChunkText :: Text
  }
  deriving (Eq, Show)

-- | Chunking rejects rather than truncates text above the UTF-8 size limit.
-- The constructor carries the actual and maximum byte counts, respectively.
data ChunkError
  = ChunkTooLarge Int Int
  deriving (Eq, Show)

-- | Maximum accepted size after newline normalization, measured as UTF-8.
maxTextBytes :: Int
maxTextBytes = 4 * 1024 * 1024

-- | Desired chunk length in Unicode characters.
targetChunkChars :: Int
targetChunkChars = 1500

-- | Character overlap between adjacent chunks.
chunkOverlapChars :: Int
chunkOverlapChars = 300

-- | Radius around a target cut in which a newline is preferred.
chunkBoundaryRadius :: Int
chunkBoundaryRadius = 220

-- | Normalize every line boundary recognized by the managed Markdown and
-- semantic-format paths to LF.
normalizeNewlines :: Text -> Text
normalizeNewlines = normalizeLineEndings

-- | Split text into deterministic, overlapping, line-aware chunks.
--
-- Whitespace-only chunks are omitted.  Content is otherwise preserved after
-- newline normalization, including leading and trailing whitespace.
chunkText :: Text -> Either ChunkError [TextChunk]
chunkText input
  | utf8Bytes > maxTextBytes = Left (ChunkTooLarge utf8Bytes maxTextBytes)
  | Text.null normalized || Text.null (Text.strip normalized) = Right []
  | otherwise = Right (go 0 0 [])
  where
    normalized = normalizeNewlines input
    textLength = Text.length normalized
    utf8Bytes = ByteString.length (TextEncoding.encodeUtf8 normalized)

    go start ordinal chunks
      | start >= textLength = reverse chunks
      | otherwise =
          let target = min textLength (start + targetChunkChars)
              preferred = preferredEnd normalized start target
              end =
                if preferred <= start
                  then min textLength (start + targetChunkChars)
                  else preferred
              raw = Text.take (end - start) (Text.drop start normalized)
              hasContent = not (Text.null (Text.strip raw))
              nextChunks =
                if hasContent
                  then
                    TextChunk
                      { textChunkOrdinal = ordinal,
                        textChunkStartLine = lineNumber normalized start,
                        textChunkEndLine = lineNumber normalized (lastContentOffset start end raw),
                        textChunkText = raw
                      }
                      : chunks
                  else chunks
              nextOrdinal = if hasContent then ordinal + 1 else ordinal
           in if end >= textLength
                then reverse nextChunks
                else
                  let nextStart = max (start + 1) (end - chunkOverlapChars)
                   in go nextStart nextOrdinal nextChunks

preferredEnd :: Text -> Int -> Int -> Int
preferredEnd text start target
  | target >= Text.length text = Text.length text
  | otherwise =
      case candidates of
        [] -> target
        first : remaining -> foldl nearer first remaining
  where
    lower = max (start + 1) (target - chunkBoundaryRadius)
    upper = min (Text.length text) (target + chunkBoundaryRadius)
    window = Text.take (upper - lower) (Text.drop lower text)
    candidates = [lower + offset + 1 | offset <- findIndices (== '\n') (Text.unpack window)]
    nearer best candidate
      | (abs (candidate - target), candidate) < (abs (best - target), best) = candidate
      | otherwise = best

lineNumber :: Text -> Int -> Int
lineNumber text offset =
  Text.count "\n" (Text.take safe text) + 1
  where
    safe = max 0 (min offset (Text.length text))

lastContentOffset :: Int -> Int -> Text -> Int
lastContentOffset start end raw =
  max start (end - trailingNewlines - 1)
  where
    trailingNewlines = Text.length (Text.takeWhileEnd (== '\n') raw)
