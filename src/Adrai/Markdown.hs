{-# LANGUAGE OverloadedStrings #-}

module Adrai.Markdown
  ( MarkdownSections (..),
    extractMarkdownSections,
  )
where

import Adrai.Provenance (normalizeLineEndings)
import Data.Char (chr, isSpace, ord)
import Data.Text (Text)
import qualified Data.Text as Text

-- | The stable retrieval-oriented projection of a Markdown document.
--
-- Every field is globally trimmed. Unknown headings retain their original
-- display label and contents in 'markdownOther'.
data MarkdownSections = MarkdownSections
  { markdownContext :: Text,
    markdownDecision :: Text,
    markdownConsequences :: Text,
    markdownOther :: Text
  }
  deriving (Eq, Show)

-- | Extract retrieval sections from ATX headings in Markdown text.
--
-- This is intentionally not a full Markdown parser: heading-looking lines in
-- fenced blocks are headings too. A document with no recognized ATX headings
-- is treated wholly as decision text.
extractMarkdownSections :: Text -> MarkdownSections
extractMarkdownSections input =
  case parsedLines of
    [] -> emptySections
    _
      | any isHeadingLine parsedLines -> finish (foldl collect initialCollection parsedLines)
      | otherwise -> emptySections {markdownDecision = normalizeSection normalizedInput}
  where
    normalizedInput = normalizeLineEndings input
    parsedLines = fmap parseLine (Text.splitOn "\n" normalizedInput)

data ParsedLine
  = BodyLine Text
  | HeadingLine Heading

data Heading = Heading
  { headingBucket :: Bucket,
    headingOriginalLabel :: Text
  }

data Bucket
  = PreambleBucket
  | ContextBucket
  | DecisionBucket
  | ConsequencesBucket
  | OtherBucket

data Collection = Collection
  { currentBucket :: Bucket,
    preambleLines :: [Text],
    contextLines :: [Text],
    decisionLines :: [Text],
    consequenceLines :: [Text],
    otherLines :: [Text]
  }

emptySections :: MarkdownSections
emptySections = MarkdownSections "" "" "" ""

initialCollection :: Collection
initialCollection = Collection PreambleBucket [] [] [] [] []

isHeadingLine :: ParsedLine -> Bool
isHeadingLine (HeadingLine _) = True
isHeadingLine (BodyLine _) = False

collect :: Collection -> ParsedLine -> Collection
collect collection (BodyLine line) = appendLine line collection
collect collection (HeadingLine heading) =
  case headingBucket heading of
    OtherBucket -> appendLine (headingOriginalLabel heading) (collection {currentBucket = OtherBucket})
    bucket -> collection {currentBucket = bucket}

appendLine :: Text -> Collection -> Collection
appendLine line collection =
  case currentBucket collection of
    PreambleBucket -> collection {preambleLines = line : preambleLines collection}
    ContextBucket -> collection {contextLines = line : contextLines collection}
    DecisionBucket -> collection {decisionLines = line : decisionLines collection}
    ConsequencesBucket -> collection {consequenceLines = line : consequenceLines collection}
    OtherBucket -> collection {otherLines = line : otherLines collection}

finish :: Collection -> MarkdownSections
finish collection =
  MarkdownSections
    { markdownContext = combinePreamble (finishLines (preambleLines collection)) (finishLines (contextLines collection)),
      markdownDecision = finishLines (decisionLines collection),
      markdownConsequences = finishLines (consequenceLines collection),
      markdownOther = finishLines (otherLines collection)
    }

finishLines :: [Text] -> Text
finishLines = normalizeSection . Text.intercalate "\n" . reverse

combinePreamble :: Text -> Text -> Text
combinePreamble preamble context
  | Text.null preamble = context
  | Text.null context = preamble
  | otherwise = preamble <> "\n" <> context

normalizeSection :: Text -> Text
normalizeSection = Text.strip

parseLine :: Text -> ParsedLine
parseLine line =
  case parseHeading line of
    Nothing -> BodyLine line
    Just heading -> HeadingLine heading

parseHeading :: Text -> Maybe Heading
parseHeading line = do
  let (indent, afterIndent) = Text.span isSpace line
  if Text.length indent > 3 then Nothing else pure ()
  let (hashes, afterHashes) = Text.span (== '#') afterIndent
      level = Text.length hashes
  if level < 1 || level > 6 then Nothing else pure ()
  let (separators, labelText) = Text.span isSpace afterHashes
  if Text.null separators || Text.null (Text.strip labelText)
    then Nothing
    else
      let originalLabel = captureHeadingLabel labelText
          cleanedLabel = cleanHeadingLabel originalLabel
       in Just (Heading (bucketForLabel cleanedLabel) originalLabel)

-- Match the non-greedy heading capture followed by optional whitespace and
-- closing hashes in the frozen regular expression. A label made solely of
-- hashes retains one hash because the capture itself must be nonempty.
captureHeadingLabel :: Text -> Text
captureHeadingLabel value =
  let stripped = Text.strip value
      (reversedHashes, reversedPrefix) = Text.span (== '#') (Text.reverse stripped)
      hashes = Text.reverse reversedHashes
      prefix = Text.stripEnd (Text.reverse reversedPrefix)
   in if Text.null hashes
        then stripped
        else if Text.null prefix then "#" else prefix

cleanHeadingLabel :: Text -> Text
cleanHeadingLabel = Text.unwords . Text.words . Text.map cleanCharacter
  where
    cleanCharacter character
      | character >= 'a' && character <= 'z' = character
      | character >= 'A' && character <= 'Z' = chr (ord character + asciiCaseOffset)
      | character >= '0' && character <= '9' = character
      | otherwise = ' '
    asciiCaseOffset = ord 'a' - ord 'A'

bucketForLabel :: Text -> Bucket
bucketForLabel label
  | label `elem` ["context", "background", "problem", "motivation"] = ContextBucket
  | label `elem` ["decision", "solution", "chosen approach", "approach"] = DecisionBucket
  | label `elem` ["consequences", "consequence", "trade offs", "tradeoffs", "outcomes", "implications"] = ConsequencesBucket
  | otherwise = OtherBucket
