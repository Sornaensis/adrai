{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Adrai.Retrieval
  ( QueryProfile (..),
    queryProfileName,
    QueryWeights (..),
    profileWeights,
    queryScaffolding,
    queryQuestionWords,
    SectionKind (..),
    sectionKindName,
    sectionAliases,
    sectionWeight,
    SectionSource (..),
    sectionSources,
    LocalAlias,
    extractLocalAliases,
    QueryPlan (..),
    classifyQuery,
    buildQueryPlan,
    ftsQuote,
    collapseWhitespace,
    retrievalImplementationFingerprint,
    retrievalFingerprintPayload,
  )
where

import Adrai.Format (renderDigest)
import Adrai.Markdown (MarkdownSections (..), extractMarkdownSections)
import Adrai.Provenance (sha256Digest)
import Adrai.Vector (identifierTerms, semanticTokens)
import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Char (GeneralCategory (DecimalNumber), generalCategory, isAlphaNum, isSpace, ord)
import Data.Foldable (toList)
import Data.List (sortOn, tails)
import qualified Data.Sequence as Sequence
import Data.Sequence ((|>))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric (showFFloat)

data QueryProfile
  = IdentifierProfile
  | KeywordProfile
  | ProseProfile
  deriving (Eq, Ord, Show, Enum, Bounded)

queryProfileName :: QueryProfile -> Text
queryProfileName IdentifierProfile = "identifier"
queryProfileName KeywordProfile = "keyword"
queryProfileName ProseProfile = "prose"

data QueryWeights = QueryWeights
  { queryWeightFtsPhrase :: Double,
    queryWeightFtsTerms :: Double,
    queryWeightFtsStemmed :: Double,
    queryWeightFtsIdentifier :: Double,
    queryWeightSemanticVector :: Double,
    queryWeightIdentifierVector :: Double
  }
  deriving (Eq, Show)

profileWeights :: QueryProfile -> QueryWeights
profileWeights IdentifierProfile = QueryWeights 0.21 0.13 0.05 0.24 0.15 0.22
profileWeights ProseProfile = QueryWeights 0.09 0.10 0.20 0.05 0.46 0.10
profileWeights KeywordProfile = QueryWeights 0.18 0.18 0.14 0.10 0.28 0.12

queryScaffolding :: Set Text
queryScaffolding =
  Set.fromList
    [ "a",
      "an",
      "and",
      "are",
      "about",
      "can",
      "could",
      "did",
      "do",
      "does",
      "for",
      "from",
      "how",
      "in",
      "is",
      "it",
      "of",
      "on",
      "our",
      "should",
      "the",
      "this",
      "to",
      "we",
      "what",
      "when",
      "where",
      "which",
      "why",
      "with",
      "would"
    ]

queryQuestionWords :: Set Text
queryQuestionWords = Set.fromList ["what", "why", "how", "where", "when", "which", "do", "does", "did", "should", "can", "could", "would"]

data SectionKind
  = TitleSummarySection
  | DecisionSection
  | RationaleSection
  | DomainsSection
  | ContextSection
  | ConsequencesSection
  | OtherSection
  deriving (Eq, Ord, Show, Enum, Bounded)

sectionKindName :: SectionKind -> Text
sectionKindName TitleSummarySection = "title-summary"
sectionKindName DecisionSection = "decision"
sectionKindName RationaleSection = "rationale"
sectionKindName DomainsSection = "domains"
sectionKindName ContextSection = "context"
sectionKindName ConsequencesSection = "consequences"
sectionKindName OtherSection = "other"

sectionWeight :: SectionKind -> Double
sectionWeight TitleSummarySection = 1.12
sectionWeight DecisionSection = 1.00
sectionWeight RationaleSection = 0.84
sectionWeight DomainsSection = 0.62
sectionWeight ContextSection = 0.55
sectionWeight ConsequencesSection = 0.45
sectionWeight OtherSection = 0.38

sectionAliases :: [(Text, Text)]
sectionAliases =
  [ ("context", "context"),
    ("background", "context"),
    ("problem", "context"),
    ("motivation", "context"),
    ("decision", "decision"),
    ("solution", "decision"),
    ("chosen approach", "decision"),
    ("approach", "decision"),
    ("consequences", "consequences"),
    ("consequence", "consequences"),
    ("trade offs", "consequences"),
    ("trade-offs", "consequences"),
    ("tradeoffs", "consequences"),
    ("outcomes", "consequences"),
    ("implications", "consequences")
  ]

data SectionSource = SectionSource
  { sectionSourceKind :: SectionKind,
    sectionSourceText :: Text,
    sectionSourceWeight :: Double
  }
  deriving (Eq, Show)

sectionSources :: Text -> Text -> Text -> [Text] -> Text -> [SectionSource]
sectionSources title summary body domains rationale =
  [ SectionSource kind text (sectionWeight kind)
    | (kind, text) <- rawSources,
      not (Text.null (Text.strip text))
  ]
  where
    parsed = extractMarkdownSections body
    titleSummary = Text.intercalate "\n" [value | value <- [title, summary], not (Text.null (Text.strip value))]
    rawSources =
      [ (TitleSummarySection, titleSummary),
        (DecisionSection, markdownDecision parsed),
        (RationaleSection, rationale),
        (DomainsSection, Text.intercalate "\n" domains),
        (ContextSection, markdownContext parsed),
        (ConsequencesSection, markdownConsequences parsed),
        (OtherSection, markdownOther parsed)
      ]

type LocalAlias = (Text, Text)

data QueryPlan = QueryPlan
  { queryPlanRaw :: Text,
    queryPlanProfile :: QueryProfile,
    queryPlanRawTokens :: [Text],
    queryPlanSemanticTerms :: [Text],
    queryPlanSemanticText :: Text,
    queryPlanIdentifierText :: Text,
    queryPlanAliases :: [Text],
    queryPlanFtsExactPhrase :: Text,
    queryPlanFtsExactTerms :: [Text],
    queryPlanFtsNear :: Text,
    queryPlanFtsPrefix :: Text,
    queryPlanFtsStemmed :: Text,
    queryPlanFtsIdentifier :: Text,
    queryPlanWeights :: QueryWeights
  }
  deriving (Eq, Show)

collapseWhitespace :: Text -> Text
collapseWhitespace = Text.unwords . Text.words

classifyQuery :: Text -> QueryProfile
classifyQuery query
  | identifierShaped query = IdentifierProfile
  | length tokens >= 6 || "?" `Text.isSuffixOf` Text.dropWhileEnd isSpace query || any (`containsBounded` lowered) (Set.toList queryQuestionWords) = ProseProfile
  | otherwise = KeywordProfile
  where
    tokens = semanticTokens query
    lowered = Text.toLower query

identifierShaped :: Text -> Bool
identifierShaped query =
  Text.any (`elem` ("_./:-" :: String)) query
    || any camelTransition adjacent
    || any upperPair adjacent
    || Text.any ((== DecimalNumber) . generalCategory) query
  where
    values = Text.unpack query
    adjacent = zip values (drop 1 values)
    camelTransition (left, right) = (isAsciiLower left || isAsciiDigit left) && isAsciiUpper right
    upperPair (left, right) = isAsciiUpper left && isAsciiUpper right

buildQueryPlan :: Text -> [LocalAlias] -> QueryPlan
buildQueryPlan query aliases =
  QueryPlan
    { queryPlanRaw = query,
      queryPlanProfile = profile,
      queryPlanRawTokens = rawTokens,
      queryPlanSemanticTerms = semanticTerms,
      queryPlanSemanticText = semanticText,
      queryPlanIdentifierText = identifierText,
      queryPlanAliases = aliasValues,
      queryPlanFtsExactPhrase = phrase,
      queryPlanFtsExactTerms = map ftsQuote exactTermsRaw,
      queryPlanFtsNear = near,
      queryPlanFtsPrefix = prefix,
      queryPlanFtsStemmed = stemmed,
      queryPlanFtsIdentifier = identifierQuery,
      queryPlanWeights = profileWeights profile
    }
  where
    rawTokens = map Text.toLower (asciiWordRuns query)
    stemmedTokens = semanticTokens query
    filteredSemantic = [token | token <- stemmedTokens, not (Set.member token queryScaffolding), Text.length token >= 2]
    semanticTerms = dedupe (if null filteredSemantic then stemmedTokens else filteredSemantic)
    filteredExact =
      [ raw
        | raw <- rawTokens,
          let normalized = case semanticTokens raw of
                [] -> raw
                value : _ -> value,
          not (Set.member normalized queryScaffolding),
          Text.length raw >= 2
      ]
    exactTermsRaw = dedupe (if null filteredExact then rawTokens else filteredExact)
    aliasValues = routeAliases query semanticTerms aliases
    augmented = Text.unwords (query : aliasValues)
    semanticText = case Text.strip augmented of
      "" -> query
      value -> value
    identifierValues = identifierTerms True augmented
    identifierText = Text.unwords identifierValues
    phrase
      | length rawTokens >= 2 = ftsQuote (Text.unwords rawTokens)
      | otherwise = ""
    near
      | length exactTermsRaw >= 2 && length exactTermsRaw <= 8 =
          "NEAR(" <> Text.unwords (map ftsQuote exactTermsRaw) <> ", 6)"
      | otherwise = ""
    prefix = Text.intercalate " OR " [ftsQuotePrefix token | token <- take 12 exactTermsRaw]
    stemmed = Text.intercalate " AND " [ftsQuote token | token <- take 12 semanticTerms]
    identifierQuery = Text.intercalate " OR " [ftsQuote token | token <- take 48 identifierValues]
    profile = classifyQuery query

routeAliases :: Text -> [Text] -> [LocalAlias] -> [Text]
routeAliases query semanticTerms aliases = dedupe (foldr route [] aliases)
  where
    lowered = Text.toLower query
    route (rawAlias, rawExpansion) rest
      | Text.null alias || Text.null expansion = rest
      | alias `elem` semanticTerms || alias `containsBounded` lowered = expansion : rest
      | expansion `Text.isInfixOf` lowered = alias : rest
      | otherwise = rest
      where
        alias = Text.toLower (collapseWhitespace rawAlias)
        expansion = Text.toLower (collapseWhitespace rawExpansion)

ftsQuote :: Text -> Text
ftsQuote value = "\"" <> Text.replace "\"" "\"\"" value <> "\""

ftsQuotePrefix :: Text -> Text
ftsQuotePrefix value = ftsQuote value <> "*"

dedupe :: [Text] -> [Text]
dedupe values = toList ordered
  where
    (_, ordered) = foldl' add (Set.empty, Sequence.empty) values
    add (seen, result) value
      | Set.member value seen || Text.null value = (seen, result)
      | otherwise = (Set.insert value seen, result |> value)

asciiWordRuns :: Text -> [Text]
asciiWordRuns input = reverse (finish current reversed)
  where
    (current, reversed) = Text.foldl' step ([], []) input
    step (characters, values) character
      | isAsciiAlphaNumeric character = (character : characters, values)
      | otherwise = ([], finish characters values)
    finish [] values = values
    finish characters values = Text.pack (reverse characters) : values

containsBounded :: Text -> Text -> Bool
containsBounded needle haystack
  | Text.null needle = False
  | otherwise = case needleChars of
      [] -> False
      firstCharacter : _ -> any (validAt firstCharacter (last needleChars)) (zip [0 :: Int ..] (tails haystackChars))
  where
    needleChars = Text.unpack needle
    haystackChars = Text.unpack haystack
    needleLength = length needleChars
    validAt firstCharacter lastCharacter (index, suffix) =
      needleChars `prefixOf` suffix
        && pythonWord firstCharacter /= maybe False pythonWord (safeIndex haystackChars (index - 1))
        && pythonWord lastCharacter /= maybe False pythonWord (safeIndex haystackChars (index + needleLength))
    prefixOf [] _ = True
    prefixOf _ [] = False
    prefixOf (left : lefts) (right : rights) = left == right && prefixOf lefts rights

safeIndex :: [value] -> Int -> Maybe value
safeIndex values index
  | index < 0 = Nothing
  | otherwise = case drop index values of
      value : _ -> Just value
      [] -> Nothing

pythonWord :: Char -> Bool
pythonWord character = isAlphaNum character || character == '_'

isAsciiLower, isAsciiUpper, isAsciiDigit, isAsciiAlphaNumeric :: Char -> Bool
isAsciiLower character = character >= 'a' && character <= 'z'
isAsciiUpper character = character >= 'A' && character <= 'Z'
isAsciiDigit character = character >= '0' && character <= '9'
isAsciiAlphaNumeric character = isAsciiLower character || isAsciiUpper character || isAsciiDigit character

extractLocalAliases :: [Text] -> [LocalAlias]
extractLocalAliases texts = toList ordered
  where
    (_, ordered) = foldl' extractText (Set.empty, Sequence.empty) texts
    extractText state text =
      let segments = parenthesizedSegments text
          afterValues = [(inside, expansion) | (before, inside) <- segments, validAcronym inside, Just expansion <- [expansionBefore before]]
          beforeValues = [(alias, inside) | (before, inside) <- segments, validExpansion inside, Just alias <- [acronymBefore before]]
       in foldl' addAlias (foldl' addAlias state afterValues) beforeValues
    addAlias (seen, values) (rawAlias, rawExpansion) =
      let alias = Text.toLower (collapseWhitespace rawAlias)
          expansion = Text.toLower (collapseWhitespace rawExpansion)
       in if Text.null alias || Text.null expansion || alias == expansion || Set.member alias seen
            then (seen, values)
            else (Set.insert alias seen, values |> (alias, expansion))

parenthesizedSegments :: Text -> [(Text, Text)]
parenthesizedSegments text = go 0
  where
    go offset = case Text.findIndex (== '(') (Text.drop offset text) of
      Nothing -> []
      Just relativeOpen ->
        let open = offset + relativeOpen
            afterOpen = Text.drop (open + 1) text
         in case Text.findIndex (== ')') afterOpen of
              Nothing -> []
              Just relativeClose ->
                let close = open + 1 + relativeClose
                    before = Text.take open text
                    inside = Text.take (close - open - 1) afterOpen
                 in (before, inside) : go (close + 1)

validAcronym :: Text -> Bool
validAcronym value =
  Text.length value >= 2
    && Text.length value <= 13
    && maybe False isAsciiUpper (Text.find (const True) value)
    && Text.all (\character -> isAsciiUpper character || isAsciiDigit character || character == '-') (Text.drop 1 value)

validExpansion :: Text -> Bool
validExpansion value =
  Text.length value >= 4
    && Text.length value <= 74
    && case Text.unpack value of
      first : second : rest -> isAsciiLetter first && isAsciiAlphaNumeric second && all expansionCharacter rest
      _ -> False

expansionBefore :: Text -> Maybe Text
expansionBefore before =
  firstValid
    [ candidate
      | (index, suffix) <- zip [0 :: Int ..] (tails (Text.unpack allowedSuffix)),
        let candidate = Text.pack suffix,
        validExpansion candidate,
        not (pythonWordAt (Text.unpack before) (baseIndex + index - 1))
    ]
  where
    trimmed = Text.dropWhileEnd isSpace before
    allowedSuffix = Text.reverse (Text.takeWhile expansionCharacter (Text.reverse trimmed))
    baseIndex = Text.length trimmed - Text.length allowedSuffix

acronymBefore :: Text -> Maybe Text
acronymBefore before =
  let trimmed = Text.dropWhileEnd isSpace before
      alias = Text.reverse (Text.takeWhile acronymCharacter (Text.reverse trimmed))
      start = Text.length trimmed - Text.length alias
   in if validAcronym alias && not (pythonWordAt (Text.unpack trimmed) (start - 1)) then Just alias else Nothing

firstValid :: [value] -> Maybe value
firstValid (value : _) = Just value
firstValid [] = Nothing

pythonWordAt :: [Char] -> Int -> Bool
pythonWordAt values index = maybe False pythonWord (safeIndex values index)

isAsciiLetter :: Char -> Bool
isAsciiLetter character = isAsciiLower character || isAsciiUpper character

expansionCharacter :: Char -> Bool
expansionCharacter character = isAsciiAlphaNumeric character || character `elem` (" /_-" :: String)

acronymCharacter :: Char -> Bool
acronymCharacter character = isAsciiUpper character || isAsciiDigit character || character == '-'

retrievalImplementationFingerprint :: Text
retrievalImplementationFingerprint = renderDigest (sha256Digest retrievalFingerprintPayload)

retrievalFingerprintPayload :: ByteString
retrievalFingerprintPayload = renderCompactJson payload
  where
    payload =
      JObject
        [ ("sections", JObject [(alias, JString section) | (alias, section) <- sectionAliases]),
          ("weights", JObject [(sectionKindName kind, JNumber (sectionWeight kind)) | kind <- [minBound .. maxBound]]),
          ("query_scaffolding", JArray (map JString (Set.toAscList queryScaffolding))),
          ( "profiles",
            JObject
              [ (queryProfileName profile, weightsJson (profileWeights profile))
                | profile <- [IdentifierProfile, KeywordProfile, ProseProfile]
              ]
          ),
          ("implementation", JString "structured-sections+three-fts+dual-vector+weighted-rrf")
        ]
    weightsJson weights =
      JObject
        [ ("fts_phrase", JNumber (queryWeightFtsPhrase weights)),
          ("fts_terms", JNumber (queryWeightFtsTerms weights)),
          ("fts_stemmed", JNumber (queryWeightFtsStemmed weights)),
          ("fts_identifier", JNumber (queryWeightFtsIdentifier weights)),
          ("semantic_vector", JNumber (queryWeightSemanticVector weights)),
          ("identifier_vector", JNumber (queryWeightIdentifierVector weights))
        ]

data CompactJson
  = JObject [(Text, CompactJson)]
  | JArray [CompactJson]
  | JString Text
  | JNumber Double

renderCompactJson :: CompactJson -> ByteString
renderCompactJson = LazyByteString.toStrict . Builder.toLazyByteString . render
  where
    render (JObject fields) =
      Builder.char8 '{'
        <> commaSeparated [renderString key <> Builder.char8 ':' <> render value | (key, value) <- sortOn fst fields]
        <> Builder.char8 '}'
    render (JArray values) = Builder.char8 '[' <> commaSeparated (map render values) <> Builder.char8 ']'
    render (JString value) = renderString value
    render (JNumber value) = Builder.string8 (showFFloat Nothing value "")
    renderString value = Builder.char8 '"' <> Text.foldl' (\builder character -> builder <> escape character) mempty value <> Builder.char8 '"'
    commaSeparated = mconcat . intersperseBuilder (Builder.char8 ',')
    intersperseBuilder _ [] = []
    intersperseBuilder separator (value : values) = value : concatMap (\next -> [separator, next]) values
    escape '"' = Builder.string8 "\\\""
    escape '\\' = Builder.string8 "\\\\"
    escape '\b' = Builder.string8 "\\b"
    escape '\f' = Builder.string8 "\\f"
    escape '\n' = Builder.string8 "\\n"
    escape '\r' = Builder.string8 "\\r"
    escape '\t' = Builder.string8 "\\t"
    escape character
      | code < 0x20 || code > 0x7f = unicodeEscape code
      | otherwise = Builder.char8 character
      where
        code = ord character
    unicodeEscape code
      | code <= 0xffff = Builder.string8 "\\u" <> hex4 code
      | otherwise =
          let adjusted = code - 0x10000
              high = 0xd800 + (adjusted `shiftR` 10)
              low = 0xdc00 + (adjusted .&. 0x3ff)
           in Builder.string8 "\\u" <> hex4 high <> Builder.string8 "\\u" <> hex4 low
    hex4 code = mconcat [Builder.char8 (hexDigit ((code `shiftR` shift) .&. 0xf)) | shift <- [12, 8, 4, 0]]
    hexDigit value
      | value < 10 = toEnum (fromEnum '0' + value)
      | otherwise = toEnum (fromEnum 'a' + value - 10)
