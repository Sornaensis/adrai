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
    sectionSourcesFromSections,
    LocalAlias,
    extractLocalAliases,
    SearchDocument (..),
    SearchPassage (..),
    SearchMaterialization (..),
    searchDocumentAliasTexts,
    materializationAliases,
    chunkSearchDocument,
    materializationImplementationFingerprint,
    materializationFingerprintPayload,
    QueryPlan (..),
    classifyQuery,
    buildQueryPlan,
    RetrievalMode (..),
    retrievalModeName,
    retrievalModeChannels,
    RetrievalChannel (..),
    retrievalChannelName,
    retrievalChannelWeight,
    RrfEvidence (..),
    weightedReciprocalRankFusion,
    rrfK,
    SemanticSectionScore (..),
    SemanticEvidence (..),
    detailedSemanticScore,
    LexicalEvidence (..),
    lexicalEvidence,
    agreementBonus,
    sectionShortlistLimit,
    forcedChannelLimit,
    fieldRerankLimit,
    rankingImplementationFingerprint,
    rankingFingerprintPayload,
    ftsQuote,
    collapseWhitespace,
    informativeTerms,
    retrievalImplementationFingerprint,
    retrievalFingerprintPayload,
  )
where

import Adrai.Format (renderDigest)
import Adrai.Markdown (MarkdownSections (..), extractMarkdownSections)
import Adrai.Provenance (sha256Digest)
import Adrai.Relevance (ChunkError, TextChunk (..), chunkText, chunkBoundaryRadius, chunkOverlapChars, maxTextBytes, targetChunkChars)
import Adrai.Types (AdrId, RecordId, StateToken, adrIdText, recordIdText)
import Adrai.Vector (identifierTerms, semanticTokens)
import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Char (GeneralCategory (DecimalNumber), generalCategory, isAlphaNum, isSpace, ord)
import Data.Foldable (toList)
import Data.List (find, sortBy, sortOn, tails)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Ord (Down (..), comparing)
import qualified Data.Sequence as Sequence
import Data.Sequence ((|>))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
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

informativeTerms :: Int -> Text -> [Text]
informativeTerms limit text =
  map snd . take (max 0 limit) . sortBy (comparing (Down . fst) <> comparing (Down . snd)) $
    [ (termScore term, term)
      | term <- Set.toList (Set.union semanticSet identifierSet),
        Text.length term >= 3,
        not (Set.member term informativeStop)
    ]
  where
    semantic = semanticTokens text
    semanticSet = Set.fromList semantic
    identifierSet = Set.fromList (identifierTerms False text)
    counts = Map.fromListWith (+) [(term, 1 :: Int) | term <- semantic]
    termScore :: Text -> Double
    termScore term =
      fromIntegral (min (Text.length term) 20) / 10
        + (if Set.member term identifierSet then 1.5 else 0)
        + 1 / fromIntegral (max 1 (Map.findWithDefault 1 term counts))

informativeStop :: Set Text
informativeStop =
  Set.union
    queryScaffolding
    ( Set.fromList
        [ "application",
          "architecture",
          "component",
          "data",
          "decision",
          "implementation",
          "record",
          "service",
          "system",
          "value"
        ]
    )

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
sectionSources title summary body domains rationale = sectionSourcesFromSections title summary (extractMarkdownSections body) domains rationale

sectionSourcesFromSections :: Text -> Text -> MarkdownSections -> [Text] -> Text -> [SectionSource]
sectionSourcesFromSections title summary parsed domains rationale =
  [ SectionSource kind text (sectionWeight kind)
    | (kind, text) <- rawSources,
      not (Text.null (Text.strip text))
  ]
  where
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

data SearchDocument = SearchDocument
  { searchDocumentItemId :: Text,
    searchDocumentAdrId :: AdrId,
    searchDocumentCandidateRecordId :: RecordId,
    searchDocumentTitle :: Text,
    searchDocumentSummary :: Text,
    searchDocumentContext :: Text,
    searchDocumentDecision :: Text,
    searchDocumentConsequences :: Text,
    searchDocumentDomains :: [Text],
    searchDocumentRationale :: Text,
    searchDocumentOther :: Text,
    searchDocumentScope :: [Text],
    searchDocumentObsolete :: Bool,
    searchDocumentConflicted :: Bool,
    searchDocumentStateToken :: StateToken,
    searchDocumentSourcePaths :: [Text],
    searchDocumentIdentifierSource :: Text,
    searchDocumentIdentifiers :: Text
  }
  deriving (Eq, Show)

data SearchPassage = SearchPassage
  { searchPassageId :: Text,
    searchPassageDocumentItemId :: Text,
    searchPassageAdrId :: AdrId,
    searchPassageCandidateRecordId :: RecordId,
    searchPassageSectionKind :: SectionKind,
    searchPassageOrdinal :: Int,
    searchPassageLineStart :: Int,
    searchPassageLineEnd :: Int,
    searchPassageText :: Text,
    searchPassageWeight :: Double,
    searchPassageSourcePaths :: [Text],
    searchPassageIdentifiers :: Text
  }
  deriving (Eq, Show)

data SearchMaterialization = SearchMaterialization
  { searchMaterializationDocuments :: [SearchDocument],
    searchMaterializationPassages :: [SearchPassage],
    searchMaterializationAliases :: [LocalAlias]
  }
  deriving (Eq, Show)

searchDocumentAliasTexts :: SearchDocument -> [Text]
searchDocumentAliasTexts document =
  [ searchDocumentTitle document,
    searchDocumentSummary document,
    searchDocumentContext document,
    searchDocumentDecision document,
    searchDocumentConsequences document,
    searchDocumentRationale document,
    searchDocumentIdentifiers document
  ]

materializationAliases :: [SearchDocument] -> [LocalAlias]
materializationAliases = sortOn fst . extractLocalAliases . concatMap searchDocumentAliasTexts . sortOn documentKey
  where
    documentKey document = (adrIdText (searchDocumentAdrId document), recordIdText (searchDocumentCandidateRecordId document))

chunkSearchDocument :: SearchDocument -> Either ChunkError [SearchPassage]
chunkSearchDocument document = concat <$> traverse chunkSource sources
  where
    sections =
      MarkdownSections
        { markdownContext = searchDocumentContext document,
          markdownDecision = searchDocumentDecision document,
          markdownConsequences = searchDocumentConsequences document,
          markdownOther = searchDocumentOther document
        }
    sources =
      sectionSourcesFromSections
        (searchDocumentTitle document)
        (searchDocumentSummary document)
        sections
        (searchDocumentDomains document)
        (searchDocumentRationale document)
    chunkSource source = map (toPassage source) <$> chunkText (sectionSourceText source)
    toPassage source chunk =
      SearchPassage
        { searchPassageId =
            searchDocumentItemId document
              <> "/section/"
              <> sectionKindName (sectionSourceKind source)
              <> "/"
              <> decimal (textChunkOrdinal chunk)
              <> "/"
              <> decimal (textChunkStartLine chunk)
              <> "-"
              <> decimal (textChunkEndLine chunk),
          searchPassageDocumentItemId = searchDocumentItemId document,
          searchPassageAdrId = searchDocumentAdrId document,
          searchPassageCandidateRecordId = searchDocumentCandidateRecordId document,
          searchPassageSectionKind = sectionSourceKind source,
          searchPassageOrdinal = textChunkOrdinal chunk,
          searchPassageLineStart = textChunkStartLine chunk,
          searchPassageLineEnd = textChunkEndLine chunk,
          searchPassageText = textChunkText chunk,
          searchPassageWeight = sectionSourceWeight source,
          searchPassageSourcePaths = searchDocumentSourcePaths document,
          searchPassageIdentifiers = Text.unwords (identifierTerms True (textChunkText chunk))
        }

decimal :: Int -> Text
decimal = Text.pack . show

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

-- | Public retrieval modes supported by the collapsed current-search service.
data RetrievalMode
  = FtsRetrieval
  | VectorRetrieval
  | HybridRetrieval
  deriving (Eq, Ord, Show, Enum, Bounded)

retrievalModeName :: RetrievalMode -> Text
retrievalModeName FtsRetrieval = "fts"
retrievalModeName VectorRetrieval = "vector"
retrievalModeName HybridRetrieval = "hybrid"

data RetrievalChannel
  = FtsPhraseChannel
  | FtsTermsChannel
  | FtsStemmedChannel
  | FtsIdentifierChannel
  | SemanticVectorChannel
  | IdentifierVectorChannel
  deriving (Eq, Ord, Show, Enum, Bounded)

retrievalChannelName :: RetrievalChannel -> Text
retrievalChannelName FtsPhraseChannel = "fts_phrase"
retrievalChannelName FtsTermsChannel = "fts_terms"
retrievalChannelName FtsStemmedChannel = "fts_stemmed"
retrievalChannelName FtsIdentifierChannel = "fts_identifier"
retrievalChannelName SemanticVectorChannel = "semantic_vector"
retrievalChannelName IdentifierVectorChannel = "identifier_vector"

retrievalModeChannels :: RetrievalMode -> [RetrievalChannel]
retrievalModeChannels FtsRetrieval = [FtsPhraseChannel, FtsTermsChannel, FtsStemmedChannel, FtsIdentifierChannel]
retrievalModeChannels VectorRetrieval = [SemanticVectorChannel, IdentifierVectorChannel]
retrievalModeChannels HybridRetrieval = [minBound .. maxBound]

retrievalChannelWeight :: QueryWeights -> RetrievalChannel -> Double
retrievalChannelWeight weights channel =
  case channel of
    FtsPhraseChannel -> queryWeightFtsPhrase weights
    FtsTermsChannel -> queryWeightFtsTerms weights
    FtsStemmedChannel -> queryWeightFtsStemmed weights
    FtsIdentifierChannel -> queryWeightFtsIdentifier weights
    SemanticVectorChannel -> queryWeightSemanticVector weights
    IdentifierVectorChannel -> queryWeightIdentifierVector weights

rrfK :: Double
rrfK = 20

data RrfEvidence = RrfEvidence
  { rrfScore :: Double,
    rrfChannelRanks :: Map RetrievalChannel Int,
    rrfChannelContributions :: Map RetrievalChannel Double
  }
  deriving (Eq, Show)

-- | Weighted reciprocal-rank fusion.  Only active, non-empty, positive-weight
-- channels participate in normalization.  Raw scores establish rank only;
-- equal scores use item id descending, exactly like the prototype.
weightedReciprocalRankFusion :: QueryWeights -> Map RetrievalChannel (Map Text Double) -> Map Text RrfEvidence
weightedReciprocalRankFusion weights channels
  | totalWeight <= 0 = Map.empty
  | otherwise = foldl' addChannel Map.empty active
  where
    active =
      [ (channel, weight, scores)
        | channel <- [minBound .. maxBound],
          let weight = retrievalChannelWeight weights channel,
          weight > 0,
          let scores = Map.findWithDefault Map.empty channel channels,
          not (Map.null scores)
      ]
    totalWeight = sum [weight | (_, weight, _) <- active]
    addChannel result (channel, weight, scores) =
      foldl' (addRank channel (weight / totalWeight)) result (zip [1 ..] (rankedScores scores))
    addRank channel normalized result (rank, (itemId, _rawScore)) =
      Map.alter (Just . updateEvidence) itemId result
      where
        contribution = normalized / (rrfK + fromIntegral rank)
        updateEvidence Nothing = RrfEvidence contribution (Map.singleton channel rank) (Map.singleton channel contribution)
        updateEvidence (Just evidence) =
          evidence
            { rrfScore = rrfScore evidence + contribution,
              rrfChannelRanks = Map.insert channel rank (rrfChannelRanks evidence),
              rrfChannelContributions = Map.insert channel contribution (rrfChannelContributions evidence)
            }
    rankedScores =
      sortBy
        (comparing (Down . snd) <> comparing (Down . fst))
        . Map.toList

data SemanticSectionScore = SemanticSectionScore
  { semanticSectionKind :: SectionKind,
    semanticSectionId :: Text,
    semanticSectionText :: Text,
    semanticSectionScore :: Double
  }
  deriving (Eq, Show)

data SemanticEvidence = SemanticEvidence
  { semanticEvidenceScore :: Double,
    semanticBestSection :: Maybe SemanticSectionScore,
    semanticSupportingSection :: Maybe SemanticSectionScore
  }
  deriving (Eq, Show)

detailedSemanticScore :: Double -> [SemanticSectionScore] -> SemanticEvidence
detailedSemanticScore summaryScore sections =
  SemanticEvidence
    { semanticEvidenceScore = min 1 (max (summaryScore * 0.92) bestScore + supportingBonus),
      semanticBestSection = best,
      semanticSupportingSection = supporting
    }
  where
    ordered =
      sortBy
        ( comparing (Down . semanticSectionScore)
            <> comparing (Down . sectionKindName . semanticSectionKind)
            <> comparing (Down . semanticSectionId)
            <> comparing (Down . semanticSectionText)
        )
        sections
    best = firstValid ordered
    bestScore = maybe (negate (1 / 0)) semanticSectionScore best
    supporting = case best of
      Nothing -> Nothing
      Just bestSection -> find ((/= semanticSectionId bestSection) . semanticSectionId) (drop 1 ordered)
    supportingBonus = maybe 0 (max 0 . (* 0.10) . semanticSectionScore) supporting

data LexicalEvidence = LexicalEvidence
  { lexicalMatchedFields :: [Text],
    lexicalMatchedTerms :: [Text],
    lexicalExactPhraseFields :: [Text],
    lexicalIdentifierTerms :: [Text],
    lexicalCoverage :: Double,
    lexicalBonus :: Double
  }
  deriving (Eq, Show)

lexicalEvidence :: QueryPlan -> [(Text, Text)] -> LexicalEvidence
lexicalEvidence plan fields =
  LexicalEvidence
    { lexicalMatchedFields = Set.toAscList (Set.fromList matchedFields),
      lexicalMatchedTerms = take 16 allMatchedTerms,
      lexicalExactPhraseFields = exactPhraseFields,
      lexicalIdentifierTerms = take 16 identifierOverlap,
      lexicalCoverage = coverage,
      lexicalBonus = bonus
    }
  where
    normalizedFields = [(name, Text.toLower (collapseWhitespace value)) | (name, value) <- fields]
    phrase = Text.toLower (Text.unwords (queryPlanRawTokens plan))
    exactPhraseFields =
      [ name
        | (name, value) <- normalizedFields,
          length (queryPlanRawTokens plan) >= 2,
          not (Text.null phrase),
          phrase `Text.isInfixOf` value
      ]
    semanticTermSet = Set.fromList (queryPlanSemanticTerms plan)
    fieldMatches =
      [ (name, Set.toAscList (Set.intersection semanticTermSet (Set.fromList (semanticTokens value))))
        | (name, value) <- normalizedFields
      ]
    matchedFields = [name | (name, terms) <- fieldMatches, not (null terms)]
    identifierQuerySet = Set.fromList (Text.words (Text.toLower (queryPlanIdentifierText plan)))
    indexedIdentifierSet =
      Set.fromList
        ( Text.words
            ( Text.toLower
                (maybe "" snd (find ((== "identifiers") . fst) fields))
            )
        )
    identifierOverlap = Set.toAscList (Set.intersection identifierQuerySet indexedIdentifierSet)
    allMatchedTerms = dedupe (concatMap snd fieldMatches <> identifierOverlap)
    coveredTerms = Set.intersection semanticTermSet (Set.fromList allMatchedTerms)
    coverage
      | Set.null semanticTermSet = 0
      | otherwise = fromIntegral (Set.size coveredTerms) / fromIntegral (Set.size semanticTermSet)
    bonus =
      min
        0.012
        ( (if null exactPhraseFields then 0 else 0.004)
            + min 0.004 (coverage * 0.004)
            + min 0.004 (fromIntegral (length identifierOverlap) * 0.0015)
        )

agreementBonus :: Int -> Double
agreementBonus participatingChannels = min 0.006 (fromIntegral (max 0 (participatingChannels - 1)) * 0.0015)

sectionShortlistLimit :: Int -> Int -> Int
sectionShortlistLimit allowedCount limit = min allowedCount (max 120 (limit * 12))

forcedChannelLimit :: Int -> Int
forcedChannelLimit limit = min 10 (max 3 limit)

fieldRerankLimit :: Int -> Int -> Int
fieldRerankLimit candidateCount limit = min candidateCount (max 80 (limit * 8))

rankingImplementationFingerprint :: Text
rankingImplementationFingerprint = renderDigest (sha256Digest rankingFingerprintPayload)

rankingFingerprintPayload :: ByteString
rankingFingerprintPayload = renderCompactJson payload
  where
    payload =
      JObject
        [ ("contract", JString "adrai-search-ranking/v1"),
          ("rrf_k", JNumber rrfK),
          ("channels", JArray [JString (retrievalChannelName channel) | channel <- [minBound .. maxBound]]),
          ("active_weight_normalization", JString "positive-nonempty"),
          ("channel_tie_break", JString "raw-score-desc,item-id-desc"),
          ("candidate_tie_break", JString "fused,semantic,max-fts,item-id-desc"),
          ("logical_tie_break", JString "fused,semantic,adr-id-desc"),
          ("section_shortlist", JString "min(allowed,max(120,limit*12))"),
          ("forced_per_channel", JString "min(10,max(3,limit))"),
          ("field_rerank", JString "min(candidates,max(80,limit*8))+conflict-heads"),
          ("lexical_bonus_cap", JNumber 0.012),
          ("agreement_bonus_cap", JNumber 0.006),
          ("score_rounding", JString "six-decimal-ties-to-even")
        ]

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

materializationImplementationFingerprint :: Text
materializationImplementationFingerprint = renderDigest (sha256Digest materializationFingerprintPayload)

materializationFingerprintPayload :: ByteString
materializationFingerprintPayload =
  TextEncoding.encodeUtf8
    ( Text.intercalate
        "\n"
        [ "adrai-search-materialization/v1",
          "resolved-item=ADR",
          "conflict-item=ADR@RID",
          "relations=amends,applies_to,domains,status",
          "alias-fields=title,summary,context,decision,consequences,rationale,identifiers",
          "sections=title-summary,decision,rationale,domains,context,consequences,other",
          "chunk-max-bytes=" <> decimal maxTextBytes,
          "chunk-target-chars=" <> decimal targetChunkChars,
          "chunk-overlap-chars=" <> decimal chunkOverlapChars,
          "chunk-boundary-radius=" <> decimal chunkBoundaryRadius,
          "passage-id=ITEM/section/KIND/ORDINAL/LINE_START-LINE_END",
          "obsolete=stored",
          "fts=three-summary+three-passage"
        ]
        <> "\n"
    )

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
