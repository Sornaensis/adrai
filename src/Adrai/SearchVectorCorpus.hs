{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | A deterministic, reusable, in-memory vector corpus for one search
-- materialization.  Corpus vectors are quantized through the release float32
-- representation at construction time; request vectors remain ephemeral.
module Adrai.SearchVectorCorpus
  ( SearchVectorCorpus,
    SearchVectorCorpusError (..),
    buildSearchVectorCorpus,
    validateSearchVectorCorpus,
    searchVectorCorpusFingerprint,
    searchVectorCorpusSemanticVectorId,
    searchVectorCorpusIdentifierVectorId,
    searchVectorCorpusSummaryVectors,
    searchVectorCorpusIdentifierVectors,
    searchVectorCorpusSectionVectors,
    searchVectorSemanticSummaryText,
    searchVectorIdentifierSourceText,
  )
where

import Adrai.Format (renderDigest)
import Adrai.Format.Json (JsonValue (JsonString), renderCanonicalJsonBytes)
import Adrai.Provenance (sha256DigestFrames)
import Adrai.Retrieval
  ( SearchDocument (..),
    SearchMaterialization (..),
    SearchPassage (..),
  )
import Adrai.Types (adrIdText, recordIdText)
import Adrai.Vector
  ( DenseVector,
    Embedder,
    VectorError,
    canonicalFloat32Vector,
    embed,
    embedderVectorId,
    identifierEmbedder,
    semanticEmbedder,
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

data SearchVectorCorpus = SearchVectorCorpus
  { searchVectorCorpusFingerprint :: Text,
    searchVectorCorpusSemanticVectorId :: Text,
    searchVectorCorpusIdentifierVectorId :: Text,
    searchVectorCorpusSummaryVectors :: Map Text DenseVector,
    searchVectorCorpusIdentifierVectors :: Map Text DenseVector,
    searchVectorCorpusSectionVectors :: Map Text DenseVector
  }
  deriving (Eq, Show)

data SearchVectorCorpusError
  = SearchVectorCorpusDuplicateDocumentItemId Text
  | SearchVectorCorpusDuplicatePassageId Text
  | SearchVectorCorpusVectorFailure Text VectorError
  | SearchVectorCorpusMissingSummaryVector Text
  | SearchVectorCorpusMissingIdentifierVector Text
  | SearchVectorCorpusMissingSectionVector Text
  | SearchVectorCorpusUnexpectedSummaryVector Text
  | SearchVectorCorpusUnexpectedIdentifierVector Text
  | SearchVectorCorpusUnexpectedSectionVector Text
  | SearchVectorCorpusMaterializationIncompatible Text
  | SearchVectorCorpusSemanticVectorIdMismatch Text Text
  | SearchVectorCorpusIdentifierVectorIdMismatch Text Text
  | SearchVectorCorpusFingerprintMismatch Text Text
  deriving (Eq, Show)

buildSearchVectorCorpus :: SearchMaterialization -> Either SearchVectorCorpusError SearchVectorCorpus
buildSearchVectorCorpus materialization = do
  validateUniqueKeys materialization
  summaries <- buildVectors "semantic summary" searchDocumentItemId semanticEmbedder searchVectorSemanticSummaryText documents
  identifiers <- buildVectors "identifier" searchDocumentItemId identifierEmbedder searchVectorIdentifierSourceText documents
  sections <- buildVectors "semantic section" searchPassageId semanticEmbedder searchPassageText passages
  Right
    SearchVectorCorpus
      { searchVectorCorpusFingerprint = compatibilityFingerprint materialization,
        searchVectorCorpusSemanticVectorId = currentSemanticVectorId,
        searchVectorCorpusIdentifierVectorId = currentIdentifierVectorId,
        searchVectorCorpusSummaryVectors = summaries,
        searchVectorCorpusIdentifierVectors = identifiers,
        searchVectorCorpusSectionVectors = sections
      }
  where
    documents = sortOn searchDocumentItemId (searchMaterializationDocuments materialization)
    passages = sortOn searchPassageId (searchMaterializationPassages materialization)

validateSearchVectorCorpus :: SearchMaterialization -> SearchVectorCorpus -> Either SearchVectorCorpusError ()
validateSearchVectorCorpus materialization corpus = do
  validateUniqueKeys materialization
  if searchVectorCorpusSemanticVectorId corpus == currentSemanticVectorId
    then Right ()
    else Left (SearchVectorCorpusSemanticVectorIdMismatch currentSemanticVectorId (searchVectorCorpusSemanticVectorId corpus))
  if searchVectorCorpusIdentifierVectorId corpus == currentIdentifierVectorId
    then Right ()
    else Left (SearchVectorCorpusIdentifierVectorIdMismatch currentIdentifierVectorId (searchVectorCorpusIdentifierVectorId corpus))
  validateKeys
    SearchVectorCorpusMissingSummaryVector
    SearchVectorCorpusUnexpectedSummaryVector
    documentKeys
    (Map.keysSet (searchVectorCorpusSummaryVectors corpus))
  validateKeys
    SearchVectorCorpusMissingIdentifierVector
    SearchVectorCorpusUnexpectedIdentifierVector
    documentKeys
    (Map.keysSet (searchVectorCorpusIdentifierVectors corpus))
  validateKeys
    SearchVectorCorpusMissingSectionVector
    SearchVectorCorpusUnexpectedSectionVector
    passageKeys
    (Map.keysSet (searchVectorCorpusSectionVectors corpus))
  let expectedFingerprint = compatibilityFingerprint materialization
  if searchVectorCorpusFingerprint corpus == expectedFingerprint
    then Right ()
    else Left (SearchVectorCorpusFingerprintMismatch expectedFingerprint (searchVectorCorpusFingerprint corpus))
  where
    documentKeys = Set.fromList (map searchDocumentItemId (searchMaterializationDocuments materialization))
    passageKeys = Set.fromList (map searchPassageId (searchMaterializationPassages materialization))

searchVectorSemanticSummaryText :: SearchDocument -> Text
searchVectorSemanticSummaryText document =
  Text.intercalate
    "\n"
    ( filter
        (not . Text.null . Text.strip)
        [ searchDocumentTitle document,
          searchDocumentSummary document,
          searchDocumentDecision document,
          Text.unwords (searchDocumentDomains document),
          searchDocumentRationale document
        ]
    )

-- | The raw, newline-delimited compiler input used by the identifier
-- embedder.  The normalized identifier field remains the lexical/FTS input.
searchVectorIdentifierSourceText :: SearchDocument -> Text
searchVectorIdentifierSourceText = searchDocumentIdentifierSource

buildVectors :: Text -> (value -> Text) -> Embedder -> (value -> Text) -> [value] -> Either SearchVectorCorpusError (Map Text DenseVector)
buildVectors _context key embedder vectorText values =
  Map.fromAscList <$> traverse buildOne values
  where
    buildOne value = Right (key value, canonicalFloat32Vector (embed embedder (vectorText value)))

validateUniqueKeys :: SearchMaterialization -> Either SearchVectorCorpusError ()
validateUniqueKeys materialization = do
  validateUnique SearchVectorCorpusDuplicateDocumentItemId (map searchDocumentItemId (searchMaterializationDocuments materialization))
  validateUnique SearchVectorCorpusDuplicatePassageId (map searchPassageId (searchMaterializationPassages materialization))

validateUnique :: (Text -> SearchVectorCorpusError) -> [Text] -> Either SearchVectorCorpusError ()
validateUnique duplicateError = go Set.empty
  where
    go _ [] = Right ()
    go seen (key : remaining)
      | Set.member key seen = Left (duplicateError key)
      | otherwise = go (Set.insert key seen) remaining

validateKeys :: (Text -> SearchVectorCorpusError) -> (Text -> SearchVectorCorpusError) -> Set Text -> Set Text -> Either SearchVectorCorpusError ()
validateKeys missingError unexpectedError expected actual =
  case Set.lookupMin (Set.difference expected actual) of
    Just key -> Left (missingError key)
    Nothing ->
      case Set.lookupMin (Set.difference actual expected) of
        Just key -> Left (unexpectedError key)
        Nothing -> Right ()

compatibilityFingerprint :: SearchMaterialization -> Text
compatibilityFingerprint materialization =
  renderDigest (sha256DigestFrames (compatibilityFingerprintFrames materialization))

-- | The exact canonical JSON byte sequence previously built as one large
-- Text and ByteString.  Frames are fed lazily to SHA-256, so memory is bounded
-- by one encoded field instead of the entire search materialization.
compatibilityFingerprintFrames :: SearchMaterialization -> [ByteString]
compatibilityFingerprintFrames materialization =
  [ "{\n",
    "  \"documents\": "
  ]
    <> canonicalArrayFrames documentFingerprintFrames documents
    <> [ ",\n",
         "  \"identifier_vector_id\": ", jsonString currentIdentifierVectorId, ",\n",
         "  \"passages\": "
       ]
    <> canonicalArrayFrames passageFingerprintFrames passages
    <> [ ",\n",
         "  \"schema\": \"adrai/search-vector-corpus/v1\",\n",
         "  \"semantic_vector_id\": ", jsonString currentSemanticVectorId, "\n}\n"
       ]
  where
    documents = sortOn documentIdentity (searchMaterializationDocuments materialization)
    passages = sortOn passageIdentity (searchMaterializationPassages materialization)

canonicalArrayFrames :: (value -> [ByteString]) -> [value] -> [ByteString]
canonicalArrayFrames _ [] = ["[]"]
canonicalArrayFrames renderValue values =
  ["[\n"] <> framedArray "    " renderValue values <> ["\n  ]"]

framedArray :: ByteString -> (value -> [ByteString]) -> [value] -> [ByteString]
framedArray _ _ [] = []
framedArray indent renderValue (first : rest) =
  [indent] <> renderValue first <> concatMap renderRest rest
  where
    renderRest value = [",\n", indent] <> renderValue value

documentFingerprintFrames :: SearchDocument -> [ByteString]
documentFingerprintFrames document =
  framedObject
    [ ("adr_id", adrIdText (searchDocumentAdrId document)),
      ("candidate_record_id", recordIdText (searchDocumentCandidateRecordId document)),
      ("identifier_input", searchVectorIdentifierSourceText document),
      ("item_id", searchDocumentItemId document),
      ("semantic_input", searchVectorSemanticSummaryText document)
    ]

passageFingerprintFrames :: SearchPassage -> [ByteString]
passageFingerprintFrames passage =
  framedObject
    [ ("adr_id", adrIdText (searchPassageAdrId passage)),
      ("candidate_record_id", recordIdText (searchPassageCandidateRecordId passage)),
      ("document_item_id", searchPassageDocumentItemId passage),
      ("passage_id", searchPassageId passage),
      ("semantic_input", searchPassageText passage)
    ]

framedObject :: [(Text, Text)] -> [ByteString]
framedObject members =
  [ "{\n" ]
    <> concatMap renderMember (zip [0 :: Int ..] members)
    <> [ "\n    }" ]
  where
    renderMember (index, (key, value)) =
      [ if index == 0 then "      " else ",\n      ",
        jsonString key,
        ": ",
        jsonString value
      ]

jsonString :: Text -> ByteString
jsonString = ByteString.init . renderCanonicalJsonBytes . JsonString

documentIdentity :: SearchDocument -> (Text, Text, Text)
documentIdentity document =
  ( searchDocumentItemId document,
    adrIdText (searchDocumentAdrId document),
    recordIdText (searchDocumentCandidateRecordId document)
  )

passageIdentity :: SearchPassage -> (Text, Text, Text, Text)
passageIdentity passage =
  ( searchPassageId passage,
    searchPassageDocumentItemId passage,
    adrIdText (searchPassageAdrId passage),
    recordIdText (searchPassageCandidateRecordId passage)
  )

currentSemanticVectorId :: Text
currentSemanticVectorId = embedderVectorId semanticEmbedder

currentIdentifierVectorId :: Text
currentIdentifierVectorId = embedderVectorId identifierEmbedder
