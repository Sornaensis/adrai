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
import Adrai.Format.Json (JsonValue (..), object, renderCanonicalJsonBytes)
import Adrai.Provenance (sha256Digest)
import Adrai.Retrieval
  ( SearchDocument (..),
    SearchMaterialization (..),
    SearchPassage (..),
  )
import Adrai.Types (adrIdText, recordIdText)
import Adrai.Vector
  ( DenseVector,
    VectorError,
    embedderVectorId,
    identifierEmbedder,
    identifierEmbedding,
    packVector,
    semanticEmbedder,
    semanticEmbedding,
    unpackVector,
  )
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
  summaries <- buildVectors "semantic summary" searchDocumentItemId (semanticEmbedding . searchVectorSemanticSummaryText) documents
  identifiers <- buildVectors "identifier" searchDocumentItemId (identifierEmbedding . searchVectorIdentifierSourceText) documents
  sections <- buildVectors "semantic section" searchPassageId (semanticEmbedding . searchPassageText) passages
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

buildVectors :: Text -> (value -> Text) -> (value -> DenseVector) -> [value] -> Either SearchVectorCorpusError (Map Text DenseVector)
buildVectors context key embedValue values =
  Map.fromAscList <$> traverse buildOne values
  where
    buildOne value = do
      vector <- mapLeft (SearchVectorCorpusVectorFailure (context <> " " <> key value)) (unpackVector (packVector (embedValue value)))
      Right (key value, vector)

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
  renderDigest (sha256Digest (renderCanonicalJsonBytes payload))
  where
    payload =
      object
        [ ("schema", JsonString "adrai/search-vector-corpus/v1"),
          ("semantic_vector_id", JsonString currentSemanticVectorId),
          ("identifier_vector_id", JsonString currentIdentifierVectorId),
          ("documents", JsonArray (map documentFingerprint documents)),
          ("passages", JsonArray (map passageFingerprint passages))
        ]
    documents = sortOn documentIdentity (searchMaterializationDocuments materialization)
    passages = sortOn passageIdentity (searchMaterializationPassages materialization)

documentFingerprint :: SearchDocument -> JsonValue
documentFingerprint document =
  object
    [ ("item_id", JsonString (searchDocumentItemId document)),
      ("adr_id", JsonString (adrIdText (searchDocumentAdrId document))),
      ("candidate_record_id", JsonString (recordIdText (searchDocumentCandidateRecordId document))),
      ("semantic_input", JsonString (searchVectorSemanticSummaryText document)),
      ("identifier_input", JsonString (searchVectorIdentifierSourceText document))
    ]

passageFingerprint :: SearchPassage -> JsonValue
passageFingerprint passage =
  object
    [ ("passage_id", JsonString (searchPassageId passage)),
      ("document_item_id", JsonString (searchPassageDocumentItemId passage)),
      ("adr_id", JsonString (adrIdText (searchPassageAdrId passage))),
      ("candidate_record_id", JsonString (recordIdText (searchPassageCandidateRecordId passage))),
      ("semantic_input", JsonString (searchPassageText passage))
    ]

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

mapLeft :: (left -> right) -> Either left value -> Either right value
mapLeft transform = either (Left . transform) Right
