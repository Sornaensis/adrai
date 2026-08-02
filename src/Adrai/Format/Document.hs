{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Adrai.Format.Document
  ( DecisionRecord (..),
    ConnectionRecord (..),
    ConnectionPayload (..),
    AmendsPayload (..),
    AppliesToPayload (..),
    DomainsPayload (..),
    StatusPayload (..),
    StatusState (..),
    ManagedRecord (..),
    ParsedManagedDocument (..),
    DocumentError (..),
    parseFrontMatter,
    renderDecisionSemantic,
    renderConnectionSemantic,
    renderManagedSemantic,
    parseManagedDocument,
    sealManagedDocument,
    canonicalManagedPath,
  )
where

import Adrai.Domain
  ( Domain,
    DomainError,
    DomainRefinement,
    canonicalDomains,
    domainRefinementText,
    domainText,
    parseDomainRefinement,
  )
import Adrai.Format.Toml
  ( Toml10Violation,
    renderTomlString,
    renderTomlStringArray,
    validateToml10,
  )
import Adrai.Provenance
  ( ProvenanceCapsule,
    ProvenanceError,
    decodeCapsule,
    normalizeSemantic,
    sealSemantic,
    semanticDigest,
    validateCapsule,
  )
import Adrai.Scope
  ( ScopePattern,
    ScopePatternError,
    mkScopePattern,
    scopePatternText,
  )
import Adrai.Types
  ( AdrId,
    ConnectionId,
    IdViolation,
    ManagedPaths,
    ObjectRef,
    RecordId,
    RepoPath,
    RepoPathViolation,
    adrIdText,
    connectionIdText,
    connectionObjectRef,
    mkAdrId,
    mkConnectionId,
    mkRecordId,
    mkRepoPath,
    managedConnectionPath,
    managedDecisionPath,
    repoPathText,
    recordIdText,
    recordObjectRef,
  )
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Char (isAsciiLower, isDigit)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Toml

data DecisionRecord = DecisionRecord
  { decisionAdr :: AdrId,
    decisionRecord :: RecordId,
    decisionTitle :: Text,
    decisionSummary :: Text,
    decisionDomains :: [Domain],
    decisionBody :: Text
  }
  deriving (Eq, Show)

data ConnectionRecord = ConnectionRecord
  { connectionRecordId :: ConnectionId,
    connectionPayload :: ConnectionPayload,
    connectionRationale :: Text
  }
  deriving (Eq, Show)

data ConnectionPayload
  = AmendsConnection AmendsPayload
  | AppliesToConnection AppliesToPayload
  | DomainsConnection DomainsPayload
  | StatusConnection StatusPayload
  deriving (Eq, Show)

data AmendsPayload = AmendsPayload
  { amendsSubjectAdr :: AdrId,
    amendsFromRecord :: RecordId,
    amendsToRecords :: [RecordId]
  }
  deriving (Eq, Show)

data AppliesToPayload = AppliesToPayload
  { appliesToSubjectAdr :: AdrId,
    appliesToParentConnections :: [ConnectionId],
    appliesToChange :: Text,
    appliesToAdded :: [ScopePattern],
    appliesToRemoved :: [ScopePattern],
    appliesToEffective :: [ScopePattern]
  }
  deriving (Eq, Show)

data DomainsPayload = DomainsPayload
  { domainsSubjectAdr :: AdrId,
    domainsParentConnections :: [ConnectionId],
    domainsChange :: Text,
    domainsAdded :: [Domain],
    domainsRemoved :: [Domain],
    domainsEffective :: [Domain],
    domainsRefinements :: [DomainRefinement]
  }
  deriving (Eq, Show)

data StatusState
  = StatusActive
  | StatusObsolete
  deriving (Eq, Ord, Show)

data StatusPayload = StatusPayload
  { statusSubjectAdr :: AdrId,
    statusParentConnections :: [ConnectionId],
    statusState :: StatusState,
    statusRecordHeads :: [RecordId],
    statusReplacementAdr :: Maybe AdrId
  }
  deriving (Eq, Show)

data ManagedRecord
  = ManagedDecision DecisionRecord
  | ManagedConnection ConnectionRecord
  deriving (Eq, Show)

data ParsedManagedDocument = ParsedManagedDocument
  { parsedManagedPath :: RepoPath,
    parsedManagedRecord :: ManagedRecord,
    parsedManagedCapsule :: ProvenanceCapsule,
    parsedManagedSemantic :: Text,
    parsedManagedBytes :: ByteString
  }
  deriving (Eq, Show)

data DocumentError
  = DocumentInvalidUtf8 Text
  | DocumentFrontMatterStart
  | DocumentFrontMatterUnterminated
  | DocumentToml10Error Toml10Violation
  | DocumentTomlParseError Text
  | DocumentUnknownKey Text
  | DocumentMissingKey Text
  | DocumentExpectedType Text Text Text
  | DocumentUnsupportedSchema Text
  | DocumentUnsupportedRelation Text
  | DocumentIdentifierError Text IdViolation
  | DocumentTextViolation Text
  | DocumentDomainError Text DomainError
  | DocumentScopeError Text ScopePatternError
  | DocumentDuplicateList Text
  | DocumentNonCanonicalList Text
  | DocumentStatusRule Text
  | DocumentCapsuleCount Int
  | DocumentCapsuleError ProvenanceError
  | DocumentPathError RepoPathViolation
  deriving (Eq, Show)

parseFrontMatter :: Text -> Either DocumentError (Map Text Toml.Value, Text)
parseFrontMatter input =
  case Text.splitOn "\n" (normalizeLf input) of
    "+++" : remaining -> do
      let (frontLines, suffix) = break (== "+++") remaining
      bodyLines <-
        case suffix of
          [] -> Left DocumentFrontMatterUnterminated
          _closing : body -> Right body
      let frontMatter = Text.unlines frontLines
      first DocumentToml10Error (validateToml10 frontMatter)
      annotated <- first (DocumentTomlParseError . Text.pack) (Toml.parse frontMatter)
      let body = Text.strip (Text.intercalate "\n" bodyLines) <> "\n"
      Right (tableEntries (Toml.forgetTableAnns annotated), body)
    _ -> Left DocumentFrontMatterStart

renderDecisionSemantic :: DecisionRecord -> Either DocumentError Text
renderDecisionSemantic record = do
  validateTrimmed "title" (decisionTitle record)
  if Text.length (decisionTitle record) > 240
    then Left (DocumentTextViolation "title exceeds 240 characters")
    else Right ()
  validateTrimmed "summary" (decisionSummary record)
  validateBody "body" (decisionBody record)
  validateDomains "domains" (decisionDomains record)
  Right
    ( Text.unlines
        [ "+++",
          "schema = \"adrai/decision/v1\"",
          "adr = " <> renderTomlString (adrIdText (decisionAdr record)),
          "record = " <> renderTomlString (recordIdText (decisionRecord record)),
          "title = " <> renderTomlString (decisionTitle record),
          "summary = " <> renderTomlString (decisionSummary record),
          "domains = " <> renderTomlStringArray (map domainText (decisionDomains record)),
          "+++",
          ""
        ]
        <> decisionBody record
    )

renderConnectionSemantic :: ConnectionRecord -> Either DocumentError Text
renderConnectionSemantic record = do
  validateBody "rationale" (connectionRationale record)
  fields <- renderConnectionFields (connectionPayload record)
  Right
    ( Text.unlines
        ( [ "+++",
            "schema = \"adrai/connection/v1\"",
            "connection = " <> renderTomlString (connectionIdText (connectionRecordId record)),
            "relation = " <> renderTomlString (relationName (connectionPayload record))
          ]
            <> fields
            <> ["+++", ""]
        )
        <> connectionRationale record
    )

renderManagedSemantic :: ManagedRecord -> Either DocumentError Text
renderManagedSemantic managed =
  case managed of
    ManagedDecision record -> renderDecisionSemantic record
    ManagedConnection record -> renderConnectionSemantic record

parseManagedDocument :: RepoPath -> ByteString -> Either DocumentError ParsedManagedDocument
parseManagedDocument path bytes = do
  decoded <- first (DocumentInvalidUtf8 . Text.pack . show) (TextEncoding.decodeUtf8' bytes)
  let normalized = normalizeLf decoded
      trailers = capsuleTrailers normalized
  encoded <-
    case trailers of
      [capsule] -> Right capsule
      values -> Left (DocumentCapsuleCount (length values))
  let semantic = normalizeSemantic normalized
  (entries, body) <- parseFrontMatter semantic
  schema <- requireText "schema" entries
  managed <-
    case schema of
      "adrai/decision/v1" -> ManagedDecision <$> parseDecision entries body
      "adrai/connection/v1" -> ManagedConnection <$> parseConnection entries body
      unsupported -> Left (DocumentUnsupportedSchema unsupported)
  capsule <- first DocumentCapsuleError (decodeCapsule encoded)
  first DocumentCapsuleError (validateCapsule (managedObjectRef managed) (semanticDigest semantic) capsule)
  Right
    ParsedManagedDocument
      { parsedManagedPath = path,
        parsedManagedRecord = managed,
        parsedManagedCapsule = capsule,
        parsedManagedSemantic = semantic,
        parsedManagedBytes = TextEncoding.encodeUtf8 normalized
      }

sealManagedDocument :: ManagedRecord -> ProvenanceCapsule -> Either DocumentError ByteString
sealManagedDocument managed capsule = do
  semantic <- renderManagedSemantic managed
  first DocumentCapsuleError (validateCapsule (managedObjectRef managed) (semanticDigest semantic) capsule)
  Right (TextEncoding.encodeUtf8 (sealSemantic semantic capsule))

canonicalManagedPath :: ManagedPaths -> ManagedRecord -> Either DocumentError RepoPath
canonicalManagedPath paths managed =
  first DocumentPathError . mkRepoPath $
    case managed of
      ManagedDecision record ->
        let identifier = recordIdText (decisionRecord record)
         in repoPathText (managedDecisionPath paths)
              <> "/"
              <> Text.take 4 identifier
              <> "/"
              <> identifier
              <> "--"
              <> slugify (decisionTitle record)
              <> ".decision.md"
      ManagedConnection record ->
        let identifier = connectionIdText (connectionRecordId record)
         in repoPathText (managedConnectionPath paths)
              <> "/"
              <> Text.take 4 identifier
              <> "/"
              <> identifier
              <> "--"
              <> relationName (connectionPayload record)
              <> ".connection.md"

parseDecision :: Map Text Toml.Value -> Text -> Either DocumentError DecisionRecord
parseDecision entries body = do
  ensureClosed ["schema", "adr", "record", "title", "summary", "domains"] entries
  ensureRequired ["schema", "adr", "record", "title", "summary", "domains"] entries
  adr <- requireText "adr" entries >>= parseAdr "adr"
  record <- requireText "record" entries >>= parseRecord "record"
  title <- requireText "title" entries
  summary <- requireText "summary" entries
  domainTexts <- requireTextArray "domains" entries
  domains <- parseDomains "domains" domainTexts
  let decision = DecisionRecord adr record title summary domains body
  decision <$ renderDecisionSemantic decision

parseConnection :: Map Text Toml.Value -> Text -> Either DocumentError ConnectionRecord
parseConnection entries rationale = do
  relation <- requireText "relation" entries
  let allowed = commonConnectionKeys <> relationKeys relation
  if null (relationKeys relation)
    then Left (DocumentUnsupportedRelation relation)
    else Right ()
  ensureClosed allowed entries
  ensureRequired (commonConnectionKeys <> requiredRelationKeys relation) entries
  identifier <- requireText "connection" entries >>= parseConnectionId "connection"
  payload <- parseConnectionPayload relation entries
  let record = ConnectionRecord identifier payload rationale
  record <$ renderConnectionSemantic record

parseConnectionPayload :: Text -> Map Text Toml.Value -> Either DocumentError ConnectionPayload
parseConnectionPayload relation entries =
  case relation of
    "amends" -> do
      subject <- subjectAdr entries
      fromRecord <- requireText "from_record" entries >>= parseRecord "from_record"
      toRecords <- requireIdArray "to_records" parseRecord entries
      Right (AmendsConnection (AmendsPayload subject fromRecord toRecords))
    "applies_to" -> do
      subject <- subjectAdr entries
      parents <- requireIdArray "parent_connections" parseConnectionId entries
      change <- requireText "change" entries
      added <- requireScopeArray "added" entries
      removed <- requireScopeArray "removed" entries
      effective <- requireScopeArray "applies_to" entries
      Right (AppliesToConnection (AppliesToPayload subject parents change added removed effective))
    "domains" -> do
      subject <- subjectAdr entries
      parents <- requireIdArray "parent_connections" parseConnectionId entries
      change <- requireText "change" entries
      added <- requireDomainArray "added" entries
      removed <- requireDomainArray "removed" entries
      effective <- requireDomainArray "domains" entries
      refinements <- requireRefinementArray "refinements" entries
      Right (DomainsConnection (DomainsPayload subject parents change added removed effective refinements))
    "status" -> do
      subject <- subjectAdr entries
      parents <- requireIdArray "parent_connections" parseConnectionId entries
      stateText <- requireText "state" entries
      state <-
        case stateText of
          "active" -> Right StatusActive
          "obsolete" -> Right StatusObsolete
          invalid -> Left (DocumentStatusRule ("unsupported status state: " <> invalid))
      heads <- requireIdArray "record_heads" parseRecord entries
      replacement <- optionalText "replacement_adr" entries >>= traverse (parseAdr "replacement_adr")
      if state == StatusActive && replacement /= Nothing
        then Left (DocumentStatusRule "active status may not name a replacement ADR")
        else Right (StatusConnection (StatusPayload subject parents state heads replacement))
    _ -> Left (DocumentUnsupportedRelation relation)

renderConnectionFields :: ConnectionPayload -> Either DocumentError [Text]
renderConnectionFields payload =
  case payload of
    AmendsConnection value -> do
      ensureUniqueIds "to_records" (map recordIdText (amendsToRecords value))
      Right
        [ textField "from_record" (recordIdText (amendsFromRecord value)),
          textField "subject_adr" (adrIdText (amendsSubjectAdr value)),
          arrayField "to_records" (map recordIdText (amendsToRecords value))
        ]
    AppliesToConnection value -> do
      ensureUniqueIds "parent_connections" (map connectionIdText (appliesToParentConnections value))
      validateScopes "added" (appliesToAdded value)
      validateScopes "removed" (appliesToRemoved value)
      validateScopes "applies_to" (appliesToEffective value)
      Right
        [ arrayField "added" (map scopePatternText (appliesToAdded value)),
          arrayField "applies_to" (map scopePatternText (appliesToEffective value)),
          textField "change" (appliesToChange value),
          arrayField "parent_connections" (map connectionIdText (appliesToParentConnections value)),
          arrayField "removed" (map scopePatternText (appliesToRemoved value)),
          textField "subject_adr" (adrIdText (appliesToSubjectAdr value))
        ]
    DomainsConnection value -> do
      ensureUniqueIds "parent_connections" (map connectionIdText (domainsParentConnections value))
      validateDomains "added" (domainsAdded value)
      validateDomains "removed" (domainsRemoved value)
      validateDomains "domains" (domainsEffective value)
      ensureUniqueIds "refinements" (map domainRefinementText (domainsRefinements value))
      Right
        [ arrayField "added" (map domainText (domainsAdded value)),
          textField "change" (domainsChange value),
          arrayField "domains" (map domainText (domainsEffective value)),
          arrayField "parent_connections" (map connectionIdText (domainsParentConnections value)),
          arrayField "refinements" (map domainRefinementText (domainsRefinements value)),
          arrayField "removed" (map domainText (domainsRemoved value)),
          textField "subject_adr" (adrIdText (domainsSubjectAdr value))
        ]
    StatusConnection value -> do
      ensureUniqueIds "parent_connections" (map connectionIdText (statusParentConnections value))
      ensureUniqueIds "record_heads" (map recordIdText (statusRecordHeads value))
      if statusState value == StatusActive && statusReplacementAdr value /= Nothing
        then Left (DocumentStatusRule "active status may not name a replacement ADR")
        else
          Right
            ( [ arrayField "parent_connections" (map connectionIdText (statusParentConnections value)),
                arrayField "record_heads" (map recordIdText (statusRecordHeads value))
              ]
                <> maybe [] (\replacement -> [textField "replacement_adr" (adrIdText replacement)]) (statusReplacementAdr value)
                <> [ textField "state" (statusStateText (statusState value)),
                     textField "subject_adr" (adrIdText (statusSubjectAdr value))
                   ]
            )

relationName :: ConnectionPayload -> Text
relationName payload =
  case payload of
    AmendsConnection _ -> "amends"
    AppliesToConnection _ -> "applies_to"
    DomainsConnection _ -> "domains"
    StatusConnection _ -> "status"

commonConnectionKeys :: [Text]
commonConnectionKeys = ["schema", "connection", "relation"]

relationKeys :: Text -> [Text]
relationKeys relation =
  case relation of
    "amends" -> ["subject_adr", "from_record", "to_records"]
    "applies_to" -> ["subject_adr", "parent_connections", "change", "added", "removed", "applies_to"]
    "domains" -> ["subject_adr", "parent_connections", "change", "added", "removed", "domains", "refinements"]
    "status" -> ["subject_adr", "parent_connections", "state", "record_heads", "replacement_adr"]
    _ -> []

requiredRelationKeys :: Text -> [Text]
requiredRelationKeys relation =
  case relation of
    "status" -> ["subject_adr", "parent_connections", "state", "record_heads"]
    _ -> relationKeys relation

subjectAdr :: Map Text Toml.Value -> Either DocumentError AdrId
subjectAdr entries = requireText "subject_adr" entries >>= parseAdr "subject_adr"

parseAdr :: Text -> Text -> Either DocumentError AdrId
parseAdr context = first (DocumentIdentifierError context) . mkAdrId

parseRecord :: Text -> Text -> Either DocumentError RecordId
parseRecord context = first (DocumentIdentifierError context) . mkRecordId

parseConnectionId :: Text -> Text -> Either DocumentError ConnectionId
parseConnectionId context = first (DocumentIdentifierError context) . mkConnectionId

requireIdArray :: Text -> (Text -> Text -> Either DocumentError identifier) -> Map Text Toml.Value -> Either DocumentError [identifier]
requireIdArray key parser entries = do
  values <- requireTextArray key entries
  ensureUniqueIds key values
  traverse (parser key) values

requireScopeArray :: Text -> Map Text Toml.Value -> Either DocumentError [ScopePattern]
requireScopeArray key entries = do
  values <- requireTextArray key entries
  ensureUniqueIds key values
  scopes <- traverse (first (DocumentScopeError key) . mkScopePattern) values
  if values == map scopePatternText scopes
    then Right scopes
    else Left (DocumentNonCanonicalList key)

requireDomainArray :: Text -> Map Text Toml.Value -> Either DocumentError [Domain]
requireDomainArray key entries = requireTextArray key entries >>= parseDomains key

parseDomains :: Text -> [Text] -> Either DocumentError [Domain]
parseDomains key values = do
  ensureUniqueIds key values
  domains <- first (DocumentDomainError key) (canonicalDomains values)
  if values == map domainText domains
    then Right domains
    else Left (DocumentNonCanonicalList key)

requireRefinementArray :: Text -> Map Text Toml.Value -> Either DocumentError [DomainRefinement]
requireRefinementArray key entries = do
  values <- requireTextArray key entries
  ensureUniqueIds key values
  refinements <- traverse (first (DocumentDomainError key) . parseDomainRefinement) values
  if values == map domainRefinementText refinements
    then Right refinements
    else Left (DocumentNonCanonicalList key)

validateDomains :: Text -> [Domain] -> Either DocumentError ()
validateDomains key domains = do
  ensureUniqueIds key (map domainText domains)
  canonical <- first (DocumentDomainError key) (canonicalDomains (map domainText domains))
  if domains == canonical
    then Right ()
    else Left (DocumentNonCanonicalList key)

validateScopes :: Text -> [ScopePattern] -> Either DocumentError ()
validateScopes key = ensureUniqueIds key . map scopePatternText

validateTrimmed :: Text -> Text -> Either DocumentError ()
validateTrimmed label value
  | Text.null value = Left (DocumentTextViolation (label <> " may not be empty"))
  | Text.strip value /= value = Left (DocumentTextViolation (label <> " must be trimmed"))
  | otherwise = Right ()

validateBody :: Text -> Text -> Either DocumentError ()
validateBody label value
  | Text.null (Text.strip value) = Left (DocumentTextViolation (label <> " may not be empty"))
  | value /= Text.strip (normalizeLf value) <> "\n" =
      Left (DocumentTextViolation (label <> " must be globally trimmed and end with exactly one LF"))
  | otherwise = Right ()

ensureUniqueIds :: Text -> [Text] -> Either DocumentError ()
ensureUniqueIds label values
  | Set.size (Set.fromList values) == length values = Right ()
  | otherwise = Left (DocumentDuplicateList label)

ensureClosed :: [Text] -> Map Text Toml.Value -> Either DocumentError ()
ensureClosed allowed entries =
  case filter (`notElem` allowed) (Map.keys entries) of
    unknown : _ -> Left (DocumentUnknownKey unknown)
    [] -> Right ()

ensureRequired :: [Text] -> Map Text Toml.Value -> Either DocumentError ()
ensureRequired required entries =
  case filter (`Map.notMember` entries) required of
    missing : _ -> Left (DocumentMissingKey missing)
    [] -> Right ()

requireText :: Text -> Map Text Toml.Value -> Either DocumentError Text
requireText key entries = requireValue key entries >>= expectText key

optionalText :: Text -> Map Text Toml.Value -> Either DocumentError (Maybe Text)
optionalText key entries = traverse (expectText key) (Map.lookup key entries)

requireTextArray :: Text -> Map Text Toml.Value -> Either DocumentError [Text]
requireTextArray key entries = requireValue key entries >>= expectTextArray key

requireValue :: Text -> Map Text Toml.Value -> Either DocumentError Toml.Value
requireValue key entries = maybe (Left (DocumentMissingKey key)) Right (Map.lookup key entries)

expectText :: Text -> Toml.Value -> Either DocumentError Text
expectText _ (Toml.Text' _ value) = Right value
expectText context actual = Left (DocumentExpectedType context "string" (valueTypeName actual))

expectTextArray :: Text -> Toml.Value -> Either DocumentError [Text]
expectTextArray context (Toml.List' _ values) = traverse (expectText context) values
expectTextArray context actual = Left (DocumentExpectedType context "array of strings" (valueTypeName actual))

valueTypeName :: Toml.Value -> Text
valueTypeName value =
  case value of
    Toml.Integer' _ _ -> "integer"
    Toml.Double' _ _ -> "float"
    Toml.List' _ _ -> "array"
    Toml.Table' _ _ -> "table"
    Toml.Bool' _ _ -> "boolean"
    Toml.Text' _ _ -> "string"
    Toml.TimeOfDay' _ _ -> "local time"
    Toml.ZonedTime' _ _ -> "offset date-time"
    Toml.LocalTime' _ _ -> "local date-time"
    Toml.Day' _ _ -> "local date"

tableEntries :: Toml.Table -> Map Text Toml.Value
tableEntries (Toml.MkTable entries) = Map.map snd entries

capsuleTrailers :: Text -> [Text]
capsuleTrailers = foldr collect [] . Text.lines
  where
    collect line found =
      case Text.stripPrefix "<!-- @adrai:" (Text.strip line) >>= Text.stripSuffix " -->" of
        Just encoded -> encoded : found
        Nothing -> found

managedObjectRef :: ManagedRecord -> ObjectRef
managedObjectRef managed =
  case managed of
    ManagedDecision record -> recordObjectRef (decisionRecord record)
    ManagedConnection record -> connectionObjectRef (connectionRecordId record)

statusStateText :: StatusState -> Text
statusStateText StatusActive = "active"
statusStateText StatusObsolete = "obsolete"

textField :: Text -> Text -> Text
textField key value = key <> " = " <> renderTomlString value

arrayField :: Text -> [Text] -> Text
arrayField key values = key <> " = " <> renderTomlStringArray values

normalizeLf :: Text -> Text
normalizeLf = Text.replace "\r" "\n" . Text.replace "\r\n" "\n"

slugify :: Text -> Text
slugify title = Text.take 64 fallback
  where
    mapped = Text.map replace (Text.toLower title)
    collapsed = Text.intercalate "-" (filter (not . Text.null) (Text.splitOn "-" mapped))
    fallback = if Text.null collapsed then "decision" else collapsed
    replace character
      | isAsciiLower character || isDigit character = character
      | otherwise = '-'
