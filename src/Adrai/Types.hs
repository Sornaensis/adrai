{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Adrai.Types
  ( -- * Typed identifiers
    AdrId,
    mkAdrId,
    adrIdText,
    RecordId,
    mkRecordId,
    recordIdText,
    ConnectionId,
    mkConnectionId,
    connectionIdText,
    OperationId(..),
    mkOperationId,
    operationIdText,
    IdViolation (..),

    -- * Object references and public prefixes
    ObjectKind (..),
    ObjectRef,
    adrObjectRef,
    recordObjectRef,
    connectionObjectRef,
    objectRefKind,
    objectRefText,
    IdPrefix,
    mkIdPrefix,
    idPrefixText,
    idPrefixKind,
    IdPrefixViolation (..),
    ReferenceError (..),
    -- | Compatibility name for the public-reference resolution error.
    PrefixResolutionError,
    resolveObjectRef,
    resolveIdPrefix,

    -- * Repository paths and content identities
    RepoPath (..),
    mkRepoPath,
    repoPathText,
    RepoPathViolation (..),
    Digest(..),
    mkDigest,
    digestBytes,
    DigestViolation (..),
    StateToken (..),
    mkStateToken,
    stateTokenText,
    StateTokenViolation (..),

    -- * Errors and process outcomes
    ExitClass (..),
    exitClassCode,
    exitClassFromCode,
    AdraiError (..),

    -- * Versioned schemas
    ConfigSchema (..),
    SourceSchema (..),
    PublicSchema (..),

    -- * Configuration foundations
    GitRef (..),
    mkGitRef,
    gitRefText,
    GitRefViolation (..),
    ManagedPaths,
    mkManagedPaths,
    managedDecisionPath,
    managedConnectionPath,
    ManagedPathsViolation (..),
    LogicalLine (..),
    mkLogicalLine,
    logicalLineId,
    logicalLineRefs,
    LogicalLineViolation (..),
    Config,
    mkConfig,
    configSchema,
    configManagedPaths,
    configLogicalLines,
    ConfigViolation (..),
    defaultConfig,

    -- * Shared service records
    RevisionSelector (..),
    ViewMode (..),
    ActorKind (..),
    Actor,
    mkActor,
    actorKind,
    actorId,
    actorModel,
    ActorViolation (..),
    ProvenanceInputs (..),
    MutationContext (..),
    CommitResult (..),
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isAsciiLower, isAsciiUpper, isControl, isDigit, isSpace)
import Data.List (nub, sort)
import Data.Text (Text)
import qualified Data.Text as T

-- Identifiers -----------------------------------------------------------------

newtype AdrId = AdrId Text
  deriving (Eq, Ord, Show)

newtype RecordId = RecordId Text
  deriving (Eq, Ord, Show)

newtype ConnectionId = ConnectionId Text
  deriving (Eq, Ord, Show)

newtype OperationId = OperationId Text
  deriving (Eq, Ord, Show)

data IdViolation
  = IdWrongLength Int
  | IdWrongKind Char
  | IdInvalidPayloadCharacter Char
  deriving (Eq, Show)

mkAdrId :: Text -> Either IdViolation AdrId
mkAdrId = fmap AdrId . validateFullId 'A'

adrIdText :: AdrId -> Text
adrIdText (AdrId value) = value

mkRecordId :: Text -> Either IdViolation RecordId
mkRecordId = fmap RecordId . validateFullId 'R'

recordIdText :: RecordId -> Text
recordIdText (RecordId value) = value

mkConnectionId :: Text -> Either IdViolation ConnectionId
mkConnectionId = fmap ConnectionId . validateFullId 'C'

connectionIdText :: ConnectionId -> Text
connectionIdText (ConnectionId value) = value

mkOperationId :: Text -> Either IdViolation OperationId
mkOperationId = fmap OperationId . validateFullId 'O'

operationIdText :: OperationId -> Text
operationIdText (OperationId value) = value

validateFullId :: Char -> Text -> Either IdViolation Text
validateFullId expected value
  | T.length value /= 27 = Left (IdWrongLength (T.length value))
  | otherwise =
      case T.uncons value of
        Nothing -> Left (IdWrongLength 0)
        Just (actual, payload)
          | actual /= expected -> Left (IdWrongKind actual)
          | otherwise ->
              case T.find (not . isCrockford) payload of
                Just invalid -> Left (IdInvalidPayloadCharacter invalid)
                Nothing -> Right value

isCrockford :: Char -> Bool
isCrockford character = character `elem` ("0123456789ABCDEFGHJKMNPQRSTVWXYZ" :: String)

-- Object references ------------------------------------------------------------

data ObjectKind
  = AdrObject
  | RecordObject
  | ConnectionObject
  deriving (Eq, Ord, Show, Enum, Bounded)

data ObjectRef
  = AdrObjectRef AdrId
  | RecordObjectRef RecordId
  | ConnectionObjectRef ConnectionId
  deriving (Eq, Ord, Show)

adrObjectRef :: AdrId -> ObjectRef
adrObjectRef = AdrObjectRef

recordObjectRef :: RecordId -> ObjectRef
recordObjectRef = RecordObjectRef

connectionObjectRef :: ConnectionId -> ObjectRef
connectionObjectRef = ConnectionObjectRef

objectRefKind :: ObjectRef -> ObjectKind
objectRefKind reference =
  case reference of
    AdrObjectRef _ -> AdrObject
    RecordObjectRef _ -> RecordObject
    ConnectionObjectRef _ -> ConnectionObject

objectRefText :: ObjectRef -> Text
objectRefText reference =
  case reference of
    AdrObjectRef identifier -> adrIdText identifier
    RecordObjectRef identifier -> recordIdText identifier
    ConnectionObjectRef identifier -> connectionIdText identifier

data IdPrefix = IdPrefix ObjectKind Text
  deriving (Eq, Ord, Show)

data IdPrefixViolation
  = IdPrefixWrongLength Int
  | IdPrefixUnsupportedKind Char
  | IdPrefixInvalidPayloadCharacter Char
  deriving (Eq, Show)

mkIdPrefix :: Text -> Either IdPrefixViolation IdPrefix
mkIdPrefix input
  | lengthOfPrefix < 8 || lengthOfPrefix > 27 = Left (IdPrefixWrongLength lengthOfPrefix)
  | otherwise =
      case T.uncons normalized of
        Nothing -> Left (IdPrefixWrongLength 0)
        Just (prefix, payload) -> do
          kind <- maybe (Left (IdPrefixUnsupportedKind prefix)) Right (kindFromPrefix prefix)
          case T.find (not . isCrockford) payload of
            Just invalid -> Left (IdPrefixInvalidPayloadCharacter invalid)
            Nothing -> Right (IdPrefix kind normalized)
  where
    normalized = asciiUpper (T.strip input)
    lengthOfPrefix = T.length normalized

idPrefixText :: IdPrefix -> Text
idPrefixText (IdPrefix _ value) = value

idPrefixKind :: IdPrefix -> ObjectKind
idPrefixKind (IdPrefix kind _) = kind

kindFromPrefix :: Char -> Maybe ObjectKind
kindFromPrefix prefix =
  case prefix of
    'A' -> Just AdrObject
    'R' -> Just RecordObject
    'C' -> Just ConnectionObject
    _ -> Nothing

data ReferenceError
  = InvalidIdPrefix IdPrefixViolation
  | PrefixNotFound IdPrefix
  | PrefixAmbiguous IdPrefix [ObjectRef]
  deriving (Eq, Show)

-- | Backwards-compatible, descriptive alias. New APIs should prefer
-- 'ReferenceError'.
type PrefixResolutionError = ReferenceError

resolveObjectRef :: Text -> [ObjectRef] -> Either ReferenceError ObjectRef
resolveObjectRef input candidates =
  case mkIdPrefix input of
    Left violation -> Left (InvalidIdPrefix violation)
    Right prefix -> resolveIdPrefix prefix candidates

resolveIdPrefix :: IdPrefix -> [ObjectRef] -> Either ReferenceError ObjectRef
resolveIdPrefix prefix candidates =
  case matching of
    [] -> Left (PrefixNotFound prefix)
    [match] -> Right match
    matches -> Left (PrefixAmbiguous prefix matches)
  where
    matching =
      sort
        . nub
        . filter
          ( \candidate ->
              objectRefKind candidate == idPrefixKind prefix
                && idPrefixText prefix `T.isPrefixOf` objectRefText candidate
          )
        $ candidates

asciiUpper :: Text -> Text
asciiUpper = T.map upper
  where
    upper character
      | character >= 'a' && character <= 'z' = toEnum (fromEnum character - 32)
      | otherwise = character

-- Repository paths -------------------------------------------------------------

newtype RepoPath = RepoPath Text
  deriving (Eq, Ord, Show)

data RepoPathViolation
  = RepoPathEmpty
  | RepoPathAbsolute
  | RepoPathDriveQualified
  | RepoPathUnc
  | RepoPathBackslash
  | RepoPathEmptySegment
  | RepoPathDotSegment
  | RepoPathParentSegment
  | RepoPathGitSegment
  | RepoPathControlCharacter
  | RepoPathInvalidCharacter Char
  | RepoPathTrailingDotOrSpace Text
  | RepoPathReservedName Text
  deriving (Eq, Show)

mkRepoPath :: Text -> Either RepoPathViolation RepoPath
mkRepoPath value
  | T.null value = Left RepoPathEmpty
  | T.isPrefixOf "//" value = Left RepoPathUnc
  | T.isPrefixOf "/" value = Left RepoPathAbsolute
  | isDriveQualified value = Left RepoPathDriveQualified
  | T.any (== '\\') value = Left RepoPathBackslash
  | T.any isControl value = Left RepoPathControlCharacter
  | Just invalid <- T.find (`elem` ("<>:\"|?*" :: String)) value = Left (RepoPathInvalidCharacter invalid)
  | any T.null segments = Left RepoPathEmptySegment
  | any (== ".") segments = Left RepoPathDotSegment
  | any (== "..") segments = Left RepoPathParentSegment
  | any ((== ".git") . asciiLower) segments = Left RepoPathGitSegment
  | Just invalid <- firstMatching hasTrailingDotOrSpace segments = Left (RepoPathTrailingDotOrSpace invalid)
  | Just invalid <- firstMatching isReservedWindowsName segments = Left (RepoPathReservedName invalid)
  | otherwise = Right (RepoPath value)
  where
    segments = T.splitOn "/" value

firstMatching :: (value -> Bool) -> [value] -> Maybe value
firstMatching predicate = go
  where
    go [] = Nothing
    go (value : remaining)
      | predicate value = Just value
      | otherwise = go remaining

hasTrailingDotOrSpace :: Text -> Bool
hasTrailingDotOrSpace segment =
  case T.unsnoc segment of
    Just (_, finalCharacter) -> finalCharacter == '.' || finalCharacter == ' '
    Nothing -> False

isReservedWindowsName :: Text -> Bool
isReservedWindowsName segment =
  base `elem` ["con", "prn", "aux", "nul", "conin$", "conout$"]
    || any (`T.isPrefixOf` base) ["com", "lpt"] && numericDeviceSuffix base
  where
    base = T.toCaseFold (T.takeWhile (/= '.') segment)
    numericDeviceSuffix value =
      case T.unsnoc value of
        Just (prefix, digit) ->
          prefix `elem` ["com", "lpt"]
            && (digit >= '1' && digit <= '9' || digit `elem` ['\x00b9', '\x00b2', '\x00b3'])
        Nothing -> False

repoPathText :: RepoPath -> Text
repoPathText (RepoPath value) = value

isDriveQualified :: Text -> Bool
isDriveQualified value =
  case T.uncons value of
    Just (drive, remainder) ->
      isAsciiLetter drive
        && case T.uncons remainder of
          Just (':', _) -> True
          _ -> False
    Nothing -> False

isAsciiLetter :: Char -> Bool
isAsciiLetter character = isAsciiLower character || isAsciiUpper character

asciiLower :: Text -> Text
asciiLower = T.map lower
  where
    lower character
      | character >= 'A' && character <= 'Z' = toEnum (fromEnum character + 32)
      | otherwise = character

-- Digests and state tokens ------------------------------------------------------

newtype Digest = Digest ByteString
  deriving (Eq, Ord, Show)

data DigestViolation = DigestWrongLength Int
  deriving (Eq, Show)

mkDigest :: ByteString -> Either DigestViolation Digest
mkDigest bytes
  | BS.length bytes == 32 = Right (Digest bytes)
  | otherwise = Left (DigestWrongLength (BS.length bytes))

digestBytes :: Digest -> ByteString
digestBytes (Digest bytes) = bytes

newtype StateToken = StateToken Text
  deriving (Eq, Ord, Show)

data StateTokenViolation
  = StateTokenWrongLength Int
  | StateTokenWrongPrefix Char
  | StateTokenInvalidCharacter Char
  deriving (Eq, Show)

mkStateToken :: Text -> Either StateTokenViolation StateToken
mkStateToken value
  | T.length value /= 23 = Left (StateTokenWrongLength (T.length value))
  | otherwise =
      case T.uncons value of
        Nothing -> Left (StateTokenWrongLength 0)
        Just (prefix, payload)
          | prefix /= 'S' -> Left (StateTokenWrongPrefix prefix)
          | otherwise ->
              case T.find (not . isBase64Url) payload of
                Just invalid -> Left (StateTokenInvalidCharacter invalid)
                Nothing -> Right (StateToken value)

stateTokenText :: StateToken -> Text
stateTokenText (StateToken value) = value

isBase64Url :: Char -> Bool
isBase64Url character =
  isAsciiLower character
    || isAsciiUpper character
    || isDigit character
    || character == '-'
    || character == '_'

-- Errors and exit classes ------------------------------------------------------

data ExitClass
  = ExitSuccess
  | ExitUserError
  | ExitConflict
  | ExitCheckFailed
  deriving (Eq, Ord, Show, Enum, Bounded)

exitClassCode :: ExitClass -> Int
exitClassCode exitClass =
  case exitClass of
    ExitSuccess -> 0
    ExitUserError -> 2
    ExitConflict -> 3
    ExitCheckFailed -> 4

exitClassFromCode :: Int -> Maybe ExitClass
exitClassFromCode code =
  case code of
    0 -> Just ExitSuccess
    2 -> Just ExitUserError
    3 -> Just ExitConflict
    4 -> Just ExitCheckFailed
    _ -> Nothing

data AdraiError = AdraiError
  { adraiErrorClass :: ExitClass,
    adraiErrorMessage :: Text
  }
  deriving (Eq, Show)

-- Exact schema identities ------------------------------------------------------

data ConfigSchema = ConfigSchemaV1
  deriving (Eq, Ord, Show, Enum, Bounded)

data SourceSchema
  = DecisionSourceV1
  | ConnectionSourceV1
  deriving (Eq, Ord, Show, Enum, Bounded)

data PublicSchema
  = SearchPublicV1
  | RelevantPublicV1
  | HistoryPublicV1
  | ShowCollapsedPublicV1
  | ShowExplodedPublicV1
  | EventsPublicV1
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Configuration ---------------------------------------------------------------

newtype GitRef = GitRef Text
  deriving (Eq, Ord, Show)

data GitRefViolation
  = GitRefEmpty
  | GitRefSurroundingWhitespace
  | GitRefNotFullyQualified
  | GitRefControlCharacter
  | GitRefInvalidCharacter Char
  | GitRefInvalidSyntax
  deriving (Eq, Show)

mkGitRef :: Text -> Either GitRefViolation GitRef
mkGitRef value
  | T.null value = Left GitRefEmpty
  | T.strip value /= value = Left GitRefSurroundingWhitespace
  | T.any isControl value = Left GitRefControlCharacter
  | not ("refs/" `T.isPrefixOf` value) = Left GitRefNotFullyQualified
  | Just invalid <- T.find invalidGitRefCharacter value = Left (GitRefInvalidCharacter invalid)
  | not (validGitRefSyntax value) = Left GitRefInvalidSyntax
  | otherwise = Right (GitRef value)

invalidGitRefCharacter :: Char -> Bool
invalidGitRefCharacter character =
  isSpace character
    || character `elem` ("~^:?*[\\" :: String)

validGitRefSyntax :: Text -> Bool
validGitRefSyntax value =
  value /= "@"
    && not ("/" `T.isPrefixOf` value)
    && not ("/" `T.isSuffixOf` value)
    && not ("." `T.isSuffixOf` value)
    && not (".." `T.isInfixOf` value)
    && not ("@{" `T.isInfixOf` value)
    && all validComponent (T.splitOn "/" value)
  where
    validComponent component =
      not (T.null component)
        && not ("." `T.isPrefixOf` component)
        && not (".lock" `T.isSuffixOf` component)

gitRefText :: GitRef -> Text
gitRefText (GitRef value) = value

data ManagedPaths = ManagedPaths
  { managedDecisionPath :: RepoPath,
    managedConnectionPath :: RepoPath
  }
  deriving (Eq, Show)

data ManagedPathsViolation = ManagedPathsOverlap RepoPath RepoPath
  deriving (Eq, Show)

mkManagedPaths :: RepoPath -> RepoPath -> Either ManagedPathsViolation ManagedPaths
mkManagedPaths decisions connections
  | pathsOverlap decisions connections = Left (ManagedPathsOverlap decisions connections)
  | otherwise = Right (ManagedPaths decisions connections)

pathsOverlap :: RepoPath -> RepoPath -> Bool
pathsOverlap first second =
  firstKey == secondKey
    || (firstKey <> "/") `T.isPrefixOf` secondKey
    || (secondKey <> "/") `T.isPrefixOf` firstKey
  where
    firstKey = T.toCaseFold (repoPathText first)
    secondKey = T.toCaseFold (repoPathText second)

data LogicalLine = LogicalLine
  { logicalLineId :: Text,
    logicalLineRefs :: [GitRef]
  }
  deriving (Eq, Show)

data LogicalLineViolation
  = LogicalLineInvalidId Text
  | LogicalLineHasNoRefs
  | LogicalLineDuplicateRef GitRef
  deriving (Eq, Show)

mkLogicalLine :: Text -> [GitRef] -> Either LogicalLineViolation LogicalLine
mkLogicalLine identifier references
  | not (validLogicalLineId identifier) = Left (LogicalLineInvalidId identifier)
  | null references = Left LogicalLineHasNoRefs
  | otherwise =
      case firstDuplicate references of
        Just duplicate -> Left (LogicalLineDuplicateRef duplicate)
        Nothing -> Right (LogicalLine identifier references)

validLogicalLineId :: Text -> Bool
validLogicalLineId value =
  case T.uncons value of
    Nothing -> False
    Just (first, remainder) ->
      isAsciiLower first
        && T.all (\character -> isAsciiLower character || isDigit character || character == '-') remainder

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate values = go [] values
  where
    go _ [] = Nothing
    go seen (value : remaining)
      | value `elem` seen = Just value
      | otherwise = go (value : seen) remaining

data Config = Config
  { configSchema :: ConfigSchema,
    configManagedPaths :: ManagedPaths,
    configLogicalLines :: [LogicalLine]
  }
  deriving (Eq, Show)

data ConfigViolation
  = ConfigHasNoLogicalLines
  | ConfigDuplicateLogicalLine Text
  deriving (Eq, Show)

mkConfig :: ConfigSchema -> ManagedPaths -> [LogicalLine] -> Either ConfigViolation Config
mkConfig schema paths logicalLines
  | null logicalLines = Left ConfigHasNoLogicalLines
  | otherwise =
      case firstDuplicate (map logicalLineId logicalLines) of
        Just duplicate -> Left (ConfigDuplicateLogicalLine duplicate)
        Nothing -> Right (Config schema paths logicalLines)

defaultConfig :: Config
defaultConfig =
  Config
    ConfigSchemaV1
    ( ManagedPaths
        (RepoPath "architecture/adrai/decisions")
        (RepoPath "architecture/adrai/connections")
    )
    [ LogicalLine
        "trunk"
        [ GitRef "refs/heads/main",
          GitRef "refs/remotes/origin/main",
          GitRef "refs/heads/master",
          GitRef "refs/remotes/origin/master"
        ]
    ]

-- Shared service records -------------------------------------------------------

data RevisionSelector
  = WorkingRevision
  | AtRevision Text
  deriving (Eq, Ord, Show)

data ViewMode
  = CollapsedView
  | ExplodedView
  deriving (Eq, Ord, Show, Enum, Bounded)

data ActorKind
  = HumanActor
  | LlmActor
  | ServiceActor
  deriving (Eq, Ord, Show, Enum, Bounded)

data Actor = Actor
  { actorKind :: ActorKind,
    actorId :: Text,
    actorModel :: Maybe Text
  }
  deriving (Eq, Show)

data ActorViolation
  = ActorIdEmpty
  | ActorIdSurroundingWhitespace
  | ActorIdControlCharacter
  | ActorModelEmpty
  | ActorModelSurroundingWhitespace
  | ActorModelControlCharacter
  deriving (Eq, Show)

mkActor :: ActorKind -> Text -> Maybe Text -> Either ActorViolation Actor
mkActor kind identifier model = do
  validateActorId identifier
  traverse_ validateActorModel model
  Right (Actor kind identifier model)

validateActorId :: Text -> Either ActorViolation ()
validateActorId value
  | T.null value = Left ActorIdEmpty
  | T.strip value /= value = Left ActorIdSurroundingWhitespace
  | T.any isControl value = Left ActorIdControlCharacter
  | otherwise = Right ()

validateActorModel :: Text -> Either ActorViolation ()
validateActorModel value
  | T.null value = Left ActorModelEmpty
  | T.strip value /= value = Left ActorModelSurroundingWhitespace
  | T.any isControl value = Left ActorModelControlCharacter
  | otherwise = Right ()

traverse_ :: (value -> Either error ()) -> Maybe value -> Either error ()
traverse_ _ Nothing = Right ()
traverse_ action (Just value) = action value

data ProvenanceInputs = ProvenanceInputs
  { provenanceInputDigest :: Maybe Digest,
    provenancePromptDigest :: Maybe Digest,
    provenanceContextDigest :: Maybe Digest
  }
  deriving (Eq, Show)

data MutationContext = MutationContext
  { mutationOperationId :: OperationId,
    mutationActor :: Actor,
    mutationBasis :: Text,
    mutationExpectedState :: Maybe StateToken,
    mutationProvenanceInputs :: ProvenanceInputs
  }
  deriving (Eq, Show)

data CommitResult = CommitResult
  { commitOperationId :: OperationId,
    commitOid :: Text,
    commitCreatedPaths :: [RepoPath],
    commitIndexUpdated :: Bool
  }
  deriving (Eq, Show)
