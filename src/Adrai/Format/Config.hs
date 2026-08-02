{-# LANGUAGE OverloadedStrings #-}

module Adrai.Format.Config
  ( ConfigParseError (..),
    ConfigFormatError,
    defaultConfigText,
    parseConfigText,
    renderConfig,
  )
where

import Adrai.Format.Toml
  ( Toml10Violation (..),
    renderTomlString,
    renderTomlStringArray,
    validateToml10,
  )
import Adrai.Types
import Data.List (intersperse)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Toml

-- | A closed, location-aware-enough error vocabulary for the committed
-- configuration contract. TOML syntax/duplicate diagnostics retain the
-- parser's original line and column text in 'ConfigTomlParseError'.
data ConfigParseError
  = ConfigTomlParseError Text
  | ConfigToml11Escape Text
  | ConfigToml11Syntax Text
  | ConfigUnknownKey Text Text
  | ConfigMissingKey Text Text
  | ConfigExpectedType Text Text Text
  | ConfigUnsupportedSchema Integer
  | ConfigRepoPathViolation Text RepoPathViolation
  | ConfigManagedPathsViolation ManagedPathsViolation
  | ConfigGitRefViolation Text GitRefViolation
  | ConfigLogicalLineViolation Int LogicalLineViolation
  | ConfigViolation ConfigViolation
  deriving (Eq, Show)

-- | Descriptive compatibility alias for callers that treat all failures as
-- format errors rather than parse errors.
type ConfigFormatError = ConfigParseError

-- | The canonical default document. Its UTF-8 representation is exactly the
-- committed 239-byte @config/default.toml@ fixture and ends with one LF.
defaultConfigText :: Text
defaultConfigText = renderConfig defaultConfig

-- | Parse the strict version-1 configuration format through toml-parser's raw
-- semantic tables and then validate every value with the existing domain
-- constructors.
parseConfigText :: Text -> Either ConfigParseError Config
parseConfigText input = do
  mapLeft configToml10Violation (validateToml10 input)
  annotated <- mapLeft (ConfigTomlParseError . T.pack) (Toml.parse input)
  parseRoot (Toml.forgetTableAnns annotated)

configToml10Violation :: Toml10Violation -> ConfigParseError
configToml10Violation violation =
  case violation of
    Toml10UnsupportedEscape _ ->
      ConfigToml11Escape "TOML 1.1 \\e and \\x escapes are not supported"
    Toml10MultilineInlineTable ->
      ConfigToml11Syntax "TOML 1.1 multiline inline tables are not supported"
    Toml10TrailingInlineTableComma ->
      ConfigToml11Syntax "TOML 1.1 trailing commas in inline tables are not supported"

-- | Render a configuration in the one canonical key and section order.
renderConfig :: Config -> Text
renderConfig config =
  T.unlines
    ( [ "schema = " <> renderSchema (configSchema config),
        "",
        "[paths]",
        "decisions = " <> renderTomlString (repoPathText (managedDecisionPath paths)),
        "connections = " <> renderTomlString (repoPathText (managedConnectionPath paths)),
        ""
      ]
        <> renderLines (configLogicalLines config)
    )
  where
    paths = configManagedPaths config

renderSchema :: ConfigSchema -> Text
renderSchema ConfigSchemaV1 = "1"

renderLines :: [LogicalLine] -> [Text]
renderLines logicalLines = concat (intersperse [""] (map renderLine logicalLines))

renderLine :: LogicalLine -> [Text]
renderLine logicalLine =
  [ "[[line]]",
    "id = " <> renderTomlString (logicalLineId logicalLine),
    "refs = " <> renderTomlStringArray (map gitRefText (logicalLineRefs logicalLine))
  ]

parseRoot :: Toml.Table -> Either ConfigParseError Config
parseRoot tableValue = do
  let entries = tableEntries tableValue
  ensureClosed "root" ["schema", "paths", "line"] entries
  schemaValue <- requireKey "root" "schema" entries
  schemaNumber <- expectInteger "schema" schemaValue
  schema <-
    if schemaNumber == 1
      then Right ConfigSchemaV1
      else Left (ConfigUnsupportedSchema schemaNumber)
  paths <-
    case Map.lookup "paths" entries of
      Nothing -> Right (configManagedPaths defaultConfig)
      Just value -> expectTable "paths" value >>= parsePaths
  logicalLines <-
    case Map.lookup "line" entries of
      Nothing -> Right (configLogicalLines defaultConfig)
      Just value -> parseLines value
  mapLeft ConfigViolation (mkConfig schema paths logicalLines)

parsePaths :: Toml.Table -> Either ConfigParseError ManagedPaths
parsePaths tableValue = do
  let entries = tableEntries tableValue
      defaults = configManagedPaths defaultConfig
  ensureClosed "paths" ["decisions", "connections"] entries
  decisions <-
    parseOptionalPath
      "paths.decisions"
      (managedDecisionPath defaults)
      (Map.lookup "decisions" entries)
  connections <-
    parseOptionalPath
      "paths.connections"
      (managedConnectionPath defaults)
      (Map.lookup "connections" entries)
  mapLeft ConfigManagedPathsViolation (mkManagedPaths decisions connections)

parseOptionalPath :: Text -> RepoPath -> Maybe Toml.Value -> Either ConfigParseError RepoPath
parseOptionalPath _ defaultValue Nothing = Right defaultValue
parseOptionalPath context _ (Just value) = do
  pathText <- expectText context value
  mapLeft (ConfigRepoPathViolation context) (mkRepoPath pathText)

parseLines :: Toml.Value -> Either ConfigParseError [LogicalLine]
parseLines (Toml.List' _ []) = Right (configLogicalLines defaultConfig)
parseLines (Toml.List' _ values) = traverse (uncurry parseLine) (zip [0 ..] values)
parseLines value = Left (expectedType "line" "array of tables" value)

parseLine :: Int -> Toml.Value -> Either ConfigParseError LogicalLine
parseLine index value = do
  tableValue <- expectTable context value
  let entries = tableEntries tableValue
  ensureClosed context ["id", "refs"] entries
  identifier <- requireKey context "id" entries >>= expectText (context <> ".id")
  refsValue <- requireKey context "refs" entries
  refTexts <- expectTextArray (context <> ".refs") refsValue
  refs <- traverse parseRef refTexts
  mapLeft (ConfigLogicalLineViolation index) (mkLogicalLine identifier refs)
  where
    context = "line[" <> T.pack (show index) <> "]"
    parseRef refText = mapLeft (ConfigGitRefViolation context) (mkGitRef refText)

tableEntries :: Toml.Table -> Map Text Toml.Value
tableEntries (Toml.MkTable entries) = Map.map snd entries

ensureClosed :: Text -> [Text] -> Map Text Toml.Value -> Either ConfigParseError ()
ensureClosed context allowed entries =
  case filter (`notElem` allowed) (Map.keys entries) of
    unknown : _ -> Left (ConfigUnknownKey context unknown)
    [] -> Right ()

requireKey :: Text -> Text -> Map Text Toml.Value -> Either ConfigParseError Toml.Value
requireKey context key entries =
  maybe (Left (ConfigMissingKey context key)) Right (Map.lookup key entries)

expectInteger :: Text -> Toml.Value -> Either ConfigParseError Integer
expectInteger _ (Toml.Integer' _ value) = Right value
expectInteger context value = Left (expectedType context "integer" value)

expectText :: Text -> Toml.Value -> Either ConfigParseError Text
expectText _ (Toml.Text' _ value) = Right value
expectText context value = Left (expectedType context "string" value)

expectTable :: Text -> Toml.Value -> Either ConfigParseError Toml.Table
expectTable _ (Toml.Table' _ value) = Right value
expectTable context value = Left (expectedType context "table" value)

expectTextArray :: Text -> Toml.Value -> Either ConfigParseError [Text]
expectTextArray context (Toml.List' _ values) = traverse (expectText context) values
expectTextArray context value = Left (expectedType context "array of strings" value)

expectedType :: Text -> Text -> Toml.Value -> ConfigParseError
expectedType context expected actual =
  ConfigExpectedType context expected (valueTypeName actual)

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

mapLeft :: (left -> otherLeft) -> Either left right -> Either otherLeft right
mapLeft action value =
  case value of
    Left problem -> Left (action problem)
    Right result -> Right result
