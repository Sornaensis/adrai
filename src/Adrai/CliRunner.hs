{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

-- | CLI argument parsing and dispatch logic.
-- Builds the optparse-applicative parser from the command types defined
-- in "Adrai.CliTypes" and dispatches parsed commands to the JSON-projection
-- helpers in that same module.
module Adrai.CliRunner
  ( CliConfig (..),
    defaultCliConfig,
    CliInvocation (..),
    CliCommand (..),
    InitCommand (..),
    CreateCommand (..),
    ContentSource (..),
    CreateRequest (..),
    CliDispatchDependencies (..),
    dispatchWith,
    CliParser,
    parser,
    parseStructuredCreate,
    parseActor,
    parseDigest,
    CliFailure (..),
    CliRendered (..),
    parseArguments,
    renderInitOutcome,
    renderCreateOutcome,
    renderFailureOutcome,
    emitRenderedToHandles,
    run,
  )
where

import Adrai.CliTypes
  ( ShowCommand (..),
    HistoryCommand (..),
    SearchCommand (..),
    RelevantCommand (..),
    CompareCommand (..),
  )
import Adrai.History (HistoryOrder (..))
import Adrai.Retrieval (RetrievalMode (FtsRetrieval, HybridRetrieval, VectorRetrieval))
import Adrai.Types (ViewMode (CollapsedView, ExplodedView))
import Adrai.Domain (Domain, canonicalDomains, domainErrorText, domainText)
import Adrai.Git (GitOid (..), Repository, RevisionSpec (RevisionSpec), discoverRepository, gitOidText, repositoryWorktreeRoot, systemGit)
import Adrai.Identity (sortableAdrId, sortableRecordId)
import qualified Adrai.Format as Format
import Adrai.Format.Json (JsonValue (..), renderCanonicalJson)
import Adrai.Provenance (sha256Digest)
import Adrai.Repository (repositorySnapshot, repositorySnapshotManagedPaths)
import Adrai.Scope (ScopePattern, mkScopePattern, scopePatternErrorText)
import Adrai.Service.Mutation (CreateResult (..), InitResult (..), createAdrCommand, initCommand)
import Adrai.Service.PostCommitIndex
  ( IndexWarning (..),
    PostCommitIndexError (..),
    PostCommitIndexResult (..),
    compilePostCommitIndex,
  )
import Adrai.Service.Transaction (TransactionError (..))
import Adrai.Types
  ( Actor,
    ActorKind (..),
    Digest,
    AdrId,
    RecordId,
    adrIdText,
    connectionIdText,
    mkActor,
    mkDigest,
    recordIdText,
    repoPathText,
  )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, lookupEnv)
import System.FilePath ((</>))
import System.IO (Handle, hGetContents, stderr, stdin, stdout)
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Control.Exception (SomeException, displayException, try)
import qualified Options.Applicative as Opt
import Options.Applicative
  ( Parser,
    execParser,
    (<|>),
    optional,
    many,
    info,
    subparser,
    command,
    strArgument,
    flag',
    strOption,
    option,
    switch,
    auto,
    value,
    showDefault,
    metavar,
    help,
    long,
    short,
    progDesc,
  )

import System.Exit (exitWith, ExitCode (..))

-- | Runtime configuration for the CLI.
data CliConfig = CliConfig
  { configRepo       :: FilePath
  , configDatabase   :: Maybe FilePath
  , configAt         :: Text
  }
  deriving (Eq, Show)

-- | Default configuration: current directory, no explicit database, HEAD revision.
defaultCliConfig :: CliConfig
defaultCliConfig =
  CliConfig
    { configRepo = "."
    , configDatabase = Nothing
    , configAt = "HEAD"
    }

-- | The global configuration is parsed before the subcommand, matching the
-- public @adrai [--repo PATH] COMMAND@ invocation shape.
data CliInvocation = CliInvocation
  { invocationConfig :: CliConfig
  , invocationCommand :: CliCommand
  }
  deriving (Eq, Show)

data InitCommand = InitCommand
  { initJson :: Bool
  }
  deriving (Eq, Show)

data ContentSource
  = BodyText Text
  | BodyFile FilePath
  | BodyStdin
  | InputJson FilePath
  deriving (Eq, Show)

data CreateCommand = CreateCommand
  { createTitle :: Maybe Text
  , createSummary :: Maybe Text
  , createDomains :: [Text]
  , createAppliesTo :: [Text]
  , createActorSpec :: Maybe Text
  , createModel :: Maybe Text
  , createInputDigest :: Maybe Text
  , createPromptDigest :: Maybe Text
  , createContextDigest :: Maybe Text
  , createPromptFile :: Maybe FilePath
  , createContextFile :: Maybe FilePath
  , createContentSources :: [ContentSource]
  , createJson :: Bool
  }
  deriving (Eq, Show)

-- | Parsed CLI command, constructed from optparse-applicative.
data CliCommand
  = CmdCompile
  | CmdDoctor
  | CmdShow ShowCommand
  | CmdHistory HistoryCommand
  | CmdSearch SearchCommand
  | CmdRelevant RelevantCommand
  | CmdCompare CompareCommand
  | CmdInit InitCommand
  | CmdCreate CreateCommand
  deriving (Eq, Show)

-- | Top-level CLI parser type alias.
type CliParser = Parser CliInvocation

-- | Top-level CLI parser combining all subcommands.
parser :: CliParser
parser =
  CliInvocation <$> globalConfigParser <*> subparser
    ( command "compile" (info (pure CmdCompile) (progDesc "compile the repository"))
     <> command "doctor" (info (pure CmdDoctor) (progDesc "diagnose the database"))
     <> command "show" (info (CmdShow <$> showParser) (progDesc "show a single ADR"))
     <> command "history" (info (CmdHistory <$> historyParser) (progDesc "show operation history"))
     <> command "search" (info (CmdSearch <$> searchParser) (progDesc "search ADRs"))
     <> command "relevant" (info (CmdRelevant <$> relevantParser) (progDesc "find relevant ADRs"))
     <> command "compare" (info (CmdCompare <$> compareParser) (progDesc "compare revisions"))
     <> command "init" (info (CmdInit <$> initParser) (progDesc "initialize an ADRAI repository"))
     <> command "create" (info (CmdCreate <$> createParser) (progDesc "create an ADR"))
    )

globalConfigParser :: Parser CliConfig
globalConfigParser =
  (\repo -> defaultCliConfig {configRepo = repo})
    <$> strOption
      ( long "repo"
       <> metavar "PATH"
       <> value "."
       <> showDefault
       <> help "repository directory (default: current directory)"
      )

initParser :: Parser InitCommand
initParser = InitCommand <$> switch (long "json" <> help "output JSON")

createParser :: Parser CreateCommand
createParser =
  CreateCommand
    <$> maybeText "title" "TEXT" "decision title"
    <*> maybeText "summary" "TEXT" "decision summary"
    <*> many (optionalText "domain" "DOMAIN" "domain (repeatable)")
    <*> many (optionalText "applies-to" "PATTERN" "scope pattern (repeatable)")
    <*> maybeText "actor" "ACTOR" "actor as kind:identifier"
    <*> maybeText "model" "MODEL" "actor model"
    <*> maybeText "input-digest" "DIGEST" "SHA-256 input digest"
    <*> maybeText "prompt-digest" "DIGEST" "SHA-256 prompt digest"
    <*> maybeText "context-digest" "DIGEST" "SHA-256 context digest"
    <*> optional (strOption (long "prompt-file" <> metavar "PATH" <> help "prompt source path"))
    <*> optional (strOption (long "context-file" <> metavar "PATH" <> help "context source path"))
    <*> contentSourcesParser
    <*> switch (long "json" <> help "output JSON")
  where
    optionalText name marker description =
      strOption (long name <> metavar marker <> help description)
    maybeText name marker description = optional (optionalText name marker description)

contentSourcesParser :: Parser [ContentSource]
contentSourcesParser = pure <$> contentSourceParser
  where
    contentSourceParser =
      BodyText <$> strOption (long "body" <> metavar "TEXT" <> help "literal decision body")
        <|> BodyFile <$> strOption (long "body-file" <> metavar "PATH" <> help "body file")
        <|> BodyStdin <$ flag' () (long "stdin" <> help "read body from standard input")
        <|> InputJson <$> strOption (long "input-json" <> metavar "PATH_OR_DASH" <> help "strict structured create input")

-- | Show command parser.
showParser :: Parser ShowCommand
showParser =
  ShowCommand
    <$> strArgument (metavar "ID" <> help "ADR identifier to show")
    <*> ( flag' CollapsedView (long "collapsed" <> help "collapsed view (default)")
        <|> flag' ExplodedView (long "exploded" <> help "exploded view")
        )
    <*> switch (long "json" <> help "output JSON")
    <*> switch (long "raw" <> help "include raw semantic data")
    <*> switch (long "rich" <> help "rich collapsed view")

-- | History command parser.
historyParser :: Parser HistoryCommand
historyParser =
  HistoryCommand
    <$> optional (strArgument (metavar "ADR_ID" <> help "optional ADR ID or prefix"))
    <*> ( flag' NewestFirst (long "newest-first" <> help "show newest entries first (default)")
        <|> flag' OldestFirst (long "oldest-first" <> help "show oldest entries first")
        )
    <*> option auto (long "limit" <> value 50 <> showDefault <> help "max results (default 50)")
    <*> optional (liftA2 (,) (strArgument (metavar "ACTOR" <> help "actor kind"))
                                 (strArgument (metavar "MODEL" <> help "model name")))
    <*> optional (option auto (long "since" <> help "show entries after this timestamp"))
    <*> optional (option auto (long "until" <> help "show entries before this timestamp"))
    <*> switch (long "reverse" <> help "reverse the order")
    <*> switch (long "json" <> help "output JSON")

-- | Search command parser.
searchParser :: Parser SearchCommand
searchParser =
  SearchCommand
    <$> strOption
      ( long "query"
       <> short 'q'
       <> metavar "QUERY"
       <> help "search query string"
      )
    <*> ( flag' FtsRetrieval (long "fts" <> help "full-text search (default)")
        <|> flag' VectorRetrieval (long "vector" <> help "semantic/vector search")
        <|> flag' HybridRetrieval (long "hybrid" <> help "full-text + semantic combined")
        )
    <*> ( flag' CollapsedView (long "collapsed" <> help "collapsed view (default)")
        <|> flag' ExplodedView (long "exploded" <> help "exploded view")
        )
    <*> optional (strOption (long "file" <> metavar "PATH" <> help "filter by file path"))
    <*> many (strOption (long "domain" <> metavar "DOMAIN" <> help "domain filter (repeatable)"))
    <*> optional (liftA2 (,) (strOption (long "actor" <> metavar "KIND" <> help "actor kind"))
                              (strOption (long "model" <> metavar "NAME" <> help "actor model")))
    <*> optional (option auto (long "since" <> help "entries after this timestamp"))
    <*> optional (option auto (long "until" <> help "entries before this timestamp"))
    <*> switch (long "include-obsolete" <> help "include obsolete ADRs")
    <*> option auto (long "limit" <> value 20 <> showDefault <> help "max results (default 20)")
    <*> switch (long "json" <> help "output JSON")

-- | Relevant command parser.
relevantParser :: Parser RelevantCommand
relevantParser =
  RelevantCommand
    <$> strArgument (metavar "FILE" <> help "file path to find relevant ADRs for")
    <*> switch (long "include-obsolete" <> help "include obsolete ADRs")
    <*> option auto (long "limit" <> value 10 <> showDefault <> help "max results (default 10)")
    <*> switch (long "json" <> help "output JSON")

-- | Compare command parser.
compareParser :: Parser CompareCommand
compareParser =
  CompareCommand
    <$> strOption
      ( long "from"
       <> short 'f'
       <> metavar "REVISION"
       <> help "revision to compare from"
      )
    <*> ( strOption
            ( long "to"
             <> short 't'
             <> metavar "REVISION"
             <> help "revision to compare to (default HEAD)"
            )
          <|> pure "HEAD"
        )
    <*> switch (long "include-unchanged" <> help "include unchanged items")
    <*> switch (long "json" <> help "output JSON")

-- | Run the CLI: parse arguments and dispatch to the appropriate handler.
run :: IO ()
run = do
  arguments <- getArgs
  case Opt.execParserPure Opt.defaultPrefs parserInfo arguments of
    Opt.Success invocation -> dispatch invocation >>= exitWith
    Opt.Failure failure -> do
      let (message, parserExit) = Opt.renderFailure failure "adrai"
          exitCode = if parserExit == ExitSuccess then ExitSuccess else ExitFailure 2
      writeUtf8 stderr (Text.pack message)
      exitWith exitCode
    Opt.CompletionInvoked completion -> Opt.execCompletion completion "adrai" >>= putStr
  where
    parserInfo = info parser (progDesc "ADRAI - Architecture Decision Record tool")

parseArguments :: [String] -> Either CliRendered CliInvocation
parseArguments arguments =
  case Opt.execParserPure Opt.defaultPrefs parserInfo arguments of
    Opt.Success invocation -> Right invocation
    Opt.Failure failure ->
      let (message, _) = Opt.renderFailure failure "adrai"
       in Left (CliRendered "" (Text.pack message) (ExitFailure 2))
    Opt.CompletionInvoked _ -> Left (CliRendered "" "" ExitSuccess)
  where
    parserInfo = info parser (progDesc "ADRAI - Architecture Decision Record tool")

-- | Dispatch a parsed command to its handler.
dispatch :: CliInvocation -> IO ExitCode
dispatch = dispatchWith productionCliDependencies

dispatchWith :: CliDispatchDependencies -> CliInvocation -> IO ExitCode
dispatchWith dependencies (CliInvocation config (CmdInit command)) = do
  result <- cliRunInit dependencies config
  case result of
    Left failure -> renderFailure failure
    Right (initResult, indexResult) -> renderInitSuccess initResult indexResult (initJson command)
dispatchWith dependencies (CliInvocation config (CmdCreate command)) = do
  requestResult <- cliMaterializeCreate dependencies command
  case requestResult of
    Left problem -> renderFailure (CliUserFailure problem)
    Right request -> do
      result <- cliRunCreate dependencies config request
      case result of
        Left failure -> renderFailure failure
        Right (createResult, indexResult) -> renderCreateSuccess command createResult (requestDomains request) indexResult
dispatchWith _ (CliInvocation _ CmdCompile) = do
  putStrLn $ "[compile] compiling repository at " <> configRepo defaultCliConfig
  pure ExitSuccess
dispatchWith _ (CliInvocation _ CmdDoctor) = do
  putStrLn "[doctor] diagnosing database"
  pure ExitSuccess
dispatchWith _ (CliInvocation _ (CmdShow ShowCommand { showAdrId })) = do
  putStrLn $ "[show] looking up ADR: " <> Text.unpack showAdrId
  pure ExitSuccess
dispatchWith _ (CliInvocation _ CmdHistory {}) = do
  putStrLn "[history] fetching history"
  pure ExitSuccess
dispatchWith _ (CliInvocation _ (CmdSearch SearchCommand { searchQuery })) = do
  putStrLn $ "[search] querying: " <> Text.unpack searchQuery
  pure ExitSuccess
dispatchWith _ (CliInvocation _ CmdRelevant {}) = do
  putStrLn "[relevant] finding relevant ADRs"
  pure ExitSuccess
dispatchWith _ (CliInvocation _ (CmdCompare CompareCommand { compareBefore, compareAfter })) = do
  putStrLn $ "[compare] from: " <> Text.unpack compareBefore <> ", to: " <> Text.unpack compareAfter
  pure ExitSuccess

data CliDispatchDependencies = CliDispatchDependencies
  { cliMaterializeCreate :: CreateCommand -> IO (Either Text CreateRequest)
  , cliRunInit :: CliConfig -> IO (Either CliFailure (InitResult, PostCommitIndexResult))
  , cliRunCreate :: CliConfig -> CreateRequest -> IO (Either CliFailure (CreateResult, PostCommitIndexResult))
  }

productionCliDependencies :: CliDispatchDependencies
productionCliDependencies =
  CliDispatchDependencies materializeCreate runProductionInit runProductionCreate

runProductionInit :: CliConfig -> IO (Either CliFailure (InitResult, PostCommitIndexResult))
runProductionInit config = do
  repositoryResult <- discoverRepository systemGit (configRepo config)
  case repositoryResult of
    Left problem -> pure (Left (CliUserFailure (Text.pack (show problem))) )
    Right repository -> do
      indexPath <- prepareIndexPath repository
      case indexPath of
        Left problem -> pure (Left (CliUserFailure problem))
        Right database -> do
          result <- initCommand repository
          case result of
            Left problem -> pure (Left (transactionFailure problem))
            Right initResult -> Right . (initResult,) <$> indexCommitted database repository (initCommitOid initResult)

runProductionCreate :: CliConfig -> CreateRequest -> IO (Either CliFailure (CreateResult, PostCommitIndexResult))
runProductionCreate config request = do
  repositoryResult <- discoverRepository systemGit (configRepo config)
  case repositoryResult of
    Left problem -> pure (Left (CliUserFailure (Text.pack (show problem))))
    Right repository -> do
      indexPath <- prepareIndexPath repository
      case indexPath of
        Left problem -> pure (Left (CliUserFailure problem))
        Right database -> do
          snapshotResult <- repositorySnapshot repository (RevisionSpec "HEAD")
          case snapshotResult of
            Left problem -> pure (Left (CliUserFailure (Text.pack (show problem))))
            Right snapshot -> do
              identifiers <- freshCreateIdentifiers
              case identifiers of
                Left problem -> pure (Left (CliUserFailure problem))
                Right (adr, record) -> do
                  result <- createAdrCommand repository (repositorySnapshotManagedPaths snapshot)
                    (requestActor request) adr record
                    (requestTitle request) (requestSummary request) (requestBody request)
                    (requestDomains request) (requestScopes request)
                    (requestInputDigest request) (requestPromptDigest request) (requestContextDigest request)
                  case result of
                    Left problem -> pure (Left (transactionFailure problem))
                    Right createResult -> Right . (createResult,) <$> indexCommitted database repository (createCommitOid createResult)

transactionFailure :: TransactionError -> CliFailure
transactionFailure problem
  | transactionConflict problem = CliConflictFailure (Text.pack (show problem))
  | otherwise = CliUserFailure (Text.pack (show problem))

data CliFailure
  = CliUserFailure Text
  | CliConflictFailure Text

data CreateRequest = CreateRequest
  { requestTitle :: Text
  , requestSummary :: Text
  , requestBody :: Text
  , requestDomains :: [Domain]
  , requestScopes :: [ScopePattern]
  , requestActor :: Actor
  , requestInputDigest :: Maybe Digest
  , requestPromptDigest :: Maybe Digest
  , requestContextDigest :: Maybe Digest
  }
  deriving (Eq, Show)

data StructuredCreate = StructuredCreate
  { structuredTitle :: Maybe Text
  , structuredSummary :: Maybe Text
  , structuredBody :: Maybe Text
  , structuredDomains :: Maybe [Text]
  , structuredScopes :: Maybe [Text]
  , structuredActor :: Maybe Text
  , structuredModel :: Maybe Text
  , structuredInputDigest :: Maybe Text
  , structuredPromptDigest :: Maybe Text
  , structuredContextDigest :: Maybe Text
  }

materializeCreate :: CreateCommand -> IO (Either Text CreateRequest)
materializeCreate command = do
  contentResult <- readContent command
  case contentResult of
    Left problem -> pure (Left problem)
    Right (maybeStructured, body) -> do
      environmentActor <- lookupEnv "ADRAI_ACTOR"
      promptFromFile <- readDigestFile "prompt" (createPromptFile command)
      contextFromFile <- readDigestFile "context" (createContextFile command)
      pure $ do
        promptFileDigest <- promptFromFile
        contextFileDigest <- contextFromFile
        let structured = fromMaybe emptyStructured maybeStructured
            title = fromMaybe "" (createTitle command <|> structuredTitle structured)
            summary = fromMaybe "" (createSummary command <|> structuredSummary structured)
            domains = if null (createDomains command) then fromMaybe [] (structuredDomains structured) else createDomains command
            scopes = if null (createAppliesTo command) then fromMaybe [] (structuredScopes structured) else createAppliesTo command
            actorSpec = createActorSpec command <|> structuredActor structured <|> Text.pack <$> environmentActor
            model = createModel command <|> structuredModel structured
            inputDigest = createInputDigest command <|> structuredInputDigest structured
            promptDigest = createPromptDigest command <|> structuredPromptDigest structured
            contextDigest = createContextDigest command <|> structuredContextDigest structured
        actorText <- maybe (Left "create requires --actor, structured actor, or ADRAI_ACTOR") Right actorSpec
        actor <- parseActor actorText model
        validatedDomains <- first domainErrorText (canonicalDomains domains)
        validatedScopes <- traverse (first scopePatternErrorText . mkScopePattern) scopes
        input <- maybe (Right (Just (sha256Digest (TextEncoding.encodeUtf8 body)))) (fmap Just . parseDigest) inputDigest
        prompt <- resolveDigest "prompt" promptDigest promptFileDigest
        context <- resolveDigest "context" contextDigest contextFileDigest
        Right CreateRequest
          { requestTitle = title
          , requestSummary = summary
          , requestBody = body
          , requestDomains = validatedDomains
          , requestScopes = validatedScopes
          , requestActor = actor
          , requestInputDigest = input
          , requestPromptDigest = prompt
          , requestContextDigest = context
          }

emptyStructured :: StructuredCreate
emptyStructured = StructuredCreate Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

readContent :: CreateCommand -> IO (Either Text (Maybe StructuredCreate, Text))
readContent command =
  case createContentSources command of
    [] -> pure (Left "create requires exactly one of --body, --body-file, --stdin, or --input-json")
    [_first, _second, _third, _fourth] -> pure (Left "create accepts exactly one content source")
    source : [] ->
      case source of
        BodyText body -> pure (Right (Nothing, body))
        BodyFile path -> readUtf8 path >>= pure . fmap (\body -> (Nothing, body))
        BodyStdin -> do
          body <- Text.pack <$> hGetContents stdin
          pure (Right (Nothing, body))
        InputJson path -> do
          raw <- if path == "-" then Right . Text.pack <$> hGetContents stdin else readUtf8 path
          pure $ do
            input <- raw
            structured <- parseStructuredCreate input
            body <- maybe (Left "structured create input requires string field body") Right (structuredBody structured)
            Right (Just structured, body)
    _ -> pure (Left "create accepts exactly one content source")

readUtf8 :: FilePath -> IO (Either Text Text)
readUtf8 path = do
  bytes <- readBytes path
  pure $ do
    input <- bytes
    first (const ("invalid UTF-8 input: " <> Text.pack path)) (TextEncoding.decodeUtf8' input)

readBytes :: FilePath -> IO (Either Text ByteString.ByteString)
readBytes path = do
  result <- try (ByteString.readFile path) :: IO (Either SomeException ByteString.ByteString)
  pure $ first (\exception -> "unable to read " <> Text.pack path <> ": " <> Text.pack (displayException exception)) result

readDigestFile :: Text -> Maybe FilePath -> IO (Either Text (Maybe Digest))
readDigestFile _ Nothing = pure (Right Nothing)
readDigestFile label (Just path) = do
  bytes <- readBytes path
  pure $ do
    content <- bytes
    Right (Just (sha256Digest content))

resolveDigest :: Text -> Maybe Text -> Maybe Digest -> Either Text (Maybe Digest)
resolveDigest _ Nothing fromFile = Right fromFile
resolveDigest label (Just explicitText) fromFile = do
  explicit <- parseDigest explicitText
  case fromFile of
    Nothing -> Right (Just explicit)
    Just derived
      | derived == explicit -> Right (Just explicit)
      | otherwise -> Left (label <> " digest conflicts with the digest derived from its file")

parseStructuredCreate :: Text -> Either Text StructuredCreate
parseStructuredCreate input = do
  value <- first (Text.pack . show) (Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 input))
  object <- case value of
    Aeson.Object fields -> Right fields
    _ -> Left "structured create input must be a JSON object"
  let allowed = ["title", "summary", "body", "domains", "applies_to", "actor", "model", "input_digest", "prompt_digest", "context_digest"]
      unknown = filter (`notElem` allowed) (map Aeson.Key.toText (KeyMap.keys object))
  if null unknown
    then StructuredCreate
      <$> optionalString "title" object
      <*> optionalString "summary" object
      <*> optionalString "body" object
      <*> optionalStrings "domains" object
      <*> optionalStrings "applies_to" object
      <*> optionalString "actor" object
      <*> optionalString "model" object
      <*> optionalString "input_digest" object
      <*> optionalString "prompt_digest" object
      <*> optionalString "context_digest" object
    else Left ("structured create input has unknown fields: " <> Text.intercalate ", " unknown)

optionalString :: Text -> Aeson.Object -> Either Text (Maybe Text)
optionalString name object =
  case KeyMap.lookup (Aeson.Key.fromText name) object of
    Nothing -> Right Nothing
    Just (Aeson.String value) -> Right (Just value)
    Just _ -> Left ("structured create field " <> name <> " must be a string")

optionalStrings :: Text -> Aeson.Object -> Either Text (Maybe [Text])
optionalStrings name object =
  case KeyMap.lookup (Aeson.Key.fromText name) object of
    Nothing -> Right Nothing
    Just (Aeson.Array values) -> Just <$> traverse valueText (toList values)
    Just _ -> Left ("structured create field " <> name <> " must be an array of strings")
  where
    valueText (Aeson.String value) = Right value
    valueText _ = Left ("structured create field " <> name <> " must be an array of strings")
    toList = foldr (:) []

parseActor :: Text -> Maybe Text -> Either Text Actor
parseActor value model =
  case Text.splitOn ":" value of
    [kindText, identifier] -> do
      kind <- case kindText of
        "human" -> Right HumanActor
        "llm" -> Right LlmActor
        "service" -> Right ServiceActor
        _ -> Left "actor kind must be human, llm, or service"
      first (Text.pack . show) (mkActor kind identifier model)
    _ -> Left "actor must have the form kind:identifier"

parseDigest :: Text -> Either Text Digest
parseDigest = first (Text.pack . show) . Format.parseDigest

freshCreateIdentifiers :: IO (Either Text (AdrId, RecordId))
freshCreateIdentifiers = do
  milliseconds <- floor . (* 1000) <$> getPOSIXTime
  entropy <- ByteString.pack <$> replicateM 10 (randomRIO (0, 255))
  let timestamp = ByteString.pack
        [ fromIntegral ((milliseconds `div` (256 ^ offset)) `mod` 256)
        | offset <- [5, 4 .. 0]
        ]
  pure $ do
    adr <- first (Text.pack . show) (sortableAdrId timestamp entropy)
    record <- first (Text.pack . show) (sortableRecordId timestamp entropy)
    Right (adr, record)

prepareIndexPath :: Repository -> IO (Either Text FilePath)
prepareIndexPath repository =
  case repositoryWorktreeRoot repository of
    Nothing -> pure (Left "worktree root is missing")
    Just root -> do
      let directory = root </> ".adrai"
          database = directory </> "index.sqlite"
      prepared <- try (createDirectoryIfMissing True directory) :: IO (Either SomeException ())
      case prepared of
        Left exception -> pure (Left ("unable to prepare index path: " <> Text.pack (displayException exception)))
        Right () -> pure (Right database)

indexCommitted :: FilePath -> Repository -> GitOid -> IO PostCommitIndexResult
indexCommitted database repository commit = do
  indexed <- try (compilePostCommitIndex repository commit database) :: IO (Either SomeException PostCommitIndexResult)
  pure $ case indexed of
    Left exception -> indexFailure (PostCommitIndexCompileException (Text.pack (displayException exception)))
    Right result -> result
  where
    indexFailure problem = PostCommitIndexResult False Nothing Nothing [] (Just problem)

data CliRendered = CliRendered
  { renderedStdout :: Text
  , renderedStderr :: Text
  , renderedExitCode :: ExitCode
  }
  deriving (Eq, Show)

renderInitSuccess :: InitResult -> PostCommitIndexResult -> Bool -> IO ExitCode
renderInitSuccess result indexResult jsonOutput = emitRendered (renderInitOutcome result indexResult jsonOutput)

renderCreateSuccess :: CreateCommand -> CreateResult -> [Domain] -> PostCommitIndexResult -> IO ExitCode
renderCreateSuccess command result domains indexResult =
  emitRendered (renderCreateOutcome result domains indexResult (createJson command))

renderInitOutcome :: InitResult -> PostCommitIndexResult -> Bool -> CliRendered
renderInitOutcome result indexResult jsonOutput =
  successOutcome jsonOutput (Text.pack (initOperationId result)) (initCommitOid result) [] indexResult
    (mutationJsonFields (initOperationId result) (initCommitOid result) (initCreatedPaths result) (initIndexUpdated result) indexResult <> [("initialized", JsonBool (initInitialized result))])

renderCreateOutcome :: CreateResult -> [Domain] -> PostCommitIndexResult -> Bool -> CliRendered
renderCreateOutcome result domains indexResult jsonOutput =
  successOutcome jsonOutput (Text.pack (createOperationId result)) (createCommitOid result) identifiers indexResult jsonFields
  where
    identifiers =
      [ "adr=" <> adrIdText (createAdrId result)
      , "record=" <> recordIdText (createRecordId result)
      , "scope=" <> connectionIdText (createScopeId result)
      , "domain=" <> connectionIdText (createDomainId result)
      , "status=" <> connectionIdText (createStatusId result)
      ]
    jsonFields =
      mutationJsonFields (createOperationId result) (createCommitOid result) (createCreatedPaths result) (createIndexUpdated result) indexResult
        <> [ ("adr", JsonString (adrIdText (createAdrId result)))
           , ("record", JsonString (recordIdText (createRecordId result)))
           , ("scope", JsonString (connectionIdText (createScopeId result)))
           , ("domain", JsonString (connectionIdText (createDomainId result)))
           , ("domains", JsonArray (map (JsonString . domainText) domains))
           , ("status", JsonString (connectionIdText (createStatusId result)))
           ]

successOutcome jsonOutput operation commit identifiers indexResult jsonFields
  | jsonOutput = CliRendered (renderCanonicalJson (JsonObject jsonFields)) "" ExitSuccess
  | otherwise = CliRendered (plainText operation commit identifiers indexResult) "" ExitSuccess

mutationJsonFields operation commit created indexUpdated indexResult =
  [ ("committed", JsonBool True)
  , ("operation", JsonString (Text.pack operation))
  , ("commit", JsonString (gitOidText commit))
  , ("created", JsonArray (map (JsonString . repoPathText) created))
  , ("index_updated", JsonBool indexUpdated)
  , ("indexed", JsonBool (postCommitIndexed indexResult))
  ] <> indexJsonFields indexResult

indexJsonFields indexResult
  | postCommitIndexed indexResult =
      [ ("database", maybe JsonNull (JsonString . Text.pack) (postCommitDatabase indexResult))
      , ("index_revision", maybe JsonNull (JsonString . gitOidText) (postCommitIndexRevision indexResult))
      , ("index_warnings", JsonNumber (fromIntegral (length (postCommitIndexWarnings indexResult))))
      ]
  | otherwise =
      [("index_error", maybe JsonNull (JsonString . Text.pack . show) (postCommitIndexError indexResult))]

plainText :: Text -> GitOid -> [Text] -> PostCommitIndexResult -> Text
plainText operation commit identifiers indexResult =
  case identifiers of
    [adr, record, scope, domain, status] ->
      Text.unlines
        [ "Committed " <> operation <> " as " <> gitOidText commit
        , adr <> "  " <> record <> "  " <> scope <> "  " <> domain <> "  " <> status
        , indexLine
        ]
    _ -> Text.unlines ["Committed " <> operation <> " as " <> gitOidText commit, indexLine]
  where
    indexLine
      | postCommitIndexed indexResult = "SQLite indexed."
      | otherwise = "SQLite indexing failed: " <> maybe "unknown failure" (Text.pack . show) (postCommitIndexError indexResult)

renderTransactionFailure :: TransactionError -> IO ExitCode
renderTransactionFailure = emitRendered . renderTransactionOutcome

renderTransactionOutcome :: TransactionError -> CliRendered
renderTransactionOutcome problem
  | transactionConflict problem = renderFailureOutcome (CliConflictFailure (Text.pack (show problem)))
  | otherwise = renderFailureOutcome (CliUserFailure (Text.pack (show problem)))

transactionConflict :: TransactionError -> Bool
transactionConflict problem =
  case problem of
    Stage3ValidateState message -> "expected head mismatch" `Text.isInfixOf` message
    _ -> False

renderFailure :: CliFailure -> IO ExitCode
renderFailure = emitRendered . renderFailureOutcome

renderFailureOutcome :: CliFailure -> CliRendered
renderFailureOutcome failure =
  case failure of
    CliUserFailure message -> CliRendered "" ("adrai: " <> message <> "\n") (ExitFailure 2)
    CliConflictFailure message -> CliRendered "" ("adrai: conflict: " <> message <> "\n") (ExitFailure 3)

emitRendered :: CliRendered -> IO ExitCode
emitRendered = emitRenderedToHandles stdout stderr

-- | Emit CLI output as raw UTF-8 bytes.  This deliberately avoids the
-- platform text-mode newline translation performed by 'hPutStr' on Windows,
-- so canonical JSON's LF bytes stay canonical on every supported platform.
emitRenderedToHandles :: Handle -> Handle -> CliRendered -> IO ExitCode
emitRenderedToHandles output errorOutput rendered = do
  if Text.null (renderedStdout rendered) then pure () else writeUtf8 output (renderedStdout rendered)
  if Text.null (renderedStderr rendered) then pure () else writeUtf8 errorOutput (renderedStderr rendered)
  pure (renderedExitCode rendered)

writeUtf8 :: Handle -> Text -> IO ()
writeUtf8 handle = ByteString.hPut handle . TextEncoding.encodeUtf8
