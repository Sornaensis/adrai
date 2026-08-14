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
    AmendCommand (..),
    ScopeCommand (..),
    DomainCommand (..),
    ContentSource (..),
    CreateRequest (..),
    AmendRequest (..),
    ScopeRequest (..),
    DomainRequest (..),
    CliDispatchDependencies (..),
    dispatchWith,
    CliParser,
    parser,
    parseStructuredCreate,
    parseStructuredAmend,
    materializeScope,
    materializeDomain,
    parseActor,
    parseDigest,
    CliFailure (..),
    CliRendered (..),
    parseArguments,
    renderInitOutcome,
    renderCreateOutcome,
    renderAmendOutcome,
    renderScopeOutcome,
    renderDomainOutcome,
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
import Adrai.Domain (Domain, DomainRefinement, canonicalDomains, domainErrorText, domainText, domainRefinementText, mkDomain, parseDomainRefinement)
import Adrai.Git (GitOid (..), Repository, RevisionSpec (RevisionSpec), discoverRepository, gitOidText, repositoryWorktreeRoot, systemGit)
import Adrai.Identity (sortableAdrId, sortableRecordId)
import qualified Adrai.Format as Format
import Adrai.Format.Json (JsonValue (..), renderCanonicalJson)
import Adrai.Provenance (sha256Digest)
import Adrai.Repository (repositorySnapshot, repositorySnapshotManagedPaths)
import Adrai.Scope (ScopePattern, mkScopePattern, scopePatternErrorText, scopePatternText)
import Adrai.Service.Mutation (AmendResult (..), CreateResult (..), DomainChangeRequest (..), DomainChangeResult (..), InitResult (..), ScopeChangeRequest (..), ScopeChangeResult (..), amendCurrentAdrCommand, changeDomainCommand, changeScopeCommand, createAdrCommand, initCommand)
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
     StateToken,
     ProvenanceInputs (..),
     adrIdText,
     connectionIdText,
     mkActor,
     mkAdrId,
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

data AmendCommand = AmendCommand
  { amendAdrSpec :: Text
  , amendTitle :: Maybe Text
  , amendSummary :: Maybe Text
  , amendChangeSummary :: Maybe Text
  , amendExpectedState :: Maybe Text
  , amendActorSpec :: Maybe Text
  , amendModel :: Maybe Text
  , amendInputDigest :: Maybe Text
  , amendPromptDigest :: Maybe Text
  , amendContextDigest :: Maybe Text
  , amendPromptFile :: Maybe FilePath
  , amendContextFile :: Maybe FilePath
  , amendContentSources :: [ContentSource]
  , amendJson :: Bool
  }
  deriving (Eq, Show)

data ScopeCommand = ScopeCommand
  { scopeAdrSpec :: Text
  , scopeAdds :: [Text]
  , scopeRemoves :: [Text]
  , scopeSets :: [Text]
  , scopeReason :: Maybe Text
  , scopeExpectedState :: Maybe Text
  , scopeActorSpec :: Maybe Text
  , scopeModel :: Maybe Text
  , scopeInputDigest :: Maybe Text
  , scopePromptDigest :: Maybe Text
  , scopeContextDigest :: Maybe Text
  , scopePromptFile :: Maybe FilePath
  , scopeContextFile :: Maybe FilePath
  , scopeJson :: Bool
  }
  deriving (Eq, Show)

data DomainCommand = DomainCommand
  { domainAdrSpec :: Text
  , domainAdds :: [Text]
  , domainRemoves :: [Text]
  , domainRefines :: [Text]
  , domainSets :: [Text]
  , domainClear :: Bool
  , domainReason :: Maybe Text
  , domainExpectedState :: Maybe Text
  , domainActorSpec :: Maybe Text
  , domainModel :: Maybe Text
  , domainInputDigest :: Maybe Text
  , domainPromptDigest :: Maybe Text
  , domainContextDigest :: Maybe Text
  , domainPromptFile :: Maybe FilePath
  , domainContextFile :: Maybe FilePath
  , domainJson :: Bool
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
  | CmdAmend AmendCommand
  | CmdScope ScopeCommand
  | CmdDomain DomainCommand
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
       <> command "amend" (info (CmdAmend <$> amendParser) (progDesc "amend an ADR"))
       <> command "scope" (info (CmdScope <$> scopeParser) (progDesc "change an ADR scope"))
       <> command "domain" (info (CmdDomain <$> domainParser) (progDesc "change an ADR domain"))
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

amendParser :: Parser AmendCommand
amendParser =
  AmendCommand
    <$> strArgument (metavar "ADR" <> help "ADR identifier to amend")
    <*> maybeText "title" "TEXT" "replacement decision title"
    <*> maybeText "summary" "TEXT" "replacement decision summary"
    <*> maybeText "change-summary" "TEXT" "nonblank amendment rationale"
    <*> maybeText "expect" "STATE_TOKEN" "expected current ADR state token"
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
    optionalText name marker description = strOption (long name <> metavar marker <> help description)
    maybeText name marker description = optional (optionalText name marker description)

scopeParser :: Parser ScopeCommand
scopeParser =
  ScopeCommand
    <$> strArgument (metavar "ADR" <> help "ADR identifier whose scope changes")
    <*> many (optionalText "add" "PATTERN" "scope pattern to add (repeatable)")
    <*> many (optionalText "remove" "PATTERN" "scope pattern to remove (repeatable)")
    <*> many (optionalText "set" "PATTERN" "reviewed replacement scope pattern (repeatable)")
    <*> maybeText "reason" "TEXT" "nonblank scope-change rationale"
    <*> maybeText "expect" "STATE_TOKEN" "expected current ADR state token"
    <*> maybeText "actor" "ACTOR" "actor as kind:identifier"
    <*> maybeText "model" "MODEL" "actor model"
    <*> maybeText "input-digest" "DIGEST" "SHA-256 input digest"
    <*> maybeText "prompt-digest" "DIGEST" "SHA-256 prompt digest"
    <*> maybeText "context-digest" "DIGEST" "SHA-256 context digest"
    <*> optional (strOption (long "prompt-file" <> metavar "PATH" <> help "prompt source path"))
    <*> optional (strOption (long "context-file" <> metavar "PATH" <> help "context source path"))
    <*> switch (long "json" <> help "output JSON")
  where
    optionalText name marker description = strOption (long name <> metavar marker <> help description)
    maybeText name marker description = optional (optionalText name marker description)

domainParser :: Parser DomainCommand
domainParser =
  DomainCommand
    <$> strArgument (metavar "ADR" <> help "ADR identifier whose domains change")
    <*> many (optionalText "add" "DOMAIN" "domain to add (repeatable)")
    <*> many (optionalText "remove" "DOMAIN" "domain to remove (repeatable)")
    <*> many (optionalText "refine" "FROM=TO" "strict domain refinement (repeatable)")
    <*> many (optionalText "set" "DOMAIN" "reviewed replacement domain (repeatable)")
    <*> switch (long "clear" <> help "clear domains by reviewed replacement")
    <*> maybeText "reason" "TEXT" "nonblank domain-change rationale"
    <*> maybeText "expect" "STATE_TOKEN" "expected current ADR state token"
    <*> maybeText "actor" "ACTOR" "actor as kind:identifier"
    <*> maybeText "model" "MODEL" "actor model"
    <*> maybeText "input-digest" "DIGEST" "SHA-256 input digest"
    <*> maybeText "prompt-digest" "DIGEST" "SHA-256 prompt digest"
    <*> maybeText "context-digest" "DIGEST" "SHA-256 context digest"
    <*> optional (strOption (long "prompt-file" <> metavar "PATH" <> help "prompt source path"))
    <*> optional (strOption (long "context-file" <> metavar "PATH" <> help "context source path"))
    <*> switch (long "json" <> help "output JSON")
  where
    optionalText name marker description = strOption (long name <> metavar marker <> help description)
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
dispatchWith dependencies (CliInvocation config (CmdAmend command)) = do
  requestResult <- cliMaterializeAmend dependencies command
  case requestResult of
    Left problem -> renderFailure (CliUserFailure problem)
    Right request -> do
      result <- cliRunAmend dependencies config request
      case result of
        Left failure -> renderFailure failure
        Right (amendResult, indexResult) -> renderAmendSuccess command amendResult indexResult
dispatchWith dependencies (CliInvocation config (CmdScope command)) = do
  requestResult <- cliMaterializeScope dependencies command
  case requestResult of
    Left problem -> renderFailure (CliUserFailure problem)
    Right request -> do
      result <- cliRunScope dependencies config request
      case result of
        Left failure -> renderFailure failure
        Right (scopeResult, indexResult) -> renderScopeSuccess command scopeResult indexResult
dispatchWith dependencies (CliInvocation config (CmdDomain command)) = do
  requestResult <- cliMaterializeDomain dependencies command
  case requestResult of
    Left problem -> renderFailure (CliUserFailure problem)
    Right request -> do
      result <- cliRunDomain dependencies config request
      case result of
        Left failure -> renderFailure failure
        Right (domainResult, indexResult) -> renderDomainSuccess command domainResult indexResult
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
  , cliMaterializeAmend :: AmendCommand -> IO (Either Text AmendRequest)
  , cliMaterializeScope :: ScopeCommand -> IO (Either Text ScopeRequest)
  , cliMaterializeDomain :: DomainCommand -> IO (Either Text DomainRequest)
  , cliRunInit :: CliConfig -> IO (Either CliFailure (InitResult, PostCommitIndexResult))
  , cliRunCreate :: CliConfig -> CreateRequest -> IO (Either CliFailure (CreateResult, PostCommitIndexResult))
  , cliRunAmend :: CliConfig -> AmendRequest -> IO (Either CliFailure (AmendResult, PostCommitIndexResult))
  , cliRunScope :: CliConfig -> ScopeRequest -> IO (Either CliFailure (ScopeChangeResult, PostCommitIndexResult))
  , cliRunDomain :: CliConfig -> DomainRequest -> IO (Either CliFailure (DomainChangeResult, PostCommitIndexResult))
  }

productionCliDependencies :: CliDispatchDependencies
productionCliDependencies =
  CliDispatchDependencies materializeCreate materializeAmend materializeScope materializeDomain runProductionInit runProductionCreate runProductionAmend runProductionScope runProductionDomain

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

runProductionAmend :: CliConfig -> AmendRequest -> IO (Either CliFailure (AmendResult, PostCommitIndexResult))
runProductionAmend config request = do
  repositoryResult <- discoverRepository systemGit (configRepo config)
  case repositoryResult of
    Left problem -> pure (Left (CliUserFailure (Text.pack (show problem))))
    Right repository -> do
      indexPath <- prepareIndexPath repository
      case indexPath of
        Left problem -> pure (Left (CliUserFailure problem))
        Right database -> do
          result <- amendCurrentAdrCommand repository (amendRequestActor request) (amendRequestAdr request)
            (amendRequestExpectedState request) (amendRequestChangeSummary request)
            (amendRequestTitle request) (amendRequestSummary request) (amendRequestBody request)
            (ProvenanceInputs (amendRequestInputDigest request) (amendRequestPromptDigest request) (amendRequestContextDigest request))
          case result of
            Left problem -> pure (Left (transactionFailure problem))
            Right amendResult -> Right . (amendResult,) <$> indexCommitted database repository (amendCommitOid amendResult)

runProductionScope :: CliConfig -> ScopeRequest -> IO (Either CliFailure (ScopeChangeResult, PostCommitIndexResult))
runProductionScope config request = do
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
              result <- changeScopeCommand repository (repositorySnapshotManagedPaths snapshot)
                (scopeRequestActor request) (scopeRequestAdr request) (scopeRequestExpectedState request)
                (scopeRequestReason request) (scopeRequestChange request)
                (ProvenanceInputs (scopeRequestInputDigest request) (scopeRequestPromptDigest request) (scopeRequestContextDigest request))
              case result of
                Left problem -> pure (Left (transactionFailure problem))
                Right scopeResult -> Right . (scopeResult,) <$> indexCommitted database repository (scopeChangeCommitOid scopeResult)

runProductionDomain :: CliConfig -> DomainRequest -> IO (Either CliFailure (DomainChangeResult, PostCommitIndexResult))
runProductionDomain config request = do
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
              result <- changeDomainCommand repository (repositorySnapshotManagedPaths snapshot)
                (domainRequestActor request) (domainRequestAdr request) (domainRequestExpectedState request)
                (domainRequestReason request) (domainRequestChange request)
                (ProvenanceInputs (domainRequestInputDigest request) (domainRequestPromptDigest request) (domainRequestContextDigest request))
              case result of
                Left problem -> pure (Left (transactionFailure problem))
                Right domainResult -> Right . (domainResult,) <$> indexCommitted database repository (domainChangeCommitOid domainResult)

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

data AmendRequest = AmendRequest
  { amendRequestAdr :: AdrId
  , amendRequestExpectedState :: Maybe StateToken
  , amendRequestChangeSummary :: Text
  , amendRequestTitle :: Text
  , amendRequestSummary :: Text
  , amendRequestBody :: Text
  , amendRequestActor :: Actor
  , amendRequestInputDigest :: Maybe Digest
  , amendRequestPromptDigest :: Maybe Digest
  , amendRequestContextDigest :: Maybe Digest
  }
  deriving (Eq, Show)

data ScopeRequest = ScopeRequest
  { scopeRequestAdr :: AdrId
  , scopeRequestExpectedState :: Maybe StateToken
  , scopeRequestReason :: Text
  , scopeRequestChange :: ScopeChangeRequest
  , scopeRequestActor :: Actor
  , scopeRequestInputDigest :: Maybe Digest
  , scopeRequestPromptDigest :: Maybe Digest
  , scopeRequestContextDigest :: Maybe Digest
  }
  deriving (Eq, Show)

data DomainRequest = DomainRequest
  { domainRequestAdr :: AdrId
  , domainRequestExpectedState :: Maybe StateToken
  , domainRequestReason :: Text
  , domainRequestChange :: DomainChangeRequest
  , domainRequestActor :: Actor
  , domainRequestInputDigest :: Maybe Digest
  , domainRequestPromptDigest :: Maybe Digest
  , domainRequestContextDigest :: Maybe Digest
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

data StructuredAmend = StructuredAmend
  { structuredAmendTitle :: Maybe Text
  , structuredAmendSummary :: Maybe Text
  , structuredAmendBody :: Maybe Text
  , structuredAmendChangeSummary :: Maybe Text
  , structuredAmendActor :: Maybe Text
  , structuredAmendModel :: Maybe Text
  , structuredAmendInputDigest :: Maybe Text
  , structuredAmendPromptDigest :: Maybe Text
  , structuredAmendContextDigest :: Maybe Text
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

materializeAmend :: AmendCommand -> IO (Either Text AmendRequest)
materializeAmend command = do
  contentResult <- readAmendContent command
  case contentResult of
    Left problem -> pure (Left problem)
    Right (maybeStructured, body) -> do
      environmentActor <- lookupEnv "ADRAI_ACTOR"
      promptFromFile <- readDigestFile "prompt" (amendPromptFile command)
      contextFromFile <- readDigestFile "context" (amendContextFile command)
      pure $ do
        promptFileDigest <- promptFromFile
        contextFileDigest <- contextFromFile
        let structured = fromMaybe emptyStructuredAmend maybeStructured
            title = fromMaybe "" (amendTitle command <|> structuredAmendTitle structured)
            summary = fromMaybe "" (amendSummary command <|> structuredAmendSummary structured)
            changeSummary = amendChangeSummary command <|> structuredAmendChangeSummary structured
            actorSpec = amendActorSpec command <|> structuredAmendActor structured <|> Text.pack <$> environmentActor
            model = amendModel command <|> structuredAmendModel structured
            inputDigest = amendInputDigest command <|> structuredAmendInputDigest structured
            promptDigest = amendPromptDigest command <|> structuredAmendPromptDigest structured
            contextDigest = amendContextDigest command <|> structuredAmendContextDigest structured
        adr <- first (Text.pack . show) (mkAdrId (amendAdrSpec command))
        expected <- traverse (first (Text.pack . show) . Format.parseStateToken) (amendExpectedState command)
        rationale <- maybe (Left "amend requires --change-summary or structured change_summary") Right changeSummary
        if Text.null (Text.strip rationale)
          then Left "amend change summary must be nonblank"
          else Right ()
        actorText <- maybe (Left "amend requires --actor, structured actor, or ADRAI_ACTOR") Right actorSpec
        actor <- parseActor actorText model
        input <- maybe (Right (Just (sha256Digest (TextEncoding.encodeUtf8 body)))) (fmap Just . parseDigest) inputDigest
        prompt <- resolveDigest "prompt" promptDigest promptFileDigest
        context <- resolveDigest "context" contextDigest contextFileDigest
        Right AmendRequest
          { amendRequestAdr = adr
          , amendRequestExpectedState = expected
          , amendRequestChangeSummary = rationale
          , amendRequestTitle = title
          , amendRequestSummary = summary
          , amendRequestBody = body
          , amendRequestActor = actor
          , amendRequestInputDigest = input
          , amendRequestPromptDigest = prompt
          , amendRequestContextDigest = context
           }

materializeScope :: ScopeCommand -> IO (Either Text ScopeRequest)
materializeScope command = do
  environmentActor <- lookupEnv "ADRAI_ACTOR"
  promptFromFile <- readDigestFile "prompt" (scopePromptFile command)
  contextFromFile <- readDigestFile "context" (scopeContextFile command)
  pure $ do
    promptFileDigest <- promptFromFile
    contextFileDigest <- contextFromFile
    adr <- first (Text.pack . show) (mkAdrId (scopeAdrSpec command))
    expected <- traverse (first (Text.pack . show) . Format.parseStateToken) (scopeExpectedState command)
    reason <- maybe (Left "scope requires --reason") Right (scopeReason command)
    if Text.null (Text.strip reason)
      then Left "scope reason must be nonblank"
      else Right ()
    change <- case (scopeSets command, scopeAdds command, scopeRemoves command) of
      (sets@(_ : _), [], []) -> ScopeReviewedSet <$> traverse parsePattern sets
      (_ : _, _, _) -> Left "scope --set cannot be combined with --add or --remove"
      ([], adds, removes) -> ScopeDelta <$> traverse parsePattern adds <*> traverse parsePattern removes
    actorText <- maybe (Left "scope requires --actor or ADRAI_ACTOR") Right (scopeActorSpec command <|> Text.pack <$> environmentActor)
    actor <- parseActor actorText (scopeModel command)
    input <- traverse parseDigest (scopeInputDigest command)
    prompt <- resolveDigest "prompt" (scopePromptDigest command) promptFileDigest
    context <- resolveDigest "context" (scopeContextDigest command) contextFileDigest
    Right ScopeRequest
      { scopeRequestAdr = adr
      , scopeRequestExpectedState = expected
      , scopeRequestReason = reason
      , scopeRequestChange = change
      , scopeRequestActor = actor
      , scopeRequestInputDigest = input
      , scopeRequestPromptDigest = prompt
      , scopeRequestContextDigest = context
      }
  where
    parsePattern = first scopePatternErrorText . mkScopePattern

materializeDomain :: DomainCommand -> IO (Either Text DomainRequest)
materializeDomain command = do
  environmentActor <- lookupEnv "ADRAI_ACTOR"
  promptFromFile <- readDigestFile "prompt" (domainPromptFile command)
  contextFromFile <- readDigestFile "context" (domainContextFile command)
  pure $ do
    promptFileDigest <- promptFromFile
    contextFileDigest <- contextFromFile
    adr <- first (Text.pack . show) (mkAdrId (domainAdrSpec command))
    expected <- traverse (first (Text.pack . show) . Format.parseStateToken) (domainExpectedState command)
    reason <- maybe (Left "domain requires --reason") Right (domainReason command)
    if Text.null (Text.strip reason) then Left "domain reason must be nonblank" else Right ()
    change <- selectDomainRequest command
    actorText <- maybe (Left "domain requires --actor or ADRAI_ACTOR") Right (domainActorSpec command <|> Text.pack <$> environmentActor)
    actor <- parseActor actorText (domainModel command)
    input <- traverse parseDigest (domainInputDigest command)
    prompt <- resolveDigest "prompt" (domainPromptDigest command) promptFileDigest
    context <- resolveDigest "context" (domainContextDigest command) contextFileDigest
    Right DomainRequest
      { domainRequestAdr = adr, domainRequestExpectedState = expected, domainRequestReason = reason
      , domainRequestChange = change, domainRequestActor = actor, domainRequestInputDigest = input
      , domainRequestPromptDigest = prompt, domainRequestContextDigest = context }

selectDomainRequest :: DomainCommand -> Either Text DomainChangeRequest
selectDomainRequest command =
  case (domainAdds command, domainRemoves command, domainRefines command, domainSets command, domainClear command) of
    ([], [], [], [], False) -> Left "domain requires --add, --remove, --refine, --set, or --clear"
    (adds, removes, [], [], False) -> DomainDelta <$> traverse parseDomain adds <*> traverse parseDomain removes
    ([], [], refinements@(_ : _), [], False) -> DomainRefine <$> traverse parseRefinement refinements
    ([], [], [], sets@(_ : _), False) -> DomainReviewedSet <$> traverse parseDomain sets
    ([], [], [], [], True) -> Right (DomainReviewedSet [])
    _ -> Left "domain modes --add/--remove, --refine, --set, and --clear are mutually exclusive"
  where
    parseDomain = first domainErrorText . mkDomain
    parseRefinement = first domainErrorText . parseDomainRefinement

emptyStructured :: StructuredCreate
emptyStructured = StructuredCreate Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

emptyStructuredAmend :: StructuredAmend
emptyStructuredAmend = StructuredAmend Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

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

readAmendContent :: AmendCommand -> IO (Either Text (Maybe StructuredAmend, Text))
readAmendContent command =
  case amendContentSources command of
    [] -> pure (Left "amend requires exactly one of --body, --body-file, --stdin, or --input-json")
    [_first, _second, _third, _fourth] -> pure (Left "amend accepts exactly one content source")
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
            structured <- parseStructuredAmend input
            body <- maybe (Left "structured amend input requires string field body") Right (structuredAmendBody structured)
            Right (Just structured, body)
    _ -> pure (Left "amend accepts exactly one content source")

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
      <$> optionalStringFor "create" "title" object
      <*> optionalStringFor "create" "summary" object
      <*> optionalStringFor "create" "body" object
      <*> optionalStrings "domains" object
      <*> optionalStrings "applies_to" object
      <*> optionalStringFor "create" "actor" object
      <*> optionalStringFor "create" "model" object
      <*> optionalStringFor "create" "input_digest" object
      <*> optionalStringFor "create" "prompt_digest" object
      <*> optionalStringFor "create" "context_digest" object
    else Left ("structured create input has unknown fields: " <> Text.intercalate ", " unknown)

parseStructuredAmend :: Text -> Either Text StructuredAmend
parseStructuredAmend input = do
  value <- first (Text.pack . show) (Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 input))
  object <- case value of
    Aeson.Object fields -> Right fields
    _ -> Left "structured amend input must be a JSON object"
  let allowed = ["title", "summary", "body", "change_summary", "actor", "model", "input_digest", "prompt_digest", "context_digest"]
      unknown = filter (`notElem` allowed) (map Aeson.Key.toText (KeyMap.keys object))
  if null unknown
    then StructuredAmend
      <$> optionalStringFor "amend" "title" object
      <*> optionalStringFor "amend" "summary" object
      <*> optionalStringFor "amend" "body" object
      <*> optionalStringFor "amend" "change_summary" object
      <*> optionalStringFor "amend" "actor" object
      <*> optionalStringFor "amend" "model" object
      <*> optionalStringFor "amend" "input_digest" object
      <*> optionalStringFor "amend" "prompt_digest" object
      <*> optionalStringFor "amend" "context_digest" object
    else Left ("structured amend input has unknown fields: " <> Text.intercalate ", " unknown)

optionalStringFor :: Text -> Text -> Aeson.Object -> Either Text (Maybe Text)
optionalStringFor commandName name object =
  case KeyMap.lookup (Aeson.Key.fromText name) object of
    Nothing -> Right Nothing
    Just (Aeson.String value) -> Right (Just value)
    Just _ -> Left ("structured " <> commandName <> " field " <> name <> " must be a string")

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

renderAmendSuccess :: AmendCommand -> AmendResult -> PostCommitIndexResult -> IO ExitCode
renderAmendSuccess command result indexResult =
  emitRendered (renderAmendOutcome result indexResult (amendJson command))

renderScopeSuccess :: ScopeCommand -> ScopeChangeResult -> PostCommitIndexResult -> IO ExitCode
renderScopeSuccess command result indexResult =
  emitRendered (renderScopeOutcome result indexResult (scopeJson command))

renderDomainSuccess :: DomainCommand -> DomainChangeResult -> PostCommitIndexResult -> IO ExitCode
renderDomainSuccess command result indexResult =
  emitRendered (renderDomainOutcome result indexResult (domainJson command))

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

renderAmendOutcome :: AmendResult -> PostCommitIndexResult -> Bool -> CliRendered
renderAmendOutcome result indexResult jsonOutput =
  successOutcome jsonOutput (Text.pack (amendOperationId result)) (amendCommitOid result) identifiers indexResult jsonFields
  where
    identifiers =
      [ "adr=" <> adrIdText (amendAdrId result)
      , "record=" <> recordIdText (amendRecordId result)
      , "amends=" <> recordIdText (amendAmends result)
      , "connection=" <> connectionIdText (amendConnectionId result)
      ]
    jsonFields =
      mutationJsonFields (amendOperationId result) (amendCommitOid result) (amendCreatedPaths result) (amendIndexUpdated result) indexResult
        <> [ ("adr", JsonString (adrIdText (amendAdrId result)))
           , ("record", JsonString (recordIdText (amendRecordId result)))
           , ("amends", JsonString (recordIdText (amendAmends result)))
           , ("connection", JsonString (connectionIdText (amendConnectionId result)))
           ]

renderScopeOutcome :: ScopeChangeResult -> PostCommitIndexResult -> Bool -> CliRendered
renderScopeOutcome result indexResult jsonOutput =
  successOutcome jsonOutput (Text.pack (scopeChangeOperationId result)) (scopeChangeCommitOid result) identifiers indexResult jsonFields
  where
    identifiers =
      [ "adr=" <> adrIdText (scopeChangeAdrId result)
      , "scope=" <> connectionIdText (scopeChangeConnectionId result)
      ]
    jsonFields =
      mutationJsonFields (scopeChangeOperationId result) (scopeChangeCommitOid result) (scopeChangeCreatedPaths result) (scopeChangeIndexUpdated result) indexResult
        <> [ ("adr", JsonString (adrIdText (scopeChangeAdrId result)))
           , ("scope", JsonString (connectionIdText (scopeChangeConnectionId result)))
           , ("scope_parents", JsonArray (map (JsonString . connectionIdText) (scopeChangeParents result)))
           , ("mode", JsonString (scopeChangeMode result))
           , ("applies_to", JsonArray (map (JsonString . scopePatternText) (scopeChangeEffective result)))
           ]

renderDomainOutcome :: DomainChangeResult -> PostCommitIndexResult -> Bool -> CliRendered
renderDomainOutcome result indexResult jsonOutput =
  successOutcome jsonOutput (Text.pack (domainChangeOperationId result)) (domainChangeCommitOid result) identifiers indexResult jsonFields
  where
    identifiers =
      [ "adr=" <> adrIdText (domainChangeAdrId result)
      , "domain=" <> connectionIdText (domainChangeConnectionId result)
      ]
    jsonFields =
      mutationJsonFields (domainChangeOperationId result) (domainChangeCommitOid result) (domainChangeCreatedPaths result) (domainChangeIndexUpdated result) indexResult
        <> [ ("adr", JsonString (adrIdText (domainChangeAdrId result)))
           , ("domain", JsonString (connectionIdText (domainChangeConnectionId result)))
           , ("domain_parents", JsonArray (map (JsonString . connectionIdText) (domainChangeParents result)))
           , ("mode", JsonString (domainChangeMode result))
           , ("domains", JsonArray (map (JsonString . domainText) (domainChangeEffective result)))
           , ("added", JsonArray (map (JsonString . domainText) (domainChangeAdded result)))
           , ("removed", JsonArray (map (JsonString . domainText) (domainChangeRemoved result)))
           , ("refinements", JsonArray (map (JsonString . domainRefinementText) (domainChangeRefinements result)))
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
    [adr, record, amends, connection] ->
      Text.unlines
        [ "Committed " <> operation <> " as " <> gitOidText commit
        , adr <> "  " <> record <> "  " <> amends <> "  " <> connection
        , indexLine
        ]
    [adr, scope] ->
      Text.unlines
        [ "Committed " <> operation <> " as " <> gitOidText commit
        , adr <> "  " <> scope
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
      || "stale ADR state:" `Text.isInfixOf` message
      || "scope target ADR is conflicted" `Text.isInfixOf` message
      || "scope target ADR has no unambiguous current scope" `Text.isInfixOf` message
      || "scope target ADR is not active" `Text.isInfixOf` message
      || "domain target ADR is conflicted" `Text.isInfixOf` message
      || "domain target ADR has no unambiguous current domain" `Text.isInfixOf` message
      || "domain target ADR is not active" `Text.isInfixOf` message
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
