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
    CompileCommand (..),
    DoctorCommand (..),
    InitCommand (..),
    CreateCommand (..),
    AmendCommand (..),
    ObsoleteCommand (..),
    ReactivateCommand (..),
    ScopeCommand (..),
    DomainCommand (..),
    ContentSource (..),
    CreateRequest (..),
    AmendRequest (..),
    ObsoleteCliRequest (..),
    ReactivateCliRequest (..),
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
    materializeObsolete,
    materializeReactivate,
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
    renderObsoleteOutcome,
    renderReactivateOutcome,
    renderShowOutcome,
    renderCompareOutcome,
    renderHistoryOutcome,
    renderSearchOutcome,
    renderRelevantOutcome,
    renderCompileOutcome,
    renderDoctorOutcome,
    capturePostCommitIndex,
    renderFailureOutcome,
    emitRenderedToHandles,
    run,
  )
where

import Adrai.CliTypes
  ( CompileResult (..),
    compileResultValue,
    DoctorOutput (..),
    DoctorIssue (..),
    DoctorCounts (..),
    doctorOutputJson,
    ShowCommand (..),
    HistoryCommand (..),
    SearchCommand (..),
    searchCommandRequest,
    RelevantCommand (..),
    relevantCommandRequest,
    CompareCommand (..),
  )
import Adrai.History
  ( ActorSelector (..),
    HistoryOptions (..),
    HistoryOrder (..),
    HistoryProjection,
    historyProjectionJson,
    renderHistoryProjection,
  )
import Adrai.Retrieval (RetrievalMode (FtsRetrieval, HybridRetrieval, VectorRetrieval))
import Adrai.Types (ViewMode (CollapsedView, ExplodedView))
import Adrai.Domain (Domain, DomainRefinement, canonicalDomains, domainErrorText, domainText, domainRefinementText, mkDomain, parseDomainRefinement)
import Adrai.Git (GitOid (..), Repository, RevisionSpec (RevisionSpec), discoverRepository, gitOidText, isShallowRepository, repositoryWorktreeRoot, systemGit)
import Adrai.Identity (sortableAdrId, sortableRecordId)
import qualified Adrai.Format as Format
import Adrai.Format.Json (JsonValue (..), renderCanonicalJson)
import Adrai.Provenance (sha256Digest)
import Adrai.Repository (resolvedCommitOid, resolveRepositoryRevision, repositorySnapshot, repositorySnapshotManagedPaths)
import Adrai.Scope (ScopePattern, mkScopePattern, scopePatternErrorText, scopePatternText)
import Adrai.Explorer.Interactive (interactiveSession)
import Adrai.Service.Mutation (AmendResult (..), CreateResult (..), DomainChangeRequest (..), DomainChangeResult (..), InitResult (..), ObsoleteRequest (..), ObsoleteResult (..), ReactivateRequest (..), ReactivateResult (..), ScopeChangeRequest (..), ScopeChangeResult (..), amendCurrentAdrCommand, changeDomainCommand, changeScopeCommand, createAdrCommand, initCommand, obsoleteCommand, reactivateCommand)
import Adrai.Service.Query
  ( CompareRequest (..),
    HistoryRequest (..),
    SearchServiceRequest (..),
    ShowRequest (..),
    ShowResult (..),
    compareFailureText,
    historyFailureText,
    runCompare,
    runHistory,
    runSearch,
    runRelevantQuery,
    runShow,
    searchFailureIsConflict,
    searchFailureText,
    relevantFailureText,
    showFailureIsConflict,
    showFailureText,
  )
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
     actorId,
     actorKind,
     adrIdText,
     connectionIdText,
     mkActor,
     mkAdrId,
    mkDigest,
    recordIdText,
    repoPathText,
  )
import Adrai.Query
  ( CompareProjection,
    SearchProjection,
    RelevantRequest,
    RelevantProjection,
    collapsedProjectionJson,
    compareProjectionJson,
    explodedProjectionJson,
    renderCollapsedProjection,
    renderCompareProjection,
    renderExplodedProjection,
    renderSearchProjection,
    searchProjectionJson,
    renderRelevantProjection,
    relevantProjectionJson,
  )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Read as TextRead
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, lookupEnv)
import System.FilePath ((</>))
import System.IO (Handle, hGetContents, stderr, stdin, stdout)
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Control.Exception (SomeAsyncException, SomeException, bracket, displayException, fromException, throwIO, try)
import Database.SQLite.Simple (close, open, query, query_)
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
    eitherReader,
    value,
    showDefault,
    showDefaultWith,
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

data CompileCommand = CompileCommand
  { compileAt :: Text
  , compileJson :: Bool
  }
  deriving (Eq, Show)

data DoctorCommand = DoctorCommand
  { doctorAt :: Text
  , doctorJson :: Bool
  }
  deriving (Eq, Show)

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

data ObsoleteCommand = ObsoleteCommand
  { obsoleteAdrSpec :: Text
  , obsoleteReasonSpec :: Text
  , obsoleteReplacementSpec :: Maybe Text
  , obsoleteExpectedStateSpec :: Maybe Text
  , obsoleteResolveSpec :: Bool
  , obsoleteActorSpec :: Maybe Text
  , obsoleteModelSpec :: Maybe Text
  , obsoleteInputDigestSpec :: Maybe Text
  , obsoletePromptDigestSpec :: Maybe Text
  , obsoleteContextDigestSpec :: Maybe Text
  , obsoletePromptFileSpec :: Maybe FilePath
  , obsoleteContextFileSpec :: Maybe FilePath
  , obsoleteJson :: Bool
  }
  deriving (Eq, Show)

data ReactivateCommand = ReactivateCommand
  { reactivateAdrSpec :: Text
  , reactivateReasonSpec :: Text
  , reactivateExpectedStateSpec :: Maybe Text
  , reactivateResolveSpec :: Bool
  , reactivateActorSpec :: Maybe Text
  , reactivateModelSpec :: Maybe Text
  , reactivateInputDigestSpec :: Maybe Text
  , reactivatePromptDigestSpec :: Maybe Text
  , reactivateContextDigestSpec :: Maybe Text
  , reactivatePromptFileSpec :: Maybe FilePath
  , reactivateContextFileSpec :: Maybe FilePath
  , reactivateJson :: Bool
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
  = CmdCompile CompileCommand
  | CmdDoctor DoctorCommand
  | CmdShow ShowCommand
  | CmdHistory HistoryCommand
  | CmdSearch SearchCommand
  | CmdRelevant RelevantCommand
  | CmdCompare CompareCommand
  | CmdInit InitCommand
  | CmdCreate CreateCommand
  | CmdAmend AmendCommand
  | CmdObsolete ObsoleteCommand
  | CmdReactivate ReactivateCommand
  | CmdScope ScopeCommand
  | CmdDomain DomainCommand
  | CmdExplore
  deriving (Eq, Show)

-- | Top-level CLI parser type alias.
type CliParser = Parser CliInvocation

-- | Top-level CLI parser combining all subcommands.
parser :: CliParser
parser =
  CliInvocation <$> globalConfigParser <*> subparser
    ( command "compile" (info (CmdCompile <$> compileParser) (progDesc "compile the repository"))
     <> command "doctor" (info (CmdDoctor <$> doctorParser) (progDesc "diagnose the database"))
     <> command "show" (info (CmdShow <$> showParser) (progDesc "show a single ADR"))
     <> command "history" (info (CmdHistory <$> historyParser) (progDesc "show operation history"))
     <> command "search" (info (CmdSearch <$> searchParser) (progDesc "search ADRs"))
     <> command "relevant" (info (CmdRelevant <$> relevantParser) (progDesc "find relevant ADRs"))
     <> command "compare" (info (CmdCompare <$> compareParser) (progDesc "compare revisions"))
     <> command "init" (info (CmdInit <$> initParser) (progDesc "initialize an ADRAI repository"))
      <> command "create" (info (CmdCreate <$> createParser) (progDesc "create an ADR"))
       <> command "amend" (info (CmdAmend <$> amendParser) (progDesc "amend an ADR"))
       <> command "obsolete" (info (CmdObsolete <$> obsoleteParser) (progDesc "mark an ADR obsolete"))
       <> command "reactivate" (info (CmdReactivate <$> reactivateParser) (progDesc "reactivate an ADR"))
       <> command "scope" (info (CmdScope <$> scopeParser) (progDesc "change an ADR scope"))
       <> command "domain" (info (CmdDomain <$> domainParser) (progDesc "change an ADR domain"))
       <> command "explore" (info (pure CmdExplore) (progDesc "open the terminal explorer"))
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

compileParser :: Parser CompileCommand
compileParser =
  CompileCommand
    <$> strOption (long "at" <> metavar "REVISION" <> value "HEAD" <> showDefault <> help "revision to compile (default HEAD)")
    <*> switch (long "json" <> help "output JSON")

doctorParser :: Parser DoctorCommand
doctorParser =
  DoctorCommand
    <$> strOption (long "at" <> metavar "REVISION" <> value "HEAD" <> showDefault <> help "revision to diagnose (default HEAD)")
    <*> switch (long "json" <> help "output JSON")

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

obsoleteParser :: Parser ObsoleteCommand
obsoleteParser =
  ObsoleteCommand
    <$> strArgument (metavar "ADR" <> help "ADR identifier to obsolete")
    <*> strOption (long "reason" <> metavar "TEXT" <> help "nonblank obsolete rationale")
    <*> optional (strOption (long "replacement" <> metavar "ADR" <> help "active replacement ADR"))
    <*> optional (strOption (long "expect" <> metavar "STATE_TOKEN" <> help "expected current ADR state token"))
    <*> switch (long "resolve" <> help "resolve a status-only conflict")
    <*> optional (strOption (long "actor" <> metavar "ACTOR" <> help "actor as kind:identifier"))
    <*> optional (strOption (long "model" <> metavar "MODEL" <> help "actor model"))
    <*> optional (strOption (long "input-digest" <> metavar "DIGEST" <> help "SHA-256 input digest"))
    <*> optional (strOption (long "prompt-digest" <> metavar "DIGEST" <> help "SHA-256 prompt digest"))
    <*> optional (strOption (long "context-digest" <> metavar "DIGEST" <> help "SHA-256 context digest"))
    <*> optional (strOption (long "prompt-file" <> metavar "PATH" <> help "prompt source path"))
    <*> optional (strOption (long "context-file" <> metavar "PATH" <> help "context source path"))
    <*> switch (long "json" <> help "output JSON")

reactivateParser :: Parser ReactivateCommand
reactivateParser =
  ReactivateCommand
    <$> strArgument (metavar "ADR" <> help "ADR identifier to reactivate")
    <*> strOption (long "reason" <> metavar "TEXT" <> help "nonblank reactivate rationale")
    <*> optional (strOption (long "expect" <> metavar "STATE_TOKEN" <> help "expected current ADR state token"))
    <*> switch (long "resolve" <> help "resolve a status-only conflict")
    <*> optional (strOption (long "actor" <> metavar "ACTOR" <> help "actor as kind:identifier"))
    <*> optional (strOption (long "model" <> metavar "MODEL" <> help "actor model"))
    <*> optional (strOption (long "input-digest" <> metavar "DIGEST" <> help "SHA-256 input digest"))
    <*> optional (strOption (long "prompt-digest" <> metavar "DIGEST" <> help "SHA-256 prompt digest"))
    <*> optional (strOption (long "context-digest" <> metavar "DIGEST" <> help "SHA-256 context digest"))
    <*> optional (strOption (long "prompt-file" <> metavar "PATH" <> help "prompt source path"))
    <*> optional (strOption (long "context-file" <> metavar "PATH" <> help "context source path"))
    <*> switch (long "json" <> help "output JSON")

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
    <*> option (Opt.eitherReader parseView) (long "view" <> metavar "collapsed|exploded" <> value CollapsedView <> showDefaultWith (const "collapsed") <> help "show collapsed (default) or exploded view")
    <*> strOption (long "at" <> metavar "REVISION" <> value "HEAD" <> showDefault <> help "immutable revision to query (default: HEAD)")
    <*> switch (long "json" <> help "output JSON")
    <*> switch (long "raw" <> help "include raw semantic data")
  where
    parseView value = case value of
      "collapsed" -> Right CollapsedView
      "exploded" -> Right ExplodedView
      _ -> Left "--view must be collapsed or exploded"

-- | History command parser.
historyParser :: Parser HistoryCommand
historyParser =
  HistoryCommand
    <$> optional (strArgument (metavar "ADR_ID" <> help "optional ADR ID or prefix"))
    <*> strOption (long "at" <> metavar "REVISION" <> value "HEAD" <> showDefault <> help "immutable revision to query (default: HEAD)")
    <*> option auto (long "limit" <> value 20 <> showDefault <> help "max results (default 20)")
    <*> optional (strOption (long "actor" <> metavar "KIND:IDENTIFIER" <> help "filter by actor"))
    <*> optional (option auto (long "since" <> metavar "MILLISECONDS" <> help "show entries at or after this Unix timestamp in milliseconds"))
    <*> optional (option auto (long "until" <> metavar "MILLISECONDS" <> help "show entries at or before this Unix timestamp in milliseconds"))
    <*> switch (long "reverse" <> help "reverse the order")
    <*> switch (long "json" <> help "output JSON")

-- | Search command parser.
searchParser :: Parser SearchCommand
searchParser =
  SearchCommand
    <$> (strArgument (metavar "QUERY" <> help "optional search query") <|> pure "")
    <*> option (eitherReader parseMode) (long "mode" <> metavar "fts|vector|hybrid" <> value HybridRetrieval <> showDefaultWith retrievalModeText <> help "retrieval mode")
    <*> option (eitherReader parseView) (long "view" <> metavar "collapsed|exploded" <> value CollapsedView <> showDefaultWith searchViewText <> help "result view")
    <*> optional (strOption (long "file" <> metavar "PATH" <> help "filter by file path"))
    <*> many (strOption (long "domain" <> metavar "DOMAIN" <> help "domain filter (repeatable)"))
    <*> optional (strOption (long "actor" <> metavar "KIND:IDENTIFIER" <> help "filter by actor"))
    <*> optional (option auto (long "since" <> metavar "MILLISECONDS" <> help "entries at or after this Unix timestamp in milliseconds"))
    <*> optional (option auto (long "until" <> metavar "MILLISECONDS" <> help "entries at or before this Unix timestamp in milliseconds"))
    <*> strOption (long "at" <> metavar "REVISION" <> value "HEAD" <> showDefault <> help "immutable revision to query")
    <*> switch (long "include-obsolete" <> help "include obsolete ADRs")
    <*> option auto (long "limit" <> value 10 <> showDefault <> help "max results (default 10)")
    <*> switch (long "json" <> help "output JSON")
  where
    parseMode value = case value of
      "fts" -> Right FtsRetrieval
      "vector" -> Right VectorRetrieval
      "hybrid" -> Right HybridRetrieval
      _ -> Left "--mode must be fts, vector, or hybrid"
    parseView value = case value of
      "collapsed" -> Right CollapsedView
      "exploded" -> Right ExplodedView
      _ -> Left "--view must be collapsed or exploded"
    retrievalModeText FtsRetrieval = "fts"
    retrievalModeText VectorRetrieval = "vector"
    retrievalModeText HybridRetrieval = "hybrid"
    searchViewText CollapsedView = "collapsed"
    searchViewText ExplodedView = "exploded"

-- | Relevant command parser.
relevantParser :: Parser RelevantCommand
relevantParser =
  RelevantCommand
    <$> strArgument (metavar "FILE" <> help "file path to find relevant ADRs for")
    <*> optional (strOption (long "at" <> metavar "REVISION" <> help "immutable revision containing FILE (default HEAD)"))
    <*> switch (long "worktree" <> help "read FILE from the worktree using HEAD as context")
    <*> switch (long "include-obsolete" <> help "include obsolete ADRs")
    <*> option auto (long "limit" <> value 10 <> showDefault <> help "max results (default 10)")
    <*> switch (long "json" <> help "output JSON")

-- | Compare command parser.
compareParser :: Parser CompareCommand
compareParser =
  CompareCommand
    <$> strArgument (metavar "FROM" <> help "revision to compare from")
    <*> (strArgument (metavar "TO" <> help "revision to compare to (default HEAD)") <|> pure "HEAD")
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
dispatchWith dependencies (CliInvocation config (CmdObsolete command)) = do
  requestResult <- cliMaterializeObsolete dependencies command
  case requestResult of
    Left problem -> renderFailure (CliUserFailure problem)
    Right request -> do
      result <- cliRunObsolete dependencies config (obsoleteAdrSpec command) request
      case result of
        Left failure -> renderFailure failure
        Right (obsoleteResult, indexResult) -> renderObsoleteSuccess command obsoleteResult indexResult
dispatchWith dependencies (CliInvocation config (CmdReactivate command)) = do
  requestResult <- cliMaterializeReactivate dependencies command
  case requestResult of
    Left problem -> renderFailure (CliUserFailure problem)
    Right request -> do
      result <- cliRunReactivate dependencies config (reactivateAdrSpec command) request
      case result of
        Left failure -> renderFailure failure
        Right (reactivateResult, indexResult) -> renderReactivateSuccess command reactivateResult indexResult
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
dispatchWith dependencies (CliInvocation config (CmdCompile command)) = do
  result <- cliRunCompile dependencies config command
  case result of
    Left failure -> renderFailure failure
    Right compiled -> emitRendered (renderCompileOutcome command compiled)
dispatchWith dependencies (CliInvocation config (CmdDoctor command)) = do
  result <- cliRunDoctor dependencies config command
  case result of
    Left failure -> renderFailure failure
    Right output -> emitRendered (renderDoctorOutcome command output)
dispatchWith dependencies (CliInvocation config (CmdShow command)) = do
  if showRaw command && showView command /= ExplodedView
    then renderFailure (CliUserFailure "--raw requires --view exploded")
    else do
      result <- cliRunShow dependencies config command
      case result of
        Left failure -> renderFailure failure
        Right projection -> renderShowSuccess command projection
dispatchWith dependencies (CliInvocation config (CmdHistory command)) = do
  result <- cliRunHistory dependencies config command
  case result of
    Left failure -> renderFailure failure
    Right projection -> emitRendered (renderHistoryOutcome command projection)
dispatchWith dependencies (CliInvocation config (CmdSearch command)) =
  case searchCommandRequest command of
    Left problem -> renderFailure (CliUserFailure problem)
    Right request -> do
      result <- cliRunSearch dependencies config (SearchServiceRequest (searchAt command) request)
      case result of
        Left failure -> renderFailure failure
        Right projection -> emitRendered (renderSearchOutcome command projection)
dispatchWith dependencies (CliInvocation config (CmdRelevant command)) =
  case relevantCommandRequest command of
    Left problem -> renderFailure (CliUserFailure problem)
    Right request -> do
      result <- cliRunRelevant dependencies config request
      case result of
        Left failure -> renderFailure failure
        Right projection -> emitRendered (renderRelevantOutcome command projection)
dispatchWith dependencies (CliInvocation config (CmdCompare command)) = do
  result <- cliRunCompare dependencies config command
  case result of
    Left failure -> renderFailure failure
    Right projection -> emitRendered (renderCompareOutcome command projection)
dispatchWith _ (CliInvocation config CmdExplore) = do
  actorResult <- pure (mkActor HumanActor "terminal-explorer" Nothing)
  case actorResult of
    Left problem -> renderFailure (CliUserFailure (Text.pack (show problem)))
    Right actor -> do
      outcome <- try (interactiveSession (configRepo config) actor) :: IO (Either SomeException (Either Text ()))
      case outcome of
        Left problem ->
          case fromException problem of
            Just cancellation -> throwIO (cancellation :: SomeAsyncException)
            Nothing -> renderFailure (CliUserFailure (Text.pack (displayException problem)))
        Right (Left problem) -> renderFailure (CliUserFailure problem)
        Right (Right ()) -> pure ExitSuccess

data CliDispatchDependencies = CliDispatchDependencies
  { cliRunCompile :: ~(CliConfig -> CompileCommand -> IO (Either CliFailure CompileResult))
  , cliRunDoctor :: ~(CliConfig -> DoctorCommand -> IO (Either CliFailure DoctorOutput))
  , cliRunShow :: ~(CliConfig -> ShowCommand -> IO (Either CliFailure ShowResult))
  , cliRunCompare :: ~(CliConfig -> CompareCommand -> IO (Either CliFailure CompareProjection))
  , cliRunHistory :: ~(CliConfig -> HistoryCommand -> IO (Either CliFailure HistoryProjection))
  , cliRunSearch :: ~(CliConfig -> SearchServiceRequest -> IO (Either CliFailure SearchProjection))
  , cliRunRelevant :: ~(CliConfig -> RelevantRequest -> IO (Either CliFailure RelevantProjection))
  , cliMaterializeCreate :: CreateCommand -> IO (Either Text CreateRequest)
  , cliMaterializeAmend :: AmendCommand -> IO (Either Text AmendRequest)
  , cliMaterializeObsolete :: ~(ObsoleteCommand -> IO (Either Text ObsoleteCliRequest))
  , cliMaterializeReactivate :: ~(ReactivateCommand -> IO (Either Text ReactivateCliRequest))
  , cliMaterializeScope :: ScopeCommand -> IO (Either Text ScopeRequest)
  , cliMaterializeDomain :: DomainCommand -> IO (Either Text DomainRequest)
  , cliRunInit :: CliConfig -> IO (Either CliFailure (InitResult, PostCommitIndexResult))
  , cliRunCreate :: CliConfig -> CreateRequest -> IO (Either CliFailure (CreateResult, PostCommitIndexResult))
  , cliRunAmend :: CliConfig -> AmendRequest -> IO (Either CliFailure (AmendResult, PostCommitIndexResult))
  , cliRunObsolete :: ~(CliConfig -> Text -> ObsoleteCliRequest -> IO (Either CliFailure (ObsoleteResult, PostCommitIndexResult)))
  , cliRunReactivate :: ~(CliConfig -> Text -> ReactivateCliRequest -> IO (Either CliFailure (ReactivateResult, PostCommitIndexResult)))
  , cliRunScope :: CliConfig -> ScopeRequest -> IO (Either CliFailure (ScopeChangeResult, PostCommitIndexResult))
  , cliRunDomain :: CliConfig -> DomainRequest -> IO (Either CliFailure (DomainChangeResult, PostCommitIndexResult))
  }

productionCliDependencies :: CliDispatchDependencies
productionCliDependencies =
  CliDispatchDependencies runProductionCompile runProductionDoctor runProductionShow runProductionCompare runProductionHistory runProductionSearch runProductionRelevant materializeCreate materializeAmend materializeObsolete materializeReactivate materializeScope materializeDomain runProductionInit runProductionCreate runProductionAmend runProductionObsolete runProductionReactivate runProductionScope runProductionDomain

runProductionCompile :: CliConfig -> CompileCommand -> IO (Either CliFailure CompileResult)
runProductionCompile config command = do
  repositoryResult <- discoverRepository systemGit (configRepo config)
  case repositoryResult of
    Left problem -> pure (Left (CliUserFailure (Text.pack (show problem))))
    Right repository -> do
      revisionResult <- resolveRepositoryRevision repository (RevisionSpec (compileAt command))
      case revisionResult of
        Left problem -> pure (Left (CliUserFailure (Text.pack (show problem))))
        Right revision -> do
          databaseResult <- prepareIndexPath repository
          case databaseResult of
            Left problem -> pure (Left (CliUserFailure problem))
            Right database -> do
              indexed <- indexCommitted database repository (resolvedCommitOid revision)
              case (postCommitIndexed indexed, postCommitDatabase indexed, postCommitIndexRevision indexed, postCommitIndexError indexed) of
                (True, Just published, Just indexedRevision, Nothing)
                  | published /= database -> pure (Left (CliUserFailure "compile published an unexpected database path"))
                  | indexedRevision /= resolvedCommitOid revision -> pure (Left (CliUserFailure "compile published an unexpected revision"))
                  | otherwise -> loadPublishedCompileResult published indexedRevision
                (_, _, _, Just problem) -> pure (Left (CliUserFailure ("compile failed: " <> Text.pack (show problem))))
                _ -> pure (Left (CliUserFailure "compile returned an incomplete result"))

runProductionDoctor :: CliConfig -> DoctorCommand -> IO (Either CliFailure DoctorOutput)
runProductionDoctor config command = do
  repositoryResult <- discoverRepository systemGit (configRepo config)
  case repositoryResult of
    Left problem -> pure (Left (CliUserFailure (Text.pack (show problem))))
    Right repository -> do
      revisionResult <- resolveRepositoryRevision repository (RevisionSpec (doctorAt command))
      case revisionResult of
        Left problem -> pure (Left (CliUserFailure (Text.pack (show problem))))
        Right revision -> do
          shallowResult <- isShallowRepository repository
          case shallowResult of
            Left problem -> pure (Left (CliUserFailure (Text.pack (show problem))))
            Right shallow -> do
              databaseResult <- prepareIndexPath repository
              case databaseResult of
                Left problem -> pure (Left (CliUserFailure problem))
                Right database -> do
                  indexed <- indexCommitted database repository (resolvedCommitOid revision)
                  case (postCommitIndexed indexed, postCommitDatabase indexed, postCommitIndexRevision indexed, postCommitIndexError indexed) of
                    (True, Just published, Just indexedRevision, Nothing)
                      | published /= database -> pure (Left (CliUserFailure "doctor published an unexpected database path"))
                      | indexedRevision /= resolvedCommitOid revision -> pure (Left (CliUserFailure "doctor published an unexpected revision"))
                      | otherwise -> loadDoctorOutput published indexedRevision shallow
                    (_, _, _, Just problem) -> pure (Left (CliUserFailure ("doctor failed: " <> Text.pack (show problem))))
                    _ -> pure (Left (CliUserFailure "doctor returned an incomplete index result"))

loadDoctorOutput :: FilePath -> GitOid -> Bool -> IO (Either CliFailure DoctorOutput)
loadDoctorOutput database expectedRevision shallow = do
  captured <- try (bracket (open database) close readRows) :: IO (Either SomeException DoctorRows)
  case captured of
    Left exception ->
      case fromException exception of
        Just cancellation -> throwIO (cancellation :: SomeAsyncException)
        Nothing -> pure (Left (CliUserFailure ("unable to read doctor database: " <> Text.pack (displayException exception))))
    Right (metadata, issues, conflicts) ->
      pure (first CliUserFailure (doctorOutputFromRows database expectedRevision shallow metadata issues conflicts))
  where
    readRows connection = do
      metadata <- query_ connection "SELECT key,value FROM meta ORDER BY key"
      issues <- query_ connection "SELECT ordinal,code,severity,origin,adr_id,object_id,path,message FROM issue ORDER BY ordinal"
      conflicts <- query_ connection "SELECT adr_id,state_token,summaries FROM adr_conflict ORDER BY adr_id"
      pure (metadata, issues, conflicts)

type DoctorRows =
  ( [(Text, Text)]
  , [(Int, Text, Text, Text, Maybe Text, Maybe Text, Maybe Text, Text)]
  , [(Text, Text, Text)]
  )

doctorOutputFromRows
  :: FilePath
  -> GitOid
  -> Bool
  -> [(Text, Text)]
  -> [(Int, Text, Text, Text, Maybe Text, Maybe Text, Maybe Text, Text)]
  -> [(Text, Text, Text)]
  -> Either Text DoctorOutput
doctorOutputFromRows database expectedRevision shallow metadata issueRows conflictRows = do
  compiled <- compileResultFromMeta database expectedRevision metadata
  if coldCompilerIssueCount compiled == length issueRows
    then pure ()
    else Left "doctor issue rows do not match compiled issue count"
  issues <- traverse materializeIssue issueRows
  let errorCount = length (filter ((== "error") . doctorIssueSeverity) issues)
      warningCount = length issues - errorCount
  Right
    DoctorOutput
      { doctorOk = errorCount == 0
      , doctorRevision = gitOidText expectedRevision
      , doctorDatabase = Just database
      , doctorShallow = shallow
      , doctorIssues = issues
      , doctorCacheStatus = []
      , doctorCounts = DoctorCounts errorCount warningCount
      , doctorCurrentAccess = Nothing
      , doctorDatabaseBuild = Nothing
      }
  where
    conflicts = Map.fromList [(adr, (token, summaries)) | (adr, token, summaries) <- conflictRows]

    materializeIssue (_, code, severity, _, adr, objectId, path, message)
      | severity /= "error" && severity /= "warning" =
          Left ("doctor database has invalid issue severity: " <> severity)
      | code == "ADR_CONFLICT" =
          case adr >>= (`Map.lookup` conflicts) of
            Nothing -> Left "doctor conflict issue is missing its conflict details"
            Just (token, summaries) ->
              Right
                DoctorIssue
                  { doctorIssueSeverity = severity
                  , doctorIssueCode = code
                  , doctorIssueMessage = message
                  , doctorIssueAdrId = adr
                  , doctorIssueObjectId = objectId
                  , doctorIssuePath = path
                  , doctorIssueStateToken = Just token
                  , doctorIssueConflicts = map Aeson.String (Text.splitOn "\n" summaries)
                  }
      | otherwise =
          Right
            DoctorIssue
              { doctorIssueSeverity = severity
              , doctorIssueCode = code
              , doctorIssueMessage = message
              , doctorIssueAdrId = adr
              , doctorIssueObjectId = objectId
              , doctorIssuePath = path
              , doctorIssueStateToken = Nothing
              , doctorIssueConflicts = []
              }

loadPublishedCompileResult :: FilePath -> GitOid -> IO (Either CliFailure CompileResult)
loadPublishedCompileResult database expectedRevision = do
  captured <- try (bracket (open database) close readRows) :: IO (Either SomeException [(Text, Text)])
  case captured of
    Left exception ->
      case fromException exception of
        Just cancellation -> throwIO (cancellation :: SomeAsyncException)
        Nothing -> pure (Left (CliUserFailure ("unable to read compiled database metadata: " <> Text.pack (displayException exception))))
    Right rows -> pure (first CliUserFailure (compileResultFromMeta database expectedRevision rows))
  where
    readRows connection = query_ connection "SELECT key,value FROM meta ORDER BY key"

compileResultFromMeta :: FilePath -> GitOid -> [(Text, Text)] -> Either Text CompileResult
compileResultFromMeta database expectedRevision rows = do
  resolved <- one "resolved_oid"
  if resolved == gitOidText expectedRevision
    then pure ()
    else Left "compiled database revision does not match the requested revision"
  managedSources <- count "managed_source_count"
  issueCount <- count "issue_count"
  conflictCount <- count "conflict_count"
  operationCount <- count "operation_count"
  searchDocuments <- count "search_document_count"
  cacheKey <- one "materialization_fingerprint"
  if conflictCount <= issueCount
    then
      Right
        CompileResult
          { coldCompilerDatabase = database
          , coldCompilerRevision = resolved
          , coldCompilerIssueCount = issueCount
          , coldCompilerErrorCount = conflictCount
          , coldCompilerWarningCount = issueCount - conflictCount
          , coldCompilerEmbeddingComputed = 0
          , coldCompilerEmbeddingReused = 0
          , coldCompilerCacheMode = "full"
          , coldCompilerDocumentsParsed = managedSources
          , coldCompilerDocumentsReused = 0
          , coldCompilerHistoryCommitsScanned = 0
          , coldCompilerIncrementalKind = "full"
          , coldCompilerAdrsRebuilt = operationCount
          , coldCompilerAdrsReused = 0
          , coldCompilerAnnBuckets = searchDocuments
          , coldCompilerCacheKey = cacheKey
          , coldCompilerCacheRetainRevisions = 12
          }
    else Left "compiled database conflict count exceeds issue count"
  where
    grouped = Map.fromListWith (<>) [(key, [value]) | (key, value) <- rows]
    one key =
      case Map.lookup key grouped of
        Just [value] -> Right value
        Just _ -> Left ("compiled database has duplicate metadata key: " <> key)
        Nothing -> Left ("compiled database is missing metadata key: " <> key)
    count key = do
      raw <- one key
      case TextRead.decimal raw of
        Right (value, "")
          | value <= fromIntegral (maxBound :: Int) -> Right value
        _ -> Left ("compiled database has invalid nonnegative integer metadata: " <> key)

runProductionShow :: CliConfig -> ShowCommand -> IO (Either CliFailure ShowResult)
runProductionShow config command = do
  repositoryResult <- discoverRepository systemGit (configRepo config)
  case repositoryResult of
    Left problem -> pure (Left (CliUserFailure (Text.pack (show problem))))
    Right repository -> do
      result <- runShow repository
        ShowRequest
          { showRequestReference = showAdrId command
          , showRequestView = showView command
          , showRequestRevision = showAt command
          , showRequestRaw = showRaw command
          }
      pure $ case result of
        Left failure
          | showFailureIsConflict failure -> Left (CliConflictFailure (showFailureText failure))
          | otherwise -> Left (CliUserFailure (showFailureText failure))
        Right projection -> Right projection

runProductionCompare :: CliConfig -> CompareCommand -> IO (Either CliFailure CompareProjection)
runProductionCompare config command = do
  repositoryResult <- discoverRepository systemGit (configRepo config)
  case repositoryResult of
    Left problem -> pure (Left (CliUserFailure (Text.pack (show problem))))
    Right repository -> do
      result <-
        runCompare repository
          CompareRequest
            { compareRequestFrom = compareBefore command,
              compareRequestTo = compareAfter command,
              compareRequestIncludeUnchanged = compareUnchanged command
            }
      pure $ case result of
        Left failure -> Left (CliUserFailure (compareFailureText failure))
        Right projection -> Right projection

runProductionHistory :: CliConfig -> HistoryCommand -> IO (Either CliFailure HistoryProjection)
runProductionHistory config command =
  case traverse historyActorSelector (historyActor command) of
    Left problem -> pure (Left (CliUserFailure problem))
    Right actorSelector -> do
      repositoryResult <- discoverRepository systemGit (configRepo config)
      case repositoryResult of
        Left problem -> pure (Left (CliUserFailure (Text.pack (show problem))))
        Right repository -> do
          result <-
            runHistory repository
              HistoryRequest
                { historyRequestReference = historyAdrId command,
                  historyRequestRevision = historyAt command,
                  historyRequestOptions =
                    HistoryOptions
                      { historyOptionOrder = if historyReverse command then OldestFirst else NewestFirst,
                        historyOptionLimit = historyLimit command,
                        historyOptionActor = actorSelector,
                        historyOptionSince = historySince command,
                        historyOptionUntil = historyUntil command
                      }
                }
          pure $ case result of
            Left failure -> Left (CliUserFailure (historyFailureText failure))
            Right projection -> Right projection
  where
    historyActorSelector value = do
      actor <- parseActor value Nothing
      Right (ActorSelector (actorKind actor) (actorId actor))

runProductionSearch :: CliConfig -> SearchServiceRequest -> IO (Either CliFailure SearchProjection)
runProductionSearch config request = do
  repositoryResult <- discoverRepository systemGit (configRepo config)
  case repositoryResult of
    Left problem -> pure (Left (CliUserFailure (Text.pack (show problem))))
    Right repository -> do
      result <- runSearch repository request
      pure $ case result of
        Left failure
          | searchFailureIsConflict failure -> Left (CliConflictFailure (searchFailureText failure))
          | otherwise -> Left (CliUserFailure (searchFailureText failure))
        Right projection -> Right projection

runProductionRelevant :: CliConfig -> RelevantRequest -> IO (Either CliFailure RelevantProjection)
runProductionRelevant config request = do
  repositoryResult <- discoverRepository systemGit (configRepo config)
  case repositoryResult of
    Left problem -> pure (Left (CliUserFailure (Text.pack (show problem))))
    Right repository -> do
      result <- runRelevantQuery repository request
      pure $ case result of
        Left failure -> Left (CliUserFailure (relevantFailureText failure))
        Right projection -> Right projection

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

runProductionObsolete :: CliConfig -> Text -> ObsoleteCliRequest -> IO (Either CliFailure (ObsoleteResult, PostCommitIndexResult))
runProductionObsolete config adrText request = do
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
            Right snapshot -> case mkAdrId adrText of
              Left problem -> pure (Left (CliUserFailure (Text.pack (show problem))))
              Right adr -> do
                result <- obsoleteCommand repository (repositorySnapshotManagedPaths snapshot) (obsoleteRequestActor request) adr (obsoleteIntent request) (obsoleteRequestInputs request)
                case result of
                  Left problem -> pure (Left (transactionFailure problem))
                  Right obsoleteResult -> Right . (obsoleteResult,) <$> indexCommitted database repository (obsoleteCommitOid obsoleteResult)

runProductionReactivate :: CliConfig -> Text -> ReactivateCliRequest -> IO (Either CliFailure (ReactivateResult, PostCommitIndexResult))
runProductionReactivate config adrText request = do
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
            Right snapshot -> case mkAdrId adrText of
              Left problem -> pure (Left (CliUserFailure (Text.pack (show problem))))
              Right adr -> do
                result <- reactivateCommand repository (repositorySnapshotManagedPaths snapshot) (reactivateRequestActor request) adr (reactivateIntent request) (reactivateRequestInputs request)
                case result of
                  Left problem -> pure (Left (transactionFailure problem))
                  Right reactivateResult -> Right . (reactivateResult,) <$> indexCommitted database repository (reactivateCommitOid reactivateResult)

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

-- | CLI provenance accompanies the typed status intent without asking the
-- service to derive facts from the repository a second time.
data ObsoleteCliRequest = ObsoleteCliRequest
  { obsoleteIntent :: ObsoleteRequest
  , obsoleteRequestActor :: Actor
  , obsoleteRequestInputs :: ProvenanceInputs
  }
  deriving (Eq, Show)

data ReactivateCliRequest = ReactivateCliRequest
  { reactivateIntent :: ReactivateRequest
  , reactivateRequestActor :: Actor
  , reactivateRequestInputs :: ProvenanceInputs
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

materializeObsolete :: ObsoleteCommand -> IO (Either Text ObsoleteCliRequest)
materializeObsolete command = do
  environmentActor <- lookupEnv "ADRAI_ACTOR"
  promptFromFile <- readDigestFile "prompt" (obsoletePromptFileSpec command)
  contextFromFile <- readDigestFile "context" (obsoleteContextFileSpec command)
  pure $ do
    promptFileDigest <- promptFromFile
    contextFileDigest <- contextFromFile
    _ <- first (Text.pack . show) (mkAdrId (obsoleteAdrSpec command))
    replacement <- traverse (first (Text.pack . show) . mkAdrId) (obsoleteReplacementSpec command)
    expected <- traverse (first (Text.pack . show) . Format.parseStateToken) (obsoleteExpectedStateSpec command)
    let reason = obsoleteReasonSpec command
    if Text.null (Text.strip reason) then Left "obsolete reason must be nonblank" else Right ()
    actorText <- maybe (Left "obsolete requires --actor or ADRAI_ACTOR") Right (obsoleteActorSpec command <|> Text.pack <$> environmentActor)
    actor <- parseActor actorText (obsoleteModelSpec command)
    input <- traverse parseDigest (obsoleteInputDigestSpec command)
    prompt <- resolveDigest "prompt" (obsoletePromptDigestSpec command) promptFileDigest
    context <- resolveDigest "context" (obsoleteContextDigestSpec command) contextFileDigest
    pure (ObsoleteCliRequest (ObsoleteRequest expected reason (obsoleteResolveSpec command) replacement) actor (ProvenanceInputs input prompt context))

materializeReactivate :: ReactivateCommand -> IO (Either Text ReactivateCliRequest)
materializeReactivate command = do
  environmentActor <- lookupEnv "ADRAI_ACTOR"
  promptFromFile <- readDigestFile "prompt" (reactivatePromptFileSpec command)
  contextFromFile <- readDigestFile "context" (reactivateContextFileSpec command)
  pure $ do
    promptFileDigest <- promptFromFile
    contextFileDigest <- contextFromFile
    _ <- first (Text.pack . show) (mkAdrId (reactivateAdrSpec command))
    expected <- traverse (first (Text.pack . show) . Format.parseStateToken) (reactivateExpectedStateSpec command)
    let reason = reactivateReasonSpec command
    if Text.null (Text.strip reason) then Left "reactivate reason must be nonblank" else Right ()
    actorText <- maybe (Left "reactivate requires --actor or ADRAI_ACTOR") Right (reactivateActorSpec command <|> Text.pack <$> environmentActor)
    actor <- parseActor actorText (reactivateModelSpec command)
    input <- traverse parseDigest (reactivateInputDigestSpec command)
    prompt <- resolveDigest "prompt" (reactivatePromptDigestSpec command) promptFileDigest
    context <- resolveDigest "context" (reactivateContextDigestSpec command) contextFileDigest
    pure (ReactivateCliRequest (ReactivateRequest expected reason (reactivateResolveSpec command)) actor (ProvenanceInputs input prompt context))

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
indexCommitted database repository commit =
  capturePostCommitIndex (compilePostCommitIndex repository commit database)

capturePostCommitIndex :: IO PostCommitIndexResult -> IO PostCommitIndexResult
capturePostCommitIndex action = do
  indexed <- try action :: IO (Either SomeException PostCommitIndexResult)
  case indexed of
    Left exception ->
      case fromException exception of
        Just cancellation -> throwIO (cancellation :: SomeAsyncException)
        Nothing -> pure (indexFailure (PostCommitIndexCompileException (Text.pack (displayException exception))))
    Right result -> pure result
  where
    indexFailure problem = PostCommitIndexResult False Nothing Nothing [] (Just problem)

data CliRendered = CliRendered
  { renderedStdout :: Text
  , renderedStderr :: Text
  , renderedExitCode :: ExitCode
  }
  deriving (Eq, Show)

renderDoctorOutcome :: DoctorCommand -> DoctorOutput -> CliRendered
renderDoctorOutcome command result =
  CliRendered output "" exitCode
  where
    output
      | doctorJson command = renderCanonicalJson (aesonValueToJsonValue (doctorOutputJson result))
      | otherwise =
          Text.unlines
            ( [ "ok=" <> booleanText (doctorOk result)
              , "revision=" <> doctorRevision result
              , "database=" <> maybe "null" Text.pack (doctorDatabase result)
              , "shallow=" <> booleanText (doctorShallow result)
              , "errors=" <> Text.pack (show (doctorErrorCount (doctorCounts result)))
              , "warnings=" <> Text.pack (show (doctorWarningCount (doctorCounts result)))
              ]
                <> map issueLine (doctorIssues result)
            )
    exitCode
      | doctorOk result = ExitSuccess
      | otherwise = ExitFailure 4
    booleanText True = "true"
    booleanText False = "false"
    issueLine issue =
      "issue="
        <> doctorIssueSeverity issue
        <> ":"
        <> doctorIssueCode issue
        <> ":"
        <> doctorIssueMessage issue

aesonValueToJsonValue :: Aeson.Value -> JsonValue
aesonValueToJsonValue value =
  case value of
    Aeson.Object fields ->
      JsonObject
        [ (Aeson.Key.toText key, aesonValueToJsonValue member)
        | (key, member) <- KeyMap.toList fields
        ]
    Aeson.Array members -> JsonArray (map aesonValueToJsonValue (foldr (:) [] members))
    Aeson.String member -> JsonString member
    Aeson.Number member ->
      case Scientific.floatingOrInteger member of
        Right integer -> JsonNumber integer
        Left decimal -> JsonDecimal decimal
    Aeson.Bool member -> JsonBool member
    Aeson.Null -> JsonNull

renderCompileOutcome :: CompileCommand -> CompileResult -> CliRendered
renderCompileOutcome command result =
  CliRendered output "" ExitSuccess
  where
    output
      | compileJson command = renderCanonicalJson (compileResultValue result)
      | otherwise =
          Text.unlines
            [ "revision=" <> coldCompilerRevision result
            , "database=" <> Text.pack (coldCompilerDatabase result)
            , "documents_parsed=" <> Text.pack (show (coldCompilerDocumentsParsed result))
            , "issues=" <> Text.pack (show (coldCompilerIssueCount result))
            ]

renderShowSuccess :: ShowCommand -> ShowResult -> IO ExitCode
renderShowSuccess command result = emitRendered (renderShowOutcome command result)

renderShowOutcome :: ShowCommand -> ShowResult -> CliRendered
renderShowOutcome command result =
  CliRendered
    output
    ""
    ExitSuccess
  where
    output
      | showJson command = renderCanonicalJson value
      | otherwise = case result of
          ShowCollapsed projection -> TextEncoding.decodeUtf8 (renderCollapsedProjection projection)
          ShowExploded projection -> TextEncoding.decodeUtf8 (renderExplodedProjection projection)
    value = case result of
      ShowCollapsed projection -> collapsedProjectionJson projection
      ShowExploded projection -> explodedProjectionJson projection

renderCompareOutcome :: CompareCommand -> CompareProjection -> CliRendered
renderCompareOutcome command projection =
  CliRendered
    output
    ""
    ExitSuccess
  where
    output
      | compareJson command = renderCanonicalJson (compareProjectionJson projection)
      | otherwise = TextEncoding.decodeUtf8 (renderCompareProjection projection)

renderHistoryOutcome :: HistoryCommand -> HistoryProjection -> CliRendered
renderHistoryOutcome command projection =
  CliRendered
    output
    ""
    ExitSuccess
  where
    output
      | historyJson command = renderCanonicalJson (historyProjectionJson projection)
      | otherwise = TextEncoding.decodeUtf8 (renderHistoryProjection projection)

renderSearchOutcome :: SearchCommand -> SearchProjection -> CliRendered
renderSearchOutcome command projection =
  CliRendered
    output
    ""
    ExitSuccess
  where
    output
      | searchJson command = renderCanonicalJson (searchProjectionJson projection)
      | otherwise = TextEncoding.decodeUtf8 (renderSearchProjection projection)

renderRelevantOutcome :: RelevantCommand -> RelevantProjection -> CliRendered
renderRelevantOutcome command projection =
  CliRendered
    output
    ""
    ExitSuccess
  where
    output
      | relevantJson command = renderCanonicalJson (relevantProjectionJson projection)
      | otherwise = TextEncoding.decodeUtf8 (renderRelevantProjection projection)

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

renderObsoleteSuccess :: ObsoleteCommand -> ObsoleteResult -> PostCommitIndexResult -> IO ExitCode
renderObsoleteSuccess command result indexResult =
  emitRendered (renderObsoleteOutcome result indexResult (obsoleteJson command))

renderReactivateSuccess :: ReactivateCommand -> ReactivateResult -> PostCommitIndexResult -> IO ExitCode
renderReactivateSuccess command result indexResult =
  emitRendered (renderReactivateOutcome result indexResult (reactivateJson command))

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
      , "amends=" <> renderAmendParents (amendAmends result)
      , "connection=" <> connectionIdText (amendConnectionId result)
      ]
    jsonFields =
      mutationJsonFields (amendOperationId result) (amendCommitOid result) (amendCreatedPaths result) (amendIndexUpdated result) indexResult
        <> [ ("adr", JsonString (adrIdText (amendAdrId result)))
           , ("record", JsonString (recordIdText (amendRecordId result)))
           , ("amends", amendParentsJson (amendAmends result))
           , ("connection", JsonString (connectionIdText (amendConnectionId result)))
           ]
    renderAmendParents = Text.intercalate "," . map recordIdText
    amendParentsJson parents =
      case parents of
        [parent] -> JsonString (recordIdText parent)
        _ -> JsonArray (map (JsonString . recordIdText) parents)

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

renderObsoleteOutcome :: ObsoleteResult -> PostCommitIndexResult -> Bool -> CliRendered
renderObsoleteOutcome result indexResult jsonOutput =
  successOutcome jsonOutput (Text.pack (obsoleteOperationId result)) (obsoleteCommitOid result) identifiers indexResult jsonFields
  where
    identifiers = ["adr=" <> adrIdText (obsoleteAdrId result), "connection=" <> connectionIdText (obsoleteConnectionId result)]
    jsonFields =
      mutationJsonFields (obsoleteOperationId result) (obsoleteCommitOid result) (obsoleteCreatedPaths result) (obsoleteIndexUpdated result) indexResult
        <> [ ("adr", JsonString (adrIdText (obsoleteAdrId result)))
           , ("connection", JsonString (connectionIdText (obsoleteConnectionId result)))
           , ("obsolete", JsonBool True)
           , ("resolved_status_conflict", JsonBool (obsoleteResolvedConflict result))
           , ("covered_records", JsonArray (map (JsonString . recordIdText) (obsoleteRecordHeads result)))
           , ("replacement", maybe JsonNull (JsonString . adrIdText) (obsoleteReplacementAdr result))
           ]

renderReactivateOutcome :: ReactivateResult -> PostCommitIndexResult -> Bool -> CliRendered
renderReactivateOutcome result indexResult jsonOutput =
  successOutcome jsonOutput (Text.pack (reactivateOperationId result)) (reactivateCommitOid result) identifiers indexResult jsonFields
  where
    identifiers = ["adr=" <> adrIdText (reactivateAdrId result), "connection=" <> connectionIdText (reactivateConnectionId result)]
    jsonFields =
      mutationJsonFields (reactivateOperationId result) (reactivateCommitOid result) (reactivateCreatedPaths result) (reactivateIndexUpdated result) indexResult
        <> [ ("adr", JsonString (adrIdText (reactivateAdrId result)))
           , ("connection", JsonString (connectionIdText (reactivateConnectionId result)))
           , ("obsolete", JsonBool False)
           , ("resolved_status_conflict", JsonBool (reactivateResolvedConflict result))
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
    [adr, connection] ->
      Text.unlines
        [ "Committed " <> operation <> " as " <> gitOidText commit
        , adr <> "  " <> connection
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
      || "amend target ADR is conflicted" `Text.isInfixOf` message
      || "amend decision conflict requires title, summary, and body" `Text.isInfixOf` message
      || "amend target ADR is not active" `Text.isInfixOf` message
      || "scope target ADR is conflicted" `Text.isInfixOf` message
      || "scope target ADR has no unambiguous current scope" `Text.isInfixOf` message
      || "scope target ADR is not active" `Text.isInfixOf` message
      || "domain target ADR is conflicted" `Text.isInfixOf` message
      || "domain target ADR has no unambiguous current domain" `Text.isInfixOf` message
      || "domain target ADR is not active" `Text.isInfixOf` message
      || "obsolete target ADR is already obsolete" `Text.isInfixOf` message
      || "reactivate target ADR is already active" `Text.isInfixOf` message
      || "status target ADR is conflicted" `Text.isInfixOf` message
      || "status target ADR has no unambiguous current status" `Text.isInfixOf` message
      || "status target ADR is not active" `Text.isInfixOf` message
      || "status target ADR is not obsolete" `Text.isInfixOf` message
      || "status resolve requires a conflicted status axis" `Text.isInfixOf` message
      || "obsolete replacement ADR is conflicted" `Text.isInfixOf` message
      || "obsolete replacement ADR is not unambiguously active" `Text.isInfixOf` message
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
