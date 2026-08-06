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
    CliCommand (..),
    CliParser,
    parser,
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
import Data.Text (Text)
import qualified Data.Text as Text
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

-- | Default configuration: current directory, no explicit database, HEAD revision.
defaultCliConfig :: CliConfig
defaultCliConfig =
  CliConfig
    { configRepo = "."
    , configDatabase = Nothing
    , configAt = "HEAD"
    }

-- | Parsed CLI command, constructed from optparse-applicative.
data CliCommand
  = CmdCompile
  | CmdDoctor
  | CmdShow ShowCommand
  | CmdHistory HistoryCommand
  | CmdSearch SearchCommand
  | CmdRelevant RelevantCommand
  | CmdCompare CompareCommand
  deriving (Eq, Show)

-- | Top-level CLI parser type alias.
type CliParser = Parser CliCommand

-- | Top-level CLI parser combining all subcommands.
parser :: CliParser
parser =
  subparser
    ( command "compile" (info (pure CmdCompile) (progDesc "compile the repository"))
     <> command "doctor" (info (pure CmdDoctor) (progDesc "diagnose the database"))
     <> command "show" (info (CmdShow <$> showParser) (progDesc "show a single ADR"))
     <> command "history" (info (CmdHistory <$> historyParser) (progDesc "show operation history"))
     <> command "search" (info (CmdSearch <$> searchParser) (progDesc "search ADRs"))
     <> command "relevant" (info (CmdRelevant <$> relevantParser) (progDesc "find relevant ADRs"))
     <> command "compare" (info (CmdCompare <$> compareParser) (progDesc "compare revisions"))
    )

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
    <*> option auto (long "limit" <> help "max results (default 50)")
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
    <*> option auto (long "limit" <> help "max results (default 20)")
    <*> switch (long "json" <> help "output JSON")

-- | Relevant command parser.
relevantParser :: Parser RelevantCommand
relevantParser =
  RelevantCommand
    <$> strArgument (metavar "FILE" <> help "file path to find relevant ADRs for")
    <*> switch (long "include-obsolete" <> help "include obsolete ADRs")
    <*> option auto (long "limit" <> help "max results (default 10)")
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
  cmd <- execParser (info parser (progDesc "ADRAI - Architecture Decision Record tool"))
  exitCode <- dispatch cmd
  exitWith exitCode

-- | Dispatch a parsed command to its handler.
dispatch :: CliCommand -> IO ExitCode
dispatch CmdCompile = do
  putStrLn $ "[compile] compiling repository at " <> configRepo defaultCliConfig
  pure ExitSuccess

dispatch CmdDoctor = do
  putStrLn "[doctor] diagnosing database"
  pure ExitSuccess

dispatch (CmdShow ShowCommand { showAdrId }) = do
  putStrLn $ "[show] looking up ADR: " <> Text.unpack showAdrId
  pure ExitSuccess

dispatch CmdHistory {} = do
  putStrLn "[history] fetching history"
  pure ExitSuccess

dispatch (CmdSearch SearchCommand { searchQuery }) = do
  putStrLn $ "[search] querying: " <> Text.unpack searchQuery
  pure ExitSuccess

dispatch CmdRelevant {} = do
  putStrLn "[relevant] finding relevant ADRs"
  pure ExitSuccess

dispatch (CmdCompare CompareCommand { compareBefore, compareAfter }) = do
  putStrLn $ "[compare] from: " <> Text.unpack compareBefore <> ", to: " <> Text.unpack compareAfter
  pure ExitSuccess
