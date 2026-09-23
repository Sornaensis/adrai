{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}

-- | Core types for the terminal explorer.
--
-- The explorer provides an interactive REPL and scripted mode. Its read
-- handlers currently emit placeholders; use the CLI or web interface for
-- repository queries.
--
-- Key types:
--
-- * 'ExplorerSession' — current session state (repo root, revision, view mode)
-- * 'ExplorerCommand' — parsed REPL commands (search, show, view, filter, ...)
-- * 'ExplorerState'   — buffered output and cursor tracking for the TUI
--
module Adrai.Explorer.Types
  ( -- * Session state
    ExplorerSession (..),
    defaultSession,

    -- * Commands
    ExplorerCommand (..),
    parseCommand,

    -- * View and mode
    ViewMode (..),
    SearchMode (..),

    -- * State
    ExplorerState (..),
    initialState,

    -- * Filter helpers
    SearchFilter (..),
    emptyFilter,
  )
where

import Adrai.Domain (Domain)
import Adrai.Types
  ( AdrId,
    Actor,
    ActorKind (..),
    RepoPath,
    mkActor,
    mkAdrId,
  )
import Data.Text (Text)
import qualified Data.Text as T

-- | Configuration for the interactive session.
--
-- Mirrors the Python 'ExplorerSession' dataclass: repo path, view mode,
-- search mode, and mutable flags that the REPL can adjust between commands.
data ExplorerSession = ExplorerSession
  { sessionRepo          :: FilePath,
    sessionView          :: ViewMode,
    sessionMode          :: SearchMode,
    sessionIncludeObsolete :: Bool,
    sessionFilePath      :: Maybe RepoPath,
    sessionRevision      :: Text,
    sessionQuery         :: Text,
    sessionActor         :: Actor
  } deriving (Show)

-- | Default session using a dummy repo and sensible defaults.
-- The REPL replaces this with a real repository before entering the loop.
defaultSession :: ExplorerSession
defaultSession =
  ExplorerSession
    { sessionRepo          = ".",
      sessionView          = CollapsedView,
      sessionMode          = HybridMode,
      sessionIncludeObsolete = False,
      sessionFilePath      = Nothing,
      sessionRevision      = "HEAD",
      sessionQuery         = "",
      sessionActor         =
        case mkActor HumanActor "user" Nothing of
          Right actor -> actor
          Left problem -> error ("default actor construction failed: " <> show problem)
    }

-- | How to display ADR details.
data ViewMode
  = CollapsedView
  | ExplodedView
  deriving (Eq, Ord, Show)

-- | Retrieval strategy for search.
data SearchMode
  = FtsOnly
  | VectorOnly
  | HybridMode
  deriving (Eq, Ord, Show)

-- | Parsed REPL command.
--
-- Status is available at the terminal. Create and amend constructors remain
-- available to the internal dispatcher, but terminal input for them is rejected.
data ExplorerCommand
  = HelpCommand
  | InvalidCommand Text
  | SearchCommand Text
  | ShowCommand AdrId
  | ViewCommand AdrId ViewMode
  | HistoryCommand (Maybe AdrId)
  | ConflictsCommand
  | CreateCommand Text Text [Domain]
      -- ^ title, body, domains
  | AmendCommand AdrId Text Text
      -- ^ id, new title, new body
  | StatusCommand AdrId Text
      -- ^ id, new status (active / obsolete)
  | SetViewCommand ViewMode
  | SetModeCommand SearchMode
  | SetObsoleteCommand Bool
  | SetRevisionCommand Text
  | SetFilePathCommand (Maybe RepoPath)
  | SetActorCommand Actor
  | ExitCommand
  deriving (Eq, Show)

-- | Parse a raw REPL line into an 'ExplorerCommand'.
--
-- Command-shaped malformed input receives guidance. Other free text remains
-- a search query for compatibility with the original REPL.
parseCommand :: Text -> ExplorerCommand
parseCommand rawInput =
  case T.words input of
    ["exit"] -> ExitCommand
    ["quit"] -> ExitCommand
    [":q"] -> ExitCommand
    [":quit"] -> ExitCommand
    ["help"] -> HelpCommand
    [":help"] -> HelpCommand
    ["conflicts"] -> ConflictsCommand
    [":conflicts"] -> ConflictsCommand
    ["history"] -> HistoryCommand Nothing
    [":history"] -> HistoryCommand Nothing
    ["history", ident] -> historyId ident
    [":history", ident] -> historyId ident
    "search" : rest | not (null rest) -> SearchCommand (T.unwords rest)
    ":search" : rest | not (null rest) -> SearchCommand (T.unwords rest)
    ["show", ident] -> withAdr ident ShowCommand
    [":show", ident] -> withAdr ident ShowCommand
    ["view", ident] -> withAdr ident (`ViewCommand` CollapsedView)
    [":view", ident] -> withAdr ident (`ViewCommand` CollapsedView)
    ["view", ident, mode] -> viewId ident mode
    [":view", ident, mode] -> viewId ident mode
    ["status", ident, status] | T.toLower status `elem` ["active", "obsolete"] -> withAdr ident (`StatusCommand` status)
    [":status", ident, status] | T.toLower status `elem` ["active", "obsolete"] -> withAdr ident (`StatusCommand` status)
    command : _ | T.dropWhile (== ':') command `elem` ["create", "amend"] ->
      InvalidCommand "Terminal explorer create/amend input is unavailable; no Git commit was made. Type :help. At the shell use adrai create --help, adrai amend --help, or adrai web."
    command : _ | command `elem` reservedCommands || T.isPrefixOf ":" command -> InvalidCommand ("Invalid explorer command: " <> input <> ". Type :help for syntax; use adrai COMMAND --help for CLI options.")
    _ -> SearchCommand input
  where
    input = T.strip rawInput
    reservedCommands = ["exit", "quit", "help", "conflicts", "history", "search", "show", "view", "amend", "status", "create", "filter"]
    withAdr ident make = either (const invalidId) make (mkAdrId ident)
    invalidId = InvalidCommand ("Invalid ADR ID or command syntax. Type :help; use adrai COMMAND --help for CLI options.")
    historyId ident = withAdr ident (HistoryCommand . Just)
    viewId ident mode = maybe invalidId (\viewMode -> withAdr ident (`ViewCommand` viewMode)) (parseViewMode mode)

    parseViewMode :: Text -> Maybe ViewMode
    parseViewMode v =
      case T.toLower v of
        "collapsed" -> Just CollapsedView
        "exploded"  -> Just ExplodedView
        _           -> Nothing

-- | Buffered output state for the terminal.
--
-- Tracks accumulated output and a cursor position for simple TUI support.
data ExplorerState = ExplorerState
  { stateOutput       :: [Text],
    stateCursorPosition :: Int,
    stateLastResults  :: [(Int, Text, Text, Double, [Text])]
      -- ^ (index, id, title, score, flags)
  } deriving (Show)

-- | Initial empty explorer state.
initialState :: ExplorerState
initialState =
  ExplorerState
    { stateOutput       = [],
      stateCursorPosition = 0,
      stateLastResults  = []
    }

-- | Filtering parameters for search.
data SearchFilter = SearchFilter
  { filterDomains       :: [Text],
    filterFile          :: Maybe RepoPath,
    filterSince         :: Maybe Integer,
    filterUntil         :: Maybe Integer
  } deriving (Show)

emptyFilter :: SearchFilter
emptyFilter = SearchFilter [] Nothing Nothing Nothing
