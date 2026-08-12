{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}

-- | Core types for the terminal explorer.
--
-- The explorer provides an interactive REPL and scripted (non-interactive)
-- mode over the shared typed services. It delegates all query and mutation
-- work to 'Adrai.Query' and 'Adrei.Service.Mutation' rather than
-- duplicating reducer / compiler / transaction paths.
--
-- Key types:
--
-- * 'ExplorerSession' — current session state (repo root, revision, view mode)
-- * 'ExplorerCommand' — parsed REPL commands (search, show, view, filter, ...)
-- * 'ExplorerState'   — buffered output and cursor tracking for the TUI
--
-- Commands support:
--
-- * 'help' — list available commands
-- * 'search <query>' — FTS search
-- * 'show <id>' — show ADR detail
-- * 'view <id> [collapsed|exploded]' — view ADR
-- * 'history [id]' — operation history
-- * 'conflicts' — show conflicts
-- * 'create --title T --body B' — create ADR (uses mutation service)
-- * 'amend <id> --title T --body B' — amend ADR
-- * 'status <id> [active|obsolete]' — change status
-- * 'exit' — quit

module Adrei.Explorer.Types
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

import Adrai.Domain (Domain (..), domainText)
import Adrai.Types
  ( AdrId (..),
    Actor,
    ActorKind (..),
    RepoPath (..),
    adrIdText,
    mkActor,
    mkAdrId,
    mkRepoPath,
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
      sessionActor         = let Right a = mkActor HumanActor "user" Nothing in a
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
-- Each constructor corresponds to a user-facing command. The parser
-- handles argument extraction so the REPL handler can focus on
-- dispatching to the appropriate service.
data ExplorerCommand
  = HelpCommand
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
-- The parser uses a simple prefix-matching scheme:
--
-- * Commands starting with @:@ are treated as configuration (:@view@,
--   @:@mode@, @:@obsolete@, @:@at@, @:@file@, @:@actor@).
-- * Mutations use @:@ prefixes (:@create@, @:@amend@, @:@status@).
-- * Standalone @history@, @conflicts@, @help@, @exit@ are parsed directly.
-- * Everything else is treated as a search query.
parseCommand :: Text -> ExplorerCommand
parseCommand input
  | input == "exit" || input == "quit" || input == ":q" || input == ":quit" = ExitCommand
  | input == "help" || input == ":help" = HelpCommand
  | input == "history" || input == ":history" = HistoryCommand Nothing
  | input == "conflicts" || input == ":conflicts" = ConflictsCommand
  | T.take 7 input == "search " = SearchCommand (T.drop 7 input)
  | input == ":search" = SearchCommand ""
  | T.take 5 input == "show " =
      case T.stripPrefix "show " input of
        Nothing -> SearchCommand input
        Just rawId ->
          case mkAdrId (T.strip rawId) of
            Right adr -> ShowCommand adr
            Left _    -> SearchCommand input
  | input == ":show" = SearchCommand ""
  | T.take 5 input == "view " = handleView input
  | input == ":view" = HelpCommand
  | T.take 6 input == "amend " = handleAmend input
  | input == ":amend" = SearchCommand ""
  | T.take 7 input == "status " = handleStatus input
  | input == ":status" = SearchCommand ""
  | T.take 7 input == "create " = CreateCommand (T.drop 7 input) "" []
  | input == ":create" = CreateCommand "" "" []
  | T.take 6 input == "filter" || T.take 6 input == "filter " = SearchCommand ""
  | T.take 4 input == "hist" || T.take 8 input == "history " = handleHistory input
  | T.take 3 input == ":vi" = handleView input
  | T.take 4 input == ":mod" = SearchCommand ""
  | T.take 8 input == ":obsolete" = SearchCommand ""
  | T.take 3 input == ":at" = SearchCommand ""
  | T.take 5 input == ":file" = SearchCommand ""
  | T.take 6 input == ":actor" = SearchCommand ""
  | otherwise = SearchCommand input
  where
    handleView :: Text -> ExplorerCommand
    handleView t =
      case T.stripPrefix "view " t of
        Nothing ->
          case T.stripPrefix ":view " t of
            Nothing -> HelpCommand
            Just rest ->
              case T.words rest of
                [idStr, modeStr] ->
                  case parseViewMode modeStr of
                    Just vm ->
                      case mkAdrId (T.strip idStr) of
                        Right adr -> ViewCommand adr vm
                        Left _    -> SearchCommand t
                    Nothing -> HelpCommand
                [idStr] ->
                  case mkAdrId (T.strip idStr) of
                    Right adr -> ViewCommand adr CollapsedView
                    Left _    -> SearchCommand t
                _ -> HelpCommand
        Just rest ->
          case T.words rest of
            [idStr, modeStr] ->
              case parseViewMode modeStr of
                Just vm ->
                  case mkAdrId (T.strip idStr) of
                    Right adr -> ViewCommand adr vm
                    Left _    -> SearchCommand rest
                Nothing -> SearchCommand rest
            [idStr] ->
              case mkAdrId (T.strip idStr) of
                Right adr -> ViewCommand adr CollapsedView
                Left _    -> SearchCommand rest
            _ -> HelpCommand

    handleHistory :: Text -> ExplorerCommand
    handleHistory t =
      case T.stripPrefix "history " t of
        Nothing ->
          case T.stripPrefix ":history " t of
            Nothing -> HistoryCommand Nothing
            Just rest ->
              case T.strip rest of
                "" -> HistoryCommand Nothing
                idStr ->
                  case mkAdrId (T.strip idStr) of
                    Right adr -> HistoryCommand (Just adr)
                    Left _    -> HistoryCommand Nothing
        Just rest ->
          case T.strip rest of
            "" -> HistoryCommand Nothing
            idStr ->
              case mkAdrId (T.strip idStr) of
                Right adr -> HistoryCommand (Just adr)
                Left _    -> HistoryCommand Nothing

    handleAmend :: Text -> ExplorerCommand
    handleAmend t =
      case T.stripPrefix "amend " t of
        Nothing ->
          case T.stripPrefix ":amend " t of
            Nothing -> SearchCommand t
            Just rest ->
              let parts = T.words rest
              in case parts of
                    [adrStr, title, body] ->
                      case mkAdrId (T.strip adrStr) of
                        Right adr -> AmendCommand adr title body
                        Left _    -> SearchCommand t
                    _ -> SearchCommand t
        Just rest ->
          let parts = T.words rest
          in case parts of
                [adrStr, title, body] ->
                  case mkAdrId (T.strip adrStr) of
                    Right adr -> AmendCommand adr title body
                    Left _    -> SearchCommand t
                _ -> SearchCommand t

    handleStatus :: Text -> ExplorerCommand
    handleStatus t =
      case T.stripPrefix "status " t of
        Nothing ->
          case T.stripPrefix ":status " t of
            Nothing -> SearchCommand t
            Just rest ->
              let parts = T.words rest
              in case parts of
                    [adrStr, newStatus] ->
                      case mkAdrId (T.strip adrStr) of
                        Right adr -> StatusCommand adr newStatus
                        Left _    -> SearchCommand t
                    _ -> SearchCommand t
        Just rest ->
          let parts = T.words rest
          in case parts of
                [adrStr, newStatus] ->
                  case mkAdrId (T.strip adrStr) of
                    Right adr -> StatusCommand adr newStatus
                    Left _    -> SearchCommand t
                _ -> SearchCommand t

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
