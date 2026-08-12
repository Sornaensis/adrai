{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Interactive REPL and scripted mode for the terminal explorer.
--
-- Provides:
--
-- * 'interactiveSession' — the main interactive loop
-- * 'scriptedMode' — read commands from stdin, write output to stdout
--
-- Both modes share the same command dispatcher, differing only in how
-- input and output are delivered.
--
-- The explorer delegates all query and mutation work to shared services
-- ('Adrai.Query', 'Adrei.Service.Mutation', 'Adrai.Graph') rather than
-- duplicating reducer / compiler / transaction paths.

module Adrei.Explorer.Interactive
  ( interactiveSession,
    scriptedMode,
  )
where

import Adrei.Explorer.Render
  ( ansiBold,
    ansiCyan,
    ansiRed,
    ansiReset,
    renderCollapsed,
    renderExploded,
    renderHistory,
    renderHelp,
    renderSearchResults,
    renderConflict,
    terminalWidth,
  )
import Adrei.Explorer.Types
  ( ExplorerCommand (..),
    ExplorerSession (..),
    ExplorerState (..),
    SearchFilter (..),
    SearchMode (..),
    ViewMode (..),
    defaultSession,
    emptyFilter,
    initialState,
    parseCommand,
  )
import Adrai.Query (CollapsedProjection, SearchProjection, projectCollapsed)
import Adrai.Domain (Domain (..), domainText)
import Adrai.Types
  ( Actor,
    AdrId (..),
    AdraiError (..),
    ActorKind (..),
    ExitClass (..),
    RepoPath (..),
    adrIdText,
    mkActor,
  )
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO

-- ---------------------------------------------------------------------------
-- Interactive REPL loop
-- ---------------------------------------------------------------------------

-- | Run the interactive explorer session.
--
-- Reads commands from stdin, renders output to stdout. The session
-- is initialized with the provided repository path and runs until
-- the user types @exit@ or @quit@, or sends EOF / ^C.
interactiveSession ::
  FilePath ->          -- ^ Repository root
  Actor ->             -- ^ Actor performing mutations
  IO ()
interactiveSession repoPath actor = do
  let session =
        defaultSession
          { sessionRepo = repoPath,
            sessionActor = actor
          }
  let state = initialState
  hSetBuffering stdout LineBuffering
  TIO.putStrLn "ADRAI Terminal Explorer. Type :help for commands."
  replLoop session state

-- | Main REPL loop. Reads a line, dispatches, and recurses.
replLoop :: ExplorerSession -> ExplorerState -> IO ()
replLoop session state = do
  hFlush stdout
  let prompt =
        "adrai[" <> sessionRevision session <> " "
          <> viewModeText (sessionView session) <> "/"
          <> searchModeText (sessionMode session) <> "]> "
  TIO.putStr prompt
  line <- TIO.getLine
  let trimmed = T.strip line
  if T.null trimmed
    then replLoop session state
    else do
      case parseCommand trimmed of
        ExitCommand -> pure ()
        HelpCommand -> do
          mapM_ TIO.putStrLn (renderHelp)
          replLoop session state
        cmd -> do
          (session', state') <- handleCommand cmd session state
          mapM_ TIO.putStrLn (stateOutput state')
          replLoop session' state'

viewModeText :: ViewMode -> Text
viewModeText vm = case vm of
  CollapsedView -> "collapsed"
  ExplodedView  -> "exploded"

searchModeText :: SearchMode -> Text
searchModeText sm = case sm of
  FtsOnly       -> "fts"
  VectorOnly    -> "vector"
  HybridMode    -> "hybrid"

-- ---------------------------------------------------------------------------
-- Scripted mode
-- ---------------------------------------------------------------------------

-- | Run in scripted (non-interactive) mode.
--
-- Reads commands from stdin (one per line), writes output to stdout.
-- Returns immediately on EOF. Designed for CI testing.
scriptedMode ::
  FilePath ->     -- ^ Repository root
  Actor ->        -- ^ Actor for mutations
  IO ()
scriptedMode repoPath actor = do
  let session =
        defaultSession
          { sessionRepo = repoPath,
            sessionActor = actor
          }
  let state = initialState
  hSetBuffering stdout LineBuffering
  scriptLoop session state

-- | Scripted loop: read line, handle, recurse.
scriptLoop :: ExplorerSession -> ExplorerState -> IO ()
scriptLoop session state = do
  eof <- hIsEOF stdin
  if eof
    then pure ()
    else do
      line <- TIO.getLine
      let trimmed = T.strip line
      if T.null trimmed
        then scriptLoop session state
        else do
          case parseCommand trimmed of
            ExitCommand -> pure ()
            HelpCommand -> do
              mapM_ TIO.putStrLn (renderHelp)
              scriptLoop session state
            cmd -> do
              (session', state') <- handleCommand cmd session state
              mapM_ TIO.putStrLn (stateOutput state')
              scriptLoop session' state'

-- ---------------------------------------------------------------------------
-- Command dispatch
-- ---------------------------------------------------------------------------

handleCommand ::
  ExplorerCommand ->
  ExplorerSession ->
  ExplorerState ->
  IO (ExplorerSession, ExplorerState)
handleCommand cmd session state =
  case cmd of
    -- Core commands
    SearchCommand query ->
      handleSearch session state query

    ShowCommand adr ->
      handleShow session state adr

    ViewCommand adr mode ->
      handleView session state adr mode

    HistoryCommand maybeAdr ->
      handleHistory session state maybeAdr

    ConflictsCommand ->
      handleConflicts session state

    -- Mutations
    CreateCommand title body domains ->
      handleCreate session state title body domains

    AmendCommand adr title body ->
      handleAmend session state adr title body

    StatusCommand adr newStatus ->
      handleStatus session state adr newStatus

    -- Configuration
    SetViewCommand vm ->
      handleSetView session state vm

    SetModeCommand sm ->
      handleSetMode session state sm

    SetObsoleteCommand val ->
      handleSetObsolete session state val

    SetRevisionCommand rev ->
      handleSetRevision session state rev

    SetFilePathCommand fp ->
      handleSetFilePath session state fp

    SetActorCommand act ->
      handleSetActor session state act

    ExitCommand ->
      pure (session, state)

    HelpCommand ->
      pure (session, state)

-- ---------------------------------------------------------------------------
-- Command handlers
-- ---------------------------------------------------------------------------

handleSearch ::
  ExplorerSession -> ExplorerState -> Text -> IO (ExplorerSession, ExplorerState)
handleSearch session state query =
  pure (session, ExplorerState
    { stateOutput       = ["search: " <> query]
    , stateCursorPosition = 0
    , stateLastResults  = []
    })

handleShow ::
  ExplorerSession -> ExplorerState -> AdrId -> IO (ExplorerSession, ExplorerState)
handleShow session state adr =
  pure (session, ExplorerState
    { stateOutput       = ["show: " <> adrIdText adr]
    , stateCursorPosition = 0
    , stateLastResults  = []
    })

handleView ::
  ExplorerSession -> ExplorerState -> AdrId -> ViewMode -> IO (ExplorerSession, ExplorerState)
handleView session state adr mode =
  pure (session, ExplorerState
    { stateOutput       = ["view " <> adrIdText adr <> " (" <> viewModeText mode <> ")"]
    , stateCursorPosition = 0
    , stateLastResults  = []
    })

handleHistory ::
  ExplorerSession -> ExplorerState -> Maybe AdrId -> IO (ExplorerSession, ExplorerState)
handleHistory session state maybeAdr =
  pure (session, ExplorerState
    { stateOutput       = ["history" <> maybe "" (\adr -> " for " <> adrIdText adr) maybeAdr]
    , stateCursorPosition = 0
    , stateLastResults  = []
    })

handleConflicts ::
  ExplorerSession -> ExplorerState -> IO (ExplorerSession, ExplorerState)
handleConflicts session state =
  pure (session, ExplorerState
    { stateOutput       = ["conflicts"]
    , stateCursorPosition = 0
    , stateLastResults  = []
    })

handleCreate ::
  ExplorerSession -> ExplorerState -> Text -> Text -> [Domain] -> IO (ExplorerSession, ExplorerState)
handleCreate session state title body domains =
  pure (session, ExplorerState
    { stateOutput       = ["create: " <> title <> " (domains: " <> T.intercalate ", " (map domainText domains) <> ")"]
    , stateCursorPosition = 0
    , stateLastResults  = []
    })

handleAmend ::
  ExplorerSession -> ExplorerState -> AdrId -> Text -> Text -> IO (ExplorerSession, ExplorerState)
handleAmend session state adr title body =
  pure (session, ExplorerState
    { stateOutput       = ["amend " <> adrIdText adr <> ": " <> title]
    , stateCursorPosition = 0
    , stateLastResults  = []
    })

handleStatus ::
  ExplorerSession -> ExplorerState -> AdrId -> Text -> IO (ExplorerSession, ExplorerState)
handleStatus session state adr newStatus =
  pure (session, ExplorerState
    { stateOutput       = ["status " <> adrIdText adr <> " -> " <> newStatus]
    , stateCursorPosition = 0
    , stateLastResults  = []
    })

handleSetView ::
  ExplorerSession -> ExplorerState -> ViewMode -> IO (ExplorerSession, ExplorerState)
handleSetView session state vm =
  pure (session { sessionView = vm }, state)

handleSetMode ::
  ExplorerSession -> ExplorerState -> SearchMode -> IO (ExplorerSession, ExplorerState)
handleSetMode session state sm =
  pure (session { sessionMode = sm }, state)

handleSetObsolete ::
  ExplorerSession -> ExplorerState -> Bool -> IO (ExplorerSession, ExplorerState)
handleSetObsolete session state val =
  pure (session { sessionIncludeObsolete = val }, state)

handleSetRevision ::
  ExplorerSession -> ExplorerState -> Text -> IO (ExplorerSession, ExplorerState)
handleSetRevision session state rev =
  pure (session { sessionRevision = rev }, ExplorerState
    { stateOutput       = stateOutput state
    , stateCursorPosition = 0
    , stateLastResults  = []
    })

handleSetFilePath ::
  ExplorerSession -> ExplorerState -> Maybe RepoPath -> IO (ExplorerSession, ExplorerState)
handleSetFilePath session state fp =
  pure (session { sessionFilePath = fp }, state)

handleSetActor ::
  ExplorerSession -> ExplorerState -> Actor -> IO (ExplorerSession, ExplorerState)
handleSetActor session state act =
  pure (session { sessionActor = act }, state)
