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
--
-- === Exit gate
--
-- After a mutation command (create, amend, status) the explorer shows
-- a summary and returns 'True' from the handler to signal the REPL
-- loop should exit. This matches the Python reference where these
-- are one-shot commands that produce output and terminate.
-- For scripted mode, mutations always exit after completion.

module Adrei.Explorer.Interactive
  ( interactiveSession,
    scriptedMode,
  )
where

import Adrei.Explorer.Mutation
  ( MutationResult (..),
    runMutation,
  )
import Adrei.Explorer.Render
  ( ansiBold,
    ansiBlue,
    ansiCyan,
    ansiGreen,
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
import Adrai.Git
  ( Repository (..),
    discoverRepository,
    gitOidText,
    systemGit,
  )
import Adrai.Types
  ( Actor,
    AdrId (..),
    AdraiError (..),
    ActorKind (..),
    ExitClass (..),
    RepoPath (..),
    adrIdText,
    mkActor,
    recordIdText,
    repoPathText,
  )
import Control.Monad (when, unless)
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
-- Discovers the repository at the given path, then enters the REPL loop.
-- Reads commands from stdin, renders output to stdout. The session
-- runs until the user types @exit@ or @quit@, or sends EOF / ^C.
interactiveSession ::
  FilePath ->          -- ^ Repository root
  Actor ->             -- ^ Actor performing mutations
  IO ()
interactiveSession repoPath actor = do
  repoResult <- discoverRepository systemGit repoPath
  case repoResult of
    Left err -> do
      TIO.putStrLn $ "Error: cannot open repository: " <> T.pack (show err)
    Right repository -> do
      let session =
            defaultSession
              { sessionRepo = repoPath,
                sessionActor = actor
              }
      let state = initialState
      hSetBuffering stdout LineBuffering
      TIO.putStrLn "ADRAI Terminal Explorer. Type :help for commands."
      replLoop repository session state

-- | Main REPL loop. Reads a line, dispatches, and recurses.
--
-- The 'Bool' return value signals whether to exit:
-- * 'True' after a successful mutation (exit gate)
-- * 'False' for all other commands (continue the loop)
replLoop ::
  Repository -> ExplorerSession -> ExplorerState -> IO ()
replLoop repository session state = do
  hFlush stdout
  let prompt =
        "adrai[" <> sessionRevision session <> " "
          <> viewModeText (sessionView session) <> "/"
          <> searchModeText (sessionMode session) <> "]> "
  TIO.putStr prompt
  line <- TIO.getLine
  let trimmed = T.strip line
  if T.null trimmed
    then replLoop repository session state
    else do
      case parseCommand trimmed of
        ExitCommand -> pure ()
        HelpCommand -> do
          mapM_ TIO.putStrLn (renderHelp)
          replLoop repository session state
        cmd -> do
          (session', state', exitGate) <- handleCommand repository cmd session state
          mapM_ TIO.putStrLn (stateOutput state')
          if exitGate
            then TIO.putStrLn "" >> TIO.putStrLn "Mutation complete. Exiting."
            else replLoop repository session' state'

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
-- Discovers the repository, reads commands from stdin (one per line),
-- writes output to stdout. Returns immediately on EOF or after a
-- mutation command (exit gate). Designed for CI testing.
scriptedMode ::
  FilePath ->     -- ^ Repository root
  Actor ->        -- ^ Actor for mutations
  IO ()
scriptedMode repoPath actor = do
  repoResult <- discoverRepository systemGit repoPath
  case repoResult of
    Left err -> do
      TIO.putStrLn $ "Error: cannot open repository: " <> T.pack (show err)
    Right repository -> do
      let session =
            defaultSession
              { sessionRepo = repoPath,
                sessionActor = actor
              }
      let state = initialState
      hSetBuffering stdout LineBuffering
      scriptLoop repository session state

-- | Scripted loop: read line, handle, recurse.
--
-- Returns on EOF or after a mutation command (exit gate).
scriptLoop ::
  Repository -> ExplorerSession -> ExplorerState -> IO ()
scriptLoop repository session state = do
  eof <- hIsEOF stdin
  if eof
    then pure ()
    else do
      line <- TIO.getLine
      let trimmed = T.strip line
      if T.null trimmed
        then scriptLoop repository session state
        else do
          case parseCommand trimmed of
            ExitCommand -> pure ()
            HelpCommand -> do
              mapM_ TIO.putStrLn (renderHelp)
              scriptLoop repository session state
            cmd -> do
              (session', state', exitGate) <- handleCommand repository cmd session state
              mapM_ TIO.putStrLn (stateOutput state')
              when exitGate $ pure ()
              unless exitGate $ scriptLoop repository session' state'

-- ---------------------------------------------------------------------------
-- Command dispatch
-- ---------------------------------------------------------------------------

-- | Handle a command, returning updated session, state, and an exit-gate flag.
--
-- The exit gate is triggered ('True') when a mutation completes successfully,
-- matching the one-shot semantics of create/amend/status commands.
handleCommand ::
  Repository ->
  ExplorerCommand ->
  ExplorerSession ->
  ExplorerState ->
  IO (ExplorerSession, ExplorerState, Bool)
handleCommand repository cmd session state =
  case cmd of
    -- Core commands (no exit gate)
    SearchCommand query ->
      (\(s, st) -> (s, st, False)) <$> handleSearch session state query

    ShowCommand adr ->
      (\(s, st) -> (s, st, False)) <$> handleShow session state adr

    ViewCommand adr mode ->
      (\(s, st) -> (s, st, False)) <$> handleView session state adr mode

    HistoryCommand maybeAdr ->
      (\(s, st) -> (s, st, False)) <$> handleHistory session state maybeAdr

    ConflictsCommand ->
      (\(s, st) -> (s, st, False)) <$> handleConflicts session state

    -- Configuration (no exit gate)
    SetViewCommand vm ->
      (\(s, st) -> (s, st, False)) <$> handleSetView session state vm

    SetModeCommand sm ->
      (\(s, st) -> (s, st, False)) <$> handleSetMode session state sm

    SetObsoleteCommand val ->
      (\(s, st) -> (s, st, False)) <$> handleSetObsolete session state val

    SetRevisionCommand rev ->
      (\(s, st) -> (s, st, False)) <$> handleSetRevision session state rev

    SetFilePathCommand fp ->
      (\(s, st) -> (s, st, False)) <$> handleSetFilePath session state fp

    SetActorCommand act ->
      (\(s, st) -> (s, st, False)) <$> handleSetActor session state act

    -- Mutations (exit gate)
    CreateCommand title body domains ->
      handleCreateMutation repository session state title body domains

    AmendCommand adr title body ->
      handleAmendMutation repository session state adr title body

    StatusCommand adr newStatus ->
      handleStatusMutation repository session state adr newStatus

    ExitCommand ->
      pure (session, state, False)

    HelpCommand ->
      pure (session, state, False)

-- ---------------------------------------------------------------------------
-- Query command handlers (no exit gate)
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

-- ---------------------------------------------------------------------------
-- Mutation command handlers (exit gate)
-- ---------------------------------------------------------------------------

-- | Handle a create mutation: run the mutation via the mutation service,
-- show the result, and signal exit.
handleCreateMutation ::
  Repository ->
  ExplorerSession ->
  ExplorerState ->
  Text ->
  Text ->
  [Domain] ->
  IO (ExplorerSession, ExplorerState, Bool)
handleCreateMutation repository session state title body domains = do
  result <- runMutation session (CreateCommand title body domains)
  pure (session, renderMutationResult state result, True)

-- | Handle an amend mutation: run the mutation via the mutation service,
-- show the result, and signal exit.
handleAmendMutation ::
  Repository ->
  ExplorerSession ->
  ExplorerState ->
  AdrId ->
  Text ->
  Text ->
  IO (ExplorerSession, ExplorerState, Bool)
handleAmendMutation repository session state adr title body = do
  result <- runMutation session (AmendCommand adr title body)
  pure (session, renderMutationResult state result, True)

-- | Handle a status mutation (obsolete/reactivate): run the mutation
-- via the mutation service, show the result, and signal exit.
handleStatusMutation ::
  Repository ->
  ExplorerSession ->
  ExplorerState ->
  AdrId ->
  Text ->
  IO (ExplorerSession, ExplorerState, Bool)
handleStatusMutation repository session state adr newStatus = do
  result <- runMutation session (StatusCommand adr newStatus)
  pure (session, renderMutationResult state result, True)

-- ---------------------------------------------------------------------------
-- Result rendering for mutations
-- ---------------------------------------------------------------------------

-- | Render a mutation result into explorer output state.
renderMutationResult :: ExplorerState -> MutationResult -> ExplorerState
renderMutationResult state result =
  case result of
    CreateMutationResult{..} ->
      ExplorerState
        { stateOutput       =
            [ ansiGreen "✓ ADR created successfully."
            , "  Operation: " <> T.pack resultOperationId
            , "  ADR: " <> ansiBold (T.take 12 (adrIdText resultAdrId))
            , "  Record: " <> ansiBold (T.take 12 (recordIdText resultRecordId))
            , "  Commit: " <> ansiBlue (T.take 16 (gitOidText resultCommitOid))
            , "  Files created: " <> T.pack (show (length resultCreatedPaths))
            , ""
            , ansiCyan "Mutation complete. Use :help for available commands."
            ]
        , stateCursorPosition = 0
        , stateLastResults  = []
        }

    AmendMutationResult{..} ->
      ExplorerState
        { stateOutput       =
            [ ansiGreen "✓ ADR amended successfully."
            , "  Operation: " <> T.pack resultOperationId
            , "  ADR: " <> ansiBold (T.take 12 (adrIdText resultAdrId))
            , "  Record: " <> ansiBold (T.take 12 (recordIdText resultRecordId))
            , "  Commit: " <> ansiBlue (T.take 16 (gitOidText resultCommitOid))
            , "  Updated path: " <> ansiBlue (repoPathText resultUpdatedPath)
            , ""
            , ansiCyan "Mutation complete. Use :help for available commands."
            ]
        , stateCursorPosition = 0
        , stateLastResults  = []
        }

    ObsoleteMutationResult{..} ->
      ExplorerState
        { stateOutput       =
            [ ansiRed "✗ ADR marked obsolete."
            , "  Operation: " <> T.pack resultOperationId
            , "  ADR: " <> ansiBold (T.take 12 (adrIdText resultAdrId))
            , "  Commit: " <> ansiBlue (T.take 16 (gitOidText resultCommitOid))
            , ""
            , ansiCyan "Mutation complete. Use :help for available commands."
            ]
        , stateCursorPosition = 0
        , stateLastResults  = []
        }

    ReactivateMutationResult{..} ->
      ExplorerState
        { stateOutput       =
            [ ansiGreen "✓ ADR reactivated successfully."
            , "  Operation: " <> T.pack resultOperationId
            , "  ADR: " <> ansiBold (T.take 12 (adrIdText resultAdrId))
            , "  Commit: " <> ansiBlue (T.take 16 (gitOidText resultCommitOid))
            , ""
            , ansiCyan "Mutation complete. Use :help for available commands."
            ]
        , stateCursorPosition = 0
        , stateLastResults  = []
        }

    ScopeMutationResult{..} ->
      ExplorerState
        { stateOutput       =
            [ ansiGreen "✓ Scope updated successfully."
            , "  Operation: " <> T.pack resultOperationId
            , "  ADR: " <> ansiBold (T.take 12 (adrIdText resultAdrId))
            , "  Commit: " <> ansiBlue (T.take 16 (gitOidText resultCommitOid))
            , ""
            , ansiCyan "Mutation complete. Use :help for available commands."
            ]
        , stateCursorPosition = 0
        , stateLastResults  = []
        }

    DomainMutationResult{..} ->
      ExplorerState
        { stateOutput       =
            [ ansiGreen "✓ Domain updated successfully."
            , "  Operation: " <> T.pack resultOperationId
            , "  ADR: " <> ansiBold (T.take 12 (adrIdText resultAdrId))
            , "  Commit: " <> ansiBlue (T.take 16 (gitOidText resultCommitOid))
            , ""
            , ansiCyan "Mutation complete. Use :help for available commands."
            ]
        , stateCursorPosition = 0
        , stateLastResults  = []
        }

    MutationError{..} ->
      ExplorerState
        { stateOutput       =
            [ ansiRed ("Error: " <> resultError)
            ]
        , stateCursorPosition = 0
        , stateLastResults  = []
        }
