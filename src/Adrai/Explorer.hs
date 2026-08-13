-- | Terminal explorer for ADRAI.
--
-- Provides an interactive REPL and scripted mode for querying and mutating
-- architectural decision records. All query and mutation work is delegated
-- to the shared typed services ('Adrai.Query', 'Adrai.Service.Mutation',
-- 'Adrai.Graph') — no duplicate reducer / compiler / transaction path.
--
-- Sub-modules:
--
-- * 'Adrai.Explorer.Types' — core types (session, commands, state)
-- * 'Adrai.Explorer.Render' — terminal rendering with ANSI colors
-- * 'Adrai.Explorer.Mutation' — mutation command runner (exit gate)
-- * 'Adrai.Explorer.Interactive' — REPL loop and scripted mode

module Adrai.Explorer
  ( -- * Types
    ExplorerSession (..),
    defaultSession,
    ExplorerCommand (..),
    ExplorerState (..),
    ViewMode (..),
    SearchMode (..),
    SearchFilter (..),

    -- * Rendering
    ansiBold,
    ansiCyan,
    ansiGreen,
    ansiRed,
    ansiReset,
    ansiYellow,
    renderCollapsed,
    renderExploded,
    renderHistory,
    renderHelp,
    renderSearchResults,
    renderConflict,
    terminalWidth,
    wrapText,

    -- * Interactive
    interactiveSession,
    scriptedMode,

    -- * Mutation
    MutationResult (..),
    runMutation,
  )
  where

import Adrai.Explorer.Types
  ( ExplorerSession (..),
    defaultSession,
    ExplorerCommand (..),
    ExplorerState (..),
    ViewMode (..),
    SearchMode (..),
    SearchFilter (..),
  )
import Adrai.Explorer.Render
  ( ansiBold,
    ansiCyan,
    ansiGreen,
    ansiRed,
    ansiReset,
    ansiYellow,
    renderCollapsed,
    renderExploded,
    renderHistory,
    renderHelp,
    renderSearchResults,
    renderConflict,
    terminalWidth,
    wrapText,
  )
import Adrai.Explorer.Interactive
  ( interactiveSession,
    scriptedMode,
  )
import Adrai.Explorer.Mutation
  ( MutationResult (..),
    runMutation,
  )
