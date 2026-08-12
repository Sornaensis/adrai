-- | Terminal explorer for ADRAI.
--
-- Provides an interactive REPL and scripted mode for querying and mutating
-- architectural decision records. All query and mutation work is delegated
-- to the shared typed services ('Adrei.Query', 'Adrei.Service.Mutation',
-- 'Adrei.Graph') — no duplicate reducer / compiler / transaction path.
--
-- Sub-modules:
--
-- * 'Adrei.Explorer.Types' — core types (session, commands, state)
-- * 'Adrei.Explorer.Render' — terminal rendering with ANSI colors
-- * 'Adrei.Explorer.Mutation' — mutation command runner (exit gate)
-- * 'Adrei.Explorer.Interactive' — REPL loop and scripted mode

module Adrei.Explorer
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

import Adrei.Explorer.Types
  ( ExplorerSession (..),
    defaultSession,
    ExplorerCommand (..),
    ExplorerState (..),
    ViewMode (..),
    SearchMode (..),
    SearchFilter (..),
  )
import Adrei.Explorer.Render
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
import Adrei.Explorer.Interactive
  ( interactiveSession,
    scriptedMode,
  )
import Adrei.Explorer.Mutation
  ( MutationResult (..),
    runMutation,
  )
