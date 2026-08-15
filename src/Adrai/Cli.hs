{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | CLI-facing result types (compile, doctor) and their JSON projections.
-- Command types and their JSON projections are defined in
-- "Adrai.CliTypes" to avoid a dependency cycle with "Adrai.CliRunner".
module Adrai.Cli
  ( CompileResult (..),
    coldCompilerToCompileResult,
    compileResultJson,
    DoctorOutput (..),
    DoctorIssue (..),
    DoctorCounts (..),
    DoctorCacheAccess (..),
    DoctorDatabaseBuild (..),
    doctorOutputJson,
    doctorIssueJson,
    doctorCountsJson,
    doctorCacheAccessJson,
    doctorDatabaseBuildJson,
    ShowCommand (..),
    showCommandJson,
    HistoryCommand (..),
    historyCommandJson,
    SearchCommand (..),
    searchCommandJson,
    RelevantCommand (..),
    relevantCommandJson,
    CompareCommand (..),
    compareCommandJson,
    toAesonValue,
    textToActorKind,
    HistoryOrder (..),
    CliCommand (..),
    CliParser,
    run,
  )
where

import qualified Adrai.CliRunner as CliRunner
import Adrai.CliRunner (CliCommand (..))
import Adrai.CliTypes
  ( CompileResult (..),
    coldCompilerToCompileResult,
    compileResultJson,
    DoctorOutput (..),
    DoctorIssue (..),
    DoctorCounts (..),
    DoctorCacheAccess (..),
    DoctorDatabaseBuild (..),
    doctorOutputJson,
    doctorIssueJson,
    doctorCountsJson,
    doctorCacheAccessJson,
    doctorDatabaseBuildJson,
    ShowCommand (..),
    showCommandJson,
    HistoryCommand (..),
    historyCommandJson,
    SearchCommand (..),
    searchCommandJson,
    RelevantCommand (..),
    relevantCommandJson,
    CompareCommand (..),
    compareCommandJson,
    toAesonValue,
    textToActorKind,
  )
import Adrai.History (HistoryOrder (..))

-- | Type alias re-exported from CliRunner for CLI-facing code.
type CliParser = CliRunner.CliParser

-- | CLI runner: delegates to Adrai.CliRunner for parsing and dispatch.
run :: IO ()
run = CliRunner.run
