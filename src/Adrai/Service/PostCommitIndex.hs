{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

-- | Disposable SQLite projection of an already committed repository revision.
--
-- This module deliberately has no transaction or cache-publication policy.  A
-- mutation caller supplies the commit that already succeeded and the exact
-- caller-owned database path to materialize.
module Adrai.Service.PostCommitIndex
  ( IndexWarning (..),
    PostCommitIndexError (..),
    PostCommitIndexResult (..),
    PostCommitIndexDependencies (..),
    postCommitIndexDependencies,
    compilePostCommitIndex,
    compilePostCommitIndexWith,
  )
where

import Adrai.Compiler
  ( ColdCompilerError,
    ColdCompilerResult (..),
    coldCompileRepository,
  )
import Adrai.Compiler.Snapshot
  ( CompilerDiagnostic (..),
    CompilerDiagnosticSeverity (CompilerDiagnosticWarning),
    analyzedDiagnostics,
    compilerDiagnosticCodeText,
  )
import Adrai.Git
  ( GitOid,
    Repository,
    RevisionSpec (RevisionSpec),
    gitOidText,
  )
import Adrai.Repository
  ( RepositorySnapshotError,
    ResolvedRepositoryRevision,
    resolveRepositoryRevision,
    resolvedCommitOid,
  )
import Control.Exception (SomeException, displayException, try)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple (Connection, close, open)

-- | A stable public projection of a compiler warning.  Compiler diagnostics
-- retain their richer internal provenance; callers of this result only need a
-- deterministic code/message pair.
data IndexWarning = IndexWarning
  { indexWarningCode :: Text,
    indexWarningMessage :: Text
  }
  deriving (Eq, Ord, Show)

-- | Actual failures that can occur after a successful Git mutation.  These
-- failures are data, never transaction errors, because no rollback is valid
-- after the commit is durable.
data PostCommitIndexError
  = PostCommitIndexResolveFailure RepositorySnapshotError
  | PostCommitIndexRevisionMismatch GitOid GitOid
  | PostCommitIndexOpenFailure Text
  | PostCommitIndexCompileFailure ColdCompilerError
  | PostCommitIndexCompileException Text
  | PostCommitIndexCloseFailure Text
  | PostCommitIndexMultipleFailures [PostCommitIndexError]
  deriving (Eq, Show)

-- | Truthful result of a disposable cold compilation.  Success-only fields
-- are absent on every failure, including a close failure.
data PostCommitIndexResult = PostCommitIndexResult
  { postCommitIndexed :: Bool,
    postCommitDatabase :: Maybe FilePath,
    postCommitIndexRevision :: Maybe GitOid,
    postCommitIndexWarnings :: [IndexWarning],
    postCommitIndexError :: Maybe PostCommitIndexError
  }
  deriving (Eq, Show)

-- | Narrow IO boundary used by the production entry point and focused tests.
-- The parameterized function cannot affect mutation transaction semantics.
data PostCommitIndexDependencies = PostCommitIndexDependencies
  { postCommitResolveRevision :: Repository -> GitOid -> IO (Either RepositorySnapshotError ResolvedRepositoryRevision),
    postCommitOpenDatabase :: FilePath -> IO Connection,
    postCommitColdCompile :: Connection -> ResolvedRepositoryRevision -> IO (Either ColdCompilerError ColdCompilerResult),
    postCommitCloseDatabase :: Connection -> IO ()
  }

postCommitIndexDependencies :: PostCommitIndexDependencies
postCommitIndexDependencies =
  PostCommitIndexDependencies
    { postCommitResolveRevision = \repository commitOid -> resolveRepositoryRevision repository (RevisionSpec (gitOidText commitOid)),
      postCommitOpenDatabase = open,
      postCommitColdCompile = coldCompileRepository,
      postCommitCloseDatabase = close
    }

-- | Materialize the exact supplied commit into the exact caller-selected
-- SQLite path.  The resolved full commit must agree with the mutation commit
-- before compilation begins.
compilePostCommitIndex :: Repository -> GitOid -> FilePath -> IO PostCommitIndexResult
compilePostCommitIndex = compilePostCommitIndexWith postCommitIndexDependencies

-- | Parameterized form with deterministic failure composition.  If compilation
-- and close both fail, errors are retained in lifecycle order: compile, close.
compilePostCommitIndexWith :: PostCommitIndexDependencies -> Repository -> GitOid -> FilePath -> IO PostCommitIndexResult
compilePostCommitIndexWith dependencies repository commitOid databasePath = do
  resolved <- postCommitResolveRevision dependencies repository commitOid
  case resolved of
    Left problem -> pure (failure (PostCommitIndexResolveFailure problem))
    Right revision
      | resolvedCommitOid revision /= commitOid ->
          pure (failure (PostCommitIndexRevisionMismatch commitOid (resolvedCommitOid revision)))
      | otherwise -> compileAt revision
  where
    compileAt :: ResolvedRepositoryRevision -> IO PostCommitIndexResult
    compileAt revision = do
      opened <- try @SomeException (postCommitOpenDatabase dependencies databasePath)
      case opened of
        Left exception -> pure (failure (PostCommitIndexOpenFailure (Text.pack (displayException exception))))
        Right connection -> do
          compiled <- try @SomeException (postCommitColdCompile dependencies connection revision)
          closed <- try @SomeException (postCommitCloseDatabase dependencies connection)
          complete revision compiled closed

    complete revision compiled closed =
      case (compileOutcome compiled, closeOutcome closed) of
        (Left compileError, Left closeError) ->
          pure (failure (PostCommitIndexMultipleFailures [compileError, closeError]))
        (Left compileError, Right ()) -> pure (failure compileError)
        (Right _, Left closeError) -> pure (failure closeError)
        (Right result, Right ()) ->
          pure
            PostCommitIndexResult
              { postCommitIndexed = True,
                postCommitDatabase = Just databasePath,
                postCommitIndexRevision = Just (resolvedCommitOid revision),
                postCommitIndexWarnings = compilerWarnings result,
                postCommitIndexError = Nothing
              }

    compileOutcome = \case
      Left exception -> Left (PostCommitIndexCompileException (Text.pack (displayException exception)))
      Right (Left problem) -> Left (PostCommitIndexCompileFailure problem)
      Right (Right result) -> Right result

    closeOutcome = \case
      Left exception -> Left (PostCommitIndexCloseFailure (Text.pack (displayException exception)))
      Right () -> Right ()

    failure problem =
      PostCommitIndexResult
        { postCommitIndexed = False,
          postCommitDatabase = Nothing,
          postCommitIndexRevision = Nothing,
          postCommitIndexWarnings = [],
          postCommitIndexError = Just problem
        }

compilerWarnings :: ColdCompilerResult -> [IndexWarning]
compilerWarnings result =
  sortOn (\warning -> (indexWarningCode warning, indexWarningMessage warning))
    [ IndexWarning
        { indexWarningCode = compilerDiagnosticCodeText (compilerDiagnosticCode diagnostic),
          indexWarningMessage = compilerDiagnosticMessage diagnostic
        }
    | diagnostic <- analyzedDiagnostics (coldCompilerAnalyzed result),
      compilerDiagnosticSeverity diagnostic == CompilerDiagnosticWarning
    ]
