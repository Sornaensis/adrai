{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Explorer mutations: connect 'ExplorerCommand' to the mutation service.
--
-- This module bridges the explorer command interface with
-- 'Adrei.Service.Mutation' functions, ensuring mutations run inside the
-- 8-stage transaction lock and report results back to the user.
--
-- The "exit gate" pattern: after a successful mutation the explorer shows
-- a summary and terminates (or offers to continue in REPL mode).

module Adrei.Explorer.Mutation
  ( MutationResult (..),
    runMutation,
    defaultManagedPaths,
  )
where

import Adrei.Explorer.Types (ExplorerCommand (..), ExplorerSession (..))
import Adrai.Git
  ( Repository (..),
    runRepository,
    resolveRevision,
    repositoryWorktreeRoot,
    GitHeadState (..),
    repositoryHeadState,
    RevisionSpec (RevisionSpec),
    GitOid (..),
    processExitCode,
    processStdout,
    gitOidText,
    GitError (..),
    GitProcessResult (..),
    discoverRepository,
    systemGit,
  )
import Adrai.Provenance
  ( ProvenanceCapsule (..),
    ProvenanceCapsuleInput (..),
    EventKind,
    mkEventKind,
    semanticDigest,
    ProvenanceError (..),
    ProvenanceObjectId (ProvenanceRecord, ProvenanceConnection),
  )
import Data.Either (fromRight)
import Adrai.Types
  ( AdrId,
    RecordId,
    ConnectionId (..),
    OperationId (..),
    RepoPath (..),
    Digest (..),
    ProvenanceInputs (..),
    Actor (..),
    ActorKind (..),
    ManagedPaths,
    mkManagedPaths,
    mkAdrId,
    mkRecordId,
    recordIdText,
    operationIdText,
    connectionIdText,
    adrIdText,
    mkActor,
    mkRepoPath,
  )
import Adrai.Domain
  ( Domain (..),
    domainText,
  )
import Adrei.Service.Mutation
  ( CreateResult (..),
    createAdrCommand,
    AmendResult (..),
    amendAdmCommand,
    ScopeChangeResult (..),
    changeScopeCommand,
    DomainChangeResult (..),
    changeDomainCommand,
    ObsoleteResult (..),
    obsoleteCommand,
    ReactivateResult (..),
    reactivateCommand,
  )
import Adrai.Service.Transaction
  ( TransactionError (..),
    TransactionResult (..),
  )
import Adrai.Format.Document
  ( ManagedRecord (..),
  )
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (isDigit)

-- | Result of a mutation executed through the explorer.
--
-- Carries the operation outcome so the terminal can display a summary
-- and decide whether to exit cleanly (exit gate).
data MutationResult
  = CreateMutationResult
      { resultOperationId  :: String,
        resultAdrId        :: AdrId,
        resultRecordId     :: RecordId,
        resultCommitOid    :: GitOid,
        resultCreatedPaths :: [RepoPath]
      }
  | AmendMutationResult
      { resultOperationId  :: String,
        resultAdrId        :: AdrId,
        resultRecordId     :: RecordId,
        resultCommitOid    :: GitOid,
        resultUpdatedPath  :: RepoPath
      }
  | ObsoleteMutationResult
      { resultOperationId  :: String,
        resultAdrId        :: AdrId,
        resultConnectionId :: ConnectionId,
        resultCommitOid    :: GitOid,
        resultNewPath      :: RepoPath
      }
  | ReactivateMutationResult
      { resultOperationId  :: String,
        resultAdrId        :: AdrId,
        resultConnectionId :: ConnectionId,
        resultCommitOid    :: GitOid,
        resultNewPath      :: RepoPath
      }
  | ScopeMutationResult
      { resultOperationId  :: String,
        resultAdrId        :: AdrId,
        resultConnectionId :: ConnectionId,
        resultCommitOid    :: GitOid,
        resultNewPath      :: RepoPath
      }
  | DomainMutationResult
      { resultOperationId  :: String,
        resultAdrId        :: AdrId,
        resultConnectionId :: ConnectionId,
        resultCommitOid    :: GitOid,
        resultNewPath      :: RepoPath
      }
  | MutationError
      { resultError :: Text
      } deriving (Show)

-- | Default managed paths for the explorer (matching the library default).
--
-- The paths are chosen so they cannot overlap, avoiding the
-- 'ManagedPathsViolation'.
defaultManagedPaths :: ManagedPaths
defaultManagedPaths =
  case mkManagedPaths
    (RepoPath "architecture/adrai/decisions")
    (RepoPath "architecture/adrai/connections")
  of
    Right mp -> mp
    Left _   -> error "defaultManagedPaths: paths should not overlap"

-- | Run an 'ExplorerCommand' that performs a mutation.
--
-- The function:
-- 1. Opens the repository at the session's repo path.
-- 2. Dispatches the appropriate mutation service function.
-- 3. Returns a 'MutationResult' with success/failure information.
--
-- The lock acquisition and release are handled internally by the
-- underlying 'Adrei.Service.Mutation' functions via
-- 'Adrei.Provenance.Git.Lock.withGitLock'.
runMutation ::
  ExplorerSession ->  -- ^ Session (provides repo path and actor)
  ExplorerCommand ->  -- ^ Mutation command to execute
  IO MutationResult
runMutation session cmd =
  case cmd of
    CreateCommand title body domains -> do
      repo <- openRepository (sessionRepo session)
      case repo of
        Left err -> pure (MutationError ("open repository: " <> err))
        Right repository -> do
          result <- executeCreate repository session title body domains
          pure result

    AmendCommand adr title body -> do
      repo <- openRepository (sessionRepo session)
      case repo of
        Left err -> pure (MutationError ("open repository: " <> err))
        Right repository -> do
          result <- executeAmend repository session adr title body
          pure result

    StatusCommand adr newStatus -> do
      repo <- openRepository (sessionRepo session)
      case repo of
        Left err -> pure (MutationError ("open repository: " <> err))
        Right repository -> do
          result <- executeStatus repository session adr newStatus
          pure result

    _ -> pure (MutationError "not a mutation command")

-- ---------------------------------------------------------------------------
-- Repository open
-- ---------------------------------------------------------------------------

-- | Discover a Git repository at the given path.
openRepository :: FilePath -> IO (Either Text Repository)
openRepository path = do
  result <- discoverRepository systemGit path
  pure $ case result of
    Left err -> Left ("cannot open repository: " <> T.pack (show err))
    Right repo ->
      case repositoryWorktreeRoot repo of
        Nothing -> Left "not a valid worktree repository"
        Just _  -> Right repo

-- ---------------------------------------------------------------------------
-- Create
-- ---------------------------------------------------------------------------

-- | Execute a create mutation via the mutation service.
executeCreate ::
  Repository ->
  ExplorerSession ->
  Text ->
  Text ->
  [Domain] ->
  IO MutationResult
executeCreate repository session title body domains = do
  let actor = sessionActor session
      managedPaths = defaultManagedPaths
      adrId  = fromRight (error "invalid ADR ID") (mkAdrId "A00000000000000000000000000")
      recordId = fromRight (error "invalid record ID") (mkRecordId "R00000000000000000000000000")
  result <-
    createAdrCommand
      repository
      managedPaths
      actor
      adrId
      recordId
      title
      body
      body
      domains
      []
      Nothing
      Nothing
      Nothing
  case result of
    Left txErr ->
      pure (MutationError ("create: " <> T.pack (show txErr)))
    Right createRes ->
      pure (CreateMutationResult
        { resultOperationId  = createOperationId createRes,
          resultAdrId        = createAdrId createRes,
          resultRecordId     = createRecordId createRes,
          resultCommitOid    = createCommitOid createRes,
          resultCreatedPaths = createCreatedPaths createRes
        })

-- ---------------------------------------------------------------------------
-- Amend
-- ---------------------------------------------------------------------------

executeAmend ::
  Repository ->
  ExplorerSession ->
  AdrId ->
  Text ->
  Text ->
  IO MutationResult
executeAmend repository session adr newTitle newBody = do
  let actor = sessionActor session
      managedPaths = defaultManagedPaths
      recordId = fromRight (error "invalid record ID") (mkRecordId "R00000000000000000000000000")
      inputs = ProvenanceInputs Nothing Nothing Nothing
  result <-
    amendAdmCommand
      repository
      managedPaths
      actor
      adr
      recordId
      newTitle
      newBody
      newBody
      inputs
  case result of
    Left txErr ->
      pure (MutationError ("amend: " <> T.pack (show txErr)))
    Right amendRes ->
      pure (AmendMutationResult
        { resultOperationId  = amendOperationId amendRes,
          resultAdrId        = amendAdrId amendRes,
          resultRecordId     = amendRecordId amendRes,
          resultCommitOid    = amendCommitOid amendRes,
          resultUpdatedPath  = amendUpdatedPath amendRes
        })

-- ---------------------------------------------------------------------------
-- Status (obsolete / reactivate)
-- ---------------------------------------------------------------------------

executeStatus ::
  Repository ->
  ExplorerSession ->
  AdrId ->
  Text ->
  IO MutationResult
executeStatus repository session adr newStatus = do
  let actor = sessionActor session
      managedPaths = defaultManagedPaths
      recordId = fromRight (error "invalid record ID") (mkRecordId "R00000000000000000000000000")
      inputs = ProvenanceInputs Nothing Nothing Nothing
  case T.toLower newStatus of
    "obsolete" -> do
      result <-
        obsoleteCommand
          repository
          managedPaths
          actor
          adr
          recordId
          inputs
      case result of
        Left txErr ->
          pure (MutationError ("obsolete: " <> T.pack (show txErr)))
        Right obsRes ->
          pure (ObsoleteMutationResult
            { resultOperationId  = obsoleteOperationId obsRes,
              resultAdrId        = obsoleteAdrId obsRes,
              resultConnectionId = obsoleteConnectionId obsRes,
              resultCommitOid    = obsoleteCommitOid obsRes,
              resultNewPath      = obsoleteNewPath obsRes
            })
    "active" -> do
      result <-
        reactivateCommand
          repository
          managedPaths
          actor
          adr
          recordId
          inputs
      case result of
        Left txErr ->
          pure (MutationError ("reactivate: " <> T.pack (show txErr)))
        Right reactRes ->
          pure (ReactivateMutationResult
            { resultOperationId  = reactivateOperationId reactRes,
              resultAdrId        = reactivateAdrId reactRes,
              resultConnectionId = reactivateConnectionId reactRes,
              resultCommitOid    = reactivateCommitOid reactRes,
              resultNewPath      = reactivateNewPath reactRes
            })
    _ ->
      pure (MutationError ("unknown status: " <> newStatus))
