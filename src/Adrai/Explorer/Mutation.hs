{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Checked explorer writes consume the session's reviewed basis and ADR token.
module Adrai.Explorer.Mutation (MutationResult (..), runMutation) where
import Adrai.Explorer.Types
import Adrai.Git (GitOid, discoverRepository, systemGit)
import Adrai.Service.Mutation
import Adrai.Types (AdrId, RecordId, RepoPath, ProvenanceInputs (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

data MutationResult
  = MutationResult
      { resultKind :: Text, resultOperationId :: String, resultAdrId :: AdrId,
        resultRecordId :: Maybe RecordId, resultCommitOid :: GitOid,
        resultCreatedPaths :: [RepoPath], resultWarnings :: [Text] }
  | MutationError { resultError :: Text }
  deriving (Show)

runMutation :: ExplorerSession -> ExplorerCommand -> IO MutationResult
runMutation session _ | sessionRevision session /= "HEAD" =
  pure (MutationError "Selected revisions are read-only. Use :revision HEAD, then view the ADR again.")
runMutation session command = case (sessionBasis session, sessionManagedPaths session) of
  (Just basis, Just paths) -> do
    opened <- discoverRepository systemGit (sessionRepo session)
    case opened of
      Left problem -> pure (MutationError (T.pack (show problem)))
      Right repository -> execute basis paths repository
  _ -> pure (MutationError "No reviewed repository basis. Use :refresh or :revision HEAD.")
  where
    actor = sessionActor session
    inputs = ProvenanceInputs Nothing Nothing Nothing
    viewed adr action = case Map.lookup adr (sessionViewedStates session) of
      Nothing -> pure (MutationError "View this ADR with show/view before editing it; use :refresh to adopt changed HEAD.")
      Just token -> action token
    execute basis paths repository = case command of
      CreateCommand MutationDraft{..} -> do
        result <- createAdrCommandAutoChecked basis repository paths actor draftTitle draftSummary draftBody draftDomains draftScopes Nothing Nothing Nothing
        pure $ either failure (\r -> MutationResult "created" (createOperationId r) (createAdrId r) (Just (createRecordId r)) (createCommitOid r) (createCreatedPaths r) (warnings (createIndexUpdated r) (createPublicationError r))) result
      AmendCommand adr MutationDraft{..} -> viewed adr $ \token -> do
        result <- amendCurrentAdrCommandChecked basis repository actor adr (Just token) draftChangeSummary draftTitle draftSummary draftBody inputs
        pure $ either failure (\r -> MutationResult "amended" (amendOperationId r) (amendAdrId r) (Just (amendRecordId r)) (amendCommitOid r) (amendCreatedPaths r) (warnings (amendIndexUpdated r) (amendPublicationError r))) result
      StatusCommand adr "obsolete" -> viewed adr $ \token -> do
        result <- obsoleteCommandChecked basis repository paths actor adr (ObsoleteRequest (Just token) "Marked obsolete in terminal explorer" False Nothing) inputs
        pure $ either failure (\r -> MutationResult "marked obsolete" (obsoleteOperationId r) (obsoleteAdrId r) Nothing (obsoleteCommitOid r) (obsoleteCreatedPaths r) (warnings (obsoleteIndexUpdated r) (obsoletePublicationError r))) result
      StatusCommand adr "active" -> viewed adr $ \token -> do
        result <- reactivateCommandChecked basis repository paths actor adr (ReactivateRequest (Just token) "Reactivated in terminal explorer" False) inputs
        pure $ either failure (\r -> MutationResult "reactivated" (reactivateOperationId r) (reactivateAdrId r) Nothing (reactivateCommitOid r) (reactivateCreatedPaths r) (warnings (reactivateIndexUpdated r) (reactivatePublicationError r))) result
      _ -> pure (MutationError "Not a supported mutation command")
    failure problem = MutationError (T.pack (show problem))
    warnings indexUpdated publication =
      ["Commit succeeded, but the caller's Git index was not updated." | not indexUpdated]
      <> maybe [] (\message -> ["Commit succeeded; publication warning: " <> message]) publication
