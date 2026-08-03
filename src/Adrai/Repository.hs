{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Immutable, raw repository observations at one resolved commit.
--
-- This module deliberately stops before managed-document parsing, graph
-- reduction, provenance, caches, SQLite, queries, and public rendering.
module Adrai.Repository
  ( RepositoryRevisionKey,
    repositoryRevisionNamespace,
    repositoryRevisionOid,
    ResolvedRepositoryRevision,
    resolvedRepository,
    resolvedRequestedRevision,
    resolvedCommitOid,
    resolvedRevisionKey,
    RepositoryConfigOrigin (..),
    RepositoryConfigObservation,
    repositoryConfigOrigin,
    repositoryObservedConfig,
    repositoryObservedManagedPaths,
    repositoryConfigEntry,
    repositoryConfigBlob,
    RepositoryTreeObservation,
    repositoryTreeEntry,
    repositoryTreeBlob,
    RepositorySnapshot,
    repositorySnapshotRevision,
    repositorySnapshotConfig,
    repositorySnapshotManagedPaths,
    repositorySnapshotEntries,
    RepositorySnapshotError (..),
    resolveRepositoryRevision,
    repositorySnapshotAt,
    repositorySnapshot,
    isManagedSourcePath,
  )
where

import Adrai.Format.Config (ConfigParseError, parseConfigText)
import Adrai.Git
import Adrai.Types
  ( Config,
    ManagedPaths,
    RepoPath,
    configManagedPaths,
    defaultConfig,
    managedConnectionPath,
    managedDecisionPath,
    mkRepoPath,
    repoPathText,
  )
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (sortOn)
import qualified Data.Set as Set
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

data RepositoryRevisionKey = RepositoryRevisionKey
  { repositoryRevisionNamespace :: FilePath,
    repositoryRevisionOid :: GitOid
  }
  deriving (Eq, Ord, Show)

data ResolvedRepositoryRevision = ResolvedRepositoryRevision
  { resolvedRepository :: Repository,
    resolvedRequestedRevision :: RevisionSpec,
    resolvedCommitOid :: GitOid,
    resolvedRevisionKey :: RepositoryRevisionKey
  }
  deriving (Eq, Show)

data RepositoryConfigOrigin
  = DefaultConfigOrigin
  | CommittedConfigOrigin
  deriving (Eq, Ord, Show)

data RepositoryConfigObservation = RepositoryConfigObservation
  { repositoryConfigOrigin :: RepositoryConfigOrigin,
    repositoryObservedConfig :: Config,
    repositoryObservedManagedPaths :: ManagedPaths,
    repositoryConfigEntry :: Maybe GitTreeEntry,
    repositoryConfigBlob :: Maybe GitBlob
  }
  deriving (Eq, Show)

data RepositoryTreeObservation = RepositoryTreeObservation
  { repositoryTreeEntry :: GitTreeEntry,
    repositoryTreeBlob :: Maybe GitBlob
  }
  deriving (Eq, Show)

data RepositorySnapshot = RepositorySnapshot
  { repositorySnapshotRevision :: ResolvedRepositoryRevision,
    repositorySnapshotConfig :: RepositoryConfigObservation,
    repositorySnapshotManagedPaths :: ManagedPaths,
    repositorySnapshotEntries :: [RepositoryTreeObservation]
  }
  deriving (Eq, Show)

data RepositorySnapshotError
  = RepositorySnapshotGitError GitError
  | RepositorySnapshotConfigInvalidUtf8 GitOid
  | RepositorySnapshotConfigParseError GitOid ConfigParseError
  | RepositorySnapshotConfigNotBlob GitTreeEntry
  | RepositorySnapshotContradictoryPath RepoPath [GitTreeEntry]
  | RepositorySnapshotMissingBatchBlob GitOid
  deriving (Eq, Show)

resolveRepositoryRevision :: Repository -> RevisionSpec -> IO (Either RepositorySnapshotError ResolvedRepositoryRevision)
resolveRepositoryRevision repository requested = do
  resolved <- resolveRevision repository requested
  pure $ do
    commitOid <- first RepositorySnapshotGitError resolved
    Right
      ResolvedRepositoryRevision
        { resolvedRepository = repository,
          resolvedRequestedRevision = requested,
          resolvedCommitOid = commitOid,
          resolvedRevisionKey = RepositoryRevisionKey (repositoryCommonDir repository) commitOid
        }

repositorySnapshot :: Repository -> RevisionSpec -> IO (Either RepositorySnapshotError RepositorySnapshot)
repositorySnapshot repository requested = do
  resolved <- resolveRepositoryRevision repository requested
  case resolved of
    Left problem -> pure (Left problem)
    Right revision -> repositorySnapshotAt revision

repositorySnapshotAt :: ResolvedRepositoryRevision -> IO (Either RepositorySnapshotError RepositorySnapshot)
repositorySnapshotAt revision = do
  configResult <- observeConfig revision
  case configResult of
    Left problem -> pure (Left problem)
    Right configObservation -> do
      entriesResult <- observeManagedEntries revision (repositoryObservedManagedPaths configObservation)
      pure $ do
        entries <- entriesResult
        Right
          RepositorySnapshot
            { repositorySnapshotRevision = revision,
              repositorySnapshotConfig = configObservation,
              repositorySnapshotManagedPaths = repositoryObservedManagedPaths configObservation,
              repositorySnapshotEntries = entries
            }

observeConfig :: ResolvedRepositoryRevision -> IO (Either RepositorySnapshotError RepositoryConfigObservation)
observeConfig revision = do
  entryResult <- lookupTreeEntryAt repository commitOid configPath
  case entryResult of
    Left problem -> pure (Left (RepositorySnapshotGitError problem))
    Right Nothing ->
      pure
        ( Right
            RepositoryConfigObservation
              { repositoryConfigOrigin = DefaultConfigOrigin,
                repositoryObservedConfig = defaultConfig,
                repositoryObservedManagedPaths = configManagedPaths defaultConfig,
                repositoryConfigEntry = Nothing,
                repositoryConfigBlob = Nothing
              }
        )
    Right (Just entry)
      | gitTreeObjectType entry /= GitBlobObject -> pure (Left (RepositorySnapshotConfigNotBlob entry))
      | otherwise -> do
          blobsResult <- readBlobBatch repository [gitTreeOid entry]
          pure $ do
            blobs <- first RepositorySnapshotGitError blobsResult
            blob <- maybe (Left (RepositorySnapshotMissingBatchBlob (gitTreeOid entry))) Right (Map.lookup (gitTreeOid entry) blobs)
            configText <- first (const (RepositorySnapshotConfigInvalidUtf8 (gitBlobOid blob))) (TextEncoding.decodeUtf8' (gitBlobBytes blob))
            config <- first (RepositorySnapshotConfigParseError (gitBlobOid blob)) (parseConfigText configText)
            Right
              RepositoryConfigObservation
                { repositoryConfigOrigin = CommittedConfigOrigin,
                  repositoryObservedConfig = config,
                  repositoryObservedManagedPaths = configManagedPaths config,
                  repositoryConfigEntry = Just entry,
                  repositoryConfigBlob = Just blob
                }
  where
    repository = resolvedRepository revision
    commitOid = resolvedCommitOid revision

observeManagedEntries :: ResolvedRepositoryRevision -> ManagedPaths -> IO (Either RepositorySnapshotError [RepositoryTreeObservation])
observeManagedEntries revision paths = do
  listed <- listTreeEntriesAt repository commitOid roots
  case first RepositorySnapshotGitError listed >>= validateSelectedEntries . filter (isSelectedManagedPath paths . gitTreePath) of
    Left problem -> pure (Left problem)
    Right selected -> do
      let blobIds =
            Set.fromList
              [ gitTreeOid entry
                | entry <- selected,
                  gitTreeObjectType entry == GitBlobObject
              ]
      blobsResult <- readBlobBatch repository (Set.toList blobIds)
      pure $ do
        blobs <- first RepositorySnapshotGitError blobsResult
        traverse (assembleObservation blobs) selected
  where
    repository = resolvedRepository revision
    commitOid = resolvedCommitOid revision
    roots = [managedDecisionPath paths, managedConnectionPath paths]

assembleObservation :: Map GitOid GitBlob -> GitTreeEntry -> Either RepositorySnapshotError RepositoryTreeObservation
assembleObservation blobs entry
  | gitTreeObjectType entry == GitBlobObject = do
      blob <- maybe (Left (RepositorySnapshotMissingBatchBlob (gitTreeOid entry))) Right (Map.lookup (gitTreeOid entry) blobs)
      Right (RepositoryTreeObservation entry (Just blob))
  | otherwise = Right (RepositoryTreeObservation entry Nothing)

validateSelectedEntries :: [GitTreeEntry] -> Either RepositorySnapshotError [GitTreeEntry]
validateSelectedEntries entries = do
  traverse_ validateGroup (Map.toAscList grouped)
  Right (sortOn (repoPathText . gitTreePath) (mapMaybe listToMaybe (Map.elems grouped)))
  where
    grouped = Map.fromListWith (<>) [(gitTreePath entry, [entry]) | entry <- entries]
    validateGroup (path, candidates) =
      case candidates of
        [] -> Right ()
        firstEntry : remaining
          | all (== firstEntry) remaining -> Right ()
          | otherwise -> Left (RepositorySnapshotContradictoryPath path candidates)

isManagedSourcePath :: RepoPath -> Bool
isManagedSourcePath path =
  ".decision.md" `Text.isSuffixOf` value
    || ".connection.md" `Text.isSuffixOf` value
  where
    value = repoPathText path

isSelectedManagedPath :: ManagedPaths -> RepoPath -> Bool
isSelectedManagedPath paths path =
  (isUnder (managedDecisionPath paths) path && ".decision.md" `Text.isSuffixOf` value)
    || (isUnder (managedConnectionPath paths) path && ".connection.md" `Text.isSuffixOf` value)
  where
    value = repoPathText path
    isUnder root candidate = (repoPathText root <> "/") `Text.isPrefixOf` repoPathText candidate

configPath :: RepoPath
configPath =
  case mkRepoPath ".adrai.toml" of
    Left problem -> error ("invalid built-in configuration path: " <> show problem)
    Right path -> path

traverse_ :: (value -> Either error ()) -> [value] -> Either error ()
traverse_ _ [] = Right ()
traverse_ action (value : remaining) = action value >> traverse_ action remaining
