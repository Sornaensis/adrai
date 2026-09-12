{-# LANGUAGE StrictData #-}

-- | Injectable repository-fact observation boundary.  This deliberately
-- shares no name or implementation with 'Adrai.Repository.repositorySnapshot'.
module Adrai.Web.Watch
  ( RepositoryFacts (..),
    RepositorySnapshot (..),
    ObservationFailure (..),
    RepositoryEvent (..),
    WatchHandle (..),
    Observer (..),
  )
where

import Adrai.Provenance (GitOid)
import Adrai.Web.Api (Repo)
import Adrai.Web.Events (Invalidation)
import Data.Text (Text)

data RepositoryFacts = RepositoryFacts
  { factsHead :: Maybe GitOid,
    factsIndexIdentity :: Maybe Text,
    factsSequencerActive :: Bool,
    factsConfigurationIdentity :: Maybe Text,
    factsManagedSourceIdentity :: Maybe Text,
    factsCommonRefsIdentity :: Maybe Text,
    factsPackedRefsIdentity :: Maybe Text,
    factsReflogsIdentity :: Maybe Text,
    factsWorktreeMetadataIdentity :: Maybe Text,
    factsRelevantWorktreeIdentity :: Maybe Text
  }
  deriving (Eq, Show)

data ObservationFailure
  = ObservationGitFailure Text
  | ObservationPathFailure Text
  | ObservationVerificationFailure Text
  deriving (Eq, Show)

data RepositorySnapshot
  = RepositorySnapshot RepositoryFacts
  | RepositorySnapshotFailed ObservationFailure
  deriving (Eq, Show)

data RepositoryEvent
  = RepositoryFactsChanged RepositorySnapshot [Invalidation]
  | RepositoryObservationFailure ObservationFailure
  deriving (Eq, Show)

-- | Runtime ownership stays with P7-03; the handle contains finite cleanup
-- actions rather than an undefined placeholder implementation.
data WatchHandle = WatchHandle
  { stopWatching :: IO (),
    awaitWatcher :: IO ()
  }

-- | Exact facade consumed by the server/runtime increments.
data Observer = Observer
  { repositorySnapshot :: Repo -> IO RepositorySnapshot,
    watchRepository :: Repo -> (RepositoryEvent -> IO ()) -> IO WatchHandle
  }
