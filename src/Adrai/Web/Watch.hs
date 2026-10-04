{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}

-- | Fact-only repository observer. Filesystem events are bounded hints; a
-- periodic worker always verifies stable content fingerprints.
module Adrai.Web.Watch
  ( RepositoryFacts (..), RepositorySnapshot (..), ObservationFailure (..), ObservationEpoch, RepositoryEvent (..), WatchHandle (..), Observer (..),
    ActiveFileRegistry, ActiveClientId, newActiveFileRegistry, registerActiveClient, replaceActiveFiles, unregisterActiveClient,
    activeFileUnion, observationEpochMatches, observerForRegistry, observerForRegistryWithHandleHook, nativeNameLengthAcceptedForTest, diffRepositoryFacts,
  ) where

import Adrai.Format.Config (parseConfigText)
import Adrai.Git (GitHeadState, GitOid, RevisionSpec (..), repositoryHeadState, resolveRevision)
import Adrai.Provenance (encodeBase64Url, sha256Digest)
import Adrai.Types (Config (..), ManagedPaths (..), RepoPath, defaultConfig, digestBytes, repoPathText)
import Adrai.Web.Api (Repo (..))
import Adrai.Web.Events (Invalidation (..))
import Adrai.Web.Watch.Native
import qualified Adrai.Web.Events as Events
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (asyncWithUnmask, cancel, waitCatch)
import Control.Concurrent.MVar (MVar, newEmptyMVar, tryPutMVar, tryTakeMVar)
import Control.Concurrent.STM
import Control.Exception (SomeAsyncException, SomeException, bracket, displayException, finally, fromException, mask, onException, throwIO, try)
import Control.Monad (void, when)
import qualified Data.ByteString as BS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word64)
import System.FilePath ((</>), isAbsolute, joinPath, makeRelative, normalise, splitDirectories)
import System.FSNotify (eventPath, watchTree, withManager)

data RepositoryFacts = RepositoryFacts
  { factsHead :: Maybe GitOid, factsHeadState :: Maybe GitHeadState,
    factsIndexIdentity :: Maybe Text, factsSequencerActive :: Bool,
    factsConfigurationIdentity :: Maybe Text, factsManagedSourceIdentity :: Maybe Text,
    factsCommonRefsIdentity :: Maybe Text, factsPackedRefsIdentity :: Maybe Text,
    factsReflogsIdentity :: Maybe Text, factsWorktreeMetadataIdentity :: Maybe Text,
    factsRelevantWorktreeIdentity :: Maybe Text
  }
  deriving (Eq, Show)

newtype ObservationEpoch = ObservationEpoch Word64 deriving (Eq, Ord, Show)
data ObservationFailure = ObservationGitFailure Text | ObservationPathFailure Text | ObservationVerificationFailure Text deriving (Eq, Show)
data RepositorySnapshot
  = RepositorySnapshot ObservationEpoch RepositoryFacts
  | RepositorySnapshotFailed ObservationEpoch ObservationFailure
  deriving (Show)

instance Eq RepositorySnapshot where
  RepositorySnapshot _ left == RepositorySnapshot _ right = left == right
  RepositorySnapshotFailed _ left == RepositorySnapshotFailed _ right = left == right
  _ == _ = False

data RepositoryEvent
  = RepositoryFactsChanged ObservationEpoch RepositorySnapshot [Invalidation]
  | RepositoryObservationFailure ObservationEpoch ObservationFailure
  deriving (Eq, Show)
data WatchHandle = WatchHandle { stopWatching :: IO (), awaitWatcher :: IO () }
data Observer = Observer { repositorySnapshot :: Repo -> IO RepositorySnapshot, watchRepository :: Repo -> (RepositoryEvent -> IO ()) -> IO WatchHandle }

newtype ActiveClientId = ActiveClientId Int deriving (Eq, Ord, Show)
data ActiveFileState = ActiveFileState Word64 Int (Map.Map ActiveClientId (Set RepoPath))
newtype ActiveFileRegistry = ActiveFileRegistry (TVar ActiveFileState)

newActiveFileRegistry :: IO ActiveFileRegistry
newActiveFileRegistry = ActiveFileRegistry <$> newTVarIO (ActiveFileState 0 0 Map.empty)

registerActiveClient :: ActiveFileRegistry -> IO (Either Text ActiveClientId)
registerActiveClient (ActiveFileRegistry state) = atomically $ do
  ActiveFileState epoch next clients <- readTVar state
  if Map.size clients >= 16 then pure (Left "active-file client limit reached") else do
    let client = ActiveClientId next
    writeTVar state (ActiveFileState epoch (next + 1) (Map.insert client Set.empty clients))
    pure (Right client)

replaceActiveFiles :: ActiveFileRegistry -> ActiveClientId -> [RepoPath] -> IO (Either Text ())
replaceActiveFiles (ActiveFileRegistry state) client paths = atomically $ do
  ActiveFileState epoch next clients <- readTVar state
  let replacement = Set.fromList paths
      proposed = Map.insert client replacement clients
      total = Set.size (Set.unions (Map.elems proposed))
  if not (Map.member client clients) then pure (Left "active-file client is not registered")
  else if length paths /= Set.size replacement then pure (Left "active-file paths must be unique")
  else if Set.size replacement > 32 || total > 256 then pure (Left "active-file lease limit reached")
  else do
    let changed = Set.unions (Map.elems clients) /= Set.unions (Map.elems proposed)
    writeTVar state (ActiveFileState (if changed then epoch + 1 else epoch) next proposed)
    pure (Right ())

unregisterActiveClient :: ActiveFileRegistry -> ActiveClientId -> IO ()
unregisterActiveClient (ActiveFileRegistry state) client = atomically $ do
  ActiveFileState epoch next clients <- readTVar state
  let proposed = Map.delete client clients
      changed = Set.unions (Map.elems clients) /= Set.unions (Map.elems proposed)
  writeTVar state (ActiveFileState (if changed then epoch + 1 else epoch) next proposed)

activeFileUnion :: ActiveFileRegistry -> IO [RepoPath]
activeFileUnion (ActiveFileRegistry state) = do
  ActiveFileState _ _ clients <- readTVarIO state
  pure (Set.toAscList (Set.unions (Map.elems clients)))

observationEpochMatches :: ActiveFileRegistry -> ObservationEpoch -> STM Bool
observationEpochMatches (ActiveFileRegistry state) (ObservationEpoch expected) = do
  ActiveFileState actual _ _ <- readTVar state
  pure (actual == expected)

observerForRegistry :: ActiveFileRegistry -> Repo -> IO Observer
observerForRegistry registry repo = observerForRegistryWithHandleHook registry repo (const (pure ()))

observerForRegistryWithHandleHook :: ActiveFileRegistry -> Repo -> (FilePath -> IO ()) -> IO Observer
observerForRegistryWithHandleHook registry repo afterHandleOpen = do
  identities <- captureRootIdentities repo
  pure (Observer (snapshotRepository registry identities afterHandleOpen) (startWatcher registry identities afterHandleOpen))

startWatcher :: ActiveFileRegistry -> RootIdentities -> (FilePath -> IO ()) -> Repo -> (RepositoryEvent -> IO ()) -> IO WatchHandle
startWatcher registry identities afterHandleOpen repo publish = mask $ \restore -> do
  initial <- restore (snapshotRepository registry identities afterHandleOpen repo)
  current <- newTVarIO initial
  signal <- newEmptyMVar
  backend <- asyncWithUnmask (\unmask -> unmask (backendAction signal))
  verifier <- asyncWithUnmask (\unmask -> unmask (verificationLoop current signal initial) `finally` (cancel backend >> void (waitCatch backend)))
    `onException` (cancel backend >> void (waitCatch backend))
  let stopAll = cancel backend >> cancel verifier
      awaitAll = void (waitCatch backend) >> void (waitCatch verifier)
  pure (WatchHandle stopAll awaitAll)
  where
    backendAction signal = withManager $ \manager -> do
      stopListening <- watchTree manager (repoWorktreeRoot repo) (not . excludedFor repo . eventPath) (\_ -> signalHint registry signal)
      threadDelay maxBound `finallySync` stopListening

    verificationLoop current signal previous = do
      threadDelay 250000
      __ <- tryTakeMVar signal
      observed <- snapshotRepository registry identities afterHandleOpen repo
      if observed == previous
        then atomically (writeTVar current observed) >> verificationLoop current signal observed
        else do
          let observedEpoch = snapshotEpoch observed
          published <- trySynchronous $ case (previous, observed) of
            (RepositorySnapshot _ before, RepositorySnapshot _ after) ->
              publish (RepositoryFactsChanged observedEpoch observed (diffRepositoryFacts before after))
            (_, RepositorySnapshotFailed _ failure) ->
              publish (RepositoryObservationFailure observedEpoch failure)
            (_, RepositorySnapshot _ _) ->
              publish (RepositoryFactsChanged observedEpoch observed [RepositoryIdentityChanged])
          case published of
            Left failure
              | Just Events.GenerationExhausted <- fromException failure -> pure ()
              | otherwise -> verificationLoop current signal previous
            Right () -> atomically (writeTVar current observed) >> verificationLoop current signal observed

snapshotEpoch :: RepositorySnapshot -> ObservationEpoch
snapshotEpoch (RepositorySnapshot epoch _) = epoch
snapshotEpoch (RepositorySnapshotFailed epoch _) = epoch

signalHint :: ActiveFileRegistry -> MVar () -> IO ()
signalHint (ActiveFileRegistry state) signal = do
  atomically $ modifyTVar' state (\(ActiveFileState epoch next clients) -> ActiveFileState (epoch + 1) next clients)
  void (tryPutMVar signal ())

finallySync :: IO a -> IO b -> IO a
finallySync action cleanup = do result <- try @SomeException action; _ <- cleanup; either throwIO pure result

trySynchronous :: IO a -> IO (Either SomeException a)
trySynchronous action = do
  outcome <- try @SomeException action
  case outcome of
    Left failure -> case fromException failure of
      Just cancellation -> throwIO (cancellation :: SomeAsyncException)
      Nothing -> pure (Left failure)
    Right value -> pure (Right value)

data RootIdentities = RootIdentities FileIdentity FileIdentity FileIdentity deriving (Eq)
data RootKind = WorktreeRoot | GitRoot | CommonRoot deriving (Eq)
data RootHandles = RootHandles NativeHandle NativeHandle NativeHandle
data ObservationBudget = ObservationBudget (IORef (Int, Int)) (FilePath -> IO ())

snapshotRepository :: ActiveFileRegistry -> RootIdentities -> (FilePath -> IO ()) -> Repo -> IO RepositorySnapshot
snapshotRepository registry expectedRoots afterHandleOpen repo = do
  (epoch, relevant) <- activeObservation registry
  captured <- try @SomeException $ withRootHandles repo $ \roots -> do
    actualRoots <- identitiesOf roots
    when (actualRoots /= expectedRoots) (throwIO (userError "permanently bound repository root identity changed"))
    budget <- ObservationBudget <$> newIORef (0 :: Int, 0 :: Int) <*> pure afterHandleOpen
    headState <- repositoryHeadState (repoRepository repo) >>= either (throwIO . userError . show) pure
    headOid <- resolveRevision (repoRepository repo) (RevisionSpec "HEAD") >>= either (throwIO . userError . show) (pure . Just)
    configBytes <- observedFile budget (rootHandle roots WorktreeRoot) [".adrai.toml"]
    config <- loadWorktreeConfig configBytes
    indexIdentity <- fileIdentity budget (rootHandle roots GitRoot) ["index"]
    sequencer <- sequencerActive budget (rootHandle roots GitRoot)
    managed <- managedIdentity budget roots (configManagedPaths config)
    commonRefs <- directoryIdentity budget CommonRoot (rootHandle roots CommonRoot) ["refs"]
    packedRefs <- fileIdentity budget (rootHandle roots CommonRoot) ["packed-refs"]
    reflogs <- directoryIdentity budget CommonRoot (rootHandle roots CommonRoot) ["logs"]
    worktreeMeta <- directoryIdentity budget GitRoot (rootHandle roots GitRoot) []
    relevantFiles <- relevantIdentity budget roots relevant
    pure (RepositoryFacts headOid (Just headState) indexIdentity sequencer (digest <$> configBytes) managed commonRefs packedRefs reflogs worktreeMeta relevantFiles)
  case captured of
    Right facts -> pure (RepositorySnapshot epoch facts)
    Left exception -> case fromException exception of
      Just cancellation -> throwIO (cancellation :: SomeAsyncException)
      Nothing -> pure (RepositorySnapshotFailed epoch (ObservationVerificationFailure (Text.pack (displayException exception))))

activeObservation :: ActiveFileRegistry -> IO (ObservationEpoch, [RepoPath])
activeObservation (ActiveFileRegistry state) = atomically $ do
  ActiveFileState epoch _ clients <- readTVar state
  pure (ObservationEpoch epoch, Set.toAscList (Set.unions (Map.elems clients)))

loadWorktreeConfig :: Maybe BS.ByteString -> IO Config
loadWorktreeConfig Nothing = pure defaultConfig
loadWorktreeConfig (Just bytes) = do
  text <- either (throwIO . userError . show) pure (TextEncoding.decodeUtf8' bytes)
  either (throwIO . userError . show) pure (parseConfigText text)

managedIdentity :: ObservationBudget -> RootHandles -> ManagedPaths -> IO (Maybe Text)
managedIdentity budget roots paths =
  combinedIdentity budget WorktreeRoot (rootHandle roots WorktreeRoot)
    [ repoComponents (managedDecisionPath paths),
      repoComponents (managedConnectionPath paths)
    ]

relevantIdentity :: ObservationBudget -> RootHandles -> [RepoPath] -> IO (Maybe Text)
relevantIdentity budget roots paths =
  combinedIdentity budget WorktreeRoot (rootHandle roots WorktreeRoot) (map repoComponents paths)

repoComponents :: RepoPath -> [FilePath]
repoComponents = map Text.unpack . Text.splitOn "/" . repoPathText

combinedIdentity :: ObservationBudget -> RootKind -> NativeHandle -> [[FilePath]] -> IO (Maybe Text)
combinedIdentity budget kind root paths =
  fingerprint . map (maybe "<missing>" TextEncoding.encodeUtf8) <$> mapM (pathIdentity budget kind root) paths

sequencerActive :: ObservationBudget -> NativeHandle -> IO Bool
sequencerActive budget root =
  or <$> mapM (pathPresent budget root . pure) ["MERGE_HEAD", "CHERRY_PICK_HEAD", "REVERT_HEAD", "rebase-apply", "rebase-merge", "sequencer"]

pathPresent :: ObservationBudget -> NativeHandle -> [FilePath] -> IO Bool
pathPresent budget root components =
  withRelativeHandle budget root components AnyEntry (pure . maybe False (const True))

pathIdentity :: ObservationBudget -> RootKind -> NativeHandle -> [FilePath] -> IO (Maybe Text)
pathIdentity budget kind root components =
  withRelativeHandle budget root components AnyEntry $ \case
    Nothing -> pure Nothing
    Just handle -> do
      info <- checkedInformation handle
      if isDirectoryInfo info
        then directoryIdentityFromHandle budget kind components handle
        else Just . digest <$> readBoundedHandle budget handle

fileIdentity :: ObservationBudget -> NativeHandle -> [FilePath] -> IO (Maybe Text)
fileIdentity budget root components = fmap digest <$> observedFile budget root components

observedFile :: ObservationBudget -> NativeHandle -> [FilePath] -> IO (Maybe BS.ByteString)
observedFile budget root components =
  withRelativeHandle budget root components FileEntry $ \case
    Nothing -> pure Nothing
    Just handle -> Just <$> readBoundedHandle budget handle

directoryIdentity :: ObservationBudget -> RootKind -> NativeHandle -> [FilePath] -> IO (Maybe Text)
directoryIdentity budget kind root components =
  withRelativeHandle budget root components DirectoryEntry $ \case
    Nothing -> pure Nothing
    Just handle -> directoryIdentityFromHandle budget kind components handle

directoryIdentityFromHandle :: ObservationBudget -> RootKind -> [FilePath] -> NativeHandle -> IO (Maybe Text)
directoryIdentityFromHandle budget kind prefix root =
  fingerprint . map frame . sort <$> collect prefix root
  where
    frame (relative, bytes) = TextEncoding.encodeUtf8 (Text.pack relative) <> "\0" <> bytes

    collect currentPrefix current =
      foldDirectory current [] $ \rows name -> do
        childRows <- observeChild currentPrefix current name
        pure (childRows <> rows)

    observeChild currentPrefix current name = do
      chargeEntry budget
      if name `elem` [".", ".."] then pure [] else do
        validateComponent name
        let full = currentPrefix <> [name]
            relative = joinComponents (drop (length prefix) full)
        if excludedComponents kind full then pure [] else
          withRelativeHandleUncharged budget current [name] AnyEntry $ \case
            Nothing -> pure [(relative, "<missing>")]
            Just child -> do
              info <- checkedInformation child
              if isReparseInfo info then pure [(relative, "<reparse>")]
              else if isDirectoryInfo info then collect full child
              else do bytes <- readBoundedHandle budget child; pure [(relative, bytes)]

chargeEntry :: ObservationBudget -> IO ()
chargeEntry (ObservationBudget budget _) = do
  exceeded <- atomicModifyIORef' budget $ \(count, total) ->
    let next = (count + 1, total)
     in (next, fst next > maximumObservationEntries)
  when exceeded (throwIO (userError "observation entry budget exceeded"))

readBoundedHandle :: ObservationBudget -> NativeHandle -> IO BS.ByteString
readBoundedHandle (ObservationBudget budget _) handle = go []
  where
    go chunks = do
      (_, consumed) <- readIORef budget
      let remaining = maximumObservationBytes - consumed
          requestBytes = min nativeBufferBytes (remaining + 1)
      when (requestBytes <= 0) (throwIO (userError "observation byte budget exceeded"))
      chunk <- readHandleChunk handle requestBytes
      if BS.null chunk then pure (BS.concat (reverse chunks)) else do
        let size = BS.length chunk
        when (size > remaining) (throwIO (userError "observation byte budget exceeded"))
        atomicModifyIORef' budget (\(count, total) -> ((count, total + size), ()))
        go (chunk : chunks)

maximumObservationEntries, maximumObservationBytes :: Int
maximumObservationEntries = 4096
maximumObservationBytes = 16 * 1024 * 1024

nativeBufferBytes :: Int
nativeBufferBytes = 64 * 1024

captureRootIdentities :: Repo -> IO RootIdentities
captureRootIdentities repo = withRootHandles repo identitiesOf

withRootHandles :: Repo -> (RootHandles -> IO value) -> IO value
withRootHandles repo action =
  bracket (openAbsoluteDirectory (repoWorktreeRoot repo)) closeHandle $ \worktree ->
    bracket (openAbsoluteDirectory (repoGitDirectory repo)) closeHandle $ \git ->
      bracket (openAbsoluteDirectory (repoCommonDirectory repo)) closeHandle $ \common ->
        action (RootHandles worktree git common)

rootHandle :: RootHandles -> RootKind -> NativeHandle
rootHandle (RootHandles worktree _ _) WorktreeRoot = worktree
rootHandle (RootHandles _ git _) GitRoot = git
rootHandle (RootHandles _ _ common) CommonRoot = common

identitiesOf :: RootHandles -> IO RootIdentities
identitiesOf (RootHandles worktree git common) =
  RootIdentities <$> handleIdentity worktree <*> handleIdentity git <*> handleIdentity common

withRelativeHandle :: ObservationBudget -> NativeHandle -> [FilePath] -> NativeEntryKind -> (Maybe NativeHandle -> IO value) -> IO value
withRelativeHandle budget root components kind action = do
  mapM_ (const (chargeEntry budget)) components
  withRelativeHandleUncharged budget root components kind action

withRelativeHandleUncharged :: ObservationBudget -> NativeHandle -> [FilePath] -> NativeEntryKind -> (Maybe NativeHandle -> IO value) -> IO value
withRelativeHandleUncharged _ root [] kind action = do
  validateHandleKind root kind
  action (Just root)
withRelativeHandleUncharged budget@(ObservationBudget _ afterHandleOpen) root (component : remaining) kind action = do
  validateComponent component
  let componentKind = if null remaining then kind else DirectoryEntry
  withNativeHandle root component componentKind $ \case
    Nothing -> action Nothing
    Just owned -> do
      afterHandleOpen component
      when (not (null remaining)) $ do
        information <- checkedInformation owned
        when (not (isDirectoryInfo information)) (throwIO (userError "observation path component is not a directory"))
      withRelativeHandleUncharged budget owned remaining kind action

validateComponent :: FilePath -> IO ()
validateComponent component =
  when (null component || component `elem` [".", ".."] || any (`elem` component) ['\\', '/', ':', '\0'])
    (throwIO (userError "unsafe native observation path component"))

joinComponents :: [FilePath] -> FilePath
joinComponents = joinPath

fingerprint :: [BS.ByteString] -> Maybe Text
fingerprint [] = Nothing
fingerprint frames = Just (digest (BS.concat frames))

digest :: BS.ByteString -> Text
digest = encodeBase64Url . digestBytes . sha256Digest

excludedFor :: Repo -> FilePath -> Bool
excludedFor repo path =
  normalise path == normalise (repoCommonDirectory repo </> "adrai.lock")
    || any (`containsPath` path)
    [ repoWorktreeRoot repo </> ".adrai" </> "cache",
      repoCommonDirectory repo </> "objects",
      repoGitDirectory repo </> "objects"
    ]
    || any (`Text.isSuffixOf` Text.pack path) [".sqlite", ".sqlite-wal", ".sqlite-shm"]

containsPath :: FilePath -> FilePath -> Bool
containsPath root path =
  let relative = normalise (makeRelative (normalise root) (normalise path))
   in relative == "." || (not (isAbsolute relative) && case splitDirectories relative of { ".." : _ -> False; _ -> True })

excludedComponents :: RootKind -> [FilePath] -> Bool
excludedComponents kind components =
  (kind == WorktreeRoot && take 2 components == [".adrai", "cache"])
    || (kind /= WorktreeRoot && take 1 components == ["objects"])
    || (kind /= WorktreeRoot && components == ["adrai.lock"])
    || any (\suffix -> suffix `Text.isSuffixOf` Text.pack (lastOrEmpty components)) [".sqlite", ".sqlite-wal", ".sqlite-shm"]

lastOrEmpty :: [FilePath] -> FilePath
lastOrEmpty = foldl (\_ value -> value) ""

diffRepositoryFacts :: RepositoryFacts -> RepositoryFacts -> [Invalidation]
diffRepositoryFacts before after =
  [RepositoryIdentityChanged | factsHeadState before /= factsHeadState after]
  <> [HeadChanged | factsHead before /= factsHead after]
  <> [IndexChanged | factsIndexIdentity before /= factsIndexIdentity after]
  <> [SequencerChanged | factsSequencerActive before /= factsSequencerActive after]
  <> [ConfigurationChanged | factsConfigurationIdentity before /= factsConfigurationIdentity after]
  <> [ManagedSourceChanged | factsManagedSourceIdentity before /= factsManagedSourceIdentity after]
  <> [CommonReferencesChanged | factsCommonRefsIdentity before /= factsCommonRefsIdentity after]
  <> [PackedReferencesChanged | factsPackedRefsIdentity before /= factsPackedRefsIdentity after]
  <> [ReflogsChanged | factsReflogsIdentity before /= factsReflogsIdentity after]
  <> [WorktreeMetadataChanged | factsWorktreeMetadataIdentity before /= factsWorktreeMetadataIdentity after]
  <> [RelevantWorktreeFileChanged | factsRelevantWorktreeIdentity before /= factsRelevantWorktreeIdentity after]
