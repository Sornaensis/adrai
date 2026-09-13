{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Small repository-bound runtime helpers shared by terminal and HTTP
-- adapters.  Durable mutations remain authoritative even when the disposable
-- search index cannot be refreshed.
module Adrai.Service.Runtime
  ( prepareIndexPath,
    capturePostCommitIndex,
    indexCommitted,
    runDoctorAt,
  )
where

import Adrai.CliTypes
  ( DoctorCounts (..),
    DoctorIssue (..),
    DoctorOutput (..),
  )
import Adrai.Git
  ( GitHeadState (..),
    GitOid,
    Repository,
    RevisionSpec (..),
    gitOidText,
    isShallowRepository,
    repositoryHeadState,
    repositoryWorktreeRoot,
    resolveRevision,
  )
import Adrai.Repository
  ( RepositorySnapshot,
    repositoryObservedConfig,
    repositorySnapshot,
    repositorySnapshotConfig,
    repositorySnapshotManagedPaths,
    resolveRepositoryRevision,
    resolvedCommitOid,
  )
import Adrai.Service.PostCommitIndex
  ( PostCommitIndexError (..),
    PostCommitIndexResult (..),
    compilePostCommitIndex,
    compilePostCommitIndexWithAttributionAndRefresh,
  )
import Adrai.Service.PostCommitIndex.Internal (clonePostCommitIndexTrustedSource)
import Adrai.Compiler.CacheSelection (withExactArchiveAliasRepair)
import Adrai.Compiler.Attribution (inertColdCompileAttribution)
import Adrai.Compiler.CacheSync (syncProvenanceSnapshot)
import Control.Exception
  ( SomeAsyncException,
    SomeException,
    bracket,
    displayException,
    fromException,
    throwIO,
    try,
  )
import Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead
import Database.SQLite.Simple (Connection, Only (..), close, execute, open, query_, withTransaction)
import Adrai.Provenance (OverlayFingerprint (..))
import Adrai.Provenance.Ensure
  ( ProvenanceUpdate (..),
    configKey,
    ensureProvenanceWithRecoveryWitness,
    seedRegisteredOperationsFromSemanticCache,
  )
import Adrai.Provenance.Lock (withOverlayLock)
import Adrai.Provenance.Overlay
  ( OverlaySchemaState (..),
    createOverlaySchema,
    overlaySchemaState,
    provenanceDatabasePath,
  )
import Adrai.Types
  ( ManagedPaths (..),
    configLogicalLines,
    configManagedPaths,
    gitRefText,
    logicalLineId,
    repoPathText,
  )
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>), takeDirectory)

prepareIndexPath :: Repository -> IO (Either Text FilePath)
prepareIndexPath repository =
  case repositoryWorktreeRoot repository of
    Nothing -> pure (Left "worktree root is missing")
    Just root -> do
      let directory = root </> ".adrai"
          database = directory </> "index.sqlite"
      prepared <- try (createDirectoryIfMissing True directory) :: IO (Either SomeException ())
      case prepared of
        Left exception -> case fromException exception of
          Just cancellation -> throwIO (cancellation :: SomeAsyncException)
          Nothing -> pure (Left ("unable to prepare index path: " <> Text.pack (displayException exception)))
        Right () -> pure (Right database)

capturePostCommitIndex :: IO PostCommitIndexResult -> IO PostCommitIndexResult
capturePostCommitIndex action = do
  indexed <- try action :: IO (Either SomeException PostCommitIndexResult)
  case indexed of
    Left exception ->
      case fromException exception of
        Just cancellation -> throwIO (cancellation :: SomeAsyncException)
        Nothing -> pure (PostCommitIndexResult False Nothing Nothing [] (Just (PostCommitIndexCompileException (Text.pack (displayException exception)))))
    Right result -> pure result

indexCommitted :: FilePath -> Repository -> GitOid -> IO PostCommitIndexResult
indexCommitted database repository commit =
  capturePostCommitIndex (compilePostCommitIndex repository commit database)

-- | Compile and read the exact requested revision using the same public
-- post-commit compiler used after mutations.  The resolved OID, rather than a
-- later ambient HEAD, is carried through compilation and projection.
runDoctorAt :: Repository -> Text -> IO (Either Text DoctorOutput)
runDoctorAt repository requested = do
  resolved <- resolveRepositoryRevision repository (RevisionSpec requested)
  case resolved of
    Left problem -> pure (Left (Text.pack (show problem)))
    Right revision -> do
      shallow <- isShallowRepository repository
      case shallow of
        Left problem -> pure (Left (Text.pack (show problem)))
        Right isShallow -> do
          databaseResult <- prepareIndexPath repository
          case databaseResult of
            Left problem -> pure (Left problem)
            Right database -> do
              let target = resolvedCommitOid revision
              archiveResult <- prepareCacheSnapshotPath repository target
              case archiveResult of
                Left problem -> pure (Left problem)
                Right archive -> do
                  reused <- publishExactArchive archive target database
                  case reused of
                    Left problem -> pure (Left problem)
                    Right True -> loadDoctorOutput database target isShallow
                    Right False -> do
                      snapshotResult <- repositorySnapshot repository (RevisionSpec (gitOidText target))
                      case snapshotResult of
                        Left problem -> pure (Left (Text.pack (show problem)))
                        Right snapshot -> do
                          indexed <- indexCommittedWithProvenance archive repository target snapshot
                          case (postCommitIndexed indexed, postCommitDatabase indexed, postCommitIndexRevision indexed, postCommitIndexError indexed) of
                            (True, Just published, Just actual, Nothing)
                              | published == archive && actual == target -> do
                                  aliased <- publishExactArchive archive target database
                                  case aliased of
                                    Right True -> loadDoctorOutput database target isShallow
                                    Right False -> pure (Left "doctor exact archive failed post-publication validation")
                                    Left problem -> pure (Left problem)
                              | otherwise -> pure (Left "doctor published an unexpected database or revision")
                            (_, _, _, Just problem) -> pure (Left ("doctor failed: " <> Text.pack (show problem)))
                            _ -> pure (Left "doctor returned an incomplete index result")

data ProvenanceRefresh = ProvenanceRefresh
  { provenanceRefreshOverlay :: FilePath,
    provenanceRefreshOperationIds :: [Text],
    provenanceRefreshConfigKey :: Text,
    provenanceRefreshTarget :: GitOid,
    provenanceRefreshRefObservations :: [(Text, Text, Text)],
    provenanceRefreshCurrentHead :: GitOid,
    provenanceRefreshCurrentRef :: Text,
    provenanceRefreshUpdate :: ProvenanceUpdate
  }

indexCommittedWithProvenance :: FilePath -> Repository -> GitOid -> RepositorySnapshot -> IO PostCommitIndexResult
indexCommittedWithProvenance database repository commit snapshot =
  capturePostCommitIndex $
    compilePostCommitIndexWithAttributionAndRefresh inertColdCompileAttribution repository commit database $ \candidate -> do
      refreshed <- withRefreshedProvenance repository (seedRegisteredOperationsFromSemanticCache candidate) commit snapshot $ \provenanceRefresh ->
        syncProvenanceIntoCache candidate provenanceRefresh
      either (ioError . userError . Text.unpack) pure refreshed

withRefreshedProvenance :: Repository -> (Connection -> IO ([Text], [Text])) -> GitOid -> RepositorySnapshot -> (ProvenanceRefresh -> IO a) -> IO (Either Text a)
withRefreshedProvenance repository seedRegistered target snapshot useRefreshed =
  case repositoryWorktreeRoot repository of
    Nothing -> pure (Left "provenance refresh requires a worktree root")
    Just root -> do
      let sharedSemanticDatabase = root </> ".adrai" </> "index.sqlite"
          overlay = provenanceDatabasePath sharedSemanticDatabase
          managed = repositorySnapshotManagedPaths snapshot
          config = repositoryObservedConfig (repositorySnapshotConfig snapshot)
          configuredManaged = configManagedPaths config
          logicalLines = configLogicalLines config
          lineIds = map logicalLineId logicalLines
          decisions = repoPathText (managedDecisionPath managed)
          connections = repoPathText (managedConnectionPath managed)
          lineKey = configKey decisions connections lineIds
      refreshed <- try $ withOverlayLock (takeDirectory overlay) $ do
        when (managed /= configuredManaged) $
          throwIO (userError "snapshot managed paths disagree with its parsed configuration")
        existing <- doesFileExist overlay
        shape <- if not existing then pure Nothing else Just <$> bracket (open overlay) close overlaySchemaState
        case shape of
          Just OverlaySchemaV1 -> removeFile overlay
          Just OverlaySchemaInvalid -> throwIO (userError "existing provenance overlay schema is invalid")
          _ -> pure ()
        let exists = existing && shape /= Just OverlaySchemaV1
        bracket (open overlay) close $ \connection -> do
          if exists then pure () else createOverlaySchema connection
          refreshedSnapshot <- withTransaction connection $ do
            (operationIds, reseededChangedOperations) <- seedRegistered connection
            ensured <- ensureProvenanceWithRecoveryWitness repository connection sharedSemanticDatabase lineIds decisions connections logicalLines Nothing operationIds Nothing target Nothing reseededChangedOperations
            case ensured of
              Left problem -> throwIO problem
              Right update -> do
                refs <- query_ connection "SELECT ref_name,tip_oid,object_type FROM ref_observation ORDER BY ref_name"
                (currentHead, currentRef) <- resolvedCurrentHeadAndRef repository
                pure (ProvenanceRefresh overlay operationIds lineKey target refs currentHead currentRef update)
          useRefreshed refreshedSnapshot
      case refreshed of
        Left exception -> case fromException exception of
          Just cancellation -> throwIO (cancellation :: SomeAsyncException)
          Nothing -> pure (Left ("provenance refresh failed: " <> Text.pack (displayException (exception :: SomeException))))
        Right value -> pure (Right value)

resolvedCurrentHeadAndRef :: Repository -> IO (GitOid, Text)
resolvedCurrentHeadAndRef repository = do
  currentHead <- resolveRevision repository (RevisionSpec "HEAD")
  headState <- repositoryHeadState repository
  case (currentHead, headState) of
    (Left problem, _) -> throwIO (userError (show problem))
    (_, Left problem) -> throwIO (userError (show problem))
    (Right headOid, Right (GitHeadAttached ref)) -> pure (headOid, gitRefText ref)
    (Right headOid, Right GitHeadDetached) -> pure (headOid, "HEAD")

syncProvenanceIntoCache :: FilePath -> ProvenanceRefresh -> IO ()
syncProvenanceIntoCache cachePath refreshed =
  bracket (open cachePath) close $ \connection -> do
    let update = provenanceRefreshUpdate refreshed
        OverlayFingerprint fingerprintText = fingerprint update
    _ <- syncProvenanceSnapshot
      connection connection (provenanceRefreshOverlay refreshed)
      (Text.intercalate " " (provenanceRefreshOperationIds refreshed))
      (provenanceRefreshConfigKey refreshed)
      (provenanceRefreshRefObservations refreshed) fingerprintText (generation update) (observedCommitCount update)
      (gitOidText (provenanceRefreshTarget refreshed))
      (map gitOidText (Set.toAscList (targetReachableCommits update)))
      (gitOidText (provenanceRefreshCurrentHead refreshed)) (provenanceRefreshCurrentRef refreshed) ""
      (commitsScanned update) "incremental" "tree-identical"
    execute connection "UPDATE meta SET value=? WHERE key='history_commits_scanned'" (Only (Text.pack (show (commitsScanned update))))

prepareCacheSnapshotPath :: Repository -> GitOid -> IO (Either Text FilePath)
prepareCacheSnapshotPath repository revision =
  case repositoryWorktreeRoot repository of
    Nothing -> pure (Left "worktree root is missing")
    Just root -> prepareDirectory (root </> ".adrai" </> "cache") (Text.unpack (gitOidText revision) <> ".sqlite")
  where
    prepareDirectory directory name = do
      prepared <- try (createDirectoryIfMissing True directory) :: IO (Either SomeException ())
      case prepared of
        Left exception -> case fromException exception of
          Just cancellation -> throwIO (cancellation :: SomeAsyncException)
          Nothing -> pure (Left ("unable to prepare cache snapshot path: " <> Text.pack (displayException exception)))
        Right () -> pure (Right (directory </> name))

publishExactArchive :: FilePath -> GitOid -> FilePath -> IO (Either Text Bool)
publishExactArchive archive revision database = do
  repaired <- withExactArchiveAliasRepair archive (gitOidText revision) database $ \_ -> do
    cloned <- capturePostCommitIndex (clonePostCommitIndexTrustedSource archive revision database Nothing Nothing)
    pure $ case (postCommitIndexed cloned, postCommitDatabase cloned, postCommitIndexRevision cloned, postCommitIndexError cloned) of
      (True, Just published, Just actual, Nothing) | published == database && actual == revision -> Right ()
      (_, _, _, Just problem) -> Left ("doctor current-alias publication failed: " <> Text.pack (show problem))
      _ -> Left "doctor current-alias publication returned an incomplete result"
  pure $ case repaired of
    Left problem -> Left problem
    Right (Just _) -> Right True
    Right Nothing -> Right False

type DoctorRows =
  ( [(Text, Text)],
    [(Int, Text, Text, Text, Maybe Text, Maybe Text, Maybe Text, Text)],
    [(Text, Text, Text)]
  )

loadDoctorOutput :: FilePath -> GitOid -> Bool -> IO (Either Text DoctorOutput)
loadDoctorOutput database expectedRevision shallow = do
  captured <- try (bracket (open database) close readRows) :: IO (Either SomeException DoctorRows)
  case captured of
    Left exception ->
      case fromException exception of
        Just cancellation -> throwIO (cancellation :: SomeAsyncException)
        Nothing -> pure (Left ("unable to read doctor database: " <> Text.pack (displayException exception)))
    Right (metadata, issueRows, conflictRows) -> pure (materialize metadata issueRows conflictRows)
  where
    readRows connection = do
      metadata <- query_ connection "SELECT key,value FROM meta ORDER BY key"
      issues <- query_ connection "SELECT ordinal,code,severity,origin,adr_id,object_id,path,message FROM issue ORDER BY ordinal"
      conflicts <- query_ connection "SELECT adr_id,state_token,summaries FROM adr_conflict ORDER BY adr_id"
      pure (metadata, issues, conflicts)

    materialize metadata issueRows conflictRows = do
      resolved <- one metadata "resolved_oid"
      if resolved == gitOidText expectedRevision then Right () else Left "compiled database revision does not match the requested revision"
      _managedSources <- count metadata "managed_source_count"
      issueCount <- count metadata "issue_count"
      conflictCount <- count metadata "conflict_count"
      _historyCommitsScanned <- count metadata "history_commits_scanned"
      _operationCount <- count metadata "operation_count"
      _searchDocuments <- count metadata "search_document_count"
      _materializationFingerprint <- one metadata "materialization_fingerprint"
      if conflictCount <= issueCount then Right () else Left "compiled database conflict count exceeds issue count"
      if issueCount == length issueRows then Right () else Left "doctor issue rows do not match compiled issue count"
      issues <- traverse (materializeIssue (Map.fromList [(adr, (token, summaries)) | (adr, token, summaries) <- conflictRows])) issueRows
      let errors = length (filter ((== "error") . doctorIssueSeverity) issues)
      Right
        DoctorOutput
          { doctorOk = errors == 0,
            doctorRevision = gitOidText expectedRevision,
            doctorDatabase = Just database,
            doctorShallow = shallow,
            doctorIssues = issues,
            doctorCacheStatus = [],
            doctorCounts = DoctorCounts errors (length issues - errors),
            doctorCurrentAccess = Nothing,
            doctorDatabaseBuild = Nothing
          }

materializeIssue :: Map.Map Text (Text, Text) -> (Int, Text, Text, Text, Maybe Text, Maybe Text, Maybe Text, Text) -> Either Text DoctorIssue
materializeIssue conflicts (_, code, severity, _, adr, objectId, path, message)
  | severity /= "error" && severity /= "warning" = Left ("doctor database has invalid issue severity: " <> severity)
  | code == "ADR_CONFLICT" =
      case adr >>= (`Map.lookup` conflicts) of
        Nothing -> Left "doctor conflict issue is missing its conflict details"
        Just (token, summaries) -> Right (issue (Just token) (map Aeson.String (Text.splitOn "\n" summaries)))
  | otherwise = Right (issue Nothing [])
  where
    issue stateToken conflictDetails =
      DoctorIssue severity code message adr objectId path stateToken conflictDetails

one :: [(Text, Text)] -> Text -> Either Text Text
one rows key =
  case Map.lookup key (Map.fromListWith (<>) [(name, [value]) | (name, value) <- rows]) of
    Just [value] -> Right value
    Just _ -> Left ("compiled database has duplicate metadata key: " <> key)
    Nothing -> Left ("compiled database is missing metadata key: " <> key)

count :: [(Text, Text)] -> Text -> Either Text Int
count rows key = do
  raw <- one rows key
  case TextRead.decimal raw of
    Right (value, "") | value <= maxBound -> Right value
    _ -> Left ("compiled database has invalid nonnegative integer metadata: " <> key)
