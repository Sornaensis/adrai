{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

-- | Typed, revision-local provenance hydration.
--
-- This module deliberately composes the provenance overlay authorities rather
-- than interpreting Git history or SQLite tables itself.  Maintenance happens
-- under the overlay lock; materialisation is then delegated to
-- 'readProvenanceEvidenceAt', whose graph is rooted exclusively at the
-- caller-resolved revision.
module Adrai.Provenance.Read
  ( PlacementHydrationError (..),
    hydratePlacementEvidenceAt,
    hydratePlacementEvidenceAtWith,
    hydratePlacementEvidenceAtWithHooks,
  )
where

import Adrai.Git
  ( GitOid,
    Repository,
    gitOidText,
    repositoryWorktreeRoot,
  )
import Adrai.History
  ( CommitPlacementEvidence (..),
    LineLandingEvidence (..),
    PlacementEvidence (..),
  )
import Adrai.Provenance
  ( provenanceBasis,
    provenanceOperationId,
  )
import Adrai.Provenance.Classification
  ( ParsedManagedDocument (..),
    operationSignature,
  )
import Adrai.Provenance.Ensure
  ( configKey,
    ensureProvenance,
    readProvenanceEvidenceAtWith,
  )
import Adrai.Provenance.Lock (withOverlayLock)
import Adrai.Provenance.Overlay
  ( LineConfigRow (..),
    LineLandingRow (..),
    OperationCommitRow (..),
    ProvenanceEvidence (..),
    ProvenanceEvidenceError (..),
    ProvenanceOperationEvidence (..),
    RegisteredObjectRow (..),
    RegisteredOperationRow (..),
    createOverlaySchema,
    overlayValidWith,
    provenanceDatabasePath,
  )
import Adrai.Repository
  ( ResolvedRepositoryRevision,
    resolvedCommitOid,
    resolvedRepository,
  )
import Adrai.Types
  ( Config (..),
    Digest (..),
    GitRef (..),
    LogicalLine (..),
    ManagedPaths (..),
    OperationId,
    digestBytes,
    operationIdText,
    repoPathText,
  )
import Control.Exception
  ( SomeAsyncException,
    SomeException,
    bracket,
    fromException,
    mask_,
    throwIO,
    try,
  )
import Control.Monad (forM_, unless, void, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Vector as Vector
import Database.SQLite.Simple (Only (..), close, open, query, withTransaction)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>))

-- | Every normal failure at this boundary is explicit.  Async exceptions are
-- never placed in this type: they are rethrown by 'captureSynchronous'.
data PlacementHydrationError
  = PlacementHydrationNoWorktree
  | PlacementHydrationInvalidOverlay Text
  | PlacementHydrationEnsureFailure Text
  | PlacementHydrationEvidenceFailure ProvenanceEvidenceError
  | PlacementHydrationInvalidConfig Text
  | PlacementHydrationRegistrationMismatch Text
  | PlacementHydrationObjectMismatch Text
  | PlacementHydrationInvalidClassification Text
  | PlacementHydrationInvalidParents Text
  | PlacementHydrationInvalidLanding Text
  | PlacementHydrationDuplicateEvidence Text
  | PlacementHydrationSynchronousFailure Text
  deriving (Eq, Show)

data LockedHydration value
  = LockedHydrationReady (Either PlacementHydrationError value)
  | LockedHydrationNeedsRead

-- | Hydrate placement evidence for exactly the operation set in a caller's
-- immutable snapshot.  The revision is accepted already resolved, so this
-- function never resolves a revision string or observes @HEAD@.
hydratePlacementEvidenceAt
  :: Repository
  -> ResolvedRepositoryRevision
  -> Config
  -> [ParsedManagedDocument]
  -> IO (Either PlacementHydrationError (Map OperationId PlacementEvidence))
hydratePlacementEvidenceAt repository revision config documents =
  hydratePlacementEvidenceAtWithHooks repository revision config documents (pure ()) (pure ())

-- | Narrow test seam for writer-phase cancellation verification.  Production
-- callers use 'hydratePlacementEvidenceAt'; this hook runs inside the
-- maintenance transaction after provenance has been prepared but before it
-- is committed.
hydratePlacementEvidenceAtWith
  :: Repository
  -> ResolvedRepositoryRevision
  -> Config
  -> [ParsedManagedDocument]
  -> IO ()
  -> IO (Either PlacementHydrationError (Map OperationId PlacementEvidence))
hydratePlacementEvidenceAtWith repository revision config documents afterOpen
  = hydratePlacementEvidenceAtWithHooks repository revision config documents (pure ()) afterOpen

-- | Full cancellation test seam.  The validation hook runs on an existing
-- overlay after its read-only connection is acquired; the maintenance hook
-- runs inside the writer transaction.  Production supplies two no-op hooks.
hydratePlacementEvidenceAtWithHooks
  :: Repository
  -> ResolvedRepositoryRevision
  -> Config
  -> [ParsedManagedDocument]
  -> IO ()
  -> IO ()
  -> IO (Either PlacementHydrationError (Map OperationId PlacementEvidence))
hydratePlacementEvidenceAtWithHooks repository revision config documents validationHook maintenanceHook
  | repository /= resolvedRepository revision =
      pure (Left (PlacementHydrationSynchronousFailure "resolved revision belongs to a different repository"))
  | null operationIds = pure (Right Map.empty)
  | otherwise = case repositoryWorktreeRoot repository of
      Nothing -> pure (Left PlacementHydrationNoWorktree)
      Just root -> captureSynchronous $ do
        let cacheDirectory = root </> ".adrai"
            semanticDatabase = cacheDirectory </> "index.sqlite"
            overlayDatabase = provenanceDatabasePath semanticDatabase
            target = resolvedCommitOid revision
            lineIds = map logicalLineId (configLogicalLines config)
            decisions = repoPathText (managedDecisionPath (configManagedPaths config))
            connections = repoPathText (managedConnectionPath (configManagedPaths config))
            requestedConfig = configKey decisions connections lineIds
        createDirectoryIfMissing True cacheDirectory
        lockedHydration <- withOverlayLock cacheDirectory $ do
          overlayExists <- doesFileExist overlayDatabase
          valid <- if overlayExists then overlayValidWith overlayDatabase validationHook else pure True
          if not valid
            then pure (LockedHydrationReady (Left (PlacementHydrationInvalidOverlay "existing overlay schema is invalid")))
            else do
              preflight <-
                if not overlayExists
                  then pure Nothing
                  else do
                    targetObserved <- overlayHasObservedTarget overlayDatabase target
                    evidenceResult <- readProvenanceEvidenceAtWith repository overlayDatabase target operationTexts requestedConfig (pure ())
                    pure $ case evidenceResult of
                      Right evidence -> Just (materialize config documents requestedConfig evidence)
                      Left problem
                        | preflightMayRequireMaintenance targetObserved problem -> Nothing
                        | otherwise -> Just (Left (PlacementHydrationEvidenceFailure problem))
              case preflight of
                Just (Left failure) -> pure (LockedHydrationReady (Left failure))
                _ -> do
                  maintenanceAttempt <- try @SomeException $
                    bracket (open overlayDatabase) close $ \connection -> do
                      -- The schema DDL is intentionally autocommitted.  Any
                      -- subsequent update is one rollback-capable transaction.
                      unless overlayExists (createOverlaySchema connection)
                      withTransaction connection $ do
                        ensured <- ensureProvenance repository connection semanticDatabase
                          lineIds decisions connections (configLogicalLines config)
                          (Just documents) operationTexts Nothing target
                        case ensured of
                          Left problem -> throwIO problem
                          Right _ -> maintenanceHook
                  case maintenanceAttempt of
                    Right () -> pure LockedHydrationNeedsRead
                    Left problem -> do
                      unless overlayExists (cleanupFreshOverlay overlayDatabase)
                      rethrowAsync problem
                      pure (LockedHydrationReady (Left (PlacementHydrationEnsureFailure (Text.pack (show problem)))))
        case lockedHydration of
          LockedHydrationReady hydration -> pure hydration
          LockedHydrationNeedsRead -> do
            evidenceResult <- readProvenanceEvidenceAtWith repository overlayDatabase target operationTexts requestedConfig (pure ())
            case evidenceResult of
              Left problem -> pure (Left (PlacementHydrationEvidenceFailure problem))
              Right evidence -> pure (materialize config documents requestedConfig evidence)
  where
    operationIds = Set.toAscList (Set.fromList (map (provenanceOperationId . parsedManagedCapsule) documents))
    operationTexts = map operationIdText operationIds

removeFreshOverlay :: FilePath -> IO ()
removeFreshOverlay database =
  forM_ [database, database <> "-journal", database <> "-wal", database <> "-shm"] $ \path -> do
    exists <- doesFileExist path
    when exists (removeFile path)

-- | Preserve the triggering exception: this recovery path is only for an
-- overlay that was absent while the writer lock was held, so it cannot erase
-- a pre-existing cache or another writer's publication.
cleanupFreshOverlay :: FilePath -> IO ()
cleanupFreshOverlay database = mask_ (void (try @SomeException (removeFreshOverlay database)))

-- | Only a genuinely absent operation registration or configuration can be
-- completed by maintenance.  Every other evidence failure is a corrupt or
-- contradictory existing-cache fact and must be returned without rewriting
-- that cache.
preflightMayRequireMaintenance :: Bool -> ProvenanceEvidenceError -> Bool
preflightMayRequireMaintenance targetObserved problem = case problem of
  ProvenanceEvidenceMissingRegistration _ -> True
  ProvenanceEvidenceMissingConfig _ -> True
  -- An unseen immutable target is a legitimate cache delta.  If the target was
  -- already observed, a missing placement is contradictory cache evidence and
  -- must remain fail-closed rather than being silently repaired.
  ProvenanceEvidenceMissingTargetPlacement _ -> not targetObserved
  _ -> False

overlayHasObservedTarget :: FilePath -> GitOid -> IO Bool
overlayHasObservedTarget database target =
  bracket (open database) close $ \connection -> do
    rows <- query connection
      "SELECT EXISTS(SELECT 1 FROM observed_commit WHERE commit_oid=?)"
      (Only (gitOidText target))
      :: IO [Only Int]
    pure (rows == [Only 1])


-- | Convert the lossless evidence authority rows into the public history
-- evidence only after rejecting inconsistent cache facts.
materialize
  :: Config
  -> [ParsedManagedDocument]
  -> Text
  -> ProvenanceEvidence
  -> Either PlacementHydrationError (Map OperationId PlacementEvidence)
materialize config documents expectedConfig evidence = do
  validateConfig config expectedConfig (provenanceEvidenceConfig evidence)
  let expected = Map.fromListWith (<>)
        [ (provenanceOperationId (parsedManagedCapsule document), [document])
          | document <- documents
        ]
      operations = provenanceEvidenceOperations evidence
  unless (map (operationIdText . fst) (Map.toAscList expected) == map (registeredOperationRowOpId . provenanceEvidenceRegistration) operations)
    (Left (PlacementHydrationRegistrationMismatch "returned operation set differs from snapshot"))
  pairs <- traverse (materializeOperation config expected) operations
  pure (Map.fromList pairs)

materializeOperation
  :: Config
  -> Map OperationId [ParsedManagedDocument]
  -> ProvenanceOperationEvidence
  -> Either PlacementHydrationError (OperationId, PlacementEvidence)
materializeOperation config expected operation = do
  let registration = provenanceEvidenceRegistration operation
      operationText = registeredOperationRowOpId registration
  operationId <- lookupOperation operationText expected
  documents <- maybe (Left (PlacementHydrationRegistrationMismatch "unknown operation")) Right (Map.lookup operationId expected)
  validateRegistration operationText documents registration
  validateObjects operationText documents (provenanceEvidenceObjects operation)
  placements <- traverse (toPlacement operationText) (provenanceEvidenceCommits operation)
  rejectDuplicates operationText (map commitPlacementOid placements)
  landings <- traverse (toLanding config operationText) (provenanceEvidenceLandings operation)
  rejectDuplicates operationText (map landingIdentity landings)
  let orderedPlacements = sortOn placementOrder placements
      orderedLandings = sortOn landingOrder landings
      preferred = case sortOn preferredOrder placements of
        [] -> Nothing
        value : _ -> Just value
      originals = [commitPlacementOid placement | placement <- orderedPlacements, commitPlacementClassification placement == "original"]
      introductions = [commitPlacementOid placement | placement <- orderedPlacements, commitPlacementClassification placement == "introduction"]
  pure
    ( operationId,
      PlacementEvidence
        (commitPlacementOid <$> preferred)
        (commitPlacementClassification <$> preferred)
        orderedPlacements
        originals
        introductions
        orderedLandings
    )

lookupOperation :: Text -> Map OperationId [ParsedManagedDocument] -> Either PlacementHydrationError OperationId
lookupOperation operationText expected =
  case [operation | operation <- Map.keys expected, operationIdText operation == operationText] of
    [operation] -> Right operation
    _ -> Left (PlacementHydrationRegistrationMismatch ("invalid operation id " <> operationText))

validateRegistration :: Text -> [ParsedManagedDocument] -> RegisteredOperationRow -> Either PlacementHydrationError ()
validateRegistration operationText documents registration = do
  let bases = Set.fromList (map (gitOidText . provenanceBasis . parsedManagedCapsule) documents)
      expectedSignature = digestHex (operationSignature documents)
  when (Set.size bases /= 1 || Set.notMember (gitOidText (registeredOperationRowBasisOid registration)) bases)
    (Left (PlacementHydrationRegistrationMismatch ("basis mismatch for " <> operationText)))
  let expectedAdrId = case documents of
        firstDocument : _ -> documentAdrId firstDocument
        [] -> Nothing
  when (registeredOperationRowAdrId registration /= expectedAdrId)
    (Left (PlacementHydrationRegistrationMismatch ("ADR mismatch for " <> operationText)))
  unless (registeredOperationRowSignature registration == expectedSignature)
    (Left (PlacementHydrationRegistrationMismatch ("signature mismatch for " <> operationText)))

documentAdrId :: ParsedManagedDocument -> Maybe Text
documentAdrId document =
  case parsedDocumentObjectRef document of
    value | Text.isPrefixOf "A" value -> Just value
    value | Text.isPrefixOf "R" value -> Just ("A" <> Text.drop 1 value)
    _ -> Nothing

digestHex :: Digest -> Text
digestHex digest = Text.concat (map byteHex (ByteString.unpack (digestBytes digest)))
  where
    byteHex value = Text.pack [hex (value `div` 16), hex (value `mod` 16)]
    hex value
      | value < 10 = toEnum (fromEnum '0' + fromIntegral value)
      | otherwise = toEnum (fromEnum 'a' + fromIntegral value - 10)

validateObjects :: Text -> [ParsedManagedDocument] -> [RegisteredObjectRow] -> Either PlacementHydrationError ()
validateObjects operationText documents rows = do
  let expected = sortOn documentIdentity
        [ ( parsedDocumentObjectRef document,
            repoPathText (parsedManagedPath document),
            maybe "" gitOidText (parsedBlobOid document)
          )
          | document <- documents
        ]
      actual = sortOn id
        [ ( registeredObjectRowObjectId row,
            registeredObjectRowPath row,
            gitOidText (registeredObjectRowBlobOid row)
          )
          | row <- rows
        ]
  rejectDuplicates operationText actual
  unless (expected == actual)
    (Left (PlacementHydrationObjectMismatch ("registered objects differ for " <> operationText)))
  where
    documentIdentity (objectId, path, blob) = (objectId, path, blob)

toPlacement :: Text -> OperationCommitRow -> Either PlacementHydrationError CommitPlacementEvidence
toPlacement operationText row = do
  classification <- validateClassification (operationCommitRowClassification row)
  parents <- decodeParents operationText (operationCommitRowParentsJson row)
  let authored = operationCommitRowAuthoredSeconds row * 1000
      committed = operationCommitRowCommittedSeconds row * 1000
  pure
    CommitPlacementEvidence
      { commitPlacementOid = gitOidText (operationCommitRowCommitOid row),
        commitPlacementClassification = classification,
        commitPlacementReachable = True,
        commitPlacementAuthoredAtMs = authored,
        commitPlacementCommittedAtMs = committed,
        commitPlacementSubject = operationCommitRowSubject row,
        commitPlacementParents = parents
      }

toLanding :: Config -> Text -> LineLandingRow -> Either PlacementHydrationError LineLandingEvidence
toLanding config operationText row = do
  let matchingLines =
        [ line
          | line <- configLogicalLines config,
            logicalLineId line == lineLandingRowLineId row,
            GitRef (lineLandingRowRefName row) `elem` logicalLineRefs line
        ]
  when (null matchingLines)
    (Left (PlacementHydrationInvalidLanding ("line/ref is not configured for " <> operationText)))
  complete <- case lineLandingRowComplete row of
    0 -> Right False
    1 -> Right True
    _ -> Left (PlacementHydrationInvalidLanding ("complete is not boolean for " <> operationText))
  pure
    ( LineLandingEvidence
        (lineLandingRowLineId row)
        (lineLandingRowRefName row)
        (gitOidText (lineLandingRowCommitOid row))
        complete
    )

validateConfig :: Config -> Text -> Maybe LineConfigRow -> Either PlacementHydrationError ()
validateConfig config expectedKey row = do
  configRow <- maybe (Left (PlacementHydrationInvalidConfig "missing line configuration")) Right row
  unless (lineConfigRowKey configRow == expectedKey)
    (Left (PlacementHydrationInvalidConfig "line configuration key mismatch"))
  let expected = Aeson.Object (KeyMap.fromList
        [ (Key.fromText "connections", Aeson.String (repoPathText (managedConnectionPath (configManagedPaths config)))),
          (Key.fromText "decisions", Aeson.String (repoPathText (managedDecisionPath (configManagedPaths config)))),
          (Key.fromText "logical_lines", Aeson.Array (Vector.fromList (map (Aeson.String . logicalLineId) (configLogicalLines config))))
        ])
  actual <- maybe (Left (PlacementHydrationInvalidConfig "line configuration JSON is invalid")) Right
    (Aeson.decodeStrict' (TextEncoding.encodeUtf8 (lineConfigRowJson configRow)))
  unless (actual == expected)
    (Left (PlacementHydrationInvalidConfig "line configuration does not match snapshot"))

validateClassification :: Text -> Either PlacementHydrationError Text
validateClassification classification
  | classification `elem` ["original", "copy", "introduction", "landing"] = Right classification
  | otherwise = Left (PlacementHydrationInvalidClassification classification)

decodeParents :: Text -> Text -> Either PlacementHydrationError [Text]
decodeParents operationText raw = do
  values <- maybe (Left (PlacementHydrationInvalidParents ("invalid parents JSON for " <> operationText))) Right
    (Aeson.decodeStrict' (TextEncoding.encodeUtf8 raw) :: Maybe [Text])
  unless (all validOid values && length values == Set.size (Set.fromList values))
    (Left (PlacementHydrationInvalidParents ("invalid or duplicate parent for " <> operationText)))
  pure values
  where
    validOid value = Text.length value `elem` [40, 64] && Text.all isLowerHex value
    isLowerHex character = ('0' <= character && character <= '9') || ('a' <= character && character <= 'f')

rejectDuplicates :: (Ord value, Show value) => Text -> [value] -> Either PlacementHydrationError ()
rejectDuplicates operationText values =
  unless (length values == Set.size (Set.fromList values))
    (Left (PlacementHydrationDuplicateEvidence ("duplicate evidence for " <> operationText)))

placementOrder :: CommitPlacementEvidence -> (Text, Integer, Text)
placementOrder placement =
  ( commitPlacementClassification placement,
    commitPlacementCommittedAtMs placement,
    commitPlacementOid placement
  )

preferredOrder :: CommitPlacementEvidence -> (Int, Integer, Text)
preferredOrder placement =
  ( classificationRank (commitPlacementClassification placement),
    commitPlacementCommittedAtMs placement,
    commitPlacementOid placement
  )

classificationRank :: Text -> Int
classificationRank classification = case classification of
  "original" -> 0
  "introduction" -> 1
  "copy" -> 2
  _ -> 3

landingIdentity :: LineLandingEvidence -> (Text, Text, Text)
landingIdentity landing = (landingLine landing, landingRef landing, landingCommit landing)

landingOrder :: LineLandingEvidence -> (Text, Text, Text)
landingOrder = landingIdentity

-- | Map synchronous failures to the typed algebra and preserve cancellation.
captureSynchronous :: IO (Either PlacementHydrationError value) -> IO (Either PlacementHydrationError value)
captureSynchronous action = do
  result <- try @SomeException action
  case result of
    Right value -> pure value
    Left exception -> rethrowAsync exception >> pure (Left (PlacementHydrationSynchronousFailure (Text.pack (show exception))))

rethrowAsync :: SomeException -> IO ()
rethrowAsync exception =
  case fromException exception of
    Just async -> throwIO (async :: SomeAsyncException)
    Nothing -> pure ()
