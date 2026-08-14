{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Mutable operations on ADRAI repositories.
--
-- Implements the mutation entry points for ADRAI repositories:
--
-- * 'initCommand'      — bootstrap a repository with default configuration,
--                        gitattributes, and gitignore files.
-- * 'createAdrCommand' — create a new architectural decision record together
--                        with its scope, domain, and status connection records.
-- * 'amendAdmCommand'  — amend an existing decision record.
-- * 'changeScopeCommand' — update scope patterns on an ADR.
-- * 'changeDomainCommand' — update domain assignments on an ADR.
-- * 'obsoleteCommand'  — mark an ADR as obsolete.
-- * 'reactivateCommand' — reactivate an obsolete ADR.
--
-- All functions use the 8-canonical-stage transaction engine from
-- 'Adrai.Service.Transaction' to guarantee atomic, lock-guarded commits.

module Adrai.Service.Mutation
  ( InitResult (..),
    initCommand,
    CreateResult (..),
    createAdrCommand,
    AmendResult (..),
    amendAdrCommand,
    amendCurrentAdrCommand,
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
where

import Adrai.Git
  ( Repository (..),
    runRepository,
    resolveRevision,
    repositoryWorktreeRoot,
    GitHeadState (..),
    repositoryHeadState,
    RevisionSpec (RevisionSpec),
    GitOid (..),
    GitBlob (..),
    GitTreeEntry (..),
    processExitCode,
    processStdout,
  )
import Adrai.Service.Transaction
  ( TransactionConfig (..),
    GeneratedFile (..),
    TransactionResult (..),
    TransactionError (..),
    commitBootstrapFiles,
    commitAppendOnlyOperation,
    nullOid,
  )
import Adrai.Provenance
  ( ProvenanceCapsule (..),
    ProvenanceCapsuleInput (..),
    EventKind,
    LineAnchor,
    ProvenanceObjectId (..),
    mkEventKind,
    sha256Digest,
    semanticDigest,
    ProvenanceError (..),
    provenanceOperationId,
    provenanceObjectIdText,
    provenanceBasis,
    provenanceActor,
    provenanceTimestampMs,
    provenanceObjectId,
    provenanceOperationContext,
    mkProvenanceCapsule,
  )
import Adrai.Format.Document
  ( DecisionRecord (..),
    ConnectionRecord (..),
    ConnectionPayload (..),
    AmendsPayload (..),
    AppliesToPayload (..),
    DomainsPayload (..),
    StatusPayload (..),
    StatusState (..),
    ParsedManagedDocument,
    parsedManagedRecord,
    parseManagedDocument,
    validateManagedLocation,
    renderDecisionSemantic,
    renderConnectionSemantic,
    canonicalManagedPath,
    sealManagedDocument,
    ManagedRecord (..),
    DocumentError (..),
  )
import Adrai.Format.Config
  ( defaultConfigText,
  )
import Adrai.Types
  ( AdrId (..),
    RecordId (..),
    ConnectionId (..),
    OperationId (..),
    RepoPath (RepoPath),
    ObjectRef (..),
    recordObjectRef,
    connectionObjectRef,
    ManagedPaths (..),
    mkManagedPaths,
    repoPathText,
    gitRefText,
    recordIdText,
    connectionIdText,
    operationIdText,
    adrIdText,
    Actor (..),
    ActorKind (..),
    Digest (..),
    digestBytes,
    ProvenanceInputs (..),
    StateToken,
    stateTokenText,
  )
import Adrai.Domain
  ( Domain,
    canonicalDomains,
    domainText,
  )
import Adrai.Scope
  ( ScopePattern,
    scopePatternText,
  )
import Adrai.Identity
  ( sortableOperationId,
    sortableRecordId,
    sortableConnectionId,
  )
import Adrai.Repository
  ( repositorySnapshot,
    repositorySnapshotEntries,
    repositorySnapshotManagedPaths,
    repositorySnapshotRevision,
    repositoryTreeBlob,
    repositoryTreeEntry,
    ResolvedRepositoryRevision,
    resolvedCommitOid,
  )
import Adrai.Graph
  ( AxisResolution (..),
    GraphReduction (..),
    ReducedAdr (..),
    ReducedStatus (..),
    lookupReducedAdr,
    reducedStateToken,
    reduceManagedGraph,
  )

import Data.Bifunctor (first)
import Data.Char (isDigit)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import qualified Data.Map.Strict as Map
import Data.List (intercalate, sort)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Control.Exception (try, SomeException)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random (StdGen, getStdRandom, uniformR)

-- | Result of the 'initCommand' bootstrap operation.
data InitResult
  = InitResult
      { initInitialized :: Bool
      , initOperationId :: String
      , initCommitOid :: GitOid
      , initCreatedPaths :: [RepoPath]
      , initIndexUpdated :: Bool
      }
  deriving (Eq, Show)

-- | Bootstrap a fresh repository with default configuration, gitattributes,
-- and gitignore files.
initCommand :: Repository -> IO (Either TransactionError InitResult)
initCommand repository =
  case repositoryWorktreeRoot repository of
    Nothing ->
      pure (Left (Stage1ResolveRepo "worktree root is missing"))
    Just root -> do
      gitattributesContent <- mergeGitattributes root
      gitignoreContent <- mergeGitignore root
      let generated =
            [ GeneratedFile (RepoPath ".adrai.toml") (encodeUtf8 defaultConfigText)
            , GeneratedFile (RepoPath ".gitattributes") gitattributesContent
            , GeneratedFile (RepoPath ".gitignore") gitignoreContent
            ]
      resolveHeadOrEmpty repository >>= \case
        Right oldHead -> do
          let config =
                TransactionConfig
                  { configOperationId = "init",
                    configSubject = "adrai: initialize repository",
                    configTrailers = Map.fromList [("Objects", "bootstrap")],
                    configExpectedHead = oldHead,
                    configGenerated = generated
                  }
          commitBootstrapFiles repository config >>= \case
            Left transactionError ->
              pure (Left transactionError)
            Right TransactionResult {..} ->
              pure
                ( Right
                    InitResult
                      { initInitialized = True
                      , initOperationId = transactionOperationId
                      , initCommitOid = transactionCommitOid
                      , initCreatedPaths = transactionCreatedPaths
                      , initIndexUpdated = transactionIndexUpdated
                      }
                )
        Left err ->
          pure (Left (Stage3ValidateState ("resolve HEAD: " <> err)))
  where
    mergeGitattributes :: FilePath -> IO BS.ByteString
    mergeGitattributes rp = do
      result <- try @SomeException (BS.readFile (rp </> ".gitattributes"))
      case result of
        Left _ -> pure (encodeUtf8 requiredAttrs)
        Right _bytes -> pure (encodeUtf8 requiredAttrs)
      where
        requiredAttrs =
          "architecture/adrai/decisions/** text eol=lf\n"
            <> "architecture/adrai/connections/** text eol=lf\n"
    mergeGitignore :: FilePath -> IO BS.ByteString
    mergeGitignore rp = do
      result <- try @SomeException (BS.readFile (rp </> ".gitignore"))
      case result of
        Left _ -> pure (encodeUtf8 requiredIgnore)
        Right _bytes -> pure (encodeUtf8 requiredIgnore)
      where
        requiredIgnore = ".adrai/\n"
    resolveHeadOrEmpty :: Repository -> IO (Either T.Text GitOid)
    resolveHeadOrEmpty repo = do
      result <-
        runRepository repo "resolve HEAD"
          ["rev-parse", "--verify", "HEAD^{commit}"] BS.empty
      pure $ case result of
        Left _ -> Right nullOid
        Right procResult
          | processExitCode procResult /= ExitSuccess -> Right nullOid
          | otherwise ->
              let output = T.strip (decodeUtf8With lenientDecode (processStdout procResult))
               in case mkGitOid output of
                    Left _ -> Right nullOid
                    Right oid -> Right oid
      where
        mkGitOid :: T.Text -> Either T.Text GitOid
        mkGitOid v
          | T.length v == 40 && T.all isHex v = Right (GitOid v)
          | T.length v == 64 && T.all isHex v = Right (GitOid v)
          | otherwise = Left (T.pack "not a valid git OID")
        isHex c = isDigit c || c >= 'a' && c <= 'f'

-- ---------------------------------------------------------------------------
-- Utility helpers
-- ---------------------------------------------------------------------------

-- | Current timestamp in milliseconds.
currentTimestamp :: IO Integer
currentTimestamp = floor . (* 1000) <$> getPOSIXTime

-- | Current timestamp encoded as the canonical 6-byte big-endian sortable-ID
-- field.
currentTimestampMs :: IO BS.ByteString
currentTimestampMs = encodeTimestampMs <$> currentTimestamp

encodeTimestampMs :: Integer -> BS.ByteString
encodeTimestampMs ms = BS.pack (toBytesBE ms)
  where
    toBytesBE :: Integer -> [Word8]
    toBytesBE n =
      [ fromIntegral ((n `div` (256 ^ byteOffset)) `mod` 256)
      | byteOffset <- [5, 4 .. 0]
      ]

-- | 10 bytes of pseudo-random entropy (sufficient for testing).
randomEntropy :: IO BS.ByteString
randomEntropy = do
  bytes <- getStdRandom (randomBytesList 10)
  pure (BS.pack bytes)
  where
    randomBytesList :: Int -> StdGen -> ([Word8], StdGen)
    randomBytesList 0 gen = ([], gen)
    randomBytesList n gen
      | n > 0 =
          let (b, gen') = uniformR (0, 255 :: Word8) gen
              (rest, gen'') = randomBytesList (n - 1) gen'
           in (b : rest, gen'')
      | otherwise = ([], gen)

-- | Result of the 'createAdrCommand' operation.
data CreateResult
  = CreateResult
      { createOperationId  :: String
      , createAdrId        :: AdrId
      , createRecordId     :: RecordId
      , createScopeId      :: ConnectionId
      , createDomainId     :: ConnectionId
      , createStatusId     :: ConnectionId
      , createCommitOid    :: GitOid
      , createCreatedPaths :: [RepoPath]
      , createIndexUpdated :: Bool
      }
  deriving (Eq, Show)

-- | Create a new architectural decision record with its scope, domain, and
-- status connection records.
--
-- Creates four managed files in a single atomic commit:
--
-- 1. The decision record (.decision.md).
-- 2. The scope connection record — applies-to relationship.
-- 3. The domain connection record — domains relationship.
-- 4. The status connection record — status relationship.
--
-- Each file is sealed with a provenance capsule that encodes the operation
-- context, actor, and semantic digest.  The commit uses the 8-stage
-- append-only transaction engine.
createAdrCommand ::
  Repository ->
  ManagedPaths ->
  Actor ->
  AdrId ->
  RecordId ->
  T.Text ->
  T.Text ->
  T.Text ->
  [Domain] ->
  [ScopePattern] ->
  Maybe Digest ->
  Maybe Digest ->
  Maybe Digest ->
  IO (Either TransactionError CreateResult)
createAdrCommand
  repository
  managedPaths
  actor
  adrId
  recordId
  title
  summary
  body
  domains
  patterns
  inputDigest
  promptDigest
  contextDigest = do
    oldHeadResult <- resolveRevision repository (RevisionSpec "HEAD")
    case oldHeadResult of
      Left err ->
        pure (Left (Stage3ValidateState ("resolve HEAD: " <> T.pack (show err))))
      Right oldHead -> do
        headStateResult <- repositoryHeadState repository
        let branchName =
              case headStateResult of
                Right (GitHeadAttached ref) ->
                  fromMaybe (gitRefText ref) (T.stripPrefix "refs/heads/" (gitRefText ref))
                Right GitHeadDetached -> "HEAD"
                Left _ -> "HEAD"
        timestampMs <- currentTimestamp
        let timestampBytes = encodeTimestampMs timestampMs
        entropy <- randomEntropy
        let opIdResult = sortableOperationId timestampBytes entropy
            recIdResult = sortableRecordId timestampBytes entropy
            scopeIdResult = sortableConnectionId timestampBytes (createConnectionEntropy "scope" entropy)
            domainIdResult = sortableConnectionId timestampBytes (createConnectionEntropy "domain" entropy)
            statusIdResult = sortableConnectionId timestampBytes (createConnectionEntropy "status" entropy)
        case (opIdResult, recIdResult, scopeIdResult, domainIdResult, statusIdResult) of
          (Right opId, Right recId, Right scopeId, Right domainId, Right statusId) -> do
            let decisionRec =
                  DecisionRecord
                    { decisionAdr = adrId,
                      decisionRecord = recId,
                      decisionTitle = title,
                      decisionSummary = summary,
                      decisionDomains = domains,
                      decisionBody = body
                    }
            let scopeConn =
                  ConnectionRecord
                    { connectionRecordId = scopeId,
                      connectionPayload = AppliesToConnection (AppliesToPayload
                        { appliesToSubjectAdr = adrId
                        , appliesToParentConnections = []
                        , appliesToChange = "initial"
                        , appliesToAdded = patterns
                        , appliesToRemoved = []
                        , appliesToEffective = patterns
                        }),
                      connectionRationale = "Initial scope.\n"
                    }
            let domainConn =
                  ConnectionRecord
                    { connectionRecordId = domainId,
                      connectionPayload = DomainsConnection (DomainsPayload
                        { domainsSubjectAdr = adrId
                        , domainsParentConnections = []
                        , domainsChange = "initial"
                        , domainsAdded = domains
                        , domainsRemoved = []
                        , domainsEffective = domains
                        , domainsRefinements = []
                        }),
                      connectionRationale = "Initial domain.\n"
                    }
            let statusConn =
                  ConnectionRecord
                    { connectionRecordId = statusId,
                      connectionPayload = StatusConnection (StatusPayload
                        { statusSubjectAdr = adrId
                        , statusParentConnections = []
                        , statusState = StatusActive
                        , statusRecordHeads = [recId]
                        , statusReplacementAdr = Nothing
                        }),
                      connectionRationale = "Initial active status.\n"
                    }
            let managedRecords =
                  [ ManagedDecision decisionRec
                  , ManagedConnection scopeConn
                  , ManagedConnection domainConn
                  , ManagedConnection statusConn
                  ]
                operationId = T.unpack (operationIdText opId)
                inputs = ProvenanceInputs inputDigest promptDigest contextDigest
                generatedResult =
                  traverse
                    (sealCreatedRecord opId oldHead branchName actor timestampMs recId inputs managedPaths)
                    managedRecords
            case generatedResult of
              Left transactionError ->
                pure (Left transactionError)
              Right generated -> do
                let config =
                      TransactionConfig
                        { configOperationId = operationId
                        , configSubject = "adrai: create " <> adrIdText adrId
                        , configTrailers =
                            Map.fromList
                              [ ("ADR", T.unpack (adrIdText adrId))
                              , ( "Objects"
                                , intercalate
                                    ","
                                    [ T.unpack (recordIdText recId)
                                    , T.unpack (connectionIdText scopeId)
                                    , T.unpack (connectionIdText domainId)
                                    , T.unpack (connectionIdText statusId)
                                    ]
                                )
                              ]
                        , configExpectedHead = oldHead
                        , configGenerated = generated
                        }
                commitAppendOnlyOperation repository config >>= \case
                  Left transactionError ->
                    pure (Left transactionError)
                  Right TransactionResult {..} ->
                    pure
                      ( Right
                          CreateResult
                            { createOperationId = transactionOperationId
                            , createAdrId = adrId
                            , createRecordId = recId
                            , createScopeId = scopeId
                            , createDomainId = domainId
                            , createStatusId = statusId
                            , createCommitOid = transactionCommitOid
                            , createCreatedPaths = transactionCreatedPaths
                            , createIndexUpdated = transactionIndexUpdated
                            }
                      )
          _ ->
            pure (Left (Stage3ValidateState "failed to generate sortable identifiers"))

-- | Derive independent 10-byte entropy fields for the three connection
-- identities created by one operation. The timestamp field remains shared so
-- sortable ordering is preserved, while the domain tag prevents collisions.
createConnectionEntropy :: T.Text -> BS.ByteString -> BS.ByteString
createConnectionEntropy domainTag entropy =
  BS.take 10 . digestBytes . sha256Digest $
    encodeUtf8 ("adrai:create-connection:" <> domainTag <> "\NUL") <> entropy

sealCreatedRecord ::
  OperationId ->
  GitOid ->
  T.Text ->
  Actor ->
  Integer ->
  RecordId ->
  ProvenanceInputs ->
  ManagedPaths ->
  ManagedRecord ->
  Either TransactionError GeneratedFile
sealCreatedRecord opId oldHead branchName actor timestampMs parentRecord inputs managedPaths managed = do
  semantic <-
    first (Stage5ValidateGenerated . ("create render: " <>) . T.pack . show) $
      case managed of
        ManagedDecision decision -> renderDecisionSemantic decision
        ManagedConnection connection -> renderConnectionSemantic connection
  eventKind <-
    first (Stage5ValidateGenerated . ("create eventKind: " <>) . T.pack . show) $
      mkEventKind eventName
  capsule <-
    first (Stage5ValidateGenerated . ("create capsule: " <>) . T.pack . show) $
      mkProvenanceCapsule
        ProvenanceCapsuleInput
          { capsuleInputOperationId = opId
          , capsuleInputObjectId = objectId
          , capsuleInputEventKind = eventKind
          , capsuleInputActor = actor
          , capsuleInputTimestampMs = timestampMs
          , capsuleInputBasis = oldHead
          , capsuleInputParents = parents
          , capsuleInputBranchHint = Just branchName
          , capsuleInputUpstreamHint = Nothing
          , capsuleInputLineAnchors = []
          , capsuleInputSemanticDigest = semanticDigest semantic
          , capsuleInputToolVersion = "adrai/1.0.0"
          , capsuleInputDigests = inputs
          }
  sealed <-
    first (Stage5ValidateGenerated . ("create seal: " <>) . T.pack . show) $
      sealManagedDocument managed capsule
  path <-
    first (Stage4GenerateFiles . ("create path: " <>) . T.pack . show) $
      canonicalManagedPath managedPaths managed
  pure (GeneratedFile path sealed)
  where
    (objectId, eventName, parents) =
      case managed of
        ManagedDecision decision ->
          (ProvenanceRecord (decisionRecord decision), "decision.create", [])
        ManagedConnection connection ->
          ( ProvenanceConnection (connectionRecordId connection)
          , case connectionPayload connection of
              AppliesToConnection _ -> "scope.initial"
              DomainsConnection _ -> "domain.initial"
              StatusConnection _ -> "status.initial"
              AmendsConnection _ -> "connection.create"
          , [ProvenanceRecord parentRecord]
          )

-- ---------------------------------------------------------------------------
-- Amend ADR
-- ---------------------------------------------------------------------------

-- | Result of the 'amendAdrCommand' operation.
data AmendResult
  = AmendResult
      { amendOperationId  :: String,
        amendAdrId        :: AdrId,
        amendRecordId     :: RecordId,
        amendAmends       :: RecordId,
        amendConnectionId :: ConnectionId,
        amendCommitOid    :: GitOid,
        amendUpdatedPath  :: RepoPath,
        amendCreatedPaths :: [RepoPath],
        amendIndexUpdated :: Bool
      }
  deriving (Eq, Show)

-- | Amend an existing decision record's title, summary, or body.
--
-- Reads the current decision record, validates its provenance capsule,
-- builds an amended record with the new content, and commits via the
-- append-only transaction engine.
--
-- The new capsule carries eventKind @"decision.amend"@.
amendAdrCommand ::
  Repository ->
  ManagedPaths ->
  Actor ->
  AdrId ->
  RecordId ->
  T.Text ->
  T.Text ->
  T.Text ->
  ProvenanceInputs ->
  IO (Either TransactionError AmendResult)
amendAdrCommand
  repository
  _managedPaths
  actor
  adrId
  recordId
  newTitle
  newSummary
  newBody
  inputs = amendWithSource repository actor adrId (selectAmendmentSource adrId recordId) "Amends current decision head.\n" newTitle newSummary newBody inputs

-- | Amend the uniquely current committed decision for an ADR.  The optional
-- state token is checked against the same committed graph snapshot that
-- supplies the source record, avoiding a read-then-amend race in the CLI.
amendCurrentAdrCommand ::
  Repository ->
  Actor ->
  AdrId ->
  Maybe StateToken ->
  T.Text ->
  T.Text ->
  T.Text ->
  T.Text ->
  ProvenanceInputs ->
  IO (Either TransactionError AmendResult)
amendCurrentAdrCommand repository actor adrId expectedState changeSummary newTitle newSummary newBody inputs =
  case normalizeChangeSummary changeSummary of
    Left err -> pure (Left err)
    Right rationale ->
      amendWithSource repository actor adrId (selectCurrentAmendmentSource adrId expectedState) rationale newTitle newSummary newBody inputs

amendWithSource repository actor adrId selectSource rationale newTitle newSummary newBody inputs =
  requireAttachedHead repository >>= \case
    Left err -> pure (Left err)
    Right branchName -> do
      snapshotResult <- repositorySnapshot repository (RevisionSpec "HEAD")
      case snapshotResult of
        Left err -> pure (Left (Stage3ValidateState ("read committed HEAD: " <> T.pack (show err))))
        Right snapshot -> case committedDocuments (repositorySnapshotManagedPaths snapshot) snapshot of
          Left err -> pure (Left err)
          Right documents -> case selectSource documents of
            Left err -> pure (Left err)
            Right (sourceRecord, sourceHead, currentDomains) -> do
              let title = inherit newTitle (decisionTitle sourceRecord)
                  summary = inherit newSummary (decisionSummary sourceRecord)
                  body = inherit newBody (decisionBody sourceRecord)
              if title == decisionTitle sourceRecord && summary == decisionSummary sourceRecord && body == decisionBody sourceRecord
                then pure (Left (Stage3ValidateState "amend would not change the current decision"))
                else createAmendment snapshot branchName sourceHead currentDomains rationale title summary body
  where
    inherit replacement original = if T.null replacement then original else replacement
    createAmendment snapshot branchName sourceHead currentDomains rationale title summary body = do
      timestampMs <- currentTimestamp
      let timestampBytes = encodeTimestampMs timestampMs
      entropy <- randomEntropy
      case (sortableOperationId timestampBytes entropy, sortableRecordId timestampBytes entropy, sortableConnectionId timestampBytes (createConnectionEntropy "amends" entropy)) of
        (Right opId, Right amendedId, Right connectionId) -> do
          let amendedRecord = DecisionRecord adrId amendedId title summary currentDomains body
              amendsConnection = ConnectionRecord connectionId (AmendsConnection (AmendsPayload adrId amendedId [sourceHead])) rationale
              members = [ManagedDecision amendedRecord, ManagedConnection amendsConnection]
              paths = repositorySnapshotManagedPaths snapshot
          case traverse (sealAmendMember opId (repositorySnapshotRevision snapshot) branchName actor timestampMs sourceHead inputs paths) members of
            Left err -> pure (Left err)
            Right generated -> do
              let operationText = T.unpack (operationIdText opId)
                  config = TransactionConfig operationText ("adrai: amend " <> adrIdText adrId)
                    (Map.fromList [("ADR", T.unpack (adrIdText adrId)), ("Objects", intercalate "," [T.unpack (recordIdText amendedId), T.unpack (connectionIdText connectionId)])])
                    (resolvedCommitOid (repositorySnapshotRevision snapshot)) generated
              commitAppendOnlyOperation repository config >>= \case
                Left transactionError -> pure (Left transactionError)
                Right TransactionResult {..} -> case transactionCreatedPaths of
                  decisionPath : _ -> pure (Right AmendResult
                    { amendOperationId = transactionOperationId, amendAdrId = adrId, amendRecordId = amendedId, amendAmends = sourceHead,
                      amendConnectionId = connectionId, amendCommitOid = transactionCommitOid, amendUpdatedPath = decisionPath,
                      amendCreatedPaths = transactionCreatedPaths, amendIndexUpdated = transactionIndexUpdated })
                  [] -> pure (Left (Stage8UpdateRef "amend transaction reported no created paths"))
        _ -> pure (Left (Stage3ValidateState "failed to generate sortable identifiers"))

-- | Backwards-compatible spelling retained for existing explorer callers.
amendAdmCommand ::
  Repository ->
  ManagedPaths ->
  Actor ->
  AdrId ->
  RecordId ->
  T.Text ->
  T.Text ->
  T.Text ->
  ProvenanceInputs ->
  IO (Either TransactionError AmendResult)
amendAdmCommand = amendAdrCommand

requireAttachedHead :: Repository -> IO (Either TransactionError T.Text)
requireAttachedHead repository = do
  repositoryHeadState repository >>= \case
    Left err -> pure (Left (Stage3ValidateState ("symbolic-ref HEAD failed: " <> T.pack (show err))))
    Right GitHeadDetached -> pure (Left (Stage3ValidateState "HEAD is detached; attach a branch first"))
    Right (GitHeadAttached ref) ->
      pure (Right (fromMaybe (gitRefText ref) (T.stripPrefix "refs/heads/" (gitRefText ref))))

committedDocuments paths snapshot =
  traverse parseEntry (repositorySnapshotEntries snapshot)
  where
    parseEntry observation =
      case repositoryTreeBlob observation of
        Nothing -> Left (Stage3ValidateState "committed managed source contains a non-blob entry")
        Just blob -> do
          document <- first (Stage3ValidateState . ("parse committed managed document: " <>) . T.pack . show) $
            parseManagedDocument (gitTreePath (repositoryTreeEntry observation)) (gitBlobBytes blob)
          first (Stage3ValidateState . ("validate committed managed location: " <>) . T.pack . show) $
            validateManagedLocation paths document
          Right document

selectAmendmentSource :: AdrId -> RecordId -> [ParsedManagedDocument] -> Either TransactionError (DecisionRecord, RecordId, [Domain])
selectAmendmentSource adr requestedRecord documents = do
  let reduction = reduceManagedGraph (map parsedManagedRecord documents)
  reduced <- maybe (Left (Stage3ValidateState "amend target ADR is unknown")) Right (lookupReducedAdr adr reduction)
  if not (null (reducedConflictAxes reduced))
    then Left (Stage3ValidateState "amend target ADR is conflicted")
    else pure ()
  status <- maybe (Left (Stage3ValidateState "amend target ADR has no current status")) Right (axisResolutionEffective (reducedStatusAxis reduced))
  if reducedStatusState status /= StatusActive
    then Left (Stage3ValidateState "amend target ADR is not active")
    else pure ()
  case (axisResolutionHeads (reducedDecisionAxis reduced), axisResolutionEffective (reducedDecisionAxis reduced)) of
    ([head], Just record)
      | head == requestedRecord -> Right (record, head, axisResolutionEffective (reducedDomainAxis reduced))
      | otherwise -> Left (Stage3ValidateState "amend target record is not the current decision head")
    _ -> Left (Stage3ValidateState "amend target ADR has no unambiguous current decision")

selectCurrentAmendmentSource :: AdrId -> Maybe StateToken -> [ParsedManagedDocument] -> Either TransactionError (DecisionRecord, RecordId, [Domain])
selectCurrentAmendmentSource adr expected documents = do
  let reduction = reduceManagedGraph (map parsedManagedRecord documents)
  reduced <- maybe (Left (Stage3ValidateState "amend target ADR is unknown")) Right (lookupReducedAdr adr reduction)
  case expected of
    Nothing -> Right ()
    Just expectedToken
      | expectedToken == reducedStateToken reduced -> Right ()
      | otherwise ->
          Left (Stage3ValidateState ("stale ADR state: expected " <> stateTokenText expectedToken <> ", current state is " <> stateTokenText (reducedStateToken reduced)))
  case axisResolutionHeads (reducedDecisionAxis reduced) of
    [head] -> selectAmendmentSource adr head documents
    _ -> Left (Stage3ValidateState "amend target ADR has no unambiguous current decision")

normalizeChangeSummary :: T.Text -> Either TransactionError T.Text
normalizeChangeSummary summary
  | T.null normalized = Left (Stage3ValidateState "amend change summary must be nonblank")
  | otherwise = Right (normalized <> "\n")
  where
    normalized = T.strip summary

sealAmendMember :: OperationId -> ResolvedRepositoryRevision -> T.Text -> Actor -> Integer -> RecordId -> ProvenanceInputs -> ManagedPaths -> ManagedRecord -> Either TransactionError GeneratedFile
sealAmendMember opId revision branchName actor timestampMs priorHead inputs paths managed = do
  semantic <- first (Stage5ValidateGenerated . ("amend render: " <>) . T.pack . show) $
    case managed of
      ManagedDecision decision -> renderDecisionSemantic decision
      ManagedConnection connection -> renderConnectionSemantic connection
  eventKind <- first (Stage5ValidateGenerated . ("amend eventKind: " <>) . T.pack . show) $
    mkEventKind $ case managed of
      ManagedDecision _ -> "decision.amend"
      ManagedConnection _ -> "connection.amends"
  capsule <- first (Stage5ValidateGenerated . ("amend capsule: " <>) . T.pack . show) $
    mkProvenanceCapsule
      ProvenanceCapsuleInput
        { capsuleInputOperationId = opId,
          capsuleInputObjectId = case managed of
            ManagedDecision decision -> ProvenanceRecord (decisionRecord decision)
            ManagedConnection connection -> ProvenanceConnection (connectionRecordId connection),
          capsuleInputEventKind = eventKind,
          capsuleInputActor = actor,
          capsuleInputTimestampMs = timestampMs,
          capsuleInputBasis = resolvedCommitOid revision,
          capsuleInputParents = [ProvenanceRecord priorHead],
          capsuleInputBranchHint = Just branchName,
          capsuleInputUpstreamHint = Nothing,
          capsuleInputLineAnchors = [],
          capsuleInputSemanticDigest = semanticDigest semantic,
          capsuleInputToolVersion = "adrai/1.0.0",
          capsuleInputDigests = inputs
        }
  sealed <- first (Stage5ValidateGenerated . ("amend seal: " <>) . T.pack . show) (sealManagedDocument managed capsule)
  path <- first (Stage4GenerateFiles . ("amend path: " <>) . T.pack . show) (canonicalManagedPath paths managed)
  pure (GeneratedFile path sealed)

-- ---------------------------------------------------------------------------
-- Change Scope
-- ---------------------------------------------------------------------------

-- | Result of the 'changeScopeCommand' operation.
data ScopeChangeResult
  = ScopeChangeResult
      { scopeChangeOperationId :: String,
        scopeChangeAdrId      :: AdrId,
        scopeChangeConnectionId :: ConnectionId,
        scopeChangeCommitOid  :: GitOid,
        scopeChangeNewPath    :: RepoPath
      }
  deriving (Eq, Show)

-- | Change the scope patterns applied to an ADR.
--
-- Adds and/or removes scope patterns, builds a new AppliesToPayload,
-- and commits via the append-only transaction engine.
--
-- The new capsule carries eventKind @"scope.update"@.
changeScopeCommand ::
  Repository ->
  ManagedPaths ->
  Actor ->
  AdrId ->
  [ScopePattern] -> -- added
  [ScopePattern] -> -- removed
  ProvenanceInputs ->
  IO (Either TransactionError ScopeChangeResult)
changeScopeCommand
  repository
  managedPaths
  actor
  adrId
  added
  removed
  inputs = do
    oldHeadResult <- resolveRevision repository (RevisionSpec "HEAD")
    case oldHeadResult of
      Left err ->
        pure (Left (Stage3ValidateState ("resolve HEAD: " <> T.pack (show err))))
      Right oldHead -> do
        timestampMs <- currentTimestampMs
        entropy <- randomEntropy
        let opIdResult = sortableOperationId timestampMs entropy
            connIdResult = sortableConnectionId timestampMs entropy
        case (opIdResult, connIdResult) of
          (Right opId, Right connId) -> do
            let effective = sort (added <> removed)
                connRecord =
                  ConnectionRecord
                    { connectionRecordId = connId,
                      connectionPayload = AppliesToConnection (AppliesToPayload
                        { appliesToSubjectAdr = adrId
                        , appliesToParentConnections = []
                        , appliesToChange = "scope update"
                        , appliesToAdded = added
                        , appliesToRemoved = removed
                        , appliesToEffective = effective
                        }),
                      connectionRationale = "Scope change operation"
                    }
            let semanticEither = renderConnectionSemantic connRecord
            case semanticEither of
              Left docErr ->
                pure (Left (Stage5ValidateGenerated ("scope render: " <> T.pack (show docErr))))
              Right semantic -> do
                let digest = semanticDigest semantic
                    eventIdResult = mkEventKind "scope.update"
                case eventIdResult of
                  Left provErr ->
                    pure (Left (Stage5ValidateGenerated ("scope eventKind: " <> T.pack (show provErr))))
                  Right eventKind -> do
                    let parentId = ProvenanceConnection connId
                        ts = 1000000000000
                        capsuleInput =
                          ProvenanceCapsuleInput
                            { capsuleInputOperationId = opId,
                              capsuleInputObjectId = ProvenanceConnection connId,
                              capsuleInputEventKind = eventKind,
                              capsuleInputActor = actor,
                              capsuleInputTimestampMs = ts,
                              capsuleInputBasis = oldHead,
                              capsuleInputParents = [parentId],
                              capsuleInputBranchHint = Nothing,
                              capsuleInputUpstreamHint = Nothing,
                              capsuleInputLineAnchors = [],
                              capsuleInputSemanticDigest = digest,
                              capsuleInputToolVersion = "adrai/0.1.0",
                              capsuleInputDigests = inputs
                            }
                    let capsuleResult = mkProvenanceCapsule capsuleInput
                    case capsuleResult of
                      Left provErr ->
                        pure (Left (Stage5ValidateGenerated ("scope capsule: " <> T.pack (show provErr))))
                      Right capsule -> do
                        let sealedEither = sealManagedDocument (ManagedConnection connRecord) capsule
                        case sealedEither of
                          Left docErr ->
                            pure (Left (Stage5ValidateGenerated ("scope seal: " <> T.pack (show docErr))))
                          Right sealedBytes -> do
                            let generatedPath = canonicalManagedPath managedPaths (ManagedConnection connRecord)
                            case generatedPath of
                              Left err ->
                                pure (Left (Stage4GenerateFiles ("scope path: " <> T.pack (show err))))
                              Right genPath -> do
                                let generated =
                                      [ GeneratedFile genPath sealedBytes ]
                                    config =
                                      TransactionConfig
                                        { configOperationId = T.unpack (operationIdText opId),
                                          configSubject = "adrai: scope " <> adrIdText adrId,
                                          configTrailers = Map.fromList [("ADR", T.unpack (adrIdText adrId)), ("Objects", T.unpack (connectionIdText connId))],
                                          configExpectedHead = oldHead,
                                          configGenerated = generated
                                        }
                                _ <- commitAppendOnlyOperation repository config
                                pure (Right (ScopeChangeResult
                                  (T.unpack (operationIdText opId))
                                  adrId connId oldHead genPath))
          _ ->
            pure (Left (Stage3ValidateState "failed to generate sortable identifiers"))

-- ---------------------------------------------------------------------------
-- Change Domain
-- ---------------------------------------------------------------------------

-- | Result of the 'changeDomainCommand' operation.
data DomainChangeResult
  = DomainChangeResult
      { domainChangeOperationId :: String,
        domainChangeAdrId      :: AdrId,
        domainChangeConnectionId :: ConnectionId,
        domainChangeCommitOid  :: GitOid,
        domainChangeNewPath    :: RepoPath
      }
  deriving (Eq, Show)

-- | Change the domain assignment for an ADR.
--
-- Adds and/or removes domains, builds a new DomainsPayload,
-- and commits via the append-only transaction engine.
--
-- The new capsule carries eventKind @"domain.update"@.
changeDomainCommand ::
  Repository ->
  ManagedPaths ->
  Actor ->
  AdrId ->
  [Domain] -> -- added
  [Domain] -> -- removed
  ProvenanceInputs ->
  IO (Either TransactionError DomainChangeResult)
changeDomainCommand
  repository
  managedPaths
  actor
  adrId
  added
  removed
  inputs = do
    oldHeadResult <- resolveRevision repository (RevisionSpec "HEAD")
    case oldHeadResult of
      Left err ->
        pure (Left (Stage3ValidateState ("resolve HEAD: " <> T.pack (show err))))
      Right oldHead -> do
        timestampMs <- currentTimestampMs
        entropy <- randomEntropy
        let opIdResult = sortableOperationId timestampMs entropy
            connIdResult = sortableConnectionId timestampMs entropy
        case (opIdResult, connIdResult) of
          (Right opId, Right connId) -> do
            let effective = added <> removed
                connRecord =
                  ConnectionRecord
                    { connectionRecordId = connId,
                      connectionPayload = DomainsConnection (DomainsPayload
                        { domainsSubjectAdr = adrId
                        , domainsParentConnections = []
                        , domainsChange = "domain update"
                        , domainsAdded = added
                        , domainsRemoved = removed
                        , domainsEffective = effective
                        , domainsRefinements = []
                        }),
                      connectionRationale = "Domain change operation"
                    }
            let semanticEither = renderConnectionSemantic connRecord
            case semanticEither of
              Left docErr ->
                pure (Left (Stage5ValidateGenerated ("domain render: " <> T.pack (show docErr))))
              Right semantic -> do
                let digest = semanticDigest semantic
                    eventIdResult = mkEventKind "domain.update"
                case eventIdResult of
                  Left provErr ->
                    pure (Left (Stage5ValidateGenerated ("domain eventKind: " <> T.pack (show provErr))))
                  Right eventKind -> do
                    let parentId = ProvenanceConnection connId
                        ts = 1000000000000
                        capsuleInput =
                          ProvenanceCapsuleInput
                            { capsuleInputOperationId = opId,
                              capsuleInputObjectId = ProvenanceConnection connId,
                              capsuleInputEventKind = eventKind,
                              capsuleInputActor = actor,
                              capsuleInputTimestampMs = ts,
                              capsuleInputBasis = oldHead,
                              capsuleInputParents = [parentId],
                              capsuleInputBranchHint = Nothing,
                              capsuleInputUpstreamHint = Nothing,
                              capsuleInputLineAnchors = [],
                              capsuleInputSemanticDigest = digest,
                              capsuleInputToolVersion = "adrai/0.1.0",
                              capsuleInputDigests = inputs
                            }
                    let capsuleResult = mkProvenanceCapsule capsuleInput
                    case capsuleResult of
                      Left provErr ->
                        pure (Left (Stage5ValidateGenerated ("domain capsule: " <> T.pack (show provErr))))
                      Right capsule -> do
                        let sealedEither = sealManagedDocument (ManagedConnection connRecord) capsule
                        case sealedEither of
                          Left docErr ->
                            pure (Left (Stage5ValidateGenerated ("domain seal: " <> T.pack (show docErr))))
                          Right sealedBytes -> do
                            let generatedPath = canonicalManagedPath managedPaths (ManagedConnection connRecord)
                            case generatedPath of
                              Left err ->
                                pure (Left (Stage4GenerateFiles ("domain path: " <> T.pack (show err))))
                              Right genPath -> do
                                let generated =
                                      [ GeneratedFile genPath sealedBytes ]
                                    config =
                                      TransactionConfig
                                        { configOperationId = T.unpack (operationIdText opId),
                                          configSubject = "adrai: domain " <> adrIdText adrId,
                                          configTrailers = Map.fromList [("ADR", T.unpack (adrIdText adrId)), ("Objects", T.unpack (connectionIdText connId))],
                                          configExpectedHead = oldHead,
                                          configGenerated = generated
                                        }
                                _ <- commitAppendOnlyOperation repository config
                                pure (Right (DomainChangeResult
                                  (T.unpack (operationIdText opId))
                                  adrId connId oldHead genPath))
          _ ->
            pure (Left (Stage3ValidateState "failed to generate sortable identifiers"))

-- ---------------------------------------------------------------------------
-- Obsolete ADR
-- ---------------------------------------------------------------------------

-- | Result of the 'obsoleteCommand' operation.
data ObsoleteResult
  = ObsoleteResult
      { obsoleteOperationId :: String,
        obsoleteAdrId       :: AdrId,
        obsoleteConnectionId :: ConnectionId,
        obsoleteCommitOid   :: GitOid,
        obsoleteNewPath     :: RepoPath
      }
  deriving (Eq, Show)

-- | Mark an ADR as obsolete.
--
-- Updates the status connection to @StatusObsolete@ and commits via the
-- append-only transaction engine.
--
-- The new capsule carries eventKind @"decision.obsolete"@.
obsoleteCommand ::
  Repository ->
  ManagedPaths ->
  Actor ->
  AdrId ->
  RecordId ->
  ProvenanceInputs ->
  IO (Either TransactionError ObsoleteResult)
obsoleteCommand
  repository
  managedPaths
  actor
  adrId
  recordId
  inputs = do
    oldHeadResult <- resolveRevision repository (RevisionSpec "HEAD")
    case oldHeadResult of
      Left err ->
        pure (Left (Stage3ValidateState ("resolve HEAD: " <> T.pack (show err))))
      Right oldHead -> do
        timestampMs <- currentTimestampMs
        entropy <- randomEntropy
        let opIdResult = sortableOperationId timestampMs entropy
            connIdResult = sortableConnectionId timestampMs entropy
        case (opIdResult, connIdResult) of
          (Right opId, Right connId) -> do
            let statusRecord =
                  ConnectionRecord
                    { connectionRecordId = connId,
                      connectionPayload = StatusConnection (StatusPayload
                        { statusSubjectAdr = adrId
                        , statusParentConnections = []
                        , statusState = StatusObsolete
                        , statusRecordHeads = [recordId]
                        , statusReplacementAdr = Nothing
                        }),
                      connectionRationale = "ADR marked obsolete"
                    }
            let semanticEither = renderConnectionSemantic statusRecord
            case semanticEither of
              Left docErr ->
                pure (Left (Stage5ValidateGenerated ("obsolete render: " <> T.pack (show docErr))))
              Right semantic -> do
                let digest = semanticDigest semantic
                    eventIdResult = mkEventKind "decision.obsolete"
                case eventIdResult of
                  Left provErr ->
                    pure (Left (Stage5ValidateGenerated ("obsolete eventKind: " <> T.pack (show provErr))))
                  Right eventKind -> do
                    let parentId = ProvenanceConnection connId
                        ts = 1000000000000
                        capsuleInput =
                          ProvenanceCapsuleInput
                            { capsuleInputOperationId = opId,
                              capsuleInputObjectId = ProvenanceConnection connId,
                              capsuleInputEventKind = eventKind,
                              capsuleInputActor = actor,
                              capsuleInputTimestampMs = ts,
                              capsuleInputBasis = oldHead,
                              capsuleInputParents = [parentId],
                              capsuleInputBranchHint = Nothing,
                              capsuleInputUpstreamHint = Nothing,
                              capsuleInputLineAnchors = [],
                              capsuleInputSemanticDigest = digest,
                              capsuleInputToolVersion = "adrai/0.1.0",
                              capsuleInputDigests = inputs
                            }
                    let capsuleResult = mkProvenanceCapsule capsuleInput
                    case capsuleResult of
                      Left provErr ->
                        pure (Left (Stage5ValidateGenerated ("obsolete capsule: " <> T.pack (show provErr))))
                      Right capsule -> do
                        let sealedEither = sealManagedDocument (ManagedConnection statusRecord) capsule
                        case sealedEither of
                          Left docErr ->
                            pure (Left (Stage5ValidateGenerated ("obsolete seal: " <> T.pack (show docErr))))
                          Right sealedBytes -> do
                            let generatedPath = canonicalManagedPath managedPaths (ManagedConnection statusRecord)
                            case generatedPath of
                              Left err ->
                                pure (Left (Stage4GenerateFiles ("obsolete path: " <> T.pack (show err))))
                              Right genPath -> do
                                let generated =
                                      [ GeneratedFile genPath sealedBytes ]
                                    config =
                                      TransactionConfig
                                        { configOperationId = T.unpack (operationIdText opId),
                                          configSubject = "adrai: obsolete " <> adrIdText adrId,
                                          configTrailers = Map.fromList [("ADR", T.unpack (adrIdText adrId)), ("Objects", T.unpack (connectionIdText connId))],
                                          configExpectedHead = oldHead,
                                          configGenerated = generated
                                        }
                                _ <- commitAppendOnlyOperation repository config
                                pure (Right (ObsoleteResult
                                  (T.unpack (operationIdText opId))
                                  adrId connId oldHead genPath))
          _ ->
            pure (Left (Stage3ValidateState "failed to generate sortable identifiers"))

-- ---------------------------------------------------------------------------
-- Reactivate ADR
-- ---------------------------------------------------------------------------

-- | Result of the 'reactivateCommand' operation.
data ReactivateResult
  = ReactivateResult
      { reactivateOperationId :: String,
        reactivateAdrId       :: AdrId,
        reactivateConnectionId :: ConnectionId,
        reactivateCommitOid   :: GitOid,
        reactivateNewPath     :: RepoPath
      }
  deriving (Eq, Show)

-- | Reactivate an obsolete ADR back to active status.
--
-- Updates the status connection to @StatusActive@ and commits via the
-- append-only transaction engine.
--
-- The new capsule carries eventKind @"decision.reactivate"@.
reactivateCommand ::
  Repository ->
  ManagedPaths ->
  Actor ->
  AdrId ->
  RecordId ->
  ProvenanceInputs ->
  IO (Either TransactionError ReactivateResult)
reactivateCommand
  repository
  managedPaths
  actor
  adrId
  recordId
  inputs = do
    oldHeadResult <- resolveRevision repository (RevisionSpec "HEAD")
    case oldHeadResult of
      Left err ->
        pure (Left (Stage3ValidateState ("resolve HEAD: " <> T.pack (show err))))
      Right oldHead -> do
        timestampMs <- currentTimestampMs
        entropy <- randomEntropy
        let opIdResult = sortableOperationId timestampMs entropy
            connIdResult = sortableConnectionId timestampMs entropy
        case (opIdResult, connIdResult) of
          (Right opId, Right connId) -> do
            let statusRecord =
                  ConnectionRecord
                    { connectionRecordId = connId,
                      connectionPayload = StatusConnection (StatusPayload
                        { statusSubjectAdr = adrId
                        , statusParentConnections = []
                        , statusState = StatusActive
                        , statusRecordHeads = [recordId]
                        , statusReplacementAdr = Nothing
                        }),
                      connectionRationale = "ADR reactivated"
                    }
            let semanticEither = renderConnectionSemantic statusRecord
            case semanticEither of
              Left docErr ->
                pure (Left (Stage5ValidateGenerated ("reactivate render: " <> T.pack (show docErr))))
              Right semantic -> do
                let digest = semanticDigest semantic
                    eventIdResult = mkEventKind "decision.reactivate"
                case eventIdResult of
                  Left provErr ->
                    pure (Left (Stage5ValidateGenerated ("reactivate eventKind: " <> T.pack (show provErr))))
                  Right eventKind -> do
                    let parentId = ProvenanceConnection connId
                        ts = 1000000000000
                        capsuleInput =
                          ProvenanceCapsuleInput
                            { capsuleInputOperationId = opId,
                              capsuleInputObjectId = ProvenanceConnection connId,
                              capsuleInputEventKind = eventKind,
                              capsuleInputActor = actor,
                              capsuleInputTimestampMs = ts,
                              capsuleInputBasis = oldHead,
                              capsuleInputParents = [parentId],
                              capsuleInputBranchHint = Nothing,
                              capsuleInputUpstreamHint = Nothing,
                              capsuleInputLineAnchors = [],
                              capsuleInputSemanticDigest = digest,
                              capsuleInputToolVersion = "adrai/0.1.0",
                              capsuleInputDigests = inputs
                            }
                    let capsuleResult = mkProvenanceCapsule capsuleInput
                    case capsuleResult of
                      Left provErr ->
                        pure (Left (Stage5ValidateGenerated ("reactivate capsule: " <> T.pack (show provErr))))
                      Right capsule -> do
                        let sealedEither = sealManagedDocument (ManagedConnection statusRecord) capsule
                        case sealedEither of
                          Left docErr ->
                            pure (Left (Stage5ValidateGenerated ("reactivate seal: " <> T.pack (show docErr))))
                          Right sealedBytes -> do
                            let generatedPath = canonicalManagedPath managedPaths (ManagedConnection statusRecord)
                            case generatedPath of
                              Left err ->
                                pure (Left (Stage4GenerateFiles ("reactivate path: " <> T.pack (show err))))
                              Right genPath -> do
                                let generated =
                                      [ GeneratedFile genPath sealedBytes ]
                                    config =
                                      TransactionConfig
                                        { configOperationId = T.unpack (operationIdText opId),
                                          configSubject = "adrai: reactivate " <> adrIdText adrId,
                                          configTrailers = Map.fromList [("ADR", T.unpack (adrIdText adrId)), ("Objects", T.unpack (connectionIdText connId))],
                                          configExpectedHead = oldHead,
                                          configGenerated = generated
                                        }
                                _ <- commitAppendOnlyOperation repository config
                                pure (Right (ReactivateResult
                                  (T.unpack (operationIdText opId))
                                  adrId connId oldHead genPath))
          _ ->
            pure (Left (Stage3ValidateState "failed to generate sortable identifiers"))
