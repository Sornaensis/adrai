{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

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
    ScopeChangeRequest (..),
    ScopeChangeResult (..),
    changeScopeCommand,
    DomainChangeRequest (..),
    DomainChangeResult (..),
    changeDomainCommand,
    ObsoleteRequest (..),
    ObsoleteResult (..),
    obsoleteCommand,
    ReactivateRequest (..),
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
    normalizeLineEndings,
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
    DomainRefinement,
    domainRefinementParent,
    domainRefinementChild,
    domainIsWithin,
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
    GraphAxis (DecisionAxis, ScopeAxis, DomainAxis, StatusAxis),
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
import qualified Data.Set as Set
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
        amendAmends       :: [RecordId],
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
      amendWithSource repository actor adrId (selectCurrentAmendmentSource adrId expectedState newTitle newSummary newBody) rationale newTitle newSummary newBody inputs

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
            Right (sourceRecord, sourceHeads, currentDomains) -> do
              let (title, summary, body, unchanged) =
                    case sourceRecord of
                      Just source ->
                        ( inherit newTitle (decisionTitle source),
                          inherit newSummary (decisionSummary source),
                          inherit newBody (decisionBody source),
                          inherit newTitle (decisionTitle source) == decisionTitle source
                            && inherit newSummary (decisionSummary source) == decisionSummary source
                            && inherit newBody (decisionBody source) == decisionBody source
                        )
                      Nothing -> (newTitle, newSummary, newBody, False)
              if unchanged
                then pure (Left (Stage3ValidateState "amend would not change the current decision"))
                else createAmendment snapshot branchName sourceHeads currentDomains rationale title summary body
  where
    inherit replacement original = if T.null replacement then original else replacement
    createAmendment snapshot branchName sourceHeads currentDomains rationale title summary body = do
      timestampMs <- currentTimestamp
      let timestampBytes = encodeTimestampMs timestampMs
      entropy <- randomEntropy
      case (sortableOperationId timestampBytes entropy, sortableRecordId timestampBytes entropy, sortableConnectionId timestampBytes (createConnectionEntropy "amends" entropy)) of
        (Right opId, Right amendedId, Right connectionId) -> do
          let amendedRecord = DecisionRecord adrId amendedId title summary currentDomains body
              amendsConnection = ConnectionRecord connectionId (AmendsConnection (AmendsPayload adrId amendedId sourceHeads)) rationale
              members = [ManagedDecision amendedRecord, ManagedConnection amendsConnection]
              paths = repositorySnapshotManagedPaths snapshot
          case traverse (sealAmendMember opId (repositorySnapshotRevision snapshot) branchName actor timestampMs sourceHeads inputs paths) members of
            Left err -> pure (Left err)
            Right generated -> do
              let operationText = T.unpack (operationIdText opId)
                  config = TransactionConfig operationText ("adrai: amend " <> adrIdText adrId)
                    (Map.fromList [("ADR", T.unpack (adrIdText adrId)), ("Objects", intercalate "," [T.unpack (recordIdText amendedId), T.unpack (connectionIdText connectionId)])])
                    (resolvedCommitOid (repositorySnapshotRevision snapshot)) generated
              commitAppendOnlyOperation repository config >>= \case
                Left transactionError -> pure (Left transactionError)
                Right TransactionResult {..} -> case (sourceHeads, transactionCreatedPaths) of
                  (_ : _, decisionPath : _) -> pure (Right AmendResult
                    { amendOperationId = transactionOperationId, amendAdrId = adrId, amendRecordId = amendedId, amendAmends = sourceHeads,
                      amendConnectionId = connectionId, amendCommitOid = transactionCommitOid, amendUpdatedPath = decisionPath,
                      amendCreatedPaths = transactionCreatedPaths, amendIndexUpdated = transactionIndexUpdated })
                  ([], _) -> pure (Left (Stage3ValidateState "amend target ADR has no current decision"))
                  (_, []) -> pure (Left (Stage8UpdateRef "amend transaction reported no created paths"))
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

selectAmendmentSource :: AdrId -> RecordId -> [ParsedManagedDocument] -> Either TransactionError (Maybe DecisionRecord, [RecordId], [Domain])
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
      | head == requestedRecord -> Right (Just record, [head], axisResolutionEffective (reducedDomainAxis reduced))
      | otherwise -> Left (Stage3ValidateState "amend target record is not the current decision head")
    _ -> Left (Stage3ValidateState "amend target ADR has no unambiguous current decision")

selectCurrentAmendmentSource :: AdrId -> Maybe StateToken -> T.Text -> T.Text -> T.Text -> [ParsedManagedDocument] -> Either TransactionError (Maybe DecisionRecord, [RecordId], [Domain])
selectCurrentAmendmentSource adr expected newTitle newSummary newBody documents = do
  let reduction = reduceManagedGraph (map parsedManagedRecord documents)
  reduced <- maybe (Left (Stage3ValidateState "amend target ADR is unknown")) Right (lookupReducedAdr adr reduction)
  case expected of
    Nothing -> Right ()
    Just expectedToken
      | expectedToken == reducedStateToken reduced -> Right ()
      | otherwise ->
          Left (Stage3ValidateState ("stale ADR state: expected " <> stateTokenText expectedToken <> ", current state is " <> stateTokenText (reducedStateToken reduced)))
  let decisionHeads = sort (axisResolutionHeads (reducedDecisionAxis reduced))
      nonDecisionConflicts = filter (/= DecisionAxis) (reducedConflictAxes reduced)
  if not (null nonDecisionConflicts)
    then Left (Stage3ValidateState "amend target ADR is conflicted")
    else pure ()
  status <- maybe (Left (Stage3ValidateState "amend target ADR has no current status")) Right (axisResolutionEffective (reducedStatusAxis reduced))
  if reducedStatusState status /= StatusActive
    then Left (Stage3ValidateState "amend target ADR is not active")
    else pure ()
  case decisionHeads of
    [head] -> selectAmendmentSource adr head documents
    heads
      | null heads -> Left (Stage3ValidateState "amend target ADR has no unambiguous current decision")
      | null nonDecisionConflicts && not (T.null newTitle || T.null newSummary || T.null newBody) ->
          Right (Nothing, heads, axisResolutionEffective (reducedDomainAxis reduced))
      | null nonDecisionConflicts ->
          Left (Stage3ValidateState "amend decision conflict requires title, summary, and body")
      | otherwise -> Left (Stage3ValidateState "amend target ADR is conflicted")

normalizeChangeSummary :: T.Text -> Either TransactionError T.Text
normalizeChangeSummary summary
  | T.null normalized = Left (Stage3ValidateState "amend change summary must be nonblank")
  | otherwise = Right (normalized <> "\n")
  where
    normalized = T.strip summary

sealAmendMember :: OperationId -> ResolvedRepositoryRevision -> T.Text -> Actor -> Integer -> [RecordId] -> ProvenanceInputs -> ManagedPaths -> ManagedRecord -> Either TransactionError GeneratedFile
sealAmendMember opId revision branchName actor timestampMs priorHeads inputs paths managed = do
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
          capsuleInputParents = map ProvenanceRecord priorHeads,
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
        scopeChangeParents :: [ConnectionId],
        scopeChangeMode :: T.Text,
        scopeChangeEffective :: [ScopePattern],
        scopeChangeCommitOid  :: GitOid,
        scopeChangeNewPath    :: RepoPath,
        scopeChangeCreatedPaths :: [RepoPath],
        scopeChangeIndexUpdated :: Bool
      }
  deriving (Eq, Show)

-- | The two intentionally narrow scope-update modes.  A delta is only safe
-- against one current scope head; a reviewed set can also reconcile the scope
-- axis when it has multiple current heads.
data ScopeChangeRequest
  = ScopeDelta
      { scopeDeltaAdded :: [ScopePattern],
        scopeDeltaRemoved :: [ScopePattern]
      }
  | ScopeReviewedSet
      { scopeReviewedPatterns :: [ScopePattern]
      }
  deriving (Eq, Show)

-- | Change the scope patterns applied to an ADR.
--
-- Adds and/or removes scope patterns, builds a new AppliesToPayload,
-- and commits via the append-only transaction engine.
--
-- The new capsule carries an event kind derived from its scope change mode.
changeScopeCommand ::
  Repository ->
  ManagedPaths ->
  Actor ->
  AdrId ->
  Maybe StateToken ->
  T.Text -> -- reason
  ScopeChangeRequest ->
  ProvenanceInputs ->
  IO (Either TransactionError ScopeChangeResult)
changeScopeCommand
  repository
  managedPaths
  actor
  adrId
  expectedState
  reason
  request
  inputs =
    case normalizeScopeReason reason of
      Left err -> pure (Left err)
      Right rationale ->
        requireAttachedHead repository >>= \case
          Left err -> pure (Left err)
          Right branchName ->
            repositorySnapshot repository (RevisionSpec "HEAD") >>= \case
              Left err -> pure (Left (Stage3ValidateState ("read committed HEAD: " <> T.pack (show err))))
              Right snapshot ->
                case committedDocuments (repositorySnapshotManagedPaths snapshot) snapshot >>= selectScopeUpdate adrId expectedState request of
                  Left err -> pure (Left err)
                  Right (parents, changeKind, added, removed, effective) ->
                    createScopeUpdate snapshot branchName parents changeKind added removed effective rationale
  where
    createScopeUpdate snapshot branchName parents changeKind added removed effective rationale = do
      timestampMs <- currentTimestamp
      let timestampBytes = encodeTimestampMs timestampMs
      entropy <- randomEntropy
      case (sortableOperationId timestampBytes entropy, sortableConnectionId timestampBytes entropy) of
        (Right opId, Right connId) -> do
          let connRecord = ConnectionRecord
                { connectionRecordId = connId
                , connectionPayload = AppliesToConnection AppliesToPayload
                    { appliesToSubjectAdr = adrId
                    , appliesToParentConnections = parents
                    , appliesToChange = changeKind
                    , appliesToAdded = added
                    , appliesToRemoved = removed
                    , appliesToEffective = effective
                    }
                , connectionRationale = rationale
                }
              paths = repositorySnapshotManagedPaths snapshot
          case sealScopeUpdate opId (repositorySnapshotRevision snapshot) branchName actor timestampMs parents inputs paths connRecord of
            Left err -> pure (Left err)
            Right generated -> do
              let config = TransactionConfig
                    { configOperationId = T.unpack (operationIdText opId)
                    , configSubject = "adrai: scope " <> adrIdText adrId
                    , configTrailers = Map.fromList [("ADR", T.unpack (adrIdText adrId)), ("Objects", T.unpack (connectionIdText connId))]
                    , configExpectedHead = resolvedCommitOid (repositorySnapshotRevision snapshot)
                    , configGenerated = [generated]
                    }
              commitAppendOnlyOperation repository config >>= \case
                Left transactionError -> pure (Left transactionError)
                Right TransactionResult {..} -> case transactionCreatedPaths of
                  [newPath] -> pure (Right ScopeChangeResult
                    { scopeChangeOperationId = transactionOperationId
                    , scopeChangeAdrId = adrId
                    , scopeChangeConnectionId = connId
                    , scopeChangeParents = parents
                    , scopeChangeMode = changeKind
                    , scopeChangeEffective = effective
                    , scopeChangeCommitOid = transactionCommitOid
                    , scopeChangeNewPath = newPath
                    , scopeChangeCreatedPaths = transactionCreatedPaths
                    , scopeChangeIndexUpdated = transactionIndexUpdated
                    })
                  _ -> pure (Left (Stage8UpdateRef "scope transaction did not report exactly one created path"))
        _ -> pure (Left (Stage3ValidateState "failed to generate sortable identifiers"))

selectScopeUpdate :: AdrId -> Maybe StateToken -> ScopeChangeRequest -> [ParsedManagedDocument] -> Either TransactionError ([ConnectionId], T.Text, [ScopePattern], [ScopePattern], [ScopePattern])
selectScopeUpdate adr expected request documents = do
  let reduction = reduceManagedGraph (map parsedManagedRecord documents)
  reduced <- maybe (Left (Stage3ValidateState "scope target ADR is unknown")) Right (lookupReducedAdr adr reduction)
  case expected of
    Nothing -> Right ()
    Just expectedToken
      | expectedToken == reducedStateToken reduced -> Right ()
      | otherwise -> Left (Stage3ValidateState ("stale ADR state: expected " <> stateTokenText expectedToken <> ", current state is " <> stateTokenText (reducedStateToken reduced)))
  status <- maybe (Left (Stage3ValidateState "scope target ADR has no current status")) Right (axisResolutionEffective (reducedStatusAxis reduced))
  if reducedStatusState status == StatusActive
    then Right ()
    else Left (Stage3ValidateState "scope target ADR is not active")
  let scopeHeads = axisResolutionHeads (reducedScopeAxis reduced)
      scopeConflictOnly = reducedConflictAxes reduced == [ScopeAxis]
      noConflicts = null (reducedConflictAxes reduced)
  case request of
    ScopeDelta added removed -> do
      if noConflicts
        then Right ()
        else Left (Stage3ValidateState "scope target ADR is conflicted")
      case scopeHeads of
        [priorHead] -> do
          (changeKind, effective) <- canonicalScopeDelta (axisResolutionEffective (reducedScopeAxis reduced)) added removed
          Right ([priorHead], changeKind, sort added, sort removed, effective)
        _ -> Left (Stage3ValidateState "scope target ADR has no unambiguous current scope")
    ScopeReviewedSet reviewed -> do
      if noConflicts || scopeConflictOnly
        then Right ()
        else Left (Stage3ValidateState "scope target ADR is conflicted")
      requested <- canonicalReviewedScopeSet reviewed
      case scopeHeads of
        [priorHead] -> do
          let current = axisResolutionEffective (reducedScopeAxis reduced)
          (added, removed) <- exactScopeDifference True current requested
          Right ([priorHead], "replace", added, removed, requested)
        parents@(_ : _ : _) -> do
          current <- effectiveScopeUnion parents (reducedScopeHistory reduced)
          -- A reviewed merge is itself a material operation: even when the
          -- requested set equals the parent union, it replaces several
          -- current heads with one canonical merge head and resolves the
          -- scope conflict.  Only this multi-parent path permits an empty
          -- semantic delta.
          (added, removed) <- exactScopeDifference False current requested
          Right (sort parents, "merge", added, removed, requested)
        _ -> Left (Stage3ValidateState "scope target ADR has no current scope")

canonicalReviewedScopeSet :: [ScopePattern] -> Either TransactionError [ScopePattern]
canonicalReviewedScopeSet reviewed
  | Set.null reviewedSet = Left (Stage3ValidateState "reviewed scope set must not be empty")
  | Set.size reviewedSet /= length reviewed = Left (Stage3ValidateState "reviewed scope set contains duplicates")
  | otherwise = Right (Set.toAscList reviewedSet)
  where
    reviewedSet = Set.fromList reviewed

effectiveScopeUnion :: [ConnectionId] -> [ConnectionRecord] -> Either TransactionError [ScopePattern]
effectiveScopeUnion parents history =
  fmap Set.toAscList (traverse lookupEffective (sort parents) >>= pure . Set.unions)
  where
    lookupEffective parent =
      case [appliesToEffective payload | connection <- history, connectionRecordId connection == parent, AppliesToConnection payload <- [connectionPayload connection]] of
        [effective] -> Right (Set.fromList effective)
        _ -> Left (Stage3ValidateState "scope target ADR has an invalid current scope head")

exactScopeDifference :: Bool -> [ScopePattern] -> [ScopePattern] -> Either TransactionError ([ScopePattern], [ScopePattern])
exactScopeDifference rejectNoOp current requested
  | rejectNoOp && currentSet == requestedSet = Left (Stage3ValidateState "reviewed scope set would not change the current scope")
  | otherwise = Right (Set.toAscList (requestedSet `Set.difference` currentSet), Set.toAscList (currentSet `Set.difference` requestedSet))
  where
    currentSet = Set.fromList current
    requestedSet = Set.fromList requested

normalizeScopeReason :: T.Text -> Either TransactionError T.Text
normalizeScopeReason raw
  | T.null normalized = Left (Stage3ValidateState "scope reason must be nonblank")
  | otherwise = Right (normalized <> "\n")
  where
    normalized = T.strip (normalizeLineEndings raw)

canonicalScopeDelta :: [ScopePattern] -> [ScopePattern] -> [ScopePattern] -> Either TransactionError (T.Text, [ScopePattern])
canonicalScopeDelta current added removed
  | Set.size addedSet /= length added = Left (Stage3ValidateState "scope additions contain duplicates")
  | Set.size removedSet /= length removed = Left (Stage3ValidateState "scope removals contain duplicates")
  | not (Set.null (Set.intersection addedSet removedSet)) = Left (Stage3ValidateState "scope additions and removals overlap")
  | Set.null addedSet && Set.null removedSet = Left (Stage3ValidateState "scope change would be empty")
  | not (Set.null (Set.intersection addedSet currentSet)) = Left (Stage3ValidateState "scope additions already exist in the current scope")
  | not (removedSet `Set.isSubsetOf` currentSet) = Left (Stage3ValidateState "scope removals are absent from the current scope")
  | Set.null effectiveSet = Left (Stage3ValidateState "scope change would leave no effective scope")
  | otherwise = Right (changeKind, Set.toAscList effectiveSet)
  where
    currentSet = Set.fromList current
    addedSet = Set.fromList added
    removedSet = Set.fromList removed
    effectiveSet = (currentSet `Set.difference` removedSet) `Set.union` addedSet
    changeKind
      | Set.null removedSet = "expand"
      | Set.null addedSet = "contract"
      | otherwise = "mixed"

sealScopeUpdate :: OperationId -> ResolvedRepositoryRevision -> T.Text -> Actor -> Integer -> [ConnectionId] -> ProvenanceInputs -> ManagedPaths -> ConnectionRecord -> Either TransactionError GeneratedFile
sealScopeUpdate opId revision branchName actor timestampMs parents inputs paths connection = do
  semantic <- first (Stage5ValidateGenerated . ("scope render: " <>) . T.pack . show) (renderConnectionSemantic connection)
  changeKind <- case connectionPayload connection of
    AppliesToConnection payload -> Right (appliesToChange payload)
    _ -> Left (Stage5ValidateGenerated "scope eventKind: expected applies_to connection")
  eventKind <- first (Stage5ValidateGenerated . ("scope eventKind: " <>) . T.pack . show) (mkEventKind ("scope." <> changeKind))
  capsule <- first (Stage5ValidateGenerated . ("scope capsule: " <>) . T.pack . show) $
    mkProvenanceCapsule ProvenanceCapsuleInput
      { capsuleInputOperationId = opId, capsuleInputObjectId = ProvenanceConnection (connectionRecordId connection)
      , capsuleInputEventKind = eventKind, capsuleInputActor = actor, capsuleInputTimestampMs = timestampMs
      , capsuleInputBasis = resolvedCommitOid revision, capsuleInputParents = map ProvenanceConnection parents
      , capsuleInputBranchHint = Just branchName, capsuleInputUpstreamHint = Nothing, capsuleInputLineAnchors = []
      , capsuleInputSemanticDigest = semanticDigest semantic, capsuleInputToolVersion = "adrai/1.0.0", capsuleInputDigests = inputs
      }
  sealed <- first (Stage5ValidateGenerated . ("scope seal: " <>) . T.pack . show) (sealManagedDocument (ManagedConnection connection) capsule)
  path <- first (Stage4GenerateFiles . ("scope path: " <>) . T.pack . show) (canonicalManagedPath paths (ManagedConnection connection))
  pure (GeneratedFile path sealed)

-- ---------------------------------------------------------------------------
-- Change Domain
-- ---------------------------------------------------------------------------

-- | Result of the 'changeDomainCommand' operation.
data DomainChangeResult
  = DomainChangeResult
      { domainChangeOperationId :: String,
        domainChangeAdrId      :: AdrId,
        domainChangeConnectionId :: ConnectionId,
        domainChangeParents :: [ConnectionId],
        domainChangeMode :: T.Text,
        domainChangeAdded :: [Domain],
        domainChangeRemoved :: [Domain],
        domainChangeEffective :: [Domain],
        domainChangeRefinements :: [DomainRefinement],
        domainChangeCommitOid  :: GitOid,
        domainChangeNewPath    :: RepoPath,
        domainChangeCreatedPaths :: [RepoPath],
        domainChangeIndexUpdated :: Bool
      }
  deriving (Eq, Show)

-- | Domain updates have deliberately non-overlapping request forms.  In
-- particular, callers cannot accidentally attach refinement mappings to a
-- regular delta or a reviewed reconciliation.
data DomainChangeRequest
  = DomainDelta [Domain] [Domain]
  | DomainRefine [DomainRefinement]
  | DomainReviewedSet [Domain]
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
  Maybe StateToken ->
  T.Text ->
  DomainChangeRequest ->
  ProvenanceInputs ->
  IO (Either TransactionError DomainChangeResult)
changeDomainCommand
  repository
  managedPaths
  actor
  adrId
  expectedState reason request inputs =
    case normalizeDomainReason reason of
      Left err -> pure (Left err)
      Right rationale -> requireAttachedHead repository >>= \case
        Left err -> pure (Left err)
        Right branchName -> repositorySnapshot repository (RevisionSpec "HEAD") >>= \case
          Left err -> pure (Left (Stage3ValidateState ("read committed HEAD: " <> T.pack (show err))))
          Right snapshot -> case committedDocuments (repositorySnapshotManagedPaths snapshot) snapshot >>= selectDomainUpdate adrId expectedState request of
            Left err -> pure (Left err)
            Right (parents, mode, added, removed, effective, refinements) -> do
              timestampMs <- currentTimestamp
              entropy <- randomEntropy
              case (sortableOperationId (encodeTimestampMs timestampMs) entropy, sortableConnectionId (encodeTimestampMs timestampMs) entropy) of
                (Right opId, Right connId) -> do
                  let record = ConnectionRecord connId (DomainsConnection DomainsPayload
                        { domainsSubjectAdr = adrId, domainsParentConnections = parents, domainsChange = mode
                        , domainsAdded = added, domainsRemoved = removed, domainsEffective = effective, domainsRefinements = refinements }) rationale
                      paths = repositorySnapshotManagedPaths snapshot
                  case sealDomainUpdate opId (repositorySnapshotRevision snapshot) branchName actor timestampMs parents inputs paths record of
                    Left err -> pure (Left err)
                    Right generated -> commitAppendOnlyOperation repository TransactionConfig
                      { configOperationId = T.unpack (operationIdText opId), configSubject = "adrai: domain " <> adrIdText adrId
                      , configTrailers = Map.fromList [("ADR", T.unpack (adrIdText adrId)), ("Objects", T.unpack (connectionIdText connId))]
                      , configExpectedHead = resolvedCommitOid (repositorySnapshotRevision snapshot), configGenerated = [generated] } >>= \case
                        Left err -> pure (Left err)
                        Right TransactionResult {..} -> case transactionCreatedPaths of
                          [newPath] -> pure (Right DomainChangeResult
                            { domainChangeOperationId = transactionOperationId, domainChangeAdrId = adrId, domainChangeConnectionId = connId
                            , domainChangeParents = parents, domainChangeMode = mode, domainChangeAdded = added, domainChangeRemoved = removed
                            , domainChangeEffective = effective, domainChangeRefinements = refinements
                            , domainChangeCommitOid = transactionCommitOid, domainChangeNewPath = newPath, domainChangeCreatedPaths = transactionCreatedPaths
                            , domainChangeIndexUpdated = transactionIndexUpdated })
                          _ -> pure (Left (Stage8UpdateRef "domain transaction did not report exactly one created path"))
                _ -> pure (Left (Stage3ValidateState "failed to generate sortable identifiers"))

normalizeDomainReason :: T.Text -> Either TransactionError T.Text
normalizeDomainReason raw
  | T.null normalized = Left (Stage3ValidateState "domain reason must be nonblank")
  | otherwise = Right (normalized <> "\n")
  where normalized = T.strip (normalizeLineEndings raw)

selectDomainUpdate :: AdrId -> Maybe StateToken -> DomainChangeRequest -> [ParsedManagedDocument] -> Either TransactionError ([ConnectionId], T.Text, [Domain], [Domain], [Domain], [DomainRefinement])
selectDomainUpdate adr expected request documents = do
  let reduction = reduceManagedGraph (map parsedManagedRecord documents)
  reduced <- maybe (Left (Stage3ValidateState "domain target ADR is unknown")) Right (lookupReducedAdr adr reduction)
  case expected of
    Nothing -> Right ()
    Just token | token == reducedStateToken reduced -> Right ()
               | otherwise -> Left (Stage3ValidateState ("stale ADR state: expected " <> stateTokenText token <> ", current state is " <> stateTokenText (reducedStateToken reduced)))
  status <- maybe (Left (Stage3ValidateState "domain target ADR has no current status")) Right (axisResolutionEffective (reducedStatusAxis reduced))
  if reducedStatusState status /= StatusActive then Left (Stage3ValidateState "domain target ADR is not active") else Right ()
  let heads = axisResolutionHeads (reducedDomainAxis reduced)
      noConflicts = null (reducedConflictAxes reduced)
      domainConflictOnly = reducedConflictAxes reduced == [DomainAxis]
      oneHead = case heads of [parent] -> Right parent; _ -> Left (Stage3ValidateState "domain target ADR has no unambiguous current domain")
      current = axisResolutionEffective (reducedDomainAxis reduced)
  if null heads
    then Left (Stage3ValidateState "domain target ADR has no current domain")
    else Right ()
  case request of
    DomainDelta added removed -> do
      if noConflicts then Right () else Left (Stage3ValidateState "domain target ADR is conflicted")
      parent <- oneHead
      (mode, effective) <- canonicalDomainDelta current added removed
      Right ([parent], mode, sort added, sort removed, effective, [])
    DomainRefine refinements -> do
      if noConflicts then Right () else Left (Stage3ValidateState "domain target ADR is conflicted")
      parent <- oneHead
      (added, removed, effective) <- canonicalDomainRefinement current refinements
      Right ([parent], "refine", added, removed, effective, sort refinements)
    DomainReviewedSet reviewed -> do
      if noConflicts || domainConflictOnly then Right () else Left (Stage3ValidateState "domain target ADR is conflicted")
      requested <- canonicalReviewedDomains reviewed
      case heads of
        [parent] -> do
          (added, removed) <- exactDomainDifference True current requested
          Right ([parent], "replace", added, removed, requested, [])
        parents@(_ : _ : _) -> do
          union <- effectiveDomainUnion parents (reducedDomainHistory reduced)
          (added, removed) <- exactDomainDifference False union requested
          Right (sort parents, "merge", added, removed, requested, [])
        _ -> Left (Stage3ValidateState "domain target ADR has no current domain")

canonicalReviewedDomains :: [Domain] -> Either TransactionError [Domain]
canonicalReviewedDomains values
  | Set.size setValues /= length values = Left (Stage3ValidateState "reviewed domain set contains duplicates")
  | otherwise = validateDomainAntichain (Set.toAscList setValues)
  where setValues = Set.fromList values

validateDomainAntichain :: [Domain] -> Either TransactionError [Domain]
validateDomainAntichain values = first (Stage3ValidateState . ("domain set: " <>) . T.pack . show) (canonicalDomains (map domainText values))

canonicalDomainDelta :: [Domain] -> [Domain] -> [Domain] -> Either TransactionError (T.Text, [Domain])
canonicalDomainDelta current added removed
  | Set.size addedSet /= length added = Left (Stage3ValidateState "domain additions contain duplicates")
  | Set.size removedSet /= length removed = Left (Stage3ValidateState "domain removals contain duplicates")
  | not (Set.disjoint addedSet removedSet) = Left (Stage3ValidateState "domain additions and removals overlap")
  | Set.null addedSet && Set.null removedSet = Left (Stage3ValidateState "domain change would be empty")
  | not (Set.disjoint addedSet currentSet) = Left (Stage3ValidateState "domain additions already exist in the current domain")
  | not (removedSet `Set.isSubsetOf` currentSet) = Left (Stage3ValidateState "domain removals are absent from the current domain")
  | otherwise = do
      effective <- validateDomainAntichain (Set.toAscList ((currentSet `Set.difference` removedSet) `Set.union` addedSet))
      Right (if Set.null removedSet then "expand" else if Set.null addedSet then "contract" else "mixed", effective)
  where currentSet = Set.fromList current; addedSet = Set.fromList added; removedSet = Set.fromList removed

canonicalDomainRefinement :: [Domain] -> [DomainRefinement] -> Either TransactionError ([Domain], [Domain], [Domain])
canonicalDomainRefinement current refinements
  | null refinements = Left (Stage3ValidateState "domain refinement mappings must not be empty")
  | Set.size (Set.fromList refinements) /= length refinements = Left (Stage3ValidateState "domain refinement mappings contain duplicates")
  | Set.size parents /= length refinements = Left (Stage3ValidateState "domain refinement sources contain duplicates")
  | not (parents `Set.isSubsetOf` currentSet) = Left (Stage3ValidateState "domain refinement source is not active")
  | any (\r -> domainRefinementChild r == domainRefinementParent r || not (domainRefinementChild r `domainIsWithin` domainRefinementParent r)) refinements = Left (Stage3ValidateState "domain refinement child must be a strict descendant")
  | otherwise = do
      effective <- validateDomainAntichain (Set.toAscList ((currentSet `Set.difference` parents) `Set.union` children))
      Right (Set.toAscList children, Set.toAscList parents, effective)
  where
    currentSet = Set.fromList current; parents = Set.fromList (map domainRefinementParent refinements); children = Set.fromList (map domainRefinementChild refinements)

effectiveDomainUnion :: [ConnectionId] -> [ConnectionRecord] -> Either TransactionError [Domain]
effectiveDomainUnion parents history = do
  effective <- traverse lookupOne (sort parents)
  -- A domain-axis conflict may legitimately contain an ancestor in one head
  -- and its descendant in another.  Each committed head must itself be a
  -- valid antichain; the temporary union is only the comparison baseline for
  -- a reviewed merge and must not be rejected before the caller supplies the
  -- final reviewed antichain.
  traverse validateDomainAntichain effective
  pure (Set.toAscList (Set.unions (map Set.fromList effective)))
  where
    lookupOne parent = case [domainsEffective payload | c <- history, connectionRecordId c == parent, DomainsConnection payload <- [connectionPayload c]] of
      [values] -> Right values
      _ -> Left (Stage3ValidateState "domain target ADR has an invalid current domain head")

exactDomainDifference :: Bool -> [Domain] -> [Domain] -> Either TransactionError ([Domain], [Domain])
exactDomainDifference rejectNoOp current requested
  | rejectNoOp && currentSet == requestedSet = Left (Stage3ValidateState "reviewed domain set would not change the current domain")
  | otherwise = Right (Set.toAscList (requestedSet `Set.difference` currentSet), Set.toAscList (currentSet `Set.difference` requestedSet))
  where currentSet = Set.fromList current; requestedSet = Set.fromList requested

sealDomainUpdate :: OperationId -> ResolvedRepositoryRevision -> T.Text -> Actor -> Integer -> [ConnectionId] -> ProvenanceInputs -> ManagedPaths -> ConnectionRecord -> Either TransactionError GeneratedFile
sealDomainUpdate opId revision branchName actor timestampMs parents inputs paths connection = do
  semantic <- first (Stage5ValidateGenerated . ("domain render: " <>) . T.pack . show) (renderConnectionSemantic connection)
  eventKind <- first (Stage5ValidateGenerated . ("domain eventKind: " <>) . T.pack . show) (mkEventKind ("domain." <> domainsChange payload))
  capsule <- first (Stage5ValidateGenerated . ("domain capsule: " <>) . T.pack . show) (mkProvenanceCapsule ProvenanceCapsuleInput
    { capsuleInputOperationId = opId, capsuleInputObjectId = ProvenanceConnection (connectionRecordId connection), capsuleInputEventKind = eventKind
    , capsuleInputActor = actor, capsuleInputTimestampMs = timestampMs, capsuleInputBasis = resolvedCommitOid revision
    , capsuleInputParents = map ProvenanceConnection parents, capsuleInputBranchHint = Just branchName, capsuleInputUpstreamHint = Nothing, capsuleInputLineAnchors = []
    , capsuleInputSemanticDigest = semanticDigest semantic, capsuleInputToolVersion = "adrai/1.0.0", capsuleInputDigests = inputs })
  sealed <- first (Stage5ValidateGenerated . ("domain seal: " <>) . T.pack . show) (sealManagedDocument (ManagedConnection connection) capsule)
  path <- first (Stage4GenerateFiles . ("domain path: " <>) . T.pack . show) (canonicalManagedPath paths (ManagedConnection connection))
  pure (GeneratedFile path sealed)
  where payload = case connectionPayload connection of DomainsConnection value -> value; _ -> error "domain connection required"

-- ---------------------------------------------------------------------------
-- Obsolete ADR
-- ---------------------------------------------------------------------------

-- | Result of the 'obsoleteCommand' operation.
data ObsoleteRequest = ObsoleteRequest
  { obsoleteExpectedState :: Maybe StateToken,
    obsoleteReason :: T.Text,
    obsoleteResolve :: Bool,
    obsoleteReplacement :: Maybe AdrId
  }
  deriving (Eq, Show)

data ObsoleteResult
  = ObsoleteResult
      { obsoleteOperationId :: String,
        obsoleteAdrId       :: AdrId,
        obsoleteConnectionId :: ConnectionId,
        obsoleteStatusParents :: [ConnectionId],
        obsoleteRecordHeads :: [RecordId],
        obsoleteReplacementAdr :: Maybe AdrId,
        obsoleteResolvedConflict :: Bool,
        obsoleteCommitOid   :: GitOid,
        obsoleteNewPath     :: RepoPath,
        obsoleteCreatedPaths :: [RepoPath],
        obsoleteIndexUpdated :: Bool
      }
  deriving (Eq, Show)

-- | Mark an ADR as obsolete.
--
-- Updates the status connection to @StatusObsolete@ and commits via the
-- append-only transaction engine.
--
-- The new capsule carries eventKind @"decision.obsolete"@.
obsoleteCommand ::
  ObsoleteIntent intent =>
  Repository ->
  ManagedPaths ->
  Actor ->
  AdrId ->
  intent ->
  ProvenanceInputs ->
  IO (Either TransactionError ObsoleteResult)
obsoleteCommand
  repository
  managedPaths
  actor
  adrId
  intent
  inputs =
    case toObsoleteRequest intent >>= normalizeObsoleteRequest of
      Left err -> pure (Left err)
      Right request -> statusTransition repository managedPaths actor adrId request StatusObsolete inputs

-- ---------------------------------------------------------------------------
-- Reactivate ADR
-- ---------------------------------------------------------------------------

-- | Result of the 'reactivateCommand' operation.
data ReactivateRequest = ReactivateRequest
  { reactivateExpectedState :: Maybe StateToken,
    reactivateReason :: T.Text,
    reactivateResolve :: Bool
  }
  deriving (Eq, Show)

data ReactivateResult
  = ReactivateResult
      { reactivateOperationId :: String,
        reactivateAdrId       :: AdrId,
        reactivateConnectionId :: ConnectionId,
        reactivateStatusParents :: [ConnectionId],
        reactivateRecordHeads :: [RecordId],
        reactivateResolvedConflict :: Bool,
        reactivateCommitOid   :: GitOid,
        reactivateNewPath     :: RepoPath,
        reactivateCreatedPaths :: [RepoPath],
        reactivateIndexUpdated :: Bool
      }
  deriving (Eq, Show)

-- | Reactivate an obsolete ADR back to active status.
--
-- Updates the status connection to @StatusActive@ and commits via the
-- append-only transaction engine.
--
-- The new capsule carries eventKind @"decision.reactivate"@.
reactivateCommand ::
  ReactivateIntent intent =>
  Repository ->
  ManagedPaths ->
  Actor ->
  AdrId ->
  intent ->
  ProvenanceInputs ->
  IO (Either TransactionError ReactivateResult)
reactivateCommand
  repository
  managedPaths
  actor
  adrId
  intent
  inputs =
    case toReactivateRequest intent >>= normalizeReactivateRequest of
      Left err -> pure (Left err)
      Right request -> fmap (fmap toReactivateResult) (statusTransition repository managedPaths actor adrId (asObsoleteRequest request) StatusActive inputs)

-- | The typed request is the public service input.  The 'RecordId' instances
-- keep the terminal explorer compiling until its deliberately separate status
-- UI migration; the value is never used as status or decision authority.
class ObsoleteIntent intent where
  toObsoleteRequest :: intent -> Either TransactionError ObsoleteRequest

instance ObsoleteIntent ObsoleteRequest where
  toObsoleteRequest = Right

instance ObsoleteIntent RecordId where
  toObsoleteRequest _ = Right (ObsoleteRequest Nothing "Explorer status update" False Nothing)

class ReactivateIntent intent where
  toReactivateRequest :: intent -> Either TransactionError ReactivateRequest

instance ReactivateIntent ReactivateRequest where
  toReactivateRequest = Right

instance ReactivateIntent RecordId where
  toReactivateRequest _ = Right (ReactivateRequest Nothing "Explorer status update" False)

normalizeObsoleteRequest :: ObsoleteRequest -> Either TransactionError ObsoleteRequest
normalizeObsoleteRequest request = do
  rationale <- normalizeStatusReason "obsolete" (obsoleteReason request)
  pure request { obsoleteReason = rationale }

normalizeReactivateRequest :: ReactivateRequest -> Either TransactionError ReactivateRequest
normalizeReactivateRequest request = do
  rationale <- normalizeStatusReason "reactivate" (reactivateReason request)
  pure request { reactivateReason = rationale }

asObsoleteRequest :: ReactivateRequest -> ObsoleteRequest
asObsoleteRequest request =
  ObsoleteRequest
    { obsoleteExpectedState = reactivateExpectedState request,
      obsoleteReason = reactivateReason request,
      obsoleteResolve = reactivateResolve request,
      obsoleteReplacement = Nothing
    }

toReactivateResult :: ObsoleteResult -> ReactivateResult
toReactivateResult result =
  ReactivateResult
    { reactivateOperationId = obsoleteOperationId result
    , reactivateAdrId = obsoleteAdrId result
    , reactivateConnectionId = obsoleteConnectionId result
    , reactivateStatusParents = obsoleteStatusParents result
    , reactivateRecordHeads = obsoleteRecordHeads result
    , reactivateResolvedConflict = obsoleteResolvedConflict result
    , reactivateCommitOid = obsoleteCommitOid result
    , reactivateNewPath = obsoleteNewPath result
    , reactivateCreatedPaths = obsoleteCreatedPaths result
    , reactivateIndexUpdated = obsoleteIndexUpdated result
    }

normalizeStatusReason :: T.Text -> T.Text -> Either TransactionError T.Text
normalizeStatusReason operation raw
  | T.null normalized = Left (Stage3ValidateState (operation <> " reason must be nonblank"))
  | otherwise = Right (normalized <> "\n")
  where
    normalized = T.strip (normalizeLineEndings raw)

statusTransition :: Repository -> ManagedPaths -> Actor -> AdrId -> ObsoleteRequest -> StatusState -> ProvenanceInputs -> IO (Either TransactionError ObsoleteResult)
statusTransition repository managedPaths actor adrId request desiredState inputs =
  requireAttachedHead repository >>= \case
    Left err -> pure (Left err)
    Right branchName ->
      repositorySnapshot repository (RevisionSpec "HEAD") >>= \case
        Left err -> pure (Left (Stage3ValidateState ("read committed HEAD: " <> T.pack (show err))))
        Right snapshot ->
          case committedDocuments (repositorySnapshotManagedPaths snapshot) snapshot >>= selectStatusTransition adrId request desiredState of
            Left err -> pure (Left err)
            Right (parents, recordHeads, resolvedConflict) -> do
              timestampMs <- currentTimestamp
              entropy <- randomEntropy
              case (sortableOperationId (encodeTimestampMs timestampMs) entropy, sortableConnectionId (encodeTimestampMs timestampMs) entropy) of
                (Right opId, Right connId) -> do
                  let record = ConnectionRecord connId (StatusConnection StatusPayload
                        { statusSubjectAdr = adrId
                        , statusParentConnections = parents
                        , statusState = desiredState
                        , statusRecordHeads = recordHeads
                        , statusReplacementAdr = obsoleteReplacement request
                        }) (obsoleteReason request)
                      paths = repositorySnapshotManagedPaths snapshot
                  case sealStatusTransition opId (repositorySnapshotRevision snapshot) branchName actor timestampMs parents recordHeads inputs paths record of
                    Left err -> pure (Left err)
                    Right generated ->
                      commitAppendOnlyOperation repository TransactionConfig
                        { configOperationId = T.unpack (operationIdText opId)
                        , configSubject = "adrai: " <> statusOperationLabel desiredState <> " " <> adrIdText adrId
                        , configTrailers = Map.fromList [("ADR", T.unpack (adrIdText adrId)), ("Objects", T.unpack (connectionIdText connId))]
                        , configExpectedHead = resolvedCommitOid (repositorySnapshotRevision snapshot)
                        , configGenerated = [generated]
                        } >>= \case
                          Left err -> pure (Left err)
                          Right TransactionResult {..} -> case transactionCreatedPaths of
                            [newPath] -> pure (Right ObsoleteResult
                              { obsoleteOperationId = transactionOperationId
                              , obsoleteAdrId = adrId
                              , obsoleteConnectionId = connId
                              , obsoleteStatusParents = parents
                              , obsoleteRecordHeads = recordHeads
                              , obsoleteReplacementAdr = obsoleteReplacement request
                              , obsoleteResolvedConflict = resolvedConflict
                              , obsoleteCommitOid = transactionCommitOid
                              , obsoleteNewPath = newPath
                              , obsoleteCreatedPaths = transactionCreatedPaths
                              , obsoleteIndexUpdated = transactionIndexUpdated
                              })
                            _ -> pure (Left (Stage8UpdateRef "status transaction did not report exactly one created path"))
                _ -> pure (Left (Stage3ValidateState "failed to generate sortable identifiers"))

selectStatusTransition :: AdrId -> ObsoleteRequest -> StatusState -> [ParsedManagedDocument] -> Either TransactionError ([ConnectionId], [RecordId], Bool)
selectStatusTransition adr request desiredState documents = do
  let reduction = reduceManagedGraph (map parsedManagedRecord documents)
  reduced <- maybe (Left (Stage3ValidateState "status target ADR is unknown")) Right (lookupReducedAdr adr reduction)
  case obsoleteExpectedState request of
    Nothing -> Right ()
    Just token
      | token == reducedStateToken reduced -> Right ()
      | otherwise -> Left (Stage3ValidateState ("stale ADR state: expected " <> stateTokenText token <> ", current state is " <> stateTokenText (reducedStateToken reduced)))
  let parents = sort (axisResolutionHeads (reducedStatusAxis reduced))
      recordHeads = sort (axisResolutionHeads (reducedDecisionAxis reduced))
      conflicts = reducedConflictAxes reduced
      statusOnlyConflict = conflicts == [StatusAxis]
      noConflicts = null conflicts
  if null parents then Left (Stage3ValidateState "status target ADR has no current status") else Right ()
  if null recordHeads then Left (Stage3ValidateState "status target ADR has no current decision") else Right ()
  if obsoleteResolve request
    then if statusOnlyConflict && length parents > 1
      then Right ()
      else Left (Stage3ValidateState "status resolve requires a conflicted status axis")
    else if noConflicts
      then Right ()
      else Left (Stage3ValidateState "status target ADR is conflicted")
  if obsoleteResolve request
    then Right ()
    else do
      current <- maybe (Left (Stage3ValidateState "status target ADR has no effective current status")) Right (axisResolutionEffective (reducedStatusAxis reduced))
      case desiredState of
        StatusObsolete | reducedStatusState current == StatusActive -> Right ()
        StatusActive | reducedStatusState current == StatusObsolete -> Right ()
        StatusObsolete -> Left (Stage3ValidateState "obsolete target ADR is already obsolete")
        StatusActive -> Left (Stage3ValidateState "reactivate target ADR is already active")
  case desiredState of
    StatusObsolete -> validateReplacement reduction adr (obsoleteReplacement request)
    StatusActive | obsoleteReplacement request /= Nothing -> Left (Stage3ValidateState "reactivate may not name a replacement ADR")
    StatusActive -> Right ()
  pure (parents, recordHeads, obsoleteResolve request)

validateReplacement :: GraphReduction -> AdrId -> Maybe AdrId -> Either TransactionError ()
validateReplacement _ _ Nothing = Right ()
validateReplacement reduction target (Just replacement)
  | replacement == target = Left (Stage3ValidateState "obsolete replacement ADR must not name the target ADR")
  | otherwise = do
      reduced <- maybe (Left (Stage3ValidateState "obsolete replacement ADR is unknown")) Right (lookupReducedAdr replacement reduction)
      if null (reducedConflictAxes reduced) then Right () else Left (Stage3ValidateState "obsolete replacement ADR is conflicted")
      case (axisResolutionHeads (reducedStatusAxis reduced), axisResolutionEffective (reducedStatusAxis reduced)) of
        ([_], Just status) | reducedStatusState status == StatusActive -> Right ()
        _ -> Left (Stage3ValidateState "obsolete replacement ADR is not unambiguously active")

statusOperationLabel :: StatusState -> T.Text
statusOperationLabel StatusObsolete = "obsolete"
statusOperationLabel StatusActive = "reactivate"

sealStatusTransition :: OperationId -> ResolvedRepositoryRevision -> T.Text -> Actor -> Integer -> [ConnectionId] -> [RecordId] -> ProvenanceInputs -> ManagedPaths -> ConnectionRecord -> Either TransactionError GeneratedFile
sealStatusTransition opId revision branchName actor timestampMs statusParents recordHeads inputs paths connection = do
  semantic <- first (Stage5ValidateGenerated . ("status render: " <>) . T.pack . show) (renderConnectionSemantic connection)
  eventKind <- first (Stage5ValidateGenerated . ("status eventKind: " <>) . T.pack . show) (mkEventKind ("decision." <> statusOperationLabel (statusState payload)))
  capsule <- first (Stage5ValidateGenerated . ("status capsule: " <>) . T.pack . show) (mkProvenanceCapsule ProvenanceCapsuleInput
    { capsuleInputOperationId = opId
    , capsuleInputObjectId = ProvenanceConnection (connectionRecordId connection)
    , capsuleInputEventKind = eventKind
    , capsuleInputActor = actor
    , capsuleInputTimestampMs = timestampMs
    , capsuleInputBasis = resolvedCommitOid revision
    , capsuleInputParents = map ProvenanceConnection statusParents <> map ProvenanceRecord recordHeads
    , capsuleInputBranchHint = Just branchName
    , capsuleInputUpstreamHint = Nothing
    , capsuleInputLineAnchors = []
    , capsuleInputSemanticDigest = semanticDigest semantic
    , capsuleInputToolVersion = "adrai/1.0.0"
    , capsuleInputDigests = inputs
    })
  sealed <- first (Stage5ValidateGenerated . ("status seal: " <>) . T.pack . show) (sealManagedDocument (ManagedConnection connection) capsule)
  path <- first (Stage4GenerateFiles . ("status path: " <>) . T.pack . show) (canonicalManagedPath paths (ManagedConnection connection))
  pure (GeneratedFile path sealed)
  where
    payload = case connectionPayload connection of StatusConnection value -> value; _ -> error "status connection required"
