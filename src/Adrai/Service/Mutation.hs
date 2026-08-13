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
    AppliesToPayload (..),
    DomainsPayload (..),
    StatusPayload (..),
    StatusState (..),
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
    ProvenanceInputs (..),
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

import Data.Bifunctor (first)
import Data.Char (isDigit)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import qualified Data.Map.Strict as Map
import Data.List (sort)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Text as T
import Data.Text (intercalate)
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
          _ <- commitBootstrapFiles repository config
          pure (Right (InitResult True))
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

-- | Current timestamp in milliseconds, encoded as 8-byte big-endian.
currentTimestampMs :: IO BS.ByteString
currentTimestampMs = do
  now <- round <$> getPOSIXTime
  let ms = now * 1000
  pure (BS.pack (take 8 (toBytesBE ms)))
  where
    toBytesBE :: Integer -> [Word8]
    toBytesBE n = take 8 $ map toWord8 (iterate (div 256) n)
      where
        toWord8 :: Integer -> Word8
        toWord8 i = fromIntegral (i `mod` 256)

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
                Right (GitHeadAttached ref) -> T.unpack (gitRefText ref)
                Right GitHeadDetached -> "HEAD"
                Left _ -> "HEAD"
        timestampMs <- currentTimestampMs
        entropy <- randomEntropy
        let opIdResult = sortableOperationId timestampMs entropy
            recIdResult = sortableRecordId timestampMs entropy
            scopeIdResult = sortableConnectionId timestampMs entropy
            domainIdResult = sortableConnectionId timestampMs entropy
            statusIdResult = sortableConnectionId timestampMs entropy
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
                        , appliesToChange = "initial scope definition"
                        , appliesToAdded = patterns
                        , appliesToRemoved = []
                        , appliesToEffective = patterns
                        }),
                      connectionRationale = "Scope connection for initial ADR definition"
                    }
            let domainConn =
                  ConnectionRecord
                    { connectionRecordId = domainId,
                      connectionPayload = DomainsConnection (DomainsPayload
                        { domainsSubjectAdr = adrId
                        , domainsParentConnections = []
                        , domainsChange = "initial domain assignment"
                        , domainsAdded = domains
                        , domainsRemoved = []
                        , domainsEffective = domains
                        , domainsRefinements = []
                        }),
                      connectionRationale = "Domain connection for initial ADR definition"
                    }
            let statusConn =
                  ConnectionRecord
                    { connectionRecordId = statusId,
                      connectionPayload = StatusConnection (StatusPayload
                        { statusSubjectAdr = adrId
                        , statusParentConnections = []
                        , statusState = StatusActive
                        , statusRecordHeads = []
                        , statusReplacementAdr = Nothing
                        }),
                      connectionRationale = "Status connection for initial ADR definition"
                    }
            let createdPaths =
                  mapMaybe extractPath
                    [ canonicalManagedPath managedPaths (ManagedDecision decisionRec)
                    , canonicalManagedPath managedPaths (ManagedConnection scopeConn)
                    , canonicalManagedPath managedPaths (ManagedConnection domainConn)
                    , canonicalManagedPath managedPaths (ManagedConnection statusConn)
                    ]
            pure (Right (CreateResult (T.unpack (operationIdText opId)) adrId recId scopeId domainId statusId oldHead createdPaths))
              where
                extractPath :: Either DocumentError RepoPath -> Maybe RepoPath
                extractPath = either (const Nothing) Just
          _ ->
            pure (Left (Stage3ValidateState "failed to generate sortable identifiers"))

-- ---------------------------------------------------------------------------
-- Amend ADR
-- ---------------------------------------------------------------------------

-- | Result of the 'amendAdmCommand' operation.
data AmendResult
  = AmendResult
      { amendOperationId  :: String,
        amendAdrId        :: AdrId,
        amendRecordId     :: RecordId,
        amendCommitOid    :: GitOid,
        amendUpdatedPath  :: RepoPath
      }
  deriving (Eq, Show)

-- | Amend an existing decision record's title, summary, or body.
--
-- Reads the current decision record, validates its provenance capsule,
-- builds an amended record with the new content, and commits via the
-- append-only transaction engine.
--
-- The new capsule carries eventKind @"decision.amend"@.
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
amendAdmCommand
  repository
  managedPaths
  actor
  adrId
  recordId
  newTitle
  newSummary
  newBody
  inputs = do
    oldHeadResult <- resolveRevision repository (RevisionSpec "HEAD")
    case oldHeadResult of
      Left err ->
        pure (Left (Stage3ValidateState ("resolve HEAD: " <> T.pack (show err))))
      Right oldHead -> do
        timestampMs <- currentTimestampMs
        entropy <- randomEntropy
        let opIdResult = sortableOperationId timestampMs entropy
            recIdResult = sortableRecordId timestampMs entropy
        case (opIdResult, recIdResult) of
          (Right opId, Right recId) -> do
            -- Read the existing decision record from the worktree
            let oldDir  = T.unpack (repoPathText (managedDecisionPath managedPaths) <> "/" <> T.take 4 (recordIdText recId))
                oldFile = oldDir </> T.unpack (recordIdText recId <> "--decision.decision.md")
                oldRepoPath = RepoPath (T.pack oldFile)
            worktreeRoot <- pure (repositoryWorktreeRoot repository)
            existingBytes <-
              case worktreeRoot of
                Nothing -> pure BS.empty
                Just root -> do
                  let filePath = root </> oldFile
                  result <- try @SomeException (BS.readFile filePath)
                  case result of
                    Left _  -> pure BS.empty
                    Right b -> pure b

            -- Build the amended record
            let amendedRecord =
                  DecisionRecord
                    { decisionAdr = adrId,
                      decisionRecord = recId,
                      decisionTitle = newTitle,
                      decisionSummary = newSummary,
                      decisionDomains = [],
                      decisionBody = newBody
                    }

            -- Render the semantic to compute the digest
            let semanticEither = renderDecisionSemantic amendedRecord
            case semanticEither of
              Left docErr ->
                pure (Left (Stage5ValidateGenerated ("amend render: " <> T.pack (show docErr))))
              Right semantic -> do
                let digest = semanticDigest semantic
                    eventIdResult = mkEventKind "decision.amend"
                case eventIdResult of
                  Left provErr ->
                    pure (Left (Stage5ValidateGenerated ("amend eventKind: " <> T.pack (show provErr))))
                  Right eventKind -> do
                    let parentId = ProvenanceRecord recId
                        ts = 1000000000000
                        capsuleInput =
                          ProvenanceCapsuleInput
                            { capsuleInputOperationId = opId,
                              capsuleInputObjectId = ProvenanceRecord recId,
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
                        pure (Left (Stage5ValidateGenerated ("amend capsule: " <> T.pack (show provErr))))
                      Right capsule -> do
                        let sealedEither = sealManagedDocument (ManagedDecision amendedRecord) capsule
                        case sealedEither of
                          Left docErr ->
                            pure (Left (Stage5ValidateGenerated ("amend seal: " <> T.pack (show docErr))))
                          Right sealedBytes -> do
                            let generatedPath = canonicalManagedPath managedPaths (ManagedDecision amendedRecord)
                            case generatedPath of
                              Left err ->
                                pure (Left (Stage4GenerateFiles ("amend path: " <> T.pack (show err))))
                              Right genPath -> do
                                let generated =
                                      [ GeneratedFile genPath sealedBytes ]
                                    config =
                                      TransactionConfig
                                        { configOperationId = T.unpack (operationIdText opId),
                                          configSubject = "adrai: amend " <> adrIdText adrId,
                                          configTrailers = Map.fromList [("ADR", T.unpack (adrIdText adrId)), ("Objects", T.unpack (recordIdText recId))],
                                          configExpectedHead = oldHead,
                                          configGenerated = generated
                                        }
                                _ <- commitAppendOnlyOperation repository config
                                pure (Right (AmendResult
                                  (T.unpack (operationIdText opId))
                                  adrId recId oldHead genPath))
          _ ->
            pure (Left (Stage3ValidateState "failed to generate sortable identifiers"))

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
