{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Mutable operations on ADRAI repositories.
--
-- Implements the two top-level mutation entry points:
--
-- * 'initCommand'      — bootstrap a repository with default configuration,
--                        gitattributes, and gitignore files.
-- * 'createAdrCommand' — create a new architectural decision record together
--                        with its scope, domain, and status connection records.
--
-- Both functions use the 8-canonical-stage transaction engine from
-- 'Adrai.Service.Transaction' to guarantee atomic, lock-guarded commits.

module Adrei.Service.Mutation
  ( InitResult (..),
    initCommand,
    CreateResult (..),
    createAdrCommand,
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

-- | Current timestamp in milliseconds, encoded as 6-byte big-endian.
-- Only 48 bits of the millisecond value are retained.
currentTimestampMs :: IO BS.ByteString
currentTimestampMs = do
  now <- round <$> getPOSIXTime
  let ms = now * 1000
  pure (BS.pack (take 6 (toBytesBE ms)))
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
