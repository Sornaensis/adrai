{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Executable contract tests for public mutation commands and their
-- transaction results, including conflict reconciliation.

module Adrai.MutationE2ETest (tests) where

import Adrai.Integration.CLI
import Adrai.Domain (Domain, mkDomain)
import Adrai.Format.Config (defaultConfigText)
import Adrai.Format.Document
  ( AppliesToPayload (..),
    AmendsPayload (..),
    ConnectionPayload (..),
    ConnectionRecord (..),
    DecisionRecord (..),
    DomainsPayload (..),
    ManagedRecord (..),
    ParsedManagedDocument (..),
    StatusPayload (..),
    StatusState (StatusActive),
    canonicalManagedPath,
    parseManagedDocument,
    renderManagedSemantic,
    sealManagedDocument,
  )
import Adrai.Format.Json (JsonValue (..), renderCanonicalJson)
import Adrai.Git (GitOid (..), Repository, discoverRepository, gitOidText, systemGit)
import Adrai.Graph (AxisResolution (..), ReducedAdr (..), lookupReducedAdr, reduceManagedGraph, reducedStateToken)
import Adrai.Provenance
  ( ProvenanceCapsule,
    ProvenanceCapsuleInput (..),
    ProvenanceObjectId (..),
    eventKindText,
    mkEventKind,
    mkProvenanceCapsule,
    provenanceActor,
    provenanceBasis,
    provenanceBranchHint,
    provenanceEventKind,
    provenanceInputs,
    provenanceLineAnchors,
    provenanceObjectId,
    provenanceOperationId,
     provenanceParents,
    semanticDigest,
    sha256Digest,
    provenanceTimestampMs,
     provenanceToolVersion,
     provenanceUpstreamHint,
  )
import Adrai.Scope (mkScopePattern)
import Adrai.Types
  ( Actor,
    ActorKind (HumanActor),
    AdrId,
    OperationId,
    ProvenanceInputs (..),
    RecordId,
    adrIdText,
     connectionIdText,
     configManagedPaths,
     defaultConfig,
     mkActor,
     mkAdrId,
     mkConnectionId,
     mkOperationId,
     mkRecordId,
     mkRepoPath,
    operationIdText,
    recordIdText,
     repoPathText,
     stateTokenText,
  )
import Adrai.Service.PostCommitIndex
  ( PostCommitIndexResult (..),
    compilePostCommitIndex,
  )
import Control.Exception (bracket)
import Data.List (sort)
import Data.Text (Text, strip, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Database.SQLite.Simple (Only (..), close, open, query_)
import System.Directory
  ( createDirectoryIfMissing,
     doesDirectoryExist,
     listDirectory,
  )
import System.FilePath ((</>), takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import System.Exit (ExitCode (..))
import System.Environment (getEnvironment, lookupEnv)
import System.Process.Typed
  ( readProcess,
    setEnv,
    proc,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( ( @?= ),
    assertBool,
    assertFailure,
    testCase,
  )

-- ---------------------------------------------------------------------------
-- JSON helpers
-- ---------------------------------------------------------------------------

_Object :: Data.Aeson.Value -> Maybe (KM.KeyMap Data.Aeson.Value)
_Object (Data.Aeson.Object o) = Just o
_Object _                     = Nothing

(.:) :: Data.Aeson.FromJSON a => KM.KeyMap Data.Aeson.Value -> Text -> Maybe a
(.:) km key =
  case KM.lookup (AesonKey.fromText key) km of
    Nothing -> Nothing
    Just v ->
      case Data.Aeson.eitherDecode (Data.Aeson.encode v) of
        Left _  -> Nothing
        Right a -> Just a

-- ---------------------------------------------------------------------------
-- Test helpers
-- ---------------------------------------------------------------------------

-- | Get HEAD commit hash of a repository.
headCommit :: FilePath -> IO Text
headCommit repo =
  gitStdout repo ["rev-parse", "HEAD"]
    >>= \h -> pure (strip (decodeUtf8 (LBS.toStrict h)))

symbolicHeadRef :: FilePath -> IO Text
symbolicHeadRef repo =
  gitStdout repo ["symbolic-ref", "--quiet", "HEAD"]
    >>= \ref -> pure (strip (decodeUtf8 (LBS.toStrict ref)))

requireRepository :: FilePath -> IO Repository
requireRepository location =
  discoverRepository systemGit location >>= \case
    Left problem -> assertFailure ("could not discover test repository: " <> show problem) >> fail "unreachable"
    Right repository -> pure repository

-- | Launch the real executable selected explicitly by the test environment.
-- These P6-02A cases deliberately never fall back to a PATH lookup: they are
-- executable-bound contract tests rather than tests of a development shell.
adraiRequiredRaw :: FilePath -> [String] -> IO (ExitCode, LBS.ByteString, LBS.ByteString)
adraiRequiredRaw repoPath args = do
  maybeExe <- lookupEnv "ADRAI_EXE"
  exe <-
    case maybeExe of
      Nothing -> assertFailure "P6-02A requires ADRAI_EXE to name the executable under test" >> fail "unreachable"
      Just "" -> assertFailure "P6-02A requires ADRAI_EXE to be non-empty" >> fail "unreachable"
      Just path -> pure path
  inheritedEnv <- getEnvironment
  let mergedEnv = isolatedGitEnvironment inheritedEnv
  readProcess (setEnv mergedEnv (proc exe (adraiTestArgs repoPath args)))

-- | Retain only the variables required to launch child processes on Windows,
-- comparing names case-insensitively, then overlay the deterministic Git test
-- environment.  This excludes inherited repository/config controls such as
-- mixed-case GIT_DIR, GIT_INDEX_FILE, GIT_CONFIG_*, HOME, and USERPROFILE.
isolatedGitEnvironment :: [(String, String)] -> [(String, String)]
isolatedGitEnvironment inheritedEnv =
  gitEnv
    <> filter
      ( \(key, _) ->
          foldedEnvironmentKey key `elem` requiredProcessEnvironment
            && all ((/= foldedEnvironmentKey key) . foldedEnvironmentKey . fst) gitEnv
      )
      inheritedEnv
  where
    requiredProcessEnvironment =
      map foldedEnvironmentKey
        [ "PATH",
          "PATHEXT",
          "SYSTEMROOT",
          "WINDIR",
          "COMSPEC",
          "TEMP",
          "TMP"
        ]

foldedEnvironmentKey :: String -> Text
foldedEnvironmentKey = T.toCaseFold . T.pack

assertIndexResolvedOid :: FilePath -> Text -> IO ()
assertIndexResolvedOid database expected =
  indexResolvedOid database >>= (@?= [Only expected])

indexResolvedOid :: FilePath -> IO [Only Text]
indexResolvedOid database =
  bracket (open database) close $ \connection -> do
    query_ connection "SELECT value FROM meta WHERE key = 'resolved_oid'"

data MutationFailureBaseline = MutationFailureBaseline
  { failureHead :: Text,
    failureSymbolicRef :: Text,
    failureSymbolicRefOid :: Text,
    failureTree :: LBS.ByteString,
    failureIndex :: LBS.ByteString,
    failureCachedBytes :: LBS.ByteString,
    failureWorktreeBytes :: LBS.ByteString,
    failureManagedPaths :: [Text],
    failureManagedWorktree :: [(Text, BS.ByteString)],
    failureStatus :: LBS.ByteString,
    failureIndexResolvedOid :: [Only Text],
    failureOwnedDirectories :: [(FilePath, [FilePath])]
  }
  deriving (Eq, Show)

captureMutationFailureBaseline :: FilePath -> FilePath -> IO MutationFailureBaseline
captureMutationFailureBaseline repo database = do
  currentHead <- headCommit repo
  currentRef <- symbolicHeadRef repo
  currentRefOid <- strip <$> gitText repo ["rev-parse", T.unpack currentRef]
  completeTree <- gitStdout repo ["ls-tree", "-r", "--name-only", "HEAD"]
  completeIndex <- gitStdout repo ["ls-files", "--stage"]
  cachedBytes <- gitStdout repo ["diff", "--cached", "--binary"]
  worktreeBytes <- gitStdout repo ["diff", "--binary"]
  let managedPaths = filter isManagedPath (T.lines (decodeUtf8 (LBS.toStrict completeTree)))
  worktree <- mapM (\path -> (,) path <$> BS.readFile (repo </> unpack path)) managedPaths
  status <- gitStdout repo ["status", "--porcelain=v1", "--untracked-files=all"]
  resolvedOid <- indexResolvedOid database
  ownedDirectories <- mapM captureOwned ["architecture/adrai/decisions", "architecture/adrai/connections", ".adrai"]
  pure (MutationFailureBaseline currentHead currentRef currentRefOid completeTree completeIndex cachedBytes worktreeBytes managedPaths worktree status resolvedOid ownedDirectories)
  where
    isManagedPath path =
      "architecture/adrai/decisions/" `T.isPrefixOf` path
        || "architecture/adrai/connections/" `T.isPrefixOf` path
    captureOwned relative = do
      let path = repo </> relative
      exists <- doesDirectoryExist path
      entries <- if exists then sort <$> listDirectory path else pure []
      pure (relative, entries)

assertMutationFailurePreserved :: FilePath -> FilePath -> MutationFailureBaseline -> IO ()
assertMutationFailurePreserved repo database baseline =
  captureMutationFailureBaseline repo database >>= (@?= baseline)

configureDeterministicGit :: FilePath -> IO ()
configureDeterministicGit repo = do
  git repo ["config", "user.name", "ADRAI P6-02A"]
  git repo ["config", "user.email", "p6-02a@example.invalid"]
  git repo ["config", "commit.gpgSign", "false"]
  git repo ["config", "tag.gpgSign", "false"]
  git repo ["config", "core.autocrlf", "false"]
  git repo ["config", "core.safecrlf", "false"]
  git repo ["config", "core.hooksPath", ".git/adrai-no-hooks"]

assertExitSuccess :: String -> (ExitCode, LBS.ByteString, LBS.ByteString) -> IO LBS.ByteString
assertExitSuccess label (exitCode, stdout, stderr) =
  case exitCode of
    ExitFailure _ ->
      assertFailure
        ( label <> " exited " <> show exitCode
            <> "\nstdout:\n" <> T.unpack (decodeUtf8 (LBS.toStrict stdout))
            <> "\nstderr:\n" <> T.unpack (decodeUtf8 (LBS.toStrict stderr))
        )
    ExitSuccess -> do
      stderr @?= ""
      assertBool (label <> " JSON must end in exactly one LF") (LBS.isSuffixOf "\n" stdout && not (LBS.isSuffixOf "\n\n" stdout))
      pure stdout

requireJsonField :: Data.Aeson.FromJSON a => String -> Data.Aeson.Value -> Text -> IO a
requireJsonField label value key =
  case _Object value >>= (.: key) of
    Nothing ->
      assertFailure
        ( label <> " JSON has no valid " <> unpack key <> " field"
            <> "; full JSON: " <> T.unpack (decodeUtf8 (LBS.toStrict (Data.Aeson.encode value)))
        )
        >> fail "unreachable"
    Just result -> pure result

decodeCanonicalJson :: String -> LBS.ByteString -> IO Data.Aeson.Value
decodeCanonicalJson label bytes =
  case Data.Aeson.eitherDecode bytes of
    Left problem -> assertFailure (label <> " stdout was not JSON: " <> problem) >> fail "unreachable"
    Right value -> pure value

gitText :: FilePath -> [String] -> IO Text
gitText repo arguments = strip . decodeUtf8 . LBS.toStrict <$> gitStdout repo arguments

commitMessageBytes :: FilePath -> Text -> IO BS.ByteString
commitMessageBytes repo commit = do
  rawCommit <- LBS.toStrict <$> gitStdout repo ["cat-file", "commit", T.unpack commit]
  let (_, messageWithSeparator) = BS.breakSubstring "\n\n" rawCommit
  if BS.null messageWithSeparator
    then assertFailure "Git commit object has no header/message separator" >> fail "unreachable"
    else pure (BS.drop 2 messageWithSeparator)

assertStagedBinaryPreserved :: FilePath -> FilePath -> BS.ByteString -> LBS.ByteString -> IO ()
assertStagedBinaryPreserved repo relativePath expected indexBefore = do
  indexAfter <- gitStdout repo ["ls-files", "-s", "--", relativePath]
  indexAfter @?= indexBefore
  BS.readFile (repo </> relativePath) >>= (@?= expected)

assertUntracked :: FilePath -> FilePath -> IO ()
assertUntracked repo relativePath = do
  (exitCode, _, _) <- readProcess (setEnv gitEnv (proc "git" ["-C", repo, "ls-files", "--error-unmatch", "--", relativePath]))
  assertBool (relativePath <> " must remain untracked") (exitCode /= ExitSuccess)

-- =====================================================================
-- P6-02A: public init/create through the executable under test
-- =====================================================================

testP602ARealExecutable :: TestTree
testP602ARealExecutable =
  testGroup
    "P6-02 compact real executable command wiring"
    [ testCase "launcher scrubs hostile mixed-case Git environment controls" p602aHostileEnvironmentScrubbed,
      testCase "public commands preserve a staged binary and return canonical exits" p602aCreate
     ]

testP603A0RealExecutable :: TestTree
testP603A0RealExecutable =
  testGroup "P6-03A.0 real executable ordinary amend reconciliation"
    [testCase "ordinary amend reconciles merged decision heads" p603a0OrdinaryAmendReconciles]

p603a0OrdinaryAmendReconciles :: IO ()
p603a0OrdinaryAmendReconciles =
  withSystemTempDirectory "adrai p6-03a0 reconcile" $ \temporary -> do
    let repo = temporary </> "reconcile"
        database = repo </> ".adrai" </> "index.sqlite"
        stagedPath = "caller-staged.bin"
        stagedBytes = BS.pack [7, 0, 255, 19]
        dirtyPath = "seed.txt"
    createDirectoryIfMissing True repo
    git repo ["init", "--initial-branch=main"]
    configureDeterministicGit repo
    BS.writeFile (repo </> dirtyPath) "seed\n"
    git repo ["add", "--", dirtyPath]
    git repo ["commit", "-m", "seed"]
    seedBasis <- headCommit repo
    branchActor <- requireRight "fixture actor" (mkActor HumanActor "e2e" Nothing)
    compiler <- requireRight "fixture domain" (mkDomain "compiler")
    sourceScope <- requireRight "fixture scope" (mkScopePattern "src/**")
    fixtureAdr <- requireRight "fixture ADR" (mkAdrId (fixtureIdentifier 'A' '1'))
    fixtureRootRecord <- requireRight "fixture root record" (mkRecordId (fixtureIdentifier 'R' '1'))
    fixtureScopeConnection <- requireRight "fixture scope connection" (mkConnectionId (fixtureIdentifier 'C' '1'))
    fixtureDomainConnection <- requireRight "fixture domain connection" (mkConnectionId (fixtureIdentifier 'C' '4'))
    fixtureStatusConnection <- requireRight "fixture status connection" (mkConnectionId (fixtureIdentifier 'C' '5'))
    fixtureRootOperation <- requireRight "fixture root operation" (mkOperationId (fixtureIdentifier 'O' '1'))
    rootFiles <-
      sealFixtureOperation branchActor (GitOid seedBasis) "main" fixtureRootOperation
        [ ( ManagedDecision
              DecisionRecord
                { decisionAdr = fixtureAdr,
                  decisionRecord = fixtureRootRecord,
                  decisionTitle = "Original",
                  decisionSummary = "Original summary",
                  decisionDomains = [compiler],
                  decisionBody = "Original body\n"
                },
            "decision.create",
            []
          ),
          ( ManagedConnection
              ConnectionRecord
                { connectionRecordId = fixtureScopeConnection,
                  connectionPayload = AppliesToConnection (AppliesToPayload fixtureAdr [] "initial" [sourceScope] [] [sourceScope]),
                  connectionRationale = "Initial scope.\n"
                },
            "scope.initial",
            [ProvenanceRecord fixtureRootRecord]
          ),
          ( ManagedConnection
              ConnectionRecord
                { connectionRecordId = fixtureDomainConnection,
                  connectionPayload = DomainsConnection (DomainsPayload fixtureAdr [] "initial" [compiler] [] [compiler] []),
                  connectionRationale = "Initial domain.\n"
                },
            "domain.initial",
            [ProvenanceRecord fixtureRootRecord]
          ),
          ( ManagedConnection
              ConnectionRecord
                { connectionRecordId = fixtureStatusConnection,
                  connectionPayload = StatusConnection (StatusPayload fixtureAdr [] StatusActive [fixtureRootRecord] Nothing),
                  connectionRationale = "Initial active status.\n"
                },
            "status.initial",
            [ProvenanceRecord fixtureRootRecord]
          )
        ]
    BS.writeFile (repo </> ".adrai.toml") (encodeUtf8 defaultConfigText)
    BS.writeFile (repo </> ".gitattributes") "architecture/adrai/decisions/** text eol=lf\narchitecture/adrai/connections/** text eol=lf\n"
    BS.writeFile (repo </> ".gitignore") ".adrai/\n"
    writeFixtureFiles repo rootFiles
    git repo (["add", "--", ".adrai.toml", ".gitattributes", ".gitignore"] <> map (T.unpack . fst) rootFiles)
    git repo ["commit", "-m", "sealed root decision"]
    root <- headCommit repo
    rootDocument <-
      case rootFiles of
        (rootPath, _) : _ -> parseCommittedAndWorktreeDocument repo root rootPath
        [] -> assertFailure "sealed root operation returned no documents" >> fail "unreachable"
    (adrId, originalRecordId, rootOperation) <-
      case rootDocument of
        ParsedManagedDocument _ (ManagedDecision decision) capsule _ _ -> do
          provenanceObjectId capsule @?= ProvenanceRecord (decisionRecord decision)
          provenanceParents capsule @?= []
          pure (decisionAdr decision, decisionRecord decision, provenanceOperationId capsule)
        _ -> assertFailure "first sealed root member is not a decision" >> fail "unreachable"
    let adr = adrIdText adrId
    assertCanonicalIdentifier "fixture root operation" 'O' (operationIdText rootOperation)
    git repo ["switch", "-c", "reconcile-first"]
    firstFiles <- sealedFixtureAmendment branchActor compiler (GitOid root) "reconcile-first" adrId originalRecordId '2' "First searchable branch" "First summary" "first searchable body\n" "first branch\n"
    firstCommit <- commitFixtureOperation repo "sealed first branch amendment" firstFiles
    git repo ["switch", "main"]
    headCommit repo >>= (@?= root)
    secondFiles <- sealedFixtureAmendment branchActor compiler (GitOid root) "main" adrId originalRecordId '3' "Second searchable branch" "Second summary" "second searchable body\n" "second branch\n"
    secondCommit <- commitFixtureOperation repo "sealed second branch amendment" secondFiles
    git repo ["merge", "--no-ff", "reconcile-first", "-m", "merge decision heads"]
    conflictHead <- headCommit repo
    mergeParents <- T.words <$> gitText repo ["show", "-s", "--format=%P", T.unpack conflictHead]
    mergeParents @?= [secondCommit, firstCommit]
    conflictPaths <- fmap T.lines (gitText repo ["ls-tree", "-r", "--name-only", T.unpack conflictHead, "--", "architecture/adrai"])
    conflictDocuments <- mapM (parseCommittedAndWorktreeDocument repo conflictHead) conflictPaths
    let documentsFor files =
          filter ((`elem` map fst files) . repoPathText . parsedManagedPath) conflictDocuments
        mergedRootDocuments = documentsFor rootFiles
    length mergedRootDocuments @?= 4
    mapM_ (\document -> provenanceBasis (parsedManagedCapsule document) @?= GitOid seedBasis) mergedRootDocuments
    mapM_ (\document -> provenanceOperationId (parsedManagedCapsule document) @?= rootOperation) mergedRootDocuments
    (firstRecordId, firstOperation) <- assertFixtureAmendment "first" (GitOid root) adrId originalRecordId (documentsFor firstFiles)
    (secondRecordId, secondOperation) <- assertFixtureAmendment "second" (GitOid root) adrId originalRecordId (documentsFor secondFiles)
    assertBool "fixture amendments are independent operations" (firstOperation /= secondOperation && firstOperation /= rootOperation && secondOperation /= rootOperation)
    assertBool "fixture amendments create distinct decision records" (firstRecordId /= secondRecordId)
    let parentIds = sort [firstRecordId, secondRecordId]
        parentRecords = map recordIdText parentIds
    conflictToken <- case lookupReducedAdr adrId (reduceManagedGraph (map parsedManagedRecord conflictDocuments)) of
      Just reduced -> do
        axisResolutionHeads (reducedDecisionAxis reduced) @?= parentIds
        pure (reducedStateToken reduced)
      Nothing -> assertFailure "merged amendment heads must reduce to their ADR" >> fail "unreachable"
    createDirectoryIfMissing True (takeDirectory database)
    repository <- requireRepository repo
    seededIndex <- compilePostCommitIndex repository (GitOid conflictHead) database
    postCommitIndexed seededIndex @?= True
    postCommitDatabase seededIndex @?= Just database
    postCommitIndexRevision seededIndex @?= Just (GitOid conflictHead)
    postCommitIndexWarnings seededIndex @?= []
    postCommitIndexError seededIndex @?= Nothing
    incompleteBaseline <- captureMutationFailureBaseline repo database
    (incompleteExit, incompleteStdout, incompleteStderr) <- adraiRequiredRaw repo ["amend", unpack adr, "--title", "Incomplete", "--summary", "Incomplete summary", "--change-summary", "incomplete reconciliation", "--body", "", "--actor", "human:e2e", "--json"]
    incompleteExit @?= ExitFailure 3
    incompleteStdout @?= ""
    incompleteStderr @?= "adrai: conflict: Stage3ValidateState \"amend decision conflict requires title, summary, and body\"\n"
    assertMutationFailurePreserved repo database incompleteBaseline
    BS.writeFile (repo </> stagedPath) stagedBytes
    git repo ["add", "--", stagedPath]
    stagedIndexBefore <- gitStdout repo ["ls-files", "-s", "--", stagedPath]
    BS.writeFile (repo </> dirtyPath) "caller worktree bytes\n"
    dirtyIndexBefore <- gitStdout repo ["ls-files", "-s", "--", dirtyPath]
    dirtyWorktreeBefore <- BS.readFile (repo </> dirtyPath)
    reconcileStdout <- assertExitSuccess "reconcile amend" =<< adraiRequiredRaw repo ["amend", unpack adr, "--title", "Reconciled", "--summary", "Reconciled summary", "--change-summary", "reconcile decision heads", "--body", "reconciled body\n", "--expect", unpack (stateTokenText conflictToken), "--actor", "human:e2e", "--json"]
    reconcileResult <- decodeCanonicalJson "reconcile amend" reconcileStdout
    record <- requireJsonField "reconcile amend" reconcileResult "record" :: IO Text
    operation <- requireJsonField "reconcile amend" reconcileResult "operation" :: IO Text
    publicParents <- requireJsonField "reconcile amend" reconcileResult "amends" :: IO [Text]
    publicParents @?= parentRecords
    created <- requireJsonField "reconcile amend" reconcileResult "created" :: IO [Text]
    publicCommit <- requireJsonField "reconcile amend" reconcileResult "commit" :: IO Text
    publicIndexRevision <- requireJsonField "reconcile amend" reconcileResult "index_revision" :: IO Text
    publicDatabase <- requireJsonField "reconcile amend" reconcileResult "database" :: IO Text
    publicWarnings <- requireJsonField "reconcile amend" reconcileResult "index_warnings" :: IO Integer
    current <- headCommit repo
    publicCommit @?= current
    publicIndexRevision @?= current
    publicDatabase @?= T.pack database
    publicWarnings @?= 0
    assertCanonicalIdentifier "reconciliation operation" 'O' operation
    assertCanonicalIdentifier "reconciliation record" 'R' record
    length created @?= 2
    length (filter (T.isSuffixOf ".decision.md") created) @?= 1
    length (filter (T.isSuffixOf "--amends.connection.md") created) @?= 1
    documents <- mapM (parseCommittedAndWorktreeDocument repo current) created
    case [payload | ParsedManagedDocument _ (ManagedConnection connection) _ _ _ <- documents, AmendsConnection payload <- [connectionPayload connection]] of
      [payload] -> do
        map recordIdText (amendsToRecords payload) @?= parentRecords
        amendsFromRecord payload @?= either (error . show) id (mkRecordId record)
      other -> assertFailure ("expected one reconciliation edge, got " <> show other)
    let expectedParents = map ProvenanceRecord parentIds
    mapM_ (\document -> provenanceParents (parsedManagedCapsule document) @?= expectedParents) documents
    resolvedPaths <- fmap T.lines (gitText repo ["ls-tree", "-r", "--name-only", T.unpack current, "--", "architecture/adrai"])
    resolvedDocuments <- mapM (parseCommittedAndWorktreeDocument repo current) resolvedPaths
    case lookupReducedAdr adrId (reduceManagedGraph (map parsedManagedRecord resolvedDocuments)) of
      Just reduced -> do
        reducedConflictAxes reduced @?= []
        axisResolutionHeads (reducedDecisionAxis reduced) @?= [either (error . show) id (mkRecordId record)]
      Nothing -> assertFailure "reconciliation result must reduce to its ADR"
    assertStagedBinaryPreserved repo stagedPath stagedBytes stagedIndexBefore
    gitStdout repo ["ls-files", "-s", "--", dirtyPath] >>= (@?= dirtyIndexBefore)
    BS.readFile (repo </> dirtyPath) >>= (@?= dirtyWorktreeBefore)
    assertIndexResolvedOid database current
    staleBaseline <- captureMutationFailureBaseline repo database
    (staleExit, staleStdout, staleStderr) <- adraiRequiredRaw repo ["amend", unpack adr, "--title", "Stale", "--summary", "Stale summary", "--change-summary", "stale reconcile", "--body", "stale body\n", "--expect", unpack (stateTokenText conflictToken), "--actor", "human:e2e", "--json"]
    staleExit @?= ExitFailure 3
    staleStdout @?= ""
    assertBool "stale reconcile must use the frozen conflict error" ("adrai: conflict: Stage3ValidateState \"stale ADR state:" `LBS.isPrefixOf` staleStderr)
    assertMutationFailurePreserved repo database staleBaseline

p602aHostileEnvironmentScrubbed :: IO ()
p602aHostileEnvironmentScrubbed = do
  let inherited =
        [ ("gIt_DiR", "hostile-dir"),
          ("GiT_ObJeCt_DiReCtOrY", "hostile-objects"),
          ("gIt_CoNfIg_CoUnT", "1"),
          ("hOmE", "hostile-home"),
          ("UsErPrOfIlE", "hostile-profile"),
          ("PaTh", "deterministic-path"),
          ("sYsTeMrOoT", "deterministic-system-root"),
          ("GiT_PaGeR", "hostile-pager")
        ]
  isolatedGitEnvironment inherited
    @?= gitEnv
      <> [ ("PaTh", "deterministic-path"),
           ("sYsTeMrOoT", "deterministic-system-root")
         ]

p602aCreate :: IO ()
p602aCreate =
  withSystemTempDirectory "adrai p6-02a create" $ \temporary -> do
    let repo = temporary </> "seeded"
        stagedName = "unrelated-create.bin"
        stagedBytes = BS.pack [222, 173, 0, 190, 239, 10]
        title = "Real executable decision"
        summary = "Exercise the public create command."
        body = "## Decision\nUse the real executable.\n"
        database = repo </> ".adrai" </> "index.sqlite"
    createDirectoryIfMissing True repo
    git repo ["init", "--initial-branch=main"]
    configureDeterministicGit repo
    BS.writeFile (repo </> "seed.txt") "normal seed\n"
    git repo ["add", "--", "seed.txt"]
    git repo ["commit", "-m", "normal seed"]
    initStdout <- assertExitSuccess "setup init" =<< adraiRequiredRaw repo ["init", "--json"]
    initResult <- decodeCanonicalJson "setup init" initStdout
    (requireJsonField "setup init" initResult "initialized" :: IO Bool) >>= (@?= True)
    (requireJsonField "setup init" initResult "operation" :: IO Text) >>= (@?= "init")
    (sort <$> (requireJsonField "setup init" initResult "created" :: IO [Text]))
      >>= (@?= [".adrai.toml", ".gitattributes", ".gitignore"])
    (requireJsonField "setup init" initResult "index_updated" :: IO Bool) >>= (@?= True)
    initCommit <- headCommit repo
    assertIndexResolvedOid database initCommit
    BS.writeFile (repo </> stagedName) stagedBytes
    git repo ["add", "--", stagedName]
    indexBefore <- gitStdout repo ["ls-files", "-s", "--", stagedName]

    stdout <-
      assertExitSuccess "create" =<< adraiRequiredRaw repo
        [ "create",
          "--title", title,
          "--summary", summary,
          "--body", body,
          "--domain", "compiler",
          "--applies-to", "src/**",
          "--actor", "human:e2e",
          "--json"
        ]
    result <- decodeCanonicalJson "create" stdout
    operation <- requireJsonField "create" result "operation" :: IO Text
    adr <- requireJsonField "create" result "adr" :: IO Text
    record <- requireJsonField "create" result "record" :: IO Text
    scope <- requireJsonField "create" result "scope" :: IO Text
    domain <- requireJsonField "create" result "domain" :: IO Text
    status <- requireJsonField "create" result "status" :: IO Text
    commit <- requireJsonField "create" result "commit" :: IO Text
    created <- requireJsonField "create" result "created" :: IO [Text]
    indexRevision <- requireJsonField "create" result "index_revision" :: IO Text
    renderedDatabase <- requireJsonField "create" result "database" :: IO Text
    warningCount <- requireJsonField "create" result "index_warnings" :: IO Integer
    let expectedPaths =
          [ "architecture/adrai/decisions/" <> T.take 4 record <> "/" <> record <> "--real-executable-decision.decision.md",
            "architecture/adrai/connections/" <> T.take 4 scope <> "/" <> scope <> "--applies_to.connection.md",
            "architecture/adrai/connections/" <> T.take 4 domain <> "/" <> domain <> "--domains.connection.md",
            "architecture/adrai/connections/" <> T.take 4 status <> "/" <> status <> "--status.connection.md"
          ]
    currentHead <- headCommit repo
    assertBool "create must advance beyond the init commit" (currentHead /= initCommit)
    assertIndexResolvedOid database currentHead
    commit @?= currentHead
    indexRevision @?= currentHead
    renderedDatabase @?= T.pack database
    warningCount @?= 0
    created @?= expectedPaths
    assertCanonicalIdentifier "operation" 'O' operation
    assertCanonicalIdentifier "ADR" 'A' adr
    assertCanonicalIdentifier "record" 'R' record
    mapM_ (assertCanonicalIdentifier "connection" 'C') [scope, domain, status]
    stdout
      @?= LBS.fromStrict (encodeUtf8 (createJson operation adr record scope domain status currentHead expectedPaths (T.pack database)))

    parents <- gitText repo ["show", "-s", "--format=%P", T.unpack currentHead]
    parents @?= initCommit
    commitMessage <- commitMessageBytes repo currentHead
    commitMessage
      @?= encodeUtf8
        ( "adrai: create " <> adr <> "\n\n"
            <> "ADRAI-Op: " <> operation <> "\n"
            <> "ADRAI-ADR: " <> adr <> "\n"
            <> "ADRAI-Objects: " <> T.intercalate "," [record, scope, domain, status] <> "\n"
        )
    changedPaths <- fmap (sort . T.lines) (gitText repo ["diff-tree", "--no-commit-id", "--name-only", "-r", T.unpack currentHead])
    changedPaths
      @?= sort expectedPaths

    documents <- mapM (parseCommittedAndWorktreeDocument repo currentHead) expectedPaths
    assertCreatedDocumentSemantics operation initCommit adr record scope domain status title summary body documents
    assertStagedBinaryPreserved repo stagedName stagedBytes indexBefore
    stagedPaths <- gitStdout repo ["diff", "--cached", "--name-only"]
    stagedPaths @?= LBS.fromStrict (encodeUtf8 (T.pack stagedName <> "\n"))
    mapM_ (assertGeneratedPathUnstaged repo) expectedPaths
    assertUntracked repo ".adrai/index.sqlite"

    (invalidExit, invalidStdout, invalidStderr) <- adraiRequiredRaw repo ["show", "A00000000000000000000000000", "--json"]
    invalidExit @?= ExitFailure 2
    invalidStdout @?= ""
    invalidStderr @?= "adrai: ADRAI reference not found in this revision: A00000000000000000000000000\n"
    assertStagedBinaryPreserved repo stagedName stagedBytes indexBefore

requireRight :: Show problem => String -> Either problem value -> IO value
requireRight label = \case
  Left problem -> assertFailure (label <> ": " <> show problem) >> fail "unreachable"
  Right value -> pure value

fixtureIdentifier :: Char -> Char -> Text
fixtureIdentifier prefix suffix = T.singleton prefix <> T.replicate 25 "0" <> T.singleton suffix

sealFixtureOperation
  :: Actor
  -> GitOid
  -> Text
  -> OperationId
  -> [(ManagedRecord, Text, [ProvenanceObjectId])]
  -> IO [(Text, BS.ByteString)]
sealFixtureOperation actor basis branch operation =
  mapM $ \(managed, eventText, parents) -> do
    semantic <- requireRight "fixture semantic" (renderManagedSemantic managed)
    event <- requireRight "fixture event" (mkEventKind eventText)
    capsule <-
      requireRight "fixture capsule" $
        mkProvenanceCapsule
          ProvenanceCapsuleInput
            { capsuleInputOperationId = operation,
              capsuleInputObjectId = fixtureManagedObject managed,
              capsuleInputEventKind = event,
              capsuleInputActor = actor,
              capsuleInputTimestampMs = 1700000000000,
              capsuleInputBasis = basis,
              capsuleInputParents = parents,
              capsuleInputBranchHint = Just branch,
              capsuleInputUpstreamHint = Nothing,
              capsuleInputLineAnchors = [],
              capsuleInputSemanticDigest = semanticDigest semantic,
              capsuleInputToolVersion = "adrai/1.0.0",
              capsuleInputDigests = ProvenanceInputs Nothing Nothing Nothing
            }
    bytes <- requireRight "sealed fixture document" (sealManagedDocument managed capsule)
    path <- requireRight "fixture managed path" (canonicalManagedPath (configManagedPaths defaultConfig) managed)
    pure (repoPathText path, bytes)

fixtureManagedObject :: ManagedRecord -> ProvenanceObjectId
fixtureManagedObject managed =
  case managed of
    ManagedDecision decision -> ProvenanceRecord (decisionRecord decision)
    ManagedConnection connection -> ProvenanceConnection (connectionRecordId connection)

writeFixtureFiles :: FilePath -> [(Text, BS.ByteString)] -> IO ()
writeFixtureFiles repo =
  mapM_ $ \(relative, bytes) -> do
    let path = repo </> T.unpack relative
    createDirectoryIfMissing True (takeDirectory path)
    BS.writeFile path bytes

commitFixtureOperation :: FilePath -> String -> [(Text, BS.ByteString)] -> IO Text
commitFixtureOperation repo message files = do
  writeFixtureFiles repo files
  git repo (["add", "--"] <> map (T.unpack . fst) files)
  git repo ["commit", "-m", message]
  headCommit repo

sealedFixtureAmendment
  :: Actor
  -> Domain
  -> GitOid
  -> Text
  -> AdrId
  -> RecordId
  -> Char
  -> Text
  -> Text
  -> Text
  -> Text
  -> IO [(Text, BS.ByteString)]
sealedFixtureAmendment actor domain basis branch adr parent suffix title summary body rationale = do
  record <- requireRight "fixture amended record" (mkRecordId (fixtureIdentifier 'R' suffix))
  connection <- requireRight "fixture amendment connection" (mkConnectionId (fixtureIdentifier 'C' suffix))
  operation <- requireRight "fixture amendment operation" (mkOperationId (fixtureIdentifier 'O' suffix))
  sealFixtureOperation actor basis branch operation
    [ ( ManagedDecision
          DecisionRecord
            { decisionAdr = adr,
              decisionRecord = record,
              decisionTitle = title,
              decisionSummary = summary,
              decisionDomains = [domain],
              decisionBody = body
            },
        "decision.amend",
        [ProvenanceRecord parent]
      ),
      ( ManagedConnection
          ConnectionRecord
            { connectionRecordId = connection,
              connectionPayload = AmendsConnection (AmendsPayload adr record [parent]),
              connectionRationale = rationale
            },
        "connection.amends",
        [ProvenanceRecord parent]
      )
    ]

assertFixtureAmendment
  :: String
  -> GitOid
  -> AdrId
  -> RecordId
  -> [ParsedManagedDocument]
  -> IO (RecordId, OperationId)
assertFixtureAmendment label basis adr parent documents =
  case (decisions, amendments) of
    ([(decision, decisionCapsule)], [(connection, payload, connectionCapsule)]) -> do
      decisionAdr decision @?= adr
      amendsSubjectAdr payload @?= adr
      amendsFromRecord payload @?= decisionRecord decision
      amendsToRecords payload @?= [parent]
      provenanceObjectId decisionCapsule @?= ProvenanceRecord (decisionRecord decision)
      provenanceObjectId connectionCapsule @?= ProvenanceConnection (connectionRecordId connection)
      provenanceOperationId connectionCapsule @?= provenanceOperationId decisionCapsule
      mapM_ (\capsule -> provenanceBasis capsule @?= basis) [decisionCapsule, connectionCapsule]
      mapM_ (\capsule -> provenanceParents capsule @?= [ProvenanceRecord parent]) [decisionCapsule, connectionCapsule]
      eventKindText (provenanceEventKind decisionCapsule) @?= "decision.amend"
      eventKindText (provenanceEventKind connectionCapsule) @?= "connection.amends"
      pure (decisionRecord decision, provenanceOperationId decisionCapsule)
    _ -> assertFailure (label <> " fixture must contain one decision and one amends connection") >> fail "unreachable"
  where
    decisions = [(decision, capsule) | ParsedManagedDocument _ (ManagedDecision decision) capsule _ _ <- documents]
    amendments =
      [ (connection, payload, capsule)
        | ParsedManagedDocument _ (ManagedConnection connection) capsule _ _ <- documents,
          AmendsConnection payload <- [connectionPayload connection]
      ]

createJson :: Text -> Text -> Text -> Text -> Text -> Text -> Text -> [Text] -> Text -> Text
createJson operation adr record scope domain status commit created database =
  renderCanonicalJson
    ( JsonObject
        [ ("adr", JsonString adr),
          ("commit", JsonString commit),
          ("committed", JsonBool True),
          ("created", JsonArray (map JsonString created)),
          ("database", JsonString database),
          ("domain", JsonString domain),
          ("domains", JsonArray [JsonString "compiler"]),
          ("index_revision", JsonString commit),
          ("index_updated", JsonBool True),
          ("index_warnings", JsonNumber 0),
          ("indexed", JsonBool True),
          ("operation", JsonString operation),
          ("record", JsonString record),
          ("scope", JsonString scope),
          ("status", JsonString status)
        ]
    )

assertCanonicalIdentifier :: String -> Char -> Text -> IO ()
assertCanonicalIdentifier label prefix identifier = do
  assertBool (label <> " identifier has its expected prefix") (T.isPrefixOf (T.singleton prefix) identifier)
  T.length identifier @?= 27

parseCommittedAndWorktreeDocument :: FilePath -> Text -> Text -> IO ParsedManagedDocument
parseCommittedAndWorktreeDocument repo commit pathText = do
  path <-
    case mkRepoPath pathText of
      Left problem -> assertFailure ("invalid expected managed path " <> unpack pathText <> ": " <> show problem) >> fail "unreachable"
      Right value -> pure value
  committed <- LBS.toStrict <$> gitStdout repo ["show", T.unpack (commit <> ":" <> pathText)]
  worktree <- BS.readFile (repo </> T.unpack pathText)
  worktree @?= committed
  parsed <-
    case parseManagedDocument path committed of
      Left problem -> assertFailure ("cannot parse committed managed document " <> unpack pathText <> ": " <> show problem) >> fail "unreachable"
      Right value -> pure value
  sealManagedDocument (parsedManagedRecord parsed) (parsedManagedCapsule parsed) @?= Right committed
  parsedManagedBytes parsed @?= committed
  pure parsed

assertCreatedDocumentSemantics
  :: Text -> Text -> Text -> Text -> Text -> Text -> Text -> String -> String -> String -> [ParsedManagedDocument] -> IO ()
assertCreatedDocumentSemantics operation basis adr record scope domain status title summary body documents = do
  expectedActor <-
    case mkActor HumanActor "e2e" Nothing of
      Left problem -> assertFailure (show problem) >> fail "unreachable"
      Right actor -> pure actor
  expectedDomain <-
    case mkDomain "compiler" of
      Left problem -> assertFailure (show problem) >> fail "unreachable"
      Right value -> pure value
  expectedScope <-
    case mkScopePattern "src/**" of
      Left problem -> assertFailure (show problem) >> fail "unreachable"
      Right value -> pure value
  let capsules = map parsedManagedCapsule documents
      timestamps = map provenanceTimestampMs capsules
      expectedInputs = ProvenanceInputs (Just (sha256Digest (encodeUtf8 (T.pack body)))) Nothing Nothing
  length documents @?= 4
  mapM_ (assertSharedCapsule operation basis expectedActor expectedInputs) capsules
  assertBool
    "all created documents share one positive timestamp"
    (case timestamps of
      timestamp : rest -> timestamp > 0 && all (== timestamp) rest
      [] -> False)
  case [(decision, capsule) | ParsedManagedDocument _ (ManagedDecision decision) capsule _ _ <- documents] of
    [(decision, capsule)] -> do
      adrIdText (decisionAdr decision) @?= adr
      recordIdText (decisionRecord decision) @?= record
      decisionTitle decision @?= T.pack title
      decisionSummary decision @?= T.pack summary
      decisionBody decision @?= T.pack body
      decisionDomains decision @?= [expectedDomain]
      provenanceObjectId capsule @?= ProvenanceRecord (decisionRecord decision)
      eventKindText (provenanceEventKind capsule) @?= "decision.create"
      provenanceParents capsule @?= []
    other -> assertFailure ("expected one decision document, got " <> show (length other))
  case [(connection, payload, capsule) | ParsedManagedDocument _ (ManagedConnection connection) capsule _ _ <- documents, AppliesToConnection payload <- [connectionPayload connection]] of
    [(connection, payload, capsule)] -> do
      connectionIdText (connectionRecordId connection) @?= scope
      adrIdText (appliesToSubjectAdr payload) @?= adr
      appliesToParentConnections payload @?= []
      appliesToChange payload @?= "initial"
      appliesToAdded payload @?= [expectedScope]
      appliesToRemoved payload @?= []
      appliesToEffective payload @?= [expectedScope]
      connectionRationale connection @?= "Initial scope.\n"
      eventKindText (provenanceEventKind capsule) @?= "scope.initial"
      provenanceParents capsule @?= [ProvenanceRecord (recordFromText record)]
    other -> assertFailure ("expected one scope document, got " <> show (length other))
  case [(connection, payload, capsule) | ParsedManagedDocument _ (ManagedConnection connection) capsule _ _ <- documents, DomainsConnection payload <- [connectionPayload connection]] of
    [(connection, payload, capsule)] -> do
      connectionIdText (connectionRecordId connection) @?= domain
      adrIdText (domainsSubjectAdr payload) @?= adr
      domainsParentConnections payload @?= []
      domainsChange payload @?= "initial"
      domainsAdded payload @?= [expectedDomain]
      domainsRemoved payload @?= []
      domainsEffective payload @?= [expectedDomain]
      domainsRefinements payload @?= []
      connectionRationale connection @?= "Initial domain.\n"
      eventKindText (provenanceEventKind capsule) @?= "domain.initial"
      provenanceParents capsule @?= [ProvenanceRecord (recordFromText record)]
    other -> assertFailure ("expected one domain document, got " <> show (length other))
  case [(connection, payload, capsule) | ParsedManagedDocument _ (ManagedConnection connection) capsule _ _ <- documents, StatusConnection payload <- [connectionPayload connection]] of
    [(connection, payload, capsule)] -> do
      connectionIdText (connectionRecordId connection) @?= status
      adrIdText (statusSubjectAdr payload) @?= adr
      statusParentConnections payload @?= []
      statusState payload @?= StatusActive
      map recordIdText (statusRecordHeads payload) @?= [record]
      statusReplacementAdr payload @?= Nothing
      connectionRationale connection @?= "Initial active status.\n"
      eventKindText (provenanceEventKind capsule) @?= "status.initial"
      provenanceParents capsule @?= [ProvenanceRecord (recordFromText record)]
    other -> assertFailure ("expected one status document, got " <> show (length other))
  where
    recordFromText value =
      case [decisionRecord decision | ParsedManagedDocument _ (ManagedDecision decision) _ _ _ <- documents, recordIdText (decisionRecord decision) == value] of
        [identifier] -> identifier
        _ -> error "the decision record was not available for connection provenance assertions"

assertSharedCapsule :: Text -> Text -> Actor -> ProvenanceInputs -> ProvenanceCapsule -> IO ()
assertSharedCapsule operation basis expectedActor expectedInputs capsule = do
  operationIdText (provenanceOperationId capsule) @?= operation
  gitOidText (provenanceBasis capsule) @?= basis
  provenanceActor capsule @?= expectedActor
  provenanceBranchHint capsule @?= Just "main"
  provenanceUpstreamHint capsule @?= Nothing
  provenanceLineAnchors capsule @?= []
  provenanceToolVersion capsule @?= "adrai/1.0.0"
  provenanceInputs capsule @?= expectedInputs

assertGeneratedPathUnstaged :: FilePath -> Text -> IO ()
assertGeneratedPathUnstaged repo path = do
  staged <- gitStdout repo ["diff", "--cached", "--name-only", "--", T.unpack path]
  staged @?= ""

-- =====================================================================
-- Test suite
-- =====================================================================

tests :: TestTree
tests =
  testGroup
    "Mutation E2E across hostile environments (P5-05)"
    [ testP602ARealExecutable,
      testP603A0RealExecutable
    ]
