{-# LANGUAGE OverloadedStrings #-}

module Adrai.ConflictFixture
  ( ConflictFixtureResult (..),
    runConflictFixture,
  )
where

import Adrai.Domain (mkDomain, parseDomainRefinement)
import Adrai.Git
  ( GitHeadState (..),
    GitOid,
    GitProcessResult (..),
    Repository (..),
    RevisionSpec (..),
    discoverRepository,
    gitOidText,
    repositoryHeadState,
    resolveRevision,
    runRepository,
    systemGit,
  )
import Adrai.Repository (repositorySnapshot, repositorySnapshotManagedPaths)
import Adrai.Scope (mkScopePattern)
import Adrai.Service.Mutation
  ( CreateResult (..),
    DomainChangeRequest (..),
    ObsoleteRequest (..),
    ScopeChangeRequest (..),
    amendCurrentAdrCommand,
    changeDomainCommand,
    changeScopeCommand,
    createAdrCommand,
    obsoleteCommand,
  )
import Adrai.Types
  ( Actor,
    ActorKind (..),
    AdrId,
    ManagedPaths,
    ProvenanceInputs (..),
    gitRefText,
    mkActor,
    mkAdrId,
    mkRecordId,
  )
import Control.Exception (IOException, try)
import Control.Monad (forM, unless)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (canonicalizePath, doesDirectoryExist)
import System.Exit (ExitCode (..))
import System.FilePath (equalFilePath)
import Text.Read (readMaybe)

data ConflictFixtureResult = ConflictFixtureResult
  { conflictFixtureBaseHead :: GitOid,
    conflictFixtureHead :: GitOid,
    conflictFixtureAdrs :: Map Text AdrId,
    conflictFixtureTransactions :: Int
  }

type Seed = ExceptT Text IO

-- | Create five real conflict-bearing ADRs and merge independently committed
-- edits from two branches. The caller owns the temporary repository and its
-- cleanup; this function neither initializes it nor changes Git configuration.
runConflictFixture :: FilePath -> IO (Either Text ConflictFixtureResult)
runConflictFixture root = runExceptT $ do
  exists <- liftIO (doesDirectoryExist root)
  unless exists (throwError "fixture repository root does not exist")
  canonicalRoot <- canonicalize root
  repository <- checked "repository discovery failed" (discoverRepository systemGit canonicalRoot)
  worktreeRoot <- maybe (throwError "fixture requires a worktree root") canonicalize (repositoryWorktreeRoot repository)
  unless (equalFilePath canonicalRoot worktreeRoot) (throwError "fixture requires the exact worktree root")
  attached <- checked "repository HEAD check failed" (repositoryHeadState repository)
  case attached of
    GitHeadAttached ref | gitRefText ref == "refs/heads/main" -> pure ()
    _ -> throwError "fixture requires the main branch"
  initialHead <- headOid repository
  snapshot <- checked "repository snapshot failed" (repositorySnapshot repository (RevisionSpec "HEAD"))
  let paths = repositorySnapshotManagedPaths snapshot
  actor <- pureChecked "fixture actor invalid" (mkActor HumanActor "browser-fixture" Nothing)
  core <- pureChecked "fixture domain invalid" (mkDomain "core")
  initialScope <- pureChecked "fixture scope invalid" (mkScopePattern "src/**")
  roots <- forM (zip [1 .. 5 :: Int] ["simultaneous", "decision", "scope", "domain", "status"]) $ \(number, kind) -> do
    let identifier prefix = Text.cons prefix (Text.justifyRight 26 '0' (Text.pack (show number)))
    adr <- pureChecked "fixture ADR ID invalid" (mkAdrId (identifier 'A'))
    record <- pureChecked "fixture record ID invalid" (mkRecordId (identifier 'R'))
    created <- checked "create transaction failed" $
      createAdrCommand repository paths actor adr record
        (kind <> " conflict browser decision")
        ("Checked " <> kind <> " conflict fixture")
        ("Review " <> kind <> " candidate bodies.\n")
        [core] [initialScope] Nothing Nothing Nothing
    unless (createAdrId created == adr) (throwError "create returned a different ADR ID")
    pure (kind, adr)
  let adrs = Map.fromList roots
  unless (Map.size adrs == 5 && Set.size (Set.fromList (Map.elems adrs)) == 5)
    (throwError "fixture ADR IDs are not distinct")
  baseHead <- headOid repository
  git repository "switch" ["-c", "fixture-left"]
  leftCount <- alter repository paths actor adrs "left"
  git repository "switch" ["main"]
  rightCount <- alter repository paths actor adrs "right"
  git repository "merge" ["--no-ff", "fixture-left", "-m", "merge independent browser conflicts"]
  finalHead <- headOid repository
  unless (finalHead /= baseHead && finalHead /= initialHead) (throwError "fixture merge did not advance HEAD")
  let transactions = length roots + leftCount + rightCount
  unless (transactions == 21) (throwError "fixture transaction count mismatch")
  commits <- gitResult repository "rev-list" ["--count", Text.unpack (gitOidText initialHead) <> ".." <> Text.unpack (gitOidText finalHead)]
  unless (readMaybe (ByteString8.unpack (processStdout commits)) == Just (22 :: Int))
    (throwError "fixture commit topology count mismatch")
  pure (ConflictFixtureResult baseHead finalHead adrs transactions)

alter :: Repository -> ManagedPaths -> Actor -> Map Text AdrId -> Text -> Seed Int
alter repository paths actor adrs side = do
  simultaneous <- member "simultaneous"
  decision <- member "decision"
  scope <- member "scope"
  domain <- member "domain"
  status <- member "status"
  let inputs = ProvenanceInputs Nothing Nothing Nothing
      amend adr = checked "amend transaction failed" $
        amendCurrentAdrCommand repository actor adr Nothing
          (side <> " independent decision change")
          (side <> " reviewed decision")
          (side <> " reviewed summary")
          (side <> " candidate decision body\n") inputs
      scopeChange adr = do
        added <- pureChecked "fixture scope addition invalid" (mkScopePattern (if side == "left" then "test/**" else "docs/**"))
        checked "scope transaction failed" $
          changeScopeCommand repository paths actor adr Nothing
            (side <> " independent scope change") (ScopeDelta [added] []) inputs
      domainChange adr = do
        refinement <- pureChecked "fixture domain refinement invalid" (parseDomainRefinement ("core=core." <> side))
        checked "domain transaction failed" $
          changeDomainCommand repository paths actor adr Nothing
            (side <> " independent domain change") (DomainRefine [refinement]) inputs
      obsolete adr = checked "obsolete transaction failed" $
        obsoleteCommand repository paths actor adr
          (ObsoleteRequest Nothing (side <> " independent status change") False Nothing) inputs
  _ <- amend simultaneous
  _ <- scopeChange simultaneous
  _ <- domainChange simultaneous
  _ <- obsolete simultaneous
  _ <- amend decision
  _ <- scopeChange scope
  _ <- domainChange domain
  _ <- obsolete status
  pure (8 :: Int)
  where
    member kind = maybe (throwError "fixture ADR mapping missing") pure (Map.lookup kind adrs)

canonicalize :: FilePath -> Seed FilePath
canonicalize path = do
  result <- liftIO (try (canonicalizePath path) :: IO (Either IOException FilePath))
  either (const (throwError "fixture root canonicalization failed")) pure result

pureChecked :: Text -> Either error value -> Seed value
pureChecked label = either (const (throwError label)) pure

checked :: Text -> IO (Either error value) -> Seed value
checked label action = liftIO action >>= pureChecked label

headOid :: Repository -> Seed GitOid
headOid repository = checked "HEAD resolution failed" (resolveRevision repository (RevisionSpec "HEAD"))

git :: Repository -> Text -> [String] -> Seed ()
git repository command arguments = do
  _ <- gitResult repository command arguments
  pure ()

gitResult :: Repository -> Text -> [String] -> Seed GitProcessResult
gitResult repository command arguments = do
  result <- checked ("git " <> command <> " failed") (runRepository repository command (Text.unpack command : arguments) ByteString.empty)
  unless (processExitCode result == ExitSuccess) (throwError ("git " <> command <> " failed"))
  pure result
