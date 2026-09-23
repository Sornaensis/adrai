{-# LANGUAGE OverloadedStrings #-}

module WindowFixtureMain (main) where

import Adrai.ConflictFixture (ConflictFixtureResult (..), runConflictFixture)
import Adrai.Format.Document (ParsedManagedDocument (..))
import Adrai.Git (Repository (..), RevisionSpec (..), discoverRepository, resolveRevision, systemGit)
import Adrai.Provenance (gitOidText, mkGitOid)
import Adrai.Types (adrIdText, repoPathText)
import Adrai.WindowDocuments (windowDocuments)
import Control.Monad (forM_, unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy.Char8 as LazyByteString8
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesPathExist, pathIsSymbolicLink)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>), isAbsolute, makeRelative, normalise, splitDirectories, takeDirectory)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    [root, rawBasis, "1001"] -> emit root rawBasis
    ["--conflicts", root] -> emitConflicts root
    _ -> die "usage: adrai-window-fixture <repository-root> <basis-oid> 1001 | --conflicts <repository-root>"

emitConflicts :: FilePath -> IO ()
emitConflicts root = do
  outcome <- runConflictFixture root
  case outcome of
    Left problem -> do
      hPutStrLn stderr ("P705_CONFLICT_EMITTER_STAGE=" <> conflictStageCode problem)
      die "conflict fixture failed"
    Right result -> LazyByteString8.putStrLn (Aeson.encode (Aeson.object
      [ "baseHead" Aeson..= gitOidText (conflictFixtureBaseHead result),
        "head" Aeson..= gitOidText (conflictFixtureHead result),
        "adrs" Aeson..= fmap adrIdText (conflictFixtureAdrs result),
        "transactions" Aeson..= conflictFixtureTransactions result
      ]))

conflictStageCode :: Text.Text -> String
conflictStageCode problem
  | problem `elem` ["fixture repository root does not exist", "fixture root canonicalization failed", "fixture requires a worktree root", "fixture requires the exact worktree root"] = "root"
  | problem == "repository discovery failed" = "discovery"
  | problem `elem` ["repository HEAD check failed", "fixture requires the main branch", "HEAD resolution failed"] = "head"
  | problem == "repository snapshot failed" = "snapshot"
  | problem `elem` ["fixture actor invalid", "fixture domain invalid", "fixture scope invalid", "fixture ADR ID invalid", "fixture record ID invalid", "fixture scope addition invalid", "fixture domain refinement invalid", "fixture ADR mapping missing"] = "typed"
  | problem `elem` ["create transaction failed", "create returned a different ADR ID", "fixture ADR IDs are not distinct"] = "create"
  | problem == "git switch failed" = "switch"
  | problem == "amend transaction failed" = "amend"
  | problem == "scope transaction failed" = "scope"
  | problem == "domain transaction failed" = "domain"
  | problem == "obsolete transaction failed" = "obsolete"
  | problem == "git merge failed" = "merge"
  | problem == "git rev-list failed" = "ancestry"
  | problem `elem` ["fixture merge did not advance HEAD", "fixture transaction count mismatch", "fixture commit topology count mismatch"] = "topology"
  | otherwise = "unknown"

emit :: FilePath -> String -> IO ()
emit root rawBasis = do
  exists <- doesDirectoryExist root
  unless exists (die "repository root does not exist")
  canonicalRoot <- canonicalizePath root
  discovered <- discoverRepository systemGit canonicalRoot >>= either (die . show) pure
  discoveredRoot <- maybe (die "repository has no worktree root") canonicalizePath (repositoryWorktreeRoot discovered)
  unless (normalise canonicalRoot == normalise discoveredRoot) (die "repository root must be the exact worktree root")
  basis <- either (die . show) pure (mkGitOid (Text.pack rawBasis))
  headOid <- resolveRevision discovered (RevisionSpec "HEAD") >>= either (die . show) pure
  unless (basis == headOid) (die "basis OID must equal the repository HEAD before fixture emission")
  let documents = concatMap (windowDocuments basis) [1 .. 1001]
      paths = map (canonicalRoot </>) (map (Text.unpack . repoPathText . parsedManagedPath) documents)
      parents = Set.toList (Set.fromList (concatMap (parentChain canonicalRoot . takeDirectory) paths))
  unless (length documents == 4004 && Set.size (Set.fromList paths) == 4004) (die "window fixture paths are not unique")
  forM_ parents (checkExistingParent canonicalRoot)
  forM_ paths $ \path -> do
    occupied <- doesPathExist path
    when occupied (die "window fixture target already exists")
  forM_ (zip paths documents) $ \(path, document) -> do
    createDirectoryIfMissing True (takeDirectory path)
    checkExistingParent canonicalRoot (takeDirectory path)
    ByteString.writeFile path (parsedManagedBytes document)
  putStrLn ("documents=4004 decisions=1001 basis=" <> Text.unpack (gitOidText basis))

parentChain :: FilePath -> FilePath -> [FilePath]
parentChain root path
  | normalise root == normalise path = [root]
  | otherwise = parentChain root (takeDirectory path) <> [path]

checkExistingParent :: FilePath -> FilePath -> IO ()
checkExistingParent root parent = do
  exists <- doesPathExist parent
  when exists $ do
    directory <- doesDirectoryExist parent
    unless directory (die "window fixture parent is not a directory")
    linked <- pathIsSymbolicLink parent
    when linked (die "window fixture parent is a link or junction")
    resolved <- canonicalizePath parent
    let relative = makeRelative root resolved
    unless (not (isAbsolute relative) && all (/= "..") (splitDirectories relative))
      (die "window fixture parent escapes the repository root")
