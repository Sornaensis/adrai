{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Focused real-executable coverage.  This component deliberately owns only
-- a small Haskell fixture; importing the integration/stress support trees
-- would make the cache gate construct unrelated retrieval fixtures.
module Main (main) where

import Adrai.Provenance (GitOid, gitOidText, mkGitOid)
import Adrai.Compiler.CacheSelection (loadCacheMeta, validateCachePublicationContract)
import CacheFixture (healthyCompilerFiles)
import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple (close, execute_, open)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, removeFile)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), takeDirectory)
import System.Info (os)
import System.IO.Temp (withSystemTempDirectory, withTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

main :: IO ()
main = defaultMain $ testGroup "production cache selection"
  [ testCase "p6-06g0-real-executable-cache-selection" realExecutableCacheContract ]

realExecutableCacheContract :: IO ()
realExecutableCacheContract =
  withSystemTempDirectory "adrai tree-identical cli" $ \temporary -> do
    -- Keep the repository itself in a second, freshly-created namespace.
    -- The outer system-temporary directory is normally unique, but Windows
    -- can retain a directory after an interrupted process.  A fixed
    -- @temporary/repository@ child could then inherit a live mutable alias
    -- from that previous fixture.  The test only relies on immutable
    -- revision-addressed snapshots, so each execution owns a new repository
    -- root and waits for every CLI child before that root is released.
    withTempDirectory temporary "repository" $ \repository -> do
      initialiseRepository repository
      basis <- repositoryHead repository
      files <- fixtureOrFail (healthyCompilerFiles basis)
      _ <- writeAndCommit repository "compiler fixture" files

      -- A system temporary-directory name can be recycled after a prior
      -- interrupted test process.  The mutable alias is never cache evidence,
      -- so reset only it before this fixture's initial cold compile.
      removeCurrentIndex repository
      cold <- compile "cold compile" repository
      assertFields "cold compile"
        [ "\"cache_mode\":\"full\"",
          "\"incremental_kind\":\"full\"",
          "\"adrs_rebuilt\":1",
          "\"ann_buckets\":0",
          "\"embedding_computed\":0",
          "\"cache_retain_revisions\":0"
        ] cold
      assertAbsent "cold compile must not claim document reuse" "\"documents_parsed\":0" cold
      coldRevision <- repositoryHead repository
      coldSnapshot <- assertSnapshot repository coldRevision
      healthySnapshot <- validateCachePublicationContract coldSnapshot
      assertBool "the real complete compiler fixture validates for publication" healthySnapshot

      -- The immediate same-revision path must select the immutable archive,
      -- not fall through to a second materialization.  Run it twice: the
      -- second invocation proves alias refresh has not changed its authority.
      firstExact <- compile "first immediate exact reuse" repository
      assertFields "first immediate exact reuse"
        [ "\"cache_mode\":\"exact\""
        , "\"incremental_kind\":\"exact\""
        , "\"documents_parsed\":0"
        , "\"adrs_rebuilt\":0"
        , "\"documents_reused\":4"
        , "\"adrs_reused\":1"
        ] firstExact
      secondExact <- compile "second immediate exact reuse" repository
      assertFields "second immediate exact reuse"
        [ "\"cache_mode\":\"exact\""
        , "\"incremental_kind\":\"exact\""
        , "\"documents_parsed\":0"
        , "\"adrs_rebuilt\":0"
        , "\"documents_reused\":4"
        , "\"adrs_reused\":1"
        ] secondExact

      -- Corruption at the authoritative revision path must never be hidden by
      -- the mutable alias.  Selection must fail closed, cold compile, and
      -- replace the damaged archive before any later exact request can pass.
      corruptExactConnection <- open coldSnapshot
      execute_ corruptExactConnection "DELETE FROM fts_search_exact"
      close corruptExactConnection
      corruptExactArchive <- validateCachePublicationContract coldSnapshot
      assertBool "a corrupted authoritative archive is not publishable" (not corruptExactArchive)
      corruptExactFallback <- compile "corrupt exact archive falls back cold" repository
      assertFields "corrupt exact archive falls back cold"
        [ "\"cache_mode\":\"full\""
        , "\"incremental_kind\":\"full\""
        ] corruptExactFallback
      repairedColdSnapshot <- assertSnapshot repository coldRevision
      repairedColdArchive <- validateCachePublicationContract repairedColdSnapshot
      assertBool "the cold fallback republishes a canonical archive" repairedColdArchive

      -- Both revision labels bind exact reuse.  requested_revision is not part
      -- of either row fingerprint, so this isolates the metadata binding check.
      requestedMismatchConnection <- open repairedColdSnapshot
      execute_ requestedMismatchConnection "UPDATE meta SET value='not-the-resolved-oid' WHERE key='requested_revision'"
      close requestedMismatchConnection
      requestedMismatchArchive <- validateCachePublicationContract repairedColdSnapshot
      assertBool "a requested-revision-mismatched archive is not publishable" (not requestedMismatchArchive)
      requestedMismatchFallback <- compile "requested revision mismatch falls back cold" repository
      assertFields "requested revision mismatch falls back cold"
        [ "\"cache_mode\":\"full\""
        , "\"incremental_kind\":\"full\""
        ] requestedMismatchFallback
      requestedMismatchRepaired <- assertSnapshot repository coldRevision
      requestedMismatchRepairedValid <- validateCachePublicationContract requestedMismatchRepaired
      assertBool "the requested-revision fallback republishes a canonical archive" requestedMismatchRepairedValid

      -- EXCEPT is set-based, so count equality closes the duplicate-row hole
      -- that membership checks alone cannot see in an FTS virtual table.
      duplicateFtsConnection <- open requestedMismatchRepaired
      execute_ duplicateFtsConnection "INSERT INTO fts_search_exact SELECT * FROM fts_search_exact LIMIT 1"
      close duplicateFtsConnection
      duplicateFtsArchive <- validateCachePublicationContract requestedMismatchRepaired
      assertBool "a duplicate FTS row archive is not publishable" (not duplicateFtsArchive)
      duplicateFtsFallback <- compile "duplicate FTS row falls back cold" repository
      assertFields "duplicate FTS row falls back cold"
        [ "\"cache_mode\":\"full\""
        , "\"incremental_kind\":\"full\""
        ] duplicateFtsFallback
      duplicateFtsRepaired <- assertSnapshot repository coldRevision
      duplicateFtsRepairedValid <- validateCachePublicationContract duplicateFtsRepaired
      assertBool "the duplicate-FTS fallback republishes a canonical archive" duplicateFtsRepairedValid

      -- A valid mutable alias alone is never an exact cache hit.  Removing the
      -- authoritative archive in this isolated fixture must take the normal
      -- cold path and recreate that archive.
      let initialAlias = repository </> ".adrai" </> "index.sqlite"
      initialAliasIsPublishable <- validateCachePublicationContract initialAlias
      assertBool "the alias remains valid before its exact archive is removed" initialAliasIsPublishable
      removeFile coldSnapshot
      coldArchiveMissing <- compile "missing exact archive" repository
      assertFields "missing exact archive must cold compile"
        [ "\"cache_mode\":\"full\""
        , "\"incremental_kind\":\"full\""
        ] coldArchiveMissing
      restoredColdSnapshot <- assertSnapshot repository coldRevision

      -- A readable SQLite file can have a healthy page-level integrity check
      -- while one FTS projection has been logically removed.  The full
      -- canonical reader contract must reject it before candidate discovery.
      let corruptLogicalSnapshot = repository </> ".adrai" </> "cache" </> "readable-logical-corruption.sqlite"
      copyFile restoredColdSnapshot corruptLogicalSnapshot
      corruptConnection <- open corruptLogicalSnapshot
      execute_ corruptConnection "DELETE FROM fts_search_exact"
      close corruptConnection
      corruptedSnapshot <- validateCachePublicationContract corruptLogicalSnapshot
      assertBool "a readable database with an incomplete FTS materialization is not publishable" (not corruptedSnapshot)

      -- Both digest fields are commitments to canonical rows, rather than
      -- syntactically-valid opaque tokens.  These values decode to 32 bytes,
      -- so this proves the validator recomputes them instead of merely checking
      -- the base64url shape.
      let forgedMetadataSnapshot = repository </> ".adrai" </> "cache" </> "forged-cache-metadata.sqlite"
      copyFile restoredColdSnapshot forgedMetadataSnapshot
      forgedMetadataConnection <- open forgedMetadataSnapshot
      execute_ forgedMetadataConnection "UPDATE meta SET value='AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA' WHERE key='source_fingerprint'"
      close forgedMetadataConnection
      forgedMetadata <- validateCachePublicationContract forgedMetadataSnapshot
      assertBool "a syntactically valid but forged source fingerprint is not publishable" (not forgedMetadata)

      let forgedMaterializationSnapshot = repository </> ".adrai" </> "cache" </> "forged-materialization-metadata.sqlite"
      copyFile restoredColdSnapshot forgedMaterializationSnapshot
      forgedMaterializationConnection <- open forgedMaterializationSnapshot
      execute_ forgedMaterializationConnection "UPDATE meta SET value='AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA' WHERE key='materialization_fingerprint'"
      close forgedMaterializationConnection
      forgedMaterialization <- validateCachePublicationContract forgedMaterializationSnapshot
      assertBool "a syntactically valid but forged materialization fingerprint is not publishable" (not forgedMaterialization)

      -- The operation wire format is part of the fingerprint proof.  Merely
      -- changing its whitespace does not change the decoded anchor values,
      -- so the validator must also require the exact canonical JSON emitted
      -- by the SQLite writer.
      let nonCanonicalAnchorsSnapshot = repository </> ".adrai" </> "cache" </> "noncanonical-operation-anchors.sqlite"
      copyFile restoredColdSnapshot nonCanonicalAnchorsSnapshot
      nonCanonicalAnchorsConnection <- open nonCanonicalAnchorsSnapshot
      execute_ nonCanonicalAnchorsConnection "UPDATE operation SET line_anchors=line_anchors || ' '"
      close nonCanonicalAnchorsConnection
      nonCanonicalAnchors <- validateCachePublicationContract nonCanonicalAnchorsSnapshot
      assertBool "a non-canonical operation anchor encoding is not publishable" (not nonCanonicalAnchors)

      let forgedFtsTextSnapshot = repository </> ".adrai" </> "cache" </> "forged-fts-text.sqlite"
      copyFile restoredColdSnapshot forgedFtsTextSnapshot
      forgedFtsTextConnection <- open forgedFtsTextSnapshot
      execute_ forgedFtsTextConnection "UPDATE fts_search_exact SET title='forged full-text projection'"
      close forgedFtsTextConnection
      forgedFtsText <- validateCachePublicationContract forgedFtsTextSnapshot
      assertBool "an FTS row with a valid key but forged indexed text is not publishable" (not forgedFtsText)

      let forgedSemanticSnapshot = repository </> ".adrai" </> "cache" </> "forged-semantic-state.sqlite"
      copyFile restoredColdSnapshot forgedSemanticSnapshot
      forgedSemanticConnection <- open forgedSemanticSnapshot
      execute_ forgedSemanticConnection "UPDATE meta SET value='invalid' WHERE key='semantic_state'"
      execute_ forgedSemanticConnection "UPDATE meta SET value='maybe' WHERE key='history_complete'"
      close forgedSemanticConnection
      forgedSemantic <- validateCachePublicationContract forgedSemanticSnapshot
      assertBool "semantic state and history completeness must be derived canonical booleans" (not forgedSemantic)

      -- This focused contract owns the tree-identical cache-selection path, not
      -- the separate Windows warm ReplaceFileW replacement contract.  Removing
      -- only its disposable current index leaves the archived cold snapshot as
      -- the candidate and makes the next compile install a fresh index.
      removeCurrentIndex repository

      _ <- writeAndCommit repository "noise-only fixture" [("src/noise/release.txt", "ordinary product work\n")]
      treeIdentical <- compile "tree-identical reuse" repository
      assertFields "tree-identical reuse"
        [ "\"cache_mode\":\"incremental\"",
          "\"incremental_kind\":\"tree-identical\"",
          "\"documents_parsed\":0",
          "\"documents_reused\":4",
          "\"adrs_reused\":1",
          "\"history_commits_scanned\":1",
          "\"ann_buckets\":0",
          "\"embedding_computed\":0",
          "\"embedding_reused\":0"
        ] treeIdentical
      _ <- assertSnapshot repository =<< repositoryHead repository

      -- The revision archive remains the source of exact reporting even when a
      -- publication-valid alias has divergent metadata.  An exact request must
      -- refresh the alias from the archive before it is read.
      let currentAlias = repository </> ".adrai" </> "index.sqlite"
      aliasConnection <- open currentAlias
      execute_ aliasConnection "UPDATE meta SET value='63' WHERE key='history_commits_scanned'"
      close aliasConnection
      divergentAliasIsPublishable <- validateCachePublicationContract currentAlias
      assertBool "a divergent history counter remains otherwise publication-valid" divergentAliasIsPublishable
      repairedExact <- compile "exact archive repairs divergent alias" repository
      assertFields "exact archive repair"
        [ "\"cache_mode\":\"exact\""
        , "\"incremental_kind\":\"exact\""
        , "\"history_commits_scanned\":1"
        ] repairedExact
      repairedAliasMeta <- loadCacheMeta currentAlias
      assertBool "exact archive repair restores the alias metadata" $
        case repairedAliasMeta of
          Just metadata -> Map.lookup "history_commits_scanned" metadata == Just "1"
          Nothing -> False

      -- Exact classification recovers from the revision-addressed snapshot
      -- when the mutable alias is absent, rather than falling into a cold or
      -- cross-revision selection path.
      removeCurrentIndex repository
      exact <- compile "exact snapshot recovery" repository
      assertFields "exact reuse"
        [ "\"cache_mode\":\"exact\"",
          "\"incremental_kind\":\"exact\"",
          "\"documents_parsed\":0",
          "\"documents_reused\":4",
          "\"adrs_reused\":1",
          "\"ann_buckets\":0",
          "\"embedding_computed\":0",
          "\"embedding_reused\":0"
        ] exact

      -- Endpoint equality is insufficient: a managed-document change followed
      -- by a revert leaves the same tree but adds history diagnostics.  The
      -- bounded per-commit proof must reject it and rebuild cold.  This cache
      -- selection fixture does not own the Windows warm-alias replacement
      -- contract, so discard only its disposable alias before the deliberately
      -- cold compile; immutable revision snapshots remain available candidates.
      removeCurrentIndex repository
      managedPath <-
        case files of
          (path, _) : _ -> pure path
          [] -> assertFailure "healthy compiler fixture unexpectedly has no managed documents" >> fail "unreachable"
      _ <- writeAndCommit repository "temporary managed-document change" [(managedPath, "invalid managed revision\n")]
      git repository ["revert", "--no-edit", "HEAD"]
      changedThenReverted <- compile "managed change/revert" repository
      assertFields "managed change/revert must cold compile"
        [ "\"cache_mode\":\"full\""
        , "\"incremental_kind\":\"full\""
        ] changedThenReverted
      changedThenRevertedSnapshot <- assertSnapshot repository =<< repositoryHead repository
      changedThenRevertedPublishable <- validateCachePublicationContract changedThenRevertedSnapshot
      assertBool "a complete diagnostic-only change/revert snapshot remains publishable" changedThenRevertedPublishable

      -- The same rule covers merge-side history.  Its final tree is unchanged,
      -- but a relevant path was touched in a reachable merged commit.
      removeCurrentIndex repository
      git repository ["switch", "-c", "cache-history-proof"]
      _ <- writeAndCommit repository "branch managed-document change" [(managedPath, "invalid branch managed revision\n")]
      git repository ["revert", "--no-edit", "HEAD"]
      git repository ["switch", "main"]
      git repository ["merge", "--no-ff", "cache-history-proof", "-m", "merge reverted managed history"]
      mergedHistory <- compile "merged managed history" repository
      assertFields "merged managed history must cold compile"
        [ "\"cache_mode\":\"full\""
        , "\"incremental_kind\":\"full\""
        ] mergedHistory

      -- A malformed archive candidate is not a cache.  It must not interfere
      -- with the valid, immutable snapshot chosen for later compiles.
      let corruptCandidate = repository </> ".adrai" </> "cache" </> "unrelated-corrupt.sqlite"
      BS.writeFile corruptCandidate "not a sqlite database"
      _ <- assertSnapshot repository =<< repositoryHead repository

      removeCurrentIndex repository
      _ <- writeAndCommit repository "configuration-path drift"
        [ ( ".adrai.toml"
          , "schema = 1\n[paths]\ndecisions = \"architecture/current/decisions\"\nconnections = \"architecture/current/connections\"\n"
          )
        ]
      configurationChanged <- compile "configuration-path drift" repository
      assertFields "configuration-path drift must cold compile"
        [ "\"cache_mode\":\"full\""
        , "\"incremental_kind\":\"full\""
        ] configurationChanged

initialiseRepository :: FilePath -> IO ()
initialiseRepository repository = do
  createDirectoryIfMissing True repository
  git repository ["init", "--initial-branch", "main"]
  git repository ["config", "user.name", "ADRAI cache test"]
  git repository ["config", "user.email", "adrai-cache-test@example.invalid"]
  _ <- writeAndCommit repository "initial" [("seed.txt", "basis\n")]
  pure ()

writeAndCommit :: FilePath -> String -> [(FilePath, BS.ByteString)] -> IO Text
writeAndCommit repository message files = do
  mapM_ writeOne files
  git repository (["add", "--"] <> map fst files)
  git repository ["commit", "-m", message]
  repositoryHeadText repository
  where
    writeOne (relative, bytes) = do
      let destination = repository </> relative
      createDirectoryIfMissing True (takeDirectory destination)
      BS.writeFile destination bytes

repositoryHead :: FilePath -> IO GitOid
repositoryHead repository = do
  revision <- repositoryHeadText repository
  case mkGitOid revision of
    Left problem -> assertFailure ("fixture HEAD was not a Git OID: " <> show problem) >> fail "unreachable"
    Right oid -> pure oid

repositoryHeadText :: FilePath -> IO Text
repositoryHeadText repository = Text.strip . Text.pack <$> gitStdout repository ["rev-parse", "HEAD"]

fixtureOrFail :: Either Text value -> IO value
fixtureOrFail = either (\problem -> assertFailure ("invalid compiler fixture: " <> Text.unpack problem) >> fail "unreachable") pure

compile :: String -> FilePath -> IO String
compile label repository = do
  testExecutable <- getExecutablePath
  let executable = takeDirectory (takeDirectory testExecutable) </> "adrai" </> ("adrai" <> executableSuffix)
  exists <- doesFileExist executable
  assertBool ("package-built real-executable test target is absent: " <> executable) exists
  -- The build-tool executable is a sibling of this test component in Cabal's
  -- current build directory.  Resolving it from this running test binary (not
  -- the installed bindir) prevents a freshly changed library from being tested
  -- against an older registered adrai executable.
  (status, stdout, stderr) <- readProcessWithExitCode executable
    ["--repo", repository, "compile", "--json", "+RTS", "-N1", "-RTS"] ""
  case status of
    ExitSuccess -> pure stdout
    ExitFailure code -> assertFailure (label <> " real adrai compile failed (" <> show code <> "): " <> stderr) >> fail "unreachable"

executableSuffix :: String
executableSuffix
  | os == "mingw32" = ".exe"
  | otherwise = ""

git :: FilePath -> [String] -> IO ()
git repository arguments = do
  (status, _, stderr) <- readProcessWithExitCode "git" ("-C" : repository : arguments) ""
  case status of
    ExitSuccess -> pure ()
    ExitFailure code -> assertFailure ("git " <> unwords arguments <> " failed (" <> show code <> "): " <> stderr)

gitStdout :: FilePath -> [String] -> IO String
gitStdout repository arguments = do
  (status, stdout, stderr) <- readProcessWithExitCode "git" ("-C" : repository : arguments) ""
  case status of
    ExitSuccess -> pure stdout
    ExitFailure code -> assertFailure ("git " <> unwords arguments <> " failed (" <> show code <> "): " <> stderr) >> fail "unreachable"

assertFields :: String -> [String] -> String -> IO ()
assertFields label expected output =
  mapM_ (\field -> assertBool (label <> " is missing " <> field <> " in " <> output) (field `isInfixOf` compactJson output)) expected

assertAbsent :: String -> String -> String -> IO ()
assertAbsent label field output = assertBool (label <> ": " <> output) (not (field `isInfixOf` compactJson output))

compactJson :: String -> String
compactJson = filter (`notElem` [' ', '\t', '\r', '\n'])

assertSnapshot :: FilePath -> GitOid -> IO FilePath
assertSnapshot repository revision = do
  let snapshot = repository </> ".adrai" </> "cache" </> Text.unpack (gitOidText revision) <> ".sqlite"
  exists <- doesFileExist snapshot
  assertBool ("published cache snapshot is missing: " <> snapshot) exists
  pure snapshot

removeCurrentIndex :: FilePath -> IO ()
removeCurrentIndex repository =
  mapM_ removeIfPresent
    [ repository </> ".adrai" </> "index.sqlite"
    , repository </> ".adrai" </> "index.sqlite-journal"
    , repository </> ".adrai" </> "index.sqlite-shm"
    , repository </> ".adrai" </> "index.sqlite-wal"
    ]
  where
    removeIfPresent path = do
      exists <- doesFileExist path
      if exists then removeFile path else pure ()
