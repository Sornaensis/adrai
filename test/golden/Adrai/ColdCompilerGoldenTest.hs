{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Adrai.ColdCompilerGoldenTest (tests, writeP403Goldens) where

import Adrai.Compiler
import Adrai.Fixture.CompilerRepository
import Adrai.Git
import Adrai.GitTestSupport
import Adrai.Provenance (mkGitOid)
import Adrai.Repository
import Adrai.RetainedNative.ResidualRepositorySeed
  ( RepositorySeed,
    createRepositorySeed,
    removeRepositorySeed,
    withRepositorySeedCopy,
  )
import Adrai.Sqlite
import Control.Exception (bracket)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Database.SQLite.Simple (Connection, Only (..), Query, close, open, query_)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit ((@?=), assertFailure, testCase)

tests :: TestTree
tests =
  withResource createEmptyRepositorySeed removeRepositorySeed $ \getRepositorySeed ->
    testGroup
      "P4-03 cold compiler goldens"
      [ testCase "declared cold SQLite schema is frozen" (assertGolden "schema.golden" schemaBytes),
        testCase "actual placement-free SQLite logical rows and fingerprints are frozen" $
          logicalDatabaseBytes getRepositorySeed >>= assertGolden "logical.golden",
        testCase "actual diagnostic-only SQLite rows are frozen" $
          diagnosticDatabaseBytes getRepositorySeed >>= assertGolden "diagnostics.golden"
      ]

schemaBytes :: ByteString
schemaBytes =
  TextEncoding.encodeUtf8 . Text.unlines $
    ["cold|" <> name <> "|" <> ddl | (name, ddl) <- coldSchemaDdl]
      <> ["search|" <> schemaName component <> "|" <> ddl | (component, ddl) <- searchOrdinarySchemaDdl]
      <> ["fts|" <> ftsTargetName target <> "|" <> ftsTargetDdl target | target <- allFtsTargets]

logicalDatabaseBytes :: IO RepositorySeed -> IO ByteString
logicalDatabaseBytes getRepositorySeed =
  withCompiledFixture getRepositorySeed healthyCompilerFiles $ \connection -> do
    semanticState <- query_ connection "SELECT value FROM meta WHERE key='semantic_state'" :: IO [Only Text]
    semanticState @?= [Only "valid"]
    issues <- query_ connection "SELECT code,severity FROM issue ORDER BY ordinal" :: IO [(Text, Text)]
    issues @?= [("BASIS_COMMIT_UNAVAILABLE", "warning")]
    dumpLogicalDatabase connection

diagnosticDatabaseBytes :: IO RepositorySeed -> IO ByteString
diagnosticDatabaseBytes getRepositorySeed =
  withCompiledFixture getRepositorySeed malformedCompilerFiles dumpLogicalDatabase

withCompiledFixture :: IO RepositorySeed -> (GitOid -> Either Text [(FilePath, ByteString)]) -> (Connection -> IO value) -> IO value
withCompiledFixture getRepositorySeed fixture action = do
  seed <- getRepositorySeed
  withRepositorySeedCopy seed "adrai p4-03 golden" $ \repository -> do
    let stableUnavailableBasis = requireOid (Text.replicate 40 "a")
    files <- requireFixture (fixture stableUnavailableBasis)
    _ <- commitFiles repository files
    resolved <- requireResolved repository
    connection <- open ":memory:"
    coldCompileRepository connection resolved >>= \case
      Left problem -> close connection >> assertFailure (show problem)
      Right _ -> do
        value <- action connection
        close connection
        pure value

malformedCompilerFiles :: GitOid -> Either Text [(FilePath, ByteString)]
malformedCompilerFiles _ =
  Right [("architecture/adrai/decisions/broken.decision.md", "broken")]

requireResolved :: FilePath -> IO ResolvedRepositoryRevision
requireResolved repository = do
  discovered <- discoverRepository systemGit repository >>= \case
    Left problem -> assertFailure (show problem)
    Right value -> pure value
  resolveRepositoryRevision discovered (requireRevision "HEAD") >>= \case
    Left problem -> assertFailure (show problem)
    Right value -> pure value

dumpLogicalDatabase :: Connection -> IO ByteString
dumpLogicalDatabase connection = do
  sections <- traverse (dumpSpec connection) dumpSpecs
  pure (TextEncoding.encodeUtf8 (Text.unlines (concat sections)))

data DumpSpec = DumpSpec
  { dumpLabel :: Text,
    dumpTable :: Text,
    dumpExpressions :: [Text],
    dumpOrder :: Text
  }

dumpSpec :: Connection -> DumpSpec -> IO [Text]
dumpSpec connection spec = do
  rows <- query_ connection (dumpQuery spec) :: IO [Only Text]
  pure
    ( (dumpLabel spec <> "|count|" <> Text.pack (show (length rows)))
        : [dumpLabel spec <> "|row|" <> row | Only row <- rows]
    )

dumpQuery :: DumpSpec -> Query
dumpQuery spec =
  fromString . Text.unpack $
    "SELECT "
      <> Text.intercalate "||'|'||" (map losslessCell (dumpExpressions spec))
      <> " FROM "
      <> dumpTable spec
      <> " ORDER BY "
      <> dumpOrder spec

losslessCell :: Text -> Text
losslessCell expression =
  "CASE WHEN "
    <> expression
    <> " IS NULL THEN 'null' ELSE 'x'||hex(CAST("
    <> expression
    <> " AS BLOB)) END"

dumpSpecs :: [DumpSpec]
dumpSpecs =
  [ table "meta" ["key", "CASE WHEN key='resolved_oid' THEN '<resolved-oid>' ELSE value END"] "key",
    table "repository_config" ["singleton", "origin", "path", "oid", "object_type", "mode", "bytes", "parse_state", "decision_root", "connection_root"] "singleton",
    table "managed_source" ["path", "oid", "object_type", "mode", "bytes", "parse_state"] "path",
    table "issue" ["ordinal", "code", "severity", "origin", "adr_id", "object_id", "operation_id", "commit_oid", "path", "message"] "ordinal",
    table "adr_conflict" ["adr_id", "code", "candidate_count", "state_token", "summaries"] "adr_id",
    table "operation" ["operation_id", "timestamp_ms", "actor_kind", "actor_id", "actor_model", "basis_oid", "branch_hint", "upstream_hint", "line_anchors", "tool_version", "input_digest", "prompt_digest", "context_digest"] "operation_id",
    table "operation_member" ["operation_id", "object_id", "object_type", "event_kind", "semantic_digest", "path", "blob_oid"] "operation_id,object_id",
    table "operation_member_parent" ["operation_id", "object_id", "ordinal", "parent_object_id"] "operation_id,object_id,ordinal",
    table "decision_record" ["record_id", "adr_id", "operation_id", "title", "summary", "domains", "body", "path"] "record_id",
    table "connection_record" ["connection_id", "adr_id", "operation_id", "relation_kind", "payload", "rationale", "path"] "connection_id",
    table "reduced_adr" ["adr_id", "state_token", "conflicted"] "adr_id",
    table "axis_head" ["adr_id", "axis", "ordinal", "object_id"] "adr_id,axis,ordinal",
    table "current_connection" ["adr_id", "axis", "ordinal", "connection_id"] "adr_id,axis,ordinal",
    table "search_document" ["item_id", "adr_id", "candidate_record_id", "title", "summary", "context", "decision", "consequences", "domains", "rationale", "identifiers", "other", "scope", "source_paths", "obsolete", "conflicted", "state_token"] "item_id",
    table "local_alias" ["alias", "expansion"] "alias",
    table "search_section" ["passage_rowid", "item_id", "search_item_id", "adr_id", "candidate_record_id", "section_kind", "ordinal", "line_start", "line_end", "text", "weight", "source_paths", "identifiers"] "passage_rowid"
  ]
    <> map ftsDump allFtsTargets
  where
    table name columns orderBy = DumpSpec name name columns orderBy
    ftsDump target =
      DumpSpec
        (ftsTargetTable target)
        (ftsTargetTable target)
        ("rowid" : map fst (ftsTargetColumns target))
        "rowid"

schemaName :: SearchStorageComponent -> Text
schemaName component =
  case component of
    SearchSchemaStorage name -> name
    _ -> error "search ordinary schema contained a row-storage component"

assertGolden :: FilePath -> ByteString -> IO ()
assertGolden name actual = BS.readFile (goldenRoot </> name) >>= (@?= actual)

writeP403Goldens :: IO ()
writeP403Goldens =
  bracket createEmptyRepositorySeed removeRepositorySeed $ \repositorySeed -> do
    createDirectoryIfMissing True goldenRoot
    logical <- logicalDatabaseBytes (pure repositorySeed)
    diagnostics <- diagnosticDatabaseBytes (pure repositorySeed)
    BS.writeFile (goldenRoot </> "schema.golden") schemaBytes
    BS.writeFile (goldenRoot </> "logical.golden") logical
    BS.writeFile (goldenRoot </> "diagnostics.golden") diagnostics

goldenRoot :: FilePath
goldenRoot = "test/golden/p4-03"

createEmptyRepositorySeed :: IO RepositorySeed
createEmptyRepositorySeed =
  createRepositorySeed "adrai-p4-03-golden-seed" initTestRepository

requireFixture :: Either Text value -> IO value
requireFixture result =
  case result of
    Left problem -> assertFailure (Text.unpack problem)
    Right value -> pure value

requireOid :: Text -> GitOid
requireOid value =
  case mkGitOid value of
    Left problem -> error (show problem)
    Right oid -> oid
