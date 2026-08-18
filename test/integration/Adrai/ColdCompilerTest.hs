{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Adrai.ColdCompilerTest (tests) where

import Adrai.Compiler
import Adrai.Compiler.Snapshot (AnalyzedRepositorySnapshot, analyzeRepositorySnapshot)
import Adrai.Fixture.CompilerRepository
import Adrai.Git
import Adrai.GitTestSupport
import Adrai.Provenance (mkGitOid)
import Adrai.Repository
import Adrai.Retrieval (SearchMaterialization (..), SearchPassage (..))
import Adrai.Sqlite
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Database.SQLite.Simple (Connection, Only (..), Query, close, execute_, open, query_)
import System.Directory (doesFileExist)
import System.FilePath ((</>), makeRelative)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Cold compiler"
    [ testCase "healthy exact-OID revision populates normalized semantics search and six FTS tables" $
        withHealthyRepository $ \repository resolved -> do
          -- Ambient bytes change after resolution and must never enter the cold build.
          BS.writeFile (repository </> decisionPath) "dirty invalid worktree bytes"
          connection <- open ":memory:"
          result <- requireCompiled connection resolved
          coldDatabaseSemanticState (coldCompilerDatabaseStats result) @?= "valid"
          coldDatabaseManagedSourceCount (coldCompilerDatabaseStats result) @?= 4
          coldDatabaseIssueCount (coldCompilerDatabaseStats result) @?= 0
          count connection "decision_record" >>= (@?= 1)
          count connection "connection_record" >>= (@?= 3)
          count connection "operation" >>= (@?= 1)
          count connection "reduced_adr" >>= (@?= 1)
          count connection "search_document" >>= (@?= 1)
          ftsTables <- query_ connection "SELECT count(*) FROM sqlite_master WHERE type='table' AND name LIKE 'fts_%' AND sql LIKE 'CREATE VIRTUAL TABLE%'" :: IO [Only Int64]
          ftsTables @?= [Only 6]
          matches <- query_ connection "SELECT item_id FROM fts_search_stemmed WHERE fts_search_stemmed MATCH 'cold'" :: IO [Only Text]
          assertBool "cold FTS result is present" (not (null matches))
          anchors <- query_ connection "SELECT line_anchors FROM operation" :: IO [Only Text]
          case anchors of
            [Only encoded] -> do
              assertBool "line anchors use a JSON array" (Text.isPrefixOf "[" encoded)
              assertBool "line anchor identifiers preserve @ and escaped newlines" ("logical@anchor\\nsecond-line" `Text.isInfixOf` encoded)
            unexpected -> assertFailure ("unexpected operation anchors: " <> show unexpected)
          scopePayloads <- query_ connection "SELECT payload FROM connection_record WHERE relation_kind='applies_to'" :: IO [Only Text]
          case scopePayloads of
            [Only encoded] -> do
              assertBool "connection payload uses a JSON object" (Text.isPrefixOf "{" encoded)
              assertBool "comma-bearing scope is one quoted JSON value" ("\"src/a,b/**\"" `Text.isInfixOf` encoded)
              assertBool "connection payload has no delimiter NULs" (not (Text.any (== '\NUL') encoded))
            unexpected -> assertFailure ("unexpected scope payload: " <> show unexpected)
          close connection,
      testCase "timestamps beyond SQLite Int64 are preserved exactly" $
        withCompilerRepository largeTimestampCompilerFiles $ \_ resolved -> do
          connection <- open ":memory:"
          result <- requireCompiled connection resolved
          coldDatabaseSemanticState (coldCompilerDatabaseStats result) @?= "valid"
          timestamps <- query_ connection "SELECT timestamp_ms FROM operation" :: IO [Only Text]
          timestamps @?= [Only "9223372036854775808"]
          storageTypes <- query_ connection "SELECT typeof(timestamp_ms) FROM operation" :: IO [Only Text]
          storageTypes @?= [Only "text"]
          close connection,
      testCase "two cold rebuilds have identical fingerprints and logical meta" $
        withHealthyRepository $ \_ resolved -> do
          firstConnection <- open ":memory:"
          secondConnection <- open ":memory:"
          first <- requireCompiled firstConnection resolved
          second <- requireCompiled secondConnection resolved
          coldCompilerMaterializationFingerprint first @?= coldCompilerMaterializationFingerprint second
          firstMeta <- meta firstConnection
          secondMeta <- meta secondConnection
          firstMeta @?= secondMeta
          firstRows <- logicalRows firstConnection
          secondRows <- logicalRows secondConnection
          firstRows @?= secondRows
          close firstConnection
          close secondConnection,
      testCase "materialization fingerprint ignores revision alias and changes on managed mode" $
        withHealthyRepository $ \repository resolvedHead -> do
          resolvedOid <- requireResolved repository (gitOidText (resolvedCommitOid resolvedHead))
          headConnection <- open ":memory:"
          oidConnection <- open ":memory:"
          headResult <- requireCompiled headConnection resolvedHead
          oidResult <- requireCompiled oidConnection resolvedOid
          coldCompilerMaterializationFingerprint headResult @?= coldCompilerMaterializationFingerprint oidResult
          close headConnection
          close oidConnection
          _ <- gitSuccess repository ["update-index", "--chmod=+x", decisionPath] BS.empty
          _ <- gitSuccess repository ["commit", "-m", "managed mode change"] BS.empty
          changed <- requireResolved repository "HEAD"
          changedConnection <- open ":memory:"
          changedResult <- requireCompiled changedConnection changed
          coldDatabaseSemanticState (coldCompilerDatabaseStats changedResult) @?= "valid"
          assertBool
            "managed Git mode changes the materialization fingerprint"
            (coldCompilerMaterializationFingerprint changedResult /= coldCompilerMaterializationFingerprint headResult)
          close changedConnection,
      testCase "valid decision multihead stores ADR_CONFLICT and ADR at record search rows" $
        withCompilerRepository conflictedCompilerFiles $ \_ resolved -> do
          connection <- open ":memory:"
          result <- requireCompiled connection resolved
          coldDatabaseSemanticState (coldCompilerDatabaseStats result) @?= "conflict"
          coldDatabaseIssueCount (coldCompilerDatabaseStats result) @?= 1
          coldDatabaseConflictCount (coldCompilerDatabaseStats result) @?= 1
          count connection "issue" >>= (@?= 1)
          count connection "adr_conflict" >>= (@?= 1)
          issueRows <- query_ connection "SELECT ordinal,code,severity,origin,adr_id,object_id,operation_id,commit_oid,path,message FROM issue ORDER BY ordinal" :: IO [(Int64, Text, Text, Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Text)]
          conflictRows <- query_ connection "SELECT adr_id,code,candidate_count,state_token,summaries FROM adr_conflict ORDER BY adr_id" :: IO [(Text, Text, Int64, Text, Text)]
          case (issueRows, conflictRows) of
            ([(0, "ADR_CONFLICT", "error", "graph", Just issueAdr, Nothing, Nothing, Nothing, Nothing, issueMessage)], [(conflictAdr, "ADR_CONFLICT", candidateCount, stateToken, summaries)]) -> do
              issueAdr @?= conflictAdr
              issueMessage @?= Text.intercalate "; " (Text.splitOn "\n" summaries)
              assertBool "semantic conflict has at least one candidate" (candidateCount >= 1)
              assertBool "semantic conflict retains its state token" (not (Text.null stateToken))
            other -> assertFailure ("unexpected linked conflict rows: " <> show other)
          publishedMeta <- meta connection
          lookup "issue_count" publishedMeta @?= Just "1"
          lookup "conflict_count" publishedMeta @?= Just "1"
          count connection "search_document" >>= (@?= 2)
          itemIds <- query_ connection "SELECT item_id FROM search_document ORDER BY item_id" :: IO [Only Text]
          assertBool "both search candidates use ADR@record identities" (all (Text.isInfixOf "@" . fromOnly) itemIds)
          close connection,
      testCase "compiler diagnostics precede semantic conflict issues deterministically" $
        withSystemTempDirectory "adrai conflict issue ordering" $ \temporary -> do
          let repository = temporary </> "repository"
              unavailableBasis = requireOid (Text.replicate 40 "f")
          initTestRepository repository
          files <- requireFixture (conflictedCompilerFiles unavailableBasis)
          _ <- commitFiles repository files
          resolved <- requireResolved repository "HEAD"
          connection <- open ":memory:"
          result <- requireCompiled connection resolved
          coldDatabaseSemanticState (coldCompilerDatabaseStats result) @?= "conflict"
          coldDatabaseIssueCount (coldCompilerDatabaseStats result) @?= 2
          issueOrder <- query_ connection "SELECT ordinal,code,severity,origin FROM issue ORDER BY ordinal" :: IO [(Int64, Text, Text, Text)]
          issueOrder @?= [(0, "BASIS_COMMIT_UNAVAILABLE", "warning", "basis"), (1, "ADR_CONFLICT", "error", "graph")]
          close connection,
      testCase "invalid committed config creates a diagnostic-only database" $
        withSystemTempDirectory "adrai invalid compiler config" $ \temporary -> do
          let repository = temporary </> "repository"
          initTestRepository repository
          _ <- commitFile repository ".adrai.toml" "not valid toml = ["
          resolved <- requireResolved repository "HEAD"
          connection <- open ":memory:"
          result <- requireCompiled connection resolved
          coldDatabaseSemanticState (coldCompilerDatabaseStats result) @?= "invalid"
          coldCompilerSearchMaterialization result @?= Nothing
          count connection "issue" >>= (@?= 1)
          count connection "managed_source" >>= (@?= 0)
          count connection "search_document" >>= (@?= 0)
          close connection,
      testCase "missing basis is a stable warning and does not block semantics" $
        withExplicitBasis (requireOid (Text.replicate 40 "f")) $ \_ resolved -> do
          connection <- open ":memory:"
          result <- requireCompiled connection resolved
          coldDatabaseSemanticState (coldCompilerDatabaseStats result) @?= "valid"
          issues <- query_ connection "SELECT code,severity FROM issue ORDER BY ordinal" :: IO [(Text, Text)]
          issues @?= [("BASIS_COMMIT_UNAVAILABLE", "warning")]
          close connection,
      testCase "noncommit basis is a stable warning and does not block semantics" $
        withSystemTempDirectory "adrai noncommit basis" $ \temporary -> do
          let repository = temporary </> "repository"
          initTestRepository repository
          basisText <- hashObject repository "basis blob"
          basis <- requireGitOid basisText
          files <- requireFixture (healthyCompilerFiles basis)
          _ <- commitFiles repository files
          resolved <- requireResolved repository "HEAD"
          connection <- open ":memory:"
          result <- requireCompiled connection resolved
          coldDatabaseSemanticState (coldCompilerDatabaseStats result) @?= "valid"
          issues <- query_ connection "SELECT code,severity FROM issue ORDER BY ordinal" :: IO [(Text, Text)]
          issues @?= [("BASIS_COMMIT_UNAVAILABLE", "warning")]
          close connection,
      testCase "custom Unicode committed roots drive the cold source set" $
        withSystemTempDirectory "adrai Unicode compiler roots" $ \temporary -> do
          let repository = temporary </> "repository"
          initTestRepository repository
          basisText <- commitFile repository "seed.txt" "basis"
          basis <- requireGitOid basisText
          defaultFiles <- requireFixture (healthyCompilerFiles basis)
          let relocated = map relocateCompilerPath defaultFiles
              config =
                TextEncoding.encodeUtf8
                  ( Text.unlines
                      [ "schema = 1",
                        "",
                        "[paths]",
                        "decisions = \"" <> Text.pack customDecisionRoot <> "\"",
                        "connections = \"" <> Text.pack customConnectionRoot <> "\""
                      ]
                  )
          _ <- commitFiles repository ((".adrai.toml", config) : relocated)
          resolved <- requireResolved repository "HEAD"
          connection <- open ":memory:"
          result <- requireCompiled connection resolved
          coldDatabaseSemanticState (coldCompilerDatabaseStats result) @?= "valid"
          paths <- query_ connection "SELECT path FROM managed_source ORDER BY path" :: IO [Only Text]
          assertBool
            "Unicode custom roots are persisted"
            ( all
                (\path -> Text.isPrefixOf (Text.pack customDecisionRoot <> "/") path || Text.isPrefixOf (Text.pack customConnectionRoot <> "/") path)
                (map fromOnly paths)
            )
          close connection,
      testCase "exact historical OID remains healthy after later HEAD corruption" $
        withHealthyRepository $ \repository resolved -> do
          _ <- commitFile repository ".adrai.toml" "invalid = ["
          connection <- open ":memory:"
          result <- requireCompiled connection resolved
          coldDatabaseSemanticState (coldCompilerDatabaseStats result) @?= "valid"
          count connection "decision_record" >>= (@?= 1)
          close connection,
      testCase "shallow cold compilation persists incomplete history without blocking semantics" $
        withSystemTempDirectory "adrai shallow cold compiler" $ \temporary -> do
          let source = temporary </> "source"
              shallow = temporary </> "shallow"
          initTestRepository source
          basisText <- commitFile source "seed.txt" "basis"
          basis <- requireGitOid basisText
          files <- requireFixture (healthyCompilerFiles basis)
          _ <- commitFiles source files
          let sourceUri = "file:///" <> map slash source
          _ <- gitSuccess temporary ["clone", "--depth", "1", sourceUri, shallow] BS.empty
          resolved <- requireResolved shallow "HEAD"
          connection <- open ":memory:"
          result <- requireCompiled connection resolved
          coldDatabaseSemanticState (coldCompilerDatabaseStats result) @?= "valid"
          issues <- query_ connection "SELECT code,severity FROM issue ORDER BY code" :: IO [(Text, Text)]
          assertBool "shallow history warning is persisted" (("HISTORY_COVERAGE_INCOMPLETE", "warning") `elem` issues)
          historyComplete <- query_ connection "SELECT value FROM meta WHERE key='history_complete'" :: IO [Only Text]
          historyComplete @?= [Only "false"]
          close connection,
      testCase "nonfresh connection is rejected without modifying existing schema" $
        withHealthyRepository $ \_ resolved -> do
          connection <- open ":memory:"
          execute_ connection "CREATE TABLE caller_owned(value TEXT)"
          coldCompileRepository connection resolved >>= \case
            Left (ColdCompilerDatabaseError (ColdDatabaseNotFresh objects)) ->
              assertBool "caller table is reported" (("table", "caller_owned") `elem` objects)
            result -> assertFailure ("expected nonfresh rejection, got " <> show result)
          count connection "caller_owned" >>= (@?= 0)
          close connection,
      testCase "foreign-key failure rolls back schema and all rows" $
        withHealthyRepository $ \_ resolved -> do
          sourceConnection <- open ":memory:"
          compiled <- requireCompiled sourceConnection resolved
          close sourceConnection
          analyzed <- requireAnalyzed resolved
          materialization <-
            maybe (assertFailure "healthy compile omitted search materialization") pure (coldCompilerSearchMaterialization compiled)
          broken <- breakFirstPassage materialization
          targetConnection <- open ":memory:"
          writeColdDatabase
            targetConnection
            analyzed
            (Just broken)
            (coldCompilerMaterializationFingerprint compiled)
            >>= \case
              Left (ColdDatabaseStorageFailure _) -> pure ()
              result -> assertFailure ("expected transactional FK failure, got " <> show result)
          schema <- query_ targetConnection "SELECT name FROM sqlite_master WHERE name NOT LIKE 'sqlite_%'" :: IO [Only Text]
          schema @?= []
          close targetConnection,
      testCase "on-disk disposable database reopens and leaves no WAL or SHM sidecars" $
        withHealthyRepository $ \repository resolved ->
          withSystemTempDirectory "adrai cold sqlite" $ \temporary -> do
            let database = temporary </> "cold.sqlite"
            connection <- open database
            _ <- requireCompiled connection resolved
            close connection
            reopened <- open database
            values <- meta reopened
            assertBool "reopened database retains schema meta" (("schema", "adrai-cache/1") `elem` values)
            close reopened
            doesFileExist (database <> "-wal") >>= (@?= False)
            doesFileExist (database <> "-shm") >>= (@?= False)
            -- Repository itself remains independent of the caller-owned DB.
            doesFileExist (repository </> ".adrai" </> "cold.sqlite") >>= (@?= False)
    ]

withHealthyRepository :: (FilePath -> ResolvedRepositoryRevision -> IO value) -> IO value
withHealthyRepository = withCompilerRepository healthyCompilerFiles

withCompilerRepository :: (GitOid -> Either Text [(FilePath, BS.ByteString)]) -> (FilePath -> ResolvedRepositoryRevision -> IO value) -> IO value
withCompilerRepository fixture action =
  withSystemTempDirectory "adrai cold compiler" $ \temporary -> do
    let repository = temporary </> "repository"
    initTestRepository repository
    basisText <- commitFile repository "seed.txt" "basis"
    basis <- case mkGitOid basisText of
      Left problem -> assertFailure (show problem)
      Right oid -> pure oid
    files <- case fixture basis of
      Left problem -> assertFailure (Text.unpack problem)
      Right value -> pure value
    _ <- commitFiles repository files
    resolved <- requireResolved repository "HEAD"
    action repository resolved

withExplicitBasis :: GitOid -> (FilePath -> ResolvedRepositoryRevision -> IO value) -> IO value
withExplicitBasis basis action =
  withSystemTempDirectory "adrai explicit compiler basis" $ \temporary -> do
    let repository = temporary </> "repository"
    initTestRepository repository
    files <- requireFixture (healthyCompilerFiles basis)
    _ <- commitFiles repository files
    resolved <- requireResolved repository "HEAD"
    action repository resolved

requireFixture :: Either Text value -> IO value
requireFixture result =
  case result of
    Left problem -> assertFailure (Text.unpack problem)
    Right value -> pure value

requireGitOid :: Text -> IO GitOid
requireGitOid value =
  case mkGitOid value of
    Left problem -> assertFailure (show problem)
    Right oid -> pure oid

requireOid :: Text -> GitOid
requireOid value =
  case mkGitOid value of
    Left problem -> error (show problem)
    Right oid -> oid

relocateCompilerPath :: (FilePath, BS.ByteString) -> (FilePath, BS.ByteString)
relocateCompilerPath (path, bytes)
  | ".decision.md" `Text.isSuffixOf` pathText = (customDecisionRoot </> makeRelative "architecture/adrai/decisions" path, bytes)
  | otherwise = (customConnectionRoot </> makeRelative "architecture/adrai/connections" path, bytes)
  where
    pathText = Text.pack path

customDecisionRoot :: FilePath
customDecisionRoot = "arkitektur/beslutninger-ø"

customConnectionRoot :: FilePath
customConnectionRoot = "arkitektur/forbindelser-å"

requireResolved :: FilePath -> Text -> IO ResolvedRepositoryRevision
requireResolved repository revision = do
  discovered <- discoverRepository systemGit repository >>= \case
    Left problem -> assertFailure (show problem)
    Right value -> pure value
  resolveRepositoryRevision discovered (requireRevision revision) >>= \case
    Left problem -> assertFailure (show problem)
    Right value -> pure value

requireCompiled :: Connection -> ResolvedRepositoryRevision -> IO ColdCompilerResult
requireCompiled connection resolved =
  coldCompileRepository connection resolved >>= \case
    Left problem -> assertFailure (show problem)
    Right result -> pure result

requireAnalyzed :: ResolvedRepositoryRevision -> IO AnalyzedRepositorySnapshot
requireAnalyzed resolved = do
  raw <-
    observeRawRepositorySnapshotAt resolved
      >>= \case
        Left problem -> assertFailure (show problem)
        Right value -> pure value
  analyzeRepositorySnapshot raw
    >>= \case
      Left problem -> assertFailure (show problem)
      Right value -> pure value

count :: Connection -> Text -> IO Int64
count connection table = do
  rows <- query_ connection (fromText ("SELECT count(*) FROM " <> table)) :: IO [Only Int64]
  case rows of
    [Only value] -> pure value
    result -> assertFailure ("unexpected count result: " <> show result)

meta :: Connection -> IO [(Text, Text)]
meta connection = query_ connection "SELECT key,value FROM meta ORDER BY key"

logicalRows :: Connection -> IO [(Text, Text)]
logicalRows connection =
  query_ connection "SELECT 'decision:'||record_id,title FROM decision_record UNION ALL SELECT 'connection:'||connection_id,relation_kind FROM connection_record UNION ALL SELECT 'search:'||item_id,title FROM search_document ORDER BY 1"

breakFirstPassage :: SearchMaterialization -> IO SearchMaterialization
breakFirstPassage materialization =
  case searchMaterializationPassages materialization of
    [] -> assertFailure "healthy materialization has no passage"
    passage : remaining ->
      pure
        materialization
          { searchMaterializationPassages =
              passage {searchPassageDocumentItemId = "missing-search-document"} : remaining
          }

fromText :: Text -> Query
fromText = fromString . Text.unpack

decisionPath :: FilePath
decisionPath = "architecture/adrai/decisions/R000/R00000000000000000000000000--use-a-cold-compiler.decision.md"

slash :: Char -> Char
slash '\\' = '/'
slash character = character
