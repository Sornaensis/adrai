{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Adrai.CompilerSearchSqliteTest (tests) where

import Adrai.Compiler
import Adrai.Cli (CompileResult (..))
import Adrai.CliTypes (compileResultValue)
import Adrai.CompilerMaterializationTest (p303RationaleSnapshot)
import Adrai.Graph (GraphAxis (DecisionAxis), reduceManagedGraph)
import Adrai.History (ReadSnapshot (..), RevisionIdentity (..))
import Adrai.Property.Generators
import Adrai.Retrieval
import Adrai.Sqlite
import Adrai.Format.Json (renderCanonicalJson)
import Adrai.Integration.CLI (adraiTestArgs, createTestRepo, gitEnv, gitStdout, parseCompileResult)
import Control.Exception (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Database.SQLite.Simple
  ( Connection,
    Only (Only),
    Query,
    close,
    execute_,
    open,
    query,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)
import Test.Tasty.HUnit (assertFailure)
import System.Exit (ExitCode (..))
import System.Environment (getEnvironment, lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (proc, readProcess, setEnv)

tests :: TestTree
tests =
  testGroup
    "P3-03 compiled search SQLite"
    [ testCase "atomic schema and complete replacement populate ordinary and six FTS targets" populationContract,
      testCase "schema initialization rolls back earlier ordinary and FTS objects on a later target failure" schemaRollbackContract,
      testCase "only current rationale and deterministic first-wins aliases reach ordinary and FTS storage" currentRationaleAndAliasContract,
      testCase "decision conflict candidates are independently retrievable and superseded root is absent" conflictContract,
      testCase "complete replacement removes stale rows and a mid-write failure rolls back" replacementContract,
      testCase "P6-02I real executable compile is revision-bound, canonical, readable, and preserves caller state" p602iRealExecutableCompile
    ]

p602iRealExecutableCompile :: IO ()
p602iRealExecutableCompile =
  withSystemTempDirectory "adrai p6-02i compile ü" $ \temporary -> do
    repository <- createTestRepo (temporary </> "repo parent with spaces")
    _ <- p602iJsonOrThrow repository ["init", "--json"]
    _ <- create repository "Compile ü first decision" "compiler.first"
    historicalRevision <- headOid repository
    _ <- create repository "Compile ü second decision" "compiler.second"
    currentRevision <- headOid repository

    BS.writeFile (repository </> "compile staged.bin") "\NUL\SOHstaged compile bytes\255"
    _ <- gitStdout repository ["add", "--", "compile staged.bin"]
    BS.writeFile (repository </> "README.md") "# Test\ncompile dirty bytes\n"
    BS.writeFile (repository </> "compile untracked ü.txt") "compile untracked UTF-8 bytes"
    beforeHead <- headOid repository
    beforeRef <- gitStdout repository ["symbolic-ref", "--quiet", "HEAD"]
    beforeTree <- gitStdout repository ["ls-tree", "-r", "--name-only", "HEAD"]
    beforeStatus <- gitStdout repository ["status", "--porcelain=v1", "--untracked-files=all", "-z"]
    beforeIndex <- gitStdout repository ["ls-files", "--stage", "-z"]
    beforeCached <- gitStdout repository ["diff", "--cached", "--binary"]
    beforeWorktree <- gitStdout repository ["diff", "--binary"]
    beforeStagedBytes <- BS.readFile (repository </> "compile staged.bin")
    beforeDirtyBytes <- BS.readFile (repository </> "README.md")
    beforeUntrackedBytes <- BS.readFile (repository </> "compile untracked ü.txt")

    current <- compileJson repository ["compile", "--json"]
    coldCompilerRevision current @?= currentRevision
    assertBool "current compile publishes a readable SQLite path" (not (null (coldCompilerDatabase current)))
    coldCompilerCacheMode current @?= "exact"
    coldCompilerIncrementalKind current @?= "exact"
    coldCompilerDocumentsParsed current @?= 0
    coldCompilerDocumentsReused current @?= 8
    coldCompilerAdrsRebuilt current @?= 0
    currentMeta <- readCompileMeta (coldCompilerDatabase current)
    lookup "resolved_oid" currentMeta @?= Just currentRevision
    lookup "managed_source_count" currentMeta @?= Just (Text.pack (show (coldCompilerDocumentsParsed current + coldCompilerDocumentsReused current)))
    lookup "operation_count" currentMeta @?= Just (Text.pack (show (coldCompilerAdrsRebuilt current + coldCompilerAdrsReused current)))
    lookup "search_document_count" currentMeta @?= Just (Text.pack (show (coldCompilerAnnBuckets current)))

    historical <- compileJson repository ["compile", "--at", Text.unpack historicalRevision, "--json"]
    coldCompilerRevision historical @?= historicalRevision
    coldCompilerDatabase historical @?= coldCompilerDatabase current
    coldCompilerCacheMode historical @?= "full"
    coldCompilerIncrementalKind historical @?= "full"
    assertBool "historical compile rebuilds managed documents" (coldCompilerDocumentsParsed historical > 0)
    coldCompilerDocumentsReused historical @?= 0
    assertBool "historical compile rebuilds operations" (coldCompilerAdrsRebuilt historical > 0)
    coldCompilerAdrsReused historical @?= 0
    historicalMeta <- readCompileMeta (coldCompilerDatabase historical)
    lookup "resolved_oid" historicalMeta @?= Just historicalRevision
    lookup "managed_source_count" historicalMeta @?= Just (Text.pack (show (coldCompilerDocumentsParsed historical + coldCompilerDocumentsReused historical)))
    lookup "operation_count" historicalMeta @?= Just (Text.pack (show (coldCompilerAdrsRebuilt historical + coldCompilerAdrsReused historical)))

    currentRebuilt <- compileJson repository ["compile", "--json"]
    coldCompilerRevision currentRebuilt @?= currentRevision
    coldCompilerDatabase currentRebuilt @?= coldCompilerDatabase current
    coldCompilerCacheMode currentRebuilt @?= "full"
    coldCompilerIncrementalKind currentRebuilt @?= "full"
    coldCompilerDocumentsParsed currentRebuilt @?= 8
    coldCompilerDocumentsReused currentRebuilt @?= 0
    assertBool "current compile after historical rebuilds operations" (coldCompilerAdrsRebuilt currentRebuilt > 0)
    coldCompilerAdrsReused currentRebuilt @?= 0
    currentRebuiltMeta <- readCompileMeta (coldCompilerDatabase currentRebuilt)
    lookup "resolved_oid" currentRebuiltMeta @?= Just currentRevision
    lookup "managed_source_count" currentRebuiltMeta @?= Just (Text.pack (show (coldCompilerDocumentsParsed currentRebuilt + coldCompilerDocumentsReused currentRebuilt)))
    lookup "operation_count" currentRebuiltMeta @?= Just (Text.pack (show (coldCompilerAdrsRebuilt currentRebuilt + coldCompilerAdrsReused currentRebuilt)))
    databaseBeforeFailure <- BS.readFile (coldCompilerDatabase currentRebuilt)

    assertFailureCall repository ["compile", "--at", "refs/heads/does-not-exist", "--json"] 2 "adrai: "
    BS.readFile (coldCompilerDatabase currentRebuilt) >>= (@?= databaseBeforeFailure)
    assertFailureCall repository ["compile", "--database", "forbidden.sqlite"] 2 "Invalid option `--database'"
    BS.readFile (coldCompilerDatabase currentRebuilt) >>= (@?= databaseBeforeFailure)

    plainCurrent <- compileJson repository ["compile", "--json"]
    plainCurrent @?= current

    (plainExit, plainOut, plainErr) <- p602iRaw repository ["compile"]
    plainExit @?= ExitSuccess
    plainErr @?= ""
    plainOut
      @?= LBS.fromStrict
        ( TextEncoding.encodeUtf8
            ( Text.unlines
                [ "revision=" <> coldCompilerRevision plainCurrent
                , "database=" <> Text.pack (coldCompilerDatabase plainCurrent)
                , "documents_parsed=" <> Text.pack (show (coldCompilerDocumentsParsed plainCurrent))
                , "issues=" <> Text.pack (show (coldCompilerIssueCount plainCurrent))
                ]
            )
        )
    readCompileMeta (coldCompilerDatabase plainCurrent) >>= \metadata -> lookup "resolved_oid" metadata @?= Just currentRevision

    afterHead <- headOid repository
    afterRef <- gitStdout repository ["symbolic-ref", "--quiet", "HEAD"]
    afterTree <- gitStdout repository ["ls-tree", "-r", "--name-only", "HEAD"]
    afterStatus <- gitStdout repository ["status", "--porcelain=v1", "--untracked-files=all", "-z"]
    afterIndex <- gitStdout repository ["ls-files", "--stage", "-z"]
    afterCached <- gitStdout repository ["diff", "--cached", "--binary"]
    afterWorktree <- gitStdout repository ["diff", "--binary"]
    afterStagedBytes <- BS.readFile (repository </> "compile staged.bin")
    afterDirtyBytes <- BS.readFile (repository </> "README.md")
    afterUntrackedBytes <- BS.readFile (repository </> "compile untracked ü.txt")
    afterHead @?= beforeHead
    afterRef @?= beforeRef
    afterTree @?= beforeTree
    afterStatus @?= beforeStatus
    afterIndex @?= beforeIndex
    afterCached @?= beforeCached
    afterWorktree @?= beforeWorktree
    afterStagedBytes @?= beforeStagedBytes
    afterDirtyBytes @?= beforeDirtyBytes
    afterUntrackedBytes @?= beforeUntrackedBytes
  where
    create repository title domain =
      p602iJsonOrThrow repository
        [ "create"
        , "--title", title
        , "--summary", title <> " summary"
        , "--body", "## Decision\nCompile the selected immutable revision.\n"
        , "--domain", domain
        , "--applies-to", "src/**"
        , "--actor", "human:compiler"
        , "--json"
        ]
    headOid repository = Text.strip . TextEncoding.decodeUtf8 . LBS.toStrict <$> gitStdout repository ["rev-parse", "HEAD"]
    compileJson repository arguments = do
      (exitCode, stdoutBytes, stderrBytes) <- p602iRaw repository arguments
      exitCode @?= ExitSuccess
      stderrBytes @?= ""
      value <- case Aeson.eitherDecode stdoutBytes of
        Left problem -> assertFailure ("compile JSON decode failed: " <> problem) >> fail "unreachable"
        Right decoded -> pure decoded
      result <- case parseCompileResult value of
        Nothing -> assertFailure "compile JSON did not match the frozen result schema" >> fail "unreachable"
        Just parsed -> pure parsed
      stdoutBytes @?= LBS.fromStrict (TextEncoding.encodeUtf8 (renderCanonicalJson (compileResultValue result)))
      case value of
        Aeson.Object object ->
          sort (map AesonKey.toText (KeyMap.keys object))
            @?= sort
              [ "adrs_rebuilt", "adrs_reused", "ann_buckets", "cache_key", "cache_mode"
              , "cache_retain_revisions", "database", "documents_parsed", "documents_reused"
              , "embedding_computed", "embedding_reused", "errors", "history_commits_scanned"
              , "incremental_kind", "issues", "revision", "warnings"
              ]
        _ -> assertFailure "compile JSON was not an object"
      pure result
    assertFailureCall repository arguments expectedExit expectedPrefix = do
      (exitCode, stdoutBytes, stderrBytes) <- p602iRaw repository arguments
      exitCode @?= ExitFailure expectedExit
      stdoutBytes @?= ""
      assertBool
        ("compile failure has deterministic stderr, got " <> show stderrBytes)
        (LBS.fromStrict (TextEncoding.encodeUtf8 expectedPrefix) `LBS.isPrefixOf` stderrBytes)

readCompileMeta :: FilePath -> IO [(Text, Text)]
readCompileMeta database = bracket (open database) close $ \connection ->
  query connection "SELECT key,value FROM meta ORDER BY key" ()

p602iRaw :: FilePath -> [String] -> IO (ExitCode, LBS.ByteString, LBS.ByteString)
p602iRaw repository arguments = do
  executable <- lookupEnv "ADRAI_EXE" >>= \case
    Just path | not (null path) -> pure path
    _ -> assertFailure "P6-02I requires ADRAI_EXE to name the executable under test" >> fail "unreachable"
  inherited <- getEnvironment
  readProcess (setEnv (p602iEnvironment inherited) (proc executable (adraiTestArgs repository arguments)))

p602iJsonOrThrow :: FilePath -> [String] -> IO Aeson.Value
p602iJsonOrThrow repository arguments = do
  (exitCode, stdoutBytes, stderrBytes) <- p602iRaw repository arguments
  case exitCode of
    ExitSuccess ->
      case Aeson.decode stdoutBytes of
        Just value -> pure value
        Nothing -> assertFailure "P6-02I executable emitted non-JSON success output" >> fail "unreachable"
    ExitFailure code ->
      assertFailure ("P6-02I executable failed with exit " <> show code <> ": " <> Text.unpack (TextEncoding.decodeUtf8 (LBS.toStrict stderrBytes))) >> fail "unreachable"

p602iEnvironment :: [(String, String)] -> [(String, String)]
p602iEnvironment inherited =
  gitEnv
    <> filter
      (\(key, _) -> folded key `elem` required && all ((/= folded key) . folded . fst) gitEnv)
      inherited
  where
    required = map folded ["PATH", "PATHEXT", "SYSTEMROOT", "WINDIR", "COMSPEC", "TEMP", "TMP"]
    folded = Text.toCaseFold . Text.pack

schemaRollbackContract :: IO ()
schemaRollbackContract = withMemory $ \connection -> do
  execute_ connection (dynamicQuery (ftsTargetDdl SearchStemmedTarget))
  initializeSearchSchema connection >>= (@?= Left (SearchStorageError (SearchFtsStorage SearchStemmedTarget)))
  remaining <-
    query
      connection
      "SELECT name FROM sqlite_master WHERE type='table' AND name IN ('search_document','local_alias','search_section','fts_search_exact','fts_search_stemmed','fts_search_identifier','fts_passage_exact','fts_passage_stemmed','fts_passage_identifier') ORDER BY name"
      ()
  remaining @?= [Only ("fts_search_stemmed" :: Text)]

currentRationaleAndAliasContract :: IO ()
currentRationaleAndAliasContract = withMemory $ \connection -> do
  initializeSearchSchema connection >>= (@?= Right ())
  let materialization = mustRight (materializeCurrentSearch p303RationaleSnapshot)
      document =
        case searchMaterializationDocuments materialization of
          [value] -> value
          values -> error ("rationale fixture expected one document, got " <> show (length values))
      currentRationale = searchDocumentRationale document
      expectedAliases = searchMaterializationAliases materialization
  replaceSearchMaterialization connection materialization >>= (@?= Right ())
  ordinaryRationale <- query connection "SELECT rationale FROM search_document ORDER BY item_id" ()
  summaryExactRationale <- query connection "SELECT rationale FROM fts_search_exact ORDER BY item_id" ()
  summaryStemmedRationale <- query connection "SELECT rationale FROM fts_search_stemmed ORDER BY item_id" ()
  passageExactRationale <- query connection "SELECT text FROM fts_passage_exact WHERE section_kind='rationale' ORDER BY item_id" ()
  passageStemmedRationale <- query connection "SELECT text FROM fts_passage_stemmed WHERE section_kind='rationale' ORDER BY item_id" ()
  let expectedRationaleRow = [Only currentRationale]
  ordinaryRationale @?= expectedRationaleRow
  summaryExactRationale @?= expectedRationaleRow
  summaryStemmedRationale @?= expectedRationaleRow
  passageExactRationale @?= expectedRationaleRow
  passageStemmedRationale @?= expectedRationaleRow
  loadLocalAliases connection >>= (@?= Right expectedAliases)
  assertBool "aliases are persisted in deterministic order" (expectedAliases == quickSortOn fst expectedAliases)
  assertBool "current first-wins alias is persisted" (("cdapi", "current decision api") `elem` expectedAliases)
  assertBool "historical alias is excluded" (not (any ((== "rsa") . fst) expectedAliases))
  persisted <- persistedSearchSnapshot connection
  let persistedText = Text.toLower (Text.intercalate "\n" (concat persisted))
  assertBool "superseded rationale is absent from all persisted rows" (not ("superseded scope" `Text.isInfixOf` persistedText))
  assertBool "quarantined rationale is absent from all persisted rows" (not ("quarantined-leak" `Text.isInfixOf` persistedText))

populationContract :: IO ()
populationContract = withMemory $ \connection -> do
  initializeSearchSchema connection >>= (@?= Right ())
  tableNames <- query connection "SELECT name FROM sqlite_master WHERE type='table' AND name IN ('search_document','local_alias','search_section','fts_search_exact','fts_search_stemmed','fts_search_identifier','fts_passage_exact','fts_passage_stemmed','fts_passage_identifier') ORDER BY name" ()
  length (tableNames :: [Only Text]) @?= 9
  let fixture = materializeProjectionDag fixedProjectionSpec
      materialization = mustRight (materializeCurrentSearch (projectionFixtureAfter fixture))
  replaceSearchMaterialization connection materialization >>= (@?= Right ())
  countRows connection "search_document" >>= (@?= length (searchMaterializationDocuments materialization))
  countRows connection "search_section" >>= (@?= length (searchMaterializationPassages materialization))
  mapM (\target -> countRows connection (ftsTargetTable target)) [SearchExactTarget, SearchStemmedTarget, SearchIdentifierTarget]
    >>= (@?= replicate 3 (length (searchMaterializationDocuments materialization)))
  mapM (\target -> countRows connection (ftsTargetTable target)) [PassageExactTarget, PassageStemmedTarget, PassageIdentifierTarget]
    >>= (@?= replicate 3 (length (searchMaterializationPassages materialization)))
  let documents = searchMaterializationDocuments materialization
  assertBool "obsolete rows remain stored" (any searchDocumentObsolete documents)
  Set.size (visibleSearchItemIds ExcludeObsolete materialization) @?= length (filter (not . searchDocumentObsolete) documents)
  visibleSearchItemIds IncludeObsolete materialization @?= Set.fromList (map searchDocumentItemId documents)
  passageLinks <- query connection "SELECT section.item_id, passage.item_id FROM search_section section JOIN fts_passage_exact passage ON passage.rowid=section.passage_rowid ORDER BY section.item_id" ()
  passageLinks @?= [(searchPassageId passage, searchPassageId passage) | passage <- sortPassages (searchMaterializationPassages materialization)]

conflictContract :: IO ()
conflictContract = withMemory $ \connection -> do
  initializeSearchSchema connection >>= (@?= Right ())
  let snapshot = conflictSnapshot
      materialization = mustRight (materializeCurrentSearch snapshot)
      documents = searchMaterializationDocuments materialization
      allowed = Set.fromList (map searchDocumentItemId documents)
      limit = mustRight (mkCandidateLimit 10)
  length documents @?= 2
  assertBool "conflict IDs are ADR@RID" (all (Text.isInfixOf "@" . searchDocumentItemId) documents)
  replaceSearchMaterialization connection materialization >>= (@?= Right ())
  left <- runFtsTarget connection SearchExactTarget "\"left\"" allowed limit
  right <- runFtsTarget connection SearchExactTarget "\"right\"" allowed limit
  root <- runFtsTarget connection SearchExactTarget "\"base\"" allowed limit
  map ftsHitItemId (mustRight left) @?= [itemContaining "left" documents]
  map ftsHitItemId (mustRight right) @?= [itemContaining "right" documents]
  root @?= Right []

replacementContract :: IO ()
replacementContract = withMemory $ \connection -> do
  initializeSearchSchema connection >>= (@?= Right ())
  let conflictMaterialization = mustRight (materializeCurrentSearch conflictSnapshot)
      projectionMaterialization = mustRight (materializeCurrentSearch (projectionFixtureAfter (materializeProjectionDag fixedProjectionSpec)))
  replaceSearchMaterialization connection conflictMaterialization >>= (@?= Right ())
  documentIds connection >>= (@?= map searchDocumentItemId (searchMaterializationDocuments conflictMaterialization))
  replaceSearchMaterialization connection projectionMaterialization >>= (@?= Right ())
  documentIds connection >>= (@?= map searchDocumentItemId (searchMaterializationDocuments projectionMaterialization))
  replaceSearchMaterialization connection conflictMaterialization >>= (@?= Right ())
  let oldIds = map searchDocumentItemId (searchMaterializationDocuments conflictMaterialization)
  oldSnapshot <- persistedSearchSnapshot connection
  execute_ connection "CREATE TRIGGER p303_fail_section BEFORE INSERT ON search_section BEGIN SELECT RAISE(ABORT,'forced p3-03 rollback'); END"
  replaceSearchMaterialization connection projectionMaterialization >>= (@?= Left (SearchStorageError SearchPassageStorage))
  documentIds connection >>= (@?= oldIds)
  persistedSearchSnapshot connection >>= (@?= oldSnapshot)

conflictSnapshot :: ReadSnapshot
conflictSnapshot =
  ReadSnapshot
    (RevisionIdentity "conflict" "conflict-resolved")
    documents
    (reduceManagedGraph records)
    Map.empty
  where
    pool = IdentifierPool 0
    fixture = materializeAxisConflict (AxisConflictSpec pool DecisionAxis 0)
    records = axisFixtureRecords fixture
    documents = [parsedDocument pool ordinal (100 + toInteger ordinal) [] record | (ordinal, record) <- zip [0 ..] records]

fixedProjectionSpec :: ProjectionDagSpec
fixedProjectionSpec =
  ProjectionDagSpec
    (IdentifierPool 0)
    (DomainSpec ["compiler", "cache"])
    (ScopeSpec ["src"] True)
    "cache compiler"

itemContaining :: Text -> [SearchDocument] -> Text
itemContaining needle documents =
  case [searchDocumentItemId document | document <- documents, needle `Text.isInfixOf` Text.toLower (searchDocumentTitle document)] of
    itemId : _ -> itemId
    [] -> error "fixture did not contain expected candidate"

documentIds :: Connection -> IO [Text]
documentIds connection = do
  rows <- query connection "SELECT item_id FROM search_document ORDER BY adr_id,candidate_record_id" ()
  pure [itemId | Only itemId <- rows]

countRows :: Connection -> Text -> IO Int
countRows connection table = do
  rows <- query connection (dynamicQuery ("SELECT count(*) FROM " <> table)) ()
  case rows of
    [Only count] -> pure count
    _ -> error "count query returned unexpected rows"

persistedSearchSnapshot :: Connection -> IO [[Text]]
persistedSearchSnapshot connection =
  mapM
    (\(table, columns, ordering) -> serializedRows connection table columns ordering)
    [ ( "search_document",
        [ "item_id",
          "adr_id",
          "candidate_record_id",
          "title",
          "summary",
          "context",
          "decision",
          "consequences",
          "domains",
          "rationale",
          "identifiers",
          "other",
          "scope",
          "source_paths",
          "obsolete",
          "conflicted",
          "state_token"
        ],
        "item_id"
      ),
      ("local_alias", ["alias", "expansion"], "alias"),
      ( "search_section",
        [ "passage_rowid",
          "item_id",
          "search_item_id",
          "adr_id",
          "candidate_record_id",
          "section_kind",
          "ordinal",
          "line_start",
          "line_end",
          "text",
          "weight",
          "source_paths",
          "identifiers"
        ],
        "passage_rowid"
      ),
      ( "fts_search_exact",
        ["rowid", "item_id", "adr_id", "candidate_record_id", "title", "summary", "decision", "domains", "rationale", "context", "consequences", "identifiers"],
        "rowid"
      ),
      ( "fts_search_stemmed",
        ["rowid", "item_id", "adr_id", "candidate_record_id", "title", "summary", "decision", "rationale", "context", "consequences"],
        "rowid"
      ),
      ("fts_search_identifier", ["rowid", "item_id", "adr_id", "candidate_record_id", "identifiers"], "rowid"),
      ("fts_passage_exact", ["rowid", "item_id", "adr_id", "candidate_record_id", "section_kind", "text", "identifiers"], "rowid"),
      ("fts_passage_stemmed", ["rowid", "item_id", "adr_id", "candidate_record_id", "section_kind", "text"], "rowid"),
      ("fts_passage_identifier", ["rowid", "item_id", "adr_id", "candidate_record_id", "section_kind", "identifiers"], "rowid")
    ]

serializedRows :: Connection -> Text -> [Text] -> Text -> IO [Text]
serializedRows connection table columns ordering = do
  rows <- query connection statement ()
  pure [serializedValue | Only serializedValue <- rows]
  where
    serialized = Text.intercalate "||char(31)||" ["quote(" <> column <> ")" | column <- columns]
    statement = dynamicQuery ("SELECT " <> serialized <> " FROM " <> table <> " ORDER BY " <> ordering)

sortPassages :: [SearchPassage] -> [SearchPassage]
sortPassages = quickSortOn searchPassageId

quickSortOn :: (Ord key) => (value -> key) -> [value] -> [value]
quickSortOn _ [] = []
quickSortOn key (value : values) = quickSortOn key [other | other <- values, key other <= key value] <> [value] <> quickSortOn key [other | other <- values, key other > key value]

dynamicQuery :: Text -> Query
dynamicQuery = fromString . Text.unpack

withMemory :: (Connection -> IO value) -> IO value
withMemory = bracket (open ":memory:") close

mustRight :: (Show failure) => Either failure value -> value
mustRight (Right value) = value
mustRight (Left failure) = error (show failure)
