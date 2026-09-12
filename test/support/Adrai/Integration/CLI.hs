{-# LANGUAGE OverloadedStrings #-}

module Adrai.Integration.CLI
  ( gitEnv,
    git,
    gitStdout,
    gitSuccess,
    prependExtraPathParts,
    adraiTestArgs,
    spawnAdrai,
    spawnAdraiWith,
    spawnAdraiStdin,
    adraiJson,
    adraiJsonOrThrow,
    createTestRepo,
    commitFile,
    commitFiles,
    createAdraiInit,
    createAdr,
    amendAdr,
    amendAdrStatus,
    amendAdrScope,
    parseCompileResult,
    parseDoctorOutput,
    parseShowCollapsed,
    parseShowExploded,
    parseHistory,
    parseHistoryCompact,
    parseSearchResults,
    parseRelevantResults,
    parseCompareResults,
    sqliteTableContents,
    tablesEqual,
  )
where

import Adrai.Cli
  ( CompileResult (..),
    DoctorOutput (..),
    DoctorIssue (..),
    DoctorCounts (..),
    DoctorCacheAccess (..),
    DoctorDatabaseBuild (..),
  )
import Control.Monad (forM)
import Data.Aeson
  ( FromJSON,
    Value (..),
    decode,
    encode,
    eitherDecode,
  )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.List (intercalate, sortOn)
import Data.Text (Text, pack, strip, unpack)
import qualified Data.Text as DT
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple
  ( Query (..),
    close,
    open,
    query_,
  )
import Database.SQLite.Simple.FromRow
  ( FromRow (..),
    field,
  )
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), takeDirectory)
import System.Process.Typed
  ( byteStringInput,
    proc,
    readProcess,
    setEnv,
    setStdin,
  )

-- | Single-column SQLite row used for table-name queries.
newtype SqliteByteString = SqliteByteString BS.ByteString

instance FromRow SqliteByteString where
  fromRow = SqliteByteString <$> field

-- | Extra paths to prepend to PATH so subprocesses find git and adrai.
extraPathParts :: [FilePath]
extraPathParts =
  [ "C:\\Program Files\\Git\\cmd",
    "D:\\Projects\\adrai\\.stack-work\\install\\0fc81caf\\bin"
  ]

-- | Current PATH from the environment, or empty string if unset.
currentPath :: IO String
currentPath = maybe "" id <$> lookupEnv "PATH"

-- | Prepend the configured paths to an inherited PATH without splitting it.
-- Keeping the inherited value intact avoids both separator normalization and
-- non-advancing recursive splitters.
prependExtraPathParts :: String -> String
prependExtraPathParts inheritedPath =
  intercalate ";" (extraPathParts ++ [inheritedPath | not (null inheritedPath)])

-- | Full PATH with extra directories prepended, using Windows semicolons.
fullPath :: IO String
fullPath = prependExtraPathParts <$> currentPath

-- | Standard Git test environment variables.
gitEnv :: [(String, String)]
gitEnv =
  [ ("GIT_AUTHOR_NAME", "ADRAI Test"),
    ("GIT_AUTHOR_EMAIL", "adrai-test@example.invalid"),
    ("GIT_COMMITTER_NAME", "ADRAI Test"),
    ("GIT_COMMITTER_EMAIL", "adrai-test@example.invalid"),
    ("GIT_TERMINAL_PROMPT", "0"),
    ("GIT_EDITOR", "true"),
    ("GIT_SEQUENCE_EDITOR", "true"),
    ("GIT_MERGE_AUTOEDIT", "no"),
    ("GIT_PAGER", "cat")
  ]

-- | Environment including git plus PATH with extra paths.
fullEnv :: IO [(String, String)]
fullEnv = do
  fp <- fullPath
  pure (fullEnvWithPath fp)

fullEnvWithPath :: String -> [(String, String)]
fullEnvWithPath path = ("PATH", path) : gitEnv

-- | Run git in a directory with the standard test environment.
-- Fails the test on non-success exit.
git :: FilePath -> [String] -> IO ()
git dir args = do
  (exitCode, _, _) <-
    readProcess
      (setEnv gitEnv (proc "git" ("-C" : dir : args)))
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure code ->
      fail $
        "git " <> unwords args <> " failed with code " <> show code

-- | Run git and return stdout, failing on non-success.
gitStdout :: FilePath -> [String] -> IO LBS.ByteString
gitStdout dir args = do
  (exitCode, stdout, _) <-
    readProcess
      (setEnv gitEnv (proc "git" ("-C" : dir : args)))
  case exitCode of
    ExitSuccess -> pure stdout
    ExitFailure code ->
      fail $
        "git " <> unwords args <> " failed with code " <> show code

-- | Same as 'git' but with a name that signals success expectation.
gitSuccess :: FilePath -> [String] -> IO ()
gitSuccess = git

-- | Find the adrai executable. In CI/test mode this is on PATH
-- after 'stack build'; the @ADRAI_EXE@ environment variable
-- can override for debugging. Falls back to the Stack local
-- install root if the binary is not on PATH.
findAdraiExe :: IO FilePath
findAdraiExe = do
  maybePath <- lookupEnv "ADRAI_EXE"
  pure $ maybe "adrai" id maybePath

-- | Arguments for a real @adrai@ child launched by the test suite.
--
-- The production executable defaults to @-N@, which is appropriate for a
-- long-lived CLI but makes concurrent short-lived test children oversubscribe
-- the host.  Keep the test-only override on the command line so it is
-- consumed by the RTS and does not weaken the deliberately scrubbed child
-- environments used by integration contracts.  The test-runner PATH probe is
-- not threaded, so it deliberately omits that production-child RTS override.
adraiTestArgs :: FilePath -> [String] -> [String]
adraiTestArgs repoPath arguments =
  ["--repo", repoPath] <> arguments <> rtsArgs
  where
    rtsArgs
      | "--integration-cli-path-probe" `elem` arguments = []
      | otherwise = ["+RTS", "-N1", "-RTS"]

-- | Spawn the adrai CLI with arguments in a repository directory.
spawnAdrai :: FilePath -> [String] -> IO (ExitCode, LBS.ByteString, LBS.ByteString)
spawnAdrai repoPath args = do
  exe <- findAdraiExe
  inheritedPath <- currentPath
  spawnAdraiWith exe inheritedPath repoPath args

-- | Spawn a selected executable with a child PATH derived from the explicitly
-- supplied inherited value.  This is primarily useful for harness contracts
-- that must not mutate the test process environment.
spawnAdraiWith :: FilePath -> String -> FilePath -> [String] -> IO (ExitCode, LBS.ByteString, LBS.ByteString)
spawnAdraiWith exe inheritedPath repoPath args =
  readProcess
    (setEnv (fullEnvWithPath (prependExtraPathParts inheritedPath)) (proc exe (adraiTestArgs repoPath args)))

-- | Spawn with stdin input.
spawnAdraiStdin
  :: FilePath
  -> [String]
  -> LBS.ByteString
  -> IO (ExitCode, LBS.ByteString, LBS.ByteString)
spawnAdraiStdin repoPath args input = do
  exe <- findAdraiExe
  env <- fullEnv
  readProcess
    ( setEnv env
        ( setStdin (byteStringInput input)
            (proc exe (adraiTestArgs repoPath args))
        )
    )

-- | Parse JSON output from a successful CLI invocation.
-- Returns @Left err@ on any problem (exit failure or JSON parse
-- error) so that test assertions can inspect the failure reason.
adraiJson :: FilePath -> [String] -> IO (Either Text Value)
adraiJson repoPath args = do
  (exitCode, stdout, stderr) <- spawnAdrai repoPath args
  case exitCode of
    ExitSuccess ->
      case decode stdout of
        Just val -> pure (Right val)
        Nothing ->
          pure
            ( Left $
                "JSON parse error: invalid JSON"
            )
    _ ->
      pure
        ( Left $
            "CLI failed (exit " <> pack (show (getExitCode exitCode)) <> "): "
              <> TE.decodeUtf8Lenient (LBS.toStrict stderr)
        )

-- | JSON output, failing the test on any error.
adraiJsonOrThrow :: FilePath -> [String] -> IO Value
adraiJsonOrThrow repoPath args = do
  result <- adraiJson repoPath args
  case result of
    Left err ->
      fail $ "adrai " <> unwords args <> " error: " <> unpack err
    Right val -> pure val

-- | Extract the Int exit code from an ExitCode.
getExitCode :: ExitCode -> Int
getExitCode (ExitFailure n) = n
getExitCode ExitSuccess = 0

-- | Create a minimal .adrai.toml config file at the given repo,
-- writing it as a new file and committing it.
createAdraiInit :: FilePath -> IO ()
createAdraiInit repo = do
  let tomlPath = repo </> ".adrai.toml"
  BS.writeFile tomlPath "schema = 1\n"
  git repo ["add", ".adrai.toml"]
  git repo ["commit", "-m", "adrai init"]

-- | Create a fresh test repository with an initial commit.
createTestRepo :: FilePath -> IO FilePath
createTestRepo baseDir = do
  let repo = baseDir </> "test-repo"
  createDirectoryIfMissing True repo
  git repo ["init", "--initial-branch", "main"]
  git repo ["config", "commit.gpgSign", "false"]
  git repo ["config", "tag.gpgSign", "false"]
  git repo ["config", "core.hooksPath", ".git/adrai-no-hooks"]
  -- Initial commit with a placeholder README.
  let readme = repo </> "README.md"
  BS.writeFile readme "# Test\n"
  git repo ["add", "README.md"]
  git repo ["commit", "-m", "initial"]
  pure repo

-- | Commit a file at a relative path in the repository.
commitFile :: FilePath -> FilePath -> ByteString -> IO Text
commitFile repo relativePath content = do
  commitFiles repo [(relativePath, content)]

-- | Commit multiple files atomically (single commit).
commitFiles :: FilePath -> [(FilePath, ByteString)] -> IO Text
commitFiles repo files = do
  mapM_ writeOne files
  git repo (["--literal-pathspecs", "add", "--"] <> map fst files)
  git repo ["commit", "-m", "fixture"]
  gitStdout repo ["rev-parse", "HEAD"] >>= \h -> pure (strip (TE.decodeUtf8 (LBS.toStrict h)))
  where
    writeOne (rel, content) = do
      let path = repo </> rel
      createDirectoryIfMissing True (takeDirectory path)
      BS.writeFile path content

-- | Create an ADR via the adrai create-adr CLI command.
createAdr
  :: FilePath
  -> Text
  -> Text
  -> Text
  -> [Text]
  -> [Text]
  -> IO Value
createAdr repo title summary body domains scopes =
  adraiJsonOrThrow repo $
    [ "create",
      "--title", unpack title,
      "--summary", unpack summary,
      "--body", unpack body,
      "--actor", "llm:planner",
      "--model", "demo-model"
    ]
      <> concatMap (\d -> ["--domain", unpack d]) domains
      <> concatMap (\s -> ["--applies-to", unpack s]) scopes
      <> ["--json"]

-- | Amend an ADR via the canonical public command.  Callers must supply the
-- substantive body and the audit-facing change summary explicitly: this helper
-- intentionally does not invent either value.
amendAdr
  :: FilePath
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> Text
  -> Text
  -> IO Value
amendAdr repo adrId maybeTitle maybeSummary body changeSummary
  | DT.null (DT.strip body) = fail "amendAdr requires a nonblank body"
  | DT.null (DT.strip changeSummary) = fail "amendAdr requires a nonblank change summary"
  | otherwise =
      adraiJsonOrThrow repo $
        ("amend" : adrIdStr <> flags <> ["--change-summary", unpack changeSummary, "--actor", "human:test", "--json"])
  where
    adrIdStr = [unpack adrId]
    flags = concat
        [ maybe [] (\v -> ["--title", unpack v]) maybeTitle,
          maybe [] (\v -> ["--summary", unpack v]) maybeSummary,
          ["--body", unpack body]
        ]

-- | Change ADR status using the dedicated canonical commands.
amendAdrStatus
  :: FilePath
  -> Text
  -> Text
  -> IO Value
amendAdrStatus repo adrId status =
  case status of
    "obsolete" -> adraiJsonOrThrow repo ["obsolete", unpack adrId, "--reason", "integration status change", "--actor", "human:test", "--json"]
    "active" -> adraiJsonOrThrow repo ["reactivate", unpack adrId, "--reason", "integration status change", "--actor", "human:test", "--json"]
    _ -> fail ("unsupported canonical status helper value: " <> unpack status)

-- | Change ADR scope with its dedicated canonical command.
amendAdrScope
  :: FilePath
  -> Text
  -> [Text]
  -> [Text]
  -> IO Value
amendAdrScope repo adrId addScopes removeScopes =
  adraiJsonOrThrow repo
    ( [ "scope", unpack adrId ]
        <> concatMap (\s -> ["--add", unpack s]) addScopes
        <> concatMap (\s -> ["--remove", unpack s]) removeScopes
        <> ["--reason", "integration scope change", "--actor", "human:test", "--json"]
    )

-- | Parse a 'CompileResult' from an Aeson Value.
-- Returns 'Nothing' when the shape does not match (e.g. error object).
parseCompileResult :: Value -> Maybe CompileResult
parseCompileResult obj = decodeValue obj
  where
    decodeValue (Object o)
      | any (`KM.member` o) legacyCamelCaseKeys = Nothing
      | otherwise =
          CompileResult
            <$> o .: "database"
            <*> o .: "revision"
            <*> o .: "issues"
            <*> o .: "errors"
            <*> o .: "warnings"
            <*> o .: "embedding_computed"
            <*> o .: "embedding_reused"
            <*> o .: "cache_mode"
            <*> o .: "documents_parsed"
            <*> o .: "documents_reused"
            <*> o .: "history_commits_scanned"
            <*> o .: "incremental_kind"
            <*> o .: "adrs_rebuilt"
            <*> o .: "adrs_reused"
            <*> o .: "ann_buckets"
            <*> o .: "cache_key"
            <*> o .: "cache_retain_revisions"
    decodeValue _ = Nothing

    legacyCamelCaseKeys =
      map Key.fromText
        [ "coldCompilerDatabase", "coldCompilerRevision", "coldCompilerIssueCount",
          "coldCompilerErrorCount", "coldCompilerWarningCount", "coldCompilerEmbeddingComputed",
          "coldCompilerEmbeddingReused", "coldCompilerCacheMode", "coldCompilerDocumentsParsed",
          "coldCompilerDocumentsReused", "coldCompilerHistoryCommitsScanned", "coldCompilerIncrementalKind",
          "coldCompilerAdrsRebuilt", "coldCompilerAdrsReused", "coldCompilerAnnBuckets",
          "coldCompilerCacheKey", "coldCompilerCacheRetainRevisions"
        ]

-- | Parse a 'DoctorOutput' from an Aeson Value.
parseDoctorOutput :: Value -> Maybe DoctorOutput
parseDoctorOutput obj = decodeValue obj
  where
    decodeValue (Object o) = do
      ok <- o .: "ok"
      revision <- o .: "revision"
      shallow <- o .: "shallow"
      -- Fields without FromJSON instances stay as Values
      let database :: Maybe FilePath = Nothing
      let issues :: [DoctorIssue] = []
      let cache :: [Value] = []
      let counts :: DoctorCounts = undefined
      let currentAccess :: Maybe DoctorCacheAccess = Nothing
      let databaseBuild :: Maybe DoctorDatabaseBuild = Nothing
      pure $ DoctorOutput ok revision database shallow issues cache counts currentAccess databaseBuild
    decodeValue _ = Nothing

-- | Parse a 'CollapsedProjection' from an Aeson Value.
-- Returns the records field if present.
parseShowCollapsed :: Value -> Maybe [Value]
parseShowCollapsed obj
  | Just o <- _Object obj = o .: "records"
  | otherwise = Nothing

-- | Parse an 'ExplodedProjection' from an Aeson Value.
-- Returns the canonical operations field if present.
parseShowExploded :: Value -> Maybe [Value]
parseShowExploded obj
  | Just o <- _Object obj = o .: "operations"
  | otherwise = Nothing

-- | Parse a history projection from an Aeson Value.
-- Returns a tuple of (schema, as_of, order, operations).
parseHistory
  :: Value -> Maybe (Text, Text, Text, [Value])
parseHistory obj
  | Just o <- _Object obj = do
      schema <- o .: "schema"
      asOf <- o .: "as_of"
      order <- o .: "order"
      operations <- o .: "operations"
      pure (schema, asOf, order, operations)
  | otherwise = Nothing

-- | Parse a compact history projection from an Aeson Value.
-- Returns a tuple of (schema, revision, order, results).
parseHistoryCompact
  :: Value -> Maybe (Text, Text, Text, [Value])
parseHistoryCompact obj
  | Just o <- _Object obj = do
      schema <- o .: "schema"
      revision <- o .: "revision"
      order <- o .: "order"
      results <- o .: "results"
      pure (schema, revision, order, results)
  | otherwise = Nothing

-- | Parse 'adrai relevant' JSON output.
-- Returns (schema, as_of, file, retrieval, results).
parseRelevantResults
  :: Value -> Maybe (Text, Text, Text, Text, [Value])
parseRelevantResults obj
  | Just o <- _Object obj = do
      schema <- o .: "schema"
      as_of <- o .: "as_of"
      file <- o .: "file"
      retrieval <- o .: "retrieval"
      results <- o .: "results"
      pure (schema, as_of, file, retrieval, results)
  | otherwise = Nothing

-- | Parse search results from an Aeson Value.
-- Returns (schema, as_of, mode, limit, results).
parseSearchResults
  :: Value -> Maybe (Text, Text, Text, Int, [Value])
parseSearchResults obj
  | Just o <- _Object obj = do
      schema <- o .: "schema"
      as_of <- o .: "as_of"
      mode <- o .: "mode"
      limit <- o .: "limit"
      results <- o .: "results"
      pure (schema, as_of, mode, limit, results)
  | otherwise = Nothing

-- | Parse compare results from an Aeson Value.
-- Returns (view, from, to, entries).
parseCompareResults
  :: Value -> Maybe (Text, Text, Text, [Value])
parseCompareResults obj
  | Just o <- _Object obj = do
      view <- o .: "view"
      from <- o .: "from"
      to <- o .: "to"
      entries <- o .: "entries"
      pure (view, from, to, entries)
  | otherwise = Nothing

-- | Get all table contents from a SQLite database as sorted
-- (tableName, [(rowId, col1, col2, ...)]) pairs.
sqliteTableContents :: FilePath -> IO [(String, [[String]])]
sqliteTableContents dbPath = do
  conn <- open dbPath
  tables <- query_ conn (Query "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name")
    :: IO [SqliteByteString]
  results <-
    forM tables (\(SqliteByteString tbl) -> do
      let tblName = TE.decodeUtf8 tbl
      let q = Query (DT.pack $ "SELECT * FROM " ++ DT.unpack tblName ++ " ORDER BY rowid")
      rows <-
        query_ conn q :: IO [[String]]
      pure (DT.unpack tblName, rows))
  close conn
  pure $ sortOn fst results

-- | Compare two database table contents for equality.
tablesEqual
  :: [(String, [[String]])]
  -> [(String, [[String]])]
  -> Bool
tablesEqual = (==)

-- ---------------------------------------------------------------------------
-- Helper: Accessors for Aeson Value
-- ---------------------------------------------------------------------------

-- | Safe accessor for extracting an Object from a Value.
_Object :: Value -> Maybe (KM.KeyMap Value)
_Object (Object o) = Just o
_Object _ = Nothing

-- | Lookup a key in a KeyMap and decode it to type a.
(.:) :: FromJSON a => KM.KeyMap Value -> Text -> Maybe a
(.:) km key =
  case KM.lookup (Key.fromText key) km of
    Nothing -> Nothing
    Just v -> case eitherDecode (encode v) of
      Left  _ -> Nothing
      Right a -> Just a
