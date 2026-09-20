{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Adrai.WebCompilationTest
  ( tests,
    exactRevisionCompilationTest,
    testRealNamespaceIsolation,
    testPhysicalCompilerShutdown,
    testConcurrentCliPublication,
  )
where

import Adrai.CliTypes (DoctorOutput (..))
import Adrai.Compiler (coldCompileRepositoryWithAttribution)
import Adrai.Compiler.Attribution
  ( AttributionDependencies (..),
    closeColdCompileAttribution,
    defaultAttributionDependencies,
    newFileColdCompileAttributionWith,
  )
import Adrai.Compiler.CacheSelection (validateExactCacheTarget)
import Adrai.Git
  ( GitOid,
    Repository,
    RevisionSpec (RevisionSpec),
    discoverRepository,
    gitOidText,
    resolveRevision,
    systemGit,
  )
import qualified Adrai.Service.Compilation as Compilation
import Adrai.Service.PostCommitIndex
  ( PostCommitIndexDependencies (..),
    PostCommitIndexResult (..),
    compilePostCommitIndexWith,
    postCommitIndexDependencies,
  )
import qualified Adrai.Service.Runtime as Runtime
import qualified Adrai.Web.Api as Api
import Adrai.Web.Application (ApplicationServices (..), defaultApplicationServices)
import Adrai.Web.Server (RunningServer, ServerDependencies (..), withWebServer)
import qualified Adrai.WebServerTest as Server
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, threadDelay, tryPutMVar)
import Control.Concurrent.Async (Async, async, cancel, race, waitAnyCatch, waitCatch, withAsync)
import Control.Exception
  ( SomeAsyncException,
    SomeException,
    bracket,
    fromException,
    throwIO,
  )
import Control.Monad (unless, void, when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString8
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>), takeDirectory)
import System.Process
  ( CreateProcess (cwd),
    callProcess,
    proc,
    readCreateProcessWithExitCode,
    readProcess,
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "web exact compilation"
    [ testCase "real repository namespaces compile independently" testRealNamespaceIsolation,
      testCase "server shutdown cancels the physical compiler and cleans its candidate" testPhysicalCompilerShutdown,
      testCase "outstanding HTTP compilation retains its revision across CLI publication" testConcurrentCliPublication
    ]

exactRevisionCompilationTest :: TestTree
exactRevisionCompilationTest = testCase "exact revision compilation is one physical flight" Server.testCompilationRuntime

-- Two real worktree bindings may name the same commit, but their immutable
-- archives and scoped SQLite readers remain binding-local.
testRealNamespaceIsolation :: IO ()
testRealNamespaceIsolation = Server.withSeededRepository $ \root -> do
  let linkedRoot = takeDirectory root </> "second-binding"
  bracket
    (callProcess "git" ["-C", root, "worktree", "add", "--detach", linkedRoot, "HEAD"])
    (const (callProcess "git" ["-C", root, "worktree", "remove", "--force", linkedRoot]))
    (const (exerciseBindings root linkedRoot))

exerciseBindings :: FilePath -> FilePath -> IO ()
exerciseBindings firstRoot secondRoot = do
  firstRepository <- requireRepository firstRoot
  secondRepository <- requireRepository secondRoot
  firstRevision <- requireHead firstRepository
  secondRevision <- requireHead secondRepository
  firstRevision @?= secondRevision
  starts <- newIORef (0 :: Int)
  producerStarted <- newEmptyMVar
  releaseProducer <- newEmptyMVar
  bracket Compilation.newCompilationCoordinator Compilation.stopCompilationCoordinator $ \coordinator -> do
    let produce repository revision = do
          atomicModifyIORef' starts (\count -> (count + 1, ()))
          putMVar producerStarted ()
          takeMVar releaseProducer
          fmap (Compilation.CompiledArtifact revision) <$> Runtime.ensureExactArchive repository revision
        acquire repository = Compilation.acquireExactCompilation coordinator repository firstRevision (produce repository firstRevision)
    withAsync (acquire firstRepository) $ \first ->
      withAsync (acquire secondRepository) $ \second -> do
        awaitSignal "first repository compiler" producerStarted [first, second]
        awaitSignal "second repository compiler" producerStarted [first, second]
        readIORef starts >>= (@?= 2)
        putMVar releaseProducer ()
        putMVar releaseProducer ()
        firstArtifact <- requireWorker "first repository compilation" 25000000 first
        secondArtifact <- requireWorker "second repository compilation" 25000000 second
        firstPath <- requireArtifact firstArtifact
        secondPath <- requireArtifact secondArtifact
        assertBool "repository bindings publish distinct immutable archives" (firstPath /= secondPath)
        firstPath @?= (firstRoot </> ".adrai" </> "cache" </> Text.unpack (gitOidText firstRevision) <> ".sqlite")
        secondPath @?= (secondRoot </> ".adrai" </> "cache" </> Text.unpack (gitOidText secondRevision) <> ".sqlite")
        validateExactCacheTarget firstPath (gitOidText firstRevision) >>= assertBool "first binding archive validates exactly"
        validateExactCacheTarget secondPath (gitOidText secondRevision) >>= assertBool "second binding archive validates exactly"
        firstDoctor <- Runtime.runDoctorFromExactArchive firstRepository firstRevision firstPath >>= requireRight "first binding doctor"
        secondDoctor <- Runtime.runDoctorFromExactArchive secondRepository secondRevision secondPath >>= requireRight "second binding doctor"
        doctorRevision firstDoctor @?= gitOidText firstRevision
        doctorRevision secondDoctor @?= gitOidText secondRevision
        doctorDatabase firstDoctor @?= Just firstPath
        doctorDatabase secondDoctor @?= Just secondPath

-- Cancellation is delivered while the real cold compiler owns its SQLite
-- candidate. The production post-commit lifecycle must close and remove it.
testPhysicalCompilerShutdown :: IO ()
testPhysicalCompilerShutdown = Server.withSeededRepository $ \root -> do
  headBefore <- Server.gitHead root
  enteredRestream <- newEmptyMVar
  releaseRestream <- newEmptyMVar
  connectionOpened <- newEmptyMVar
  connectionClosed <- newEmptyMVar
  candidatesRef <- newIORef []
  clientRef <- newIORef Nothing
  let cacheDirectory = root </> ".adrai" </> "cache"
      archive = cacheDirectory </> Text.unpack headBefore <> ".sqlite"
      attributionPath = root </> "shutdown-attribution.tsv"
      baseAttribution = defaultAttributionDependencies
      writeLine handle line = do
        attributionWriteLine baseAttribution handle line
        when ("\tmanagedsourcerestream\t" `isInfixOf` line) $ do
          void (tryPutMVar enteredRestream ())
          takeMVar releaseRestream
      attributionDependencies = baseAttribution {attributionWriteLine = writeLine}
      compileExact repository revision = do
        createDirectoryIfMissing True cacheDirectory
        bracket
          (newFileColdCompileAttributionWith attributionDependencies attributionPath)
          closeColdCompileAttribution
          (\attribution -> do
              let base = postCommitIndexDependencies
                  postCommitDependencies =
                    base
                      { postCommitOpenTemporary = \directory template -> do
                          created@(path, _) <- postCommitOpenTemporary base directory template
                          atomicModifyIORef' candidatesRef (\paths -> (path : paths, ()))
                          pure created,
                        postCommitOpenDatabase = \path -> do
                          connection <- postCommitOpenDatabase base path
                          void (tryPutMVar connectionOpened ())
                          pure connection,
                        postCommitColdCompile = coldCompileRepositoryWithAttribution attribution,
                        postCommitCloseDatabase = \connection -> do
                          postCommitCloseDatabase base connection
                          void (tryPutMVar connectionClosed ()),
                        postCommitAttribution = attribution
                      }
              result <- compilePostCommitIndexWith postCommitDependencies repository revision archive
              exactArchiveResult revision archive result
          )
      services = defaultApplicationServices {applicationCompileExact = compileExact}
      dependencies = Server.dependencies {serverApplicationServices = services}
      cleanupClient = readIORef clientRef >>= mapM_ (\worker -> cancel worker >> void (waitCatch worker))
  bracket (pure ()) (const cleanupClient) $ \() -> do
    stopped <- timeout 8000000 $ withWebServer dependencies root (Api.WebOptions Nothing False) $ \running _ -> do
      client <- async (Server.getJson running ("/api/v1/doctor?at=" <> headBefore))
      writeIORef clientRef (Just client)
      awaitSignal "managed-source restream" enteredRestream [client]
      awaitSignal "compiler SQLite open" connectionOpened [client]
    case stopped of
      Nothing -> assertFailure "server shutdown exceeded eight seconds with the physical compiler paused"
      Just (Left problem) -> assertFailure ("server shutdown failed: " <> Text.unpack problem)
      Just (Right ()) -> pure ()
    awaitMVar "compiler SQLite close" 5000000 connectionClosed
    readIORef clientRef >>= \case
      Nothing -> assertFailure "the outstanding compilation request was not recorded"
      Just client -> requireReleasedWorker "outstanding compilation request" 5000000 client
    candidates <- readIORef candidatesRef
    assertBool "the compiler created a private candidate" (not (null candidates))
    mapM_ assertOwnedSqliteAbsent candidates
    Server.gitHead root >>= (@?= headBefore)
    repository <- requireRepository root
    revision <- requireHead repository
    Runtime.ensureExactArchive repository revision >>= requireRight "subsequent exact compilation"
      >>= \published -> validateExactCacheTarget published (gitOidText revision) >>= assertBool "subsequent exact compilation validates"

-- A cross-process CLI cannot join the server's in-memory flight. Both paths
-- nevertheless converge through the immutable exact archive contract.
testConcurrentCliPublication :: IO ()
testConcurrentCliPublication = Server.withSeededRepository $ \root -> do
  executable <- requireAdraiExecutable
  repository <- requireRepository root
  revisionA <- requireHead repository
  archiveA <- Runtime.ensureExactArchive repository revisionA >>= requireRight "initial exact archive"
  validateExactCacheTarget archiveA (gitOidText revisionA) >>= assertBool "initial exact archive validates"
  let tracked = root </> "seed.txt"
      untracked = root </> "caller-untracked.bin"
  appendFile tracked "caller staged bytes\n"
  callProcess "git" ["-C", root, "add", "--", "seed.txt"]
  appendFile tracked "caller unstaged bytes\n"
  ByteString.writeFile untracked (ByteString.pack [0, 255, 13, 10, 17, 99])
  callerBefore <- captureCallerState root tracked untracked
  pauseOnce <- newIORef True
  archiveReady <- newEmptyMVar
  releaseHttp <- newEmptyMVar
  let compileExact repositoryToCompile revision = do
        archive <- Runtime.ensureExactArchive repositoryToCompile revision >>= requireRight "HTTP exact archive"
        accepted <- validateExactCacheTarget archive (gitOidText revision)
        unless accepted (assertFailure "HTTP producer returned an invalid exact archive")
        shouldPause <- atomicModifyIORef' pauseOnce (\armed -> (False, armed))
        when shouldPause $ putMVar archiveReady () >> takeMVar releaseHttp
        pure (Right archive)
      services = defaultApplicationServices {applicationCompileExact = compileExact}
      dependencies = Server.dependencies {serverApplicationServices = services}
  started <- withWebServer dependencies root (Api.WebOptions Nothing False) $ \running _ ->
    withAsync (Server.getJson running ("/api/v1/doctor?at=" <> gitOidText revisionA)) $ \outstanding -> do
      awaitSignal "validated HTTP archive A" archiveReady [outstanding]
      cliDoctorA <- runAdraiJson executable root ["doctor", "--at", Text.unpack (gitOidText revisionA), "--json"]
      Server.textAt ["revision"] cliDoctorA >>= (@?= gitOidText revisionA)
      created <-
        runAdraiJson executable root
          [ "create",
            "--title", "Concurrent CLI publication",
            "--summary", "Publish revision B while HTTP retains revision A.",
            "--body", "## Decision\nKeep exact web compilation responses revision-bound.\n",
            "--actor", "llm:web-compilation-test",
            "--model", "fixture-model",
            "--domain", "runtime.web",
            "--applies-to", "seed.txt",
            "--json"
          ]
      revisionB <- Server.textAt ["commit"] created
      assertBool "the CLI mutation advances to revision B" (revisionB /= gitOidText revisionA)
      Server.gitHead root >>= (@?= revisionB)
      putMVar releaseHttp ()
      retained <- requireWorker "outstanding HTTP revision A" 15000000 outstanding
      Server.textAt ["metadata", "as_of", "oid"] retained >>= (@?= gitOidText revisionA)
      Server.textAt ["data", "revision"] retained >>= (@?= gitOidText revisionA)
      fresh <- getDoctorAfterCliPublication running revisionB
      Server.textAt ["metadata", "as_of", "oid"] fresh >>= (@?= revisionB)
      Server.textAt ["data", "revision"] fresh >>= (@?= revisionB)
      cliDoctorB <- runAdraiJson executable root ["doctor", "--at", Text.unpack revisionB, "--json"]
      Server.textAt ["revision"] cliDoctorB >>= (@?= revisionB)
      let archiveB = root </> ".adrai" </> "cache" </> Text.unpack revisionB <> ".sqlite"
      validateExactCacheTarget archiveA (gitOidText revisionA) >>= assertBool "archive A remains valid after CLI publication"
      validateExactCacheTarget archiveB revisionB >>= assertBool "archive B validates after HTTP and CLI convergence"
  either (assertFailure . Text.unpack) pure started
  callerAfter <- captureCallerState root tracked untracked
  callerAfter @?= callerBefore

-- The CLI and the watcher's verification worker may briefly own the shared
-- repository lock immediately after publication. Retry only the safe read,
-- and only when the HTTP response identifies that specific busy condition.
getDoctorAfterCliPublication :: RunningServer -> Text -> IO Aeson.Value
getDoctorAfterCliPublication running revision = do
  observed <- timeout 5000000 retryBusyRead
  maybe (assertFailure "fresh revision-B doctor remained repository-busy for five seconds") pure observed
  where
    path = "/api/v1/doctor?at=" <> revision
    retryBusyRead = do
      (status, value) <- Server.getJsonStatus running path
      case status of
        200 -> pure value
        503 -> do
          code <- Server.textAt ["error", "code"] value
          reason <- Server.textAt ["metadata", "as_of", "reason"] value
          if code == "repository-busy" && reason == "request-failed"
            then threadDelay 20000 >> retryBusyRead
            else assertFailure ("GET " <> Text.unpack path <> " returned 503: " <> show value)
        _ -> assertFailure ("GET " <> Text.unpack path <> " returned " <> show status <> ": " <> show value)

type CallerState =
  ( ByteString.ByteString,
    ByteString.ByteString,
    ByteString.ByteString,
    Text
  )

captureCallerState :: FilePath -> FilePath -> FilePath -> IO CallerState
captureCallerState root tracked untracked = do
  stage <- ByteString8.pack <$> readProcess "git" ["-C", root, "ls-files", "--stage", "--", "seed.txt"] ""
  trackedBytes <- ByteString.readFile tracked
  untrackedBytes <- ByteString.readFile untracked
  status <- Text.pack <$> readProcess "git" ["-C", root, "status", "--porcelain=v1", "--", "seed.txt", "caller-untracked.bin"] ""
  pure (stage, trackedBytes, untrackedBytes, status)

requireRepository :: FilePath -> IO Repository
requireRepository root = discoverRepository systemGit root >>= requireRight "repository discovery"

requireHead :: Repository -> IO GitOid
requireHead repository = resolveRevision repository (RevisionSpec "HEAD") >>= requireRight "HEAD resolution"

requireArtifact :: Either Text Compilation.CompiledArtifact -> IO FilePath
requireArtifact = fmap Compilation.compiledArtifactDatabase . requireRight "exact compilation"

requireRight :: Show problem => String -> Either problem value -> IO value
requireRight label = either (assertFailure . ((label <> " failed: ") <>) . show) pure

exactArchiveResult :: GitOid -> FilePath -> PostCommitIndexResult -> IO (Either Text FilePath)
exactArchiveResult revision archive result =
  case (postCommitIndexed result, postCommitDatabase result, postCommitIndexRevision result, postCommitIndexError result) of
    (True, Just published, Just actual, Nothing)
      | published == archive && actual == revision -> do
          accepted <- validateExactCacheTarget archive (gitOidText revision)
          pure (if accepted then Right archive else Left "physical compiler archive failed validation")
    _ -> pure (Left ("physical compiler failed: " <> Text.pack (show result)))

assertOwnedSqliteAbsent :: FilePath -> IO ()
assertOwnedSqliteAbsent candidate =
  mapM_
    (\path -> doesFileExist path >>= assertBool ("compiler-owned path survived cancellation: " <> path) . not)
    [candidate, candidate <> "-journal", candidate <> "-shm", candidate <> "-wal"]

awaitSignal :: String -> MVar () -> [Async value] -> IO ()
awaitSignal label signal workers = do
  observed <- timeout 5000000 (race (takeMVar signal) (waitAnyCatch workers))
  case observed of
    Just (Left ()) -> pure ()
    Just (Right (_, Left exception)) -> rethrowAsyncOrFail (label <> " worker failed before the signal") exception
    Just (Right (_, Right _)) -> assertFailure (label <> " worker completed before the signal")
    Nothing -> assertFailure (label <> " signal timed out after five seconds")

awaitMVar :: String -> Int -> MVar () -> IO ()
awaitMVar label microseconds signal =
  timeout microseconds (takeMVar signal) >>= \case
    Just () -> pure ()
    Nothing -> assertFailure (label <> " timed out")

requireWorker :: String -> Int -> Async value -> IO value
requireWorker label microseconds worker =
  timeout microseconds (waitCatch worker) >>= \case
    Nothing -> assertFailure (label <> " timed out")
    Just (Left exception) -> rethrowAsyncOrFail (label <> " failed") exception
    Just (Right value) -> pure value

requireReleasedWorker :: String -> Int -> Async value -> IO ()
requireReleasedWorker label microseconds worker =
  timeout microseconds (waitCatch worker) >>= \case
    Nothing -> assertFailure (label <> " remained blocked")
    Just (Left exception) ->
      case fromException exception of
        Just cancellation -> throwIO (cancellation :: SomeAsyncException)
        Nothing -> pure ()
    Just (Right _) -> pure ()

rethrowAsyncOrFail :: String -> SomeException -> IO value
rethrowAsyncOrFail label exception =
  case fromException exception of
    Just cancellation -> throwIO (cancellation :: SomeAsyncException)
    Nothing -> assertFailure (label <> ": " <> show exception)

requireAdraiExecutable :: IO FilePath
requireAdraiExecutable =
  lookupEnv "ADRAI_EXE" >>= maybe (assertFailure "ADRAI_EXE is required for concurrent CLI publication") pure

runAdraiJson :: FilePath -> FilePath -> [String] -> IO Aeson.Value
runAdraiJson executable root arguments = do
  (exitCode, stdoutText, stderrText) <- readCreateProcessWithExitCode ((proc executable arguments) {cwd = Just root}) ""
  unless (exitCode == ExitSuccess) $
    assertFailure ("ADRAI command failed: " <> unwords arguments <> "\n" <> take 2000 stderrText)
  case Aeson.eitherDecodeStrict' (ByteString8.pack stdoutText) of
    Left problem -> assertFailure ("ADRAI command returned invalid JSON: " <> problem)
    Right value -> pure value
