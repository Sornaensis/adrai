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
import Adrai.Provenance.Git.Lock (gitLockStatus)
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
import Control.Concurrent (MVar, newEmptyMVar, putMVar, readMVar, takeMVar, threadDelay, tryPutMVar, tryReadMVar)
import Control.Concurrent.Async (Async, async, cancel, poll, race, waitAnyCatch, waitCatch, withAsync)
import Control.Exception
  ( SomeAsyncException,
    SomeException,
    bracket,
    fromException,
    onException,
    throwIO,
  )
import Control.Monad (unless, void, when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString8
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
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
  pausedCompiler <- newEmptyMVar
  candidatesRef <- newIORef []
  clientRef <- newIORef Nothing
  stagesRef <- newIORef ([] :: [(String, Word64)])
  startedAt <- getMonotonicTimeNSec
  let cacheDirectory = root </> ".adrai" </> "cache"
      archive = cacheDirectory </> Text.unpack headBefore <> ".sqlite"
      attributionPath = root </> "shutdown-attribution.tsv"
      baseAttribution = defaultAttributionDependencies
      recordStage stage = do
        now <- getMonotonicTimeNSec
        atomicModifyIORef' stagesRef (\stages -> ((stage, now) : stages, ()))
      stageTrace = do
        stages <- reverse <$> readIORef stagesRef
        pure [(stage, (at - startedAt) `div` 1000000) | (stage, at) <- stages]
      writeLine handle line = do
        attributionWriteLine baseAttribution handle line
        when ("\tmanagedsourcerestream\t" `isInfixOf` line) $ do
          recordStage "managed-source restream entered"
          void (tryPutMVar enteredRestream ())
          takeMVar releaseRestream
      attributionDependencies = baseAttribution {attributionWriteLine = writeLine}
      compileExact repository revision = do
        recordStage "physical compiler started"
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
                          recordStage "candidate created"
                          atomicModifyIORef' candidatesRef (\paths -> (path : paths, ()))
                          pure created,
                        postCommitOpenDatabase = \path -> do
                          connection <- postCommitOpenDatabase base path
                          recordStage "compiler SQLite opened"
                          void (tryPutMVar connectionOpened ())
                          pure connection,
                        postCommitColdCompile = coldCompileRepositoryWithAttribution attribution,
                        postCommitCloseDatabase = \connection -> do
                          postCommitCloseDatabase base connection
                          recordStage "compiler SQLite closed"
                          void (tryPutMVar connectionClosed ()),
                        postCommitAttribution = attribution
                      }
              result <- compilePostCommitIndexWith postCommitDependencies repository revision archive
              exactArchiveResult revision archive result
          )
      services = defaultApplicationServices {applicationCompileExact = compileExact}
      dependencies = Server.dependencies {serverApplicationServices = services, serverReady = const (recordStage "server ready"), serverStopping = recordStage "server stopping"}
      cleanupClient = readIORef clientRef >>= mapM_ (\worker -> cancel worker >> void (waitCatch worker))
  bracket (pure ()) (const cleanupClient) $ \() -> do
    recordStage "server starting"
    withAsync
      (do
          result <- withWebServer dependencies root (Api.WebOptions Nothing False) $ \running _ -> do
            recordStage "HTTP request starting"
            client <- async (Server.getJson running ("/api/v1/doctor?at=" <> headBefore))
            writeIORef clientRef (Just client)
            awaitSignalFor 20000000 "managed-source restream" enteredRestream [client]
            recordStage "managed-source restream observed"
            awaitSignalFor 20000000 "compiler SQLite open" connectionOpened [client]
            recordStage "compiler SQLite open observed"
            recordStage "physical compiler paused"
            void (tryPutMVar pausedCompiler ())
          recordStage "server returned"
          pure result
      )
      $ \server -> do
        admission <- timeout 45000000 (race (readMVar pausedCompiler) (waitCatch server))
        case admission of
          Just (Left ()) -> pure ()
          Just (Right (Left exception)) -> rethrowAsyncOrFail "server failed before the compiler paused" exception
          Just (Right (Right result)) -> do
            paused <- tryReadMVar pausedCompiler
            case paused of
              Just () -> pure ()
              Nothing -> assertFailure ("server returned before the compiler paused: " <> show result)
          Nothing -> stageTrace >>= \trace -> assertFailure ("server did not reach the paused compiler within forty-five seconds: stages (ms) " <> show trace)
        stopped <- timeout 8000000 (waitCatch server)
        trace <- stageTrace
        case stopped of
          Nothing -> do
            candidates <- readIORef candidatesRef
            candidateExists <- mapM doesFileExist candidates
            client <- readIORef clientRef >>= mapM poll
            assertFailure ("server shutdown exceeded eight seconds after the physical compiler paused: stages (ms) " <> show trace <> ", candidate exists " <> show candidateExists <> ", request worker " <> show (fmap (fmap (either show (const "completed"))) client))
          Just (Left exception) -> rethrowAsyncOrFail ("server shutdown failed after the physical compiler paused; stages (ms) " <> show trace) exception
          Just (Right (Left problem)) -> assertFailure ("server shutdown failed: " <> Text.unpack problem <> ", stages (ms) " <> show trace)
          Just (Right (Right ())) -> do
            stages <- readIORef stagesRef
            case (lookup "physical compiler paused" stages, lookup "server stopping" stages, lookup "server returned" stages) of
              (Just pausedAt, Just stoppingAt, Just returnedAt) ->
                assertBool
                  ("server shutdown exceeded eight seconds after the physical compiler paused: stages (ms) " <> show trace)
                  (pausedAt <= stoppingAt && stoppingAt <= returnedAt && returnedAt - pausedAt <= 8000000000)
              _ -> assertFailure ("server shutdown did not record its complete lifecycle: stages (ms) " <> show trace)
            putStrLn ("physical compiler shutdown stages (ms): " <> show trace)
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
  compilePhase <- newIORef ("HTTP exact producer not started" :: Text)
  phaseEvents <- newIORef ([] :: [(Text, Word64)])
  clientPhase <- newIORef ("HTTP client not launched" :: String)
  archiveReady <- newEmptyMVar
  releaseHttp <- newEmptyMVar
  let recordProducerPhase phase = do
        now <- getMonotonicTimeNSec
        writeIORef compilePhase phase
        atomicModifyIORef' phaseEvents (\events -> ((phase, now) : events, ()))
      compileExact repositoryToCompile revision = do
        recordProducerPhase ("preparing exact archive " <> gitOidText revision)
        archive <- Runtime.ensureExactArchive repositoryToCompile revision >>= requireRight "HTTP exact archive"
        recordProducerPhase ("validating exact archive " <> gitOidText revision)
        accepted <- validateExactCacheTarget archive (gitOidText revision)
        unless accepted (assertFailure "HTTP producer returned an invalid exact archive")
        recordProducerPhase ("validated exact archive " <> gitOidText revision)
        shouldPause <- atomicModifyIORef' pauseOnce (\armed -> (False, armed))
        when shouldPause $ do
          recordProducerPhase ("revision A held after validation " <> gitOidText revision)
          putMVar archiveReady ()
          takeMVar releaseHttp
          recordProducerPhase ("revision A released after validation " <> gitOidText revision)
        pure (Right archive)
      services = defaultApplicationServices {applicationCompileExact = compileExact}
      dependencies = Server.dependencies {serverApplicationServices = services}
  -- The HTTP producer waits through two CLI commands. Bound that hold to 60s;
  -- the 80s socket guard then covers the existing 15s post-release check and
  -- leaves 5s for scheduling and cancellation.
  started <- withWebServer dependencies root (Api.WebOptions Nothing False) $ \running _ ->
    withAsync (Server.getJsonLabeledObserved 80000000 (writeIORef clientPhase) "outstanding revision-A doctor" running ("/api/v1/doctor?at=" <> gitOidText revisionA)) $ \outstanding -> do
      held <- timeout 60000000 $ do
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
        pure revisionB
      revisionB <- case held of
        Just revision -> pure revision
        Nothing -> do
          client <- readIORef clientPhase
          phases <- reverse <$> readIORef phaseEvents
          lock <- gitLockStatus repository
          void (tryPutMVar releaseHttp ())
          assertFailure ("revision-A HTTP hold/CLI phase exceeded 60s; client phase " <> client <> ", producer phases " <> show phases <> ", lock " <> show lock)
      putMVar releaseHttp ()
      retained <- requireWorker "outstanding HTTP revision A" 15000000 outstanding `onException` (do
        phases <- reverse <$> readIORef phaseEvents
        client <- readIORef clientPhase
        lock <- gitLockStatus repository
        putStrLn ("outstanding HTTP revision A failed; client phase " <> client <> ", producer phases " <> show phases <> ", lock " <> show lock))
      Server.textAt ["metadata", "as_of", "oid"] retained >>= (@?= gitOidText revisionA)
      Server.textAt ["data", "revision"] retained >>= (@?= gitOidText revisionA)
      fresh <- getDoctorAfterCliPublication repository compilePhase phaseEvents running revisionB
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

-- The fresh exact archive can take longer than a warm read after publication.
-- The watcher may also briefly own the repository lock. Retry only a typed
-- busy response while keeping one guard around the complete HTTP read.
getDoctorAfterCliPublication :: Repository -> IORef Text -> IORef [(Text, Word64)] -> RunningServer -> Text -> IO Aeson.Value
getDoctorAfterCliPublication repository compilePhase phaseEvents running revision = do
  lastStatus <- newIORef ("HTTP response pending" :: String)
  startedAt <- getMonotonicTimeNSec
  observed <- timeout 60000000 (retryBusyRead lastStatus)
  finishedAt <- getMonotonicTimeNSec
  events <- reverse <$> readIORef phaseEvents
  let phaseTrace =
        [ (Text.unpack phase, fromIntegral (at - startedAt) `div` (1000000 :: Integer))
          | (phase, at) <- events,
            at >= startedAt
        ]
      elapsedMs = (finishedAt - startedAt) `div` 1000000
  case observed of
    Just value -> do
      putStrLn ("revision-B doctor HTTP completed in " <> show elapsedMs <> " ms; producer phases (ms from request): " <> show phaseTrace)
      pure value
    Nothing -> do
      progress <- readIORef lastStatus
      producer <- readIORef compilePhase
      lock <- gitLockStatus repository
      assertFailure ("GET " <> Text.unpack path <> " did not complete within sixty seconds: " <> progress <> ", producer " <> Text.unpack producer <> ", phases " <> show phaseTrace <> ", lock " <> show lock)
  where
    path = "/api/v1/doctor?at=" <> revision
    retryBusyRead lastStatus = do
      (status, value) <- Server.getJsonStatus running path
      case status of
        200 -> pure value
        503 -> do
          code <- Server.textAt ["error", "code"] value
          reason <- Server.textAt ["metadata", "as_of", "reason"] value
          if code == "repository-busy" && reason == "request-failed"
            then writeIORef lastStatus ("typed 503 repository-busy") >> threadDelay 20000 >> retryBusyRead lastStatus
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
awaitSignal = awaitSignalFor 5000000

awaitSignalFor :: Int -> String -> MVar () -> [Async value] -> IO ()
awaitSignalFor microseconds label signal workers = do
  observed <- timeout microseconds (race (takeMVar signal) (waitAnyCatch workers))
  case observed of
    Just (Left ()) -> pure ()
    Just (Right (_, Left exception)) -> rethrowAsyncOrFail (label <> " worker failed before the signal") exception
    Just (Right (_, Right _)) -> assertFailure (label <> " worker completed before the signal")
    Nothing -> assertFailure (label <> " signal timed out after " <> show (microseconds `div` 1000000) <> " seconds")

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
