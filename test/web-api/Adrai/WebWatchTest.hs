{-# LANGUAGE OverloadedStrings #-}
module Adrai.WebWatchTest (tests, headObservationTest, headResponseTest) where

import qualified Adrai.WebServerTest as Server
import qualified Adrai.Web.Api as Api
import qualified Adrai.Web.Watch as Watch
import Adrai.Git
import Adrai.RetainedNative.NativeFixture (constantNativeFixture, tracingNativeFixture)
import Control.Exception (finally)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Encoding
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>), takeDirectory)
import System.Process (callProcess)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests = testCase "fact watcher verifies and retries bounded changes" Server.testWatchRuntime

headObservationTest :: TestTree
headObservationTest = testCase "snapshot HEAD facts use one validated Git invocation" $
  Server.withSeededRepository $ \root -> do
    repository <- checked =<< discoverRepository systemGit root
    let headFile = repositoryGitDir repository </> "HEAD"
        setHead = BS.writeFile headFile
        git = callProcess "git" . (["-C", root] <>)
        check label expectedFresh selectedRoot = checkTracedSnapshot root label expectedFresh selectedRoot
    originalHead <- BS.readFile headFile
    finally (do
      check "attached" True root
      commit <- checked =<< resolveRevision repository (RevisionSpec "HEAD")
      setHead (Encoding.encodeUtf8 (gitOidText commit) <> "\n")
      check "detached" True root
      setHead "ref: refs/heads/watch-alias\n"
      BS.writeFile (repositoryGitDir repository </> "refs" </> "heads" </> "watch-alias") "ref: refs/heads/main\n"
      check "recursive symbolic" True root
      setHead originalHead
      git ["tag", "-a", "watch-tag", "-m", "watch tag"]
      tagged <- checked =<< runRepository repository "fixture tag" ["rev-parse", "--verify", "watch-tag"] BS.empty
      processExitCode tagged @?= ExitSuccess
      setHead (processStdout tagged)
      check "annotated tag" True root
      setHead originalHead
      blob <- checked =<< runRepository repository "fixture blob" ["hash-object", "-w", "seed.txt"] BS.empty
      processExitCode blob @?= ExitSuccess
      setHead (processStdout blob)
      check "noncommit" False root
      setHead "1111111111111111111111111111111111111111\n"
      check "missing object" False root
      setHead originalHead
      let linked = takeDirectory root </> "watch-linked"
      git ["worktree", "add", "-b", "watch-linked", linked]
      check "linked worktree" True linked
      let unborn = takeDirectory root </> "watch-unborn"
      createDirectoryIfMissing True unborn
      callProcess "git" ["-C", unborn, "init", "--initial-branch=main"]
      check "unborn" False unborn
      ) (setHead originalHead)

checkTracedSnapshot :: FilePath -> String -> Bool -> FilePath -> IO ()
checkTracedSnapshot fixtureRoot label expectedFresh root = do
  repository <- checked =<< discoverRepository systemGit root
  bound <- checked (Api.validateRepositoryBinding (Right repository))
  registry <- Watch.newActiveFileRegistry
  referenceObserver <- Watch.observerForRegistry registry bound
  reference <- Watch.repositorySnapshot referenceObserver bound
  let directory = takeDirectory fixtureRoot </> ("head-trace-" <> map replaceSpace label)
      trace = directory </> "argv.txt"
  fixture <- tracingNativeFixture directory trace
  client <- checked (gitClient fixture)
  let tracedBound = bound {Api.repoRepository=repository {repositoryClient=client}}
  observer <- Watch.observerForRegistry registry tracedBound
  writeFile trace ""
  observed <- Watch.repositorySnapshot observer tracedBound
  case observed of
    Watch.RepositorySnapshot _ facts -> do
      assertBool (label <> " must produce fresh facts") expectedFresh
      expectedHead <- checked =<< resolveRevision repository (RevisionSpec "HEAD")
      expectedState <- checked =<< repositoryHeadState repository
      Watch.factsHead facts @?= Just expectedHead
      Watch.factsHeadState facts @?= Just expectedState
      observed @?= reference
    Watch.RepositorySnapshotFailed _ _ ->
      assertBool (label <> " must fail closed") (not expectedFresh)
  arguments <- lines <$> readFile trace
  assertBool (label <> " launches one real Git invocation; observed " <> show arguments) (length arguments == 1)
  where
    replaceSpace ' ' = '-'
    replaceSpace value = value

headResponseTest :: TestTree
headResponseTest = testCase "snapshot HEAD responses reject malformed and failed observations" $
  Server.withSeededRepository $ \root -> do
    repository <- checked =<< discoverRepository systemGit root
    bound <- checked (Api.validateRepositoryBinding (Right repository))
    commit <- checked =<< resolveRevision repository (RevisionSpec "HEAD")
    let oid = Encoding.encodeUtf8 (gitOidText commit)
        valid = oid <> "\nrefs/heads/main\n"
        directory = takeDirectory root </> "head-response-fixture"
        cases :: [(String, BS.ByteString, Int)]
        cases =
          [ ("nonzero", valid, 1), ("missing", "", 0), ("invalid oid", "bad\nHEAD\n", 0),
            ("invalid ref", oid <> "\nmain\n", 0), ("empty ref", oid <> "\n\n", 0),
            ("trailing record", valid <> "extra\n", 0), ("duplicate", valid <> valid, 0),
            ("unterminated", oid <> "\nHEAD", 0), ("embedded NUL", oid <> "\nrefs/heads/a\0b\n", 0),
            ("invalid UTF8", oid <> "\nrefs/heads/" <> BS.pack [255,10], 0),
            ("Unicode digits in oid", BS.concat (replicate 40 (BS.pack [217,160])) <> "\nHEAD\n", 0)
          ]
    fixture <- constantNativeFixture directory valid BS.empty 0
    client <- checked (gitClient fixture)
    registry <- Watch.newActiveFileRegistry
    let fixtureBound = bound {Api.repoRepository=repository {repositoryClient=client}}
    observer <- Watch.observerForRegistry registry fixtureBound
    accepted <- Watch.repositorySnapshot observer fixtureBound
    case accepted of
      Watch.RepositorySnapshot _ facts -> Watch.factsHead facts @?= Just commit
      other -> assertFailure ("valid strict HEAD response rejected: " <> show other)
    forM_ cases $ \(label, output, code) -> do
      BS.writeFile (directory </> "fixture-stdout") output
      writeFile (directory </> "fixture-exit") (show code)
      observed <- Watch.repositorySnapshot observer fixtureBound
      case observed of
        Watch.RepositorySnapshotFailed _ _ -> pure ()
        other -> assertFailure (label <> " must reject fresh observation: " <> show other)
    BS.writeFile (directory </> "fixture-stdout") (oid <> "\r\nHEAD\r\n")
    writeFile (directory </> "fixture-exit") "0"
    recovered <- Watch.repositorySnapshot observer fixtureBound
    case recovered of
      Watch.RepositorySnapshot _ facts -> do
        Watch.factsHead facts @?= Just commit
        Watch.factsHeadState facts @?= Just GitHeadDetached
      other -> assertFailure ("valid CRLF detached response must recover: " <> show other)

checked :: Show failure => Either failure value -> IO value
checked = either (\failure -> assertFailure (show failure) >> fail "unreachable") pure
