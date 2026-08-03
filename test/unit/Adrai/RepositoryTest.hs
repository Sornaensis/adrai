{-# LANGUAGE OverloadedStrings #-}

module Adrai.RepositoryTest (tests) where

import Adrai.Git
import Adrai.Repository (isManagedSourcePath)
import Adrai.Types (RepoPath, gitRefText, mkRepoPath)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Text (Text)
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Repository snapshot contract"
    [ testCase "managed source suffixes are exact and case-sensitive" $ do
        map (isManagedSourcePath . path)
          [ "roots/a.decision.md",
            "roots/a.connection.md",
            "roots/a.DECISION.md",
            "roots/a.connection.md.bak",
            "roots/decision.md",
            "roots/.decision.md",
            "roots/a-decision.md"
          ]
          @?= [True, True, False, False, False, True, False],
      testCase "HEAD state decoder distinguishes strict attached and detached output" $ do
        case decodeGitHeadState ExitSuccess "refs/heads/main\n" BS.empty of
          Right (GitHeadAttached reference) -> gitRefText reference @?= "refs/heads/main"
          result -> assertFailure (show result)
        decodeGitHeadState (ExitFailure 1) BS.empty BS.empty @?= Right GitHeadDetached
        assertBool "attached ref requires one framed valid full ref" (isLeft (decodeGitHeadState ExitSuccess "main\n" BS.empty))
        assertBool "detached output must be empty" (isLeft (decodeGitHeadState (ExitFailure 1) "noise\n" BS.empty))
        case decodeGitHeadState (ExitFailure 2) BS.empty "failure" of
          Left (GitCommandFailed "symbolic HEAD" 2 _ _) -> pure ()
          result -> assertFailure (show result)
    ]

path :: Text -> RepoPath
path value =
  case mkRepoPath value of
    Left problem -> error (show problem)
    Right result -> result
