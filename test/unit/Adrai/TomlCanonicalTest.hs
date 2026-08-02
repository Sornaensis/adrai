{-# LANGUAGE OverloadedStrings #-}

module Adrai.TomlCanonicalTest (tests) where

import Adrai.Format.Toml
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "canonical TOML rendering"
    [ testCase "renders printable Unicode unchanged" $
        renderTomlString "architecture/beslutning-ø/日本語"
          @?= "\"architecture/beslutning-ø/日本語\"",
      testCase "escapes quotes, slashes, and named controls canonically" $
        renderTomlString "quote \" slash \\ \b\t\n\f\r"
          @?= "\"quote \\\" slash \\\\ \\b\\t\\n\\f\\r\"",
      testCase "uses TOML 1.0 Unicode escapes for other controls" $
        renderTomlString (T.pack ['\0', '\ESC', '\DEL'])
          @?= "\"\\u0000\\u001B\\u007F\"",
      testCase "renders compact string arrays in input order" $
        renderTomlStringArray (["refs/heads/main", "quote\""] :: [Text])
          @?= "[\"refs/heads/main\", \"quote\\\"\"]"
    ]
