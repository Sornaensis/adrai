{-# LANGUAGE OverloadedStrings #-}

module Adrai.MarkdownTest (tests) where

import Adrai.Markdown (MarkdownSections (..), extractMarkdownSections)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "Markdown section extraction"
    [ testCase "extracts the static Context Decision Consequences example" $
        extractMarkdownSections staticExample
          @?= sections
            "The current cache key includes the workspace path."
            "Use the source digest and compiler ABI."
            "Cache entries can be shared across workspaces."
            "",
      testCase "recognizes every frozen heading alias" $ do
        mapM_ (assertAlias markdownContext "context body") contextAliases
        mapM_ (assertAlias markdownDecision "decision body") decisionAliases
        mapM_ (assertAlias markdownConsequences "consequences body") consequenceAliases,
      testCase "concatenates repeated categories and retains original unknown labels" $
        extractMarkdownSections
          "# Context\nfirst\n# Alternatives Considered\nalpha\n## Background\nsecond\n### Risks & Mitigations\nbeta"
          @?= sections
            "first\nsecond"
            ""
            ""
            "Alternatives Considered\nalpha\nRisks & Mitigations\nbeta",
      testCase "prepends preamble text to context" $
        extractMarkdownSections "A short preamble.\n\n# Context\nExplicit context.\n# Decision\nChoose A."
          @?= sections
            "A short preamble.\nExplicit context."
            "Choose A."
            ""
            "",
      testCase "uses the entire body as decision text when there are no headings" $
        extractMarkdownSections "plain text\n\nwith another paragraph\n"
          @?= sections "" "plain text\n\nwith another paragraph" "" "",
      testCase "normalizes CRLF and bare CR exactly like LF" $ do
        let lf = "preface\n# Decision\nchoose\n# Consequences\nresult\n"
            expected = extractMarkdownSections lf
        extractMarkdownSections (Text.replace "\n" "\r\n" lf) @?= expected
        extractMarkdownSections (Text.replace "\n" "\r" lf) @?= expected,
      testCase "matches Unicode splitlines and whitespace semantics" $
        extractMarkdownSections
          ("preamble\x2028##\x00a0\&Decision\x0085\&chosen\x2029## Context\x001c\&background")
          @?= sections "preamble\nbackground" "chosen" "" "",
      testCase "accepts at most three leading spaces before an ATX heading" $
        extractMarkdownSections
          " # Context\none\n  ###### Decision\ntwo\n   ### Consequences\nthree\n    # Context\nstays consequences"
          @?= sections "one" "two" "three\n    # Context\nstays consequences" "",
      testCase "removes optional trailing hashes and requires whitespace after opening hashes" $
        extractMarkdownSections "# Context###\none\n## Decision\t####\ntwo\n#Decision\nstays decision"
          @?= sections "one" "two\n#Decision\nstays decision" "" "",
      testCase "requires a label and accepts tab indentation" $ do
        extractMarkdownSections "##\nnot a heading\n\t# Decision\nchosen"
          @?= sections "##\nnot a heading" "chosen" "" ""
        extractMarkdownSections "\t\t\t# Context\nthree tabs\n\t\t\t\t# Decision\nnot a heading"
          @?= sections "three tabs\n\t\t\t\t# Decision\nnot a heading" "" "" "",
      testCase "unknown headings retain raw labels but drop attached closing hashes" $
        extractMarkdownSections "# Risks & Mitigations###\nHandle rollback."
          @?= sections "" "" "" "Risks & Mitigations\nHandle rollback.",
      testCase "treats heading-looking lines inside fences as headings" $
        extractMarkdownSections
          "# Decision\nbefore fence\n```markdown\n# Context\ninside fence\n```\nafter fence"
          @?= sections
            "inside fence\n```\nafter fence"
            "before fence\n```markdown"
            ""
            ""
    ]

staticExample :: Text
staticExample =
  "# Context\n\
  \The current cache key includes the workspace path.\n\
  \# Decision\n\
  \Use the source digest and compiler ABI.\n\
  \# Consequences\n\
  \Cache entries can be shared across workspaces."

contextAliases :: [Text]
contextAliases = ["Context", "BACKGROUND", "Problem", "Motivation"]

decisionAliases :: [Text]
decisionAliases = ["Decision", "Solution", "Chosen Approach", "Approach"]

consequenceAliases :: [Text]
consequenceAliases = ["Consequences", "Consequence", "Trade-offs", "Tradeoffs", "Outcomes", "Implications"]

assertAlias :: (MarkdownSections -> Text) -> Text -> Text -> IO ()
assertAlias select expected alias =
  select (extractMarkdownSections ("# " <> alias <> " ###\n" <> Text.stripEnd expected)) @?= expected

sections :: Text -> Text -> Text -> Text -> MarkdownSections
sections = MarkdownSections
