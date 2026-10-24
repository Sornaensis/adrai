{-# LANGUAGE OverloadedStrings #-}

module Adrai.ScopeFormatTest (tests) where

import Adrai.Scope
import Adrai.Types (RepoPath, mkRepoPath)
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "scope format"
    [ testCase "normalizes one leading dot and trailing directory slash" $ do
        canonical "./src/cache/**" @?= Right "src/cache/**"
        canonical "src/cache/" @?= Right "src/cache/**",
      testCase "rejects frozen unsafe scope vectors with exact errors" $
        forM_ invalidScopePatterns $ \(value, expected) ->
          mkScopePattern value @?= Left expected,
      testCase "single star never crosses a slash" $ do
        assertMatch True "src/*/cache/*.java" "src/a/cache/Key.java"
        assertMatch False "src/*/cache/*.java" "src/a/b/cache/Key.java",
      testCase "double star crosses directories and double-star slash permits zero" $ do
        assertMatch True "src/**/cache/*.java" "src/compiler/cache/Key.java"
        assertMatch True "src/**/cache/*.java" "src/cache/Key.java"
        assertMatch True "**/Main.hs" "Main.hs"
        assertMatch True "**/Main.hs" "src/Adrai/Main.hs"
        assertMatch False "**/Main.hs" "src/Adrai/Main.lhs",
      testCase "question mark consumes exactly one non-slash character" $ do
        assertMatch True "src/?.hs" "src/A.hs"
        assertMatch False "src/?.hs" "src/AB.hs"
        assertMatch False "src/?.hs" "src/a/b.hs",
      testCase "trailing directory scope covers nested repository paths" $ do
        assertMatch True "src/cache/" "src/cache/nested/Key.java"
        assertMatch False "src/cache/" "src/compiler/cache/Key.java",
      testCase "closed character classes are deterministic" $ do
        assertMatch True "snapshot-[0-9].json" "snapshot-7.json"
        assertMatch False "snapshot-[0-9].json" "snapshot-x.json"
        assertMatch True "snapshot-[!0-9].json" "snapshot-x.json"
        assertMatch False "snapshot-[!0-9].json" "snapshot-7.json",
      testCase "literal paths and cross-directory suffix globs remain distinct" $ do
        assertMatch True "docs/FORMAT.md" "docs/FORMAT.md"
        assertMatch False "docs/FORMAT.md" "docs/archive/FORMAT.md"
        assertMatch True "docs/**" "docs/archive/v1/FORMAT.md"
        patterns <- mapM scopeOrFail ["**", "**/*.hs", "src/**", "src/Adrai/**", "src/Adrai/Query.hs"]
        path <- pathOrFail "src/Adrai/Query.hs"
        bestScopeMatch patterns path @?= Just (ScopeMatchEvidence "src/Adrai/Query.hs" ScopeLiteral 3 0)
        bestScopeMatch (take 4 patterns) path @?= Just (ScopeMatchEvidence "**/*.hs" ScopeFile 0 2)
        assertSpecificity "**/Query.hs" "Query.hs" ScopeFile 1 1
        assertSpecificity "**/simulation/**" "simulation/cache.rs" ScopePath 1 2
        assertSpecificity "**/*.hs" "src/Query.hs" ScopeFile 0 2
        assertSpecificity "src/[a-z]/Query.hs" "src/a/Query.hs" ScopeFile 2 1
        assertSpecificity "src/**/*.rs" "src/Entity.rs" ScopeFile 1 2
        assertSpecificity "src/**/*.rs" "src/simulation/Entity.rs" ScopeFile 1 2
        assertMatch False "src/**/*.rs" "src/simulation/Entity.py"
        assertSpecificity "**/[A-Z]*" "nested/Entity" ScopeFile 0 3
        assertSpecificity "src/*" "src/Entity.rs" ScopePath 1 1
        assertSpecificity "src/**/*" "src/simulation/Entity.rs" ScopePath 1 2
        assertSpecificity "src*/**" "src-generated/Entity.rs" ScopePath 0 2
        assertSpecificity "[st]rc/**" "src/Entity.rs" ScopePath 0 2
        assertSpecificity "src/prefix**" "src/prefix-dir/Entity.rs" ScopePath 1 1
        assertSpecificity "src**" "src/simulation/Entity.rs" ScopePath 0 1
        assertSpecificity "src/**.rs" "src/simulation/Entity.rs" ScopeFile 1 1
        assertSpecificity "**" "src/Entity.rs" ScopeBroad 0 1
        assertSpecificity "**/*" "src/Entity.rs" ScopeBroad 0 2
    ]

canonical :: Text -> Either ScopePatternError Text
canonical = fmap scopePatternText . mkScopePattern

invalidScopePatterns :: [(Text, ScopePatternError)]
invalidScopePatterns =
  [ ("", ScopePatternEmpty),
    (" src/**", ScopePatternSurroundingWhitespace),
    ("src/** ", ScopePatternSurroundingWhitespace),
    ("src/\x0001/cache", ScopePatternControlCharacter '\x0001'),
    ("src\\cache\\**", ScopePatternBackslash),
    ("!src/cache/**", ScopePatternLeadingNegation),
    ("/src/cache/**", ScopePatternAbsolute),
    ("C:/src/cache/**", ScopePatternDriveQualified),
    ("./", ScopePatternEmpty),
    ("src//cache", ScopePatternEmptySegment),
    ("src/./cache", ScopePatternDotSegment),
    ("src/../cache", ScopePatternParentSegment),
    ("././src", ScopePatternDotSegment),
    ("src/[abc", ScopePatternUnclosedCharacterClass),
    ("src/[]/cache", ScopePatternEmptyCharacterClass),
    ("src/[!]/cache", ScopePatternEmptyCharacterClass)
  ]

assertMatch :: Bool -> Text -> Text -> IO ()
assertMatch expected patternText pathText = do
  scopePattern <- scopeOrFail patternText
  repoPath <- pathOrFail pathText
  scopeMatches scopePattern repoPath @?= expected

assertSpecificity :: Text -> Text -> ScopeSpecificity -> Int -> Int -> IO ()
assertSpecificity patternText pathText kind literalCount wildcardCount = do
  pattern <- scopeOrFail patternText
  path <- pathOrFail pathText
  bestScopeMatch [pattern] path @?= Just (ScopeMatchEvidence patternText kind literalCount wildcardCount)

scopeOrFail :: Text -> IO ScopePattern
scopeOrFail value =
  case mkScopePattern value of
    Left scopeError -> assertFailure (Text.unpack (scopePatternErrorText scopeError)) >> fail "unreachable"
    Right scopePattern -> pure scopePattern

pathOrFail :: Text -> IO RepoPath
pathOrFail value =
  case mkRepoPath value of
    Left pathError -> assertFailure (show pathError) >> fail "unreachable"
    Right repoPath -> pure repoPath
