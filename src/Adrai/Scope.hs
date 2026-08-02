{-# LANGUAGE OverloadedStrings #-}

module Adrai.Scope
  ( ScopePattern,
    mkScopePattern,
    scopePatternText,
    ScopePatternError (..),
    scopePatternErrorText,
    scopeMatches,
  )
where

import Adrai.Types (RepoPath, repoPathText)
import Data.Char (isAscii, isControl)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

data ScopePattern = ScopePattern Text [GlobToken]

instance Eq ScopePattern where
  ScopePattern left _ == ScopePattern right _ = left == right

instance Ord ScopePattern where
  compare (ScopePattern left _) (ScopePattern right _) = compare left right

instance Show ScopePattern where
  showsPrec precedence (ScopePattern value _) =
    showParen (precedence > 10) (showString "ScopePattern " . shows value)

data ScopePatternError
  = ScopePatternEmpty
  | ScopePatternSurroundingWhitespace
  | ScopePatternControlCharacter Char
  | ScopePatternBackslash
  | ScopePatternLeadingNegation
  | ScopePatternAbsolute
  | ScopePatternDriveQualified
  | ScopePatternEmptySegment
  | ScopePatternDotSegment
  | ScopePatternParentSegment
  | ScopePatternUnclosedCharacterClass
  | ScopePatternEmptyCharacterClass
  deriving (Eq, Show)

scopePatternErrorText :: ScopePatternError -> Text
scopePatternErrorText scopeError =
  case scopeError of
    ScopePatternEmpty -> "scope pattern may not be empty"
    ScopePatternSurroundingWhitespace ->
      "scope pattern may not have surrounding whitespace"
    ScopePatternControlCharacter character ->
      "scope pattern contains a control character: " <> Text.pack (show character)
    ScopePatternBackslash ->
      "scope pattern must use repository-style '/' separators"
    ScopePatternLeadingNegation ->
      "negative scope patterns are not supported; use explicit removal"
    ScopePatternAbsolute -> "scope pattern must be repository-relative"
    ScopePatternDriveQualified ->
      "scope pattern may not contain a drive-qualified path"
    ScopePatternEmptySegment -> "scope pattern contains an empty path segment"
    ScopePatternDotSegment -> "scope pattern contains a '.' path segment"
    ScopePatternParentSegment -> "scope pattern contains a '..' path segment"
    ScopePatternUnclosedCharacterClass ->
      "scope pattern contains an unclosed character class"
    ScopePatternEmptyCharacterClass ->
      "scope pattern contains an empty character class"

-- | Validate and canonicalize a repository-relative scope glob. One leading
-- @./@ is removed and a trailing slash becomes @/**@.
mkScopePattern :: Text -> Either ScopePatternError ScopePattern
mkScopePattern input
  | Text.null input = Left ScopePatternEmpty
  | Text.strip input /= input = Left ScopePatternSurroundingWhitespace
  | Just control <- Text.find isControl input =
      Left (ScopePatternControlCharacter control)
  | Text.any (== '\\') input = Left ScopePatternBackslash
  | Text.isPrefixOf "!" input = Left ScopePatternLeadingNegation
  | Text.isPrefixOf "/" input = Left ScopePatternAbsolute
  | driveQualified input = Left ScopePatternDriveQualified
  | Text.null normalized = Left ScopePatternEmpty
  | any Text.null segments = Left ScopePatternEmptySegment
  | any (== ".") segments = Left ScopePatternDotSegment
  | any (== "..") segments = Left ScopePatternParentSegment
  | otherwise = ScopePattern normalized <$> tokenize (Text.unpack normalized)
  where
    withoutLeadingDot = maybe input id (Text.stripPrefix "./" input)
    normalized
      | Text.isSuffixOf "/" withoutLeadingDot = withoutLeadingDot <> "**"
      | otherwise = withoutLeadingDot
    segments = Text.splitOn "/" normalized

scopePatternText :: ScopePattern -> Text
scopePatternText (ScopePattern value _) = value

-- | Match a validated pattern against a validated repository path. Matching is
-- memoized by pattern/path suffix, so wildcard backtracking is deterministic.
scopeMatches :: ScopePattern -> RepoPath -> Bool
scopeMatches (ScopePattern _ tokens) path = matchGlob tokens (Text.unpack (repoPathText path))

data GlobToken
  = GlobLiteral Char
  | GlobOne
  | GlobStar
  | GlobDoubleStar
  | GlobDoubleStarSlash
  | GlobClass CharacterClass
  deriving (Eq, Ord, Show)

data CharacterClass = CharacterClass Bool [ClassAtom]
  deriving (Eq, Ord, Show)

data ClassAtom
  = ClassCharacter Char
  | ClassRange Char Char
  deriving (Eq, Ord, Show)

tokenize :: String -> Either ScopePatternError [GlobToken]
tokenize [] = Right []
tokenize ('*' : '*' : '/' : remaining) =
  (GlobDoubleStarSlash :) <$> tokenize remaining
tokenize ('*' : '*' : remaining) =
  (GlobDoubleStar :) <$> tokenize remaining
tokenize ('*' : remaining) = (GlobStar :) <$> tokenize remaining
tokenize ('?' : remaining) = (GlobOne :) <$> tokenize remaining
tokenize ('[' : remaining) =
  case break (== ']') remaining of
    (_, []) -> Left ScopePatternUnclosedCharacterClass
    (content, _ : suffix) -> do
      characterClass <- parseCharacterClass content
      (GlobClass characterClass :) <$> tokenize suffix
tokenize (character : remaining) =
  (GlobLiteral character :) <$> tokenize remaining

parseCharacterClass :: String -> Either ScopePatternError CharacterClass
parseCharacterClass [] = Left ScopePatternEmptyCharacterClass
parseCharacterClass ('!' : content)
  | null content = Left ScopePatternEmptyCharacterClass
  | otherwise = Right (CharacterClass True (classAtoms content))
parseCharacterClass content = Right (CharacterClass False (classAtoms content))

classAtoms :: String -> [ClassAtom]
classAtoms (start : '-' : end : remaining) =
  ClassRange start end : classAtoms remaining
classAtoms (character : remaining) =
  ClassCharacter character : classAtoms remaining
classAtoms [] = []

matchGlob :: [GlobToken] -> String -> Bool
matchGlob tokens characters = fst (go Set.empty tokens characters)
  where
    go visited remainingTokens remainingCharacters
      | Set.member key visited = (False, visited)
      | otherwise =
          matchCurrent (Set.insert key visited) remainingTokens remainingCharacters
      where
        key = (length remainingTokens, length remainingCharacters)

    matchCurrent visited [] [] = (True, visited)
    matchCurrent visited [] (_ : _) = (False, visited)
    matchCurrent visited (GlobLiteral expected : remainingTokens) (actual : remainingCharacters)
      | expected == actual = go visited remainingTokens remainingCharacters
      | otherwise = (False, visited)
    matchCurrent visited (GlobLiteral _ : _) [] = (False, visited)
    matchCurrent visited (GlobOne : remainingTokens) (actual : remainingCharacters)
      | actual /= '/' = go visited remainingTokens remainingCharacters
      | otherwise = (False, visited)
    matchCurrent visited (GlobOne : _) [] = (False, visited)
    matchCurrent visited current@(GlobStar : remainingTokens) remainingCharacters =
      tryAlternative
        (go visited remainingTokens remainingCharacters)
        (\nextVisited ->
           case remainingCharacters of
             actual : rest
               | actual /= '/' -> go nextVisited current rest
             _ -> (False, nextVisited)
        )
    matchCurrent visited current@(GlobDoubleStar : remainingTokens) remainingCharacters =
      tryAlternative
        (go visited remainingTokens remainingCharacters)
        (\nextVisited ->
           case remainingCharacters of
             _ : rest -> go nextVisited current rest
             [] -> (False, nextVisited)
        )
    matchCurrent visited (GlobDoubleStarSlash : remainingTokens) remainingCharacters =
      tryAlternative
        (go visited remainingTokens remainingCharacters)
        (\nextVisited ->
           tryDirectoryEnds nextVisited remainingTokens remainingCharacters
        )
    matchCurrent visited (GlobClass characterClass : remainingTokens) (actual : remainingCharacters)
      | actual /= '/' && characterClassMatches characterClass actual =
          go visited remainingTokens remainingCharacters
      | otherwise = (False, visited)
    matchCurrent visited (GlobClass _ : _) [] = (False, visited)

    tryDirectoryEnds visited remainingTokens = scan visited
      where
        scan currentVisited [] = (False, currentVisited)
        scan currentVisited (actual : rest)
          | actual == '/' =
              tryAlternative
                (go currentVisited remainingTokens rest)
                (\nextVisited -> scan nextVisited rest)
          | otherwise = scan currentVisited rest

tryAlternative :: (Bool, state) -> (state -> (Bool, state)) -> (Bool, state)
tryAlternative firstAttempt secondAttempt =
  case firstAttempt of
    success@(True, _) -> success
    (False, state) -> secondAttempt state

characterClassMatches :: CharacterClass -> Char -> Bool
characterClassMatches (CharacterClass negated atoms) character =
  if negated then not matched else matched
  where
    matched = any (`classAtomMatches` character) atoms

classAtomMatches :: ClassAtom -> Char -> Bool
classAtomMatches (ClassCharacter expected) actual = expected == actual
classAtomMatches (ClassRange start end) actual = start <= actual && actual <= end

driveQualified :: Text -> Bool
driveQualified value =
  case Text.unpack (Text.take 2 value) of
    [drive, ':'] -> isAscii drive && asciiLetter drive
    _ -> False
  where
    asciiLetter character =
      (character >= 'A' && character <= 'Z')
        || (character >= 'a' && character <= 'z')
