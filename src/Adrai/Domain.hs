{-# LANGUAGE OverloadedStrings #-}

module Adrai.Domain
  ( Domain,
    mkDomain,
    domainText,
    DomainError (..),
    domainErrorText,
    canonicalDomains,
    domainIsWithin,
    DomainRefinement,
    mkDomainRefinement,
    parseDomainRefinement,
    domainRefinementParent,
    domainRefinementChild,
    domainRefinementText,
  )
where

import Data.Char (isAscii, isAsciiLower, isDigit)
import Data.List (tails)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Normalize (NormalizationMode (NFKC), normalize)

-- | A canonical ASCII ADRAI domain. The constructor is deliberately private.
newtype Domain = Domain Text
  deriving (Eq, Ord, Show)

data DomainError
  = DomainEmpty
  | DomainNonAscii Text
  | DomainTooLong Int
  | DomainTooManySegments Int
  | DomainSegmentTooLong Text Int
  | DomainInvalidSegment Text
  | DomainAntichainViolation Domain Domain
  | DomainRefinementSyntax Text
  | DomainRefinementNotStrictDescendant Domain Domain
  deriving (Eq, Show)

-- | Render a stable, user-facing explanation without losing the structured
-- error available to callers.
domainErrorText :: DomainError -> Text
domainErrorText domainError =
  case domainError of
    DomainEmpty -> "domain may not be empty"
    DomainNonAscii value ->
      "domain contains non-ASCII characters after NFKC normalization: " <> value
    DomainTooLong actual ->
      "domain exceeds 160 canonical characters (actual " <> decimal actual <> ")"
    DomainTooManySegments actual ->
      "domain has more than 8 segments (actual " <> decimal actual <> ")"
    DomainSegmentTooLong segment actual ->
      "domain segment exceeds 48 characters (actual "
        <> decimal actual
        <> "): "
        <> segment
    DomainInvalidSegment segment ->
      "invalid domain segment; expected a letter-led lower-case segment with digits or internal hyphens: "
        <> segment
    DomainAntichainViolation parent child ->
      "domain set contains both ancestor "
        <> domainText parent
        <> " and descendant "
        <> domainText child
    DomainRefinementSyntax value ->
      "invalid domain refinement; expected exactly parent=parent.child: " <> value
    DomainRefinementNotStrictDescendant parent child ->
      domainText child
        <> " is not a strict descendant of "
        <> domainText parent

-- | Apply NFKC, then canonicalize trimmed, case-insensitive user input.
-- Whitespace around dots is ignored. Characters that remain non-ASCII after
-- compatibility normalization are outside the deliberately narrow grammar.
mkDomain :: Text -> Either DomainError Domain
mkDomain input
  | Text.any (not . isAscii) nfkcInput = Left (DomainNonAscii input)
  | Text.null normalized = Left DomainEmpty
  | Text.length normalized > maximumDomainLength =
      Left (DomainTooLong (Text.length normalized))
  | length segments > maximumDomainSegments =
      Left (DomainTooManySegments (length segments))
  | otherwise = Domain normalized <$ validateSegments segments
  where
    nfkcInput = normalize NFKC input
    normalized =
      Text.intercalate "."
        . fmap (asciiLower . Text.strip)
        . Text.splitOn "."
        $ Text.strip nfkcInput
    segments = Text.splitOn "." normalized

domainText :: Domain -> Text
domainText (Domain value) = value

-- | Canonicalize, sort, deduplicate, and validate a domain antichain.
canonicalDomains :: [Text] -> Either DomainError [Domain]
canonicalDomains inputs = do
  domains <- Set.toAscList . Set.fromList <$> traverse mkDomain inputs
  case firstAncestorPair domains of
    Nothing -> Right domains
    Just (parent, child) -> Left (DomainAntichainViolation parent child)

-- | A requested root contains itself and every dotted descendant.
domainIsWithin :: Domain -> Domain -> Bool
domainIsWithin candidate requested =
  candidate == requested
    || (domainText requested <> ".") `Text.isPrefixOf` domainText candidate

data DomainRefinement = DomainRefinement
  { domainRefinementParent :: Domain,
    domainRefinementChild :: Domain
  }
  deriving (Eq, Ord, Show)

mkDomainRefinement :: Domain -> Domain -> Either DomainError DomainRefinement
mkDomainRefinement parent child
  | child `domainIsWithin` parent && child /= parent =
      Right (DomainRefinement parent child)
  | otherwise = Left (DomainRefinementNotStrictDescendant parent child)

-- | Parse exactly one @=@ separator and canonicalize both sides.
parseDomainRefinement :: Text -> Either DomainError DomainRefinement
parseDomainRefinement input =
  case Text.splitOn "=" input of
    [parentInput, childInput]
      | not (Text.null (Text.strip parentInput)),
        not (Text.null (Text.strip childInput)) -> do
          parent <- mkDomain parentInput
          child <- mkDomain childInput
          mkDomainRefinement parent child
    _ -> Left (DomainRefinementSyntax input)

domainRefinementText :: DomainRefinement -> Text
domainRefinementText refinement =
  domainText (domainRefinementParent refinement)
    <> "="
    <> domainText (domainRefinementChild refinement)

maximumDomainLength :: Int
maximumDomainLength = 160

maximumDomainSegments :: Int
maximumDomainSegments = 8

maximumDomainSegmentLength :: Int
maximumDomainSegmentLength = 48

validateSegments :: [Text] -> Either DomainError ()
validateSegments [] = Right ()
validateSegments (segment : remaining)
  | Text.length segment > maximumDomainSegmentLength =
      Left (DomainSegmentTooLong segment (Text.length segment))
  | not (validSegment segment) = Left (DomainInvalidSegment segment)
  | otherwise = validateSegments remaining

validSegment :: Text -> Bool
validSegment segment =
  case Text.uncons segment of
    Nothing -> False
    Just (firstCharacter, _) ->
      isAsciiLower firstCharacter
        && all validHyphenPart (Text.splitOn "-" segment)
  where
    validHyphenPart part =
      not (Text.null part)
        && Text.all (\character -> isAsciiLower character || isDigit character) part

firstAncestorPair :: [Domain] -> Maybe (Domain, Domain)
firstAncestorPair domains =
  firstJust
    [ if child `domainIsWithin` parent && child /= parent
        then Just (parent, child)
        else Nothing
      | parent : later <- tails domains,
        child <- later
    ]

firstJust :: [Maybe value] -> Maybe value
firstJust [] = Nothing
firstJust (Nothing : remaining) = firstJust remaining
firstJust (Just value : _) = Just value

asciiLower :: Text -> Text
asciiLower = Text.map lower
  where
    lower character
      | character >= 'A' && character <= 'Z' =
          toEnum (fromEnum character + fromEnum 'a' - fromEnum 'A')
      | otherwise = character

decimal :: Int -> Text
decimal = Text.pack . show
