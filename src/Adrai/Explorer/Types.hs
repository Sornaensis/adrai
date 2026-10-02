{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
-- | Terminal commands and the immutable basis reviewed by a session.
module Adrai.Explorer.Types
  ( ExplorerSession (..), defaultSession, ExplorerCommand (..), MutationDraft (..),
    parseCommand, ViewMode (..), SearchMode (..), ExplorerState (..), initialState,
    SearchFilter (..), emptyFilter ) where
import Adrai.Domain (Domain, canonicalDomains, domainErrorText)
import Adrai.Provenance (normalizeLineEndings)
import Adrai.Scope (ScopePattern, mkScopePattern, scopePatternErrorText)
import Adrai.Service.Transaction (ExpectedRepositoryBasis)
import Adrai.Types (AdrId, Actor, ActorKind (..), ManagedPaths, RepoPath, StateToken, mkActor, mkAdrId, mkRepoPath)
import qualified Data.Aeson as Json
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

data ExplorerSession = ExplorerSession
  { sessionRepo :: FilePath, sessionView :: ViewMode, sessionMode :: SearchMode,
    sessionIncludeObsolete :: Bool, sessionFilePath :: Maybe RepoPath,
    sessionRevision :: Text, sessionQuery :: Text, sessionActor :: Actor,
    sessionBasis :: Maybe ExpectedRepositoryBasis,
    sessionManagedPaths :: Maybe ManagedPaths,
    sessionViewedStates :: Map.Map AdrId StateToken } deriving (Show)
defaultSession :: ExplorerSession
defaultSession = ExplorerSession "." CollapsedView HybridMode False Nothing "HEAD" "" actor Nothing Nothing Map.empty
  where actor = either (error . show) id (mkActor HumanActor "user" Nothing)
data ViewMode = CollapsedView | ExplodedView deriving (Eq, Ord, Show)
data SearchMode = FtsOnly | VectorOnly | HybridMode deriving (Eq, Ord, Show)
-- | Actor and concurrency tokens come from the session, never from draft JSON.
data MutationDraft = MutationDraft
  { draftTitle :: Text, draftSummary :: Text, draftBody :: Text,
    draftDomains :: [Domain], draftScopes :: [ScopePattern],
    draftChangeSummary :: Text } deriving (Eq, Show)
data ExplorerCommand
  = HelpCommand | InvalidCommand Text | SearchCommand Text | ShowCommand AdrId
  | ViewCommand AdrId ViewMode | HistoryCommand (Maybe AdrId) | ConflictsCommand
  | CreateCommand MutationDraft | AmendCommand AdrId MutationDraft | StatusCommand AdrId Text
  | SetViewCommand ViewMode | SetModeCommand SearchMode | SetObsoleteCommand Bool
  | SetRevisionCommand Text | RefreshCommand | SetFilePathCommand (Maybe RepoPath)
  | SetActorCommand Actor | ExitCommand deriving (Eq, Show)

parseCommand :: Text -> ExplorerCommand
parseCommand raw = case T.words input of
  ["exit"] -> ExitCommand
  ["quit"] -> ExitCommand
  [":q"] -> ExitCommand
  [":quit"] -> ExitCommand
  ["help"] -> HelpCommand
  [":help"] -> HelpCommand
  ["conflicts"] -> ConflictsCommand
  [":conflicts"] -> ConflictsCommand
  ["history"] -> HistoryCommand Nothing
  [":history"] -> HistoryCommand Nothing
  [command, ident] | elem command ["history", ":history"] -> withAdr ident (HistoryCommand . Just)
  command : rest | elem command ["search", ":search"], not (null rest) -> SearchCommand (T.unwords rest)
  [command, ident] | elem command ["show", ":show"] -> withAdr ident ShowCommand
  [":view", mode] | Just view <- parseView mode -> SetViewCommand view
  [command, ident] | elem command ["view", ":view"] -> withAdr ident (\adr -> ViewCommand adr CollapsedView)
  [command, ident, mode] | elem command ["view", ":view"], Just view <- parseView mode -> withAdr ident (\adr -> ViewCommand adr view)
  [command, ident, status] | elem command ["status", ":status"], elem (T.toLower status) ["active", "obsolete"] -> withAdr ident (\adr -> StatusCommand adr (T.toLower status))
  [":mode", "fts"] -> SetModeCommand FtsOnly
  [":mode", "vector"] -> SetModeCommand VectorOnly
  [":mode", "hybrid"] -> SetModeCommand HybridMode
  [":obsolete", value] -> obsolete value
  [":revision", revision] -> SetRevisionCommand revision
  [":refresh"] -> RefreshCommand
  [":actor", value] -> either invalid SetActorCommand (parseActor value)
  ":file" : rest | not (null rest) -> file (after ":file")
  [command, "obsolete", value] | elem command ["filter", ":filter"] -> obsolete value
  command : "file" : rest | elem command ["filter", ":filter"], not (null rest) -> file (T.strip (T.drop 4 (after command)))
  command : _ | elem command ["create", ":create"] -> either invalid CreateCommand (parseDraft False (after command))
  command : ident : _ | elem command ["amend", ":amend"] -> withAdr ident (\adr -> either invalid (AmendCommand adr) (parseDraft True (T.strip (T.drop (T.length ident) (after command)))))
  command : _ | elem (T.dropWhile (== ':') command) reserved || T.isPrefixOf ":" command -> invalid "Invalid explorer command or syntax"
  _ -> SearchCommand input
  where
    input = T.strip raw
    after command = T.strip (T.drop (T.length command) input)
    invalid message = InvalidCommand (message <> ". No Git commit was made. Type :help for syntax.")
    withAdr ident make = either (const (invalid "Invalid ADR ID")) make (mkAdrId ident)
    reserved = ["exit", "quit", "help", "conflicts", "history", "search", "show", "view", "amend", "status", "create", "filter"]
    obsolete "on" = SetObsoleteCommand True
    obsolete "off" = SetObsoleteCommand False
    obsolete _ = invalid "Use :obsolete on|off"
    file "clear" = SetFilePathCommand Nothing
    file value = either (const (invalid "Invalid repository-relative file path")) (SetFilePathCommand . Just) (mkRepoPath value)
parseView :: Text -> Maybe ViewMode
parseView value = case T.toLower value of
  "collapsed" -> Just CollapsedView
  "exploded" -> Just ExplodedView
  _ -> Nothing
parseActor :: Text -> Either Text Actor
parseActor value = case T.splitOn ":" value of
  [kind, ident] -> do
    actorKind <- case kind of
      "human" -> Right HumanActor
      "llm" -> Right LlmActor
      "service" -> Right ServiceActor
      _ -> Left "Actor kind must be human, llm or service"
    first (T.pack . show) (mkActor actorKind ident Nothing)
  _ -> Left "Actor must be KIND:ID"
parseDraft :: Bool -> Text -> Either Text MutationDraft
parseDraft amendment input = do
  value <- first T.pack (Json.eitherDecodeStrict' (TE.encodeUtf8 input))
  fields <- case value of
    Json.Object object -> Right object
    _ -> Left "Draft must be a JSON object"
  let allowed = ["title", "summary", "body"] <> if amendment then ["change_summary"] else ["domains", "applies_to"]
      unknown = filter (\key -> not (elem key allowed)) (map Key.toText (KM.keys fields))
  if null unknown then Right () else Left ("Unknown draft fields: " <> T.intercalate ", " unknown)
  title <- string fields "title" (not amendment)
  summary <- string fields "summary" (not amendment)
  body <- string fields "body" True
  reason <- if amendment then string fields "change_summary" True else Right ""
  domains <- strings fields "domains" >>= first domainErrorText . canonicalDomains
  scopes <- strings fields "applies_to" >>= traverse (first scopePatternErrorText . mkScopePattern)
  pure (MutationDraft title summary ((<> "\n") . T.strip . normalizeLineEndings $ body) domains scopes reason)
  where
    string fields name required = case KM.lookup (Key.fromText name) fields of
      Nothing | not required -> Right ""
      Just (Json.String value) | not (T.null (T.strip value)) -> Right value
      _ -> Left ("Draft field " <> name <> " must be a nonempty string")
    strings fields name = case KM.lookup (Key.fromText name) fields of
      Nothing -> Right []
      Just (Json.Array values) -> traverse text (foldr (:) [] values)
      _ -> Left ("Draft field " <> name <> " must be an array of strings")
    text (Json.String value) = Right value
    text _ = Left "Draft arrays must contain strings"
data ExplorerState = ExplorerState
  { stateOutput :: [Text], stateCursorPosition :: Int,
    stateLastResults :: [(Int, Text, Text, Double, [Text])] } deriving (Show)
initialState :: ExplorerState
initialState = ExplorerState [] 0 []
data SearchFilter = SearchFilter
  { filterDomains :: [Text], filterFile :: Maybe RepoPath,
    filterSince :: Maybe Integer, filterUntil :: Maybe Integer } deriving (Show)
emptyFilter :: SearchFilter
emptyFilter = SearchFilter [] Nothing Nothing Nothing
