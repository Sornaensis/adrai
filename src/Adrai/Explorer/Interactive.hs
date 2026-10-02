{-# LANGUAGE OverloadedStrings #-}
-- | Interactive and scripted modes share real revision-local reads and checked
-- writes. A successful write exits; errors retain the reviewed session.
module Adrai.Explorer.Interactive
  ( interactiveSession, scriptedMode, handleCommand, refreshSession ) where
import Adrai.Explorer.Mutation
import Adrai.Explorer.Render
import Adrai.Explorer.Types
import Adrai.Git (Repository, RevisionSpec (..), discoverRepository, gitOidText, repositoryHeadState, systemGit)
import Adrai.Graph (GraphReduction (..), ReducedAdr (..))
import Adrai.History (ReadSnapshot (..), defaultHistoryOptions)
import qualified Adrai.Query as Query
import Adrai.Repository (repositorySnapshot, repositorySnapshotManagedPaths, repositorySnapshotRevision, resolvedCommitOid)
import Adrai.Retrieval (RetrievalMode (..))
import qualified Adrai.Service.Query as Service
import Adrai.Service.Transaction (ExpectedRepositoryBasis (..))
import Adrai.Types (Actor, adrIdText, recordIdText, repoPathText)
import qualified Adrai.Types as Types
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (stdin, stdout, utf8, hSetEncoding, hSetBuffering, BufferMode (LineBuffering), hIsEOF, hFlush)

interactiveSession :: FilePath -> Actor -> IO (Either Text ())
interactiveSession root actor = do
  opened <- discoverRepository systemGit root
  case opened of
    Left problem -> pure (Left (T.pack (show problem)))
    Right repository -> do
      prepared <- refreshSession repository (defaultSession { sessionRepo = root, sessionActor = actor })
      case prepared of
        Left problem -> pure (Left problem)
        Right session -> do
          prepareHandles
          TIO.putStrLn "ADRAI Terminal Explorer. Type :help."
          commandLoop True repository session
          pure (Right ())

scriptedMode :: FilePath -> Actor -> IO ()
scriptedMode root actor = do
  opened <- discoverRepository systemGit root
  case opened of
    Left problem -> TIO.putStrLn ("Error: " <> T.pack (show problem))
    Right repository -> do
      prepared <- refreshSession repository (defaultSession { sessionRepo = root, sessionActor = actor })
      case prepared of
        Left problem -> TIO.putStrLn ("Error: " <> problem)
        Right session -> prepareHandles >> commandLoop False repository session

prepareHandles :: IO ()
prepareHandles = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  hSetBuffering stdout LineBuffering

commandLoop :: Bool -> Repository -> ExplorerSession -> IO ()
commandLoop interactive repository session = do
  eof <- hIsEOF stdin
  if eof then pure () else do
    if interactive then do
      TIO.putStr ("adrai[" <> sessionRevision session <> "]> ")
      hFlush stdout
    else pure ()
    input <- T.strip <$> TIO.getLine
    if T.null input then commandLoop interactive repository session else
      case parseCommand input of
        ExitCommand -> pure ()
        command -> do
          (next, output, exitGate) <- handleCommand repository command session initialState
          mapM_ TIO.putStrLn (stateOutput output)
          if exitGate then pure () else commandLoop interactive repository next

-- | Explicitly adopt a selected revision and HEAD identity. An invalid selection
-- leaves the caller's previous basis intact. Reads never refresh it implicitly.
refreshSession :: Repository -> ExplorerSession -> IO (Either Text ExplorerSession)
refreshSession repository session = do
  before <- repositoryHeadState repository
  observed <- repositorySnapshot repository (RevisionSpec (sessionRevision session))
  after <- repositoryHeadState repository
  pure $ case (before, observed, after) of
    (Right headState, Right snapshot, Right finalState) | headState == finalState ->
      Right session
        { sessionBasis = Just (ExpectedRepositoryBasis (resolvedCommitOid (repositorySnapshotRevision snapshot)) headState),
          sessionManagedPaths = Just (repositorySnapshotManagedPaths snapshot),
          sessionViewedStates = Map.empty }
    (Left problem, _, _) -> Left (T.pack (show problem))
    (_, Left problem, _) -> Left (T.pack (show problem))
    (_, _, Left problem) -> Left (T.pack (show problem))
    _ -> Left "HEAD/ref changed while adopting revision; retry :refresh."

handleCommand :: Repository -> ExplorerCommand -> ExplorerSession -> ExplorerState -> IO (ExplorerSession, ExplorerState, Bool)
handleCommand repository command session state = case command of
  HelpCommand -> output session renderHelp
  InvalidCommand message -> output session [message]
  ExitCommand -> pure (session, state, False)
  SetViewCommand view -> output (session { sessionView = view }) ["View updated."]
  SetModeCommand mode -> output (session { sessionMode = mode }) ["Search mode updated."]
  SetObsoleteCommand include -> output (session { sessionIncludeObsolete = include }) ["Obsolete filter updated."]
  SetFilePathCommand file -> output (session { sessionFilePath = file }) ["File filter updated."]
  SetActorCommand actor -> output (session { sessionActor = actor }) ["Actor updated."]
  SetRevisionCommand revision -> adopt (session { sessionRevision = revision })
  RefreshCommand -> adopt (session { sessionRevision = "HEAD" })
  SearchCommand query -> readAt $ \revision -> do
    let request = (Query.defaultSearchRequest query)
          { Query.searchRequestMode = retrieval (sessionMode session),
            Query.searchRequestView = viewMode (sessionView session),
            Query.searchRequestIncludeObsolete = sessionIncludeObsolete session,
            Query.searchRequestFile = sessionFilePath session }
    result <- Service.runSearch repository (Service.SearchServiceRequest revision request)
    either errorOutput (output (session { sessionQuery = query }) . renderSearchResults) result
  ShowCommand adr -> showAdr adr (sessionView session)
  ViewCommand adr view -> showAdr adr view
  HistoryCommand adr -> readAt $ \revision -> do
    result <- Service.runHistory repository (Service.HistoryRequest (adrIdText <$> adr) revision defaultHistoryOptions)
    either errorOutput (output session . renderHistory) result
  ConflictsCommand -> readAt $ \revision -> do
    result <- Service.readSnapshotAt repository revision
    case result of
      Left problem -> errorOutput problem
      Right snapshot -> case traverse (Query.projectCollapsed Query.RichProjection snapshot . reducedAdrId) (graphReductionAdrs (readSnapshotReduction snapshot)) of
        Left problem -> errorOutput problem
        Right projections ->
          let conflicted = filter (Query.resolutionStateRequired . Query.collapsedResolution) projections
          in output session (if null conflicted then ["No conflicts."] else concatMap (\p -> renderConflict (adrIdText (Query.collapsedAdr p)) (Query.resolutionStateConflicts (Query.collapsedResolution p))) conflicted)
  CreateCommand _ -> mutate
  AmendCommand _ _ -> mutate
  StatusCommand _ _ -> mutate
  where
    output next lines' = pure (next, initialState { stateOutput = lines' }, False)
    errorOutput problem = output session ["Error: " <> T.pack (show problem)]
    readAt action = case sessionBasis session of
      Nothing -> output session ["Error: no reviewed revision. Use :refresh."]
      Just basis -> action (gitOidText (expectedBasisHead basis))
    adopt candidate = do
      result <- refreshSession repository candidate
      either (\problem -> output session ["Error: " <> problem]) (\next -> output next ["Revision adopted; view ADRs again before editing."]) result
    showAdr adr view = readAt $ \revision -> do
      result <- Service.runShow repository (Service.ShowRequest (adrIdText adr) (viewMode view) revision False)
      case result of
        Left problem -> errorOutput problem
        Right projection ->
          let (token, lines') = case projection of
                Service.ShowCollapsed p -> (Query.collapsedStateToken p, renderCollapsed p)
                Service.ShowExploded p -> (Query.explodedStateToken p, renderExploded p)
          in output (session { sessionViewedStates = Map.insert adr token (sessionViewedStates session) }) lines'
    mutate = do
      result <- runMutation session command
      case result of
        MutationError problem -> output session ["Error: " <> problem]
        MutationResult kind operation adr record commit paths warnings ->
          pure (session, initialState { stateOutput =
            [ "ADR " <> kind <> " successfully.", "Operation: " <> T.pack operation,
              "ADR: " <> adrIdText adr, "Commit: " <> gitOidText commit ]
            <> maybe [] (\ident -> ["Record: " <> recordIdText ident]) record
            <> map (("Path: " <>) . repoPathText) paths
            <> map ("Warning: " <>) warnings }, True)

viewMode :: ViewMode -> Types.ViewMode
viewMode CollapsedView = Types.CollapsedView
viewMode ExplodedView = Types.ExplodedView
retrieval :: SearchMode -> RetrievalMode
retrieval FtsOnly = FtsRetrieval
retrieval VectorOnly = VectorRetrieval
retrieval HybridMode = HybridRetrieval
