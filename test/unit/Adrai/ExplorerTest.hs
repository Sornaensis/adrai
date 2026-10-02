{-# LANGUAGE OverloadedStrings #-}
module Adrai.ExplorerTest (tests) where
import Adrai.Explorer.Interactive (handleCommand, refreshSession)
import Adrai.Explorer.Mutation (MutationResult (..), runMutation)
import Adrai.Explorer.Types
import Adrai.Format.Config (defaultConfigText)
import Adrai.Git (Repository, RevisionSpec (..), discoverRepository, gitOidText, resolveRevision, systemGit)
import Adrai.GitTestSupport (commitFile, gitSuccess, initTestRepository, outputText)
import Adrai.Graph (GraphReduction (..), ReducedAdr (..))
import Adrai.History (ReadSnapshot (..))
import Adrai.Integration.CLI (spawnAdraiStdin)
import qualified Adrai.Service.Query as Query
import Adrai.Types (AdrId, adrIdText)
import Control.Monad (foldM, forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

tests :: TestTree
tests = testGroup "terminal explorer"
  [ testCase "real reads controls and generated checked writes use configured roots" readsAndWrites
  , testCase "stale and selected revisions reject writes until explicit HEAD refresh" reviewedBasis
  ]

withExplorer :: (FilePath -> Repository -> ExplorerSession -> IO ()) -> IO ()
withExplorer action = withSystemTempDirectory "explore" $ \root -> do
  initTestRepository root
  let config = T.replace "architecture/adrai/connections" "design/links"
             (T.replace "architecture/adrai/decisions" "design/decisions" defaultConfigText)
  _ <- commitFile root ".adrai.toml" (TE.encodeUtf8 config)
  repository <- discoverRepository systemGit root >>= requireRight
  session <- refreshSession repository (defaultSession { sessionRepo = root }) >>= requireRight
  action root repository session

createInput :: Text
createInput = "create {\"title\":\"SQLite decisions\",\"summary\":\"Storage\",\"body\":\"Use SQLite\\n\",\"domains\":[\"platform\"],\"applies_to\":[\"src/**\"]}"
amendInput :: AdrId -> Text
amendInput adr = "amend " <> adrIdText adr <> " {\"title\":\"Updated SQLite\",\"summary\":\"Updated storage\",\"body\":\"New decision\\n\",\"change_summary\":\"Reviewed update\"}"

dispatch :: Repository -> ExplorerSession -> Text -> IO (ExplorerSession, ExplorerState, Bool)
dispatch repository session text = handleCommand repository (parseCommand text) session initialState

headOid :: Repository -> IO Text
headOid repository = gitOidText <$> (resolveRevision repository (RevisionSpec "HEAD") >>= requireRight)

createdAdr :: Repository -> IO AdrId
createdAdr repository = do
  snapshot <- Query.readSnapshotAt repository "HEAD" >>= requireRight
  case graphReductionAdrs (readSnapshotReduction snapshot) of
    [adr] -> pure (reducedAdrId adr)
    _ -> fail "Expected one real created ADR"

readsAndWrites :: IO ()
readsAndWrites = withExplorer $ \root repository initial -> do
  before <- headOid repository
  (created, createOutput, committed) <- dispatch repository initial createInput
  committed @?= True
  assertBool "actual commit output" (any (T.isPrefixOf "Commit: ") (stateOutput createOutput))
  after <- headOid repository
  assertBool "create changed HEAD" (after /= before)
  adr <- createdAdr repository
  assertBool "generated identifier" (adrIdText adr /= "A00000000000000000000000000")
  assertBool "configured paths" (all (T.isPrefixOf "Path: design/") (filter (T.isPrefixOf "Path: ") (stateOutput createOutput)))
  refreshed <- refreshSession repository created >>= requireRight
  (fts, _, _) <- dispatch repository refreshed ":mode fts"
  (searched, searchOutput, _) <- dispatch repository fts "search SQLite"
  assertBool "complete copyable search identifier" (any (T.isInfixOf (adrIdText adr)) (stateOutput searchOutput))
  let visibleId = T.take 27 (snd (T.breakOn "A" (T.unlines (stateOutput searchOutput))))
  visibleId @?= adrIdText adr
  (_, selectedOutput, _) <- dispatch repository searched ("show " <> visibleId)
  assertBool "search identifier opens actual detail" (any (T.isInfixOf "Use SQLite") (stateOutput selectedOutput))
  let readCommands = [ (":mode fts", "Search mode updated.")
              , ("search SQLite", "SQLite decisions")
              , ("filter file src/store.hs", "File filter updated.")
              , ("SQLite", "SQLite decisions")
              , (":file clear", "File filter updated.")
              , (":obsolete on", "Obsolete filter updated.")
              , (":view collapsed", "View updated.")
              , ("show " <> adrIdText adr, "Use SQLite")
              , ("view " <> adrIdText adr <> " EXPLODED", "LINEAGE")
              , ("history " <> adrIdText adr, "SQLite decisions")
              , ("conflicts", "No conflicts.")
              , (":actor service:explorer-test", "Actor updated.") ]
  viewed <- foldM (\session (input, expected) -> do
    (next, output, exitGate) <- dispatch repository session input
    exitGate @?= False
    assertBool (T.unpack input <> ": " <> show (stateOutput output)) (any (T.isInfixOf expected) (stateOutput output))
    pure next) refreshed readCommands
  headOid repository >>= (@?= after)
  (exitCode, stdoutBytes, stderrBytes) <- spawnAdraiStdin root ["explore"] (LBS.fromStrict (TE.encodeUtf8 (T.unlines [":mode fts", "search SQLite", "show " <> adrIdText adr, "history", "conflicts", "exit"])))
  exitCode @?= ExitSuccess
  stderrBytes @?= LBS.empty
  forM_ ["SQLite decisions", "Use SQLite", "HISTORY", "No conflicts."] $ \expected ->
    assertBool "public explorer real output" (T.isInfixOf expected (TE.decodeUtf8 (LBS.toStrict stdoutBytes)))
  headOid repository >>= (@?= after)
  (unchanged, badOutput, badExit) <- dispatch repository viewed "create {\"unknown\":true}"
  badExit @?= False
  assertBool "malformed guidance" (any (T.isInfixOf ":help") (stateOutput badOutput))
  headOid repository >>= (@?= after)
  (amended, amendment, amendExit) <- dispatch repository unchanged (amendInput adr)
  amendExit @?= True
  assertBool "generated record" (not (any (T.isInfixOf "R00000000000000000000000000") (stateOutput amendment)))
  current <- refreshSession repository amended >>= requireRight
  (shown, shownOutput, _) <- dispatch repository current ("show " <> adrIdText adr)
  assertBool "amended data" (any (T.isInfixOf "New decision") (stateOutput shownOutput))
  (_, _, obsoleteExit) <- dispatch repository shown ("status " <> adrIdText adr <> " obsolete")
  obsoleteExit @?= True
  active <- refreshSession repository shown >>= requireRight
  (obsoleteView, _, _) <- dispatch repository active ("show " <> adrIdText adr)
  (_, _, activeExit) <- dispatch repository obsoleteView ("status " <> adrIdText adr <> " active")
  activeExit @?= True

reviewedBasis :: IO ()
reviewedBasis = withExplorer $ \root repository initial -> do
  created <- runMutation initial (parseCommand createInput)
  case created of
    MutationError problem -> fail (T.unpack problem)
    _ -> pure ()
  adr <- createdAdr repository
  session <- refreshSession repository initial >>= requireRight
  (viewed, _, _) <- dispatch repository session ("show " <> adrIdText adr)
  assertBool "real viewed token" (Map.member adr (sessionViewedStates viewed))
  sameHead <- headOid repository
  -- An explicit immutable snapshot stays read-only even when it equals HEAD.
  (selected, _, _) <- dispatch repository viewed (":revision " <> sameHead)
  (selectedView, _, _) <- dispatch repository selected ("show " <> adrIdText adr)
  assertRejected root repository selectedView createInput
  assertRejected root repository selectedView (amendInput adr)
  assertRejected root repository selectedView ("status " <> adrIdText adr <> " obsolete")
  _ <- commitFile root "external.txt" "External change\n"
  assertRejected root repository viewed createInput
  assertRejected root repository viewed (amendInput adr)
  assertRejected root repository viewed ("status " <> adrIdText adr <> " obsolete")
  (historical, _, _) <- dispatch repository viewed (":revision " <> sameHead)
  (historicalView, _, _) <- dispatch repository historical ("view " <> adrIdText adr <> " exploded")
  assertRejected root repository historicalView (amendInput adr)
  (invalidRevision, _, _) <- dispatch repository viewed ":revision absent-ref"
  sessionBasis invalidRevision @?= sessionBasis viewed
  (cancelled, _, cancelExit) <- dispatch repository viewed "exit"
  cancelExit @?= False
  sessionBasis cancelled @?= sessionBasis viewed
  (fresh, _, _) <- dispatch repository viewed ":refresh"
  assertBool "refresh cleared viewed tokens" (Map.null (sessionViewedStates fresh))
  assertRejected root repository fresh (amendInput adr)
  (refreshedSelection, _, _) <- dispatch repository historicalView ":refresh"
  sessionRevision refreshedSelection @?= "HEAD"
  sessionBasis refreshedSelection @?= sessionBasis fresh
  assertBool "selected refresh cleared tokens" (Map.null (sessionViewedStates refreshedSelection))
  (adopted, _, _) <- dispatch repository historicalView ":revision HEAD"
  (ready, _, _) <- dispatch repository adopted ("show " <> adrIdText adr)
  before <- headOid repository
  (_, output, success) <- dispatch repository ready (amendInput adr)
  success @?= True
  assertBool "actual committed result" (any (T.isPrefixOf "Commit: ") (stateOutput output))
  after <- headOid repository
  assertBool "explicit adoption succeeded" (after /= before)
  parent <- outputText <$> gitSuccess root ["rev-parse", "HEAD^"] BS.empty
  parent @?= before
  updated <- refreshSession repository ready >>= requireRight
  (_, detail, _) <- dispatch repository updated ("show " <> adrIdText adr)
  assertBool "committed body" (any (T.isInfixOf "New decision") (stateOutput detail))

assertRejected :: FilePath -> Repository -> ExplorerSession -> Text -> IO ()
assertRejected root repository session input = do
  before <- callerState root repository
  (_, output, success) <- dispatch repository session input
  success @?= False
  assertBool ("rejection " <> T.unpack input) (any (T.isPrefixOf "Error: ") (stateOutput output))
  after <- callerState root repository
  after @?= before

callerState :: FilePath -> Repository -> IO (Text, BS.ByteString, BS.ByteString, BS.ByteString)
callerState root repository = do
  head' <- headOid repository
  index <- BS.readFile (root </> ".git" </> "index")
  tree <- gitSuccess root ["ls-tree", "-r", "HEAD"] BS.empty
  status <- gitSuccess root ["status", "--porcelain=v1", "--untracked-files=all"] BS.empty
  pure (head', index, tree, status)

requireRight :: Show problem => Either problem value -> IO value
requireRight = either (fail . show) pure
