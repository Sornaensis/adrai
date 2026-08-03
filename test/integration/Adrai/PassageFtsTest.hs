{-# LANGUAGE OverloadedStrings #-}

module Adrai.PassageFtsTest (tests) where

import Adrai.Compiler (materializeCurrentSearch)
import Adrai.CompilerMaterializationTest (p303RationaleSnapshot)
import Adrai.Retrieval
import Adrai.Sqlite
import Control.Exception (bracket)
import qualified Data.Set as Set
import Data.Text (Text)
import Database.SQLite.Simple (Connection, close, open)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "P3-05 persisted passage FTS"
    [ testCase "exact OR stemmed and identifier channels retrieve persisted passages" channelContract,
      testCase "allowed sections and descending item-ID ties are deterministic" filteringAndTieContract,
      testCase "empty allowed sets and typed FTS failures are explicit" emptyAndErrorContract
    ]

channelContract :: IO ()
channelContract = withMemory $ \connection -> do
  materialization <- persistBaseline connection
  let allowed = passageIds materialization
      limit = mustRight (mkCandidateLimit 20)
      decisionId = passageIdFor DecisionSection materialization
      consequenceId = passageIdFor ConsequencesSection materialization
      titleId = passageIdFor TitleSummarySection materialization

  let exactPlan = buildQueryPlan "decision consequence" []
  queryPlanFtsExactTerms exactPlan @?= ["\"decision\"", "\"consequence\""]
  exactChannels <- mustRightIO (runPassageFtsChannels connection exactPlan allowed limit)
  let exactIds = map ftsHitItemId (passageFtsExact exactChannels)
  assertBool "OR retrieves the decision-only passage" (decisionId `elem` exactIds)
  assertBool "OR retrieves the consequence-only passage" (consequenceId `elem` exactIds)

  stemmedChannels <-
    mustRightIO
      (runPassageFtsChannels connection (buildQueryPlan "decisions" []) allowed limit)
  passageFtsExact stemmedChannels @?= []
  assertBool
    "Porter stemming retrieves singular decision text for a plural query"
    (decisionId `elem` map ftsHitItemId (passageFtsStemmed stemmedChannels))

  identifierChannels <-
    mustRightIO
      (runPassageFtsChannels connection (buildQueryPlan "CDAPI" []) allowed limit)
  assertBool
    "identifier expansion retrieves the persisted title-summary passage"
    (titleId `elem` map ftsHitItemId (passageFtsIdentifier identifierChannels))

filteringAndTieContract :: IO ()
filteringAndTieContract = withMemory $ \connection -> do
  initializeSearchSchema connection >>= (@?= Right ())
  let baseline = baselineMaterialization
      decision = passageFor DecisionSection baseline
      tiePassage itemId ordinal =
        decision
          { searchPassageId = itemId,
            searchPassageOrdinal = ordinal,
            searchPassageLineStart = ordinal + 1,
            searchPassageLineEnd = ordinal + 1,
            searchPassageText = "passage tie token",
            searchPassageIdentifiers = "passage tie token"
          }
      tieA = tiePassage "tie-a" 900
      tieZ = tiePassage "tie-z" 901
      materialization =
        baseline
          { searchMaterializationPassages =
              searchMaterializationPassages baseline <> [tieA, tieZ]
          }
      decisionId = searchPassageId decision
      allowedDecision = Set.singleton decisionId
      tieAllowed = Set.fromList [searchPassageId tieA, searchPassageId tieZ]
      limit = mustRight (mkCandidateLimit 10)
  replaceSearchMaterialization connection materialization >>= (@?= Right ())

  filtered <-
    mustRightIO
      (runPassageFtsChannels connection (buildQueryPlan "decision" []) allowedDecision limit)
  map ftsHitItemId (passageFtsExact filtered) @?= [decisionId]
  assertBool
    "every channel is restricted to the caller's allowed sections"
    ( all
        (`Set.isSubsetOf` allowedDecision)
        [ Set.fromList (map ftsHitItemId (passageFtsExact filtered)),
          Set.fromList (map ftsHitItemId (passageFtsStemmed filtered)),
          Set.fromList (map ftsHitItemId (passageFtsIdentifier filtered))
        ]
    )

  tied <-
    mustRightIO
      (runPassageFtsChannels connection (buildQueryPlan "tie token" []) tieAllowed limit)
  map ftsHitItemId (passageFtsExact tied) @?= ["tie-z", "tie-a"]

emptyAndErrorContract :: IO ()
emptyAndErrorContract = do
  withMemory $ \connection -> do
    initializeSearchSchema connection >>= (@?= Right ())
    let limit = mustRight (mkCandidateLimit 3)
        plan = buildQueryPlan "needle" []
    empty <- mustRightIO (runPassageFtsChannels connection plan Set.empty limit)
    empty @?= PassageFtsCandidates [] [] []

    let malformedPlan =
          plan
            { queryPlanFtsExactTerms = ["\"unterminated"],
              queryPlanFtsStemmed = "",
              queryPlanFtsIdentifier = ""
            }
    runPassageFtsChannels connection malformedPlan (Set.singleton "missing") limit
      >>= (@?= Left (MalformedFtsQuery PassageExactTarget))

  withMemory $ \connection -> do
    let limit = mustRight (mkCandidateLimit 3)
        plan = buildQueryPlan "needle" []
    runPassageFtsChannels connection plan (Set.singleton "missing") limit
      >>= (@?= Left (RetrievalIndexError PassageExactTarget))

persistBaseline :: Connection -> IO SearchMaterialization
persistBaseline connection = do
  initializeSearchSchema connection >>= (@?= Right ())
  let materialization = baselineMaterialization
  replaceSearchMaterialization connection materialization >>= (@?= Right ())
  pure materialization

baselineMaterialization :: SearchMaterialization
baselineMaterialization = mustRight (materializeCurrentSearch p303RationaleSnapshot)

passageIds :: SearchMaterialization -> Set.Set Text
passageIds = Set.fromList . map searchPassageId . searchMaterializationPassages

passageIdFor :: SectionKind -> SearchMaterialization -> Text
passageIdFor kind = searchPassageId . passageFor kind

passageFor :: SectionKind -> SearchMaterialization -> SearchPassage
passageFor kind materialization =
  case filter ((== kind) . searchPassageSectionKind) (searchMaterializationPassages materialization) of
    [passage] -> passage
    passages -> error ("expected one " <> show kind <> " passage, got " <> show (length passages))

withMemory :: (Connection -> IO value) -> IO value
withMemory = bracket (open ":memory:") close

mustRightIO :: (Show failure) => IO (Either failure value) -> IO value
mustRightIO action = mustRight <$> action

mustRight :: (Show failure) => Either failure value -> value
mustRight (Right value) = value
mustRight (Left failure) = error (show failure)
