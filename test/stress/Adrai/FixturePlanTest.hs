{-# LANGUAGE OverloadedStrings #-}

module Adrai.FixturePlanTest (tests) where

import Adrai.Fixture.LargeStress
import Adrai.Fixture.Prng (seedWord64)
import Adrai.Fixture.ProductionShape
import Adrai.Fixture.RetrievalScale
import Adrai.Fixture.Types
import Data.List (findIndices)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "fixture plans"
    [ testCase "production profile has exact counts and seed" $ do
        profile productionShapeV1 @?= (2000, 400, 1600, 260729)
        productionOperationCountsV1 @?= OperationCounts 200 100 50 25 25,
      testCase "production document counts are exact" $
        productionDocumentCountsV1 @?= DocumentCounts 300 800 1100,
      testCase "large profile has exact counts and seed" $ do
        profile largeStressV1 @?= (12000, 2000, 10000, 260729)
        largeStressOperationCountsV1 @?= OperationCounts 900 650 250 100 100,
      testCase "large document counts are exact" $
        largeStressDocumentCountsV1 @?= DocumentCounts 1550 3800 5350,
      testCase "production steps have exact semantic, noise, and per-operation counts" $
        countRepositorySteps productionShapeV1
          @?= StepCounts 400 1600 200 100 50 25 25,
      testCase "large steps have exact semantic, noise, and per-operation counts" $
        countRepositorySteps largeStressV1
          @?= StepCounts 2000 10000 900 650 250 100 100,
      testCase "integer due scheduling places production and large operations every fifth and sixth commit" $ do
        take 4 (semanticOrdinals productionShapeV1) @?= [5, 10, 15, 20]
        take 4 (semanticOrdinals largeStressV1) @?= [6, 12, 18, 24],
      testCase "ordinals and linear parents are valid" $ do
        let (count, valid) = foldRepositoryPlan checkParents (0, True) largeStressV1
        count @?= 12000
        assertBool "every parent precedes its child" valid,
      testCase "production branch schedule pins all twelve switches and branch invariants" $ do
        repositoryBranchEvents (repositoryPlanSpec productionShapeV1)
          @?= productionBranchEvents
        assertBool
          "branch descriptors retain definition and isolation invariants"
          ( all
              (`elem` repositoryInvariants (repositoryPlanSpec productionShapeV1))
              [BranchesDefinedBeforeUse, BranchRevisionIsolation]
          ),
      testCase "large branch schedule pins all twelve switches and ordinals" $
        repositoryBranchEvents (repositoryPlanSpec largeStressV1)
          @?= largeBranchEvents,
      testCase "retrieval scale is separate, indexed, and has exact topic counts" $ do
        retrievalLogicalAdrCount retrievalScaleV1 @?= 2000
        retrievalOperationCountsV1 @?= OperationCounts 2000 4 4 0 2
        retrievalTopicCountsV1 @?= [("cache identity", 667), ("trace propagation", 667), ("job delivery", 666)]
        fmap adrTemplateKey (adrTemplateAt 0) @?= Just (AdrKey 0)
        fmap adrTemplateKey (adrTemplateAt 1999) @?= Just (AdrKey 1999)
        adrTemplateAt 2000 @?= Nothing
        adrTemplateAt (-1) @?= Nothing,
      testCase "retrieval tail template preserves the untruncated index" $
        fmap adrTemplateTitle (adrTemplateAt 1999)
          @?= Just "Decision 1999: trace propagation"
    ]

profile :: RepositoryPlan -> (Int, Int, Int, Integer)
profile plan =
  ( repositoryCommitCount spec,
    operationCountTotal (repositoryOperationCounts spec),
    repositoryNoiseCommitCount spec,
    fromIntegral (seedWord64 (fixtureSeed (repositoryPlanMeta plan)))
  )
  where
    spec = repositoryPlanSpec plan

semanticOrdinals :: RepositoryPlan -> [Int]
semanticOrdinals =
  map (+ 1)
    . findIndices isSemantic
    . repositorySteps
  where
    isSemantic commit =
      case plannedCommitKind commit of
        PlannedSemantic _ _ -> True
        _ -> False

checkParents :: (Int, Bool) -> PlannedCommit -> (Int, Bool)
checkParents (count, valid) commit =
  let CommitOrdinal ordinal = plannedCommitOrdinal commit
      parentsValid = all (\(CommitOrdinal parent) -> parent < ordinal) (plannedCommitParents commit)
      expectedParents = if ordinal == 1 then [] else [CommitOrdinal (ordinal - 1)]
   in (count + 1, valid && parentsValid && plannedCommitParents commit == expectedParents)

data StepCounts = StepCounts
  { countedSemantic :: !Int,
    countedNoise :: !Int,
    countedCreates :: !Int,
    countedAmendments :: !Int,
    countedScopes :: !Int,
    countedDomains :: !Int,
    countedObsoletions :: !Int
  }
  deriving (Eq, Show)

countRepositorySteps :: RepositoryPlan -> StepCounts
countRepositorySteps = foldRepositoryPlan countStep (StepCounts 0 0 0 0 0 0 0)

countStep :: StepCounts -> PlannedCommit -> StepCounts
countStep counts commit =
  case plannedCommitKind commit of
    PlannedNoise _ -> counts {countedNoise = countedNoise counts + 1}
    PlannedMerge _ -> counts
    PlannedSemantic _ operation -> countOperation counts operation

countOperation :: StepCounts -> PlannedOperation -> StepCounts
countOperation counts operation =
  case operation of
    PlannedCreate _ ->
      semantic (counts {countedCreates = countedCreates counts + 1})
    PlannedAmend _ _ ->
      semantic (counts {countedAmendments = countedAmendments counts + 1})
    PlannedScope _ _ ->
      semantic (counts {countedScopes = countedScopes counts + 1})
    PlannedDomain _ _ ->
      semantic (counts {countedDomains = countedDomains counts + 1})
    PlannedObsolete _ ->
      semantic (counts {countedObsoletions = countedObsoletions counts + 1})
  where
    semantic value = value {countedSemantic = countedSemantic value + 1}

productionBranchEvents :: [BranchEvent]
productionBranchEvents =
  [ CreateBranchAt (CommitOrdinal 2000) featureBranch (CommitOrdinal 2000),
    CheckoutBranchAt (CommitOrdinal 2001) featureBranch,
    CheckoutBranchAt (CommitOrdinal 2002) mainBranch,
    CheckoutBranchAt (CommitOrdinal 2003) featureBranch,
    CheckoutBranchAt (CommitOrdinal 2004) mainBranch,
    CheckoutBranchAt (CommitOrdinal 2005) featureBranch,
    CheckoutBranchAt (CommitOrdinal 2006) mainBranch,
    CheckoutBranchAt (CommitOrdinal 2007) featureBranch,
    CheckoutBranchAt (CommitOrdinal 2008) mainBranch,
    CheckoutBranchAt (CommitOrdinal 2009) featureBranch,
    CheckoutBranchAt (CommitOrdinal 2010) mainBranch,
    CheckoutBranchAt (CommitOrdinal 2011) featureBranch,
    CheckoutBranchAt (CommitOrdinal 2012) mainBranch
  ]

largeBranchEvents :: [BranchEvent]
largeBranchEvents =
  [ CreateBranchAt (CommitOrdinal 12000) featureBranch (CommitOrdinal 12000),
    CheckoutBranchAt (CommitOrdinal 12001) featureBranch,
    CheckoutBranchAt (CommitOrdinal 12002) mainBranch,
    CheckoutBranchAt (CommitOrdinal 12003) featureBranch,
    CheckoutBranchAt (CommitOrdinal 12004) mainBranch,
    CheckoutBranchAt (CommitOrdinal 12005) featureBranch,
    CheckoutBranchAt (CommitOrdinal 12006) mainBranch,
    CheckoutBranchAt (CommitOrdinal 12007) featureBranch,
    CheckoutBranchAt (CommitOrdinal 12008) mainBranch,
    CheckoutBranchAt (CommitOrdinal 12009) featureBranch,
    CheckoutBranchAt (CommitOrdinal 12010) mainBranch,
    CheckoutBranchAt (CommitOrdinal 12011) featureBranch,
    CheckoutBranchAt (CommitOrdinal 12012) mainBranch
  ]

featureBranch :: BranchKey
featureBranch = BranchKey "stress/feature-adrs"

mainBranch :: BranchKey
mainBranch = BranchKey "main"
