{-# LANGUAGE OverloadedStrings #-}

module Adrai.Fixture.LargeStress
  ( largeStressV1,
    largeStressOperationCountsV1,
    largeStressDocumentCountsV1,
  )
where

import Adrai.Fixture.Prng (algorithmName, mkSeed)
import Adrai.Fixture.ProductionShape
  ( DocumentCounts,
    branchSwitchEvents,
    documentCountsFor,
  )
import Adrai.Fixture.Types

largeStressOperationCountsV1 :: OperationCounts
largeStressOperationCountsV1 =
  OperationCounts
    { createOperationCount = 900,
      amendOperationCount = 650,
      scopeOperationCount = 250,
      domainOperationCount = 100,
      obsoleteOperationCount = 100
    }

largeStressDocumentCountsV1 :: DocumentCounts
largeStressDocumentCountsV1 = documentCountsFor largeStressOperationCountsV1

largeStressV1 :: RepositoryPlan
largeStressV1 =
  RepositoryPlan
    { repositoryPlanMeta =
        FixtureMeta
          { fixtureVersion = FixtureV1,
            fixtureGenerator = algorithmName,
            fixtureSeed = mkSeed 260729,
            fixturePurpose =
              "12,000-commit lazy stress plan with 2,000 grouped semantic operations"
          },
      repositoryPlanSpec =
        RepositorySpec
          { repositorySpecName = "large-stress-v1",
            repositoryCommitCount = 12000,
            repositoryOperationCounts = largeStressOperationCountsV1,
            repositoryNoiseCommitCount = 10000,
            repositoryBranchEvents = branchSwitchEvents 12000,
            repositoryInvariants =
              [ ExactCommitCount 12000,
                ExactNoiseCommitCount 10000,
                ExactOperationCount CreateOperation 900,
                ExactOperationCount AmendOperation 650,
                ExactOperationCount ScopeOperation 250,
                ExactOperationCount DomainOperation 100,
                ExactOperationCount ObsoleteOperation 100,
                ParentsPrecedeChildren,
                BranchesDefinedBeforeUse,
                OperationsReferenceCreatedAdrs,
                BranchRevisionIsolation,
                NamedInvariant "large plan must be consumed by a strict fold"
              ]
          }
    }
