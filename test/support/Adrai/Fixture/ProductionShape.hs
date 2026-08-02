{-# LANGUAGE OverloadedStrings #-}

module Adrai.Fixture.ProductionShape
  ( DocumentCounts (..),
    productionShapeV1,
    productionOperationCountsV1,
    productionDocumentCountsV1,
    documentCountsFor,
    repositorySteps,
    foldRepositoryPlan,
    branchSwitchEvents,
    withRepositorySeed,
    noiseCompatibilityNote,
  )
where

import Adrai.Fixture.Prng
  ( Gen,
    Seed,
    algorithmName,
    mkGen,
    mkSeed,
    nextWord64,
  )
import Adrai.Fixture.Types
import Data.Text (Text)
import qualified Data.Text as Text

data DocumentCounts = DocumentCounts
  { decisionDocumentCount :: Int,
    connectionDocumentCount :: Int,
    semanticDocumentCount :: Int
  }
  deriving (Eq, Show)

productionOperationCountsV1 :: OperationCounts
productionOperationCountsV1 =
  OperationCounts
    { createOperationCount = 200,
      amendOperationCount = 100,
      scopeOperationCount = 50,
      domainOperationCount = 25,
      obsoleteOperationCount = 25
    }

productionDocumentCountsV1 :: DocumentCounts
productionDocumentCountsV1 = documentCountsFor productionOperationCountsV1

productionShapeV1 :: RepositoryPlan
productionShapeV1 =
  RepositoryPlan
    { repositoryPlanMeta =
        FixtureMeta
          { fixtureVersion = FixtureV1,
            fixtureGenerator = algorithmName,
            fixtureSeed = mkSeed 260729,
            fixturePurpose =
              "2,000-commit production shape; Haskell SplitMix noise is not Python byte-compatible"
          },
      repositoryPlanSpec =
        RepositorySpec
          { repositorySpecName = "production-shape-v1",
            repositoryCommitCount = 2000,
            repositoryOperationCounts = productionOperationCountsV1,
            repositoryNoiseCommitCount = 1600,
            repositoryBranchEvents = branchSwitchEvents 2000,
            repositoryInvariants = invariantsFor 2000 1600 productionOperationCountsV1
          }
    }

documentCountsFor :: OperationCounts -> DocumentCounts
documentCountsFor counts =
  DocumentCounts
    { decisionDocumentCount = creates + amendments,
      connectionDocumentCount = 3 * creates + amendments + scopes + domains + obsoletions,
      semanticDocumentCount = 4 * creates + 2 * amendments + scopes + domains + obsoletions
    }
  where
    creates = createOperationCount counts
    amendments = amendOperationCount counts
    scopes = scopeOperationCount counts
    domains = domainOperationCount counts
    obsoletions = obsoleteOperationCount counts

-- | Lazily interpret a compact repository plan. Operations remain grouped in
-- create/amend/scope/domain/obsolete order and are spread with
-- @due = floor(commitOrdinal * operationCount / commitCount)@.
repositorySteps :: RepositoryPlan -> [PlannedCommit]
repositorySteps plan
  | total <= 0 = []
  | otherwise = go 1 0 0 (mkGen (fixtureSeed (repositoryPlanMeta plan))) operationPlan
  where
    spec = repositoryPlanSpec plan
    total = repositoryCommitCount spec
    counts = repositoryOperationCounts spec
    totalOperations = operationCountTotal counts
    operationPlan = plannedOperations counts
    mainBranch = BranchKey "main"

    go :: Int -> Int -> Int -> Gen -> [(OperationKey, PlannedOperation)] -> [PlannedCommit]
    go ordinal emitted noiseIndex gen remaining
      | ordinal > total = []
      | emitted < due =
          case remaining of
            [] -> noiseCommit ordinal noiseIndex gen remaining
            ((operationKey, operation) : rest) ->
              PlannedCommit
                { plannedCommitOrdinal = CommitOrdinal ordinal,
                  plannedCommitBranch = mainBranch,
                  plannedCommitParents = parentsFor ordinal,
                  plannedCommitKind = PlannedSemantic operationKey operation
                }
                : go (ordinal + 1) (emitted + 1) noiseIndex gen rest
      | otherwise = noiseCommit ordinal noiseIndex gen remaining
      where
        due = (ordinal * totalOperations) `div` total

        noiseCommit current currentNoise currentGen rest =
          let (word, nextGen) = nextWord64 currentGen
              nextNoise = currentNoise + 1
              label =
                algorithmName
                  <> "/noise-"
                  <> Text.pack (show nextNoise)
                  <> "-"
                  <> Text.pack (show word)
           in PlannedCommit
                { plannedCommitOrdinal = CommitOrdinal current,
                  plannedCommitBranch = mainBranch,
                  plannedCommitParents = parentsFor current,
                  plannedCommitKind = PlannedNoise label
                }
                : go (current + 1) emitted nextNoise nextGen rest

-- | Strictly consume the lazy stream, allowing large plans to be checked without
-- retaining every commit.
foldRepositoryPlan :: (accumulator -> PlannedCommit -> accumulator) -> accumulator -> RepositoryPlan -> accumulator
foldRepositoryPlan step initial = foldl' step initial . repositorySteps

withRepositorySeed :: Seed -> RepositoryPlan -> RepositoryPlan
withRepositorySeed seed plan =
  plan
    { repositoryPlanMeta =
        (repositoryPlanMeta plan)
          { fixtureSeed = seed
          }
    }

-- | The branch-consistency benchmark creates one divergent branch and performs
-- twelve alternating checkouts after the linear baseline.
branchSwitchEvents :: Int -> [BranchEvent]
branchSwitchEvents baseline =
  CreateBranchAt base feature base
    : zipWith checkout [1 .. 12] (cycle [feature, mainBranch])
  where
    base = CommitOrdinal baseline
    feature = BranchKey "stress/feature-adrs"
    mainBranch = BranchKey "main"
    checkout offset branch = CheckoutBranchAt (CommitOrdinal (baseline + offset)) branch

noiseCompatibilityNote :: Text
noiseCompatibilityNote =
  "Noise is Haskell-owned adrai-fixture-splitmix64/v1; no Python random.Random byte-sequence parity is claimed."

parentsFor :: Int -> [CommitOrdinal]
parentsFor 1 = []
parentsFor ordinal = [CommitOrdinal (ordinal - 1)]

plannedOperations :: OperationCounts -> [(OperationKey, PlannedOperation)]
plannedOperations counts = zipWith keyed [1 ..] operations
  where
    creates = createOperationCount counts
    amendments = amendOperationCount counts
    scopes = scopeOperationCount counts
    domains = domainOperationCount counts
    obsoletions = obsoleteOperationCount counts
    operations =
      [PlannedCreate (productionAdrTemplate index) | index <- [0 .. creates - 1]]
        <> [ PlannedAmend (targetAt creates cursor) ("production amendment " <> number (cursor + 1))
             | cursor <- [0 .. amendments - 1]
           ]
        <> [ PlannedScope
               target
               [ "tests/"
                   <> Text.replace "." "/" (domainFor target)
                   <> "/revision_"
                   <> number4 (creates + amendments + cursor + 1)
                   <> "/**"
               ]
             | cursor <- [0 .. scopes - 1],
               let target = targetAt creates cursor
           ]
        <> [ PlannedDomain
               target
               [ domainFor target
                   <> ".refinement-"
                   <> number5 (creates + amendments + scopes + cursor + 1)
               ]
             | cursor <- [0 .. domains - 1],
               let target = targetAt creates cursor
           ]
        <> [PlannedObsolete (targetAt creates cursor) | cursor <- [0 .. obsoletions - 1]]
    keyed key operation = (OperationKey key, operation)

targetAt :: Int -> Int -> AdrKey
targetAt creates cursor
  | creates <= 0 = AdrKey 0
  | otherwise = AdrKey ((cursor * 37) `mod` creates)

productionAdrTemplate :: Int -> AdrTemplate
productionAdrTemplate index =
  AdrTemplate
    { adrTemplateKey = key,
      adrTemplateTitle = titlePrefix <> " " <> number4 creationIndex,
      adrTemplateSummary =
        titlePrefix
          <> " is governed by deterministic semantic inputs, explicit ownership, and observable failure behavior for "
          <> component
          <> ".",
      adrTemplateBodySections =
        [ ("Context", component <> " participates in a large multi-service product repository."),
          ("Decision", "Use a deterministic " <> domain <> " contract with versioned inputs and bounded retries."),
          ("Consequences", "Operational tooling can reconstruct why the decision applied and which commit introduced it.")
        ],
      adrTemplateDomains = [domain],
      adrTemplateScopes = ["src/" <> rootDomain domain <> "/" <> component <> "/**"]
    }
  where
    key = AdrKey index
    creationIndex = index + 1
    (domain, titlePrefix) = productionCategories !! (index `mod` length productionCategories)
    component = "component_" <> number4 creationIndex

productionCategories :: [(Text, Text)]
productionCategories =
  [ ("compiler", "Compiler architecture"),
    ("runtime.jobs", "Durable background job acknowledgement"),
    ("api", "Service API architecture"),
    ("persistence.events", "Append-only domain event storage"),
    ("security", "Security architecture"),
    ("observability.telemetry", "Structured telemetry propagation"),
    ("ui", "Client UI architecture"),
    ("delivery.pipeline", "Hermetic deployment pipeline"),
    ("data", "Data architecture"),
    ("platform.storage", "Tenant-isolated object storage")
  ]

domainFor :: AdrKey -> Text
domainFor (AdrKey index) = fst (productionCategories !! (index `mod` length productionCategories))

rootDomain :: Text -> Text
rootDomain = Text.takeWhile (/= '.')

number :: Int -> Text
number = Text.pack . show

number4 :: Int -> Text
number4 = padded 4

number5 :: Int -> Text
number5 = padded 5

padded :: Int -> Int -> Text
padded width value = Text.replicate (max 0 (width - Text.length digits)) "0" <> digits
  where
    digits = number value

invariantsFor :: Int -> Int -> OperationCounts -> [InvariantDescriptor]
invariantsFor total noise counts =
  [ ExactCommitCount total,
    ExactNoiseCommitCount noise,
    ExactOperationCount CreateOperation (createOperationCount counts),
    ExactOperationCount AmendOperation (amendOperationCount counts),
    ExactOperationCount ScopeOperation (scopeOperationCount counts),
    ExactOperationCount DomainOperation (domainOperationCount counts),
    ExactOperationCount ObsoleteOperation (obsoleteOperationCount counts),
    ParentsPrecedeChildren,
    BranchesDefinedBeforeUse,
    OperationsReferenceCreatedAdrs,
    BranchRevisionIsolation,
    NamedInvariant "semantic document counts are derived without materialization"
  ]
