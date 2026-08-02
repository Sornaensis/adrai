module Adrai.Fixture.Types
  ( FixtureVersion (..),
    FixtureMeta (..),
    AdrKey (..),
    OperationKey (..),
    CommitOrdinal (..),
    BranchKey (..),
    AdrTemplate (..),
    OperationKind (..),
    OperationCounts (..),
    operationCountFor,
    operationCountTotal,
    PlannedOperation (..),
    PlannedCommitKind (..),
    PlannedCommit (..),
    RepositorySpec (..),
    RepositoryPlan (..),
    SourceMode (..),
    SourceTemplate (..),
    ConfidenceClass (..),
    RelevanceExpectation (..),
    RelevanceCorpus (..),
    RetrievalProbe (..),
    RetrievalScaleSpec (..),
    BranchEvent (..),
    InvariantDescriptor (..),
  )
where

import Adrai.Fixture.Prng (Seed)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

data FixtureVersion
  = FixtureV1
  deriving (Eq, Ord, Show)

data FixtureMeta = FixtureMeta
  { fixtureVersion :: FixtureVersion,
    fixtureGenerator :: Text,
    fixtureSeed :: Seed,
    fixturePurpose :: Text
  }
  deriving (Eq, Show)

newtype AdrKey = AdrKey Int
  deriving (Eq, Ord, Show)

newtype OperationKey = OperationKey Int
  deriving (Eq, Ord, Show)

newtype CommitOrdinal = CommitOrdinal Int
  deriving (Eq, Ord, Show)

newtype BranchKey = BranchKey Text
  deriving (Eq, Ord, Show)

-- | Logical ADR material used before canonical record encoding exists.
data AdrTemplate = AdrTemplate
  { adrTemplateKey :: AdrKey,
    adrTemplateTitle :: Text,
    adrTemplateSummary :: Text,
    adrTemplateBodySections :: [(Text, Text)],
    adrTemplateDomains :: [Text],
    adrTemplateScopes :: [Text]
  }
  deriving (Eq, Show)

data OperationKind
  = CreateOperation
  | AmendOperation
  | ScopeOperation
  | DomainOperation
  | ObsoleteOperation
  deriving (Bounded, Enum, Eq, Ord, Show)

data OperationCounts = OperationCounts
  { createOperationCount :: Int,
    amendOperationCount :: Int,
    scopeOperationCount :: Int,
    domainOperationCount :: Int,
    obsoleteOperationCount :: Int
  }
  deriving (Eq, Show)

operationCountFor :: OperationKind -> OperationCounts -> Int
operationCountFor CreateOperation = createOperationCount
operationCountFor AmendOperation = amendOperationCount
operationCountFor ScopeOperation = scopeOperationCount
operationCountFor DomainOperation = domainOperationCount
operationCountFor ObsoleteOperation = obsoleteOperationCount

operationCountTotal :: OperationCounts -> Int
operationCountTotal counts =
  createOperationCount counts
    + amendOperationCount counts
    + scopeOperationCount counts
    + domainOperationCount counts
    + obsoleteOperationCount counts

-- | Logical operations only. P2 and P5 later supply graph validation and Git
-- interpretation respectively.
data PlannedOperation
  = PlannedCreate AdrTemplate
  | PlannedAmend AdrKey Text
  | PlannedScope AdrKey [Text]
  | PlannedDomain AdrKey [Text]
  | PlannedObsolete AdrKey
  deriving (Eq, Show)

data PlannedCommitKind
  = PlannedNoise Text
  | PlannedSemantic OperationKey PlannedOperation
  | PlannedMerge BranchKey
  deriving (Eq, Show)

data PlannedCommit = PlannedCommit
  { plannedCommitOrdinal :: CommitOrdinal,
    plannedCommitBranch :: BranchKey,
    plannedCommitParents :: [CommitOrdinal],
    plannedCommitKind :: PlannedCommitKind
  }
  deriving (Eq, Show)

-- | Compact parameters for a repository fixture. The generated commit stream is
-- intentionally not retained in this record.
data RepositorySpec = RepositorySpec
  { repositorySpecName :: Text,
    repositoryCommitCount :: Int,
    repositoryOperationCounts :: OperationCounts,
    repositoryNoiseCommitCount :: Int,
    repositoryBranchEvents :: [BranchEvent],
    repositoryInvariants :: [InvariantDescriptor]
  }
  deriving (Eq, Show)

data RepositoryPlan = RepositoryPlan
  { repositoryPlanMeta :: FixtureMeta,
    repositoryPlanSpec :: RepositorySpec
  }
  deriving (Eq, Show)

data SourceMode
  = CommittedSource
  | WorktreeSource
  | DirtyWorktreeSource
  | UntrackedWorktreeSource
  deriving (Eq, Ord, Show)

data SourceTemplate = SourceTemplate
  { sourceTemplateKey :: Text,
    sourceTemplatePath :: Text,
    sourceTemplateBytes :: ByteString,
    sourceTemplateMode :: SourceMode
  }
  deriving (Eq, Show)

data ConfidenceClass
  = LowConfidence
  | MediumConfidence
  | HighConfidence
  deriving (Eq, Ord, Show)

data RelevanceExpectation = RelevanceExpectation
  { relevanceSourceKey :: Text,
    -- Hard negatives intentionally carry an empty expected-result set.
    relevanceExpectedAdrs :: [AdrKey],
    relevanceExpectedTopK :: Int,
    relevanceConfidenceCeiling :: Maybe ConfidenceClass,
    relevanceIsHardNegative :: Bool
  }
  deriving (Eq, Show)

data RelevanceCorpus = RelevanceCorpus
  { relevanceCorpusMeta :: FixtureMeta,
    relevanceCorpusAdrs :: NonEmpty AdrTemplate,
    relevanceCorpusSources :: NonEmpty SourceTemplate,
    relevanceCorpusExpectations :: NonEmpty RelevanceExpectation
  }
  deriving (Eq, Show)

data RetrievalProbe = RetrievalProbe
  { retrievalProbeName :: Text,
    retrievalProbeQuery :: Text,
    retrievalProbeExpectedAdrs :: [AdrKey],
    retrievalProbeTopK :: Int
  }
  deriving (Eq, Show)

data RetrievalScaleSpec = RetrievalScaleSpec
  { retrievalScaleMeta :: FixtureMeta,
    retrievalLogicalAdrCount :: Int,
    retrievalQueryProbes :: NonEmpty RetrievalProbe,
    retrievalShortlistBound :: Int,
    retrievalInvariants :: [InvariantDescriptor]
  }
  deriving (Eq, Show)

-- | Declarative branch scheduling. Interpreting these events into refs and
-- commits is intentionally deferred to the Git/service phases.
data BranchEvent
  = CreateBranchAt CommitOrdinal BranchKey CommitOrdinal
  | CheckoutBranchAt CommitOrdinal BranchKey
  | MergeBranchAt CommitOrdinal BranchKey BranchKey
  | ResetBranchAt CommitOrdinal BranchKey CommitOrdinal
  deriving (Eq, Show)

data InvariantDescriptor
  = ExactCommitCount Int
  | ExactNoiseCommitCount Int
  | ExactOperationCount OperationKind Int
  | ParentsPrecedeChildren
  | BranchesDefinedBeforeUse
  | OperationsReferenceCreatedAdrs
  | BranchRevisionIsolation
  | BoundedRetrievalCandidates Int
  | NamedInvariant Text
  deriving (Eq, Show)
