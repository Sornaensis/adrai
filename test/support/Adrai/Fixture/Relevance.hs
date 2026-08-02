{-# LANGUAGE OverloadedStrings #-}

module Adrai.Fixture.Relevance
  ( relevanceCorpusV1,
    relevanceCorpusNamespace,
    relevanceCorpusSeed,
    relevanceCorpusActor,
    relevanceTopK,
    primarySourceKeys,
    cacheAdrKey,
    authAdrKey,
    queueAdrKey,
    storageAdrKey,
    observabilityAdrKey,
    deploymentAdrKey,
  )
where

import Adrai.Fixture.Prng (Seed, algorithmName, mkSeed)
import Adrai.Fixture.Types
  ( AdrKey (..),
    AdrTemplate (..),
    ConfidenceClass (..),
    FixtureMeta (..),
    FixtureVersion (..),
    RelevanceCorpus (..),
    RelevanceExpectation (..),
    SourceMode (..),
    SourceTemplate (..),
  )
import qualified Data.ByteString as ByteString
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)

-- | Stable namespace for the Haskell-owned transcription of the six-ADR
-- relevance fixture. The Python prototype generated runtime IDs; this corpus
-- deliberately uses 'AdrKey' ordinals instead.
relevanceCorpusNamespace :: Text
relevanceCorpusNamespace = "adrai-relevance-six:v1"

-- | The seed is metadata for later deterministic materializers. The corpus
-- itself is fully declarative and does not draw random values.
relevanceCorpusSeed :: Seed
relevanceCorpusSeed = mkSeed 260729

relevanceCorpusActor :: Text
relevanceCorpusActor = "human:fixture-architect"

relevanceTopK :: Int
relevanceTopK = 3

cacheAdrKey, authAdrKey, queueAdrKey, storageAdrKey, observabilityAdrKey, deploymentAdrKey :: AdrKey
cacheAdrKey = AdrKey 1
authAdrKey = AdrKey 2
queueAdrKey = AdrKey 3
storageAdrKey = AdrKey 4
observabilityAdrKey = AdrKey 5
deploymentAdrKey = AdrKey 6

primarySourceKeys :: [Text]
primarySourceKeys =
  [ "cache-python",
    "auth-text",
    "queue-compose",
    "storage-sql",
    "observability-custom",
    "deployment-extensionless"
  ]

relevanceCorpusV1 :: RelevanceCorpus
relevanceCorpusV1 =
  RelevanceCorpus
    { relevanceCorpusMeta =
        FixtureMeta
          { fixtureVersion = FixtureV1,
            fixtureGenerator = algorithmName,
            fixtureSeed = relevanceCorpusSeed,
            fixturePurpose =
              relevanceCorpusNamespace
                <> ": actor="
                <> relevanceCorpusActor
                <> "; deterministic semantic relevance and hard-negative parity corpus"
          },
      relevanceCorpusAdrs = cacheAdr :| [authAdr, queueAdr, storageAdr, observabilityAdr, deploymentAdr],
      relevanceCorpusSources =
        cacheSource
          :| [ authSource,
               queueSource,
               storageSource,
               observabilitySource,
               deploymentSource,
               outsideScopeCacheSource,
               scopeOnlyUnrelatedSource,
               authenticationInsideCacheScopeSource,
               untrackedQueueSource,
               genericHardNegativeSource,
               emptyHardNegativeSource,
               tinyHardNegativeSource
             ],
      relevanceCorpusExpectations =
        positive "cache-python" cacheAdrKey
          :| [ positive "auth-text" authAdrKey,
               positive "queue-compose" queueAdrKey,
               positive "storage-sql" storageAdrKey,
               positive "observability-custom" observabilityAdrKey,
               positive "deployment-extensionless" deploymentAdrKey,
               positive "cache-outside-scope" cacheAdrKey,
               noHighConfidence "scope-only-unrelated",
               positive "auth-inside-cache-scope" authAdrKey,
               positive "queue-untracked-worktree" queueAdrKey,
               hardNegative "generic-hard-negative" (Just LowConfidence),
               hardNegative "empty-hard-negative" Nothing,
               hardNegative "tiny-hard-negative" Nothing
             ]
    }

cacheAdr, authAdr, queueAdr, storageAdr, observabilityAdr, deploymentAdr :: AdrTemplate
cacheAdr =
  adr
    cacheAdrKey
    "Stable cache identity"
    "Cache keys derive from durable content fingerprints and exclude workspace paths."
    "Use source-content digests, compiler ABI identifiers, and deterministic cache keys. Absolute workspace directories never participate."
    "compiler.cache"
    "src/cache/**"

authAdr =
  adr
    authAdrKey
    "Signed API authentication"
    "API requests use short-lived signed bearer tokens and explicit authorization."
    "Verify access-token signatures and expiry, rotate credentials, and authorize the caller before serving protected endpoints."
    "security.identity"
    "src/auth/**"

queueAdr =
  adr
    queueAdrKey
    "Durable queue acknowledgement"
    "Workers acknowledge persistent jobs only after successful transaction commit."
    "Use a durable task queue, idempotent handlers, bounded retries, and acknowledge only after committed processing."
    "runtime.jobs"
    "src/jobs/**"

storageAdr =
  adr
    storageAdrKey
    "Transactional SQLite persistence"
    "SQLite is the local system of record and uses WAL transactions."
    "Persist records in SQLite, enable write-ahead logging, and use atomic transactions with rollback on failure."
    "storage.sqlite"
    "src/storage/**"

observabilityAdr =
  adr
    observabilityAdrKey
    "Structured request telemetry"
    "Requests carry correlation identifiers through structured logs, metrics, and traces."
    "Propagate request and trace identifiers, emit structured telemetry, and record latency metrics for every service boundary."
    "observability.telemetry"
    "src/telemetry/**"

deploymentAdr =
  adr
    deploymentAdrKey
    "Gradual container deployment"
    "Containers roll out gradually with readiness checks and rollback."
    "Use staged Kubernetes deployment, health and readiness probes, and automatic rollback when the rollout fails."
    "delivery.deployment"
    "ops/deploy/**"

adr :: AdrKey -> Text -> Text -> Text -> Text -> Text -> AdrTemplate
adr key title summary decision domain scope =
  AdrTemplate
    { adrTemplateKey = key,
      adrTemplateTitle = title,
      adrTemplateSummary = summary,
      adrTemplateBodySections = [("Decision", decision)],
      adrTemplateDomains = [domain],
      adrTemplateScopes = [scope]
    }

cacheSource, authSource, queueSource, storageSource, observabilitySource, deploymentSource :: SourceTemplate
cacheSource =
  committed
    "cache-python"
    "src/cache/key.py"
    "cache_key = sha256(source_digest + compiler_abi)\n# never include workspace path\n"

authSource =
  committed
    "auth-text"
    "notes/security.txt"
    "Protected API calls verify signed short lived bearer token credentials, expiry, and authorization.\n"

queueSource =
  committed
    "queue-compose"
    "worker.compose"
    "worker consumes persistent task queue; retry idempotently; acknowledge only after transaction commit\n"

storageSource =
  committed
    "storage-sql"
    "schema.sql"
    "-- SQLite persistent system of record using WAL and atomic rollback transactions\nPRAGMA journal_mode=WAL; CREATE TABLE tasks(id TEXT PRIMARY KEY);\n"

observabilitySource =
  committed
    "observability-custom"
    "telemetry.custom"
    "propagate request_id and trace_id; emit structured logs, latency metrics, and distributed traces\n"

deploymentSource =
  committed
    "deployment-extensionless"
    "deployment"
    "kubernetes staged container rollout with readiness health probe and automatic rollback\n"

outsideScopeCacheSource, scopeOnlyUnrelatedSource, authenticationInsideCacheScopeSource :: SourceTemplate
outsideScopeCacheSource =
  committed
    "cache-outside-scope"
    "outside/contracts/cache-policy.prose"
    "Derive deterministic build cache identifiers from source content digests and compiler ABI; exclude workspace directories."

scopeOnlyUnrelatedSource =
  committed
    "scope-only-unrelated"
    "src/cache/unrelated.py"
    "A poem about weather, gardens, colors, windows, and afternoon tea.\n"

authenticationInsideCacheScopeSource =
  committed
    "auth-inside-cache-scope"
    "src/cache/authentication-inside-cache-path.txt"
    "verify signed bearer token signature expiry credentials and authorize the protected API caller\n"

untrackedQueueSource :: SourceTemplate
untrackedQueueSource =
  source
    "queue-untracked-worktree"
    "untracked-relevance.txt"
    "durable queue worker retries and acknowledges only after transaction commit"
    UntrackedWorktreeSource

genericHardNegativeSource, emptyHardNegativeSource, tinyHardNegativeSource :: SourceTemplate
genericHardNegativeSource =
  committedBytes
    "generic-hard-negative"
    "logs/noise.log"
    (ByteString.concat (replicate 500 genericNoiseUnit))

emptyHardNegativeSource =
  committed "empty-hard-negative" "empty.txt" " \n\t\n"

tinyHardNegativeSource =
  committed "tiny-hard-negative" "tiny.txt" "x = 1\n"

genericNoiseUnit :: ByteString.ByteString
genericNoiseUnit =
  "2026-07-31 INFO helper started value=17 option=true\n\
  \generic technical component interface data system service module\n"

committed :: Text -> Text -> ByteString.ByteString -> SourceTemplate
committed key path bytes = committedBytes key path bytes

committedBytes :: Text -> Text -> ByteString.ByteString -> SourceTemplate
committedBytes key path bytes = source key path bytes CommittedSource

source :: Text -> Text -> ByteString.ByteString -> SourceMode -> SourceTemplate
source key path bytes mode =
  SourceTemplate
    { sourceTemplateKey = key,
      sourceTemplatePath = path,
      sourceTemplateBytes = bytes,
      sourceTemplateMode = mode
    }

positive :: Text -> AdrKey -> RelevanceExpectation
positive sourceKey expected =
  RelevanceExpectation
    { relevanceSourceKey = sourceKey,
      relevanceExpectedAdrs = [expected],
      relevanceExpectedTopK = relevanceTopK,
      relevanceConfidenceCeiling = Nothing,
      relevanceIsHardNegative = False
    }

noHighConfidence :: Text -> RelevanceExpectation
noHighConfidence sourceKey =
  RelevanceExpectation
    { relevanceSourceKey = sourceKey,
      relevanceExpectedAdrs = [],
      relevanceExpectedTopK = 10,
      relevanceConfidenceCeiling = Just MediumConfidence,
      relevanceIsHardNegative = False
    }

hardNegative :: Text -> Maybe ConfidenceClass -> RelevanceExpectation
hardNegative sourceKey confidenceCeiling =
  RelevanceExpectation
    { relevanceSourceKey = sourceKey,
      relevanceExpectedAdrs = [],
      relevanceExpectedTopK = 10,
      relevanceConfidenceCeiling = confidenceCeiling,
      relevanceIsHardNegative = True
    }
