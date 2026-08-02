{-# LANGUAGE OverloadedStrings #-}

module Adrai.FixtureRelevanceTest (tests) where

import Adrai.Fixture.Prng (algorithmName, seedWord64)
import Adrai.Fixture.Relevance
  ( authAdrKey,
    cacheAdrKey,
    deploymentAdrKey,
    observabilityAdrKey,
    primarySourceKeys,
    queueAdrKey,
    relevanceCorpusActor,
    relevanceCorpusNamespace,
    relevanceCorpusSeed,
    relevanceCorpusV1,
    relevanceTopK,
    storageAdrKey,
  )
import Adrai.Fixture.Types
  ( AdrKey,
    AdrTemplate (..),
    ConfidenceClass (..),
    FixtureMeta (..),
    RelevanceCorpus (..),
    RelevanceExpectation (..),
    SourceMode (..),
    SourceTemplate (..),
  )
import qualified Data.ByteString as ByteString
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "six-ADR relevance fixture"
    [ metadataTests,
      adrTests,
      expectationTests,
      sourceTests
    ]

metadataTests :: TestTree
metadataTests =
  testCase "records stable namespace, generator, seed, actor, and purpose" $ do
    relevanceCorpusNamespace @?= "adrai-relevance-six:v1"
    relevanceCorpusActor @?= "human:fixture-architect"
    seedWord64 relevanceCorpusSeed @?= 260729
    let meta = relevanceCorpusMeta relevanceCorpusV1
    fixtureGenerator meta @?= algorithmName
    fixtureSeed meta @?= relevanceCorpusSeed
    assertBool "purpose names the stable namespace" (relevanceCorpusNamespace `Text.isPrefixOf` fixturePurpose meta)
    assertBool "purpose records the fixture actor" (relevanceCorpusActor `Text.isInfixOf` fixturePurpose meta)

adrTests :: TestTree
adrTests =
  testGroup
    "ADR templates"
    [ testCase "contains exactly six unique Haskell-owned ordinal keys" $ do
        let templates = NonEmpty.toList (relevanceCorpusAdrs relevanceCorpusV1)
            keys = fmap adrTemplateKey templates
        length templates @?= 6
        Set.size (Set.fromList keys) @?= 6
        keys @?= [cacheAdrKey, authAdrKey, queueAdrKey, storageAdrKey, observabilityAdrKey, deploymentAdrKey],
      testCase "preserves exact titles, content, domains, and scopes" $
        fmap adrContract (NonEmpty.toList (relevanceCorpusAdrs relevanceCorpusV1))
          @?= expectedAdrContracts
    ]

expectationTests :: TestTree
expectationTests =
  testGroup
    "expectations"
    [ testCase "pins all thirteen ordered expectation contracts" $
        fmap expectationContract (NonEmpty.toList (relevanceCorpusExpectations relevanceCorpusV1))
          @?= expectedExpectationContracts,
      testCase "positive expectations reference known ADRs within top three" $ do
        let known = Set.fromList (fmap adrTemplateKey (NonEmpty.toList (relevanceCorpusAdrs relevanceCorpusV1)))
            expectations = NonEmpty.toList (relevanceCorpusExpectations relevanceCorpusV1)
            positives = filter (not . null . relevanceExpectedAdrs) expectations
        assertBool "has positive cases" (not (null positives))
        assertBool
          "all positive keys are defined"
          (all (all (`Set.member` known) . relevanceExpectedAdrs) positives)
        assertBool
          "all positives use the stable top-three oracle"
          (all ((== relevanceTopK) . relevanceExpectedTopK) positives),
      testCase "hard negatives never claim a positive ADR" $ do
        let negatives =
              filter
                relevanceIsHardNegative
                (NonEmpty.toList (relevanceCorpusExpectations relevanceCorpusV1))
        assertBool "has hard negatives" (not (null negatives))
        assertBool "hard negatives have no positives" (all (null . relevanceExpectedAdrs) negatives)
        fmap expectationContract negatives
          @?= [ ("generic-hard-negative", [], 10, Just LowConfidence, True),
                ("empty-hard-negative", [], 10, Nothing, True),
                ("tiny-hard-negative", [], 10, Nothing, True)
              ],
      testCase "every expectation references a defined source" $ do
        let sourceKeys = Set.fromList (fmap sourceTemplateKey (NonEmpty.toList (relevanceCorpusSources relevanceCorpusV1)))
            expectedSourceKeys = fmap relevanceSourceKey (NonEmpty.toList (relevanceCorpusExpectations relevanceCorpusV1))
        assertBool "expectation source keys are defined" (all (`Set.member` sourceKeys) expectedSourceKeys)
    ]

sourceTests :: TestTree
sourceTests =
  testGroup
    "source corpus"
    [ testCase "pins all thirteen ordered source templates" $
        fmap sourceContract (NonEmpty.toList (relevanceCorpusSources relevanceCorpusV1))
          @?= expectedSourceContracts,
      testCase "primary cases preserve format variety including extensionless input" $ do
        let primarySources = take 6 (NonEmpty.toList (relevanceCorpusSources relevanceCorpusV1))
            primaryPaths = fmap sourceTemplatePath primarySources
        fmap sourceTemplateKey primarySources @?= primarySourceKeys
        primaryPaths
          @?= [ "src/cache/key.py",
                "notes/security.txt",
                "worker.compose",
                "schema.sql",
                "telemetry.custom",
                "deployment"
              ]
        assertBool "deployment is extensionless" (not (Text.any (== '.') "deployment"))
    ]

adrContract :: AdrTemplate -> (Text, Text, [(Text, Text)], [Text], [Text])
adrContract template =
  ( adrTemplateTitle template,
    adrTemplateSummary template,
    adrTemplateBodySections template,
    adrTemplateDomains template,
    adrTemplateScopes template
  )

expectedAdrContracts :: [(Text, Text, [(Text, Text)], [Text], [Text])]
expectedAdrContracts =
  [ ( "Stable cache identity",
      "Cache keys derive from durable content fingerprints and exclude workspace paths.",
      [("Decision", "Use source-content digests, compiler ABI identifiers, and deterministic cache keys. Absolute workspace directories never participate.")],
      ["compiler.cache"],
      ["src/cache/**"]
    ),
    ( "Signed API authentication",
      "API requests use short-lived signed bearer tokens and explicit authorization.",
      [("Decision", "Verify access-token signatures and expiry, rotate credentials, and authorize the caller before serving protected endpoints.")],
      ["security.identity"],
      ["src/auth/**"]
    ),
    ( "Durable queue acknowledgement",
      "Workers acknowledge persistent jobs only after successful transaction commit.",
      [("Decision", "Use a durable task queue, idempotent handlers, bounded retries, and acknowledge only after committed processing.")],
      ["runtime.jobs"],
      ["src/jobs/**"]
    ),
    ( "Transactional SQLite persistence",
      "SQLite is the local system of record and uses WAL transactions.",
      [("Decision", "Persist records in SQLite, enable write-ahead logging, and use atomic transactions with rollback on failure.")],
      ["storage.sqlite"],
      ["src/storage/**"]
    ),
    ( "Structured request telemetry",
      "Requests carry correlation identifiers through structured logs, metrics, and traces.",
      [("Decision", "Propagate request and trace identifiers, emit structured telemetry, and record latency metrics for every service boundary.")],
      ["observability.telemetry"],
      ["src/telemetry/**"]
    ),
    ( "Gradual container deployment",
      "Containers roll out gradually with readiness checks and rollback.",
      [("Decision", "Use staged Kubernetes deployment, health and readiness probes, and automatic rollback when the rollout fails.")],
      ["delivery.deployment"],
      ["ops/deploy/**"]
    )
  ]

sourceContract :: SourceTemplate -> (Text, Text, ByteString.ByteString, SourceMode)
sourceContract sourceTemplate =
  ( sourceTemplateKey sourceTemplate,
    sourceTemplatePath sourceTemplate,
    sourceTemplateBytes sourceTemplate,
    sourceTemplateMode sourceTemplate
  )

expectedSourceContracts :: [(Text, Text, ByteString.ByteString, SourceMode)]
expectedSourceContracts =
  [ ( "cache-python",
      "src/cache/key.py",
      "cache_key = sha256(source_digest + compiler_abi)\n# never include workspace path\n",
      CommittedSource
    ),
    ( "auth-text",
      "notes/security.txt",
      "Protected API calls verify signed short lived bearer token credentials, expiry, and authorization.\n",
      CommittedSource
    ),
    ( "queue-compose",
      "worker.compose",
      "worker consumes persistent task queue; retry idempotently; acknowledge only after transaction commit\n",
      CommittedSource
    ),
    ( "storage-sql",
      "schema.sql",
      "-- SQLite persistent system of record using WAL and atomic rollback transactions\nPRAGMA journal_mode=WAL; CREATE TABLE tasks(id TEXT PRIMARY KEY);\n",
      CommittedSource
    ),
    ( "observability-custom",
      "telemetry.custom",
      "propagate request_id and trace_id; emit structured logs, latency metrics, and distributed traces\n",
      CommittedSource
    ),
    ( "deployment-extensionless",
      "deployment",
      "kubernetes staged container rollout with readiness health probe and automatic rollback\n",
      CommittedSource
    ),
    ( "cache-outside-scope",
      "outside/contracts/cache-policy.prose",
      "Derive deterministic build cache identifiers from source content digests and compiler ABI; exclude workspace directories.",
      CommittedSource
    ),
    ( "scope-only-unrelated",
      "src/cache/unrelated.py",
      "A poem about weather, gardens, colors, windows, and afternoon tea.\n",
      CommittedSource
    ),
    ( "auth-inside-cache-scope",
      "src/cache/authentication-inside-cache-path.txt",
      "verify signed bearer token signature expiry credentials and authorize the protected API caller\n",
      CommittedSource
    ),
    ( "queue-untracked-worktree",
      "untracked-relevance.txt",
      "durable queue worker retries and acknowledges only after transaction commit",
      UntrackedWorktreeSource
    ),
    ( "generic-hard-negative",
      "logs/noise.log",
      ByteString.concat (replicate 500 expectedGenericNoiseUnit),
      CommittedSource
    ),
    ( "empty-hard-negative",
      "empty.txt",
      " \n\t\n",
      CommittedSource
    ),
    ( "tiny-hard-negative",
      "tiny.txt",
      "x = 1\n",
      CommittedSource
    )
  ]

expectedGenericNoiseUnit :: ByteString.ByteString
expectedGenericNoiseUnit =
  "2026-07-31 INFO helper started value=17 option=true\n\
  \generic technical component interface data system service module\n"

expectationContract :: RelevanceExpectation -> (Text, [AdrKey], Int, Maybe ConfidenceClass, Bool)
expectationContract expectation =
  ( relevanceSourceKey expectation,
    relevanceExpectedAdrs expectation,
    relevanceExpectedTopK expectation,
    relevanceConfidenceCeiling expectation,
    relevanceIsHardNegative expectation
  )

expectedExpectationContracts :: [(Text, [AdrKey], Int, Maybe ConfidenceClass, Bool)]
expectedExpectationContracts =
  [ ("cache-python", [cacheAdrKey], 3, Nothing, False),
    ("auth-text", [authAdrKey], 3, Nothing, False),
    ("queue-compose", [queueAdrKey], 3, Nothing, False),
    ("storage-sql", [storageAdrKey], 3, Nothing, False),
    ("observability-custom", [observabilityAdrKey], 3, Nothing, False),
    ("deployment-extensionless", [deploymentAdrKey], 3, Nothing, False),
    ("cache-outside-scope", [cacheAdrKey], 3, Nothing, False),
    ("scope-only-unrelated", [], 10, Just MediumConfidence, False),
    ("auth-inside-cache-scope", [authAdrKey], 3, Nothing, False),
    ("queue-untracked-worktree", [queueAdrKey], 3, Nothing, False),
    ("generic-hard-negative", [], 10, Just LowConfidence, True),
    ("empty-hard-negative", [], 10, Nothing, True),
    ("tiny-hard-negative", [], 10, Nothing, True)
  ]
