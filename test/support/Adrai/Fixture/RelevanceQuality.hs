{-# LANGUAGE OverloadedStrings #-}

module Adrai.Fixture.RelevanceQuality
  ( relevanceQualityMeta,
    relevanceQualityAdrs,
    baseQualityAdrs,
    configQualityAdrs,
    correlationQualityAdrs,
    relevanceQualitySources,
    literalQueueSourceKey,
    renamedQueueSourceKey,
    largeRegionSourceKey,
    configComposeSourceKey,
    leaseTokenSourceKey,
    terseCorrelationSourceKey,
    configAdrKey,
    healthAdrKey,
    secretsAdrKey,
    correlationAdrKey,
  )
where

import Adrai.Fixture.Relevance (relevanceCorpusV1)
import Adrai.Fixture.Types
  ( AdrKey (..),
    AdrTemplate (..),
    FixtureMeta,
    RelevanceCorpus (..),
    SourceMode (CommittedSource),
    SourceTemplate (..),
  )
import qualified Data.ByteString as ByteString
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)

relevanceQualityMeta :: FixtureMeta
relevanceQualityMeta = relevanceCorpusMeta relevanceCorpusV1

relevanceQualityAdrs :: NonEmpty AdrTemplate
relevanceQualityAdrs = appendAdrs baseQualityAdrs [configAdr, healthAdr, secretsAdr, correlationAdr]

baseQualityAdrs :: NonEmpty AdrTemplate
baseQualityAdrs = relevanceCorpusAdrs relevanceCorpusV1

configQualityAdrs :: NonEmpty AdrTemplate
configQualityAdrs = appendAdrs baseQualityAdrs [configAdr, healthAdr, secretsAdr]

correlationQualityAdrs :: NonEmpty AdrTemplate
correlationQualityAdrs = appendAdrs baseQualityAdrs [correlationAdr]

relevanceQualitySources :: [SourceTemplate]
relevanceQualitySources =
  NonEmpty.toList (relevanceCorpusSources relevanceCorpusV1)
    <> [ literalQueueSource,
         renamedQueueSource,
         largeRegionSource,
         configComposeSource,
         leaseTokenSource,
         terseCorrelationSource
       ]

literalQueueSourceKey, renamedQueueSourceKey, largeRegionSourceKey, configComposeSourceKey, leaseTokenSourceKey, terseCorrelationSourceKey :: Text
literalQueueSourceKey = "literal-queue-path"
renamedQueueSourceKey = "renamed-queue-extension"
largeRegionSourceKey = "large-cache-observability-region"
configComposeSourceKey = "config-health-secrets-compose"
leaseTokenSourceKey = "lease-token-context"
terseCorrelationSourceKey = "terse-x-request-id"

configAdrKey, healthAdrKey, secretsAdrKey, correlationAdrKey :: AdrKey
configAdrKey = AdrKey 101
healthAdrKey = AdrKey 102
secretsAdrKey = AdrKey 103
correlationAdrKey = AdrKey 104

configAdr, healthAdr, secretsAdr, correlationAdr :: AdrTemplate
configAdr =
  adr
    configAdrKey
    "Layer runtime configuration"
    "Environment variables override optional repository configuration."
    "Load defaults, then a config file, then environment variables."
    "configuration.runtime"
    "deploy/**"

healthAdr =
  adr
    healthAdrKey
    "Use local readiness health checks"
    "Readiness checks local storage and never calls downstream services."
    "Use a healthcheck with liveness and readiness probes."
    "operations.health"
    "deploy/**"

secretsAdr =
  adr
    secretsAdrKey
    "Inject webhook secrets through the environment"
    "Credentials and tokens are not stored in repository configuration."
    "Read webhook credentials only from process environment variables."
    "security.secrets"
    "deploy/**"

correlationAdr =
  adr
    correlationAdrKey
    "Propagate request correlation identifiers"
    "HTTP requests carry X-Request-ID through every boundary."
    "Read X-Request-ID at ingress and pass request_id to structured logs and downstream calls."
    "observability.correlation"
    "src/http/**"

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

literalQueueSource, renamedQueueSource, largeRegionSource, configComposeSource, leaseTokenSource, terseCorrelationSource :: SourceTemplate
literalQueueSource =
  committed
    literalQueueSourceKey
    "[literal].txt"
    "durable queue acknowledgement after transaction commit and bounded retry"

renamedQueueSource =
  committed
    renamedQueueSourceKey
    "renamed.worker.unknown"
    queueBytes

largeRegionSource =
  committed
    largeRegionSourceKey
    "src/large.generated"
    (ByteString.concat (replicate 900 largeBoilerplateLine) <> relevantRegion <> ByteString.concat (replicate 900 largeBoilerplateLine))

configComposeSource =
  committed
    configComposeSourceKey
    "deploy/relay.compose"
    "services:\n  relay:\n    environment:\n      DROPWIRE_DATABASE: /data/relay.sqlite\n      DROPWIRE_WEBHOOK_TOKEN: ${DROPWIRE_WEBHOOK_TOKEN}\n    healthcheck:\n      test: [CMD, relay, readiness]\n"

leaseTokenSource =
  committed
    leaseTokenSourceKey
    "worker.lease"
    "lease_token expires; acknowledge remote queue delivery only after transaction commit"

terseCorrelationSource =
  committed
    terseCorrelationSourceKey
    "helpers/correlation.custom"
    "x_request_id = headers.get('X-Request-ID')\n"

queueBytes :: ByteString.ByteString
queueBytes =
  case
      [ sourceTemplateBytes source
        | source <- NonEmpty.toList (relevanceCorpusSources relevanceCorpusV1),
          sourceTemplateKey source == "queue-compose"
      ]
    of
      [bytes] -> bytes
      _ -> error "the frozen queue source was not unique"

largeBoilerplateLine :: ByteString.ByteString
largeBoilerplateLine = "generic helper value rendering option state\n"

relevantRegion :: ByteString.ByteString
relevantRegion =
  "cache identity uses source digest and compiler ABI without workspace path\n\
  \request_id and trace_id propagate through structured logs and latency metrics\n"

committed :: Text -> Text -> ByteString.ByteString -> SourceTemplate
committed key path bytes =
  SourceTemplate
    { sourceTemplateKey = key,
      sourceTemplatePath = path,
      sourceTemplateBytes = bytes,
      sourceTemplateMode = CommittedSource
    }

appendAdrs :: NonEmpty AdrTemplate -> [AdrTemplate] -> NonEmpty AdrTemplate
appendAdrs templates extras =
  case NonEmpty.toList templates of
    first : remaining -> first :| (remaining <> extras)
    [] -> error "NonEmpty.toList violated its constructor invariant"
