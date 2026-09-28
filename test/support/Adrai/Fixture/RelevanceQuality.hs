{-# LANGUAGE OverloadedStrings #-}

module Adrai.Fixture.RelevanceQuality
  ( relevanceQualityMeta,
    relevanceQualityAdrs,
    baseQualityAdrs,
    configQualityAdrs,
    correlationQualityAdrs,
    mcpQualityAdrs,
    authTokenQualityAdrs,
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
    mcpResponseAdrKey,
    planningAdrKey,
    mcpToolsSourceKey,
    authTokenSourceKey,
    authTokenAdrKey,
    pathCitationAdrKey,
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
import qualified Data.ByteString.Char8 as ByteStringChar8
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

mcpQualityAdrs :: NonEmpty AdrTemplate
mcpQualityAdrs = appendAdrs baseQualityAdrs [mcpResponseAdr, planningAdr]

authTokenQualityAdrs :: NonEmpty AdrTemplate
authTokenQualityAdrs = appendAdrs baseQualityAdrs [authTokenAdr, pathCitationAdr]

relevanceQualitySources :: [SourceTemplate]
relevanceQualitySources =
  NonEmpty.toList (relevanceCorpusSources relevanceCorpusV1)
    <> [ literalQueueSource,
         renamedQueueSource,
         largeRegionSource,
         configComposeSource,
         leaseTokenSource,
         terseCorrelationSource,
         mcpToolsSource,
         authTokenSource
       ]

literalQueueSourceKey, renamedQueueSourceKey, largeRegionSourceKey, configComposeSourceKey, leaseTokenSourceKey, terseCorrelationSourceKey :: Text
literalQueueSourceKey = "literal-queue-path"
renamedQueueSourceKey = "renamed-queue-extension"
largeRegionSourceKey = "large-cache-observability-region"
configComposeSourceKey = "config-health-secrets-compose"
leaseTokenSourceKey = "lease-token-context"
terseCorrelationSourceKey = "terse-x-request-id"

mcpToolsSourceKey :: Text
mcpToolsSourceKey = "mcp-tools-results"

authTokenSourceKey :: Text
authTokenSourceKey = "auth-token-symbol"

configAdrKey, healthAdrKey, secretsAdrKey, correlationAdrKey :: AdrKey
configAdrKey = AdrKey 101
healthAdrKey = AdrKey 102
secretsAdrKey = AdrKey 103
correlationAdrKey = AdrKey 104

mcpResponseAdrKey, planningAdrKey :: AdrKey
mcpResponseAdrKey = AdrKey 105
planningAdrKey = AdrKey 106

authTokenAdrKey, pathCitationAdrKey :: AdrKey
authTokenAdrKey = AdrKey 107
pathCitationAdrKey = AdrKey 108

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

-- Both decisions mention the same tool file, but only one directly governs
-- the source's result projection. This mirrors the hmem MCP tools example.
mcpResponseAdr, planningAdr :: AdrTemplate
mcpResponseAdr =
  adr
    mcpResponseAdrKey
    "Shape MCP results for agent context"
    "Return compact Observation summaries and operation-specific acknowledgements while retaining detail retrieval."
    "MCP mutations return acknowledgements with identifiers and status. Observation matches return bounded content previews, provenance, pagination, and match evidence. observation_get returns full detail."
    "mcp.response"
    "src/MCP/Tools.hs"

planningAdr =
  adr
    planningAdrKey
    "Keep plan descriptions durable and execution state explicit"
    "Project and Task descriptions contain lasting scope while statuses track progress and subtasks hold new work."
    "Project and Task descriptions are durable specifications. Track execution progress with status fields and create subtasks for new atomic work. Evidence: src/MCP/Tools.hs."
    "planning.semantics"
    "src/MCP/Tools.hs"

authTokenAdr, pathCitationAdr :: AdrTemplate
authTokenAdr =
  adr
    authTokenAdrKey
    "Authorize with auth token"
    "Use the auth token to authorize requests."
    "The auth token is read from the request and checked before dispatch."
    "security.authentication"
    "src/auth_token.hs"

pathCitationAdr =
  adr
    pathCitationAdrKey
    "Repository inventory"
    "See src/auth_token.hs."
    "The inventory cites src/auth_token.hs."
    "repository.inventory"
    "src/auth_token.hs"

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

mcpToolsSource :: SourceTemplate
mcpToolsSource =
  committed
    mcpToolsSourceKey
    "src/MCP/Tools.hs"
    ( "module MCP.Tools where\ntoolDefinitions =\n  tool \"project_create\" \"Project and Task descriptions are durable specifications for scope, constraints, approach, and acceptance intent; statuses track execution progress; create subtasks for discovered atomic work.\"\n"
        <> ByteString.concat
          [ "  tool \"endpoint_" <> ByteStringChar8.pack (show ordinal)
              <> "\" \"Expose one typed endpoint with a validated request body, a bounded list of fields, an explicit cursor, and an authorized HTTP transport boundary for an agent operation.\"\n"
            | ordinal <- [1 :: Int .. 280]
          ]
        <> "compactObservationSummary :: Value -> Value\ncompactObservationSummary result = object [\"preview\" .= boundedContentPreview result, \"provenance\" .= provenance result]\ncompactObservationDetail :: Value -> Value\ncompactObservationDetail result = fullObservationDetail result\ncompactObservationMatches :: Value -> Value\ncompactObservationMatches results = object [\"items\" .= map compactObservationSummary results, \"pagination\" .= nextOffset results, \"match_evidence\" .= matchedSubjects results]\nmutationAck :: Value -> Value\nmutationAck result = acknowledgementWithIdentifierAndStatus result\nstatusAck :: Value -> Value\nstatusAck result = statusAcknowledgement result\n"
    )

authTokenSource :: SourceTemplate
authTokenSource =
  committed
    authTokenSourceKey
    "src/auth_token.hs"
    "module AuthToken where\nauthToken :: Text -> Text\nauthToken value = value\n"

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
