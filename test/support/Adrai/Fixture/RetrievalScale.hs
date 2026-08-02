{-# LANGUAGE OverloadedStrings #-}

module Adrai.Fixture.RetrievalScale
  ( retrievalScaleV1,
    retrievalOperationCountsV1,
    retrievalTopicCountsV1,
    adrTemplateAt,
    adrTemplates,
  )
where

import Adrai.Fixture.Prng (mkSeed)
import Adrai.Fixture.Types
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import qualified Data.Text as Text

retrievalOperationCountsV1 :: OperationCounts
retrievalOperationCountsV1 =
  OperationCounts
    { createOperationCount = 2000,
      amendOperationCount = 4,
      scopeOperationCount = 4,
      domainOperationCount = 0,
      obsoleteOperationCount = 2
    }

retrievalTopicCountsV1 :: [(Text, Int)]
retrievalTopicCountsV1 =
  [ ("cache identity", 667),
    ("trace propagation", 667),
    ("job delivery", 666)
  ]

retrievalScaleV1 :: RetrievalScaleSpec
retrievalScaleV1 =
  RetrievalScaleSpec
    { retrievalScaleMeta =
        FixtureMeta
          { fixtureVersion = FixtureV1,
            fixtureGenerator = "adrai-indexed-retrieval/v1",
            fixtureSeed = mkSeed 0,
            fixturePurpose =
              "2,000 logical ADR retrieval corpus; distinct from commit-history stress"
          },
      retrievalLogicalAdrCount = 2000,
      retrievalQueryProbes =
        RetrievalProbe "cache identity" "semantic cache identity" [AdrKey 0] 10
          :| [ RetrievalProbe "trace propagation" "structured trace propagation" [AdrKey 1] 10,
               RetrievalProbe "job delivery" "durable job delivery" [AdrKey 2] 10,
               RetrievalProbe "tail ADR" "Architecture rule 1999" [AdrKey 1999] 10
             ],
      retrievalShortlistBound = 120,
      retrievalInvariants =
        [ ExactOperationCount CreateOperation 2000,
          ExactOperationCount AmendOperation 4,
          ExactOperationCount ScopeOperation 4,
          ExactOperationCount ObsoleteOperation 2,
          BoundedRetrievalCandidates 120,
          NamedInvariant "retrieval corpus has 2,000 logical ADRs, not 2,000 commits"
        ]
    }

adrTemplateAt :: Int -> Maybe AdrTemplate
adrTemplateAt index
  | index < 0 || index >= retrievalLogicalAdrCount retrievalScaleV1 = Nothing
  | otherwise = Just (template index)

adrTemplates :: [AdrTemplate]
adrTemplates = map template [0 .. retrievalLogicalAdrCount retrievalScaleV1 - 1]

template :: Int -> AdrTemplate
template index =
  AdrTemplate
    { adrTemplateKey = AdrKey index,
      adrTemplateTitle = "Decision " <> indexText <> ": " <> topic,
      adrTemplateSummary = "Architecture rule " <> indexText <> " governs " <> topic <> ".",
      adrTemplateBodySections =
        [ ("Context", "A synthetic repository needs stable retrieval."),
          ("Decision", "Use rule " <> indexText <> " for " <> topic <> "; preserve semantic provenance."),
          ("Consequences", "The corpus remains independently inspectable.")
        ],
      adrTemplateDomains = ["benchmark.group-" <> Text.pack (show (index `mod` 3))],
      adrTemplateScopes = ["src/component-" <> indexText <> "/**"]
    }
  where
    indexText = pad2 index
    topic = topics !! (index `mod` length topics)

topics :: [Text]
topics = ["cache identity", "trace propagation", "job delivery"]

pad2 :: Int -> Text
pad2 value = Text.replicate (max 0 (2 - Text.length digits)) "0" <> digits
  where
    digits = Text.pack (show value)
