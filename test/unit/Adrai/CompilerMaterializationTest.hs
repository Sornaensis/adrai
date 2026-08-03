{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Adrai.CompilerMaterializationTest (tests, p303RationaleSnapshot) where

import Adrai.Compiler
import Adrai.Domain (mkDomain)
import Adrai.Format.Document
import Adrai.Graph (GraphAxis (..), reduceManagedGraph)
import Adrai.History
import Adrai.Property.Generators
import Adrai.Relevance
import Adrai.Retrieval
import Adrai.Scope (mkScopePattern)
import Adrai.Types
import Adrai.Vector (identifierTerms)
import Data.List (sort, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "P3-03 current search materialization"
    [ testCase "resolved items use ADR identity and explicit obsolete visibility" resolvedIdentityAndVisibility,
      testCase "decision forks use ADR@RID while other axis conflicts do not multiply items" conflictCandidateIdentity,
      testCase "effective domains, scope, and status remain independent axes" independentAxisProjection,
      testCase "rationale uses only path-sorted current validated relations" currentRationaleProjection,
      testCase "superseded and quarantined content and aliases are excluded" historicalContentExclusion,
      testCase "passage IDs, section identities, and inclusive line ranges are exact" exactPassageProjection,
      testCase "chunking normalizes CR boundaries and preserves other separators" chunkBoundaryContract
    ]

resolvedIdentityAndVisibility :: IO ()
resolvedIdentityAndVisibility = do
  let fixture = resolvedFixture
      materialization = mustMaterialize (projectionFixtureAfter fixture)
      primaryAdr = projectionFixturePrimaryAdr fixture
      secondaryAdr = adrIdAt fixturePool 1
      primary = documentFor primaryAdr materialization
      secondary = documentFor secondaryAdr materialization
  map searchDocumentItemId (searchMaterializationDocuments materialization)
    @?= sort [adrIdText primaryAdr, adrIdText secondaryAdr]
  searchDocumentItemId primary @?= adrIdText primaryAdr
  searchDocumentCandidateRecordId primary @?= recordIdAt fixturePool 1
  searchDocumentDecision primary @?= "Application Programming Interface (API) v2"
  searchDocumentDomains primary @?= ["search"]
  searchDocumentScope primary @?= ["src", "src/generated"]
  searchDocumentObsolete primary @?= True
  searchDocumentConflicted primary @?= False
  searchDocumentItemId secondary @?= adrIdText secondaryAdr
  visibleSearchItemIds ExcludeObsolete materialization @?= Set.singleton (adrIdText secondaryAdr)
  visibleSearchItemIds IncludeObsolete materialization
    @?= Set.fromList [adrIdText primaryAdr, adrIdText secondaryAdr]
  assertBool "current alias is retained" (("api", "application programming interface") `elem` searchMaterializationAliases materialization)
  searchMaterializationAliases materialization @?= sort (searchMaterializationAliases materialization)

conflictCandidateIdentity :: IO ()
conflictCandidateIdentity = do
  let fixture = materializeAxisConflict (AxisConflictSpec fixturePool DecisionAxis 0)
      materialization = mustMaterialize (axisSnapshot fixture)
      adr = axisFixtureSubject fixture
      expectedRecords = [recordIdAt fixturePool 1, recordIdAt fixturePool 2]
      expectedIds = [adrIdText adr <> "@" <> recordIdText record | record <- expectedRecords]
      documents = searchMaterializationDocuments materialization
  map searchDocumentItemId documents @?= expectedIds
  map searchDocumentCandidateRecordId documents @?= expectedRecords
  map searchDocumentDecision documents @?= ["left", "right"]
  assertBool "every fork is explicitly conflicted" (all searchDocumentConflicted documents)
  case documents of
    [left, right] -> searchDocumentRationale left @?= searchDocumentRationale right
    _ -> error "expected exactly two decision conflict candidates"
  mapM_
    (\axis -> do
       let other = materializeAxisConflict (AxisConflictSpec fixturePool axis 0)
       length (searchMaterializationDocuments (mustMaterialize (axisSnapshot other))) @?= 1
    )
    [ScopeAxis, DomainAxis, StatusAxis]

independentAxisProjection :: IO ()
independentAxisProjection = do
  let domainFixture = materializeAxisConflict (AxisConflictSpec fixturePool DomainAxis 0)
      domainDocument = onlyDocument (mustMaterialize (axisSnapshot domainFixture))
      scopeFixture = materializeAxisConflict (AxisConflictSpec fixturePool ScopeAxis 0)
      scopeDocument = onlyDocument (mustMaterialize (axisSnapshot scopeFixture))
      statusFixture = materializeAxisConflict (AxisConflictSpec fixturePool StatusAxis 0)
      statusDocument = onlyDocument (mustMaterialize (axisSnapshot statusFixture))
  searchDocumentDomains domainDocument @?= ["compiler.cache", "compiler.search"]
  searchDocumentScope domainDocument @?= ["src/**"]
  searchDocumentObsolete domainDocument @?= False
  searchDocumentDomains scopeDocument @?= ["compiler"]
  searchDocumentScope scopeDocument @?= []
  searchDocumentObsolete scopeDocument @?= False
  searchDocumentDomains statusDocument @?= ["compiler"]
  searchDocumentScope statusDocument @?= ["src/**"]
  searchDocumentObsolete statusDocument @?= False

currentRationaleProjection :: IO ()
currentRationaleProjection = do
  let fixture = rationaleFixture
      materialization = mustMaterialize (rationaleSnapshot fixture)
      document = onlyDocument materialization
  searchDocumentRationale document
    @?= Text.intercalate
      "\n"
      [ "status: current status",
        "amends: current amendment",
        "domains: current domain",
        "applies_to: current scope"
      ]
  searchDocumentSourcePaths document @?= rationaleCurrentPaths fixture
  searchDocumentDomains document @?= ["current.domain"]
  searchDocumentScope document @?= ["src/current/**"]
  searchDocumentObsolete document @?= True
  let expectedBody =
        Text.intercalate
          "\n"
          [ "# Context",
            "context first",
            "context second",
            "# Decision",
            "decision first",
            "decision second",
            "# Consequences",
            "consequence",
            "# Notes",
            "other note"
          ]
      expectedIdentifierSource =
        Text.intercalate
          "\n"
          [ "Current Decision API (CDAPI)",
            "Current summary",
            expectedBody,
            "current.domain",
            searchDocumentRationale document
          ]
  searchDocumentIdentifierSource document @?= expectedIdentifierSource
  searchDocumentIdentifiers document
    @?= Text.unwords (identifierTerms True expectedIdentifierSource)
  visibleSearchItemIds ExcludeObsolete materialization @?= Set.empty
  visibleSearchItemIds IncludeObsolete materialization @?= Set.singleton (adrIdText (rationaleAdr fixture))

historicalContentExclusion :: IO ()
historicalContentExclusion = do
  let fixture = rationaleFixture
      materialization = mustMaterialize (rationaleSnapshot fixture)
      documents = searchMaterializationDocuments materialization
      passages = searchMaterializationPassages materialization
      aliases = searchMaterializationAliases materialization
      searchable =
        Text.toLower . Text.intercalate "\n" $
          concatMap documentTexts documents
            <> map searchPassageText passages
            <> concatMap (\(alias, expansion) -> [alias, expansion]) aliases
  assertBool "superseded decision text is absent" (not ("superseded-secret" `Text.isInfixOf` searchable))
  assertBool "superseded connection rationale is absent" (not ("superseded scope" `Text.isInfixOf` searchable))
  assertBool "quarantined rationale is absent" (not ("quarantined-leak" `Text.isInfixOf` searchable))
  assertBool "superseded alias is absent" (not (any ((== "rsa") . fst) aliases))
  assertBool "current alias is retained" (("cdapi", "current decision api") `elem` aliases)
  aliases @?= sort aliases

exactPassageProjection :: IO ()
exactPassageProjection = do
  let fixture = rationaleFixture
      document = onlyDocument (mustMaterialize (rationaleSnapshot fixture))
      itemId = searchDocumentItemId document
      passages = sortOn searchPassageSectionKind (mustPassages document)
      expected =
        [ (TitleSummarySection, itemId <> "/section/title-summary/0/1-2", 0, 1, 2, "Current Decision API (CDAPI)\nCurrent summary"),
          (DecisionSection, itemId <> "/section/decision/0/1-2", 0, 1, 2, "decision first\ndecision second"),
          (RationaleSection, itemId <> "/section/rationale/0/1-4", 0, 1, 4, "status: current status\namends: current amendment\ndomains: current domain\napplies_to: current scope"),
          (DomainsSection, itemId <> "/section/domains/0/1-1", 0, 1, 1, "current.domain"),
          (ContextSection, itemId <> "/section/context/0/1-2", 0, 1, 2, "context first\ncontext second"),
          (ConsequencesSection, itemId <> "/section/consequences/0/1-1", 0, 1, 1, "consequence"),
          (OtherSection, itemId <> "/section/other/0/1-2", 0, 1, 2, "Notes\nother note")
        ]
  map passageShape passages @?= expected
  assertBool "passage source identity is preserved" (all ((== itemId) . searchPassageDocumentItemId) passages)
  assertBool "passage record identity is preserved" (all ((== rationaleCurrentRecord fixture) . searchPassageCandidateRecordId) passages)
  map searchPassageWeight passages @?= map (sectionWeight . firstOfSix) expected

chunkBoundaryContract :: IO ()
chunkBoundaryContract = do
  let everyBoundary =
        Text.pack
          [ 'a',
            '\r',
            '\n',
            'b',
            '\r',
            'c',
            '\v',
            'd',
            '\f',
            'e',
            '\x001c',
            'f',
            '\x001d',
            'g',
            '\x001e',
            'h',
            '\x0085',
            'i',
            '\x2028',
            'j',
            '\x2029',
            'k'
          ]
      normalized = "a\nb\nc\vd\fe\x001c\&f\x001d\&g\x001e\&h\x0085\&i\x2028\&j\x2029\&k"
      exactlyFourMiB = Text.replicate (maxTextBytes `div` 2) "é"
  normalizeNewlines everyBoundary @?= normalized
  chunkText everyBoundary @?= Right [TextChunk 0 1 3 normalized]
  case chunkText exactlyFourMiB of
    Right _ -> pure ()
    Left problem -> assertFailure ("exact 4 MiB UTF-8 input was rejected: " <> show problem)
  chunkText (exactlyFourMiB <> "a") @?= Left (ChunkTooLarge (maxTextBytes + 1) maxTextBytes)

data RationaleFixture = RationaleFixture
  { rationaleAdr :: AdrId,
    rationaleCurrentRecord :: RecordId,
    rationaleSnapshot :: ReadSnapshot,
    rationaleCurrentPaths :: [Text]
  }

rationaleFixture :: RationaleFixture
rationaleFixture = RationaleFixture adr currentRecord snapshot currentPaths
  where
    pool = fixturePool
    adr = adrIdAt pool 0
    rootRecord = recordIdAt pool 0
    currentRecord = recordIdAt pool 1
    missingRecord = recordIdAt pool 9
    cStatus = connectionIdAt pool 0
    cAmend = connectionIdAt pool 1
    cDomain = connectionIdAt pool 2
    cScope = connectionIdAt pool 3
    cOldScope = connectionIdAt pool 4
    cOldDomain = connectionIdAt pool 5
    cOldStatus = connectionIdAt pool 6
    cInvalid = connectionIdAt pool 7
    oldDomain = must "old domain" (mkDomain "legacy.domain")
    currentDomain = must "current domain" (mkDomain "current.domain")
    oldScope = must "old scope" (mkScopePattern "src/legacy/**")
    currentScope = must "current scope" (mkScopePattern "src/current/**")
    rootDecision =
      ManagedDecision
        ( DecisionRecord
            adr
            rootRecord
            "Retired Secret Alias (RSA)"
            "superseded-secret summary"
            [oldDomain]
            "# Decision\nsuperseded-secret\n"
        )
    currentDecision =
      ManagedDecision
        ( DecisionRecord
            adr
            currentRecord
            "Current Decision API (CDAPI)"
            "Current summary"
            [oldDomain]
            ( Text.intercalate
                "\n"
                [ "# Context",
                  "context first",
                  "context second",
                  "# Decision",
                  "decision first",
                  "decision second",
                  "# Consequences",
                  "consequence",
                  "# Notes",
                  "other note"
                ]
                <> "\n"
            )
        )
    connect identifier payload rationale = ManagedConnection (ConnectionRecord identifier payload rationale)
    amend = connect cAmend (AmendsConnection (AmendsPayload adr currentRecord [rootRecord])) "current amendment\n"
    oldScopeConnection = connect cOldScope (AppliesToConnection (AppliesToPayload adr [] "initial" [oldScope] [] [oldScope])) "superseded scope\n"
    currentScopeConnection = connect cScope (AppliesToConnection (AppliesToPayload adr [cOldScope] "replace" [currentScope] [oldScope] [currentScope])) "current scope\n"
    oldDomainConnection = connect cOldDomain (DomainsConnection (DomainsPayload adr [] "initial" [oldDomain] [] [oldDomain] [])) "superseded domain\n"
    currentDomainConnection = connect cDomain (DomainsConnection (DomainsPayload adr [cOldDomain] "replace" [currentDomain] [oldDomain] [currentDomain] [])) "current domain\n"
    oldStatusConnection = connect cOldStatus (StatusConnection (StatusPayload adr [] StatusActive [rootRecord] Nothing)) "superseded status\n"
    currentStatusConnection = connect cStatus (StatusConnection (StatusPayload adr [cOldStatus] StatusObsolete [currentRecord] Nothing)) "current status\n"
    invalidConnection = connect cInvalid (AmendsConnection (AmendsPayload adr missingRecord [currentRecord])) "quarantined-leak\n"
    records =
      [ invalidConnection,
        oldStatusConnection,
        currentDomainConnection,
        rootDecision,
        currentScopeConnection,
        currentDecision,
        oldDomainConnection,
        currentStatusConnection,
        oldScopeConnection,
        amend
      ]
    documents = documentsFor pool records
    snapshot = ReadSnapshot (RevisionIdentity "rationale" "rationale-resolved") documents (reduceManagedGraph records) Map.empty
    decisionPath = pathOf currentDecision documents
    currentConnectionPaths = sort (map (`pathOf` documents) [currentStatusConnection, amend, currentDomainConnection, currentScopeConnection])
    currentPaths = decisionPath : currentConnectionPaths

p303RationaleSnapshot :: ReadSnapshot
p303RationaleSnapshot = rationaleSnapshot rationaleFixture

resolvedFixture :: ProjectionFixture
resolvedFixture =
  materializeProjectionDag
    (ProjectionDagSpec fixturePool (DomainSpec ["search"]) (ScopeSpec ["src"] False) "Application Programming Interface (API)")

fixturePool :: IdentifierPool
fixturePool = IdentifierPool 0

axisSnapshot :: AxisFixture -> ReadSnapshot
axisSnapshot fixture =
  ReadSnapshot
    (RevisionIdentity "axis" "axis-resolved")
    (documentsFor pool records)
    (reduceManagedGraph records)
    Map.empty
  where
    records = axisFixtureRecords fixture
    pool = axisConflictPool (axisFixtureSpec fixture)

documentsFor :: IdentifierPool -> [ManagedRecord] -> [ParsedManagedDocument]
documentsFor pool = zipWith (\ordinal -> parsedDocument pool ordinal (fromIntegral ordinal + 100) []) [0 ..]

pathOf :: ManagedRecord -> [ParsedManagedDocument] -> Text
pathOf record documents =
  case [repoPathText (parsedManagedPath document) | document <- documents, parsedManagedRecord document == record] of
    [path] -> path
    _ -> error "fixture record path was not unique"

documentFor :: AdrId -> SearchMaterialization -> SearchDocument
documentFor adr materialization =
  case filter ((== adr) . searchDocumentAdrId) (searchMaterializationDocuments materialization) of
    [document] -> document
    _ -> error "expected exactly one materialized ADR document"

onlyDocument :: SearchMaterialization -> SearchDocument
onlyDocument materialization =
  case searchMaterializationDocuments materialization of
    [document] -> document
    _ -> error "expected exactly one materialized document"

mustMaterialize :: ReadSnapshot -> SearchMaterialization
mustMaterialize snapshot =
  case materializeCurrentSearch snapshot of
    Right materialization -> materialization
    Left problem -> error ("search materialization failed: " <> show problem)

mustPassages :: SearchDocument -> [SearchPassage]
mustPassages document =
  case chunkSearchDocument document of
    Right passages -> passages
    Left problem -> error ("passage materialization failed: " <> show problem)

documentTexts :: SearchDocument -> [Text]
documentTexts document =
  [ searchDocumentTitle document,
    searchDocumentSummary document,
    searchDocumentContext document,
    searchDocumentDecision document,
    searchDocumentConsequences document,
    Text.unlines (searchDocumentDomains document),
    searchDocumentRationale document,
    searchDocumentOther document,
    Text.unlines (searchDocumentScope document),
    Text.unlines (searchDocumentSourcePaths document),
    searchDocumentIdentifierSource document,
    searchDocumentIdentifiers document
  ]

passageShape :: SearchPassage -> (SectionKind, Text, Int, Int, Int, Text)
passageShape passage =
  ( searchPassageSectionKind passage,
    searchPassageId passage,
    searchPassageOrdinal passage,
    searchPassageLineStart passage,
    searchPassageLineEnd passage,
    searchPassageText passage
  )

firstOfSix :: (a, b, c, d, e, f) -> a
firstOfSix (value, _, _, _, _, _) = value

must :: String -> Either error value -> value
must label result =
  case result of
    Right value -> value
    Left _ -> error ("invalid P3-03 fixture: " <> label)
