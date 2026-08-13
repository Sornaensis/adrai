{-# LANGUAGE OverloadedStrings #-}

module Adrai.CoverageLedgerAuditTest (tests) where

import qualified Adrai.CoverageLedgerAudit as Audit
import Control.Exception (SomeException, try)
import qualified Data.ByteString.Char8 as ByteString
import Data.List (intercalate)
import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "P6-01A coverage ledger audit"
    [ testCase "frozen open ledger renders deterministic typed sorted gaps" testDeterministicReport,
      testCase "frozen closed ledger satisfies closure gate" testClosedLedger,
      testCase "manifest policy drift is fatal" testPolicyDrift,
      testCase "unknown state creates a blocking validation gap" testUnknownState,
      testCase "translation state outside the reviewed identities is blocked" testInvalidTranslation,
      testCase "category count and identity drift are fatal" testCategoryMismatch,
      testCase "missing repository target is a blocking validation gap" testMissingTarget,
      testCase "omitted evidence command is a blocking validation gap" testOmittedEvidenceCommand,
      testCase "omitted evidence fixture is a blocking validation gap" testOmittedEvidenceFixture,
      testCase "omitted evidence result is a blocking validation gap" testOmittedEvidenceResult,
      testCase "empty evidence command is a blocking validation gap" testEmptyEvidenceCommand,
      testCase "empty evidence fixture is a blocking validation gap" testEmptyEvidenceFixture,
      testCase "empty evidence result is a blocking validation gap" testEmptyEvidenceResult,
      testCase "invalid evidence command is a blocking validation gap" testInvalidEvidenceCommand,
      testCase "committed ledger runtime audit decodes all six fragments" testCommittedLedgerRuntimeAudit,
      testCase "require-closed exits nonzero for an open ledger" testRequireClosed
    ]

testDeterministicReport :: IO ()
testDeterministicReport =
  withFrozenLedger OpenLedger $ \root -> do
    first <- Audit.auditLedgerAt root
    second <- Audit.auditLedgerAt root
    first @?= second
    case first of
      Left problem -> assertFailure (Text.unpack problem)
      Right audit -> do
        Audit.auditIsClosed audit @?= False
        let report = Audit.renderAuditReport audit
        assertBool "coverage gaps are typed" ("gap-kind=coverage count=1" `Text.isInfixOf` report)
        assertBool "coverage gap has stable detail" ("detail=non-closure-state" `Text.isInfixOf` report)

testClosedLedger :: IO ()
testClosedLedger =
  withFrozenLedger ClosedLedger $ \root -> do
    result <- Audit.auditLedgerAt root
    case result of
      Left problem -> assertFailure (Text.unpack problem)
      Right audit -> do
        assertBool "all frozen closure-state rows should pass the explicit gate" (Audit.auditIsClosed audit)
        Text.isInfixOf "status=closed" (Audit.renderAuditReport audit) @?= True

testPolicyDrift :: IO ()
testPolicyDrift =
  withFrozenLedger PolicyDrift $ \root -> do
    result <- Audit.auditLedgerAt root
    case result of
      Left problem -> Text.isInfixOf "closureStates" problem @?= True
      Right _ -> assertFailure "policy drift must be fatal"

testUnknownState :: IO ()
testUnknownState = assertValidationGap UnknownState "invalid-state"

testInvalidTranslation :: IO ()
testInvalidTranslation = assertValidationGap InvalidTranslation "invalid-translation-identity"

testCategoryMismatch :: IO ()
testCategoryMismatch =
  withFrozenLedger CategoryMismatch $ \root -> do
    result <- Audit.auditLedgerAt root
    case result of
      Left problem -> Text.isInfixOf "count" problem @?= True
      Right _ -> assertFailure "fragment count drift must be fatal"

testMissingTarget :: IO ()
testMissingTarget = assertValidationGap MissingTarget "missing-haskell-test-target"

testOmittedEvidenceCommand, testOmittedEvidenceFixture, testOmittedEvidenceResult :: IO ()
testOmittedEvidenceCommand = assertValidationGap OmittedEvidenceCommand "missing-evidence-command"
testOmittedEvidenceFixture = assertValidationGap OmittedEvidenceFixture "missing-evidence-fixture"
testOmittedEvidenceResult = assertValidationGap OmittedEvidenceResult "missing-evidence-result"

testEmptyEvidenceCommand, testEmptyEvidenceFixture, testEmptyEvidenceResult :: IO ()
testEmptyEvidenceCommand = assertValidationGap EmptyEvidenceCommand "missing-evidence-command"
testEmptyEvidenceFixture = assertValidationGap EmptyEvidenceFixture "missing-evidence-fixture"
testEmptyEvidenceResult = assertValidationGap EmptyEvidenceResult "missing-evidence-result"

testInvalidEvidenceCommand :: IO ()
testInvalidEvidenceCommand = assertValidationGap InvalidEvidenceCommand "invalid-evidence-command"

testCommittedLedgerRuntimeAudit :: IO ()
testCommittedLedgerRuntimeAudit = do
  result <- Audit.auditLedgerAt ("test" </> "coverage" </> "ledger" </> "v1")
  case result of
    Left problem -> assertFailure (Text.unpack problem)
    Right audit -> do
      let report = Audit.renderAuditReport audit
      assertBool "the committed ledger must contain all 206 rows from six decodable fragments" ("actual-total=206" `Text.isInfixOf` report)

testRequireClosed :: IO ()
testRequireClosed =
  withFrozenLedger OpenLedger $ \root -> do
    result <- try (Audit.requireLedgerClosedAt root) :: IO (Either SomeException ())
    case result of
      Left _ -> pure ()
      Right () -> assertFailure "require-closed must fail for a nonzero gap count"

assertValidationGap :: Mutation -> Text.Text -> IO ()
assertValidationGap mutation expectedDetail =
  withFrozenLedger mutation $ \root -> do
    result <- Audit.auditLedgerAt root
    case result of
      Left problem -> assertFailure (Text.unpack problem)
      Right audit -> do
        Audit.auditIsClosed audit @?= False
        assertBool
          ("missing validation gap " <> Text.unpack expectedDetail)
          (Text.isInfixOf ("detail=" <> expectedDetail) (Audit.renderAuditReport audit))

data Mutation
  = ClosedLedger
  | OpenLedger
  | PolicyDrift
  | UnknownState
  | InvalidTranslation
  | CategoryMismatch
  | MissingTarget
  | OmittedEvidenceCommand
  | OmittedEvidenceFixture
  | OmittedEvidenceResult
  | EmptyEvidenceCommand
  | EmptyEvidenceFixture
  | EmptyEvidenceResult
  | InvalidEvidenceCommand
  deriving (Eq)

withFrozenLedger :: Mutation -> (FilePath -> IO value) -> IO value
withFrozenLedger mutation action =
  withSystemTempDirectory "adrai-coverage-ledger-audit" $ \temporaryRoot -> do
    let root = temporaryRoot </> "test" </> "coverage" </> "ledger" </> "v1"
        target = temporaryRoot </> "test" </> "unit" </> "Adrai" </> "CoverageLedgerAuditTest.hs"
    createDirectoryIfMissing True root
    createDirectoryIfMissing True (temporaryRoot </> "test" </> "unit" </> "Adrai")
    ByteString.writeFile target "module Adrai.CoverageLedgerAuditTest where\n"
    ByteString.writeFile (root </> "manifest.json") (ByteString.pack (manifestJson mutation))
    mapM_ (writeFragment root mutation) (zip [0 :: Int ..] frozenCategories)
    action root

writeFragment :: FilePath -> Mutation -> (Int, (String, String, Int)) -> IO ()
writeFragment root mutation (categoryIndex, (fileName, category, count)) =
  ByteString.writeFile (root </> fileName) (ByteString.pack (fragmentJson mutation categoryIndex category count))

manifestJson :: Mutation -> String
manifestJson mutation =
  "{\"schema\":\"adrai/coverage-ledger/v1\",\"expectedTotal\":206,\"categories\":["
    <> intercalate "," ["{\"file\":\"" <> fileName <> "\",\"category\":\"" <> category <> "\",\"expectedCount\":" <> show count <> "}" | (fileName, category, count) <- frozenCategories]
    <> "],\"allowedStates\":[\"planned\",\"partial\",\"covered\",\"installed-haskell-equivalent\",\"not-applicable\"],\"closureStates\":"
    <> if mutation == PolicyDrift then "[\"planned\",\"covered\",\"installed-haskell-equivalent\",\"not-applicable\"]" else "[\"covered\",\"installed-haskell-equivalent\",\"not-applicable\"]"
    <> ",\"gapStates\":[\"planned\",\"partial\"],\"allowedPhaseOwners\":[\"P2\",\"P3\",\"P4\",\"P5\",\"P6\",\"P7\"],\"allowedHaskellTestTypes\":[\"unit\",\"property\",\"integration\",\"golden\",\"e2e\"],\"allowedHaskellTestDirectories\":[\"test/unit/\",\"test/property/\",\"test/integration/\",\"test/golden/\",\"test/e2e/\"]}"

fragmentJson :: Mutation -> Int -> String -> Int -> String
fragmentJson mutation categoryIndex category count =
  "{\"schema\":\"adrai/coverage-ledger-category/v1\",\"category\":\""
    <> category
    <> "\",\"expectedCount\":"
    <> show fragmentCount
    <> ",\"sourceModules\":[{\"pythonPath\":\"ADRAI_1_Source/tests/test_synthetic_"
    <> show categoryIndex
    <> ".py\",\"expectedCount\":"
    <> show count
    <> "}],\"rows\":["
    <> intercalate "," [rowJson mutation categoryIndex rowIndex | rowIndex <- [1 .. count]]
    <> "]}"
  where
    fragmentCount
      | mutation == CategoryMismatch && categoryIndex == 0 = count - 1
      | otherwise = count

rowJson :: Mutation -> Int -> Int -> String
rowJson mutation categoryIndex rowIndex =
  "{\"pythonPath\":\"ADRAI_1_Source/tests/test_synthetic_"
    <> show categoryIndex
    <> ".py\",\"pythonMethod\":\"test_row_"
    <> show rowIndex
    <> "\",\"behaviorContract\":\"Synthetic behavior contract is nonempty.\",\"haskellTestName\":\"synthetic coverage row "
    <> show categoryIndex
    <> " "
    <> show rowIndex
    <> "\",\"haskellTestPath\":\""
    <> targetPath
    <> "\",\"haskellTestType\":\"unit\",\"phaseOwner\":\"P2\",\"state\":\""
    <> state
    <> "\",\"evidence\":"
    <> evidence
    <> "}"
  where
    firstRow = categoryIndex == 0 && rowIndex == 1
    state
      | firstRow && mutation == OpenLedger = "planned"
      | firstRow && mutation == UnknownState = "unknown"
      | firstRow && mutation == InvalidTranslation = "installed-haskell-equivalent"
      | otherwise = "covered"
    targetPath
      | firstRow && mutation == MissingTarget = "test/unit/Adrai/MissingCoverageLedgerTarget.hs"
      | otherwise = "test/unit/Adrai/CoverageLedgerAuditTest.hs"
    evidence
      | firstRow && mutation == OmittedEvidenceCommand = "{\"fixture\":\"synthetic fixture\",\"result\":\"synthetic result\"}"
      | firstRow && mutation == OmittedEvidenceFixture = "{\"command\":\"stack test adrai:adrai-test --test-arguments=--pattern=coverage-ledger\",\"result\":\"synthetic result\"}"
      | firstRow && mutation == OmittedEvidenceResult = "{\"command\":\"stack test adrai:adrai-test --test-arguments=--pattern=coverage-ledger\",\"fixture\":\"synthetic fixture\"}"
      | firstRow && mutation == EmptyEvidenceCommand = evidenceWith "" "synthetic fixture" "synthetic result"
      | firstRow && mutation == EmptyEvidenceFixture = evidenceWith "stack test adrai:adrai-test --test-arguments=--pattern=coverage-ledger" "" "synthetic result"
      | firstRow && mutation == EmptyEvidenceResult = evidenceWith "stack test adrai:adrai-test --test-arguments=--pattern=coverage-ledger" "synthetic fixture" ""
      | firstRow && mutation == InvalidEvidenceCommand = evidenceWith "cabal test" "synthetic fixture" "synthetic result"
      | otherwise = evidenceWith "stack test adrai:adrai-test --test-arguments=--pattern=coverage-ledger" "synthetic fixture" "synthetic result"

    evidenceWith command fixture result =
      "{\"command\":\"" <> command <> "\",\"fixture\":\"" <> fixture <> "\",\"result\":\"" <> result <> "\"}"

frozenCategories :: [(String, String, Int)]
frozenCategories =
  [ ("formats-distribution-footprint.json", "formats-distribution-footprint", 16),
    ("graph-service-conflicts-integrity.json", "graph-service-conflicts-integrity", 50),
    ("git-transactions-environments-provenance-branches-concurrency.json", "git-transactions-environments-provenance-branches-concurrency", 42),
    ("compiler-cache-consistency-usability.json", "compiler-cache-consistency-usability", 28),
    ("search-relevance-vectors-history-compare-explorer.json", "search-relevance-vectors-history-compare-explorer", 69),
    ("large-stress.json", "large-stress", 1)
  ]
