{-# LANGUAGE OverloadedStrings #-}

module Adrai.GoldenFixturesTest (tests) where

import Adrai.Format (publicSchemaText)
import Adrai.Types
  ( ExitClass (..),
    PublicSchema (..),
    exitClassCode,
    exitClassFromCode,
  )
import Control.Monad (forM)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isHexDigit)
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as Vector
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.FilePath ((</>), isAbsolute, splitDirectories, takeDirectory, takeExtension, takeFileName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
    (@?=),
  )

tests :: TestTree
tests =
  testGroup
    "versioned golden fixtures"
    [ testCase "manifest has complete, resolvable provenance" testManifestProvenance,
      testCase "declared files match byte length, newline, and SHA-256" testManifestIntegrity,
      testCase "every declared JSON fixture parses" testJsonFixturesParse,
      testGroup
        "canonical configuration"
        [ testCase "default TOML bytes are exact" $ assertFixtureBytes "config/default.toml" defaultTomlBytes,
          testCase "default TOML hash is exact" $ assertFixtureHash "config/default.toml" defaultTomlSha256
        ],
      testGroup
        "decision document golden bytes"
        [ testCase "semantic bytes are exact" $ assertFixtureBytes "documents/decision-create.semantic.md" semanticDecisionBytes,
          testCase "semantic hash is exact" $ assertFixtureHash "documents/decision-create.semantic.md" semanticDecisionSha256,
          testCase "sealed bytes are exact" $ assertFixtureBytes "documents/decision-create.sealed.md" sealedDecisionBytes,
          testCase "sealed hash is exact" $ assertFixtureHash "documents/decision-create.sealed.md" sealedDecisionSha256
        ],
      testCase "current Dropwire result-only baseline has exact aggregate quality thresholds" testCurrentDropwire,
      testCase "historical Dropwire snapshot is explicit and not current" testHistoricalDropwire,
      testCase "production timing observations are not an equality gate" testProductionTimingPolicy,
      testCase "CLI exit classes are exactly 0, 2, 3, and 4" testExitClasses,
      testCase "public schema tags are exact" testPublicSchemaTags
    ]

data Manifest = Manifest
  { manifestFixtureContract :: Text,
    manifestSchemaVersion :: Int,
    manifestExtractionDate :: Text,
    manifestLicense :: Text,
    manifestProvenance :: Aeson.Value,
    manifestSources :: [ManifestSource],
    manifestFixtures :: [ManifestFixture]
  }
  deriving (Eq, Show)

instance Aeson.FromJSON Manifest where
  parseJSON = Aeson.withObject "fixture manifest" $ \value ->
    Manifest
      <$> value .: "fixtureContract"
      <*> value .: "schemaVersion"
      <*> value .: "extractionDate"
      <*> value .: "license"
      <*> value .: "provenance"
      <*> value .: "sources"
      <*> value .: "fixtures"

data ManifestSource = ManifestSource
  { manifestSourceId :: Text,
    manifestSourcePath :: FilePath,
    manifestSourceSha256 :: Text
  }
  deriving (Eq, Show)

instance Aeson.FromJSON ManifestSource where
  parseJSON = Aeson.withObject "manifest source" $ \value ->
    ManifestSource
      <$> value .: "id"
      <*> value .: "sourcePath"
      <*> value .: "sha256"

data ManifestFixture = ManifestFixture
  { manifestFixturePath :: FilePath,
    manifestFixtureKind :: Text,
    manifestFixtureSourceRefs :: [Text],
    manifestFixtureSha256 :: Text,
    manifestFixtureByteLength :: Integer,
    manifestFixtureFinalNewline :: Bool,
    manifestFixtureAssertion :: Text
  }
  deriving (Eq, Show)

instance Aeson.FromJSON ManifestFixture where
  parseJSON = Aeson.withObject "manifest fixture" $ \value ->
    ManifestFixture
      <$> value .: "path"
      <*> value .: "kind"
      <*> value .: "sourceRefs"
      <*> value .: "sha256"
      <*> value .: "byteLength"
      <*> value .: "finalNewline"
      <*> value .: "intendedHaskellAssertion"

newtype CliExitContract = CliExitContract
  { cliExitCodes :: [Int]
  }
  deriving (Eq, Show)

instance Aeson.FromJSON CliExitContract where
  parseJSON = Aeson.withObject "CLI exit contract" $ \value -> do
    exitCases <- value .: "exits"
    pure (CliExitContract (map cliExitCode exitCases))

newtype CliExitCase = CliExitCase
  { cliExitCode :: Int
  }
  deriving (Eq, Show)

instance Aeson.FromJSON CliExitCase where
  parseJSON = Aeson.withObject "CLI exit case" $ \value ->
    CliExitCase <$> value .: "code"

data DropwireResultContract = DropwireResultContract
  { dropwireFixtureSchema :: Text,
    dropwireStatus :: Text,
    dropwireSourceArtifact :: Text,
    dropwireSourceSha256 :: Text,
    dropwireTimingPolicy :: Text,
    dropwireTimingsAreEqualityGate :: Bool,
    dropwireBaseline :: DropwireBaseline
  }
  deriving (Eq, Show)

instance Aeson.FromJSON DropwireResultContract where
  parseJSON = Aeson.withObject "Dropwire result contract" $ \value ->
    DropwireResultContract
      <$> value .: "fixtureSchema"
      <*> value .: "status"
      <*> value .: "sourceArtifact"
      <*> value .: "sourceSha256"
      <*> value .: "timingPolicy"
      <*> value .: "timingsAreEqualityGate"
      <*> value .: "baseline"

data DropwireBaseline = DropwireBaseline
  { dropwireAnyExpectedTop3Cases :: Int,
    dropwireAnyExpectedTop3Rate :: Double,
    dropwireAssociationHits :: Map Text Int,
    dropwireAssociationRecall :: Map Text Double,
    dropwireExpectedAssociations :: Int,
    dropwireHighConfidenceFalsePositives :: Int,
    dropwireMaximumSeconds :: Double,
    dropwireMedianSeconds :: Double,
    dropwireMediumConfidenceFalsePositives :: Int,
    dropwireNegativeCases :: Int,
    dropwireP95Seconds :: Double,
    dropwirePositiveCases :: Int,
    dropwireTop1PositiveCases :: Int,
    dropwireTop1PositiveRate :: Double
  }
  deriving (Eq, Show)

instance Aeson.FromJSON DropwireBaseline where
  parseJSON = Aeson.withObject "Dropwire aggregate baseline" $ \value ->
    DropwireBaseline
      <$> value .: "any_expected_top3_cases"
      <*> value .: "any_expected_top3_rate"
      <*> value .: "association_hits"
      <*> value .: "association_recall"
      <*> value .: "expected_associations"
      <*> value .: "high_confidence_false_positives"
      <*> value .: "max_seconds"
      <*> value .: "median_seconds"
      <*> value .: "medium_confidence_false_positives"
      <*> value .: "negative_cases"
      <*> value .: "p95_seconds"
      <*> value .: "positive_cases"
      <*> value .: "top1_positive_cases"
      <*> value .: "top1_positive_rate"

testManifestProvenance :: Assertion
testManifestProvenance = do
  (_, manifest) <- loadManifest
  manifestFixtureContract manifest @?= "adrai-contract-fixtures/1"
  manifestSchemaVersion manifest @?= 1
  assertNonEmpty "extractionDate" (manifestExtractionDate manifest)
  assertNonEmpty "license" (manifestLicense manifest)
  assertProvenance (manifestProvenance manifest)
  assertBool "manifest must declare sources" (not (null (manifestSources manifest)))
  assertBool "manifest must declare fixtures" (not (null (manifestFixtures manifest)))
  mapM_ assertSource (manifestSources manifest)
  let sourceIds = map manifestSourceId (manifestSources manifest)
      uniqueSourceIds = Set.fromList sourceIds
  assertEqual "source ids must be unique" (length sourceIds) (Set.size uniqueSourceIds)
  mapM_ (assertFixtureMetadata uniqueSourceIds) (manifestFixtures manifest)

assertProvenance :: Aeson.Value -> Assertion
assertProvenance value =
  case value of
    Aeson.Object objectValue ->
      mapM_ (assertSubstantiveField objectValue) ["method", "constraints", "generatedBy"]
    _ -> assertFailure "provenance must be a JSON object"

assertSubstantiveField :: Aeson.Object -> Text -> Assertion
assertSubstantiveField objectValue name =
  case KeyMap.lookup (Key.fromText name) objectValue of
    Nothing -> assertFailure ("provenance is missing " <> T.unpack name)
    Just fieldValue -> assertBool ("provenance field is empty: " <> T.unpack name) (isSubstantive fieldValue)

isSubstantive :: Aeson.Value -> Bool
isSubstantive value =
  case value of
    Aeson.Null -> False
    Aeson.String textValue -> not (T.null (T.strip textValue))
    Aeson.Array values -> not (Vector.null values)
    Aeson.Object objectValue -> not (KeyMap.null objectValue)
    Aeson.Number _ -> True
    Aeson.Bool _ -> True

assertSource :: ManifestSource -> Assertion
assertSource source = do
  assertNonEmpty "source id" (manifestSourceId source)
  assertBool "sourcePath must be repository-relative" (safeRelativePath (manifestSourcePath source))
  assertSha256 "source sha256" (manifestSourceSha256 source)

assertFixtureMetadata :: Set.Set Text -> ManifestFixture -> Assertion
assertFixtureMetadata sourceIds fixture = do
  assertBool "fixture path must be repository-relative" (safeRelativePath (manifestFixturePath fixture))
  assertNonEmpty "fixture kind" (manifestFixtureKind fixture)
  assertBool "fixture must cite at least one source" (not (null (manifestFixtureSourceRefs fixture)))
  mapM_
    (\sourceRef -> assertBool ("unknown sourceRef: " <> T.unpack sourceRef) (Set.member sourceRef sourceIds))
    (manifestFixtureSourceRefs fixture)
  assertSha256 "fixture sha256" (manifestFixtureSha256 fixture)
  assertBool "fixture byteLength must be non-negative" (manifestFixtureByteLength fixture >= 0)
  assertNonEmpty "intendedHaskellAssertion" (manifestFixtureAssertion fixture)

safeRelativePath :: FilePath -> Bool
safeRelativePath path =
  not (null path)
    && not (isAbsolute path)
    && all (\component -> component /= ".." && component /= "." && not (null component)) (splitDirectories path)

assertSha256 :: String -> Text -> Assertion
assertSha256 label value =
  assertBool
    (label <> " must contain exactly 64 hexadecimal characters")
    (T.length value == 64 && T.all isHexDigit value)

testManifestIntegrity :: Assertion
testManifestIntegrity = do
  (root, manifest) <- loadManifest
  dataFiles <- listFixtureDataFiles root
  let declaredFiles = sort (map manifestFixturePath (manifestFixtures manifest))
  assertEqual "manifest must declare every fixture data file" dataFiles declaredFiles
  mapM_ (assertFixtureIntegrity root) (manifestFixtures manifest)

assertFixtureIntegrity :: FilePath -> ManifestFixture -> Assertion
assertFixtureIntegrity root fixture = do
  let path = root </> manifestFixturePath fixture
  exists <- doesFileExist path
  assertBool ("declared fixture does not exist: " <> path) exists
  bytes <- BS.readFile path
  assertEqual ("byteLength for " <> path) (manifestFixtureByteLength fixture) (fromIntegral (BS.length bytes))
  assertEqual ("finalNewline for " <> path) (manifestFixtureFinalNewline fixture) (hasFinalNewline bytes)
  assertEqual ("sha256 for " <> path) (T.toLower (manifestFixtureSha256 fixture)) (sha256Text bytes)

testJsonFixturesParse :: Assertion
testJsonFixturesParse = do
  (root, manifest) <- loadManifest
  mapM_ (assertJsonParses root) (filter ((== ".json") . takeExtension . manifestFixturePath) (manifestFixtures manifest))

assertJsonParses :: FilePath -> ManifestFixture -> Assertion
assertJsonParses root fixture = do
  bytes <- BS.readFile (root </> manifestFixturePath fixture)
  case Aeson.eitherDecodeStrict' bytes :: Either String Aeson.Value of
    Left problem -> assertFailure (manifestFixturePath fixture <> " is not valid JSON: " <> problem)
    Right _ -> pure ()

assertFixtureBytes :: FilePath -> BS.ByteString -> Assertion
assertFixtureBytes relativePath expected = do
  root <- findFixtureRoot
  actual <- BS.readFile (root </> relativePath)
  actual @?= expected

assertFixtureHash :: FilePath -> Text -> Assertion
assertFixtureHash relativePath expected = do
  root <- findFixtureRoot
  actual <- BS.readFile (root </> relativePath)
  assertEqual ("sha256 for " <> relativePath) expected (sha256Text actual)

testCurrentDropwire :: Assertion
testCurrentDropwire = do
  contract <- loadTypedJsonFixture "search/dropwire-current.json"
  rawValue <- loadJsonFixture "search/dropwire-current.json"
  let baseline = dropwireBaseline contract
      expectedHits = Map.fromList [("1", 19), ("3", 38), ("5", 43), ("7", 47), ("10", 47)]
      expectedRecall = Map.map (roundSix . (/ 47) . fromIntegral) expectedHits
      totalCases = dropwirePositiveCases baseline + dropwireNegativeCases baseline
      observedTimings = [dropwireMedianSeconds baseline, dropwireP95Seconds baseline, dropwireMaximumSeconds baseline]
  assertEqual resultOnlyContext "adrai/golden/search-baseline/v1" (dropwireFixtureSchema contract)
  assertEqual resultOnlyContext "current" (dropwireStatus contract)
  assertEqual
    resultOnlyContext
    "ADRAI_1_Source/verification/ADRAI_1_Search_Enhanced_Dropwire_Evaluation_24.json"
    (dropwireSourceArtifact contract)
  assertEqual
    resultOnlyContext
    "97B70927B44CC3D4125BC91AC48DCB5096E631E5183261FF59E16960C94E2C73"
    (dropwireSourceSha256 contract)
  assertSha256 (resultOnlyContext <> ": sourceSha256") (dropwireSourceSha256 contract)
  assertEqual resultOnlyContext 22 totalCases
  assertEqual resultOnlyContext 19 (dropwirePositiveCases baseline)
  assertEqual resultOnlyContext 3 (dropwireNegativeCases baseline)
  assertEqual resultOnlyContext 47 (dropwireExpectedAssociations baseline)
  assertEqual resultOnlyContext 19 (dropwireTop1PositiveCases baseline)
  assertEqual resultOnlyContext 1.0 (dropwireTop1PositiveRate baseline)
  assertEqual resultOnlyContext 19 (dropwireAnyExpectedTop3Cases baseline)
  assertEqual resultOnlyContext 1.0 (dropwireAnyExpectedTop3Rate baseline)
  assertEqual resultOnlyContext expectedHits (dropwireAssociationHits baseline)
  assertEqual resultOnlyContext expectedRecall (dropwireAssociationRecall baseline)
  assertEqual resultOnlyContext 0 (dropwireMediumConfidenceFalsePositives baseline)
  assertEqual resultOnlyContext 0 (dropwireHighConfidenceFalsePositives baseline)
  assertEqual resultOnlyContext "observedSnapshot" (dropwireTimingPolicy contract)
  assertEqual resultOnlyContext False (dropwireTimingsAreEqualityGate contract)
  assertBool resultOnlyContext (all finiteNonNegative observedTimings)
  assertBool
    resultOnlyContext
    (dropwireMedianSeconds baseline <= dropwireP95Seconds baseline && dropwireP95Seconds baseline <= dropwireMaximumSeconds baseline)
  assertBool (resultOnlyContext <> ": old_summary is historical-only") (not (containsKey "old_summary" rawValue))

resultOnlyContext :: String
resultOnlyContext =
  "Dropwire result-contract validation only; the source corpus is absent, was not reconstructed, and executable Dropwire parity is not claimed"

roundSix :: Double -> Double
roundSix value
  | rounded == 0 = 0
  | otherwise = rounded
  where
    rounded = fromInteger (round (value * 1000000)) / 1000000

finiteNonNegative :: Double -> Bool
finiteNonNegative value = value >= 0 && not (isNaN value || isInfinite value)

testHistoricalDropwire :: Assertion
testHistoricalDropwire = do
  value <- loadJsonFixture "search/dropwire-old-historical.json"
  status <- requiredTextAt "status" value
  description <- requiredTextAt "description" value
  status @?= "historical"
  assertBool "historical Dropwire snapshot must explicitly not be current" (status /= "current")
  assertBool
    "historical Dropwire description must explicitly reject current-baseline status"
    ("not the current" `T.isInfixOf` T.toLower description)

testProductionTimingPolicy :: Assertion
testProductionTimingPolicy = do
  value <- loadJsonFixture "search/production-stress-compact.json"
  timingsAreGate <- requiredBoolAt "timingsAreEqualityGate" value
  timingsAreGate @?= False

testExitClasses :: Assertion
testExitClasses = do
  let classes = [ExitSuccess, ExitUserError, ExitConflict, ExitCheckFailed]
      codes = [0, 2, 3, 4]
  map exitClassCode classes @?= codes
  map exitClassFromCode codes @?= map Just classes
  exitClassFromCode 1 @?= Nothing
  fixtureValue <- loadJsonFixture "cli/exits.json"
  case Aeson.fromJSON fixtureValue of
    Aeson.Error problem -> assertFailure ("cli/exits.json has an invalid contract shape: " <> problem)
    Aeson.Success contract -> Set.toAscList (Set.fromList (cliExitCodes contract)) @?= codes

testPublicSchemaTags :: Assertion
testPublicSchemaTags =
  map publicSchemaText publicSchemas
    @?= [ "adrai/search/v1",
          "adrai/relevant/v1",
          "adrai/history/v1",
          "adrai/show-collapsed/v1",
          "adrai/show-exploded/v1",
          "adrai/events/v1"
        ]
  where
    publicSchemas =
      [ SearchPublicV1,
        RelevantPublicV1,
        HistoryPublicV1,
        ShowCollapsedPublicV1,
        ShowExplodedPublicV1,
        EventsPublicV1
      ]

loadManifest :: IO (FilePath, Manifest)
loadManifest = do
  root <- findFixtureRoot
  bytes <- BS.readFile (root </> "manifest.json")
  case Aeson.eitherDecodeStrict' bytes of
    Left problem -> assertFailure ("manifest.json is invalid: " <> problem) >> fail "unreachable"
    Right manifest -> pure (root, manifest)

findFixtureRoot :: IO FilePath
findFixtureRoot = do
  workingDirectory <- getCurrentDirectory
  search workingDirectory
  where
    search directory = do
      let candidate = directory </> "test" </> "fixtures" </> "contracts" </> "v1"
      hasManifest <- doesFileExist (candidate </> "manifest.json")
      isDirectory <- doesDirectoryExist candidate
      if hasManifest && isDirectory
        then pure candidate
        else
          let parent = takeDirectory directory
           in if parent == directory
                then assertFailure "could not locate test/fixtures/contracts/v1/manifest.json" >> fail "unreachable"
                else search parent

loadJsonFixture :: FilePath -> IO Aeson.Value
loadJsonFixture relativePath = do
  root <- findFixtureRoot
  bytes <- BS.readFile (root </> relativePath)
  case Aeson.eitherDecodeStrict' bytes of
    Left problem -> assertFailure (relativePath <> " is not valid JSON: " <> problem) >> fail "unreachable"
    Right value -> pure value

loadTypedJsonFixture :: (Aeson.FromJSON value) => FilePath -> IO value
loadTypedJsonFixture relativePath = do
  root <- findFixtureRoot
  bytes <- BS.readFile (root </> relativePath)
  case Aeson.eitherDecodeStrict' bytes of
    Left problem ->
      assertFailure
        ( resultOnlyContext
            <> "; fixture="
            <> relativePath
            <> "; typed JSON contract failure: "
            <> problem
        )
        >> fail "unreachable"
    Right value -> pure value

requiredTextAt :: Text -> Aeson.Value -> IO Text
requiredTextAt field value =
  case lookupTopLevel field value of
    Just (Aeson.String textValue) -> pure textValue
    _ -> assertFailure ("missing string field: " <> T.unpack field) >> fail "unreachable"

requiredBoolAt :: Text -> Aeson.Value -> IO Bool
requiredBoolAt field value =
  case lookupTopLevel field value of
    Just (Aeson.Bool boolValue) -> pure boolValue
    _ -> assertFailure ("missing boolean field: " <> T.unpack field) >> fail "unreachable"

lookupTopLevel :: Text -> Aeson.Value -> Maybe Aeson.Value
lookupTopLevel field value =
  case value of
    Aeson.Object objectValue -> KeyMap.lookup (Key.fromText field) objectValue
    _ -> Nothing

containsKey :: Text -> Aeson.Value -> Bool
containsKey name value =
  case value of
    Aeson.Object objectValue ->
      KeyMap.member (Key.fromText name) objectValue || any (containsKey name) (KeyMap.elems objectValue)
    Aeson.Array values -> any (containsKey name) (Vector.toList values)
    _ -> False

assertNonEmpty :: String -> Text -> Assertion
assertNonEmpty label value = assertBool (label <> " must be non-empty") (not (T.null (T.strip value)))

hasFinalNewline :: BS.ByteString -> Bool
hasFinalNewline bytes =
  case BS.unsnoc bytes of
    Nothing -> False
    Just (_, finalByte) -> finalByte == 10

sha256Text :: BS.ByteString -> Text
sha256Text bytes = T.pack (show (hash bytes :: Digest SHA256))

listFixtureDataFiles :: FilePath -> IO [FilePath]
listFixtureDataFiles root = sort . map canonicalFixturePath <$> walk "" root
  where
    walk relative directory = do
      entries <- listDirectory directory
      nested <- forM entries $ \entry -> do
        let relativePath = if null relative then entry else relative </> entry
            absolutePath = root </> relativePath
        isDirectory <- doesDirectoryExist absolutePath
        if isDirectory
          then walk relativePath absolutePath
          else
            pure
              [ relativePath
                | takeFileName relativePath /= "README.md",
                  takeFileName relativePath /= "manifest.json"
              ]
      pure (concat nested)

canonicalFixturePath :: FilePath -> FilePath
canonicalFixturePath = map (\character -> if character == '\\' then '/' else character)

defaultTomlBytes :: BS.ByteString
defaultTomlBytes =
  BC.pack . unlines $
    [ "schema = 1",
      "",
      "[paths]",
      "decisions = \"architecture/adrai/decisions\"",
      "connections = \"architecture/adrai/connections\"",
      "",
      "[[line]]",
      "id = \"trunk\"",
      "refs = [\"refs/heads/main\", \"refs/remotes/origin/main\", \"refs/heads/master\", \"refs/remotes/origin/master\"]"
    ]

semanticDecisionBytes :: BS.ByteString
semanticDecisionBytes =
  BC.pack . unlines $
    [ "+++",
      "schema = \"adrai/decision/v1\"",
      "adr = \"A00000000000000000000000000\"",
      "record = \"R00000000000000000000000000\"",
      "title = \"Stable cache identity\"",
      "summary = \"Cache keys derive from semantic inputs.\"",
      "domains = [\"compiler.cache.identity\"]",
      "+++",
      "",
      "## Decision",
      "Use semantic inputs."
    ]

sealedDecisionBytes :: BS.ByteString
sealedDecisionBytes =
  semanticDecisionBytes
    <> BC.pack
      "\n<!-- @adrai:eyJhIjp7ImkiOiJhcmNoaXRlY3QiLCJrIjoiaHVtYW4ifSwiYiI6IjAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAiLCJrIjoiZGVjaXNpb24uY3JlYXRlIiwibyI6IlIwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMCIsIm9wIjoiTzAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwIiwicyI6InNoYTI1Njo5cW5OYWxjZWZVSURuTjdXQWhyQ2J0NEVtU0FIOHdwZXBaT2Znd2gwdVlZIiwidCI6MTcwMDAwMDAwMDAwMCwidiI6MSwieCI6ImFkcmFpLzEuMC4wIn0 -->\n"

-- Filled from the immutable v1 manifest; each hash independently pins the
-- canonical fixture in addition to the byte-for-byte expectation above.
defaultTomlSha256 :: Text
defaultTomlSha256 = "634db372b5ddf19440951835ed203d310bc5f18edbc696497e02f143254b2eea"

semanticDecisionSha256 :: Text
semanticDecisionSha256 = "f6a9cd6a571e7d42039cded6021ac26ede04992007f30a5ea5939f830874b986"

sealedDecisionSha256 :: Text
sealedDecisionSha256 = "48f53cd5d5ca94fdcd506b766bfaa3bf4d6f20d19cbe7b2c5bdf77ef0027caec"
