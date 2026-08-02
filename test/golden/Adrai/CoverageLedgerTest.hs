{-# LANGUAGE OverloadedStrings #-}

module Adrai.CoverageLedgerTest (tests) where

import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?))
import qualified Data.ByteString as BS
import Data.Char (isAsciiLower, isDigit)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.FilePath ((</>), takeDirectory, takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Python-to-Haskell coverage ledger"
    [ testCase "manifest freezes identity and runtime independence" testManifestIdentity,
      testCase "manifest freezes all six category files and 206 rows" testManifestCategories,
      testCase "category schemas and row counts are exact" testCategoryContracts,
      testCase "Python method identities and Haskell test names are unique" testUniqueIdentities,
      testCase "all required row fields are substantive and well shaped" testRowShapes,
      testCase "test directories, types, phases, and states are approved" testApprovedEnums,
      testCase "every row carries substantive evidence" testEvidence,
      testCase "translation-only states are limited to distribution rows" testTranslationStates,
      testCase "source-module declarations account for every row" testSourceModuleTotals
    ]

data LedgerManifest = LedgerManifest
  { ledgerSchema :: Text,
    ledgerExtractionDate :: Text,
    ledgerExpectedTotal :: Int,
    ledgerCategories :: [CategorySpec],
    ledgerAllowedStates :: [Text],
    ledgerAllowedPhases :: [Text],
    ledgerAllowedTypes :: [Text],
    ledgerAllowedDirectories :: [Text],
    ledgerRuntimeIndependence :: RuntimeIndependence,
    ledgerSourceDeletion :: SourceDeletion,
    ledgerDynamicGenerators :: [DynamicGenerator]
  }
  deriving (Eq, Show)

instance Aeson.FromJSON LedgerManifest where
  parseJSON = Aeson.withObject "coverage ledger manifest" $ \value ->
    LedgerManifest
      <$> value .: "schema"
      <*> value .: "extractionDate"
      <*> value .: "expectedTotal"
      <*> value .: "categories"
      <*> value .: "allowedStates"
      <*> value .: "allowedPhaseOwners"
      <*> value .: "allowedHaskellTestTypes"
      <*> value .: "allowedHaskellTestDirectories"
      <*> value .: "runtimeIndependence"
      <*> value .: "sourceDeletion"
      <*> value .: "dynamicGenerators"

data CategorySpec = CategorySpec
  { categoryFile :: FilePath,
    categoryName :: Text,
    categoryExpectedCount :: Int
  }
  deriving (Eq, Ord, Show)

instance Aeson.FromJSON CategorySpec where
  parseJSON = Aeson.withObject "coverage category specification" $ \value ->
    CategorySpec
      <$> value .: "file"
      <*> value .: "category"
      <*> value .: "expectedCount"

data RuntimeIndependence = RuntimeIndependence
  { runtimeRequiresPrototype :: Bool,
    runtimeRequiresPython :: Bool,
    runtimeLedgerHaskellOwned :: Bool
  }
  deriving (Eq, Show)

instance Aeson.FromJSON RuntimeIndependence where
  parseJSON = Aeson.withObject "runtime independence" $ \value ->
    RuntimeIndependence
      <$> value .: "requiresPrototypeAtTestRuntime"
      <*> value .: "requiresPython"
      <*> value .: "ledgerIsHaskellOwned"

data SourceDeletion = SourceDeletion
  { deletionPrototypeAllowed :: Bool,
    deletionLedgerSurvives :: Bool,
    deletionPathsHistorical :: Bool
  }
  deriving (Eq, Show)

instance Aeson.FromJSON SourceDeletion where
  parseJSON = Aeson.withObject "source deletion policy" $ \value ->
    SourceDeletion
      <$> value .: "prototypeMayBeDeletedAfterParity"
      <*> value .: "ledgerRemainsValidAfterDeletion"
      <*> value .: "pythonPathsAreHistoricalAnchors"

data DynamicGenerator = DynamicGenerator
  { generatorContract :: Text,
    generatorPythonPath :: Text,
    generatorHaskellOwner :: Text,
    generatorPhaseOwner :: Text
  }
  deriving (Eq, Ord, Show)

instance Aeson.FromJSON DynamicGenerator where
  parseJSON = Aeson.withObject "dynamic generator ownership" $ \value ->
    DynamicGenerator
      <$> value .: "contract"
      <*> value .: "pythonPath"
      <*> value .: "haskellOwner"
      <*> value .: "phaseOwner"

data CategoryFragment = CategoryFragment
  { fragmentSchema :: Text,
    fragmentCategory :: Text,
    fragmentExpectedCount :: Int,
    fragmentSourceModules :: [SourceModule],
    fragmentRows :: [CoverageRow]
  }
  deriving (Eq, Show)

instance Aeson.FromJSON CategoryFragment where
  parseJSON = Aeson.withObject "coverage category" $ \value ->
    CategoryFragment
      <$> value .: "schema"
      <*> value .: "category"
      <*> value .: "expectedCount"
      <*> value .: "sourceModules"
      <*> value .: "rows"

data SourceModule = SourceModule
  { sourceModulePythonPath :: Text,
    sourceModuleExpectedCount :: Maybe Int
  }
  deriving (Eq, Ord, Show)

instance Aeson.FromJSON SourceModule where
  parseJSON value =
    case value of
      Aeson.String path -> pure (SourceModule path Nothing)
      _ -> Aeson.withObject "source module" parseObject value
    where
      parseObject objectValue =
        SourceModule
          <$> objectValue .: "pythonPath"
          <*> (Just <$> objectValue .: "expectedCount")

data CoverageRow = CoverageRow
  { rowPythonPath :: Text,
    rowPythonMethod :: Text,
    rowBehaviorContract :: Text,
    rowHaskellTestName :: Text,
    rowHaskellTestPath :: Text,
    rowHaskellTestType :: Text,
    rowPhaseOwner :: Text,
    rowState :: Text,
    rowEvidence :: Evidence,
    rowTranslationNote :: Maybe Text
  }
  deriving (Eq, Show)

instance Aeson.FromJSON CoverageRow where
  parseJSON = Aeson.withObject "coverage row" $ \value ->
    CoverageRow
      <$> value .: "pythonPath"
      <*> value .: "pythonMethod"
      <*> value .: "behaviorContract"
      <*> value .: "haskellTestName"
      <*> value .: "haskellTestPath"
      <*> value .: "haskellTestType"
      <*> value .: "phaseOwner"
      <*> value .: "state"
      <*> value .: "evidence"
      <*> value .:? "translationNote"

newtype Evidence = Evidence {evidenceItems :: [Text]}
  deriving (Eq, Show)

instance Aeson.FromJSON Evidence where
  parseJSON value =
    case value of
      Aeson.String item -> pure (Evidence [item])
      _ -> Evidence <$> Aeson.parseJSON value

testManifestIdentity :: Assertion
testManifestIdentity = do
  (_, manifest) <- loadManifest
  ledgerSchema manifest @?= "adrai/coverage-ledger/v1"
  ledgerExtractionDate manifest @?= "2026-08-02"
  ledgerExpectedTotal manifest @?= 206
  let runtimeIndependence = ledgerRuntimeIndependence manifest
      sourceDeletion = ledgerSourceDeletion manifest
  runtimeRequiresPrototype runtimeIndependence @?= False
  runtimeRequiresPython runtimeIndependence @?= False
  runtimeLedgerHaskellOwned runtimeIndependence @?= True
  deletionPrototypeAllowed sourceDeletion @?= True
  deletionLedgerSurvives sourceDeletion @?= True
  deletionPathsHistorical sourceDeletion @?= True
  ledgerAllowedStates manifest @?= exactStates
  ledgerAllowedPhases manifest @?= exactPhases
  ledgerAllowedTypes manifest @?= exactTypes
  ledgerAllowedDirectories manifest @?= map snd exactTypeDirectories
  ledgerDynamicGenerators manifest @?= exactDynamicGenerators
  forM_ (ledgerDynamicGenerators manifest) $ \generator -> do
    assertSubstantive "generator contract" (generatorContract generator)
    assertSubstantive "generator Python path" (generatorPythonPath generator)
    assertSubstantive "generator Haskell owner" (generatorHaskellOwner generator)
    assertSubstantive "generator phase owner" (generatorPhaseOwner generator)

testManifestCategories :: Assertion
testManifestCategories = do
  (root, manifest) <- loadManifest
  ledgerCategories manifest @?= exactCategories
  map categoryExpectedCount (ledgerCategories manifest) @?= [16, 50, 42, 28, 69, 1]
  sum (map categoryExpectedCount (ledgerCategories manifest)) @?= 206
  entries <- listDirectory root
  let categoryJsonFiles = sort [entry | entry <- entries, takeExtension entry == ".json", entry /= "manifest.json"]
  categoryJsonFiles @?= sort (map categoryFile exactCategories)

testCategoryContracts :: Assertion
testCategoryContracts = do
  (_, manifest, fragments) <- loadLedger
  length fragments @?= 6
  forM_ fragments $ \(specification, fragment) -> do
    fragmentSchema fragment @?= "adrai/coverage-ledger-category/v1"
    fragmentCategory fragment @?= categoryName specification
    fragmentExpectedCount fragment @?= categoryExpectedCount specification
    length (fragmentRows fragment) @?= categoryExpectedCount specification
  sum (map (length . fragmentRows . snd) fragments) @?= ledgerExpectedTotal manifest

testUniqueIdentities :: Assertion
testUniqueIdentities = do
  (_, _, fragments) <- loadLedger
  let rows = allRows fragments
      pythonIdentities = map (\row -> (rowPythonPath row, rowPythonMethod row)) rows
      haskellNames = map rowHaskellTestName rows
  length rows @?= 206
  Set.size (Set.fromList pythonIdentities) @?= 206
  Set.size (Set.fromList haskellNames) @?= 206

testRowShapes :: Assertion
testRowShapes = do
  (_, _, fragments) <- loadLedger
  forM_ (allRows fragments) $ \row -> do
    assertSubstantive "pythonPath" (rowPythonPath row)
    assertSubstantive "pythonMethod" (rowPythonMethod row)
    assertSubstantive "behaviorContract" (rowBehaviorContract row)
    assertSubstantive "haskellTestName" (rowHaskellTestName row)
    assertSubstantive "haskellTestPath" (rowHaskellTestPath row)
    assertSubstantive "haskellTestType" (rowHaskellTestType row)
    assertSubstantive "phaseOwner" (rowPhaseOwner row)
    assertSubstantive "state" (rowState row)
    assertBool ("invalid Python test path: " <> T.unpack (rowPythonPath row)) (validPythonPath (rowPythonPath row))
    assertBool ("invalid Python method: " <> T.unpack (rowPythonMethod row)) (validPythonMethod (rowPythonMethod row))

testApprovedEnums :: Assertion
testApprovedEnums = do
  (_, manifest, fragments) <- loadLedger
  let allowedStates = Set.fromList (ledgerAllowedStates manifest)
      allowedPhases = Set.fromList (ledgerAllowedPhases manifest)
      allowedTypes = Set.fromList (ledgerAllowedTypes manifest)
      directoryByType = Map.fromList exactTypeDirectories
  forM_ (allRows fragments) $ \row -> do
    assertBool "row state is not approved" (Set.member (rowState row) allowedStates)
    assertBool "row phase is not approved" (Set.member (rowPhaseOwner row) allowedPhases)
    assertBool "row test type is not approved" (Set.member (rowHaskellTestType row) allowedTypes)
    case Map.lookup (rowHaskellTestType row) directoryByType of
      Nothing -> assertFailure ("no directory for Haskell test type " <> T.unpack (rowHaskellTestType row))
      Just directory ->
        assertBool
          ("Haskell test path is outside approved directory: " <> T.unpack (rowHaskellTestPath row))
          (directory `T.isPrefixOf` rowHaskellTestPath row)

testEvidence :: Assertion
testEvidence = do
  (_, _, fragments) <- loadLedger
  forM_ (allRows fragments) $ \row -> do
    let items = evidenceItems (rowEvidence row)
    assertBool "evidence must contain at least one item" (not (null items))
    forM_ items (assertSubstantive "evidence item")

testTranslationStates :: Assertion
testTranslationStates = do
  (_, _, fragments) <- loadLedger
  let specialRows = filter ((`Set.member` translationStates) . rowState) (allRows fragments)
      actual =
        sort
          [ (rowPythonPath row, rowPythonMethod row, rowState row)
            | row <- specialRows
          ]
  actual @?= sort exactTranslationRows
  forM_ specialRows $ \row -> do
    rowPythonPath row @?= "ADRAI_1_Source/tests/test_distribution_cli.py"
    case rowTranslationNote row of
      Nothing -> assertFailure "translated distribution row is missing translationNote"
      Just note -> assertSubstantive "translationNote" note

testSourceModuleTotals :: Assertion
testSourceModuleTotals = do
  (_, _, fragments) <- loadLedger
  forM_ fragments $ \(_, fragment) -> do
    let rows = fragmentRows fragment
        rowCounts = Map.fromListWith (+) [(rowPythonPath row, 1 :: Int) | row <- rows]
        declared = fragmentSourceModules fragment
        declaredPaths = map sourceModulePythonPath declared
    length declaredPaths @?= Set.size (Set.fromList declaredPaths)
    Set.fromList declaredPaths @?= Map.keysSet rowCounts
    sum (Map.elems rowCounts) @?= fragmentExpectedCount fragment
    forM_ declared $ \sourceModule ->
      case Map.lookup (sourceModulePythonPath sourceModule) rowCounts of
        Nothing -> assertFailure ("source module has no rows: " <> T.unpack (sourceModulePythonPath sourceModule))
        Just actualCount ->
          case sourceModuleExpectedCount sourceModule of
            Nothing -> assertBool "legacy source module must still have rows" (actualCount > 0)
            Just expectedCount -> actualCount @?= expectedCount

loadManifest :: IO (FilePath, LedgerManifest)
loadManifest = do
  root <- findLedgerRoot
  manifest <- decodeJsonFile (root </> "manifest.json")
  pure (root, manifest)

loadLedger :: IO (FilePath, LedgerManifest, [(CategorySpec, CategoryFragment)])
loadLedger = do
  (root, manifest) <- loadManifest
  fragments <- mapM (loadFragment root) (ledgerCategories manifest)
  pure (root, manifest, fragments)

loadFragment :: FilePath -> CategorySpec -> IO (CategorySpec, CategoryFragment)
loadFragment root specification = do
  fragment <- decodeJsonFile (root </> categoryFile specification)
  pure (specification, fragment)

decodeJsonFile :: (Aeson.FromJSON value) => FilePath -> IO value
decodeJsonFile path = do
  bytes <- BS.readFile path
  case Aeson.eitherDecodeStrict' bytes of
    Left problem -> assertFailure (path <> " is invalid: " <> problem) >> fail "unreachable"
    Right value -> pure value

findLedgerRoot :: IO FilePath
findLedgerRoot = do
  workingDirectory <- getCurrentDirectory
  search workingDirectory
  where
    search directory = do
      let candidate = directory </> "test" </> "coverage" </> "ledger" </> "v1"
      hasManifest <- doesFileExist (candidate </> "manifest.json")
      isDirectory <- doesDirectoryExist candidate
      if hasManifest && isDirectory
        then pure candidate
        else
          let parent = takeDirectory directory
           in if parent == directory
                then assertFailure "could not locate test/coverage/ledger/v1/manifest.json" >> fail "unreachable"
                else search parent

allRows :: [(CategorySpec, CategoryFragment)] -> [CoverageRow]
allRows = concatMap (fragmentRows . snd)

assertSubstantive :: String -> Text -> Assertion
assertSubstantive label value =
  assertBool (label <> " must be nonempty and trimmed") (not (T.null value) && T.strip value == value)

validPythonPath :: Text -> Bool
validPythonPath path =
  "ADRAI_1_Source/tests/test_" `T.isPrefixOf` path
    && ".py" `T.isSuffixOf` path
    && not ("/../" `T.isInfixOf` path)
    && T.count "/" path == 2

validPythonMethod :: Text -> Bool
validPythonMethod method =
  "test_" `T.isPrefixOf` method
    && T.all (\character -> isAsciiLower character || isDigit character || character == '_') method

exactCategories :: [CategorySpec]
exactCategories =
  [ CategorySpec "formats-distribution-footprint.json" "formats-distribution-footprint" 16,
    CategorySpec "graph-service-conflicts-integrity.json" "graph-service-conflicts-integrity" 50,
    CategorySpec "git-transactions-environments-provenance-branches-concurrency.json" "git-transactions-environments-provenance-branches-concurrency" 42,
    CategorySpec "compiler-cache-consistency-usability.json" "compiler-cache-consistency-usability" 28,
    CategorySpec "search-relevance-vectors-history-compare-explorer.json" "search-relevance-vectors-history-compare-explorer" 69,
    CategorySpec "large-stress.json" "large-stress" 1
  ]

exactStates :: [Text]
exactStates = ["planned", "partial", "covered", "installed-haskell-equivalent", "not-applicable"]

exactPhases :: [Text]
exactPhases = ["P2", "P3", "P4", "P5", "P6", "P7"]

exactTypes :: [Text]
exactTypes = ["unit", "property", "integration", "golden", "e2e"]

exactTypeDirectories :: [(Text, Text)]
exactTypeDirectories =
  [ ("unit", "test/unit/"),
    ("property", "test/property/"),
    ("integration", "test/integration/"),
    ("golden", "test/golden/"),
    ("e2e", "test/e2e/")
  ]

translationStates :: Set.Set Text
translationStates = Set.fromList ["installed-haskell-equivalent", "not-applicable"]

exactTranslationRows :: [(Text, Text, Text)]
exactTranslationRows =
  [ ( "ADRAI_1_Source/tests/test_distribution_cli.py",
      "test_pyproject_declares_dependency_free_console_entrypoint",
      "not-applicable"
    ),
    ( "ADRAI_1_Source/tests/test_distribution_cli.py",
      "test_python_module_entrypoint_exposes_the_same_cli",
      "installed-haskell-equivalent"
    )
  ]

exactDynamicGenerators :: [DynamicGenerator]
exactDynamicGenerators =
  [ DynamicGenerator
      "relevance"
      "ADRAI_1_Source/tests/test_relevance.py"
      "test/support/Adrai/RelevanceFixture.hs"
      "P5",
    DynamicGenerator
      "large-stress"
      "ADRAI_1_Source/tests/test_large.py"
      "test/support/Adrai/LargeStressFixture.hs"
      "P6"
  ]
