{-# LANGUAGE OverloadedStrings #-}

module Adrai.CoverageLedgerTest (tests) where

import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?))
import qualified Data.ByteString as BS
import Data.Char (isAsciiLower, isDigit, isSpace)
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
      testCase "stress evidence accepts only one explicit standalone opt-in" testStressEvidenceOptIn,
      testCase "PowerShell-safe stress evidence decodes the exact argv" testPowerShellStressEvidenceArgv,
      testCase "translation-only states are limited to distribution rows" testTranslationStates,
      testCase "source-module declarations account for every row" testSourceModuleTotals,
      testCase "JSON decoder strips exactly one leading UTF-8 BOM" testUtf8BomDecoding
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
    ledgerClosureStates :: [Text],
    ledgerGapStates :: [Text],
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
      <*> value .: "closureStates"
      <*> value .: "gapStates"
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

data Evidence = Evidence
  { evidenceCommand :: Text,
    evidenceFixture :: Text,
    evidenceResult :: Text
  }
  deriving (Eq, Show)

instance Aeson.FromJSON Evidence where
  parseJSON = Aeson.withObject "coverage evidence" $ \value ->
    Evidence
      <$> value .: "command"
      <*> value .: "fixture"
      <*> value .: "result"

testManifestIdentity :: Assertion
testManifestIdentity = do
  (ledgerRoot, manifest) <- loadManifest
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
  ledgerClosureStates manifest @?= exactClosureStates
  ledgerGapStates manifest @?= exactGapStates
  ledgerDynamicGenerators manifest @?= exactDynamicGenerators
  forM_ (ledgerDynamicGenerators manifest) $ \generator -> do
    assertSubstantive "generator contract" (generatorContract generator)
    assertSubstantive "generator Python path" (generatorPythonPath generator)
    assertSubstantive "generator Haskell owner" (generatorHaskellOwner generator)
    assertSubstantive "generator phase owner" (generatorPhaseOwner generator)
    let repositoryRoot = takeDirectory (takeDirectory (takeDirectory (takeDirectory ledgerRoot)))
        ownerPath = repositoryRoot </> T.unpack (generatorHaskellOwner generator)
    ownerExists <- doesFileExist ownerPath
    assertBool ("dynamic generator Haskell owner does not exist: " <> ownerPath) ownerExists

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
    let evidence = rowEvidence row
    assertSubstantive "evidence command" (evidenceCommand evidence)
    assertSubstantive "evidence fixture" (evidenceFixture evidence)
    assertSubstantive "evidence result" (evidenceResult evidence)
    assertBool
      "evidence command must invoke a valid Haskell test suite command"
      (validEvidenceCommand (evidenceCommand evidence))

validEvidenceCommand :: Text -> Bool
validEvidenceCommand command =
  case shellWords command of
    Just ("stack" : "test" : "adrai:adrai-test" : _) -> True
    Just ("stack" : "test" : "adrai:adrai-cache-selection-test" : _) -> True
    Just ("stack" : "test" : "adrai:adrai-stress-test" : arguments) -> hasStressOptIn arguments
    _ -> False

-- Deliberately independent from CoverageLedgerAudit: the golden verifier is
-- the second line of defence for the frozen ledger contract.
hasStressOptIn :: [Text] -> Bool
hasStressOptIn arguments =
  case exactlyOneTestArguments arguments of
    Nothing -> False
    Just testArguments ->
      case shellWords testArguments of
        Nothing -> False
        Just innerArguments -> "--run-stress" `elem` innerArguments

exactlyOneTestArguments :: [Text] -> Maybe Text
exactlyOneTestArguments = go Nothing
  where
    go found [] = found
    go found ("--test-arguments" : value : remaining) = add found value remaining
    go _ ["--test-arguments"] = Nothing
    go found (argument : remaining)
      | "--test-arguments=" `T.isPrefixOf` argument =
          add found (T.drop (T.length "--test-arguments=") argument) remaining
      | otherwise = go found remaining

    add Nothing value remaining = go (Just value) remaining
    add (Just _) _ _ = Nothing

shellWords :: Text -> Maybe [Text]
shellWords = fmap (map T.pack) . go [] [] Nothing False . T.unpack
  where
    go completed current quote started [] =
      case quote of
        Just _ -> Nothing
        Nothing -> Just (reverse (finish completed current started))
    go completed current quote started (character : remaining) =
      case quote of
        Nothing
          | isSpace character -> go (finish completed current started) [] Nothing False remaining
          | character == '\'' || character == '\"' -> go completed current (Just character) True remaining
          | character == '\\' -> escaped completed current Nothing remaining
          | otherwise -> go completed (character : current) Nothing True remaining
        Just delimiter
          | character == delimiter -> go completed current Nothing started remaining
          | character == '\\' -> escaped completed current (Just delimiter) remaining
          | otherwise -> go completed (character : current) (Just delimiter) True remaining

    escaped completed current quote [] = go completed ('\\' : current) quote True []
    escaped completed current quote (character : remaining)
      | character == '\\' || character == '\'' || character == '\"' || isSpace character =
          go completed (character : current) quote True remaining
      | otherwise = go completed (character : '\\' : current) quote True remaining

    finish completed current started
      | started = reverse current : completed
      | otherwise = completed

testStressEvidenceOptIn :: Assertion
testStressEvidenceOptIn = do
  assertBool "cache-selection evidence command was rejected" (validEvidenceCommand cacheSelectionCommand)
  assertBool "unsupported component evidence command was accepted" (not (validEvidenceCommand unsupportedComponentCommand))
  mapM_ (assertBool "valid stress evidence command was rejected" . validEvidenceCommand) validCommands
  mapM_ (assertBool "ambiguous stress evidence command was accepted" . not . validEvidenceCommand) invalidCommands
  where
    validCommands =
      [ "stack test adrai:adrai-stress-test --test-arguments \"--run-stress --pattern=focused\"",
        "stack test adrai:adrai-stress-test --test-arguments=\"--run-stress --pattern=focused\"",
        powershellStressCommand
      ]
    invalidCommands =
      [ "stack test adrai:adrai-stress-test",
        "stack test adrai:adrai-stress-test --test-arguments=--run-stress --test-arguments=--pattern=focused",
        "stack test adrai:adrai-stress-test --test-arguments=--run-stress=true",
        "stack test adrai:adrai-stress-test --test-arguments=prefix--run-stress",
        "stack test adrai:adrai-stress-test --test-arguments=\"--run-stress",
        "stack test adrai:adrai-stress-test --test-arguments=\"--run-stress \\\"unterminated\""
      ]

testPowerShellStressEvidenceArgv :: Assertion
testPowerShellStressEvidenceArgv =
  decodeStressCommandArguments powershellStressCommand
    @?= Just
      [ "--run-stress",
        "--pattern=12,000-commit repository with 2,000 ADR operations"
      ]

decodeStressCommandArguments :: Text -> Maybe [Text]
decodeStressCommandArguments command = do
  commandWords <- shellWords command
  case commandWords of
    "stack" : "test" : "adrai:adrai-stress-test" : arguments -> decodeStressTestArguments arguments
    _ -> Nothing

decodeStressTestArguments :: [Text] -> Maybe [Text]
decodeStressTestArguments arguments = exactlyOneTestArguments arguments >>= shellWords

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
  case decodeJsonBytes bytes of
    Left problem -> assertFailure (path <> " is invalid: " <> problem) >> fail "unreachable"
    Right value -> pure value

decodeJsonBytes :: Aeson.FromJSON value => BS.ByteString -> Either String value
decodeJsonBytes = Aeson.eitherDecodeStrict' . stripUtf8Bom

stripUtf8Bom :: BS.ByteString -> BS.ByteString
stripUtf8Bom bytes
  | utf8Bom `BS.isPrefixOf` bytes = BS.drop (BS.length utf8Bom) bytes
  | otherwise = bytes
  where
    utf8Bom = BS.pack [0xEF, 0xBB, 0xBF]

testUtf8BomDecoding :: Assertion
testUtf8BomDecoding = do
  let json = "{\"ok\":true}"
      utf8Bom = BS.pack [0xEF, 0xBB, 0xBF]
      decoded = Aeson.eitherDecodeStrict' json :: Either String Aeson.Value
  decodeJsonBytes (utf8Bom <> json) @?= decoded
  stripUtf8Bom json @?= json
  stripUtf8Bom (utf8Bom <> utf8Bom <> json) @?= utf8Bom <> json
  case (decodeJsonBytes (utf8Bom <> utf8Bom <> json) :: Either String Aeson.Value) of
    Left _ -> pure ()
    Right _ -> assertFailure "only one leading BOM may be removed"
  case (decodeJsonBytes (utf8Bom <> "{") :: Either String Aeson.Value) of
    Left _ -> pure ()
    Right _ -> assertFailure "malformed JSON must remain rejected after BOM stripping"

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

exactClosureStates :: [Text]
exactClosureStates = ["covered", "installed-haskell-equivalent", "not-applicable"]

exactGapStates :: [Text]
exactGapStates = ["planned", "partial"]

exactPhases :: [Text]
exactPhases = ["P2", "P3", "P4", "P5", "P6", "P7"]

exactTypes :: [Text]
exactTypes = ["unit", "property", "integration", "golden", "e2e", "cache-selection"]

exactTypeDirectories :: [(Text, Text)]
exactTypeDirectories =
  [ ("unit", "test/unit/"),
    ("property", "test/property/"),
    ("integration", "test/integration/"),
    ("golden", "test/golden/"),
    ("e2e", "test/e2e/"),
    ("cache-selection", "test/cache-selection/")
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
      "test/support/Adrai/Fixture/Relevance.hs"
      "P3",
    DynamicGenerator
      "large-stress"
      "ADRAI_1_Source/tests/test_large.py"
      "test/support/Adrai/Fixture/LargeStress.hs"
      "P6"
  ]

powershellStressCommand :: Text
powershellStressCommand =
  "stack test adrai:adrai-stress-test --test-arguments='--run-stress --pattern=\"12,000-commit repository with 2,000 ADR operations\"'"

cacheSelectionCommand :: Text
cacheSelectionCommand =
  "stack test adrai:adrai-cache-selection-test --test-arguments='--pattern=\"production cache selection.exact CLI reports immutable archive reuse\"'"

unsupportedComponentCommand :: Text
unsupportedComponentCommand =
  "stack test adrai:unapproved-test --test-arguments='--pattern=fixture'"
