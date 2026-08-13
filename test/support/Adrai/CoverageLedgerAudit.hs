{-# LANGUAGE OverloadedStrings #-}

module Adrai.CoverageLedgerAudit
  ( LedgerAudit,
    auditGapRows,
    auditIsClosed,
    auditLedgerAt,
    renderAuditReport,
    requireLedgerClosedAt,
    writeCurrentLedgerReport,
    requireCurrentLedgerClosed
  )
where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?))
import qualified Data.ByteString as ByteString
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>), isRelative, splitDirectories, takeDirectory, takeExtension)
import System.IO (stderr)

data LedgerAudit = LedgerAudit
  { auditExpectedTotal :: Int,
    auditActualTotal :: Int,
    auditGapRows :: [GapRow]
  }
  deriving (Eq, Show)

data GapRow = GapRow
  { gapKind :: Text.Text,
    gapCategory :: Text.Text,
    gapPhase :: Text.Text,
    gapState :: Text.Text,
    gapPythonPath :: Text.Text,
    gapPythonMethod :: Text.Text,
    gapHaskellTestPath :: Text.Text,
    gapHaskellTestName :: Text.Text,
    gapDetail :: Text.Text
  }
  deriving (Eq, Ord, Show)

data LedgerManifest = LedgerManifest
  { manifestSchema :: Text.Text,
    manifestExpectedTotal :: Int,
    manifestCategories :: [CategorySpec],
    manifestAllowedStates :: [Text.Text],
    manifestClosureStates :: [Text.Text],
    manifestGapStates :: [Text.Text],
    manifestAllowedPhases :: [Text.Text],
    manifestAllowedTypes :: [Text.Text],
    manifestAllowedDirectories :: [Text.Text]
  }

instance Aeson.FromJSON LedgerManifest where
  parseJSON = Aeson.withObject "coverage ledger manifest" $ \value ->
    LedgerManifest
      <$> value .: "schema"
      <*> value .: "expectedTotal"
      <*> value .: "categories"
      <*> value .: "allowedStates"
      <*> value .: "closureStates"
      <*> value .: "gapStates"
      <*> value .: "allowedPhaseOwners"
      <*> value .: "allowedHaskellTestTypes"
      <*> value .: "allowedHaskellTestDirectories"

data CategorySpec = CategorySpec
  { categoryFile :: FilePath,
    categoryName :: Text.Text,
    categoryExpectedCount :: Int
  }
  deriving (Eq, Show)

instance Aeson.FromJSON CategorySpec where
  parseJSON = Aeson.withObject "coverage ledger category specification" $ \value ->
    CategorySpec
      <$> value .: "file"
      <*> value .: "category"
      <*> value .: "expectedCount"

data CategoryFragment = CategoryFragment
  { fragmentSchema :: Text.Text,
    fragmentCategory :: Text.Text,
    fragmentExpectedCount :: Int,
    fragmentSourceModules :: [SourceModule],
    fragmentRows :: [CoverageRow]
  }

instance Aeson.FromJSON CategoryFragment where
  parseJSON = Aeson.withObject "coverage ledger category" $ \value ->
    CategoryFragment
      <$> value .: "schema"
      <*> value .: "category"
      <*> value .: "expectedCount"
      <*> value .: "sourceModules"
      <*> value .: "rows"

data SourceModule = SourceModule
  { sourceModulePythonPath :: Text.Text,
    sourceModuleExpectedCount :: Maybe Int
  }

instance Aeson.FromJSON SourceModule where
  parseJSON value =
    case value of
      Aeson.String path -> pure (SourceModule path Nothing)
      _ -> Aeson.withObject "coverage source module" (\object -> SourceModule <$> object .: "pythonPath" <*> object .:? "expectedCount") value

data CoverageRow = CoverageRow
  { rowPythonPath :: Text.Text,
    rowPythonMethod :: Text.Text,
    rowBehaviorContract :: Text.Text,
    rowHaskellTestName :: Text.Text,
    rowHaskellTestPath :: Text.Text,
    rowHaskellTestType :: Text.Text,
    rowPhaseOwner :: Text.Text,
    rowState :: Text.Text,
    rowEvidence :: Evidence,
    rowTranslationNote :: Maybe Text.Text
  }

instance Aeson.FromJSON CoverageRow where
  parseJSON = Aeson.withObject "coverage ledger row" $ \value ->
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
  { evidenceCommand :: Text.Text,
    evidenceFixture :: Text.Text,
    evidenceResult :: Text.Text
  }

instance Aeson.FromJSON Evidence where
  parseJSON = Aeson.withObject "coverage evidence" $ \value ->
    Evidence
      <$> (fromMaybe "" <$> value .:? "command")
      <*> (fromMaybe "" <$> value .:? "fixture")
      <*> (fromMaybe "" <$> value .:? "result")

auditLedgerAt :: FilePath -> IO (Either Text.Text LedgerAudit)
auditLedgerAt root = do
  manifestResult <- decodeJsonFile (root </> "manifest.json")
  case manifestResult of
    Left problem -> pure (Left problem)
    Right manifest ->
      case validateManifest manifest of
        Left problem -> pure (Left problem)
        Right () -> do
          fragmentsResult <- traverse (loadFragment root) (manifestCategories manifest)
          case sequence fragmentsResult of
            Left problem -> pure (Left problem)
            Right fragments -> do
              let rows = [(category, row) | (category, categoryRows) <- fragments, row <- categoryRows]
              targetGaps <- fmap concat (traverse (targetExistenceGaps root) rows)
              pure $ do
                validateFragments manifest fragments
                let identityGaps = identityValidationGaps rows
                    rowGaps = concatMap rowValidationGaps rows
                    coverageGaps =
                      [ gapFor "coverage" "non-closure-state" category row
                        | (category, row) <- rows,
                          rowState row `Set.member` Set.fromList exactStates,
                          rowState row `notElem` exactClosureStates
                      ]
                    gaps = sortGaps (identityGaps <> rowGaps <> targetGaps <> coverageGaps)
                pure
                  LedgerAudit
                    { auditExpectedTotal = manifestExpectedTotal manifest,
                      auditActualTotal = length rows,
                      auditGapRows = gaps
                    }

auditIsClosed :: LedgerAudit -> Bool
auditIsClosed audit =
  auditExpectedTotal audit == auditActualTotal audit
    && null (auditGapRows audit)

renderAuditReport :: LedgerAudit -> Text.Text
renderAuditReport audit =
  Text.unlines
    ( [ "schema=adrai/coverage-ledger-audit/v1",
        "status=" <> if auditIsClosed audit then "closed" else "open",
        "expected-total=" <> showText (auditExpectedTotal audit),
        "actual-total=" <> showText (auditActualTotal audit),
        "closed-total=" <> showText (auditActualTotal audit - length coverageGaps),
        "gap-total=" <> showText (length gaps)
      ]
        <> map renderGapKind (Map.toAscList gapKinds)
        <> map renderGapState (Map.toAscList gapStates)
        <> map renderGap gaps
    )
  where
    gaps = auditGapRows audit
    coverageGaps = filter ((== "coverage") . gapKind) gaps
    gapKinds = Map.fromListWith (+) [(gapKind gap, 1 :: Int) | gap <- gaps]
    gapStates = Map.fromListWith (+) [(gapState gap, 1 :: Int) | gap <- gaps]

    renderGapKind (kind, count) = "gap-kind=" <> kind <> " count=" <> showText count
    renderGapState (state, count) = "gap-state=" <> state <> " count=" <> showText count
    renderGap gap =
      Text.intercalate
        "\t"
        [ "gap",
          "kind=" <> gapKind gap,
          "category=" <> gapCategory gap,
          "phase=" <> gapPhase gap,
          "state=" <> gapState gap,
          "python-path=" <> gapPythonPath gap,
          "python-method=" <> gapPythonMethod gap,
          "haskell-test-path=" <> gapHaskellTestPath gap,
          "haskell-test-name=" <> gapHaskellTestName gap,
          "detail=" <> gapDetail gap
        ]

writeCurrentLedgerReport :: IO ()
writeCurrentLedgerReport = do
  result <- loadCurrentLedgerAudit
  case result of
    Left problem -> failAudit problem
    Right audit -> TextIO.putStr (renderAuditReport audit)

requireCurrentLedgerClosed :: IO ()
requireCurrentLedgerClosed = do
  rootResult <- findLedgerRoot
  case rootResult of
    Left problem -> failAudit problem
    Right root -> requireLedgerClosedAt root

requireLedgerClosedAt :: FilePath -> IO ()
requireLedgerClosedAt root = do
  result <- auditLedgerAt root
  case result of
    Left problem -> failAudit problem
    Right audit -> do
      TextIO.putStr (renderAuditReport audit)
      if auditIsClosed audit
        then pure ()
        else do
          TextIO.hPutStrLn stderr "coverage ledger is open; see gap rows above"
          exitFailure

validateManifest :: LedgerManifest -> Either Text.Text ()
validateManifest manifest
  | manifestSchema manifest /= "adrai/coverage-ledger/v1" = Left "manifest schema must be adrai/coverage-ledger/v1"
  | manifestExpectedTotal manifest /= 206 = Left "manifest expectedTotal must be 206"
  | manifestCategories manifest /= exactCategories = Left "manifest categories and counts do not match the frozen v1 ledger"
  | manifestAllowedStates manifest /= exactStates = Left "manifest allowedStates does not match the frozen v1 policy"
  | manifestClosureStates manifest /= exactClosureStates = Left "manifest closureStates does not match the frozen v1 policy"
  | manifestGapStates manifest /= exactGapStates = Left "manifest gapStates does not match the frozen v1 policy"
  | manifestAllowedPhases manifest /= exactPhases = Left "manifest allowedPhaseOwners does not match the frozen v1 policy"
  | manifestAllowedTypes manifest /= exactTypes = Left "manifest allowedHaskellTestTypes does not match the frozen v1 policy"
  | manifestAllowedDirectories manifest /= map snd exactTypeDirectories = Left "manifest allowedHaskellTestDirectories does not match the frozen v1 policy"
  | otherwise = Right ()

validateFragments :: LedgerManifest -> [(CategorySpec, [CoverageRow])] -> Either Text.Text ()
validateFragments manifest fragments
  | length fragments /= 6 = Left "ledger must contain exactly six category fragments"
  | sum (map (length . snd) fragments) /= manifestExpectedTotal manifest = Left "ledger row count does not match manifest expectedTotal"
  | otherwise = Right ()

identityValidationGaps :: [(CategorySpec, CoverageRow)] -> [GapRow]
identityValidationGaps rows =
  [ gapFor "validation" "duplicate-python-identity" category row
    | (category, row) <- rows,
      duplicate (rowPythonPath row, rowPythonMethod row) (map (\(_, candidate) -> (rowPythonPath candidate, rowPythonMethod candidate)) rows)
  ]
    <> [ gapFor "validation" "duplicate-haskell-test-name" category row
         | (category, row) <- rows,
           duplicate (rowHaskellTestName row) (map (rowHaskellTestName . snd) rows)
       ]
  where
    duplicate value values = length (filter (== value) values) > 1

rowValidationGaps :: (CategorySpec, CoverageRow) -> [GapRow]
rowValidationGaps (category, row) =
  [ gapFor "validation" detail category row
    | detail <- checks
  ]
  where
    checks =
      [ "invalid-python-path" | not (validPythonPath (rowPythonPath row))
      ]
        <> ["invalid-python-method" | not (validPythonMethod (rowPythonMethod row))]
        <> ["missing-behavior-contract" | not (substantive (rowBehaviorContract row))]
        <> ["missing-haskell-test-name" | not (substantive (rowHaskellTestName row))]
        <> ["invalid-state" | rowState row `notElem` exactStates]
        <> ["invalid-phase-owner" | rowPhaseOwner row `notElem` exactPhases]
        <> ["invalid-haskell-test-type" | rowHaskellTestType row `notElem` exactTypes]
        <> ["invalid-haskell-test-directory" | not (validTestDirectory row)]
        <> ["invalid-haskell-test-path" | not (validRepositoryPath (rowHaskellTestPath row))]
        <> evidenceProblems (rowEvidence row)
        <> translationProblems row

targetExistenceGaps :: FilePath -> (CategorySpec, CoverageRow) -> IO [GapRow]
targetExistenceGaps root (category, row)
  | not (validRepositoryPath (rowHaskellTestPath row)) = pure []
  | otherwise = do
      let repositoryRoot = takeDirectory (takeDirectory (takeDirectory (takeDirectory root)))
      exists <- doesFileExist (repositoryRoot </> Text.unpack (rowHaskellTestPath row))
      pure [gapFor "validation" "missing-haskell-test-target" category row | not exists]

evidenceProblems :: Evidence -> [Text.Text]
evidenceProblems evidence =
  [ "missing-evidence-command" | not (substantive (evidenceCommand evidence)) ]
    <> ["invalid-evidence-command" | substantive (evidenceCommand evidence) && not ("stack test adrai:adrai-test" `Text.isPrefixOf` evidenceCommand evidence)]
    <> ["missing-evidence-fixture" | not (substantive (evidenceFixture evidence))]
    <> ["missing-evidence-result" | not (substantive (evidenceResult evidence))]

translationProblems :: CoverageRow -> [Text.Text]
translationProblems row
  | rowState row `notElem` translationStates = []
  | (rowPythonPath row, rowPythonMethod row, rowState row) `notElem` exactTranslationRows = ["invalid-translation-identity"]
  | maybe True (not . substantive) (rowTranslationNote row) = ["missing-translation-note"]
  | otherwise = []

validTestDirectory :: CoverageRow -> Bool
validTestDirectory row =
  case lookup (rowHaskellTestType row) exactTypeDirectories of
    Nothing -> False
    Just directory -> directory `Text.isPrefixOf` rowHaskellTestPath row

validRepositoryPath :: Text.Text -> Bool
validRepositoryPath path =
  substantive path
    && isRelative (Text.unpack path)
    && takeExtension (Text.unpack path) == ".hs"
    && ".." `notElem` splitDirectories (Text.unpack path)

validPythonPath :: Text.Text -> Bool
validPythonPath path =
  "ADRAI_1_Source/tests/test_" `Text.isPrefixOf` path
    && ".py" `Text.isSuffixOf` path
    && Text.count "/" path == 2
    && ".." `notElem` splitDirectories (Text.unpack path)

validPythonMethod :: Text.Text -> Bool
validPythonMethod method =
  "test_" `Text.isPrefixOf` method
    && substantive method
    && Text.all (\character -> character == '_' || (character >= 'a' && character <= 'z') || (character >= '0' && character <= '9')) method

substantive :: Text.Text -> Bool
substantive value = not (Text.null value) && Text.strip value == value

gapFor :: Text.Text -> Text.Text -> CategorySpec -> CoverageRow -> GapRow
gapFor kind detail category row =
  GapRow
    { gapKind = kind,
      gapCategory = categoryName category,
      gapPhase = rowPhaseOwner row,
      gapState = rowState row,
      gapPythonPath = rowPythonPath row,
      gapPythonMethod = rowPythonMethod row,
      gapHaskellTestPath = rowHaskellTestPath row,
      gapHaskellTestName = rowHaskellTestName row,
      gapDetail = detail
    }

sortGaps :: [GapRow] -> [GapRow]
sortGaps =
  sortOn
    ( \gap ->
        ( gapKind gap,
          gapCategory gap,
          gapPhase gap,
          gapState gap,
          gapPythonPath gap,
          gapPythonMethod gap,
          gapHaskellTestPath gap,
          gapHaskellTestName gap,
          gapDetail gap
        )
    )

loadCurrentLedgerAudit :: IO (Either Text.Text LedgerAudit)
loadCurrentLedgerAudit = do
  rootResult <- findLedgerRoot
  case rootResult of
    Left problem -> pure (Left problem)
    Right root -> auditLedgerAt root

findLedgerRoot :: IO (Either Text.Text FilePath)
findLedgerRoot = getCurrentDirectory >>= search
  where
    search directory = do
      let candidate = directory </> "test" </> "coverage" </> "ledger" </> "v1"
      hasManifest <- doesFileExist (candidate </> "manifest.json")
      isDirectory <- doesDirectoryExist candidate
      if hasManifest && isDirectory
        then pure (Right candidate)
        else
          let parent = takeDirectory directory
           in if parent == directory
                then pure (Left "could not locate test/coverage/ledger/v1/manifest.json")
                else search parent

loadFragment :: FilePath -> CategorySpec -> IO (Either Text.Text (CategorySpec, [CoverageRow]))
loadFragment root category = do
  fragmentResult <- decodeJsonFile (root </> categoryFile category)
  pure $ do
    fragment <- fragmentResult
    if fragmentSchema fragment /= "adrai/coverage-ledger-category/v1"
      then Left ("category fragment has invalid schema: " <> Text.pack (categoryFile category))
      else
        if fragmentCategory fragment /= categoryName category
          then Left ("category fragment identity does not match manifest: " <> Text.pack (categoryFile category))
          else
            if fragmentExpectedCount fragment /= categoryExpectedCount category || length (fragmentRows fragment) /= categoryExpectedCount category
              then Left ("category fragment count does not match manifest: " <> Text.pack (categoryFile category))
              else do
                validateSourceModules category fragment
                Right (category, fragmentRows fragment)

validateSourceModules :: CategorySpec -> CategoryFragment -> Either Text.Text ()
validateSourceModules category fragment
  | modulePaths /= Set.fromList (map rowPythonPath (fragmentRows fragment)) = Left ("category sourceModules do not match row anchors: " <> Text.pack (categoryFile category))
  | length sourceModules /= Set.size modulePaths = Left ("category sourceModules contain duplicate anchors: " <> Text.pack (categoryFile category))
  | any invalidCount sourceModules = Left ("category sourceModules expectedCount does not match rows: " <> Text.pack (categoryFile category))
  | otherwise = Right ()
  where
    sourceModules = fragmentSourceModules fragment
    modulePaths = Set.fromList (map sourceModulePythonPath sourceModules)
    rowCounts = Map.fromListWith (+) [(rowPythonPath row, 1 :: Int) | row <- fragmentRows fragment]
    invalidCount sourceModule =
      case sourceModuleExpectedCount sourceModule of
        Nothing -> False
        Just expectedCount -> Map.lookup (sourceModulePythonPath sourceModule) rowCounts /= Just expectedCount

decodeJsonFile :: Aeson.FromJSON value => FilePath -> IO (Either Text.Text value)
decodeJsonFile path = do
  exists <- doesFileExist path
  if not exists
    then pure (Left ("missing coverage ledger file: " <> Text.pack path))
    else do
      bytes <- ByteString.readFile path
      pure $
        case Aeson.eitherDecodeStrict' (stripUtf8Bom bytes) of
          Left problem -> Left (Text.pack path <> " is invalid: " <> Text.pack problem)
          Right value -> Right value

stripUtf8Bom :: ByteString.ByteString -> ByteString.ByteString
stripUtf8Bom bytes
  | utf8Bom `ByteString.isPrefixOf` bytes = ByteString.drop (ByteString.length utf8Bom) bytes
  | otherwise = bytes
  where
    utf8Bom = ByteString.pack [0xEF, 0xBB, 0xBF]

failAudit :: Text.Text -> IO ()
failAudit problem = do
  TextIO.hPutStrLn stderr ("coverage ledger audit failed: " <> problem)
  exitFailure

showText :: Show value => value -> Text.Text
showText = Text.pack . show

exactCategories :: [CategorySpec]
exactCategories =
  [ CategorySpec "formats-distribution-footprint.json" "formats-distribution-footprint" 16,
    CategorySpec "graph-service-conflicts-integrity.json" "graph-service-conflicts-integrity" 50,
    CategorySpec "git-transactions-environments-provenance-branches-concurrency.json" "git-transactions-environments-provenance-branches-concurrency" 42,
    CategorySpec "compiler-cache-consistency-usability.json" "compiler-cache-consistency-usability" 28,
    CategorySpec "search-relevance-vectors-history-compare-explorer.json" "search-relevance-vectors-history-compare-explorer" 69,
    CategorySpec "large-stress.json" "large-stress" 1
  ]

exactStates, exactClosureStates, exactGapStates, exactPhases, exactTypes :: [Text.Text]
exactStates = ["planned", "partial", "covered", "installed-haskell-equivalent", "not-applicable"]
exactClosureStates = ["covered", "installed-haskell-equivalent", "not-applicable"]
exactGapStates = ["planned", "partial"]
exactPhases = ["P2", "P3", "P4", "P5", "P6", "P7"]
exactTypes = ["unit", "property", "integration", "golden", "e2e"]

exactTypeDirectories :: [(Text.Text, Text.Text)]
exactTypeDirectories =
  [ ("unit", "test/unit/"),
    ("property", "test/property/"),
    ("integration", "test/integration/"),
    ("golden", "test/golden/"),
    ("e2e", "test/e2e/")
  ]

translationStates :: [Text.Text]
translationStates = ["installed-haskell-equivalent", "not-applicable"]

exactTranslationRows :: [(Text.Text, Text.Text, Text.Text)]
exactTranslationRows =
  [ ("ADRAI_1_Source/tests/test_distribution_cli.py", "test_pyproject_declares_dependency_free_console_entrypoint", "not-applicable"),
    ("ADRAI_1_Source/tests/test_distribution_cli.py", "test_python_module_entrypoint_exposes_the_same_cli", "installed-haskell-equivalent")
  ]
