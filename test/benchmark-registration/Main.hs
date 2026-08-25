module Main (main) where

import Control.Monad (forM_)
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import System.Directory (doesFileExist, listDirectory)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase)

main :: IO ()
main =
  defaultMain . testGroup "benchmark registration" $
    [ testCase "native benchmark component is registered" benchmarkComponentRegistration,
      testCase "one-action profile driver is Criterion-free and registered" profileDriverRegistration,
      testCase "retired measurement scripts remain absent" retiredMeasurementScriptsAbsent,
      testCase "all selectable workloads have stable isolated setup" workloadRegistration,
      testCase "native artifacts and legacy profile mapping are documented" artifactAndLegacyDocumentation,
      testCase "EVID-02 paired profile handoff is documented" evid02HandoffDocumentation
    ]

benchmarkComponentRegistration :: Assertion
benchmarkComponentRegistration = do
  packageBenchmark <- packageBenchmarkStanza <$> readRequired "package.yaml"
  cabalBenchmark <- cabalBenchmarkStanza <$> readRequired "adrai.cabal"
  forM_ packageRegistrationRequirements $ \needle ->
    assertContains packageBenchmark needle ("adrai-bench package.yaml stanza is missing: " <> needle)
  forM_ cabalRegistrationRequirements $ \needle ->
    assertContains cabalBenchmark needle ("adrai-bench Cabal stanza is missing: " <> needle)

profileDriverRegistration :: Assertion
profileDriverRegistration = do
  packageExecutable <- packageExecutableStanza <$> readRequired "package.yaml"
  cabalExecutable <- cabalExecutableStanza <$> readRequired "adrai.cabal"
  profileMain <- readRequired "bench/ProfileMain.hs"
  profileDriver <- readRequired "bench/Adrai/Benchmark/ProfileDriver.hs"
  sharedFixture <- readRequired "bench/Adrai/Benchmark/CurrentSearch.hs"
  benchmarkMain <- readRequired "bench/Main.hs"
  forM_ profilePackageRegistrationRequirements $ \needle ->
    assertContains packageExecutable needle ("adrai-profile package.yaml stanza is missing: " <> needle)
  forM_ profileCabalRegistrationRequirements $ \needle ->
    assertContains cabalExecutable needle ("adrai-profile Cabal stanza is missing: " <> needle)
  assertContains profileMain "ProfileDriver.main" "adrai-profile must delegate to the profile driver"
  forM_ profileDriverRequirements $ \needle ->
    assertContains profileDriver needle ("profile driver is missing one-action contract: " <> needle)
  forM_ actionIsolationRequirements $ \needle ->
    assertContains profileDriver needle ("profile driver is missing action-isolation instrumentation: " <> needle)
  assertNotContains profileDriver "import Criterion" "profile driver must not import Criterion"
  assertNotContains profileDriver "Criterion.Main" "profile driver must not run Criterion"
  forM_ sharedFixtureReadinessRequirements $ \needle ->
    assertContains sharedFixture needle ("shared current-search fixture is missing ready-state forcing: " <> needle)
  assertContains benchmarkMain "rnf fixture = forceCurrentSearchFixture fixture `seq` ()" "Criterion must use the shared current-search fixture forcing API"

retiredMeasurementScriptsAbsent :: Assertion
retiredMeasurementScriptsAbsent = do
  entries <- listDirectory legacyMeasurementScriptDirectory
  let legacyPaths =
        [ legacyMeasurementScriptDirectory <> "/" <> entry
          | entry <- entries,
            legacyMeasurementScriptPrefix `isPrefixOf` entry,
            ".ps1" `isSuffixOf` entry
        ]
  assertBool
    ("retired measurement scripts must not reappear under " <> legacyMeasurementScriptDirectory <> ": " <> show legacyPaths)
    (null legacyPaths)

workloadRegistration :: Assertion
workloadRegistration = do
  benchmarkMain <- benchmarkTreeSource <$> readRequired "bench/Main.hs"
  benchmarkReadme <- readRequired "bench/README.md"
  assertBool "benchmark main must contain exactly the registered adrai Criterion tree" (benchmarkMain == exactCriterionTree)
  forM_ selectableCriterionPaths $ \path ->
    assertContains benchmarkReadme ("`" <> path <> "`") ("benchmark documentation is missing selectable Criterion path " <> path)

artifactAndLegacyDocumentation :: Assertion
artifactAndLegacyDocumentation = do
  benchmarkReadme <- readRequired "bench/README.md"
  forM_ exactArtifactDocumentation $ \needle ->
    assertContains benchmarkReadme needle ("benchmark documentation is missing native artifact contract: " <> needle)
  assertBool
    "every documented adrai-bench invocation must pass its benchmark arguments through one quoted --ba option"
    ( all (isInfixOf "--ba \"") (stackBenchInvocationLines benchmarkReadme)
        && not (null (stackBenchInvocationLines benchmarkReadme))
    )
  forM_ legacyBenchmarkForwardingForms $ \legacyForm ->
    assertNotContains benchmarkReadme legacyForm ("benchmark documentation must use Stack 3.11 --ba forwarding, not: " <> legacyForm)
  forM_ exactLegacyRows $ \row ->
    assertContains benchmarkReadme row ("benchmark documentation is missing legacy mapping row: " <> row)

evid02HandoffDocumentation :: Assertion
evid02HandoffDocumentation = do
  benchmarkReadme <- readRequired "bench/README.md"
  forM_ evid02HandoffRequirements $ \needle ->
    assertContains benchmarkReadme needle ("benchmark documentation is missing EVID-02 handoff requirement: " <> needle)

selectableCriterionPaths :: [String]
selectableCriterionPaths =
  [ "adrai/corpus/construction-2000-adr",
    "adrai/sqlite/replacement-warm-2000-adr",
    "adrai/search/current-cold-2000-adr",
    "adrai/search/current-warm-2000-adr",
    "adrai/relevance/cold-six-adr",
    "adrai/relevance/warm-six-adr"
  ]

exactCriterionTree :: String
exactCriterionTree =
  "main::IO()main=dosetLocaleEncodingutf8defaultMain[bgroup\"adrai\"[bgroup\"corpus\"[envWithCleanupprepareCorpusFixturediscardFixture(\\fixture->bench\"construction-2000-adr\"(nf(forceCorpus.buildSearchVectorCorpus)(corpusFixtureMaterializationfixture)))],bgroup\"sqlite\"[envWithCleanupprepareSqliteFixturecloseSqliteFixture(\\fixture->bench\"replacement-warm-2000-adr\"(whnfIO(sqliteReplacementfixture)))],bgroup\"search\"[envWithCleanup(prepareCurrentSearchFixtureFalse)closeCurrentSearchFixture(\\fixture->bench\"current-cold-2000-adr\"(nfIO(currentSearchColdfixture))),envWithCleanup(prepareCurrentSearchFixtureTrue)closeCurrentSearchFixture(\\fixture->bench\"current-warm-2000-adr\"(nfIO(currentSearchWarmfixture)))],bgroup\"relevance\"[envWithCleanup(prepareRelevanceFixtureFalse)closeRelevanceFixture(\\fixture->bench\"cold-six-adr\"(nfIO(relevanceSearchColdfixture))),envWithCleanup(prepareRelevanceFixtureTrue)closeRelevanceFixture(\\fixture->bench\"warm-six-adr\"(nfIO(relevanceSearchWarmfixture)))]]]"

packageRegistrationRequirements :: [String]
packageRegistrationRequirements =
  [ "main: Main.hs",
    "source-dirs:\n      - bench\n      - test/support",
    "- adrai\n      - criterion\n      - deepseq",
    "ghc-options: [-threaded, -rtsopts, -eventlog, -with-rtsopts=-N1]"
  ]

cabalRegistrationRequirements :: [String]
cabalRegistrationRequirements =
  [ "type: exitcode-stdio-1.0",
    "main-is: Main.hs",
    "hs-source-dirs:\n      bench\n      test/support",
    ", criterion",
    ", deepseq",
    "ghc-options: -threaded -rtsopts -eventlog -with-rtsopts=-N1"
  ]

profilePackageRegistrationRequirements :: [String]
profilePackageRegistrationRequirements =
  [ "main: ProfileMain.hs",
    "source-dirs:\n      - bench\n      - test/support",
    "- Adrai.Benchmark.CurrentSearch\n      - Adrai.Benchmark.ProfileDriver",
    "dependencies: [adrai]",
    "ghc-options: [-threaded, -rtsopts, -eventlog, -with-rtsopts=-N1]"
  ]

profileCabalRegistrationRequirements :: [String]
profileCabalRegistrationRequirements =
  [ "main-is: ProfileMain.hs",
    "Adrai.Benchmark.CurrentSearch",
    "Adrai.Benchmark.ProfileDriver",
    "hs-source-dirs:\n      bench\n      test/support",
    "ghc-options: -threaded -rtsopts -eventlog -with-rtsopts=-N1"
  ]

profileDriverRequirements :: [String]
profileDriverRequirements =
  [ "withCurrentSearchWarmFixture",
    "performMajorGC",
    "traceMarkerIO \"adrai-profile: current-warm-2000-adr action-start\"",
    "traceMarkerIO \"adrai-profile: current-warm-2000-adr action-complete\"",
    "ByteString.length output",
    "[\"--workload\", \"current-warm-2000-adr\"]"
  ]

actionIsolationRequirements :: [String]
actionIsolationRequirements =
  [ "ActionBatch Int",
    "FixtureOnlyControl Int",
    "withCurrentSearchWarmFixture",
    "performMajorGC",
    "getMonotonicTimeNSec",
    "action-batch-start",
    "action-batch-complete",
    "fixture-control-start",
    "fixture-control-complete",
    "duration-ns=",
    "--mode\", \"action-batch\", \"--iterations\"",
    "--mode\", \"fixture-control\", \"--iterations\"",
    "parsePositiveIterations"
  ]

sharedFixtureReadinessRequirements :: [String]
sharedFixtureReadinessRequirements =
  [ "forceCurrentSearchFixture",
    "evaluate (forceCurrentSearchFixture preparedFixture)",
    "withCurrentSearchWarmFixture = bracket (prepareCurrentSearchFixture True) closeCurrentSearchFixture"
  ]

legacyMeasurementScriptDirectory :: FilePath
legacyMeasurementScriptDirectory = "tools"

legacyMeasurementScriptPrefix :: String
legacyMeasurementScriptPrefix = "MeasurePerformance"

exactArtifactDocumentation :: [String]
exactArtifactDocumentation =
  [ "stack bench adrai:bench:adrai-bench --ba \"--list\"",
    "stack bench adrai:bench:adrai-bench --ba \"--help\"",
    "stack test adrai:adrai-benchmark-registration-test",
    "under `.adrai/benchmarks/`",
    "Criterion\ncreates `criterion.json` and `criterion.csv` from the `--json` and `--csv`",
    "GHC RTS creates the `.prof`, `.hp`, and `.eventlog` profiling files",
    "Reports are comparative and host-specific observations, not CI pass/fail\n  thresholds.",
    "`STACK_ROOT` must be writable, and Stack invocations must be serialized.",
    "`adrai-profile` is a separate, Criterion-free executable for one-action\nattribution.",
    "stack build --profile adrai:exe:adrai-profile",
    "stack exec --profile adrai-profile -- --workload current-warm-2000-adr +RTS -N1 -p -po$profile_dir/current-warm -RTS",
    "stack exec --profile adrai-profile -- --workload current-warm-2000-adr +RTS -N1 -p -hc -i0.02 -l -po$profile_dir/current-warm-heap -ol$profile_dir/current-warm-heap.eventlog -RTS",
    "`--mode action-batch --iterations N` profile",
    "`--mode fixture-control --iterations N` profile",
    "stack exec --profile adrai-profile -- --workload current-warm-2000-adr --mode action-batch --iterations 8 +RTS -N1 -p -hc -i0.02 -l -po$isolation_dir/action-batch -ol$isolation_dir/action-batch.eventlog -RTS",
    "stack exec --profile adrai-profile -- --workload current-warm-2000-adr --mode fixture-control --iterations 8 +RTS -N1 -p -hc -i0.02 -l -po$isolation_dir/fixture-control -ol$isolation_dir/fixture-control.eventlog -RTS",
    "The paired run must produce non-empty `action-batch.prof`, `action-batch.hp`,",
    "hp2ps \"$profile_dir/current-warm-heap.hp\"",
    "eventlog2html \"$profile_dir/current-warm-heap.eventlog\"",
    "The former PowerShell measurement profiles were retired after native Criterion\nJSON/CSV and selected-workload Stack profiling artifacts were verified.",
    "The native workflow above replaces the retired measurement harness."
  ]

legacyBenchmarkForwardingForms :: [String]
legacyBenchmarkForwardingForms =
  [ "stack bench adrai:bench:adrai-bench -- --list",
    "stack bench adrai:bench:adrai-bench -- --help",
    "stack bench adrai:bench:adrai-bench -- \\",
    "stack bench adrai:bench:adrai-bench --profile -- \"--match",
    "stack run adrai:exe:adrai-profile --profile",
    "stack bench ... -- --list"
  ]

exactLegacyRows :: [String]
exactLegacyRows =
  [ "| `default` | None |",
    "| `retrieval-2k` | `adrai/corpus/construction-2000-adr`, `adrai/sqlite/replacement-warm-2000-adr`, `adrai/search/current-cold-2000-adr`, and `adrai/search/current-warm-2000-adr` |",
    "| `compiler-search-storage` | `adrai/sqlite/replacement-warm-2000-adr`, plus the 2,000-ADR search workloads |",
    "| `cli-heavy` | None |",
    "The legacy 2,000-ADR fixture materialization and multi-probe mix remain uncovered; there is no 2,000-ADR relevance cold/warm workload. The six-ADR relevance benchmarks are only a non-scale-equivalent proxy, not a replacement for those gaps."
  ]

evid02HandoffRequirements :: [String]
evid02HandoffRequirements =
  [ "EVID-02 must collect at least three paired same-condition action/control profile\nruns.",
    "same workload, positive `iterations` value,\nprofile way, resolver/compiler provenance, and explicit `+RTS -N1` setting.",
    "an action report is only comparable with the control\nreport from its own `pair-NN` directory.",
    "for pair in 01 02 03; do",
    "stack exec --profile -- sh -c 'command -v adrai-profile'",
    "sha256sum \"$profile_exe\" | tee \"$pair_dir/adrai-profile.sha256\"",
    "--mode action-batch --iterations \"$iterations\" +RTS -N1",
    "--mode fixture-control --iterations \"$iterations\" +RTS -N1",
    "accept the set only when all three hashes match.",
    "`adrai-profile-path.txt`; `adrai-profile.sha256`; and the\nStack/compiler/configuration provenance files above.",
    "do not average, subtract, or\notherwise combine mismatched pairs."
  ]

packageBenchmarkStanza :: String -> String
packageBenchmarkStanza = requiredStanza "benchmarks:" "  adrai-bench:" isYamlSibling . lines

cabalBenchmarkStanza :: String -> String
cabalBenchmarkStanza = requiredStanza "benchmark adrai-bench" "benchmark adrai-bench" isCabalStanzaBoundary . lines

packageExecutableStanza :: String -> String
packageExecutableStanza = requiredStanza "executables:" "  adrai-profile:" isYamlSibling . lines

cabalExecutableStanza :: String -> String
cabalExecutableStanza = requiredStanza "executable adrai-profile" "executable adrai-profile" isCabalStanzaBoundary . lines

requiredStanza :: String -> String -> (String -> Bool) -> [String] -> String
requiredStanza parentHeader stanzaHeader boundary lines' =
  unlines . takeUntil boundary . drop 1 . dropWhile (/= stanzaHeader) $ parentLines
  where
    parentLines = dropWhile (/= parentHeader) lines'

takeUntil :: (String -> Bool) -> [String] -> [String]
takeUntil boundary = takeWhile (not . boundary)

isYamlSibling :: String -> Bool
isYamlSibling line =
  "  " `isPrefixOf` line
    && not ("    " `isPrefixOf` line)

isCabalStanzaBoundary :: String -> Bool
isCabalStanzaBoundary line =
  not (null line)
    && not (isSpace (head line))

compact :: String -> String
compact = filter (not . isSpace)

benchmarkTreeSource :: String -> String
benchmarkTreeSource source =
  compact . unlines . takeWhile (/= "prepareCorpusFixture :: IO CorpusFixture") . dropWhile (/= "main :: IO ()") $ lines source

stackBenchInvocationLines :: String -> [String]
stackBenchInvocationLines =
  filter (isInfixOf "stack bench adrai:bench:adrai-bench") . lines

readRequired :: FilePath -> IO String
readRequired path = do
  exists <- doesFileExist path
  if exists
    then readFile path
    else assertFailure ("Required benchmark contract file is missing: " <> path) >> pure ""

assertContains :: String -> String -> String -> Assertion
assertContains haystack needle message =
  assertBool message (needle `isInfixOf` haystack)

assertNotContains :: String -> String -> String -> Assertion
assertNotContains haystack needle message =
  assertBool message (not (needle `isInfixOf` haystack))
