# Testing

ADRAI has four retained test components. A complete gate runs every registered
test in each component, enables the stress component explicitly, and then runs
the reliability repeats recorded in `test/coverage/retained-suite.json`.

`tools/RunRetainedTests.ps1` is the only supported launcher for retained tests.
It gives each child process its own Windows Job Object. The child is created
suspended, assigned to the Job Object, and resumed only after assignment
succeeds. Closing or terminating that job therefore includes the child's Git,
CLI, console, and helper descendants without selecting unrelated processes by
name.

## Build and artifact manifest

Builds and test execution use separate deadlines. Build mode compiles every
component with pedantic checks while disabling test and benchmark execution:

```powershell
.\tools\RunRetainedTests.ps1 `
  -Mode Build `
  -RepositoryRoot D:\Projects\adrai `
  -StackExe C:\path\to\stack.exe `
  -AdraiExe C:\path\to\adrai.exe `
  -OrdinaryTestExe C:\path\to\adrai-test.exe `
  -CacheSelectionTestExe C:\path\to\adrai-cache-selection-test.exe `
  -StressTestExe C:\path\to\adrai-stress-test.exe `
  -BenchmarkRegistrationTestExe C:\path\to\adrai-benchmark-registration-test.exe `
  -BuildManifestPath C:\temp\adrai-build-manifest.json
```

All executable paths must be absolute and must resolve to the corresponding
component under the repository's Stack `--dist-dir`; caller-selected copies or
unrelated binaries are rejected. Build and artifact-discovery commands clear
inherited `STACK_YAML` and pass the repository's hashed `stack.yaml` explicitly.
Before compilation, build mode records a
deterministic path and SHA-256 inventory of every file under `src`, `app`,
`bench`, and `test`, plus the Stack and package configuration. It verifies that
inventory again after compilation. The generated manifest also records Git
HEAD and each executable path and SHA-256 hash. List, focused, and complete
modes recompute the full input inventory and reject a changed input, stale
configuration, moved executable, or changed executable. This includes new or
untracked Haskell modules. The manifest and run evidence must be outside the
repository.

## Retained-suite ledger

The runner consumes `test/coverage/retained-suite.json`; it does not generate or
repair that file. The initial schema is:

```json
{
  "schemaVersion": 1,
  "components": [
    {
      "name": "adrai-test",
      "executableRole": "ordinary",
      "args": [],
      "tests": ["exact Tasty test path"]
    },
    {
      "name": "adrai-cache-selection-test",
      "executableRole": "cacheSelection",
      "args": [],
      "tests": ["exact Tasty test path"]
    },
    {
      "name": "adrai-stress-test",
      "executableRole": "stress",
      "args": ["--run-stress"],
      "tests": ["exact Tasty test path"]
    },
    {
      "name": "adrai-benchmark-registration-test",
      "executableRole": "benchmarkRegistration",
      "args": [],
      "tests": ["exact Tasty test path"]
    }
  ],
  "repeats": [
    { "component": "adrai-test", "test": "exact Tasty test path", "count": 2 }
  ]
}
```

The four names and executable roles are fixed. Ordinary, cache-selection, and
benchmark-registration arguments must be empty. Stress arguments must be
exactly `--run-stress`. Selectors, skip options, and arbitrary component
arguments are rejected. Tests must be nonempty and unique within a component;
every repeat must name a retained test. Complete mode obtains the actual full
registration with `--list-tests` under the component's fixed enablement and
compares exact names before scheduling it. Missing, added, or duplicate
registration fails the gate. Source counts are never treated as run evidence.

`test/coverage/ordinary-partitions.json` is a checked scheduling overlay for
the ordinary component. It lists fifteen disjoint ordinary jobs, their compact
Tasty selectors, every exact expected test name, and the complete queue order.
Complete mode proves that the partition lists are pairwise disjoint and that
their union equals the full ordinary registration. It then runs each selector
with `--list-tests` and requires the selected names to match exactly. The
overlay cannot add, omit, or duplicate a ledger test, component, or repeat.
`K` is the exact complement of the singleton `Krace` inside Cache integration.
`Krace` contains only `competing tree-identical targets retain their own
provenance projections` and is exclusive. Query, Environment, compiled search
SQLite, and Mutation E2E remain intact resource groups; `Rest` is their checked
complement with the other anchored ordinary groups.

## Running the gate

Complete mode uses one monotonic 600-second deadline and one coordinator with
at most three active owned test roots. Every root is still an isolated
`TASTY_NUM_THREADS=1`, `GHCRTS=-N1` process with its own Job Object; retained
concurrency inside an individual test remains unchanged. The timer starts before
runner setup, type compilation, artifact hashing, ledger validation, or evidence
directory creation and is never reset for a component or repeat. It includes
enumeration, all test processes, fixture setup performed by those processes,
descendant teardown, evidence finalization, and cleanup verification:

```powershell
.\tools\RunRetainedTests.ps1 `
  -Mode Complete `
  -RepositoryRoot D:\Projects\adrai `
  -LedgerPath D:\Projects\adrai\test\coverage\retained-suite.json `
  -BuildManifestPath C:\temp\adrai-build-manifest.json `
  -AdraiExe C:\path\to\adrai.exe `
  -OrdinaryTestExe C:\path\to\adrai-test.exe `
  -CacheSelectionTestExe C:\path\to\adrai-cache-selection-test.exe `
  -StressTestExe C:\path\to\adrai-stress-test.exe `
  -BenchmarkRegistrationTestExe C:\path\to\adrai-benchmark-registration-test.exe
```

The queue has seventeen normal jobs in this exact order: `Q`, `N`, `R`, `T`,
`Env`, `A`, `CompilerSearch`, `K`, `O`, `MutationE2E`, `D`, cache-selection,
`C`, `Rest`, stress, `E`, and benchmark-registration. `Krace` and the unchanged
named reliability repeat are the final two exclusive jobs. Each exclusive job
is a barrier: the coordinator drains active jobs before launching it and does
not launch another job until its complete descendant tree exits. Cache, stress
(with `--run-stress`), and benchmark-registration remain whole-component jobs
in the same bounded queue.
The runner derives the required job-ID
multiset from the verified partitions, retained components, and expanded repeat
counts and requires exact equality with the configured queue before dispatch.
Quoted command lines are checked against the Windows 32,767-character limit.

The current source ledger declares 852 unique ordinary tests, 9 cache-selection
tests, 28 stress tests, and 6 benchmark-registration tests: 895 unique
registrations. The explicit competing-target repeat makes 896 executions. The
runner does not hardcode these totals. A fresh matching build must list every
actual registration and prove exact equality before dispatch.

The Round8 2026-09-07 snapshot baseline is historical diagnostic evidence for
that frozen input set. It is not an elapsed-time forecast for another snapshot:

| Job | Current tests | Round8 2026-09-07 snapshot baseline |
| --- | ---: | --- |
| `Q` | 2 | Completed in 125.203237 seconds. |
| `N` | 70 | The former 77-test job completed in 197.706567 seconds. |
| `R` | 98 | The former 111-test job completed in 494.973748 seconds. |
| `T` | 47 | Completed in 220.011431 seconds. |
| `Env` | 2 | Completed in 75.019434 seconds. |
| `A` | 23 | Completed in 120.079787 seconds. |
| `CompilerSearch` | 7 | Completed in 46.302814 seconds. |
| `K` | 9 | The former 13-test job completed in 165.228843 seconds. |
| `O` | 35 | The former 43-test job was censored at 193.192355 seconds. |
| `MutationE2E` | 3 | Censored at 90.343516 seconds. |
| `D` | 2 | The former 3-test job was censored at 29.257487 seconds. |
| `cache` | 9 | Unstarted. |
| `C` | 4 | Unstarted. |
| `Rest` | 547 | Unstarted. |
| `stress` | 28 | Unstarted; the gate still requires actual `--run-stress`. |
| `E` | 2 | Unstarted. |
| `registration` | 6 | Unstarted. |
| `Krace` | 1 | Unstarted. |
| `repeat-001-001` | 1 | Unstarted. |

The Round8 baseline stopped incomplete at 590.032027 seconds without an observed
functional failure. All 30 invocation cleanups and scheduler cleanup passed,
with no orphan, input drift, or registry residue. Its result is
`C:/Users/Sornaensis/AppData/Local/Temp/adrai-retained-complete-round8-20260907T183433935Z/result.json`
(SHA-256 `492C90EE69F0096AAA5D1BA0767EDF1F06738A24D9ACD14852BF3C52082C7C47`).
Censored and unstarted jobs retain unknown positive work. Each changed snapshot
requires its own Complete result; static removal and process counts are not
seconds or evidence of a guaranteed fit.

The current gate has one shared 600-second deadline. It includes the four
full-component listings, fifteen selected-partition listings, 17 normal FIFO jobs
with at most three active, the actual stress opt-in, the exclusive genuine race
and repeat, setup, type loading, hashing, validation, descendant cleanup, and
dispatch overhead.
Only a complete run on one frozen build and matching ledgers can establish the
current queue time; timeout or an unfinished suffix remains incomplete.

The round-nine changed-functional matrix has four exact ordinary leaves:

- `ADRAI.P4-02.Repository snapshots.nonblob config is rejected`.
- `ADRAI.P4-07.Cache integration (P4-07).v3 exact archive rejects missing or mismatched target placement coverage`.
- `ADRAI.P5-05.Mutation E2E across hostile environments (P5-05).P6-02 compact real executable command wiring.public commands preserve a staged binary and return canonical exits`.
- `ADRAI.P5-05.Mutation E2E across hostile environments (P5-05).P6-03A.0 real executable ordinary amend reconciliation.ordinary amend reconciles merged decision heads`.

The cache carrier also checks private old-schema eligibility rejection. Deleted
N, R, and overlay cases add no focused repetitions, and unchanged race, stress,
and query cases are not repeated.
Every selected run remains an exact ledger leaf through Focused mode with a
maximum 60-second deadline; a focused result is leaf evidence, not a whole-job
measurement.

Exclusive scheduling controls observed load; it does not change the lock timeout
or serialize the competing-target test's two real CLI children. Each `Krace` or
repeat execution retains the genuine simultaneous-target pair. Runtime evidence
must confirm all source-derived counts against the frozen executable snapshot.

If one queued process times out, exits unsuccessfully, leaves a descendant, or
fails its expected Tasty count, dispatch stops. The coordinator requests
termination of every active owned Job Object before waiting for any cleanup,
then verifies all trees against the remaining time in the same global deadline.
It never extends the deadline or selects unrelated processes by name.

The runner removes inherited `TASTY_*` options, sets only
`TASTY_NUM_THREADS=1`, sets `GHCRTS=-N1`, and supplies the frozen `ADRAI_EXE`
to children. A successful component must also report exactly the number of
executed tests recorded in the verified ledger, so a listing-only process
cannot satisfy the gate. Every test component is built with `-N1`; concurrency
inside an individual retained sentinel remains controlled by that test. The runner reserves cleanup time
inside the deadline. Timeout, nonzero exit, missing or unexpected registration,
an omitted stress opt-in, a root process that exits while descendants remain,
or cleanup that cannot be confirmed makes the result fail or incomplete.

List and focused modes have a maximum 60-second deadline and use the same
artifact and registration checks. Focused mode accepts one exact ledger test
name rather than a free-form Tasty selector:

```powershell
.\tools\RunRetainedTests.ps1 -Mode Focused `
  -Component adrai-test -TestName 'exact Tasty test path' `
  -RepositoryRoot D:\Projects\adrai `
  -LedgerPath D:\Projects\adrai\test\coverage\retained-suite.json `
  -BuildManifestPath C:\temp\adrai-build-manifest.json `
  -AdraiExe C:\path\to\adrai.exe `
  -OrdinaryTestExe C:\path\to\adrai-test.exe `
  -CacheSelectionTestExe C:\path\to\adrai-cache-selection-test.exe `
  -StressTestExe C:\path\to\adrai-stress-test.exe `
  -BenchmarkRegistrationTestExe C:\path\to\adrai-benchmark-registration-test.exe
```

Each run writes JSON evidence and separate stdout/stderr files under a new
temporary directory unless `-EvidenceDirectory` supplies another path outside
the repository. Evidence records artifact hashes, exact arguments, process IDs,
durations, exit codes, timeout/orphan classification, and cleanup verification.

The accepted stress redesign reduces previous large capacity fixtures to the
retained sizes declared by the test ledger. This gate proves the retained risk
coverage and its real process boundaries; it no longer provides the deleted
2,000-ADR and 12,000-commit capacity evidence.
