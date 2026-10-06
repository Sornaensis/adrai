# Testing

ADRAI has four retained test components. A complete gate runs every registered
test in each component, enables the stress component explicitly, and then runs
the reliability repeats recorded in `test/coverage/retained-suite.json`.

`tools/RunRetainedTests.ps1` owns the retained-test scheduler on both platforms.
On Windows it gives each child process its own Windows Job Object. The child is created
suspended, assigned to the Job Object, and resumed only after assignment
succeeds. Closing or terminating that job therefore includes the child's Git,
CLI, console, and helper descendants without selecting unrelated processes by
name.

The Windows provider retains PowerShell 5.1 compatibility. Linux loads a separate
PowerShell 7/.NET provider and a caller-selected native helper; it requires a
permitted unprivileged user/PID namespace, single-ID mapping, pidfds and
`PR_SET_PDEATHSIG`. Missing facilities fail admission rather than falling back
to process groups. The helper keeps namespace PID1 alive to reap descendants,
including detached children, and separates payload exec acknowledgement/root
exit from namespace reaping and joined status/diagnostic readers. A private
controller-only channel couples PowerShell caller death to cancellation; the
native creating thread and its stable pidfd cover PID1 parent-death races.
Namespace activity is reported separately from an unknown member count, and
controller PIDs are diagnostic metadata rather than PID-based cleanup targets.

Prepare the Linux helper outside the repository with the selected C compiler:

```sh
"$CC" -std=c11 -O2 -Wall -Wextra -Werror tools/RetainedTests/linux_owner.c -o "$owner_exe"
PWSH_EXE=/absolute/path/to/pwsh ADRAI_RETAINED_OWNER_EXE="$owner_exe" \
  sh tools/run-retained-tests.sh -Mode SelfCheck -RepositoryRoot "$repo" \
  -EvidenceDirectory "$scratch/selfcheck"
```

The adjacent `$owner_exe.json` build record must have `schemaVersion: 1`, the
lowercase SHA-256 `sourceSha256` and `binarySha256`, and the selected compiler's
absolute `compilerPath`, lowercase `compilerSha256` and `compilerVersion`.
Build manifests bind the runner/provider/helper sources, helper binary and this
record independently of the five product artifacts. Finite cleanup that cannot
verify namespace reaping and reader joins remains a cleanup failure; disposing
an object is not a tree-absence assertion. Windows and Linux x86_64 (amd64)
are supported with the stated toolchain and ownership facilities. The POSIX
implementation boundary does not promise other Unix systems or architectures;
local owner checks alone do not establish complete-host acceptance.

For reviewed exported validation inputs, `-SourceReferencePath` explicitly binds
the canonical repository root, workspace UUID, commit and exact exported file
hashes. The actual Linux build root stays separate; an export is not a Git
repository and receives no synthetic Git or hmem metadata. Native checkouts
continue to resolve their real Git HEAD normally.
The reference declares `scope: "tools"` for a tool-only SelfCheck or
`scope: "product-and-tools"` for Build/List/Focused/Complete. The latter must
cover every actual build input and all ownership tool sources. Reference identity
and hash are frozen at admission and checked again after execution.

## Build and artifact manifest

Builds and test execution use separate deadlines. Build mode compiles every
component with pedantic checks while disabling test and benchmark execution:

```powershell
$evidenceRoot = Join-Path ([IO.Path]::GetTempPath()) ("adrai-" + [guid]::NewGuid())
New-Item -ItemType Directory -Path $evidenceRoot | Out-Null
.\tools\RunRetainedTests.ps1 `
  -Mode Build `
  -RepositoryRoot D:\Projects\adrai `
  -StackExe C:\path\to\stack.exe `
  -AdraiExe C:\path\to\adrai.exe `
  -OrdinaryTestExe C:\path\to\adrai-test.exe `
  -CacheSelectionTestExe C:\path\to\adrai-cache-selection-test.exe `
  -StressTestExe C:\path\to\adrai-stress-test.exe `
  -BenchmarkRegistrationTestExe C:\path\to\adrai-benchmark-registration-test.exe `
  -BuildManifestPath "$evidenceRoot\build-manifest.json"
```

All executable paths must be absolute and must resolve to the corresponding
component under the repository's Stack `--dist-dir`; caller-selected copies or
unrelated binaries are rejected. Build and artifact-discovery commands clear
inherited `STACK_YAML` and pass the repository's hashed `stack.yaml` explicitly.
Before compilation, build mode records a
deterministic path and SHA-256 inventory of every file under `src`, `app`,
`bench`, and `test`, the Elm sources and tests, static bridge/bootstrap/style,
web build scripts/manifests, shared API fixture, generated app and provenance
receipt, plus the Stack and package configuration. It verifies that
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

Complete mode defaults to one monotonic 1,800-second deadline; an explicit
`-DeadlineSeconds` may allocate up to 3,600 seconds. One coordinator runs with
at most three active owned test roots. Every root is still an isolated
`TASTY_NUM_THREADS=1`, `GHCRTS=-N1` process with its own platform ownership scope; retained
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
  -OrdinaryPartitionsPath D:\Projects\adrai\test\coverage\ordinary-partitions.json `
  -BuildManifestPath "$evidenceRoot\build-manifest.json" `
  -AdraiExe C:\path\to\adrai.exe `
  -OrdinaryTestExe C:\path\to\adrai-test.exe `
  -CacheSelectionTestExe C:\path\to\adrai-cache-selection-test.exe `
  -StressTestExe C:\path\to\adrai-stress-test.exe `
  -BenchmarkRegistrationTestExe C:\path\to\adrai-benchmark-registration-test.exe
```

The queue has seventeen normal jobs in this exact order: `O`, `Rest`, `Q`, `N`,
`R`, `T`, `Env`, `A`, `CompilerSearch`, `K`, `MutationE2E`, `D`, cache-selection,
`C`, stress, `E`, and benchmark-registration. The initial wave follows this source-defined order. `Krace` and the unchanged
named reliability repeat are the final two exclusive jobs. Each exclusive job
is a barrier: the coordinator drains active jobs before launching it and does
not launch another job until its complete descendant tree exits. Cache, stress
(with `--run-stress`), and benchmark-registration remain whole-component jobs
in the same bounded queue.
The runner derives the required job-ID
multiset from the verified partitions, retained components, and expanded repeat
counts and requires exact equality with the configured queue before dispatch.
Quoted command lines are checked against the Windows 32,767-character limit.

Derive unique registration totals from the current ledger and each component's
fresh List. Expand the ledger's repeat counts to derive planned executions.
Only Complete establishes actual execution; the runner does not hardcode these
totals. A fresh matching build must list every actual registration and prove
exact equality before dispatch.
The Complete gate uses its one shared selected deadline, including
listing, setup, dispatch, and owned-descendant cleanup. It is a finite liveness
guard, not a product latency target or a promise about another checkout or host.
Only a complete run on a frozen build and matching ledgers establishes execution;
a timeout or unfinished suffix remains incomplete.

Focused mode selects one exact registered leaf with a 600-second maximum
liveness guard. It is leaf evidence, not whole-job or aggregate acceptance.

Exclusive scheduling controls observed load; it does not change the lock timeout
or serialize the competing-target test's two real CLI children. Each `Krace` or
repeat execution retains the genuine simultaneous-target pair. Runtime evidence
must confirm all source-derived counts against the frozen executable snapshot.

If one queued process times out, exits unsuccessfully, leaves a descendant, or
fails its expected Tasty count, dispatch stops. The coordinator requests
termination of every active platform ownership scope before waiting for cleanup,
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

List mode has a 60-second maximum deadline. Focused mode has a 600-second
default and maximum deadline. Both use the same artifact and registration
checks. Focused mode accepts one exact ledger test name rather than a free-form
Tasty selector:

```powershell
.\tools\RunRetainedTests.ps1 -Mode Focused `
  -Component adrai-test -TestName 'exact Tasty test path' `
  -RepositoryRoot D:\Projects\adrai `
  -LedgerPath D:\Projects\adrai\test\coverage\retained-suite.json `
  -OrdinaryPartitionsPath D:\Projects\adrai\test\coverage\ordinary-partitions.json `
  -BuildManifestPath "$evidenceRoot\build-manifest.json" `
  -AdraiExe C:\path\to\adrai.exe `
  -OrdinaryTestExe C:\path\to\adrai-test.exe `
  -CacheSelectionTestExe C:\path\to\adrai-cache-selection-test.exe `
  -StressTestExe C:\path\to\adrai-stress-test.exe `
  -BenchmarkRegistrationTestExe C:\path\to\adrai-benchmark-registration-test.exe
```

All logs, manifests, fixture workspaces, and profiling output belong in fresh
OS temporary directories outside the repository. Set process-local TEMP/TMP to
an external OS temporary directory before running native tests. Keep the single
standard root .stack-work for compilation and preserve product runtime caches.
Each run writes JSON evidence and separate stdout/stderr files under a new
temporary directory unless `-EvidenceDirectory` supplies another path outside
the repository. Evidence records artifact hashes, exact arguments, process IDs,
durations, exit codes, timeout/orphan classification, and cleanup verification.

Run `npm --prefix web run verify:assets` before the canonical Haskell Build.
It recompiles optimized Elm in an owned temporary location and compares the
generated bundle and input receipt without publishing. The Haskell asset
module independently validates that receipt at compile time, including the
recursive web source set. A warm Haskell Build must reject changed or newly
added web source, bridge, or bootstrap inputs until the bundle is rebuilt. List, Focused, and
Complete then reject any input drift relative to the Haskell build manifest.
Run `npm test` from `browser-tests` for six independent production-Elm browser
workflows. They use small deterministic HTTP fixtures and causal promises rather
than real Git/server setup, per-case deadlines, or matrix admission receipts.
The supported command disables Playwright case, expectation, action, and navigation
timeouts explicitly. Compilation and all browser output go to a fresh OS temporary
directory printed by the launcher; owned browser contexts, HTTP servers, and child
processes are cleaned up on success and failure. See [browser workflows](../browser-tests/README.md)
for the precise UI assurance and filtering command.

The component and browser npm entrypoints select Windows PowerShell 5.1 on Windows or the
absolute `PWSH_EXE` on Linux, and pass their actual Node executable explicitly.
Linux requires the source-bound native owner and its verified build sidecar in
`ADRAI_RETAINED_OWNER_EXE`. Set external executable `TEMP`, `TMP`, and `TMPDIR`
roots, `ELM_HOME`, and `PLAYWRIGHT_BROWSERS_PATH` before launch. Dependencies and
browser binaries must match the locks; use the local Playwright CLI to install
Chromium, without silently installing system packages. These controlled fixtures
retain pinned Playwright's default Chromium launch options, including its
inherited `--no-sandbox`. This is not a sandbox-enabled or general browsing
security claim; do not add bypass flags, privileges, or broader namespace rules.

`ADRAI_FRONTEND_REMAINING_MS` passes the caller's remaining execution and cleanup
allocation (standalone default: 3,600,000 milliseconds). The Node dispatcher
passes an absolute expiry through PowerShell startup and checks completion
against its monotonic clock. A combined host gate must own the entire chain and
pass the remaining shared allocation at each stage. Test case and step timeouts
remain disabled; the aggregate bound is a liveness and cleanup guard.

The component supervisor also supports its existing `-Probe timeout`,
`spawn-failure`, `early-success`, and `early-error` modes through
`npm --prefix web run test:components -- -Probe <mode>`. The timeout-named probe
requests cancellation after the descendant's actual readiness signal; it does
not assert a startup speed. Provider cleanup proves the owned tree has exited.
The dispatcher maps SIGINT and SIGTERM to Node's default child termination
request (SIGTERM on Linux, forceful termination on Windows). PowerShell's
`finally` is not guaranteed by that request. Forced dispatcher death requires
an enclosing owned runner; standalone npm is not an independent caller-death
supervisor. Asset build, verification, and tests remain direct Node commands;
their enclosing owner supplies forced-caller-death and finite process cleanup.
The asset tests reserve cleanup within the supplied allocation and reject late
completion without adding per-case speed limits.

The historical P7-04/P7-05 real-server browser specs and supervisors remain outside
default discovery. Their paging, conflicts, reconnect, and real-service integration
claims are not claims of these deterministic UI workflows. Native HTTP/WebSocket
and Elm component tests continue to exercise their separate documented scopes.

The P7-05 exact-archive regression leaves exercise simultaneous validators
and mixed HTTP consumers. When an exact SQLite archive is temporarily locked,
safe reads return a typed `repository-busy` 503 and can recover at the same
revision. A mutation that already committed retains its commit and reports an
indexing warning if publication cannot finish; exact archive validation remains
required before successful reads.

The accepted stress redesign reduces previous large capacity fixtures to the
retained sizes declared by the test ledger. This gate proves the retained risk
coverage and its real process boundaries; it no longer provides the deleted
2,000-ADR and 12,000-commit capacity evidence.
