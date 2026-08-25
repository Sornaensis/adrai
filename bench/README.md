# Adrai benchmarks

`adrai-bench` is a Criterion benchmark component. It is deliberately separate
from the Tasty correctness and stress suites: use those suites to establish
behavioural correctness, and this component to measure selected production
workloads.

## Component checks

These Stack-native checks build or enumerate the component without taking
benchmark samples:

```sh
stack build adrai:bench:adrai-bench
stack bench adrai:bench:adrai-bench --ba "--list"
stack bench adrai:bench:adrai-bench --ba "--help"
stack test adrai:adrai-benchmark-registration-test
```

`adrai-benchmark-registration-test` is a fast source-level Tasty guard. It
checks the benchmark component registration, the six stable Criterion names,
the `-N1`/eventlog RTS contract, and this document's artifact/migration
contract. It neither constructs a benchmark fixture nor takes a Criterion
sample, and it deliberately has no time, allocation, or heap threshold.

## One-run artifact capture

Run one selected workload at a time. The following commands use a
POSIX-compatible interactive shell (including Git Bash on Windows); they are
commands to type, not a repository script.

```sh
run_dir=".adrai/benchmarks/20260820T120000Z-current-search-warm"
mkdir -p "$run_dir"
stack --version | tee "$run_dir/stack-version.txt"
stack ghc -- --numeric-version | tee "$run_dir/compiler-version.txt"
stack path --compiler-exe | tee "$run_dir/compiler-path.txt"
stack path --snapshot-pkg-db | tee "$run_dir/snapshot-pkg-db.txt"
cp stack.yaml stack.yaml.lock "$run_dir/"
stack bench adrai:bench:adrai-bench --ba "--match prefix adrai/search/current-warm-2000-adr --json $run_dir/criterion.json --csv $run_dir/criterion.csv" \
  2>&1 | tee "$run_dir/stack-bench.log"
```

Use a fresh, caller-owned `run_dir` per invocation. `criterion.json`,
`criterion.csv`, Stack's identity files, `stack.yaml`, `stack.yaml.lock`, and
its combined command log all belong to that one directory; do not combine
outputs from separate invocations. The copied configuration and lock file,
effective snapshot package database, and actual GHC version are the evidence
for resolver/compiler comparisons. The `.adrai/` directory is ignored, so
machine-specific reports are never committed.

`STACK_ROOT` must be writable, and Stack invocations must be serialized. A
shared Stack root has a single Pantry writer lock, so concurrent Stack work can
block or invalidate a run.

## Reproducibility policy

- Time benchmarks use the ordinary, non-profiled component. It embeds `-N1` as
  its RTS default to avoid machine-wide capability differences. A caller may
  deliberately override it with `+RTS -N4 -RTS`, but must record that choice in
  the run directory and may compare results only with the same RTS setting.
- A workload must declare its state in its name and setup. A `cold` workload
  uses a fresh per-sample target; a `warm` workload builds its deterministic
  fixture and reaches its documented ready state before Criterion starts timing
  the operation. Fixture construction is never silently included in a warm
  measurement.
- There is no ad-hoc external warm-up loop. Criterion performs its own
  calibration and sampling; any required warm transition is a single,
  documented part of the warm fixture setup. The workload implementation must
  keep that setup outside the timed action.
- Compare only non-profiled runs with the same deterministic fixture, benchmark
  selection, cold/warm policy, RTS setting, resolver, and compiler.
- Reports are comparative and host-specific observations, not CI pass/fail
  thresholds. Record the machine and any intentional RTS override with the
  artifact directory before comparing two runs.

## Native artifact contract

The caller chooses a new ignored directory under `.adrai/benchmarks/`; no
PowerShell script creates or interprets native benchmark artifacts. Criterion
creates `criterion.json` and `criterion.csv` from the `--json` and `--csv`
arguments. GHC RTS creates the `.prof`, `.hp`, and `.eventlog` profiling files
from the explicit `+RTS` options. The surrounding identity/configuration copies
and combined command log are evidence files for that same single run.

Do not manufacture a Criterion result file, concatenate rows from multiple
runs, or use PowerShell's legacy `performance.json`/`performance.csv` format
as a native baseline. A valid timing artifact is the matching Criterion
JSON/CSV pair for one exact benchmark selection. A valid profiling artifact is
the selected workload's raw RTS output plus the profile command log and copied
Stack identity/configuration files. All of these live below `.adrai/`, which is
ignored by Git.

| Artifact | Native producer | Format and use |
| --- | --- | --- |
| `criterion.json` | Criterion `--json` | Criterion's own JSON schema, retained verbatim for machine-readable estimates from one selected workload. |
| `criterion.csv` | Criterion `--csv` | Criterion's own CSV export for the same invocation; it is paired with that JSON and never merged across runs. |
| `stack-version.txt`, `compiler-version.txt`, `compiler-path.txt`, `snapshot-pkg-db.txt`, `stack.yaml`, `stack.yaml.lock`, `stack-bench.log` | Stack plus the caller's native shell redirection/copy commands | Plain-text/configuration provenance for the corresponding timing or profile run. |
| `.prof`, `.hp`, `.eventlog` | GHC RTS | Raw cost-centre, heap-sample, and eventlog profiling artifacts; profiling-only and not comparable with Criterion timing estimates. |

Criterion and GHC own their file schemas. This repository intentionally adds no
PowerShell-defined report wrapper, median row, process-tree sample, or
time/heap acceptance threshold around them.

## Legacy profile mapping

The former PowerShell measurement profiles were retired after native Criterion
JSON/CSV and selected-workload Stack profiling artifacts were verified. They
were not native benchmarks; the table records the historical migration boundary
so no archived report is mistaken for a Criterion comparison.

The native workflow above replaces the retired measurement harness. The table
below remains as historical context for interpreting prior reports; it does not
refer to runnable legacy profiles or files.

| Legacy profile | Native mapping | Scope decision |
| --- | --- | --- |
| `default` | None | Intentionally excluded: it times the entire correctness suite and Stack/test-process startup rather than one production operation. |
| `retrieval-2k` | `adrai/corpus/construction-2000-adr`, `adrai/sqlite/replacement-warm-2000-adr`, `adrai/search/current-cold-2000-adr`, and `adrai/search/current-warm-2000-adr` | Provides isolated 2,000-ADR corpus construction, warmed SQLite replacement, and current-search operations. |
| `compiler-search-storage` | `adrai/sqlite/replacement-warm-2000-adr`, plus the 2,000-ADR search workloads | Retains SQLite replacement/search materialization measurement; mixed compiler/test-suite and Stack-process costs are intentionally excluded. |
| `cli-heavy` | None | Intentionally excluded: CLI mutation coverage measures executable/process-tree behaviour, not the library workloads owned by this component. |

The legacy 2,000-ADR fixture materialization and multi-probe mix remain uncovered; there is no 2,000-ADR relevance cold/warm workload. The six-ADR relevance benchmarks are only a non-scale-equivalent proxy, not a replacement for those gaps.

Use the native workload names in this table with `stack bench` selection and
the native artifact commands above. The registration guard keeps these names,
the component configuration, and this mapping visible without converting
host-specific observations into correctness tests.

## Diagnostic profiling entry points

Profiling is for attribution, not timing comparison. `stack bench --profile`
rebuilds the benchmark and local libraries with profiling enabled and changes
their run-time behaviour; do not compare a profile's elapsed time, allocation
totals, or Criterion estimates with an ordinary Criterion run. Never place
profiling artifacts in a timing run directory: create a fresh,
profiling-specific directory and capture its identity/configuration before
profiling one selected workload.

`adrai-profile` is a separate, Criterion-free executable for one-action
attribution. It prepares the frozen 2,000-ADR SQLite/index/vector fixture,
performs a major GC, then fully forces exactly one production
`current-warm-2000-adr` public projection. It emits eventlog markers immediately
before and after that action. Setup still appears in a whole-process cost-centre
report, but the driver log and markers make the setup/action boundary explicit;
the action is never inside Criterion calibration or a sample loop. Ordinary
`stack bench` remains the only source of timing JSON/CSV.

The default `--workload current-warm-2000-adr` invocation deliberately keeps
that single-action contract. For action-isolation evidence, run the optional
`--mode action-batch --iterations N` profile and its paired
`--mode fixture-control --iterations N` profile. Both modes construct and
fully force the identical warm fixture, perform one major GC, and emit the same
number of outer and per-iteration eventlog-marker boundaries. The action batch
executes and fully forces the exact public warm-search projection `N` times;
the fixture-only control does not call it. Each driver log line records the
action/control iteration's monotonic `duration-ns` between its start and
complete markers. Use the control only to identify driver/marker-loop overhead,
not as a timing result or a value to subtract from a Criterion measurement.

The profile executable embeds `-N1` as its default RTS setting, and every
profile command below passes `-N1` explicitly as provenance. Compare action and
control only when they have the same iteration count, resolver/compiler,
profile way, and RTS settings.

The shared fixture constructor fully forces the materialization and reusable
vector corpus before returning. Criterion uses that exact forcing checksum for
its environment, and the profile driver receives the same fixture-ready state
before its major GC and `action-start` marker.

```sh
profile_dir=".adrai/benchmarks/20260820T121000Z-current-search-warm-profile"
mkdir -p "$profile_dir"
stack --version | tee "$profile_dir/stack-version.txt"
stack ghc -- --numeric-version | tee "$profile_dir/compiler-version.txt"
stack path --compiler-exe | tee "$profile_dir/compiler-path.txt"
stack path --snapshot-pkg-db | tee "$profile_dir/snapshot-pkg-db.txt"
cp stack.yaml stack.yaml.lock "$profile_dir/"
```

First build the named profile executable and its local-library dependencies.
Stack 3.11's `run` command does not select a named component, so this is a
two-step workflow: `stack build --profile` compiles the profile way, and
`stack exec --profile adrai-profile` selects that same profile-way install
root before running the executable. Unlike `stack bench --profile`, `stack
exec` does not add `-p`; the explicit RTS
arguments below therefore request the cost-centre report. `-po` pins the
report destination under the caller-owned profile directory.

```sh
stack build --profile adrai:exe:adrai-profile \
  2>&1 | tee "$profile_dir/stack-profile-build.log"
stack exec --profile adrai-profile -- --workload current-warm-2000-adr +RTS -N1 -p -po$profile_dir/current-warm -RTS \
  2>&1 | tee "$profile_dir/profile-driver.log"
```

The profile-enabled driver writes `$profile_dir/current-warm.prof`. It contains
fixture setup and one selected warm action; use the driver's setup-complete log
line and the action markers to focus attribution on the latter. It is not a
timing result. For a cost-centre heap profile, start a separate fresh profiling
directory and capture its identity/configuration before running the heap
command:

### Paired action-isolation profile

Use this only after the single-action profile shows that setup still obscures
the action in a whole-process report. Choose a modest, identical positive `N`
for both commands (eight is an example), retain both raw reports and logs, and
inspect the repeated `action-N-start`/`action-N-complete` marker spans against
the matching `fixture-control-N-*` spans. These are attribution artifacts, not
Criterion samples.

```sh
isolation_dir=".adrai/benchmarks/20260821T120000Z-current-search-warm-isolation-profile"
mkdir -p "$isolation_dir"
stack --version | tee "$isolation_dir/stack-version.txt"
stack ghc -- --numeric-version | tee "$isolation_dir/compiler-version.txt"
stack path --compiler-exe | tee "$isolation_dir/compiler-path.txt"
stack path --snapshot-pkg-db | tee "$isolation_dir/snapshot-pkg-db.txt"
cp stack.yaml stack.yaml.lock "$isolation_dir/"
stack build --profile adrai:exe:adrai-profile \
  2>&1 | tee "$isolation_dir/stack-profile-build.log"
stack exec --profile adrai-profile -- --workload current-warm-2000-adr --mode action-batch --iterations 8 +RTS -N1 -p -hc -i0.02 -l -po$isolation_dir/action-batch -ol$isolation_dir/action-batch.eventlog -RTS \
  2>&1 | tee "$isolation_dir/action-batch.log"
stack exec --profile adrai-profile -- --workload current-warm-2000-adr --mode fixture-control --iterations 8 +RTS -N1 -p -hc -i0.02 -l -po$isolation_dir/fixture-control -ol$isolation_dir/fixture-control.eventlog -RTS \
  2>&1 | tee "$isolation_dir/fixture-control.log"
```

The paired run must produce non-empty `action-batch.prof`, `action-batch.hp`,
`action-batch.eventlog`, `fixture-control.prof`, `fixture-control.hp`, and
`fixture-control.eventlog`, plus the two driver logs. A useful action-isolation
claim cites both profiles, their matched `N`, and the eventlog marker spans; it
does not treat the batch duration as a replacement for a Criterion estimate.

### EVID-02 action/control handoff

Do not select a production optimization from one action/control pair.
EVID-02 must collect at least three paired same-condition action/control profile
runs. Every pair must use the same workload, positive `iterations` value,
profile way, resolver/compiler provenance, and explicit `+RTS -N1` setting.
Keep the pairs separate: an action report is only comparable with the control
report from its own `pair-NN` directory.

The following caller-owned shell commands create the minimum three-pair
evidence set. They intentionally rebuild the profile component and recapture
the complete Stack/compiler/executable provenance for every pair; incremental
builds are expected. Do not change `iterations`, the workload, or the RTS
arguments between pairs. `sha256sum` records the exact profile executable used
for each action/control pair; accept the set only when all three hashes match.

```sh
evidence_root=".adrai/benchmarks/20260821T120000Z-current-search-warm-evid-02"
iterations=8

for pair in 01 02 03; do
  pair_dir="$evidence_root/pair-$pair"
  mkdir -p "$pair_dir"
  stack --version | tee "$pair_dir/stack-version.txt"
  stack ghc -- --numeric-version | tee "$pair_dir/compiler-version.txt"
  stack path --compiler-exe | tee "$pair_dir/compiler-path.txt"
  stack path --snapshot-pkg-db | tee "$pair_dir/snapshot-pkg-db.txt"
  cp stack.yaml stack.yaml.lock "$pair_dir/"
  stack build --profile adrai:exe:adrai-profile \
    2>&1 | tee "$pair_dir/stack-profile-build.log"
  stack exec --profile -- sh -c 'command -v adrai-profile' \
    | tee "$pair_dir/adrai-profile-path.txt"
  profile_exe="$(cat "$pair_dir/adrai-profile-path.txt")"
  sha256sum "$profile_exe" | tee "$pair_dir/adrai-profile.sha256"
  stack exec --profile adrai-profile -- --workload current-warm-2000-adr --mode action-batch --iterations "$iterations" +RTS -N1 -p -hc -i0.02 -l -po"$pair_dir/action-batch" -ol"$pair_dir/action-batch.eventlog" -RTS \
    2>&1 | tee "$pair_dir/action-batch.log"
  stack exec --profile adrai-profile -- --workload current-warm-2000-adr --mode fixture-control --iterations "$iterations" +RTS -N1 -p -hc -i0.02 -l -po"$pair_dir/fixture-control" -ol"$pair_dir/fixture-control.eventlog" -RTS \
    2>&1 | tee "$pair_dir/fixture-control.log"
done
```

Before the EVID-02 handoff, retain all three `pair-NN` directories and verify
that each contains non-empty action/control `.prof`, `.hp`, and `.eventlog`
files; both logs; `adrai-profile-path.txt`; `adrai-profile.sha256`; and the
Stack/compiler/configuration provenance files above. Record the three identical
executable hashes and each pair's matching condition in the investigation
record. If a hash or provenance field differs, discard that comparison and
collect a new same-condition three-pair set; do not average, subtract, or
otherwise combine mismatched pairs.

```sh
profile_dir=".adrai/benchmarks/20260820T122000Z-current-search-warm-heap-profile"
mkdir -p "$profile_dir"
stack --version | tee "$profile_dir/stack-version.txt"
stack ghc -- --numeric-version | tee "$profile_dir/compiler-version.txt"
stack path --compiler-exe | tee "$profile_dir/compiler-path.txt"
stack path --snapshot-pkg-db | tee "$profile_dir/snapshot-pkg-db.txt"
cp stack.yaml stack.yaml.lock "$profile_dir/"
```

```sh
stack build --profile adrai:exe:adrai-profile \
  2>&1 | tee "$profile_dir/stack-heap-profile-build.log"
stack exec --profile adrai-profile -- --workload current-warm-2000-adr +RTS -N1 -p -hc -i0.02 -l -po$profile_dir/current-warm-heap -ol$profile_dir/current-warm-heap.eventlog -RTS \
  2>&1 | tee "$profile_dir/profile-driver.log"
```

That command writes these raw artifacts in the stated directory:

- `current-warm-heap.prof`: cost-centre time and allocation report.
- `current-warm-heap.hp`: legacy cost-centre-stack live-heap samples.
- `current-warm-heap.eventlog`: eventlog containing heap samples and the
  driver's `action-start` / `action-complete` markers, suitable for
  interactive inspection.
- `stack-heap-profile-build.log`: Stack profile-way build output.
- `profile-driver.log`: driver output, including its setup/action boundary.

`-hc` attributes live heap to the cost-centre stack that produced it; `-i0.02`
requests a 20 ms heap-sample interval; `-l` enables eventlog output; `-po`
sets the common `.prof`/`.hp` stem; and `-ol` pins the eventlog path. The
explicit `-p` supplies the cost-centre profile. GHC 9.10 supports both output
options. The driver accepts only `--workload current-warm-2000-adr`; do not add
a second workload to a profile invocation. Change only the new directory name
when repeating the same attribution command.

### Inspecting and retaining profiles

First confirm the raw artifact is non-empty and that the report header records
the expected executable, selected workload, and RTS setting. In a `.prof`,
start with the total allocation and the cost-centre rows with material
individual or inherited `%alloc` / `%time`; then follow their parent/child
stack. In a `.hp`, look for sustained live-heap bands rather than a single
short setup spike. The reported live heap is not the same thing as operating
system process memory, and profile samples are perturbed by profiling itself.

If the relevant GHC tools are installed, render copies beside the raw files;
keep the raw files as the source evidence:

```sh
hp2ps "$profile_dir/current-warm-heap.hp"
eventlog2html "$profile_dir/current-warm-heap.eventlog"
```

`hp2ps` renders the legacy `.hp` data to PostScript. `eventlog2html` renders
the eventlog to an interactive HTML report; its output filename is tool-version
dependent, so retain both the command log and its generated HTML alongside the
raw eventlog. Absence of either optional renderer does not invalidate the raw
`.prof`, `.hp`, or `.eventlog` artifact.

Keep a completed profiling directory intact while it supports an investigation:
the raw reports, eventlog, copied Stack configuration, identity files, and
logs are the minimum evidence required to reproduce an attribution. The
repository ignores `.adrai/`; never commit those machine-specific raw or
rendered artifacts. Delete the entire caller-owned run directory only after
its conclusions have been recorded elsewhere. Do not keep profiles that
combine different workloads, resolver/compiler identities, or RTS capability
settings under one result claim.

## Workload catalogue

Every workload uses a deterministic fixture and keeps the listed setup outside
Criterion's timed action. Each workload owns an isolated Criterion
`envWithCleanup` fixture, so Criterion parses `--help`, `--list`, and selection
arguments before it constructs a 2,000-ADR materialization, opens SQLite, or
builds a reusable corpus. Selecting one workload therefore never keeps another
workload's database or corpus live.

- `adrai/corpus/construction-2000-adr` builds and fully forces a reusable
  vector corpus from the frozen 2,000-logical-ADR retrieval materialization.
  The materialization itself is constructed before measurement.
- `adrai/sqlite/replacement-warm-2000-adr` replaces the same 2,000-ADR search
  materialization in an already initialized in-memory SQLite schema. It
  measures clear-and-repopulate work, not opening a database or creating its
  schema.
- `adrai/search/current-cold-2000-adr` builds a new vector corpus during every
  current-search invocation, then runs the hybrid tail probe against the
  preloaded 2,000-ADR SQLite FTS indexes. Its `warm` counterpart,
  `adrai/search/current-warm-2000-adr`, reuses the corpus built and fully
  forced during fixture setup. Both fully force the emitted public JSON.
- `adrai/relevance/cold-six-adr` builds a new corpus during every relevance
  invocation for the frozen `cache-python` source. Its `warm` counterpart,
  `adrai/relevance/warm-six-adr`, reuses the corpus prepared for the separately
  preloaded six-ADR quality fixture. Both variants use that fixture's SQLite
  indexes and fully force their public JSON.

The `cold` names mean a new in-memory vector corpus is built in each timed
invocation; the materialization and SQLite indexes remain the deterministic,
preloaded fixture. The `warm` names mean a reusable corpus has reached the
stated ready state before Criterion begins. Neither state hides an external
warm-up loop. The SQLite replacement case is intentionally warm because it
measures index replacement rather than database/schema creation. Use `--list`
to enumerate the exact selectable names before collecting artifacts.
