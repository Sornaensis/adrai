# Real-server browser tests

`p704-smoke.spec.ts` is the compact repository explorer proof. The P7-05 matrix
has exactly B01–B15 in `p705-scenarios.json`, split across the read, mutation,
and live specifications. Discovery must match every ledger ID, title, and file
before any selected test runs.

The matrix uses pinned Playwright 1.61.1 with Chromium 1228, the built `adrai`
executable and optimized Elm bundle. Each test starts its own real Git repository
and loopback web server. The paging case generates 1001 sealed decisions with
the test-only `adrai-window-fixture` executable. Conflict cases create divergent
shared mutation services on two branches and inspect their merged candidate heads through
the authenticated real HTTP API. Staged, unstaged, and untracked caller files
are checked for exact preservation.

On Windows, run only through the owned-job supervisor. For example, after a
fresh canonical build:

```powershell
stack exec powershell.exe -- -NoProfile -ExecutionPolicy Bypass -File browser-tests/support/run-p705-matrix.ps1 -Scenario all -AssetGateReceipt C:\temp\adrai-p705-g01.json -AdraiExe D:\Projects\adrai\.stack-work\dist\1a191874\build\adrai\adrai.exe -WindowFixtureExe D:\Projects\adrai\.stack-work\dist\1a191874\build\adrai-window-fixture\adrai-window-fixture.exe
```

Use `-Scenario B01` through `B15` for a focused diagnostic. The runner discovers
all 15 cases even for a focused run, forces one worker and zero retries, and
requires a persisted fresh G01 asset verification receipt and records exact
source, asset, browser headless shell, and executable hashes in a unique temporary
`result.json`. The aggregate has a 6,795-second deadline: 6,660 seconds of
declared case caps, exactly 75 seconds for discovery and runner overhead, and
60 seconds for owned-job cleanup and finalization. The root work wait excludes
that cleanup reserve. The runner rejects inventory/aggregate arithmetic drift
and records the case sum, overhead, work budget, cleanup reserve, and verified
inventory sum in its receipt. These are finite liveness guards, not product
latency targets. The runner verifies child exit, listener closure, and
removal of the owned temporary root on every result. A failed assertion remains
a failed result even when ownership cleanup succeeds.

## Whole-case liveness accounting

Every case has one finite Playwright timer covering real setup, unchanged browser
work, and per-case teardown. The allocations below explain that single timer;
they are operational assumptions, not separate phase timers or a proved maximum.
All values are seconds. B04's ledger seed remains `linked`; its source executes
both main and linked fixtures under one timer.

| Case | Fixture(s) | Setup | Workflow | Teardown | Case cap |
|---|---|---:|---:|---:|---:|
| B01 | main | 180 | 330 | 20 | 530 |
| B02 | linked | 200 | 90 | 20 | 310 |
| B03 | paging | 330 | 180 | 20 | 530 |
| B04 | main + linked | 440 | 240 | 40 | 720 |
| B05 | conflicts + safe text | 450 | 180 | 20 | 650 |
| B06–B10 | main, each | 180 | 90 | 20 | 290 each |
| B11 | main | 180 | 180 | 20 | 380 |
| B12 | conflicts | 420 | 450 | 20 | 890 |
| B13 | main | 180 | 180 | 20 | 380 |
| B14 | main | 180 | 180 | 20 | 380 |
| B15 | main | 180 | 240 | 20 | 440 |

`support/p705-server.ts` allows initialization 40 seconds, both real sequential
CLI creates 30 seconds each, and web readiness 25 seconds (125 seconds). Ordinary
setup also has fourteen sequential Git commands: five initial repository commands,
four sentinel construction/check commands, and five initial sentinel checks.
Each retains its existing 20-second child guard. The main setup policy assumes
2 seconds per Git command (28 seconds) plus 27 seconds of filesystem, parsing,
and scheduling work: 125 + 28 + 27 = 180 seconds. This is an operational allowance;
it does not grant all fourteen Git children their full independent ceilings.
Summing all child guards would give 405 seconds before other work and would still
not prove a full setup maximum. An individually slow child still fails at its
own unchanged guard, and the case can fail first if aggregate setup is too slow.

Linked setup adds the full 20-second worktree guard. Paging adds four 20-second
Git commands, a 35-second emitter, and a 35-second compile (150 seconds). B04 adds
one 30-second real relevance create per fixture: (180 + 30) + (200 + 30) = 440.
Conflict setup adds three 20-second Git commands, a 50-second emitter, and a
30-second doctor (140 seconds). Authenticated preflight has bootstrap and six
reads with existing 12-second busy-retry windows: allocate 72 seconds to those
windows and 28 seconds to bootstrap/parsing/scheduling. Thus conflict setup is
180 + 140 + 72 + 28 = 420 seconds. B05 adds its 30-second real safe-text create.
Fetch/body parsing have no independent hard deadline; the finite case timer
bounds them. These assumptions require uncensored real setup/action evidence.

Workflow accounting preserves the existing assertion, request, and retry limits:

- B01's initial/reload navigations each permit four 10-second Repo and four
  10-second Search waves. The new tab permits four Repo and seven Search waves
  (one initial, three paired with Repo refreshes, three later Load-view waves).
  Paired waves run concurrently, preserving the 240-second wave allowance;
  two 20-second socket checks and a 50-second navigation/assertion/sentinel
  allowance give 330 seconds.
- B02's two 20-second historical/current loads, two HEAD checks, primary inspection,
  and sentinels use a 90-second policy. B03's 35-second browse, 15-second visible
  window, ten local pages, three 20-second search modes, and sentinels use 180.
- Each B04 fixture has three 20-second relevance loads, six 500ms quiet checks,
  a 10-second invalidation check, interest assertions, and sentinels within 120
  seconds; count both fixtures. B05's five 20-second view loads, five busy-read
  helpers with 5-second retry windows, provenance/candidate assertions, and
  sentinels use 180. Busy windows do not independently bound in-flight fetches.
- Mutation helpers retain five-attempt refresh, Search, and inspection loops.
  B06 has one review/submit plus refreshed readback; B07 has one review/submit
  and two extra inspections; B08–B10 have one review/submit and one extra inspection.
  Each gets 90 seconds of workflow policy, retaining at least the old 45-second
  whole-case allowance for browser work alone. B11 counts two such workflows
  (180 seconds). B12 counts initial simultaneous-candidate proof plus four
  isolated-axis resolution workflows (5 × 90 = 450): five reviews, four submits,
  and four extra inspections. Each review checks HEAD; each submit checks HEAD
  and five sentinel Git commands. Their 20-second child guards stay unchanged;
  the workflow allowance assumes these small Git checks typically take 2 seconds
  each, and does not promise every child/HTTP attempt can reach its ceiling.
- B13's real CLI amendment has 40 seconds; two selections permit six 5-second
  Search attempts each; refresh has 5 seconds and invalidation has 12. HEAD,
  sentinels, and UI work share the remainder of its 180-second workflow allowance.
- B14 retains held-old-read/newer-query ordering, both real committed mutations,
  two 12-second response/invalidation waits, Search selection, 5-second refresh,
  HEAD/sentinel checks, and the 3-second lost-response observation. Its policy
  allocates 180 seconds; uncapped promises remain subject to the single case timer.
- B15's explicit socket/read waits total 137 seconds. Two 5-second refreshes,
  lock lookup/readiness/release, sentinel checks, and UI work use the remainder
  of its 240-second workflow allowance. Lock readiness retains 5 seconds and
  child stop retains 8 seconds.

Source counts and the labelled assumptions above determine the legacy policy.
Per-fixture teardown allocates 20 seconds for browser context closure, existing
child stops (8 seconds each, up to 16 with a lock child), filesystem removal,
and scheduling. B04 counts twice that. Owned-job cleanup and receipt finalization
retain an independent 60-second supervisor reserve.

The result also records bounded startup events from the supervisor, owned
wrapper, and reporter. Fixed phase and input labels identify inventory, hash,
pin, asset, launch, discovery, CLI, and reporter boundaries without retaining
raw errors or credentials. `startup_verified` requires complete discovery,
CLI exit, reporter initialization, and first-test evidence; fixture progress
separately identifies real fixture execution. Missing, malformed, truncated,
or out-of-order startup journals fail the run. Reporter error markers and its
bounded final error count must agree with the execution receipt; malformed counts
are rejected before inclusion in the result. Missing final execution evidence
alone does not prove that no test began. Early input failures retain a nonzero
receipt with a safe failure category and independent cleanup results.

Do not log the process bootstrap URL, cookie, rendered page content, mutation
body, or raw server error message. Browser diagnostics record only request
method/path, status, exact revision OID, ADR ID, and typed error code.
