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
`result.json`. B01 has a 90-second case cap. The aggregate has a 1,200-second
deadline including discovery, fixture setup, browser work, and cleanup; 60
seconds remain reserved for owned-job cleanup and finalization. The 15 declared
case caps sum to 1,035 seconds, leaving 105 seconds of the 1,140-second work
budget for discovery and runner overhead. These are finite liveness guards, not
product latency targets. The runner verifies child exit, listener closure, and
removal of the owned temporary root on every result. A failed assertion remains
a failed result even when ownership cleanup succeeds.

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
