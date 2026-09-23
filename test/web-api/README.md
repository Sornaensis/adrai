# Web API runtime tests

These retained tests start the package WAI/Warp application on a real IPv4
loopback socket. They keep browser opening injected and exercise credential,
route, occupied-port, and shutdown behavior. P7-03 cases also use real
WebSocket clients, temporary Git repositories and worktrees, the live watcher,
and concurrent exact-revision compilation. P7-04 cases exercise the rich and
conflicted shared projections, strict query windows, decimal generation
boundary, and durable mutation outcome against the same runtime. The browser
smoke in `browser-tests/tests/p704-smoke.spec.ts` uses a separate temporary
repository and the real built server for the Elm client.

`WebServerTest` exercises the live HTTP admission and error envelope: bootstrap,
Host and Origin, bearer and cookie rules, method and path failures, strict query
and JSON errors, request bounds, exact snapshots, checked mutations, occupied
ports, and listener release. `WebEventsTest` uses real WebSocket handshakes and
frames for authentication, Origin and credential conflicts, interest leases,
frame bounds, pending-client capacity, slow-client deadlines, and socket cleanup.
`WebWatchTest` covers native facts, periodic verification fallback, linked
worktrees, and terminal watcher shutdown. `WebCompilationTest` covers physical
single-flight compilation, namespace isolation, cleanup, and an HTTP read held
across a concurrent CLI commit. `WebExplorerApiTest` covers conflicted rich
projections and the bounded 1000-result window. Pure route, schema, generation,
and redaction edge cases live in `test/unit/Adrai/WebContractTest.hs`.
