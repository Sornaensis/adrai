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
