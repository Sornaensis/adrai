# Web API runtime tests

These retained tests start the package WAI/Warp application on a real IPv4
loopback socket. They keep browser opening injected and exercise credential,
route, occupied-port, and shutdown behavior. P7-03 cases also use real
WebSocket clients, temporary Git repositories and worktrees, the live watcher,
and concurrent exact-revision compilation. The Elm client remains a later
increment.
