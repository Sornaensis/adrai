# Web API runtime tests

These retained tests start the package WAI/Warp application on a real IPv4
loopback socket. They keep browser opening injected and exercise credential,
route, unavailable-events, occupied-port, and shutdown behavior without a live
watcher or Elm client.
