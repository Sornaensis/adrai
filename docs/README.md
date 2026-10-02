# ADRAI documentation

ADRAI stores and queries Architecture Decision Records in Git. The source-built executable includes a command-line interface, terminal explorer, and browser explorer with an HTTP API and event WebSocket.

The native CLI implements the original fifteen core commands, including a
service-backed terminal explorer; `web` adds the Haskell loopback HTTP/WebSocket
and Elm interface. Vector retrieval uses deterministic built-in feature sketches,
not a learned embedding provider. These are capability descriptions, not a claim
that all parity or native acceptance gates are complete.

The supported browser workflows exercise production Elm controls against small
deterministic HTTP fixtures. They do not establish real Git/backend, large paging,
conflict, or reconnect integration coverage; the older real-server matrix is
historical and nondefault. Current runtime/launchers require Windows; portability
remains analysis. See [browser scope](../browser-tests/README.md) and
[retained testing](TESTING.md) for separate coverage contracts.

## Start here

- [Installation](INSTALL.md) — prerequisites and source builds
- [Usage](USAGE.md) — quick start and command overview
- [Web interface](WEB.md) — browser explorer and HTTP/WebSocket contract
- [Data format](FORMAT.md) — repository configuration and managed documents

## Concepts

- [Search](SEARCH.md) — full-text, vector, and relevance search
- [Conflict handling](CONFLICTS.md) — valid multihead states and integrity failures
- [Cache](CACHE.md) — generated SQLite indexes and revision snapshots

## Contributing

- [Development](DEVELOPMENT.md) — project layout, builds, and test suites

Command help is the authoritative option reference:

```console
adrai --help
adrai COMMAND --help
```
