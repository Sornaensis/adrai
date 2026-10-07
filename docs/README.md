# ADRAI documentation

ADRAI stores and searches Architecture Decision Records (ADRs) in Git. It is for developers and teams who want to record software decisions and find them later, using a command-line, terminal, or web interface.

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
