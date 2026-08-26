# ADRAI documentation

ADRAI stores and queries Architecture Decision Records in Git. The command-line interface is implemented; the browser interface is not yet available.

## Start here

- [Installation](INSTALL.md) — prerequisites and source builds
- [Usage](USAGE.md) — quick start and command overview
- [Data format](FORMAT.md) — repository configuration and managed documents

## Concepts

- [Search](SEARCH.md) — full-text, vector, and relevance search
- [Conflict handling](CONFLICTS.md) — valid multihead states and integrity failures
- [Cache](CACHE.md) — generated SQLite indexes and revision snapshots

## Contributing

- [Development](DEVELOPMENT.md) — project layout, builds, and test suites
- [Web interface](WEB.md) — current implementation status

Command help is the authoritative option reference:

```console
adrai --help
adrai COMMAND --help
```
