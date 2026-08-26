# Cache

ADRAI stores generated data under the repository's ignored `.adrai/` directory:

- `.adrai/index.sqlite` is the mutable alias for the current compiled index.
- `.adrai/cache/<commit-oid>.sqlite` is an immutable, revision-addressed snapshot.

Both use the `adrai-cache/1` SQLite/FTS5 schema. They are derived artifacts and must not be committed.

## Reuse

`compile`, read commands, and post-mutation indexing resolve a commit before selecting a cache. ADRAI can reuse an exact snapshot or clone a compatible snapshot when Git proves that configuration and managed document trees are unchanged. Otherwise it performs a fresh compile. The published result is checked against its resolved revision and logical fingerprints.

Vector corpora remain process-local. A corpus belongs to one exact search materialization and is rejected when document identities, passage identities, embedding inputs, or vector implementations differ. Query and relevance-source vectors are request-ephemeral.

## Maintenance

The current compiler does not enforce a revision-snapshot retention limit. Old files under `.adrai/cache/` may therefore accumulate.

The entire `.adrai/` directory is disposable when no ADRAI process is using it; the next command rebuilds the required data from committed source. Removing it loses only generated indexes and local provenance acceleration, not managed ADR documents.
