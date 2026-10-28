# Cache

`domains` can reuse an existing exact-revision archive through the same validated
transaction used by search. It decodes a consistent snapshot and enforces the
compiler's source-integrity error gate without rebuilding search passages or
materialization. Valid domain conflicts remain browsable. Missing or rejected
archives fall back to the committed snapshot read at the same resolved revision;
busy and cancellation signals retain their existing behavior.

Domain reads do not publish archives. Use `adrai compile`, or the normal indexing
after a successful mutation, to prepare one. Repeated `domains` calls without an
exact matching archive continue to use the cold path.

ADRAI stores generated data under the repository's ignored `.adrai/` directory:

- `.adrai/index.sqlite` is the mutable alias for the current compiled index.
- `.adrai/cache/<commit-oid>.sqlite` is an immutable, revision-addressed snapshot.

Both use the `adrai-cache/3` SQLite/FTS5 schema. They are derived artifacts and must not be committed. Cache v1 and v2 are incompatible and are rebuilt cold; they are never reused, seeded, or cloned across revisions.

## Reuse

`compile`, read commands, and post-mutation indexing resolve a commit before selecting a cache. ADRAI can reuse an exact snapshot or clone a compatible snapshot when Git proves that configuration and managed document trees are unchanged. Otherwise it performs a fresh compile. The published result is checked against its resolved revision and logical fingerprints.

Optional TEI corpus embeddings use a separate disposable user cache, keyed by endpoint, live model identity/revision, dimensions and exact text digest. sqlite-vec indexes only eligible entries for each request. Query and relevance-source vectors remain request-ephemeral. Unavailable TEI or sqlite-vec uses FTS5, even with a warm embedding cache; this never rejects an immutable lexical archive. See [Search](SEARCH.md#optional-text-embeddings) for user configuration and cache paths.

`adrai prepare-embeddings --at REVISION` acquires the exact lexical archive, then
fully materializes its summary/passage corpus and closes the archive transaction
before optional TEI work. Validated embedding batches publish independently, so
interrupted preparation resumes from the same identity/text cache. The lexical
archive remains independent of optional preparation success. Search/relevance
repair only bounded small deltas; larger missing corpora expose a preparation
action while continuing with lexical retrieval. No vector index is retained
across requests.

`relevant` first validates an exact revision-addressed archive and falls back to cold compilation when one is unavailable. Its `diagnostics.index_revision` names the commit represented by the context actually used, and `diagnostics.index_preparation` reports `cache-hit` or `cold-fallback`. The separately captured `checkout_head` supplies the freshness comparison; an intentionally historical archive remains valid even when `stale` is true. The mutable `index.sqlite` alias is not evidence of which revision a relevance query used.

## Maintenance

The current compiler does not enforce a revision-snapshot retention limit. Old files under `.adrai/cache/` may therefore accumulate.

The entire `.adrai/` directory is disposable when no ADRAI process is using it; the next command rebuilds the required data from committed source. Removing it loses only generated indexes and local provenance acceleration, not managed ADR documents.
