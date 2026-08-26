# Search

ADRAI exposes two related commands:

- `search [QUERY]` searches compiled ADR content.
- `relevant FILE` ranks ADRs against a source file.

Both operate against a resolved Git revision and return deterministic `adrai/search/v1` or `adrai/relevant/v1` projections with `--json`.

## ADR search

```console
adrai search "database migration"
adrai search DROPWIRE_DATABASE --mode fts --domain operations
adrai search "lease token" --mode vector --limit 5 --json
```

Retrieval modes are:

- `fts`: SQLite FTS5 with exact, phrase, proximity, prefix, stemming, and identifier channels.
- `vector`: built-in semantic and identifier embeddings.
- `hybrid`: weighted reciprocal-rank fusion of the active FTS and vector channels; this is the default.

Filters include file scope, repeatable domains, actor, inclusive timestamps, obsolete state, and immutable revision. Filters are applied before candidates are ranked. A blank query returns a deterministic filtered ADR listing.

Collapsed results group competing decision heads by ADR. `--view exploded` exposes operation-level detail. Use `--include-obsolete` to include obsolete ADRs.

## File relevance

```console
adrai relevant src/Storage.hs
adrai relevant src/Storage.hs --worktree
adrai relevant src/Storage.hs --at main~1 --json
```

Without `--worktree`, the file and ADR context come from the same resolved revision (`HEAD` by default). `--worktree` reads the current file safely from the worktree while keeping ADR context bound to `HEAD`; it cannot be combined with `--at`.

Input is limited to 4 MiB. Binary-like input, NUL bytes, and very short or uninformative text are rejected or produce no weak match. Informative text is chunked, shortlisted through exact, FTS, and vector channels, and reranked against stored ADR passages. File scope affects ranking but does not exclude an otherwise relevant ADR.

## Indexing and conflicts

Only strictly valid reduced semantics reach search tables. Obsolete ADRs remain indexed but hidden by default. Valid decision multiheads are scored independently and grouped while retaining the matched head as evidence; malformed graphs are not indexed.

See [Cache](CACHE.md) for index reuse and [Conflict handling](CONFLICTS.md) for semantic-state behavior.
