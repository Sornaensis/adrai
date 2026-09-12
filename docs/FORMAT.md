# Data format

ADRAI has three versioned formats: repository configuration, managed Markdown documents, and a generated SQLite cache.

## Repository configuration

`.adrai.toml` uses schema `1`. The default configuration is equivalent to:

```toml
schema = 1

[paths]
decisions = "architecture/adrai/decisions"
connections = "architecture/adrai/connections"

[[line]]
id = "trunk"
refs = ["refs/heads/main", "refs/remotes/origin/main", "refs/heads/master", "refs/remotes/origin/master"]
```

Paths are case-sensitive, repository-relative logical paths. Decision and connection roots must not overlap. The `line` entries name logical lines and the refs that belong to each line.

Configuration is read from the same resolved commit as the managed documents. An absent `.adrai.toml` selects the defaults. A present but empty, invalid UTF-8, invalid TOML, or non-blob entry is an error; it never falls back silently.

## Managed documents

ADRAI selects only exact `*.decision.md` and `*.connection.md` suffixes under their configured roots. Both formats use canonical TOML front matter followed by Markdown and a sealed provenance capsule:

- decisions: `adrai/decision/v1`
- connections: `adrai/connection/v1`

Managed paths are derived from record identity and content. Line endings, field ordering, lists, identifiers, and the provenance capsule are validated canonically. Prefer `adrai create` and the mutation commands over hand-editing these files.

Repository reads are revision-local: sparse checkouts, staged changes, dirty files, and untracked files cannot replace bytes from the resolved Git commit. The explicit `relevant --worktree` mode is the exception for relevance input; its ADR context still comes from `HEAD`.

## Generated database

The SQLite schema is `adrai-cache/3`. It contains source observations, diagnostics, conflicts, reduced semantics, operations (whose members carry the authoritative requested-revision blob OID), search documents, passages, and FTS5 indexes. It is a derived artifact under `.adrai/`, not repository source or a portable interchange format. Cache v1 and v2 are incompatible and are always rebuilt cold.

Compilation publishes schema and data atomically and verifies foreign keys, row counts, operation membership, and ordinary/FTS key parity before commit. Invalid source can still produce diagnostic data, but normalized semantic and search rows are withheld. Logical fingerprints are canonical; raw SQLite page bytes are not.

See [Cache](CACHE.md) for lifecycle details.
