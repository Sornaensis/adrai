# Data format

ADRAI's implemented v1 managed-document, repository-configuration, and generated cold-database contracts are versioned and tested below. Later cache-publication and public command formats remain outside this document's current scope.

## Revision-local repository configuration

Repository observation reads `.adrai.toml` from the same resolved commit OID as every managed tree and blob. Only an absent path selects the v1 default configuration. A committed empty file, invalid UTF-8, or invalid TOML is an error; it never silently falls back to defaults. A config entry with any Git blob mode, including executable or symlink mode, is decoded from its literal blob bytes. A tree, commit/gitlink, or other nonblob entry at `.adrai.toml` is rejected.

The parsed decision and connection roots are logical repository-relative paths in that committed tree. Selection is case-sensitive and includes only paths ending exactly in `.decision.md` or `.connection.md` under their corresponding configured roots. Selected blob entries retain their exact bytes and Git metadata; repeated object IDs are retained once per logical path. Selected nonblob entries retain metadata with no fabricated content. Ordering is deterministic and does not depend on checkout materialization, sparse patterns, the index, or dirty and untracked files.

## Disposable cold database

P4-03 defines the generated SQLite contract `adrai-cache/1`. The caller supplies a fresh connection; a cold compile creates configuration, managed-source, diagnostic, conflict, normalized semantic/operation, P3 search, and all six bundled FTS5 tables in one transaction. The database is a derived artifact, not repository source and not a committed format.

Every selected current path retains its Git OID, mode, object type, exact nullable blob bytes, and parse state. Operation storage preserves shared capsule context, each member's object/event/digest, and ordered provenance parents. Unbounded positive operation timestamps are stored as checked decimal text. Ordered line anchors and typed connection payloads are canonical JSON rather than delimiter-packed text, so identifiers containing `@` or newlines and scope values containing commas remain reconstructible without reparsing managed source. Normalized decision, connection, reduction, and search rows exist only when strict source validation succeeds. Invalid source still yields a complete diagnostic database with `semantic_state=invalid` and no semantic or search rows.

Before the transaction commits, P4-03 verifies exact metadata, foreign keys, every declared normalized/search/FTS table count, operation-member identity coverage, and bidirectional ordinary/FTS key parity. Any mismatch rolls back schema and rows together.

The source fingerprint covers committed configuration facts and sorted path/mode/type/OID/byte observations, excluding the machine-local repository namespace and mutable requested-revision alias. The final fingerprint also covers diagnostics, conflicts, reduced semantics, operations, search DTOs, and ABI tags. Raw SQLite page bytes are not canonical.
