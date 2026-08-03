# Data format

ADRAI's persisted and interchange formats are not defined by the scaffold. Versioning, schemas, compatibility rules, and examples will be documented once the Haskell format implementation is tested.

## Revision-local repository configuration

Repository observation reads `.adrai.toml` from the same resolved commit OID as every managed tree and blob. Only an absent path selects the v1 default configuration. A committed empty file, invalid UTF-8, or invalid TOML is an error; it never silently falls back to defaults. A config entry with any Git blob mode, including executable or symlink mode, is decoded from its literal blob bytes. A tree, commit/gitlink, or other nonblob entry at `.adrai.toml` is rejected.

The parsed decision and connection roots are logical repository-relative paths in that committed tree. Selection is case-sensitive and includes only paths ending exactly in `.decision.md` or `.connection.md` under their corresponding configured roots. Selected blob entries retain their exact bytes and Git metadata; repeated object IDs are retained once per logical path. Selected nonblob entries retain metadata with no fabricated content. Ordering is deterministic and does not depend on checkout materialization, sparse patterns, the index, or dirty and untracked files.
