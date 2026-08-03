# P4-03 cold compiler goldens

These Haskell-owned files freeze the declared `adrai-cache/1` schema and ordered logical dumps queried from actual placement-free healthy and diagnostic-only cold SQLite compilations. The dumps cover metadata/fingerprints, configuration and source observations, diagnostics/conflicts, normalized operation/graph/search rows, and all six FTS populations. Values use a lossless hex cell encoding; only the nondeterministic resolved commit OID is replaced with `<resolved-oid>`. They deliberately do not freeze raw SQLite page bytes, machine-local repository paths, placement, or cache-publication state.

Tests read these files normally. Regenerate only through the explicit Haskell test-suite flag `--write-p4-03-goldens`; the writer does not execute or import the protected Python prototype.
