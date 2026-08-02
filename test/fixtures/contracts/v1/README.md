# ADRAI v1 contract fixtures

This directory freezes the public v1 behavior that the Haskell implementation
must reproduce. The fixtures were independently hand-transcribed from static
reads of the protected Python prototype, its contract tests, its format
documentation, and its completed verification artifacts on 2026-08-02. Python
was not executed, imported, or used to generate these files.

The Haskell tests consume only this directory. They have no runtime dependency
on `ADRAI_1_Source`, so the prototype can be removed after feature parity and
the required end-to-end suites are established. `manifest.json` records the
exact source paths, source hashes, fixture hashes, byte lengths, newline state,
and intended Haskell assertions needed to audit the transcription after that
removal.

## Frozen semantics

- Text fixtures are UTF-8 with LF line endings and a final LF. Hashes apply to
  exact bytes, not parsed or re-rendered values.
- JSON fixtures capture codecs, identifiers, state tokens, strict document
  parsing, CLI commands and exits, public JSON projections, and search
  acceptance snapshots. Timing measurements are observations, not equality
  gates.
- `search/dropwire-current.json` is the current Dropwire acceptance baseline,
  transcribed from the source artifact's top-level `summary` with its nested
  `old_summary` omitted.
- `search/dropwire-old-historical.json` preserves that nested `old_summary` as
  historical provenance only. It is explicitly not the current acceptance
  baseline.
- `search/production-stress-compact.json` preserves the completed production
  stress assertions and measurements. Its correctness assertions are useful
  acceptance evidence; its timing snapshot remains non-gating across machines.

## Provenance and license

All named source artifacts are from the same local ADRAI repository/project as
this Haskell reimplementation. They were consulted as static text only; the
manifest's lowercase SHA-256 values bind each transcription to the exact source
and fixture bytes. No copied executable Python or runtime bridge is included.

The Haskell project root declares the exact SPDX license `BSD-3-Clause` in
`package.yaml`. These same-project contract fixtures are recorded under that
project license. At extraction time the protected prototype's `pyproject.toml`
did not contain a separate license field and its root did not contain a
standalone license file; the provenance claim here is therefore deliberately
limited to same-project origin plus the target project's explicit
`BSD-3-Clause` declaration.

## Static verification

The manifest can be checked without Python by parsing JSON with PowerShell
`ConvertFrom-Json`, hashing files with `Get-FileHash -Algorithm SHA256`, reading
byte lengths via `Get-Item`, and inspecting the final byte for LF. The manifest
does not hash itself, and this README is contract documentation rather than a
data fixture; every other file in this directory is enumerated as a fixture.
