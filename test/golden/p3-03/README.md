# P3-03 materialization goldens

These Haskell-owned fixtures freeze current logical search rows, ADR-section passage identities, the materialization fingerprint, and the disposable SQLite schema. Normal tests are read-only. Regeneration is explicit through `stack test --test-arguments=--write-p3-03-goldens` and never invokes the Python prototype.

- `materialization.golden` is 7,743 bytes; SHA-256: `8B55EA4093AD5D879CAD2843198B93E5F5E5DA5E3B1C02964DAA050CA1730767`
- `search-schema.golden` is 2,473 bytes; SHA-256: `434C0F5486465C4E23020ED99606BD09E6082D7C367A3828511470689D4399EC`
