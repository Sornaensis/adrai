# P3-03 materialization goldens

These Haskell-owned fixtures freeze current logical search rows, ADR-section passage identities, the materialization fingerprint, and the disposable SQLite schema. Normal tests are read-only. Regeneration is explicit through `stack test --test-arguments=--write-p3-03-goldens` and never invokes the Python prototype.

- `materialization.golden` is 7,355 bytes; SHA-256: `50D069D78A059E9B5340DC7AE3D084BACC36732745EE00AB6D6BAD1275BE55A0`
- `search-schema.golden` is 2,473 bytes; SHA-256: `434C0F5486465C4E23020ED99606BD09E6082D7C367A3828511470689D4399EC`
