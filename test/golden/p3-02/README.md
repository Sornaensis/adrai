# P3-02 retrieval goldens

These Haskell-owned fixtures freeze canonical query planning and the six SQLite FTS5 target contracts. Normal tests only read them. Regeneration is explicit through `stack test --test-arguments=--write-p3-02-goldens` and never invokes the Python prototype.

- `query-plans.golden` SHA-256: `772E658C3B5F4CFBEDCD8C2A697B808AA88CE28224E23D5A4C06E11593A78754`
- `fts-channel-contract.golden` SHA-256: `8276EC6C13B3607D9871F810BAF14F85D03F8A0272E0C4EA422894490B0A187E`
