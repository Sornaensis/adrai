# P3-04 weighted-search golden

This Haskell-owned fixture freezes the collapsed `adrai/search/v1` projection for blank, FTS, vector, hybrid, and conflicted-decision searches together with the P3-04 ranking fingerprint. Normal tests only read it. Regeneration is explicit through `stack test --test-arguments=--write-p3-04-goldens` and never executes or imports the protected Python prototype.

- `current-search.golden` is 18,679 bytes; SHA-256: `DD1806A3EA65EB8A7F225D10FEF6A0B97F7D9274DC1C2184A134E15AA406D1AE`
