# P3-04 weighted-search golden

This Haskell-owned fixture freezes the collapsed `adrai/search/v1` projection for blank, FTS, vector, hybrid, and conflicted-decision searches together with the P3-04 ranking fingerprint. Normal tests only read it. Regeneration is explicit through `stack test --test-arguments=--write-p3-04-goldens` and never executes or imports the protected Python prototype.

- `current-search.golden` is 18,440 bytes; SHA-256: `51B7FEC0C33CC0EF82AA04E218FA87DADDE0D105260EF85DFAF1FAE1220BC2D9`
