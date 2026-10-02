# P3-05 raw-text relevance golden

This Haskell-owned fixture freezes resolved, outside-scope, and decision-conflict `adrai/relevant/v2` projections together with the raw-text and scoring contract fingerprints. Normal tests only read it. Regeneration is maintenance that writes these owned fixtures, not retained
verification. It is explicit through `stack test adrai:adrai-test --test-arguments=--write-p3-05-goldens` and never executes or imports the protected Python prototype.

- `relevance.golden` is 13,043 bytes; SHA-256: `80F16C75E61E619986FB19C5514C2864DFED1E867A1759A4C5773E11DACC8885`
