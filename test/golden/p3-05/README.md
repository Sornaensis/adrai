# P3-05 raw-text relevance golden

This Haskell-owned fixture freezes resolved, outside-scope, and decision-conflict `adrai/relevant/v2` projections together with the raw-text and scoring contract fingerprints. Normal tests only read it. Regeneration is maintenance that writes these owned fixtures, not retained
verification. It is explicit through `stack test adrai:adrai-test --test-arguments=--write-p3-05-goldens` and never executes or imports the protected Python prototype.

- `relevance.golden` is 14,008 bytes; SHA-256: `9E237DFD086DD9DC90D0A3628B8B762C4B160EC2767A596FC9C0771AAD2A4208`
