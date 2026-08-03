# P3-05 raw-text relevance golden

This Haskell-owned fixture freezes resolved, outside-scope, and decision-conflict `adrai/relevant/v1` projections together with the raw-text and scoring contract fingerprints. Normal tests only read it. Regeneration is explicit through `stack test --test-arguments=--write-p3-05-goldens` and never executes or imports the protected Python prototype.

- `relevance.golden` is 12,178 bytes; SHA-256: `DF074C9B64F84AE52B3D063D9AF4A402CCC308A9C937BC9D4C1EF77ACAA4F779`
