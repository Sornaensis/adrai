# P3-06 retrieval, relevance, and corpus quality golden

This Haskell-owned evidence freezes the executable six-ADR quality summary, the qualified result-only Dropwire aggregate, the canonical float32-origin in-memory SearchVectorCorpus, cold/reused exact-byte digests, and deterministic diagnostics from the separately executable 2,000-ADR scale integration test.

Current query quality here uses FTS5 with isolated user configuration. SearchVectorCorpus sections retain historical compatibility evidence; they do not validate external embeddings. Optional TEI and native sqlite-vec are covered by `test/integration/Adrai/TextEmbeddingTest.hs`.

Normal tests are read-only. Regeneration is maintenance that writes these owned fixtures, not retained
verification. It is explicit through `stack test adrai:adrai-test --test-arguments=--write-p3-06-goldens` and never executes or imports the protected Python prototype.

The Dropwire source corpus is absent, so its section is result-contract validation only and is not executable parity. Scale timings are reporting-only and are neither thresholds nor golden values. This golden covers in-memory retrieval contracts. Persisted revision caches,
incremental invalidation, branch isolation, and corrupt-cache recovery are
implemented separately; see [Cache](../../../docs/CACHE.md) and
`Adrai.Service.Compilation` for their current contracts.

- `quality.golden` is 7507 bytes; SHA-256: `A09EBF5260C0DE289DD83A36E9CB3230324E15891BA5FA35B27D7403865DC806`
