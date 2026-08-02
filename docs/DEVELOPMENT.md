# Development

ADRAI is built and tested as a native Haskell project. The test suite includes unit, property, golden, integration, E2E, and shared `test/support` source directories. The protected Python prototype is a static reference only: development and tests must never execute or import it.

## Deterministic fixture plans

The production, large-stress, and relevance fixtures use the versioned `adrai-fixture-splitmix64/v1` algorithm and seed `260729`. This sequence is deliberately Haskell-owned and does not reproduce Python RNG bytes. The indexed retrieval fixture is deterministic by ADR index instead, and records generator `adrai-indexed-retrieval/v1` with seed `0`; it does not consume the SplitMix stream. Changing either tag or seed, the canonical compact summary, or its digest is a reviewed contract change.

The support layer is pure. `productionShapeV1` and `largeStressV1` describe commit streams, `retrievalScaleV1` describes an indexed logical corpus, and `relevanceCorpusV1` describes focused semantic cases. None of them writes files, initializes Git, or calls the service. Later phases own interpreters:

1. The Git interpreter materializes commits, parents, refs, checkouts, and merges.
2. The service interpreter creates canonical ADRAI operations through public Haskell APIs.
3. Integration and E2E suites verify the materialized repository against the plan invariants.

The production-shape plan has 2,000 commits, 200 created ADRs, 400 semantic operations, and 1,600 noise commits. The large plan has 12,000 commits, 900 created ADRs, 2,000 semantic operations, and 10,000 noise commits. The retrieval plan has 2,000 logical ADRs and a 120-candidate bound; it is not a commit-history fixture. The relevance corpus has six ADRs, 13 sources, and 13 expectations under `adrai-relevance-six:v1`.

The current Dropwire JSON fixture records result-level acceptance only. Its external source corpus is absent, so contributors must not describe that JSON as a reproducible corpus. The bundled production result represents a 2,000-commit/200-ADR workload; it is also distinct from both the 12,000-commit stress plan and the 2,000-ADR retrieval corpus.

When adding a fixture, keep its logical plan independent of interpreters, document exact counts and invariants, add deterministic tests, and update the canonical contract digest when the reviewed summary changes.
