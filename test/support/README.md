# Haskell fixture support

The test-support modules own deterministic logical fixture data. Their repository generator is identified as `adrai-fixture-splitmix64/v1` and uses seed `260729`. Its SplitMix64 sequence is a Haskell contract; it does not claim byte-for-byte parity with Python's random-number generator.

Generators return pure logical plans and declarative corpora. They do not create repositories, call Git, or invoke ADRAI services. Later integration and E2E suites will provide Git and service interpreters that materialize these plans through the public Haskell implementation.

Four fixtures have intentionally different jobs:

- `productionShapeV1` models the bundled 2,000-commit/200-ADR production exercise: 400 semantic operations, 1,600 noise commits, 300 decision documents, 800 connection documents, and 1,100 semantic documents. It preserves exact operation counts, parent ordering, target validity, and branch-revision isolation.
- `largeStressV1` is the separate 12,000-commit stress plan: 2,000 semantic operations, 10,000 noise commits, 900 created ADRs, 1,550 decision documents, 3,800 connection documents, and 5,350 semantic documents. It is lazy and must be consumed with a strict fold.
- `retrievalScaleV1` is a 2,000-logical-ADR retrieval corpus, not a 2,000-commit repository. It freezes topic distribution, four probes, a 120-candidate shortlist bound, and first/tail ADR identities.
- `relevanceCorpusV1` is the six-ADR `adrai-relevance-six:v1` corpus with 13 source cases and 13 expectations. It covers format variety, semantic-over-scope behavior, worktree input, hard negatives, and a top-three oracle.

Python is never invoked or imported by these generators or their tests. Static prototype paths in the coverage ledger are historical evidence only.
