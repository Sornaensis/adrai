# Search

P3-02 establishes the Haskell query-planning and SQLite FTS retrieval boundary. It classifies identifier, keyword, and prose queries; preserves phrase and exact-term expressions without truncation while independently capping the NEAR, prefix, Porter, and identifier fallbacks; extracts ordered local acronym aliases; and exposes the same six summary/passage FTS5 schemas, tokenizers, and BM25 field weights as the statically reviewed prototype.

Collapsed summary retrieval applies allowed item IDs inside each parameterized SQL query, uses deterministic 700-ID batches, negates BM25 so larger scores are better, orders ties by descending item ID, and merges duplicate IDs by maximum score. Candidate limits are positive and overflow-checked. Exact-AND and NEAR coverage—not phrase coverage—controls the prefix fallback, and the merged exact-term channel is intentionally not capped a second time.

This phase does not persist search materializations, fuse channels with reciprocal-rank fusion, apply public search filters or explanations, create source chunks, or expose CLI/provider behavior. Those remain later search phases. Normal tests read Haskell-owned retrieval goldens; regeneration is an explicit Haskell-only action and never runs the Python prototype.
