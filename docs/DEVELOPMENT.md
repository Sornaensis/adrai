# Development

ADRAI is built and tested as a native Haskell project. The test suite includes unit, property, golden, integration, E2E, and shared `test/support` source directories. The protected Python prototype is a static reference only: development and tests must never execute or import it.

## Read-only Git observation

`Adrai.Git` is a low-level observation boundary. It discovers main worktrees, linked worktrees (including worktrees attached to bare common storage), and direct bare repositories; resolves a requested commit once; and returns exact tree and blob bytes. It does not parse ADRAI documents, reduce the graph, assemble a read snapshot, update SQLite, classify provenance, mutate refs, or implement public command schemas. Those semantics belong to later P4 and P6 work.

Every runtime Git call uses `typed-process` with an explicit argument array. No command is interpreted by a shell, and resolved object reads accept full validated object IDs rather than user-controlled revision arguments. Git is the only external runtime program at this layer; libgit2 and compatibility subprocesses are not used.

Object-info and blob requests are sorted, deduplicated, and split into batches of at most 256 objects. Each request is flushed and its response is parsed before the next request; the folding API retains at most one payload in addition to the caller's accumulator, while a concurrent drain retains only a bounded stderr diagnostic. The batch protocol verifies response order, full object IDs, object types, decimal sizes, payload lengths, framing newlines, and trailing bytes. Exact byte reads are distinct from the strict UTF-8 convenience API, which rejects invalid text rather than replacing it.

The integration suite creates temporary real Git repositories using the same argv-only process rule. It covers spaces and Unicode, bare and linked layouts, sparse and shallow repositories, custom committed roots, strict path/blob decoding, and batch rollover. No `.git` fixture is committed. Worktree file reads resolve the final file physically: an in-repository link may be read while a link that escapes the canonical worktree is rejected. This read policy has an unavoidable check/read race and must not be reused for writes; `resolveManagedWritePath` retains its stricter component-by-component redirection policy.

`Adrai.Repository` builds the next read-only boundary on those primitives. A requested revision is resolved exactly once, and every configuration lookup, managed-root tree listing, and blob read for that observation is addressed by the resulting full commit OID. Its immutable key is the canonical Git common directory plus that OID, so linked worktrees share a namespace without allowing their current branches, indexes, or worktree bytes to change a bound observation. Branch names, current `HEAD` attachment, shallow state, and the continued existence of the requested ref are advisory after resolution; deleting or moving a ref does not retarget an already resolved observation. If later object pruning removes that bound commit or one of its required objects, observation fails atomically with the structured Git/object error instead of falling back to another ref, the worktree, or a partial result.

Repository observation returns raw committed configuration and selected tree/blob facts only. It deliberately does not parse decision or connection documents, construct `History.ReadSnapshot`, compile a graph, access SQLite, populate a cache, serve queries, or mutate the repository. Blob entries preserve exact bytes even for symlink mode and arbitrary non-UTF-8 managed content; nonblob entries preserve metadata without pretending to have blob bytes. Bare, linked, sparse, shallow, detached, dirty, staged, and untracked states therefore cannot substitute ambient filesystem content for committed object data. P4-03 owns document parsing, graph reduction, cold compilation, and SQLite materialization over this raw immutable input. P4-04 adds provenance and placement enrichment and assembles the final `History.ReadSnapshot`; P4-05 owns revision-cache publication and reuse.

## Cold compiler boundary

P4-03 starts from an already resolved revision and never resolves `HEAD` or another ref again. It retains invalid committed configuration as raw data, parses every observable managed blob, aggregates deterministic diagnostics, chooses the first path-sorted valid copy of each object, and runs the P2 graph reducer exactly once. It separately validates operation membership and capsule rules, batches provenance-basis checks, and walks the exact reachable commit DAG. Every parent edge of a merge is compared for byte rewrites and disappearances; shallow history is explicitly marked incomplete.

The analysis gate admits warnings and semantic `ADR_CONFLICT` states but rejects every integrity error. Only a gated placement-free snapshot is passed to the P3 search materializer. No empty or inferred placement map is manufactured, and P4-03 does not construct the final `History.ReadSnapshot` owned by P4-04.

Storage accepts a fresh caller-owned SQLite connection. Schema creation, source and diagnostic rows, optional semantic and search rows, foreign-key and count verification, and meta rows share one outer transaction. A failure rolls back schema and data together. Connection paths, filesystem publication, reuse, retention, and corruption recovery remain P4-05 responsibilities.

## Deterministic fixture plans

The production, large-stress, and relevance fixtures use the versioned `adrai-fixture-splitmix64/v1` algorithm and seed `260729`. This sequence is deliberately Haskell-owned and does not reproduce Python RNG bytes. The indexed retrieval fixture is deterministic by ADR index instead, and records generator `adrai-indexed-retrieval/v1` with seed `0`; it does not consume the SplitMix stream. Changing either tag or seed, the canonical compact summary, or its digest is a reviewed contract change.

The support layer is pure. `productionShapeV1` and `largeStressV1` describe commit streams, `retrievalScaleV1` describes an indexed logical corpus, and `relevanceCorpusV1` describes focused semantic cases. None of them writes files, initializes Git, or calls the service. Later phases own interpreters:

1. A later Git fixture interpreter materializes commits, parents, refs, checkouts, and merges; P4-01 only supplies read-only Git facts and bytes.
2. The service interpreter creates canonical ADRAI operations through public Haskell APIs.
3. Integration and E2E suites verify the materialized repository against the plan invariants.

The production-shape plan has 2,000 commits, 200 created ADRs, 400 semantic operations, and 1,600 noise commits. The large plan has 12,000 commits, 900 created ADRs, 2,000 semantic operations, and 10,000 noise commits. The retrieval plan has 2,000 logical ADRs and a 120-candidate bound; it is not a commit-history fixture. The relevance corpus has six ADRs, 13 sources, and 13 expectations under `adrai-relevance-six:v1`.

The current Dropwire JSON fixture records result-level acceptance only. Its external source corpus is absent, so contributors must not describe that JSON as a reproducible corpus. The bundled production result represents a 2,000-commit/200-ADR workload; it is also distinct from both the 12,000-commit stress plan and the 2,000-ADR retrieval corpus.

When adding a fixture, keep its logical plan independent of interpreters, document exact counts and invariants, add deterministic tests, and update the canonical contract digest when the reviewed summary changes.
