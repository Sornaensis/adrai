# Conflict handling

P2 graph reduction distinguishes a valid unresolved multihead from malformed graph state. P4-03 preserves that distinction in the cold database.

`ADR_CONFLICT` is semantic state, not an integrity issue. A valid multihead passes the strict compiler gate, is stored in `adr_conflict`, retains normalized semantics, and produces one deterministic `ADR@record` search candidate per current decision head. Its database `semantic_state` is `conflict`.

Malformed graph state, including zero-head axes, missing parents, cycles, and invalid deltas, produces typed graph-origin issues. Those errors set `semantic_state=invalid` and block normalized semantic and search rows. Source, history, operation, config, and basis diagnostics remain in the separate `issue` table; a missing or noncommit provenance basis is a warning and does not itself block materialization.

P4-03 does not implement public conflict-resolution mutation or CLI rendering. Those workflows remain later-phase behavior.
