# Conflict handling

ADRAI distinguishes a valid unresolved multihead from malformed graph state.

## Semantic conflicts

`ADR_CONFLICT` represents multiple valid current heads. It is semantic state, not corrupt data. Compilation retains the competing records in `adr_conflict`, and search indexes one deterministic `ADR@record` candidate for each current decision head. Collapsed results group those candidates under the logical ADR while preserving the matched head as evidence.

`doctor` and `explore` expose conflicts. A command that cannot safely choose a head exits with status `3`. Resolution is axis-specific: use the relevant mutation command and its reviewed replacement or resolution option; inspect `adrai COMMAND --help` before changing a conflicted ADR.

## Integrity failures

Zero-head axes, missing parents, cycles, invalid deltas, noncanonical documents, and malformed operation membership are integrity failures. They produce typed diagnostics, set the compiled semantic state to `invalid`, and prevent normalized semantic and search rows from being published.

Warnings, such as unavailable provenance basis information, remain diagnostics but do not by themselves block materialization.
