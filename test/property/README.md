# Property tests

The P2-06 suites exercise canonical formats, domain algebra, graph reduction,
append-only reconciliation, and public projections with focused Hedgehog
generators. Generated IDs and references come from the same shrinking ordinal
specifications, so minimized cases retain their intended validity or their one
deliberate graph/input fault.

Use an exact registered P2-06 property name with the canonical `Focused` runner
in [Testing](../../docs/TESTING.md). It supplies the property's source-defined
budget; it does not accept arbitrary Hedgehog or Tasty arguments.

Hedgehog prints a native replay value when a property fails. Preserve that value
with the external failure evidence; do not translate it into an application seed.
The retained runner currently exposes no replay option.

The default per-property budgets are 100-150 format/domain cases, 75 graph and
token cases, and 60 reconciliation/projection cases. Input-order properties
sample at most two shuffles per generated case and never enumerate factorial
permutations.
