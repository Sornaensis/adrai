# Property tests

The P2-06 suites exercise canonical formats, domain algebra, graph reduction,
append-only reconciliation, and public projections with focused Hedgehog
generators. Generated IDs and references come from the same shrinking ordinal
specifications, so minimized cases retain their intended validity or their one
deliberate graph/input fault.

Run the bounded P2-06 group with:

```powershell
stack test --test-arguments "--pattern P2-06 --hedgehog-tests 100"
```

Hedgehog prints a native replay value when a property fails. Copy that exact
value, retain the failing test pattern, and pass it back to the installed
runner; do not translate it into a separate application seed:

```powershell
stack test --test-arguments "--pattern '<failing P2-06 test>' --hedgehog-replay '<exact value printed by the runner>'"
```

The default per-property budgets are 100-150 format/domain cases, 75 graph and
token cases, and 60 reconciliation/projection cases. Input-order properties
sample at most two shuffles per generated case and never enumerate factorial
permutations.
