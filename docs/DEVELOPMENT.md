# Development

ADRAI is a native Haskell project built with Stack. The executable entry point is `app/Main.hs`; production modules live under `src/Adrai`.

## Build and test

```console
stack build
stack test adrai:adrai-test
```

The ordinary suite combines unit, property, golden, and integration tests. Larger or specialized gates are separate components so routine test runs do not construct stress fixtures:

```console
stack test adrai:adrai-stress-test
stack test adrai:adrai-cache-selection-test
stack test adrai:adrai-benchmark-registration-test
stack bench adrai:adrai-bench
```

Golden tests are read-only by default. Their fixture directories under `test/golden/` document the explicit regeneration switches. Regeneration must remain Haskell-owned and must not execute or import a prototype implementation.

## Architecture boundaries

- `Adrai.Git` invokes Git with explicit argument arrays and returns exact object data.
- `Adrai.Repository` binds configuration and managed bytes to one resolved commit OID.
- `Adrai.Format.*`, `Adrai.Graph`, and `Adrai.Compiler.*` validate documents, reduce state, and materialize SQLite.
- `Adrai.Service.*` owns revision-bound queries and transactional mutations.
- `Adrai.CliRunner` parses and dispatches the public executable.
- `Adrai.Explorer.*` implements the terminal UI.
- `Adrai.Web.*` is currently scaffold-only.

Keep these boundaries narrow: low-level Git observation should not parse ADRAI documents or mutate refs, and repository reads must not fall back to ambient worktree bytes after resolving a revision.

## Tests and fixtures

Integration and E2E tests create temporary real Git repositories. They cover normal, bare, linked, sparse, shallow, detached, dirty, staged, Unicode, and conflict scenarios without committing `.git` fixtures.

Deterministic fixture plans live in `test/support`. Keep logical plans independent from their Git or service interpreters, record counts and invariants near the fixture, and treat generator tags, seeds, and canonical digests as reviewed contract changes.

The web workspace uses the Node version in `.node-version`, but it is not part of a supported web build yet; see [Web interface](WEB.md).
