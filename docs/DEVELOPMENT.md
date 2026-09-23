# Development

ADRAI is a native Haskell project built with Stack. The executable entry point is `app/Main.hs`; production modules live under `src/Adrai`.

## Build and test

Use [Testing](TESTING.md) for the canonical retained-test runner and its
artifact-binding workflow. The complete gate executes every registered test
in the ordinary, cache-selection, stress, and benchmark-registration
components, enables the stress cases with `--run-stress`, and includes the
named reliability repeats, fixture setup, and owned-descendant cleanup within
one aggregate 600-second deadline. A timeout, omitted test, or surviving
descendant fails the gate.

Compilation is a separate, consistently configured pedantic build of all
components. Tests and benchmarks are compiled but not executed, and build time
is not part of the retained-test runtime. Running benchmarks is an optional
performance investigation outside retained-test acceptance.

Golden tests are read-only by default. Their fixture directories under `test/golden/` document the explicit regeneration switches. Regeneration must remain Haskell-owned and must not execute or import a prototype implementation.

## Architecture boundaries

- `Adrai.Git` invokes Git with explicit argument arrays and returns exact object data.
- `Adrai.Repository` binds configuration and managed bytes to one resolved commit OID.
- `Adrai.Format.*`, `Adrai.Graph`, and `Adrai.Compiler.*` validate documents, reduce state, and materialize SQLite.
- `Adrai.Service.*` owns revision-bound queries and transactional mutations.
- `Adrai.CliRunner` parses and dispatches the public executable.
- `Adrai.Explorer.*` implements the terminal UI.
- `Adrai.Web.Application`, `Adrai.Web.Server`, and `Adrai.Web.Assets` implement
  the authenticated loopback HTTP and WebSocket runtime over the shared service layer.
- `Adrai.Web.Api`, `Adrai.Web.Security`, `Adrai.Web.Events`, and
  `Adrai.Web.Socket` define admission and bounded event delivery.
  `Adrai.Web.Watch` observes raw repository facts; `Adrai.Service.Compilation`
  coordinates exact-revision archive creation. `Adrai.Web.Assets` embeds the
  generated Elm application and its provenance receipt into the executable.

Keep these boundaries narrow: low-level Git observation should not parse ADRAI documents or mutate refs, and repository reads must not fall back to ambient worktree bytes after resolving a revision.

## Tests and fixtures

Integration and E2E tests create temporary real Git repositories. They cover normal, bare, linked, sparse, shallow, detached, dirty, staged, Unicode, and conflict scenarios without committing `.git` fixtures.

Deterministic fixture plans live in `test/support`. Keep logical plans independent from their Git or service interpreters, record counts and invariants near the fixture, and treat generator tags, seeds, and canonical digests as reviewed contract changes.

The web page embeds checked-in HTML, CSS, and the generated Elm/bridge bundle
during the Haskell build. Use Node 24.15.0 and npm 11.12.1, then run from
`web/`:

```sh
npm ci
npm run build
npm run verify:assets
npm run test:assets
npm run test:components
```

`npm run build` compiles `src/Main.elm` with the pinned Elm 0.19.2 compiler and
`--optimize`, then appends `static/bridge.js` and publishes `dist/app.js` plus
`dist/provenance.json`. The receipt hashes every Elm source, the web manifests,
build and verification scripts, bridge, bootstrap HTML, CSS, and the exact
bundle bytes. It also records the pinned Node, npm, and compiler versions.
`verify:assets` rebuilds in a temporary directory and fails if either checked-in
file differs. Run it before the canonical Haskell build to prove the generated
bytes match a fresh optimized compile. `Adrai.Web.Assets` separately checks the
receipt's complete input path set, source and bundle hashes, and toolchain pins
at compile time before embedding captured bytes. Stack tracks the web inputs,
including new nested source files, and forces that module to recheck the receipt
on a warm build. The retained build-input inventory also hashes Elm sources,
fixtures, component and asset tests, manifests, build tools, static files and
generated files; it excludes `node_modules`, `elm-stuff`, and temporary output.
