# Elm component checks

Run `npm run test:components` from `web`. The runner reads
`web/fixtures/api-v1.json`, generates a temporary Elm module containing those
exact cases, runs `elm-test` against production decoders, route windows, form
builders, and state updates, then removes the generated module. The fixture is
owned by the API contract tests as well. Real browser behavior is checked by
the bounded P7-04 Playwright smoke in `browser-tests`.

The PowerShell supervisor uses the repository's Windows Job helper to assign
the Node worker before it runs. Its 60-second deadline includes setup, every
wait, cleanup, and evidence. It independently waits for the root process and
the whole Job to empty before removing the temporary module. Each run emits
an external JSON receipt with source and fixture hashes. Run the same
supervisor with `-Probe timeout`, `spawn-failure`, `early-success`, or
`early-error` to exercise cleanup and descendant ownership; each probe succeeds
only if its expected outcome and complete cleanup are both verified.
