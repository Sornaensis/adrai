# Browser workflows

From `browser-tests`, run:

```powershell
npm test
```

Install the pinned npm dependencies here and in `web` first if they are absent.
The suite uses the installed Playwright Chromium headless shell and Elm compiler.
Use ordinary Playwright filtering, for example `npm test -- --grep amend`.

Five independent workflows exercise production Elm `Main.init`, `update`, and
`view`, the production bootstrap HTML, and the production JavaScript bridge:
credential removal before asset requests and read-only reload, search and both
inspection views, checked create and refreshed reads, edits made while an amend
reply is pending, and explicit review after external invalidation. Each workflow
owns a small deterministic loopback HTTP fixture on an automatically chosen port.
Mutation assertions inspect the actual method, route, authorization, and JSON
fields emitted by the UI. A promise holds a reply to establish causal ordering.
Tests interact through visible controls; they do not inject model or DOM state.

This is UI assurance against deterministic transport responses. It does not
claim real Git, Haskell service, conflict, large paging, or reconnect coverage.
Native HTTP/WebSocket tests and Elm component tests retain their separate scopes.
The older `tests` directory and `run-p704-*`/`run-p705-*` supervisors are historical,
excluded from default discovery, and are not prerequisites for this command.

There are no case, expectation, action, or navigation deadlines or speed assertions.
Playwright timeout defaults are explicitly disabled, including for manually
created contexts. Interrupt a run that cannot make progress to diagnose its awaited
condition. Browser contexts and fixture servers close in nested `finally` blocks;
a thin Windows owned-job launcher contains descendants on cancellation or failure.
Its finite cleanup waits drain owned processes, not measure test performance.

The launcher prints a fresh OS temporary output directory. Elm compilation/cache,
Playwright output, and stdout/stderr remain there, outside the repository. The
test-only Elm entrypoint is compiled through an external manifest, so production
web sources, embedded assets, and their provenance are unchanged. Screenshots and
traces are off by default; ordinary Playwright CLI options can enable them while
keeping output in the selected temporary directory.
