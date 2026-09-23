# Installation

ADRAI is currently installed from source.

## Requirements

- Git 2.31 or newer
- Stack 3.11.1

The project pins its compiler and package set in `stack.yaml` (`lts-24.52`). Stack downloads the matching GHC toolchain when needed. Python and libgit2 are not runtime dependencies.

## Build

From the repository root:

```console
stack build
stack exec adrai -- --help
```

To copy the executable into Stack's local binary directory:

```console
stack install
adrai --help
```

If Stack's binary directory is not on `PATH`, use `stack path --local-bin` to locate the installed executable.

The source-built executable embeds the checked-in `web/static/index.html`,
`web/static/app.css`, optimized Elm/bridge bundle `web/dist/app.js`, and its
`web/dist/provenance.json` receipt. The build checks the receipt against the
web sources and bundle. Node and Elm are needed only to rebuild or verify web
assets, not to run the installed executable; see [Development](DEVELOPMENT.md).

From the target Git worktree, start the browser explorer, HTTP API, and event
WebSocket:

```console
adrai web --no-open
```

The service binds to `127.0.0.1` and this worktree; `web` rejects an explicit
global `--repo`. Open the one-time URL printed by the process. Its credential
stays in page memory and is removed from the browser URL before assets load.
Reloading that cleaned URL allows read-only snapshots while the session cookie
is valid. Reopen the process bootstrap URL for mutations and live updates.
See [Usage](USAGE.md) for repository setup and browser operation, and the
[Web interface contract](WEB.md) for API and session details.
