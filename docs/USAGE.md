# Usage

ADRAI operates on an existing Git repository. Run `adrai --help` to see all 16 commands grouped by purpose. Pass `--repo PATH` before the command to target a repository other than the current directory (for example, `adrai --repo ../project show ADR_ID`). `web` must run in its worktree and rejects an explicit `--repo`.

## Quick start

Initialize ADRAI in an existing Git repository:

```console
adrai init
```

This creates and commits `.adrai.toml`, `.gitattributes`, and `.gitignore`. If the Git rule files already exist, `init` keeps their contents and appends only missing ADRAI rules. The default managed roots are `architecture/adrai/decisions` and `architecture/adrai/connections`. Initialize only once per repository.

Create an ADR:

```console
adrai create \
  --title "Use PostgreSQL" \
  --summary "Store transactional application data in PostgreSQL." \
  --body "PostgreSQL provides the consistency and tooling the service needs." \
  --domain data \
  --applies-to "src/**" \
  --actor human:YOUR_NAME
```

On PowerShell, enter the command on one line or replace each trailing `\` with a backtick.

Inspect and search the repository:

```console
adrai show ADR_ID
adrai history ADR_ID
adrai search "transactional database"
adrai relevant src/Storage.hs --worktree
adrai doctor
```

Use the identifier printed by `create` in place of `ADR_ID`. `init`, `create`, `amend`, `scope`, `domain`, `obsolete`, and `reactivate` create Git commits. Read and diagnostic commands do not create Git commits; `compile` may build a disposable derived index.

## Commands

| Command | Purpose |
| --- | --- |
| `init` | Add the default ADRAI configuration to a repository. |
| `create` | Create an ADR and its initial scope, domain, and status records. |
| `amend` | Replace a decision's title, summary, or body. |
| `scope` | Add, remove, or replace file-scope patterns. |
| `domain` | Add, remove, refine, replace, or clear domains. |
| `obsolete` / `reactivate` | Change an ADR's lifecycle state. |
| `show` | Display one collapsed or exploded ADR. |
| `history` | Display operation history, optionally filtered by ADR or actor. |
| `search` | Search ADRs with FTS, vector, or hybrid retrieval. |
| `relevant` | Rank ADRs against a committed or worktree file. |
| `compare` | Compare two immutable revisions. |
| `compile` | Build or reuse the repository's derived SQLite index. |
| `doctor` | Report source, graph, provenance, and cache diagnostics. |
| `explore` | Open the terminal explorer. Its read commands currently print placeholders; use the CLI or `web` for real results. |
| `web` | Start the authenticated loopback repository explorer, HTTP API, and event WebSocket. |

Most read commands accept `--at REVISION`; the default is `HEAD`. Most commands also accept `--json` for stable machine-readable output. Run `adrai COMMAND --help` for the complete option list.

Mutation commands require an actor in `kind:identifier` form and create Git commits. Pass `--actor` or set `ADRAI_ACTOR`; valid kinds are `human`, `llm`, and `service`. Options such as `--expect STATE_TOKEN` provide optimistic concurrency checks when a caller is acting on previously read state.

In the terminal explorer, `:help` lists accepted input syntax and a first step. `help`, `exit`, `quit`, and `:q` also work. `search QUERY` (or free text), `show ADR_ID`, `view ADR_ID [collapsed|exploded]`, `history [ADR_ID]`, and `conflicts` parse, but their output is currently a placeholder, not repository data. Use `adrai search`, `adrai show`, `adrai history`, or `adrai web` for live inspection. `status ADR_ID active|obsolete` (also `:status`) commits a status change and exits on success. Terminal `create` and `amend` input is unavailable and makes no Git commit; use the corresponding CLI commands or web forms for those edits. Malformed command-shaped input reports guidance instead of becoming a search.

Run `adrai web --no-open` from a worktree to print a one-time authenticated
loopback URL without opening a browser. Use `--port PORT` to request a specific
port. Web mode is permanently bound to the current worktree and therefore does
not accept the global `--repo` option. Authenticated clients may subscribe to
`/api/v1/events` for repository invalidations; see [Web interface](WEB.md) for
the WebSocket control frames and reconnect rules.

Open the printed URL to use the three-pane explorer. The left pane chooses a
revision and shows browse, search, relevance, history, compare, conflicts, and
doctor results. The middle pane inspects a decision, its conflict candidates,
and operation provenance. The right pane holds checked create, amend, scope,
domain, obsolete, and reactivate forms. Search and history use a server window
of at most 1000 items; pages of at most 100 items move within that window.
A full search window may have more matches.
Existing-ADR actions become available after both decision and operation
inspection have loaded at the selected revision.

The one-time URL credential stays in page memory and is removed from browser
history before assets load. Reloading the cleaned URL can still show readable
HTTP snapshots while the session cookie is valid, but live updates and
mutations require reopening the process bootstrap URL. A stale draft keeps its
contents; refresh the repository, inspect the exact current decision and
candidate heads, then explicitly adopt new state tokens before submitting.
Historical inspection is read-only.
If a decision inspection is temporarily busy, the explorer retries that view
without discarding the other view. If it remains busy, use the retry button in
the middle pane when the repository settles.
If event generation is exhausted, the explorer preserves its visible snapshots
and draft but stops freshness-dependent work. Restart the web server and open
its new bootstrap URL before resuming edits.

## Exit status

- `0`: success
- `2`: invalid input, repository integrity failure, or another user-facing error
- `3`: a semantic conflict prevented the requested operation or projection

See [Conflict handling](CONFLICTS.md) for the distinction between conflicts and malformed history.
