# Usage

ADRAI operates on an existing Git repository. Run `adrai --help` to see all 17 commands grouped by purpose. Pass `--repo PATH` before the command to target a repository other than the current directory (for example, `adrai --repo ../project show ADR_ID`). `web` must run in its worktree and rejects an explicit `--repo`.

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
adrai search --limit 1000
adrai domains
adrai domains platform.storage
adrai domains --tree
adrai search --domain platform.storage
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
| `search` | Search ADRs with FTS, vector, or hybrid retrieval, or list non-obsolete ADRs with no query. |
| `domains [DOMAIN]` | Browse top-level domains or immediate children with recursive distinct ADR counts; `--tree` draws every level. |
| `relevant` | Rank ADRs against a committed or worktree file. |
| `compare` | Compare two immutable revisions. |
| `compile` | Build or reuse the repository's derived SQLite index. |
| `doctor` | Report source, graph, provenance, and cache diagnostics. |
| `explore` | Open the terminal explorer. Search, inspection, history, conflicts, and checked edits use the shared repository services. |
| `web` | Start the authenticated loopback repository explorer, HTTP API, and event WebSocket. |

Most read commands accept `--at REVISION`; the default is `HEAD`. Most commands also accept `--json` for stable machine-readable output. Run `adrai COMMAND --help` for the complete option list.

Ordinary invocations print plain text. `show ADR_ID` displays the ID and title,
lifecycle status, domains, file scope, summary, and decision body with real
newlines. `show ADR_ID --view exploded` lists the requested operations, records,
events, and their readable content. Use `--json` for the structured projection,
including state tokens and provenance, or when parsing the output in a script.
Canonical serialization remains available through explicit `--json`; list projections include additive pagination metadata.

`history` prints compact rows with each operation's claimed timestamp, ID,
change label, ADR, title, and actor, followed by a reason when present. Its
filters, order, and `--limit` also apply to this text view. `compare FROM TO`
prints counts and ADRs classified as added, removed, changed, or unchanged,
with changed field names. Add `--include-unchanged` to list unchanged ADRs.
Both commands emit their structured projections only with `--json`.

`history --page N --limit N` pages filtered operations without splitting a grouped
operation (default page 1, size 20, maximum 1000). `truncated` is `has_next`;
use the next `--page` to continue. `compare --page N --limit N` pages visible ADR
entries (default page 1, size 20, maximum 1000), while summary category counts
always cover the complete comparison. Both commands print
`RETURNED of TOTAL results — Page CURRENT of PAGES`, including empty and final
pages. Legacy unpaged comparisons use only `N of N results`.
The compare service and HTTP API keep complete unpaged results when both
`page` and `limit` are omitted; either parameter opts into pagination. Unpaged
JSON has null `page`, `page_size`, and `total_pages`, full totals, and zero remaining.

`domains DOMAIN --tree` includes the parent and every descendant, showing
`domain D (N descendants)` for distinct exact and strictly descendant assignments.
The same ADR can belong to both counts. Flat domain rows keep inclusive counts.
Combine `--tree --json` for nested `children` (`adrai/domains-tree/v1`); default
flat JSON remains `adrai/domains/v1`. Domain reads reuse exact archives published
by `compile` or successful mutation indexing. A miss reads committed state without
creating an archive; repeated misses remain cold. See [Search](SEARCH.md).

`search` and `relevant` print simple ADR ID and title lists by default. Add
`--view exploded` to include indented summaries. JSON requires `--json` and
contains `schema`, `algorithm`, `pagination` and `results`, with `adr`, `title` and a
numeric `score` in `[0,1]` in each result, plus `summary` in the exploded view.
Scores preserve the existing ranking totals rounded to six decimal places;
they are not confidence levels or rescaled against the response. Blank search
uses `filtered-list` with score `1` as an eligibility sentinel. Score metadata
appears only in JSON. The CLI schemas are `adrai/search-cli/v1`
and `adrai/relevant-cli/v1`; see [Search](SEARCH.md) for details.

Omitting the search query or supplying blank text lists non-obsolete ADRs in ADR
ID order at the selected committed revision (default `HEAD`). The listing is
paged by `--limit`: the default is 10 and the supported range is 1–1000.
Add `--include-obsolete` to include obsolete decisions. File,
domain, actor, and time filters also apply to the listing.
`--limit` is the page size (1–1000, default 10); `--page` is one-based and defaults
to 1. A blank query pages the full filtered corpus, so `adrai search --limit 1000
--page 2` can visit ADRs beyond the first 1000. Ranked queries page a documented
fixed retrieved result set; changing page size does not change ranking budgets.
Text ends with `RETURNED of TOTAL results — Page CURRENT of PAGES`.
JSON retains its full `pagination` metadata, including remaining counts and
empty or out-of-range pages. See [Search](SEARCH.md) for the retrieval policy and metadata fields.
Domain filters match exact assignments by default. Add `--all` to include dotted
descendants of every requested `--domain`; obsolete ADRs still require
`--include-obsolete`.

`show --raw` requires `--view exploded`, for example
`adrai show ADR_ID --view exploded --raw`. It prints each raw semantic document
(metadata and body) directly. Combining `--raw --json` keeps that text in the
existing `raw_semantic` JSON fields.

CLI mutation commands require an actor in `kind:identifier` form and create Git commits. Pass `--actor` or set `ADRAI_ACTOR`; valid kinds are `human`, `llm`, and `service`. Options such as `--expect STATE_TOKEN` provide optimistic concurrency checks when a caller is acting on previously read state. The option-free terminal explorer instead defaults to `human:terminal-explorer` and changes its actor through the `:actor` session control.

`amend` requires exactly one content source: `--body`, `--body-file`, `--stdin`,
or `--input-json PATH_OR_DASH` (`-` reads JSON from standard input). It also
requires a nonblank `--change-summary` or JSON `change_summary`. Amendment JSON
uses its own schema: `body`, `change_summary`, optional `title` and `summary`,
and optional `actor`, `model`, `input_digest`, `prompt_digest`, and
`context_digest`. Supply `body` in JSON; `--change-summary` may supply its
rationale. Omitted title and summary inherit the current decision's values.

For `scope`, combine `--add` and `--remove` for a delta, or use repeatable
`--set` for a reviewed replacement. `--set` excludes `--add` and `--remove`.
For `domain`, choose exactly one mode: `--add`/`--remove` (which may be
combined), repeatable `--refine FROM=TO`, repeatable `--set`, or `--clear`.
The four domain modes are mutually exclusive.

### Review before committing

Add `--preview` or its synonymous `--dry-run` to `init`, `create`, `amend`,
`scope`, `domain`, `obsolete`, or `reactivate` to review the validated proposed
files without changing repository files, the caller's index, caches, Git objects,
refs, or commits. Both spellings work with readable output and `--json`:

```console
adrai --repo ../project init --preview
adrai create --title "Use PostgreSQL" --summary "Store application data" --body "Require transactions." --actor human:alice --dry-run --json
adrai amend ADR_ID --body "Require transactions and compatible migrations." --change-summary "Document migrations" --actor human:alice --preview
```

The proposal is bound to the observed attached branch and HEAD. Existing ADRs
and configuration come from that committed revision; dirty managed files do not
become proposal inputs. Unrelated staged or unstaged changes are preserved.
`init` can preview an existing unborn Git repository, merges the caller's rule
file bytes, and displays previous and proposed bootstrap bytes. Unsafe or
occupied generated destinations, detached or bare repositories, active Git
operations, stale `--expect` tokens, and invalid lifecycle changes are rejected.
Preview also refuses partial-clone or promisor repositories before reading
objects: Git can otherwise fetch missing promised objects and write its object
store during a read. This conservative preview-only refusal includes promisor
configuration (with included config files), partial-clone filters, and existing
`.promisor` pack markers. Ordinary commands retain their existing behavior.

JSON uses the dedicated `adrai/preview/v1` schema with `preview: true`,
`committed: false`, `operation`, `basis` (a commit OID, or `null` for unborn
init), `head_ref`, `adr_id`, `state_token`, `operation_id`, `commit_subject`,
`commit_trailers`, and `files`. Each file has a repository-relative `path`, full
`content`, exact `bytes_hex`, and `previous_bytes_hex` for existing init inputs
(`null` otherwise). Content is displayed as UTF-8; hex preserves exact bytes
even for rule files containing non-UTF-8 data. No commit OID or indexing success
is claimed. Success exits 0; request errors exit 2 and state conflicts exit 3.

Generated identifiers and timestamps are provisional (`provisional: true`).
Rerun the same command without either flag to commit: it generates fresh IDs
and timestamps and validates the current repository again. A preview is not an
apply token and does not reserve paths or hold a lock.

In the option-free terminal explorer, `:help` lists the syntax. Use `search QUERY`
(or free text), `show ADR_ID`, `view ADR_ID [collapsed|exploded]`,
`history [ADR_ID]`, and `conflicts` for repository data. Session controls are
`:view collapsed|exploded`, `:mode fts|vector|hybrid`, `:obsolete on|off`,
`:file PATH|clear`, `filter file PATH|clear`, `filter obsolete on|off`,
`:actor human|llm|service:ID`, `:revision REF`, and `:refresh`.

`create JSON` accepts `title`, `summary`, `body`, `domains`, and `applies_to`;
`amend ADR_ID JSON` accepts `body`, `change_summary`, and optional `title` and
`summary`. Inspect an ADR before `amend` or `status ADR_ID active|obsolete`.
Checked edits retain the viewed repository basis and ADR state token: an
external change rejects the edit rather than silently adopting new state.
Explicit non-HEAD revisions are read-only. `:refresh` returns to HEAD and clears
viewed tokens; inspect again before editing. A successful commit exits the
session; errors leave it open. `exit`, `quit`, and `:q` cancel without a commit.

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
- `4`: `doctor` completed and reported `ok=false`

See [Conflict handling](CONFLICTS.md) for the distinction between conflicts and malformed history.
