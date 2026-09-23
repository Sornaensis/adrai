# Web interface contract

`adrai web` starts the repository-bound Haskell HTTP and WebSocket service and
serves its embedded Elm explorer from locally embedded assets.

## Repository binding and routes

The process starts in the current worktree and binds permanently to its
canonical worktree root, per-worktree Git directory, and common Git directory.
It accepts main and linked worktrees and rejects direct bare and non-Git
launches. Requests cannot carry a repository, local path source, Git executable
or arguments, shell command, or file contents. `WebOptions` models an optional
validated port and browser opening; runtime selection is restricted to
`127.0.0.1`. Start it with `adrai web --no-open`; omit `--no-open` to ask the
OS to open the one-time bootstrap URL. `--port PORT` selects a loopback port,
while omission asks the OS for an available port. Web startup rejects every
explicit global `--repo` override.

The exact GET routes are:

```text
/api/v1/repository
/api/v1/search
/api/v1/relevant
/api/v1/adrs/:id
/api/v1/history
/api/v1/compare
/api/v1/conflicts
/api/v1/doctor
/api/v1/events
```

The exact POST routes are `/api/v1/adrs` and
`/api/v1/adrs/:id/{amend,scope,domain,obsolete,reactivate}`. Method and path are
discriminated before request decoding. Query names are route-specific;
duplicate, valueless, unknown, malformed, and over-bound values fail as typed
input errors. JSON bodies have a 1 MiB default bound, reject unknown fields,
and materialize the existing shared service requests. Search and history
default to their CLI windows of 10 and 20 and accept limits from 1 through
1000. Relevant accepts 1 through 100. Encoded query data has a 4096-byte
default bound. Search requires `q`, including `q=` for deterministic blank
browse; it also accepts `actor=kind:identifier` and inclusive signed-decimal
Unix-millisecond `since`/`until` filters. History accepts those same filters.
Relevant defaults to a committed `at=HEAD` source; `worktree=true` reads the
safe current worktree file against the exact HEAD compilation context and
cannot be combined with an explicit `at`.

The explorer keeps each search or history response as one fixed result window
for its exact revision, filters, and limit. Page controls display at most 100
items from that window without another request or reranking. Changing the
window or an input fetches a new response and returns to page one. A full
search window is labeled as bounded rather than exhaustive; history uses its
server-provided `truncated` flag.

Collapsed ADR inspection returns the rich shared projection, including
candidate records, scopes and domains when a valid snapshot is conflicted.
Exploded inspection retains operation provenance and optional exploded-only
raw content. Web search also returns valid conflicted candidates for review;
the CLI's existing semantic-conflict result is unchanged.

Every response carries a monotonic process generation and an explicit `as_of`.
The JSON `metadata.generation` field, and the event envelope's `generation`,
are canonical unsigned decimal Word64 strings, including `"0"`. Clients must
reject numeric, signed, empty, leading-zero, fractional and out-of-range
forms, and compare valid strings by length followed by lexical order. The
`X-Adrai-Generation` header uses the same decimal text. At the maximum
generation, the server cannot allocate another freshness stamp: new dependent
work returns a 503 `generation-exhausted` unavailable response and requires a
server restart. Existing authenticated sockets close with
`generation-exhausted; restart the web server`, and new authenticated sockets
receive the same close reason; the watcher stops publishing after this terminal
condition. A mutation already committed before a publication failure
retains its actual operation and commit with a warning.
A successful one-snapshot query reports the exact resolved commit used to build
its payload, and compare reports both exact operands. Snapshot identity and its
generation are captured together under the shared Git lock; a successful
mutation publishes its commit generation after the ref CAS and before releasing
that lock. A competing observation receives a bounded `repository-busy` 503.
If generation publication fails after a durable commit, the mutation remains
`committed: true`, carries a `publication_warning`, and reports unavailable
freshness metadata instead of claiming a published commit generation.
Pre-authentication, no-commit, and failure responses use an explicit unavailable reason. JSON puts
metadata beside the unchanged shared projection; non-JSON responses use
`X-Adrai-Generation` and `X-Adrai-As-Of`. Events carry the same concepts in the
`adrai/events/v1` envelope. Each client view matches replies to its latest
request and immutable request context. A separate ordered invalidation
watermark marks affected views stale; an unrelated newer HTTP response cannot
suppress that invalidation. Known committed mutation outcomes retain their
operation and actual commit even if a later read or event has a newer stamp.

Malformed input maps to 400 (or 405 for a known route with the wrong method),
authentication to 401, origin/Host admission to 403, missing routes/resources
to 404, and stale state or semantic/CAS conflicts to 409. Unexpected shared
service failures map to 500. Runtime adapters preserve the existing CLI JSON
payload and committed post-index-warning outcome; they do not turn a durable
commit into failure because disposable indexing failed.

## Session and origin protocol

Each process owns at least 256 bits of cryptographically secure entropy for its
short-lived secret. Credential types redact diagnostic rendering and comparison
is constant-time. The initial `GET /?token=<secret>` is the only route that
accepts the secret in a query. It creates a process-specific cookie with
`Path=/; HttpOnly; SameSite=Strict`, no persistent expiry, `Cache-Control:
no-store`, and `Referrer-Policy: no-referrer`. The bootstrap script keeps the
token only in memory and removes it from browser history before loading other
resources; it never uses browser storage.
Reloading the cleaned URL can still read snapshots with its valid cookie, but
has no in-memory mutation or WebSocket credential. The explorer keeps those
actions disabled and directs the user to reopen the process bootstrap URL.

All requests require the exact `127.0.0.1:<bound-port>` Host and a matching
process cookie or `Authorization: Bearer`. Query tokens elsewhere and
duplicate, malformed, or conflicting credentials fail closed. Mutations also
require the explicit matching Bearer and `application/json`; the automatically
sent matching process cookie may coexist with that Bearer. POST and WebSocket
upgrade require the exact `http://127.0.0.1:<bound-port>` Origin. `null`, foreign
scheme/host/port, multiple Origin values, and forwarded-host substitution are
rejected. GET/static/bootstrap navigation may omit Origin, but any supplied
Origin must match. No permissive CORS policy is part of the contract.

Only `GET /api/v1/events` may upgrade to WebSocket. Admission checks the exact
Host, Origin, and cookie or Bearer credential before upgrade. The first
application message must be a text JSON object
`{"type":"authenticate","credential":"<process secret>"}`. The message
limit is 4096 bytes and the authentication deadline is five seconds. Binary,
malformed, wrong, repeated, or non-auth first messages close the socket before
repository data is emitted. A subsequent text control message may replace the
client's complete relevant-file interest set with
`{"type":"active-files","paths":["relative/path"]}`. The decoder rejects
duplicate or unknown fields, duplicate paths, and unsafe repository paths.
An empty `paths` array releases that client's interests; disconnect also
releases them. Each client may register at most 32 paths, with at most 256
distinct paths across clients. A control message never selects a repository or
causes a file's bytes to appear in an event.

After authentication, the server atomically subscribes the client and queues
a full `repository-invalidated` resync with its process generation and `as_of`
metadata. Later invalidations name changed fact categories. An
`observation-failed` event or unavailable `as_of` means the client must fetch
fresh HTTP state before acting. There is no durable event replay: reconnect
always starts with a new full resync. Clients discard responses and events
for superseded view contexts, reload current HEAD/ref and repository-state
and ADR-state tokens before submitting a mutation, and keep dirty forms visible
while marking them stale. A draft requires explicit review and token adoption
after such a refresh; it is never silently rebased or resubmitted.

The runtime admits at most 16 pending/authenticated sockets and 16 subscribers.
Each subscriber queue holds 64 events; overflow closes that socket so it must
reconnect and resync. Sends have a five-second bound, idle receives a
60-second bound, and close frames a one-second bound. Slow subscribers cannot
hold mutation publication or delay other subscribers.

## Mutation and observation boundaries

Repository responses issue a repository-state token derived from the immutable
binding, exact observed HEAD commit, and attached-ref or detached basis. Create
requires that token. Existing-ADR operations require it plus the existing ADR
`StateToken`; the shared request always receives `Just expected`. The two token
kinds are not interchangeable. The HTTP runtime must carry the captured
repository basis into the transaction and enforce the comparison under the Git
lock. A web preflight comparison outside the transaction is insufficient.
The HTTP mutation adapter passes this expected basis into the shared service,
which verifies it under the existing Git lock before any effect. Existing CLI
callers retain their prior behavior.

`Adrai.Web.Watch` is a separate, qualified fact-observation facade. Its injected
selectors have these shapes:

```haskell
repositorySnapshot :: Repo -> IO RepositorySnapshot
watchRepository :: Repo -> (RepositoryEvent -> IO ()) -> IO WatchHandle
```

It reports HEAD OID and ref identity, index, sequencer, configuration,
managed-source, common-ref, packed-ref, reflog, linked-worktree metadata, and
registered relevant-worktree-file facts or explicit observation failures. It
does not reduce graphs, rank search, infer invalidation semantics, or alter
`Adrai.Repository.repositorySnapshot`. Filesystem notifications only request
verification; the process worker also verifies every 250 ms, including when
notifications are lost or watcher setup fails. It fingerprints content and
bound directory identity, excludes `.adrai` cache activity, and limits each
scan to 4096 entries and 16 MiB. Missing, replaced, inaccessible, and unsafe
paths remain explicit facts or failures. External writers do not hold ADRAI's
Git lock, so a scan is not an atomic snapshot of arbitrary external writes;
periodic verification converges and stale scans are retried before publication.

The HTTP observer, mutation publisher, watcher, and initial WebSocket resync
share one process generation order. A successful mutation stamps its actual
commit under the Git lock after the ref CAS. Scans, socket sends, and
compilation run outside that short lock. Exact-revision search, relevance,
doctor, and web post-mutation indexing join one process-local compile flight
for a bound repository and exact commit OID, with at most eight concurrent
keys. The elected producer uses the existing validated cache publication path;
each consumer opens its own scoped validated context. CLI processes keep their
own cache coordination and may publish concurrently. Server shutdown rejects
new flights and waits for owned producers and their resources to close.
