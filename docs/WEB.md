# Web interface contract

The repository-bound web contract is available as Haskell modules, but the
`adrai web` executable, HTTP listener, browser assets, filesystem observer, and
Elm application are not exposed yet. `Adrai.Web.Api`, `Adrai.Web.Security`,
`Adrai.Web.Events`, and `Adrai.Web.Watch` are the production contract consumed
by those later runtime increments.

## Repository binding and routes

The future process starts in the current worktree and binds permanently to its
canonical worktree root, per-worktree Git directory, and common Git directory.
It accepts main and linked worktrees and rejects direct bare and non-Git
launches. Requests cannot carry a repository, local path source, Git executable
or arguments, shell command, or file contents. `WebOptions` models an optional
validated port and browser opening; runtime selection is restricted to
`127.0.0.1`.

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
and materialize the existing shared service requests. Search/result limits
default to their CLI values and have a maximum of 100; encoded query data has a
4096-byte default bound.

Every response carries a monotonic process generation and an explicit `as_of`.
A successful one-snapshot query reports the exact resolved commit used to build
its payload, and compare reports both exact operands. Pre-authentication,
no-commit, and failure responses use an explicit unavailable reason. JSON puts
metadata beside the unchanged shared projection; non-JSON responses use
`X-Adrai-Generation` and `X-Adrai-As-Of`. Events carry the same concepts in the
`adrai/events/v1` envelope. Clients discard responses or events older than the
latest generation they have accepted.

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

All requests require the exact `127.0.0.1:<bound-port>` Host and a matching
process cookie or `Authorization: Bearer`. Query tokens elsewhere and
duplicate, malformed, or conflicting credentials fail closed. Mutations also
require the explicit matching Bearer and `application/json`; the automatically
sent matching process cookie may coexist with that Bearer. POST and WebSocket
upgrade require the exact `http://127.0.0.1:<bound-port>` Origin. `null`, foreign
scheme/host/port, multiple Origin values, and forwarded-host substitution are
rejected. GET/static/bootstrap navigation may omit Origin, but any supplied
Origin must match. No permissive CORS policy is part of the contract.

WebSocket admission performs credential, Host, and Origin checks first. The
first application frame is the strict `authenticate` object with the same
process credential. The default contract bounds that frame to 4096 bytes and
authentication to five seconds. Malformed, wrong, repeated, or non-auth first
frames close the socket before repository data is emitted.

## Mutation and observation boundaries

Repository responses issue a repository-state token derived from the immutable
binding, exact observed HEAD commit, and attached-ref or detached basis. Create
requires that token. Existing-ADR operations require it plus the existing ADR
`StateToken`; the shared request always receives `Just expected`. The two token
kinds are not interchangeable. The HTTP runtime must carry the captured
repository basis into the transaction and enforce the comparison under the Git
lock. A web preflight comparison outside the transaction is insufficient.
Extending the create service with this expected-basis CAS while preserving the
CLI's current behavior belongs to the server increment.

`Adrai.Web.Watch` is a separate, qualified fact-observation facade. Its injected
selectors have these shapes:

```haskell
repositorySnapshot :: Repo -> IO RepositorySnapshot
watchRepository :: Repo -> (RepositoryEvent -> IO ()) -> IO WatchHandle
```

It reports HEAD, index, sequencer, configuration, managed-source, common-ref,
packed-ref, reflog, linked-worktree metadata, and open relevant-worktree-file
facts or explicit observation failures. It does not reduce graphs, rank search,
infer invalidation semantics, or alter
`Adrai.Repository.repositorySnapshot`. Runtime fsnotify/verification,
single-flight compilation, and event translation belong to the observation
increment.
