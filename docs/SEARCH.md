# Search

ADRAI exposes two related commands:

- `search [QUERY]` searches compiled ADR content.
- `relevant FILE` ranks ADRs against a source file.

Both operate against a resolved Git revision. By default, each result is a single
line containing its raw score, ADR ID and title, in descending numeric score
order. Scores use two decimal places (`0.10`, `0.03`, `1.00`), at most three
digits plus the decimal point; rounding is for display only. Equal-score blank
listings retain ADR ID order. `--view exploded` adds the summary below
each title, indented by two spaces. Every response ends with a compact page footer,
including no matches; successful queries produce no stderr output. Result order and limits are unchanged by the
output format. Conflicted ADRs retain their conflict title rather than displaying
one matched head as a resolved decision.

JSON is opt-in with `--json`. The CLI schemas are `adrai/search-cli/v1` and
`adrai/relevant-cli/v1`. Each envelope contains `schema`, `algorithm`, `pagination` and
`results`; each row contains only `adr`, `title` and `score`, plus `summary`
with `--view exploded`. Source paths, scoring breakdowns, evidence and
diagnostics are omitted. For example:

```json
{"schema":"adrai/search-cli/v1","algorithm":"three-fts+weighted-rrf","results":[{"adr":"A0123456789ABCDEFGHJKMNPQRS","title":"Use transactional storage","score":0.047619}]}
```

Scores are the existing total ranking scores in `[0,1]`, rounded to six decimal
places. They are not probabilities or confidence levels, and are not rescaled
against other results in a response. Search ranking scores typically occupy a
small part of this range. With an omitted or blank search query, `algorithm` is
`filtered-list` and every result has score `1`, an eligibility sentinel rather
than a relevance measurement. The algorithm is included even when there are no
results.

For nonblank search queries, the algorithm labels are:

| Mode | Algorithm |
| --- | --- |
| `fts` | `three-fts+weighted-rrf` |
| `vector` | `tei+sqlite-vec+weighted-rrf` |
| `hybrid` | `three-fts+tei+sqlite-vec+weighted-rrf` |

Relevance reports `tei+sqlite-vec+passage-fts` when the embedding service is available,
or `passage-fts` when it uses lexical retrieval alone. Algorithm metadata remains
in JSON, and JSON scores retain six-decimal precision. Plain and expanded text show the
two-decimal score before the ID/title, with optional summaries.

Detailed service and HTTP projections remain available separately. Relevance
results and ordering are deterministic for the same revision and source bytes;
measured service diagnostics vary between runs.

## ADR search

```console
adrai search "database migration"
adrai search DROPWIRE_DATABASE --mode fts --domain operations
adrai search "lease token" --mode vector --limit 5 --json
```

Retrieval modes are:

- `fts`: SQLite FTS5 with exact, phrase, proximity, prefix, stemming, and identifier channels.
- `vector`: external TEI text embeddings retrieved with sqlite-vec cosine distance.
  If the optional service or vector runtime is unavailable, it uses FTS5.
- `hybrid`: weighted reciprocal-rank fusion of the active FTS and vector channels; this is the default.

Filters include file scope, repeatable domains, actor, inclusive timestamps, obsolete state, and immutable revision. Filters are applied before candidates are ranked. A blank query returns a deterministic filtered ADR listing.

`--domain platform.storage` matches ADRs assigned to that exact canonical domain.
Add `--all` to include dotted descendants, such as `platform.storage.postgresql`.
Both modes exclude near prefixes such as `platform.storage-old`. Repeat `--domain`
to require every requested domain (or every requested subtree with `--all`) to
match. `--all` does not include obsolete ADRs; use `--include-obsolete` separately.
Omit the query to browse matching ADRs with the usual page size.

Search accepts `--page N` (one-based, default 1) and `--limit N` (results per
page, default 10, maximum 1000). A blank query pages the complete filtered ADR
corpus in ADR ID order, including corpora larger than 1000. For example,
`adrai search --domain platform --all --limit 1000 --page 2` returns the next
window of that subtree. Domain, file, actor, time, revision, and obsolete filters
are applied before counting and paging. `--all` only expands domain matching;
it does not change the page size or include obsolete decisions.

Ranked queries page the complete distinct ADR result set produced by a fixed
retrieval policy anchored to retrieval depth 10. The policy allows 300 hits per
individual FTS query, retains the existing union of exact/near/prefix terms,
uses a prefix fallback threshold of `min(30, allowed items)`, retrieves at most
300 eligible document/passage vector neighbours, and reranks up to 80 documents
plus their competing heads. These budgets are independent of both page and page
size. Ranking and ADR deduplication finish before paging.
Ranked totals describe this retrieved result set, while blank-query totals
describe the full filtered corpus.

Every text response ends with `RETURNED of TOTAL results — Page CURRENT of PAGES`,
for example `2 of 5 results — Page 2 of 3`. Empty sets use the same format,
such as `0 of 0 results — Page 1 of 0`; pages beyond the end also retain the
requested page, such as `0 of 1 results — Page 2 of 1`.
The CLI and rich JSON projections add a `pagination` object with `page`,
`page_size`, `total_results`, `total_pages`, `returned`, `remaining`, and
`has_next`. Empty sets have `total_pages: 0`. Existing result row fields and
schema names are preserved; this metadata is additive.

Collapsed results group competing decision heads by ADR. `--view exploded` adds summaries; use `adrai show ADR_ID --view exploded` for operation-level detail. Use `--include-obsolete` to include obsolete ADRs.

## Optional text embeddings

Configure your local TEI service in the **user** file `adrai/config.toml` under
`System.Directory.getXdgDirectory XdgConfig`: normally
`~/.config/adrai/config.toml` on Linux and `%APPDATA%\adrai\config.toml` on
Windows. `XDG_CONFIG_HOME` overrides the config base directory. This is separate
from the committed repository configuration. ADRAI does not create this file or
assume an endpoint. For example, replacing the endpoint with yours:

```toml
[text_embedding]
endpoint = "http://localhost:8080"
timeout_ms = 2000
batch_size = 16
```

The optional `model = "your-model-id"` setting must exactly match TEI `/info`
`model_id`; omit it to use the live model. `endpoint` is the local TEI `http://`
base URL, using `localhost`, numeric IPv4, or bracketed numeric IPv6, without
credentials, query or fragment. `localhost` uses IPv4 loopback. Other hostnames
and HTTPS endpoints select FTS5-only retrieval. Proxy environment settings are
bypassed; no DNS lookup is performed.
ADRAI calls `GET /info` and `POST /embed` with `inputs`, `normalize=true`
and `truncate=true`. TEI truncates text beyond its model token limit; evidence
still uses the original text. This policy is part of the vector cache identity.
`timeout_ms` is the per-request timeout (100-10000 ms,
default 2000). The complete optional phase has a 30-second deadline; leaving an
in-flight native socket operation can add up to one configured socket interval. `batch_size`
is 1-64 (default 16). Each text is limited to 32768 UTF-8 bytes and responses to
8 MiB. Malformed configuration, unavailable services, HTTP errors, model mismatch,
invalid response counts/dimensions/nonfinite or zero vectors, and sqlite-vec
failures all select FTS5-only retrieval. Explicit `--mode vector` also falls back.
Detailed search `mode` and algorithm metadata describe the effective retrieval.
Explicit `--mode fts` and blank listings make no service calls.

The optional vector cache lives under `getXdgDirectory XdgCache` in
`adrai/text-embeddings-v1`. It stores float32 corpus vectors by endpoint, live
model ID/revision, dimensions, format and input digest. Changed corpus text or a
changed provider/model cannot reuse incompatible vectors. Source/query embeddings
remain request-local. The model and query embeddings must succeed on each request,
so an outage uses FTS5 even after the corpus cache has warmed. Cache failures do
not invalidate immutable lexical archives. This cache is disposable and can be
removed while ADRAI is stopped. sqlite-vec v0.1.6 is bundled; no extension install
or shared-library path is needed.

## Browse domains

```console
adrai domains
adrai domains platform
adrai domains --tree
adrai domains platform.storage --tree --json
adrai domains platform.storage --at HEAD~1 --include-obsolete --json
adrai search --domain platform.storage --all
```

With no argument, `domains` lists top-level names. With a parent, it lists only
immediate children using full canonical names. Intermediate prefixes are inferred
from assignments: `platform.storage.postgresql` also makes `platform` and
`platform.storage` browsable. Rows are sorted by domain and print `DOMAIN COUNT`.
A leaf, unknown parent, or empty hierarchy produces empty text output. Malformed
domain input fails validation; input case and whitespace are canonicalized.

Each count includes distinct ADRs assigned directly to that displayed domain or
any dotted descendant. Multiple assignments under one subtree count the ADR once.
Counts use the complete resolved Git snapshot, without search's 10/1000 result
limits. Obsolete ADRs are hidden by default; `--include-obsolete` includes them.
`--at REVISION` selects committed historical state (default `HEAD`). Competing
domain heads contribute their union so unresolved assignments remain discoverable;
only unambiguously obsolete ADRs are hidden, consistent with search eligibility.

JSON uses `adrai/domains/v1` with the resolved commit in `as_of`, the canonical
requested `parent` (or `null` at the top level), and a `domains` array of
`{"domain":"platform.storage","adr_count":12}` rows. Empty listings retain this
envelope with an empty array.

Add `--tree` to draw every level, with separate distinct ADR counts at each
node. With a parent argument, the tree includes that parent as its root; a leaf
therefore produces one row, and an unknown parent produces none. Each row is
`domain D (N descendants)`: D counts exact assignments, while N counts assignments
strictly below that domain. Each set deduplicates ADRs, including conflicting
domain heads. An ADR assigned both directly and below appears in both counts;
N is not computed by subtracting D from the inclusive total. Inferred parents
can have zero direct assignments. Each root keeps its fully qualified
name; descendants display only their local segment:

```text
platform 0 (12 descendants)
├── api 7 (0 descendants)
└── storage 3 (5 descendants)
    └── postgresql 5 (0 descendants)
```

`--tree --json` uses `adrai/domains-tree/v1`. It keeps `as_of`, `parent`, and
`domains`, with a `children` array on every domain node (empty at leaves).
Each node retains inclusive `adr_count` and adds `direct_adr_count` and
`descendant_adr_count` for the separate exact and strict-descendant sets.
JSON domain names remain fully qualified, including descendants. For example,
`domains platform.storage --tree` prints `platform.storage` at the root and
`postgresql` below it, while JSON retains `platform.storage.postgresql`.
Without `--tree`, the flat text and `adrai/domains/v1` JSON contracts are unchanged.

Domain reads reuse an existing validated exact-revision archive without rebuilding
search materialization. `adrai compile` and successful mutation post-commit
indexing normally publish these archives. A missing or rejected archive falls
back to the same committed snapshot integrity checks and provenance read; `domains`
does not populate an archive, so repeated reads without one still take this cold
path. `--at` reuse requires an archive for that exact resolved commit.

## File relevance

```console
adrai relevant src/Storage.hs
adrai relevant src/Storage.hs --worktree
adrai relevant src/Storage.hs --at main~1 --json
```

Without `--worktree`, the file and ADR context come from the same resolved revision (`HEAD` by default). `--worktree` reads the current file safely from the worktree while keeping ADR context bound to `HEAD`; it cannot be combined with `--at`.

`--page N` selects a one-based page (default 1); `--limit N` sets its size
(default 10, range 1–100). CLI and HTTP responses include the shared `pagination`
fields described above; text uses the same compact returned/total and page footer.
Only ADRs with an overall raw score of at least `0.1` are returned. This inclusive
floor is applied before counting and paging, using the unrounded score. Confidence
and margins still use every retrieved competitor, including those below the floor.
The floor applies to `relevant`; `search` retains its existing ranking score scale
and admission behavior.
Totals describe the retained distinct ADRs retrieved by a fixed depth-10 policy,
not all `eligible_adrs`. Each selected source chunk retrieves up to 60 lexical
passages and 40 vector passages. Up to 24 informative source chunks participate.
Only eligible passages enter sqlite-vec, before its K-neighbour limit.
Changing page size never changes these budgets.
All competitors contribute to confidence and margins before a page is selected,
so concatenating pages preserves complete-result order, evidence and confidence.

Input is limited to 4 MiB. Binary-like input, NUL bytes, and very short or uninformative text are rejected or produce no weak match. Informative text is chunked and retrieved through passage FTS and optional text embeddings. The retrieved passages are ranked and aggregated per ADR. File scope affects ranking but does not exclude an otherwise relevant ADR.

### Detailed relevance service and HTTP JSON contract

The service projection and the `data` member of `/api/v1/relevant` use `adrai/relevant/v2`. CLI consumers migrating from that rich projection should use `adrai/relevant-cli/v1` for lists, or the HTTP API for detailed evidence and diagnostics. The API route version describes the HTTP envelope; the `data.schema` value identifies this projection. Each `results` entry is one ADR, ordered by descending `score`, `semantic_score`, `lexical_score`, then ADR ID. `record` identifies the resolved decision record; a conflict has `record: null`, while `matched_record` and `matched_title` identify the head that supplied the strongest evidence. `title`, `summary`, `domains`, `applies_to`, status, replacement, and resolution fields retain decision context.

`score` combines semantic evidence, `lexical_bonus`, and `scope_bonus`. The lexical signal uses source content and localized source-definition phrases, with file scope a small bonus. The reported `semantic_score`, `lexical_score`, `scope_match`, `scope_bonus`, `margin`, strongest-pair scores, and `source_information` explain ranking and confidence. Scope alone cannot produce high confidence. The `retrieval.scoring` object lists the scoring constants. Result order and evidence order are deterministic for the same revision and file bytes.

Service and HTTP relevance responses also include `diagnostics`. `index_revision` is the exact commit in the validated archive or cold-compiled snapshot actually queried; `checkout_head` is a separate observation of the checkout's HEAD at query start. `stale` means these commits differ. An explicit historical `--at` is therefore expected to be stale when the checkout has advanced. A worktree source still uses HEAD for ADR context; its bytes come from the working file.

`index_preparation` is `cache-hit` or `cold-fallback`. All `timing_ms` values are measured elapsed milliseconds from a monotonic clock. `prepare_index` runs from exact-archive acquisition/validation (including a rejected attempt) through source loading, or through cold snapshot analysis and compilation, until the compiled context and source are ready. `retrieve_candidates` covers source preparation, vector and FTS retrieval, shortlist and fully evaluated passage-pair construction. `rank_results` covers aggregation, ordering, result scoring, and materialization of the published JSON projection, including evidence excerpts and resolution fields. The latter two are `null` when a short or empty source skips candidate work. Timings are observational; they do not affect result scores or ordering and are not latency targets.

Each result has at most three ranked `evidence` entries and at most three distinct `passages`. A passage stores `adr_chunk`, `adr_section`, `candidate_record`, and an `adr_excerpt` once. Each evidence entry has a zero-based `passage` index, source `file_lines`, semantic and lexical pair scores, and `matched_terms`. The first evidence entry also carries `file_excerpt`; later entries use the source line range and passage reference to avoid repeating large excerpts. The first entry and its passage can therefore be read directly from one result.

To inspect any supporting entry in full, read `file.path` at `file.revision` and its `file_lines` (or the working file when `file.source` is `worktree`). For a resolved ADR, run `adrai show <adr> --view exploded --at <as_of> --json`. For an unresolved conflict, request `GET /api/v1/adrs/<adr>?view=exploded&at=<as_of>` with API authentication: the web inspection route retains every decision head, while CLI `show` reports a semantic conflict. Select the `operations[].items[]` decision whose `item` equals the passage's `candidate_record`; `adr_chunk` identifies its section and lines.

#### Migrating from `adrai/relevant/v1`

The result and retrieval metadata fields remain in place. The evidence layout changed, so clients should branch on `schema`. For each v1 `evidence[n]`, v2 `evidence[n].passage` points to `results[i].passages[index]`; that passage holds the old `adr_chunk`, `adr_section`, `candidate_record`, and `adr_excerpt`. The v1 `adr_candidate` is the prefix of `adr_chunk` before `/section/`, and v1 evidence `score` equals `semantic_score`. V2 includes `file_excerpt` only on the highest-ranked entry; use `file_lines` and the revision-bound file for later entries. The v1 projection is a historical contract and is no longer emitted by these current relevance endpoints.

## Indexing and conflicts

Only strictly valid reduced semantics reach search tables. Obsolete ADRs remain indexed but hidden by default. Valid decision multiheads are scored independently and grouped while retaining the matched head as evidence; malformed graphs are not indexed.

See [Cache](CACHE.md) for index reuse and [Conflict handling](CONFLICTS.md) for semantic-state behavior.
