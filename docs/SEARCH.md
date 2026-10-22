# Search

ADRAI exposes two related commands:

- `search [QUERY]` searches compiled ADR content.
- `relevant FILE` ranks ADRs against a source file.

Both operate against a resolved Git revision. By default, each result is a single
line containing its ADR ID and title. `--view exploded` adds the summary below
each title, indented by two spaces. No matches produce empty stdout; successful
queries produce no stderr output. Result order and limits are unchanged by the
output format. Conflicted ADRs retain their conflict title rather than displaying
one matched head as a resolved decision.

JSON is opt-in with `--json`. The CLI schemas are `adrai/search-cli/v1` and
`adrai/relevant-cli/v1`. Each envelope contains only `schema`, `algorithm` and
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
| `vector` | `exact-summary+identifier+section-rerank+weighted-rrf` |
| `hybrid` | `three-fts+exact-summary+section-rerank+weighted-rrf` |

Relevance reports its retrieval strategy,
`exact-summary+passage-fts+bounded-section-rerank`. Score and algorithm metadata
appear only in JSON; plain and expanded text remain ID/title lists with optional
summaries.

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
- `vector`: built-in semantic and identifier embeddings.
- `hybrid`: weighted reciprocal-rank fusion of the active FTS and vector channels; this is the default.

Filters include file scope, repeatable domains, actor, inclusive timestamps, obsolete state, and immutable revision. Filters are applied before candidates are ranked. A blank query returns a deterministic filtered ADR listing.

Collapsed results group competing decision heads by ADR. `--view exploded` adds summaries; use `adrai show ADR_ID --view exploded` for operation-level detail. Use `--include-obsolete` to include obsolete ADRs.

## File relevance

```console
adrai relevant src/Storage.hs
adrai relevant src/Storage.hs --worktree
adrai relevant src/Storage.hs --at main~1 --json
```

Without `--worktree`, the file and ADR context come from the same resolved revision (`HEAD` by default). `--worktree` reads the current file safely from the worktree while keeping ADR context bound to `HEAD`; it cannot be combined with `--at`.

Input is limited to 4 MiB. Binary-like input, NUL bytes, and very short or uninformative text are rejected or produce no weak match. Informative text is chunked, shortlisted through exact, FTS, and vector channels, and reranked against stored ADR passages. File scope affects ranking but does not exclude an otherwise relevant ADR.

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
