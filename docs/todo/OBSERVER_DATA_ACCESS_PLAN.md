# Observer Data Access Plan — give the AI observer the real analysis (summaries + lineage)

**Status:** parked (scoped, not started). Owner arc: the read-only MCP observer.
**Why:** Today the observer (`mcp/cecelia_mcp/`) sees only QC metrics, run logs, image meta and the
lab log. It can NOT read the actual analysis — populations/gating, saved track measures, HMM,
cluster definitions/props, or the analysis boards — so every session the user has to re-explain the
whole workflow ("this image was denoised, then cellpose-segmented, then gated on these channels,
then tracked, then HMM-classified, then clustered, then plotted on this board"). External sessions
repeatedly hit this wall (reconstructing track counts from clustering inputs because
`tracking.track_measures` metrics weren't reachable).

**Goal:** give it as much *decision-useful* information as feasible, **summary-level only** — never
raw 100 MB tables — plus a synthesized **lineage** so it understands *how* the data was produced.

**Coverage — not just tracking.** Live/tracking projects are the current focus, but many users have
**static images** where the signal is **phenotype** — per-cell channel intensities + morphology — with
no tracks at all. Phenotype is a first-class summary target (see `get_measure_summary`), equal to
motility. **Spatial** / neighbourhood analysis is a future stage (not yet ported) — leave a slot for it,
don't build it.

## Locked decisions

1. **Lineage-first.** Build `get_analysis_lineage` (Slice A) before the richer per-stage numbers —
   it's the lightest data and directly removes the re-explain-every-session problem.
2. **Summaries/aggregates ONLY.** Counts, medians, quantiles, distributions, top-N. Never raw
   cell/track rows. Hard caps on every list (top-N pops, N states, etc.).
3. **Thin summary wrappers, with pushdown.** Compute each aggregate through the canonical
   `label_props`/`pop_df` *views* with column/row selection pushed down — do NOT materialize the full
   table for a set just to take a median (that would be overkill; see `docs/DATAMODEL.md`,
   `docs/POPULATION.md`). Reuse an existing cheap aggregate only where one already exists; otherwise a
   dedicated thin summary function in Julia (`app/src/`), Revise-friendly and unit-tested.
4. **Read-only.** Every new route is added to the MCP `ALLOWED_ROUTES` allow-list
   (`mcp/cecelia_mcp/client.py`); no writes, ever. New tools mirror the existing observer tool style
   in `mcp/cecelia_mcp/server.py`.
5. **Two layers, one arc.** Layer 1 = the lineage story; Layer 2 = per-stage summaries. Boards +
   chains feed both.
6. **New modules wire this layer.** When a module that produces analysable data lands, extending the
   observer summary layer (a thin route + MCP tool, or a new field on an existing one) is PART of the
   module — not an afterthought — so the observer never silently goes blind on a new data type. Codified
   as a checklist in [`docs/MODULES.md`](../MODULES.md) (the module-building doc).

## Layer 1 — lineage (Slice A)

`get_analysis_lineage(project_uid[, image_uid | set_uid])` → an ordered pipeline **synthesized** from
data we mostly already have:

- **run log** (`app/src/run_log.jl`, `[{fun, valueName, status, at}]` per image) → the task order +
  outcome + which value_name each step wrote.
- **value_name / suffix links** → which segmentation fed which tracking fed which cluster run
  (segmentation `value_name` → tracking `value_name` → cluster `suffix` via `co_clustered_value_names`).
- **gating** (`gating/{value_name}.json`) → "segmentation gated into CD3 / CD8 (on channels X, Y)".
- **boards** (`settings/analysisBoards.json`, `api_projects_boards`) → "plotted on board 'Behaviour'
  as a UMAP + a proportion chart".
- **chain templates** (`_chains_dir`, `app/src/tasks/chain.jl`) → which steps were *wired* in the
  whiteboard vs run ad-hoc.

Output (compact): per image `steps: [{stage, fun, valueName, status, at}]`, the segmentation→track→
cluster relationships, `plottedOn: [board names + plot kinds]`, `wiredInChains: [names]`, and a
set-level roll-up (the common pipeline + where images diverge, e.g. the excluded one — ties to
`#9`).

## Layer 2 — per-stage summaries (Slices B–E)

Each = a read-only `/api` route + an MCP tool returning aggregates (+ caps):

| Tool | Slice | Returns (summary only) |
|---|---|---|
| `get_populations` | B | pop tree: names, type (gate/clust/track), **gate defs** (channels/measures + gate kind), **membership counts** (n cells/tracks per pop) |
| `get_measure_summary` | C | per pop/image: median + quantiles + n. **Phenotype** (per-cell channel intensities + morphology) for static images, **and** track motility (speed / displacement / track-length) for live. Static projects have no tracks — phenotype is their whole signal. |
| `get_behaviour_summary` | D | HMM state distribution (fraction per state), n transitions / distinct |
| `get_cluster_summary` | D | n clusters, sizes, largestFrac, feature list, per label set |
| `get_analysis_boards` | E | what plots exist (type, data source, pops/measures plotted) |
| `get_chains` | E | whiteboard chain templates (which tasks wired) |
| *(spatial)* | *future* | neighbourhood / proximity summaries — **not yet ported**; leave the slot, don't build |

## Shared scaffold (reuse — do NOT re-duplicate per slice)

Every summary tool has the same shape (scope to image/set/project → per-image map → `{projectUid,
images}`), so that shape lives once at each layer; a new slice is just its **per-image builder + one
MCP-tool docstring**, not another copy of the plumbing:

- **Julia** — `observer_image_summary(proj, per_image_fn; image_uid, set_uid)` + `_observer_scope_images`
  + `_observer_image_header` in `app/src/ai/observer_summary.jl`. (`analysis_lineage` merges project-level
  chains/boards/rollup onto the base; `populations_summary` is just the scaffold.)
- **API** — `_observer_summary_route(req, build)` in `api/src/routes.jl`; each route is one line.
- **MCP client** — `CeceliaClient._analysis_summary(path, …)`; each method is one line. The MCP **tools**
  stay separate (their rich docstrings are the model-facing interface).

## Build slices (each shippable)

- **A — Lineage** (`get_analysis_lineage`). Highest value, lightest data. ✅ **DONE** — `app/src/ai/lineage.jl`
  (`analysis_lineage`), `GET /api/analysis/lineage`, MCP `get_analysis_lineage`, on the `ALLOWED_ROUTES`
  allow-list; pkg + api + mcp tests. Returns per-image `steps`/`segmentations`/`tracked`/`clusterRuns`/
  `gatedPops` + project `chains`/`boards` + a `rollup`. Boards are best-effort tab names only (the board
  JSON is opaque at the Julia layer — plot semantics stay Slice E).
- **B — Populations + gating** (`get_populations`). ✅ **DONE** — `app/src/ai/populations.jl`
  (`populations_summary`), `GET /api/analysis/populations`, MCP `get_populations`, allow-listed; pkg +
  api + mcp tests. Per-image pop tree + gate/filter DEFINITIONS (cheap sidecar read). Membership COUNTS
  are deferred to Slice C (they need computing gates over the full table — heavy, forbidden on an
  always-on read).
- **C — Measures** (`get_measure_summary`). ✅ **DONE** — `app/src/ai/measures.jl` (`measure_summary`),
  `GET /api/analysis/measures`, MCP `get_measure_summary`, allow-listed; pkg (fixture-gated + pure
  summary logic) + api + mcp tests, validated off-suite on real gated data. Per-population phenotype +
  motility (median/quantiles/mean/n) over the MEANINGFUL pops (gated pops when present, else the base
  `_tracked`/all-cells — so it works with or without gating). Channels named consistently (resolved from
  the label view, not pop_df's pop_type-dependent rename). Membership counts land here as each summary's `n`.
- **D — HMM + clusters** (`get_behaviour_summary`, `get_cluster_summary`). ✅ **DONE** —
  `app/src/ai/behaviour_clusters.jl`, `GET /api/analysis/{behaviour,clusters}`, MCP `get_behaviour_summary`/
  `get_cluster_summary`, allow-listed; pkg (pure distribution + fixture smoke) + api + mcp tests, validated
  off-suite. HMM state/transition distributions + per-run cluster sizes/largestFrac/features. Reads obs via
  the canonical `as_df` reader (pop_df's "labels" pop drops obs) — a shared `_category_distribution` returns
  the per-category breakdown `category_dist_metrics` (aggregates-only) can't.
- **E — Boards + chains** (`get_analysis_boards`, `get_chains`) — chains ✅ **DONE** (`app/src/ai/chains.jl`,
  `chains_summary`, `GET /api/analysis/chains`, MCP `get_chains`, allow-listed; pkg + api + mcp tests,
  validated on the real "3P" chain). Returns the wired templates (node DAG + task fns) + recent runs with
  node-outcome roll-ups — the intended pipeline + actual executions, which the run-log window can miss.
  **Boards SKIPPED**: the board JSON is opaque at the Julia layer (plot/pop/measure semantics live only in
  the frontend blob), so a board tool couldn't cheaply add more than lineage's tab names already do —
  deferred until there's a concrete need + a frontend-side reader.

## Grounding (where the data lives)

- run log → `app/src/run_log.jl`
- boards → `settings/analysisBoards.json` (`api_projects_boards`, `api/src/routes.jl`)
- chains → `_chains_dir` / `ChainTemplate` (`app/src/tasks/chain.jl`)
- populations / gating → `gating/{value_name}.json` + `pop_df` (`docs/POPULATION.md`)
- clusters / HMM → h5ad `obs` (`clusters.{suffix}`, HMM states) via `label_props`/`pop_df` views
  (`docs/DATAMODEL.md`)
- MCP → `mcp/cecelia_mcp/{server.py,client.py}` (read-only allow-list), `docs/ai-assist/OBSERVER.md`

## Non-goals

- No raw tables (payloads stay ~KB). No writes. No 100 MB dumps. No new heavy compute — summaries
  reuse the existing readers with pushdown.

## When built

Promote the durable parts into `docs/ai-assist/OBSERVER.md` (the tool catalogue) and delete this
plan.
