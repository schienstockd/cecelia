# Parked plan — Cluster page: pool across all co-clustered segmentations

Status snapshot / design lock. See `docs/todo/README.md` for the convention. Branch:
`feat/phenotype-population-summary` (rides with the population-summary work).

## Problem

Track/cell clustering (`clustTracks.cluster` / `clustPops.cluster`) runs **jointly** across the
selected populations, which may span several **segmentations** (value_names) — e.g. `B/qc/_tracked`
+ `T/qc/_tracked`. The engine writes ONE shared `clusters.{suffix}` column (and a shared
`X_umap.{suffix}`) back into **each** segment's per-(uID, value_name) table, and a
`{props}.clustfeatures.json` sidecar per segment (keyed by suffix → `{features, partOf}`).

But the cluster **page** (and board) is single-segmentation:
- UMAP (`api_plots_umap`) and heatmap read ONE value_name (`_resolve_vn` → `_active`, e.g. `B`).
- The named cluster pops (`__trackclust.json` → Scanning/Directed/Meandering) are authored under ONE
  value_name (whichever was active when named), so the picker + pop-summary only ever show B.
- Net: on `4kS67f`/`3w4IY5` the user sees **B only** everywhere on the cluster page, even though
  T was part of the same run and its data sits in `T__tracks.h5ad` in the same cluster space.

Behaviour pages don't have this: their pops are stored per value_name (both B & T exist), and there
is no single-value_name UMAP/heatmap.

## Decision (user)

**Pool everything across ALL segmentations used to create the clustering.** UMAP shows all segments'
points together (in the shared embedding), heatmap aggregates across them, and the named cluster pops
apply to every co-clustered segmentation (picker + summary). "All at the same time."

## Key concept — co-clustered value_names

For an (image, suffix, granularity): the value_names whose per-vn props table carries
`clusters.{suffix}` — equivalently, whose `{props}.clustfeatures.json` has the `suffix` key. This is
exactly the set of segmentations that went into the run (the engine writes the column + sidecar to
every input segment). Source of truth = the clustfeatures sidecar (cheap JSON, no h5ad read).

## Build sequence

### Stage 1 — backend foundation (package, Revise-tracked, headless-testable)  ← START HERE
- `co_clustered_value_names(img, suffix; granularity) -> Vector{String}` in `population_manager.jl`
  (or a small clustering util): scan the image's value_names; keep those whose clustfeatures sidecar
  (track table for `:track`, cell table for `:cell`) has `suffix`. Stable order; fallback to the
  active vn when no sidecar (pre-clustfeatures runs). TEST with the `4kS67f` shape (or synthetic
  sidecars) → returns `["B","T"]`.

### Stage 2 — cluster-pop auto-share (fixes the pop summary split, the original ask)
- `load_pop_map(img; value_name, pop_type)` (CciaImage method ONLY — not the task_dir base, which
  save uses): for cluster pop_types (`clust`/`trackclust`) when the requested vn has NO own sidecar,
  BORROW a co-clustered sibling's map, relabeled to the requested vn (set `m.value_name` + each
  `pop.value_name`), so BOTH the picker (`plot_population_groups`) and membership (`pop_df`) resolve
  the named cluster pops over the requested vn's own table. Guard: only borrow when the requested vn
  shares the sibling map's referenced `clusters.{suffix}` (from each pop's `filter_measure`) per
  Stage-1. Own sidecar always wins; editing+saving under T materialises a real T sidecar (copy
  semantics — acceptable). TEST: borrow round-trips; membership over T's table.

### Stage 3 — UMAP pooling (`api_plots_umap`)
- Pool `obsm[X_umap.suffix]` + `clusters.suffix` across `co_clustered_value_names`. Binary becomes
  `[x, y, code, segIdx]` per point (segIdx into a returned value_name list — via an `X-Value-Names`
  response header, keeping the body pure binary). Pop subset resolves membership per vn. Frontend
  (regl-scatterplot UMAP view) reads segIdx → optional tint/split by segmentation; default colours by
  cluster code as today. Drop NaN-embedding rows as now.

### Stage 4 — heatmap pooling
- The cluster heatmap aggregates features per cluster from one vn's table → pool rows across
  co-clustered vns before aggregation (find the heatmap data path; likely `plot_summary_data`
  matrix/profile over the cluster column, or a dedicated endpoint). Keep the shared cluster-id
  universe (`_cluster_ids` already pools across member IMAGES; extend to VALUE_NAMES via Stage-1).

### Stage 5 — frontend cluster page
- Consume pooled UMAP/heatmap; surface "segmentations: B, T" info; no per-vn selector needed.
  Pop picker/manager keeps editing under the primary vn (auto-share covers the rest). Optional:
  colour-by-segmentation toggle on the UMAP.

## Notes / invariants
- `api/src/*.jl` (umap/heatmap endpoints) are NOT Revise-tracked → server restart to test Stages 3-4.
- `app/src/` (Stages 1-2) IS Revise-tracked; `pixi run test-pkg` must stay green (868 base + new).
- Don't touch the SAVE path — borrow is READ-side only (CciaImage `load_pop_map`), so a user edit
  still writes a normal per-vn sidecar.
- Pop colours are stable per (valueName, path); the frontend colour map already accumulates
  (see population-summary work) so pooled/borrowed pops render in their manager colours.
