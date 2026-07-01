# Clustering phase — plan

> **Status: UNBLOCKED (2026-06-30).** The "shipping first" precondition is **done** — pixi-isation
> shipped, `napari/.venv` deleted, `python_bin_path()` config-driven, browser launch (no Tauri),
> and `leidenalg`/`scanpy`/`anndata` are already in `pixi.toml`. The old *Env cleanup* section
> below is therefore satisfied. This file is the living plan; the **Decisions** section is current.
> Repo: `~/cc-workspace/cecelia/cecelia-pineapple`. Test data: project NRUBxU / set jFWePN
> (KDIeEm/CHWgkH/hVNx8o tracked live images; A/B/C segs with `track_id`).
>
> **Re-derivation pass 2026-06-30 (D's answers locked):** set-level clustering; drop refPops;
> redesign keepPops as elegant sub-clustering; track features = celltrackR + HMM + transitions.
> See **Decisions 6–9** below. Key enabler found in the new stack: **set-scope tasks already
> exist** — `behaviour/hmm_states.jl` + `hmm_transitions.jl` take `Vector{CciaImage}`, pool the
> set via `pop_df(imgs, uids, "live", pops; granularity=:cell)`, and write back per image. That is
> the template both cluster runners copy.

## Current state & remaining (2026-07-01) — the forward-working summary

Everything in **Steps 1–6 is built + merged** and the cluster canvas is fully usable: run
`clustPops`/`clustTracks`, view **UMAP + heatmap + HMM behaviour plots** (states 100%-stacked bar,
transitions from→to grid — cluster-number OR pop mode), tick clusters into populations in the shared
floating pop-manager (set-wide mirror write, `partOf` run-membership guard), and every canvas plot
carries **duplicate + export** (PNG/SVG/CSV). Plot styling (`VisProps`) is generalised: shared
`PlotOptions` + `PopulationPanelShell` (chrome) + `plots/export.ts`/`overlays.ts`, reused by the
summary `SeriesPicker` and the gating/cluster `PopulationManager` — so the future universal analysis
canvas is mostly assembly.

**Remaining for clustering v1:**
1. **Sub-clustering (Decision 8)** — the one real feature gap. A run restricted to a parent `clust`
   pop (in-process `cells_in_pop`, btrack's gated path) into a NEW `clusters.{suffix}` column, expressed
   as a child in the clust pop-tree (no overwrite — `keepPops`/`merge_new_adata` NOT ported). Needs:
   `popsToCluster` accepting a cluster pop as parent + the membership restriction in both runners +
   pop-tree parent/child UX. **Design first, then build.**
2. **`umap-learn` explicit pin** in `pixi.toml` (transitive via scanpy today; `sc.tl.umap` needs it) —
   quick, but relock.
3. **jFWePN e2e spot-check** — tick→membership set-wide, trackclust expand-to-cells (backend verified
   this session via the `_expand_tracks_to_cells` fix), napari colour-by-`clusters.{suffix}`.

**Done since (2026-07-01):** Canvas export/theme cleanup (**#00061**) — DPR-aware export scale,
UMAP now composites into its PNG (ancestor-background clear), UMAP + heatmap honour the dark-theme
knob, gate-plot x-axis label no longer clipped by the footer. Installer 404 fix (prerelease-aware
bootstrap) also merged.

**Deferred beyond v1 (correctly parked):** PAGA, batch correction (ComBat/Harmony), GPU/RAPIDS +
CPU↔GPU ARI, pooled set-wide pop counts in the manager.

## Progress

- **Step 1 — engine: DONE.** `app/py/utils/clustering_utils.py` `find_populations` (transform →
  normalise → neighbours(use_rep=X) → Leiden(leidenalg) → optional UMAP/PAGA), backend auto-detect
  (GPU parked). Shared per-segment write-back `split_back_and_write` (clusters.{suffix} int codes +
  obsm['X_umap.{suffix}']) — used by BOTH runners (the one canonical writer; CLAUDE.md rule).
- **Step 2 — both set-scope tasks: DONE.** Tasks named **`clustPops`** (cells) and **`clustTracks`**
  (tracks) — two separate module pages, like gating/tracking (not `clust.cluster_cells/_tracks`).
  - `clustPops.cluster` — `app/src/tasks/clustPops/cluster.{jl,json}` + `cluster_run.py`. Julia
    resolves membership (`pop_df` flow, `:cell`) + segment→labelProps paths; Python READS the chosen
    cell columns by path, pools, clusters, writes back to each `{vn}.h5ad`.
  - `clustTracks.cluster` — `app/src/tasks/clustTracks/cluster.{jl,json}` + `cluster_run.py`. Julia
    builds the per-track feature matrix via `track_props` (`pop_df` `track`, `:track`) — motility
    (stored) ⊕ on-read aggregates (HMM-state/transition freqs, cell-measure means) — and passes it
    **inline** to Python (the aggregates are compute-on-read, so only Julia can produce them; cf.
    clustPops reading stored cell columns by path — divergence forced by the data model, not
    duplication). Python clusters + writes back to each `{vn}__tracks.h5ad` (label == track_id) → the
    column flows through `track_props` and is gateable as `trackclust`.
  - **Decided (D, this session):** do NOT persist HMM-per-track — on-read aggregation is a columnar
    read + one groupby, negligible next to Leiden/UMAP, and persisting would couple track_measures↔HMM
    (HMM runs later) and go stale. track_props stays compute-on-read.
  - `clusters.*` now pinned categorical in `_is_categorical_col` (track_props.jl) regardless of level
    count — a high-res run can exceed the 20-int-level cap but codes are never a count. Applied at both
    callers (track_props loop + plot_data mtype).
  - Drive-by fix: clustPops JSON `fun_name` was `clust.cluster_cells` (≠ registry `clustPops.cluster`)
    → a GUI launch would not resolve. Corrected to `clustPops.cluster`.
  - Tests: dispatch + param-validation testsets for both tasks; `_is_categorical_col` name-rule cases.
- **Step 3 — pop layer: DONE.** `clust` + `trackclust` pop_types registered. `gating_path` routes each
  to its own sidecar (`{vn}__clust.json` / `{vn}__trackclust.json`, via `POP_MAP_SUFFIX`) — no collision
  with flow's `{vn}.json`. `pop_df`: `clust` rides the default cell path (loads the clust map; membership
  = filter on `clusters.{suffix}` cell obs); `trackclust` routes through a parametrised
  `_pop_df_track_gating` (data source `track_props`, `:track` + expand-to-cells). Cluster pops are
  **stored filter pops** (`filter_fun="in"` over `clusters.{suffix}`), created via `pop/add`'s `filter`
  dict; **`pop/update` now also mutates `filter`** (the tick-cluster-into-pop UX). API helpers
  (`_live_map`/`_plot_xy_raw`/`_track_fetch`) treat `trackclust` as track-grained; new `_track_free_cols`
  keeps `clusters.*`/lineage from being mistaken for a cell aggregate. No dedicated endpoint (Decision 10
  holds). Tests: gating_path routing + filter membership + storage round-trip (headless).
- **Step 4a — UMAP endpoint: DONE.** `GET /api/plots/umap?popType=clust|trackclust&suffix&pop?` →
  binary `Float32 [x,y,code,…]` (obsm `X_umap.{suffix}` + `clusters.{suffix}`; clust=cell table,
  trackclust=track table; optional pop subset). `obsm`/`obsm_keys` now exported for the api layer.
  Heatmap reuses `_matrix_agg` (no new backend). **Backend is now feature-complete for clustering.**
- **Step 5 (frontend) — module-page SHELLS up.** `ClusterCellsModule.vue` / `ClusterTracksModule.vue`
  (mirror BehaviourModule: `ModuleLayout` + `TaskRunner` for `clustPops`/`clustTracks`, set-scope).
  Routes `/clust-cells` `/clust-tracks` in `main.ts`; nav items under Analysis in `AppSidebar.vue`
  (replaced the disabled UMAP placeholder). Below-table = a placeholder where heatmap/UMAP/pop-manager
  go. vue-tsc clean. So the pages exist + can RUN clustering from the GUI; rendering is next.
- **Step 4b — UMAP chart: DONE.** `ScatterGL` extended with a `category` colour mode (categories +
  palette props, no second regl wrapper). `components/cluster/ClusterUmapPanel.vue` fetches
  `/api/plots/umap` per selected image, concatenates (shared UMAP space + cluster numbering across the
  set), colours by cluster with a legend (id + count); suffix + point-size persist via the canvas
  shared bag. Wired into both module pages' below-table (popType clust/trackclust). vue-tsc clean.
- **Step 4b/5 — UMAP + heatmap on the CANVAS framework (reworked per D): DONE.** D corrected the first
  cut (a bespoke embedded UMAP panel) → cluster plots must use the SAME canvas shell as gating
  (`useCanvasPanels` + `CanvasPanel`; docs/UI.md). Built `modules/cluster/ClusterPlots.vue` (canvas host,
  mirrors GatingPlots, key `clust:${popType}`) + `ClusterPlotPanel.vue` (a CanvasPanel toggling UMAP |
  heatmap). UMAP = `ScatterGL` category mode (legend); heatmap = `PlotChart` matrix/profile via
  `POST /api/plot_data` (category=`clusters.{suffix}`, user-picked features). Feature candidates from
  `/api/gating/channels` (markers for cells, motility for tracks — not every column, per D). Set-scope
  pooling (shared UMAP space; heatmap via setUid). Bespoke `ClusterUmapPanel` deleted. Sidebar icon fixed
  (`pi-chart-bubble` doesn't exist → `pi-palette`). Removed `refChannel` from clustPops options (D). vue-tsc + 616 Julia tests green.
  - **Lesson (saved to memory [[feedback_use_existing_framework]]):** read docs/UI.md before building frontend;
    use the canonical framework, don't fork. The canvas pattern was documented; I skipped the doc.
- **Steps 5–6 — DONE (sessions 5–6):** shared floating pop-manager with cluster-ID tick chips
  (set-wide mirror write, `partOf` run-membership + "select clustered images"); napari per-pop-type
  toggles (clust points / trackclust ribbons); HMM behaviour plots (states / transitions) on the
  canvas; styling generalised (`PlotOptions` + `PopulationPanelShell` + `plots/export.ts`/`overlays.ts`),
  duplicate/export on every cluster + gate plot. **See "Current state & remaining (2026-07-01)" at the
  top for the live status + the short remaining list** (sub-clustering, #00061 cleanup, umap-learn pin,
  e2e spot-check).

## Goal

Port the old R/Shiny clustering features (`leidenClustering.R` for cells,
`clusterTracks.R` for tracks) to the Julia + Python + Vue stack. The user defines
**populations from clusters** (heatmap + UMAP, tick clusters into a population), visualised
on napari — exactly the gating UX, but membership comes from a `clusters` column instead of
a hand-drawn gate.

## Key findings (research, done)

- **Both old modules call the same scanpy routine** — `find_populations()` in
  `inst/py/scanpy_utils.py`: normalise/transform → `sc.pp.neighbors(use_rep='X')` →
  `sc.tl.leiden(key_added='clusters')` → optional UMAP/PAGA. The *only* real differences
  are the **feature matrix** (cell intensities+measures vs per-track aggregates) and **where
  results are saved**.
- **Old save split (the thing to unify):** cell clustering wrote a *separate*
  `{vn}.clust.h5ad` + a metadata pointer; track clustering wrote an intermediate `.sc.h5ad`
  then **merged** a `live.cell.track.clusters.{suffix}` column back into the original
  labelProps. That split only existed because R's labelProps made in-place cell-column
  merges awkward.
- **New stack is already cluster-ready:** `Population` has
  `filter_measure`/`filter_fun`/`filter_values`; `pop_df` already handles
  `granularity=:track` (reads `{vn}__tracks.h5ad`, expands a selected track back to its
  cells); `_matrix_agg` profile mode + napari `show_populations`/`colour_labels` work off any
  obs column. **A cluster population is just a filter-on-a-column population.**

## Decisions (locked with D)

1. **Unified save mechanism** (replaces the old two-mechanism split): a run writes ONE obs
   column `clusters.{suffix}` (+ `obsm['X_umap']`) into the **granularity-appropriate
   existing h5ad** — cells → `labelProps/{vn}.h5ad`; tracks → `labelProps/{vn}__tracks.h5ad`.
   No separate `.clust.h5ad`, no pointer juggling. `{suffix}` (default `default`) keeps
   multiple resolutions/re-clusterings side by side. (btrack already merges columns in place
   — same pattern.)

2. **Two dedicated cluster pop_types** (D chose this, mirroring the existing flow/track
   split):
   - `clust` → cell clusters, sidecar `{vn}__clust.json`, cell granularity, filters
     `clusters.{suffix}` in `{vn}.h5ad`.
   - `trackclust` → track clusters, sidecar `{vn}__trackclust.json`, track granularity
     (expand-to-cells), filters `clusters.{suffix}` in the **shared** `{vn}__tracks.h5ad`
     (same data file as track gates; only the pop-definition sidecar differs).
   - Both are filter-based pops (`filter_measure="clusters.{suffix}"`, `filter_fun="in"`,
     `filter_values=[ids]`). Only new pop-engine surface: a `pop/set-filter` endpoint +
     wiring both new pop_types' granularity into `pop_df`/napari/plot paths.

3. **Two SEPARATE canvas plot types** — heatmap and UMAP as distinct panels, **never
   overlaid** (explicit D instruction).
   - **Heatmap**: reuse `_matrix_agg` profile, `category="clusters.{suffix}"` → cluster ×
     measure signature (z-scorable). No new renderer.
   - **Boxplot**: already exists (series = cluster).
   - **UMAP**: the one new renderer — `GET /api/plots/umap` serves `obsm['X_umap']` +
     `clusters` as a binary point buffer → regl-scatterplot (same WebGL path as gating),
     colour-by cluster / population / marker / attribute (ports old `clustUMAP*` plotly).

4. **GPU is auto-detected, NOT a task param.** Hard rule `CLAUDE.md:125`: *"Do not add a
   `useGPU` param to task JSON or Julia handlers."* The engine auto-detects RAPIDS+CUDA and
   silently falls back to CPU. No `backend` knob in the task form.

5. **GPU/RAPIDS deferred to the env/shipping phase (CUDA-only).** RAPIDS needs CUDA →
   Linux/Windows+NVIDIA only; macOS/CPU can't use it. CPU path (scanpy + `leidenalg`) is the
   cross-platform default and ships now. GPU becomes an optional CUDA-only feature of the
   locked env (pixi `gpu` feature — already a commented `[feature.gpu]` stub in `pixi.toml`);
   the **side-by-side ARI check** (CPU vs GPU, gate auto-switch on ARI ≥ 0.95) runs then, on a
   CUDA box. Engine is built GPU-ready regardless.

6. **Clustering is per-SET, not per-image (D, 2026-06-30).** "The gating approach was per image;
   clustering is per set." Both runners are **set-scope tasks** (`_run_task(::Task,
   imgs::Vector{CciaImage}, params)`) — pool the selected pops across all set images, cluster
   **once** jointly, then split `clusters.{suffix}` back per image. This is exactly how the
   existing `behaviour/hmm_states.jl` + `hmm_transitions.jl` already work (pool via
   `pop_df(imgs, uids, "live", pops; granularity=:cell)`, write each image's labelProps in a
   per-uID loop). **Copy that structure** — it also gives cross-image-comparable cluster IDs for
   free (the scientific point). No single-image mode. (Supersedes the per-image framing implied
   by old Step 2.)

7. **Drop `refPops` (reference-population normalisation).** D: "the reason for refpops was
   normalisation, which didn't really work." Cut it from the param set and the engine. Ordinary
   per-channel/percentile normalisation + the optional `refChannel` divide stay.

8. **`keepPops` → redesign as elegant sub-clustering (don't port `merge_new_adata`).** D: keepPops
   was "sequential clustering — a kind of gating hierarchy with clustering… it sort of worked to
   subcluster clusters, but it was clunky and overwrote the previous clusters." The new design uses
   the machinery we already have instead of the overwrite-merge crutch:
   - Sub-clustering = run clustering **restricted to a parent cluster population** into a NEW
     `clusters.{suffix}` column (the suffix mechanism already keeps columns side by side — no
     overwrite, no cluster-number bumping).
   - Express the hierarchy in the **clust pop tree**: a child `clust` pop whose parent is the
     cluster being subdivided, filtering the new `clusters.{suffix2}` column. Parent membership
     is enforced by the in-process `cells_in_pop` restriction at run time (same path btrack uses
     for gated tracking).
   - Net: hierarchical/iterative clustering falls out of (filter-restricted run) × (new suffix
     column) × (pop-tree parent/child). Detailed design TBD at implementation; the old
     `merge_new_adata` is explicitly NOT ported.

9. **Track-clustering feature matrix = celltrackR + HMM + transitions (D: "check tracking again").**
   The new tracking stack already produces everything the matrix needs; nothing new to compute
   from scratch:
   - **Per-track celltrackR measures** are materialised in `{vn}__tracks.h5ad` X/var by
     `tracking/track_measures.jl` (`live.track.speed/duration/trackLength/displacement/
     straightness/displacementRatio/outreachRatio/meanTurningAngle/overallAngle/asphericity`),
     plus lineage obs (`track_parent/root/state/generation`). These are read directly.
   - **HMM state** (`live.cell.hmm.state.*`, numeric codes) and **transitions**
     (`live.cell.hmm.transitions.*`, categorical) live in the **cell** obs → aggregate to per-track
     **state/transition frequencies** via the on-read `track_props()` aggregation path (the same
     on-read aggregation that already serves selected cell measures per track). NOT pre-stored.
   - So `cluster_tracks` builds its matrix = `{vn}__tracks.h5ad` motility columns ⊕ per-track
     aggregates (HMM-state fractions, transition fractions, and any selected cell-measure
     means/quantiles). No `useGPU`, no refPops.

10. **`pop/set-filter` endpoint is probably unnecessary.** `api_gating_pop_add`
    (`gating_api.jl:357`) already accepts a `filter` dict (`measure`/`fun`/`values`/`default_all`),
    and `pop/update` already exists. So a cluster pop is created via `pop/add` with a filter, and
    the tick-box UI toggles cluster IDs by `pop/update`-ing `filter_values`. Don't add a new
    endpoint unless a gap shows up in implementation. (Simplifies old Step 3.)

## Plan (when we resume)

### Step 1 — Shared engine (Python)
- `app/py/utils/clustering_utils.py`: one `find_populations(adata, ..., backend="auto",
  random_state=0)` — normalise/transform → neighbors → leiden(`key_added='clusters'`) →
  optional UMAP/PAGA. Granularity-blind: takes feature matrix `X`, returns `clusters` +
  `X_umap`.
  - `_gpu_available()`: try import `rapids_singlecell` + `cupy`; check
    `cupy.cuda.runtime.getDeviceCount() > 0`; cached. `backend="auto"` → gpu if available
    else cpu; graceful fallback on import/CUDA failure.
  - CPU: `sc.pp.neighbors` → `sc.tl.leiden(random_state=...)` → `sc.tl.umap(random_state=...)`.
  - GPU: `rsc.get.anndata_to_GPU` → `rsc.pp.neighbors` → `rsc.tl.leiden` → `rsc.tl.umap` →
    `rsc.get.anndata_to_CPU`.
  - Logs which backend ran. Keep the interface clean so a GPU backend swaps in behind the
    same call.

### Step 2 — Two SET-SCOPE runners + Julia tasks (snake_case per `feedback_julia_naming`)
**Both tasks are set-scope** — Julia signature `_run_task(::Task, imgs::Vector{CciaImage},
params)`, copying `behaviour/hmm_states.jl`: pool the set with `pop_df(imgs, uids, "live"/pop_type,
pops; granularity=…)`, cluster once, then loop per uID writing each image's labelProps. Cluster IDs
are thus comparable across the set.
- `app/py/tasks/clust/cluster_cells_run.py` → pooled matrix across the set's `{vn}.h5ad` (channel
  intensities + object measures, optional ref-channel divide) → engine → split + write
  `clusters.{suffix}` + `obsm['X_umap']` back into **each image's** `{vn}.h5ad`.
- `app/py/tasks/clust/cluster_tracks_run.py` → pooled per-track matrix across the set: motility
  from `{vn}__tracks.h5ad` ⊕ per-track HMM-state/transition frequencies + selected cell-measure
  aggregates (Decision 9) → same engine → write `clusters.{suffix}` + `X_umap` back into each
  image's `{vn}__tracks.h5ad`.
- Julia: `app/src/tasks/clust/cluster_cells.{jl,json}` + `cluster_tracks.{jl,json}`,
  fun_names `clust.cluster_cells` / `clust.cluster_tracks`; register in `task_registry.jl`,
  include in `Cecelia.jl`. Params: `resolution`, `mergeUmap`, normalise group
  (`normaliseAxis`/`normaliseToMedian`/`normalisePercentile`…), `transformation`/`logBase`,
  `clusterChannels`+`objectMeasures` (cells) / `trackMeasures`+HMM/transition+cell aggregates
  (tracks), `popsToCluster`, `valueNameSuffix`. **Dropped vs old:** `refPops` (Decision 7),
  `keepPops`/`reLeiden` (replaced by the sub-clustering design, Decision 8), `useGPU` (auto-detect,
  hard rule). Sub-clustering = run restricted to a parent `clust` pop via in-process
  `cells_in_pop` (btrack's gated path) into a fresh suffix column (Decision 8).

### Step 3 — Pop layer
- Register pop_types `clust` (cell granularity, `{vn}__clust.json`) and `trackclust` (track
  granularity, `{vn}__trackclust.json`) in the population manager + `pop_df` granularity
  routing + napari/plot paths.
- **No new endpoint needed** (Decision 10): `pop/add` already accepts a `filter` dict and
  `pop/update` already mutates it. Cluster pop = `pop/add` with `filter_measure="clusters.{suffix}",
  filter_fun="in", filter_values=[ids]`; the tick-box UI toggles IDs via `pop/update`. Only add a
  dedicated endpoint if a real gap surfaces.

### Step 4 — UMAP plot endpoint + chart type
- `GET /api/plots/umap` → binary buffer of `obsm['X_umap']` + `clusters` (+ optional colour
  column). Frontend `umap` chart type via regl-scatterplot; colour-by cluster/pop/marker/attr.
- Heatmap + boxplot need no new backend (reuse `_matrix_agg` / existing chart types).

### Step 5 — Two module pages + cluster pop-manager UI
- `frontend/src/modules/ClusterCellsModule.vue` + `ClusterTracksModule.vue` (copy
  `GatingModule` layout: `ModuleLayout` + below-table plots). Sidebar entries + routes
  (after Behaviour, or grouped). Old `isTrack` toggle becomes *which page you're on*.
- Pop manager: heatmap + UMAP panels (separate); add a population; **tick boxes assign
  cluster IDs to it** (toggle `filter_values`). No per-bar boxes under the heatmap (D's call).
  Reuse gating store's pop tree / colour / show / count plumbing.

### Step 6 — napari + docs
- `show_populations` / `colour_labels` work as-is (cluster pops = label-id lists; colour-by
  `clusters.{suffix}`).
- Docs: MODULES (two tasks + popSelection), POPULATION (clust/trackclust pop_types +
  `clusters.{suffix}`), DATAMODEL (the obs column + `obsm['X_umap']`), PLOTS (umap chart
  type), UI (two module pages), TODO.

### Step 7 — Verify (on jFWePN)
- Run `clust.cluster_cells` on a seg → assert `clusters.default` + `X_umap` land in
  `{vn}.h5ad`. Run `clust.cluster_tracks` → assert they land in `{vn}__tracks.h5ad`.
- Define a cluster pop via `pop/add` (filter spec, Decision 10) → assert `pop_df` membership;
  trackclust expand-to-cells pulls member cells. Run on the **set** (all 3 images) and assert
  cluster IDs are shared across images.
- Param-validation tests in `app/test/runtests.jl`.
- UMAP + heatmap render; napari colour-by cluster works.

## TODO / deferred
- **RAPIDS GPU env** (CUDA-only; pixi `gpu` feature / uv extra; platform-gated). Build during
  the env/shipping phase. macOS/CPU stay on `leidenalg`.
- **CPU↔GPU side-by-side ARI validation** (gate auto-switch on ARI ≥ 0.95; cluster count +
  size distribution). Runs on a CUDA box once RAPIDS is installed. GPU labels won't be
  bit-identical to CPU leidenalg — both stochastic, different impls; concordance is the bar.
- **PAGA** layout option (`usePaga`/`pagaThreshold`) — port if needed (old default off).
- **Batch correction** (ComBat/Harmony from old `correctBatch`) — stays in Python (scanpy
  `sc.pp.combat` / harmonypy). Now that clustering is **always per-set** (Decision 6), pooling
  images with batch effects is the norm, so this is more likely wanted than the old note implied —
  but still optional; add when a real batch artefact shows up. Not in v1.
- **Sub-clustering design** (Decision 8) — the elegant replacement for `keepPops`/`reLeiden`. Mechanism
  sketched (restricted run × new suffix column × clust pop-tree parent/child); finalise the UX +
  the `cells_in_pop` restriction wiring at implementation.
- **`refPops`** — dropped (Decision 7); revisit only if a working reference-normalisation idea emerges.

## Env cleanup — ✅ DONE (was the "shipping first" gate; no longer blocks clustering)
This whole section shipped in the distribution phase and is recorded in `docs/SHIPPING.md`:
- Env relocated out of `napari/` to the repo-root **`.pixi/`** (pixi is the dep manager);
  `napari/.venv` deleted, `napari/requirements.*` removed.
- `python_bin_path()` is config-driven and `app/src/napari.jl` now routes through it (no
  hardcoded venv path).
- `leidenalg`/`scanpy`/`anndata` already pinned in `pixi.toml`; RAPIDS is a commented
  `[feature.gpu]` stub (CUDA-only, parked — Decision 5).
- Launch is browser-based via `pixi run app` (no Tauri/Electron); CI + release pipeline exist.
- ⚠️ At implementation, pin `umap-learn` explicitly in `pixi.toml` (scanpy pulls it transitively,
  but `sc.tl.umap` needs it — make it explicit).
