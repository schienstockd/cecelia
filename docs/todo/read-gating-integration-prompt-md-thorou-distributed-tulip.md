# Plan: Population Manager + Gating (Cecelia Pineapple)

## Context

Cecelia is being rebuilt from the old R/Shiny stack (`old-R-shiny-version/`) to a new
stack (`cecelia-pineapple/`): **Julia** (task dispatch, **gating**, statistics, REST/WS
API) + **Vue 3 + TypeScript** (UI only) + **Python** (image I/O, Napari, PyTorch,
Cellpose). Layer boundaries are fixed by `docs/ARCHITECTURE.md:183-195`, including the
hard invariant **"Never add Rust or a fourth language"** — gating maps 1:1 from R to
Julia's scientific ecosystem; there is no perf argument for a native core. This plan is
**Julia-native** accordingly.

This phase builds the **population manager** and the **manual gating UI**. In the old
stack flow gating wrapped `flowWorkspace`/`flowCore`/`cytolib`: measurements were loaded
from H5AD into a `flowFrame` → `GatingSet`, gates were `flowCore` polygon/rectangle
objects, membership was *derived in memory* by `recompute()` — **never written into the
H5AD**. The `GatingSet` persisted to a flowWorkspace directory (a *copy* of the data with
transforms baked into its cytoframe backend — the data-duplication wart we are removing);
per-population label **CSVs** (`savePops`, `populationUtils.R:107`) were the only export
channel for Python/Napari, a crutch that existed *purely because Python could not read
flowWorkspace gates* (`inst/py/pop_utils.py` reads `{pop_type}.json` + `{pop_id}.csv`;
`napari_utils.py:show_pop_mapping` uses those label IDs to colour centroids). We are
**not** porting flowWorkspace/cytolib.

### How transforms work (the design that removes the old wart)

Gating plots use **logicle/biexponential** axes. Logicle is nonlinear, so a polygon
drawn on a logicle plot is a *curve* in raw space — point-in-polygon is not invariant
under the transform. Therefore, exactly as FlowJo/OMIQ/FlowKit do
(`docs/ARCHITECTURE.md:247`):

- **The raw H5AD data is the single source and is never transformed on disk.** No
  duplicated/transformed dataset (that copy was a cytolib artifact, not a requirement).
- **Display**: read raw, apply the transform *on the fly* to position points.
- **Gate**: drawn on the logicle display → vertices stored in **transformed coordinates**
  (a handful of numbers), with the transform spec attached to the gate.
- **Membership**: to test a raw cell, apply the same transform to its raw value, then
  point-in-polygon in transformed space. (FlowKit does this via `flowutils`' compiled
  logicle; we port the same Parks-2006 algorithm to Julia per `ARCHITECTURE.md:250`.)

So: raw data stays raw, transform is a pure on-the-fly function used for both display and
evaluation, gate lives in transformed space. This is the long-standing "display raw as
logicle, draw gate on logicle, apply to raw" model — achievable and standard.

### Decisions locked in
- **Julia-native gating engine** (no Rust): transforms (linear/log/asinh/**logicle**,
  Parks 2006) + gate eval + tree walk + density binning. Julia owns gating end-to-end.
- **Gates stored in transformed coordinates** in a **per-segmentation sidecar**, not in
  `ccid.json` (keep that lean). One `gating/{value_name}.json` per segmentation,
  name-matched to its labels + labelProps, e.g. for image KDIeEm segmentation "B":
  `labels/B.zarr` · `labelProps/B.h5ad` · `gating/B.json`. The file holds the population
  tree `{name, gate, transform, children:[...]}`. Plain JSON, readable by Julia and
  Python. (Overrides the old `ARCHITECTURE.md:248` ccid.json plan — updated in Step 9.)
- **Read = Julia (`HDF5.jl`) / Write = Python** (PythonCall→`anndata.write_h5ad`;
  `ARCHITECTURE.md:214`). The fluent reader is `app/src/label_props.jl`, mirroring Python
  `LabelPropsView`; HDF5 opens inside `as_df` in a `do` block (no leaked handles).
- **Rendering**: `regl-scatterplot` (WebGL) for millions of points + `canvas2D` overlay
  for gate drawing; density (2D-histogram) fallback for very large N. **No Plotly** for
  gating (Plotly stays only for summary charts like `IntensityHistogram.vue`).
- **Frontend is transform-free**: the Julia API serves *already-transformed* `Float32`
  coordinates + axis-tick positions (Julia owns the transform per the layer boundary).
  The browser plots linearly in display space, draws/returns gate vertices in that space,
  and does trivial JS point-in-polygon only for live during-drag preview (authoritative
  counts come from the API).
- **No CSV crutch, and no Python gate engine.** Julia is the sole evaluator (logicle lives
  in exactly one place; no `flowutils` dep). Python gets membership *from Julia*, never
  re-deriving it: **Napari** POSTs gate draws to `/api/napari/event` and gets
  membership/colours back (`ARCHITECTURE.md:249`); **headless batch tasks** (launched by
  Julia's task system) receive the label-IDs they need from Julia as a task param / temp
  handoff, or call the local API. `pop_utils.py` may *read* the gating JSON for metadata
  but does not evaluate gates. (Offline-only Python with no Julia running is the lone gap —
  defer `juliacall`/thin-fallback unless such a flow exists.) Pops are **never** written
  to H5AD.
- **Build scope**: build **`flow` end-to-end** (engine + API + Vue UI + Napari link).
  Design the population manager, reader, `pop_dt()`, and storage **generically for all
  three types** (`flow`/`live`/`clust`) and let `pop_dt()` read all three — but only wire
  `flow` to the gating UI. `live`/`clust` creation UIs are later phases.
- **Recompute runs synchronously in-process** in the Julia API handler (no task pool /
  subprocess). Measurements cached per image.

### H5AD reality (audited `projects/NRUBxU/1/KDIeEm/labelProps/B.h5ad`)
`/X` dense `float32` (1377×27); `/obs/_index`, `/var/_index` variable-length UTF-8
**string-array**s; `/obsm/spatial`(×3), `/obsm/temporal`(×1) dense float32; `/uns` has a
**scalar string** (`intensity_measure`) + string-arrays (`spatial_cols`,`temporal_cols`).
No categorical/sparse in this file, but future `live`/`clust` H5ADs can (cluster labels →
`categorical`=`categories`+`codes`; `csr/csc` sparse). The reader must **dispatch on the
`encoding-type` attribute**: `array`, `string`/`string-array` (UTF-8 decode; scalar at
`()`), `dataframe` (follow `_index` group attr), `categorical`, `csr/csc_matrix`. Watch
HDF5.jl row/col order vs numpy. Intensity cols are `{measure}_intensity_{i}` with
`measure` from `uns.intensity_measure` (`ARCHITECTURE.md:236`).

---

## The old R implementation is inspiration, not specification

All `old-R-shiny-version/...:line` citations below are **behavioural reference** — they
show *what* the old system achieved and *why* (the capabilities to preserve), not *how*
to build it. We can and should change the mechanics, data shapes, and APIs where the new
stack allows something cleaner. Treat the citations as "here's the prior art for this
behaviour," then design the Julia-native version freely. The non-negotiables are the
*capabilities* (unified `pop_dt`, cross-segmentation pooling, gate/track/cluster
composition, transformed-space gating) and the architecture invariants
(`docs/ARCHITECTURE.md`) — not the R structure.

## Steps

> Self-contained: execute, verify, report, then proceed. Dependencies flagged.
> Per CLAUDE.md: adding a dep to `app/Project.toml` requires
> `julia --project -e 'import Pkg; Pkg.instantiate()'` from `api/` before server restart;
> `api/src/*.jl` are not Revise-tracked (restart after route edits); all code Windows-safe
> (`joinpath`, no inline `kill`/`du`); **docs updated in the same change** (see Step 9).

### Step 1 — Julia LabelProps fluent reader (`app/src/label_props.jl`)
**Files**: `app/Project.toml` (+`HDF5`), new `app/src/label_props.jl`, register in
`app/src/Cecelia.jl`. (Then `instantiate` from `api/`.)
- Lazy chainable reader following the **documented** convention
  (`ARCHITECTURE.md:218-237`) and extending it for gating:
  `label_props(obj; value_name) |> view_channel_cols |> filter_rows([...], by=:label) |> as_df`,
  plus `select_cols([...])`, `filter_pops([...])`, `view_centroid_cols`, `sort_by`.
  **HDF5 I/O only inside `as_df`/`as_matrix`** (in a `do` block — no leaked handles).
- Resolve `.h5ad` path via the `label_props` versioned field on `CciaImage`
  (`app/src/model/image.jl`).
- **AnnData decode dispatcher** keyed on `encoding-type` (see H5AD reality).
- Column resolution: channel/intensity (`{measure}_intensity_{i}`), morphology, centroids
  (`centroid-0..2` from `obsm/spatial` via `uns/spatial_cols`), `label` (`obs/_index`).
- **Verify (REPL)**: load KDIeEm `B.h5ad`; assert `1377×27`; var_names match the 27
  audited features; `select_cols(["area"])` reads only that column; spot-check X values;
  read `obsm/spatial`.
**Blocks**: 2, 3.

### Step 2 — Gating engine: transforms + gates + tree (Julia)
**Files**: new `app/src/gating/transforms.jl`, `app/src/gating/gating_engine.jl`.
- **Transforms**: `LinearTransform`, `LogTransform`, `AsinhTransform`,
  `LogicleTransform` (params T/W/M/A) — **port Parks 2006** (`ARCHITECTURE.md:250`); use
  FlowKit/`flowutils` + the C++ as numeric references. Vectorized `apply(channel)::Vector`.
  Validate against published logicle reference values.
- **Gate structs**: `RectangleGate`, `PolygonGate` first (point-in-polygon ray-cast/
  winding; rect = 4 compares). Abstract `Gate` type so `ellipse`, `quadrant`, `boolean`
  slot in later. Vertices in **transformed space**; eval transforms the raw column then
  tests. JSON serialise/deserialise (StructTypes) — schema readable by Python.
- **Tree walk** `recompute!(map, obj; value_name)`: for each pop read its two channel
  columns via `label_props |> select_cols`, transform, test gate, propagate parent∩child
  in topological order (old `recompute()` + `gs_get_pop_paths(order="tsort")`,
  `flowGatingSet.R:78,431`). Cache the measurement matrix per image. Returns per-pop label
  vectors.
- **Density**: 2D histogram (bins + extents) for the plot fallback.
- **Time-agnostic**: gate eval is identical on timecourse segmentations — it gates the
  pooled cell-instances across all timepoints (no special code). This is what makes
  "gate first, then track" seamless (Step 3), replacing the old flow-GatingSet-then-track
  hack. (Per-timepoint gating can be added later; default is pooled.)
- **Verify**: point-in-polygon on a hand gate; logicle round-trip vs reference; tree
  parent∩child intersection.
**Depends on**: 1. **Blocks**: 3, 5.

### Step 3 — Population manager data model + persistence + `pop_dt()` (Julia)
**Files**: new `app/src/gating/population_manager.jl`. Persistence is a **sidecar file**
`{task_dir}/gating/{value_name}.json` (NOT a `ccid.json` field) — resolve `task_dir` the
same way `labelProps/` is resolved on `CciaImage` (`app/src/model/image.jl`).
- **`Population`** (generic): `name`, `path`, `parent`, `colour`, `show`, `value_name`,
  `pop_type`; flow → `gate` + `x_channel`/`y_channel` + **per-axis transform spec**;
  live/clust → **filter spec** (`filter_measure`/`filter_fun`/`filter_values` +
  `filter_default_all`, plural variants too) and `is_track`. Mirror the field set from old
  `populationUtils.R`/`flowHelpers.R` + the filtered-pop fields used in popDT:1606-1748;
  drop the flowWorkspace `gateID` indirection (gate lives on the Population).
- **Storage scope note**: the `gating/{value_name}.json` sidecar holds **flow** trees
  (one segmentation per file). `clust`/`live` pops span multiple value_names (their path
  prefix names the segmentation) and are created by later phases — their pop-map storage
  (names/colours/filter-specs, likely an image-level `populations/{pop_type}.json`) is
  **deferred**, but `pop_dt` must be written to *read* them. Decide their on-disk format
  when the clust/live phases land; do not block flow on it.
- **`PopulationMap`** tree (`{name, gate, children}`, `ARCHITECTURE.md:248`) + helpers
  ported from `flowHelpers.R`: `pop_parent`, `pop_path`, `pop_is_root`, `norm_root_path`,
  `trim_path`, `is_parent`, `replace_parent`, `change_parent_name`, `direct_leaves`,
  `leaves`. Root `"root"`/`"/"` normalised.
- **Tree mutations** (semantics from `flowGatingSet.R` + `gatePopulationsServer.R`):
  `add_pop!`, `set_gate!`, `rename_pop!` (cascade child-path rewrite), `del_pop!`
  (cascade descendants); each followed by `recompute!`.
- **Channel-name normalisation**: port `.flowCorrectChannelNames`
  (`gsub(" |\\(|\\)|\\-",".")`+dot-collapse); one shared function.
- **Persistence**: gate defs + transforms + tree → `{task_dir}/gating/{value_name}.json`
  (one strategy per segmentation, name-matched to `labelProps/{value_name}.h5ad`);
  `save_pop_map!(obj, pop_type; value_name)` writes/creates the `gating/` folder;
  `load_pop_map(obj; value_name)` reads it. Independent of `ccid.json` `save!`.
- **`pop_dt()` unified accessor** — old `popDT` (cciaImage.R:1490) + `completePopDT` (:67)
  are the *inspiration* for the capabilities below (redesign the mechanics freely; the
  R structure is not binding). The capabilities to provide:
  `pop_dt(obj, pop_type, pops; value_name=nothing, pop_cols, include_x=false,
  include_obs=true, drop_na, unique_labels=true, flush_cache=false) -> DataFrame` with a
  `pop` column.
  - **Pools across multiple value_names / segmentations.** The pop-path prefix encodes the
    value_name (anndataUtils.R:75: `value_name = match(pop, "^.+(?=/)")`), so
    `pop_dt(obj,"live",["Tcells/tracked","Bcells/tracked"])` loads `Tcells.h5ad` +
    `Bcells.h5ad`, joins each on `(value_name, label)`, and `vcat`s into ONE DataFrame
    with a `value_name` column. This is the core "pool all anndata files" behaviour.
  - **completePopDT join**: membership DT + label-props columns, per value_name, joined on
    `(value_name, label)`; `include_x` pulls the intensity matrix, `include_obs` pulls obs
    columns — each read via `label_props`.
  - **Membership source differs by type**: `flow` → gate eval via `recompute!`; `clust` →
    anndata `.obs` cluster column(s); `live` → tracking + **filtered populations**
    (`filterMeasure`/`filterFun`/`filterValues` — e.g. track-stat thresholds; the
    filtered-pop machinery at popDT:1606-1748 is core for live/clust, not just gates).
    Design all three branches now; only `flow` gets the gating UI this phase.
  - **Dedup** keyed on `(value_name, uID, pop, label, track_id)` (those present).
  - **Caching**: keyed by a hash of `(pop_type, pops, pop_cols, …)`; `flush_cache=true`
    clears (≈ R `flushCache`). In-memory per image.
  - **Set level** (CciaSet, later phase): pool across images too, adding a `uID` column —
    design `pop_dt` so the image-level result composes into a set-level `vcat`.
  - **Gate↔track composition** (design in now; tracking *task/UI* is a later phase): a
    **live** pop = a gated pop whose cells have been tracked. `pop_dt` for a live pop
    composes membership = gate-eval ∩ `track_id` present, joins `track_id`/`centroid_t`
    from obs, and applies any track-measure filters (filtered-pop machinery). The
    **tracking task** (later) consumes a gated pop via `popSelection` → `cells_in_pops`,
    runs btrack on those cells, and writes `track_id` to the H5AD obs (Python write path)
    — no GatingSet hack. Workflow: gate → track this pop → now live → optionally gate
    again on track measures. Ensure the flow pop model + `pop_dt` don't preclude a
    "tracked" node flag living in the same `gating/{value_name}.json` tree.
- **REPL contract** end-to-end headless: `add_pop!`, `set_gate!`, `recompute!`,
  `cells_in_pop`, `pop_dt`, `save_pop_map!`.
- **Verify (REPL + `app/test/runtests.jl`)**: gating JSON **round-trip** (`save_pop_map!`
  → `load_pop_map` preserves the tree + gate coords + transform specs) and the on-disk
  path is `gating/{value_name}.json`. Plus nested-tree identity, cascade rename/delete,
  point-in-polygon correctness. (No `ccid.json` change in this step, so no ccid round-trip
  test needed here.)
**Depends on**: 1, 2. **Blocks**: 4, 5.

### Step 4 — API routes: transformed plot data, density, pop CRUD, stats (Julia)
**Files**: `api/src/routes.jl` (handlers), `api/src/server.jl` (binary response + existing
`broadcast_ws`). (Restart server after edits — not Revise-tracked.)
- **Pop CRUD** (synchronous, in-process — no pool): `GET /api/gating/popmap`;
  `POST/PUT/DELETE /api/gating/pop`; `POST /api/gating/pop/rename`. Each mutation →
  `recompute!` → persist → `broadcast_ws` a `gating:popmap` update to all clients (reuse
  the `task:*`/`chain:*` broadcast pattern). Follow the documented HTTP.jl v2 stream
  conventions in CLAUDE.md (`read(stream)` before `setstatus`, etc.).
- **Scatter data — transformed binary Float32**:
  `GET /api/gating/plotdata?image&value_name&x&y&pop` → Julia reads raw cols
  (`label_props`), applies the axis transforms, returns interleaved `Float32Array`
  (`octet-stream`) + a small JSON sidecar (count, extents, **axis-tick positions** in
  transformed space).
- **Density fallback**: `GET /api/gating/density?...&bins=N` → 2D histogram → Float32 grid
  + extents; client switches to heatmap above a point threshold (overlay identical).
- **Stats**: `GET /api/gating/stats?pop` → count / %-of-parent (port `.flowStatsForPop`).
- **Membership**: `GET /api/gating/membership?image&value_name&pop_type&pops&invert` →
  per-pop label IDs. Backs `CeceliaClient.cells_in_pops` (Step 5). Default JSON, but serve
  a **binary `Int32` body** (same trick as `/plotdata`) for large/low-selectivity pops —
  at the **10⁶** target (static large images; live ~1000 cells × 180 timepoints) a 10⁶-ID
  JSON list is wasteful (~tens of ms); binary drops it to ~few ms. Supports multi-pop /
  multi-value_name requests (returns IDs grouped by `(value_name, pop)`).
**Depends on**: 3. **Blocks**: 5, 6, 7, 8.

### Step 5 — Napari consumer + Python membership handoff (no Python engine)
**Files**: `napari/napari_bridge.py`, `api/src/napari_api.jl`; light edits to
`app/py/pop_utils.py` (reduce to a metadata reader of `gating/{value_name}.json`).
- **No Python gate evaluation.** Julia is the sole evaluator.
- **Napari** is a pure consumer (`ARCHITECTURE.md:249`): draws polygon → POST
  `/api/napari/event` → Julia stores + reruns tree → Julia returns per-pop label IDs +
  colours → bridge colours label/points layers (replaces `show_pop_mapping`'s CSV read).
  Reuse the existing bridge command/dispatch pattern + Napari WS channel (7655).
- **Python membership via a thin HTTP client** (new `app/py/cecelia_client.py`): Python
  tasks/notebooks get membership from the running Julia API, never evaluating gates.
  `CeceliaClient(base_url, project_uid, image_uid).cells_in_pops(pop_type, pops;
  value_name, invert)` → label IDs (tiny payload). `PopUtils(client=cc).pop_df(task_dir,
  LabelPropsUtils(task_dir), pop_type, pops, cols)` keeps its **old signature** but
  resolves membership via the client and reads the measurement **columns locally** from
  the H5AD (no bulk data over HTTP). One code path for notebook development and shipped
  modules (the API is running in both — it launches production tasks). Mirrors the old
  module pattern (`inst/modules/sources/spatialAnalysis/py/cell_contacts_mesh.py`:
  `run(params)` → `pop_utils.pop_df(...)`); pop paths come from `popSelection` params.
  `pop_utils.py` reads the gating JSON only for metadata, never to evaluate gates. No
  `flowutils`/`juliacall` dependency.
- **Verify**: Step 10 — Julia membership consumed by Python yields the expected labels.
**Depends on**: 3, 4. **Blocks**: 10.

### Step 6 — WS types + Pinia stores (TS)
**Files**: `frontend/src/stores/ws.ts` (register `gating:popmap` via `on(type,handler)`),
new `frontend/src/stores/gating.ts` (mirror `stores/project.ts`),
`frontend/src/stores/project.ts` (+`gating` on `CciaImage`).
- `gating.ts`: pop tree, selected pop, per-panel plot state, stats; subscribes to
  `gating:popmap` for the re-entrancy-safe server push (Step 8).
**Depends on**: 4. **Blocks**: 7, 8.

### Step 7 — `ScatterGL.vue` (regl-scatterplot, transform-free)
**Files**: `frontend/package.json` (+`regl-scatterplot`), new
`frontend/src/components/plots/ScatterGL.vue`. Follow `docs/UI.md` plot conventions +
the lazy-import / `onBeforeUnmount` cleanup pattern from `IntensityHistogram.vue`.
- Loads the transformed `Float32Array` from `/api/gating/plotdata` (no JSON parse, no
  logicle in JS — already in display space). Renders axes from the tick sidecar. Props:
  image/value/x/y/pop. Exposes the data↔screen transform for the overlay. Handles the
  density-heatmap fallback.
**Depends on**: 4. **Blocks**: 8.

### Step 8 — `GateOverlay.vue` + re-entrancy guard, then `GateModule.vue` (TS)
**Files**: new `frontend/src/components/plots/GateOverlay.vue`,
`frontend/src/modules/GateModule.vue` + `frontend/src/modules/gate/*`; enable the sidebar
entry in `frontend/src/components/AppSidebar.vue` (`disabled:true`). Follow `docs/UI.md`
+ `docs/MODULES.md` module-page authoring; use `ModuleLayout` + `#right` slot (see
`MetadataModule.vue`/`SegmentModule.vue`).
- **GateOverlay**: `<canvas>` over the WebGL canvas; draws/edits **polygon** + **rectangle**
  gates in display space; emits `gate:created`/`gate:edited` with vertices in display
  coords → store → API. Trivial JS point-in-polygon gives **live during-drag** counts
  (authoritative counts from API on release). **No lasso / no transient selection.**
  **Re-entrancy guard**: ref `listeningToGating` — set `false` before any programmatic
  redraw from a `gating:popmap` push, `true` after; suppress emits while `false` (ports
  `flowListenToGating`/`flowNumGateUpdates`, `gatePopulationsServer.R`).
- **GateModule**: **multiple simultaneous panels** (port `flowPlotManager`): each =
  `{pop, xChannel, yChannel, xTransform, yTransform, leaves}` with its own
  `ScatterGL`+`GateOverlay`; a gate edited on one panel sharing pop+axes redraws the
  others via the store (guarded). **Population tree** (PrimeVue `Tree`, generic for all 3
  types): name, colour swatch, visibility, counts/%, add/rename/delete (cascade), colour
  edit; selection drives panels.
**Depends on**: 6, 7.

### Step 9 — Docs update (same change, per CLAUDE.md)
**Files**: `docs/ARCHITECTURE.md` (gating section `:241-250` from "planned" → implemented:
routes, WS `gating:popmap`, transform/membership model; **correct `:248`** —
gating is a per-segmentation sidecar `gating/{value_name}.json`, NOT a `ccid.json` field;
Julia is the sole evaluator, Python consumes), `docs/OBJECTMODEL.md` +
`docs/DATAMODEL.md` (the `gating/{value_name}.json` sidecar layout), `docs/UI.md` (GateModule +
ScatterGL/GateOverlay), `docs/NAPARI.md` (gate event + colour commands),
`docs/MODULES.md` if any task JSON is added, `docs/TODO.md` (move gating items to
`## Fixed` with date).
**Depends on**: runs alongside the steps that change each area.

### Step 10 — Verification suite
**Files**: `app/test/runtests.jl` (Julia), a cross-language script.
1. **Julia headless** (`cd app && julia --project test/runtests.jl`): reader (Step 1),
   engine transforms/gates/tree (Step 2), manager + `gating/{value_name}.json` round-trip
   + `pop_dt` (Step 3) — against KDIeEm `B.h5ad`. Include **pop_dt coverage**:
   multi-pop/multi-value_name **pooling** (two pops from two `.h5ad`s → one DT with a
   `value_name` col, deduped on `(value_name,label,…)`), `include_x`/`include_obs` joins,
   and a `live` **filtered-population** case (`filter_measure`/`filter_fun`). Use a
   synthetic second segmentation if no real multi-segmentation image is on disk.
2. **Julia↔Python handoff**: for one flow pop on KDIeEm, Julia `pop_dt`/`cells_in_pop`
   produces label IDs; assert a Python consumer fed those IDs selects the **same cells**
   from `B.h5ad` (validates the handoff contract — Julia is the sole evaluator).
3. **End-to-end**: drive add-pop→set-gate→recompute→stats→save over the API; assert
   `gating:popmap` broadcast fires and `/plotdata` returns the right transformed Float32
   length.
**Depends on**: all.

---

## Critical files
- **Julia**: `app/src/label_props.jl`, `app/src/gating/{transforms,gating_engine,population_manager}.jl`;
  edits to `app/Project.toml`, `app/src/Cecelia.jl`, `app/src/model/image.jl`,
  `api/src/{routes,server}.jl`; tests in `app/test/runtests.jl`.
- **Python**: `app/py/cecelia_client.py` (thin HTTP client for membership),
  `app/py/pop_utils.py` (`pop_df` resolves membership via the client; metadata reader),
  `napari/napari_bridge.py`, `api/src/napari_api.jl`. No `flowutils`/`FlowKit`/`juliacall`.
- **Vue**: `frontend/package.json`, `frontend/src/components/plots/{ScatterGL,GateOverlay}.vue`,
  `frontend/src/modules/GateModule.vue` (+`gate/`),
  `frontend/src/stores/{gating,ws,project}.ts`, `frontend/src/components/AppSidebar.vue`.
- **Docs**: `docs/{ARCHITECTURE,OBJECTMODEL,DATAMODEL,UI,NAPARI,MODULES,TODO}.md`.
- **Reference (old, read-only)**: `old-R-shiny-version/R/{flowGatingSet,flowHelpers,populationUtils}.R`,
  `inst/py/{pop_utils,napari_utils,label_props_view}.py`,
  `inst/app/modules/server/gatePopulationsServer.R`.

## Out of scope (this phase)
Lasso/transient selection; compensation matrices; FCS import/export;
flowWorkspace/cytolib compatibility; clustering UI; tracking UI. `pop_dt()` *reads*
`live`/`clust`, but their creation UIs are later phases. Gate types beyond
rectangle/polygon (ellipse, quadrant, boolean) are designed-for in the `Gate` type but
only rect+polygon are wired this phase. **No Rust / no fourth language**
(`ARCHITECTURE.md:191`).
