# Population manager & gating

> Status: **in progress**. Implementation plan (step-by-step) lives outside the repo at
> `~/.claude/plans/read-gating-integration-prompt-md-thorou-distributed-tulip.md`.
> This doc is the durable design reference; keep it current as steps land.

The **population manager** is the central abstraction for *all* population types — not a
gating module. Gating is one way to *define* a population; the manager, the unified
`pop_df` accessor, and the storage model are shared across types.

## Population types

| type | membership defined by | data source | stored sidecar |
|------|----------------------|-------------|----------------|
| `flow` | gates (polygon, rectangle; later ellipse/quadrant/boolean) on cell measurements | gate strategy + H5AD | `gating/{vn}.json` |
| `live` | tracking (btrack links cells across time) + optional track-measure filters | H5AD `.obs` (`track_id`) | none (derived off `flow`) |
| `track` | gates on **per-track** properties (one point per track) | `track_props` (motility + on-read aggregates) | `gating/{vn}__tracks.json` |
| `clust` | a filter on the **cell** cluster column (`filter_fun="in"` over `clusters.{suffix}`) | cell H5AD `.obs` column (clustPops) | `gating/{vn}__clust.json` |
| `trackclust` | a filter on the **track** cluster column (`filter_fun="in"` over `clusters.{suffix}`) | per-track table `.obs` column (clustTracks) | `gating/{vn}__trackclust.json` |

All types share: a population tree (name, colour, path, parent, show), a `pop_df`
accessor returning a filtered `DataFrame` regardless of type, and membership that is
**derived**, never duplicated into the H5AD.

## Language boundaries (per `ARCHITECTURE.md`)

- **Julia owns gating** — gate evaluation, logicle transform, tree recompute, `pop_df`.
  **No Rust / no fourth language** (`ARCHITECTURE.md:191`).
- **Read = Julia** (`HDF5.jl`, native; the fluent reader is `app/src/label_props.jl`).
  **Write is split** (see `DATAMODEL.md`): Julia writes **numeric `obs` columns** natively via
  `save!` (`HDF5.jl`), but **cannot write `X`** — the feature matrix, `var`, and any new file are
  Python's job (a subprocess via `python_bin_path()` → `anndata.write_h5ad`), as are **categorical/
  string `obs`** columns (`write_categorical_obs`). So: obs measures → Julia; `X`/`var`/new-file +
  categoricals → Python.
- **Frontend = UI only**: it never transforms or evaluates gates (see Transforms).
- **Python** is a *consumer* of membership, not an evaluator (see Membership access).

## Transforms (the key model)

Gating plots use **logicle / biexponential** axes. Logicle is nonlinear, so a polygon
drawn on a logicle plot is a curve in raw space — point-in-polygon is not invariant under
the transform. Therefore (FlowJo/OMIQ/FlowKit convention, `ARCHITECTURE.md:247`):

- **Raw H5AD data stays raw and singular** — there is no transformed copy on disk. (The
  old flowWorkspace `GatingSet` kept a transformed data copy; that duplication is gone.)
- **Display**: read raw, apply the transform on the fly to position points.
- **Gate**: drawn on the logicle display → vertices stored in **transformed coordinates**,
  with the transform spec attached to the gate.
- **Membership**: to test a raw cell, apply the same transform to its raw value, then
  point-in-polygon in transformed space.

Transforms (`linear`, `log`, `asinh`, **`logicle`** — Parks 2006) are implemented
**once, in Julia**. The API serves *already-transformed* `Float32` coordinates + axis
tick positions to the browser, so the frontend stays transform-free.

### Auto-linearisation (range-based)

A non-linear transform exists to spread a wide dynamic range; on a **bounded / small-range**
measure — morphology like `solidity` ∈ [0,1] — it does the opposite and collapses every value into a
sliver (logicle is calibrated to `T≈262144`, so `1.0` maps a hair above `logicle(0)` → an empty/clipped
axis). So `plotmeta` (when the client sends `autoLinear=1`) computes `effective_transform` from the
**whole-dataset** raw extent (stable across populations) and **substitutes `linear`** when the requested
transform would map the data into < `COERCE_MIN_FRAC` (5%) of its display span (`transforms.jl`). It
returns the transform it actually used as `usedX`/`usedY`; the client keeps the user's *preferred*
transform, displays the effective one, marks the axis **amber**, and reverts automatically on a
compatible measure. `plotmeta` and `plotdata` therefore agree by construction (the client fetches points
with the effective transform). Membership is untouched — gates keep their own stored transform. Both
the single flow plot and the channel-pairs / gating-strategy montage opt in (`autoLinear=1`); the montage
decides per canonical pair and amber-flags its shared transform control when any tile is coerced.

### Gate outlines projected for display

A gate's geometry is stored in the transform it was **drawn** in. If the axis is later shown in a
different transform (a switched/auto-linearised scale), drawing the stored coordinates as-is puts the
outline nowhere near the dots — and the client has **no** transform math to fix it. So `plotmeta`
returns each displayed population's child-gate outlines **re-projected into the effective display
transform** (`project_gate`: stored → raw via `invert_transform` → display via `apply_transform`, per
axis; a rectangle stays a rectangle, polygon vertices map pointwise). The client renders them as-is.
Drawing/editing stamps the **effective** transform on the gate (the space it was drawn in), so
membership stays correct.

## Storage

Gating is a **per-segmentation sidecar**, keyed by `value_name`, colocated with the
matching labels + labelProps — `ccid.json` stays lean:

```
{proj}/1/{uid}/
  labels/       B.zarr      ← segmentation "B"
  labelProps/   B.h5ad      ← cell measurements
  gating/       B.json      ← gating strategy for segmentation "B"
```

`gating/{value_name}.json` holds the population tree: `{name, gate, transform, children:[…]}`.
Plain JSON — readable by Julia and Python. (This supersedes the earlier
`ARCHITECTURE.md:248` "gating in ccid.json" plan.)

`live` populations span multiple value_names (their path prefix names the segmentation). They are
**derived populations** — membership from a column rule layered on the `flow` gates, not a stored
gate — so they need **no** pop-map storage of their own: `pop_df` reads the `flow` gates and layers
the derived filter on at read time (see "Reserved namespace" under `pop_df` below). The canonical
case, a `live` **tracked** pop, is `flow gate ∩ track_id > 0`.

**`clust`/`trackclust` populations are *stored* filter pops**, not derived/transient ones: the user
names a population and ticks cluster IDs into it, so each is persisted in its own sidecar
(`gating/{vn}__clust.json` / `__trackclust.json`) as a `Population` carrying
`filter_measure="clusters.{suffix}"`, `filter_fun="in"`, `filter_values=[ids]` (no gate). Created via
`pop/add` with a `filter` dict and retoggled via `pop/update`'s `filter` (Decision 10 — no dedicated
endpoint). Membership is still **derived at read time** (`recompute!` applies the filter over the
cluster column), never written into the H5AD; only the *definition* (which IDs) is stored. `clust`
evaluates over the cell table (`:cell`), `trackclust` over `track_props` (`:track`, with
expand-to-cells for `:cell`) — exactly mirroring `flow` and `track` respectively.

Clustering is **set-scope**, so a cluster pop is written **set-wide**: the same filter pop lands in
every clustered image's sidecar (the filter is image-independent and cluster IDs are comparable
across the run). Membership of a run is recorded as `partOf` (the clustered uIDs) in the
`{props}.clustfeatures.json` manifest — a cluster pop is only meaningful for images in that set, so
the UI restricts writes to the selected `partOf` images. The frontend applies this by replaying the
per-image `pop/add`/`pop/update`/… calls across those images (the gating store's `mirrorUids`); the
old R `propagatePopMap` "populate to set" crutch is gone.

## `pop_df` — unified accessor

> **Home:** `pop_df`/`_pop_df` live in `gating/population_manager.jl`, alongside the `pop_map`
> accessors (`load_pop_map`/`save_pop_map!`) — the generic, `pop_type`-neutral population layer.
> Deliberately **not** in `gating_engine.jl`: the accessor serves all `pop_type`s (`flow`, `live`,
> `clust`, transient) and gating is only one membership source. It builds on the gating engine's
> `recompute!`/`cells_in_pop` at call time.

```julia
pop_df(obj, pop_type, pops;
       value_name=nothing, pop_cols=nothing,
       include_x=false, include_obs=true,
       drop_na=false, unique_labels=true, flush_cache=false,
       raw_channel_names=false, granularity=:cell,
       cell_measures=String[], categorical=String[]) -> DataFrame
```

Returns a `DataFrame` with a `pop` column (+ `value_name`, requested cols). Capabilities:

- **Channel names resolved by default.** Intensity columns are returned under their channel
  names (`mean_intensity_0` → `"CD4"`, `nuc_mean_intensity_0` → `"nuc_CD4"`), mirroring R
  `popDT` / Python `change_channel_names`. Pass `raw_channel_names=true` for the raw
  `{measure}_intensity_{i}` names. Implementation: `label_props.rename_channels` is set on the
  **output** fetch only — gate evaluation always reads RAW columns (gates store raw intensity
  column names), so `_pop_df` takes a separate `membership_fetch`. Channel names are resolved
  via `channel_names(img; value_name)`, which falls back to the active version when a
  value_name (e.g. a labelProps name like `B`) has no own channel-name entry.

- **Pools across segmentations / value_names.** A pop path resolves its value_name two ways:
  a **prefix** names the value_name (`"A/qc"` → value_name `A`, path `/qc`), while a
  **leading-slash** path stays within the given/active value_name
  (`pop_df(img, "flow", ["/qc"]; value_name="A")`). So one call pools the same population across
  segmentations — `pop_df(img, "live", ["A/qc/_tracked", "B/qc/_tracked", "C/qc/_tracked"])`
  loads `A/B/C.h5ad`, resolves each value_name from the prefix, and `vcat`s into one DataFrame —
  but a leading-slash path *cannot* reach a different value_name than the one passed (so
  `["/A/qc", "/B/qc"]` with `value_name="A"` both resolve under `A`, not a mix).
- **Cell vs track grain (`granularity`).** `:cell` (default) → one row per cell. `:track` → one
  row per **track**: cell-level gate membership still drives selection, but the member cells'
  `track_id`s pick rows from the companion per-track table `{vn}__tracks.h5ad` (measures in
  `X`/`var`, lineage in obs; written by `tracking.track_measures`, see `docs/TRACKING.md` /
  `docs/DATAMODEL.md`). A track belongs to a pop if any of its cells are in it; the dedup/row key
  is `(value_name, track_id)`. This is the **one-point-per-track** view for track-property gating
  and per-track summary plots —
  `pop_df(img, "live", ["A/_tracked","B/_tracked","C/_tracked"]; granularity=:track)` returns one
  row per track pooled across A/B/C, ready to compare e.g. `live.track.speed` on one plot. The
  cache key folds in the track table's mtime so a re-run auto-invalidates.
- **`track` pop_type — gate DIRECTLY on per-track properties.** Distinct from `live`+`:track`
  above (which gates *cells* then aggregates): `pop_type="track"` evaluates the gate over the
  **per-track table** itself (one point per track), so a gate on *average track speed* / *mean
  turning angle* excludes whole tracks. The data source is **`track_props`** (`docs/TRACKING.md`):
  motility from `{vn}__tracks.h5ad` + on-read cell→track aggregates. The track gate map is a
  separate file `gating/{vn}__tracks.json` (loaded with `pop_type="track"`; `save_pop_map!` routes
  there by `m.pop_type`). `granularity=:track` returns the gated track rows; `granularity=:cell`
  **expands** each gated track to its member cells (`label`/`track_id`/`pop`/`value_name`) — the
  "select a track, pull in all its cells" behaviour for napari / membership. `cell_measures` and
  `categorical` name the *base* cell columns to aggregate into gateable track properties (numeric →
  `.mean/.median/.sum/.qUp/.qLow/.sd`; categorical → per-category frequency `{m}.{cat}`); the common
  case (motility-only gating) needs neither. **Numeric vs categorical is auto-detected** from the
  decoded type + values (strings like `hmm.transitions="1.3"` and integer code sets like
  `hmm.state∈{1,2,3}` → categorical; continuous floats like `speed=10.12` → numeric); no config map
  (replaces the old R `config.yml` `labelStats`). `categorical`/`numeric` are escape-hatch overrides
  on `track_props` when the heuristic misreads a column. The cache key folds the `{vn}__tracks.json`
  + track-table mtimes.
- **Completion join**: membership + label-props columns per value_name; `include_x`
  pulls the intensity matrix, `include_obs` pulls obs columns (read via `label_props`).
- **Membership source by type**: `flow` → gate eval (recompute); `clust` → `.obs`
  cluster column; `live` (and future `clust`) → a parent + a **derived population**.
  A **derived population**'s membership is a column rule (a filter on an obs/measure column), not a
  hand-drawn gate. The canonical one is **tracked**: `filter_measure="track_id"`,
  `filter_fun="gt"`, `filter_values=0` — cells of the parent gate that received a `track_id > 0`.
  **Derived pops are not stored**: the gates live in the `flow` map, and a path whose leaf is a
  registered derived pop is injected as a *transient* filtered child at read time, then
  `recompute!` composes parent ∩ filter. There is **no** `live` gating file; `"A/qc/_tracked"`
  reads `gating/A.json` (the flow gates) + `A.h5ad`'s `track_id`.
- **Reserved namespace — leaf names beginning with `_`.** Derived pops own the `_` prefix
  (`_tracked` now, clustering pops later; registry `_DERIVED_POPS`, predicate
  `is_reserved_pop_name`). Hand-drawn gates **may not** use it: `add_pop!`/`rename_pop!` reject a
  `_`-leaf (→ 400), and the frontend hints against it while typing. This makes a derived name
  unambiguous and impossible to shadow with a real gate. (Deserialisation via `from_tree` bypasses
  the guard, and the derived injection itself passes `reserved_ok=true`.)
- **`value_name=nothing`** resolves to the image's **active** segmentation (same resolution
  as `label_props(img)`); pass a name to set the default value_name for unprefixed pops.
- **`drop_na`** drops cells that are NA/NaN in any requested `pop_col` (mirrors R popDT
  `dropNA`; only the user-requested measure columns are considered, not bookkeeping columns).
- **Dedup / `unique_labels`**: when `true` (default), collapse to one row per cell with the
  **most-specific pop winning**. The dedup key is the present-column subset of
  `(uID, value_name, label, track_id)` — mirroring R popDT's merge cols — so `uID` (set-level
  pooling) and `track_id` participate only when those columns exist; `pop` is the value being
  collapsed, not part of the key. With `unique_labels=false` rows are kept per pop.
- **Cache** keyed by the request signature (`hash` of pop_type/value_name/pops/pop_cols/flags)
  **plus the on-disk mtimes of each involved value_name's `gating/{vn}.json` + `{vn}.h5ad`**,
  stored on the image (`img._pop_df_cache`, runtime-only, not serialised). Because `pop_df`
  reloads the map and reads the h5ad fresh every call, it always reflects the on-disk state — so
  folding the mtimes into the key **auto-invalidates** the cache when a gate edit (rewrites the
  map) or a re-track (rewrites the h5ad) lands. `flush_cache=true` is a manual override for
  in-memory edits never written to disk. The accessor returns a copy, so callers can mutate the
  result without corrupting the cache.
- **Set level** (later): pool across images, add a `uID` column — image results `vcat`. The
  dedup key picks up `uID` automatically once present.

REPL contract (headless, no API/UI): `add_pop!`, `set_gate!`, `recompute!`,
`cells_in_pop`, `pop_df`, `save_pop_map!`/`load_pop_map`.

## Gating engine

- Lives in Julia; operates on H5AD read via `label_props`.
- Gate types: rectangle, polygon first; abstract `Gate` so ellipse/quadrant/boolean slot
  in. Vertices in transformed space.
- `recompute!`: walk the tree in topological order, child membership = parent ∩ child
  gate. Cache the measurement matrix per image. Synchronous, in-process (no task pool) —
  interactive latency.
- Cascade `rename_pop!` (rewrite child paths) and `del_pop!` (remove descendants).
- **Time-agnostic**: gate eval is identical on timecourse segmentations (gates pooled
  cell-instances across timepoints) — no special code.

## Gate↔track composition (replaces the old GatingSet-then-track hack)

In the old system, "live" gating was hacked by building a flow `GatingSet` and tracking
it. The new design unifies via one engine + one cell table:

1. Gate cells (time-agnostic) → a flow population.
2. **Track** that population: the `tracking.bayesian_tracking` task (btrack) tracks either
   the whole segmentation or a gated flow population. For the gated case, **Julia** (the
   sole gate evaluator) resolves membership in-process via `cells_in_pop` and hands the
   label-ID list to the Python btrack runner — no CSV, no HTTP callback. btrack writes the
   lineage columns into the segmentation's `labelProps/{value_name}.h5ad` `obs`:
   `track_id, track_parent, track_root, track_state, track_generation, cell_id` (cells not
   in a track get `NaN`). See `app/src/tasks/tracking/bayesian_tracking.jl` +
   `python/cecelia/utils/tracking_utils.py`.
3. The result is a **live** population (gated cells that now have tracks).
4. Gate again on **track properties** if desired (deferred — see below).

`pop_df` for a live pop composes: membership = gate ∩ `track_id` present, joins
`track_id`/`centroid_t`.

### Gating on track properties — deferred (next phase)

The user-facing goal is to exclude tracks that are too short / too jumpy by **gating** on
track-level measures (e.g. *average track speed*, *average turning angle*), replacing the
old celltrackR-based post-track filters. This is **not** done yet and is a dedicated phase
to design together, for one concrete reason: gating on a track-level measure must show
**one point per track** (a 20-timepoint track is a single dot; selecting it pulls in all
its cells), which needs a per-track aggregation table + per-track plotdata/membership +
expand-to-cells — not a per-cell broadcast (that would render 20 dots per track). Note the
gateability constraint: the gating "channels" endpoint exposes only `X`/`var` feature
columns (`col_names(lp; data_type=:vars)`), so track measures must ultimately live as
gateable columns. The tracking task above writes only `track_id`+lineage — exactly what a
later phase needs to derive track aggregates and build the per-track gating view.

## Membership access (no CSV, no Python engine)

Julia is the **sole evaluator**. Membership is never written to the H5AD and there is no
per-pop CSV.

- **Napari** is a pure consumer — it never evaluates gates. Two linked directions
  (`docs/NAPARI.md`):
  - *show populations*: `POST /api/napari/show-populations` → Julia recomputes → sends per-pop
    label IDs + colours → bridge colours centroid Points layers (reads centroids locally). The
    transient "Napari selection" pop is **excluded** here (it's the selection *source*; rendering
    it back stole napari's active layer mid-draw).
  - *cell selection (linked brushing)*: user draws a region on the image → bridge POSTs the
    inside cells' label IDs to `/api/napari/event` → Julia stores them and broadcasts the tree
    with a **transient selection population** so the flow plots highlight exactly those cells.
- **Python tasks / notebooks** get membership via a thin HTTP client
  (`python/cecelia/cecelia_client.py`) → `GET /api/gating/membership` (label IDs only; the
  bulk measurement columns are read locally from the H5AD via `python/cecelia/utils/label_props_utils.py`).
  `PopUtils(client=cc).pop_df(…)` (`python/cecelia/utils/pop_utils.py`) keeps its old signature. Same
  code path in notebook dev and shipped modules (the API is running in both). No
  `flowutils`/`juliacall` dependency.

### Transient populations (explicit-label membership)

Besides gate (`flow`) and filter (`live`/`clust`) membership, a `Population` may carry
`explicit_labels` — its cells **are** that label set (∩ parent), bypassing any gate. This backs
the napari cell selection: `add_pop!(m, "Napari selection"; explicit_labels=ids, transient=true)`.
`transient` pops are kept in the in-memory map and the `gating:popmap` broadcast (so every
client and `pop_df`/`plotdata`/`stats` treat them like any population) but are **excluded from
`save_pop_map!`** — they never reach `gating/{value_name}.json`. The API keeps the selection in
an in-memory registry keyed by `(task_dir, value_name)` and re-injects it into every served/
broadcast map; an empty selection clears it (the manager exposes a trash button →
`store.clearNapariSelection`, since there's no persisted pop to delete). Because a transient pop
can vanish between renders, `_plot_xy` returns empty data for an unknown pop rather than throwing
— a stale plot/highlight pointing at a cleared "Napari selection" no longer 500s.

`_node_dict` emits a **`membership_sig`** (hash of the sorted label set) for explicit-label pops.
They have no gate/filter for the client to diff, so without this a resized napari selection would
update the manager's cell count (stats always refetch) but **not** the plots — the client's
per-pop version (`gateSignatures` → `popVersion`) only bumps on a signature change. The signature
now includes `membership_sig`, so resizing the selection shape refreshes the highlighted overlay.

**Reconnect resync (server restart).** Because the selection lives only in the server's in-memory
registry, a backend restart wipes it — but the browser keeps the old tree AND the persisted highlight
that referenced it (`shared.hl` in `moduleCanvases.json`), so on the same image the plot stayed greyed
(highlight → `showPops` on, but the membership fetch returns 0 points). `GatingPlots` now resyncs on WS
**reconnect** (`ws.status` → `connected` after a drop, guarded so it skips the first connect): it
refetches the popmap, the fresh tree drops the transient pop, and the existing stale-highlight prune
watch clears the dangling reference (which then autosaves the cleaned `hl` back). The fix is
client-side; the server persistence was already correct.

## API & frontend

- **Routes** (synchronous, in-process): `GET /api/gating/popmap`; `POST/PUT/DELETE
  /api/gating/pop`; `POST /api/gating/pop/rename`; `GET /api/gating/plotdata`
  (transformed `Float32`, binary + JSON sidecar with extents + axis ticks);
  `GET /api/gating/density` (2D-histogram fallback); `GET /api/gating/stats`;
  `GET /api/gating/membership` (label IDs; **binary `Int32`** for large pops).
- **WS**: `gating:popmap` broadcast after any mutation (reuses `broadcast_ws`).
- **Frontend**: `regl-scatterplot` (WebGL, millions of points) + a `canvas2D` overlay for
  gate drawing; density heatmap fallback above a point threshold; **re-entrancy guard**
  (`listeningToGating`) so server pushes don't re-emit gate mutations; multiple
  simultaneous panels; hierarchical population tree (generic for all 3 types). **No
  Plotly** for gating.
- **Channel-pairs matrix (read-only)**: the gating page's `+ Pairs` button (`GatingPlots`) adds a
  `GatePairsPanel` — the single plot's X/Y generalised to a *list* of channels, rendered as a
  scatter-plot matrix (GGally `ggpairs` layout) for choosing the best two channels to gate a population
  on: **lower triangle** = the scatters, **diagonal** = each channel's name (labels its row + column, so
  per-tile axis labels are dropped), **upper triangle** = each pair's Pearson correlation (reused from
  the mirror scatter's points — no extra fetch, text scaled by |r|). Only the lower triangle fetches, so
  it's N(N-1)/2 point clouds, not N². It draws NO gates, but shows the displayed population's child gate
  outlines on the matching scatter tiles and honours the SAME highlight pipeline as a normal plot
  (manager "eye" pops + transient napari cell-selection light up every tile). Shared with both flow and
  track gating (one `GatingPlots` host, keyed by `popType`). It reuses the shared `GateMontage` renderer
  (see [`docs/UI.md`](UI.md) → *Gate scatters*); the pure helpers are `plots/pairsMatrix.ts`
  (`buildPairDefs` / `reconcileChannels` / `estimateMatrixLoad`) + `plots/montage.ts` (`pearson`), all
  unit-tested. It honours the manager's
  **Axis** toggle (whole-dataset origin-0 vs autoscale-to-population) — alignment holds because a tile's
  x-range depends only on (x-channel, pop) and its y-range only on (y-channel, pop). Channel count is
  capped (8 → 28 scatter plots); above an estimated point-load threshold (pairs × population cell-count) a
  brief amber warning shows, with the numbers + the fix in its tooltip. The selection is pruned to the
  current segmentation's columns on a value_name switch (reseeding defaults if it empties).

## Scale

10⁶ cells is a real target (static large images; live ~1000 cells × 180 timepoints).
Implications: WebGL rendering with density fallback; binary `Float32`/`Int32` transfer;
per-image measurement + membership caching in Julia; lazy column reads in `label_props`.

## Out of scope (this phase)

Lasso/transient selection; compensation; FCS import/export; flowWorkspace/cytolib compat;
clustering UI; tracking UI/task. `pop_df` *reads* `live`/`clust`, but their creation UIs
are later phases; only `flow` is wired to the gating UI now.
