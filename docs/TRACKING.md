# Tracking

Cell tracking links per-cell detections across time into tracks. This ports the old
R/Shiny **Bayesian tracking** (btrack) module to the new stack: it tracks either a whole
segmentation or a gated flow population, and writes track lineage into the segmentation's
label-props H5AD (below). A separate **`tracking.track_measures`** task then computes
celltrackR-style motility measures (speed, angle, displacement, …): **per-cell** measures go to
the cell H5AD `obs`, **per-track** measures go to a companion per-track table
`labelProps/{value_name}__tracks.h5ad` (one row per track, measures in `X`/`var` so they are
gateable). See "Track measures". The per-track view is read via `pop_df(…; granularity=:track)`;
**gating on** track properties in the UI is the next phase ("Still deferred").

## Pipeline at a glance

```
ParamRenderer (popSelection) → TaskRunner (task:run)
        → Julia tracking.bayesian_tracking  _run_task
            → (gated?) cells_in_pop(...)        # Julia is the sole gate evaluator
            → subprocess: bayesian_tracking_run.py
                → btrack  → lineage columns
                → write back into labelProps/{value_name}.h5ad  obs
```

## What it does

- **Input choice (the only two modes):** track the **whole segmentation**, or track a
  **gated flow population**. The old "filter" stage (`filterMeasure`/`filterFun`/
  `filterValues`) and the post-track `minDisplacement`/`maxMeanTurningAngle` filters are
  **dropped** — they only existed because the old stack couldn't track gated cells.
- **Gated membership is resolved in Julia, in-process.** The handler calls
  `cells_in_pop(load_pop_map(img; value_name, pop_type="flow") |> recompute!, pop)` and
  passes the **label-ID list** to Python. Julia is the sole gate evaluator (see
  `docs/POPULATION.md`); Python never evaluates gates, and there is **no CSV handoff** and
  **no HTTP callback** (unlike the napari linked-brushing path).
- **btrack** runs in the Python subprocess and writes lineage into the **same**
  `labelProps/{value_name}.h5ad` `obs`.

## Files

| Layer | Path |
|---|---|
| Julia handler | `app/src/tasks/tracking/bayesian_tracking.jl` (`struct BayesianTracking`) |
| Param spec | `app/src/tasks/tracking/bayesian_tracking.json` |
| Python runner | `app/src/tasks/tracking/bayesian_tracking_run.py` |
| Python utils | `python/cecelia/utils/tracking_utils.py` (`BayesianTrackingUtils`) |
| Vendored config | `app/src/tasks/tracking/cell_config.json` |
| Frontend page | `frontend/src/modules/TrackingModule.vue` (route `/track`, sidebar after Gate) |
| popSelection widget | `frontend/src/tasks/ParamRenderer.vue` |

Registered in `app/src/tasks/task_registry.jl` (`_spec_path` + `_fun_name_map` →
`tracking.bayesian_tracking`) and included in `app/src/Cecelia.jl`.

## Output — track lineage in H5AD `obs`

Written to the segmentation's `labelProps/{value_name}.h5ad` `obs`, aligned on the integer
cell label, following our AnnData convention (`docs/DATAMODEL.md`). `X`/`var`/`obsm`/`uns`
are preserved untouched.

| obs column | meaning |
|---|---|
| `track_id` | btrack track number (NOT the cell label). Cells of one track share it. |
| `track_parent` | parent track id (cell division / branching) |
| `track_root` | root track id (lineage) |
| `track_state` | btrack state (e.g. 5 = alive) |
| `track_generation` | generation index in a division tree |
| `cell_id` | 1-based index of the cell within its track, ordered by time |

Cells not assigned to a track get **`NaN`** in all of these. A "tracked" population is then
just cells with `track_id` present (the old `live` filter `track_id > 0`).

## Non-obvious things (carry into the next phases)

- **`track_id` is the btrack track number, not the cell label.** `label_id` is btrack's
  name for our cell label and is the merge key back onto `obs`. Don't conflate them.
- **Lineage in `obs`, track *measures* in the track table's `X`/`var`.** Lineage is identity, so
  it goes in the cell `obs`; the gating "channels" endpoint exposes only `X`/`var` as gateable
  (`col_names(lp; data_type=:vars)`), so `track_id`/lineage are deliberately **not** gateable in
  the cell table. Per-track *measures* that must be gateable live in `X`/`var` of the companion
  `{value_name}__tracks.h5ad` (one row per track) — see "Track measures".
- **Timecourse required.** Tracking needs `obsm['temporal']` + `uns['temporal_cols']`
  containing `t`. The runner raises a clear error on a single-timepoint segmentation. The
  `B.h5ad` fixture has 20 timepoints and can be used for end-to-end btrack verification.
- **Pixel-space tracking.** Centroids are fed to btrack in **pixel** coordinates (no
  physical-unit scaling), so `maxSearchRadius` is in pixels (matches the UI label). The old
  module scaled by `omeXMLPixelRes`; we deferred that. For anisotropic-Z 3D data this may
  need physical pixel sizes passed from Julia later.
- **Centroid axis order.** `obsm['spatial']`/`uns['spatial_cols']` are skimage order:
  2D = `(y, x)`, 3D = `(z, y, x)`. The runner maps these to btrack `x/y/z` by dimensionality.
- **Vendored btrack config.** `btrack.datasets.cell_config()` **downloads from the
  internet** (pooch) — unusable headless. We vendor the proven old-cecelia config
  (`old-R-shiny-version/inst/models/btrackModels/cell_config.json`) at
  `app/src/tasks/tracking/cell_config.json` and load it from disk. UI params override the
  motion/hypothesis fields on top of it (e.g. `accuracy*10`, `P*noiseInital`,
  reversed `prob_to_assign`, `+P_branch` when branching is on).
- **`minTimepoints` is kept** as a basic tracking cutoff (drop tracks shorter than N) — it
  bounds what btrack emits and is distinct from the dropped displacement/angle *filters*.
- **The bridge analogy doesn't apply.** Unlike napari (a long-running WS bridge), tracking
  is a normal one-shot task subprocess streaming `[PROGRESS] n/total`; nothing is
  hot-reloaded specially.

## Track measures (`tracking.track_measures`)

A separate task computes celltrackR-style motility measures from the `track_id` lineage above.
**Per-cell** measures (`live.cell.*`) are written to the cell `labelProps/{value_name}.h5ad`
`obs` (one value per cell, keyed by label like any per-cell measure); **per-track** measures
(`live.track.*`) are written to a companion per-track table
`labelProps/{value_name}__tracks.h5ad` (one row per `track_id`, measures in `X`/`var`). It is a
standalone task so any future tracker (beyond btrack) shares the same downstream measures step;
the composite
**`tracking.bayesian_track_measures`** chains `tracking.bayesian_tracking` → `tracking.track_measures`
(the old `calcTrackingStats` tickbox is replaced by this composite).

| Layer | Path |
|---|---|
| Julia handler | `app/src/tasks/tracking/track_measures.jl` (`struct TrackMeasures`, `struct Track`) |
| Param spec | `app/src/tasks/tracking/track_measures.json` (`valueName`, `dims`, `forceRecompute`) |
| Composite spec | `app/src/tasks/tracking/bayesian_track_measures.json` |

Registered in `task_registry.jl` (`tracking.track_measures` + the composite in
`_COMPOSITE_SPEC_PATHS`) and included in `Cecelia.jl`.

### Units

Coordinates are scaled to **µm** (`pixel_res` from `PhysicalSizeX/Y/Z`) and time to **minutes**
(`time_step = TimeIncrement / 60`, since OME `TimeIncrement` is in seconds) — both via
`img_physical_sizes` (`app/src/model/image.jl`), matching the old R `omeXMLTimelapseInfo` (which also
reports minutes). So **`speed` is in µm/min** (T cells ~10; KDIeEm mean 14.5), `trackLength`/
`displacement` in µm, `duration` in minutes. (Earlier the time step was left in seconds, giving
speeds on a 0–1 µm/s scale — fixed.) Changing this convention requires **re-running
`tracking.track_measures`** to regenerate the persisted `{vn}__tracks.h5ad` (the measures are not
recomputed on read).

### Motion dimensionality — 2D in-plane vs full 3D (`dims` param)

All per-cell and `live.track.*` measures derive from the **same step vectors**, so 2D-vs-3D is **one
decision** governing every measure. It matters because a coarse/anisotropic z (thin stacks, few
slices) often carries only **segmentation jitter**, not real migration — feeding it into the 3D
turning angle + speed corrupts them (a fast cell appears to reverse). This was a real bug: on a
z=5 µm / xy=0.5 µm, 8-slice set, 3D states came out `(slow/low-angle, …, fast/high-angle)` instead of
the expected scanning(slow/high-angle) / directed(fast/low-angle). Our port is a **bit-exact**
celltrackR match (verified by running celltrackR 1.2.2 on the same tracks → identical to 1e-16), so
the fix is the input dimensionality, not the math.

`dims` param: **`auto` | `2D` | `3D`** (default `auto`). `detect_motion_dims(props_path, pixel_res,
time_step)` (`track_measures.jl`, **cached by the h5ad mtime**) decides by testing whether z carries
**directional motion**, using corroborating signals so it's robust:
- **per-axis lag-1 velocity autocorrelation** — a migratory axis is persistent (`> 0`); a jitter axis
  is anti-persistent (`≤ 0`). Primary signal (works for float *or* quantized centroids — note z is a
  float regionprops centroid, ~⅓ integer on thin stacks, so an "is-integer" test would be unreliable).
- **directional persistence** (mean `cos` turning angle) in 3D vs in-plane — collapse if 3D drops far
  below in-plane.
- z keeps 3D only if clearly migratory; collapses to 2D only if clearly jitter; **ambiguous / too few
  steps → keeps 3D at low confidence** (never silently drops a dimension).

The decision + metrics are **logged** (a `[WARN]` when it collapses to 2D or is low-confidence) and
surfaced **before running** via the run-form `motionDimsSelection` widget (which calls
`GET /api/tracking/motion-dims` for the selected image — same cached result) showing the recommendation
+ warning; the user can override (e.g. force 3D). When resolved to 2D the tracks are collapsed to the
in-plane (xy) axes before any measure is computed. The result dict carries `dims`/`dimsAuto`/`dimsReason`.

### celltrackR port

The measure functions are a Julia port of **celltrackR** (Wortel et al. 2021,
doi:10.1016/j.crmeth.2021.100006; GPL-2). The attribution header is at the top of
`track_measures.jl`. celltrackR is **not** a runtime dependency — the port is pinned to it by
**golden values cross-checked against celltrackR 1.2.2** in `app/test/runtests.jl` (the
`Track measures (celltrackR golden)` testset, to 1e-6, plus single-step / single-position /
zero-displacement edge cases). Ported: `trackLength`, `duration`, `speed`, `displacement`,
`maxDisplacement`, `straightness`, `displacementRatio`, `outreachRatio`, `asphericity`,
`overallAngle`, `meanTurningAngle`, `vecAngle`. **Not** ported: simulation (random walks,
bootstrapping), MSD/autocorrelation, clustering, gap repair (btrack handles gap closing).

Edge-case conventions (match celltrackR, asserted in the golden test): `straightness` → `1.0`
when `trackLength == 0`; `displacementRatio` → `NaN` when `maxDisplacement == 0`; `asphericity`
→ `1.0` for <3 steps and `NaN` for 1-D; `vecAngle` clamps the cosine to `[-1, 1]`.

### The `Track` struct and physical units

```julia
struct Track
    id::Int
    t::Vector{Float64}       # physical time (frame index × time_step seconds)
    coords::Matrix{Float64}  # n_steps × n_dims, physical space (px × pixel_res µm), skimage z?,y,x
end
```

Coordinates are read **in pixels** from `obsm/spatial` and time **as frame index** from
`obsm/temporal` (via the `LabelProps` chain — no raw HDF5), then converted to physical units at
compute time using `img_physical_sizes(img)` (µm/px per axis + s/frame, read from `img.meta`,
persisted at import from the OME-NGFF scale transform). If physical metadata is absent the
conversion falls back to `1.0` (pixel/frame units) — measures stay correct, just unscaled.

### Output — per-cell in cell `obs`, per-track in a companion table

Measures are split by their natural grain (no redundant broadcast):

**Per-cell → cell `labelProps/{value_name}.h5ad` `obs`** (one value per cell, `add_obs |> save!`):

| obs column | scope | meaning |
|---|---|---|
| `live.cell.speed` | per-cell | step speed to this cell (celltrackR `subtracks(·,1)` speed; cell 1 = `NaN`) |
| `live.cell.angle` | per-cell | turning angle at this cell, degrees (`subtracks(·,2)` `overallAngle`; cells 1–2 = `NaN`) |

Per-cell angle assignment follows celltrackR's `increment.cell.id=TRUE` (the step measure is
assigned to its endpoint cell). Untracked cells get `NaN`.

**Per-track → `labelProps/{value_name}__tracks.h5ad`** — ONE row per `track_id`:

- `X`/`var` = the 10 track measures: `live.track.speed`, `.duration`, `.trackLength`,
  `.displacement`, `.straightness`, `.displacementRatio`, `.outreachRatio`, `.meanTurningAngle`,
  `.overallAngle`, `.asphericity`.
- `obs` = lineage carried over per track (`track_root`, `track_parent`, `track_state`,
  `track_generation`); `obs._index` = `track_id`.
- Created via Python `anndata` (`tracking_utils.write_track_props`, called by the
  `track_props_run.py` subprocess) — new-file creation is Python's job (`docs/DATAMODEL.md`).
  Julia computes the measures (the celltrackR port) and hands the table over as JSON.

**Why a separate table, measures in `X`/`var`.** A track is the natural row, so per-track values
live once per track — not broadcast across every cell (the old, redundant design). Putting them
in `X`/`var` makes them **gateable**: track-property gating is just flow-gating pointed at
`{value_name}__tracks.h5ad`. The double-underscore suffix is reserved
(`is_reserved_value_name`) so it can't collide with a segmentation named `{x}_tracks`.

The per-track view is read with **`pop_df(img, "live", pops; granularity=:track)`** (one row per
track; the member cells' `track_id`s select rows from the track table — see `docs/POPULATION.md`).

### Cache & invalidation

`track_measures` is a deterministic function of the track coordinates, so it caches: the sentinel
is `live.cell.speed` present in the cell `obs` **and** the `{value_name}__tracks.h5ad` file
existing. A cached run is skipped; `forceRecompute=true` overrides. **The tracking task owns
invalidation** — when btrack writes new `track_id`s it drops any stale `live.cell.*` /
`live.track.*` obs columns (`tracking_utils.py._write_back` via the `drop_obs` chain verb, see
`docs/DATAMODEL.md` and TODO #00028); `track_measures` likewise drops any leftover broadcast
`live.track.*` from the cell obs when it runs. Re-running the composite recomputes against the
fresh tracking and rewrites the track table.

## Track-property gating — backend done, frontend/napari deferred

Gating on track properties (one point per track) is a first-class **`track` pop_type**. The backend
is **done** (ports R `tracksInfo`); the gating UI + napari Tracks layer are the next sub-steps
(plan: plotting-canvas-and-track-df, phase 3c–3e).

- **`track_props` (`app/src/tracking/track_props.jl`) — done.** Compute-on-read per-track table
  keyed by `track_id` (== `label`, so the gate engine's by-`label` membership works unchanged):
  `num_cells` + motility from `{value_name}__tracks.h5ad` + on-read aggregates of any requested
  **cell** column (numeric → `.mean/.median/.sum/.qUp/.qLow/.sd`; categorical → per-category
  frequency `{m}.{cat}`). Nothing persisted — never stale, no re-run when new cell measures appear
  (same "derive don't duplicate" choice as the track table). Ports R `tracksInfo`.
  - **Numeric vs categorical is auto-detected** from the decoded type + values — no config map
    (replaces the old R `config.yml` `labelStats`). `_is_categorical_col`: a non-`Real` column
    (anndata `string-array`/`categorical` decode to `String`) → categorical (e.g.
    `hmm.transitions = "1.3"`); a `Real` column with any fractional value → numeric (continuous, e.g.
    `speed = 10.12`); a `Real` all-integer column with few distinct levels → categorical (integer
    code set, e.g. `hmm.state ∈ {1,2,3}`; threshold `_MAX_CATEGORICAL_LEVELS`). The integer rule is a
    heuristic (a wide-spread integer count stays numeric); `categorical`/`numeric` kwargs force a
    column either way. The string/categorical-encoding path is exact, so a producing task can
    guarantee correct detection by writing a true categorical as an anndata `categorical`.
- **`pop_df(img, "track", pops; …)` — done.** Gates evaluated DIRECTLY over the `track_props`
  table; gate map in `gating/{value_name}__tracks.json`. `granularity=:track` → gated track rows;
  `granularity=:cell` → expand to member cells. Distinct from `pop_type="live"` + `granularity=:track`,
  which gates *cells* then aggregates to tracks. See `docs/POPULATION.md`. Verified on KDIeEm B
  (gate on `live.track.speed`; track rows ↔ expanded cells).
- **Gate-map storage — done.** `gating_path(task_dir, vn; pop_type="track")` →
  `gating/{vn}__tracks.json`; `save_pop_map!`/`load_pop_map` route by `pop_type`.

Still deferred:

1. **Gating API track-awareness (3c)** — the gating endpoints branch their data source on
   `popType="track"` → `track_props`; channels list motility + cell-aggregate columns; pop CRUD
   persists to `{vn}__tracks.json`.
2. **Show tracks in napari (3d)** — napari's native **Tracks layer** (`viewer.add_tracks(data, …)`,
   `data` = `[track_id, t, (z,) y, x]` + lineage `graph`) built from the `track_id` + centroids +
   `t` in the H5AD, kept in sync with the gating selection (port R `show_tracks`; `docs/NAPARI.md`).
3. **Track-gating canvas in the Tracking module (3e)** — the gating scatter + population manager
   with `popType="track"`, reusing the extracted canvas shell; conditional manager option-groups.

Tracked as TODO #00021. All of it builds on the `track_id` + measures already written — nothing
above needs to change.
