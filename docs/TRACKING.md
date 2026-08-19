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

**A root track's parent is ITSELF.** For a track with no division history btrack writes
`track_parent == track_root == track_id` and `track_generation == 0` — *not* `NaN`. Anything that
creates a track by hand (see correction, below) must follow that convention; writing `NaN` for
"no parent" produces a track that reads as untracked lineage. Verified on real data
(`zolIMa/1/fXgbTl`, 374 tracks, all roots). Every one of these six columns is stored **float64** —
`add_obs` writes Float64 only (`app/src/label_props.jl`), so round the values on read.

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
- **µm-space tracking (2026-08-07).** Centroids are scaled to **µm** before btrack sees them
  — `physicalSizes` from `img_physical_sizes`, skimage order `[sz, sy, sx]`, the same
  accessor `track_measures`, `cellNeighbours` and the mesh tasks already use. Every distance
  param is therefore µm: `maxSearchRadius` (default **20**, ~T cell), `distThresh`,
  `thetaDist`. Time stays in **frames** (`maxLost`, `timeThresh`) because btrack's `t` is a
  frame index.

  It was pixels until then, and tracking was the ONLY spatial task not calling
  `img_physical_sizes` — so the linking ran in pixels while `track_measures` reported µm/min
  on its own output. One pipeline, two coordinate systems.

  **Scaling the coordinates, not each param**, for a reason no per-param conversion could
  reach: at 0.33 µm XY and 2 µm Z, pixel space scored a one-plane hop as 0.33 µm of motion
  when it is 2 µm — a **6× under-count**, in exactly the direction that links cells at
  different depths. In µm the axes are commensurate by construction.

  **This reinterprets saved values.** `maxSearchRadius = 20` used to mean 20 px (6.6 µm at
  0.33 µm/px) and now means 20 µm — ~3× looser in XY, stricter in Z. The DEFAULTS did not
  change numerically (20/10/5), which preserves their tuned ratios but makes the change
  invisible on the form. Same class as the `minCellSize`/`labelExpansion` µm migration.
  3D results will not reproduce; re-run to regenerate.

  An image with no calibration falls back to unscaled — old pixel behaviour — rather than
  pretending 1 px = 1 µm.
- **btrack runs TWO phases, and different params bound each.** `tracker.track()` links
  frame to frame under `maxSearchRadius` (per STEP) and `maxLost`. Then
  `tracker.optimize()` runs the global hypothesis optimiser, which **joins finished track
  ends** under `distThresh` / `timeThresh` / `thetaDist` / `lambdaDist`. `maxSearchRadius`
  and `maxLost` are not consulted there.

  Two consequences that read as bugs and are not:

  * **A drawn track is longer than the search radius, by design.** The radius bounds one
    step; the viewer draws the accumulated trail. Measured on `zolIMa/fXgbTl`
    (`maxSearchRadius = 8`, 0.33 µm/px): worst single step 8.2 px/frame — the limit is
    honoured — while a 31-frame track covers ~25 µm at the median step and ~70 µm at p95.
    So check whether the long thing is one STEP or a whole trail before suspecting the
    tracker. (Open: on that run a single-frame jump was still reported by eye across two
    consecutive frames, which the cell table does not contain — the drawn vertices come
    from `napari_bridge._tracks_matrix`, so that is where to look, not at the params.)
  * **`maxLost` does not bound the gaps you end up with.** Same run, `maxLost = 1`, and
    final tracks contain gaps of 2–5 frames: each tracklet respected `max_lost`, then the
    optimiser joined them across up to `timeThresh` (5). To limit gaps, set `timeThresh`.

  To diagnose "why is this linked", measure per-step displacement from the tracked
  `labelProps` (`LabelPropsView(...).view_centroid_cols()`, group by `track_id`, diff the
  centroids) rather than judging trail length by eye.

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
`docs/DATAMODEL.md`); `track_measures` likewise drops any leftover broadcast
`live.track.*` from the cell obs when it runs. Re-running the composite recomputes against the
fresh tracking and rewrites the track table.

## Manual track correction (`tracking.correct`)

Fixing a wrong track by hand. Design + the old-R ground truth it ports:
[`docs/todo/CORRECTION_PLAN.md`](todo/CORRECTION_PLAN.md). Shipped: the ops engine, the task, the
journal and its QC (plan phase **P1**), and the worklist UI (**P4**). Segmentation correction (P2) is
not built.

**Two modes, because the detector is not the only source of a bad track.** *Suggested* is the ranked
worklist; *All tracks* lists every track and turns a selection into the same op objects the detector
emits — one queue, one task run, one journal, whether an edit was suggested or hand-authored. Without it
the surface was WORSE than old R for the case a user simply sees: there you could at least name the
tracks. Join is blocked with a reason when the two tracks share frames — the engine's own rule, checked
before Apply instead of after, and the picked track can be flown to in napari like a suggested one.

**From napari, not from a table.** *All tracks* also reads the viewer: draw a region around the cells
(`POST /api/napari/start-selection`, the existing brush), then **Read selection** →
`GET /api/tracking/selection` resolves the enclosed labels to their tracks and **Pick** selects them in
the list, fetching them explicitly if they fall outside the picker's cap. That closes the loop the
worklist could not: you see the bad track in the image and act on it there, instead of reading an id off
the viewer and hunting for it. The same selection drives the one CELL-level op — **Untrack cells**
(`points.remove`), which drops bad detections and leaves the rest of their tracks intact.

**Naming beats raising a cap.** The picker lists 2000 tracks (longest first) because a 5000-row table is
not a picker; `find` sends `ids=` to `/api/tracking/paths`, which bypasses the cap for exactly the tracks
named. Raising the limit would make every request slower for everyone to serve one lookup.

**The detector's thresholds are exposed** (a collapsed *Sensitivity* section). They matter more than a
default can: on the reference image the same 374 tracks yield **10 candidates** at `jumpQuantile 0.999`
and **309** at `minLen 15`. The panel seeds the knobs from what the server actually used
(`thresholds` in the response) and sends only what the user moved, so the measured defaults live on the
Julia constants and are never copied into TypeScript to drift.

**The UI inverts the old version.** There you found the wrong track yourself, among hundreds, and
then said how to fix it. Here `find_track_issues` ranks what looks wrong and pre-picks the op
(`GET /api/tracking/issues`), each row draws its own geometry, and the user only judges it. Nothing is
written until Apply, which submits the whole queue as ONE `tracking.correct_measures` run. It is a
panel on the **Track page's canvas** (`GatingPlots`, `popType="track"`) — beside the track gating it
changes — hosted through the generic `InteractivePanel` from the `trackCorrection` registry entry. That
entry carries no surface flag on purpose: it MUTATES, and the Analysis board is read-only.

**A track correction is an `obs` rewrite and nothing else.** It moves cells between `track_id`s and
maintains the lineage columns; it never touches `X`/`var`, so the cell table needs no re-measure.
That is why it is cheap, and why it is a separate task from segmentation correction (which rewrites
the label store and *does* force a re-measure).

| Op | Effect |
|---|---|
| `points.remove` | untrack the given cells (`track_id := NaN`) |
| `points.add` | attach cells to a track, or to a new one when no `trackId` is given |
| `track.remove` | untrack every cell of a track |
| `track.join` | fold track B into track A; **B ceases to exist** |
| `track.split` | cells at/after a timepoint become a new track |

Ops are pure functions over a label-keyed frame (`app/src/tracking/track_correction.jl`) and are
applied **in order**, each seeing the previous result — so the op list is a replay script, which is
what makes a correction reproducible from the producing task's output plus the journal.

Three rules that are not obvious:

- **The write is never population-scoped.** `add_obs` aligns by label and writes `NaN` for every
  label *absent* from the staged frame, so staging a population subset would untrack every cell
  outside it. A population may scope what the user can select; it must never scope the write.
- **A join refuses a temporal overlap.** Two tracks that both have a cell at one timepoint are not
  one cell. Old R silently re-assigned only the non-overlapping part, leaving B alive as a shorter
  remnant; consuming the overlap instead would give the joined track two cells at one time and make
  every `dt` — and so every speed — degenerate. Both are wrong, so the op fails and names the
  timepoints.
- **`cell_id` is renumbered and lineage is reconciled** after every batch. A joined track keeps A's
  parent/root/generation; a split fragment becomes a root. `track_state` is per-cell and is left
  alone. Old R maintained none of this — it only ever wrote `track_id`.

**Invalidation.** The task drops stale `live.cell.*` / `live.track.*` obs columns itself (the same
thing btrack does when it rewrites tracks), so a standalone correction can never leave measures
describing the previous assignment. The composite **`tracking.correct_measures`** chains
`tracking.correct` → `tracking.track_measures` to recompute them; `track_measures` rebuilds tracks
from `obs.track_id` alone, so it recomputes correctly after any correction.

**Journal.** Every applied op is appended to `{task_dir}/corrections/{value_name}.json` — the same
per-segmentation sidecar shape as `gating/{value_name}.json`, written with `write_json_atomic`. This
is the durable, per-image edit history; old R's died with the Shiny session.

## Tracks as a plot (`trackPaths`)

Tracks were viewable only in napari, which is fine for judging one cell and useless for a figure: a
viewer screenshot cannot be recoloured by a measure, put beside another condition, or exported as
vectors. `TrackPathsView` is the plot half — an interactive-view registry entry, so it lands on the
Track canvas **and** the Analysis board with panel chrome, zoom and PNG/SVG/CSV export attached.

Three modes, because "the tracks" is three questions:

| Mode | Shows | Why |
|---|---|---|
| Paths | the polylines where the cells were | the spatial picture, the one napari draws |
| Star | every track translated to a common origin | position discarded, SHAPE preserved — the celltrackR rose family (Wortel et al. 2021, doi:10.1016/j.crmeth.2021.100006); directed migration fans, a random walk fills a disc |
| Rose | one arrow per track, start → end | net displacement, when hundreds of paths have become a scribble |

- **Axes are always square** (`pathDomain` in `frontend/src/plots/trackPaths.ts`). A track plot
  stretched to its panel turns a straight run into a diagonal, destroying the one thing these modes
  exist to show.
- **Geometry comes from `GET /api/tracking/paths`**, in the same wire shape the correction worklist
  reads — one Julia helper (`track_path_dicts`) builds it for both routes, so they cannot drift.
- **The colour-by list is not a second vocabulary.** It comes from
  `/api/gating/channels?popType=track`, the same call the track-gating axes read, so anything you can
  gate on you can colour by (motility measures + the per-track cell aggregates).
- **The cap is stated, not silent.** Longest-first, capped, and the plot reports `shown of total` —
  a hairball of 500 tracks looks exactly like a hairball of 5000.

### A track view never defaults to `default` — or to the active segmentation

Both are routinely untracked. On the reference image (`zolIMa/1/fXgbTl`) `default` and the active
`three` carry no tracks while `memTom` holds 374 — so a panel defaulting to either reported "nothing to
review" for an image with 31 correction candidates. `GET /api/gating/channels?popType=track` therefore
returns **`trackedValueNames`** (via `is_tracked`, which reads only the obs column list), and every track
surface resolves through one helper, `resolveTrackValueName`: a persisted choice that is still tracked →
the active segmentation if it is tracked → the first tracked one. The picker disables the untracked
entries rather than hiding them, so "why is this one not offered" has a visible answer.

## Track diagnostics — the celltrackR QC battery (`track_diagnostics.jl`)

"Can this tracking result be trusted, and what kind of motion is in it." Ported from celltrackR's
source (not its vignette prose) and **validated against celltrackR 1.2.2 itself** — five golden
testsets in `app/test/suite.jl` match its output to 10 decimals, which pins the CONVENTIONS as well as
the arithmetic (see below).

| Check | Statistic | Read it as |
|---|---|---|
| Displacement | MSD vs lag over every overlapping subtrack (`squareDisplacement`), log-log slope | 1 = random walk, 2 = directed, <1 = confined |
| Persistence | mean cosine between two steps `lag` apart (`overallNormDot`), and the lag where it hits 1/e | slow decay = directional migration; flat at 0 = none; negative = jitter |
| Volume edge | step angle vs distance to the lower z plane (`angleToPlane`/`distanceToPlane`), 3D only | unbiased 3D motion averages **32.7°** (Beltman 2009) at every distance; a sag ONLY near the edge is a tracking artefact |
| Drift | Hotelling's T² on step displacement vectors (`hotellingsTest`) | a net field direction — stage drift *or* chemotaxis, the user decides |
| Track pairs | angle vs distance between every pair of paths (`analyzeCellPairs`) | far-apart pairs average 90°; lower = the field moves together |

**Three things that are easy to get wrong, and are pinned by the goldens:**

- **`step_spacing` is not optional.** Consecutive steps of a persistent cell are correlated, so
  Hotelling's T² on every step is significant for essentially any real dataset. On the golden fixture:
  every step → p = 5.2e-4, steps 3 frames apart → p = 0.11. Same data, opposite verdict. The parameter
  counts frames SKIPPED (celltrackR's `overlap = -step.spacing`), so the stride is `step_spacing + 1`
  and `0` means every step. Drift is tested in **xy only**, matching celltrackR's `dim = c("x","y")`.
- **A time gap is not a lag.** celltrackR's subtracks are contiguous by construction; a btrack table's
  are not — a dropped detection leaves non-consecutive frames, and indexing by position would average
  a 4-frame displacement into the lag-1 MSD. Only pairs whose FRAME difference equals the lag count.
- **A subtrack length is not a lag.** celltrackR's `subtrack.length = L` dots the first and last step
  of an L-step subtrack, so its L maps to lag L−1 here; its L=1 is the trivial 1.0.

**Not assessed is not zero.** A drift p with too few decorrelated samples, an sem at n=1, a plane angle
for 2D data — all `NaN` in the package, all `null` on the wire, all absent from the plot. A verdict
invented from a matrix that cannot be inverted is worse than no verdict; `drift_test` on a *noiseless*
drift returns no p-value at all, and says so.

**Routine, not available.** The earlier pair diagnostics (`analyze_cell_pairs`, `find_duplicate_tracks`,
`track_pair_drift`) shipped exported and reachable from nothing — no task, no route, no view. A
diagnostic nobody can open is a diagnostic nobody has. So one roll-up (`track_diagnostics`) is now read
two ways:

- **`tracking.track_measures` banks the findings as QC on every run**, whether or not anyone looks —
  drift, confined motion, edge artefact, duplicate pairs. `msdSlope` and `persistenceLag` join
  `COHORT_METRICS`, so a movie whose motion reads as confined while its peers are random walks shows up
  as a cohort outlier. `driftP` deliberately does NOT: a p-value is not a quantity to take a median of.
- **The `trackDiagnostics` plot** (Track page → **+ Checks**, and the Analysis board) draws the curves
  and shows the SAME findings, rendered from the same objects. The panel cannot disagree with the QC
  line, because neither computes a threshold of its own.

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

Tracked in `docs/TODO.md`. All of it builds on the `track_id` + measures already written — nothing
above needs to change.
