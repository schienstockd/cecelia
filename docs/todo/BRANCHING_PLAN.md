# Branching (skeleton) analysis — port plan

**Status:** planning (2026-07-25). No branch yet, no code written.
**Ports:** `old-R-shiny-version/inst/modules/sources/segment/createBranching.R` (+ its
`py/create_branching.py` and `inst/app/modules/inputDefinitions/segment/createBranching.json`).

## Goal

`segment.branching` — skeletonise an existing segmentation into a **branch/path network**, measure each
path (length, tortuosity, branch type, endpoints), and expose the paths as a first-class population
type so branches can be plotted, gated against, and used as a spatial reference for cells.

Scientific target: fibrous / reticular structures that are *not* cells — collagen (SHG), nerves,
stromal networks, FRC/CCL19 reticular networks. The old version used it for exactly this
(`mxIBEX` stroma, `behaviourTumour` SHG, `mxCCL19`, `stomics_10x_breast_cancer`).

Intent is **full parity**, including the ILEE extended cytoskeleton measures — see Decision 4.

---

## Why this is a port, not a rewrite

Verified 2026-07-25 against the current pixi env (Python 3.12.13, numpy 2.0.2, scipy 1.18.0,
scikit-image 0.26.0, numba 0.65.1):

- **`skan` is healthy.** conda-forge `skan 0.13.1`, published 2026-02-06, BSD-3. Ran it against the
  old code's expectations: `skan.Skeleton` + `skan.summarize` still emit exactly the columns
  `create_branching.py` consumes — `skeleton-id`, `node-id-src`, `node-id-dst`, `branch-distance`,
  `branch-type`, `image-coord-src-N`, `image-coord-dst-N`, `euclidean-distance`. Only change needed:
  pass `separator='-'` explicitly (the default flips to `_` in a future release; a
  `VisibleDeprecationWarning` fires today).
- **The old Python maps almost 1:1 onto current helpers.** `DimUtils`, `slice_utils.create_slices`,
  `zarr_utils.open_as_zarr` / `create_multiscales` / `fortify` and `ome_xml_utils` have effectively the
  same API as the versions the old script was written against. Steps "open → slice → skeletonise →
  summarise → write pyramid" port near-literally.
- **ILEE runs unmodified in the current env.** Both `analyze_actin_2d_standard` and
  `analyze_actin_3d_standard` were executed on synthetic 2D/3D fibre images and returned the summary
  frame + the 5-tuple of anisotropy box arrays. No numpy-2 / scipy-1.18 / skimage-0.26 breakage on
  the code paths branching uses.

The parts that genuinely need work are the framework-contract changes (pop model, h5ad creation,
centroid naming, QC) and the ILEE vendoring hygiene — not the algorithm.

---

## Locked decisions (2026-07-25)

### Decision 1 — Branch tables are a **sidecar of the segmentation**, not a new label_props value_name

Write per-branch measurements to **`labelProps/{vn}__branch.h5ad`**, mirroring how per-track tables
work (`{vn}__tracks.h5ad`, `TRACK_PROPS_SUFFIX` in `app/src/model/image.jl:94`). Add
`BRANCH_PROPS_SUFFIX = "__branch"` + `img_branch_props_path(img, vn)` beside the track equivalents,
and extend `is_reserved_value_name` so a user can't create a segmentation called `X__branch`.

**Rejected alternative:** registering `{vn}.branch` in the `label_props` dict as its own value_name
(what the old R version did). The old version could get away with it because `valueNames()` took a
`valueType` regex argument and filtered `\.branch$` in or out per call site
(`R/cciaImage.R:3040-3041`). The new framework has **no such classification** — every
`popSelection` / `valueNameSelection` / measure picker enumerates `versioned_keys(img.label_props)`
flat. Registering a branch value_name there would leak "stroma.branch" into the tracking picker, the
clustering picker, the gating segmentation dropdown and the measure-labels picker, where it is
meaningless. The sidecar pattern sidesteps this entirely — and it is the pattern tracks already
proved.

### Decision 2 — A real `branch` pop type

Add `POP_MAP_SUFFIX["branch"] = "__branch"` so branch populations persist to
`gating/{vn}__branch.json`, exactly parallel to `track` → `gating/{vn}__tracks.json`.

This was initially argued against in favour of reusing the `flow` map over a `.branch` value_name.
That was wrong, for two reasons found on closer reading:

1. Decision 1 removes the branch table from the `label_props` dict, so there is no value_name for a
   `flow` map to hang off. The pop map must be keyed the way the data is — by suffix.
2. Reusing `flow` would invite the gating UI to offer channel-intensity gates on branch objects as
   if they were cells. Branches are a distinct granularity; conflating them is the semantic muddle,
   not the cure for it.

**Cost, stated honestly.** `granularity` is today `"cell" | "track"` and `pop_category` returns
`gated | clustered | region | tracked | aggregated`. Branches are a **third granularity**. Sites to
extend (measured, not guessed):

| File | What |
|---|---|
| `app/src/gating/population_manager.jl` | `POP_MAP_SUFFIX`, `ACCEPT_TOKENS`, `_POP_TYPE_PROBE_ORDER`, `_NAME_GUARD_POP_TYPES`, `scope_pop_types`, `population_accept_groups`, `pop_category`, `is_track_pop`'s granularity peer |
| `app/src/model/image.jl` | `BRANCH_PROPS_SUFFIX`, `img_branch_props_path`, `is_reserved_value_name` |
| `frontend/src/utils/popGroups.ts` | `GRAN_LABEL`, `ORDER` (add `branch:*` rows) |
| `frontend/src/stores/gating.ts` | popType union comment + any `'track'` special-casing that should read "non-cell" |
| `app/test/runtests.jl` | pop-map round-trip + accept-token + dispatch tests |

For scale: `region` touched 16 sites in `population_manager.jl` and 23 assertions in
`runtests.jl`. Branching should be lighter than that (it needs none of `region`'s
co-clustered-sibling auto-share machinery — `branch-type` is one fixed categorical column, not a
per-run `{prefix}{suffix}` column family), but it is the one part of this plan whose cost I would not
call precisely in advance. **This is the decision most worth challenging at review.**

### Decision 3 — Per-branch-type pops via `ensure_filter_pop!`, not bespoke code

The old task's tail auto-created one filtered pop per unique `branch-type`
(`createBranching.R:91-108`). `ensure_filter_pop!` (`population_manager.jl:1018`) is exactly this
mechanism and is documented as measure-agnostic — "a 0/1 flag (`> 0`), a probability (`≥ 0.5`) or a
category (`in […]`) all work". So the port is one call per branch type, idempotent on re-run, with no
new machinery. Name the pops by branch-type semantics (`endpoint-to-junction`,
`junction-to-junction`, `endpoint-to-endpoint`, `isolated-cycle`) rather than the old
`xfun::numbers_to_words` integers — the skan codes are stable and documented.

### Decision 4 — Ship ILEE extended measures, vendored under `python/cecelia/vendor/`

`calcExtended` is not optional dead weight: it is used in real analyses
(`behaviourTumour.Rmd:132` SHG collagen; `behaviourTcells3P.Rmd:793-836` consumes
`ilee_coor_list` / `ilee_eigval` / `ilee_eigvec` for anisotropy quiver plots), and the box-data
return that produces those arrays is a **local addition that does not exist upstream**.

Vendor to **`python/cecelia/vendor/ILEE_CSK/`** (`functions.py`, `fast_interp.py`, `LICENSE.txt`).
`python/pyproject.toml` ships only `include = ["cecelia", "cecelia.utils"]`, so the vendored package
is importable in-repo (via the `PYTHONPATH=python/` that `run_py` sets) but is **not** pulled by an
external `pip install cecelia` — coastal never inherits it. No `sys.path` hack needed.

Also required: a `THIRD_PARTY.md` entry (MIT, Copyright 2021 Pai Li) and a
`python/cecelia/vendor/ILEE_CSK/PATCHES.md` carrying the ledger below, so the next person does not
have to re-derive it by diffing against a dead repo.

### Decision 5 — Drop `saveProps` / `saveMeshes`

The old script called `measure_utils.measure_from_zarr(...)` with `slices=`, `integrate_time=`,
`save_meshes=`, `calc_intensities=`. That function was **rewritten** — it is now
`MeasureUtils(params, dim_utils).measure_from_zarr(label_zarrs, im_dat, log)` and has none of those
arguments. Rather than teaching the rewrite about per-slice measurement again, the branch labels zarr
is registered in the `labels` dict (Decision 6) so the user runs the existing
**`segment.measureLabels`** on it. That is the framework's canonical measure/segment split — cellpose
does the same.

### Decision 6 — Branch **labels** ARE registered in the `labels` dict as `{vn}.branch`

Asymmetric with Decision 1 on purpose. The skeleton needs its own zarr for napari display, and
`labels` is a separate ccid dict from `label_props`, enumerated by a different picker
(`valueNameSelection` with `"field": "labels"`). Registering there is what makes Decision 5 work —
`segment.measureLabels` can target the branch label set.

> **Open sub-question for review.** If a user then runs `segment.measureLabels` on `{vn}.branch`, it
> writes `labelProps/{vn}.branch.h5ad` and registers that value_name — i.e. it reintroduces exactly
> the picker pollution Decision 1 avoids, and creates a *second* branch table alongside
> `{vn}__branch.h5ad`. Options: (a) accept it (the user opted in explicitly), (b) have branching also
> accept an `intensityValueName` and fold per-branch intensities into `{vn}__branch.h5ad` itself,
> making `measureLabels` unnecessary, (c) teach `measureLabels` to write to the sidecar when its
> target is a branch label set. **(b) is the cleanest and probably the right answer, but it partly
> un-does Decision 5 — resolve before Phase 2.**

### Decision 7 — `refPops` membership resolves in Julia, never in Python

The old Python did its own population lookup (`PopUtils.pop_data` → mask the label array,
`create_branching.py:86-97`). Do **not** port that. Julia is the sole gate evaluator
(`docs/POPULATION.md`); follow `bayesian_tracking.jl:29-42` — resolve `cells_in_pop` in-process and
hand the label-ID list to Python as a param. The JSON param becomes a `popSelection` with
`"accepts": ["live", "clust", "region"]`.

### Decision 8 — Centroids follow the current convention, not the old one

The old script hand-built `uns['spatial_cols']` as `centroid_t/_z/_y/_x` string arrays and computed
`obsm['spatial']` as the median of the `image-coord-src-*` / `image-coord-dst-*` pairs. Keep the
median-of-endpoints definition (it is the branch's representative point), but write it through the
current contract — `obsm['spatial']` + `obsm['temporal']` with
`uns['spatial_cols'] = skimage_centroid_axis_names(n)` and `uns['temporal_cols'] = ['centroid_t']`,
as `measure_utils._to_anndata` does (`measure_utils.py:359-392`). See
`docs/todo/CENTROID_AXES_PLAN.md`.

---

## ILEE_CSK patch ledger

Upstream `github.com/phylars/ILEE_CSK` — **last substantive commit 2024-04-22**, README-only edits
before that back to 2023-07, **no tags, no PyPI/conda release**. Treat as unmaintained; we own this
copy. Diff of the old-cecelia vendored `functions.py` against upstream `main` = 69 added / 41 removed
lines, in six changes:

| # | Change | Why it exists | Keep? |
|---|---|---|---|
| 1 | MATLAB-engine block commented out | Upstream does `from imp import find_module` **at module scope**; `imp` was removed in **Python 3.12**, so upstream is literally unimportable on our interpreter (3.12.13). This is the breakage that forced the fork. | **Keep** — and replace with an explicit comment saying why, not a commented-out block |
| 2 | `NE_peak`: `mark = -1` guard + `if mark >= 0` | Upstream raises `UnboundLocalError` when the first scan loop never breaks | **Keep** |
| 3 | `@nb.njit` disabled on `anisotropy_2d_internal` | numba cannot compile it (takes skan's `nbgraph`) | **Keep** — but note it, this is the slow path |
| 4 | `@nb.njit` disabled on `anisotropy_3d_internal` | same | **Keep** |
| 5 | `analyze_actin_2d_standard` / `_3d_standard` gained `aniso_radius`, `aniso_box_size`, `aniso_weighting_method`, `return_box_data` | Local **feature**: upstream hardcodes radius/box (150/75 in 2D, 50/25 in 3D) and returns no box data at all. The `coor_list`/`eigval`/`eigvec`/`box_total_length`/`box_anisotropy` arrays the quiver plots consume exist only because of this patch. | **Keep** — this is ours |
| 6 | 2D path: 3× oversampling `resize` commented out, marked `# TODO why is this here?` | ⚠️ **Bug.** The three downstream `/3` divisions were left in place | **FIX** — see below |

### The `/3` bug (patch 6)

In `analyze_actin_2d_standard`, upstream oversamples 3× before skeletonising:

```python
img_dif_ovsp = resize(img_dif, (img_dif.shape[0]*3, img_dif.shape[1]*3), order=3)
img_binary   = img_dif_ovsp > 0
```

The vendored copy replaces this with `img_binary = img_dif > 0` but keeps all three compensating
divisions:

```python
diameter_tdt   = 4*(mean_DT-0.5)*pixel_size/3
diameter_sdt   = 2*(mean_DT_sk-0.5)*pixel_size/3
MF_full_length = total_length(img_sk.astype('float'))*pixel_size/3   # /3 because interpolated 3 fold
```

So `Diameter_tdt`, `Diameter_sdt`, `MF_full_length` — and `linear_density`, which divides by
`MF_full_length` — come out **3× too small** in the 2D path. Either restore the oversampling (slower,
upstream-faithful) or drop the `/3`s (fast, but the `-0.5` border correction was tuned for the
oversampled grid, so it is not a pure algebraic swap). **Restoring the oversampling is the safer
default; make it a param if the cost matters.** Any `calcExtended` 2D numbers already in existing
projects carry this error — they are not comparable with post-fix output.

### Two more fixes to apply while vendoring

- **`gaussian_scaled` (line 289)** calls `gaussian(..., multichannel=False)`; `multichannel` was
  **removed in scikit-image 0.26** → `TypeError`. Dormant for branching (only reachable via
  `ILEE_3d`, the *thresholding* entry point, which branching never calls) but a live landmine for a
  future `segment.ilee` port, whose `ilee_utils.py` calls `ILEE_CSK.ILEE_2d`/`ILEE_3d` directly. Fix
  now: drop the kwarg (the kernel is single-channel).
- **`from scipy.ndimage.morphology import distance_transform_edt`** — deprecated namespace, removed
  in SciPy 2.0. Change to `from scipy.ndimage import distance_transform_edt`.

### Deps

Only **`skan`** is genuinely new (`pixi add skan`, conda-forge, noarch, ~1.5 MiB; pulls
`imageio`/`matplotlib-base`/`networkx`, all already present). `numba 0.65.1` is already in the env
transitively and `cvxopt` is already pinned as a btrack dep — ILEE adds no new heavy dependency.

---

## Files

```
app/src/tasks/segment/branching.jl            NEW  struct + _run_task
app/src/tasks/segment/branching.json          NEW  param spec
app/src/tasks/segment/branching_run.py        NEW  skeletonise + skan + (opt) ILEE
app/src/tasks/task_registry.jl                EDIT _spec_path + "segment.branching" => Branching()
app/src/model/image.jl                        EDIT BRANCH_PROPS_SUFFIX, img_branch_props_path, is_reserved_value_name
app/src/gating/population_manager.jl          EDIT `branch` pop type (Decision 2 table)
app/src/qc_cohort.jl                          EDIT COHORT_METRICS["segment.branching"]
app/src/Cecelia.jl                            EDIT exports
python/cecelia/vendor/ILEE_CSK/               NEW  vendored lib + LICENSE.txt + PATCHES.md
pixi.toml / pixi.lock                         EDIT skan
THIRD_PARTY.md                                EDIT ILEE_CSK entry
frontend/src/utils/popGroups.ts               EDIT branch granularity label + order
app/test/runtests.jl                          EDIT pop-map round-trip, accepts, dispatch, param validation
docs/SEGMENTATION.md                          EDIT branching section
docs/POPULATION.md                            EDIT branch pop type
INVENTORY.md                                  EDIT vendor dir + branch props accessor
```

No new frontend module page: `SegmentModule.vue` is generic (`useTaskDefs('segment')` → `TaskRunner`),
so the task appears from its JSON alone.

## Params (from `createBranching.json`, adjusted)

| Param | Type | Notes |
|---|---|---|
| `valueName` | `valueNameSelection` (`labels`) | segmentation to skeletonise |
| `refPops` | `popSelection`, `accepts: [live, clust, region]` | optional mask; membership resolved in Julia (Decision 7) |
| `preDilationSize` | int 0–10, default 2 | binary closing before skeletonise |
| `postDilationSize` | int 0–10, default 2 | dilation of the skeleton (visibility) |
| `useBorders` | bool | skeletonise label *boundaries* instead of interiors |
| `flattenBranching` | bool | Z-MIP the labels before skeletonising |
| `calcExtended` | bool | ILEE cytoskeleton indices → `uns` |
| `calcFlattened` | bool | run ILEE on a Z-MIP (2D mode) for a 3D image |
| `anisoRadius` | int 0–250, default 50 | ILEE anisotropy window; box size = `floor(radius/2)` |
| ~~`saveProps`~~ | — | dropped (Decision 5) |
| ~~`saveMeshes`~~ | — | dropped (Decision 5) |
| ~~`popType`~~ | — | subsumed by `popSelection`'s value-prefixed refs |

`resource_pool: "cpu"`. Skeletonisation is single-threaded CPU; the non-jitted ILEE anisotropy loops
are the slow part (patches 3/4).

## QC (mandatory — `docs/MODULES.md`)

Metrics: `nBranches`, `meanBranchLength`, `nSkeletons`. Advisory finding: `branching.no_branches`
(warn) when the skeleton yields zero paths — the one unambiguous failure (empty/over-eroded input).
Keep the finding logic in a pure helper next to `_segment_qc_findings` and unit-test it. Register
`COHORT_METRICS["segment.branching"] = ["nBranches", "meanBranchLength"]`.

Finding text per `docs/MODULES.md` (short = the problem, long = the action, imperative):
short `"No branches found"`, long `"Lower the pre-dilation or check the segmentation, then re-run."`

## UI compliance

This port adds **no new chrome** — that is the point of Decision 2's cost table. Still, three current
rules bind the frontend edits in Phase 2:

- **UX-primitive catalog is mandatory *and* test-enforced** (`docs/UI.md` → *CHECK BEFORE BUILDING*).
  `utils/cssScenarios.test.ts` holds a per-file baseline that **may shrink and must never grow**, and
  `utils/cssTokens.test.ts` fails on any undeclared custom property or `var(--x, #hex)` fallback. The
  planned edits (`popGroups.ts`, `stores/gating.ts`) are pure logic with no CSS, so they should move
  neither baseline — if a branch-specific control ever seems necessary, it is a signal the design went
  wrong, not a licence to add a class.
- **No raw sizes, radii or hex colours** anywhere, including inline `style=`. Same tests.
- **UI text stays brief.** `popGroups.ts` gets `branch: 'Branches'` — one word, matching `Cells` /
  `Tracks`. Task-JSON `tip` fields default to **omitted**; where a param genuinely needs one, keep it
  under ~10 words. Do **not** copy the house style of `segment/measure_labels.json`, whose tips are
  full sentences describing on-disk paths — that is the old convention and reads as generated. The
  explanation of what branching does belongs in `docs/SEGMENTATION.md`, not on the form.

---

## Phases

**Phase 0 — deps + vendoring.** `pixi add skan`; vendor ILEE_CSK with patches 1–5 preserved, patch 6
fixed, plus the `multichannel` and `scipy.ndimage.morphology` fixes; `PATCHES.md` + `THIRD_PARTY.md`.
Checkpoint: `pixi run test-py` green; a Python unit test imports the vendor package and asserts
`analyze_actin_2d_standard` returns 9 summary columns + a 5-tuple on a synthetic fibre image.

**Phase 1 — core branching, no pops.** `branching.jl` / `.json` / `_run.py`: mask → optional flatten →
optional borders → per-timepoint close/skeletonise/dilate → labels zarr + pyramid → `skan.summarize`
paths table → `{vn}__branch.h5ad` (Decision 8) → register labels (Decision 6) → QC. Registry entry.
Checkpoint: runs end-to-end on a real fibrous image; `pixi run test-pkg` green (dispatch + param
validation + a bad-param `ParamValidationError`).

**Phase 2 — the `branch` pop type.** Decision 2's table + `ensure_filter_pop!` per branch type
(Decision 3). **Resolve Decision 6's open sub-question first.** Checkpoint: branch pops appear under a
"Branches · Gated" header in a `popSelection`; pop-map round-trips; `pop_df` returns branch rows.

**Phase 3 — ILEE extended measures.** Wire `calcExtended` / `calcFlattened` / `anisoRadius` through to
the vendored entry points; `ilee_summary` + the four anisotropy arrays into `uns`. Cite the paper +
upstream URL at the call site per the "cite sources" rule. Checkpoint: `calcExtended=true` on a 2D and
a 3D image; `uns` shapes match what `behaviourTcells3P.Rmd` expected.

**Phase 4 — napari + plots (follow-up).** Old version colour-mapped the branch labels layer by
`branch-{property}` with a viridis LUT (`inst/py/napari_utils.py:673-700`); the current bridge has
colour-by helpers but no branch path. Also: a branch-length/tortuosity summary plot spec. Both
optional, both after the core lands.

---

## Risks

1. **Decision 2's third granularity is the real unknown.** Everything else is bounded. If review
   pushes back, the fallback is Decision 1's sidecar + reading branches through the existing `labels`
   pop_type (ungated, no filter pops) — less capable, much cheaper, and reversible later.
2. **The `/3` fix changes numbers.** Existing 2D `calcExtended` output is not comparable with
   post-fix output. Needs saying in the release notes, not just here.
3. **ILEE anisotropy is slow** (patches 3/4 disable numba). Unmeasured on real image sizes — measure
   in Phase 3 before advertising `calcExtended` as routine.
4. **ILEE is unmaintained upstream.** We own this code from now on. No upstream will fix the next
   numpy/skimage break; the `PATCHES.md` ledger is what makes that survivable.
5. **`skan.summarize` separator flip** is a scheduled upstream change. Pinning `separator='-'`
   defuses it, but the obs column names (`branch-type`, …) are then ours to maintain against a
   library that has moved on.

## References

- Old implementation: `old-R-shiny-version/inst/modules/sources/segment/{createBranching.R,py/create_branching.py}`
- Old input spec: `old-R-shiny-version/inst/app/modules/inputDefinitions/segment/createBranching.json`
- Old branch pop plumbing: `R/cciaImage.R:1311, 2371-2393, 3008, 3040`; consumers
  `inst/modules/sources/spatialAnalysis/{networkWeights.R,cellsToStructuresWO.R}`
- skan: <https://skeleton-analysis.org/stable/> (BSD-3)
- ILEE_CSK: <https://github.com/phylars/ILEE_CSK> (MIT; Li et al., *Plant Cell* 2023) — unmaintained
- Patterns to follow: `app/src/tasks/tracking/bayesian_tracking.jl` (pop membership → Python),
  `app/src/tasks/segment/cellpose.jl` (labels registration + QC),
  `python/cecelia/utils/measure_utils.py:359-392` (h5ad creation contract)
- Related: `docs/POPULATION.md`, `docs/SEGMENTATION.md`, `docs/todo/CENTROID_AXES_PLAN.md`,
  `docs/todo/SPATIAL_REGIONS_PLAN.md` (the `region` pop type — the closest precedent)
