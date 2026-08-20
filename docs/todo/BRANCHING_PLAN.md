# Branching (skeleton) analysis — port plan

**Status: SHIPPED** — `segment.branching` is built and registered (`app/src/tasks/segment/branching.{jl,json}`
+ `branching_run.py`, `task_registry.jl`). Kept as history for the rationale + locked decisions; the
durable documentation was promoted to `docs/SEGMENTATION.md` → *Branching (skeleton analysis)* and
`docs/POPULATION.md` → the `branch` pop type. Known UI gap: `docs/FUTURE.md` → *Branch populations in
the UI*. Audited 2026-07-27 before implementation.
**Ports:** `old-R-shiny-version/inst/modules/sources/segment/createBranching.R` (+ its
`py/create_branching.py` and `inst/app/modules/inputDefinitions/segment/createBranching.json`).

## Goal

`segment.branching` — skeletonise an existing segmentation into a **branch/path network**, measure each
path (length, tortuosity, branch type, endpoints), and expose the paths as a first-class population
type so branches can be plotted, gated against, and used as a spatial reference for cells.

Scientific target: fibrous / reticular structures that are *not* cells — collagen (SHG), nerves,
stromal networks, FRC/CCL19 reticular networks. The old version used it for exactly this
(`mxIBEX` stroma, `behaviourTumour` SHG, `mxCCL19`, `stomics_10x_breast_cancer`).

Anisotropy (the quiver-plot inputs the old `calcExtended` produced) is in scope — but via
`skimage.feature.structure_tensor`, not the vendored ILEE_CSK. See Decision 4.

---

## What's being ported vs rebuilt

Verified 2026-07-25 against the current pixi env (Python 3.12.13, numpy 2.0.2, scipy 1.18.0,
scikit-image 0.26.0, numba 0.65.1). Audited 2026-07-27 for cohesion.

**Ported (near-literally):**
- The skeletonisation shape: mask → optional Z-MIP → optional borders → binary_closing →
  `skimage.morphology.skeletonize` → `skan.Skeleton` + `skan.summarize` → per-timepoint labels
  zarr. **`skan` is healthy** — conda-forge 0.13.1 (2026-02-06, BSD-3), still emits the columns
  the old code consumed (`branch-type`, `node-id-src/dst`, `branch-distance`, `image-coord-*`).
  Only change: pass `separator='-'` explicitly to defuse a scheduled default flip.
- The per-branch-type filter-pop pattern (`ensure_filter_pop!` per unique `branch-type`).

**Rebuilt (the old Python is genuinely ad-hoc):**
- The runner (`create_branching.py`, 433 LOC) is a rewrite, not a copy. It has debug-log spam left
  in prod (`logfile_utils.log(ref_pops)` × 6), a `# TODO this will propagate the 2D image into 3D`
  hack that replicates a 2D result into every Z slice to preserve dimensions, half-commented mesh
  code, and a Python-side population lookup we're moving to Julia (Decision 7). Half the old
  params are dropped or moved (Decisions 5, 7, 8).
- **Anisotropy** — dropped from ILEE_CSK vendoring, rebuilt on `skimage.feature.structure_tensor`
  (Decision 4). Of ~2314 LOC in ILEE_CSK, cecelia only ever reached ~300 LOC (the anisotropy
  internals + a scalar summary threaded through scipy/skan). skimage's maintained structure-tensor
  primitive plus ~50 LOC of aggregation replaces it. See Decision 4.

**Out of scope (deliberately):**
- The downstream spatial-analysis tasks `spatialAnalysis/networkWeights.R` (99 LOC) and
  `spatialAnalysis/cellsToStructuresWO.R` (531 LOC) consume branch outputs and are what the mxIBEX
  workflow actually finishes with. They are **not** part of this port — port them when someone asks.
- The separate `segment.ilee` task (`ilee.R` + `ilee_wrapper.py` + `ilee_utils.py`, ~260 LOC) is a
  filament *thresholding* task that uses `ILEE_CSK.ILEE_2d` directly. Not touched here; if
  someone needs it, port against the actual ILEE algorithm at that point.

**Upstream dependency (NOT in this PR, but load-bearing for real use):** the segmentation input to
branching, on real fibrous images (dendritic cells, SHG collagen, FRC networks), was produced by
the custom Cellpose model **`ccia.fluo`** in the old R version (`inst/models/cellposeModels/ccia.fluo`,
~26 MiB). **RESOLVED** (2026-08-05): custom checkpoints are drop-in. The model picker is populated at serve time
from `list_cellpose_models()`, any name outside cellpose's built-ins resolves through
`cellpose_model_path` (user dir → bundled dir) and is loaded by the Python runner as
`CellposeModel(pretrained_model=<path>)`; `install.sh`/`install.ps1` fetch `ceceliaModels` at install
time and `pixi run models-fetch` does the same for a clone. So the real workflow — "segment SHG →
branch it" — is available to a new user. See `docs/SEGMENTATION.md` → *Custom cellpose checkpoints*.

---

## Locked decisions (2026-07-27)

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

**Cost, measured against the current code (audit 2026-07-27):** The framework was deliberately
generalised for third pop_types (`ACCEPT_TOKENS` is a tuple, `POP_MAP_SUFFIX` a Dict,
`pop_category`/`_accept_permits`/`_accept_pop_types` are switch/dispatch). Adding "branch":

| File | Sites | LOC |
|---|---|---|
| `population_manager.jl` | `POP_MAP_SUFFIX` (+1), `ACCEPT_TOKENS` (+1), `_accept_permits` (+1 arm), `_accept_pop_types` (+1 arm), `pop_category` (+1 arm), `pop_df` routing to `img_branch_props_path` + cache-key mtime (+1 real edit), `plot_pop_types`/`scope_pop_types` (+1-2 arms) | ~10 |
| `image.jl` | `BRANCH_PROPS_SUFFIX`, `img_branch_props_path`, `is_reserved_value_name` | 3 |
| `frontend/src/utils/popGroups.ts` | `GRAN_LABEL['branch']`, ORDER | 2 |
| `frontend/src/stores/gating.ts` | popType union (comment + any `'track'` special-case that should read "non-cell") | 1-2 |
| `app/test/runtests.jl` | round-trip + accepts + dispatch + `pop_df` shape | ~10 assertions |

Total ~10 code sites + ~10 test assertions. Region (the closest precedent) cost 16+23 because it
also added the pooled `regions.{suffix}` column + cluster-sibling auto-share machinery; branch
needs none of that. The one load-bearing edit is `pop_df` learning to route by pop_type to the
branch sidecar; everything else is one-line dispatch.

The one subtlety: `is_track_pop` currently returns `Bool` and its ~5 in-file callers use
`!is_track ⇒ cell`. Branch is a third granularity, so those callers must learn it explicitly.
All 5 sites are inside `population_manager.jl` and are counted above.

### Decision 3 — Per-branch-type pops via `ensure_filter_pop!`, not bespoke code

The old task's tail auto-created one filtered pop per unique `branch-type`
(`createBranching.R:91-108`). `ensure_filter_pop!` (`population_manager.jl:1018`) is exactly this
mechanism and is documented as measure-agnostic — "a 0/1 flag (`> 0`), a probability (`≥ 0.5`) or a
category (`in […]`) all work". So the port is one call per branch type, idempotent on re-run, with no
new machinery. Name the pops by branch-type semantics (`endpoint-to-junction`,
`junction-to-junction`, `endpoint-to-endpoint`, `isolated-cycle`) rather than the old
`xfun::numbers_to_words` integers — the skan codes are stable and documented.

### Decision 4 — Anisotropy via `skimage.feature.structure_tensor`, NOT vendored ILEE_CSK

The old `calcExtended` path called `ILEE_CSK.analyze_actin_{2d,3d}_standard` and returned
`ilee_summary` + a 5-tuple of anisotropy arrays (`coor_list`, `eigval`, `eigvec`,
`box_total_length`, `box_anisotropy`) into the h5ad `uns`. The 7 vignettes that consume this only
ever use the anisotropy 5-tuple (quiver plots) — none of them consume the `Diameter_tdt`/
`Diameter_sdt` fields where the `/3` bug lives.

**Replace with a fresh implementation on `skimage.feature.structure_tensor`:**

```python
# ~50 LOC in branching_run.py — no vendoring
Sxx, Sxy, Syy = skimage.feature.structure_tensor(img_channel, sigma=sigma)
# aggregate to a box grid, eigh per box → coor_list, eigval, eigvec, box_length, box_anisotropy
```

- Algorithm: local structure tensor of the fibre channel over a box grid → per-box eigendecomposition
  → eigenvectors give the principal direction (quiver arrows), eigenvalues give the anisotropy
  magnitude. ~300 LOC of unmaintained ILEE code with numba disabled is replaced by skimage's
  `structure_tensor` primitive, which is maintained, in-env, and 2D+3D.

  > **CORRECTION (2026-07-29, `docs/todo/SPATIAL_ANISOTROPY_PLAN.md` finding A1).** This bullet
  > originally read "This is what ILEE's `analyze_anisotropy_2d/3d` computes". It is **not**.
  > ILEE computes a **tangent tensor over the skeleton graph** (accumulate `outer(t̂,t̂)·length`
  > over skan edges); the structure tensor measures **intensity gradients**. Their principal
  > directions are **orthogonal**: the fibre runs along the structure tensor's *minor* eigenvector
  > and along the tangent tensor's *major* one (verified to ~1° on synthetic fields at 0/30/45/60/
  > 90/135°, `python/cecelia/tests/test_anisotropy_utils.py`). Always read the direction via
  > `cecelia.utils.anisotropy_utils.fibre_orientation`, never by indexing `orientation_eigvec`.
- Cite Li et al. *Plant Cell* 2023 (DOI + upstream URL) as the algorithmic ancestor at the
  function's docstring, per the "cite sources" rule. `THIRD_PARTY.md` gets a **skan** entry, not an
  ILEE one.
- Output keeps the old `uns` names.
  > **CORRECTION (2026-07-29) — both halves of this bullet were wrong, see
  > `docs/todo/SPATIAL_ANISOTROPY_PLAN.md`.** It originally promised "existing R notebooks read
  > post-port outputs unchanged". They do not: the eigenvalues are sorted **ascending** here vs
  > ILEE's descending, and the eigenvector array is stored **transposed** (rows vs ILEE's columns).
  > Shape-alike, index-incompatible. So the names were **renamed off the `ilee_` prefix** — they
  > were claiming a lineage the arrays don't have — to `orientation_coords` / `orientation_eigval` /
  > `orientation_eigvec` / `orientation_box_length` / `orientation_box_coherence` /
  > `orientation_summary`, plus an `orientation_meta` block recording the layout explicitly
  > (`eigval_order`, `eigvec_layout`, `fibre_direction`) so no reader has to guess. Note
  > `ilee_box_anisotropy` → `orientation_box_coherence`: it is per-box *coherence*, not the
  > per-image `anisotropy` scalar. The contract now lives in `docs/SEGMENTATION.md`.
  The summary scalar table (occupancy, cv, skewness,
  MF_full_length, branching_act, anisotropy) is recomputed inline from skan + scipy — ~30 LOC.
- Language: **Python**, inside `branching_run.py`. The skeleton and channel image already live in
  that process; a Julia handoff for a per-image compute would be pure overhead and no dep gets
  liberated (see coastal-denoise rationale: Julia moves are for hard lock-in, not code relocation).
- New param `structureTensorSigma` (float, default 2.0 px) replaces `anisoRadius`. Old task's
  `aniso_box_size = floor(radius/2)` becomes the aggregation box side.

**Rejected alternatives:** vendoring ILEE_CSK (Decision-4-original) — inherits 6 patches, one live
`/3` bug, upstream dead since 2024-04-22, `imp` unimportable on py3.12, `multichannel` removed in
skimage 0.26, `scipy.ndimage.morphology` removed in scipy 2.0, numba-disabled anisotropy path
"unmeasured on real image sizes". We own that code forever if we vendor. Skipped.

### Decision 5 — Drop `saveProps` / `saveMeshes`

The old script called `measure_utils.measure_from_zarr(...)` with `slices=`, `integrate_time=`,
`save_meshes=`, `calc_intensities=`. That function was **rewritten** — it is now
`MeasureUtils(params, dim_utils).measure_from_zarr(label_zarrs, im_dat, log)` and has none of those
arguments. Per-branch intensity measurement is **not part of this port**. If a user needs per-branch
channel means, that's a follow-up (see Decision 6 for why we can't just re-route `measureLabels`).

### Decision 6 — Branch labels live on a DEDICATED image field, NOT in the generic `labels` dict

The skeleton needs its own zarr on disk for napari display, but registering it as another entry in
`img.labels` reintroduces exactly the picker pollution Decision 1 avoids — every
`valueNameSelection` with `"field": "labels"` (measure, segment, tracking) would then offer
`{vn}.branch` alongside real cell segmentations. It would also mean a user could point
`segment.measureLabels` at branch labels, writing a **second** `labelProps/{vn}.branch.h5ad`
alongside the sidecar `{vn}__branch.h5ad` this plan defines — the picker-pollution + double-table
problem the previous draft flagged as an "open sub-question."

**Resolution:** add a dedicated field `img.branch_labels` (versioned, same shape as `img.labels`)
+ accessor `img_branch_labels_path(img, vn)`, and a napari-bridge branch that renders it as a
labels layer. Callers that need to display branch labels (napari overlay, future branch summary
plots) resolve through the dedicated field; the generic `labels` picker never sees branch labels.
`is_reserved_value_name` blocks `{vn}.branch` from being registered as a plain label set (defence
in depth).

**Cost:** roughly parallel to how `track_props` sits beside `label_props` — one accessor + one
versioned field on `CciaImage` + one napari-bridge dispatch site. Cheap, and it locks Decisions 1
+ 5 + 6 into a mutually consistent set (no more "resolve before Phase 2").

**Deferred capability:** per-branch channel intensities. Doable later by teaching
`branching_run.py` to accept `intensityChannels` and fold means into `{vn}__branch.h5ad` directly
(the old `saveProps` behaviour, but inside the sidecar contract). Note it in `docs/FUTURE.md`
when the plan ships.

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

## Files

```
app/src/tasks/segment/branching.jl            NEW  struct + _run_task
app/src/tasks/segment/branching.json          NEW  param spec
app/src/tasks/segment/branching_run.py        NEW  skeletonise + skan + structure_tensor
app/src/tasks/task_registry.jl                EDIT _spec_path + "segment.branching" => Branching()
app/src/model/image.jl                        EDIT BRANCH_PROPS_SUFFIX, img_branch_props_path,
                                                   img_branch_labels_path, versioned branch_labels
                                                   field, is_reserved_value_name
app/src/gating/population_manager.jl          EDIT `branch` pop type (Decision 2 table)
app/src/qc_cohort.jl                          EDIT COHORT_METRICS["segment.branching"]
app/src/Cecelia.jl                            EDIT exports
api/src/napari_api.jl                         EDIT branch-labels display path (if needed)
pixi.toml / pixi.lock                         EDIT skan
THIRD_PARTY.md                                EDIT skan entry (BSD-3, Nunez-Iglesias 2018)
frontend/src/utils/popGroups.ts               EDIT branch granularity label + order
app/test/runtests.jl                          EDIT pop-map round-trip, accepts, dispatch,
                                                   param validation, branch_labels round-trip
docs/SEGMENTATION.md                          EDIT branching section
docs/POPULATION.md                            EDIT branch pop type
docs/FUTURE.md                                EDIT deferred per-branch intensities note
INVENTORY.md                                  EDIT branch props + branch labels accessors
```

**No vendor directory**, no `PATCHES.md`, no ILEE THIRD_PARTY entry — Decision 4 kills all of that.

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
| `calcAnisotropy` | bool | run structure-tensor anisotropy → `uns` (Decision 4) |
| `calcFlattened` | bool | run anisotropy on a Z-MIP (2D mode) for a 3D image |
| `structureTensorSigma` | float 0.5–20, default 2.0 | Gaussian window for structure tensor (Decision 4) |
| `anisotropyBoxSize` | int 5–200, default 45 | aggregation box side for eigendecomp grid (Decision 4) |
| ~~`calcExtended`~~ | — | renamed to `calcAnisotropy` (Decision 4) |
| ~~`anisoRadius`~~ | — | replaced by `structureTensorSigma` + `anisotropyBoxSize` (Decision 4) |

> **CORRECTION (2026-07-30).** Those two params are now **`structureTensorSigmaUm`** (7 µm) and
> **`anisotropyBoxUm`** (5 µm) — physical, not pixels, converted per-image by the handler. New keys,
> so a saved pixel value cannot be reread as µm. See `docs/SEGMENTATION.md` for how to choose them.
| ~~`saveProps`~~ | — | dropped (Decision 5) |
| ~~`saveMeshes`~~ | — | dropped (Decision 5) |
| ~~`popType`~~ | — | subsumed by `popSelection`'s value-prefixed refs |

`resource_pool: "cpu"`. Skeletonisation is single-threaded CPU; structure_tensor is
convolution-based and fast (no numba-disabled loop like the old ILEE anisotropy).

## QC (mandatory — `docs/MODULES.md`)

Metrics: `nBranches`, `meanBranchLength`, `nSkeletons`. Advisory finding: `branching.no_branches`
(warn) when the skeleton yields zero paths — the one unambiguous failure (empty/over-eroded input).
Keep the finding logic in a pure helper next to `_segment_qc_findings` and unit-test it. Register
`COHORT_METRICS["segment.branching"] = ["nBranches", "meanBranchLength"]`.

Finding text per `docs/MODULES.md` (short = the problem, long = the action, imperative):
short `"No branches found"`, long `"Lower the pre-dilation or check the segmentation, then re-run."`

## UI compliance

This port adds **no new chrome** — that is the point of Decision 2's cost table. Still, three current
rules bind the frontend edits:

- **UX-primitive catalog is mandatory *and* test-enforced** (`docs/ui/PRIMITIVES.md`).
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

**Phase 0 — dep + smoke test.** `pixi add --pypi skan` (**not** `pixi add skan` — conda-forge
`skan` resolves numpy to 2.4.6, which violates cellpose 3's `numpy<2.1` ceiling, making the conda
solve unsatisfiable. PyPI-side skan respects the existing numpy pin, so it lands in
`[pypi-dependencies]` next to cellpose/anndata/scanpy). Pixi picked `>=0.13.1, <0.14`. No
vendoring. Checkpoint: `pixi run test-py` green; a Python unit test imports skan, runs
`Skeleton` + `summarize` on a synthetic image with `separator='-'`, asserts the expected column
set is present.

**Phase 1 — core branching (skeleton only, no pops, no anisotropy).**
`branching.jl` / `.json` / `_run.py`: mask → optional flatten → optional borders → per-timepoint
close/skeletonise/dilate → labels zarr + pyramid → `skan.summarize` paths table →
`{vn}__branch.h5ad` (Decision 8) → dedicated `branch_labels` field on the image (Decision 6) →
QC. Registry entry. Checkpoint: runs end-to-end on a real fibrous image; `pixi run test-pkg` green
(dispatch + param validation + a bad-param `ParamValidationError`); `pixi run test-py` green.

**Phase 2 — the `branch` pop type.** Decision 2's table + `ensure_filter_pop!` per branch type
(Decision 3). Checkpoint: branch pops appear under a "Branches · Gated" header in a
`popSelection`; pop-map round-trips; `pop_df` returns branch rows.

**Phase 3 — anisotropy via structure tensor.** Wire `calcAnisotropy` / `calcFlattened` /
`structureTensorSigma` / `anisotropyBoxSize` into `branching_run.py`; ~50 LOC that computes
`skimage.feature.structure_tensor` → box-aggregate → `eigh` → the 5 arrays
(`coor_list`/`eigval`/`eigvec`/`box_total_length`/`box_anisotropy`) + a scalar `ilee_summary` from
skan/scipy; write into `uns`. Cite Li et al. 2023 in the docstring. Checkpoint:
`calcAnisotropy=true` on a 2D and a 3D image; `uns` shapes match what
`behaviourTcells3P.Rmd`/`behaviourTumour.Rmd` expected; per-image runtime measured and reported
(with numba out of the picture this should be materially faster than the old ILEE path).

**Phase 4 — napari + plots (follow-up).** Old version colour-mapped the branch labels layer by
`branch-{property}` with a viridis LUT (`inst/py/napari_utils.py:673-700`); the current bridge has
colour-by helpers but no branch path. Also: a branch-length/tortuosity summary plot spec. Both
optional, both after the core lands.

Ship all four as one PR per the "finish feature before opening PR" rule.

---

## Risks

1. **Decision 2's third granularity.** Now bounded — ~10 code sites + ~10 test assertions, all
   inside pre-generalised dispatch (`ACCEPT_TOKENS`, `POP_MAP_SUFFIX`, switch-per-pop_type). Not
   the biggest risk anymore. Fallback: sidecar + read via existing `labels` pop_type (ungated, no
   filter pops) — much cheaper, reversible later. But note that fallback kills Decision 3
   (`ensure_filter_pop!` per branch-type) which is the killer app of the whole task.
2. ~~**Structure-tensor output shape parity.**~~ **Retired 2026-07-29.** The premise — that R
   notebooks index `x$ilee_coor_list[1,,,1]` and should keep working — does not survive the
   correction above: the arrays are index-incompatible whatever they are called, so shape parity
   bought nothing and the names now say `orientation_*`. The fixture-with-known-geometry half of
   this risk WAS acted on: `test_anisotropy_utils.py` holds both estimators to synthetic fields at
   0/30/45/60/90/135° (5° tolerance).
3. **`skan.summarize` separator flip** is a scheduled upstream change. Pinning `separator='-'`
   defuses it, but the obs column names (`branch-type`, …) are then ours to maintain against a
   library that has moved on.
4. **No parity of numbers with old projects.** Existing `calcExtended` outputs (the summary table,
   the anisotropy 5-tuple) were computed by ILEE's specific box-tensor formulation with the `/3`
   bug in the 2D path. The new structure-tensor path will produce different numbers — better
   (bug-free), but not comparable pre/post. Say so in release notes.
5. **Sigma/box tuning.** ILEE hardcoded radius/box (150/75 in 2D, 50/25 in 3D). Structure-tensor
   sigma controls a Gaussian window rather than a hard box; sensible defaults need to be picked
   against the fibrous-image examples the vignettes used (SHG collagen, stroma). Do this in
   Phase 3 before advertising the param.

## References

- Old implementation: `old-R-shiny-version/inst/modules/sources/segment/{createBranching.R,py/create_branching.py}`
- Old input spec: `old-R-shiny-version/inst/app/modules/inputDefinitions/segment/createBranching.json`
- Old branch pop plumbing: `R/cciaImage.R:1311, 2371-2393, 3008, 3040`
- Old downstream consumers (NOT in scope): `inst/modules/sources/spatialAnalysis/{networkWeights.R,cellsToStructuresWO.R}`
- skan: <https://skeleton-analysis.org/stable/> (BSD-3, Nunez-Iglesias 2018)
- `skimage.feature.structure_tensor`: <https://scikit-image.org/docs/stable/api/skimage.feature.html#skimage.feature.structure_tensor>
- Anisotropy algorithmic ancestry: Li et al. *Plant Cell* 2023 (ILEE_CSK — no longer vendored, cited)
- Patterns to follow: `app/src/tasks/tracking/bayesian_tracking.jl` (pop membership → Python),
  `app/src/tasks/segment/cellpose.jl` (labels registration + QC),
  `python/cecelia/utils/measure_utils.py:359-392` (h5ad creation contract)
- Related: `docs/POPULATION.md`, `docs/SEGMENTATION.md`, `docs/todo/CENTROID_AXES_PLAN.md`,
  `docs/todo/SPATIAL_REGIONS_PLAN.md` (the `region` pop type — the closest precedent)
