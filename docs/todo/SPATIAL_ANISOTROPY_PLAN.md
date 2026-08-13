# Structure anisotropy — branching-port audit + the notebook readouts

**Status:** 2026-07-29 — **done, uncommitted.** Worktree `spatial-anisotropy`, branch
`feat/spatial-anisotropy`. Supersedes `docs/archive/spatial-anisotropy-quiver-prompt.md`.
**Depends on:** `docs/todo/BRANCHING_PLAN.md` (Phases 0–3 shipped, PRs #387 + #396).

## What this is

Two readouts from the 3P behaviour paper's **Figure 4**, made computable and reachable:

- **B — the quiver.** A dense arrow field of SHG collagen directionality with cell tracks drawn
  over it, plus the branch network coloured by branch type.
- **D — anisotropy per image.** One scalar per image, *"SHG Anisotropy (1 = non-uniform)"*,
  scattered against the % of that image's tracks in each behaviour state.

**Neither is an app plot.** Both were built on the spatial module page, did not work there, and
were reverted (Dominik, 2026-07-29). They are figure-shaped, so they belong in a **Notebook**; the
app's job is to compute and store the numbers correctly, which is what this branch does. The
recipe is `docs/NOTEBOOKS.md` → *Structure anisotropy*; the stored contract is
`docs/SEGMENTATION.md`.

Getting there meant auditing the anisotropy pass, which turned out to be substantially wrong —
that audit (A1–A8) is the bulk of this document and the bulk of the change.

**The legacy implementation is a reference, not gospel** (Dominik: parts of it were eyeballed
rather than validated). Correctness is established against synthetic fields of known orientation;
agreement with legacy only *explains* a difference, it never passes or fails anything.

---

## What shipped

**Scope line: the Cecelia package, the branching task, the Python IO library, docs and tests.**
No API route, no Vue component, no plot definition. Anything that existed only to serve the
reverted UI was reverted with it.

| | |
|---|---|
| `python/cecelia/utils/anisotropy_utils.py` | NEW — all the orientation maths, out of the runner so it is testable and shareable |
| `app/src/tasks/segment/branching_run.py` | A1–A8 fixes, `anisotropySource`, `integrateTime`, per-branch `anisotropy`, `orientation_meta` |
| `app/src/tasks/segment/branching.{jl,json}` | retuned defaults, new params, `anisotropy` QC metric |
| `python/cecelia/utils/zarr_utils.py` | `create_multiscales(..., axes=)` — A8 |
| `app/src/label_props.jl` | `uns_keys` / `uns_array` / `uns_dict` / `uns_df` |
| `app/src/anisotropy.jl` | NEW — `quiver_df` / `branch_segments` / `anisotropy_df`, the notebook entry points |
| `app/src/model/image.jl` | `img_branch_value_names` |
| `app/src/qc_cohort.jl`, `mcp/…` | the per-image `anisotropy` metric + the ratio-vs-count note |

**Renamed the `uns` keys `ilee_*` → `orientation_*`** (Dominik, 2026-07-29). Two reasons: the old
names claimed an ILEE lineage the arrays do not have (A1), and `ilee_box_anisotropy` was per-box
*coherence*, easily confused with the per-image `anisotropy` scalar. `orientation_` also avoids
the `aniso_anisotropy` stutter an `aniso_` prefix would have produced. Runs banked before the
rename keep the old keys — re-run the task.

**Per-branch `anisotropy`** is an obs column on `{vn}__branch.h5ad`, so it is a measure on branch
populations. On EaMaVq the medians separate by branch type (0.397 endpoint-to-endpoint, 0.222
endpoint-to-junction, 0.185 junction-to-junction), which a per-image scalar cannot show.

### Deliberately NOT built

- Any plot on a module page or the analysis board, and the routes/registry entries for one.
- A generic QC-measure plot source (built, then reverted).
- A `quiver_tracks` accessor — `pop_df` already returns exactly what a track overlay needs.
- Field-vs-field angle correlation + `binariseTracks` (Decision 11).

### Known limitation — the `branch` pop type has no working UI surface

Branch pops exist on disk and `pop_df(img, "branch", …)` reads them, but neither the gating page
nor the plot picker can reach them: `/api/gating/channels` and `/api/plots/populations` both
enumerate `label_props` value_names, and a branch value_name is a *segmentation* (`SHG`) that
usually has no per-cell table. Dominik also saw the four branch types listed **flat** rather than
as subpopulations of one `SHG` pop.

Partial fixes for this were written and then reverted here, deliberately: repairing one link of a
chain that is broken end-to-end just hides the breakage. `img_branch_value_names` (the piece those
fixes needed) is kept, since `anisotropy_df` uses it. If branch pops should be reachable in the
UI, that is its own change — see the trigger in `docs/FUTURE.md`.

---

## Branching-port audit (verified on project `4kS67f` image `EaMaVq`)

`EaMaVq` — 201 T × 4 C × 20 Z × 544 × 548, drift-corrected active, channels
`THG / Tcells-uGFP / Bcells-ubiTom / SHG`, `SHG` segmentation + `SHG` branch labels + `T`/`B`
tracked cells.

**Healthy baseline.** `SHG__branch.h5ad`: 47,680 branches, `obsm['spatial'] (47680,3)` +
`obsm['temporal'] (47680,1)`, branch-type mix `{0: 22716, 1: 18455, 2: 6428, 3: 81}`. A plausible
four-way distribution confirms the PR #396 dilation-order fix holds on real data (the corrupted
run had only types 1/2).

### A1 — The eigenvector convention is orthogonal to ILEE's, and the code claimed otherwise

The runner claimed its `uns` layout was "deliberately compatible with the old `uns` layout so R
notebooks that indexed `x$ilee_coor_list[1,,,1]` still read post-port outputs", and that the
row-major eigvec layout "matches the old ILEE convention". Neither holds.

ILEE is **Implicit Laplacian of Enhanced Edge** (Li et al., *Plant Cell* 35:371, 2023), and its
anisotropy (`ILEE_CSK/functions.py:757-860`) is **not** an intensity structure tensor — it is a
**tangent tensor over the skan skeleton graph**: for every edge within `radius` of a grid point,
accumulate `outer(t̂, t̂) · edge_length`, then eigendecompose. Intensity never enters. Measured on
synthetic fibre fields at four known orientations (0/30/60/90°):

| | eigenvector pointing **along** the fibre |
|---|---|
| `skimage.feature.structure_tensor` (shipped) | the **minor** one — 0.0–1.3° error |
| ILEE tangent tensor (legacy) | the **major** one — 0.0–1.2° error |

The shipped *major* eigenvector is the gradient direction, **90° across** the fibre (measured
88.7–90.0°). The repo's own unit test asserted exactly this, so the implementation was
self-consistent — the *documentation* was wrong, which is the dangerous half: it invited the next
author to reuse the vignette's "take the first eigenvector" formula and get a quiver rotated 90°,
silently. Two further deviations behind the same claim: eigenvalue sort flipped (ILEE descending →
`eigh` ascending) and the eigvec array transposed (ILEE stored numpy's columns, the port swaps to
rows). Net: index `0` still happens to be the fibre direction in both, but arrow **lengths**
invert and the vignette's `eigvec[1,,,2,1]` indexing reads the wrong axis. Shape-compatible,
semantically incompatible — nothing errors.

**Fix:** one named helper per convention — `fibre_orientation` (structure tensor, minor) and
`tangent_orientation` (tangent tensor, major) — with the same `(direction, coherence)` contract, so
a call site cannot pick wrong. `quiver_df` is the Julia-side equivalent. The compatibility claim is
deleted from the docstring and from BRANCHING_PLAN; the keys are renamed off `ilee_*`.

### A2 — The shipped defaults produced a near-random direction field

**Coherence magnitude is not the quality metric** — checked, dead end. The legacy tangent-tensor
field on this image scores mean normalised anisotropy **0.175**, the same ballpark as the new
path's 0.13–0.16, and the legacy quiver read fine, because `ggquiver` auto-scales the whole field
to the grid — the eye reads *direction*, not eigenvalue magnitude. Panel D settles it
independently: the published anisotropy axis runs **0.1–0.4**. A coherence gate would have
rejected the very thing being rebuilt.

Two concrete defects, against the legacy field's ~22° neighbour consistency:

- **σ=2.0 was ~6× too small.** The legacy integration radius was 50 px (`anisoRadius`), giving
  `box = 25`. σ=2 scored 38.6° — barely better than random (45° is noise). σ≈12 recovers
  legacy-like smoothness.
- **`anisotropyBoxSize=45` was ~2× the legacy grid.** Panel B's own arrow spacing (~24×20 arrows
  over ~550 px) independently confirms ~25.

Feeding the tensor the segmentation **mask** or **skeleton** rather than raw intensity both
smooths the field and moves it much closer to the legacy field (23–27° vs 38°) — unsurprising,
since the legacy estimator was skeleton-only. Hence Decision 5.

### A3 — `flattenBranching` + `calcAnisotropy` without `calcFlattened` wrote mismatched shapes

With `flattenBranching=true` on a 3D image the labels are Z-MIPed so the skeleton is 2D, but
`_extract_fibre_image` still returned the full 3D stack. The box tensor was then a 3D grid while
`box_total_length` was 2D. No exception — it just landed in `uns` with a rank its four siblings
don't share. Fixed by `_match_rank`.

### A4 — Anisotropy silently dropped timepoints, breaking the T axis

The `if calc_anisotropy` block sat after `if df.empty: continue`, so a frame with zero skeleton
paths was skipped for anisotropy too — making the stacked leading axis "index among non-empty
frames", not `t`. Any consumer indexing `eigvec[t]` read the wrong frame the moment one frame was
empty. Fixed: the pass runs on every frame, and `orientation_meta["t_index"]` records the real
mapping explicitly.

### A5 — The per-image scalar used the wrong definition, and nothing consumed it

`aniso_scalar = ban.mean()` — an unweighted mean over **all** boxes including empty ones. The
legacy scalar (`analyze_anisotropy_2d`, `weighting_method='by_length'`) is
`sum(box_anisotropy) / sum(box_total_length)`: a **length-weighted** mean, insensitive to how much
blank field the image contains. Measured on EaMaVq at legacy-parity settings:

| T | 0 | 50 | 100 | 150 | 200 |
|---|---|---|---|---|---|
| unweighted (shipped) | 0.274 | 0.350 | 0.372 | 0.418 | 0.367 |
| **length-weighted (legacy)** | **0.210** | **0.258** | **0.324** | **0.359** | **0.307** |

And nothing read it: no QC metric, no cohort entry, no napari layer, no plot, no MCP.
`calcAnisotropy` was compute that produced a file nobody opened. Decision 6 gives it a consumer.

### A6 — Minor

- A `np.concatenate([sk_obj.paths.indices, np.array([], …)])` no-op.
- `hasattr(dim_utils, "im_physical_size")` is always true, so the `1.0` fallback was dead and a
  missing scale surfaced as an exception rather than the intended default.
- `coor_list` is in **pixels** of the (possibly Z-MIPed) array with no scale recorded beside it →
  `scale_um_per_px` in `orientation_meta`.

### A7 — `flattenBranching` silently dropped the TIME axis from the branch table

Found by running the pass, not by reading it. `_write_branch_h5ad` was called with
`has_time and not flatten_branching`. But `flattenBranching` is a **Z** operation — the runner
still skeletonises every timepoint. So a Z-flattened timeseries produced **66,834 branches over
201 frames with no `obsm['temporal']`**: no way to tell which frame any branch came from. That is
the standard intravital case, and it makes panel B impossible on it. Fixed by passing `has_time`
directly; `integrateTime` is the flag that legitimately suppresses the temporal axis.

### A8 — The branch labels zarr declared the wrong axes, so its Y scale was the Z step

Also found by running it, and it predates this work — it affects **every branching run to date**.
`create_multiscales` took the store's axes and per-axis scale straight from `dim_utils`, i.e. from
the SOURCE IMAGE. The branch labels store is not the source image: it never has C, and loses Z
under `flattenBranching` (and T under `integrateTime`). Result on EaMaVq: a 3-axis
`(201, 544, 548)` array tagged `t,c,z,y,x` with scale `[1.0, 1.0, 3.0, 0.596, 0.596]`. Read
positionally, **Y got the 3 µm Z step — a 5× stretch**, in napari and in any µm conversion.

The chunk vector was wrong the same way and merely survived: `create_zarr_from_ndarray` has an
`if len(shape) != len(chunks): chunks.pop(0)` fallback that drops a *leading* entry, so the
remaining chunk sizes land on the wrong axes rather than erroring.

Fixed with an explicit `axes=` override (scale then mapped by axis NAME, never zipped
positionally) and chunks computed from the stored array. Both pinned, including a no-override case
so every other caller is provably unaffected.

---

## Locked decisions

### Decision 1 — Fibre orientation is the structure tensor's minor eigenvector, behind one named helper

Keep `skimage.feature.structure_tensor` (BRANCHING_PLAN Decision 4 stands — ILEE_CSK stays
un-vendored): A2 shows it reproduces the legacy field once σ and box are right, so there is no case
for re-vendoring 300 LOC of unmaintained upstream to reach the same answer. But stop pretending the
output is ILEE-compatible — see A1.

**Rejected:** flipping the stored arrays to ILEE's descending/column convention to make the old R
formula work. BRANCHING_PLAN Risk 4 already accepted the numbers aren't comparable pre/post;
preserving *indexing* compatibility for numbers that mean something different is worse than none.

### Decision 3 — A self-describing `orientation_meta` block

```
orientation_meta = { box_size_px, sigma_px, source, scale_um_per_px (y,x[,z]), flattened,
                     t_index (one entry per stacked frame; [-1] = time collapsed),
                     eigvec_layout: "vec_major", eigval_order: "ascending",
                     fibre_direction: "minor" }
```

`t_index` is A4's fix — an explicit frame index beats an implicit one. The block is what makes the
arrays readable without reading `branching_run.py`.

### Decision 4 — Retune to legacy parity; score on field smoothness, never on coherence

**Final: `anisotropySource=skeleton`, σ = 12 px, box = 15 px** (from `channel` / 2.0 / 45).

> **Now expressed in µm (2026-07-30, Dominik).** Pixels are not a meaningful unit to a user and made
> a mixed-calibration cohort silently incomparable, so the params became `structureTensorSigmaUm`
> (default **7 µm**) and `anisotropyBoxUm` (default **5 µm**) — the same settings at EaMaVq's
> 0.596 µm/px, converted by the Julia handler. NEW KEYS deliberately: reusing the old ones would let
> a project's saved pixel values (2, 45) be reread as µm, i.e. ~3 px and ~75 px, silently.
> A follow-up scan showed finer is strictly better here down to ~5 µm with no noise penalty (σ does
> the smoothing), so the only cost is file size — which the run now logs and warns on.

**The objective had to be fixed first.** A2 proposed scoring on *neighbour consistency*. That is
invalid — it is monotonically improved by blurring: σ 2 → 25 drove it 43.5° → 17.2° while coherence
collapsed 0.32 → 0.13, so the "best" score was an oversmoothed field that had stopped describing
local structure. Replaced by `direction_contrast`: the gap between how much NEARBY boxes agree and
how much DISTANT ones do. Oversmoothing drives both to 0, noise drives both to 45°, and only real
spatial structure separates them. (Second correction: the "far" separation must be a fixed
*physical* distance, not a fixed number of boxes, or contrast isn't comparable across box sizes.)

Scan over source × σ ∈ {2,6,12,25} × box ∈ {15,25,45}, EaMaVq + the Y7oL9h crop, contrast at a
fixed 90 px separation:

| | contrast | near | anisotropy |
|---|---|---|---|
| old defaults (channel, σ=2, box=45) | 2.3 | 38.0° | 0.14 |
| **chosen (skeleton, σ=12, box=15)** | **26.7** | **18.1°** | **0.22** |
| legacy tangent tensor, box=15 | 26.1 | 16.7° | 0.18 |
| skeleton, σ=25, box=15 | 31.5 | 9.2° | 0.14 |

Two judgement calls worth knowing:

- **box=15, not the legacy-parity 25.** box=15 wins for every source on both images, and the
  *legacy itself* scores better at 15 (26.1) than at its own default 25 (22.3) — so legacy's box
  was not optimal either, consistent with it having been eyeballed.
- **σ=12, not σ=25**, even though σ=25 scores higher contrast. At σ=25 the near-angle falls to 9°,
  well below the legacy field's ~17° regime, and coherence drops to 0.14 — the smoothing regime
  again, just not fully caught by contrast. σ=12 sits in the same regime as the reference field
  while matching its contrast.

**Test-data caveat.** The second scan point (`Y7oL9h`) is a *crop of EaMaVq* — different geometry,
same underlying data, so it corroborates rather than generalises. There is no 2D static fibrous
image in the dev projects, and the one other candidate (`zolIMa/ldYr8J`) has a near-flat SHG
channel (0–23, mean 14.6) with no segmentation. Revisit these defaults when a genuinely
independent fibrous dataset exists.

`tangent_tensor_field` ships in `anisotropy_utils` (not the test tree) so any legacy-vs-now
question is a function call — `compare_fields(image, skeleton, sigma, box)` — rather than an
argument. It requires a THIN skeleton: passing it a thick mask misses a known 30° field by 59°.

### Decision 5 — The anisotropy input is selectable: `channel` / `mask` / `skeleton`

Default **`skeleton`** (legacy parity, best measured). Raw intensity stays available because it is
the only input that survives a bad segmentation and the only one that can describe structure
nobody segmented. All three share one code path — only the input array changes.

### Decision 6 — Per-image anisotropy is a QC metric, length-weighted

Banked via `write_qc` **only when the pass actually ran** (a structural 0.0 would poison the
cohort) and listed in `COHORT_METRICS`, which gets it into the QC surface, the cohort outlier pass
and MCP `get_qc_metrics` for free. For a timeseries, the median over frames; the per-frame series
stays in `orientation_summary`. Labelled **"Anisotropy (1 = non-uniform)"** — the published wording.

`anisotropy` is the first *ratio* metric in a cohort list otherwise made of counts, so the outlier
rule was checked at real magnitudes: `{0.31, 0.33, 0.30, 0.09}` flags the 0.09; a cohort merely
spanning the normal band `{0.12, 0.21, 0.30, 0.38}` flags nothing. Both pinned.
**Not demonstrated on a real multi-image cohort** — only EaMaVq (and its crop) has an SHG
segmentation, so there is no set of ≥3 independent images to aggregate over.

### Decision 9 — T-collapse is a param on branching, not a new task

`integrateTime` (bool) + `integrateTimeMode` (`max` | `avg`), default off — a pre-processing step
on the label stack before skeletonisation, exactly as `flattenBranching` is for Z. Legacy names
kept so legacy migration stays cheap. The LABEL stack always collapses by MAX (a union of where
structure existed); the *average* of a label image is meaningless, so the mode applies only to the
raw channel. That asymmetry is on the param tip, not hidden.

Verified on EaMaVq, both modes end to end:

| | per-timepoint | `integrateTime=max` |
|---|---|---|
| branches / skeletons | 66,834 / 29,980 | 717 / 25 |
| anisotropy | 0.324 | 0.308 |
| store | `(201, 544, 548)` axes `t,y,x` scale `[1.0, 0.596, 0.596]` | `(544, 548)` axes `y,x` |
| `obsm['temporal']` | present, t=0–200 | absent (correct) |
| `t_index` | 0…200 | `[-1]` |

The two scalars agreeing to 0.016 across wildly different branch counts is a decent consistency
check on the readout.

### Decision 10 — `spatialAnalysis.network_weights` is a near-literal Julia port (NOT DONE)

Legacy `networkWeights.R`, 99 LOC: branch centroids → a cell/track population →
`dbscan::kNN(k=1)` cells→branches → count cells within `maxDist` per branch → write back to the
branch label-props. Every piece has a current equivalent: `pop_df` for both tables,
`NearestNeighbors.jl` for the kNN, `label_props |> add_obs |> save!` for the write. No Python.
Column name stays **`branch-weight`** (the napari colour-by path reads branch columns by name).
QC: `nWeightedBranches` + `meanBranchWeight`, warn when nothing got weighted.

Not part of Figure 4 — it is the other shipped-but-unported branch consumer, and it makes the
branch layer colourable by cell traffic. Its own change.

### Decision 11 — Field-vs-field angle correlation and `binariseTracks` are DEFERRED

The vignette (`behaviourTcells3P.Rmd:595-710`) runs the whole pipeline on the **track network**
too — `binariseTracks` rasterises a `live` population's paths into a label image, `createBranching`
skeletonises it, and the "angle correlation" is field-vs-field between the structure grid and the
track grid, rendered as `geom_tile`. **That is not in Figure 4.** Panels B and D answer "how do
tracks relate to the structure" with an overlay and a correlation; building a second
rasterise→skeletonise→eigendecompose pipeline for a tile map nobody asked for is speculative scope.
Recorded in `docs/FUTURE.md` with the trigger: *someone wants the per-box structure-vs-motion angle
map*. If built, note that the vignette's 0–180° range splits one physical alignment across both
ends of its scale — fold to 0–90°.

### Decision 12 — MCP surfaces summaries, not grids

`get_qc_metrics` picks up per-image `anisotropy` for free via Decision 6, with the ratio-vs-count
note so an LLM doesn't read "anisotropy 0.3" as a count and call every real image an anomaly. The
raw eigenvector grid is **not** an MCP payload — it scales with image size and box tuning, and the
observer is read-only summary-first by design.

---

## Remaining work

1. **Re-run `segment.branching` on EaMaVq with Anisotropy on** (skeleton, σ=12, box=15, flatten Z).
   Until then no image has the `orientation_*` block under its new names, so every notebook read
   comes back empty. This also regenerates the branch labels zarr with the corrected axes (A8).
   Dominik runs it from the GUI.
2. **Decision 10** — `network_weights`, if wanted.
3. **The branch pop-type UI gap**, if wanted — see *Known limitation* above.

## Risks

1. **The tuned defaults rest on one dataset** (EaMaVq + a crop of it). See Decision 4's caveat.
2. **A1's silent-90° trap survives anywhere the helper isn't used.** The whole mitigation is one
   helper per language (`fibre_orientation`, `quiver_df`); a hand-rolled `eigvec[…, 1, :]` in a
   notebook still goes wrong. Noted in `docs/SEGMENTATION.md` and `docs/NOTEBOOKS.md`.
3. **The `uns` key rename is breaking** for anything already banked. Nothing in-tree reads the old
   names, and no notebook exists yet, so the blast radius is Dominik's own EaMaVq run — which
   needs re-running anyway (item 1).
4. **`ccia.fluo` reaches a new user** (resolved 2026-08-05): the installers fetch `ceceliaModels` and
   the picker lists any checkpoint in the models dir, so "segment SHG → branch → quiver" from scratch
   is available. Verified by reading the chain end-to-end, not yet by running it on a fresh install.
5. **`segmentation_qc.json` declares `groupByOptions: ["t"]`** while the columns API reports
   `["centroid_t"]`, so its LOESS trend chart is unreachable. A one-word fix, deliberately NOT
   applied here — it would enable a chart nobody asked for. Its own change if the trend is wanted.

## References

- **Target figure:** `~/Downloads/Figure 4.pdf` (3P behaviour paper) — panels B and D
- Prompt this supersedes: `docs/archive/spatial-anisotropy-quiver-prompt.md`
- Prerequisite: `docs/todo/BRANCHING_PLAN.md` (Decision 4 needed the A1 correction)
- Legacy anisotropy: `old-R-shiny-version/inst/py/ILEE_CSK/functions.py:757-860`
  (`anisotropy_2d_internal`, `analyze_anisotropy_2d` — note `weighting_method='by_length'`)
- Legacy `uns` write: `.../inst/modules/sources/segment/py/create_branching.py:286-410`
- Legacy grid defaults: `.../inst/app/modules/inputDefinitions/segment/createBranching.json:55-60`
  (`anisoRadius` 50 → box 25)
- Legacy quiver + per-image scatter: `old-R-shiny-version/vignettes/behaviourTcells3P.Rmd:595-850`
  and `behaviourUbiTom3P.Rmd:200-560` (the `ilee_summary` → `exp.info$SHG.anisotropy` merge, and
  the `SHG.anisotropy` vs behaviour-freq scatters that ARE panel D)
- Legacy network weights: `.../inst/modules/sources/spatialAnalysis/networkWeights.R`
- Deferred (Decision 11): `.../inst/modules/sources/segment/{binariseTracks.R,py/binarise_tracks.py}`
- Anisotropy ancestry: Li et al., *Plant Cell* 35:371 (2023), doi:10.1093/plcell/koac290
- Related: `docs/NOTEBOOKS.md`, `docs/SEGMENTATION.md`, `docs/todo/SPATIAL_REGIONS_PLAN.md`
- Test data: project `4kS67f`, image `EaMaVq` (SHG segmentation + branches, `T`/`B` tracks)
