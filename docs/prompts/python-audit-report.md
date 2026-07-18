# Audit Result: Python Task + Utils Classification

> Opus planning pass — output of `docs/prompts/python-audit-prompt.md`. **No code changes were made**
> (except the deferred-goal entry appended to `docs/FUTURE.md`, as the prompt instructed). Sonnet
> executes from this document. Mesh-feasibility claims were adversarially verified; corrections from
> that pass are folded in and marked **[verified]** / **[corrected]**.
>
> **See `docs/prompts/julia-port-watchlist.md`** for the distilled, living checklist — what can/cannot
> be ported and the *specific Julia drop-in to watch for* on each blocked item. **Rule: only port
> what the current framework uses; do not port unused algorithms** (e.g. `findSignalPeaks`, spatial)
> speculatively. Net: the only in-framework Julia-portable tasks are drift/AF correct, and they are
> *feasible now* — `Zarr.jl` reads/writes zarr pixels (v2+v3) — gated only on building cecelia's own
> multiscale pixel-I/O layer on it (watch item W1: an implementation task, not an ecosystem gap,
> whose payoff is consolidation only).

End goal: Julia owns all computation except Napari. Default to Julia unless there is a concrete
blocker; do not port anything that creates a net complexity increase.

---

## Bottom line: does this remove Python? No — and what the real boundary is

**Read this before the tables.** The aspirational framing ("Julia owns all computation except
napari") is only half-right as a *disappearance* goal. Python does not go away, and — importantly —
**the Python environment does not meaningfully shrink**, regardless of how much we port. The honest
picture:

**Python is irreducible for a short, well-defined list of algorithms, plus the viewer:**
1. **DL segmentation / denoise** — cellpose + torch. No Julia equivalent.
2. **Bayesian tracking** — btrack (C++/Python). No Julia equivalent.
3. **Leiden clustering** — scanpy + leidenalg + UMAP + PAGA + ComBat + Harmony. **Verified: no mature
   native-Julia replacement exists** (Leiden.jl variants are small/unproven or PyCall wrappers; no
   Julia PAGA/ComBat/Harmony). Trading it risks cluster-label reproducibility and drops batch
   correction. **This is the anchor that keeps a Python data path alive.**
4. **napari** — the viewer (see `docs/FUTURE.md` → *Julia-native image viewer*).

**Everything else Julia can own** — and already owns a lot: gating/logicle, the H5AD *read* path,
track measures (celltrackR port). Portable additions: drift correct, AF-core, `findSignalPeaks`,
spatial neighbours / contacts-graph / clusters / regions, 3D Delaunay (`TetGen.jl`/`Quickhull.jl`),
mesh construction + descriptors. Triangulation specifically **is** Julia-ready — only clustering is not.

**Porting does not shrink the environment.** No PORT_JULIA candidate removes a heavyweight dependency:
port both correction tasks and `skimage`/`scipy`/`zarr`/`dask` all stay (measure + segmentation +
pixel I/O need them). The disk weight — `torch`/`cellpose` (~2.5 GB), `scanpy`, `btrack` — is gated
*entirely* behind the KEEP_PYTHON tasks above. **The payoff of porting is consolidation (compute in
one language, fewer Julia↔Python seams), not dependency elimination.** Be clear-eyed that that is the
return.

**On h5ad (storage format decision — resolved):** h5ad's original justifications were (a) the
Python-world standard, (b) R access via reticulate, (c) easy scanpy clustering. In pineapple (a) and
(b) are **dead** (Julia is primary; no R/reticulate) — and, per the old-R codebase, h5ad was *never*
read natively in R anyway; R always reached into Python for it. Only (c) survives. External
Python-ecosystem openability is **not** a requirement (confirmed). **Nevertheless, keep h5ad** — for
a *fourth*, still-live reason: it is **the one on-disk format both Julia and Python read/write
natively** (Julia via `HDF5.jl`; Python via `anndata`). That dual-native access is what lets the
Julia engine and the Python side (scanpy clustering + the napari bridge's local centroid reads) touch
cell data directly without proxying through the API. A Julia-native store would force writing *Python
readers* for it (re-introducing the coupling we want gone) and delivers **no** env shrink. So h5ad
stays — not for legacy or ecosystem reasons, but as neutral dual-read ground while an unportable
Python clustering consumer exists. **The storage format is not the Python lock-in; the clustering
algorithm is.**

- *Optional, low-priority optimisation:* teaching the Julia writer to emit anndata-format `X` +
  categoricals directly (parity-tested vs anndata — the CI parity test already exists) would remove
  the `measure_labels`/`track_props`/`write_categorical_obs` writer subprocesses. Worth it only if
  reducing Python *entry points* is a goal in itself; it is **not** strategically necessary, because
  scanpy keeps a Python data path regardless. Do not prioritise it over the compute ports.
- *Rejected:* switching to a Julia-native cell-data format. No benefit (no env shrink), real cost
  (Python readers for the new format, napari-bridge rework), and it fights the dual-read property
  that makes the current split work.

**Net:** the goal is not "remove Python" (unreachable while cellpose/btrack/scanpy/napari exist) but
"Julia owns the numeric middle; Python is a permanent, well-bounded rim of DL + clustering + viewer."
The ports in this document are worth doing for architectural coherence and fewer cross-language
handoffs — not because they liberate the codebase from Python.

---

## Tasks

| File | Classification | Reason | Julia packages (if PORT_JULIA) |
|---|---|---|---|
| `cleanupImages/drift_correct_run.py` | **PORT_JULIA** | Pure FFT phase cross-correlation registration + array shift. No DL, no Napari, no Python-only lib. Cleanest first candidate. | `SubpixelRegistration.jl` (direct port of skimage `phase_cross_correlation`), `FFTW.jl`, `Statistics`, `Images.jl`, `Zarr.jl` (I/O plumbing) |
| `cleanupImages/af_correct_run.py` | **PORT_JULIA** (core) | Percentile background subtraction + rescale/clip + median/Gaussian filter — pure array/morphology. **Caveat:** two *optional, default-off* features have no drop-in: `rolling_ball` (no Julia pkg) and `denoise_wavelet`/`denoise_tv_chambolle` (thresholding hand-port). Gate those behind their ports; port the mandatory path now. | `ImageFiltering.jl`, `ImageMorphology.jl`, `Statistics`/`StatsBase.jl`, `Images.jl`, `Wavelets.jl` (optional, hand-port shrink); `rolling_ball` = port algorithm |
| `cleanupImages/cellpose_correct_run.py` | **KEEP_PYTHON** | `cellpose.denoise.DenoiseModel` + torch DL inference. cellpose pinned v3.1.1.2 for `DenoiseModel`. | — |
| `clustPops/cluster_run.py` | **KEEP_PYTHON** | scanpy + leidenalg + UMAP/PAGA Leiden pipeline. Porting risks divergent cluster labels vs the R engine for little benefit. | — |
| `clustTracks/cluster_run.py` | **KEEP_PYTHON** | Track analogue of clustPops; same scanpy/leidenalg engine. | — |
| `importImages/read_imagej_physical_size_run.py` | **KEEP_PYTHON** | Tiny metadata-only helper (`tifffile.imagej_metadata`). No Julia ImageJ-metadata parser; port = negligible benefit. | — |
| `segment/cellpose_run.py` | **KEEP_PYTHON** | Tiled cellpose (torch) segmentation. | — |
| `segment/measure_labels_run.py` | **UNCERTAIN** | Core `regionprops` + Gaussian is portable, BUT: (1) optional `extendedMeasures`/`saveMeshes` use trimesh (3D mesh + STL); (2) it **creates** the `.h5ad` (sanctioned Python job); (3) `regionprops` has no single Julia equivalent. Needs a scoping decision — see *Recommended port order*. | `ImageMorphology.jl` (partial regionprops), `Statistics`/`StatsBase.jl`, `ImageFiltering.jl`; mesh path see Spatial section |
| `tracking/bayesian_tracking_run.py` | **KEEP_PYTHON** | Wraps `btrack` (C++/Python, no Julia equivalent). | — |
| `tracking/track_props_run.py` | **KEEP_PYTHON** | Pure writer — measures already computed in Julia (celltrackR port); this only **creates** the per-track `.h5ad`. Sanctioned Python file-creation. | — |
| `writers/write_categorical_obs_run.py` | **KEEP_PYTHON** | Writes anndata **categorical** obs columns — exists precisely because the Julia LabelProps writer is float64-only. Deliberate Python half of the write split. | — |

**Summary:** 2 clean ports (drift, AF-core), 1 uncertain (measure_labels — mesh + file-creation
surface), 8 stay Python (DL inference, scanpy ecosystem, or sanctioned `.h5ad` creation).

---

## Unported legacy modules

Top-level `.R` files are empty R6 base classes; logic lives in method files + `py/` subdirs.
Cross-referenced against `app/py/tasks/`, `app/src/tasks/` (Julia), and `task_registry.jl`.

| R module | Has port? | Recommendation | Why |
|---|---|---|---|
| cleanupImages | Yes | — | af/drift/cellpose correct ported. |
| importImages | Partial | (a) Julia sub-methods | Module ported (omezarr/remove). **Sub-gaps:** `ometiff`, `tenxXenium`, `cellToLocation`, `cropImage`, `splitIntoCores/Series`, `normalise/rescale` not yet ported. |
| segment | Yes | — | cellpose + measure_labels ported. |
| tracking | Yes | — | bayesian_tracking + track_measures/props ported. |
| clustPopulations | Yes | — | clustPops (Leiden). |
| gatePopulations | Yes | — | Ported natively in Julia. |
| behaviourAnalysis | Partial | mixed | hmmStates/hmmTransitions → Julia. **UNPORTED:** `createPseudotime` **(b, scanpy DPT)**, `clusterTracks` **(a, celltrackR morphodynamic clustering — distinct from Leiden clustTracks)**, `hmmHybrid` + `hmmStatesNoiseFilter` **(a, light R logic)**. |
| **spatialAnalysis** | **No** | mixed (see Spatial) | Entire module unported — the largest real gap. Neighbour/contact/cluster/region + network weights → **(a) Julia** (NearestNeighbors.jl + Graphs.jl); `cellNeighbours` → **(b) squidpy**; `cell*Mesh` → mesh/collision (see Spatial section). |
| **signalAnalysis** | **No** | **(a) Julia** | `findSignalPeaks` per-track peak detection; empty `py/`, pure R/data.table. **Cheapest genuine win.** |
| clustRegions | **No** | (a) Julia or drop | Only an empty stub base class in R — never implemented. Port alongside spatial region work or drop if dead. |
| trainModels | **No** | **(b) Python** | `trainN2V` needs n2v/TensorFlow (Noise2Void DL). Hard Python-only. |
| objcl | **No** | **(b) Python** | Object classification via `apoc` (pyclesperanto/OpenCL). |
| pixcl | **No** | **(b) Python** | Pixel classification, same `apoc` dep. |
| importFlow | **No** | (b) Python or (a) `FCSFiles.jl` | `fcsRaw` FCS import. Python `flowkit` lands in the anndata pipeline with least friction; `FCSFiles.jl` is the Julia-native option. |
| hpc | — | **(c) drop** | HPC job submission — superseded by `scheduler.jl`. |
| mflux | — | **(c) drop** | Mediaflux data transfer. |
| launchpad | — | **(c) drop** | Task-launcher infra. |
| taskTest | — | **(c) drop** | Dev-only — replaced by Julia `testTasks`. |

**Biggest gaps:** spatialAnalysis (whole module, splits Julia-mostly + squidpy/trimesh holdouts);
behaviourAnalysis trajectory side (`createPseudotime`, `clusterTracks`); signalAnalysis (cheapest);
the objcl/pixcl/trainModels Python-DL cluster (apoc + n2v); importFlow (FCS ingest).

---

## Utils

| Module | Classification | Notes | Duplication risk |
|---|---|---|---|
| `label_props_utils.py` | **ELIMINATE** (= no Julia port needed) | **[doc-corrected]** Per DATAMODEL.md *"Two implementations, one spec — keep them in lock-step"*: the Julia `LabelProps` chain and this Python `LabelPropsView` are **co-equal implementations of one contract**, not a Julia original + Python copy. "ELIMINATE" means *no port work* — **the file is permanent**, not deletable, as long as the napari bridge + Python tasks need direct cell access. Governed by the same-change lock-step rule + the CI `LabelProps Julia/Python parity` testset. | Any change to one reader/writer (encoding type, chain verb, semantics) **must** be made on the other in the same change — the documented drift risk. |
| `tracking_utils.py` | **SHARED** | Two halves: `BayesianTrackingUtils` (btrack → Python) and `write_track_props` (creates per-track `.h5ad`, invoked *by* Julia `track_measures.jl`). | **YES** — the `.h5ad`-creation contract (obs index = track_id string, `uns` marker, None→NaN float32 X) must stay in sync between Julia (assembles X/lineage) and this writer. |
| `ome_xml_utils.py` | **SHARED** | ome_types parse/save + NGFF metadata. Consumed by must-stay-Python image tasks. | **YES** — same conventions independently reimplemented in Julia `omezarr.jl` (`read_ome_metadata`, `update_ome_*`). Keep both in sync by hand; note the documented nested-vs-flat `.zattrs` trap. |
| `dim_utils.py` | **KEEP_PYTHON_ONLY** | Axis-order + slice/index math (`dim_idx`, `create_channel_slices`, `get_padded_cube`). Spine of every Python image task. | Conceptual overlap with Julia `img_physical_sizes`, but the slice-generation half has no Julia consumer. |
| `cellpose_utils.py` | **KEEP_PYTHON_ONLY** | cellpose + torch predict_slice. | — |
| `segmentation_utils.py` | **KEEP_PYTHON_ONLY** | Tiled-segmentation base: tiling, IoU seam stitching, nuc-cyto matching. Serves cellpose only. | — |
| `measure_utils.py` | **KEEP_PYTHON_ONLY** | regionprops + trimesh; **produces** `labelProps/{vn}.h5ad` (sanctioned exception). | — |
| `correction_utils.py` | **KEEP_PYTHON_ONLY** | Drift + AF/denoise/rolling-ball/top-hat backing the two PORT_JULIA tasks. | See tasks — the *tasks* port, but this util backs them; a Julia port reimplements the numeric core (not a shared-file duplication). |
| `clustering_utils.py` | **KEEP_PYTHON_ONLY** | scanpy + leidenalg engine; correctly writes back via `LabelPropsView`. | — |
| `obs_utils.py` | **KEEP_PYTHON_ONLY** | `write_categorical_obs` — the Python half of the write split; invoked from Julia `label_props.jl`. Cannot be eliminated. | — |
| `zarr_utils.py` | **KEEP_PYTHON_ONLY** | Actual dask/zarr **pixel** I/O + multiscale pyramid writing + layout detection. Julia zarr code only reads `.zattrs`, never pixels. | Only the layout-detection convention overlaps `omezarr.jl`. |
| `slice_utils.py` | **KEEP_PYTHON_ONLY** | numpy slice-tuple generation for tiling/downsampling. | Mild in-Python overlap with `segmentation_utils._create_xy_tiles` — **CONSOLIDATE candidate**, but different code, low value. |
| `math_helpers.py` | **KEEP_PYTHON_ONLY** | Two round-to-multiple helpers, used only by `dim_utils`. | — |
| `script_utils.py` | **KEEP_PYTHON_ONLY** | Python side of the `run_py` contract (`[PROGRESS]`, `--params`). Counterpart to `py_runner.jl`. Inherently Python glue. | — |
| `rechunk_zarr.py` | **KEEP_PYTHON_ONLY** | One-off maintenance CLI; not a task, not imported. | — |
| `pop_utils.py` | **KEEP_PYTHON_ONLY** (by design) | **[doc-corrected — was UNCERTAIN]** POPULATION.md §*Membership access* documents this as the **designed** Python-side membership path: `PopUtils(client=cc).pop_df(…)` → `cecelia_client` → `GET /api/gating/membership` (label IDs only; measurement columns read locally via `label_props_utils`). "No in-repo consumer" is expected — the consuming modules/UIs are explicitly **later phases** (POPULATION.md §*Out of scope*: clustering/tracking UIs). It does *not* re-evaluate gates (Julia is sole evaluator); it assembles the DataFrame around Julia-supplied membership. **Do not eliminate.** | Not a real duplication risk: it consumes Julia membership, it does not re-implement the gating engine. Kept dependency-free on purpose (no flowutils/juliacall) so logicle/gate logic stays Julia-only. |
| `cecelia_client.py` | **KEEP_PYTHON_ONLY** (by design) | **[doc-corrected]** The deliberately thin (stdlib `urllib` only) HTTP client that is the *sanctioned* Python→Julia membership channel (POPULATION.md §*Membership access*). Dependency-free by design so it adds nothing to the env and keeps gate logic solely in Julia. Currently only reachable via `pop_utils` because consuming modules are future phases — permanent-by-design, not dormant-then-drop. | — |

---

## Spatial/mesh feasibility

The legacy spatial pipeline is **Python-backed** (R6 `SpatialAnalysis.R` only manages
populations/filters; geometry lives in `inst/py/morpho_utils.py`, `inst/py/measure_utils.py`, and
`spatialAnalysis/py/*.py`). It splits into five concerns:

**1a. Label mask → per-cell mesh → shape descriptors** (3D only). `trimesh.voxel.ops.matrix_to_marching_cubes`
(→ skimage `marching_cubes` at anisotropic Z/Y/X spacing) → `fill_holes` → watertight `split` →
`merge_vertices` → `process` → optional quadric decimation → **one `.stl` per label** → trimesh
descriptors (`surface_area`, `volume`, oriented bbox, convex-hull area/volume via qhull,
`euler_number`, `extent`, `solidity`, `integral_mean_curvature`, `feret_diameter_max`,
`equivalent_diameter`) + custom `ellipsoid_fit` (numpy.linalg) + derived `sphericity`/`compactness`.

**1b/1c. Cell–cell contact + clusters via mesh collision.** Loads the saved `.stl`s into a
`trimesh.collision.CollisionManager` (backed by **python-fcl / FCL BVHModel**),
`min_distance_single` → `contact`/`contact_id`/`contact_n`; `mesh.contains` → `contained_by`/`contains_n`;
edges → `igraph` connected components ≥ N for clusters.

**1d. Non-mesh neighbours.** `squidpy` `sq.gr.spatial_neighbors` — grid / generic (KD-tree radius) /
delaunay → `obsp` adjacency.

**1e.** `coastal/morphology.py` is a *separate* 2D boundary path (find_contours → shapely → hmmlearn
HMM boundary states) — not the mesh bottleneck.

### Per-capability feasibility (post-verification)

| Capability | Feasible | Recommended Julia pkg | Gap |
|---|---|---|---|
| Marching cubes from label mask | **Yes** [verified] | `Meshing.jl` (`isosurface`, MarchingCubes) → GeometryBasics/Meshes mesh | Anisotropic spacing trivial. `Meshing.jl` does the MC; `GeometryBasics.jl` is the mesh *type* (does not itself do MC). |
| Surface area | **Yes** | `Meshes.jl` `measure`, or `GeometryBasics.area` | — |
| **Volume** | **Yes** **[corrected]** | **`GeometryBasics.volume`** (built-in signed-tet sum) | Original draft wrongly attributed volume to `Meshes.jl measure` (that returns *area* only) and called signed-tet a bespoke gap. It is **packaged** in GeometryBasics. Real precondition: closed, consistently-oriented mesh. |
| Watertight repair (`fill_holes`/`process`) | Partial (bespoke) **[corrected — impact ↓]** | hand-roll | No packaged equivalent, but MC on a clean binary mask normally yields an already-closed, vertex-merged manifold, so this is a **defensive edge-case handler, not a first-order volume-correctness blocker** as first stated. |
| Quadric decimation (simplify) | **No** (low-impact) | none maintained | trimesh `simplify_quadratic_decimation` has no Julia equivalent — but off by default (`simplify=False`). |
| Convex hull | **Yes** (native) | **`Quickhull.jl`** (pure-Julia N-d; 3D confirmed) | Avoid `QHull.jl` — it wraps scipy via PyCall (re-introduces Python). **[corrected]** hull **area/volume are not built-in** to Quickhull.jl — sum from facets (trivial). |
| Oriented (min-volume) bbox | Partial | roll via `LinearAlgebra` PCA | trimesh `bounding_box_oriented` not packaged. AABB trivial. |
| `euler_number`, `integral_mean_curvature` | Partial/No (bespoke) | manual from topology / dihedral sum | No packaged discrete mean-curvature integral. Straightforward but bespoke. |
| feret/equivalent diameter, sphericity, compactness, solidity, extent, S/V | **Yes** | `Distances.jl` + arithmetic | Pure formulas on hull vertices + area/volume. |
| Ellipsoid fit (axis lengths, ellipticity) | **Yes** | `LinearAlgebra` (`\`, `eigen`) | 1:1 numpy.linalg port; keep the singular-matrix try/catch. |
| **Cell–cell contact: non-convex mesh min-distance / collision** | **No drop-in** — critical risk | see below | The load-bearing gap. |
| Point-in-mesh containment | Partial | `Meshes.jl` `∈` (convex) / roll winding-number ray-cast (non-convex) | Needed for `contained_by`/`contains_n`. |
| Contact graph clustering | **Yes** | `Graphs.jl` `connected_components` | Direct `igraph` replacement. |
| Neighbours: KD-tree / radius | **Yes** | `NearestNeighbors.jl` (`KDTree`/`BallTree`, `knn`, `inrange`) | Mature. Static trees — fine here. |
| Neighbours: Delaunay adjacency | **Yes** **[corrected]** | 2D `DelaunayTriangulation.jl`; **3D `TetGen.jl` or `Quickhull.jl` (n-D Delaunay)** | Original draft called 3D Delaunay a gap needing a "Python shim" — **wrong**: TetGen.jl and Quickhull.jl both cover 3D. No shim required. |

### Bottom line — collision detection is the real risk [verified, stands]

Cell–cell contact/cluster detection (1b/1c) has **no honest Julia-native drop-in today**. The
semantics come from `trimesh.collision.CollisionManager` → **python-fcl `BVHModel`**, which computes
minimum distance *and* collision between **arbitrary non-convex triangle meshes**. Cell meshes are
non-convex and interdigitate; the measured surface-to-surface distances and contact/contained counts
depend on that non-convexity.

- **`GJK.jl` / `Meshes.jl` GJK are convex-only** — using them forces per-cell convex-hull reduction,
  a *semantic change* (false contacts, hull-based containment).
- **No `FCL.jl` binding exists** — achievable via CxxWrap/BinaryBuilder, but that's a real C++/build
  task and swaps Python-FCL for C++-FCL (the "no non-Julia dependency" goal isn't actually met).
- **[corrected]** A mature native-Julia BVH **does exist** — **`ImplicitBVH.jl`** (actively
  maintained, GPU-capable). But it is **broad-phase only** (bounding-volume overlap + ray traversal);
  it does **not** provide the narrow-phase (exact triangle-triangle min-distance, non-convex
  containment). So the realistic native path is `ImplicitBVH.jl` (broad phase) **+ a hand-written
  narrow-phase** — smaller than "build a BVH from scratch," but still correctness-sensitive.

**Ranked options for 1b/1c:** (1) **keep FCL** — small Python/trimesh sidecar for collision only, or
invest in an FCL binding; (2) **convex-hull GJK approximation** with `Meshes.jl`, validated against
real data (test set jFWePN) before committing; (3) native `ImplicitBVH.jl` + bespoke narrow-phase
(highest cost/risk). **Do not assume a pure-Julia collision replacement is a drop-in.**

**Also flagged by verification (omitted from first draft):** the pipeline exports meshes to a
**napari surface layer** (`to_napari_surface_object`, `tracks_to_meshes`) — a genuine porting item
tied to the viewer. Plus three minor 2D features not listed: `asymmetry`, `perimeter_to_area`,
`nc_ratio`.

---

## Duplication risks (summary)

Utils that would need to exist in both Julia and Python (or be kept in sync):

1. **`tracking_utils.py` → `track_props_run.py` ↔ Julia `track_measures.jl`.** The per-track
   `.h5ad`-creation contract (obs index = track_id string, `uns['cecelia_table']='tracks'`,
   None→NaN float32 X reshape). **Keep the writer Python** (sanctioned file-creation); pin the
   contract with a round-trip test so Julia (assembler) and Python (writer) can't drift.
2. **`ome_xml_utils.py` ↔ `omezarr.jl`.** OME-XML/NGFF conventions (PhysicalSize*, dimension_order,
   sidecar location, µ→u normalisation) already live in both. **Keep Python side as-is** (only
   Python image tasks need it); track the pair as live duplication, mind the nested-vs-flat `.zattrs`
   trap.
3. **`correction_utils.py` numeric core.** When drift/AF port to Julia, the Julia task reimplements
   the phase-correlation / percentile-correction math. Not a shared *file* — the Python util is
   deleted once its two tasks port. No dual maintenance if ports land together.
4. **`pop_utils.py` ↔ Julia `pop_df`.** **[doc-corrected]** *Not* a duplication risk. Per
   POPULATION.md it consumes Julia-evaluated membership over HTTP and assembles the DataFrame around
   it — it does not re-implement the gating engine. Keep as the documented Python membership path.
5. **`slice_utils.py` ↔ `segmentation_utils._create_xy_tiles`.** In-Python tiling overlap —
   CONSOLIDATE candidate, low value, not a Julia concern.

Rule applied throughout: if a util would need duplicating just for one small task, **that task stays
Python** (per prompt).

### Doc reconciliation (cross-checked against DATAMODEL.md + POPULATION.md)

The audit was re-grounded against the repo's own invariants. Findings:

- **`label_props_utils` / `pop_utils` / `cecelia_client` are architecturally load-bearing, not
  disposable** — see the doc-corrected rows above. The H5AD reader parity contract (DATAMODEL.md)
  and the Python membership-access path (POPULATION.md) are *designed* Python surfaces that persist
  as long as the napari bridge + Python tasks/notebooks exist. There is **no open "eliminate
  `pop_utils`?" decision** — the docs already answer it: keep.
- **Doc fixed:** POPULATION.md §*Language boundaries* previously said *"Write = Python … never write
  H5AD from Julia,"* which predated the native Julia obs writer. Corrected to the actual split:
  **Julia writes numeric `obs` columns natively** via `save!`/`HDF5.jl` (crash-safe
  column-order-last, bidirectionally verified), but **Julia cannot write `X`** — the feature matrix,
  `var`, and any new file are Python's job (`anndata.write_h5ad`), as are **categorical/string obs**
  (`write_categorical_obs`). This is the boundary the audit's utils classifications follow: obs
  measures → Julia; `X`/`var`/new-file + categoricals → Python. The corollary for porting is that
  `measure_labels`/`track_props`/`write_categorical_obs` stay Python **because they produce `X`/new
  files or categoricals**, which Julia cannot emit — independent of whether their *compute* could be
  Julia.
- **Doc fixed:** DATAMODEL.md referred to the categorical writer as `py/tasks/labels/`; corrected to
  `app/py/writers/write_categorical_obs_run.py`.
- **Reinforced, not changed:** DATAMODEL.md §*Mesh files* confirms per-cell STLs are "the input to
  downstream spatial analysis (cell contact detection — equivalent of the original
  `cellContactsMeshes.R`)" — i.e. the mesh→collision pipeline is a real forward goal, so the
  collision-detection gap flagged in the Spatial section is on the critical path, not hypothetical.

---

## Recommended port order

Ordered by effort/benefit:

1. **`drift_correct` → Julia.** Cleanest port; `SubpixelRegistration.jl` is a direct
   `phase_cross_correlation` equivalent. Only zarr-read + OME-XML-write plumbing needed Julia-side.
2. **`af_correct` (mandatory path) → Julia.** Pure percentile/filter/morphology math. Gate the
   optional `rolling_ball` + wavelet/TV denoise behind their (bespoke) ports — ship the default path
   first. *(Ports 1–2 drop no Python deps yet — cellpose_correct keeps skimage/scipy alive — but
   remove the first compute tasks from the Python surface.)*
3. **`signalAnalysis.findSignalPeaks` → Julia (new).** Cheapest genuine analysis win: pure
   R/data.table peak detection, no external dep. Closes a real gap with `DataFrames.jl`.
4. **behaviourAnalysis light holdouts → Julia:** `hmmHybrid`, `hmmStatesNoiseFilter` (light R logic),
   then `clusterTracks` (celltrackR morphodynamic — pairs with the existing celltrackR port).
5. **spatialAnalysis Julia-mostly half → Julia (new):** neighbour/contact/cluster/region + network
   weights via `NearestNeighbors.jl` + `Graphs.jl`; 3D Delaunay via `TetGen.jl`/`Quickhull.jl`.
6. **`measure_labels` scoping decision** (the UNCERTAIN task) — split the portable regionprops/intensity
   core from (a) the trimesh mesh path and (b) `.h5ad` creation. Likely keep `.h5ad` creation Python
   (sanctioned) and defer the mesh path to the collision decision below.
7. **Mesh + descriptors (1a) → Julia** once (6) is scoped: `Meshing.jl` + `GeometryBasics`
   (volume) + `Quickhull.jl` (hull) + bespoke oriented-bbox/curvature. Prototype and validate volume
   on real data.
8. **De-risk collision (1b/1c) separately** — decide FCL-sidecar vs convex-GJK-approx vs
   ImplicitBVH+narrow-phase **before** committing spatial mesh work. This is the gating unknown for a
   fully-Julia spatial module.

Keep-Python-forever (do not schedule): cellpose (seg + denoise), btrack, scanpy clustering, the
anndata categorical/`.h5ad` writers, and the objcl/pixcl/trainModels DL cluster (unless the classifier
stack itself is replaced).

---

## Notebooks: port QMD/Jupyter → Pluto.jl

No `.qmd` files exist in either tree. Verdicts:

| Notebook | Deps | Verdict | Reason |
|---|---|---|---|
| `cecelia/notebooks/populations.ipynb` | Cecelia.jl, DataFrames | **PORT_PLUTO** | Already Julia; pure read-only Cecelia.jl usage. Ideal. |
| `cecelia/notebooks/backend_model.ipynb` | Cecelia.jl, ImportOmezarr, NapariViewer | **PORT_PLUTO (caveats)** | Port create/load/access cells; mark the import (needs external `.tif` + bioformats2raw) and napari (spawns a Python bridge subprocess) cells as manual/`@bind`-gated — they fit Pluto's reactive model poorly. |
| `coastal/notebooks/*` (optical_flow, tracking, treecell-v2/3/4, treemove-v1/v2/torch, pipeline_confetti_ceiling) | torch / tensorflow / btrack / hmmlearn / leidenalg / sklearn / shapely | **KEEP** | All depend on Python-only ML libs and the `coastal` package, which `docs/SHIPPING.md` records as **dropped** from the shipped product. None run against Cecelia.jl — research notebooks, not Cecelia usage. Convert to `.qmd` only if text-diffable review is wanted; they cannot become Pluto notebooks. |

**Bottom line:** only the two `cecelia/notebooks/*.ipynb` port to Pluto (both already Julia).

**Versioning:** split by audience. The two cecelia notebooks are "how to use Cecelia.jl" docs → keep
**global, versioned with code** so they track API changes; lift hardcoded UIDs
(`NRUBxU`/`KDIeEm`/`jFWePN`) to a top parameter cell so they are dataset-agnostic. Real dataset-specific
analyses → **per-project `{proj}/notebooks/`** (versioned with the project dir, alongside `ccid.json`/
`gating/`). Rule: *teaches the API → repo; analyses this dataset → project dir.*

**Cold-start:** **Do not use `DaemonMode.jl`** — it dispatches CLI *scripts* to a daemon; Pluto manages
its own workers and won't route through it. The pain is time-to-first-execution of the heavy stack, not
process spawn. Recommended, in order: (1) ship a **`PackageCompiler.jl` sysimage** bundling Cecelia.jl +
heavy deps and launch Pluto against it (`Pluto.run(; compiler_options=…sysimage=…)`) — the real fix,
slots into the Pixi/constructor release flow, drive the build with a `PrecompileTools` workload
exercising `init_object → pop_df`; (2) run **one persistent warm Pluto server** (`pixi run notebooks`);
(3) interim, a `PrecompileTools.@compile_workload` in Cecelia.jl. Net: **persistent server + sysimage**,
sysimage load-bearing.

**Playground link (Vue):** launches Pluto pointed at the notebook dir; notebook management table
(add/remove, per-row description — same pattern as the image table's exclusion notes). Pluto runs its
own Julia session (`using Cecelia`), **not** the API server (that's the debug console).

---

## coastal

- **Package does NOT import cecelia** (`grep cecelia coastal/*.py` → zero). Self-contained. Its
  notebooks reference the *old R cecelia* (`R-workspace/cecelia/inst`), pulling only narrow IO/calibration
  helpers (`zarr_utils.open_as_zarr`, `dim_utils.DimUtils`, vendored btrack `cell_config.json`).
- **Not a full-Julia migration candidate.** Core value = Python-only ML: **torch** (UNet learned-affinity
  segmentation — the raison d'être), **cv2 Farneback optical flow**, plus hmmlearn/shapely/cma. Specific
  blocker: torch learned-affinity inference + OpenCV optical-flow front-end. This matches `docs/SHIPPING.md`'s
  record that coastal was **dropped as a cecelia dependency**.
- **Shared utils worth porting once (serves both):** an IoU label-matching kernel (coastal
  `match_masks_3d`/`label_overlap`/`intersection_over_union` ↔ cecelia `segmentation_utils`
  `_compute_iou_matrix`/`_stitch_seam`/`_match_nuc_cyto` — same kernel, different axis); small-object
  filter (coastal `filter_small_cells` ≈ cecelia `_post_process` min-size branch); centroid + per-cell
  intensity extraction; regionprops shape descriptors; percentile normalisation. These are **the rim, not
  the wheel** — port into Cecelia.jl and consume from both if/when coastal can depend on `Cecelia.jl`.

---

## Napari footprint + Python-env shrinkage

**Framing:** the Pixi env is explicitly **not** napari-specific — cellpose/measure/btrack/label-props
writer **and** the bridge share one env. Dropping napari alone shrinks disk only modestly; the bulk is
compute.

**Napari-only closure (~80–130 MB, <5% of env once torch-CUDA ~2.5 GB counted):** `napari`, `pyqt5`,
**`pyqt5-qt5` (~50–90 MB, the single largest item)**, `pyqt5-sip`, `vispy`, `qtpy`, `magicgui`, `superqt`,
`app-model`, `npe2`, `psygnal`, `napari-console`/`-svg`/`-plugin-engine`, and `websockets` (bridge WS
transport). *(matplotlib is NOT napari-only — scanpy pulls it.)* Heavy in **system requirements**
(Qt/display server, a second process, a WS hop), light in bytes.

**Shrinkage buckets:**
- **Bucket 1 — stays regardless (data + import):** python, numpy, zarr, dask, anndata, pandas, ome-types,
  scipy, scikit-image, matplotlib, + `openjdk 17` (bioformats2raw JVM). Leaves only when the whole Python
  data/import layer is reimplemented in Julia — last to go.
- **Bucket 2 — removable per compute-port batch (the disk wins):** seg/denoise port → drop **torch/torchvision
  (~2.5 GB — biggest win)** + cellpose; tracking → btrack + cvxopt; clustering → scanpy + leidenalg; mesh →
  trimesh. (scikit-image/scipy linger for measure.)
- **Bucket 3 — napari-only, last GUI batch:** the closure above; removing it also deletes the `:7655` process,
  the `napari`/`stop-napari` pixi tasks, `napari_bridge.py`, and the Qt/display requirement — but ~80–130 MB.
- **Bucket 4 — env deletable only when 1+2+3 all done.**

**Sequencing takeaway:** dropping napari is cleanly separable and removes process/GUI complexity, but frees
little disk; disk lives in Bucket 2. The env can't be deleted until Bucket 1 (data layer + JVM import) is
ported. See `docs/FUTURE.md` → *Julia-native image viewer* for the replacement analysis and recommendation.

---

## Method

Produced by a fan-out workflow (`python-audit`, run `wf_90ef6976-638`): 7 parallel investigators
(tasks / utils / R-gap / coastal / napari-footprint / notebooks / mesh-research) + 3 adversarial
verifiers on the mesh/Julia-package feasibility claims (mesh-construction lens, collision/BVH lens,
faithfulness-to-Python lens). Verifier corrections are marked **[verified]**/**[corrected]** above.
