# Deliberately Deferred

Things that are **not forgotten and not ruled out — just consciously set aside**: an optimisation not
worth the complexity at the current scale, a library the ecosystem hasn't caught up with, a product
non-goal, or work gated on a trigger that may never fire. Each entry states what it is, why it was
deferred, and the concrete condition under which it becomes worth revisiting.

This is distinct from `docs/TODO.md`: TODO is the working backlog of things we intend to do; FUTURE is
what we have consciously **set aside** — a known-better alternative, a deliberate non-goal, or work
gated on a trigger that may never fire. If nobody should act on it, it belongs here rather than in the
backlog. Do not implement anything listed here without explicit instruction — this is a reference
document, not a backlog.

When generating or updating documentation, include a pointer to this file for any decision that
involved a known-better alternative being deliberately set aside.

---

## Zarr v3 + Sharding

**What:** Replace the current OME-Zarr v2 image storage with Zarr v3, enabling the sharding codec
(ZEP-2). Sharding stores multiple chunks in a single file, decoupling chunk size from file count.
For large 3D timecourse microscopy images, small chunks (needed for fast random access in Napari)
currently produce large numbers of files — a real filesystem and inode pressure problem at scale.

**Why deferred:** As of the current implementation, OME-NGFF has accepted the Zarr v3 transition
in RFC-2 but the spec has not fully landed in the OME-Zarr ecosystem. Napari's support for Zarr
v3 / sharded arrays should be confirmed before adopting it as the default write format.
Additionally, shard/chunk sizing requires careful tuning per image geometry — naive adoption with
wrong sizing can make write performance significantly worse, not better (benchmarks show up to
100× write slowdown with mismatched shard geometry).

**Adopt when:**
- OME-NGFF v0.5 (Zarr v3 based) is stable and widely supported
- Napari reads sharded OME-Zarr without issues
- Image import module is being touched anyway (avoid a standalone migration)
- Images are large enough that file count is causing observable filesystem problems (typically
  >10k chunks per image)

**Reference:** `zarr-python` 3.x (released January 2025) already supports sharding. The bottleneck
is ecosystem readiness, not library availability. See `docs/SEGMENTATION.md` / image import.

---

## InMemoryDatasets.jl for large cross-image DataFrame operations

**What:** Replace `DataFrames.jl` with `InMemoryDatasets.jl` for groupby, join, and aggregation
operations. InMemoryDatasets.jl uses multithreaded, columnar-optimised algorithms — the closest
Julia equivalent to R's `data.table`. It offers meaningfully faster groupby and joins for large
in-memory tables.

**Why deferred:** DataFrames.jl is the Julia ecosystem standard — all tooling, packages, and
existing Cecelia code integrates with it. For Cecelia's current per-image scale (10k–100k cells
per image), the expensive step is HDF5 I/O (handled lazily by the `LabelProps` chain before any
DataFrame operation, see `docs/DATAMODEL.md`). What arrives in memory is small enough that
DataFrames.jl with in-place `!` functions performs adequately. The data.table performance problem
existed in R because R itself is slow for loops — Julia's JIT means that problem largely doesn't
transfer.

**Adopt when:**
- Cross-image batch operations aggregate many images simultaneously in one DataFrame (e.g. spatial
  analysis across 50+ images, large cohort HMM training)
- Profiling shows groupby or join operations — not HDF5 I/O — as the bottleneck
- The operation in question handles millions of rows in memory at once, not tens of thousands

**Note:** InMemoryDatasets.jl is not a drop-in replacement — its API differs from DataFrames.jl.
Adoption would require updating call sites. Do not switch speculatively; only switch when profiling
identifies a specific bottleneck that InMemoryDatasets.jl addresses.

---

## Julia-native image viewer (napari replacement)

**What:** Replace the Python/Napari viewer with a Julia-native, Qt-based viewer so Napari becomes
the *last* Python dependency to fall away. Target stack discussed: `GLMakie.jl` (rendering),
`Mousetrap.jl` (GTK/Qt shell), `Zarr.jl` (data). It must match what the current bridge actually
uses (`napari_bridge.py` / `api/src/napari_api.jl`): multiscale **OME-Zarr pyramid** display (both
bioformats2raw `zarr/0/[level]` and flat layouts, lazy dask/zarr, v2 **and** v3), per-layer
physical **scale + units** (µm/nm — inconsistent units disable unit rendering for all layers),
t/z navigation with a 2D↔3D volumetric toggle (orthogonal slicing is sufficient — no Blender-style
raycast needed), the layer set **image / labels / points / tracks / shapes**, categorical
(Okabe–Ito) + continuous (viridis) label colourmaps, and — the hardest interactive piece — the
**bidirectional linked-brushing round-trip** (draw polygon → resolve enclosed centroids → POST IDs
back to Julia; render Julia-owned populations/tracks as overlays with per-pop reconciliation).

**Why deferred:** The hard gap is not rendering — it is the microscopy scaffolding Napari built over
years: **pyramidal LOD tile scheduling** (no LOD/tile scheduler exists in GLMakie), the label
colourmap shader, and linked cursor planes. A ground-up build is a *months* effort for something
merely usable. Crucially, the footprint audit shows the payoff is **process/system complexity, not
disk**: the Napari-only closure (napari, pyqt5, pyqt5-qt5, vispy, qtpy, magicgui, superqt, npe2,
psygnal, the bundled plugins, and the `websockets` bridge transport) is only ~80–130 MB — <5% of the
env once the torch-CUDA wheel (~2.5 GB) is counted. Dropping Napari removes the Qt/display-server
requirement, the `:7655` bridge process, and a WS hop — but frees little space. The disk savings live
in the *compute* ports (torch/cellpose/btrack/scanpy), which are independent of the viewer. So a
Julia-native viewer is worth doing for architectural cleanliness (one language, no second process),
not to shrink the environment — which lowers its urgency.

**Independent recommendation (the more direct path):** Before committing to a ground-up GLMakie
build, evaluate **embedding Napari as a stripped pure-display component** — disable the `npe2`
plugin system (Cecelia owns the full stack, so the plugin surface is dead weight) and drive the
viewer purely through the existing bridge commands. This keeps the battle-hardened pyramid LOD
scheduler and label shader that are the *actual* multi-month gap, while shedding the plugin
complexity that is the only part we don't want. The GLMakie + `Zarr.jl` + **custom tile scheduler**
route is the real Julia-native answer, but the tile scheduler is precisely the piece Napari already
solved — rebuilding it is the bulk of the cost. Recommended sequencing: (1) strip Napari to a
display-only embed as the near-term simplification; (2) treat the full GLMakie viewer as a genuine
multi-month project taken on only once every compute task is Julia and Napari is the sole remaining
Python dependency. Do not start the ground-up viewer to "save the environment" — that reason does
not hold.

**Adopt when:**
- Every compute task has ported to Julia and Napari is demonstrably the *only* remaining Python
  dependency (until then the Pixi Python env exists regardless, so the viewer buys no env removal)
- A GLMakie pyramidal-LOD tile scheduler exists (prototype it in isolation first — it is the
  load-bearing unknown; everything else is comparatively mechanical)
- There is appetite for a multi-month focused build, not an incremental side-task

**Reference:** `docs/NAPARI.md` (bridge process model, layer props, OME-Zarr layouts),
`napari/napari_bridge.py`, `api/src/napari_api.jl`; footprint + shrinkage buckets in
`docs/archive/python-audit-report.md`. Julia candidates: `GLMakie.jl`, `Mousetrap.jl`, `Zarr.jl`.

---

## Interactive pan/zoom on the gating plots

**What:** Let the user pan/zoom the gating scatter, changing the visible data range rather than just
scaling the whole panel (which is what the canvas-level `useCanvasZoom` CSS transform does today).

**Why deferred:** Deliberate product decision — the gating plots should not have zoom for now
(Dominik, 2026-07-27), and they don't. Recorded here because the *reason it was hard has gone away*
and that is worth not re-deriving: the old blocker was the WebGL layer, where the overlays would have
had to replicate regl's `projectionLocal · cameraView · model` transform and invert it for gate
hit-testing. That layer was removed. The dots and the gate overlay are now both 2D canvases mapping
data→px through the same `viewExtents`, and every layer already redraws when it changes.

**Revisit when:** someone actually wants it. The work is then a wheel/drag handler that edits
`viewExtents` — not a matrix-replication job. Hit-testing needs no change (`GateOverlay` already maps
through the same extents).

**Reference:** `docs/POPULATION.md` → *Gating plot — rendering & UX hacks*. (Was a `docs/TODO.md`
item; moved here because a deliberate non-goal is not open work. It carried a numeric ID that had
already been issued twice — one of the collisions that got numeric IDs retired, see `docs/TODO.md`.)

---

## Observer summary set roll-up mode

**What:** An optional set-scope roll-up for the observer's summary tools (`get_measure_summary`,
`get_cluster_summary`): per-population median-across-images + range, instead of per-image detail.

**Why deferred:** Set-scoped calls return per-image detail × many measures — large enough that the
observer offloads them to a subagent (~80k tokens). But the first-pass payload trim (dedupe cluster
`features` → `featuresByRun`, drop `mean`, round to 4 significant figures) is expected to shrink that
a lot on its own, so the roll-up may never be needed. It is also **not obviously correct**: behaviour
and phenotype vary *within* an image per population, so a median-per-image roll-up flattens real
structure the observer needs in order to spot outliers (Dominik).

**Revisit when:** the payload trim proves insufficient in practice. Then add it as an explicit opt-in
for the "compare T vs B across the set" question only — per-image stays the default.

**Reference:** `docs/ai-assist/OBSERVER.md`. (Was a `docs/TODO.md` item; moved here because it is
conditional on something that may never happen.)

---

## Per-branch channel intensities

**What.** `segment.branching` writes one row per skeleton path with skan's geometric measures
(length, tortuosity, branch-type, endpoint coordinates, anisotropy). It does **not** fold in
per-branch channel intensities (the old R version's `saveProps=TRUE` path).

**Why deferred.** The old approach was: register the skeleton labels zarr as a normal label set
under `{vn}.branch` and let the user run `segment.measureLabels` on it. That would work
mechanically in the current framework too — but it puts the skeleton value_name into the generic
`labels` picker (measure/track/segment dropdowns), which was the exact picker-pollution
`BRANCHING_PLAN.md` Decision 6 dodges by giving branch labels a dedicated `img.branch_labels`
field. It also creates a *second* branch table (`labelProps/{vn}.branch.h5ad`) alongside the
sidecar (`labelProps/{vn}__branch.h5ad`), which pop_df can't cleanly reconcile.

The clean fix is a `intensityChannels` param on `segment.branching` itself: extract each branch's
channel means per timepoint inside the runner and fold them into the sidecar's `X`/`var`. That
keeps the sidecar contract but is a small self-contained addition — deferred until someone asks.

**Revisit when.** A user needs per-branch channel intensities (e.g. co-localisation of a marker
with branch objects). Implementation is small — a mean-per-branch pass in `branching_run.py`
using the skeleton labels + the raw image, plus a new task param.

**Reference:** `docs/todo/BRANCHING_PLAN.md` Decisions 5 + 6.

---

## Structure-vs-motion angle map (field-vs-field correlation)

**What.** A tile map of the angle between the local collagen orientation and the local direction
of cell motion — "where do cells move along the fibres vs across them". The legacy version
(`behaviourTcells3P.Rmd:595-710`) got there by running the *whole* pipeline a second time on the
tracks: `segment.binariseTracks` rasterised a `live` population's paths into a label image,
`createBranching` skeletonised that, and the map was then field-vs-field between the two grids.

**Why deferred.** It is not in Figure 4, and it was not what was asked for — panels B and D answer
"how do tracks relate to the structure" with an overlay and a per-image correlation. Building a
second rasterise → skeletonise → eigendecompose pipeline for a map nobody requested is speculative
scope.

**Revisit when.** Someone wants the per-box structure-vs-motion angle map. The cheap route is
probably NOT the legacy one: the orientation grid already exists (`quiver_df`), so a per-box mean
track vector from `pop_df` would give the same map without a second skeletonisation. Note the
vignette's 0–180° range splits one physical alignment across both ends of its scale — fold to
0–90°.

**Reference:** `docs/todo/SPATIAL_ANISOTROPY_PLAN.md` Decision 11;
`old-R-shiny-version/inst/modules/sources/segment/{binariseTracks.R,py/binarise_tracks.py}`.

---

## Branch populations in the UI (gating page + plot picker)

**What.** `branch` is a real pop type — the sidecar, the pop map and `pop_df(img, "branch", …)` all
work — but no UI surface can reach it. `/api/gating/channels` and `/api/plots/populations` both
enumerate `label_props` value_names, and a branch value_name is a *segmentation* (`SHG`) that
usually has no per-cell table, so the picker looks for `B__branch`/`T__branch` and finds nothing.
The four branch types also list flat rather than as subpopulations of one `SHG` pop.

**Why deferred.** Partial fixes were written during the anisotropy work and then reverted
deliberately: repairing one link of a chain that is broken end to end just hides the breakage, and
the readouts that motivated it turned out to belong in a notebook anyway (where `pop_df` reaches
branch pops directly, no picker involved).

**Revisit when.** Someone wants to select branch populations in the app — most likely the moment a
branch-level measure (`anisotropy`, or `branch-weight` from the network-weights port) is worth
plotting on a module page. The work is: value_name enumeration that includes
`img_branch_value_names` (that helper exists), a `_resolve_vn` that doesn't silently redirect a
branch value_name to the active cell one, a `channels` route branch that reads the branch sidecar,
and a real parent/child hierarchy for the auto-created branch-type pops.

**Reference:** `docs/todo/SPATIAL_ANISOTROPY_PLAN.md` → *Known limitation*; `docs/POPULATION.md`.

## Reducing image bit depth on import (8-bit conversion)

**What.** Convert images to 8-bit at import to shrink them, choosing an intensity window automatically.

**Why NOT — this is a non-goal, not a deferral.** It was built, measured and removed. Two findings
killed it, both on real data:

1. **The size win it was for is mostly available losslessly.** Measured whole-store on a real 16-bit
   acquisition: the codec alone (`blosc/zstd-3` + byte shuffle vs bioformats2raw's `blosc/lz4-5`) takes
   33% off with no pixel change and no extra write time. Discarding 8 bits then buys only a further
   27% — not the ~2x the design assumed.
2. **Every possible window is wrong in some way.** Per-channel skews the cross-channel ratio the
   analysis measures; one window per image leaves a dim channel with `its_max / brightest_max * 255`
   levels (measured: 87/255); a set-wide window needs a number nobody can produce without eyeballing a
   histogram, or a nominated reference that measurement showed spans 2.46x across nine movies. So the
   real choice was "low dynamic range **or** break the ratio" — and a reporter that is genuinely off
   must read as off.

**Revisit when.** Only if storage becomes a binding constraint that lossless compression cannot meet —
and then the honest form is a fixed divide by the detector's bit depth (data-independent, identical for
every channel/image/set, exactly invertible), not an automatic window. On the data measured, that puts
signal near 50/255.

**Also worth knowing:** 4 of 9 real movies had a channel **saturated at acquisition**. No window,
derived however cleverly, recovers those — there was always a ceiling on what this line of work could
buy.

**Five window designs were tried and removed**, which is the reason this is a non-goal rather than a
"needs more thought": two hand-tuned percentiles (a different gain per channel, and at the default of
100 one hot pixel setting the window); a typed absolute window (nobody can produce the number); a
set-wide window derived from a nominated reference image × a leeway multiplier (import ORDER became
load-bearing, and the nomination was a guess); a per-channel window (justified by a recorded window
that nothing downstream ever read); and one shared window per image (correct about the ratio, and the
dim-channel cost above is then unavoidable arithmetic).

**Reference:** `zarr_utils.store_compressor` (the measured codec table that replaced it);
`intensity_utils` retains the whole-stack histogram helpers the work produced, now used by AF
correction, segmentation normalisation and the import saturation check. `image_window` and
`rescale_stack_to_uint8` are gone (git history). `is_saturated` was removed with the feature and then
**brought back for its own sake**: clipping at acquisition is worth reporting whatever the bit depth,
and it is now a standard QC pass on every import (`saturation_run.py`, `qc.jl::saturation_qc_findings`).

## Flag clipping per CELL, not as a voxel fraction

**What.** Report how many CELLS have clipped voxels in a channel, at `segment.measureLabels` (or
`tracking.track_measures`), instead of only what fraction of a channel's voxels piled at the detector
ceiling at import.

**Why it is better.** A voxel fraction is not answerable. "0.004% of signal voxels clipped" gives a
user nothing to decide; "17 of 2 400 cells have truncated intensity in CH2" does — one affected cell in
ten matters when you are comparing means, while 500 clipped voxels spread over a 377 M-voxel movie may
not. It would also give the import check's threshold something real to be calibrated against: today
`_SATURATION_WARN_SIGNAL_FRAC` is deliberately a smoke alarm ~140x above anything measured, because no
material clipping case exists in any data we have (worst observed: 0.007% of signal voxels, across all
36 channels of nine movies).

**Why deferred.** It needs labels, and specifically labels good enough to trust a per-cell count from —
so it waits on segmentation being reliable on real data (Dominik, 2026-08-04). Building it against
uncertain segmentation would produce a number nobody can act on either, for a different reason.

**Revisit when.** Segmentation is dependable on real acquisitions. The import check stays as it is until
then: detection + metrics on every import, a finding only for unmistakable damage.

**Reference:** `qc.jl::saturation_qc_findings` / `_SATURATION_WARN_SIGNAL_FRAC`,
`intensity_utils.saturation_stats` (both denominators), `#456`/`#462`.

---

## Temporal downsampling / overlapping tracklets for behaviour

**What.** Two celltrackR knobs the old R framework exposed on HMM: `skipTimesteps` (treat 10 s/frame
data as if it were 30 s/frame, so movies acquired at different rates can be compared) and
`subtrackOverlap` (generate overlapping tracklets rather than disjoint ones). The old stack could
offer them for free because it computed track measures **on the fly**, so the knobs were just
arguments passed down into celltrackR at analysis time.

**Why deferred.** The new stack precomputes `live.cell.*` at native resolution
(`app/src/tasks/behaviour/track_measures.jl`), so there is nothing left to push the arguments into —
they were silent no-ops on `behaviour.hmm_states` and were dropped rather than left as controls that
did nothing. Restoring the capability is therefore not a parameter, it is a storage decision, and
none of the three ways in is obviously right:
- a track-measures variant that recomputes speed/angle over every k-th position (subtrack stride +
  overlap) and writes `live.cell.speed@kN`-style columns the HMM can select;
- a resampling step that emits overlapping sub-tracks as first-class rows;
- a per-image frame-interval normalisation, so cross-rate comparison needs no manual skip at all.

Each multiplies either the column count or the row count of every tracked image, which is why the
storage/UX has to be settled before anything is built.

**Revisit when.** Someone actually needs to compare behaviour across acquisitions taken at different
frame intervals — that is the case the knobs existed for, and it is the one that picks between the
three options above. (Option (c) is the only one that solves it without asking the user to guess a
stride.) Nobody has hit it on current data.

**Reference:** `docs/TRACKING.md`, `app/src/tasks/behaviour/track_measures.jl`. Old implementation:
`old-R-shiny-version/R/trackHelpers.R` (`celltrackR::subtracks(x, steps.subtracks, steps.overlap)`),
called from `inst/modules/sources/behaviourAnalysis/hmmStates.R:42-43`.

---

## Adding entries

Add an entry when you set something aside — a known-better approach, a non-goal, or work waiting on a
trigger. Each one carries:
- **What** it is
- **Why deferred** — complexity, ecosystem maturity, not needed at this scale, or an explicit decision
- **Revisit when** — the concrete condition that would change the answer (for a non-goal: "someone
  wants it", plus what the work actually is now, so the next reader doesn't re-derive it)
- A **reference** to the relevant doc or external resource
