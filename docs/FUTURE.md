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
`docs/prompts/python-audit-report.md`. Julia candidates: `GLMakie.jl`, `Mousetrap.jl`, `Zarr.jl`.

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

**Reference:** `docs/POPULATION.md` → *Gating plot — rendering & UX hacks*. (Was `docs/TODO.md`
`#00087`, and before that a duplicate `#00003`; moved here because a deliberate non-goal is not open
work.)

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

**Reference:** `docs/ai-assist/OBSERVER.md`. (Was `docs/TODO.md` `#00084`; moved here because it is
conditional on something that may never happen.)

---

## Adding entries

Add an entry when you set something aside — a known-better approach, a non-goal, or work waiting on a
trigger. Each one carries:
- **What** it is
- **Why deferred** — complexity, ecosystem maturity, not needed at this scale, or an explicit decision
- **Revisit when** — the concrete condition that would change the answer (for a non-goal: "someone
  wants it", plus what the work actually is now, so the next reader doesn't re-derive it)
- A **reference** to the relevant doc or external resource
