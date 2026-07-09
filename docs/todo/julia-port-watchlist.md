# Julia Port Watch List

> Living checklist distilled from `docs/todo/python-audit-report.md`. Purpose: recheck periodically
> whether a Julia drop-in has appeared for each **blocked** item. Each "cannot port (yet)" row names
> the *specific* package/capability to watch for, so the recheck is a concrete search, not a re-audit.

## Governing rules (read before porting anything)

1. **Only port what the current framework actually uses.** Do **not** port an algorithm that isn't
   wired into a live task/module (e.g. `findSignalPeaks`, spatial analysis) — that imports unused
   code = net complexity. When such a module is *actually needed*, build it in Julia from scratch
   where feasible (see the "future modules" table for the target package), rather than porting the
   Python/R speculatively now.
2. **Porting does not shrink the Python env** — the weight (`torch`/`cellpose`/`scanpy`/`btrack`/
   napari) is all KEEP_PYTHON. The payoff of any port is *consolidation + fewer cross-language
   seams*, never dependency removal. Weigh every port against that (modest) benefit.
3. **Don't reimplement to create a second variant.** If porting would duplicate a helper for one
   task, keep the task Python (CLAUDE.md divergent-reimplementation rule).
4. **h5ad stays** — it's the neutral dual-native format (Julia `HDF5.jl` + Python `anndata`). The
   Python lock-in is the *clustering algorithm*, not the storage format. See the audit report's
   *Bottom line* section.

---

## A. In-framework components — current Julia status

| Component | Status | Note / what it would take |
|---|---|---|
| Gating / logicle / tree recompute / `pop_df` | ✅ **Julia-native** | Done. Julia is the sole gate evaluator. |
| H5AD **read** (`LabelProps` chain) | ✅ **Julia-native** | Done (`app/src/label_props.jl`), parity-tested vs Python. |
| Numeric `obs` **write** (`save!`) | ✅ **Julia-native** | Done. Julia writes float64 obs directly via `HDF5.jl`. |
| Track measures (celltrackR subset) | ✅ **Julia-native** | Done (`track_measures.jl`). |
| **`drift_correct`** | 🟡 **Portable now — needs I/O layer built** | Compute ports cleanly (`SubpixelRegistration.jl`). Requires building cecelia's OME-ZARR pixel-I/O layer on **Zarr.jl** (which exists) — an implementation task, **not** an ecosystem blocker. See **W1**. |
| **`af_correct`** (core) | 🟡 **Portable now — needs I/O layer built** | Compute ports (`ImageFiltering.jl`/`ImageMorphology.jl`). Same **W1** I/O-layer build. Optional features also need **W4**. |
| `cellpose_correct`, `cellpose` (segment) | 🔴 **KEEP_PYTHON** | cellpose + torch DL. No Julia equivalent — see Do-Not-Port. |
| `bayesian_tracking` | 🔴 **KEEP_PYTHON** | btrack (C++/Python). No Julia equivalent. |
| `clustPops` / `clustTracks` (Leiden) | 🔴 **KEEP_PYTHON** | scanpy/leidenalg/UMAP/PAGA/ComBat/Harmony. **Blocked on: no mature Julia stack.** Watch item **W2**. |
| `measure_labels` | 🔴 **KEEP_PYTHON** | regionprops compute *could* port, but it **creates the `.h5ad` (X/var)** — Julia can't write X. Also optional trimesh mesh path (**W3**). Stays Python. |
| `track_props` writer, `write_categorical_obs` | 🔴 **KEEP_PYTHON** | Pure `.h5ad`/categorical creation — Julia can't emit `X`/categoricals. Optional low-value: teach Julia to write anndata-format `X`+categoricals (parity-tested) to drop these subprocesses; not strategic. |
| H5AD `X` / new-file / categorical **write** | 🔴 **Python by design** | Julia writes numeric obs only; `X`/`var`/new files/categoricals go through `anndata`. Optional to change (see above). |

Legend: ✅ done · 🟡 portable but blocked · 🔴 stays Python.

---

## B. Watch list — the specific Julia drop-ins to recheck

Recheck these periodically. When one lands (and is mature), the linked item(s) above become portable.

### W1 — cecelia's OME-ZARR **pixel** I/O layer (implementation task, NOT an ecosystem gap)
- **Unlocks:** `drift_correct`, `af_correct` (the only in-framework Julia-portable tasks).
- **Status: the package exists — `Zarr.jl` reads AND writes zarr pixel arrays (v2 and v3).** This is
  **not** a "wait for the ecosystem" item. What's missing is *cecelia-side code*: `omezarr.jl`
  currently reads only `.zattrs`; nobody has written the Julia equivalent of `zarr_utils.py`'s pixel
  path — lazy chunked reads, `create_multiscales` pyramid **writing** (downsampling + per-plane
  chunking, load-bearing for napari perf), and dual bioformats2raw/flat layout detection. All of
  that is buildable on `Zarr.jl` today.
- **One real caveat (not blocking):** `Zarr.jl`'s **v3 sharding codec** is on its roadmap, not yet
  implemented. cecelia doesn't use sharding either (it's deferred in `docs/FUTURE.md`; the `zarr>=3`
  pin is for the v3 API, not sharding), so this does not block the port — plain `Zarr.jl` covers the
  need. (`Zarrs.jl` has sharding but is a Rust crate wrapped via `zarrs_ffi`. Whether a precompiled
  Rust dep behind a C FFI is acceptable is a *decision for Dominik / ARCHITECTURE.md* — it's a transitive
  binary dep like the existing torch/HDF5/JVM deps, not "Rust in the maintained stack," and Rust has
  an existing carve-out (Tauri). Not needed here regardless, since we don't use sharding.)
- **Decision note:** this is a real, non-trivial build (multiscale write + chunking + layout +
  OME-XML sidecar) whose only payoff is consolidation (rule 2) — it does not shrink the env. It's a
  *cost/benefit* decision, not a *feasibility* one. If pursued, `drift_correct` is the cleanest first
  target (pre-identified), and the compute needs `SubpixelRegistration.jl` added to `app/Project.toml`
  (+ `instantiate` from `api/`), with golden validation against the current Python output.

### W2 — Mature native-Julia Leiden clustering stack
- **Unlocks:** `clustPops` / `clustTracks` → fully Julia; **this is the single highest-leverage
  item** — it's the algorithm keeping a Python *data* path alive (not h5ad).
- **Gap (verified July 2026):** no mature native stack. `bicycle1885/Leiden.jl` (native, small/
  unproven), `pitsianis/Leiden.jl` (unregistered C++ wrapper, WIP), `JuliaCommunity` (wraps Python
  `leidenalg` via PyCall). No Julia **UMAP** with scanpy parity guarantee (`UMAP.jl` exists,
  unmaintained-ish), **no PAGA**, **no ComBat**, **no Harmony**.
- **Watch for:** a registered, maintained native Leiden with resolution-parameter parity to
  `leidenalg`; a maintained `UMAP.jl`; Julia batch-correction (ComBat/Harmony analogues). All must
  land before trading scanpy (else cluster labels diverge + batch correction is lost).

### W3 — Non-convex mesh collision / distance (FCL-equivalent)
- **Unlocks:** the mesh-based cell–cell **contact/cluster** detection in a future spatial-analysis
  module (per `DATAMODEL.md` §Mesh files, the `cellContactsMeshes.R` equivalent).
- **Gap (verified):** no Julia drop-in for min-distance/collision between arbitrary **non-convex**
  triangle meshes. `GJK.jl`/`Meshes.jl` GJK are convex-only; `ImplicitBVH.jl` exists but is
  **broad-phase only** (no narrow-phase triangle-triangle distance / non-convex containment); no
  `FCL.jl` binding.
- **Watch for:** an `FCL.jl` binding, **or** a native BVH+narrow-phase mesh-distance package. Until
  then, options are: keep a Python/trimesh sidecar for collision only, or a convex-hull GJK
  approximation (validate accuracy loss on real data first).
- *Note:* the rest of the mesh path (marching cubes via `Meshing.jl`, volume via `GeometryBasics`,
  hull via `Quickhull.jl`, descriptors, KD-tree neighbours via `NearestNeighbors.jl`, 3D Delaunay
  via `TetGen.jl`) **is** feasible in Julia today — only collision is the hard gap.

### W4 — Minor `af_correct` optional-feature gaps
- **Unlocks:** the *optional, default-off* AF features (mandatory AF path is fine once W1 lands).
- **Gap:** no Julia `rolling_ball` background subtraction; no `denoise_wavelet` (BayesShrink/
  VisuShrink) or `denoise_tv_chambolle` (Wavelets.jl exists but the shrink/TV variants need
  hand-porting).
- **Watch for:** an ImageJ-style rolling-ball package; wavelet/TV denoise in the Julia image stack.
  Low priority (features are off by default).

---

## C. Do-NOT-port (permanent Python, barring a watch-item landing)

| Item | Why | Would only change if |
|---|---|---|
| cellpose + torch (segment, denoise) | DL inference; no Julia equivalent | A Julia DL-segmentation stack with model parity appears (not on the horizon) |
| btrack (bayesian tracking) | C++/Python multi-object tracker | A Julia Bayesian-tracker port exists |
| scanpy / leidenalg (Leiden clustering) | see **W2** | **W2** lands |
| napari (viewer) | see `docs/FUTURE.md` → *Julia-native image viewer* | A GLMakie pyramid-LOD viewer is built (months of work) |
| anndata `X`/new-file/categorical writer | Julia writes numeric obs only; new encodings are Python's job | Optionally: parity-tested Julia anndata-format writer (low value — scanpy keeps Python anyway) |

---

## D. Future modules (unported R) — build in Julia *when needed*, do not port speculatively

Per rule 1, these are **not** to be built now. Listed so that *when a real need arises*, the target
Julia approach is already scoped. Full detail: audit report *Unported legacy modules* + *Spatial/
mesh feasibility*.

| R module / method | When built, target | Blocker to note |
|---|---|---|
| `signalAnalysis.findSignalPeaks` | Julia-native (DataFrames + Statistics; obs-column output) | None — clean when needed. **Not used anywhere today; do not build yet.** |
| `spatialAnalysis` (neighbours/contacts-graph/clusters/regions) | Julia (`NearestNeighbors.jl`, `Graphs.jl`, `TetGen.jl`) | Mesh contact detection needs **W3**; `cellNeighbours` squidpy path may stay Python |
| `behaviourAnalysis.clusterTracks` / `createPseudotime` | clusterTracks → Julia (celltrackR-adjacent); pseudotime → Python (scanpy DPT) | pseudotime pulls scanpy (**W2**-adjacent) |
| `objcl` / `pixcl` / `trainModels` | Python (apoc / n2v / TensorFlow) | Python-only DL/OpenCL; no Julia path |
| `importFlow` (FCS) | Julia `FCSFiles.jl` or Python `flowkit` | Either works; pick when needed |

---

*Last audited: 2026-07 (Julia 1.12, ecosystem checked live). Re-audit the W-items on each ecosystem
sweep.*
