# Future Optimisations

This document tracks known optimisation opportunities that are deliberately deferred — not
forgotten, not ruled out, just not worth the complexity at the current scale or ecosystem
maturity. Each entry states what it is, why it was deferred, and the concrete condition under
which it becomes worth revisiting.

This is distinct from `docs/TODO.md`: TODO is the working backlog of things we intend to do;
FUTURE is the set of known-better alternatives we have consciously *set aside*. Do not implement
anything listed here without explicit instruction — this is a reference document, not a backlog.

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

## Adding entries

When deferring a known-better approach during implementation, add an entry here with:
- What the approach is
- Why it was deferred (complexity, ecosystem maturity, not needed at current scale)
- The concrete condition under which it becomes worth adopting
- A reference to the relevant doc or external resource
