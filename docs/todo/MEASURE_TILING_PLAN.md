# Measure labels — bounded-memory tiling by bounding-box ownership

Status: **P1–P3 built** (2026-10-11, branch `fix/measure-bbox-tiling`, PR #1611); P0 skipped (P1
replaced it the same day); P4 trigger-gated. "How it works" is promoted to
[`docs/SEGMENTATION.md`](../SEGMENTATION.md) → *Bounded memory: block ownership*; this plan stays as
the record of why.

## Goal

`segment.measureLabels` must measure any image whose label plane and one block of pixels fit in RAM,
which today it cannot, and must scale to tile scans with **millions of cells**. Every label in the
store gets exactly one row, with values identical to a whole-plane skimage run.

## Why (the incident, 2026-10-11)

`cellposeMeasure` on `4rNbMp/f8gzA2` (a 2D tile scan, 25 channels, 16898 × 20329 px, uint16, 109,982
cells) segmented fine. Then `measureLabels` was **OOM-killed** (`anon-rss` 28 GB on a 31 GB machine).
It logged nothing, wrote no `labelProps/`, and the run just looked like it had stopped.

Cause: `MeasureUtils.measure_from_zarr._measure_one` (`python/cecelia/utils/measure_utils.py`) reads a
**whole timepoint, all channels**, and casts it to float32. Here that is 17 GB of uint16 plus a 34 GB
float32 copy. The per-channel intensity loop downstream was already one channel at a time, so only the
upfront read is the problem.

Two related defects:
- **The GUI tiling controls do nothing.** `measure_labels.json` includes the shared `imageTiling`
  fragment (`blockSize` 512, `overlap` 64), and `measure_labels.jl` passes both through, but
  `measure_labels_run.py` documents them as "informational; measurement is per-timepoint" and
  `MeasureUtils` never reads them. `docs/SEGMENTATION.md` → *$include template system* claims cellpose
  and measureLabels "share" these settings, which overstates it.
- **The worker budget is wasted on one timepoint.** `nThreads` (CPU-pool headroom, capped at 8) is
  spent across timepoints, so a single-timepoint tile scan uses one worker and leaves the rest idle.

This revises [`ZARR_STREAMING_PLAN.md`](ZARR_STREAMING_PLAN.md) **Decision 1** ("granularity = one
timepoint, in RAM") **for measure only**. A tile-scan frame is not a bounded unit.

## Evidence (2026-10-11, one 1024² block of `f8gzA2`, 558 cells, single thread)

| Step | Block | Per cell | × 1M cells |
|---|---|---|---|
| Morphology (current 2D prop set, skimage `regionprops_table`) | 0.53 s | ~0.95 ms | ~16 min |
| Mean intensity, 25 ch, `regionprops_table` per channel | 0.43 s | ~0.77 ms | ~13 min |
| Mean intensity, 25 ch, `np.bincount` sum/count | 0.09 s | ~0.16 ms | ~3 min |
| Median intensity, 25 ch, `regionprops_table` | 0.60 s | ~1.1 ms | ~18 min |
| `scipy.ndimage.find_objects` (labels only) | 0.005 s | — | seconds |
| Read the block, 25 ch | 0.03 s | — | — |

Object extent (max of bbox height/width) in that block: p50 40 px, **p99 69 px, max 89 px**. So the
old R default overlap (64) would already drop cells on this image.

Threads vs processes was measured in P2 (below). The median cost at full-image scale is still not
measured.

## Options considered

**A. Old R tiling** (`old-R-shiny-version/inst/py/measure_utils.py` → `measure_from_zarr`). It tiled
with block + overlap, ran `clear_border` per tile, then `drop_duplicates('label')`. **Rejected.** Any
object wider than the overlap that straddles a seam touches a border in every tile, so it is cleared
everywhere and silently loses its row. It also clears objects at the true image edge, which changes
cell counts. And the overlap is a guess the user has to make.

**B. Pure block streaming** (additive reductions per zarr block). This is exact and needs no overlap
for anything that is a sum: area, centroid, bbox, intensity sum/mean/min/max/std, and raw moments,
which give the axis lengths, eccentricity and orientation. **Rejected as the architecture.**
Perimeter, convex area, solidity, Feret, Euler number, median/percentiles and every 3D mesh measure
are not additive. Supporting them means re-implementing skimage `regionprops` with our own cross-block
merge logic, which is a permanent maintenance burden and a silent-divergence risk. **Kept as the
intensity kernel** inside C (Decision 4).

**C. Bounding-box ownership. Chosen.** One cheap labels-only pass finds every object's bbox. Each
object is **owned** by the one block that contains its bbox start. Each block is read with a margin
derived from the real object extents, unmodified skimage runs on it, and only owned objects are kept.
The result is exact (identical to whole-plane), has no dropped objects and no user-guessed overlap,
and memory per worker is about (block + 2·margin)² × channels.

## Decisions (2026-10-11)

1. **Ownership, not clearing.** An object belongs to the block containing its bbox **start** corner
   (min Y, min X; plus min Z in 3D if Z is ever blocked, which it isn't, see 7). Exactly one block
   measures it. Nothing is ever dropped for touching a seam or the image edge. This deliberately
   breaks with R's `clear_touching_border`.
2. **The margin comes from the data.** Pass 1 records each object's bbox, and the margin is the max
   object extent beyond its owning block's end, capped by a sanity ceiling (`MARGIN_CAP`, e.g. 512
   px). Objects whose extent exceeds the cap are **outliers**: each is measured from its own bbox crop
   and counted in the log (`>> 3 objects larger than 512 px measured individually`). Never dropped.
3. **skimage stays the source of truth for morphology.** Run it unchanged on the block's label array,
   then filter to owned labels. Coordinate-valued outputs (centroid, and bbox if it ever comes back)
   get the block origin added. Golden check: block-mode output equals whole-plane output, column for
   column (P1).
4. **Mean intensity uses `bincount`, not `regionprops`.** Within a block, sum and count per owned
   label per channel go through `np.bincount(labels, weights=channel)`. That is 5× faster, measured.
   **Median** and anything non-additive stays on `regionprops` over the block crop, which is exact
   because the owned object lies entirely within block + margin.
5. **Gaussian pre-filter: read with a halo, then trim.** The halo is `ceil(4σ)` beyond the margin,
   smoothed per channel then cropped, which gives byte-identical results to whole-plane smoothing.
   The halo is never a user setting.
6. **One channel at a time inside a block** where that bounds memory, i.e. the median path. The
   bincount path may read all channels of a block at once (25 × 1024² × 4 B ≈ 100 MB); pick by the
   P2 measurement.
7. **Blocks are XY only.** The whole Z extent of a block column is read, matching the cellpose
   convention ("no Z tiling", `docs/SEGMENTATION.md`). 3D extended measures (trimesh) work per owned
   object inside the block crop, unchanged.
8. **No tiling params in the measure GUI** (revised while building P3). `overlap` is computed (D2).
   `blockSize` was dropped too: a composite passes its FULL params dict to every step and the merged
   form dedups by key, first step wins (`docs/MODULES.md` → *Composite tasks*). So inside
   `cellposeMeasure` a measure `blockSize` would silently be cellpose's tile size. The block is
   `block_size_for(labels)`: 1024 rounded up to whole chunks of the label store (read with
   `zarr_utils.store_xy_tile`), so a block read never over-fetches.
   Python still honours a `blockSize` param (tests, REPL).
9. **The worker budget stays the Julia CPU-pool headroom** (`measure_labels.jl`, capped at 8). That
   is consistent with ZARR_STREAMING Decision 3: no second, uncapped pool. Workers iterate **blocks**
   (across all timepoints), not timepoints. Threads vs processes is decided by measurement (P2), not
   assumed.
10. **Output and contract unchanged.** Same `.h5ad` columns, the label id as index, the `pass` column,
    the `t` column, and secondary label types (`nuc`/`cyto`/`halo`) measured on the same block crop,
    keyed by the base object's id, with the same owned-id filter. Downstream (`label_props.jl`, gating, tracking)
    must see no difference beyond the job succeeding.

## Phases (each independently shippable)

### P0: stop the bleed (optional, about 20 lines)
Read each channel inside `_measure_intensities` / `_measure_secondary_intensities` instead of the
whole float32 stack upfront, and smooth per channel. Peak memory becomes the label plane plus one
channel (≈ 2.8 GB for `f8gzA2`). That unblocks `f8gzA2` now. It does **not** scale to millions of
cells or 3D tile scans, so P1 still replaces it. Skip it if P1 lands soon.
- Check: `f8gzA2` measures to completion. Peak RSS logged. Existing measure tests stay green.

### P1: ownership tiling, single worker
- Pass 1 `scan_bboxes(label_arr, t)`: per chunk `ndi.find_objects`, then merge min/max per label into
  `bbox[label] = (y0, x0, y1, x1[, z0, z1])`. It also yields the object list, so the label plane is
  never held whole.
- Plan blocks: owner = block of `(y0, x0)`; margin per D2; outliers listed.
- Per block: read labels + image (+ halo), optionally smooth, morphology on the label crop filtered
  to owned labels, intensities per D4, offset coordinates, append.
- Outliers: per-object crop, same code path.
- Remove the old whole-timepoint `_measure_one`. Do not keep two paths.
- **Golden test**: a synthetic 2D and 3D label image with objects straddling seams, objects at the
  image edge, one object larger than `MARGIN_CAP`, two passes, and a secondary label type. Block-mode
  output must equal the whole-plane reference (computed in the test with plain skimage) for every
  column. That test is the guard for D1 to D5.
- Check: `f8gzA2` row count == label count (109,982). Peak RSS ≪ 1 GB at `blockSize` 1024.

### P2: worker model (measure, then decide)
Run on `f8gzA2` at 1, 2, 4 and 8 workers, **threads** (`ThreadPoolExecutor`) vs **processes**
(`ProcessPoolExecutor`, each worker opening the zarr itself). Record wall-clock and peak RSS per
config. Separately time morphology-only vs intensity-only to see which step blocks on the GIL.
Decide:
- If threads scale ≥ ~0.7× linear: keep threads, which is simpler and needs no pickling.
- Otherwise use processes, returning per-block DataFrames. Keep the budget from D9.
Record the table in this plan.

**Measured (2026-10-11, `f8gzA2`, every 3rd block = 85 blocks / 36,132 cells, i9-13950HX 24 cores,
mean intensity, no Gaussian), pass 2 wall-clock:**

| workers | threads | processes (spawn) |
|---|---|---|
| 1 | 39.0 s | 35.6 s |
| 2 | 24.8 s | 18.5 s |
| 4 | 25.7 s | 10.3 s |
| 8 | 33.6 s | 6.7 s |

Threads stall at 2 and get slower past 4: the per-object props hold the GIL. Processes scale 5.3× at
8. **Decided: processes for pass 2, `spawn` on every OS** (Windows has nothing else and it is macOS's
default; forking a process that has run blosc/thread pools can deadlock on Linux). The bbox scan (pass 1) is
numpy-bound and stays on threads: 9.2 s → 2.0 s at 8.

**Full image, processes:** 110k cells in **23 s at 8 workers**. Peak RSS summed over the process
tree was **1.76 GB at 4 workers and 3.1 GB at 8**. The old code was OOM-killed past 28 GB. Output at
4 and 8 workers is byte-identical.

### P3: GUI + docs
`measure_labels.json` per D8. Rewrite `measure_labels_run.py`'s param docstring. Fix the
`docs/SEGMENTATION.md` "share" claim and add a *Measure labels → tiling* section (ownership, margin,
outliers). Add a `docs/inventory/` entry for `scan_bboxes` if it becomes a shared helper.

### P4 (trigger-gated): incremental re-measure
`segmentCorrectMeasures` re-measures the whole image after hand edits. With bboxes and ownership, it
could re-measure only the edited labels' blocks, which narrows the frame-scoped re-measure already
tracked in `docs/TODO.md` → *Frame-scoped `segment.measureLabels` for the correction composite*
down to the edited objects. Build it with that item, not separately.

## Found while building (not fixed here)

- **A 2D image stored with a singleton Z was measured as 3D** (`is_3d = 'Z' in dim_order`): the 3D
  prop set plus an all-zero `centroid_z`. Fixed in its own change: `measure_from_zarr` now uses
  `DimUtils.is_3D()` and drops the singleton Z at read time, like segmentation's `drop_z`. Existing
  h5ad files keep the 3D shape until the image is re-measured.

## Risks / open questions

- **Millions of objects in pass 1**: the per-label bbox merge must be vectorised (arrays indexed by
  label id, or a sorted merge), not a Python dict loop. A 5M-entry int32 array is about 100 MB, which
  is fine.
- **Label ids sparse or very large** (uint32 max): use `np.unique` remap per block rather than
  `bincount` up to max id.
- **The h5ad writer at millions of rows × ~60 columns** is a few hundred MB. Not a blocker, but check
  the `_to_anndata` concat isn't quadratic.
- **3D tile scans**: a block column holds the full Z. A 200 × 1024² × 25 ch float32 block is about
  21 GB, too big. D6 (one channel at a time) must apply there, giving about 0.8 GB per channel. Verify
  on a real 3D scan before calling 3D done.
