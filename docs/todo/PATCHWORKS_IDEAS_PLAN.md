# Ideas from patchworks — automatic tiling and the image-tiling params

Status: **in progress** (2026-10-29), branch `patchworks-audit-archive`. Fix 1 (read only the
models' channels) and Fix 3 (sparse Z-stitch) are **built** (#1825). D1 + D2 (Auto tile size and
overlap) and P2 steps 2–4 are **built** on `auto-tiling`, with cellpose's cost measured; coastal's
Auto is the whole frame until a second frame size is measured. Next: D3 (seam rule), D4
(normalisation reframe), P3–P4.

## Goal

Nobody should have to guess **Tile size** or **Tile overlap**. Today the honest advice is "try it and
see if you run out of memory". Cecelia should pick both from the machine and the image, record what it
picked, and recover by itself when the guess is wrong. On the way, take whatever else
[patchworks](https://github.com/imcf/patchworks) does better for tiled segmentation.

The original audit ask is archived at
[`docs/archive/PATCHWORKS_AUDIT_PROMPT.md`](../archive/PATCHWORKS_AUDIT_PROMPT.md). It was not run as
written: Cecelia already tiles with overlap and IoU seam stitching, so "adopt their merge" was the wrong
question.

## Sources

- **patchworks**: read at `706cd99` (2026-10-28, 711 commits). This was a source read only; nothing
  was installed or run. Licence: GPL-3.0 since `89b8394` (2026-06-30). Before that, MIT was declared
  in `pyproject.toml` only, with no LICENSE file. **Clean-room only.** We reimplement from the
  behaviour described here and copy no code. Each reimplemented idea cites the repo URL in a comment
  (CLAUDE.md → *Cite sources*).
- **Cecelia usage**: the tiling params of all 99 segmentation/measure runs recorded in
  `ccid.json` `funParams` / `funParamsByName` across the dev projects (2026-10-29).

## What the usage data says

| `blockSize` / `overlap` / `labelOverlap` | Runs | Reading |
|---|---|---|
| 4096 / 0 / 0 (or absent) | 88 | Tiling switched **off**. Every frame here is under 4096 px, so it is one tile |
| 1024 / 64 / 0 | 4 | Includes the tile scan `4rNbMp/f8gzA2` (16898 × 20329): ~340 tiles, **seam stitch off** |
| 512 / 64 / 0.25 or 0 | 6 | The spec defaults |
| 4096 / 64 / 0 | 2 | |

- **On intravital movies, people max out the tile size.** Frames there are about 1000 px, so 4096
  means one tile. This does **not** mean tiling is unwanted: tile scans need it (project `4rNbMp`,
  set *Lymphoid organs* `UZD4d7`: 16898 × 20329, 6543 × 12977 and 7293 × 7848 px, 25–38 channels).
  The 512 default fits neither case.
- **The tile scan was segmented with stitching off.** Seam stitching only started working, and
  `labelOverlap` only became 0.25, in `9b3946f5` (2026-10-03). Before that it was 0, and saved
  params keep their old value. So a run on 2026-10-11 still had 0. Every cell crossing one of its
  ~340 tile seams has two ids.
- `normaliseToWhole = false` appears in 4 runs, all at 4096. There it means **per frame**, not per
  tile (see P1, D4).

## What patchworks does — the parts that matter here

Citations are patchworks paths at `706cd99`.

1. **Auto tile size** (`src/patchworks/_chunks.py`).
   - **Host budget** = the minimum of `SLURM_MEM_PER_NODE`, `SLURM_MEM_PER_CPU × cpus`, cgroup
     headroom and `psutil...available`. Cgroup headroom is limit minus anon RSS, at the tightest
     cgroup level. Fallback is 8 GiB.
   - **GPU budget** = free VRAM × 0.8.
   - **Generic sizer**: a fixed byte target per tile, 64 MiB by default.
   - **Cellpose sizer**: `(min(vram, host) − 2 GiB model) / 20` bytes of raw input. Cost is scaled
     by `(30/diameter)²` because cellpose rescales to diameter 30, and the side is at least
     4 × diameter. The 20× factor is not calibrated; their own docstring says so.
   - There is no rounding to chunks and no maximum.
2. **OOM** (`_gpu.py:71-128`). On a GPU out-of-memory it frees cached models and the torch/cupy
   pools, then retries the same tile up to 4 times with a linear backoff. It never splits the tile.
   Host-RAM OOM is not handled.
3. **Empty tiles** (`_io.py:444-484`, `_occupancy.py`). An Otsu threshold from three diagonal
   windows, then a max-pooled occupancy map; a tile is skipped if its max ≤ threshold. This is
   **lossy**: a cell dimmer than the Otsu cut is dropped silently.
4. **Halo (overlap) size** (`_chunks.py:48-102`, `workflow/scripts/_pw.py:336-458`). Derived from
   object size, in this order:
   - 1.1 × p90 of object bbox extents from an earlier run;
   - else measured on sample tiles;
   - else 1.1 × cellpose diameter;
   - else 30.
   It is capped at tile/2. `suggest_overlap` (`_autotune.py`) picks the smallest halo whose tiled
   result matches an untiled crop at object F1 ≥ 0.99.
5. **Seam merge, `stitch="iou"`** (`_merge.py:422-469`). A pair joins when the two are **mutual best
   matches** and `intersection / min(area)` ≥ 0.5. That is overlap of the smaller object, not IoU,
   because a cut fragment lies inside the whole cell. It also logs the p10 and median agreement. A
   "rescue" joins a sub-`min_voxels` fragment to its single best partner rather than letting the size
   filter drop it: they measured 7% orphans on seams vs 0.2% inside tiles. Their default
   `stitch="touch"` has no overlap gate and merges touching cells. Our IoU gate is already better than
   that default.
6. **Seam report** (`_seams.py`). It compares the orphan rate on seam planes with control planes
   halfway through each tile, and warns above 2×.
7. **Review queue** (`_review.py:764-903`). It scores objects from the tables alone:
   - orphaned from a parent: 3.0
   - meets another object exactly at a seam: 2.5
   - partly inside a parent: 2.0+
   - wrong child count: 2.0
   - size outlier (robust z > 3.5): 1+
8. **Relations** (`_tables.py:848-945`). Each child takes the parent with the most voxel overlap,
   and gets `<parent>_id`, `<parent>_overlap` (a fraction) and an optional nearest-parent distance.
9. **Label pyramid**: downsampled by **mode** (most frequent non-zero id per block). Ours is nearest
   (`zarr_utils.write_label_pyramid`).
10. **Image-wide params measured once** and passed to every tile (intensity range, Otsu). We already
    do this with `normaliseToWhole`.

## Measurements (2026-10-29)

**Setup**: RTX 2000 Ada laptop GPU (8 GB) and 33 GB RAM. One cellpose `cpsam_v2` tile per process,
read-only from the stores, with only the model's channels read. Peaks are measured after a warm-up,
so the model itself is not counted. Script: `scratchpad/tileprobe/probe.py` (not committed).

**2D, `f8gzA2`** (Hoechst, 8 µm diameter at 0.16 µm/px, so cellpose downscales ×0.6):

| Tile | s | s/Mpx | RAM Δ GB | VRAM alloc GB | VRAM reserved GB |
|---|---|---|---|---|---|
| 512 | 0.36 | 1.4 | 0.13 | 0.48 | 2.37 |
| 1024 | 1.03 | 1.0 | 0.12 | 0.96 | 2.88 |
| 2048 | 5.41 | 1.3 | 0.36 | 0.96 | 3.04 |
| 3072 | 14.7 | 1.6 | 0.76 | 1.65 | 4.33 |
| 4096 | 24.1 | 1.4 | 1.24 | 2.83 | 5.98 |
| 6144 | 63.0 | 1.7 | 2.63 | 5.67 | 7.48 of 8 |

**3D with Z-stitch, `FtGoJO`** (Z = 26, 2 channels, 10 µm at 0.35 µm/px, no downscale):

| Tile | s | RAM Δ GB | VRAM alloc GB |
|---|---|---|---|
| 512 | 17.7 | 0.40 | 0.96 |
| 1024 | 85.0 | 1.32 | 0.96 |
| 2024 (whole frame) | **1226** | **16.3** | 0.96 |

What this shows:
- **Speed per pixel is flat from 1024 up.** A bigger tile is not faster; it only has fewer seams. So
  "largest that fits" is about seams, and it needs a real safety margin: 6144 reserved 7.5 of 8 GB.
- **2D: VRAM is the limit**, at about 0.15 GB per Mpx above a floor of about 1 GB. The floor is
  cellpose's own 256 px batches. RAM is about 0.07 GB per Mpx.
- **3D with Z-stitch: RAM is the limit, and it is superlinear.** VRAM stays at the floor because
  cellpose takes one plane at a time. From 1024 to 2024 px (4× the area), RAM went 1.3 → 16.3 GB
  (12×) and time 85 → 1226 s (14×).
  - **Cause: cellpose's `stitch3D`.** Per plane pair it builds a **dense** overlap matrix
    (`metrics._label_overlap(...).toarray()`) of this plane's labels × the **cumulative** max id over
    every plane stitched so far. So it grows with (labels per plane) × (labels in the stack), which is
    quadratic in tile area. At 2024 px: about 2.6k × 69k int64 per step, plus float copies.
  - **Consequence**: for 3D-stitched cellpose, smaller tiles are faster and lighter today. No
    "largest that fits" model makes sense until this is replaced (Fix 3).
- The cost scales with the area **after cellpose rescales to diameter 30**, so `cellDiameter` and
  pixel size enter the model (patchworks' `(30/d)²`). Still to measure: the same image at another
  diameter, and coastal.

**The bigger problem is not the tile.** `predict_from_zarr` reads the **whole frame with every
channel** (`zarr_utils.read_timepoint`, `segmentation_utils.py:431`) and tiles it in RAM.
`_context_channels` does not help: it narrows only coastal's temporal-window read, not this frame
read, so every engine reads every channel. On `f8gzA2` that is
25 × 16898 × 20329 × uint16 = **17 GB held for the whole run**, whatever the tile size, against the
roughly 1 GB a 4096 tile costs. That is how a 33 GB machine runs out on a tile scan with a "small"
tile.
- **Fix 1 — BUILT**: narrow the frame read to the union of channels the model groups use (cell + nucleus
  over all groups), for every engine, and remap the channel indices `predict_slice` receives. This
  is the same mapping problem `_context_channels` documents. On `f8gzA2` that is 17 GB → 0.7 GB.
  Measured end to end: the whole 340-tile cellpose run peaked at **6.0 GB** RSS (459 s).
- **Fix 2**: on frames bigger than the budget, read per tile (chunk-aligned, since chunks are 1024)
  instead of per frame.
- **Fix 3 — BUILT**: replace cellpose's `stitch3D` with a sparse per-plane-pair IoU over only the labels
  present in the two planes. This is what coastal's `_match_3d` already does for its per-Z output.
  Run cellpose per plane (as the `stitchThreshold = 0` path does) and stitch ourselves. Keep
  `split_z_gaps`. Results on `FtGoJO`:
  - at 1024 px the labels are byte-identical to the old path;
  - the whole 2024 px frame went 1242 s / 21.3 GB → **265 s / 5.4 GB**;
  - at 2024 px, the old path also merged 24 unrelated cells across the tile (an id wrap, likely
    16-bit) and kept 29 fragments under 15 voxels. The new path does neither.
  See `docs/SEGMENTATION.md` → *Tiling*.
- The per-frame label buffer stays: 16898 × 20329 × uint32 = 1.4 GB per label type.

## P1 — Audit of the image-tiling params (start here)

Today's **Image tiling** section on `segment.cellpose` and `segment.coastal` holds three params:
`blockSize` and `overlap` (fragment `app/src/tasks/fragments/imageTiling.json`) and `labelOverlap`.
`normaliseToWhole`, in **Segmentation options**, only exists because of tiling, so it is audited here
too. `segment.measureLabels` has its own block size (`measure_utils.block_size_for`, chunk-aligned) and
is out of scope.

**D1 — `blockSize` "Tile size (px)": default becomes Auto.**
- Auto means the whole frame when it fits the budget (no seams at all). Otherwise it is the largest
  tile that fits.
- Resolved in the Python runner at run start, so it reads the machine the run is actually on (an
  HPC node included). Logged, and banked with the run: *Auto shows what was picked*.
- A typed number stays as an override.
- The cost model is in P2.
- *Evidence*: 88/99 runs used 4096 to mean "off".

**D2 — `overlap` "Tile overlap (px)": default becomes Auto, from object size.** (Q2 answered: coastal uses `seedSize`.)
- Overlap does two jobs: model context at the tile edge, and the seam band
  (`_collect_seam_strips` uses `h = overlap`). **At 0 the seam stitch silently does nothing.**
- Auto = about 1.5 × cell size, converted to px from the physical pixel size, so it means the same on
  every image of a set.
  - For cellpose, the size is `cellDiameter`.
  - For coastal, the size is `seedSize` (decided 2026-10-29).
- Overlap is cheap once tiles are large: at a 4096 tile, a 64 px overlap is about 6% extra pixels.

**D3 — `labelOverlap` "Seam stitch threshold": keep the stitch, change the test, repair stale 0s.**
- Replace greedy IoU ≥ 0.25 with **mutual best match + overlap-of-smaller ≥ 0.5** (idea 5), and log
  the agreement distribution as a QC number.
- Measure both rules on the setup from `9b3946f5`. There, cells split by a seam went 13 → 2 at IoU
  0.25 and 5 at IoU 0.5. Adopt the new rule only if it does at least as well with no added merges.
- **Legacy runs are left alone** (decided 2026-10-29). No migration of saved `0`s; the param stays.

**D4 — `normaliseToWhole` "Global normalisation": reframe.**
- `false` takes the percentiles **per tile** (`cellpose_utils._prepare_channel` with no
  `norm_params`). On a tiled run that makes every tile a different brightness: a seam artefact
  generator.
- In the 4 runs that used it, the image was one tile, so it meant "per frame".
- Proposal: **Normalise over: whole image / each frame**. Per tile is never offered. Per frame is
  computed over the whole frame, not the tile.
- The tip ("lowest-resolution level") is also stale, since the streaming path computes it at full
  resolution.
- Check that coastal follows the same path before changing it.

**Touchpoints (P1 + P2):**
- `imageTiling.json`; `cellpose.json`, `coastal.json` (param defs, `labelOverlap`, `normaliseToWhole`)
- `cellpose.jl`, `coastal.jl` (`blockSize::Int` parse: Auto needs a representation)
- `segmentation_utils.py` (one resolver, used by both the run and the preview)
- `cellpose_utils.py`, `coastal_utils.py` (cost per pixel)
- the preview path: its seam warning reads `blockSize` (`test_preview_tiling.py`)
- `docs/SEGMENTATION.md` → *Tiling*; `docs/UI.md` copy
- one ccid.json param migration, if Q3 says so

## P2 — Auto tile size, built on measurement

1. **Measure first.** Run cellpose and coastal on real images from the dev projects at 3–4 tile sizes,
   headless (*Run tasks in the REPL*), recording peak RSS and peak VRAM. Fit bytes per XY pixel per
   engine as a function of Z, channels, temporal window length and dtype. Do not build on a guessed
   factor: patchworks' 20× is exactly that.
   - **Cellpose**: the network's VRAM is bounded by its own 256 px tiling (`batch_size=8`), so what
     scales with our tile is the flows, the cell probability and the mask-dynamics step over the
     whole tile × Z.
   - **Coastal**: the window frames × C × Z × tile², plus the per-plane flow caches × `FLOW_WORKERS`.
2. **Budget.**
   - Host budget = min(SLURM allocation, cgroup headroom, available RAM). This is patchworks' rule;
     the cgroup and SLURM part matters for HPC dispatch. Build on
     `cpu_utils.available_memory_bytes()` (it reads `MemAvailable`) rather than a second reader.
   - GPU budget = `gpu_utils.free_vram_gb()` × 0.8.
   - Each is divided by the pool limit of the pool the task runs in, since that many can share it.
3. **Each engine declares its own cost**, e.g. `bytes_per_px(model_params, frame_shape)` on the
   segmenter subclass. `SegmentationUtils` owns the one resolver.
4. **On OOM: stop and reset** (decided 2026-10-29). Catch the CUDA or host OOM, free the model and
   the caches, and stop the run with one actionable line: "Auto picked N px; ran out of GPU memory;
   set Tile size to N/2". No in-run tile splitting.

## P3 — Lossless empty-tile skip (tile scans)

Patchworks' Otsu skip is lossy. We have a lossless criterion already: with `normaliseToWhole`, a tile
whose max ≤ the global low percentile (`norm_min`) after thresholding is **all zeros** to the model.
Skipping it changes nothing.
- Only worth it on tile scans; check the fraction that would be skipped on `4rNbMp` first.
- Coastal needs its own check, because the temporal window can carry signal in from neighbouring
  frames.

## P4 — Seam QC finding

A `seg.*` finding that compares the count of objects ending exactly on a seam with control lines
halfway through each tile (idea 6), and flags above 2×. This turns "stitching was off" (the tile-scan
case) into something a user sees.

## Parked — recorded, not planned

- **Review queue** (idea 7). Its scoring (orphaned / partial parent, seam contact, robust size
  outlier) belongs in the correction cockpit worklist, not here.
- **Relation columns** (idea 8). An `overlap` fraction and nearest-parent distance beside our nuc↔base
  match (`matchThreshold`). Only if someone asks for them.
- **Mode-downsampled label pyramid** (idea 9). Nearest drops small objects at low zoom. Cosmetic in
  the viewer, and measurement reads level 0. Revisit only if it shows.
- **Not taken**:
  - staging tile labels to disk (we hold seam strips in RAM per frame and never recompute the model);
  - Dask/Snakemake orchestration;
  - their plugin contract;
  - dry-run ETA (the preview covers it).

## Decisions (2026-10-29)

1. **OOM**: stop and reset (P2 step 4), no tile splitting.
2. **Coastal overlap**: from `seedSize`.
3. **Legacy runs**: left alone. No migration of saved params, and no re-run of `f8gzA2` from this
   plan.
