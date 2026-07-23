# Zarr/Dask processing rework — plan

Status: planning (no branch yet). Scope guardrail (Dominik): **don't overengineer — only changes
with an actual measured benefit.** This doc deliberately marks which candidate changes pay off and
which are churn to skip.

## Goal

Keep the speed of "load to RAM" without the whole-image RAM cost that OOMs on large time-lapses.

Background (the *why*, from Dominik): `fortify()` (load the whole level into a numpy array) spread
through the image tasks because **dask was catastrophically slow** for tiled/segmentation access —
its auto-chunks span the whole timecourse, so reading one XY tile over-fetches a ~128 MB chunk.
Loading everything to RAM sidestepped the repeated over-reads. It was a speed workaround, not a
laziness abuse. The memory fix already landed (drift/AF/cellpose/segmentation now stream per
timepoint/channel — see `INVENTORY.md` → streaming writers), but it must not reintroduce that
slowness.

## Locked decisions

1. **Granularity = one timepoint (frame), in RAM.** The unit we hold is a single frame, not the
   movie and not a single tile. Bounded memory (one frame) *and* fast in-RAM tiling. This is the
   reconciliation of "RAM was faster" with "don't hold the whole movie".
2. **Reads go through plain `zarr.Array` slicing, not dask, on the hot path.** `z[slice]` already
   reads only the chunks it touches; dask added the over-read footgun and the `da.store` chunk-race
   (regression-tested in `test_zarr_store`) with no compute benefit here. `as_dask=True` becomes the
   exception, kept only where a real parallel graph earns it (today: effectively nowhere).
3. **Parallelism stays in the Julia resource-pool layer** (across images/tasks), NOT a second pool
   inside Python. For a single big image the lever is GPU **batching**, not CPU threads (which would
   oversubscribe against the pools). See Phase 2.
4. **No grand `map_over_zarr` abstraction.** The task bodies differ too much (drift's cumulative
   canvas, AF's per-channel global percentile, cellpose's subset-overwrite, segmentation's
   tile+stitch) to share one iterator without contortion. Shared code stays at the *write* side
   (already done) + a thin frame/tile iterator (Phase 3, only if it falls out cleanly).

## Phases (independently shippable; stop after any)

### Phase 1 — read-frame-once for tiled tasks  ← the actual win  ✅ DONE (segmentation)
**Benefit:** removes N_tiles per-frame disk reads → 1; tiling becomes in-RAM numpy slicing (kills the
original dask-tile slowness); memory unchanged (the frame is already resident for labels).
- **Done:** `segmentation_utils.predict_from_zarr` reads one timepoint via the reusable
  `zarr_utils.read_timepoint(level, dim_utils, t)` (time axis squeezed) and `_extract_tile` slices
  that in-RAM frame. Byte-identical — `test_segmentation_streaming` golden (captured from the
  whole-stack original) still matches. NB: the input frame keeps the channel axis, so its Y/X
  indices differ from the (channel-less) label frame's — see `ifa_y/ifa_x` vs `fa_y/fa_x`.
- **`cellpose_correct`: evaluated, skipped.** It reads `im_dat[0][sl]` one (c,z,t) PLANE at a time,
  not a tile — with the default `as_dask=False` that reads only the plane's chunks, so there is no
  per-tile over-read to remove. Read-frame-once would add slice-reindexing complexity for ~no gain.
  Revisit only if a store's chunking makes per-plane reads over-fetch in practice.

### Phase 2 — cellpose eval batching  ← targeted GPU throughput, measure first
**Benefit:** `dn.eval([tile])` is called one plane at a time. Cellpose accepts a *list*; batching
multiple planes/tiles per call improves GPU utilization. Low complexity, isolated to the eval loop.
- Batch the per-plane list in `cellpose_correct` and the tile list in `segment` predict per frame.
- **Measure before/after** on a real GPU run; keep only if the speedup is real (batch size is a
  tuning knob — too large re-introduces a memory spike, defeating Phase 1). Skip if marginal.

### Phase 3 — cheap cleanups (only while already in the file)
Each is small; do opportunistically, not as a dedicated sweep.
- **Centralize byte-order** (`newbyteorder('=')`, the napari big-endian fix) into the writer so tasks
  stop copy-pasting it. Real dedup, ~zero risk.
- **Two tilers → one:** `slice_utils.create_slices` vs segmentation's `_create_xy_tiles` overlap.
  Consolidate **only if** it merges without contortion (they return different shapes today —
  dim-ordered slice tuples vs read/write/crop triples). Evaluate; don't force it.
- **Retire `da.store(lock=False)`+rechunk** in `create_multiscales` once crop/rescale (its last dask
  callers) stream too. Low benefit (it works now) — do only if it removes real complexity.

## Explicitly NOT doing (and why)

- **Full dask-native `map_blocks`/`map_overlap`.** Fights drift's cumulative shift, cellpose's
  per-block model cost, and segmentation's cross-tile label stitching. High effort, negative fit.
- **A blanket `as_dask=True → False` sweep.** Churn without functional gain; flip per-file only when
  Phase 1 touches it.
- **Intra-task thread/process pools.** The Julia scheduler already parallelizes across images
  (pools: cpu=20, gpu=1). A second Python layer risks oversubscription. Revisit ONLY if a single
  image is measured as the wall-clock bottleneck after Phases 1–2.

## References
- Streaming writers (done): `python/cecelia/utils/zarr_utils.py` — `open_multiscales_for_writing`,
  `write_multiscale_pyramid`, `copy_stream`; `INVENTORY.md`.
- Chunking policy: `zarr_utils.plane_chunks` (the ~128 MB auto-chunk / per-plane rationale).
- `da.store` chunk-race regression: `python/cecelia/tests/test_zarr_store.py`.
