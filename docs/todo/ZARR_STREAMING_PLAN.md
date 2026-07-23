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

### Phase 2 — cellpose eval batching  ← MEASURED, no benefit, SKIP
The idea: raise the cellpose `batch_size` (both `CellposeModel.eval` and `DenoiseModel.eval` accept
it, default 8) so more Z-slices/planes go to the GPU per launch — a zero-restructure knob.

**Measured** on a real GPU (cuda), segmenting a 3-frame time-crop of EaMaVq (proj 4kS67f, 3D,
512-tiled, 4 tiles/frame, cyto2), timing `predict_from_zarr` after model warm-up:

| batch_size | time | cells |
|-----------:|-----:|------:|
| 8 (default) | 34.1s | 95 |
| 16 | 37.5s | 95 |
| 32 | 37.5s | 95 |

Raising it does **not** help — slightly worse at 16/32. The per-tile eval already hands cellpose a
full 512×512×Z stack, so the GPU is saturated at the default and there is no launch overhead to
amortise; larger batches only add memory pressure. Tile-/frame-batching (eval a *list* of stacks)
was **not** pursued: it wouldn't reduce the actual per-slice compute (the real cost here) and it
fights the Phase-1 one-frame-in-RAM streaming. **Conclusion: no code change — GPU eval is the floor.**
(Revisit only if a future model/hardware makes per-launch overhead dominant.)

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
