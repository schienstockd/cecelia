# Cache Farneback flow output as its own task, decoupled from segmentation reruns

> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

## Context

#617 measured Farneback as ~86% of flow-prep time, with metric
materialization at ~14% and cumulative displacement at ~0.2%. Right now
segmentation reruns pay the full 86% every time a threshold or param gets
tweaked, even though the underlying flow field hasn't changed.

## Goal

Split "compute flow" from "prepare metrics for training/segmentation"
into two tasks, with the first cached to a sidecar.

## 1. New task: `compute_flow`

`compute_flow(movie, temporalScales, cropSize|full_frame, zSpacing) -> sidecar path`

- Runs Farneback only, produces raw u/v flow fields per scale (not the
  16 materialized metrics).
- Cache key: hash of (movie identity/checksum, temporalScales, crop
  region or full-frame flag, zSpacing). Anything that changes the
  Farneback inputs invalidates the cache; metric choice and loss
  weights must **not** be part of the key.
- Store as float32 (evaluate float16 later, don't default to it), u/v
  channels only — not per-metric — compressed with blosc/zstd. Confirm
  compression ratio on a real movie before committing to a format.
- Reuse the existing thread/joblib budget wiring from #617
  (`LOKY_MAX_CPU_COUNT` / `task_worker_threads()`), same as the flow
  stage does now.

## 2. Update `prepare_data_for_unet` (or segmentation's flow-metrics step)

Change to first check for a valid sidecar via the cache key, and if
present, skip Farneback and materialize the 16 metrics straight from
cached u/v. If absent, call `compute_flow` first, write the sidecar,
then proceed as today.

## 3. Segmentation rerun path

Confirm it goes through the same cache lookup, so changing a
segmentation threshold/param that doesn't touch
temporalScales/crop/zSpacing never re-triggers Farneback.

## 4. Invalidation

No need for TTL, just key correctness. Add a manual "clear flow cache
for this movie" affordance (CLI or button) for cases where a movie file
itself is replaced but the checksum check is skipped or wrong.

## 5. Tests

- Cache hit/miss on key changes (temporalScales, crop, zSpacing each
  independently)
- Cache hit persists across metric-set and loss-weight changes
- Segmentation rerun with only a threshold change skips Farneback
- Compression ratio / round-trip integrity on a real movie
- Manual cache-clear affordance works end to end
