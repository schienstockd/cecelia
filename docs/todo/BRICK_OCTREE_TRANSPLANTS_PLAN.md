**Status:** planning (2026-08-31) · branch `feat/brick-followup`

## Goal

Two learnings pulled from the octree audit (`docs/archive/octree-rendering-audit.md`) that improve
the flat brick renderer without adopting a hierarchical data structure. The audit's verdict was NO
on octree; this plan captures the parts that do carry over.

- **P1 — timestamp-query bench pass.** Split whole-`draw()` ms into shader / page-table upload /
  submission. Small, self-contained, unblocks every future perf decision on this renderer.
- **P2 — per-brick max-mip sidecar.** Skip fetch + shader ray-step on empty bricks. Conditional on
  P1 showing residual is fetch-driven, or on a felt UX cost of pop-in at wide zoom on f8gzA2.

Not the whole octree toolkit — see *Not in scope*. Origin: `docs/archive/octree-rendering-audit.md`
§Recommendation → §Conditional flip #3, and the "biggest unresolved measurement gap" callout.

## Decisions

**D1 (2026-08-31) — instrument first, optimise later.** P1 is a prerequisite for P2's cost/benefit
call. Building P2 blind is exactly the "brickThr tuned against two screenshots, not a cost model"
mistake the audit's brief was written to avoid.

**D2 (2026-08-31) — timestamp queries are best-effort, not required.** WebGPU's `timestamp-query`
feature is gated on adapter support (Chrome + Firefox nightly, Safari ships later). When absent,
bench still records `drawMs` as today and the extra columns record `null`. `?bench=1` must not
degrade or refuse to open on adapters that lack the feature.

**D3 (2026-08-31) — bench blob schema bumps to `version: 2` when GPU splits land.** The v1 shape
stays valid for reading; the summariser tolerates missing GPU fields. No renaming of `drawMs`.

**D4 (2026-08-31) — the max-mip sidecar is a NEW zarr, not an added array in the image store.**
Same producing-task discipline as the segmentation output stores — written under
`<image>/labels/…` sibling, `zarr_utils.staged_store` + `store_compressor(kind='labels')` because
per-brick max is discrete integer summary data. `open_as_zarr` reads it. No hand-rolled I/O.

**D5 (2026-08-31) — sidecar is optional at render time.** Renderer works with or without it. If
absent for a store, scheduler + shader use today's behaviour. Sidecar existence is a per-store
opt-in, not a required migration.

## Phases

### P1 — GPU timestamp bench (do this first)

Small end-to-end pass:

- `webgpuProbe.ts::acquireGpuDevice` adds `'timestamp-query'` to `requiredFeatures` when the
  adapter has it (probe already reports `hasTimestamps` but the device request never asks for
  it). Best-effort, no throw when absent.
- `brickVolumeRenderer.ts` creates a `GPUQuerySet` (size 2) + resolve buffer + a ring of mappable
  readback buffers when `report.hasTimestamps`. Main draw path (`:1126`) uses ONE render pass
  (raycast + overlays via `encodePass`) — wraps that one pass with `timestampWrites: { querySet,
  beginningOfPassWriteIndex: 0, endOfPassWriteIndex: 1 }`, resolves after `enc.finish()`, maps
  async and delivers timings via a new `setOnFrameTimings` hook. Async delivery means GPU
  timings arrive on frame N+K, uncorrelated with individual CPU frames — the blob stores them
  as a parallel stream.
- Also captured in the same hook, CPU-side wraps around today's `draw()` internals:
  `tickSchedulerCpuMs`, `writePageTableCpuMs`, `writeUniformCpuMs`, `encoderSubmitCpuMs`.
  Together these split the whole-`drawMs` lump that today's harness records.
- `benchRecorder.ts`:
  - New `GpuFrameSample { atMs, gpuFrameMs, tickSchedulerCpuMs, writePageTableCpuMs,
    writeUniformCpuMs, encoderSubmitCpuMs }`. Fields the adapter can't populate (GPU-side ones
    on adapters lacking `timestamp-query`) are `null`.
  - `BenchBlob` gains `gpuFrames: GpuFrameSample[]`, empty on v1-mode. `version` type widens to
    `1 | 2`; blob is v2 when `gpuFrames.length > 0`.
  - `summarize` extends with a `gpuSummary` sub-object (p50/p95 per bucket) when data exists,
    null when it doesn't.
- Bench chip in `ViewerWindow.vue` shows extra rows: `GPU frame — p50/p95`, `CPU tick / pt / submit
  — p50/p95` when data present. When adapter lacks the feature, `GPU frame — n/a`; CPU-side rows
  always populate.
- Tests: `benchRecorder.test.ts` covers the two-shape summariser (v1 blob without GPU fields, v2
  blob with) and null-tolerance. No renderer mounting.

Exit criteria: run a bench session on all three typicals (fXgbTl / Dml3RG / f8gzA2), open the
saved v2 blob, read the split. That's the whole win — we can now see what an optimisation would
actually move.

### P2 — Per-brick max-mip sidecar (conditional on P1)

Only start when P1 shows one of:

- residual `drawMs` on f8gzA2 is dominated by `pageTableUploadMs` scaling with intersect list, or
- fetch bandwidth (`bytesFetched`) is a felt UX cost on wide zoom-out on f8gzA2 (pop-in visible).

Then:

- **Producer** (Julia task, `app/src/…`): reads image via `zarr_utils` accessors, walks each
  channel × level × brick, writes `max_u16` per brick to a sidecar zarr under
  `<image>/.brick_max/{value_name}.zarr` (naming aligns with `gating/{value_name}.json` convention).
  Threshold policy stays a render-time input (matches `brickShader.ts` current `threshold` shape).
- **Scheduler** (`brickScheduler.ts`): before enqueueing a brick fetch, checks the sidecar for
  `max < threshold` and skips. Halo bricks with low max are pinned as "known-empty" — no fetch,
  no atlas slot spent, shader falls through to prev-level or background as today.
- **Shader** (`brickShader.ts`): a per-step "is this brick known-empty" lookup gated on the same
  sidecar. Skips one brick's worth of ray when true. No hierarchy — the sidecar is one flat u16
  per-brick per-level, sampled via a new page-table-shaped indirection.
- Tests: `brickScheduler.test.ts` cases for (a) sidecar absent → today's behaviour, (b) sidecar
  present + low-max brick → not enqueued, (c) threshold above ceiling → all skipped.

P2 is deliberately shaped to be a no-op on 4/5 sampled stores (intravital + lymphoid: brick-empty
= 0 %) and a real fetch cut on f8gzA2 (brick-empty 12–21 %). That's the audit's shape, matched.

## Not in scope

Explicit non-goals — the parts of octree work the audit rejected:

- **Hierarchical page table.** `pageTableCpu` stays flat. No parent/child pointers.
- **Trilinear LOD blend / mixed-LOD-per-frame sampling.** No per-brick level in the shader; one
  level per frame stays the rule.
- **GPU-side intersect-list compute prepass.** `MAX_INTERSECT_BRICKS = 256` in
  `brickScheduler.ts` already handles the f8gzA2 wide-zoom case at CPU cost < 1 ms.
- **Importance-weighted LRU.** Halo + prev-level touch bias already carry the load.
- Retuning `brickThr` / brick size / `MAX_INTERSECT_BRICKS` — separate concerns tracked in
  `BRICK_INTEGRATION_PLAN.md` §Open questions.

## References

- Origin audit: [`docs/archive/octree-rendering-audit.md`](../archive/octree-rendering-audit.md) —
  §Recommendation, §Conditional flip #3, "biggest unresolved measurement gap"
- Renderer plans: [`KILN_BRICK_PLAN.md`](KILN_BRICK_PLAN.md),
  [`BRICK_INTEGRATION_PLAN.md`](BRICK_INTEGRATION_PLAN.md) (§Decision 2 / B4 — the intersect guard
  this plan sits on top of)
- Code touched: `frontend/src/lib/webgpu/brickVolumeRenderer.ts` (device features, query set,
  render pass boundaries at `:1140`, `:1313`); `frontend/src/lib/webgpu/brickShader.ts` (P2 only);
  `frontend/src/utils/benchRecorder.ts` (schema v2); `frontend/src/utils/brickScheduler.ts` (P2
  only); `frontend/src/modules/ViewerWindow.vue` (bench chip row)
- Zarr I/O: `python/cecelia/utils/zarr_utils.py` — `staged_store`, `store_compressor(kind='labels')`,
  `open_as_zarr` (P2 sidecar)
