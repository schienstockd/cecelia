// ── Bench harness — flat vs brick 3D renderer comparison ────────────────────────
//
// Debug-only, gated on `?bench=1` in ViewerWindow. Records: (1) time from setImage to first
// draw, (2) CPU-side r.draw() cost across every subsequent frame, (3) frame count in the
// session, (4) bytes transferred over the wire (via PerformanceObserver on 'resource' entries),
// (5) a VRAM snapshot at save time.
//
// The user drives the workload — scrub the time strip, spin the camera, zoom. That's more
// honest than a synthetic loop: it measures the paths the actual UI takes.
//
// The pure parts (summary math, JSON shape) live here so they can be tested without a browser.

/** Descriptor for the image the session is measuring. Nulls when meta hasn't loaded yet. */
export interface BenchMeta {
  imageUid: string
  valueName: string
  nT: number
  nC: number
  nZ: number
  nY: number
  nX: number
  nLevels: number
  bytesPerVoxel: number
}

/** One draw sample — CPU-side cost of a single r.draw() submission. GPU-side time isn't in
 *  here (would need a GPU timestamp query); this catches CPU overhead differences between the
 *  two renderers, which is where per-sample page-table indirection would show up. */
export interface BenchSample {
  atMs: number       // performance.now() at frame end
  drawMs: number     // r.draw() submission duration in ms
}

/** VRAM footprint at snapshot time — raw fields the renderer already exposes, captured so
 *  the saved JSON is self-contained. Bytes are derivable: flat = capacity × bytesPerTimepoint,
 *  brick = residentBricks × product(brickSizeVox) × bytesPerVoxel × (channelsPerBrick + 1 if
 *  labels). Kept as raw fields rather than derived so a downstream analysis can spot a wrong
 *  formula rather than blindly trusting the number here. */
export interface BenchVram {
  cacheCapacity: number
  cacheBytesPerTimepoint: number
  cacheZDepth: number
  residentTimepoints: number
  // Brick-only fields. -1/[] when the flat renderer is running.
  brickCurrentLevel: number
  residentBricks: number
  brickSizeVox: readonly [number, number, number]
}

/** The blob saved to disk. Absolute times are `performance.now()` relative — the session
 *  origin (`t0`) is 0. Wall-clock stamp is separate so two sessions can be diffed by date. */
/** Per-writeBrick timing sample — brick renderer only. Populated via `setOnBrickWritten` on
 *  the renderer. Times the atlas-upload path (CPU-side duration of one writeBrick). */
export interface BenchWriteSample {
  atMs: number            // performance.now() at the write, relative to session t0 in the blob
  durationMs: number      // CPU-side duration of the writeBrick call
  bytes: number           // payload byte count for this brick
}

/** GPU + fine-grained CPU timings for one frame. `gpuFrameMs` is null on adapters that lack the
 *  `timestamp-query` feature — the CPU-side buckets always populate. Emitted async (frame N+K) via
 *  the renderer's `setOnFrameTimings` hook, so these are NOT correlated 1:1 with `BenchSample`
 *  entries; the blob stores them as a parallel stream and the summariser reports p50/p95 across
 *  the session. Split rationale: `octree-rendering-audit.md` §2 flagged whole-`drawMs` as too
 *  coarse to decide any future perf move on. */
export interface GpuFrameSample {
  atMs: number                       // performance.now() at the CPU side of the frame, relative to t0
  gpuFrameMs: number | null          // GPU-side end-to-end render pass (raycast + overlays)
  tickSchedulerCpuMs: number         // CPU-side scheduler tick (fetch decisions, residency)
  writePageTableCpuMs: number        // CPU-side page-table upload (both current + prev tables)
  writeUniformCpuMs: number          // CPU-side uniform buffer upload
  encoderSubmitCpuMs: number         // CPU-side `enc.finish()` + `queue.submit()`
}

export interface BenchBlob {
  version: 1 | 2
  mode: 'flat' | 'brick'
  savedAtIso: string
  sessionMs: number         // performance.now() at save, minus t0
  meta: BenchMeta
  firstFrameMs: number | null
  frames: BenchSample[]
  bytesFetched: number
  vram: BenchVram | null
  writes: BenchWriteSample[]   // empty on flat mode
  /** v2 only — parallel stream of GPU + CPU sub-frame timings. Empty on v1 blobs and on renderers
   *  that don't populate it. Not correlated with `frames[i]`. */
  gpuFrames: GpuFrameSample[]
  summary: BenchSummary
}

/** Rolled-up numbers so the JSON is readable without post-processing. */
export interface BenchSummary {
  nFrames: number
  drawMedianMs: number | null
  drawP95Ms: number | null
  drawMeanMs: number | null
  framesPerSecond: number | null
  /** v2 only — p50/p95 per GPU/CPU sub-frame bucket. Null when `gpuFrames` is empty. */
  gpuSummary: GpuBucketSummary | null
}

/** Per-bucket p50/p95 across the session. `gpuFrameMs50/95` are null on adapters without
 *  `timestamp-query`; the CPU-side buckets are populated whenever `gpuFrames` has any samples. */
export interface GpuBucketSummary {
  nGpuFrames: number
  gpuFrameMs50: number | null
  gpuFrameMs95: number | null
  tickSchedulerCpuMs50: number
  tickSchedulerCpuMs95: number
  writePageTableCpuMs50: number
  writePageTableCpuMs95: number
  writeUniformCpuMs50: number
  writeUniformCpuMs95: number
  encoderSubmitCpuMs50: number
  encoderSubmitCpuMs95: number
}

/** Median of a numeric array. Returns null for an empty input (a session with zero frames is
 *  a bench that never ran, not one where the answer is 0). */
export function median(xs: readonly number[]): number | null {
  if (xs.length === 0) return null
  const s = [...xs].sort((a, b) => a - b)
  const mid = s.length >> 1
  return s.length % 2 ? s[mid] : (s[mid - 1] + s[mid]) / 2
}

/** 95th percentile — nearest-rank. Same null-on-empty policy as `median`. */
export function percentile95(xs: readonly number[]): number | null {
  if (xs.length === 0) return null
  const s = [...xs].sort((a, b) => a - b)
  const rank = Math.ceil(0.95 * s.length) - 1
  return s[Math.max(0, Math.min(s.length - 1, rank))]
}

/** GPU-side samples that arrived — always a subset (or superset) of the CPU-side per-frame
 *  count because delivery is async. `null` return when no GPU sample has landed yet. */
function summarizeGpu(gpuFrames: readonly GpuFrameSample[]): GpuBucketSummary | null {
  if (gpuFrames.length === 0) return null
  const gpu = gpuFrames.map(f => f.gpuFrameMs).filter((x): x is number => x !== null)
  const tick = gpuFrames.map(f => f.tickSchedulerCpuMs)
  const wpt = gpuFrames.map(f => f.writePageTableCpuMs)
  const wu = gpuFrames.map(f => f.writeUniformCpuMs)
  const es = gpuFrames.map(f => f.encoderSubmitCpuMs)
  const p50CpuBucket = (xs: number[]): number => median(xs) ?? 0
  const p95CpuBucket = (xs: number[]): number => percentile95(xs) ?? 0
  return {
    nGpuFrames: gpuFrames.length,
    gpuFrameMs50: median(gpu),
    gpuFrameMs95: percentile95(gpu),
    tickSchedulerCpuMs50: p50CpuBucket(tick),
    tickSchedulerCpuMs95: p95CpuBucket(tick),
    writePageTableCpuMs50: p50CpuBucket(wpt),
    writePageTableCpuMs95: p95CpuBucket(wpt),
    writeUniformCpuMs50: p50CpuBucket(wu),
    writeUniformCpuMs95: p95CpuBucket(wu),
    encoderSubmitCpuMs50: p50CpuBucket(es),
    encoderSubmitCpuMs95: p95CpuBucket(es),
  }
}

export function summarize(
  frames: readonly BenchSample[],
  sessionMs: number,
  gpuFrames: readonly GpuFrameSample[] = [],
): BenchSummary {
  const draws = frames.map(f => f.drawMs)
  const mean = draws.length === 0 ? null : draws.reduce((a, b) => a + b, 0) / draws.length
  // Frames per second: only meaningful when the session actually ran long enough that a
  // sub-second bench doesn't overstate the rate wildly. Reported anyway; a caller reading
  // this can weight it against `sessionMs`.
  const fps = sessionMs > 0 && frames.length > 0
    ? (frames.length / (sessionMs / 1000)) : null
  return {
    nFrames: frames.length,
    drawMedianMs: median(draws),
    drawP95Ms: percentile95(draws),
    drawMeanMs: mean,
    framesPerSecond: fps,
    gpuSummary: summarizeGpu(gpuFrames),
  }
}

export function buildBlob(input: {
  mode: 'flat' | 'brick'
  meta: BenchMeta
  t0: number
  savedAt: number
  firstFrameMs: number | null
  frames: readonly BenchSample[]
  bytesFetched: number
  vram: BenchVram | null
  isoDate: string
  writes?: readonly BenchWriteSample[]
  gpuFrames?: readonly GpuFrameSample[]
}): BenchBlob {
  const sessionMs = input.savedAt - input.t0
  const gpuFrames = (input.gpuFrames ?? []).map(g => ({
    atMs: g.atMs - input.t0,
    gpuFrameMs: g.gpuFrameMs,
    tickSchedulerCpuMs: g.tickSchedulerCpuMs,
    writePageTableCpuMs: g.writePageTableCpuMs,
    writeUniformCpuMs: g.writeUniformCpuMs,
    encoderSubmitCpuMs: g.encoderSubmitCpuMs,
  }))
  return {
    // v2 whenever any GPU/CPU sub-frame samples landed; keeps v1 shape otherwise so an old
    // reader that never looks at `gpuFrames` still parses cleanly.
    version: gpuFrames.length > 0 ? 2 : 1,
    mode: input.mode,
    savedAtIso: input.isoDate,
    sessionMs,
    meta: input.meta,
    firstFrameMs: input.firstFrameMs,
    frames: input.frames.map(f => ({ atMs: f.atMs - input.t0, drawMs: f.drawMs })),
    bytesFetched: input.bytesFetched,
    vram: input.vram,
    // Normalise write timestamps to be relative to t0 too — same convention as `frames`.
    writes: (input.writes ?? []).map(w => ({
      atMs: w.atMs - input.t0, durationMs: w.durationMs, bytes: w.bytes,
    })),
    gpuFrames,
    summary: summarize(input.frames, sessionMs, gpuFrames),
  }
}

/** Filename convention: `bench-{imageUid}-{mode}-{isoDateCompact}.json`. Kept short enough to
 *  paste from a shell but unique across the five images and two modes so a directory of them
 *  doesn't need renaming. */
export function benchFilename(mode: 'flat' | 'brick', imageUid: string, iso: string): string {
  const stamp = iso.replace(/[:.]/g, '-').replace(/T/, '_')
  return `bench-${imageUid}-${mode}-${stamp}.json`
}
