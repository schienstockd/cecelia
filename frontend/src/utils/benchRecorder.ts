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
export interface BenchBlob {
  version: 1
  mode: 'flat' | 'brick'
  savedAtIso: string
  sessionMs: number         // performance.now() at save, minus t0
  meta: BenchMeta
  firstFrameMs: number | null
  frames: BenchSample[]
  bytesFetched: number
  vram: BenchVram | null
  summary: BenchSummary
}

/** Rolled-up numbers so the JSON is readable without post-processing. */
export interface BenchSummary {
  nFrames: number
  drawMedianMs: number | null
  drawP95Ms: number | null
  drawMeanMs: number | null
  framesPerSecond: number | null
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

export function summarize(frames: readonly BenchSample[], sessionMs: number): BenchSummary {
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
}): BenchBlob {
  const sessionMs = input.savedAt - input.t0
  return {
    version: 1,
    mode: input.mode,
    savedAtIso: input.isoDate,
    sessionMs,
    meta: input.meta,
    firstFrameMs: input.firstFrameMs,
    frames: input.frames.map(f => ({ atMs: f.atMs - input.t0, drawMs: f.drawMs })),
    bytesFetched: input.bytesFetched,
    vram: input.vram,
    summary: summarize(input.frames, sessionMs),
  }
}

/** Filename convention: `bench-{imageUid}-{mode}-{isoDateCompact}.json`. Kept short enough to
 *  paste from a shell but unique across the five images and two modes so a directory of them
 *  doesn't need renaming. */
export function benchFilename(mode: 'flat' | 'brick', imageUid: string, iso: string): string {
  const stamp = iso.replace(/[:.]/g, '-').replace(/T/, '_')
  return `bench-${imageUid}-${mode}-${stamp}.json`
}
