// Play-health summary for the viewer's debug panel: how close is playback to smooth?
//
// Three questions decided this shape:
//   1. Is the shader drawing complete frames, or filling holes with previous-t? (`missingAtDisplay`)
//   2. Is `displayT` keeping up with what the user asked to play, or lagging? (`boundTLag`)
//   3. Is playback advancing at the requested fps, or stalling? (`achievedFps`)
//
// The renderer already exposes these instantaneously via `brickResidency()`; this module records a
// rolling window of samples during play and reduces to averages / p95 / a derived fps. Pure over the
// samples so the summary can be unit-tested without a WebGPU device — same discipline as
// `volumeCache.ts` / `pageTable.ts`.

export interface PlayHealthSample {
  /** Wall clock in ms (`performance.now()`). Advancing between samples is what defines achievedFps. */
  timeMs: number
  /** `brickResidency().displayT` — the timepoint the shader drew from on the tick this sample was
   *  taken. A stalled tick keeps this equal to the previous sample. */
  displayT: number
  /** `brickResidency().boundT` — the timepoint playback ASKED for. `boundT - displayT` is the lag. */
  boundT: number
  /** `brickResidency().missing` — core bricks the shader wanted at `displayT` but had to hole-fill
   *  from `prevDisplayT`. Non-zero = the "flicker between blocks" is on screen. */
  missingAtDisplay: number
}

export interface PlayHealthSummary {
  /** How many samples went into the summary. Zero when the window is empty. */
  count: number
  /** Mean `missingAtDisplay` across the window. Zero = smooth; higher = more hole-fill visible. */
  avgMissingAtDisplay: number
  /** 95th-percentile `missingAtDisplay` — catches the worst frames the average smooths over. */
  p95MissingAtDisplay: number
  /** 95th-percentile of `boundT - displayT`. A big value means the scheduler is chasing frames
   *  that the shader hasn't reached yet — playback is stalled and displaying a stale t. */
  p95BoundTLag: number
  /** Frames per second the shader ACTUALLY advanced through, derived from unique `displayT`
   *  transitions across the sample window. `NaN` when the window spans zero time. */
  achievedFps: number
}

/** Reduce a rolling window of samples to a summary. Pure; caller manages the ring buffer. */
export function playHealthSummary(samples: readonly PlayHealthSample[]): PlayHealthSummary {
  const n = samples.length
  if (n === 0) {
    return {
      count: 0, avgMissingAtDisplay: 0, p95MissingAtDisplay: 0, p95BoundTLag: 0, achievedFps: NaN,
    }
  }
  let sumMissing = 0
  const missingSorted: number[] = []
  const lagSorted: number[] = []
  let displayAdvances = 0
  let prevDisplay = samples[0].displayT
  for (const s of samples) {
    sumMissing += s.missingAtDisplay
    missingSorted.push(s.missingAtDisplay)
    lagSorted.push(Math.max(0, s.boundT - s.displayT))
    if (s.displayT !== prevDisplay) {
      displayAdvances++
      prevDisplay = s.displayT
    }
  }
  missingSorted.sort((a, b) => a - b)
  lagSorted.sort((a, b) => a - b)
  const spanMs = samples[n - 1].timeMs - samples[0].timeMs
  return {
    count: n,
    avgMissingAtDisplay: sumMissing / n,
    p95MissingAtDisplay: percentile(missingSorted, 0.95),
    p95BoundTLag: percentile(lagSorted, 0.95),
    achievedFps: spanMs > 0 ? (displayAdvances * 1000) / spanMs : NaN,
  }
}

/** Trim a sample buffer to the last `windowMs` of samples (by `timeMs`), plus an absolute cap. The
 *  cap is a memory guard — a stuck play loop should not accumulate an unbounded array. Returns a
 *  new array; caller replaces its ring in one assignment. */
export function trimSamples(
  samples: readonly PlayHealthSample[], nowMs: number, windowMs: number, maxSamples: number,
): PlayHealthSample[] {
  const cutoff = nowMs - windowMs
  const out: PlayHealthSample[] = []
  for (const s of samples) if (s.timeMs >= cutoff) out.push(s)
  if (out.length > maxSamples) return out.slice(out.length - maxSamples)
  return out
}

/** Nearest-rank percentile on a sorted array of non-negative numbers. Not fancy; matches the p95
 *  used in `benchRecorder.ts` for consistency across the debug panel. */
function percentile(sortedAsc: readonly number[], q: number): number {
  const n = sortedAsc.length
  if (n === 0) return 0
  const idx = Math.min(n - 1, Math.max(0, Math.ceil(q * n) - 1))
  return sortedAsc[idx]
}
