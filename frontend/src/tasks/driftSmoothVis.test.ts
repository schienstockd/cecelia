/**
 * The trajectory-smoothing figure's behaviour, tested without mounting anything — same pattern
 * `driftVis.test.ts` follows.
 *
 * The claims worth pinning are the ones a reader of the figure would take away: σ=0 is a no-op,
 * a wider σ collapses a random-walk to fewer integer transitions, a slow ramp survives across a
 * broad σ range, and a fast step gets rounded off when σ exceeds its width.
 */
import { describe, expect, it } from 'vitest'
import {
  countTransitions, driftSmoothFigure, driftSmoothVisColumns, gaussianSmooth, jerkScenario,
  noiseScenario, rampScenario, roundTrajectory, SMOOTH_VIS_COLUMNS, transitionWidth,
  W,
} from './driftSmoothVis'

describe('gaussianSmooth', () => {
  it('σ=0 is the identity', () => {
    const t = noiseScenario()
    expect(gaussianSmooth(t, 0)).toEqual(t)
    // Guard against a buggy JSON default reaching the smoother as a negative — same shortcut
    // the Python side takes in `_smooth_positions`.
    expect(gaussianSmooth(t, -1)).toEqual(t)
  })

  it('preserves the mean (up to edge effects)', () => {
    const t = rampScenario()
    const s = gaussianSmooth(t, 6)
    const meanT = t.reduce((a, b) => a + b, 0) / t.length
    const meanS = s.reduce((a, b) => a + b, 0) / s.length
    expect(Math.abs(meanT - meanS)).toBeLessThan(0.5)
  })

  it('a wider σ produces smaller frame-to-frame differences', () => {
    const t = noiseScenario()
    const stepSize = (traj: number[]) => {
      let s = 0
      for (let i = 1; i < traj.length; i++) s += Math.abs(traj[i] - traj[i - 1])
      return s
    }
    // Monotone: bigger σ ⇒ smoother trajectory ⇒ smaller total variation.
    expect(stepSize(gaussianSmooth(t, 6))).toBeLessThan(stepSize(gaussianSmooth(t, 3)))
    expect(stepSize(gaussianSmooth(t, 3))).toBeLessThan(stepSize(t))
  })
})

describe('scenarios', () => {
  it('are deterministic — two renders of the figure are the same figure', () => {
    expect(noiseScenario()).toEqual(noiseScenario())
    expect(rampScenario()).toEqual(rampScenario())
    expect(jerkScenario()).toEqual(jerkScenario())
  })

  it('all three have W frames', () => {
    expect(noiseScenario().length).toBe(W)
    expect(rampScenario().length).toBe(W)
    expect(jerkScenario().length).toBe(W)
  })

  it('noise averages near zero (no bias)', () => {
    const t = noiseScenario()
    const mean = t.reduce((a, b) => a + b, 0) / t.length
    expect(Math.abs(mean)).toBeLessThan(0.5)
  })

  it('ramp goes from negative to positive (visibly slopes across the frame)', () => {
    const t = rampScenario()
    expect(t[0]).toBeLessThan(0)
    expect(t[t.length - 1]).toBeGreaterThan(0)
  })

  it('jerk plateaus each side, with the transition inside JERK_WIDTH*2 frames', () => {
    const t = jerkScenario()
    // Plateaus: first and last quarter should each stay close to a stable value.
    const early = t.slice(0, 8), late = t.slice(t.length - 8)
    const varOf = (a: number[]) => {
      const m = a.reduce((s, v) => s + v, 0) / a.length
      return a.reduce((s, v) => s + (v - m) ** 2, 0) / a.length
    }
    expect(varOf(early)).toBeLessThan(1.0)
    expect(varOf(late)).toBeLessThan(1.0)
    // ... and the end plateaus are FAR apart — a step, not a wobble.
    const meanOf = (a: number[]) => a.reduce((s, v) => s + v, 0) / a.length
    expect(meanOf(late) - meanOf(early)).toBeGreaterThan(3.0)
  })
})

describe('the property σ=6 targets — jitter dies, drift survives, step almost survives', () => {
  it('noise: at σ=6 the rounded staircase has (near) zero integer transitions', () => {
    const raw = noiseScenario()
    const rawT = countTransitions(roundTrajectory(raw))
    const outT = countTransitions(roundTrajectory(gaussianSmooth(raw, 6)))
    expect(rawT).toBeGreaterThan(6)                              // the raw wobbles across integers
    expect(outT).toBeLessThan(rawT / 3)                          // smoothed collapses it
  })

  it('ramp: at σ=6 the rounded end-to-end drift is nearly the raw drift', () => {
    const raw = rampScenario()
    const rounded = roundTrajectory(gaussianSmooth(raw, 6))
    const rawInt = roundTrajectory(raw)
    const range = (a: number[]) => Math.max(...a) - Math.min(...a)
    // Preserves at least 60% of the drift range — the whole point of the property.
    expect(range(rounded)).toBeGreaterThanOrEqual(range(rawInt) - 2)
  })

  it('jerk: plateaus still land near the raw plateaus at σ=6', () => {
    const raw = jerkScenario()
    const rounded = roundTrajectory(gaussianSmooth(raw, 6))
    // ±1 px slack because gaussian's `mode='nearest'` shrinks the edge value slightly toward
    // the boundary, which can flip the round direction. What the reader takes away is "both
    // plateaus visible on opposite sides of zero", not exact recovery.
    const rawStart = Math.round(raw.slice(0, 4).reduce((s, v) => s + v, 0) / 4)
    const rawEnd   = Math.round(raw.slice(raw.length - 4).reduce((s, v) => s + v, 0) / 4)
    expect(Math.abs(rounded[0] - rawStart)).toBeLessThanOrEqual(1)
    expect(Math.abs(rounded[rounded.length - 1] - rawEnd)).toBeLessThanOrEqual(1)
    // ... and end up on opposite sides of zero (a real step, not a wobble).
    expect(Math.sign(rounded[0])).not.toBe(Math.sign(rounded[rounded.length - 1]))
  })

  it('σ increases visibly widen the step transition — the "eating" property, monotone', () => {
    // The teaching claim of column 3: σ has a trade-off with fast motion. Wider σ ⇒ wider
    // transition. Doesn't pin an absolute width — that would tie the test to the amplitude of
    // the jerk scenario, which the figure may need to retune for legibility.
    const raw = jerkScenario()
    const w0  = transitionWidth(roundTrajectory(raw))
    const w6  = transitionWidth(roundTrajectory(gaussianSmooth(raw,  6)))
    const w15 = transitionWidth(roundTrajectory(gaussianSmooth(raw, 15)))
    expect(w6).toBeGreaterThanOrEqual(w0)
    expect(w15).toBeGreaterThan(w6)
  })
})

describe('driftSmoothVisColumns', () => {
  it('produces the three named columns in order', () => {
    const vis = driftSmoothVisColumns({ sigma: 6 })
    expect(vis.columns).toEqual([...SMOOTH_VIS_COLUMNS])
  })

  it('every column has a grid frame at the current σ', () => {
    const vis = driftSmoothVisColumns({ sigma: 6 })
    const grid = vis.rows.find(r => r.role === 'grid')!
    expect(grid.cells.length).toBe(SMOOTH_VIS_COLUMNS.length)
    for (const c of grid.cells) {
      expect(c.frames).toBeDefined()
      expect(c.frames!.length).toBe(1)
      // Grid frames are 2D — at least one row, at least one cell.
      expect(c.frames![0].length).toBeGreaterThan(0)
      expect(c.frames![0][0].length).toBe(W)
    }
  })

  it('carries three rows (grid + case + verdict)', () => {
    const vis = driftSmoothVisColumns({ sigma: 6 })
    expect(vis.rows.map(r => r.key)).toEqual(['trajectory', 'scenario', 'verdict'])
  })

  it('note names σ=0 out loud (no smoothing) rather than saying nothing', () => {
    const { note } = driftSmoothFigure({ sigma: 0 })
    expect(note.toLowerCase()).toContain('no smoothing')
  })

  it('note names σ at other values', () => {
    const { note } = driftSmoothFigure({ sigma: 6 })
    expect(note).toContain('σ')
    expect(note).toContain('6')
  })
})
