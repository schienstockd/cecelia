/**
 * The trajectory-smoothing figure's behaviour, tested without mounting anything — same pattern
 * `driftVis.test.ts` follows.
 *
 * The claims worth pinning are the ones a reader of the figure would take away: σ=0 is a no-op,
 * a wider σ collapses a random-walk to fewer integer transitions, a slow ramp survives across a
 * broad σ range, a fast step gets rounded off when σ exceeds its width, and a stage bump is kept
 * in one frame. Plus the port itself: the figure must draw what the Python smoother does.
 */
import { describe, expect, it } from 'vitest'
import {
  BUMP_FRAME, bumpScenario, countTransitions, driftSmoothFigure, driftSmoothVisColumns,
  jerkScenario, noiseScenario, rampScenario, roundTrajectory, SMOOTH_VIS_COLUMNS,
  trajectorySmooth, trajectorySteps, transitionWidth, W,
} from './driftSmoothVis'

describe('trajectorySmooth', () => {
  it('σ=0 is the identity', () => {
    const t = noiseScenario()
    expect(trajectorySmooth(t, 0)).toEqual(t)
    // Guard against a buggy JSON default reaching the smoother as a negative — same shortcut
    // the Python side takes in `_smooth_positions`.
    expect(trajectorySmooth(t, -1)).toEqual(t)
  })

  it('smooths at the smallest σ the slider offers (0.5), like the Python side', () => {
    const t = noiseScenario()
    const s = trajectorySmooth(t, 0.5)
    expect(s.some((v, i) => Math.abs(v - t[i]) > 1e-9)).toBe(true)
  })

  it('reproduces a quadratic exactly, ends included (order-2 Savitzky–Golay)', () => {
    const t = Array.from({ length: 30 }, (_, i) => 0.02 * i * i - 0.5 * i + 3)
    const s = trajectorySmooth(t, 6)
    for (let i = 0; i < t.length; i++) expect(s[i]).toBeCloseTo(t[i], 9)
  })

  it('matches the Python smoother on a fixed trajectory (scipy golden values)', () => {
    // `_smooth_positions(x[:, None], 3.0)` on this input, printed from the analysis env. It cuts
    // after frame 9 (the 10.7 px jump) and smooths 10 + 12 frames with windows 9 and 11.
    const x = [0.0, 0.4, -0.3, 0.8, 1.1, 0.6, 1.4, 1.0, 1.9, 1.5, 12.2, 12.9, 12.4, 13.3, 12.8,
      13.6, 13.1, 14.0, 13.7, 14.4, 14.1, 14.9]
    const golden = [0.011515, 0.157879, 0.322424, 0.505152, 0.706061, 0.965801, 1.155931,
      1.33632, 1.50697, 1.667879, 12.346853, 12.546014, 12.743543, 12.939441, 13.133706,
      13.32634, 13.448252, 13.662517, 13.89986, 14.16028, 14.443776, 14.75035]
    expect(trajectorySteps(x)).toEqual([0, 10, 22])
    const s = trajectorySmooth(x, 3)
    for (let i = 0; i < x.length; i++) expect(s[i]).toBeCloseTo(golden[i], 5)
  })

  it('a wider σ produces smaller frame-to-frame differences', () => {
    const t = noiseScenario()
    const stepSize = (traj: number[]) => {
      let s = 0
      for (let i = 1; i < traj.length; i++) s += Math.abs(traj[i] - traj[i - 1])
      return s
    }
    // Monotone: bigger σ ⇒ smoother trajectory ⇒ smaller total variation.
    expect(stepSize(trajectorySmooth(t, 6))).toBeLessThan(stepSize(trajectorySmooth(t, 3)))
    expect(stepSize(trajectorySmooth(t, 3))).toBeLessThan(stepSize(t))
  })
})

describe('scenarios', () => {
  it('are deterministic — two renders of the figure are the same figure', () => {
    expect(noiseScenario()).toEqual(noiseScenario())
    expect(rampScenario()).toEqual(rampScenario())
    expect(jerkScenario()).toEqual(jerkScenario())
    expect(bumpScenario()).toEqual(bumpScenario())
  })

  it('all four have W frames', () => {
    expect(noiseScenario().length).toBe(W)
    expect(rampScenario().length).toBe(W)
    expect(jerkScenario().length).toBe(W)
    expect(bumpScenario().length).toBe(W)
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
    const outT = countTransitions(roundTrajectory(trajectorySmooth(raw, 6)))
    expect(rawT).toBeGreaterThan(6)                              // the raw wobbles across integers
    expect(outT).toBeLessThan(rawT / 3)                          // smoothed collapses it
  })

  it('ramp: at σ=6 the rounded end-to-end drift is nearly the raw drift', () => {
    const raw = rampScenario()
    const rounded = roundTrajectory(trajectorySmooth(raw, 6))
    const rawInt = roundTrajectory(raw)
    const range = (a: number[]) => Math.max(...a) - Math.min(...a)
    // Preserves at least 60% of the drift range — the whole point of the property.
    expect(range(rounded)).toBeGreaterThanOrEqual(range(rawInt) - 2)
  })

  it('jerk: plateaus still land near the raw plateaus at σ=6', () => {
    const raw = jerkScenario()
    const rounded = roundTrajectory(trajectorySmooth(raw, 6))
    // ±2 px slack: this step is too gradual (~2.7 px/frame) to be cut as a stage bump, so one
    // quadratic fit spans the plateau end AND part of the S-curve, and lands up to 2 px off the
    // plateau. The trade the smoother makes — a gaussian kept the plateau but smeared the
    // corners by far more. What the reader takes away is "both plateaus visible on opposite sides
    // of zero", not exact recovery.
    const rawStart = Math.round(raw.slice(0, 4).reduce((s, v) => s + v, 0) / 4)
    const rawEnd   = Math.round(raw.slice(raw.length - 4).reduce((s, v) => s + v, 0) / 4)
    expect(Math.abs(rounded[0] - rawStart)).toBeLessThanOrEqual(2)
    expect(Math.abs(rounded[rounded.length - 1] - rawEnd)).toBeLessThanOrEqual(2)
    // ... and end up on opposite sides of zero (a real step, not a wobble).
    expect(Math.sign(rounded[0])).not.toBe(Math.sign(rounded[rounded.length - 1]))
  })

  it('σ increases visibly widen the step transition — the "eating" property, monotone', () => {
    // The teaching claim of column 3: σ has a trade-off with fast motion. Wider σ ⇒ wider
    // transition. Doesn't pin an absolute width — that would tie the test to the amplitude of
    // the jerk scenario, which the figure may need to retune for legibility.
    const raw = jerkScenario()
    const w0  = transitionWidth(roundTrajectory(raw))
    const w6  = transitionWidth(roundTrajectory(trajectorySmooth(raw,  6)))
    const w15 = transitionWidth(roundTrajectory(trajectorySmooth(raw, 15)))
    expect(w6).toBeGreaterThanOrEqual(w0)
    expect(w15).toBeGreaterThan(w6)
  })
})

describe('the stage bump — the case the smoother is built not to touch', () => {
  it('stays a single-frame jump at σ=6 and at σ=15', () => {
    const raw = bumpScenario()
    for (const sigma of [6, 15]) {
      const r = roundTrajectory(trajectorySmooth(raw, sigma))
      expect(Math.abs(r[BUMP_FRAME] - r[BUMP_FRAME - 1])).toBeGreaterThanOrEqual(9)
    }
  })

  it('its verdict says so', () => {
    const vis = driftSmoothVisColumns({ sigma: 6 })
    const verdict = vis.rows.find(r => r.key === 'verdict')!
    expect(verdict.cells[SMOOTH_VIS_COLUMNS.indexOf('bump')].text).toBe('bump kept in one frame')
  })
})

describe('driftSmoothVisColumns', () => {
  it('produces the four named columns in order', () => {
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
