/**
 * The Z-plane-coupling figure's behaviour, tested without mounting anything — same pattern
 * `driftSmoothVis.test.ts` follows.
 *
 * The claims worth pinning are the ones a reader of the figure would take away: σ=0 is a no-op,
 * a wider σ pulls a lone outlier plane toward the ramp, the linear shear survives moderate σ,
 * and independent per-plane noise collapses toward zero as σ climbs.
 */
import { describe, expect, it } from 'vitest'
import {
  driftZSmoothFigure, driftZSmoothVisColumns, gaussianSmoothZ, noiseScenario,
  OUTLIER_Z, outlierScenario, shearScenario, W, ZP, Z_SMOOTH_VIS_COLUMNS,
} from './driftZSmoothVis'

describe('gaussianSmoothZ', () => {
  it('σ=0 is the identity', () => {
    const s = shearScenario()
    expect(gaussianSmoothZ(s, 0)).toEqual(s)
    // Guard against a buggy JSON default reaching the smoother as a negative — same shortcut
    // the Python side takes in `_estimate_drift_per_plane`.
    expect(gaussianSmoothZ(s, -1)).toEqual(s)
  })

  it('preserves the mean (up to edge effects)', () => {
    const s = shearScenario()
    const out = gaussianSmoothZ(s, 1)
    const meanS = s.reduce((a, b) => a + b, 0) / s.length
    const meanO = out.reduce((a, b) => a + b, 0) / out.length
    expect(Math.abs(meanS - meanO)).toBeLessThan(0.5)
  })

  it('a wider σ produces smaller plane-to-plane differences', () => {
    const s = noiseScenario()
    const totalVar = (vs: number[]) => {
      let a = 0
      for (let i = 1; i < vs.length; i++) a += Math.abs(vs[i] - vs[i - 1])
      return a
    }
    // Monotone: bigger σ ⇒ smoother profile ⇒ smaller total variation across planes.
    expect(totalVar(gaussianSmoothZ(s, 2))).toBeLessThan(totalVar(gaussianSmoothZ(s, 1)))
    expect(totalVar(gaussianSmoothZ(s, 1))).toBeLessThan(totalVar(s))
  })
})

describe('scenarios', () => {
  it('are deterministic — two renders of the figure are the same figure', () => {
    expect(shearScenario()).toEqual(shearScenario())
    expect(outlierScenario()).toEqual(outlierScenario())
    expect(noiseScenario()).toEqual(noiseScenario())
  })

  it('all three have ZP planes', () => {
    expect(shearScenario().length).toBe(ZP)
    expect(outlierScenario().length).toBe(ZP)
    expect(noiseScenario().length).toBe(ZP)
  })

  it('shear ramp goes from positive at the shallow plane to negative at the deep plane', () => {
    const s = shearScenario()
    expect(s[0]).toBeGreaterThan(0)
    expect(s[s.length - 1]).toBeLessThan(0)
  })

  it('outlier plane sits far off the underlying ramp', () => {
    const ramp = shearScenario()
    const out = outlierScenario()
    // Every plane except OUTLIER_Z matches the ramp exactly (deterministic PRNG).
    for (let z = 0; z < ZP; z++) {
      if (z !== OUTLIER_Z) expect(out[z]).toBe(ramp[z])
    }
    // ... and the outlier is at least 10 px off its ramp value.
    expect(Math.abs(out[OUTLIER_Z] - ramp[OUTLIER_Z])).toBeGreaterThan(10)
  })

  it('noise averages near zero (no bias)', () => {
    const s = noiseScenario()
    const mean = s.reduce((a, b) => a + b, 0) / s.length
    expect(Math.abs(mean)).toBeLessThan(1.5)
  })
})

describe('the property Z-coupling targets — shear survives, outlier snaps in, noise dampens', () => {
  it('shear: at moderate σ the range is largely preserved', () => {
    const s = shearScenario()
    const smoothed = gaussianSmoothZ(s, 1)
    const range = (vs: number[]) => Math.max(...vs) - Math.min(...vs)
    // Preserves at least 60% of the ramp range — the property Decision 3 in the plan pins.
    expect(range(smoothed)).toBeGreaterThanOrEqual(range(s) * 0.6)
  })

  it('outlier: at moderate σ the outlier plane snaps toward its neighbours', () => {
    const raw = outlierScenario()
    const smoothed = gaussianSmoothZ(raw, 1)
    // Target: mean of the two neighbouring RAW ramp values (their smoothed values move too,
    // but the target the reader has in mind is "the ramp the neighbours are on").
    const target = (raw[OUTLIER_Z - 1] + raw[OUTLIER_Z + 1]) / 2
    const rawGap = Math.abs(raw[OUTLIER_Z] - target)
    const outGap = Math.abs(smoothed[OUTLIER_Z] - target)
    expect(outGap).toBeLessThan(rawGap * 0.6)
  })

  it('noise: at higher σ the peak magnitude collapses', () => {
    const s = noiseScenario()
    const absMax = (vs: number[]) => vs.reduce((m, v) => Math.max(m, Math.abs(v)), 0)
    expect(absMax(gaussianSmoothZ(s, 2))).toBeLessThan(absMax(s) * 0.7)
  })

  it('σ = 5 (the JSON max) starts eating shear at the extremes', () => {
    const s = shearScenario()
    const range = (vs: number[]) => Math.max(...vs) - Math.min(...vs)
    // At the JSON's cap the range has visibly contracted (the "over-coupled" regime the note
    // warns about). Not a hard bound — the exact fraction depends on ZP and edge policy — just
    // "less than the raw", which is the claim the verdict makes.
    expect(range(gaussianSmoothZ(s, 5))).toBeLessThan(range(s))
  })
})

describe('driftZSmoothVisColumns', () => {
  it('produces the three named columns in order', () => {
    const vis = driftZSmoothVisColumns({ sigma: 1 })
    expect(vis.columns).toEqual([...Z_SMOOTH_VIS_COLUMNS])
  })

  it('every column has a grid frame at the current σ', () => {
    const vis = driftZSmoothVisColumns({ sigma: 1 })
    const grid = vis.rows.find(r => r.role === 'grid')!
    expect(grid.cells.length).toBe(Z_SMOOTH_VIS_COLUMNS.length)
    for (const c of grid.cells) {
      expect(c.frames).toBeDefined()
      expect(c.frames!.length).toBe(1)
      expect(c.frames![0].length).toBeGreaterThan(0)
      expect(c.frames![0][0].length).toBe(W)
    }
  })

  it('carries three rows (stack + case + verdict)', () => {
    const vis = driftZSmoothVisColumns({ sigma: 1 })
    expect(vis.rows.map(r => r.key)).toEqual(['stack', 'case', 'verdict'])
  })

  it('note names σ = 0 out loud (no coupling) rather than saying nothing', () => {
    const { note } = driftZSmoothFigure({ sigma: 0 })
    expect(note.toLowerCase()).toContain('no coupling')
  })

  it('note names σ at other values', () => {
    const { note } = driftZSmoothFigure({ sigma: 0.5 })
    expect(note).toContain('σ')
    expect(note).toContain('0.5')
  })

  it('note says bad planes are fixed and shear is kept at moderate σ', () => {
    const { note } = driftZSmoothFigure({ sigma: 0.5 })
    expect(note.toLowerCase()).toContain('bad planes')
    expect(note.toLowerCase()).toContain('shear')
  })

  it('note warns when σ starts eating shear', () => {
    const { note } = driftZSmoothFigure({ sigma: 2 })
    expect(note.toLowerCase()).toMatch(/soften|lower/)
  })

  it('note calls out the whole-stack collapse at σ ≥ 3', () => {
    // The whole point of surfacing the tradeoff: at high σ the feature reduces to whole-volume
    // correction and the shear per-plane was turned on for is erased. The line has to say that
    // out loud — a reader ramping the slider to max should be told what they just gave up.
    const { note } = driftZSmoothFigure({ sigma: 5 })
    expect(note.toLowerCase()).toContain('whole-volume')
  })

  it('every verdict line is short — under 80 chars', () => {
    // The line under a vis-aid is one hint, not an essay (docs/ui/COPY.md).
    for (const sigma of [0, 0.3, 0.5, 1.5, 2, 3, 5]) {
      const { note } = driftZSmoothFigure({ sigma })
      expect(note.length, `σ=${sigma}: "${note}"`).toBeLessThanOrEqual(80)
    }
  })
})
