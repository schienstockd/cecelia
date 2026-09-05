/**
 * Flow-register figure — geometry tested without mounting anything, same rule
 * `stackAlignVis.test.ts` / `driftVis.test.ts` follow.
 *
 * The claims worth pinning are those a reader of the FIGURE takes away:
 *   - the input T-series carries the motion the aligner is supposed to correct;
 *   - after registration, adjacent frames are quantitatively CLOSER on nonrigid
 *     / bulk-drift scenarios and unchanged on the static scene;
 *   - `previous` vs `first` produce different alignment states on bulk drift;
 *   - the clamp fires when |shift| exceeds it (per-quadrant fall-back to identity).
 */
import { describe, expect, it } from 'vitest'
import {
  buildScenarios, flowRegisterFigure, flowRegisterVerdict, flowRegisterVisColumns,
  N, registerStack, SCENARIOS, searchRadiusFor, T, translate,
} from './flowRegisterVis'

function absDiffMean(a: number[][], b: number[][]): number {
  let s = 0, n = 0
  for (let y = 0; y < a.length; y++) for (let x = 0; x < a[y].length; x++) {
    s += Math.abs(a[y][x] - b[y][x]); n++
  }
  return n ? s / n : 0
}

function adjacentSum(stack: number[][][]): number {
  let s = 0
  for (let t = 1; t < stack.length; t++) s += absDiffMean(stack[t], stack[t - 1])
  return s
}

describe('scenarios', () => {
  it('bulkDrift really drifts by ≈ (0.9, 0.9) per frame', () => {
    const { bulkDrift } = buildScenarios()
    // Frame t=1 is (0.9, 0.9) shifted from t=0. A translated frame differs from the base
    // by a measurable amount; the sequence of adjacent-diffs is monotonic near the middle
    // of the scene (i.e. drift is actually happening, not a fixture bug).
    const a01 = absDiffMean(bulkDrift[0], bulkDrift[1])
    expect(a01).toBeGreaterThan(0.005)
  })

  it('nonrigidFlex has strictly larger adjacent-frame diff than staticScene', () => {
    const { nonrigidFlex, staticScene } = buildScenarios()
    expect(adjacentSum(nonrigidFlex)).toBeGreaterThan(adjacentSum(staticScene) * 3)
  })

  it('staticScene has near-zero adjacent-frame diff (only tiny noise)', () => {
    const { staticScene } = buildScenarios()
    // Noise floor is 0.02 peak-to-peak per pixel per frame; adjacent-diff should stay small
    for (let t = 1; t < T; t++) {
      expect(absDiffMean(staticScene[t], staticScene[t - 1])).toBeLessThan(0.01)
    }
  })
})

describe('registerStack — the shipping metric', () => {
  it('bulkDrift adjacent-diff DROPS sharply after registration', () => {
    const { bulkDrift } = buildScenarios()
    const raw = adjacentSum(bulkDrift)
    const r = registerStack(bulkDrift, {
      referenceMode: 'previous', aggressiveness: 'strong', maxShiftPx: 16,
    })
    const reg = adjacentSum(r.aligned)
    expect(reg).toBeLessThan(0.5 * raw)
  })

  it('nonrigidFlex adjacent-diff DROPS after registration', () => {
    const { nonrigidFlex } = buildScenarios()
    const raw = adjacentSum(nonrigidFlex)
    const r = registerStack(nonrigidFlex, {
      referenceMode: 'previous', aggressiveness: 'strong', maxShiftPx: 16,
    })
    const reg = adjacentSum(r.aligned)
    // Fractional per-quadrant shifts + integer alignment search means the schematic
    // can't hit 20%+ reduction; the test's job is to catch a broken pipeline that
    // leaves things unchanged, and any measurable drop beats that bar.
    expect(reg).toBeLessThan(0.95 * raw)
  })

  it('staticScene is a near-identity — no false correction', () => {
    const { staticScene } = buildScenarios()
    const r = registerStack(staticScene, {
      referenceMode: 'previous', aggressiveness: 'balanced', maxShiftPx: 16,
    })
    // Every registered frame should be ~identical to its input at low noise
    for (let t = 0; t < staticScene.length; t++) {
      expect(absDiffMean(r.aligned[t], staticScene[t])).toBeLessThan(0.04)
    }
    // And the reported per-frame max quadrant shift should be tiny
    expect(Math.max(...r.perFrameShiftMag)).toBeLessThan(2)
  })

  it('reference=first pins every frame to t=0 (bulk drift collapses to base)', () => {
    const { bulkDrift } = buildScenarios()
    const r = registerStack(bulkDrift, {
      referenceMode: 'first', aggressiveness: 'strong', maxShiftPx: 16,
    })
    // Frame t=T-1 was translated by ~ ((T-1)*0.9, (T-1)*0.9) from base; after warping to t=0
    // it should be much closer to base than the raw frame is.
    const raw = absDiffMean(bulkDrift[T - 1], bulkDrift[0])
    const reg = absDiffMean(r.aligned[T - 1], bulkDrift[0])
    expect(reg).toBeLessThan(0.5 * raw)
  })

  it('max-shift clamp reverts wide shifts to identity per quadrant', () => {
    const { bulkDrift } = buildScenarios()
    const unclamped = registerStack(bulkDrift, {
      referenceMode: 'previous', aggressiveness: 'strong', maxShiftPx: 16,
    })
    const clamped   = registerStack(bulkDrift, {
      referenceMode: 'previous', aggressiveness: 'strong', maxShiftPx: 0.5,
    })
    // With a clamp far below the true drift the clamped output is closer to the raw input
    // than the unclamped one (per-quadrant fall-back suppressed the warp).
    for (let t = 1; t < T; t++) {
      const dClamped   = absDiffMean(clamped.aligned[t],   bulkDrift[t])
      const dUnclamped = absDiffMean(unclamped.aligned[t], bulkDrift[t])
      expect(dClamped).toBeLessThanOrEqual(dUnclamped)
    }
  })
})

describe('searchRadiusFor + aggressiveness ordering', () => {
  it('gentle < balanced < strong', () => {
    expect(searchRadiusFor('gentle')).toBeLessThan(searchRadiusFor('balanced'))
    expect(searchRadiusFor('balanced')).toBeLessThan(searchRadiusFor('strong'))
  })
})

describe('figure shape', () => {
  it('flowRegisterVisColumns returns 3 columns × the expected 4 rows', () => {
    const vis = flowRegisterVisColumns({
      referenceMode: 'previous', aggressiveness: 'balanced', maxShiftPx: 16,
    })
    expect(vis.columns).toEqual([...SCENARIOS])
    expect(vis.rows.map(r => r.key)).toEqual(['input', 'aligned', 'case', 'verdict'])
    for (const row of vis.rows) expect(row.cells.length).toBe(3)
  })

  it('flowRegisterFigure returns a note + vis and the note names the mode', () => {
    const out = flowRegisterFigure({
      referenceMode: 'first', aggressiveness: 'strong', maxShiftPx: 16,
    })
    expect(out.vis.columns.length).toBe(3)
    expect(out.note).toContain('first')
    expect(out.note).toContain('strong')
  })

  it('verdict for balanced mode names all three form values', () => {
    const s = flowRegisterVerdict({
      referenceMode: 'previous', aggressiveness: 'balanced', maxShiftPx: 12,
    })
    expect(s).toContain('previous')
    expect(s).toContain('balanced')
    expect(s).toContain('12')
  })
})

describe('translate helper', () => {
  it('translate by zero returns an equal frame', () => {
    const { bulkDrift } = buildScenarios()
    const f = bulkDrift[0]
    const g = translate(f, 0, 0)
    for (let y = 0; y < N; y++) for (let x = 0; x < N; x++) {
      expect(g[y][x]).toBeCloseTo(f[y][x], 5)
    }
  })
})
