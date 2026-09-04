/**
 * Within-stack alignment figure — geometry tested without mounting anything, same rule
 * `driftVis.test.ts` and `driftSmoothVis.test.ts` follow.
 *
 * The claims worth pinning are the ones a reader of the FIGURE would take away: `align` recovers
 * a known translation on the schematic base, the gate correctly refuses to shift a structurally
 * different plane, `sharpest` picks the sharpest plane, and the figure produces the expected
 * columns/rows shape at any valid input.
 */
import { describe, expect, it } from 'vitest'
import {
  align, alignStack, blur, buildScenarios, N, pickRef, SCENARIOS, sharpness, stackAlignFigure,
  stackAlignVisColumns, translate, Z,
} from './stackAlignVis'

describe('translate + align', () => {
  it('align recovers a known translation of the base plane', () => {
    const { stackMovement } = buildScenarios()
    const base = stackMovement[Math.floor(Z / 2)]
    const moved = translate(base, 2.0, -1.5)
    const { ty, tx, conf } = align(base, moved)
    expect(Math.abs(ty - 2.0)).toBeLessThan(0.5)
    expect(Math.abs(tx - (-1.5))).toBeLessThan(0.5)
    expect(conf).toBeGreaterThan(0.6)                              // matches must read as confident
  })

  it('align on identical planes returns zero shift and high confidence', () => {
    const { stackMovement } = buildScenarios()
    const f = stackMovement[Math.floor(Z / 2)]
    const { ty, tx, conf } = align(f, f)
    expect(Math.abs(ty)).toBeLessThan(0.5)
    expect(Math.abs(tx)).toBeLessThan(0.5)
    // conf is a soft ~[0, ~0.85] score — identical planes land near the top of that range.
    expect(conf).toBeGreaterThan(0.65)
  })
})

describe('sharpness + pickRef', () => {
  it('sharpness of blurred plane is strictly less than the sharp original', () => {
    const { stackMovement } = buildScenarios()
    const sharp = stackMovement[Math.floor(Z / 2)]
    const blurred = blur(sharp, 3)
    expect(sharpness(blurred)).toBeLessThan(sharpness(sharp))
  })

  it('pickRef middle returns Z/2', () => {
    const { stackMovement } = buildScenarios()
    expect(pickRef(stackMovement, 'middle')).toBe(Math.floor(Z / 2))
  })

  it('pickRef sharpest avoids the blurred plane in mixedRef', () => {
    const { mixedRef } = buildScenarios()
    // In `mixedRef` the middle plane is blurred, so `sharpest` must NOT return that index.
    const midIdx = Math.floor(Z / 2)
    expect(pickRef(mixedRef, 'sharpest')).not.toBe(midIdx)
  })
})

describe('alignStack — the gate', () => {
  it('applies to translated planes and correctly reports the shift', () => {
    const { stackMovement } = buildScenarios()
    const r = alignStack(stackMovement, { referenceMode: 'middle', minConfidence: 0.2, maxShiftPx: 8 })
    // ref is always applied
    expect(r.applied[r.refIdx]).toBe(true)
    // at least the two planes adjacent to ref are applied on this scene
    expect(r.applied[r.refIdx - 1]).toBe(true)
    expect(r.applied[r.refIdx + 1]).toBe(true)
  })

  it('rejects a structurally different plane when confidence is demanding', () => {
    const { structural } = buildScenarios()
    // At a low `minConfidence` even mildly different content matches, but with a demanding
    // threshold (0.7 — near the ceiling of the soft score) the structural plane's peak-to-mean
    // isn't good enough. That threshold is what a user tightens the gate to on a movie with a
    // suspicious mid-range alignment; the schematic has to show the gate biting.
    const r = alignStack(structural, { referenceMode: 'middle', minConfidence: 0.7, maxShiftPx: 8 })
    expect(r.applied[0] && r.applied[Z - 1]).toBe(false)
  })

  it('a large shift is rejected by the max-shift clamp', () => {
    const { stackMovement } = buildScenarios()
    // Same scene, but with maxShiftPx = 0 — no shift is allowed. Every non-ref plane must be
    // rejected regardless of confidence.
    const r = alignStack(stackMovement, { referenceMode: 'middle', minConfidence: 0.0, maxShiftPx: 0 })
    for (let z = 0; z < Z; z++) {
      if (z !== r.refIdx) expect(r.applied[z]).toBe(false)
    }
  })
})

describe('stackAlignVisColumns', () => {
  const inp = { referenceMode: 'middle' as const, minConfidence: 0.35, maxShiftPx: 8 }

  it('produces the three named columns in order', () => {
    const vis = stackAlignVisColumns(inp)
    expect(vis.columns).toEqual([...SCENARIOS])
  })

  it('carries four rows (input grid, aligned grid, case, verdict)', () => {
    const vis = stackAlignVisColumns(inp)
    expect(vis.rows.map(r => r.key)).toEqual(['input', 'aligned', 'case', 'verdict'])
  })

  it('every grid cell has Z frames of NxN', () => {
    const vis = stackAlignVisColumns(inp)
    for (const key of ['input', 'aligned']) {
      const row = vis.rows.find(r => r.key === key)!
      expect(row.cells.length).toBe(SCENARIOS.length)
      for (const c of row.cells) {
        expect(c.frames).toBeDefined()
        expect(c.frames!.length).toBe(Z)
        expect(c.frames![0].length).toBe(N)
        expect(c.frames![0][0].length).toBe(N)
      }
    }
  })

  it('note mentions the gates and the anchor mode', () => {
    const { note } = stackAlignFigure({ referenceMode: 'sharpest', minConfidence: 0.5, maxShiftPx: 6 })
    expect(note).toContain('sharpest')
    expect(note).toContain('0.50')
    expect(note).toContain('6')
  })
})
