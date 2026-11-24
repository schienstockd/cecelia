import { describe, it, expect } from 'vitest'
import type { GateSpec } from '../stores/gating'
import { withDivider } from './splitGeometry'
import {
  histCounts, countTicks, bisectorRegions, defaultRuleWindow, histRegion, histHandles, moveHistHandle, toggleRule,
} from './histGeometry'

// Expected values from the definitions (equal bins over [lo, hi]; + above the cut; a window a fifth of the
// view either side), not from the code's output.
const base = { x_channel: 'c', y_channel: 'c', x_transform: { kind: 'linear' }, y_transform: { kind: 'linear' } } as const
const view = { xMin: 0, xMax: 10, yMin: 0, yMax: 100 }
const bis = (cut: number, side = 1, extra: Partial<GateSpec> = {}): GateSpec => ({ ...base, kind: 'bisector', cut, side, ...extra })

describe('histCounts', () => {
  it('bins x of each (x, y) pair into equal bins; out of range and NaN are not counted', () => {
    // x values 0.5, 1.5, 1.6, 9.99, 10 (the top edge belongs to the last bin), -1, 11, NaN
    const pts = new Float32Array([0.5, 0, 1.5, 0, 1.6, 0, 9.99, 0, 10, 0, -1, 0, 11, 0, NaN, 0])
    const c = histCounts(pts, 0, 10, 10)
    expect(c).toEqual([1, 2, 0, 0, 0, 0, 0, 0, 0, 2])
  })
  it('an empty range counts nothing', () => {
    expect(histCounts(new Float32Array([1, 1]), 5, 5, 4)).toEqual([0, 0, 0, 0])
  })
})

describe('countTicks', () => {
  it('a 1/2/5 step from 0, at most ~5 ticks', () => {
    expect(countTicks(1000).map(t => t.pos)).toEqual([0, 200, 400, 600, 800, 1000])
    expect(countTicks(37).map(t => t.pos)).toEqual([0, 10, 20, 30])
    expect(countTicks(0)).toEqual([{ pos: 0, label: '0' }])
  })
})

describe('bisector', () => {
  it('names name+ (above, side 1) and name-, dropping the path separator', () => {
    expect(bisectorRegions(' CD3/a ')).toEqual([{ name: 'CD3a+', region: 1 }, { name: 'CD3a-', region: -1 }])
  })
  it('the region each half covers in the view', () => {
    expect(histRegion(bis(4, 1), view)).toEqual([4, 10])
    expect(histRegion(bis(4, -1), view)).toEqual([0, 4])
    expect(histRegion(bis(12, 1), view)).toBeNull()                   // the + half is off to the right
    expect(histRegion({ ...base, kind: 'range', lo: -5, hi: 3 }, view)).toEqual([0, 3])
  })
  it('a fixed cut drags; a rule cut does not — its window ends do', () => {
    expect(histHandles(bis(4)).map(h => h.part)).toEqual(['cut'])
    const r = bis(4, 1, { rule: { kind: 'valley', lo: 2, hi: 6 } })
    expect(histHandles(r)).toEqual([{ part: 'rule-lo', x: 2 }, { part: 'rule-hi', x: 6 }])
    expect(moveHistHandle(r, 'rule-hi', 1).rule).toEqual({ kind: 'valley', lo: 1, hi: 2 })   // kept in order
    expect(moveHistHandle({ ...base, kind: 'range', lo: 2, hi: 5 }, 'lo', 7)).toMatchObject({ lo: 5, hi: 7 })
  })
  it('the rule toggles on around the cut, inside the view, and off keeping the cut', () => {
    expect(defaultRuleWindow(5, view)).toEqual([3, 7])
    expect(defaultRuleWindow(1, view)).toEqual([0, 3])
    const on = toggleRule(bis(5), view)
    expect(on.rule).toEqual({ kind: 'valley', lo: 3, hi: 7 })
    const off = toggleRule({ ...on, cut: 4.2, rule_ok: true }, view)
    expect(off.rule).toBeUndefined(); expect(off.rule_ok).toBeUndefined(); expect(off.cut).toBe(4.2)
  })
  it('the other half follows a dragged cut and keeps its side', () => {
    const a = bis(4, 1, { group: 'g' }), b = bis(4, -1, { group: 'g' })
    expect(withDivider(b, { ...a, cut: 6 })).toMatchObject({ cut: 6, side: -1 })
    expect(withDivider({ ...b, group: 'h' }, { ...a, cut: 6 }).cut).toBe(4)
  })
})
