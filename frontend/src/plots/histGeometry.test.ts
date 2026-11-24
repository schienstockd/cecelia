import { describe, it, expect } from 'vitest'
import type { GateSpec } from '../stores/gating'
import { withDivider } from './splitGeometry'
import {
  histCounts, countTicks, bisectorRegions, defaultRuleWindow, histRegion, histHandles, moveHistHandle, toggleRule,
  histLines, histWindows, shiftRange, histogramView, histTip,
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

describe('histogramView', () => {
  it('bins the base and each overlay over the view; the count axis tops the tallest bin', () => {
    const base = new Float32Array([1, 1, 1, 1, 3, 3]), sub = new Float32Array([3, 3])
    const v = histogramView(base, [{ colour: '#f00', points: sub }], 0, 4)
    expect(v.hist.base.reduce((a, b) => a + b, 0)).toBe(3)
    expect(Math.max(...v.hist.base)).toBe(2)                     // the two x = 1 values share a bin
    expect(v.hist.layers[0]).toMatchObject({ colour: '#f00' })
    expect(v.yMax).toBeCloseTo(2 * 1.08)
    expect(v.yTicks.at(-1)!.pos).toBeLessThanOrEqual(2)
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
  it("a range's ends follow their own valleys: handles, lines, windows, toggles", () => {
    const r: GateSpec = { ...base, kind: 'range', lo: 2, hi: 8 }
    const on = toggleRule(r, view, 'hi')
    expect(on.hi_rule).toEqual({ kind: 'valley', lo: 6, hi: 10 })            // a fifth either side, in view
    expect(on.lo_rule).toBeUndefined()
    expect(histHandles(on).map(h => h.part)).toEqual(['lo', 'hi_rule-lo', 'hi_rule-hi'])
    expect(histLines({ ...on, hi_rule_ok: false })).toEqual([
      { end: 'lo', x: 2, ruled: false, ok: true }, { end: 'hi', x: 8, ruled: true, ok: false }])
    expect(histWindows(on)).toEqual([[6, 10]])
    expect(moveHistHandle(on, 'hi_rule-lo', 7).hi_rule).toMatchObject({ lo: 7, hi: 10 })
    expect(shiftRange(on, 1)).toMatchObject({ lo: 3, hi: 9, hi_rule: { lo: 7, hi: 11 } })
    expect(toggleRule({ ...on, hi_rule_ok: true }, view, 'hi')).toEqual(r)   // off again
    expect(toggleRule(r, view, 'cut')).toBe(r)                                // a range has no cut
  })
  it("draws the windows a range's ends search: an overlap split at its middle, a fixed end a limit", () => {
    const n: GateSpec = { ...base, kind: 'range', lo: 4, hi: 6,
                          lo_rule: { kind: 'valley', lo: 2, hi: 7 }, hi_rule: { kind: 'valley', lo: 4, hi: 8 } }
    expect(histWindows(n)).toEqual([[2, 5.5], [5.5, 8]])                         // overlap [4, 7] → 5.5
    expect(histWindows({ ...n, lo_rule: undefined, lo: 3 })).toEqual([[4, 8]])   // nothing to split with
    expect(histWindows({ ...n, lo_rule: undefined, lo: 5 })).toEqual([[5, 8]])   // stops at the fixed lo
    expect(histWindows({ ...n, hi_rule: undefined, hi: 9 })).toEqual([[2, 7]])
  })
  it('says what a hovered handle or ruled line is, and whether its rule found a valley', () => {
    const r = bis(4, 1, { rule: { kind: 'valley', lo: 2, hi: 6 } })
    expect(histTip(r, { kind: 'hist-handle', part: 'rule-hi' })).toMatch(/search window/)
    expect(histTip(r, { kind: 'hist-line', end: 'cut' })).toMatch(/^Cut at the valley/)
    expect(histTip({ ...r, rule_ok: false }, { kind: 'hist-line', end: 'cut' })).toMatch(/No valley in window: cut stays put/)
    expect(histTip({ ...base, kind: 'range', lo: 1, hi: 5, hi_rule: { kind: 'valley', lo: 4, hi: 7 }, hi_rule_ok: false },
                   { kind: 'hist-line', end: 'hi' })).toMatch(/end stays put/)
    expect(histTip(bis(4), { kind: 'hist-handle', part: 'cut' })).toMatch(/follow the valley/)
  })
})
