import { describe, it, expect } from 'vitest'
import { normSpan, applyDrag, clampSpans, spansToSlots, slotsToSpans, buildPlate, type PlateSpan } from './plateBuilder'

describe('normSpan', () => {
  it('orders two dragged corners top-left → bottom-right', () => {
    expect(normSpan({ r: 2, c: 3 }, { r: 0, c: 1 })).toEqual({ r0: 0, c0: 1, r1: 2, c1: 3 })
  })
})

describe('spansToSlots', () => {
  it('a plain grid with no spans is row-major 1×1 cells', () => {
    expect(spansToSlots(2, 2, [])).toEqual([
      '1 / 1 / 2 / 2', '1 / 2 / 2 / 3', '2 / 1 / 3 / 2', '2 / 2 / 3 / 3',
    ])
  })
  it('emits a span at its top-left, remaining cells as 1×1s (row-major)', () => {
    // 3×2 grid, top row merged into one banner
    const banner: PlateSpan = { r0: 0, c0: 0, r1: 0, c1: 2 }
    expect(spansToSlots(3, 2, [banner])).toEqual([
      '1 / 1 / 2 / 4',                       // banner spans all 3 cols of row 1
      '2 / 1 / 3 / 2', '2 / 2 / 3 / 3', '2 / 3 / 3 / 4',
    ])
  })
})

describe('applyDrag', () => {
  it('a multi-cell rectangle merges and drops overlapping spans', () => {
    const rect: PlateSpan = { r0: 0, c0: 0, r1: 1, c1: 1 }
    expect(applyDrag([], rect)).toEqual([rect])
    // a new merge overlapping an old one replaces it
    const old: PlateSpan = { r0: 0, c0: 0, r1: 0, c1: 1 }
    expect(applyDrag([old], rect)).toEqual([rect])
  })
  it('a single cell inside a span un-merges it; elsewhere is a no-op', () => {
    const span: PlateSpan = { r0: 0, c0: 0, r1: 1, c1: 1 }
    expect(applyDrag([span], { r0: 0, c0: 0, r1: 0, c1: 0 })).toEqual([])   // split
    expect(applyDrag([span], { r0: 3, c0: 3, r1: 3, c1: 3 })).toEqual([span]) // no-op
  })
})

describe('clampSpans', () => {
  it('drops spans that fall outside a resized grid', () => {
    const inside: PlateSpan = { r0: 0, c0: 0, r1: 1, c1: 1 }
    const outside: PlateSpan = { r0: 2, c0: 0, r1: 3, c1: 1 }
    expect(clampSpans([inside, outside], 2, 2)).toEqual([inside])
  })
})

describe('slotsToSpans (round-trip)', () => {
  it('recovers multi-cell spans from slot areas and drops 1×1s', () => {
    const banner: PlateSpan = { r0: 0, c0: 0, r1: 0, c1: 2 }
    const slots = spansToSlots(3, 2, [banner])
    expect(slotsToSpans(slots)).toEqual([banner])
  })
})

describe('buildPlate', () => {
  it('produces a template with clamped spans applied', () => {
    const t = buildPlate(2, 2, [{ r0: 0, c0: 0, r1: 0, c1: 1 }])
    expect(t).toMatchObject({ cols: 2, rows: 2, orient: 'any' })
    expect(t.slots).toEqual(['1 / 1 / 2 / 3', '2 / 1 / 3 / 2', '2 / 2 / 3 / 3'])
  })
})
