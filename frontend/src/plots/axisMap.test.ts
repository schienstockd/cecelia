import { describe, it, expect } from 'vitest'
import { yFrac, xFrac, dataToPx, pxToData, gridToPx, type Ext, type PxBox } from './axisMap'

const SRC = import.meta.glob(['/src/**/*.vue', '/src/**/*.ts'],
  { query: '?raw', import: 'default', eager: true }) as Record<string, string>

const e: Ext = { xMin: 0, xMax: 10, yMin: 0, yMax: 100 }
const box = (o: Partial<PxBox> = {}): PxBox => ({ w: 200, h: 400, ...o })

describe('xFrac', () => {
  it('runs left to right and is never flipped', () => {
    expect(xFrac(e, 0)).toBe(0)
    expect(xFrac(e, 10)).toBe(1)
    expect(xFrac(e, 5)).toBe(0.5)
  })
})

describe('yFrac', () => {
  // the chart default: bigger value, higher on screen
  it('unflipped puts yMax at the top', () => {
    expect(yFrac(e, 100)).toBe(0)
    expect(yFrac(e, 0)).toBe(1)
  })

  // an image row index: 0 is the TOP of the frame, which is what viewer draws
  it('flipped puts yMin at the top', () => {
    expect(yFrac(e, 0, true)).toBe(0)
    expect(yFrac(e, 100, true)).toBe(1)
  })

  it('the two are mirror images', () => {
    for (const v of [0, 25, 50, 75, 100]) expect(yFrac(e, v) + yFrac(e, v, true)).toBeCloseTo(1, 12)
  })
})

describe('dataToPx', () => {
  it('maps the corners', () => {
    expect(dataToPx(e, box(), 0, 100)).toEqual([0, 0])
    expect(dataToPx(e, box(), 10, 0)).toEqual([200, 400])
  })
  it('flips y for an image axis', () => {
    expect(dataToPx(e, box({ flipY: true }), 0, 0)).toEqual([0, 0])
    expect(dataToPx(e, box({ flipY: true }), 10, 100)).toEqual([200, 400])
  })
  it('honours an inset plot area', () => {
    expect(dataToPx(e, box({ x0: 30, y0: 12 }), 0, 100)).toEqual([30, 12])
  })
})

// THE INVARIANT THAT MATTERS. `pxToData` turns a dragged rectangle into the numbers written to
// gating/{value_name}.json. If it and `dataToPx` ever disagree about the flip, every new position
// gate is stored MIRRORED — a wrong gate on disk, applied to every future image, and nothing looks
// wrong on screen at the moment it happens.
describe('round trip', () => {
  for (const flipY of [false, true]) {
    it(`pxToData undoes dataToPx exactly (flipY=${flipY})`, () => {
      const b = box({ flipY, x0: 7, y0: 3 })
      for (const [vx, vy] of [[0, 0], [10, 100], [3.7, 41.2], [9.99, 0.01]] as [number, number][]) {
        const [px, py] = dataToPx(e, b, vx, vy)
        const [rx, ry] = pxToData(e, b, px, py)
        expect(rx).toBeCloseTo(vx, 10)
        expect(ry).toBeCloseTo(vy, 10)
      }
    })
  }

  it('a gate drawn on a flipped axis stores the SAME data span as on an unflipped one', () => {
    // the user drags the same two screen corners; the stored gate must cover the same data range
    const [ax, ay] = pxToData(e, box({ flipY: false }), 40, 80)
    const [bx, by] = pxToData(e, box({ flipY: false }), 160, 320)
    const [cx, cy] = pxToData(e, box({ flipY: true }), 40, 80)
    const [dx, dy] = pxToData(e, box({ flipY: true }), 160, 320)
    expect(Math.min(ax, bx)).toBeCloseTo(Math.min(cx, dx), 10)
    expect(Math.max(ax, bx)).toBeCloseTo(Math.max(cx, dx), 10)
    expect(Math.min(ay, by)).toBeCloseTo(Math.min(cy, dy), 10)
    expect(Math.max(ay, by)).toBeCloseTo(Math.max(cy, dy), 10)
  })
})

describe('degenerate extents', () => {
  const flat: Ext = { xMin: 5, xMax: 5, yMin: 2, yMax: 2 }
  it('never divides by zero', () => {
    const [px, py] = dataToPx(flat, box(), 5, 2)
    expect(Number.isFinite(px)).toBe(true)
    expect(Number.isFinite(py)).toBe(true)
  })
  it('survives a zero-size box', () => {
    const [vx, vy] = pxToData(e, { w: 0, h: 0 }, 0, 0)
    expect(Number.isFinite(vx)).toBe(true)
    expect(Number.isFinite(vy)).toBe(true)
  })
})

describe('gridToPx', () => {
  // the density grid is binned in DATA order (row 0 at yMin), so it must go through the same mapping
  // rather than carrying a second orientation rule that could disagree with the points drawn over it
  it('grid row 0 is at yMin, wherever that lands on screen', () => {
    expect(gridToPx(e, box(), 64, 0, 0)).toEqual([0, 400])
    expect(gridToPx(e, box({ flipY: true }), 64, 0, 0)).toEqual([0, 0])
  })
  it('the last row is at yMax', () => {
    expect(gridToPx(e, box(), 64, 64, 64)).toEqual([200, 0])
  })
})

// The reason this module exists is that the same six lines lived in four files. A fifth copy would not
// fail anything — it would just quietly keep the old orientation while everything else flipped, which
// is the hardest kind of bug to see: half a plot mirrored.
//
// `density.ts` is exempt: it bins the grid in DATA order (row 0 at yMin) and never touches pixels;
// `gridToPx` converts those back through this module.
describe('no component hand-rolls the y mapping', () => {
  it('every screen-space y comes from axisMap', () => {
    const offenders: string[] = []
    for (const [path, text] of Object.entries(SRC)) {
      if (path.includes('/plots/axisMap') || path.includes('/plots/density.ts')) continue
      if (path.includes('.test.')) continue
      // the shape: `1 - (something - <anything>yMin) / <anything>` — a flip-less inverse of a y extent
      if (/1\s*-\s*\(?\s*\(?[\w.]+\s*-\s*[\w.]*yMin\s*\)?\s*\//.test(text))
        offenders.push(path.replace('/src/', ''))
    }
    expect(offenders,
      'use dataToPx / pxToData / yFrac from plots/axisMap — a private copy cannot flip for an image axis')
      .toEqual([])
  })
})
