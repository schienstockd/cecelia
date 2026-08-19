import { describe, it, expect } from 'vitest'
import { facetGrid, facetSlot, facetBox } from './facetGrid'

describe('facetGrid', () => {
  it('is as square as it can be, columns first', () => {
    expect(facetGrid(1)).toEqual({ cols: 1, rows: 1 })
    expect(facetGrid(2)).toEqual({ cols: 2, rows: 1 })
    expect(facetGrid(3)).toEqual({ cols: 2, rows: 2 })
    expect(facetGrid(4)).toEqual({ cols: 2, rows: 2 })
    expect(facetGrid(6)).toEqual({ cols: 3, rows: 2 })
    expect(facetGrid(9)).toEqual({ cols: 3, rows: 3 })
  })

  // callers divide by cols; zero groups must not divide by zero
  it('never returns zero columns', () => {
    expect(facetGrid(0).cols).toBe(1)
    expect(facetGrid(-3).cols).toBe(1)
  })

  it('always holds every facet', () => {
    for (let n = 1; n <= 40; n++) {
      const { cols, rows } = facetGrid(n)
      expect(cols * rows).toBeGreaterThanOrEqual(n)
    }
  })
})

describe('facetSlot', () => {
  it('fills row-major, so reading order matches group order', () => {
    expect(facetSlot(0, 3)).toEqual({ fx: 0, fy: 0 })
    expect(facetSlot(2, 3)).toEqual({ fx: 2, fy: 0 })
    expect(facetSlot(3, 3)).toEqual({ fx: 0, fy: 1 })
    expect(facetSlot(5, 3)).toEqual({ fx: 2, fy: 1 })
  })
})

describe('facetBox', () => {
  // the whole point: a stretched track plot turns a straight run into a diagonal, so the CELLS must be
  // square — not the outer box
  it('makes the facet cells square, margins excluded', () => {
    const { width, height } = facetBox({ cols: 2, rows: 1, w: 400, h: 300, mx: 56, my: 46 })
    const cellW = (width - 56) / 2
    const cellH = height - 46
    expect(cellW).toBeCloseTo(cellH, 6)
  })

  it('fits inside the space it was given', () => {
    for (const n of [1, 2, 3, 5, 8]) {
      const { cols, rows } = facetGrid(n)
      const box = facetBox({ cols, rows, w: 420, h: 260, mx: 56, my: 46 })
      expect(box.width).toBeLessThanOrEqual(420)
      expect(box.height).toBeLessThanOrEqual(260)
    }
  })

  it('square: false fills the box — a curve plot has no aspect to preserve', () => {
    expect(facetBox({ cols: 3, rows: 2, w: 400, h: 220, square: false }))
      .toEqual({ width: 400, height: 220 })
  })

  it('never returns a negative or absurd size when the panel is tiny', () => {
    const box = facetBox({ cols: 4, rows: 4, w: 10, h: 10, mx: 56, my: 46 })
    expect(box.width).toBeGreaterThan(0)
    expect(box.height).toBeGreaterThan(0)
  })
})
