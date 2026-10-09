import { describe, it, expect } from 'vitest'
import { moveBox, resizeBox, drawnBox, PREVIEW_BOX_MIN_SIDE } from './previewBox'
import { visibleRegion, PREVIEW_REGION_MAX_SIDE } from './visibleRegion'

const lim = { imageW: 4000, imageH: 3000 }
const box = { x0: 1000, y0: 1000, x1: 1500, y1: 1400 }

describe('moveBox', () => {
  it('translates, keeping the size', () => {
    expect(moveBox(box, 100, -50, lim)).toEqual({ x0: 1100, y0: 950, x1: 1600, y1: 1350 })
  })
  it('slides along the image edge instead of leaving it', () => {
    expect(moveBox(box, -5000, 99999, lim)).toEqual({ x0: 0, y0: 2600, x1: 500, y1: 3000 })
  })
})

describe('resizeBox', () => {
  it('an edge moves only its own side', () => {
    expect(resizeBox(box, 'e', 200, 999, lim)).toEqual({ ...box, x1: 1700 })
    expect(resizeBox(box, 'n', 999, -100, lim)).toEqual({ ...box, y0: 900 })
  })
  it('a corner moves two sides', () => {
    expect(resizeBox(box, 'sw', -100, 100, lim)).toEqual({ x0: 900, y0: 1000, x1: 1500, y1: 1500 })
  })
  it('is clamped at the region budget, from the side that moved', () => {
    const r = resizeBox(box, 'e', 5000, 0, lim)
    expect(r.x0).toBe(1000)
    expect(r.x1 - r.x0).toBe(PREVIEW_REGION_MAX_SIDE)
    const l = resizeBox(box, 'w', -5000, 0, lim)
    expect(l.x1).toBe(1500)
    expect(l.x1 - l.x0).toBe(PREVIEW_REGION_MAX_SIDE)
  })
  it('never collapses below the minimum side', () => {
    const r = resizeBox(box, 'e', -499, 0, lim)
    expect(r.x1 - r.x0).toBe(PREVIEW_BOX_MIN_SIDE)
    expect(r.x0).toBe(1000)
  })
  it('a handle dragged past the fixed edge stops at the minimum side', () => {
    expect(resizeBox(box, 'e', -700, 0, lim)).toEqual({ ...box, x1: 1000 + PREVIEW_BOX_MIN_SIDE })
  })
  it('stays inside the image', () => {
    const r = resizeBox({ x0: 3900, y0: 0, x1: 3990, y1: 50 }, 'ne', 500, -500, lim)
    expect(r).toEqual({ x0: 3900, y0: 0, x1: 4000, y1: 50 })
  })
})

describe('drawnBox', () => {
  it('normalises the corners', () => {
    expect(drawnBox([1500, 1400], [1000, 1000], lim)).toEqual(box)
  })
  it('keeps the press corner and clamps the far one at the budget', () => {
    const r = drawnBox([100, 100], [3000, 2900], lim)
    expect(r).toEqual({ x0: 100, y0: 100, x1: 100 + PREVIEW_REGION_MAX_SIDE, y1: 100 + PREVIEW_REGION_MAX_SIDE })
    const up = drawnBox([3000, 2900], [100, 100], lim)
    expect(up.x1).toBe(3000)
    expect(up.y1).toBe(2900)
  })
})

describe('visibleRegion with a box', () => {
  const view = { x0: 0, y0: 0, x1: 300, y1: 300 }
  it('the box wins over the view in 2D — panning does not move the region', () => {
    const a = visibleRegion({ view, box, imageW: 4000, imageH: 3000, currentZ: 2, currentT: 1, ndisplay: 2 })
    const b = visibleRegion({ view: { x0: 2000, y0: 2000, x1: 2300, y1: 2300 }, box,
                              imageW: 4000, imageH: 3000, currentZ: 2, currentT: 1, ndisplay: 2 })
    expect(a.xy).toEqual({ X: [1000, 1500], Y: [1000, 1400] })
    expect(b).toEqual(a)
    expect(a.capped).toBe(false)
  })
  it('3D ignores the box', () => {
    const r = visibleRegion({ view, box, imageW: 800, imageH: 600, currentZ: 2, currentT: 1, ndisplay: 3 })
    expect(r.xy).toEqual({ X: [0, 800], Y: [0, 600] })
  })
})
