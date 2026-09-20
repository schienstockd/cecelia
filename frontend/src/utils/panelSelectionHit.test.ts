import { describe, it, expect } from 'vitest'
import { overlapArea, unionBox, selectedByDrag, panelAt, SELECTION_THRESHOLD } from './panelSelectionHit'

describe('panelSelectionHit', () => {
  describe('overlapArea', () => {
    it('is zero for disjoint rects', () => {
      expect(overlapArea({ x: 0, y: 0, w: 10, h: 10 }, { x: 20, y: 0, w: 10, h: 10 })).toBe(0)
      expect(overlapArea({ x: 0, y: 0, w: 10, h: 10 }, { x: 0, y: 20, w: 10, h: 10 })).toBe(0)
    })
    it('is the product for full containment', () => {
      expect(overlapArea({ x: 0, y: 0, w: 10, h: 10 }, { x: 2, y: 2, w: 6, h: 6 })).toBe(36)
    })
    it('is a corner slice for corner overlap', () => {
      expect(overlapArea({ x: 0, y: 0, w: 10, h: 10 }, { x: 8, y: 8, w: 10, h: 10 })).toBe(4)
    })
  })

  describe('unionBox', () => {
    it('returns null on empty', () => { expect(unionBox([])).toBeNull() })
    it('bounds a single rect exactly', () => {
      expect(unionBox([{ x: 5, y: 6, w: 10, h: 12 }])).toEqual({ x: 5, y: 6, w: 10, h: 12 })
    })
    it('bounds multiple rects', () => {
      const u = unionBox([
        { x: 0, y: 0, w: 10, h: 10 },
        { x: 20, y: 5, w: 5,  h: 30 },
        { x: -5, y: 40, w: 3, h: 3 },
      ])
      expect(u).toEqual({ x: -5, y: 0, w: 30, h: 43 })
    })
  })

  describe('selectedByDrag', () => {
    const panels = [
      { id: 1, geom: { x: 0,   y: 0,   w: 100, h: 100 } },
      { id: 2, geom: { x: 150, y: 0,   w: 100, h: 100 } },
      { id: 3, geom: { x: 0,   y: 150, w: 100, h: 100 } },
    ]
    it('selects a panel when the drag covers ≥ 50% of its area', () => {
      expect(selectedByDrag(panels, { x: 0, y: 0, w: 100, h: 60 })).toEqual([1])
    })
    it('does NOT select on a 40% clip', () => {
      expect(selectedByDrag(panels, { x: 0, y: 0, w: 100, h: 40 })).toEqual([])
    })
    it('selects several panels a wide drag crosses', () => {
      expect(selectedByDrag(panels, { x: 0, y: 0, w: 260, h: 260 })).toEqual([1, 2, 3])
    })
    it('honours SELECTION_THRESHOLD default', () => { expect(SELECTION_THRESHOLD).toBe(0.5) })
    it('respects a lower threshold when passed', () => {
      // a 30 % clip does select at threshold 0.3
      expect(selectedByDrag(panels, { x: 0, y: 0, w: 100, h: 30 }, 0.3)).toEqual([1])
    })
    it('ignores zero-area panels (defensive against a mid-drag remount)', () => {
      expect(selectedByDrag([{ id: 9, geom: { x: 0, y: 0, w: 0, h: 0 } }],
        { x: 0, y: 0, w: 10, h: 10 })).toEqual([])
    })
  })

  describe('panelAt', () => {
    const panels = [
      { id: 1, geom: { x: 0,  y: 0,  w: 100, h: 100 } },
      { id: 2, geom: { x: 50, y: 50, w: 100, h: 100 } },   // overlaps 1 in one corner
    ]
    it('returns the topmost containing panel', () => { expect(panelAt(panels, 60, 60)).toBe(2) })
    it('returns null outside all panels', () => { expect(panelAt(panels, 1000, 1000)).toBeNull() })
    it('returns the first panel when only it contains the point', () => {
      expect(panelAt(panels, 10, 10)).toBe(1)
    })
  })
})
