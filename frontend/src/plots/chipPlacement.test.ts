import { describe, it, expect } from 'vitest'
import { placeChips, type Chip } from './chipPlacement'

const overlap = (a: Chip, b: Chip) =>
  Math.abs(a.x - b.x) < (a.w + b.w) / 2 && Math.abs(a.y - b.y) < (a.h + b.h) / 2
const inside = (c: Chip, W: number, H: number) =>
  c.x - c.w / 2 >= 0 && c.x + c.w / 2 <= W && c.y - c.h / 2 >= 0 && c.y + c.h / 2 <= H

describe('placeChips', () => {
  it('leaves a chip that already fits on its anchor', () => {
    expect(placeChips([{ x: 50, y: 50, w: 20, h: 10 }], 100, 100)).toEqual([{ x: 50, y: 50 }])
  })
  it('pulls a chip anchored on the edge fully inside the box', () => {
    const [p] = placeChips([{ x: 2, y: 99, w: 60, h: 14 }], 200, 100)
    expect(p).toEqual({ x: 30, y: 93 })
  })
  it('keeps clear of an inset band (axis title)', () => {
    const [p] = placeChips([{ x: 0, y: 50, w: 40, h: 10 }], 200, 100, { left: 16 })
    expect(p.x).toBe(36)
  })
  it('separates a cluster of chips stacked on nearly the same anchor, all inside', () => {
    // the screenshot case: four labels on one clump in the bottom-left corner
    const chips: Chip[] = [0, 1, 2, 3].map(i => ({ x: 5 + i, y: 395 - i, w: 110, h: 16 }))
    const pos = placeChips(chips, 360, 400)
    const placed = chips.map((c, i) => ({ ...c, ...pos[i] }))
    for (const c of placed) expect(inside(c, 360, 400)).toBe(true)
    for (let i = 0; i < placed.length; i++) for (let j = i + 1; j < placed.length; j++)
      expect(overlap(placed[i], placed[j])).toBe(false)
  })
  it('parts chips on identical anchors', () => {
    const c = { x: 100, y: 50, w: 40, h: 10 }
    const [a, b] = placeChips([c, c], 200, 100)
    expect(Math.abs(a.y - b.y)).toBeGreaterThanOrEqual(10)
  })
  it('centres a chip wider than the box', () => {
    expect(placeChips([{ x: 0, y: 0, w: 300, h: 10 }], 200, 100)[0]).toEqual({ x: 100, y: 5 })
  })
})
