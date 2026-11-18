import { describe, it, expect } from 'vitest'
import { workspaceBox, floorHeight, hostMinHeight } from './useCanvasWorkspace'

// the visible box, and the two things that can enlarge the logical workspace inside it
const VP = { w: 1600, h: 800 }

describe('workspaceBox — zoom out widens it', () => {
  it('is the viewport at 100%', () => {
    const { base, size } = workspaceBox(VP.w, VP.h, 1)
    expect(base).toEqual({ w: 1600, h: 800 })
    expect(size).toEqual(base)
  })

  it('grows to viewport/zoom when zoomed OUT — real extra room to lay plots across', () => {
    expect(workspaceBox(VP.w, VP.h, 0.5).size).toEqual({ w: 3200, h: 1600 })
  })

  it('does NOT shrink when zoomed in — zoom-in inspects, it does not remove room', () => {
    expect(workspaceBox(VP.w, VP.h, 2).size).toEqual({ w: 1600, h: 800 })
  })

  it('never collapses to zero on an unmeasured viewport', () => {
    expect(workspaceBox(0, 0, 1).size).toEqual({ w: 1, h: 1 })
  })
})

describe('workspaceBox — content makes it TALLER', () => {
  it('extends past the viewport to hold the lowest panel, plus breathing room', () => {
    const { size } = workspaceBox(VP.w, VP.h, 1, { w: 1348, h: 812 })
    expect(size.h).toBe(828)          // 812 + 16 pad
  })

  it('leaves the height alone when the plots already fit', () => {
    expect(workspaceBox(VP.w, VP.h, 1, { w: 900, h: 400 }).size.h).toBe(800)
  })

  it('grows the HEIGHT ONLY — a wide panel does not force a horizontal scrollbar', () => {
    const { size } = workspaceBox(VP.w, VP.h, 1, { w: 4000, h: 2000 })
    expect(size.w).toBe(1600)
    expect(size.h).toBe(2016)
  })

  it('ignores an empty content box (no panels placed yet)', () => {
    expect(workspaceBox(VP.w, VP.h, 1, { w: 0, h: 0 }).size.h).toBe(800)
    expect(workspaceBox(VP.w, VP.h, 1, null).size.h).toBe(800)
  })

  it('keeps `base` free of the content — this is what makes Tile idempotent', () => {
    // Tile is handed `base`; if content leaked into it, the grid Tile produced would change the box
    // the NEXT Tile is measured against, and pressing it twice would give two layouts.
    const tall = { w: 1348, h: 3000 }
    expect(workspaceBox(VP.w, VP.h, 1, tall).base).toEqual({ w: 1600, h: 800 })
    expect(workspaceBox(VP.w, VP.h, 1).base).toEqual(workspaceBox(VP.w, VP.h, 1, tall).base)
  })

  it('composes with zoom — the grown height is in the same logical px as the base', () => {
    const { base, size } = workspaceBox(VP.w, VP.h, 0.5, { w: 3200, h: 2400 })
    expect(base.h).toBe(1600)         // logical, zoom-expanded
    expect(size.h).toBe(2416)         // content is logical too, so it just extends it
  })
})

describe('the canvas box grows with the workspace — one page scroll, nothing clipped', () => {
  it('holds the box open to the lowest panel, in screen px', () => {
    const { size } = workspaceBox(VP.w, VP.h, 1, { w: 900, h: 1200 })
    expect(hostMinHeight(size.h, 1, VP.h)).toBe(1216)
  })

  it('adds nothing while the panels fit the box', () => {
    const { size } = workspaceBox(VP.w, VP.h, 1, { w: 900, h: 400 })
    expect(hostMinHeight(size.h, 1, VP.h)).toBe(0)
  })

  it('scales with zoom — zoomed out, a tall grid still fits in the floor; zoomed in, it grows', () => {
    const out = workspaceBox(VP.w, VP.h, 0.5, { w: 900, h: 1400 })
    expect(hostMinHeight(out.size.h, 0.5, VP.h)).toBe(0)           // 1416 logical × 0.5 < 800
    const inn = workspaceBox(VP.w, VP.h, 2, { w: 900, h: 400 })
    expect(hostMinHeight(inn.size.h, 2, VP.h)).toBe(1600)          // 800 logical × 2
  })

  it('keeps the floor measured before growth — the grown box never becomes the base', () => {
    // grown: the box measures exactly the min-height we put on it → the floor stays
    expect(floorHeight(1216, 1216, 800)).toBe(800)
    // the layout is taller than our min-height → that is the real floor
    expect(floorHeight(1300, 1216, 800)).toBe(1300)
    // not grown → whatever the box measures
    expect(floorHeight(760, 0, 800)).toBe(760)
    // first measure while held open (no floor yet) → take it
    expect(floorHeight(1216, 1216, 0)).toBe(1216)
  })

  it('does not ratchet — shrinking the content lets the box come back to its floor', () => {
    let floor = 800
    const tall = workspaceBox(VP.w, floor, 1, { w: 900, h: 1500 })
    const held = hostMinHeight(tall.size.h, 1, floor)
    floor = floorHeight(held, held, floor)                          // box measured at the min-height
    const short = workspaceBox(VP.w, floor, 1, { w: 900, h: 300 })
    expect(short.size.h).toBe(800)
    expect(hostMinHeight(short.size.h, 1, floor)).toBe(0)
  })
})
