import { describe, it, expect } from 'vitest'
import { panelBounds, clampPanel, maximisedRect, KEEP_VISIBLE_X, KEEP_VISIBLE_Y } from './panelBounds'

const VW = 1200, VH = 800, HEADER = 40

describe('panelBounds', () => {
  it('floors the top at the app header, not the viewport', () => {
    // THE BUG: a panel at y < headerH sits under a z-index 100 header while itself at z 60, so its
    // drag handle is unclickable and it cannot be brought back.
    expect(panelBounds(VW, VH, HEADER).minY).toBe(HEADER)
  })

  it('keeps a grabbable strip on screen on both axes', () => {
    const b = panelBounds(VW, VH, HEADER)
    expect(b.maxX).toBe(VW - KEEP_VISIBLE_X)
    expect(b.maxY).toBe(VH - KEEP_VISIBLE_Y)
    expect(b.minX).toBe(0)
  })

  // A window shorter than header+strip has no legal position. maxY must not fall BELOW minY, or the
  // clamp inverts and parks the panel above the header — the very state this prevents.
  it('never inverts in a viewport too short to hold the panel', () => {
    const b = panelBounds(VW, 50, HEADER)
    expect(b.maxY).toBeGreaterThanOrEqual(b.minY)
    expect(clampPanel(0, 0, b).y).toBe(HEADER)
  })

  it('treats a zero or negative header as no header', () => {
    expect(panelBounds(VW, VH, 0).minY).toBe(0)
    expect(panelBounds(VW, VH, -10).minY).toBe(0)
  })
})

describe('clampPanel', () => {
  const b = panelBounds(VW, VH, HEADER)

  it('pulls a panel dragged above the header back down to it', () => {
    expect(clampPanel(300, -200, b)).toEqual({ x: 300, y: HEADER })
    expect(clampPanel(300, 0, b)).toEqual({ x: 300, y: HEADER })
    expect(clampPanel(300, HEADER - 1, b).y).toBe(HEADER)
  })

  it('leaves a legal position untouched', () => {
    expect(clampPanel(300, 200, b)).toEqual({ x: 300, y: 200 })
  })

  it('holds the panel inside the other three edges', () => {
    expect(clampPanel(-50, 200, b).x).toBe(0)
    expect(clampPanel(99999, 200, b).x).toBe(VW - KEEP_VISIBLE_X)
    expect(clampPanel(300, 99999, b).y).toBe(VH - KEEP_VISIBLE_Y)
  })
})

describe('maximisedRect', () => {
  it('fills the width and everything below the header', () => {
    expect(maximisedRect(VW, VH, HEADER)).toEqual({ x: 0, y: HEADER, w: VW, h: VH - HEADER })
  })

  // maximising must not cover the app's own header — the panel would be screen-sized with the
  // controls that dismiss it hidden underneath
  it('starts at the header, and its top is a legal panel position', () => {
    const r = maximisedRect(VW, VH, HEADER)
    const b = panelBounds(VW, VH, HEADER)
    expect(r.y).toBe(b.minY)
    expect(clampPanel(r.x, r.y, b)).toEqual({ x: r.x, y: r.y })
  })

  it('does not produce a negative height in a viewport shorter than the header', () => {
    expect(maximisedRect(VW, 20, HEADER).h).toBe(0)
  })
})
