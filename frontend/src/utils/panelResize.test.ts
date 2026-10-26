import { describe, it, expect } from 'vitest'
import { resizeRect, squareRect } from './panelResize'
import { panelBounds } from './panelBounds'

const VW = 1200, VH = 800, HEADER = 40
const B = panelBounds(VW, VH, HEADER)
const OPTS = { minW: 220, minH: 140, viewportW: VW, viewportH: VH, bounds: B }
const START = { x: 400, y: 200, w: 300, h: 200 }

describe('resizeRect', () => {
  it('SE corner grows width and height, x/y untouched', () => {
    expect(resizeRect(START, 50, 30, { s: true, e: true }, OPTS))
      .toEqual({ x: 400, y: 200, w: 350, h: 230 })
  })

  it('E edge only touches width', () => {
    expect(resizeRect(START, 50, 999, { e: true }, OPTS))
      .toEqual({ x: 400, y: 200, w: 350, h: 200 })
  })

  it('S edge only touches height', () => {
    expect(resizeRect(START, 999, 30, { s: true }, OPTS))
      .toEqual({ x: 400, y: 200, w: 300, h: 230 })
  })

  it('W drag moves x AND shrinks/grows w to keep the right edge fixed', () => {
    // right edge stays at 700
    const r = resizeRect(START, 40, 0, { w: true }, OPTS)
    expect(r.x).toBe(440); expect(r.w).toBe(260); expect(r.x + r.w).toBe(700)
    const g = resizeRect(START, -50, 0, { w: true }, OPTS)
    expect(g.x).toBe(350); expect(g.w).toBe(350); expect(g.x + g.w).toBe(700)
  })

  it('N drag moves y AND shrinks/grows h to keep the bottom edge fixed', () => {
    const r = resizeRect(START, 0, 30, { n: true }, OPTS)
    expect(r.y).toBe(230); expect(r.h).toBe(170); expect(r.y + r.h).toBe(400)
  })

  it('NW corner moves both x and y, both edges anchored to the opposite side', () => {
    const r = resizeRect(START, 20, 25, { n: true, w: true }, OPTS)
    expect(r.x + r.w).toBe(700); expect(r.y + r.h).toBe(400)
    expect(r).toEqual({ x: 420, y: 225, w: 280, h: 175 })
  })

  it('W drag is capped so width never falls below minW', () => {
    const r = resizeRect(START, 999, 0, { w: true }, OPTS)
    expect(r.w).toBe(OPTS.minW)
    expect(r.x + r.w).toBe(700)   // right edge still anchored
  })

  it('N drag is capped so height never falls below minH', () => {
    const r = resizeRect(START, 0, 999, { n: true }, OPTS)
    expect(r.h).toBe(OPTS.minH)
    expect(r.y + r.h).toBe(400)
  })

  it('N drag cannot slide the top above the app header (same floor as clampPanel)', () => {
    // A big negative dy would otherwise put y above HEADER, where the panel header sits under the
    // app header and becomes ungrabbable — the bug panelBounds.minY exists to prevent.
    const r = resizeRect(START, 0, -9999, { n: true }, OPTS)
    expect(r.y).toBe(HEADER)
    expect(r.y + r.h).toBe(400)   // bottom anchor preserved, height grew accordingly
  })

  it('W drag cannot cross x=0', () => {
    const r = resizeRect(START, -9999, 0, { w: true }, OPTS)
    expect(r.x).toBe(0)
    expect(r.x + r.w).toBe(700)
  })

  it('E drag cannot exceed viewport width', () => {
    const r = resizeRect(START, 9999, 0, { e: true }, OPTS)
    expect(r.x + r.w).toBe(VW)
  })

  it('S drag cannot exceed viewport height', () => {
    const r = resizeRect(START, 0, 9999, { s: true }, OPTS)
    expect(r.y + r.h).toBe(VH)
  })

  it('E drag falls back to minW in a viewport narrower than minW', () => {
    const tiny = { ...OPTS, viewportW: 100 }
    expect(resizeRect(START, 50, 0, { e: true }, tiny).w).toBe(OPTS.minW)
  })
})

describe('squareRect', () => {
  // a :square panel: body w × w under 60px of chrome, so the box is w × (w + 60)
  const CH = 60
  const SQ = { x: 100, y: 100, w: 400, h: 460 }
  const O = { minW: 340, minH: 320, viewportW: 1200, viewportH: 900, bounds: { minX: 0, minY: 0, maxX: 1200, maxY: 900 } }
  const sq = (dx: number, dy: number, edges: Parameters<typeof resizeRect>[3]) =>
    squareRect(resizeRect(SQ, dx, dy, edges, O), SQ, edges, CH, O)

  it('S drag drives the side from the body height — and the result is already square', () => {
    // the flicker: the free rect would be 400 × 400 (body 340 × 400), which a later snap undid
    const r = sq(0, -60, { s: true })
    expect(r).toEqual({ x: 100, y: 100, w: 340, h: 400 })
    expect(r.h - CH).toBe(r.w)
  })

  it('a second identical move gives the identical rect (no oscillation)', () => {
    expect(sq(0, -60, { s: true })).toEqual(sq(0, -60, { s: true }))
  })

  it('E drag drives the side from the width', () => {
    expect(sq(50, 0, { e: true })).toEqual({ x: 100, y: 100, w: 450, h: 510 })
  })

  it('N drag keeps the bottom edge fixed', () => {
    const r = sq(0, 40, { n: true })
    expect(r.w).toBe(360); expect(r.h).toBe(420); expect(r.y + r.h).toBe(SQ.y + SQ.h)
  })

  it('W drag keeps the right edge fixed', () => {
    const r = sq(-50, 0, { w: true })
    expect(r.w).toBe(450); expect(r.x + r.w).toBe(SQ.x + SQ.w)
  })

  it('a corner follows the axis that moved further', () => {
    expect(sq(10, 80, { s: true, e: true }).w).toBe(480)    // height led
    expect(sq(-50, 5, { s: true, e: true }).w).toBe(350)    // width led
  })

  it('NW corner anchors both opposite edges', () => {
    const r = sq(-30, -10, { n: true, w: true })
    expect(r.x + r.w).toBe(SQ.x + SQ.w); expect(r.y + r.h).toBe(SQ.y + SQ.h); expect(r.h - CH).toBe(r.w)
  })

  it('shrinking stops at the min width', () => {
    expect(sq(0, -300, { s: true })).toEqual({ x: 100, y: 100, w: 340, h: 400 })
  })

  it('growing stops where the OTHER axis runs out of canvas', () => {
    // E drag wants 1000 wide, but only 900 - 100 - 60 = 740 of body height fits below y=100
    const r = sq(600, 0, { e: true })
    expect(r.w).toBe(740); expect(r.y + r.h).toBe(900)
  })
})
