import { describe, it, expect } from 'vitest'
import { placeBox, parsePlacement, arrowOffset, type AnchorRect } from './anchorPosition'

// The arithmetic `TeleportPopover.reposition()` used to do inline, plus the two extra sides
// `GuideBubble` needs. The first block pins the EXISTING popover behaviour (7 call sites depend on
// it), so extracting the maths can't quietly move a dropdown; the rest cover the new grammar.

const vp = { width: 1000, height: 800 }
const box = { width: 200, height: 100 }
// a comfortable anchor mid-screen: nothing is near an edge, so no clamping/flipping is in play
const mid: AnchorRect = { top: 300, left: 400, width: 80, height: 30 }

describe('parsePlacement', () => {
  it('splits side and alignment, defaulting a bare side to centre', () => {
    expect(parsePlacement('bottom-start')).toEqual({ side: 'bottom', align: 'start' })
    expect(parsePlacement('bottom-end')).toEqual({ side: 'bottom', align: 'end' })
    expect(parsePlacement('left')).toEqual({ side: 'left', align: 'center' })
    expect(parsePlacement('top')).toEqual({ side: 'top', align: 'center' })
  })
})

describe('placeBox — the popover behaviour it was extracted from', () => {
  it('bottom-start sits under the anchor, left edges aligned', () => {
    const p = placeBox({ anchor: mid, box, viewport: vp, placement: 'bottom-start', gap: 4 })
    expect(p).toMatchObject({ top: 334, left: 400, side: 'bottom', flipped: false })
  })

  it('bottom-end right-aligns to the anchor', () => {
    const p = placeBox({ anchor: mid, box, viewport: vp, placement: 'bottom-end', gap: 4 })
    expect(p.left).toBe(mid.left + mid.width - box.width)   // 480 - 200 = 280
  })

  it('clamps the cross axis so the box never leaves the viewport', () => {
    const nearRight: AnchorRect = { top: 300, left: 960, width: 30, height: 30 }
    const p = placeBox({ anchor: nearRight, box, viewport: vp, placement: 'bottom-start', margin: 4 })
    expect(p.left).toBe(vp.width - box.width - 4)           // 796, not 960
    const nearLeft: AnchorRect = { top: 300, left: 2, width: 30, height: 30 }
    expect(placeBox({ anchor: nearLeft, box, viewport: vp, placement: 'bottom-end' }).left).toBe(4)
  })

  it('flips above when there is no room below', () => {
    const low: AnchorRect = { top: 740, left: 400, width: 80, height: 30 }
    const p = placeBox({ anchor: low, box, viewport: vp, placement: 'bottom-start', gap: 4 })
    expect(p).toMatchObject({ side: 'top', flipped: true })
    expect(p.top).toBe(740 - 100 - 4)                       // 636 — above the anchor
  })

  it('reports the roomier side when the box fits on NEITHER', () => {
    // A box too tall for either gap gets pinned into the viewport on the main axis, so the inline
    // version and this one end up at the same coordinate — what differs is the SIDE reported. The
    // inline version always flipped ('top' for an anchor near the top of the screen, which is
    // nonsense); we keep the side with the room, so `GuideBubble` points its arrow the right way.
    const high: AnchorRect = { top: 10, left: 400, width: 80, height: 30 }
    const tall = { width: 200, height: 780 }
    const p = placeBox({ anchor: high, box: tall, viewport: vp, placement: 'bottom-start' })
    expect(p).toMatchObject({ side: 'bottom', flipped: false })
    expect(p.top).toBe(16)                                  // pinned: 800 - 780 - 4
  })

  it('never returns a negative coordinate', () => {
    const p = placeBox({ anchor: { top: 0, left: 0, width: 10, height: 10 }, box: { width: 2000, height: 2000 }, viewport: vp })
    expect(p.top).toBeGreaterThanOrEqual(0)
    expect(p.left).toBeGreaterThanOrEqual(0)
  })
})

describe('placeBox — the sides guide bubbles add', () => {
  it('right places beside the anchor, vertically centred', () => {
    const p = placeBox({ anchor: mid, box, viewport: vp, placement: 'right', gap: 8 })
    expect(p).toMatchObject({ side: 'right', flipped: false })
    expect(p.left).toBe(mid.left + mid.width + 8)           // 488
    expect(p.top).toBe(300 + 15 - 50)                       // anchor mid (315) - half box
  })

  it('left flips to right when the anchor hugs the left edge', () => {
    const nearLeft: AnchorRect = { top: 300, left: 20, width: 40, height: 30 }
    const p = placeBox({ anchor: nearLeft, box, viewport: vp, placement: 'left', gap: 8 })
    expect(p).toMatchObject({ side: 'right', flipped: true })
    expect(p.left).toBe(20 + 40 + 8)
  })

  it('top centres horizontally on the anchor', () => {
    const p = placeBox({ anchor: mid, box, viewport: vp, placement: 'top', gap: 8 })
    expect(p.side).toBe('top')
    expect(p.top).toBe(300 - 100 - 8)
    expect(p.left).toBe(400 + 40 - 100)                     // anchor mid (440) - half box
  })

  it('right-start aligns the box top with the anchor top', () => {
    const p = placeBox({ anchor: mid, box, viewport: vp, placement: 'right-start' })
    expect(p.top).toBe(mid.top)
  })

  it('right-end aligns the box bottom with the anchor bottom', () => {
    const p = placeBox({ anchor: mid, box, viewport: vp, placement: 'right-end' })
    expect(p.top).toBe(mid.top + mid.height - box.height)
  })
})

describe('arrowOffset', () => {
  it('points at the anchor centre, measured from the box corner', () => {
    const placed = placeBox({ anchor: mid, box, viewport: vp, placement: 'bottom' })
    // bottom-centred → the arrow lands mid-box
    expect(arrowOffset(placed, mid, box)).toBe(box.width / 2)
  })

  it('follows the anchor when the box was clamped away from it', () => {
    const nearRight: AnchorRect = { top: 300, left: 960, width: 30, height: 30 }
    const placed = placeBox({ anchor: nearRight, box, viewport: vp, placement: 'bottom-start' })
    // box got clamped to left=796; the anchor centre (975) is 179px into it
    expect(arrowOffset(placed, nearRight, box)).toBe(975 - 796)
  })

  it('keeps clear of the rounded corners at both ends', () => {
    const farLeft: AnchorRect = { top: 300, left: 0, width: 4, height: 30 }
    const placed = placeBox({ anchor: farLeft, box, viewport: vp, placement: 'bottom-start' })
    expect(arrowOffset(placed, farLeft, box, 10)).toBe(10)
    const farRight: AnchorRect = { top: 300, left: 996, width: 4, height: 30 }
    const placed2 = placeBox({ anchor: farRight, box, viewport: vp, placement: 'bottom-start' })
    expect(arrowOffset(placed2, farRight, box, 10)).toBe(box.width - 10)
  })
})
