// The task preview's region as a box the user owns — pure geometry for moving, resizing and drawing
// it, in L0 pixels. The viewer seeds the box from the first preview (the view, capped — see
// `visibleRegion.ts`), then the box stays put: panning doesn't move it, and a parameter change
// re-runs the same box, so settings are compared on the same cells.
//
// The drag gesture itself is `drawGeometry.ts`'s rectangle state machine in canvas px; the viewer
// converts its corners to L0 (`screenToL0`) and hands them here.

import type { L0Rect } from '../viewerPick'
import { PREVIEW_REGION_MAX_SIDE } from './visibleRegion'
import { resizeRect } from '../panelResize'

/** Which part of the box a drag grabbed: an edge or corner resizes, `move` translates. */
export type BoxHandle = 'n' | 's' | 'e' | 'w' | 'ne' | 'nw' | 'se' | 'sw' | 'move'

export const BOX_HANDLES: Exclude<BoxHandle, 'move'>[] = ['n', 's', 'e', 'w', 'ne', 'nw', 'se', 'sw']

/** Smallest side a resize may leave, L0 px — below this the model sees no whole cell. */
export const PREVIEW_BOX_MIN_SIDE = 16

interface Limits { imageW: number; imageH: number; maxSide?: number }

/** Integer bounds inside the image, each side within [min, max]. Shrinks from the end that moved
 *  (`anchorX`/`anchorY` say which end stays): a resize past the budget stops at it rather than
 *  jumping the box elsewhere. */
function fit(r: L0Rect, { imageW, imageH, maxSide = PREVIEW_REGION_MAX_SIDE }: Limits,
             anchorX: 'lo' | 'hi', anchorY: 'lo' | 'hi'): L0Rect {
  const span = (lo: number, hi: number, len: number, anchor: 'lo' | 'hi'): [number, number] => {
    const max = Math.max(1, Math.min(maxSide, Math.floor(len)))
    const min = Math.min(PREVIEW_BOX_MIN_SIDE, max)
    let a = Math.round(Math.min(lo, hi)), b = Math.round(Math.max(lo, hi))
    a = Math.max(0, a); b = Math.min(Math.floor(len), b)
    if (b - a > max) { if (anchor === 'lo') b = a + max; else a = b - max }
    if (b - a < min) {
      if (anchor === 'lo') b = Math.min(Math.floor(len), a + min)
      else a = Math.max(0, b - min)
      if (b - a < min) { if (a === 0) b = min; else a = b - min }
    }
    return [a, b]
  }
  const [x0, x1] = span(r.x0, r.x1, imageW, anchorX)
  const [y0, y1] = span(r.y0, r.y1, imageH, anchorY)
  return { x0, y0, x1, y1 }
}

/** Translate by (dx, dy) L0 px, sliding along an image edge rather than leaving it. */
export function moveBox(b: L0Rect, dx: number, dy: number, lim: Limits): L0Rect {
  const w = b.x1 - b.x0, h = b.y1 - b.y0
  const x0 = Math.round(Math.max(0, Math.min(lim.imageW - w, b.x0 + dx)))
  const y0 = Math.round(Math.max(0, Math.min(lim.imageH - h, b.y0 + dy)))
  return fit({ x0, y0, x1: x0 + w, y1: y0 + h }, lim, 'lo', 'lo')
}

/** Drag `handle` by (dx, dy) L0 px. The opposite edge stays put; the size is clamped to
 *  [PREVIEW_BOX_MIN_SIDE, budget] and the image. The edge arithmetic is the floating panels'
 *  `resizeRect` (`utils/panelResize.ts`), in L0 px with the image as the viewport; the budget is the
 *  one thing it doesn't know, applied after from the fixed edge. */
export function resizeBox(b: L0Rect, handle: BoxHandle, dx: number, dy: number, lim: Limits): L0Rect {
  if (handle === 'move') return moveBox(b, dx, dy, lim)
  const edges = { n: handle.includes('n'), s: handle.includes('s'), e: handle.includes('e'), w: handle.includes('w') }
  const r = resizeRect({ x: b.x0, y: b.y0, w: b.x1 - b.x0, h: b.y1 - b.y0 }, dx, dy, edges, {
    minW: PREVIEW_BOX_MIN_SIDE, minH: PREVIEW_BOX_MIN_SIDE,
    viewportW: lim.imageW, viewportH: lim.imageH,
    bounds: { minX: 0, minY: 0, maxX: lim.imageW, maxY: lim.imageH },
  })
  return fit({ x0: r.x, y0: r.y, x1: r.x + r.w, y1: r.y + r.h }, lim,
             edges.w ? 'hi' : 'lo', edges.n ? 'hi' : 'lo')
}

/** A freshly drawn box from its drag corners (start, end), clamped to the image and the budget —
 *  the start corner stays where the user pressed. */
export function drawnBox(start: [number, number], end: [number, number], lim: Limits): L0Rect {
  return fit({ x0: start[0], y0: start[1], x1: end[0], y1: end[1] }, lim,
             end[0] >= start[0] ? 'lo' : 'hi', end[1] >= start[1] ? 'lo' : 'hi')
}
