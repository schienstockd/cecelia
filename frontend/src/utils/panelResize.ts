// Pure resize maths for `components/FloatingPanel.vue`. Given the rect the gesture started from, the
// pointer delta since then, and which edges the user grabbed, return the new rect — respecting min
// size, viewport bounds, and the same top floor as panelBounds (so a north-drag can't slide the
// header under the app header where it becomes ungrabbable).
//
// Extracted to a util so eight-handle resize (four corners + four edges) is one function tested in
// isolation, not eight subtly-different snippets inside the SFC. The old bottom-right-only path was
// `w = clientX - offset`; adding the other seven handles needs proper anchor arithmetic (a west drag
// moves x AND changes w), which is exactly the kind of thing that goes wrong silently in a template.

import type { PanelBounds, Rect } from './panelBounds'

export type Edge = 'n' | 's' | 'e' | 'w'
export interface Edges { n?: boolean; s?: boolean; e?: boolean; w?: boolean }

export interface ResizeOpts {
  minW: number
  minH: number
  viewportW: number
  viewportH: number
  bounds: PanelBounds
}

/**
 * Compute the resized rect from a pointer gesture.
 *
 * The east/south edges are bounded by the viewport (`x + w <= viewportW`, `y + h <= viewportH`).
 * The west/north edges are bounded by `bounds.minX`/`bounds.minY` — the SAME floor the drag path uses,
 * so a north-drag can't tuck the header under the app header.
 */
export function resizeRect(start: Rect, dx: number, dy: number, edges: Edges, o: ResizeOpts): Rect {
  let { x, y, w, h } = start

  if (edges.e) {
    w = clamp(start.w + dx, o.minW, o.viewportW - start.x)
  } else if (edges.w) {
    // Anchor is the RIGHT edge (start.x + start.w). Moving x while keeping the right edge fixed means
    // w = rightEdge - x, so the two must be solved together — clamp x to a range that keeps w legal.
    const rightEdge = start.x + start.w
    const minX = o.bounds.minX
    const maxX = rightEdge - o.minW
    x = clamp(start.x + dx, minX, maxX)
    w = rightEdge - x
  }

  if (edges.s) {
    h = clamp(start.h + dy, o.minH, o.viewportH - start.y)
  } else if (edges.n) {
    const bottomEdge = start.y + start.h
    const minY = o.bounds.minY
    const maxY = bottomEdge - o.minH
    y = clamp(start.y + dy, minY, maxY)
    h = bottomEdge - y
  }

  return { x, y, w, h }
}

/**
 * Square-panel variant: turn the free rect `resizeRect` produced into one whose BODY is square
 * (`w × w`, the box `w × (w + chromeH)`), for `CanvasPanel :square`.
 *
 * The drag has to produce the square itself. Writing the free rect and letting the panel snap its
 * height afterwards means an N/S drag sets the height every pointermove and the snap restores it every
 * frame — the plot flickers between the two for as long as the drag lasts.
 *
 * The grabbed axis drives the side: an E/W edge the width, an N/S edge the body height, a corner
 * whichever moved further from `start`. Anchors stay where `resizeRect` put them (a W/N drag keeps the
 * right/bottom edge fixed). The side is clamped so both the min size and the room left in the canvas
 * hold on BOTH axes.
 */
export function squareRect(r: Rect, start: Rect, edges: Edges, chromeH: number, o: ResizeOpts): Rect {
  const ch = Math.max(0, chromeH)
  const horiz = !!(edges.e || edges.w)
  const vert = !!(edges.n || edges.s)
  const fromW = r.w
  const fromH = r.h - ch
  let side: number
  if (horiz && vert) side = Math.abs(r.w - start.w) >= Math.abs(r.h - start.h) ? fromW : fromH
  else side = vert ? fromH : fromW
  const right = r.x + r.w, bottom = r.y + r.h
  const roomW = edges.w ? right - o.bounds.minX : o.viewportW - r.x
  const roomH = (edges.n ? bottom - o.bounds.minY : o.viewportH - r.y) - ch
  side = clamp(side, Math.max(o.minW, o.minH - ch), Math.min(roomW, roomH))
  const w = side, h = side + ch
  return { x: edges.w ? right - w : r.x, y: edges.n ? bottom - h : r.y, w, h }
}

function clamp(v: number, lo: number, hi: number): number {
  // A viewport smaller than the minimum size gives hi < lo. Prefer the minimum size in that case —
  // matches what the old bottom-right code did via Math.max(minW, Math.min(...)).
  if (hi < lo) return lo
  return Math.min(Math.max(v, lo), hi)
}
