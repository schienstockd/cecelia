// Snap a dragged canvas panel into place: its edges to the edges of the other panels (side by side —
// touching — or lined up) and to the canvas's left/top edges, when within `SNAP_PX`. Per axis, the
// nearest candidate wins; nothing near → the position the pointer asked for. Canvas px (unzoomed), the
// space `useFloatingPanel` positions in. Off unless the canvas's Snap toggle is on.

import type { Rect } from './panelBounds'

/** How close (canvas px) an edge must come to another before it snaps. */
export const SNAP_PX = 12

/** The nearest of `targets` to `v` within `tol`, else `v`. */
function nearest(v: number, targets: number[], tol: number): number {
  let best = v, d = tol + 1
  for (const t of targets) { const dt = Math.abs(t - v); if (dt <= tol && dt < d) { best = t; d = dt } }
  return best
}

/** Where a panel of size `w`×`h` asked to be at (`x`, `y`) settles among `others`. */
export function snapPosition(at: Rect, others: readonly Rect[], tol = SNAP_PX): { x: number; y: number } {
  // a neighbour only pulls on an axis when the two overlap (or nearly) on the OTHER axis — two plots at
  // opposite ends of the canvas shouldn't line up by accident
  const nearY = (o: Rect) => at.y < o.y + o.h + tol && o.y < at.y + at.h + tol
  const nearX = (o: Rect) => at.x < o.x + o.w + tol && o.x < at.x + at.w + tol
  const xs = [0]   // the canvas's left edge
  for (const o of others) if (nearY(o)) xs.push(o.x + o.w, o.x - at.w, o.x, o.x + o.w - at.w)
  const ys = [0]   // the canvas's top edge
  for (const o of others) if (nearX(o)) ys.push(o.y + o.h, o.y - at.h, o.y, o.y + o.h - at.h)
  return { x: nearest(at.x, xs, tol), y: nearest(at.y, ys, tol) }
}
