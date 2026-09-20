// Pure hit-testing for the canvas Share selection overlay.
//
// Two questions the overlay asks this module:
//   (a) which panels does a drag rectangle SELECT? — the panel is selected when the drag rect
//       overlaps ≥ THRESHOLD of the panel's OWN area. Fraction-of-panel (not fraction-of-rect) is
//       what matches the user's intent ("did I cover most of this plot"). Cropping a panel to 60 %
//       is worse than useless — the shot loses the legend — so a snap-to-panel semantics is what
//       we want, not literal-rect crop.
//   (b) which panel is under a single-click point? — the topmost panel whose rect contains the
//       point. `panels` is passed in whatever order the canvas draws them; the caller ensures the
//       top-most is last.

import type { PanelGeom } from '../stores/canvasPanels'

/** Union bounding box of the given rects. Returns `null` if the list is empty — a caller either
 *  degrades gracefully or skips the composite. */
export function unionBox(rects: PanelGeom[]): PanelGeom | null {
  if (rects.length === 0) return null
  let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity
  for (const r of rects) {
    if (r.x < x0) x0 = r.x
    if (r.y < y0) y0 = r.y
    if (r.x + r.w > x1) x1 = r.x + r.w
    if (r.y + r.h > y1) y1 = r.y + r.h
  }
  return { x: x0, y: y0, w: x1 - x0, h: y1 - y0 }
}

/** Overlap of two axis-aligned rects, in pixels. 0 if they don't intersect. */
export function overlapArea(a: PanelGeom, b: PanelGeom): number {
  const w = Math.max(0, Math.min(a.x + a.w, b.x + b.w) - Math.max(a.x, b.x))
  const h = Math.max(0, Math.min(a.y + a.h, b.y + b.h) - Math.max(a.y, b.y))
  return w * h
}

export interface PanelHit { id: number; geom: PanelGeom }

/** Selection threshold: the drag must cover at least this fraction of a panel's area for the
 *  panel to snap in. 0.5 keeps a corner-clip from selecting a panel by accident, while still
 *  letting a rough drag catch every panel it visibly crosses. Not user-settable — feel matters
 *  more than tunability. */
export const SELECTION_THRESHOLD = 0.5

/** Panels the drag rect selects at the given threshold (default 0.5 = half a panel's area). */
export function selectedByDrag(
  panels: PanelHit[], drag: PanelGeom, threshold = SELECTION_THRESHOLD,
): number[] {
  const ids: number[] = []
  for (const p of panels) {
    const panelArea = p.geom.w * p.geom.h
    if (panelArea <= 0) continue
    if (overlapArea(p.geom, drag) / panelArea >= threshold) ids.push(p.id)
  }
  return ids
}

/** Top-most panel containing the point (`panels` is caller-ordered; last is topmost). */
export function panelAt(panels: PanelHit[], x: number, y: number): number | null {
  for (let i = panels.length - 1; i >= 0; i--) {
    const g = panels[i].geom
    if (x >= g.x && x <= g.x + g.w && y >= g.y && y <= g.y + g.h) return panels[i].id
  }
  return null
}
