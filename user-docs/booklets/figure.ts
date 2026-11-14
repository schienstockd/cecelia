// Where a booklet figure crops and where its pointer goes, from a capture's recorded boxes (CSS px).
// Pure, so utils/booklets.test.ts pins it; scripts/booklets/build.mjs draws the result.
//
// One print scale for every crop (0.19 mm per CSS px), so the app's text prints the same size on every
// page (screenshots plan → *Booklet direction*); only a crop wider than the step column is scaled down.

import { placeBox, type Side } from '../../frontend/src/utils/anchorPosition.ts'

export interface Box { x: number; y: number; width: number; height: number }

/** What capture.mjs writes next to each PNG. */
export interface ShotMeta {
  id: string
  viewport: { width: number; height: number }
  boxes: Record<string, Box>
  point: { anchor: string; label?: string; side?: Side } | null
  crop: string[]
}

export const MM_PER_PX = 0.19
export const COLUMN_MM = 173
/** Below this share of the print scale the app's text is too small to read (review guideline 8):
 *  the crop is too wide — crop tighter, not smaller. */
export const MIN_SCALE_SHARE = 0.8
const PAD = 8
const GAP = 34                       // between the pointer's ring and its label

export interface Figure {
  /** Crop origin and size in CSS px of the screenshot. */
  x0: number; y0: number; w: number; h: number
  /** mm per CSS px in print. */
  k: number
  /** False when the crop had to shrink below MIN_SCALE_SHARE of the print scale to fit the column. */
  legible: boolean
  /** Ring target and label, relative to the crop; null without a pointer. */
  target: Box | null
  label: Box | null
  /** Arrow from the label's edge to the ring's edge, relative to the crop. */
  arrow: { x1: number; y1: number; x2: number; y2: number } | null
}

/** The label's box: on `side` of the target (default right), else the next of right, left, below,
 *  above that fits in the viewport. Placed by the app's own positioner (the one guide bubbles use), so
 *  a pointer here and a guide bubble in the app sit the same way. A shot picks `side` to keep the
 *  label inside the panel it shows. */
export function labelBox(t: Box, label: string, vp: { width: number; height: number }, side: Side = 'right'): Box {
  const box = { width: Math.min(320, 7.4 * label.length + 22), height: 28 }
  const p = placeBox({
    anchor: { top: t.y, left: t.x, width: t.width, height: t.height }, box, viewport: vp,
    placement: side, gap: GAP, fallbacks: (['right', 'left', 'bottom', 'top'] as Side[]).filter(s => s !== side),
  })
  return { x: p.left, y: p.top, ...box }
}

export function figureGeometry(m: ShotMeta): Figure {
  const boxes = m.crop.map(a => m.boxes[a]).filter((b): b is Box => !!b)
  const t = m.point ? m.boxes[m.point.anchor] ?? null : null
  const l = t && m.point?.label ? labelBox(t, m.point.label, m.viewport, m.point.side) : null
  const all = [...boxes, ...(t ? [t] : []), ...(l ? [l] : [])]
  if (all.length === 0) throw new Error(`shot ${m.id}: no recorded box to crop to`)
  const left = Math.max(0, Math.min(...all.map(b => b.x)) - PAD)
  const top = Math.max(0, Math.min(...all.map(b => b.y)) - PAD)
  const right = Math.min(m.viewport.width, Math.max(...all.map(b => b.x + b.width)) + PAD)
  const bottom = Math.min(m.viewport.height, Math.max(...all.map(b => b.y + b.height)) + PAD)
  const w = right - left, h = bottom - top
  const rel = (b: Box): Box => ({ x: b.x - left, y: b.y - top, width: b.width, height: b.height })
  const target = t ? rel(t) : null, label = l ? rel(l) : null
  let arrow: Figure['arrow'] = null
  if (target && label) {
    const lc = { x: label.x + label.width / 2, y: label.y + label.height / 2 }
    const tc = { x: target.x + target.width / 2, y: target.y + target.height / 2 }
    const horiz = Math.abs(lc.x - tc.x) / (target.width + label.width) > Math.abs(lc.y - tc.y) / (target.height + label.height)
    arrow = horiz
      ? { x1: lc.x < tc.x ? label.x + label.width : label.x, y1: lc.y, x2: lc.x < tc.x ? target.x - 5 : target.x + target.width + 5, y2: tc.y }
      : { x1: lc.x, y1: lc.y < tc.y ? label.y + label.height : label.y, x2: tc.x, y2: lc.y < tc.y ? target.y - 5 : target.y + target.height + 5 }
  }
  const k = Math.min(MM_PER_PX, COLUMN_MM / w)
  return { x0: left, y0: top, w, h, k, legible: k >= MM_PER_PX * MIN_SCALE_SHARE, target, label, arrow }
}
