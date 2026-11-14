// Where a user-docs figure crops and where its pointer goes, from a capture's recorded boxes (CSS px).
// Pure, so utils/docShots.test.ts pins it; scripts/docshots/compose.mjs draws the result.
//
// One scale for every crop (the app's own size, 1 CSS px per CSS px), so the app's text reads the same
// size on every page; only a crop wider than the page column is scaled down.

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

/** The width a GitHub markdown page gives an image, in CSS px. */
export const COLUMN_PX = 880
/** Below this share of the app's own size its text is too small to read (review guideline 8): the crop
 *  is too wide — crop tighter, not smaller. */
export const MIN_SCALE_SHARE = 0.8
const PAD = 8
const GAP = 34                       // between the pointer's ring and its label

export interface Figure {
  /** Crop origin and size in CSS px of the screenshot. */
  x0: number; y0: number; w: number; h: number
  /** Shown size per captured CSS px: 1, or less when the crop is wider than the column. */
  k: number
  /** False when the crop had to shrink below MIN_SCALE_SHARE of the app's size to fit the column. */
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

/** Where the figure goes: its column width in CSS px (default the page's) and, for a slot of fixed
 *  shape (a What's new card), the width/height it must have: the crop is trimmed about its centre to
 *  that shape, never grown past what was captured. `pad` is the margin around the crop (default 8;
 *  0 for a picture of the image, whose margin would be the page around the canvas). */
export interface Slot { column?: number; aspect?: number; pad?: number }

export function figureGeometry(m: ShotMeta, slot: Slot = {}): Figure {
  const boxes = m.crop.map(a => m.boxes[a]).filter((b): b is Box => !!b)
  const t = m.point ? m.boxes[m.point.anchor] ?? null : null
  const l = t && m.point?.label ? labelBox(t, m.point.label, m.viewport, m.point.side) : null
  const all = [...boxes, ...(t ? [t] : []), ...(l ? [l] : [])]
  if (all.length === 0) throw new Error(`shot ${m.id}: no recorded box to crop to`)
  const pad = slot.pad ?? PAD, out = Math.max(0, pad)
  let left = Math.max(0, Math.min(...all.map(b => b.x)) - out)
  let top = Math.max(0, Math.min(...all.map(b => b.y)) - out)
  const right = Math.min(m.viewport.width, Math.max(...all.map(b => b.x + b.width)) + out)
  const bottom = Math.min(m.viewport.height, Math.max(...all.map(b => b.y + b.height)) + out)
  // a picture's edge is the canvas', on a fractional CSS px whose last row is the page's: one whole
  // px inward on every side, so no line of the page shows
  // (a negative pad insets further: chrome over the canvas' edge, the status bar)
  const inset = pad <= 0 ? 1 - pad : 0
  if (pad <= 0) { left = Math.ceil(left) + inset; top = Math.ceil(top) + inset }
  let w = (pad <= 0 ? Math.floor(right) - inset : right) - left, h = (pad <= 0 ? Math.floor(bottom) - inset : bottom) - top
  if (slot.aspect) {
    if (w / h > slot.aspect) { const nw = h * slot.aspect; left += (w - nw) / 2; w = nw }
    else { const nh = w / slot.aspect; top += (h - nh) / 2; h = nh }
  }
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
  const k = Math.min(1, (slot.column ?? COLUMN_PX) / w)
  return { x0: left, y0: top, w, h, k, legible: k >= MIN_SCALE_SHARE, target, label, arrow }
}
