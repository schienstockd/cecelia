// Pure helpers for the FreeformOverlay component (BIDIR PR #5 follow-up — the visual renderer
// that closes the gap left by `mark_freeform`, whose marks land in `viewer.freeformMarks` but had
// no consumer until now). Two coord modes per `docs/todo/BIDIR_CONTEXT_PLAN.md` Decision 17:
//   • target = "live_viewer"           → geom is in viewport pixels (draw as-is)
//   • target = "<captureId>" (cap-…)   → geom is in 0..1 frame-relative coords (scale by box)
// The renderer treats the current viewer box as the frame — a captureId mark is a best-effort
// pointer AT what the user shared, not a promise the frame hasn't moved. TTL (5-min default,
// enforced on the store side via setTimeout) is what keeps a stale mark from lingering.
//
// Kept out of the SFC so the coord logic is unit-testable (`freeformRender.test.ts`). No DOM.

import type { OverlayKind } from './captureAddress'

export interface Rect   { x: number; y: number; w: number; h: number }
export interface Circle { cx: number; cy: number; r: number }
export interface Arrow  { x1: number; y1: number; x2: number; y2: number }

export type FreeformTarget = 'live_viewer' | string   // "cap-…" for a captureId

export function isCaptureTarget(target: string): boolean {
  return target !== 'live_viewer' && target.startsWith('cap-')
}

// Scale one coord to box space. For captureId marks the value is 0..1, so multiply. For
// live_viewer it's already viewport-px, so return unchanged. `dim` is the box dimension
// (`boxW` or `boxH`) — a zero box (viewer not yet measured) collapses to 0 rather than throwing.
function scale(value: number, dim: number, mode: 'norm' | 'px'): number {
  if (!Number.isFinite(value)) return 0
  return mode === 'norm' ? value * dim : value
}

// ── Per-kind resolvers ─────────────────────────────────────────────────────────────────────────
// Each takes the raw `geom` from the OverlayMark (untyped — it came off the wire) and returns a
// paint-ready shape in box coords, or null when the geom doesn't parse. Kept per-kind rather than
// a switch so a bogus mark of one kind can't crash a valid mark of another; the consumer keeps
// rendering the ones that make sense.

export function resolveRect(geom: unknown, boxW: number, boxH: number,
                            mode: 'norm' | 'px'): Rect | null {
  if (!geom || typeof geom !== 'object') return null
  const g = geom as Record<string, unknown>
  const x = Number(g.x), y = Number(g.y), w = Number(g.w), h = Number(g.h)
  if (![x, y, w, h].every(Number.isFinite)) return null
  return { x: scale(x, boxW, mode), y: scale(y, boxH, mode),
           w: scale(w, boxW, mode), h: scale(h, boxH, mode) }
}

export function resolveCircle(geom: unknown, boxW: number, boxH: number,
                              mode: 'norm' | 'px'): Circle | null {
  if (!geom || typeof geom !== 'object') return null
  const g = geom as Record<string, unknown>
  const cx = Number(g.cx), cy = Number(g.cy), r = Number(g.r)
  if (![cx, cy, r].every(Number.isFinite)) return null
  // Radius scales by the SMALLER box dim so an "r = 0.1" circle looks the same on a tall or a wide
  // viewer — the intent is "10% of the field", not "10% of whichever axis, distorted".
  const rDim = Math.min(boxW, boxH)
  return { cx: scale(cx, boxW, mode), cy: scale(cy, boxH, mode),
           r:  scale(r,  rDim, mode) }
}

export function resolveArrow(geom: unknown, boxW: number, boxH: number,
                             mode: 'norm' | 'px'): Arrow | null {
  if (!geom || typeof geom !== 'object') return null
  const g = geom as Record<string, unknown>
  const x1 = Number(g.x1), y1 = Number(g.y1), x2 = Number(g.x2), y2 = Number(g.y2)
  if (![x1, y1, x2, y2].every(Number.isFinite)) return null
  return { x1: scale(x1, boxW, mode), y1: scale(y1, boxH, mode),
           x2: scale(x2, boxW, mode), y2: scale(y2, boxH, mode) }
}

export function resolvePoints(geom: unknown, boxW: number, boxH: number,
                              mode: 'norm' | 'px'): Array<[number, number]> | null {
  if (!geom || typeof geom !== 'object') return null
  const g = geom as Record<string, unknown>
  const pts = g.pts
  if (!Array.isArray(pts) || pts.length === 0) return null
  const out: Array<[number, number]> = []
  for (const p of pts) {
    if (!Array.isArray(p) || p.length < 2) continue
    const px = Number(p[0]), py = Number(p[1])
    if (!Number.isFinite(px) || !Number.isFinite(py)) continue
    out.push([scale(px, boxW, mode), scale(py, boxH, mode)])
  }
  return out.length ? out : null
}

// SVG "points" attribute joins a Point list into "x1,y1 x2,y2 …". Small enough to inline but named
// so the SFC template stays uncluttered.
export function pointsToSvgAttr(pts: Array<[number, number]>): string {
  return pts.map(p => `${p[0]},${p[1]}`).join(' ')
}

// Compute the (x, y) SVG anchor for a mark's label chip + ✕ button. We put the chip at the
// TOP-RIGHT of the mark's bounding box so it doesn't obscure the shape itself — same convention
// PointerBubble uses (label rides beside the dot, not on top of it).
export function markAnchor(kind: OverlayKind, geom: unknown, boxW: number, boxH: number,
                           mode: 'norm' | 'px'): { x: number; y: number } | null {
  if (kind === 'rect') {
    const r = resolveRect(geom, boxW, boxH, mode)
    return r ? { x: r.x + r.w, y: r.y } : null
  }
  if (kind === 'circle') {
    const c = resolveCircle(geom, boxW, boxH, mode)
    return c ? { x: c.cx + c.r, y: c.cy - c.r } : null
  }
  if (kind === 'arrow') {
    const a = resolveArrow(geom, boxW, boxH, mode)
    return a ? { x: Math.max(a.x1, a.x2), y: Math.min(a.y1, a.y2) } : null
  }
  if (kind === 'poly' || kind === 'stroke') {
    const pts = resolvePoints(geom, boxW, boxH, mode)
    if (!pts || pts.length === 0) return null
    // Top-right of the bounding box — same rule as rect.
    let xMax = -Infinity, yMin = Infinity
    for (const p of pts) { if (p[0] > xMax) xMax = p[0]; if (p[1] < yMin) yMin = p[1] }
    return { x: xMax, y: yMin }
  }
  return null
}

// Whether this mark should paint on the current viewer. Filters out cross-image marks when the
// mark carries an `imageUid` scope (live_viewer marks do; captureId marks don't carry one on the
// wire, so they always render — best-effort). Kept here rather than in the SFC so the rule is
// pinned by a test.
export function markMatchesViewer(mark: { imageUid?: string; target: string },
                                  currentImageUid: string): boolean {
  if (!mark.imageUid) return true                // no scope on the mark ⇒ render
  if (!currentImageUid) return false             // scoped mark, no image open ⇒ hide
  return mark.imageUid === currentImageUid
}

export function coordMode(target: string): 'norm' | 'px' {
  return isCaptureTarget(target) ? 'norm' : 'px'
}

// Overlays with unknown `kind`s came off the wire — the server allow-lists these already, but the
// renderer treats an unrecognised kind as a no-op rather than throwing.
export const KNOWN_KINDS: ReadonlySet<OverlayKind> =
  new Set<OverlayKind>(['rect', 'poly', 'stroke', 'circle', 'arrow'])

export function isKnownKind(kind: string): kind is OverlayKind {
  return KNOWN_KINDS.has(kind as OverlayKind)
}

// Convenience for the SFC: turn one mark's overlay entries into a paintable list, dropping any
// that don't resolve. Preserves order (bottom → top the way SVG paints). Accepts loosely-typed
// entries (`kind: string` off the wire); unknown kinds are filtered here so the caller doesn't
// have to pre-narrow.
export interface Paintable {
  kind: OverlayKind
  label?: string
  shape: Rect | Circle | Arrow | { pts: Array<[number, number]> }
  anchor: { x: number; y: number }
}
export function paintableFor(overlay: ReadonlyArray<{ kind: string; geom: unknown; label?: string }>,
                             boxW: number, boxH: number, mode: 'norm' | 'px'): Paintable[] {
  const out: Paintable[] = []
  for (const m of overlay) {
    if (!isKnownKind(m.kind)) continue
    const kind = m.kind as OverlayKind
    let shape: Paintable['shape'] | null = null
    if (kind === 'rect')   shape = resolveRect(m.geom, boxW, boxH, mode)
    else if (kind === 'circle') shape = resolveCircle(m.geom, boxW, boxH, mode)
    else if (kind === 'arrow')  shape = resolveArrow(m.geom, boxW, boxH, mode)
    else {
      const pts = resolvePoints(m.geom, boxW, boxH, mode)
      if (pts) shape = { pts }
    }
    if (!shape) continue
    const anchor = markAnchor(kind, m.geom, boxW, boxH, mode)
    if (!anchor) continue
    out.push({ kind, label: m.label, shape, anchor })
  }
  return out
}
