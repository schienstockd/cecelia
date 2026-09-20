// Pure helpers for the bidirectional-context capture envelope (`docs/todo/BIDIR_CONTEXT_PLAN.md`
// Part 2 → share-in). The Vue caller (`components/DrawSurface.vue`) collects the drawn overlay in
// its own pixel frame; these helpers normalise coords + assemble the address the Julia route
// (`POST /api/viewer/capture`) writes to `<proj>/captures/<id>/meta.json` and the MCP tool
// (`get_capture`) hands to Claude. Kept out of the SFC so the shape is unit-tested — none of it
// touches the DOM.
//
// Coord system for `geom`: NORMALISED 0..1 in the frame's OWN pixel space (0,0 = top-left,
// 1,1 = bottom-right). A capture is inherently frame-relative — the pixels travel with the
// envelope, and any downstream renderer (Claude, a preview UI, an export tool) composes them
// against the frame it was given. Normalised coords survive any resample; they read the same
// against the 2048×2048 PNG we stored and against a 512×512 thumbnail we might show later.

import type { Point } from './drawGeometry'

// ── Address ─────────────────────────────────────────────────────────────────────────────────────

// What identifies the frame the user marked. Every field except `projectUid` is optional — a UI or
// plot capture (PR #5) has no `imageUid`; a still frame has a single `t`, a slab (PR #6) has
// `[from, to]`; `extentUm` is the physical size of what's on screen when we know it. The MCP-side
// consumer reads whatever fields are present and doesn't care about the others.
export interface CaptureAddress {
  projectUid: string
  imageUid?: string
  valueName?: string
  t?: number | [number, number]
  z?: number
  extentUm?: { x: number; y: number; unit?: string | null }
  domAnchor?: string
  plotSpec?: { specId: string; params?: Record<string, unknown>; dataRefs?: unknown[] }
}

// Drop nullish/empty fields so the on-disk envelope stays tight — `undefined` and `''` mean "not
// available", NOT "explicitly cleared". Keeps a viewer capture from carrying an empty `domAnchor`
// key that only makes sense for a UI capture.
export function buildCaptureAddress(input: CaptureAddress): CaptureAddress {
  const out: CaptureAddress = { projectUid: input.projectUid }
  if (input.imageUid) out.imageUid = input.imageUid
  if (input.valueName) out.valueName = input.valueName
  if (input.t !== undefined && input.t !== null) out.t = input.t
  if (input.z !== undefined && input.z !== null) out.z = input.z
  if (input.extentUm && input.extentUm.x > 0 && input.extentUm.y > 0) out.extentUm = input.extentUm
  if (input.domAnchor) out.domAnchor = input.domAnchor
  if (input.plotSpec && input.plotSpec.specId) out.plotSpec = input.plotSpec
  return out
}

// ── Overlay geometry ────────────────────────────────────────────────────────────────────────────

// The five mark kinds the server accepts (see `_CAPTURE_OVERLAY_KINDS` in `api/src/captures_api.jl`).
// Rect / poly / stroke are what `drawGeometry.ts` produces today; circle + arrow are reserved for
// PR #5 (freeform point-out) so the schema doesn't churn when they land.
export type OverlayKind = 'rect' | 'poly' | 'stroke' | 'circle' | 'arrow'

// Palette-name slot on a mark. The 5 CVD-safe / microscopy-neutral choices are locked in
// `utils/overlayCompose.ts::ANNOTATION_PALETTE` and safelisted by the server so a stray value
// gets dropped rather than stored as arbitrary CSS. Absent ⇒ `white` (also the pre-palette
// default), matching every capture written before this field existed. `black` was added when
// the canvas Share flow started producing white-background plot composites — the earlier four
// options all disappeared or read poorly on white.
export type OverlayColor = 'magenta' | 'cyan' | 'yellow' | 'white' | 'black'

export interface OverlayMark {
  kind: OverlayKind
  geom: Record<string, unknown>
  label?: string
  color?: OverlayColor
}

// Scale a pixel point into the [0,1] frame-relative space. `w`/`h` are the CAPTURED FRAME's pixel
// dimensions (the PNG we serialise), not the DrawSurface's DOM box — they can differ (device pixel
// ratio, a canvas rendered at 2x). Clamped so a mark drawn on the very edge doesn't round to > 1
// after scaling and read as out-of-frame by a downstream renderer.
export const normalisePoint = (p: Point, w: number, h: number): [number, number] => [
  w > 0 ? Math.min(1, Math.max(0, p[0] / w)) : 0,
  h > 0 ? Math.min(1, Math.max(0, p[1] / h)) : 0,
]

// A rectangle in frame-relative coords: `{x, y, w, h}`, top-left origin.
export function rectToOverlayGeom(r: { xMin: number; yMin: number; xMax: number; yMax: number },
                                   w: number, h: number): Record<string, number> {
  const [x0, y0] = normalisePoint([r.xMin, r.yMin], w, h)
  const [x1, y1] = normalisePoint([r.xMax, r.yMax], w, h)
  return { x: x0, y: y0, w: x1 - x0, h: y1 - y0 }
}

// A polygon or a stroke in frame-relative coords: `{pts: [[x,y], …]}` — same shape for both, kind
// disambiguates. Empty input → empty pts (the caller drops the mark rather than shipping it).
export const pointsToOverlayGeom = (pts: Point[], w: number, h: number): { pts: [number, number][] } =>
  ({ pts: pts.map(p => normalisePoint(p, w, h)) })
