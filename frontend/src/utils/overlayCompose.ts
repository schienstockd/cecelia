// BIDIR share-in — annotation palette + frame-with-overlay compositing.
//
// The share flow shipped in PR #1040 stored user-drawn marks as a separate vector overlay next to
// the frame PNG, and DrawSurface rendered them in a fixed accent colour. That produced two
// downstream failures the moment a user tried to POINT at things visually:
//   - Every mark was the same colour, so a chat like "red circle around cell A vs green around B"
//     had no way to disambiguate.
//   - The PNG the assistant reads through `get_capture(id)` has NO marks in it — the frontend was
//     `viewerCanvas.toDataURL()`, and DrawSurface is a separate SVG canvas mounted on top. The
//     assistant sees a bare frame + a colourless polygon.
//
// This module fixes both:
//   - `ANNOTATION_PALETTE` is a small CVD-safe / microscopy-neutral palette (magenta / cyan /
//     yellow / white). Server safelists these four names (`_CAPTURE_OVERLAY_COLORS` in
//     `api/src/captures_api.jl`); anything else is dropped rather than stored as arbitrary CSS.
//   - `composeFrameWithOverlay` composites the marks onto a copy of the frame canvas and returns
//     a data-URL PNG, so `get_capture` returns pixels-with-marks.
//
// Why this palette (locked 2026-09-19 in chat):
//   Red + green — the obvious "bad vs good" pair — is textbook deuteran/protan hostile. Blue also
//   clashes with DAPI channels a user will typically have on screen. Magenta / cyan / yellow /
//   white are distinguishable across all three common CVD types (deutan / protan / tritan) AND
//   rarely occur as fluorophore emissions, so they don't disappear into a GFP / mCherry overlay.
//   Napari's default paint tool uses a very similar palette for the same reason.

import type { OverlayColor, OverlayMark } from './captureAddress'
import { loadImg } from '../plots/export'

/** The four palette values, keyed by the name that goes on `OverlayMark.color`. */
export const ANNOTATION_PALETTE: Record<OverlayColor, string> = {
  magenta: '#ff2fb0',
  cyan:    '#00e5ff',
  yellow:  '#ffd800',
  white:   '#ffffff',
}

/** The default palette name — applied to any mark that arrived without one, either because it was
 *  drawn before this field existed or because the payload was tampered with. */
export const DEFAULT_ANNOTATION_COLOR: OverlayColor = 'white'

/** Resolve a mark's colour name to a CSS hex. Unknown / absent names fall back to the default so a
 *  round-trip from an older capture never crashes the renderer. */
export function resolveMarkColor(mark: OverlayMark): string {
  const c = mark.color
  return (c && ANNOTATION_PALETTE[c]) || ANNOTATION_PALETTE[DEFAULT_ANNOTATION_COLOR]
}

/** Iterate over the four palette entries in a stable order — used by the DrawSurface swatch. */
export const ANNOTATION_COLOR_ORDER: readonly OverlayColor[] = ['magenta', 'cyan', 'yellow', 'white']

// ── Compositing ─────────────────────────────────────────────────────────────────────────────────
// The frame canvas is the WebGPU viewer surface at its rendered pixel size; marks are stored in
// [0,1] frame-relative coords (see `captureAddress.ts` normaliser). Composite = "draw marks scaled
// to the frame's native pixel size onto a copy of the frame, return the data-URL."
//
// Line width and font scale with the frame so the marks read the same at any capture resolution
// (a 512×512 render and a 2048×2048 render both get proportional stroke weight, not a fixed 2 px
// that vanishes on the larger one).

const MARK_LINE_WIDTH = (w: number) => Math.max(2, Math.round(w / 500))
const MARK_FONT_PX    = (w: number) => Math.max(12, Math.round(w / 80))
const MARK_LABEL_HALO = 'rgba(0, 0, 0, 0.75)'
const MARK_LABEL_LIFT = 6      // pixels above the mark's anchor for the label baseline

/** Paint every mark onto `ctx` in the [0,w]×[0,h] pixel box. Pure DOM canvas — no Vue, no globals. */
export function paintOverlayOnCanvas(
  ctx: CanvasRenderingContext2D,
  marks: OverlayMark[],
  w: number, h: number,
): void {
  const lw = MARK_LINE_WIDTH(w)
  const font = `${MARK_FONT_PX(w)}px ui-monospace, monospace`
  ctx.lineJoin = 'round'
  ctx.lineCap = 'round'
  for (const m of marks) {
    const stroke = resolveMarkColor(m)
    ctx.strokeStyle = stroke
    ctx.lineWidth = lw
    const g = m.geom as Record<string, number> & { pts?: [number, number][] }
    let anchor: [number, number] = [0, 0]
    if (m.kind === 'rect') {
      const x = g.x * w, y = g.y * h, rw = g.w * w, rh = g.h * h
      ctx.strokeRect(x, y, rw, rh)
      anchor = [x, y]
    } else if (m.kind === 'poly' || m.kind === 'stroke') {
      const pts = g.pts ?? []
      if (pts.length < 2) continue
      ctx.beginPath()
      ctx.moveTo(pts[0][0] * w, pts[0][1] * h)
      for (let i = 1; i < pts.length; i++) ctx.lineTo(pts[i][0] * w, pts[i][1] * h)
      if (m.kind === 'poly') ctx.closePath()
      ctx.stroke()
      anchor = [pts[0][0] * w, pts[0][1] * h]
    }
    // Label: dark halo underneath the coloured fill so it reads on both light and dark pixels.
    if (m.label) {
      ctx.font = font
      ctx.textBaseline = 'alphabetic'
      const lx = anchor[0], ly = anchor[1] - MARK_LABEL_LIFT
      ctx.lineWidth = 3
      ctx.strokeStyle = MARK_LABEL_HALO
      ctx.strokeText(m.label, lx, ly)
      ctx.fillStyle = stroke
      ctx.fillText(m.label, lx, ly)
      ctx.lineWidth = lw     // restore for the next mark
      ctx.strokeStyle = stroke
    }
  }
}

/** Composite `marks` onto a copy of the viewer frame and return the resulting PNG data URL. Never
 *  mutates the input canvas — a separate offscreen canvas holds the composite. Async because the
 *  WebGPU presentation backbuffer is consumed by the browser compositor between frames, so
 *  `ctx.drawImage(webgpuCanvas)` reads back BLANK — we have to route the frame through
 *  `canvas.toDataURL()` (a different readback path that does return pixels — the same trick
 *  `__cceceliaViewerCapture` / `__cceceliaViewerScreenshot` use), then `Image.decode` before we can
 *  paint it back onto a 2D canvas alongside the marks. On any fallback path (zero-dim canvas, no
 *  2d context, image failed to load) we return the bare frame's `toDataURL` so a share still
 *  succeeds — the marks vanish but the frame doesn't. */
export async function composeFrameWithOverlay(
  frameCanvas: HTMLCanvasElement,
  marks: OverlayMark[],
): Promise<string> {
  const w = frameCanvas.width, h = frameCanvas.height
  const framePng = frameCanvas.toDataURL('image/png')
  if (w === 0 || h === 0 || marks.length === 0) return framePng
  const img = await loadImg(framePng)
  if (!img) return framePng
  return composeOverImage(img, w, h, marks) ?? framePng
}

/** Composite `marks` onto a copy of an already-loaded `HTMLImageElement` and return the PNG data
 *  URL. Used by CaptureViewSurface's re-annotate flow (Kiwi PR B): the frozen frame is already an
 *  `<img>` in the DOM, so we skip the WebGPU readback entirely — no toDataURL/`loadImg` round-trip
 *  needed, no async gymnastics. `w × h` is the pixel box you want the composite in (usually the
 *  image's natural size, so the marks land where a downstream `get_capture` reader expects). */
export function composeImageWithOverlay(
  frame: HTMLImageElement,
  marks: OverlayMark[],
): string | null {
  const w = frame.naturalWidth, h = frame.naturalHeight
  if (w === 0 || h === 0) return null
  return composeOverImage(frame, w, h, marks)
}

/** Shared innards: 2D offscreen canvas of size w×h, drawImage the source, paint the marks, return
 *  the PNG data URL. Any failure ⇒ `null`; callers pick their own fallback. */
function composeOverImage(
  frame: CanvasImageSource, w: number, h: number, marks: OverlayMark[],
): string | null {
  const off = document.createElement('canvas')
  off.width = w; off.height = h
  const ctx = off.getContext('2d')
  if (!ctx) return null
  ctx.drawImage(frame, 0, 0, w, h)
  if (marks.length > 0) paintOverlayOnCanvas(ctx, marks, w, h)
  return off.toDataURL('image/png')
}

// ── Multi-panel composite (canvas Share) ────────────────────────────────────────────────────────
// One PNG assembled from N panels' individual exports, tiled at their positions inside a workspace-
// relative bounding box. Same DOM-canvas approach as `composeImageWithOverlay`: an offscreen 2D
// canvas, `drawImage` each panel into place, paint any marks last, return the data URL. No new
// layout engine — the panels' own `PanelGeom {x,y,w,h}` from the canvas store IS the layout, so
// what the user was looking at on-screen is what the composite reads as.
//
// Coord frame. Panel geoms are in the workspace's own CSS px (the same frame the selection overlay
// lives in). We translate to the composite by subtracting the union bbox's origin — so a panel at
// workspace (250, 380) with union origin (250, 0) lands at (0, 380) in the composite.

export interface PanelTile {
  /** PNG data URL from the panel's own exporter (`SummaryPanel.exportImage`). `null` skips the
   *  tile — the composite still shows the other panels rather than aborting the whole share. */
  pngDataUrl: string | null
  /** The panel's bbox in the canvas workspace's CSS-px frame (from `useCanvasPanelsStore.geom`). */
  geom: { x: number; y: number; w: number; h: number }
}

/** Compose selected panels' PNGs into one composite PNG. Fits every tile onto its declared bbox
 *  (the exporter's rendered pixel size can be higher — a 2× DPR PNG — but the tile position and
 *  size come from the workspace geom, so overshoot is silently downscaled by `drawImage`). Marks
 *  are painted in [0,1] frame-relative coords over the composite's OWN box, matching how
 *  `composeFrameWithOverlay` treats them.
 *
 *  Returns `null` if the input list is empty or every tile failed to load — the caller can then
 *  show an inline note rather than posting a black square. Background is white so a mix of light-
 *  theme plots reads uniform. */
export async function composePanelGrid(
  tiles: PanelTile[],
  marks: OverlayMark[] = [],
): Promise<string | null> {
  if (tiles.length === 0) return null
  // Union bbox — origin is the composite's (0,0). Translate every tile by (-x0, -y0).
  let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity
  for (const t of tiles) {
    if (t.geom.x < x0) x0 = t.geom.x
    if (t.geom.y < y0) y0 = t.geom.y
    if (t.geom.x + t.geom.w > x1) x1 = t.geom.x + t.geom.w
    if (t.geom.y + t.geom.h > y1) y1 = t.geom.y + t.geom.h
  }
  const w = Math.max(1, Math.round(x1 - x0)), h = Math.max(1, Math.round(y1 - y0))
  const off = document.createElement('canvas')
  off.width = w; off.height = h
  const ctx = off.getContext('2d')
  if (!ctx) return null
  ctx.fillStyle = '#ffffff'; ctx.fillRect(0, 0, w, h)
  // Decode all tile PNGs in parallel; failures fall through to a blank slot with a hairline border
  // (a totally missing tile in a 2×2 grid is worse than a labelled empty box). The border also
  // tells Claude "this panel exists but its render was unavailable" if the composite ever ships
  // with a stale export.
  const decoded = await Promise.all(tiles.map(t => t.pngDataUrl ? loadImg(t.pngDataUrl) : Promise.resolve(null)))
  let painted = 0
  for (let i = 0; i < tiles.length; i++) {
    const t = tiles[i]
    const dx = Math.round(t.geom.x - x0), dy = Math.round(t.geom.y - y0)
    const dw = Math.round(t.geom.w),      dh = Math.round(t.geom.h)
    const img = decoded[i]
    if (img) {
      ctx.drawImage(img, dx, dy, dw, dh)
      painted++
    } else {
      ctx.strokeStyle = 'rgba(0,0,0,0.25)'
      ctx.lineWidth = 1
      ctx.strokeRect(dx + 0.5, dy + 0.5, dw - 1, dh - 1)
    }
  }
  if (painted === 0) return null
  if (marks.length > 0) paintOverlayOnCanvas(ctx, marks, w, h)
  return off.toDataURL('image/png')
}
