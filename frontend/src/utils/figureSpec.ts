// A figure spec from what the viewer shows — "Save as figure" (docs/todo/FIGURES_PLAN.md, P2).
// The spec is rendered server-side (`POST /api/figures/render` → `Cecelia.render_figure`), so this only
// describes WHAT to show: the crop the camera frames, the z window, the time point, and the visible
// channels in their colours at their on-screen contrast. Style, pixel size and interval come from the
// house style and the image.
import { colourForRender } from './viewerColormap'
import { niceScaleBar } from './stillOverlay'
import type { ViewerViewState } from './viewer/viewState'

export interface FigureChannel { c: string; colour: string; name?: string }
export interface FigureSpec {
  kind: 'sequence'
  valueName?: string
  center: [number, number]
  sizePx: number
  z: number
  zHalf: number
  frames: number[]
  rows: { channels: FigureChannel[] }[]
  display: { ranges: Record<string, [number, number]> }
  scaleBarUm?: number
  interpolation?: 'pixels' | 'smooth'
  style?: { tileMm?: number }
}

/** Why a view can't become a figure, or the spec. Figures are 2D: a 3D view has no single crop. */
export function figureSpecFromView(vs: ViewerViewState | null | undefined, channelNames: string[],
                                   opts: { valueName?: string; scaleBarUm?: number } = {}):
  { spec: FigureSpec } | { error: string } {
  if (!vs || !vs.canvas?.height) return { error: 'Open the image in the viewer first' }
  if (vs.dims.ndisplay === 3) return { error: 'Figures are 2D — switch the viewer to 2D' }
  const zoom = vs.camera.zoom > 0 ? vs.camera.zoom : 1
  // The square the camera frames: the shorter side of the visible region, in level-0 pixels.
  const visH = vs.canvas.height / zoom
  const visW = (vs.canvas.width || vs.canvas.height) / zoom
  const sizePx = Math.max(8, Math.round(Math.min(visH, visW)))
  const step = vs.dims.current_step ?? []
  const t = Math.max(0, Math.round(step[0] ?? 0))
  const zPlane = Math.max(0, Math.round(step[1] ?? 0))
  const [lo, hi] = vs.dims.zRange ?? [zPlane, zPlane]
  const channels: FigureChannel[] = []
  const ranges: Record<string, [number, number]> = {}
  for (const name of channelNames) {
    const l = vs.layers[name]
    if (!l || l.visible === false || typeof l.colormap !== 'string') continue
    channels.push({ c: name, colour: colourForRender(l.colormap) })
    ranges[name] = [l.contrast_limits[0], l.contrast_limits[1]]
  }
  if (!channels.length) return { error: 'No channel is visible' }
  const spec: FigureSpec = {
    kind: 'sequence',
    center: [Math.round(vs.camera.center[1]), Math.round(vs.camera.center[2])],
    sizePx, z: Math.round((lo + hi) / 2), zHalf: Math.max(0, Math.round((hi - lo) / 2)),
    frames: [t], rows: [{ channels }], display: { ranges },
  }
  if (opts.valueName) spec.valueName = opts.valueName
  if (opts.scaleBarUm) spec.scaleBarUm = opts.scaleBarUm
  return { spec }
}

/** A round scale bar about a fifth of the crop — the movies' `niceScaleBar` ladder. */
export function niceFigureBarUm(sizePx: number, pixelUm: number | null | undefined): number | undefined {
  if (!pixelUm || pixelUm <= 0) return undefined
  return niceScaleBar(sizePx * pixelUm, 'µm', 0.2)?.um
}

/** "50, 54, 57" or "50-110:10" (a range with an optional step) → sorted unique frames; an error names
 *  the token that isn't one. */
export function parseFrames(text: string): { frames: number[] } | { error: string } {
  const out = new Set<number>()
  for (const raw of text.split(/[,\s]+/).filter(Boolean)) {
    const m = /^(\d+)(?:-(\d+)(?::(\d+))?)?$/.exec(raw)
    if (!m) return { error: `"${raw}" is not a frame or a range (50-110:10)` }
    const a = Number(m[1]), b = m[2] !== undefined ? Number(m[2]) : a, step = Number(m[3] ?? 1)
    if (b < a || step < 1) return { error: `"${raw}": a range runs low-high with a step ≥ 1` }
    for (let t = a; t <= b; t += step) out.add(t)
  }
  if (!out.size) return { error: 'No frames' }
  return { frames: [...out].sort((x, y) => x - y) }
}

export const formatFrames = (frames: number[] | undefined): string => (frames ?? []).join(', ')

export interface FigureEdits {
  frames?: number[]
  names?: Record<string, string>         // channel (as the spec names it) → legend name; '' = the image's
  tileMm?: number
  scaleBarUm?: number | null             // null = no bar
  interpolation?: 'pixels' | 'smooth'
}

/** The spec with the editor's changes — a new object, the stored one untouched. */
export function applyFigureEdits<T extends Record<string, unknown>>(spec: T, e: FigureEdits): T {
  const s: Record<string, unknown> = structuredClone(spec)
  if (e.frames) s.frames = e.frames
  if (e.names) {
    const rows = (s.rows ?? []) as { channels: { c: string | number; name?: string }[] }[]
    for (const row of rows) for (const ch of row.channels) {
      const n = e.names[String(ch.c)]
      if (n === undefined) continue
      if (n.trim()) ch.name = n.trim(); else delete ch.name
    }
  }
  if (e.tileMm) s.style = { ...((s.style ?? {}) as object), tileMm: e.tileMm }
  if (e.scaleBarUm === null) delete s.scaleBarUm
  else if (e.scaleBarUm) s.scaleBarUm = e.scaleBarUm
  if (e.interpolation) s.interpolation = e.interpolation
  return s as T
}
