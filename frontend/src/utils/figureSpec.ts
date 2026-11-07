// A figure spec from what the viewer shows — "Save as figure" (docs/todo/FIGURES_PLAN.md, P2).
// The spec is rendered server-side (`POST /api/figures/render` → `Cecelia.render_figure`), so this only
// describes WHAT to show: the crop the camera frames, the z window, the time point, and the visible
// channels in their colours at their on-screen contrast. Style, pixel size and interval come from the
// house style and the image.
import { colourForRender } from './viewerColormap'
import { niceScaleBar } from './stillOverlay'
import type { ViewerViewState } from './viewer/viewState'

export type FigureLayout = 'strip' | 'grid'
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
  /** `grid` wraps a long series into a near-square matrix (render_figure.py `grid_columns`) */
  layout?: FigureLayout
  style?: { tileMm?: number }
  /** The viewer snapshot it was saved from — what "open at source" restores (the renderer ignores it). */
  view?: ViewerViewState
}

/** Why a view can't become a figure, or the spec. Figures are 2D: a 3D view has no single crop. */
export function figureSpecFromView(vs: ViewerViewState | null | undefined, channelNames: string[],
                                   opts: { valueName?: string; scaleBarUm?: number; frames?: number[];
                                           split?: boolean; grid?: boolean } = {}):
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
    frames: opts.frames?.length ? opts.frames : [t], rows: splitRows(channels, !!opts.split), display: { ranges },
    view: vs,
  }
  if (opts.grid) spec.layout = 'grid'
  if (opts.valueName) spec.valueName = opts.valueName
  if (opts.scaleBarUm) spec.scaleBarUm = opts.scaleBarUm
  return { spec }
}

/** How many frames an AUTO interval aims for — one page-width strip of 20 mm tiles. */
export const FIGURE_AUTO_FRAMES = 9

/** The frames a figure takes from the range `lo..hi`: every `every`th (null = auto — the smallest
 *  step giving at most FIGURE_AUTO_FRAMES), always including `lo`. Returns the step it used, so the
 *  control can show what auto picked. */
export function figureFrames(lo: number, hi: number, every: number | null | undefined):
  { frames: number[]; every: number; auto: boolean } {
  const a = Math.max(0, Math.round(Math.min(lo, hi))), b = Math.max(a, Math.round(Math.max(lo, hi)))
  const auto = !every || every < 1
  const step = auto ? Math.max(1, Math.ceil((b - a) / (FIGURE_AUTO_FRAMES - 1))) : Math.round(every as number)
  const frames: number[] = []
  for (let t = a; t <= b; t += step) frames.push(t)
  return { frames, every: step, auto }
}

/** The figure's file name: `<image>_t64` for a still, `<image>_t50-110` for a strip, then the viewer's
 *  `name` field (`_<suffix>`) like a movie's — the server sanitises it. */
export function figureName(image: string, frames: number[], suffix = ''): string {
  const t = frames.length > 1 ? `t${frames[0]}-${frames[frames.length - 1]}` : `t${frames[0] ?? 0}`
  const tail = suffix.trim()
  return `${image}_${t}${tail ? `_${tail}` : ''}`
}

/** The Save-as-figure button's tooltip: which frames it will take, so "auto" says what it picked. */
export function figureTipText(r: { frames: number[]; every: number; auto: boolean } | null): string {
  if (!r || r.frames.length <= 1) return `Save this view as a figure${r ? ` (t${r.frames[0]})` : ''} → Movies`
  const last = r.frames[r.frames.length - 1]
  return `Save t${r.frames[0]}–${last}, every ${r.every}${r.auto ? ' (auto)' : ''}: ${r.frames.length} frames → Movies`
}

/** A round scale bar about a fifth of the crop — the movies' `niceScaleBar` ladder. */
export function niceFigureBarUm(sizePx: number, pixelUm: number | null | undefined): number | undefined {
  if (!pixelUm || pixelUm <= 0) return undefined
  return niceScaleBar(sizePx * pixelUm, 'µm', 0.2)?.um
}

/** A saved frame list as the range + interval controls: first, last, and the step when the list is
 *  exactly `figureFrames(first, last, step)` (`exact`); a hand-picked list is not. `every` is null when
 *  the step is what auto would pick, so the control shows "auto". */
export function framesAsRange(frames: number[]): { lo: number; hi: number; every: number | null; exact: boolean } {
  const f = [...frames].sort((a, b) => a - b)
  const lo = f[0] ?? 0, hi = f[f.length - 1] ?? lo
  const step = f.length > 1 ? f[1] - f[0] : 1
  const exact = f.every((t, i) => t === lo + i * step) && figureFrames(lo, hi, step).frames.length === f.length
  const auto = figureFrames(lo, hi, null)
  return { lo, hi, exact, every: auto.every === step && auto.frames.length === f.length ? null : step }
}

type Row = { channels: FigureChannel[] }

/** The rows for the merge's channels: the merge alone, or — split — the merge then one row per channel
 *  in grey (the paper convention: a channel alone reads by brightness, not hue). */
export function splitRows(merge: FigureChannel[], split: boolean): Row[] {
  const rows: Row[] = [{ channels: merge.map(c => ({ ...c })) }]
  if (split && merge.length > 1) {
    for (const c of merge) rows.push({ channels: [{ ...c, colour: '#ffffff' }] })
  }
  return rows
}

/** Is this a split figure — the merge followed by one single-channel row per merged channel? */
export function isSplit(rows: Row[] | undefined): boolean {
  if (!rows || rows.length < 2) return false
  const merged = rows[0].channels.map(c => String(c.c))
  const singles = rows.slice(1).map(r => r.channels.length === 1 ? String(r.channels[0].c) : '')
  return singles.length === merged.length && singles.every((c, i) => c === merged[i])
}

export interface FigureEdits {
  split?: boolean                        // merge + one grey row per channel
  grid?: boolean                         // frames wrapped into a near-square matrix
  frames?: number[]
  names?: Record<string, string>         // channel (as the spec names it) → legend name; '' = the image's
  tileMm?: number
  scaleBarUm?: number | null             // null = no bar
  interpolation?: 'pixels' | 'smooth'
}

/** The spec with the editor's changes — a new object, the stored one untouched. */
export function applyFigureEdits<T extends Record<string, unknown>>(spec: T, e: FigureEdits): T {
  // A spec is JSON by definition; a JSON copy also takes the editor's Vue reactive proxy, which
  // structuredClone refuses (DataCloneError).
  const s: Record<string, unknown> = JSON.parse(JSON.stringify(spec))
  if (e.frames) s.frames = e.frames
  if (e.split !== undefined) {
    const rows = (s.rows ?? []) as Row[]
    if (rows.length) s.rows = splitRows(rows[0].channels, e.split)
  }
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
  if (e.grid !== undefined) { if (e.grid) s.layout = 'grid'; else delete s.layout }
  return s as T
}

/** The view a figure was made from, for "open in viewer": the viewer snapshot it was saved with
 *  (`spec.view`), else rebuilt from the spec — crop centre and size as the camera, its first frame and
 *  z window, its channels at its display ranges. A figure Claude made has no snapshot. */
export function figureViewState(spec: Record<string, unknown> | null | undefined): ViewerViewState | null {
  if (!spec) return null
  const view = spec.view as ViewerViewState | undefined
  if (view?.camera && view?.dims) return view
  const center = spec.center as [number, number] | undefined
  const sizePx = Number(spec.sizePx)
  if (!center || !(sizePx > 0)) return null
  const z = Math.round(Number(spec.z ?? 0)), zh = Math.round(Number(spec.zHalf ?? 0))
  const t = ((spec.frames as number[] | undefined) ?? [Number(spec.t ?? 0)])[0] ?? 0
  const CANVAS = 720
  const ranges = ((spec.display as { ranges?: Record<string, [number, number]> } | undefined)?.ranges) ?? {}
  const layers: ViewerViewState['layers'] = {}
  for (const row of (spec.rows as { channels: { c: string | number; colour?: string }[] }[] | undefined) ?? []) {
    for (const ch of row.channels) {
      const name = String(ch.c)
      if (layers[name]) continue
      const r = ranges[name]
      layers[name] = { visible: true, colormap: ch.colour ?? null,
                       contrast_limits: r ? [r[0], r[1]] : [0, 0] }
    }
  }
  return {
    camera: { center: [z, center[0], center[1]], zoom: CANVAS / sizePx, angles: [0, 0, 0], perspective: 0 },
    dims: { ndisplay: 2, current_step: [t, z, 0, 0], point: [t, z, 0, 0], zRange: [Math.max(0, z - zh), z + zh] },
    layers, canvas: { width: CANVAS, height: CANVAS },
  }
}
