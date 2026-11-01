// The house figure style and its layout, for the style editor's diagram (docs/todo/FIGURES_PLAN.md,
// Decision B). The style file is the renderer's own (`frontend/src/lib/figureStyle.json`, read by
// `render_figure.py`); `tileMm` and the band heights here MIRROR `render_figure.tile_mm` / `_compose`
// so the diagram is the figure's proportions — `figureStyle.test.ts` pins the same tile widths the
// Python test does.
import STYLE_FILE from '../lib/figureStyle.json'
import { elapsedLabel } from './stillOverlay'

export interface FigureStyle {
  dpi: number; tileMm: number | null; figureWidthMm: number; tileMmMin: number; tileMmMax: number
  gapMm: number; fontPt: number; scaleFontPt: number; barMm: number
  scaleBarOn: 'first' | 'last' | 'all'; timeFormat: 'min' | 'clock'
}
export type FigureStyleKey = keyof FigureStyle
export const HOUSE_STYLE = STYLE_FILE.defaults as FigureStyle
export const STYLE_LIMITS = STYLE_FILE.limits as unknown as Record<string, [number, number]>
export const STYLE_CHOICES = STYLE_FILE.choices as Record<string, string[]>

const PT_MM = 25.4 / 72

/** The house style with a project's overrides on top. */
export const effectiveStyle = (over: Partial<FigureStyle> | null | undefined): FigureStyle =>
  ({ ...HOUSE_STYLE, ...(over ?? {}) })

/** Only the keys that differ from the house style — what the project stores. */
export function styleOverrides(s: FigureStyle): Partial<FigureStyle> {
  const out: Partial<FigureStyle> = {}
  for (const k of Object.keys(HOUSE_STYLE) as FigureStyleKey[]) {
    if (s[k] !== HOUSE_STYLE[k]) (out as Record<string, unknown>)[k] = s[k]
  }
  return out
}

/** A tile's width in mm: a fixed `tileMm`, else the page width shared by `nCols`, clamped. */
export function tileMm(s: FigureStyle, nCols: number): number {
  if (s.tileMm) return s.tileMm
  return Math.max(s.tileMmMin, Math.min(s.tileMmMax, s.figureWidthMm / Math.max(1, nCols)))
}

export interface FigureLayout {
  widthMm: number; heightMm: number; tile: number; gap: number; head: number; band: number
  font: number; scaleFont: number; bar: number
  tiles: { x: number; y: number; bar: boolean }[]
  titles: { x: number; y: number }[]
  bands: { y: number }[]
}

/** The figure's geometry in mm — the renderer's `_compose`: a title row (one line of text plus a third
 *  of a line above and below), then per row its tiles and a legend band of the same height. */
export function figureLayout(s: FigureStyle, nCols: number, nRows = 1): FigureLayout {
  const tile = tileMm(s, nCols), gap = s.gapMm
  const font = s.fontPt * PT_MM, line = font, pad = line / 3
  const head = line + 2 * pad, band = head
  const tiles: FigureLayout['tiles'] = [], bands: FigureLayout['bands'] = []
  let y = head
  for (let r = 0; r < nRows; r++) {
    for (let c = 0; c < nCols; c++) {
      const bar = s.scaleBarOn === 'all' || (s.scaleBarOn === 'first' && r === 0 && c === 0) ||
                  (s.scaleBarOn === 'last' && r === nRows - 1 && c === nCols - 1)
      tiles.push({ x: c * (tile + gap), y, bar })
    }
    y += tile
    bands.push({ y })
    y += band + (r < nRows - 1 ? gap : 0)
  }
  const widthMm = nCols * tile + (nCols - 1) * gap
  return { widthMm, heightMm: y, tile, gap, head, band, font, scaleFont: s.scaleFontPt * PT_MM, bar: s.barMm,
           tiles, titles: Array.from({ length: nCols }, (_, c) => ({ x: c * (tile + gap) + tile / 2, y: pad + line * 0.8 })),
           bands }
}

/** A column title in the chosen format, for the diagram — `render_figure.elapsed_label`'s two forms. */
export const sampleTimeLabel = (fmt: FigureStyle['timeFormat'], minutes: number): string =>
  fmt === 'clock'
    ? elapsedLabel(minutes, 1, 'min', 'clock')
    : minutes >= 120 ? `${Math.round(minutes / 6) / 10} h` : `${Math.round(minutes * 10) / 10} min`
