// Export a contact sheet of server-rendered PNG tiles — the shape the optical-flow plots have (a grid
// of `<figure><img data-url><figcaption>name`). Two views need it identically (FlowMetricsView's metric
// planes, FlowProbabilityView's input + probability map) and `ImageStripView` is a third of the same
// shape, so this is the one place the layout and the serialisation live.
//
// **Why not `elementToImageURL` like the other plots.** That serialises the DOM into a `foreignObject`
// at the element's CSS size, which is right for a vector-ish Observable Plot and wrong here: the tiles
// are 512 px (up to 768) crops displayed in a ~180 px grid cell, so capturing the DOM would hand back a
// 3-4x downsample of data that is already on the client at full resolution. These export at the tiles'
// NATIVE size, so a plane can go into a figure.
//
// Pure layout + pure SVG string building live here and are unit-tested; only `imageGridPng` touches a
// canvas, and it is a thin loop over the layout this file already computed.
import { loadImg, svgDoc, svgImage, svgText } from './export'

export interface GridImage {
  name: string
  dataUrl: string
}

export interface GridStyle {
  gap?: number          // between cells, in export px
  captionH?: number     // strip under each tile for its name; 0 ⇒ no captions
  fontSize?: number
  padding?: number
  background?: string
  colour?: string       // caption colour
}

const DEFAULTS = {
  gap: 12, captionH: 22, fontSize: 14, padding: 12,
  background: '#ffffff', colour: '#111111',
} satisfies Required<GridStyle>

export interface GridCell {
  x: number
  y: number
  w: number
  h: number             // the TILE's height, caption excluded
  captionY: number      // text baseline, already inside the caption strip
}

export interface GridLayout {
  width: number
  height: number
  cells: GridCell[]
}

/**
 * Where every tile lands. Uniform cells — the tiles are crops of one region so they share a size, and a
 * ragged sheet would imply a difference that isn't there.
 *
 * `columns` is passed in rather than derived: on screen the grid is `repeat(auto-fill, …)`, so the count
 * is a function of the panel's width, and an export that silently rewrapped to its own idea of a grid
 * would not be the sheet the user was looking at. Callers read it off the DOM with `gridColumns`.
 */
export function gridLayout(n: number, columns: number, tileW: number, tileH: number,
                           style: GridStyle = {}): GridLayout {
  const s = { ...DEFAULTS, ...style }
  const cols = Math.max(1, Math.min(Math.floor(columns) || 1, Math.max(1, n)))
  const rows = Math.ceil(n / cols)
  const cellH = tileH + s.captionH
  const cells: GridCell[] = []
  for (let i = 0; i < n; i++) {
    const c = i % cols, r = Math.floor(i / cols)
    const x = s.padding + c * (tileW + s.gap)
    const y = s.padding + r * (cellH + s.gap)
    // the baseline sits ~70% down the caption strip, which optically centres a cap-height glyph
    cells.push({ x, y, w: tileW, h: tileH, captionY: y + tileH + s.captionH * 0.7 })
  }
  return {
    width: n ? s.padding * 2 + cols * tileW + (cols - 1) * s.gap : 0,
    height: n ? s.padding * 2 + rows * cellH + (rows - 1) * s.gap : 0,
    cells,
  }
}

/**
 * How many columns the live grid is actually showing — the count of children sharing the first row's
 * `offsetTop`. A CSS-derived answer (`getComputedStyle().gridTemplateColumns`) is the obvious
 * alternative and is worse: it reports the resolved track list only in some engines, and `auto-fill`
 * tracks include empty ones, so a half-full last row inflates the count.
 */
export function gridColumns(el: HTMLElement | null): number {
  const kids = el ? Array.from(el.children) as HTMLElement[] : []
  if (!kids.length) return 1
  const top = kids[0].offsetTop
  const n = kids.filter(k => k.offsetTop === top).length
  return Math.max(1, n)
}

/** The sheet as one `<svg>` document string — tiles embedded as `<image>`, captions as real text. */
export function imageGridSvg(images: readonly GridImage[], layout: GridLayout,
                             style: GridStyle = {}): string {
  const s = { ...DEFAULTS, ...style }
  if (!images.length || !layout.cells.length) return ''
  let body = ''
  images.forEach((img, i) => {
    const c = layout.cells[i]
    if (!c) return
    body += svgImage(img.dataUrl, c.x, c.y, c.w, c.h)
    if (s.captionH > 0)
      body += svgText(c.x, c.captionY, img.name, { fill: s.colour, size: s.fontSize })
  })
  return svgDoc({ width: layout.width, height: layout.height, background: s.background, body })
}

/**
 * The sheet as a PNG data URL, tiles drawn at their natural size. Returns null when nothing decoded —
 * a blank sheet is worse than no download, because it looks like the export worked.
 *
 * `columns` comes from the caller (see `gridLayout`). The tile size is the LARGEST natural size across
 * the set, so a plane rendered at a different size is letterboxed rather than stretched.
 */
export async function imageGridPng(images: readonly GridImage[], columns: number,
                                   style: GridStyle = {}): Promise<string | null> {
  const s = { ...DEFAULTS, ...style }
  const loaded = (await Promise.all(images.map(async i =>
    ({ ...i, img: await loadImg(i.dataUrl) })))).filter(x => x.img)
  if (!loaded.length) return null
  const tileW = Math.max(...loaded.map(x => x.img!.naturalWidth || 1))
  const tileH = Math.max(...loaded.map(x => x.img!.naturalHeight || 1))
  const layout = gridLayout(loaded.length, columns, tileW, tileH, s)
  const cv = document.createElement('canvas')
  cv.width = Math.round(layout.width)
  cv.height = Math.round(layout.height)
  const ctx = cv.getContext('2d')
  if (!ctx) return null
  ctx.fillStyle = s.background
  ctx.fillRect(0, 0, cv.width, cv.height)
  ctx.fillStyle = s.colour
  ctx.font = `${s.fontSize}px system-ui, sans-serif`
  ctx.textBaseline = 'alphabetic'
  loaded.forEach((x, i) => {
    const c = layout.cells[i]
    if (!c) return
    // aspect-fit inside the uniform cell, centred — matches the SVG's `preserveAspectRatio` intent
    const nw = x.img!.naturalWidth || c.w, nh = x.img!.naturalHeight || c.h
    const k = Math.min(c.w / nw, c.h / nh)
    const w = nw * k, h = nh * k
    ctx.drawImage(x.img!, c.x + (c.w - w) / 2, c.y + (c.h - h) / 2, w, h)
    if (s.captionH > 0) ctx.fillText(x.name, c.x, c.captionY)
  })
  return cv.toDataURL('image/png')
}

/** The same sheet as SVG, decoding only to learn the tiles' natural size. */
export async function imageGridSvgFrom(images: readonly GridImage[], columns: number,
                                       style: GridStyle = {}): Promise<string | null> {
  const loaded = (await Promise.all(images.map(async i =>
    ({ ...i, img: await loadImg(i.dataUrl) })))).filter(x => x.img)
  if (!loaded.length) return null
  const tileW = Math.max(...loaded.map(x => x.img!.naturalWidth || 1))
  const tileH = Math.max(...loaded.map(x => x.img!.naturalHeight || 1))
  return imageGridSvg(loaded, gridLayout(loaded.length, columns, tileW, tileH, style), style)
}
