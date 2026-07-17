// Shared plot export helpers — one place for "serialise an SVG to an image URL" and the download
// plumbing, used by every canvas plot (PlotChart's summary charts + the bespoke cluster HMM panels).
// Keeping this in one module avoids the divergent re-implementation the codebase warns against: a
// plot that renders its own <svg> node exports it exactly like the generic renderer does.

// Serialise a rendered <svg> to a data URL. SVG = native serialisation (crisp, editable); PNG =
// rasterise onto a 2× canvas over a white ground (so a dark-theme plot still exports on white).
export async function svgToImageURL(svg: SVGSVGElement | null, type: 'png' | 'svg'): Promise<string | null> {
  if (!svg) return null
  const xml = new XMLSerializer().serializeToString(svg)
  const svgUrl = 'data:image/svg+xml;charset=utf-8,' + encodeURIComponent(xml)
  if (type === 'svg') return svgUrl
  const w = svg.width.baseVal.value || svg.clientWidth
  const h = svg.height.baseVal.value || svg.clientHeight
  return await rasterize(svgUrl, w, h)
}

const DPR = (typeof window !== 'undefined' ? window.devicePixelRatio || 1 : 1)
// SVG plots (heatmap, HMM, summary charts) rasterise crisply at any factor since they're vector, so
// DPR×2 (capped 4×) is plenty and keeps file sizes sane.
const EXPORT_SCALE = Math.min(4, 2 * DPR)
// Raster/WebGL composites (UMAP + gate scatter) have NO vector fallback — the point cloud is a
// bitmap. ScatterGL.exportCanvas re-renders it at this scale, so push it higher for a crisp PNG.
const RASTER_SCALE = Math.min(8, 4 * DPR)

// rasterise an SVG data URL onto a hi-DPI canvas over a white ground → PNG data URL
async function rasterize(svgUrl: string, w: number, h: number): Promise<string | null> {
  const scale = EXPORT_SCALE
  return await new Promise<string | null>(resolve => {
    const img = new Image()
    img.onload = () => {
      const c = document.createElement('canvas')
      c.width = w * scale; c.height = h * scale
      const ctx = c.getContext('2d')
      if (!ctx) { resolve(null); return }
      ctx.fillStyle = 'white'; ctx.fillRect(0, 0, c.width, c.height)
      ctx.scale(scale, scale); ctx.drawImage(img, 0, 0)
      resolve(c.toDataURL('image/png'))
    }
    img.onerror = () => resolve(null)
    img.src = svgUrl
  })
}

// recursively copy COMPUTED styles from a live tree onto its clone (so a detached clone rasterises
// with the same look — scoped-CSS classes don't apply inside a serialised <foreignObject>).
function inlineComputedStyles(src: Element, dst: Element) {
  const cs = getComputedStyle(src)
  let s = ''
  for (let i = 0; i < cs.length; i++) { const k = cs[i]; s += `${k}:${cs.getPropertyValue(k)};` }
  // getComputedStyle().width/height are the CONTENT-box values. If the element is `box-sizing:border-box`
  // (e.g. the gate plot's .plot-capture + the montage cells), inlining that content width UNDER
  // border-box makes the clone TOTAL = content (padding eaten), shrinking it — and it compounds through
  // each nested padded element. The cloned HTML axis overlay then renders SMALLER than the composited
  // (live-rect) canvas → the gating-PDF "dots and axis on a different scale". Force content-box so
  // width + padding + border reconstruct the original box. (Appended last so it wins.)
  s += 'box-sizing:content-box;'
  dst.setAttribute('style', s)
  const sc = src.children, dc = dst.children
  for (let i = 0; i < sc.length; i++) inlineComputedStyles(sc[i], dc[i])
}

// Capture a whole host ELEMENT (plot <svg> + HTML overlay legend/title) to an image. Unlike
// `svgToImageURL` (svg only), this wraps a fully style-inlined clone in an SVG <foreignObject>, so the
// overlays are included. Used for the cluster HMM panels whose legend is an HTML overlay.
// `bg` = 'transparent' (or '') → no background fill (for compositing over already-drawn canvas pixels).
// `opts.blankCanvases` hides <canvas> in the clone (their pixels are composited separately — a canvas
// serialises blank inside foreignObject, and its CSS background would otherwise cover the composite).
export async function elementToImageURL(el: HTMLElement | null, type: 'png' | 'svg', bg: string,
                                        opts: { blankCanvases?: boolean } = {}): Promise<string | null> {
  if (!el) return null
  const w = el.clientWidth || el.offsetWidth, h = el.clientHeight || el.offsetHeight
  if (!w || !h) return null
  const clone = el.cloneNode(true) as HTMLElement
  inlineComputedStyles(el, clone)
  const transparent = !bg || bg === 'transparent'
  if (transparent) clone.style.background = 'transparent'
  if (opts.blankCanvases) for (const cv of Array.from(clone.querySelectorAll('canvas'))) {
    (cv as HTMLElement).style.visibility = 'hidden'
    // the canvas's ancestors' (opaque) backgrounds would paint OVER the separately-composited canvas
    // pixels in plotHostToImageURL and hide them — clear them so pass-1 shows through. Non-ancestor
    // siblings (legend, labels) keep their backgrounds.
    for (let a = cv.parentElement; a && a !== clone; a = a.parentElement) {
      (a as HTMLElement).style.background = 'transparent'; (a as HTMLElement).style.backgroundColor = 'transparent'
    }
    clone.style.background = 'transparent'; clone.style.backgroundColor = 'transparent'
  }
  clone.setAttribute('xmlns', 'http://www.w3.org/1999/xhtml')
  const xml = new XMLSerializer().serializeToString(clone)
  const svg = `<svg xmlns="http://www.w3.org/2000/svg" width="${w}" height="${h}">` +
              (transparent ? '' : `<rect width="100%" height="100%" fill="${bg}"/>`) +
              `<foreignObject x="0" y="0" width="${w}" height="${h}">${xml}</foreignObject></svg>`
  const url = 'data:image/svg+xml;charset=utf-8,' + encodeURIComponent(svg)
  return type === 'svg' ? url : await rasterize(url, w, h)
}

export function loadImg(url: string): Promise<HTMLImageElement | null> {
  return new Promise(res => { const i = new Image(); i.onload = () => res(i); i.onerror = () => res(null); i.src = url })
}

// Capture a host that contains <canvas> layers (WebGL scatter + canvas2D overlays) PLUS HTML/SVG
// overlays (legend, ticks, axis labels) — the interactive plots (UMAP, gate plot). Canvas pixels
// can't be serialised via foreignObject, so we composite in two passes onto one hi-DPI canvas:
//   1. draw every <canvas> in the host at its offset (WebGL reads back via regl-scatterplot's
//      preserveDrawingBuffer; opts.hiRes swaps in a higher-res re-render for crisp output);
//   2. draw the HTML/SVG overlay layer on top (canvases blanked so they don't cover pass 1).
// Returns a PNG data URL (raster; SVG makes no sense for a rasterised point cloud).
// `opts.hiRes(cv, scale)` lets a caller supply a HIGHER-RESOLUTION replacement for a specific canvas
// (ScatterGL.exportCanvas re-renders the WebGL point cloud at `scale`× — its on-screen backing store
// is only CSS×DPR, so drawing the live canvas would upscale and pixelate). Resolver returns null →
// composite the live canvas as-is (canvas2D overlays with no hi-res path).
export async function plotHostToImageURL(host: HTMLElement | null, bg: string,
  opts: { hiRes?: (cv: HTMLCanvasElement, scale: number) => Promise<CanvasImageSource | null>; scale?: number } = {}): Promise<string | null> {
  if (!host) return null
  const w = host.clientWidth || host.offsetWidth, h = host.clientHeight || host.offsetHeight
  if (!w || !h) return null
  // caller may force a higher scale (e.g. the flow/gate plot targets a fixed crisp resolution regardless
  // of its on-screen size); both the WebGL re-render and the vector overlay rasterise at this scale.
  const scale = opts.scale ?? RASTER_SCALE
  const c = document.createElement('canvas'); c.width = w * scale; c.height = h * scale
  const ctx = c.getContext('2d'); if (!ctx) return null
  ctx.scale(scale, scale)
  if (bg && bg !== 'transparent') { ctx.fillStyle = bg; ctx.fillRect(0, 0, w, h) }
  const hr = host.getBoundingClientRect()
  // getBoundingClientRect includes any ancestor CSS transform (the canvas ZOOM: scale()), but the
  // composite + HTML overlay are sized from clientWidth (UNtransformed). Divide rect deltas by that
  // scale so the canvas layers land in the same untransformed CSS-px space as the axis overlay — else a
  // zoomed board/module (scale ≠ 1) exports the dots/gate at a different scale than the axis ticks.
  const k = w ? hr.width / w : 1
  for (const cv of Array.from(host.querySelectorAll('canvas'))) {
    const r = cv.getBoundingClientRect()
    const hi = opts.hiRes ? await opts.hiRes(cv, scale) : null
    try { ctx.drawImage(hi ?? cv, (r.left - hr.left) / k, (r.top - hr.top) / k, r.width / k, r.height / k) } catch { /* tainted/empty */ }
  }
  const overlayUrl = await elementToImageURL(host, 'svg', 'transparent', { blankCanvases: true })
  if (overlayUrl) { const img = await loadImg(overlayUrl); if (img) ctx.drawImage(img, 0, 0, w, h) }
  return c.toDataURL('image/png')
}

// Export a WebGL/raster plot host (a regl point cloud + HTML/SVG overlays) at a CRISP FIXED
// resolution, regardless of its on-screen size. A raster plot drawn at its screen backing store
// exports soft on a small slot; instead we aim for a fixed ~`targetPx` long side (scale bounded
// 4–14×) and let the `hiRes` resolver re-render the point cloud at that scale — regl scales the point
// size with it, so the look stays constant but sharp. This is the ONE hi-res path shared by the gating
// scatter (GateScatterCell) and the cluster UMAP (UmapView); don't reinvent the scale math per plot.
export function rasterExportScale(host: HTMLElement | null, targetPx = 2200): number | undefined {
  const px = host ? Math.max(host.clientWidth, host.clientHeight) : 0
  return px ? Math.min(14, Math.max(4, Math.ceil(targetPx / px))) : undefined
}
export async function rasterPlotToImageURL(host: HTMLElement | null, bg: string,
  hiRes: (cv: HTMLCanvasElement, scale: number) => Promise<CanvasImageSource | null>,
  targetPx = 2200): Promise<string | null> {
  return plotHostToImageURL(host, bg, { hiRes, scale: rasterExportScale(host, targetPx) })
}

// find the <svg> in a rendered node (Observable Plot returns a <figure> wrapper when it has a legend)
export function svgOf(node: Element | null): SVGSVGElement | null {
  if (!node) return null
  return node.tagName.toLowerCase() === 'svg'
    ? (node as unknown as SVGSVGElement)
    : node.querySelector('svg')
}

// trigger a browser download of a data URL (image export)
export function downloadDataUrl(name: string, url: string) {
  const a = document.createElement('a'); a.href = url; a.download = name; a.click()
}

// trigger a browser download of a Blob (CSV / text export)
export function downloadBlob(name: string, blob: Blob) {
  const url = URL.createObjectURL(blob)
  const a = document.createElement('a'); a.href = url; a.download = name; a.click()
  URL.revokeObjectURL(url)
}

// tidy CSV from rows of records (header = union of keys in first-appearance order)
export function rowsToCsv(rows: Record<string, unknown>[]): string {
  if (!rows.length) return ''
  const header: string[] = []
  for (const r of rows) for (const k of Object.keys(r)) if (!header.includes(k)) header.push(k)
  const esc = (v: unknown) => { const s = v == null ? '' : String(v); return /[",\n]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s }
  return [header, ...rows.map(r => header.map(h => esc(r[h])))].map(row => row.join(',')).join('\n')
}

// ── TRUE-VECTOR SVG builders ─────────────────────────────────────────────────────────────────────
// Pure string builders for exporting the CANVAS dot plots (UMAP, gating scatter/pairs) as a real
// vector SVG — every dot a `<circle>` so the figure opens editable in Illustrator (recolour dots).
// Kept here (not per-component) so all dot plots emit identical vector output, and kept as pure string
// functions so they're unit-testable without mounting a component (docs/DEV.md → frontend test rule).
// The point cloud is grouped by colour: one `<g fill=…>` per cluster/population, so selecting a group
// in Illustrator and changing its fill recolours that whole group at once (the boss's ask).

// round to 1 dp — halves the byte count of a big point cloud and keeps Illustrator responsive; sub-px
// precision is invisible in a figure. `-0` normalises to `0`.
const r1 = (n: number) => { const v = Math.round(n * 10) / 10; return v === 0 ? 0 : v }
// XML-escape a text/attribute value (labels can contain & < > " ')
export function svgEsc(s: string): string {
  return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;').replace(/'/g, '&#39;')
}

// assemble a complete <svg> document from a body string (already-built element strings)
export function svgDoc(o: { width: number; height: number; background?: string; body: string }): string {
  const bg = o.background && o.background !== 'transparent'
    ? `<rect width="100%" height="100%" fill="${o.background}"/>` : ''
  return `<svg xmlns="http://www.w3.org/2000/svg" width="${r1(o.width)}" height="${r1(o.height)}" ` +
         `viewBox="0 0 ${r1(o.width)} ${r1(o.height)}">${bg}${o.body}</svg>`
}

// one colour-bucket of points as a group of <circle>s. Group-level fill/opacity = the recolour hook:
// change the group's fill in Illustrator → the whole cluster recolours. Empty input → empty group ''.
export function svgCircles(pts: ArrayLike<[number, number]> | [number, number][],
                           o: { fill: string; opacity?: number; r?: number; label?: string }): string {
  if (!pts.length) return ''
  const rad = r1(o.r ?? 2)
  let body = ''
  for (let i = 0; i < pts.length; i++) { const p = pts[i]; body += `<circle cx="${r1(p[0])}" cy="${r1(p[1])}" r="${rad}"/>` }
  const op = o.opacity != null && o.opacity < 1 ? ` fill-opacity="${r1(o.opacity)}"` : ''
  const id = o.label ? ` data-group="${svgEsc(o.label)}"` : ''
  return `<g fill="${o.fill}"${op}${id}>${body}</g>`
}

// small vector primitives (axes, gates, contours, labels)
export function svgLine(x1: number, y1: number, x2: number, y2: number, o: { stroke: string; width?: number }): string {
  return `<line x1="${r1(x1)}" y1="${r1(y1)}" x2="${r1(x2)}" y2="${r1(y2)}" stroke="${o.stroke}" stroke-width="${r1(o.width ?? 1)}"/>`
}
export function svgPolygon(pts: [number, number][], o: { stroke?: string; fill?: string; width?: number; opacity?: number }): string {
  if (pts.length < 2) return ''
  const p = pts.map(q => `${r1(q[0])},${r1(q[1])}`).join(' ')
  const attrs = [`fill="${o.fill ?? 'none'}"`, o.stroke ? `stroke="${o.stroke}"` : '',
    o.stroke ? `stroke-width="${r1(o.width ?? 1)}"` : '', o.opacity != null && o.opacity < 1 ? `stroke-opacity="${r1(o.opacity)}"` : '']
  return `<polygon points="${p}" ${attrs.filter(Boolean).join(' ')}/>`
}
export function svgPath(d: string, o: { stroke?: string; fill?: string; width?: number; opacity?: number }): string {
  if (!d) return ''
  const attrs = [`fill="${o.fill ?? 'none'}"`, o.stroke ? `stroke="${o.stroke}"` : '',
    o.stroke ? `stroke-width="${r1(o.width ?? 1)}"` : '', o.opacity != null && o.opacity < 1 ? `stroke-opacity="${r1(o.opacity)}"` : '',
    'stroke-linejoin="round"', 'stroke-linecap="round"']
  return `<path d="${d}" ${attrs.filter(Boolean).join(' ')}/>`
}
export function svgRect(x: number, y: number, w: number, h: number,
                        o: { fill?: string; stroke?: string; width?: number; rx?: number; opacity?: number }): string {
  const attrs = [`fill="${o.fill ?? 'none'}"`, o.stroke ? `stroke="${o.stroke}"` : '',
    o.stroke ? `stroke-width="${r1(o.width ?? 1)}"` : '', o.rx ? `rx="${r1(o.rx)}"` : '',
    o.opacity != null && o.opacity < 1 ? `fill-opacity="${r1(o.opacity)}"` : '']
  return `<rect x="${r1(x)}" y="${r1(y)}" width="${r1(w)}" height="${r1(h)}" ${attrs.filter(Boolean).join(' ')}/>`
}
export function svgText(x: number, y: number, s: string,
                        o: { fill: string; size?: number; anchor?: 'start' | 'middle' | 'end'; weight?: number | string; rotate?: number } = { fill: '#111' }): string {
  const anchor = o.anchor ? ` text-anchor="${o.anchor}"` : ''
  const weight = o.weight ? ` font-weight="${o.weight}"` : ''
  const rot = o.rotate ? ` transform="rotate(${r1(o.rotate)} ${r1(x)} ${r1(y)})"` : ''
  return `<text x="${r1(x)}" y="${r1(y)}" fill="${o.fill}" font-family="system-ui, sans-serif" ` +
         `font-size="${r1(o.size ?? 11)}"${anchor}${weight}${rot}>${svgEsc(s)}</text>`
}
// embed a raster layer as an <image>. `fit='none'` (default) stretches to the rect — used for the gating
// density base, which exactly fills its plot area. `fit='meet'` aspect-fits (letterbox) — used for a
// board slot's raster fallback, matching the PDF's aspect-preserving image placement.
export function svgImage(dataUrl: string, x: number, y: number, w: number, h: number, fit: 'none' | 'meet' = 'none'): string {
  if (!dataUrl) return ''
  const par = fit === 'meet' ? 'xMidYMid meet' : 'none'
  return `<image x="${r1(x)}" y="${r1(y)}" width="${r1(w)}" height="${r1(h)}" ` +
         `preserveAspectRatio="${par}" href="${dataUrl}"/>`
}

// Nest a complete <svg> as a child positioned at (x,y,w,h) inside a parent SVG. SVG supports a nested
// <svg> carrying its own viewBox, which scales the inner content to the rect (letterboxed via
// preserveAspectRatio, matching the PDF's aspect-fit). Used to stitch each Analysis-board slot's plot
// SVG into ONE page SVG (plots/boardSvg.ts). We control the input (our svgDoc output, or Observable
// Plot's <svg>), so a light re-attribute of the root tag is safe. Default overflow (hidden) clips the
// inner content to the slot rect, so a plot can't bleed into its neighbours.
export function nestSvg(fullSvg: string, x: number, y: number, w: number, h: number): string {
  if (!fullSvg) return ''
  const m = fullSvg.match(/<svg\b([^>]*)>/i)
  if (!m) return ''
  const attrs = m[1]
  let viewBox = (attrs.match(/viewBox\s*=\s*"([^"]*)"/i) ?? [])[1] ?? ''
  if (!viewBox) {   // no viewBox → derive one from the root width/height so scaling still works
    const wa = attrs.match(/\bwidth\s*=\s*"([\d.]+)/i), ha = attrs.match(/\bheight\s*=\s*"([\d.]+)/i)
    if (wa && ha) viewBox = `0 0 ${wa[1]} ${ha[1]}`
  }
  const inner = fullSvg.slice((m.index ?? 0) + m[0].length).replace(/<\/svg>\s*$/i, '')
  const vb = viewBox ? ` viewBox="${viewBox}"` : ''
  return `<svg x="${r1(x)}" y="${r1(y)}" width="${r1(w)}" height="${r1(h)}"${vb} ` +
         `preserveAspectRatio="xMidYMid meet">${inner}</svg>`
}

// A true-vector SVG keeps EVERY point as its own element, so a big point cloud (e.g. a 100k+-cell UMAP)
// makes a heavy file that can be slow to open / edit in Illustrator. ~8 MB ≈ a few hundred thousand
// dots — past that, warn the user (non-blocking) so a "why is my export huge/slow" is never a surprise.
export const SVG_SIZE_WARN_BYTES = 8_000_000
// Returns a human warning if the generated SVG is large enough to be sluggish in Illustrator, else null.
// Uses byte size (not a point count) so it catches ANY massive element — a big UMAP, a dense gating
// overlay, a whole board — through one check.
export function svgSizeWarning(svg: string, label = 'This figure'): string | null {
  if (svg.length <= SVG_SIZE_WARN_BYTES) return null
  return `${label} is a ~${(svg.length / 1e6).toFixed(0)} MB vector SVG — a large point cloud keeps every ` +
    `dot as an editable element, so the file may be slow to open in Illustrator. For a quick view use ` +
    `PNG/PDF, or export a smaller selection.`
}

// trigger a browser download of a text string (SVG / CSV) — thin wrapper over downloadBlob
export function downloadText(name: string, text: string, mime = 'text/plain') {
  downloadBlob(name, new Blob([text], { type: mime }))
}
