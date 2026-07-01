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

function loadImg(url: string): Promise<HTMLImageElement | null> {
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
  opts: { hiRes?: (cv: HTMLCanvasElement, scale: number) => Promise<CanvasImageSource | null> } = {}): Promise<string | null> {
  if (!host) return null
  const w = host.clientWidth || host.offsetWidth, h = host.clientHeight || host.offsetHeight
  if (!w || !h) return null
  const scale = RASTER_SCALE
  const c = document.createElement('canvas'); c.width = w * scale; c.height = h * scale
  const ctx = c.getContext('2d'); if (!ctx) return null
  ctx.scale(scale, scale)
  if (bg && bg !== 'transparent') { ctx.fillStyle = bg; ctx.fillRect(0, 0, w, h) }
  const hr = host.getBoundingClientRect()
  for (const cv of Array.from(host.querySelectorAll('canvas'))) {
    const r = cv.getBoundingClientRect()
    const hi = opts.hiRes ? await opts.hiRes(cv, scale) : null
    try { ctx.drawImage(hi ?? cv, r.left - hr.left, r.top - hr.top, r.width, r.height) } catch { /* tainted/empty */ }
  }
  const overlayUrl = await elementToImageURL(host, 'svg', 'transparent', { blankCanvases: true })
  if (overlayUrl) { const img = await loadImg(overlayUrl); if (img) ctx.drawImage(img, 0, 0, w, h) }
  return c.toDataURL('image/png')
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
