import { svgDoc, svgText, svgImage, nestSvg } from './export'
import { layoutPages, type PdfPage } from './pdf'

// Board → VECTOR SVG (docs/ANALYSIS.md). One SVG document per board tab, laid out on the SAME A4 page
// geometry as the PDF (`layoutPages` — one geometry, two backends) so the two exports land identically.
// Each slot is a NESTED <svg> when a vector export was captured (summary / cluster heatmap / UMAP /
// gating — editable in Illustrator), else its raster PNG as an <image> (image/filmstrip + HMM slots,
// which have no clean vector form). Pure: the caller (TabbedCanvas) supplies the captured slots.

export interface BoardSvg { title: string; svg: string }

export function buildBoardSvgs(pages: PdfPage[]): BoardSvg[] {
  return layoutPages(pages).map(lp => {
    let body = ''
    // page title (top-origin, baseline just below the top margin — mirrors the PDF's drawText)
    body += svgText(16, 16 + lp.titleSize, lp.title, { fill: '#111', size: lp.titleSize, weight: 600 })
    for (const ls of lp.slots) {
      const { slot } = ls
      if (slot.title) body += svgText(ls.x + ls.w / 2, ls.yTop + 9, slot.title, { fill: '#111', size: 9, anchor: 'middle' })
      // vector when we have it; else the raster fallback, aspect-fit like the PDF image placement
      if (slot.svg) body += nestSvg(slot.svg, ls.x, ls.imgTop, ls.w, ls.imgH)
      else if (slot.png) body += svgImage(slot.png, ls.x, ls.imgTop, ls.w, ls.imgH, 'meet')
    }
    return { title: lp.title, svg: svgDoc({ width: lp.pageW, height: lp.pageH, background: '#ffffff', body }) }
  })
}
