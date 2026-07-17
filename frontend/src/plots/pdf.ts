import { downloadBlob } from './export'

// pdf-lib is large and only needed when the user actually exports — dynamic-import it inside
// exportTabsToPdf so it lands in its own chunk fetched on export, not the initial bundle.

// Multipage PDF of the Analysis board: ONE page per tab, laid out as that tab's grid (each slot image
// placed at its grid-area rectangle). Each summary plot's aggregated data can ride along as an embedded
// CSV attachment (pdf-lib `attach`), so the figure + re-plottable data are one file
// (docs/todo/ANALYSIS_CANVAS_PLAN.md, Phase E). Pure — the caller supplies captured slot PNGs + CSVs.

// Each slot carries a NORMALISED rect (0..1) measured from the on-screen board, so the PDF reproduces
// the real layout (spans, plates, row height, gaps) rather than a re-derived uniform grid. `svg` is the
// slot's VECTOR export (board→SVG path, plots/boardSvg.ts); `png` is the raster fallback / PDF image.
export interface PdfSlot { rect: { x: number; y: number; w: number; h: number }; png: string | null; svg?: string | null; csv?: string | null; name?: string; title?: string }
export interface PdfPage { title: string; aspect: number; slots: PdfSlot[] }

// A4 in points (1pt = 1/72"): 210×297mm → 595.28 × 841.89. Each page picks the ORIENTATION that best
// fits its board (wide board → landscape, tall board → portrait) so the sheet stays true A4 either way.
// Tight margins — the board proportions (not padding) drive the spacing.
const A4_SHORT = 595.28, A4_LONG = 841.89, MARGIN = 16, TITLE_H = 18, PAD = 2, SLOT_TITLE_H = 14

// ── shared page layout (ONE geometry, two backends: PDF here + SVG in boardSvg.ts) ─────────────────
// Pure: turn each page's normalised slot rects into absolute TOP-ORIGIN point rects on its A4 sheet
// (orientation from aspect; board aspect-fit + centred; per-slot title strip reserved). Both exporters
// consume this so the PDF and the board SVG land pixel-identical. Top-origin (y grows down) matches SVG;
// the PDF builder converts to its bottom-left origin.
export interface LaidSlot {
  slot: PdfSlot
  x: number; yTop: number; w: number; h: number        // slot rect (top-origin, pt)
  capH: number; imgTop: number; imgH: number           // title strip height + image area (top-origin)
}
export interface LaidPage { title: string; pageW: number; pageH: number; titleSize: number; slots: LaidSlot[] }
export function layoutPages(pages: PdfPage[]): LaidPage[] {
  return pages.map(page => {
    const landscape = (page.aspect || 1) >= 1
    const pageW = landscape ? A4_LONG : A4_SHORT
    const pageH = landscape ? A4_SHORT : A4_LONG
    const availW = pageW - 2 * MARGIN
    const availH = pageH - 2 * MARGIN - TITLE_H
    const aspect = page.aspect || (availW / availH)
    let bw = availW, bh = bw / aspect
    if (bh > availH) { bh = availH; bw = bh * aspect }
    const boardLeft = MARGIN + (availW - bw) / 2
    const boardTop = MARGIN + TITLE_H + (availH - bh) / 2   // from the PAGE TOP
    const slots = page.slots.map(slot => {
      const x = boardLeft + slot.rect.x * bw + PAD
      const yTop = boardTop + slot.rect.y * bh + PAD
      const w = slot.rect.w * bw - 2 * PAD
      const h = slot.rect.h * bh - 2 * PAD
      const capH = slot.title ? SLOT_TITLE_H : 0
      return { slot, x, yTop, w, h, capH, imgTop: yTop + capH, imgH: h - capH }
    })
    return { title: page.title, pageW, pageH, titleSize: 11, slots }
  })
}

function dataUrlToBytes(dataUrl: string): Uint8Array {
  const b64 = dataUrl.slice(dataUrl.indexOf(',') + 1)
  const bin = atob(b64)
  const out = new Uint8Array(bin.length)
  for (let i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i)
  return out
}

const safe = (s: string) => s.replace(/[^\w.-]+/g, '_')

export async function exportTabsToPdf(pages: PdfPage[], filename = 'analysis.pdf') {
  const { PDFDocument, StandardFonts } = await import('pdf-lib')
  const doc = await PDFDocument.create()
  const font = await doc.embedFont(StandardFonts.Helvetica)   // for centring slot titles (needs width metrics)
  for (const lp of layoutPages(pages)) {   // shared layout (identical to the board SVG)
    const p = doc.addPage([lp.pageW, lp.pageH])
    p.drawText(lp.title, { x: MARGIN, y: lp.pageH - MARGIN - 2, size: lp.titleSize })
    for (const ls of lp.slots) {
      const { slot } = ls
      // optional per-slot title (figure caption): a centred line at the top of the slot (top-origin →
      // convert to PDF's bottom-left origin). The image area shrinks below it.
      if (slot.title) {
        const size = 9
        const tw = font.widthOfTextAtSize(slot.title, size)
        p.drawText(slot.title, { x: ls.x + Math.max(0, (ls.w - tw) / 2), y: lp.pageH - ls.yTop - size, size, font })
      }
      if (slot.png) {
        try {
          const img = await doc.embedPng(dataUrlToBytes(slot.png))
          const s = Math.min(ls.w / img.width, ls.imgH / img.height)
          const iw = img.width * s, ih = img.height * s
          // centre the (aspect-preserved) image in its slot rect (below the title); PDF origin is bottom-left
          const x = ls.x + (ls.w - iw) / 2
          const y = lp.pageH - (ls.imgTop + (ls.imgH - ih) / 2) - ih
          p.drawImage(img, { x, y, width: iw, height: ih })
        } catch { /* skip an unembeddable image */ }
      }
      if (slot.csv) {
        try {
          await doc.attach(new TextEncoder().encode(slot.csv), safe(`${lp.title}_${slot.name ?? 'plot'}.csv`),
            { mimeType: 'text/csv', description: `Data — ${slot.name ?? 'plot'} (${lp.title})` })
        } catch { /* attachment optional */ }
      }
    }
  }
  const bytes = await doc.save()
  downloadBlob(filename, new Blob([bytes as unknown as BlobPart], { type: 'application/pdf' }))
}
