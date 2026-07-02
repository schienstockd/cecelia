import { PDFDocument } from 'pdf-lib'
import { downloadBlob } from './export'

// Multipage PDF of the Analysis canvas: ONE page per tab, laid out as that tab's grid (each slot image
// placed at its grid-area rectangle). Each summary plot's aggregated data can ride along as an embedded
// CSV attachment (pdf-lib `attach`), so the figure + re-plottable data are one file
// (docs/todo/ANALYSIS_CANVAS_PLAN.md, Phase E). Pure — the caller supplies captured slot PNGs + CSVs.

// Each slot carries a NORMALISED rect (0..1) measured from the on-screen board, so the PDF reproduces
// the real layout (spans, plates, row height, gaps) rather than a re-derived uniform grid.
export interface PdfSlot { rect: { x: number; y: number; w: number; h: number }; png: string | null; csv?: string | null; name?: string }
export interface PdfPage { title: string; aspect: number; slots: PdfSlot[] }

// A4 in points (1pt = 1/72"): 210×297mm → 595.28 × 841.89. Each page picks the ORIENTATION that best
// fits its board (wide board → landscape, tall board → portrait) so the sheet stays true A4 either way.
// Tight margins — the board proportions (not padding) drive the spacing.
const A4_SHORT = 595.28, A4_LONG = 841.89, MARGIN = 16, TITLE_H = 18, PAD = 2

function dataUrlToBytes(dataUrl: string): Uint8Array {
  const b64 = dataUrl.slice(dataUrl.indexOf(',') + 1)
  const bin = atob(b64)
  const out = new Uint8Array(bin.length)
  for (let i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i)
  return out
}

const safe = (s: string) => s.replace(/[^\w.-]+/g, '_')

export async function exportTabsToPdf(pages: PdfPage[], filename = 'analysis.pdf') {
  const doc = await PDFDocument.create()
  for (const page of pages) {
    // A4, oriented to the board: wide (aspect ≥ 1) → landscape, tall → portrait
    const landscape = (page.aspect || 1) >= 1
    const PAGE_W = landscape ? A4_LONG : A4_SHORT
    const PAGE_H = landscape ? A4_SHORT : A4_LONG
    const p = doc.addPage([PAGE_W, PAGE_H])
    p.drawText(page.title, { x: MARGIN, y: PAGE_H - MARGIN - 2, size: 11 })

    // available area below the title, then fit the BOARD (preserving its aspect ratio) inside it and
    // centre — so the whole layout scales as one unit and slot proportions match the screen.
    const availW = PAGE_W - 2 * MARGIN
    const availH = PAGE_H - 2 * MARGIN - TITLE_H
    const aspect = page.aspect || (availW / availH)
    let bw = availW, bh = bw / aspect
    if (bh > availH) { bh = availH; bw = bh * aspect }
    const boardLeft = MARGIN + (availW - bw) / 2
    const boardTopFromTop = MARGIN + TITLE_H + (availH - bh) / 2   // distance from the PAGE TOP

    for (const slot of page.slots) {
      // slot rect within the board (top-origin), minus a hair of padding so neighbours don't touch
      const sx = boardLeft + slot.rect.x * bw + PAD
      const syTop = boardTopFromTop + slot.rect.y * bh + PAD
      const w = slot.rect.w * bw - 2 * PAD
      const h = slot.rect.h * bh - 2 * PAD

      if (slot.png) {
        try {
          const img = await doc.embedPng(dataUrlToBytes(slot.png))
          const s = Math.min(w / img.width, h / img.height)
          const iw = img.width * s, ih = img.height * s
          // centre the (aspect-preserved) image in its slot rect; PDF origin is bottom-left
          const x = sx + (w - iw) / 2
          const y = PAGE_H - (syTop + (h - ih) / 2) - ih
          p.drawImage(img, { x, y, width: iw, height: ih })
        } catch { /* skip an unembeddable image */ }
      }
      if (slot.csv) {
        try {
          await doc.attach(new TextEncoder().encode(slot.csv), safe(`${page.title}_${slot.name ?? 'plot'}.csv`),
            { mimeType: 'text/csv', description: `Data — ${slot.name ?? 'plot'} (${page.title})` })
        } catch { /* attachment optional */ }
      }
    }
  }
  const bytes = await doc.save()
  downloadBlob(filename, new Blob([bytes as unknown as BlobPart], { type: 'application/pdf' }))
}
