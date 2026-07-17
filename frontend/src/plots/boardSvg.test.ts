import { describe, it, expect } from 'vitest'
import { buildBoardSvgs } from './boardSvg'
import type { PdfPage } from './pdf'

// The board→SVG export stitches each slot into ONE page SVG on the same A4 layout as the PDF: a slot with
// a captured vector SVG becomes a nested <svg>; a slot with only a PNG (image/HMM fallback) becomes an
// <image>. See docs/ANALYSIS.md.

describe('buildBoardSvgs — vector board stitch', () => {
  const page: PdfPage = {
    title: 'Board 1', aspect: 1.4,
    slots: [
      { rect: { x: 0, y: 0, w: 0.5, h: 1 }, png: 'data:image/png;base64,AAAA',
        svg: '<svg width="100" height="100" viewBox="0 0 100 100"><circle cx="5" cy="5" r="2"/></svg>', title: 'UMAP' },
      { rect: { x: 0.5, y: 0, w: 0.5, h: 1 }, png: 'data:image/png;base64,BBBB', svg: null, name: 'filmstrip' },
    ],
  }

  it('emits one document per page, sized to an A4 sheet, on a white ground', () => {
    const out = buildBoardSvgs([page])
    expect(out).toHaveLength(1)
    expect(out[0].title).toBe('Board 1')
    expect(out[0].svg.startsWith('<svg xmlns="http://www.w3.org/2000/svg"')).toBe(true)
    expect(out[0].svg).toContain('fill="#ffffff"')       // white background rect
    expect(out[0].svg).toContain('>Board 1</text>')      // page title
  })

  it('nests the vector slot as <svg> and embeds the raster-only slot as <image>', () => {
    const svg = buildBoardSvgs([page])[0].svg
    // the vector slot's inner content survives, nested (aspect-fit)
    expect(svg).toContain('<circle cx="5" cy="5" r="2"/>')
    expect(svg).toContain('preserveAspectRatio="xMidYMid meet"')
    // the raster-only slot embeds its PNG as an <image>
    expect(svg).toContain('<image ')
    expect(svg).toContain('href="data:image/png;base64,BBBB"')
    // the vector slot's PNG is NOT embedded (vector took precedence)
    expect(svg).not.toContain('data:image/png;base64,AAAA')
    // the per-slot caption is drawn
    expect(svg).toContain('>UMAP</text>')
  })
})
