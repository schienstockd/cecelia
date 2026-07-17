import { describe, it, expect } from 'vitest'
import { svgDoc, svgCircles, svgPolygon, svgPath, svgRect, svgLine, svgText, svgImage, svgEsc, nestSvg,
         svgSizeWarning, SVG_SIZE_WARN_BYTES } from './export'

// The dot plots (UMAP, gating) export a TRUE-VECTOR SVG so figures open editable in Illustrator and the
// boss can recolour the dots (docs/PLOTS.md). These pure string builders are the shared engine — tested
// here without mounting a component (frontend test rule: logic lives in plain .ts).

describe('svgCircles — the recolourable dot group', () => {
  it('emits one <circle> per point under a single group with the group fill', () => {
    const svg = svgCircles([[1, 2], [3, 4], [5, 6]], { fill: '#ef4444', r: 2 })
    expect((svg.match(/<circle /g) ?? [])).toHaveLength(3)
    // ONE group fill (the recolour hook) — not a per-point fill
    expect((svg.match(/fill=/g) ?? [])).toHaveLength(1)
    expect(svg.startsWith('<g fill="#ef4444"')).toBe(true)
    expect(svg).toContain('<circle cx="1" cy="2" r="2"/>')
  })

  it('carries group opacity + a data-group label, and rounds coords to 1dp', () => {
    const svg = svgCircles([[1.234, 5.678]], { fill: '#000', opacity: 0.5, r: 1.5, label: 'cluster 3' })
    expect(svg).toContain('fill-opacity="0.5"')
    expect(svg).toContain('data-group="cluster 3"')
    expect(svg).toContain('cx="1.2" cy="5.7"')       // rounded
  })

  it('empty input → empty string (no stray group)', () => {
    expect(svgCircles([], { fill: '#000' })).toBe('')
  })
})

describe('svgDoc — document wrapper', () => {
  it('wraps a body in a valid <svg> root with viewBox + a background rect when given', () => {
    const doc = svgDoc({ width: 100, height: 80, background: '#ffffff', body: '<circle/>' })
    expect(doc.startsWith('<svg xmlns="http://www.w3.org/2000/svg"')).toBe(true)
    expect(doc).toContain('viewBox="0 0 100 80"')
    expect(doc).toContain('<rect width="100%" height="100%" fill="#ffffff"/>')
    expect(doc).toContain('<circle/>')
    expect(doc.endsWith('</svg>')).toBe(true)
  })

  it('omits the background rect for transparent / unset backgrounds', () => {
    expect(svgDoc({ width: 10, height: 10, body: '' })).not.toContain('<rect')
    expect(svgDoc({ width: 10, height: 10, background: 'transparent', body: '' })).not.toContain('<rect')
  })
})

describe('vector primitives', () => {
  it('svgPolygon builds a points list; no stroke → fill none', () => {
    const p = svgPolygon([[0, 0], [10, 0], [10, 10]], { stroke: '#a78bfa', width: 1.5 })
    expect(p).toContain('points="0,0 10,0 10,10"')
    expect(p).toContain('stroke="#a78bfa"')
    expect(p).toContain('stroke-width="1.5"')
    expect(svgPolygon([[0, 0]], { stroke: '#000' })).toBe('')   // <2 points → nothing
  })

  it('svgPath / svgRect / svgLine / svgText / svgImage emit their elements', () => {
    expect(svgPath('M0 0L1 1Z', { stroke: '#333' })).toContain('<path d="M0 0L1 1Z"')
    expect(svgRect(1, 2, 3, 4, { fill: '#fff', stroke: '#000', rx: 2 })).toContain('<rect x="1" y="2" width="3" height="4"')
    expect(svgLine(0, 0, 5, 5, { stroke: '#000' })).toBe('<line x1="0" y1="0" x2="5" y2="5" stroke="#000" stroke-width="1"/>')
    expect(svgText(4, 4, 'hi', { fill: '#111', size: 12, anchor: 'middle' })).toContain('>hi</text>')
    expect(svgImage('data:image/png;base64,AAAA', 0, 0, 10, 10)).toContain('href="data:image/png;base64,AAAA"')
  })

  it('rotated text carries a rotate transform around its anchor', () => {
    expect(svgText(10, 20, 'y', { fill: '#111', rotate: -90 })).toContain('transform="rotate(-90 10 20)"')
  })
})

describe('nestSvg — stitch a plot SVG into a board slot', () => {
  it('re-attributes the root to the slot rect, keeps the viewBox, preserves inner content', () => {
    const nested = nestSvg('<svg xmlns="http://www.w3.org/2000/svg" width="100" height="50" viewBox="0 0 100 50"><circle cx="1" cy="2" r="3"/></svg>', 10, 20, 200, 120)
    expect(nested.startsWith('<svg ')).toBe(true)
    expect(nested).toContain('x="10" y="20" width="200" height="120"')
    expect(nested).toContain('viewBox="0 0 100 50"')
    expect(nested).toContain('preserveAspectRatio="xMidYMid meet"')
    expect(nested).toContain('<circle cx="1" cy="2" r="3"/>')
    expect(nested.endsWith('</svg>')).toBe(true)
    expect((nested.match(/<svg/g) ?? [])).toHaveLength(1)   // one root re-attributed (not doubly wrapped)
  })

  it('derives a viewBox from width/height when the source has none', () => {
    expect(nestSvg('<svg width="80" height="40"><g/></svg>', 0, 0, 8, 4)).toContain('viewBox="0 0 80 40"')
  })

  it('empty / non-svg input → empty string', () => {
    expect(nestSvg('', 0, 0, 1, 1)).toBe('')
    expect(nestSvg('<div>not svg</div>', 0, 0, 1, 1)).toBe('')
  })
})

describe('svgSizeWarning — heavy-SVG heads-up', () => {
  it('returns null for a small SVG, a message (with the label) for a large one', () => {
    expect(svgSizeWarning('<svg/>', 'UMAP')).toBeNull()
    const big = 'x'.repeat(SVG_SIZE_WARN_BYTES + 1)
    const msg = svgSizeWarning(big, 'UMAP (400,000 cells)')
    expect(msg).toContain('UMAP (400,000 cells)')
    expect(msg).toContain('Illustrator')
  })
})

describe('svgEsc — XML safety in labels', () => {
  it('escapes the five XML entities', () => {
    expect(svgEsc(`a & b < c > d " e ' f`)).toBe('a &amp; b &lt; c &gt; d &quot; e &#39; f')
  })
  it('a population name with an ampersand survives into text/circle-group markup', () => {
    expect(svgText(0, 0, 'CD4 & CD8', { fill: '#000' })).toContain('CD4 &amp; CD8')
    expect(svgCircles([[0, 0]], { fill: '#000', label: 'a<b' })).toContain('data-group="a&lt;b"')
  })
})
