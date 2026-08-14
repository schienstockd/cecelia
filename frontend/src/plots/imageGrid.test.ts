import { describe, it, expect } from 'vitest'
import { gridLayout, imageGridSvg, type GridImage } from './imageGrid'

const style = { gap: 10, captionH: 20, fontSize: 12, padding: 10, background: '#fff', colour: '#111' }

describe('gridLayout', () => {
  it('places tiles row-major and sizes the sheet to fit them', () => {
    const l = gridLayout(5, 3, 100, 80, style)
    // 3 columns → 100*3 + 10*2 gaps + 10*2 padding
    expect(l.width).toBe(340)
    // 2 rows of (80 tile + 20 caption) + 1 gap + padding
    expect(l.height).toBe(230)
    expect(l.cells).toHaveLength(5)
    expect(l.cells[0]).toMatchObject({ x: 10, y: 10 })
    expect(l.cells[2]).toMatchObject({ x: 230, y: 10 })     // end of row 1
    expect(l.cells[3]).toMatchObject({ x: 10, y: 120 })     // wrapped
  })

  it('puts the caption baseline inside its own strip, never over the tile', () => {
    const l = gridLayout(1, 1, 100, 80, style)
    const c = l.cells[0]
    expect(c.captionY).toBeGreaterThan(c.y + c.h)               // below the image
    expect(c.captionY).toBeLessThanOrEqual(c.y + c.h + style.captionH)
  })

  it('reserves no caption strip when captions are off', () => {
    const l = gridLayout(2, 1, 100, 80, { ...style, captionH: 0 })
    expect(l.height).toBe(10 + 80 + 10 + 80 + 10)               // padding, tile, gap, tile, padding
  })

  // A zero/NaN column count reaches this from `gridColumns` on an unmounted grid; a negative one from
  // arithmetic upstream. Either would make `rows` Infinity and the canvas allocation throw.
  it('never produces fewer than one column', () => {
    for (const cols of [0, -3, NaN]) {
      const l = gridLayout(4, cols, 100, 80, style)
      expect(l.cells).toHaveLength(4)
      expect(l.height).toBeGreaterThan(0)
      expect(Number.isFinite(l.width)).toBe(true)
    }
  })

  it('does not leave empty columns when there are fewer tiles than columns', () => {
    const l = gridLayout(2, 6, 100, 80, style)
    expect(l.width).toBe(10 + 100 * 2 + 10 + 10)                // 2 columns, not 6
  })

  it('is empty for no tiles, so a caller can bail before touching a canvas', () => {
    const l = gridLayout(0, 3, 100, 80, style)
    expect(l).toMatchObject({ width: 0, height: 0, cells: [] })
  })
})

describe('imageGridSvg', () => {
  const imgs: GridImage[] = [
    { name: 'flow_magnitude_s1', dataUrl: 'data:image/png;base64,AAA' },
    { name: 'divergence', dataUrl: 'data:image/png;base64,BBB' },
  ]

  it('emits one <image> and one <text> per tile, inside a sized document', () => {
    const svg = imageGridSvg(imgs, gridLayout(2, 2, 100, 80, style), style)
    expect(svg.match(/<image /g)).toHaveLength(2)
    expect(svg.match(/<text /g)).toHaveLength(2)
    expect(svg).toContain('href="data:image/png;base64,AAA"')
    expect(svg).toContain('>flow_magnitude_s1</text>')
    expect(svg).toContain('width="230"')                        // the layout's width, not a guess
    expect(svg).toContain('<rect width="100%" height="100%" fill="#fff"/>')
  })

  it('omits captions when the strip is zero', () => {
    const svg = imageGridSvg(imgs, gridLayout(2, 2, 100, 80, { ...style, captionH: 0 }),
                             { ...style, captionH: 0 })
    expect(svg.match(/<image /g)).toHaveLength(2)
    expect(svg).not.toContain('<text ')
  })

  // The tile names come from the server's plane list, so they are ours — but this is string
  // concatenation into markup, and `svgText` escaping is the only thing between the two.
  it('escapes a tile name rather than emitting it raw', () => {
    const svg = imageGridSvg([{ name: '<script>x</script>', dataUrl: 'data:image/png;base64,A' }],
                             gridLayout(1, 1, 10, 10, style), style)
    expect(svg).toContain('&lt;script&gt;')
    expect(svg).not.toContain('<script>')
  })

  it('is empty for no tiles', () => {
    expect(imageGridSvg([], gridLayout(0, 2, 100, 80, style), style)).toBe('')
  })

  // Guards the pairing between the two arguments: a layout computed for fewer cells than there are
  // images must drop the extras, not emit an `<image>` at NaN.
  it('skips an image with no cell', () => {
    const svg = imageGridSvg(imgs, gridLayout(1, 1, 100, 80, style), style)
    expect(svg.match(/<image /g)).toHaveLength(1)
    expect(svg).not.toContain('NaN')
  })
})

// The gap this file closes was invisible: three views rendered a base64 tile grid and NONE offered an
// export, because the omission looks like nothing — there is no broken pixel, just a missing dropdown.
// So it is asserted rather than remembered.
const VIEWS = import.meta.glob('/src/components/plots/*View.vue', {
  query: '?raw', import: 'default', eager: true,
}) as Record<string, string>

// Board-only views, which InteractivePanel never draws an Export select for (`!docked`). `filmstrip`
// is `analysisBoard: true` with no page flag, and it already implements `exportImage` for the board's
// PDF — a dropdown there would render nowhere. Give it a page flag and this list is what fails.
const EXPORTLESS_BY_DESIGN = ['ImageStripView.vue']

describe('every base64 tile grid offers an export', () => {
  it('…and the ones that do not are board-only, on purpose', () => {
    const missing = Object.entries(VIEWS)
      .filter(([, src]) => /data:image\/png;base64/.test(src))
      .filter(([, src]) => !/exportFormats/.test(src))
      .map(([p]) => p.split('/').pop()!)
    expect(missing.sort()).toEqual([...EXPORTLESS_BY_DESIGN].sort())
  })

  it('the glob resolved and found the tile grids', () => {
    const grids = Object.values(VIEWS).filter(src => /data:image\/png;base64/.test(src))
    expect(grids.length).toBeGreaterThanOrEqual(3)
  })
})
