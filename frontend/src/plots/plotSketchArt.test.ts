import { describe, it, expect } from 'vitest'
import { PLOT_SKETCH_KINDS, SKETCH_H, SKETCH_W, placed, plotSketchArt, type SketchShape } from './plotSketchArt'

/** The box a shape covers (a path by every point it names — control points included, so a curve's
 *  bound is conservative). */
function bounds(s: SketchShape): [number, number, number, number] {
  switch (s.el) {
    case 'circle': return [s.x - s.r, s.y - s.r, s.x + s.r, s.y + s.r]
    case 'ellipse': return [s.x - s.rx, s.y - s.ry, s.x + s.rx, s.y + s.ry]
    case 'rect': return [s.x, s.y, s.x + s.w, s.y + s.h]
    case 'path': {
      const n = (s.d.match(/-?\d+(\.\d+)?/g) ?? []).map(Number)
      const xs = n.filter((_, i) => i % 2 === 0), ys = n.filter((_, i) => i % 2 === 1)
      return [Math.min(...xs), Math.min(...ys), Math.max(...xs), Math.max(...ys)]
    }
  }
}

describe('plot sketch art', () => {
  it.each(PLOT_SKETCH_KINDS)('%s draws something, inside its box', kind => {
    const shapes = plotSketchArt(kind)
    expect(shapes.length).toBeGreaterThan(0)
    for (const s of shapes) {
      const [x0, y0, x1, y1] = bounds(s)
      expect(x0, `${kind} ${JSON.stringify(s)}`).toBeGreaterThanOrEqual(0)
      expect(y0, `${kind} ${JSON.stringify(s)}`).toBeGreaterThanOrEqual(0)
      expect(x1, `${kind} ${JSON.stringify(s)}`).toBeLessThanOrEqual(SKETCH_W)
      expect(y1, `${kind} ${JSON.stringify(s)}`).toBeLessThanOrEqual(SKETCH_H)
    }
  })

  it('paths use absolute coordinate pairs only, so placing them moves every point', () => {
    for (const kind of PLOT_SKETCH_KINDS)
      for (const s of plotSketchArt(kind))
        if (s.el === 'path') expect(s.d, kind).toMatch(/^[MLQCZ0-9.\s-]+$/)
  })

  it('placing a subject moves and scales every point', () => {
    const [p] = placed([{ el: 'path', d: 'M10 20 L30 40', cls: 'ink' }], 5, 6, 0.5)
    expect(p).toMatchObject({ d: 'M10 16 L20 26' })
    const [c] = placed([{ el: 'circle', x: 10, y: 20, r: 4, cls: 'ink' }], 5, 6, 0.5)
    expect(c).toMatchObject({ x: 10, y: 16, r: 2 })
  })

  it('draws the same every time', () => {
    expect(plotSketchArt('tracksStar')).toEqual(plotSketchArt('tracksStar'))
  })
})
