import { describe, it, expect } from 'vitest'
import { splitXYZ, normValues, barTicks, fitLabel, barStops, colourBarSvg, BAR_STOPS } from './valueColour'

describe('splitXYZ', () => {
  it('de-interleaves triples into xy pairs + values', () => {
    const { points, values } = splitXYZ(new Float32Array([1, 2, 3, 4, 5, 6]))
    expect(Array.from(points)).toEqual([1, 2, 4, 5])
    expect(Array.from(values)).toEqual([3, 6])
  })
  it('ignores a truncated trailing point', () => {
    const { points, values } = splitXYZ(new Float32Array([1, 2, 3, 4]))
    expect(Array.from(points)).toEqual([1, 2])
    expect(Array.from(values)).toEqual([3])
  })
})

describe('normValues', () => {
  it('maps the extent onto 0..1 and clamps outside it', () => {
    const t = normValues(new Float32Array([0, 5, 10, -3, 99]), [0, 10])
    expect(Array.from(t)).toEqual([0, 0.5, 1, 0, 1])
  })
  it('keeps a missing value NaN instead of painting it as the ramp floor', () => {
    const t = normValues(new Float32Array([NaN, Infinity, 5]), [0, 10])
    expect(t[0]).toBeNaN(); expect(t[1]).toBeNaN(); expect(t[2]).toBe(0.5)
  })
  it('sends a zero-width extent to the middle of the ramp, not to a division by zero', () => {
    expect(Array.from(normValues(new Float32Array([7, 7]), [7, 7]))).toEqual([0.5, 0.5])
  })
})

describe('barTicks', () => {
  const ticks = [{ pos: 0, label: '0' }, { pos: 5, label: '500' }, { pos: 10, label: '10k' }]
  it('places each served tick at its fraction along the bar', () => {
    expect(barTicks(ticks, [0, 10])).toEqual([
      { frac: 0, label: '0' }, { frac: 0.5, label: '500' }, { frac: 1, label: '10k' }])
  })
  it('drops ticks outside the bar', () => {
    expect(barTicks(ticks, [4, 6]).map(t => t.label)).toEqual(['500'])
  })
  it('labels a flat ramp with its single value', () => {
    expect(barTicks(ticks, [3, 3])).toEqual([{ frac: 1, label: '10k' }])
  })
})

describe('fitLabel', () => {
  const measure = (s: string) => s.length * 2      // 2px per char
  it('leaves a label that fits alone', () => {
    expect(fitLabel('CD169', 20, measure)).toBe('CD169')
  })
  it('ellipsises the end so the measure name still reads', () => {
    expect(fitLabel('CD169-Katushka', 12, measure)).toBe('CD169…')
  })
  it('gives up rather than drawing a lone ellipsis in no space', () => {
    expect(fitLabel('CD169', 1, measure)).toBe('')
  })
})

describe('barStops', () => {
  it('runs a vertical bar high→low, so the max sits at the top like an axis', () => {
    const v = barStops('v', 3)
    expect(v).toEqual([1, 0.5, 0])
  })
  it('runs a horizontal bar low→high (left→right)', () => {
    expect(barStops('h', 3)).toEqual([0, 0.5, 1])
  })
})

describe('colourBarSvg', () => {
  const ticks = [{ pos: 0, label: '0' }, { pos: 10, label: '10k' }]
  const box = { x: 10, y: 20, w: 8, h: 40 }
  const opts = { extent: [0, 10] as [number, number], ticks, ink: '#333', fontSize: 9 }
  it('emits one band per stop plus a frame, and every served label', () => {
    const svg = colourBarSvg(box, opts)
    expect((svg.match(/<rect/g) ?? []).length).toBe(BAR_STOPS + 1)
    expect(svg).toContain('>0<')
    expect(svg).toContain('>10k<')
  })
  it('puts the low end at the BOTTOM of a vertical bar', () => {
    const at = (svg: string, label: string) => {
      for (const m of svg.matchAll(/<text[^>]*y="([-\d.]+)"[^>]*>([^<]*)</g)) if (m[2] === label) return Number(m[1])
      return NaN
    }
    const svg = colourBarSvg(box, opts)
    expect(at(svg, '0')).toBeGreaterThan(at(svg, '10k'))
    // ...and left→right on a horizontal one
    const h = colourBarSvg(box, { ...opts, orient: 'h' })
    const x = (label: string) => {
      for (const m of h.matchAll(/<text x="([-\d.]+)"[^>]*>([^<]*)</g)) if (m[2] === label) return Number(m[1])
      return NaN
    }
    expect(x('10k')).toBeGreaterThan(x('0'))
  })
  it('captions the bar with the measure and nothing when unnamed', () => {
    expect(colourBarSvg(box, { ...opts, label: 'CD169' })).toContain('>CD169<')
    expect(colourBarSvg(box, opts)).not.toContain('CD169')
  })
})
