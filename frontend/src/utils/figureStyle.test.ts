import { describe, it, expect } from 'vitest'
import { HOUSE_STYLE, tileMm, figureLayout, styleOverrides, effectiveStyle, sampleTimeLabel } from './figureStyle'

describe('figure style', () => {
  it('auto tile width matches the renderer (render_figure.tile_mm — the Python test pins the same)', () => {
    expect([1, 2, 3, 9, 20].map(n => tileMm(HOUSE_STYLE, n))).toEqual([80, 80, 60, 20, 20])
    expect(tileMm(effectiveStyle({ tileMm: 35 }), 9)).toBe(35)
  })
  it('lays a strip out as the renderer composes it', () => {
    const l = figureLayout(HOUSE_STYLE, 3, 2)
    expect(l.widthMm).toBeCloseTo(3 * 60 + 2 * 0.4)
    expect(l.tiles.filter(t => t.bar)).toHaveLength(1)                     // one bar, on the first tile
    expect(l.tiles[0]).toMatchObject({ x: 0, bar: true })
    expect(l.bands).toHaveLength(2)
    expect(figureLayout(effectiveStyle({ scaleBarOn: 'all' }), 3).tiles.every(t => t.bar)).toBe(true)
  })
  it('a project stores only what differs from the house style', () => {
    expect(styleOverrides(HOUSE_STYLE)).toEqual({})
    expect(styleOverrides(effectiveStyle({ fontPt: 8, timeFormat: 'min' }))).toEqual({ fontPt: 8, timeFormat: 'min' })
    expect(HOUSE_STYLE.timeFormat).toBe('clock')          // the viewer's format by default
  })
  it('previews the time label in either format', () => {
    expect(sampleTimeLabel('min', 12.5)).toBe('12.5 min')
    expect(sampleTimeLabel('min', 150)).toBe('2.5 h')
    expect(sampleTimeLabel('clock', 12.5)).toBe('0:12:30')
    expect(sampleTimeLabel('clock', 59.999)).toBe('1:00:00')     // rounds once, like the renderer
  })
})
