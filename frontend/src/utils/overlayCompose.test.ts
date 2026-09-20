import { describe, it, expect } from 'vitest'
import { ANNOTATION_PALETTE, ANNOTATION_COLOR_ORDER, DEFAULT_ANNOTATION_COLOR,
         resolveMarkColor } from './overlayCompose'
import type { OverlayColor, OverlayMark } from './captureAddress'

const M = (over: Partial<OverlayMark>): OverlayMark => ({
  kind: 'rect', geom: { x: 0, y: 0, w: 0.5, h: 0.5 }, ...over,
})

describe('ANNOTATION_PALETTE', () => {
  it('names the four CVD-safe palette values', () => {
    expect(Object.keys(ANNOTATION_PALETTE).sort()).toEqual(['cyan', 'magenta', 'white', 'yellow'])
  })
  it('every palette value is a 7-char hex', () => {
    for (const v of Object.values(ANNOTATION_PALETTE)) {
      expect(v).toMatch(/^#[0-9a-f]{6}$/i)
    }
  })
  it('white is exactly #ffffff — the default fallback must not have a tint', () => {
    expect(ANNOTATION_PALETTE.white).toBe('#ffffff')
  })
  it('the swatch order matches the palette keys (no drift)', () => {
    expect([...ANNOTATION_COLOR_ORDER].sort()).toEqual(Object.keys(ANNOTATION_PALETTE).sort())
  })
})

describe('resolveMarkColor', () => {
  it('returns the palette value for every valid name', () => {
    for (const name of ANNOTATION_COLOR_ORDER) {
      expect(resolveMarkColor(M({ color: name }))).toBe(ANNOTATION_PALETTE[name])
    }
  })
  it('falls back to white when the field is absent (older captures)', () => {
    expect(resolveMarkColor(M({}))).toBe(ANNOTATION_PALETTE[DEFAULT_ANNOTATION_COLOR])
    expect(resolveMarkColor(M({ color: undefined }))).toBe(ANNOTATION_PALETTE[DEFAULT_ANNOTATION_COLOR])
  })
  it('falls back to white on an unknown palette name (tampered payload)', () => {
    expect(resolveMarkColor(M({ color: 'chartreuse' as OverlayColor })))
      .toBe(ANNOTATION_PALETTE[DEFAULT_ANNOTATION_COLOR])
  })
})
