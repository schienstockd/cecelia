import { describe, it, expect } from 'vitest'
import { ANNOTATION_PALETTE, ANNOTATION_COLOR_ORDER, DEFAULT_ANNOTATION_COLOR,
         ANNOTATION_STROKE_WIDTH_ORDER, STROKE_WIDTH_SCALES, DEFAULT_STROKE_WIDTH,
         resolveMarkColor, resolveMarkWidth,
         markCentroid01, resolveMarkRotate } from './overlayCompose'
import type { OverlayColor, OverlayMark, OverlayStrokeWidth } from './captureAddress'

const M = (over: Partial<OverlayMark>): OverlayMark => ({
  kind: 'rect', geom: { x: 0, y: 0, w: 0.5, h: 0.5 }, ...over,
})

describe('ANNOTATION_PALETTE', () => {
  it('names the five CVD-safe palette values', () => {
    expect(Object.keys(ANNOTATION_PALETTE).sort())
      .toEqual(['black', 'cyan', 'magenta', 'white', 'yellow'])
  })
  it('black is exactly #000000 — the white-composite plot ink must not have a tint', () => {
    expect(ANNOTATION_PALETTE.black).toBe('#000000')
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

describe('STROKE_WIDTH_SCALES', () => {
  it('covers exactly the three named presets — no drift with the type', () => {
    expect(Object.keys(STROKE_WIDTH_SCALES).sort()).toEqual(['medium', 'thick', 'thin'])
  })
  it('medium is 1x — a new mark reads the same weight as a pre-preset legacy mark', () => {
    expect(STROKE_WIDTH_SCALES.medium).toBe(1)
  })
  it('thin < medium < thick — a chip strip reads monotonically', () => {
    expect(STROKE_WIDTH_SCALES.thin).toBeLessThan(STROKE_WIDTH_SCALES.medium)
    expect(STROKE_WIDTH_SCALES.medium).toBeLessThan(STROKE_WIDTH_SCALES.thick)
  })
  it('the chip order matches the preset keys (no drift)', () => {
    expect([...ANNOTATION_STROKE_WIDTH_ORDER].sort()).toEqual(Object.keys(STROKE_WIDTH_SCALES).sort())
  })
})

describe('resolveMarkWidth', () => {
  const W = 1000   // 1000-px frame → MARK_LINE_WIDTH = max(2, round(1000/500)) = 2
  it('scales the auto width by the preset multiplier', () => {
    expect(resolveMarkWidth(M({ strokeWidth: 'medium' }), W)).toBe(2)
    expect(resolveMarkWidth(M({ strokeWidth: 'thick'  }), W)).toBe(4)
  })
  it('floors at 1 so a thin preset on a tiny frame is still visible', () => {
    // MARK_LINE_WIDTH = max(2, round(100/500)) = 2 ⇒ 2 * 0.5 = 1 (also the floor)
    expect(resolveMarkWidth(M({ strokeWidth: 'thin' }), 100)).toBe(1)
  })
  it('falls back to medium when the field is absent (older captures)', () => {
    expect(resolveMarkWidth(M({}), W)).toBe(resolveMarkWidth(M({ strokeWidth: DEFAULT_STROKE_WIDTH }), W))
  })
  it('falls back to medium on an unknown preset (tampered payload)', () => {
    expect(resolveMarkWidth(M({ strokeWidth: 'chunky' as OverlayStrokeWidth }), W))
      .toBe(resolveMarkWidth(M({ strokeWidth: 'medium' }), W))
  })
})

describe('markCentroid01', () => {
  it('rect: centre of the AA box', () => {
    const [cx, cy] = markCentroid01(M({ kind: 'rect', geom: { x: 0.1, y: 0.2, w: 0.4, h: 0.6 } }))
    expect(cx).toBeCloseTo(0.3); expect(cy).toBeCloseTo(0.5)
  })
  it('poly: AA-bounding-box centre — matches PowerPoint\'s selection frame origin', () => {
    // Diamond: pts at (0.5, 0.1) (0.9, 0.5) (0.5, 0.9) (0.1, 0.5). BBox is (0.1..0.9, 0.1..0.9)
    // — centre (0.5, 0.5). Arithmetic mean would ALSO give (0.5, 0.5) here (symmetric case);
    // asymmetric case below distinguishes the two.
    expect(markCentroid01(M({ kind: 'poly',
      geom: { pts: [[0.5, 0.1], [0.9, 0.5], [0.5, 0.9], [0.1, 0.5]] } }))).toEqual([0.5, 0.5])
    // Asymmetric — three vertices clustered left, one right. BBox centre stays at the midpoint;
    // arithmetic mean would drift left. This is the case where PowerPoint parity matters.
    const [cx, cy] = markCentroid01(M({ kind: 'poly',
      geom: { pts: [[0.1, 0.1], [0.15, 0.2], [0.2, 0.3], [0.9, 0.5]] } }))
    expect(cx).toBeCloseTo(0.5); expect(cy).toBeCloseTo(0.3)
  })
  it('empty vertex set: falls back to frame centre (no divide-by-zero)', () => {
    expect(markCentroid01(M({ kind: 'poly', geom: { pts: [] } }))).toEqual([0.5, 0.5])
  })
})

describe('resolveMarkRotate', () => {
  it('numeric rotate survives on every kind, including stroke (bbox-based rotation)', () => {
    expect(resolveMarkRotate(M({ rotate: 42 }))).toBe(42)
    expect(resolveMarkRotate(M({ rotate: -180 }))).toBe(-180)
    expect(resolveMarkRotate(M({ kind: 'stroke', geom: { pts: [[0, 0], [1, 1]] }, rotate: 90 }))).toBe(90)
  })
  it('absent / non-finite ⇒ 0', () => {
    expect(resolveMarkRotate(M({}))).toBe(0)
    expect(resolveMarkRotate(M({ rotate: NaN }))).toBe(0)
    expect(resolveMarkRotate(M({ rotate: Infinity }))).toBe(0)
  })
})
