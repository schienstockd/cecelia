import { describe, it, expect } from 'vitest'
import {
  parseHex, toHex, composite, mix, luminance, contrastRatio, readableOn, WCAG_AA,
} from './colour'

describe('parseHex', () => {
  it('accepts 3, 6 and 8 digit forms, with or without the hash', () => {
    expect(parseHex('#fff')).toEqual([255, 255, 255])
    expect(parseHex('065f46')).toEqual([6, 95, 70])
    expect(parseHex('#065F46')).toEqual([6, 95, 70])
    // an 8-digit value is what our own tints look like (`colour + '22'`); the alpha is dropped here
    // because compositing needs the BACKGROUND, which a colour string cannot carry
    expect(parseHex('#065f4622')).toEqual([6, 95, 70])
  })

  it('returns null rather than a wrong colour', () => {
    for (const bad of ['', '#', 'red', '#12', '#12345', 'rgb(1,2,3)', 'javascript:alert(1)'])
      expect(parseHex(bad), bad).toBeNull()
  })
})

describe('composite', () => {
  // The direction is the whole point: `fg·a + bg·(1-a)`. Inverting it renders a 13% tint as a nearly
  // solid colour, and the result still looks like a plausible pill — which is how it survives review.
  it('paints the foreground AT the alpha, not the background', () => {
    // #065f46 at 13.3% over #21262d is mostly the surface
    expect(composite('#065f46', '#21262d', 0x22 / 255)).toBe('#1d2e30')
    expect(luminance(composite('#065f46', '#21262d', 0x22 / 255)))
      .toBeLessThan(luminance('#065f46') + 0.02)
  })

  it('is the identity at the alpha extremes', () => {
    expect(composite('#abcdef', '#000000', 1)).toBe('#abcdef')
    expect(composite('#abcdef', '#123456', 0)).toBe('#123456')
  })

  it('clamps a nonsense alpha instead of producing a negative channel', () => {
    expect(composite('#ffffff', '#000000', -3)).toBe('#000000')
    expect(composite('#ffffff', '#000000', 7)).toBe('#ffffff')
  })

  it('passes the colour through unchanged when either side is unparseable', () => {
    expect(composite('#065f46', 'nope', 0.5)).toBe('#065f46')
  })
})

describe('luminance / contrastRatio — WCAG 2.1 golden values', () => {
  // The spec's own anchors: black is 0, white is 1, and their ratio is exactly 21.
  it('matches the spec at the extremes', () => {
    expect(luminance('#000000')).toBe(0)
    expect(luminance('#ffffff')).toBeCloseTo(1, 10)
    expect(contrastRatio('#000000', '#ffffff')).toBeCloseTo(21, 10)
    expect(contrastRatio('#123456', '#123456')).toBeCloseTo(1, 10)
  })

  // Mid grey #808080: (0.5019..)^2.4-ish through the sRGB curve → 0.2158, and 5.32:1 on white. These
  // are the numbers a naive (channel-average) luminance gets wrong, which is why they are pinned.
  it('applies the sRGB curve, not a channel average', () => {
    expect(luminance('#808080')).toBeCloseTo(0.21586, 5)
    expect(contrastRatio('#808080', '#ffffff')).toBeCloseTo(3.95, 2)
    expect(contrastRatio('#808080', '#000000')).toBeCloseTo(5.32, 2)
    // pure green is far brighter than pure blue at the same channel value — the 0.7152 vs 0.0722 split
    expect(luminance('#00ff00')).toBeCloseTo(0.7152, 4)
    expect(luminance('#0000ff')).toBeCloseTo(0.0722, 4)
  })

  it('is symmetric in its arguments', () => {
    expect(contrastRatio('#065f46', '#21262d')).toBeCloseTo(contrastRatio('#21262d', '#065f46'), 10)
  })
})

describe('mix / toHex', () => {
  it('interpolates and clamps', () => {
    expect(mix('#000000', '#ffffff', 0.5)).toBe('#808080')
    expect(mix('#000000', '#ffffff', 0)).toBe('#000000')
    expect(mix('#000000', '#ffffff', 2)).toBe('#ffffff')
    expect(toHex([-5, 300, 128])).toBe('#00ff80')
  })
})

describe('readableOn', () => {
  const surface = (c: string) => composite(c, '#21262d', 0x22 / 255)

  // The real case: every one of the twelve MODULE_COLORS fails AA as its own label text.
  it('lifts a colour to the target and no further', () => {
    const c = '#065f46'
    const bg = surface(c)
    expect(contrastRatio(c, bg)).toBeLessThan(WCAG_AA)      // the problem
    const lifted = readableOn(c, bg)
    expect(contrastRatio(lifted, bg)).toBeGreaterThanOrEqual(WCAG_AA)
    // "and no further" — one step back is still short, so nothing was over-lightened
    expect(contrastRatio(mix(c, '#ffffff', 0.37), bg)).toBeLessThan(WCAG_AA)
  })

  it('keeps the hue recognisable rather than washing to white', () => {
    const c = '#7c3aed'                                     // manageimages purple
    const lifted = readableOn(c, surface(c))
    const [r, g, b] = parseHex(lifted)!
    expect(b).toBeGreaterThan(g)                            // still purple, not grey
    expect(r).toBeGreaterThan(g)
    expect(lifted).not.toBe('#ffffff')
  })

  it('returns the colour untouched when it already passes', () => {
    expect(readableOn('#ffffff', '#000000')).toBe('#ffffff')
  })

  // Not a hypothetical direction: an exported light-theme plot would need it, and getting there by
  // mixing toward white would make contrast WORSE — which is why `toward` is a parameter and why
  // `readableOn` steps rather than solving for a monotonic curve that doesn't exist.
  it('can be pointed at black for a light background', () => {
    const c = '#a97df3'                                     // fails on near-white (2.79:1)
    expect(contrastRatio(c, '#f5f5f5')).toBeLessThan(WCAG_AA)
    const lifted = readableOn(c, '#f5f5f5', WCAG_AA, '#000000')
    expect(contrastRatio(lifted, '#f5f5f5')).toBeGreaterThanOrEqual(WCAG_AA)
    expect(luminance(lifted)).toBeLessThan(luminance(c))     // darkened, not lightened
  })
})
