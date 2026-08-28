import { describe, it, expect } from 'vitest'
import { napariColormapHex, napariColormapForHex, CHANNEL_COLORMAP_OPTIONS } from './napariColormap'

describe('napariColormapHex', () => {
  it('maps single-hue channel colormaps', () => {
    expect(napariColormapHex('red')).toBe('#ff0000')
    expect(napariColormapHex('green')).toBe('#00ff00')
    expect(napariColormapHex('magenta')).toBe('#ff00ff')
    expect(napariColormapHex('bop blue')).toBe('#1e6fff')
  })

  it('is case-insensitive', () => {
    expect(napariColormapHex('Red')).toBe('#ff0000')
    expect(napariColormapHex('BOP Orange')).toBe('#ff7f0e')
  })

  it('maps gray/grey to a light swatch', () => {
    expect(napariColormapHex('gray')).toBe('#d4d4d4')
    expect(napariColormapHex('grey')).toBe('#d4d4d4')
  })

  it('returns null for continuous maps and unknowns (not a channel tint)', () => {
    expect(napariColormapHex('viridis')).toBeNull()
    expect(napariColormapHex('turbo')).toBeNull()
    expect(napariColormapHex('magma')).toBeNull()
    expect(napariColormapHex('')).toBeNull()
    expect(napariColormapHex(null)).toBeNull()
    expect(napariColormapHex(undefined)).toBeNull()
  })
})

describe('napariColormapForHex (reverse)', () => {
  it('reverses the picker palette', () => {
    expect(napariColormapForHex('#ff0000')).toBe('red')
    expect(napariColormapForHex('#00ff00')).toBe('green')
    expect(napariColormapForHex('#0000ff')).toBe('blue')
  })

  it('prefers the picker canonical name when several map to one hex', () => {
    // 'gray' and 'grey' both map to #d4d4d4; the picker uses 'gray'.
    expect(napariColormapForHex('#d4d4d4')).toBe('gray')
  })

  it('is case-insensitive on the hex', () => {
    expect(napariColormapForHex('#FF7F0E')).toBe('bop orange')
  })

  it('returns null for a colour outside the palette', () => {
    expect(napariColormapForHex('#123456')).toBeNull()
    expect(napariColormapForHex('')).toBeNull()
    expect(napariColormapForHex(null)).toBeNull()
    expect(napariColormapForHex(undefined)).toBeNull()
  })

  it('round-trips the palette (name → hex → name)', () => {
    for (const o of CHANNEL_COLORMAP_OPTIONS) expect(napariColormapForHex(o.hex)).toBe(o.value)
  })
})

describe('CHANNEL_COLORMAP_OPTIONS (batch-movie swatch palette)', () => {
  it('every option has a valid napari colormap value + a real hex swatch (single source of truth)', () => {
    expect(CHANNEL_COLORMAP_OPTIONS.length).toBeGreaterThan(0)
    for (const o of CHANNEL_COLORMAP_OPTIONS) {
      expect(o.hex).toMatch(/^#[0-9a-f]{6}$/i)
      expect(o.hex).toBe(napariColormapHex(o.value))   // derived from NAPARI_COLORMAP_HEX, not a copy
    }
  })
})
