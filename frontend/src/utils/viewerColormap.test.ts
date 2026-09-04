import { describe, it, expect } from 'vitest'
import { viewerColormapHex, viewerColormapForHex, CHANNEL_COLORMAP_OPTIONS } from './viewerColormap'

describe('viewerColormapHex', () => {
  it('maps single-hue channel colormaps', () => {
    expect(viewerColormapHex('red')).toBe('#ff0000')
    expect(viewerColormapHex('green')).toBe('#00ff00')
    expect(viewerColormapHex('magenta')).toBe('#ff00ff')
    expect(viewerColormapHex('bop blue')).toBe('#1e6fff')
  })

  it('is case-insensitive', () => {
    expect(viewerColormapHex('Red')).toBe('#ff0000')
    expect(viewerColormapHex('BOP Orange')).toBe('#ff7f0e')
  })

  it('maps gray/grey to a light swatch', () => {
    expect(viewerColormapHex('gray')).toBe('#d4d4d4')
    expect(viewerColormapHex('grey')).toBe('#d4d4d4')
  })

  it('returns null for continuous maps and unknowns (not a channel tint)', () => {
    expect(viewerColormapHex('viridis')).toBeNull()
    expect(viewerColormapHex('turbo')).toBeNull()
    expect(viewerColormapHex('magma')).toBeNull()
    expect(viewerColormapHex('')).toBeNull()
    expect(viewerColormapHex(null)).toBeNull()
    expect(viewerColormapHex(undefined)).toBeNull()
  })
})

describe('viewerColormapForHex (reverse)', () => {
  it('reverses the picker palette', () => {
    expect(viewerColormapForHex('#ff0000')).toBe('red')
    expect(viewerColormapForHex('#00ff00')).toBe('green')
    expect(viewerColormapForHex('#0000ff')).toBe('blue')
  })

  it('prefers the picker canonical name when several map to one hex', () => {
    // 'gray' and 'grey' both map to #d4d4d4; the picker uses 'gray'.
    expect(viewerColormapForHex('#d4d4d4')).toBe('gray')
  })

  it('is case-insensitive on the hex', () => {
    expect(viewerColormapForHex('#FF7F0E')).toBe('bop orange')
  })

  it('returns null for a colour outside the palette', () => {
    expect(viewerColormapForHex('#123456')).toBeNull()
    expect(viewerColormapForHex('')).toBeNull()
    expect(viewerColormapForHex(null)).toBeNull()
    expect(viewerColormapForHex(undefined)).toBeNull()
  })

  it('round-trips the palette (name → hex → name)', () => {
    for (const o of CHANNEL_COLORMAP_OPTIONS) expect(viewerColormapForHex(o.hex)).toBe(o.value)
  })
})

describe('CHANNEL_COLORMAP_OPTIONS (batch-movie swatch palette)', () => {
  it('every option has a valid viewer colormap value + a real hex swatch (single source of truth)', () => {
    expect(CHANNEL_COLORMAP_OPTIONS.length).toBeGreaterThan(0)
    for (const o of CHANNEL_COLORMAP_OPTIONS) {
      expect(o.hex).toMatch(/^#[0-9a-f]{6}$/i)
      expect(o.hex).toBe(viewerColormapHex(o.value))   // derived from NAPARI_COLORMAP_HEX, not a copy
    }
  })
})
