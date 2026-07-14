import { describe, it, expect } from 'vitest'
import { napariColormapHex, CHANNEL_COLORMAP_OPTIONS } from './napariColormap'

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

describe('CHANNEL_COLORMAP_OPTIONS (batch-movie swatch palette)', () => {
  it('every option has a valid napari colormap value + a real hex swatch (single source of truth)', () => {
    expect(CHANNEL_COLORMAP_OPTIONS.length).toBeGreaterThan(0)
    for (const o of CHANNEL_COLORMAP_OPTIONS) {
      expect(o.hex).toMatch(/^#[0-9a-f]{6}$/i)
      expect(o.hex).toBe(napariColormapHex(o.value))   // derived from NAPARI_COLORMAP_HEX, not a copy
    }
  })
})
