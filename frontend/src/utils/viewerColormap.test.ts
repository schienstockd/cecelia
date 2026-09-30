import { describe, it, expect } from 'vitest'
import { viewerColormapHex, viewerColormapForHex, CHANNEL_COLORMAP_OPTIONS, distinctChannelHexes, distinctChannelOptions } from './viewerColormap'

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

describe('distinctChannelHexes', () => {
  it('matches the colours the viewer Distinct toggle wrote into a real movie config', () => {
    // zolIMa fXgbTl "smoothed-with-pops": SHG hidden, channels 2-4 recorded with Distinct on
    const [, nuc, mem, kat] = distinctChannelHexes(4)
    expect([nuc, mem, kat]).toEqual(['#3ce26e', '#a862da', '#dec821'])
  })
  it('is per-index, so the channel count does not move a channel\'s colour', () => {
    expect(distinctChannelHexes(6).slice(0, 4)).toEqual(distinctChannelHexes(4))
  })
  it('labels them distinct 1..n for the movie picker', () => {
    expect(distinctChannelOptions(2).map(o => o.label)).toEqual(['distinct 1', 'distinct 2'])
  })
})
