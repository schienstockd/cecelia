import { describe, it, expect } from 'vitest'
import { napariColormapHex } from './napariColormap'

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
