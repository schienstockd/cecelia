import { describe, it, expect } from 'vitest'
import { cropSourceLine, type CropSource } from './cropSource'

const WHOLE = { x0: -1, x1: -1, y0: -1, y1: -1, z0: -1, z1: -1, t0: -1, t1: -1 }

describe('cropSourceLine', () => {
  const cs: CropSource = {
    uid: 'Dml3RG', valueName: 'denoised',
    box: { ...WHOLE, x0: 49, x1: 516, y0: 279, y1: 747 },
  }

  it('names the source + version and lists only the cropped axes', () => {
    expect(cropSourceLine(cs, 'M2b-MERTK')).toBe('Cropped from M2b-MERTK (denoised) · x 49–516 · y 279–747')
  })

  it('falls back to the uid when the source is not in the open project', () => {
    expect(cropSourceLine(cs, null)).toBe('Cropped from Dml3RG (denoised) · x 49–516 · y 279–747')
  })

  it('includes trimmed z / t and omits a missing version', () => {
    const zt: CropSource = { uid: 'abc', valueName: null, box: { ...WHOLE, z0: 2, z1: 8, t0: 10, t1: 40 } }
    expect(cropSourceLine(zt)).toBe('Cropped from abc · z 2–8 · t 10–40')
  })

  it('is null for an image that is not a crop', () => {
    expect(cropSourceLine(null)).toBeNull()
    expect(cropSourceLine(undefined)).toBeNull()
  })
})
