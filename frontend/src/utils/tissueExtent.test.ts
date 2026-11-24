import { describe, it, expect } from 'vitest'
import { tissueExtent, extentShort, formatLength, formatArea, formatVolume, sig3 } from './tissueExtent'

describe('tissueExtent', () => {
  it('multiplies pixel counts by pixel size; Z is planes × step', () => {
    // 1000 × 500 px at 0.5 µm, 10 planes at 2 µm → 500 × 250 × 20 µm
    const e = tissueExtent({ sizeX: 1000, sizeY: 500, sizeZ: 10 },
      { physicalSizeX: 0.5, physicalSizeY: 0.5, physicalSizeZ: 2, physicalSizeUnit: 'micrometer' })
    expect(e.um).toEqual({ x: 500, y: 250, z: 20 })
    expect(e.areaUm2).toBe(125_000)
    expect(e.volumeUm3).toBe(2_500_000)
  })

  it('converts a nm / mm pixel unit to µm', () => {
    const e = tissueExtent({ sizeX: 100, sizeY: 100 }, { physicalSizeX: 500, physicalSizeY: 500, physicalSizeUnit: 'nm' })
    expect(e.um.x).toBeCloseTo(50)
  })

  it('a single plane has no depth and no volume, but an area', () => {
    const e = tissueExtent({ sizeX: 2000, sizeY: 2000, sizeZ: 1 }, { physicalSizeX: 1, physicalSizeY: 1, physicalSizeZ: 1 })
    expect(e.um.z).toBeNull()
    expect(e.volumeUm3).toBeNull()
    expect(e.areaUm2).toBe(4e6)
  })

  it('no pixel size → pixel counts only', () => {
    const e = tissueExtent({ sizeX: 512, sizeY: 256, sizeZ: 3 }, {})
    expect(e.um).toEqual({ x: null, y: null, z: null })
    expect(e.areaUm2).toBeNull()
    expect(extentShort(e)).toBe('512 × 256 × 3 px')
  })

  it('a stack without a Z step keeps X/Y and area, drops volume', () => {
    const e = tissueExtent({ sizeX: 100, sizeY: 100, sizeZ: 5 }, { physicalSizeX: 1, physicalSizeY: 1 })
    expect(e.um.z).toBeNull()
    expect(e.areaUm2).toBe(10_000)
    expect(e.volumeUm3).toBeNull()
  })
})

describe('formatting', () => {
  it('sig3 keeps three significant figures', () => {
    expect(sig3(2563.2)).toBe('2560')
    expect(sig3(0.03912)).toBe('0.0391')
    expect(sig3(1)).toBe('1')
  })
  it('lengths switch to mm at 1 mm', () => {
    expect(formatLength(640)).toBe('640 µm')
    expect(formatLength(2563)).toBe('2.56 mm')
  })
  it('area in mm², volume in mm³ or µm³ when tiny', () => {
    expect(formatArea(6.5e6)).toBe('6.5 mm²')
    expect(formatVolume(2.6e8)).toBe('0.26 mm³')
    expect(formatVolume(3.68e6)).toBe('3.68×10⁶ µm³')
  })
  it('the short cell uses one unit for all three axes', () => {
    const big = tissueExtent({ sizeX: 5000, sizeY: 5000, sizeZ: 13 },
      { physicalSizeX: 0.507, physicalSizeY: 0.507, physicalSizeZ: 3 })
    expect(extentShort(big)).toBe('2.54 × 2.54 × 0.039 mm')
    const small = tissueExtent({ sizeX: 512, sizeY: 512 }, { physicalSizeX: 0.6, physicalSizeY: 0.6 })
    expect(extentShort(small)).toBe('307 × 307 µm')
  })
})
