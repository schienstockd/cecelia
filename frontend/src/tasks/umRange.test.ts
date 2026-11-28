import { describe, it, expect } from 'vitest'
import { autoUmScale, finestPixelUm, umSliderRange, hasUmParam, parseUmScale, umAutoReason } from './umRange'
import { clampToSpec } from './paramValues'
import type { ParamDef } from './types'

const windowSize: ParamDef = { key: 'windowSize', label: 'Window size (µm)', type: 'float', unit: 'um',
                               min: 5, max: 500, step: 1, default: 50 }
const minCell: ParamDef = { key: 'minCellSize', label: 'Min cell size (µm²)', type: 'float', unit: 'um2',
                            min: 0, max: 1000, step: 1, default: 0 }

describe('autoUmScale', () => {
  it('keeps ×1 at or above the 2P reference and for an uncalibrated image', () => {
    expect(autoUmScale(0.3)).toBe(1)
    expect(autoUmScale(0.91)).toBe(1)
    expect(autoUmScale(null)).toBe(1)
  })
  it('scales SIM pixels (0.041 µm) to ×0.1 and confocal (0.16 µm) to ×0.5', () => {
    // 0.041 / 0.3 = 0.137 → nearest 1-2-5 in log is 0.1; 0.16 / 0.3 = 0.53 → 0.5
    expect(autoUmScale(0.041)).toBe(0.1)
    expect(autoUmScale(0.16)).toBe(0.5)
  })
})

describe('finestPixelUm', () => {
  it('takes the finest calibrated image and says when they differ', () => {
    const f = finestPixelUm([
      { physicalSizeX: 0.5, physicalSizeUnit: 'micrometer' },
      { physicalSizeX: 41, physicalSizeUnit: 'nm' },
      { physicalSizeX: null, physicalSizeUnit: null },
    ])
    expect(f?.um).toBeCloseTo(0.041)
    expect(f?.calibrated).toBe(2)
    expect(f?.distinct).toBe(true)
    expect(umAutoReason([{ physicalSizeX: 0.041, physicalSizeUnit: 'micrometer' }])).toBe('From 0.041 µm pixels')
  })
})

describe('umSliderRange', () => {
  it('is the spec range at ×1 and for a param without a unit', () => {
    expect(umSliderRange(windowSize, 1)).toEqual({ min: 5, max: 500, step: 1 })
    expect(umSliderRange({ ...windowSize, unit: undefined }, 0.1)).toEqual({ min: 5, max: 500, step: 1 })
  })
  it('scales a length linearly: SIM window 0.5–50 µm in 0.1 steps', () => {
    expect(umSliderRange(windowSize, 0.1)).toEqual({ min: 0.5, max: 50, step: 0.1 })
  })
  it('scales an area by the square', () => {
    expect(umSliderRange(minCell, 0.1)).toEqual({ min: 0, max: 10, step: 0.01 })
  })
  it('keeps whole-number steps on an int param', () => {
    const gap: ParamDef = { key: 'marginUm', label: 'Edge margin (µm)', type: 'int', unit: 'um',
                            min: 0, max: 1000, step: 10, default: 50 }
    expect(umSliderRange(gap, 0.05)).toEqual({ min: 0, max: 50, step: 1 })
  })
})

describe('form helpers', () => {
  it('finds a unit param inside a section', () => {
    expect(hasUmParam([{ key: 's', label: 'S', type: 'section', params: [windowSize] }])).toBe(true)
    expect(hasUmParam([{ key: 'n', label: 'N', type: 'int' }])).toBe(false)
  })
  it('reads back only a factor on offer', () => {
    expect(parseUmScale('0.1')).toBe(0.1)
    expect(parseUmScale('0.3')).toBeNull()
    expect(parseUmScale(null)).toBeNull()
  })
})

describe('clampToSpec on a unit param', () => {
  it('keeps a stored value below the 2P floor (a SIM window) and only fixes the sign', () => {
    expect(clampToSpec(windowSize, 0.4)).toBe(0.4)
    expect(clampToSpec(windowSize, 900)).toBe(900)
    expect(clampToSpec(windowSize, 0)).toBe(5)      // floor above 0: zero is not "off" here
    expect(clampToSpec(minCell, -1)).toBe(0)
    expect(clampToSpec({ ...windowSize, unit: undefined }, 0.4)).toBe(5)
  })
})
