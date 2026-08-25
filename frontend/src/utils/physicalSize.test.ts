import { describe, it, expect } from 'vitest'
import { shortUnit, fmtNum, formatPhysicalSize } from './physicalSize'

describe('shortUnit', () => {
  it('collapses OME micron variants to µm', () => {
    // OME sometimes spells it out; the cell/modal calibration line needs a symbol that fits
    for (const u of ['micrometer', 'micrometre', 'micron', 'microns', 'MICROMETER', 'Microns']) {
      expect(shortUnit(u)).toBe('µm')
    }
  })

  it('keeps any other unit verbatim — never silently mislabels an nm image', () => {
    expect(shortUnit('nm')).toBe('nm')
    expect(shortUnit('mm')).toBe('mm')
    expect(shortUnit('px')).toBe('px')
  })

  it('defaults to µm when the unit is null/undefined/empty', () => {
    expect(shortUnit(null)).toBe('µm')
    expect(shortUnit(undefined)).toBe('µm')
    expect(shortUnit('')).toBe('µm')
  })
})

describe('fmtNum', () => {
  it('caps at 3 decimals AND strips trailing zeros', () => {
    // the whole reason this exists — the modal was printing 0.3459441507762987
    expect(fmtNum(0.3459441507762987)).toBe('0.346')
    expect(fmtNum(0.9965434999999999)).toBe('0.997')
  })

  it('reads a round number as itself, not as 1.000', () => {
    expect(fmtNum(1)).toBe('1')
    expect(fmtNum(2.5)).toBe('2.5')
    expect(fmtNum(0)).toBe('0')
  })
})

describe('formatPhysicalSize', () => {
  it('composes trimmed number + short unit', () => {
    expect(formatPhysicalSize(0.3459441507762987, 'micrometer')).toBe('0.346 µm')
    expect(formatPhysicalSize(500, 'nm')).toBe('500 nm')
  })

  it('returns the em-dash fallback for a null/undefined value', () => {
    // an image with no Z depth reports null there; the modal shows `—` not `null µm`
    expect(formatPhysicalSize(null, 'micrometer')).toBe('—')
    expect(formatPhysicalSize(undefined, 'nm')).toBe('—')
  })

  it('accepts a custom fallback (e.g. the tooltip using `?`)', () => {
    expect(formatPhysicalSize(null, 'micrometer', '?')).toBe('?')
  })
})
