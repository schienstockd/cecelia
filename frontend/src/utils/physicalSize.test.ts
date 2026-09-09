import { describe, it, expect } from 'vitest'
import { shortUnit, shortTimeUnit, fmtNum, formatPhysicalSize, formatTimeIncrement } from './physicalSize'

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

describe('shortTimeUnit', () => {
  it('collapses OME long forms to symbols', () => {
    expect(shortTimeUnit('second')).toBe('s')
    expect(shortTimeUnit('Seconds')).toBe('s')
    expect(shortTimeUnit('SEC')).toBe('s')
    expect(shortTimeUnit('millisecond')).toBe('ms')
    expect(shortTimeUnit('minute')).toBe('min')
    expect(shortTimeUnit('hour')).toBe('h')
  })

  it('defaults to s when the unit is null/undefined/empty', () => {
    expect(shortTimeUnit(null)).toBe('s')
    expect(shortTimeUnit(undefined)).toBe('s')
    expect(shortTimeUnit('')).toBe('s')
  })

  it('keeps unrecognised units verbatim', () => {
    expect(shortTimeUnit('frame')).toBe('frame')
  })
})

describe('formatTimeIncrement', () => {
  it('produces "<n> <shortTimeUnit>" — the readout the ImageTable cell was missing the space in', () => {
    expect(formatTimeIncrement(30.26, 'second')).toBe('30.26 s')
    expect(formatTimeIncrement(500, 'millisecond')).toBe('500 ms')
    expect(formatTimeIncrement(30, null)).toBe('30 s')
  })

  it('falls back for null/undefined', () => {
    expect(formatTimeIncrement(null, 'second')).toBe('—')
    expect(formatTimeIncrement(undefined, 'second')).toBe('—')
  })
})
