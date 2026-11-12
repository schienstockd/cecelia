import { describe, it, expect } from 'vitest'
import { fmtContrast, fmtContrastWindow } from './contrastReadout'

describe('fmtContrast', () => {
  it('rounds auto-contrast float noise to an integer', () => {
    expect(fmtContrast(6100.098000000027)).toBe('6100')
    expect(fmtContrast(4656.979000000005)).toBe('4657')
  })
  it('shortens five-digit values to thousands', () => {
    expect(fmtContrast(13415.998000000254)).toBe('13.4k')
    expect(fmtContrast(65535)).toBe('65.5k')
    expect(fmtContrast(20000)).toBe('20k')
  })
  it('keeps 3 significant digits for float-image values', () => {
    expect(fmtContrast(0.123456)).toBe('0.123')
    expect(fmtContrast(2.5)).toBe('2.5')
    expect(fmtContrast(0)).toBe('0')
  })
  it('formats the window', () => {
    expect(fmtContrastWindow(2, 13415.998000000254)).toBe('2–13.4k')
  })
})
