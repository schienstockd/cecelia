import { describe, it, expect } from 'vitest'
import { stripExt, timestampForFilename, screenshotFilename } from './viewerScreenshot'

describe('stripExt', () => {
  it('drops a short trailing extension only', () => {
    expect(stripExt('mouse5-day2.tif')).toBe('mouse5-day2')
    expect(stripExt('mouse5-day2.png')).toBe('mouse5-day2')
    expect(stripExt('mouse5.day2.tif')).toBe('mouse5.day2')  // interior dot survives
  })
  it('leaves names with no extension alone', () => {
    expect(stripExt('mouse5-day2')).toBe('mouse5-day2')
    expect(stripExt('')).toBe('')
  })
  it('does not strip a leading dot (dotfile)', () => {
    expect(stripExt('.hidden')).toBe('.hidden')
  })
  it('does not strip a long trailing token that just happens to have a dot', () => {
    // "wound-2026-09-18" — no extension, the dashes are the sortable timestamp
    expect(stripExt('wound-2026-09-18')).toBe('wound-2026-09-18')
    // long "extension" (>5 chars including the dot) means it was probably part of the name
    expect(stripExt('name.customlong')).toBe('name.customlong')
  })
})

describe('timestampForFilename', () => {
  it('formats as YYYY-MM-DD_HH-MM-SS, zero-padded, no colons', () => {
    const d = new Date(2026, 8, 18, 14, 5, 3)   // month is 0-indexed
    expect(timestampForFilename(d)).toBe('2026-09-18_14-05-03')
    expect(timestampForFilename(d)).not.toContain(':')
  })
  it('handles single-digit fields', () => {
    const d = new Date(2026, 0, 1, 1, 1, 1)
    expect(timestampForFilename(d)).toBe('2026-01-01_01-01-01')
  })
})

describe('screenshotFilename', () => {
  const now = new Date(2026, 8, 18, 14, 5, 3)
  it('joins the image name and the timestamp with `.png`', () => {
    expect(screenshotFilename('mouse5-day2', now)).toBe('mouse5-day2-2026-09-18_14-05-03.png')
  })
  it('strips a source extension so the output has ONE .png', () => {
    expect(screenshotFilename('mouse5-day2.tif', now)).toBe('mouse5-day2-2026-09-18_14-05-03.png')
  })
  it('falls back to `viewer` on empty / null / whitespace', () => {
    expect(screenshotFilename('', now)).toBe('viewer-2026-09-18_14-05-03.png')
    expect(screenshotFilename(null, now)).toBe('viewer-2026-09-18_14-05-03.png')
    expect(screenshotFilename('   ', now)).toBe('viewer-2026-09-18_14-05-03.png')
  })
})
