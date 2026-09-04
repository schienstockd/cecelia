import { describe, it, expect } from 'vitest'
import { elapsedLabel, niceScaleBar } from './stillOverlay'

describe('elapsedLabel', () => {
  it('formats seconds → m / h m', () => {
    expect(elapsedLabel(0, 30, 'second')).toBe('0m')
    expect(elapsedLabel(10, 30, 'second')).toBe('5m')       // 300 s
    expect(elapsedLabel(240, 30, 'second')).toBe('2h 0m')   // 7200 s
  })
  it('respects a minutes interval unit', () => {
    expect(elapsedLabel(3, 2, 'min')).toBe('6m')            // 3 × 2 min
    expect(elapsedLabel(90, 1, 'minutes')).toBe('1h 30m')
  })
  it('falls back to t{N} when the interval is unknown, and "" with no timepoint', () => {
    expect(elapsedLabel(4, undefined)).toBe('t4')
    expect(elapsedLabel(null, 30)).toBe('')
    expect(elapsedLabel(undefined, 30)).toBe('')
  })
})

describe("elapsedLabel, clock style — what viewer's overlay shows", () => {
  it('is H:MM:SS, zero-padded, so the two read alike side by side', () => {
    expect(elapsedLabel(0, 30, 'second', 'clock')).toBe('0:00:00')
    expect(elapsedLabel(10, 30, 'second', 'clock')).toBe('0:05:00')     // 300 s
    expect(elapsedLabel(240, 30, 'second', 'clock')).toBe('2:00:00')    // 7200 s
    expect(elapsedLabel(7, 90, 'second', 'clock')).toBe('0:10:30')      // 630 s — seconds survive
  })
  it('takes the interval in minutes too, which is what the image accessor reports', () => {
    expect(elapsedLabel(99, 0.3167, 'min', 'clock')).toBe('0:31:21')
  })
  it("falls back to viewer's own wording when the interval is unknown", () => {
    expect(elapsedLabel(4, undefined, 'min', 'clock')).toBe('t = 4')
    expect(elapsedLabel(null, 30, 'min', 'clock')).toBe('')
  })
})

describe('niceScaleBar', () => {
  it('picks the largest nice step within maxFraction of the extent', () => {
    // extent 500 µm, 30% = 150 → largest nice ≤ 150 is 100
    expect(niceScaleBar(500)).toEqual({ um: 100, label: '100 µm' })
    // extent 40 µm, 30% = 12 → largest nice ≤ 12 is 10
    expect(niceScaleBar(40)).toEqual({ um: 10, label: '10 µm' })
  })
  it('rolls µm up to mm at/above 1000', () => {
    // extent 8000 µm, 30% = 2400 → 2000 µm → "2 mm"
    expect(niceScaleBar(8000)).toEqual({ um: 2000, label: '2 mm' })
  })
  it('keeps a non-micron unit as given', () => {
    expect(niceScaleBar(500, 'nm')).toEqual({ um: 100, label: '100 nm' })
  })

  it("reads OME's own spelling of microns, which is the word 'micrometer'", () => {
    // `PhysicalSizeUnit` comes out of OME-XML as "micrometer". Matching only 'micron' labelled the bar
    // "100 micrometer" and skipped the mm roll-up entirely.
    for (const u of ['micrometer', 'micrometre', 'micron', 'µm', 'um', 'Micrometer']) {
      expect(niceScaleBar(500, u)).toEqual({ um: 100, label: '100 µm' })
      expect(niceScaleBar(8000, u)).toEqual({ um: 2000, label: '2 mm' })
    }
  })
  it('returns null for a missing / too-small extent', () => {
    expect(niceScaleBar(0)).toBeNull()
    expect(niceScaleBar(null)).toBeNull()
    expect(niceScaleBar(2)).toBeNull()          // 30% = 0.6, smaller than the smallest step (1)
  })
})
