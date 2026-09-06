import { describe, it, expect } from 'vitest'
import { formatWhen } from './formatWhen'

const NOW = Date.parse('2026-08-15T12:00:00Z')

describe('formatWhen', () => {
  it('is empty for an undefined moment — a missing time renders as nothing, not "Invalid Date"', () => {
    expect(formatWhen(undefined, NOW)).toBe('')
  })

  it("carries a same-day moment as time only, so today's rows stay narrow", () => {
    const today = new Date(NOW - 3 * 60 * 60 * 1000)      // 3h ago, same UTC day as NOW
    const s = formatWhen(today, NOW)
    // formatting is locale-dependent — assert the SHAPE (no year, no month name), not exact text.
    // The AM/PM suffix is allowed — a 12-hour locale keeps it, but a same-day label still fits in
    // the narrow column it was designed for.
    expect(s).not.toMatch(/\d{4}/)                        // no year
    expect(s).not.toMatch(/Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec/i)   // no month token
    expect(s.length).toBeLessThanOrEqual(9)               // "14:32" / "07:00 pm"
  })

  it('adds day+month once the moment is on an earlier day this year', () => {
    const aWeekAgo = new Date(NOW - 7 * 24 * 60 * 60 * 1000)
    const s = formatWhen(aWeekAgo, NOW)
    expect(s).toMatch(/Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec/i)
    expect(s).not.toMatch(/202[0-9]/)                     // no year
  })

  it('shows the year once the moment crosses into a different year', () => {
    const lastYear = new Date(NOW - 400 * 24 * 60 * 60 * 1000)
    const s = formatWhen(lastYear, NOW)
    expect(s).toMatch(/202[0-9]/)
  })

  it("defaults now to the wall clock — a caller with no shared clock is fine", () => {
    // Same day as the wall clock → time-only shape
    expect(formatWhen(new Date())).not.toMatch(/\d{4}/)
    expect(formatWhen(new Date())).not.toMatch(/Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec/i)
  })
})
