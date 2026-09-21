import { describe, it, expect } from 'vitest'
import { filterEntries, entryPassesFilters } from './blackboardFilters'
import type { BlackboardEntrySummary } from './blackboardApi'

// Compact fixture — the two filter dimensions across four representative rows plus the profile.
// PROJECT_MEMORY_PLAN Decisions 3 (status), 11 (outcome), 2 (profile always visible).
const rows: BlackboardEntrySummary[] = [
  { entryId: 'profile', title: 'Project profile', current: 0, updatedAt: 'x',
    attachmentsCount: 0, status: 'open' },
  { entryId: 'bb-1', title: 'A open + bad', current: 0, updatedAt: 'x',
    attachmentsCount: 0, status: 'open',
    outcome: { verdict: 'bad', note: 'why', taggedAt: '' } },
  { entryId: 'bb-2', title: 'B open + good', current: 0, updatedAt: 'x',
    attachmentsCount: 0, status: 'open',
    outcome: { verdict: 'good', note: 'why', taggedAt: '' } },
  { entryId: 'bb-3', title: 'C resolved + untagged', current: 0, updatedAt: 'x',
    attachmentsCount: 0, status: 'resolved' },
  { entryId: 'bb-4', title: 'D parked + untagged', current: 0, updatedAt: 'x',
    attachmentsCount: 0, status: 'parked' },
]

describe('blackboardFilters — profile always survives', () => {
  it('any status filter keeps the profile row', () => {
    for (const s of ['all', 'open', 'resolved', 'parked'] as const) {
      const out = filterEntries(rows, s, 'all')
      expect(out.some(r => r.entryId === 'profile')).toBe(true)
    }
  })
  it('any outcome filter keeps the profile row', () => {
    for (const o of ['all', 'untagged', 'good', 'bad'] as const) {
      const out = filterEntries(rows, 'all', o)
      expect(out.some(r => r.entryId === 'profile')).toBe(true)
    }
  })
})

describe('blackboardFilters — status', () => {
  it('all passes every row', () => {
    expect(filterEntries(rows, 'all', 'all')).toHaveLength(5)
  })
  it('resolved keeps only bb-3 + profile', () => {
    const ids = filterEntries(rows, 'resolved', 'all').map(r => r.entryId).sort()
    expect(ids).toEqual(['bb-3', 'profile'])
  })
  it('parked keeps only bb-4 + profile', () => {
    const ids = filterEntries(rows, 'parked', 'all').map(r => r.entryId).sort()
    expect(ids).toEqual(['bb-4', 'profile'])
  })
})

describe('blackboardFilters — outcome', () => {
  it('bad keeps only bb-1 + profile', () => {
    const ids = filterEntries(rows, 'all', 'bad').map(r => r.entryId).sort()
    expect(ids).toEqual(['bb-1', 'profile'])
  })
  it('good keeps only bb-2 + profile', () => {
    const ids = filterEntries(rows, 'all', 'good').map(r => r.entryId).sort()
    expect(ids).toEqual(['bb-2', 'profile'])
  })
  it('untagged keeps only rows with no outcome + profile', () => {
    const ids = filterEntries(rows, 'all', 'untagged').map(r => r.entryId).sort()
    expect(ids).toEqual(['bb-3', 'bb-4', 'profile'])
  })
})

describe('blackboardFilters — combined', () => {
  it('open + good keeps only bb-2 + profile (bb-1 is bad, bb-3/4 are wrong status)', () => {
    const ids = filterEntries(rows, 'open', 'good').map(r => r.entryId).sort()
    expect(ids).toEqual(['bb-2', 'profile'])
  })
  it('resolved + bad ⇒ only profile (bb-1 is open, bb-3 is untagged)', () => {
    const ids = filterEntries(rows, 'resolved', 'bad').map(r => r.entryId).sort()
    expect(ids).toEqual(['profile'])
  })
})

describe('entryPassesFilters — single-row API', () => {
  it('a bb entry matching neither filter drops', () => {
    expect(entryPassesFilters(rows[3], 'open', 'all')).toBe(false)   // resolved row, open filter
    expect(entryPassesFilters(rows[3], 'all', 'bad')).toBe(false)    // untagged row, bad filter
  })
  it('profile passes even against the strictest combo', () => {
    expect(entryPassesFilters(rows[0], 'parked', 'bad')).toBe(true)
  })
})
