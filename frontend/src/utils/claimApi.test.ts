import { describe, it, expect } from 'vitest'
import { claimPopRef, claimScopeOf, defaultUnitAttr, stageCounts, skippedLine, parseClaimResult } from './claimApi'

const s = (valueName: string, pop: string) => ({ valueName, pop, popType: 'flow' })

describe('claimApi', () => {
  it('names a series the way the claim checks do', () => {
    expect(claimPopRef(s('B', '/qc'))).toBe('B/qc')
    expect(claimPopRef(s('T', 'qc/_tracked'))).toBe('T/qc/_tracked')
    expect(claimPopRef(s('B', '/'))).toBe('B')
  })
  it('only a set-wide plot with a measure is a claim scope; pops are deduplicated', () => {
    expect(claimScopeOf({ setUid: null, series: [s('B', '/qc')], measure: 'live.track.speed' })).toBeNull()
    expect(claimScopeOf({ setUid: 'X', series: [], measure: 'live.track.speed' })).toBeNull()
    expect(claimScopeOf({ setUid: 'X', series: [s('B', '/qc')], measure: '' })).toBeNull()
    expect(claimScopeOf({ setUid: 'X', series: [s('B', '/qc'), s('B', '/qc'), s('T', '/qc')], measure: 'm' }))
      .toEqual({ setUid: 'X', populations: ['B/qc', 'T/qc'], measure: 'm' })
  })
  it('defaults the unit to the attribute naming the animal', () => {
    expect(defaultUnitAttr(['Location', 'Mouse', 'Treatment'])).toBe('Mouse')
    expect(defaultUnitAttr(['Treatment', 'animal_id'])).toBe('animal_id')
    expect(defaultUnitAttr(['Mousetrap', 'Location'])).toBe('Mousetrap')   // no exact match: first
    expect(defaultUnitAttr([])).toBe('')
  })
  it('counts warnings per stage, not info', () => {
    const r = parseClaimResult({ entryId: 'bb-1', round: 2, ran: ['S0.data'], skipped: { 'S0.unit': 'no unit attribute given' },
      findings: [
        { level: 'warn', code: 'claim.no_data', short: 'a', long: 'b', sectionId: 's01', detail: { stage: 'S0', evidence: { images: ['x'] } } },
        { level: 'warn', code: 'claim.few_units', short: 'a', long: 'b', sectionId: 's02', detail: { stage: 'S2' } },
        { level: 'info', code: 'claim.unit_count', short: 'a', long: 'b', detail: { stage: 'S2' } },
      ] })!
    expect(r.round).toBe(2)
    expect(stageCounts(r.findings)).toEqual({ S0: 1, S1: 0, S2: 1 })
    expect(r.findings[0].evidence).toEqual({ images: ['x'] })
    expect(skippedLine(r.skipped)).toBe('Not checked: S0.unit (no unit attribute given)')
    expect(skippedLine({})).toBe('')
  })
  it('rejects a reply that is not a claim result', () => {
    expect(parseClaimResult(null)).toBeNull()
    expect(parseClaimResult({ error: 'x' })).toBeNull()
  })
})
