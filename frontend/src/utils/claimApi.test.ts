import { describe, it, expect, vi, afterEach } from 'vitest'
import { claimPopRef, reviewClaim, claimScopeOf, claimUnitsOf, defaultUnitAttr, findingsOf, openCount, reviewByCategory, parseClaimResult } from './claimApi'

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
    // the replicate a panel kept: a list, or the single attribute an older panel stored
    expect(claimUnitsOf({ claimUnits: ['Mouse', 'Location'] })).toEqual(['Mouse', 'Location'])
    expect(claimUnitsOf({ claimUnit: 'Mouse' })).toEqual(['Mouse'])
    expect(claimUnitsOf({ claimUnit: '' })).toEqual([])
    expect(claimUnitsOf({})).toBeNull()
  })
  it('groups findings under their check, and counts open problems with the review', () => {
    const r = parseClaimResult({ entryId: 'bb-1', round: 2, text: 'T > B', ran: ['S0.data'],
      skipped: { 'S0.unit': 'no replicate attribute given' },
      checks: [{ id: 'S0.data', state: 'warn', text: '', tip: 't' }, { id: 'S0.unit', state: 'skipped', text: 'no replicate attribute given', tip: 't' },
               { id: 'S2.calibration', state: 'nonsense', text: 'Calibrated', tip: 't' }],
      findings: [
        { level: 'info', code: 'claim.unit_count', short: 'a', long: 'b', detail: { stage: 'S2', check: 'S2.unit' } },
        { level: 'warn', code: 'claim.no_data', short: 'a', long: 'b', sectionId: 's01', detail: { stage: 'S0', check: 'S0.data', evidence: { images: ['x'] } } },
        { level: 'warn', code: 'claim.few_units', short: 'a', long: 'b', sectionId: 's02', detail: { stage: 'S2', check: 'S2.unit' } },
      ],
      review: { turnId: 'kt-1', status: 'done', model: 'sonnet', claims: [
        { kind: 'observation', text: 'o', sectionId: 's03' }, { kind: 'question', text: 'q', sectionId: 's04' },
        { kind: 'interpretation', text: 'descriptive only' }] } })!
    expect(r.round).toBe(2)
    expect(r.findings[1].evidence).toEqual({ images: ['x'] })
    expect(r.checks.map(l => l.state)).toEqual(['warn', 'skipped', 'ok'])   // an unknown state reads as ok
    expect(findingsOf(r, 'S2.unit').map(f => f.code)).toEqual(['claim.few_units', 'claim.unit_count'])  // warnings first
    expect(r.review?.items.map(x => x.text)).toEqual(['o', 'q'])
    expect(r.review?.supports).toBe('descriptive only')
    expect(openCount(r)).toBe(4)            // 2 warnings + 2 review items; info doesn't count
    expect(openCount(null)).toBe(0)
    expect(parseClaimResult({ entryId: 'bb-2' })!.review).toBeNull()
  })
  it('reads categories and files the review points under them, in reading order', () => {
    const r = parseClaimResult({ entryId: 'bb-3',
      categories: [{ id: 'images', label: 'Images', tip: 't', state: 'warn' }, { id: 'units', label: 'Units', tip: 't', state: 'odd' }],
      categoryLabels: { images: 'Images', replicates: 'Replicates', data: 'The data', other: 'Other' },
      checks: [{ id: 'S0.data', category: 'images', state: 'warn', text: '', tip: '' }],
      review: { turnId: 'kt', claims: [
        { kind: 'observation', category: 'data', text: 'B tracks are short', action: 'set a minimum duration', sectionId: 's08' },
        { kind: 'question', category: 'images', text: 'why is i1 out?', sectionId: 's09' },
        { kind: 'observation', category: 'mystery', text: 'x', sectionId: 's10' }] } })!
    expect(r.categories.map(c => c.state)).toEqual(['warn', 'ok'])
    expect(r.checks[0].category).toBe('images')
    const g = reviewByCategory(r)
    expect(g.map(x => x.label)).toEqual(['Images', 'The data', 'mystery'])   // order of the labels; unknown last
    expect(g[1].items[0].action).toBe('set a minimum duration')
    expect(r.review?.items[1].action).toBe('')
    expect(r.review?.lastError).toBe('')
    const f = parseClaimResult({ entryId: 'bb-4',
      review: { status: 'failed', model: 'sonnet', claims: [], lastError: 'You have hit your limit' } })!
    expect(f.review?.model).toBe('sonnet')
    expect(f.review?.lastError).toBe('You have hit your limit')
  })
  it('rejects a reply that is not a claim result', () => {
    expect(parseClaimResult(null)).toBeNull()
    expect(parseClaimResult({ error: 'x' })).toBeNull()
  })
  afterEach(() => { vi.unstubAllGlobals() })
  it('runs the review on the model picked for Kiwi', async () => {
    const fetch = vi.fn(async (_u: string, _init: { body: string }) => ({ ok: true, status: 200, json: async () => ({ turnId: 'kt-1' }) }))
    vi.stubGlobal('fetch', fetch)
    expect(await reviewClaim('p1', 'bb-1', 'opus')).toBe('kt-1')
    expect(JSON.parse(fetch.mock.calls[0][1].body)).toEqual({ projectUid: 'p1', entryId: 'bb-1', model: 'opus' })
  })
})
