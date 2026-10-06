import { describe, it, expect } from 'vitest'
import { parseSectionOutcomes, sectionVerdictReady, SECTION_CAUSES, SECTION_CAUSE_OPTIONS } from './blackboardApi'

// GUIDE_RUNS_PLAN Decision 2 — a `bad` section verdict on a run record names its cause.
describe('section verdict cause', () => {
  it('reads a cause, and a bad stored before causes existed as "not set"', () => {
    const so = parseSectionOutcomes({
      d01: { verdict: 'bad', note: 'no AF step', cause: 'guide', at: 't', by: { via: 'app' } },
      d02: { verdict: 'bad', note: 'old', at: 't' },
      d03: { verdict: 'bad', note: 'x', cause: 'luck', at: 't' },
      d04: { verdict: 'good', note: '', at: 't' },
    })!
    expect(so.d01.cause).toBe('guide')
    expect(so.d02).toEqual({ verdict: 'bad', note: 'old', at: 't' })
    expect(so.d03.cause).toBeUndefined()           // not in the closed set
    expect(so.d04.cause).toBeUndefined()
  })

  it('a bad needs a note, and on a run record a cause', () => {
    expect(sectionVerdictReady(null, 'x', 'guide', true)).toBe(false)
    expect(sectionVerdictReady('good', '', null, true)).toBe(true)
    expect(sectionVerdictReady('unsure', '', null, true)).toBe(true)
    expect(sectionVerdictReady('bad', '  ', 'guide', true)).toBe(false)
    expect(sectionVerdictReady('bad', 'why', null, true)).toBe(false)
    expect(sectionVerdictReady('bad', 'why', 'agent', true)).toBe(true)
    expect(sectionVerdictReady('bad', 'why', null, false)).toBe(true)   // an ordinary note's section
  })

  it('offers every cause once, each with a tooltip', () => {
    expect(SECTION_CAUSE_OPTIONS.map(o => o.value)).toEqual([...SECTION_CAUSES])
    for (const o of SECTION_CAUSE_OPTIONS) expect(o.tip.length).toBeGreaterThan(0)
  })
})
