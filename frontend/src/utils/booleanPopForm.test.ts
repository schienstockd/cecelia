import { describe, it, expect } from 'vitest'
import { termsToSpec, specToTerms, booleanSpecValid, booleanSummary } from './booleanPopForm'

const leaf = (p: string) => p.slice(p.lastIndexOf('/') + 1)

describe('boolean pop form', () => {
  it('splits terms into included and excluded, and back', () => {
    const terms = [{ path: '/gfp+', negate: false }, { path: '/qc/tom+', negate: false },
                   { path: '/kat+', negate: true }]
    const spec = termsToSpec('and', terms)
    expect(spec).toEqual({ op: 'and', pops: ['/gfp+', '/qc/tom+'], not: ['/kat+'] })
    // round-trip: included first, then excluded (the order the form re-renders in)
    expect(specToTerms(spec)).toEqual(terms)
  })

  it('drops empty rows and rejects an empty combination', () => {
    expect(termsToSpec('or', [{ path: '', negate: false }])).toEqual({ op: 'or', pops: [], not: [] })
    expect(booleanSpecValid({ op: 'or', pops: [], not: [] })).toBe(false)
    // no included term is legitimate — that IS the plain not-gate
    expect(booleanSpecValid({ op: 'and', pops: [], not: ['/kat+'] })).toBe(true)
  })

  it('summarises the combination the way it reads', () => {
    expect(booleanSummary({ op: 'or', pops: ['/gfp+', '/tom+'], not: [] }, leaf)).toBe('gfp+ or tom+')
    expect(booleanSummary({ op: 'and', pops: ['/gfp+', '/tom+'], not: ['/qc/kat+'] }, leaf))
      .toBe('gfp+ and tom+, not kat+')
    expect(booleanSummary({ op: 'and', pops: [], not: ['/kat+'] }, leaf)).toBe('not kat+')
    expect(booleanSummary(undefined, leaf)).toBe('')
  })
})
