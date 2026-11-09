import { describe, it, expect } from 'vitest'
import { matchesQuery } from './textQuery'

describe('matchesQuery', () => {
  it('empty or blank query matches everything', () => {
    expect(matchesQuery(['T cells'], '')).toBe(true)
    expect(matchesQuery(['T cells'], '   ')).toBe(true)
  })
  it('is a case-insensitive substring match', () => {
    expect(matchesQuery(['Immunemap', 'XbeT2l'], 'immune')).toBe(true)
    expect(matchesQuery(['Immunemap', 'XbeT2l'], 'xbet')).toBe(true)
    expect(matchesQuery(['Immunemap'], 'spleen')).toBe(false)
  })
  it('requires every word, in any field, in any order', () => {
    const fields = ['Agent run 2026-10-06 01:59', 'HdcNGF', 'peanut']
    expect(matchesQuery(fields, 'peanut agent')).toBe(true)
    expect(matchesQuery(fields, 'agent spleen')).toBe(false)
  })
  it('skips missing fields', () => {
    expect(matchesQuery(['MERTK', null, undefined], 'mertk')).toBe(true)
    expect(matchesQuery([null, undefined], 'x')).toBe(false)
  })
})
