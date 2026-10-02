import { describe, it, expect } from 'vitest'
import { parseAuthorStamp, authorLabel, authorSummary, profileLabel } from './authorStamp'

describe('authorStamp', () => {
  it('parses the server stamp and drops junk', () => {
    expect(parseAuthorStamp({ profile: 'alice', via: 'claude' })).toEqual({ profile: 'alice', via: 'claude' })
    expect(parseAuthorStamp({ profile: 'alice', via: 'weird' })).toEqual({ profile: 'alice', via: 'app' })
    expect(parseAuthorStamp({ via: 'app' })).toBeUndefined()
    expect(parseAuthorStamp(null)).toBeUndefined()
  })

  it('names people only when there is more than the default profile', () => {
    expect(authorLabel(undefined)).toBe('')
    expect(authorLabel({ profile: 'default', via: 'app' })).toBe('')
    expect(authorLabel({ profile: 'alice', via: 'app' })).toBe('alice')
    expect(authorLabel({ profile: 'default', via: 'claude' })).toBe('Claude')
    expect(authorLabel({ profile: 'alice', via: 'claude' })).toBe('Claude for alice')
  })

  it('summarises creator and last editor in one line', () => {
    const alice = { profile: 'alice', via: 'app' as const }
    const claude = { profile: 'alice', via: 'claude' as const }
    const dflt = { profile: 'default', via: 'app' as const }
    expect(authorSummary(alice, alice)).toBe('by alice')
    expect(authorSummary(claude, alice)).toBe('by Claude for alice · edited by alice')
    expect(authorSummary(undefined, alice)).toBe('edited by alice')
    expect(authorSummary(dflt, dflt)).toBe('')
    expect(authorSummary(alice)).toBe('by alice')
    expect([profileLabel('alice'), profileLabel('default'), profileLabel(undefined)]).toEqual(['alice', '', ''])
  })
})
