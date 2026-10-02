import { describe, it, expect } from 'vitest'
import { parseAuthorStamp, authorLabel } from './authorStamp'

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
})
