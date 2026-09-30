import { describe, it, expect } from 'vitest'
import { formatAgo } from './formatAgo'

describe('formatAgo', () => {
  const now = new Date('2026-09-19T12:00:00Z')
  it('recent → "just now"', () => {
    expect(formatAgo('2026-09-19T11:59:30Z', now)).toBe('just now')
  })
  it('minutes', () => {
    expect(formatAgo('2026-09-19T11:55:00Z', now)).toBe('5m')
  })
  it('hours', () => {
    expect(formatAgo('2026-09-19T10:00:00Z', now)).toBe('2h')
  })
  it('days', () => {
    expect(formatAgo('2026-09-16T12:00:00Z', now)).toBe('3d')
  })
  it('empty on unparseable / future / missing', () => {
    expect(formatAgo('', now)).toBe('')
    expect(formatAgo('not a date', now)).toBe('')
    expect(formatAgo('2026-09-19T13:00:00Z', now)).toBe('') // future ⇒ empty (clock skew, not "in the future")
  })
})
