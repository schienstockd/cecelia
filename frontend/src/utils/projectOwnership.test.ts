import { describe, expect, it } from 'vitest'
import { visibleToProfile } from './projectOwnership'

describe('visibleToProfile', () => {
  it('own and shared projects are visible, others are not', () => {
    expect(visibleToProfile({ uid: 'a', owners: ['alice'] }, 'alice')).toBe(true)
    expect(visibleToProfile({ uid: 'a', owners: ['bob', 'alice'] }, 'alice')).toBe(true)
    expect(visibleToProfile({ uid: 'a', owners: [] }, 'alice')).toBe(true)
    expect(visibleToProfile({ uid: 'a' }, 'alice')).toBe(true)
    expect(visibleToProfile({ uid: 'a', owners: ['bob'] }, 'alice')).toBe(false)
  })
  it('the open project is always visible', () => {
    expect(visibleToProfile({ uid: 'a', owners: ['bob'] }, 'alice', 'a')).toBe(true)
    expect(visibleToProfile({ uid: 'a', owners: ['bob'] }, 'alice', 'b')).toBe(false)
  })
})
