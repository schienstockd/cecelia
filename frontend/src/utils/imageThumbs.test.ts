import { describe, expect, it } from 'vitest'
import { allShown, parseShown, serializeShown, thumbCacheKey, thumbsStorageKey, toggleAllShown, toggleShown } from './imageThumbs'

describe('image table thumbnails — which rows show one', () => {
  it('round-trips through storage, and reads junk as nothing shown', () => {
    const s = new Set(['b', 'a'])
    expect(parseShown(serializeShown(s))).toEqual(s)
    expect(serializeShown(s)).toBe('["a","b"]')
    expect(parseShown(null).size).toBe(0)
    expect(parseShown('not json').size).toBe(0)
    expect(parseShown('{"a":1}').size).toBe(0)
    expect([...parseShown('["a", 3, null]')]).toEqual(['a'])
  })

  it('flips one row without touching the others', () => {
    const s = new Set(['a'])
    const on = toggleShown(s, 'b')
    expect([...on].sort()).toEqual(['a', 'b'])
    expect(s.has('b')).toBe(false)          // a new set — the ref re-renders
    expect([...toggleShown(on, 'a')]).toEqual(['b'])
  })

  it('the header button shows all unless all are shown, and leaves other sets alone', () => {
    const other = 'x-other-set'
    const some = new Set(['a', other])
    const all = toggleAllShown(some, ['a', 'b', 'c'])
    expect(allShown(all, ['a', 'b', 'c'])).toBe(true)
    expect(all.has(other)).toBe(true)
    const none = toggleAllShown(all, ['a', 'b', 'c'])
    expect([...none]).toEqual([other])
    expect(allShown(new Set(), [])).toBe(false)
    expect(toggleAllShown(new Set(), []).size).toBe(0)
  })

  it('keys storage per project and the cache per version', () => {
    expect(thumbsStorageKey('P1')).not.toBe(thumbsStorageKey('P2'))
    expect(thumbCacheKey('P', 'i', 'default')).not.toBe(thumbCacheKey('P', 'i', 'corrected'))
    expect(thumbCacheKey('P', 'i', 'default', 0)).not.toBe(thumbCacheKey('P', 'i', 'default', 1))
  })
})
