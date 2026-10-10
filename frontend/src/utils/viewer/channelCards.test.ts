import { describe, it, expect } from 'vitest'
import { isCompact, toggleFlip, nextAllCompact, defaultCompact, COMPACT_ABOVE } from './channelCards'

describe('channel cards', () => {
  it('starts compact only on many-channel images', () => {
    expect(defaultCompact(4)).toBe(false)
    expect(defaultCompact(COMPACT_ABOVE)).toBe(false)
    expect(defaultCompact(COMPACT_ABOVE + 1)).toBe(true)
    expect(defaultCompact(38)).toBe(true)
  })

  it('a flipped channel goes against the section mode', () => {
    const f = new Set([2])
    expect(isCompact(false, f, 2)).toBe(true)
    expect(isCompact(false, f, 1)).toBe(false)
    expect(isCompact(true, f, 2)).toBe(false)
    expect(isCompact(true, f, 1)).toBe(true)
  })

  it('toggleFlip returns a new set and round-trips', () => {
    const a = new Set<number>()
    const b = toggleFlip(a, 3)
    expect(b).not.toBe(a)
    expect([...b]).toEqual([3])
    expect([...toggleFlip(b, 3)]).toEqual([])
  })

  it('the all button compacts unless every card already is compact', () => {
    expect(nextAllCompact(false, new Set(), 4)).toBe(true)
    expect(nextAllCompact(true, new Set(), 4)).toBe(false)
    // one card opened by hand in compact mode → the button compacts it again
    expect(nextAllCompact(true, new Set([1]), 4)).toBe(true)
    // every card closed by hand in expanded mode → the button expands
    expect(nextAllCompact(false, new Set([0, 1]), 2)).toBe(false)
  })
})
