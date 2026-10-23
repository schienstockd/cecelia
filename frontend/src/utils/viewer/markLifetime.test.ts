import { describe, it, expect } from 'vitest'
import { DEFAULT_MARK_TTL_S, markTtlSeconds, expiresAtFrom, msUntil, isLive, liveOrNull,
         clearsLinkedSelection, assistantMarkOn } from './markLifetime'

describe('markTtlSeconds', () => {
  it('takes the frame TTL, else the server default', () => {
    expect(markTtlSeconds(30)).toBe(30)
    expect(markTtlSeconds('45')).toBe(45)
    expect(markTtlSeconds(undefined)).toBe(DEFAULT_MARK_TTL_S)
    expect(markTtlSeconds(null)).toBe(DEFAULT_MARK_TTL_S)
  })
  it('falls back on a TTL that would never fire or fire at once', () => {
    expect(markTtlSeconds('abc')).toBe(DEFAULT_MARK_TTL_S)
    expect(markTtlSeconds(0)).toBe(DEFAULT_MARK_TTL_S)
    expect(markTtlSeconds(-5)).toBe(DEFAULT_MARK_TTL_S)
    expect(markTtlSeconds(Infinity)).toBe(DEFAULT_MARK_TTL_S)
  })
})

describe('expiry arithmetic', () => {
  it('expires ttl seconds after it lands', () => {
    expect(expiresAtFrom(300, 1_000)).toBe(301_000)
  })
  it('never schedules a negative delay', () => {
    expect(msUntil(5_000, 2_000)).toBe(3_000)
    expect(msUntil(1_000, 2_000)).toBe(0)
  })
})

describe('isLive / liveOrNull', () => {
  // The regression in #1733: a mark_cells pick was persisted with no expiry and came back on every
  // reload. Seeding through liveOrNull drops it once its TTL has passed.
  const claudePick = { labels: [1, 2], origin: 'claude' as const, expiresAt: 10_000 }
  it('keeps an assistant mark until its expiry, then drops it', () => {
    expect(liveOrNull(claudePick, 9_999)).toBe(claudePick)
    expect(liveOrNull(claudePick, 10_000)).toBeNull()
    expect(isLive(claudePick, 20_000)).toBe(false)
  })
  it("never expires a user's own highlight (no expiresAt)", () => {
    const userPick: { labels: number[]; origin: 'user'; expiresAt?: number } = { labels: [3], origin: 'user' }
    expect(liveOrNull(userPick, Number.MAX_SAFE_INTEGER)).toBe(userPick)
  })
  it('passes null through', () => {
    expect(liveOrNull(null, 0)).toBeNull()
    expect(isLive(undefined, 0)).toBe(false)
  })
})

describe('clearsLinkedSelection', () => {
  const claudeBag = { source: 'claude:mark_cells' }
  const userBag = { source: 'gate-plot-3' }
  it('a lapsed assistant mark takes its own selection with it', () => {
    expect(clearsLinkedSelection({ origin: 'claude' }, claudeBag)).toBe(true)
  })
  it("a lapsed assistant mark leaves a brush the user made since", () => {
    expect(clearsLinkedSelection({ origin: 'claude' }, userBag)).toBe(false)
  })
  it('a user highlight going away clears as before', () => {
    expect(clearsLinkedSelection({ origin: 'user' }, userBag)).toBe(true)
    expect(clearsLinkedSelection({}, claudeBag)).toBe(true)
    expect(clearsLinkedSelection(null, userBag)).toBe(true)
  })
  it('nothing to clear without a bag', () => {
    expect(clearsLinkedSelection({ origin: 'claude' }, null)).toBe(false)
  })
})

describe('assistantMarkOn', () => {
  const mark = { imageUid: 'A', origin: 'claude' as const, labels: [1] }
  it("returns Claude's mark on this image", () => {
    expect(assistantMarkOn(mark, 'A')).toBe(mark)
  })
  it("ignores another image, a user's pick, or no image", () => {
    expect(assistantMarkOn(mark, 'B')).toBeNull()
    expect(assistantMarkOn({ ...mark, origin: 'user' as const }, 'A')).toBeNull()
    expect(assistantMarkOn({ imageUid: 'A', labels: [1] }, 'A')).toBeNull()
    expect(assistantMarkOn(mark, null)).toBeNull()
    expect(assistantMarkOn(null, 'A')).toBeNull()
  })
})
