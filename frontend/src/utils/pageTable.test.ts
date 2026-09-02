import { describe, it, expect } from 'vitest'
import { PageTable, brickKey, slotToAtlasOrigin, slotCount, pickStaleInflightKeys } from './pageTable'

describe('brickKey', () => {
  it('is stable and axis-labelled', () => {
    expect(brickKey({ t: 0, level: 1, bx: 2, by: 3, bz: 4 })).toBe('T0/L1/B2,3,4')
  })
  it('distinguishes every axis (a t-change is not a bx-change)', () => {
    const base = { t: 0, level: 0, bx: 0, by: 0, bz: 0 }
    const seen = new Set([
      brickKey(base),
      brickKey({ ...base, t: 1 }),
      brickKey({ ...base, level: 1 }),
      brickKey({ ...base, bx: 1 }),
      brickKey({ ...base, by: 1 }),
      brickKey({ ...base, bz: 1 }),
    ])
    // All 6 axis-variants distinct — a store's `t` axis must not silently collide with `bx`.
    expect(seen.size).toBe(6)
  })
})

describe('slotToAtlasOrigin', () => {
  it('walks x then y then z (cache-friendly linear scan)', () => {
    // 2x2x2 atlas of 128x128x4 bricks.
    const counts = [2, 2, 2] as const
    const brick = [128, 128, 4] as const
    expect(slotToAtlasOrigin(0, counts, brick)).toEqual([0, 0, 0])
    expect(slotToAtlasOrigin(1, counts, brick)).toEqual([128, 0, 0])          // +x
    expect(slotToAtlasOrigin(2, counts, brick)).toEqual([0, 128, 0])          // wrap → +y
    expect(slotToAtlasOrigin(4, counts, brick)).toEqual([0, 0, 4])            // wrap wrap → +z
    expect(slotToAtlasOrigin(7, counts, brick)).toEqual([128, 128, 4])        // last slot
  })
  it('handles anisotropic bricks (small z, big xy — SispLk shape)', () => {
    // SispLk-ish: bricks 128x128x4 in a 8x8x1 atlas → covers 1024x1024x4 at r8uint = 4 MB.
    const counts = [8, 8, 1] as const
    const brick = [128, 128, 4] as const
    expect(slotToAtlasOrigin(0, counts, brick)).toEqual([0, 0, 0])
    expect(slotToAtlasOrigin(63, counts, brick)).toEqual([7 * 128, 7 * 128, 0])
  })
})

describe('slotCount', () => {
  it('is the product of the three axis counts', () => {
    expect(slotCount([1, 1, 1])).toBe(1)
    expect(slotCount([8, 8, 8])).toBe(512)
    expect(slotCount([64, 32, 1])).toBe(2048)
  })
})

describe('PageTable — insert / lookup / touch', () => {
  const brick = (bx: number): { t: number; level: number; bx: number; by: number; bz: number } =>
    ({ t: 0, level: 0, bx, by: 0, bz: 0 })

  it('inserts a brick and hands out slot 0 first', () => {
    const t = new PageTable(4)
    const { entry, evictedKey } = t.insertOrEvictLru(brick(0), 100)
    expect(entry.slot).toBe(0)
    expect(evictedKey).toBeNull()
    expect(t.has('T0/L0/B0,0,0')).toBe(true)
    expect(t.size()).toBe(1)
  })

  it('a second insert on the same key returns the same slot (idempotent)', () => {
    const t = new PageTable(4)
    const a = t.insertOrEvictLru(brick(0), 100).entry
    const b = t.insertOrEvictLru(brick(0), 200).entry
    expect(a.slot).toBe(b.slot)
    expect(b.lastUsed).toBe(200)
    expect(t.size()).toBe(1)
  })

  it('touch updates lastUsed only if resident', () => {
    const t = new PageTable(4)
    t.insertOrEvictLru(brick(0), 100)
    t.touch('T0/L0/B0,0,0', 500)
    expect(t.get('T0/L0/B0,0,0')?.lastUsed).toBe(500)
    // Not resident — touch is a no-op, not an error. A late touch after eviction must not throw
    // (this happens when the ranker runs the same frame the LRU trims).
    expect(() => t.touch('T0/L0/B99,0,0', 600)).not.toThrow()
  })

  it('fills every slot, then reports full', () => {
    const t = new PageTable(3)
    t.insertOrEvictLru(brick(0), 1)
    t.insertOrEvictLru(brick(1), 2)
    t.insertOrEvictLru(brick(2), 3)
    expect(t.isFull()).toBe(true)
    expect(t.size()).toBe(3)
    // Slots are the first three — 0, 1, 2 — in insertion order.
    const slots = t.entries().map(e => e.slot).sort()
    expect(slots).toEqual([0, 1, 2])
  })
})

describe('PageTable — LRU eviction', () => {
  const brick = (bx: number): { t: number; level: number; bx: number; by: number; bz: number } =>
    ({ t: 0, level: 0, bx, by: 0, bz: 0 })

  it('evicts the oldest brick when full', () => {
    const t = new PageTable(3)
    t.insertOrEvictLru(brick(0), 1)             // oldest
    t.insertOrEvictLru(brick(1), 2)
    t.insertOrEvictLru(brick(2), 3)
    const { entry, evictedKey } = t.insertOrEvictLru(brick(3), 4)
    expect(evictedKey).toBe('T0/L0/B0,0,0')      // brick 0 was the oldest
    expect(t.has('T0/L0/B0,0,0')).toBe(false)
    expect(t.has('T0/L0/B3,0,0')).toBe(true)
    // Reused slot: the newcomer takes the freed slot rather than growing the atlas.
    expect(entry.slot).toBe(0)
  })

  it('touch protects against eviction', () => {
    const t = new PageTable(3)
    t.insertOrEvictLru(brick(0), 1)
    t.insertOrEvictLru(brick(1), 2)
    t.insertOrEvictLru(brick(2), 3)
    // Brick 0 was oldest, but a touch bumps it — brick 1 becomes the LRU.
    t.touch('T0/L0/B0,0,0', 10)
    const { evictedKey } = t.insertOrEvictLru(brick(3), 11)
    expect(evictedKey).toBe('T0/L0/B1,0,0')
  })

  it('touch stamp bias survives an eviction round against same-frame touches — the fix for the boundT-brick eviction thrash', () => {
    // Regression test for the "black rectangular holes in the volume" bug 2026-09-02: when
    // every resident brick had `lastUsed = frameNow` (all touched this tick), `evictLru`'s
    // strict `<` tie-break picked the first-inserted resident as victim — which was typically
    // an early-loaded boundT current-render brick. The fix in `brickVolumeRenderer.ts` touches
    // boundT bricks with `frameNow + BOUND_T_TOUCH_BIAS` so LRU can't pick them under a tie.
    // Guard the underlying contract here so a future refactor of the touch policy can't
    // reintroduce the tie-break scenario.
    const t = new PageTable(3)
    t.insertOrEvictLru(brick(0), 1)       // simulate an early-loaded boundT brick
    t.insertOrEvictLru(brick(1), 2)       // simulate a prefetch brick
    t.insertOrEvictLru(brick(2), 3)       // simulate a second prefetch brick
    // This-tick touches — boundT gets the bias, prefetch bricks get plain frameNow=100.
    const BOUND_T_BIAS = 500_000_000
    t.touch('T0/L0/B0,0,0', 100 + BOUND_T_BIAS)  // boundT — protected
    t.touch('T0/L0/B1,0,0', 100)                  // prefetch
    t.touch('T0/L0/B2,0,0', 100)                  // prefetch
    // A new prefetch brick arrives, triggering an eviction.
    const { evictedKey } = t.insertOrEvictLru(brick(3), 100)
    // The boundT brick MUST survive — a prefetch brick has to die instead.
    expect(evictedKey).not.toBe('T0/L0/B0,0,0')
    expect(t.has('T0/L0/B0,0,0')).toBe(true)
    // And the biased brick keeps its bias — a subsequent tie-break round can't pick it either.
    const { evictedKey: evicted2 } = t.insertOrEvictLru(brick(4), 100)
    expect(evicted2).not.toBe('T0/L0/B0,0,0')
    expect(t.has('T0/L0/B0,0,0')).toBe(true)
  })

  it('explicit evict frees the slot and does not throw on an absent key', () => {
    const t = new PageTable(2)
    t.insertOrEvictLru(brick(0), 1)
    t.evict('T0/L0/B0,0,0')
    expect(t.size()).toBe(0)
    expect(t.isFull()).toBe(false)
    expect(() => t.evict('T0/L0/B99,0,0')).not.toThrow()
    // The freed slot 0 is reused by the next inserter.
    expect(t.insertOrEvictLru(brick(1), 2).entry.slot).toBe(0)
  })
})

describe('pickStaleInflightKeys — the queue-skip contract', () => {
  it('names keys whose t is outside the wanted set — playback→stop→scrub-elsewhere', () => {
    // Bug shape (Dominik 2026-09-02): playback burst filled inflight at t=5,6,7; user then
    // scrubbed to t=20. Without this sweep the new boundT=20 kickFetches sit behind the queue
    // cap (`MAX_INFLIGHT=16`) waiting for now-obsolete t=5..7 fetches to land.
    const inflight = [
      'T5/L0/B0,0,0', 'T6/L0/B0,0,0', 'T7/L0/B0,0,0',
      'T20/L0/B0,0,0', 'T21/L0/B0,0,0',
    ]
    const wanted = new Set([20, 21, 22])
    expect(pickStaleInflightKeys(inflight, wanted).sort()).toEqual([
      'T5/L0/B0,0,0', 'T6/L0/B0,0,0', 'T7/L0/B0,0,0',
    ])
  })
  it('keeps everything when every t is wanted (playback stepping forward)', () => {
    // Playback t=5→6, old prefetch was {6,7,8}, new prefetch = {7,8,9}. Wanted =
    // {6,7,8,9} (boundT plus new window). All old-prefetch bricks stay — no wasted socket work.
    const inflight = ['T6/L0/B0,0,0', 'T7/L0/B0,0,0', 'T8/L0/B0,0,0']
    expect(pickStaleInflightKeys(inflight, new Set([6, 7, 8, 9]))).toEqual([])
  })
  it('ignores unparseable keys — the aborter cannot name what it cannot identify', () => {
    // Defensive: a malformed key holds its queue slot rather than being aborted blindly. Better
    // to leak one slot than to abort an unrelated request whose identity we cannot confirm.
    const stale = pickStaleInflightKeys(['not-a-key', 'T5/L0/B0,0,0'], new Set([20]))
    expect(stale).toEqual(['T5/L0/B0,0,0'])
  })
})

describe('PageTable — construction guards', () => {
  it('rejects a non-positive capacity — catches an atlas that would allocate nothing', () => {
    expect(() => new PageTable(0)).toThrow()
    expect(() => new PageTable(-1)).toThrow()
    expect(() => new PageTable(1.5)).toThrow()
  })
})
