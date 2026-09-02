import { describe, it, expect } from 'vitest'
import {
  PageTable, brickKey, slotToAtlasOrigin, slotCount, maxSafePrefetchDepth, shouldAdmitKick,
} from './pageTable'

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

describe('maxSafePrefetchDepth — the atlas-sizing prefetch guard', () => {
  it('caps at the request when the atlas has plenty of headroom (SispLk-shape)', () => {
    // 64 slots, 12-brick core → (64/12) − 1 = 4.33 → 4; requestedCap=1 wins.
    expect(maxSafePrefetchDepth(64, 12, 1)).toBe(1)
    expect(maxSafePrefetchDepth(64, 12, 4)).toBe(4)
  })

  it('bounds the request when the atlas can barely hold two t\'s — the Dml3RG shape at cacheMB=2048', () => {
    // 442-slot atlas (post-sizer-rewrite), 81-brick core (9×9×1) — (442/81)−1 = 4.45 → 4.
    // A hardcoded cap=4 was RIGHT on the edge (405 wanted vs 442 capacity) and prefetch churn
    // was still visible. Cap=3 gives (1+3)×81=324 residents, comfortable margin.
    expect(maxSafePrefetchDepth(442, 81, 4)).toBe(4)      // exact-fit case
    expect(maxSafePrefetchDepth(256, 81, 4)).toBe(2)      // pre-sizer atlas: 256/81=3 → 2
  })

  it('returns 0 when the atlas is not big enough for even boundT plus one prefetch t', () => {
    // Contract: boundT still wins even at depth=0 (BOUND_T_TOUCH_BIAS in the touch loop).
    expect(maxSafePrefetchDepth(80, 81, 4)).toBe(0)       // core > capacity
    expect(maxSafePrefetchDepth(100, 81, 4)).toBe(0)      // 100/81=1, minus 1 = 0
  })

  it('trusts the caller when coreBricksPerT is unknown (0)', () => {
    // Renderer has no atlas bound yet — leave the caller\'s preference alone rather than clamp
    // to 0 and silently kill prefetch.
    expect(maxSafePrefetchDepth(442, 0, 4)).toBe(4)
  })

  it('never returns a negative depth', () => {
    expect(maxSafePrefetchDepth(0, 81, 4)).toBe(0)
    expect(maxSafePrefetchDepth(442, 81, -3)).toBe(0)
  })
})

describe('shouldAdmitKick — the two-tier queue contract', () => {
  const bgKeys = (n: number, t: number = 5): string[] =>
    Array.from({ length: n }, (_, i) => `T${t}/L0/B${i},0,0`)

  it('admits a boundT brick when total is under cap', () => {
    // Full slate of 8 bg fetches for a stale t + 7 boundT fetches — a new boundT brick fits.
    const inflight = [...bgKeys(8, 5), ...bgKeys(7, 20)]
    expect(shouldAdmitKick(inflight, 20, 20, 16, 8)).toBe(true)
  })

  it('refuses a boundT brick when total hits the hard cap', () => {
    const inflight = [...bgKeys(8, 5), ...bgKeys(8, 20)]  // 16 total
    expect(shouldAdmitKick(inflight, 20, 20, 16, 8)).toBe(false)
  })

  it('caps bg at maxBg even while boundT slots are free', () => {
    // 8 bg fetches, 0 boundT — bg is at its cap. A new bg brick MUST be refused so the 8 free
    // slots stay open for boundT. This is the whole point of the two-tier scheme.
    const inflight = bgKeys(8, 5)
    expect(shouldAdmitKick(inflight, 6, 20, 16, 8)).toBe(false)
    // But boundT admission from the same state is fine — the reservation works.
    expect(shouldAdmitKick(inflight, 20, 20, 16, 8)).toBe(true)
  })

  it('admits bg while below the bg cap', () => {
    expect(shouldAdmitKick(bgKeys(7, 5), 6, 20, 16, 8)).toBe(true)
  })

  it('re-classifies stale inflight when boundT drifts (no counter to keep in sync)', () => {
    // Fetches kicked at boundT=5 now count as BG because boundT has moved to 20 — the exact case
    // that made an event-driven counter fragile. Recount on each admit keeps the semantics stable.
    const inflight = bgKeys(8, 5)  // originally boundT bricks, now stale
    // A new bg (at t=6) should be refused because the "stale-t=5" bricks now occupy the bg cap.
    expect(shouldAdmitKick(inflight, 6, 20, 16, 8)).toBe(false)
    // A new boundT=20 brick admits fine.
    expect(shouldAdmitKick(inflight, 20, 20, 16, 8)).toBe(true)
  })

  it('does not count unparseable keys toward bg (a malformed key must not block boundT)', () => {
    const inflight = ['not-a-key', 'also-not-a-key', ...bgKeys(6, 5)]  // 8 total, 6 bg-known
    // A new bg brick: only 6 bg counted, cap is 8 → admit.
    expect(shouldAdmitKick(inflight, 6, 20, 16, 8)).toBe(true)
  })
})

describe('PageTable — construction guards', () => {
  it('rejects a non-positive capacity — catches an atlas that would allocate nothing', () => {
    expect(() => new PageTable(0)).toThrow()
    expect(() => new PageTable(-1)).toThrow()
    expect(() => new PageTable(1.5)).toThrow()
  })
})
