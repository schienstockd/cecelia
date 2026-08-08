import { describe, it, expect } from 'vitest'
import { awaitIdle, anyBusy } from './awaitIdle'

// A fake clock: `sleep` advances time instead of waiting, so these run instantly and deterministically
// (a real-timer test of a 10s timeout is either slow or flaky).
function fakeClock() {
  let t = 0
  return { now: () => t, sleep: async (ms: number) => { t += ms } }
}

describe('awaitIdle', () => {
  it('returns immediately when nothing is busy', async () => {
    const c = fakeClock()
    expect(await awaitIdle(() => false, { ...c, settleMs: 0 })).toBe(true)
    expect(c.now()).toBe(0)                       // no waiting at all
  })

  it('waits for a panel to finish, then settles', async () => {
    const c = fakeClock()
    const busyUntil = 300
    expect(await awaitIdle(() => c.now() < busyUntil, { ...c, intervalMs: 50, settleMs: 100 })).toBe(true)
    expect(c.now()).toBeGreaterThanOrEqual(busyUntil + 100)
  })

  // The point of settleMs: a panel can report idle BETWEEN two fetches, and capturing in that gap is
  // exactly the bug (an export of a chart that is about to be replaced).
  it('does not believe a momentary idle', async () => {
    const c = fakeClock()
    const busy = () => { const t = c.now(); return !(t >= 100 && t < 150) && t < 400 }
    expect(await awaitIdle(busy, { ...c, intervalMs: 50, settleMs: 100 })).toBe(true)
    expect(c.now()).toBeGreaterThanOrEqual(500)   // settled only after the REAL idle at 400
  })

  // A stuck panel must not hang the export — a document with one bad plot beats a button that never
  // returns, which is what the export did before this existed.
  it('gives up at the timeout and lets the caller proceed', async () => {
    const c = fakeClock()
    expect(await awaitIdle(() => true, { ...c, timeoutMs: 1000, intervalMs: 50 })).toBe(false)
    expect(c.now()).toBeGreaterThanOrEqual(1000)
    expect(c.now()).toBeLessThan(1200)            // and gives up promptly, not eventually
  })
})

describe('anyBusy', () => {
  it('is true when any panel is busy', () => {
    expect(anyBusy([{ isBusy: () => false }, { isBusy: () => true }])).toBe(true)
    expect(anyBusy([{ isBusy: () => false }, { isBusy: () => false }])).toBe(false)
  })

  // Panels that predate the accessor, or are mid-teardown, must not block or break an export.
  it('treats a panel with no isBusy, or one that throws, as idle', () => {
    expect(anyBusy([{}, undefined])).toBe(false)
    expect(anyBusy([{ isBusy: () => { throw new Error('unmounting') } }])).toBe(false)
  })

  it('is false for an empty board', () => {
    expect(anyBusy([])).toBe(false)
  })
})
