import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest'
import { nowMs, subscribeNowTick, nowTickRunning } from './nowTick'

describe('nowTick', () => {
  beforeEach(() => vi.useFakeTimers())
  afterEach(() => vi.useRealTimers())

  it('advances once a second while subscribed', () => {
    const stop = subscribeNowTick()
    const t0 = nowMs.value
    vi.advanceTimersByTime(2_500)
    expect(nowMs.value).toBeGreaterThanOrEqual(t0 + 2_000)
    stop()
  })

  it('runs one timer for many subscribers and stops with the last', () => {
    const a = subscribeNowTick()
    const b = subscribeNowTick()
    expect(nowTickRunning()).toBe(true)
    a()
    expect(nowTickRunning()).toBe(true)      // b is still watching
    b()
    expect(nowTickRunning()).toBe(false)     // …nothing left to tick for
  })

  it('ignores a repeated unsubscribe', () => {
    // A double-release used to be able to drive the count negative, which left the interval running for
    // the rest of the session (subscribers never returns to 0 again).
    const a = subscribeNowTick()
    a(); a(); a()
    expect(nowTickRunning()).toBe(false)
    const b = subscribeNowTick()
    expect(nowTickRunning()).toBe(true)
    b()
    expect(nowTickRunning()).toBe(false)
  })
})
