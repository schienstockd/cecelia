import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import { resizePaint, LIVE_RESIZE_MS, RESIZE_SETTLE_MS } from './resizePaint'

// A paint that "takes" `cost` ms on a stubbed clock, so the live-vs-settle rule is checkable without
// a canvas or a real stopwatch.
let now = 0
function painterCosting(cost: () => number) {
  const calls = { n: 0 }
  const p = resizePaint(() => { calls.n++; now += cost() })
  return { p, calls }
}

beforeEach(() => { now = 0; vi.useFakeTimers(); vi.stubGlobal('performance', { now: () => now }) })
afterEach(() => { vi.useRealTimers(); vi.unstubAllGlobals() })

describe('resizePaint', () => {
  it('a cheap paint keeps painting on every resize step', () => {
    const { p, calls } = painterCosting(() => 1)
    p.paint()
    for (let i = 0; i < 5; i++) p.resized()
    expect(calls.n).toBe(6)
  })

  it('a slow paint is not repeated per resize step — once, after the drag settles', async () => {
    const { p, calls } = painterCosting(() => LIVE_RESIZE_MS + 40)
    p.paint()
    expect(calls.n).toBe(1)
    for (let i = 0; i < 20; i++) { p.resized(); await vi.advanceTimersByTimeAsync(16) }
    expect(calls.n).toBe(1)                       // the browser stretched the old bitmap meanwhile
    await vi.advanceTimersByTimeAsync(RESIZE_SETTLE_MS + 10)
    expect(calls.n).toBe(2)                       // one crisp repaint at the final size
  })

  it('a content change paints at once, even mid-resize, and supersedes the pending settle paint', async () => {
    const { p, calls } = painterCosting(() => LIVE_RESIZE_MS + 40)
    p.paint(); p.resized()
    p.paint()
    expect(calls.n).toBe(2)
    await vi.advanceTimersByTimeAsync(RESIZE_SETTLE_MS * 3)
    expect(calls.n).toBe(2)
  })

  it('cancel drops the pending settle paint (unmount)', async () => {
    const { p, calls } = painterCosting(() => LIVE_RESIZE_MS + 40)
    p.paint(); p.resized(); p.cancel()
    await vi.advanceTimersByTimeAsync(RESIZE_SETTLE_MS * 3)
    expect(calls.n).toBe(1)
  })
})
