import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import { rafCoalesce } from './rafCoalesce'

// A hand-driven animation clock: `frame()` runs whatever the code under test asked for. Node has no
// requestAnimationFrame, and a real one would make these tests timing-dependent — the point is to
// assert the RULES (one apply per frame, latest wins, steps compound), not the browser's cadence.
function fakeRaf() {
  let next = 1
  const queued = new Map<number, () => void>()
  vi.stubGlobal('requestAnimationFrame', (cb: () => void) => { const id = next++; queued.set(id, cb); return id })
  vi.stubGlobal('cancelAnimationFrame', (id: number) => { queued.delete(id) })
  return {
    /** run every callback queued for the next frame */
    async frame() {
      const cbs = [...queued.values()]
      queued.clear()
      cbs.forEach(cb => cb())
      await Promise.resolve()
    },
    get queuedCount() { return queued.size },
  }
}

let clock: ReturnType<typeof fakeRaf>
beforeEach(() => { clock = fakeRaf() })
afterEach(() => { vi.unstubAllGlobals() })

describe('rafCoalesce', () => {
  it('a burst costs one apply, with the last value', async () => {
    const applied: number[] = []
    const c = rafCoalesce<number>(v => { applied.push(v) })
    for (let i = 0; i < 40; i++) c.schedule(i)      // one slider drag
    expect(applied).toEqual([])                      // nothing before the frame
    await clock.frame()
    expect(applied).toEqual([39])
  })

  it('does not re-arm the frame on every event — a continuous stream still paints', async () => {
    // the bug this guards: re-arming (debounce-style) on each schedule pushes the frame forever, so a
    // slider held in motion would paint NOTHING until the user stopped.
    const applied: number[] = []
    const c = rafCoalesce<number>(v => { applied.push(v) })
    c.schedule(1); c.schedule(2)
    expect(clock.queuedCount).toBe(1)
    await clock.frame()
    c.schedule(3)
    await clock.frame()
    expect(applied).toEqual([2, 3])
  })

  it('peek exposes the pending value so successive steps compound within one frame', async () => {
    let painted = 1
    const c = rafCoalesce<number>(v => { painted = v })
    const current = () => c.peek() ?? painted
    c.schedule(current() * 2)      // 2
    c.schedule(current() * 2)      // 4 — off the PENDING value, not the painted one
    expect(c.peek()).toBe(4)
    await clock.frame()
    expect(painted).toBe(4)
    expect(c.peek()).toBeUndefined()
  })

  it('flush applies now and consumes the pending frame', async () => {
    const applied: number[] = []
    const c = rafCoalesce<number>(v => { applied.push(v) })
    c.schedule(7)
    await c.flush()
    expect(applied).toEqual([7])
    await clock.frame()
    expect(applied).toEqual([7])        // the frame had nothing left to do
  })

  it('flush is a no-op when nothing is pending', async () => {
    const apply = vi.fn()
    const c = rafCoalesce<number>(apply)
    await c.flush()
    expect(apply).not.toHaveBeenCalled()
  })

  it('flush awaits async work, so a caller can read the result straight after', async () => {
    let done = false
    const c = rafCoalesce<number>(async () => { await Promise.resolve(); done = true })
    c.schedule(1)
    await c.flush()
    expect(done).toBe(true)
  })

  it('cancel drops the pending value', async () => {
    const apply = vi.fn()
    const c = rafCoalesce<number>(apply)
    c.schedule(3)
    c.cancel()
    expect(c.peek()).toBeUndefined()
    await clock.frame()
    expect(apply).not.toHaveBeenCalled()
  })

  it('treats undefined as a real argument, not as "nothing pending"', async () => {
    const applied: unknown[] = []
    const c = rafCoalesce<number | undefined>(v => { applied.push(v) })
    c.schedule(undefined)
    await clock.frame()
    expect(applied).toEqual([undefined])
  })

  it('a no-argument coalescer works (the render case)', async () => {
    const render = vi.fn()
    const c = rafCoalesce<void>(render)
    c.schedule(); c.schedule(); c.schedule()
    await clock.frame()
    expect(render).toHaveBeenCalledTimes(1)
  })
})
