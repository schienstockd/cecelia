import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import { debouncedLatest, type RunState } from './debouncedLatest'

// Fake timers so the debounce window is exercised deterministically rather than by sleeping.
beforeEach(() => { vi.useFakeTimers() })
afterEach(() => { vi.useRealTimers() })

/** A controllable async job: records the args it ran with, and lets each call be resolved by hand. */
function recorder() {
  const ran: number[] = []
  const applied: number[] = []
  let resolve: (() => void) | null = null
  const work = async (arg: number, isCurrent: () => boolean) => {
    ran.push(arg)
    await new Promise<void>(r => { resolve = r })
    if (isCurrent()) applied.push(arg)     // the guard callers are told to use
  }
  return {
    ran,
    applied,
    work,
    /** finish the in-flight call and let the scheduler's `finally` run */
    async finish() { resolve?.(); resolve = null; await vi.advanceTimersByTimeAsync(0) },
    get inflight() { return resolve !== null },
  }
}

describe('debouncedLatest', () => {
  it('collapses a burst into a single run with the last argument', async () => {
    const r = recorder()
    const s = debouncedLatest(r.work, { wait: 100 })
    s.schedule(1); s.schedule(2); s.schedule(3)
    expect(r.ran).toEqual([])                     // nothing yet — still inside the window
    await vi.advanceTimersByTimeAsync(100)
    expect(r.ran).toEqual([3])                    // only the newest, once
  })

  it('does not fire before the window elapses', async () => {
    const r = recorder()
    const s = debouncedLatest(r.work, { wait: 100 })
    s.schedule(1)
    await vi.advanceTimersByTimeAsync(99)
    expect(r.ran).toEqual([])
    await vi.advanceTimersByTimeAsync(1)
    expect(r.ran).toEqual([1])
  })

  it('queues a request that arrives mid-run instead of dropping it', async () => {
    // The rule a naive debounce gets wrong. A cellpose call cannot be cancelled, so a request during
    // a run must survive — otherwise the view keeps a mask for a region the user has left.
    const r = recorder()
    const s = debouncedLatest(r.work, { wait: 100 })
    s.schedule(1)
    await vi.advanceTimersByTimeAsync(100)
    expect(r.ran).toEqual([1])
    expect(r.inflight).toBe(true)

    s.schedule(2)
    await vi.advanceTimersByTimeAsync(100)
    expect(r.ran).toEqual([1])                    // still only the first — it hasn't finished

    await r.finish()                              // first completes → queued one starts after the window
    await vi.advanceTimersByTimeAsync(100)
    expect(r.ran).toEqual([1, 2])
  })

  it('lets a superseded run discard its result', async () => {
    const r = recorder()
    const s = debouncedLatest(r.work, { wait: 100 })
    s.schedule(1)
    await vi.advanceTimersByTimeAsync(100)
    s.schedule(2)                                  // supersedes while 1 is in flight
    await vi.advanceTimersByTimeAsync(100)
    await r.finish()                               // 1 resolves...
    await vi.advanceTimersByTimeAsync(100)         // ...and 2 starts
    await r.finish()
    expect(r.ran).toEqual([1, 2])
    // 1's isCurrent() was still true when it settled (2 had not yet started), so both applied here.
    // The discard case is asserted below via cancel(), which supersedes without starting a successor.
    expect(r.applied).toEqual([1, 2])
  })

  it('cancel() supersedes an in-flight run so its result is not applied', async () => {
    const r = recorder()
    const s = debouncedLatest(r.work, { wait: 100 })
    s.schedule(1)
    await vi.advanceTimersByTimeAsync(100)
    expect(r.inflight).toBe(true)
    s.cancel()
    await r.finish()
    expect(r.ran).toEqual([1])
    expect(r.applied).toEqual([])                  // ran, but its mask must never reach the viewer
  })

  it('cancel() drops a pending request', async () => {
    const r = recorder()
    const s = debouncedLatest(r.work, { wait: 100 })
    s.schedule(1)
    s.cancel()
    await vi.advanceTimersByTimeAsync(500)
    expect(r.ran).toEqual([])
    expect(s.state()).toBe('idle')
  })

  // "Stop after this one" — what pinning the preview means. The distinction from cancel() is which
  // result the caller ends up reporting.
  it('dropPending() lets the in-flight run finish AND apply', async () => {
    const r = recorder()
    const s = debouncedLatest(r.work, { wait: 100 })
    s.schedule(1)
    await vi.advanceTimersByTimeAsync(100)
    s.schedule(2)                                  // queued behind the run
    s.dropPending()
    await r.finish()
    expect(r.ran).toEqual([1])                     // 2 never ran
    expect(r.applied).toEqual([1])                 // ...and 1 still counted, unlike under cancel()
    expect(s.state()).toBe('idle')
  })

  it('dropPending() settles to idle immediately when nothing is in flight', async () => {
    const r = recorder()
    const s = debouncedLatest(r.work, { wait: 100 })
    s.schedule(1)
    expect(s.state()).toBe('pending')
    s.dropPending()
    expect(s.state()).toBe('idle')                 // the button that did this must look like it worked
    await vi.advanceTimersByTimeAsync(500)
    expect(r.ran).toEqual([])
  })

  it('flush() runs the pending request without waiting', async () => {
    const r = recorder()
    const s = debouncedLatest(r.work, { wait: 10_000 })
    s.schedule(7)
    s.flush()
    await vi.advanceTimersByTimeAsync(0)
    expect(r.ran).toEqual([7])
  })

  it('flush() with nothing pending is a no-op', async () => {
    const r = recorder()
    const s = debouncedLatest(r.work, { wait: 100 })
    s.flush()
    await vi.advanceTimersByTimeAsync(500)
    expect(r.ran).toEqual([])
    expect(s.state()).toBe('idle')
  })

  it('reports idle → pending → running → idle, without repeats', async () => {
    // The states exist so the user can see a preview is coming; fire-and-forget reads as broken.
    const seen: RunState[] = []
    const r = recorder()
    const s = debouncedLatest(r.work, { wait: 100, onState: st => seen.push(st) })
    expect(s.state()).toBe('idle')
    s.schedule(1)
    s.schedule(2)                                  // a second schedule must not re-emit 'pending'
    expect(seen).toEqual(['pending'])
    await vi.advanceTimersByTimeAsync(100)
    expect(seen).toEqual(['pending', 'running'])
    await r.finish()
    expect(seen).toEqual(['pending', 'running', 'idle'])
    expect(s.state()).toBe('idle')
  })

  it('stays in running while a mid-run request is queued', async () => {
    const seen: RunState[] = []
    const r = recorder()
    const s = debouncedLatest(r.work, { wait: 100, onState: st => seen.push(st) })
    s.schedule(1)
    await vi.advanceTimersByTimeAsync(100)
    expect(s.state()).toBe('running')
    s.schedule(2)
    expect(s.state()).toBe('running')              // NOT back to pending — a run is still on the GPU
    expect(seen).toEqual(['pending', 'running'])
    await r.finish()
    expect(s.state()).toBe('pending')              // now the queued one is waiting its window
  })

  it('a throwing run reports the error and does not wedge the scheduler', async () => {
    // Runs start from a timer, so a rejection has nowhere to propagate: it must reach `onError`, not
    // escape as an unhandled rejection in the user's console.
    let calls = 0
    const errors: unknown[] = []
    const s = debouncedLatest(async (arg: number) => {
      calls++
      if (arg === 1) throw new Error('boom')
    }, { wait: 100, onError: e => errors.push(e) })
    s.schedule(1)
    await vi.advanceTimersByTimeAsync(100)
    expect(calls).toBe(1)
    expect((errors[0] as Error).message).toBe('boom')
    expect(s.state()).toBe('idle')                 // recovered, not stuck in 'running'
    s.schedule(2)
    await vi.advanceTimersByTimeAsync(100)
    expect(calls).toBe(2)
    expect(errors).toHaveLength(1)
  })

  it('a throwing run with no onError is still swallowed', async () => {
    // Guards the unhandled-rejection regression specifically: onError is optional, and omitting it
    // must not turn a failed preview into a console error.
    const s = debouncedLatest(async () => { throw new Error('quiet') }, { wait: 10 })
    s.schedule(1)
    await vi.advanceTimersByTimeAsync(50)
    expect(s.state()).toBe('idle')
  })
})
