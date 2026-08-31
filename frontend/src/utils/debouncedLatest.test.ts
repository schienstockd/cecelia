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
    // 1 is discarded even though 2 had not STARTED when it settled — it could not have, runs are
    // serialised. Requiring a started successor made this guard true in every case rule 2 is about.
    expect(r.applied).toEqual([2])
  })

  it('stops long work at its next checkpoint, without waiting for the successor to start', async () => {
    // The viewer's prefetch walk: many awaits in one run, not one. It has to be able to give up on a
    // window the user has left, and the only signal it gets is `isCurrent()`.
    const steps: number[] = []
    const gate: { release: (() => void) | null } = { release: null }
    const s = debouncedLatest<number>(async (arg, isCurrent) => {
      for (let i = 0; i < 5; i++) {
        if (!isCurrent()) return
        steps.push(arg * 10 + i)
        await new Promise<void>(r => { gate.release = r })
      }
    }, { wait: 0 })
    s.schedule(1)
    await vi.advanceTimersByTimeAsync(0)
    expect(steps).toEqual([10])                    // first step of the walk for 1
    s.schedule(2)                                  // the user moves on, mid-walk
    gate.release?.()
    await vi.advanceTimersByTimeAsync(0)
    // 1 abandoned its remaining four steps and 2 took over, rather than 1 running to completion.
    expect(steps).toEqual([10, 20])
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

  // maxWait — the scrub knob. A slider whose user wants to see intermediate positions during a
  // drag needs a burst to fire periodically, not only on release. The knob does not change the
  // one-at-a-time or `isCurrent` guarantees; it only lifts the "wait for the burst to pause" rule.

  it('maxWait fires periodically through a sustained burst', async () => {
    const r = recorder()
    const s = debouncedLatest(r.work, { wait: 100, maxWait: 150 })
    // A drag: keep scheduling before `wait` can ever elapse — a plain trailing debounce would never
    // fire until we stop.
    for (let i = 1; i <= 8; i++) {
      s.schedule(i)
      await vi.advanceTimersByTimeAsync(50)     // total 400 ms of continuous scheduling
    }
    // maxWait=150 should have fired at 150 ms (arg=3) — one run, latest arg at that moment.
    // (The second maxWait window won't fire while the first run is still inflight.)
    expect(r.ran).toEqual([3])
    // Finish the in-flight run, then the finally-block re-armed timers pick up the latest arg.
    await r.finish()
    await vi.advanceTimersByTimeAsync(150)
    expect(r.ran).toEqual([3, 8])
  })

  it('maxWait does not reset on subsequent schedules within a burst', async () => {
    // The whole point of a max: a burst that keeps scheduling faster than `wait` must still see
    // the timer count down instead of being pushed back forever.
    const r = recorder()
    const s = debouncedLatest(r.work, { wait: 100, maxWait: 200 })
    s.schedule(1)
    await vi.advanceTimersByTimeAsync(80)
    s.schedule(2)                                  // resets `wait` — trailing debounce restarts
    await vi.advanceTimersByTimeAsync(80)
    s.schedule(3)                                  // resets `wait` again
    // Total 160 ms — trailing timer has been reset twice; maxWait has been running the whole time.
    expect(r.ran).toEqual([])
    await vi.advanceTimersByTimeAsync(40)          // 200 ms since first schedule → maxWait elapses
    expect(r.ran).toEqual([3])
  })

  it('maxWait: without it, a sustained burst never fires (regression baseline)', async () => {
    // Pins the shape the knob exists to fix. Same drag as above, plain trailing debounce = no runs.
    const r = recorder()
    const s = debouncedLatest(r.work, { wait: 100 })
    for (let i = 1; i <= 8; i++) {
      s.schedule(i)
      await vi.advanceTimersByTimeAsync(50)
    }
    expect(r.ran).toEqual([])                      // 400 ms of dragging, zero runs
  })

  it('maxWait: trailing wait still fires when the burst pauses inside the max window', async () => {
    const r = recorder()
    const s = debouncedLatest(r.work, { wait: 100, maxWait: 500 })
    s.schedule(1)
    s.schedule(2)
    await vi.advanceTimersByTimeAsync(100)         // burst paused → trailing fires first
    expect(r.ran).toEqual([2])
    await r.finish()
    // maxTimer was cleared inside fire — no phantom fire after settling.
    await vi.advanceTimersByTimeAsync(1_000)
    expect(r.ran).toEqual([2])
  })

  it('cancel() clears the maxWait timer', async () => {
    // Same guarantee as the trailing timer — a cancelled scheduler must not fire from a still-armed
    // scrub cap.
    const r = recorder()
    const s = debouncedLatest(r.work, { wait: 500, maxWait: 100 })
    s.schedule(1)
    s.cancel()
    await vi.advanceTimersByTimeAsync(1_000)
    expect(r.ran).toEqual([])
    expect(s.state()).toBe('idle')
  })

  it('flush() bypasses both wait and maxWait', async () => {
    const r = recorder()
    const s = debouncedLatest(r.work, { wait: 500, maxWait: 500 })
    s.schedule(9)
    s.flush()
    await vi.advanceTimersByTimeAsync(0)
    expect(r.ran).toEqual([9])
  })
})
