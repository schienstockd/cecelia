// Schedule expensive async work so that only the LATEST request matters. Extracted as a util (not
// left in the .vue) so the timing rules below are unit-testable without a mounted component — the
// component side is then just "build the arg, render the state".
//
// Written for the task preview, whose two triggers both fire in bursts: editing a parameter, and
// moving the napari view (a pan emits many camera events). Each run is real cellpose on the GPU
// (~0.3 s on 512², ~2 s on 2048²), so firing per event would queue seconds of stale work behind the
// one result the user actually wants.
//
// Three rules, and the second is the one a naive debounce gets wrong:
//
//  1. A burst collapses to one run (trailing debounce).
//  2. **An in-flight run is never abandoned mid-flight, but its result can be discarded.** There is no
//     way to cancel a cellpose call, so a request arriving during a run is queued rather than dropped,
//     and the running call is handed `isCurrent()` — false once superseded, so it can skip applying a
//     mask the user has already scrolled away from. Dropping the request instead would leave the view
//     showing the wrong region; awaiting it without the guard would apply a stale mask over a newer one.
//
//     **A QUEUED request supersedes, not just a started one** — and that distinction is the whole rule
//     rather than a detail of it. Runs are serialised, so a successor cannot START until its
//     predecessor returns; comparing against a token that only moves when a run starts therefore made
//     `isCurrent()` true for every run that was not explicitly `cancel()`ed, i.e. dead code in all of
//     rule 2's actual cases. It was found through the volume viewer, where the work is a long walk
//     rather than one await: a prefetch of 170 timepoints ran to completion wherever the user went, so
//     jumping to a timepoint waited for every frame before it and playback stopped outright (each tick
//     queued a request that could not start). Every other consumer had the same hole and it merely
//     looked like a stale paint.
//  3. State is observable (`idle` | `pending` | `running`), because fire-and-forget reads as broken:
//     the user needs to see that a preview is coming.
//
// **`maxWait` — the scrub knob.** A plain trailing debounce is the wrong shape for a slider whose
// user wants to see PLANES GO PAST while they drag: `schedule` restarts the timer on every event, so
// a continuous drag never fires until the pointer stops. Set `maxWait` and a second timer starts on
// the first event of a burst, does NOT reset on subsequent events, and fires with the latest arg
// when it elapses — so a long drag gets a run every `maxWait` ms with the current position, while
// short drags still coalesce cleanly through `wait`. Same one-at-a-time and same `isCurrent()`
// semantics; the difference is just how often work is allowed to START during a sustained burst.
// Written for the volume viewer's z-slider, which without it only painted the release plane
// (Dominik 2026-08-31). The canonical shape for a scrub — no hand-rolled fourth timer.
//
// Deliberately framework-agnostic (same as `coalesce.ts`) — the caller maps `onState` onto a ref.

export type RunState = 'idle' | 'pending' | 'running'

export interface DebouncedLatestOptions {
  /** Trailing debounce window, ms. */
  wait: number
  /**
   * Maximum time a burst can hold `fire` off, ms. Timer starts on the first `schedule` of a burst
   * and does NOT reset on subsequent schedules, so a sustained drag gets a run every `maxWait` ms
   * with the latest arg — the "scrub" cadence. Omit for a plain trailing debounce.
   */
  maxWait?: number
  /** Called on every state transition (never with the state it is already in). */
  onState?: (state: RunState) => void
  /**
   * Called when `work` rejects. Runs are started from a timer, so a rejection has nowhere to
   * propagate — without this it surfaces as an UNHANDLED rejection in the user's console (and fails
   * under Vitest). The scheduler therefore swallows the error and hands it here; a caller that wants
   * the failure visible renders it from this callback.
   */
  onError?: (error: unknown) => void
}

export interface DebouncedLatest<A> {
  /** Request a run with `arg`. Supersedes anything pending; restarts the debounce window. */
  schedule(arg: A): void
  /** Run the pending request immediately, skipping the remaining debounce. No-op when nothing pending. */
  flush(): void
  /** Drop the pending request and mark any in-flight run superseded (its `isCurrent()` goes false).
   *  Note the asymmetry with `dropPending()`: dropping the queued request RESTORES `isCurrent()` for
   *  the run in flight, because nothing newer is waiting any more. */
  cancel(): void
  /**
   * Drop the queued request but let an in-flight run finish AND apply its result.
   *
   * The difference from `cancel()` is who is right about what the user is looking at. `cancel()` is
   * for "that result is no longer wanted" (the toggle went off, the image changed) — it supersedes the
   * run so a late result can't land. This is for "stop after this one": the in-flight run is the
   * freshest there will be, so discarding its result would leave the caller reporting an older one.
   */
  dropPending(): void
  state(): RunState
}

/**
 * `work` receives the argument and an `isCurrent()` predicate — check it before applying a result, and
 * after every await inside long work. It goes false as soon as a newer request EXISTS, whether or not
 * that request has been able to start (see rule 2), so long work stops at its next checkpoint.
 */
export function debouncedLatest<A>(
  work: (arg: A, isCurrent: () => boolean) => Promise<void>,
  { wait, maxWait, onState, onError }: DebouncedLatestOptions,
): DebouncedLatest<A> {
  // `timer` is the trailing debounce — reset per `schedule`, fires when the burst pauses for `wait`.
  // `maxTimer` is the scrub cap — set on the FIRST schedule of a burst and NOT reset by subsequent
  // schedules, so a sustained drag still fires every `maxWait` ms with the latest arg. Both share
  // the same `fire` sink; whichever elapses first wins and the other is cleared inside `fire`.
  let timer: ReturnType<typeof setTimeout> | null = null
  let maxTimer: ReturnType<typeof setTimeout> | null = null
  let pending: { arg: A } | null = null
  let running = false
  let token = 0
  let state: RunState = 'idle'

  const setState = (s: RunState) => {
    if (s === state) return
    state = s
    onState?.(s)
  }

  const clearTrailing = () => {
    if (timer !== null) { clearTimeout(timer); timer = null }
  }
  const clearAll = () => {
    clearTrailing()
    if (maxTimer !== null) { clearTimeout(maxTimer); maxTimer = null }
  }

  async function fire(): Promise<void> {
    clearAll()   // whichever timer called us, ensure the OTHER cannot fire during work
    // A run is already going: leave `pending` alone — the running call's `finally` picks it up. Without
    // this the queued request would be lost and the view would keep a mask for a region left behind.
    if (running) return
    const next = pending
    if (next === null) { setState('idle'); return }
    pending = null
    const mine = ++token
    running = true
    setState('running')
    try {
      // `pending === null` is the "nothing newer is waiting" half. Read at CALL time, not captured:
      // the whole point is that the answer changes under a run in flight.
      await work(next.arg, () => mine === token && pending === null)
    } catch (e) {
      // never rethrow: `fire` is invoked from a timer, so a rejection here would be unhandled
      onError?.(e)
    } finally {
      running = false
      if (pending !== null) {
        setState('pending')
        // Restart both timers for the NEXT burst — `maxTimer` starts fresh here rather than
        // continuing from the previous burst's start, so the scrub cadence is one run per maxWait
        // rather than a hard ceiling that would fire immediately after work returned.
        if (timer === null) timer = setTimeout(() => { void fire() }, wait)
        if (maxWait !== undefined && maxTimer === null) {
          maxTimer = setTimeout(() => { void fire() }, maxWait)
        }
      } else {
        setState('idle')
      }
    }
  }

  return {
    schedule(arg: A) {
      pending = { arg }
      clearTrailing()
      timer = setTimeout(() => { void fire() }, wait)
      // maxWait: arm on the FIRST schedule of a burst, and while nothing is in flight — a mid-run
      // schedule's cap is enforced by the `finally` block's re-arm above.
      if (maxWait !== undefined && maxTimer === null && !running) {
        maxTimer = setTimeout(() => { void fire() }, maxWait)
      }
      // stay 'running' while a call is in flight — a queued request is not a new visible state
      if (!running) setState('pending')
    },
    flush() {
      if (pending === null) return
      clearAll()
      void fire()
    },
    cancel() {
      clearAll()
      pending = null
      token++                      // any in-flight run is now superseded
      if (!running) setState('idle')
    },
    dropPending() {
      clearAll()
      pending = null               // note: `token` untouched — the in-flight run stays current
      if (!running) setState('idle')
    },
    state: () => state,
  }
}
