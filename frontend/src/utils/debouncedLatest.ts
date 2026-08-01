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
//  3. State is observable (`idle` | `pending` | `running`), because fire-and-forget reads as broken:
//     the user needs to see that a preview is coming.
//
// Deliberately framework-agnostic (same as `coalesce.ts`) — the caller maps `onState` onto a ref.

export type RunState = 'idle' | 'pending' | 'running'

export interface DebouncedLatestOptions {
  /** Trailing debounce window, ms. */
  wait: number
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
  /** Drop the pending request and mark any in-flight run superseded (its `isCurrent()` goes false). */
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
 * after every await inside long work.
 */
export function debouncedLatest<A>(
  work: (arg: A, isCurrent: () => boolean) => Promise<void>,
  { wait, onState, onError }: DebouncedLatestOptions,
): DebouncedLatest<A> {
  let timer: ReturnType<typeof setTimeout> | null = null
  let pending: { arg: A } | null = null
  let running = false
  let token = 0
  let state: RunState = 'idle'

  const setState = (s: RunState) => {
    if (s === state) return
    state = s
    onState?.(s)
  }

  const clearTimer = () => {
    if (timer !== null) { clearTimeout(timer); timer = null }
  }

  async function fire(): Promise<void> {
    timer = null
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
      await work(next.arg, () => mine === token)
    } catch (e) {
      // never rethrow: `fire` is invoked from a timer, so a rejection here would be unhandled
      onError?.(e)
    } finally {
      running = false
      if (pending !== null) {
        setState('pending')
        if (timer === null) timer = setTimeout(() => { void fire() }, wait)
      } else {
        setState('idle')
      }
    }
  }

  return {
    schedule(arg: A) {
      pending = { arg }
      clearTimer()
      timer = setTimeout(() => { void fire() }, wait)
      // stay 'running' while a call is in flight — a queued request is not a new visible state
      if (!running) setState('pending')
    },
    flush() {
      if (pending === null) return
      clearTimer()
      void fire()
    },
    cancel() {
      clearTimer()
      pending = null
      token++                      // any in-flight run is now superseded
      if (!running) setState('idle')
    },
    dropPending() {
      clearTimer()
      pending = null               // note: `token` untouched — the in-flight run stays current
      if (!running) setState('idle')
    },
    state: () => state,
  }
}
