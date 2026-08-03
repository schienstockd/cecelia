// One shared clock for live elapsed counters.
//
// Anything showing "how long has this been running" needs a reactive `now` that advances on its own —
// the Task Manager rows, the per-module task list, the chain board's live nodes. Each of them rolling its
// own `setInterval` means N timers ticking on N different phases (so two counters on screen disagree by
// up to a second), N chances to forget the cleanup, and a timer that keeps firing after the last counter
// is gone. This is one interval, reference-counted: it starts with the first subscriber, stops with the
// last, and every counter on screen ticks on the same beat.
//
// Split out of the composable so it is testable without mounting a component (docs/DEV.md → frontend
// test scope). `useNowTick` (composables/useNowTick.ts) is the wrapper components should use — it ties
// the unsubscribe to the component's scope so callers can't leak one.
import { ref } from 'vue'

const TICK_MS = 1000

/** Wall-clock ms, advanced once a second while anything is subscribed (frozen otherwise). */
export const nowMs = ref(Date.now())

let timer: ReturnType<typeof setInterval> | null = null
let subscribers = 0

/**
 * Start the clock (if it isn't already) and return the unsubscribe.
 *
 * `nowMs` is refreshed immediately on the first subscribe, so a counter mounted 900ms into a tick shows
 * the right value at once instead of a stale one until the next beat. The returned function is safe to
 * call more than once — a double-unsubscribe must not drive the count negative and leave the timer
 * running forever.
 */
export function subscribeNowTick(): () => void {
  if (subscribers === 0) {
    nowMs.value = Date.now()
    timer = setInterval(() => { nowMs.value = Date.now() }, TICK_MS)
  }
  subscribers++
  let released = false
  return () => {
    if (released) return
    released = true
    subscribers--
    if (subscribers === 0 && timer) { clearInterval(timer); timer = null }
  }
}

/** Test seam: whether the interval is currently running. */
export function nowTickRunning(): boolean {
  return timer !== null
}
