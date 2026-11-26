// The scheduler's in-flight snapshot (`GET /api/tasks`), kept fresh while something on screen needs it.
//
// The queued-row reason (`utils/queueReason.ts`) needs to know what is holding a pool RIGHT NOW,
// including work from other projects and profiles that never sends this tab a frame. So it polls —
// but only while subscribed, and the task lists subscribe only while one of their rows is queued.
// Reference-counted like `utils/nowTick.ts`: both task lists open at once share one interval.
import { ref } from 'vue'
import { fetchInFlightTasks, type InFlightTaskRow } from './runningTasks'

const POLL_MS = 3000

/** The latest snapshot; `[]` until the first fetch lands (or when it fails — fail closed, say nothing). */
export const inFlightSnapshot = ref<InFlightTaskRow[]>([])

let timer: ReturnType<typeof setInterval> | null = null
let subscribers = 0

async function refresh() {
  const rows = await fetchInFlightTasks()
  if (subscribers > 0) inFlightSnapshot.value = rows
}

/** Start polling (if not already) and return the unsubscribe; safe to call the returned fn twice. */
export function subscribeInFlightPoll(): () => void {
  if (subscribers === 0) {
    timer = setInterval(() => { void refresh() }, POLL_MS)
  }
  subscribers++
  void refresh()
  let released = false
  return () => {
    if (released) return
    released = true
    subscribers--
    if (subscribers === 0) {
      if (timer) { clearInterval(timer); timer = null }
      inFlightSnapshot.value = []
    }
  }
}

/** Test seam: whether the interval is running. */
export function inFlightPollRunning(): boolean {
  return timer !== null
}
