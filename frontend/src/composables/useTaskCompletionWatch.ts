import { useWsStore } from '../stores/ws'
import { isObserverTrigger, OBSERVER_AUTO_FRAME_TYPES } from '../utils/observerAuto'

// Generic backbone for "react to a finished task/chain node in the background, for the app lifetime".
// It owns only the MECHANISM — subscribe to the terminal task/chain WS frames once, filter, debounce,
// and gate on an `enabled()` predicate — never the POLICY (what to do, whether it's allowed). The
// observer's "Watch" is the first consumer; a future auto-QC-recompute / auto-export watcher can reuse
// this instead of hand-rolling its own subscription (which is how parallel, drifting listeners start).
//
// Install ONCE from an always-mounted place (App.vue), NOT from a v-if'd component — a subscription
// inside a component that unmounts stops firing exactly when background work is most wanted.
export interface TaskCompletionWatchOptions {
  /** Run only when this returns true (e.g. a toggle is on + a backend is available). */
  enabled: () => boolean
  /** What to do after a (debounced) task completion. */
  onComplete: (frame: any) => void
  /** Coalesce a burst of completions into one call. Default 8s. */
  debounceMs?: number
  /** Which frames count as "a task finished". Defaults to the terminal task/chain-node set. */
  frameTypes?: string[]
  /** Predicate for a terminal frame. Defaults to the shared task-completion test. */
  isTrigger?: (frame: any) => boolean
}

export function useTaskCompletionWatch(opts: TaskCompletionWatchOptions) {
  const ws = useWsStore()
  const debounceMs = opts.debounceMs ?? 8000
  const frameTypes = opts.frameTypes ?? OBSERVER_AUTO_FRAME_TYPES
  const isTrigger = opts.isTrigger ?? isObserverTrigger
  let timer: ReturnType<typeof setTimeout> | null = null
  let installed = false

  function onFrame(frame: any) {
    if (!opts.enabled()) return
    if (!isTrigger(frame)) return
    if (timer) clearTimeout(timer)
    timer = setTimeout(() => {
      timer = null
      if (opts.enabled()) opts.onComplete(frame)
    }, debounceMs)
  }

  /** Subscribe for the app lifetime (idempotent — safe to call more than once). */
  function install() {
    if (installed) return
    installed = true
    frameTypes.forEach(t => ws.on(t, onFrame))
  }

  return { install }
}
