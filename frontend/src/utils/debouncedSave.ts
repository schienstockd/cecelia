// Write-behind autosave: the app has no "save" button, so every board/canvas/animation edit is written
// back on a trailing debounce. The third of the three coalescing shapes in the frontend:
//
//  • `debouncedLatest` — a REQUEST whose latest answer is what the user is waiting for (a preview, a
//    plot fetch). Keeps a result, discards superseded ones.
//  • `rafCoalesce`     — a PAINT. The frame is the unit; there is no result at all.
//  • `debouncedSave`   — a WRITE. Nothing waits on it and there is no result to discard, but it has one
//    hazard the other two don't: the store is ALSO written by a restore from disk, and a restore's own
//    mutations trip the same watcher that a user edit does. Echoing them straight back is at best a
//    pointless request and at worst a clobber.
//
// So this owns the piece all three stores were hand-rolling: the trailing timer, and a suppression
// window around a restore that is DERIVED from the debounce rather than picked by hand. That coupling
// is the part that was getting written down as a comment ("900ms — > the 800ms autosave debounce") and
// therefore the part that would eventually be wrong: it has to outlast the debounce, so it is computed.
//
// What stays with the CALLER: building the payload, the dirty check (per-object in `canvasPanels`,
// whole-document in `analysisLayout`), version/conflict handling, and swallowing network errors —
// autosave is best-effort by definition. The helper never inspects what is saved.

/** Extra suppression held after a restore, beyond the debounce window it must outlast. */
const SETTLE_MARGIN = 200

export interface DebouncedSave {
  /** Note an edit. No-op while a restore is settling. */
  schedule(): void
  /** Save now, skipping the wait. No-op when no edit is pending. */
  flush(): Promise<void>
  /** Drop the pending save. */
  cancel(): void
  /** True while a restore is suppressing autosaves. */
  restoring(): boolean
  /**
   * Hydrate the store from disk with the autosave suppressed — during `hydrate` AND for long enough
   * after that the mutations it made cannot land as an edit. Vue watchers are asynchronous, so
   * clearing the flag at the end of `hydrate` does NOT work: the watcher has not run yet and the
   * restore is written straight back. (`stores/animation` did exactly that.)
   */
  duringRestore<T>(hydrate: () => T): T
}

export function debouncedSave(save: () => void | Promise<void>, { wait }: { wait: number }): DebouncedSave {
  let timer: ReturnType<typeof setTimeout> | null = null
  let pending = false
  // a DEPTH, not a boolean: two restores overlapping (a project switch during a reload) must not have
  // the first one's timer unsuppress while the second is still hydrating
  let holds = 0

  const clearTimer = () => { if (timer !== null) { clearTimeout(timer); timer = null } }

  async function run(): Promise<void> {
    timer = null
    if (!pending) return
    pending = false
    await save()
  }

  return {
    schedule() {
      if (holds > 0) return
      pending = true
      clearTimer()
      timer = setTimeout(() => { void run() }, wait)
    },
    async flush() { clearTimer(); await run() },
    cancel() { clearTimer(); pending = false },
    restoring: () => holds > 0,
    duringRestore<T>(hydrate: () => T): T {
      holds++
      try {
        return hydrate()
      } finally {
        // whatever the restore queued is not an edit
        clearTimer(); pending = false
        setTimeout(() => { holds-- }, wait + SETTLE_MARGIN)
      }
    },
  }
}
