// Which project this app has open, published so the app's OTHER windows can follow it.
//
// A pop-out (`lib/popout.ts`) is a second full app instance with its own Pinia, so its `projectMeta`
// starts empty and stays wherever it was pointed. That is fine for the console, which has no notion of
// a project, and wrong for the Task Manager window: switching project in the main window left the
// popped-out list quietly scoped to the project you had just left, still labelled as if it were
// current. A window showing a different project from the app is not a state anyone asked for, so this
// is not a toggle.
//
// **`localStorage` + the `storage` event**, not `BroadcastChannel`, for one reason that decides it: the
// event carries state that PERSISTS, so a window that reloads (or opens late) reads the current answer
// from the same key it would have received an event about. A channel only delivers to whoever was
// listening at the time, which would need a second, durable copy anyway.
//
// The `storage` event fires only in the OTHER windows of the origin, never in the one that wrote —
// so the publisher cannot hear itself, and a follower that publishes in turn (it does: it opens the
// project through the ordinary store path) bounces one event back that every reader no-ops on, because
// they all compare against the uid they already hold.
const KEY = 'cc.openProject'

/** Announce the open project to the app's other windows. `''`/null = no project open. */
export function publishOpenProject(uid: string | null | undefined): void {
  try { localStorage.setItem(KEY, uid ?? '') } catch { /* storage disabled — following just won't work */ }
}

/** The project the app last had open, or `''`. Used at mount, before any event can have arrived. */
export function readOpenProject(): string {
  try { return localStorage.getItem(KEY) ?? '' } catch { return '' }
}

/**
 * What a `storage` event means for a window currently showing `currentUid`, as a pure decision:
 * `null` = nothing to do, otherwise the uid to open (`''` = close).
 *
 * Separated from the listener because the listener is the untestable half. The three ignores are all
 * real: an event about any other key (the settings store writes a dozen), an event whose value equals
 * what we already show (the bounce described above), and a `clear()` — `key === null` — which is not
 * someone closing a project.
 */
export function openProjectFromStorageEvent(
  e: Pick<StorageEvent, 'key' | 'newValue'>, currentUid: string | null | undefined,
): string | null {
  if (e.key !== KEY) return null
  const next = e.newValue ?? ''
  return next === (currentUid ?? '') ? null : next
}

/** Subscribe to project switches made in the app's other windows. Returns the unsubscribe. */
export function onOpenProjectChange(cb: (e: StorageEvent) => void): () => void {
  window.addEventListener('storage', cb)
  return () => window.removeEventListener('storage', cb)
}
