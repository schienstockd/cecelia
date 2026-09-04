// A user-driven "clear the viewer's cached frames" signal — the escape hatch #779 doesn't cover.
//
// #779 invalidates the tile atlas when the STORE identity changes (e.g. `default` → `smoothed`,
// same imageUid, different valueName). What it can't detect is a re-run of a task that overwrites
// the SAME `ccidSmoothed.ome.zarr` in place: identity string unchanged, bytes changed, cache HIT
// still returns stale pixels. Same-store re-writes need a signal from the user (or from the task
// completion, but that is the durable follow-up — see docs/TODO.md). This channel is the button.
//
// **`localStorage` + the `storage` event AND a same-window CustomEvent**, mirroring
// `openProjectChannel.ts` but adding the same-window path. The `storage` event fires only in the
// OTHER windows of the origin, so a viewer opened as a pop-out (`lib/popout.ts`) hears it, but the
// in-panel `ViewerPanel` living in the SAME window as the Settings page does not — clicking the
// button and having nothing happen in-panel is worse than not having the button. The CustomEvent
// covers the same-window case; pop-outs get the storage event as normal.
//
// The signal is a monotonic revision string; viewers thread it into their `sourceId` (tile atlas)
// and `BrickSource.rev` (brick renderer), and the same invalidation logic #779 wired up for a
// version swap kicks in for the same reason.

const KEY = 'cc.viewerCacheClearRev'
const SAME_WINDOW_EVENT = 'cc:viewer-cache-clear'

/** Publish a fresh revision. Fires in this window AND every other window on this origin. */
export function publishViewerCacheClear(): void {
  const rev = String(Date.now())
  try { localStorage.setItem(KEY, rev) } catch { /* storage disabled — pop-outs won't hear it */ }
  // storage events don't fire in the publishing window, so raise a same-window CustomEvent too.
  try { window.dispatchEvent(new CustomEvent(SAME_WINDOW_EVENT, { detail: rev })) } catch {}
}

/** The last-published revision, or `''` when nothing was ever published. Used at mount. */
export function readViewerCacheClearRev(): string {
  try { return localStorage.getItem(KEY) ?? '' } catch { return '' }
}

/**
 * What a `storage` event means for a viewer holding rev `currentRev`, as a pure decision:
 * `null` = nothing to do; otherwise the new rev to adopt. Separated from the listener because the
 * listener is the untestable half. Two ignores are real: an event about any other key (the app
 * writes several to localStorage), and an event whose value equals what we already hold (the
 * publisher fires both a `storage` event AND a CustomEvent in the same tick to cover the
 * same-window case, and a viewer sitting in another window will see the storage event first and
 * then re-trigger nothing on the follow-up).
 */
export function viewerCacheClearFromStorageEvent(
  e: Pick<StorageEvent, 'key' | 'newValue'>,
  currentRev: string,
): string | null {
  if (e.key !== KEY) return null
  const next = e.newValue ?? ''
  return next === currentRev ? null : next
}

/** Subscribe to clear signals from THIS window (button click) or any pop-out. Returns unsubscribe. */
export function onViewerCacheClear(cb: (rev: string) => void): () => void {
  const onStorage = (e: StorageEvent) => {
    // Reads localStorage inside the callback so we always see the latest, no need to thread state.
    const decision = viewerCacheClearFromStorageEvent(e, readViewerCacheClearRev())
    if (decision !== null) cb(decision)
  }
  const onCustom = (e: Event) => {
    const detail = (e as CustomEvent).detail
    cb(detail === undefined || detail === null ? '' : String(detail))
  }
  window.addEventListener('storage', onStorage)
  window.addEventListener(SAME_WINDOW_EVENT, onCustom)
  return () => {
    window.removeEventListener('storage', onStorage)
    window.removeEventListener(SAME_WINDOW_EVENT, onCustom)
  }
}
