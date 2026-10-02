// A "this image's display metadata just changed" signal — channel names renamed in the image table
// or the metadata panel while a viewer has that image open.
//
// The viewer reads names once from `/api/viewer/meta` at open, and the pop-out lives in its own
// window with its own (empty) project store, so a rename in the main window never reached it — the
// old names stayed until the viewer was reopened. `stores/project.updateImageMeta` publishes here
// when an image's `channelNames` actually change; `ViewerWindow` refetches meta and patches the
// names in place (contrast, LUT and visibility untouched).
//
// Same transport as `viewerCacheClearChannel.ts`: `localStorage` + `storage` for other windows, and
// a same-window CustomEvent for the in-panel viewer (`storage` never fires in the writing window).
// Kept separate from the cache-clear channel on purpose — that one reallocates the renderer and
// refetches pixels, which a rename must not do.

const KEY = 'cc.viewerImageMetaRev'
const SAME_WINDOW_EVENT = 'cc:viewer-image-meta'

export interface ViewerImageMetaEvent {
  rev: string
  imageUid: string
}

/** Announce that `imageUid`'s metadata changed. Fires in this window AND every other window. */
export function publishViewerImageMeta(imageUid: string): void {
  const ev: ViewerImageMetaEvent = { rev: `${Date.now()}-${Math.random().toString(36).slice(2, 8)}`, imageUid }
  try { localStorage.setItem(KEY, JSON.stringify(ev)) } catch { /* storage disabled — pop-outs won't hear it */ }
  try { window.dispatchEvent(new CustomEvent(SAME_WINDOW_EVENT, { detail: ev })) } catch {}
}

/** Pure: a `storage` event → the payload to react to, or null (other key, cleared, unparseable). */
export function viewerImageMetaFromStorageEvent(
  e: Pick<StorageEvent, 'key' | 'newValue'>,
): ViewerImageMetaEvent | null {
  if (e.key !== KEY || !e.newValue) return null
  try {
    const p = JSON.parse(e.newValue)
    if (typeof p?.rev !== 'string' || typeof p?.imageUid !== 'string') return null
    return { rev: p.rev, imageUid: p.imageUid }
  } catch { return null }
}

/** Subscribe to metadata-change signals from this window or any other. Returns unsubscribe. */
export function onViewerImageMeta(cb: (ev: ViewerImageMetaEvent) => void): () => void {
  const onStorage = (e: StorageEvent) => {
    const ev = viewerImageMetaFromStorageEvent(e)
    if (ev) cb(ev)
  }
  const onCustom = (e: Event) => {
    const d = (e as CustomEvent).detail
    if (d && typeof d.rev === 'string' && typeof d.imageUid === 'string') cb(d as ViewerImageMetaEvent)
  }
  window.addEventListener('storage', onStorage)
  window.addEventListener(SAME_WINDOW_EVENT, onCustom)
  return () => {
    window.removeEventListener('storage', onStorage)
    window.removeEventListener(SAME_WINDOW_EVENT, onCustom)
  }
}
