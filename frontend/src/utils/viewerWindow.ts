// Opening an image in the browser volume viewer — the ONE place that knows what that window needs.
//
// A pop-out is a fresh app instance with no project open (`lib/popout.ts`), so the query has to carry
// enough to name what's on screen. IDENTITY only: the project uid, the image uid, and optionally the
// image version. Everything else the viewer needed — the set (for per-set prefs) and the display name
// (for the window title) — used to travel in the URL too; that grew to five keys, and the server
// already knows all of it. Both moved to `/api/viewer/meta` (2026-08-28); the pop-out's query is now
// `project + image` plus optional `valueName`.
//
import { openPopoutWindow, getOpenPopoutWindow, POPOUT_WINDOW_NAMES } from '../lib/popout'

export interface ViewerWindowTarget {
  projectUid: string
  imageUid: string
  /** Which image version. Absent lets the server resolve the default, which is what the image table
   *  has: it shows images, not versions. */
  valueName?: string
}

/** The query string for `/viewer-window`, without the leading `?`. Pure, so the key list is pinned by
 *  a test rather than by whichever call site was edited last. */
export function viewerWindowQuery(t: ViewerWindowTarget): string {
  const q = new URLSearchParams({ project: t.projectUid, image: t.imageUid })
  t.valueName && q.set('valueName', t.valueName)
  return q.toString()
}

/** Open (or re-focus) the viewer window on this image. No-op without both uids — a viewer with no
 *  image to show is a window that opens onto an error. */
export function openViewerWindow(t: ViewerWindowTarget): void {
  if (!t.projectUid || !t.imageUid) return
  openPopoutWindow('/viewer-window', 1200, 800, '?' + viewerWindowQuery(t))
}

/** Bring the viewer window forward, or open it on `t` when this window holds no live handle to one.
 *  Raising re-opens the window's OWN url by name: Chrome ignores `w.focus()` from another page, but a
 *  named `window.open` is a navigation that raises, and a same-url hash navigation does not reload
 *  (the way `focusMainWindow` raises the main window). False when there is nothing to show. */
export function raiseViewerWindow(t: ViewerWindowTarget | null): boolean {
  const w = getOpenPopoutWindow('/viewer-window')
  if (w) {
    try { window.open(w.location.href, POPOUT_WINDOW_NAMES['/viewer-window']) } catch { /* same origin: cannot throw */ }
    return true
  }
  if (!t?.projectUid || !t.imageUid) return false
  openViewerWindow(t)
  return true
}
