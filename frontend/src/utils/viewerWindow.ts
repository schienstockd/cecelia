// Opening an image in the browser volume viewer — the ONE place that knows what that window needs.
//
// A pop-out is a fresh app instance with no project open (`lib/popout.ts`), so everything the viewer
// needs has to travel in the query. That list is not obvious and it grew: the project and image, the
// SET (the per-set viewer prefs — point size, colour-by, which population type is shown — are stored
// per set and the popup cannot look them up), the image VERSION, and the display NAME so the window
// has a title before its first fetch returns.
//
// It is a helper rather than two inline URL builders because there are now two entry points — the eye
// in the image table and the viewer panel's ↗ — and a query key missing from one of them is a viewer
// that silently forgets a preference rather than an error anyone would notice.
//
// NOTHING HERE DEPENDS ON NAPARI. The viewer used to be reachable only from the panel that drives
// napari, so seeing an image in the browser meant starting a desktop process first (Dominik,
// 2026-08-25). The image uid and the set are all it ever needed.
import { openPopoutWindow } from '../lib/popout'

export interface ViewerWindowTarget {
  projectUid: string
  imageUid: string
  /** For the per-set viewer preferences. Absent is legal — the viewer falls back to window-local. */
  setUid?: string
  /** Which image version. Absent lets the server resolve the default, which is what the image table
   *  has: it shows images, not versions. */
  valueName?: string
  /** For the window title, before the first fetch answers. */
  name?: string
}

/** The query string for `/viewer-window`, without the leading `?`. Pure, so the key list is pinned by
 *  a test rather than by whichever call site was edited last. */
export function viewerWindowQuery(t: ViewerWindowTarget): string {
  const q = new URLSearchParams({ project: t.projectUid, image: t.imageUid })
  // Only when there is something to say: an empty `set=` reads as a set whose prefs are all defaults,
  // which is not the same as no set.
  t.setUid && q.set('set', t.setUid)
  t.valueName && q.set('valueName', t.valueName)
  t.name && q.set('name', t.name)
  return q.toString()
}

/** Open (or re-focus) the viewer window on this image. No-op without both uids — a viewer with no
 *  image to show is a window that opens onto an error. */
export function openViewerWindow(t: ViewerWindowTarget): void {
  if (!t.projectUid || !t.imageUid) return
  openPopoutWindow('/viewer-window', 1200, 800, '?' + viewerWindowQuery(t))
}
