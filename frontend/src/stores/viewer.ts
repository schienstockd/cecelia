// The browser viewer's shared state — what image it has open and where it is looking — for callers
// that must chase it (task preview, linked-brushing, movie recording later).
//
// **Cross-window** by design. The volume viewer runs in a POPUP with its own Pinia store, so a plain
// `defineStore` here would not reach the task-preview toggle sitting on the main window's module page.
// Bridged through `localStorage` + the `storage` event — same idiom as `stores/settings.ts` for the
// per-image bags (VIEWER_CONTROLS_SPLIT_PLAN P2). Two keys:
//   * `cc.viewer.openImage`     — JSON, changes on route load / version picker (rare)
//   * `cc.viewer.visibleRegion` — JSON, changes on pan/zoom/z/t (debounced 100 ms at the writer)
//
// SESSION-ONLY, and that is deliberate. A reload opens the viewer window fresh from its route seed,
// so restoring these would show a stale region on the module page's toggle. The keys are wiped on
// window unload; a stale write from a crashed session is harmless — the writer overwrites on first
// emit and the region shape is `null` until then.

import { defineStore, acceptHMRUpdate } from 'pinia'
import { ref } from 'vue'
import type { VisibleRegion } from '../utils/viewer/visibleRegion'

/** What image is open in the browser viewer, in the same shape the preview worker's request needs. */
export interface OpenImage {
  projectUid: string
  imageUid: string
  /** the version currently on screen — used to detect a version-mismatch before hitting the API */
  valueName: string
  /** absolute path to the store the viewer opened. Body-carried into `/api/preview/run`. */
  zarrPath: string
  /** the image's meta dir on disk (`project/0/<uid>/`). Body-carried too. */
  taskDir: string
}

const K_OPEN_IMAGE     = 'cc.viewer.openImage'
const K_VISIBLE_REGION = 'cc.viewer.visibleRegion'
const K_PREVIEW_LABELS = 'cc.viewer.previewLabels'

/** What the `<vn>__preview.ome.zarr` scratch store contains — set by taskPreview after a run, read
 *  by ViewerWindow to flip its labels slab request onto the preview path. Lives HERE (not in
 *  taskPreview) so the viewer's popup Pinia can see it through the same cross-window bridge as
 *  `openImage` — the run completes in the module page's window and the labels render in the popup. */
export interface PreviewLabels {
  valueName: string
  imageUid: string
  projectUid: string
}

function _readJson<T>(key: string): T | null {
  if (typeof window === 'undefined') return null
  try {
    const raw = window.localStorage.getItem(key)
    return raw ? JSON.parse(raw) as T : null
  } catch { return null }
}

function _writeJson(key: string, value: unknown) {
  if (typeof window === 'undefined') return
  try {
    if (value === null) window.localStorage.removeItem(key)
    else                window.localStorage.setItem(key, JSON.stringify(value))
  } catch { /* quota / privacy mode — drop silently, same as settings.ts */ }
}

export const useViewerStore = defineStore('viewer', () => {
  // Seed from localStorage so a module page opened AFTER the viewer window already has the current
  // state, not `null` until the next pan.
  const openImage     = ref<OpenImage | null>(_readJson<OpenImage>(K_OPEN_IMAGE))
  const visibleRegion = ref<VisibleRegion | null>(_readJson<VisibleRegion>(K_VISIBLE_REGION))
  const previewLabels = ref<PreviewLabels | null>(_readJson<PreviewLabels>(K_PREVIEW_LABELS))

  /** ViewerWindow calls this when the image changes (route load, valueName picker). */
  function setOpenImage(next: OpenImage | null) {
    openImage.value = next
    _writeJson(K_OPEN_IMAGE, next)
  }

  /** ViewerWindow calls this on every pan/zoom/z/t/ndisplay change — DEBOUNCED at the sink to avoid
   *  overwhelming subscribers with per-frame updates. */
  function setVisibleRegion(next: VisibleRegion | null) {
    visibleRegion.value = next
    _writeJson(K_VISIBLE_REGION, next)
  }

  /** taskPreview calls this after a run: `next` non-null flips the viewer window's labels slab
   *  request onto the preview scratch store; `null` (on stop / error / mismatch) flips it back. */
  function setPreviewLabels(next: PreviewLabels | null) {
    previewLabels.value = next
    _writeJson(K_PREVIEW_LABELS, next)
  }

  // Cross-window sync: `storage` events fire only in OTHER same-origin windows on a write, so the
  // pattern is symmetric — every window listens, every window writes on its own change.
  if (typeof window !== 'undefined') {
    window.addEventListener('storage', e => {
      if (e.key === K_OPEN_IMAGE) {
        openImage.value = e.newValue ? JSON.parse(e.newValue) : null
      } else if (e.key === K_VISIBLE_REGION) {
        visibleRegion.value = e.newValue ? JSON.parse(e.newValue) : null
      } else if (e.key === K_PREVIEW_LABELS) {
        previewLabels.value = e.newValue ? JSON.parse(e.newValue) : null
      }
    })
  }

  return { openImage, visibleRegion, previewLabels,
           setOpenImage, setVisibleRegion, setPreviewLabels }
})

if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useViewerStore, import.meta.hot))
