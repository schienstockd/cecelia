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
import type { ViewerViewState } from '../utils/viewer/viewState'

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
const K_VIEW_STATE     = 'cc.viewer.viewState'
const K_PREVIEW_LABELS = 'cc.viewer.previewLabels'
const K_PREVIEW_IMAGES = 'cc.viewer.previewImages'

/** What the `<vn>__preview.ome.zarr` scratch store contains — set by taskPreview after a run, read
 *  by ViewerWindow to flip its labels slab request onto the preview path. Lives HERE (not in
 *  taskPreview) so the viewer's popup Pinia can see it through the same cross-window bridge as
 *  `openImage` — the run completes in the module page's window and the labels render in the popup.
 *
 *  `updateId` is a monotonic stamp per run. Two runs on the same image/vn produce identical
 *  {valueName, imageUid, projectUid} — and per the DOM spec, `localStorage.setItem` with the exact
 *  same string value does NOT emit a `storage` event. Without a per-run stamp, the popup viewer
 *  never wakes up on a plane change or a param edit, and the mask on screen stays from the FIRST
 *  run. Set by `setPreviewLabels`; callers pass just the identity. */
export interface PreviewLabels {
  valueName: string
  imageUid: string
  projectUid: string
  updateId: number
}

/** One corrected channel from an AF preview run — set by taskPreview after an AF run, read by
 *  ViewerWindow to swap that channel's slab request onto the scratch AF store. Same cross-window
 *  bridge as `previewLabels`, and the same `updateId` idiom: identical identity across re-runs
 *  would produce no `storage` event and the popup would keep showing the first run's pixels.
 *
 *  The set is per-image and per-run. When a new AF preview run lands, every corrected channel's
 *  entry is written at once with a shared `updateId`, and the ViewerWindow watch fires ONCE per
 *  run (not per channel), even though the swap runs across every entry. */
export interface PreviewImage {
  sourceChannel: number
  valueName: string
  imageUid: string
  projectUid: string
  updateId: number
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
  const viewState     = ref<ViewerViewState | null>(_readJson<ViewerViewState>(K_VIEW_STATE))
  const previewLabels = ref<PreviewLabels | null>(_readJson<PreviewLabels>(K_PREVIEW_LABELS))
  const previewImages = ref<PreviewImage[] | null>(_readJson<PreviewImage[]>(K_PREVIEW_IMAGES))

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

  /** ViewerWindow calls this on every pan/zoom/z/t/ndisplay/channel change — same debounced sink as
   *  `visibleRegion`, but the payload is a napari-shaped viewState snapshot the AnimationPanel and
   *  movie recorder can consume. Same JSON round-trip as the region, so a popup writer reaches the
   *  main-window animation page through the storage bridge. Deliberately napari's schema so the
   *  offline renderer's `viewstate_to_render_args` reads them identically. */
  function setViewState(next: ViewerViewState | null) {
    viewState.value = next
    _writeJson(K_VIEW_STATE, next)
  }

  /** taskPreview calls this after a run: `next` non-null flips the viewer window's labels slab
   *  request onto the preview scratch store; `null` (on stop / error / mismatch) flips it back.
   *  A monotonic `updateId` is added here (never taken from the caller) so two runs that return
   *  identical identity still produce a distinct localStorage value — see PreviewLabels doc. */
  let _updateIdSeq = 0
  function setPreviewLabels(next: Omit<PreviewLabels, 'updateId'> | null) {
    const stamped: PreviewLabels | null = next ? { ...next, updateId: ++_updateIdSeq } : null
    previewLabels.value = stamped
    _writeJson(K_PREVIEW_LABELS, stamped)
  }

  /** taskPreview calls this after an AF run: `next` non-null flips each corrected channel's slab
   *  onto the scratch AF store; `null` (on stop / error / mismatch) flips them all back. A single
   *  `updateId` stamps the whole array — every entry shares it — so the watch on the popup viewer
   *  fires ONCE per run, not once per channel. */
  function setPreviewImages(next: Omit<PreviewImage, 'updateId'>[] | null) {
    if (!next || next.length === 0) {
      previewImages.value = null
      _writeJson(K_PREVIEW_IMAGES, null)
      return
    }
    const stamp = ++_updateIdSeq
    const stamped = next.map(m => ({ ...m, updateId: stamp }))
    previewImages.value = stamped
    _writeJson(K_PREVIEW_IMAGES, stamped)
  }

  // Cross-window sync: `storage` events fire only in OTHER same-origin windows on a write, so the
  // pattern is symmetric — every window listens, every window writes on its own change.
  if (typeof window !== 'undefined') {
    window.addEventListener('storage', e => {
      if (e.key === K_OPEN_IMAGE) {
        openImage.value = e.newValue ? JSON.parse(e.newValue) : null
      } else if (e.key === K_VISIBLE_REGION) {
        visibleRegion.value = e.newValue ? JSON.parse(e.newValue) : null
      } else if (e.key === K_VIEW_STATE) {
        viewState.value = e.newValue ? JSON.parse(e.newValue) : null
      } else if (e.key === K_PREVIEW_LABELS) {
        previewLabels.value = e.newValue ? JSON.parse(e.newValue) : null
      } else if (e.key === K_PREVIEW_IMAGES) {
        previewImages.value = e.newValue ? JSON.parse(e.newValue) : null
      }
    })
  }

  return { openImage, visibleRegion, viewState, previewLabels, previewImages,
           setOpenImage, setVisibleRegion, setViewState, setPreviewLabels, setPreviewImages }
})

if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useViewerStore, import.meta.hot))
