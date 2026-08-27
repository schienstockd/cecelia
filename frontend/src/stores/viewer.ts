// The browser viewer's shared state — what image it has open and where it is looking — for callers
// that must chase it (task preview, linked-brushing, movie recording later).
//
// A Pinia store rather than component props because there are multiple consumers and one producer:
// ViewerWindow.vue (the only writer) publishes here, and taskPreview.ts + any future consumer read
// without a dependency on the SFC. Same shape as the napari-side `current_napari_image()`
// (`imageUid`/`zarrPath`/`taskDir`) so a switch from napari-tracked to browser-tracked is a change
// of source, not of contract.
//
// SESSION-ONLY, and that is deliberate. A reload opens the viewer window fresh from its route seed,
// so persisting these would restore what a fresh viewer would immediately overwrite. See
// docs/todo/WEB_VIEWER_PLAN.md → P7.

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

export const useViewerStore = defineStore('viewer', () => {
  const openImage = ref<OpenImage | null>(null)
  const visibleRegion = ref<VisibleRegion | null>(null)

  /** ViewerWindow calls this when the image changes (route load, valueName picker). */
  function setOpenImage(next: OpenImage | null) {
    openImage.value = next
  }

  /** ViewerWindow calls this on every pan/zoom/z/t/ndisplay change — DEBOUNCED at the sink to avoid
   *  overwhelming subscribers with per-frame updates. */
  function setVisibleRegion(next: VisibleRegion | null) {
    visibleRegion.value = next
  }

  return { openImage, visibleRegion, setOpenImage, setVisibleRegion }
})

if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useViewerStore, import.meta.hot))
