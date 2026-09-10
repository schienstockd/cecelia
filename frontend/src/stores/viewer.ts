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
  /** how many multiscale levels the OPEN IMAGE has — the range the 3D detail control offers on
   *  movie surfaces. 0 or 1 = no choice to make, so the control hides. */
  nLevels?: number
}

const K_OPEN_IMAGE       = 'cc.viewer.openImage'
const K_VISIBLE_REGION   = 'cc.viewer.visibleRegion'
const K_VIEW_STATE       = 'cc.viewer.viewState'
const K_PENDING_VIEW     = 'cc.viewer.pendingViewState'
const K_PREVIEW_LABELS   = 'cc.viewer.previewLabels'
const K_PREVIEW_IMAGES   = 'cc.viewer.previewImages'
const K_TRACK_HIGHLIGHT  = 'cc.viewer.trackHighlight'
const K_LABELS_DIM_MISMATCH = 'cc.viewer.labelsDimMismatch'

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

/** A viewState the AnimationPanel wants the browser viewer to apply — the OTHER direction of the
 *  bridge. AnimationPanel calls `setPendingViewState(vs)` when the user clicks a keyframe with
 *  "sync viewer" on, or when they toggle sync on and want the selected keyframe to appear. The
 *  ViewerWindow watches this ref; on change it converts back to the orbit-camera form and applies.
 *
 *  Stamped with `updateId` — same reason as `PreviewLabels`: identical repeat writes across
 *  localStorage return the same JSON and DON'T fire the `storage` event, so a keyframe re-clicked
 *  never wakes the popup viewer. Stamping guarantees the value differs each set. */
export interface PendingViewState {
  viewState: unknown       // opaque `ViewerViewState` object; the ViewerWindow parses it
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

/**
 * "Highlight ONLY these track ids on the viewer" — the browser-viewer replacement for
 * `showTracksInNapari` (removed in P9 slice 4, commit 842d8d36, without a browser-viewer
 * equivalent). Written by TrackSchemeView's **Show** button; the popup viewer subscribes and
 * narrows the per-vn track source to just these ids for the named (imageUid, valueName).
 *
 * When `trackIds` is empty (or the whole record is null), highlight mode is OFF and ribbons render
 * as usual. Scope is deliberately per (imageUid, valueName) — a highlight for image A's tracks
 * must NOT survive an image swap in the viewer, and a highlight authored for vn `memTom` must not
 * accidentally narrow vn `default` (different track_id namespaces).
 *
 * `updateId` is monotonic like the other viewer bag entries: two Show presses on the same track
 * set produce identical `trackIds` and would otherwise emit no storage event.
 */
export interface TrackHighlight {
  imageUid: string
  valueName: string
  trackIds: number[]
  updateId: number
}

/**
 * Which mask stores' spatial dims no longer match the open image version, per (imageUid,
 * valueName). ViewerWindow computes this from its meta payload on load / on version pick and
 * publishes here; ViewerPanel reads it to flag the offending rows in the segmentation list, so a
 * user can see WHY the eye toggle silently refused to overlay. Bridged across the popup ⇄ main
 * window through the same `storage`-event pattern as the other viewer bags.
 *
 * `byVn` names each mismatched mask's L0 dims — the flag's tooltip can quote them ("mask is
 * 441×420, image is 434×418"). Empty `mismatched` means "checked, all clear" (a distinct signal
 * from `null`: "no viewer open, nothing to say").
 */
export interface LabelsDimMismatch {
  imageUid: string
  valueName: string          // the IMAGE version the check was done against
  imageNX: number
  imageNY: number
  mismatched: string[]
  byVn: Record<string, { nX: number; nY: number }>
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
  const viewState        = ref<ViewerViewState | null>(_readJson<ViewerViewState>(K_VIEW_STATE))
  // Seeded from localStorage: `openViewerWindow` handoffs (analysis-strip zoom-to-source) may write
  // the pending BEFORE the popup mounts, and a popup that started null would never see it.
  // The popup viewer's watcher clears the entry after applying (`consumePendingViewState`), so a
  // reload doesn't silently re-move the camera. The FRESHNESS guard is the seed's own `updateId` +
  // consumption pattern — the setter writes, whoever applies calls `consumePendingViewState()`, and
  // a straggler entry from a crashed apply expires when the next explicit call overwrites it.
  const pendingViewState = ref<PendingViewState | null>(_readJson<PendingViewState>(K_PENDING_VIEW))
  const previewLabels    = ref<PreviewLabels | null>(_readJson<PreviewLabels>(K_PREVIEW_LABELS))
  const previewImages    = ref<PreviewImage[] | null>(_readJson<PreviewImage[]>(K_PREVIEW_IMAGES))
  const trackHighlight   = ref<TrackHighlight | null>(_readJson<TrackHighlight>(K_TRACK_HIGHLIGHT))
  const labelsDimMismatch = ref<LabelsDimMismatch | null>(_readJson<LabelsDimMismatch>(K_LABELS_DIM_MISMATCH))

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
   *  `visibleRegion`, but the payload is a viewer-shaped viewState snapshot the AnimationPanel and
   *  movie recorder can consume. Same JSON round-trip as the region, so a popup writer reaches the
   *  main-window animation page through the storage bridge. Deliberately the viewer's schema so the
   *  offline renderer's `viewstate_to_render_args` reads them identically. */
  function setViewState(next: ViewerViewState | null) {
    viewState.value = next
    _writeJson(K_VIEW_STATE, next)
  }

  /** AnimationPanel + ImageStripView.zoomToSource call this to ask the ViewerWindow to jump to a
   *  captured keyframe. Stamped with a monotonic `updateId` so a re-click of the same keyframe
   *  fires the storage event (identical writes are suppressed — see `PendingViewState`).
   *  ViewerWindow watches `pendingViewState`, applies, then calls `consumePendingViewState()` so a
   *  later reload of the popup doesn't silently re-move the camera to a stale seed. */
  function setPendingViewState(vs: ViewerViewState | null) {
    const stamped: PendingViewState | null = vs ? { viewState: vs, updateId: ++_updateIdSeq } : null
    pendingViewState.value = stamped
    _writeJson(K_PENDING_VIEW, stamped)
  }
  /** ViewerWindow calls this AFTER applying a pending, so the seed doesn't survive to the next
   *  popup reload. Only clears when the caller reports the id it just applied — a concurrent
   *  setPendingViewState (a follow-up jump arriving mid-apply) is preserved. */
  function consumePendingViewState(appliedUpdateId: number) {
    if (pendingViewState.value?.updateId !== appliedUpdateId) return
    pendingViewState.value = null
    _writeJson(K_PENDING_VIEW, null)
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

  /** TrackSchemeView calls this on **Show** to narrow the viewer's per-vn track source to just
   *  these ids. `null` (or an empty `trackIds`) clears the highlight. Stamped monotonically for
   *  the same reason as previewLabels — two identical Show presses would otherwise not wake the
   *  popup's storage listener. */
  function setTrackHighlight(next: Omit<TrackHighlight, 'updateId'> | null) {
    const stamped: TrackHighlight | null = next && next.trackIds.length
      ? { ...next, updateId: ++_updateIdSeq } : null
    trackHighlight.value = stamped
    _writeJson(K_TRACK_HIGHLIGHT, stamped)
  }

  /** ViewerWindow calls this whenever meta lands (image open / valueName pick), so the sidebar
   *  panel can flag mask rows whose stored spatial dims no longer match the current image
   *  version. `null` clears (no viewer open). */
  function setLabelsDimMismatch(next: LabelsDimMismatch | null) {
    labelsDimMismatch.value = next
    _writeJson(K_LABELS_DIM_MISMATCH, next)
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
      } else if (e.key === K_PENDING_VIEW) {
        pendingViewState.value = e.newValue ? JSON.parse(e.newValue) : null
      } else if (e.key === K_PREVIEW_LABELS) {
        previewLabels.value = e.newValue ? JSON.parse(e.newValue) : null
      } else if (e.key === K_PREVIEW_IMAGES) {
        previewImages.value = e.newValue ? JSON.parse(e.newValue) : null
      } else if (e.key === K_TRACK_HIGHLIGHT) {
        trackHighlight.value = e.newValue ? JSON.parse(e.newValue) : null
      } else if (e.key === K_LABELS_DIM_MISMATCH) {
        labelsDimMismatch.value = e.newValue ? JSON.parse(e.newValue) : null
      }
    })
  }

  return { openImage, visibleRegion, viewState, pendingViewState, previewLabels, previewImages,
           trackHighlight, labelsDimMismatch,
           setOpenImage, setVisibleRegion, setViewState, setPendingViewState,
           consumePendingViewState, setPreviewLabels, setPreviewImages, setTrackHighlight,
           setLabelsDimMismatch }
})

if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useViewerStore, import.meta.hot))
