import { ref, onMounted, onUnmounted, watch } from 'vue'
import { useProjectStore } from '../stores/project'
import { useWsStore } from '../stores/ws'
import { useViewerStore } from '../stores/viewer'
import {
  createClaimRegistry,
  liveLabelPreviews, shouldRefreshPreview, type LivePreview, type TaskListEntry,
} from '../utils/overlayAutoShow'

// Turn REMEMBERED overlay state + live-write previews into VIEWER state — the app-level glue that
// watches the project store's `openImageUid` (which changes when an image is opened, whatever the
// trigger: route load, task:result, panel click, popup focus) and wires WS events (`gating:popmap`,
// `task:status`, `task:progress`, chain nodes) to the panel's overlay bag and the popup viewer.
//
// `openImageUid` (not `viewerImageUid`): the panel's `openedImage` is keyed off `openImageUid`, and
// with napari being retired `viewerImageUid` is no longer written by any live path — which left
// `livePreviews` permanently empty, so the `pi-bolt` live-preview row never appeared during a run.
//
// TWO RULES, both learned from real bugs — read before adding a fourth WS handler:
//
// 1. OWNERSHIP. None of this may live in a component that can be unmounted. It used to live entirely
//    in ViewerPanel.vue, which App.vue mounts behind `v-if="settings.viewerPanelOpen"` — and that
//    floating panel is off by default. With it closed, nothing was subscribed to image-open events, so
//    opening an image restored no overlays at all while the toggles (persisted in localStorage) still
//    read ON. `useOverlayAutoShow()` is mounted ONCE in App.vue so the wiring runs regardless.
//
// 2. READ `settings`, NEVER A COMPONENT'S REFS. These run off WS events, so no component watcher is
//    guaranteed to have run first. Trusting ViewerPanel's refs is what previously pushed against a
//    stale/empty visibility map and skipped branches entirely.

// ── Shared colour-by legend ──────────────────────────────────────────────────────
// {category value → hex} and {category value → population name} — module-level (not
// ViewerPanel-local) because whoever ends up populating them app-side (a route reply, the browser
// viewer's own legend derivation, or the animation-snapshot legend) needs to leave them correct
// whether or not the panel is open. Empty during PR 2; PR 4 rewires the source.
export const colourLegend       = ref<Record<string, string>>({})
export const colourLegendLabels = ref<Record<string, string>>({})
export function resetColourLegend() { colourLegend.value = {}; colourLegendLabels.value = {} }

// ── Live preview of a running task's label store ─────────────────────────────────
// A segmentation creates its label zarr at full shape and fills it one timepoint at a time, so it can
// be watched while it runs. `ccid.json` only registers the set on success, so the running task itself
// is the source of truth for what exists (`live_outputs` → GET /api/tasks).
//
// The panel row's toggle sets `previewShown`; `_syncLiveLabels` hands the shown store to the browser
// viewer through `viewerStore.liveLabels` (cross-window), which reads that vn's labels from the run's
// staging store (`live=1` on the slab route) and refetches whenever the stamp changes.

// Label stores being written right now for the open image (drives the ViewerPanel rows).
export const livePreviews = ref<LivePreview[]>([])
// Which of them the user has actually asked to see, by value_name. Deliberately NOT persisted:
// describes a store that exists only while one task runs; persisting would restore a preview for
// a value_name that may never exist again (a cancelled or failed run leaves nothing to register).
export const previewShown = ref<Record<string, boolean>>({})
const _lastRefreshAt: Record<string, number> = {}

// Re-read what is in flight and reconcile `previewShown` against it. Called on every task lifecycle
// event rather than polled: `list_tasks()` is a point-in-time snapshot, and the WS already says when
// it changed.
export async function refreshLivePreviews(): Promise<void> {
  const project  = useProjectStore()
  const imageUid = project.openImageUid
  if (!imageUid) { livePreviews.value = []; _syncLiveLabels(); return }
  let tasks: TaskListEntry[] = []
  try {
    const res = await fetch('/api/tasks')
    if (res.ok) tasks = await res.json() as TaskListEntry[]
  } catch { /* a snapshot we couldn't fetch just means no previews offered this round */ }
  const next = liveLabelPreviews(tasks, imageUid)
  const live = new Set(next.map(p => p.valueName))
  // Drop previews whose task is gone; the panel row will disappear on the next render.
  previewShown.value = Object.fromEntries(
    Object.entries(previewShown.value).filter(([vn, on]) => on && live.has(vn)))
  livePreviews.value = next
  _syncLiveLabels()
}

// Point the viewer at the shown live store (the viewer draws one mask, so the first shown wins), or
// at nothing. `restamp` re-sends the same store so the viewer refetches what was written since; without
// it an unchanged choice is left alone, so unrelated task events do not reload the viewer's frame.
function _syncLiveLabels(restamp = false): void {
  const viewer = useViewerStore()
  const uid = useProjectStore().openImageUid
  const shown = uid ? livePreviews.value.find(p => previewShown.value[p.valueName]) : undefined
  const cur = viewer.liveLabels
  if (!uid || !shown) { viewer.setLiveLabels(null); return }
  if (!restamp && cur && cur.imageUid === uid && cur.valueName === shown.valueName) return
  viewer.setLiveLabels({ imageUid: uid, valueName: shown.valueName })
}

// Show/hide one live preview. Returns the new state so the caller can reflect the choice.
export async function togglePreview(valueName: string): Promise<boolean> {
  const want = !previewShown.value[valueName]
  previewShown.value = { ...previewShown.value, [valueName]: want }
  if (want) _lastRefreshAt[valueName] = Date.now()
  _syncLiveLabels()
  return want
}

// Progress tick → re-stamp the shown preview (throttled) so the viewer refetches the frames written
// since its last read.
function _onProgressTick(): void {
  const now = Date.now()
  let due = false
  for (const p of livePreviews.value) {
    if (!previewShown.value[p.valueName]) continue
    if (!shouldRefreshPreview(_lastRefreshAt[p.valueName], now)) continue
    _lastRefreshAt[p.valueName] = now
    due = true
  }
  if (due) _syncLiveLabels(true)
}

// ── Opt-out for callers that restore a DIFFERENT view than the remembered toggles ───────────────
const _claims = createClaimRegistry()
// Claim an image's next open (analysis-board zoom-to-source replays a captured frame instead).
export function suppressAutoShowOnce(imageUid: string) { _claims.claim(imageUid) }
// Release the claim when the open never happened (request failed), so the next legitimate open for
// that image is not silently swallowed. No argument drops every claim.
export function releaseAutoShowSuppression(imageUid?: string) { _claims.release(imageUid) }

// Mount ONCE, app-level (App.vue) — see rule 1. Not for use in a page or a floating panel.
export function useOverlayAutoShow() {
  const ws = useWsStore()
  const project = useProjectStore()
  // Any task lifecycle change can add or remove a watchable store. Chain nodes are included because a
  // chain-launched segmentation writes exactly the same store — the frontend never sees its params, so
  // the backend's own `live_outputs` snapshot is what makes chain runs previewable at all.
  const onTaskLifecycle = () => { void refreshLivePreviews() }
  const onProgress = () => _onProgressTick()
  // React to the project store's `openImageUid` (the image the user is looking at) — set by the
  // ImageTable eye click and by the popup viewer's `cc.viewerFocus` bridge. Previously watched
  // `viewerImageUid`, but with napari retired nothing writes that any more, so the previews list
  // stayed empty for every run and the `pi-bolt` live-preview row never appeared.
  let stopWatch: (() => void) | null = null
  onMounted(() => {
    stopWatch = watch(() => project.openImageUid, (uid) => {
      // previews belong to the image that was open; a different image's runs are a different set
      previewShown.value = {}
      void refreshLivePreviews()
      // The `suppressAutoShowOnce` claim stays as a hook for the eventual browser-viewer restore path
      // — consuming the claim keeps the semantic that "this open was handled by whoever set the claim,
      // don't run the default restore".
      if (uid) void _claims.consume(uid)
    })
    ws.on('task:status', onTaskLifecycle)
    ws.on('chain:node:running', onTaskLifecycle)
    ws.on('chain:node:done', onTaskLifecycle)
    ws.on('chain:node:failed', onTaskLifecycle)
    ws.on('task:progress', onProgress)
    void refreshLivePreviews()   // a run may already be in flight when the app connects
  })
  onUnmounted(() => {
    stopWatch?.()
    ws.off('task:status', onTaskLifecycle)
    ws.off('chain:node:running', onTaskLifecycle)
    ws.off('chain:node:done', onTaskLifecycle)
    ws.off('chain:node:failed', onTaskLifecycle)
    ws.off('task:progress', onProgress)
  })
}
