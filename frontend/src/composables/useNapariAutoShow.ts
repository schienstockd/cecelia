import { ref, onMounted, onUnmounted } from 'vue'
import { useProjectStore } from '../stores/project'
import { useWsStore } from '../stores/ws'
import {
  createClaimRegistry,
  liveLabelPreviews, shouldRefreshPreview, type LivePreview, type TaskListEntry,
} from '../utils/napariAutoShow'

// Turn REMEMBERED overlay state + live-write previews into VIEWER state — the app-level glue that
// wires WS events (`napari:opened`, `gating:popmap`, `task:status`, `task:progress`, chain nodes)
// to the panel's overlay bag and the popup viewer.
//
// This used to also fire napari push commands (show-labels / show-tracks / show-populations /
// colour-labels / refresh-labels) as the second half. Deleted in the P9 decommission (PR 2): every
// panel toggle persists to `settings` and pings `cc.viewerOverlaysTick`, which the WebGPU viewer
// subscribes to; a second HTTP push into a Qt canvas we no longer draw was a mirror the browser
// viewer didn't need. The WS handlers stay because the STATE they manage (the panel's live-preview
// rows, the colour-by legend refs) still drives the panel UI regardless of who renders the frame.
//
// TWO RULES, both learned from real bugs — read before adding a fourth WS handler:
//
// 1. OWNERSHIP. None of this may live in a component that can be unmounted. It used to live entirely
//    in ViewerPanel.vue, which App.vue mounts behind `v-if="settings.viewerPanelOpen"` — and that
//    floating panel is off by default. With it closed, nothing was subscribed to `napari:opened`, so
//    opening an image restored no overlays at all while the toggles (persisted in localStorage) still
//    read ON. `useNapariAutoShow()` is mounted ONCE in App.vue so the WS wiring runs regardless.
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
// The napari `(vn) Labels (live)` layer that used to render this went with the P9 push helpers. The
// browser viewer's own live-write preview rendering is not in this PR — the panel row's toggle now
// only manages STATE (`previewShown`); the viewer will pick it up when that path lands.

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
  const imageUid = project.napariImageUid
  if (!imageUid) { livePreviews.value = []; return }
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
}

// Show/hide one live preview. Returns the new state so the caller can reflect the choice.
export async function togglePreview(valueName: string): Promise<boolean> {
  const want = !previewShown.value[valueName]
  previewShown.value = { ...previewShown.value, [valueName]: want }
  if (want) _lastRefreshAt[valueName] = Date.now()
  return want
}

// Progress tick → note we saw one for the shown previews. The push that used to refresh napari's
// live layer went with the P9 helpers; the browser viewer's own preview refresh will read this
// map when that path lands.
function _onProgressTick(): void {
  const now = Date.now()
  for (const p of livePreviews.value) {
    if (!previewShown.value[p.valueName]) continue
    if (!shouldRefreshPreview(_lastRefreshAt[p.valueName], now)) continue
    _lastRefreshAt[p.valueName] = now
  }
}

// ── Opt-out for callers that restore a DIFFERENT view than the remembered toggles ───────────────
const _claims = createClaimRegistry()
// Claim an image's next open (analysis-board zoom-to-source replays a captured frame instead).
export function suppressAutoShowOnce(imageUid: string) { _claims.claim(imageUid) }
// Release the claim when the open never happened (request failed), so the next legitimate open for
// that image is not silently swallowed. No argument drops every claim.
export function releaseAutoShowSuppression(imageUid?: string) { _claims.release(imageUid) }

// Mount ONCE, app-level (App.vue) — see rule 1. Not for use in a page or a floating panel.
export function useNapariAutoShow() {
  const ws = useWsStore()
  const onOpened = (data: Record<string, unknown>) => {
    const uid = String(data?.imageUid ?? '')
    // previews belong to the image that was open; a different image's runs are a different set
    previewShown.value = {}
    void refreshLivePreviews()
    // The `suppressAutoShowOnce` claim used to gate the napari-restore push. It stays as a hook
    // for the eventual browser-viewer restore path — consuming the claim keeps the semantic that
    // "this open was handled by whoever set the claim, don't run the default restore".
    if (uid) void _claims.consume(uid)
  }
  // Any task lifecycle change can add or remove a watchable store. Chain nodes are included because a
  // chain-launched segmentation writes exactly the same store — the frontend never sees its params, so
  // the backend's own `live_outputs` snapshot is what makes chain runs previewable at all.
  const onTaskLifecycle = () => { void refreshLivePreviews() }
  const onProgress = () => _onProgressTick()
  onMounted(() => {
    ws.on('napari:opened', onOpened)
    ws.on('task:status', onTaskLifecycle)
    ws.on('chain:node:running', onTaskLifecycle)
    ws.on('chain:node:done', onTaskLifecycle)
    ws.on('chain:node:failed', onTaskLifecycle)
    ws.on('task:progress', onProgress)
    void refreshLivePreviews()   // a run may already be in flight when the app connects
  })
  onUnmounted(() => {
    ws.off('napari:opened', onOpened)
    ws.off('task:status', onTaskLifecycle)
    ws.off('chain:node:running', onTaskLifecycle)
    ws.off('chain:node:done', onTaskLifecycle)
    ws.off('chain:node:failed', onTaskLifecycle)
    ws.off('task:progress', onProgress)
  })
}
