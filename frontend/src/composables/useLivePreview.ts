import { ref, onMounted, onUnmounted } from 'vue'
import { useWsStore } from '../stores/ws'
import { useViewerStore } from '../stores/viewer'
import { fetchInFlightTasks } from '../utils/runningTasks'
import { liveLabelPreviews, shouldRefreshPreview, type LivePreview, type TaskListEntry } from '../utils/overlayAutoShow'

// ── Live preview of a running task's label store ─────────────────────────────────
// A segmentation creates its label zarr at full shape and fills it one timepoint at a time, so it can
// be watched while it runs. `ccid.json` only registers the set on success, so the running task itself
// is the source of truth for what exists (`live_outputs` → GET /api/tasks).
//
// Owned by the viewer window (docs/todo/VIEWER_ON_THE_IMAGE_PLAN.md, S4): its "On the image" overlay
// lists `livePreviews` as ⚡ rows, the toggle sets `previewShown`, and `syncLiveLabels` points
// `viewerStore.liveLabels` at the shown store, which the window reads from the run's staging store
// (`live=1` on the slab route) and refetches whenever the stamp changes. ONE writer: the main window
// used to own this, and two windows writing the cross-window `liveLabels` would clobber each other.
// The viewer window has its own WS connection, so it hears the same task events.

/** Label stores being written right now for the open image. */
export const livePreviews = ref<LivePreview[]>([])
/** Which of them are shown, by value name. NOT persisted: the store exists only while one task runs,
 *  and a cancelled or failed run leaves nothing to restore. */
export const previewShown = ref<Record<string, boolean>>({})
const lastRefreshAt: Record<string, number> = {}
let openUid = ''

/** Re-read what is in flight and drop shown previews whose task is gone. On every task lifecycle
 *  event rather than polled: GET /api/tasks is a snapshot, and the WS says when it changed. */
export async function refreshLivePreviews(): Promise<void> {
  if (!openUid) { livePreviews.value = []; syncLiveLabels(); return }
  // fails closed to [] — a snapshot we couldn't fetch just means no previews offered this round
  const tasks = await fetchInFlightTasks() as TaskListEntry[]
  const next = liveLabelPreviews(tasks, openUid)
  const live = new Set(next.map(p => p.valueName))
  previewShown.value = Object.fromEntries(
    Object.entries(previewShown.value).filter(([vn, on]) => on && live.has(vn)))
  livePreviews.value = next
  syncLiveLabels()
}

// Point the viewer at the shown live store (it draws one mask, so the first shown wins), or at nothing.
// `restamp` re-sends the same store so the viewer refetches what was written since; without it an
// unchanged choice is left alone, so unrelated task events do not reload the frame.
function syncLiveLabels(restamp = false): void {
  const viewer = useViewerStore()
  const shown = openUid ? livePreviews.value.find(p => previewShown.value[p.valueName]) : undefined
  const cur = viewer.liveLabels
  if (!openUid || !shown) { viewer.setLiveLabels(null); return }
  if (!restamp && cur && cur.imageUid === openUid && cur.valueName === shown.valueName
      && cur.startedAt === shown.startedAt) return
  viewer.setLiveLabels({ imageUid: openUid, valueName: shown.valueName, startedAt: shown.startedAt })
}

/** Show or hide one live preview. */
export function togglePreview(valueName: string): void {
  const want = !previewShown.value[valueName]
  previewShown.value = { ...previewShown.value, [valueName]: want }
  if (want) lastRefreshAt[valueName] = Date.now()
  syncLiveLabels()
}

// Progress tick → re-stamp the shown preview, throttled: Cellpose ticks per XY tile, and each refresh
// re-reads label chunks from disk.
function onProgressTick(): void {
  const now = Date.now()
  let due = false
  for (const p of livePreviews.value) {
    if (!previewShown.value[p.valueName]) continue
    if (!shouldRefreshPreview(lastRefreshAt[p.valueName], now)) continue
    lastRefreshAt[p.valueName] = now
    due = true
  }
  if (due) syncLiveLabels(true)
}

/** Mount once in the viewer window for its image (the window reloads on an image change). Chain nodes
 *  count: a chain-launched segmentation writes the same store, and only the backend's `live_outputs`
 *  makes it previewable. */
export function useLivePreview(imageUid: string) {
  const ws = useWsStore()
  const onLifecycle = () => { void refreshLivePreviews() }
  const LIFECYCLE = ['task:status', 'chain:node:running', 'chain:node:done', 'chain:node:failed'] as const
  onMounted(() => {
    openUid = imageUid
    previewShown.value = {}
    for (const e of LIFECYCLE) ws.on(e, onLifecycle)
    ws.on('task:progress', onProgressTick)
    void refreshLivePreviews()   // a run may already be in flight, and a stale `liveLabels` is cleared
  })
  onUnmounted(() => {
    for (const e of LIFECYCLE) ws.off(e, onLifecycle)
    ws.off('task:progress', onProgressTick)
  })
}
