// Task preview — the frontend's single owner of "show me what these params do, here".
//
// A STORE rather than a composable because there is exactly one of everything behind it: one worker
// process holding a GPU model, one napari layer. A per-component composable would let two module pages
// fight over both, and the state would reset on every remount — while the worker kept running.
//
// The decisions live in `utils/taskPreview.ts` and the timing in `utils/debouncedLatest.ts`, both
// unit-tested; this file is the wiring (refs, WS subscription, HTTP) and should stay that way.
//
// Two triggers, one scheduler:
//   * the user edits a parameter    → `setContext`
//   * the user moves the napari view → `napari:view-changed` (the bridge posts it; see
//     `napari_bridge._attach_view_listener`)
// Both funnel into one debounced, latest-wins run, because a preview that fires per event queues
// seconds of stale cellpose behind the one result the user is waiting for.

import { defineStore } from 'pinia'
import { computed, ref, watch } from 'vue'
import { previewApi } from '../utils/serviceApi'
import { debouncedLatest, type RunState } from '../utils/debouncedLatest'
import {
  previewBlocker, blockerMessage, previewSummary, baseOnlyWarning, tilingWarning,
  PREVIEW_DEBOUNCE_MS,
  type PreviewContext, type PreviewStatus, type PreviewBlocker,
} from '../utils/taskPreview'
import { useWsStore } from './ws'

export const useTaskPreviewStore = defineStore('taskPreview', () => {
  // Persisted: both are user-settable options, so they must survive a remount (docs/MODULES.md —
  // "persist every user-settable option"; a bare ref() loses them the moment you navigate away).
  const enabled = ref(localStorage.getItem('cc.taskPreviewEnabled') === 'true')   // default off
  const pinned  = ref(localStorage.getItem('cc.taskPreviewPinned') === 'true')    // default off
  watch(enabled, v => localStorage.setItem('cc.taskPreviewEnabled', String(v)))
  watch(pinned,  v => localStorage.setItem('cc.taskPreviewPinned',  String(v)))

  const runState = ref<RunState>('idle')
  const status   = ref<PreviewStatus | null>(null)
  const context  = ref<PreviewContext | null>(null)
  const counts   = ref<Record<string, number> | null>(null)
  const fallback2d = ref(false)
  const signal   = ref<{ hasSignal?: boolean; noSignalWhy?: string } | null>(null)
  const tiling   = ref<{ runSeams?: Record<string, number>; blockSize?: number } | null>(null)
  const error    = ref('')
  /** true between toggle-on and the worker answering — its imports take ~18 s, so this is visible */
  const starting = ref(false)

  const blocker = computed<PreviewBlocker | null>(
    () => previewBlocker(context.value, status.value, { enabled: enabled.value, pinned: pinned.value }))
  const hint    = computed(() => error.value || blockerMessage(blocker.value))
  const summary = computed(() => previewSummary(counts.value, fallback2d.value, signal.value ?? undefined))
  /** a two-model run previews only its base type — say so rather than let it look complete */
  const baseOnly = computed(() => baseOnlyWarning(context.value?.params ?? null))
  /** a run would split this region at a tile seam; the preview segments it whole */
  const tiled    = computed(() => tilingWarning(tiling.value?.runSeams, tiling.value?.blockSize))
  /** what the toggle shows: a run in flight, or one about to start */
  const busy    = computed(() => runState.value !== 'idle' || starting.value)

  async function refreshStatus() {
    try {
      status.value = await previewApi.status()
      starting.value = Boolean(status.value?.starting)
    } catch {
      status.value = null            // backend unreachable; the blocker becomes 'no-image-open'
    }
  }

  // ── the one scheduler ───────────────────────────────────────────────────────
  const scheduler = debouncedLatest<PreviewContext>(async (ctx, isCurrent) => {
    const res = await previewApi.run({
      projectUid: ctx.projectUid, imageUid: ctx.imageUid,
      valueName: ctx.valueName, funName: ctx.funName, params: ctx.params ?? {},
    })
    // Superseded while cellpose ran: the user has already moved on, so applying this would show a mask
    // for a region that is no longer on screen. The layer the backend just set is replaced by the next
    // run; only the READOUT would lie, so only the readout is guarded.
    if (!isCurrent()) return
    if (res?.starting) { starting.value = true; return }
    starting.value = false
    counts.value = res?.counts ?? null
    fallback2d.value = Boolean(res?.fallback2d)
    signal.value = { hasSignal: res?.hasSignal, noSignalWhy: res?.noSignalWhy }
    tiling.value = { runSeams: res?.runSeams, blockSize: res?.blockSize }
    error.value = ''
  }, {
    wait: PREVIEW_DEBOUNCE_MS,
    onState: s => { runState.value = s },
    // a failed preview must be visible, not a console-only unhandled rejection
    onError: e => { error.value = e instanceof Error ? e.message : String(e) },
  })

  /** Ask for a preview unless something says not to. Safe to call on every keystroke. */
  function request() {
    if (blocker.value !== null || !context.value) return
    scheduler.schedule(context.value)
  }

  /** The module page keeps this current; a change re-previews (debounced). */
  function setContext(ctx: PreviewContext | null) {
    context.value = ctx
    request()
  }

  async function start() {
    enabled.value = true
    pinned.value = false
    error.value = ''
    starting.value = true
    try {
      // warm first: pays the worker's imports (and, on the first run, the normalisation statistic) at
      // toggle-on rather than making the user's first parameter change look like a 10 s hang
      await previewApi.start()
    } catch (e) {
      error.value = e instanceof Error ? e.message : String(e)
    }
    await refreshStatus()
    request()
  }

  async function stop() {
    enabled.value = false
    scheduler.cancel()               // drop pending + supersede in flight, so no late mask lands
    counts.value = null
    fallback2d.value = false
    signal.value = null
    tiling.value = null
    error.value = ''
    starting.value = false
    try {
      // removes the layer AND stops the worker — the only thing that releases the model's VRAM
      await previewApi.stop(context.value?.valueName)
    } catch (e) {
      error.value = e instanceof Error ? e.message : String(e)
    }
    await refreshStatus()
  }

  const toggle = () => (enabled.value ? stop() : start())

  // ── the view-change trigger ─────────────────────────────────────────────────
  // Subscribed once, at store creation, NOT in a component's onMounted: the preview must keep tracking
  // the view while the user is on another page (the worker and the layer both outlive the panel).
  const ws = useWsStore()
  ws.on('napari:view-changed', () => { request() })
  ws.on('napari:opened', () => {
    // Opening an image calls `layers.clear()` bridge-side, so the preview layer is gone. Clear the
    // readout with it — otherwise it keeps reporting "42 cells" for a mask nobody can see, which is
    // worse than reporting nothing. The worker stays warm on purpose (that is what it is for); a new
    // preview is requested below if the params still apply to what is now open.
    scheduler.cancel()
    counts.value = null
    fallback2d.value = false
    signal.value = null
    tiling.value = null
    void refreshStatus().then(request)
  })

  return {
    enabled, pinned, runState, status, context, counts, fallback2d, signal, tiling, error, starting,
    blocker, hint, summary, busy, baseOnly, tiled,
    setContext, request, start, stop, toggle, refreshStatus,
    /** show the current result now, skipping the debounce (the manual "preview now" action) */
    flush: () => scheduler.flush(),
  }
})
