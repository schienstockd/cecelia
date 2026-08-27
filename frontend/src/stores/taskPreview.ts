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

import { defineStore, acceptHMRUpdate } from 'pinia'
import { computed, ref, watch } from 'vue'
import { previewApi, type SvcError } from '../utils/serviceApi'
import { debouncedLatest, type RunState } from '../utils/debouncedLatest'
import {
  previewBlocker, previewNotice, previewSummary, baseOnlyWarning, tilingWarning,
  compositeWarning, warmPollAction,
  PREVIEW_DEBOUNCE_MS, WORKER_WARM_POLL_MS,
  type PreviewContext, type PreviewStatus, type PreviewBlocker, type PreviewPass,
  previewFailureLog } from '../utils/taskPreview'
import { useWsStore } from './ws'
import { useLogStore } from './log'
import { useViewerStore } from './viewer'

export const useTaskPreviewStore = defineStore('taskPreview', () => {
  // SESSION-ONLY, and a deliberate exception to "persist every user-settable option"
  // (docs/MODULES.md). That rule is about VIEW options: restoring a chart type costs nothing, so
  // losing it is pure annoyance. Restoring this one spawns a 17.7 s Python process, loads a cellpose
  // model into GPU memory and starts inference the moment the user scrolls — a side effect, not a
  // restored preference. Persisted, it meant every session after the first ran previews without
  // anyone asking for one.
  //
  // The store outlives route changes (Pinia is created once per app load), so navigating away from the
  // task page and back does NOT lose the toggle — only a reload/restart does, which is exactly the
  // boundary where resuming GPU work unasked would be wrong. A worker left running from a previous
  // session is adopted rather than relaunched, and Settings → Task preview can stop it.
  //
  // `pinned` additionally MUST NOT persist: a pin holds one result, and the result does not survive a
  // reload. Persisted, `enabled` + `pinned` restored together left the toggle looking on while
  // `previewBlocker` returned 'pinned' — an on-looking preview that could never run.
  const enabled = ref(false)
  const pinned  = ref(false)

  const runState = ref<RunState>('idle')
  const status   = ref<PreviewStatus | null>(null)
  const context  = ref<PreviewContext | null>(null)
  /** Set from the reply's `previewLabels` block after a successful run. The ViewerWindow reads this
   *  and appends `&preview=1` to its labels slab URL while the vn matches; a stop clears it. */
  const previewLabelsActive = ref(false)
  const previewLabels = ref<{ valueName: string; imageUid: string; projectUid: string } | null>(null)
  const counts   = ref<Record<string, number> | null>(null)
  // Per model group, for a multi-pass config — see `previewSummary`. Null for a single pass.
  const passes   = ref<PreviewPass[] | null>(null)
  const fallback2d = ref(false)
  const signal   = ref<{ hasSignal?: boolean; noSignalWhy?: string } | null>(null)
  const tiling   = ref<{ runSeams?: Record<string, number>; blockSize?: number } | null>(null)
  /** composite steps the preview does not run, from the backend (each step's own label) */
  const notPreviewed = ref<Array<{ label?: string; fun?: string }>>([])
  const error    = ref('')
  /** the backend's machine-readable refusal reason — what the notice's severity/label switch on */
  const errorCode = ref('')
  /** true between toggle-on and the worker answering — its imports take ~18 s, so this is visible */
  const starting = ref(false)

  const log = useLogStore()
  const viewerStore = useViewerStore()

  /**
   * Record a failure: the readout AND the error console.
   *
   * One helper because three paths set `error` and every one of them was readout-only. The notice shows
   * ≤4 words with the specifics in a tooltip, which is how an `AttributeError` from the worker reached
   * the user as a bare "Preview failed" — the text was there, one hover away, with nothing saying so.
   * `previewFailureLog` (pure, unit-tested) decides the level and the headline so the console and the
   * button cannot disagree.
   */
  function fail(message: string, code = '') {
    error.value = message
    errorCode.value = code
    const entry = previewFailureLog({ message, code })
    if (entry) log[entry.level](entry.message, { detail: entry.detail, source: entry.source })
  }

  /** Forget the last result. Called whenever it stops describing what is on screen. */
  function clearResult() {
    counts.value = null
    passes.value = null
    fallback2d.value = false
    signal.value = null
    tiling.value = null
    notPreviewed.value = []
    previewLabelsActive.value = false
    previewLabels.value = null
  }

  const blocker = computed<PreviewBlocker | null>(
    () => previewBlocker(context.value, status.value,
      { enabled: enabled.value, pinned: pinned.value, openImage: viewerStore.openImage }))
  /** the one line under the button: what is wrong, and whether it is amber */
  const notice  = computed(() => previewNotice(
    blocker.value, error.value ? { message: error.value, code: errorCode.value } : null))
  const summary = computed(() => previewSummary(counts.value, fallback2d.value, signal.value ?? undefined, passes.value))
  /** a two-model run previews only its base type — say so rather than let it look complete */
  const baseOnly = computed(() => baseOnlyWarning(context.value?.params ?? null))
  /** a run would split this region at a tile seam; the preview segments it whole */
  const tiled    = computed(() => tilingWarning(tiling.value?.runSeams, tiling.value?.blockSize))
  /** a composite runs more steps than it previews */
  const composite = computed(() => compositeWarning(notPreviewed.value))
  /**
   * Every "the run will not look exactly like this" caveat, in one list.
   *
   * Collected here rather than as four near-identical spans in the component: they are the same kind of
   * statement, they render identically, and a fifth (this one) made the duplication the obvious thing
   * to remove rather than extend.
   */
  const warnings = computed(() => [
    { short: summary.value.warn, detail: summary.value.warnDetail },
    baseOnly.value,
    tiled.value,
    composite.value,
  ].filter(w => w.short))
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
    // Region and the browser-viewer's open image come from the viewer store (P7). If the viewer
    // hasn't reported yet the run is skipped — same intent as `no-image-open`, but before spending a
    // request. This never fires in practice: `request()`'s blocker check catches the same state.
    const openImage = viewerStore.openImage
    const region = viewerStore.visibleRegion
    if (!region) return
    const res = await previewApi.run({
      projectUid: ctx.projectUid, imageUid: ctx.imageUid,
      valueName: ctx.valueName, funName: ctx.funName, params: ctx.params ?? {},
      region,
      zarrPath: openImage?.zarrPath, taskDir: openImage?.taskDir,
    })
    // Superseded while cellpose ran: the user has already moved on, so applying this would show a mask
    // for a region that is no longer on screen. The layer the backend just set is replaced by the next
    // run; only the READOUT would lie, so only the readout is guarded.
    if (!isCurrent()) return
    // 202: the worker is warming. Wait it out and re-request — nothing else will.
    if (res?.starting) { starting.value = true; pollUntilWarm(); return }
    starting.value = false
    counts.value = res?.counts ?? null
    passes.value = res?.passes ?? null
    fallback2d.value = Boolean(res?.fallback2d)
    signal.value = { hasSignal: res?.hasSignal, noSignalWhy: res?.noSignalWhy }
    tiling.value = { runSeams: res?.runSeams, blockSize: res?.blockSize }
    notPreviewed.value = Array.isArray(res?.notPreviewed) ? res.notPreviewed : []
    // P7: a labels-shaped reply carries `previewLabels: {valueName, imageUid, projectUid}`; when set,
    // the ViewerWindow's labels slab URL flips to the scratch `<vn>__preview.ome.zarr` for this vn.
    if (res?.previewLabels && typeof res.previewLabels === 'object') {
      previewLabels.value = res.previewLabels
      previewLabelsActive.value = true
    } else {
      previewLabels.value = null
      previewLabelsActive.value = false
    }
    error.value = ''
    errorCode.value = ''
  }, {
    wait: PREVIEW_DEBOUNCE_MS,
    onState: s => { runState.value = s },
    // A failed preview must be visible, not a console-only unhandled rejection — and the previous
    // result must go with it. "12 cells" beside "Wrong version open" reads as twelve cells in THIS
    // version; the count no longer describes anything we accepted.
    onError: e => {
      fail(e instanceof Error ? e.message : String(e), (e as SvcError)?.code ?? '')
      starting.value = false
      clearResult()
    },
  })

  // Pinning has to take effect NOW, not after the queue drains. `previewBlocker` stops new requests,
  // but a request queued a moment before the click would still run — so the readout stayed
  // "Previewing…" and the pin read as a dead button. `dropPending` (not `cancel`) because the run in
  // flight is the freshest there will be and its mask is the one that ends up on screen: superseding
  // it would leave the readout describing an older result.
  // Unpinning must refresh, not wait for the next move. The view has probably moved while pinned (the
  // bridge kept reporting; `request` kept declining), and the bridge dedups against the region it last
  // reported — so there may be no further event to ride, and the user would sit looking at a mask for
  // a region they left. "Follow the view again" has to include catching up to it.
  watch(pinned, v => (v ? scheduler.dropPending() : request()))

  /** Ask for a preview unless something says not to. Safe to call on every keystroke. */
  function request() {
    if (blocker.value !== null || !context.value) return
    scheduler.schedule(context.value)
  }

  // ── waiting out a cold worker ───────────────────────────────────────────────
  // A request that arrives while the worker is still importing gets `202 {starting}` instead of a
  // result, and the worker cannot call back when it is ready. That made the 202 a DEAD END: the UI sat
  // on "Starting…" until the user happened to change a parameter again, so toggling the preview on — or
  // editing a param during the ~18 s warm-up — looked like a preview that never finished. So poll, and
  // re-issue the request that got the 202. Decision logic is `warmPollAction` (pure, tested).
  let warmTimer: ReturnType<typeof setTimeout> | null = null
  let warmStartedAt = 0

  function cancelWarmPoll() {
    if (warmTimer !== null) { clearTimeout(warmTimer); warmTimer = null }
  }

  function pollUntilWarm() {
    if (warmTimer !== null) return          // one loop at a time, however many 202s arrive
    warmStartedAt = Date.now()
    const tick = async () => {
      warmTimer = null
      await refreshStatus()
      // refreshStatus sets `starting` from the backend; it is authoritative from here on
      switch (warmPollAction(status.value, Date.now() - warmStartedAt,
                             { enabled: enabled.value, pinned: pinned.value })) {
        case 'stop':                        // user turned it off or pinned — their choice, not an error
          starting.value = false
          return
        case 'request':
          starting.value = false
          request()
          return
        case 'abandon':
          starting.value = false
          fail('The preview worker did not start.', 'timeout')
          return
        default:
          warmTimer = setTimeout(tick, WORKER_WARM_POLL_MS)
      }
    }
    warmTimer = setTimeout(tick, WORKER_WARM_POLL_MS)
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
    errorCode.value = ''
    starting.value = true
    try {
      // warm first: pays the worker's imports (and, on the first run, the normalisation statistic) at
      // toggle-on rather than making the user's first parameter change look like a 10 s hang
      await previewApi.start()
    } catch (e) {
      fail(e instanceof Error ? e.message : String(e))
    }
    await refreshStatus()
    request()
  }

  async function stop() {
    enabled.value = false
    scheduler.cancel()               // drop pending + supersede in flight, so no late mask lands
    cancelWarmPoll()                 // …and stop waiting on a worker we are about to shut down
    clearResult()
    error.value = ''
    errorCode.value = ''
    starting.value = false
    try {
      // sweeps the preview labels store AND stops the worker — the only thing that releases the
      // model's VRAM. `taskDir` scopes the sweep to the currently-open image.
      await previewApi.stop(viewerStore.openImage?.taskDir)
    } catch (e) {
      fail(e instanceof Error ? e.message : String(e))
    }
    await refreshStatus()
  }

  const toggle = () => (enabled.value ? stop() : start())

  // ── the view-change trigger ─────────────────────────────────────────────────
  // Subscribed once, at store creation, NOT in a component's onMounted: the preview must keep tracking
  // the view while the user is on another page (the worker and the layer both outlive the panel).
  //
  // Under P7, the BROWSER VIEWER writes to `useViewerStore().visibleRegion`, and we watch that
  // directly rather than routing through a WS message: the store update happens in the same tab,
  // debounce is already done at the viewer store's sink, and this saves a round trip through the
  // backend just to reach a peer store. `napari:view-changed` stays as a fallback while other
  // callers still act through napari.
  const ws = useWsStore()
  watch(() => viewerStore.visibleRegion, () => { request() })
  watch(() => viewerStore.openImage, () => {
    // Opening a different image (route load, valueName picker) invalidates the mask on screen — the
    // preview labels store belongs to the previous vn/uid pair.
    scheduler.cancel()
    clearResult()
    void refreshStatus().then(request)
  })
  ws.on('napari:view-changed', () => { request() })
  ws.on('napari:opened', () => {
    scheduler.cancel()
    clearResult()
    void refreshStatus().then(request)
  })

  return {
    enabled, pinned, runState, status, context, counts, passes, fallback2d, signal, tiling,
    error, errorCode, starting,
    previewLabels, previewLabelsActive,
    blocker, notice, summary, busy, baseOnly, tiled, composite, warnings, notPreviewed,
    setContext, request, start, stop, toggle, refreshStatus,
    /** show the current result now, skipping the debounce (the manual "preview now" action) */
    flush: () => scheduler.flush(),
  }
})

// Replace the live instance on hot-reload — see the note in `stores/customModules.ts`.
if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useTaskPreviewStore, import.meta.hot))
