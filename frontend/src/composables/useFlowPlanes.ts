/**
 * The shared half of the two flow canvas plots: ask `/api/optical-flow/inspect` for a set of planes
 * and hold the answer.
 *
 * Extracted because there are two callers and they must not drift. `FlowMetricsView` shows the metric
 * planes a model would be TRAINED on; `FlowProbabilityView` shows one model's probability map. Both
 * are the same request against the same route — the model is the only difference — and both exist to
 * claim "this is what a run is actually fed". Two copies of the request building, the debounce and
 * the stale-response guard would be two chances for one of those claims to stop being true.
 *
 * What lives here: the request, the scheduling, and the geometry the sliders need. What does not: the
 * controls and which planes get rendered, because that is exactly what differs.
 */
import { ref, computed, watch, onMounted, onBeforeUnmount, type Ref } from 'vue'
import { debouncedLatest, type RunState } from '../utils/debouncedLatest'
import { flowRegionLabel, type FlowRegion } from '../utils/flowRegion'
import { PREVIEW_DEBOUNCE_MS } from '../utils/taskPreview'
import { useDelayedLoading } from './useDelayedLoading'

export interface Plane { name: string; png: string }

/** The panel state these plots persist. Callers add their own keys. */
export interface FlowPlaneState {
  [key: string]: unknown
  imageUid?: string
  valueName?: string
  channels?: string[]
  t?: number
  z?: number | null
  colormap?: string
  /** XY pixels per axis, centred — see `FLOW_REGION_OPTIONS`. */
  regionSize?: number
}

export interface FlowRequest {
  projectUid: string
  imageUid: string
  valueName: string
  cellChannels: string[]
  t: number
  z: number | null
  colormap: string
  /** XY pixels per axis, centred on the frame. Always sent, so what is on screen says what it is. */
  regionSize: number
  /** Temporal scales — the metric sheet's own choice; omitted for a model, which carries its own. */
  temporalScales?: number[]
  /** A vault `.pt` name. Present ⇒ the route answers with the probability map instead. */
  model?: string
}

// The worker pays ~18 s of torch imports on first use and answers 202 `starting` until it is up.
// Telling the user to "try again in a moment" and stopping made the panel look broken for a minute,
// so we wait for it — bounded, and with the wait visible rather than a frozen spinner.
const RETRY_MS = 1500
const MAX_WAIT_MS = 120_000
const sleep = (ms: number) => new Promise(r => setTimeout(r, ms))

export function useFlowPlanes(
  state: Ref<FlowPlaneState>,
  /** The request, or `null` when there is nothing to ask for yet. Rebuilt by the caller's computed. */
  request: Ref<FlowRequest | null>,
) {
  const planes = ref<Plane[]>([])
  const extent = ref({ t: 1, z: 1 })     // slider bounds, from the image's own geometry
  // The crop the worker rendered, echoed back in the reply. Held here rather than derived from the
  // request because the server clamps it to the axis length — see `flowRegionLabel`.
  const region = ref<FlowRegion>(null)
  const runState = ref<RunState>('idle')
  const loading = computed(() => runState.value !== 'idle')
  const starting = ref(false)
  const error = ref('')

  // Debounced, latest-wins — the same scheduler the task preview uses, for the same reason. A slider
  // drag emits an event per pixel and each one is real compute in the worker, so firing per event
  // queued a dozen runs whose results then LANDED ONE BY ONE: the sheet visibly flipped through
  // every timepoint the user had scrubbed past, seconds after they stopped. Two rules fix it and
  // both are needed — the debounce collapses the burst, and `isCurrent()` stops a run that was
  // already in flight from painting its (now stale) planes over the one the user is waiting for.
  // Without the guard, whichever request finished last would win, which is not the same as latest.
  const scheduler = debouncedLatest<FlowRequest>(async (req, isCurrent) => {
    error.value = ''
    for (let waited = 0; ; waited += RETRY_MS) {
      const r = await fetch('/api/optical-flow/inspect', {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(req),
      })
      const d = await r.json()
      // superseded while the worker computed — neither the planes NOR an error belong to the current
      // settings any more, so nothing here is applied
      if (!isCurrent()) return
      if (!r.ok) throw new Error(d.error ?? `HTTP ${r.status}`)
      if (!d.starting) {
        starting.value = false
        planes.value = d.planes ?? []
        region.value = d.region ?? null
        return
      }
      if (waited >= MAX_WAIT_MS) throw new Error('Preview worker did not start.')
      starting.value = true
      await sleep(RETRY_MS)
      if (!isCurrent()) return
    }
  }, {
    wait: PREVIEW_DEBOUNCE_MS,
    onState: s => { runState.value = s },
    onError: e => {
      starting.value = false
      error.value = e instanceof Error ? e.message : String(e)
    },
  })
  onBeforeUnmount(() => scheduler.cancel())

  /** Run now, skipping the debounce — the refresh button and the first paint. */
  function load() {
    if (!request.value) return
    scheduler.schedule(request.value)
    scheduler.flush()
  }

  // Slider bounds come from the ONE geometry route (`api/src/image_geometry.jl`), per VERSION — the
  // active version can be a different shape from `default`, so the valueName has to travel with the
  // question or the sliders end past the end of the store being read.
  async function loadExtent() {
    if (!state.value.imageUid) return
    try {
      const q = new URLSearchParams({
        projectUid: request.value?.projectUid ?? '',
        imageUid: state.value.imageUid,
        valueName: state.value.valueName ?? 'default',
      })
      const r = await fetch(`/api/images/geometry?${q}`)
      if (!r.ok) return
      const g = await r.json()
      extent.value = { t: Math.max(1, g.sizeT ?? 1), z: Math.max(1, g.sizeZ ?? 1) }
    } catch { /* the sliders just keep their previous bounds */ }
  }

  // The extent is NOT awaited before the first load: it only sets the slider bounds, and the request
  // sends `z: null` for "the middle plane" (the server resolves it), so the two are independent. When
  // they were sequential, a slow geometry call let the debounce timer expire first and the panel made
  // two requests on mount for the same picture.
  onMounted(() => { void loadExtent(); load() })
  watch(() => state.value.imageUid, loadExtent)
  // One watch over the whole request object: any control that changes it schedules a run, and the
  // scheduler decides when. A new object identity per change is exactly the trigger we want.
  watch(request, req => { if (req) scheduler.schedule(req) })

  // The spinner, decided HERE rather than per view, so both plots answer "is it doing anything?" the
  // same way. `loading` covers the debounce window too (state `pending`), which is the point: the
  // gap the user actually notices is between letting go of a slider and the picture changing, and
  // most of that gap is a request that has not been sent yet.
  //
  // Delayed, per docs/UI.md — but the threshold matters less here than usual, because neither of
  // these plots is ever fast: both are a worker round-trip and the probability map is a forward pass
  // on top. Nothing to protect against a flash on a cheap render, everything to gain from the wheel
  // appearing before the user concludes the control is dead.
  const showSpinner = useDelayedLoading(loading)

  /** `"512 × 512"` — the crop on screen, so the panel never implies it is showing the whole frame. */
  const regionLabel = computed(() => flowRegionLabel(region.value))

  return { planes, extent, regionLabel, runState, loading, showSpinner, starting, error, load }
}
