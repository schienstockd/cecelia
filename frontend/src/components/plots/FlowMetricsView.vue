<!--
  "Which of these look like cells" — every flow metric plane the UNet can read, for one timepoint, as
  a contact sheet. This is a PRE-TRAINING tool: the metrics are a property of the movie and the
  temporal scales, not of any checkpoint, so it needs no model. You look here, then tick the ones
  worth training on in the Train flow model task (`flowMetrics`).

  That is why the default metric set is a default and not a rule. Three planes ship unticked
  (divergence, vorticity, flow_structure_alignment) because they measured flat on ONE intravital
  dataset — cell/background 0.99, 1.00, 1.65. On other data they may not be, and this panel is how
  you find out instead of inheriting someone else's table.

  NO MODEL APPEARS HERE. The metrics are a property of the movie, the CHANNEL and the temporal
  scales; a checkpoint has nothing to say about them, and the question is asked before one exists.
  An earlier version offered a model picker that added its probability map, which quietly turned a
  "what should I train on" panel into a "what did I train" one. The channel is the choice that
  belongs here — it decides what the flow is even computed over.

  Colour-mapped (viridis by default, server-side in `plane_render.py`). Grey hides the mid-range
  structure that "does this look like cells" is entirely about.

  NOT instances. Instances are segmentation output, and the Segment page already previews them
  through the normal preview path; a second instance renderer here would be the same picture computed
  a different way.

  Sourced from POST /api/optical-flow/inspect, which runs the RESIDENT PREVIEW WORKER — the same
  process, the same `CoastalUtils`, the same temporal window the real run builds. So these are the
  planes the model would actually be fed, not a re-derivation free to drift from it. Fetch-a-PNG-into
  -an-<img> is the crop panel's pattern; the planes are rendered server-side because that is where the
  data is. Nothing here touches napari.
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted, onBeforeUnmount } from 'vue'
import ChipSelect, { type ChipOption } from '../ChipSelect.vue'
import { useProjectStore } from '../../stores/project'
import { debouncedLatest, type RunState } from '../../utils/debouncedLatest'
import { PREVIEW_DEBOUNCE_MS } from '../../utils/taskPreview'

interface Plane { name: string; png: string }
interface FlowState {
  imageUid?: string; valueName?: string
  channels?: string[]; t?: number; z?: number | null
  scales?: string[]                     // temporal scales — only used when no model is picked
  show?: string[]                       // which planes are visible, by name
  colormap?: string
}

const props = defineProps<{ projectUid: string; imageUids?: string[]; state: FlowState }>()
const project = useProjectStore()

// The training task's own default, so the sheet shows what training would compute by default.
const DEFAULT_SCALES = ['1', '2', '4', '8']
const SCALE_OPTIONS: ChipOption[] = ['1', '2', '3', '4', '6', '8', '12', '16']
  .map(v => ({ value: v, label: v }))

const COLORMAPS = ['viridis', 'magma', 'grey']

const planes = ref<Plane[]>([])
const extent = ref({ t: 1, z: 1 })       // slider bounds, from the image's own geometry
const runState = ref<RunState>('idle')
const loading = computed(() => runState.value !== 'idle')
const starting = ref(false)
const error = ref('')

const state = computed(() => props.state)
const t = computed({ get: () => state.value.t ?? 0, set: v => (state.value.t = v) })
// z is nullable — empty means "the middle plane", which is what training reads. The slider needs a
// number, so it shows the middle until the user moves it.
const z = computed({
  get: () => state.value.z ?? Math.floor(extent.value.z / 2),
  set: v => (state.value.z = v),
})
const colormap = computed({
  get: () => state.value.colormap ?? 'viridis',
  set: v => (state.value.colormap = v),
})
const scales = computed({
  get: () => state.value.scales ?? DEFAULT_SCALES,
  set: v => (state.value.scales = v),
})

// Declared BEFORE `load` and its watchers, not after: a watcher getter naming `channels` runs
// immediately at setup, and a `const` below it is still in the temporal dead zone — which throws,
// takes the whole panel's setup with it, and aborts the canvas patch so the vault disappears too.
// Type-checking cannot see this; only the order can.
// The host's selection IS the image list (standard bag) — the page's table on one surface, the
// board's image picker on the other. Names come from the project store; the uids do not.
const nameOf = (uid: string) =>
  project.sets.flatMap(s => s.images).find(i => i.uid === uid)?.name ?? uid
const imageOptions = computed<ChipOption[]>(() =>
  (props.imageUids ?? []).map(uid => ({ value: uid, label: nameOf(uid) })))

// Which channel the flow is computed ON — and the single most consequential control here. With none
// sent, `_project_window` falls back to channel 0, so this sheet was quietly showing flow over the
// SHG channel while the model had been trained on mem-TOM: sparse dots, no error, nothing to say so.
const imageChannels = computed<string[]>(() =>
  project.sets.flatMap(s => s.images).find(i => i.uid === state.value.imageUid)?.channelNames ?? [])
const channelOptions = computed<ChipOption[]>(() =>
  imageChannels.value.map(c => ({ value: c, label: c })))
const channels = computed({
  get: () => state.value.channels ?? [],
  set: v => (state.value.channels = v),
})

// Default to EVERY channel, max-merged the way the segmenter merges them. Defaulting to the first
// is what produced "why is input just sparse dots" — on this data channel 0 is SHG, so the sheet
// opened on a channel nobody meant and looked broken. All-channels is never empty, and the chips
// put the choice on screen one click from narrower.
watch(imageChannels, avail => {
  if (!avail.length) return
  const cur = state.value.channels ?? []
  if (!cur.length || !cur.every(c => avail.includes(c))) state.value.channels = [...avail]
}, { immediate: true })

// The whole request, as of the moment it is built. Every control feeds this ONE object, and it is
// what gets scheduled — so the run that eventually fires carries the settings the user had when they
// stopped moving, not whatever the refs happen to hold when the timer expires.
interface InspectRequest {
  projectUid: string; imageUid: string; valueName: string
  cellChannels: string[]; t: number; z: number | null
  temporalScales: number[]; colormap: string
}
const inspectRequest = computed<InspectRequest | null>(() => state.value.imageUid ? {
  projectUid: props.projectUid, imageUid: state.value.imageUid,
  valueName: state.value.valueName ?? 'default',
  cellChannels: state.value.channels ?? [], t: t.value, z: state.value.z ?? null,
  temporalScales: scales.value.map(Number), colormap: colormap.value,
} : null)

// The worker pays ~18 s of torch imports on first use and answers 202 `starting` until it is up.
// Telling the user to "try again in a moment" and stopping made the panel look broken for a minute,
// so we wait for it — bounded, and with the wait visible rather than a frozen spinner.
const RETRY_MS = 1500
const MAX_WAIT_MS = 120_000
const sleep = (ms: number) => new Promise(r => setTimeout(r, ms))

// Debounced, latest-wins — the same scheduler the task preview uses, for the same reason. A slider
// drag emits an event per pixel and each one is a real flow computation in the worker, so firing per
// event queued a dozen runs whose results then LANDED ONE BY ONE: the sheet visibly flipped through
// every timepoint the user had scrubbed past, seconds after they stopped. Two rules fix it and both
// are needed — the debounce collapses the burst, and `isCurrent()` stops a run that was already in
// flight from painting its (now stale) planes over the one the user is waiting for. Without the
// guard, whichever request happened to finish last would win, which is not the same as the latest.
const scheduler = debouncedLatest<InspectRequest>(async (req, isCurrent) => {
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
      // Show everything by default — a contact sheet you have to unhide plane by plane answers
      // nothing. The chips are for narrowing once you know what you're looking at.
      if (!state.value.show) state.value.show = planes.value.map(p => p.name)
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
  if (!inspectRequest.value) return
  scheduler.schedule(inspectRequest.value)
  scheduler.flush()
}

// Slider bounds come from the ONE geometry route (`api/src/image_geometry.jl`), per VERSION — the
// active version can be a different shape from `default`, so the valueName has to travel with the
// question or the sliders end past the end of the store being read.
async function loadExtent() {
  if (!state.value.imageUid) return
  try {
    const q = new URLSearchParams({ projectUid: props.projectUid, imageUid: state.value.imageUid,
                                    valueName: state.value.valueName ?? 'default' })
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
watch(inspectRequest, req => { if (req) scheduler.schedule(req) })


// Follow the host: seed the first image, and drop a pick that has left the selection (else the panel
// keeps rendering an image the page no longer shows).
watch(imageOptions, opts => {
  const uids = opts.map(o => o.value)
  if (uids.length && (!state.value.imageUid || !uids.includes(state.value.imageUid)))
    state.value.imageUid = uids[0]
}, { immediate: true })

const planeOptions = computed<ChipOption[]>(() => planes.value.map(p => ({ value: p.name, label: p.name })))
const shown = computed(() => planes.value.filter(p => (state.value.show ?? []).includes(p.name)))
</script>

<template>
  <div class="fmv">
    <!-- one auto-hide control strip (docs/UI.md → "Auto-hide panel controls"), so the planes get the
         whole panel — the same treatment UmapView/ImageStripView give their in-body toolbars -->
    <div class="fmv-ctrl cc-panel-controls">
      <div class="cc-row fmv-bar">
        <select class="select-input" :value="state.imageUid ?? ''"
                v-tooltip.top="'Image to compute the metrics on'"
                @change="state.imageUid = ($event.target as HTMLSelectElement).value">
          <option value="" disabled>Image…</option>
          <option v-for="o in imageOptions" :key="o.value" :value="o.value">{{ o.label }}</option>
        </select>
        <select class="select-input fmv-cmap" v-model="colormap"
                v-tooltip.top="'Colour map for every plane'">
          <option v-for="c in COLORMAPS" :key="c" :value="c">{{ c }}</option>
        </select>
        <!-- disabled only while a run is IN FLIGHT, not while one is merely queued: during the
             debounce window this button is how you skip the wait, so greying it out there would
             remove the one control that answers "just render it now" -->
        <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.left="'Reload'"
                :disabled="runState === 'running'" @click="load()">
          <i class="pi pi-refresh" :class="{ 'pi-spin': loading }" />
        </button>
      </div>

      <!-- t / z as sliders: stepping through a movie is a scrub, not a number you type. Side by side
           — two short tracks read as one control, and a row each wasted the panel's height. -->
      <div class="cc-row fmv-sliders">
        <label class="cc-row-group fmv-slider" v-tooltip.top="'Timepoint'">
          <span class="cc-muted cc-fs-xs fmv-axis">t</span>
          <input type="range" class="slider" min="0" :max="extent.t - 1" step="1" :value="t"
                 @input="t = Number(($event.target as HTMLInputElement).value)" />
          <span class="cc-readout cc-fs-xs fmv-val">{{ t }}/{{ extent.t - 1 }}</span>
        </label>
        <label v-if="extent.z > 1" class="cc-row-group fmv-slider"
               v-tooltip.top="'Z plane'">
          <span class="cc-muted cc-fs-xs fmv-axis">z</span>
          <input type="range" class="slider" min="0" :max="extent.z - 1" step="1" :value="z"
                 @input="z = Number(($event.target as HTMLInputElement).value)" />
          <span class="cc-readout cc-fs-xs fmv-val">{{ z }}/{{ extent.z - 1 }}</span>
        </label>
      </div>

      <label v-if="channelOptions.length" class="cc-row fmv-terms">
        <span class="cc-muted cc-fs-xs"
              v-tooltip.top="'Channel the flow is computed on — a model seeds its own'">channel</span>
        <ChipSelect :options="channelOptions" :model-value="channels" multiple aria-label="Channels"
                    @update:model-value="v => channels = v as string[]" />
      </label>

      <label class="cc-row fmv-scales">
        <span class="cc-muted cc-fs-xs"
              v-tooltip.top="'Frame lags — these decide the per-scale magnitude planes'">scales</span>
        <ChipSelect :options="SCALE_OPTIONS" :model-value="scales" multiple aria-label="Temporal scales"
                    @update:model-value="v => scales = v as string[]" />
      </label>

      <ChipSelect v-if="planeOptions.length" :options="planeOptions"
                  :model-value="state.show ?? []" multiple aria-label="Planes"
                  v-tooltip.top="'Which planes to show'"
                  @update:model-value="v => state.show = v as string[]" />
    </div>

    <p v-if="error" class="cc-muted-warn">{{ error }}</p>
    <p v-else-if="starting" class="cc-muted">Starting the preview worker…</p>
    <p v-else-if="!planes.length && !loading" class="cc-muted">
      Pick an image and a channel to see the flow metrics.
    </p>

    <div class="fmv-grid">
      <figure v-for="p in shown" :key="p.name">
        <img :src="`data:image/png;base64,${p.png}`" :alt="p.name" />
        <figcaption class="cc-muted">{{ p.name }}</figcaption>
      </figure>
    </div>
  </div>
</template>

<style scoped>
/* position: relative so the overlaid .fmv-ctrl (.cc-panel-controls) anchors to the plane grid */
.fmv { position: relative; display: flex; flex-direction: column; gap: 0.4rem; height: 100%;
       overflow: auto; }
.fmv-ctrl { display: flex; flex-direction: column; gap: 0.4rem; padding: 4px 6px; }
.fmv-bar { flex-wrap: wrap; }
.fmv-scales { flex-wrap: wrap; gap: 0.4rem; }
.fmv-cmap { max-width: 8rem; }
.fmv-sliders { gap: 0.8rem; }
.fmv-terms { flex-wrap: wrap; gap: 0.4rem; }
/* each slider is a row-GROUP so a label never splits from its track when the strip wraps */
.fmv-slider { flex: 1; min-width: 9rem; align-items: center; gap: 0.4rem; }
.fmv-axis { width: 1ch; }
.fmv-slider .slider { flex: 1; min-width: 5rem; }
.fmv-val { width: 5ch; text-align: right; }   /* + .cc-readout (tabular nums, dim) */
.fmv-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(180px, 1fr));
            gap: 0.5rem; padding: 0.5rem; }
.fmv-grid figure { margin: 0; }
.fmv-grid img { width: 100%; display: block; image-rendering: pixelated;
                border-radius: var(--cc-radius-sm); }
</style>
