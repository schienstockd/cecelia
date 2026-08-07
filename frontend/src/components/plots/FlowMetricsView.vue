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
import { computed, watch } from 'vue'
import ChipSelect, { type ChipOption } from '../ChipSelect.vue'
import PlotSpinner from './PlotSpinner.vue'
import { useProjectStore } from '../../stores/project'
import { useFlowPlanes, type FlowPlaneState, type FlowRequest } from '../../composables/useFlowPlanes'

interface FlowState extends FlowPlaneState {
  scales?: string[]                     // temporal scales — this sheet's own choice
  show?: string[]                       // which planes are visible, by name
}

const props = defineProps<{ projectUid: string; imageUids?: string[]; state: FlowState }>()
const project = useProjectStore()

// The training task's own default, so the sheet shows what training would compute by default.
const DEFAULT_SCALES = ['1', '2', '4', '8']
const SCALE_OPTIONS: ChipOption[] = ['1', '2', '3', '4', '6', '8', '12', '16']
  .map(v => ({ value: v, label: v }))

const COLORMAPS = ['viridis', 'magma', 'grey']

const state = computed(() => props.state)

// Which image version the metrics are computed on. Same bug as the probability plot had: this was
// hardcoded to `default`, so the sheet showed flow over the RAW import while a model is trained on the
// denoised one — a different photometric world, and nothing said so. No model here to name the right
// version, so the default is the image's ACTIVE one, matching what a task form resolves to
// (`preferredValueName`) and what the viewer shows.
const image = computed(() =>
  project.sets.flatMap(s => s.images).find(i => i.uid === state.value.imageUid))
const versionOptions = computed<string[]>(() => Object.keys(image.value?.filepaths ?? {}))
const valueName = computed({
  get: () => state.value.valueName ?? '',
  set: v => (state.value.valueName = v),
})
watch(versionOptions, opts => {
  if (!opts.length) return
  if (state.value.valueName && opts.includes(state.value.valueName)) return
  state.value.valueName = image.value?.activeValueName && opts.includes(image.value.activeValueName)
    ? image.value.activeValueName : opts[0]
}, { immediate: true })

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

// The request. `temporalScales` is this sheet's own control — the metric planes depend on it, and
// no model is involved to carry them (see the header).
const request = computed<FlowRequest | null>(() => state.value.imageUid ? {
  projectUid: props.projectUid, imageUid: state.value.imageUid,
  valueName: state.value.valueName || 'default',
  cellChannels: state.value.channels ?? [], t: t.value, z: state.value.z ?? null,
  colormap: colormap.value, temporalScales: scales.value.map(Number),
} : null)

const { planes, extent, runState, loading, showSpinner, starting, error, load } =
  useFlowPlanes(state, request)

// Show everything by default — a contact sheet you have to unhide plane by plane answers nothing.
// The chips are for narrowing once you know what you are looking at.
watch(planes, ps => {
  if (ps.length && !state.value.show) state.value.show = ps.map(p => p.name)
})

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
        <select v-if="versionOptions.length > 1" class="select-input fmv-ver" v-model="valueName"
                v-tooltip.top="'Image version the metrics are computed on'">
          <option v-for="v in versionOptions" :key="v" :value="v">{{ v }}</option>
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

    <div class="fmv-grid" :class="{ 'planes-stale': showSpinner }">
      <figure v-for="p in shown" :key="p.name">
        <img :src="`data:image/png;base64,${p.png}`" :alt="p.name" />
        <figcaption class="cc-muted">{{ p.name }}</figcaption>
      </figure>
    </div>

    <!-- Delayed, so it never flashes on a fast render (docs/UI.md → Plot loading state). The panel is
         `position: relative`, which is what this overlay fills. -->
    <PlotSpinner v-if="showSpinner" label="Rendering…" />
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
.fmv-ver { max-width: 10rem; }
.fmv-sliders { gap: 0.8rem; }
.fmv-terms { flex-wrap: wrap; gap: 0.4rem; }
/* each slider is a row-GROUP so a label never splits from its track when the strip wraps */
.fmv-slider { flex: 1; min-width: 9rem; align-items: center; gap: 0.4rem; }
.fmv-axis { width: 1ch; }
.fmv-slider .slider { flex: 1; min-width: 5rem; }
.fmv-val { width: 5ch; text-align: right; }   /* + .cc-readout (tabular nums, dim) */
.fmv-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(180px, 1fr));
            gap: 0.5rem; padding: 0.5rem; }
/* While a render is queued or running, the planes on screen are the PREVIOUS settings. Dimming says
   so — without it a slow render is indistinguishable from a control that did nothing. Tied to the
   same delayed flag as the wheel, so a quick render never flickers. */
.fmv-grid.planes-stale { opacity: 0.45; transition: opacity 120ms ease; }
.fmv-grid figure { margin: 0; }
.fmv-grid img { width: 100%; display: block; image-rendering: pixelated;
                border-radius: var(--cc-radius-sm); }
</style>
