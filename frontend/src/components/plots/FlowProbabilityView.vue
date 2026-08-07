<!--
  "Did this model learn anything" — the projected input beside the model's probability map.

  The POST-training counterpart to FlowMetricsView, and deliberately a separate plot rather than a
  toggle on that one. The metric sheet is asked BEFORE a model exists and must not take one: an
  earlier version offered a model picker there and it quietly turned "what should I train on" into
  "what did I train". This plot is meaningless without a checkpoint, so it is the other question with
  its own panel.

  Two planes, not instances. Instances are segmentation output — the Segment page previews them
  through the normal preview path — and they hide the thing you want to see behind a threshold and a
  region-growing step. Whether the model can tell cell from background at all is exactly what the
  probability map shows and what a label mask cannot.

  The MODEL COMES FROM THE VAULT, like the convergence plot: the vault owns the selection and its
  global/local scope, the way the population manager owns which pops the plots highlight. No picker
  here — two pickers for one thing is how a canvas ends up with panels that disagree about what they
  are showing.

  `predict_frame` returns `(prob_map, instances, props)` and a real run throws the first away
  (`CoastalUtils._predict_plane`), so the worker's `opticalFlow.probability` backend is the one place
  that value is looked at. Same window, same projection, same metric build as the run — the request
  machinery is shared with the metric sheet (`useFlowPlanes`) precisely so the two cannot drift.
-->
<script setup lang="ts">
import { computed, watch } from 'vue'
import ChipSelect, { type ChipOption } from '../ChipSelect.vue'
import { useProjectStore } from '../../stores/project'
import { useFlowPlanes, type FlowPlaneState, type FlowRequest } from '../../composables/useFlowPlanes'

const COLORMAPS = ['viridis', 'magma', 'grey']

const props = defineProps<{
  projectUid: string
  imageUids?: string[]
  state: FlowPlaneState
  /** The vault's selection, resolved by the canvas for the current scope. */
  model?: string
}>()
const project = useProjectStore()

const state = computed(() => props.state)
const t = computed({ get: () => state.value.t ?? 0, set: v => (state.value.t = v) })
const z = computed({
  get: () => state.value.z ?? Math.floor(extent.value.z / 2),
  set: v => (state.value.z = v),
})
const colormap = computed({
  get: () => state.value.colormap ?? 'viridis',
  set: v => (state.value.colormap = v),
})

// Declared BEFORE the watches that name them: a watch source runs immediately at setup, and a `const`
// below it is still in the temporal dead zone — which throws, takes the panel's setup with it, and
// aborts the canvas patch so every sibling panel vanishes too. Enforced by `utils/setupOrder.ts`.
const nameOf = (uid: string) =>
  project.sets.flatMap(s => s.images).find(i => i.uid === uid)?.name ?? uid
const imageOptions = computed<ChipOption[]>(() =>
  (props.imageUids ?? []).map(uid => ({ value: uid, label: nameOf(uid) })))

const imageChannels = computed<string[]>(() =>
  project.sets.flatMap(s => s.images).find(i => i.uid === state.value.imageUid)?.channelNames ?? [])
const channelOptions = computed<ChipOption[]>(() =>
  imageChannels.value.map(c => ({ value: c, label: c })))
const channels = computed({
  get: () => state.value.channels ?? [],
  set: v => (state.value.channels = v),
})

// Every channel by default, max-merged the way the segmenter merges them. Defaulting to the first is
// what made the metric sheet open on SHG and look broken; all-channels is never empty.
watch(imageChannels, avail => {
  if (!avail.length) return
  const cur = state.value.channels ?? []
  if (!cur.length || !cur.every(c => avail.includes(c))) state.value.channels = [...avail]
}, { immediate: true })

// `model` is what makes this the probability view — the route dispatches on its presence. No model
// means no request at all rather than a metric sheet nobody asked for.
const request = computed<FlowRequest | null>(() =>
  state.value.imageUid && props.model
    ? {
        projectUid: props.projectUid, imageUid: state.value.imageUid,
        valueName: state.value.valueName ?? 'default',
        cellChannels: state.value.channels ?? [], t: t.value, z: state.value.z ?? null,
        colormap: colormap.value, model: props.model,
      }
    : null)

const { planes, extent, runState, loading, starting, error, load } = useFlowPlanes(state, request)

// Follow the host: seed the first image, and drop a pick that has left the selection.
watch(imageOptions, opts => {
  const uids = opts.map(o => o.value)
  if (uids.length && (!state.value.imageUid || !uids.includes(state.value.imageUid)))
    state.value.imageUid = uids[0]
}, { immediate: true })
</script>

<template>
  <div class="fpv">
    <div class="fpv-ctrl cc-panel-controls">
      <div class="cc-row fpv-bar">
        <select class="select-input" :value="state.imageUid ?? ''"
                v-tooltip.top="'Image to run the model on'"
                @change="state.imageUid = ($event.target as HTMLSelectElement).value">
          <option value="" disabled>Image…</option>
          <option v-for="o in imageOptions" :key="o.value" :value="o.value">{{ o.label }}</option>
        </select>
        <select class="select-input fpv-cmap" v-model="colormap"
                v-tooltip.top="'Colour map for both planes'">
          <option v-for="c in COLORMAPS" :key="c" :value="c">{{ c }}</option>
        </select>
        <!-- disabled only while a run is IN FLIGHT, not while one is merely queued: during the
             debounce window this button is how you skip the wait -->
        <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.left="'Reload'"
                :disabled="runState === 'running'" @click="load()">
          <i class="pi pi-refresh" :class="{ 'pi-spin': loading }" />
        </button>
      </div>

      <div class="cc-row fpv-sliders">
        <label class="cc-row-group fpv-slider" v-tooltip.top="'Timepoint'">
          <span class="cc-muted cc-fs-xs fpv-axis">t</span>
          <input type="range" class="slider" min="0" :max="extent.t - 1" step="1" :value="t"
                 @input="t = Number(($event.target as HTMLInputElement).value)" />
          <span class="cc-readout cc-fs-xs fpv-val">{{ t }}/{{ extent.t - 1 }}</span>
        </label>
        <label v-if="extent.z > 1" class="cc-row-group fpv-slider" v-tooltip.top="'Z plane'">
          <span class="cc-muted cc-fs-xs fpv-axis">z</span>
          <input type="range" class="slider" min="0" :max="extent.z - 1" step="1" :value="z"
                 @input="z = Number(($event.target as HTMLInputElement).value)" />
          <span class="cc-readout cc-fs-xs fpv-val">{{ z }}/{{ extent.z - 1 }}</span>
        </label>
      </div>

      <label v-if="channelOptions.length" class="cc-row fpv-terms">
        <span class="cc-muted cc-fs-xs"
              v-tooltip.top="'Channel the flow is computed on'">channel</span>
        <ChipSelect :options="channelOptions" :model-value="channels" multiple aria-label="Channels"
                    @update:model-value="v => channels = v as string[]" />
      </label>
    </div>

    <p v-if="error" class="cc-muted-warn">{{ error }}</p>
    <p v-else-if="starting" class="cc-muted">Starting the preview worker…</p>
    <p v-else-if="!model" class="cc-muted">Select a model in the vault.</p>
    <p v-else-if="!planes.length && !loading" class="cc-muted">
      Pick an image to see what the model predicts.
    </p>

    <div class="fpv-grid">
      <figure v-for="p in planes" :key="p.name">
        <img :src="`data:image/png;base64,${p.png}`" :alt="p.name" />
        <figcaption class="cc-muted">{{ p.name }}</figcaption>
      </figure>
    </div>
  </div>
</template>

<style scoped>
/* position: relative so the overlaid .fpv-ctrl (.cc-panel-controls) anchors to the plane grid */
.fpv { position: relative; display: flex; flex-direction: column; gap: 0.4rem; height: 100%;
       overflow: auto; }
.fpv-ctrl { display: flex; flex-direction: column; gap: 0.4rem; padding: 4px 6px; }
.fpv-bar { flex-wrap: wrap; }
.fpv-cmap { max-width: 8rem; }
.fpv-sliders { gap: 0.8rem; }
.fpv-terms { flex-wrap: wrap; gap: 0.4rem; }
/* each slider is a row-GROUP so a label never splits from its track when the strip wraps */
.fpv-slider { flex: 1; min-width: 9rem; align-items: center; gap: 0.4rem; }
.fpv-axis { width: 1ch; }
.fpv-slider .slider { flex: 1; min-width: 5rem; }
.fpv-val { width: 5ch; text-align: right; }   /* + .cc-readout (tabular nums, dim) */
/* Two planes side by side — the comparison IS the plot, so they must not stack until it is narrow. */
.fpv-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
            gap: 0.5rem; padding: 0.5rem; }
.fpv-grid figure { margin: 0; }
.fpv-grid img { width: 100%; display: block; image-rendering: pixelated;
                border-radius: var(--cc-radius-sm); }
</style>
