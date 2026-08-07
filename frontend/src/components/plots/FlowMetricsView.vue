<!--
  "Which of these look like cells" — every flow metric plane the UNet can read, for one timepoint, as
  a contact sheet. This is a PRE-TRAINING tool: the metrics are a property of the movie and the
  temporal scales, not of any checkpoint, so it needs no model. You look here, then tick the ones
  worth training on in the Train flow model task (`flowMetrics`).

  That is why the default metric set is a default and not a rule. Three planes ship unticked
  (divergence, vorticity, flow_structure_alignment) because they measured flat on ONE intravital
  dataset — cell/background 0.99, 1.00, 1.65. On other data they may not be, and this panel is how
  you find out instead of inheriting someone else's table.

  A model is OPTIONAL and adds exactly one thing: the probability map, which is the UNet's own output
  and cannot exist without it. Which planes that model was trained WITHOUT is deliberately not marked
  here — it is a property of the model, the vault's details modal answers it, and on the sheet it sat
  next to the chips and read as if toggling one would change it.

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

interface Plane { name: string; png: string }
interface FlowState {
  imageUid?: string; valueName?: string; model?: string
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
const loading = ref(false)
const starting = ref(false)
const error = ref('')
const models = ref<{ name: string; label: string }[]>([])

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

async function loadModels() {
  try {
    const r = await fetch('/api/optical-flow/models')
    const d = await r.json()
    models.value = (d.models ?? []).map((m: { name: string; label: string }) =>
      ({ name: m.name, label: m.label }))
  } catch { /* the picker just stays empty; `load` reports the real failure */ }
}

// The worker pays ~18 s of torch imports on first use and answers 202 `starting` until it is up.
// Telling the user to "try again in a moment" and stopping made the panel look broken for a minute,
// so we wait for it — bounded, and with the wait visible rather than a frozen spinner.
const RETRY_MS = 1500
const MAX_WAIT_MS = 120_000
let retry: ReturnType<typeof setTimeout> | null = null
const cancelRetry = () => { if (retry) { clearTimeout(retry); retry = null } }
onBeforeUnmount(cancelRetry)

async function load(since = 0) {
  if (!state.value.imageUid) return
  cancelRetry()
  loading.value = true
  error.value = ''
  try {
    const r = await fetch('/api/optical-flow/inspect', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        projectUid: props.projectUid, imageUid: state.value.imageUid,
        valueName: state.value.valueName ?? 'default', model: state.value.model ?? '',
        cellChannels: state.value.channels ?? [], t: t.value, z: state.value.z ?? null,
        temporalScales: scales.value.map(Number), colormap: colormap.value,
      }),
    })
    const d = await r.json()
    if (!r.ok) throw new Error(d.error ?? `HTTP ${r.status}`)
    if (d.starting) {
      if (since >= MAX_WAIT_MS) throw new Error('Preview worker did not start.')
      starting.value = true
      retry = setTimeout(() => load(since + RETRY_MS), RETRY_MS)
      return
    }
    starting.value = false
    planes.value = d.planes ?? []
    // Show everything by default — a contact sheet you have to unhide plane by plane answers
    // nothing. The chips are for narrowing once you know what you're looking at.
    if (!state.value.show) state.value.show = planes.value.map(p => p.name)
  } catch (e) {
    starting.value = false
    error.value = e instanceof Error ? e.message : String(e)
  } finally {
    if (!retry) loading.value = false
  }
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

onMounted(async () => { await loadModels(); await loadExtent(); await load() })
watch(() => state.value.imageUid, loadExtent)
watch(() => [state.value.imageUid, state.value.model, state.value.t, state.value.z,
             colormap.value, scales.value.join(',')], () => load())

// The host's selection IS the image list (standard bag) — the page's table on one surface, the
// board's image picker on the other. Names come from the project store; the uids do not.
const nameOf = (uid: string) =>
  project.sets.flatMap(s => s.images).find(i => i.uid === uid)?.name ?? uid
const imageOptions = computed<ChipOption[]>(() =>
  (props.imageUids ?? []).map(uid => ({ value: uid, label: nameOf(uid) })))

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
        <select class="select-input" :value="state.model ?? ''"
                v-tooltip.top="'Optional — adds this model’s probability map'"
                @change="state.model = ($event.target as HTMLSelectElement).value">
          <option value="">No model</option>
          <option v-for="m in models" :key="m.name" :value="m.name">{{ m.label }}</option>
        </select>
        <select class="select-input fmv-cmap" v-model="colormap"
                v-tooltip.top="'Colour map for every plane'">
          <option v-for="c in COLORMAPS" :key="c" :value="c">{{ c }}</option>
        </select>
        <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.left="'Reload'"
                :disabled="loading" @click="load()">
          <i class="pi pi-refresh" :class="{ 'pi-spin': loading }" />
        </button>
      </div>

      <!-- t / z as sliders: stepping through a movie is a scrub, not a number you type. Each shows
           its own value, so the readout replaces the spinner rather than adding to it. -->
      <label class="cc-row fmv-slider" v-tooltip.top="'Timepoint'">
        <span class="cc-muted cc-fs-xs fmv-axis">t</span>
        <input type="range" class="slider" min="0" :max="extent.t - 1" step="1" :value="t"
               @input="t = Number(($event.target as HTMLInputElement).value)" />
        <span class="cc-readout cc-fs-xs fmv-val">{{ t }}/{{ extent.t - 1 }}</span>
      </label>
      <label v-if="extent.z > 1" class="cc-row fmv-slider"
             v-tooltip.top="'Z plane — the middle is what training reads'">
        <span class="cc-muted cc-fs-xs fmv-axis">z</span>
        <input type="range" class="slider" min="0" :max="extent.z - 1" step="1" :value="z"
               @input="z = Number(($event.target as HTMLInputElement).value)" />
        <span class="cc-readout cc-fs-xs fmv-val">{{ z }}/{{ extent.z - 1 }}</span>
      </label>

      <!-- Only without a model: with one, the manifest's scales win (inference must match training),
           so a control here would be a lie. -->
      <label v-if="!state.model" class="cc-row fmv-scales">
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
      Pick an image to see the flow metrics.
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
.fmv-slider { align-items: center; gap: 0.4rem; }
.fmv-axis { width: 1ch; }
.fmv-slider .slider { flex: 1; min-width: 6rem; }
.fmv-val { width: 5ch; text-align: right; }   /* + .cc-readout (tabular nums, dim) */
.fmv-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(180px, 1fr));
            gap: 0.4rem; }
.fmv-grid figure { margin: 0; }
.fmv-grid img { width: 100%; display: block; image-rendering: pixelated;
                border-radius: var(--cc-radius-sm); }
</style>
