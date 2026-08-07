<!--
  "What did this model learn" — the optical-flow model's inputs and outputs for one timepoint, as a
  grid of planes: the projected input, the probability map, the instances, and every flow metric the
  model was trained on.

  Sourced from POST /api/optical-flow/inspect, which runs the RESIDENT PREVIEW WORKER — the same
  process, the same `CoastalUtils`, the same temporal window the real run builds. So these are the
  planes the model is actually fed, not a re-derivation that is free to drift from it.

  Hosted on BOTH surfaces from one registry line: the Optical Flow module page's canvas
  (`opticalFlowPage`) and the Analysis board (`analysisBoard`). It reads its images from the standard
  bag's `imageUids`, so on each surface it offers exactly what that surface has selected.

  Nothing here touches napari. The Analysis board's other image slot (`ImageStripView`) is a napari
  SCREENSHOT montage, which is right for "show the pipeline stages as the viewer renders them" and
  wrong for this: these planes are not viewer layers and have no reason to become any.

  Fetch-a-PNG-into-a-blob is the crop panel's pattern (`CropPanel` + `/api/crop/frame`); the planes
  are rendered server-side because that is where the data is.
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted } from 'vue'
import ChipSelect, { type ChipOption } from '../ChipSelect.vue'
import { useProjectStore } from '../../stores/project'

interface Plane { name: string; png: string }
interface FlowState {
  imageUid?: string; valueName?: string; model?: string
  channels?: string[]; t?: number; z?: number | null
  show?: string[]                       // which planes are visible, by name
}

const props = defineProps<{ projectUid: string; imageUids?: string[]; state: FlowState }>()
const project = useProjectStore()

const planes = ref<Plane[]>([])
const loading = ref(false)
const error = ref('')
const models = ref<{ name: string; label: string }[]>([])

const state = computed(() => props.state)
const t = computed({ get: () => state.value.t ?? 0, set: v => (state.value.t = v) })

async function loadModels() {
  try {
    const r = await fetch('/api/optical-flow/models')
    const d = await r.json()
    models.value = (d.models ?? []).map((m: { name: string; label: string }) =>
      ({ name: m.name, label: m.label }))
    if (!state.value.model && models.value.length) state.value.model = models.value[0].name
  } catch { /* the picker just stays empty; `load` reports the real failure */ }
}

async function load() {
  if (!state.value.imageUid || !state.value.model) return
  loading.value = true
  error.value = ''
  try {
    const r = await fetch('/api/optical-flow/inspect', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        projectUid: props.projectUid, imageUid: state.value.imageUid,
        valueName: state.value.valueName ?? 'default', model: state.value.model,
        cellChannels: state.value.channels ?? [], t: t.value, z: state.value.z ?? null,
      }),
    })
    const d = await r.json()
    if (!r.ok) throw new Error(d.error ?? `HTTP ${r.status}`)
    if (d.starting) { error.value = 'Preview worker is starting — try again in a moment.'; return }
    planes.value = d.planes ?? []
    if (!state.value.show?.length) state.value.show = planes.value.slice(0, 3).map(p => p.name)
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
  } finally {
    loading.value = false
  }
}

onMounted(async () => { await loadModels(); await load() })
watch(() => [state.value.imageUid, state.value.model, state.value.t, state.value.z], load)

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

const planeOptions = computed<ChipOption[]>(() =>
  planes.value.map(p => ({ value: p.name, label: p.name.replace(/^flow: /, '') })))
const shown = computed(() => planes.value.filter(p => (state.value.show ?? []).includes(p.name)))
</script>

<template>
  <div class="fmv">
    <!-- one auto-hide control strip (docs/UI.md → "Auto-hide panel controls"), so the planes get the
         whole panel — the same treatment UmapView/ImageStripView give their in-body toolbars -->
    <div class="fmv-ctrl cc-panel-controls">
    <div class="cc-row fmv-bar">
      <select class="select-input" :value="state.imageUid ?? ''"
              v-tooltip.top="'Image to run the model over'"
              @change="state.imageUid = ($event.target as HTMLSelectElement).value">
        <option value="" disabled>Image…</option>
        <option v-for="o in imageOptions" :key="o.value" :value="o.value">{{ o.label }}</option>
      </select>
      <select class="select-input" :value="state.model ?? ''"
              v-tooltip.top="'Trained model from the vault'"
              @change="state.model = ($event.target as HTMLSelectElement).value">
        <option v-for="m in models" :key="m.name" :value="m.name">{{ m.label }}</option>
      </select>
      <label class="cc-muted fmv-t">
        t
        <input type="number" min="0" class="text-input fmv-num" :value="t"
               v-tooltip.top="'Timepoint'"
               @change="t = Number(($event.target as HTMLInputElement).value)" />
      </label>
      <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.left="'Reload'"
              :disabled="loading" @click="load">
        <i class="pi pi-refresh" :class="{ 'pi-spin': loading }" />
      </button>
    </div>

    <ChipSelect v-if="planeOptions.length" :options="planeOptions"
                :model-value="state.show ?? []" multiple aria-label="Planes"
                v-tooltip.top="'Which planes to show'"
                @update:model-value="v => state.show = v as string[]" />
    </div>

    <p v-if="error" class="cc-muted-warn">{{ error }}</p>
    <p v-else-if="!planes.length && !loading" class="cc-muted">
      Pick an image and a model to see what it reads.
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
.fmv-t { display: flex; align-items: center; gap: 0.25rem; }
.fmv-num { width: 5ch; }
.fmv-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(180px, 1fr));
            gap: 0.4rem; }
.fmv-grid figure { margin: 0; }
.fmv-grid img { width: 100%; display: block; image-rendering: pixelated;
                border-radius: var(--cc-radius-sm); }
</style>
