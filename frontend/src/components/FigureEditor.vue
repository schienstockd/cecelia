<!--
  Edit a still figure on the Movies page and re-make it (docs/todo/FIGURES_PLAN.md, P2). Reads the
  figure's saved spec from its registry entry, applies the edits (`applyFigureEdits`, tested) and
  re-renders it in place through `POST /api/figures/render`. Only the fields a figure request usually
  changes: frames, legend names, tile size, scale bar, crisp/smooth. Crop and channels come from the
  view it was saved from — save a new one from the viewer to change those.
-->
<script setup lang="ts">
import { ref, watch, computed } from 'vue'
import CcToggle from './CcToggle.vue'
import { parseFrames, formatFrames, applyFigureEdits, type FigureSpec } from '../utils/figureSpec'
import { useToast } from '../composables/useToast'

const props = defineProps<{ projectUid: string; name: string }>()
const emit = defineEmits<{ rendered: [name: string] }>()
const toast = useToast()

type Spec = FigureSpec & Record<string, unknown>
const spec = ref<Spec | null>(null)
const imageUid = ref('')
const framesText = ref('')
const names = ref<Record<string, string>>({})
const tileMm = ref<number | null>(null)
const barUm = ref<number | null>(null)
const crisp = ref(true)
const busy = ref(false)
const error = ref('')

const channels = computed(() => [...new Set((spec.value?.rows ?? []).flatMap(r => r.channels.map(c => String(c.c))))])

async function load() {
  spec.value = null; error.value = ''
  const res = await fetch(`/api/movies/meta?projectUid=${encodeURIComponent(props.projectUid)}&name=${encodeURIComponent(props.name)}`)
  const body = await res.json().catch(() => ({}))
  const cfg = body.entry?.config
  if (!cfg?.spec) { error.value = 'No saved spec — save it again from the viewer'; return }
  spec.value = cfg.spec as Spec
  imageUid.value = cfg.imageUid ?? body.entry?.imageUid ?? ''
  framesText.value = formatFrames(spec.value.frames)
  const n: Record<string, string> = {}
  for (const r of spec.value.rows ?? []) for (const c of r.channels) n[String(c.c)] ??= c.name ?? ''
  names.value = n
  tileMm.value = spec.value.style?.tileMm ?? null
  barUm.value = spec.value.scaleBarUm ?? null
  crisp.value = spec.value.interpolation !== 'smooth'
}
watch(() => [props.projectUid, props.name], load, { immediate: true })

async function rerender() {
  if (!spec.value) return
  const f = parseFrames(framesText.value)
  if ('error' in f) { error.value = f.error; return }
  error.value = ''
  const next = applyFigureEdits(spec.value, {
    frames: f.frames, names: names.value, tileMm: tileMm.value ?? undefined,
    scaleBarUm: barUm.value && barUm.value > 0 ? barUm.value : null,
    interpolation: crisp.value ? 'pixels' : 'smooth',
  })
  busy.value = true
  try {
    const res = await fetch('/api/figures/render', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: props.projectUid, imageUid: imageUid.value, spec: next,
                             name: props.name.replace(/\.png$/i, ''), replace: true }),
    })
    const body = await res.json().catch(() => ({}))
    if (!res.ok) { error.value = body.error ?? `HTTP ${res.status}`; return }
    spec.value = next
    toast.add({ severity: 'success', summary: 'Figure re-made', life: 2500 })
    emit('rendered', body.name)
  } finally {
    busy.value = false
  }
}
</script>

<template>
  <div v-if="spec" class="fe cc-row">
    <span class="cc-row-group fe-own-row">
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'Frames: 50, 64 or a range 50-110:10'">frames</span>
      <input v-model="framesText" class="cc-input-2xs fe-text" v-tooltip.bottom="'Frames: 50, 64 or a range 50-110:10'"
             @keydown.enter="rerender" />
    </span>
    <span v-for="c in channels" :key="c" class="cc-row-group">
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'Name in the legend; blank = the channel name'">{{ c }}</span>
      <input v-model="names[c]" class="cc-input-2xs fe-name" :placeholder="c"
             v-tooltip.bottom="'Name in the legend; blank = the channel name'" />
    </span>
    <span class="cc-row-group">
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'Tile width in mm at 300 dpi'">mm</span>
      <input v-model.number="tileMm" type="number" min="5" max="200" class="cc-input-2xs fe-num" placeholder="20"
             v-tooltip.bottom="'Tile width in mm at 300 dpi'" />
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'Scale bar length; blank = none'">bar µm</span>
      <input v-model.number="barUm" type="number" min="0" class="cc-input-2xs fe-num"
             v-tooltip.bottom="'Scale bar length; blank = none'" />
    </span>
    <span class="cc-row-group">
      <CcToggle v-model="crisp" label="crisp pixels" v-tooltip.bottom="'Native pixels as sharp blocks; off = smooth'" />
      <button class="cc-btn cc-btn-primary" :disabled="busy" @click="rerender"
              v-tooltip.bottom="'Re-make this figure with these settings'">
        <i :class="['pi', busy ? 'pi-spin pi-spinner' : 'pi-replay']" /> Re-make
      </button>
    </span>
    <span v-if="error" class="cc-row-group fe-own-row cc-muted-error">{{ error }}</span>
  </div>
  <p v-else-if="error" class="cc-muted cc-fs-2xs">{{ error }}</p>
</template>

<style scoped>
.fe-own-row { flex-basis: 100%; }
.fe-text { flex: 1 1 14rem; }
.fe-name { width: 8rem; }
.fe-num { width: 4.5rem; }
</style>
