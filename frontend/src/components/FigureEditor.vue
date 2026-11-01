<!--
  Edit a still figure on the Movies page and re-make it (docs/todo/FIGURES_PLAN.md, P2). Reads the
  figure's saved spec from its registry entry, applies the edits (`applyFigureEdits`, tested) and
  re-renders it in place through `POST /api/figures/render`. Only the fields a figure request usually
  changes: frames (the same range + every as the viewer's Save as figure), legend names, tile size,
  scale bar, crisp/smooth. Crop and channels come from the
  view it was saved from — save a new one from the viewer to change those.
-->
<script setup lang="ts">
import { ref, watch, computed } from 'vue'
import CcToggle from './CcToggle.vue'
import InlineNote from './InlineNote.vue'
import MovieTimeRange from './MovieTimeRange.vue'
import { applyFigureEdits, figureFrames, framesAsRange, type FigureSpec } from '../utils/figureSpec'
import { resolveFrameRange, storeFrameEnd } from '../utils/batchMovie'
import { useToast } from '../composables/useToast'
import { fetchMovieEntry } from '../utils/movieRestore'

const props = defineProps<{ projectUid: string; name: string; staleReason?: string
                             /** the image's frame count — the range slider's length */
                             frameCount: number }>()
const emit = defineEmits<{ rendered: [name: string] }>()
const toast = useToast()

type Spec = FigureSpec & Record<string, unknown>
const spec = ref<Spec | null>(null)
const imageUid = ref('')
const tStart = ref(0)
const tEnd = ref<number | null>(null)
const every = ref<number | null>(null)
// A spec whose frames are no range + interval (a list Claude chose) keeps them until the range is moved.
const customFrames = ref<number[] | null>(null)
const picked = computed(() => {
  const r = resolveFrameRange(tStart.value, tEnd.value, props.frameCount)
  return figureFrames(r.lo, r.hi, every.value)
})
const names = ref<Record<string, string>>({})
const tileMm = ref<number | null>(null)
const barUm = ref<number | null>(null)
const crisp = ref(true)
const busy = ref(false)
const error = ref('')

const channels = computed(() => [...new Set((spec.value?.rows ?? []).flatMap(r => r.channels.map(c => String(c.c))))])

async function load() {
  spec.value = null; error.value = ''
  const entry = await fetchMovieEntry(props.projectUid, props.name).catch(() => ({} as Record<string, unknown>))
  const cfg = entry.config as { spec?: Spec; imageUid?: string } | undefined
  if (!cfg?.spec) { error.value = 'No saved spec — save it again from the viewer'; return }
  spec.value = cfg.spec as Spec
  imageUid.value = cfg.imageUid ?? String(entry.imageUid ?? '')
  const fr = framesAsRange(spec.value.frames ?? [0])
  tStart.value = fr.lo
  tEnd.value = storeFrameEnd(fr.hi, props.frameCount)
  every.value = fr.every
  customFrames.value = fr.exact ? null : [...(spec.value.frames ?? [])]
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
  error.value = ''
  const next = applyFigureEdits(spec.value, {
    frames: customFrames.value ?? picked.value.frames, names: names.value, tileMm: tileMm.value ?? undefined,
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
    <InlineNote v-if="staleReason" class="fe-own-row" severity="warn" short="Out of date"
                :detail="`${staleReason} — Re-make renders it from the current data`" />
    <MovieTimeRange v-if="frameCount > 1" class="fe-own-row" :frames="frameCount"
                    :t-start="tStart" :t-end="tEnd" show-every :every="every" :every-auto="picked.every"
                    @update:t-start="tStart = $event; customFrames = null"
                    @update:t-end="tEnd = $event; customFrames = null"
                    @update:every="every = $event; customFrames = null" />
    <span class="cc-row-group fe-own-row cc-muted cc-fs-2xs">
      {{ (customFrames ?? picked.frames).length }} frame{{ (customFrames ?? picked.frames).length === 1 ? '' : 's' }}:
      {{ (customFrames ?? picked.frames).join(', ') }}{{ customFrames ? ' (as saved)' : '' }}
    </span>
    <span v-for="c in channels" :key="c" class="cc-row-group">
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'Name in the legend; blank = the channel name'">{{ c }}</span>
      <input v-model="names[c]" class="cc-input-2xs fe-name" :placeholder="c"
             v-tooltip.bottom="'Name in the legend; blank = the channel name'" />
    </span>
    <span class="cc-row-group">
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'Tile width in mm at 300 dpi'">mm</span>
      <input v-model.number="tileMm" type="number" min="5" max="200" class="cc-input-2xs fe-num" placeholder="auto"
             v-tooltip.bottom="'Tile width in mm at 300 dpi; blank = auto (fills 180 mm)'" />
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
.fe-name { width: 8rem; }
.fe-num { width: 4.5rem; }
</style>
