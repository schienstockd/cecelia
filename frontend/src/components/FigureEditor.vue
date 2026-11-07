<!--
  Edit a still figure on the Movies page and re-make it (docs/todo/FIGURES_PLAN.md, P2). Reads the
  figure's saved spec from its registry entry, applies the edits (`applyFigureEdits`, tested) and
  re-renders it in place through `POST /api/figures/render`. Only the fields a figure request usually
  changes: frames (the same range + every as the viewer's Save as figure), legend names, tile size,
  scale bar, crisp/smooth, strip or grid. Crop and channels come from the
  view it was saved from — save a new one from the viewer to change those.
-->
<script setup lang="ts">
import { ref, watch, computed } from 'vue'
import ChipSelect, { type ChipOption } from './ChipSelect.vue'
import InlineNote from './InlineNote.vue'
import MovieTimeRange from './MovieTimeRange.vue'
import { applyFigureEdits, figureFrames, framesAsRange, isSplit, type FigureSpec } from '../utils/figureSpec'
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
const split = ref(false)
const grid = ref(false)
const busy = ref(false)
const error = ref('')

// the merge's channels, each with its colour (the legend input's dot)
const channels = computed(() => {
  const seen = new Map<string, string>()
  for (const r of spec.value?.rows ?? []) for (const c of r.channels) if (!seen.has(String(c.c))) seen.set(String(c.c), c.colour)
  return [...seen].map(([c, colour]) => ({ c, colour }))
})
const shownFrames = computed(() => customFrames.value ?? picked.value.frames)
// crisp / split / grid as one chip row — three switches in a row read as clutter
const LOOK_OPTIONS: ChipOption[] = [
  { value: 'crisp', label: 'crisp' }, { value: 'split', label: 'split' }, { value: 'grid', label: 'grid' },
]
const look = computed<string[]>({
  get: () => [crisp.value && 'crisp', split.value && 'split', grid.value && 'grid'].filter(Boolean) as string[],
  set: v => { crisp.value = v.includes('crisp'); split.value = v.includes('split'); grid.value = v.includes('grid') },
})

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
  split.value = isSplit(spec.value.rows)
  grid.value = spec.value.layout === 'grid'
}
watch(() => [props.projectUid, props.name], load, { immediate: true })

async function rerender() {
  if (!spec.value) return
  error.value = ''
  const next = applyFigureEdits(spec.value, {
    frames: customFrames.value ?? picked.value.frames, names: names.value, split: split.value, grid: grid.value, tileMm: tileMm.value ?? undefined,
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
  <!-- Three lines: which frames · what the legend says · how it looks + Re-make. -->
  <div v-if="spec" class="fe cc-row">
    <InlineNote v-if="staleReason" class="fe-own-row" severity="warn" short="Out of date"
                :detail="`${staleReason} — Re-make renders it from the current data`" />
    <MovieTimeRange v-if="frameCount > 1" class="fe-own-row" :frames="frameCount"
                    :t-start="tStart" :t-end="tEnd" show-every :every="every" :every-auto="picked.every"
                    @update:t-start="tStart = $event; customFrames = null"
                    @update:t-end="tEnd = $event; customFrames = null"
                    @update:every="every = $event; customFrames = null" />
    <span class="cc-row-group fe-own-row">
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'Names in the legend; blank = the channel name'">legend</span>
      <span v-for="ch in channels" :key="ch.c" class="fe-ch">
        <span class="cc-dot" :style="{ background: ch.colour }" />
        <input v-model="names[ch.c]" class="cc-input-2xs fe-name" :placeholder="ch.c"
               v-tooltip.bottom="`${ch.c} in the legend; blank = its name`" />
      </span>
    </span>
    <span class="cc-row-group fe-own-row">
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs">look</span>
      <ChipSelect multiple :options="LOOK_OPTIONS" v-model="look" aria-label="Figure look"
                  v-tooltip.bottom="'crisp: native pixels as blocks · split: a grey row per channel · grid: frames in a square'" />
      <input v-model.number="tileMm" type="number" min="5" max="200" class="cc-input-2xs fe-num" placeholder="mm"
             v-tooltip.bottom="'Tile width in mm; blank = auto'" />
      <input v-model.number="barUm" type="number" min="0" class="cc-input-2xs fe-num" placeholder="bar µm"
             v-tooltip.bottom="'Scale bar in µm; blank = none'" />
      <span class="fe-go">
        <span class="cc-muted cc-fs-2xs" v-tooltip.bottom="shownFrames.join(', ')">
          {{ shownFrames.length }} frame{{ shownFrames.length === 1 ? '' : 's' }}{{ customFrames ? ' (as saved)' : '' }}
        </span>
        <button class="cc-btn cc-btn-primary" :disabled="busy" @click="rerender"
                v-tooltip.bottom="'Re-make this figure with these settings'">
          <i :class="['pi', busy ? 'pi-spin pi-spinner' : 'pi-replay']" /> Re-make
        </button>
      </span>
    </span>
    <span v-if="error" class="cc-row-group fe-own-row cc-muted-error">{{ error }}</span>
  </div>
  <p v-else-if="error" class="cc-muted cc-fs-2xs">{{ error }}</p>
</template>

<style scoped>
.fe-own-row { flex-basis: 100%; }
.fe-ch { display: inline-flex; align-items: center; gap: 0.3rem; }
.fe-name { width: 7rem; }
.fe-num { width: 4rem; }
/* frame count + Re-make sit at the end of the look line */
.fe-go { margin-left: auto; display: inline-flex; align-items: center; gap: 0.5rem; }
</style>
