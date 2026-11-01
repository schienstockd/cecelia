<!--
  The project's figure style (docs/todo/FIGURES_PLAN.md, Decision B): the house style with the project's
  overrides, edited in a form beside a diagram of a figure drawn in it — the renderer's proportions
  (`figureLayout`, mirroring `render_figure.py`), so text size against tile size reads before anything
  renders. Saved to `{project}/settings/figureStyle.json` (`POST /api/figures/style`); figures made in
  the old style then list as out of date.
-->
<script setup lang="ts">
import { ref, computed, watch } from 'vue'
import ChipSelect from './ChipSelect.vue'
import { useToast } from '../composables/useToast'
import { HOUSE_STYLE, STYLE_LIMITS, effectiveStyle, styleOverrides, figureLayout, sampleTimeLabel,
         type FigureStyle } from '../utils/figureStyle'

const props = defineProps<{ projectUid: string }>()
const emit = defineEmits<{ saved: [] }>()
const toast = useToast()

const draft = ref<FigureStyle>({ ...HOUSE_STYLE })
const frames = ref<string>('3')     // the preview's frame count (a ChipSelect value is a string)
const busy = ref(false)
const error = ref('')

async function load() {
  error.value = ''
  const res = await fetch(`/api/figures/style?projectUid=${encodeURIComponent(props.projectUid)}`)
  const body = await res.json().catch(() => ({}))
  draft.value = effectiveStyle(body.style)
  saved.value = JSON.stringify(draft.value)
}
watch(() => props.projectUid, load, { immediate: true })

async function save(style: Partial<FigureStyle>) {
  busy.value = true
  error.value = ''
  try {
    const res = await fetch('/api/figures/style', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: props.projectUid, style }),
    })
    const body = await res.json().catch(() => ({}))
    if (!res.ok) { error.value = body.error ?? `HTTP ${res.status}`; return }
    draft.value = effectiveStyle(body.style)
    saved.value = JSON.stringify(draft.value)
    toast.add({ severity: 'success', summary: 'Figure style saved', life: 2500 })
    emit('saved')
  } finally {
    busy.value = false
  }
}

// The fields, grouped as the figure reads: text, tiles, the scale bar, the file. Limits come from the
// style file; labels and tooltips are one phrase each.
type Num = { key: keyof FigureStyle; label: string; tip: string; step: number }
const GROUPS: { title: string; nums: Num[] }[] = [
  { title: 'Text', nums: [
    { key: 'fontPt', label: 'titles pt', tip: 'Column titles and legend', step: 0.5 },
    { key: 'scaleFontPt', label: 'bar label pt', tip: 'The scale-bar label', step: 0.5 } ] },
  { title: 'Tiles', nums: [
    { key: 'figureWidthMm', label: 'page mm', tip: 'Width an auto strip fills', step: 5 },
    { key: 'tileMmMin', label: 'min mm', tip: 'Smallest auto tile', step: 1 },
    { key: 'tileMmMax', label: 'max mm', tip: 'Largest auto tile — a still', step: 1 },
    { key: 'gapMm', label: 'gap mm', tip: 'White gap between tiles', step: 0.1 } ] },
  { title: 'Scale bar', nums: [
    { key: 'barMm', label: 'thickness mm', tip: 'Scale-bar thickness', step: 0.05 } ] },
  { title: 'Output', nums: [
    { key: 'dpi', label: 'dpi', tip: 'Output resolution', step: 50 } ] },
]
const lim = (k: string) => STYLE_LIMITS[k] ?? [0, 1000]
// the numeric keys of the draft, as one record the form binds to
const nums = computed(() => draft.value as unknown as Record<string, number>)
const tileFixed = computed({
  get: () => draft.value.tileMm ?? '',
  set: (v: string | number) => { draft.value.tileMm = v === '' || v === null ? null : Number(v) },
})

const BAR_OPTS = [{ value: 'first', label: 'first' }, { value: 'last', label: 'last' }, { value: 'all', label: 'all' }]
const TIME_OPTS = [{ value: 'clock', label: '0:12:30' }, { value: 'min', label: '12.5 min' }]
const FRAME_OPTS = [{ value: '1', label: 'still' }, { value: '3', label: '3' }, { value: '9', label: '9' }]

const layout = computed(() => figureLayout(draft.value, Number(frames.value), 1))
// what was last loaded or saved — Save is live only once something changed
const saved = ref<string>(JSON.stringify(HOUSE_STYLE))
const dirty = computed(() => JSON.stringify(draft.value) !== saved.value)
</script>

<template>
  <div class="fs">
    <div class="fs-grid cc-fs-2xs">
      <template v-for="g in GROUPS" :key="g.title">
        <div class="fs-group cc-eyebrow">{{ g.title }}</div>
        <template v-for="n in g.nums" :key="n.key">
          <div class="fs-label cc-muted" v-tooltip.bottom="n.tip">{{ n.label }}</div>
          <div class="fs-cell">
            <input type="number" class="cc-input-2xs fs-num" :min="lim(n.key)[0]" :max="lim(n.key)[1]"
                   :step="n.step" v-model.number="nums[n.key]" v-tooltip.bottom="n.tip" />
          </div>
        </template>
        <template v-if="g.title === 'Tiles'">
          <div class="fs-label cc-muted" v-tooltip.bottom="'A fixed tile width; blank = auto by frame count'">fixed mm</div>
          <div class="fs-cell">
            <input type="number" class="cc-input-2xs fs-num" placeholder="auto" v-model="tileFixed"
                   v-tooltip.bottom="'A fixed tile width; blank = auto by frame count'" />
          </div>
        </template>
        <template v-if="g.title === 'Text'">
          <div class="fs-label cc-muted" v-tooltip.bottom="'How column titles show time'">time</div>
          <div class="fs-cell">
            <ChipSelect variant="segmented" :options="TIME_OPTS" v-model="draft.timeFormat" aria-label="Time format" />
          </div>
        </template>
        <template v-if="g.title === 'Scale bar'">
          <div class="fs-label cc-muted" v-tooltip.bottom="'Which tile carries the scale bar'">on</div>
          <div class="fs-cell">
            <ChipSelect variant="segmented" :options="BAR_OPTS" v-model="draft.scaleBarOn" aria-label="Scale bar on" />
          </div>
        </template>
      </template>
    </div>

    <div class="fs-preview">
      <div class="cc-row cc-row-tight">
        <span class="cc-eyebrow cc-fs-2xs">Preview</span>
        <ChipSelect variant="segmented" :options="FRAME_OPTS" v-model="frames" aria-label="Preview frames"
                    v-tooltip.bottom="'Preview a still, or a strip of 3 or 9'" />
      </div>
      <!-- A figure in this style, to scale: tiles, gaps, column titles, legend band, scale bar -->
      <svg class="fs-svg" :viewBox="`-2 -2 ${layout.widthMm + 4} ${layout.heightMm + 4}`" role="img"
           aria-label="Figure style preview">
        <rect :x="-2" :y="-2" :width="layout.widthMm + 4" :height="layout.heightMm + 4" fill="#fff" />
        <text v-for="(t, i) in layout.titles" :key="'t' + i" :x="t.x" :y="t.y" :font-size="layout.font"
              text-anchor="middle" fill="#000" font-family="Arial, Helvetica, sans-serif">
          {{ sampleTimeLabel(draft.timeFormat, 12.5 + i * 15) }}</text>
        <g v-for="(t, i) in layout.tiles" :key="'c' + i">
          <rect :x="t.x" :y="t.y" :width="layout.tile" :height="layout.tile" fill="#000" />
          <!-- a cell in the merge's two colours, kept small and dim so the text and bar read first -->
          <circle :cx="t.x + layout.tile * 0.5" :cy="t.y + layout.tile * 0.5" :r="layout.tile * 0.22"
                  fill="none" stroke="#2f8f2f" :stroke-width="layout.tile * 0.05" />
          <circle :cx="t.x + layout.tile * 0.54" :cy="t.y + layout.tile * 0.54" :r="layout.tile * 0.06" fill="#a020a0" />
          <g v-if="t.bar">
            <rect :x="t.x + layout.tile * 0.75 - 1" :y="t.y + layout.tile - 1 - layout.bar"
                  :width="layout.tile * 0.2" :height="layout.bar" fill="#fff" />
            <text :x="t.x + layout.tile * 0.85 - 1" :y="t.y + layout.tile - 1.6 - layout.bar"
                  :font-size="layout.scaleFont" text-anchor="middle" fill="#fff"
                  font-family="Arial, Helvetica, sans-serif">5 µm</text>
          </g>
        </g>
        <g v-for="(b, i) in layout.bands" :key="'b' + i">
          <rect x="0" :y="b.y" :width="layout.widthMm" :height="layout.band" fill="#000" />
          <text :x="layout.widthMm / 2" :y="b.y + layout.band * 0.7" :font-size="layout.font" text-anchor="middle"
                font-family="Arial, Helvetica, sans-serif"><tspan fill="#f0f">tdTom</tspan><tspan fill="#3c3"> Katushka</tspan></text>
        </g>
      </svg>
      <span class="cc-muted cc-fs-2xs">{{ layout.widthMm.toFixed(0) }} × {{ layout.heightMm.toFixed(0) }} mm ·
        tile {{ layout.tile.toFixed(0) }} mm</span>
    </div>

    <div class="fs-actions cc-row cc-row-tight">
      <span v-if="error" class="cc-muted-error">{{ error }}</span>
      <button class="cc-btn cc-btn-ghost cc-btn-micro" :disabled="busy" @click="save({})"
              v-tooltip.bottom="'Back to the house style (the Cecelia paper\'s)'">House style</button>
      <button class="cc-btn cc-btn-primary cc-btn-micro" :disabled="busy || !dirty" @click="save(styleOverrides(draft))"
              v-tooltip.bottom="'Use this style for this project\'s figures'">Save</button>
    </div>
  </div>
</template>

<style scoped>
/* The app's visual-aid layout (VisualAid.vue): a fixed label column, right-aligned muted labels, one
   thin rule per row, 2xs text — so this reads like every other figure-of-settings. */
.fs { display: grid; grid-template-columns: auto minmax(20rem, 1fr); gap: 0.6rem 1.25rem; align-items: start; }
.fs-grid { display: grid; grid-template-columns: 6rem auto; column-gap: 0.5rem; align-items: stretch; }
.fs-group { grid-column: 1 / -1; padding: 0.4rem 0 0.15rem; }
.fs-group:first-child { padding-top: 0; }
.fs-label, .fs-cell { border-bottom: 1px solid var(--cc-border); padding: 0.18rem 0; display: flex; align-items: center; }
.fs-label { justify-content: flex-end; text-align: right; }
.fs-num { width: 4.25rem; }
.fs-preview { display: flex; flex-direction: column; gap: 0.35rem; min-width: 0; }
.fs-svg { width: 100%; height: auto; max-height: 24rem; border: 1px solid var(--cc-border); background: #fff; }
.fs-actions { grid-column: 1 / -1; justify-content: flex-end; }
</style>
