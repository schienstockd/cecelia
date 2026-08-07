<!--
  Did this model converge, and is each loss term earning its place — the per-epoch loss, one line per
  term.

  Per TERM, not just the total, because the total is the one curve that cannot answer the question
  you ask a loss curve. Coastal optimises a weighted sum (intensity · temporal · variance ·
  foreground · …) and three of those weights are task params, so "is this term adding anything" is
  the reason to look at all.

  **Contribution, not the raw term, by default.** Coastal's history records each term BEFORE its
  weight; the total is the weighted sum. So a raw curve at 0.9 with weight 0 contributes exactly
  nothing while one at 0.05 with weight 2.0 contributes twice as much — reading the raw curves side
  by side would rank them backwards. `lossWeights` travels in the manifest for precisely this, and
  the Raw toggle is there for tuning a single term's own scale.

  A PLOT, not a chart inside the vault's details modal, and that is the point of it being a registry
  view: it lands on the canvas with the panel chrome, the zoom, CSV/PNG/SVG export and the board's
  PDF export already attached. A chart in a modal would need every one of those written again, worse.

  Data is the model's own manifest, read through `GET /api/optical-flow/models` — the route the
  picker and the vault already use, so there is no second listing that can disagree with them. Models
  trained before the curves were recorded say so rather than drawing an empty box; the run kept only
  `finalLoss`, and that is not recoverable.

  Observable Plot directly, like the cluster HMM panels. The summary `PlotChart` builds from a
  `PlotDataResponse` (server-aggregated CELL data), and its `trend` chart is a LOESS fit — the wrong
  thing for a loss curve, where you want the epochs as they happened, not a smooth through them.
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted, onBeforeUnmount, nextTick, useTemplateRef } from 'vue'
import ChipSelect, { type ChipOption } from '../ChipSelect.vue'
import { rowsToCsv, downloadBlob, downloadDataUrl, elementToImageURL, svgOf } from '../../plots/export'
import { distinctColors } from '../../plots/plot'
import { useDataRefresh } from '../../composables/useDataRefresh'
import { useProjectStore } from '../../stores/project'
import { lossSeries, lossTable, type LossCurves } from '../../plots/lossCurves'

interface TrainState { model?: string; logY?: boolean; raw?: boolean; terms?: string[] }
interface Manifest { lossCurves?: LossCurves; lossWeights?: Record<string, number>; epochs?: number }
interface FlowModel { name: string; label: string; stem: string; manifest: Manifest }

const props = defineProps<{ state: TrainState }>()
const project = useProjectStore()

const models = ref<FlowModel[]>([])
const loading = ref(false)
const error = ref('')
const host = useTemplateRef<HTMLElement>('host')
// @observablehq/plot is loosely typed for our purposes; keep it as any (its types are large).
let Plot: any = null                                   // eslint-disable-line @typescript-eslint/no-explicit-any
let node: SVGElement | HTMLElement | null = null
let ro: ResizeObserver | null = null
const forceLight = ref(false)

const state = computed(() => props.state)
const logY = computed({ get: () => state.value.logY ?? false, set: v => (state.value.logY = v) })
const raw = computed({ get: () => state.value.raw ?? false, set: v => (state.value.raw = v) })
const current = computed(() => models.value.find(m => m.name === state.value.model) ?? null)

const series = computed(() => lossSeries(current.value?.manifest?.lossCurves,
                                         current.value?.manifest?.lossWeights, raw.value))
const termOptions = computed<ChipOption[]>(() => series.value.map(s => ({
  value: s.term,
  label: s.term,
  // A term at weight 0 is switched off — say so on the chip rather than letting a flat line at zero
  // read as "trained to nothing".
  tip: s.weight === 0 ? 'weight 0 — this term is off' : `weight ${s.weight}`,
})))
// Default: everything that is actually on. `undefined` means "not chosen yet", so an explicit empty
// pick is respected (docs/UI.md → Persisting view state).
const terms = computed<string[]>(() =>
  state.value.terms ?? series.value.filter(s => s.weight !== 0).map(s => s.term))
const shown = computed(() => series.value.filter(s => terms.value.includes(s.term)))
const rows = computed(() => shown.value.flatMap(s =>
  s.values.map((loss, i) => ({ epoch: i + 1, term: s.term, loss }))))

async function load() {
  loading.value = true
  error.value = ''
  try {
    const r = await fetch('/api/optical-flow/models')
    if (!r.ok) throw new Error(`HTTP ${r.status}`)
    models.value = (await r.json()).models ?? []
    // prefer a model that actually HAS curves, so the panel opens on something to look at
    if (!state.value.model || !models.value.some(m => m.name === state.value.model))
      state.value.model = (models.value.find(m => Object.keys(m.manifest?.lossCurves ?? {}).length)
                           ?? models.value[0])?.name
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
  } finally {
    loading.value = false
    await nextTick()
    await render()
  }
}

// A finished training run adds a model. Training is set-scope and this panel is not bound to one
// image, so it watches every image in the project — same shared primitive, same opt-out.
const allUids = computed(() => project.sets.flatMap(s => s.images.map(i => i.uid)))
useDataRefresh(() => allUids.value, load)

async function render() {
  if (!host.value) return
  if (!Plot) Plot = await import('@observablehq/plot')
  node?.remove(); node = null
  if (!rows.value.length) return
  const w = Math.max(200, host.value.clientWidth || 360)
  const h = Math.max(160, host.value.clientHeight || 240)
  const dark = !forceLight.value
  const fg = dark ? '#e6e6e6' : '#111'
  const bg = dark ? '#1f2226' : 'white'
  const domain = shown.value.map(s => s.term)
  node = Plot.plot({
    width: w, height: h, marginLeft: 58, marginRight: 12, marginTop: 12,
    style: { background: bg, color: fg, fontSize: '11px' },
    x: { label: 'epoch', grid: true },
    // Log only when every plotted value is positive. Zero is a legitimate loss and log(0) would drop
    // the point silently rather than fail.
    y: { label: raw.value ? 'loss (raw)' : 'loss (weighted)', grid: true,
         type: logY.value && rows.value.every(r => r.loss > 0) ? 'log' : 'linear' },
    color: { domain, range: distinctColors(domain.length), legend: false },
    marks: [
      Plot.line(rows.value, { x: 'epoch', y: 'loss', stroke: 'term', strokeWidth: 1.5, tip: true }),
    ],
  }) as SVGElement
  host.value.append(node)
}

onMounted(() => {
  load()
  if (host.value && typeof ResizeObserver !== 'undefined') {
    ro = new ResizeObserver(() => render()); ro.observe(host.value)
  }
})
onBeforeUnmount(() => { ro?.disconnect(); ro = null; node?.remove(); node = null })
watch([() => state.value.model, logY, raw, () => terms.value.join(',')], render)

// ── export (the generic panel contract — plots/export.ts, same helpers as the cluster panels) ──
const exportFormats = ['png', 'svg', 'csv']
const stem = computed(() => `training_${current.value?.stem ?? 'model'}`.replace(/[^\w.-]+/g, '_'))
// One row per epoch, one COLUMN per term — a long/tidy dump would make the obvious spreadsheet
// question ("plot these against each other") a pivot first.
const csv = () => rows.value.length ? rowsToCsv(lossTable(shown.value)) : null
function exportAs(kind: string) {
  if (kind === 'csv') {
    const text = csv()
    if (text) downloadBlob(`${stem.value}.csv`, new Blob([text], { type: 'text/csv' }))
  } else if (kind === 'png' || kind === 'svg') {
    elementToImageURL(host.value, kind, '#1f2226')
      .then(url => url && downloadDataUrl(`${stem.value}.${kind}`, url))
  }
}
// board PDF/CSV/SVG: a plot-only LIGHT re-render, per the contract in docs/UI.md
async function exportImage(): Promise<string | null> {
  forceLight.value = true
  await nextTick(); await render()
  const url = await elementToImageURL(host.value, 'png', '#ffffff')
  forceLight.value = false; await render()
  return url
}
async function exportSvg(): Promise<string | null> {
  forceLight.value = true
  await nextTick(); await render()
  const svg = svgOf(host.value)?.outerHTML ?? null
  forceLight.value = false; await render()
  return svg
}
defineExpose({ exportFormats, exportAs, exportImage, exportSvg, getCsv: csv })
</script>

<template>
  <div class="ftv">
    <div class="ftv-ctrl cc-panel-controls">
      <div class="cc-row ftv-bar">
        <select class="select-input ftv-model" :value="state.model ?? ''"
                v-tooltip.top="'Model whose training run to show'"
                @change="state.model = ($event.target as HTMLSelectElement).value">
          <option v-for="m in models" :key="m.name" :value="m.name">{{ m.label }}</option>
        </select>
        <label class="cc-muted cc-fs-xs ftv-opt"
               v-tooltip.top="'Show each term before its weight is applied'">
          <input type="checkbox" v-model="raw" /> raw
        </label>
        <label class="cc-muted cc-fs-xs ftv-opt" v-tooltip.top="'Log scale on the loss axis'">
          <input type="checkbox" v-model="logY" /> log
        </label>
        <button class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.left="'Reload'"
                :disabled="loading" @click="load">
          <i class="pi pi-refresh" :class="{ 'pi-spin': loading }" />
        </button>
      </div>
      <label v-if="termOptions.length" class="cc-row ftv-terms">
        <span class="cc-muted cc-fs-xs"
              v-tooltip.top="'Which loss terms to draw — a term at weight 0 is off'">terms</span>
        <ChipSelect :options="termOptions" :model-value="terms" multiple aria-label="Loss terms"
                    @update:model-value="v => state.terms = v as string[]" />
      </label>
    </div>

    <p v-if="error" class="cc-muted-warn">{{ error }}</p>
    <p v-else-if="!models.length && !loading" class="cc-muted">
      No models yet — run Train flow model on a set.
    </p>
    <p v-else-if="current && !series.length" class="cc-muted">
      No loss curves — {{ current.stem }} was trained before they were recorded. Re-train to get them.
    </p>

    <div ref="host" class="ftv-host" />
  </div>
</template>

<style scoped>
/* position: relative so the overlaid .ftv-ctrl (.cc-panel-controls) anchors to the plot box */
.ftv { position: relative; display: flex; flex-direction: column; height: 100%; min-height: 0; }
.ftv-ctrl { display: flex; flex-direction: column; gap: 0.4rem; padding: 4px 6px; }
.ftv-bar { flex-wrap: wrap; }
.ftv-terms { flex-wrap: wrap; gap: 0.4rem; }
.ftv-model { max-width: 14rem; }
.ftv-opt { display: flex; align-items: center; gap: 0.25rem; }
.ftv-host { flex: 1; min-height: 0; }
</style>
