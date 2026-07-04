<!--
  Segmentation integrity (QC) panel — module page + (later) whiteboard.
  Renders segmentation_qc_data (/api/plots/segmentation-qc) via the shared PlotChart. Lets the user
  pick a QC metric (cell count, or a morphology distribution) and toggle per-image vs per-timepoint
  (the temporal-consistency view). See docs/todo/SEGMENTATION_QC_PLOT_PLAN.md.
-->
<script setup lang="ts">
import { ref, computed, watch } from 'vue'
import PlotChart from './PlotChart.vue'
import { defaultVis, type BuildOpts } from '../../plots/plot'
import type { PlotDataResponse, PlotSeries } from '../../plots/types'

// Okabe–Ito-ish palette; colour series by image (uID) / group, cycling deterministically.
const QC_PALETTE = ['#4c78a8', '#f58518', '#54a24b', '#e45756', '#72b7b2', '#eeca3b', '#b279a2', '#ff9da6']
const _colorKeys = new Map<string, number>()
function qcColor(s: PlotSeries): string {
  const key = String(s.uID ?? s.pop ?? s.group ?? '')
  if (!_colorKeys.has(key)) _colorKeys.set(key, _colorKeys.size)
  return QC_PALETTE[_colorKeys.get(key)! % QC_PALETTE.length]
}

const props = defineProps<{
  projectUid: string
  valueName: string            // the segmentation to QC
  setUid?: string
  imageUids: string[]          // selected images (rows / series)
}>()

// Metric options: "count" is special (no measure, rendered as bars); the rest are per-cell
// morphology measures shown as a distribution. Intensity channels could be added later.
const METRICS = [
  { key: 'count',        label: 'Cell count' },
  { key: 'area',         label: 'Area' },
  { key: 'solidity',     label: 'Solidity' },
  { key: 'aspect_ratio', label: 'Aspect ratio' },
  { key: 'eccentricity', label: 'Eccentricity' },
]
const metric = ref('count')
const perTimepoint = ref(false)

const result = ref<PlotDataResponse | null>(null)
const err = ref('')
const loading = ref(false)

// count → bar rendering (series carry `value`); a measure → boxplot distribution.
const renderChart = computed(() => metric.value === 'count' ? 'bar' : 'boxplot')

const buildOpts = computed<BuildOpts>(() => ({
  chartType: renderChart.value,
  byImage: true,
  normalize: false,
  errorMetric: 'sd',
  nonNegative: true,
  colorOf: qcColor,
  ...defaultVis(),
}))

async function load() {
  err.value = ''
  result.value = null
  if (!props.projectUid || !props.valueName || !props.imageUids.length) return
  loading.value = true
  try {
    const body: Record<string, unknown> = {
      projectUid: props.projectUid,
      valueName: props.valueName,
      perTimepoint: perTimepoint.value,
      chartType: metric.value === 'count' ? 'count' : 'boxplot',
      measure: metric.value === 'count' ? undefined : metric.value,
    }
    if (props.setUid) { body.setUid = props.setUid; body.imageUids = props.imageUids }
    else body.imageUid = props.imageUids[0]
    const res = await fetch('/api/plots/segmentation-qc', {
      method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body),
    })
    if (!res.ok) { err.value = (await res.json()).error ?? res.statusText; return }
    result.value = await res.json() as PlotDataResponse
  } catch (e) {
    err.value = e instanceof Error ? e.message : String(e)
  } finally {
    loading.value = false
  }
}

watch([() => props.projectUid, () => props.valueName, () => props.imageUids, () => props.setUid,
       metric, perTimepoint], load, { immediate: true, deep: true })
</script>

<template>
  <div class="qc-panel">
    <div class="qc-toolbar">
      <span class="qc-title">Segmentation QC</span>
      <select class="qc-select" v-model="metric" v-tooltip.bottom="'QC metric'">
        <option v-for="m in METRICS" :key="m.key" :value="m.key">{{ m.label }}</option>
      </select>
      <label class="qc-check" v-tooltip.bottom="'Split by timepoint (temporal consistency)'">
        <input type="checkbox" v-model="perTimepoint" /> per timepoint
      </label>
      <span v-if="loading" class="qc-hint">loading…</span>
    </div>
    <div class="qc-body">
      <div v-if="err" class="qc-error">{{ err }}</div>
      <div v-else-if="!imageUids.length" class="qc-hint">Select images to see segmentation QC.</div>
      <PlotChart v-else :data="result" :opts="buildOpts" />
    </div>
  </div>
</template>

<style scoped>
.qc-panel { display: flex; flex-direction: column; height: 100%; min-height: 0; }
.qc-toolbar {
  display: flex; align-items: center; gap: 0.6rem;
  padding: 0.4rem 0.6rem; border-bottom: 1px solid var(--cc-border); flex-shrink: 0;
}
.qc-title { font-size: 0.72rem; font-weight: 700; text-transform: uppercase; letter-spacing: 0.06em; color: var(--cc-text-dim); }
.qc-select {
  font-size: 0.78rem; background: var(--cc-surface-2); border: 1px solid var(--cc-border);
  border-radius: 0.3rem; color: var(--cc-text); padding: 0.2rem 0.4rem;
}
.qc-check { display: flex; align-items: center; gap: 0.3rem; font-size: 0.75rem; color: var(--cc-text-dim); cursor: pointer; }
.qc-hint { font-size: 0.72rem; color: var(--cc-text-dim); font-style: italic; }
.qc-error { font-size: 0.75rem; color: #f87171; padding: 0.5rem; }
.qc-body { flex: 1; min-height: 220px; position: relative; }
</style>
