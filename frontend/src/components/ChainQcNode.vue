<!--
  Live-view QC thumbnail — a compact, always-on summary for a processing node whose task declares a
  `qcPlot` (e.g. segmentation). Sits in the QC band above the image grid, aligned to that node's
  column. Clean + simple: headline cell count + a tiny per-image sparkline. Click → expand to the
  full QC canvas (handled by ChainModule). See docs/SCHEDULER.md → Live QC row, docs/PLOTS.md → Segmentation QC plot.
-->
<script setup lang="ts">
import { computed } from 'vue'

const props = defineProps<{
  id: string
  data: {
    label: string           // task label (e.g. "Cellpose Segment + Measure")
    valueName: string       // the segmentation this QC targets (e.g. "T" / "default")
    total?: number          // aggregate cell count across the run's images
    values?: number[]       // per-image cell counts (sparkline)
    imageCount?: number
    loading?: boolean
  }
}>()

// tiny sparkline bars, normalised to the max (0-height guard)
const bars = computed(() => {
  const v = props.data.values ?? []
  const max = Math.max(1, ...v)
  return v.map(x => Math.max(2, Math.round((x / max) * 22)))
})
const fmt = (n?: number) => n == null ? '—' : n.toLocaleString()
</script>

<template>
  <div class="qc-node" v-tooltip.top="'Segmentation QC — click to expand'">
    <div class="qc-head">
      <i class="pi pi-chart-bar" />
      <span class="qc-name">{{ data.valueName }}</span>
      <span v-if="data.loading" class="qc-spin"><i class="pi pi-spin pi-spinner" /></span>
    </div>
    <div class="qc-count">{{ fmt(data.total) }} <span class="qc-unit">cells</span></div>
    <div class="qc-spark">
      <span v-for="(h, i) in bars" :key="i" class="qc-bar" :style="{ height: h + 'px' }" />
      <span v-if="!bars.length" class="qc-empty">no data</span>
    </div>
    <div class="qc-foot">{{ data.imageCount ?? 0 }} img · expand</div>
  </div>
</template>

<style scoped>
.qc-node {
  background: var(--cc-surface-1, #1e1b2e);
  border: 1px dashed var(--cc-accent, #a78bfa);
  border-radius: 6px;
  padding: 5px 9px;
  min-width: 120px;
  cursor: pointer;
  font-size: 11px;
}
.qc-head { display: flex; align-items: center; gap: 5px; color: var(--cc-accent, #a78bfa); }
.qc-head .pi { font-size: 0.7rem; }
.qc-name { font-size: 9px; font-weight: 700; font-family: var(--cc-mono, monospace); letter-spacing: 0.04em; }
.qc-spin { margin-left: auto; font-size: 0.6rem; opacity: 0.7; }
.qc-count { font-size: 15px; font-weight: 700; color: var(--cc-text, #e2e2f0); margin: 2px 0; }
.qc-unit { font-size: 9px; font-weight: 400; color: var(--cc-text-dim, #8b8ca7); }
.qc-spark { display: flex; align-items: flex-end; gap: 2px; height: 24px; }
.qc-bar { width: 4px; background: var(--cc-accent, #a78bfa); opacity: 0.7; border-radius: 1px; }
.qc-empty { font-size: 9px; color: var(--cc-text-dim, #8b8ca7); font-style: italic; }
.qc-foot { font-size: 8px; color: var(--cc-text-dim, #8b8ca7); text-transform: uppercase; letter-spacing: 0.05em; margin-top: 2px; }
</style>
