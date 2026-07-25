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
    <div class="qc-count">{{ fmt(data.total) }} <span class="qc-unit cc-muted cc-fs-3xs">cells</span></div>
    <div class="qc-spark">
      <span v-for="(h, i) in bars" :key="i" class="qc-bar" :style="{ height: h + 'px' }" />
      <span v-if="!bars.length" class="qc-empty cc-empty-inline cc-fs-3xs">no data</span>
    </div>
    <div class="qc-foot cc-eyebrow cc-fs-3xs">{{ data.imageCount ?? 0 }} img · expand</div>
  </div>
</template>

<style scoped>
.qc-node {
  background: var(--cc-surface-1);
  border: 1px dashed var(--cc-accent);
  border-radius: var(--cc-radius-md);
  padding: 5px 9px;
  min-width: 120px;
  cursor: pointer;
  font-size: var(--cc-fs-xs);
}
.qc-head { display: flex; align-items: center; gap: 5px; color: var(--cc-accent); }
.qc-head .pi { font-size: var(--cc-fs-xs); }
.qc-name { font-size: var(--cc-fs-3xs); font-weight: 700; font-family: var(--cc-mono, monospace); letter-spacing: 0.04em; }
.qc-spin { margin-left: auto; font-size: var(--cc-fs-2xs); opacity: 0.7; }
.qc-count { font-size: var(--cc-fs-lg); font-weight: 700; color: var(--cc-text); margin: 2px 0; }
/* + cc-muted cc-fs-3xs — the weight reset is this site's (it sits inside a 700 count) */
.qc-unit { font-weight: 400; }
.qc-spark { display: flex; align-items: flex-end; gap: 2px; height: 24px; }
/* 1px is deliberate and off-scale: --cc-radius-xs (3px) on a 4px-wide bar rounds it to a blob */
.qc-bar { width: 4px; background: var(--cc-accent); opacity: 0.7; border-radius: 1px; }
.qc-empty { font-style: italic; }   /* + .cc-empty-inline .cc-fs-3xs (row/colour/9px tier) */
.qc-foot { margin-top: 2px; }   /* + cc-eyebrow cc-fs-3xs */
</style>
