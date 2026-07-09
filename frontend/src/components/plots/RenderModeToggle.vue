<!--
  Shared render-mode segmented control for gate scatters. ONE definition so the mode set stays in sync
  across the Gate panel (GatePlotPanel), the channel-pairs matrix (GatePairsPanel) and the read-only
  gating-strategy montage (GatingStrategyView) — instead of three hand-rolled copies. Ports the old R
  fcs.gating.plotTypes (pseudocolour / contour / contour ± outliers).
-->
<script setup lang="ts">
export type RenderMode = 'points' | 'contour' | 'outliers'
defineProps<{ modelValue: RenderMode }>()
const emit = defineEmits<{ 'update:modelValue': [RenderMode] }>()
const MODES: { v: RenderMode; icon: string; tip: string }[] = [
  { v: 'points',   icon: 'pi pi-circle-fill', tip: 'Pseudocolour points (density-coloured)' },
  { v: 'contour',  icon: 'pi pi-chart-line',  tip: 'Density contours only — fast (no point cloud)' },
  { v: 'outliers', icon: 'pi pi-asterisk',    tip: 'Contours + outliers (individual sparse-tail points)' },
]
</script>

<template>
  <div class="seg" v-tooltip.bottom="'Render: points / contour / contour + outliers'">
    <button v-for="m in MODES" :key="m.v" type="button" :class="{ on: modelValue === m.v }"
            v-tooltip.bottom="m.tip" @click="emit('update:modelValue', m.v)"><i :class="m.icon" /></button>
  </div>
</template>

<style scoped>
.seg { display: inline-flex; border: 1px solid var(--cc-border); border-radius: 5px; overflow: hidden; }
.seg button { background: var(--cc-surface-2); color: var(--cc-text-dim); border: none; padding: 4px 8px; cursor: pointer; font-size: 12px; }
.seg button + button { border-left: 1px solid var(--cc-border); }
.seg button:hover { color: var(--cc-text); }
.seg button.on { background: var(--cc-accent); color: #fff; }
</style>
