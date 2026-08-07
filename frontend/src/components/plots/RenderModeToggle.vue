<!--
  Shared render-mode segmented control for gate scatters. ONE definition so the mode set stays in sync
  across the Gate panel (GatePlotPanel), the channel-pairs matrix (GatePairsPanel) and the read-only
  gating-strategy montage (GatingStrategyView). Ports the old R fcs.gating.plotTypes (pseudocolour /
  contour / contour ± outliers). Thin wrapper over the canonical <ChipSelect> (segmented, single-select).
-->
<script setup lang="ts">
import ChipSelect, { type ChipOption } from '../ChipSelect.vue'
export type RenderMode = 'points' | 'contour' | 'outliers'
defineProps<{ modelValue: RenderMode }>()
const emit = defineEmits<{ 'update:modelValue': [RenderMode] }>()
const MODES: ChipOption[] = [
  { value: 'points',   label: '', icon: 'pi pi-circle-fill', tip: 'Pseudocolour points (density-coloured)' },
  { value: 'contour',  label: '', icon: 'pi pi-chart-line',  tip: 'Density contours only — fast (no point cloud)' },
  { value: 'outliers', label: '', icon: 'pi pi-asterisk',    tip: 'Contours + outliers (individual sparse-tail points)' },
]
</script>

<template>
  <ChipSelect variant="segmented" :options="MODES" :model-value="modelValue"
              aria-label="Render: points / contour / contour + outliers"
              @update:model-value="v => emit('update:modelValue', v as RenderMode)" />
</template>
