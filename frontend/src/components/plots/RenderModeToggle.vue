<!--
  Shared render-mode segmented control for gate scatters. ONE definition so the mode set stays in sync
  across the Gate panel (GatePlotPanel), the channel-pairs matrix (GatePairsPanel) and the read-only
  gating-strategy montage (GatingStrategyView). Ports the old R fcs.gating.plotTypes (pseudocolour /
  contour / contour ± outliers), plus `binned` — the colour-by field (docs/PLOTS.md). Thin wrapper over
  the canonical <ChipSelect> (segmented, single-select).

  `binned` is OFFERED ONLY when the host has a colour-by measure (`colourBy`), because there is nothing
  to average without one: it would be the binned DENSITY raster, which this renderer deliberately does
  not draw (it showed visible rectangles — docs/PLOTS.md). A persisted state that names it with no
  measure falls back to the dot plot in PlotLayers rather than going blank.
-->
<script setup lang="ts">
import { computed } from 'vue'
import ChipSelect, { type ChipOption } from '../ChipSelect.vue'
export type RenderMode = 'points' | 'contour' | 'outliers' | 'binned'
const props = defineProps<{ modelValue: RenderMode; colourBy?: boolean }>()
const emit = defineEmits<{ 'update:modelValue': [RenderMode] }>()
const MODES: ChipOption[] = [
  { value: 'points',   label: '', icon: 'pi pi-circle-fill', tip: 'Pseudocolour points (density-coloured)' },
  { value: 'contour',  label: '', icon: 'pi pi-chart-line',  tip: 'Density contours only — fast (no point cloud)' },
  { value: 'outliers', label: '', icon: 'pi pi-asterisk',    tip: 'Contours + outliers (individual sparse-tail points)' },
]
const BINNED: ChipOption = { value: 'binned', label: '', icon: 'pi pi-th-large',
  tip: 'Binned: mean of the colour measure per cell (reads as a field, not speckle)' }
const options = computed(() => props.colourBy ? [...MODES, BINNED] : MODES)
</script>

<template>
  <!-- the group carries its own hover help: the per-chip `tip`s only satisfy the tooltip ratchet when
       the `:options` binding can be followed back to a literal, and this one is a computed (the binned
       chip appears only with a colour measure) -->
  <ChipSelect variant="segmented" :options="options" :model-value="modelValue"
              v-tooltip.bottom="'How the cloud is drawn'"
              aria-label="Render: points / contour / contour + outliers / binned"
              @update:model-value="v => emit('update:modelValue', v as RenderMode)" />
</template>
