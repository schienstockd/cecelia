<script setup lang="ts">
// Presentational legend for a napari view — renders LegendSection[] (see utils/viewLegend.ts) as
// grouped colour swatches. Style-light: text inherits `color`, size scales with the parent font-size,
// so each host (image-strip overlay, animation page, viewer panel) styles it via its own container.
// Section headings show only when there's more than one section (a lone group needs no title).
import type { LegendSection } from '../utils/viewLegend'

withDefaults(defineProps<{ sections: LegendSection[]; swatch?: number }>(), { swatch: 10 })
</script>

<template>
  <div class="view-legend">
    <div v-for="sec in sections" :key="sec.title" class="vl-section">
      <div v-if="sections.length > 1" class="vl-title">{{ sec.title }}</div>
      <div class="vl-items">
        <span v-for="it in sec.items" :key="it.label" class="vl-item">
          <span class="vl-swatch" :style="{ background: it.colour, width: swatch + 'px', height: swatch + 'px' }" />
          {{ it.label }}
        </span>
      </div>
    </div>
  </div>
</template>

<style scoped>
.view-legend { display: flex; flex-direction: column; gap: 3px; }
.vl-section { display: flex; flex-direction: column; gap: 1px; }
.vl-title { font-size: 0.82em; font-weight: 700; opacity: 0.65; text-transform: uppercase; letter-spacing: 0.04em; }
.vl-items { display: flex; flex-wrap: wrap; gap: 1px 8px; }
.vl-item { display: inline-flex; align-items: center; gap: 4px; font-size: 0.9em; white-space: nowrap; }
.vl-swatch { border-radius: 2px; flex-shrink: 0; border: 1px solid rgba(128, 128, 128, 0.4); }
</style>
