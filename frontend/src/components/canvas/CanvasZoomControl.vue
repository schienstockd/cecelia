<!--
  Reusable zoom control (fit-width / fit-height / slider / % reset) for any plot canvas — the Analysis
  board and the free-floating module canvases share it (docs/UI.md → "Canvas zoom"). Pure UI: it owns no
  state, just relays to the host's useCanvasZoom. Styled to sit in a canvas toolbar's `.seg` row.
-->
<script setup lang="ts">
import { ZOOM_MIN, ZOOM_MAX } from '../../composables/useCanvasZoom'

const props = defineProps<{ zoom: number }>()
const emit = defineEmits<{ 'update:zoom': [number]; fitWidth: []; fitHeight: []; reset: [] }>()
const pct = () => Math.round(props.zoom * 100)
</script>

<template>
  <!-- the group tip moved onto the SLIDER: it was the only hover help the range had, but on the
       container it also fired over every button's own tip (docs/UI.md → nested tooltips) -->
  <div class="cz">
    <button class="cz-btn cc-btn cc-btn-bare cc-btn-icon" @click="emit('fitWidth')" v-tooltip.bottom="'Fit width'"><i class="pi pi-arrows-h" /></button>
    <button class="cz-btn cc-btn cc-btn-bare cc-btn-icon" @click="emit('fitHeight')" v-tooltip.bottom="'Fit height'"><i class="pi pi-arrows-v" /></button>
    <input class="cz-range" type="range" :min="ZOOM_MIN * 100" :max="ZOOM_MAX * 100" step="5"
           v-tooltip.bottom="'Zoom the view (does not change the exported page)'"
           :value="pct()" @input="emit('update:zoom', +($event.target as HTMLInputElement).value / 100)" />
    <button class="cz-val" @click="emit('reset')" v-tooltip.bottom="'Reset to 100%'">{{ pct() }}%</button>
  </div>
</template>

<style scoped>
.cz { display: inline-flex; align-items: center; gap: 4px; border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-sm); padding: 1px 4px; background: var(--cc-surface-2); }
/* .cz-btn → cc-btn cc-btn-bare cc-btn-icon */
.cz-btn:hover { color: var(--cc-text); }
.cz-range { width: 6rem; }
.cz-val { min-width: 2.6rem; text-align: center; background: transparent; border: none; cursor: pointer;
  color: var(--cc-text-dim); font-size: var(--cc-fs-xs); font-variant-numeric: tabular-nums; }
.cz-val:hover { color: var(--cc-text); }
</style>
