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
  <div class="cz" v-tooltip.bottom="'Zoom the view to fit your screen (does not change the exported page)'">
    <button class="cz-btn" @click="emit('fitWidth')" v-tooltip.bottom="'Fit width'"><i class="pi pi-arrows-h" /></button>
    <button class="cz-btn" @click="emit('fitHeight')" v-tooltip.bottom="'Fit height'"><i class="pi pi-arrows-v" /></button>
    <input class="cz-range" type="range" :min="ZOOM_MIN * 100" :max="ZOOM_MAX * 100" step="5"
           :value="pct()" @input="emit('update:zoom', +($event.target as HTMLInputElement).value / 100)" />
    <button class="cz-val" @click="emit('reset')" v-tooltip.bottom="'Reset to 100%'">{{ pct() }}%</button>
  </div>
</template>

<style scoped>
.cz { display: inline-flex; align-items: center; gap: 4px; border: 1px solid var(--cc-border);
  border-radius: 5px; padding: 1px 4px; background: var(--cc-surface-2); }
.cz-btn { display: inline-flex; align-items: center; justify-content: center; width: 1.4rem; height: 1.4rem;
  background: transparent; color: var(--cc-text-dim); border: none; cursor: pointer; font-size: 0.72rem; }
.cz-btn:hover { color: var(--cc-text); }
.cz-range { width: 6rem; }
.cz-val { min-width: 2.6rem; text-align: center; background: transparent; border: none; cursor: pointer;
  color: var(--cc-text-dim); font-size: 11px; font-variant-numeric: tabular-nums; }
.cz-val:hover { color: var(--cc-text); }
</style>
