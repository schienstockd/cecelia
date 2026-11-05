<!--
  Where a value came from, as a shape AND a colour (Okabe–Ito, plots/palettes.json) so it reads without
  colour vision: image metadata = circle, microscope file = diamond, measured = triangle, not recorded
  = hollow circle. Shared by the light-path and data-path panels and their legend.
-->
<script setup lang="ts">
import palettes from '../plots/palettes.json'
import type { LightPathSource } from '../utils/acquisitionLightPath'

defineProps<{ source: LightPathSource }>()

const oi = palettes.palettes['okabe-ito']
const C = { ome: oi[4], file: oi[0], measured: oi[6] }
</script>

<template>
  <svg class="lps" viewBox="0 0 10 10" aria-hidden="true">
    <circle v-if="source === 'ome'" cx="5" cy="5" r="3.5" :fill="C.ome" />
    <path v-else-if="source === 'file'" d="M5 1 L9 5 L5 9 L1 5 Z" :fill="C.file" />
    <path v-else-if="source === 'measured'" d="M5 1.2 L9 8.6 H1 Z" :fill="C.measured" />
    <circle v-else cx="5" cy="5" r="3.2" class="lps-missing" />
  </svg>
</template>

<style scoped>
.lps { flex: none; width: 10px; height: 10px; }
.lps-missing { fill: none; stroke: var(--cc-text-dim); stroke-width: 1.2; }
</style>
