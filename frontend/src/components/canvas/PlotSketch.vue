<!--
  The sketch on a "+ Plot…" card: what the plot is ABOUT (a cell, a track, two images, states), drawn
  from plots/plotSketchArt.ts. That file owns every shape; this only renders the list, in the line-art
  vocabulary of DatasetAskFigure.vue (theme strokes, one accent for the thing measured). The chart shape
  lives on the kind's tile instead (PlotKindSketch.vue).
-->
<script setup lang="ts">
import { computed } from 'vue'
import { plotSketchArt, SKETCH_W, SKETCH_H, type PlotSketchKind } from '../../plots/plotSketchArt'

const props = defineProps<{ kind: PlotSketchKind }>()
const shapes = computed(() => plotSketchArt(props.kind))
</script>

<template>
  <svg class="plot-sketch" :viewBox="`0 0 ${SKETCH_W} ${SKETCH_H}`" fill="none" aria-hidden="true">
    <template v-for="(s, i) in shapes" :key="i">
      <circle v-if="s.el === 'circle'" :class="s.cls" :cx="s.x" :cy="s.y" :r="s.r" />
      <ellipse v-else-if="s.el === 'ellipse'" :class="s.cls" :cx="s.x" :cy="s.y" :rx="s.rx" :ry="s.ry" />
      <rect v-else-if="s.el === 'rect'" :class="s.cls" :x="s.x" :y="s.y" :width="s.w" :height="s.h" rx="2"
            :style="s.op !== undefined ? { opacity: s.op } : undefined" />
      <path v-else :class="s.cls" :d="s.d" />
    </template>
  </svg>
</template>

<style scoped>
.plot-sketch { width: 80px; height: 50px; flex-shrink: 0; stroke-linecap: round; stroke-linejoin: round; stroke-width: 1.5; }
.ink { stroke: var(--cc-text); }
.dim { stroke: var(--cc-text-dim); }
.acc { stroke: var(--cc-accent); }
.dash { stroke-dasharray: 2.5 2.5; }
.thick { stroke-width: 2.6; }
.lane { stroke-width: 4; }
.frame { stroke: var(--cc-border); stroke-width: 1.5; fill: var(--cc-surface-2); }
.fill { fill: currentColor; }
.ink.fill { color: var(--cc-text); }
.dim.fill { color: var(--cc-text-dim); }
.acc.fill { color: var(--cc-accent); }
.fill-dim { fill: var(--cc-text-dim); fill-opacity: 0.35; }
.fill-acc { fill: var(--cc-accent); fill-opacity: 0.15; }
.heat { fill: var(--cc-accent); stroke: none; }
</style>
