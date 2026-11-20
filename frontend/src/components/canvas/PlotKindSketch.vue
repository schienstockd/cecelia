<!--
  The chart SHAPE of a kind of plot, drawn once in the "+ Plot…" picker's column of kinds: every
  "Compare groups" plot opens as a box plot, every "Make-up" plot as stacked bars, so the shape says
  what kind of plot you get, and the cards beside it (PlotSketch.vue) say which one. One inline SVG per
  category; theme tokens only, so it follows light/dark like the rest of the picker.
-->
<script setup lang="ts">
import type { PlotCategory } from '../../plots/plotPicker'

defineProps<{ category: PlotCategory }>()

// box points around the hollow group's box; odd ones move to the filled group (right and down)
const BOX_POINTS = [[16, 18], [21, 17], [18, 22], [22, 23], [15, 21], [20, 14], [17, 26], [23, 19], [19, 12],
  [16, 24], [21, 28], [18, 16], [22, 21], [15, 17], [20, 23], [17, 10], [23, 25], [19, 20], [16, 29], [21, 15]]
</script>

<template>
  <svg class="plot-kind-sketch" viewBox="0 0 60 36" aria-hidden="true">
    <path v-if="!['profiles', 'look'].includes(category)" class="ax" d="M5 3 V32 H57" />

    <!-- the comparison plot as it usually opens: two groups (hollow, filled), the points over each box,
         a significance bracket -->
    <g v-if="category === 'compare'">
      <path class="dim" d="M19 6.5 V5 H43 V6.5" />
      <line class="dim" x1="19" y1="10" x2="19" y2="31" /><rect class="box-o" x="13" y="15" width="12" height="10" />
      <line class="mark" x1="13" y1="20" x2="25" y2="20" />
      <line class="dim" x1="43" y1="16" x2="43" y2="31" /><rect class="box-f" x="37" y="21" width="12" height="7" />
      <line class="mark" x1="37" y1="25" x2="49" y2="25" />
      <circle v-for="(p, i) in BOX_POINTS" :key="i" :class="i % 2 ? 'dot-a' : 'dot'" :cx="p[0]! + (i % 2 ? 24 : 0)"
              :cy="p[1]! + (i % 2 ? 5 : 0)" r="0.75" />
    </g>

    <g v-else-if="category === 'time'">
      <polyline class="line-a" points="7,26 16,22 25,23 34,15 43,12 55,7" />
      <polyline class="line-b" points="7,29 16,28 25,25 34,26 43,21 55,19" />
    </g>

    <g v-else-if="category === 'makeup'">
      <template v-for="(x, i) in [10, 23, 36, 49]" :key="x">
        <rect class="fill-a" :x="x" y="4" width="9" :height="[10, 16, 7, 12][i]" />
        <rect class="fill-b" :x="x" :y="4 + [10, 16, 7, 12][i]!" width="9" :height="[12, 6, 14, 9][i]" />
        <rect class="fill-c" :x="x" :y="4 + [22, 22, 21, 21][i]!" width="9" :height="28 - [22, 22, 21, 21][i]!" />
      </template>
    </g>

    <g v-else-if="category === 'profiles'">
      <template v-for="r in 4" :key="r">
        <rect v-for="c in 6" :key="c" class="cell" :x="6 + (c - 1) * 8.4" :y="2 + (r - 1) * 8"
              width="8" height="7.6" :style="{ opacity: 0.15 + (((r * 7 + c * 5) % 9) / 9) * 0.85 }" />
      </template>
    </g>

    <g v-else-if="category === 'look'">
      <rect class="frame" x="3" y="4" width="17" height="28" /><rect class="frame" x="22" y="4" width="17" height="28" />
      <rect class="frame" x="41" y="4" width="17" height="28" />
      <circle class="fill-a" cx="10" cy="14" r="3" /><circle class="fill-b" cx="15" cy="24" r="2.4" />
      <circle class="fill-a" cx="31" cy="16" r="3" /><circle class="fill-b" cx="28" cy="25" r="2.4" />
      <circle class="fill-a" cx="50" cy="18" r="3" /><circle class="fill-b" cx="46" cy="10" r="2.4" />
    </g>

    <!-- a curve settling onto its target line: did it work? -->
    <g v-else-if="category === 'checks'">
      <line class="dim" x1="5" y1="27" x2="57" y2="27" stroke-dasharray="2 2" />
      <polyline class="line-a" points="7,7 18,19 30,24 44,26 55,26" />
    </g>

    <g v-else>
      <circle v-for="(p, i) in [[14, 26], [24, 22], [34, 18], [44, 13]]" :key="i" class="dot-a" :cx="p[0]" :cy="p[1]" r="1.8" />
    </g>
  </svg>
</template>

<style scoped>
.plot-kind-sketch { width: 120px; height: 72px; flex-shrink: 0; }
.ax { fill: none; stroke: var(--cc-text-dim); stroke-width: 0.8; opacity: 0.6; }
.dim { fill: none; stroke: var(--cc-text-dim); stroke-width: 0.8; }
.mark { stroke: var(--cc-text); stroke-width: 1; }
.box-o { fill: none; stroke: var(--cc-text-dim); stroke-width: 0.8; }
.box-f { fill: var(--cc-accent); fill-opacity: 0.3; stroke: var(--cc-accent); stroke-width: 0.8; }
.fill-a { fill: var(--cc-accent); opacity: 0.85; }
.fill-b { fill: var(--cc-text-dim); opacity: 0.55; }
.fill-c { fill: var(--cc-text-dim); opacity: 0.25; }
.cell { fill: var(--cc-accent); }
.dot { fill: var(--cc-text-dim); }
.dot-a { fill: var(--cc-accent); }
.line-a { fill: none; stroke: var(--cc-accent); stroke-width: 1.4; stroke-linejoin: round; }
.line-b { fill: none; stroke: var(--cc-text-dim); stroke-width: 1.4; stroke-linejoin: round; }
.frame { fill: var(--cc-surface-2); stroke: var(--cc-border); stroke-width: 0.6; }
</style>
