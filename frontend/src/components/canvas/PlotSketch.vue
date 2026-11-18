<!--
  The schematic a "+ Plot…" picker card draws: the rough SHAPE of a plot, not its data — so it costs
  nothing, needs no task to have run, and shows the same thing before and after. One small inline SVG
  per `PlotSketchKind` (plots/plotPicker.ts); a plot picks its kind in its registry entry or spec.
  Theme tokens only, so it follows light/dark like the rest of the picker.
-->
<script setup lang="ts">
import type { PlotSketchKind } from '../../plots/plotPicker'

defineProps<{ kind: PlotSketchKind }>()
</script>

<template>
  <svg class="plot-sketch" viewBox="0 0 60 36" aria-hidden="true">
    <!-- axes, shared by the chart kinds; the picture kinds (image/cards/gates/timeline) draw their own frame -->
    <path v-if="!['image', 'cards', 'heatmap', 'timeline'].includes(kind)" class="ax" d="M5 3 V32 H57" />

    <g v-if="kind === 'box'">
      <line class="dim" x1="15" y1="8" x2="15" y2="29" /><rect class="fill-a" x="10" y="13" width="10" height="10" />
      <line class="mark" x1="10" y1="18" x2="20" y2="18" />
      <line class="dim" x1="32" y1="5" x2="32" y2="24" /><rect class="fill-b" x="27" y="8" width="10" height="9" />
      <line class="mark" x1="27" y1="12" x2="37" y2="12" />
      <line class="dim" x1="49" y1="14" x2="49" y2="30" /><rect class="fill-a" x="44" y="18" width="10" height="8" />
      <line class="mark" x1="44" y1="22" x2="54" y2="22" />
      <circle class="dot" cx="13" cy="20" r="1" /><circle class="dot" cx="17" cy="15" r="1" />
      <circle class="dot" cx="30" cy="10" r="1" /><circle class="dot" cx="34" cy="14" r="1" />
      <circle class="dot" cx="47" cy="24" r="1" /><circle class="dot" cx="51" cy="20" r="1" />
    </g>

    <g v-else-if="kind === 'hist'">
      <rect class="fill-a" x="8" y="25" width="6" height="7" /><rect class="fill-a" x="14" y="17" width="6" height="15" />
      <rect class="fill-a" x="20" y="9" width="6" height="23" /><rect class="fill-a" x="26" y="6" width="6" height="26" />
      <rect class="fill-a" x="32" y="12" width="6" height="20" /><rect class="fill-a" x="38" y="19" width="6" height="13" />
      <rect class="fill-a" x="44" y="26" width="6" height="6" />
    </g>

    <g v-else-if="kind === 'bars'">
      <rect class="fill-a" x="10" y="16" width="9" height="16" /><rect class="fill-b" x="21" y="8" width="9" height="24" />
      <rect class="fill-a" x="35" y="20" width="9" height="12" /><rect class="fill-b" x="46" y="11" width="9" height="21" />
    </g>

    <g v-else-if="kind === 'stacked'">
      <template v-for="(x, i) in [10, 23, 36, 49]" :key="x">
        <rect class="fill-a" :x="x" y="4" width="9" :height="[10, 16, 7, 12][i]" />
        <rect class="fill-b" :x="x" :y="4 + [10, 16, 7, 12][i]" width="9" :height="[12, 6, 14, 9][i]" />
        <rect class="fill-c" :x="x" :y="4 + [22, 22, 21, 21][i]" width="9" :height="28 - [22, 22, 21, 21][i]" />
      </template>
    </g>

    <g v-else-if="kind === 'line'">
      <polyline class="line-a" points="7,26 16,22 25,23 34,15 43,12 55,7" />
      <polyline class="line-b" points="7,29 16,28 25,25 34,26 43,21 55,19" />
    </g>

    <g v-else-if="kind === 'elbow'">
      <polyline class="line-a" points="9,28 18,14 27,9 36,7.5 45,7 54,6.8" />
      <circle class="dot-a" cx="9" cy="28" r="1.6" /><circle class="dot-a" cx="18" cy="14" r="1.6" />
      <circle class="dot-a" cx="27" cy="9" r="2.4" /><circle class="dot-a" cx="36" cy="7.5" r="1.6" />
      <circle class="dot-a" cx="45" cy="7" r="1.6" /><circle class="dot-a" cx="54" cy="6.8" r="1.6" />
    </g>

    <g v-else-if="kind === 'heatmap'">
      <template v-for="r in 4" :key="r">
        <rect v-for="c in 6" :key="c" class="cell" :x="6 + (c - 1) * 8.4" :y="2 + (r - 1) * 8"
              width="8" height="7.6" :style="{ opacity: 0.15 + (((r * 7 + c * 5) % 9) / 9) * 0.85 }" />
      </template>
    </g>

    <g v-else-if="kind === 'scatter'">
      <circle v-for="(p, i) in [[14,10],[17,13],[12,15],[16,8],[19,11],[38,22],[41,25],[36,26],[43,21],[40,19],[44,10],[47,12],[49,8],[45,7],[25,26],[28,24],[23,22]]"
              :key="i" :class="i < 5 ? 'dot-a' : i < 10 ? 'dot-b' : 'dot'" :cx="p[0]" :cy="p[1]" r="1.4" />
    </g>

    <g v-else-if="kind === 'tracks'">
      <polyline class="line-a" points="31,18 35,13 39,12 44,7" /><polyline class="line-b" points="31,18 26,21 22,20 15,26" />
      <polyline class="line-a" points="31,18 34,23 40,25 47,27" /><polyline class="line-b" points="31,18 28,12 24,9 20,5" />
      <circle class="dot" cx="31" cy="18" r="1.2" />
    </g>

    <g v-else-if="kind === 'gates'">
      <circle v-for="(p, i) in [[12,25],[15,22],[18,27],[14,28],[34,12],[37,15],[40,10],[38,13],[42,16],[36,9],[45,12]]"
              :key="i" class="dot" :cx="p[0]" :cy="p[1]" r="1.2" />
      <path class="gate" d="M30 6 H49 V20 H30 Z" />
    </g>

    <g v-else-if="kind === 'image'">
      <rect class="frame" x="3" y="4" width="17" height="28" /><rect class="frame" x="22" y="4" width="17" height="28" />
      <rect class="frame" x="41" y="4" width="17" height="28" />
      <circle class="fill-a" cx="10" cy="14" r="3" /><circle class="fill-b" cx="15" cy="24" r="2.4" />
      <circle class="fill-a" cx="31" cy="16" r="3" /><circle class="fill-b" cx="28" cy="25" r="2.4" />
      <circle class="fill-a" cx="50" cy="18" r="3" /><circle class="fill-b" cx="46" cy="10" r="2.4" />
    </g>

    <g v-else-if="kind === 'cards'">
      <template v-for="x in [3, 22, 41]" :key="x">
        <rect class="frame" :x="x" y="3" width="16" height="30" />
        <rect class="fill-a" :x="x + 2" y="5" width="12" height="15" />
        <line class="dim" :x1="x + 2" y1="24" :x2="x + 14" y2="24" /><line class="dim" :x1="x + 2" y1="28" :x2="x + 10" y2="28" />
      </template>
    </g>

    <g v-else-if="kind === 'timeline'">
      <line class="lane-a" x1="6" y1="8" x2="34" y2="8" /><line class="lane-b" x1="22" y1="15" x2="54" y2="15" />
      <line class="lane-a" x1="10" y1="22" x2="28" y2="22" /><line class="lane-b" x1="31" y1="22" x2="55" y2="22" />
      <line class="lane-a" x1="4" y1="29" x2="46" y2="29" />
      <line class="dim" x1="28" y1="22" x2="31" y2="22" stroke-dasharray="1 1" />
    </g>
  </svg>
</template>

<style scoped>
.plot-sketch { width: 60px; height: 36px; flex-shrink: 0; }
.ax { fill: none; stroke: var(--cc-text-dim); stroke-width: 0.8; opacity: 0.6; }
.dim { stroke: var(--cc-text-dim); stroke-width: 0.8; }
.mark { stroke: var(--cc-text); stroke-width: 1; }
.fill-a { fill: var(--cc-accent); opacity: 0.85; }
.fill-b { fill: var(--cc-text-dim); opacity: 0.55; }
.fill-c { fill: var(--cc-text-dim); opacity: 0.25; }
.cell { fill: var(--cc-accent); }
.dot { fill: var(--cc-text-dim); }
.dot-a { fill: var(--cc-accent); }
.dot-b { fill: var(--cc-text); opacity: 0.7; }
.line-a { fill: none; stroke: var(--cc-accent); stroke-width: 1.4; stroke-linejoin: round; }
.line-b { fill: none; stroke: var(--cc-text-dim); stroke-width: 1.4; stroke-linejoin: round; }
.lane-a { stroke: var(--cc-accent); stroke-width: 3; stroke-linecap: round; }
.lane-b { stroke: var(--cc-text-dim); stroke-width: 3; stroke-linecap: round; opacity: 0.7; }
.frame { fill: var(--cc-surface-2); stroke: var(--cc-border); stroke-width: 0.6; }
.gate { fill: var(--cc-accent); fill-opacity: 0.12; stroke: var(--cc-accent); stroke-width: 1; }
</style>
