<!--
  The small schematic on an expanded Call for Datasets row (`fig` in lib/callForDatasets.ts) — the
  same mechanism as the Kiwi / Claude Code cards (ClaudeWaysIn.vue): a hand-drawn inline SVG on a
  96×60 viewBox at 72×45 px, strokes in theme tokens only so it follows light/dark, one accent for
  the thing the dataset would show. An entry with nothing worth drawing has no `fig`.
-->
<script setup lang="ts">
import type { AskFig } from '../lib/callForDatasets'

defineProps<{ fig: AskFig }>()
</script>

<template>
  <svg class="af" viewBox="0 0 96 60" fill="none" aria-hidden="true">
    <template v-if="fig === 'tilt'">
      <!-- the planes at t=0, flat; later the same planes tilted through Z -->
      <path v-for="y in [16, 30, 44]" :key="y" class="af-dim" :d="`M4 ${y} h26 l8 -6 h-26z`" />
      <path class="af-ink" d="M42 30 h10 M49 27 l3 3 l-3 3" />
      <g transform="rotate(-22 74 30)">
        <path v-for="y in [16, 30, 44]" :key="y" class="af-acc" :d="`M58 ${y} h26 l8 -6 h-26z`" />
      </g>
    </template>

    <template v-else-if="fig === 'cycles'">
      <!-- two staining cycles of one 3D block, offset; registration lines them up -->
      <path class="af-dim" d="M10 22 h30 v30 h-30z M10 22 l10 -10 h30 l-10 10 M40 52 l10 -10 v-30" />
      <path class="af-acc af-dash" d="M44 16 h30 v30 h-30z M44 16 l10 -10 h30 l-10 10 M74 46 l10 -10 v-30" />
      <path class="af-ink" d="M42 34 h-8 M37 31 l-3 3 l3 3" />
    </template>

    <template v-else-if="fig === 'clusters'">
      <!-- many clusters; a proposed grouping around them -->
      <circle v-for="[x, y] in [[12, 18], [20, 26], [14, 34], [24, 14]]" :key="`a${x}${y}`" class="af-ink" :cx="x" :cy="y" r="3" />
      <circle v-for="[x, y] in [[46, 40], [54, 48], [42, 50], [56, 36]]" :key="`b${x}${y}`" class="af-dim" :cx="x" :cy="y" r="3" />
      <circle v-for="[x, y] in [[76, 16], [86, 22], [80, 28]]" :key="`c${x}${y}`" class="af-ink" :cx="x" :cy="y" r="3" />
      <ellipse class="af-acc af-dash" cx="18" cy="24" rx="15" ry="16" />
      <ellipse class="af-acc af-dash" cx="49" cy="43" rx="14" ry="13" />
      <ellipse class="af-acc af-dash" cx="81" cy="22" rx="12" ry="12" />
    </template>

    <template v-else-if="fig === 'bleed'">
      <!-- one label, two channels: what leaks into the second is the measured bleedthrough -->
      <rect class="af-frame" x="4" y="10" width="36" height="40" rx="3" />
      <rect class="af-frame" x="56" y="10" width="36" height="40" rx="3" />
      <circle class="af-ink af-fill" cx="22" cy="30" r="8" />
      <circle class="af-dim af-dash" cx="74" cy="30" r="8" />
      <path class="af-acc af-dash" d="M32 30 h32 M60 27 l4 3 l-4 3" />
    </template>

    <template v-else-if="fig === 'masks'">
      <!-- a slice of a stack, its cells outlined by hand -->
      <rect class="af-frame" x="14" y="4" width="70" height="44" rx="3" />
      <rect class="af-frame" x="8" y="10" width="70" height="44" rx="3" />
      <ellipse v-for="[x, y, r] in [[24, 26, 7], [44, 36, 8], [62, 24, 7]]" :key="`${x}`" class="af-dim af-fill" :cx="x" :cy="y" :rx="r" :ry="r - 1" />
      <path class="af-acc" d="M15 25 q2 -10 11 -8 q8 2 6 10 q-2 8 -10 6 q-8 -1 -7 -8z" />
      <path class="af-acc" d="M35 35 q2 -11 11 -9 q9 3 6 11 q-3 8 -11 6 q-8 -1 -6 -8z" />
      <path class="af-acc" d="M54 22 q3 -8 10 -6 q7 2 5 9 q-2 7 -9 6 q-8 -1 -6 -9z" />
    </template>

    <template v-else-if="fig === 'pair'">
      <!-- the same field, noisy and clean -->
      <rect class="af-frame" x="4" y="10" width="36" height="40" rx="3" />
      <rect class="af-frame" x="56" y="10" width="36" height="40" rx="3" />
      <circle class="af-dim af-dash" cx="22" cy="30" r="8" />
      <circle v-for="[x, y] in [[9, 16], [15, 44], [33, 15], [35, 40], [11, 31], [28, 24], [19, 36], [31, 33], [25, 46], [8, 23]]"
              :key="`${x}${y}`" class="af-dim af-fill" :cx="x" :cy="y" r="0.8" />
      <circle class="af-ink af-fill" cx="74" cy="30" r="8" />
      <path class="af-acc" d="M44 30 h8" />
    </template>

    <template v-else-if="fig === 'swap'">
      <!-- the same markers, in two channel orders -->
      <rect v-for="y in [10, 26, 42]" :key="`l${y}`" class="af-frame" x="8" :y="y" width="18" height="10" rx="2" />
      <rect v-for="y in [10, 26, 42]" :key="`r${y}`" class="af-frame" x="70" :y="y" width="18" height="10" rx="2" />
      <path class="af-acc" d="M26 15 L70 31 M26 31 L70 15" />
      <path class="af-dim" d="M26 47 H70" />
    </template>

    <template v-else-if="fig === 'tracks'">
      <!-- two tracks that pass close, one dividing -->
      <path class="af-ink" d="M6 48 L22 40 L36 34 L50 30 L64 22 L78 12" />
      <path class="af-acc" d="M6 14 L20 22 L34 30 L48 34 L62 40" />
      <path class="af-acc" d="M62 40 L78 36 L90 34 M62 40 L76 50 L90 54" />
      <circle class="af-acc af-fill" cx="62" cy="40" r="2" />
    </template>

    <template v-else-if="fig === 'switch'">
      <!-- a cell that circles in place, then migrates off; the switch is marked -->
      <path class="af-dim" d="M14 36 l6 -8 l5 9 l-9 3 l3 -10 l8 4 l-6 7 l-2 -9 l9 1" />
      <path class="af-ink" d="M28 33 L44 28 L60 22 L76 16 L90 12" />
      <circle class="af-acc af-dash" cx="28" cy="33" r="6" />
    </template>
  </svg>
</template>

<style scoped>
.af { flex: none; width: 72px; height: 45px; stroke-linecap: round; stroke-linejoin: round; stroke-width: 1.3; }
/* the stroke vocabulary of ClaudeWaysIn.vue's schematics, with the app accent for the point */
.af-frame { stroke: var(--cc-border); stroke-width: 1.5; fill: var(--cc-surface-1); }
.af-ink { stroke: var(--cc-text); }
.af-dim { stroke: var(--cc-text-dim); }
.af-acc { stroke: var(--cc-accent); }
.af-fill { fill: currentColor; }
.af-ink.af-fill { color: var(--cc-text); }
.af-dim.af-fill { color: var(--cc-text-dim); fill-opacity: 0.35; }
.af-acc.af-fill { color: var(--cc-accent); }
.af-dash { stroke-dasharray: 2.5 2.5; }
</style>
