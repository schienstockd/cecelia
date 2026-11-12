<!-- Airyscan light path: the confocal path (visible lines → AOTF → scanner → objective → one plane,
     emission descanned) onto an opened pinhole and a hexagonal detector array, then processing.
     Generic, not one scope's layout. -->
<script setup lang="ts">
import LightPathMarks from './LightPathMarks.vue'
import { LIGHT_PATH_COLOURS as C } from './colours'

defineProps<{ hover: number | null }>()

// Stage markers by stage number (TECHNOLOGY_STAGES.airyscan).
const MARKS: Record<number, [number, number]> = {
  1: [38, 8], 2: [115, 16], 3: [208, 8], 4: [290, 124], 5: [326, 170],
  6: [132, 92], 7: [348, 92], 8: [392, 80], 9: [470, 170], 10: [372, 196],
}

// A hexagon of detector elements around the centre one (drawn smaller than the real 32).
const ARRAY: [number, number][] = [[0, 0], [9, 0], [-9, 0], [4.5, 7.8], [-4.5, 7.8], [4.5, -7.8], [-4.5, -7.8],
  [13.5, 7.8], [-13.5, 7.8], [13.5, -7.8], [-13.5, -7.8], [0, 15.6], [0, -15.6], [18, 0], [-18, 0]]
</script>

<template>
  <svg class="lp-fig" viewBox="0 0 490 205" fill="none" role="img"
       aria-label="Schematic Airyscan light path with numbered stages">
    <!-- 1 lasers -->
    <rect class="lp-frame" x="10" y="14" width="56" height="40" rx="4" />
    <path class="lp-ink" d="M18 22 h10 M18 34 h10 M18 46 h10" />
    <path class="lp-exc" :stroke="C.exc" d="M66 33 H100" />
    <!-- 2 power control (AOTF) -->
    <rect class="lp-frame" x="100" y="24" width="30" height="18" rx="3" />
    <path class="lp-dim" d="M106 38 l4 -10 l4 10 l4 -10 l4 10" />
    <path class="lp-exc" :stroke="C.exc" d="M130 33 H196" />
    <!-- main beam splitter: excitation on, emission down to the pinhole -->
    <path class="lp-ink lp-heavy" d="M150 46 l22 -26" />
    <!-- 3 scan mirrors -->
    <circle class="lp-dim" cx="200" cy="33" r="10" />
    <circle class="lp-dim" cx="216" cy="33" r="10" />
    <path class="lp-ink" d="M193 42 l14 -18 M209 42 l14 -18" />
    <path class="lp-exc" :stroke="C.exc" d="M216 33 H262" />
    <ellipse class="lp-glass" cx="246" cy="33" rx="4" ry="15" />
    <path class="lp-ink" d="M254 42 l16 -18" />
    <path class="lp-exc" :stroke="C.exc" d="M262 33 V112" />
    <!-- 4 objective -->
    <path class="lp-frame" d="M244 112 h36 v8 l-6 20 h-24 l-6 -20 z" />
    <path class="lp-exc" :stroke="C.exc" d="M253 140 L262 162 L271 140" />
    <!-- 5 sample: one plane in focus -->
    <rect class="lp-tissue" x="208" y="150" width="108" height="40" rx="4" />
    <path class="lp-dim lp-hair" d="M208 162 h108 M208 176 h108" />
    <circle cx="262" cy="162" r="3" :fill="C.exc" />
    <!-- emission back through the scanner (descanned), then down to the pinhole -->
    <path class="lp-em lp-thin" :stroke="C.em" d="M268 156 L268 38" />
    <path class="lp-em lp-thin" :stroke="C.em" d="M224 26 H156 V78" />
    <!-- 6 pinhole, opened wide -->
    <ellipse class="lp-glass" cx="156" cy="84" rx="12" ry="3" />
    <path class="lp-ink lp-heavy" d="M140 96 h6 M166 96 h6" />
    <path class="lp-em" :stroke="C.em" d="M156 96 V108 H350" />
    <!-- 7 emission filter -->
    <path class="lp-dim lp-filter" d="M356 100 v16" />
    <!-- 8 detector array -->
    <circle v-for="([x, y], i) in ARRAY" :key="i" class="lp-frame" :cx="392 + x" :cy="108 + y" r="4.2" />
    <!-- 9 processing -->
    <path class="lp-ink" d="M414 108 h14 V150" />
    <rect class="lp-frame" x="408" y="150" width="48" height="18" rx="3" />
    <path class="lp-dim" d="M414 159 h36 M420 154 v10 M432 154 v10 M444 154 v10" />
    <!-- 10 stack, time, tiles -->
    <path v-for="dy in [0, 8, 16]" :key="dy" class="lp-frame" :d="`M352 ${166 + dy} h32 l10 -6 h-32 z`" />

    <LightPathMarks :marks="MARKS" :hover="hover" />
  </svg>
</template>
