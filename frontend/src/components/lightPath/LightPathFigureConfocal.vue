<!-- Confocal light path: visible lines → AOTF → main beam splitter → scan mirrors → objective → one
     plane; emission back through the scanner (descanned) → pinhole → emission bands → PMT/HyD.
     Generic, not one scope's layout. -->
<script setup lang="ts">
import LightPathMarks from './LightPathMarks.vue'
import { LIGHT_PATH_COLOURS as C } from './colours'

defineProps<{ hover: number | null }>()

// Stage markers by stage number (TECHNOLOGY_STAGES.confocal).
const MARKS: Record<number, [number, number]> = {
  1: [38, 8], 2: [115, 16], 3: [208, 8], 4: [290, 124], 5: [326, 170],
  6: [132, 92], 7: [360, 94], 8: [468, 104], 9: [468, 160], 10: [372, 196],
}
</script>

<template>
  <svg class="lp-fig" viewBox="0 0 490 205" fill="none" role="img"
       aria-label="Schematic confocal light path with numbered stages">
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
    <!-- 6 pinhole -->
    <ellipse class="lp-glass" cx="156" cy="84" rx="12" ry="3" />
    <path class="lp-ink lp-heavy" d="M144 96 h9 M159 96 h9" />
    <path class="lp-em" :stroke="C.em" d="M156 96 V108 H350" />
    <!-- 7 emission bands (prism / filters) -->
    <path class="lp-dim" d="M350 98 l20 10 l-20 10 z" />
    <path class="lp-em lp-thin" :stroke="C.em" d="M370 108 L400 92 M370 108 L400 108 M370 108 L400 124" />
    <path class="lp-dim lp-filter" d="M404 86 v12 M404 102 v12 M404 118 v12" />
    <!-- 8 detectors -->
    <rect v-for="y in [84, 101, 118]" :key="y" class="lp-frame" x="410" :y="y" width="20" height="14" rx="3" />
    <!-- 9 digitisation -->
    <path class="lp-ink" d="M430 91 h10 V152 M430 108 h10 M430 125 h10" />
    <rect class="lp-frame" x="424" y="152" width="32" height="16" rx="3" />
    <path class="lp-dim" d="M430 164 v-3 M435 164 v-7 M440 164 v-5 M445 164 v-8 M450 164 v-2" />
    <!-- 10 stack, time, tiles -->
    <path v-for="dy in [0, 8, 16]" :key="dy" class="lp-frame" :d="`M352 ${166 + dy} h32 l10 -6 h-32 z`" />

    <LightPathMarks :marks="MARKS" :hover="hover" />
  </svg>
</template>
