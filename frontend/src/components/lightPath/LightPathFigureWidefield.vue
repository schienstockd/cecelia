<!-- Widefield light path: lamp/LED → filter cube (exciter, dichroic, emitter) → objective → the whole
     field lit at once; emission back through the cube to a camera. Generic, not one scope's layout. -->
<script setup lang="ts">
import LightPathMarks from './LightPathMarks.vue'
import { LIGHT_PATH_COLOURS as C } from './colours'

defineProps<{ hover: number | null }>()

// Stage markers by stage number (TECHNOLOGY_STAGES.widefield).
const MARKS: Record<number, [number, number]> = {
  1: [35, 10], 2: [258, 24], 3: [255, 124], 4: [281, 166], 5: [372, 16], 6: [366, 78], 7: [400, 142],
}
</script>

<template>
  <svg class="lp-fig" viewBox="0 0 490 205" fill="none" role="img"
       aria-label="Schematic widefield light path with numbered stages">
    <!-- 1 illumination -->
    <rect class="lp-frame" x="10" y="20" width="50" height="26" rx="4" />
    <circle class="lp-dim" cx="26" cy="33" r="6" />
    <path class="lp-dim" d="M38 27 l6 -4 M38 39 l6 4 M40 33 h7" />
    <path class="lp-exc" :stroke="C.exc" d="M60 30 H180 M60 36 H180" />
    <!-- 2 filter cube: exciter, dichroic, emitter -->
    <rect class="lp-frame" x="180" y="14" width="68" height="76" rx="4" />
    <path class="lp-dim lp-filter" d="M190 26 v14" />
    <path class="lp-ink lp-heavy" d="M200 60 l34 -34" />
    <path class="lp-dim lp-filter" d="M210 80 h14" />
    <path class="lp-exc" :stroke="C.exc" d="M214 40 V112 M220 40 V112" />
    <!-- 3 objective -->
    <path class="lp-frame" d="M199 112 h36 v8 l-6 20 h-24 l-6 -20 z" />
    <path class="lp-exc" :stroke="C.exc" d="M206 140 L206 158 M228 140 L228 158" />
    <!-- 4 sample: the whole field lit at once -->
    <rect class="lp-tissue" x="163" y="150" width="108" height="40" rx="4" />
    <rect x="196" y="156" width="42" height="28" :fill="C.exc" opacity="0.18" />
    <path class="lp-dim lp-hair" d="M163 162 h108 M163 176 h108" />
    <!-- emission back up through the cube to the camera -->
    <path class="lp-em lp-thin" :stroke="C.em" d="M211 148 V84 M223 148 V84" />
    <path class="lp-em" :stroke="C.em" d="M217 80 V8 H330 V24" />
    <!-- 5 camera -->
    <rect class="lp-frame" x="312" y="24" width="40" height="30" rx="4" />
    <path class="lp-dim lp-hair" d="M318 30 h28 M318 36 h28 M318 42 h28 M318 48 h28 M324 26 v26 M332 26 v26 M340 26 v26" />
    <!-- 6 digitisation -->
    <path class="lp-ink" d="M332 54 V70" />
    <rect class="lp-frame" x="316" y="70" width="32" height="16" rx="3" />
    <path class="lp-dim" d="M322 82 v-3 M327 82 v-7 M332 82 v-5 M337 82 v-8 M342 82 v-2" />
    <!-- 7 stack, time, tiles -->
    <path v-for="dy in [0, 8, 16]" :key="dy" class="lp-frame" :d="`M352 ${166 + dy} h32 l10 -6 h-32 z`" />
    <path class="lp-frame" d="M400 150 h24 v24 h-24 z M424 150 h24 v24 h-24 z M400 174 h24 v24 h-24 z M424 174 h24 v24 h-24 z" />

    <LightPathMarks :marks="MARKS" :hover="hover" />
  </svg>
</template>
