<!-- Two-photon light path: laser → power → scan mirrors → primary dichroic → objective → focus at
     depth; emission back to non-descanned detectors. Generic, not one scope's layout. -->
<script setup lang="ts">
import LightPathMarks from './LightPathMarks.vue'
import { LIGHT_PATH_COLOURS as C } from './colours'

defineProps<{ hover: number | null }>()

// Stage markers on the drawing, by stage number (TECHNOLOGY_STAGES['two-photon']).
const MARKS: Record<number, [number, number]> = {
  1: [38, 12], 2: [113, 12], 3: [178, 10], 4: [276, 124], 5: [234, 168],
  6: [282, 70], 7: [470, 98], 8: [370, 134], 9: [368, 176],
}
</script>

<template>
  <svg class="lp-fig" viewBox="0 0 490 200" fill="none" role="img"
       aria-label="Schematic two-photon light path with numbered stages">
    <!-- 1 laser -->
    <rect class="lp-frame" x="10" y="22" width="56" height="22" rx="4" />
    <path class="lp-ink" d="M20 33 h14 M40 27 v12 M45 27 v12" />
    <path class="lp-exc" :stroke="C.exc" d="M66 33 H100" />
    <!-- 2 power control -->
    <rect class="lp-frame" x="100" y="24" width="26" height="18" rx="3" />
    <path class="lp-dim" d="M105 38 l4 -10 l4 10 l4 -10 l4 10" />
    <path class="lp-exc" :stroke="C.exc" d="M126 33 H168" />
    <!-- 3 scan mirrors -->
    <circle class="lp-dim" cx="172" cy="33" r="10" />
    <circle class="lp-dim" cx="188" cy="33" r="10" />
    <path class="lp-ink" d="M165 42 l14 -18 M181 42 l14 -18" />
    <path class="lp-exc" :stroke="C.exc" d="M188 33 H226" />
    <path class="lp-exc lp-thin" :stroke="C.exc" d="M188 33 L226 26 M188 33 L226 40" />
    <ellipse class="lp-glass" cx="230" cy="33" rx="4" ry="14" />
    <path class="lp-exc" :stroke="C.exc" d="M234 33 H260" />
    <ellipse class="lp-glass" cx="264" cy="33" rx="4" ry="17" />
    <path class="lp-exc" :stroke="C.exc" d="M268 33 H300" />
    <!-- fold down; 6 primary dichroic passes the infrared down, sends emission right -->
    <path class="lp-ink" d="M292 42 l16 -18" />
    <path class="lp-exc" :stroke="C.exc" d="M300 33 V112" />
    <path class="lp-ink lp-heavy" d="M288 90 l26 -20" />
    <!-- 4 objective -->
    <path class="lp-frame" d="M282 112 h36 v8 l-6 20 h-24 l-6 -20 z" />
    <path class="lp-exc" :stroke="C.exc" d="M291 140 L300 166 L309 140" />
    <!-- 5 sample, focus at depth -->
    <rect class="lp-tissue" x="246" y="150" width="108" height="40" rx="4" />
    <path class="lp-dim lp-hair" d="M246 160 h108 M246 172 h108 M246 182 h108" />
    <circle cx="300" cy="166" r="3" :fill="C.exc" />
    <circle cx="300" cy="166" r="7" class="lp-em lp-thin" :stroke="C.em" />
    <!-- emission back up to the dichroic, then out to the detectors -->
    <path class="lp-em lp-thin" :stroke="C.em" d="M305 160 L305 82" />
    <path class="lp-em" :stroke="C.em" d="M305 80 H448" />
    <g v-for="x in [340, 380, 420]" :key="x">
      <path class="lp-ink" :d="`M${x - 7} ${88} l14 -16`" />
      <path class="lp-em lp-thin" :stroke="C.em" :d="`M${x} 80 V94`" />
    </g>
    <path class="lp-em lp-thin" :stroke="C.em" d="M448 80 H460 V94" />
    <!-- filters + 7 detectors -->
    <path v-for="x in [340, 380, 420, 460]" :key="`f${x}`" class="lp-dim lp-filter" :d="`M${x - 6} 92 h12`" />
    <rect v-for="x in [340, 380, 420, 460]" :key="`d${x}`" class="lp-frame" :x="x - 9" y="96" width="18" height="16" rx="3" />
    <!-- 8 digitisation -->
    <path class="lp-ink" d="M340 112 V118 H460 V112 M380 112 V118 M420 112 V118 M400 118 V126" />
    <rect class="lp-frame" x="384" y="126" width="32" height="16" rx="3" />
    <path class="lp-dim" d="M390 138 v-3 M395 138 v-7 M400 138 v-5 M405 138 v-8 M410 138 v-2" />
    <path class="lp-ink" d="M400 142 V152" />
    <!-- 9 stack and time -->
    <path v-for="dy in [0, 8, 16]" :key="dy" class="lp-frame" :d="`M386 ${160 + dy} h32 l10 -6 h-32 z`" />
    <path class="lp-dim" d="M434 178 h32 M461 174 l5 4 l-5 4" />

    <LightPathMarks :marks="MARKS" :hover="hover" />
  </svg>
</template>
