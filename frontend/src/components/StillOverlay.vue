<!--
  Vector scale bar + timestamp for a captured still (Phase E2). An SVG whose viewBox is the frame's
  physical extent (µm) with preserveAspectRatio "xMidYMid meet" — the SAME fit as the frame <img>'s
  object-fit: contain — so annotations stay geometrically correct AND aligned to the image content even
  when the frame is letterboxed. The scale bar length is drawn in µm (viewBox units), so it's correct by
  construction; text/bar sizes are fractions of the extent so they scale proportionally with the frame.
  Drawn on the CLEAN capture (napari's own scale bar/timestamp hidden — see E1). Absolutely fills the
  parent (which must be position:relative and the frame's box).
-->
<script setup lang="ts">
import { computed } from 'vue'
import { niceScaleBar } from '../utils/stillOverlay'

const props = withDefaults(defineProps<{
  extentUm?: { x?: number; y?: number; unit?: string | null } | null
  timeLabel?: string        // '' → no timestamp
  showScaleBar?: boolean
  showTimestamp?: boolean
}>(), { showScaleBar: true, showTimestamp: true })

const ex = computed(() => props.extentUm?.x ?? 0)
const ey = computed(() => props.extentUm?.y ?? 0)
const ok = computed(() => ex.value > 0 && ey.value > 0)
const bar = computed(() => niceScaleBar(ex.value, props.extentUm?.unit ?? 'µm'))

// geometry in viewBox (µm) units — margins/sizes as fractions of the extent so they scale with the frame
const mx = computed(() => ex.value * 0.045)
const my = computed(() => ey.value * 0.045)
const barH = computed(() => ey.value * 0.012)
const font = computed(() => Math.min(ex.value, ey.value) * 0.05)
const barX1 = computed(() => ex.value - mx.value - (bar.value?.um ?? 0))
const barX2 = computed(() => ex.value - mx.value)
const barY = computed(() => ey.value - my.value - barH.value)
</script>

<template>
  <svg v-if="ok" class="still-ovl" :viewBox="`0 0 ${ex} ${ey}`" preserveAspectRatio="xMidYMid meet">
    <!-- timestamp, top-left -->
    <text v-if="showTimestamp && timeLabel" :x="mx" :y="my + font" :font-size="font"
          class="ovl-text" text-anchor="start">{{ timeLabel }}</text>
    <!-- scale bar, bottom-right: bar + label centered above it -->
    <template v-if="showScaleBar && bar">
      <rect :x="barX1" :y="barY" :width="bar.um" :height="barH" class="ovl-fill" />
      <text :x="(barX1 + barX2) / 2" :y="barY - font * 0.35" :font-size="font"
            class="ovl-text" text-anchor="middle">{{ bar.label }}</text>
    </template>
  </svg>
</template>

<style scoped>
.still-ovl { position: absolute; inset: 0; width: 100%; height: 100%; pointer-events: none; }
/* white with a dark outline so it reads on any background (like napari's overlays) */
.ovl-text { fill: #fff; paint-order: stroke; stroke: rgba(0,0,0,0.85); stroke-width: 0.5;
  font-weight: 700; font-family: system-ui, sans-serif; }
.ovl-fill { fill: #fff; stroke: rgba(0,0,0,0.85); stroke-width: 0.3; }
</style>
