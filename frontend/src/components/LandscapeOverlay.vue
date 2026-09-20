<!--
  LandscapeOverlay — cheap tile-level semantic heatmap over the viewer, PR #6 of
  `docs/todo/BIDIR_CONTEXT_PLAN.md` (rescoped 2026-09-20 — Decision 14). Categorical fills
  (`dark` / `bright-uniform` / `bright-textured` / `edge` / `mixed`) painted over the same grid
  GridOverlay draws, so a share-in / MCP reader has a rough semantic prior BEFORE reading raw RGB.

  NOT A SEGMENTATION. Fills are per TILE, not per pixel, and the swatches are the same speakable
  legend (`bright-textured`) so a viewer at a glance knows the resolution is tile-level. Legend is
  emitted alongside — the parent renders it near the grid density control so the two controls read
  as one feature.

  `showLabels` (debug toggle): print the category name in each tile — the same HTML-labels pattern
  GridOverlay uses for A1..H8. Off by default; the user turns it on to answer "is that cluster
  actually `bright-textured`?" and then off. Not for Claude — the MCP tool returns the category
  per tile in JSON; this is only for a human eyeball check.

  All computation is in `utils/landscape.ts`; this component is layout + colour only. Coord frame
  matches GridOverlay: viewport-relative, `pointer-events: none`, sits ABOVE the WebGPU canvas
  and BELOW the SoM grid so the grid labels stay legible on top.
-->
<script setup lang="ts">
import { computed } from 'vue'
import type { LandscapeResult } from '../utils/landscape'
import { LANDSCAPE_SWATCHES } from '../utils/landscape'

const props = defineProps<{
  landscape: LandscapeResult | null
  opacity?: number   // 0..1; low by default so the frame stays readable underneath
  showLabels?: boolean
}>()

const OPACITY_DEFAULT = 0.28

const paintOpacity = computed(() => Math.min(1, Math.max(0, props.opacity ?? OPACITY_DEFAULT)))

const vbW = 100
const vbH = 100
const cellW = computed(() => vbW / Math.max(1, props.landscape?.grid.cols ?? 1))
const cellH = computed(() => vbH / Math.max(1, props.landscape?.grid.rows ?? 1))
</script>

<template>
  <div v-if="landscape" class="cc-landscape-overlay" aria-hidden="true">
    <svg class="cc-landscape-fill"
         :viewBox="`0 0 ${vbW} ${vbH}`" preserveAspectRatio="none">
      <rect v-for="t in landscape.tiles" :key="t.id"
            :x="t.col * cellW" :y="t.row * cellH"
            :width="cellW" :height="cellH"
            :fill="LANDSCAPE_SWATCHES[t.category]"
            :fill-opacity="paintOpacity" />
    </svg>
    <!-- Debug labels — HTML layer at fractional percent positions, same pattern GridOverlay uses.
         Kept OFF a text-drawing SVG viewBox because a non-square viewport distorts SVG text. -->
    <template v-if="showLabels">
      <span v-for="t in landscape.tiles" :key="`lbl-${t.id}`" class="cc-landscape-label"
            :style="{ top: `${(t.row + 0.5) * cellH}%`, left: `${(t.col + 0.5) * cellW}%` }">
        {{ t.category }}
      </span>
    </template>
  </div>
</template>

<style scoped>
.cc-landscape-overlay {
  position: absolute; inset: 0;
  pointer-events: none;      /* never blocks the viewer's own pan/zoom */
  user-select: none;
}
.cc-landscape-fill {
  position: absolute; inset: 0;
  width: 100%; height: 100%;
}
.cc-landscape-label {
  position: absolute;
  transform: translate(-50%, -50%);
  color: #fff;
  font: 600 10px system-ui, sans-serif;
  /* Halo mirrors GridOverlay so the label stays readable over any tile colour. */
  text-shadow:
    0 0 2px rgba(0, 0, 0, 0.9),
    0 0 3px rgba(0, 0, 0, 0.75);
  white-space: nowrap;
}
</style>
