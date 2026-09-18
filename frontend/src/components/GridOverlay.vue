<!--
  Set-of-Mark grid drawn over the WebGPU viewer (`modules/ViewerWindow.vue`). A N×N mesh with
  spreadsheet-style cell labels (A1..H8 at the default 8×8) so a share-in / point-out session can
  cite a region by NAME instead of pixel coords. PR #2 of `docs/todo/BIDIR_CONTEXT_PLAN.md`;
  toggled from the Annotations section of ViewerWindow's viewer panel.

  In VIEWPORT coords, not image coords: at zoom-in "C4" means a quarter of the currently visible
  view, which is what the SoM use case wants. When the user pans/zooms, the grid stays put over
  the canvas rect and re-labels what they now see. Pure decoration — `pointer-events: none`, no
  click handling, doesn't intercept the viewer's own pan/zoom.

  SVG with `viewBox="0 0 100 100"` + `preserveAspectRatio="none"` so a percentage-of-viewport
  layout scales cleanly to whatever CSS size the canvas actually settles at (the viewer canvas is
  responsive; a fixed pixel viewBox would tear on resize). Labels are counter-scaled to real px so
  they read the same at any density.

  Cell math + labels live in `utils/gridOverlay.ts` (pure, tested). This component is layout only.
-->
<script setup lang="ts">
import { computed } from 'vue'
import { gridCells, clampDensity, GRID_DENSITY_DEFAULT } from '../utils/gridOverlay'

const props = defineProps<{
  cols?: number   // grid density (clamped 4..16 in the util). Falls back to the plan default.
  rows?: number   // if omitted, square grid (rows = cols)
}>()

const nCols = computed(() => clampDensity(props.cols ?? GRID_DENSITY_DEFAULT))
const nRows = computed(() => clampDensity(props.rows ?? nCols.value))
const cells = computed(() => gridCells(nCols.value, nRows.value))

// SVG uses a 100×100 viewBox for percent-of-viewport arithmetic. Cells sit at their fractional
// (col+0.5, row+0.5) centres so labels centre-anchor regardless of density.
const vbW = 100
const vbH = 100
const cellW = computed(() => vbW / nCols.value)
const cellH = computed(() => vbH / nRows.value)
</script>

<template>
  <svg class="cc-grid-overlay" aria-hidden="true"
       :viewBox="`0 0 ${vbW} ${vbH}`" preserveAspectRatio="none">
    <!-- interior column dividers: N-1 vertical lines at fractional x positions; skip the outer
         edges so the border doesn't double against the canvas frame. -->
    <line v-for="i in nCols - 1" :key="`v${i}`" :x1="i * cellW" :y1="0" :x2="i * cellW" :y2="vbH"
          class="cc-grid-line" vector-effect="non-scaling-stroke" />
    <line v-for="i in nRows - 1" :key="`h${i}`" :x1="0" :y1="i * cellH" :x2="vbW" :y2="i * cellH"
          class="cc-grid-line" vector-effect="non-scaling-stroke" />
    <!-- labels centred in each cell. `vector-effect` doesn't apply to text; the counter-scale
         is unnecessary because we use screen-px font sizing (SVG `font-size` in the CSS below,
         combined with `preserveAspectRatio="none"` on a stretched viewBox, still renders at the
         CSS font size because SVG text metrics come from CSS, not viewBox units). -->
    <text v-for="c in cells" :key="c.label"
          :x="(c.col + 0.5) * cellW" :y="(c.row + 0.5) * cellH"
          class="cc-grid-label" text-anchor="middle" dominant-baseline="central">{{ c.label }}</text>
  </svg>
</template>

<style scoped>
.cc-grid-overlay {
  position: absolute; inset: 0;
  width: 100%; height: 100%;
  pointer-events: none;                        /* never blocks the viewer's own pan/zoom */
  user-select: none;
}
.cc-grid-line {
  stroke: rgba(255, 255, 255, 0.35);
  stroke-width: 1;                             /* non-scaling-stroke keeps this at 1 CSS pixel */
}
.cc-grid-label {
  fill: #fff;
  font: 600 12px system-ui, sans-serif;
  /* dark halo for legibility over any frame, matching StillOverlay's white/dark chrome pattern */
  paint-order: stroke;
  stroke: rgba(0, 0, 0, 0.75);
  stroke-width: 3;
  stroke-linejoin: round;
}
</style>
