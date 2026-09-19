<!--
  Set-of-Mark grid drawn over the WebGPU viewer (`modules/ViewerWindow.vue`). A N×N mesh with
  spreadsheet-style cell labels (A1..H8 at the default 8×8) so a share-in / point-out session can
  cite a region by NAME instead of pixel coords. PR #2 of `docs/todo/BIDIR_CONTEXT_PLAN.md`;
  toggled from the Annotations section of ViewerWindow's viewer panel.

  In VIEWPORT coords, not image coords: at zoom-in "C4" means a quarter of the currently visible
  view, which is what the SoM use case wants. When the user pans/zooms, the grid stays put over
  the canvas rect and re-labels what they now see. Pure decoration — `pointer-events: none`, no
  click handling, doesn't intercept the viewer's own pan/zoom.

  Two layers because SVG text sizing and non-uniform viewBox don't mix: lines are an SVG with
  `viewBox="0 0 100 100"` + `preserveAspectRatio="none"` (percentage-of-viewport, stretches to
  whatever CSS size the canvas settles at); labels are absolutely-positioned HTML at fractional
  `top`/`left` so the font stays at a real CSS px size regardless of viewport aspect or density.
  A single SVG can't do both — `font-size` in SVG is in user units, so a stretched 100×100 viewBox
  renders 12-unit text as many CSS pixels; a non-square viewport also distorts it.

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
  <div class="cc-grid-overlay" aria-hidden="true">
    <svg class="cc-grid-lines"
         :viewBox="`0 0 ${vbW} ${vbH}`" preserveAspectRatio="none">
      <!-- interior column / row dividers: N-1 lines at fractional positions; outer edges skipped
           so the border doesn't double against the canvas frame. `non-scaling-stroke` pins the
           line at 1 CSS pixel regardless of viewport size. -->
      <line v-for="i in nCols - 1" :key="`v${i}`" :x1="i * cellW" :y1="0" :x2="i * cellW" :y2="vbH"
            class="cc-grid-line" vector-effect="non-scaling-stroke" />
      <line v-for="i in nRows - 1" :key="`h${i}`" :x1="0" :y1="i * cellH" :x2="vbW" :y2="i * cellH"
            class="cc-grid-line" vector-effect="non-scaling-stroke" />
    </svg>
    <span v-for="c in cells" :key="c.label" class="cc-grid-label"
          :style="{ top: `${(c.row + 0.5) * cellH}%`, left: `${(c.col + 0.5) * cellW}%` }">
      {{ c.label }}
    </span>
  </div>
</template>

<style scoped>
.cc-grid-overlay {
  position: absolute; inset: 0;
  pointer-events: none;                        /* never blocks the viewer's own pan/zoom */
  user-select: none;
}
.cc-grid-lines {
  position: absolute; inset: 0;
  width: 100%; height: 100%;
}
.cc-grid-line {
  stroke: rgba(255, 255, 255, 0.35);
  stroke-width: 1;                             /* non-scaling-stroke keeps this at 1 CSS pixel */
}
.cc-grid-label {
  position: absolute;
  transform: translate(-50%, -50%);
  color: #fff;
  font: 600 11px system-ui, sans-serif;
  /* dark halo for legibility over any frame, matching StillOverlay's white/dark chrome pattern */
  text-shadow:
    0 0 2px rgba(0, 0, 0, 0.9),
    0 0 3px rgba(0, 0, 0, 0.75);
  white-space: nowrap;
}
</style>
