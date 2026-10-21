<!--
  TileMarkOverlay — Claude's `mark_tile` pointers over the live viewer: an outline around the named
  Set-of-Mark tile ("B3") plus its label. Same coord frame as GridOverlay / LandscapeOverlay
  (viewport-relative percent, `pointer-events: none`), at the grid density on screen, so "B3" here is
  the "B3" `get_landscape` returned and the grid labels. Drawn whether or not the grid is toggled on —
  a pointer must show without the user first finding the grid switch.

  Viewport coords are the grid's contract, not image coords: a pan or zoom after the mark moves what
  the tile covers, exactly as it re-labels the grid. Amber (`--cc-warn`) like Claude's marks on a
  capture (`CaptureViewSurface.vue`). TTL-only; the store prunes it (`viewer.ts::pushTileMark`).
-->
<script setup lang="ts">
import { computed } from 'vue'
import { clampDensity, parseCellLabel, GRID_DENSITY_DEFAULT } from '../utils/gridOverlay'
import type { TileMark } from '../stores/viewer'

const props = defineProps<{
  marks: TileMark[]   // already scoped to the open image by the caller
  cols?: number       // grid density on screen
}>()

const n = computed(() => clampDensity(props.cols ?? GRID_DENSITY_DEFAULT))
const boxes = computed(() => props.marks.flatMap(m => {
  const rc = parseCellLabel(m.cellId, n.value)
  if (!rc) return []
  const w = 100 / n.value
  return [{ id: m.markerId, x: rc.col * w, y: rc.row * w, w, label: m.label || m.cellId }]
}))
</script>

<template>
  <div v-if="boxes.length" class="cc-tile-marks" aria-hidden="true">
    <svg class="cc-tile-marks-svg" viewBox="0 0 100 100" preserveAspectRatio="none">
      <rect v-for="b in boxes" :key="b.id" :x="b.x" :y="b.y" :width="b.w" :height="b.w"
            class="cc-tile-mark" vector-effect="non-scaling-stroke" />
    </svg>
    <!-- HTML labels at percent positions, as GridOverlay does — SVG text distorts in a stretched viewBox -->
    <span v-for="b in boxes" :key="`l-${b.id}`" class="cc-tile-mark-label cc-fs-2xs"
          :style="{ top: `${b.y}%`, left: `${b.x}%` }">{{ b.label }}</span>
  </div>
</template>

<style scoped>
.cc-tile-marks {
  position: absolute; inset: 0;
  pointer-events: none;      /* never blocks the viewer's own pan/zoom */
  user-select: none;
}
.cc-tile-marks-svg {
  position: absolute; inset: 0;
  width: 100%; height: 100%;
}
.cc-tile-mark {
  fill: color-mix(in srgb, var(--cc-warn) 8%, transparent);
  stroke: var(--cc-warn);
  stroke-width: 2;
}
.cc-tile-mark-label {
  position: absolute;
  margin: 2px 0 0 4px;
  color: var(--cc-warn);
  font-weight: 600;
  /* halo as on the grid labels, so the text reads over any frame */
  text-shadow:
    0 0 2px rgba(0, 0, 0, 0.9),
    0 0 3px rgba(0, 0, 0, 0.75);
  white-space: nowrap;
}
</style>
