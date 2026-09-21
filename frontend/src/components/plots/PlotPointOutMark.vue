<!--
  Amber pulsing ring for Claude's plot point-out (BIDIR PR #4b). One shared marker across every
  plot family so the "Claude pointer" reads uniformly; the family site owns positioning by binding
  `:style="{ left: '…px', top: '…px' }"` (or percentages) — the style falls through onto the root
  span via Vue 3's default attribute inheritance and stacks on top of the base positioning here.

  Same colour family as CVS freeform marks (`--cc-warn`) so a viewer point-out and a plot point-out
  look like the same idiom. `pointer-events: none` on the root so a marker never eats a click on
  the underlying plot (gate drawing, brush selection, panel drag).
-->
<script setup lang="ts">
import type { PlotMark } from '../../stores/viewer'

defineProps<{ mark: PlotMark }>()
</script>

<template>
  <span class="pt-out" v-tooltip.top="mark.label || 'Claude'">
    <span class="pt-out-dot" />
  </span>
</template>

<style scoped>
/* Base positioning is `position: absolute` — callers add `left` / `top` in whatever coords their
   own frame uses (%, px). The negative margins centre the 20px ring on the (u, v) point. z-index 6
   sits above the canvases (dots ~1, gate outlines ~5) but below chrome (10+). */
.pt-out {
  position: absolute; z-index: 6; pointer-events: none;
  width: 20px; height: 20px; margin-left: -10px; margin-top: -10px;
  border-radius: 50%; border: 2px solid var(--cc-warn);
  box-shadow: 0 0 0 1px rgba(0, 0, 0, 0.5), inset 0 0 0 1px rgba(0, 0, 0, 0.5);
  background: rgba(245, 158, 11, 0.15);
  display: flex; align-items: center; justify-content: center;
  animation: pt-out-pulse 1.4s ease-out infinite;
}
.pt-out-dot { width: 4px; height: 4px; border-radius: 50%; background: var(--cc-warn); }
@keyframes pt-out-pulse {
  0%   { transform: scale(1);   opacity: 1; }
  70%  { transform: scale(1.4); opacity: 0.4; }
  100% { transform: scale(1);   opacity: 1; }
}
</style>
