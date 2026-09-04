<!-- Orientation triad for the 3D viewer — the "which way am I looking" affordance in the top-right
     of the volume canvas. Volume mode only; the plane mode uses the overview minimap in the same
     slot. Camera props are (yaw, pitch) alone: pan and dist do not rotate the volume, so the
     component is oblivious to them and the SVG never redraws on a pan or a wheel-zoom.

     Projection lives in `utils/axesGizmo.ts` — SAME basis as `lib/webgpu/mipShader.ts`'s
     `camera()`, so the arrows can never drift off the rotating volume behind them.

     Layout is two stacked rows — the SVG on top, a separate readout strip below — because a
     rotated axis tip and a corner readout in the same SVG COLLIDE (Dominik 2026-09-03: `+Y` ring
     drew straight through `0.63×`). Two rows also lets the readout use its own font size without
     eating into the triad's radius. -->
<script setup lang="ts">
import { computed } from 'vue'
import { projectAxes, formatZoom } from '../utils/axesGizmo'

const props = withDefaults(defineProps<{
  yaw: number
  pitch: number
  /** Fit-relative zoom multiplier — 1.0 at Reset view, >1 zoomed in. Optional so the same
   *  component works with or without a readout; `undefined` hides the strip entirely. */
  zoom?: number
  size?: number
}>(), { size: 72 })

/** Padding inside the SVG so a tip label at the edge is not clipped. Radius = half-size - pad. */
const PAD = 12

const centre = computed(() => ({ x: props.size / 2, y: props.size / 2 }))
const radius = computed(() => props.size / 2 - PAD)
const tips = computed(() =>
  projectAxes(props.yaw, props.pitch, radius.value, centre.value))

/** Blender/viewer convention — the axis identity, not tied to the app's purple/warn palette
 *  (which is UI chrome, not a coordinate frame). */
const COLOUR: Record<string, string> = {
  X: '#e5484d',
  Y: '#46a758',
  Z: '#4a8fef',
}
function axisOf(key: string) { return key[1] }        // '+X' → 'X'
function signOf(key: string) { return key[0] === '+' }

const viewBox = computed(() => `0 0 ${props.size} ${props.size}`)
const zoomLabel = computed(() =>
  props.zoom === undefined ? null : formatZoom(props.zoom))
</script>

<template>
  <div class="vw-gizmo"
       v-tooltip.bottom="'Orientation triad and zoom — 1× at Reset view'"
       aria-label="Orientation gizmo">
    <div class="vw-gizmo-triad" :style="{ width: size + 'px', height: size + 'px' }">
      <svg :viewBox="viewBox" preserveAspectRatio="none">
        <!-- Two-step draw per tip so painter order is honoured across ALL marks: draw the line
             + the tip together, back tips first, front tips last. -->
        <g v-for="t in tips" :key="t.key">
          <line v-if="signOf(t.key)"
                :x1="centre.x" :y1="centre.y" :x2="t.x" :y2="t.y"
                :stroke="COLOUR[axisOf(t.key)]" stroke-width="1.6" stroke-linecap="round" />
          <!-- Negative tip: hollow ring only; the positive half already drew the segment. -->
          <circle v-if="!signOf(t.key)"
                  :cx="t.x" :cy="t.y" r="2.4"
                  fill="none" :stroke="COLOUR[axisOf(t.key)]" stroke-width="1.2" />
          <template v-if="signOf(t.key)">
            <circle :cx="t.x" :cy="t.y" r="3.4" :fill="COLOUR[axisOf(t.key)]" />
            <text :x="t.x + (t.x - centre.x) * 0.28"
                  :y="t.y + (t.y - centre.y) * 0.28 + 3"
                  class="vw-gizmo-lbl"
                  :fill="COLOUR[axisOf(t.key)]"
                  text-anchor="middle">{{ axisOf(t.key) }}</text>
          </template>
        </g>
      </svg>
    </div>
    <div v-if="zoomLabel" class="vw-gizmo-zoom">{{ zoomLabel }}</div>
  </div>
</template>

<style scoped>
.vw-gizmo {
  position: absolute; top: 0.75rem; right: 0.75rem;
  background: rgba(0, 0, 0, 0.78); border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-xs);
  pointer-events: auto; touch-action: none;
  display: flex; flex-direction: column;
}
.vw-gizmo-triad svg { display: block; width: 100%; height: 100%; }
.vw-gizmo-lbl {
  font: 600 var(--cc-fs-3xs) / 1 system-ui, sans-serif;
  paint-order: stroke;
  stroke: rgba(0, 0, 0, 0.9);
  stroke-width: 2;
  stroke-linejoin: round;
}
/* Readout strip lives BELOW the triad so a rotated axis tip and the number never collide
   (2026-09-03). Its own font size, its own colour; the top border echoes the panel border so the
   two rows read as one panel rather than two stacked chips. */
.vw-gizmo-zoom {
  text-align: center;
  padding: 0.15rem 0.35rem;
  font: 600 var(--cc-fs-2xs) / 1 system-ui, sans-serif;
  color: #fff;
  border-top: 1px solid var(--cc-border);
  font-variant-numeric: tabular-nums;
}
</style>
