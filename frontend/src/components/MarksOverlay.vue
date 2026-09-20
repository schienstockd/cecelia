<!--
  Read-only marks overlay on the pop-out viewer. Paints a capture's annotations (rects / polys /
  strokes) over the live WebGPU canvas so the user can look at Claude's marks in-place while the
  live pixels underneath keep responding to pan / zoom / t / z.

  Peer of `StillOverlay` / `GridOverlay` / `DrawSurface` in `ViewerWindow.vue`. `pointer-events: none`
  so the viewer's own gestures aren't intercepted. Not interactive — the source of truth is the
  capture on disk; a user who wants to edit clicks Refine on the blackboard entry, not on this.

  Coord convention. Marks are in [0,1] FRAME-relative coords (the same normalisation the capture
  stored — see `captureAddress.ts`). The overlay is an SVG with `viewBox="0 0 1 1"` +
  `preserveAspectRatio="none"` so those 0..1 coords map directly to the canvas rect. The catch:
  if the user has since panned / zoomed the live view, the frame the capture was drawn on and the
  frame currently on screen don't coincide, so marks will read as "in the general area" rather
  than pixel-accurate. That's OK for this first pass: the dismissible chip in the viewer header
  tells the user THESE marks belong to a specific captured frame, and the chip auto-drops when
  the user seeks t / z away — nothing pretends to track pan / zoom.

  Colour palette: same `ANNOTATION_PALETTE` DrawSurface + share compositing use, so a capture
  drawn in cyan reads as cyan here too.
-->
<script setup lang="ts">
import { computed } from 'vue'
import type { OverlayMark } from '../utils/captureAddress'
import { resolveMarkColor } from '../utils/overlayCompose'

const props = defineProps<{ marks: OverlayMark[] }>()

interface RectShape   { key: string; kind: 'rect'; color: string; x: number; y: number; w: number; h: number; label?: string; anchor: [number, number] }
interface PathShape   { key: string; kind: 'poly' | 'stroke'; color: string; d: string; label?: string; anchor: [number, number] }
type Shape = RectShape | PathShape

/** Turn the raw marks into ready-to-render shape records. Guards against a missing `pts` /
 *  unknown kind — a bad mark is dropped, the rest still paint. */
const shapes = computed<Shape[]>(() => {
  const out: Shape[] = []
  for (let i = 0; i < props.marks.length; i++) {
    const m = props.marks[i]
    if (!m || typeof m !== 'object') continue
    const g = m.geom as Record<string, number> & { pts?: [number, number][] }
    const color = resolveMarkColor(m)
    const key = `mk-${i}`
    if (m.kind === 'rect') {
      const x = Number(g?.x), y = Number(g?.y), w = Number(g?.w), h = Number(g?.h)
      if (![x, y, w, h].every(Number.isFinite)) continue
      out.push({ key, kind: 'rect', color, x, y, w, h,
                 label: m.label, anchor: [x, y] })
    } else if (m.kind === 'poly' || m.kind === 'stroke') {
      const pts = Array.isArray(g?.pts) ? g.pts : []
      if (pts.length < 2) continue
      const cmd = pts.map(([px, py], j) => `${j === 0 ? 'M' : 'L'}${px} ${py}`).join(' ')
      const d = m.kind === 'poly' ? `${cmd} Z` : cmd
      out.push({ key, kind: m.kind, color, d,
                 label: m.label, anchor: pts[0] })
    }
  }
  return out
})
</script>

<template>
  <svg class="cc-marks-overlay" viewBox="0 0 1 1" preserveAspectRatio="none" aria-hidden="true">
    <template v-for="s in shapes" :key="s.key">
      <!-- Rects and paths both get `vector-effect: non-scaling-stroke` so the stroke is 1.5 CSS-px
           regardless of the SVG viewBox scale (otherwise a stroke in 0..1 coords is invisible). -->
      <rect v-if="s.kind === 'rect'"
            :x="(s as RectShape).x" :y="(s as RectShape).y"
            :width="(s as RectShape).w" :height="(s as RectShape).h"
            :stroke="s.color" fill="none" stroke-width="1.5"
            vector-effect="non-scaling-stroke" />
      <path v-else :d="(s as PathShape).d"
            :stroke="s.color" fill="none" stroke-width="1.5"
            stroke-linejoin="round" stroke-linecap="round"
            vector-effect="non-scaling-stroke" />
    </template>
    <!-- Labels as HTML (foreignObject would inherit the stretched viewBox); we position them via
         percentages instead. Rendered in a sibling absolute-positioned layer below. -->
  </svg>
  <div class="cc-marks-labels" aria-hidden="true">
    <span v-for="s in shapes.filter(x => !!x.label)" :key="s.key + '-lbl'"
          class="cc-mark-label"
          :style="{ left: (s.anchor[0] * 100) + '%', top: (s.anchor[1] * 100) + '%', color: s.color }">
      {{ s.label }}
    </span>
  </div>
</template>

<style scoped>
.cc-marks-overlay {
  position: absolute; inset: 0;
  width: 100%; height: 100%;
  pointer-events: none;
}
.cc-marks-labels {
  position: absolute; inset: 0;
  pointer-events: none;
  user-select: none;
}
.cc-mark-label {
  position: absolute;
  transform: translate(0, -0.4rem);
  font: 600 11px ui-monospace, SFMono-Regular, Menlo, monospace;
  /* Dark halo so a label reads on any fluorescence background — matches StillOverlay's chrome. */
  text-shadow:
    0 0 2px rgba(0, 0, 0, 0.9),
    0 0 3px rgba(0, 0, 0, 0.75);
  white-space: nowrap;
}
</style>
