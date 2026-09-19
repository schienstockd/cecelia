<!--
  FreeformOverlay — paints Claude's `mark_freeform` marks on the viewer, closing the visual gap
  left by BIDIR PR #5 (`mark_freeform` had a store dispatch but no renderer). Mounts as a peer of
  DrawSurface / GridOverlay / StillOverlay inside modules/ViewerWindow.vue.

  DIVISION OF LABOUR.
    • `utils/freeformRender.ts` — pure coord resolution + per-kind shape helpers (unit-tested).
    • `stores/viewer.ts::freeformMarks` — the ephemeral bag (session-only; TTL-pruned by setTimeout).
    • `stores/ws.ts::dispatch()` — reads the `viewer:mark` (kind: "freeform") frame, calls
      `pushFreeformMark`.
    • This SFC — SVG shapes + per-mark ✕ dismiss.

  COORD MODES (per Decision 17 of `docs/todo/BIDIR_CONTEXT_PLAN.md`).
    • target = "live_viewer"   → geom is viewport-px (draw as-is in the SVG box).
    • target = "cap-…"         → geom is 0..1 frame-relative (scale by the SVG box).
      A captureId mark is a best-effort pointer AT what the user shared — the frame the mark
      was authored against may not match what's currently rendered. TTL (5 min default) is the
      thing that keeps a stale mark from lingering; a strict image-match gate would need a
      round-trip to the capture meta, and Claude typically only sends captureId marks in direct
      response to a share, so the "wrong image" window is small.

  Why SVG and not another canvas: the shapes are cheap, and we want per-mark ✕ hit-testing +
  label chip DOM (styled by tokens) rather than reinventing pointer hit tests in canvas space.
  Same reasoning as PointerBubble / GridOverlay.
-->
<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { useViewerStore } from '../stores/viewer'
import {
  paintableFor, coordMode, markMatchesViewer, pointsToSvgAttr,
  type Rect, type Circle, type Arrow,
} from '../utils/freeformRender'

const viewer = useViewerStore()

// SVG viewBox tracks its own client CSS px so pointer coords match viewport px 1:1 for
// live_viewer marks, and the 0..1 → box scale for captureId marks is against what the user sees.
const svg  = ref<SVGSVGElement | null>(null)
const boxW = ref(0)
const boxH = ref(0)
function measureBox() {
  const r = svg.value?.getBoundingClientRect()
  if (!r) return
  boxW.value = r.width; boxH.value = r.height
}
// Measure on mount + window resize + when the first mark shows up (SVG only exists in the DOM
// while marks are present via `v-if`, so we can't measure before then). No ResizeObserver: the
// ratchet in `utils/continuousControls.test.ts` mandates any RO go through `usePlotResize`, which
// is for rAF-coalesced RE-RENDERS; we only need to recompute a coord frame, and a pop-out window
// `resize` is the event that actually matters. Same tradeoff DrawSurface makes.
onMounted(() => { window.addEventListener('resize', measureBox); measureBox() })
onBeforeUnmount(() => { window.removeEventListener('resize', measureBox) })

// Resolve one mark into an array of paintables, keyed for :key stability. Filters by imageUid
// scope (live_viewer marks may carry one; captureId marks don't).
interface ResolvedMark {
  markerId: string
  label: string
  paintables: ReturnType<typeof paintableFor>
}
const currentImageUid = computed(() => viewer.openImage?.imageUid ?? '')
const resolved = computed<ResolvedMark[]>(() => {
  const w = boxW.value, h = boxH.value
  if (w <= 0 || h <= 0) return []
  const out: ResolvedMark[] = []
  for (const m of viewer.freeformMarks) {
    if (!markMatchesViewer(m, currentImageUid.value)) continue
    const mode = coordMode(m.target)
    const paintables = paintableFor(m.overlay, w, h, mode)
    if (!paintables.length) continue
    out.push({ markerId: m.markerId, label: m.label, paintables })
  }
  return out
})

// The SVG stays mounted (so `svg` ref is always live and measurements work); the marks-group
// inside is `v-if`. When a mark arrives on an unmeasured overlay (mount race with the canvas
// wrap), re-measure on the next tick so paintables resolve on that same frame.
watch(() => viewer.freeformMarks.length, async (n) => {
  if (!n) return
  await nextTick()
  if (boxW.value === 0 || boxH.value === 0) measureBox()
})

function dismiss(markerId: string) { viewer.dismissFreeformMark(markerId) }

// Narrow one paintable's shape into the type its template branch needs. Vue templates don't do
// discriminated-union narrowing, so keep the casts here rather than sprinkling them in the SVG.
function asRect(s: unknown):   Rect   { return s as Rect }
function asCircle(s: unknown): Circle { return s as Circle }
function asArrow(s: unknown):  Arrow  { return s as Arrow }
function asPoints(s: unknown): Array<[number, number]> {
  return (s as { pts: Array<[number, number]> }).pts
}
</script>

<template>
  <!-- Sits directly on the canvas as a peer of GridOverlay / DrawSurface — absolute inside
       `.vw-canvas-wrap`. Layer itself is transparent to clicks so it doesn't block canvas
       interaction; the ✕ button opts back in via pointer-events: auto.
       SVG stays mounted (v-show) so its ref + bounding box are always live; the mark group
       is v-if so an empty overlay costs nothing. -->
  <svg ref="svg" class="ff-svg" v-show="resolved.length"
       :viewBox="`0 0 ${boxW || 1} ${boxH || 1}`" preserveAspectRatio="none">
    <g v-for="m in resolved" :key="m.markerId" class="ff-mark">
      <template v-for="(p, i) in m.paintables" :key="i">
        <rect v-if="p.kind === 'rect'"
              :x="asRect(p.shape).x" :y="asRect(p.shape).y"
              :width="asRect(p.shape).w" :height="asRect(p.shape).h"
              class="ff-shape" />
        <circle v-else-if="p.kind === 'circle'"
                :cx="asCircle(p.shape).cx" :cy="asCircle(p.shape).cy" :r="asCircle(p.shape).r"
                class="ff-shape" />
        <line v-else-if="p.kind === 'arrow'"
              :x1="asArrow(p.shape).x1" :y1="asArrow(p.shape).y1"
              :x2="asArrow(p.shape).x2" :y2="asArrow(p.shape).y2"
              class="ff-shape ff-arrow" marker-end="url(#ff-arrowhead)" />
        <polygon v-else-if="p.kind === 'poly'"
                 :points="pointsToSvgAttr(asPoints(p.shape))" class="ff-shape ff-poly" />
        <polyline v-else-if="p.kind === 'stroke'"
                  :points="pointsToSvgAttr(asPoints(p.shape))" class="ff-shape ff-stroke" />
      </template>
      <!-- Label chip + ✕ at the FIRST paintable's anchor — one chip per mark, not per shape. -->
      <foreignObject :x="m.paintables[0].anchor.x" :y="m.paintables[0].anchor.y - 20"
                     width="240" height="24">
        <div class="ff-chip cc-fs-2xs">
          <span v-if="m.label" class="ff-label">{{ m.label }}</span>
          <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro ff-x"
                  @click="dismiss(m.markerId)" v-tooltip.top="'Dismiss'"
                  aria-label="Dismiss pointer"><i class="pi pi-times" /></button>
        </div>
      </foreignObject>
    </g>
    <!-- Arrowhead marker defined once, referenced by every arrow shape. -->
    <defs>
      <marker id="ff-arrowhead" viewBox="0 0 10 10" refX="8" refY="5"
              markerWidth="6" markerHeight="6" orient="auto-start-reverse">
        <path d="M0,0 L10,5 L0,10 z" fill="var(--cc-warn)" />
      </marker>
    </defs>
  </svg>
</template>

<style scoped>
.ff-svg {
  position: absolute; inset: 0;
  pointer-events: none;   /* layer transparent to canvas clicks; ✕ button opts back in below */
  z-index: 30;            /* above canvas + StillOverlay/GridOverlay, below DrawSurface toolbar */
}
.ff-shape {
  fill: none;
  stroke: var(--cc-warn);
  stroke-width: 2px;
  vector-effect: non-scaling-stroke;   /* line width stays 2 CSS-px regardless of viewBox scaling */
}
.ff-poly   { fill: rgba(245, 158, 11, 0.08); }   /* faint amber fill so a closed shape reads */
.ff-stroke { fill: none; stroke-linejoin: round; stroke-linecap: round; }
.ff-arrow  { stroke-linecap: round; }

.ff-chip {
  display: inline-flex; align-items: center; gap: 0.25rem;
  padding: 0.1rem 0.35rem 0.1rem 0.5rem;
  background: var(--cc-surface-1);
  border: 1px solid var(--cc-warn);
  border-radius: var(--cc-radius-pill);
  color: var(--cc-text);
  pointer-events: auto;   /* opt back in so ✕ receives the click */
  box-shadow: 0 0 0 2px rgba(0, 0, 0, 0.35);
  white-space: nowrap;
  max-width: 100%; overflow: hidden; text-overflow: ellipsis;
}
.ff-label { overflow: hidden; text-overflow: ellipsis; }
</style>
