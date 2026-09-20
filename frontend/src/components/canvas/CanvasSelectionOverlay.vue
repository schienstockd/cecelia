<!--
  Selection overlay for canvas Share mode. Ubuntu-Screenshot-style: the whole canvas dims, the
  user drags a rectangle across the panels they want, panels the rect covers ≥ 50 % light up,
  everything else stays dimmed. Single-click on a panel toggles it. ESC / a click-outside cancels.
  A floating toolbar confirms "Share (N)" or exits.

  What this component OWNS.
    • The dim veil + the drag-select gesture + the click-toggle.
    • The selection-highlight rings drawn OVER the panels' own borders.
    • The floating toolbar at the top of the canvas.
  What it does NOT own (deliberately).
    • Panel geometry — read from the caller's `panels` prop (which the host derives from
      `useCanvasPanelsStore.getGeom(key)`).
    • The share selection state — a `useCanvasShareSelection()` instance passed in by the host, so
      the store's `beginShare` handler and the toolbar's `share` emit see the same set.
    • The actual composite + POST — the host handles `@share` and does whatever compositing +
      network path applies.

  COORD FRAME. Panels' geoms are in the workspace's own CSS-px (top-left = 0,0), and the overlay
  is mounted inside the same scaled `.sc-zoom` wrapper — so a rect drawn from a pointer event's
  offset lands on the panel bboxes directly. The dim veil is `position: absolute; inset: 0` inside
  the same wrapper, so it grows with the workspace (a wider zoom-out doesn't leave a bare corner).
-->
<script setup lang="ts">
import { onBeforeUnmount, onMounted, ref } from 'vue'
import type { PanelGeom } from '../../stores/canvasPanels'
import type { useCanvasShareSelection } from '../../composables/useCanvasShareSelection'
import { panelAt, selectedByDrag, type PanelHit } from '../../utils/panelSelectionHit'

const props = defineProps<{
  /** All panels currently on the canvas, in draw order (topmost last). */
  panels: PanelHit[]
  /** The share-selection state — owned by the host so it survives the overlay's own lifecycle. */
  selection: ReturnType<typeof useCanvasShareSelection>
  /** Short label rendered in the toolbar — e.g. `"behaviour · plot canvas"` for orientation. */
  addressLine?: string
}>()

const emit = defineEmits<{
  (e: 'cancel'): void
  (e: 'share', payload: { panelIds: number[] }): void
}>()

// ── Pointer state ────────────────────────────────────────────────────────────
// A drag has a start point and a live point; while active, the rect between them is drawn and
// hit-tested against the panels. On release we commit the selection to `props.selection`. A
// single click without significant motion is treated as click-to-toggle rather than a zero-size
// drag (which would deselect everything).
//
// COORD FRAME. Panel geoms come from `useCanvasPanelsStore.geom`, which stores them in the
// workspace's CSS-px frame (offsetParent = `.sc-zoom`). The SVG is mounted inside `.sc-zoom` at
// `inset: 0`, so the SVG's OWN CSS-px box coincides with the workspace box. We measure the SVG
// box on mount + on resize and set `viewBox` to `0 0 boxW boxH` — that is what forces one SVG
// unit == one CSS px in the workspace frame, so a panel at workspace (200, 380) renders at
// exactly (200, 380) inside the overlay. An earlier version used the union-of-panels bbox as
// the viewBox with `preserveAspectRatio="none"`, which stretched the SVG and offset every ring
// away from its panel — visible as rings a bit up-and-left of the panels on real data.
const CLICK_MOTION_PX = 4
const svgRoot = ref<SVGSVGElement | null>(null)
const boxW = ref(0)
const boxH = ref(0)
function measureBox() {
  const r = svgRoot.value?.getBoundingClientRect()
  if (!r) return
  boxW.value = r.width
  boxH.value = r.height
}
onMounted(() => { measureBox(); window.addEventListener('resize', measureBox) })
onBeforeUnmount(() => window.removeEventListener('resize', measureBox))
const dragStart = ref<[number, number] | null>(null)
const dragCur = ref<[number, number] | null>(null)

function svgPoint(ev: PointerEvent): [number, number] {
  const r = svgRoot.value?.getBoundingClientRect()
  if (!r) return [0, 0]
  return [ev.clientX - r.left, ev.clientY - r.top]
}
function dragRect(): PanelGeom | null {
  const a = dragStart.value, b = dragCur.value
  if (!a || !b) return null
  const x = Math.min(a[0], b[0]), y = Math.min(a[1], b[1])
  return { x, y, w: Math.abs(b[0] - a[0]), h: Math.abs(b[1] - a[1]) }
}

function onPointerDown(ev: PointerEvent) {
  if (ev.button !== 0) return
  const p = svgPoint(ev)
  dragStart.value = p
  dragCur.value = p
  svgRoot.value?.setPointerCapture?.(ev.pointerId)
}
function onPointerMove(ev: PointerEvent) {
  if (!dragStart.value) return
  dragCur.value = svgPoint(ev)
}
function onPointerUp(ev: PointerEvent) {
  if (!dragStart.value || !dragCur.value) return
  const p = svgPoint(ev)
  const dx = Math.abs(p[0] - dragStart.value[0])
  const dy = Math.abs(p[1] - dragStart.value[1])
  if (dx < CLICK_MOTION_PX && dy < CLICK_MOTION_PX) {
    // click semantics: toggle the panel under the point
    const id = panelAt(props.panels, p[0], p[1])
    if (id !== null) props.selection.toggle(id)
  } else {
    const rect = dragRect()
    if (rect) {
      const ids = selectedByDrag(props.panels, rect)
      // Replace wholesale — a drag REPLACES the selection (matches GNOME Screenshot). Shift-drag
      // to extend is a nice-to-have we can add later; keeping the gesture simple for v1.
      props.selection.set(ids)
    }
  }
  dragStart.value = null
  dragCur.value = null
  svgRoot.value?.releasePointerCapture?.(ev.pointerId)
}

// ESC cancels; Enter confirms. Global listener so the user doesn't have to click into the overlay
// first.
function onKey(ev: KeyboardEvent) {
  if (ev.key === 'Escape') { emit('cancel'); ev.preventDefault() }
  if (ev.key === 'Enter' && props.selection.count.value > 0) {
    emit('share', { panelIds: [...(props.selection.selected.value ?? [])] })
    ev.preventDefault()
  }
}
onMounted(() => window.addEventListener('keydown', onKey))
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))
</script>

<template>
  <div class="cso-root">
    <!-- Dim veil — same trick GNOME uses. Fills the workspace; opaque enough (~55 %) that dimmed
         panels obviously de-emphasise but not so dark that the plot outlines vanish. -->
    <svg ref="svgRoot" class="cso-svg"
         :viewBox="`0 0 ${boxW || 1} ${boxH || 1}`" preserveAspectRatio="none"
         @pointerdown="onPointerDown" @pointermove="onPointerMove" @pointerup="onPointerUp">
      <!-- Full-canvas veil with a HOLE per selected panel — the mask makes selected panels read
           at their real brightness while everything else dims. -->
      <defs>
        <mask :id="'cso-mask'">
          <rect x="0" y="0" :width="boxW || 1" :height="boxH || 1" fill="white" />
          <rect v-for="p in panels" v-show="selection.has(p.id)" :key="`m${p.id}`"
                :x="p.geom.x" :y="p.geom.y" :width="p.geom.w" :height="p.geom.h" fill="black" />
        </mask>
      </defs>
      <rect x="0" y="0" :width="boxW || 1" :height="boxH || 1"
            class="cso-veil" :mask="`url(#cso-mask)`" />
      <!-- Selection ring per selected panel. Drawn OVER the veil so a selected panel gets a strong
           outline even when the veil is thin. -->
      <template v-for="p in panels" :key="p.id">
        <rect v-if="selection.has(p.id)" class="cso-ring"
              :x="p.geom.x" :y="p.geom.y" :width="p.geom.w" :height="p.geom.h" />
      </template>
      <!-- Live drag rectangle. -->
      <rect v-if="dragStart && dragCur && dragRect()" class="cso-drag"
            :x="dragRect()!.x" :y="dragRect()!.y" :width="dragRect()!.w" :height="dragRect()!.h" />
    </svg>

    <!-- Floating toolbar at the top of the workspace. Fixed to the top-left so it doesn't drift
         with the workspace scroll. -->
    <div class="cso-toolbar cc-row cc-row-tight">
      <span class="cso-title cc-eyebrow cc-fs-2xs">Select plots</span>
      <span v-if="addressLine" class="cso-addr cc-muted cc-fs-2xs">{{ addressLine }}</span>
      <span class="cc-spacer" />
      <span class="cso-count cc-fs-2xs" :class="{ 'cso-count-armed': selection.count.value > 0 }">
        {{ selection.count.value }} selected
      </span>
      <button class="cc-btn cc-btn-ghost cc-btn-sm" @click="emit('cancel')"
              v-tooltip.bottom="'Cancel (Esc)'">Cancel</button>
      <button class="cc-btn cc-btn-primary cc-btn-sm"
              :disabled="selection.count.value === 0"
              @click="emit('share', { panelIds: [...(selection.selected.value ?? [])] })"
              v-tooltip.bottom="selection.count.value > 0
                ? 'Share the selected plots (Enter)'
                : 'Drag or click to select at least one plot'">
        Share ({{ selection.count.value }})
      </button>
    </div>
    <span class="cso-hint cc-fs-2xs">
      <i class="pi pi-info-circle" /> Drag a rectangle across the plots you want — or click one at a time.
    </span>
  </div>
</template>

<style scoped>
.cso-root {
  position: absolute; inset: 0;
  pointer-events: none;   /* the SVG + toolbar re-enable pointer events themselves */
  z-index: 30;            /* over CanvasPanel (10) and CanvasArrangeButtons */
}
.cso-svg {
  position: absolute; inset: 0;
  width: 100%; height: 100%;
  pointer-events: auto;
  touch-action: none;
  cursor: crosshair;
}
.cso-veil {
  fill: rgba(0, 0, 0, 0.55);
  pointer-events: none;
}
.cso-ring {
  fill: none;
  stroke: var(--cc-accent);
  stroke-width: 3;
  vector-effect: non-scaling-stroke;
  pointer-events: none;
}
.cso-drag {
  fill: rgba(255, 255, 255, 0.08);
  stroke: var(--cc-accent);
  stroke-width: 2;
  stroke-dasharray: 4 4;
  vector-effect: non-scaling-stroke;
  pointer-events: none;
}
.cso-toolbar {
  position: absolute; top: 8px; left: 8px; right: 8px;
  padding: 0.35rem 0.5rem;
  background: var(--cc-surface-1);
  border: 1px solid var(--cc-accent);
  border-radius: var(--cc-radius-md);
  pointer-events: auto;
}
.cso-addr { font-family: var(--cc-mono); overflow: hidden; text-overflow: ellipsis;
            white-space: nowrap; max-width: 40ch; }
.cso-count { color: var(--cc-text-dim); font-variant-numeric: tabular-nums; }
.cso-count-armed { color: var(--cc-accent); }
.cso-hint {
  position: absolute; top: 3.4rem; left: 0.75rem;
  padding: 0.2rem 0.5rem;
  background: rgba(0, 0, 0, 0.55); color: var(--cc-text-dim);
  border-radius: var(--cc-radius-md);
  pointer-events: none;
}
</style>
