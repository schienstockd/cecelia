<script lang="ts">
// Module scope (NOT `<script setup>`, which re-runs per instance): one stacking order shared by
// every FloatingPanel on screen, so "the panel you last touched is on top" can be decided across
// instances. Ordering logic + the z-index base live in utils/panelStack.ts so they're unit-tested.
import { ref } from 'vue'
import { raisePanel, dropPanel, panelZ } from '../utils/panelStack'

const stack = ref<string[]>([])
</script>

<script setup lang="ts">
// A generic floating, draggable, resizable, collapsible panel that floats above the app content
// (position: fixed). Position/size/collapsed persist per `storageKey` so it reopens where you left it.
// The parent owns visibility (v-if) and handles @close; the panel owns everything else. Reusable —
// not viewer-specific — so any tool that wants a floating box uses this one component.
//
// NOT the same as `composables/useFloatingPanel.ts` — that drives the *canvas* panels (position:
// absolute inside a zoomable offsetParent, mouse-drag with zoom compensation + tile/cascade arrange,
// size owned by CSS `resize`, no persistence). This is a top-level viewport window (position: fixed,
// pointer-drag + resize handle + collapse + localStorage). Different coordinate system, event model,
// and feature set — a deliberate split, not duplication to merge (see INVENTORY.md).
// `ref` comes from the module-scope <script> block above (the two blocks share one scope) — importing
// it again here is a duplicate-identifier error, not a shadow.
import { reactive, computed, onMounted, onUnmounted, watch } from 'vue'
import { panelBounds, clampPanel, maximisedRect } from '../utils/panelBounds'
import { resizeRect, type Edges } from '../utils/panelResize'

const props = withDefaults(defineProps<{
  title: string
  storageKey: string            // localStorage namespace: cc.floating.<storageKey>
  icon?: string                 // optional PrimeIcons class (e.g. 'pi-eye')
  accent?: string               // optional highlight colour applied to the panel border + header icon
  defaultX?: number
  defaultY?: number
  defaultW?: number
  defaultH?: number
}>(), { icon: '', accent: '', defaultX: 240, defaultY: 84, defaultW: 290, defaultH: 460 })

const emit = defineEmits<{ (e: 'close'): void }>()

interface PanelState { x: number; y: number; w: number; h: number; collapsed: boolean; maximised: boolean }
const LSKEY = `cc.floating.${props.storageKey}`
function load(): PanelState {
  try {
    const s = JSON.parse(localStorage.getItem(LSKEY) ?? 'null') as PanelState | null
    // `maximised` post-dates the first saved states, so default it rather than trusting the shape.
    if (s && typeof s.x === 'number') return { ...s, maximised: !!s.maximised }
  } catch { /* corrupt / absent → defaults */ }
  return { x: props.defaultX, y: props.defaultY, w: props.defaultW, h: props.defaultH,
           collapsed: false, maximised: false }
}
const st = reactive(load())
const rootEl = ref<HTMLElement | null>(null)
// For a host that must step aside: roll up to the header when something it points at sits underneath
// it (Kiwi pointing at a plot). The user expands it again with the header chevron, as always.
defineExpose({
  collapse: () => { st.collapsed = true },
  rect: (): DOMRect | null => rootEl.value?.getBoundingClientRect() ?? null,
})
watch(st, () => localStorage.setItem(LSKEY, JSON.stringify(st)), { deep: true })

// The app header is `z-index: 100` and panels stack from 60, so anything under it is unclickable —
// see utils/panelBounds.ts for why the top bound is the header rather than 0. Read from the CSS var so
// this cannot drift from `--cc-header-h`.
function headerHeight(): number {
  const raw = getComputedStyle(document.documentElement).getPropertyValue('--cc-header-h')
  const n = parseFloat(raw)
  return Number.isFinite(n) ? n : 40
}
const bounds = () => panelBounds(window.innerWidth, window.innerHeight, headerHeight())

// keep the panel reachable: clamp its top-left into the usable area (a smaller window / a stale saved
// position could otherwise leave it unreachable with no way to grab it).
function clampIntoView() {
  const { x, y } = clampPanel(st.x, st.y, bounds())
  st.x = x; st.y = y
}

// ── maximise ──
// Declared BEFORE `maxRect`, which reads them: a computed's getter runs at setup, so declaring these
// after it would be a temporal-dead-zone crash rather than a stale value.
const viewportW = ref(window.innerWidth)
const viewportH = ref(window.innerHeight)
function onViewportResize() {
  viewportW.value = window.innerWidth
  viewportH.value = window.innerHeight
  if (!st.maximised) clampIntoView()   // a maximised panel tracks the window via maxRect instead
}
// `st.x/y/w/h` keep the RESTORE geometry while maximised, so the rect is computed rather than written
// — un-maximising cannot lose where the panel was, even across a reload.
const maxRect = computed(() => maximisedRect(viewportW.value, viewportH.value, headerHeight()))
function toggleMaximise() {
  st.maximised = !st.maximised
  if (!st.maximised) clampIntoView()   // restoring must land somewhere legal
}
// ── stacking: the most recently touched panel renders on top ──
// Opening a panel raises it (you just asked for it, so it should be in front), and any pointer
// press inside it raises it again. Closing drops it so it doesn't hold a slot in the ordering.
const z = computed(() => panelZ(stack.value, props.storageKey))
function raise() { stack.value = raisePanel(stack.value, props.storageKey) }

onMounted(() => { clampIntoView(); raise(); window.addEventListener('resize', onViewportResize) })
onUnmounted(() => {
  window.removeEventListener('resize', onViewportResize)
  endGesture()
  stack.value = dropPanel(stack.value, props.storageKey)
})

// ── drag (by header) / resize (any of 8 handles) — one pointer-move loop for both ──
// Resize uses a start-rect + start-pointer snapshot rather than a per-frame offset, so corners/edges
// that anchor to the OPPOSITE side (N/W) can move x/y AND w/h without drift. Maths + tests live in
// utils/panelResize.ts.
let mode: 'drag' | 'resize' | null = null
let dragOffX = 0, dragOffY = 0
let resizeEdges: Edges = {}
let startPointerX = 0, startPointerY = 0
let startRect = { x: 0, y: 0, w: 0, h: 0 }

function onHeaderDown(e: PointerEvent) {
  if ((e.target as HTMLElement).closest('.fp-btn')) return   // header buttons aren't drag handles
  if (st.maximised) return                                   // a maximised window doesn't move
  mode = 'drag'; dragOffX = e.clientX - st.x; dragOffY = e.clientY - st.y; beginGesture(e)
}
function onResizeDown(e: PointerEvent, edges: Edges) {
  if (st.maximised) return
  mode = 'resize'
  resizeEdges = edges
  startPointerX = e.clientX; startPointerY = e.clientY
  startRect = { x: st.x, y: st.y, w: st.w, h: st.h }
  beginGesture(e); e.stopPropagation()
}
function beginGesture(e: PointerEvent) {
  window.addEventListener('pointermove', onMove)
  window.addEventListener('pointerup', endGesture)
  e.preventDefault()
}
function onMove(e: PointerEvent) {
  if (mode === 'drag') {
    // same bounds as clampIntoView — ONE definition, so the drag floor and the mount/resize floor
    // cannot drift apart (they did: both were 0, i.e. both under the app header)
    const { x, y } = clampPanel(e.clientX - dragOffX, e.clientY - dragOffY, bounds())
    st.x = x; st.y = y
  } else if (mode === 'resize') {
    const r = resizeRect(startRect, e.clientX - startPointerX, e.clientY - startPointerY,
      resizeEdges,
      { minW: 220, minH: 140, viewportW: window.innerWidth, viewportH: window.innerHeight, bounds: bounds() })
    st.x = r.x; st.y = r.y; st.w = r.w; st.h = r.h
  }
}
function endGesture() {
  mode = null
  window.removeEventListener('pointermove', onMove)
  window.removeEventListener('pointerup', endGesture)
}
</script>

<template>
  <!-- .capture: the resize grip stops propagation on pointerdown, so a bubble-phase handler here
       would miss a resize gesture. Capture runs on the way down, before any child handler. -->
  <div ref="rootEl" class="fp" :class="{ 'fp-max': st.maximised }" @pointerdown.capture="raise"
       :style="{ left: (st.maximised ? maxRect.x : st.x) + 'px',
                 top: (st.maximised ? maxRect.y : st.y) + 'px',
                 width: (st.maximised ? maxRect.w : st.w) + 'px',
                 height: st.collapsed ? 'auto' : (st.maximised ? maxRect.h : st.h) + 'px', zIndex: z,
                 ...(accent ? { borderColor: accent } : {}) }">
    <!-- double-click to maximise/restore, the usual window gesture; the buttons stay the discoverable
         route, since a double-click affordance is invisible -->
    <div class="fp-header" @pointerdown="onHeaderDown" @dblclick="toggleMaximise">
      <i v-if="icon" :class="['pi', icon, 'fp-icon']" :style="accent ? { color: accent } : undefined" />
      <span class="fp-title">{{ title }}</span>
      <!-- Panel-specific header actions (e.g. an overview `?`) sit before the window glyphs so the
           collapse/maximise/close cluster stays in the same spot across every panel. -->
      <slot name="header-actions" />
      <!-- collapse rolls the panel up to its header; it is NOT minimise, so it takes the chevron and
           leaves the window glyphs to the real window control beside it -->
      <button class="fp-btn cc-btn cc-btn-bare cc-btn-icon" @click="st.collapsed = !st.collapsed"
              v-tooltip.bottom="st.collapsed ? 'Expand' : 'Collapse to header'">
        <i :class="['pi', st.collapsed ? 'pi-chevron-down' : 'pi-chevron-up']" />
      </button>
      <button class="fp-btn cc-btn cc-btn-bare cc-btn-icon" @click="toggleMaximise"
              v-tooltip.bottom="st.maximised ? 'Restore' : 'Maximise'">
        <i :class="['pi', st.maximised ? 'pi-window-minimize' : 'pi-window-maximize']" />
      </button>
      <button class="fp-btn cc-btn cc-btn-bare cc-btn-icon" @click="emit('close')" v-tooltip.bottom="'Close'">
        <i class="pi pi-times" />
      </button>
    </div>
    <div v-show="!st.collapsed" class="fp-body"><slot /></div>
    <!-- Eight resize handles: four thin edges + four corner squares (the corners sit on top so their
         diagonal cursor wins where they overlap the edges). No tooltip — a desktop window doesn't
         label its own frame, and a tooltip on every edge would flicker as the pointer crosses them. -->
    <template v-if="!st.collapsed && !st.maximised">
      <div class="fp-edge fp-edge-n" @pointerdown="e => onResizeDown(e, { n: true })" />
      <div class="fp-edge fp-edge-s" @pointerdown="e => onResizeDown(e, { s: true })" />
      <div class="fp-edge fp-edge-e" @pointerdown="e => onResizeDown(e, { e: true })" />
      <div class="fp-edge fp-edge-w" @pointerdown="e => onResizeDown(e, { w: true })" />
      <div class="fp-corner fp-corner-nw" @pointerdown="e => onResizeDown(e, { n: true, w: true })" />
      <div class="fp-corner fp-corner-ne" @pointerdown="e => onResizeDown(e, { n: true, e: true })" />
      <div class="fp-corner fp-corner-sw" @pointerdown="e => onResizeDown(e, { s: true, w: true })" />
      <div class="fp-corner fp-corner-se" @pointerdown="e => onResizeDown(e, { s: true, e: true })" />
    </template>
  </div>
</template>

<style scoped>
.fp {
  position: fixed;
  /* z-index is bound inline (see PANEL_Z_BASE in utils/panelStack.ts) — panels are stacked by
     most-recently-touched, so it can't be a flat value here. */
  display: flex;
  flex-direction: column;
  background: var(--cc-surface-1);          /* solid — floats over content, must not be see-through */
  border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-lg);
  box-shadow: 0 8px 28px rgba(0, 0, 0, 0.45);
  overflow: hidden;
  min-width: 220px;
}
/* maximised: flush to the viewport edges, so the rounding + drop shadow would only show as artefacts */
.fp-max { border-radius: 0; box-shadow: none; }
.fp-header {
  display: flex;
  align-items: center;
  gap: 0.4rem;
  padding: 0.3rem 0.4rem 0.3rem 0.6rem;
  background: var(--cc-surface-2);
  border-bottom: 1px solid var(--cc-border);
  cursor: move;
  user-select: none;
  flex-shrink: 0;
}
/* a maximised window doesn't move, so the header must not advertise a drag it will ignore */
.fp-max .fp-header { cursor: default; }
.fp-icon { font-size: var(--cc-fs-md); color: var(--cc-accent); flex-shrink: 0; }
.fp-title {
  flex: 1;
  font-size: var(--cc-fs-sm);
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  color: var(--cc-text);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}
/* .fp-btn → cc-btn cc-btn-bare cc-btn-icon */
.fp-btn:hover { color: var(--cc-text); background: var(--cc-surface-2); }
.fp-body { flex: 1; overflow: auto; min-height: 0; }
/* ── resize frame ─────────────────────────────────────────────────────────────
   Eight invisible hit regions around the edge of the panel, laid out like a
   desktop window frame. Edges are a thin strip along each side; corners are
   small squares layered ABOVE the edges (larger z-index) so the diagonal
   cursor wins in the overlap. Widths chosen wide enough to grab without
   pixel-perfect aim but small enough not to eat clicks near the border. */
/* The panel has overflow: hidden (rounded-corner mask), so handles must sit INSIDE the frame — a
   negative offset would be clipped and un-grabbable. A ~5px inset gives enough grab area without
   eating clicks near the border. */
.fp-edge, .fp-corner { position: absolute; z-index: 1; }
.fp-edge-n { top: 0; left: 10px; right: 10px; height: 5px; cursor: ns-resize; }
.fp-edge-s { bottom: 0; left: 10px; right: 10px; height: 5px; cursor: ns-resize; }
.fp-edge-e { top: 10px; bottom: 10px; right: 0; width: 5px; cursor: ew-resize; }
.fp-edge-w { top: 10px; bottom: 10px; left: 0; width: 5px; cursor: ew-resize; }
.fp-corner { width: 14px; height: 14px; z-index: 2; }
.fp-corner-nw { top: 0; left: 0; cursor: nwse-resize; }
.fp-corner-se { bottom: 0; right: 0; cursor: nwse-resize; }
.fp-corner-ne { top: 0; right: 0; cursor: nesw-resize; }
.fp-corner-sw { bottom: 0; left: 0; cursor: nesw-resize; }
/* Keep the visible grip lines in the SE corner — the one users already know as "the resize corner".
   The other seven handles are invisible, discovered by the cursor change like any desktop window. */
.fp-corner-se {
  background:
    linear-gradient(135deg, transparent 0 6px, var(--cc-border) 6px 7px, transparent 7px 9px,
                    var(--cc-border) 9px 10px, transparent 10px);
}
</style>
