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

// ── drag (by header) / resize (bottom-right handle) — one pointer-move loop for both ──
let mode: 'drag' | 'resize' | null = null
let offX = 0, offY = 0
function onHeaderDown(e: PointerEvent) {
  if ((e.target as HTMLElement).closest('.fp-btn')) return   // header buttons aren't drag handles
  if (st.maximised) return                                   // a maximised window doesn't move
  mode = 'drag'; offX = e.clientX - st.x; offY = e.clientY - st.y; beginGesture(e)
}
function onResizeDown(e: PointerEvent) {
  if (st.maximised) return
  mode = 'resize'; offX = e.clientX - st.w; offY = e.clientY - st.h; beginGesture(e); e.stopPropagation()
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
    const { x, y } = clampPanel(e.clientX - offX, e.clientY - offY, bounds())
    st.x = x; st.y = y
  } else if (mode === 'resize') {
    st.w = Math.max(220, Math.min(e.clientX - offX, window.innerWidth - st.x))
    st.h = Math.max(140, Math.min(e.clientY - offY, window.innerHeight - st.y))
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
  <div class="fp" :class="{ 'fp-max': st.maximised }" @pointerdown.capture="raise"
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
    <div v-show="!st.collapsed && !st.maximised" class="fp-resize" @pointerdown="onResizeDown"
         v-tooltip.left="'Drag to resize'" />
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
.fp-resize {
  position: absolute;
  right: 0;
  bottom: 0;
  width: 14px;
  height: 14px;
  cursor: nwse-resize;
  /* corner grip lines */
  background:
    linear-gradient(135deg, transparent 0 6px, var(--cc-border) 6px 7px, transparent 7px 9px,
                    var(--cc-border) 9px 10px, transparent 10px);
}
</style>
