<!--
  Generic free-floating canvas panel: the chrome (draggable title bar, resize, active border,
  Tile/Cascade arrange, remove) with no knowledge of what's inside. Plot panels wrap their content
  here: the gating scatter (GatePlotPanel) today; the summary (Vega-Lite) and track-gating panels
  next. Header tools go in the `actions` slot, the plot body in the default slot.

  Drag/clamp/arrange come from useFloatingPanel so every floating panel behaves identically.
-->
<script setup lang="ts">
import { ref, computed, onMounted, onBeforeUnmount, onUpdated, nextTick, useTemplateRef, watch, useSlots } from 'vue'
import { useFloatingPanel, type ArrangeCmd } from '../../composables/useFloatingPanel'
import { useResizeHandles } from '../../composables/useResizeHandles'
import { useCanvasPanelsStore } from '../../stores/canvasPanels'
import { useInjectedZoom } from '../../composables/useCanvasZoom'
import { rafCoalesce } from '../../utils/rafCoalesce'
import { fitSquare } from '../../utils/tileGrid'
import CcCycleButton, { type CycleOption } from '../CcCycleButton.vue'
import AddToKiwiButton from '../kiwi/AddToKiwiButton.vue'
import { usePlotRegistryStore } from '../../stores/plotRegistry'

const props = withDefaults(defineProps<{
  index: number
  title: string
  active?: boolean
  removable?: boolean
  // window-arrangement command (Tile/Cascade); seq bumps to force re-apply
  arrange?: ArrangeCmd | null
  // when set (`${canvasKey}:${panelId}`), drag position + size persist here across navigation
  persistKey?: string
  // DOCKED: fill the parent slot (Analysis-canvas grid layout) instead of free-floating — no drag,
  // no resize, no geometry persistence. Keeps the header/actions/body/footer chrome.
  docked?: boolean
  // AUTO-HIDE: give the plot the whole box and overlay the control surfaces (actions + footer),
  // revealing them only on hover (or when pinned). Default ON — every summary/interactive plot wants
  // its plot un-squashed (and the PDF captures the full-box plot, not a control-squashed strip). The
  // gate-DRAWING panels opt OUT (`:auto-hide="false"`) because you interact with the plot constantly
  // there and popping controls over the canvas would fight the gate tools. Interactive views whose
  // toolbar lives INSIDE the body opt in by tagging it `.cc-panel-controls` (see style.css / docs/UI.md).
  autoHide?: boolean
  // coord-fixed plots (gate scatter, UMAP) want a 1:1 box so the square plot fills it with no blank
  // space — snap the free-floating panel's height to its width on resize. No-op when docked (the board
  // grid owns slot size) or collapsed.
  square?: boolean
}>(), { active: false, removable: true, arrange: null, docked: false, autoHide: true, square: false })
const emit = defineEmits<{ activate: [number]; remove: [] }>()

const collapsed = ref(false)
// 3-state chrome-visibility toggle for the auto-hide controls (local like `collapsed` — a
// transient chrome preference, not a persisted plot option). Only meaningful when autoHide is on
// and there are controls to reveal.
//   auto    — default; controls hidden, revealed on panel hover
//   visible — pinned open; controls always shown
//   hidden  — controls never shown, hover doesn't reveal them either (screenshot mode)
// Rendered as a `CcCycleButton` — a generic N-state icon-button primitive; the two extremes
// suppress the hover reveal via distinct classes on the root (see style.css →
// `.controls-pinned` / `.controls-hidden`).
const chromeMode = ref<'auto' | 'visible' | 'hidden'>('auto')
const CHROME_OPTIONS: CycleOption[] = [
  { value: 'auto',    icon: 'pi pi-thumbtack', tip: 'Controls: auto (hover to show)' },
  { value: 'visible', icon: 'pi pi-thumbtack', tip: 'Controls: always visible', on: true },
  { value: 'hidden',  icon: 'pi pi-eye-slash', tip: 'Controls: always hidden' },
]
const slots = useSlots()
// A view whose toolbar lives INSIDE the body tags it `.cc-panel-controls` (docs/UI.md) and gets the
// auto-hide behaviour from the stylesheet — but the PIN was keyed only on the slots, so those views
// auto-hid their controls with no way to pin them open. Reported as "why do these floating plots not
// have the pin". Keying the pin on the same class the CSS keys on means the two cannot disagree.
//
// Detected from the DOM rather than declared, because the class is what the stylesheet acts on; a
// parallel `hasControls` prop would be a second source of truth to forget. Re-checked on update only
// until it is found, since a view may render its toolbar after its first data arrives.
const bodyEl = useTemplateRef<HTMLElement>('bodyEl')
const bodyControls = ref(false)
const checkBodyControls = () => {
  if (!bodyControls.value && bodyEl.value?.querySelector('.cc-panel-controls')) bodyControls.value = true
}
onMounted(() => nextTick(checkBodyControls))
onUpdated(checkBodyControls)
// A FUNCTION, not a computed, and that distinction is the whole bug: `slots` is not reactive, so a
// computed caches whatever it saw on the FIRST render — and on that render an InteractivePanel has not
// mounted its view yet, so `exportFormats` is empty, so the `#footer` template is not provided. The
// panel then decided "no controls" forever and never showed the pin, even once the Export footer
// appeared and started overlaying the body. Reported on the correction panel, whose only chrome is
// that footer. Called from the template, it is re-evaluated whenever the panel re-renders — which is
// exactly when its slots change.
const hasControls = () => !!slots.actions || !!slots.footer || bodyControls.value
const root = useTemplateRef<HTMLElement>('root')
const mainEl = useTemplateRef<HTMLElement>('mainEl')   // .panel-main — the plot region kept square by :square
const store = useCanvasPanelsStore()
// "Add to Kiwi" only on a panel that is a LIVE registered plot — its `persistKey` is then the plotId
// `list_plots` and the Kiwi resolver know (stores/plotRegistry.ts). Unregistered panels get no button,
// because a plot ref to them would fail to resolve.
const plotRegistry = usePlotRegistryStore()
const isLivePlot = computed(() => !!props.persistKey && !!plotRegistry.getLast(props.persistKey))
const saved = props.persistKey ? store.getGeom(props.persistKey) : undefined
// the host canvas may apply a visual zoom (transform:scale); inject it so drag deltas are zoom-correct
const injectedZoom = useInjectedZoom()
const { pos, startDrag } = useFloatingPanel(root, {
  // restore the saved position, else stagger by index
  initial: saved ? { x: saved.x, y: saved.y } : { x: 16 + props.index * 30, y: 16 + props.index * 30 },
  onActivate: () => emit('activate', props.index),
  arrange: () => fitted(props.arrange),
  zoom: injectedZoom,
})
// The 8-handle resize gesture — same primitive as the top-level `FloatingPanel`. Bounds sit inside
// the canvas offsetParent (0,0 → parent width/height) so a resize cannot push the panel outside its
// workspace; `zoom` matches the drag path so screen-pixel deltas are canvas-pixel-correct.
// `setRect` writes SIZE straight to the DOM style (so the existing `ResizeObserver` continues to fire
// `persist()` + `enforceSquare`) and updates `pos` for the anchor edges (W/N). It respects the
// `edges` mask: for a `:square` panel a horizontal-only drag must NOT rewrite `height`, otherwise
// the composable would overwrite `enforceSquare`'s snap every pointermove and flicker.
const { onResizeDown } = useResizeHandles({
  getRect: () => ({ x: pos.value.x, y: pos.value.y, w: root.value?.offsetWidth ?? 0, h: root.value?.offsetHeight ?? 0 }),
  setRect: (r, edges) => {
    if (edges.w || edges.n) pos.value = { x: r.x, y: r.y }   // N/W anchor to the opposite side, so position moves too
    if (!root.value) return
    if (edges.e || edges.w) root.value.style.width = r.w + 'px'
    if (edges.n || edges.s) root.value.style.height = r.h + 'px'
  },
  bounds: () => {
    // resize floor inside the canvas — matches the drag `clamp` in `useFloatingPanel` (top-left ≥ 0
    // for the y axis; negative x is allowed for the drag path but a resize can't drag the top-left
    // out of the parent, so it stays at 0 here).
    const par = root.value?.offsetParent as HTMLElement | null
    return { minX: 0, minY: 0, maxX: par?.clientWidth ?? 0, maxY: par?.clientHeight ?? 0 }
  },
  viewportSize: () => {
    // ceiling for E/S — the canvas, not the browser window. The panel sits inside a zoomable canvas
    // that can be smaller than the viewport, and a browser-window ceiling would let the panel be
    // dragged past the canvas edge.
    const par = root.value?.offsetParent as HTMLElement | null
    return { w: par?.clientWidth ?? window.innerWidth, h: par?.clientHeight ?? window.innerHeight }
  },
  min: { w: 340, h: 320 },   // matches `.panel { min-width / min-height }`
  zoom: injectedZoom,
  onActivate: () => emit('activate', props.index),
})

// A TILE cell is a BOX TO FIT INTO, not a size to adopt. A :square panel that took the cell's width
// would square itself to width + chrome a frame later (see enforceSquare) and overflow the row it was
// just placed in — which is how three gating plots came out ~2x their row height. Fit the square
// inside the cell instead, using the chrome THIS panel actually has: the gate pages keep their axis
// selectors in flow, so no shared estimate would be right for both panel kinds.
// Only for `cell` commands (Tile). Cascade's staggered size has no row below it to overflow, so
// clamping it there would shrink the plot for nothing. Non-square panels take what they are given.
function fitted(a: ArrangeCmd | null): ArrangeCmd | null {
  if (!a?.cell || !props.square || props.docked || !root.value || !mainEl.value) return a
  const chromeH = root.value.offsetHeight - mainEl.value.offsetHeight
  return { ...a, ...fitSquare(a, chromeH) }
}

// persist geometry (position + the CSS-resized size) so the layout survives navigation.
let ro: ResizeObserver | null = null
// keep the PLOT REGION (.panel-main) square by adjusting the box height, so a coord-fixed plot fills it
// with no blank space AND fixed in-flow controls (e.g. the gate axis selectors) are accounted for — the
// plot stays 1:1 and its x-axis is never clipped. Overlay (auto-hide) controls don't reserve height, so
// this reduces to a square box for them. The >1px diff settles it in one pass rather than oscillating.
//
// NEVER call this straight from the ResizeObserver — go through `squareFrame` (below). It writes
// `root.style.height` on the element the observer WATCHES, and a callback that resizes an observed
// element during delivery is precisely what the browser reports as "ResizeObserver loop completed with
// undelivered notifications" — the line observed in the log rail. The >1px guard bounds the
// loop but not the message: the notification is already undeliverable after the FIRST write.
function enforceSquare() {
  if (!props.square || props.docked || collapsed.value || !root.value || !mainEl.value) return
  const chromeH = root.value.offsetHeight - mainEl.value.offsetHeight   // head + in-flow controls/footer + borders
  const target = mainEl.value.offsetWidth + chromeH                     // → main becomes square (w × w)
  if (Math.abs(root.value.offsetHeight - target) > 1) root.value.style.height = target + 'px'
}
function persist() {
  if (!props.persistKey || !root.value) return
  if (collapsed.value) return   // collapsed height is transient — don't overwrite the saved size
  store.setGeom(props.persistKey, { x: pos.value.x, y: pos.value.y, w: root.value.offsetWidth, h: root.value.offsetHeight })
}
// ONE frame, one write — see `enforceSquare` for why this may not run inline in the observer
const squareFrame = rafCoalesce(() => { enforceSquare(); persist() })
watch(pos, persist, { deep: true })           // covers drag + Tile/Cascade
onMounted(() => {
  if (props.docked) return   // docked panels fill their slot; no saved geometry / resize tracking
  if (saved && root.value) { root.value.style.width = saved.w + 'px'; root.value.style.height = saved.h + 'px' }
  enforceSquare()            // square an odd saved geometry on first mount
  if (props.persistKey && root.value && typeof ResizeObserver !== 'undefined') {
    // schedule, don't write: the callback must leave layout alone (see `enforceSquare`). rAF puts the
    // write in the next frame, so the resize it causes is delivered as a fresh cycle — the same
    // medicine `usePlotResize` applies to plots, and `persist` rides along a frame later for free.
    ro = new ResizeObserver(() => squareFrame.schedule()); ro.observe(root.value)   // covers manual resize
  }
})
onBeforeUnmount(() => { squareFrame.cancel(); ro?.disconnect(); ro = null })
</script>

<template>
  <!-- `plot:<persistKey>` — the anchor a Kiwi plot ref points at (utils/guideAnchor, PointerBubble) -->
  <div ref="root" class="panel" :data-guide="persistKey ? `plot:${persistKey}` : undefined"
       :class="{ active, collapsed, docked,
                                          'controls-pinned': chromeMode === 'visible',
                                          'controls-hidden': chromeMode === 'hidden' }"
       :style="docked ? undefined : { left: pos.x + 'px', top: pos.y + 'px' }" @mousedown="emit('activate', index)">
    <!-- title row: the WHOLE row drags (like PopulationManager); buttons stop the drag -->
    <!-- the drag hint sits on the TITLE TEXT, not the row: the row also holds the collapse/remove
         buttons, and a tip there fired on top of theirs (docs/UI.md → nested tooltips) -->
    <div class="panel-head" @mousedown.prevent="docked || startDrag($event)">
      <span class="panel-title"><i v-if="!docked" class="pi pi-arrows-alt drag-icon" /><!--
        --><i v-else class="pi pi-arrows-alt drag-icon grip" draggable="true"
             v-tooltip.bottom="'Drag to move / swap'" @mousedown.stop @click.stop /><!--
        --><span class="panel-title-txt"
                 v-tooltip.bottom="docked ? undefined : 'Drag to move'">{{ title }}</span></span>
      <span class="panel-spacer" />
      <!-- 3-state cycle: auto (default, hover to show) → always visible → always hidden. Only
           renders when there ARE controls to reveal and auto-hide is active. -->
      <CcCycleButton v-if="autoHide && hasControls() && !collapsed"
                     class="panel-btn" v-model="chromeMode" :options="CHROME_OPTIONS"
                     @mousedown.stop />
      <AddToKiwiButton v-if="isLivePlot && !collapsed" class="panel-btn" size="dense"
                       :kiwi-ref="{ kind: 'plot', plotId: persistKey! }" />
      <button v-if="!docked" class="panel-btn cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense panel-collapse" v-tooltip.bottom="collapsed ? 'Expand' : 'Collapse'"
              @mousedown.stop @click.stop="collapsed = !collapsed">
        <i :class="collapsed ? 'pi pi-chevron-down' : 'pi pi-chevron-up'" />
      </button>
      <template v-if="removable">
        <span class="ctrl-sep" />
        <button class="panel-btn cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense panel-remove" v-tooltip.bottom="'Remove this plot'"
                @mousedown.stop @click.stop="emit('remove')">
          <i class="pi pi-minus" />
        </button>
      </template>
    </div>
    <!-- IN-FLOW controls (auto-hide OFF, e.g. the gate-drawing page): own rows so they never clip -->
    <div v-if="!autoHide && slots.actions && !collapsed" class="panel-controls cc-row cc-row-tight inflow"><slot name="actions" /></div>
    <!-- body always gets the whole box; in auto-hide mode the controls overlay it (see .cc-panel-controls) -->
    <div v-show="!collapsed" ref="mainEl" class="panel-main">
      <div ref="bodyEl" class="panel-body"><slot /></div>
      <template v-if="autoHide">
        <div v-if="slots.actions" class="panel-controls cc-row cc-row-tight cc-panel-controls"><slot name="actions" /></div>
        <div v-if="slots.footer" class="panel-foot cc-panel-controls bottom"><slot name="footer" /></div>
      </template>
    </div>
    <div v-if="!autoHide && slots.footer && !collapsed" class="panel-foot inflow"><slot name="footer" /></div>
    <!-- 8-handle resize frame — shared with `FloatingPanel` via `useResizeHandles`. Docked/collapsed
         panels don't resize (docked = grid slot owns the size; collapsed = height is transient). -->
    <template v-if="!docked && !collapsed">
      <div class="panel-edge panel-edge-n" @pointerdown="e => onResizeDown(e, { n: true })" />
      <div class="panel-edge panel-edge-s" @pointerdown="e => onResizeDown(e, { s: true })" />
      <div class="panel-edge panel-edge-e" @pointerdown="e => onResizeDown(e, { e: true })" />
      <div class="panel-edge panel-edge-w" @pointerdown="e => onResizeDown(e, { w: true })" />
      <div class="panel-corner panel-corner-nw" @pointerdown="e => onResizeDown(e, { n: true, w: true })" />
      <div class="panel-corner panel-corner-ne" @pointerdown="e => onResizeDown(e, { n: true, e: true })" />
      <div class="panel-corner panel-corner-sw" @pointerdown="e => onResizeDown(e, { s: true, w: true })" />
      <div class="panel-corner panel-corner-se" @pointerdown="e => onResizeDown(e, { s: true, e: true })" />
    </template>
  </div>
</template>

<style scoped>
/* free-floating, draggable + resizable box */
.panel { position: absolute; display: flex; flex-direction: column; border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-md); background: var(--cc-surface-1); overflow: hidden; box-shadow: 0 4px 18px rgba(0,0,0,0.35);
  width: 460px; height: 440px; min-width: 340px; min-height: 320px; z-index: 5;
  transition: border-color 0.12s, box-shadow 0.12s; }
.panel.active { border-color: var(--cc-selected); box-shadow: 0 0 0 1px var(--cc-selected), 0 6px 22px rgba(0,0,0,0.45); z-index: 6; }
/* docked: fill the grid slot, no float/drag/resize/shadow */
.panel.docked { position: static; width: 100%; height: 100%; min-width: 0; min-height: 0;
  box-shadow: none; z-index: auto; }
.panel.docked .panel-head { cursor: default; }
/* collapsed: box shrinks to just the header (overrides any inline/resized height); handles hidden via v-if */
.panel.collapsed { height: auto !important; min-height: 0 !important; }
.panel-head { display: flex; align-items: center; gap: 8px; padding: 5px 8px; cursor: move;
  border-bottom: 1px solid var(--cc-border); background: var(--cc-surface-2); }
/* min-width:0 lets the title shrink; the text span truncates so it never shoves the head buttons */
.panel-title { display: inline-flex; align-items: center; gap: 5px; font-weight: 700; font-size: var(--cc-fs-sm);
  letter-spacing: 0.02em; user-select: none; min-width: 0; }
.panel-title-txt { overflow: hidden; white-space: nowrap; text-overflow: ellipsis; }
.drag-icon { font-size: var(--cc-fs-2xs); opacity: 0.55; flex: none; }
/* docked panels: the drag icon IS the reorder handle (in-flow in the header → aligned with the other
   buttons, no absolute overlay to collide with the pin). Its native dragstart bubbles to the board slot. */
.drag-icon.grip { cursor: grab; font-size: var(--cc-fs-sm); opacity: 0.7; padding: 2px; margin: -2px 0; }
.drag-icon.grip:active { cursor: grabbing; }
.panel-spacer { flex: 1; }
/* main region below the head: the anchor for the auto-hide control overlays (position: relative) and
   the box the body fills. */
/* overflow: hidden so a plot with its own min-height can't spill out of the main region and cover the
   footer (export/duplicate) or header when the panel/slot is small — it clips within the box instead */
.panel-main { position: relative; flex: 1; min-height: 0; display: flex; flex-direction: column; overflow: hidden; }
/* controls / footer: shared layout only. The overlay look + hover-reveal live in the global
   .cc-panel-controls utility (style.css); the in-flow look (auto-hide off) is the .inflow variant. */
.panel-controls { padding: 5px 8px; }
.panel-foot { display: flex; align-items: center; justify-content: flex-end; gap: 6px; padding: 5px 8px; }
/* in-flow (auto-hide OFF): solid rows that reserve height, as before */
.panel-controls.inflow { border-bottom: 1px solid var(--cc-border); background: var(--cc-surface-1); }
.panel-foot.inflow { border-top: 1px solid var(--cc-border); background: var(--cc-surface-2); flex-shrink: 0; }
.ctrl-sep { width: 1px; align-self: stretch; background: var(--cc-border); margin: 2px 2px; }
/* .panel-btn → cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense */
.panel-btn:hover { color: var(--cc-text); border-color: #484f58; }
.panel-remove:hover { color: #f87171; border-color: #f87171; }
/* the panel content fills the rest; body is a column so a plot area can flex:1 inside it */
.panel-body { display: flex; flex-direction: column; flex: 1; min-height: 0; }
/* ── resize frame ─────────────────────────────────────────────────────────────
   Eight invisible hit regions around the edge of the panel (same rig as
   `FloatingPanel.vue`; the maths + pointer loop live in `useResizeHandles`).
   Corners sit ABOVE the edges so the diagonal cursor wins in the overlap.
   Panel has `overflow: hidden` (rounded-corner mask), so handles sit INSIDE
   the frame — a negative offset would be clipped and un-grabbable.
   z-index sits ABOVE the auto-hide control overlays (`.cc-panel-controls` z:6 in style.css) so a
   revealed bottom-strip does not eat the S / SE handles it sits over — the drift after this rig
   replaced browser-native `resize: both`, when the SE handle rode on the panel's own corner and had
   no z-index competition. Handles are 5px / 14px, so covering the control strip is imperceptible. */
.panel-edge, .panel-corner { position: absolute; z-index: 7; }
.panel-edge-n { top: 0; left: 10px; right: 10px; height: 5px; cursor: ns-resize; }
.panel-edge-s { bottom: 0; left: 10px; right: 10px; height: 5px; cursor: ns-resize; }
.panel-edge-e { top: 10px; bottom: 10px; right: 0; width: 5px; cursor: ew-resize; }
.panel-edge-w { top: 10px; bottom: 10px; left: 0; width: 5px; cursor: ew-resize; }
.panel-corner { width: 14px; height: 14px; z-index: 8; }
.panel-corner-nw { top: 0; left: 0; cursor: nwse-resize; }
.panel-corner-se { bottom: 0; right: 0; cursor: nwse-resize; }
.panel-corner-ne { top: 0; right: 0; cursor: nesw-resize; }
.panel-corner-sw { bottom: 0; left: 0; cursor: nesw-resize; }
/* Visible SE grip — the "resize corner" users already know from CSS `resize: both`. Other seven
   handles are invisible, discovered by the cursor change like any desktop window. */
.panel-corner-se {
  background:
    linear-gradient(135deg, transparent 0 6px, var(--cc-border) 6px 7px, transparent 7px 9px,
                    var(--cc-border) 9px 10px, transparent 10px);
}
</style>
