<!--
  Generic free-floating canvas panel: the chrome (draggable title bar, resize, active border,
  Tile/Cascade arrange, remove) with no knowledge of what's inside. Plot panels wrap their content
  here: the gating scatter (GatePlotPanel) today; the summary (Vega-Lite) and track-gating panels
  next. Header tools go in the `actions` slot, the plot body in the default slot.

  Drag/clamp/arrange come from useFloatingPanel so every floating panel behaves identically.
-->
<script setup lang="ts">
import { ref, computed, onMounted, onBeforeUnmount, useTemplateRef, watch, useSlots } from 'vue'
import { useFloatingPanel, type ArrangeCmd } from '../../composables/useFloatingPanel'
import { useCanvasPanelsStore } from '../../stores/canvasPanels'
import { useInjectedZoom } from '../../composables/useCanvasZoom'

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
// pin the auto-hide controls open (local like `collapsed` — a transient chrome preference, not a
// persisted plot option). Only meaningful when autoHide is on and there are controls to reveal.
const pinned = ref(false)
const slots = useSlots()
const hasControls = computed(() => !!slots.actions || !!slots.footer)
const root = useTemplateRef<HTMLElement>('root')
const mainEl = useTemplateRef<HTMLElement>('mainEl')   // .panel-main — the plot region kept square by :square
const store = useCanvasPanelsStore()
const saved = props.persistKey ? store.getGeom(props.persistKey) : undefined
// the host canvas may apply a visual zoom (transform:scale); inject it so drag deltas are zoom-correct
const injectedZoom = useInjectedZoom()
const { pos, startDrag } = useFloatingPanel(root, {
  // restore the saved position, else stagger by index
  initial: saved ? { x: saved.x, y: saved.y } : { x: 16 + props.index * 30, y: 16 + props.index * 30 },
  onActivate: () => emit('activate', props.index),
  arrange: () => props.arrange,
  zoom: injectedZoom,
})

// persist geometry (position + the CSS-resized size) so the layout survives navigation.
let ro: ResizeObserver | null = null
// keep the PLOT REGION (.panel-main) square by adjusting the box height, so a coord-fixed plot fills it
// with no blank space AND fixed in-flow controls (e.g. the gate axis selectors) are accounted for — the
// plot stays 1:1 and its x-axis is never clipped. Overlay (auto-hide) controls don't reserve height, so
// this reduces to a square box for them. Guarded by a >1px diff so we don't loop the ResizeObserver.
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
watch(pos, persist, { deep: true })           // covers drag + Tile/Cascade
onMounted(() => {
  if (props.docked) return   // docked panels fill their slot; no saved geometry / resize tracking
  if (saved && root.value) { root.value.style.width = saved.w + 'px'; root.value.style.height = saved.h + 'px' }
  enforceSquare()            // square an odd saved geometry on first mount
  if (props.persistKey && root.value && typeof ResizeObserver !== 'undefined') {
    ro = new ResizeObserver(() => { enforceSquare(); persist() }); ro.observe(root.value)   // covers manual resize
  }
})
onBeforeUnmount(() => { ro?.disconnect(); ro = null })
</script>

<template>
  <div ref="root" class="panel" :class="{ active, collapsed, docked, 'controls-pinned': pinned }"
       :style="docked ? undefined : { left: pos.x + 'px', top: pos.y + 'px' }" @mousedown="emit('activate', index)">
    <!-- title row: the WHOLE row drags (like PopulationManager); buttons stop the drag -->
    <div class="panel-head" @mousedown.prevent="docked || startDrag($event)"
         v-tooltip.bottom="docked ? undefined : 'Drag to move'">
      <span class="panel-title"><i v-if="!docked" class="pi pi-arrows-alt drag-icon" /><!--
        --><i v-else class="pi pi-arrows-alt drag-icon grip" draggable="true"
             v-tooltip.bottom="'Drag to move / swap'" @mousedown.stop @click.stop /><!--
        --><span class="panel-title-txt">{{ title }}</span></span>
      <span class="panel-spacer" />
      <!-- pin the auto-hiding controls open (next to the drag icon). Only when there ARE controls
           to reveal and auto-hide is active. -->
      <button v-if="autoHide && hasControls && !collapsed" class="panel-btn" :class="{ on: pinned }"
              v-tooltip.bottom="pinned ? 'Auto-hide controls (show on hover)' : 'Keep controls visible'"
              @mousedown.stop @click.stop="pinned = !pinned">
        <i class="pi pi-thumbtack" />
      </button>
      <button v-if="!docked" class="panel-btn panel-collapse" v-tooltip.bottom="collapsed ? 'Expand' : 'Collapse'"
              @mousedown.stop @click.stop="collapsed = !collapsed">
        <i :class="collapsed ? 'pi pi-chevron-down' : 'pi pi-chevron-up'" />
      </button>
      <template v-if="removable">
        <span class="ctrl-sep" />
        <button class="panel-btn panel-remove" v-tooltip.bottom="'Remove this plot'"
                @mousedown.stop @click.stop="emit('remove')">
          <i class="pi pi-minus" />
        </button>
      </template>
    </div>
    <!-- IN-FLOW controls (auto-hide OFF, e.g. the gate-drawing page): own rows so they never clip -->
    <div v-if="!autoHide && slots.actions && !collapsed" class="panel-controls inflow"><slot name="actions" /></div>
    <!-- body always gets the whole box; in auto-hide mode the controls overlay it (see .cc-panel-controls) -->
    <div v-show="!collapsed" ref="mainEl" class="panel-main">
      <div class="panel-body"><slot /></div>
      <template v-if="autoHide">
        <div v-if="slots.actions" class="panel-controls cc-panel-controls"><slot name="actions" /></div>
        <div v-if="slots.footer" class="panel-foot cc-panel-controls bottom"><slot name="footer" /></div>
      </template>
    </div>
    <div v-if="!autoHide && slots.footer && !collapsed" class="panel-foot inflow"><slot name="footer" /></div>
  </div>
</template>

<style scoped>
/* free-floating, draggable + resizable box */
.panel { position: absolute; display: flex; flex-direction: column; border: 1px solid var(--cc-border);
  border-radius: 6px; background: var(--cc-surface-1); overflow: hidden; box-shadow: 0 4px 18px rgba(0,0,0,0.35);
  width: 460px; height: 440px; min-width: 340px; min-height: 320px; resize: both; z-index: 5;
  transition: border-color 0.12s, box-shadow 0.12s; }
.panel.active { border-color: #ff8c1a; box-shadow: 0 0 0 1px #ff8c1a, 0 6px 22px rgba(0,0,0,0.45); z-index: 6; }
/* docked: fill the grid slot, no float/drag/resize/shadow */
.panel.docked { position: static; width: 100%; height: 100%; min-width: 0; min-height: 0; resize: none;
  box-shadow: none; z-index: auto; }
.panel.docked .panel-head { cursor: default; }
/* collapsed: box shrinks to just the header (overrides any inline/resized height); no resize handle */
.panel.collapsed { height: auto !important; min-height: 0 !important; resize: none; }
.panel-head { display: flex; align-items: center; gap: 8px; padding: 5px 8px; cursor: move;
  border-bottom: 1px solid var(--cc-border); background: var(--cc-surface-2); }
/* min-width:0 lets the title shrink; the text span truncates so it never shoves the head buttons */
.panel-title { display: inline-flex; align-items: center; gap: 5px; font-weight: 700; font-size: 12px;
  letter-spacing: 0.02em; user-select: none; min-width: 0; }
.panel-title-txt { overflow: hidden; white-space: nowrap; text-overflow: ellipsis; }
.drag-icon { font-size: 10px; opacity: 0.55; flex: none; }
/* docked panels: the drag icon IS the reorder handle (in-flow in the header → aligned with the other
   buttons, no absolute overlay to collide with the pin). Its native dragstart bubbles to the board slot. */
.drag-icon.grip { cursor: grab; font-size: 12px; opacity: 0.7; padding: 2px; margin: -2px 0; }
.drag-icon.grip:active { cursor: grabbing; }
.panel-spacer { flex: 1; }
/* main region below the head: the anchor for the auto-hide control overlays (position: relative) and
   the box the body fills. */
/* overflow: hidden so a plot with its own min-height can't spill out of the main region and cover the
   footer (export/duplicate) or header when the panel/slot is small — it clips within the box instead */
.panel-main { position: relative; flex: 1; min-height: 0; display: flex; flex-direction: column; overflow: hidden; }
/* controls / footer: shared layout only. The overlay look + hover-reveal live in the global
   .cc-panel-controls utility (style.css); the in-flow look (auto-hide off) is the .inflow variant. */
.panel-controls { display: flex; flex-wrap: wrap; align-items: center; gap: 6px; padding: 5px 8px; }
.panel-foot { display: flex; align-items: center; justify-content: flex-end; gap: 6px; padding: 5px 8px; }
/* in-flow (auto-hide OFF): solid rows that reserve height, as before */
.panel-controls.inflow { border-bottom: 1px solid var(--cc-border); background: var(--cc-surface-1); }
.panel-foot.inflow { border-top: 1px solid var(--cc-border); background: var(--cc-surface-2); flex-shrink: 0; }
.ctrl-sep { width: 1px; align-self: stretch; background: var(--cc-border); margin: 2px 2px; }
.panel-btn { display: inline-flex; align-items: center; justify-content: center; width: 1.5rem; height: 1.5rem;
  border: 1px solid var(--cc-border); border-radius: 0.3rem; background: var(--cc-surface-1); color: var(--cc-text-dim);
  cursor: pointer; font-size: 0.7rem; }
.panel-btn:hover { color: var(--cc-text); border-color: #484f58; }
.panel-btn.on { color: var(--cc-text); border-color: var(--cc-accent); }
.panel-remove:hover { color: #f87171; border-color: #f87171; }
/* the panel content fills the rest; body is a column so a plot area can flex:1 inside it */
.panel-body { display: flex; flex-direction: column; flex: 1; min-height: 0; }
</style>
