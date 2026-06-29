<!--
  Generic free-floating canvas panel: the chrome (draggable title bar, resize, active border,
  Tile/Cascade arrange, remove) with no knowledge of what's inside. Plot panels wrap their content
  here: the gating scatter (GatePlotPanel) today; the summary (Vega-Lite) and track-gating panels
  next. Header tools go in the `actions` slot, the plot body in the default slot.

  Drag/clamp/arrange come from useFloatingPanel so every floating panel behaves identically.
-->
<script setup lang="ts">
import { ref, onMounted, onBeforeUnmount, useTemplateRef, watch, useSlots } from 'vue'
import { useFloatingPanel, type ArrangeCmd } from '../../composables/useFloatingPanel'
import { useCanvasPanelsStore } from '../../stores/canvasPanels'

const props = withDefaults(defineProps<{
  index: number
  title: string
  active?: boolean
  removable?: boolean
  // window-arrangement command (Tile/Cascade); seq bumps to force re-apply
  arrange?: ArrangeCmd | null
  // when set (`${canvasKey}:${panelId}`), drag position + size persist here across navigation
  persistKey?: string
}>(), { active: false, removable: true, arrange: null })
const emit = defineEmits<{ activate: [number]; remove: [] }>()

const collapsed = ref(false)
const slots = useSlots()
const root = useTemplateRef<HTMLElement>('root')
const store = useCanvasPanelsStore()
const saved = props.persistKey ? store.getGeom(props.persistKey) : undefined
const { pos, startDrag } = useFloatingPanel(root, {
  // restore the saved position, else stagger by index
  initial: saved ? { x: saved.x, y: saved.y } : { x: 16 + props.index * 30, y: 16 + props.index * 30 },
  onActivate: () => emit('activate', props.index),
  arrange: () => props.arrange,
})

// persist geometry (position + the CSS-resized size) so the layout survives navigation.
let ro: ResizeObserver | null = null
function persist() {
  if (!props.persistKey || !root.value) return
  if (collapsed.value) return   // collapsed height is transient — don't overwrite the saved size
  store.setGeom(props.persistKey, { x: pos.value.x, y: pos.value.y, w: root.value.offsetWidth, h: root.value.offsetHeight })
}
watch(pos, persist, { deep: true })           // covers drag + Tile/Cascade
onMounted(() => {
  if (saved && root.value) { root.value.style.width = saved.w + 'px'; root.value.style.height = saved.h + 'px' }
  if (props.persistKey && root.value && typeof ResizeObserver !== 'undefined') {
    ro = new ResizeObserver(persist); ro.observe(root.value)   // covers manual resize
  }
})
onBeforeUnmount(() => { ro?.disconnect(); ro = null })
</script>

<template>
  <div ref="root" class="panel" :class="{ active, collapsed }"
       :style="{ left: pos.x + 'px', top: pos.y + 'px' }" @mousedown="emit('activate', index)">
    <!-- title row: the WHOLE row drags (like PopulationManager); buttons stop the drag -->
    <div class="panel-head" @mousedown.prevent="startDrag" v-tooltip.bottom="'Drag to move'">
      <span class="panel-title"><i class="pi pi-arrows-alt drag-icon" />{{ title }}</span>
      <span class="panel-spacer" />
      <button class="panel-collapse" v-tooltip.bottom="collapsed ? 'Expand' : 'Collapse'"
              @mousedown.stop @click.stop="collapsed = !collapsed">
        <i :class="collapsed ? 'pi pi-chevron-down' : 'pi pi-chevron-up'" />
      </button>
      <template v-if="removable">
        <span class="ctrl-sep" />
        <button class="panel-remove" v-tooltip.bottom="'Remove this plot'"
                @mousedown.stop @click.stop="emit('remove')">
          <i class="pi pi-minus" />
        </button>
      </template>
    </div>
    <!-- controls row (plot options) — its own row so it never clips at min width -->
    <div v-if="slots.actions && !collapsed" class="panel-controls"><slot name="actions" /></div>
    <div v-show="!collapsed" class="panel-body"><slot /></div>
    <!-- footer row (utility actions: duplicate / export) -->
    <div v-if="slots.footer && !collapsed" class="panel-foot"><slot name="footer" /></div>
  </div>
</template>

<style scoped>
/* free-floating, draggable + resizable box */
.panel { position: absolute; display: flex; flex-direction: column; border: 1px solid var(--cc-border);
  border-radius: 6px; background: var(--cc-surface-1); overflow: hidden; box-shadow: 0 4px 18px rgba(0,0,0,0.35);
  width: 460px; height: 440px; min-width: 340px; min-height: 320px; resize: both; z-index: 5;
  transition: border-color 0.12s, box-shadow 0.12s; }
.panel.active { border-color: #ff8c1a; box-shadow: 0 0 0 1px #ff8c1a, 0 6px 22px rgba(0,0,0,0.45); z-index: 6; }
/* collapsed: box shrinks to just the header (overrides any inline/resized height); no resize handle */
.panel.collapsed { height: auto !important; min-height: 0 !important; resize: none; }
.panel-head { display: flex; align-items: center; gap: 8px; padding: 5px 8px; cursor: move;
  border-bottom: 1px solid var(--cc-border); background: var(--cc-surface-2); }
.panel-title { display: inline-flex; align-items: center; gap: 5px; font-weight: 700; font-size: 12px;
  letter-spacing: 0.02em; user-select: none; }
.drag-icon { font-size: 10px; opacity: 0.55; }
.panel-spacer { flex: 1; }
/* controls row: wraps instead of clipping when the panel is narrow */
.panel-controls { display: flex; flex-wrap: wrap; align-items: center; gap: 6px; padding: 5px 8px;
  border-bottom: 1px solid var(--cc-border); background: var(--cc-surface-1); }
/* footer row: utility actions, right-aligned */
.panel-foot { display: flex; align-items: center; justify-content: flex-end; gap: 6px; padding: 5px 8px;
  border-top: 1px solid var(--cc-border); background: var(--cc-surface-2); flex-shrink: 0; }
.ctrl-sep { width: 1px; align-self: stretch; background: var(--cc-border); margin: 2px 2px; }
.panel-remove, .panel-collapse { display: inline-flex; align-items: center; justify-content: center; width: 1.5rem; height: 1.5rem;
  border: 1px solid var(--cc-border); border-radius: 0.3rem; background: var(--cc-surface-1); color: var(--cc-text-dim);
  cursor: pointer; font-size: 0.7rem; }
.panel-remove:hover { color: #f87171; border-color: #f87171; }
.panel-collapse:hover { color: var(--cc-text); border-color: #484f58; }
/* the panel content fills the rest; body is a column so a plot area can flex:1 inside it */
.panel-body { display: flex; flex-direction: column; flex: 1; min-height: 0; }
</style>
