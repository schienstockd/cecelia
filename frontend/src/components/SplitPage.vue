<script setup lang="ts">
// THE pipeline / writing-surface page frame: a toolbar on top, then an optional resizable LIST pane
// beside the DETAIL (docs/ui/PRIMITIVES.md → "A pipeline / writing-surface module page").
// Used by Tasks, Blackboard and the Chains Live tab; the bar is the same as ModuleLayout's.
//
//   #toolbar   the bar rows (a `.cc-row` each). Several rows stack with a small gap — Tasks has a
//              search/view row and a filter row. Omit it and there is no bar.
//   #list      the left pane. It scrolls itself; the divider beside it drags its width (persisted
//              under `storageKey`). Omit it and the detail takes the whole width (the Chains Live tab).
//   default    the detail. A flex column that owns its own scrolling.
//
// The divider sits BESIDE the scrolling half, not over it — overlaid, it covers the list's scrollbar
// and swallows every grab of the thumb.
import { usePanelResize } from '../composables/usePanelResize'

const props = withDefaults(defineProps<{
  /** localStorage key for the list pane's width. */
  storageKey?: string
  min?: number
  max?: number
  /** Starting list width in px. */
  defaultWidth?: number
}>(), { storageKey: undefined, min: 220, max: 760, defaultWidth: 360 })

// handle on the list's RIGHT edge → dragging right widens it
const { widthStyle, onResizeStart } = usePanelResize({
  min: props.min, max: props.max, default: props.defaultWidth, storageKey: props.storageKey, edge: 'right',
})
</script>

<template>
  <div class="sp">
    <div v-if="$slots.toolbar" class="sp-toolbar"><slot name="toolbar" /></div>
    <div class="sp-body">
      <aside v-if="$slots.list" class="sp-list" :style="widthStyle">
        <div class="sp-list-scroll"><slot name="list" /></div>
        <div class="sp-divider" @mousedown="onResizeStart" v-tooltip.top="'Drag to resize the list'" />
      </aside>
      <div class="sp-detail"><slot /></div>
    </div>
  </div>
</template>

<style scoped>
.sp { display: flex; flex-direction: column; height: 100%; overflow: hidden; background: var(--cc-bg); }

/* the same bar as ModuleLayout's action bar and SetBar: one padding token, one surface, one divider */
.sp-toolbar {
  display: flex; flex-direction: column; gap: var(--cc-space-xs);
  padding: var(--cc-bar-pad);
  background: var(--cc-surface-1);
  border-bottom: 1px solid var(--cc-border);
  flex-shrink: 0;
}

.sp-body { flex: 1; display: flex; min-height: 0; overflow: hidden; }

/* width from usePanelResize; `flex-shrink: 0` so the row does not fight it. A ROW: scroll half, then
   the divider beside it. */
.sp-list { flex-shrink: 0; display: flex; min-height: 0; overflow: hidden; border-right: 1px solid var(--cc-border); }
/* takes the space so a table inside sits at its natural height at the top instead of stretching */
.sp-list-scroll { flex: 1; min-width: 0; min-height: 0; overflow: auto; }
.sp-divider { flex: 0 0 5px; cursor: col-resize; }
.sp-divider:hover { background: var(--cc-accent); opacity: 0.35; }

.sp-detail { flex: 1; min-width: 0; min-height: 0; display: flex; flex-direction: column; overflow: hidden; position: relative; }
</style>
