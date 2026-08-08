<script setup lang="ts">
// THE right-hand side panel: a thin always-visible handle on its left edge folds the content away,
// and a drag strip beside the handle resizes it (docs/UI.md → UX-primitive catalog).
//
// Extracted from ModuleLayout, where this had been inline — the module pages' functions/tasks panel.
// Nothing else could reuse it, so the Movies player had no way to get the same affordance, and
// ModuleLayout had meanwhile hand-rolled its OWN drag-to-resize next to the shared `usePanelResize`
// that TaskRunner and MetadataPanel use. One component, one composable underneath.
//
// COLLAPSE IS ONE SHARED FLAG, deliberately: `settings.rightPanelCollapsed` is global, so folding the
// panel away on one page folds it everywhere (decision: Dominik). That is the behaviour module pages
// have always had, and the Movies list joins it rather than introducing a second, per-panel rule the
// user would have to learn. WIDTH stays per panel (`storageKey`) — panels hold different things and a
// shared width would be wrong for all of them.
import { useSettingsStore } from '../stores/settings'
import { usePanelResize } from '../composables/usePanelResize'

const props = withDefaults(defineProps<{
  /** localStorage key for THIS panel's width. Widths are per panel; the collapse state is not. */
  storageKey: string
  /** What the panel holds, for the handle's tooltip — e.g. "functions panel", "movie list". */
  label: string
  min?: number
  max?: number
  /** Starting width in px. `null` (the default) sizes to content until the user drags. */
  defaultWidth?: number | null
}>(), { min: 200, max: 680, defaultWidth: null })

const settings = useSettingsStore()
const { widthStyle, onResizeStart } = usePanelResize({
  min: props.min, max: props.max, default: props.defaultWidth, storageKey: props.storageKey,
})
</script>

<template>
  <div class="cc-panel" :class="{ collapsed: settings.rightPanelCollapsed }"
       :style="settings.rightPanelCollapsed ? undefined : widthStyle">
    <!-- drag the left edge to resize (persisted per panel) -->
    <div v-if="!settings.rightPanelCollapsed" class="cc-panel-resizer" @mousedown="onResizeStart"
         v-tooltip.left="'Drag to resize'" />
    <button class="cc-panel-handle"
            @click="settings.rightPanelCollapsed = !settings.rightPanelCollapsed"
            v-tooltip.left="`${settings.rightPanelCollapsed ? 'Show' : 'Hide'} ${label}`"
            :aria-label="`${settings.rightPanelCollapsed ? 'Show' : 'Hide'} ${label}`">
      <i :class="['pi', settings.rightPanelCollapsed ? 'pi-angle-double-left' : 'pi-angle-double-right']" />
    </button>
    <!-- v-show, not v-if: collapsing must not tear down the content (and refetch everything on reopen) -->
    <div v-show="!settings.rightPanelCollapsed" class="cc-panel-slot"><slot /></div>
  </div>
</template>

<style scoped>
/* A thin always-visible handle on the left edge toggles the slot; when collapsed only the handle
   remains, so the panel folds away to the right. */
.cc-panel {
  display: flex;
  flex-shrink: 0;
  overflow: hidden;
}
/* Full-height strip down the panel's edge, NOT an icon button: it has no height of its own and
   stretches as a flex child. .cc-btn-icon's fixed square collapsed it to a chip at the top. */
.cc-panel-handle {
  flex-shrink: 0;
  width: 1.1rem;
  border: none;
  border-left: 1px solid var(--cc-border);
  background: var(--cc-surface-1);
  color: var(--cc-text-dim);
  cursor: pointer;
  display: flex;
  align-items: center;
  justify-content: center;
  transition: background 0.12s, color 0.12s;
}
.cc-panel-handle .pi { font-size: var(--cc-fs-xs); }
.cc-panel-handle:hover { background: var(--cc-surface-2); color: var(--cc-text); }
/* drag strip on the panel's left edge to resize (col-resize); thin, highlights on hover */
.cc-panel-resizer {
  flex-shrink: 0; width: 5px; cursor: col-resize;
  background: transparent; transition: background 0.12s;
}
.cc-panel-resizer:hover { background: var(--cc-accent); }
.cc-panel-slot { flex: 1; display: flex; min-width: 0; min-height: 0; overflow-y: auto; }
</style>
