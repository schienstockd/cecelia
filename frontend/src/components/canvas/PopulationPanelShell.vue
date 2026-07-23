<!--
  Shared CHROME for the floating population panels — the draggable, collapsible box that both the
  gating `PopulationManager` (single tree, mutating) and the summary `SeriesPicker` (read-only,
  cross-segmentation) wrap. Owns everything they had in common: the container + top-right placement,
  the draggable header (icon · title · count · collapse), the global/local scope footer, and the
  optional shared `PlotOptions` styling block. The differing bit — the population LIST — is the
  default slot; a host with its own extra controls (the gating manager's gate/viewer options) uses the
  `#options` slot. One place for the chrome so the future universal analysis board reuses it too.
-->
<script setup lang="ts">
import { ref, onMounted, useTemplateRef } from 'vue'
import { useFloatingPanel } from '../../composables/useFloatingPanel'
import PlotOptions from './PlotOptions.vue'
import ChipSelect, { type ChipOption } from '../ChipSelect.vue'
import type { VisProps } from '../../plots/plot'

// scope: global = every plot, local = active plot only (icon-only segmented control)
const SCOPE_OPTIONS: ChipOption[] = [
  { value: 'global', label: '', icon: 'pi pi-globe', tip: 'Global — applies to every plot' },
  { value: 'local', label: '', icon: 'pi pi-map-marker', tip: 'Local — applies to the active plot only' },
]

const props = withDefaults(defineProps<{
  title?: string
  count?: number | string          // shown at the right of the header (population count)
  scope: 'global' | 'local'
  // when provided, the shared PlotOptions styling block renders above the footer (obeys `scope`)
  vis?: VisProps
  optionsSections?: ('layout' | 'points' | 'colours' | 'labels')[]
  // DOCKED: render in-flow (a fixed rail, e.g. the Analysis-canvas layout) instead of a draggable
  // floating box — no absolute positioning, no drag, full width of its container.
  docked?: boolean
}>(), { title: 'Populations', count: undefined, vis: undefined, optionsSections: undefined, docked: false })
const emit = defineEmits<{
  'update:scope': ['global' | 'local']
  'update:vis': [patch: Partial<VisProps>]
}>()

const collapsed = ref(false)
// drag-to-move, clamped to the workspace; open at the top-right so it doesn't start on the plots.
// (docked mode ignores all of this — it renders in-flow.)
const panel = useTemplateRef<HTMLElement>('panel')
const { pos, startDrag } = useFloatingPanel(panel)
onMounted(() => {
  if (props.docked) return
  const par = panel.value?.offsetParent as HTMLElement | null
  if (par) pos.value = { x: Math.max(16, par.clientWidth - (panel.value!.offsetWidth || 300) - 16), y: 16 }
})
function onHeaderDown(e: MouseEvent) { if (!props.docked) startDrag(e) }
</script>

<template>
  <div ref="panel" class="pop-manager" :class="{ docked }"
       :style="docked ? undefined : { left: pos.x + 'px', top: pos.y + 'px' }">
    <div class="pm-header" @mousedown.prevent="onHeaderDown">
      <i class="pi pi-sitemap" />
      <span class="pm-title">{{ title }}</span>
      <span v-if="count !== undefined" class="pm-count">{{ count }}</span>
      <button v-if="!docked" class="pm-icon" v-tooltip.left="collapsed ? 'Expand' : 'Collapse'"
              @click.stop="collapsed = !collapsed">
        <i :class="collapsed ? 'pi pi-chevron-down' : 'pi pi-chevron-up'" />
      </button>
    </div>

    <div v-show="!collapsed" class="pm-body"><slot /></div>

    <!-- host-specific extra controls (e.g. the gating manager's gate / viewer options) -->
    <div v-show="!collapsed"><slot name="options" /></div>

    <!-- shared plot-styling block (only when the host passes a `vis` bag), obeys the scope below -->
    <div v-show="!collapsed" v-if="vis" class="pm-opts">
      <PlotOptions :vis="vis" :sections="optionsSections" @update:vis="emit('update:vis', $event)" />
    </div>

    <!-- scope (global = every plot / local = active plot only): icons only, at the very bottom -->
    <div v-show="!collapsed" class="pm-footer">
      <ChipSelect class="pm-seg" variant="segmented" :options="SCOPE_OPTIONS"
                  :model-value="scope" aria-label="Scope"
                  @update:model-value="v => emit('update:scope', v as 'global' | 'local')" />
    </div>
  </div>
</template>

<style scoped>
.pop-manager {
  position: absolute; z-index: 20; width: 300px;
  background: var(--cc-surface-1); border: 1px solid var(--cc-border);
  border-radius: 6px; box-shadow: 0 6px 24px rgba(0,0,0,0.4);
  font-size: 12px; color: var(--cc-text); user-select: none;
}
/* docked: in-flow rail (no float/drag/shadow), fills its container column */
.pop-manager.docked { position: static; z-index: auto; width: 100%; box-shadow: none; }
.pop-manager.docked .pm-header { cursor: default; }
.pm-header {
  display: flex; align-items: center; gap: 6px; padding: 6px 8px;
  cursor: move; border-bottom: 1px solid var(--cc-border); background: var(--cc-surface-2);
  border-radius: 6px 6px 0 0;
}
.pm-title { font-weight: 600; }
.pm-count { color: var(--cc-text-dim); margin-left: auto; }
.pm-body { max-height: 60vh; overflow-y: auto; }
.pm-icon { background: none; border: none; color: var(--cc-text-dim); cursor: pointer; padding: 2px; }
.pm-icon:hover { color: var(--cc-text); }
.pm-opts { border-top: 1px solid var(--cc-border); }
.pm-footer { display: flex; align-items: center; padding: 6px 8px; border-top: 1px solid var(--cc-border); background: var(--cc-surface-2); border-radius: 0 0 6px 6px; }
.pm-seg { margin-left: auto; }
</style>
