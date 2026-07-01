<!--
  Shared CHROME for the floating population panels — the draggable, collapsible box that both the
  gating `PopulationManager` (single tree, mutating) and the summary `SeriesPicker` (read-only,
  cross-segmentation) wrap. Owns everything they had in common: the container + top-right placement,
  the draggable header (icon · title · count · collapse), the global/local scope footer, and the
  optional shared `PlotOptions` styling block. The differing bit — the population LIST — is the
  default slot; a host with its own extra controls (the gating manager's gate/viewer options) uses the
  `#options` slot. One place for the chrome so the future universal analysis canvas reuses it too.
-->
<script setup lang="ts">
import { ref, onMounted, useTemplateRef } from 'vue'
import { useFloatingPanel } from '../../composables/useFloatingPanel'
import PlotOptions from './PlotOptions.vue'
import type { VisProps } from '../../plots/plot'

withDefaults(defineProps<{
  title?: string
  count?: number | string          // shown at the right of the header (population count)
  scope: 'global' | 'local'
  // when provided, the shared PlotOptions styling block renders above the footer (obeys `scope`)
  vis?: VisProps
  optionsSections?: ('layout' | 'points' | 'colours' | 'labels')[]
}>(), { title: 'Populations', count: undefined, vis: undefined, optionsSections: undefined })
const emit = defineEmits<{
  'update:scope': ['global' | 'local']
  'update:vis': [patch: Partial<VisProps>]
}>()

const collapsed = ref(false)
// drag-to-move, clamped to the workspace; open at the top-right so it doesn't start on the plots
const panel = useTemplateRef<HTMLElement>('panel')
const { pos, startDrag } = useFloatingPanel(panel)
onMounted(() => {
  const par = panel.value?.offsetParent as HTMLElement | null
  if (par) pos.value = { x: Math.max(16, par.clientWidth - (panel.value!.offsetWidth || 300) - 16), y: 16 }
})
</script>

<template>
  <div ref="panel" class="pop-manager" :style="{ left: pos.x + 'px', top: pos.y + 'px' }">
    <div class="pm-header" @mousedown.prevent="startDrag">
      <i class="pi pi-sitemap" />
      <span class="pm-title">{{ title }}</span>
      <span v-if="count !== undefined" class="pm-count">{{ count }}</span>
      <button class="pm-icon" v-tooltip.left="collapsed ? 'Expand' : 'Collapse'"
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
      <div class="pm-seg">
        <button class="seg-btn" :class="{ active: scope === 'global' }"
                v-tooltip.top="'Global — applies to every plot'"
                @click="emit('update:scope', 'global')"><i class="pi pi-globe" /></button>
        <button class="seg-btn" :class="{ active: scope === 'local' }"
                v-tooltip.top="'Local — applies to the active plot only'"
                @click="emit('update:scope', 'local')"><i class="pi pi-map-marker" /></button>
      </div>
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
.pm-seg { display: inline-flex; gap: 4px; margin-left: auto; }
.seg-btn {
  display: inline-flex; align-items: center; justify-content: center;
  width: 1.8rem; height: 1.8rem; border-radius: 0.3rem;
  border: 1px solid var(--cc-border); background: var(--cc-surface-1);
  color: var(--cc-text-dim); cursor: pointer; font-size: 0.8rem; transition: background 0.1s, color 0.1s, border-color 0.1s;
}
.seg-btn:hover { color: var(--cc-text); border-color: #484f58; }
.seg-btn.active { background: #2d1b69; border-color: #7c3aed; color: #c4b5fd; }
</style>
