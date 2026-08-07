<!--
  Shared CHROME for a canvas SIDE PANEL — the draggable, collapsible box that sits beside the plots on
  a canvas and manages the things they show. Three wrap it: the gating `PopulationManager` (single
  tree, mutating), the summary `SeriesPicker` (read-only, cross-segmentation) and the optical-flow
  `FlowModelVault` (the trained-model list). Owns everything they had in common: the container +
  top-right placement, the draggable header (icon · title · count · collapse), the optional
  global/local scope footer, and the optional shared `PlotOptions` styling block. The differing bit —
  the LIST — is the default slot; a host with its own extra controls (the gating manager's gate/viewer
  options) uses the `#options` slot.

  It was `PopulationPanelShell` until the model vault showed the chrome was never population-specific.
  Both plot-only parts are opt-in: pass `scope` for the global/local footer, `vis` for the styling
  block. A manager of things that are not plot series (the vault) passes neither.
  NB the internal classes keep the `pm-` prefix — the consumers' slotted rows use the same prefix in
  their own scoped styles, so renaming here would only half-rename the visible markup.

  NOT the app's `FloatingPanel` (Viewer, Lab log). That one is VIEWPORT-fixed and stacks with the other
  app windows; this one is absolutely positioned inside a zoomable canvas and belongs to it. Putting a
  canvas-scoped manager in a top-level window makes it collide with the Viewer and the Lab log — the
  two floating mechanisms are a deliberate split (see INVENTORY.md).
-->
<script setup lang="ts">
import { ref, onMounted, useTemplateRef } from 'vue'
import { useFloatingPanel } from '../../composables/useFloatingPanel'
import PlotOptions from './PlotOptions.vue'
import ChipSelect, { type ChipOption } from '../ChipSelect.vue'
import type { VisProps } from '../../plots/plot'
import { emptyReadout, type PlotReadout } from '../../plots/plotReadout'

// scope: global = every plot, local = active plot only (icon-only segmented control)
const SCOPE_OPTIONS: ChipOption[] = [
  { value: 'global', label: '', icon: 'pi pi-globe', tip: 'Global — applies to every plot' },
  { value: 'local', label: '', icon: 'pi pi-map-marker', tip: 'Local — applies to the active plot only' },
]

const props = withDefaults(defineProps<{
  title?: string
  icon?: string                    // header icon (a PrimeIcons class, e.g. 'pi-database')
  count?: number | string          // shown at the right of the header (population count)
  width?: number                   // px; a wider list (the model vault's table) needs more room
  // when provided, the global/local footer renders — omit it for a panel that manages things which
  // are not plot series (the model vault: a model is not shown "on the active plot only")
  scope?: 'global' | 'local'
  // when provided, the shared PlotOptions styling block renders above the footer (obeys `scope`)
  vis?: VisProps
  optionsSections?: ('layout' | 'points' | 'colours' | 'labels' | 'stats')[]
  readout?: PlotReadout            // active plot's last render (stats test + auto-overrides)
  // DOCKED: render in-flow (a fixed rail, e.g. the Analysis-canvas layout) instead of a draggable
  // floating box — no absolute positioning, no drag, full width of its container.
  docked?: boolean
}>(), { title: 'Populations', icon: 'pi-sitemap', count: undefined, width: 300, scope: undefined,
        vis: undefined, optionsSections: undefined, readout: emptyReadout, docked: false })
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
  if (par) pos.value = { x: Math.max(16, par.clientWidth - (panel.value!.offsetWidth || props.width) - 16), y: 16 }
})
function onHeaderDown(e: MouseEvent) { if (!props.docked) startDrag(e) }
</script>

<template>
  <div ref="panel" class="pop-manager" :class="{ docked, collapsed }"
       :style="docked ? { width: '100%' } : { left: pos.x + 'px', top: pos.y + 'px', width: width + 'px' }">
    <div class="pm-header" @mousedown.prevent="onHeaderDown">
      <i class="pi" :class="icon" />
      <span class="pm-title">{{ title }}</span>
      <span v-if="count !== undefined" class="pm-count">{{ count }}</span>
      <button v-if="!docked" class="pm-icon cc-btn cc-btn-bare cc-btn-icon" v-tooltip.left="collapsed ? 'Expand' : 'Collapse'"
              @click.stop="collapsed = !collapsed">
        <i :class="collapsed ? 'pi pi-chevron-down' : 'pi pi-chevron-up'" />
      </button>
    </div>

    <div v-show="!collapsed" class="pm-body"><slot /></div>

    <!-- host-specific extra controls (e.g. the gating manager's gate / viewer options) -->
    <div v-show="!collapsed"><slot name="options" /></div>

    <!-- shared plot-styling block (only when the host passes a `vis` bag), obeys the scope below -->
    <div v-show="!collapsed" v-if="vis" class="pm-opts">
      <PlotOptions :vis="vis" :sections="optionsSections" :readout="readout"
                   @update:vis="emit('update:vis', $event)" />
    </div>

    <!-- scope (global = every plot / local = active plot only): icons only, at the very bottom.
         Opt-in: a panel whose contents are not plot series (the model vault) passes no `scope`. -->
    <div v-show="!collapsed" v-if="scope" class="pm-footer">
      <ChipSelect class="pm-seg" variant="segmented" :options="SCOPE_OPTIONS"
                  :model-value="scope" aria-label="Scope"
                  v-tooltip.top="'Apply these options to every plot or just the active one'"
                  @update:model-value="v => emit('update:scope', v as 'global' | 'local')" />
    </div>
  </div>
</template>

<style scoped>
/* Width is set inline from the `width` prop (docked → 100%); the user can then drag the corner.
   `resize` needs a non-`visible` overflow, so the box clips and the LIST scrolls inside it — which is
   also what lets a taller drag show more rows instead of just more empty box. Same idiom as
   CanvasPanel (CSS `resize`, not a hand-rolled grip); the height cap is the viewport so a long list
   can't run off the canvas. */
.pop-manager {
  position: absolute; z-index: 20;
  display: flex; flex-direction: column;
  max-height: 90vh; min-width: 240px; min-height: 140px;
  resize: both; overflow: hidden;
  background: var(--cc-surface-1); border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-md); box-shadow: 0 6px 24px rgba(0,0,0,0.4);
  font-size: var(--cc-fs-sm); color: var(--cc-text); user-select: none;
}
/* docked: in-flow rail (no float/drag/resize/shadow), fills its container column */
.pop-manager.docked { position: static; z-index: auto; box-shadow: none;
                      resize: none; overflow: visible; max-height: none; min-height: 0; }
.pop-manager.docked .pm-header { cursor: default; }
/* docked has no box height of its own to fill, so the list keeps its own cap (the board rail must not
   grow without bound on a long population list) */
.pop-manager.docked .pm-body { max-height: 60vh; }
/* collapsed: shrink to the header, overriding any dragged height; no grip on a header-only box */
.pop-manager.collapsed { height: auto !important; min-height: 0 !important; resize: none; }
.pm-header {
  display: flex; align-items: center; gap: 6px; padding: 6px 8px; flex-shrink: 0;
  cursor: move; border-bottom: 1px solid var(--cc-border); background: var(--cc-surface-2);
  border-radius: var(--cc-radius-md) 6px 0 0;
}
.pm-title { font-weight: 600; }
.pm-count { color: var(--cc-text-dim); margin-left: auto; }
/* the one flexible row: takes the leftover height and scrolls (min-height:0 or flex won't shrink it) */
.pm-body { flex: 1 1 auto; min-height: 0; overflow-y: auto; }
/* .pm-icon → cc-btn cc-btn-bare cc-btn-icon */
.pm-icon:hover { color: var(--cc-text); }
.pm-opts { border-top: 1px solid var(--cc-border); flex-shrink: 0; }
.pm-footer { display: flex; align-items: center; padding: 6px 8px; flex-shrink: 0; border-top: 1px solid var(--cc-border); background: var(--cc-surface-2); border-radius: 0 0 6px 6px; }
.pm-seg { margin-left: auto; }
</style>
