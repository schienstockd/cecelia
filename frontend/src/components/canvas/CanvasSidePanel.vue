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
  The `vis` styling block is opt-in — pass it for a manager of plot SERIES, omit it for one whose
  contents are not (the vault). `scope` is not: all three managers mean the same thing by it (one pick
  for every plot, or the active plot's own), including the vault — a model is picked per plot exactly
  as a highlight set is.

  What a HOST may ask of the thing slotted in here is `canvasManager.ts` (`CanvasManagerProps`) — the
  contract the Analysis board's rail swaps on. This file is only the box around it.

  Classes are `csp-` (root `.canvas-side-panel`). They were `pm-`, from the days when this WAS the
  population manager; consumers keep their own prefixes (`pm-`, `pick-`, `vault-`) because scoped
  styles mean a slotted row carries the CONSUMER's scope id — nothing here can reach it.

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
  // when provided, the global/local footer renders (every manager passes it — see the header)
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
  // `width` is a STARTING width, applied once — not a bound `:style`. CSS `resize` works by writing
  // `style.width` on the element, so a reactive style binding re-applies the prop on the next render
  // and the box visibly snaps back the moment anything else re-renders. Model names are long enough
  // that the widening is worth keeping.
  if (panel.value) panel.value.style.width = `${props.width}px`
  const par = panel.value?.offsetParent as HTMLElement | null
  if (par) pos.value = { x: Math.max(16, par.clientWidth - (panel.value!.offsetWidth || props.width) - 16), y: 16 }
})
function onHeaderDown(e: MouseEvent) { if (!props.docked) startDrag(e) }
</script>

<template>
  <div ref="panel" class="canvas-side-panel" :class="{ docked, collapsed }"
       :style="docked ? undefined : { left: pos.x + 'px', top: pos.y + 'px' }">
    <div class="csp-header" @mousedown.prevent="onHeaderDown">
      <i class="pi" :class="icon" />
      <span class="csp-title">{{ title }}</span>
      <span v-if="count !== undefined" class="csp-count">{{ count }}</span>
      <button v-if="!docked" class="cc-btn cc-btn-bare cc-btn-icon" v-tooltip.left="collapsed ? 'Expand' : 'Collapse'"
              @click.stop="collapsed = !collapsed">
        <i :class="collapsed ? 'pi pi-chevron-down' : 'pi pi-chevron-up'" />
      </button>
    </div>

    <div v-show="!collapsed" class="csp-body"><slot /></div>

    <!-- host-specific extra controls (e.g. the gating manager's gate / viewer options) -->
    <div v-show="!collapsed"><slot name="options" /></div>

    <!-- shared plot-styling block (only when the host passes a `vis` bag), obeys the scope below -->
    <div v-show="!collapsed" v-if="vis" class="csp-opts">
      <PlotOptions :vis="vis" :sections="optionsSections" :readout="readout"
                   @update:vis="emit('update:vis', $event)" />
    </div>

    <!-- scope (global = every plot / local = active plot only): icons only, at the very bottom -->
    <div v-show="!collapsed" v-if="scope" class="csp-footer">
      <ChipSelect class="csp-seg" variant="segmented" :options="SCOPE_OPTIONS"
                  :model-value="scope" aria-label="Scope"
                  @update:model-value="v => emit('update:scope', v as 'global' | 'local')" />
    </div>
  </div>
</template>

<style scoped>
/* Width: set ONCE on mount from the `width` prop (see onMounted — a bound `:style` would fight the
   resize grip), so a drag sticks. Docked fills its container instead.
   `resize` needs a non-`visible` overflow, so the box clips and the LIST scrolls inside it — which is
   also what lets a taller drag show more rows instead of just more empty box. Same idiom as
   CanvasPanel (CSS `resize`, not a hand-rolled grip); the height cap is the viewport so a long list
   can't run off the canvas. */
.canvas-side-panel {
  position: absolute; z-index: 20;
  display: flex; flex-direction: column;
  max-height: 90vh; min-width: 240px; min-height: 140px;
  resize: both; overflow: hidden;
  background: var(--cc-surface-1); border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-md); box-shadow: 0 6px 24px rgba(0,0,0,0.4);
  font-size: var(--cc-fs-sm); color: var(--cc-text); user-select: none;
}
/* docked: in-flow rail (no float/drag/resize/shadow), fills its container column */
.canvas-side-panel.docked { position: static; z-index: auto; width: 100%; box-shadow: none;
                            resize: none; overflow: visible; max-height: none; min-height: 0; }
.canvas-side-panel.docked .csp-header { cursor: default; }
/* docked has no box height of its own to fill, so the list keeps its own cap (the board rail must not
   grow without bound on a long population list) */
.canvas-side-panel.docked .csp-body { max-height: 60vh; }
/* collapsed: shrink to the header, overriding any dragged height; no grip on a header-only box */
.canvas-side-panel.collapsed { height: auto !important; min-height: 0 !important; resize: none; }
.csp-header {
  display: flex; align-items: center; gap: 6px; padding: 6px 8px; flex-shrink: 0;
  cursor: move; border-bottom: 1px solid var(--cc-border); background: var(--cc-surface-2);
  border-radius: var(--cc-radius-md) 6px 0 0;
}
.csp-title { font-weight: 600; }
.csp-count { color: var(--cc-text-dim); margin-left: auto; }
/* the one flexible row: takes the leftover height and scrolls (min-height:0 or flex won't shrink it) */
.csp-body { flex: 1 1 auto; min-height: 0; overflow-y: auto; }
/* the collapse button is `cc-btn cc-btn-bare cc-btn-icon` and nothing more — its old `.pm-icon:hover`
   rule was byte-identical to `.cc-btn-bare:hover`, so it went rather than got renamed. */
.csp-opts { border-top: 1px solid var(--cc-border); flex-shrink: 0; }
.csp-footer { display: flex; align-items: center; padding: 6px 8px; flex-shrink: 0; border-top: 1px solid var(--cc-border); background: var(--cc-surface-2); border-radius: 0 0 6px 6px; }
.csp-seg { margin-left: auto; }
</style>
