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
import { ref, watch, onMounted, useTemplateRef } from 'vue'
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

// The PlotOptions block gets crowded — the Layout/Points/Colours/Stats/Labels sections plus the pop
// list on a small panel. This bottom-LEFT toggle folds the block away entirely (mirrors the bottom-
// RIGHT scope chip so the two footer controls sit at the same tier). Persisted globally via
// localStorage — same pattern as CollapsibleSection's `storageKey`, so the preference survives a
// remount and is shared across canvases (nobody wants to re-hide it per panel).
const PLOT_OPTS_KEY = 'canvasSidePanel.plotOptionsVisible'

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
  // Manual-apply mode toggle + Apply-chip state (OPT-IN, for canvases with many pops / images where
  // per-click fetches trickle pops into the plot). See useSummaryData.manualApply — the host owns
  // the staging state; this shell just surfaces the toggle in the footer and the chip in the header.
  // `manualApply: null` marks the tri-state "the host doesn't offer this" — same anti-cast idiom as
  // `CollapsibleSection.open`; an optional `boolean` gets Vue-cast to `false`, so the toggle would
  // always render on the vault / gating manager.
  manualApply?: boolean | null
  hasStaged?: boolean
  stagedChangeCount?: number
}>(), { title: 'Populations', icon: 'pi-sitemap', count: undefined, width: 300, scope: undefined,
        vis: undefined, optionsSections: undefined, readout: emptyReadout, docked: false,
        manualApply: null, hasStaged: false, stagedChangeCount: 0 })
const emit = defineEmits<{
  'update:scope': ['global' | 'local']
  'update:vis': [patch: Partial<VisProps>]
  'update:manualApply': [boolean]
  'apply:staged': []
  'discard:staged': []
}>()

const collapsed = ref(false)
// PlotOptions visibility — defaults OFF (the block is crowded and rarely touched — surface it from
// the footer toggle when it's wanted). Reads from localStorage so a user who turned it on stays on.
const plotOptionsVisible = ref((() => {
  const v = typeof window !== 'undefined' ? window.localStorage.getItem(PLOT_OPTS_KEY) : null
  return v === null ? false : v === '1'
})())
watch(plotOptionsVisible, v => {
  try { window.localStorage.setItem(PLOT_OPTS_KEY, v ? '1' : '0') } catch { /* ignore */ }
})
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

    <!-- Apply chip: renders in manual-apply mode when the staged selection differs from what the
         plots are showing. Two buttons — Apply commits, Discard reverts stagedSel to gSel. -->
    <div v-if="manualApply && hasStaged" v-show="!collapsed" class="csp-apply">
      <span class="csp-apply-note cc-fs-2xs cc-muted">{{ stagedChangeCount }} pending</span>
      <button class="csp-apply-btn cc-btn cc-btn-primary cc-btn-dense"
              v-tooltip.top="'Apply staged selection to the plots'"
              @click="emit('apply:staged')">Apply</button>
      <button class="cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense"
              v-tooltip.top="'Remove staged changes'"
              @click="emit('discard:staged')"><i class="pi pi-times" /></button>
    </div>

    <div v-show="!collapsed" class="csp-body"><slot /></div>

    <!-- host-specific extra controls (e.g. the gating manager's gate / viewer options) -->
    <div v-show="!collapsed"><slot name="options" /></div>

    <!-- shared plot-styling block (only when the host passes a `vis` bag), obeys the scope below.
         `v-if` on `plotOptionsVisible` UNMOUNTS the block when the settings toggle is off, so it costs
         nothing to render while hidden. -->
    <div v-show="!collapsed" v-if="vis && plotOptionsVisible" class="csp-opts">
      <PlotOptions :vis="vis" :sections="optionsSections" :readout="readout"
                   @update:vis="emit('update:vis', $event)" />
    </div>

    <!-- footer: settings toggle + manual-apply toggle (left, when a `vis` bag is passed) + scope chip
         (right, when a `scope` is passed). Renders when any half is meaningful — hosts without any
         (rare) get no footer. Manual-apply is only offered when the host opts in with `manualApply`
         defined (undefined means "the host doesn't support staging" — hide the toggle entirely). -->
    <div v-show="!collapsed" v-if="scope || vis" class="csp-footer">
      <button v-if="vis" class="csp-opts-toggle cc-btn cc-btn-ghost cc-btn-icon"
              :class="{ 'cc-btn-on cc-btn-on-tint': plotOptionsVisible }"
              v-tooltip.top="plotOptionsVisible ? 'Hide plot settings' : 'Show plot settings'"
              @click="plotOptionsVisible = !plotOptionsVisible">
        <i class="pi pi-sliders-h" />
      </button>
      <button v-if="manualApply !== null" class="csp-opts-toggle cc-btn cc-btn-ghost cc-btn-icon"
              :class="{ 'cc-btn-on cc-btn-on-tint': manualApply }"
              v-tooltip.top="manualApply
                ? 'Manual apply — click Apply to update plots'
                : 'Live — plots update on every pop toggle'"
              @click="emit('update:manualApply', !manualApply)">
        <i class="pi pi-clock" />
      </button>
      <ChipSelect v-if="scope" class="csp-seg" variant="segmented" :options="SCOPE_OPTIONS"
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
.csp-footer { display: flex; align-items: center; gap: 6px; padding: 6px 8px; flex-shrink: 0; border-top: 1px solid var(--cc-border); background: var(--cc-surface-2); border-radius: 0 0 6px 6px; }
.csp-seg { margin-left: auto; }
/* pending-changes chip: sits directly under the header, above the pop list, so it's visible while
   the user ticks eyes. Bordered so it separates from the list rows. */
.csp-apply { display: flex; align-items: center; gap: 6px; padding: 5px 8px; flex-shrink: 0;
             border-bottom: 1px solid var(--cc-border);
             background: color-mix(in srgb, var(--cc-accent) 12%, transparent); }
.csp-apply-note { flex: 1; }
.csp-apply-btn { min-width: 4rem; }
</style>
