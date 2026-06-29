<!--
  Floating, collapsible population manager — shared across canvases (gating today; track-gating and
  the universal canvas next). Flat list (indented by depth): colour swatch, name (rename inline),
  count + %parent, an EYE that toggles whether the pop is colour-highlighted on the plots, napari
  visibility, and delete. Clicking a row sets it as the displayed parent on the active plot.

  Population SOURCE is the gating store, which is pop_type-agnostic (`g.popType` = flow / live /
  clust) — the manager renders whatever populations the store holds, so it is NOT flow-only. The
  plot-options (gate labels, line width, axis) are passed in as props by the host canvas, since
  they belong to the plot panels, not the manager.

  Highlight scope (footer): GLOBAL (globe) = highlights apply to every plot; LOCAL (pin) = only the
  active plot. The eye state reflects the current scope's highlight set. Draggable by its header
  (clamped on-screen via useFloatingPanel); collapsible body.
-->
<script setup lang="ts">
import { ref, onMounted, useTemplateRef } from 'vue'
import { useGatingStore, isReservedPopName, type FlatPop } from '../../stores/gating'
import { useLogStore } from '../../stores/log'
import { useSettingsStore } from '../../stores/settings'
import { useFloatingPanel } from '../../composables/useFloatingPanel'

const props = withDefaults(defineProps<{
  selected: string
  highlighted: string[]            // pops highlighted in the current scope (global / active plot)
  scope: 'global' | 'local'
  lineWidth: number                // gate stroke width
  gateLabels: boolean              // show population names on gates
  axisFromZero: boolean            // axis origin at 0 vs autoscale
  popType?: string                 // 'flow' (default) | 'track' — toggles viewer-option group
}>(), { popType: 'flow' })
const emit = defineEmits<{
  'update:selected': [string]
  'update:scope': ['global' | 'local']
  'update:lineWidth': [number]
  'update:gateLabels': [boolean]
  'update:axisFromZero': [boolean]
  toggleHighlight: [string]
}>()
const g = useGatingStore()
const log = useLogStore()
const settings = useSettingsStore()

const collapsed = ref(false)
const optionsOpen = ref(false)     // extra-options box under Highlight
const editing = ref<string | null>(null)
const editName = ref('')

// drag-to-move, clamped to the workspace (shared with the plot panels)
const panel = useTemplateRef<HTMLElement>('panel')
const { pos, startDrag } = useFloatingPanel(panel)
// open at the top-right of the workspace so it doesn't start stacked on the plots
onMounted(() => {
  const par = panel.value?.offsetParent as HTMLElement | null
  if (par) pos.value = { x: Math.max(16, par.clientWidth - (panel.value!.offsetWidth || 300) - 16), y: 16 }
})

function pick(p: FlatPop) { emit('update:selected', p.path) }
function beginRename(p: FlatPop) { editing.value = p.path; editName.value = p.name }
async function commitRename(p: FlatPop) {
  const name = editName.value.trim()
  if (isReservedPopName(name)) {   // `_`-prefix is reserved for derived pops (tracked / clustering)
    log.error(`Population names can't start with “${'_'}” (reserved for tracked / clustering)`,
              { source: 'gating' })
    editing.value = null
    return
  }
  editing.value = null
  if (name && name !== p.name) await g.renamePop(p.path, name)
}
const isLit = (p: FlatPop) => props.highlighted.includes(p.path)
const fmtPct = (v?: number) => v == null ? '' : `${v.toFixed(1)}%`

// per-pop napari visibility: flip the persisted `show` flag, then re-push to napari (silent
// if napari isn't open). Routes to the right overlay for the popType (Tracks layers vs Points).
async function toggleNapari(p: FlatPop) {
  await g.updatePop(p.path, { show: !p.show })
  g.refreshNapari()
}
</script>

<template>
  <div ref="panel" class="pop-manager" :style="{ left: pos.x + 'px', top: pos.y + 'px' }">
    <div class="pm-header" @mousedown.prevent="startDrag">
      <i class="pi pi-sitemap" />
      <span class="pm-title">Populations</span>
      <span class="pm-count">{{ g.flat.length }}</span>
      <button class="pm-icon" v-tooltip.left="collapsed ? 'Expand' : 'Collapse'"
              @click.stop="collapsed = !collapsed">
        <i :class="collapsed ? 'pi pi-chevron-down' : 'pi pi-chevron-up'" />
      </button>
    </div>

    <div v-show="!collapsed" class="pm-body">
      <div v-if="!g.flat.length" class="pm-empty">No populations yet — draw a gate.</div>

      <div v-for="p in g.flat" :key="p.path"
           class="pm-row" :class="{ active: p.path === props.selected, transient: p.transient }"
           :style="{ paddingLeft: 6 + p.depth * 14 + 'px' }"
           @click="pick(p)">
        <i v-if="p.transient" class="pi pi-map-marker pm-napari"
           v-tooltip.left="'Cells selected in napari (temporary)'" :style="{ color: p.colour }" />
        <input v-else type="color" class="pm-swatch" :value="p.colour"
               v-tooltip.left="'Colour'"
               @click.stop @change="g.updatePop(p.path, { colour: ($event.target as HTMLInputElement).value })" />

        <span v-if="editing !== p.path" class="pm-name"
              @dblclick.stop="p.transient || beginRename(p)">{{ p.name }}</span>
        <input v-else class="pm-rename" v-model="editName" autofocus
               @keyup.enter="commitRename(p)" @blur="commitRename(p)" @click.stop />

        <span class="pm-stat" v-tooltip.left="'cells · % of parent'">
          {{ g.stats[p.path]?.count ?? '–' }}
          <small>{{ fmtPct(g.stats[p.path]?.pctParent) }}</small>
        </span>

        <button class="pm-icon" :class="{ lit: isLit(p) }"
                v-tooltip.left="isLit(p) ? 'Hide colour on plots' : 'Highlight colour on plots'"
                @click.stop="emit('toggleHighlight', p.path)">
          <i :class="isLit(p) ? 'pi pi-eye' : 'pi pi-eye-slash'" />
        </button>
        <button v-if="!p.transient" class="pm-icon" :class="{ lit: p.show }"
                v-tooltip.left="p.show ? 'Visible in napari (click to hide)' : 'Hidden in napari (click to show)'"
                @click.stop="toggleNapari(p)">
          <i class="pi pi-images" />
        </button>
        <button v-if="!p.transient" class="pm-icon danger" v-tooltip.left="'Delete population'"
                @click.stop="g.deletePop(p.path)">
          <i class="pi pi-trash" />
        </button>
        <!-- the napari selection is transient (never persisted) — this clears it so it doesn't
             linger forever; there's no persisted pop to delete. -->
        <button v-else class="pm-icon danger" v-tooltip.left="'Clear napari selection'"
                @click.stop="g.clearNapariSelection()">
          <i class="pi pi-trash" />
        </button>
      </div>
    </div>

    <!-- options (collapsible), grouped by where they apply: plot vs viewer -->
    <div v-show="!collapsed" class="pm-opts">
      <button class="pm-opts-toggle" @click="optionsOpen = !optionsOpen">
        <i :class="optionsOpen ? 'pi pi-chevron-down' : 'pi pi-chevron-right'" />
        <span>Options</span>
      </button>
      <div v-show="optionsOpen" class="pm-opts-body">
        <div class="pm-opt-head"><span>plot</span></div>
        <div class="pm-opt-row">
          <span class="pm-opt-label">Gate labels</span>
          <button class="seg-btn" :class="{ active: gateLabels }"
                  v-tooltip.top="'Show population names on gates'"
                  @click="emit('update:gateLabels', !gateLabels)"><i class="pi pi-tag" /></button>
        </div>
        <div class="pm-opt-row">
          <span class="pm-opt-label">Line width</span>
          <input type="range" min="0.5" max="4" step="0.5" :value="lineWidth"
                 v-tooltip.top="'Gate line thickness'"
                 @input="emit('update:lineWidth', parseFloat(($event.target as HTMLInputElement).value))" />
          <span class="pm-opt-val">{{ lineWidth.toFixed(1) }}</span>
        </div>
        <div class="pm-opt-row">
          <span class="pm-opt-label">Axis</span>
          <div class="pm-seg">
            <button class="seg-btn" :class="{ active: axisFromZero }"
                    v-tooltip.top="'Whole-dataset scale (origin at 0) — axis stays fixed across populations'"
                    @click="emit('update:axisFromZero', true)"><i class="pi pi-arrows-alt" /></button>
            <button class="seg-btn" :class="{ active: !axisFromZero }"
                    v-tooltip.top="'Autoscale to the selected population'"
                    @click="emit('update:axisFromZero', false)"><i class="pi pi-arrow-down-left" /></button>
          </div>
        </div>

        <!-- viewer-option group is popType-specific: flow/live populations render as napari Points
             (size slider); track pops render as Tracks ribbons (no point size — tail width is a
             plot-panel concern), so the group is hidden for track rather than showing an inert control. -->
        <template v-if="props.popType !== 'track'">
          <div class="pm-opt-head"><span>viewer</span></div>
          <!-- napari point size (re-renders the napari overlay on release) -->
          <div class="pm-opt-row">
            <span class="pm-opt-label">Napari dots</span>
            <input type="range" min="1" max="20" step="1" :value="settings.napariPointSize"
                   v-tooltip.top="'Population point size in napari'"
                   @input="settings.napariPointSize = parseInt(($event.target as HTMLInputElement).value)"
                   @change="g.refreshNapariPops()" />
            <span class="pm-opt-val">{{ settings.napariPointSize }}</span>
          </div>
        </template>
      </div>
    </div>

    <!-- scope (global = every plot / local = active plot only): icons only, at the very bottom -->
    <div v-show="!collapsed" class="pm-footer">
      <div class="pm-seg">
        <button class="seg-btn" :class="{ active: scope === 'global' }"
                v-tooltip.top="'Global — options apply to every plot'"
                @click="emit('update:scope', 'global')"><i class="pi pi-globe" /></button>
        <button class="seg-btn" :class="{ active: scope === 'local' }"
                v-tooltip.top="'Local — options apply to the active plot only'"
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
.pm-empty { padding: 12px; color: var(--cc-text-dim); }
.pm-row { display: flex; align-items: center; gap: 6px; padding: 4px 8px 4px 6px; cursor: pointer; border-bottom: 1px solid var(--cc-border); }
.pm-row:hover { background: var(--cc-surface-2); }
.pm-row.active { background: color-mix(in srgb, var(--cc-accent) 22%, transparent); }
.pm-row.transient { font-style: italic; background: color-mix(in srgb, #22d3ee 8%, transparent); }
.pm-napari { width: 16px; text-align: center; font-size: 13px; }
.pm-swatch { width: 16px; height: 16px; padding: 0; border: none; background: none; cursor: pointer; }
.pm-name { flex: 1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.pm-rename { flex: 1; background: var(--cc-bg); color: var(--cc-text); border: 1px solid var(--cc-accent); border-radius: 3px; padding: 1px 4px; }
.pm-stat { color: var(--cc-text-dim); font-variant-numeric: tabular-nums; }
.pm-stat small { opacity: 0.7; margin-left: 3px; }
.pm-icon { background: none; border: none; color: var(--cc-text-dim); cursor: pointer; padding: 2px; }
.pm-icon:hover { color: var(--cc-text); }
.pm-icon.lit { color: var(--cc-accent); }
.pm-icon.danger:hover { color: #f87171; }

/* ── footer scope toggle (icons only, bottom-right) ── */
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

/* ── extra options ── */
.pm-opts { border-top: 1px solid var(--cc-border); }
.pm-opts-toggle { display: flex; align-items: center; gap: 6px; width: 100%; background: none; border: none;
  color: var(--cc-text-dim); cursor: pointer; padding: 6px 8px; font-size: 11px; text-transform: uppercase; letter-spacing: 0.05em; }
.pm-opts-toggle:hover { color: var(--cc-text); }
.pm-opts-body { padding: 4px 10px 10px; display: flex; flex-direction: column; gap: 8px; }
/* small section heading: ──── plot ──── */
.pm-opt-head { display: flex; align-items: center; gap: 6px; margin-top: 2px;
  color: var(--cc-text-dim); font-size: 10px; text-transform: uppercase; letter-spacing: 0.08em; }
.pm-opt-head::before, .pm-opt-head::after { content: ""; flex: 1; height: 1px; background: var(--cc-border); }
.pm-opt-head:first-child { margin-top: 0; }
.pm-opt-row { display: flex; align-items: center; gap: 8px; }
.pm-opt-label { color: var(--cc-text-dim); font-size: 11px; flex: 1; }
.pm-opt-row input[type="range"] { flex: 1; max-width: 110px; }
.pm-opt-val { color: var(--cc-text-dim); font-size: 11px; width: 1.8rem; text-align: right; font-variant-numeric: tabular-nums; }
</style>
