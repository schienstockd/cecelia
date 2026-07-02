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
import { ref, computed } from 'vue'
import { useGatingStore, isReservedPopName, type FlatPop } from '../../stores/gating'
import { useLogStore } from '../../stores/log'
import { useSettingsStore } from '../../stores/settings'
import PopulationPanelShell from './PopulationPanelShell.vue'
import type { VisProps } from '../../plots/plot'

const props = withDefaults(defineProps<{
  selected: string
  highlighted: string[]            // pops highlighted in the current scope (global / active plot)
  scope: 'global' | 'local'
  lineWidth: number                // gate stroke width
  gateLabels: boolean              // show population names on gates
  axisFromZero: boolean            // axis origin at 0 vs autoscale
  popType?: string                 // 'flow' (default) | 'track' | 'clust' | 'trackclust'
  clusterIds?: number[]            // cluster mode: the tickable cluster IDs for the active suffix
  suffix?: string                  // cluster mode: which clusters.{suffix} new pops filter on
  // OPTIONAL plot-styling block: when a host canvas passes `vis`, the shared PlotOptions styling
  // controls render below the gate options (same knobs as the summary SeriesPicker). Omit → no block.
  vis?: VisProps
  docked?: boolean                 // fill a docked rail (Analysis board) instead of floating
  readonly?: boolean               // read-only surface (Analysis board): highlight only — no add / delete /
                                   // rename / recolour / cluster reassignment (project_analysis_canvas_readonly)
}>(), { popType: 'flow', clusterIds: () => [], suffix: 'default', vis: undefined, docked: false, readonly: false })
const emit = defineEmits<{
  'update:selected': [string]
  'update:scope': ['global' | 'local']
  'update:lineWidth': [number]
  'update:gateLabels': [boolean]
  'update:axisFromZero': [boolean]
  'update:vis': [patch: Partial<VisProps>]
  toggleHighlight: [string]
}>()
const g = useGatingStore()
const log = useLogStore()
const settings = useSettingsStore()

const optionsOpen = ref(false)     // gate / viewer options box (host-specific, in the shell #options slot)
const editing = ref<string | null>(null)
const editName = ref('')

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

// ── Cluster mode (clust / trackclust): a population IS a set of cluster IDs (a filter on
// clusters.{suffix}). Instead of drawing gates, the user ticks cluster IDs into a pop here. A
// cluster belongs to AT MOST ONE pop (ticking it into B moves it off A) — mirrors old R
// setClusterForPop. Writes go set-wide via the store's mirrorUids. ────────────────────────────
const clusterMode = computed(() => props.popType === 'clust' || props.popType === 'trackclust')
const POP_PALETTE = [
  '#ef4444', '#f59e0b', '#10b981', '#3b82f6', '#a78bfa', '#ec4899', '#14b8a6', '#eab308',
  '#f97316', '#22d3ee', '#84cc16', '#8b5cf6', '#f43f5e', '#06b6d4', '#a3e635', '#d946ef',
]
const popClusterIds = (p: FlatPop): number[] => {
  const v = p.filter?.values
  return Array.isArray(v) ? (v as unknown[]).map(Number) : []
}
const clusterOwner = (id: number): FlatPop | undefined => g.flat.find(p => popClusterIds(p).includes(id))

async function toggleCluster(p: FlatPop, id: number) {
  const owner = clusterOwner(id)
  if (owner?.path === p.path) {                       // already in this pop → remove
    await g.updatePop(p.path, { filter: { values: popClusterIds(p).filter(x => x !== id) } })
    return
  }
  // move it off whatever pop currently owns it, then add to this one (mutually exclusive)
  if (owner) await g.updatePop(owner.path, { filter: { values: popClusterIds(owner).filter(x => x !== id) } })
  await g.updatePop(p.path, { filter: { values: [...popClusterIds(p), id].sort((a, b) => a - b) } })
}

async function addClusterPopulation() {
  const n = g.flat.length
  await g.addClusterPop(`Population ${n + 1}`, props.suffix, POP_PALETTE[n % POP_PALETTE.length])
}
</script>

<template>
  <PopulationPanelShell :count="g.flat.length" :scope="scope" :vis="vis" :docked="docked"
                        @update:scope="emit('update:scope', $event)" @update:vis="emit('update:vis', $event)">
    <!-- ── population list (default slot) ── -->
      <!-- cluster mode: pops are made here (no gate to draw), then clusters ticked into them -->
      <div v-if="clusterMode && !readonly" class="pm-add">
        <button class="pm-add-btn" @click="addClusterPopulation"
                v-tooltip.bottom="'Create a population, then tick cluster IDs into it'">
          <i class="pi pi-plus" /> Add population
        </button>
      </div>

      <div v-if="!g.flat.length" class="pm-empty">
        {{ clusterMode ? 'No populations yet — add one, then tick clusters into it.' : 'No populations yet — draw a gate.' }}
      </div>

      <template v-for="p in g.flat" :key="p.path">
        <div class="pm-row" :class="{ active: p.path === props.selected, transient: p.transient }"
             :style="{ paddingLeft: 6 + p.depth * 14 + 'px' }"
             @click="pick(p)">
          <i v-if="p.transient" class="pi pi-map-marker pm-napari"
             v-tooltip.left="'Cells selected in napari (temporary)'" :style="{ color: p.colour }" />
          <input v-else type="color" class="pm-swatch" :value="p.colour" :disabled="readonly"
                 v-tooltip.left="readonly ? '' : 'Colour'"
                 @click.stop @change="g.updatePop(p.path, { colour: ($event.target as HTMLInputElement).value })" />

          <span v-if="editing !== p.path" class="pm-name"
                @dblclick.stop="!readonly && !p.transient && beginRename(p)">{{ p.name }}</span>
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
          <button v-if="!p.transient && !readonly" class="pm-icon danger" v-tooltip.left="'Delete population'"
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

        <!-- cluster-ID toggles: tick a cluster into this pop (filled = assigned; a cluster lives in
             at most one pop). Tooltip names the owner if it's assigned elsewhere. -->
        <div v-if="clusterMode && p.filter" class="pm-clusters"
             :style="{ paddingLeft: 22 + p.depth * 14 + 'px' }">
          <button v-for="id in props.clusterIds" :key="id" class="pm-chip"
                  :class="{ on: popClusterIds(p).includes(id), ro: readonly }" :disabled="readonly"
                  :style="popClusterIds(p).includes(id) ? { background: p.colour, borderColor: p.colour, color: '#111' } : {}"
                  v-tooltip.bottom="clusterOwner(id) && clusterOwner(id)?.path !== p.path ? `In “${clusterOwner(id)?.name}”` : ''"
                  @click.stop="!readonly && toggleCluster(p, id)">{{ id }}</button>
          <span v-if="!props.clusterIds.length" class="pm-chip-empty">no clusters at this suffix</span>
        </div>
      </template>

    <!-- ── gate / viewer options (host-specific, #options slot). In cluster mode there are no gates
         (the plot group); trackclust has no viewer control either, so the whole block is hidden. ── -->
    <template v-if="props.popType !== 'trackclust'" #options>
      <div class="pm-opts">
        <button class="pm-opts-toggle" @click="optionsOpen = !optionsOpen">
          <i :class="optionsOpen ? 'pi pi-chevron-down' : 'pi pi-chevron-right'" />
          <span>Options</span>
        </button>
        <div v-show="optionsOpen" class="pm-opts-body">
          <template v-if="!clusterMode">
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
          </template>

          <!-- viewer-option group is popType-specific: flow/live/clust populations render as napari
               Points (size slider); track/trackclust render as Tracks ribbons (no point size — tail
               width is a plot-panel concern), so the group is hidden for those. -->
          <template v-if="props.popType !== 'track' && props.popType !== 'trackclust'">
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
    </template>
  </PopulationPanelShell>
</template>

<style scoped>
/* row / options styles — applied to content rendered into PopulationPanelShell's slots (slotted
   content keeps THIS component's scoped styles; the floating chrome + scope footer live in the shell). */
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

/* ── cluster mode: add-pop bar + per-pop cluster-ID toggle chips ── */
.pm-add { padding: 6px 8px; border-bottom: 1px solid var(--cc-border); }
.pm-add-btn { display: inline-flex; align-items: center; gap: 5px; font-size: 11px; padding: 4px 9px;
  border: 1px solid var(--cc-border); border-radius: 4px; background: var(--cc-surface-2);
  color: var(--cc-text); cursor: pointer; }
.pm-add-btn:hover { border-color: #7c3aed; color: #c4b5fd; }
.pm-clusters { display: flex; flex-wrap: wrap; gap: 3px; padding: 2px 8px 6px; border-bottom: 1px solid var(--cc-border); }
.pm-chip { min-width: 1.4rem; height: 1.4rem; padding: 0 4px; font-size: 11px; line-height: 1;
  border: 1px solid var(--cc-border); border-radius: 3px; background: var(--cc-surface-1);
  color: var(--cc-text-dim); cursor: pointer; font-variant-numeric: tabular-nums; transition: background 0.1s, color 0.1s, border-color 0.1s; }
.pm-chip:hover { border-color: #7c3aed; color: var(--cc-text); }
.pm-chip.on { font-weight: 700; }
/* read-only (Analysis board): chips show assignment but aren't clickable */
.pm-chip.ro { cursor: default; }
.pm-chip.ro:hover { border-color: var(--cc-border); color: var(--cc-text-dim); }
.pm-chip.ro.on:hover { color: #111; }
.pm-chip-empty { font-size: 10px; color: var(--cc-text-dim); font-style: italic; }

/* segmented toggle (axis option in the #options slot; the shell owns the footer scope toggle) */
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
