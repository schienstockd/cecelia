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
import { useGatingStore, type FlatPop } from '../../stores/gating'
import { useLogStore } from '../../stores/log'
import { useProjectStore } from '../../stores/project'
import { useSettingsStore } from '../../stores/settings'
import PopulationPanelShell from './PopulationPanelShell.vue'
import ConfirmDeleteButton from '../ConfirmDeleteButton.vue'
import TeleportPopover from '../TeleportPopover.vue'
import { parseFilterValues, filterSummary } from '../../utils/filterPopForm'
import { popNameError } from '../../utils/popName'
import { PALETTES, type VisProps } from '../../plots/plot'
import { clusterMeasure, isClusterPopType } from '../../utils/clusterMeasure'
import ChipSelect, { type ChipOption } from '../ChipSelect.vue'

const AXIS_OPTIONS: ChipOption[] = [
  { value: 'zero', label: '', icon: 'pi pi-arrows-alt',     tip: 'Whole-dataset scale (origin at 0) — axis stays fixed across populations' },
  { value: 'auto', label: '', icon: 'pi pi-arrow-down-left', tip: 'Autoscale to the selected population' },
]

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
  showDefiningPlot: [FlatPop]      // open the plot where this pop's gate was drawn
}>()
const g = useGatingStore()
const log = useLogStore()
const projectStore = useProjectStore()
const settings = useSettingsStore()

// napari point size is a PER-SET viewer preference (keyed by the gated image's set), set once and
// held across the set's images. Guard the setter so we never write under an empty set key.
const napariSetUid = computed(() => (g.imageUid ? projectStore.setUidOfImage(g.imageUid) : null) ?? '')
const napariPointSize = computed<number>({
  get: () => settings.getPointSize(napariSetUid.value),
  set: v => { if (napariSetUid.value) settings.setPointSize(napariSetUid.value, v) },
})

const optionsOpen = ref(false)     // gate / viewer options box (host-specific, in the shell #options slot)
const editing = ref<string | null>(null)
const editName = ref('')

// pop colour picker: clicking a pop's swatch opens a small popover offering the Cecelia palette
// (click a chip) plus the native picker for anything custom. A native <input type="color"> can't
// carry preset swatches, so we anchor our own popover to the clicked swatch instead.
const CECELIA_PALETTE = PALETTES.cecelia
const colourPop = ref<string | null>(null)          // path of the pop whose picker is open
const colourAnchor = ref<HTMLElement | null>(null)  // the clicked swatch (drives popover placement)
const colourOpen = computed<boolean>({ get: () => colourPop.value !== null, set: v => { if (!v) colourPop.value = null } })
const colourPopColour = computed(() => g.flat.find(p => p.path === colourPop.value)?.colour ?? '#ffffff')
const eqColour = (a: string, b: string) => a.toLowerCase() === b.toLowerCase()
function openColour(p: FlatPop, e: MouseEvent) {
  if (props.readonly || p.transient) return
  colourAnchor.value = e.currentTarget as HTMLElement
  colourPop.value = p.path
}
function setColour(c: string, close = true) {
  if (colourPop.value) g.updatePop(colourPop.value, { colour: c })
  if (close) colourPop.value = null
}

function pick(p: FlatPop) { emit('update:selected', p.path) }
function beginRename(p: FlatPop) { editing.value = p.path; editName.value = p.name }
async function commitRename(p: FlatPop) {
  const name = editName.value.trim()
  // reserved-prefix + same-list duplicate are caught here for instant feedback; a cross-pop-type
  // collision is rejected by the server (pop_name_conflict) and surfaced via the store's error toast.
  const err = popNameError(name, g.flat.map(x => x.name), { currentName: p.name })
  if (err) { log.error(err, { source: 'gating' }); editing.value = null; return }
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
const clusterMode = computed(() => isClusterPopType(props.popType))
const POP_PALETTE = [
  '#ef4444', '#f59e0b', '#10b981', '#3b82f6', '#a78bfa', '#ec4899', '#14b8a6', '#eab308',
  '#f97316', '#22d3ee', '#84cc16', '#8b5cf6', '#f43f5e', '#06b6d4', '#a3e635', '#d946ef',
]
// PER-RUN scoping (cluster mode): a run's populations are exactly the pops whose filter targets THIS
// run's `clusters.{suffix}` column. The store holds every run's pops for the segmentation in ONE
// sidecar (gating/{vn}__{popType}.json), so we scope to the active run here — switching the run
// (suffix) dropdown then shows that run's pops, not another run's. Non-cluster surfaces are unfiltered.
const visiblePops = computed<FlatPop[]>(() =>
  clusterMode.value
    ? g.flat.filter(p => p.filter?.measure === clusterMeasure(props.popType, props.suffix))
    : g.flat)
const popClusterIds = (p: FlatPop): number[] => {
  const v = p.filter?.values
  return Array.isArray(v) ? (v as unknown[]).map(Number) : []
}
// a cluster ID belongs to at most one pop WITHIN the current run (scope to visiblePops so ticking is
// exclusive per run, not across runs that happen to share the segmentation's sidecar)
const clusterOwner = (id: number): FlatPop | undefined => visiblePops.value.find(p => popClusterIds(p).includes(id))

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
  const n = visiblePops.value.length
  await g.addClusterPop(`Population ${n + 1}`, props.suffix, POP_PALETTE[n % POP_PALETTE.length])
}

// ── Filter-population form (Decision 15): define a pop by an AND-ed filter on ANY obs measure, for the
// current popType. Reuses pop/add (compound `conditions`). Aligned with the old populationUI.R dialog,
// modernised — colour is user-picked (not random) and it's the same Population underneath (badged, not
// a separate manager). ──
interface FpCond { measure: string; fun: string; values: string }
const FILTER_FUNS = ['gt', 'gte', 'lt', 'lte', 'eq', 'neq', 'in']
const showFilterForm = ref(false)
const fpEditPath = ref<string | null>(null)   // null → creating; a path → editing that filter pop
const fpName = ref('')
const fpParent = ref('root')
const fpColour = ref(POP_PALETTE[0])
const fpConds = ref<FpCond[]>([{ measure: '', fun: 'gt', values: '' }])
// measures: per-cell obs (regions/clusters/aggregate/hmm/speed…) + gateable var columns (intensities)
const filterMeasures = computed(() => [...new Set([...g.obsColumns, ...g.columns])].sort())
const parentOptions = computed(() => ['root', ...visiblePops.value.map(p => p.path)])

function addFpCond() { fpConds.value.push({ measure: filterMeasures.value[0] ?? '', fun: 'gt', values: '' }) }
function removeFpCond(i: number) { fpConds.value.splice(i, 1) }

function resetFilterForm() {
  fpEditPath.value = null; fpName.value = ''; fpParent.value = 'root'
  fpColour.value = POP_PALETTE[0]; fpConds.value = [{ measure: '', fun: 'gt', values: '' }]
}
function openCreateFilter() { resetFilterForm(); showFilterForm.value = true }
// EDIT reuses the same form (nothing special about editing): pre-fill from the pop's stored filter.
function beginEditFilter(p: FlatPop) {
  const f = p.filter
  const conds = f?.conditions?.length ? f.conditions
    : (f ? [{ measure: f.measure, fun: f.fun, values: f.values }] : [])
  fpEditPath.value = p.path; fpName.value = p.name; fpParent.value = p.parent; fpColour.value = p.colour
  fpConds.value = conds.length
    ? conds.map(c => ({ measure: String(c.measure ?? ''), fun: String(c.fun ?? 'gt'),
        values: Array.isArray(c.values) ? c.values.join(', ') : (c.values == null ? '' : String(c.values)) }))
    : [{ measure: '', fun: 'gt', values: '' }]
  showFilterForm.value = true
}

async function submitFilterPop() {
  const name = fpName.value.trim()
  const conds = fpConds.value.filter(c => c.measure)
    .map(c => ({ measure: c.measure, fun: c.fun, values: parseFilterValues(c.fun, c.values) }))
  if (!conds.length) return
  const cur = fpEditPath.value ? visiblePops.value.find(p => p.path === fpEditPath.value) : undefined
  // reserved-prefix + duplicate (same list) checked here; cross-pop-type collision → server toast
  const nameErr = popNameError(name, g.flat.map(x => x.name), { currentName: cur?.name })
  if (nameErr) { log.error(nameErr, { source: 'gating' }); return }
  if (fpEditPath.value) {
    const path = fpEditPath.value
    await g.updateFilterPop(path, conds)                                   // conditions
    if (cur && cur.colour !== fpColour.value) await g.updatePop(path, { colour: fpColour.value })  // colour
    if (cur && name !== cur.name) await g.renamePop(path, name)            // rename LAST (path changes)
  } else {
    await g.addFilterPop(name, fpParent.value, fpColour.value, conds)
  }
  resetFilterForm(); showFilterForm.value = false
}

// a hand-drawn gate vs a declarative filter pop (badge in the list)
const isFilterPop = (p: FlatPop) => !!p.filter && !p.gate
const popFilterSummary = (p: FlatPop) => filterSummary(p.filter, g.colLabel)
</script>

<template>
  <PopulationPanelShell :count="visiblePops.length" :scope="scope" :vis="vis" :docked="docked"
                        @update:scope="emit('update:scope', $event)" @update:vis="emit('update:vis', $event)">
    <!-- ── population list (default slot) ── -->
      <!-- cluster mode: pops are made here (no gate to draw), then clusters ticked into them -->
      <div v-if="clusterMode && !readonly" class="pm-add">
        <button class="pm-add-btn" @click="addClusterPopulation"
                v-tooltip.bottom="'Create a population, then tick cluster IDs into it'">
          <i class="pi pi-plus" /> Add population
        </button>
      </div>

      <!-- filter-population form (Decision 15): a pop defined by an AND-ed filter on any obs measure.
           Same form creates AND edits (fpEditPath) — editing is not a special path. -->
      <div v-if="!readonly && !clusterMode" class="pm-add">
        <button class="pm-add-btn" @click="showFilterForm ? (showFilterForm = false) : openCreateFilter()"
                v-tooltip.bottom="'Define a population by filtering on obs measures (region, cluster, aggregate, intensity, speed…)'">
          <i class="pi pi-filter" /> New filter population
        </button>
      </div>
      <div v-if="showFilterForm && !readonly && !clusterMode" class="pm-ff">
        <div class="pm-ff-title">{{ fpEditPath ? 'Edit filter population' : 'New filter population' }}</div>
        <div class="pm-ff-head">
          <input v-model="fpName" class="pm-ff-name" placeholder="Population name" />
          <input v-model="fpColour" type="color" class="pm-ff-colour" v-tooltip.top="'Colour'" />
        </div>
        <label class="pm-ff-row">Under
          <select v-model="fpParent" :disabled="!!fpEditPath" v-tooltip.top="fpEditPath ? 'Parent is fixed when editing — delete & recreate to move' : ''">
            <option v-for="o in parentOptions" :key="o" :value="o">{{ o === 'root' ? '(all cells)' : o }}</option>
          </select>
        </label>
        <div v-for="(c, i) in fpConds" :key="i" class="pm-ff-cond">
          <select v-model="c.measure" class="pm-ff-measure">
            <option value="" disabled>measure…</option>
            <option v-for="m in filterMeasures" :key="m" :value="m">{{ g.colLabel(m) }}</option>
          </select>
          <select v-model="c.fun" class="pm-ff-fun">
            <option v-for="f in FILTER_FUNS" :key="f" :value="f">{{ f }}</option>
          </select>
          <input v-model="c.values" class="pm-ff-vals" :placeholder="c.fun === 'in' ? 'a, b, c' : 'value'" />
          <button v-if="fpConds.length > 1" class="pm-icon" @click="removeFpCond(i)" v-tooltip.left="'Remove condition'">
            <i class="pi pi-times" />
          </button>
        </div>
        <div class="pm-ff-actions">
          <button class="pm-ff-cond-add" @click="addFpCond"><i class="pi pi-plus" /> AND condition</button>
          <span class="pm-ff-spacer" />
          <button class="pm-ff-cancel" @click="showFilterForm = false; resetFilterForm()">Cancel</button>
          <button class="pm-add-btn" :disabled="!fpName.trim() || !fpConds.some(c => c.measure)"
                  @click="submitFilterPop">{{ fpEditPath ? 'Save' : 'Create' }}</button>
        </div>
      </div>

      <div v-if="!visiblePops.length" class="pm-empty cc-muted">
        {{ clusterMode ? 'No populations yet — add one, then tick clusters into it.' : 'No populations yet — draw a gate.' }}
      </div>

      <template v-for="p in visiblePops" :key="p.path">
        <div class="pm-row" :class="{ active: p.path === props.selected, transient: p.transient }"
             :style="{ paddingLeft: 6 + p.depth * 14 + 'px' }"
             @click="pick(p)">
          <i v-if="p.transient" class="pi pi-map-marker pm-napari"
             v-tooltip.left="'Cells selected in napari (temporary)'" :style="{ color: p.colour }" />
          <button v-else type="button" class="pm-swatch" :style="{ background: p.colour }" :disabled="readonly"
                  v-tooltip.left="readonly ? '' : 'Colour'"
                  @click.stop="openColour(p, $event)" />

          <span v-if="editing !== p.path" class="pm-name"
                @dblclick.stop="!readonly && !p.transient && beginRename(p)">{{ p.name }}</span>
          <input v-else class="pm-rename" v-model="editName" autofocus
                 @keyup.enter="commitRename(p)" @blur="commitRename(p)" @click.stop />

          <!-- filter pops are badged (vs hand-drawn gates); the badge is the EDIT affordance (click →
               open the same form pre-filled). Read-only surfaces show a static badge. Tooltip = predicate. -->
          <button v-if="isFilterPop(p) && !readonly && !p.transient" type="button"
                  class="pm-icon pm-filter-badge" v-tooltip.top="`Edit: ${popFilterSummary(p)}`"
                  @click.stop="beginEditFilter(p)"><i class="pi pi-filter" /></button>
          <i v-else-if="isFilterPop(p)" class="pi pi-filter pm-filter-badge"
             v-tooltip.top="popFilterSummary(p)" />

          <span class="pm-stat" v-tooltip.left="'cells · % of parent'">
            {{ g.stats[p.path]?.count ?? '–' }}
            <small>{{ fmtPct(g.stats[p.path]?.pctParent) }}</small>
          </span>

          <button v-if="p.gate" class="pm-icon" v-tooltip.left="'Show the plot where this gate was drawn'"
                  @click.stop="emit('showDefiningPlot', p)">
            <i class="pi pi-search" />
          </button>
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
          <ConfirmDeleteButton v-if="!p.transient && !readonly"
                  title="Delete population" armed-title="Click again to delete this population"
                  @confirm="g.deletePop(p.path)" />
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

      <!-- pop colour picker popover: Cecelia palette chips + a native picker for custom colours -->
      <TeleportPopover v-model="colourOpen" :anchor="colourAnchor" placement="bottom-start">
        <div class="pm-colours">
          <div class="pm-colours-grid">
            <button v-for="c in CECELIA_PALETTE" :key="c" type="button" class="pm-colour-chip"
                    :class="{ on: eqColour(c, colourPopColour) }" :style="{ background: c }"
                    v-tooltip.top="c" @click="setColour(c)" />
          </div>
          <label class="pm-colour-custom">
            <span>custom</span>
            <input type="color" :value="colourPopColour"
                   @change="setColour(($event.target as HTMLInputElement).value, false)" />
          </label>
        </div>
      </TeleportPopover>

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
            <ChipSelect class="pm-seg" variant="segmented" :options="AXIS_OPTIONS"
                        :model-value="axisFromZero ? 'zero' : 'auto'" aria-label="Axis scale"
                        @update:model-value="v => emit('update:axisFromZero', v === 'zero')" />
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
              <input type="range" min="1" max="20" step="1" :value="napariPointSize"
                     v-tooltip.top="'Population point size in napari (per experiment/set)'"
                     @input="napariPointSize = parseInt(($event.target as HTMLInputElement).value)"
                     @change="g.refreshNapariPops()" />
              <span class="pm-opt-val">{{ napariPointSize }}</span>
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
.pm-empty { padding: 12px; }   /* + .cc-muted */
.pm-row { display: flex; align-items: center; gap: 6px; padding: 4px 8px 4px 6px; cursor: pointer; border-bottom: 1px solid var(--cc-border); }
.pm-row:hover { background: var(--cc-surface-2); }
.pm-row.active { background: color-mix(in srgb, var(--cc-accent) 22%, transparent); }
.pm-row.transient { font-style: italic; background: color-mix(in srgb, #22d3ee 8%, transparent); }
.pm-napari { width: 16px; text-align: center; font-size: 13px; }
.pm-swatch { width: 16px; height: 16px; padding: 0; border: 1px solid var(--cc-border); border-radius: 3px; cursor: pointer; flex-shrink: 0; }
.pm-swatch:disabled { cursor: default; opacity: 0.7; }
/* colour picker popover (Cecelia palette + native custom) — TeleportPopover gives surface/border */
.pm-colours { display: flex; flex-direction: column; gap: 8px; padding: 8px; }
.pm-colours-grid { display: grid; grid-template-columns: repeat(4, 1fr); gap: 5px; }
.pm-colour-chip { width: 22px; height: 22px; border: 1px solid var(--cc-border); border-radius: 4px; cursor: pointer; padding: 0; }
.pm-colour-chip:hover { transform: scale(1.08); }
.pm-colour-chip.on { outline: 2px solid var(--cc-text); outline-offset: 1px; }
.pm-colour-custom { display: flex; align-items: center; justify-content: space-between; gap: 8px;
  font-size: 12px; color: var(--cc-text-dim); border-top: 1px solid var(--cc-border); padding-top: 6px; }
.pm-colour-custom input { width: 28px; height: 20px; padding: 0; border: none; background: none; cursor: pointer; }
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
.pm-add-btn:disabled { opacity: 0.5; cursor: not-allowed; }
/* filter-population form (Decision 15) */
.pm-ff { display: flex; flex-direction: column; gap: 5px; padding: 6px 8px; border-bottom: 1px solid var(--cc-border);
  background: var(--cc-surface-1); }
.pm-ff-head { display: flex; gap: 5px; align-items: center; }
.pm-ff-name { flex: 1; font-size: 11px; padding: 3px 6px; border: 1px solid var(--cc-border); border-radius: 4px;
  background: var(--cc-surface-2); color: var(--cc-text); }
.pm-ff-colour { width: 24px; height: 24px; padding: 0; border: 1px solid var(--cc-border); border-radius: 4px;
  background: none; cursor: pointer; }
.pm-ff-row, .pm-ff-cond { display: flex; gap: 4px; align-items: center; font-size: 11px; color: var(--cc-text-dim); }
.pm-ff-cond select, .pm-ff-row select, .pm-ff-vals { font-size: 11px; padding: 2px 4px; border: 1px solid var(--cc-border);
  border-radius: 3px; background: var(--cc-surface-2); color: var(--cc-text); }
.pm-ff-measure { flex: 1; min-width: 0; }
.pm-ff-fun { width: 48px; }
.pm-ff-vals { width: 64px; }
.pm-ff-actions { display: flex; justify-content: space-between; align-items: center; }
.pm-ff-cond-add { background: none; border: none; color: var(--cc-text-dim); font-size: 11px; cursor: pointer; padding: 2px; }
.pm-ff-cond-add:hover { color: var(--cc-text); }
.pm-ff-title { font-size: 11px; font-weight: 600; color: var(--cc-text); }
.pm-ff-spacer { flex: 1; }
.pm-ff-cancel { background: none; border: none; color: var(--cc-text-dim); font-size: 11px; cursor: pointer; padding: 4px 6px; }
.pm-ff-cancel:hover { color: var(--cc-text); }
.pm-filter-badge { font-size: 10px; color: #8b5cf6; margin-left: 2px; opacity: 0.8; }
button.pm-filter-badge { border: none; background: none; cursor: pointer; padding: 2px; }
button.pm-filter-badge:hover { opacity: 1; }
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
.pm-seg { margin-left: auto; }
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
