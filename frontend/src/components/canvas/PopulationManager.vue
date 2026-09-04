<!--
  Floating, collapsible population manager — shared across canvases (gating today; track-gating and
  the universal canvas next). Flat list (indented by depth): colour swatch, name (rename inline),
  count + %parent, an EYE that toggles whether the pop is colour-highlighted on the plots, viewer
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
import { ref, computed, onMounted, onBeforeUnmount } from 'vue'
import { useGatingStore, type FlatPop } from '../../stores/gating'
import { useLogStore } from '../../stores/log'
import { useProjectStore } from '../../stores/project'
import { useSettingsStore } from '../../stores/settings'
import CanvasSidePanel from './CanvasSidePanel.vue'
import ConfirmButton from '../ConfirmButton.vue'
import TeleportPopover from '../TeleportPopover.vue'
import ColourPicker from '../ColourPicker.vue'
import { parseFilterValues, filterSummary } from '../../utils/filterPopForm'
import { termsToSpec, specToTerms, booleanSpecValid, booleanSummary,
         type BooleanTerm } from '../../utils/booleanPopForm'
import { popNameError, popPath, isInSubtree } from '../../utils/popName'
import { useInlineEdit } from '../../composables/useInlineEdit'
// `PALETTES` is gone from here on purpose: the swatch is `ColourPicker` now, which owns the default
// palette. Only `DOT_R` is still needed, for the gate colour-by legend main added.
import { type VisProps } from '../../plots/plot'
import { DOT_R } from '../../plots/density'
import { clusterMeasure, isClusterPopType, isGatingPopType } from '../../utils/clusterMeasure'
import { isTypingTarget } from '../../utils/typingTarget'
import { measureGroups, groupedCols } from '../../utils/measureGroups'
import { convertGateKind, otherGateKind } from '../../plots/gateGeometry'
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
  dotSize?: number                 // scatter dot radius (px) — the plot-side twin of "Viewer dots" below
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
  'update:dotSize': [number]
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

// viewer point size is a PER-SET viewer preference (keyed by the gated image's set), set once and
// held across the set's images. Guard the setter so we never write under an empty set key.
const viewerSetUid = computed(() => (g.imageUid ? projectStore.setUidOfImage(g.imageUid) : null) ?? '')
const viewerPointSize = computed<number>({
  get: () => settings.getPointSize(viewerSetUid.value),
  set: v => { if (viewerSetUid.value) settings.setPointSize(viewerSetUid.value, v) },
})

const optionsOpen = ref(false)     // gate / viewer options box (host-specific, in the shell #options slot)
// edit-in-place, shared with the model vault and the tables (composables/useInlineEdit). Adopting it
// also fixed a real bug here: `@keyup.enter` and `@blur` both called `commitRename` straight through,
// so Enter renamed and the blur it caused ran the whole thing again.
const { draft: editName, isEditing, start: beginRenameAt, cancel: cancelRename, commit,
        focusInput } = useInlineEdit()
const beginRename = (p: FlatPop) => beginRenameAt(p.path, p.name)

// Pop colours use the shared `ColourPicker` (components/ColourPicker.vue), extracted from here when the
// browser volume viewer needed the same control. It owns the popover, the palette and the anchor, so the
// `colourPop`/`colourAnchor` bookkeeping this file used to carry is gone.

function pick(p: FlatPop) { emit('update:selected', p.path) }
const commitRename = (p: FlatPop) => commit(p.path, p.name, async name => {
  // reserved-prefix + same-list duplicate are caught here for instant feedback; a cross-pop-type
  // collision is rejected by the server (pop_name_conflict) and surfaced via the store's error toast.
  const err = popNameError(name, g.flat.map(x => x.name), { currentName: p.name })
  if (err) { log.error(err, { source: 'gating' }); return }
  if (name) await g.renamePop(p.path, name)
})
const isLit = (p: FlatPop) => props.highlighted.includes(p.path)
const fmtPct = (v?: number) => v == null ? '' : `${v.toFixed(1)}%`

// Per-pop visibility: flip the persisted `show` flag, then ping the WebGPU viewer so it re-derives
// the overlay set from the shared bag. The viewer routes to the right overlay for the popType
// (Tracks layers vs Points) internally.
async function toggleViewer(p: FlatPop) {
  await g.updatePop(p.path, { show: !p.show })
  g.refreshOverlays()
}

// ── Undo / redo ───────────────────────────────────────────────────────────────
// Hand-drawn gating only (flow = cells, track = tracks). A cluster/region/filter pop's edit is a
// tick you can un-tick, so it needs no history; a gate you dragged or a population you deleted with
// its children is work you cannot get back. History itself lives on the server — see the gating
// store — so these two flags are its answer, not a local guess.
// Read-only surfaces (the Analysis board) get nothing: they cannot mutate in the first place.
const historyMode = computed(() => !props.readonly && isGatingPopType(props.popType))
// Ctrl/⌘+Z and Ctrl/⌘+Shift+Z, the bindings every user already has in their fingers. Bound to the
// WINDOW, so it works with focus on the plot canvas where the gate was just dragged — hence the
// typing guard, or "z" in a rename field would undo the rename you are in the middle of typing.
function onKey(e: KeyboardEvent) {
  if (!historyMode.value || isTypingTarget(e)) return
  if (!(e.ctrlKey || e.metaKey) || e.key.toLowerCase() !== 'z') return
  e.preventDefault()
  if (e.shiftKey) { if (g.canRedo) g.redo() }
  else if (g.canUndo) g.undo()
}
onMounted(() => window.addEventListener('keydown', onKey))
onBeforeUnmount(() => window.removeEventListener('keydown', onKey))

// ── Change a gate's SHAPE in place: rectangle ⇄ polygon ───────────────────────────────────────
// The POPULATION survives — same name, colour, children, place in the tree; only the geometry is
// rewritten (`pop/set-gate` → Julia `set_gate!`, which re-derives membership for it and everything
// below it). Before this the only way to turn a rectangle into a polygon was to delete the pop and
// redraw it, which took its children with it.
// rect → poly is LOSSLESS (the four corners, same region, same cells) so it fires on the first
// click; poly → rect is the vertices' BOUNDING BOX, which can only widen the gate, so that
// direction arms first (ConfirmButton `needs-confirm`). Geometry lives in gateGeometry.ts.
const convertTo = (p: FlatPop) => p.gate ? otherGateKind(p.gate.kind) : null
const convertIcon = (p: FlatPop) => convertTo(p) === 'polygon' ? 'pi pi-share-alt' : 'pi pi-stop'
// SHORT on purpose. This panel is ~250px and the button sits at the LEFT of the icon cluster, so a
// wide `.left` tooltip cannot fit beside it: PrimeVue's align() falls through left → top → bottom and
// drops it onto the row below, hiding the controls there (docs/ui/COPY.md — a tooltip that covers the
// thing you were about to click). "Rectangle" already implies the bounding box; the widening is in
// docs/POPULATION.md, not in hover help.
const convertTip = (p: FlatPop) =>
  convertTo(p) === 'polygon' ? 'Convert to polygon' : 'Convert to rectangle'
async function convertGate(p: FlatPop) {
  const gate = p.gate && convertGateKind(p.gate)
  if (!gate) { log.error('This gate has no geometry to convert', { source: 'gating' }); return }
  await g.setGate(p.path, gate)
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
// measures: per-cell obs (regions/clusters/aggregate/hmm/speed…) + gateable var columns (intensities),
// headed by family (utils/measureGroups.ts — the same grouping the gate axis pickers use). Sorted
// WITHIN each family: one alphabetical run put `area` next to `cell_id` next to `mean_intensity_0`.
const filterGroups = computed(() => measureGroups({
  columns: [...g.columns].sort(), channels: g.channels,
  obsColumns: [...g.obsColumns].sort(), popType: g.popType }))
const filterMeasures = computed(() => groupedCols(filterGroups.value))
// Parent choices for the pop-defining forms (filter + combination). When EDITING, the pop's own
// subtree is excluded — moving a population under its own descendant is a cycle (Julia `move_pop!`
// rejects it; this never offers it). One helper, because both forms need exactly this list.
const parentChoices = (exclude: string | null) => ['root', ...visiblePops.value
  .filter(p => !p.transient && (!exclude || !isInSubtree(p.path, exclude)))
  .map(p => p.path)]
const parentOptions = computed(() => parentChoices(fpEditPath.value))

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
    let path = fpEditPath.value
    await g.updateFilterPop(path, conds)                                   // conditions
    if (cur && cur.colour !== fpColour.value) await g.updatePop(path, { colour: fpColour.value })  // colour
    // rename and move BOTH change the path, so they go last and each one re-derives it for the next
    if (cur && name !== cur.name) { await g.renamePop(path, name); path = popPath(cur.parent, name) }
    if (cur && fpParent.value !== cur.parent) await g.movePop(path, fpParent.value)
  } else {
    await g.addFilterPop(name, fpParent.value, fpColour.value, conds)
  }
  resetFilterForm(); showFilterForm.value = false
}

// a hand-drawn gate vs a declarative filter pop (badge in the list)
const isFilterPop = (p: FlatPop) => !!p.filter && !p.gate
const popFilterSummary = (p: FlatPop) => filterSummary(p.filter, g.colLabel)

// ── Combined (boolean) populations (Decision 16) ──────────────────────────────────────────────
// A population defined by COMBINING others rather than by a shape on a plot: "positive for nuc-GFP
// or mem-TOM", "double positive but not CD169". No single 2D gate can express either — before this
// the answer was to draw one gate and re-gate its children by hand. Same form creates and edits, as
// the filter form does; each term carries its own is / is not.
const showBoolForm = ref(false)
const bpEditPath = ref<string | null>(null)     // null → creating; a path → editing that pop
const bpName = ref('')
const bpParent = ref('root')
const bpColour = ref(POP_PALETTE[0])
const bpOp = ref<'and' | 'or'>('or')
const bpTerms = ref<BooleanTerm[]>([{ path: '', negate: false }])
// What can be combined: any real population except the one being edited and its own subtree (which
// depends on it, so combining it would be a loop the server rejects) and the transient viewer pop.
const boolTermOptions = computed(() => visiblePops.value
  .filter(p => !p.transient && (!bpEditPath.value || !isInSubtree(p.path, bpEditPath.value)))
  .map(p => p.path))
const boolParentOptions = computed(() => parentChoices(bpEditPath.value))
// a path → the population's own name, for the summary line (paths get long and the leaf is the name)
const popLabel = (path: string) => g.flat.find(x => x.path === path)?.name ?? path

function addBoolTerm() { bpTerms.value.push({ path: '', negate: false }) }
function removeBoolTerm(i: number) { bpTerms.value.splice(i, 1) }
function resetBoolForm() {
  bpEditPath.value = null; bpName.value = ''; bpParent.value = 'root'
  bpColour.value = POP_PALETTE[0]; bpOp.value = 'or'; bpTerms.value = [{ path: '', negate: false }]
}
// `seed` = the ⋯ → "Combine with…" entry: the row you opened the menu on becomes the first term and
// its parent the new pop's parent, so the common case is two clicks and a second population.
function openCreateBool(seed?: FlatPop) {
  resetBoolForm()
  if (seed) {
    bpTerms.value = [{ path: seed.path, negate: false }, { path: '', negate: false }]
    bpParent.value = seed.parent
  }
  showBoolForm.value = true
}
function beginEditBool(p: FlatPop) {
  bpEditPath.value = p.path; bpName.value = p.name; bpParent.value = p.parent; bpColour.value = p.colour
  bpOp.value = p.boolean?.op === 'and' ? 'and' : 'or'
  const terms = specToTerms(p.boolean)
  bpTerms.value = terms.length ? terms : [{ path: '', negate: false }]
  showBoolForm.value = true
}

async function submitBoolPop() {
  const name = bpName.value.trim()
  const spec = termsToSpec(bpOp.value, bpTerms.value)
  if (!booleanSpecValid(spec)) return
  const cur = bpEditPath.value ? visiblePops.value.find(p => p.path === bpEditPath.value) : undefined
  const nameErr = popNameError(name, g.flat.map(x => x.name), { currentName: cur?.name })
  if (nameErr) { log.error(nameErr, { source: 'gating' }); return }
  if (bpEditPath.value) {
    let path = bpEditPath.value
    await g.updateBooleanPop(path, spec)
    if (cur && cur.colour !== bpColour.value) await g.updatePop(path, { colour: bpColour.value })
    // rename and move BOTH change the path, so they go last and each re-derives it for the next —
    // and the server rewrites every reference to this pop along with it.
    if (cur && name !== cur.name) { await g.renamePop(path, name); path = popPath(cur.parent, name) }
    if (cur && bpParent.value !== cur.parent) await g.movePop(path, bpParent.value)
  } else {
    await g.addBooleanPop(name, bpParent.value, bpColour.value, spec)
  }
  resetBoolForm(); showBoolForm.value = false
}

const isBoolPop = (p: FlatPop) => !!p.boolean
const popBoolSummary = (p: FlatPop) => booleanSummary(p.boolean, popLabel)

// ── Row actions overflow menu (⋯) ────────────────────────────────────────────────────────────
// Same shape as the image table's per-image menu (TeleportPopover + the shared `.cc-actions-*`
// utilities in style.css). The row keeps only what the user toggles WHILE reading it — colour,
// highlight, viewer visibility — and everything episodic (convert the gate, open its plot, re-parent,
// delete) moves in here. This panel is ~250px wide, so each icon added to a row came straight out of
// the name and the count; the actions were also outgrowing what fits beside them.
const actionsPath   = ref<string | null>(null)
const actionsAnchor = ref<HTMLElement | null>(null)   // the clicked ⋯ button (drives placement)
const actionsPop    = computed(() => actionsPath.value
  ? (visiblePops.value.find(p => p.path === actionsPath.value) ?? null) : null)
const actionsOpen   = computed({ get: () => actionsPath.value !== null,
                                 set: v => { if (!v) closeActions() } })
function closeActions() { actionsPath.value = null; moveMode.value = false }
function openActions(p: FlatPop, e: MouseEvent) {
  if (actionsPath.value === p.path) { closeActions(); return }
  actionsAnchor.value = e.currentTarget as HTMLElement
  moveMode.value = false
  actionsPath.value = p.path
}
// run the chosen action, THEN close — the item closures read `actionsPop`, which is derived from
// `actionsPath`, so closing first would null it out from under them (the ImageTable menu's ordering).
function runAction(fn: () => void) { fn(); closeActions() }
// how many populations sit BELOW this one — what "delete" takes with it, and what "delete children"
// removes on its own.
const childCount = (p: FlatPop) => g.flat.filter(x => x.path.startsWith(p.path + '/')).length

// ── Move (re-parent) ──────────────────────────────────────────────────────────────────────────
// A pop's cells are its own gate ∩ its parent's, so re-parenting RE-DERIVES membership: lifting a
// population out of a QC gate re-evaluates it against all cells, keeping the gate, the children and
// the colours. Before this the only way there was to delete and redraw the whole branch.
// "Move under…" reuses the SAME popover rather than opening a second one: the choice is a list of
// populations, which is exactly what a menu renders.
const moveMode = ref(false)
const moveTargets = computed<string[]>(() => {
  const p = actionsPop.value
  if (!p) return []
  // every pop except the one being moved, its own subtree (that would be a cycle), the transient
  // viewer selection (never persisted, so it can't parent anything) and its current parent.
  return ['root', ...visiblePops.value
    .filter(t => !isInSubtree(t.path, p.path) && !t.transient)
    .map(t => t.path)].filter(t => t !== p.parent)
})
function moveTo(target: string) {
  const p = actionsPop.value
  closeActions()
  if (p) g.movePop(p.path, target)
}
</script>

<template>
  <CanvasSidePanel :count="visiblePops.length" :scope="scope" :vis="vis" :docked="docked"
                        @update:scope="emit('update:scope', $event)" @update:vis="emit('update:vis', $event)">
    <!-- ── population list (default slot) ── -->
      <!-- cluster mode: pops are made here (no gate to draw), then clusters ticked into them -->
      <div v-if="clusterMode && !readonly" class="pm-add cc-row cc-row-tight">
        <button class="pm-add-btn" data-guide="popmanager.addClusterPop" @click="addClusterPopulation"
                v-tooltip.bottom="'Create a population, then tick cluster IDs into it'">
          <i class="pi pi-plus" /> Add population
        </button>
      </div>

      <!-- Filter + Combine + history in one row, `+[icon]` compact form — matches `+ Plot` / `+ Pairs`
           in the gating toolbar (Dominik, 2026-08-26). Two labelled rows was a whole extra bar of
           height for a control the user reaches for once per pop, and the icons are what a repeat
           user reads for anyway (pi-filter = filter pop, pi-link = combined pop).
           Decision 15: filter pop = AND-ed filter on any obs measure. Same form creates AND edits.
           Decision 16: combined pop = "nuc-GFP OR mem-TOM", which no single 2D gate can draw.  -->
      <div v-if="!readonly && !clusterMode" class="pm-add cc-row cc-row-tight">
        <button class="pm-add-icon cc-btn cc-btn-primary"
                @click="showFilterForm ? (showFilterForm = false) : openCreateFilter()"
                v-tooltip.bottom="'New filter population'" aria-label="New filter population">
          <i class="pi pi-plus" /><i class="pi pi-filter" />
        </button>
        <button class="pm-add-icon cc-btn cc-btn-primary"
                @click="showBoolForm ? (showBoolForm = false) : openCreateBool()"
                v-tooltip.bottom="'Combine populations'" aria-label="Combine populations">
          <i class="pi pi-plus" /><i class="pi pi-link" />
        </button>
        <!-- Undo/redo for hand-drawn gating. In this bar rather than a bar of their own: the panel is
             the document these act on. -->
        <template v-if="historyMode">
          <span class="pm-add-spacer" />
          <button class="pm-icon cc-btn cc-btn-bare cc-btn-icon" :disabled="!g.canUndo"
                  v-tooltip.bottom="'Undo (Ctrl+Z)'" @click="g.undo()">
            <i class="pi pi-undo" />
          </button>
          <button class="pm-icon pm-redo cc-btn cc-btn-bare cc-btn-icon" :disabled="!g.canRedo"
                  v-tooltip.bottom="'Redo (Ctrl+Shift+Z)'" @click="g.redo()">
            <i class="pi pi-undo" />
          </button>
        </template>
      </div>
      <div v-if="showFilterForm && !readonly && !clusterMode" class="pm-ff">
        <div class="pm-ff-title">{{ fpEditPath ? 'Edit filter population' : 'New filter population' }}</div>
        <div class="pm-ff-head">
          <input v-model="fpName" class="pm-ff-name" placeholder="Population name"
                 v-tooltip.top="'Name for the new population'" />
          <input v-model="fpColour" type="color" class="pm-ff-colour" v-tooltip.top="'Colour'" />
        </div>
        <label class="pm-ff-row cc-muted cc-fs-xs">Under
          <select v-model="fpParent" v-tooltip.top="'Parent population — its cells are the ones this filters'">
            <option v-for="o in parentOptions" :key="o" :value="o">{{ o === 'root' ? '(all cells)' : o }}</option>
          </select>
        </label>
        <div v-for="(c, i) in fpConds" :key="i" class="pm-ff-cond cc-muted cc-fs-xs">
          <select v-model="c.measure" class="pm-ff-measure" v-tooltip.top="'Measure this condition filters on'">
            <option value="" disabled>measure…</option>
            <optgroup v-for="grp in filterGroups" :key="grp.title" :label="grp.title">
              <option v-for="m in grp.cols" :key="m" :value="m">{{ g.colLabel(m) }}</option>
            </optgroup>
          </select>
          <select v-model="c.fun" class="pm-ff-fun" v-tooltip.top="'How the measure is compared'">
            <option v-for="f in FILTER_FUNS" :key="f" :value="f">{{ f }}</option>
          </select>
          <input v-model="c.values" class="pm-ff-vals" :placeholder="c.fun === 'in' ? 'a, b, c' : 'value'"
                 v-tooltip.top="'Value to compare against; comma-separated for in'" />
          <button v-if="fpConds.length > 1" class="pm-icon cc-btn cc-btn-bare cc-btn-icon" @click="removeFpCond(i)" v-tooltip.left="'Remove condition'">
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

      <!-- combined-population form (Decision 16): pick the populations, say how they combine, and
           mark any that must NOT be in it. Same form creates and edits (bpEditPath). -->
      <div v-if="showBoolForm && !readonly && !clusterMode" class="pm-ff">
        <div class="pm-ff-title">{{ bpEditPath ? 'Edit combined population' : 'New combined population' }}</div>
        <div class="pm-ff-head">
          <input v-model="bpName" class="pm-ff-name" placeholder="Population name"
                 v-tooltip.top="'Name for the new population'" />
          <input v-model="bpColour" type="color" class="pm-ff-colour" v-tooltip.top="'Colour'" />
        </div>
        <!-- parent + operator on ONE line: two short selects, and the sentence reads across them
             ("Under (all cells), in any of"). `cc-row-group` keeps each label with its own select if
             a long parent path pushes the pair onto a second line. -->
        <div class="cc-row cc-row-tight cc-muted cc-fs-xs">
          <label class="cc-row-group pm-ff-parent">Under
            <select v-model="bpParent"
                    v-tooltip.top="'Parent population — the cells this combination is drawn from'">
              <option v-for="o in boolParentOptions" :key="o" :value="o">{{ o === 'root' ? '(all cells)' : o }}</option>
            </select>
          </label>
          <label class="cc-row-group">, in
            <select v-model="bpOp" v-tooltip.top="'How the included populations combine'">
              <option value="or">any of</option>
              <option value="and">all of</option>
            </select>
          </label>
        </div>
        <div v-for="(t, i) in bpTerms" :key="i" class="pm-ff-cond cc-muted cc-fs-xs">
          <select v-model="t.negate" class="pm-ff-neg" v-tooltip.top="'Must be in it, or must not'">
            <option :value="false">is</option>
            <option :value="true">is not</option>
          </select>
          <select v-model="t.path" class="pm-ff-measure" v-tooltip.top="'Population to combine'">
            <option value="" disabled>population…</option>
            <option v-for="o in boolTermOptions" :key="o" :value="o">{{ o }}</option>
          </select>
          <button v-if="bpTerms.length > 1" class="pm-icon cc-btn cc-btn-bare cc-btn-icon"
                  @click="removeBoolTerm(i)" v-tooltip.left="'Remove'">
            <i class="pi pi-times" />
          </button>
        </div>
        <div class="pm-ff-actions">
          <button class="pm-ff-cond-add" @click="addBoolTerm"><i class="pi pi-plus" /> population</button>
          <span class="pm-ff-spacer" />
          <button class="pm-ff-cancel" @click="showBoolForm = false; resetBoolForm()">Cancel</button>
          <button class="pm-add-btn" :disabled="!bpName.trim() || !bpTerms.some(t => t.path)"
                  @click="submitBoolPop">{{ bpEditPath ? 'Save' : 'Create' }}</button>
        </div>
      </div>

      <div v-if="!visiblePops.length" class="pm-empty cc-muted">
        {{ clusterMode ? 'No populations yet — add one, then tick clusters into it.' : 'No populations yet — draw a gate.' }}
      </div>

      <template v-for="p in visiblePops" :key="p.path">
        <div class="pm-row" data-guide="popmanager.row"
             :class="{ active: p.path === props.selected, transient: p.transient }"
             :style="{ paddingLeft: 6 + p.depth * 14 + 'px' }"
             @click="pick(p)">
          <!-- `.bottom`, not `.left`: this marker is the row's LEFTMOST element, so there is never room
               beside it and PrimeVue drops the tip somewhere it covers a pop row. The injected viewer
               pop is always the last root child, so below it is the panel edge, not a control. The row
               already reads "Viewer selection" — the tip carries only what the label can't. -->
          <i v-if="p.transient" class="pi pi-map-marker pm-viewer"
             v-tooltip.bottom="'Temporary — not saved'" :style="{ color: p.colour }" />
          <ColourPicker
            v-else :model-value="p.colour" :disabled="readonly || p.transient"
            @update:model-value="c => g.updatePop(p.path, { colour: c })"
          />

          <span v-if="!isEditing(p.path)" class="pm-name"
                @dblclick.stop="!readonly && !p.transient && beginRename(p)">{{ p.name }}</span>
          <input v-else class="pm-rename" v-model="editName" :ref="focusInput"
                 v-tooltip.top="'Enter to save the new name'"
                 @keyup.enter="commitRename(p)" @keyup.esc="cancelRename"
                 @blur="commitRename(p)" @click.stop />

          <!-- filter pops are badged (vs hand-drawn gates); the badge is the EDIT affordance (click →
               open the same form pre-filled). Read-only surfaces show a static badge. Tooltip = predicate. -->
          <button v-if="isFilterPop(p) && !readonly && !p.transient" type="button"
                  class="pm-icon cc-btn cc-btn-bare cc-btn-icon pm-filter-badge" v-tooltip.top="`Edit: ${popFilterSummary(p)}`"
                  @click.stop="beginEditFilter(p)"><i class="pi pi-filter" /></button>
          <i v-else-if="isFilterPop(p)" class="pi pi-filter pm-filter-badge"
             v-tooltip.top="popFilterSummary(p)" />

          <!-- combined pops carry the same kind of badge, and it is likewise the EDIT affordance —
               the tooltip is the combination itself, which is the only way to read it from the list. -->
          <button v-if="isBoolPop(p) && !readonly && !p.transient" type="button"
                  class="pm-icon cc-btn cc-btn-bare cc-btn-icon pm-filter-badge pm-bool-badge"
                  v-tooltip.top="`Edit: ${popBoolSummary(p)}`"
                  @click.stop="beginEditBool(p)"><i class="pi pi-link" /></button>
          <i v-else-if="isBoolPop(p)" class="pi pi-link pm-filter-badge pm-bool-badge"
             v-tooltip.top="popBoolSummary(p)" />

          <span class="pm-stat" v-tooltip.left="'cells · % of parent'">
            {{ g.stats[p.path]?.count ?? '–' }}
            <small>{{ fmtPct(g.stats[p.path]?.pctParent) }}</small>
          </span>

          <!-- these `.left` tips are kept SHORT: this panel is ~250px, so a wide tooltip can't fit
               beside an icon and PrimeVue drops it onto the row below (docs/ui/COPY.md). -->
          <button class="pm-icon cc-btn cc-btn-bare cc-btn-icon" :class="{ lit: isLit(p) }"
                  v-tooltip.left="isLit(p) ? 'Hide colour on plots' : 'Highlight colour on plots'"
                  @click.stop="emit('toggleHighlight', p.path)">
            <i :class="isLit(p) ? 'pi pi-eye' : 'pi pi-eye-slash'" />
          </button>
          <button v-if="!p.transient" class="pm-icon cc-btn cc-btn-bare cc-btn-icon" :class="{ lit: p.show }"
                  v-tooltip.left="p.show ? 'Hide in viewer' : 'Show in viewer'"
                  @click.stop="toggleViewer(p)">
            <i class="pi pi-images" />
          </button>
          <!-- everything episodic (gate shape, its plot, move, delete) is one ⋯ menu — same pattern
               as the image table's row menu. Read-only surfaces keep only the gate's plot. -->
          <button v-if="!readonly || !!p.gate" class="pm-icon pm-actions-btn cc-btn cc-btn-bare cc-btn-icon"
                  :class="{ on: actionsPath === p.path }" v-tooltip.left="'More'"
                  @click.stop="openActions(p, $event)">
            <i class="pi pi-ellipsis-h" />
          </button>
        </div>

        <!-- cluster-ID toggles: tick a cluster into this pop (filled = assigned; a cluster lives in
             at most one pop). Tooltip names the owner if it's assigned elsewhere. -->
        <div v-if="clusterMode && p.filter" class="pm-clusters cc-row cc-row-tight"
             data-guide="popmanager.clusterChips"
             :style="{ paddingLeft: 22 + p.depth * 14 + 'px' }">
          <button v-for="id in props.clusterIds" :key="id" class="pm-chip"
                  :class="{ on: popClusterIds(p).includes(id), ro: readonly }" :disabled="readonly"
                  :style="popClusterIds(p).includes(id) ? { background: p.colour, borderColor: p.colour, color: '#111' } : {}"
                  v-tooltip.bottom="clusterOwner(id) && clusterOwner(id)?.path !== p.path ? `In “${clusterOwner(id)?.name}”` : ''"
                  @click.stop="!readonly && toggleCluster(p, id)">{{ id }}</button>
          <span v-if="!props.clusterIds.length" class="pm-chip-empty cc-empty-inline cc-fs-2xs">no clusters at this suffix</span>
        </div>
      </template>

      <!-- ⋯ row menu — the per-population actions that don't fit beside the row. Shares the
           `.cc-actions-*` utilities with the image table's menu. -->
      <TeleportPopover v-model="actionsOpen" :anchor="actionsAnchor" placement="bottom-end" flush>
        <div v-if="actionsPop" class="cc-actions-menu pm-actions">
          <!-- MOVE: the same popover becomes the parent list (the choice IS a list of populations) -->
          <template v-if="moveMode">
            <div class="cc-actions-head">Move “{{ actionsPop.name }}” under</div>
            <button v-for="t in moveTargets" :key="t" class="cc-actions-item pm-move-target"
                    @click.stop="moveTo(t)">{{ t === 'root' ? 'All cells' : t }}</button>
            <div v-if="!moveTargets.length" class="cc-actions-head">nowhere else to put it</div>
          </template>
          <template v-else>
            <button v-if="actionsPop.gate" class="cc-actions-item"
                    @click.stop="runAction(() => emit('showDefiningPlot', actionsPop!))">
              <i class="pi pi-search" /> Show the gate's plot
            </button>
            <!-- rectangle ⇄ polygon on the SAME population (no delete-and-redraw). Widening
                 (poly → rect) arms first; the label names the shape you get. -->
            <ConfirmButton v-if="actionsPop.gate && !readonly && !actionsPop.transient"
                           :needs-confirm="actionsPop.gate.kind === 'polygon'"
                           @confirm="runAction(() => convertGate(actionsPop!))" v-slot="{ armed, arm, confirm }">
              <button class="cc-actions-item" :class="{ armed }" @click.stop="armed ? confirm() : arm()">
                <i :class="armed ? 'pi pi-exclamation-triangle' : convertIcon(actionsPop)" />
                {{ armed ? 'Click again — the gate widens' : convertTip(actionsPop) }}
              </button>
            </ConfirmButton>
            <button v-if="!readonly && !actionsPop.transient" class="cc-actions-item"
                    @click.stop="moveMode = true">
              <i class="pi pi-arrows-h" /> Move under…
            </button>
            <!-- combine THIS one with another: opens the form with this population already in it, so
                 the "…or the other marker too" case is two clicks rather than a form from scratch. -->
            <button v-if="!readonly && !actionsPop.transient && !clusterMode" class="cc-actions-item"
                    @click.stop="runAction(() => openCreateBool(actionsPop!))">
              <i class="pi pi-link" /> Combine with…
            </button>
            <ConfirmButton v-if="!readonly && !actionsPop.transient && childCount(actionsPop) > 0"
                           @confirm="runAction(() => g.deletePopChildren(actionsPop!.path))"
                           v-slot="{ armed, arm, confirm }">
              <button class="cc-actions-item danger" :class="{ armed }" @click.stop="armed ? confirm() : arm()">
                <i :class="armed ? 'pi pi-exclamation-triangle' : 'pi pi-trash'" />
                {{ armed ? 'Click again to delete' : `Delete ${childCount(actionsPop)} below it` }}
              </button>
            </ConfirmButton>
            <ConfirmButton v-if="!readonly && !actionsPop.transient"
                           @confirm="runAction(() => g.deletePop(actionsPop!.path))"
                           v-slot="{ armed, arm, confirm }">
              <button class="cc-actions-item danger" :class="{ armed }" @click.stop="armed ? confirm() : arm()">
                <i :class="armed ? 'pi pi-exclamation-triangle' : 'pi pi-trash'" />
                {{ armed ? 'Click again to delete' : (childCount(actionsPop) ? `Delete it and ${childCount(actionsPop)} below` : 'Delete population') }}
              </button>
            </ConfirmButton>
            <!-- The cell-selection pop is transient (never persisted) — this clears it so it
                 doesn't linger forever; there's no persisted pop to delete. -->
            <button v-if="actionsPop.transient && !readonly" class="cc-actions-item danger"
                    @click.stop="runAction(() => g.clearSelection())">
              <i class="pi pi-trash" /> Clear selection
            </button>
          </template>
        </div>
      </TeleportPopover>


    <!-- ── gate / viewer options (host-specific, #options slot). In cluster mode there are no gates
         (the plot group); trackclust has no viewer control either, so the whole block is hidden. ── -->
    <template v-if="props.popType !== 'trackclust'" #options>
      <div class="pm-opts">
        <button class="pm-opts-toggle cc-section-toggle" @click="optionsOpen = !optionsOpen">
          <i :class="optionsOpen ? 'pi pi-chevron-down' : 'pi pi-chevron-right'" />
          <span class="cc-eyebrow">Options</span>
        </button>
        <div v-show="optionsOpen" class="pm-opts-body">
          <template v-if="!clusterMode">
          <div class="pm-opt-head cc-eyebrow cc-fs-2xs"><span>plot</span></div>
          <div class="pm-opt-row">
            <span class="pm-opt-label cc-muted cc-fs-xs">Gate labels</span>
            <button class="seg-btn cc-btn cc-btn-ghost cc-btn-icon cc-btn-lg"
                    :class="{ 'cc-btn-on cc-btn-on-tint': gateLabels }"
                    v-tooltip.top="'Show population names on gates'"
                    @click="emit('update:gateLabels', !gateLabels)"><i class="pi pi-tag" /></button>
          </div>
          <div class="pm-opt-row">
            <span class="pm-opt-label cc-muted cc-fs-xs">Line width</span>
            <input type="range" min="0.5" max="4" step="0.5" :value="lineWidth"
                   v-tooltip.top="'Gate line thickness'"
                   @input="emit('update:lineWidth', parseFloat(($event.target as HTMLInputElement).value))" />
            <span class="pm-opt-val cc-readout cc-fs-xs">{{ lineWidth.toFixed(1) }}</span>
          </div>
          <!-- scatter dot size: the PLOT twin of the viewer point size below. The default (0.7 → a
               1.4px square) is the FlowJo speckle, which reads on a dense cloud but is hard to see on a
               sparse one or when the dots carry a colour-by measure. Scales every dot on the plot. -->
          <div class="pm-opt-row">
            <span class="pm-opt-label cc-muted cc-fs-xs">Dot size</span>
            <input type="range" min="0.25" max="3" step="0.25" :value="dotSize ?? DOT_R"
                   v-tooltip.top="'Radius of each plotted cell'"
                   @input="emit('update:dotSize', parseFloat(($event.target as HTMLInputElement).value))" />
            <span class="pm-opt-val cc-readout cc-fs-xs">{{ (dotSize ?? DOT_R).toFixed(2) }}</span>
          </div>
          <div class="pm-opt-row">
            <span class="pm-opt-label cc-muted cc-fs-xs">Axis</span>
            <ChipSelect class="pm-seg" variant="segmented" :options="AXIS_OPTIONS"
                        :model-value="axisFromZero ? 'zero' : 'auto'" aria-label="Axis scale"
                        @update:model-value="v => emit('update:axisFromZero', v === 'zero')" />
          </div>
          </template>

          <!-- viewer-option group is popType-specific: flow/live/clust populations render as viewer
               Points (size slider); track/trackclust render as Tracks ribbons (no point size — tail
               width is a plot-panel concern), so the group is hidden for those. -->
          <template v-if="props.popType !== 'track' && props.popType !== 'trackclust'">
            <div class="pm-opt-head cc-eyebrow cc-fs-2xs"><span>viewer</span></div>
            <!-- viewer point size (re-renders the viewer overlay on release) -->
            <div class="pm-opt-row">
              <span class="pm-opt-label cc-muted cc-fs-xs">Point size</span>
              <input type="range" min="1" max="20" step="1" :value="viewerPointSize"
                     v-tooltip.top="'Population point size on the viewer (per experiment/set)'"
                     @input="viewerPointSize = parseInt(($event.target as HTMLInputElement).value)"
                     @change="g.refreshPops()" />
              <span class="pm-opt-val cc-readout cc-fs-xs">{{ viewerPointSize }}</span>
            </div>
          </template>
        </div>
      </div>
    </template>
  </CanvasSidePanel>
</template>

<style scoped>
/* row / options styles — applied to content rendered into CanvasSidePanel's slots (slotted
   content keeps THIS component's scoped styles; the floating chrome + scope footer live in the shell). */
.pm-empty { padding: 12px; }   /* + .cc-muted */
.pm-row { display: flex; align-items: center; gap: 6px; padding: 4px 8px 4px 6px; cursor: pointer; border-bottom: 1px solid var(--cc-border); }
.pm-row:hover { background: var(--cc-surface-2); }
.pm-row.active { background: color-mix(in srgb, var(--cc-accent) 22%, transparent); }
.pm-row.transient { font-style: italic; background: color-mix(in srgb, #22d3ee 8%, transparent); }
.pm-viewer { width: 16px; text-align: center; font-size: var(--cc-fs-md); }
.pm-name { flex: 1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.pm-rename { flex: 1; background: var(--cc-bg); border: 1px solid var(--cc-accent); border-radius: var(--cc-radius-xs); padding: 1px 4px; }
.pm-stat { color: var(--cc-text-dim); font-variant-numeric: tabular-nums; }
.pm-stat small { opacity: 0.7; margin-left: 3px; }
/* .pm-icon → cc-btn cc-btn-bare cc-btn-icon; only the STATE variants are its own (the plain `:hover`
   rule was byte-identical to `.cc-btn-bare:hover` and is gone) */
.pm-icon.lit { color: var(--cc-accent); }
/* ⋯ trigger: faintly visible at rest (discoverable without shouting), full on hover/open — the
   image table's row menu makes the same call. */
.pm-actions-btn { opacity: 0.55; }
.pm-row:hover .pm-actions-btn, .pm-actions-btn.on { opacity: 1; }
/* the menu's own width: a move target is a pop PATH, which can be long — wrap rather than stretch
   the popover across the canvas. */
.pm-actions { max-width: 260px; }
.pm-actions .cc-actions-item { word-break: break-word; }
/* move targets carry no icon (the text IS the population path) — indent to the icon gutter so the
   list still lines up under its heading */
.pm-move-target { padding-left: 2.15rem; }

/* ── cluster mode: add-pop bar + per-pop cluster-ID toggle chips ── */
/* layout is `.cc-row` (it WRAPS: at ~250px the two pop-defining buttons — filter, combine — don't
   fit on one line beside undo/redo, and a squashed row hides one behind an overflow nobody looks
   for). Only the bar's own chrome stays here. */
.pm-add { padding: 6px 8px; border-bottom: 1px solid var(--cc-border); }
.pm-add-spacer { flex: 1; }
/* Redo is `pi-undo` MIRRORED, the way every icon set draws the pair — PrimeIcons has no redo glyph,
   and the two nearest candidates are both wrong: `pi-replay` is pixel-identical to `pi-undo` (same
   counter-clockwise arrow, so the two buttons looked the same), and `pi-refresh` already means
   "reload / restart a service". Mirroring keeps ONE glyph for stepping through history and lets the
   direction carry the difference. See docs/UI.md → Icons. */
.pm-redo i { transform: scaleX(-1); }
.pm-add-btn { display: inline-flex; align-items: center; gap: 5px; font-size: var(--cc-fs-xs); padding: 4px 9px;
  border: 1px solid var(--cc-border); border-radius: var(--cc-radius-xs); background: var(--cc-surface-2);
  color: var(--cc-text); cursor: pointer; }
.pm-add-btn:hover { border-color: var(--cc-accent-strong); color: var(--cc-accent-soft); }
.pm-add-btn:disabled { opacity: 0.5; cursor: not-allowed; }
/* `+[icon]` compact add buttons (Dominik, 2026-08-26). Two icons in one primary button — same
   idiom as `+ Plot` / `+ Pairs` in the gating toolbar, minus the label. Sized to fit both glyphs
   comfortably (default `.cc-btn-icon` is a 1.5rem square built for ONE glyph and clipped these). */
.pm-add-icon { padding: 3px 8px; font-size: var(--cc-fs-xs); }
.pm-add-icon i + i { margin-left: 3px; }
/* filter-population form (Decision 15) */
.pm-ff { display: flex; flex-direction: column; gap: 5px; padding: 6px 8px; border-bottom: 1px solid var(--cc-border);
  background: var(--cc-surface-1); }
.pm-ff-head { display: flex; gap: 5px; align-items: center; }
.pm-ff-name { flex: 1; font-size: var(--cc-fs-xs); padding: 3px 6px; border-radius: var(--cc-radius-xs); }
.pm-ff-colour { width: 24px; height: 24px; padding: 0; border-radius: var(--cc-radius-xs);
  background: none; cursor: pointer; }
.pm-ff-row, .pm-ff-cond { display: flex; gap: 4px; align-items: center; }
.pm-ff-cond select, .pm-ff-row select, .pm-ff-vals { font-size: var(--cc-fs-xs); padding: 2px 4px;
  border-radius: var(--cc-radius-xs); }
.pm-ff-measure { flex: 1; min-width: 0; }
.pm-ff-fun { width: 48px; }
.pm-ff-vals { width: 64px; }
.pm-ff-actions { display: flex; justify-content: space-between; align-items: center; }
.pm-ff-cond-add { background: none; border: none; color: var(--cc-text-dim); font-size: var(--cc-fs-xs); cursor: pointer; padding: 2px; }
.pm-ff-cond-add:hover { color: var(--cc-text); }
.pm-ff-title { font-size: var(--cc-fs-xs); font-weight: 600; color: var(--cc-text); }
.pm-ff-spacer { flex: 1; }
.pm-ff-cancel { background: none; border: none; color: var(--cc-text-dim); font-size: var(--cc-fs-xs); cursor: pointer; padding: 4px 6px; }
.pm-ff-cancel:hover { color: var(--cc-text); }
/* combined-population form (Decision 16): the term row reuses the filter form's layout — only the
   is / is not select is wider than a comparison operator, and the badge takes its own hue so a
   combination is distinguishable from a filter at a glance. */
.pm-ff-neg { width: 62px; }
/* the parent pair shares its line with the operator, so IT takes the slack (and shrinks) rather than
   sizing to its longest option — a deep path would otherwise push "in any of" off the row */
.pm-ff-parent, .pm-ff-parent select { flex: 1; min-width: 0; }
.pm-bool-badge { color: #38bdf8; }
.pm-filter-badge { font-size: var(--cc-fs-2xs); color: #8b5cf6; margin-left: 2px; opacity: 0.8; }
button.pm-filter-badge { border: none; background: none; cursor: pointer; padding: 2px; }
button.pm-filter-badge:hover { opacity: 1; }
.pm-clusters { padding: 2px 8px 6px; border-bottom: 1px solid var(--cc-border); }
.pm-chip { min-width: 1.4rem; height: 1.4rem; padding: 0 4px; font-size: var(--cc-fs-xs); line-height: 1;
  border: 1px solid var(--cc-border); border-radius: var(--cc-radius-xs); background: var(--cc-surface-1);
  color: var(--cc-text-dim); cursor: pointer; font-variant-numeric: tabular-nums; transition: background 0.1s, color 0.1s, border-color 0.1s; }
.pm-chip:hover { border-color: var(--cc-accent-strong); color: var(--cc-text); }
.pm-chip.on { font-weight: 700; }
/* read-only (Analysis board): chips show assignment but aren't clickable */
.pm-chip.ro { cursor: default; }
.pm-chip.ro:hover { border-color: var(--cc-border); color: var(--cc-text-dim); }
.pm-chip.ro.on:hover { color: #111; }
.pm-chip-empty { font-style: italic; }   /* + .cc-empty-inline .cc-fs-2xs (row/colour/10px tier) */

/* segmented toggle (axis option in the #options slot; the shell owns the footer scope toggle) */
.pm-seg { margin-left: auto; }
.seg-btn { transition: background 0.1s, color 0.1s, border-color 0.1s; }   /* + cc-btn cc-btn-ghost cc-btn-icon cc-btn-lg */
.seg-btn:hover { color: var(--cc-text); border-color: #484f58; }

/* ── extra options ── */
.pm-opts { border-top: 1px solid var(--cc-border); }
/* + cc-section-toggle (row) + cc-eyebrow on the label — padding is all that is this site's */
.pm-opts-toggle { padding: 6px 8px; }
.pm-opts-body { padding: 4px 10px 10px; display: flex; flex-direction: column; gap: 8px; }
/* small section heading: ──── plot ──── */
.pm-opt-head { display: flex; align-items: center; gap: 6px; margin-top: 2px; }
.pm-opt-head::before, .pm-opt-head::after { content: ""; flex: 1; height: 1px; background: var(--cc-border); }
.pm-opt-head:first-child { margin-top: 0; }
.pm-opt-row { display: flex; align-items: center; gap: 8px; }
.pm-opt-label { flex: 1; }
.pm-opt-row input[type="range"] { flex: 1; max-width: 110px; }
.pm-opt-val { width: 1.8rem; text-align: right; }
</style>
