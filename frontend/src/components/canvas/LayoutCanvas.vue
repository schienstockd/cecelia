<!--
  Grid LAYOUT canvas for one Analysis tab (docs/todo/ANALYSIS_CANVAS_PLAN.md, Phase A2). A template
  (uniform N×M or a rectangular "comic plate") defines SLOTS; each slot holds one plot. This is the
  READ-ONLY analysis surface (project_analysis_canvas_readonly): plots are chosen from the full spec
  catalog and what they show is picked in the DOCKED right rail — no gate/pop mutation.

  WHICH manager that rail holds comes from the active slot's registry `rail` (canvasManager.ts), not
  from a branch here: SeriesPicker for population series, the read-only PopulationManager for a cluster
  run, the FlowModelVault for a plot that needs a trained model. See docs/ANALYSIS.md → The rail.

  Reuse: the plot data + view-state come from useSummaryData (shared with the free-floating
  SummaryCanvas); each filled slot renders a DOCKED SummaryPanel (fills the slot, no float/drag). Only
  the container differs from SummaryCanvas — the panels/picker are the same components in `docked` mode.

  Persistence: the template + per-slot content + the shared view bag live in the analysisLayout store
  under this tab's `canvasKey`; the parent (TabbedCanvas) :keys us by it so a tab switch rebinds.
-->
<script setup lang="ts">
import { computed, watch, ref, provide, nextTick, useTemplateRef } from 'vue'
import { awaitIdle, anyBusy } from '../../utils/awaitIdle'
import { useCanvasZoom, CANVAS_ZOOM_KEY } from '../../composables/useCanvasZoom'
import CanvasZoomControl from './CanvasZoomControl.vue'
import { plotHostToImageURL } from '../../plots/export'
import { useProjectStore } from '../../stores/project'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useAnalysisLayoutStore, type SlotContent } from '../../stores/analysisLayout'
import { useSummaryData } from '../../composables/useSummaryData'
import { useClusterContext } from '../../composables/useClusterContext'
import { tkey, parseTkey, seriesMemo } from '../../plots/series'
import { defaultVis, DEFAULT_VIS, type VisProps } from '../../plots/plot'
import { UNIFORM_PRESETS, COMIC_PRESETS, uniform, A4_PORTRAIT_ASPECT, A4_LANDSCAPE_ASPECT } from '../../plots/layoutTemplates'
import type { SeriesTarget } from '../../plots/types'
import { migrateSpecId, isPrecomputedSpec } from '../../plots/popTypes'
import { emptyReadout, type PlotReadout } from '../../plots/plotReadout'
import SummaryPanel from './SummaryPanel.vue'
import InteractivePanel from './InteractivePanel.vue'
import PlateBuilder from './PlateBuilder.vue'
import type { LayoutTemplate } from '../../plots/layoutTemplates'
import { INTERACTIVE_VIEWS, boardViews, railFor, popTypesFor, popTypeSpecFor } from './interactiveViews'
import { DEFAULT_RAIL, type RailKind } from './canvasManager'
import SeriesPicker from './SeriesPicker.vue'
import PopulationManager from './PopulationManager.vue'
import FlowModelVault from '../../modules/opticalFlow/FlowModelVault.vue'
import TeleportPopover from '../TeleportPopover.vue'
import ChipSelect, { type ChipOption } from '../ChipSelect.vue'
import { CLUSTER_PANELS, isClusterPanel, clusterPanelRail } from '../../modules/cluster/clusterPanels'
import CcToggle from '../CcToggle.vue'

const props = defineProps<{ imageUids: string[]; module?: string | null; canvasKey: string }>()

const project = useProjectStore()
const meta = useProjectMetaStore()
const layout = useAnalysisLayoutStore()

const projectUid = computed(() => meta.current?.uid ?? '')
const imageUid = computed(() => props.imageUids[0] ?? null)
const setUid = computed(() => project.activeSetUid)

layout.ensure(props.canvasKey)
const entry = computed(() => layout.entries[props.canvasKey])
// minmax(0, 1fr) — NOT bare 1fr (= minmax(auto,1fr)). Bare 1fr lets a cell grow to its content's
// min-size, and a plot's ResizeObserver then re-renders taller → the grid keeps growing. minmax(0,1fr)
// pins every track to an equal share of the (bounded) grid height, so content clips/scrolls instead.
// board-level slot height: rows share a fixed total height (rowHeight × rows); the board scrolls in the
// page if it's taller than the viewport. Default 320px/row.
const rowHeight = computed({ get: () => entry.value.rowHeight ?? 320, set: v => (entry.value.rowHeight = v) })

// A4 sheet lock — undefined (older boards) reads as portrait so the "board is too wide" fix applies
// retroactively. In an A4 mode the board's WIDTH is derived from its height × the page aspect, so the
// on-screen layout matches the exported PDF page exactly (capturePage's measured aspect becomes exact).
const sheet = computed<'free' | 'a4-portrait' | 'a4-landscape'>({
  get: () => entry.value.sheet ?? 'a4-portrait', set: v => (entry.value.sheet = v) })
// only the plates that suit the current sheet orientation (all when Free); uniform grids are neutral
const platePresets = computed(() => {
  if (sheet.value === 'free') return COMIC_PRESETS
  const want = sheet.value === 'a4-portrait' ? 'portrait' : 'landscape'
  return COMIC_PRESETS.filter(t => (t.orient ?? 'any') === want || t.orient === 'any')
})

// grid size + height controls live in a ⚙ popover so the board bar doesn't crowd; close on outside click
// grid-size + custom-plate popovers use the shared TeleportPopover (escape the bar's clipping; the
// component handles outside-click/Escape dismiss). Anchor = each trigger button.
const optsOpen = ref(false)
const optsBtn = useTemplateRef<HTMLElement>('optsBtn')
const builderOpen = ref(false)
const builderBtn = useTemplateRef<HTMLElement>('builderBtn')
function applyCustomPlate(t: LayoutTemplate) { layout.applyTemplate(props.canvasKey, t); builderOpen.value = false }

// ── preset + sheet selectors (shared ChipSelect) ──────────────────────────────
const SHEET_OPTIONS: ChipOption[] = [
  { value: 'a4-portrait', label: 'A4 ↕' }, { value: 'a4-landscape', label: 'A4 ↔' }, { value: 'free', label: 'Free' },
]
// A preset row is a single-select whose active chip is DERIVED from the current grid matching a preset
// (none active when the grid is custom). Picking one applies that template.
const uniformOptions = computed<ChipOption[]>(() => UNIFORM_PRESETS.map(t => ({ value: t.id, label: t.label })))
const plateOptions   = computed<ChipOption[]>(() => platePresets.value.map(t => ({ value: t.id, label: t.label })))
const uniformMatchId = computed(() => {
  const e = entry.value; if (!e) return ''
  return UNIFORM_PRESETS.find(t => e.cols === t.cols && e.rows === t.rows && e.slotAreas.length === t.slots.length)?.id ?? ''
})
const plateMatchId = computed(() => {
  const e = entry.value; if (!e) return ''
  return platePresets.value.find(t => e.slotAreas.join('|') === t.slots.join('|'))?.id ?? ''
})
function applyPreset(presets: LayoutTemplate[], id: string) {
  const t = presets.find(p => p.id === id)
  if (t) layout.applyTemplate(props.canvasKey, t)
}
// board natural (unscaled) size — height from rows×rowHeight; width from the A4 page aspect (null in
// Free mode, where the grid fills the available width).
const boardH = computed(() => rowHeight.value * entry.value.rows + 8 * (entry.value.rows - 1))
const boardW = computed<number | null>(() => {
  if (sheet.value === 'a4-portrait') return boardH.value * A4_PORTRAIT_ASPECT
  if (sheet.value === 'a4-landscape') return boardH.value * A4_LANDSCAPE_ASPECT
  return null
})
const gridStyle = computed(() => {
  const base: Record<string, string> = {
    gridTemplateColumns: entry.value.colTracks ?? `repeat(${entry.value.cols}, minmax(0, 1fr))`,
    gridTemplateRows: entry.value.rowTracks ?? `repeat(${entry.value.rows}, minmax(0, 1fr))`,
    height: `${boardH.value}px`,
  }
  // A4: lock width to height × page aspect so the box IS the page. Free: leave width to CSS (fills).
  if (boardW.value != null) { base.width = `${boardW.value}px`; base.flex = 'none' }
  return base
})

// ── visual zoom (fit-to-view, Word/Illustrator style) — A4 modes only (Free already fills width) ──
const canvasWrapRef = useTemplateRef<HTMLElement>('canvasWrapRef')
const { zoom, zooming, fitWidth, fitHeight, fitWidthIfOverflow, setZoom, reset: resetZoom } =
  useCanvasZoom(canvasWrapRef, () => ({ w: boardW.value, h: boardH.value }))
provide(CANVAS_ZOOM_KEY, zoom)   // docked panels don't pixel-drag, but keep the contract uniform
// neutralise zoom during PDF capture so the measured slot rects are at full 1:1 size (the transform
// would otherwise scale getBoundingClientRect and throw off the hi-res composite)
const effZoom = computed(() => (capturing.value ? 1 : zoom.value))
const zoomWrapStyle = computed(() => boardW.value != null
  ? { width: `${boardW.value * effZoom.value}px`, height: `${boardH.value * effZoom.value}px`, margin: '0 auto' }
  : { width: '100%' })
const gridZoomStyle = computed(() => (boardW.value != null && effZoom.value !== 1)
  // `will-change` only WHILE zooming: it promotes the board to its own compositor layer so a drag
  // re-composites instead of re-rasterising every SVG plot per step. Held permanently it would keep a
  // full-board layer alive for nothing, so useCanvasZoom drops the flag shortly after the interaction.
  ? { transform: `scale(${effZoom.value})`, transformOrigin: 'top left',
      ...(zooming.value ? { willChange: 'transform' } : {}) }
  : {})
// first render (and image switch): fit-to-width if the board would overflow, so the whole board is
// visible without hiding the sidebar; a board that already fits stays at 100%.
watch(imageUid, () => nextTick(fitWidthIfOverflow), { immediate: true })

// shared summary-plot data + view-state (same composable the free-floating canvas uses)
const {
  specs, specById, segPops, seriesColor, reloadToken, validSelKeys, popType,
  compareMode, compareAttr, compareAttr2, scope, gSel, gVis, poolGroups,
  canCompare, panelSetUid, panelImageUids, panelScope, panelGroupAttr, attrOptions2, setAttrs,
} = useSummaryData({
  projectUid, imageUids: computed(() => props.imageUids), setUid, module: props.module,
  shared: computed(() => entry.value.shared),
  // the board is mixed-popType: point the picker at the ACTIVE summary slot so it surfaces THAT plot's
  // population family — either the family its spec carries, or (for a spec offering a choice, like the
  // one Population summary) the family the slot has picked.
  activeSpecId: computed(() => {
    const c = entry.value.contents[entry.value.activeIndex]
    return c && c.kind === 'summary' ? c.ref : null
  }),
  // NB: read `state` inline rather than via the `st()` helper — that is declared below, and this
  // computed can be evaluated during useSummaryData's own setup (temporal dead zone).
  activePopType: computed(() => {
    const c = entry.value.contents[entry.value.activeIndex]
    if (!c) return null
    return ((c.state as Record<string, unknown> | undefined)?.popType as string | undefined) ?? null
  }),
  // an INTERACTIVE slot that slices by population declares its families on its registry entry (the two
  // track plots) — so the rail lists that plot's family, exactly as it follows a summary slot's spec.
  activeFamily: computed(() => {
    const c = entry.value.contents[entry.value.activeIndex]
    return c && c.kind === 'interactive' ? popTypeSpecFor(c.ref) : null
  }),
})

// Migrate boards persisted before the four per-popType population summaries collapsed into one spec: a
// summary slot's `ref` IS the spec id, so a stale id would resolve to no spec and render an empty slot.
for (const c of entry.value.contents) {
  if (!c || c.kind !== 'summary') continue          // slots may be empty (null)
  const state = (c.state ?? {}) as Record<string, unknown>
  const s = { specId: c.ref, popType: state.popType as string | undefined }
  if (migrateSpecId(s)) {
    c.ref = s.specId
    state.popType = s.popType
    c.state = state
  }
}

// ── slot content: active slot, add/clear, drag-swap ──────────────────────────────────────────────
const activeContent = computed<SlotContent | null>(() => entry.value.contents[entry.value.activeIndex] ?? null)
type PState = { sel: string[]; vis: VisProps; [k: string]: unknown }
const st = (c: SlotContent): PState => c.state as PState

// The picker's three interactive optgroups come STRAIGHT from the registry's `analysisBoard` flag +
// `boardGroup` — no key list here. There used to be one (`ANALYSIS_VIEWS`/`IMAGE_VIEWS`), which made
// the flag a lie: a view could set `analysisBoard: true` and never appear, because nothing read it.
// Self-contained interactive views (read their own context / pops):
const interactiveOptions = computed(() => boardViews('interactive'))
// Which MANAGER a slot needs, straight from the registry that owns the slot's kind. Summary slots have
// no registry entry and take the default (the population picker) — see canvasManager.ts.
function railOf(c: SlotContent | null): RailKind {
  if (!c || c.kind !== 'interactive') return DEFAULT_RAIL
  return isClusterPanel(c.ref) ? clusterPanelRail(c.ref) : railFor(c.ref)
}
// CLUSTERING plots — one clustering run per board (see useClusterContext / ANALYSIS_CANVAS_PLAN Phase G):
// the clustering-group interactive views (UMAP) + the cluster panels (CLUSTER_PANELS registry). "Driven
// by the board's clustering run" and "wants the cluster pop manager" are the same statement, so this is
// now the rail declaration rather than a second, parallel test that could disagree with it.
const isClusterSlot = (c: SlotContent | null): boolean => railOf(c) === 'clusterPops'
const clusterOptions = computed(() => {
  const out: { key: string; label: string }[] = [...boardViews('clustering')]
  for (const [key, def] of Object.entries(CLUSTER_PANELS)) {
    if (!def.analysisBoard) continue
    if (def.trackOnly && clustPopType.value !== 'trackclust') continue          // HMM = track runs only
    if (def.needsCols === 'hmmState' && !clustHmmStateCols.value.length) continue
    if (def.needsCols === 'hmmTransition' && !clustHmmTransitionCols.value.length) continue
    out.push({ key, label: def.label })
  }
  return out
})
// image-content views (napari screenshot slots) — grouped separately in the picker
const imageOptions = computed(() => boardViews('image'))

// the "+ Plot" value is "summary:<specId>" or "interactive:<viewKey>"
function addPlot(i: number, val: string) {
  if (!val) return
  const sep = val.indexOf(':'); const kind = val.slice(0, sep); const ref = val.slice(sep + 1)
  if (kind === 'summary') layout.setContent(props.canvasKey, i, { kind: 'summary', ref, state: { specId: ref, sel: [], vis: defaultVis() } })
  else if (kind === 'interactive') {
    // a view seeds its own new-panel state (registry `initialState`); cluster PANELS carry a `hl`
    // (highlight) bag and self-seed the rest (e.g. heatmap features)
    const state = INTERACTIVE_VIEWS[ref]?.initialState?.() ?? (isClusterPanel(ref) ? { hl: [] } : {})
    layout.setContent(props.canvasKey, i, { kind: 'interactive', ref, state })
  }
  layout.setActive(props.canvasKey, i)
}
function clearSlot(i: number) { layout.setContent(props.canvasKey, i, null) }

// per-slot title (figure caption) — persisted in the slot's own state bag (survives navigation with the
// rest of the layout). Empty by default; drawn above the plot in the PDF export.
const slotTitle = (i: number): string => (entry.value.contents[i]?.state.title as string) ?? ''
function setSlotTitle(i: number, v: string) { const c = entry.value.contents[i]; if (c) c.state.title = v }

// duplicate a slot's plot into the NEXT EMPTY slot (deep-copy its state so you can tweak one thing);
// no-op if the grid is full.
function nextEmpty(from: number): number {
  const c = entry.value.contents
  for (let k = 1; k <= c.length; k++) { const j = (from + k) % c.length; if (!c[j]) return j }
  return -1
}
function duplicateSlot(i: number) {
  const src = entry.value.contents[i]
  if (!src) return
  const j = nextEmpty(i)
  if (j < 0) return
  layout.setContent(props.canvasKey, j, { kind: src.kind, ref: src.ref, state: JSON.parse(JSON.stringify(src.state)) })
  layout.setActive(props.canvasKey, j)
}

// drag reorder via the slot's GRIP handle only (so interacting with the plot itself never starts a
// drag); drop swaps the two slots' contents.
const dragFrom = { i: -1 }
function onDrop(i: number) { if (dragFrom.i >= 0) layout.swap(props.canvasKey, dragFrom.i, i); dragFrom.i = -1 }

// ── global/local scope (drives eye-selection + vis), targeting the ACTIVE slot when local ─────────
const panelSel = (c: SlotContent) => scope.value === 'global' ? gSel.value : (st(c).sel ?? [])
const panelVis = (c: SlotContent) => scope.value === 'global' ? gVis.value : (st(c).vis ?? DEFAULT_VIS)
const activeSel = computed(() => scope.value === 'global' ? gSel.value : (activeContent.value ? st(activeContent.value).sel : []))
// fall back to defaultVis() when the active slot has no local vis yet (matches ClusterPlots) — else the
// pop manager's `vis` is undefined and the whole PlotOptions styling block is hidden (the "cluster-tracks
// manager has no plot params" bug).
const activeVis = computed(() => scope.value === 'global' ? gVis.value : (activeContent.value ? (st(activeContent.value).vis ?? DEFAULT_VIS) : DEFAULT_VIS))
const toggle = (arr: string[], v: string) => arr.includes(v) ? arr.filter(x => x !== v) : [...arr, v]
function toggleTarget(valueName: string, pop: string, pt: string) {
  const k = tkey(pt, valueName, pop)
  if (scope.value === 'global') gSel.value = toggle(gSel.value, k)
  else if (activeContent.value) st(activeContent.value).sel = toggle(st(activeContent.value).sel ?? [], k)
}
function setVis(patch: Partial<VisProps>) {
  if (scope.value === 'global') gVis.value = { ...gVis.value, ...patch }
  else if (activeContent.value) st(activeContent.value).vis = { ...(st(activeContent.value).vis ?? defaultVis()), ...patch }
}
// keyed by SLOT INDEX so a slot keeps one entry; identity holds while its selection does (see
// seriesMemo) — a template-built list must not hand every panel a "new" series array per render.
const memoSeries = seriesMemo<number>()
const panelSeries = (i: number, c: SlotContent): SeriesTarget[] => memoSeries(i, panelSel(c))

// the stats test each summary slot's last result actually ran (`auto` resolves it server-side from the
// group count) — the rail shows the ACTIVE slot's, so the user can see what `auto` chose. Keyed by slot
// index; a readout of the current result, not a persisted setting.
const readouts = ref<Record<number, PlotReadout>>({})
const activeReadout = computed<PlotReadout>(() => readouts.value[entry.value.activeIndex] ?? emptyReadout())

// ── cluster context: ONE clustering run per board (board-level popType + suffix in the shared bag) so
// the singleton gating store is driven unambiguously; only active when a cluster slot exists. ─────────
const hasClusterSlot = computed(() => entry.value.contents.some(isClusterSlot))
const clustPopType = computed<'clust' | 'trackclust'>({
  get: () => (entry.value.shared.clustPopType as 'clust' | 'trackclust') ?? 'clust',
  set: v => (entry.value.shared.clustPopType = v) })
const clustSuffix = computed<string>({
  get: () => (entry.value.shared.clustSuffix as string) ?? 'default',
  set: v => (entry.value.shared.clustSuffix = v) })
const { suffixes: clustSuffixes, clusterIds: clustClusterIds, validUids: clustValidUids,
        featureOptions: clustFeatureOptions, labelMap: clustNameMap,
        hmmStateCols: clustHmmStateCols, hmmTransitionCols: clustHmmTransitionCols, shownPopsFor } =
  useClusterContext({ projectUid, imageUids: computed(() => props.imageUids),
                      popType: clustPopType, suffix: clustSuffix, enabled: hasClusterSlot })

// cluster HIGHLIGHT — global (shared, one run per board) or per-slot (local), same scope as summary
const clustHl = computed<string[]>({ get: () => (entry.value.shared.clustHl as string[]) ?? [], set: v => (entry.value.shared.clustHl = v) })
const clustHlOf = (c: SlotContent): string[] => (st(c).hl as string[]) ?? []
const panelClustHl = (c: SlotContent) => scope.value === 'global' ? clustHl.value : clustHlOf(c)
const activeClustHl = computed(() => scope.value === 'global' ? clustHl.value
  : (activeContent.value ? clustHlOf(activeContent.value) : []))
const activeIsCluster = computed(() => isClusterSlot(activeContent.value))
// the manager the rail should show, from the active slot's registry entry
const activeRail = computed<RailKind>(() => railOf(activeContent.value))
// the active slot's plot is PRECOMPUTED (its populations come from an analysis run, not the picker)
const activeIsPrecomputed = computed(() => {
  const c = activeContent.value
  if (!c || c.kind !== 'summary') return false
  const spec = specById.value[c.ref]
  return !!spec && isPrecomputedSpec(spec)
})
function toggleClustHl(path: string) {
  if (scope.value === 'global') clustHl.value = toggle(clustHl.value, path)
  else if (activeContent.value) st(activeContent.value).hl = toggle(clustHlOf(activeContent.value), path)
}

// ── flow-model rail: the vault's pick, held here exactly as the cluster highlight is — GLOBAL in the
// board's shared bag, LOCAL in the active slot's own state. Same shape `FlowPlots` uses on the module
// page, so a flow plot receives its model through the standard context bag on both surfaces and needs
// no board-specific branch of its own. ───────────────────────────────────────────────────────────────
const flowModel = computed<string>({
  get: () => (entry.value.shared.flowModel as string) ?? '', set: v => (entry.value.shared.flowModel = v) })
const panelFlowModel = (c: SlotContent) =>
  scope.value === 'global' ? flowModel.value : ((st(c).model as string | undefined) ?? flowModel.value)
const activeFlowModel = computed(() => scope.value === 'global' ? flowModel.value
  : (activeContent.value ? ((st(activeContent.value).model as string | undefined) ?? flowModel.value) : flowModel.value))
function setFlowModel(v: string) {
  if (scope.value === 'global') flowModel.value = v
  else if (activeContent.value) st(activeContent.value).model = v
}

// context handed to an interactive slot: cluster views get the board cluster run + shown pops; flow
// views get the vault's model; the self-contained views (gating strategy, filmstrip) just get the
// image/project context.
function ctxFor(c: SlotContent, i: number) {
  if (isClusterSlot(c)) return {
    projectUid: projectUid.value, imageUids: clustValidUids.value, setUid: setUid.value,
    popType: clustPopType.value, suffix: clustSuffix.value,
    shownPops: shownPopsFor(panelClustHl(c)), vis: panelVis(c),
  }
  const base = { projectUid: projectUid.value, imageUids: props.imageUids, setUid: setUid.value, vis: panelVis(c) }
  if (railOf(c) === 'flowModels') return { ...base, model: panelFlowModel(c) }
  // A plot on the POPULATION rail is part of the board's comparison, so it gets the same four things a
  // SummaryPanel gets: the selection, the compare mode + its attributes, and the pool toggle. The plot
  // decides what to do with them; a self-contained view declares `rail: 'none'` and never sees them.
  // (No `popColors`: the two track plots identify a group by its facet TITLE, not by a colour, so
  // passing the map would be a prop wired to nothing — the failure this file's rail comments warn about.)
  if (railOf(c) === 'pops') return {
    ...base, series: panelSeries(i, c), popTypes: popTypesFor(c.ref),
    compareMode: compareMode.value, groupAttr: panelGroupAttr.value, poolGroups: poolGroups.value,
  }
  return base
}

// props for a cluster PANEL slot (CLUSTER_PANELS): the common bag + the panel-specific props its registry
// entry maps from the shared cluster context — so the slot renders with one generic <component v-bind>.
function clusterPanelProps(i: number) {
  const c = entry.value.contents[i]!
  const ctx = { featureOptions: clustFeatureOptions.value, nameMap: clustNameMap.value,
                hmmStateCols: clustHmmStateCols.value, hmmTransitionCols: clustHmmTransitionCols.value }
  return {
    index: i, active: i === entry.value.activeIndex, arrange: null, docked: true,
    projectUid: projectUid.value, setUid: setUid.value, imageUids: clustValidUids.value,
    popType: clustPopType.value, suffix: clustSuffix.value,
    shownPops: shownPopsFor(panelClustHl(c)), vis: panelVis(c), state: c.state,
    persistKey: `${props.canvasKey}:slot:${i}`,
    ...(CLUSTER_PANELS[c.ref].props?.(ctx) ?? {}),
  }
}

// prune vanished pops from every slot's local selection (the composable prunes the global one). Guard on
// a non-empty segPops — it's transiently [] during load/image-switch, and pruning then would wipe (and
// then persist-empty) a restored per-slot selection. popType-AWARE (mixed board): segPops holds only the
// active slot's popType, so only prune keys of THAT popType — else selecting e.g. a trackclust slot would
// wipe the live/track selections of the other (track-measure) plots.
watch(segPops, () => {
  if (!segPops.value.length) return
  const valid = validSelKeys.value, pt = popType.value
  const keep = (k: string) => parseTkey(k).popType !== pt || valid.has(k)
  for (const c of entry.value.contents) if (c && Array.isArray(st(c).sel)) st(c).sel = st(c).sel.filter(keep)
})

// ── PDF export: capture each filled slot to a PNG (hiding the drag grips), keyed by its grid-area ──
const gridRef = useTemplateRef<HTMLElement>('gridRef')
const capturing = ref(false)
function labelFor(c: SlotContent): string {
  if (c.kind === 'summary') return specById.value[c.ref]?.label ?? c.ref
  if (isClusterPanel(c.ref)) return CLUSTER_PANELS[c.ref].label
  return INTERACTIVE_VIEWS[c.ref]?.label ?? c.ref
}
// panel instances by slot index, so we can ask each for a PLOT-ONLY, LIGHT-theme image (no chrome) and
// pull the summary plot's aggregated CSV. Both panel types expose exportImage(); summary also getCsv().
type SummaryRef = { getCsv(): string | null | Promise<string | null>; getStatsCsv?(): string; csvName?(): string; exportImage(): Promise<string | null>; exportSvg?(): string | null | Promise<string | null>; isBusy?(): boolean }
type ExportRef = { exportImage(): Promise<string | null>; exportSvg?(): string | null | Promise<string | null>; isBusy?(): boolean }
const summaryRefs = new Map<number, SummaryRef>()
const interactiveRefs = new Map<number, ExportRef>()
function setSummaryRef(i: number, el: unknown) { if (el) summaryRefs.set(i, el as SummaryRef); else summaryRefs.delete(i) }
function setInteractiveRef(i: number, el: unknown) { if (el) interactiveRefs.set(i, el as ExportRef); else interactiveRefs.delete(i) }

type PdfSlotOut = { rect: { x: number; y: number; w: number; h: number }; png: string | null; svg?: string | null; name: string; title?: string; csv?: string | null }
// `vector` (board→SVG export): additionally capture each slot's true-vector SVG where the panel offers
// one (summary/heatmap/UMAP/gating); slots without `exportSvg` (image/filmstrip, HMM) leave `svg` null
// and the board embeds their PNG as raster (docs/ANALYSIS.md). PNG is always captured as the fallback.
async function capturePage(vector = false) {
  const gridEl = gridRef.value
  if (!gridEl) return { aspect: 1, slots: [] as PdfSlotOut[] }
  const slotEls = Array.from(gridEl.querySelectorAll('.lc-slot')) as HTMLElement[]
  capturing.value = true
  await new Promise(r => requestAnimationFrame(() => r(null)))   // let the grip-hide take effect
  // …and wait for every panel to STOP loading. One rAF only covered the grip; a slot still fetching or
  // drawing was captured as-is, which put a blank plot into the finished PDF with no error. Bounded, so
  // a stuck panel degrades to the old behaviour instead of hanging the export button.
  const settled = await awaitIdle(() => anyBusy([...summaryRefs.values(), ...interactiveRefs.values()]))
  if (!settled) console.warn('[board export] a panel was still loading; capturing anyway')
  // measure the grid + each slot so the PDF reproduces the ON-SCREEN layout (spans, plates, row height,
  // gaps) exactly — the board IS the layout guide, so slots land at their real proportions/positions.
  const gr = gridEl.getBoundingClientRect()
  const slots: PdfSlotOut[] = []
  try {
    for (let i = 0; i < entry.value.contents.length; i++) {
      const c = entry.value.contents[i]
      const el = slotEls[i]
      if (!c || !el) continue
      // prefer the panel's plot-only light-theme export; fall back to a white-ground DOM snapshot
      // (e.g. filmstrip/image slots, which are already screenshots) with the chrome hidden.
      // summary + cluster-heatmap panels expose exportImage()/getCsv() (summaryRefs); other interactive
      // views expose exportImage() (interactiveRefs); anything else → white-ground DOM snapshot.
      const summaryLike = c.kind === 'summary' || isClusterPanel(c.ref)
      const ref = summaryLike ? summaryRefs.get(i) : c.kind === 'interactive' ? interactiveRefs.get(i) : undefined
      let png: string | null = null
      let svg: string | null = null
      if (vector) svg = (await ref?.exportSvg?.()) ?? null    // vector when the panel provides it
      png = (await ref?.exportImage?.()) ?? null              // always capture the raster fallback
      if (!png) png = await plotHostToImageURL(el, '#ffffff')
      const csv = summaryLike ? (await summaryRefs.get(i)?.getCsv?.() ?? null) : null
      const sr = el.getBoundingClientRect()
      const rect = { x: (sr.left - gr.left) / gr.width, y: (sr.top - gr.top) / gr.height,
                     w: sr.width / gr.width, h: sr.height / gr.height }
      slots.push({ rect, png, svg, name: labelFor(c), title: (c.state.title as string) || undefined, csv })
    }
  } finally { capturing.value = false }
  return { aspect: gr.width / Math.max(1, gr.height), slots }
}
// the shown (aggregated) data for every summary slot — for the standalone CSV export (data → Prism).
// Append the panel's axis descriptor (measure ± groupBy) to the plot label so two same-type plots are
// distinguishable in the zip — "Track_measures" alone can't tell you WHICH track measure it is.
async function collectCsvs(): Promise<{ name: string; csv: string | null }[]> {
  const out: { name: string; csv: string | null }[] = []
  for (let i = 0; i < entry.value.contents.length; i++) {
    const c = entry.value.contents[i]
    if (c && (c.kind === 'summary' || isClusterPanel(c.ref))) {
      const axis = summaryRefs.get(i)?.csvName?.() ?? ''
      const base = axis ? `${labelFor(c)}_${axis}` : labelFor(c)
      out.push({ name: base, csv: await summaryRefs.get(i)?.getCsv?.() ?? null })
      // Stats sidecar: `{base}.stats.csv`. Emit only when the panel actually has a `comparisons`
      // result to serialise; skipped entries would just be empty files in the zip. See
      // STATS_ANNOTATIONS_PLAN.md → D7.
      const statsCsv = summaryRefs.get(i)?.getStatsCsv?.() ?? ''
      if (statsCsv) out.push({ name: `${base}.stats`, csv: statsCsv })
    }
  }
  return out
}
defineExpose({ capturePage, collectCsvs })
</script>

<template>
  <div class="layout-canvas">
    <div v-if="!imageUid" class="lc-empty cc-muted">Select one or more images above to plot.</div>
    <template v-else>
      <!-- controls, grouped so nothing crowds: Layout row (uniform + custom sliders), Plates row
           (varied presets, wraps to two lines), then the data/compare row. -->
      <div class="lc-bar">
        <div class="lc-row cc-row cc-row-loose">
          <span class="lc-lbl cc-eyebrow">Layout</span>
          <ChipSelect variant="segmented" :options="uniformOptions" :model-value="uniformMatchId"
                      v-tooltip.bottom="'Uniform grids'" aria-label="Uniform grid layout"
                      @update:model-value="v => applyPreset(UNIFORM_PRESETS, v as string)" />
          <!-- custom grid size + slot height, tucked into a ⚙ popover to keep the bar tidy -->
          <div class="lc-opts">
            <button ref="optsBtn" class="lc-gear cc-btn cc-btn-ghost cc-btn-icon" data-guide="board.options" :class="{ 'cc-btn-on': optsOpen }" @click="optsOpen = !optsOpen"
                    v-tooltip.bottom="'Grid size & slot height'"><i class="pi pi-sliders-h" /></button>
            <TeleportPopover v-model="optsOpen" :anchor="optsBtn">
              <div class="lc-pop">
                <label class="lc-pop-row cc-muted" v-tooltip.left="'Number of plot columns on the board'"><span>cols</span>
                  <input type="range" min="1" max="6" :value="entry.cols"
                         @input="layout.applyTemplate(canvasKey, uniform(+($event.target as HTMLInputElement).value, entry.rows))" />
                  <span class="lc-val">{{ entry.cols }}</span></label>
                <label class="lc-pop-row cc-muted" v-tooltip.left="'Number of plot rows on the board'"><span>rows</span>
                  <input type="range" min="1" max="6" :value="entry.rows"
                         @input="layout.applyTemplate(canvasKey, uniform(entry.cols, +($event.target as HTMLInputElement).value))" />
                  <span class="lc-val">{{ entry.rows }}</span></label>
                <label class="lc-pop-row cc-muted" v-tooltip.left="'Height of each plot slot in pixels'"><span>height</span>
                  <input type="range" min="160" max="720" step="10" :value="rowHeight"
                         @input="rowHeight = +($event.target as HTMLInputElement).value" />
                  <span class="lc-val">{{ rowHeight }}</span></label>
              </div>
            </TeleportPopover>
          </div>
          <!-- A4 sheet lock: keep the board at page proportions (WYSIWYG with the PDF) or let it fill -->
          <ChipSelect variant="segmented" :options="SHEET_OPTIONS" :model-value="sheet" aria-label="Sheet size"
                      v-tooltip.bottom="'A4 locks the board to page proportions; Free fills the width'"
                      @update:model-value="v => sheet = v as 'free' | 'a4-portrait' | 'a4-landscape'" />
          <!-- fit-to-view zoom (visual only; the exported page is unchanged) -->
          <CanvasZoomControl v-if="sheet !== 'free'" :zoom="zoom"
                             @update:zoom="setZoom" @fit-width="fitWidth" @fit-height="fitHeight" @reset="resetZoom" />
          <!-- clustering run: ONE per board (drives all cluster slots + the cluster pop manager) -->
          <div v-if="hasClusterSlot" class="lc-clust" v-tooltip.bottom="'Clustering run shown by this board’s cluster plots'">
            <span class="lc-lbl cc-eyebrow">cluster</span>
            <select v-model="clustPopType">
              <option value="clust">cells</option>
              <option value="trackclust">tracks</option>
            </select>
            <select v-model="clustSuffix">
              <option v-if="!clustSuffixes.length" :value="clustSuffix">{{ clustSuffix }}</option>
              <option v-for="s in clustSuffixes" :key="s" :value="s">{{ s }}</option>
            </select>
          </div>
          <!-- compare + pool sit at the right of the layout row -->
          <div class="lc-right">
            <div v-if="canCompare" class="lc-compare"
                 v-tooltip.bottom="'Compare across the selected images'">
              <span class="lc-lbl cc-eyebrow">compare</span>
              <select v-model="compareMode">
                <option value="image">this image</option>
                <option value="per_image">per image</option>
                <option value="summarised">pooled</option>
                <option value="by_attr" :disabled="!setAttrs.length">by attribute</option>
              </select>
              <template v-if="compareMode === 'by_attr'">
                <select v-model="compareAttr"><option v-for="a in setAttrs" :key="a.name" :value="a.name">{{ a.name }}</option></select>
                <template v-if="attrOptions2.length">
                  <span class="lc-x">×</span>
                  <select v-model="compareAttr2">
                    <option value="">none</option>
                    <option v-for="a in attrOptions2" :key="a.name" :value="a.name">{{ a.name }}</option>
                  </select>
                </template>
              </template>
            </div>
            <CcToggle class="lc-pool" v-model="poolGroups" label="pool to groups"
              v-tooltip.bottom="'Pool across populations and images so each plot shows one series per Split-by group only'" />
          </div>
        </div>
        <div class="lc-row cc-row cc-row-loose">
          <span class="lc-lbl cc-eyebrow">Plates</span>
          <ChipSelect variant="pill" :options="plateOptions" :model-value="plateMatchId"
                      v-tooltip.bottom="'Comic plates — varied-size panels, matched to the sheet orientation'"
                      aria-label="Comic plate layout"
                      @update:model-value="v => applyPreset(platePresets, v as string)" />
          <!-- custom plate builder: drag cells to merge into varied-size panels -->
          <div class="lc-opts">
            <button ref="builderBtn" class="cc-btn cc-btn-ghost lc-custom" :class="{ 'cc-btn-on': builderOpen }" @click="builderOpen = !builderOpen"
                    v-tooltip.bottom="'Build a custom plate — drag cells to merge, click a merge to split'">
              <i class="pi pi-th-large" /> Custom…</button>
            <TeleportPopover v-model="builderOpen" :anchor="builderBtn">
              <div class="lc-pop">
                <PlateBuilder :cols="entry.cols" :rows="entry.rows" :slot-areas="entry.slotAreas"
                              @apply="applyCustomPlate" @cancel="builderOpen = false" />
              </div>
            </TeleportPopover>
          </div>
        </div>
      </div>

      <div class="lc-body">
        <!-- scroll viewport → .lc-zoom (scaled footprint, centred) → the grid (visually scaled) -->
        <div ref="canvasWrapRef" class="lc-canvas-wrap">
        <div class="lc-zoom" :style="zoomWrapStyle">
        <div ref="gridRef" class="lc-grid" :class="{ capturing }" :style="[gridStyle, gridZoomStyle]">
          <!-- reorder drag: the drag SOURCE is the panel header's drag icon (CanvasPanel, docked);
               its native dragstart bubbles here, so the grip lives IN the header (aligned with the
               other buttons) instead of a fragile absolute overlay that collided with the pin. -->
          <div v-for="(area, i) in entry.slotAreas" :key="i" class="lc-slot"
               :class="{ active: i === entry.activeIndex, filled: !!entry.contents[i] }"
               :style="{ gridArea: area }"
               @dragstart="dragFrom.i = i" @dragend="dragFrom.i = -1"
               @dragover.prevent @drop.prevent="onDrop(i)"
               @mousedown="layout.setActive(canvasKey, i)">
            <!-- per-slot title (figure caption) — persisted in the slot's state, drawn above the plot
                 in the PDF export (pdf.ts). Only for filled slots. -->
            <input v-if="entry.contents[i]" class="lc-slot-cap" :value="slotTitle(i)" v-tooltip.top="'Caption drawn above this plot on export'"
                   @input="setSlotTitle(i, ($event.target as HTMLInputElement).value)"
                   @mousedown.stop placeholder="Add a title…" />
            <div class="lc-slot-plot">
            <!-- summary plot -->
            <SummaryPanel v-if="entry.contents[i]?.kind === 'summary' && specById[entry.contents[i]!.ref]"
                          :ref="el => setSummaryRef(i, el)"
                          :index="i" :active="i === entry.activeIndex" :docked="true" :arrange="null"
                          :spec="specById[entry.contents[i]!.ref]"
                          :project-uid="projectUid" :image-uid="imageUid"
                          :set-uid="panelSetUid" :image-uids="panelImageUids" :scope="panelScope"
                          :group-attr="panelGroupAttr" :series="panelSeries(i, entry.contents[i]!)" :series-color="seriesColor"
                          :vis="panelVis(entry.contents[i]!)" :ui="entry.contents[i]!.state" :collapse-series="poolGroups"
                          :reload-token="reloadToken" :persist-key="`${canvasKey}:slot:${i}`"
                          @activate="layout.setActive(canvasKey, i)" @remove="clearSlot(i)" @duplicate="duplicateSlot(i)"
                          @readout="readouts[i] = $event" />
            <!-- cluster PANEL (heatmap / HMM …) — rendered GENERICALLY from the CLUSTER_PANELS registry
                 (docked, board's single cluster run); no per-plot branch -->
            <component v-else-if="entry.contents[i] && isClusterPanel(entry.contents[i]!.ref)"
                       :is="CLUSTER_PANELS[entry.contents[i]!.ref].component" :ref="(el: unknown) => setSummaryRef(i, el)"
                       v-bind="clusterPanelProps(i)"
                       @activate="layout.setActive(canvasKey, i)" @remove="clearSlot(i)" @duplicate="duplicateSlot(i)" />
            <!-- interactive plot (UMAP / gating-strategy / …) -->
            <InteractivePanel v-else-if="entry.contents[i]?.kind === 'interactive' && INTERACTIVE_VIEWS[entry.contents[i]!.ref]"
                              :ref="el => setInteractiveRef(i, el)"
                              :index="i" :active="i === entry.activeIndex" :docked="true"
                              :view="entry.contents[i]!.ref" :context="ctxFor(entry.contents[i]!, i)" :state="entry.contents[i]!.state"
                              :duplicable="true" :persist-key="`${canvasKey}:slot:${i}`"
                              @activate="layout.setActive(canvasKey, i)" @remove="clearSlot(i)" @duplicate="duplicateSlot(i)" />
            <!-- empty slot: add a plot (summary spec or interactive view) -->
            <div v-else class="lc-add">
              <select data-guide="board.addPlot" v-tooltip.bottom="'Add a plot to this slot'"
                      @change="addPlot(i, ($event.target as HTMLSelectElement).value); ($event.target as HTMLSelectElement).value = ''">
                <option value="">+ Plot…</option>
                <optgroup label="Summary">
                  <option v-for="s in specs" :key="s.id" :value="`summary:${s.id}`">{{ s.label }}</option>
                </optgroup>
                <optgroup v-if="interactiveOptions.length" label="Interactive">
                  <option v-for="v in interactiveOptions" :key="v.key" :value="`interactive:${v.key}`">{{ v.label }}</option>
                </optgroup>
                <optgroup v-if="clusterOptions.length" label="Clustering">
                  <option v-for="v in clusterOptions" :key="v.key" :value="`interactive:${v.key}`">{{ v.label }}</option>
                </optgroup>
                <optgroup v-if="imageOptions.length" label="Image">
                  <option v-for="v in imageOptions" :key="v.key" :value="`interactive:${v.key}`">{{ v.label }}</option>
                </optgroup>
              </select>
              <span class="lc-add-hint cc-muted cc-fs-xs">empty slot</span>
            </div>
            </div>
          </div>
        </div>
        </div>
        </div>

        <!-- docked MANAGER (control, not content — excluded from PDF). Follows the ACTIVE slot, and
             WHICH manager comes from that plot's registry `rail` — never a key list here. `'none'`
             still renders the picker for its styling block + scope footer, with the (dead) population
             list suppressed. -->
        <div class="lc-rail" data-guide="board.rail">
          <FlowModelVault v-if="activeRail === 'flowModels'" :docked="true"
                          :selected="activeFlowModel" :scope="scope"
                          @update:selected="setFlowModel" @update:scope="scope = $event" />
          <PopulationManager v-else-if="activeIsCluster" :docked="true" :readonly="true"
                             :selected="''" :highlighted="activeClustHl" :scope="scope"
                             :line-width="1" :gate-labels="false" :axis-from-zero="false"
                             :pop-type="clustPopType" :cluster-ids="clustClusterIds[clustSuffix] ?? []"
                             :suffix="clustSuffix" :vis="activeVis"
                             @update:scope="scope = $event" @update:vis="setVis" @toggle-highlight="toggleClustHl" />
          <SeriesPicker v-else :groups="segPops" :selected="activeSel" :scope="scope" :vis="activeVis" :docked="true"
                        :readout="activeReadout" :selection-unused="activeIsPrecomputed || activeRail === 'none'"
                        :unused-note="activeRail === 'none' && !activeIsPrecomputed ? 'This plot picks its own data.' : undefined"
                        @toggle="toggleTarget" @update:scope="scope = $event" @update:vis="setVis" />
        </div>
      </div>
    </template>
  </div>
</template>

<style scoped>
.layout-canvas { display: flex; flex-direction: column; }
.lc-empty { padding: 20px; }   /* + .cc-muted */
.lc-bar { display: flex; flex-direction: column; gap: 6px; padding: 8px 4px; font-size: var(--cc-fs-sm); flex-shrink: 0; }

/* sits right after the sliders (NOT pushed to the far right) so it doesn't shift when the compare
   dropdown changes width (e.g. "by attribute" adds selects) */
.lc-right { display: flex; align-items: center; gap: 10px; }

.lc-sep { width: 1px; height: 1.4rem; background: var(--cc-border); }
.lc-nm { display: inline-flex; align-items: center; gap: 6px; color: var(--cc-text-dim); }
.lc-nm input[type="range"] { width: 5rem; }
.lc-clust { display: inline-flex; align-items: center; gap: 6px; color: var(--cc-text-dim);
  padding: 2px 8px; border: 1px solid var(--cc-border); border-radius: var(--cc-radius-md); }
.lc-clust select { font-size: var(--cc-fs-xs); }
/* ⚙ grid-size / height popover */
.lc-opts { position: relative; display: inline-flex; }
/* .lc-gear → cc-btn cc-btn-ghost cc-btn-icon */
.lc-gear:hover { color: var(--cc-text); border-color: var(--cc-accent-strong); }
.lc-custom { font-size: var(--cc-fs-xs); padding: 0.22rem 0.55rem; }
/* inner layout only — TeleportPopover provides surface/border/shadow/position */
.lc-pop { min-width: 13rem; display: flex; flex-direction: column; gap: 8px; }   /* padding: TeleportPopover */
.lc-pop-row { display: flex; align-items: center; gap: 8px; }
.lc-pop-row span:first-child { width: 3rem; }
.lc-pop-row input[type="range"] { flex: 1; }
.lc-val { min-width: 0.9rem; text-align: center; font-weight: 700; color: var(--cc-text); }
.lc-compare { display: inline-flex; align-items: center; gap: 6px; color: var(--cc-text-dim);
  padding: 2px 8px; border: 1px solid var(--cc-border); border-radius: var(--cc-radius-md); }
.lc-x { opacity: 0.6; }
.lc-pool { display: flex; align-items: center; gap: 6px; color: var(--cc-text-dim); }
/* the board sizes to its grid (rowHeight × rows); the page's panel-scroll handles overflow */
.lc-body { display: flex; align-items: flex-start; gap: 8px; }
/* scroll viewport for the board. .lc-zoom holds the (visually scaled) footprint: fixed-size + centred
   via margin auto for an A4 board, full-width for a Free board (styled inline via zoomWrapStyle). */
.lc-canvas-wrap { flex: 1; min-width: 0; overflow: auto; }
.lc-zoom { display: block; }
.lc-grid { flex: 1; display: grid; gap: 8px; padding: 4px; overflow: hidden; }
.lc-slot { position: relative; border: 1px dashed var(--cc-border); border-radius: var(--cc-radius-md); overflow: hidden;
  display: flex; flex-direction: column; min-width: 0; min-height: 0; background: var(--cc-bg); }
/* per-slot title (figure caption): a plain-looking, centred, editable line above the plot */
.lc-slot-cap { flex: 0 0 auto; width: 100%; box-sizing: border-box; border: none; background: transparent; font-weight: 600; text-align: center; padding: 3px 6px 1px; }
.lc-slot-cap::placeholder { color: var(--cc-text-dim); font-weight: 400; opacity: 0.55; }
/* the plot area fills the rest of the slot (was the slot itself before the caption was added) */
.lc-slot-plot { flex: 1; min-width: 0; min-height: 0; display: flex; position: relative; }
.lc-slot.filled { border-style: solid; }
/* selection = amber, matching CanvasPanel .panel.active and every module page (was a clashing violet).
   A FILLED slot's panel already draws the amber border + glow, so don't double it there — only an empty
   slot needs its own amber selection border. */
.lc-slot.active { border-color: var(--cc-selected); }
.lc-slot.filled.active { border-color: var(--cc-border); }
/* reorder drag handle now lives IN the panel header (CanvasPanel docked drag icon); its native
   dragstart bubbles to .lc-slot (@dragstart above). No absolute overlay grip here anymore. */
.lc-add { flex: 1; display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 6px; }
.lc-add-hint { opacity: 0.6; }
/* stick to the top of the scroll viewport so the pop manager stays reachable as the (tall) board
   scrolls past — otherwise you must scroll back up to change the selection. align-self so the sticky
   box hugs the top of the flex row; its own overflow-y scrolls a manager taller than the viewport. */
.lc-rail { flex-shrink: 0; width: 300px; overflow-y: auto; padding-right: 10px; box-sizing: content-box;
  position: sticky; top: 8px; align-self: flex-start; max-height: calc(100vh - 16px); }
</style>
