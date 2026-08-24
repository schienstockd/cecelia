<!--
  Gating workspace below the image table — the ONE gating canvas, reused for both flow gating (Gate
  page, popType="flow") and track-property gating (Tracking page, popType="track"). Page-level
  controls: segmentation (value_name) select + a "+ Plot" button (no fixed count). Plots are
  free-floating, draggable, resizable boxes (GatePlotPanel → CanvasPanel) added/removed dynamically;
  each has a "−" to remove itself. The floating PopulationManager sits on top. Per-plot state
  (displayed parent, local highlight) is owned here keyed by a stable id (via useCanvasPanels), so
  the manager can drive the ACTIVE plot.

  popType only changes (a) the data source the store/API reads (flow cells vs the per-track table —
  handled server-side) and (b) the napari overlay: flow offers cell-selection linked brushing
  (transient pops) + Points layers; track offers a "Show tracks" push (napari Tracks layers). The
  panel/manager components and the workspace logic (useCanvasPanels: add/remove, Tile/Cascade,
  active panel) are shared as-is — no track-specific clone.
-->
<script setup lang="ts">
import { DOT_R } from '../../plots/density'
import { toggleSelected, narrowToSingle } from '../../utils/selection'
import { ref, computed, watch, provide, onMounted, onUnmounted, useTemplateRef } from 'vue'
import CanvasArrangeButtons from '../../components/canvas/CanvasArrangeButtons.vue'
import { useGatingStore } from '../../stores/gating'
import { useWsStore } from '../../stores/ws'
import { useProjectStore } from '../../stores/project'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useNapariOpen } from '../../composables/useNapariOpen'
import { useCanvasPanels } from '../../composables/useCanvasPanels'
import { useCanvasWorkspace } from '../../composables/useCanvasWorkspace'
import { useViewState } from '../../composables/useViewState'
import { useCanvasZoom, CANVAS_ZOOM_KEY } from '../../composables/useCanvasZoom'
import GatePlotPanel from './GatePlotPanel.vue'
import InteractivePanel from '../../components/canvas/InteractivePanel.vue'
import { isInteractiveView, pageViews, migrateViewKey, railFor, popTypesFor, singlePopFor }
  from '../../components/canvas/interactiveViews'
import SeriesPicker from '../../components/canvas/SeriesPicker.vue'
import { fetchSegmentationPops } from '../../plots/populations'
import { granularityFor } from '../../plots/popTypes'
import { usePopFamily } from '../../composables/usePopFamily'
import { tkey, seriesMemo } from '../../plots/series'
import { useDataRefresh } from '../../composables/useDataRefresh'
import type { SegmentationPops } from '../../plots/types'
import { readCanvasTrackSelection, EMPTY_TRACK_SELECTION, type CanvasTrackSelection }
  from '../../lib/trackSelection'
import GatePairsPanel from './GatePairsPanel.vue'
import PopulationManager from '../../components/canvas/PopulationManager.vue'
import CanvasZoomControl from '../../components/canvas/CanvasZoomControl.vue'
import GatingCopyDialog from './GatingCopyDialog.vue'
import type { FlatPop } from '../../stores/gating'

const props = withDefaults(defineProps<{
  imageUid: string | null
  popType?: string
  orderedUids?: string[]                        // visible images in table order (for prev/next)
  selectUids?: (uids: string[]) => void          // drive the table selection (ModuleLayout)
}>(), { popType: 'flow', orderedUids: () => [] })
const isTrack = computed(() => props.popType === 'track')
const g = useGatingStore()
const ws = useWsStore()
const project = useProjectStore()
const { openInNapari } = useNapariOpen()

// ── Scope ─────────────────────────────────────────────────────────────────────
// EVERY manager option (highlighted pops, gate labels, line width, axis) obeys this:
// GLOBAL → one shared value applied to all plots; LOCAL → the active plot's own value.
// per-plot copies of every scoped option (used when scope = 'local')
// per-plot copies also carry the axis config (channels/transforms/render mode) so those persist per
// plot across navigation — like the summary panels' `ui` bag. Bare refs in the panel reset on remount.
type GateKind = 'linear' | 'log' | 'asinh' | 'logicle'
// A panel is either a single gate scatter (default, drawable) or a read-only channel-pairs matrix.
// `channels` is the pairs plot's selected list; the single plot ignores it (and vice-versa for x/y).
// index signature so a panel's state is assignable to the generic InteractivePanel's
// `Record<string, unknown>` state (the correction view reads its own keys) — as in ClusterPlots.
interface PlotState { [key: string]: unknown; kind: string; parent: string; hl: string[]; lineWidth: number; labels: boolean; fromZero: boolean; dotSize: number
  x: string; y: string; xt?: GateKind; yt?: GateKind; renderMode: 'points' | 'contour' | 'outliers'; channels: string[]
  // colour-by (single plot): the third measure painted as the dot colour, and its ramp scale.
  // Unset like xt/yt so the panel's per-measure transform default fires (see the comment on ckey).
  z?: string; zt?: GateKind }
const canvasRef = useTemplateRef<HTMLElement>('canvasRef')   // the visible viewport (zoom + fit measure it)
const zoomRef = useTemplateRef<HTMLElement>('zoomRef')       // the scaled workspace (panels' offsetParent)
// Per-image + segmentation: gating populations are per-value_name, so each (image, segmentation) keeps
// its own plots/parents/highlights and the canvas rebinds when either the image or the segmentation
// (g.valueName) changes.
const ckey = computed(() => `gate:${props.popType}:${props.imageUid ?? 'none'}:${g.valueName}`)
// Axis transforms (xt/yt) are intentionally LEFT UNSET here so the panels' own per-axis default
// fires: GatePlotPanel/GatePairsPanel resolve `ui.xt ?? axisDefaultTransform(col)` (linear for
// spatial/centroid axes via the store's defaultTransformFor, logicle for flow intensities). That fallback
// only runs while ui.xt is undefined — pre-seeding a concrete transform here would pin logicle and
// silently defeat it. Channels (x/y) start empty; the panel picks index-based defaults once columns
// load (see ensureChannels).
const { panels, activeId, activePanel, shared, add, remove, removeAll, arrangeGrid, arrangeCascade, contentBounds } =
  useCanvasPanels<PlotState>(zoomRef, () =>
    ({ kind: 'single', parent: 'root', hl: [], lineWidth: 1.5, labels: true, fromZero: true, dotSize: DOT_R,
       x: '', y: '', renderMode: 'points', channels: [] }), ckey,
    // every plot panel here is `:square` (GatePlotPanel / GatePairsPanel), so Tile hands out square
    // cells; `tileBox` keeps the grid sized to the VIEWPORT even once the workspace has grown taller
    // than it — see utils/tileGrid.ts
    { squareCells: true, tileBox: () => workspaceBase.value })
// show/hide the floating population manager — persisted per canvas in the `shared` bag (default shown)
const showManager = computed<boolean>({ get: () => (shared.value.showManager as boolean) ?? true, set: v => (shared.value.showManager = v) })

// visual zoom (shared control): scale the free-floating plot workspace to see everything at once. Fit
// fits the actual plot bounding box; drag is zoom-corrected via the injected zoom. The workspace GROWS
// when zoomed out (useCanvasWorkspace); the population manager sits OUTSIDE the zoom layer (full-size).
const { zoom, fitWidth, fitHeight, setZoom, reset: resetZoom } = useCanvasZoom(canvasRef,
  () => ({ w: contentBounds.value.w || null, h: contentBounds.value.h }))
provide(CANVAS_ZOOM_KEY, zoom)
const { workspaceStyle, workspaceBase } = useCanvasWorkspace(canvasRef, zoom,
  // grow the workspace to hold the plots (a tall Tile grid scrolls instead of spilling);
  // a getter, so it may name `contentBounds` from the line above
  () => contentBounds.value)
// add a read-only channel-pairs matrix panel (same canvas, same shared options as a single plot)
function addPairs() { const id = add(); const p = panels.value.find(x => x.id === id); if (p) p.state.kind = 'pairs' }
// A canvas persisted before a view was renamed still holds the old key; without this its panel falls
// through the template's `v-if` chain and renders as a GATING PLOT carrying someone else's state.
for (const p of panels.value) migrateViewKey(p.state as { kind: string })

// TRACK ONLY. The correction workspace is a panel on this canvas rather than a page of its own: it is
// read beside the track gating it will change, and a canvas panel already collapses, persists, zooms
// and exports. It is the one panel here that MUTATES — see TrackSchemeView's header.
// the correction worklist needs the project it is correcting; the gating canvas otherwise never has
// to name it (every gating call is keyed by image + value name).
const projectMeta = useProjectMetaStore()
const projectUid = computed(() => projectMeta.current?.uid ?? '')
function addView(key: string) {
  if (!key) return
  const id = add(); const p = panels.value.find(x => x.id === id); if (p) p.state.kind = key
}
// what the "+ Track…" picker offers — the registry's `trackPage` flag, never a key list here
const trackOptions = computed(() => pageViews('trackPage'))
// what a registry view needs from the page (the panel's own state carries the rest) — the same
// {projectUid, imageUids, setUid} contract the cluster and optical-flow canvases pass
/**
 * What a registry view gets from this page.
 *
 * A view on the POPULATION rail is driven by the rail's SERIES PICKER (below), in the same vocabulary
 * the Analysis board uses: a series is `{popType, valueName, pop}` and the picker's rows carry all
 * three. Without it the track views fell back to a private segmentation `<select>`, which is a second
 * picker for a job this page already has a canonical one for, and the wrong noun besides: you pick
 * TRACKS, not the label set they were measured on.
 *
 * **It was wired to the gating tree first, and that could not work.** The tree has no popType — the
 * canvas had to invent one (`props.popType`, so `track`) while a track panel resolves its family from
 * its registry entry (so `live`, the first one declared). `filterSeriesToPopType` then correctly
 * dropped every population the user had ticked, and the panels silently fell back to the whole
 * segmentation: the picker was on screen, was clicked, and reached nothing. Two managers is not
 * chrome, it is the difference between "which tree am I editing" (one, mutated) and "which
 * populations am I plotting" (any number, across segmentations).
 *
 * `compareMode: 'image'` because this canvas is one image by construction — the cohort comparison is
 * the board's job, and pretending otherwise here would silently widen what a gating plot shows.
 */
function ctxForView(key: string, id: number) {
  const base = { projectUid: projectUid.value, imageUids: props.imageUid ? [props.imageUid] : [],
                 setUid: null }
  const pops = railFor(key) === 'pops'
    ? { series: memoSeries(id, panelPopSel(id)),
        popTypes: popTypesFor(key), compareMode: 'image' as const, poolGroups: false }
    : {}
  return { ...base, ...pops, ...trackLink.value }
}

const trackLink = computed(() => ({
  // the shared selection, plus the one way to change it. A setter in the context rather than an event
  // on InteractivePanel: the panel is generic infrastructure and must not learn what a track is.
  trackSel: readCanvasTrackSelection(selTracks.value),
  setTrackSel: (v: CanvasTrackSelection) => { selTracks.value = v },
}))

// global-scope values live in the canvas `shared` bag via useViewState, so they PERSIST across
// navigation with no per-field wiring (the highlighted pops were resetting on remount). Add an
// option to the defaults below and it persists automatically — see useViewState.ts.
const { scope, hl: gHL, lineWidth: gLineWidth, labels: gLabels, fromZero: gFromZero, dotSize: gDotSize,
        selTracks, trackPops } = useViewState(shared, {
  scope: 'global' as 'global' | 'local',     // global = one value for every plot; local = active plot only
  hl: [] as string[],                         // global-scope highlighted pop paths
  lineWidth: 1.5, labels: true, fromZero: true,
  // dot radius on the scatters (plots/density DOT_R = the FlowJo speckle). Scoped like every other
  // plot option, so one slider can grow the dots on all plots or just the active one.
  dotSize: DOT_R,
  // WHICH POPULATIONS the registry views plot (`popType::valueName/pop` keys, the board's own
  // vocabulary). Separate from `hl` above: that one is "overlay this population on the gating
  // scatter", which is a different question with a different answer, and reusing it is what left the
  // track panels unable to say which family their populations belonged to.
  trackPops: [] as string[],
  // WHICH TRACKS THE CANVAS IS TALKING ABOUT — the cross-panel link. Selecting lanes in the timeline
  // is the same act as choosing what the x/y track plot draws and what napari flies to, so the
  // selection lives on the CANVAS rather than in one panel's state. Same mechanism the highlighted
  // populations already use, so it persists across navigation with no extra wiring.
  //
  // It carries its SCOPE (image + segmentation), not just ids: a track id means nothing on its own —
  // see lib/trackSelection.ts for the empty plot that taught us so.
  selTracks: { ...EMPTY_TRACK_SELECTION },
})

// effective value for a given plot (what the panel renders with)
const panelHL = (s: PlotState) => scope.value === 'global' ? gHL.value : s.hl
const panelLineWidth = (s: PlotState) => scope.value === 'global' ? gLineWidth.value : s.lineWidth
const panelLabels = (s: PlotState) => scope.value === 'global' ? gLabels.value : s.labels
const panelFromZero = (s: PlotState) => scope.value === 'global' ? gFromZero.value : s.fromZero
const panelDotSize = (s: PlotState) => scope.value === 'global' ? gDotSize.value : (s.dotSize ?? DOT_R)

// what the manager shows/edits = the active scope's value
const activeHL = computed(() => scope.value === 'global' ? gHL.value : (activePanel.value?.state.hl ?? []))
const activeLineWidth = computed(() => scope.value === 'global' ? gLineWidth.value : (activePanel.value?.state.lineWidth ?? 1.5))
const activeLabels = computed(() => scope.value === 'global' ? gLabels.value : (activePanel.value?.state.labels ?? true))
const activeFromZero = computed(() => scope.value === 'global' ? gFromZero.value : (activePanel.value?.state.fromZero ?? true))
const activeDotSize = computed(() => scope.value === 'global' ? gDotSize.value : (activePanel.value?.state.dotSize ?? DOT_R))

// edits route to the global value or the active plot depending on scope
// the ONE selection toggle (utils/selection.ts) — four hosts had a copy of it each
const toggle = (arr: string[], v: string) => toggleSelected(arr, v)

// ── The rail's SECOND manager: which track populations the track panels plot ──────────────────
//
// `CANVAS_MANAGER_RAIL_PLAN.md` Decision 5 says module pages render ONE manager statically, and its
// stated reason is "indirection with no second case". This canvas is the second case: it hosts three
// registry views that slice by population and one gating tree that is edited, and those are two
// different questions asked of two different components (see `ctxForView`). So the rail follows the
// ACTIVE panel, exactly as the board's does — via `railFor`, never a key list here.
const activeIsPopsView = computed(() =>
  !!activePanel.value && isInteractiveView(activePanel.value.state.kind)
  && railFor(activePanel.value.state.kind) === 'pops')

// The active panel's FAMILY, through the same `usePopFamily` the panel itself uses — so the rail lists
// the populations that panel can actually draw, and the two cannot name different families.
const { popType: activeFamily } = usePopFamily(
  () => (activePanel.value && isInteractiveView(activePanel.value.state.kind)
    ? popTypesFor(activePanel.value.state.kind) : undefined),
  () => activePanel.value?.state.popType as string | undefined,
  v => { if (activePanel.value) activePanel.value.state.popType = v })
const activeGranularity = computed<'cell' | 'track'>(() => {
  const pt = activePanel.value && isInteractiveView(activePanel.value.state.kind)
    ? popTypesFor(activePanel.value.state.kind) : []
  return pt.length ? granularityFor({ dataSource: { popTypes: pt } }, activeFamily.value) : 'track'
})

// the populations available, grouped by segmentation — the picker's rows. ONE reader
// (`plots/populations.ts`), shared with every summary canvas.
//
// Asked for the SEGMENTATION THIS CANVAS IS ON (`g.valueName`, the toolbar select). Without it the
// reader answers for the whole image — right for the summary canvas, which overlays segmentations on
// purpose, wrong here: the tree, the plots and the copy dialog are all scoped to the one selected
// value_name, so an unscoped rail contradicted the toolbar (working on flowTom listed memTom's tracks
// and the mask-less imported track sets). Those still live in the segmentation select, which lists
// every labelProps registry entry — one place, reachable, not in front of every canvas. Sent as a
// query param rather than filtered here, because the server evaluates each tracked segmentation's
// gates to build the list and there is no reason to buy eight answers to throw away.
const segPops = ref<SegmentationPops[]>([])
async function loadSegPops() {
  if (!activeIsPopsView.value) { segPops.value = []; return }
  segPops.value = await fetchSegmentationPops({
    projectUid: projectUid.value, imageUids: props.imageUid ? [props.imageUid] : [],
    setUid: null, valueName: g.valueName || null,
    popType: activeFamily.value, granularity: activeGranularity.value })
}
watch([() => props.imageUid, projectUid, activeIsPopsView, activeFamily, activeGranularity,
       () => g.valueName], loadSegPops, { immediate: true })
// gating, tracking and correction all change which populations EXIST — the one refresh chokepoint, so
// the global autoRefreshOnTask setting governs this list like every plot on the page.
useDataRefresh(() => (props.imageUid ? [props.imageUid] : []), loadSegPops)
// a population the user gates by hand appears on the popmap broadcast, not on a task completion
watch(() => g.flat.length, () => { if (activeIsPopsView.value) loadSegPops() })

// The ticked series, obeying the canvas's existing global/local scope: one set for every panel, or the
// active panel's own. Same shape as the board's `gSel` / per-slot `sel`.
//
// The per-panel key is `popSel`, NOT `sel` — `TrackSchemeView` already owns `state.sel` for the LANES
// it has selected, and two different selections under one key is a silent collision.
function panelPopSel(id: number): string[] {
  if (scope.value === 'global') return trackPops.value
  return (panels.value.find(p => p.id === id)?.state.popSel as string[] | undefined) ?? []
}
const activePopSel = computed(() => activePanel.value ? panelPopSel(activePanel.value.id) : [])
// ONE population at a time when the active view says so (`singlePop`) — picking replaces instead of
// adding, and picking the one already lit clears it. The timeline is the case: several populations
// resolve to several groups and it can only draw one, so the extra ticks were input it silently threw
// away. Its two siblings facet, so they keep the multi-select they need.
const activeSinglePop = computed(() =>
  !!activePanel.value && isInteractiveView(activePanel.value.state.kind)
  && singlePopFor(activePanel.value.state.kind))
function togglePop(valueName: string, pop: string, pt: string) {
  const k = tkey(pt, valueName, pop)
  const next = (cur: string[]) => toggleSelected(cur, k, { single: activeSinglePop.value })
  if (scope.value === 'global') trackPops.value = next(trackPops.value)
  else if (activePanel.value) activePanel.value.state.popSel = next(panelPopSel(activePanel.value.id))
}
// The policy can change UNDER a selection: this rail follows the active panel, so ticking three
// populations for a facetting plot and then clicking the timeline leaves three ticked. Narrow instead
// of drawing one of them and saying nothing — which is the bug `singlePop` exists to prevent.
watch(activeSinglePop, single => {
  if (!single) return
  if (scope.value === 'global') trackPops.value = narrowToSingle(trackPops.value)
  else if (activePanel.value) activePanel.value.state.popSel = narrowToSingle(activePopSel.value)
})
// keyed by PANEL ID so each panel keeps one series array while its ticks are unchanged — a
// template-built `.map(parseTkey)` would hand every panel a "new" list on every canvas render.
const memoSeries = seriesMemo<number>()

function toggleHighlight(path: string) {
  if (scope.value === 'global') gHL.value = toggle(gHL.value, path)
  else if (activePanel.value) activePanel.value.state.hl = toggle(activePanel.value.state.hl, path)
}
function setLineWidth(v: number) { if (scope.value === 'global') gLineWidth.value = v; else if (activePanel.value) activePanel.value.state.lineWidth = v }
function setDotSize(v: number) { if (scope.value === 'global') gDotSize.value = v; else if (activePanel.value) activePanel.value.state.dotSize = v }
function setLabels(v: boolean)   { if (scope.value === 'global') gLabels.value = v;    else if (activePanel.value) activePanel.value.state.labels = v }
function setFromZero(v: boolean) { if (scope.value === 'global') gFromZero.value = v;  else if (activePanel.value) activePanel.value.state.fromZero = v }

// manager highlights the ACTIVE plot's population; clicking it again resets to root
const selected = computed(() => activePanel.value?.state.parent ?? 'root')
function setParent(id: number, v: string) { const p = panels.value.find(x => x.id === id); if (p) p.state.parent = v }
function onPickPop(path: string) {
  const s = activePanel.value?.state
  if (s) s.parent = s.parent === path ? 'root' : path
}

// Prev/next image navigation: step the table selection through the visible image list, so gating a
// batch is just << / >>, not a manual re-pick each time. Stops at the ends (no wrap) — the buttons
// disable there. Changing the selection re-drives imageUid via ModuleLayout (see `load`).
const navIndex = computed(() => props.imageUid ? props.orderedUids.indexOf(props.imageUid) : -1)
const hasPrev  = computed(() => navIndex.value > 0)
const hasNext  = computed(() => navIndex.value >= 0 && navIndex.value < props.orderedUids.length - 1)
function navTo(delta: number) {
  const i = navIndex.value + delta
  if (i < 0 || i >= props.orderedUids.length) return
  const uid = props.orderedUids[i]
  props.selectUids?.([uid])                    // switch the gating plots to the next image
  // follow along in the viewer IF napari is currently showing an image — so gating a batch keeps the
  // image in sync too, not just the plot. Don't force-launch napari when it isn't open.
  if (project.napariImageUid) openInNapari(uid, setUid.value)
}

// "Copy gating strategy to other images" dialog (per current pop type; see GatingCopyDialog).
const showCopy = ref(false)
const setUid = computed(() => g.napariSetUid())   // the set the gated image belongs to

// Defining-plot: open the plot where a pop's gate was drawn — a new single panel showing the pop's
// PARENT (the cloud the gate was drawn on) on the gate's own channels + transforms. The gate outline
// appears automatically (it's a child of that parent, server-projected). Works for flow or track.
function showDefiningPlot(pop: FlatPop) {
  if (!pop.gate) return
  const id = add()
  const p = panels.value.find(x => x.id === id)
  if (!p) return
  p.state.kind = 'single'
  p.state.parent = pop.parent
  p.state.x = pop.gate.x_channel
  p.state.y = pop.gate.y_channel
  p.state.xt = pop.gate.x_transform.kind
  p.state.yt = pop.gate.y_transform.kind
}

async function load() {
  if (props.imageUid) await g.selectImage(props.imageUid, g.valueName, props.popType)
}
function onBroadcast(d: unknown) { g.applyBroadcast(d as any) }

watch(() => props.imageUid, load)
// a transient pop (napari cell selection) appears → auto-highlight it on every plot so the
// spatially-selected cells light up in channel space immediately (linked brushing)
watch(() => g.transientPaths, (paths) => {
  for (const p of paths) if (!gHL.value.includes(p)) gHL.value = [...gHL.value, p]
}, { deep: true })
// a pop disappeared (cleared napari selection, deleted pop) → drop it from highlights and any
// plot displaying it. Without this a stale highlight keeps showPops true, so the base plot stays
// dimmed/flat with no overlay to load (grey) instead of reverting to pseudocolour/contour.
watch(() => g.flat.map(p => p.path).join('\n'), () => {
  const exist = new Set(g.flat.map(p => p.path))
  gHL.value = gHL.value.filter(p => exist.has(p))
  for (const p of panels.value) {
    p.state.hl = p.state.hl.filter(x => exist.has(x))
    if (p.state.parent !== 'root' && !exist.has(p.state.parent)) p.state.parent = 'root'
  }
})
// WS (re)connect resync: the transient napari-selection pop lives ONLY in the server's in-memory
// registry (never persisted — see docs/POPULATION.md), so a backend restart wipes it. But the client's
// tree (and the persisted highlight referencing it) survive, so without a resync the stale selection
// keeps a plot greyed on the same image. On a RECONNECT (not the first connect — onMounted already
// loaded) refetch the popmap; the fresh tree drops the transient pop and the prune watch above clears
// the dangling highlight. `everConnected` seeded from the current status so a reconnect is detected even
// when the page mounts already-connected.
let everConnected = ws.status === 'connected'
watch(() => ws.status, (s) => {
  if (s !== 'connected') return
  if (everConnected && props.imageUid) load()
  everConnected = true
})
onMounted(() => { ws.on('gating:popmap', onBroadcast); load() })
// Seed two starter plots for any (image, segmentation) that has none yet — on first bind AND after an
// image/segmentation switch (the reactive key rebinds to a fresh entry; the component doesn't remount).
// Gated on valueNames being loaded so we don't seed a transient placeholder key, and skipped for
// restored canvases (they come back non-empty). Persisted per (image, value_name), so no 2→4→6 stacking.
watch([ckey, () => g.valueNames.length], () => {
  if (props.imageUid && g.valueName && g.valueNames.includes(g.valueName) && panels.value.length === 0) {
    add(); add()
  }
}, { immediate: true })
onUnmounted(() => ws.off('gating:popmap', onBroadcast))
</script>

<template>
  <div class="gating-plots">
    <div v-if="!props.imageUid" class="gp-empty cc-muted">Select one image above to gate.</div>
    <template v-else>
      <div class="gp-bar">
        <label>segmentation
          <select data-guide="gate.segmentation" v-model="g.valueName" v-tooltip.bottom="'Which segmentation (labelProps) to gate on'"
                  @change="g.selectImage(props.imageUid!, g.valueName, props.popType)">
            <option v-for="v in g.valueNames" :key="v" :value="v">{{ v }}</option>
          </select>
        </label>
        <button class="cc-btn cc-btn-primary" data-guide="gate.addPlot" v-tooltip.bottom="'Add a plot'" @click="add">
          <i class="pi pi-plus" /> Plot
        </button>
        <button class="cc-btn cc-btn-primary" v-tooltip.bottom="'Add a read-only channel-pairs matrix'"
                @click="addPairs">
          <i class="pi pi-plus" /> Pairs
        </button>
        <!-- TRACK: the paths themselves, and what looks wrong in them. ONE picker, not one button
             each — four `+ Xxx` buttons made this bar wider than the window. Built from the registry's
             `trackPage` flag (`pageViews`), so a new track view appears here with no edit to this file;
             a hardcoded key list here is the same silently-dead-checkbox bug the registry warns about. -->
        <select v-if="isTrack && trackOptions.length" class="gp-add"
                v-tooltip.bottom="'Add a track plot'" aria-label="Add a track plot"
                @change="addView(($event.target as HTMLSelectElement).value); ($event.target as HTMLSelectElement).value = ''">
          <option value="">+ Track…</option>
          <option v-for="v in trackOptions" :key="v.key" :value="v.key">{{ v.label }}</option>
        </select>
        <!-- FLOW: spatial cell-selection brush (linked brushing → transient cell pop). -->
        <div v-if="!isTrack" class="cc-btn-group">
          <!-- showing populations in napari is the ViewerPanel's palette toggle (remembered);
               here we only offer the spatial cell-selection brush. -->
          <button class="cc-btn cc-btn-bare cc-btn-icon"
                  v-tooltip.bottom="'Draw a region on the napari image to highlight those cells here'"
                  @click="g.startCellSelection"><i class="pi pi-pencil" /></button>
          <!-- z scope: whole stack (default) vs only the current z-slice (± window). Changing this
               re-evaluates any active selection live (and applies to the next one). -->
          <button class="cc-btn cc-btn-bare"
                  :class="{ 'cc-btn-on cc-btn-on-tint': g.napariZMode === 'slice' }"
                  v-tooltip.bottom="g.napariZMode === 'slice'
                    ? `Selecting cells from the current z-slice ±${g.napariZWindow} — click for the whole stack`
                    : 'Selecting cells across the whole z-stack — click to restrict to the current z-slice'"
                  @click="g.napariZMode = g.napariZMode === 'slice' ? 'stack' : 'slice'">
            <i class="pi pi-clone" /> Z
          </button>
        </div>
        <label v-if="!isTrack && g.napariZMode === 'slice'" class="zwin"
               v-tooltip.bottom="'Include cells within ± this many z-slices (0 = current only)'">
          ±<input type="number" min="0" max="50" step="1" v-model.number="g.napariZWindow" />
        </label>
        <CanvasArrangeButtons :count="panels.length" @tile="arrangeGrid" @cascade="arrangeCascade"
                              @close-all="removeAll" />
        <div class="cc-btn-group">
          <button class="cc-btn cc-btn-bare cc-btn-icon" data-guide="gate.popManager"
                  :class="{ 'cc-btn-on cc-btn-on-tint': showManager }"
                  @click="showManager = !showManager"
                  v-tooltip.bottom="showManager ? 'Hide the populations rail' : 'Show the populations rail'">
            <i class="pi pi-sitemap" />
          </button>
        </div>
        <div class="cc-btn-group" v-tooltip.bottom="'Step to the previous / next image in the list'">
          <button class="cc-btn cc-btn-bare cc-btn-icon" :disabled="!hasPrev"
                  @click="navTo(-1)" aria-label="Previous image">&laquo;</button>
          <button class="cc-btn cc-btn-bare cc-btn-icon" :disabled="!hasNext"
                  @click="navTo(1)" aria-label="Next image">&raquo;</button>
        </div>
        <button class="cc-btn" v-tooltip.bottom="'Copy this gating to other images in the set'"
                @click="showCopy = true"><i class="pi pi-copy" /> Copy</button>
        <CanvasZoomControl :zoom="zoom" @update:zoom="setZoom" @fit-width="fitWidth" @fit-height="fitHeight" @reset="resetZoom" />
        <!-- Shown only on an EMPTY canvas. `.gp-bar` is a nowrap flex row, so a text node at the end
             of it is the one item with nothing to push against: it collapsed to a ~40px column and
             wrapped over six lines, which is what set the bar's height. It is also orientation a user
             needs exactly once (docs/UI.md → UI copy), so the fix and the right behaviour agree. -->
        <span v-if="!panels.length" class="gp-hint cc-muted cc-fs-xs">drag plots by their title · resize from the corner</span>
      </div>
      <div class="gp-canvas">
        <!-- scroll viewport (measured): the workspace inside it may be TALLER than the
             visible box, so the plots scroll. The rail is a sibling BELOW, outside this
             box, so it stays put instead of scrolling away with them. -->
        <div ref="canvasRef" class="gp-scroll">
        <!-- scaled workspace: the plots zoom together; the population manager stays full-size (below) -->
        <div ref="zoomRef" class="gp-zoom" :style="workspaceStyle">
        <template v-for="(p, i) in panels" :key="`${ckey}:${p.id}`">
          <!-- registry views (track paths, the correction worklist) → generic InteractivePanel, the
               same host the cluster and optical-flow canvases use -->
          <InteractivePanel v-if="isInteractiveView(p.state.kind)" :index="i" :arrange="p.arrange"
                            :active="p.id === activeId" :view="p.state.kind"
                            :context="ctxForView(p.state.kind, p.id)" :state="p.state" :persist-key="`${ckey}:${p.id}`"
                            @activate="activeId = p.id" @remove="remove(p.id)" />
          <GatePairsPanel v-else-if="p.state.kind === 'pairs'" :index="i" :arrange="p.arrange"
                          :active="p.id === activeId" :parent="p.state.parent" :highlight="panelHL(p.state)"
                          :gate-line-width="panelLineWidth(p.state)" :gate-labels="panelLabels(p.state)" :axis-from-zero="panelFromZero(p.state)"
                          :dot-size="panelDotSize(p.state)"
                          :ui="p.state" :persist-key="`${ckey}:${p.id}`"
                          @activate="activeId = p.id" @update:parent="setParent(p.id, $event)" @remove="remove(p.id)" />
          <GatePlotPanel v-else :index="i" :arrange="p.arrange"
                         :active="p.id === activeId" :parent="p.state.parent" :highlight="panelHL(p.state)"
                         :gate-line-width="panelLineWidth(p.state)" :gate-labels="panelLabels(p.state)" :axis-from-zero="panelFromZero(p.state)"
                         :dot-size="panelDotSize(p.state)"
                         :ui="p.state" :persist-key="`${ckey}:${p.id}`"
                         @activate="activeId = p.id" @update:parent="setParent(p.id, $event)" @remove="remove(p.id)" />
        </template>
        </div>
        </div>
        <!-- THE RAIL, following the ACTIVE panel (railFor, never a key list here). A track view slices by
             population, so it gets the SERIES PICKER — populations grouped by segmentation, each row
             carrying its family; a gating plot is a tree being edited, so it gets the tree. The gating
             tree could not serve both: it has no popType to give, so every series it built was filtered
             out again (see ctxForView). No `vis`: the track panels read none of the styling block, and
             five controls wired to nothing is what the rail plan calls dead chrome. -->
        <SeriesPicker v-if="showManager && activeIsPopsView" title="Tracks" icon="pi-share-alt"
                      :groups="segPops" :selected="activePopSel" :scope="scope"
                      :single="activeSinglePop"
                      @toggle="togglePop" @update:scope="scope = $event" />
        <PopulationManager v-else-if="showManager" :selected="selected" :highlighted="activeHL" :scope="scope" :pop-type="props.popType"
                           :line-width="activeLineWidth" :gate-labels="activeLabels" :axis-from-zero="activeFromZero"
                           :dot-size="activeDotSize"
                           @update:selected="onPickPop" @update:scope="scope = $event" @toggle-highlight="toggleHighlight"
                           @update:line-width="setLineWidth" @update:dot-size="setDotSize" @update:gate-labels="setLabels"
                           @update:axis-from-zero="setFromZero" @show-defining-plot="showDefiningPlot" />
      </div>
    </template>
    <GatingCopyDialog v-if="showCopy" :set-uid="setUid" :source-uid="props.imageUid!"
                      :value-name="g.valueName" :pop-type="props.popType" @close="showCopy = false" />
  </div>
</template>

<style scoped>
/* fill all available height so the plot workspace isn't capped */
.gating-plots { display: flex; flex-direction: column; height: 100%; min-height: 80vh; }
.gp-empty { padding: 20px; }   /* + .cc-muted */
.gp-bar { display: flex; align-items: center; gap: 14px; padding: 8px 4px; font-size: var(--cc-fs-sm); flex-shrink: 0; }
.gp-bar label { display: flex; align-items: center; gap: 6px; color: var(--cc-text-dim); }
.gp-bar select { min-width: 9rem; }   /* visual styling from the global form base */
.gp-hint { opacity: 0.7; white-space: nowrap; }
/* the picker sits with the + buttons, so it must not take the 9rem the segmentation select does */
.gp-bar select.gp-add { min-width: auto; }
/* z-slice window stepper (shown only in slice mode) */
.zwin { display: flex; align-items: center; gap: 2px; color: var(--cc-text-dim); }
.zwin input { width: 3.2rem; padding: 3px 4px; }
/* free-floating plot workspace: panels + manager are absolutely positioned within */
.gp-canvas { position: relative; flex: 1; min-height: 70vh; }
/* the scaled workspace fills the canvas (offsetParent for the floating plot panels); transform inline */
/* scaled workspace (offsetParent for panels); size + transform set inline by useCanvasWorkspace.
   min 100% so it always at least fills the viewport (like the old inset:0) even before the JS size
   lands — else a 0 measurement collapses it and drag pins panels to the top-left. */
/* the measured viewport: the workspace it holds can be taller than this box (useCanvasWorkspace
   grows it to fit the plots), so overflow scrolls here rather than escaping the canvas. */
.gp-scroll { position: absolute; inset: 0; overflow: auto; }
.gp-zoom { position: absolute; top: 0; left: 0; min-width: 100%; min-height: 100%; }
</style>
