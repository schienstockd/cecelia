<!--
  Summary-plot canvas (the analysis-plot surface). Same shell as the gating workspace
  (useCanvasPanels + CanvasPanel), but the panels are Vega-Lite SummaryPanels chosen from the
  plot-spec registry (GET /api/plots/definitions, filtered by `module`) and the population picker
  is the read-only, cross-segmentation SeriesPicker (not the gating PopulationManager).

  The user selects one or more images above the canvas; this canvas shows ALL populations available
  across those images, GROUPED BY SEGMENTATION (GET /api/plots/populations). Eye-selecting a
  population makes it a plot series — so populations from different images AND different
  segmentations can be overlaid on one plot. The "compare" control pools across the selected images
  (per image / pooled); chart type is a per-panel choice, independent of the data source.

  This is the per-module canvas; the universal canvas (Phase 4) is the same component with the
  module filter off.
-->
<script setup lang="ts">
import { toggleSelected, narrowToSingle } from '../../utils/selection'
import { computed, ref, watch } from 'vue'
import CanvasArrangeButtons from './CanvasArrangeButtons.vue'
import { useProjectStore } from '../../stores/project'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useFloatingCanvas } from '../../composables/useFloatingCanvas'
import FloatingCanvasHost from './FloatingCanvasHost.vue'
import { useSummaryData } from '../../composables/useSummaryData'
import SeriesPicker from './SeriesPicker.vue'
import SummaryPanel from './SummaryPanel.vue'
import InteractivePanel from './InteractivePanel.vue'
import { INTERACTIVE_VIEWS, isPluginView, railFor, popTypesFor, popTypeSpecFor, singlePopFor } from './interactiveViews'
import CanvasZoomControl from './CanvasZoomControl.vue'
import { tkey, parseTkey, seriesMemo } from '../../plots/series'
import { defaultVis, DEFAULT_VIS, type VisProps } from '../../plots/plot'
import type { SeriesTarget, ChartType } from '../../plots/types'
import { migrateSpecId, isPrecomputedSpec } from '../../plots/popTypes'
import { emptyReadout, type PlotReadout } from '../../plots/plotReadout'
import CcToggle from '../CcToggle.vue'
import PlotNotice from './PlotNotice.vue'
import CanvasSelectionOverlay from './CanvasSelectionOverlay.vue'
import FrameAnnotator from '../FrameAnnotator.vue'
import CaptureViewSurface from '../CaptureViewSurface.vue'
import { useCanvasPanelsStore } from '../../stores/canvasPanels'
import { useCaptureReshowStore } from '../../stores/captureReshow'
import type { OverlayMark, CaptureAddress } from '../../utils/captureAddress'
import type { CaptureEnvelope } from '../../utils/kiwiCaptures'
import { restorePanelsFromCapture, type CapturedPanel } from '../../utils/restorePanels'
import { reresolvePops } from '../../plots/reresolvePops'
import { useCanvasShare, type SharePanelExported } from '../../composables/useCanvasShare'

// `canvasKey` OPTIONALLY overrides the persistence namespace (default `summary:{module|universal}`).
// The tabbed Analysis board passes `analysis:{projectUid}:tab:{id}` per tab so each board persists
// independently; parents that switch the key MUST also `:key` this component by it so setup re-runs.
// `views` = interactive plots this page ALSO offers, named by their stable registry id. A plugin
// declares them in `plugin.json` → `contributions.views` (PLUGINS_PLAN Decision 11) and the custom
// module page passes them through; every other host passes none and is unchanged. What this makes
// public is view IDS, not components — renaming `trackPaths` breaks installed plugins, rewriting
// `TrackPathsView.vue` does not.
export interface DeclaredView { view: string; label?: string; plugin?: string }
const props = defineProps<{ imageUids: string[]; module?: string | null; canvasKey?: string
                            views?: DeclaredView[] }>()
const project = useProjectStore()
const meta = useProjectMetaStore()

const projectUid = computed(() => meta.current?.uid ?? '')
const imageUid = computed(() => props.imageUids[0] ?? null)   // drives "this image" plots
const setUid = computed(() => project.activeSetUid)

// Persistence key: an explicit override (the Analysis board) wins; otherwise per-module + PER SET.
//
// It used to be per IMAGE, which coupled two unrelated things to whichever image happened to be first
// in the selection: not just what was plotted, but WHICH SAVED CANVAS you were looking at. Ticking a
// different set of images silently swapped your whole plot layout, and ticking five images showed the
// layout of the first. Summary plots are set-aware by design — per-image vs pooled vs by-attribute is
// exactly what the `compare` control decides — so the layout has no business being image-scoped on top
// of that. `ClusterPlots` was already `clust:{popType}:{setUid}`, and `objectOf` persists a set-keyed
// canvas to the SET's own moduleCanvases.json, so this is an existing path, not a new one.
//
// Canvases saved under the old per-image keys are deliberately NOT ported (agreed 2026-08-15): there is
// no honest merge from N per-image layouts into one, so they are simply no longer read.
const ckey = computed(() => props.canvasKey ?? `summary:${props.module ?? 'universal'}:${setUid.value ?? 'none'}`)

// per-plot state (edited inside SummaryPanel; persists in the panel objects). Canvas-level view state
// + all shared data (specs/pops/attrs, compare/scope/global sel+vis) come from useSummaryData below.
interface PanelState {
  // Index signature so a panel's state is assignable to the generic InteractivePanel's
  // `Record<string, unknown>` — the same shape GatingPlots' panel state carries, for the same reason.
  [key: string]: unknown
  specId: string; sel: string[]; vis: VisProps
  // set → this panel is an INTERACTIVE view (interactiveViews.ts) rather than a summary spec. Every
  // panel keeps `sel`/`vis` regardless, so the selection/prune/duplicate helpers below need no
  // interactive-only branches; the picker simply says the eye toggles do nothing on such a panel.
  kind?: string
  popType?: string        // which population family this plot shows (specs that offer a choice)
  chartType?: ChartType; measure?: string; bins?: number; normalize?: boolean; errorMetric?: 'sd' | 'sem' | 'ci95'
  groupBy?: string; smooth?: number; interval?: boolean
  matrixMode?: 'profile' | 'crosstab'; zscore?: boolean; heatmapValues?: boolean; matrixNormalize?: 'none' | 'row' | 'col' | 'total'
}
// Workspace shell + zoomable panels + zoom provided to plot components inside. See
// useFloatingCanvas — the wiring that used to be four hand-rolled statements here (and identically
// in GatingPlots + ClusterPlots). The workspace GROWS when zoomed out so the whole page stays
// usable; plot renders/exports stay at 100% (Vega, PNG, PDF, and the frame-annotator composite all
// sample from the logical canvas via CANVAS_ZOOM_KEY-injected 1/zoom).
const {
  bindCanvas, bindZoom, workspaceStyle,
  panels, activeId, activePanel, shared,
  add, remove, removeAll, arrangeGrid, arrangeCascade,
  zoom, fitWidth, fitHeight, setZoom, resetZoom,
} = useFloatingCanvas<PanelState>(
  ckey,
  () => ({ specId: specs.value[0]?.id ?? '', sel: [], vis: defaultVis() }),
)
// show/hide the floating population picker — persisted per canvas in the `shared` bag (default shown)
const showManager = computed<boolean>({ get: () => (shared.value.showManager as boolean) ?? true, set: v => (shared.value.showManager = v) })
// Tile Columns knob (0 = Auto). Persisted per canvas so the last pick survives navigation. See
// CanvasArrangeButtons — the escape hatch for a narrow/unmeasured workspace falling back to 1 col.
const tileCols = computed<number>({ get: () => (shared.value.tileCols as number) ?? 0, set: v => (shared.value.tileCols = v) })
// shared summary-plot data + canvas-level view-state (identical whether plots float or sit in a grid)
const {
  specs, specById, segPops, seriesColor, reloadToken, validSelKeys, popType,
  compareMode, compareAttr, compareAttr2, scope, gSel, gVis, poolGroups,
  manualApply, stagedSel, hasStaged, stagedChangeCount, applyStaged, discardStaged,
  canCompare, panelSetUid, panelImageUids, panelScope, panelGroupAttr, attrOptions2, setAttrs,
} = useSummaryData({ projectUid, imageUids: computed(() => props.imageUids), setUid, module: props.module, shared,
  // The population picker follows the ACTIVE plot's spec, exactly as on the Analysis board. This used
  // to be board-only, on the assumption that a module page's specs all share one popType — which stopped
  // being true once the per-poptype population summaries moved onto the Explore pages (Phenotype hosts
  // flow + clust, Behaviour live + trackclust, Spatial region). Without it the picker falls back to
  // `specs[0]`, and since /api/plots/definitions just walks readdir, "first" is filename order — so the
  // page would silently offer the wrong population family for the selected plot.
  activeSpecId: computed(() => activePanel.value?.state.specId ?? null),
  // the active plot's chosen population family — the manager lists THAT family (one control, on the plot)
  activePopType: computed(() => activePanel.value?.state.popType ?? null),
  // An INTERACTIVE panel that slices by population declares its families on its registry entry, so the
  // rail lists THAT plot's family rather than whichever one `specs[0]` happens to carry. Same
  // resolution the board uses — a second path here could disagree about what the plot is showing.
  activeFamily: computed(() => {
    const k = activePanel.value?.state.kind
    return k ? popTypeSpecFor(String(k)) : null
  }) })

// Migrate canvases persisted before the four per-popType population summaries collapsed into one spec
// with a family picker. Without this a saved panel's specId no longer resolves and the panel silently
// renders nothing (`v-if="specById[...]"` below). Mirrors ClusterPlots' KIND_ALIASES.
for (const p of panels.value) if (!p.state.kind) migrateSpecId(p.state)

// ── interactive views (PLUGINS_PLAN Decision 11) ──────────────────────────────────────────────────
// A declared id is resolved against the registry HERE, because the registry is a frontend module and
// the manifest that names it is read in Julia. Two ways it can fail to resolve, deliberately reported
// as one line: the id does not exist in this Cecelia, or it exists but is not offered to plugins
// (`pluginPage` — e.g. `trackCorrection`, which mutates, or a view needing a rail this canvas does not
// render). Either way it must be SAID: a view silently missing from the picker is exactly the
// blank-panel failure this codebase keeps producing. (A panel already holding an unusable kind is
// covered too — InteractivePanel renders "Unknown interactive plot".)
const VIEW_OPT_PREFIX = 'view:'   // option values are namespaced so a view id cannot collide with a spec id
const declaredViews = computed(() => (props.views ?? []).filter(v => isPluginView(v.view)))
const unusableViews = computed(() => (props.views ?? []).filter(v => !isPluginView(v.view)))
const viewLabel = (v: DeclaredView) => v.label || INTERACTIVE_VIEWS[v.view]?.label || v.view
const unusableViewText = computed(() =>
  `Plot not available here: ` +
  unusableViews.value.map(v => v.plugin ? `${v.view} (${v.plugin})` : v.view).join(', '))
// The bag an interactive view receives (docs/UI.md → generic plot-integration interface). Views pick
// their own image out of `imageUids` and keep the rest in their panel state.
//
// A view on the POPULATION rail is part of THIS canvas's comparison and gets the same four things a
// SummaryPanel gets — selection, compare mode + its attributes, pool toggle — exactly as the board
// builds it (`LayoutCanvas.ctxFor`). A self-contained view declares `rail: 'none'` and never sees them.
// Both branches exist because #593 moved the two track plots onto the pops rail: before it, every
// plugin-nameable view was self-contained and this was one object.
const viewContext = (id: number, st: PanelState) => {
  const base = { projectUid: projectUid.value, imageUids: props.imageUids, setUid: setUid.value,
                 vis: panelVis(st) }
  return railFor(String(st.kind)) === 'pops'
    ? { ...base, series: panelSeries(id, st), popTypes: popTypesFor(String(st.kind)),
        compareMode: compareMode.value, groupAttr: panelGroupAttr.value, poolGroups: poolGroups.value }
    : base
}

// global/local scope governs BOTH the eye-selection AND the visual properties (like the gating
// PopulationManager): global = one value shared by every plot, local = the active plot's own.
const panelSel = (s: PanelState) => scope.value === 'global' ? gSel.value : s.sel
// The picker shows STAGED selection in manual-apply mode (so ticking an eye updates the row
// immediately even though the plots don't refetch until Apply). Local scope isn't staged.
const pickerSel = computed(() => scope.value === 'global'
  ? (manualApply.value ? stagedSel.value : gSel.value)
  : (activePanel.value?.state.sel ?? []))
// the SAME registry policy the other two hosts read (`singlePopFor`). No plugin-nameable view declares
// it today, but a policy honoured by two hosts out of three is how the three drift apart.
const activeSinglePop = computed(() => {
  const k = activePanel.value?.state.kind
  return !!k && singlePopFor(String(k))
})
const panelVis = (s: PanelState) => scope.value === 'global' ? gVis.value : s.vis
const activeVis = computed(() => scope.value === 'global' ? gVis.value : (activePanel.value?.state.vis ?? DEFAULT_VIS))
// the stats test each panel's last result actually ran (`auto` resolves it server-side from the group
// count) — the picker shows the ACTIVE plot's, so the user can see what `auto` chose. Not persisted:
// it's a readout of the current result, not a setting.
const readouts = ref<Record<number, PlotReadout>>({})
const activeReadout = computed<PlotReadout>(() => readouts.value[activeId.value] ?? emptyReadout())
// the active plot is PRECOMPUTED — its populations come from an analysis run, so the picker says so
// instead of offering eye toggles that do nothing (see isPrecomputedSpec)
const activeIsPrecomputed = computed(() => {
  // An interactive view on the POPS rail consumes the eye-selection (the two track plots do). One on
  // any other rail brings its own data and controls, so the picker says the selection is unused
  // rather than offering toggles that do nothing.
  const kind = activePanel.value?.state.kind
  if (kind) return railFor(String(kind)) !== 'pops' 
  const id = activePanel.value?.state.specId
  const spec = id ? specById.value[id] : null
  return !!spec && isPrecomputedSpec(spec)
})
function removePanel(id: number) { remove(id); delete readouts.value[id] }

// ── Canvas Share (Kiwi's canvas Share button) ──────────────────────────────
// The two-phase Share flow (selection → annotate → POST) is extracted to `useCanvasShare` so
// every canvas host uses the same machinery. Per-canvas concerns stay here:
// - `buildEnvelope` — how each selected panel is described on the wire (SummaryCanvas puts the
//   FULL panel state on `plotRef.ui` + a `dataSlice` for Claude's read; other canvases carry
//   their own state).
// - `buildPlotSpec` — the address's plotSpec (`multi-panel` with the module tag).
// - `onSaveSuccess` — seed the reshown ref so CaptureViewSurface stays up over the canvas.
// Reshow itself stays inline (below) — zoom-to-source restore is SummaryCanvas-specific
// (`restorePanelsFromCapture` with the reresolvePops policy).
const geomStore = useCanvasPanelsStore()
// Alias so the buildEnvelope serialiser can read the current panels without shadowing the
// `panels` local it builds (mirrors the pre-extraction `panelsSnapshot` trick).
const panelsSnapshot = panels
const share = useCanvasShare({
  projectUid: () => projectUid.value,
  canvasKey: () => ckey.value,
  panels: () => panels.value,
  label: () => `${props.module ?? 'universal'} · plot canvas`,
  buildPlotSpec: ({ panelCount }) => ({ specId: 'multi-panel',
    params: { module: props.module ?? 'universal', panelCount } }),
  buildEnvelope: ({ selected, workspaceOrigin }) => {
    // panels[] — per-panel structure Claude reads to say "the top-left panel is speed for pops B/T."
    // plotRef.ui carries the FULL panel state (measure, groupBy, chartType, sel, vis, …) so a later
    // zoom-to-source restores the panel exactly as it was. `dataSlice.series` is a parallel
    // Claude-facing view of sel (tkey → SeriesTarget); keeping sel on plotRef.ui too is
    // duplication of a small array, cheap next to the composite PNG.
    // uid map from CURRENT segPops, keyed by tkey. Empty uid populations (`/labels`, `/_tracked`,
    // synthetic root) contribute nothing here — those pops have no gating-map identity and restore
    // via path fallback in `reresolvePops`.
    const uidByTkey = new Map<string, string>()
    for (const g of segPops.value) {
      for (const pop of g.populations) {
        if (pop.uid) uidByTkey.set(tkey(pop.popType, g.valueName, pop.path), pop.uid)
      }
    }
    const { x: x0, y: y0 } = workspaceOrigin
    return selected.map((e: SharePanelExported) => {
      const panel = panelsSnapshot.value.find(pp => pp.id === e.id)
      const st = panel?.state as (PanelState | undefined)
      const specId = st?.kind ? String(st.kind) : (st?.specId ?? '')
      const plotRef: Record<string, unknown> = { specId }
      if (st) {
        // `selUids` is a parallel array to `sel`: uid of each picked pop, empty when this pop has
        // no gating-map identity. Consumed on zoom-to-source by `reresolvePops` to survive a pop
        // rename / reparent since capture time.
        const selUids = (st.sel ?? []).map(t => uidByTkey.get(t) ?? '')
        plotRef.ui = { ...st, selUids }
      }
      return {
        panelId: String(e.id),
        position: { x: e.geom.x - x0, y: e.geom.y - y0, w: e.geom.w, h: e.geom.h },
        plotRef,
        dataSlice: {
          imageUids: panelImageUids.value,
          setUid: panelSetUid.value ?? null,
          scope: panelScope.value,
          series: st ? panelSeries(e.id, st) : [],
        },
      }
    })
  },
  onSaveSuccess: (env) => { reshown.value = env },
})
// Composable-owned state exposed to the template (keeps template renames minimal).
const { shareSel, sharePanelHits, pendingShare, shareBusy, shareToast,
        onShareCancel, onShareConfirm, onAnnotateCancel, onAnnotateSave, dismissShareToast } = share

// `beginShare` on the composable clears its own pendingShare; also clear reshown here so a
// live-shared / re-shown frame doesn't sit on top of the selection overlay eating clicks.
// The composable's Kiwi registration wraps this: we watch shareSel.active for the leading edge
// and clear reshown at that instant. Cheap and keeps beginShare's contract single-purpose.
watch(() => shareSel.active.value, on => { if (on) reshown.value = null })

// ── Reshow (Kiwi / Blackboard clicked refocus on a plot capture) ──────────────
// The reshow store carries the envelope in. We mount CaptureViewSurface — the same surface the
// viewer's re-annotate uses — over the plot canvas so the user sees the frozen composite + their
// original marks + any Claude freeform paint targeted at this captureId, all in one place. On
// zoom-to-source we rebuild `panels[]` into the canvas persistence store; on close we drop back
// to the live layout.
const reshowStore = useCaptureReshowStore()
const reshown = ref<CaptureEnvelope | null>(null)
// Consume the bag on mount AND whenever a fresh bag lands. Watching `pending` (not `props.module`)
// is what makes a Kiwi refocus work when the user is ALREADY on this module page — same-route
// `router.push` doesn't remount, so a module-only watch would never fire. `consumeFor` still
// checks the module match, so a bag targeted at another page sits until that page mounts.
watch(() => reshowStore.pending, () => {
  const env = reshowStore.consumeFor(String(props.module ?? ''))
  if (env) reshown.value = env
}, { immediate: true })
// Address bag CaptureViewSurface expects.
const reshownAddress = computed<CaptureAddress>(() => {
  const a = reshown.value?.address as CaptureAddress | null | undefined
  return a ?? { projectUid: projectUid.value }
})
const reshownAddressLine = computed(() => {
  const panelsN = (reshown.value?.panels?.length) ?? 0
  const mod = props.module ?? 'universal'
  return panelsN > 0 ? `${mod} · ${panelsN} panels` : `${mod} · plot canvas`
})
function onReshowClose() { reshown.value = null }
function onReshowReannotate(payload: { captureId: string; frameDataUrl: string; overlay: OverlayMark[]; notes: string }) {
  // The refined capture replaces the reshow — the user is now looking at the newer version.
  if (!reshown.value) return
  reshown.value = { ...reshown.value,
    captureId: payload.captureId, frame: payload.frameDataUrl,
    overlay: payload.overlay, notes: payload.notes }
}
function onReshowZoomToSource() {
  // Restore the panels underneath. Uses the envelope's `panels[]` — pure translation, no fresh
  // fetches. Drops the reshow surface after so the user is looking at the live restored layout.
  const env = reshown.value
  if (!env) return
  const captured = Array.isArray(env.panels) ? env.panels as unknown[] : []
  const cps: CapturedPanel[] = []
  for (const raw of captured) {
    if (!raw || typeof raw !== 'object') continue
    const p = raw as Record<string, unknown>
    const pos = p.position as { x?: number; y?: number; w?: number; h?: number } | undefined
    if (!pos || typeof pos.x !== 'number' || typeof pos.y !== 'number'
             || typeof pos.w !== 'number' || typeof pos.h !== 'number') continue
    cps.push({
      panelId: String(p.panelId ?? ''),
      position: { x: pos.x, y: pos.y, w: pos.w, h: pos.h },
      plotRef: p.plotRef as CapturedPanel['plotRef'],
      dataSlice: p.dataSlice as CapturedPanel['dataSlice'],
    })
  }
  restorePanelsFromCapture<PanelState>(geomStore, ckey.value, cps, (cp) => {
    // Rebuild a `PanelState` from the captured `plotRef.ui` — which now carries the FULL
    // configuration (sel + vis included). A zoom-to-source lands the panels with populations
    // already picked and vis (log axes, bin widths, …) preserved. Fallbacks cover legacy
    // captures written before `sel`/`vis` were preserved.
    //
    // `reresolvePops` rebinds captured `sel` tkeys against the CURRENT segPops. Two-pass: pop uids
    // first (survives a rename / reparent since capture time), path fallback second (needed when
    // uid isn't in the current image or the capture predates uid capture). See
    // `plots/reresolvePops.ts` for the full matching policy.
    const specId = String(cp.plotRef?.specId ?? '')
    const ui = (cp.plotRef?.ui as Partial<PanelState> & { selUids?: unknown }) ?? {}
    const capSel = Array.isArray(ui.sel) ? ui.sel : []
    const capUids = Array.isArray(ui.selUids) ? (ui.selUids as unknown[]).map(u => String(u ?? '')) : []
    return {
      ...ui,
      specId,
      sel: reresolvePops(capSel, capUids, segPops.value),
      vis: ui.vis ? { ...defaultVis(), ...ui.vis } : defaultVis(),
    } as PanelState
  }, env.workspaceOrigin)
  reshown.value = null
}

// Close all must drop the readouts too — they are keyed by panel id, and a stale entry would be
// re-adopted by the next panel that reuses a freed id (`activeReadout` reads this map by id).
function removeAllPanels() { removeAll(); readouts.value = {} }
function toggleTarget(valueName: string, pop: string, pt: string) {
  const k = tkey(pt, valueName, pop)
  const next = (cur: string[]) => toggleSelected(cur, k, { single: activeSinglePop.value })
  if (scope.value === 'global') {
    // manual-apply mode: stage the toggle; plots stay on the committed gSel until Apply is pressed.
    // See useSummaryData `manualApply` — the opt-in trickle escape hatch for users with many pops.
    if (manualApply.value) stagedSel.value = next(stagedSel.value)
    else gSel.value = next(gSel.value)
  } else if (activePanel.value) activePanel.value.state.sel = next(activePanel.value.state.sel)
}
// the policy can change under an existing selection when the ACTIVE panel changes — narrow rather than
// let a single-population plot draw one of several and say nothing
watch(activeSinglePop, single => {
  if (!single) return
  if (scope.value === 'global') gSel.value = narrowToSingle(gSel.value)
  else if (activePanel.value) activePanel.value.state.sel = narrowToSingle(activePanel.value.state.sel)
})
function setVis(patch: Partial<VisProps>) {
  if (scope.value === 'global') gVis.value = { ...gVis.value, ...patch }
  else if (activePanel.value) activePanel.value.state.vis = { ...activePanel.value.state.vis, ...patch }
}
// a panel's series = its selected target keys parsed back into {valueName, pop}
// keyed by PANEL ID (panels are free-floating, not slot-indexed) — see seriesMemo
const memoSeries = seriesMemo<number>()
const panelSeries = (id: number, s: PanelState): SeriesTarget[] => memoSeries(id, panelSel(s))

function addPanel(value: string) {
  if (!value) return
  add()
  const p = panels.value.at(-1)
  if (!p) return
  if (value.startsWith(VIEW_OPT_PREFIX)) {
    const kind = value.slice(VIEW_OPT_PREFIX.length)
    p.state = { ...p.state, ...(INTERACTIVE_VIEWS[kind]?.initialState?.() ?? {}), kind }
  } else p.state.specId = value
}

// Duplicate a panel: new panel with a deep copy of the source's state (spec, series selection,
// chart type/measure/groupBy/vis) — so the user can change one thing (e.g. measure speed → angle).
function duplicatePanel(src: { state: PanelState }) {
  add()
  const p = panels.value.at(-1)
  if (p) p.state = { ...src.state, sel: [...src.state.sel], vis: { ...src.state.vis } }
}

// "Show series": explode this panel by measurement — one duplicate per selected measure, so all the
// track measurements (speed, displacement, straightness, …) are visible side by side instead of
// flipping the single measure dropdown. Same deep-clone as duplicatePanel, overriding `measure`.
function explodePanel(src: { state: PanelState }, measures: string[]) {
  for (const m of measures) {
    add()
    const p = panels.value.at(-1)
    if (p) p.state = { ...src.state, sel: [...src.state.sel], vis: { ...src.state.vis }, measure: m }
  }
  arrangeGrid(tileCols.value)   // tile them so the whole set is visible at once (the point of "show series")
}

// useSummaryData prunes the GLOBAL selection when pops vanish; prune each panel's LOCAL selection here.
// popType-aware (keep other-popType keys) for parity with the board's mixed-popType prune.
watch(segPops, () => {
  const valid = validSelKeys.value, pt = popType.value
  const keep = (k: string) => parseTkey(k).popType !== pt || valid.has(k)
  for (const p of panels.value) p.state.sel = p.state.sel.filter(keep)
})
</script>

<template>
  <div class="summary-canvas">
    <div v-if="!imageUid" class="sc-empty cc-muted">Select one or more images above to plot.</div>
    <template v-else>
      <div class="sc-bar cc-row cc-row-loose">
        <select class="sc-add" v-tooltip.bottom="'Add a plot'"
                @change="addPanel(($event.target as HTMLSelectElement).value); ($event.target as HTMLSelectElement).value = ''">
          <option value="">+ Plot…</option>
          <option v-for="s in specs" :key="s.id" :value="s.id">{{ s.label }}</option>
          <!-- Interactive views a plugin asked for on this page. In their own group so it is obvious
               they are a different kind of plot: they fetch their own data and carry their own
               controls, so the population picker does not drive them. -->
          <optgroup v-if="declaredViews.length" label="Interactive">
            <option v-for="v in declaredViews" :key="v.view" :value="`${VIEW_OPT_PREFIX}${v.view}`">
              {{ viewLabel(v) }}
            </option>
          </optgroup>
        </select>
        <!-- compare cluster: mode + (by attribute) its attribute selects, kept tight in one group -->
        <!-- the row's tip belongs on the mode select (the control it describes, and the only one here
             without its own); on the container it fired over the attribute selects too -->
        <div v-if="canCompare" class="sc-compare">
          <span class="sc-lbl">compare</span>
          <select v-model="compareMode" class="sc-cmp"
                  v-tooltip.bottom="'How to compare across the selected images'">
            <option value="image">this image</option>
            <option value="per_image">per image</option>
            <option value="summarised">pooled</option>
            <option value="by_attr" :disabled="!setAttrs.length">by attribute</option>
          </select>
          <template v-if="compareMode === 'by_attr'">
            <select v-model="compareAttr" class="sc-attr" v-tooltip.bottom="'Attribute to group by'">
              <option v-for="a in setAttrs" :key="a.name" :value="a.name">{{ a.name }}</option>
            </select>
            <template v-if="attrOptions2.length">
              <span class="sc-x">×</span>
              <select v-model="compareAttr2" class="sc-attr" v-tooltip.bottom="'Optional second attribute to combine (e.g. Treatment × Mouse)'">
                <option value="">none</option>
                <option v-for="a in attrOptions2" :key="a.name" :value="a.name">{{ a.name }}</option>
              </select>
            </template>
          </template>
        </div>
        <CcToggle class="sc-pool" v-model="poolGroups" label="pool to groups"
          v-tooltip.bottom="'Pool populations and images — one series per Split-by group'" />
        <CanvasArrangeButtons :count="panels.length" :cols="tileCols"
                              @update:cols="tileCols = $event"
                              @tile="arrangeGrid(tileCols)" @cascade="arrangeCascade"
                              @close-all="removeAllPanels" />
        <div class="cc-btn-group">
          <button class="cc-btn cc-btn-bare cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': showManager }"
                  @click="showManager = !showManager"
                  v-tooltip.bottom="showManager ? 'Hide the population picker' : 'Show the population picker'">
            <i class="pi pi-sitemap" />
          </button>
        </div>
        <CanvasZoomControl :zoom="zoom" @update:zoom="setZoom" @fit-width="fitWidth" @fit-height="fitHeight" @reset="resetZoom" />
        <!-- A declared view that resolves to nothing: say which id, and which plugin asked for it.
             Silently dropping it from the picker is the failure Decision 11 exists to avoid. -->
        <PlotNotice v-if="unusableViews.length" :text="unusableViewText"
                    tip="The plugin names a plot this Cecelia does not offer — update the plugin, or ask its author." />
        <span v-if="!specs.length && !declaredViews.length" class="sc-hint cc-muted cc-fs-xs">No plot types available for this module yet.</span>
        <span v-else-if="specs.length" class="sc-hint cc-muted cc-fs-xs">eye-select populations to plot · drag plots by their title</span>
      </div>
      <FloatingCanvasHost :bind-canvas="bindCanvas" :bind-zoom="bindZoom" :workspace-style="workspaceStyle">
        <template v-for="(p, i) in panels" :key="`${ckey}:${p.id}`">
          <InteractivePanel v-if="p.state.kind" :index="i" :arrange="p.arrange"
                            :active="p.id === activeId" :view="p.state.kind"
                            :context="viewContext(p.id, p.state)" :state="p.state" :duplicable="true"
                            :persist-key="`${ckey}:${p.id}`"
                            @activate="activeId = p.id" @remove="removePanel(p.id)"
                            @duplicate="duplicatePanel(p)" />
          <SummaryPanel v-else-if="specById[p.state.specId]" :index="i" :arrange="p.arrange"
                        :active="p.id === activeId" :spec="specById[p.state.specId]"
                        :project-uid="projectUid" :image-uid="imageUid"
                        :set-uid="panelSetUid" :image-uids="panelImageUids" :scope="panelScope"
                        :group-attr="panelGroupAttr"
                        :series="panelSeries(p.id, p.state)" :series-color="seriesColor" :vis="panelVis(p.state)"
                        :ui="p.state" :collapse-series="poolGroups"
                        :reload-token="reloadToken" :persist-key="`${ckey}:${p.id}`"
                        @activate="activeId = p.id" @remove="removePanel(p.id)"
                        @duplicate="duplicatePanel(p)" @explode="explodePanel(p, $event)"
                        @readout="readouts[p.id] = $event" />
        </template>
        <!-- Share mode: dim veil + drag/click selection + toolbar. Inside the zoom workspace so
             its SVG coords are in the same workspace-CSS-px frame the panels' geoms are in. -->
        <CanvasSelectionOverlay v-if="shareSel.active.value"
                                :panels="sharePanelHits"
                                :selection="shareSel"
                                :address-line="`${module ?? 'universal'} · plot canvas`"
                                @cancel="onShareCancel" @share="onShareConfirm" />
        <!-- Annotate mode (Phase 2): frozen composite + DrawSurface tools + palette + labels,
             using the SAME FrameAnnotator the viewer's re-annotate flow does. Save fires the
             POST; Cancel drops the composite and leaves the canvas as it was. Gated on `!reshown`
             so an old shared frame can't sit on top of the annotator (both at z:40; later DOM
             would win and eat clicks). `beginShare` also clears `reshown` up front. -->
        <FrameAnnotator v-if="pendingShare &amp;&amp; !reshown"
                        :frame-data-url="pendingShare.composite"
                        :address-line="`${module ?? 'universal'} · ${pendingShare.panels.length} panels`"
                        :busy="shareBusy"
                        v-model:attach-to-kiwi="share.captureDest.attachToKiwi.value"
                        v-model:send-to-paired="share.captureDest.sendToPaired.value"
                        @save="onAnnotateSave" @cancel="onAnnotateCancel" />
        <!-- Reshow mode: same CaptureViewSurface the viewer uses. Mounts over the plot canvas,
             shows the frozen composite + user marks + any Claude freeform paint targeted at this
             captureId. Pencil = re-annotate (chained capture, panels[] inherited via
             extraPostFields). Zoom-to-source = restore the panel layout underneath. Gated on
             `!pendingShare` for symmetry with FrameAnnotator's `!reshown` guard. -->
        <CaptureViewSurface v-if="reshown &amp;&amp; !pendingShare"
                            :project-uid="projectUid"
                            :capture-id="reshown.captureId"
                            :frame-data-url="reshown.frame"
                            :overlay="reshown.overlay"
                            :notes="reshown.notes"
                            :address="reshownAddress"
                            :address-line="reshownAddressLine"
                            surface="plot"
                            :extra-post-fields="reshown.panels ? { panels: reshown.panels } : {}"
                            :show-zoom-to-source="!!(reshown.panels && reshown.panels.length)"
                            @close="onReshowClose"
                            @reannotate="onReshowReannotate"
                            @zoom-to-source="onReshowZoomToSource" />
        <!-- Post-Save toast: FrameAnnotator dismisses on Save, taking the "paste to Claude" hint
             with it, so surface the outcome inside the plot canvas for a few seconds — same role
             ViewerWindow's `.vw-share-chip` plays for viewer captures. In the host's `overlay`
             slot (inside `.floating-canvas`, outside the zoom transform) so the workspace CSS
             transform can't scale the chip along with the panels. -->
        <template #overlay>
          <div v-if="shareToast" class="sc-share-chip"
               :class="{ 'sc-share-chip-error': shareToast.kind === 'fail' }">
            <i :class="['pi', shareToast.kind === 'ok' ? 'pi-clipboard' : 'pi-exclamation-triangle',
                        'sc-share-chip-icon']" />
            <span>{{ shareToast.message }}</span>
            <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro sc-share-chip-dismiss"
                    @click="dismissShareToast" v-tooltip.top="'Dismiss'"
                    aria-label="Dismiss share notice"><i class="pi pi-times" /></button>
          </div>
          <!-- Floating population picker: `position: absolute` on the undocked CanvasSidePanel
               anchors it to the nearest positioned ancestor, so it stays in the overlay slot
               (inside `.floating-canvas`) alongside the share chip — not below the host. -->
          <SeriesPicker v-if="showManager" :groups="segPops" :selected="pickerSel" :scope="scope" :vis="activeVis"
                        :readout="activeReadout" :selection-unused="activeIsPrecomputed"
                        :manual-apply="manualApply" :staged-change-count="stagedChangeCount"
                        :has-staged="hasStaged"
                        @toggle="toggleTarget" @update:scope="scope = $event" @update:vis="setVis"
                        @update:manualApply="manualApply = $event"
                        @apply:staged="applyStaged" @discard:staged="discardStaged" />
        </template>
      </FloatingCanvasHost>
    </template>
  </div>
</template>

<style scoped>
.summary-canvas { display: flex; flex-direction: column; height: 100%; min-height: 80vh; }
.sc-empty { padding: 20px; }   /* + .cc-muted */
.sc-bar { padding: 8px 4px; font-size: var(--cc-fs-sm); flex-shrink: 0; }
.sc-bar label { display: flex; align-items: center; gap: 6px; color: var(--cc-text-dim); }
.sc-add { min-width: 8rem; }
/* compare cluster: one tight group so the mode + attribute selects read as a unit */
.sc-compare { display: inline-flex; align-items: center; gap: 6px; color: var(--cc-text-dim);
  padding: 2px 8px; border: 1px solid var(--cc-border); border-radius: var(--cc-radius-md); }
.sc-lbl { font-size: var(--cc-fs-xs); opacity: 0.8; }
.sc-cmp { min-width: 8rem; }
.sc-attr { min-width: 5.5rem; max-width: 8rem; }   /* short attribute names — no need for 9rem */
.sc-x { opacity: 0.6; }
.sc-hint { opacity: 0.7; margin-left: auto; }
/* Post-Save share toast — same visual family as ViewerWindow's `.vw-status-chip` so the outcome
   looks the same on both surfaces. Sits above CaptureViewSurface (z:40) at z:50 so a user reads
   the "prompt on your clipboard" line whether or not the frozen frame is up. `pointer-events:auto`
   on the whole chip because the dismiss button needs to be clickable. */
.sc-share-chip {
  position: absolute; left: 0.75rem; bottom: 0.75rem; z-index: 50;
  padding: 0.3rem 0.55rem; border-radius: var(--cc-radius-xs);
  background: rgba(0, 0, 0, 0.78); color: #fff;
  font-size: var(--cc-fs-xs); pointer-events: auto;
  display: inline-flex; align-items: center; gap: 0.35rem; max-width: calc(100% - 1.5rem);
}
.sc-share-chip-error { color: var(--cc-sev-fail); }
.sc-share-chip-icon { font-size: 1em; }
.sc-share-chip-dismiss { margin-left: 0.15rem; color: #fff; opacity: 0.8; }
.sc-share-chip-dismiss:hover { opacity: 1; }
</style>
