import type { Component } from 'vue'
import { DEFAULT_RAIL, type RailKind } from './canvasManager'
import type { PopTypeOption, PopTypeSpecLike } from '../../plots/popTypes'
import UmapView from '../plots/UmapView.vue'
import GatingStrategyView from '../plots/GatingStrategyView.vue'
import ImageStripView from '../plots/ImageStripView.vue'
import FlowMetricsView from '../plots/FlowMetricsView.vue'
import FlowTrainingView from '../plots/FlowTrainingView.vue'
import FlowProbabilityView from '../plots/FlowProbabilityView.vue'
import TrackPathsView from '../plots/TrackPathsView.vue'
import TrackDiagnosticsView from '../plots/TrackDiagnosticsView.vue'
import TrackSchemeView from '../plots/TrackSchemeView.vue'

// Registry of INTERACTIVE plot views (client/WebGL point clouds with per-point interaction, e.g.
// 2D-canvas dot plots), keyed by a stable view id. This is the counterpart to SUMMARY plots — those are
// server-aggregated plot-def JSONs rendered by the one generic PlotChart; interactive plots each
// need their own renderer + data endpoint, so they live here as self-contained view components.
//
// To add an interactive plot: write a <XView>.vue (fetch + render + its own controls; UmapView is the
// reference) and add ONE line here. The generic InteractivePanel + the canvases pick it up — no panel
// or canvas changes. Shared infra (not cluster-specific) so the future universal canvas reuses it.
// See docs/UI.md "Interactive plots".
export type BoardGroup = 'interactive' | 'clustering' | 'image'
export type PageFlag = 'clusterPage' | 'opticalFlowPage' | 'trackPage'

export interface InteractiveView {
  label: string
  component: Component
  clusterPage?: boolean       // offered on the Cluster module page's +Plot picker (UMAP only)
  opticalFlowPage?: boolean   // offered on the Optical Flow module page's +Plot picker
  trackPage?: boolean         // offered in the Track canvas's "+ Track…" picker
  analysisBoard?: boolean     // offered on the Analysis board's +Plot picker
  // NAMEABLE BY A PLUGIN, in `plugin.json` → `contributions.views` (PLUGINS_PLAN Decision 11), to show
  // on that plugin's custom module page. A separate opt-in rather than "any registered id", for two
  // reasons a plugin author cannot be expected to know: that page is the SUMMARY canvas, which renders
  // the population picker and nothing else, so a view wanting the cluster manager or the model vault
  // would be handed the wrong rail (ratcheted below); and `trackScheme` MUTATES, so it must not
  // become a panel a manifest can request. Flagging one makes its ID public — see the registry comment.
  // (It was `trackCorrection` when this rule was written; the timeline replaced that worklist and
  // inherited the rule, which is the point of stating it about the BEHAVIOUR rather than the id.)
  pluginPage?: boolean
  boardGroup?: BoardGroup     // which optgroup it lands in on the board (default 'interactive')
  // WHICH MANAGER this plot needs in the host's rail (default 'pops'). The plot declares it; the host
  // resolves it. Before this, the board hardcoded `activeIsCluster ? PopulationManager : SeriesPicker`,
  // so a plot needing the model vault had no way to say so and `flowProbability` was simply dead
  // there. See canvasManager.ts + docs/todo/CANVAS_MANAGER_RAIL_PLAN.md.
  rail?: RailKind
  // The population FAMILIES this plot can slice by, exactly as a summary spec's `dataSource.popTypes`
  // (`plots/popTypes.ts` reads both through one function). Declaring them is what makes a `rail: 'pops'`
  // view usable: the rail lists the ACTIVE plot's family, and without this it would list whichever
  // family the first registered summary spec happens to carry — a picker full of populations the plot
  // cannot draw. One family at a time, per docs/PLOTS.md; the plot owns the choice.
  popTypes?: PopTypeOption[]
  square?: boolean            // coord-fixed plot → free-floating panel snaps to a 1:1 box (no blank space)
  initialState?: () => Record<string, unknown>   // seed for a NEW panel's state bag (host-agnostic)
}

// The three TRACK families, shared by both track plots — the same set (and the same labels) the
// population summary declares for the behaviour page, because it is the same question: which populations
// of tracks. `granularity: 'track'` is what the picker lists; the plots then read those tracks' CELLS to
// draw a path or measure a displacement, which is a property of the plot, not of the family.
const TRACK_FAMILIES: PopTypeOption[] = [
  { popType: 'live', granularity: 'track', label: 'Tracked' },
  { popType: 'track', granularity: 'track', label: 'Tracked (gated)' },
  { popType: 'trackclust', granularity: 'track', label: 'Track clusters' },
]

// The flags are the surface "checkboxes": each host builds its picker with `pageViews`/`boardViews`
// below, so a view appears on a surface with no host-side wiring (see docs/UI.md).
//
// They only work if the hosts actually READ them. `flowMetrics` shipped with `analysisBoard: true`
// and never appeared, because `LayoutCanvas` filtered a hardcoded key list that the flag had no way
// to reach — a silently dead checkbox. The helpers below exist so no host can hardcode that list
// again; `interactiveViews.test.ts` pins that `LayoutCanvas` names no view key at all.
export const INTERACTIVE_VIEWS: Record<string, InteractiveView> = {
  umap: {
    label: 'UMAP', component: UmapView, clusterPage: true, analysisBoard: true,
    boardGroup: 'clustering', square: true, rail: 'clusterPops',
    initialState: () => ({ labels: true, hl: [] }),
  },
  // Self-contained: both pick their own image/segmentation in their panel state, so a population list
  // is dead chrome for them. `'none'` still gives them the rail's styling block — the gating strategy
  // reads `vis.fontSize` from it.
  gatingStrategy: { label: 'Gating strategy', component: GatingStrategyView, analysisBoard: true, rail: 'none', pluginPage: true },
  filmstrip: { label: 'Image / strip', component: ImageStripView, analysisBoard: true, boardGroup: 'image', rail: 'none', pluginPage: true },
  // What the UNet reads: every flow metric plane, so the user can pick which to train on. Distinct
  // from `filmstrip`, which is a napari SCREENSHOT montage — these planes are computed, are not
  // viewer layers, and have no reason to become any.
  // `rail: 'none'` is the same statement its header makes: this question is asked BEFORE a model
  // exists, so it must not be handed one.
  flowMetrics: { label: 'Flow metrics', component: FlowMetricsView, opticalFlowPage: true, analysisBoard: true, rail: 'none' },
  // Training convergence per loss TERM. A plot, not a chart in the vault's details modal, so it gets
  // the canvas chrome and the CSV/PNG/SVG + board-PDF export for free.
  flowTraining: { label: 'Training convergence', component: FlowTrainingView, opticalFlowPage: true, analysisBoard: true, rail: 'flowModels' },
  // What the trained model SEES: the projected input beside its probability map. Separate from
  // `flowMetrics` on purpose — that one is asked before a model exists and must not take one, this
  // one is meaningless without a checkpoint. Model comes from the vault's selection, not a picker.
  flowProbability: { label: 'Model probability', component: FlowProbabilityView, opticalFlowPage: true, analysisBoard: true, rail: 'flowModels' },
  // Tracks as a PLOT (the napari layer's counterpart): read-only, so unlike the timeline below it
  // belongs on the board. It takes the board's COMPARISON (`rail: 'pops'` + `TRACK_FAMILIES`): one group
  // per (images × population), so treatments and populations sit side by side like every other plot here
  // — see docs/TRACKING.md. `square` because its axes are µm on both sides and a stretched track plot is
  // a wrong one; the facet cells stay square too (`plots/facetGrid.ts`).
  trackPaths: { label: 'Tracks', component: TrackPathsView, trackPage: true, analysisBoard: true,
                square: true, rail: 'pops', popTypes: TRACK_FAMILIES, pluginPage: true },
  // Can this tracking result be trusted — the celltrackR QC battery (docs/TRACKING.md). Read-only, so
  // it belongs on the board: "is this movie comparable to its peers" is a board question, and it answers
  // it as a cohort — one curve per group, a group's images POOLED. Its verdicts come from the server, the
  // same ones `tracking.track_measures` banks as QC.
  trackDiagnostics: { label: 'Track diagnostics', component: TrackDiagnosticsView, trackPage: true,
                      analysisBoard: true, rail: 'pops', popTypes: TRACK_FAMILIES, pluginPage: true },
  // Tracks as LANES OVER FRAMES — the correction workspace (docs/todo/TRACK_SCHEME_PLAN.md). This
  // REPLACED the `trackCorrection` worklist, deleted with it: that surface drew each candidate on
  // SPATIAL axes and so could not answer the question every join turns on — are these two tracks in
  // the same frames? Track page only, because it MUTATES and the board is read-only
  // (docs/ANALYSIS.md); it is registered rather than hand-mounted so it gets the InteractivePanel
  // chrome (title bar, drag, resize, collapse, persist) that a hand-mount silently skips.
  trackScheme: {
    label: 'Track timeline', component: TrackSchemeView, trackPage: true,
    // `'pops'` like its two siblings, and NOT `pluginPage`: this one mutates. A track population is
    // what a user picks — the segmentation is the storage detail underneath it — so the Track canvas
    // shows the series picker whenever one of these three is the active panel, rather than each panel
    // growing a private segmentation `<select>` (docs/TRACKING.md → Which picker).
    rail: 'pops', popTypes: TRACK_FAMILIES,
    initialState: () => ({ order: 'pair', offset: 0, sel: [] }),
  },
}

/**
 * Renamed / replaced view keys, for canvases persisted before the change.
 *
 * A stored panel whose key no longer resolves does not fail loudly — `isInteractiveView` returns
 * false and the host's `v-else` renders something else entirely (on the Track canvas, a gating plot
 * with a correction panel's state in it). So a retired key must MAP, not vanish. Same contract as
 * `SPEC_ALIASES` in plots/popTypes.ts and `KIND_ALIASES` in ClusterPlots.
 *
 * `trackCorrection` → `trackScheme`: the worklist was replaced by the timeline, which authors the
 * same ops onto the same queue, so a saved worklist panel becomes the thing that replaced it rather
 * than an empty slot.
 */
export const VIEW_ALIASES: Record<string, string> = { trackCorrection: 'trackScheme' }

/** Apply `VIEW_ALIASES` to a persisted panel state in place; returns true when it changed. */
export function migrateViewKey(state: { kind: string }): boolean {
  const a = VIEW_ALIASES[state.kind]
  if (!a) return false
  state.kind = a
  return true
}

/** The manager a view needs, defaulted. Hosts call this rather than reading `.rail` themselves. */
export const railFor = (key: string): RailKind => INTERACTIVE_VIEWS[key]?.rail ?? DEFAULT_RAIL

/** The population families a view offers (empty when it does not slice by population). */
export const popTypesFor = (key: string): PopTypeOption[] => INTERACTIVE_VIEWS[key]?.popTypes ?? []

/**
 * A view's families in the shape every `plots/popTypes.ts` reader takes, or null.
 *
 * The host passes this to `useSummaryData` so the rail lists the ACTIVE interactive plot's family — the
 * same mechanism a summary slot uses through its spec, rather than a second resolution path that could
 * disagree about which populations a plot is showing.
 */
export const popTypeSpecFor = (key: string): PopTypeSpecLike | null => {
  const popTypes = popTypesFor(key)
  return popTypes.length ? { dataSource: { popTypes } } : null
}

export const isInteractiveView = (key: string): boolean => key in INTERACTIVE_VIEWS

/** May a PLUGIN name this view on its own module page? See `pluginPage` above. */
export const isPluginView = (key: string): boolean => !!INTERACTIVE_VIEWS[key]?.pluginPage

export interface ViewOption { key: string; label: string }

const options = (pick: (v: InteractiveView) => boolean): ViewOption[] =>
  Object.entries(INTERACTIVE_VIEWS).filter(([, v]) => pick(v)).map(([key, v]) => ({ key, label: v.label }))

/** Views a MODULE PAGE offers in its "+ Plot" picker, by that page's flag. */
export const pageViews = (flag: PageFlag): ViewOption[] => options(v => !!v[flag])

/** Views the ANALYSIS BOARD offers in one optgroup of its "+ Plot" picker. */
export const boardViews = (group: BoardGroup): ViewOption[] =>
  options(v => !!v.analysisBoard && (v.boardGroup ?? 'interactive') === group)
