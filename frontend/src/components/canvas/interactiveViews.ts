import type { Component } from 'vue'
import { DEFAULT_RAIL, type RailKind } from './canvasManager'
import { popTypeLabel, type PopTypeOption, type PopTypeSpecLike } from '../../plots/popTypes'
import { TRACK_FAMILY_POP_TYPES, CELL_PLOT_POP_TYPES } from '../../plots/popTypeRegistry'
import type { VisControl } from '../../plots/plotControls'
import ClusterMapView from '../plots/ClusterMapView.vue'
import GatingStrategyView from '../plots/GatingStrategyView.vue'
import ImageStripView from '../plots/ImageStripView.vue'
import FigureView from '../plots/FigureView.vue'
import FlowMetricsView from '../plots/FlowMetricsView.vue'
import FlowTrainingView from '../plots/FlowTrainingView.vue'
import FlowProbabilityView from '../plots/FlowProbabilityView.vue'
import type { PlotCategory, PlotSketchKind, PlotOption } from '../../plots/plotPicker'
import TrackPathsView from '../plots/TrackPathsView.vue'
import TrackDiagnosticsView from '../plots/TrackDiagnosticsView.vue'
import TrackSchemeView from '../plots/TrackSchemeView.vue'
import MotifCardsView from '../plots/MotifCardsView.vue'
import HmmStateCardsView from '../plots/HmmStateCardsView.vue'
import PopPositionsView from '../plots/PopPositionsView.vue'

// Registry of INTERACTIVE plot views (client/WebGL point clouds with per-point interaction, e.g.
// 2D-canvas dot plots), keyed by a stable view id. This is the counterpart to SUMMARY plots — those are
// server-aggregated plot-def JSONs rendered by the one generic PlotChart; interactive plots each
// need their own renderer + data endpoint, so they live here as self-contained view components.
//
// To add an interactive plot: write a <XView>.vue (fetch + render + its own controls; ClusterMapView is the
// reference) and add ONE line here. The generic InteractivePanel + the canvases pick it up — no panel
// or canvas changes. Shared infra (not cluster-specific) so the future universal canvas reuses it.
// See docs/UI.md "Interactive plots".
export type PageFlag = 'clusterPage' | 'opticalFlowPage' | 'trackPage'

export interface InteractiveView {
  label: string
  // the question this view answers, in the user's words — what an agent matches "how do I plot X?"
  // against (lib/guides/plotCatalogue.ts → get_guide("which-plot")). Required, so no view goes unlisted.
  answers: string
  component: Component
  clusterPage?: boolean       // offered on the Cluster module page's +Plot picker (cluster map only)
  opticalFlowPage?: boolean   // offered on the Model training page's +Plot picker
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
  // Where the "+ Plot…" picker files it and the schematic it draws (plots/plotPicker.ts). Required, so
  // a new view cannot land in the picker ungrouped.
  category: PlotCategory
  sketch: PlotSketchKind
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
  // The built-in SUMMARY-canvas module pages that offer this view (Phenotype, Behaviour, Spatial —
  // `SummaryCanvas`'s `module`), each with the families it slices THERE: the same shape and the same
  // per-page curation as a summary spec's `modules` (docs/PLOTS.md), so a page lists only its own
  // families. Absent = on no summary page (a plugin page still names it through `pluginPage`).
  modules?: Record<string, string[]>
  // ONE population at a time, and the rail enforces it. A plot that FACETS can take any number — that
  // is what the cohort comparison is — but a plot that edits one image's tracks cannot: several ticked
  // populations resolve to several groups, and the timeline draws `groups.find(mine) ?? groups[0]`, so
  // the second and third ticks changed nothing and said nothing. A picker that accepts input it
  // discards is worse than one that refuses it.
  singlePop?: boolean
  // The rail's plot settings this view reacts to (plots/plotControls.ts); absent = none, so the rail
  // shows no options block. Pinned against the view's source by `visControlsDeclared.test.ts`.
  visControls?: VisControl[]
  square?: boolean            // coord-fixed plot → free-floating panel snaps to a 1:1 box (no blank space)
  initialState?: () => Record<string, unknown>   // seed for a NEW panel's state bag (host-agnostic)
}

// The three TRACK families, shared by both track plots — the same set (and the same labels) the
// population summary declares for the behaviour page, because it is the same question: which populations
// of tracks. `granularity: 'track'` is what the picker lists; the plots then read those tracks' CELLS to
// draw a path or measure a displacement, which is a property of the plot, not of the family.
const TRACK_FAMILIES: PopTypeOption[] = TRACK_FAMILY_POP_TYPES.map(popType =>
  ({ popType, granularity: 'track', label: popTypeLabel({ popType, granularity: 'track' }) }))

// EVERY family, at cell granularity: a population of tracks is drawn as its member cells, frame by frame.
const CELL_FAMILIES: PopTypeOption[] = CELL_PLOT_POP_TYPES.map(popType =>
  ({ popType, granularity: 'cell', label: popTypeLabel({ popType, granularity: 'cell' }) }))

// The flags are the surface "checkboxes": each host builds its picker with `pageViews`/`boardViews`
// below, so a view appears on a surface with no host-side wiring (see docs/UI.md).
//
// They only work if the hosts actually READ them. `flowMetrics` shipped with `analysisBoard: true`
// and never appeared, because `LayoutCanvas` filtered a hardcoded key list that the flag had no way
// to reach — a silently dead checkbox. The helpers below exist so no host can hardcode that list
// again; `interactiveViews.test.ts` pins that `LayoutCanvas` names no view key at all.
export const INTERACTIVE_VIEWS: Record<string, InteractiveView> = {
  clusterMap: {
    label: 'Cluster map',
    answers: 'Where does each cluster sit on the UMAP / behaviour map?',
    component: ClusterMapView, clusterPage: true, analysisBoard: true,
    category: 'profiles', sketch: 'umap', square: true, rail: 'clusterPops',
    visControls: ['legend', 'pointSize', 'pointOpacity', 'palette', 'fontSize'],
    initialState: () => ({ labels: true, hl: [] }),
  },
  // Self-contained: both pick their own image/segmentation in their panel state, so a population list
  // is dead chrome for them. `'none'` still gives them the rail's styling block — the gating strategy
  // reads font size and dot size from it.
  gatingStrategy: { label: 'Gating strategy',
    answers: 'How was a population gated, from the root down?',
    component: GatingStrategyView, analysisBoard: true, category: 'look', sketch: 'gate', rail: 'none', pluginPage: true,
    visControls: ['pointSize', 'fontSize'] },
  filmstrip: { label: 'Image / strip',
    answers: 'What does it look like — a viewer screenshot or filmstrip?',
    component: ImageStripView, analysisBoard: true, category: 'look', sketch: 'image', rail: 'none', pluginPage: true },
  // A rendered figure (docs/todo/FIGURES_PLAN.md), picked by name and shown as made — the paper-quality
  // counterpart of `filmstrip`'s viewer screenshots. Read-only; the Movies & figures page edits it.
  figure: { label: 'Figure',
    answers: 'Can a figure from Movies & figures sit beside the plots?',
    component: FigureView, analysisBoard: true, category: 'look', sketch: 'image', rail: 'none' },
  // What the UNet reads: every flow metric plane, so the user can pick which to train on. Distinct
  // from `filmstrip`, which is a viewer SCREENSHOT montage — these planes are computed, are not
  // viewer layers, and have no reason to become any.
  // `rail: 'none'` is the same statement its header makes: this question is asked BEFORE a model
  // exists, so it must not be handed one.
  flowMetrics: { label: 'Flow metrics',
    answers: 'Which flow metric planes should the motion model train on?',
    component: FlowMetricsView, opticalFlowPage: true, analysisBoard: true, category: 'checks', sketch: 'planes', rail: 'none' },
  // Training convergence per loss TERM. A plot, not a chart in the vault's details modal, so it gets
  // the canvas chrome and the CSV/PNG/SVG + board-PDF export for free.
  flowTraining: { label: 'Training convergence',
    answers: 'Has the flow model\'s training converged?',
    component: FlowTrainingView, opticalFlowPage: true, analysisBoard: true, category: 'checks', sketch: 'curve', rail: 'flowModels' },
  // What the trained model SEES: the projected input beside its probability map. Separate from
  // `flowMetrics` on purpose — that one is asked before a model exists and must not take one, this
  // one is meaningless without a checkpoint. Model comes from the vault's selection, not a picker.
  flowProbability: { label: 'Model probability',
    answers: 'What does the trained flow model see?',
    component: FlowProbabilityView, opticalFlowPage: true, analysisBoard: true, category: 'checks', sketch: 'probability', rail: 'flowModels' },
  // Tracks as a PLOT (the viewer layer's counterpart): read-only, so unlike the timeline below it
  // belongs on the board. It takes the board's COMPARISON (`rail: 'pops'` + `TRACK_FAMILIES`): one group
  // per (images × population), so treatments and populations sit side by side like every other plot here
  // — see docs/TRACKING.md. `square` because its axes are µm on both sides and a stretched track plot is
  // a wrong one; the facet cells stay square too (`plots/facetGrid.ts`).
  trackPaths: { label: 'Tracks',
    answers: 'What do the tracks look like — paths, star or rose plots?',
    component: TrackPathsView, trackPage: true, analysisBoard: true, category: 'look', sketch: 'tracksStar',
                square: true, rail: 'pops', popTypes: TRACK_FAMILIES, pluginPage: true },
  // Can this tracking result be trusted — the celltrackR QC battery (docs/TRACKING.md). Read-only, so
  // it belongs on the board: "is this movie comparable to its peers" is a board question, and it answers
  // it as a cohort — one curve per group, a group's images POOLED. Its verdicts come from the server, the
  // same ones `tracking.track_measures` banks as QC.
  trackDiagnostics: { label: 'Track diagnostics',
    answers: 'Can this tracking be trusted?',
    component: TrackDiagnosticsView, trackPage: true, category: 'checks', sketch: 'trackCheck',
                      analysisBoard: true, rail: 'pops', popTypes: TRACK_FAMILIES, pluginPage: true },
  // Tracks as LANES OVER FRAMES — the correction workspace (docs/todo/TRACK_SCHEME_PLAN.md). This
  // REPLACED the `trackCorrection` worklist, deleted with it: that surface drew each candidate on
  // SPATIAL axes and so could not answer the question every join turns on — are these two tracks in
  // the same frames? Track page only, because it MUTATES and the board is read-only
  // (docs/ANALYSIS.md); it is registered rather than hand-mounted so it gets the InteractivePanel
  // chrome (title bar, drag, resize, collapse, persist) that a hand-mount silently skips.
  // Motif cards — one card per motif class discovered in the image's cells h5ad. Server enumerates
  // every segmentation whose h5ad carries `motif.class` and returns the list as
  // `availableValueNames`; the panel exposes a segmentation picker (persisted as
  // `state.valueName`, priority via the shared `resolveValueName`). `rail: 'none'` (motif classes
  // are h5ad obs values, not populations — BEHAVIOUR_CARDS_PLAN Decision 6 re-scoped 2026-09-20).
  // `category: 'look'` beside cellCards — both show real cells.
  motifCards: {
    label: 'Motif cards',
    answers: 'What does each motif class look like on real cells?',
    component: MotifCardsView, analysisBoard: true,
    category: 'look', sketch: 'motifCard', rail: 'none',
  },
  // HMM state cards — one card per HMM state value on a chosen `live.cell.hmm.state.<measure>`
  // column. Server enumerates eligible segmentations + hmm cols; panel exposes both pickers.
  // Medoid = the state-run whose owning track spends the most fraction of its life in this state
  // (Fig 4c, BEHAVIOUR_CARDS_PLAN Decision 3). `rail: 'none'` — HMM state values live in h5ad obs,
  // not on the pop rail.
  hmmStateCards: {
    label: 'HMM state cards',
    answers: 'What does each HMM state look like on a real track?',
    component: HmmStateCardsView, analysisBoard: true,
    category: 'look', sketch: 'statesCard', rail: 'none',
  },
  // Where each population's cells sit on the image, frame by frame. Read-only, so it belongs on the board and every page that defines or compares populations:
  // on the rail where the page has one, and on the Cluster / Region pages (`clusterPage`) it draws the
  // populations their manager shows. `square`: x and y are both image distance.
  popPositions: { label: 'Population positions',
    answers: 'Where do the populations sit on the image, frame by frame?',
    component: PopPositionsView, analysisBoard: true, clusterPage: true, category: 'look', sketch: 'popPositions',
    square: true, rail: 'pops', popTypes: CELL_FAMILIES,
    modules: {
      // POP-TYPES-OK: per-page curation, the same lists as a summary spec's `modules` (population_summary.json)
      phenotype: ['flow', 'clust'],
      // POP-TYPES-OK: per-page curation; `region` too — a movie's regions are built from its behaviour
      // clusters, so where they sit over time is a behaviour question
      behaviourAnalysis: ['live', 'track', 'trackclust', 'region'],
      spatialAnalysis: ['region'],
    },
    visControls: ['pointSize', 'fontSize'] },
  trackScheme: {
    label: 'Track timeline',
    answers: 'Which tracks overlap in time, to join or split them?',
    component: TrackSchemeView, trackPage: true, category: 'look', sketch: 'trackLanes',
    // `'pops'` like its two siblings, and NOT `pluginPage`: this one mutates. A track population is
    // what a user picks — the segmentation is the storage detail underneath it — so the Track canvas
    // shows the series picker whenever one of these three is the active panel, rather than each panel
    // growing a private segmentation `<select>` (docs/TRACKING.md → Which picker).
    rail: 'pops', popTypes: TRACK_FAMILIES,
    // ONE population: this panel edits ONE image's tracks, and a track id is only unique within one
    // (image, segmentation) — so a second ticked population is not a second facet, it is a second set
    // of ids the ops could not tell apart. Its two read-only siblings facet and take any number.
    singlePop: true,
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
 *
 * `umap` → `clusterMap`: the cluster view outgrew its one embedding (diffusion axes beside the UMAP,
 * docs/todo/TRACK_BEHAVIOUR_AXES_PLAN.md Decision 7). `BOARD_VIEW_ALIASES` in analysis_board_spec.jl
 * is the server-side mirror.
 */
export const VIEW_ALIASES: Record<string, string> = { trackCorrection: 'trackScheme', umap: 'clusterMap' }

/** Apply `VIEW_ALIASES` to a persisted panel state in place; returns true when it changed. */
export function migrateViewKey(state: { kind: string }): boolean {
  const a = VIEW_ALIASES[state.kind]
  if (!a) return false
  state.kind = a
  return true
}

/** The manager a view needs, defaulted. Hosts call this rather than reading `.rail` themselves. */
export const railFor = (key: string): RailKind => INTERACTIVE_VIEWS[key]?.rail ?? DEFAULT_RAIL

/** The rail settings a view reacts to — none unless it declares them. */
export const viewControls = (key: string): VisControl[] => INTERACTIVE_VIEWS[key]?.visControls ?? []

/** The population families a view offers (empty when it does not slice by population) — on a summary
 *  module page (`module`), only that page's families from its `modules` entry. */
export const popTypesFor = (key: string, module?: string | null): PopTypeOption[] => {
  const all = INTERACTIVE_VIEWS[key]?.popTypes ?? []
  const page = module ? INTERACTIVE_VIEWS[key]?.modules?.[module] : undefined
  return page ? all.filter(o => page.includes(o.popType)) : all
}

/** The built-in views a SUMMARY module page offers (their `modules` names it), by key. */
export const moduleViewKeys = (module: string | null | undefined): string[] =>
  module ? Object.entries(INTERACTIVE_VIEWS).filter(([, v]) => !!v.modules?.[module]).map(([k]) => k) : []

/** Does this view take exactly ONE population? Hosts ask; they must not read `.singlePop` themselves. */
export const singlePopFor = (key: string): boolean => !!INTERACTIVE_VIEWS[key]?.singlePop

/**
 * A view's families in the shape every `plots/popTypes.ts` reader takes, or null.
 *
 * The host passes this to `useSummaryData` so the rail lists the ACTIVE interactive plot's family — the
 * same mechanism a summary slot uses through its spec, rather than a second resolution path that could
 * disagree about which populations a plot is showing.
 */
export const popTypeSpecFor = (key: string, module?: string | null): PopTypeSpecLike | null => {
  const popTypes = popTypesFor(key, module)
  return popTypes.length ? { dataSource: { popTypes } } : null
}

export const isInteractiveView = (key: string): boolean => key in INTERACTIVE_VIEWS

/** May a PLUGIN name this view on its own module page? See `pluginPage` above. */
export const isPluginView = (key: string): boolean => !!INTERACTIVE_VIEWS[key]?.pluginPage

/** A view as a picker row (plots/plotPicker.ts → `PlotOption`, keyed by the view id). */
export interface ViewOption extends Omit<PlotOption, 'value'> { key: string }

export const viewOption = (key: string, v: Pick<InteractiveView, 'label' | 'answers' | 'category' | 'sketch'>): ViewOption =>
  ({ key, label: v.label, answers: v.answers, category: v.category, sketch: v.sketch })

const options = (pick: (v: InteractiveView) => boolean): ViewOption[] =>
  Object.entries(INTERACTIVE_VIEWS).filter(([, v]) => pick(v)).map(([key, v]) => viewOption(key, v))

/** Views a MODULE PAGE offers in its "+ Plot" picker, by that page's flag. */
export const pageViews = (flag: PageFlag): ViewOption[] => options(v => !!v[flag])

/** Views the ANALYSIS BOARD offers in its "+ Plot" picker (grouped there by `category`, like every picker). */
export const boardViews = (): ViewOption[] => options(v => !!v.analysisBoard)
