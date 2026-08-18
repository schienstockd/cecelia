import type { Component } from 'vue'
import { DEFAULT_RAIL, type RailKind } from './canvasManager'
import UmapView from '../plots/UmapView.vue'
import GatingStrategyView from '../plots/GatingStrategyView.vue'
import ImageStripView from '../plots/ImageStripView.vue'
import FlowMetricsView from '../plots/FlowMetricsView.vue'
import FlowTrainingView from '../plots/FlowTrainingView.vue'
import FlowProbabilityView from '../plots/FlowProbabilityView.vue'
import TrackCorrectionView from '../plots/TrackCorrectionView.vue'

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
export type PageFlag = 'clusterPage' | 'opticalFlowPage'

export interface InteractiveView {
  label: string
  component: Component
  clusterPage?: boolean       // offered on the Cluster module page's +Plot picker (UMAP only)
  opticalFlowPage?: boolean   // offered on the Optical Flow module page's +Plot picker
  analysisBoard?: boolean     // offered on the Analysis board's +Plot picker
  boardGroup?: BoardGroup     // which optgroup it lands in on the board (default 'interactive')
  // WHICH MANAGER this plot needs in the host's rail (default 'pops'). The plot declares it; the host
  // resolves it. Before this, the board hardcoded `activeIsCluster ? PopulationManager : SeriesPicker`,
  // so a plot needing the model vault had no way to say so and `flowProbability` was simply dead
  // there. See canvasManager.ts + docs/todo/CANVAS_MANAGER_RAIL_PLAN.md.
  rail?: RailKind
  square?: boolean            // coord-fixed plot → free-floating panel snaps to a 1:1 box (no blank space)
  initialState?: () => Record<string, unknown>   // seed for a NEW panel's state bag (host-agnostic)
}

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
  gatingStrategy: { label: 'Gating strategy', component: GatingStrategyView, analysisBoard: true, rail: 'none' },
  filmstrip: { label: 'Image / strip', component: ImageStripView, analysisBoard: true, boardGroup: 'image', rail: 'none' },
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
  // The tracking-correction worklist. Deliberately NOT `analysisBoard`: it is the only view here that
  // MUTATES — applying a row submits `tracking.correct_measures` — and the board is read-only
  // (docs/ANALYSIS.md). `rail: 'none'` because it picks its own image and label set in panel state;
  // a population list would be dead chrome, and correction is never scoped by one (the write is
  // whole-image by necessity — see CORRECTION_PLAN.md on `add_obs` and NaN).
  trackCorrection: { label: 'Correct tracks', component: TrackCorrectionView, rail: 'none' },
}

/** The manager a view needs, defaulted. Hosts call this rather than reading `.rail` themselves. */
export const railFor = (key: string): RailKind => INTERACTIVE_VIEWS[key]?.rail ?? DEFAULT_RAIL

export const isInteractiveView = (key: string): boolean => key in INTERACTIVE_VIEWS

export interface ViewOption { key: string; label: string }

const options = (pick: (v: InteractiveView) => boolean): ViewOption[] =>
  Object.entries(INTERACTIVE_VIEWS).filter(([, v]) => pick(v)).map(([key, v]) => ({ key, label: v.label }))

/** Views a MODULE PAGE offers in its "+ Plot" picker, by that page's flag. */
export const pageViews = (flag: PageFlag): ViewOption[] => options(v => !!v[flag])

/** Views the ANALYSIS BOARD offers in one optgroup of its "+ Plot" picker. */
export const boardViews = (group: BoardGroup): ViewOption[] =>
  options(v => !!v.analysisBoard && (v.boardGroup ?? 'interactive') === group)
