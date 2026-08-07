import type { Component } from 'vue'
import UmapView from '../plots/UmapView.vue'
import GatingStrategyView from '../plots/GatingStrategyView.vue'
import ImageStripView from '../plots/ImageStripView.vue'
import FlowMetricsView from '../plots/FlowMetricsView.vue'
import FlowTrainingView from '../plots/FlowTrainingView.vue'
import FlowProbabilityView from '../plots/FlowProbabilityView.vue'

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
    boardGroup: 'clustering', square: true, initialState: () => ({ labels: true, hl: [] }),
  },
  gatingStrategy: { label: 'Gating strategy', component: GatingStrategyView, analysisBoard: true },
  filmstrip: { label: 'Image / strip', component: ImageStripView, analysisBoard: true, boardGroup: 'image' },
  // What the UNet reads: every flow metric plane, so the user can pick which to train on. Distinct
  // from `filmstrip`, which is a napari SCREENSHOT montage — these planes are computed, are not
  // viewer layers, and have no reason to become any.
  flowMetrics: { label: 'Flow metrics', component: FlowMetricsView, opticalFlowPage: true, analysisBoard: true },
  // Training convergence per loss TERM. A plot, not a chart in the vault's details modal, so it gets
  // the canvas chrome and the CSV/PNG/SVG + board-PDF export for free.
  flowTraining: { label: 'Training convergence', component: FlowTrainingView, opticalFlowPage: true, analysisBoard: true },
  // What the trained model SEES: the projected input beside its probability map. Separate from
  // `flowMetrics` on purpose — that one is asked before a model exists and must not take one, this
  // one is meaningless without a checkpoint. Model comes from the vault's selection, not a picker.
  flowProbability: { label: 'Model probability', component: FlowProbabilityView, opticalFlowPage: true, analysisBoard: true },
}

export const isInteractiveView = (key: string): boolean => key in INTERACTIVE_VIEWS

export interface ViewOption { key: string; label: string }

const options = (pick: (v: InteractiveView) => boolean): ViewOption[] =>
  Object.entries(INTERACTIVE_VIEWS).filter(([, v]) => pick(v)).map(([key, v]) => ({ key, label: v.label }))

/** Views a MODULE PAGE offers in its "+ Plot" picker, by that page's flag. */
export const pageViews = (flag: PageFlag): ViewOption[] => options(v => !!v[flag])

/** Views the ANALYSIS BOARD offers in one optgroup of its "+ Plot" picker. */
export const boardViews = (group: BoardGroup): ViewOption[] =>
  options(v => !!v.analysisBoard && (v.boardGroup ?? 'interactive') === group)

/** Is this view key a board slot of `group`? (the board treats clustering slots as one family) */
export const inBoardGroup = (key: string, group: BoardGroup): boolean =>
  key in INTERACTIVE_VIEWS && (INTERACTIVE_VIEWS[key].boardGroup ?? 'interactive') === group
