import type { Component } from 'vue'
import { DEFAULT_RAIL, type RailKind } from '../../components/canvas/canvasManager'
import type { PlotCategory, PlotSketchKind } from '../../plots/plotPicker'
import ClusterHeatmapPanel from './ClusterHeatmapPanel.vue'
import ClusterHmmStatesPanel from './ClusterHmmStatesPanel.vue'
import ClusterHmmTransitionsPanel from './ClusterHmmTransitionsPanel.vue'
import CellCardsView from '../../components/plots/CellCardsView.vue'
import { visControlsFor, type VisControl } from '../../plots/plotControls'
import { viewControls } from '../../components/canvas/interactiveViews'

// Registry of CLUSTER "panel" plots — the summary-family cluster plots that wrap CanvasPanel themselves
// (heatmap, HMM behaviour), as opposed to the interactive WebGL views in interactiveViews.ts (UMAP).
// This is the ONE place both the Cluster module page and the Analysis board discover them, so adding a
// cluster plot is a single registry line — no per-plot host wiring. Each entry declares:
//   • how it appears (label, trackOnly, needsCols) and WHERE (`analysisBoard` — the "put it on the board"
//     checkbox the whole design hinges on), and
//   • `props(ctx)` — the panel-specific props mapped from the shared cluster context, so the host can
//     render every panel with one generic `<component :is v-bind>` (no per-panel prop knowledge).
// Every panel must accept the common bag (index/active/docked/projectUid/setUid/imageUids/popType/suffix/
// shownPops/vis/state/persistKey) and expose exportImage()/getCsv() for the board's PDF/CSV export.
export interface ClusterCtx {
  featureOptions: string[]
  nameMap: Record<string, string>
  channelNames: string[]
  hmmStateCols: string[]
  hmmTransitionCols: string[]
}
export interface ClusterPanelDef {
  label: string
  answers: string                            // the question it answers (lib/guides/plotCatalogue.ts)
  component: Component
  category: PlotCategory                     // where the "+ Plot…" picker files it (plots/plotPicker.ts)
  sketch: PlotSketchKind                     // the schematic the picker draws
  trackOnly?: boolean                        // trackclust runs only (HMM behaviour plots)
  needsCols?: 'hmmState' | 'hmmTransition'   // only offer when the run has these obs columns
  analysisBoard?: boolean                    // ALSO offered on the Analysis board (not just the cluster page)
  rail?: RailKind                            // which manager the host's rail shows (default below)
  props?: (ctx: ClusterCtx) => Record<string, unknown>   // panel-specific props from the shared context
  // the rail settings it reacts to (plots/plotControls.ts); absent = none. Pinned by visControlsDeclared.test.ts
  visControls?: VisControl[]
}

// Every cluster panel is driven by the board's one clustering run, so they all want the read-only
// cluster PopulationManager. Declared per entry anyway (rather than assumed by the host) so the host
// stays registry-driven — see canvasManager.ts / docs/todo/CANVAS_MANAGER_RAIL_PLAN.md.
const CLUSTER_RAIL: RailKind = 'clusterPops'

export const CLUSTER_PANELS: Record<string, ClusterPanelDef> = {
  heatmap: {
    label: 'Cluster heatmap',
    answers: 'What defines each cluster — its feature profile?',
    component: ClusterHeatmapPanel, category: 'profiles', sketch: 'clusters', analysisBoard: true, rail: CLUSTER_RAIL,
    visControls: visControlsFor({ chartType: 'heatmap' }),    // built by buildPlotOptions, like a summary heatmap
    props: ctx => ({ featureOptions: ctx.featureOptions, nameMap: ctx.nameMap, channelNames: ctx.channelNames }),
  },
  hmmStates: {
    label: 'HMM states',
    answers: 'How do the HMM states split across track clusters?',
    component: ClusterHmmStatesPanel, category: 'makeup', sketch: 'states', trackOnly: true, needsCols: 'hmmState',
    analysisBoard: true, rail: CLUSTER_RAIL, props: ctx => ({ hmmCols: ctx.hmmStateCols }),
    visControls: ['legend', 'grid', 'palette', 'title', 'labX', 'labY', 'fontSize'],
  },
  hmmTransitions: {
    label: 'HMM transitions',
    answers: 'How do HMM transitions differ per cluster?',
    component: ClusterHmmTransitionsPanel, category: 'profiles', sketch: 'transitions', trackOnly: true, needsCols: 'hmmTransition',
    analysisBoard: true, rail: CLUSTER_RAIL, props: ctx => ({ hmmCols: ctx.hmmTransitionCols }),
    visControls: ['legend', 'grid', 'title', 'labX', 'labY', 'fontSize'],
  },
  // One card per trackclust pop the manager ticks: medoid track's filmstrip (server-rendered via
  // /api/cell_cards, trace baked in by overlay_author's `track_color_mode="pop"`) + a stats footer
  // of the pop's median motility measures. Needs no per-panel `ClusterCtx` beyond the shared bag.
  cellCards: {
    label: 'Cell cards',
    answers: 'What does a typical track of each cluster look like?',
    component: CellCardsView, category: 'look', sketch: 'trackCard', trackOnly: true,
    analysisBoard: true, rail: CLUSTER_RAIL,
  },
}

export const isClusterPanel = (kind: string): boolean => kind in CLUSTER_PANELS

/** The manager this panel needs, defaulted — the `railFor` of interactiveViews.ts. */
export const clusterPanelRail = (kind: string): RailKind => CLUSTER_PANELS[kind]?.rail ?? DEFAULT_RAIL

/** The rail settings a panel reacts to — none unless it declares them. */
export const clusterPanelControls = (kind: string): VisControl[] => CLUSTER_PANELS[kind]?.visControls ?? []

/** …for a plot of either registry, by key — what a host hosting both (board, cluster page) asks. */
export const registeredControls = (kind: string): VisControl[] =>
  isClusterPanel(kind) ? clusterPanelControls(kind) : viewControls(kind)
