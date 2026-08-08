import type { Component } from 'vue'
import { DEFAULT_RAIL, type RailKind } from '../../components/canvas/canvasManager'
import ClusterHeatmapPanel from './ClusterHeatmapPanel.vue'
import ClusterHmmStatesPanel from './ClusterHmmStatesPanel.vue'
import ClusterHmmTransitionsPanel from './ClusterHmmTransitionsPanel.vue'

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
  hmmStateCols: string[]
  hmmTransitionCols: string[]
}
export interface ClusterPanelDef {
  label: string
  component: Component
  trackOnly?: boolean                        // trackclust runs only (HMM behaviour plots)
  needsCols?: 'hmmState' | 'hmmTransition'   // only offer when the run has these obs columns
  analysisBoard?: boolean                    // ALSO offered on the Analysis board (not just the cluster page)
  rail?: RailKind                            // which manager the host's rail shows (default below)
  props?: (ctx: ClusterCtx) => Record<string, unknown>   // panel-specific props from the shared context
}

// Every cluster panel is driven by the board's one clustering run, so they all want the read-only
// cluster PopulationManager. Declared per entry anyway (rather than assumed by the host) so the host
// stays registry-driven — see canvasManager.ts / docs/todo/CANVAS_MANAGER_RAIL_PLAN.md.
const CLUSTER_RAIL: RailKind = 'clusterPops'

export const CLUSTER_PANELS: Record<string, ClusterPanelDef> = {
  heatmap: {
    label: 'Cluster heatmap', component: ClusterHeatmapPanel, analysisBoard: true, rail: CLUSTER_RAIL,
    props: ctx => ({ featureOptions: ctx.featureOptions, nameMap: ctx.nameMap }),
  },
  hmmStates: {
    label: 'HMM states', component: ClusterHmmStatesPanel, trackOnly: true, needsCols: 'hmmState',
    analysisBoard: true, rail: CLUSTER_RAIL, props: ctx => ({ hmmCols: ctx.hmmStateCols }),
  },
  hmmTransitions: {
    label: 'HMM transitions', component: ClusterHmmTransitionsPanel, trackOnly: true, needsCols: 'hmmTransition',
    analysisBoard: true, rail: CLUSTER_RAIL, props: ctx => ({ hmmCols: ctx.hmmTransitionCols }),
  },
}

export const isClusterPanel = (kind: string): boolean => kind in CLUSTER_PANELS

/** The manager this panel needs, defaulted — the `railFor` of interactiveViews.ts. */
export const clusterPanelRail = (kind: string): RailKind => CLUSTER_PANELS[kind]?.rail ?? DEFAULT_RAIL
