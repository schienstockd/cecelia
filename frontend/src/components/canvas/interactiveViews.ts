import type { Component } from 'vue'
import UmapView from '../plots/UmapView.vue'
import GatingStrategyView from '../plots/GatingStrategyView.vue'
import ImageStripView from '../plots/ImageStripView.vue'

// Registry of INTERACTIVE plot views (client/WebGL point clouds with per-point interaction, e.g.
// regl ScatterGL), keyed by a stable view id. This is the counterpart to SUMMARY plots — those are
// server-aggregated plot-def JSONs rendered by the one generic PlotChart; interactive plots each
// need their own renderer + data endpoint, so they live here as self-contained view components.
//
// To add an interactive plot: write a <XView>.vue (fetch + render + its own controls; UmapView is the
// reference) and add ONE line here. The generic InteractivePanel + the canvases pick it up — no panel
// or canvas changes. Shared infra (not cluster-specific) so the future universal canvas reuses it.
// See docs/UI.md "Interactive plots".
export interface InteractiveView {
  label: string
  component: Component
  clusterPage?: boolean     // offered on the Cluster module page's +Plot picker (UMAP only)
  analysisBoard?: boolean   // offered on the Analysis board's +Plot picker
}

// `clusterPage` / `analysisBoard` are the surface "checkboxes": each host builds its picker by filtering
// on its own flag, so a view appears on a surface with no host-side wiring (see docs/UI.md).
export const INTERACTIVE_VIEWS: Record<string, InteractiveView> = {
  umap: { label: 'UMAP', component: UmapView, clusterPage: true, analysisBoard: true },
  gatingStrategy: { label: 'Gating strategy', component: GatingStrategyView, analysisBoard: true },
  filmstrip: { label: 'Image / strip', component: ImageStripView, analysisBoard: true },
}

export const isInteractiveView = (key: string): boolean => key in INTERACTIVE_VIEWS
