import type { Component } from 'vue'
import UmapView from '../plots/UmapView.vue'

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
}

export const INTERACTIVE_VIEWS: Record<string, InteractiveView> = {
  umap: { label: 'UMAP', component: UmapView },
}

export const isInteractiveView = (key: string): boolean => key in INTERACTIVE_VIEWS
