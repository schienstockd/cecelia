// One-line adopter for a plot host (BIDIR PR #8) — folds `usePlotRegistry` (always) with the
// optional `usePanelExport` (panels that carry a PNG exporter, and optionally the settings Claude may
// change — MCP `get_plot_image` / `set_plot_view`). Kept intentionally thin: hosts import ONE
// composable, callers see ONE registration site, and a future per-panel side effect (e.g. a hover
// mirror) has one obvious home. A "just register" call stays a two-arg call.

import { usePlotRegistry, type PlotMeta } from '../stores/plotRegistry'
import { usePanelExport, type PanelExporter, type PanelExportOpts } from '../stores/canvasPanelExports'

/** Register a plot panel with the live registry (Claude discovers it via `list_plots`), and — if
 *  the panel supports it — with the PNG export bag (canvas Share compositor grabs it by
 *  persistKey). Empty-key / no-project cases are handled inside `usePlotRegistry`; nothing to
 *  guard here. */
export function useVisualPanel(
  persistKey: () => string,
  meta: () => PlotMeta,
  exporter?: PanelExporter,
  opts?: PanelExportOpts,
): void {
  usePlotRegistry(persistKey, meta)
  if (exporter) usePanelExport(persistKey, exporter, opts)
}
