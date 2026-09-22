// One-line adopter for a plot host (BIDIR PR #8) — folds `usePlotRegistry` (always) with the
// optional `usePanelExport` (only for panels that carry a PNG exporter — SummaryPanel today, the
// eight others sit registry-only). Kept intentionally thin: hosts import ONE composable, callers
// see ONE registration site, and adding a future per-panel side effect (e.g. a hover mirror) has
// one obvious home.
//
// Why this exists as a separate composable rather than folded into `usePlotRegistry`: eight of ten
// host panels have no `exportImage` yet, and forcing every caller to pass an exporter (even
// `undefined`) is noise at the call site. This wrapper lets a "just register" call stay a two-arg
// call, while SummaryPanel — the one panel that already exports — passes the third arg.

import { usePlotRegistry, type PlotMeta } from '../stores/plotRegistry'
import { usePanelExport, type PanelExporter } from '../stores/canvasPanelExports'

/** Register a plot panel with the live registry (Claude discovers it via `list_plots`), and — if
 *  the panel supports it — with the PNG export bag (canvas Share compositor grabs it by
 *  persistKey). Empty-key / no-project cases are handled inside `usePlotRegistry`; nothing to
 *  guard here. */
export function useVisualPanel(
  persistKey: () => string,
  meta: () => PlotMeta,
  exporter?: PanelExporter,
): void {
  usePlotRegistry(persistKey, meta)
  if (exporter) usePanelExport(persistKey, exporter)
}
