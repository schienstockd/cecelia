// A plot panel's side of the MCP `get_plot_image` / `set_plot_view` asks (`api/src/viewer_request.jl`):
// the server broadcasts `plot:request` {kind, projectUid, plotId, requestId, args}; the panel registered
// under that plotId in this tab answers on `/api/viewer/reply` — `image`: its export PNG, `view`: its
// settings changed (`args.settings`, checked by `utils/plotSettings.ts`). The pure part lives here;
// `usePanelExport` (stores/canvasPanelExports.ts) wires it to the panel's exporter.

/** The request addressed to this panel, or null when it is for another panel / project / malformed. */
export function plotRequestFor(data: unknown, plotId: string, projectUid: string):
    { requestId: string; kind: 'image' | 'view'; maxSide?: number; settings?: unknown } | null {
  if (!data || typeof data !== 'object' || !plotId || !projectUid) return null
  const d = data as Record<string, unknown>
  if (String(d.plotId ?? '') !== plotId || String(d.projectUid ?? '') !== projectUid) return null
  const requestId = String(d.requestId ?? '')
  if (!requestId) return null
  const args = (d.args && typeof d.args === 'object' ? d.args : {}) as Record<string, unknown>
  if (d.kind === 'view') return { requestId, kind: 'view', settings: args.settings }
  const ms = Number(args.maxSide)
  return { requestId, kind: 'image', ...(Number.isFinite(ms) && ms > 0 ? { maxSide: ms } : {}) }
}
