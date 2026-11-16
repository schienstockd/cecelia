// Per-panel PNG export registry — one entry per mounted plot panel that can render itself to a PNG.
//
// Why a store rather than template refs: SummaryCanvas mounts panels via `v-for` where each panel's
// `id` is a store-generated number, not its index. A `[...refs]` array collected by Vue gives us
// order + count but no stable id ↔ ref mapping. The canvas Share compositor asks "please export
// the panel with id 7"; that has to be an id-keyed lookup. Registering + unregistering by id keeps
// the wiring straight even across duplicate / explode / remove operations.
//
// Registration is opt-in per host, not a base-class contract on CanvasPanel — a panel that has no
// export path (e.g. a placeholder / notice) simply doesn't register, and the share compositor
// will silently skip it. Cheaper than teaching every panel to say "I can't export."

import { defineStore, acceptHMRUpdate } from 'pinia'
import { ref, onActivated, onDeactivated, onScopeDispose } from 'vue'
import { useWsStore } from './ws'
import { useProjectMetaStore } from './projectMeta'
import { plotRequestFor } from '../utils/plotRequest'
import { replyToRequest } from '../utils/viewer/viewerRequest'
import { fitPngLongSide } from '../plots/export'
import { waitForPlotsIdle } from '../utils/plotReady'
import { describeSettings, checkSettingsPatch, type PlotSettings } from '../utils/plotSettings'

/** A registered panel's export contract. Returns a PNG data URL or `null` on failure (a plot that
 *  hasn't rendered yet, or a raster the browser refused). Async because the underlying path goes
 *  through an `<img>` decode. */
export interface PanelExporter { (): Promise<string | null> }

export const useCanvasPanelExportsStore = defineStore('canvasPanelExports', () => {
  // Keyed `${canvasKey}:${panelId}` — same shape as `useCanvasPanelsStore.geom`, so a consumer
  // can look up the export next to the geom without a second index. The alternative — a nested
  // map — buys nothing and complicates the register/unregister path.
  const exporters = ref<Record<string, PanelExporter>>({})

  function register(key: string, exporter: PanelExporter) { exporters.value[key] = exporter }
  // `owner`: remove only if the entry is still that exporter. Two mounted canvases can share a key (the
  // Whiteboard's QC canvas and the Segment page are both `summary:segment:<set>`, and both are kept
  // alive) — the one unmounting must not take the other's registration with it.
  function unregister(key: string, owner?: PanelExporter) {
    if (!(key in exporters.value)) return
    if (owner && exporters.value[key] !== owner) return
    delete exporters.value[key]
  }
  function get(key: string): PanelExporter | undefined { return exporters.value[key] }

  return { exporters, register, unregister, get }
})

/** Lifecycle-safe registration helper — registered while the caller is mounted AND visible. Under
 *  App.vue's `<KeepAlive>` a hidden page stays mounted, so with a shared key the last-mounted page would
 *  otherwise own the entry and Share would export the hidden page's panel.
 *  The same exporter answers the MCP `get_plot_image` ask (`plot:request` → `/api/viewer/reply`) —
 *  only while registered, so a hidden page's panel stays silent and the server's wait says so.
 *  `opts.settings` = what `set_plot_view` may change (utils/plotSettings.ts). Both asks wait for every
 *  plot load to settle first (`waitForPlotsIdle`), so the picture is never of a half-loaded plot. */
export interface PanelExportOpts { settings?: () => PlotSettings | undefined }
export function usePanelExport(key: () => string, exporter: PanelExporter, opts: PanelExportOpts = {}): void {
  const store = useCanvasPanelExportsStore()
  // Register under the current key; re-register when the key changes (a persistKey rebind).
  let lastKey = key()
  store.register(lastKey, exporter)
  // The key is expected to be stable for the lifetime of the panel (panel id + canvas key don't
  // change under a mounted CanvasPanel), so a watch is overkill; refresh only if the caller opts
  // to re-invoke this composable, and clean the previous entry.
  onActivated(() => store.register(lastKey, exporter))
  onDeactivated(() => store.unregister(lastKey, exporter))
  onScopeDispose(() => store.unregister(lastKey, exporter))

  const ws = useWsStore()
  const projectMeta = useProjectMetaStore()
  async function onPlotRequest(data: Record<string, unknown>) {
    const req = plotRequestFor(data, lastKey, projectMeta.current?.uid ?? '')
    if (!req || store.get(lastKey) !== exporter) return
    let result: Record<string, unknown>
    const settings = opts.settings?.() ?? {}
    const idle = () => waitForPlotsIdle({ timeoutMs: 20000 })
    try {
      if (req.kind === 'view') {
        const { apply, rejected } = await checkSettingsPatch(settings, req.settings)
        for (const [k, v] of apply) settings[k].set(v)
        await idle()
        result = { settings: await describeSettings(settings), ...(Object.keys(rejected).length ? { rejected } : {}) }
      } else {
        await idle()
        const png = await exporter()
        result = png ? { ...await fitPngLongSide(png, req.maxSide), settings: await describeSettings(settings) }
                     : { error: 'the plot has nothing drawn yet' }
      }
    } catch (e) {
      result = { error: e instanceof Error ? e.message : String(e) }
    }
    replyToRequest(req.requestId, result)
  }
  ws.on('plot:request', onPlotRequest)
  onScopeDispose(() => ws.off('plot:request', onPlotRequest))
  // Return void — the caller doesn't need the store. Kept as a bare function on purpose.
  void lastKey
}

if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useCanvasPanelExportsStore, import.meta.hot))
